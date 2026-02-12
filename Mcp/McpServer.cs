using System.IO;
using System.Net.Http;
using System.Text;
using System.Text.Json;
using System.Text.Json.Nodes;
using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Agents;
using CobolToQuarkusMigration.Agents.Infrastructure;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Persistence;

namespace CobolToQuarkusMigration.Mcp;

/// <summary>
/// Minimal Model Context Protocol server that exposes migration insights backed by SQLite.
/// Uses IChatClient for AI operations.
/// </summary>
public sealed class McpServer
{
    private readonly IMigrationRepository _repository;
    private readonly int _runId;
    private readonly ILogger<McpServer> _logger;
    private readonly StreamReader _reader;
    private readonly StreamWriter _writer;
    private readonly Stream _outputStream;
    private readonly JsonSerializerOptions _jsonOptions = new(JsonSerializerDefaults.Web)
    {
        WriteIndented = false
    };
    private readonly AISettings? _aiSettings;
    private readonly IChatClient? _chatClient;
    private readonly ResponsesApiClient? _responsesClient;
    private readonly string? _modelId;
    private readonly BusinessLogicExtractorAgent? _extractorAgent;
    private readonly ProfileManager _profileManager;

    private MigrationRunSummary? _runSummaryCache;
    private DependencyMap? _dependencyMapCache;
    private IReadOnlyList<CobolAnalysis>? _analysisCache;

    public McpServer(IMigrationRepository repository, int runId, ILogger<McpServer> logger, AISettings? aiSettings = null, IChatClient? chatClient = null, ResponsesApiClient? responsesClient = null)
    {
        _repository = repository;
        _runId = runId;
        _logger = logger;
        _aiSettings = aiSettings;
        _chatClient = chatClient;
        _responsesClient = responsesClient;
        
        _profileManager = new ProfileManager(Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "Config", "GenerationProfiles.json"));

        // Use provided IChatClient or create one from settings
        if (_chatClient != null)
        {
            _modelId = _aiSettings?.ChatDeploymentName ?? _aiSettings?.ChatModelId ?? _aiSettings?.DeploymentName ?? _aiSettings?.ModelId ?? "chat-model";
            _logger.LogInformation("IChatClient provided for custom Q&A with model {ModelId}", _modelId);
        }
        else
        {
            // Prefer chat-specific settings for portal/Q&A; fall back to general model settings
            var chatEndpoint = _aiSettings?.ChatEndpoint;
            var chatApiKey = _aiSettings?.ChatApiKey;
            var chatDeployment = _aiSettings?.ChatDeploymentName;

            if (string.IsNullOrWhiteSpace(chatEndpoint)) chatEndpoint = _aiSettings?.Endpoint;
            if (string.IsNullOrWhiteSpace(chatApiKey)) chatApiKey = _aiSettings?.ApiKey;
            if (string.IsNullOrWhiteSpace(chatDeployment)) chatDeployment = _aiSettings?.ChatModelId;
            if (string.IsNullOrWhiteSpace(chatDeployment)) chatDeployment = _aiSettings?.DeploymentName;
            if (string.IsNullOrWhiteSpace(chatDeployment)) chatDeployment = _aiSettings?.ModelId;

            if (!string.IsNullOrEmpty(chatEndpoint) && !string.IsNullOrEmpty(chatApiKey) && !string.IsNullOrEmpty(chatDeployment))
            {
                try
                {
                    _chatClient = ChatClientFactory.CreateAzureOpenAIChatClient(
                        chatEndpoint,
                        chatApiKey,
                        chatDeployment);
                    _modelId = chatDeployment;
                    _logger.LogInformation("IChatClient initialized for custom Q&A with model {ModelId}", _modelId);
                }
                catch (Exception ex)
                {
                    _logger.LogWarning(ex, "Failed to initialize IChatClient for custom Q&A");
                }
            }
        }

        // Initialize Agents - Prefer ResponsesApiClient if available
        ILogger<BusinessLogicExtractorAgent> extractorLogger;
        if (logger is ILogger<BusinessLogicExtractorAgent> existingExtractorLogger)
        {
            extractorLogger = existingExtractorLogger;
        }
        else
        {
            using var loggerFactory = new LoggerFactory();
            extractorLogger = loggerFactory.CreateLogger<BusinessLogicExtractorAgent>();
        }
        
        if (_responsesClient != null)
        {
            _extractorAgent = new BusinessLogicExtractorAgent(_responsesClient, extractorLogger, _aiSettings?.ModelId ?? "extractor-model");
            _logger.LogInformation("Agents initialized with ResponsesApiClient");
        }
        else if (_chatClient != null)
        {
             _extractorAgent = new BusinessLogicExtractorAgent(_chatClient, extractorLogger, _modelId ?? "extractor-model");
             _logger.LogInformation("Agents initialized with IChatClient");
        }
        else
        {
            _extractorAgent = null;
            _logger.LogWarning("AI Client not available, Agents will not be functional.");
        }

        var inputStream = Console.OpenStandardInput();
        _outputStream = Console.OpenStandardOutput();
        _reader = new StreamReader(inputStream, new UTF8Encoding(false));
        _writer = new StreamWriter(_outputStream, new UTF8Encoding(false))
        {
            AutoFlush = true
        };
    }

    public async Task RunAsync(CancellationToken cancellationToken)
    {
        _logger.LogInformation("MCP server started for migration run {RunId}", _runId);

        while (!cancellationToken.IsCancellationRequested)
        {
            var request = await ReadRequestAsync(cancellationToken);
            if (request is null)
            {
                await Task.Delay(25, cancellationToken);
                continue;
            }

            using (request)
            {
                if (request.Method is null)
                {
                    await WriteErrorAsync(request.Id, -32600, "Invalid request");
                    continue;
                }

                try
                {
                    switch (request.Method)
                    {
                        case "initialize":
                            await HandleInitializeAsync(request);
                            break;
                        case "ping":
                            await WriteResultAsync(request.Id, new JsonObject());
                            break;
                        case "shutdown":
                            await WriteResultAsync(request.Id, new JsonObject());
                            _logger.LogInformation("Shutdown requested via MCP");
                            return;
                        case "resources/list":
                            await HandleResourcesListAsync(request);
                            break;
                        case "resources/read":
                            await HandleResourceReadAsync(request);
                            break;
                        case "messages/create":
                            await HandleMessagesCreateAsync(request);
                            break;
                        case "tools/call":
                            await HandleToolsCallAsync(request);
                            break;
                        case "tools/list":
                            await HandleToolsListAsync(request);
                            break;
                        default:
                            if (request.Id.HasValue)
                            {
                                await WriteErrorAsync(request.Id, -32601, $"Method '{request.Method}' not found");
                            }
                            _logger.LogDebug("Ignored MCP notification for method {Method}", request.Method);
                            break;
                    }
                }
                catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
                {
                    throw;
                }
                catch (Exception ex)
                {
                    _logger.LogError(ex, "Error handling MCP method {Method}", request.Method);
                    if (request.Id.HasValue)
                    {
                        await WriteErrorAsync(request.Id, -32603, ex.Message);
                    }
                }
            }
        }
    }

    private async Task HandleInitializeAsync(JsonRpcRequest request)
    {
        var result = new JsonObject
        {
            ["serverInfo"] = new JsonObject
            {
                ["name"] = "cobol-migration-insights",
                ["version"] = "0.2.0"  // Bumped version for AF migration
            },
            ["capabilities"] = new JsonObject
            {
                ["tools"] = new JsonObject
                {
                    ["list"] = true,
                    ["call"] = true
                },
                ["resources"] = new JsonObject
                {
                    ["list"] = true,
                    ["read"] = true
                },
                ["messages"] = new JsonObject
                {
                    ["create"] = new JsonObject
                    {
                        ["contentTypes"] = new JsonArray("text")
                    }
                }
            }
        };

        await WriteResultAsync(request.Id, result);
    }

    private async Task HandleResourcesListAsync(JsonRpcRequest request)
    {
        var resources = new JsonArray
        {
            BuildResource($"insights://runs/{_runId}/summary", "application/json", "High-level metrics for the migration run"),
            BuildResource($"insights://runs/{_runId}/dependencies", "application/json", "Dependency map and Mermaid diagram"),
            BuildResource($"insights://runs/{_runId}/analyses", "application/json", "Index of analyzed COBOL programs"),
            BuildResource($"insights://runs/{_runId}/analyses/<program>", "application/json", "Structured analysis for a specific COBOL file"),
            BuildResource($"insights://runs/{_runId}/graph", "application/json", "Dependency graph visualization data (Neo4j)"),
            BuildResource($"insights://runs/{_runId}/circular-dependencies", "application/json", "Circular dependency cycles detected in codebase"),
            BuildResource($"insights://runs/{_runId}/critical-files", "application/json", "Files with highest dependency connections"),
            BuildResource($"insights://runs/{_runId}/impact/<filename>", "application/json", "Impact analysis for a specific file"),
            
            // Smart Chunking resources
            BuildResource($"insights://runs/{_runId}/chunks", "application/json", "Chunk processing status for all files in the run"),
            BuildResource($"insights://runs/{_runId}/chunks/<filename>", "application/json", "Detailed chunk metadata for a specific file"),
            BuildResource($"insights://runs/{_runId}/signatures", "application/json", "All method signatures across the run for consistency tracking"),
            BuildResource($"insights://runs/{_runId}/signatures/<filename>", "application/json", "Method signatures for a specific file"),
            BuildResource($"insights://runs/{_runId}/source/<filename>", "text/x-cobol", "Original COBOL source code")
        };

        var result = new JsonObject
        {
            ["resources"] = resources
        };

        await WriteResultAsync(request.Id, result);
    }

    private async Task HandleResourceReadAsync(JsonRpcRequest request)
    {
        if (!request.Params.HasValue)
        {
            await WriteErrorAsync(request.Id, -32602, "Missing params");
            return;
        }

        var uri = ExtractUri(request.Params.Value);
        if (uri is null)
        {
            await WriteErrorAsync(request.Id, -32602, "Missing resource uri");
            return;
        }

        var contents = new JsonArray();
        switch (uri)
        {
            case var summaryUri when summaryUri.EndsWith("/summary", StringComparison.OrdinalIgnoreCase):
                contents.Add(await BuildContentAsync(uri, await BuildSummaryPayloadAsync()));
                break;
            case var dependenciesUri when dependenciesUri.EndsWith("/dependencies", StringComparison.OrdinalIgnoreCase):
                contents.Add(await BuildContentAsync(uri, await BuildDependencyPayloadAsync()));
                break;
            case var analysesUri when analysesUri.EndsWith("/analyses", StringComparison.OrdinalIgnoreCase):
                contents.Add(await BuildContentAsync(uri, await BuildAnalysesIndexPayloadAsync()));
                break;
            case var graphUri when graphUri.EndsWith("/graph", StringComparison.OrdinalIgnoreCase):
                {
                    _logger.LogInformation("🔍 Processing graph request for URI: {Uri}", uri);
                    var runIdFromUri = ExtractRunIdFromUri(uri);
                    _logger.LogInformation("🔍 Extracted runId: {RunId} from URI: {Uri}", runIdFromUri, uri);
                    contents.Add(await BuildContentAsync(uri, (await BuildGraphPayloadAsync(runIdFromUri))!));
                }
                break;
            case var circularUri when circularUri.EndsWith("/circular-dependencies", StringComparison.OrdinalIgnoreCase):
                contents.Add(await BuildContentAsync(uri, (await BuildCircularDependenciesPayloadAsync())!));
                break;
            case var criticalUri when criticalUri.EndsWith("/critical-files", StringComparison.OrdinalIgnoreCase):
                contents.Add(await BuildContentAsync(uri, (await BuildCriticalFilesPayloadAsync())!));
                break;
            default:
                if (uri.Contains("/analyses/", StringComparison.OrdinalIgnoreCase))
                {
                    var program = uri[(uri.LastIndexOf('/') + 1)..];
                    var payload = await BuildAnalysisPayloadAsync(Uri.UnescapeDataString(program));
                    if (payload is null)
                    {
                        await WriteErrorAsync(request.Id, -32001, $"Analysis for '{program}' not found");
                        return;
                    }

                    contents.Add(await BuildContentAsync(uri, payload));
                }
                else if (uri.Contains("/impact/", StringComparison.OrdinalIgnoreCase))
                {
                    var fileName = uri[(uri.LastIndexOf('/') + 1)..];
                    var payload = await BuildImpactAnalysisPayloadAsync(Uri.UnescapeDataString(fileName));
                    if (payload is null)
                    {
                        await WriteErrorAsync(request.Id, -32001, $"Impact analysis for '{fileName}' not available");
                        return;
                    }

                    contents.Add(await BuildContentAsync(uri, payload));
                }
                // Smart Chunking resources
                else if (uri.EndsWith("/chunks", StringComparison.OrdinalIgnoreCase))
                {
                    contents.Add(await BuildContentAsync(uri, (await BuildChunkStatusPayloadAsync())!));
                }
                else if (uri.Contains("/chunks/", StringComparison.OrdinalIgnoreCase))
                {
                    var fileName = uri[(uri.LastIndexOf('/') + 1)..];
                    var payload = await BuildChunkDetailsPayloadAsync(Uri.UnescapeDataString(fileName));
                    if (payload is null)
                    {
                        await WriteErrorAsync(request.Id, -32001, $"No chunks found for '{fileName}'");
                        return;
                    }
                    contents.Add(await BuildContentAsync(uri, payload));
                }
                else if (uri.EndsWith("/signatures", StringComparison.OrdinalIgnoreCase))
                {
                    contents.Add(await BuildContentAsync(uri, (await BuildAllSignaturesPayloadAsync())!));
                }
                else if (uri.Contains("/signatures/", StringComparison.OrdinalIgnoreCase))
                {
                    var fileName = uri[(uri.LastIndexOf('/') + 1)..];
                    var payload = await BuildFileSignaturesPayloadAsync(Uri.UnescapeDataString(fileName));
                    if (payload is null)
                    {
                        await WriteErrorAsync(request.Id, -32001, $"No signatures found for '{fileName}'");
                        return;
                    }
                    contents.Add(await BuildContentAsync(uri, payload));
                }
                else if (uri.Contains("/source/", StringComparison.OrdinalIgnoreCase))
                {
                    var fileName = uri[(uri.LastIndexOf('/') + 1)..];
                    var sourceCode = await GetCobolSourceAsync(Uri.UnescapeDataString(fileName));
                    
                    if (sourceCode is null)
                    {
                         await WriteErrorAsync(request.Id, -32001, $"Source code for '{fileName}' not found");
                         return;
                    }

                    contents.Add(new JsonObject 
                    {
                        ["uri"] = uri,
                        ["mimeType"] = "text/x-cobol",
                        ["text"] = sourceCode
                    });
                }
                else
                {
                    await WriteErrorAsync(request.Id, -32602, $"Unknown resource '{uri}'");
                    return;
                }
                break;
        }

        var result = new JsonObject
        {
            ["contents"] = contents
        };

        await WriteResultAsync(request.Id, result);
    }

    private async Task HandleToolsListAsync(JsonRpcRequest request)
    {
        var tools = new JsonArray
        {
        };

        await WriteResultAsync(request.Id, new JsonObject { ["tools"] = tools });
    }

    private async Task HandleToolsCallAsync(JsonRpcRequest request)
    {
        if (!request.Params.HasValue)
        {
            await WriteErrorAsync(request.Id, -32602, "Missing params");
            return;
        }

        var name = request.Params.Value.GetProperty("name").GetString();
        var arguments = request.Params.Value.GetProperty("arguments");

        switch (name)
        {
            default:
                await WriteErrorAsync(request.Id, -32601, $"Tool '{name}' not found");
                break;
        }
    }

    private async Task HandleMessagesCreateAsync(JsonRpcRequest request)
    {
        if (!request.Params.HasValue)
        {
            await WriteErrorAsync(request.Id, -32602, "Missing params");
            return;
        }

        var prompt = ExtractUserPrompt(request.Params.Value);
        var summary = await EnsureRunSummaryAsync();
        if (summary is null)
        {
            await WriteErrorAsync(request.Id, -32002, "No migration runs have been recorded yet");
            return;
        }

        var dependencyMap = await EnsureDependencyMapAsync();
        var analyses = await EnsureAnalysesAsync();

        var responseText = await BuildChatResponseAsync(prompt, summary, dependencyMap, analyses);

        var result = new JsonObject
        {
            ["content"] = new JsonArray
            {
                new JsonObject
                {
                    ["type"] = "text",
                    ["text"] = responseText
                }
            }
        };

        await WriteResultAsync(request.Id, result);
    }

    private async Task<JsonRpcRequest?> ReadRequestAsync(CancellationToken cancellationToken)
    {
        var headers = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
        string? line;

        while ((line = await _reader.ReadLineAsync()) != null)
        {
            cancellationToken.ThrowIfCancellationRequested();

            if (line.Length == 0)
            {
                break;
            }

            var separatorIndex = line.IndexOf(':');
            if (separatorIndex <= 0)
            {
                continue;
            }

            var name = line[..separatorIndex].Trim();
            var value = line[(separatorIndex + 1)..].Trim();
            headers[name] = value;
        }

        if (line is null)
        {
            return null;
        }

        if (!headers.TryGetValue("Content-Length", out var lengthValue) || !int.TryParse(lengthValue, out var contentLength))
        {
            return null;
        }

        var buffer = new char[contentLength];
        var totalRead = 0;
        while (totalRead < contentLength)
        {
            var read = await _reader.ReadAsync(buffer, totalRead, contentLength - totalRead);
            if (read == 0)
            {
                return null;
            }
            totalRead += read;
        }

        var payload = new string(buffer, 0, contentLength);
        var document = JsonDocument.Parse(payload);
        var root = document.RootElement;

        JsonElement? id = root.TryGetProperty("id", out var idElement) ? idElement : null;
        string? method = root.TryGetProperty("method", out var methodElement) ? methodElement.GetString() : null;
        JsonElement? parameters = root.TryGetProperty("params", out var paramsElement) ? paramsElement : null;

        return new JsonRpcRequest(document, id, method, parameters);
    }

    private async Task WriteResultAsync(JsonElement? id, JsonObject result)
    {
        var response = new JsonObject
        {
            ["jsonrpc"] = "2.0",
            ["result"] = result
        };

        if (id.HasValue)
        {
            response["id"] = ConvertId(id.Value);
        }

        await WriteMessageAsync(response);
    }

    private async Task WriteResultAsync(JsonElement? id, JsonArray result)
    {
        var response = new JsonObject
        {
            ["jsonrpc"] = "2.0",
            ["result"] = result
        };

        if (id.HasValue)
        {
            response["id"] = ConvertId(id.Value);
        }

        await WriteMessageAsync(response);
    }

    private async Task WriteErrorAsync(JsonElement? id, int code, string message)
    {
        if (!id.HasValue)
        {
            return;
        }

        var error = new JsonObject
        {
            ["code"] = code,
            ["message"] = message
        };

        var response = new JsonObject
        {
            ["jsonrpc"] = "2.0",
            ["id"] = ConvertId(id.Value),
            ["error"] = error
        };

        await WriteMessageAsync(response);
    }

    private async Task WriteMessageAsync(JsonObject payload)
    {
        var json = payload.ToJsonString(_jsonOptions);
        var bytes = Encoding.UTF8.GetBytes(json);
        await _writer.WriteAsync($"Content-Length: {bytes.Length}\r\n\r\n");
        await _writer.WriteAsync(json);
        await _writer.FlushAsync();
    }

    private static JsonNode ConvertId(JsonElement id)
    {
        return id.ValueKind switch
        {
            JsonValueKind.String => JsonValue.Create(id.GetString())!,
            JsonValueKind.Number when id.TryGetInt64(out var longValue) => JsonValue.Create(longValue)!,
            JsonValueKind.Number => JsonValue.Create(id.GetDouble())!,
            _ => JsonNode.Parse(id.GetRawText()) ?? JsonValue.Create<string?>(null)!
        };
    }

    private static string? ExtractUri(JsonElement parameters)
    {
        if (parameters.TryGetProperty("uri", out var singleUri) && singleUri.ValueKind == JsonValueKind.String)
        {
            return singleUri.GetString();
        }

        if (parameters.TryGetProperty("uris", out var uriArray) && uriArray.ValueKind == JsonValueKind.Array)
        {
            foreach (var element in uriArray.EnumerateArray())
            {
                if (element.ValueKind == JsonValueKind.String)
                {
                    return element.GetString();
                }
            }
        }

        return null;
    }

    private static string ExtractUserPrompt(JsonElement parameters)
    {
        if (!parameters.TryGetProperty("messages", out var messagesElement) || messagesElement.ValueKind != JsonValueKind.Array)
        {
            return string.Empty;
        }

        foreach (var message in messagesElement.EnumerateArray().Reverse())
        {
            if (!message.TryGetProperty("role", out var roleElement) || !string.Equals(roleElement.GetString(), "user", StringComparison.OrdinalIgnoreCase))
            {
                continue;
            }

            if (!message.TryGetProperty("content", out var contentElement) || contentElement.ValueKind != JsonValueKind.Array)
            {
                continue;
            }

            foreach (var contentItem in contentElement.EnumerateArray())
            {
                if (contentItem.TryGetProperty("type", out var typeElement) && string.Equals(typeElement.GetString(), "text", StringComparison.OrdinalIgnoreCase) &&
                    contentItem.TryGetProperty("text", out var textElement))
                {
                    return textElement.GetString() ?? string.Empty;
                }
            }
        }

        return string.Empty;
    }

    private async Task<JsonObject> BuildSummaryPayloadAsync()
    {
        var summary = await EnsureRunSummaryAsync() ?? throw new InvalidOperationException("No migration run summary available");

        var node = new JsonObject
        {
            ["runId"] = summary.RunId,
            ["status"] = summary.Status,
            ["startedAt"] = summary.StartedAt,
            ["completedAt"] = summary.CompletedAt,
            ["cobolSourcePath"] = summary.CobolSourcePath,
            ["javaOutputPath"] = summary.JavaOutputPath,
            ["notes"] = summary.Notes
        };

        if (summary.Metrics is not null)
        {
            node["metrics"] = JsonNode.Parse(JsonSerializer.Serialize(summary.Metrics, _jsonOptions));
        }

        if (!string.IsNullOrWhiteSpace(summary.AnalysisInsights))
        {
            node["analysisInsights"] = summary.AnalysisInsights;
        }

        if (!string.IsNullOrWhiteSpace(summary.MermaidDiagram))
        {
            node["mermaidDiagram"] = summary.MermaidDiagram;
        }

        return node;
    }

    private async Task<JsonObject> BuildDependencyPayloadAsync()
    {
        var dependencyMap = await EnsureDependencyMapAsync() ?? new DependencyMap();
        return JsonNode.Parse(JsonSerializer.Serialize(dependencyMap, _jsonOptions))!.AsObject();
    }

    private async Task<JsonObject> BuildAnalysesIndexPayloadAsync()
    {
        var analyses = await EnsureAnalysesAsync();
        var items = new JsonArray();

        foreach (var analysis in analyses)
        {
            items.Add(new JsonObject
            {
                ["fileName"] = analysis.FileName,
                ["programDescription"] = analysis.ProgramDescription,
                ["copybooks"] = JsonNode.Parse(JsonSerializer.Serialize(analysis.CopybooksReferenced, _jsonOptions)),
                ["paragraphCount"] = analysis.Paragraphs.Count,
                ["variableCount"] = analysis.Variables.Count
            });
        }

        return new JsonObject
        {
            ["analyses"] = items
        };
    }

    private async Task<JsonObject?> BuildAnalysisPayloadAsync(string program)
    {
        var analyses = await EnsureAnalysesAsync();
        var match = analyses.FirstOrDefault(a => string.Equals(a.FileName, program, StringComparison.OrdinalIgnoreCase));
        if (match is null)
        {
            return null;
        }

        return JsonNode.Parse(JsonSerializer.Serialize(match, _jsonOptions))!.AsObject();
    }

    private async Task<JsonObject> BuildContentAsync(string uri, JsonObject payload)
    {
        await Task.Yield();
        return new JsonObject
        {
            ["uri"] = uri,
            ["mimeType"] = "application/json",
            ["text"] = JsonSerializer.Serialize(payload, _jsonOptions)
        };
    }

    private JsonObject BuildResource(string uri, string mimeType, string description)
    {
        return new JsonObject
        {
            ["uri"] = uri,
            ["name"] = uri,
            ["description"] = description,
            ["mimeType"] = mimeType
        };
    }

    private async Task<string> BuildChatResponseAsync(string prompt, MigrationRunSummary summary, DependencyMap? dependencyMap, IReadOnlyList<CobolAnalysis> analyses)
    {
        var builder = new StringBuilder();

        builder.AppendLine($"Run {summary.RunId} ({summary.Status})");
        if (summary.Metrics is not null)
        {
            builder.AppendLine($"• Programs: {summary.Metrics.TotalPrograms}, Copybooks: {summary.Metrics.TotalCopybooks}, Dependencies: {summary.Metrics.TotalDependencies}");
            if (!string.IsNullOrWhiteSpace(summary.Metrics.MostUsedCopybook))
            {
                builder.AppendLine($"• Most used copybook: {summary.Metrics.MostUsedCopybook} ({summary.Metrics.MostUsedCopybookCount} references)");
            }
        }

        if (!string.IsNullOrWhiteSpace(summary.AnalysisInsights))
        {
            builder.AppendLine();
            builder.AppendLine("Insights:");
            builder.AppendLine(summary.AnalysisInsights);
        }

        // Use IChatClient to answer custom questions
        if (!string.IsNullOrWhiteSpace(prompt) && _chatClient != null && !string.IsNullOrEmpty(_modelId))
        {
            builder.AppendLine();
            builder.AppendLine($"Your question: {prompt}");
            builder.AppendLine();

            try
            {
                var aiResponse = await GetAIResponseAsync(prompt, summary, dependencyMap, analyses);
                
                // Big ASCII banner to make AI response easy to find
                builder.AppendLine();
                builder.AppendLine("╔══════════════════════════════════════════════════════════════════╗");
                builder.AppendLine("║     _    ___      _    _   _ ______        _______ ____          ║");
                builder.AppendLine("║    / \\  |_ _|    / \\  | \\ | / ___\\ \\      / / ____|  _ \\         ║");
                builder.AppendLine("║   / _ \\  | |    / _ \\ |  \\| \\___ \\\\ \\ /\\ / /|  _| | |_) |        ║");
                builder.AppendLine("║  / ___ \\ | |   / ___ \\| |\\  |___) |\\ V  V / | |___|  _ <         ║");
                builder.AppendLine("║ /_/   \\_\\___|_/_/   \\_\\_| \\_|____/  \\_/\\_/  |_____|_| \\_\\        ║");
                builder.AppendLine("║                                                                  ║");
                builder.AppendLine($"║  Model: {_modelId,-54} ║");
                builder.AppendLine("╚══════════════════════════════════════════════════════════════════╝");
                builder.AppendLine();
                builder.AppendLine(aiResponse);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Failed to get AI response for question: {Prompt}", prompt);
                builder.AppendLine($"Unable to get AI answer. Error: {ex.Message}");
                // builder.AppendLine(ex.StackTrace); // Uncomment for more details if needed
                builder.AppendLine("Using fallback response.");
                AppendFallbackResponse(builder, prompt, analyses, dependencyMap);
            }
        }
        else if (!string.IsNullOrWhiteSpace(prompt))
        {
            builder.AppendLine();
            builder.AppendLine($"Your question: {prompt}");
            AppendFallbackResponse(builder, prompt, analyses, dependencyMap);
        }

        if (dependencyMap is not null && dependencyMap.Dependencies.Count > 0)
        {
            builder.AppendLine();
            builder.AppendLine("Top dependencies:");
            foreach (var dep in dependencyMap.Dependencies.Take(5))
            {
                builder.AppendLine($"- {dep.SourceFile} -> {dep.TargetFile} ({dep.DependencyType})");
            }
        }

        builder.AppendLine();
        builder.AppendLine("Explore richer data via resources:");
        builder.AppendLine($"• Run summary: insights://runs/{_runId}/summary");
        builder.AppendLine($"• Dependency graph: insights://runs/{_runId}/dependencies");
        builder.AppendLine($"• Analyses index: insights://runs/{_runId}/analyses");
        builder.AppendLine($"• Specific analysis: insights://runs/{_runId}/analyses/<program>");

        return builder.ToString();
    }

    private void AppendFallbackResponse(StringBuilder builder, string prompt, IReadOnlyList<CobolAnalysis> analyses, DependencyMap? dependencyMap)
    {
        var matches = analyses
            .Where(a => prompt.Contains(a.FileName, StringComparison.OrdinalIgnoreCase) ||
                        prompt.Contains(Path.GetFileNameWithoutExtension(a.FileName), StringComparison.OrdinalIgnoreCase))
            .Take(5)
            .ToList();

        if (matches.Count > 0)
        {
            builder.AppendLine();
            builder.AppendLine("Relevant COBOL files:");
            foreach (var match in matches)
            {
                builder.AppendLine($"- {match.FileName}: {match.ProgramDescription}");
                if (match.CopybooksReferenced.Count > 0)
                {
                    builder.AppendLine($"  Copybooks: {string.Join(", ", match.CopybooksReferenced)}");
                }
            }
        }
    }


    private string ExtractResponseText(ChatResponse response)
    {
        if (response == null)
            return string.Empty;

        var sb = new StringBuilder();
        
        Console.Error.WriteLine($"[Debug] ChatResponse: Messages={response.Messages.Count}, FinishReason={response.FinishReason}");

        foreach (var message in response.Messages)
        {
            Console.Error.WriteLine($"[Debug] Message: Role={message.Role}, TextLen={message.Text?.Length ?? 0}, Contents={message.Contents?.Count ?? 0}");
            
            if (message.Role == ChatRole.Assistant)
            {
                if (!string.IsNullOrEmpty(message.Text))
                {
                    sb.Append(message.Text);
                }
                else if (message.Contents != null)
                {
                    foreach (var content in message.Contents)
                    {
                        Console.Error.WriteLine($"[Debug] Content Type: {content.GetType().Name}");
                        if (content is TextContent textContent)
                        {
                            sb.Append(textContent.Text);
                        }
                    }
                }
            }
        }
        
        var result = sb.ToString();
        Console.Error.WriteLine($"[Debug] Extracted text length: {result.Length}");
        return result;
    }

    private async Task<string> GetAIResponseAsync(string prompt, MigrationRunSummary summary, DependencyMap? dependencyMap, IReadOnlyList<CobolAnalysis> analyses)
    {
        // Build context for the AI
        var contextBuilder = new StringBuilder();
        contextBuilder.AppendLine($"Migration Run {summary.RunId} Context:");
        contextBuilder.AppendLine($"Status: {summary.Status}");

        if (summary.Metrics != null)
        {
            contextBuilder.AppendLine($"Programs: {summary.Metrics.TotalPrograms}, Copybooks: {summary.Metrics.TotalCopybooks}, Dependencies: {summary.Metrics.TotalDependencies}");
        }

        if (!string.IsNullOrEmpty(summary.AnalysisInsights))
        {
            contextBuilder.AppendLine("Dependency Insights:");
            contextBuilder.AppendLine(summary.AnalysisInsights.Length > 2000 ? summary.AnalysisInsights[..2000] + "..." : summary.AnalysisInsights);
        }

        if (dependencyMap != null && dependencyMap.Dependencies.Count > 0)
        {
            contextBuilder.AppendLine($"\nTop Dependencies ({dependencyMap.Dependencies.Count} total):");
            foreach (var dep in dependencyMap.Dependencies.Take(10))
            {
                contextBuilder.AppendLine($"- {dep.SourceFile} -> {dep.TargetFile} ({dep.DependencyType})");
            }
        }

        if (analyses.Count > 0)
        {
            contextBuilder.AppendLine($"\nCOBOL Files ({analyses.Count} total):");
            foreach (var analysis in analyses.Take(5))
            {
                contextBuilder.AppendLine($"- {analysis.FileName}: {analysis.ProgramDescription}");
            }
        }

        // Use IChatClient for the response
        var messages = new List<Microsoft.Extensions.AI.ChatMessage>
        {
            new(ChatRole.System, "You are an expert COBOL migration assistant. Answer questions about the COBOL to Java migration project based on the provided context. Be concise and specific."),
            new(ChatRole.User, $"Context:\n{contextBuilder}\n\nUser Question: {prompt}")
        };

        var chatOptions = new ChatOptions
        {
            MaxOutputTokens = 4000
        };

        var response = await _chatClient!.GetResponseAsync(messages, chatOptions);
        var messageContent = ExtractResponseText(response);
        
        if (string.IsNullOrWhiteSpace(messageContent))
        {
            return $"Unable to generate response. FinishReason: {response.FinishReason}";
        }
        
        return messageContent;
    }

    private async Task<MigrationRunSummary?> EnsureRunSummaryAsync()
    {
        if (_runSummaryCache is not null)
        {
            return _runSummaryCache;
        }

        _runSummaryCache = await _repository.GetRunAsync(_runId) ?? await _repository.GetLatestRunAsync();
        return _runSummaryCache;
    }

    private async Task<DependencyMap?> EnsureDependencyMapAsync()
    {
        if (_dependencyMapCache is not null)
        {
            return _dependencyMapCache;
        }

        _dependencyMapCache = await _repository.GetDependencyMapAsync(_runId);
        return _dependencyMapCache;
    }

    private async Task<IReadOnlyList<CobolAnalysis>> EnsureAnalysesAsync()
    {
        if (_analysisCache is not null)
        {
            return _analysisCache;
        }

        _analysisCache = await _repository.GetAnalysesAsync(_runId);
        return _analysisCache;
    }

    private async Task<JsonObject?> BuildGraphPayloadAsync(int? requestedRunId = null)
    {
        Console.WriteLine($"🚀 BuildGraphPayloadAsync called with requestedRunId={requestedRunId}");

        var runId = requestedRunId ?? _runId;
        Console.WriteLine($"🔍 Using runId={runId}, default _runId={_runId}");
        _logger.LogInformation("🔍 BuildGraphPayloadAsync: requestedRunId={RequestedRunId}, using runId={RunId}, default _runId={DefaultRunId}",
            requestedRunId, runId, _runId);

        if (_repository is not HybridMigrationRepository hybridRepo)
        {
            return new JsonObject
            {
                ["error"] = "Graph database not available - Neo4j integration required",
                ["nodes"] = new JsonArray(),
                ["edges"] = new JsonArray()
            };
        }

        var graphData = await hybridRepo.GetDependencyGraphDataAsync(runId);
        if (graphData is null || (graphData.Nodes.Count == 0 && graphData.Edges.Count == 0))
        {
            _logger.LogWarning("No graph data found for run {RunId}", runId);
            return new JsonObject
            {
                ["error"] = $"No graph data available for run {runId}",
                ["nodes"] = new JsonArray(),
                ["edges"] = new JsonArray()
            };
        }

        _logger.LogInformation("Returning graph data for run {RunId}: {NodeCount} nodes, {EdgeCount} edges",
            runId, graphData.Nodes.Count, graphData.Edges.Count);

        var nodesArray = new JsonArray();
        foreach (var node in graphData.Nodes)
        {
            nodesArray.Add(new JsonObject
            {
                ["id"] = node.Id,
                ["label"] = node.Label,
                ["isCopybook"] = node.IsCopybook,
                ["lineCount"] = node.LineCount
            });
        }

        var edgesArray = new JsonArray();
        foreach (var edge in graphData.Edges)
        {
            var edgeObj = new JsonObject
            {
                ["source"] = edge.Source,
                ["target"] = edge.Target,
                ["type"] = edge.Type
            };

            if (edge.LineNumber.HasValue)
            {
                edgeObj["lineNumber"] = edge.LineNumber.Value;
            }

            if (!string.IsNullOrEmpty(edge.Context))
            {
                edgeObj["context"] = edge.Context;
            }

            edgesArray.Add(edgeObj);
        }

        return new JsonObject
        {
            ["nodes"] = nodesArray,
            ["edges"] = edgesArray
        };
    }

    private async Task<JsonObject?> BuildCircularDependenciesPayloadAsync()
    {
        if (_repository is not HybridMigrationRepository hybridRepo)
        {
            return new JsonObject
            {
                ["error"] = "Graph database not available",
                ["cycles"] = new JsonArray()
            };
        }

        var cycles = await hybridRepo.GetCircularDependenciesAsync(_runId);
        var cyclesArray = new JsonArray();

        foreach (var cycle in cycles)
        {
            var pathArray = new JsonArray();
            foreach (var file in cycle.Files)
            {
                pathArray.Add(file);
            }

            cyclesArray.Add(new JsonObject
            {
                ["length"] = cycle.Length,
                ["path"] = pathArray
            });
        }

        return new JsonObject
        {
            ["count"] = cycles.Count,
            ["cycles"] = cyclesArray
        };
    }

    private async Task<JsonObject?> BuildCriticalFilesPayloadAsync()
    {
        if (_repository is not HybridMigrationRepository hybridRepo)
        {
            return new JsonObject
            {
                ["error"] = "Graph database not available",
                ["files"] = new JsonArray()
            };
        }

        var criticalFiles = await hybridRepo.GetCriticalFilesAsync(_runId);
        var filesArray = new JsonArray();

        foreach (var file in criticalFiles)
        {
            filesArray.Add(new JsonObject
            {
                ["fileName"] = file.FileName,
                ["incomingDependencies"] = file.IncomingDependencies,
                ["outgoingDependencies"] = file.OutgoingDependencies,
                ["totalConnections"] = file.TotalConnections
            });
        }

        return new JsonObject
        {
            ["count"] = criticalFiles.Count,
            ["files"] = filesArray
        };
    }

    private async Task<JsonObject?> BuildImpactAnalysisPayloadAsync(string fileName)
    {
        if (_repository is not HybridMigrationRepository hybridRepo)
        {
            return new JsonObject
            {
                ["error"] = "Graph database not available",
                ["fileName"] = fileName
            };
        }

        var impact = await hybridRepo.GetImpactAnalysisAsync(_runId, fileName);
        if (impact is null)
        {
            return null;
        }

        var downstreamArray = new JsonArray();
        foreach (var (file, distance) in impact.AffectedFiles)
        {
            downstreamArray.Add(new JsonObject
            {
                ["fileName"] = file,
                ["distance"] = distance
            });
        }

        var upstreamArray = new JsonArray();
        foreach (var (file, distance) in impact.Dependencies)
        {
            upstreamArray.Add(new JsonObject
            {
                ["fileName"] = file,
                ["distance"] = distance
            });
        }

        return new JsonObject
        {
            ["fileName"] = impact.TargetFile,
            ["downstreamFiles"] = downstreamArray,
            ["upstreamDependencies"] = upstreamArray,
            ["totalImpactedFiles"] = impact.TotalAffected,
            ["totalDependencies"] = impact.TotalDependencies
        };
    }

    private int? ExtractRunIdFromUri(string uri)
    {
        var match = System.Text.RegularExpressions.Regex.Match(uri, @"runs/(\d+)/");
        if (match.Success && int.TryParse(match.Groups[1].Value, out var runId))
        {
            _logger.LogDebug("Extracted runId {RunId} from URI: {Uri}", runId, uri);
            return runId;
        }

        _logger.LogWarning("Could not extract runId from URI: {Uri}, using default {DefaultRunId}", uri, _runId);
        return null;
    }

    private async Task<string?> GetCobolSourceAsync(string fileName)
    {
        try
        {
            var summary = await EnsureRunSummaryAsync();
            var sourcePath = summary?.CobolSourcePath;
            
            // Default to "source" if not set in summary
            if (string.IsNullOrEmpty(sourcePath)) sourcePath = "source";
            
            Console.Error.WriteLine($"[McpServer] DEBUG: Request to read file '{fileName}' (configured source path: '{sourcePath}')");

            // Define search candidates
            var candidates = new List<string>();
            
            // Case 1: Absolute path from summary
            if (Path.IsPathRooted(sourcePath))
            {
                candidates.Add(sourcePath);
            }
            else
            {
                // Case 2: Relative path from current directory
                candidates.Add(Path.Combine(Directory.GetCurrentDirectory(), sourcePath));
            }
            
            // Case 3: Fallback "source" in current directory (if different)
            var fallback = Path.Combine(Directory.GetCurrentDirectory(), "source");
            if (!candidates.Contains(fallback)) candidates.Add(fallback);

            foreach (var dir in candidates)
            {
                if (!Directory.Exists(dir)) continue;

                try 
                {
                    // 1. Try case-insensitive fuzzy search for fileName*
                    // This handles Bdsda10i -> BDSDA10I.cpy or Bdsda10i.cbl
                    var matches = Directory.GetFiles(dir, $"{fileName}*", new EnumerationOptions 
                    { 
                        MatchCasing = MatchCasing.CaseInsensitive,
                        RecurseSubdirectories = false 
                    });

                    foreach (var matchPath in matches)
                    {
                        var name = Path.GetFileName(matchPath);
                        // Check exact match (case-insensitive)
                        if (name.Equals(fileName, StringComparison.OrdinalIgnoreCase)) 
                        {
                            return await ReadFileSafeAsync(matchPath);
                        }
                        
                        // Check common extensions
                        var extensions = new[] { ".cbl", ".cpy", ".cob", ".pl1", ".jcl" };
                        foreach (var ext in extensions)
                        {
                             if (name.Equals(fileName + ext, StringComparison.OrdinalIgnoreCase))
                             {
                                 return await ReadFileSafeAsync(matchPath);
                             }
                        }
                    }
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine($"[McpServer] Scan error in {dir}: {ex.Message}");
                }
            }
            
            Console.Error.WriteLine($"[McpServer] File {fileName} not found in any candidate directory.");
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error reading source file {FileName}", fileName);
            Console.Error.WriteLine($"[McpServer] FATAL ERROR reading source file {fileName}: {ex}");
        }
        return null;
    }

    private async Task<string> ReadFileSafeAsync(string path)
    {
        Console.Error.WriteLine($"[McpServer] Reading file: {path}");
        var info = new FileInfo(path);
        if (info.Length > 5_000_000) 
            return "File too large to view via MCP (Start of file only):\n" + await ReadHeadAsync(path);
        return await File.ReadAllTextAsync(path);
    }

    private async Task<string> ReadHeadAsync(string path)
    {
        var buffer = new char[5000];
        using var reader = new StreamReader(path);
        var read = await reader.ReadAsync(buffer, 0, buffer.Length);
        return new string(buffer, 0, read) + "\n... (truncated)";
    }

    // ============================================================
    // SMART CHUNKING PAYLOAD BUILDERS
    // ============================================================

    private async Task<JsonObject?> BuildChunkStatusPayloadAsync()
    {
        if (_repository is not HybridMigrationRepository hybridRepo)
        {
            return new JsonObject
            {
                ["error"] = "Chunking data not available - requires HybridMigrationRepository",
                ["files"] = new JsonArray()
            };
        }

        try
        {
            var analyses = await hybridRepo.GetAnalysesAsync(_runId);
            var totalFiles = analyses.Count;

            var statusList = await hybridRepo.GetChunkProcessingStatusAsync(_runId);
            var filesArray = new JsonArray();

            foreach (var status in statusList)
            {
                filesArray.Add(new JsonObject
                {
                    ["sourceFile"] = status.SourceFile,
                    ["totalChunks"] = status.TotalChunks,
                    ["completedChunks"] = status.CompletedChunks,
                    ["failedChunks"] = status.FailedChunks,
                    ["pendingChunks"] = status.PendingChunks,
                    ["progressPercentage"] = Math.Round(status.ProgressPercentage, 2),
                    ["totalProcessingTimeMs"] = status.TotalProcessingTimeMs,
                    ["totalTokensUsed"] = status.TotalTokensUsed
                });
            }

            // Calculate Smart Migration stats
            var chunkedFilesCount = statusList.Count;
            // Files that are analyzed but not in chunk list are considered "Small Files" (processed directly)
            var smallFilesCount = Math.Max(0, totalFiles - chunkedFilesCount);
            
            // Heuristic: If we have ANY chunk records, Smart Migration was likely enabled/active.
            // If totalFiles > 0 and chunkedFiles == 0, it might mean all files were small OR chunking step hasn't run.
            // But we'll represent what we see in the DB.
            var smartMigrationActive = chunkedFilesCount > 0;

            return new JsonObject
            {
                ["runId"] = _runId,
                ["smartMigrationActive"] = smartMigrationActive,
                ["totalFiles"] = totalFiles,
                ["chunkedFiles"] = chunkedFilesCount,
                ["smallFiles"] = smallFilesCount,
                ["files"] = filesArray
            };
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error building chunk status payload");
            return new JsonObject
            {
                ["error"] = $"Failed to retrieve chunk status: {ex.Message}",
                ["files"] = new JsonArray()
            };
        }
    }

    private async Task<JsonObject?> BuildChunkDetailsPayloadAsync(string fileName)
    {
        if (_repository is not HybridMigrationRepository hybridRepo)
        {
            return null;
        }

        try
        {
            var chunks = await hybridRepo.GetChunksForFileAsync(_runId, fileName);
            if (chunks.Count == 0)
            {
                return null;
            }

            var chunksArray = new JsonArray();
            foreach (var chunk in chunks)
            {
                var chunkObj = new JsonObject
                {
                    ["chunkIndex"] = chunk.ChunkIndex,
                    ["startLine"] = chunk.StartLine,
                    ["endLine"] = chunk.EndLine,
                    ["status"] = chunk.Status,
                    ["tokensUsed"] = chunk.TokensUsed,
                    ["processingTimeMs"] = chunk.ProcessingTimeMs
                };

                if (chunk.SemanticUnits.Count > 0)
                {
                    var unitsArray = new JsonArray();
                    foreach (var unit in chunk.SemanticUnits)
                    {
                        unitsArray.Add(unit);
                    }
                    chunkObj["semanticUnits"] = unitsArray;
                }

                if (chunk.CompletedAt.HasValue)
                {
                    chunkObj["completedAt"] = chunk.CompletedAt.Value.ToString("O");
                }

                chunksArray.Add(chunkObj);
            }

            var completedChunks = chunks.Count(c => c.Status == "Completed");
            var totalLines = chunks.Max(c => c.EndLine) - chunks.Min(c => c.StartLine) + 1;

            return new JsonObject
            {
                ["runId"] = _runId,
                ["sourceFile"] = fileName,
                ["totalChunks"] = chunks.Count,
                ["completedChunks"] = completedChunks,
                ["progressPercentage"] = chunks.Count > 0 ? Math.Round((double)completedChunks / chunks.Count * 100, 2) : 0,
                ["totalLines"] = totalLines,
                ["totalTokensUsed"] = chunks.Sum(c => c.TokensUsed),
                ["totalProcessingTimeMs"] = chunks.Sum(c => c.ProcessingTimeMs),
                ["chunks"] = chunksArray
            };
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error building chunk details payload for {FileName}", fileName);
            return null;
        }
    }

    private async Task<JsonObject?> BuildAllSignaturesPayloadAsync()
    {
        if (_repository is not HybridMigrationRepository hybridRepo)
        {
            return new JsonObject
            {
                ["error"] = "Signature data not available - requires HybridMigrationRepository",
                ["signatures"] = new JsonArray()
            };
        }

        try
        {
            var signatures = await hybridRepo.GetAllSignaturesAsync(_runId);
            var signaturesArray = new JsonArray();

            var byFile = signatures.GroupBy(s => s.SourceFile);
            var fileCount = byFile.Count();

            foreach (var sig in signatures)
            {
                signaturesArray.Add(new JsonObject
                {
                    ["sourceFile"] = sig.SourceFile,
                    ["legacyName"] = sig.LegacyName,
                    ["targetMethodName"] = sig.TargetMethodName,
                    ["targetSignature"] = sig.TargetSignature,
                    ["returnType"] = sig.ReturnType,
                    ["definedInChunk"] = sig.DefinedInChunk
                });
            }

            return new JsonObject
            {
                ["runId"] = _runId,
                ["totalSignatures"] = signatures.Count,
                ["filesWithSignatures"] = fileCount,
                ["signatures"] = signaturesArray
            };
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error building all signatures payload");
            return new JsonObject
            {
                ["error"] = $"Failed to retrieve signatures: {ex.Message}",
                ["signatures"] = new JsonArray()
            };
        }
    }

    private async Task<JsonObject?> BuildFileSignaturesPayloadAsync(string fileName)
    {
        if (_repository is not HybridMigrationRepository hybridRepo)
        {
            return null;
        }

        try
        {
            var signatures = await hybridRepo.GetSignaturesForFileAsync(_runId, fileName);
            if (signatures.Count == 0)
            {
                return null;
            }

            var signaturesArray = new JsonArray();
            foreach (var sig in signatures)
            {
                signaturesArray.Add(new JsonObject
                {
                    ["legacyName"] = sig.LegacyName,
                    ["targetMethodName"] = sig.TargetMethodName,
                    ["targetSignature"] = sig.TargetSignature,
                    ["returnType"] = sig.ReturnType,
                    ["definedInChunk"] = sig.DefinedInChunk
                });
            }

            return new JsonObject
            {
                ["runId"] = _runId,
                ["sourceFile"] = fileName,
                ["totalSignatures"] = signatures.Count,
                ["signatures"] = signaturesArray
            };
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error building file signatures payload for {FileName}", fileName);
            return null;
        }
    }

    private sealed class JsonRpcRequest : IDisposable
    {
        public JsonRpcRequest(JsonDocument document, JsonElement? id, string? method, JsonElement? parameters)
        {
            Document = document;
            Id = id;
            Method = method;
            Params = parameters;
        }

        public JsonDocument Document { get; }
        public JsonElement? Id { get; }
        public string? Method { get; }
        public JsonElement? Params { get; }

        public void Dispose() => Document.Dispose();
    }
}
