using System.IO;
using System.Text;
using System.Text.Json;
using System.Text.Json.Nodes;
using Microsoft.Extensions.Logging;
using Microsoft.SemanticKernel;
using Microsoft.SemanticKernel.ChatCompletion;
using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Persistence;

namespace CobolToQuarkusMigration.Mcp;

/// <summary>
/// Minimal Model Context Protocol server that exposes migration insights backed by SQLite.
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
    private readonly Kernel? _kernel;
    private readonly string? _modelId;

    private MigrationRunSummary? _runSummaryCache;
    private DependencyMap? _dependencyMapCache;
    private IReadOnlyList<CobolAnalysis>? _analysisCache;

    public McpServer(IMigrationRepository repository, int runId, ILogger<McpServer> logger, AISettings? aiSettings = null)
    {
        _repository = repository;
        _runId = runId;
        _logger = logger;
        _aiSettings = aiSettings;

        if (_aiSettings != null && !string.IsNullOrEmpty(_aiSettings.Endpoint) && !string.IsNullOrEmpty(_aiSettings.ApiKey))
        {
            try
            {
                var kernelBuilder = Kernel.CreateBuilder();
                var deploymentName = _aiSettings.DeploymentName;
                if (string.IsNullOrEmpty(deploymentName))
                {
                    deploymentName = _aiSettings.ModelId;
                }

                kernelBuilder.AddAzureOpenAIChatCompletion(
                    deploymentName: deploymentName!,
                    endpoint: _aiSettings.Endpoint,
                    apiKey: _aiSettings.ApiKey);

                _kernel = kernelBuilder.Build();
                _modelId = deploymentName;
                _logger.LogInformation("Semantic Kernel initialized for custom Q&A with model {ModelId}", _modelId);
            }
            catch (Exception ex)
            {
                _logger.LogWarning(ex, "Failed to initialize Semantic Kernel for custom Q&A");
            }
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
                ["version"] = "0.1.0"
            },
            ["capabilities"] = new JsonObject
            {
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
            BuildResource($"insights://runs/{_runId}/impact/<filename>", "application/json", "Impact analysis for a specific file")
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
                    // Extract runId from URI: insights://runs/{runId}/graph
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

        // Use Azure OpenAI to answer custom questions
        if (!string.IsNullOrWhiteSpace(prompt) && _kernel != null && !string.IsNullOrEmpty(_modelId))
        {
            builder.AppendLine();
            builder.AppendLine($"**AI Model: {_modelId}**");
            builder.AppendLine();
            builder.AppendLine($"Your question: {prompt}");
            builder.AppendLine();

            try
            {
                var aiResponse = await GetAIResponseAsync(prompt, summary, dependencyMap, analyses);
                builder.AppendLine("AI Answer:");
                builder.AppendLine(aiResponse);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Failed to get AI response for question: {Prompt}", prompt);
                builder.AppendLine("Unable to get AI answer. Using fallback response.");
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

        var chatHistory = new ChatHistory();
        chatHistory.AddSystemMessage("You are an expert COBOL migration assistant. Answer questions about the COBOL to Java migration project based on the provided context. Be concise and specific.");
        chatHistory.AddUserMessage($"Context:\n{contextBuilder}\n\nUser Question: {prompt}");

        var chatCompletionService = _kernel!.GetRequiredService<IChatCompletionService>();
        var executionSettings = new PromptExecutionSettings
        {
            ExtensionData = new Dictionary<string, object>
            {
                ["max_completion_tokens"] = 500  // gpt-5-mini only supports default temperature (1)
            }
        };

        var response = await chatCompletionService.GetChatMessageContentAsync(chatHistory, executionSettings);
        return response.Content ?? "Unable to generate response.";
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

        // Use requested runId from URI, or fall back to server's default _runId
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

    /// <summary>
    /// Extracts the runId from a resource URI like "insights://runs/44/graph"
    /// </summary>
    private int? ExtractRunIdFromUri(string uri)
    {
        // Pattern: insights://runs/{runId}/...
        var match = System.Text.RegularExpressions.Regex.Match(uri, @"runs/(\d+)/");
        if (match.Success && int.TryParse(match.Groups[1].Value, out var runId))
        {
            _logger.LogDebug("Extracted runId {RunId} from URI: {Uri}", runId, uri);
            return runId;
        }

        _logger.LogWarning("Could not extract runId from URI: {Uri}, using default {DefaultRunId}", uri, _runId);
        return null; // Will use default _runId
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
