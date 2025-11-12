using System.Diagnostics;
using System.Text;
using System.Text.Json;
using System.Text.Json.Nodes;
using System.Threading;
using McpChatWeb.Configuration;
using McpChatWeb.Models;
using Microsoft.Extensions.Options;

namespace McpChatWeb.Services;

public sealed class McpProcessClient : IMcpClient
{
    private readonly SemaphoreSlim _mutex = new(1, 1);
    private readonly McpOptions _options;
    private readonly SemaphoreSlim _sendLock = new(1, 1);
    private Process? _process;
    private StreamWriter? _stdin;
    private StreamReader? _stdout;
    private Task? _stderrDrainer;
    private long _nextId = 1;
    private bool _initialized;

    public McpProcessClient(IOptions<McpOptions> options)
    {
        _options = options.Value;
    }

    public async Task EnsureReadyAsync(CancellationToken cancellationToken = default)
    {
        await _mutex.WaitAsync(cancellationToken).ConfigureAwait(false);
        try
        {
            if (_process is not null && !_process.HasExited)
            {
                if (!_initialized)
                {
                    await InitializeAsync(cancellationToken).ConfigureAwait(false);
                }
                return;
            }

            await StartProcessAsync(cancellationToken).ConfigureAwait(false);
            await InitializeAsync(cancellationToken).ConfigureAwait(false);
        }
        finally
        {
            _mutex.Release();
        }
    }

    public async Task<IReadOnlyList<ResourceDto>> ListResourcesAsync(CancellationToken cancellationToken = default)
    {
        await EnsureReadyAsync(cancellationToken).ConfigureAwait(false);
        var response = await SendRequestAsync("resources/list", new JsonObject(), cancellationToken).ConfigureAwait(false);
        var resourcesNode = response["resources"] as JsonArray ?? new JsonArray();
        var resources = new List<ResourceDto>(resourcesNode.Count);
        foreach (var item in resourcesNode)
        {
            if (item is not JsonObject obj)
            {
                continue;
            }

            var uri = obj["uri"]?.GetValue<string>() ?? string.Empty;
            var name = obj["name"]?.GetValue<string>() ?? uri;
            var description = obj["description"]?.GetValue<string>() ?? string.Empty;
            var mimeType = obj["mimeType"]?.GetValue<string>() ?? "application/json";
            resources.Add(new ResourceDto(uri, name, description, mimeType));
        }

        return resources;
    }

    public async Task<string> ReadResourceAsync(string uri, CancellationToken cancellationToken = default)
    {
        await EnsureReadyAsync(cancellationToken).ConfigureAwait(false);

        var parameters = new JsonObject
        {
            ["uri"] = uri
        };

        var response = await SendRequestAsync("resources/read", parameters, cancellationToken).ConfigureAwait(false);

        if (response.TryGetPropertyValue("contents", out var contentsNode) && contentsNode is JsonArray contentsArray && contentsArray.Count > 0)
        {
            if (contentsArray[0] is JsonObject contentObj && contentObj.TryGetPropertyValue("text", out var textNode))
            {
                return textNode?.GetValue<string>() ?? string.Empty;
            }
        }

        return string.Empty;
    }

    public async Task<string> SendChatAsync(string prompt, CancellationToken cancellationToken = default)
    {
        await EnsureReadyAsync(cancellationToken).ConfigureAwait(false);

        var messages = new JsonArray
        {
            new JsonObject
            {
                ["role"] = "system",
                ["content"] = new JsonArray(new JsonObject
                {
                    ["type"] = "text",
                    ["text"] = "You are a helpful assistant that answers questions about COBOL migration runs."
                })
            },
            new JsonObject
            {
                ["role"] = "user",
                ["content"] = new JsonArray(new JsonObject
                {
                    ["type"] = "text",
                    ["text"] = prompt
                })
            }
        };

        var parameters = new JsonObject
        {
            ["model"] = "cobol-migration-insights",
            ["messages"] = messages
        };

        var response = await SendRequestAsync("messages/create", parameters, cancellationToken).ConfigureAwait(false);
        var contentNode = response["content"] as JsonArray;
        if (contentNode is null || contentNode.Count == 0)
        {
            return "No response from MCP server.";
        }

        foreach (var item in contentNode)
        {
            if (item is JsonObject obj && obj["type"]?.GetValue<string>() == "text")
            {
                return obj["text"]?.GetValue<string>() ?? string.Empty;
            }
        }

        return contentNode[0]?.ToJsonString() ?? string.Empty;
    }

    public async ValueTask DisposeAsync()
    {
        if (_process is null)
        {
            return;
        }

        try
        {
            if (!_process.HasExited)
            {
                await SendRequestAsync("shutdown", new JsonObject(), CancellationToken.None).ConfigureAwait(false);
            }
        }
        catch
        {
            // ignore disposal issues
        }
        finally
        {
            try
            {
                _stdin?.Dispose();
                _stdout?.Dispose();
            }
            catch
            {
                // ignored
            }

            if (_process is { HasExited: false })
            {
                _process.Kill(true);
            }

            _process.Dispose();
            _stderrDrainer = null;
            _process = null;
            _stdin = null;
            _stdout = null;
        }

        await _mutex.WaitAsync().ConfigureAwait(false);
        _mutex.Release();
    }

    private Task StartProcessAsync(CancellationToken cancellationToken)
    {
        var arguments = new StringBuilder();

        var baseDirectory = _options.WorkingDirectory;
        if (string.IsNullOrWhiteSpace(baseDirectory))
        {
            baseDirectory = AppContext.BaseDirectory;
        }

        if (!Path.IsPathFullyQualified(baseDirectory))
        {
            baseDirectory = Path.GetFullPath(baseDirectory, AppContext.BaseDirectory);
        }

        var assemblyPath = Path.IsPathFullyQualified(_options.AssemblyPath)
            ? _options.AssemblyPath
            : Path.GetFullPath(_options.AssemblyPath, baseDirectory);

        var configPath = Path.IsPathFullyQualified(_options.ConfigPath)
            ? _options.ConfigPath
            : Path.GetFullPath(_options.ConfigPath, baseDirectory);

        arguments.Append('"').Append(assemblyPath).Append('"');
        arguments.Append(" mcp");
        arguments.Append(" --config ").Append('"').Append(configPath).Append('"');
        if (_options.RunId.HasValue)
        {
            arguments.Append(" --run-id ").Append(_options.RunId.Value);
        }

        var psi = new ProcessStartInfo
        {
            FileName = _options.DotnetExecutable,
            Arguments = arguments.ToString(),
            WorkingDirectory = baseDirectory,
            RedirectStandardInput = true,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
            CreateNoWindow = true
        };

        _process = Process.Start(psi) ?? throw new InvalidOperationException("Failed to start MCP process");
        _stdin = new StreamWriter(_process.StandardInput.BaseStream, new UTF8Encoding(false)) { AutoFlush = true };
        _stdout = new StreamReader(_process.StandardOutput.BaseStream, new UTF8Encoding(false));

        _stderrDrainer = Task.Run(async () =>
        {
            try
            {
                string? line;
                while ((line = await _process.StandardError.ReadLineAsync().ConfigureAwait(false)) is not null)
                {
                    Console.Error.WriteLine($"[MCP] {line}");
                }
            }
            catch
            {
                // ignore
            }
        }, cancellationToken);

        _initialized = false;
        return Task.CompletedTask;
    }

    private async Task InitializeAsync(CancellationToken cancellationToken)
    {
        var capabilities = await SendRequestAsync("initialize", new JsonObject(), cancellationToken).ConfigureAwait(false);
        _initialized = true;
    }

    private async Task<JsonObject> SendRequestAsync(string method, JsonObject parameters, CancellationToken cancellationToken)
    {
        if (_stdin is null || _stdout is null)
        {
            throw new InvalidOperationException("MCP process is not running");
        }

        var id = Interlocked.Increment(ref _nextId);

        var payload = new JsonObject
        {
            ["jsonrpc"] = "2.0",
            ["id"] = id,
            ["method"] = method
        };

        if (parameters.Count > 0)
        {
            payload["params"] = parameters;
        }

        var json = payload.ToJsonString();
        var bytes = Encoding.UTF8.GetBytes(json);

        await _sendLock.WaitAsync(cancellationToken).ConfigureAwait(false);
        try
        {
            await _stdin.WriteAsync($"Content-Length: {bytes.Length}\r\n\r\n").ConfigureAwait(false);
            await _stdin.WriteAsync(json).ConfigureAwait(false);
            await _stdin.FlushAsync().ConfigureAwait(false);

            while (true)
            {
                cancellationToken.ThrowIfCancellationRequested();
                var reply = await ReadResponseAsync(cancellationToken).ConfigureAwait(false);
                if (reply is null)
                {
                    continue;
                }

                if (!reply.TryGetPropertyValue("id", out var responseIdNode) || responseIdNode is not JsonValue idValue)
                {
                    continue;
                }

                if (!idValue.TryGetValue<long>(out var responseId) || responseId != id)
                {
                    continue;
                }

                if (reply.TryGetPropertyValue("error", out var errorNode) && errorNode is JsonObject errorObj)
                {
                    var message = errorObj["message"]?.GetValue<string>() ?? "Unknown MCP error";
                    throw new InvalidOperationException(message);
                }

                if (reply.TryGetPropertyValue("result", out var resultNode) && resultNode is JsonObject resultObj)
                {
                    return resultObj;
                }

                throw new InvalidOperationException("MCP response did not include a 'result' object");
            }
        }
        finally
        {
            _sendLock.Release();
        }
    }

    private async Task<JsonObject?> ReadResponseAsync(CancellationToken cancellationToken)
    {
        if (_stdout is null)
        {
            return null;
        }

        string? line;
        Dictionary<string, string> headers = new(StringComparer.OrdinalIgnoreCase);
        while ((line = await _stdout.ReadLineAsync().ConfigureAwait(false)) is not null)
        {
            cancellationToken.ThrowIfCancellationRequested();

            if (line.Length == 0)
            {
                break;
            }

            var index = line.IndexOf(':');
            if (index <= 0)
            {
                continue;
            }

            headers[line[..index].Trim()] = line[(index + 1)..].Trim();
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
            cancellationToken.ThrowIfCancellationRequested();
            var read = await _stdout.ReadAsync(buffer, totalRead, contentLength - totalRead).ConfigureAwait(false);
            if (read == 0)
            {
                return null;
            }

            totalRead += read;
        }

        var payload = new string(buffer, 0, contentLength);
        using var document = JsonDocument.Parse(payload);
        return JsonNode.Parse(document.RootElement.GetRawText())?.AsObject();
    }
}
