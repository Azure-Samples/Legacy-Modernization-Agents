using McpChatWeb.Models;

namespace McpChatWeb.Services;

public interface IMcpClient : IAsyncDisposable
{
    Task EnsureReadyAsync(CancellationToken cancellationToken = default);
    Task RestartAsync(CancellationToken cancellationToken = default);
    Task<IReadOnlyList<ResourceDto>> ListResourcesAsync(CancellationToken cancellationToken = default);
    Task<string> ReadResourceAsync(string uri, CancellationToken cancellationToken = default);
    Task<System.Text.Json.Nodes.JsonObject> CallToolAsync(string name, Dictionary<string, object> arguments, CancellationToken cancellationToken = default);
    Task<string> SendChatAsync(string prompt, CancellationToken cancellationToken = default);
    Task<string> SendChatAsync(string prompt, ChatSendOptions options, CancellationToken cancellationToken = default);
}

// Context "report" means the prompt already carries everything the answer may use, so the server
// must not add run context of its own.
public sealed record ChatSendOptions(string? Context = null, int? MaxOutputTokens = null);