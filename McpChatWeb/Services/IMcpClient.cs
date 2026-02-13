using McpChatWeb.Models;

namespace McpChatWeb.Services;

public interface IMcpClient : IAsyncDisposable
{
    Task EnsureReadyAsync(CancellationToken cancellationToken = default);
    Task<IReadOnlyList<ResourceDto>> ListResourcesAsync(CancellationToken cancellationToken = default);
    Task<string> ReadResourceAsync(string uri, CancellationToken cancellationToken = default);
    Task<System.Text.Json.Nodes.JsonObject> CallToolAsync(string name, Dictionary<string, object> arguments, CancellationToken cancellationToken = default);
    Task<string> SendChatAsync(string prompt, CancellationToken cancellationToken = default);
}