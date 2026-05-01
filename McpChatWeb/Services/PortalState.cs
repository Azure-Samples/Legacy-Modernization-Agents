using McpChatWeb.Models;

namespace McpChatWeb.Services;

/// <summary>
/// Holds in-memory portal session state for model discovery and selection.
/// Registered as a singleton so all endpoints share the same state.
/// </summary>
public sealed class PortalState
{
    /// <summary>Active model ID selected by the user in the portal.</summary>
    public string? ActiveModelId { get; set; }

    /// <summary>Models discovered via the connect/probe flow.</summary>
    public List<ModelInfo> DiscoveredModels { get; } = new();

    /// <summary>Service type of the last successful connection (e.g., "AzureOpenAI").</summary>
    public string? ConnectedServiceType { get; set; }

    /// <summary>Endpoint URL of the last successful connection.</summary>
    public string? ConnectedEndpoint { get; set; }

    /// <summary>Whether the last connection used DefaultAzureCredential.</summary>
    public bool ConnectedViaDefaultCredential { get; set; }
}
