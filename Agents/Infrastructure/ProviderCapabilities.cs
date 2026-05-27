namespace CobolToQuarkusMigration.Agents.Infrastructure;

/// <summary>
/// Lightweight metadata describing what a provider supports. Used by the
/// retry helper and provider clients to avoid behavioural drift, and read
/// by tests to assert that provider parity is preserved.
/// </summary>
/// <remarks>
/// This is intentionally a small, static record set. It is <b>not</b> a routing
/// engine and does not influence provider selection — that is still done by the
/// caller / configuration. See docs/throttling-and-cache-design.md §4.
/// </remarks>
public readonly record struct ProviderCapabilities(
    string ProviderKey,
    /// <summary>Provider returns a usable Retry-After header on 429.</summary>
    bool SupportsRetryAfterHeader,
    /// <summary>Provider streams partial responses (delta events).</summary>
    bool SupportsStreaming,
    /// <summary>Provider reports actual token usage in the response.</summary>
    bool ReportsActualTokenUsage,
    /// <summary>Strategy hint used for budgeting when actual usage is unavailable.</summary>
    string TokenEstimationStrategy,
    /// <summary>Default soft TPM cap, applied when config does not override.</summary>
    int DefaultTpm,
    /// <summary>Default soft RPM cap, applied when config does not override.</summary>
    int DefaultRpm);

public static class ProviderCapabilityRegistry
{
    public static readonly ProviderCapabilities AzureOpenAI = new(
        ProviderKey: "azure-openai",
        SupportsRetryAfterHeader: true,
        SupportsStreaming: true,
        ReportsActualTokenUsage: true,
        TokenEstimationStrategy: "server-reported",
        DefaultTpm: 1_000_000,
        DefaultRpm: 1_000);

    public static readonly ProviderCapabilities GitHubCopilotSdk = new(
        ProviderKey: "github-copilot-sdk",
        // SDK does not expose HTTP headers — we use heuristic message matching instead.
        SupportsRetryAfterHeader: false,
        SupportsStreaming: true,
        ReportsActualTokenUsage: false,
        TokenEstimationStrategy: "chars/4-approx",
        DefaultTpm: 200_000,
        DefaultRpm: 60);

    public static ProviderCapabilities? TryGet(string providerKey) =>
        providerKey switch
        {
            "azure-openai" => AzureOpenAI,
            "github-copilot-sdk" => GitHubCopilotSdk,
            _ => null,
        };
}
