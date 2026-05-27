namespace CobolToQuarkusMigration.Agents.Infrastructure;

/// <summary>
/// Thrown when a request cannot proceed because the provider has rate-limited us
/// for longer than the per-call wait ceiling, or after bounded retries on a 429
/// without a usable Retry-After header.
/// </summary>
/// <remarks>
/// Callers should treat this as a soft failure: defer the unit of work, surface
/// to the user, or schedule a retry — but do not loop on it within the same
/// request. See docs/throttling-and-cache-design.md §4 for the policy.
/// </remarks>
public sealed class RateLimitedException : Exception
{
    /// <summary>Provider that rate-limited us (e.g. "azure-openai", "github-copilot-sdk").</summary>
    public string Provider { get; }

    /// <summary>Model or deployment identifier the call targeted.</summary>
    public string Model { get; }

    /// <summary>
    /// Server-suggested retry delay if a Retry-After header was present, otherwise null.
    /// </summary>
    public TimeSpan? RetryAfter { get; }

    public RateLimitedException(string provider, string model, TimeSpan? retryAfter, string message)
        : base(message)
    {
        Provider = provider;
        Model = model;
        RetryAfter = retryAfter;
    }
}
