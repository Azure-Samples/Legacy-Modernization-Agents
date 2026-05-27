namespace CobolToQuarkusMigration.Agents.Infrastructure.Caching;

/// <summary>
/// Result returned from the invoke callback supplied to <see cref="CachedLlmInvoker"/>.
/// </summary>
/// <remarks>
/// The wrapper around <see cref="string"/> exists so the cache can refuse to store
/// incomplete or non-cacheable responses (e.g. <c>status == "incomplete"</c> from
/// Azure Responses API). Wrapping <see cref="string"/> in the callback would silently
/// cache truncated outputs.
/// </remarks>
public sealed record LlmInvocationResult(
    string Text,
    bool IsComplete,
    bool IsCacheable,
    string FinishReason = "")
{
    /// <summary>Convenience for the common "200 OK, full text, deterministic prompt" case.</summary>
    public static LlmInvocationResult Cacheable(string text) =>
        new(text, IsComplete: true, IsCacheable: true, FinishReason: "stop");

    /// <summary>Convenience for "use this response, but do not cache it" (incomplete, non-deterministic, etc.).</summary>
    public static LlmInvocationResult NotCacheable(string text, string reason) =>
        new(text, IsComplete: true, IsCacheable: false, FinishReason: reason);
}
