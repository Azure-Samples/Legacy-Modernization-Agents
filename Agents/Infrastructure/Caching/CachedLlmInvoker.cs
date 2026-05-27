using Microsoft.Extensions.Logging;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Caching;

/// <summary>
/// Wraps a deterministic provider call with cache lookup + store. Agents opt in
/// per call site — there is no agent-level wiring in P1 by design. See the P1
/// plan in docs/throttling-and-cache-design.md §4.
/// </summary>
/// <remarks>
/// <para>
/// Hard rules enforced here (not callable around):
/// </para>
/// <list type="bullet">
///   <item>If <paramref name="cache"/> is <c>null</c> or <paramref name="enabled"/> is <c>false</c>: bypass entirely, log <c>missReason=Disabled</c>.</item>
///   <item>If <paramref name="isDeterministic"/> is <c>false</c>: bypass entirely, log <c>missReason=NonDeterministic</c>.</item>
///   <item>If the invoke result has <c>IsCacheable=false</c> or <c>IsComplete=false</c>: do not store. Log <c>missReason=UpstreamNotCacheable</c>.</item>
/// </list>
/// </remarks>
public static class CachedLlmInvoker
{
    public static async Task<string> GetOrInvokeAsync(
        IResponseCache? cache,
        bool enabled,
        bool isDeterministic,
        CacheKey key,
        Func<CancellationToken, Task<LlmInvocationResult>> invoke,
        ILogger? logger,
        CancellationToken cancellationToken)
    {
        // Bypass paths first — explicit "why we didn't try the cache" logging.
        if (cache is null || !enabled)
        {
            LogBypass(logger, key, CacheMissReason.Disabled);
            var fresh = await invoke(cancellationToken);
            return fresh.Text;
        }

        if (!isDeterministic)
        {
            LogBypass(logger, key, CacheMissReason.NonDeterministic);
            var fresh = await invoke(cancellationToken);
            return fresh.Text;
        }

        // Cache fast-path — lookup; on hit return immediately.
        var lookup = await cache.TryGetAsync(key, cancellationToken);
        if (lookup.IsHit)
        {
            return lookup.Entry!.ResponseText;
        }

        // Miss — invoke the provider. The cache implementation logs the miss
        // reason; the bypass paths above log their own reasons. Do nothing here
        // to avoid double-logging.
        var result = await invoke(cancellationToken);

        if (!result.IsComplete || !result.IsCacheable)
        {
            LogSkippedStore(logger, key, result.FinishReason);
            return result.Text;
        }

        await cache.PutAsync(key, result.Text, cancellationToken);
        return result.Text;
    }

    private static void LogBypass(ILogger? logger, CacheKey key, CacheMissReason reason)
    {
        logger?.LogInformation(
            "[{Event}] runId={RunId} correlationId={CorrelationId} provider={Provider} model={Model} " +
            "decision=bypass missReason={MissReason} identityScheme={IdScheme} basename={Basename}",
            SqliteResponseCache.LogEventName, LlmCorrelationContext.RunId, LlmCorrelationContext.CorrelationId,
            key.ProviderKey, key.Model, reason, key.IdentitySchemeVersion, key.Basename ?? "-");
    }

    private static void LogSkippedStore(ILogger? logger, CacheKey key, string finishReason)
    {
        logger?.LogInformation(
            "[{Event}] runId={RunId} correlationId={CorrelationId} provider={Provider} model={Model} " +
            "decision=skip-store missReason={MissReason} finishReason={FinishReason} basename={Basename}",
            SqliteResponseCache.LogEventName, LlmCorrelationContext.RunId, LlmCorrelationContext.CorrelationId,
            key.ProviderKey, key.Model, CacheMissReason.UpstreamNotCacheable, finishReason,
            key.Basename ?? "-");
    }
}
