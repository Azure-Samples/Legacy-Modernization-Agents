namespace CobolToQuarkusMigration.Agents.Infrastructure.Caching;

/// <summary>
/// Provider-agnostic deterministic response cache. Implementations must be safe
/// for concurrent calls from multiple agents in the same process.
/// </summary>
/// <remarks>
/// Lifecycle:
/// <list type="number">
///   <item>Caller builds <see cref="CacheKey"/> with all required fields.</item>
///   <item>Caller invokes <see cref="TryGetAsync"/>.</item>
///   <item>On hit: use <see cref="CacheLookupResult.Entry"/>.</item>
///   <item>On miss: invoke the provider, then <see cref="PutAsync"/> with the result.</item>
/// </list>
/// Callers should prefer <see cref="CachedLlmInvoker.GetOrInvokeAsync"/> — it bundles
/// the lookup/store dance with completeness checks and structured logging.
/// </remarks>
public interface IResponseCache
{
    /// <summary>
    /// Looks up an entry by key. Returns <see cref="CacheLookupResult"/> carrying
    /// either the entry or a <see cref="CacheMissReason"/>. Increments hit count
    /// and updates LRU timestamp on hit.
    /// </summary>
    Task<CacheLookupResult> TryGetAsync(CacheKey key, CancellationToken cancellationToken = default);

    /// <summary>
    /// Stores a deterministic response. Idempotent — re-storing the same key
    /// replaces the existing entry.
    /// </summary>
    Task PutAsync(CacheKey key, string responseText, CancellationToken cancellationToken = default);

    /// <summary>
    /// Prunes entries past TTL and (if specified) LRU-prunes down to a byte cap.
    /// Returns the number of entries deleted.
    /// </summary>
    Task<int> PruneAsync(TimeSpan ttl, long? maxBytes = null, CancellationToken cancellationToken = default);
}
