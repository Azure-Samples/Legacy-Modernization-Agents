namespace CobolToQuarkusMigration.Agents.Infrastructure.Caching;

/// <summary>
/// Reason a cache lookup did not return an entry. Always logged so cache misses
/// are explainable from logs alone.
/// </summary>
public enum CacheMissReason
{
    /// <summary>Cache is disabled at the call site (default-disabled per config).</summary>
    Disabled,

    /// <summary>Call is non-deterministic (temp&gt;0, streaming, interactive).</summary>
    NonDeterministic,

    /// <summary>Key not present in the cache.</summary>
    KeyNotFound,

    /// <summary>Entry exists but exceeded TTL.</summary>
    Expired,

    /// <summary>Previous attempt did not produce a cacheable result (incomplete/error).</summary>
    UpstreamNotCacheable,
}

/// <summary>
/// Single record returned from <see cref="IResponseCache.TryGetAsync"/> so callers
/// always get a typed reason for a miss. Async methods cannot use <c>out</c>.
/// </summary>
public sealed record CacheLookupResult(CacheEntry? Entry, CacheMissReason? MissReason)
{
    public bool IsHit => Entry is not null;
}

/// <summary>A stored cache entry.</summary>
/// <param name="ResponseText">Provider response body (text only — no streaming).</param>
/// <param name="CreatedAtUtc">When the entry was first stored.</param>
/// <param name="KeyHash">SHA-256 of the cache key (hex).</param>
/// <param name="HitCount">Number of times this entry has been served (post-this-hit).</param>
/// <param name="Age">Server-computed age at lookup time.</param>
public sealed record CacheEntry(
    string ResponseText,
    DateTime CreatedAtUtc,
    string KeyHash,
    int HitCount,
    TimeSpan Age);
