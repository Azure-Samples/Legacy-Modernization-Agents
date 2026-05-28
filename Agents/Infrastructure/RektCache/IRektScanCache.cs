namespace CobolToQuarkusMigration.Agents.Infrastructure.RektCache;

/// <summary>
/// Persistent store for REKT scan metadata. Provides skip/parse decisions for
/// <see cref="IncrementalScanPlanner"/>. Implementations must be safe for
/// concurrent reads by multiple agents in the same process and must fail
/// open on any storage error — the scan cache is derived data and must
/// never block a parse.
/// </summary>
public interface IRektScanCache
{
    /// <summary>
    /// Looks up a previous parse for <paramref name="basename"/> under the current
    /// <paramref name="identityScheme"/>. Returns null if absent or unreadable.
    /// </summary>
    Task<RektScanEntry?> TryGetAsync(
        string basename, string identityScheme, CancellationToken cancellationToken = default);

    /// <summary>
    /// Bulk lookup. Returns a basename→entry map. Missing entries are simply
    /// absent from the result. Safe to call with thousands of names.
    /// </summary>
    Task<IReadOnlyDictionary<string, RektScanEntry>> GetManyAsync(
        IReadOnlyCollection<string> basenames, string identityScheme,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Inserts or replaces a scan entry. Idempotent on (basename, identityScheme).
    /// </summary>
    Task UpsertAsync(RektScanEntry entry, CancellationToken cancellationToken = default);

    /// <summary>
    /// Removes entries whose identity scheme is no longer current. Returns the
    /// number of rows deleted. Use when an identity-scheme migration completes.
    /// </summary>
    Task<int> PruneOtherIdentitySchemesAsync(
        string currentIdentityScheme, CancellationToken cancellationToken = default);

    /// <summary>
    /// Deletes entries whose <c>parsed_at_utc</c> is older than
    /// <paramref name="maxAge"/>. Returns the number of rows deleted.
    /// Use as a TTL eviction step so deleted-file rows don't linger forever.
    /// </summary>
    Task<int> PruneByAgeAsync(TimeSpan maxAge, CancellationToken cancellationToken = default);

    /// <summary>
    /// Deletes entries whose stored semantic-invalidation version is not the
    /// current <see cref="SqliteRektScanCache.SemanticInvalidationVersion"/>.
    /// These would already be treated as cache misses by lookups — this op
    /// just reclaims the disk space. Returns the number of rows deleted.
    /// </summary>
    Task<int> PruneStaleSemanticVersionsAsync(CancellationToken cancellationToken = default);

    /// <summary>
    /// LRU-style cap: if more than <paramref name="maxEntries"/> rows exist,
    /// delete the oldest by <c>parsed_at_utc</c> until the cap is met.
    /// Returns the number of rows deleted.
    /// </summary>
    Task<int> PruneToMaxEntriesAsync(int maxEntries, CancellationToken cancellationToken = default);
}
