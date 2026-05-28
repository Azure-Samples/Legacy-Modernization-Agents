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
}
