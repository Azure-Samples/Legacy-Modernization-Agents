namespace CobolToQuarkusMigration.Agents.Infrastructure.RektCache;

// Cache storage is derived data; implementations must fail open on storage errors.
public interface IRektScanCache
{
    Task<RektScanEntry?> TryGetAsync(
        string basename, string identityScheme, CancellationToken cancellationToken = default);

    Task<IReadOnlyDictionary<string, RektScanEntry>> GetManyAsync(
        IReadOnlyCollection<string> basenames, string identityScheme,
        CancellationToken cancellationToken = default);

    Task UpsertAsync(RektScanEntry entry, CancellationToken cancellationToken = default);

    Task<int> PruneOtherIdentitySchemesAsync(
        string currentIdentityScheme, CancellationToken cancellationToken = default);

    Task<int> PruneByAgeAsync(TimeSpan maxAge, CancellationToken cancellationToken = default);

    Task<int> PruneStaleSemanticVersionsAsync(CancellationToken cancellationToken = default);

    Task<int> PruneToMaxEntriesAsync(int maxEntries, CancellationToken cancellationToken = default);
}
