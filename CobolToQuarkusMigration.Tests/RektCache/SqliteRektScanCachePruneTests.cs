using CobolToQuarkusMigration.Agents.Infrastructure.RektCache;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.RektCache;

public class SqliteRektScanCachePruneTests : IDisposable
{
    private readonly string _dbPath;
    private readonly SqliteRektScanCache _cache;

    public SqliteRektScanCachePruneTests()
    {
        _dbPath = Path.Combine(Path.GetTempPath(), $"rekt-prune-{Guid.NewGuid():N}.db");
        _cache = new SqliteRektScanCache(_dbPath, logger: null);
    }

    public void Dispose()
    {
        Microsoft.Data.Sqlite.SqliteConnection.ClearAllPools();
        if (File.Exists(_dbPath)) File.Delete(_dbPath);
        var wal = _dbPath + "-wal"; if (File.Exists(wal)) File.Delete(wal);
        var shm = _dbPath + "-shm"; if (File.Exists(shm)) File.Delete(shm);
    }

    private static RektScanEntry MakeEntry(string basename, DateTime parsedAt) => new()
    {
        Basename = basename,
        IdentitySchemeVersion = "v1-basename",
        PreprocessedHash = "h",
        ParseOutcome = RektParseOutcome.Full,
        Confidence = RektScanConfidence.High,
        ParsedAtUtc = parsedAt,
        DependencySnapshot = new Dictionary<string, string>(),
    };

    [Fact]
    public async Task PruneByAge_DeletesOlderEntriesOnly()
    {
        await _cache.UpsertAsync(MakeEntry("OLD.cbl", DateTime.UtcNow.AddDays(-30)));
        await _cache.UpsertAsync(MakeEntry("FRESH.cbl", DateTime.UtcNow.AddHours(-1)));

        var deleted = await _cache.PruneByAgeAsync(TimeSpan.FromDays(7));
        deleted.Should().Be(1);

        (await _cache.TryGetAsync("OLD.cbl", "v1-basename")).Should().BeNull();
        (await _cache.TryGetAsync("FRESH.cbl", "v1-basename")).Should().NotBeNull();
    }

    [Fact]
    public async Task PruneByAge_NoOp_WhenEverythingFresh()
    {
        await _cache.UpsertAsync(MakeEntry("A.cbl", DateTime.UtcNow.AddHours(-1)));
        await _cache.UpsertAsync(MakeEntry("B.cbl", DateTime.UtcNow.AddHours(-2)));

        var deleted = await _cache.PruneByAgeAsync(TimeSpan.FromDays(30));
        deleted.Should().Be(0);
    }

    [Fact]
    public async Task PruneToMaxEntries_DropsOldestWhenOverCap()
    {
        var now = DateTime.UtcNow;
        await _cache.UpsertAsync(MakeEntry("OLDEST.cbl", now.AddHours(-3)));
        await _cache.UpsertAsync(MakeEntry("MID.cbl", now.AddHours(-2)));
        await _cache.UpsertAsync(MakeEntry("NEWEST.cbl", now.AddHours(-1)));

        var deleted = await _cache.PruneToMaxEntriesAsync(maxEntries: 2);
        deleted.Should().Be(1);

        (await _cache.TryGetAsync("OLDEST.cbl", "v1-basename")).Should().BeNull("oldest must go first");
        (await _cache.TryGetAsync("MID.cbl", "v1-basename")).Should().NotBeNull();
        (await _cache.TryGetAsync("NEWEST.cbl", "v1-basename")).Should().NotBeNull();
    }

    [Fact]
    public async Task PruneToMaxEntries_NoOpWhenUnderCap()
    {
        await _cache.UpsertAsync(MakeEntry("A.cbl", DateTime.UtcNow));
        await _cache.UpsertAsync(MakeEntry("B.cbl", DateTime.UtcNow));

        var deleted = await _cache.PruneToMaxEntriesAsync(maxEntries: 10);
        deleted.Should().Be(0);
    }

    [Fact]
    public async Task PruneToMaxEntries_ZeroEmptiesCache()
    {
        await _cache.UpsertAsync(MakeEntry("A.cbl", DateTime.UtcNow));
        await _cache.UpsertAsync(MakeEntry("B.cbl", DateTime.UtcNow));

        var deleted = await _cache.PruneToMaxEntriesAsync(maxEntries: 0);
        deleted.Should().Be(2);
    }

    [Fact]
    public async Task PruneStaleSemanticVersions_OnlyTouchesNonCurrentRows()
    {
        // Insert one row through the cache (gets current semantic version),
        // then directly insert one with a different version.
        await _cache.UpsertAsync(MakeEntry("CURRENT.cbl", DateTime.UtcNow));

        using (var conn = new Microsoft.Data.Sqlite.SqliteConnection(
            $"Data Source={_dbPath};Cache=Shared"))
        {
            conn.Open();
            using var cmd = conn.CreateCommand();
            cmd.CommandText = @"
                INSERT INTO scan_entry (
                    basename, identity_scheme, preprocessed_hash, parse_outcome, confidence,
                    parsed_at_utc, warnings_json, dependency_snapshot_json, semantic_invalidation_ver
                ) VALUES (
                    'STALE.cbl', 'v1-basename', 'h', 'Full', 'High',
                    $now, '[]', '{}', 'OLD-SEMANTIC-VERSION'
                );";
            cmd.Parameters.AddWithValue("$now",
                DateTime.UtcNow.ToString("O", System.Globalization.CultureInfo.InvariantCulture));
            cmd.ExecuteNonQuery();
        }

        var deleted = await _cache.PruneStaleSemanticVersionsAsync();
        deleted.Should().Be(1);

        // Current-version row survives; stale row is gone.
        (await _cache.TryGetAsync("CURRENT.cbl", "v1-basename")).Should().NotBeNull();
        // STALE.cbl would have returned null from TryGet already (semantic-version
        // mismatch is a logical miss), so this primarily proves the disk row went away.
    }

    [Fact]
    public async Task PruneByAge_FailsOpenOnCorruption()
    {
        // Smoke test that fail-open semantics hold — pass an absurd age that
        // matches everything, then corrupt the DB and re-prune.
        await _cache.UpsertAsync(MakeEntry("A.cbl", DateTime.UtcNow));
        Microsoft.Data.Sqlite.SqliteConnection.ClearAllPools();
        File.WriteAllText(_dbPath, "this is not a valid sqlite database");

        var deleted = await _cache.PruneByAgeAsync(TimeSpan.FromDays(1));
        deleted.Should().Be(0, "corruption must fail open, never throw");
    }
}
