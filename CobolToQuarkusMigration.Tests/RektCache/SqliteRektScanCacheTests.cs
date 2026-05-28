using CobolToQuarkusMigration.Agents.Infrastructure.RektCache;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.RektCache;

public class SqliteRektScanCacheTests : IDisposable
{
    private readonly string _dbPath;
    private readonly SqliteRektScanCache _cache;

    public SqliteRektScanCacheTests()
    {
        _dbPath = Path.Combine(Path.GetTempPath(), $"rekt-scan-{Guid.NewGuid():N}.db");
        _cache = new SqliteRektScanCache(_dbPath, logger: null);
    }

    public void Dispose()
    {
        Microsoft.Data.Sqlite.SqliteConnection.ClearAllPools();
        if (File.Exists(_dbPath)) File.Delete(_dbPath);
        var wal = _dbPath + "-wal"; if (File.Exists(wal)) File.Delete(wal);
        var shm = _dbPath + "-shm"; if (File.Exists(shm)) File.Delete(shm);
    }

    private static RektScanEntry MakeEntry(string basename = "PROG.cbl", string hash = "h1") => new()
    {
        Basename = basename,
        IdentitySchemeVersion = "v1-basename",
        PreprocessedHash = hash,
        SourceHash = "raw-h",
        ParseOutcome = RektParseOutcome.Full,
        Confidence = RektScanConfidence.High,
        ParsedAtUtc = DateTime.UtcNow,
        Warnings = new List<string> { "warn-1" },
        DependencySnapshot = new Dictionary<string, string>
        {
            ["BOOK.cpy"] = "book-h",
        },
    };

    [Fact]
    public async Task TryGet_OnEmpty_ReturnsNull()
    {
        var got = await _cache.TryGetAsync("PROG.cbl", "v1-basename");
        got.Should().BeNull();
    }

    [Fact]
    public async Task UpsertThenTryGet_Roundtrips()
    {
        var entry = MakeEntry();
        await _cache.UpsertAsync(entry);

        var got = await _cache.TryGetAsync("PROG.cbl", "v1-basename");
        got.Should().NotBeNull();
        got!.PreprocessedHash.Should().Be("h1");
        got.ParseOutcome.Should().Be(RektParseOutcome.Full);
        got.Confidence.Should().Be(RektScanConfidence.High);
        got.Warnings.Should().BeEquivalentTo(new[] { "warn-1" });
        got.DependencySnapshot.Should().ContainKey("BOOK.cpy")
            .WhoseValue.Should().Be("book-h");
    }

    [Fact]
    public async Task Upsert_IsIdempotent()
    {
        await _cache.UpsertAsync(MakeEntry(hash: "h1"));
        await _cache.UpsertAsync(MakeEntry(hash: "h2"));

        var got = await _cache.TryGetAsync("PROG.cbl", "v1-basename");
        got!.PreprocessedHash.Should().Be("h2");
    }

    [Fact]
    public async Task DifferentIdentityScheme_IsolatesEntries()
    {
        await _cache.UpsertAsync(MakeEntry() with { IdentitySchemeVersion = "v1-basename" });
        await _cache.UpsertAsync(MakeEntry() with { IdentitySchemeVersion = "v2-relative-path" });

        var v1 = await _cache.TryGetAsync("PROG.cbl", "v1-basename");
        var v2 = await _cache.TryGetAsync("PROG.cbl", "v2-relative-path");
        v1.Should().NotBeNull();
        v2.Should().NotBeNull();
    }

    [Fact]
    public async Task PruneOtherIdentitySchemes_DropsLegacy()
    {
        await _cache.UpsertAsync(MakeEntry() with { IdentitySchemeVersion = "v1-basename" });
        await _cache.UpsertAsync(MakeEntry() with { IdentitySchemeVersion = "v0-legacy" });

        var deleted = await _cache.PruneOtherIdentitySchemesAsync("v1-basename");
        deleted.Should().Be(1);

        (await _cache.TryGetAsync("PROG.cbl", "v0-legacy")).Should().BeNull();
        (await _cache.TryGetAsync("PROG.cbl", "v1-basename")).Should().NotBeNull();
    }

    [Fact]
    public async Task GetMany_ReturnsOnlyPresentEntries()
    {
        await _cache.UpsertAsync(MakeEntry(basename: "A.cbl"));
        await _cache.UpsertAsync(MakeEntry(basename: "B.cbl"));

        var got = await _cache.GetManyAsync(new[] { "A.cbl", "B.cbl", "C.cbl" }, "v1-basename");
        got.Should().ContainKeys("A.cbl", "B.cbl");
        got.Should().NotContainKey("C.cbl");
    }

    [Fact]
    public async Task SchemaMismatch_FailsOpenAndRecreates()
    {
        await _cache.UpsertAsync(MakeEntry());
        (await _cache.TryGetAsync("PROG.cbl", "v1-basename")).Should().NotBeNull();

        // Force schema mismatch.
        using (var conn = new Microsoft.Data.Sqlite.SqliteConnection(
            $"Data Source={_dbPath};Cache=Shared"))
        {
            conn.Open();
            using var cmd = conn.CreateCommand();
            cmd.CommandText = "PRAGMA user_version = 999;";
            cmd.ExecuteNonQuery();
        }

        // Re-open: constructor must DROP and recreate; old entry is gone, no throw.
        var fresh = new SqliteRektScanCache(_dbPath, logger: null);
        (await fresh.TryGetAsync("PROG.cbl", "v1-basename")).Should().BeNull();
    }
}
