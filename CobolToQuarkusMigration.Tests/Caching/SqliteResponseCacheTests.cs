using CobolToQuarkusMigration.Agents.Infrastructure.Caching;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Caching;

public class SqliteResponseCacheTests : IDisposable
{
    private readonly string _dbPath;
    private readonly SqliteResponseCache _cache;

    public SqliteResponseCacheTests()
    {
        _dbPath = Path.Combine(Path.GetTempPath(), $"llm-cache-test-{Guid.NewGuid():N}.db");
        _cache = new SqliteResponseCache(_dbPath, logger: null);
    }

    public void Dispose()
    {
        Microsoft.Data.Sqlite.SqliteConnection.ClearAllPools();
        if (File.Exists(_dbPath)) File.Delete(_dbPath);
        var wal = _dbPath + "-wal"; if (File.Exists(wal)) File.Delete(wal);
        var shm = _dbPath + "-shm"; if (File.Exists(shm)) File.Delete(shm);
    }

    private static CacheKey MakeKey(string user = "u", string source = "src") => CacheKey.Build(new CacheKey
    {
        ProviderKey = "azure-openai",
        Model = "gpt-5.3-codex",
        SystemPromptHash = "sys",
        UserPromptHash = user,
        ReasoningEffort = "high",
        ResponseFormat = "text",
        PromptTemplateId = "java-converter",
        PromptTemplateVersion = "1",
        TargetLanguage = "java",
        FrameworkSettings = "quarkus",
        SourceHash = source,
        RektFactsHash = "",
        GenerationSettingsHash = "g",
        Basename = "PROG.cbl",
    });

    [Fact]
    public async Task TryGet_OnEmptyCache_ReturnsKeyNotFound()
    {
        var result = await _cache.TryGetAsync(MakeKey());
        result.IsHit.Should().BeFalse();
        result.MissReason.Should().Be(CacheMissReason.KeyNotFound);
    }

    [Fact]
    public async Task PutThenTryGet_ReturnsEntry()
    {
        var key = MakeKey();
        await _cache.PutAsync(key, "the response");

        var result = await _cache.TryGetAsync(key);
        result.IsHit.Should().BeTrue();
        result.Entry!.ResponseText.Should().Be("the response");
        result.Entry.HitCount.Should().Be(1, "this is the first hit");
        result.Entry.KeyHash.Should().Be(key.Compute());
    }

    [Fact]
    public async Task TryGet_IncrementsHitCount()
    {
        var key = MakeKey();
        await _cache.PutAsync(key, "x");

        var r1 = await _cache.TryGetAsync(key);
        var r2 = await _cache.TryGetAsync(key);
        var r3 = await _cache.TryGetAsync(key);

        r1.Entry!.HitCount.Should().Be(1);
        r2.Entry!.HitCount.Should().Be(2);
        r3.Entry!.HitCount.Should().Be(3);
    }

    [Fact]
    public async Task DifferentKeys_DoNotCollide()
    {
        await _cache.PutAsync(MakeKey(user: "u1"), "r1");
        await _cache.PutAsync(MakeKey(user: "u2"), "r2");

        (await _cache.TryGetAsync(MakeKey(user: "u1"))).Entry!.ResponseText.Should().Be("r1");
        (await _cache.TryGetAsync(MakeKey(user: "u2"))).Entry!.ResponseText.Should().Be("r2");
    }

    [Fact]
    public async Task Put_IdempotentReplacement()
    {
        var key = MakeKey();
        await _cache.PutAsync(key, "first");
        await _cache.PutAsync(key, "second");

        var result = await _cache.TryGetAsync(key);
        result.Entry!.ResponseText.Should().Be("second");
        // Hit count resets on replacement (INSERT OR REPLACE with hit_count = 0).
        result.Entry.HitCount.Should().Be(1);
    }

    [Fact]
    public async Task Prune_EvictsExpiredEntries()
    {
        var key = MakeKey();
        await _cache.PutAsync(key, "x");

        // Backdate via direct SQL so we don't have to wait.
        BackdateAllEntries(TimeSpan.FromDays(30));

        var deleted = await _cache.PruneAsync(ttl: TimeSpan.FromDays(7));
        deleted.Should().Be(1);

        var result = await _cache.TryGetAsync(key);
        result.IsHit.Should().BeFalse();
    }

    [Fact]
    public async Task Prune_DoesNotEvictFreshEntries()
    {
        var key = MakeKey();
        await _cache.PutAsync(key, "x");

        var deleted = await _cache.PruneAsync(ttl: TimeSpan.FromDays(7));
        deleted.Should().Be(0);

        var result = await _cache.TryGetAsync(key);
        result.IsHit.Should().BeTrue();
    }

    [Fact]
    public async Task SchemaMismatch_RecreatesTable()
    {
        // Insert an entry, then re-open the DB with a forced wrong user_version.
        var key = MakeKey();
        await _cache.PutAsync(key, "x");
        (await _cache.TryGetAsync(key)).IsHit.Should().BeTrue();

        // Force schema mismatch via direct PRAGMA.
        using (var conn = new Microsoft.Data.Sqlite.SqliteConnection(
            $"Data Source={_dbPath};Cache=Shared"))
        {
            conn.Open();
            using var cmd = conn.CreateCommand();
            cmd.CommandText = "PRAGMA user_version = 999;";
            cmd.ExecuteNonQuery();
        }

        // Re-open: constructor must drop + recreate; entry is gone.
        var fresh = new SqliteResponseCache(_dbPath, logger: null);
        var result = await fresh.TryGetAsync(key);
        result.IsHit.Should().BeFalse();
        result.MissReason.Should().Be(CacheMissReason.KeyNotFound);
    }

    private void BackdateAllEntries(TimeSpan offset)
    {
        using var conn = new Microsoft.Data.Sqlite.SqliteConnection(
            $"Data Source={_dbPath};Cache=Shared");
        conn.Open();
        var past = DateTime.UtcNow.Subtract(offset).ToString("O",
            System.Globalization.CultureInfo.InvariantCulture);
        using var cmd = conn.CreateCommand();
        cmd.CommandText = "UPDATE response_cache SET created_at_utc = $p;";
        cmd.Parameters.AddWithValue("$p", past);
        cmd.ExecuteNonQuery();
    }
}
