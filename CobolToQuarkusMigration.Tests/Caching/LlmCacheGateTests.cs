using CobolToQuarkusMigration.Agents.Infrastructure.Caching;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Caching;

public class LlmCacheGateTests : IDisposable
{
    private readonly string? _previousEnabled;
    private readonly string? _previousDbPath;
    private readonly string _tempDbPath;

    public LlmCacheGateTests()
    {
        _previousEnabled = Environment.GetEnvironmentVariable("_LLM_CACHE_ENABLED");
        _previousDbPath = Environment.GetEnvironmentVariable("_LLM_CACHE_DB");
        _tempDbPath = Path.Combine(Path.GetTempPath(), $"gate-test-{Guid.NewGuid():N}.db");
        // Each test starts clean; the gate is process-static.
        LlmCacheGate.ResetForTests(null);
    }

    public void Dispose()
    {
        // Restore env.
        Environment.SetEnvironmentVariable("_LLM_CACHE_ENABLED", _previousEnabled);
        Environment.SetEnvironmentVariable("_LLM_CACHE_DB", _previousDbPath);
        LlmCacheGate.ResetForTests(null);

        Microsoft.Data.Sqlite.SqliteConnection.ClearAllPools();
        if (File.Exists(_tempDbPath)) File.Delete(_tempDbPath);
        var wal = _tempDbPath + "-wal"; if (File.Exists(wal)) File.Delete(wal);
        var shm = _tempDbPath + "-shm"; if (File.Exists(shm)) File.Delete(shm);
    }

    [Fact]
    public void DisabledByDefault_WhenEnvVarUnset()
    {
        Environment.SetEnvironmentVariable("_LLM_CACHE_ENABLED", null);
        LlmCacheGate.Enabled.Should().BeFalse();
        LlmCacheGate.Cache.Should().BeNull();
    }

    [Fact]
    public void DisabledExplicitly_WhenEnvVarFalse()
    {
        Environment.SetEnvironmentVariable("_LLM_CACHE_ENABLED", "false");
        LlmCacheGate.Enabled.Should().BeFalse();
        LlmCacheGate.Cache.Should().BeNull();
    }

    [Fact]
    public void Enabled_BuildsCacheAtCustomPath()
    {
        Environment.SetEnvironmentVariable("_LLM_CACHE_ENABLED", "true");
        Environment.SetEnvironmentVariable("_LLM_CACHE_DB", _tempDbPath);
        LlmCacheGate.Enabled.Should().BeTrue();
        LlmCacheGate.Cache.Should().NotBeNull();
        File.Exists(_tempDbPath).Should().BeTrue("the cache constructor creates the file eagerly");
    }

    [Fact]
    public void EnsureCache_IsIdempotent()
    {
        Environment.SetEnvironmentVariable("_LLM_CACHE_ENABLED", "true");
        Environment.SetEnvironmentVariable("_LLM_CACHE_DB", _tempDbPath);

        var c1 = LlmCacheGate.EnsureCache(null);
        var c2 = LlmCacheGate.EnsureCache(null);
        c1.Should().BeSameAs(c2, "the gate is process-static");
    }
}
