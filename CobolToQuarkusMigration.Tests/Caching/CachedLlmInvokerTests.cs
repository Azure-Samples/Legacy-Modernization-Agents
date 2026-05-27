using CobolToQuarkusMigration.Agents.Infrastructure.Caching;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Caching;

public class CachedLlmInvokerTests : IDisposable
{
    private readonly string _dbPath;
    private readonly SqliteResponseCache _cache;
    private int _invocationCount;

    public CachedLlmInvokerTests()
    {
        _dbPath = Path.Combine(Path.GetTempPath(), $"llm-cache-inv-{Guid.NewGuid():N}.db");
        _cache = new SqliteResponseCache(_dbPath, logger: null);
    }

    public void Dispose()
    {
        Microsoft.Data.Sqlite.SqliteConnection.ClearAllPools();
        if (File.Exists(_dbPath)) File.Delete(_dbPath);
        var wal = _dbPath + "-wal"; if (File.Exists(wal)) File.Delete(wal);
        var shm = _dbPath + "-shm"; if (File.Exists(shm)) File.Delete(shm);
    }

    private static CacheKey MakeKey() => CacheKey.Build(new CacheKey
    {
        ProviderKey = "azure-openai",
        Model = "gpt-5.3-codex",
        SystemPromptHash = "s",
        UserPromptHash = "u",
        ReasoningEffort = "high",
        ResponseFormat = "text",
        PromptTemplateId = "t",
        PromptTemplateVersion = "1",
        TargetLanguage = "java",
        FrameworkSettings = "q",
        SourceHash = "src",
        RektFactsHash = "rekt",
        GenerationSettingsHash = "g",
    });

    private Task<LlmInvocationResult> Invoke(CancellationToken _)
    {
        _invocationCount++;
        return Task.FromResult(LlmInvocationResult.Cacheable("the answer"));
    }

    [Fact]
    public async Task NullCache_BypassesAndInvokes()
    {
        var result = await CachedLlmInvoker.GetOrInvokeAsync(
            cache: null, enabled: true, isDeterministic: true,
            key: MakeKey(), invoke: Invoke, logger: null, cancellationToken: default);

        result.Should().Be("the answer");
        _invocationCount.Should().Be(1);
    }

    [Fact]
    public async Task Disabled_BypassesAndInvokes()
    {
        var result = await CachedLlmInvoker.GetOrInvokeAsync(
            cache: _cache, enabled: false, isDeterministic: true,
            key: MakeKey(), invoke: Invoke, logger: null, cancellationToken: default);

        result.Should().Be("the answer");
        _invocationCount.Should().Be(1);
        (await _cache.TryGetAsync(MakeKey())).IsHit.Should().BeFalse("disabled cache must not store");
    }

    [Fact]
    public async Task NonDeterministic_BypassesAndInvokes()
    {
        var result = await CachedLlmInvoker.GetOrInvokeAsync(
            cache: _cache, enabled: true, isDeterministic: false,
            key: MakeKey(), invoke: Invoke, logger: null, cancellationToken: default);

        result.Should().Be("the answer");
        _invocationCount.Should().Be(1);
        (await _cache.TryGetAsync(MakeKey())).IsHit.Should().BeFalse("non-deterministic must not store");
    }

    [Fact]
    public async Task FirstCallStores_SecondCallSkipsInvocation()
    {
        var k = MakeKey();
        var r1 = await CachedLlmInvoker.GetOrInvokeAsync(
            _cache, enabled: true, isDeterministic: true, k, Invoke, logger: null, default);
        var r2 = await CachedLlmInvoker.GetOrInvokeAsync(
            _cache, enabled: true, isDeterministic: true, k, Invoke, logger: null, default);

        r1.Should().Be("the answer");
        r2.Should().Be("the answer");
        _invocationCount.Should().Be(1, "second call must come from cache");
    }

    [Fact]
    public async Task IncompleteResult_NotStored()
    {
        Task<LlmInvocationResult> partial(CancellationToken _) =>
            Task.FromResult(new LlmInvocationResult("partial...", IsComplete: false, IsCacheable: false, "max_tokens"));

        var r1 = await CachedLlmInvoker.GetOrInvokeAsync(
            _cache, enabled: true, isDeterministic: true, MakeKey(), partial, logger: null, default);
        r1.Should().Be("partial...");

        // Second call must invoke again — nothing was stored.
        var hits = 0;
        Task<LlmInvocationResult> count(CancellationToken _) { hits++; return Task.FromResult(LlmInvocationResult.Cacheable("ok")); }
        var r2 = await CachedLlmInvoker.GetOrInvokeAsync(
            _cache, enabled: true, isDeterministic: true, MakeKey(), count, logger: null, default);
        r2.Should().Be("ok");
        hits.Should().Be(1);
    }

    [Fact]
    public async Task ExplicitNotCacheable_NotStored()
    {
        Task<LlmInvocationResult> notCacheable(CancellationToken _) =>
            Task.FromResult(LlmInvocationResult.NotCacheable("text", "policy"));

        await CachedLlmInvoker.GetOrInvokeAsync(
            _cache, enabled: true, isDeterministic: true, MakeKey(), notCacheable, logger: null, default);

        (await _cache.TryGetAsync(MakeKey())).IsHit.Should().BeFalse();
    }
}
