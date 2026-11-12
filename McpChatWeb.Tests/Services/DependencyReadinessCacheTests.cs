using System;
using System.IO;
using System.Threading;
using System.Threading.Tasks;
using McpChatWeb.Services;
using Xunit;

namespace McpChatWeb.Tests.Services;

public sealed class DependencyReadinessCacheTests : IDisposable
{
    private readonly string _root = Directory.CreateTempSubdirectory("readiness-cache-").FullName;
    private readonly string _source;
    private readonly string _facts;
    private int _assessments;

    public DependencyReadinessCacheTests()
    {
        _source = Directory.CreateDirectory(Path.Combine(_root, "source")).FullName;
        _facts = Directory.CreateDirectory(Path.Combine(_root, "facts")).FullName;
    }

    public void Dispose() => Directory.Delete(_root, recursive: true);

    private Task<DependencyReadiness> Assess(CancellationToken _)
    {
        Interlocked.Increment(ref _assessments);
        return Task.FromResult(new DependencyReadiness([], _source, _facts, DateTime.UtcNow));
    }

    [Fact]
    public async Task Unchanged_inputs_reuse_the_previous_assessment()
    {
        using var cache = new DependencyReadinessCache();
        var first = await cache.GetAsync(_source, _facts, Assess);
        var second = await cache.GetAsync(_source, _facts, Assess);

        Assert.Same(first, second);
        Assert.Equal(1, _assessments);
    }

    [Theory]
    [InlineData("source")]
    [InlineData("facts")]
    public async Task A_file_change_under_a_watched_root_forces_a_fresh_assessment(string root)
    {
        using var cache = new DependencyReadinessCache();
        var first = await cache.GetAsync(_source, _facts, Assess);

        var nested = Directory.CreateDirectory(Path.Combine(_root, root, "nested")).FullName;
        await File.WriteAllTextAsync(Path.Combine(nested, "PROG.cbl"), "changed");

        // Watcher events are asynchronous; a stale result is a failure, not something to wait out.
        DependencyReadiness? fresh = null;
        for (var i = 0; i < 50 && (fresh is null || ReferenceEquals(fresh, first)); i++)
        {
            await Task.Delay(100);
            fresh = await cache.GetAsync(_source, _facts, Assess);
        }
        Assert.NotSame(first, fresh);
    }

    [Fact]
    public async Task A_missing_root_is_never_cached()
    {
        using var cache = new DependencyReadinessCache();
        var missing = Path.Combine(_root, "absent");
        await cache.GetAsync(_source, missing, Assess);
        await cache.GetAsync(_source, missing, Assess);

        Assert.Equal(2, _assessments);
    }

    [Fact]
    public async Task An_extra_watched_root_does_not_stand_in_for_a_missing_facts_root()
    {
        var tools = Directory.CreateDirectory(Path.Combine(_root, "tools")).FullName;
        using var cache = new DependencyReadinessCache([tools]);
        var missing = Path.Combine(_root, "absent");
        await cache.GetAsync(_source, missing, Assess);
        Directory.CreateDirectory(missing);
        await cache.GetAsync(_source, missing, Assess);

        Assert.Equal(2, _assessments);
    }
    [Fact]
    public async Task An_edit_to_a_referenced_input_outside_the_watched_roots_forces_a_fresh_assessment()
    {
        var profiles = Directory.CreateDirectory(Path.Combine(_root, "profiles")).FullName;
        var profile = Path.Combine(profiles, "target-port.json");
        await File.WriteAllTextAsync(profile, "{}");
        using var cache = new DependencyReadinessCache();
        await cache.GetAsync(_source, _facts, Assess, referencedInputs: _ => [profile]);
        // The first result was read before the profile was watched, so it is not reused.
        var watched = await cache.GetAsync(_source, _facts, Assess, referencedInputs: _ => [profile]);
        Assert.Same(watched, await cache.GetAsync(_source, _facts, Assess, referencedInputs: _ => [profile]));

        await File.WriteAllTextAsync(profile, "{\"changed\":true}");
        DependencyReadiness? fresh = null;
        for (var i = 0; i < 50 && (fresh is null || ReferenceEquals(fresh, watched)); i++)
        {
            await Task.Delay(100);
            fresh = await cache.GetAsync(_source, _facts, Assess, referencedInputs: _ => [profile]);
        }
        Assert.NotSame(watched, fresh);
    }

    [Fact]
    public async Task A_referenced_input_in_a_missing_directory_is_never_cached()
    {
        var absent = Path.Combine(_root, "absent", "contract.json");
        using var cache = new DependencyReadinessCache();
        await cache.GetAsync(_source, _facts, Assess, referencedInputs: _ => [absent]);
        await cache.GetAsync(_source, _facts, Assess, referencedInputs: _ => [absent]);
        await cache.GetAsync(_source, _facts, Assess, referencedInputs: _ => [absent]);

        Assert.Equal(3, _assessments);
    }

    [Fact]
    public async Task Invalidate_discards_the_cached_assessment()
    {
        using var cache = new DependencyReadinessCache();
        var first = await cache.GetAsync(_source, _facts, Assess);
        cache.Invalidate(_source, _facts);

        Assert.NotSame(first, await cache.GetAsync(_source, _facts, Assess));
        Assert.Equal(2, _assessments);
    }
}
