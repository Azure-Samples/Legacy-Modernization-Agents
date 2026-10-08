using CobolToQuarkusMigration.Agents.Infrastructure;
using CobolToQuarkusMigration.Models;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Infrastructure;

public class LlmRateLimiterTests
{
    private static async Task<bool> AdmitsWithin(LlmRateLimiter limiter, int tokens, int ms)
    {
        using var cts = new CancellationTokenSource(ms);
        try
        {
            using var reservation = await limiter.AcquireAsync(tokens, cts.Token);
            return true;
        }
        catch (OperationCanceledException)
        {
            return false;
        }
    }

    [Fact]
    public async Task ReservationCountsBeforeCommit_SoParallelCallersCannotOversubscribe()
    {
        var limiter = new LlmRateLimiter(tokensPerMinute: 100, requestsPerMinute: 0, safetyMargin: 0.99);
        var first = await limiter.AcquireAsync(80);

        (await AdmitsWithin(limiter, 30, 300)).Should().BeFalse();

        first.Cancel();
        (await AdmitsWithin(limiter, 30, 300)).Should().BeTrue();
    }

    [Fact]
    public async Task OversizedRequest_IsAdmittedAloneInsteadOfWaitingForever()
    {
        var limiter = new LlmRateLimiter(tokensPerMinute: 100, requestsPerMinute: 0);
        (await AdmitsWithin(limiter, 10_000, 300)).Should().BeTrue();
    }

    [Fact]
    public async Task RequestsPerMinute_IsEnforced()
    {
        var limiter = new LlmRateLimiter(tokensPerMinute: 0, requestsPerMinute: 2, safetyMargin: 0.99);
        var first = await limiter.AcquireAsync(1);
        first.Commit(1);
        (await AdmitsWithin(limiter, 1, 300)).Should().BeFalse();
    }

    [Fact]
    public async Task A429_PausesEveryCallerUntilCooldownEnds()
    {
        var limiter = new LlmRateLimiter(tokensPerMinute: 0, requestsPerMinute: 0);
        limiter.NoteRateLimitResponse(TimeSpan.FromMilliseconds(600));

        limiter.CooldownRemaining.Should().BeGreaterThan(TimeSpan.Zero);
        (await AdmitsWithin(limiter, 1, 200)).Should().BeFalse();
        (await AdmitsWithin(limiter, 1, 2000)).Should().BeTrue();
    }

    [Fact]
    public void Shared_ReturnsOneLimiterPerKey()
    {
        var profile = new ModelProfileSettings();
        var key = Guid.NewGuid().ToString();
        LlmRateLimiter.Shared(key, profile).Should().BeSameAs(LlmRateLimiter.Shared(key, profile));
        LlmRateLimiter.Shared(key + "-other", profile).Should().NotBeSameAs(LlmRateLimiter.Shared(key, profile));
    }
}
