using CobolToQuarkusMigration.Agents.Infrastructure;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Infrastructure;

public class RateLimitTrackerTests
{
    [Fact]
    public async Task Acquire_ReservesAndShowsInSnapshot()
    {
        var t = new RateLimitTracker(tokensPerMinute: 1_000_000, requestsPerMinute: 1000, logger: null);
        t.Snapshot().OutstandingReservations.Should().Be(0);

        var id = await t.WaitForCapacityAsync(10_000, default);

        var after = t.Snapshot();
        after.OutstandingReservations.Should().Be(1);
        after.CurrentTpm.Should().Be(10_000, "reservation must count toward TPM immediately");
        after.CurrentRpm.Should().Be(1);
        id.Should().BeGreaterThan(0);
    }

    [Fact]
    public async Task RecordUsage_ReplacesReservationWithActual()
    {
        var t = new RateLimitTracker(1_000_000, 1000, logger: null);
        var id = await t.WaitForCapacityAsync(20_000, default);
        t.RecordUsage(id, actualTokens: 5_000);

        var snap = t.Snapshot();
        snap.OutstandingReservations.Should().Be(0);
        snap.CurrentTpm.Should().Be(5_000, "actual must replace reservation");
        snap.CurrentRpm.Should().Be(1);
    }

    [Fact]
    public async Task ReleaseReservation_DropsBothTokensAndRequest()
    {
        var t = new RateLimitTracker(1_000_000, 1000, logger: null);
        var id = await t.WaitForCapacityAsync(10_000, default);
        t.ReleaseReservation(id);

        var snap = t.Snapshot();
        snap.OutstandingReservations.Should().Be(0);
        snap.CurrentTpm.Should().Be(0, "release must drop reserved tokens");
        snap.CurrentRpm.Should().Be(0, "release must drop the request slot");
    }

    [Fact]
    public void NoteRateLimitResponse_InstallsCooldown()
    {
        var t = new RateLimitTracker(1_000_000, 1000, logger: null);
        t.NoteRateLimitResponse(TimeSpan.FromSeconds(5));

        var snap = t.Snapshot();
        snap.CooldownRemainingMs.Should().BeGreaterThan(4_000);
        snap.CooldownRemainingMs.Should().BeLessThanOrEqualTo(5_000);
    }

    [Fact]
    public async Task Cooldown_BlocksAcquireUntilExpired()
    {
        var t = new RateLimitTracker(1_000_000, 1000, logger: null);
        t.NoteRateLimitResponse(TimeSpan.FromMilliseconds(300));

        var start = DateTime.UtcNow;
        _ = await t.WaitForCapacityAsync(1_000, default);
        var elapsed = DateTime.UtcNow - start;

        elapsed.Should().BeGreaterThanOrEqualTo(TimeSpan.FromMilliseconds(200));
    }

    [Fact]
    public async Task Reservation_AutoCancelsOnDispose()
    {
        IRateLimiter t = new RateLimitTracker(1_000_000, 1000, logger: null);
        using (await t.AcquireAsync(15_000))
        {
            ((RateLimitTracker)t).Snapshot().OutstandingReservations.Should().Be(1);
            // No Commit / Cancel — Dispose must release.
        }

        ((RateLimitTracker)t).Snapshot().OutstandingReservations
            .Should().Be(0, "Dispose should auto-cancel");
    }

    [Fact]
    public async Task Reservation_CommitReplacesEstimate()
    {
        var t = new RateLimitTracker(1_000_000, 1000, logger: null);
        using (var res = await ((IRateLimiter)t).AcquireAsync(20_000))
        {
            res.Commit(7_500);
        }
        t.Snapshot().CurrentTpm.Should().Be(7_500);
    }

    [Fact]
    public async Task ConcurrentAcquires_DoNotOverAdmitBeyondTpm()
    {
        // Tight cap: 10K TPM with 0.9 safety = 9000 effective. Three 3K reservations fit.
        // The fourth must wait — we cancel before the timer expires.
        var t = new RateLimitTracker(tokensPerMinute: 10_000, requestsPerMinute: 1000, logger: null);

        var ids = await Task.WhenAll(
            t.WaitForCapacityAsync(3_000, default),
            t.WaitForCapacityAsync(3_000, default),
            t.WaitForCapacityAsync(3_000, default));

        var snap = t.Snapshot();
        snap.OutstandingReservations.Should().Be(3);
        snap.CurrentTpm.Should().Be(9_000);

        using var cts = new CancellationTokenSource(TimeSpan.FromMilliseconds(50));
        var act = async () => await t.WaitForCapacityAsync(3_000, cts.Token);
        await act.Should().ThrowAsync<OperationCanceledException>(
            because: "TPM is fully reserved; fourth concurrent acquire must wait, not over-admit");

        t.Snapshot().OutstandingReservations.Should().Be(3, "the cancelled acquire never reserved");

        // Cleanup: release the three reservations to prove no leak.
        foreach (var id in ids) t.ReleaseReservation(id);
        t.Snapshot().OutstandingReservations.Should().Be(0);
    }
}
