namespace CobolToQuarkusMigration.Agents.Infrastructure;

/// <summary>
/// Point-in-time view of a single provider's rate-limit state. Used for
/// observability (structured logs). Cheap to snapshot — no allocations
/// beyond this record itself.
/// </summary>
/// <param name="CurrentTpm">Tokens consumed in the rolling 60-second window (includes outstanding reservations).</param>
/// <param name="TpmLimit">Soft TPM cap (after safety factor).</param>
/// <param name="CurrentRpm">Requests issued in the rolling 60-second window (includes outstanding reservations).</param>
/// <param name="RpmLimit">Soft RPM cap (after safety factor).</param>
/// <param name="OutstandingReservations">Number of in-flight reservations that have not yet been committed or released.</param>
/// <param name="CooldownRemainingMs">Milliseconds remaining on an adaptive cooldown installed by <see cref="IRateLimiter.NoteRateLimitResponse"/>; 0 if no cooldown.</param>
public readonly record struct LimiterSnapshot(
    long CurrentTpm,
    long TpmLimit,
    int CurrentRpm,
    int RpmLimit,
    int OutstandingReservations,
    long CooldownRemainingMs);

/// <summary>
/// Capability hint exposed by limiters that can provide a cheap snapshot.
/// Optional — callers tolerate limiters that do not implement this.
/// </summary>
public interface ILimiterObservable
{
    LimiterSnapshot Snapshot();
}
