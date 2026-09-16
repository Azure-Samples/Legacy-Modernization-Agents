namespace CobolToQuarkusMigration.Agents.Infrastructure;

/// <summary>
/// Disposable reservation handle returned by <see cref="IRateLimiter.AcquireAsync"/>.
/// One of <see cref="Commit"/> or <see cref="Cancel"/> must be called before
/// disposal (Dispose treats unsettled reservations as cancelled).
/// </summary>
public interface IRateLimitReservation : IDisposable
{
    /// <summary>Records actual token usage and releases the reservation.</summary>
    void Commit(int actualTokens);

    /// <summary>Releases the reservation without recording any usage.</summary>
    void Cancel();
}

/// <summary>
/// Provider-agnostic rate-limit gate. Implementations enforce TPM/RPM budgets
/// for a single provider in-process. See docs/throttling-and-cache-design.md §4.
/// </summary>
/// <remarks>
/// Lifecycle for one call:
/// <list type="number">
/// <item><see cref="AcquireAsync"/> — waits for capacity and reserves the estimate.</item>
/// <item>Caller issues the request.</item>
/// <item>On success: <c>reservation.Commit(actualTokens)</c>.</item>
/// <item>On failure / cancel: <c>reservation.Cancel()</c> (or just let <c>using</c> dispose it).</item>
/// </list>
/// Returning a handle avoids the AsyncLocal-asymmetry trap where reservation
/// state set inside an async method does not flow back to the caller.
/// </remarks>
public interface IRateLimiter
{
    /// <summary>
    /// Waits until the bucket has capacity for <paramref name="estimatedTokens"/>
    /// tokens and one request slot, reserves them, and returns a handle the
    /// caller must commit or cancel.
    /// </summary>
    Task<IRateLimitReservation> AcquireAsync(int estimatedTokens, CancellationToken cancellationToken = default);

    /// <summary>
    /// Tells the limiter the provider asked us to back off for <paramref name="retryAfter"/>.
    /// Future <see cref="AcquireAsync"/> calls will wait until the cooldown expires
    /// (subject to the caller's wait-ceiling policy).
    /// </summary>
    void NoteRateLimitResponse(TimeSpan retryAfter);
}

