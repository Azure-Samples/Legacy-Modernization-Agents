namespace CobolToQuarkusMigration.Agents.Infrastructure;

/// <summary>
/// Provider-agnostic rate-limit gate. Implementations enforce TPM/RPM budgets
/// for a single provider in-process. See docs/throttling-and-cache-design.md §4.
/// </summary>
/// <remarks>
/// Lifecycle for one call:
/// <list type="number">
/// <item><see cref="AcquireAsync"/> — reserves capacity for the estimate.</item>
/// <item>Caller issues the HTTP request.</item>
/// <item><see cref="RecordUsage"/> — reconciles estimate against actual usage.</item>
/// <item>(on 429) <see cref="NoteRateLimitResponse"/> — installs an adaptive cooldown.</item>
/// </list>
/// Failover, cross-process coordination, and persistent buckets are out of scope.
/// </remarks>
public interface IRateLimiter
{
    /// <summary>
    /// Waits until the bucket has capacity for <paramref name="estimatedTokens"/> tokens
    /// and one request slot. The estimate is also <i>reserved</i> so that concurrent
    /// callers do not all pass the gate before any usage is recorded.
    /// </summary>
    /// <param name="estimatedTokens">Best-effort total-token estimate (input + max output).</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    Task AcquireAsync(int estimatedTokens, CancellationToken cancellationToken = default);

    /// <summary>
    /// Records the actual token count once the response is in. Releases any
    /// over-reservation back to the bucket.
    /// </summary>
    void RecordUsage(int actualTokens);

    /// <summary>
    /// Tells the limiter the provider asked us to back off for <paramref name="retryAfter"/>.
    /// Future <see cref="AcquireAsync"/> calls will wait until the cooldown expires
    /// (subject to the caller's wait-ceiling policy).
    /// </summary>
    void NoteRateLimitResponse(TimeSpan retryAfter);
}
