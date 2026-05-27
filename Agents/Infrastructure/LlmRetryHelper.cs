using Microsoft.Extensions.Logging;

namespace CobolToQuarkusMigration.Agents.Infrastructure;

/// <summary>
/// What a single call attempt produced. The retry helper does not interpret HTTP
/// itself — provider-specific code maps its response into one of these outcomes
/// and the helper decides whether to retry, wait, or surface the result.
/// </summary>
public abstract record CallOutcome
{
    /// <summary>Success — return the value to the caller.</summary>
    public sealed record Success<T>(T Value) : CallOutcome;

    /// <summary>HTTP 429. <paramref name="RetryAfter"/> is the parsed Retry-After header, if any.</summary>
    public sealed record RateLimited(TimeSpan? RetryAfter, string Reason) : CallOutcome;

    /// <summary>5xx or other transient HTTP failure that should back off and retry.</summary>
    public sealed record TransientFailure(string Reason) : CallOutcome;

    /// <summary>Non-retryable error. The exception (if any) will be surfaced.</summary>
    public sealed record Fatal(Exception? Exception, string Reason) : CallOutcome;
}

/// <summary>
/// Shared retry logic for provider clients. Honours Retry-After, applies a
/// per-call wait ceiling, and emits structured throttling logs.
/// </summary>
/// <remarks>
/// Design rules (see docs/throttling-and-cache-design.md §4):
/// <list type="bullet">
///   <item>429 with Retry-After ≤ wait ceiling: sleep, retry once, then surface <see cref="RateLimitedException"/>.</item>
///   <item>429 with Retry-After &gt; wait ceiling: fast-fail with <see cref="RateLimitedException"/>.</item>
///   <item>429 without Retry-After: 15s then 45s back-off, then surface <see cref="RateLimitedException"/>.</item>
///   <item>Transient (5xx etc.): exponential back-off with jitter, bounded.</item>
///   <item>Fatal: surface immediately, never silently swallow.</item>
/// </list>
/// </remarks>
public static class LlmRetryHelper
{
    /// <summary>Default per-call wait ceiling — overridable via constructor of the using client.</summary>
    public const int DefaultWaitCeilingSeconds = 120;

    /// <summary>Maximum number of transient (5xx) retries before giving up.</summary>
    private const int MaxTransientRetries = 3;

    /// <summary>Stable structured-log event name for grep-ability.</summary>
    public const string LogEventName = "LlmRateLimit";

    /// <summary>
    /// Runs <paramref name="attempt"/> with retry semantics described in the class docs.
    /// The attempt callback receives the attempt index (1-based) and must return a
    /// <see cref="CallOutcome"/> describing what happened — never throw for retryable
    /// conditions.
    /// </summary>
    public static async Task<T> ExecuteAsync<T>(
        string provider,
        string model,
        Func<int, CancellationToken, Task<CallOutcome>> attempt,
        IRateLimiter? limiter,
        TimeSpan waitCeiling,
        ILogger? logger,
        CancellationToken cancellationToken)
    {
        var rateLimited429Attempts = 0;
        var transientAttempts = 0;
        var attemptIndex = 0;

        while (true)
        {
            cancellationToken.ThrowIfCancellationRequested();
            attemptIndex++;

            CallOutcome outcome;
            try
            {
                outcome = await attempt(attemptIndex, cancellationToken);
            }
            catch (OperationCanceledException)
            {
                throw;
            }
            catch (Exception ex)
            {
                // The attempt callback should map exceptions to CallOutcome; anything
                // that leaks out is treated as fatal so we don't loop forever on bugs.
                outcome = new CallOutcome.Fatal(ex, ex.Message);
            }

            switch (outcome)
            {
                case CallOutcome.Success<T> ok:
                    return ok.Value;

                case CallOutcome.RateLimited rl:
                {
                    rateLimited429Attempts++;
                    limiter?.NoteRateLimitResponse(rl.RetryAfter ?? TimeSpan.FromSeconds(60));

                    // Fast-fail if the server told us to wait longer than the ceiling.
                    if (rl.RetryAfter is { } ra && ra > waitCeiling)
                    {
                        LogThrottle(logger, provider, model, statusCode: 429,
                            retryAfterSeconds: ra.TotalSeconds, waitMs: 0,
                            decision: "fast-fail-over-ceiling", reason: rl.Reason);
                        throw new RateLimitedException(provider, model, ra,
                            $"{provider}/{model} rate-limited for {ra.TotalSeconds:F0}s, exceeds wait ceiling {waitCeiling.TotalSeconds:F0}s.");
                    }

                    // Give up after 2 attempts on Retry-After-bearing responses
                    // (1 honoured wait + 1 retry); without header, allow 15s+45s.
                    var maxAttempts = rl.RetryAfter is null ? 3 : 2;
                    if (rateLimited429Attempts >= maxAttempts)
                    {
                        LogThrottle(logger, provider, model, statusCode: 429,
                            retryAfterSeconds: rl.RetryAfter?.TotalSeconds ?? 0, waitMs: 0,
                            decision: "give-up", reason: rl.Reason);
                        throw new RateLimitedException(provider, model, rl.RetryAfter,
                            $"{provider}/{model} rate-limited after {rateLimited429Attempts} attempts: {rl.Reason}");
                    }

                    var wait = rl.RetryAfter ?? (rateLimited429Attempts == 1
                        ? TimeSpan.FromSeconds(15)
                        : TimeSpan.FromSeconds(45));

                    LogThrottle(logger, provider, model, statusCode: 429,
                        retryAfterSeconds: rl.RetryAfter?.TotalSeconds ?? wait.TotalSeconds,
                        waitMs: wait.TotalMilliseconds,
                        decision: $"retry-after-wait-{(rl.RetryAfter is null ? "default" : "header")}",
                        reason: rl.Reason);

                    await Task.Delay(wait, cancellationToken);
                    break;
                }

                case CallOutcome.TransientFailure tf:
                {
                    transientAttempts++;
                    if (transientAttempts > MaxTransientRetries)
                    {
                        LogThrottle(logger, provider, model, statusCode: 0,
                            retryAfterSeconds: 0, waitMs: 0,
                            decision: "give-up-transient", reason: tf.Reason);
                        throw new HttpRequestException(
                            $"{provider}/{model} transient failure after {MaxTransientRetries} retries: {tf.Reason}");
                    }
                    // Exponential back-off with jitter: 1s, 2s, 4s (+/- 20%).
                    var baseSec = Math.Pow(2, transientAttempts - 1);
                    var jitter = (Random.Shared.NextDouble() * 0.4) - 0.2;
                    var wait = TimeSpan.FromSeconds(baseSec * (1.0 + jitter));
                    LogThrottle(logger, provider, model, statusCode: 0,
                        retryAfterSeconds: 0, waitMs: wait.TotalMilliseconds,
                        decision: $"transient-backoff-{transientAttempts}", reason: tf.Reason);
                    await Task.Delay(wait, cancellationToken);
                    break;
                }

                case CallOutcome.Fatal fatal:
                {
                    LogThrottle(logger, provider, model, statusCode: 0,
                        retryAfterSeconds: 0, waitMs: 0,
                        decision: "fatal", reason: fatal.Reason);
                    if (fatal.Exception is not null) throw fatal.Exception;
                    throw new InvalidOperationException(
                        $"{provider}/{model} fatal error: {fatal.Reason}");
                }

                default:
                    // CallOutcome.Success<U> for a U != T means the attempt produced the wrong type.
                    throw new InvalidOperationException(
                        $"LlmRetryHelper: attempt returned unexpected outcome {outcome.GetType().Name}");
            }
        }
    }

    /// <summary>
    /// Parse an HTTP Retry-After header (seconds-or-HTTP-date) into a TimeSpan.
    /// Returns null if the header is missing or unparseable.
    /// </summary>
    public static TimeSpan? ParseRetryAfter(string? headerValue)
    {
        if (string.IsNullOrWhiteSpace(headerValue)) return null;

        if (int.TryParse(headerValue, out var seconds) && seconds >= 0)
            return TimeSpan.FromSeconds(seconds);

        if (DateTimeOffset.TryParse(headerValue, out var when))
        {
            var delta = when - DateTimeOffset.UtcNow;
            return delta > TimeSpan.Zero ? delta : TimeSpan.Zero;
        }

        return null;
    }

    private static void LogThrottle(
        ILogger? logger, string provider, string model, int statusCode,
        double retryAfterSeconds, double waitMs, string decision, string reason)
    {
        logger?.LogWarning(
            "[{Event}] runId={RunId} correlationId={CorrelationId} provider={Provider} model={Model} " +
            "statusCode={StatusCode} retryAfterSeconds={RetryAfter:F1} waitMs={WaitMs:F0} " +
            "decision={Decision} reason={Reason}",
            LogEventName, LlmCorrelationContext.RunId, LlmCorrelationContext.CorrelationId,
            provider, model, statusCode, retryAfterSeconds, waitMs, decision, reason);
    }
}
