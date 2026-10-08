using System.ClientModel;
using System.Net;
using System.Text.RegularExpressions;

namespace CobolToQuarkusMigration.Agents.Infrastructure;

/// <summary>
/// Decides what a failed model call was and how long to wait before trying again.
/// Every agent's retry loop asks this class, so a 429 is recognised the same way
/// whichever provider or agent produced it.
/// </summary>
public static class LlmErrorClassifier
{
    /// <summary>Cooldown applied when a 429 carries no Retry-After header.</summary>
    public static readonly TimeSpan DefaultRateLimitCooldown = TimeSpan.FromSeconds(15);

    private static readonly Regex RetryAfterInMessage = new(
        @"(?:retry|try again)\s+(?:after|in)\s+(\d+(?:\.\d+)?)\s*(ms|milliseconds?|s|sec|seconds?)\b",
        RegexOptions.IgnoreCase | RegexOptions.Compiled);

    /// <summary>True when the provider rejected the call for exceeding a rate or quota limit.</summary>
    public static bool IsRateLimit(Exception ex)
    {
        for (var e = ex; e is not null; e = e.InnerException)
        {
            if (e is RateLimitedException) return true;
            if (StatusOf(e) == 429) return true;

            var message = e.Message.ToLowerInvariant();
            if (message.Contains("rate limit") || message.Contains("ratelimit") ||
                message.Contains("429") || message.Contains("too many requests") ||
                message.Contains("toomanyrequests") || message.Contains("quota exceeded"))
                return true;
        }
        return false;
    }

    /// <summary>True for failures worth retrying soon: timeouts, dropped connections, 5xx.</summary>
    public static bool IsTransient(Exception ex)
    {
        if (IsRateLimit(ex)) return false;

        // Type first. A timeout is transient however its message happens to be worded, and the
        // Copilot client's own timeout says "did not respond within 5 minutes", in which the
        // substring checks below never find the word "timeout".
        if (ex is TimeoutException or HttpRequestException or TaskCanceledException)
            return true;
        if (StatusOf(ex) is >= 500 and <= 599) return true;

        var message = ex.Message.ToLowerInvariant();
        return message.Contains("timeout") ||
               message.Contains("temporarily unavailable") ||
               message.Contains("service unavailable") ||
               message.Contains("502") ||
               message.Contains("503") ||
               message.Contains("504") ||
               message.Contains("connection") ||
               // The Copilot CLI reports a network that is missing or not yet back as a failure
               // to reach the model catalogue. A laptop waking from sleep produces exactly this,
               // and treating it as permanent drops the program from the run for good.
               message.Contains("failed to list models");
    }

    /// <summary>True when the provider's content filter blocked the prompt or the answer.</summary>
    public static bool IsContentFilter(Exception ex)
    {
        var message = ex.Message.ToLowerInvariant();
        return message.Contains("content_filter") ||
               message.Contains("content filter") ||
               message.Contains("filtered") ||
               message.Contains("content management policy");
    }

    /// <summary>
    /// The wait the provider asked for, read from the exception's response headers or,
    /// failing that, from its message. Null when the provider did not say.
    /// </summary>
    public static TimeSpan? GetRetryAfter(Exception ex)
    {
        for (var e = ex; e is not null; e = e.InnerException)
        {
            if (e is RateLimitedException { RetryAfter: { } known }) return known;

            var fromHeaders = e switch
            {
                ClientResultException cre => FromHeaders(name =>
                    cre.GetRawResponse() is { } r && r.Headers.TryGetValue(name, out var v) ? v : null),
                Azure.RequestFailedException rfe => FromHeaders(name =>
                    rfe.GetRawResponse() is { } r && r.Headers.TryGetValue(name, out var v) ? v : null),
                _ => null
            };
            if (fromHeaders is not null) return fromHeaders;

            var match = RetryAfterInMessage.Match(e.Message);
            if (match.Success && double.TryParse(match.Groups[1].Value,
                    System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture, out var amount))
            {
                return match.Groups[2].Value.StartsWith("m", StringComparison.OrdinalIgnoreCase)
                    ? TimeSpan.FromMilliseconds(amount)
                    : TimeSpan.FromSeconds(amount);
            }
        }
        return null;
    }

    /// <summary>
    /// Reads Retry-After from a header lookup, preferring Azure's millisecond header.
    /// </summary>
    public static TimeSpan? FromHeaders(Func<string, string?> header)
    {
        if (double.TryParse(header("retry-after-ms"), System.Globalization.NumberStyles.Float,
                System.Globalization.CultureInfo.InvariantCulture, out var ms) && ms >= 0)
            return TimeSpan.FromMilliseconds(ms);
        return ParseRetryAfter(header("Retry-After"));
    }

    /// <summary>
    /// Parses an HTTP Retry-After header (seconds or HTTP-date). Null if missing or unparseable.
    /// </summary>
    public static TimeSpan? ParseRetryAfter(string? headerValue)
    {
        if (string.IsNullOrWhiteSpace(headerValue)) return null;

        if (int.TryParse(headerValue, out var seconds) && seconds >= 0)
            return TimeSpan.FromSeconds(seconds);

        if (DateTimeOffset.TryParse(headerValue, System.Globalization.CultureInfo.InvariantCulture,
                System.Globalization.DateTimeStyles.AssumeUniversal, out var when))
        {
            var delta = when - DateTimeOffset.UtcNow;
            return delta > TimeSpan.Zero ? delta : TimeSpan.Zero;
        }

        return null;
    }

    /// <summary>
    /// How long to wait before retry <paramref name="attempt"/> (1-based) after a 429.
    /// Honours the provider's Retry-After; without one, backs off 8s, 16s, 32s.
    /// Returns null when the provider asked for longer than <paramref name="maxWait"/>,
    /// meaning the caller should stop retrying this unit of work.
    /// </summary>
    public static TimeSpan? RateLimitDelay(int attempt, TimeSpan? retryAfter, TimeSpan maxWait)
    {
        if (retryAfter is { } asked)
            return asked > maxWait ? null : asked;

        var backoff = TimeSpan.FromSeconds(Math.Pow(2, Math.Max(1, attempt) + 2));
        return backoff > maxWait ? maxWait : backoff;
    }

    /// <summary>Exponential back-off for transient failures: 2s, 4s, 8s.</summary>
    public static TimeSpan TransientDelay(int attempt) =>
        TimeSpan.FromSeconds(Math.Pow(2, Math.Max(1, attempt)));

    private static int? StatusOf(Exception e) => e switch
    {
        HttpRequestException { StatusCode: { } code } => (int)code,
        ClientResultException cre when cre.Status > 0 => cre.Status,
        Azure.RequestFailedException rfe when rfe.Status > 0 => rfe.Status,
        _ => null
    };
}
