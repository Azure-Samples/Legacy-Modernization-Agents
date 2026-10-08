using System.Collections.Concurrent;
using CobolToQuarkusMigration.Models;
using Microsoft.Extensions.Logging;

namespace CobolToQuarkusMigration.Agents.Infrastructure;

/// <summary>
/// The one rate limiter for model calls. A sliding one-minute window enforces the
/// tokens-per-minute and requests-per-minute budgets of a model profile, and a 429
/// from the provider puts every caller of the same deployment into a shared cooldown
/// instead of letting the other agents keep hitting it.
/// </summary>
/// <remarks>
/// A reservation counts against the window as soon as it is granted, so parallel
/// agents cannot all pass the check before any of them has recorded usage. Commit
/// replaces the estimate with the actual token count; Cancel removes it.
/// </remarks>
public sealed class LlmRateLimiter : IRateLimiter
{
    private static readonly ConcurrentDictionary<string, LlmRateLimiter> SharedLimiters =
        new(StringComparer.OrdinalIgnoreCase);

    private readonly int _tokensPerMinute;
    private readonly int _requestsPerMinute;
    private readonly ILogger? _logger;
    private readonly object _lock = new();
    private readonly List<Entry> _window = new();
    private long _nextId;
    private DateTime _cooldownUntil = DateTime.MinValue;

    private sealed class Entry
    {
        public required long Id { get; init; }
        public required DateTime Time { get; init; }
        public int Tokens { get; set; }
    }

    /// <param name="tokensPerMinute">Provider TPM quota; zero or less disables the token check.</param>
    /// <param name="requestsPerMinute">Provider RPM quota; zero or less disables the request check.</param>
    /// <param name="safetyMargin">Fraction of each quota to use (clamped to 0.5–0.99).</param>
    public LlmRateLimiter(int tokensPerMinute, int requestsPerMinute, double safetyMargin = 0.90, ILogger? logger = null)
    {
        var margin = Math.Clamp(safetyMargin, 0.5, 0.99);
        _tokensPerMinute = tokensPerMinute > 0 ? (int)(tokensPerMinute * margin) : 0;
        _requestsPerMinute = requestsPerMinute > 0 ? (int)(requestsPerMinute * margin) : 0;
        _logger = logger;
    }

    /// <summary>
    /// Returns the limiter shared by every caller of <paramref name="key"/> in this process.
    /// The first caller's profile sets the budget.
    /// </summary>
    public static LlmRateLimiter Shared(string key, ModelProfileSettings profile, double safetyMargin = 0.90, ILogger? logger = null) =>
        SharedLimiters.GetOrAdd(key, _ => new LlmRateLimiter(profile.TokensPerMinute, profile.RequestsPerMinute, safetyMargin, logger));

    /// <summary>Time left on the provider cooldown, or zero.</summary>
    public TimeSpan CooldownRemaining
    {
        get
        {
            lock (_lock)
            {
                var left = _cooldownUntil - DateTime.UtcNow;
                return left > TimeSpan.Zero ? left : TimeSpan.Zero;
            }
        }
    }

    public async Task<IRateLimitReservation> AcquireAsync(int estimatedTokens, CancellationToken cancellationToken = default)
    {
        estimatedTokens = Math.Max(0, estimatedTokens);
        while (true)
        {
            cancellationToken.ThrowIfCancellationRequested();
            TimeSpan wait;
            string reason;

            lock (_lock)
            {
                var now = DateTime.UtcNow;
                Prune(now);
                var tokensUsed = _window.Sum(e => e.Tokens);

                if (now < _cooldownUntil)
                {
                    wait = _cooldownUntil - now;
                    reason = "provider asked to back off";
                }
                // An empty window always admits one call, so a request larger than the
                // whole budget is sent alone instead of waiting forever.
                else if (_tokensPerMinute > 0 && _window.Count > 0 && tokensUsed + estimatedTokens > _tokensPerMinute)
                {
                    wait = _window[0].Time.AddMinutes(1) - now;
                    reason = $"TPM {tokensUsed:N0}+{estimatedTokens:N0} > {_tokensPerMinute:N0}";
                }
                else if (_requestsPerMinute > 0 && _window.Count + 1 > _requestsPerMinute)
                {
                    wait = _window[0].Time.AddMinutes(1) - now;
                    reason = $"RPM {_window.Count}+1 > {_requestsPerMinute}";
                }
                else
                {
                    var entry = new Entry { Id = ++_nextId, Time = now, Tokens = estimatedTokens };
                    _window.Add(entry);
                    return new Reservation(this, entry.Id);
                }
            }

            if (wait < TimeSpan.FromMilliseconds(100)) wait = TimeSpan.FromMilliseconds(100);
            _logger?.LogInformation("Rate limit: waiting {Wait:F1}s ({Reason})", wait.TotalSeconds, reason);
            await Task.Delay(wait, cancellationToken);
        }
    }

    public void NoteRateLimitResponse(TimeSpan retryAfter)
    {
        if (retryAfter <= TimeSpan.Zero) return;
        lock (_lock)
        {
            var until = DateTime.UtcNow + retryAfter;
            if (until > _cooldownUntil) _cooldownUntil = until;
        }
        _logger?.LogWarning("Rate limit: provider returned 429, pausing calls for {Seconds:F0}s", retryAfter.TotalSeconds);
    }

    private void Prune(DateTime now)
    {
        var cutoff = now.AddMinutes(-1);
        _window.RemoveAll(e => e.Time < cutoff);
    }

    private void Settle(long id, int? actualTokens)
    {
        lock (_lock)
        {
            var index = _window.FindIndex(e => e.Id == id);
            if (index < 0) return;
            if (actualTokens is { } tokens) _window[index].Tokens = Math.Max(0, tokens);
            else _window.RemoveAt(index);
        }
    }

    private sealed class Reservation : IRateLimitReservation
    {
        private readonly LlmRateLimiter _owner;
        private readonly long _id;
        private int _settled;

        public Reservation(LlmRateLimiter owner, long id)
        {
            _owner = owner;
            _id = id;
        }

        public void Commit(int actualTokens)
        {
            if (Interlocked.Exchange(ref _settled, 1) == 0) _owner.Settle(_id, actualTokens);
        }

        public void Cancel()
        {
            if (Interlocked.Exchange(ref _settled, 1) == 0) _owner.Settle(_id, null);
        }

        public void Dispose() => Cancel();
    }
}
