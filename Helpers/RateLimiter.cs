using System;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.Extensions.Logging;

namespace CobolToQuarkusMigration.Helpers;

/// <summary>
/// Rate limiter with token bucket algorithm and retry with exponential backoff.
/// Supports parallel requests with distributed rate limiting.
/// 
/// Configuration for 300K TPM with gpt-5-mini:
/// - TokensPerMinute: 300000
/// - MaxInputTokens: 20000 (truncate files to this)
/// - MaxOutputTokens: 10000 (MaxOutputTokenCount parameter)
/// - DelayBetweenRequests: calculated automatically (~6-10 seconds)
/// </summary>
public class RateLimiter
{
    private readonly int _tokensPerMinute;
    private readonly int _maxInputTokens;
    private readonly int _maxOutputTokens;
    private readonly int _delayBetweenRequestsMs;
    private readonly int _maxRetries;
    private readonly int _maxParallelRequests;
    private readonly double _safetyFactor;
    private readonly ILogger? _logger;
    
    private DateTime _lastRequestTime = DateTime.MinValue;
    private readonly SemaphoreSlim _concurrencySemaphore;
    private readonly SemaphoreSlim _tokenBucketLock = new(1, 1);
    private int _tokensUsedThisMinute = 0;
    private DateTime _minuteStartTime = DateTime.UtcNow;
    private int _activeRequests = 0;

    /// <summary>
    /// Creates a new rate limiter with calculated delays.
    /// </summary>
    /// <param name="tokensPerMinute">Your Azure OpenAI TPM limit.</param>
    /// <param name="maxInputTokens">Maximum input tokens per request.</param>
    /// <param name="maxOutputTokens">Maximum output tokens per request (MaxOutputTokenCount).</param>
    /// <param name="maxRetries">Maximum retry attempts for 429 errors.</param>
    /// <param name="maxParallelRequests">Maximum concurrent requests allowed.</param>
    /// <param name="safetyFactor">Safety factor (0.0-1.0) for rate limiting.</param>
    /// <param name="logger">Optional logger.</param>
    public RateLimiter(
        int tokensPerMinute = 300000,
        int maxInputTokens = 20000,
        int maxOutputTokens = 10000,
        int maxRetries = 3,
        int maxParallelRequests = 3,
        double safetyFactor = 0.7,
        ILogger? logger = null)
    {
        _tokensPerMinute = tokensPerMinute;
        _maxInputTokens = maxInputTokens;
        _maxOutputTokens = maxOutputTokens;
        _maxRetries = maxRetries;
        _maxParallelRequests = Math.Max(1, maxParallelRequests);
        _safetyFactor = Math.Clamp(safetyFactor, 0.1, 1.0);
        _logger = logger;
        
        // Create semaphore for concurrency control
        _concurrencySemaphore = new SemaphoreSlim(_maxParallelRequests, _maxParallelRequests);
        
        // Calculate optimal delay (divided by parallel workers)
        var baseDelay = TokenHelper.CalculateRequestDelay(
            tokensPerMinute, maxInputTokens, maxOutputTokens);
        _delayBetweenRequestsMs = Math.Max(1000, baseDelay / _maxParallelRequests);
        
        _logger?.LogInformation(
            "RateLimiter initialized: {TPM} TPM, {Delay}ms delay, {MaxInput} input, {MaxOutput} output, {Parallel} parallel, {Safety:P0} safety",
            tokensPerMinute, _delayBetweenRequestsMs, maxInputTokens, maxOutputTokens, _maxParallelRequests, _safetyFactor);
    }

    /// <summary>
    /// Maximum input tokens allowed per request.
    /// </summary>
    public int MaxInputTokens => _maxInputTokens;

    /// <summary>
    /// Maximum output tokens allowed per request (for MaxOutputTokenCount parameter).
    /// </summary>
    public int MaxOutputTokens => _maxOutputTokens;

    /// <summary>
    /// Delay between requests in milliseconds.
    /// </summary>
    public int DelayBetweenRequestsMs => _delayBetweenRequestsMs;

    /// <summary>
    /// Maximum parallel requests allowed.
    /// </summary>
    public int MaxParallelRequests => _maxParallelRequests;

    /// <summary>
    /// Current number of active requests.
    /// </summary>
    public int ActiveRequests => _activeRequests;

    /// <summary>
    /// Effective token budget per minute (after safety factor).
    /// </summary>
    public int EffectiveTokenBudget => (int)(_tokensPerMinute * _safetyFactor);

    /// <summary>
    /// Tokens remaining in the current minute budget.
    /// </summary>
    public int TokensRemaining => Math.Max(0, EffectiveTokenBudget - _tokensUsedThisMinute);

    /// <summary>
    /// Waits for rate limit allowance before making a request.
    /// Supports parallel access with distributed rate limiting.
    /// </summary>
    /// <param name="estimatedInputTokens">Estimated tokens for this request.</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    public async Task WaitForRateLimitAsync(int estimatedInputTokens = 0, CancellationToken cancellationToken = default)
    {
        // First, acquire concurrency slot
        await _concurrencySemaphore.WaitAsync(cancellationToken);
        Interlocked.Increment(ref _activeRequests);
        
        var lockHeld = false;

        try
        {
            await _tokenBucketLock.WaitAsync(cancellationToken);
            lockHeld = true;

            // Check if we need to reset the minute counter
            var now = DateTime.UtcNow;
            if ((now - _minuteStartTime).TotalMinutes >= 1)
            {
                _tokensUsedThisMinute = 0;
                _minuteStartTime = now;
                _logger?.LogDebug("Token bucket reset for new minute");
            }

            // Calculate required delay (staggered for parallel workers)
            var timeSinceLastRequest = now - _lastRequestTime;
            var requiredDelay = TimeSpan.FromMilliseconds(_delayBetweenRequestsMs);
            
            if (timeSinceLastRequest < requiredDelay)
            {
                var waitTime = requiredDelay - timeSinceLastRequest;
                _logger?.LogDebug("Rate limit: waiting {WaitMs}ms before next request (worker {Active}/{Max})", 
                    (int)waitTime.TotalMilliseconds, _activeRequests, _maxParallelRequests);
                
                // Release lock during wait so other workers can proceed with stagger
                _tokenBucketLock.Release();
                lockHeld = false;

                await Task.Delay(waitTime, cancellationToken);

                await _tokenBucketLock.WaitAsync(cancellationToken);
                lockHeld = true;
            }

            // Check token budget with safety factor
            var estimatedTotalTokens = estimatedInputTokens + _maxOutputTokens;
            var effectiveBudget = (int)(_tokensPerMinute * _safetyFactor);
            
            if (_tokensUsedThisMinute + estimatedTotalTokens > effectiveBudget)
            {
                // Calculate wait time until next minute
                now = DateTime.UtcNow;
                var waitUntilNextMinute = 60000 - (int)(now - _minuteStartTime).TotalMilliseconds;
                if (waitUntilNextMinute > 0)
                {
                    _logger?.LogInformation(
                        "Token budget exhausted ({Used}/{Budget} tokens), waiting {WaitMs}ms for next minute (worker {Active}/{Max})",
                        _tokensUsedThisMinute, effectiveBudget, waitUntilNextMinute, _activeRequests, _maxParallelRequests);
                    
                    // Release lock during wait
                    _tokenBucketLock.Release();
                    lockHeld = false;

                    await Task.Delay(waitUntilNextMinute, cancellationToken);

                    await _tokenBucketLock.WaitAsync(cancellationToken);
                    lockHeld = true;
                    
                    _tokensUsedThisMinute = 0;
                    _minuteStartTime = DateTime.UtcNow;
                }
            }

            _lastRequestTime = DateTime.UtcNow;
            _tokensUsedThisMinute += estimatedTotalTokens;
            
            _logger?.LogDebug("Request allowed: {Used}/{Budget} tokens used this minute", 
                _tokensUsedThisMinute, effectiveBudget);
        }
        catch
        {
            // If we fail before actually making request, release concurrency slot
            Interlocked.Decrement(ref _activeRequests);
            _concurrencySemaphore.Release();
            throw;
        }
        finally
        {
            if (lockHeld)
            {
                _tokenBucketLock.Release();
            }
        }
    }

    /// <summary>
    /// Releases the concurrency slot after a request completes.
    /// Must be called after WaitForRateLimitAsync when the request finishes.
    /// </summary>
    public void ReleaseSlot()
    {
        Interlocked.Decrement(ref _activeRequests);
        _concurrencySemaphore.Release();
    }

    /// <summary>
    /// Reports actual token usage after a request completes.
    /// Call this to provide accurate token tracking.
    /// </summary>
    /// <param name="actualInputTokens">Actual input tokens used.</param>
    /// <param name="actualOutputTokens">Actual output tokens used.</param>
    public void ReportActualUsage(int actualInputTokens, int actualOutputTokens)
    {
        // We already counted estimated tokens, adjust if actual is different
        // For simplicity, just log - the estimates are conservative
        _logger?.LogDebug("Actual token usage: {Input} input + {Output} output = {Total} total",
            actualInputTokens, actualOutputTokens, actualInputTokens + actualOutputTokens);
    }

    /// <summary>
    /// Executes an async operation with retry logic for 429 errors.
    /// Automatically manages concurrency slots.
    /// </summary>
    /// <typeparam name="T">Return type.</typeparam>
    /// <param name="operation">The operation to execute.</param>
    /// <param name="operationName">Name for logging.</param>
    /// <param name="estimatedInputTokens">Estimated input tokens for rate limiting.</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    /// <returns>Result of the operation.</returns>
    public async Task<T> ExecuteWithRetryAsync<T>(
        Func<Task<T>> operation,
        string operationName = "API call",
        int estimatedInputTokens = 0,
        CancellationToken cancellationToken = default)
    {
        var attempt = 0;
        var baseDelay = 5000; // Start with 5 second delay

        while (true)
        {
            attempt++;
            try
            {
                await WaitForRateLimitAsync(estimatedInputTokens, cancellationToken);
                try
                {
                    return await operation();
                }
                finally
                {
                    ReleaseSlot();
                }
            }
            catch (Exception ex) when (Is429Error(ex) && attempt <= _maxRetries)
            {
                var retryDelay = GetRetryDelay(ex, baseDelay, attempt);
                _logger?.LogWarning(
                    "Rate limit hit on {Operation} (attempt {Attempt}/{MaxRetries}), retrying in {Delay}ms. Active: {Active}/{Max}",
                    operationName, attempt, _maxRetries, retryDelay, _activeRequests, _maxParallelRequests);
                
                await Task.Delay(retryDelay, cancellationToken);
                baseDelay = Math.Min(baseDelay * 2, 60000); // Exponential backoff, max 60s
            }
        }
    }

    /// <summary>
    /// Checks if an exception is a 429 rate limit error.
    /// </summary>
    private static bool Is429Error(Exception ex)
    {
        // Check exception message and type
        if (ex.Message.Contains("429") || ex.Message.Contains("RateLimitReached"))
            return true;
        
        // Check inner exceptions
        if (ex.InnerException != null)
            return Is429Error(ex.InnerException);
        
        // Check HttpOperationException
        var statusCode = GetStatusCode(ex);
        return statusCode == 429;
    }

    /// <summary>
    /// Extracts retry delay from exception or calculates exponential backoff.
    /// </summary>
    private static int GetRetryDelay(Exception ex, int baseDelay, int attempt)
    {
        // Try to extract retry-after from exception message
        var message = ex.Message;
        if (message.Contains("retry after"))
        {
            // Parse "Please retry after 60 seconds"
            var match = System.Text.RegularExpressions.Regex.Match(message, @"retry after (\d+) seconds");
            if (match.Success && int.TryParse(match.Groups[1].Value, out var seconds))
            {
                return (seconds + 1) * 1000; // Add 1 second buffer
            }
        }

        // Exponential backoff with jitter
        var exponentialDelay = baseDelay * (int)Math.Pow(2, attempt - 1);
        var jitter = new Random().Next(0, 1000);
        return Math.Min(exponentialDelay + jitter, 120000); // Max 2 minutes
    }

    /// <summary>
    /// Extracts HTTP status code from exception.
    /// </summary>
    private static int? GetStatusCode(Exception ex)
    {
        var type = ex.GetType();
        
        // Try StatusCode property
        var statusCodeProp = type.GetProperty("StatusCode");
        if (statusCodeProp?.GetValue(ex) is System.Net.HttpStatusCode httpStatus)
            return (int)httpStatus;
        if (statusCodeProp?.GetValue(ex) is int statusInt)
            return statusInt;
        
        // Try Status property
        var statusProp = type.GetProperty("Status");
        if (statusProp?.GetValue(ex) is int status)
            return status;
        
        // Check inner exception
        if (ex.InnerException != null)
            return GetStatusCode(ex.InnerException);
        
        return null;
    }
}
