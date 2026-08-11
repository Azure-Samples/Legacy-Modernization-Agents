using System;
using System.Text.RegularExpressions;

namespace CobolToQuarkusMigration.Helpers;

/// <summary>
/// Helper for token estimation and rate-limit calculations.
/// 
/// Token Calculation for Azure OpenAI:
/// - ~4 characters per token for English text
/// - ~3 characters per token for code (more whitespace/symbols)
/// - COBOL has lots of whitespace, so ~3.5 chars/token is a good estimate
/// 
/// Rate Limit Calculation Example (300K TPM):
/// - TPM = Tokens Per Minute
/// - Each request uses: Input tokens + Output tokens
/// - If MaxOutputTokenCount = 10,000 and MaxInputTokens = 20,000
/// - Total per request = 30,000 tokens
/// - Safe requests per minute = 300,000 / 30,000 = 10 requests/min
/// - Minimum delay between requests = 60,000ms / 10 = 6,000ms (6 seconds)
/// </summary>
public static class TokenHelper
{
    /// <summary>
    /// Characters per token estimate for COBOL code
    /// </summary>
    public const double CharsPerToken = 3.5;
    
    /// <summary>
    /// Safety margin to stay under limits (use 50% of calculated limit for safety)
    /// </summary>
    public const double SafetyMargin = 0.5;

    /// <summary>
    /// Estimates the number of tokens in a string.
    /// </summary>
    /// <param name="text">The text to estimate tokens for.</param>
    /// <returns>Estimated token count.</returns>
    public static int EstimateTokens(string text)
    {
        if (string.IsNullOrEmpty(text))
            return 0;
        
        return (int)Math.Ceiling(text.Length / CharsPerToken);
    }

    /// <summary>
    /// Estimates the number of tokens in COBOL code (slightly different ratio).
    /// </summary>
    /// <param name="cobolCode">The COBOL code to estimate tokens for.</param>
    /// <returns>Estimated token count.</returns>
    public static int EstimateCobolTokens(string cobolCode)
    {
        if (string.IsNullOrEmpty(cobolCode))
            return 0;
        
        // COBOL has lots of whitespace and fixed columns, use 3.0 chars/token
        return (int)Math.Ceiling(cobolCode.Length / 3.0);
    }

    /// <summary>
    /// Compatibility shim retained for callers compiled against earlier versions.
    /// Truncation is disabled; the complete content is always returned.
    /// </summary>
    [Obsolete("Truncation is disabled. Use semantic chunking when input exceeds a model context window.")]
    public static (string Content, bool WasTruncated) TruncateToTokenLimit(
        string content,
        int maxTokens,
        bool preserveStart = true)
    {
        return (content, false);
    }

    /// <summary>
    /// Compatibility shim retained for callers compiled against earlier versions.
    /// Truncation is disabled; the complete COBOL source is always returned.
    /// </summary>
    [Obsolete("Truncation is disabled. Use semantic chunking when input exceeds a model context window.")]
    public static (string Content, bool WasTruncated, string Summary) TruncateCobolIntelligently(
        string cobolContent,
        int maxTokens)
    {
        var estimatedTokens = EstimateCobolTokens(cobolContent);
        return (cobolContent, false, $"Full content ({estimatedTokens} tokens)");
    }

    /// <summary>
    /// Calculates the recommended delay between API requests based on rate limits.
    /// </summary>
    /// <param name="tokensPerMinuteLimit">Your TPM limit (e.g., 300000).</param>
    /// <param name="estimatedInputTokens">Estimated input tokens per request.</param>
    /// <param name="maxOutputTokens">Max output tokens per request.</param>
    /// <param name="minDelayMs">Minimum delay floor in milliseconds (default 2000). Use lower values for fast profiles.</param>
    /// <returns>Recommended delay in milliseconds between requests.</returns>
    public static int CalculateRequestDelay(
        int tokensPerMinuteLimit, 
        int estimatedInputTokens, 
        int maxOutputTokens,
        int minDelayMs = 2000)
    {
        // Total tokens per request
        var tokensPerRequest = estimatedInputTokens + maxOutputTokens;
        
        // Safe requests per minute (with safety margin)
        var safeRequestsPerMinute = (tokensPerMinuteLimit * SafetyMargin) / tokensPerRequest;
        
        // Delay in milliseconds
        if (safeRequestsPerMinute <= 0)
            return 60000; // 1 minute if calculation fails
        
        var delayMs = (int)(60000 / safeRequestsPerMinute);
        
        // Configurable minimum, maximum 120 seconds
        return Math.Clamp(delayMs, Math.Max(500, minDelayMs), 120000);
    }

    /// <summary>
    /// Gets rate limit configuration summary for logging.
    /// </summary>
    public static string GetRateLimitSummary(
        int tokensPerMinuteLimit,
        int maxInputTokens,
        int maxOutputTokens)
    {
        var tokensPerRequest = maxInputTokens + maxOutputTokens;
        var safeRequestsPerMinute = (int)((tokensPerMinuteLimit * SafetyMargin) / tokensPerRequest);
        var delayMs = CalculateRequestDelay(tokensPerMinuteLimit, maxInputTokens, maxOutputTokens);
        
        return $@"
Rate Limit Configuration:
  TPM Limit: {tokensPerMinuteLimit:N0} tokens/minute
  Max Input Tokens: {maxInputTokens:N0}
  Max Output Tokens: {maxOutputTokens:N0}
  Tokens per Request: {tokensPerRequest:N0}
  Safe Requests/Min: {safeRequestsPerMinute}
  Delay Between Requests: {delayMs}ms ({delayMs / 1000.0:F1}s)
";
    }
}
