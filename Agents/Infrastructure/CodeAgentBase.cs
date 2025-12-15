using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using System.Text;

namespace CobolToQuarkusMigration.Agents.Infrastructure;

/// <summary>
/// Base class for code generation agents that use the Azure OpenAI Responses API.
/// This is used for codex/reasoning models that don't support the Chat Completions API.
/// </summary>
public abstract class CodeAgentBase
{
    protected readonly ResponsesApiClient ResponsesClient;
    protected readonly ILogger Logger;
    protected readonly string ModelId;
    protected readonly EnhancedLogger? EnhancedLogger;
    protected readonly ChatLogger? ChatLogger;
    protected readonly RateLimiter? RateLimiter;
    protected readonly AppSettings? Settings;

    /// <summary>
    /// Gets the name of the agent for logging purposes.
    /// </summary>
    protected abstract string AgentName { get; }

    /// <summary>
    /// Initializes a new instance of the <see cref="CodeAgentBase"/> class.
    /// </summary>
    /// <param name="responsesClient">The Responses API client for AI interactions.</param>
    /// <param name="logger">The logger.</param>
    /// <param name="modelId">The model ID to use.</param>
    /// <param name="enhancedLogger">Enhanced logger for API call tracking.</param>
    /// <param name="chatLogger">Chat logger for conversation tracking.</param>
    /// <param name="rateLimiter">Optional rate limiter for API throttling.</param>
    /// <param name="settings">Optional app settings for configuration.</param>
    protected CodeAgentBase(
        ResponsesApiClient responsesClient,
        ILogger logger,
        string modelId,
        EnhancedLogger? enhancedLogger = null,
        ChatLogger? chatLogger = null,
        RateLimiter? rateLimiter = null,
        AppSettings? settings = null)
    {
        ResponsesClient = responsesClient ?? throw new ArgumentNullException(nameof(responsesClient));
        Logger = logger ?? throw new ArgumentNullException(nameof(logger));
        ModelId = modelId ?? throw new ArgumentNullException(nameof(modelId));
        EnhancedLogger = enhancedLogger;
        ChatLogger = chatLogger;
        RateLimiter = rateLimiter;
        Settings = settings;
    }

    /// <summary>
    /// Executes a Responses API call with the specified prompts.
    /// </summary>
    /// <param name="systemPrompt">The system prompt.</param>
    /// <param name="userPrompt">The user prompt.</param>
    /// <param name="contextIdentifier">An identifier for logging context (e.g., filename).</param>
    /// <returns>The response text from the AI model.</returns>
    protected async Task<string> ExecuteResponsesApiAsync(
        string systemPrompt,
        string userPrompt,
        string contextIdentifier)
    {
        // Apply rate limiting if configured
        if (RateLimiter != null)
        {
            await RateLimiter.WaitForRateLimitAsync(TokenHelper.EstimateTokens(systemPrompt + userPrompt));
        }

        Logger.LogDebug("[{Agent}] Executing Responses API call for {Context}", AgentName, contextIdentifier);

        // Log the request
        ChatLogger?.LogUserMessage(AgentName, contextIdentifier, userPrompt, systemPrompt);
        EnhancedLogger?.LogBehindTheScenes("API_CALL", "ResponsesAPI",
            $"Calling Azure OpenAI Responses API for {contextIdentifier}", AgentName);

        try
        {
            // Use auto-optimized token settings based on input size
            var response = await ResponsesClient.GetResponseAutoAsync(systemPrompt, userPrompt);

            // Log the response
            ChatLogger?.LogAIResponse(AgentName, contextIdentifier, response);
            EnhancedLogger?.LogBehindTheScenes("API_RESPONSE", "ResponsesAPI",
                $"Received {response.Length} chars from Azure OpenAI Responses API", AgentName);

            // Release rate limiter slot after completion
            RateLimiter?.ReleaseSlot();

            return response;
        }
        catch (Exception ex)
        {
            RateLimiter?.ReleaseSlot();
            Logger.LogError(ex, "[{Agent}] Error executing Responses API call for {Context}", AgentName, contextIdentifier);
            throw;
        }
    }

    /// <summary>
    /// Executes a Responses API call with fallback handling for common errors.
    /// Returns a tuple of (response, usedFallback, fallbackReason).
    /// </summary>
    protected async Task<(string Response, bool UsedFallback, string? FallbackReason)> ExecuteWithFallbackAsync(
        string systemPrompt,
        string userPrompt,
        string contextIdentifier,
        int maxRetries = 3)
    {
        int attempt = 0;
        Exception? lastException = null;

        while (attempt < maxRetries)
        {
            attempt++;

            try
            {
                var response = await ExecuteResponsesApiAsync(systemPrompt, userPrompt, contextIdentifier);
                return (response, false, null);
            }
            catch (Exception ex) when (IsTransientError(ex) && attempt < maxRetries)
            {
                lastException = ex;
                var delay = TimeSpan.FromSeconds(Math.Pow(2, attempt)); // Exponential backoff

                Logger.LogWarning(
                    "[{Agent}] Transient error on attempt {Attempt}/{MaxRetries} for {Context}. Retrying in {Delay}s. Error: {Error}",
                    AgentName, attempt, maxRetries, contextIdentifier, delay.TotalSeconds, ex.Message);

                EnhancedLogger?.LogBehindTheScenes("RETRY", "TRANSIENT_ERROR",
                    $"Retry {attempt}/{maxRetries} for {contextIdentifier}: {ex.Message}");

                await Task.Delay(delay);
            }
            catch (Exception ex) when (IsContentFilterError(ex))
            {
                Logger.LogWarning(
                    "[{Agent}] Content filter triggered for {Context}: {Error}",
                    AgentName, contextIdentifier, ex.Message);

                EnhancedLogger?.LogBehindTheScenes("CONTENT_FILTER", "BLOCKED",
                    $"Content filter blocked request for {contextIdentifier}: {ex.Message}");

                return (string.Empty, true, $"Content filter: {ex.Message}");
            }
            catch (Exception ex) when (IsRateLimitError(ex) && attempt < maxRetries)
            {
                lastException = ex;
                var delay = TimeSpan.FromSeconds(Math.Pow(2, attempt + 2)); // Longer delay for rate limits

                Logger.LogWarning(
                    "[{Agent}] Rate limited on attempt {Attempt}/{MaxRetries} for {Context}. Retrying in {Delay}s",
                    AgentName, attempt, maxRetries, contextIdentifier, delay.TotalSeconds);

                EnhancedLogger?.LogBehindTheScenes("RATE_LIMIT", "THROTTLED",
                    $"Rate limited, waiting {delay.TotalSeconds}s before retry");

                await Task.Delay(delay);
            }
            catch (Exception ex)
            {
                // Non-retryable error
                Logger.LogError(ex,
                    "[{Agent}] Non-retryable error for {Context}: {Error}",
                    AgentName, contextIdentifier, ex.Message);

                return (string.Empty, true, ex.Message);
            }
        }

        // All retries exhausted
        var finalReason = $"Max retries ({maxRetries}) exhausted. Last error: {lastException?.Message}";
        Logger.LogError("[{Agent}] {Reason}", AgentName, finalReason);

        return (string.Empty, true, finalReason);
    }

    /// <summary>
    /// Determines if an exception represents a transient error that can be retried.
    /// </summary>
    protected virtual bool IsTransientError(Exception ex)
    {
        var message = ex.Message.ToLowerInvariant();
        return message.Contains("timeout") ||
               message.Contains("temporarily unavailable") ||
               message.Contains("service unavailable") ||
               message.Contains("502") ||
               message.Contains("503") ||
               message.Contains("504") ||
               message.Contains("connection") ||
               ex is HttpRequestException ||
               ex is TaskCanceledException;
    }

    /// <summary>
    /// Determines if an exception represents a content filter error.
    /// </summary>
    protected virtual bool IsContentFilterError(Exception ex)
    {
        var message = ex.Message.ToLowerInvariant();
        return message.Contains("content_filter") ||
               message.Contains("content filter") ||
               message.Contains("filtered") ||
               message.Contains("content management policy");
    }

    /// <summary>
    /// Determines if an exception represents a rate limit error.
    /// </summary>
    protected virtual bool IsRateLimitError(Exception ex)
    {
        var message = ex.Message.ToLowerInvariant();
        return message.Contains("rate limit") ||
               message.Contains("429") ||
               message.Contains("too many requests") ||
               message.Contains("quota exceeded");
    }

    /// <summary>
    /// Builds a detailed error message for logging.
    /// </summary>
    protected string BuildDetailedErrorMessage(Exception ex, string context)
    {
        var sb = new StringBuilder();
        sb.AppendLine($"Error in {AgentName} for {context}:");
        sb.AppendLine($"  Message: {ex.Message}");
        sb.AppendLine($"  Type: {ex.GetType().FullName}");

        if (ex.InnerException != null)
        {
            sb.AppendLine($"  Inner: {ex.InnerException.Message}");
        }

        return sb.ToString();
    }
}
