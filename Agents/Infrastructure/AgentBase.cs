using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using System.Text;

using AIChatMessage = Microsoft.Extensions.AI.ChatMessage;

namespace CobolToQuarkusMigration.Agents.Infrastructure;

/// <summary>
/// Base class for all agents supporting both Responses API (for codex models) and Chat Completions API (for chat models).
/// Provides common functionality for chat completions, error handling, and fallback logic.
/// </summary>
public abstract class AgentBase
{
    protected readonly IChatClient? ChatClient;
    protected readonly ResponsesApiClient? ResponsesClient;
    protected readonly ILogger Logger;
    protected readonly string ModelId;
    protected readonly EnhancedLogger? EnhancedLogger;
    protected readonly ChatLogger? ChatLogger;
    protected readonly RateLimiter? RateLimiter;
    protected readonly AppSettings? Settings;
    protected readonly bool UseResponsesApi;

    /// <summary>
    /// Gets the name of the agent for logging purposes.
    /// </summary>
    protected abstract string AgentName { get; }

    /// <summary>
    /// Initializes a new instance using Chat Completions API (for chat models like gpt-5.1-chat).
    /// </summary>
    protected AgentBase(
        IChatClient chatClient,
        ILogger logger,
        string modelId,
        EnhancedLogger? enhancedLogger = null,
        ChatLogger? chatLogger = null,
        RateLimiter? rateLimiter = null,
        AppSettings? settings = null)
    {
        ChatClient = chatClient ?? throw new ArgumentNullException(nameof(chatClient));
        Logger = logger ?? throw new ArgumentNullException(nameof(logger));
        ModelId = modelId ?? throw new ArgumentNullException(nameof(modelId));
        EnhancedLogger = enhancedLogger;
        ChatLogger = chatLogger;
        RateLimiter = rateLimiter;
        Settings = settings;
        UseResponsesApi = false;
    }

    /// <summary>
    /// Initializes a new instance using Responses API (for codex models like gpt-5.1-codex-mini).
    /// </summary>
    protected AgentBase(
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
        UseResponsesApi = true;
    }

    /// <summary>
    /// Executes a chat completion with the specified prompts.
    /// Automatically selects the appropriate API based on how the agent was initialized.
    /// </summary>
    protected async Task<string> ExecuteChatCompletionAsync(
        string systemPrompt,
        string userPrompt,
        string contextIdentifier)
    {
        // Apply rate limiting if configured
        if (RateLimiter != null)
        {
            await RateLimiter.WaitForRateLimitAsync(TokenHelper.EstimateTokens(systemPrompt + userPrompt));
        }

        Logger.LogDebug("[{Agent}] Executing {ApiType} for {Context}", 
            AgentName, UseResponsesApi ? "Responses API" : "Chat Completions API", contextIdentifier);

        // Log the request
        ChatLogger?.LogUserMessage(AgentName, contextIdentifier, userPrompt, systemPrompt);
        EnhancedLogger?.LogBehindTheScenes("API_CALL", UseResponsesApi ? "ResponsesAPI" : "ChatCompletion", 
            $"Calling Azure OpenAI for {contextIdentifier}", AgentName);

        try
        {
            string responseText;
            
            if (UseResponsesApi && ResponsesClient != null)
            {
                // Use Responses API for codex models with auto-optimized token settings
                responseText = await ResponsesClient.GetResponseAutoAsync(systemPrompt, userPrompt);
            }
            else if (ChatClient != null)
            {
                // Use Chat Completions API for chat models
                var messages = new List<AIChatMessage>
                {
                    new AIChatMessage(ChatRole.System, systemPrompt),
                    new AIChatMessage(ChatRole.User, userPrompt)
                };

                var options = new ChatOptions
                {
                    ModelId = ModelId,
                    // NOTE: gpt-5.1-chat does NOT support custom temperature, only default (1.0)
                    MaxOutputTokens = 16384
                };

                var response = await ChatClient.GetResponseAsync(messages, options);
                responseText = ExtractResponseText(response);
            }
            else
            {
                throw new InvalidOperationException("No API client configured");
            }

            // Log the response
            ChatLogger?.LogAIResponse(AgentName, contextIdentifier, responseText);
            EnhancedLogger?.LogBehindTheScenes("API_RESPONSE", UseResponsesApi ? "ResponsesAPI" : "ChatCompletion", 
                $"Received {responseText.Length} chars from Azure OpenAI", AgentName);

            // Release rate limiter slot after completion
            RateLimiter?.ReleaseSlot();

            return responseText;
        }
        catch (Exception ex)
        {
            RateLimiter?.ReleaseSlot();
            Logger.LogError(ex, "[{Agent}] Error executing {ApiType} for {Context}", 
                AgentName, UseResponsesApi ? "Responses API" : "Chat Completions", contextIdentifier);
            throw;
        }
    }

    /// <summary>
    /// Executes a chat completion with fallback handling for common errors.
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
                var response = await ExecuteChatCompletionAsync(systemPrompt, userPrompt, contextIdentifier);
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
    /// Extracts the text content from a chat response.
    /// </summary>
    protected string ExtractResponseText(ChatResponse response)
    {
        if (response == null)
            return string.Empty;

        // The ChatResponse has a Messages property for multi-turn, 
        // or we can look at the last message
        var sb = new StringBuilder();
        
        // Get assistant messages
        foreach (var message in response.Messages)
        {
            if (message.Role == ChatRole.Assistant)
            {
                if (message.Text != null)
                {
                    sb.Append(message.Text);
                }
                else if (message.Contents != null)
                {
                    foreach (var content in message.Contents)
                    {
                        if (content is TextContent textContent)
                        {
                            sb.Append(textContent.Text);
                        }
                    }
                }
            }
        }

        return sb.ToString();
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
