using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using System.Text;
using System.Text.RegularExpressions;

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

    protected abstract string AgentName { get; }

    protected string ProviderName =>
        ChatClient is CopilotChatClient ? "GitHub Copilot" : "Azure OpenAI";

    /// <summary>Detected capabilities of the configured model (family, reasoning strategy, etc.).</summary>
    protected ModelCapabilities Capabilities { get; }

    /// <summary>The model profile controlling reasoning effort tiers and token limits.</summary>
    protected ModelProfileSettings Profile { get; }

    // Pre-compiled structural regexes for complexity scoring
    private static readonly Regex PicRegex = new(@"\bPIC\b", RegexOptions.Compiled | RegexOptions.IgnoreCase);
    private static readonly Regex LevelRegex = new(@"^\s*\d{2}\s+", RegexOptions.Compiled | RegexOptions.Multiline);
    private static readonly Regex CopyNearStorageRegex = new(
        @"(WORKING-STORAGE|LINKAGE)\s+SECTION[\s\S]{0,500}COPY\b",
        RegexOptions.Compiled | RegexOptions.IgnoreCase);
    private static readonly Regex ExecSqlDliRegex = new(
        @"EXEC\s+(SQL|DLI)\b", RegexOptions.Compiled | RegexOptions.IgnoreCase);
    private static readonly Regex NoiseLineRegex = new(
        @"^\s*$|^.{6}\*|^.{0,5}\*>|^.{7}\s*(CBL|PROCESS|EJECT|SKIP1|SKIP2|SKIP3)\b",
        RegexOptions.Compiled | RegexOptions.IgnoreCase);

    // Pre-compiled config-driven indicator regexes (built at construction)
    private readonly List<(Regex regex, int weight)> _compiledIndicators;

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
        Capabilities = ModelCapabilities.Detect(modelId);
        Profile = settings?.ModelProfile ?? new ModelProfileSettings();

        // ── Clamp profile token limits to model's actual capabilities ──
        // This prevents HTTP 400 "max_tokens too large" errors for models
        // with lower output limits (e.g. gpt-4o = 16384, grok = 32768).
        if (Capabilities.DefaultMaxOutputTokens > 0)
        {
            Profile.MaxOutputTokens = Math.Min(Profile.MaxOutputTokens, Capabilities.DefaultMaxOutputTokens);
            Profile.MinOutputTokens = Math.Min(Profile.MinOutputTokens, Capabilities.DefaultMaxOutputTokens);
        }

        _compiledIndicators = CompileIndicators(Profile);

        logger.LogInformation(
            "[{Agent}] IChatClient path: model={Model}, family={Family}, reasoning={Reasoning}",
            GetType().Name, modelId, Capabilities.Family, Capabilities.Reasoning);
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
        Capabilities = ModelCapabilities.Detect(modelId);
        Profile = responsesClient.Profile;
        _compiledIndicators = CompileIndicators(Profile);
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
            $"Calling {ProviderName} for {contextIdentifier}", AgentName);

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
                // Use Chat Completions API with content-aware token/reasoning optimization
                var (maxTokens, reasoningEffort) = CalculateTokenSettings(systemPrompt, userPrompt);

                var messages = new List<AIChatMessage>
                {
                    new AIChatMessage(ChatRole.System, systemPrompt),
                    new AIChatMessage(ChatRole.User, userPrompt)
                };

                var options = new ChatOptions
                {
                    ModelId = ModelId,
                    MaxOutputTokens = maxTokens
                };

                // Apply model-specific reasoning options
                ApplyModelSpecificOptions(options, reasoningEffort, maxTokens);

                var response = await ChatClient.GetResponseAsync(messages, options);
                responseText = ExtractResponseText(response);

                // ── Truncation detection ──
                DetectTruncation(response, responseText, maxTokens, reasoningEffort, contextIdentifier);
            }
            else
            {
                throw new InvalidOperationException("No API client configured");
            }

            // Log the response
            ChatLogger?.LogAIResponse(AgentName, contextIdentifier, responseText);
            EnhancedLogger?.LogBehindTheScenes("API_RESPONSE", UseResponsesApi ? "ResponsesAPI" : "ChatCompletion", 
                $"Received {responseText.Length} chars from {ProviderName}", AgentName);

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
    /// Per-LLM-call hang timeout. If the underlying client doesn't return within
    /// this window we treat the call as failed and retry. Tunable via the
    /// LLM_CALL_TIMEOUT_SECONDS environment variable (default 240 s).
    /// </summary>
    private static TimeSpan LlmCallTimeout
    {
        get
        {
            var raw = Environment.GetEnvironmentVariable("LLM_CALL_TIMEOUT_SECONDS");
            if (int.TryParse(raw, out var s) && s > 10 && s < 3600)
                return TimeSpan.FromSeconds(s);
            return TimeSpan.FromSeconds(480);
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
                // Hang-guard: race the actual call against a timeout. Some providers
                // (GitHub Copilot in particular) sometimes accept a request and
                // never reply — no exception is thrown, just silence. The timeout
                // converts that into a TaskCanceledException so the standard
                // retry path picks it up.
                using var cts = new CancellationTokenSource(LlmCallTimeout);
                var callTask = ExecuteChatCompletionAsync(systemPrompt, userPrompt, contextIdentifier);
                var timeoutTask = Task.Delay(LlmCallTimeout, cts.Token);
                var finished = await Task.WhenAny(callTask, timeoutTask);

                if (finished == timeoutTask)
                {
                    Logger.LogWarning(
                        "[{Agent}] LLM call for {Context} exceeded {Seconds}s hang-timeout (attempt {Attempt}/{MaxRetries}). Treating as transient and retrying.",
                        AgentName, contextIdentifier, LlmCallTimeout.TotalSeconds, attempt, maxRetries);
                    EnhancedLogger?.LogBehindTheScenes("HANG_TIMEOUT", "TIMEOUT",
                        $"LLM call exceeded {LlmCallTimeout.TotalSeconds}s for {contextIdentifier} — retrying");
                    throw new TaskCanceledException(
                        $"LLM call exceeded {LlmCallTimeout.TotalSeconds}s hang-timeout for {contextIdentifier}");
                }

                cts.Cancel();
                var response = await callTask;
                return (response, false, null);
            }
            // ── Reasoning exhaustion catch (before transient error catch) ──
            catch (ReasoningExhaustionException rex) when (UseResponsesApi && ResponsesClient != null)
            {
                var profile = ResponsesClient.Profile;
                var maxExhaustionRetries = profile.ReasoningExhaustionMaxRetries;

                Logger.LogWarning(
                    "[{Agent}] Reasoning exhaustion for {Context}: {Message}",
                    AgentName, contextIdentifier, rex.Message);

                EnhancedLogger?.LogBehindTheScenes("REASONING_EXHAUSTION", "DETECTED",
                    $"max_output_tokens={rex.MaxOutputTokens}, reasoning={rex.ReasoningTokens}, " +
                    $"output={rex.ActualOutputTokens}, effort='{rex.ReasoningEffort}'", AgentName);

                // Escalation loop: increase tokens and promote reasoning effort
                var currentMaxTokens = rex.MaxOutputTokens;
                var currentEffort = rex.ReasoningEffort;

                for (int exhaustionRetry = 0; exhaustionRetry < maxExhaustionRetries; exhaustionRetry++)
                {
                    // Double the output tokens
                    currentMaxTokens = (int)(currentMaxTokens * profile.ReasoningExhaustionRetryMultiplier);
                    // Cap at profile maximum
                    currentMaxTokens = Math.Min(currentMaxTokens, profile.MaxOutputTokens);

                    // Promote reasoning effort: low → medium → high
                    if (currentEffort == profile.LowReasoningEffort && currentEffort != profile.MediumReasoningEffort)
                        currentEffort = profile.MediumReasoningEffort;
                    else if (currentEffort == profile.MediumReasoningEffort && currentEffort != profile.HighReasoningEffort)
                        currentEffort = profile.HighReasoningEffort;

                    // Thrash guard — if already at max tokens AND max effort, don't burn another API call
                    if (currentMaxTokens >= profile.MaxOutputTokens && currentEffort == profile.HighReasoningEffort)
                    {
                        Logger.LogError(
                            "[{Agent}] Thrash guard: already at max tokens ({Tokens}) and max effort ('{Effort}') " +
                            "for {Context}. Failing fast — further retries are hopeless.",
                            AgentName, currentMaxTokens, currentEffort, contextIdentifier);

                        EnhancedLogger?.LogBehindTheScenes("REASONING_EXHAUSTION", "THRASH_GUARD",
                            $"Stopped retrying: tokens={currentMaxTokens} (max), effort='{currentEffort}' (max)", AgentName);

                        break;
                    }

                    Logger.LogInformation(
                        "[{Agent}] Reasoning exhaustion retry {Retry}/{MaxRetries} for {Context}: " +
                        "max_output_tokens={Tokens}, effort='{Effort}'",
                        AgentName, exhaustionRetry + 1, maxExhaustionRetries,
                        contextIdentifier, currentMaxTokens, currentEffort);

                    try
                    {
                        var retryResponse = await ResponsesClient.GetResponseAsync(
                            systemPrompt, userPrompt, currentMaxTokens, currentEffort);

                        EnhancedLogger?.LogBehindTheScenes("REASONING_EXHAUSTION_RECOVERED", "SUCCESS",
                            $"Recovered on retry {exhaustionRetry + 1} with tokens={currentMaxTokens}, effort='{currentEffort}'",
                            AgentName);

                        ChatLogger?.LogAIResponse(AgentName, contextIdentifier, retryResponse);
                        return (retryResponse, false, null);
                    }
                    catch (ReasoningExhaustionException)
                    {
                        // Still exhausted, continue escalation loop
                        Logger.LogWarning(
                            "[{Agent}] Still exhausted after retry {Retry} with tokens={Tokens}",
                            AgentName, exhaustionRetry + 1, currentMaxTokens);
                    }
                }

                // All exhaustion retries failed — try adaptive re-chunking
                Logger.LogWarning(
                    "[{Agent}] All {MaxRetries} reasoning exhaustion retries failed for {Context}. " +
                    "Attempting adaptive re-chunking…",
                    AgentName, maxExhaustionRetries, contextIdentifier);

                EnhancedLogger?.LogBehindTheScenes("ADAPTIVE_RECHUNK", "TRIGGERED",
                    $"Exhaustion retries exhausted for {contextIdentifier}. " +
                    "Splitting input at semantic boundary and re-processing.", AgentName);

                var rechunkResult = await TryAdaptiveRechunkAsync(
                    systemPrompt, userPrompt, contextIdentifier, currentMaxTokens, currentEffort);

                if (rechunkResult.HasValue)
                    return rechunkResult.Value;

                // Adaptive re-chunking also failed — give up
                Logger.LogError(
                    "[{Agent}] Adaptive re-chunking failed for {Context}. No further recovery possible.",
                    AgentName, contextIdentifier);

                return (string.Empty, true,
                    $"Reasoning exhaustion: all {maxExhaustionRetries} escalation retries AND adaptive re-chunking failed");
            }
            // ── END Reasoning exhaustion ──
            // ── Output truncation catch (IChatClient path) ──
            catch (OutputTruncationException otex)
            {
                Logger.LogWarning(
                    "[{Agent}] Output truncated for {Context}: {Message}",
                    AgentName, contextIdentifier, otex.Message);

                EnhancedLogger?.LogBehindTheScenes("OUTPUT_TRUNCATION", "DETECTED",
                    $"max_output_tokens={otex.MaxOutputTokens}, output={otex.OutputCharCount} chars, " +
                    $"effort='{otex.ReasoningEffort}', signal='{otex.TruncationSignal}'", AgentName);

                // Escalation loop: increase tokens and promote reasoning effort
                var currentMaxTokens = otex.MaxOutputTokens;
                var currentEffort = otex.ReasoningEffort;
                var maxTruncRetries = Profile.ReasoningExhaustionMaxRetries;

                for (int truncRetry = 0; truncRetry < maxTruncRetries; truncRetry++)
                {
                    currentMaxTokens = (int)(currentMaxTokens * Profile.ReasoningExhaustionRetryMultiplier);
                    currentMaxTokens = Math.Min(currentMaxTokens, Profile.MaxOutputTokens);

                    // Promote reasoning effort
                    if (currentEffort == Profile.LowReasoningEffort && currentEffort != Profile.MediumReasoningEffort)
                        currentEffort = Profile.MediumReasoningEffort;
                    else if (currentEffort == Profile.MediumReasoningEffort && currentEffort != Profile.HighReasoningEffort)
                        currentEffort = Profile.HighReasoningEffort;

                    // Thrash guard
                    if (currentMaxTokens >= Profile.MaxOutputTokens && currentEffort == Profile.HighReasoningEffort)
                    {
                        Logger.LogError(
                            "[{Agent}] Thrash guard: already at max tokens ({Tokens}) and max effort ('{Effort}') " +
                            "for {Context}. Moving to adaptive re-chunking.",
                            AgentName, currentMaxTokens, currentEffort, contextIdentifier);
                        break;
                    }

                    Logger.LogInformation(
                        "[{Agent}] Truncation retry {Retry}/{MaxRetries} for {Context}: " +
                        "max_output_tokens={Tokens}, effort='{Effort}'",
                        AgentName, truncRetry + 1, maxTruncRetries,
                        contextIdentifier, currentMaxTokens, currentEffort);

                    try
                    {
                        var messages = new List<AIChatMessage>
                        {
                            new AIChatMessage(ChatRole.System, systemPrompt),
                            new AIChatMessage(ChatRole.User, userPrompt)
                        };
                        var retryOptions = new ChatOptions
                        {
                            ModelId = ModelId,
                            MaxOutputTokens = currentMaxTokens
                        };
                        ApplyModelSpecificOptions(retryOptions, currentEffort, currentMaxTokens);

                        var retryResponse = await ChatClient!.GetResponseAsync(messages, retryOptions);
                        var retryText = ExtractResponseText(retryResponse);

                        // Re-check for truncation on the retry
                        DetectTruncation(retryResponse, retryText, currentMaxTokens, currentEffort, contextIdentifier);

                        EnhancedLogger?.LogBehindTheScenes("OUTPUT_TRUNCATION_RECOVERED", "SUCCESS",
                            $"Recovered on retry {truncRetry + 1} with tokens={currentMaxTokens}, effort='{currentEffort}'",
                            AgentName);

                        ChatLogger?.LogAIResponse(AgentName, contextIdentifier, retryText);
                        return (retryText, false, null);
                    }
                    catch (OutputTruncationException)
                    {
                        Logger.LogWarning(
                            "[{Agent}] Still truncated after retry {Retry} with tokens={Tokens}",
                            AgentName, truncRetry + 1, currentMaxTokens);
                    }
                }

                // All truncation retries failed — try adaptive re-chunking
                Logger.LogWarning(
                    "[{Agent}] All {MaxRetries} truncation retries failed for {Context}. " +
                    "Attempting adaptive re-chunking…",
                    AgentName, maxTruncRetries, contextIdentifier);

                EnhancedLogger?.LogBehindTheScenes("ADAPTIVE_RECHUNK", "TRIGGERED",
                    $"Truncation retries exhausted for {contextIdentifier}. " +
                    "Splitting input at semantic boundary and re-processing.", AgentName);

                var rechunkResult = await TryAdaptiveRechunkAsync(
                    systemPrompt, userPrompt, contextIdentifier, currentMaxTokens, currentEffort);

                if (rechunkResult.HasValue)
                    return rechunkResult.Value;

                Logger.LogError(
                    "[{Agent}] Adaptive re-chunking failed for {Context}. No further recovery possible.",
                    AgentName, contextIdentifier);

                return (string.Empty, true,
                    $"Output truncation: all {maxTruncRetries} escalation retries AND adaptive re-chunking failed");
            }
            // ── END Output truncation ──
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
                // If the exception is transient but we've used the last attempt,
                // log "max retries exhausted" rather than "non-retryable error" so
                // diagnosis stays honest. Without this branch the hang-timeout
                // path on attempt N gets misclassified as "non-retryable" because
                // the `when (IsTransientError && attempt < maxRetries)` clause
                // stops matching after the budget is spent.
                if (IsTransientError(ex) || IsRateLimitError(ex))
                {
                    var reason = $"Max retries ({maxRetries}) exhausted after transient errors. Last error: {ex.Message}";
                    Logger.LogError("[{Agent}] {Reason} for {Context}", AgentName, reason, contextIdentifier);
                    EnhancedLogger?.LogBehindTheScenes("RETRIES_EXHAUSTED", "GIVING_UP",
                        $"All {maxRetries} attempts for {contextIdentifier} failed transiently. Last: {ex.Message}");
                    return (string.Empty, true, reason);
                }

                // Non-retryable error
                Logger.LogError(ex,
                    "[{Agent}] Non-retryable error for {Context}: {Error}",
                    AgentName, contextIdentifier, ex.Message);

                return (string.Empty, true, ex.Message);
            }
        }

        // All retries exhausted (reached when the loop exited normally — e.g.
        // every attempt threw a retryable error that the `when` clause matched).
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

    // =========================================================================
    // Adaptive Re-Chunking — splits input on OUTPUT exhaustion
    // =========================================================================

    /// <summary>
    /// Number of overlap lines to include between re-chunked halves for context continuity.
    /// </summary>
    private const int RechunkOverlapLines = 50;

    /// <summary>
    /// Regex that matches COBOL semantic boundaries: DIVISIONs, SECTIONs, and paragraph headers.
    /// Paragraph headers are identified as lines that start in Area A (cols 8-11) and end with a period.
    /// </summary>
    private static readonly Regex CobolBoundaryRegex = new(
        @"^\s{0,3}\S.*\b(DIVISION|SECTION)\b|^\s{7}\S[\w-]+\.\s*$",
        RegexOptions.IgnoreCase | RegexOptions.Compiled);

    /// <summary>
    /// Attempts adaptive re-chunking when reasoning exhaustion retries are exhausted.
    /// Splits the COBOL source inside the user prompt into two halves at a semantic boundary,
    /// processes each half independently, and merges the results.
    /// </summary>
    /// <returns>A merged result tuple, or null if re-chunking is not applicable or fails.</returns>
    private async Task<(string Response, bool UsedFallback, string? FallbackReason)?> TryAdaptiveRechunkAsync(
        string systemPrompt,
        string userPrompt,
        string contextIdentifier,
        int maxOutputTokens,
        string reasoningEffort)
    {
        // Step 1 — Extract COBOL source from user prompt
        var (cobolContent, beforeCobol, afterCobol) = ExtractCobolFromPrompt(userPrompt);

        if (string.IsNullOrWhiteSpace(cobolContent))
        {
            Logger.LogWarning("[{Agent}] Adaptive re-chunking skipped: no COBOL content found in user prompt.",
                AgentName);
            return null;
        }

        var cobolLines = cobolContent.Split('\n');
        if (cobolLines.Length < 20)
        {
            Logger.LogWarning(
                "[{Agent}] Adaptive re-chunking skipped: COBOL content too small ({Lines} lines) to split.",
                AgentName, cobolLines.Length);
            return null;
        }

        // Step 2 — Find best semantic split point near the midpoint
        var splitLine = FindSemanticSplitPoint(cobolLines);

        Logger.LogInformation(
            "[{Agent}] Adaptive re-chunking: splitting {TotalLines} lines at line {SplitLine} for {Context}",
            AgentName, cobolLines.Length, splitLine, contextIdentifier);

        EnhancedLogger?.LogBehindTheScenes("ADAPTIVE_RECHUNK", "SPLITTING",
            $"Total lines={cobolLines.Length}, split at line {splitLine} " +
            $"(first half={splitLine}, second half={cobolLines.Length - splitLine})",
            AgentName);

        // Step 3 — Build two halves with overlap
        var overlapStart = Math.Max(0, splitLine - RechunkOverlapLines);
        var firstHalf = string.Join("\n", cobolLines.Take(splitLine));
        var secondHalf = string.Join("\n", cobolLines.Skip(overlapStart));

        // Step 4 — Build prompts for each half
        var firstUserPrompt = RebuildUserPrompt(beforeCobol, firstHalf, afterCobol,
            chunkLabel: $"PART 1 of 2 (lines 1-{splitLine})");
        var secondUserPrompt = RebuildUserPrompt(beforeCobol, secondHalf, afterCobol,
            chunkLabel: $"PART 2 of 2 (lines {overlapStart + 1}-{cobolLines.Length})");

        var firstSystemPrompt = systemPrompt + @"

ADAPTIVE RE-CHUNK INSTRUCTIONS (PART 1 of 2):
- You are processing the FIRST half of a file that was too large for a single pass.
- Output ALL converted code for this portion — do NOT abbreviate or summarize.
- Do NOT close the class — the second part will continue from where you stop.
- End with a comment: // --- END OF PART 1 ---";

        var secondSystemPrompt = systemPrompt + @"

ADAPTIVE RE-CHUNK INSTRUCTIONS (PART 2 of 2):
- You are processing the SECOND half of a file that was continued from a previous part.
- The first ~" + RechunkOverlapLines + @" lines overlap with Part 1 to provide context continuity.
- Do NOT re-convert code that was already converted — skip the overlap region's logic.
- Continue from where Part 1 left off.
- Output ALL converted code for this portion — do NOT abbreviate or summarize.
- End with a comment: // --- END OF PART 2 ---";

        // Step 5 — Process both halves (sequentially to respect rate limits)
        string? firstResponse = null;
        string? secondResponse = null;

        try
        {
            Logger.LogInformation("[{Agent}] Re-chunk: processing Part 1 of 2 for {Context}…",
                AgentName, contextIdentifier);

            firstResponse = UseResponsesApi && ResponsesClient != null
                ? await ResponsesClient.GetResponseAsync(firstSystemPrompt, firstUserPrompt,
                    maxOutputTokens, reasoningEffort)
                : await ExecuteChatCompletionAsync(firstSystemPrompt, firstUserPrompt,
                    $"{contextIdentifier} [rechunk-part-1]");

            ChatLogger?.LogAIResponse(AgentName, $"{contextIdentifier} [rechunk-part-1]", firstResponse);
        }
        catch (OperationCanceledException)
        {
            // Preserve cancellation semantics for callers.
            throw;
        }
        catch (Exception ex) when (ex is not OperationCanceledException)
        {
            Logger.LogError(ex, "[{Agent}] Re-chunk Part 1 failed for {Context}: {Error}",
                AgentName, contextIdentifier, ex.Message);
            return null;
        }

        try
        {
            Logger.LogInformation("[{Agent}] Re-chunk: processing Part 2 of 2 for {Context}…",
                AgentName, contextIdentifier);

            secondResponse = UseResponsesApi && ResponsesClient != null
                ? await ResponsesClient.GetResponseAsync(secondSystemPrompt, secondUserPrompt,
                    maxOutputTokens, reasoningEffort)
                : await ExecuteChatCompletionAsync(secondSystemPrompt, secondUserPrompt,
                    $"{contextIdentifier} [rechunk-part-2]");

            ChatLogger?.LogAIResponse(AgentName, $"{contextIdentifier} [rechunk-part-2]", secondResponse);
        }
        catch (OperationCanceledException)
        {
            // Preserve cancellation semantics for callers.
            throw;
        }
        catch (Exception ex) when (ex is not OperationCanceledException)
        {
            Logger.LogError(ex, "[{Agent}] Re-chunk Part 2 failed for {Context}: {Error}",
                AgentName, contextIdentifier, ex.Message);

            // Part 1 succeeded but Part 2 failed — return partial result with warning
            if (!string.IsNullOrWhiteSpace(firstResponse))
            {
                Logger.LogWarning(
                    "[{Agent}] Returning partial result (Part 1 only) for {Context}.",
                    AgentName, contextIdentifier);
                return (firstResponse, false,
                    "Adaptive re-chunking: Part 2 failed, returning partial Part 1 result");
            }
            return null;
        }

        // Step 6 — Merge and validate
        var merged = MergeAndValidateChunkOutputs(firstResponse, secondResponse, contextIdentifier);

        EnhancedLogger?.LogBehindTheScenes("ADAPTIVE_RECHUNK", "SUCCESS",
            $"Merged {firstResponse.Length + secondResponse.Length} chars " +
            $"from 2 parts into {merged.Length} chars for {contextIdentifier}",
            AgentName);

        Logger.LogInformation(
            "[{Agent}] Adaptive re-chunking succeeded for {Context}: " +
            "Part 1={Part1Len} chars + Part 2={Part2Len} chars → Merged={MergedLen} chars",
            AgentName, contextIdentifier, firstResponse.Length, secondResponse.Length, merged.Length);

        return (merged, false, null);
    }

    /// <summary>
    /// Extracts COBOL source code from the user prompt.
    /// Looks for content between ```cobol markers, or falls back to the entire prompt.
    /// Returns (cobolContent, textBeforeCobol, textAfterCobol).
    /// </summary>
    private static (string CobolContent, string Before, string After) ExtractCobolFromPrompt(string userPrompt)
    {
        // Look for ```cobol ... ``` markers
        var cobolStart = userPrompt.IndexOf("```cobol", StringComparison.OrdinalIgnoreCase);
        if (cobolStart >= 0)
        {
            var contentStart = userPrompt.IndexOf('\n', cobolStart);
            if (contentStart < 0) contentStart = cobolStart + 8;
            else contentStart++; // skip the newline

            var contentEnd = userPrompt.IndexOf("```", contentStart);
            if (contentEnd < 0) contentEnd = userPrompt.Length;

            var before = userPrompt[..cobolStart];
            var cobol = userPrompt[contentStart..contentEnd];
            var afterStart = Math.Min(contentEnd + 3, userPrompt.Length);
            var after = userPrompt[afterStart..];

            return (cobol.Trim(), before, after);
        }

        // No markers found — check for generic code block
        var genericStart = userPrompt.IndexOf("```", StringComparison.Ordinal);
        if (genericStart >= 0)
        {
            var contentStartGeneric = userPrompt.IndexOf('\n', genericStart);
            if (contentStartGeneric < 0) contentStartGeneric = genericStart + 3;
            else contentStartGeneric++;

            var contentEndGeneric = userPrompt.IndexOf("```", contentStartGeneric);
            if (contentEndGeneric < 0) contentEndGeneric = userPrompt.Length;

            var before = userPrompt[..genericStart];
            var cobol = userPrompt[contentStartGeneric..contentEndGeneric];
            var afterStart = Math.Min(contentEndGeneric + 3, userPrompt.Length);
            var after = userPrompt[afterStart..];

            return (cobol.Trim(), before, after);
        }

        // No code block at all — cannot safely split
        return (string.Empty, userPrompt, string.Empty);
    }

    /// <summary>
    /// Finds the best line index at which to split COBOL content.
    /// Prefers DIVISION or SECTION boundaries near the midpoint.
    /// Falls back to paragraph boundaries, then to the raw midpoint.
    /// </summary>
    private static int FindSemanticSplitPoint(string[] lines)
    {
        var midpoint = lines.Length / 2;
        var searchRadius = lines.Length / 4; // look within ±25% of midpoint

        int bestDivisionLine = -1;
        int bestDivisionDistance = int.MaxValue;
        int bestSectionLine = -1;
        int bestSectionDistance = int.MaxValue;
        int bestParagraphLine = -1;
        int bestParagraphDistance = int.MaxValue;

        var rangeStart = Math.Max(10, midpoint - searchRadius);  // don't split in first 10 lines
        var rangeEnd = Math.Min(lines.Length - 10, midpoint + searchRadius); // don't split in last 10 lines

        for (int i = rangeStart; i < rangeEnd; i++)
        {
            var line = lines[i];
            if (string.IsNullOrWhiteSpace(line)) continue;

            var distance = Math.Abs(i - midpoint);

            // Check for DIVISION boundary (highest priority)
            if (Regex.IsMatch(line, @"\bDIVISION\b", RegexOptions.IgnoreCase) && distance < bestDivisionDistance)
            {
                bestDivisionLine = i;
                bestDivisionDistance = distance;
            }
            // Check for SECTION boundary
            else if (Regex.IsMatch(line, @"\bSECTION\b", RegexOptions.IgnoreCase) && distance < bestSectionDistance)
            {
                bestSectionLine = i;
                bestSectionDistance = distance;
            }
            // Check for paragraph boundary (starts in Area A, ends with period)
            else if (CobolBoundaryRegex.IsMatch(line) && distance < bestParagraphDistance)
            {
                bestParagraphLine = i;
                bestParagraphDistance = distance;
            }
        }

        // Return best available boundary, preferring divisions > sections > paragraphs > midpoint
        if (bestDivisionLine >= 0) return bestDivisionLine;
        if (bestSectionLine >= 0) return bestSectionLine;
        if (bestParagraphLine >= 0) return bestParagraphLine;

        return midpoint;
    }

    /// <summary>
    /// Rebuilds the user prompt with a different COBOL content portion and a chunk label.
    /// </summary>
    private static string RebuildUserPrompt(string before, string cobolContent, string after, string chunkLabel)
    {
        var sb = new StringBuilder();

        if (!string.IsNullOrWhiteSpace(before))
            sb.Append(before);

        sb.AppendLine($"[{chunkLabel}]");
        sb.AppendLine("```cobol");
        sb.AppendLine(cobolContent);
        sb.AppendLine("```");

        if (!string.IsNullOrWhiteSpace(after))
            sb.Append(after);

        return sb.ToString();
    }

    /// <summary>
    /// Merges the output from two re-chunked parts and validates that no content was truncated.
    /// Removes part-boundary markers, deduplicates overlap regions, and checks for truncation signals.
    /// </summary>
    private string MergeAndValidateChunkOutputs(string part1, string part2, string contextIdentifier)
    {
        // Clean up part boundary markers
        part1 = Regex.Replace(part1, @"//\s*-+\s*END OF PART 1\s*-+", "", RegexOptions.IgnoreCase).TrimEnd();
        part2 = Regex.Replace(part2, @"//\s*-+\s*END OF PART 2\s*-+", "", RegexOptions.IgnoreCase).TrimEnd();

        // Remove duplicate package/import declarations from part 2 (they belong in part 1 only)
        var part2Lines = part2.Split('\n').ToList();
        var cleanedPart2Lines = new List<string>();
        var seenPackageOrImport = false;

        for (int idx = 0; idx < part2Lines.Count; idx++)
        {
            var line = part2Lines[idx];
            var trimmed = line.TrimStart();

            // Skip package declarations and import statements at the top of part 2
            // (these would have been generated by part 1 already)
            if (!seenPackageOrImport &&
                (trimmed.StartsWith("package ") || trimmed.StartsWith("import ") ||
                 trimmed.StartsWith("using ") || string.IsNullOrWhiteSpace(trimmed)))
            {
                if (trimmed.StartsWith("package ") || trimmed.StartsWith("import ") || trimmed.StartsWith("using "))
                    seenPackageOrImport = true;
                continue;
            }

            // Also skip duplicate class declarations
            if (!seenPackageOrImport && (trimmed.StartsWith("public class ") || trimmed.StartsWith("public final class ")))
            {
                seenPackageOrImport = true;
                continue;
            }

            cleanedPart2Lines.Add(line);

            // Once we hit actual code, stop filtering and add all remaining lines
            if (!string.IsNullOrWhiteSpace(trimmed) &&
                !trimmed.StartsWith("package ") && !trimmed.StartsWith("import ") && !trimmed.StartsWith("using "))
            {
                cleanedPart2Lines.AddRange(part2Lines.Skip(idx + 1));
                break;
            }
        }

        var mergedPart2 = string.Join("\n", cleanedPart2Lines).TrimStart();

        // Merge with a clear separator comment
        var merged = part1 + "\n\n" +
                     "    // ── Adaptive re-chunk boundary ──────────────────────\n\n" +
                     mergedPart2;

        // Validate: check for common truncation signals
        var truncationPatterns = new[]
        {
            @"//\s*\.\.\.\s*(remaining|rest|continue|more|etc)",
            @"//\s*TODO:?\s*(implement|add|complete|remaining)",
            @"//\s*(omitted|truncated|abbreviated|skipped)",
            @"\.\.\.\s*$"  // trailing ellipsis
        };

        var truncationWarnings = new List<string>();
        foreach (var pattern in truncationPatterns)
        {
            var matches = Regex.Matches(merged, pattern, RegexOptions.IgnoreCase | RegexOptions.Multiline);
            foreach (Match match in matches)
            {
                truncationWarnings.Add(match.Value.Trim());
            }
        }

        if (truncationWarnings.Count > 0)
        {
            Logger.LogWarning(
                "[{Agent}] ⚠️ Truncation signals detected in merged output for {Context}: {Signals}",
                AgentName, contextIdentifier, string.Join("; ", truncationWarnings.Take(5)));

            EnhancedLogger?.LogBehindTheScenes("ADAPTIVE_RECHUNK", "TRUNCATION_WARNING",
                $"Detected {truncationWarnings.Count} potential truncation signal(s) in merged output: " +
                string.Join("; ", truncationWarnings.Take(5)),
                AgentName);
        }
        else
        {
            Logger.LogInformation(
                "[{Agent}] ✅ No truncation signals detected in merged output for {Context}.",
                AgentName, contextIdentifier);
        }

        return merged;
    }

    /// <summary>
    /// Formats a <see cref="BusinessLogic"/> object as a concise text block suitable for injection
    /// into AI conversion prompts. Shared by all converter agents.
    /// </summary>
    /// <param name="businessLogic">The business logic extracted during reverse engineering.</param>
    /// <returns>A formatted string describing the business logic context.</returns>
    protected static string FormatBusinessLogicContext(BusinessLogic businessLogic)
    {
        if (string.IsNullOrWhiteSpace(businessLogic.BusinessPurpose)
            && businessLogic.BusinessRules.Count == 0
            && businessLogic.Features.Count == 0
            && businessLogic.UserStories.Count == 0)
        {
            return string.Empty;
        }

        var sb = new StringBuilder();
        if (!string.IsNullOrWhiteSpace(businessLogic.BusinessPurpose))
        {
            sb.AppendLine($"Purpose: {businessLogic.BusinessPurpose}");
        }
        if (businessLogic.BusinessRules.Count > 0)
        {
            sb.AppendLine("Business Rules:");
            foreach (var rule in businessLogic.BusinessRules)
            {
                sb.AppendLine($"- {rule.Description}");
                if (!string.IsNullOrWhiteSpace(rule.Condition))
                    sb.AppendLine($"  Condition: {rule.Condition}");
                if (!string.IsNullOrWhiteSpace(rule.Action))
                    sb.AppendLine($"  Action: {rule.Action}");
            }
        }
        if (businessLogic.Features.Count > 0)
        {
            sb.AppendLine("Features:");
            foreach (var feature in businessLogic.Features)
            {
                sb.AppendLine($"- {feature.Name}: {feature.Description}");
            }
        }
        if (businessLogic.UserStories.Count > 0)
        {
            sb.AppendLine("Feature Descriptions:");
            foreach (var story in businessLogic.UserStories)
            {
                sb.AppendLine($"- {story.Title}: {story.Action}");
            }
        }
        sb.AppendLine();
        return sb.ToString();
    }

    // ═══════════════════════════════════════════════════════════════════════
    // THREE-TIER CONTENT-AWARE REASONING — works for ALL model families
    // ═══════════════════════════════════════════════════════════════════════

    /// <summary>
    /// Calculates optimal max_output_tokens and reasoning effort based on content complexity.
    /// Uses three-tier system: low / medium / high based on complexity score.
    /// Works the same regardless of model family — only the API call mechanics differ.
    /// </summary>
    protected (int maxOutputTokens, string reasoningEffort) CalculateTokenSettings(
        string systemPrompt,
        string userPrompt)
    {
        var inputTokens = EstimateTokens(systemPrompt) + EstimateTokens(userPrompt);

        var cobolSource = ExtractCobolSourceFromPrompt(userPrompt);
        var complexityScore = CalculateComplexityScore(cobolSource);

        string reasoningEffort;
        double multiplier;

        if (complexityScore >= Profile.HighThreshold)
        {
            reasoningEffort = Profile.HighReasoningEffort;
            multiplier = Profile.HighMultiplier;
        }
        else if (complexityScore >= Profile.MediumThreshold)
        {
            reasoningEffort = Profile.MediumReasoningEffort;
            multiplier = Profile.MediumMultiplier;
        }
        else
        {
            reasoningEffort = Profile.LowReasoningEffort;
            multiplier = Profile.LowMultiplier;
        }

        var estimatedOutputNeeded = (int)(inputTokens * multiplier);

        // Conditional MinOutputTokens floor — only enforce for medium+ complexity
        // or large inputs. For low-complexity small inputs, allow smaller allocations.
        int effectiveMinTokens =
            (complexityScore >= Profile.MediumThreshold || inputTokens >= Profile.MinOutputTokens / 2)
                ? Profile.MinOutputTokens
                : Math.Max(4096, estimatedOutputNeeded);

        var maxOutputTokens = Math.Clamp(estimatedOutputNeeded, effectiveMinTokens, Profile.MaxOutputTokens);

        // Also clamp to the model's actual max output limit (e.g. gpt-4o = 16384)
        // to avoid HTTP 400 "max_tokens is too large" errors
        if (Capabilities.DefaultMaxOutputTokens > 0)
        {
            maxOutputTokens = Math.Min(maxOutputTokens, Capabilities.DefaultMaxOutputTokens);
        }

        Logger.LogInformation(
            "[{Agent}] Token settings ({Model}/{Family}): Input ~{Input}, complexity={Score} → {Tier} " +
            "(effort='{Effort}', multiplier={Mult:F1}×), max_output_tokens={MaxOutput}",
            AgentName, ModelId, Capabilities.Family, inputTokens, complexityScore,
            complexityScore >= Profile.HighThreshold ? "HIGH" :
            complexityScore >= Profile.MediumThreshold ? "MEDIUM" : "LOW",
            reasoningEffort, multiplier, maxOutputTokens);

        return (maxOutputTokens, reasoningEffort);
    }

    /// <summary>
    /// Applies model-specific reasoning options to ChatOptions based on detected capabilities.
    /// Claude → extended thinking budget, Codex/o-series → reasoning_effort, standard → temperature.
    /// </summary>
    private void ApplyModelSpecificOptions(ChatOptions options, string reasoningEffort, int maxOutputTokens)
    {
        switch (Capabilities.Reasoning)
        {
            case ReasoningStrategy.ThinkingBudget:
                // Claude extended thinking
                options.AdditionalProperties ??= new AdditionalPropertiesDictionary();
                var thinkingBudget = CalculateThinkingBudget(maxOutputTokens, reasoningEffort);
                options.AdditionalProperties["thinking"] = new Dictionary<string, object>
                {
                    ["type"] = "enabled",
                    ["budget_tokens"] = thinkingBudget
                };
                Logger.LogInformation(
                    "[{Agent}] Claude thinking budget: {Budget} tokens (effort: {Effort})",
                    AgentName, thinkingBudget, reasoningEffort);
                break;

            case ReasoningStrategy.EffortBased:
                // Codex/o-series reasoning effort via IChatClient
                options.AdditionalProperties ??= new AdditionalPropertiesDictionary();
                options.AdditionalProperties["reasoning_effort"] = reasoningEffort;
                Logger.LogInformation(
                    "[{Agent}] Reasoning effort: '{Effort}' for {Model}",
                    AgentName, reasoningEffort, ModelId);
                break;

            case ReasoningStrategy.None:
            default:
                // Standard model — use temperature for deterministic output
                if (Capabilities.SupportsTemperature)
                {
                    options.Temperature = 0.1f;
                }
                break;
        }
    }

    /// <summary>
    /// Calculates the thinking budget for Claude models based on reasoning effort tier.
    /// Higher effort = more thinking budget.
    /// </summary>
    private static int CalculateThinkingBudget(int maxOutputTokens, string reasoningEffort)
    {
        var ratio = reasoningEffort.ToLowerInvariant() switch
        {
            "high" => 0.7,
            "medium" => 0.5,
            "low" => 0.3,
            _ => 0.5
        };
        return (int)(maxOutputTokens * ratio);
    }

    /// <summary>
    /// Calculates a complexity score for COBOL source using config-driven regex indicators
    /// plus structural baselines.
    /// </summary>
    private int CalculateComplexityScore(string cobolSource)
    {
        if (string.IsNullOrWhiteSpace(cobolSource)) return 0;

        int score = 0;

        foreach (var (regex, weight) in _compiledIndicators)
        {
            var matches = regex.Matches(cobolSource);
            if (matches.Count > 0)
                score += weight * matches.Count;
        }

        var lines = cobolSource.Split('\n');
        var meaningfulLines = Math.Max(CountMeaningfulLines(lines), 1);

        var picDensity = (double)PicRegex.Matches(cobolSource).Count / meaningfulLines;
        if (picDensity > Profile.PicDensityFloor) score += 3;

        var levelDensity = (double)LevelRegex.Matches(cobolSource).Count / meaningfulLines;
        if (levelDensity > Profile.LevelDensityFloor) score += 2;

        if (Profile.EnableAmplifiers)
        {
            if (CopyNearStorageRegex.IsMatch(cobolSource)) score += Profile.CopyNearStorageBonus;
            if (ExecSqlDliRegex.IsMatch(cobolSource)) score += Profile.ExecSqlDliBonus;
        }

        return score;
    }

    /// <summary>
    /// Extracts COBOL source code from a user prompt that may contain instructions and markdown markers.
    /// </summary>
    private static string ExtractCobolSourceFromPrompt(string userPrompt)
    {
        if (string.IsNullOrWhiteSpace(userPrompt))
            return string.Empty;

        var cobolStart = userPrompt.IndexOf("```cobol", StringComparison.OrdinalIgnoreCase);
        if (cobolStart >= 0)
        {
            var contentStart = userPrompt.IndexOf('\n', cobolStart);
            if (contentStart < 0) contentStart = cobolStart + 8;
            else contentStart++;

            var contentEnd = userPrompt.IndexOf("```", contentStart, StringComparison.Ordinal);
            if (contentEnd < 0) contentEnd = userPrompt.Length;

            return userPrompt[contentStart..contentEnd].Trim();
        }

        var genericStart = userPrompt.IndexOf("```", StringComparison.Ordinal);
        if (genericStart >= 0)
        {
            var contentStart = userPrompt.IndexOf('\n', genericStart);
            if (contentStart < 0) contentStart = genericStart + 3;
            else contentStart++;

            var contentEnd = userPrompt.IndexOf("```", contentStart, StringComparison.Ordinal);
            if (contentEnd < 0) contentEnd = userPrompt.Length;

            return userPrompt[contentStart..contentEnd].Trim();
        }

        return userPrompt;
    }

    /// <summary>Estimates the number of tokens in a text string (~3.5 chars/token for code).</summary>
    private static int EstimateTokens(string text)
    {
        if (string.IsNullOrEmpty(text)) return 0;
        return (int)Math.Ceiling(text.Length / 3.5);
    }

    private static int CountMeaningfulLines(string[] lines)
    {
        return lines.Count(line => !NoiseLineRegex.IsMatch(line));
    }

    private static List<(Regex, int)> CompileIndicators(ModelProfileSettings profile)
    {
        var compiled = new List<(Regex, int)>();
        foreach (var indicator in profile.ComplexityIndicators
            .Where(i => !string.IsNullOrWhiteSpace(i.Pattern)))
        {
            try
            {
                var regex = new Regex(indicator.Pattern, RegexOptions.Compiled | RegexOptions.IgnoreCase);
                compiled.Add((regex, indicator.Weight));
            }
            catch (ArgumentException) { /* invalid regex — skip */ }
        }
        return compiled;
    }

    // ═══════════════════════════════════════════════════════════════════════
    // OUTPUT TRUNCATION DETECTION — works for ALL IChatClient providers
    // ═══════════════════════════════════════════════════════════════════════

    /// <summary>
    /// Regex patterns that signal truncated output (trailing ellipsis, TODO stubs, etc.).
    /// </summary>
    private static readonly Regex[] TruncationPatterns = new[]
    {
        new Regex(@"//\s*\.\.\.\s*(remaining|rest|continue|more|etc)", RegexOptions.IgnoreCase | RegexOptions.Compiled),
        new Regex(@"//\s*TODO:?\s*(implement|add|complete|remaining)", RegexOptions.IgnoreCase | RegexOptions.Compiled),
        new Regex(@"//\s*(omitted|truncated|abbreviated|skipped)", RegexOptions.IgnoreCase | RegexOptions.Compiled),
        new Regex(@"\.\.\.\s*$", RegexOptions.Multiline | RegexOptions.Compiled),
    };

    /// <summary>
    /// Checks a ChatResponse for truncation via FinishReason and text-based signals.
    /// Throws <see cref="OutputTruncationException"/> if truncation is detected.
    /// </summary>
    private void DetectTruncation(
        ChatResponse response,
        string responseText,
        int maxOutputTokens,
        string reasoningEffort,
        string contextIdentifier)
    {
        // 1. Check FinishReason — most reliable signal
        if (response.FinishReason == ChatFinishReason.Length)
        {
            Logger.LogWarning(
                "[{Agent}] FinishReason=Length for {Context} — output was truncated at {Tokens} max_output_tokens",
                AgentName, contextIdentifier, maxOutputTokens);

            throw new OutputTruncationException(
                maxOutputTokens, responseText.Length, reasoningEffort, "FinishReason=Length");
        }

        // 2. Check for text-based truncation signals (models sometimes truncate without setting Length)
        if (responseText.Length > 500) // only check substantial responses
        {
            // Check the last ~500 chars for truncation patterns
            var tail = responseText.Length > 500
                ? responseText[^500..]
                : responseText;

            foreach (var pattern in TruncationPatterns)
            {
                var match = pattern.Match(tail);
                if (match.Success)
                {
                    Logger.LogWarning(
                        "[{Agent}] Text-based truncation signal for {Context}: '{Signal}'",
                        AgentName, contextIdentifier, match.Value.Trim());

                    throw new OutputTruncationException(
                        maxOutputTokens, responseText.Length, reasoningEffort,
                        $"Text signal: {match.Value.Trim()}");
                }
            }
        }

        // 3. Check for unclosed code blocks (model cut off mid-output)
        var openBlocks = CountOccurrences(responseText, "```");
        if (openBlocks % 2 != 0)
        {
            Logger.LogWarning(
                "[{Agent}] Unclosed code block for {Context} — likely truncated output",
                AgentName, contextIdentifier);

            throw new OutputTruncationException(
                maxOutputTokens, responseText.Length, reasoningEffort, "Unclosed code block (odd ``` count)");
        }
    }

    private static int CountOccurrences(string text, string pattern)
    {
        int count = 0;
        int idx = 0;
        while ((idx = text.IndexOf(pattern, idx, StringComparison.Ordinal)) >= 0)
        {
            count++;
            idx += pattern.Length;
        }
        return count;
    }
}
