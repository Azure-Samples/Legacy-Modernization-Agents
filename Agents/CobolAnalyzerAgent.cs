using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Agents.Infrastructure;
using CobolToQuarkusMigration.Agents.Interfaces;
using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Helpers;
using System.Diagnostics;
using System.Text;
using System.Text.RegularExpressions;
using ChatMessage = Microsoft.Extensions.AI.ChatMessage;

namespace CobolToQuarkusMigration.Agents;

/// <summary>
/// Implementation of the COBOL analyzer agent supporting both Chat Completions API and Responses API.
/// Analyzes COBOL source files and extracts structured information about program structure,
/// variables, paragraphs, logic flow, and embedded SQL/DB2.
/// </summary>
public class CobolAnalyzerAgent : ICobolAnalyzerAgent
{
    private readonly ResponsesApiClient? _responsesClient;
    private readonly IChatClient? _chatClient;
    private readonly ILogger<CobolAnalyzerAgent> _logger;
    private readonly string _modelId;
    private readonly EnhancedLogger? _enhancedLogger;
    private readonly ChatLogger? _chatLogger;
    private readonly IRateLimiter? _rateLimiter;
    private readonly AppSettings? _settings;
    private readonly bool _useResponsesApi;

    /// <summary>Detected capabilities of the configured model.</summary>
    private ModelCapabilities Capabilities { get; }

    /// <summary>The model profile controlling reasoning effort tiers and token limits.</summary>
    private ModelProfileSettings Profile { get; }

    // Pre-compiled config-driven indicator regexes (built at construction)
    private readonly List<(Regex regex, int weight)> _compiledIndicators;

    private string AgentName => "CobolAnalyzerAgent";

    private string ProviderName =>
        _chatClient is Infrastructure.CopilotChatClient ? "GitHub Copilot" : "Azure OpenAI";

    /// <summary>
    /// Creates a CobolAnalyzerAgent, routing to Responses API or Chat API based on availability.
    /// </summary>
    public static CobolAnalyzerAgent Create(
        ResponsesApiClient? responsesClient,
        IChatClient? chatClient,
        ILogger<CobolAnalyzerAgent> logger,
        string modelId,
        EnhancedLogger? enhancedLogger = null,
        ChatLogger? chatLogger = null,
        IRateLimiter? rateLimiter = null,
        AppSettings? settings = null)
    {
        return responsesClient != null
            ? new CobolAnalyzerAgent(responsesClient, logger, modelId, enhancedLogger, chatLogger, rateLimiter, settings)
            : new CobolAnalyzerAgent(chatClient!, logger, modelId, enhancedLogger, chatLogger, rateLimiter, settings);
    }

    /// <summary>
    /// Initializes a new instance using Responses API (for codex models).
    /// </summary>
    public CobolAnalyzerAgent(
        ResponsesApiClient responsesClient,
        ILogger<CobolAnalyzerAgent> logger,
        string modelId,
        EnhancedLogger? enhancedLogger = null,
        ChatLogger? chatLogger = null,
        IRateLimiter? rateLimiter = null,
        AppSettings? settings = null)
    {
        _responsesClient = responsesClient ?? throw new ArgumentNullException(nameof(responsesClient));
        _logger = logger ?? throw new ArgumentNullException(nameof(logger));
        _modelId = modelId ?? throw new ArgumentNullException(nameof(modelId));
        _enhancedLogger = enhancedLogger;
        _chatLogger = chatLogger;
        _rateLimiter = rateLimiter;
        _settings = settings;
        _useResponsesApi = true;
        Capabilities = ModelCapabilities.Detect(modelId);
        Profile = responsesClient.Profile;
        ContentAwareReasoning.ClampProfileToModel(Profile, Capabilities);
        _compiledIndicators = ContentAwareReasoning.CompileIndicators(Profile);
    }
    public CobolAnalyzerAgent(
        IChatClient chatClient,
        ILogger<CobolAnalyzerAgent> logger,
        string modelId,
        EnhancedLogger? enhancedLogger = null,
        ChatLogger? chatLogger = null,
        IRateLimiter? rateLimiter = null,
        AppSettings? settings = null)
    {
        _chatClient = chatClient ?? throw new ArgumentNullException(nameof(chatClient));
        _logger = logger ?? throw new ArgumentNullException(nameof(logger));
        _modelId = modelId ?? throw new ArgumentNullException(nameof(modelId));
        _enhancedLogger = enhancedLogger;
        _chatLogger = chatLogger;
        _settings = settings;
        _useResponsesApi = false;
        Capabilities = ModelCapabilities.Detect(modelId);
        Profile = settings?.ModelProfile ?? new ModelProfileSettings();
        ContentAwareReasoning.ClampProfileToModel(Profile, Capabilities);
        // Same key as AgentBase, so the analyzer and the converters share one budget and cooldown.
        _rateLimiter = rateLimiter ?? LlmRateLimiter.Shared(
            $"chat|{ProviderName}|{modelId}", Profile,
            settings?.ChunkingSettings?.RateLimitSafetyFactor ?? 0.90, logger);
        _compiledIndicators = ContentAwareReasoning.CompileIndicators(Profile);

        logger.LogInformation(
            "[CobolAnalyzerAgent] IChatClient path: model={Model}, family={Family}, reasoning={Reasoning}",
            modelId, Capabilities.Family, Capabilities.Reasoning);
    }

    /// <inheritdoc/>
    public async Task<CobolAnalysis> AnalyzeCobolFileAsync(CobolFile cobolFile)
    {
        var stopwatch = Stopwatch.StartNew();

        _logger.LogInformation("Analyzing COBOL file: {FileName}", cobolFile.FileName);
        _enhancedLogger?.LogBehindTheScenes("AI_PROCESSING", "COBOL_ANALYSIS_START",
            $"Starting analysis of {cobolFile.FileName}", cobolFile.FileName);

        try
        {
            AiLoopEvents.CancellationToken.ThrowIfCancellationRequested();
            // Character limit for API safety (use configured threshold or default to 150K)
            int maxContentChars = _settings?.ChunkingSettings?.AutoChunkCharThreshold ?? 150_000;

            var contentToAnalyze = cobolFile.Content;

            _logger.LogInformation("File {FileName} has {Length:N0} characters", cobolFile.FileName, contentToAnalyze.Length);

            // NEVER TRUNCATE - fail if file is too large
            if (contentToAnalyze.Length > maxContentChars)
            {
                var errorMsg = $"❌ FILE TOO LARGE: {cobolFile.FileName} has {contentToAnalyze.Length:N0} chars (max: {maxContentChars:N0}). " +
                              "Large files are automatically chunked for processing. Truncation is disabled to preserve context.";

                _logger.LogError(errorMsg);
                AiLoopEvents.Fallback(AgentName, cobolFile.FileName, "input_too_large", null);

                return new CobolAnalysis
                {
                    FileName = cobolFile.FileName,
                    FilePath = cobolFile.FilePath,
                    ProgramDescription = errorMsg,
                    Paragraphs = new List<CobolParagraph>(),
                    Variables = new List<CobolVariable>(),
                    RawAnalysisData = errorMsg
                };
            }

            var estimatedInputTokens = TokenHelper.EstimateCobolTokens(contentToAnalyze);
            _logger.LogDebug("Estimated input tokens for {FileName}: {Tokens}", cobolFile.FileName, estimatedInputTokens);

            // System prompt for COBOL analysis
            var systemPrompt = PromptLoader.LoadSection("CobolAnalyzer", "System");

            // User prompt for COBOL analysis
            var userPrompt = PromptLoader.LoadSection("CobolAnalyzer", "User", new Dictionary<string, string>
            {
                ["CobolContent"] = contentToAnalyze
            });

            var (analysisText, usedFallback, fallbackReason) = await ExecuteWithFallbackAsync(
                systemPrompt,
                userPrompt,
                cobolFile.FileName);

            if (usedFallback)
            {
                return CreateFallbackAnalysis(cobolFile, fallbackReason ?? "Unknown error");
            }

            stopwatch.Stop();
            _enhancedLogger?.LogPerformanceMetrics($"COBOL Analysis - {cobolFile.FileName}", stopwatch.Elapsed, 1);

            // Parse the analysis into a structured object
            var analysis = new CobolAnalysis
            {
                FileName = cobolFile.FileName,
                FilePath = cobolFile.FilePath,
                IsCopybook = cobolFile.IsCopybook,
                RawAnalysisData = analysisText,
                ProgramDescription = "Extracted from AI analysis"
            };

            _logger.LogInformation("Completed analysis of COBOL file: {FileName}", cobolFile.FileName);

            return analysis;
        }
        catch (Exception ex)
        {
            stopwatch.Stop();

            var detailedError = BuildDetailedErrorMessage(ex, cobolFile.FileName);
            _enhancedLogger?.LogBehindTheScenes("ERROR", "COBOL_ANALYSIS_FAILED",
                $"Failed to analyze {cobolFile.FileName}: {ex.Message}", detailedError);

            _logger.LogError(ex, "Error analyzing COBOL file: {FileName}", cobolFile.FileName);
            throw;
        }
    }

    /// <inheritdoc/>
    public async Task<List<CobolAnalysis>> AnalyzeCobolFilesAsync(List<CobolFile> cobolFiles, Action<int, int>? progressCallback = null)
    {
        _logger.LogInformation("Analyzing {Count} COBOL files", cobolFiles.Count);

        int processedCount = 0;
        var lockObj = new object();

        // Get max parallel analysis from settings (default to 6)
        var maxParallel = Math.Min(_settings?.ChunkingSettings?.MaxParallelAnalysis ?? 6, cobolFiles.Count);
        var enableParallel = _settings?.ChunkingSettings?.EnableParallelProcessing ?? true;

        if (enableParallel && cobolFiles.Count > 1 && maxParallel > 1)
        {
            _logger.LogInformation("🚀 Using parallel analysis with {Workers} workers for {FileCount} files", maxParallel, cobolFiles.Count);

            using var semaphore = new SemaphoreSlim(maxParallel, maxParallel);
            var staggerDelay = _settings?.ChunkingSettings?.ParallelStaggerDelayMs ?? 500;

            // Use indexed tuples to preserve original order after parallel completion
            var indexedTasks = new List<Task<(int Index, CobolAnalysis Analysis)>>();

            for (int i = 0; i < cobolFiles.Count; i++)
            {
                var file = cobolFiles[i];
                var index = i;

                var task = Task.Run(async () =>
                {
                    await semaphore.WaitAsync();
                    try
                    {
                        var analysis = await AnalyzeCobolFileAsync(file);

                        lock (lockObj)
                        {
                            processedCount++;
                            progressCallback?.Invoke(processedCount, cobolFiles.Count);
                            _logger.LogInformation("📊 Analysis progress: {Completed}/{Total} files ({Percent:F1}%) - {FileName}",
                                processedCount, cobolFiles.Count, (processedCount * 100.0) / cobolFiles.Count, file.FileName);
                        }

                        return (Index: index, Analysis: analysis);
                    }
                    finally
                    {
                        semaphore.Release();
                    }
                });

                indexedTasks.Add(task);

                // Stagger task starts to avoid burst requests
                await Task.Delay(staggerDelay);
            }

            var results = await Task.WhenAll(indexedTasks);

            // Sort by original index to preserve file order for context coherence
            var analyses = results
                .OrderBy(r => r.Index)
                .Select(r => r.Analysis)
                .ToList();

            _logger.LogInformation("✅ Completed parallel analysis of {Count} COBOL files (order preserved)", cobolFiles.Count);
            return analyses;
        }
        else
        {
            // Sequential processing for single file or when parallel is disabled
            var analyses = new List<CobolAnalysis>();
            foreach (var cobolFile in cobolFiles)
            {
                var analysis = await AnalyzeCobolFileAsync(cobolFile);
                analyses.Add(analysis);

                processedCount++;
                progressCallback?.Invoke(processedCount, cobolFiles.Count);
            }

            _logger.LogInformation("✅ Completed sequential analysis of {Count} COBOL files", cobolFiles.Count);
            return analyses;
        }
    }

    #region Helper Methods

    private async Task<(string Response, bool UsedFallback, string? FallbackReason)> ExecuteWithFallbackAsync(
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
                var response = await ExecuteAttemptAsync(systemPrompt, userPrompt, contextIdentifier);
                return (response, false, null);
            }
            catch (OperationCanceledException)
            {
                throw;
            }
            // ── Output truncation catch ──
            catch (OutputTruncationException otex)
            {
                _logger.LogWarning(
                    "[CobolAnalyzerAgent] Output truncated for {Context}: {Message}",
                    contextIdentifier, otex.Message);

                _enhancedLogger?.LogBehindTheScenes("OUTPUT_TRUNCATION", "DETECTED",
                    $"max_output_tokens={otex.MaxOutputTokens}, output={otex.OutputCharCount} chars, " +
                    $"effort='{otex.ReasoningEffort}', signal='{otex.TruncationSignal}'", AgentName);

                var currentMaxTokens = otex.MaxOutputTokens;
                var currentEffort = otex.ReasoningEffort;
                var maxTruncRetries = Profile.ReasoningExhaustionMaxRetries;

                for (int truncRetry = 0; truncRetry < maxTruncRetries; truncRetry++)
                {
                    currentMaxTokens = (int)(currentMaxTokens * Profile.ReasoningExhaustionRetryMultiplier);
                    currentMaxTokens = Math.Min(currentMaxTokens, Profile.MaxOutputTokens);

                    if (currentEffort == Profile.LowReasoningEffort && currentEffort != Profile.MediumReasoningEffort)
                        currentEffort = Profile.MediumReasoningEffort;
                    else if (currentEffort == Profile.MediumReasoningEffort && currentEffort != Profile.HighReasoningEffort)
                        currentEffort = Profile.HighReasoningEffort;

                    if (currentMaxTokens >= Profile.MaxOutputTokens && currentEffort == Profile.HighReasoningEffort)
                    {
                        _logger.LogError(
                            "[CobolAnalyzerAgent] Thrash guard: max tokens ({Tokens}) and max effort ('{Effort}') for {Context}.",
                            currentMaxTokens, currentEffort, contextIdentifier);
                        break;
                    }

                    _logger.LogInformation(
                        "[CobolAnalyzerAgent] Truncation retry {Retry}/{Max} for {Context}: tokens={Tokens}, effort='{Effort}'",
                        truncRetry + 1, maxTruncRetries, contextIdentifier, currentMaxTokens, currentEffort);
                    AiLoopEvents.Retry(AgentName, contextIdentifier, "output_truncation", truncRetry + 1);

                    try
                    {
                        var retryText = await ExecuteAttemptAsync(systemPrompt, userPrompt, contextIdentifier,
                            currentMaxTokens, currentEffort);

                        _enhancedLogger?.LogBehindTheScenes("OUTPUT_TRUNCATION_RECOVERED", "SUCCESS",
                            $"Recovered on retry {truncRetry + 1} with tokens={currentMaxTokens}", AgentName);

                        return (retryText, false, null);
                    }
                    catch (OutputTruncationException)
                    {
                        _logger.LogWarning("[CobolAnalyzerAgent] Still truncated after retry {Retry} with tokens={Tokens}",
                            truncRetry + 1, currentMaxTokens);
                    }
                }

                AiLoopEvents.Fallback(AgentName, contextIdentifier, "output_truncation", null);
                return (string.Empty, true,
                    $"Output truncation: all {maxTruncRetries} escalation retries failed");
            }
            // ── END Output truncation ──
            catch (Exception ex) when (IsRateLimitError(ex) && attempt < maxRetries)
            {
                lastException = ex;
                var retryAfter = LlmErrorClassifier.GetRetryAfter(ex);
                _rateLimiter?.NoteRateLimitResponse(retryAfter ?? LlmErrorClassifier.DefaultRateLimitCooldown);
                var maxWait = TimeSpan.FromSeconds(Profile.RateLimitMaxWaitSeconds);
                if (LlmErrorClassifier.RateLimitDelay(attempt, retryAfter, maxWait) is not { } delay)
                {
                    var reason = $"Rate limited: provider asked to wait {retryAfter!.Value.TotalSeconds:F0}s, over the {maxWait.TotalSeconds:F0}s limit (ModelProfile.RateLimitMaxWaitSeconds)";
                    _logger.LogWarning("[{Agent}] {Reason} for {Context}", AgentName, reason, contextIdentifier);
                    AiLoopEvents.Fallback(AgentName, contextIdentifier, "rate_limit_wait_exceeded", null);
                    return (string.Empty, true, reason);
                }
                _logger.LogWarning("[{Agent}] Rate limited on attempt {Attempt}/{MaxRetries} for {Context}. Retrying in {Delay:F0}s ({Source})",
                    AgentName, attempt, maxRetries, contextIdentifier, delay.TotalSeconds,
                    retryAfter is null ? "back-off" : "Retry-After");
                AiLoopEvents.Retry(AgentName, contextIdentifier, "rate_limit", attempt);
                await Task.Delay(delay, AiLoopEvents.CancellationToken);
            }
            catch (Exception ex) when (IsTransientError(ex) && attempt < maxRetries)
            {
                lastException = ex;
                var delay = LlmErrorClassifier.TransientDelay(attempt);
                _logger.LogWarning("[{Agent}] Transient error on attempt {Attempt}/{MaxRetries} for {Context}. Retrying in {Delay}s. Error: {Error}",
                    AgentName, attempt, maxRetries, contextIdentifier, delay.TotalSeconds, ex.Message);
                AiLoopEvents.Retry(AgentName, contextIdentifier, "transient_error", attempt);
                await Task.Delay(delay, AiLoopEvents.CancellationToken);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "[{Agent}] Non-retryable error for {Context}: {Error}",
                    AgentName, contextIdentifier, ex.Message);
                AiLoopEvents.Fallback(AgentName, contextIdentifier, "non_retryable_error", null);
                return (string.Empty, true, ex.Message);
            }
        }

        var finalReason = $"Max retries ({maxRetries}) exhausted. Last error: {lastException?.Message}";
        _logger.LogError("[{Agent}] {Reason}", AgentName, finalReason);
        AiLoopEvents.Fallback(AgentName, contextIdentifier, "retries_exhausted", null);
        return (string.Empty, true, finalReason);
    }

    private async Task<string> ExecuteAttemptAsync(string systemPrompt, string userPrompt, string context,
        int? maxTokens = null, string? reasoningEffort = null)
    {
        var cancellationToken = AiLoopEvents.CancellationToken;
        cancellationToken.ThrowIfCancellationRequested();
        var estimated = TokenHelper.EstimateTokens(systemPrompt + userPrompt);
        using var reservation = _rateLimiter is null ? null : await _rateLimiter.AcquireAsync(estimated, cancellationToken);
        var requestId = AiLoopEvents.RequestStarted(AgentName, ProviderName, _modelId, context, systemPrompt, userPrompt);
        _chatLogger?.LogUserMessage(AgentName, context, userPrompt, systemPrompt, requestId);
        var stopwatch = Stopwatch.StartNew();
        long? input = null, output = null;
        var outcomeRecorded = false;
        try
        {
            string text;
            if (_useResponsesApi && _responsesClient is not null)
            {
                text = await _responsesClient.GetResponseAutoAsync(systemPrompt, userPrompt, cancellationToken);
            }
            else if (_chatClient is not null)
            {
                var (tokens, effort) = maxTokens is { } supplied
                    ? (supplied, reasoningEffort!)
                    : CalculateTokenSettings(systemPrompt, userPrompt);
                var options = new ChatOptions { ModelId = _modelId, MaxOutputTokens = tokens };
                ApplyModelSpecificOptions(options, effort, tokens);
                var response = await _chatClient.GetResponseAsync(
                    [new ChatMessage(ChatRole.System, systemPrompt), new ChatMessage(ChatRole.User, userPrompt)],
                    options, cancellationToken);
                text = ExtractResponseText(response);
                input = response.Usage?.InputTokenCount;
                output = response.Usage?.OutputTokenCount;
                DetectTruncation(response, text, tokens, effort, context);
            }
            else
            {
                throw new InvalidOperationException("No API client configured");
            }
            AiLoopEvents.Call(AgentName, ProviderName, _modelId, context, stopwatch.ElapsedMilliseconds, true,
                systemPrompt.Length + userPrompt.Length, text.Length, input, output, null, requestId);
            outcomeRecorded = true;
            reservation?.Commit((int)((input ?? estimated) + (output ?? TokenHelper.EstimateTokens(text))));
            _chatLogger?.LogAIResponse(AgentName, context, text, requestId: requestId);
            return text;
        }
        catch (Exception ex)
        {
            if (LlmErrorClassifier.IsRateLimit(ex))
                _rateLimiter?.NoteRateLimitResponse(LlmErrorClassifier.GetRetryAfter(ex) ?? LlmErrorClassifier.DefaultRateLimitCooldown);
            if (!outcomeRecorded)
                AiLoopEvents.Call(AgentName, ProviderName, _modelId, context, stopwatch.ElapsedMilliseconds, false,
                    systemPrompt.Length + userPrompt.Length, 0, input, output, ex.GetType().Name, requestId,
                    ex is OperationCanceledException);
            throw;
        }
    }

    private string ExtractResponseText(Microsoft.Extensions.AI.ChatResponse response)
    {
        if (response == null) return string.Empty;
        var sb = new System.Text.StringBuilder();
        foreach (var message in response.Messages)
        {
            if (message.Role == Microsoft.Extensions.AI.ChatRole.Assistant)
            {
                if (message.Text != null) sb.Append(message.Text);
                else if (message.Contents != null)
                {
                    foreach (var content in message.Contents)
                    {
                        if (content is Microsoft.Extensions.AI.TextContent textContent)
                            sb.Append(textContent.Text);
                    }
                }
            }
        }
        return sb.ToString();
    }

    private static bool IsTransientError(Exception ex) => LlmErrorClassifier.IsTransient(ex);

    private static bool IsRateLimitError(Exception ex) => LlmErrorClassifier.IsRateLimit(ex);

    private string BuildDetailedErrorMessage(Exception ex, string context)
    {
        var sb = new System.Text.StringBuilder();
        sb.AppendLine($"Error in {AgentName} for {context}:");
        sb.AppendLine($"  Message: {ex.Message}");
        sb.AppendLine($"  Type: {ex.GetType().FullName}");
        if (ex.InnerException != null) sb.AppendLine($"  Inner: {ex.InnerException.Message}");
        return sb.ToString();
    }

    #endregion

    private static CobolAnalysis CreateFallbackAnalysis(CobolFile cobolFile, string reason)
    {
        var message = $"AI analysis unavailable for {cobolFile.FileName}: {reason}";

        return new CobolAnalysis
        {
            FileName = cobolFile.FileName,
            FilePath = cobolFile.FilePath,
            IsCopybook = cobolFile.IsCopybook,
            ProgramDescription = $"Analysis skipped because the AI service was unavailable. Reason: {reason}",
            RawAnalysisData = message,
            Paragraphs =
            {
                new CobolParagraph
                {
                    Name = "FALLBACK",
                    Description = "AI analysis unavailable",
                    Logic = message,
                    VariablesUsed = new List<string>(),
                    ParagraphsCalled = new List<string>()
                }
            }
        };
    }

    // ═══════════════════════════════════════════════════════════════════════
    // THREE-TIER CONTENT-AWARE REASONING — delegates to ContentAwareReasoning
    // ═══════════════════════════════════════════════════════════════════════

    private (int maxOutputTokens, string reasoningEffort) CalculateTokenSettings(
        string systemPrompt, string userPrompt)
    {
        var (maxOutputTokens, reasoningEffort) = ContentAwareReasoning.CalculateTokenSettings(
            systemPrompt, userPrompt, Profile, Capabilities, _compiledIndicators);

        var cobolSource = ContentAwareReasoning.ExtractCobolSourceFromPrompt(userPrompt);
        var complexityScore = ContentAwareReasoning.CalculateComplexityScore(cobolSource, Profile, _compiledIndicators);
        var inputTokens = ContentAwareReasoning.EstimateTokens(systemPrompt) + ContentAwareReasoning.EstimateTokens(userPrompt);

        _logger.LogInformation(
            "[CobolAnalyzerAgent] Token settings ({Model}/{Family}): Input ~{Input}, complexity={Score} → {Tier} " +
            "(effort='{Effort}', multiplier={Mult:F1}×), max_output_tokens={MaxOutput}",
            _modelId, Capabilities.Family, inputTokens, complexityScore,
            complexityScore >= Profile.HighThreshold ? "HIGH" :
            complexityScore >= Profile.MediumThreshold ? "MEDIUM" : "LOW",
            reasoningEffort,
            complexityScore >= Profile.HighThreshold ? Profile.HighMultiplier :
            complexityScore >= Profile.MediumThreshold ? Profile.MediumMultiplier : Profile.LowMultiplier,
            maxOutputTokens);

        return (maxOutputTokens, reasoningEffort);
    }

    private void ApplyModelSpecificOptions(ChatOptions options, string reasoningEffort, int maxOutputTokens)
    {
        ContentAwareReasoning.ApplyModelSpecificOptions(options, reasoningEffort, maxOutputTokens, Capabilities);

        if (Capabilities.Reasoning == ReasoningStrategy.ThinkingBudget)
        {
            _logger.LogInformation("[CobolAnalyzerAgent] Claude thinking budget: {Budget} tokens (effort: {Effort})",
                ContentAwareReasoning.CalculateThinkingBudget(maxOutputTokens, reasoningEffort), reasoningEffort);
        }
    }

    // ═══════════════════════════════════════════════════════════════════════
    // OUTPUT TRUNCATION DETECTION — delegates to ContentAwareReasoning
    // ═══════════════════════════════════════════════════════════════════════

    private void DetectTruncation(
        Microsoft.Extensions.AI.ChatResponse response,
        string responseText,
        int maxOutputTokens,
        string reasoningEffort,
        string contextIdentifier)
    {
        try
        {
            ContentAwareReasoning.DetectTruncation(response, responseText, maxOutputTokens, reasoningEffort, contextIdentifier);
        }
        catch (OutputTruncationException otex)
        {
            _logger.LogWarning(
                "[CobolAnalyzerAgent] {Signal} for {Context} — output truncated at {Tokens} max_output_tokens",
                otex.TruncationSignal, contextIdentifier, maxOutputTokens);
            throw;
        }
    }
}
