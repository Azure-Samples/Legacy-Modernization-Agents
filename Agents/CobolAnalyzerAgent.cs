using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Agents.Infrastructure;
using CobolToQuarkusMigration.Agents.Interfaces;
using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Helpers;
using System.Diagnostics;

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
    private readonly RateLimiter? _rateLimiter;
    private readonly AppSettings? _settings;
    private readonly bool _useResponsesApi;

    private string AgentName => "CobolAnalyzerAgent";

    /// <summary>
    /// Initializes a new instance using Responses API (for codex models).
    /// </summary>
    public CobolAnalyzerAgent(
        ResponsesApiClient responsesClient,
        ILogger<CobolAnalyzerAgent> logger,
        string modelId,
        EnhancedLogger? enhancedLogger = null,
        ChatLogger? chatLogger = null,
        RateLimiter? rateLimiter = null,
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
    }

    /// <summary>
    /// Initializes a new instance using Chat Completions API (for chat models).
    /// </summary>
    public CobolAnalyzerAgent(
        IChatClient chatClient,
        ILogger<CobolAnalyzerAgent> logger,
        string modelId,
        EnhancedLogger? enhancedLogger = null,
        ChatLogger? chatLogger = null,
        RateLimiter? rateLimiter = null,
        AppSettings? settings = null)
    {
        _chatClient = chatClient ?? throw new ArgumentNullException(nameof(chatClient));
        _logger = logger ?? throw new ArgumentNullException(nameof(logger));
        _modelId = modelId ?? throw new ArgumentNullException(nameof(modelId));
        _enhancedLogger = enhancedLogger;
        _chatLogger = chatLogger;
        _rateLimiter = rateLimiter;
        _settings = settings;
        _useResponsesApi = false;
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
            var systemPrompt = @"
You are an expert COBOL analyzer. Your task is to analyze COBOL source code and extract key information about the program structure, variables, paragraphs, logic flow and embedded SQL or DB2.
Analyze the provided COBOL program and provide a detailed, structured analysis that includes:

1. Overall program description
2. Data divisions and their purpose
3. Procedure divisions and their purpose
4. Variables (name, level, type, size, group structure)
5. Paragraphs/sections (name, description, logic, variables used, paragraphs called)
6. Copybooks referenced
7. File access (file name, mode, verbs used, status variable, FD linkage)
8. Any embedded SQL or DB2 statements (type, purpose, variables used)

Your analysis should be structured in a way that can be easily parsed by a conversion system.
If the file appears truncated, focus on analyzing the visible portions and note what sections are missing.
";

            // User prompt for COBOL analysis
            var userPrompt = $@"
Analyze the following COBOL program:

```cobol
{contentToAnalyze}
```

Provide a detailed, structured analysis as described in your instructions.
";

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

            var semaphore = new SemaphoreSlim(maxParallel, maxParallel);
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
                string response;
                
                // Apply rate limiting if configured
                if (_rateLimiter != null)
                {
                    await _rateLimiter.WaitForRateLimitAsync(TokenHelper.EstimateTokens(systemPrompt + userPrompt));
                }

                // Log the request
                _chatLogger?.LogUserMessage(AgentName, contextIdentifier, userPrompt, systemPrompt);
                
                if (_useResponsesApi && _responsesClient != null)
                {
                    _enhancedLogger?.LogBehindTheScenes("API_CALL", "ResponsesAPI",
                        $"Calling Azure OpenAI Responses API for {contextIdentifier}", AgentName);
                    // Use auto-optimized token settings based on input size
                    response = await _responsesClient.GetResponseAutoAsync(systemPrompt, userPrompt);
                }
                else if (_chatClient != null)
                {
                    _enhancedLogger?.LogBehindTheScenes("API_CALL", "ChatCompletion",
                        $"Calling Azure OpenAI Chat API for {contextIdentifier}", AgentName);
                    
                    var messages = new List<Microsoft.Extensions.AI.ChatMessage>
                    {
                        new Microsoft.Extensions.AI.ChatMessage(Microsoft.Extensions.AI.ChatRole.System, systemPrompt),
                        new Microsoft.Extensions.AI.ChatMessage(Microsoft.Extensions.AI.ChatRole.User, userPrompt)
                    };
                    
                    var options = new Microsoft.Extensions.AI.ChatOptions
                    {
                        ModelId = _modelId,
                        // NOTE: gpt-5.1-chat does NOT support custom temperature, only default (1.0)
                        MaxOutputTokens = 16384
                    };
                    
                    var chatResponse = await _chatClient.GetResponseAsync(messages, options);
                    response = ExtractResponseText(chatResponse);
                }
                else
                {
                    throw new InvalidOperationException("No API client configured");
                }

                // Log the response
                _chatLogger?.LogAIResponse(AgentName, contextIdentifier, response);
                _enhancedLogger?.LogBehindTheScenes("API_RESPONSE", _useResponsesApi ? "ResponsesAPI" : "ChatCompletion",
                    $"Received {response.Length} chars from Azure OpenAI", AgentName);

                _rateLimiter?.ReleaseSlot();
                return (response, false, null);
            }
            catch (Exception ex) when (IsTransientError(ex) && attempt < maxRetries)
            {
                lastException = ex;
                var delay = TimeSpan.FromSeconds(Math.Pow(2, attempt));
                _logger.LogWarning("[{Agent}] Transient error on attempt {Attempt}/{MaxRetries} for {Context}. Retrying in {Delay}s. Error: {Error}",
                    AgentName, attempt, maxRetries, contextIdentifier, delay.TotalSeconds, ex.Message);
                await Task.Delay(delay);
            }
            catch (Exception ex) when (IsRateLimitError(ex) && attempt < maxRetries)
            {
                lastException = ex;
                var delay = TimeSpan.FromSeconds(Math.Pow(2, attempt + 2));
                _logger.LogWarning("[{Agent}] Rate limited on attempt {Attempt}/{MaxRetries} for {Context}. Retrying in {Delay}s",
                    AgentName, attempt, maxRetries, contextIdentifier, delay.TotalSeconds);
                await Task.Delay(delay);
            }
            catch (Exception ex)
            {
                _rateLimiter?.ReleaseSlot();
                _logger.LogError(ex, "[{Agent}] Non-retryable error for {Context}: {Error}",
                    AgentName, contextIdentifier, ex.Message);
                return (string.Empty, true, ex.Message);
            }
        }

        var finalReason = $"Max retries ({maxRetries}) exhausted. Last error: {lastException?.Message}";
        _logger.LogError("[{Agent}] {Reason}", AgentName, finalReason);
        return (string.Empty, true, finalReason);
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

    private bool IsTransientError(Exception ex)
    {
        var message = ex.Message.ToLowerInvariant();
        return message.Contains("timeout") || message.Contains("temporarily unavailable") ||
               message.Contains("service unavailable") || message.Contains("502") ||
               message.Contains("503") || message.Contains("504") || message.Contains("connection") ||
               ex is HttpRequestException || ex is TaskCanceledException;
    }

    private bool IsRateLimitError(Exception ex)
    {
        var message = ex.Message.ToLowerInvariant();
        return message.Contains("rate limit") || message.Contains("429") ||
               message.Contains("too many requests") || message.Contains("quota exceeded");
    }

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
}
