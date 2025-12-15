using Microsoft.Extensions.Logging;
using Microsoft.SemanticKernel;
using Microsoft.SemanticKernel.ChatCompletion;
using Microsoft.SemanticKernel.Connectors.OpenAI;
using Microsoft.SemanticKernel.Http;
using CobolToQuarkusMigration.Agents.Interfaces;
using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Helpers;
using System.ClientModel;
using System.Diagnostics;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Net.Sockets;
using System.Reflection;

namespace CobolToQuarkusMigration.Agents;

/// <summary>
/// Implementation of the COBOL analyzer agent with enhanced API call tracking.
/// </summary>
public class CobolAnalyzerAgent : ICobolAnalyzerAgent
{
    private readonly IKernelBuilder _kernelBuilder;
    private readonly ILogger<CobolAnalyzerAgent> _logger;
    private readonly string _modelId;
    private readonly EnhancedLogger? _enhancedLogger;
    private readonly ChatLogger? _chatLogger;

    /// <summary>
    /// Initializes a new instance of the <see cref="CobolAnalyzerAgent"/> class.
    /// </summary>
    /// <param name="kernelBuilder">The kernel builder.</param>
    /// <param name="logger">The logger.</param>
    /// <param name="modelId">The model ID to use for analysis.</param>
    /// <param name="enhancedLogger">Enhanced logger for API call tracking.</param>
    /// <param name="chatLogger">Chat logger for Azure OpenAI conversation tracking.</param>
    public CobolAnalyzerAgent(IKernelBuilder kernelBuilder, ILogger<CobolAnalyzerAgent> logger, string modelId, EnhancedLogger? enhancedLogger = null, ChatLogger? chatLogger = null)
    {
        _kernelBuilder = kernelBuilder;
        _logger = logger;
        _modelId = modelId;
        _enhancedLogger = enhancedLogger;
        _chatLogger = chatLogger;
    }

    /// <inheritdoc/>
    public async Task<CobolAnalysis> AnalyzeCobolFileAsync(CobolFile cobolFile)
    {
        var stopwatch = Stopwatch.StartNew();

        _logger.LogInformation("Analyzing COBOL file: {FileName}", cobolFile.FileName);
        _enhancedLogger?.LogBehindTheScenes("AI_PROCESSING", "COBOL_ANALYSIS_START",
            $"Starting analysis of {cobolFile.FileName}", cobolFile.FileName);

        var kernel = _kernelBuilder.Build();

        // Declare apiCallId outside try block for proper scope
        var apiCallId = 0;

        try
        {
            // Create system prompt for COBOL analysis
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


Your analysis should be structured in a way that can be easily parsed by a Java conversion system.
";

            // Create prompt for COBOL analysis
            var prompt = $@"
Analyze the following COBOL program:

```cobol
{cobolFile.Content}
```

Provide a detailed, structured analysis as described in your instructions.
";

            // Log API call start
            apiCallId = _enhancedLogger?.LogApiCallStart(
                "CobolAnalyzerAgent",
                "POST",
                "Azure OpenAI Chat Completion",
                _modelId,
                $"Analyzing {cobolFile.FileName} ({cobolFile.Content.Length} chars)"
            ) ?? 0;

            // Log user message to chat logger
            _chatLogger?.LogUserMessage("CobolAnalyzerAgent", cobolFile.FileName, prompt, systemPrompt);

            // Create execution settings
            var executionSettings = new OpenAIPromptExecutionSettings
            {
                // gpt-5-mini only supports default temperature (1) and topP (1)
                // Model ID/deployment name is handled at the kernel level
                ExtensionData = new Dictionary<string, object>
                {
                    ["max_completion_tokens"] = 32768  // gpt-5-mini uses max_completion_tokens
                }
            };

            // Create the full prompt including system and user message
            var fullPrompt = $"{systemPrompt}\n\n{prompt}";

            _enhancedLogger?.LogBehindTheScenes("AI_PROCESSING", "PROMPT_GENERATION",
                "Generated analysis prompt", $"System prompt: {systemPrompt.Length} chars, User prompt: {prompt.Length} chars");

            // Convert OpenAI settings to kernel arguments
            var kernelArguments = new KernelArguments(executionSettings);

            var functionResult = await kernel.InvokePromptAsync(
                fullPrompt,
                kernelArguments);

            var analysisText = functionResult.GetValue<string>() ?? string.Empty;

            // Log AI response to chat logger
            _chatLogger?.LogAIResponse("CobolAnalyzerAgent", cobolFile.FileName, analysisText);

            stopwatch.Stop();

            // Log API call completion
            _enhancedLogger?.LogApiCallEnd(apiCallId, analysisText, analysisText.Length / 4, 0.001m); // Rough token estimate
            _enhancedLogger?.LogPerformanceMetrics($"COBOL Analysis - {cobolFile.FileName}", stopwatch.Elapsed, 1);

            // Parse the analysis into a structured object
            var analysis = new CobolAnalysis
            {
                FileName = cobolFile.FileName,
                FilePath = cobolFile.FilePath,
                IsCopybook = cobolFile.IsCopybook,
                RawAnalysisData = analysisText
            };

            // In a real implementation, we would parse the analysis text to extract structured data
            // For this example, we'll just set some basic information
            analysis.ProgramDescription = "Extracted from AI analysis";

            _logger.LogInformation("Completed analysis of COBOL file: {FileName}", cobolFile.FileName);

            return analysis;
        }
        catch (Exception ex) when (ShouldFallback(ex))
        {
            stopwatch.Stop();

            if (apiCallId > 0)
            {
                _enhancedLogger?.LogApiCallError(apiCallId, ex.Message);
            }

            var reason = GetFallbackReason(ex);
            var fallback = CreateFallbackAnalysis(cobolFile, reason);
            _enhancedLogger?.LogBehindTheScenes("WARNING", "COBOL_ANALYSIS_FALLBACK",
                $"Skipping AI analysis for {cobolFile.FileName}: {reason}", ex.GetType().Name);
            _logger.LogWarning(ex, "Skipping AI analysis for {FileName}. Using fallback analysis. Reason: {Reason}", cobolFile.FileName, reason);

            return fallback;
        }
        catch (Exception ex)
        {
            stopwatch.Stop();

            // Log API call error if we have a call ID
            if (apiCallId > 0)
            {
                _enhancedLogger?.LogApiCallError(apiCallId, ex.Message);
            }

            _enhancedLogger?.LogBehindTheScenes("ERROR", "COBOL_ANALYSIS_FAILED",
                $"Failed to analyze {cobolFile.FileName}: {ex.Message}", ex.GetType().Name);

            _logger.LogError(ex, "Error analyzing COBOL file: {FileName}", cobolFile.FileName);
            throw;
        }
    }

    /// <inheritdoc/>
    public async Task<List<CobolAnalysis>> AnalyzeCobolFilesAsync(List<CobolFile> cobolFiles, Action<int, int>? progressCallback = null)
    {
        _logger.LogInformation("Analyzing {Count} COBOL files", cobolFiles.Count);

        var analyses = new List<CobolAnalysis>();
        int processedCount = 0;

        foreach (var cobolFile in cobolFiles)
        {
            var analysis = await AnalyzeCobolFileAsync(cobolFile);
            analyses.Add(analysis);

            processedCount++;
            progressCallback?.Invoke(processedCount, cobolFiles.Count);
        }

        _logger.LogInformation("Completed analysis of {Count} COBOL files", cobolFiles.Count);

        return analyses;
    }

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

    private static bool IsUnauthorizedException(Exception exception)
    {
        var statusCode = ExtractStatusCode(exception);
        return statusCode is 401 or 403;
    }

    private static bool ShouldFallback(Exception exception)
    {
        return IsUnauthorizedException(exception) || IsNetworkException(exception);
    }

    private static bool IsNetworkException(Exception exception)
    {
        switch (exception)
        {
            case HttpRequestException:
            case SocketException:
                return true;
            case HttpOperationException http when http.InnerException != null:
                return IsNetworkException(http.InnerException);
            case ClientResultException client when client.InnerException != null:
                return IsNetworkException(client.InnerException);
            case AggregateException aggregate:
                return aggregate.InnerExceptions.Any(IsNetworkException);
            default:
                return exception.InnerException != null && IsNetworkException(exception.InnerException);
        }
    }

    private static string GetFallbackReason(Exception exception)
    {
        var innermost = exception;
        while (innermost.InnerException != null)
        {
            innermost = innermost.InnerException;
        }

        var message = innermost.Message;
        return string.IsNullOrWhiteSpace(message)
            ? exception.Message
            : message.Replace('\r', ' ').Replace('\n', ' ').Trim();
    }

    private static int? ExtractStatusCode(Exception exception)
    {
        switch (exception)
        {
            case HttpOperationException httpException when httpException.StatusCode.HasValue:
                return (int)httpException.StatusCode.Value;
            case ClientResultException clientException:
                return clientException.Status;
            case AggregateException aggregateException:
                foreach (var inner in aggregateException.InnerExceptions)
                {
                    var aggregateStatus = ExtractStatusCode(inner);
                    if (aggregateStatus.HasValue)
                    {
                        return aggregateStatus;
                    }
                }
                break;
        }

        var statusCodeProperty = exception.GetType().GetRuntimeProperty("StatusCode");
        if (statusCodeProperty?.GetValue(exception) is HttpStatusCode httpStatus)
        {
            return (int)httpStatus;
        }

        if (statusCodeProperty?.GetValue(exception) is int statusInt)
        {
            return statusInt;
        }

        var statusProperty = exception.GetType().GetRuntimeProperty("Status");
        if (statusProperty?.GetValue(exception) is int status)
        {
            return status;
        }

        return exception.InnerException != null ? ExtractStatusCode(exception.InnerException) : null;
    }
}
