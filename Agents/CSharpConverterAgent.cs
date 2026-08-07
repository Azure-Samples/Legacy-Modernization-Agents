using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Agents.Infrastructure;
using CobolToQuarkusMigration.Agents.Interfaces;
using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Helpers;
using System.Diagnostics;

using System.Text;

namespace CobolToQuarkusMigration.Agents;

/// <summary>
/// Implementation of the C# converter agent supporting both Responses API (codex) and Chat Completions API.
/// </summary>
public class CSharpConverterAgent : AgentBase, ICodeConverterAgent
{
    /// <inheritdoc/>
    protected override string AgentName => "CSharpConverterAgent";

    public string TargetLanguage => "CSharp";
    public string FileExtension => ".cs";

    /// <summary>
    /// Creates a CSharpConverterAgent, routing to Responses API or Chat API based on availability.
    /// </summary>
    public static CSharpConverterAgent Create(
        ResponsesApiClient? responsesClient,
        IChatClient? chatClient,
        ILogger<CSharpConverterAgent> logger,
        string modelId,
        EnhancedLogger? enhancedLogger = null,
        ChatLogger? chatLogger = null,
        RateLimiter? rateLimiter = null,
        AppSettings? settings = null,
        int? runId = null)
    {
        return responsesClient != null
            ? new CSharpConverterAgent(responsesClient, logger, modelId, enhancedLogger, chatLogger, rateLimiter, settings, runId)
            : new CSharpConverterAgent(chatClient!, logger, modelId, enhancedLogger, chatLogger, rateLimiter, settings, runId);
    }

    private int? _runId;
    private List<BusinessLogic> _businessLogicExtracts = new();

    /// <summary>
    /// Sets the Run ID for the current context.
    /// </summary>
    public void SetRunId(int runId)
    {
        _runId = runId;
    }

    /// <inheritdoc/>
    public void SetBusinessLogicContext(List<BusinessLogic> businessLogicExtracts)
    {
        _businessLogicExtracts = businessLogicExtracts ?? new();
    }

    /// <summary>
    /// Initializes a new instance using Responses API (for codex models like gpt-5.1-codex-mini).
    /// </summary>
    public CSharpConverterAgent(
        ResponsesApiClient responsesClient,
        ILogger<CSharpConverterAgent> logger,
        string modelId,
        EnhancedLogger? enhancedLogger = null,
        ChatLogger? chatLogger = null,
        RateLimiter? rateLimiter = null,
        AppSettings? settings = null,
        int? runId = null)
        : base(responsesClient, logger, modelId, enhancedLogger, chatLogger, rateLimiter, settings)
    {
        _runId = runId;
    }

    /// <summary>
    /// Initializes a new instance using Chat Completions API (for chat models).
    /// </summary>
    public CSharpConverterAgent(
        IChatClient chatClient,
        ILogger<CSharpConverterAgent> logger,
        string modelId,
        EnhancedLogger? enhancedLogger = null,
        ChatLogger? chatLogger = null,
        RateLimiter? rateLimiter = null,
        AppSettings? settings = null,
        int? runId = null)
        : base(chatClient, logger, modelId, enhancedLogger, chatLogger, rateLimiter, settings)
    {
        _runId = runId;
    }

    /// <inheritdoc/>
    public async Task<CodeFile> ConvertAsync(CobolFile cobolFile, CobolAnalysis cobolAnalysis)
    {
        var stopwatch = Stopwatch.StartNew();

        Logger.LogInformation("Converting COBOL file to C#: {FileName}", cobolFile.FileName);
        EnhancedLogger?.LogBehindTheScenes("AI_PROCESSING", "CSHARP_CONVERSION_START",
            $"Starting C# conversion of {cobolFile.FileName}", cobolFile.FileName);

        try
        {
            var systemPrompt = PromptLoader.LoadSectionValidated(
                "CSharpConverter", "System", new Dictionary<string, string>());

            // NOTE: Large files are handled by SmartMigrationOrchestrator which routes them
            // to ChunkedMigrationProcess. Files reaching this agent should fit within API limits.
            // If a file is unexpectedly large, log a warning but proceed (chunking should have caught it)
            var contentToConvert = cobolFile.Content;
            var estimatedTokens = TokenHelper.EstimateTokens(contentToConvert);
            if (estimatedTokens > 15000)
            {
                Logger.LogWarning(
                    "⚠️ Large file {FileName} ({Tokens} tokens) reached CSharpConverterAgent. " +
                    "This should have been routed to ChunkedMigrationProcess. Processing anyway...",
                    cobolFile.FileName, estimatedTokens);
            }

            // Sanitize COBOL content for content filtering
            string sanitizedContent = SanitizeCobolContent(contentToConvert);

            // Inject business logic context from reverse engineering when available
            var businessLogic = _businessLogicExtracts
                .FirstOrDefault(bl => string.Equals(bl.FileName, cobolFile.FileName, StringComparison.OrdinalIgnoreCase));
            var businessLogicContext = businessLogic is null
                ? string.Empty
                : PromptLoader.LoadSectionValidated("CSharpConverter", "BusinessLogic", new Dictionary<string, string>
                {
                    ["BusinessLogic"] = FormatBusinessLogicContext(businessLogic)
                });

            // REKT structural context + shared-types registry (opt-in via ENABLE_REKT_CONTEXT).
            var structuralContextBuilder = new StringBuilder();
            await RektPromptInjector.InjectAsync(
                structuralContextBuilder, "C#", cobolFile.FileName, AgentName, _runId, Logger);

            var userPrompt = PromptLoader.LoadSectionValidated("CSharpConverter", "User", new Dictionary<string, string>
            {
                ["CobolContent"] = sanitizedContent,
                ["Analysis"] = cobolAnalysis.RawAnalysisData,
                ["BusinessLogicContext"] = businessLogicContext,
                ["StructuralContext"] = structuralContextBuilder.ToString()
            });

            var (csharpCode, usedFallback, fallbackReason) = await ExecuteWithFallbackAsync(
                systemPrompt,
                userPrompt,
                cobolFile.FileName);

            if (usedFallback)
            {
                return CreateFallbackCodeFile(cobolFile, cobolAnalysis, fallbackReason ?? "Unknown error");
            }

            stopwatch.Stop();
            EnhancedLogger?.LogBehindTheScenes("AI_PROCESSING", "CSHARP_CONVERSION_COMPLETE",
                $"Completed C# conversion of {cobolFile.FileName} in {stopwatch.ElapsedMilliseconds}ms");

            csharpCode = ExtractCSharpCode(csharpCode);

            // Continuation retry: when the provider truncates mid-output, ask it to
            // resume from the last lines rather than shipping a partial class.
            var hasAnyCode = csharpCode.Contains("{") && (csharpCode.Contains("class ") || csharpCode.Contains("namespace "));
            var maxContinuations = hasAnyCode ? 3 : 0;
            for (int cont = 0; cont < maxContinuations; cont++)
            {
                var hasNs = csharpCode.Contains("namespace ", StringComparison.Ordinal);
                var hasCls = csharpCode.Contains("class ", StringComparison.Ordinal);
                var opens = csharpCode.Count(c => c == '{');
                var closes = csharpCode.Count(c => c == '}');
                if (hasNs && hasCls && opens == closes) break;

                Logger.LogWarning(
                    "[CSharpConverterAgent] Output truncated (ns={HasNs} cls={HasCls} braces={Opens}/{Closes}) — sending continuation {Cont}/{Max}",
                    hasNs, hasCls, opens, closes, cont + 1, maxContinuations);

                var lastLines = string.Join("\n", csharpCode.Split('\n').TakeLast(10));
                var contPrompt = PromptLoader.LoadSectionValidated(
                    "CSharpConverter", "Continuation", new Dictionary<string, string>
                    {
                        ["LastLines"] = lastLines
                    });

                var (contCode, contFallback, _) = await ExecuteWithFallbackAsync(
                    systemPrompt, contPrompt, $"{cobolFile.FileName} [continuation-{cont + 1}]");

                if (contFallback || string.IsNullOrWhiteSpace(contCode)) break;

                contCode = ExtractCSharpCode(contCode);
                var contLines = contCode.Split('\n')
                    .SkipWhile(l => l.TrimStart().StartsWith("namespace ") || l.TrimStart().StartsWith("using ") || l.Trim() == "")
                    .ToList();
                var classIdx = contLines.FindIndex(l => l.Contains("class ") && l.Contains("{"));
                if (classIdx >= 0 && classIdx < 3) contLines = contLines.Skip(classIdx + 1).ToList();

                csharpCode = csharpCode.TrimEnd() + "\n" + string.Join("\n", contLines);
                Logger.LogInformation("[CSharpConverterAgent] Continuation {Cont} appended {Lines} lines",
                    cont + 1, contLines.Count);
            }

            csharpCode = ValidateCSharpCode(csharpCode);

            // Extract AI's semantic class name (based on domain/action/type pattern)
            string aiClassName = ExtractClassNameFromCode(csharpCode);
            string namespaceName = GetNamespaceName(csharpCode);

            // Prefer AI-generated semantic name if it's not generic
            // Fall back to filename-derived name only if AI gave a generic name
            string finalClassName;
            if (NamingHelper.IsSemanticClassName(aiClassName))
            {
                // AI generated a good semantic name like "PaymentBatchValidator"
                finalClassName = aiClassName;
                Logger.LogInformation("Using AI-generated semantic class name: {ClassName}", finalClassName);
            }
            else
            {
                // Fall back to filename-derived name
                finalClassName = NamingHelper.DeriveClassNameFromCobolFile(cobolFile.FileName);
                Logger.LogWarning("AI generated generic class name '{AiClass}', using filename-derived: {ClassName}",
                    aiClassName, finalClassName);

                // Update the code to use the new class name
                if (aiClassName != finalClassName)
                {
                    csharpCode = NamingHelper.ReplaceGenericClassName(csharpCode, aiClassName, finalClassName);
                }
            }

            return new CodeFile
            {
                // Use semantic class name for output filename
                FileName = $"{finalClassName}{FileExtension}",
                Content = csharpCode,
                ClassName = finalClassName,
                NamespaceName = namespaceName,
                OriginalCobolFileName = cobolFile.FileName,
                TargetLanguage = TargetLanguage
            };
        }
        catch (Exception ex)
        {
            stopwatch.Stop();
            EnhancedLogger?.LogBehindTheScenes("ERROR", "CSHARP_CONVERSION_ERROR",
                $"Failed to convert {cobolFile.FileName}: {ex.Message}", ex);
            Logger.LogError(ex, "Error converting COBOL file to C#: {FileName}", cobolFile.FileName);
            throw;
        }
    }

    /// <inheritdoc/>
    public async Task<List<CodeFile>> ConvertAsync(List<CobolFile> cobolFiles, List<CobolAnalysis> cobolAnalyses, Action<int, int>? progressCallback = null)
    {
        Logger.LogInformation("Converting {Count} COBOL files to C#", cobolFiles.Count);

        var maxParallel = Math.Min(
            Settings?.ChunkingSettings?.MaxParallelConversion ?? 1, cobolFiles.Count);
        var enableParallel = maxParallel > 1 && cobolFiles.Count > 1;

        if (enableParallel)
        {
            Logger.LogInformation(
                "\u26a1 Parallel conversion: {Workers} workers for {Files} files",
                maxParallel, cobolFiles.Count);

            var staggerDelay = Settings?.ChunkingSettings?.ParallelStaggerDelayMs ?? 500;
            using var semaphore = new SemaphoreSlim(maxParallel, maxParallel);
            var completed = 0;

            var tasks = cobolFiles.Select((cobolFile, i) =>
            {
                var cobolAnalysis = i < cobolAnalyses.Count ? cobolAnalyses[i] : null;
                return Task.Run(async () =>
                {
                    await semaphore.WaitAsync();
                    try
                    {
                        if (cobolAnalysis == null)
                        {
                            Logger.LogWarning("No analysis found for COBOL file: {FileName}", cobolFile.FileName);
                            return (Index: i, Result: (CodeFile?)null);
                        }

                        await Task.Delay((i % maxParallel) * staggerDelay);
                        var codeFile = await ConvertAsync(cobolFile, cobolAnalysis);
                        var done = Interlocked.Increment(ref completed);
                        progressCallback?.Invoke(done, cobolFiles.Count);
                        return (Index: i, Result: (CodeFile?)codeFile);
                    }
                    finally
                    {
                        semaphore.Release();
                    }
                });
            }).ToList();

            var all = await Task.WhenAll(tasks);
            return all.Where(r => r.Result != null)
                      .OrderBy(r => r.Index)
                      .Select(r => r.Result!)
                      .ToList();
        }

        // Sequential fallback
        var codeFiles = new List<CodeFile>();
        int processedCount = 0;

        for (int i = 0; i < cobolFiles.Count; i++)
        {
            var cobolFile = cobolFiles[i];
            var cobolAnalysis = i < cobolAnalyses.Count ? cobolAnalyses[i] : null;

            if (cobolAnalysis == null)
            {
                Logger.LogWarning("No analysis found for COBOL file: {FileName}", cobolFile.FileName);
                continue;
            }

            var codeFile = await ConvertAsync(cobolFile, cobolAnalysis);
            codeFiles.Add(codeFile);

            processedCount++;
            progressCallback?.Invoke(processedCount, cobolFiles.Count);
        }

        return codeFiles;
    }

    private CodeFile CreateFallbackCodeFile(CobolFile cobolFile, CobolAnalysis cobolAnalysis, string reason)
    {
        var className = NamingHelper.GetFallbackClassName(cobolFile.FileName);
        var namespaceName = "CobolMigration.Fallback";
        var sanitizedReason = reason.Replace("\"", "'");

        var csharpCode = $$"""
namespace {{namespaceName}};

/// <summary>
/// Placeholder implementation generated because the AI conversion service was unavailable.
/// Original COBOL file: {{cobolFile.FileName}}
/// Reason: {{sanitizedReason}}
/// </summary>
public class {{className}}
{
    public void Run()
    {
        throw new NotSupportedException("AI conversion unavailable. Details: {{sanitizedReason}}");
    }
}
""";

        return new CodeFile
        {
            FileName = $"{className}.cs",
            NamespaceName = namespaceName,
            ClassName = className,
            Content = csharpCode,
            OriginalCobolFileName = cobolFile.FileName,
            TargetLanguage = TargetLanguage
        };
    }

    private string ExtractCSharpCode(string input)
    {
        return ConversionOutputGuard.ExtractFencedCode(input, "```csharp", "```c#");
    }

    private string ValidateCSharpCode(string input)
    {
        // Fail loud on unusable output. A silent 0-byte "success" is worse than a
        // file that explains what went wrong, so write a self-documenting stub.
        var hasNsFinal = input.Contains("namespace ", StringComparison.Ordinal);
        var hasClassFinal = input.Contains("class ", StringComparison.Ordinal);
        var opensFinal = input.Count(c => c == '{');
        var closesFinal = input.Count(c => c == '}');
        if (!hasNsFinal || !hasClassFinal || opensFinal != closesFinal)
        {
            Logger.LogWarning(
                "[CSharpConverterAgent] OUTPUT APPEARS TRUNCATED: namespace={HasNs}, class={HasClass}, braces {Opens}/{Closes}. " +
                "The provider likely hit its output token limit.",
                hasNsFinal, hasClassFinal, opensFinal, closesFinal);
            EnhancedLogger?.LogBehindTheScenes("TRUNCATION_DETECTED", "WARNING",
                $"namespace={hasNsFinal}, class={hasClassFinal}, braces={opensFinal}/{closesFinal}");

            if (ConversionOutputGuard.ShouldCreateWholeFileStub(
                    input,
                    hasClassFinal,
                    opensFinal,
                    closesFinal))
            {
                var trimmedReason =
                    !hasClassFinal && !hasNsFinal && opensFinal == 0
                        ? "EMPTY_LLM_RESPONSE — model returned no usable output (often: hit output-token budget, REKT context missing for deps-only programs, or provider rate-limit)."
                    : !hasClassFinal
                        ? "NO_CLASS_KEYWORD — model emitted prose or a non-C# code block."
                    : "BRACE_IMBALANCE — opens=" + opensFinal + " closes=" + closesFinal + ".";

                return ConversionOutputGuard.BuildWholeFileDiagnosticStub(
                    "C#",
                    trimmedReason,
                    input);
            }
        }

        return input;
    }

    /// <summary>
    /// Extracts the class name from generated C# code.
    /// </summary>
    private string ExtractClassNameFromCode(string csharpCode)
    {
        try
        {
            var lines = csharpCode.Split('\n');
            foreach (var line in lines)
            {
                var trimmedLine = line.Trim();
                if (trimmedLine.StartsWith("public class ") || trimmedLine.StartsWith("internal class ") || trimmedLine.StartsWith("class "))
                {
                    var classIndex = trimmedLine.IndexOf("class ", StringComparison.Ordinal);
                    if (classIndex >= 0)
                    {
                        var afterClass = trimmedLine.Substring(classIndex + "class ".Length);
                        var className = afterClass.Split('{', ' ', '\t', '\r', '\n', ':')[0].Trim();
                        if (NamingHelper.IsValidIdentifier(className)) return className;
                    }
                }
            }
        }
        catch (Exception ex)
        {
            Logger.LogWarning(ex, "Error extracting class name from C# code");
        }
        return "ConvertedCobolProgram";
    }

    private string GetNamespaceName(string csharpCode)
    {
        var namespaceIndex = csharpCode.IndexOf("namespace ");
        if (namespaceIndex >= 0)
        {
            var start = namespaceIndex + "namespace ".Length;
            var remaining = csharpCode.Substring(start);
            var end = remaining.IndexOfAny(new[] { ';', '{', '\r', '\n' });
            if (end >= 0) return remaining.Substring(0, end).Trim();
        }
        return "CobolMigration.Legacy";
    }

    private string SanitizeCobolContent(string cobolContent)
    {
        if (string.IsNullOrEmpty(cobolContent)) return cobolContent;

        var sanitizationMap = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)
        {
            {"FEJL", "ERROR_CODE"}, {"FEJLMELD", "ERROR_MSG"}, {"FEJL-", "ERROR_"},
            {"FEJLMELD-", "ERROR_MSG_"}, {"INC-FEJLMELD", "INC-ERROR-MSG"},
            {"FEJL VED KALD", "ERROR IN CALL"}, {"KALD", "CALL_OP"}, {"MEDD-TEKST", "MSG_TEXT"},
        };

        string sanitizedContent = cobolContent;
        foreach (var (original, replacement) in sanitizationMap)
        {
            if (sanitizedContent.Contains(original))
                sanitizedContent = sanitizedContent.Replace(original, replacement);
        }
        return sanitizedContent;
    }
}
