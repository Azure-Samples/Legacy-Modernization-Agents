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
/// Implementation of the Java converter agent supporting both Responses API (codex) and Chat Completions API.
/// Converts COBOL source files to Java/Quarkus code.
/// </summary>
public class JavaConverterAgent : AgentBase, IJavaConverterAgent, ICodeConverterAgent
{
    /// <inheritdoc/>
    protected override string AgentName => "JavaConverterAgent";

    public string TargetLanguage => "Java";
    public string FileExtension => ".java";

    /// <summary>
    /// Creates a JavaConverterAgent, routing to Responses API or Chat API based on availability.
    /// </summary>
    public static JavaConverterAgent Create(
        ResponsesApiClient? responsesClient,
        IChatClient? chatClient,
        ILogger<JavaConverterAgent> logger,
        string modelId,
        EnhancedLogger? enhancedLogger = null,
        ChatLogger? chatLogger = null,
        RateLimiter? rateLimiter = null,
        AppSettings? settings = null,
        int? runId = null)
    {
        return responsesClient != null
            ? new JavaConverterAgent(responsesClient, logger, modelId, enhancedLogger, chatLogger, rateLimiter, settings, runId)
            : new JavaConverterAgent(chatClient!, logger, modelId, enhancedLogger, chatLogger, rateLimiter, settings, runId);
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
    public JavaConverterAgent(
        ResponsesApiClient responsesClient,
        ILogger<JavaConverterAgent> logger,
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
    public JavaConverterAgent(
        IChatClient chatClient,
        ILogger<JavaConverterAgent> logger,
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
    public async Task<JavaFile> ConvertToJavaAsync(CobolFile cobolFile, CobolAnalysis cobolAnalysis)
    {
        var stopwatch = Stopwatch.StartNew();

        Logger.LogInformation("Converting COBOL file to Java: {FileName}", cobolFile.FileName);
        EnhancedLogger?.LogBehindTheScenes("AI_PROCESSING", "JAVA_CONVERSION_START",
            $"Starting Java conversion of {cobolFile.FileName}", cobolFile.FileName);

        try
        {
            // System prompt for Java conversion
            var systemPrompt = PromptLoader.LoadSection("JavaConverter", "System");

            // NOTE: Large files are handled by SmartMigrationOrchestrator which routes them
            // to ChunkedMigrationProcess. Files reaching this agent should fit within API limits.
            // If a file is unexpectedly large, log a warning but proceed (chunking should have caught it)
            var contentToConvert = cobolFile.Content;
            var estimatedTokens = TokenHelper.EstimateTokens(contentToConvert);
            if (estimatedTokens > 15000)
            {
                Logger.LogWarning(
                    "⚠️ Large file {FileName} ({Tokens} tokens) reached JavaConverterAgent. " +
                    "This should have been routed to ChunkedMigrationProcess. Processing anyway...",
                    cobolFile.FileName, estimatedTokens);
            }

            // Sanitize COBOL content for content filtering
            string sanitizedContent = SanitizeCobolContent(contentToConvert);

            // User prompt for Java conversion
            var userPromptBuilder = new StringBuilder();
            userPromptBuilder.AppendLine("Convert the following COBOL program to Java with Quarkus:");
            userPromptBuilder.AppendLine();
            userPromptBuilder.AppendLine("```cobol");
            userPromptBuilder.AppendLine(sanitizedContent);
            userPromptBuilder.AppendLine("```");

            userPromptBuilder.AppendLine();
            userPromptBuilder.AppendLine("Here is the analysis of the COBOL program to help you understand its structure:");
            userPromptBuilder.AppendLine();
            userPromptBuilder.AppendLine(cobolAnalysis.RawAnalysisData);

            // Inject business logic context from reverse engineering when available
            var businessLogic = _businessLogicExtracts
                .FirstOrDefault(bl => string.Equals(bl.FileName, cobolFile.FileName, StringComparison.OrdinalIgnoreCase));
            if (businessLogic != null)
            {
                userPromptBuilder.AppendLine();
                userPromptBuilder.AppendLine("Here is the extracted business logic from the reverse engineering phase. Use this to ensure the converted code faithfully implements all business rules and features:");
                userPromptBuilder.AppendLine();
                userPromptBuilder.Append(FormatBusinessLogicContext(businessLogic));
            }

            // REKT structural context + shared-types registry (opt-in via ENABLE_REKT_CONTEXT).
            await RektPromptInjector.InjectAsync(
                userPromptBuilder, "Java", cobolFile.FileName, AgentName, _runId, Logger);

            userPromptBuilder.AppendLine();
            userPromptBuilder.AppendLine("IMPORTANT REQUIREMENTS:");
            userPromptBuilder.AppendLine("1. Return ONLY the Java code - NO explanations, NO markdown blocks, NO additional text");
            userPromptBuilder.AppendLine("2. Start with: package com.example.something; (single line, lowercase, no comments)");
            userPromptBuilder.AppendLine("3. Do NOT include newlines or explanatory text in the package declaration");
            userPromptBuilder.AppendLine("4. Your response must be valid, compilable Java code starting with 'package' and ending with the class closing brace");

            userPromptBuilder.AppendLine();
            userPromptBuilder.AppendLine("Note: The original code contains Danish error handling terms replaced with placeholders.");

            var userPrompt = userPromptBuilder.ToString();

            var (javaCode, usedFallback, fallbackReason) = await ExecuteWithFallbackAsync(
                systemPrompt,
                userPrompt,
                cobolFile.FileName);

            if (usedFallback)
            {
                return CreateFallbackJavaFile(cobolFile, cobolAnalysis, fallbackReason ?? "Unknown error");
            }

            stopwatch.Stop();
            EnhancedLogger?.LogBehindTheScenes("AI_PROCESSING", "JAVA_CONVERSION_COMPLETE",
                $"Completed Java conversion of {cobolFile.FileName} in {stopwatch.ElapsedMilliseconds}ms");

            // Extract the Java code from markdown code blocks if necessary
            javaCode = ExtractJavaCode(javaCode);

            // Continuation retry: when the provider truncates mid-output, ask it to
            // resume from the last lines rather than shipping a partial class.
            var hasAnyCode = javaCode.Contains("{") && (javaCode.Contains("class ") || javaCode.Contains("void ") || javaCode.Contains("public "));
            var maxContinuations = hasAnyCode ? 3 : 0;
            if (!hasAnyCode && !string.IsNullOrWhiteSpace(javaCode))
            {
                Logger.LogWarning("[JavaConverterAgent] Response contains no valid Java code — skipping continuation");
            }
            for (int cont = 0; cont < maxContinuations; cont++)
            {
                var hasPkg = javaCode.Contains("package ", StringComparison.Ordinal);
                var hasCls = javaCode.Contains("class ", StringComparison.Ordinal);
                var opens = javaCode.Count(c => c == '{');
                var closes = javaCode.Count(c => c == '}');
                if (hasPkg && hasCls && opens == closes) break; // complete

                Logger.LogWarning(
                    "[JavaConverterAgent] Output truncated (pkg={HasPkg} cls={HasCls} braces={Opens}/{Closes}) — sending continuation {Cont}/{Max}",
                    hasPkg, hasCls, opens, closes, cont + 1, maxContinuations);

                var lastLines = string.Join("\n", javaCode.Split('\n').TakeLast(10));
                var contPrompt = $"Your previous response was truncated mid-output. Here are the LAST 10 lines you generated:\n\n```java\n{lastLines}\n```\n\n" +
                    $"Continue from EXACTLY where you left off. Return ONLY the remaining Java code — no package declaration, no class declaration, no imports. " +
                    $"Start with the next line after the fragment above and end with the final closing brace '}}' of the class.";

                var (contCode, contFallback, _) = await ExecuteWithFallbackAsync(
                    systemPrompt, contPrompt, $"{cobolFile.FileName} [continuation-{cont + 1}]");

                if (contFallback || string.IsNullOrWhiteSpace(contCode)) break;

                contCode = ExtractJavaCode(contCode);
                var contLines = contCode.Split('\n')
                    .SkipWhile(l => l.TrimStart().StartsWith("package ") || l.TrimStart().StartsWith("import ") || l.Trim() == "")
                    .ToList();
                var classIdx = contLines.FindIndex(l => l.Contains("class ") && l.Contains("{"));
                if (classIdx >= 0 && classIdx < 3) contLines = contLines.Skip(classIdx + 1).ToList();

                javaCode = javaCode.TrimEnd() + "\n" + string.Join("\n", contLines);
                Logger.LogInformation("[JavaConverterAgent] Continuation {Cont} appended {Lines} lines",
                    cont + 1, contLines.Count);
            }

            // Extract AI's semantic class name (based on domain/action/type pattern)
            string aiClassName = ExtractClassNameFromCode(javaCode);
            string packageName = GetPackageName(javaCode);

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
                    javaCode = NamingHelper.ReplaceGenericClassName(javaCode, aiClassName, finalClassName);
                }
            }

            var javaFile = new JavaFile
            {
                // Use semantic class name for output filename
                FileName = $"{finalClassName}{FileExtension}",
                Content = javaCode,
                ClassName = finalClassName,
                PackageName = packageName,
                OriginalCobolFileName = cobolFile.FileName
            };

            Logger.LogInformation("Completed conversion of COBOL file to Java: {FileName}", cobolFile.FileName);

            return javaFile;
        }
        catch (Exception ex)
        {
            stopwatch.Stop();

            EnhancedLogger?.LogBehindTheScenes("ERROR", "JAVA_CONVERSION_ERROR",
                $"Failed to convert {cobolFile.FileName}: {ex.Message}", ex);

            Logger.LogError(ex, "Error converting COBOL file to Java: {FileName}", cobolFile.FileName);
            throw;
        }
    }

    /// <inheritdoc/>
    async Task<CodeFile> ICodeConverterAgent.ConvertAsync(CobolFile cobolFile, CobolAnalysis cobolAnalysis)
    {
        return await ConvertToJavaAsync(cobolFile, cobolAnalysis);
    }

    /// <inheritdoc/>
    async Task<List<CodeFile>> ICodeConverterAgent.ConvertAsync(List<CobolFile> cobolFiles, List<CobolAnalysis> cobolAnalyses, Action<int, int>? progressCallback)
    {
        var javaFiles = await ConvertToJavaAsync(cobolFiles, cobolAnalyses, progressCallback);
        return javaFiles.Cast<CodeFile>().ToList();
    }

    /// <inheritdoc/>
    public async Task<List<JavaFile>> ConvertToJavaAsync(List<CobolFile> cobolFiles, List<CobolAnalysis> cobolAnalyses, Action<int, int>? progressCallback = null)
    {
        Logger.LogInformation("Converting {Count} COBOL files to Java", cobolFiles.Count);

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
                            return (Index: i, Result: (JavaFile?)null);
                        }

                        await Task.Delay((i % maxParallel) * staggerDelay);
                        var javaFile = await ConvertToJavaAsync(cobolFile, cobolAnalysis);
                        var done = Interlocked.Increment(ref completed);
                        progressCallback?.Invoke(done, cobolFiles.Count);
                        return (Index: i, Result: (JavaFile?)javaFile);
                    }
                    finally
                    {
                        semaphore.Release();
                    }
                });
            }).ToList();

            var all = await Task.WhenAll(tasks);
            var result = all.Where(r => r.Result != null)
                            .OrderBy(r => r.Index)
                            .Select(r => r.Result!)
                            .ToList();

            Logger.LogInformation("Completed parallel conversion of {Count} COBOL files to Java", cobolFiles.Count);
            return result;
        }

        // Sequential fallback
        var javaFiles = new List<JavaFile>();
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

            var javaFile = await ConvertToJavaAsync(cobolFile, cobolAnalysis);
            javaFiles.Add(javaFile);

            processedCount++;
            progressCallback?.Invoke(processedCount, cobolFiles.Count);
        }

        Logger.LogInformation("Completed conversion of {Count} COBOL files to Java", cobolFiles.Count);

        return javaFiles;
    }

    private JavaFile CreateFallbackJavaFile(CobolFile cobolFile, CobolAnalysis cobolAnalysis, string reason)
    {
        var className = NamingHelper.GetFallbackClassName(cobolFile.FileName);
        var packageName = "com.example.cobol";
        var sanitizedReason = reason.Replace("\"", "'");

        var javaCode = $$"""
package {{packageName}};

public class {{className}} {
    /**
     * Placeholder implementation generated because the AI conversion service was unavailable.
     * Original COBOL file: {{cobolFile.FileName}}
     * Reason: {{sanitizedReason}}
     */
    public void run() {
        throw new UnsupportedOperationException("AI conversion unavailable. Please supply valid Azure OpenAI credentials and rerun the migration. Details: {{sanitizedReason}}");
    }
}
""";

        return new JavaFile
        {
            FileName = $"{className}.java",
            PackageName = packageName,
            ClassName = className,
            Content = javaCode,
            OriginalCobolFileName = cobolFile.FileName
        };
    }

    private string ExtractJavaCode(string input)
    {
        // If the input contains markdown code blocks, extract the Java code
        if (input.Contains("```java"))
        {
            var startMarker = "```java";
            var endMarker = "```";

            int startIndex = input.IndexOf(startMarker);
            if (startIndex >= 0)
            {
                startIndex += startMarker.Length;
                int endIndex = input.IndexOf(endMarker, startIndex);

                if (endIndex >= 0)
                {
                    input = input.Substring(startIndex, endIndex - startIndex).Trim();
                }
            }
        }

        // Prefer a balanced class body when a truncated response restarts from scratch.
        var firstPkg = input.IndexOf("package ", StringComparison.Ordinal);
        if (firstPkg >= 0)
        {
            var afterFirstPkg = input.IndexOf('\n', firstPkg) + 1;
            if (afterFirstPkg > 0)
            {
                var secondPkg = input.IndexOf("package ", afterFirstPkg, StringComparison.Ordinal);
                if (secondPkg > 0)
                {
                    var firstBody = input.Substring(firstPkg, secondPkg - firstPkg);
                    var secondBody = input.Substring(secondPkg);
                    bool firstBalanced = firstBody.Count(c => c == '{') == firstBody.Count(c => c == '}');
                    bool secondBalanced = secondBody.Count(c => c == '{') == secondBody.Count(c => c == '}');
                    string keep;
                    if (firstBalanced && !secondBalanced) keep = firstBody;
                    else if (!firstBalanced && secondBalanced) keep = secondBody;
                    else keep = secondBody.Length >= firstBody.Length ? secondBody : firstBody;
                    Logger.LogWarning(
                        "[JavaConverterAgent] Duplicate 'package …;' detected in LLM output (first={FirstLen}c balanced={FirstBal}, second={SecondLen}c balanced={SecondBal}) — keeping the {Pick}.",
                        firstBody.Length, firstBalanced, secondBody.Length, secondBalanced,
                        keep == firstBody ? "first" : "second");
                    input = keep.TrimEnd();
                }
            }
        }

        // Fail loud on unusable output. A silent 0-byte "success" is worse than a
        // file that explains what went wrong, so write a self-documenting stub.
        var hasPkgFinal = input.Contains("package ", StringComparison.Ordinal);
        var hasClassFinal = input.Contains("class ", StringComparison.Ordinal);
        var opensFinal = input.Count(c => c == '{');
        var closesFinal = input.Count(c => c == '}');
        if (!hasPkgFinal || !hasClassFinal || opensFinal != closesFinal)
        {
            Logger.LogWarning(
                "[JavaConverterAgent] OUTPUT APPEARS TRUNCATED: package={HasPkg}, class={HasClass}, braces {Opens}/{Closes}. " +
                "The provider likely hit its output token limit.",
                hasPkgFinal, hasClassFinal, opensFinal, closesFinal);
            EnhancedLogger?.LogBehindTheScenes("TRUNCATION_DETECTED", "WARNING",
                $"package={hasPkgFinal}, class={hasClassFinal}, braces={opensFinal}/{closesFinal}");

            // Only replace essentially unusable output. Truncated-but-mostly-valid
            // code is left alone so the chunked path can still salvage it.
            if (ConversionOutputGuard.ShouldCreateWholeFileStub(
                    input,
                    hasClassFinal,
                    opensFinal,
                    closesFinal))
            {
                var trimmedReason =
                    !hasClassFinal && !hasPkgFinal && opensFinal == 0
                        ? "EMPTY_LLM_RESPONSE — model returned no usable output (often: hit output-token budget, REKT context missing for deps-only programs, or provider rate-limit)."
                    : !hasClassFinal
                        ? "NO_CLASS_KEYWORD — model emitted prose or a non-Java code block."
                    : "BRACE_IMBALANCE — opens=" + opensFinal + " closes=" + closesFinal + ".";

                var stub = new StringBuilder();
                stub.AppendLine("// ════════════════════════════════════════════════════════════════════");
                stub.AppendLine("// ⚠ CONVERSION DID NOT PRODUCE USABLE JAVA");
                stub.AppendLine("// ════════════════════════════════════════════════════════════════════");
                stub.AppendLine("// This file is a placeholder. The model responded but the response did");
                stub.AppendLine("// not contain a usable Java class. The pipeline is NOT pretending this");
                stub.AppendLine("// conversion succeeded — the file is kept so the failure stays visible.");
                stub.AppendLine("//");
                stub.AppendLine("// Reason: " + trimmedReason);
                stub.AppendLine("//");
                stub.AppendLine("// What to do");
                stub.AppendLine("// ──────────");
                stub.AppendLine("// 1. Check the 'Unusable Conversions' table in migration-report.md.");
                stub.AppendLine("//");
                stub.AppendLine("// 2. If a 'NO REKT DATA' warning was logged, this program is deps-only.");
                stub.AppendLine("//    Resolve missing copybooks (see output/rekt/missing-copybooks.txt),");
                stub.AppendLine("//    re-run './doctor.sh rekt-full', then re-convert.");
                stub.AppendLine("//");
                stub.AppendLine("// 3. If the model returned 0 tokens, it likely hit its output-token");
                stub.AppendLine("//    budget. Re-run with chunking or switch to a provider with a");
                stub.AppendLine("//    higher per-call budget.");
                stub.AppendLine("//");
                stub.AppendLine("// 4. See migration-conversation-log.md in this run folder for the raw");
                stub.AppendLine("//    prompt and response.");
                stub.AppendLine("// ════════════════════════════════════════════════════════════════════");
                stub.AppendLine();
                stub.AppendLine("// Original output is preserved below for debugging.");
                stub.AppendLine("/*");
                stub.AppendLine(ConversionOutputGuard.EscapeBlockCommentContent(input));
                stub.AppendLine("*/");
                return stub.ToString();
            }
        }

        return input;
    }

    /// <summary>
    /// Extracts the class name from generated Java code.
    /// </summary>
    private string ExtractClassNameFromCode(string javaCode)
    {
        try
        {
            var lines = javaCode.Split('\n');
            foreach (var line in lines)
            {
                var trimmedLine = line.Trim();
                if (trimmedLine.StartsWith("public class ") || trimmedLine.StartsWith("class "))
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
            Logger.LogWarning(ex, "Error extracting class name from Java code");
        }
        return "ConvertedCobolProgram";
    }

    private string GetPackageName(string javaCode)
    {
        var packageIndex = javaCode.IndexOf("package ");
        if (packageIndex >= 0)
        {
            var start = packageIndex + "package ".Length;
            var end = javaCode.IndexOf(";", start);

            if (end >= 0)
            {
                return javaCode.Substring(start, end - start).Trim();
            }
        }

        return "com.example.cobol";
    }

    /// <summary>
    /// Sanitizes COBOL content to avoid Azure OpenAI content filtering issues.
    /// </summary>
    private string SanitizeCobolContent(string cobolContent)
    {
        if (string.IsNullOrEmpty(cobolContent))
            return cobolContent;

        Logger.LogDebug("Sanitizing COBOL content for content filtering compatibility");

        var sanitizationMap = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)
        {
            {"FEJL", "ERROR_CODE"},
            {"FEJLMELD", "ERROR_MSG"},
            {"FEJL-", "ERROR_"},
            {"FEJLMELD-", "ERROR_MSG_"},
            {"INC-FEJLMELD", "INC-ERROR-MSG"},
            {"FEJL VED KALD", "ERROR IN CALL"},
            {"FEJL VED KALD AF", "ERROR CALLING"},
            {"FEJL VED KALD BDSDATO", "ERROR CALLING BDSDATO"},
            {"KALD", "CALL_OP"},
            {"MEDD-TEKST", "MSG_TEXT"},
        };

        string sanitizedContent = cobolContent;
        bool contentModified = false;

        foreach (var (original, replacement) in sanitizationMap)
        {
            if (sanitizedContent.Contains(original))
            {
                sanitizedContent = sanitizedContent.Replace(original, replacement);
                contentModified = true;
                Logger.LogDebug("Replaced '{Original}' with '{Replacement}' in COBOL content", original, replacement);
            }
        }

        if (contentModified)
        {
            EnhancedLogger?.LogBehindTheScenes("CONTENT_FILTER", "SANITIZATION_APPLIED",
                "Applied content sanitization to avoid Azure OpenAI content filtering");
        }

        return sanitizedContent;
    }
}
