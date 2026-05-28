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

        MetricsSink.CurrentRunId = _runId;

        try
        {
            var systemPrompt = PromptLoader.LoadSection("CSharpConverter", "System");

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

            // =========================================================================================
            // SPEC-DRIVEN CODE GENERATION (MITM HOOK)
            var userPromptBuilder = new StringBuilder();
            userPromptBuilder.AppendLine("Convert the following COBOL program to C# with .NET:");
            userPromptBuilder.AppendLine();
            userPromptBuilder.AppendLine("```cobol");
            userPromptBuilder.AppendLine(sanitizedContent);
            userPromptBuilder.AppendLine("```");

            userPromptBuilder.AppendLine();
            userPromptBuilder.AppendLine("Here is the analysis of the COBOL program:");
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

            // REKT structural context — same opt-in as JavaConverterAgent.
            if (string.Equals(Environment.GetEnvironmentVariable("ENABLE_REKT_CONTEXT"), "true", StringComparison.OrdinalIgnoreCase))
            {
                try
                {
                    var repoRoot = AppContext.BaseDirectory;
                    var d = new DirectoryInfo(repoRoot);
                    while (d != null && !File.Exists(Path.Combine(d.FullName, "doctor.sh"))) d = d.Parent;
                    if (d != null)
                    {
                        var srcFolder = Environment.GetEnvironmentVariable("COBOL_SOURCE_FOLDER") ?? "source";

                        // ── PR4.b: program-facts.json projection (opt-in) ──
                        // Identical pattern to JavaConverterAgent (PR4.a).
                        bool factsInjected = false;
                        int projectionTokens = 0;
                        if (CobolToQuarkusMigration.Helpers.PromptProjections.CSharpConverterProjection.IsEnabled())
                        {
                            var factsDir = Path.Combine(d.FullName, "output", "rekt");
                            var facts = CobolToQuarkusMigration.Helpers.PromptProjections.CSharpConverterProjection.TryLoad(factsDir, cobolFile.FileName);
                            if (facts is not null)
                            {
                                var (projectionBlock, _, projectionHash, wasCacheHit) =
                                    CobolToQuarkusMigration.Helpers.PromptProjections.ProjectionCache.GetOrBuild(
                                        "C#",
                                        facts,
                                        () => CobolToQuarkusMigration.Helpers.PromptProjections.CSharpConverterProjection.BuildPromptBlock(facts),
                                        _runId,
                                        Logger);
                                projectionTokens = TokenHelper.EstimateTokens(projectionBlock);
                                userPromptBuilder.AppendLine();
                                userPromptBuilder.AppendLine(projectionBlock);
                                Logger.LogInformation(
                                    "[CSharpConverterAgent] Injected program-facts projection for {File} (schema={Schema}, confidence={Conf}, warnings={Warn}, hash={Hash}, cacheHit={Hit})",
                                    cobolFile.FileName, facts.SchemaVersion, facts.Confidence, facts.Warnings.Count, projectionHash.Substring(0, 12), wasCacheHit);
                                Logger.LogInformation(
                                    "[CSharpConverterAgent] PROJECTION_METRICS projectionMode=projection file={File} projectionTokens={ProjTok} rawRektTokens=0 reductionPercent=n/a",
                                    cobolFile.FileName, projectionTokens);
                                MetricsSink.Emit(_runId?.ToString(), new
                                {
                                    Agent = "CSharpConverterAgent",
                                    Event = "projection_metrics",
                                    File = cobolFile.FileName,
                                    TargetLanguage = "C#",
                                    ProjectionMode = "projection",
                                    ProjectionTokens = projectionTokens,
                                    RawRektTokens = 0,
                                    ProjectionHash = projectionHash,
                                    ProjectionCacheHit = wasCacheHit,
                                    FactsSchema = facts.SchemaVersion,
                                    FactsConfidence = facts.Confidence,
                                    FactsWarnings = facts.Warnings.Count
                                });
                                factsInjected = true;
                            }
                            else
                            {
                                Logger.LogInformation(
                                    "[CSharpConverterAgent] _USE_PROGRAM_FACTS=true but no facts.json for {File} — falling back to raw-AST path",
                                    cobolFile.FileName);
                            }
                        }

                        // Raw-AST fallback (existing path). Only runs when the facts projection didn't fire.
                        if (!factsInjected)
                        {
                        var fallback = string.Equals(Environment.GetEnvironmentVariable("STRUCTURAL_FALLBACK_TO_AI"), "true", StringComparison.OrdinalIgnoreCase);
                        var provider = new StructuralContextProvider(d.FullName, srcFolder, fallbackToAi: fallback);
                        var sc = await provider.GetAsync(cobolFile.FileName);
                        var hasContext = sc.Context.Sections.Count > 0
                            || sc.Context.CallTargets.Count > 0
                            || sc.Context.CopybookUsage.Count > 0
                            || sc.Context.DataStructure.Count > 0
                            || sc.Context.SqlStatements.Count > 0
                            || sc.Context.TargetPlan != null;
                        if (hasContext)
                        {
                            var rektHeaderLines = new[]
                            {
                                "---",
                                "REKT STRUCTURAL CONTEXT (authoritative — use this as the conversion blueprint):",
                                "",
                                "FACT-LOCKING RULES — read these BEFORE looking at the structural context:",
                                "  • Treat the structural context below as GROUND TRUTH.",
                                "  • Every method you emit must map to a section or paragraph listed in the context.",
                                "  • Every field you emit must map to a data-structure entry in the context.",
                                "  • Never invent new fields, methods, classes, SQL operations, or CALL targets that are not present here.",
                                "  • If a name is unclear from the source, prefer the name in the structural context.",
                                "  • If the structural context shows zero items for a category (e.g. no CALL targets), do NOT generate any.",
                                "",
                                "DATA STRUCTURE → DTO RULES:",
                                "  • For EVERY 01-level data group below, generate a COMPLETE DTO class with ALL fields.",
                                "  • Map PIC X→string, PIC S9V9→decimal, PIC 9 COMP-3→decimal, PIC 9 COMP→int/long.",
                                "  • Preserve original COBOL field names (PascalCase). Do NOT simplify to fewer fields.",
                                "  • If a group has >50 fields, still generate ALL of them.",
                                "",
                                "CALL TARGET → SERVICE INJECTION RULES:",
                                "  • For EVERY CALL target below: generate an interface + constructor-injected field + method call.",
                                "  • Do NOT inline the called program's logic.",
                                "",
                            };
                            var rektBody = RektContextFormatter.ToPromptBlock(sc);
                            var rektBlock = string.Join("\n", rektHeaderLines) + rektBody;
                            var rawRektTokens = TokenHelper.EstimateTokens(rektBlock);
                            userPromptBuilder.AppendLine();
                            foreach (var line in rektHeaderLines) userPromptBuilder.AppendLine(line);
                            userPromptBuilder.AppendLine(rektBody);
                            Logger.LogInformation("[CSharpConverterAgent] Injected REKT context for {File} (provenance={Prov}, confidence={Conf:F2})",
                                cobolFile.FileName, sc.Provenance, sc.Confidence);
                            Logger.LogInformation(
                                "[CSharpConverterAgent] PROJECTION_METRICS projectionMode=raw-rekt file={File} projectionTokens=0 rawRektTokens={RawTok} reductionPercent=n/a",
                                cobolFile.FileName, rawRektTokens);
                            MetricsSink.Emit(_runId?.ToString(), new
                            {
                                Agent = "CSharpConverterAgent",
                                Event = "projection_metrics",
                                File = cobolFile.FileName,
                                TargetLanguage = "C#",
                                ProjectionMode = "raw-rekt",
                                ProjectionTokens = 0,
                                RawRektTokens = rawRektTokens,
                                RektProvenance = sc.Provenance.ToString(),
                                RektConfidence = sc.Confidence
                            });
                        }
                        else
                        {
                            Logger.LogWarning("[CSharpConverterAgent] ⚠️ NO REKT DATA available for {File} (provenance={Prov})",
                                cobolFile.FileName, sc.Provenance);
                            Logger.LogInformation(
                                "[CSharpConverterAgent] PROJECTION_METRICS projectionMode=none file={File} projectionTokens=0 rawRektTokens=0 reductionPercent=n/a",
                                cobolFile.FileName);
                            MetricsSink.Emit(_runId?.ToString(), new
                            {
                                Agent = "CSharpConverterAgent",
                                Event = "projection_metrics",
                                File = cobolFile.FileName,
                                TargetLanguage = "C#",
                                ProjectionMode = "none",
                                ProjectionTokens = 0,
                                RawRektTokens = 0,
                                RektProvenance = sc.Provenance.ToString()
                            });
                        }
                        }   // end raw-AST fallback block

                        // Shared-types registry: prevent duplicate type definitions across files.
                        try
                        {
                            var registry = SharedTypeRegistryHolder.GetOrBuild(d.FullName, srcFolder);
                            var sharedBlock = registry.ToPromptBlock("C#");
                            if (!string.IsNullOrEmpty(sharedBlock))
                            {
                                userPromptBuilder.Append(sharedBlock);
                                Logger.LogInformation("[CSharpConverterAgent] Injected shared-types registry for {File} ({Count} shared names)",
                                    cobolFile.FileName, registry.SharedTypeNames.Count);
                            }
                        }
                        catch (Exception strEx)
                        {
                            Logger.LogDebug("[CSharpConverterAgent] Shared-types registry build failed for {File}: {Msg}", cobolFile.FileName, strEx.Message);
                        }
                    }
                }
                catch (Exception ex)
                {
                    Logger.LogWarning("[CSharpConverterAgent] ⚠️ REKT injection FAILED for {File}: {Msg}", cobolFile.FileName, ex.Message);
                }
            }

            userPromptBuilder.AppendLine();
            userPromptBuilder.AppendLine("IMPORTANT REQUIREMENTS:");
            userPromptBuilder.AppendLine("1. Return ONLY the C# code - NO explanations, NO markdown blocks");
            userPromptBuilder.AppendLine("2. Start with: namespace CobolMigration.Something; (single line)");
            userPromptBuilder.AppendLine("3. Your response must be valid, compilable C# code");

            var userPrompt = userPromptBuilder.ToString();
            
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

            // ── Continuation retry: if the output is truncated, ask the LLM to
            // continue from where it left off. ──
            var hasAnyCode = csharpCode.Contains("{") && (csharpCode.Contains("class ") || csharpCode.Contains("void ") || csharpCode.Contains("public "));
            var maxContinuations = hasAnyCode ? 3 : 0;
            if (!hasAnyCode && !string.IsNullOrWhiteSpace(csharpCode))
            {
                Logger.LogWarning("[CSharpConverterAgent] ⚠️ Response contains no valid C# code — skipping continuation");
            }
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
                var contPrompt = $"Your previous response was truncated mid-output. Here are the LAST 10 lines you generated:\n\n```csharp\n{lastLines}\n```\n\n" +
                    $"Continue from EXACTLY where you left off. Return ONLY the remaining C# code — no namespace, no using statements, no class declaration. " +
                    $"Start with the next line after the fragment above and end with the final closing brace '}}' of the class.";

                var (contCode, contFallback, _) = await ExecuteWithFallbackAsync(
                    systemPrompt, contPrompt, $"{cobolFile.FileName} [continuation-{cont + 1}]");

                if (contFallback || string.IsNullOrWhiteSpace(contCode)) break;

                contCode = ExtractCSharpCode(contCode);
                var contLines = contCode.Split('\n')
                    .SkipWhile(l => l.TrimStart().StartsWith("using ") || l.TrimStart().StartsWith("namespace ") || l.Trim() == "")
                    .ToList();
                var classIdx = contLines.FindIndex(l => l.Contains("class ") && l.Contains("{"));
                if (classIdx >= 0 && classIdx < 3) contLines = contLines.Skip(classIdx + 1).ToList();

                csharpCode = csharpCode.TrimEnd() + "\n" + string.Join("\n", contLines);
                Logger.LogInformation("[CSharpConverterAgent] Continuation {Cont} appended {Lines} lines",
                    cont + 1, contLines.Count);
            }

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
        if (input.Contains("```csharp") || input.Contains("```c#"))
        {
            var startMarker = input.Contains("```csharp") ? "```csharp" : "```c#";
            var endMarker = "```";
            int startIndex = input.IndexOf(startMarker);
            if (startIndex >= 0)
            {
                startIndex += startMarker.Length;
                int endIndex = input.IndexOf(endMarker, startIndex);
                if (endIndex >= 0)
                    input = input.Substring(startIndex, endIndex - startIndex).Trim();
            }
        }

        // Defensive: detect duplicate `namespace …;` declarations from the LLM
        // emitting two complete bodies (token-limit restart). Keep the one with
        // balanced braces / the longer (complete) body.
        var firstNs = input.IndexOf("namespace ", StringComparison.Ordinal);
        if (firstNs >= 0)
        {
            var afterFirstNs = input.IndexOf('\n', firstNs) + 1;
            if (afterFirstNs > 0)
            {
                var secondNs = input.IndexOf("namespace ", afterFirstNs, StringComparison.Ordinal);
                if (secondNs > 0)
                {
                    var firstBody = input.Substring(firstNs, secondNs - firstNs);
                    var secondBody = input.Substring(secondNs);
                    bool firstBalanced = firstBody.Count(c => c == '{') == firstBody.Count(c => c == '}');
                    bool secondBalanced = secondBody.Count(c => c == '{') == secondBody.Count(c => c == '}');
                    string keep;
                    if (firstBalanced && !secondBalanced) keep = firstBody;
                    else if (!firstBalanced && secondBalanced) keep = secondBody;
                    else keep = secondBody.Length >= firstBody.Length ? secondBody : firstBody;
                    Logger.LogWarning(
                        "[CSharpConverterAgent] Duplicate 'namespace …;' detected in LLM output — keeping the {Pick} body.",
                        keep == firstBody ? "first" : "second");
                    input = keep.TrimEnd();
                }
            }
        }

        // ── Truncation detection ──
        var hasNs = input.Contains("namespace ", StringComparison.Ordinal);
        var hasClass = input.Contains("class ", StringComparison.Ordinal);
        var opens = input.Count(c => c == '{');
        var closes = input.Count(c => c == '}');
        if (!hasNs || !hasClass || opens != closes)
        {
            Logger.LogWarning(
                "[CSharpConverterAgent] ⚠️ OUTPUT APPEARS TRUNCATED: namespace={HasNs}, class={HasClass}, braces {Opens}/{Closes}. " +
                "The provider likely hit its output token limit. Re-run with chunking or switch to Azure OpenAI.",
                hasNs, hasClass, opens, closes);
            EnhancedLogger?.LogBehindTheScenes("TRUNCATION_DETECTED", "WARNING",
                $"namespace={hasNs}, class={hasClass}, braces={opens}/{closes}");
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
