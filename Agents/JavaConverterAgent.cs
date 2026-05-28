using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Agents.Infrastructure;
using CobolToQuarkusMigration.Agents.Infrastructure.Caching;
using CobolToQuarkusMigration.Agents.Interfaces;
using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Helpers.PromptProjections;
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

        // Publish runId into AsyncLocal so infrastructure (CopilotChatClient,
        // RektPromptInjector, retry helpers) can emit metrics without us
        // threading the id through every call.
        MetricsSink.CurrentRunId = _runId;

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

            // Inject REKT structural context when available — this gives the LLM
            // an authoritative section/paragraph/call/SQL/data layout instead of
            // forcing it to re-derive structure from source. Opt-in via env var.
            //
            // Two sources, in priority order:
            //   1. PR4: program-facts.json projection (curated, schema-versioned).
            //      Enabled by _USE_PROGRAM_FACTS=true AND a *.facts.json present
            //      under output/rekt/. Replaces the raw-AST path entirely for
            //      this program — no double-injection.
            //   2. Raw-AST fallback via RektContextLoader (existing path).
            //      Used when (1) is not active for this program.
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

                        // ── PR4: program-facts.json projection (opt-in) ──
                        bool factsInjected = false;
                        int projectionTokens = 0;
                        if (JavaConverterProjection.IsEnabled())
                        {
                            var factsDir = Path.Combine(d.FullName, "output", "rekt");
                            var facts = JavaConverterProjection.TryLoad(factsDir, cobolFile.FileName);
                            if (facts is not null)
                            {
                                var (projectionBlock, _, projectionHash, wasCacheHit) =
                                    ProjectionCache.GetOrBuild(
                                        "Java",
                                        facts,
                                        () => JavaConverterProjection.BuildPromptBlock(facts),
                                        _runId,
                                        Logger);
                                projectionTokens = TokenHelper.EstimateTokens(projectionBlock);
                                userPromptBuilder.AppendLine();
                                userPromptBuilder.AppendLine(projectionBlock);
                                Logger.LogInformation(
                                    "[JavaConverterAgent] Injected program-facts projection for {File} (schema={Schema}, confidence={Conf}, warnings={Warn}, hash={Hash}, cacheHit={Hit})",
                                    cobolFile.FileName, facts.SchemaVersion, facts.Confidence, facts.Warnings.Count, projectionHash.Substring(0, 12), wasCacheHit);
                                // Structured projection metrics — parsed by ab-projection.sh
                                Logger.LogInformation(
                                    "[JavaConverterAgent] PROJECTION_METRICS projectionMode=projection file={File} projectionTokens={ProjTok} rawRektTokens=0 reductionPercent=n/a",
                                    cobolFile.FileName, projectionTokens);
                                // Logger-independent metrics — survives process buffer drops.
                                MetricsSink.Emit(_runId?.ToString(), new
                                {
                                    Agent = "JavaConverterAgent",
                                    Event = "projection_metrics",
                                    File = cobolFile.FileName,
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
                                    "[JavaConverterAgent] _USE_PROGRAM_FACTS=true but no facts.json for {File} — falling back to raw-AST path",
                                    cobolFile.FileName);
                            }
                        }

                        // Raw-AST fallback (existing path). Only runs when the facts projection didn't fire.
                        if (!factsInjected)
                        {
                        var fallback = string.Equals(Environment.GetEnvironmentVariable("STRUCTURAL_FALLBACK_TO_AI"), "true", StringComparison.OrdinalIgnoreCase);
                        var provider = new StructuralContextProvider(d.FullName, srcFolder, fallbackToAi: fallback);
                        var sc = await provider.GetAsync(cobolFile.FileName);
                        // Inject whenever ANY useful context is available — even
                        // a bare target plan or copybook list adds value.
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
                                "  • Map PIC X→String, PIC S9V9→BigDecimal, PIC 9 COMP-3→BigDecimal, PIC 9 COMP→int/long.",
                                "  • Preserve original COBOL field names (camelCase). Do NOT simplify to fewer fields.",
                                "  • If a group has >50 fields, still generate ALL of them.",
                                "",
                                "CALL TARGET → SERVICE INJECTION RULES:",
                                "  • For EVERY CALL target below: generate an interface + @Inject field + method call.",
                                "  • Do NOT inline the called program's logic.",
                                "",
                            };
                            var rektBody = RektContextFormatter.ToPromptBlock(sc);
                            var rektBlock = string.Join("\n", rektHeaderLines) + rektBody;
                            var rawRektTokens = TokenHelper.EstimateTokens(rektBlock);
                            userPromptBuilder.AppendLine();
                            foreach (var line in rektHeaderLines) userPromptBuilder.AppendLine(line);
                            userPromptBuilder.AppendLine(rektBody);
                            Logger.LogInformation("[JavaConverterAgent] Injected REKT context for {File} (provenance={Prov}, confidence={Conf:F2})",
                                cobolFile.FileName, sc.Provenance, sc.Confidence);
                            // Structured projection metrics — parsed by ab-projection.sh
                            Logger.LogInformation(
                                "[JavaConverterAgent] PROJECTION_METRICS projectionMode=raw-rekt file={File} projectionTokens=0 rawRektTokens={RawTok} reductionPercent=n/a",
                                cobolFile.FileName, rawRektTokens);
                            // Logger-independent metrics — survives process buffer drops.
                            MetricsSink.Emit(_runId?.ToString(), new
                            {
                                Agent = "JavaConverterAgent",
                                Event = "projection_metrics",
                                File = cobolFile.FileName,
                                ProjectionMode = "raw-rekt",
                                ProjectionTokens = 0,
                                RawRektTokens = rawRektTokens,
                                RektProvenance = sc.Provenance.ToString(),
                                RektConfidence = sc.Confidence
                            });
                        }
                        else
                        {
                            Logger.LogWarning("[JavaConverterAgent] ⚠️ NO REKT DATA available for {File} (provenance={Prov})",
                                cobolFile.FileName, sc.Provenance);
                            Logger.LogInformation(
                                "[JavaConverterAgent] PROJECTION_METRICS projectionMode=none file={File} projectionTokens=0 rawRektTokens=0 reductionPercent=n/a",
                                cobolFile.FileName);
                            MetricsSink.Emit(_runId?.ToString(), new
                            {
                                Agent = "JavaConverterAgent",
                                Event = "projection_metrics",
                                File = cobolFile.FileName,
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
                            var sharedBlock = registry.ToPromptBlock("Java");
                            if (!string.IsNullOrEmpty(sharedBlock))
                            {
                                userPromptBuilder.Append(sharedBlock);
                                Logger.LogInformation("[JavaConverterAgent] Injected shared-types registry for {File} ({Count} shared names)",
                                    cobolFile.FileName, registry.SharedTypeNames.Count);
                            }
                        }
                        catch (Exception strEx)
                        {
                            Logger.LogDebug("[JavaConverterAgent] Shared-types registry build failed for {File}: {Msg}", cobolFile.FileName, strEx.Message);
                        }
                    }
                }
                catch (Exception ex)
                {
                    Logger.LogWarning("[JavaConverterAgent] ⚠️ REKT injection FAILED for {File}: {Msg}", cobolFile.FileName, ex.Message);
                }
            }

            userPromptBuilder.AppendLine();
            userPromptBuilder.AppendLine("IMPORTANT REQUIREMENTS:");
            userPromptBuilder.AppendLine("1. Return ONLY the Java code - NO explanations, NO markdown blocks, NO additional text");
            userPromptBuilder.AppendLine("2. Start with: package com.example.something; (single line, lowercase, no comments)");
            userPromptBuilder.AppendLine("3. Do NOT include newlines or explanatory text in the package declaration");
            userPromptBuilder.AppendLine("4. Your response must be valid, compilable Java code starting with 'package' and ending with the class closing brace");
            
            userPromptBuilder.AppendLine();
            userPromptBuilder.AppendLine("Note: The original code contains Danish error handling terms replaced with placeholders.");

            var userPrompt = userPromptBuilder.ToString();

            // ── P1 response cache (opt-in via _LLM_CACHE_ENABLED=true) ──
            // Cache the FINAL Java code (post-continuation, post-extraction) keyed
            // on every input that shaped the prompt. Caching at the outermost
            // boundary means only structurally complete, validated code lands in
            // the cache — partial / truncated responses are impossible to poison
            // with because they never reach the store. See
            // docs/p1-response-cache.md for the invalidation matrix.
            var cache = LlmCacheGate.EnsureCache(Logger);
            var cacheEnabled = LlmCacheGate.Enabled && cache is not null && ResponsesClient is not null;
            CacheKey? cacheKey = null;
            string? cacheHitJava = null;
            int cachedMaxTokens = 0;
            string cachedReasoning = "";
            string rektContextForKey = ExtractRektContextBlock(userPrompt);
            if (cacheEnabled)
            {
                // ResponsesClient is non-null when cacheEnabled is true (guard above).
                // CalculateTokenSettings is deterministic on the prompts, so we can
                // compute the generation settings hash without actually calling the
                // model. This guarantees the key is stable across runs.
                (cachedMaxTokens, cachedReasoning) =
                    ResponsesClient!.CalculateTokenSettings(systemPrompt, userPrompt);
                cacheKey = JavaConverterCacheKeys.ForConversion(
                    systemPrompt: systemPrompt,
                    userPrompt: userPrompt,
                    preprocessedSourceBytes: sanitizedContent,
                    rektContextBlock: rektContextForKey,
                    model: ModelId,
                    maxOutputTokens: cachedMaxTokens,
                    reasoningEffort: cachedReasoning,
                    cobolFile: cobolFile);

                var lookup = await cache!.TryGetAsync(cacheKey);
                if (lookup.IsHit)
                {
                    cacheHitJava = lookup.Entry!.ResponseText;
                    Logger.LogInformation(
                        "[JavaConverterAgent] Cache HIT for {File} (age={Age:F0}s, hits={Hits}, key={KeyShort}). Skipping LLM call.",
                        cobolFile.FileName, lookup.Entry.Age.TotalSeconds, lookup.Entry.HitCount,
                        lookup.Entry.KeyHash[..Math.Min(12, lookup.Entry.KeyHash.Length)]);
                }
            }

            string javaCode;
            bool cameFromCache = cacheHitJava is not null;
            bool usedFallback = false;
            string? fallbackReason = null;

            if (cameFromCache)
            {
                javaCode = cacheHitJava!;
            }
            else
            {
                (javaCode, usedFallback, fallbackReason) = await ExecuteWithFallbackAsync(
                    systemPrompt,
                    userPrompt,
                    cobolFile.FileName);

                if (usedFallback)
                {
                    return CreateFallbackJavaFile(cobolFile, cobolAnalysis, fallbackReason ?? "Unknown error");
                }
            }

            stopwatch.Stop();
            EnhancedLogger?.LogBehindTheScenes("AI_PROCESSING", "JAVA_CONVERSION_COMPLETE",
                $"Completed Java conversion of {cobolFile.FileName} in {stopwatch.ElapsedMilliseconds}ms");

            // Extract the Java code from markdown code blocks if necessary.
            // Cached responses have already been through this — skip when from cache.
            if (!cameFromCache)
            {
                javaCode = ExtractJavaCode(javaCode);
            }

            // ── Continuation retry: if the output is truncated, ask the LLM to
            // continue from where it left off. Up to 3 continuations to reconstruct
            // the full file without needing chunking. ──
            // Skipped entirely on cache hit — the cached value already passed
            // the completeness check at store time, so additional LLM calls
            // would only re-introduce non-determinism.
            var hasAnyCode = javaCode.Contains("{") && (javaCode.Contains("class ") || javaCode.Contains("void ") || javaCode.Contains("public "));
            var maxContinuations = (cameFromCache || !hasAnyCode) ? 0 : 3;
            if (!cameFromCache && !hasAnyCode && !string.IsNullOrWhiteSpace(javaCode))
            {
                Logger.LogWarning("[JavaConverterAgent] ⚠️ Response contains no valid Java code — skipping continuation (response may be an error message or empty)");
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
                // Strip any duplicate package/import/class declarations the LLM may re-emit
                var contLines = contCode.Split('\n')
                    .SkipWhile(l => l.TrimStart().StartsWith("package ") || l.TrimStart().StartsWith("import ") || l.Trim() == "")
                    .ToList();
                // Remove class re-declaration if present
                var classIdx = contLines.FindIndex(l => l.Contains("class ") && l.Contains("{"));
                if (classIdx >= 0 && classIdx < 3) contLines = contLines.Skip(classIdx + 1).ToList();

                javaCode = javaCode.TrimEnd() + "\n" + string.Join("\n", contLines);
                Logger.LogInformation("[JavaConverterAgent] Continuation {Cont} appended {Lines} lines",
                    cont + 1, contLines.Count);
            }

            // ── P1 cache store: persist the final, validated Java code so future
            // runs with the same inputs skip the LLM round-trip entirely. We only
            // cache when (a) the response actually came from the LLM (not from
            // cache itself), (b) the conversion structurally passed our own
            // validity checks (matching braces, package+class present). Storing
            // post-validation guarantees the cache cannot serve truncated or
            // poisoned output to a subsequent run.
            if (cacheEnabled && cacheKey is not null && !cameFromCache)
            {
                if (JavaConverterCacheKeys.IsCacheableJava(javaCode))
                {
                    try
                    {
                        await cache!.PutAsync(cacheKey, javaCode);
                    }
                    catch (Exception storeEx)
                    {
                        // Cache failures must never break a conversion. Log and continue.
                        Logger.LogWarning(storeEx,
                            "[JavaConverterAgent] Failed to store conversion in cache for {File}; continuing uncached.",
                            cobolFile.FileName);
                    }
                }
                else
                {
                    Logger.LogInformation(
                        "[{Event}] runId={RunId} correlationId={CorrelationId} provider={Provider} model={Model} " +
                        "decision=skip-store missReason=UpstreamNotCacheable reason=java-structurally-incomplete " +
                        "basename={Basename}",
                        SqliteResponseCache.LogEventName, LlmCorrelationContext.RunId, LlmCorrelationContext.CorrelationId,
                        JavaConverterCacheKeys.Provider, ModelId, cobolFile.FileName);
                }
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

    /// <summary>
    /// Extracts the REKT structural-context block from an assembled user prompt,
    /// so the response cache key can hash exactly the REKT text that influenced
    /// the LLM call (rather than the raw REKT files on disk, which include data
    /// the prompt doesn't see). Returns the empty string when no REKT block is
    /// present (cache key encodes "no REKT" distinctly from "REKT was empty").
    /// </summary>
    private static string ExtractRektContextBlock(string userPrompt)
    {
        const string startMarker = "REKT STRUCTURAL CONTEXT (authoritative";
        const string endMarker = "IMPORTANT REQUIREMENTS:";
        var startIdx = userPrompt.IndexOf(startMarker, StringComparison.Ordinal);
        if (startIdx < 0) return string.Empty;
        var endIdx = userPrompt.IndexOf(endMarker, startIdx, StringComparison.Ordinal);
        return endIdx < 0
            ? userPrompt.Substring(startIdx)
            : userPrompt.Substring(startIdx, endIdx - startIdx);
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

        // Defensive: some LLM responses emit two complete class bodies for the
        // same program — typically when the model hits an internal token limit
        // mid-output and "restarts" from scratch. Pick the body with balanced
        // braces (preferring the longer/complete one), not blindly the first.
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

        // ── Truncation detection ──
        // If the output has no 'package' declaration OR unbalanced braces, it was
        // truncated by the provider (Copilot drops responses silently). Log a clear
        // warning so the user knows to re-run or switch provider.
        var hasPkg = input.Contains("package ", StringComparison.Ordinal);
        var hasClass = input.Contains("class ", StringComparison.Ordinal);
        var opens = input.Count(c => c == '{');
        var closes = input.Count(c => c == '}');
        if (!hasPkg || !hasClass || opens != closes)
        {
            Logger.LogWarning(
                "[JavaConverterAgent] ⚠️ OUTPUT APPEARS TRUNCATED: package={HasPkg}, class={HasClass}, braces {Opens}/{Closes}. " +
                "The provider likely hit its output token limit. Re-run with chunking (lower --copilot-safe thresholds) or switch to Azure OpenAI.",
                hasPkg, hasClass, opens, closes);
            EnhancedLogger?.LogBehindTheScenes("TRUNCATION_DETECTED", "WARNING",
                $"package={hasPkg}, class={hasClass}, braces={opens}/{closes}");
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
            {"ERROR CALLING DATECONV", "ERROR CALLING DATE SERVICE"},
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
