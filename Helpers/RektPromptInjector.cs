// RektPromptInjector.cs — shared helper used by JavaConverter, CSharpConverter,
// ChunkAwareJavaConverter, and ChunkAwareCSharpConverter to inject the REKT
// structural context block + shared-types registry into per-program prompts.
//
// Centralised here so the four converter agents stay consistent when the
// injection contract evolves.

namespace CobolToQuarkusMigration.Helpers;

using System.Text;
using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Helpers.PromptProjections;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;

public static class RektPromptInjector
{
    /// <summary>
    /// Append the REKT structural context (when ENABLE_REKT_CONTEXT=true) and
    /// the shared-types registry block (always, when ≥1 shared copybook) to
    /// the supplied prompt builder.
    ///
    /// When _USE_PROGRAM_FACTS=true AND a matching <code>.facts.json</code>
    /// exists in <code>output/rekt/</code>, the PR4 projection block is
    /// preferred over raw REKT — same behaviour as JavaConverterAgent's
    /// inline path.
    ///
    /// Safe to call from any converter agent. Fail-soft: any I/O exception
    /// is logged and the prompt is returned unchanged.
    /// </summary>
    /// <param name="sb">prompt builder to append to.</param>
    /// <param name="targetLanguage">"Java" or "C#" — drives wording AND projection selection.</param>
    /// <param name="fileName">COBOL file being converted, e.g. "ACCTMGR.cbl".</param>
    /// <param name="agentName">caller agent name (for metrics attribution).</param>
    /// <param name="runId">migration run id (for MetricsSink filename); null disables metrics emission.</param>
    /// <param name="logger">optional logger for visibility.</param>
    public static async Task InjectAsync(
        StringBuilder sb,
        string targetLanguage,
        string fileName,
        string agentName,
        int? runId,
        ILogger? logger = null)
    {
        var enabled = string.Equals(
            Environment.GetEnvironmentVariable("ENABLE_REKT_CONTEXT"),
            "true",
            StringComparison.OrdinalIgnoreCase);
        if (!enabled)
        {
            logger?.LogInformation("[RektPromptInjector] REKT injection DISABLED (ENABLE_REKT_CONTEXT={Val})",
                Environment.GetEnvironmentVariable("ENABLE_REKT_CONTEXT") ?? "(null)");
            return;
        }

        try
        {
            var repoRoot = AppContext.BaseDirectory;
            var d = new DirectoryInfo(repoRoot);
            while (d != null && !File.Exists(Path.Combine(d.FullName, "doctor.sh"))) d = d.Parent;
            if (d == null)
            {
                logger?.LogWarning("[RektPromptInjector] Could not find repo root (doctor.sh) walking up from {Base}", repoRoot);
                return;
            }

            logger?.LogInformation("[RektPromptInjector] Repo root: {Root}, source: {Src}, file: {File}",
                d.FullName, Environment.GetEnvironmentVariable("COBOL_SOURCE_FOLDER") ?? "source", fileName);

            var srcFolder = Environment.GetEnvironmentVariable("COBOL_SOURCE_FOLDER") ?? "source";
            var fallback = string.Equals(
                Environment.GetEnvironmentVariable("STRUCTURAL_FALLBACK_TO_AI"),
                "true",
                StringComparison.OrdinalIgnoreCase);

            // ── PR4: program-facts projection (opt-in, language-aware) ─────
            bool factsInjected = false;
            if (JavaConverterProjection.IsEnabled())
            {
                var factsDir = Path.Combine(d.FullName, "output", "rekt");
                ProgramFacts? facts;
                string projectionBlock;
                if (string.Equals(targetLanguage, "C#", StringComparison.OrdinalIgnoreCase)
                    || string.Equals(targetLanguage, "CSharp", StringComparison.OrdinalIgnoreCase))
                {
                    facts = CSharpConverterProjection.TryLoad(factsDir, fileName);
                    projectionBlock = facts is null ? string.Empty : CSharpConverterProjection.BuildPromptBlock(facts!);
                }
                else
                {
                    facts = JavaConverterProjection.TryLoad(factsDir, fileName);
                    projectionBlock = facts is null ? string.Empty : JavaConverterProjection.BuildPromptBlock(facts!);
                }

                if (facts is not null && !string.IsNullOrEmpty(projectionBlock))
                {
                    ProgramFacts nonNullFacts = facts!;
                    var projectionTokens = TokenHelper.EstimateTokens(projectionBlock);
                    // Stable projection hash: future cache key. Same projection
                    // block (same facts + same language + same template version)
                    // → same hash → eligible for projection-level cache reuse.
                    var projectionHash = CanonicalHasher.HashUtf8(projectionBlock);
                    sb.AppendLine();
                    sb.AppendLine(projectionBlock);
                    factsInjected = true;
                    logger?.LogInformation(
                        "[RektPromptInjector] Injected program-facts projection for {File} (target={Lang}, schema={Schema}, confidence={Conf}, warnings={Warn}, hash={Hash})",
                        fileName, targetLanguage, nonNullFacts.SchemaVersion, nonNullFacts.Confidence, nonNullFacts.Warnings.Count, projectionHash.Substring(0, 12));
                    logger?.LogInformation(
                        "[RektPromptInjector] PROJECTION_METRICS projectionMode=projection agent={Agent} file={File} projectionTokens={ProjTok} rawRektTokens=0 reductionPercent=n/a",
                        agentName, fileName, projectionTokens);
                    MetricsSink.Emit(runId?.ToString(), new
                    {
                        Agent = agentName,
                        Event = "projection_metrics",
                        File = fileName,
                        TargetLanguage = targetLanguage,
                        ProjectionMode = "projection",
                        ProjectionTokens = projectionTokens,
                        RawRektTokens = 0,
                        ProjectionHash = projectionHash,
                        FactsSchema = nonNullFacts.SchemaVersion,
                        FactsConfidence = nonNullFacts.Confidence,
                        FactsWarnings = nonNullFacts.Warnings.Count
                    });
                }
                else
                {
                    logger?.LogInformation(
                        "[RektPromptInjector] _USE_PROGRAM_FACTS=true but no facts.json for {File} (target={Lang}) — falling back to raw-AST path",
                        fileName, targetLanguage);
                }
            }

            // ── REKT structural context (raw-AST fallback) ─────────────────
            if (!factsInjected)
            {
                try
                {
                    var provider = new StructuralContextProvider(d.FullName, srcFolder, fallbackToAi: fallback);
                    var sc = await provider.GetAsync(fileName);
                    var hasContext = sc.Context.Sections.Count > 0
                        || sc.Context.CallTargets.Count > 0
                        || sc.Context.CopybookUsage.Count > 0
                        || sc.Context.DataStructure.Count > 0
                        || sc.Context.SqlStatements.Count > 0
                        || sc.Context.TargetPlan != null;
                    if (hasContext)
                    {
                        var rektBuilder = new StringBuilder();
                        rektBuilder.AppendLine();
                        rektBuilder.AppendLine("---");
                        rektBuilder.AppendLine("REKT STRUCTURAL CONTEXT (authoritative — use this as the conversion blueprint):");
                        rektBuilder.AppendLine();
                        rektBuilder.AppendLine("FACT-LOCKING RULES — read these BEFORE looking at the structural context:");
                        rektBuilder.AppendLine("  • Treat the structural context below as GROUND TRUTH.");
                        rektBuilder.AppendLine("  • Every method you emit must map to a section or paragraph listed in the context.");
                        rektBuilder.AppendLine("  • Every field you emit must map to a data-structure entry in the context.");
                        rektBuilder.AppendLine("  • Never invent new fields, methods, classes, SQL operations, or CALL targets that are not present here.");
                        rektBuilder.AppendLine("  • If a name is unclear from the source, prefer the name in the structural context.");
                        rektBuilder.AppendLine("  • If the structural context shows zero items for a category (e.g. no CALL targets), do NOT generate any.");
                        rektBuilder.AppendLine();
                        rektBuilder.AppendLine("DATA STRUCTURE → DTO RULES:");
                        rektBuilder.AppendLine("  • For EVERY 01-level data group in the DATA STRUCTURE section below, generate a");
                        rektBuilder.AppendLine("    complete DTO/record class with ALL fields — not just the ones referenced in the");
                        rektBuilder.AppendLine("    procedure division. Copybook structures are shared");
                        rektBuilder.AppendLine("    types used by multiple programs — they must be complete.");
                        rektBuilder.AppendLine("  • Map EVERY PIC clause to the correct target type (PIC X→String, PIC S9V9→BigDecimal/decimal,");
                        rektBuilder.AppendLine("    PIC 9 COMP-3→BigDecimal/decimal, PIC 9 COMP→int/long). Do NOT simplify to fewer fields.");
                        rektBuilder.AppendLine("  • Preserve the original COBOL field name as the Java/C# field name (camelCase).");
                        rektBuilder.AppendLine("  • If a group has >50 fields, still generate ALL of them — completeness is more");
                        rektBuilder.AppendLine("    important than brevity.");
                        rektBuilder.AppendLine();
                        rektBuilder.AppendLine("CALL TARGET → SERVICE INJECTION RULES:");
                        rektBuilder.AppendLine("  • For EVERY CALL target in the structural context, generate:");
                        rektBuilder.AppendLine("    - A service interface (e.g. IDateService / IAccountService)");
                        rektBuilder.AppendLine("    - An @Inject/@Autowired field in the main service class");
                        rektBuilder.AppendLine("    - A method call at the point where the COBOL CALL appears");
                        rektBuilder.AppendLine("  • Java: use @Inject (CDI) for the interface field");
                        rektBuilder.AppendLine("  • C#: use constructor injection for the interface");
                        rektBuilder.AppendLine("  • Do NOT inline the called program's logic — it will be converted separately.");
                        rektBuilder.AppendLine();
                        rektBuilder.AppendLine(RektContextFormatter.ToPromptBlock(sc));
                        var rektBlock = rektBuilder.ToString();
                        var rawRektTokens = TokenHelper.EstimateTokens(rektBlock);
                        sb.Append(rektBlock);
                        logger?.LogInformation("[RektPromptInjector] Injected REKT context for {File} (provenance={Prov}, confidence={Conf:F2})",
                            fileName, sc.Provenance, sc.Confidence);
                        logger?.LogInformation(
                            "[RektPromptInjector] PROJECTION_METRICS projectionMode=raw-rekt agent={Agent} file={File} projectionTokens=0 rawRektTokens={RawTok} reductionPercent=n/a",
                            agentName, fileName, rawRektTokens);
                        MetricsSink.Emit(runId?.ToString(), new
                        {
                            Agent = agentName,
                            Event = "projection_metrics",
                            File = fileName,
                            TargetLanguage = targetLanguage,
                            ProjectionMode = "raw-rekt",
                            ProjectionTokens = 0,
                            RawRektTokens = rawRektTokens,
                            RektProvenance = sc.Provenance.ToString(),
                            RektConfidence = sc.Confidence
                        });
                    }
                    else
                    {
                        logger?.LogWarning(
                            "[RektPromptInjector] ⚠️ NO REKT DATA for {File} — sections={Sec}, calls={Call}, data={Data}, copybooks={Cpy}. " +
                            "Run './doctor.sh rekt-full' first to populate output/rekt/. Conversion will proceed without structural facts.",
                            fileName, sc.Context.Sections.Count, sc.Context.CallTargets.Count,
                            sc.Context.DataStructure.Count, sc.Context.CopybookUsage.Count);
                        MetricsSink.Emit(runId?.ToString(), new
                        {
                            Agent = agentName,
                            Event = "projection_metrics",
                            File = fileName,
                            TargetLanguage = targetLanguage,
                            ProjectionMode = "none",
                            ProjectionTokens = 0,
                            RawRektTokens = 0,
                            RektProvenance = sc.Provenance.ToString()
                        });
                    }
                }
                catch (Exception ex)
                {
                    logger?.LogWarning("[RektPromptInjector] ⚠️ REKT injection FAILED for {File}: {Msg}. Conversion will proceed without structural facts.",
                        fileName, ex.Message);
                }
            }

            // ── Shared-types registry ─────────────────────────────────────
            try
            {
                var registry = SharedTypeRegistryHolder.GetOrBuild(d.FullName, srcFolder);
                var sharedBlock = registry.ToPromptBlock(targetLanguage);
                if (!string.IsNullOrEmpty(sharedBlock))
                {
                    sb.Append(sharedBlock);
                    logger?.LogInformation("[RektPromptInjector] Injected shared-types registry for {File} ({Count} shared names)",
                        fileName, registry.SharedTypeNames.Count);
                }
            }
            catch (Exception ex)
            {
                logger?.LogWarning("[RektPromptInjector] ⚠️ Shared-types injection failed for {File}: {Msg}", fileName, ex.Message);
            }
        }
        catch (Exception ex)
        {
            logger?.LogDebug("[RektPromptInjector] Could not locate repo root for {File}: {Msg}", fileName, ex.Message);
        }
    }
}
