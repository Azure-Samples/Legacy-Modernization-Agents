// Keeps structural-context prompt injection consistent across converter agents.

namespace CobolToQuarkusMigration.Helpers;

using System.Text;
using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Helpers.PromptProjections;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;

public static class RektPromptInjector
{
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

            bool factsInjected = false;
            if (JavaConverterProjection.IsEnabled())
            {
                var factsDir = Path.Combine(d.FullName, "output", "rekt");
                ProgramFacts? facts;
                Func<string>? builder = null;
                if (string.Equals(targetLanguage, "C#", StringComparison.OrdinalIgnoreCase)
                    || string.Equals(targetLanguage, "CSharp", StringComparison.OrdinalIgnoreCase))
                {
                    facts = CSharpConverterProjection.TryLoad(factsDir, fileName);
                    if (facts is not null)
                    {
                        var capturedFacts = facts;
                        builder = () => CSharpConverterProjection.BuildPromptBlock(capturedFacts);
                    }
                }
                else
                {
                    facts = JavaConverterProjection.TryLoad(factsDir, fileName);
                    if (facts is not null)
                    {
                        var capturedFacts = facts;
                        builder = () => JavaConverterProjection.BuildPromptBlock(capturedFacts);
                    }
                }

                if (facts is not null && builder is not null)
                {
                    ProgramFacts nonNullFacts = facts!;
                    var (projectionBlock, _, projectionHash, wasCacheHit) =
                        ProjectionCache.GetOrBuild(targetLanguage, nonNullFacts, builder, runId, logger);
                    var projectionTokens = TokenHelper.EstimateTokens(projectionBlock);
                    sb.AppendLine();
                    sb.AppendLine(projectionBlock);
                    factsInjected = true;
                    logger?.LogInformation(
                        "[RektPromptInjector] Injected program-facts projection for {File} (target={Lang}, schema={Schema}, confidence={Conf}, warnings={Warn}, hash={Hash}, cacheHit={Hit})",
                        fileName, targetLanguage, nonNullFacts.SchemaVersion, nonNullFacts.Confidence, nonNullFacts.Warnings.Count, projectionHash.Substring(0, 12), wasCacheHit);
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
                        ProjectionCacheHit = wasCacheHit,
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
                        rektBuilder.AppendLine(PromptLoader.LoadSectionValidated(
                            "RektContext", "CommonPolicy", new Dictionary<string, string>
                            {
                                ["SourceMetadata"] = string.Empty
                            }));
                        rektBuilder.AppendLine(PromptLoader.LoadSectionValidated(
                            "RektContext", "RawTargetPolicy", new Dictionary<string, string>()));
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
