// Keeps structural-context prompt injection consistent across converter agents.

namespace CobolToQuarkusMigration.Helpers;

using System.Text;
using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Helpers.PromptProjections;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using CobolToQuarkusMigration.Agents;

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
        var repoRoot = AppContext.BaseDirectory;
        var d = new DirectoryInfo(repoRoot);
        while (d != null && !File.Exists(Path.Combine(d.FullName, "doctor.sh"))) d = d.Parent;
        if (d == null)
        {
            logger?.LogWarning("[RektPromptInjector] Could not find repo root (doctor.sh) walking up from {Base}", repoRoot);
            return;
        }

        var srcFolder = Environment.GetEnvironmentVariable("COBOL_SOURCE_FOLDER") ?? "source";

        // On by default. Measured on a 36-program estate with reverse engineering held constant
        // and only this varying: mean parity rose from 0.859 to 0.957, 28 of 32 scored programs
        // improved, and the three that had been below the conversion threshold all cleared it.
        // Withholding the structural facts the parser already produced, and then scoring the
        // output against those same facts, was never a defensible default.
        //
        // Set ENABLE_REKT_CONTEXT=false to convert without it, which is worth doing when
        // comparing against a previous run that had no REKT data.
        var enabled = IsEnabled(Environment.GetEnvironmentVariable("ENABLE_REKT_CONTEXT"));

        try
        {
            logger?.LogInformation("[RektPromptInjector] Repo root: {Root}, source: {Src}, file: {File}",
                d.FullName, srcFolder, fileName);

            // Assigned before any structural context, and independently of it: a program still
            // needs a namespace when REKT is switched off, and leaving the choice to the model
            // scattered one estate across five unrelated roots.
            var relativePath = ResolveSourceRelativePath(d.FullName, srcFolder, fileName);
            sb.AppendLine();
            sb.AppendLine(PromptLoader.LoadSectionValidated(
                "RektContext", "NamespacePolicy", new Dictionary<string, string>
                {
                    ["ProgramNamespace"] = ConversionNamespacePolicy.ForProgram(targetLanguage, relativePath),
                    ["SharedNamespace"] = ConversionNamespacePolicy.ForSharedTypes(targetLanguage),
                }));

            if (enabled)
            {
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
                        ProgramFacts nonNullFacts = facts;
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
                            rektBuilder.Append(SyntheticLayoutNotice.Build(
                                StubWarningsFor(sc.Context.CopybookUsage, d.FullName, srcFolder)));
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
                                "[RektPromptInjector] NO REKT DATA for {File} — sections={Sec}, calls={Call}, data={Data}, copybooks={Cpy}. " +
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
                        logger?.LogWarning(
                            "[RektPromptInjector] REKT injection FAILED for {File}: {Msg}. Conversion will proceed without structural facts.",
                            fileName, ex.Message);
                    }
                }
            }
            else
            {
                logger?.LogInformation(
                    "[RektPromptInjector] REKT injection turned off by ENABLE_REKT_CONTEXT=false. "
                    + "Conversion will use reverse-engineering context only; measured parity is "
                    + "about ten points lower without the structural facts.");
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
                logger?.LogWarning("[RektPromptInjector] Shared-types injection failed for {File}: {Msg}", fileName, ex.Message);
            }

            try
            {
                var callTargets = CallTargetRegistryHolder.GetOrBuild(d.FullName, srcFolder);
                var stem = Path.GetFileNameWithoutExtension(Path.GetFileName(fileName));
                var contractBlock = callTargets.ToPromptBlock(stem, targetLanguage);
                if (!string.IsNullOrEmpty(contractBlock))
                {
                    sb.Append(contractBlock);
                    logger?.LogInformation(
                        "[RektPromptInjector] Injected call-target contracts for {File} ({Count} known targets)",
                        fileName, callTargets.Contracts.Count);
                }
            }
            catch (Exception ex)
            {
                logger?.LogWarning(
                    "[RektPromptInjector] Call-target contract injection failed for {File}: {Msg}",
                    fileName, ex.Message);
            }
        }
        catch (Exception ex)
        {
            logger?.LogDebug("[RektPromptInjector] Could not locate repo root for {File}: {Msg}", fileName, ex.Message);
        }
    }
    /// <summary>
    /// Structural context is injected unless it is explicitly switched off. Only the literal
    /// "false" disables it: an unset, empty or unrecognised value leaves it on, so a typo in the
    /// configuration cannot quietly return a run to the weaker behaviour.
    /// </summary>
    internal static bool IsEnabled(string? configured) =>
        string.IsNullOrWhiteSpace(configured)
        || !string.Equals(configured.Trim(), "false", StringComparison.OrdinalIgnoreCase);

    /// <summary>
    /// The program's path relative to the source folder, which is what names its service.
    /// </summary>
    /// <remarks>
    /// Callers pass a bare file name, so the path is recovered from the source tree. A basename
    /// shared by two programs cannot be resolved this way; the first match is used, and both
    /// land in the same namespace only if they really do sit in the same folder.
    /// </remarks>
    private static string? ResolveSourceRelativePath(string repoRoot, string sourceFolder, string fileName)
    {
        // Combine, not Join: COBOL_SOURCE_FOLDER may be an absolute path, and an absolute source
        // folder is meant to win outright rather than be appended to the repository root.
        var root = Path.Combine(repoRoot, sourceFolder);
        if (!Directory.Exists(root)) return null;

        var bare = Path.GetFileName(fileName);
        if (string.IsNullOrEmpty(bare)) return null;

        try
        {
            var match = Directory
                .EnumerateFiles(root, bare, SearchOption.AllDirectories)
                .FirstOrDefault(p => !p.Contains(".rekt-staging", StringComparison.OrdinalIgnoreCase)
                                  && !p.Contains(".preprocessed", StringComparison.OrdinalIgnoreCase));

            return match is null
                ? null
                : Path.GetRelativePath(root, match).Replace(Path.DirectorySeparatorChar, '/');
        }
        catch (IOException)
        {
            return null;
        }
        catch (UnauthorizedAccessException)
        {
            return null;
        }
    }

    /// <summary>
    /// Names the copybooks this program uses whose layouts were synthesised rather than parsed.
    /// The facts path reads them from program-facts.json; the raw-AST path has no facts file, so
    /// it recovers the same answer by matching the program's copybook usage against the stub
    /// catalog on disk.
    /// </summary>
    private static IReadOnlyList<string> StubWarningsFor(
        IReadOnlyList<string> copybooksUsed, string repoRoot, string sourceFolder)
    {
        if (copybooksUsed.Count == 0) return Array.Empty<string>();

        var catalog = StubCopybookCatalog.Load(repoRoot, sourceFolder);
        if (catalog.Copybooks.Count == 0) return Array.Empty<string>();

        var stubs = catalog.Copybooks.ToHashSet(StringComparer.OrdinalIgnoreCase);
        return copybooksUsed
            .Select(name => Path.GetFileNameWithoutExtension(name.Trim()))
            .Where(stubs.Contains)
            .Distinct(StringComparer.OrdinalIgnoreCase)
            .Select(name => $"generated-copybook-stub:{name.ToUpperInvariant()}")
            .ToList();
    }
}
