// RektPromptInjector.cs — shared helper used by JavaConverter, CSharpConverter,
// ChunkAwareJavaConverter, and ChunkAwareCSharpConverter to inject the REKT
// structural context block + shared-types registry into per-program prompts.
//
// Centralised here so the four converter agents stay consistent when the
// injection contract evolves.

namespace CobolToQuarkusMigration.Helpers;

using System.Text;
using Microsoft.Extensions.Logging;

public static class RektPromptInjector
{
    /// <summary>
    /// Append the REKT structural context (when ENABLE_REKT_CONTEXT=true) and
    /// the shared-types registry block (always, when ≥1 shared copybook) to
    /// the supplied prompt builder. Safe to call from any converter agent.
    /// </summary>
    /// <param name="sb">prompt builder to append to.</param>
    /// <param name="targetLanguage">"Java" or "C#" — drives wording.</param>
    /// <param name="fileName">COBOL file being converted, e.g. "ACCTMGR.cbl".</param>
    /// <param name="logger">optional logger for visibility.</param>
    public static async Task InjectAsync(StringBuilder sb, string targetLanguage, string fileName, ILogger? logger = null)
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

            // ── REKT structural context ───────────────────────────────────
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
                    sb.AppendLine();
                    sb.AppendLine("---");
                    sb.AppendLine("REKT STRUCTURAL CONTEXT (authoritative — use this as the conversion blueprint):");
                    sb.AppendLine();
                    sb.AppendLine("FACT-LOCKING RULES — read these BEFORE looking at the structural context:");
                    sb.AppendLine("  • Treat the structural context below as GROUND TRUTH.");
                    sb.AppendLine("  • Every method you emit must map to a section or paragraph listed in the context.");
                    sb.AppendLine("  • Every field you emit must map to a data-structure entry in the context.");
                    sb.AppendLine("  • Never invent new fields, methods, classes, SQL operations, or CALL targets that are not present here.");
                    sb.AppendLine("  • If a name is unclear from the source, prefer the name in the structural context.");
                    sb.AppendLine("  • If the structural context shows zero items for a category (e.g. no CALL targets), do NOT generate any.");
                    sb.AppendLine();
                    sb.AppendLine("DATA STRUCTURE → DTO RULES:");
                    sb.AppendLine("  • For EVERY 01-level data group in the DATA STRUCTURE section below, generate a");
                    sb.AppendLine("    complete DTO/record class with ALL fields — not just the ones referenced in the");
                    sb.AppendLine("    procedure division. Copybook structures are shared");
                    sb.AppendLine("    types used by multiple programs — they must be complete.");
                    sb.AppendLine("  • Map EVERY PIC clause to the correct target type (PIC X→String, PIC S9V9→BigDecimal/decimal,");
                    sb.AppendLine("    PIC 9 COMP-3→BigDecimal/decimal, PIC 9 COMP→int/long). Do NOT simplify to fewer fields.");
                    sb.AppendLine("  • Preserve the original COBOL field name as the Java/C# field name (camelCase).");
                    sb.AppendLine("  • If a group has >50 fields, still generate ALL of them — completeness is more");
                    sb.AppendLine("    important than brevity.");
                    sb.AppendLine();
                    sb.AppendLine("CALL TARGET → SERVICE INJECTION RULES:");
                    sb.AppendLine("  • For EVERY CALL target in the structural context, generate:");
                    sb.AppendLine("    - A service interface (e.g. IDateService / IAccountService)");
                    sb.AppendLine("    - An @Inject/@Autowired field in the main service class");
                    sb.AppendLine("    - A method call at the point where the COBOL CALL appears");
                    sb.AppendLine("  • Java: use @Inject (CDI) for the interface field");
                    sb.AppendLine("  • C#: use constructor injection for the interface");
                    sb.AppendLine("  • Do NOT inline the called program's logic — it will be converted separately.");
                    sb.AppendLine();
                    sb.AppendLine(RektContextFormatter.ToPromptBlock(sc));
                    logger?.LogInformation("[RektPromptInjector] Injected REKT context for {File} (provenance={Prov}, confidence={Conf:F2})",
                        fileName, sc.Provenance, sc.Confidence);
                }
                else
                {
                    logger?.LogWarning(
                        "[RektPromptInjector] ⚠️ NO REKT DATA for {File} — sections={Sec}, calls={Call}, data={Data}, copybooks={Cpy}. " +
                        "Run './doctor.sh rekt-full' first to populate output/rekt/. Conversion will proceed without structural facts.",
                        fileName, sc.Context.Sections.Count, sc.Context.CallTargets.Count,
                        sc.Context.DataStructure.Count, sc.Context.CopybookUsage.Count);
                }
            }
            catch (Exception ex)
            {
                logger?.LogWarning("[RektPromptInjector] ⚠️ REKT injection FAILED for {File}: {Msg}. Conversion will proceed without structural facts.",
                    fileName, ex.Message);
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
