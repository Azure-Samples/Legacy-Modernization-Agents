using System.Text;
using System.Text.Json;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;

namespace CobolToQuarkusMigration.Helpers.PromptProjections;

/// <summary>
/// PR4.b — C# / .NET counterpart to <see cref="JavaConverterProjection"/>.
/// </summary>
/// <remarks>
/// <para>
/// Mirrors the Java projection's shape (same marker, same fact-locking rules,
/// same "(none)" empty markers) so the cache-key extractor in the C# converter
/// agent picks up the projection content automatically. Only the language-
/// specific rule sections differ:
/// </para>
/// <list type="bullet">
///   <item>DTO rules → PIC X → <c>string</c>, COMP-3 → <c>decimal</c>, PascalCase.</item>
///   <item>CALL rules → constructor-injected fields (not <c>@Inject</c>).</item>
/// </list>
/// <para>
/// Backed by the same <see cref="ProgramFacts"/> contract — PR3 wrote the
/// schema once; both Java and C# consume it identically.
/// </para>
/// </remarks>
public static class CSharpConverterProjection
{
    /// <summary>
    /// Shares <see cref="JavaConverterProjection.EnableEnvVar"/> intentionally:
    /// one env var enables facts-based prompting across all converter agents
    /// so a user opts into the new behaviour once.
    /// </summary>
    public const string EnableEnvVar = JavaConverterProjection.EnableEnvVar;

    public static bool IsEnabled() => JavaConverterProjection.IsEnabled();

    /// <summary>Reuses <see cref="JavaConverterProjection.TryLoad"/> — same schema, same file location.</summary>
    public static ProgramFacts? TryLoad(string factsDir, string programBasename) =>
        JavaConverterProjection.TryLoad(factsDir, programBasename);

    public static string BuildPromptBlock(ProgramFacts facts)
    {
        var sb = new StringBuilder();
        sb.AppendLine("---");
        sb.AppendLine("REKT STRUCTURAL CONTEXT (authoritative — use this as the conversion blueprint):");
        sb.AppendLine();
        sb.AppendLine($"(source: program-facts.json schema {facts.SchemaVersion}, " +
            $"identity {facts.IdentitySchemeVersion}, confidence {facts.Confidence})");
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
        sb.AppendLine("  • For EVERY 01-level data group below, generate a COMPLETE DTO class with ALL fields.");
        sb.AppendLine("  • Map PIC X→string, PIC S9V9→decimal, PIC 9 COMP-3→decimal, PIC 9 COMP→int/long.");
        sb.AppendLine("  • Preserve original COBOL field names (PascalCase). Do NOT simplify to fewer fields.");
        sb.AppendLine("  • If a group has >50 fields, still generate ALL of them.");
        sb.AppendLine();

        sb.AppendLine("CALL TARGET → SERVICE INJECTION RULES:");
        sb.AppendLine("  • For EVERY CALL target below: generate an interface + constructor-injected field + method call.");
        sb.AppendLine("  • Do NOT inline the called program's logic.");
        sb.AppendLine();

        if (facts.Warnings.Count > 0)
        {
            sb.AppendLine("WARNINGS (preserved from REKT extraction — surface in the generated code as TODOs where relevant):");
            foreach (var w in facts.Warnings) sb.AppendLine($"  • {w}");
            sb.AppendLine();
        }
        if (facts.PreprocessNotes.Count > 0)
        {
            sb.AppendLine("PREPROCESSOR TRANSFORMS APPLIED (the source you see has been rewritten — preserve original semantics):");
            foreach (var n in facts.PreprocessNotes)
                sb.AppendLine($"  • {n.Rule} @line {n.Line}: {n.Before ?? "?"} → {n.After ?? "?"}");
            sb.AppendLine();
        }

        sb.AppendLine("PROGRAM SUMMARY:");
        sb.AppendLine($"  programId   : {(string.IsNullOrEmpty(facts.Summary.ProgramId) ? "(unknown)" : facts.Summary.ProgramId)}");
        sb.AppendLine($"  basename    : {facts.Basename}");
        if (!string.IsNullOrEmpty(facts.RelativePath))
            sb.AppendLine($"  relativePath: {facts.RelativePath}");
        sb.AppendLine($"  loc         : {facts.Summary.Loc}");
        sb.AppendLine($"  sections    : {facts.Summary.Sections}");
        sb.AppendLine($"  paragraphs  : {facts.Summary.Paragraphs}");
        sb.AppendLine($"  isCopybook  : {facts.Summary.IsCopybook}");
        sb.AppendLine();

        sb.AppendLine("DATA GROUPS (01-level — one DTO/record class per entry, PascalCase property names):");
        if (facts.Data.Groups.Count == 0) sb.AppendLine("  (none)");
        else foreach (var g in facts.Data.Groups)
            sb.AppendLine($"  • {g.Name} — {g.FieldCount} field(s){(g.Redefines ? " [REDEFINES — generate as variant DTO]" : "")}");
        sb.AppendLine();

        sb.AppendLine("COPYBOOKS USED:");
        if (facts.Data.CopybooksUsed.Count == 0) sb.AppendLine("  (none)");
        else foreach (var c in facts.Data.CopybooksUsed) sb.AppendLine($"  • {c}");
        sb.AppendLine();

        sb.AppendLine("IO — DB TABLES (each becomes an EF Core entity / repository method):");
        if (facts.Io.DbTables.Count == 0) sb.AppendLine("  (none)");
        else foreach (var t in facts.Io.DbTables)
            sb.AppendLine($"  • {t.Name} : {string.Join(", ", t.Operations.OrderBy(o => o, StringComparer.Ordinal))}");
        sb.AppendLine();

        sb.AppendLine("IO — FILES (heuristic-extracted from PROCEDURE DIVISION; treat as file-IO ports):");
        if (facts.Io.Files.Count == 0) sb.AppendLine("  (none)");
        else foreach (var f in facts.Io.Files)
            sb.AppendLine($"  • {f.Name} : {string.Join(", ", f.Operations.OrderBy(o => o, StringComparer.Ordinal))}");
        sb.AppendLine();

        if (facts.Io.Screens.Count > 0)
        {
            sb.AppendLine("IO — SCREENS (CICS):");
            foreach (var s in facts.Io.Screens) sb.AppendLine($"  • {s}");
            sb.AppendLine();
        }
        if (facts.Io.Queues.Count > 0)
        {
            sb.AppendLine("IO — QUEUES:");
            foreach (var q in facts.Io.Queues) sb.AppendLine($"  • {q}");
            sb.AppendLine();
        }

        sb.AppendLine("CALL TARGETS (each becomes a service interface + constructor-injected field):");
        if (facts.Callees.Count == 0) sb.AppendLine("  (none)");
        else foreach (var c in facts.Callees) sb.AppendLine($"  • {c}");
        sb.AppendLine();

        if (facts.Callers.Count > 0)
        {
            sb.AppendLine("CALLED BY (informational — these programs depend on this one):");
            foreach (var c in facts.Callers) sb.AppendLine($"  • {c}");
            sb.AppendLine();
        }

        sb.AppendLine("CONTROL FLOW:");
        sb.Append("  entryPoints: ");
        sb.AppendLine(facts.ControlFlow.EntryPoints.Count == 0 ? "(none)" : string.Join(", ", facts.ControlFlow.EntryPoints));
        sb.Append("  exits      : ");
        sb.AppendLine(facts.ControlFlow.Exits.Count == 0 ? "(none)" : string.Join(", ", facts.ControlFlow.Exits));
        if (facts.ControlFlow.PerformChains.Count > 0)
        {
            sb.AppendLine("  performChains:");
            foreach (var chain in facts.ControlFlow.PerformChains)
                sb.AppendLine($"    • {string.Join(" → ", chain)}");
        }
        sb.AppendLine();

        sb.AppendLine("EXTERNAL EFFECTS (use to choose .NET libraries / DI registrations):");
        if (facts.ExternalEffects.Count == 0) sb.AppendLine("  (none)");
        else foreach (var e in facts.ExternalEffects) sb.AppendLine($"  • {e}");

        return sb.ToString();
    }
}
