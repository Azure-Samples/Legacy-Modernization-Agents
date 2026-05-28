using System.Text;
using System.Text.Json;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;

namespace CobolToQuarkusMigration.Helpers.PromptProjections;

/// <summary>
/// Projects a <see cref="ProgramFacts"/> instance into the structural-context
/// block the Java converter prompt embeds. PR4.a.
/// </summary>
/// <remarks>
/// <para>
/// Why this exists: <see cref="ProgramFacts"/> is the curated, schema-versioned
/// PR3 contract. The projection turns it into a compact prompt block that
/// replaces the raw <c>RektContextLoader</c> dump when
/// <c>_USE_PROGRAM_FACTS=true</c>.
/// </para>
/// <para>
/// Output rules:
/// </para>
/// <list type="bullet">
///   <item>Starts with the same marker line the existing REKT block uses
///         (<c>"REKT STRUCTURAL CONTEXT (authoritative — use this as the conversion blueprint):"</c>)
///         so <see cref="Agents.JavaConverterAgent.ExtractRektContextBlock"/>
///         still picks up the right slice for the cache key.</item>
///   <item>Carries the same fact-locking rules as the raw path (keeps
///         agent behaviour deterministic when the source of facts swaps).</item>
///   <item>Emits only the data sections that <see cref="ProgramFacts"/>
///         actually populates — empty lists are explicit ("none") so the
///         LLM does not invent values.</item>
///   <item>Surfaces <see cref="ProgramFacts.Warnings"/> and
///         <see cref="ProgramFacts.PreprocessNotes"/> as first-class
///         signals so the model treats them as constraints.</item>
/// </list>
/// </remarks>
public static class JavaConverterProjection
{
    /// <summary>Env var that activates the projection. Default off.</summary>
    public const string EnableEnvVar = "_USE_PROGRAM_FACTS";

    /// <summary>
    /// Returns true when the projection should be used for the next call.
    /// Pure function of the environment — no AsyncLocal, no hidden state.
    /// </summary>
    public static bool IsEnabled() =>
        string.Equals(
            Environment.GetEnvironmentVariable(EnableEnvVar), "true",
            StringComparison.OrdinalIgnoreCase);

    /// <summary>
    /// Loads <c>&lt;stem&gt;.facts.json</c> from <paramref name="factsDir"/>
    /// for the given program. Returns null when the file is missing or unreadable
    /// so callers can fall back to the raw-AST path.
    /// </summary>
    public static ProgramFacts? TryLoad(string factsDir, string programBasename)
    {
        var stem = Path.GetFileNameWithoutExtension(programBasename);
        if (string.IsNullOrEmpty(stem)) return null;

        var path = Path.Combine(factsDir, $"{stem}.facts.json");
        if (!File.Exists(path)) return null;

        try
        {
            var json = File.ReadAllText(path);
            return JsonSerializer.Deserialize<ProgramFacts>(json);
        }
        catch
        {
            return null;
        }
    }

    /// <summary>
    /// Renders the projection as a single string suitable for appending to the
    /// Java converter user prompt. The string starts with the
    /// <c>"REKT STRUCTURAL CONTEXT (authoritative"</c> marker the cache-key
    /// extractor recognises.
    /// </summary>
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
        sb.AppendLine("  • Map PIC X→String, PIC S9V9→BigDecimal, PIC 9 COMP-3→BigDecimal, PIC 9 COMP→int/long.");
        sb.AppendLine("  • Preserve original COBOL field names (camelCase). Do NOT simplify to fewer fields.");
        sb.AppendLine("  • If a group has >50 fields, still generate ALL of them.");
        sb.AppendLine();

        sb.AppendLine("CALL TARGET → SERVICE INJECTION RULES:");
        sb.AppendLine("  • For EVERY CALL target below: generate an interface + @Inject field + method call.");
        sb.AppendLine("  • Do NOT inline the called program's logic.");
        sb.AppendLine();

        // ── Warnings (first-class so the LLM treats them as constraints) ──
        if (facts.Warnings.Count > 0)
        {
            sb.AppendLine("WARNINGS (preserved from REKT extraction — surface in the generated code as TODOs where relevant):");
            foreach (var w in facts.Warnings)
                sb.AppendLine($"  • {w}");
            sb.AppendLine();
        }
        if (facts.PreprocessNotes.Count > 0)
        {
            sb.AppendLine("PREPROCESSOR TRANSFORMS APPLIED (the source you see has been rewritten — preserve original semantics):");
            foreach (var n in facts.PreprocessNotes)
                sb.AppendLine($"  • {n.Rule} @line {n.Line}: {n.Before ?? "?"} → {n.After ?? "?"}");
            sb.AppendLine();
        }

        // ── Summary ──
        sb.AppendLine("PROGRAM SUMMARY:");
        sb.AppendLine($"  programId   : {Display(facts.Summary.ProgramId)}");
        sb.AppendLine($"  basename    : {facts.Basename}");
        if (!string.IsNullOrEmpty(facts.RelativePath))
            sb.AppendLine($"  relativePath: {facts.RelativePath}");
        sb.AppendLine($"  loc         : {facts.Summary.Loc}");
        sb.AppendLine($"  sections    : {facts.Summary.Sections}");
        sb.AppendLine($"  paragraphs  : {facts.Summary.Paragraphs}");
        sb.AppendLine($"  isCopybook  : {facts.Summary.IsCopybook}");
        sb.AppendLine();

        // ── Data groups (drive DTO generation) ──
        sb.AppendLine("DATA GROUPS (01-level — one DTO/record class per entry):");
        if (facts.Data.Groups.Count == 0)
            sb.AppendLine("  (none)");
        else
            foreach (var g in facts.Data.Groups)
                sb.AppendLine($"  • {g.Name} — {g.FieldCount} field(s){(g.Redefines ? " [REDEFINES — generate as variant DTO]" : "")}");
        sb.AppendLine();

        // ── Copybooks ──
        sb.AppendLine("COPYBOOKS USED:");
        if (facts.Data.CopybooksUsed.Count == 0)
            sb.AppendLine("  (none)");
        else
            foreach (var c in facts.Data.CopybooksUsed)
                sb.AppendLine($"  • {c}");
        sb.AppendLine();

        // ── IO ──
        sb.AppendLine("IO — DB TABLES (each becomes a Panache entity / repository method):");
        if (facts.Io.DbTables.Count == 0)
            sb.AppendLine("  (none)");
        else
            foreach (var t in facts.Io.DbTables)
                sb.AppendLine($"  • {t.Name} : {string.Join(", ", t.Operations.OrderBy(o => o, StringComparer.Ordinal))}");
        sb.AppendLine();

        sb.AppendLine("IO — FILES (heuristic-extracted from PROCEDURE DIVISION; treat as file-IO ports):");
        if (facts.Io.Files.Count == 0)
            sb.AppendLine("  (none)");
        else
            foreach (var f in facts.Io.Files)
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

        // ── Callees (drive @Inject service generation) ──
        sb.AppendLine("CALL TARGETS (each becomes an @Inject service interface):");
        if (facts.Callees.Count == 0)
            sb.AppendLine("  (none)");
        else
            foreach (var c in facts.Callees)
                sb.AppendLine($"  • {c}");
        sb.AppendLine();

        // ── Callers (informational — agent shouldn't synthesise these) ──
        if (facts.Callers.Count > 0)
        {
            sb.AppendLine("CALLED BY (informational — these programs depend on this one):");
            foreach (var c in facts.Callers) sb.AppendLine($"  • {c}");
            sb.AppendLine();
        }

        // ── Control flow ──
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

        // ── External effects ──
        sb.AppendLine("EXTERNAL EFFECTS (use to choose Quarkus extensions / annotations):");
        if (facts.ExternalEffects.Count == 0)
            sb.AppendLine("  (none)");
        else
            foreach (var e in facts.ExternalEffects)
                sb.AppendLine($"  • {e}");

        return sb.ToString();
    }

    private static string Display(string s) => string.IsNullOrEmpty(s) ? "(unknown)" : s;
}
