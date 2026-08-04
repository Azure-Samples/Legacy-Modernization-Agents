using System.Text;
using System.Text.Json;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;

namespace CobolToQuarkusMigration.Helpers.PromptProjections;

public static class CSharpConverterProjection
{
    public const string EnableEnvVar = JavaConverterProjection.EnableEnvVar;

    public static bool IsEnabled() => JavaConverterProjection.IsEnabled();

    public static ProgramFacts? TryLoad(string factsDir, string programBasename) =>
        JavaConverterProjection.TryLoad(factsDir, programBasename);

    public static string BuildPromptBlock(ProgramFacts facts)
    {
        var sb = new StringBuilder();
        sb.AppendLine(PromptLoader.LoadSectionValidated(
            "RektContext", "CommonPolicy", new Dictionary<string, string>
            {
                ["SourceMetadata"] =
                    $"(source: program-facts.json schema {facts.SchemaVersion}, " +
                    $"identity {facts.IdentitySchemeVersion}, confidence {facts.Confidence}){Environment.NewLine}"
            }));
        sb.AppendLine(PromptLoader.LoadSectionValidated(
            "RektContext", "CSharpTargetPolicy", new Dictionary<string, string>()));

        if (facts.Warnings.Count > 0)
        {
            sb.AppendLine(PromptLoader.LoadSectionValidated(
                "RektContext", "WarningsHeader", new Dictionary<string, string>()));
            foreach (var w in facts.Warnings) sb.AppendLine($"  • {w}");
            sb.AppendLine();
        }
        if (facts.PreprocessNotes.Count > 0)
        {
            sb.AppendLine(PromptLoader.LoadSectionValidated(
                "RektContext", "PreprocessHeader", new Dictionary<string, string>()));
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

        sb.AppendLine(PromptLoader.LoadSectionValidated(
            "RektContext", "CSharpDataGroupsHeader", new Dictionary<string, string>()));
        if (facts.Data.Groups.Count == 0) sb.AppendLine("  (none)");
        else foreach (var g in facts.Data.Groups)
            sb.AppendLine($"  • {g.Name} — {g.FieldCount} field(s){(g.Redefines ? " [REDEFINES — generate as variant DTO]" : "")}");
        sb.AppendLine();

        sb.AppendLine("COPYBOOKS USED:");
        if (facts.Data.CopybooksUsed.Count == 0) sb.AppendLine("  (none)");
        else foreach (var c in facts.Data.CopybooksUsed) sb.AppendLine($"  • {c}");
        sb.AppendLine();

        sb.AppendLine(PromptLoader.LoadSectionValidated(
            "RektContext", "CSharpDbTablesHeader", new Dictionary<string, string>()));
        if (facts.Io.DbTables.Count == 0) sb.AppendLine("  (none)");
        else foreach (var t in facts.Io.DbTables)
            sb.AppendLine($"  • {t.Name} : {string.Join(", ", t.Operations.OrderBy(o => o, StringComparer.Ordinal))}");
        sb.AppendLine();

        sb.AppendLine(PromptLoader.LoadSectionValidated(
            "RektContext", "FilesHeader", new Dictionary<string, string>()));
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

        sb.AppendLine(PromptLoader.LoadSectionValidated(
            "RektContext", "CSharpCallTargetsHeader", new Dictionary<string, string>()));
        if (facts.Callees.Count == 0) sb.AppendLine("  (none)");
        else foreach (var c in facts.Callees) sb.AppendLine($"  • {c}");
        sb.AppendLine();

        if (facts.Callers.Count > 0)
        {
            sb.AppendLine(PromptLoader.LoadSectionValidated(
                "RektContext", "CalledByHeader", new Dictionary<string, string>()));
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

        sb.AppendLine(PromptLoader.LoadSectionValidated(
            "RektContext", "CSharpExternalEffectsHeader", new Dictionary<string, string>()));
        if (facts.ExternalEffects.Count == 0) sb.AppendLine("  (none)");
        else foreach (var e in facts.ExternalEffects) sb.AppendLine($"  • {e}");

        return sb.ToString();
    }
}
