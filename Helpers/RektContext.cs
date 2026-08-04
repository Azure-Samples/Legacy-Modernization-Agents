// Shared structural-context model for parser and LLM extraction results.

using System.Text.Json.Serialization;

namespace CobolToQuarkusMigration.Helpers;

public enum StructuralProvenance
{
    None,            // No structure available; fall back to raw source only
    RektNative,      // Full REKT JSON present and parsed cleanly
    RektPartial,     // Only dependency export available (AST writer NPE)
    LlmExtracted,    // Structure synthesised by StructuralExtractorAgent from source
}

public sealed class StructuralContext
{
    public string Program { get; set; } = "";
    public StructuralProvenance Provenance { get; set; }
    public double Confidence { get; set; }              // 0.0–1.0
    public RektContext Context { get; set; } = new();
    public string? Notes { get; set; }                  // e.g. "deps-only output, AST missing"
}

public sealed class RektContext
{
    public string Program { get; set; } = "";
    public int LineCount { get; set; }
    public bool IsCopybook { get; set; }

    public List<RektSection>      Sections      { get; set; } = new();
    public List<RektPerformEdge>  PerformGraph  { get; set; } = new();
    public List<RektCallTarget>   CallTargets   { get; set; } = new();
    public List<RektSqlStatement> SqlStatements { get; set; } = new();
    public List<string>           CopybookUsage { get; set; } = new();
    public List<RektDataItem>     DataStructure { get; set; } = new();

    // From target-architecture.json — null if no plan saved yet.
    public RektTargetPlan? TargetPlan { get; set; }
}

public sealed class RektSection
{
    public string Name { get; set; } = "";
    public int StartLine { get; set; }
    public int EndLine { get; set; }
    public List<RektParagraph> Paragraphs { get; set; } = new();
}

public sealed class RektParagraph
{
    public string Name { get; set; } = "";
    public int StartLine { get; set; }
    public int EndLine { get; set; }
}

public sealed class RektPerformEdge
{
    public string From { get; set; } = "";
    public string To { get; set; } = "";
    public bool Conditional { get; set; }
}

public sealed class RektCallTarget
{
    public string TargetProgram { get; set; } = "";
    public bool IsDynamic { get; set; }
    public int LineNumber { get; set; }
}

public sealed class RektSqlStatement
{
    public string Operation { get; set; } = "";           // SELECT, INSERT, UPDATE, DELETE, …
    public List<string> Tables { get; set; } = new();
    public int LineNumber { get; set; }
    public string? Excerpt { get; set; }
}

public sealed class RektDataItem
{
    public int Level { get; set; }                        // 01, 03, 05, …
    public string Name { get; set; } = "";
    public string? PicClause { get; set; }
    public string? Usage { get; set; }                    // COMP, COMP-3, …
    public string? Value { get; set; }
    public string? Redefines { get; set; }
    public int? Occurs { get; set; }
    public List<RektDataItem> Children { get; set; } = new();
}

public sealed class RektTargetPlan
{
    public string TargetComponent { get; set; } = "";
    public string TargetComponentName { get; set; } = "";
    public string TargetLayer { get; set; } = "";
    public string TargetTech { get; set; } = "";
    public string Strategy { get; set; } = "";
    public int Wave { get; set; }
    public double Complexity { get; set; }
    public string Rationale { get; set; } = "";
    public List<string> Patterns { get; set; } = new();
    public List<string> MigrationNotes { get; set; } = new();
}

public static class RektContextFormatter
{
    public static string ToPromptBlock(StructuralContext sc)
    {
        var sb = new System.Text.StringBuilder();
        sb.AppendLine($"STRUCTURAL CONTEXT (program: {sc.Program})");
        sb.AppendLine($"SOURCE: {sc.Provenance}    confidence: {sc.Confidence:F2}");
        if (!string.IsNullOrEmpty(sc.Notes)) sb.AppendLine($"NOTES: {sc.Notes}");
        sb.AppendLine();

        var ctx = sc.Context;
        if (ctx.TargetPlan is { } plan)
        {
            sb.AppendLine($"TARGET COMPONENT: {plan.TargetComponentName} ({plan.TargetLayer})");
            sb.AppendLine($"TARGET TECH: {plan.TargetTech}");
            sb.AppendLine($"STRATEGY: {plan.Strategy}    WAVE: {plan.Wave}    COMPLEXITY: {plan.Complexity:F2}");
            if (plan.Patterns.Count > 0)
                sb.AppendLine($"PATTERNS: {string.Join(", ", plan.Patterns)}");
            if (plan.MigrationNotes.Count > 0)
            {
                sb.AppendLine("MIGRATION NOTES:");
                foreach (var n in plan.MigrationNotes) sb.AppendLine($"  - {n}");
            }
            sb.AppendLine();
        }

        if (ctx.Sections.Count > 0)
        {
            sb.AppendLine($"SECTIONS ({ctx.Sections.Count}):");
            foreach (var s in ctx.Sections)
            {
                sb.AppendLine($"  {s.Name} (lines {s.StartLine}-{s.EndLine})  paragraphs: {s.Paragraphs.Count}");
                foreach (var p in s.Paragraphs.Take(6))
                    sb.AppendLine($"    - {p.Name} (lines {p.StartLine}-{p.EndLine})");
                if (s.Paragraphs.Count > 6) sb.AppendLine($"    … and {s.Paragraphs.Count - 6} more");
            }
            sb.AppendLine();
        }

        if (ctx.PerformGraph.Count > 0)
        {
            sb.AppendLine($"PERFORM GRAPH ({ctx.PerformGraph.Count} edges):");
            foreach (var e in ctx.PerformGraph.Take(30))
                sb.AppendLine($"  {e.From} → {e.To}{(e.Conditional ? " (conditional)" : "")}");
            if (ctx.PerformGraph.Count > 30) sb.AppendLine($"  … and {ctx.PerformGraph.Count - 30} more");
            sb.AppendLine();
        }

        if (ctx.CallTargets.Count > 0)
        {
            sb.AppendLine($"PROGRAM CALLS ({ctx.CallTargets.Count}):");
            foreach (var c in ctx.CallTargets)
                sb.AppendLine($"  → {c.TargetProgram} (line {c.LineNumber}){(c.IsDynamic ? " [dynamic]" : "")}");
            sb.AppendLine();
        }

        if (ctx.SqlStatements.Count > 0)
        {
            sb.AppendLine($"EXEC SQL ({ctx.SqlStatements.Count}):");
            foreach (var s in ctx.SqlStatements.Take(20))
                sb.AppendLine($"  {s.Operation} {string.Join(",", s.Tables)} (line {s.LineNumber})");
            if (ctx.SqlStatements.Count > 20) sb.AppendLine($"  … and {ctx.SqlStatements.Count - 20} more");
            sb.AppendLine();
        }

        if (ctx.CopybookUsage.Count > 0)
            sb.AppendLine($"COPYBOOKS USED: {string.Join(", ", ctx.CopybookUsage)}");

        if (ctx.DataStructure.Count > 0)
        {
            // Filter out smojol artefacts (level -1, TypedRecord noise) and FILLER-only groups.
            var meaningful = ctx.DataStructure
                .Where(d => d.Level >= 0 && !d.Name.StartsWith("TypedRecord") && d.Name != "FILLER")
                .ToList();

            if (meaningful.Count > 0)
            {
                sb.AppendLine();
                sb.AppendLine($"DATA STRUCTURE ({meaningful.Count} groups — generate a DTO/record class for each):");
                sb.AppendLine("  Each top-level group (01-level) should become a separate class/record.");
                sb.AppendLine("  Use the field names and PIC clauses below to derive the correct types.");
                sb.AppendLine("  If a group comes from a COPY (copybook), name the class after the copybook.");
                sb.AppendLine();
                foreach (var d in meaningful)
                {
                    // Limit large prompt projections; the source remains authoritative for omitted fields.
                    int totalFields = CountFields(d);
                    int maxDepth = totalFields > 50 ? 10 : 30;
                    RenderDataItem(sb, d, indent: 2, maxChildren: maxDepth);
                    if (totalFields > 50)
                        sb.AppendLine($"    … {totalFields} fields total — generate ALL in the DTO using the COBOL source for complete field list");
                }
            }
        }

        return sb.ToString();
    }

    private static void RenderDataItem(System.Text.StringBuilder sb, RektDataItem d, int indent, int maxChildren = 30)
    {
        var pad = new string(' ', indent);
        var pic = d.PicClause != null ? $" {d.PicClause}" : "";
        var usage = d.Usage != null ? $" {d.Usage}" : "";
        var redef = d.Redefines != null ? $" REDEFINES {d.Redefines}" : "";
        var occ = d.Occurs.HasValue ? $" OCCURS {d.Occurs}" : "";
        sb.AppendLine($"{pad}{d.Level:00} {d.Name}{pic}{usage}{redef}{occ}");
        foreach (var c in d.Children.Take(maxChildren)) RenderDataItem(sb, c, indent + 2, maxChildren);
        if (d.Children.Count > maxChildren)
            sb.AppendLine($"{pad}  … and {d.Children.Count - maxChildren} more child fields");
    }

    private static int CountFields(RektDataItem d)
    {
        int count = 1;
        foreach (var c in d.Children) count += CountFields(c);
        return count;
    }
}
