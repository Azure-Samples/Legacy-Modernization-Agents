// RektContextLoader.cs — Reads native REKT JSON outputs into a typed RektContext.
//
// Sources (all under output/rekt/):
//   flow-ast-<program>.json           — sections, paragraphs, perform, call, branches
//   flow-cfg-<program>.json           — control-flow edges (optional, used for perform graph)
//   flow-data-<program>.json          — working-storage / linkage structure
//   <program>-deps.json               — dependency export (used when full AST is missing)
//   target-architecture.json          — per-program recommendation plan
//
// All readers are tolerant: missing files / fields don't throw, they just leave the
// corresponding list empty. The caller (StructuralContextProvider) decides how to
// react (fall back to LLM extraction, etc).

using System.Text.Json;

namespace CobolToQuarkusMigration.Helpers;

public sealed class RektContextLoader
{
    private readonly string _repoRoot;
    private readonly string _rektDir;
    private readonly string? _targetArchPath;

    // Parsed target architecture cached for the lifetime of the loader.
    private Dictionary<string, RektTargetPlan>? _targetPlansByProgram;

    public RektContextLoader(string repoRoot)
    {
        _repoRoot = repoRoot;
        _rektDir = Path.Combine(repoRoot, "output", "rekt");
        _targetArchPath = Path.Combine(_rektDir, "target-architecture.json");
    }

    /// <summary>
    /// Returns true if any REKT output exists for any program (i.e. rekt-full has run).
    /// </summary>
    public bool HasAnyRektOutput()
    {
        if (!Directory.Exists(_rektDir)) return false;
        return Directory.EnumerateFiles(_rektDir, "*.json").Any();
    }

    /// <summary>
    /// Best-effort load. Returns a RektContext populated from whatever is available;
    /// LineCount/IsCopybook/Program are always filled (from disk inspection) even if
    /// no REKT JSON exists. Caller checks Sections.Count / CallTargets.Count etc. to
    /// decide whether to invoke the LLM fallback.
    /// </summary>
    public RektContext Load(string programFileName, string sourceFolder)
    {
        var ctx = new RektContext
        {
            Program = programFileName,
            IsCopybook = programFileName.EndsWith(".cpy", StringComparison.OrdinalIgnoreCase),
        };

        var srcPath = Path.Combine(_repoRoot, sourceFolder, programFileName);
        if (File.Exists(srcPath))
            ctx.LineCount = File.ReadLines(srcPath).Count();

        var stem = Path.GetFileNameWithoutExtension(programFileName);

        TryLoadFlowAst(ctx, stem);
        TryLoadFlowCfg(ctx, stem);
        TryLoadFlowData(ctx, stem);
        TryLoadDeps(ctx, stem);

        ctx.TargetPlan = LookupTargetPlan(programFileName);

        return ctx;
    }

    public List<string> EnumerateProgramFiles(string sourceFolder)
    {
        var dir = Path.Combine(_repoRoot, sourceFolder);
        if (!Directory.Exists(dir)) return new();
        return Directory.EnumerateFiles(dir)
            .Where(p =>
            {
                var ext = Path.GetExtension(p);
                return ext.Equals(".cbl", StringComparison.OrdinalIgnoreCase)
                    || ext.Equals(".cpy", StringComparison.OrdinalIgnoreCase)
                    || ext.Equals(".CBL", StringComparison.OrdinalIgnoreCase)
                    || ext.Equals(".CPY", StringComparison.OrdinalIgnoreCase);
            })
            .Select(Path.GetFileName)
            .Where(n => n is not null)
            .Cast<string>()
            .OrderBy(n => n, StringComparer.OrdinalIgnoreCase)
            .ToList();
    }

    // ── Per-source readers ────────────────────────────────────────────────

    private void TryLoadFlowAst(RektContext ctx, string stem)
    {
        var path = FindRektFile(stem, prefix: "flow-ast-");
        if (path is null) return;

        try
        {
            using var doc = JsonDocument.Parse(File.ReadAllText(path));
            WalkAst(doc.RootElement, ctx, currentSection: null);
        }
        catch (Exception) { /* tolerated; loader returns what it found */ }
    }

    // smojol flow-ast files vary in shape — the safe approach is to walk the JSON
    // tree recursively and harvest any object that looks like a SECTION / PARAGRAPH /
    // PERFORM / CALL / EXEC SQL node based on its `nodeType` (or `type`) field.
    private void WalkAst(JsonElement el, RektContext ctx, RektSection? currentSection)
    {
        if (el.ValueKind == JsonValueKind.Object)
        {
            string? nodeType = null;
            if (el.TryGetProperty("nodeType", out var nt) && nt.ValueKind == JsonValueKind.String) nodeType = nt.GetString();
            else if (el.TryGetProperty("type", out var t) && t.ValueKind == JsonValueKind.String) nodeType = t.GetString();

            var name = TryGetString(el, "name") ?? TryGetString(el, "displayName") ?? "";
            var startLine = TryGetInt(el, "startLine");
            var endLine = TryGetInt(el, "endLine");

            switch (nodeType?.ToUpperInvariant())
            {
                case "SECTION":
                {
                    var section = new RektSection { Name = name, StartLine = startLine, EndLine = endLine };
                    ctx.Sections.Add(section);
                    currentSection = section;
                    break;
                }
                case "PARAGRAPH":
                {
                    var p = new RektParagraph { Name = name, StartLine = startLine, EndLine = endLine };
                    if (currentSection != null) currentSection.Paragraphs.Add(p);
                    else ctx.Sections.Add(new RektSection { Name = "(implicit)", Paragraphs = { p } });
                    break;
                }
                case "PERFORM":
                {
                    var target = TryGetString(el, "target") ?? name;
                    if (!string.IsNullOrEmpty(target))
                    {
                        ctx.PerformGraph.Add(new RektPerformEdge
                        {
                            From = currentSection?.Name ?? "",
                            To = target,
                            Conditional = TryGetBool(el, "conditional")
                        });
                    }
                    break;
                }
                case "CALL":
                case "CALLSTATEMENT":
                {
                    var target = TryGetString(el, "target") ?? name;
                    if (!string.IsNullOrEmpty(target))
                    {
                        ctx.CallTargets.Add(new RektCallTarget
                        {
                            TargetProgram = target.Trim('\''),
                            IsDynamic = TryGetBool(el, "dynamic"),
                            LineNumber = startLine,
                        });
                    }
                    break;
                }
                case "DIALECT":
                case "DIALECT_CONTAINER":
                {
                    // smojol surfaces EXEC SQL via DIALECT nodes. We try to harvest the
                    // operation keyword from a text/excerpt field if present.
                    var excerpt = TryGetString(el, "text") ?? TryGetString(el, "excerpt") ?? "";
                    var op = ExtractSqlOperation(excerpt);
                    if (op != null)
                    {
                        ctx.SqlStatements.Add(new RektSqlStatement
                        {
                            Operation = op,
                            Tables = ExtractSqlTables(excerpt),
                            LineNumber = startLine,
                            Excerpt = excerpt.Length > 200 ? excerpt[..200] : excerpt,
                        });
                    }
                    break;
                }
            }

            foreach (var prop in el.EnumerateObject())
                WalkAst(prop.Value, ctx, currentSection);
        }
        else if (el.ValueKind == JsonValueKind.Array)
        {
            foreach (var item in el.EnumerateArray())
                WalkAst(item, ctx, currentSection);
        }
    }

    private void TryLoadFlowCfg(RektContext ctx, string stem)
    {
        var path = FindRektFile(stem, prefix: "flow-cfg-");
        if (path is null) return;
        // CFG edges are useful for branch counts; we don't expose them directly today.
        // Reserved for future use (e.g. test synthesizer per-branch coverage).
    }

    private void TryLoadFlowData(RektContext ctx, string stem)
    {
        var path = FindRektFile(stem, prefix: "flow-data-");
        if (path is null) return;
        try
        {
            using var doc = JsonDocument.Parse(File.ReadAllText(path));
            WalkData(doc.RootElement, ctx.DataStructure, depth: 0);
        }
        catch (Exception) { /* tolerated */ }
    }

    private void WalkData(JsonElement el, List<RektDataItem> bucket, int depth)
    {
        if (el.ValueKind == JsonValueKind.Object)
        {
            var name = TryGetString(el, "name");
            var level = TryGetInt(el, "level");
            if (!string.IsNullOrEmpty(name) && level > 0)
            {
                var item = new RektDataItem
                {
                    Level = level,
                    Name = name,
                    PicClause = TryGetString(el, "pic") ?? TryGetString(el, "picClause"),
                    Usage = TryGetString(el, "usage"),
                    Value = TryGetString(el, "value"),
                    Redefines = TryGetString(el, "redefines"),
                    Occurs = TryGetIntNullable(el, "occurs"),
                };
                bucket.Add(item);

                // Children
                if (el.TryGetProperty("children", out var children) && children.ValueKind == JsonValueKind.Array)
                {
                    foreach (var child in children.EnumerateArray())
                        WalkData(child, item.Children, depth + 1);
                }
                return; // don't double-walk
            }

            foreach (var prop in el.EnumerateObject())
                WalkData(prop.Value, bucket, depth);
        }
        else if (el.ValueKind == JsonValueKind.Array)
        {
            foreach (var item in el.EnumerateArray())
                WalkData(item, bucket, depth);
        }
    }

    private void TryLoadDeps(RektContext ctx, string stem)
    {
        // Even when AST writer fails, deps export usually succeeds — feed it into CallTargets/CopybookUsage
        // so the converter at least knows what the program depends on.
        var path = Path.Combine(_rektDir, $"{stem}-deps.json");
        if (!File.Exists(path)) return;
        try
        {
            using var doc = JsonDocument.Parse(File.ReadAllText(path));
            if (doc.RootElement.TryGetProperty("dependencies", out var deps) && deps.ValueKind == JsonValueKind.Array)
            {
                foreach (var d in deps.EnumerateArray())
                {
                    var name = TryGetString(d, "name") ?? "";
                    if (string.IsNullOrEmpty(name)) continue;

                    if (name.EndsWith(".cpy", StringComparison.OrdinalIgnoreCase))
                    {
                        if (!ctx.CopybookUsage.Contains(name, StringComparer.OrdinalIgnoreCase))
                            ctx.CopybookUsage.Add(name);
                    }
                    else
                    {
                        if (!ctx.CallTargets.Any(c => string.Equals(c.TargetProgram, name, StringComparison.OrdinalIgnoreCase)))
                            ctx.CallTargets.Add(new RektCallTarget { TargetProgram = name });
                    }
                }
            }
        }
        catch (Exception) { /* tolerated */ }
    }

    // ── Target architecture lookup ────────────────────────────────────────

    private RektTargetPlan? LookupTargetPlan(string programFileName)
    {
        if (_targetArchPath is null || !File.Exists(_targetArchPath)) return null;
        try
        {
            _targetPlansByProgram ??= ParseTargetArchitecture(_targetArchPath);
            // Look up by exact name first, then bare stem.
            if (_targetPlansByProgram.TryGetValue(programFileName, out var p)) return p;
            var stem = Path.GetFileNameWithoutExtension(programFileName);
            return _targetPlansByProgram.TryGetValue(stem, out var p2) ? p2 : null;
        }
        catch (Exception) { return null; }
    }

    private Dictionary<string, RektTargetPlan> ParseTargetArchitecture(string path)
    {
        var map = new Dictionary<string, RektTargetPlan>(StringComparer.OrdinalIgnoreCase);
        using var doc = JsonDocument.Parse(File.ReadAllText(path));
        if (!doc.RootElement.TryGetProperty("programMappings", out var pm)) return map;
        if (pm.ValueKind != JsonValueKind.Array) return map;

        foreach (var entry in pm.EnumerateArray())
        {
            var program = TryGetString(entry, "program") ?? "";
            if (string.IsNullOrEmpty(program)) continue;
            if (!entry.TryGetProperty("recommendation", out var rec) || rec.ValueKind != JsonValueKind.Object) continue;

            var plan = new RektTargetPlan
            {
                TargetComponent = TryGetString(rec, "targetComponent") ?? "",
                TargetComponentName = TryGetString(rec, "targetComponentName") ?? "",
                TargetLayer = TryGetString(rec, "targetLayer") ?? "",
                TargetTech = TryGetString(rec, "targetTech") ?? "",
                Strategy = TryGetString(rec, "strategy") ?? "",
                Wave = TryGetInt(rec, "wave"),
                Complexity = TryGetDouble(rec, "complexity"),
                Rationale = TryGetString(rec, "rationale") ?? "",
            };
            if (rec.TryGetProperty("patterns", out var pat) && pat.ValueKind == JsonValueKind.Array)
                plan.Patterns = pat.EnumerateArray()
                    .Where(x => x.ValueKind == JsonValueKind.String)
                    .Select(x => x.GetString()!)
                    .ToList();
            if (rec.TryGetProperty("migrationNotes", out var notes) && notes.ValueKind == JsonValueKind.Array)
                plan.MigrationNotes = notes.EnumerateArray()
                    .Where(x => x.ValueKind == JsonValueKind.String)
                    .Select(x => x.GetString()!)
                    .ToList();

            map[program] = plan;
            map[Path.GetFileNameWithoutExtension(program)] = plan;
        }
        return map;
    }

    /// <summary>
    /// Exposes the full target architecture mappings (used by ProgramSelectorService
    /// for --wave / --target selectors).
    /// </summary>
    public IReadOnlyDictionary<string, RektTargetPlan> GetAllTargetPlans()
    {
        if (_targetArchPath is null || !File.Exists(_targetArchPath))
            return new Dictionary<string, RektTargetPlan>();
        _targetPlansByProgram ??= ParseTargetArchitecture(_targetArchPath);
        return _targetPlansByProgram;
    }

    // ── Utilities ─────────────────────────────────────────────────────────

    private string? FindRektFile(string stem, string prefix)
    {
        // smojol emits files using either bare stem or with .cbl suffix in the name.
        foreach (var candidate in new[] { $"{prefix}{stem}.json", $"{prefix}{stem}.cbl.json" })
        {
            var p = Path.Combine(_rektDir, candidate);
            if (File.Exists(p)) return p;
        }
        return null;
    }

    private static string? TryGetString(JsonElement el, string name)
        => el.TryGetProperty(name, out var v) && v.ValueKind == JsonValueKind.String ? v.GetString() : null;

    private static int TryGetInt(JsonElement el, string name)
        => el.TryGetProperty(name, out var v) && v.ValueKind == JsonValueKind.Number && v.TryGetInt32(out var i) ? i : 0;

    private static int? TryGetIntNullable(JsonElement el, string name)
        => el.TryGetProperty(name, out var v) && v.ValueKind == JsonValueKind.Number && v.TryGetInt32(out var i) ? i : null;

    private static double TryGetDouble(JsonElement el, string name)
        => el.TryGetProperty(name, out var v) && v.ValueKind == JsonValueKind.Number && v.TryGetDouble(out var d) ? d : 0.0;

    private static bool TryGetBool(JsonElement el, string name)
        => el.TryGetProperty(name, out var v) && v.ValueKind is JsonValueKind.True or JsonValueKind.False && v.GetBoolean();

    private static string? ExtractSqlOperation(string sql)
    {
        if (string.IsNullOrWhiteSpace(sql)) return null;
        var trimmed = sql.TrimStart();
        var firstWord = new string(trimmed.TakeWhile(c => !char.IsWhiteSpace(c)).ToArray()).ToUpperInvariant();
        return firstWord switch
        {
            "SELECT" or "INSERT" or "UPDATE" or "DELETE" or "MERGE" or "OPEN" or "FETCH" or "CLOSE" or "DECLARE" or "EXEC" => firstWord,
            _ => null,
        };
    }

    private static List<string> ExtractSqlTables(string sql)
    {
        // Lightweight: pick the token after FROM / INTO / UPDATE / JOIN.
        var tables = new List<string>();
        if (string.IsNullOrWhiteSpace(sql)) return tables;
        var rx = new System.Text.RegularExpressions.Regex(
            @"\b(?:FROM|INTO|UPDATE|JOIN)\s+([A-Z][A-Z0-9_]*)",
            System.Text.RegularExpressions.RegexOptions.IgnoreCase);
        foreach (System.Text.RegularExpressions.Match m in rx.Matches(sql))
        {
            var t = m.Groups[1].Value.ToUpperInvariant();
            if (!tables.Contains(t)) tables.Add(t);
        }
        return tables;
    }
}
