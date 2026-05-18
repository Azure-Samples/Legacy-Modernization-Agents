// ProgramSelectorService.cs — Resolves a CLI/portal selector to a concrete list of
// program file names from output/rekt/ + target-architecture.json + the source folder.
//
// Selector semantics (matches CLI flag combine logic):
//   - Same field repeated  → OR within that field
//   - Different fields     → AND between fields
//   - --include-callees / --include-callers expand the result through the CALL graph
//
// Selection sources:
//   --program     direct file match (with or without .cbl extension)
//   --transaction CICS RETURN TRANSID(X) / LINK PROGRAM(X) probe across all programs
//   --wave        target-architecture.json → programMappings[].recommendation.wave
//   --target      target-architecture.json → programMappings[].recommendation.targetComponent
//   --keyword     simple substring search across .cbl source (whole-word, case-insensitive)
//   --include-callees / --include-callers walks REKT CallTargets

using System.Text.RegularExpressions;
using CobolToQuarkusMigration.Helpers;

namespace McpChatWeb.Services;

public sealed class ProgramSelector
{
    public List<string> Programs { get; set; } = new();        // direct names
    public List<string> Transactions { get; set; } = new();    // CICS tranids
    public List<int> Waves { get; set; } = new();              // 1, 2, 3
    public List<string> Targets { get; set; } = new();         // svc-business, ...
    public List<string> Keywords { get; set; } = new();
    public bool IncludeCallees { get; set; }
    public bool IncludeCallers { get; set; }
    public string SourceFolder { get; set; } = "source";
}

public sealed class ProgramSelectorResult
{
    public List<string> Files { get; set; } = new();
    public List<string> Reasons { get; set; } = new();         // human-readable explanation per match
    public string Summary { get; set; } = "";
}

public sealed class ProgramSelectorService
{
    private readonly string _repoRoot;

    public ProgramSelectorService(string repoRoot) => _repoRoot = repoRoot;

    public ProgramSelectorResult Resolve(ProgramSelector sel)
    {
        var loader = new RektContextLoader(_repoRoot);
        var sourceDir = Path.Combine(_repoRoot, sel.SourceFolder);
        var allFiles = loader.EnumerateProgramFiles(sel.SourceFolder);
        var result = new ProgramSelectorResult();
        var reasons = new Dictionary<string, List<string>>(StringComparer.OrdinalIgnoreCase);

        // Per-field result sets (OR within), then AND-intersect.
        var hits = new List<HashSet<string>>();

        // --program
        if (sel.Programs.Count > 0)
        {
            var bucket = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
            foreach (var p in sel.Programs)
            {
                var stem = Path.GetFileNameWithoutExtension(p);
                foreach (var f in allFiles)
                {
                    if (string.Equals(f, p, StringComparison.OrdinalIgnoreCase)
                        || string.Equals(Path.GetFileNameWithoutExtension(f), stem, StringComparison.OrdinalIgnoreCase))
                    {
                        bucket.Add(f);
                        Note(reasons, f, $"matches --program {p}");
                    }
                }
            }
            hits.Add(bucket);
        }

        // --transaction (BMS-not-yet probe: look for EXEC CICS RETURN TRANSID(X) / LINK PROGRAM(X))
        if (sel.Transactions.Count > 0)
        {
            var bucket = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
            foreach (var tran in sel.Transactions)
            {
                var rxTransid = new Regex($@"\bEXEC\s+CICS\b[^\.]*?\bTRANSID\s*\(\s*['""]?{Regex.Escape(tran)}['""]?\s*\)",
                    RegexOptions.IgnoreCase | RegexOptions.Singleline);
                var rxLink = new Regex($@"\bEXEC\s+CICS\s+LINK\b[^\.]*?\bPROGRAM\s*\(\s*['""]?{Regex.Escape(tran)}['""]?\s*\)",
                    RegexOptions.IgnoreCase | RegexOptions.Singleline);
                foreach (var f in allFiles)
                {
                    try
                    {
                        var src = File.ReadAllText(Path.Combine(sourceDir, f));
                        if (rxTransid.IsMatch(src) || rxLink.IsMatch(src))
                        {
                            bucket.Add(f);
                            Note(reasons, f, $"references transaction '{tran}'");
                        }
                    }
                    catch { /* unreadable file, skip */ }
                }
            }
            hits.Add(bucket);
        }

        // --wave + --target (both look at target-architecture.json)
        var plans = loader.GetAllTargetPlans();
        if (sel.Waves.Count > 0)
        {
            var bucket = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
            foreach (var w in sel.Waves)
            {
                foreach (var (prog, plan) in plans)
                {
                    if (plan.Wave != w) continue;
                    var match = allFiles.FirstOrDefault(f =>
                        f.Equals(prog, StringComparison.OrdinalIgnoreCase) ||
                        Path.GetFileNameWithoutExtension(f).Equals(Path.GetFileNameWithoutExtension(prog), StringComparison.OrdinalIgnoreCase));
                    if (match != null)
                    {
                        bucket.Add(match);
                        Note(reasons, match, $"in wave {w}");
                    }
                }
            }
            hits.Add(bucket);
        }
        if (sel.Targets.Count > 0)
        {
            var bucket = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
            foreach (var tgt in sel.Targets)
            {
                foreach (var (prog, plan) in plans)
                {
                    if (!plan.TargetComponent.Equals(tgt, StringComparison.OrdinalIgnoreCase)) continue;
                    var match = allFiles.FirstOrDefault(f =>
                        f.Equals(prog, StringComparison.OrdinalIgnoreCase) ||
                        Path.GetFileNameWithoutExtension(f).Equals(Path.GetFileNameWithoutExtension(prog), StringComparison.OrdinalIgnoreCase));
                    if (match != null)
                    {
                        bucket.Add(match);
                        Note(reasons, match, $"targets {tgt}");
                    }
                }
            }
            hits.Add(bucket);
        }

        // --keyword
        if (sel.Keywords.Count > 0)
        {
            var bucket = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
            foreach (var k in sel.Keywords)
            {
                var pattern = new Regex(@"\b" + Regex.Escape(k) + @"\b", RegexOptions.IgnoreCase);
                foreach (var f in allFiles)
                {
                    try
                    {
                        var src = File.ReadAllText(Path.Combine(sourceDir, f));
                        if (pattern.IsMatch(src))
                        {
                            bucket.Add(f);
                            Note(reasons, f, $"contains keyword '{k}'");
                        }
                    }
                    catch { }
                }
            }
            hits.Add(bucket);
        }

        // If no selectors at all → no result. Caller decides whether that means
        // "everything" (default-run behaviour) or "empty" (explicit search).
        IEnumerable<string> resolved;
        if (hits.Count == 0)
        {
            resolved = Enumerable.Empty<string>();
            result.Summary = "no selectors supplied";
        }
        else
        {
            // AND intersect
            HashSet<string>? acc = null;
            foreach (var h in hits)
            {
                acc = acc is null ? new HashSet<string>(h, StringComparer.OrdinalIgnoreCase)
                                  : new HashSet<string>(acc.Intersect(h, StringComparer.OrdinalIgnoreCase), StringComparer.OrdinalIgnoreCase);
            }
            resolved = acc ?? Enumerable.Empty<string>();
        }

        // Closure expansion
        var resolvedSet = new HashSet<string>(resolved, StringComparer.OrdinalIgnoreCase);
        if (sel.IncludeCallees && resolvedSet.Count > 0)
        {
            var added = ExpandCallees(loader, resolvedSet, sel.SourceFolder, allFiles, reasons);
            foreach (var a in added) resolvedSet.Add(a);
        }
        if (sel.IncludeCallers && resolvedSet.Count > 0)
        {
            var added = ExpandCallers(loader, resolvedSet, sel.SourceFolder, allFiles, reasons);
            foreach (var a in added) resolvedSet.Add(a);
        }

        result.Files = resolvedSet.OrderBy(f => f, StringComparer.OrdinalIgnoreCase).ToList();
        result.Reasons = result.Files.Select(f =>
            $"{f}  ({string.Join("; ", reasons.GetValueOrDefault(f, new List<string> { "matched" }))})").ToList();
        if (string.IsNullOrEmpty(result.Summary))
            result.Summary = $"{result.Files.Count} program(s) selected";
        return result;
    }

    private static HashSet<string> ExpandCallees(
        RektContextLoader loader, HashSet<string> seed, string sourceFolder,
        List<string> allFiles, Dictionary<string, List<string>> reasons)
    {
        var added = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        var queue = new Queue<string>(seed);
        var seen = new HashSet<string>(seed, StringComparer.OrdinalIgnoreCase);
        while (queue.Count > 0)
        {
            var f = queue.Dequeue();
            var ctx = loader.Load(f, sourceFolder);
            foreach (var c in ctx.CallTargets)
            {
                var target = MatchFile(allFiles, c.TargetProgram);
                if (target is null || seen.Contains(target)) continue;
                seen.Add(target);
                added.Add(target);
                Note(reasons, target, $"transitive callee of {f}");
                queue.Enqueue(target);
            }
        }
        return added;
    }

    private static HashSet<string> ExpandCallers(
        RektContextLoader loader, HashSet<string> seed, string sourceFolder,
        List<string> allFiles, Dictionary<string, List<string>> reasons)
    {
        // Inverse traversal: scan all files and add any whose CallTargets include a seed.
        var added = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        var queue = new Queue<string>(seed);
        var seen = new HashSet<string>(seed, StringComparer.OrdinalIgnoreCase);
        while (queue.Count > 0)
        {
            var target = queue.Dequeue();
            var targetStem = Path.GetFileNameWithoutExtension(target);
            foreach (var f in allFiles)
            {
                if (seen.Contains(f)) continue;
                var ctx = loader.Load(f, sourceFolder);
                if (ctx.CallTargets.Any(c =>
                    c.TargetProgram.Equals(target, StringComparison.OrdinalIgnoreCase) ||
                    c.TargetProgram.Equals(targetStem, StringComparison.OrdinalIgnoreCase)))
                {
                    seen.Add(f);
                    added.Add(f);
                    Note(reasons, f, $"transitive caller of {target}");
                    queue.Enqueue(f);
                }
            }
        }
        return added;
    }

    private static string? MatchFile(List<string> allFiles, string target)
    {
        var clean = target.Trim().Trim('\'').Trim('"');
        var stem = Path.GetFileNameWithoutExtension(clean);
        return allFiles.FirstOrDefault(f =>
            f.Equals(clean, StringComparison.OrdinalIgnoreCase) ||
            Path.GetFileNameWithoutExtension(f).Equals(stem, StringComparison.OrdinalIgnoreCase));
    }

    private static void Note(Dictionary<string, List<string>> dict, string file, string reason)
    {
        if (!dict.TryGetValue(file, out var list))
        {
            list = new List<string>();
            dict[file] = list;
        }
        if (!list.Contains(reason)) list.Add(reason);
    }
}
