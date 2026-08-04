using System.Text.Json;
using CobolToQuarkusMigration.Agents.Infrastructure.RektCache;
using CobolToQuarkusMigration.Helpers;
using Microsoft.Extensions.Logging;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Facts;

/// <summary>
/// Builds <see cref="ProgramFacts"/> for a batch of programs from REKT output
/// JSONs plus the staging-dir source bytes plus (optionally) a scan-cache entry
/// for confidence.
/// </summary>
/// <remarks>
/// <para>
/// The extractor is intentionally <b>not</b> wired into agent prompts in PR3.
/// PR4 (prompt projection layer) will consume <c>output/rekt/&lt;stem&gt;.facts.json</c>
/// directly. PR3 only produces the contract; behaviour change comes with PR4.
/// </para>
/// <para>
/// Missing data is represented explicitly (empty list + warning), never silently.
/// </para>
/// </remarks>
public sealed class ProgramFactsExtractor
{
    private static readonly JsonSerializerOptions JsonOptions = new()
    {
        WriteIndented = true,
        Encoder = System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping,
    };

    private readonly string _repoRoot;
    private readonly string _stagingDir;
    private readonly string _rektDir;
    private readonly IRektScanCache? _scanCache;
    private readonly ILogger? _logger;

    public ProgramFactsExtractor(
        string repoRoot,
        string stagingDir,
        string? rektDir = null,
        IRektScanCache? scanCache = null,
        ILogger? logger = null)
    {
        _repoRoot = repoRoot;
        _stagingDir = stagingDir;
        _rektDir = rektDir ?? Path.Combine(repoRoot, "output", "rekt");
        _scanCache = scanCache;
        _logger = logger;
    }

    /// <summary>
    /// Extracts facts for every program in <paramref name="programBasenames"/>
    /// and writes <c>&lt;stem&gt;.facts.json</c> to <paramref name="outputDir"/>.
    /// Returns the count of files written.
    /// </summary>
    public async Task<int> ExtractAllAsync(
        IReadOnlyList<string> programBasenames,
        string outputDir,
        CancellationToken cancellationToken = default)
    {
        Directory.CreateDirectory(outputDir);
        var loader = new RektContextLoader(_repoRoot);

        // Pre-compute an inverse callees → callers map by scanning every
        // deps.json once. Avoids O(N²) per-extraction work.
        var callersByCallee = BuildCallersMap(programBasenames, loader);

        var written = 0;
        foreach (var basename in programBasenames)
        {
            cancellationToken.ThrowIfCancellationRequested();
            var facts = await ExtractOneAsync(basename, loader, callersByCallee, cancellationToken);
            var stem = Path.GetFileNameWithoutExtension(basename);
            var outPath = Path.Combine(outputDir, $"{stem}.facts.json");
            await File.WriteAllTextAsync(outPath, JsonSerializer.Serialize(facts, JsonOptions), cancellationToken);
            _logger?.LogInformation(
                "[ProgramFacts] basename={Basename} confidence={Conf} groups={Groups} callees={Callees} callers={Callers} warnings={Warnings} → {OutPath}",
                basename, facts.Confidence, facts.Data.Groups.Count, facts.Callees.Count,
                facts.Callers.Count, facts.Warnings.Count, outPath);
            written++;
        }
        return written;
    }

    /// <summary>Public for tests — extracts one program's facts without persisting.</summary>
    public async Task<ProgramFacts> ExtractOneAsync(
        string basename,
        RektContextLoader loader,
        IReadOnlyDictionary<string, IReadOnlyList<string>> callersByCallee,
        CancellationToken cancellationToken = default)
    {
        var stem = Path.GetFileNameWithoutExtension(basename);

        // Load source bytes from the staging dir (preprocessed). Same bytes the
        // PR2 scan cache and the PR1 response cache hash — keeps SourceHash
        // consistent across the stack.
        string sourceContent = "";
        var srcPath = Path.Combine(_stagingDir, basename);
        if (File.Exists(srcPath))
        {
            sourceContent = await File.ReadAllTextAsync(srcPath, cancellationToken);
        }

        var sourceHash = string.IsNullOrEmpty(sourceContent)
            ? ""
            : CanonicalHasher.HashUtf8(sourceContent);

        // Pull whatever REKT data exists. The loader is best-effort; missing
        // files become empty lists.
        var ctx = loader.Load(basename, _stagingDir);
        var warnings = new List<string>();
        if (sourceContent.Length == 0)
            warnings.Add($"source-not-found: {srcPath}");
        if (ctx.Sections.Count == 0 && ctx.DataStructure.Count == 0
            && ctx.CallTargets.Count == 0 && ctx.SqlStatements.Count == 0)
            warnings.Add("rekt-output-empty: no AST/CFG/DataStructure JSONs found");

        // Confidence from scan cache outcome when available; otherwise infer from
        // what we managed to load.
        var confidence = FactConfidence.None;
        if (_scanCache is not null)
        {
            var entry = await _scanCache.TryGetAsync(basename, ProgramFacts.CurrentIdentitySchemeVersion, cancellationToken);
            if (entry is not null)
            {
                confidence = entry.Confidence switch
                {
                    RektScanConfidence.High => FactConfidence.High,
                    RektScanConfidence.Partial => FactConfidence.Partial,
                    RektScanConfidence.Low => FactConfidence.Low,
                    _ => FactConfidence.None,
                };
            }
        }
        if (confidence == FactConfidence.None)
        {
            // Fallback inference when no cache entry is available.
            if (ctx.Sections.Count > 0 && ctx.DataStructure.Count > 0)
                confidence = FactConfidence.High;
            else if (ctx.DataStructure.Count > 0 || ctx.CallTargets.Count > 0)
                confidence = FactConfidence.Partial;
            else if (warnings.Count == 0)
                confidence = FactConfidence.Low;
        }

        var programId = ExtractProgramId(sourceContent);
        var preprocessNotes = LoadPreprocessNotes(basename);

        // ── IO ──
        var dbTables = BuildDbTables(ctx.SqlStatements);
        var files = ExtractFileAccess(sourceContent);
        // Screens / queues left as empty lists with a warning — extraction needs
        // PR5 (BMS reader integration) and is out of scope for PR3.
        if (sourceContent.Contains("EXEC CICS", StringComparison.OrdinalIgnoreCase))
            warnings.Add("cics-detected-screens-not-extracted");

        // ── Data groups (01-level only — that's what becomes a DTO/record). ──
        var groups = ctx.DataStructure
            .Where(d => d.Level == 1)
            .Select(d => new DataGroup(
                Name: d.Name,
                FieldCount: CountLeaves(d),
                Redefines: !string.IsNullOrEmpty(d.Redefines)))
            .ToList();

        // ── Callers / callees ──
        var calleesList = ctx.CallTargets
            .Select(c => c.TargetProgram)
            .Where(n => !string.IsNullOrEmpty(n))
            .Distinct(StringComparer.OrdinalIgnoreCase)
            .ToList();
        callersByCallee.TryGetValue(basename, out var callersForThisProgram);

        // ── Control flow ──
        var entryPoints = ctx.Sections.Count > 0
            ? new List<string> { ctx.Sections[0].Name }
            : new List<string>();
        var performChains = BuildPerformChains(ctx);
        var exits = sourceContent.Contains("GOBACK", StringComparison.OrdinalIgnoreCase)
            ? new List<string> { "GOBACK" }
            : sourceContent.Contains("STOP RUN", StringComparison.OrdinalIgnoreCase)
                ? new List<string> { "STOP RUN" }
                : new List<string>();

        // ── External effects ──
        var effects = new List<string>();
        if (dbTables.Count > 0) effects.Add("DB_IO");
        if (calleesList.Count > 0) effects.Add("CALL_OUT");
        if (files.Count > 0) effects.Add("FILE_IO");
        if (sourceContent.Contains("EXEC CICS", StringComparison.OrdinalIgnoreCase)) effects.Add("CICS");
        if (sourceContent.Contains("EXEC DLI", StringComparison.OrdinalIgnoreCase)) effects.Add("IMS_DLI");

        return new ProgramFacts
        {
            Basename = basename,
            Stem = stem,
            RelativePath = null,    // forward-compat; populated when we have it
            SourceHash = sourceHash,
            Confidence = confidence,
            Warnings = warnings,
            PreprocessNotes = preprocessNotes,
            Summary = new ProgramSummary
            {
                Loc = ctx.LineCount,
                Paragraphs = ctx.Sections.Sum(s => s.Paragraphs.Count),
                Sections = ctx.Sections.Count,
                IsCopybook = ctx.IsCopybook,
                ProgramId = programId,
            },
            Io = new IoFacts
            {
                Files = files,
                DbTables = dbTables,
                Screens = Array.Empty<string>(),
                Queues = Array.Empty<string>(),
            },
            Data = new DataFacts
            {
                Groups = groups,
                CopybooksUsed = ctx.CopybookUsage.ToList(),
            },
            Callees = calleesList,
            Callers = callersForThisProgram ?? new List<string>(),
            ControlFlow = new ControlFlowFacts
            {
                EntryPoints = entryPoints,
                PerformChains = performChains,
                Exits = exits,
            },
            ExternalEffects = effects,
        };
    }

    // ─────────────────────────── helpers ───────────────────────────

    private static IReadOnlyDictionary<string, IReadOnlyList<string>> BuildCallersMap(
        IReadOnlyList<string> programBasenames, RektContextLoader loader)
    {
        // Index of stem (case-insensitive) → basename for normalising CALL targets
        // (which arrive as bare program names from smojol, e.g. "CHILD") to the
        // basename we key callers by (e.g. "CHILD.cbl").
        var stemToBasename = programBasenames.ToDictionary(
            Path.GetFileNameWithoutExtension!,
            b => b,
            StringComparer.OrdinalIgnoreCase);

        var map = new Dictionary<string, List<string>>(StringComparer.OrdinalIgnoreCase);
        foreach (var caller in programBasenames)
        {
            var ctx = loader.Load(caller, sourceFolder: "source");   // sourceFolder unused for CallTargets
            foreach (var callee in ctx.CallTargets)
            {
                var rawTarget = callee.TargetProgram;
                if (string.IsNullOrEmpty(rawTarget)) continue;

                // Normalise the target: try as-is, then as stem→basename, then with .cbl/.cob.
                string key;
                if (stemToBasename.TryGetValue(Path.GetFileNameWithoutExtension(rawTarget), out var b))
                    key = b;
                else
                    key = rawTarget;

                if (!map.TryGetValue(key, out var list))
                {
                    list = new List<string>();
                    map[key] = list;
                }
                if (!list.Contains(caller, StringComparer.OrdinalIgnoreCase))
                    list.Add(caller);
            }
        }
        return map.ToDictionary(kv => kv.Key, kv => (IReadOnlyList<string>)kv.Value,
            StringComparer.OrdinalIgnoreCase);
    }

    /// <summary>Extracts PROGRAM-ID. value from the first 50 non-comment lines.</summary>
    private static string ExtractProgramId(string content)
    {
        if (string.IsNullOrEmpty(content)) return "";
        var lines = content.Split('\n').Take(50);
        foreach (var raw in lines)
        {
            var line = raw.TrimEnd();
            if (line.Length > 6 && line[6] == '*') continue;
            var idx = line.IndexOf("PROGRAM-ID", StringComparison.OrdinalIgnoreCase);
            if (idx < 0) continue;
            // PROGRAM-ID. NAME.   or   PROGRAM-ID NAME.
            var rest = line.Substring(idx + "PROGRAM-ID".Length).TrimStart('.', ' ', '\t').TrimEnd('.');
            return rest.Split(' ', '\t')[0].Trim();
        }
        return "";
    }

    private static int CountLeaves(Helpers.RektDataItem item)
    {
        if (item.Children.Count == 0) return 1;
        return item.Children.Sum(CountLeaves);
    }

    private static IReadOnlyList<DbTableAccess> BuildDbTables(IEnumerable<Helpers.RektSqlStatement> sqls)
    {
        var byTable = new Dictionary<string, HashSet<string>>(StringComparer.OrdinalIgnoreCase);
        foreach (var sql in sqls)
        {
            var op = string.IsNullOrEmpty(sql.Operation) ? "UNKNOWN" : sql.Operation.ToUpperInvariant();
            foreach (var t in sql.Tables)
            {
                if (string.IsNullOrEmpty(t)) continue;
                if (!byTable.TryGetValue(t, out var ops))
                {
                    ops = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
                    byTable[t] = ops;
                }
                ops.Add(op);
            }
        }
        return byTable
            .OrderBy(kv => kv.Key, StringComparer.OrdinalIgnoreCase)
            .Select(kv => new DbTableAccess(kv.Key,
                kv.Value.OrderBy(o => o, StringComparer.OrdinalIgnoreCase).ToList()))
            .ToList();
    }

    private static IReadOnlyList<FileAccess> ExtractFileAccess(string content)
    {
        // Heuristic: scan the procedure division for OPEN/READ/WRITE/REWRITE/CLOSE on
        // file names. The smojol AST doesn't expose FD entries today; this is a
        // best-effort fallback that surfaces *some* file IO rather than nothing.
        if (string.IsNullOrEmpty(content)) return Array.Empty<FileAccess>();
        var byName = new Dictionary<string, HashSet<string>>(StringComparer.OrdinalIgnoreCase);
        var rx = new System.Text.RegularExpressions.Regex(
            @"\b(OPEN\s+(?:INPUT|OUTPUT|I-O|EXTEND)|READ|WRITE|REWRITE|CLOSE)\s+([A-Z][A-Z0-9_-]*)",
            System.Text.RegularExpressions.RegexOptions.IgnoreCase);
        foreach (System.Text.RegularExpressions.Match m in rx.Matches(content))
        {
            var op = m.Groups[1].Value.ToUpperInvariant().Split(' ')[0];
            var name = m.Groups[2].Value;
            if (!byName.TryGetValue(name, out var ops))
            {
                ops = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
                byName[name] = ops;
            }
            ops.Add(op);
        }
        return byName
            .OrderBy(kv => kv.Key, StringComparer.OrdinalIgnoreCase)
            .Select(kv => new FileAccess(kv.Key,
                kv.Value.OrderBy(o => o, StringComparer.OrdinalIgnoreCase).ToList()))
            .ToList();
    }

    private static IReadOnlyList<IReadOnlyList<string>> BuildPerformChains(Helpers.RektContext ctx)
    {
        if (ctx.PerformGraph.Count == 0) return Array.Empty<IReadOnlyList<string>>();
        var byFrom = ctx.PerformGraph
            .GroupBy(e => e.From, StringComparer.OrdinalIgnoreCase)
            .ToDictionary(g => g.Key, g => g.Select(e => e.To).ToList(),
                StringComparer.OrdinalIgnoreCase);

        // Emit one chain per top-level entry section, depth-bounded to avoid cycles.
        var chains = new List<IReadOnlyList<string>>();
        foreach (var entry in ctx.Sections.Take(5))
        {
            var chain = new List<string> { entry.Name };
            var current = entry.Name;
            var seen = new HashSet<string>(StringComparer.OrdinalIgnoreCase) { current };
            while (chain.Count < 25 && byFrom.TryGetValue(current, out var nexts))
            {
                var next = nexts.FirstOrDefault(n => !seen.Contains(n));
                if (next is null) break;
                chain.Add(next);
                seen.Add(next);
                current = next;
            }
            if (chain.Count > 1) chains.Add(chain);
        }
        return chains;
    }

    /// <summary>
    /// Loads PR5 <c>.preprocess.json</c> sidecar if present. PR3 ships with the schema
    /// reader so once PR5 starts writing them they land in <c>ProgramFacts</c> automatically.
    /// </summary>
    private IReadOnlyList<PreprocessNote> LoadPreprocessNotes(string basename)
    {
        var notePath = Path.Combine(_stagingDir, basename + ".preprocess.json");
        if (!File.Exists(notePath)) return Array.Empty<PreprocessNote>();
        try
        {
            using var doc = JsonDocument.Parse(File.ReadAllText(notePath));
            if (!doc.RootElement.TryGetProperty("transforms", out var arr)) return Array.Empty<PreprocessNote>();
            var notes = new List<PreprocessNote>(arr.GetArrayLength());
            foreach (var el in arr.EnumerateArray())
            {
                notes.Add(new PreprocessNote(
                    Rule: el.TryGetProperty("rule", out var r) ? r.GetString() ?? "" : "",
                    Line: el.TryGetProperty("line", out var l) && l.TryGetInt32(out var ln) ? ln : 0,
                    Before: el.TryGetProperty("before", out var b) ? b.GetString() : null,
                    After: el.TryGetProperty("after", out var a) ? a.GetString() : null));
            }
            return notes;
        }
        catch
        {
            return Array.Empty<PreprocessNote>();
        }
    }
}
