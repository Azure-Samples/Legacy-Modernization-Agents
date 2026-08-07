using System.Text.Json;
using System.Text.RegularExpressions;
using CobolToQuarkusMigration.Agents.Infrastructure.RektCache;
using CobolToQuarkusMigration.Helpers;
using Microsoft.Extensions.Logging;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Facts;

public sealed class ProgramFactsExtractor
{
    private const string CompatibleScanCacheIdentityScheme = "v1-basename";
    private static readonly Regex CopyPattern = new(
        @"\bCOPY\s+['""]?([A-Z][A-Z0-9_-]*)['""]?",
        RegexOptions.Compiled | RegexOptions.IgnoreCase);

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

    public async Task<int> ExtractAllAsync(
        IReadOnlyList<string> programRelativePaths,
        string outputDir,
        CancellationToken cancellationToken = default)
    {
        Directory.CreateDirectory(outputDir);
        var loader = new RektContextLoader(_repoRoot, _rektDir);
        var targets = programRelativePaths
            .Select(SourcePathHelper.NormalizeRelativePath)
            .Where(path => !string.IsNullOrWhiteSpace(path))
            .Distinct(StringComparer.OrdinalIgnoreCase)
            .ToList();
        var catalog = new ProgramSourceCatalog(targets);

        // Pre-compute an inverse callees → callers map by scanning every
        // deps.json once. Avoids O(N²) per-extraction work.
        var callersByCallee = BuildCallersMap(targets, catalog, loader, _stagingDir);

        var written = 0;
        foreach (var programRelativePath in targets)
        {
            cancellationToken.ThrowIfCancellationRequested();
            var facts = await ExtractOneAsync(
                programRelativePath,
                catalog,
                loader,
                callersByCallee,
                cancellationToken);
            var outPath = ProgramFactsArtifactLocator.GetFactsFilePath(outputDir, programRelativePath);
            var outDir = Path.GetDirectoryName(outPath);
            if (!string.IsNullOrWhiteSpace(outDir))
                Directory.CreateDirectory(outDir);
            await File.WriteAllTextAsync(outPath, JsonSerializer.Serialize(facts, JsonOptions), cancellationToken);
            _logger?.LogInformation(
                "[ProgramFacts] program={Program} confidence={Conf} groups={Groups} callees={Callees} callers={Callers} warnings={Warnings} → {OutPath}",
                programRelativePath, facts.Confidence, facts.Data.Groups.Count, facts.Callees.Count,
                facts.Callers.Count, facts.Warnings.Count, outPath);
            written++;
        }
        return written;
    }

    internal async Task<ProgramFacts> ExtractOneAsync(
        string programRelativePath,
        ProgramSourceCatalog catalog,
        RektContextLoader loader,
        IReadOnlyDictionary<string, IReadOnlyList<string>> callersByCallee,
        CancellationToken cancellationToken = default)
    {
        programRelativePath = SourcePathHelper.NormalizeRelativePath(programRelativePath);
        var basename = Path.GetFileName(programRelativePath);
        var stem = Path.GetFileNameWithoutExtension(basename);

        // Hash the same preprocessed bytes used by scan and response caches.
        string sourceContent = "";
        var srcPath = Path.Combine(_stagingDir, SourcePathHelper.ToOsRelativePath(programRelativePath));
        if (File.Exists(srcPath))
        {
            sourceContent = await File.ReadAllTextAsync(srcPath, cancellationToken);
        }

        var sourceHash = string.IsNullOrEmpty(sourceContent)
            ? ""
            : CanonicalHasher.HashUtf8(sourceContent);

        // Pull whatever REKT data exists. The loader is best-effort; missing
        // files become empty lists.
        var ctx = loader.Load(programRelativePath, _stagingDir);
        var warnings = new List<string>();
        if (sourceContent.Length == 0)
            warnings.Add($"source-not-found: {srcPath}");
        if (ctx.Sections.Count == 0 && ctx.DataStructure.Count == 0
            && ctx.CallTargets.Count == 0 && ctx.SqlStatements.Count == 0)
            warnings.Add("rekt-output-empty: no AST/CFG/DataStructure JSONs found");
        warnings.AddRange(GetGeneratedStubWarnings(sourceContent));

        RektScanEntry? cacheEntry = null;
        if (_scanCache is not null && catalog.HasUniqueBasename(basename, out var uniqueProgramPath)
            && string.Equals(uniqueProgramPath, programRelativePath, StringComparison.OrdinalIgnoreCase))
        {
            cacheEntry = await _scanCache.TryGetAsync(
                basename,
                CompatibleScanCacheIdentityScheme,
                cancellationToken);
            if (cacheEntry is not null)
            {
                warnings.AddRange(cacheEntry.Warnings);
            }
        }

        var confidence = ResolveConfidence(cacheEntry, ctx, warnings);

        var programId = ExtractProgramId(sourceContent);
        var preprocessNotes = LoadPreprocessNotes(programRelativePath);

        var dbTables = BuildDbTables(ctx.SqlStatements);
        var files = ExtractFileAccess(sourceContent);
        // Screen and queue facts remain explicit until their readers are integrated.
        if (sourceContent.Contains("EXEC CICS", StringComparison.OrdinalIgnoreCase))
            warnings.Add("cics-detected-screens-not-extracted");

        var groups = ctx.DataStructure
            .Where(d => d.Level == 1)
            .Select(d => new DataGroup(
                Name: d.Name,
                FieldCount: CountLeaves(d),
                Redefines: !string.IsNullOrEmpty(d.Redefines)))
            .ToList();

        var calleesList = ctx.CallTargets
            .Select(c => catalog.ResolveCallTarget(c.TargetProgram) ?? c.TargetProgram)
            .Where(n => !string.IsNullOrEmpty(n))
            .Distinct(StringComparer.OrdinalIgnoreCase)
            .OrderBy(n => n, StringComparer.OrdinalIgnoreCase)
            .ToList();
        callersByCallee.TryGetValue(programRelativePath, out var callersForThisProgram);

        var entryPoints = ctx.Sections.Count > 0
            ? new List<string> { ctx.Sections[0].Name }
            : new List<string>();
        var performChains = BuildPerformChains(ctx);
        var exits = sourceContent.Contains("GOBACK", StringComparison.OrdinalIgnoreCase)
            ? new List<string> { "GOBACK" }
            : sourceContent.Contains("STOP RUN", StringComparison.OrdinalIgnoreCase)
                ? new List<string> { "STOP RUN" }
                : new List<string>();

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
            RelativePath = programRelativePath,
            SourceHash = sourceHash,
            Confidence = confidence,
            Warnings = warnings.Distinct(StringComparer.OrdinalIgnoreCase).ToList(),
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
            Callers = callersForThisProgram ?? Array.Empty<string>(),
            ControlFlow = new ControlFlowFacts
            {
                EntryPoints = entryPoints,
                PerformChains = performChains,
                Exits = exits,
            },
            ExternalEffects = effects,
        };
    }

    internal static FactConfidence ResolveConfidence(
        RektScanEntry? cacheEntry,
        RektContext context,
        IReadOnlyCollection<string> warnings)
    {
        FactConfidence confidence;
        if (cacheEntry is not null)
        {
            confidence = cacheEntry.Confidence switch
            {
                RektScanConfidence.High => FactConfidence.High,
                RektScanConfidence.Partial => FactConfidence.Partial,
                RektScanConfidence.Low => FactConfidence.Low,
                _ => FactConfidence.None,
            };
        }
        else if (context.Sections.Count > 0 && context.DataStructure.Count > 0)
        {
            confidence = FactConfidence.High;
        }
        else if (context.DataStructure.Count > 0 || context.CallTargets.Count > 0)
        {
            confidence = FactConfidence.Partial;
        }
        else
        {
            confidence = warnings.Count == 0 ? FactConfidence.Low : FactConfidence.None;
        }

        return warnings.Any(w => w.StartsWith("generated-copybook-stub:", StringComparison.OrdinalIgnoreCase))
            && confidence > FactConfidence.Partial
                ? FactConfidence.Partial
                : confidence;
    }

    private IReadOnlyList<string> GetGeneratedStubWarnings(string sourceContent)
    {
        var manifestPath = Path.Combine(_stagingDir, ".generated-stubs");
        if (string.IsNullOrWhiteSpace(sourceContent) || !File.Exists(manifestPath))
            return Array.Empty<string>();

        var generatedStubs = File.ReadAllLines(manifestPath)
            .Where(line => !string.IsNullOrWhiteSpace(line))
            .Select(line => line.Trim())
            .ToHashSet(StringComparer.OrdinalIgnoreCase);
        if (generatedStubs.Count == 0)
            return Array.Empty<string>();

        var used = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        foreach (var line in sourceContent.Split('\n'))
        {
            var trimmed = line.TrimStart();
            if (trimmed.StartsWith("*>", StringComparison.Ordinal)
                || (line.Length > 6 && line[6] is '*' or '/'))
                continue;

            var match = CopyPattern.Match(line);
            if (match.Success && generatedStubs.Contains(match.Groups[1].Value))
                used.Add(match.Groups[1].Value.ToUpperInvariant());
        }

        return used
            .OrderBy(name => name, StringComparer.OrdinalIgnoreCase)
            .Select(name => $"generated-copybook-stub:{name}")
            .ToList();
    }

    private static IReadOnlyDictionary<string, IReadOnlyList<string>> BuildCallersMap(
        IReadOnlyList<string> programRelativePaths,
        ProgramSourceCatalog catalog,
        RektContextLoader loader,
        string sourceFolder)
    {
        var map = new Dictionary<string, List<string>>(StringComparer.OrdinalIgnoreCase);
        foreach (var caller in programRelativePaths)
        {
            var ctx = loader.Load(caller, sourceFolder);
            foreach (var callee in ctx.CallTargets)
            {
                var rawTarget = callee.TargetProgram;
                if (string.IsNullOrEmpty(rawTarget)) continue;

                var key = catalog.ResolveCallTarget(rawTarget) ?? rawTarget;

                if (!map.TryGetValue(key, out var list))
                {
                    list = new List<string>();
                    map[key] = list;
                }
                if (!list.Contains(caller, StringComparer.OrdinalIgnoreCase))
                    list.Add(caller);
            }
        }
        return map.ToDictionary(
            kv => kv.Key,
            kv => (IReadOnlyList<string>)kv.Value
                .OrderBy(path => path, StringComparer.OrdinalIgnoreCase)
                .ToList(),
            StringComparer.OrdinalIgnoreCase);
    }

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
        // Fall back to procedure-division verbs because the AST omits FD entries.
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

    private IReadOnlyList<PreprocessNote> LoadPreprocessNotes(string programRelativePath)
    {
        foreach (var notePath in EnumeratePreprocessNotePaths(programRelativePath))
        {
            if (!File.Exists(notePath)) continue;
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

        return Array.Empty<PreprocessNote>();
    }

    private IEnumerable<string> EnumeratePreprocessNotePaths(string programRelativePath)
    {
        var normalizedRelativePath = SourcePathHelper.NormalizeRelativePath(programRelativePath);
        yield return Path.Combine(
            _stagingDir,
            SourcePathHelper.ToOsRelativePath(normalizedRelativePath) + ".preprocess.json");
        yield return Path.Combine(_stagingDir, Path.GetFileName(normalizedRelativePath) + ".preprocess.json");
    }
}
