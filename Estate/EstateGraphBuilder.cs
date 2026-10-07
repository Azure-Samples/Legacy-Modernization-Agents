using System.Text.RegularExpressions;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Jcl;

namespace CobolToQuarkusMigration.Estate;

// Builds the estate graph from the source alone: programs and copybooks by their own statements,
// jobs by the JCL parser, transactions and CICS files by the CSD. No model is involved, so the same
// source always gives the same graph.
public static class EstateGraphBuilder
{
    public static EstateGraph Build(string sourceRoot, string? jclRoot = null, EstateGraphOptions? options = null,
        IEnumerable<string>? extraDiagnostics = null)
    {
        options ??= new EstateGraphOptions();
        var g = new Accumulator(options);
        g.Diagnostics.AddRange(extraDiagnostics ?? []);

        var scans = AddPrograms(g, sourceRoot, options);
        AddCopybooks(g, sourceRoot, options);
        AddMaps(g, sourceRoot, options);
        AddCicsDefinitions(g, sourceRoot, options);
        AddCicsYamlDefinitions(g, sourceRoot, options);
        AddDdlTables(g, sourceRoot, options);
        AddApis(g, sourceRoot, options);
        foreach (var scan in scans) AddReferences(g, scan);
        AddJobs(g, jclRoot ?? sourceRoot, scans);

        var nodes = g.Nodes.Values.OrderBy(n => n.Id, StringComparer.Ordinal).ToList();
        var edges = g.Edges.Values
            .OrderBy(e => e.From, StringComparer.Ordinal).ThenBy(e => e.To, StringComparer.Ordinal)
            .ThenBy(e => e.Kind, StringComparer.Ordinal).ToList();

        var analysis = EstateAnalysis.Analyze(nodes, edges, options);

        var counts = new SortedDictionary<string, int>(StringComparer.Ordinal);
        foreach (var group in nodes.GroupBy(n => n.Kind)) counts[group.Key] = group.Count();
        counts["missing"] = nodes.Count(n => !n.InSource);
        counts["edges"] = edges.Count;
        counts["hubs"] = analysis.Hubs.Count;
        counts["clusters"] = analysis.Clusters.Count;
        counts["waves"] = analysis.Waves.Count;

        return new EstateGraph
        {
            GeneratedAtUtc = DateTime.UtcNow,
            Counts = counts,
            Nodes = nodes,
            Edges = edges,
            Hubs = analysis.Hubs,
            Clusters = analysis.Clusters,
            Waves = analysis.Waves,
            Diagnostics = g.Diagnostics,
        };
    }

    public static string ProgramId(string nameOrPath) => $"{EstateNodeKind.Program}:{nameOrPath}";
    public static string NodeId(string kind, string name) => $"{kind}:{name}";

    private sealed class Accumulator(EstateGraphOptions options)
    {
        public readonly Dictionary<string, EstateNode> Nodes = new(StringComparer.Ordinal);
        public readonly Dictionary<(string, string, string), EstateEdge> Edges = new();
        public readonly List<string> Diagnostics = [];
        // Program name (file stem or PROGRAM-ID) to the nodes that carry it; more than one is ambiguous.
        public readonly Dictionary<string, List<string>> ProgramsByName = new(StringComparer.OrdinalIgnoreCase);
        public readonly Dictionary<string, string> CopybooksByName = new(StringComparer.OrdinalIgnoreCase);
        public readonly Dictionary<string, string> ProgramByFile = new(StringComparer.Ordinal);

        public EstateNode Node(string kind, string name, string? file = null, bool inSource = true)
        {
            var id = NodeId(kind, name);
            if (!Nodes.TryGetValue(id, out var node))
                Nodes[id] = node = new EstateNode { Id = id, Kind = kind, Name = name, File = file, InSource = inSource };
            return node;
        }

        public void Edge(string from, string to, string kind, string? via, IEnumerable<EstateEvidence> evidence)
        {
            if (!Edges.TryGetValue((from, to, kind), out var edge))
                Edges[(from, to, kind)] = edge = new EstateEdge { From = from, To = to, Kind = kind, Via = via };
            foreach (var e in evidence)
            {
                if (edge.Evidence.Count >= options.MaxEvidencePerEdge) break;
                if (!edge.Evidence.Contains(e)) edge.Evidence.Add(e);
            }
        }

        // A name the source has once resolves to that program; one it lacks becomes a missing node so
        // the reference stays visible; one it has twice resolves to both, and says so.
        public IReadOnlyList<string> ResolveProgram(string name, string referencedFrom)
        {
            if (ProgramsByName.TryGetValue(name, out var ids))
            {
                if (ids.Count > 1)
                    Diagnostics.Add($"{referencedFrom} refers to {name}, which {ids.Count} source files define: {string.Join(", ", ids)}.");
                return ids;
            }
            var missing = Node(EstateNodeKind.Program, name.ToUpperInvariant(), inSource: false);
            return [missing.Id];
        }
    }

    private static List<ProgramScan> AddPrograms(Accumulator g, string sourceRoot, EstateGraphOptions options)
    {
        var files = SourceTypeRegistry.EnumerateProgramFiles(sourceRoot)
            .Select(f => Path.GetRelativePath(sourceRoot, f).Replace('\\', '/'))
            .OrderBy(f => f, StringComparer.Ordinal).ToList();
        var stems = files.GroupBy(f => Path.GetFileNameWithoutExtension(f).ToUpperInvariant())
            .ToDictionary(x => x.Key, x => x.Count());

        var scans = new List<ProgramScan>();
        foreach (var rel in files)
        {
            string text;
            try { text = File.ReadAllText(Path.Join(sourceRoot, rel)); }
            catch (Exception ex) when (ex is IOException or UnauthorizedAccessException)
            {
                g.Diagnostics.Add($"{rel} could not be read: {ex.Message}");
                continue;
            }

            var stem = Path.GetFileNameWithoutExtension(rel).ToUpperInvariant();
            var key = stems[stem] > 1 ? rel : stem;
            var scan = EstateSourceScanner.Scan(rel, text, options);
            var node = g.Node(EstateNodeKind.Program, key, rel);
            g.ProgramByFile[rel] = node.Id;
            node.Attributes["loc"] = text.Count(c => c == '\n').ToString(System.Globalization.CultureInfo.InvariantCulture);
            if (scan.ProgramId is { } pid && !pid.Equals(stem, StringComparison.OrdinalIgnoreCase))
                node.Attributes["programId"] = pid;
            if (scan.AssignedDds.Count > 0) node.Attributes["dds"] = string.Join(",", scan.AssignedDds);
            if (stems[stem] > 1) node.Attributes["ambiguousBasename"] = "true";
            foreach (var (k, v) in EstateProgramMetrics.Measure(text))
                node.Attributes[k] = v.ToString(System.Globalization.CultureInfo.InvariantCulture);
            if (EstateProgramMetrics.Description(text) is { } description) node.Attributes["description"] = description;

            foreach (var name in new[] { stem, scan.ProgramId }.OfType<string>().Distinct(StringComparer.OrdinalIgnoreCase))
            {
                if (!g.ProgramsByName.TryGetValue(name, out var list)) g.ProgramsByName[name] = list = [];
                if (!list.Contains(node.Id)) list.Add(node.Id);
            }

            foreach (var dynamic in scan.UnresolvedDynamic)
                g.Diagnostics.Add($"{rel}:{dynamic.Evidence[0].Line} {dynamic.Kind} through {dynamic.Via}, which no literal in the program is moved into.");
            if (scan.UnresolvedDynamic.Count > 0)
                node.Attributes["unresolvedDynamicCalls"] = scan.UnresolvedDynamic.Count.ToString(System.Globalization.CultureInfo.InvariantCulture);

            scans.Add(scan);
        }
        return scans;
    }

    private static void AddCopybooks(Accumulator g, string sourceRoot, EstateGraphOptions options)
    {
        // A real copybook wins over a generated stand-in of the same name, matching the REKT parse.
        var generatedDirs = options.GeneratedCopybookDirectories.ToHashSet(StringComparer.OrdinalIgnoreCase);
        bool IsGenerated(string file) =>
            Path.GetRelativePath(sourceRoot, Path.GetDirectoryName(file)!)
                .Split(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar)
                .Any(generatedDirs.Contains);

        foreach (var file in SourceTypeRegistry.EnumerateCopybookFiles(sourceRoot)
                     .OrderBy(IsGenerated)
                     .ThenBy(f => f, StringComparer.Ordinal))
        {
            var rel = Path.GetRelativePath(sourceRoot, file).Replace('\\', '/');
            var name = Path.GetFileNameWithoutExtension(rel).ToUpperInvariant();
            if (g.CopybooksByName.ContainsKey(name))
            {
                g.Diagnostics.Add($"Copybook {name} exists more than once; {g.CopybooksByName[name]} is used.");
                continue;
            }
            g.CopybooksByName[name] = rel;
            g.Node(EstateNodeKind.Copybook, name, rel);
        }
    }

    private static IEnumerable<string> FilesWith(string root, IReadOnlyCollection<string> extensions) =>
        Directory.Exists(root)
            ? Directory.EnumerateFiles(root, "*", SearchOption.AllDirectories)
                .Where(f => extensions.Contains(Path.GetExtension(f), StringComparer.OrdinalIgnoreCase))
                .Where(f => !SourceTypeRegistry.IsScratchPath(Path.GetRelativePath(root, f)))
                .Order(StringComparer.Ordinal)
            : [];

    private static void AddMaps(Accumulator g, string sourceRoot, EstateGraphOptions options)
    {
        foreach (var file in FilesWith(sourceRoot, options.MapExtensions))
        {
            var rel = Path.GetRelativePath(sourceRoot, file).Replace('\\', '/');
            g.Node(EstateNodeKind.Map, Path.GetFileNameWithoutExtension(rel).ToUpperInvariant(), rel);
        }
    }

    private static readonly Regex CsdDefine = new(@"\bDEFINE\s+(TRANSACTION|FILE|PROGRAM)\s*\(\s*([A-Z0-9#@$]+)\s*\)",
        RegexOptions.IgnoreCase | RegexOptions.CultureInvariant);
    private static readonly Regex CsdProgram = new(@"\bPROGRAM\s*\(\s*([A-Z0-9#@$]+)\s*\)", RegexOptions.IgnoreCase | RegexOptions.CultureInvariant);
    private static readonly Regex CsdDsname = new(@"\bDSNAME\s*\(\s*([A-Z0-9#@$.]+)\s*\)", RegexOptions.IgnoreCase | RegexOptions.CultureInvariant);

    // The CSD is where CICS says which program a transaction starts and which dataset a file is.
    private static void AddCicsDefinitions(Accumulator g, string sourceRoot, EstateGraphOptions options)
    {
        foreach (var file in FilesWith(sourceRoot, options.CicsDefinitionExtensions))
        {
            var rel = Path.GetRelativePath(sourceRoot, file).Replace('\\', '/');
            var lines = File.ReadAllLines(file);
            var text = string.Join('\n', lines.Select(l => l.TrimStart().StartsWith('*') ? "" : l));
            var defines = CsdDefine.Matches(text).ToList();
            for (var i = 0; i < defines.Count; i++)
            {
                var m = defines[i];
                var end = i + 1 < defines.Count ? defines[i + 1].Index : text.Length;
                var body = text[(m.Index + m.Length)..end];
                var line = text[..m.Index].Count(c => c == '\n') + 1;
                var evidence = new EstateEvidence(rel, line, lines[line - 1].Trim());
                var kind = m.Groups[1].Value.ToUpperInvariant();
                var name = m.Groups[2].Value.ToUpperInvariant();
                if (kind == "TRANSACTION")
                {
                    var tx = g.Node(EstateNodeKind.Transaction, name, rel);
                    if (CsdProgram.Match(body) is { Success: true } p)
                        foreach (var target in g.ResolveProgram(p.Groups[1].Value, rel))
                            g.Edge(tx.Id, target, EstateEdgeKind.Runs, "CSD", [evidence]);
                }
                else if (kind == "FILE")
                {
                    var f = g.Node(EstateNodeKind.File, name, rel);
                    if (CsdDsname.Match(body) is { Success: true } d)
                        g.Edge(f.Id, g.Node(EstateNodeKind.Dataset, d.Groups[1].Value.ToUpperInvariant()).Id,
                            EstateEdgeKind.BackedBy, "CSD", [evidence]);
                }
            }
        }
    }

    private static readonly Regex YamlEntry = new(@"^(\s*)-\s+(transaction|file|program):\s*$", RegexOptions.IgnoreCase | RegexOptions.CultureInvariant);
    private static readonly Regex YamlAttribute = new(@"^\s+([A-Za-z_]+):\s*(.*?)\s*$", RegexOptions.CultureInvariant);

    // CICS resource definitions as a YAML list: '- transaction:' with name and program, '- file:' with
    // name and dsname. Read like the CSD; other YAML is left alone.
    private static void AddCicsYamlDefinitions(Accumulator g, string sourceRoot, EstateGraphOptions options)
    {
        var apiFiles = options.ApiOperationFileNames.Concat(options.ApiAssetFileNames).ToHashSet(StringComparer.OrdinalIgnoreCase);
        foreach (var file in FilesWith(sourceRoot, options.CicsYamlExtensions).Where(f => !apiFiles.Contains(Path.GetFileName(f))))
        {
            var rel = Path.GetRelativePath(sourceRoot, file).Replace('\\', '/');
            var lines = File.ReadAllLines(file);
            for (var i = 0; i < lines.Length; i++)
            {
                if (YamlEntry.Match(lines[i]) is not { Success: true } m) continue;
                var indent = m.Groups[1].Value.Length;
                var attrs = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
                var j = i + 1;
                for (; j < lines.Length; j++)
                {
                    var line = lines[j];
                    if (line.Trim().Length == 0 || line.TrimStart().StartsWith('#')) continue;
                    if (line.Length - line.TrimStart().Length <= indent) break;
                    if (YamlAttribute.Match(line) is { Success: true } a) attrs.TryAdd(a.Groups[1].Value, a.Groups[2].Value.Trim('"', '\''));
                }
                var evidence = new EstateEvidence(rel, i + 1, lines[i].Trim());
                var name = attrs.GetValueOrDefault("name")?.ToUpperInvariant();
                if (string.IsNullOrEmpty(name)) continue;
                switch (m.Groups[2].Value.ToLowerInvariant())
                {
                    case "transaction":
                        var tx = g.Node(EstateNodeKind.Transaction, name, rel);
                        if (attrs.GetValueOrDefault("description") is { Length: > 0 } d) tx.Attributes.TryAdd("description", d);
                        if (attrs.GetValueOrDefault("program") is { Length: > 0 } p)
                            foreach (var target in g.ResolveProgram(p.ToUpperInvariant(), rel))
                                g.Edge(tx.Id, target, EstateEdgeKind.Runs, "CICS definition", [evidence]);
                        break;
                    case "file":
                        var f = g.Node(EstateNodeKind.File, name, rel);
                        if (attrs.GetValueOrDefault("description") is { Length: > 0 } fd) f.Attributes.TryAdd("description", fd);
                        if (attrs.GetValueOrDefault("dsname") is { Length: > 0 } dsn)
                            g.Edge(f.Id, g.Node(EstateNodeKind.Dataset, dsn.ToUpperInvariant()).Id, EstateEdgeKind.BackedBy, "CICS definition", [evidence]);
                        break;
                }
                i = j - 1;
            }
        }
    }

    private static readonly Regex CreateTable = new(@"\bCREATE\s+TABLE\s+([A-Z0-9_#@$]+(?:\.[A-Z0-9_#@$]+)?)",
        RegexOptions.IgnoreCase | RegexOptions.CultureInvariant);

    // A table the DDL creates is in the source, so it is not mistaken for one only the code names.
    private static void AddDdlTables(Accumulator g, string sourceRoot, EstateGraphOptions options)
    {
        foreach (var file in FilesWith(sourceRoot, options.DdlExtensions))
        {
            var rel = Path.GetRelativePath(sourceRoot, file).Replace('\\', '/');
            foreach (Match m in CreateTable.Matches(File.ReadAllText(file)))
                g.Node(EstateNodeKind.Table, m.Groups[1].Value.ToUpperInvariant(), rel);
        }
    }

    private static readonly Regex ZAsset = new(@"\bzasset:\s*[""']?([A-Za-z0-9_#@$-]+)", RegexOptions.CultureInvariant);
    private static readonly Regex AssetProgram = new(@"^\s*program:\s*[""']?([A-Za-z0-9#@$-]+)", RegexOptions.Multiline | RegexOptions.CultureInvariant);

    // operations/<url-encoded path>/<method>/operation.yaml names an asset; zosAssets/<asset>/zosAsset.yaml
    // names the program behind it. Each operation becomes an API entry point that invokes that program.
    private static void AddApis(Accumulator g, string sourceRoot, EstateGraphOptions options)
    {
        if (!Directory.Exists(sourceRoot)) return;
        var opNames = options.ApiOperationFileNames.ToHashSet(StringComparer.OrdinalIgnoreCase);
        var assetNames = options.ApiAssetFileNames.ToHashSet(StringComparer.OrdinalIgnoreCase);
        var files = Directory.EnumerateFiles(sourceRoot, "*", SearchOption.AllDirectories)
            .Where(f => !SourceTypeRegistry.IsScratchPath(Path.GetRelativePath(sourceRoot, f)))
            .Order(StringComparer.Ordinal).ToList();

        var assets = new Dictionary<string, (string Program, string File)>(StringComparer.OrdinalIgnoreCase);
        foreach (var file in files.Where(f => assetNames.Contains(Path.GetFileName(f))))
            if (AssetProgram.Match(File.ReadAllText(file)) is { Success: true } p)
                assets.TryAdd(Path.GetFileName(Path.GetDirectoryName(file)!),
                    (p.Groups[1].Value.ToUpperInvariant(), Path.GetRelativePath(sourceRoot, file).Replace('\\', '/')));

        foreach (var file in files.Where(f => opNames.Contains(Path.GetFileName(f))))
        {
            var rel = Path.GetRelativePath(sourceRoot, file).Replace('\\', '/');
            var lines = File.ReadAllLines(file);
            var line = Array.FindIndex(lines, l => ZAsset.IsMatch(l));
            if (line < 0) continue;
            var asset = ZAsset.Match(lines[line]).Groups[1].Value;
            var methodDir = Path.GetDirectoryName(file)!;
            var method = Path.GetFileName(methodDir).ToUpperInvariant();
            var path = Uri.UnescapeDataString(Path.GetFileName(Path.GetDirectoryName(methodDir)!));
            var api = g.Node(EstateNodeKind.Api, $"{method} {path}", rel);
            api.Attributes["asset"] = asset.ToUpperInvariant();
            var program = assets.TryGetValue(asset, out var a) ? a.Program : asset.ToUpperInvariant();
            foreach (var target in g.ResolveProgram(program, rel))
                g.Edge(api.Id, target, EstateEdgeKind.Invokes, asset.ToUpperInvariant(), [new EstateEvidence(rel, line + 1, lines[line].Trim())]);
        }
    }

    private static void AddReferences(Accumulator g, ProgramScan scan)
    {
        var from = g.ProgramByFile[scan.RelativePath];
        foreach (var r in scan.References)
        {
            IReadOnlyList<string> targets = r.TargetKind switch
            {
                EstateNodeKind.Program => g.ResolveProgram(r.Target, scan.RelativePath),
                EstateNodeKind.Copybook => [g.CopybooksByName.ContainsKey(r.Target)
                    ? NodeId(EstateNodeKind.Copybook, r.Target)
                    : g.Node(EstateNodeKind.Copybook, r.Target, inSource: false).Id],
                // Maps and transactions are in the source only when their BMS or CSD definition is.
                EstateNodeKind.Map or EstateNodeKind.Transaction =>
                    [g.Nodes.ContainsKey(NodeId(r.TargetKind, r.Target))
                        ? NodeId(r.TargetKind, r.Target)
                        : g.Node(r.TargetKind, r.Target, inSource: false).Id],
                _ => [g.Node(r.TargetKind, r.Target).Id],
            };
            foreach (var to in targets) g.Edge(from, to, r.Kind, r.Via, r.Evidence);
        }
    }

    private static void AddJobs(Accumulator g, string jclRoot, IReadOnlyList<ProgramScan> scans)
    {
        IReadOnlyList<JclJob> jobs;
        try { jobs = JclEstate.Parse(jclRoot); }
        catch (Exception ex) when (ex is IOException or UnauthorizedAccessException)
        {
            g.Diagnostics.Add($"JCL in {jclRoot} could not be read: {ex.Message}");
            return;
        }

        var ddsByProgram = scans.ToDictionary(s => s.RelativePath, s => s.AssignedDds, StringComparer.Ordinal);
        foreach (var job in jobs)
        {
            var jobNode = g.Node(EstateNodeKind.Job, job.Name.ToUpperInvariant(), job.File);
            if (job.Diagnostics.Count > 0)
                jobNode.Attributes["diagnostics"] = job.Diagnostics.Count.ToString(System.Globalization.CultureInfo.InvariantCulture);
            jobNode.Attributes["steps"] = job.Steps.Count.ToString(System.Globalization.CultureInfo.InvariantCulture);

            foreach (var step in job.Steps)
            {
                var stepEvidence = new EstateEvidence(job.File, step.Line, $"//{step.Name} EXEC {step.Program ?? step.Procedure}".Trim());
                var programs = step.ProgramsRun
                    .SelectMany(p => g.ResolveProgram(p, job.File))
                    .Distinct(StringComparer.Ordinal).ToList();
                foreach (var program in programs)
                    g.Edge(jobNode.Id, program, EstateEdgeKind.Runs, step.Name, [stepEvidence]);

                foreach (var dd in step.Dds)
                {
                    if (dd.Dataset is not { Temporary: false } ds || JclEstateLineage.IsLibraryDd(dd.Name)) continue;
                    var kind = AccessKind(dd.Access);
                    if (kind is null) continue;
                    var dsNode = g.Node(EstateNodeKind.Dataset, ds.Name.ToUpperInvariant());
                    var ddEvidence = new EstateEvidence(job.File, dd.Line, $"//{dd.Name} DD DSN={ds.Name}");
                    g.Edge(jobNode.Id, dsNode.Id, kind, $"{step.Name}.{dd.Name}", [ddEvidence]);

                    // The program's own SELECT ... ASSIGN says which DDs it opens; without one, the
                    // step's DDs are the best evidence of what it touches.
                    var ddName = dd.Name.Split('.')[^1];
                    foreach (var program in programs)
                    {
                        var file = g.Nodes[program].File;
                        if (file is not null && ddsByProgram.TryGetValue(file, out var assigned)
                            && assigned.Count > 0 && !assigned.Contains(ddName)) continue;
                        g.Edge(program, dsNode.Id, kind, $"{job.Name}.{step.Name} DD {ddName}", [ddEvidence]);
                    }
                }

                foreach (var effect in step.Effects.Where(e => !e.Dataset.Temporary))
                {
                    var kind = AccessKind(effect.Access);
                    if (kind is null) continue;
                    g.Edge(jobNode.Id, g.Node(EstateNodeKind.Dataset, effect.Dataset.Name.ToUpperInvariant()).Id, kind,
                        $"{step.Name} {effect.Source}", [stepEvidence]);
                }
            }

            foreach (var d in job.Diagnostics)
                g.Diagnostics.Add($"{job.File}:{d.Line} {d.Message}");
        }

        // The evidence for "A feeds B" is the DD lines where A writes and B reads the shared datasets.
        foreach (var dep in JclEstateLineage.Build(jobs).Dependencies)
        {
            var (up, down) = (NodeId(EstateNodeKind.Job, dep.Upstream.ToUpperInvariant()), NodeId(EstateNodeKind.Job, dep.Downstream.ToUpperInvariant()));
            var evidence = dep.Datasets.SelectMany(ds => g.Edges.Values
                    .Where(e => e.To == NodeId(EstateNodeKind.Dataset, ds) && (e.From == up || e.From == down))
                    .OrderBy(e => e.From == up ? 0 : 1)
                    .SelectMany(e => e.Evidence))
                .ToList();
            g.Edge(up, down, EstateEdgeKind.Feeds, string.Join(", ", dep.Datasets), evidence);
        }
    }

    private static string? AccessKind(JclDatasetAccess? access) => access switch
    {
        JclDatasetAccess.Read => EstateEdgeKind.Reads,
        JclDatasetAccess.Create or JclDatasetAccess.Append => EstateEdgeKind.Writes,
        JclDatasetAccess.Exclusive => EstateEdgeKind.Updates,
        JclDatasetAccess.Delete => EstateEdgeKind.Deletes,
        _ => null,
    };
}
