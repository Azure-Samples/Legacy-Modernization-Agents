using CobolToQuarkusMigration.Agents;
using CobolToQuarkusMigration.Estate;

namespace McpChatWeb.Services;

// What the portal knows about a program beyond the graph: how well REKT parsed it and how the latest
// conversion of it scored on parity.
public sealed record EstateProgramStatus(
    string? ParseFidelity,
    int? FactsConfidence,
    IReadOnlyList<EstateParityStatus> Parity);

public sealed record EstateParityStatus(string TargetLanguage, string Outcome, double? Score, double Threshold);

public sealed record EstateSummary(
    DateTime GeneratedAtUtc,
    IReadOnlyDictionary<string, int> Counts,
    IReadOnlyList<EstateHub> Hubs,
    IReadOnlyList<EstateCluster> Clusters,
    IReadOnlyList<EstateWave> Waves,
    IReadOnlyList<string> Diagnostics,
    int DiagnosticCount,
    IReadOnlyDictionary<string, EstateProgramStatus> Programs,
    IReadOnlyDictionary<string, string> ClusterOf,
    string? Warning);

public sealed record EstateGraphView(IReadOnlyList<EstateNode> Nodes, IReadOnlyList<EstateEdgeView> Edges);

public sealed record EstateEdgeView(string From, string To, string Kind, string? Via, int EvidenceCount);

public sealed record EstateNodeDetail(
    EstateNode Node,
    string? Cluster,
    EstateProgramStatus? Status,
    IReadOnlyList<EstateEdge> Outgoing,
    IReadOnlyList<EstateEdge> Incoming);

public sealed record EstateSliceView(EstateSlice Slice, string Command, IReadOnlyList<string> Selectors);

// Builds the estate graph from the same source folder the conversion reads, and rebuilds it only
// when that source changes. Deterministic: no model is involved.
public sealed class EstateGraphService
{
    public const int MaxDiagnosticsInSummary = 50;

    private readonly RektEstateReader _rekt;
    private readonly ConversionParityReader _parity;
    private readonly ILogger<EstateGraphService> _logger;
    private readonly SemaphoreSlim _lock = new(1, 1);
    private (string Stamp, EstateGraph Graph, string? Warning)? _cache;

    public EstateGraphService(RektEstateReader rekt, ConversionParityReader parity, ILogger<EstateGraphService> logger)
    {
        _rekt = rekt;
        _parity = parity;
        _logger = logger;
    }

    public string SourceRoot => _rekt.SourceRoot;

    // The source folder as a run takes it: relative to the repository, so selectors resolve against
    // the same files the graph was built from.
    public string SourceFolderForRun => Path.GetRelativePath(_rekt.RepoRoot, SourceRoot).Replace('\\', '/');

    // JCL_SOURCE_FOLDER, as doctor.sh reads it; otherwise the JCL lives with the COBOL.
    public string? JclRoot
    {
        get
        {
            var folder = Environment.GetEnvironmentVariable("JCL_SOURCE_FOLDER");
            if (string.IsNullOrWhiteSpace(folder)) return null;
            var path = Path.IsPathRooted(folder) ? folder : Path.Join(_rekt.RepoRoot, folder);
            return Directory.Exists(path) ? path : null;
        }
    }

    public async Task<(EstateGraph Graph, string? Warning)> GetGraphAsync(CancellationToken ct = default)
    {
        if (!Directory.Exists(SourceRoot))
            return (new EstateGraph { GeneratedAtUtc = DateTime.UtcNow }, $"No source folder at {_rekt.SourceFolderName}.");

        var stamp = Stamp(SourceRoot, JclRoot);
        if (_cache is { } hit && hit.Stamp == stamp) return (hit.Graph, hit.Warning);

        await _lock.WaitAsync(ct);
        try
        {
            if (_cache is { } again && again.Stamp == stamp) return (again.Graph, again.Warning);
            var options = EstateGraphOptions.Load(Path.Join(_rekt.RepoRoot, "Config", "appsettings.json"), out var warning);
            if (warning is not null) _logger.LogWarning("{Warning}", warning);
            var graph = await Task.Run(() => EstateGraphBuilder.Build(SourceRoot, JclRoot, options), ct);
            _cache = (stamp, graph, warning);
            return (graph, warning);
        }
        finally
        {
            _lock.Release();
        }
    }

    // Count, total size and newest write time of every file: cheap, and any edit, add or delete changes it.
    internal static string Stamp(string sourceRoot, string? jclRoot)
    {
        long count = 0, size = 0, newest = 0;
        foreach (var root in new[] { sourceRoot, jclRoot }.OfType<string>().Distinct())
            foreach (var f in new DirectoryInfo(root).EnumerateFiles("*", SearchOption.AllDirectories))
            {
                count++;
                size += f.Length;
                newest = Math.Max(newest, f.LastWriteTimeUtc.Ticks);
            }
        return $"{count}:{size}:{newest}";
    }

    public async Task<EstateSummary> GetSummaryAsync(CancellationToken ct = default)
    {
        var (graph, warning) = await GetGraphAsync(ct);
        var status = await ProgramStatusAsync(graph, ct);
        var clusterOf = graph.Clusters.SelectMany(c => c.Programs.Select(p => (p, c.Id)))
            .ToDictionary(x => x.p, x => x.Id, StringComparer.Ordinal);
        return new EstateSummary(graph.GeneratedAtUtc, graph.Counts, graph.Hubs, graph.Clusters, graph.Waves,
            graph.Diagnostics.Take(MaxDiagnosticsInSummary).ToList(), graph.Diagnostics.Count, status, clusterOf, warning);
    }

    // The whole graph, or one cluster with everything one step away from it; evidence is left to
    // the node endpoint so the payload stays small.
    public async Task<EstateGraphView?> GetGraphViewAsync(string? clusterId, CancellationToken ct = default)
    {
        var (graph, _) = await GetGraphAsync(ct);
        IEnumerable<EstateEdge> edges = graph.Edges;
        if (!string.IsNullOrEmpty(clusterId))
        {
            var cluster = graph.Clusters.FirstOrDefault(c => c.Id.Equals(clusterId, StringComparison.OrdinalIgnoreCase));
            if (cluster is null) return null;
            var members = cluster.Programs.ToHashSet(StringComparer.Ordinal);
            edges = graph.Edges.Where(e => members.Contains(e.From) || members.Contains(e.To));
        }
        var list = edges.ToList();
        var ids = list.SelectMany(e => new[] { e.From, e.To }).ToHashSet(StringComparer.Ordinal);
        if (!string.IsNullOrEmpty(clusterId))
            foreach (var p in graph.Clusters.First(c => c.Id.Equals(clusterId, StringComparison.OrdinalIgnoreCase)).Programs) ids.Add(p);
        else
            foreach (var n in graph.Nodes) ids.Add(n.Id);
        return new EstateGraphView(
            graph.Nodes.Where(n => ids.Contains(n.Id)).ToList(),
            list.Select(e => new EstateEdgeView(e.From, e.To, e.Kind, e.Via, e.Evidence.Count)).ToList());
    }

    public async Task<EstateNodeDetail?> GetNodeAsync(string id, CancellationToken ct = default)
    {
        var (graph, _) = await GetGraphAsync(ct);
        var node = graph.Nodes.FirstOrDefault(n => n.Id.Equals(id, StringComparison.OrdinalIgnoreCase));
        if (node is null) return null;
        var status = node.Kind == EstateNodeKind.Program ? (await ProgramStatusAsync(graph, ct)).GetValueOrDefault(node.Id) : null;
        var cluster = graph.Clusters.FirstOrDefault(c => c.Programs.Contains(node.Id))?.Id;
        return new EstateNodeDetail(node, cluster, status,
            graph.Edges.Where(e => e.From == node.Id).ToList(),
            graph.Edges.Where(e => e.To == node.Id).ToList());
    }

    public async Task<EstateSliceView?> GetSliceAsync(string clusterId, bool includeNeeds = true, CancellationToken ct = default)
    {
        var (graph, _) = await GetGraphAsync(ct);
        var slice = EstateAnalysis.Slice(graph, clusterId);
        if (slice is null) return null;
        var selectors = includeNeeds ? slice.ProgramSelectors.Concat(slice.NeedSelectors).ToList() : slice.ProgramSelectors.ToList();
        return new EstateSliceView(slice, $"./doctor.sh convert-only --program {string.Join(',', selectors)}", selectors);
    }

    private async Task<Dictionary<string, EstateProgramStatus>> ProgramStatusAsync(EstateGraph graph, CancellationToken ct)
    {
        Dictionary<string, RektProgramRecord> rekt = new(StringComparer.OrdinalIgnoreCase);
        List<ConversionParityReport> parity = [];
        try
        {
            var estate = await _rekt.ReadAsync(ct);
            foreach (var p in estate.Programs.Where(p => !p.IsCopybook))
                rekt[p.RelativePath.Replace('\\', '/')] = p;
            parity = (await _parity.ReadAsync(ct)).Reports;
        }
        catch (Exception ex) when (ex is not OperationCanceledException)
        {
            _logger.LogWarning(ex, "Estate graph enrichment unavailable");
        }

        var result = new Dictionary<string, EstateProgramStatus>(StringComparer.Ordinal);
        foreach (var node in graph.Nodes.Where(n => n.Kind == EstateNodeKind.Program && n.InSource && n.File is not null))
        {
            var stem = Path.GetFileNameWithoutExtension(node.File!);
            var r = rekt.GetValueOrDefault(node.File!);
            var scores = parity
                .SelectMany(rep => rep.Programs
                    .Where(p => Path.GetFileNameWithoutExtension(p.Program).Equals(stem, StringComparison.OrdinalIgnoreCase))
                    .Select(p => new EstateParityStatus(rep.TargetLanguage, p.Outcome.ToString(), p.Score, rep.Threshold)))
                .ToList();
            result[node.Id] = new EstateProgramStatus(r?.ParseFidelity, r?.HasFacts == true ? r.FactsConfidence : null, scores);
        }
        return result;
    }
}
