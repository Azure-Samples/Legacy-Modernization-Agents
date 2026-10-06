namespace CobolToQuarkusMigration.Estate;

public sealed record EstateAnalysisResult(
    IReadOnlyList<EstateHub> Hubs,
    IReadOnlyList<EstateCluster> Clusters,
    IReadOnlyList<EstateWave> Waves);

// Turns the graph into a migration plan: which nodes everything leans on (hubs), which programs
// belong together (clusters), how cleanly each cluster comes away (carve score), and in which order
// (waves, callees first so a converted caller never points at an unconverted callee).
public static class EstateAnalysis
{
    public const string StandaloneClusterId = "standalone";

    private static readonly HashSet<string> ProgramToProgram = [EstateEdgeKind.Calls, EstateEdgeKind.Links];
    private static readonly HashSet<string> WriteKinds = [EstateEdgeKind.Writes, EstateEdgeKind.Updates, EstateEdgeKind.Deletes];
    private static readonly HashSet<string> SharedResourceKinds =
        [EstateNodeKind.Table, EstateNodeKind.Dataset, EstateNodeKind.File, EstateNodeKind.Copybook];

    public static EstateAnalysisResult Analyze(IReadOnlyList<EstateNode> nodes, IReadOnlyList<EstateEdge> edges, EstateGraphOptions options)
    {
        var byId = nodes.ToDictionary(n => n.Id, StringComparer.Ordinal);
        var hubs = FindHubs(nodes, edges, byId, options);
        var hubIds = hubs.Select(h => h.NodeId).ToHashSet(StringComparer.Ordinal);

        var programs = nodes.Where(n => n.Kind == EstateNodeKind.Program && n.InSource)
            .Select(n => n.Id).Order(StringComparer.Ordinal).ToList();
        var index = programs.Select((id, i) => (id, i)).ToDictionary(x => x.id, x => x.i, StringComparer.Ordinal);
        var adj = Coupling(programs, index, edges, byId, hubIds, options);

        var community = Louvain(programs.Count, adj, options.LouvainResolution);
        var clusters = BuildClusters(programs, index, adj, community, edges, byId, hubIds, options);
        var (waves, waveOf) = Waves(clusters);
        clusters = clusters.Select(c => c with { Wave = waveOf[c.Id] })
            .OrderBy(c => c.Wave).ThenByDescending(c => c.CarveScore).ThenBy(c => c.Id, StringComparer.Ordinal).ToList();
        return new EstateAnalysisResult(hubs, clusters, waves);
    }

    // ---- hubs ---------------------------------------------------------------------------------

    private static List<EstateHub> FindHubs(IReadOnlyList<EstateNode> nodes, IReadOnlyList<EstateEdge> edges,
        Dictionary<string, EstateNode> byId, EstateGraphOptions options)
    {
        var fanIn = new Dictionary<string, HashSet<string>>(StringComparer.Ordinal);
        var fanOut = new Dictionary<string, HashSet<string>>(StringComparer.Ordinal);
        foreach (var e in edges)
        {
            // A program's degree is its call graph; a shared resource's is how many things use it.
            var counts = ProgramToProgram.Contains(e.Kind) || SharedResourceKinds.Contains(byId[e.To].Kind);
            if (!counts) continue;
            Add(fanIn, e.To, e.From);
            if (ProgramToProgram.Contains(e.Kind)) Add(fanOut, e.From, e.To);
        }

        var hubs = new List<EstateHub>();
        foreach (var kind in nodes.GroupBy(n => n.Kind).Where(g => g.Key == EstateNodeKind.Program || SharedResourceKinds.Contains(g.Key)))
        {
            var degrees = kind.Select(n => (Node: n, In: Count(fanIn, n.Id), Out: Count(fanOut, n.Id)))
                .Where(d => d.In + d.Out > 0).ToList();
            if (degrees.Count == 0) continue;
            var threshold = Math.Max(options.HubMinDegree, Percentile(degrees.Select(d => d.In + d.Out).ToList(), options.HubPercentile));
            hubs.AddRange(degrees.Where(d => d.In + d.Out >= threshold)
                .Select(d => new EstateHub(d.Node.Id, d.Node.Kind, d.Node.Name, d.In, d.Out)));
        }
        return hubs.OrderByDescending(h => h.FanIn + h.FanOut).ThenBy(h => h.NodeId, StringComparer.Ordinal).ToList();

        static int Count(Dictionary<string, HashSet<string>> d, string id) => d.TryGetValue(id, out var s) ? s.Count : 0;
    }

    private static void Add(Dictionary<string, HashSet<string>> d, string key, string value)
    {
        if (!d.TryGetValue(key, out var set)) d[key] = set = new(StringComparer.Ordinal);
        set.Add(value);
    }

    internal static double Percentile(IReadOnlyList<int> values, double p)
    {
        var sorted = values.Order().ToList();
        var rank = (int)Math.Ceiling(Math.Clamp(p, 0, 1) * sorted.Count) - 1;
        return sorted[Math.Clamp(rank, 0, sorted.Count - 1)];
    }

    // ---- coupling -------------------------------------------------------------------------------

    private static Dictionary<int, Dictionary<int, double>> Coupling(List<string> programs, Dictionary<string, int> index,
        IReadOnlyList<EstateEdge> edges, Dictionary<string, EstateNode> byId, HashSet<string> hubIds, EstateGraphOptions options)
    {
        var adj = new Dictionary<int, Dictionary<int, double>>();
        for (var i = 0; i < programs.Count; i++) adj[i] = [];

        void Link(string a, string b, double w)
        {
            if (w <= 0 || a == b || !index.TryGetValue(a, out var i) || !index.TryGetValue(b, out var j)) return;
            adj[i][j] = adj[i].GetValueOrDefault(j) + w;
            adj[j][i] = adj[j].GetValueOrDefault(i) + w;
        }

        double HubScaled(string a, string b, double w) =>
            hubIds.Contains(a) || hubIds.Contains(b) ? w * options.HubEdgeFactor : w;

        var txPrograms = edges.Where(e => e.Kind == EstateEdgeKind.Runs && byId[e.From].Kind == EstateNodeKind.Transaction)
            .GroupBy(e => e.From).ToDictionary(g => g.Key, g => g.Select(e => e.To).ToList(), StringComparer.Ordinal);

        foreach (var e in edges)
        {
            if (ProgramToProgram.Contains(e.Kind))
                Link(e.From, e.To, HubScaled(e.From, e.To, options.Coupling("call")));
            else if (e.Kind == EstateEdgeKind.Starts && txPrograms.TryGetValue(e.To, out var started))
                foreach (var p in started) Link(e.From, p, HubScaled(e.From, p, options.Coupling("call")));
        }

        // Programs one job runs: the job is the unit operations schedules, so they move together.
        foreach (var job in edges.Where(e => e.Kind == EstateEdgeKind.Runs && byId[e.From].Kind == EstateNodeKind.Job).GroupBy(e => e.From))
            Pairs(job.Select(e => e.To).Where(index.ContainsKey).Distinct().Order(StringComparer.Ordinal).ToList(),
                (a, b, n) => Link(a, b, options.Coupling("sameJob") / (n - 1)));

        // Shared data and copybooks, normalised by how many programs share them so a popular table
        // does not outweigh a direct call. A hub resource is shared infrastructure and adds nothing.
        foreach (var resource in edges.Where(e => index.ContainsKey(e.From) && SharedResourceKinds.Contains(byId[e.To].Kind)
                                                  && !hubIds.Contains(e.To)).GroupBy(e => e.To))
        {
            var kind = byId[resource.Key].Kind;
            var writers = resource.Where(e => WriteKinds.Contains(e.Kind)).Select(e => e.From).ToHashSet(StringComparer.Ordinal);
            var users = resource.Select(e => e.From).Distinct().Order(StringComparer.Ordinal).ToList();
            Pairs(users, (a, b, n) =>
            {
                var anyWriter = writers.Contains(a) || writers.Contains(b);
                var weight = kind switch
                {
                    EstateNodeKind.Copybook => options.Coupling("sharedCopybook"),
                    EstateNodeKind.Dataset => anyWriter ? options.Coupling("datasetFlow") : 0,
                    _ => anyWriter ? options.Coupling("sharedTableWrite") : options.Coupling("sharedTableRead"),
                };
                Link(a, b, weight / (n - 1));
            });
        }

        return adj;
    }

    private static void Pairs(IReadOnlyList<string> items, Action<string, string, int> action)
    {
        if (items.Count < 2) return;
        for (var i = 0; i < items.Count; i++)
            for (var j = i + 1; j < items.Count; j++)
                action(items[i], items[j], items.Count);
    }

    // ---- Louvain --------------------------------------------------------------------------------

    // Louvain modularity clustering, made deterministic: nodes are visited in index order, a move
    // must strictly improve modularity, and ties go to the lower community number.
    internal static int[] Louvain(int n, Dictionary<int, Dictionary<int, double>> adj, double resolution, int maxLevels = 32)
    {
        var membership = Enumerable.Range(0, n).ToArray();
        var graph = Enumerable.Range(0, n).Select(i => new SortedDictionary<int, double>(adj.GetValueOrDefault(i) ?? [])).ToList();

        for (var level = 0; level < maxLevels; level++)
        {
            var m = graph.Count;
            var k = graph.Select(row => row.Values.Sum()).ToArray();
            var m2 = k.Sum();
            if (m2 <= 0) break;

            var comm = Enumerable.Range(0, m).ToArray();
            var tot = (double[])k.Clone();
            var improved = false;

            for (var pass = 0; pass < 1000; pass++)
            {
                var moved = false;
                for (var i = 0; i < m; i++)
                {
                    var ci = comm[i];
                    var toComm = new SortedDictionary<int, double>();
                    foreach (var (j, w) in graph[i])
                        if (j != i) toComm[comm[j]] = toComm.GetValueOrDefault(comm[j]) + w;

                    tot[ci] -= k[i];
                    var best = ci;
                    var bestGain = toComm.GetValueOrDefault(ci) - resolution * tot[ci] * k[i] / m2;
                    foreach (var (c, w) in toComm)
                    {
                        var gain = w - resolution * tot[c] * k[i] / m2;
                        if (gain > bestGain + 1e-12) (best, bestGain) = (c, gain);
                    }
                    tot[best] += k[i];
                    if (best != ci) { comm[i] = best; moved = improved = true; }
                }
                if (!moved) break;
            }
            if (!improved) break;

            var renumber = new Dictionary<int, int>();
            foreach (var c in comm) if (!renumber.ContainsKey(c)) renumber[c] = renumber.Count;
            for (var v = 0; v < n; v++) membership[v] = renumber[comm[membership[v]]];

            var next = Enumerable.Range(0, renumber.Count).Select(_ => new SortedDictionary<int, double>()).ToList();
            for (var i = 0; i < m; i++)
                foreach (var (j, w) in graph[i])
                {
                    var (a, b) = (renumber[comm[i]], renumber[comm[j]]);
                    next[a][b] = next[a].GetValueOrDefault(b) + w;
                }
            graph = next;
        }
        return membership;
    }

    // ---- clusters -------------------------------------------------------------------------------

    private static List<EstateCluster> BuildClusters(List<string> programs, Dictionary<string, int> index,
        Dictionary<int, Dictionary<int, double>> adj, int[] community, IReadOnlyList<EstateEdge> edges,
        Dictionary<string, EstateNode> byId, HashSet<string> hubIds, EstateGraphOptions options)
    {
        var standalone = programs.Where(p => adj[index[p]].Count == 0).ToList();
        var groups = programs.Where(p => adj[index[p]].Count > 0)
            .GroupBy(p => community[index[p]])
            .Select(g => g.Order(StringComparer.Ordinal).ToList())
            .OrderByDescending(g => g.Count).ThenBy(g => g[0], StringComparer.Ordinal)
            .ToList();

        var clusterOf = new Dictionary<string, string>(StringComparer.Ordinal);
        var width = Math.Max(2, groups.Count.ToString(System.Globalization.CultureInfo.InvariantCulture).Length);
        var named = groups.Select((g, i) => (Id: "C" + (i + 1).ToString(System.Globalization.CultureInfo.InvariantCulture).PadLeft(width, '0'), Members: g, Standalone: false)).ToList();
        if (standalone.Count > 0) named.Add((StandaloneClusterId, standalone, true));
        foreach (var (id, members, _) in named) foreach (var p in members) clusterOf[p] = id;

        var outgoing = edges.GroupBy(e => e.From).ToDictionary(g => g.Key, g => g.ToList(), StringComparer.Ordinal);
        var incoming = edges.GroupBy(e => e.To).ToDictionary(g => g.Key, g => g.ToList(), StringComparer.Ordinal);
        var txPrograms = edges.Where(e => e.Kind == EstateEdgeKind.Runs && byId[e.From].Kind == EstateNodeKind.Transaction)
            .GroupBy(e => e.From).ToDictionary(g => g.Key, g => g.Select(e => e.To).ToList(), StringComparer.Ordinal);

        var clusters = new List<EstateCluster>();
        foreach (var (id, members, isStandalone) in named)
        {
            var set = members.ToHashSet(StringComparer.Ordinal);
            double internalW = 0, externalW = 0;
            foreach (var p in members)
                foreach (var (j, w) in adj[index[p]])
                {
                    if (set.Contains(programs[j])) internalW += w / 2;
                    else externalW += w;
                }

            var refs = members.SelectMany(p => outgoing.GetValueOrDefault(p) ?? []).ToList();
            var callTargets = refs.Where(e => ProgramToProgram.Contains(e.Kind)).Select(e => e.To)
                .Concat(refs.Where(e => e.Kind == EstateEdgeKind.Starts).SelectMany(e => txPrograms.GetValueOrDefault(e.To) ?? []))
                .Where(t => byId[t].InSource).Distinct().ToList();
            var outside = callTargets.Where(t => !set.Contains(t)).ToList();
            var codeRefs = refs.Where(e => ProgramToProgram.Contains(e.Kind) || e.Kind is EstateEdgeKind.Copies or EstateEdgeKind.UsesMap or EstateEdgeKind.Starts)
                .Select(e => e.To).Distinct().ToList();
            var missing = codeRefs.Where(t => !byId[t].InSource).Select(t => byId[t].Name).Order(StringComparer.Ordinal).ToList();

            var cohesion = internalW + externalW == 0 ? 1 : internalW / (internalW + externalW);
            var completeness = codeRefs.Count == 0 ? 1 : 1 - (double)missing.Count / codeRefs.Count;
            var independence = callTargets.Count == 0 ? 1 : 1 - (double)outside.Count / callTargets.Count;
            var size = members.Count <= options.TargetSliceSize ? 1 : (double)options.TargetSliceSize / members.Count;
            var breakdown = new SortedDictionary<string, double>(StringComparer.Ordinal)
            {
                ["cohesion"] = Math.Round(cohesion, 3),
                ["completeness"] = Math.Round(completeness, 3),
                ["independence"] = Math.Round(independence, 3),
                ["size"] = Math.Round(size, 3),
            };
            var totalWeight = breakdown.Keys.Sum(options.Carve);
            var score = totalWeight <= 0 ? 0 : 100 * breakdown.Sum(kv => options.Carve(kv.Key) * kv.Value) / totalWeight;

            var jobs = members.SelectMany(p => incoming.GetValueOrDefault(p) ?? [])
                .Where(e => e.Kind == EstateEdgeKind.Runs && byId[e.From].Kind == EstateNodeKind.Job)
                .Select(e => byId[e.From].Name).Distinct().Order(StringComparer.Ordinal).ToList();
            List<string> Touched(string kind) => refs.Where(e => byId[e.To].Kind == kind)
                .Select(e => byId[e.To].Name).Distinct().Order(StringComparer.Ordinal).ToList();

            clusters.Add(new EstateCluster
            {
                Id = id,
                Label = isStandalone ? "Standalone programs" : Label(members, adj, index, byId),
                Programs = members,
                Jobs = jobs,
                Tables = Touched(EstateNodeKind.Table),
                Datasets = Touched(EstateNodeKind.Dataset),
                Hubs = members.Where(hubIds.Contains).ToList(),
                InternalWeight = Math.Round(internalW, 3),
                ExternalWeight = Math.Round(externalW, 3),
                DependsOn = outside.Select(t => clusterOf[t]).Where(c => c != id).Distinct().Order(StringComparer.Ordinal).ToList(),
                Missing = missing,
                Standalone = isStandalone,
                CarveScore = Math.Round(score, 1),
                ScoreBreakdown = breakdown,
            });
        }

        var dependents = clusters.SelectMany(c => c.DependsOn.Select(d => (From: c.Id, To: d)))
            .GroupBy(x => x.To).ToDictionary(g => g.Key, g => g.Select(x => x.From).Order(StringComparer.Ordinal).ToList());
        return clusters.Select(c => c with { DependedOnBy = dependents.GetValueOrDefault(c.Id) ?? [] }).ToList();
    }

    // The shared name prefix when members follow a naming scheme, else the most coupled member.
    private static string Label(List<string> members, Dictionary<int, Dictionary<int, double>> adj,
        Dictionary<string, int> index, Dictionary<string, EstateNode> byId)
    {
        var names = members.Select(m => Path.GetFileNameWithoutExtension(byId[m].Name)).ToList();
        var prefix = names.Aggregate((a, b) => new string(a.Zip(b).TakeWhile(p => p.First == p.Second).Select(p => p.First).ToArray()));
        if (names.Count > 1 && prefix.Length >= 3) return prefix + "*";
        var top = members.OrderByDescending(m => adj[index[m]].Values.Sum()).ThenBy(m => m, StringComparer.Ordinal).First();
        return names.Count > 1 ? $"{Path.GetFileNameWithoutExtension(byId[top].Name)} +{names.Count - 1}" : names[0];
    }

    // ---- waves ----------------------------------------------------------------------------------

    // Strongly connected clusters share a wave; otherwise a cluster comes one wave after the last
    // cluster it calls into.
    private static (List<EstateWave>, Dictionary<string, int>) Waves(List<EstateCluster> clusters)
    {
        var ids = clusters.Select(c => c.Id).Order(StringComparer.Ordinal).ToList();
        var deps = clusters.ToDictionary(c => c.Id, c => c.DependsOn, StringComparer.Ordinal);
        var sccs = Tarjan(ids, deps);
        var sccOf = new Dictionary<string, int>(StringComparer.Ordinal);
        for (var i = 0; i < sccs.Count; i++) foreach (var c in sccs[i]) sccOf[c] = i;

        // Tarjan emits a component only after every component it reaches, so one pass suffices.
        var waveOfScc = new int[sccs.Count];
        for (var i = 0; i < sccs.Count; i++)
        {
            var reached = sccs[i].SelectMany(c => deps[c]).Select(d => sccOf[d]).Where(s => s != i).ToList();
            waveOfScc[i] = reached.Count == 0 ? 1 : reached.Max(s => waveOfScc[s]) + 1;
        }

        var waveOf = ids.ToDictionary(id => id, id => waveOfScc[sccOf[id]], StringComparer.Ordinal);
        var waves = waveOf.GroupBy(kv => kv.Value).OrderBy(g => g.Key)
            .Select(g => new EstateWave(g.Key,
                g.Select(kv => kv.Key).OrderByDescending(id => clusters.First(c => c.Id == id).CarveScore).ThenBy(id => id, StringComparer.Ordinal).ToList(),
                sccs.Where(s => s.Count > 1 && waveOf[s[0]] == g.Key).Select(s => (IReadOnlyList<string>)s.Order(StringComparer.Ordinal).ToList()).ToList()))
            .ToList();
        return (waves, waveOf);
    }

    private static List<List<string>> Tarjan(List<string> ids, Dictionary<string, IReadOnlyList<string>> deps)
    {
        var index = 0;
        var indices = new Dictionary<string, int>(StringComparer.Ordinal);
        var low = new Dictionary<string, int>(StringComparer.Ordinal);
        var stack = new Stack<string>();
        var onStack = new HashSet<string>(StringComparer.Ordinal);
        var result = new List<List<string>>();

        void Visit(string v)
        {
            indices[v] = low[v] = index++;
            stack.Push(v);
            onStack.Add(v);
            foreach (var w in deps[v])
            {
                if (!indices.ContainsKey(w)) { Visit(w); low[v] = Math.Min(low[v], low[w]); }
                else if (onStack.Contains(w)) low[v] = Math.Min(low[v], indices[w]);
            }
            if (low[v] != indices[v]) return;
            var scc = new List<string>();
            string x;
            do { x = stack.Pop(); onStack.Remove(x); scc.Add(x); } while (x != v);
            result.Add(scc);
        }

        foreach (var id in ids) if (!indices.ContainsKey(id)) Visit(id);
        return result;
    }

    // ---- slices ---------------------------------------------------------------------------------

    public static EstateSlice? Slice(EstateGraph graph, string clusterId)
    {
        var cluster = graph.Clusters.FirstOrDefault(c => c.Id.Equals(clusterId, StringComparison.OrdinalIgnoreCase));
        return cluster is null ? null : SliceOf(graph, cluster.Id, cluster.Programs);
    }

    public static EstateSlice SliceOf(EstateGraph graph, string id, IReadOnlyList<string> programIds)
    {
        var byId = graph.Nodes.ToDictionary(n => n.Id, StringComparer.Ordinal);
        var outgoing = graph.Edges.GroupBy(e => e.From).ToDictionary(g => g.Key, g => g.ToList(), StringComparer.Ordinal);
        var txPrograms = graph.Edges.Where(e => e.Kind == EstateEdgeKind.Runs && byId[e.From].Kind == EstateNodeKind.Transaction)
            .GroupBy(e => e.From).ToDictionary(g => g.Key, g => g.Select(e => e.To).ToList(), StringComparer.Ordinal);

        var members = programIds.Where(byId.ContainsKey).ToHashSet(StringComparer.Ordinal);
        var needs = new SortedSet<string>(StringComparer.Ordinal);
        var missing = new SortedSet<string>(StringComparer.Ordinal);
        var queue = new Queue<string>(members.Order(StringComparer.Ordinal));
        var seen = new HashSet<string>(members, StringComparer.Ordinal);
        while (queue.Count > 0)
        {
            var p = queue.Dequeue();
            foreach (var e in outgoing.GetValueOrDefault(p) ?? [])
            {
                IEnumerable<string> targets = ProgramToProgram.Contains(e.Kind) ? [e.To]
                    : e.Kind == EstateEdgeKind.Starts ? txPrograms.GetValueOrDefault(e.To) ?? []
                    : [];
                if (e.Kind is EstateEdgeKind.Copies or EstateEdgeKind.UsesMap && !byId[e.To].InSource) missing.Add(byId[e.To].Name);
                foreach (var t in targets)
                {
                    if (!byId[t].InSource) { missing.Add(byId[t].Name); continue; }
                    if (seen.Add(t)) { needs.Add(t); queue.Enqueue(t); }
                }
            }
        }

        // A job can run once every program it runs is in the slice or what the slice needs.
        var covered = new HashSet<string>(members.Concat(needs), StringComparer.Ordinal);
        var jobs = graph.Edges.Where(e => e.Kind == EstateEdgeKind.Runs && byId[e.From].Kind == EstateNodeKind.Job)
            .GroupBy(e => e.From)
            .Where(g => g.Any(e => members.Contains(e.To)) && g.All(e => covered.Contains(e.To)))
            .Select(g => byId[g.Key].Name).Order(StringComparer.Ordinal).ToList();

        var basenames = graph.Nodes.Where(n => n.Kind == EstateNodeKind.Program && n.File is not null)
            .GroupBy(n => Path.GetFileName(n.File!), StringComparer.OrdinalIgnoreCase)
            .ToDictionary(g => g.Key, g => g.Count(), StringComparer.OrdinalIgnoreCase);
        string Selector(string programId)
        {
            var file = byId[programId].File!;
            var name = Path.GetFileName(file);
            return basenames[name] > 1 ? file : name;
        }

        return new EstateSlice
        {
            ClusterId = id,
            Programs = members.Order(StringComparer.Ordinal).ToList(),
            Needs = needs.ToList(),
            Missing = missing.ToList(),
            Jobs = jobs,
            ProgramSelectors = members.Order(StringComparer.Ordinal).Select(Selector).ToList(),
            NeedSelectors = needs.Select(Selector).ToList(),
        };
    }
}
