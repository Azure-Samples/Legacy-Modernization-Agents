using System.Globalization;
using System.Text.RegularExpressions;

namespace CobolToQuarkusMigration.Estate;

// What the portal knows about a program beyond the source: whether REKT produced an AST for it and
// how each target's latest conversion scored on parity.
public sealed record EstateMissionStatus(bool Parsed, string? Fidelity, IReadOnlyDictionary<string, EstateMissionConversion> Conversions);

public sealed record EstateMissionConversion(string Outcome, double? Parity, double Threshold);

public sealed record EstateMissionNodeStatus(bool Rekt, string? Fidelity, IReadOnlyDictionary<string, EstateMissionConversion> Conversions, double? BestParity);

public sealed record EstateMissionNode
{
    public required string Id { get; init; }
    public required string Type { get; init; }
    public required string Label { get; init; }
    public string? Estate { get; init; }
    public string? Kind { get; init; }
    public IReadOnlyList<string> Tech { get; init; } = [];
    public string? Domain { get; init; }
    public string? DomainReason { get; init; }
    public string? Cluster { get; init; }
    public string? Hub { get; init; }
    public IReadOnlyList<string> Flags { get; init; } = [];
    public bool? Reachable { get; init; }
    public IReadOnlyDictionary<string, int>? Metrics { get; init; }
    public EstateMissionNodeStatus? Status { get; init; }
    public string? File { get; init; }
    public string? Description { get; init; }
    public string? Dsn { get; init; }
    public string? Store { get; init; }
}

public sealed record EstateMissionEvidence(string File, int Line);

public sealed record EstateMissionEdge(string Source, string Target, string Type, int Count, string? Via, IReadOnlyList<EstateMissionEvidence> Evidence);

public sealed record EstateMissionLink(string From, string To);

public sealed record EstateMissionSharedData(string Id, IReadOnlyList<string> Clusters);

public sealed record EstateMissionCluster
{
    public required string Id { get; init; }
    public required string Label { get; init; }
    public string? Domain { get; init; }
    public int Wave { get; init; }
    // low-risk, moderate or core, from the carve score; platform for the shared services.
    public required string Tier { get; init; }
    public required IReadOnlyList<string> Members { get; init; }
    public int Programs { get; init; }
    public int Lines { get; init; }
    public double? Cohesion { get; init; }
    public double? CarveScore { get; init; }
    public IReadOnlyDictionary<string, double> ScoreBreakdown { get; init; } = new SortedDictionary<string, double>();
    public IReadOnlyList<string> EntryPoints { get; init; } = [];
    public IReadOnlyList<string> OwnedData { get; init; } = [];
    public IReadOnlyList<EstateMissionSharedData> SharedData { get; init; } = [];
    public IReadOnlyList<EstateMissionLink> ApiSurface { get; init; } = [];
    public IReadOnlyList<EstateMissionLink> DependsOn { get; init; } = [];
    public IReadOnlyList<string> DependsOnClusters { get; init; } = [];
    public IReadOnlyList<string> UsedByClusters { get; init; } = [];
    public IReadOnlyList<string> Missing { get; init; } = [];
    public bool Standalone { get; init; }
    public required string Rationale { get; init; }
    // The analysis cluster a slice is cut from; null for the shared services, which every slice needs.
    public string? SliceId { get; init; }
}

public sealed record EstateMissionEstate(string Id, int Programs, int Lines, int Jobs, int Transactions, int Apis);

public sealed record EstateMissionDomain(string Name, int Programs, int Lines);

public sealed record EstateMissionDocument
{
    public int SchemaVersion { get; init; } = 1;
    public DateTime GeneratedAtUtc { get; init; }
    public string Generator { get; init; } = "Estate/EstateGraphBuilder (deterministic)";
    public IReadOnlyDictionary<string, int> Inputs { get; init; } = new SortedDictionary<string, int>();
    public IReadOnlyDictionary<string, int> Kpis { get; init; } = new SortedDictionary<string, int>();
    public IReadOnlyList<EstateMissionEstate> Estates { get; init; } = [];
    public IReadOnlyList<EstateMissionDomain> Domains { get; init; } = [];
    public IReadOnlyList<EstateMissionNode> Nodes { get; init; } = [];
    public IReadOnlyList<EstateMissionEdge> Edges { get; init; } = [];
    public IReadOnlyList<EstateMissionCluster> Clusters { get; init; } = [];
    public int DiagnosticCount { get; init; }
    public string? Warning { get; init; }
}

// Mission Control's view of the estate graph: node types, program kind, technology, business
// function, status, and carve-out clusters with the data they own and share. Everything is derived
// from the graph and the status passed in; nothing is estimated.
public static class EstateMissionView
{
    public const string SharedClusterId = "shared";
    public const int EvidencePerEdge = 3;
    public const int MaxSharedApiSurface = 200;

    private static readonly HashSet<string> CallKinds = [EstateEdgeKind.Calls, EstateEdgeKind.Links];
    private static readonly HashSet<string> DataKinds = [EstateEdgeKind.Reads, EstateEdgeKind.Writes, EstateEdgeKind.Updates, EstateEdgeKind.Deletes];
    private static readonly HashSet<string> DataNodeKinds = [EstateNodeKind.Table, EstateNodeKind.Dataset, EstateNodeKind.File];
    private static readonly HashSet<string> ReachKinds =
        [EstateEdgeKind.Runs, EstateEdgeKind.Invokes, EstateEdgeKind.Calls, EstateEdgeKind.Links, EstateEdgeKind.Starts, EstateEdgeKind.Feeds];
    private static readonly HashSet<string> EntryNodeKinds = [EstateNodeKind.Transaction, EstateNodeKind.Job, EstateNodeKind.Api];

    public static EstateMissionDocument Build(EstateGraph graph, IReadOnlyDictionary<string, EstateMissionStatus> status,
        EstateGraphOptions options, string? warning = null)
    {
        var byId = graph.Nodes.ToDictionary(n => n.Id, StringComparer.Ordinal);
        var outgoing = graph.Edges.GroupBy(e => e.From).ToDictionary(g => g.Key, g => g.ToList(), StringComparer.Ordinal);
        var incoming = graph.Edges.GroupBy(e => e.To).ToDictionary(g => g.Key, g => g.ToList(), StringComparer.Ordinal);
        List<EstateEdge> Out(string id) => outgoing.GetValueOrDefault(id) ?? [];
        List<EstateEdge> In(string id) => incoming.GetValueOrDefault(id) ?? [];

        bool IsSystem(EstateNode n) => !n.InSource && n.Kind == EstateNodeKind.Program
            && options.SystemProgramPrefixes.Any(p => n.Name.StartsWith(p, StringComparison.OrdinalIgnoreCase));
        bool IsProgram(EstateNode n) => n.Kind == EstateNodeKind.Program && n.InSource;

        // ---- estate per node: the top folder of its file, else the one estate all its neighbours share.
        var estateOf = new Dictionary<string, string?>(StringComparer.Ordinal);
        foreach (var n in graph.Nodes) estateOf[n.Id] = EstateOfFile(n.File);
        foreach (var n in graph.Nodes.Where(n => estateOf[n.Id] is null))
        {
            var near = Out(n.Id).Select(e => e.To).Concat(In(n.Id).Select(e => e.From))
                .Select(id => estateOf.GetValueOrDefault(id)).OfType<string>().Distinct().ToList();
            if (near.Count == 1) estateOf[n.Id] = near[0];
        }

        // ---- hubs: programs everything leans on are carved first, as shared services.
        var programHubs = graph.Hubs.Where(h => byId.TryGetValue(h.NodeId, out var n) && IsProgram(n))
            .ToDictionary(h => h.NodeId, h => h.FanIn >= options.HubMinDegree ? "shared service" : "navigation hub", StringComparer.Ordinal);

        var clusterOf = new Dictionary<string, string>(StringComparer.Ordinal);
        foreach (var c in graph.Clusters)
            foreach (var p in c.Programs)
                clusterOf[p] = programHubs.ContainsKey(p) ? SharedClusterId : c.Id;

        // ---- program facts
        var metrics = graph.Nodes.Where(IsProgram).ToDictionary(n => n.Id, n => Metrics(n), StringComparer.Ordinal);
        var kinds = new Dictionary<string, string>(StringComparer.Ordinal);
        var tech = new Dictionary<string, List<string>>(StringComparer.Ordinal);
        foreach (var n in graph.Nodes.Where(IsProgram))
        {
            var m = metrics[n.Id];
            var from = In(n.Id).Select(e => (e.Kind, byId[e.From].Kind)).ToList();
            bool FromKind(string k) => from.Any(f => f.Item2 == k);
            var sendsMaps = Out(n.Id).Any(e => e.Kind == EstateEdgeKind.UsesMap);
            kinds[n.Id] =
                FromKind(EstateNodeKind.Api) && !FromKind(EstateNodeKind.Transaction) ? "api"
                : FromKind(EstateNodeKind.Transaction) || (m.GetValueOrDefault("execCics") > 0 && sendsMaps) ? "online"
                : FromKind(EstateNodeKind.Job) ? "batch"
                : from.Any(f => CallKinds.Contains(f.Item1)) ? (m.GetValueOrDefault("execCics") > 0 ? "subroutine (CICS)" : "subroutine")
                : m.GetValueOrDefault("execCics") > 0 ? "online" : "batch";

            var t = new SortedSet<string>(StringComparer.Ordinal);
            var targets = Out(n.Id).Select(e => byId[e.To]).ToList();
            if (m.GetValueOrDefault("execCics") > 0) t.Add("CICS");
            if (m.GetValueOrDefault("execSql") > 0 || targets.Any(x => x.Kind == EstateNodeKind.Table)) t.Add("DB2");
            if (m.GetValueOrDefault("execDli") > 0 || targets.Any(x => x.Kind == EstateNodeKind.Program && x.Name is "CBLTDLI" or "AIBTDLI")) t.Add("IMS");
            if (targets.Any(x => x.Kind == EstateNodeKind.Program && !x.InSource && x.Name.StartsWith("MQ", StringComparison.OrdinalIgnoreCase))) t.Add("MQ");
            if (targets.Any(x => x.Kind == EstateNodeKind.File)) t.Add("VSAM");
            if (n.Attributes.ContainsKey("dds") || targets.Any(x => x.Kind == EstateNodeKind.Dataset)) t.Add("Files");
            tech[n.Id] = t.ToList();
        }

        // ---- reachability from transactions, jobs and APIs
        var entries = graph.Nodes.Where(n => EntryNodeKinds.Contains(n.Kind)).Select(n => n.Id).ToList();
        var reached = new HashSet<string>(entries, StringComparer.Ordinal);
        var stack = new Stack<string>(entries);
        while (stack.Count > 0)
            foreach (var e in Out(stack.Pop()).Where(e => ReachKinds.Contains(e.Kind)))
                if (reached.Add(e.To)) stack.Push(e.To);

        // ---- business function
        var rules = options.DomainRules.Select(r => (Rule: r, Name: Compile(r.Name), Text: Compile(r.Text))).ToList();
        var domains = new Dictionary<string, (string Domain, string Reason)>(StringComparer.Ordinal);
        foreach (var n in graph.Nodes.Where(IsProgram))
        {
            var touched = Out(n.Id).Where(e => DataKinds.Contains(e.Kind) || e.Kind == EstateEdgeKind.UsesMap).Select(e => byId[e.To].Name);
            var text = string.Join(' ', new[] { n.Attributes.GetValueOrDefault("description") ?? "" }.Concat(touched));
            domains[n.Id] = Classify(Label(n), text, rules, options.DomainFallback);
        }
        string? DomainOfNeighbours(EstateNode n, bool forward)
        {
            var owners = (forward ? Out(n.Id).Select(e => e.To) : In(n.Id).Select(e => e.From)).Where(domains.ContainsKey);
            return owners.GroupBy(o => domains[o].Domain).OrderByDescending(g => g.Count()).ThenBy(g => g.Key, StringComparer.Ordinal)
                .Select(g => g.Key).FirstOrDefault();
        }

        // ---- nodes
        var nodes = new List<EstateMissionNode>();
        foreach (var n in graph.Nodes)
        {
            var flags = new SortedSet<string>(StringComparer.Ordinal);
            string type;
            string? store = null, dsn = null, kind = null, domain = null, reason = null;
            switch (n.Kind)
            {
                case EstateNodeKind.Program when IsSystem(n):
                    type = "utility";
                    break;
                case EstateNodeKind.Program:
                    type = "program";
                    if (!n.InSource) flags.Add("not-in-source");
                    break;
                case EstateNodeKind.Map: type = "screen"; break;
                case EstateNodeKind.File: type = "dataset"; store = "CICS file"; break;
                case EstateNodeKind.Dataset: type = "dataset"; dsn = n.Name; break;
                case EstateNodeKind.Table: type = "table"; store = "DB2 table"; break;
                default: type = n.Kind; break;
            }
            if (!n.InSource && n.Kind is EstateNodeKind.Copybook) flags.Add("unresolved");
            if (!n.InSource && n.Kind is EstateNodeKind.Transaction or EstateNodeKind.Map) flags.Add("not-in-source");

            EstateMissionNodeStatus? st = null;
            bool? reachable = null;
            if (IsProgram(n))
            {
                kind = kinds[n.Id];
                (domain, reason) = domains[n.Id];
                if (n.Attributes.ContainsKey("unresolvedDynamicCalls")) flags.Add("dynamic-call");
                if (In(n.Id).Count == 0) flags.Add("unreferenced");
                if (programHubs.ContainsKey(n.Id)) flags.Add("hub");
                reachable = entries.Count > 0 ? reached.Contains(n.Id) : null;
                var s = status.GetValueOrDefault(n.Id);
                var conv = s?.Conversions ?? new Dictionary<string, EstateMissionConversion>();
                st = new EstateMissionNodeStatus(s?.Parsed ?? false, s?.Fidelity, conv,
                    conv.Values.Select(c => c.Parity).OfType<double>().Cast<double?>().DefaultIfEmpty(null).Max());
            }
            else if (n.Kind is EstateNodeKind.Transaction or EstateNodeKind.Api)
                domain = DomainOfNeighbours(n, forward: true);
            else if (n.Kind is EstateNodeKind.Map)
                domain = DomainOfNeighbours(n, forward: false);

            nodes.Add(new EstateMissionNode
            {
                Id = n.Id,
                Type = type,
                Label = Label(n),
                Estate = estateOf[n.Id],
                Kind = kind,
                Tech = tech.GetValueOrDefault(n.Id) ?? [],
                Domain = domain,
                DomainReason = reason,
                Cluster = clusterOf.GetValueOrDefault(n.Id),
                Hub = programHubs.GetValueOrDefault(n.Id),
                Flags = flags.ToList(),
                Reachable = reachable,
                Metrics = metrics.GetValueOrDefault(n.Id),
                Status = st,
                File = n.File,
                Description = n.Attributes.GetValueOrDefault("description")
                              ?? (type == "utility" ? "runtime or middleware interface" : !n.InSource && type == "program" ? "called but not found in source" : null),
                Dsn = dsn,
                Store = store,
            });
        }
        var nodeById = nodes.ToDictionary(n => n.Id, StringComparer.Ordinal);

        // ---- edges, in Mission Control's vocabulary
        var edges = graph.Edges
            .Select(e => (e, Type: EdgeType(e, byId)))
            .GroupBy(x => (x.e.From, x.e.To, x.Type))
            .Select(g => new EstateMissionEdge(g.Key.From, g.Key.To, g.Key.Type,
                Math.Max(1, g.Sum(x => x.e.Evidence.Count)),
                g.Select(x => x.e.Via).FirstOrDefault(v => v is not null),
                g.SelectMany(x => x.e.Evidence).Take(EvidencePerEdge).Select(v => new EstateMissionEvidence(v.File, v.Line)).ToList()))
            .OrderBy(e => e.Type, StringComparer.Ordinal).ThenBy(e => e.Source, StringComparer.Ordinal).ThenBy(e => e.Target, StringComparer.Ordinal)
            .ToList();

        var clusters = Clusters(graph, byId, Out, In, clusterOf, programHubs, metrics, domains, options);

        // ---- headline numbers
        var programs = nodes.Where(n => n.Type == "program" && !n.Flags.Contains("not-in-source")).ToList();
        int Count(string type) => nodes.Count(n => n.Type == type);
        var kpis = new SortedDictionary<string, int>(StringComparer.Ordinal)
        {
            ["programs"] = programs.Count,
            ["lines"] = programs.Sum(p => p.Metrics?.GetValueOrDefault("lines") ?? 0),
            ["transactions"] = Count(EstateNodeKind.Transaction),
            ["apis"] = Count(EstateNodeKind.Api),
            ["jobs"] = Count(EstateNodeKind.Job),
            ["screens"] = Count("screen"),
            ["tables"] = Count("table"),
            ["datasets"] = Count("dataset"),
            ["copybooks"] = Count(EstateNodeKind.Copybook),
            ["utilities"] = Count("utility"),
            ["missingPrograms"] = nodes.Count(n => n.Flags.Contains("not-in-source") && n.Type == "program"),
            ["edges"] = edges.Count,
            ["online"] = programs.Count(p => p.Kind == "online"),
            ["batch"] = programs.Count(p => p.Kind == "batch"),
            ["subroutines"] = programs.Count(p => p.Kind?.StartsWith("subroutine", StringComparison.Ordinal) == true),
            ["apiPrograms"] = programs.Count(p => p.Kind == "api"),
            ["unreferenced"] = programs.Count(p => p.Flags.Contains("unreferenced")),
            ["unreachable"] = programs.Count(p => p.Reachable == false),
            ["rektParsed"] = programs.Count(p => p.Status?.Rekt == true),
            ["converted"] = programs.Count(p => p.Status?.Conversions.Count > 0),
            ["clusters"] = clusters.Count,
            ["hubs"] = programHubs.Count,
        };

        var inputs = new SortedDictionary<string, int>(StringComparer.Ordinal)
        {
            ["cobol"] = programs.Count,
            ["copybooks"] = graph.Nodes.Count(n => n.Kind == EstateNodeKind.Copybook && n.InSource),
            ["jcl"] = graph.Nodes.Count(n => n.Kind == EstateNodeKind.Job),
            ["bms"] = graph.Nodes.Count(n => n.Kind == EstateNodeKind.Map && n.InSource),
            ["cicsDefinitions"] = Files(graph.Nodes.Where(n => n.Kind is EstateNodeKind.Transaction or EstateNodeKind.File)),
            ["ddl"] = Files(graph.Nodes.Where(n => n.Kind == EstateNodeKind.Table)),
            ["apiOperations"] = graph.Nodes.Count(n => n.Kind == EstateNodeKind.Api),
        };

        var estates = programs.Where(p => p.Estate is not null).GroupBy(p => p.Estate!)
            .OrderBy(g => g.Key, StringComparer.Ordinal)
            .Select(g => new EstateMissionEstate(g.Key, g.Count(), g.Sum(p => p.Metrics?.GetValueOrDefault("lines") ?? 0),
                nodes.Count(n => n.Type == EstateNodeKind.Job && n.Estate == g.Key),
                nodes.Count(n => n.Type == EstateNodeKind.Transaction && n.Estate == g.Key),
                nodes.Count(n => n.Type == EstateNodeKind.Api && n.Estate == g.Key)))
            .ToList();

        var domainList = programs.GroupBy(p => p.Domain ?? options.DomainFallback)
            .Select(g => new EstateMissionDomain(g.Key, g.Count(), g.Sum(p => p.Metrics?.GetValueOrDefault("lines") ?? 0)))
            .OrderByDescending(d => d.Programs).ThenBy(d => d.Name, StringComparer.Ordinal).ToList();

        return new EstateMissionDocument
        {
            GeneratedAtUtc = graph.GeneratedAtUtc,
            Inputs = inputs,
            Kpis = kpis,
            Estates = estates,
            Domains = domainList,
            Nodes = nodes.OrderBy(n => n.Type, StringComparer.Ordinal).ThenBy(n => n.Id, StringComparer.Ordinal).ToList(),
            Edges = edges,
            Clusters = clusters,
            DiagnosticCount = graph.Diagnostics.Count,
            Warning = warning,
        };

        static int Files(IEnumerable<EstateNode> ns) => ns.Where(n => n.InSource && n.File is not null).Select(n => n.File).Distinct().Count();
    }

    private static List<EstateMissionCluster> Clusters(EstateGraph graph, Dictionary<string, EstateNode> byId,
        Func<string, List<EstateEdge>> Out, Func<string, List<EstateEdge>> In, Dictionary<string, string> clusterOf,
        Dictionary<string, string> programHubs, Dictionary<string, SortedDictionary<string, int>> metrics,
        Dictionary<string, (string Domain, string Reason)> domains, EstateGraphOptions options)
    {
        int Lines(IEnumerable<string> ps) => ps.Sum(p => metrics.GetValueOrDefault(p)?.GetValueOrDefault("lines") ?? 0);
        string? MainDomain(IEnumerable<string> ps) => ps.Where(domains.ContainsKey).GroupBy(p => domains[p].Domain)
            .OrderByDescending(g => g.Count()).ThenBy(g => g.Key, StringComparer.Ordinal).Select(g => g.Key).FirstOrDefault();

        // Which clusters read or write each data store: one owner moves it, more than one shares it.
        var dataUsers = new Dictionary<string, SortedSet<string>>(StringComparer.Ordinal);
        foreach (var e in graph.Edges.Where(e => DataKinds.Contains(e.Kind) && clusterOf.ContainsKey(e.From) && DataNodeKinds.Contains(byId[e.To].Kind)))
        {
            if (!dataUsers.TryGetValue(e.To, out var set)) dataUsers[e.To] = set = new(StringComparer.Ordinal);
            set.Add(clusterOf[e.From]);
        }

        var result = new List<EstateMissionCluster>();
        foreach (var c in graph.Clusters)
        {
            var members = c.Programs.Where(p => !programHubs.ContainsKey(p)).ToList();
            if (members.Count == 0) continue;
            var set = members.ToHashSet(StringComparer.Ordinal);
            var touched = members.SelectMany(Out).Where(e => DataKinds.Contains(e.Kind) && DataNodeKinds.Contains(byId[e.To].Kind))
                .Select(e => e.To).Distinct().Order(StringComparer.Ordinal).ToList();
            var owned = touched.Where(d => dataUsers[d].Count == 1).ToList();
            var shared = touched.Where(d => dataUsers[d].Count > 1).Select(d => new EstateMissionSharedData(d, dataUsers[d].ToList())).ToList();
            var entries = members.SelectMany(In).Where(e => EntryNodeKinds.Contains(byId[e.From].Kind)
                    && e.Kind is EstateEdgeKind.Runs or EstateEdgeKind.Invokes)
                .Select(e => e.From).Distinct().Order(StringComparer.Ordinal).ToList();
            var apiSurface = members.SelectMany(In).Where(e => CallKinds.Contains(e.Kind) && !set.Contains(e.From) && clusterOf.ContainsKey(e.From))
                .Select(e => new EstateMissionLink(e.From, e.To)).Distinct().OrderBy(l => l.From, StringComparer.Ordinal).ThenBy(l => l.To, StringComparer.Ordinal).ToList();
            var dependsOn = members.SelectMany(Out).Where(e => CallKinds.Contains(e.Kind) && !set.Contains(e.To))
                .Select(e => new EstateMissionLink(e.From, e.To)).Distinct().OrderBy(l => l.From, StringComparer.Ordinal).ThenBy(l => l.To, StringComparer.Ordinal).ToList();
            var total = c.InternalWeight + c.ExternalWeight;
            var cohesion = total == 0 ? 1 : c.InternalWeight / total;
            var domain = MainDomain(members);
            var name = members.Count == c.Programs.Count ? c.Label : MemberLabel(members);
            var entryTypes = entries.GroupBy(x => byId[x].Kind).OrderBy(g => g.Key, StringComparer.Ordinal)
                .Select(g => $"{g.Count()} {g.Key}").ToList();
            var why = new List<string>
            {
                $"cohesion {Pct(cohesion)}",
                $"completeness {Pct(c.ScoreBreakdown.GetValueOrDefault("completeness"))}",
                $"independence {Pct(c.ScoreBreakdown.GetValueOrDefault("independence"))}",
                $"{owned.Count} owned / {shared.Count} shared data stores",
                $"{entries.Count} entry point(s)" + (entryTypes.Count > 0 ? $" ({string.Join(", ", entryTypes)})" : ""),
                $"{apiSurface.Count} inbound call(s) from other clusters become service APIs",
                $"{dependsOn.Count} outbound call(s)",
            };
            // System and runtime routines (LE, DB2, CICS stubs) are never in source; they are not gaps.
            var missing = c.Missing.Where(m => !options.SystemProgramPrefixes.Any(p => m.StartsWith(p, StringComparison.OrdinalIgnoreCase))).ToList();
            if (missing.Count > 0) why.Add($"{missing.Count} reference(s) not in source");

            result.Add(new EstateMissionCluster
            {
                Id = c.Id,
                Label = c.Standalone ? "Standalone programs" : domain is null ? name : $"{domain} · {name}",
                Domain = domain,
                Wave = c.Wave,
                Tier = c.CarveScore >= options.LowRiskCarveScore ? "low-risk" : c.CarveScore >= options.ModerateCarveScore ? "moderate" : "core",
                Members = members,
                Programs = members.Count,
                Lines = Lines(members),
                Cohesion = Math.Round(cohesion, 3),
                CarveScore = Math.Round(c.CarveScore / 100, 3),
                ScoreBreakdown = c.ScoreBreakdown,
                EntryPoints = entries,
                OwnedData = owned,
                SharedData = shared,
                ApiSurface = apiSurface,
                DependsOn = dependsOn,
                DependsOnClusters = c.DependsOn,
                UsedByClusters = c.DependedOnBy,
                Missing = missing,
                Standalone = c.Standalone,
                Rationale = string.Join("; ", why),
                SliceId = c.Id,
            });
        }

        if (programHubs.Count > 0)
        {
            var hubs = programHubs.Keys.Order(StringComparer.Ordinal).ToList();
            var hubSet = hubs.ToHashSet(StringComparer.Ordinal);
            var calls = hubs.SelectMany(In).Where(e => CallKinds.Contains(e.Kind) && !hubSet.Contains(e.From)).ToList();
            var users = calls.Select(e => clusterOf.GetValueOrDefault(e.From)).OfType<string>().Distinct().Order(StringComparer.Ordinal).ToList();
            result.Add(new EstateMissionCluster
            {
                Id = SharedClusterId,
                Label = "Shared services & navigation",
                Domain = "Platform",
                Wave = 0,
                Tier = "platform",
                Members = hubs,
                Programs = hubs.Count,
                Lines = Lines(hubs),
                ApiSurface = calls.Select(e => new EstateMissionLink(e.From, e.To)).Distinct().Take(MaxSharedApiSurface).ToList(),
                UsedByClusters = users,
                Rationale = $"{hubs.Count} hub program(s) that many others call or that route to many others: " +
                            "extract them once as a platform service (or replicate them) before carving the domains.",
            });
        }

        return result.OrderBy(c => c.Wave).ThenByDescending(c => c.CarveScore ?? 0).ThenBy(c => c.Id, StringComparer.Ordinal).ToList();

        // Same rule as the analysis: a shared name prefix, else the member with the most call edges.
        string MemberLabel(List<string> ms)
        {
            var names = ms.Select(m => Path.GetFileNameWithoutExtension(byId[m].Name)).ToList();
            var prefix = names.Aggregate((a, b) => new string(a.Zip(b).TakeWhile(p => p.First == p.Second).Select(p => p.First).ToArray()));
            if (names.Count > 1 && prefix.Length >= 3) return prefix + "*";
            var top = ms.OrderByDescending(m => Out(m).Count(e => CallKinds.Contains(e.Kind)) + In(m).Count(e => CallKinds.Contains(e.Kind)))
                .ThenBy(m => m, StringComparer.Ordinal).First();
            return names.Count > 1 ? $"{Path.GetFileNameWithoutExtension(byId[top].Name)} +{names.Count - 1}" : names[0];
        }

        static string Pct(double x) => (x * 100).ToString("0", CultureInfo.InvariantCulture) + "%";
    }

    private static string EdgeType(EstateEdge e, Dictionary<string, EstateNode> byId) => e.Kind switch
    {
        EstateEdgeKind.Links => "calls",
        EstateEdgeKind.Updates or EstateEdgeKind.Deletes => "writes",
        EstateEdgeKind.UsesMap => "sends",
        EstateEdgeKind.Runs when byId[e.From].Kind == EstateNodeKind.Transaction => "starts",
        EstateEdgeKind.Feeds => "triggers",
        _ => e.Kind,
    };

    private static SortedDictionary<string, int> Metrics(EstateNode n)
    {
        var m = new SortedDictionary<string, int>(StringComparer.Ordinal);
        foreach (var key in new[] { "lines", "paragraphs", "complexity", "perform", "goto", "execCics", "execSql", "execDli" })
            if (n.Attributes.TryGetValue(key, out var v) && int.TryParse(v, NumberStyles.Integer, CultureInfo.InvariantCulture, out var i))
                m[key] = i;
        if (!m.ContainsKey("lines") && n.Attributes.TryGetValue("loc", out var loc) && int.TryParse(loc, NumberStyles.Integer, CultureInfo.InvariantCulture, out var l))
            m["lines"] = l;
        return m;
    }

    // Program ids carry a source-relative path when two files share a name; the label is the name.
    private static string Label(EstateNode n) =>
        n.Kind == EstateNodeKind.Program && n.Name.Contains('/') ? Path.GetFileNameWithoutExtension(n.Name) : n.Name;

    private static string? EstateOfFile(string? file)
    {
        if (string.IsNullOrEmpty(file)) return null;
        var parts = file.Split('/');
        return parts.Length > 1 ? parts[0] : null;
    }

    private static readonly TimeSpan RuleTimeout = TimeSpan.FromMilliseconds(250);

    private static Regex? Compile(string? pattern) => string.IsNullOrWhiteSpace(pattern)
        ? null
        : new Regex(pattern, RegexOptions.IgnoreCase | RegexOptions.CultureInvariant, RuleTimeout);

    // First rule whose name pattern matches wins; failing that, the first whose text pattern does.
    internal static (string Domain, string Reason) Classify(string name, string text,
        IReadOnlyList<(EstateDomainRule Rule, Regex? Name, Regex? Text)> rules, string fallback)
    {
        foreach (var (rule, rx, _) in rules)
            if (rx is not null && Safe(rx, name)) return (rule.Domain, $"name matches /{rule.Name}/");
        foreach (var (rule, _, rx) in rules)
            if (rx is not null && Safe(rx, text)) return (rule.Domain, $"description or touched resources match /{rule.Text}/");
        return (fallback, "no rule matched");

        static bool Safe(Regex rx, string s)
        {
            try { return rx.IsMatch(s); }
            catch (RegexMatchTimeoutException) { return false; }
        }
    }
}
