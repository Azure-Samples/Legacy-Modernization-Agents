using System.Text.Json;

namespace CobolToQuarkusMigration.Persistence.GraphIngest;

public sealed record GraphEdge(string FromId, string ToId, string Type, string Label = "");

// Turns the facts jcl-facts wrote for one job (Jcl/JclEstate.cs) into JclNode rows. Nothing is
// re-derived: dataset access and the exclusion of program libraries come from the lineage.
public static class JclGraphBuilder
{
    public const string FactsSuffix = ".job.json";
    public const string LineageFile = "jcl-lineage.json";
    public const int SupportedSchema = 1;

    private const int MaxListed = 3;
    private const int MaxControlLines = 6;

    private static readonly Dictionary<string, string> StepTypes = new()
    {
        ["Program"] = "JCL_STEP",
        ["Utility"] = "JCL_UTILITY_STEP",
        ["TsoBatch"] = "JCL_TSO_STEP",
        ["UnresolvedProcedure"] = "JCL_UNRESOLVED_STEP",
    };

    private static readonly Dictionary<string, string> AccessEdges = new()
    {
        ["Read"] = "READS",
        ["Create"] = "CREATES",
        ["Append"] = "APPENDS",
        ["Exclusive"] = "OPENS_EXCLUSIVE",
        ["Delete"] = "DELETES",
    };

    public static bool IsJclArtifact(string fileName)
    {
        var lower = fileName.ToLowerInvariant();
        return lower.EndsWith(FactsSuffix, StringComparison.Ordinal) || lower == LineageFile;
    }

    public static (List<Dictionary<string, object?>> Nodes, List<GraphEdge> Edges) Build(
        JsonElement facts, JsonElement? lineage, long runId)
    {
        var job = GraphJson.GetOrNull(facts, "job") ?? default;
        var program = FirstTruthy(job, "file", "name") ?? "";
        var jobName = FirstTruthy(job, "name") ?? program;
        var nodes = new Dictionary<string, Dictionary<string, object?>>();
        var order = new List<string>();
        var edges = new List<GraphEdge>();

        string Node(string localId, string nodeType, string name, string text, object? line)
        {
            var uid = GraphSourcePaths.ScopedGraphId(runId, program, localId);
            if (!nodes.ContainsKey(uid))
            {
                order.Add(uid);
                nodes[uid] = new Dictionary<string, object?>
                {
                    ["uid"] = uid,
                    ["id"] = localId,
                    ["program"] = program,
                    ["job"] = jobName,
                    ["runId"] = runId,
                    ["nodeType"] = nodeType,
                    ["label"] = name,
                    ["name"] = name,
                    ["originalText"] = text,
                    ["startLine"] = line,
                    ["endLine"] = line,
                };
            }
            return uid;
        }

        var jobUid = Node("JOB", "JCL_JOB", jobName, JobText(job), 1L);

        var stepUids = new Dictionary<string, string>();
        string? previous = null;
        foreach (var step in GraphJson.Items(job, "steps"))
        {
            var name = FirstTruthy(step, "name") ?? "";
            var kind = GraphJson.GetString(step, "kind");
            // A qualified name (RUN1.STEPA) is a step of an expanded procedure, whose line is in
            // the procedure's member rather than in this file.
            var line = name.Contains('.') ? -1L : GraphJson.Get(step, "line", -1L);
            var uid = Node($"STEP:{name}", kind is not null && StepTypes.TryGetValue(kind, out var type) ? type : "JCL_STEP",
                name, StepText(step), line);
            stepUids[name.ToUpperInvariant()] = uid;
            edges.Add(new GraphEdge(jobUid, uid, "CONTAINS"));
            if (previous is not null) edges.Add(new GraphEdge(previous, uid, "FOLLOWED_BY"));
            previous = uid;

            if (FirstTruthy(step, "program") is { } stepProgram)
            {
                var utility = kind is "Utility" or "TsoBatch";
                var target = Node($"PGM:{stepProgram}", utility ? "JCL_UTILITY" : "JCL_PROGRAM", stepProgram, "", -1L);
                edges.Add(new GraphEdge(uid, target, "RUNS", "EXEC PGM"));
            }
            foreach (var run in GraphJson.Items(step, "runs"))
            {
                if (FirstTruthy(run, "program") is not { } runProgram) continue;
                var target = Node($"PGM:{runProgram}", "JCL_PROGRAM", runProgram, "", -1L);
                edges.Add(new GraphEdge(uid, target, "RUNS", Join("RUN PROGRAM", Plan(run))));
            }
        }

        var lineageRoot = lineage ?? default;
        foreach (var dataset in GraphJson.Items(lineageRoot, "datasets"))
        {
            var key = FirstTruthy(dataset, "dataset") ?? "";
            foreach (var use in GraphJson.Items(dataset, "uses"))
            {
                if (GraphJson.GetString(use, "job") != jobName) continue;
                if (!stepUids.TryGetValue((FirstTruthy(use, "step") ?? "").ToUpperInvariant(), out var stepUid)) continue;
                var temporary = GraphJson.Truthy(GraphJson.GetOrNull(dataset, "temporary"));
                // Temporary datasets are keyed JOB:&&NAME in the lineage; the name alone is what the JCL says.
                var display = temporary && key.Contains(':') ? key[(key.IndexOf(':') + 1)..] : key;
                var target = Node($"DS:{key}", temporary ? "JCL_TEMP_DATASET" : "JCL_DATASET", display, "", -1L);
                var generation = GraphJson.GetOrNull(use, "generation");
                var access = GraphJson.GetString(use, "access");
                edges.Add(new GraphEdge(stepUid, target,
                    access is not null && AccessEdges.TryGetValue(access, out var edgeType) ? edgeType : "USES",
                    Join(FirstTruthy(use, "via") ?? "", GraphJson.Truthy(generation) ? $"({GraphJson.Str(generation!.Value)})" : null)));
            }
        }

        var datasetsBetween = new Dictionary<(string?, string?), List<string>>();
        foreach (var dependency in GraphJson.Items(lineageRoot, "dependencies"))
        {
            datasetsBetween[(GraphJson.GetString(dependency, "upstream"), GraphJson.GetString(dependency, "downstream"))] =
                GraphJson.Items(dependency, "datasets").Select(d => GraphJson.Str(d)).ToList();
        }
        foreach (var upstream in GraphJson.Items(facts, "upstream").Select(u => GraphJson.Str(u)))
        {
            var reference = Node($"JOBREF:{upstream}", "JCL_JOB_REF", upstream, "", -1L);
            edges.Add(new GraphEdge(reference, jobUid, "FEEDS", Listed(datasetsBetween.GetValueOrDefault((upstream, jobName)))));
        }
        foreach (var downstream in GraphJson.Items(facts, "downstream").Select(d => GraphJson.Str(d)))
        {
            var reference = Node($"JOBREF:{downstream}", "JCL_JOB_REF", downstream, "", -1L);
            edges.Add(new GraphEdge(jobUid, reference, "FEEDS", Listed(datasetsBetween.GetValueOrDefault((jobName, downstream)))));
        }

        return (order.Select(uid => nodes[uid]).ToList(), edges);
    }

    private static string JobText(JsonElement job)
    {
        var lines = new List<string>
        {
            Join($"//{TextOr(job, "name", "")} JOB",
                FirstTruthy(job, "jobClass") is { } jobClass ? $"CLASS={jobClass}" : null,
                FirstTruthy(job, "cond") is { } cond ? $"COND={cond}" : null),
        };
        var diagnostics = GraphJson.Items(job, "diagnostics").ToList();
        if (diagnostics.Count > 0)
        {
            var counts = diagnostics
                .GroupBy(d => TextOr(d, "code", ""))
                .OrderBy(g => g.Key, StringComparer.Ordinal)
                .Select(g => $"{g.Key} ×{g.Count()}");
            lines.Add($"{diagnostics.Count} diagnostic(s): " + string.Join(", ", counts));
        }
        var unresolved = GraphJson.Items(job, "unresolvedSymbols").Select(s => GraphJson.Str(s)).ToList();
        if (unresolved.Count > 0) lines.Add("Unresolved symbols: " + string.Join(", ", unresolved));
        return string.Join("\n", lines);
    }

    private static string StepText(JsonElement step)
    {
        var lines = new List<string>();
        if (FirstTruthy(step, "program") is { } program)
        {
            var parm = GraphJson.GetOrNull(step, "parm");
            lines.Add(Join($"EXEC PGM={program}", parm is { } p ? $"PARM='{GraphJson.Str(p)}'" : null));
        }
        if (FirstTruthy(step, "procedure") is { } procedure)
        {
            var inner = FirstTruthy(step, "procedureStep");
            lines.Add($"Procedure {procedure}" + (inner is not null ? $", step {inner}" : "")
                      + (GraphJson.GetString(step, "kind") == "UnresolvedProcedure" ? " (not in the source)" : ""));
        }
        if (FirstTruthy(step, "cond") is { } cond) lines.Add($"COND={cond}");
        foreach (var condition in GraphJson.Items(step, "conditions"))
        {
            var expression = TextOr(condition, "expression", "");
            lines.Add(GraphJson.Truthy(GraphJson.GetOrNull(condition, "negated")) ? $"ELSE of IF {expression}" : $"IF {expression}");
        }
        foreach (var run in GraphJson.Items(step, "runs"))
        {
            lines.Add(Join($"RUN PROGRAM({TextOr(run, "program", "")})", Plan(run)));
        }
        var control = GraphJson.Items(step, "controlStatements").Select(s => GraphJson.Str(s)).ToList();
        lines.AddRange(control.Take(MaxControlLines));
        if (control.Count > MaxControlLines)
        {
            lines.Add($"... {control.Count - MaxControlLines} more control statement(s)");
        }
        return string.Join("\n", lines);
    }

    private static string? Plan(JsonElement run) => FirstTruthy(run, "plan") is { } plan ? $"PLAN({plan})" : null;

    // dict.get(a) or dict.get(b): the first value that is not falsy, as text.
    private static string? FirstTruthy(JsonElement element, params string[] keys)
    {
        foreach (var key in keys)
        {
            var value = GraphJson.GetOrNull(element, key);
            if (GraphJson.Truthy(value)) return GraphJson.Str(value!.Value);
        }
        return null;
    }

    // dict.get(key, default) interpolated into text: a present null prints as None.
    private static string TextOr(JsonElement element, string key, string fallback) =>
        GraphJson.TryGet(element, key, out var value) ? GraphJson.Str(value) : fallback;

    private static string Join(params string?[] parts) => string.Join(" ", parts.Where(p => !string.IsNullOrEmpty(p)));

    private static string Listed(List<string>? items)
    {
        items ??= [];
        var shown = string.Join(", ", items.Take(MaxListed));
        return shown + (items.Count > MaxListed ? $" +{items.Count - MaxListed}" : "");
    }
}
