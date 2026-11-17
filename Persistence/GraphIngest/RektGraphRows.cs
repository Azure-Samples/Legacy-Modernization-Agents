using System.Text.Json;

namespace CobolToQuarkusMigration.Persistence.GraphIngest;

public sealed record RelationshipBatch(
    string FromLabel, string FromKey, string Type, string ToLabel, string ToKey,
    List<Dictionary<string, object?>> Rows, string[] Properties)
{
    public static RelationshipBatch ByUid(string fromLabel, string type, string toLabel,
        List<Dictionary<string, object?>> rows, params string[] properties) =>
        new(fromLabel, "uid", type, toLabel, "uid", rows, properties);
}

// Builds the node and relationship rows for one parse artifact without touching Neo4j, in the
// shapes the portal, the MCP server and Neo4jMigrationRepository read back.
public static class RektGraphRows
{
    public const int SourceBlockLines = 500;

    public static string Uid(params object?[] parts) => GraphUid.Make(parts.Select(GraphJson.Str).ToArray<object>());

    public static Dictionary<string, object?> Link(object? fromId, object? toId) => new() { ["from_id"] = fromId, ["to_id"] = toId };

    public static (List<Dictionary<string, object?>> Nodes, RelationshipBatch Contains) Ast(
        string program, JsonElement ast, long runId)
    {
        var nodes = new List<Dictionary<string, object?>>();
        var edges = new List<Dictionary<string, object?>>();
        // Pre-order like the original recursive walk, whose node count feeds id-less identities.
        var pending = new Stack<(JsonElement Node, string? Parent)>();
        pending.Push((ast, null));
        while (pending.Count > 0)
        {
            var (node, parent) = pending.Pop();
            // Empty native text is authoritative; only absent/null values fall back.
            var text = GraphJson.GetOrNull(node, "originalText") ?? GraphJson.GetOrNull(node, "text");
            object? originalText = text is { } t ? GraphJson.ToGraphValue(t) : "";
            var id = GraphJson.GetOrNull(node, "id");
            var upstream = GraphJson.Truthy(id)
                ? GraphJson.Str(id!.Value)
                : Uid(TextOr(node, "type", ""), originalText, nodes.Count);
            var uid = GraphSourcePaths.ScopedGraphId(runId, program, upstream);
            var span = GraphJson.TryGet(node, "span", out var s) ? s : default;
            var props = new Dictionary<string, object?>
            {
                ["uid"] = uid,
                ["id"] = upstream,
                ["program"] = program,
                ["runId"] = runId,
                ["nodeType"] = GraphJson.Get(node, "type", "UNKNOWN"),
                ["label"] = GraphJson.Get(node, "label", GraphJson.Get(node, "type", "")),
                ["originalText"] = originalText,
                ["startLine"] = GraphJson.Get(span, "start", -1L),
                ["endLine"] = GraphJson.Get(span, "end", -1L),
            };
            foreach (var key in new[] { "scope", "level", "name", "section", "paragraph" })
            {
                if (GraphJson.TryGet(node, key, out var value)) props[key] = GraphJson.ToGraphValue(value);
            }
            nodes.Add(props);
            if (parent is not null) edges.Add(Link(parent, uid));

            foreach (var child in GraphJson.Items(node, "children").Reverse()) pending.Push((child, uid));
        }
        return (nodes, RelationshipBatch.ByUid("ASTNode", "CONTAINS", "ASTNode", edges));
    }

    public static List<RelationshipBatch> Cfg(string program, JsonElement cfg, long runId)
    {
        var followedBy = new List<Dictionary<string, object?>>();
        var jumpsTo = new List<Dictionary<string, object?>>();
        foreach (var edge in GraphJson.Items(cfg, "edges"))
        {
            var from = GraphJson.GetOrNull(edge, "from") ?? GraphJson.GetOrNull(edge, "fromNodeID");
            var to = GraphJson.GetOrNull(edge, "to") ?? GraphJson.GetOrNull(edge, "toNodeID");
            if (from is null || to is null) continue;
            var row = Link(
                GraphSourcePaths.ScopedGraphId(runId, program, GraphJson.Str(from.Value)),
                GraphSourcePaths.ScopedGraphId(runId, program, GraphJson.Str(to.Value)));
            var type = GraphJson.Truthy(GraphJson.GetOrNull(edge, "type"))
                ? GraphJson.GetString(edge, "type")
                : GraphJson.Get(edge, "edgeType", "FOLLOWED_BY") as string;
            (type == "JUMPS_TO" ? jumpsTo : followedBy).Add(row);
        }
        return
        [
            RelationshipBatch.ByUid("ASTNode", "FOLLOWED_BY", "ASTNode", followedBy),
            RelationshipBatch.ByUid("ASTNode", "JUMPS_TO", "ASTNode", jumpsTo),
        ];
    }

    public static (List<Dictionary<string, object?>> Nodes, List<RelationshipBatch> Relationships) DataStructures(
        string program, JsonElement ds, long runId)
    {
        var nodes = new List<Dictionary<string, object?>>();
        var contains = new List<Dictionary<string, object?>>();
        var flowsInto = new List<Dictionary<string, object?>>();
        var redefines = new List<Dictionary<string, object?>>();

        IEnumerable<JsonElement> records = ds.ValueKind == JsonValueKind.Array
            ? ds.EnumerateArray()
            : GraphJson.TryGet(ds, "records", out var listed) ? (listed.ValueKind == JsonValueKind.Array ? listed.EnumerateArray() : []) : [ds];

        foreach (var record in records)
        {
            var pending = new Stack<(JsonElement Node, string? Parent)>();
            pending.Push((record, null));
            while (pending.Count > 0)
            {
                var (node, parent) = pending.Pop();
                // Unlike AST ids, only an absent id falls back; a present null keeps Python's "None".
                var upstream = GraphJson.TryGet(node, "id", out var id)
                    ? GraphJson.Str(id)
                    : Uid(program, "ds", TextOr(node, "name", ""), nodes.Count);
                var uid = GraphSourcePaths.ScopedGraphId(runId, program, upstream);
                nodes.Add(new Dictionary<string, object?>
                {
                    ["uid"] = uid,
                    ["id"] = upstream,
                    ["program"] = program,
                    ["runId"] = runId,
                    ["name"] = GraphJson.Get(node, "name", ""),
                    ["level"] = GraphJson.Get(node, "level", 0L),
                    ["dataType"] = GraphJson.Get(node, "dataType", ""),
                    ["picture"] = GraphJson.Get(node, "picture", ""),
                    ["originalText"] = GraphJson.Get(node, "text", ""),
                });
                if (parent is not null) contains.Add(Link(parent, uid));
                foreach (var child in GraphJson.Items(node, "children").Reverse()) pending.Push((child, uid));
            }
        }

        foreach (var edge in GraphJson.Items(ds, "edges"))
        {
            var row = Link(
                GraphSourcePaths.ScopedGraphId(runId, program, Required(edge, "from")),
                GraphSourcePaths.ScopedGraphId(runId, program, Required(edge, "to")));
            (GraphJson.GetString(edge, "type") == "REDEFINES" ? redefines : flowsInto).Add(row);
        }

        return (nodes,
        [
            RelationshipBatch.ByUid("DataStructure", "CONTAINS", "DataStructure", contains),
            RelationshipBatch.ByUid("DataStructure", "FLOWS_INTO", "DataStructure", flowsInto),
            RelationshipBatch.ByUid("DataStructure", "REDEFINES", "DataStructure", redefines),
        ]);
    }

    public static (List<Dictionary<string, object?>> Nodes, RelationshipBatch HasSourceBlock) SourceBlocks(
        string program, string content, object runId)
    {
        var lines = content.Split('\n');
        var blocks = new List<Dictionary<string, object?>>();
        for (var i = 0; i < lines.Length; i += SourceBlockLines)
        {
            blocks.Add(new Dictionary<string, object?>
            {
                ["uid"] = Uid(runId, program, "sb", i),
                ["program"] = program,
                ["runId"] = runId,
                ["blockIndex"] = (long)(i / SourceBlockLines),
                ["startLine"] = (long)(i + 1),
                ["endLine"] = (long)Math.Min(i + SourceBlockLines, lines.Length),
                ["content"] = string.Join("\n", lines.Skip(i).Take(SourceBlockLines)),
            });
        }
        var fileUid = Uid(runId, program);
        var links = blocks.Select(b => Link(fileUid, b["uid"])).ToList();
        return (blocks, RelationshipBatch.ByUid("CobolFile", "HAS_SOURCE_BLOCK", "SourceBlock", links));
    }

    private static string TextOr(JsonElement element, string key, string fallback) =>
        GraphJson.TryGet(element, key, out var value) ? GraphJson.Str(value) : fallback;

    private static string Required(JsonElement edge, string key) =>
        GraphJson.TryGet(edge, key, out var value)
            ? GraphJson.Str(value)
            : throw new InvalidDataException($"A data-structure edge has no '{key}'.");
}
