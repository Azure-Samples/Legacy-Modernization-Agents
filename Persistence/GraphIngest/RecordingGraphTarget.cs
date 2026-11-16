namespace CobolToQuarkusMigration.Persistence.GraphIngest;

public sealed record RecordedNode(string Label, Dictionary<string, object?> Properties);

public sealed record RecordedRelationship(
    string Type, RecordedNode From, RecordedNode To, Dictionary<string, object?> Properties);

// Applies the same MERGE semantics as the Cypher in Neo4jGraphTarget to an in-memory graph, so a
// dry run reports exactly what an ingest would write and how many relationship rows would match.
public sealed class RecordingGraphTarget : IGraphTarget
{
    private readonly Dictionary<(string Label, string Key, string Value), RecordedNode> _index = new();
    private readonly Dictionary<(RecordedNode, string, RecordedNode), RecordedRelationship> _relationships = new();

    public List<RecordedNode> Nodes { get; } = [];

    public IEnumerable<RecordedRelationship> Relationships => _relationships.Values;

    public List<string> SchemaStatements { get; } = [];

    public Dictionary<long, string> ScanStatus { get; } = new();

    public Task ApplySchemaAsync(IReadOnlyList<string> statements)
    {
        SchemaStatements.AddRange(statements);
        return Task.CompletedTask;
    }

    public Task<bool> TryCreateScanAsync(long runId, string name, string sourceDirectory, string outputDirectory, string? manifestPath)
    {
        var occupied = Nodes.Any(n =>
            (n.Label is "CobolFile" or "JclNode" && Equals(n.Properties.GetValueOrDefault("runId"), runId)) ||
            (n.Label is "RektScan" or "Run" && Equals(n.Properties.GetValueOrDefault("id"), runId)));
        if (occupied) return Task.FromResult(false);
        Merge("RektScan", "id", new Dictionary<string, object?>
        {
            ["id"] = runId, ["name"] = name, ["sourceDirectory"] = sourceDirectory,
            ["outputDirectory"] = outputDirectory, ["manifestPath"] = manifestPath, ["ingestionStatus"] = "ingesting",
            ["evidenceNote"] = Neo4jGraphTarget.EvidenceNote,
        });
        ScanStatus[runId] = "ingesting";
        return Task.FromResult(true);
    }

    public Task FinishScanAsync(long runId, string status)
    {
        if (Find("RektScan", "id", runId) is { } scan)
        {
            scan.Properties["ingestionStatus"] = status;
            ScanStatus[runId] = status;
        }
        return Task.CompletedTask;
    }

    public Task MergeNodesAsync(string label, string mergeKey, IReadOnlyList<Dictionary<string, object?>> batch)
    {
        foreach (var row in batch) Merge(label, mergeKey, row);
        return Task.CompletedTask;
    }

    public Task<long> MergeRelationshipsAsync(RelationshipBatch shape, IReadOnlyList<Dictionary<string, object?>> batch)
    {
        long matched = 0;
        foreach (var row in batch)
        {
            var from = Find(shape.FromLabel, shape.FromKey, row["from_id"]);
            var to = Find(shape.ToLabel, shape.ToKey, row["to_id"]);
            if (from is null || to is null) continue;
            matched++;
            var key = (from, shape.Type, to);
            if (!_relationships.TryGetValue(key, out var relationship))
            {
                relationship = new RecordedRelationship(shape.Type, from, to, new Dictionary<string, object?>());
                _relationships[key] = relationship;
            }
            foreach (var property in shape.Properties) Set(relationship.Properties, property, row.GetValueOrDefault(property));
        }
        return Task.FromResult(matched);
    }

    public Task<(long Nodes, long Relationships)> TotalsAsync() =>
        Task.FromResult(((long)Nodes.Count, (long)_relationships.Count));

    private void Merge(string label, string mergeKey, Dictionary<string, object?> row)
    {
        var node = Find(label, mergeKey, row[mergeKey]);
        if (node is null)
        {
            node = new RecordedNode(label, new Dictionary<string, object?>());
            Nodes.Add(node);
        }
        // SET n += row: every key is overwritten and a null removes the property.
        foreach (var (key, value) in row) Set(node.Properties, key, value);
        _index[(label, mergeKey, KeyOf(row[mergeKey]))] = node;
    }

    private RecordedNode? Find(string label, string key, object? value)
    {
        if (value is null) return null;
        if (_index.TryGetValue((label, key, KeyOf(value)), out var node)) return node;
        return Nodes.FirstOrDefault(n => n.Label == label && n.Properties.TryGetValue(key, out var v) && KeyOf(v) == KeyOf(value));
    }

    // Neo4j matches a string id and a numeric id as different values.
    private static string KeyOf(object? value) => $"{value?.GetType().Name}:{GraphJson.Str(value)}";

    private static void Set(Dictionary<string, object?> properties, string key, object? value)
    {
        if (value is null) properties.Remove(key);
        else properties[key] = value;
    }
}
