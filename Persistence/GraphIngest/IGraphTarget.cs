namespace CobolToQuarkusMigration.Persistence.GraphIngest;

// Where ingestion writes: Neo4j for real runs, memory for --dry-run and tests.
public interface IGraphTarget
{
    Task ApplySchemaAsync(IReadOnlyList<string> statements);

    // False when the run id already holds data, in which case nothing is written.
    Task<bool> TryCreateScanAsync(long runId, string name, string sourceDirectory, string outputDirectory, string? manifestPath);

    Task FinishScanAsync(long runId, string status);

    Task MergeNodesAsync(string label, string mergeKey, IReadOnlyList<Dictionary<string, object?>> batch);

    // Returns how many rows found both endpoints.
    Task<long> MergeRelationshipsAsync(RelationshipBatch shape, IReadOnlyList<Dictionary<string, object?>> batch);

    Task<(long Nodes, long Relationships)> TotalsAsync();
}
