using Neo4j.Driver;

namespace CobolToQuarkusMigration.Persistence.GraphIngest;

public sealed class Neo4jGraphTarget(IDriver driver) : IGraphTarget
{
    public const string EvidenceNote =
        "Structural parser evidence only; source is the ingested snapshot and may be normalized. " +
        "Missing inputs or unsupported dependency coverage may remain; this is not verified conversion.";

    public async Task ApplySchemaAsync(IReadOnlyList<string> statements)
    {
        await using var session = driver.AsyncSession();
        foreach (var statement in statements)
        {
            var cursor = await session.RunAsync(statement);
            await cursor.ConsumeAsync();
        }
    }

    public async Task<bool> TryCreateScanAsync(long runId, string name, string sourceDirectory, string outputDirectory, string? manifestPath)
    {
        await using var session = driver.AsyncSession();
        return await session.ExecuteWriteAsync(async tx =>
        {
            var check = await tx.RunAsync(
                "RETURN EXISTS { MATCH (:CobolFile {runId: $runId}) } " +
                "OR EXISTS { MATCH (:RektScan {id: $runId}) } " +
                "OR EXISTS { MATCH (:Run {id: $runId}) } " +
                "OR EXISTS { MATCH (:JclNode {runId: $runId}) } AS occupied",
                new { runId });
            if ((await check.SingleAsync())["occupied"].As<bool>()) return false;

            var create = await tx.RunAsync(
                "CREATE (s:RektScan {id: $runId, name: $name, sourceDirectory: $source, " +
                "outputDirectory: $output, manifestPath: $manifest, createdAt: datetime(), ingestionStatus: 'ingesting', evidenceNote: $note})",
                new Dictionary<string, object?>
                {
                    ["runId"] = runId, ["name"] = name, ["source"] = sourceDirectory,
                    ["output"] = outputDirectory, ["manifest"] = manifestPath, ["note"] = EvidenceNote,
                });
            await create.ConsumeAsync();
            return true;
        });
    }

    public async Task FinishScanAsync(long runId, string status)
    {
        await using var session = driver.AsyncSession();
        var cursor = await session.RunAsync(
            "MATCH (s:RektScan {id: $runId}) SET s.ingestionStatus = $status, s.updatedAt = datetime()",
            new { runId, status });
        await cursor.ConsumeAsync();
    }

    public async Task MergeNodesAsync(string label, string mergeKey, IReadOnlyList<Dictionary<string, object?>> batch)
    {
        var cypher = $"UNWIND $batch AS row MERGE (n:{label} {{{mergeKey}: row.{mergeKey}}}) SET n += row";
        await using var session = driver.AsyncSession();
        await session.ExecuteWriteAsync(async tx => await (await tx.RunAsync(cypher, new { batch })).ConsumeAsync());
    }

    public async Task<long> MergeRelationshipsAsync(RelationshipBatch shape, IReadOnlyList<Dictionary<string, object?>> batch)
    {
        var set = shape.Properties.Length == 0
            ? ""
            : "SET " + string.Join(", ", shape.Properties.Select(p => $"r.{p} = row.{p}"));
        var cypher =
            "UNWIND $batch AS row " +
            $"MATCH (a:{shape.FromLabel} {{{shape.FromKey}: row.from_id}}) " +
            $"MATCH (b:{shape.ToLabel} {{{shape.ToKey}: row.to_id}}) " +
            $"MERGE (a)-[r:{shape.Type}]->(b) " +
            $"{set} " +
            "RETURN count(r) AS matched";
        await using var session = driver.AsyncSession();
        return await session.ExecuteWriteAsync(async tx =>
            (await (await tx.RunAsync(cypher, new { batch })).SingleAsync())["matched"].As<long>());
    }

    public async Task<(long Nodes, long Relationships)> TotalsAsync()
    {
        await using var session = driver.AsyncSession();
        var cursor = await session.RunAsync(
            "CALL { MATCH (n) RETURN count(n) AS nodes } CALL { MATCH ()-[r]->() RETURN count(r) AS rels } RETURN nodes, rels");
        var record = await cursor.SingleAsync();
        return (record["nodes"].As<long>(), record["rels"].As<long>());
    }
}
