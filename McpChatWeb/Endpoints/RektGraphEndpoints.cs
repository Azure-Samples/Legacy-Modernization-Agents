using McpChatWeb.Services;
using Neo4j.Driver;

namespace McpChatWeb.Endpoints;

// The graph only exists after ./doctor.sh rekt-ingest. When unreachable these endpoints
// return an empty projection with a note, so the portal explains itself rather than breaking.
public static class RektGraphEndpoints
{
    public static void MapRektGraphEndpoints(this WebApplication app)
    {
        var group = app.MapGroup("/api/graph/rekt").WithTags("REKT Graph");

        group.MapGet("/runs", async (
            ILoggerFactory loggerFactory,
            CancellationToken cancellationToken) =>
        {
            var logger = loggerFactory.CreateLogger("RektGraph");
            if (!RektNeo4j.IsConfigured)
                return Results.Ok(new { runs = Array.Empty<object>(), note = RektNeo4j.NotConfiguredNote });

            try
            {
                await using var session = RektNeo4j.Shared.AsyncSession();
                var cursor = await session.RunAsync(@"
                    MATCH (f:CobolFile)
                    WHERE f.runId IS NOT NULL
                    WITH f.runId AS runId,
                         count(DISTINCT f.fileName) AS fileCount,
                         max(COALESCE(f.lineCount, 0)) AS maxLines
                    RETURN runId, fileCount, maxLines
                    ORDER BY runId DESC");

                var runs = new List<object>();
                await cursor.ForEachAsync(r => runs.Add(new
                {
                    runId = r["runId"].As<long>(),
                    fileCount = r["fileCount"].As<int>(),
                    maxLines = r["maxLines"].As<int>(),
                }));

                return Results.Ok(new { runs, note = (string?)null });
            }
            catch (Exception ex)
            {
                logger.LogWarning(ex, "REKT graph unavailable while listing scan runs.");
                return Results.Ok(new { runs = Array.Empty<object>(), note = Unavailable });
            }
        })
        .WithName("GetRektScanRuns")
        .WithSummary("Scan runs present in the REKT graph, newest first.");

        group.MapGet("/architect", async (
            long? scanRunId,
            ILoggerFactory loggerFactory,
            CancellationToken cancellationToken) =>
        {
            var logger = loggerFactory.CreateLogger("RektGraph");
            if (!RektNeo4j.IsConfigured)
                return EmptyArchitecture(RektNeo4j.NotConfiguredNote);

            try
            {
                await using var session = RektNeo4j.Shared.AsyncSession();

                var programs = new List<object>();
                var seen = new HashSet<string>(StringComparer.OrdinalIgnoreCase);

                await foreach (var record in QueryFilesAsync(session, scanRunId))
                {
                    var fileName = record.FileName;
                    if (!seen.Add(fileName)) continue;
                    programs.Add(new
                    {
                        fileName,
                        isCopybook = record.IsCopybook,
                        lineCount = record.LineCount,
                        hasAst = record.HasAst,
                    });
                }

                var dependencies = await ReadDependenciesAsync(session);
                return Results.Ok(new { programs, dependencies, note = (string?)null });
            }
            catch (Exception ex)
            {
                logger.LogWarning(ex, "REKT graph unavailable while building the architecture projection.");
                return EmptyArchitecture(Unavailable);
            }
        })
        .WithName("GetRektArchitecture")
        .WithSummary("Files and dependency edges from the REKT graph.");

        group.MapGet("/services", async (
            long? scanRunId,
            ILoggerFactory loggerFactory,
            CancellationToken cancellationToken) =>
        {
            var logger = loggerFactory.CreateLogger("RektGraph");
            if (!RektNeo4j.IsConfigured)
                return EmptyServices(RektNeo4j.NotConfiguredNote);

            try
            {
                await using var session = RektNeo4j.Shared.AsyncSession();

                var runFilter = scanRunId.HasValue ? "AND f.runId = $scanRunId" : "";
                var cursor = await session.RunAsync($@"
                    MATCH (f:CobolFile)
                    WHERE f.runId IS NOT NULL {runFilter}
                    WITH f.fileName AS fileName, max(f.runId) AS latestRun, collect(f) AS files
                    WITH fileName, latestRun, [x IN files WHERE x.runId = latestRun][0] AS f
                    OPTIONAL MATCH (f)-[:HAS_AST]->(root:ASTNode)
                    OPTIONAL MATCH (root)-[:CONTAINS*1..3]->(n:ASTNode)
                    WITH f, fileName, root IS NOT NULL AS hasAst,
                        count(DISTINCT CASE WHEN n.nodeType IN ['DIALECT','DIALECT_CONTAINER'] THEN n END) AS sqlCount,
                        count(DISTINCT CASE WHEN n.nodeType = 'CALL' THEN n END) AS callCount,
                        count(DISTINCT CASE WHEN n.nodeType = 'PERFORM' THEN n END) AS performCount,
                        count(DISTINCT CASE WHEN n.nodeType = 'DISPLAY' THEN n END) AS displayCount
                    RETURN DISTINCT fileName, f.isCopybook AS isCopybook, f.lineCount AS lineCount,
                        hasAst, sqlCount, callCount, performCount, displayCount",
                    scanRunId.HasValue ? new { scanRunId = scanRunId.Value } : null);

                var nodes = new List<object>();
                var seen = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
                await cursor.ForEachAsync(r =>
                {
                    var fileName = r["fileName"].As<string>();
                    if (!seen.Add(fileName)) return;
                    nodes.Add(new
                    {
                        id = fileName,
                        type = r["isCopybook"].As<bool?>() == true ? "copybook" : "program",
                        lineCount = r["lineCount"].As<int?>() ?? 0,
                        hasAst = r["hasAst"].As<bool>(),
                        sqlCount = r["sqlCount"].As<int>(),
                        callCount = r["callCount"].As<int>(),
                        performCount = r["performCount"].As<int>(),
                        displayCount = r["displayCount"].As<int>(),
                    });
                });

                var edges = await ReadDependenciesAsync(session);
                return Results.Ok(new { nodes, edges, note = (string?)null });
            }
            catch (Exception ex)
            {
                logger.LogWarning(ex, "REKT graph unavailable while building the services projection.");
                return EmptyServices(Unavailable);
            }
        })
        .WithName("GetRektServices")
        .WithSummary("Deduplicated program graph with per-file AST statement counts.");
    }

    private const string Unavailable =
        "REKT graph is unavailable. Start it with ./doctor.sh rekt-full and confirm bolt://localhost:7688 is reachable.";

    private static IResult EmptyArchitecture(string note) =>
        Results.Ok(new
        {
            programs = Array.Empty<object>(),
            dependencies = Array.Empty<object>(),
            note,
        });

    private static IResult EmptyServices(string note) =>
        Results.Ok(new
        {
            nodes = Array.Empty<object>(),
            edges = Array.Empty<object>(),
            note,
        });

    private sealed record FileRecord(string FileName, bool IsCopybook, int LineCount, bool HasAst);

    // Newest run per file name, so a re-ingest does not duplicate nodes. hasAst reads the
    // HAS_AST edge, not a name convention, so the two projections cannot disagree on it.
    private static async IAsyncEnumerable<FileRecord> QueryFilesAsync(IAsyncSession session, long? scanRunId)
    {
        var runFilter = scanRunId.HasValue ? "AND f.runId = $scanRunId" : "";
        var cursor = await session.RunAsync($@"
            MATCH (f:CobolFile)
            WHERE f.runId IS NOT NULL {runFilter}
            WITH f.fileName AS fileName, max(f.runId) AS latestRun, collect(f) AS files
            WITH fileName, latestRun, [x IN files WHERE x.runId = latestRun][0] AS f
            OPTIONAL MATCH (f)-[:HAS_AST]->(root:ASTNode)
            RETURN DISTINCT fileName, f.isCopybook AS isCopybook, f.lineCount AS lineCount,
                root IS NOT NULL AS hasAst",
            scanRunId.HasValue ? new { scanRunId = scanRunId.Value } : null);

        await foreach (var record in cursor)
        {
            yield return new FileRecord(
                record["fileName"].As<string>(),
                record["isCopybook"].As<bool?>() ?? false,
                record["lineCount"].As<int?>() ?? 0,
                record["hasAst"].As<bool>());
        }
    }

    private static async Task<List<object>> ReadDependenciesAsync(IAsyncSession session)
    {
        var cursor = await session.RunAsync(@"
            MATCH (a:CobolFile)-[d:DEPENDS_ON]->(b:CobolFile)
            RETURN DISTINCT a.fileName AS source, b.fileName AS target, d.type AS type");

        var edges = new List<object>();
        var seen = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        await cursor.ForEachAsync(r =>
        {
            var source = r["source"].As<string>();
            var target = r["target"].As<string>();
            var type = r["type"].As<string?>() ?? "DEPENDS_ON";
            if (!seen.Add($"{source}->{target}:{type}")) return;
            edges.Add(new { source, target, type });
        });
        return edges;
    }
}
