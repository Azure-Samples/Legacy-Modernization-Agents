using System.Runtime.CompilerServices;
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
        var logger = app.Services.GetRequiredService<ILoggerFactory>().CreateLogger("RektGraph");

        group.MapGet("/runs", async (
            CancellationToken cancellationToken) =>
        {
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
                await foreach (var r in cursor.WithCancellation(cancellationToken))
                {
                    runs.Add(new
                    {
                        runId = r["runId"].As<long>(),
                        fileCount = r["fileCount"].As<int>(),
                        maxLines = r["maxLines"].As<int>(),
                    });
                }

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
            string? scanRunId,
            CancellationToken cancellationToken) =>
        {
            if (!RektNeo4j.IsConfigured)
                return EmptyArchitecture(RektNeo4j.NotConfiguredNote);

            try
            {
                await using var session = RektNeo4j.Shared.AsyncSession();
                var runId = await ResolveScanRunIdAsync(session, scanRunId, cancellationToken);

                var programs = new List<object>();
                var seen = new HashSet<string>(StringComparer.OrdinalIgnoreCase);

                await foreach (var record in QueryFilesAsync(session, runId, cancellationToken))
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

                var dependencies = await ReadDependenciesAsync(session, runId, seen, cancellationToken);
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
            string? scanRunId,
            CancellationToken cancellationToken) =>
        {
            if (!RektNeo4j.IsConfigured)
                return EmptyServices(RektNeo4j.NotConfiguredNote);

            try
            {
                await using var session = RektNeo4j.Shared.AsyncSession();
                var runId = await ResolveScanRunIdAsync(session, scanRunId, cancellationToken);

                var runFilter = runId.HasValue ? "AND f.runId = $scanRunId" : "";
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
                    runId.HasValue ? new { scanRunId = runId.Value } : null);

                var nodes = new List<object>();
                var seen = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
                await foreach (var r in cursor.WithCancellation(cancellationToken))
                {
                    var fileName = r["fileName"].As<string>();
                    if (!seen.Add(fileName)) continue;
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
                }

                var edges = await ReadDependenciesAsync(session, runId, seen, cancellationToken);
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

        // Programs the graph holds AST for, which is the inventory the explorer offers.
        group.MapGet("/files", async (long? scanRunId, CancellationToken cancellationToken) =>
        {
            if (!RektNeo4j.IsConfigured)
                return Results.Ok(Array.Empty<object>());
            try
            {

                var driver = McpChatWeb.Services.RektNeo4j.Shared;
                await using var session = driver.AsyncSession();

                // Return files with AST data + flag which ones have CFG edges.
                // When a specific scan run is requested, filter to that run only.
                //
                // Otherwise report the latest scan, meaning the highest run id in the graph and
                // the programs belonging to it. Taking max(runId) per program instead keeps every
                // program any scan ever saw: the graph accumulates across runs, so an estate that
                // once held 173 programs still offered all of them after a rescan found 36, and a
                // caller picking one would be working from a program that no longer exists.
                var result = scanRunId.HasValue
                    ? await session.RunAsync(@"
                        MATCH (a:ASTNode) WHERE a.program IS NOT NULL AND coalesce(a.runId, 0) = $runId
                        WITH DISTINCT a.program AS program
                        OPTIONAL MATCH (cfg:ASTNode {program: program})-[:FOLLOWED_BY|JUMPS_TO]->()
                        WHERE coalesce(cfg.runId, 0) = $runId
                        WITH program, count(cfg) > 0 AS hasCfg
                        RETURN program AS name, hasCfg
                        ORDER BY program",
                        new { runId = scanRunId.Value })
                    : await session.RunAsync(@"
                        MATCH (a:ASTNode) WHERE a.program IS NOT NULL
                        WITH max(coalesce(a.runId, 0)) AS latestRun
                        MATCH (b:ASTNode)
                        WHERE b.program IS NOT NULL AND coalesce(b.runId, 0) = latestRun
                        WITH DISTINCT b.program AS program, latestRun
                        OPTIONAL MATCH (cfg:ASTNode {program: program})-[:FOLLOWED_BY|JUMPS_TO]->()
                        WHERE coalesce(cfg.runId, 0) = latestRun
                        WITH program, count(cfg) > 0 AS hasCfg
                        RETURN program AS name, hasCfg
                        ORDER BY program");

                var files = new List<object>();
                await result.ForEachAsync(r => files.Add(new
                {
                    name = r["name"].As<string>(),
                    hasAst = true,
                    hasCfg = r["hasCfg"].As<bool>()
                }));

                return Results.Ok(files);
            }
            // Cancellation is the caller's decision, not a graph failure.
            catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
            {
                throw;
            }
            catch (Exception ex) when (
                ex is Neo4j.Driver.Neo4jException      // driver and server-side errors
                or IOException                        // connection torn down mid-read
                or InvalidOperationException)
            {
                Console.WriteLine($"⚠️ Rekt files endpoint: {ex.Message}");
                return Results.Ok(Array.Empty<object>());
            }
        });

        // Sections and paragraphs for one program, with statement counts gathered separately so a
        // slow count cannot take the whole response down with it.
        group.MapGet("/structure", async (string file, long? scanRunId, CancellationToken cancellationToken) =>
        {
            if (!RektNeo4j.IsConfigured)
                return Results.Ok(new { nodes = Array.Empty<object>(), edges = Array.Empty<object>(), note = RektNeo4j.NotConfiguredNote });
            try
            {

                var driver = McpChatWeb.Services.RektNeo4j.Shared;
                await using var session = driver.AsyncSession();

                // Server-side ceiling on every Cypher call — without this, a large
                // rekt graph (300K+ nodes) can stall the unbounded CONTAINS* walks
                // for minutes, holding the HTTP request hostage. 12s is enough for
                // healthy programs and ensures the UI's 15s fetch timeout always
                // wins before the connection drops.
                Action<Neo4j.Driver.TransactionConfigBuilder> txTimeout = b => b.WithTimeout(TimeSpan.FromSeconds(12));

                // Resolve file name (exact, flow-ast- prefix, stripped)
                var candidates = new[] { file, $"flow-ast-{file}", file.Replace("flow-ast-", "") };
                string? matchedProgram = null;
                var sections = new List<object>();

                foreach (var candidate in candidates)
                {
                    // Use pinned scanRunId if provided, otherwise auto-select the latest run for this file.
                    var structRunId = scanRunId.GetValueOrDefault(0);
                    var structRunInit = structRunId > 0
                        ? "WITH $runId AS _r"
                        : "MATCH (a0:ASTNode {program: $file}) WITH max(coalesce(a0.runId, 0)) AS _r";
                    // PHASE 1: get sections + paragraphs WITHOUT statement counts.
                    // This is the cheap, almost-certain-to-finish part — just a
                    // 2-hop walk from the program root. We do the statement counts
                    // in a separate query (Phase 2) so a slow stmt count doesn't
                    // kill the whole response. Each phase is independently bounded
                    // by txTimeout.
                    var result = await session.RunAsync($@"
                        {structRunInit}
                        MATCH (root:ASTNode {{program: $file}})-[:CONTAINS*1..2]->(sec:ASTNode)
                        WHERE coalesce(root.runId, 0) = _r
                          AND coalesce(sec.runId, 0) = _r
                          AND sec.nodeType IN ['SECTION', 'PARAGRAPHS']
                        OPTIONAL MATCH (sec)-[:CONTAINS*1..2]->(para:ASTNode)
                        WHERE para.nodeType IN ['PARAGRAPH', 'PARAGRAPH_NAME']
                          AND coalesce(para.runId, 0) = _r
                        RETURN sec.id AS sectionId, sec.name AS sectionName, sec.nodeType AS sectionType,
                               sec.startLine AS secStart, sec.endLine AS secEnd,
                               para.id AS paraId, para.name AS paraName, para.nodeType AS paraType,
                               para.startLine AS paraStart, para.endLine AS paraEnd
                        ORDER BY sec.name, para.name",
                        new Dictionary<string, object> { ["file"] = candidate, ["runId"] = structRunId },
                        txTimeout);

                    await result.ForEachAsync(r => sections.Add(new
                    {
                        sectionId = r["sectionId"].As<string?>() ?? "",
                        sectionName = r["sectionName"].As<string?>() ?? "UNNAMED",
                        sectionType = r["sectionType"].As<string?>() ?? "",
                        secStart = r["secStart"].As<int?>() ?? -1,
                        secEnd = r["secEnd"].As<int?>() ?? -1,
                        paraId = r["paraId"].As<string?>() ?? "",
                        paraName = r["paraName"].As<string?>() ?? "",
                        paraType = r["paraType"].As<string?>() ?? "",
                        paraStart = r["paraStart"].As<int?>() ?? -1,
                        paraEnd = r["paraEnd"].As<int?>() ?? -1,
                        stmtCount = 0,
                        sqlCount = 0,
                        performCount = 0,
                        moveCount = 0,
                        branchCount = 0,
                        callCount = 0
                    }));

                    if (sections.Count > 0) { matchedProgram = candidate; break; }
                }

                if (matchedProgram == null)
                    return Results.NotFound(new { error = $"No structure data for {file}" });

                // PERFORM edges between paragraphs (control flow) — latest-run-per-program.
                // CONTAINS depth bounded to keep the query under the 12s ceiling on
                // large rekt graphs (300K+ nodes). Wrapped in its own try/catch so a
                // CFG timeout still returns the sections payload with empty edges
                // rather than failing the whole request.
                var performEdges = new List<object>();
                try
                {
                    var cfgResult = await session.RunAsync(@"
                        MATCH (a0:ASTNode {program: $file})
                        WITH max(coalesce(a0.runId, 0)) AS _r
                        MATCH (p:ASTNode {program: $file, nodeType: 'PERFORM'})
                        WHERE coalesce(p.runId, 0) = _r AND p.name IS NOT NULL AND p.name <> ''
                        MATCH (caller:ASTNode {program: $file})-[c:CONTAINS*1..6]->(p)
                        WHERE coalesce(caller.runId, 0) = _r
                          AND caller.nodeType IN ['PARAGRAPH', 'PARAGRAPHS', 'SECTION']
                          AND caller.name IS NOT NULL AND caller.name <> '' AND caller.name <> 'para-group:'
                        WITH p, caller.name AS callerName, size(c) AS dist
                        ORDER BY dist ASC
                        WITH p, head(collect(callerName)) AS caller
                        RETURN DISTINCT caller AS caller, p.name AS target, p.label AS label
                        ORDER BY caller
                        LIMIT 800",
                        new Dictionary<string, object> { ["file"] = matchedProgram! },
                        txTimeout);

                    await cfgResult.ForEachAsync(r => performEdges.Add(new
                    {
                        from = r["caller"].As<string?>() ?? "MAIN",
                        to = r["target"].As<string?>() ?? "",
                        label = r["label"].As<string?>() ?? "PERFORM"
                    }));
                }
                catch (Neo4j.Driver.Neo4jException cfgEx) when (cfgEx.Message.Contains("timeout", StringComparison.OrdinalIgnoreCase) || cfgEx.Message.Contains("transaction has been terminated", StringComparison.OrdinalIgnoreCase))
                {
                    Console.WriteLine($"⏱ CFG sub-query timed out for {matchedProgram} — returning sections without performEdges.");
                }

                return Results.Ok(new { program = matchedProgram, sections, performEdges });
            }
            catch (Neo4j.Driver.Neo4jException nex) when (nex.Message.Contains("timeout", StringComparison.OrdinalIgnoreCase) || nex.Message.Contains("transaction has been terminated", StringComparison.OrdinalIgnoreCase))
            {
                // Server-side 12s ceiling hit — let the UI fall back to deps-only.
                Console.WriteLine($"⏱ Structure endpoint timeout for {file}: {nex.Message}");
                return Results.Json(new { error = "structure_query_timeout", file, message = "Cypher exceeded 12s ceiling — graph is large or query plan suboptimal." }, statusCode: 504);
            }
            // Cancellation is the caller's decision, not a graph failure.
            catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
            {
                throw;
            }
            catch (Exception ex) when (
                ex is Neo4j.Driver.Neo4jException      // driver and server-side errors
                or IOException                        // connection torn down mid-read
                or InvalidOperationException)
            {
                Console.WriteLine($"⚠️ Structure endpoint: {ex.Message}");
                return Results.Problem(ex.Message);
            }
        });

        // The raw AST as stored, for reading what the parser actually produced rather than a projection of it.
        group.MapGet("/ast", async (string file, long? scanRunId, CancellationToken cancellationToken) =>
        {
            if (!RektNeo4j.IsConfigured)
                return Results.Ok(new { nodes = Array.Empty<object>(), edges = Array.Empty<object>(), note = RektNeo4j.NotConfiguredNote });
            try
            {

                var driver = McpChatWeb.Services.RektNeo4j.Shared;
                await using var session = driver.AsyncSession();

                // Try exact match first, then with flow-ast- prefix (rekt naming convention)
                var candidates = new[] { file, $"flow-ast-{file}", file.Replace("flow-ast-", "") };
                var nodes = new List<object>();
                var edges = new List<object>();
                string? matchedProgram = null;
                var astRunId = scanRunId.GetValueOrDefault(0);
                var astRunInit = astRunId > 0
                    ? "WITH $runId AS _r"
                    : "MATCH (a0:ASTNode {program: $file}) WITH max(coalesce(a0.runId, 0)) AS _r";

                foreach (var candidate in candidates)
                {
                    var nodeResult = await session.RunAsync($@"
                        {astRunInit}
                        MATCH (a:ASTNode {{program: $file}})
                        WHERE coalesce(a.runId, 0) = _r
                        RETURN a.id AS id, a.nodeType AS nodeType, a.label AS label,
                               a.originalText AS originalText, a.startLine AS startLine,
                               a.endLine AS endLine, a.name AS name,
                               a.section AS section, a.paragraph AS paragraph",
                        new { file = candidate, runId = astRunId });

                    await nodeResult.ForEachAsync(r => nodes.Add(new
                    {
                        id = r["id"].As<string>(),
                        nodeType = r["nodeType"].As<string?>() ?? "",
                        label = r["label"].As<string?>() ?? r["nodeType"].As<string?>() ?? "",
                        originalText = r["originalText"].As<string?>() ?? "",
                        startLine = r["startLine"].As<int?>() ?? 0,
                        endLine = r["endLine"].As<int?>() ?? 0,
                        name = r["name"].As<string?>() ?? "",
                        section = r["section"].As<string?>() ?? "",
                        paragraph = r["paragraph"].As<string?>() ?? ""
                    }));

                    if (nodes.Count > 0) { matchedProgram = candidate; break; }
                }

                if (matchedProgram != null)
                {
                    var edgeResult = await session.RunAsync($@"
                        {astRunInit}
                        MATCH (a:ASTNode {{program: $file}})-[r:CONTAINS|FOLLOWED_BY|JUMPS_TO]->(b:ASTNode {{program: $file}})
                        WHERE coalesce(a.runId, 0) = _r AND coalesce(b.runId, 0) = _r
                        RETURN a.id AS source, b.id AS target, type(r) AS type",
                        new { file = matchedProgram, runId = astRunId });

                    await edgeResult.ForEachAsync(r => edges.Add(new
                    {
                        source = r["source"].As<string>(),
                        target = r["target"].As<string>(),
                        type = r["type"].As<string>()
                    }));
                }

                if (nodes.Count == 0)
                    return Results.NotFound(new { error = $"No AST data for {file}. Run: ./doctor.sh rekt" });

                return Results.Ok(new { nodes, edges });
            }
            // Cancellation is the caller's decision, not a graph failure.
            catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
            {
                throw;
            }
            catch (Exception ex) when (
                ex is Neo4j.Driver.Neo4jException      // driver and server-side errors
                or IOException                        // connection torn down mid-read
                or InvalidOperationException)
            {
                Console.WriteLine($"⚠️ AST endpoint: {ex.Message}");
                return Results.Problem(ex.Message);
            }
        });

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

    // "latest" must mean the most recent scan, not the most recent version of every file ever
    // scanned: without this an estate shows programs that were deleted several scans ago.
    // Only an explicit "all" spans runs.
    private static async Task<long?> ResolveScanRunIdAsync(
        IAsyncSession session, string? requested, CancellationToken cancellationToken)
    {
        if (string.Equals(requested, "all", StringComparison.OrdinalIgnoreCase)) return null;
        if (long.TryParse(requested, out var explicitRun)) return explicitRun;

        var cursor = await session.RunAsync(
            "MATCH (f:CobolFile) WHERE f.runId IS NOT NULL RETURN max(f.runId) AS latest");
        await foreach (var record in cursor.WithCancellation(cancellationToken))
            return record["latest"].As<long?>();
        return null;
    }

    // Newest run per file name, so a re-ingest does not duplicate nodes. hasAst reads the
    // HAS_AST edge, not a name convention, so the two projections cannot disagree on it.
    private static async IAsyncEnumerable<FileRecord> QueryFilesAsync(
        IAsyncSession session, long? scanRunId,
        [EnumeratorCancellation] CancellationToken cancellationToken)
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

        await foreach (var record in cursor.WithCancellation(cancellationToken))
        {
            yield return new FileRecord(
                record["fileName"].As<string>(),
                record["isCopybook"].As<bool?>() ?? false,
                record["lineCount"].As<int?>() ?? 0,
                record["hasAst"].As<bool>());
        }
    }

    // Edges are scoped to the same run and the same file set as the nodes: an unscoped read
    // leaks historical edges into a run-specific projection and points at files it does not contain.
    private static async Task<List<object>> ReadDependenciesAsync(
        IAsyncSession session,
        long? scanRunId,
        IReadOnlySet<string> includedFiles,
        CancellationToken cancellationToken)
    {
        var runFilter = scanRunId.HasValue
            ? "WHERE a.runId = $scanRunId AND b.runId = $scanRunId"
            : "";

        var cursor = await session.RunAsync($@"
            MATCH (a:CobolFile)-[d:DEPENDS_ON]->(b:CobolFile)
            {runFilter}
            RETURN DISTINCT a.fileName AS source, b.fileName AS target, d.type AS type",
            scanRunId.HasValue ? new { scanRunId = scanRunId.Value } : null);

        var edges = new List<object>();
        var seen = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        await foreach (var r in cursor.WithCancellation(cancellationToken))
        {
            var source = r["source"].As<string>();
            var target = r["target"].As<string>();
            if (!includedFiles.Contains(source) || !includedFiles.Contains(target)) continue;

            var type = r["type"].As<string?>() ?? "DEPENDS_ON";
            if (!seen.Add($"{source}->{target}:{type}")) continue;
            edges.Add(new { source, target, type });
        }
        return edges;
    }
}
