using Neo4j.Driver;
using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Persistence;

public class Neo4jMigrationRepository
{
    private readonly IDriver _driver;
    private readonly ILogger<Neo4jMigrationRepository> _logger;

    public Neo4jMigrationRepository(IDriver driver, ILogger<Neo4jMigrationRepository> logger)
    {
        _driver = driver;
        _logger = logger;
    }

    /// <summary>
    /// Creates a resilient Neo4j driver with connection pooling and retry settings
    /// </summary>
    public static IDriver CreateResilientDriver(string uri, string username, string password)
    {
        return GraphDatabase.Driver(uri, AuthTokens.Basic(username, password), o => o
            .WithMaxConnectionPoolSize(50)
            .WithConnectionAcquisitionTimeout(TimeSpan.FromSeconds(30))
            .WithConnectionTimeout(TimeSpan.FromSeconds(30))
            .WithMaxTransactionRetryTime(TimeSpan.FromSeconds(30))
            .WithEncryptionLevel(EncryptionLevel.None)
            .WithConnectionIdleTimeout(TimeSpan.FromMinutes(10))
            .WithMaxConnectionLifetime(TimeSpan.FromHours(1)));
    }

    public async Task SaveDependencyGraphAsync(int runId, DependencyMap dependencyMap)
    {
        await using var session = _driver.AsyncSession();

        try
        {
            await session.ExecuteWriteAsync(async tx =>
            {
                // Create Run node
                await tx.RunAsync(
                    "MERGE (r:Run {id: $runId}) SET r.timestamp = datetime()",
                    new { runId });

                // Create CobolFile nodes from dependency relationships
                var allFiles = new HashSet<string>();
                foreach (var dep in dependencyMap.Dependencies)
                {
                    allFiles.Add(dep.SourceFile);
                    allFiles.Add(dep.TargetFile);
                }

                foreach (var fileName in allFiles)
                {
                    // Determine if it's a copybook based on file extension or usage
                    var isCopybook = fileName.EndsWith(".cpy", StringComparison.OrdinalIgnoreCase) ||
                                   fileName.EndsWith(".CPY", StringComparison.OrdinalIgnoreCase) ||
                                   dependencyMap.ReverseDependencies.ContainsKey(fileName);

                    await tx.RunAsync(@"
                        MERGE (f:CobolFile {fileName: $fileName})
                        SET f.isCopybook = $isCopybook,
                            f.runId = $runId,
                            f.lineCount = 0
                        WITH f
                        MATCH (r:Run {id: $runId})
                        MERGE (r)-[:ANALYZED]->(f)",
                        new
                        {
                            fileName,
                            isCopybook,
                            runId
                        });
                }

                // Create dependency relationships
                foreach (var dependency in dependencyMap.Dependencies)
                {
                    await tx.RunAsync(@"
                        MATCH (source:CobolFile {fileName: $source})
                        MATCH (target:CobolFile {fileName: $target})
                        MERGE (source)-[d:DEPENDS_ON]->(target)
                        SET d.type = $type,
                            d.lineNumber = $lineNumber,
                            d.context = $context,
                            d.runId = $runId",
                        new
                        {
                            source = dependency.SourceFile,
                            target = dependency.TargetFile,
                            type = dependency.DependencyType,
                            lineNumber = dependency.LineNumber,
                            context = dependency.Context ?? "",
                            runId
                        });
                }
            });

            _logger.LogInformation($"Saved dependency graph to Neo4j for run {runId}");
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error saving dependency graph to Neo4j");
            throw;
        }
    }

    public async Task<List<CircularDependency>> GetCircularDependenciesAsync(int runId)
    {
        await using var session = _driver.AsyncSession();

        var result = await session.ExecuteReadAsync(async tx =>
        {
            var cursor = await tx.RunAsync(@"
                MATCH path = (start:CobolFile)-[:DEPENDS_ON*2..10]->(start)
                WHERE start.runId = $runId
                WITH path, length(path) as pathLength
                ORDER BY pathLength
                RETURN [node in nodes(path) | node.fileName] as cycle, pathLength
                LIMIT 50",
                new { runId });

            var cycles = new List<CircularDependency>();
            await foreach (var record in cursor)
            {
                var fileNames = record["cycle"].As<List<string>>();
                cycles.Add(new CircularDependency
                {
                    Files = fileNames,
                    Length = record["pathLength"].As<int>()
                });
            }
            return cycles;
        });

        return result;
    }

    public async Task<ImpactAnalysis> GetImpactAnalysisAsync(string fileName, int runId)
    {
        await using var session = _driver.AsyncSession();

        var result = await session.ExecuteReadAsync(async tx =>
        {
            // Find all files affected by this file (downstream)
            var downstreamCursor = await tx.RunAsync(@"
                MATCH (source:CobolFile {fileName: $fileName})
                WHERE source.runId = $runId
                MATCH path = (source)<-[:DEPENDS_ON*1..5]-(affected)
                RETURN DISTINCT affected.fileName as fileName, length(path) as distance
                ORDER BY distance, fileName",
                new { fileName, runId });

            var affectedFiles = new List<(string FileName, int Distance)>();
            await foreach (var record in downstreamCursor)
            {
                affectedFiles.Add((record["fileName"].As<string>(), record["distance"].As<int>()));
            }

            // Find all dependencies of this file (upstream)
            var upstreamCursor = await tx.RunAsync(@"
                MATCH (source:CobolFile {fileName: $fileName})
                WHERE source.runId = $runId
                MATCH path = (source)-[:DEPENDS_ON*1..5]->(dependency)
                RETURN DISTINCT dependency.fileName as fileName, length(path) as distance
                ORDER BY distance, fileName",
                new { fileName, runId });

            var dependencies = new List<(string FileName, int Distance)>();
            await foreach (var record in upstreamCursor)
            {
                dependencies.Add((record["fileName"].As<string>(), record["distance"].As<int>()));
            }

            return new ImpactAnalysis
            {
                TargetFile = fileName,
                AffectedFiles = affectedFiles,
                Dependencies = dependencies,
                TotalAffected = affectedFiles.Count,
                TotalDependencies = dependencies.Count
            };
        });

        return result;
    }

    public async Task<List<CriticalFile>> GetCriticalFilesAsync(int runId)
    {
        await using var session = _driver.AsyncSession();

        var result = await session.ExecuteReadAsync(async tx =>
        {
            var cursor = await tx.RunAsync(@"
                MATCH (f:CobolFile)
                WHERE f.runId = $runId
                OPTIONAL MATCH (f)<-[incoming:DEPENDS_ON]-()
                OPTIONAL MATCH (f)-[outgoing:DEPENDS_ON]->()
                WITH f, count(DISTINCT incoming) as incomingCount, count(DISTINCT outgoing) as outgoingCount
                RETURN f.fileName as fileName, 
                       f.isCopybook as isCopybook,
                       incomingCount, 
                       outgoingCount,
                       incomingCount + outgoingCount as totalConnections
                ORDER BY totalConnections DESC
                LIMIT 20",
                new { runId });

            var files = new List<CriticalFile>();
            await foreach (var record in cursor)
            {
                files.Add(new CriticalFile
                {
                    FileName = record["fileName"].As<string>(),
                    IsCopybook = record["isCopybook"].As<bool>(),
                    IncomingDependencies = record["incomingCount"].As<int>(),
                    OutgoingDependencies = record["outgoingCount"].As<int>(),
                    TotalConnections = record["totalConnections"].As<int>()
                });
            }
            return files;
        });

        return result;
    }

    public async Task<GraphVisualizationData> GetDependencyGraphDataAsync(int runId)
    {
        _logger.LogInformation("🔍 Neo4j GetDependencyGraphDataAsync called with runId: {RunId}", runId);
        await using var session = _driver.AsyncSession();

        var result = await session.ExecuteReadAsync(async tx =>
        {
            _logger.LogInformation("🔍 Executing Neo4j query WHERE source.runId = {RunId} AND target.runId = {RunId2}", runId, runId);
            var cursor = await tx.RunAsync(@"
                MATCH (source:CobolFile)-[d:DEPENDS_ON]->(target:CobolFile)
                WHERE source.runId = $runId AND target.runId = $runId
                RETURN source.fileName as source, 
                       target.fileName as target,
                       source.isCopybook as sourceCopybook,
                       target.isCopybook as targetCopybook,
                       source.lineCount as sourceLineCount,
                       target.lineCount as targetLineCount,
                       d.type as dependencyType,
                       d.lineNumber as lineNumber,
                       d.context as context",
                new { runId });

            var nodes = new HashSet<GraphNode>();
            var edges = new List<GraphEdge>();

            await foreach (var record in cursor)
            {
                var source = record["source"].As<string>();
                var target = record["target"].As<string>();
                var sourceCopybook = record["sourceCopybook"].As<bool>();
                var targetCopybook = record["targetCopybook"].As<bool>();
                var sourceLineCount = record["sourceLineCount"].As<int>();
                var targetLineCount = record["targetLineCount"].As<int>();
                var depType = record["dependencyType"].As<string>();
                var lineNumber = record["lineNumber"].As<int?>();
                var context = record["context"].As<string>();

                nodes.Add(new GraphNode { Id = source, Label = source, IsCopybook = sourceCopybook, LineCount = sourceLineCount });
                nodes.Add(new GraphNode { Id = target, Label = target, IsCopybook = targetCopybook, LineCount = targetLineCount });

                edges.Add(new GraphEdge
                {
                    Source = source,
                    Target = target,
                    Type = depType,
                    LineNumber = lineNumber,
                    Context = context
                });
            }

            _logger.LogInformation("✅ Neo4j query result: {NodeCount} unique nodes, {EdgeCount} edges for runId {RunId}", nodes.Count, edges.Count, runId);
            return new GraphVisualizationData
            {
                Nodes = nodes.ToList(),
                Edges = edges
            };
        });

        return result;
    }

    public async Task<List<int>> GetAvailableRunsAsync()
    {
        await using var session = _driver.AsyncSession();

        var result = await session.ExecuteReadAsync(async tx =>
        {
            var cursor = await tx.RunAsync(@"
                MATCH (f:CobolFile)
                WHERE f.runId IS NOT NULL
                RETURN DISTINCT f.runId as runId
                ORDER BY runId DESC");

            var runIds = new List<int>();
            await foreach (var record in cursor)
            {
                runIds.Add(record["runId"].As<int>());
            }

            return runIds;
        });

        _logger.LogInformation("Found {Count} runs with graph data in Neo4j", result.Count);
        return result;
    }

    public async Task CloseAsync()
    {
        await _driver.DisposeAsync();
    }
}

// Supporting models
public class CircularDependency
{
    public List<string> Files { get; set; } = new();
    public int Length { get; set; }
}

public class ImpactAnalysis
{
    public string TargetFile { get; set; } = string.Empty;
    public List<(string FileName, int Distance)> AffectedFiles { get; set; } = new();
    public List<(string FileName, int Distance)> Dependencies { get; set; } = new();
    public int TotalAffected { get; set; }
    public int TotalDependencies { get; set; }
}

public class CriticalFile
{
    public string FileName { get; set; } = string.Empty;
    public bool IsCopybook { get; set; }
    public int IncomingDependencies { get; set; }
    public int OutgoingDependencies { get; set; }
    public int TotalConnections { get; set; }
}

public class GraphVisualizationData
{
    public List<GraphNode> Nodes { get; set; } = new();
    public List<GraphEdge> Edges { get; set; } = new();
}

public class GraphNode
{
    public string Id { get; set; } = string.Empty;
    public string Label { get; set; } = string.Empty;
    public bool IsCopybook { get; set; }
    public int LineCount { get; set; }
}

public class GraphEdge
{
    public string Source { get; set; } = string.Empty;
    public string Target { get; set; } = string.Empty;
    public string Type { get; set; } = string.Empty;
    public int? LineNumber { get; set; }
    public string? Context { get; set; }
}
