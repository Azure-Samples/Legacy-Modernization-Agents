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

    // ============================================================
    // CHUNKING SUPPORT METHODS
    // ============================================================

    /// <summary>
    /// Saves chunk metadata to Neo4j for cross-file dependency tracking.
    /// </summary>
    public async Task SaveChunkAsync(int runId, ChunkNode chunk)
    {
        await using var session = _driver.AsyncSession();

        try
        {
            await session.ExecuteWriteAsync(async tx =>
            {
                var chunkId = $"{runId}:{chunk.SourceFile}:{chunk.ChunkIndex}";
                var semanticUnitsJson = System.Text.Json.JsonSerializer.Serialize(chunk.SemanticUnits);

                await tx.RunAsync(@"
                    MERGE (c:Chunk {id: $chunkId})
                    SET c.runId = $runId,
                        c.sourceFile = $sourceFile,
                        c.chunkIndex = $chunkIndex,
                        c.startLine = $startLine,
                        c.endLine = $endLine,
                        c.status = $status,
                        c.tokensUsed = $tokensUsed,
                        c.processingTimeMs = $processingTimeMs,
                        c.semanticUnits = $semanticUnits,
                        c.completedAt = $completedAt
                    WITH c
                    MATCH (f:CobolFile {fileName: $sourceFile, runId: $runId})
                    MERGE (f)-[:HAS_CHUNK]->(c)",
                    new
                    {
                        chunkId,
                        runId,
                        sourceFile = chunk.SourceFile,
                        chunkIndex = chunk.ChunkIndex,
                        startLine = chunk.StartLine,
                        endLine = chunk.EndLine,
                        status = chunk.Status,
                        tokensUsed = chunk.TokensUsed,
                        processingTimeMs = chunk.ProcessingTimeMs,
                        semanticUnits = semanticUnitsJson,
                        completedAt = chunk.CompletedAt?.ToString("o")
                    });
            });

            _logger.LogDebug("Saved chunk {ChunkIndex} for {SourceFile} to Neo4j", chunk.ChunkIndex, chunk.SourceFile);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error saving chunk to Neo4j for {SourceFile} chunk {ChunkIndex}", chunk.SourceFile, chunk.ChunkIndex);
            throw;
        }
    }

    /// <summary>
    /// Saves a method signature to Neo4j for cross-file consistency tracking.
    /// </summary>
    public async Task SaveSignatureAsync(int runId, SignatureNode signature)
    {
        await using var session = _driver.AsyncSession();

        try
        {
            await session.ExecuteWriteAsync(async tx =>
            {
                var signatureId = $"{runId}:{signature.SourceFile}:{signature.LegacyName}";

                await tx.RunAsync(@"
                    MERGE (s:Signature {id: $signatureId})
                    SET s.runId = $runId,
                        s.sourceFile = $sourceFile,
                        s.legacyName = $legacyName,
                        s.targetMethodName = $targetMethodName,
                        s.targetSignature = $targetSignature,
                        s.returnType = $returnType,
                        s.definedInChunk = $definedInChunk
                    WITH s
                    MATCH (f:CobolFile {fileName: $sourceFile, runId: $runId})
                    MERGE (f)-[:DEFINES_SIGNATURE]->(s)",
                    new
                    {
                        signatureId,
                        runId,
                        sourceFile = signature.SourceFile,
                        legacyName = signature.LegacyName,
                        targetMethodName = signature.TargetMethodName,
                        targetSignature = signature.TargetSignature,
                        returnType = signature.ReturnType,
                        definedInChunk = signature.DefinedInChunk
                    });
            });

            _logger.LogDebug("Saved signature {LegacyName} -> {TargetMethodName} to Neo4j", signature.LegacyName, signature.TargetMethodName);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error saving signature to Neo4j");
            throw;
        }
    }

    /// <summary>
    /// Creates a cross-chunk dependency relationship in Neo4j.
    /// </summary>
    public async Task SaveChunkDependencyAsync(int runId, ChunkDependency dependency)
    {
        await using var session = _driver.AsyncSession();

        try
        {
            await session.ExecuteWriteAsync(async tx =>
            {
                await tx.RunAsync(@"
                    MATCH (source:Chunk {id: $sourceChunkId})
                    MATCH (target:Chunk {id: $targetChunkId})
                    MERGE (source)-[d:CALLS_IN_CHUNK]->(target)
                    SET d.dependencyType = $dependencyType,
                        d.callerMethod = $callerMethod,
                        d.targetMethod = $targetMethod,
                        d.runId = $runId",
                    new
                    {
                        sourceChunkId = dependency.SourceChunkId,
                        targetChunkId = dependency.TargetChunkId,
                        dependencyType = dependency.DependencyType,
                        callerMethod = dependency.CallerMethod,
                        targetMethod = dependency.TargetMethod,
                        runId
                    });
            });

            _logger.LogDebug("Saved chunk dependency from {Source} to {Target}", dependency.SourceChunkId, dependency.TargetChunkId);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error saving chunk dependency to Neo4j");
            throw;
        }
    }

    /// <summary>
    /// Gets all chunks for a file in a specific run.
    /// </summary>
    public async Task<List<ChunkNode>> GetChunksForFileAsync(int runId, string sourceFile)
    {
        await using var session = _driver.AsyncSession();

        var result = await session.ExecuteReadAsync(async tx =>
        {
            var cursor = await tx.RunAsync(@"
                MATCH (c:Chunk)
                WHERE c.runId = $runId AND c.sourceFile = $sourceFile
                RETURN c.id as id, c.sourceFile as sourceFile, c.chunkIndex as chunkIndex,
                       c.startLine as startLine, c.endLine as endLine, c.status as status,
                       c.tokensUsed as tokensUsed, c.processingTimeMs as processingTimeMs,
                       c.semanticUnits as semanticUnits, c.completedAt as completedAt
                ORDER BY c.chunkIndex",
                new { runId, sourceFile });

            var chunks = new List<ChunkNode>();
            await foreach (var record in cursor)
            {
                var semanticUnitsJson = record["semanticUnits"].As<string>() ?? "[]";
                var semanticUnits = System.Text.Json.JsonSerializer.Deserialize<List<string>>(semanticUnitsJson) ?? new();

                chunks.Add(new ChunkNode
                {
                    Id = record["id"].As<string>(),
                    SourceFile = record["sourceFile"].As<string>(),
                    ChunkIndex = record["chunkIndex"].As<int>(),
                    StartLine = record["startLine"].As<int>(),
                    EndLine = record["endLine"].As<int>(),
                    Status = record["status"].As<string>(),
                    TokensUsed = record["tokensUsed"].As<int>(),
                    ProcessingTimeMs = record["processingTimeMs"].As<long>(),
                    SemanticUnits = semanticUnits,
                    CompletedAt = record["completedAt"].As<string?>() != null 
                        ? DateTime.Parse(record["completedAt"].As<string>()) 
                        : null
                });
            }
            return chunks;
        });

        return result;
    }

    /// <summary>
    /// Gets chunk processing status for all files in a run.
    /// </summary>
    public async Task<List<ChunkProcessingStatus>> GetChunkProcessingStatusAsync(int runId)
    {
        await using var session = _driver.AsyncSession();

        var result = await session.ExecuteReadAsync(async tx =>
        {
            var cursor = await tx.RunAsync(@"
                MATCH (c:Chunk)
                WHERE c.runId = $runId
                WITH c.sourceFile as sourceFile, 
                     count(c) as totalChunks,
                     sum(CASE WHEN c.status = 'Completed' THEN 1 ELSE 0 END) as completedChunks,
                     sum(CASE WHEN c.status = 'Failed' THEN 1 ELSE 0 END) as failedChunks,
                     sum(CASE WHEN c.status = 'Pending' OR c.status = 'Processing' THEN 1 ELSE 0 END) as pendingChunks,
                     sum(c.processingTimeMs) as totalProcessingTimeMs,
                     sum(c.tokensUsed) as totalTokensUsed
                RETURN sourceFile, totalChunks, completedChunks, failedChunks, pendingChunks,
                       totalProcessingTimeMs, totalTokensUsed
                ORDER BY sourceFile",
                new { runId });

            var statuses = new List<ChunkProcessingStatus>();
            await foreach (var record in cursor)
            {
                var totalChunks = record["totalChunks"].As<int>();
                var completedChunks = record["completedChunks"].As<int>();

                statuses.Add(new ChunkProcessingStatus
                {
                    SourceFile = record["sourceFile"].As<string>(),
                    TotalChunks = totalChunks,
                    CompletedChunks = completedChunks,
                    FailedChunks = record["failedChunks"].As<int>(),
                    PendingChunks = record["pendingChunks"].As<int>(),
                    ProgressPercentage = totalChunks > 0 ? (double)completedChunks / totalChunks * 100 : 0,
                    TotalProcessingTimeMs = record["totalProcessingTimeMs"].As<long>(),
                    TotalTokensUsed = record["totalTokensUsed"].As<int>()
                });
            }
            return statuses;
        });

        return result;
    }

    /// <summary>
    /// Gets all signatures for a file in a run.
    /// </summary>
    public async Task<List<SignatureNode>> GetSignaturesForFileAsync(int runId, string sourceFile)
    {
        await using var session = _driver.AsyncSession();

        var result = await session.ExecuteReadAsync(async tx =>
        {
            var cursor = await tx.RunAsync(@"
                MATCH (s:Signature)
                WHERE s.runId = $runId AND s.sourceFile = $sourceFile
                RETURN s.id as id, s.sourceFile as sourceFile, s.legacyName as legacyName,
                       s.targetMethodName as targetMethodName, s.targetSignature as targetSignature,
                       s.returnType as returnType, s.definedInChunk as definedInChunk
                ORDER BY s.definedInChunk, s.legacyName",
                new { runId, sourceFile });

            var signatures = new List<SignatureNode>();
            await foreach (var record in cursor)
            {
                signatures.Add(new SignatureNode
                {
                    Id = record["id"].As<string>(),
                    SourceFile = record["sourceFile"].As<string>(),
                    LegacyName = record["legacyName"].As<string>(),
                    TargetMethodName = record["targetMethodName"].As<string>(),
                    TargetSignature = record["targetSignature"].As<string>(),
                    ReturnType = record["returnType"].As<string>(),
                    DefinedInChunk = record["definedInChunk"].As<int>()
                });
            }
            return signatures;
        });

        return result;
    }

    /// <summary>
    /// Gets all signatures across all files for a run (for cross-file consistency).
    /// </summary>
    public async Task<List<SignatureNode>> GetAllSignaturesAsync(int runId)
    {
        await using var session = _driver.AsyncSession();

        var result = await session.ExecuteReadAsync(async tx =>
        {
            var cursor = await tx.RunAsync(@"
                MATCH (s:Signature)
                WHERE s.runId = $runId
                RETURN s.id as id, s.sourceFile as sourceFile, s.legacyName as legacyName,
                       s.targetMethodName as targetMethodName, s.targetSignature as targetSignature,
                       s.returnType as returnType, s.definedInChunk as definedInChunk
                ORDER BY s.sourceFile, s.definedInChunk, s.legacyName",
                new { runId });

            var signatures = new List<SignatureNode>();
            await foreach (var record in cursor)
            {
                signatures.Add(new SignatureNode
                {
                    Id = record["id"].As<string>(),
                    SourceFile = record["sourceFile"].As<string>(),
                    LegacyName = record["legacyName"].As<string>(),
                    TargetMethodName = record["targetMethodName"].As<string>(),
                    TargetSignature = record["targetSignature"].As<string>(),
                    ReturnType = record["returnType"].As<string>(),
                    DefinedInChunk = record["definedInChunk"].As<int>()
                });
            }
            return signatures;
        });

        return result;
    }

    /// <summary>
    /// Gets cross-chunk dependencies for a run.
    /// </summary>
    public async Task<List<ChunkDependency>> GetChunkDependenciesAsync(int runId)
    {
        await using var session = _driver.AsyncSession();

        var result = await session.ExecuteReadAsync(async tx =>
        {
            var cursor = await tx.RunAsync(@"
                MATCH (source:Chunk)-[d:CALLS_IN_CHUNK]->(target:Chunk)
                WHERE d.runId = $runId
                RETURN source.id as sourceChunkId, target.id as targetChunkId,
                       d.dependencyType as dependencyType, d.callerMethod as callerMethod,
                       d.targetMethod as targetMethod",
                new { runId });

            var dependencies = new List<ChunkDependency>();
            await foreach (var record in cursor)
            {
                dependencies.Add(new ChunkDependency
                {
                    SourceChunkId = record["sourceChunkId"].As<string>(),
                    TargetChunkId = record["targetChunkId"].As<string>(),
                    DependencyType = record["dependencyType"].As<string>(),
                    CallerMethod = record["callerMethod"].As<string>(),
                    TargetMethod = record["targetMethod"].As<string>()
                });
            }
            return dependencies;
        });

        return result;
    }

    /// <summary>
    /// Updates chunk status in Neo4j.
    /// </summary>
    public async Task UpdateChunkStatusAsync(int runId, string sourceFile, int chunkIndex, string status, int? tokensUsed = null, long? processingTimeMs = null)
    {
        await using var session = _driver.AsyncSession();

        try
        {
            await session.ExecuteWriteAsync(async tx =>
            {
                var chunkId = $"{runId}:{sourceFile}:{chunkIndex}";
                var completedAt = status == "Completed" ? DateTime.UtcNow.ToString("o") : null;

                var query = @"
                    MATCH (c:Chunk {id: $chunkId})
                    SET c.status = $status";

                if (completedAt != null)
                    query += ", c.completedAt = $completedAt";
                if (tokensUsed.HasValue)
                    query += ", c.tokensUsed = $tokensUsed";
                if (processingTimeMs.HasValue)
                    query += ", c.processingTimeMs = $processingTimeMs";

                await tx.RunAsync(query, new
                {
                    chunkId,
                    status,
                    completedAt,
                    tokensUsed = tokensUsed ?? 0,
                    processingTimeMs = processingTimeMs ?? 0
                });
            });

            _logger.LogDebug("Updated chunk status for {SourceFile} chunk {ChunkIndex} to {Status}", sourceFile, chunkIndex, status);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error updating chunk status in Neo4j");
            throw;
        }
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

// ============================================================
// CHUNKING SUPPORT MODELS AND METHODS
// ============================================================

/// <summary>
/// Represents a chunk in the Neo4j graph for cross-file dependency tracking.
/// </summary>
public class ChunkNode
{
    public string Id { get; set; } = string.Empty;
    public string SourceFile { get; set; } = string.Empty;
    public int ChunkIndex { get; set; }
    public int StartLine { get; set; }
    public int EndLine { get; set; }
    public string Status { get; set; } = string.Empty;
    public int TokensUsed { get; set; }
    public long ProcessingTimeMs { get; set; }
    public List<string> SemanticUnits { get; set; } = new();
    public DateTime? CompletedAt { get; set; }
}

/// <summary>
/// Represents a method signature stored in Neo4j for cross-file consistency.
/// </summary>
public class SignatureNode
{
    public string Id { get; set; } = string.Empty;
    public string SourceFile { get; set; } = string.Empty;
    public string LegacyName { get; set; } = string.Empty;
    public string TargetMethodName { get; set; } = string.Empty;
    public string TargetSignature { get; set; } = string.Empty;
    public string ReturnType { get; set; } = string.Empty;
    public int DefinedInChunk { get; set; }
}

/// <summary>
/// Represents a cross-chunk dependency relationship.
/// </summary>
public class ChunkDependency
{
    public string SourceChunkId { get; set; } = string.Empty;
    public string TargetChunkId { get; set; } = string.Empty;
    public string DependencyType { get; set; } = string.Empty;
    public string CallerMethod { get; set; } = string.Empty;
    public string TargetMethod { get; set; } = string.Empty;
}

/// <summary>
/// Chunk processing status summary.
/// </summary>
public class ChunkProcessingStatus
{
    public string SourceFile { get; set; } = string.Empty;
    public int TotalChunks { get; set; }
    public int CompletedChunks { get; set; }
    public int FailedChunks { get; set; }
    public int PendingChunks { get; set; }
    public double ProgressPercentage { get; set; }
    public long TotalProcessingTimeMs { get; set; }
    public int TotalTokensUsed { get; set; }
}
