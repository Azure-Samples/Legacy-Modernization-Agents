using CobolToQuarkusMigration.Models;
using Microsoft.Extensions.Logging;

namespace CobolToQuarkusMigration.Persistence;

/// <summary>
/// Hybrid repository that combines SQLite (for metadata and persistence) 
/// with Neo4j (for graph queries and analysis).
/// </summary>
public class HybridMigrationRepository : IMigrationRepository
{
    private readonly SqliteMigrationRepository _sqliteRepo;
    private readonly Neo4jMigrationRepository? _neo4jRepo;
    private readonly ILogger<HybridMigrationRepository> _logger;

    public HybridMigrationRepository(
        SqliteMigrationRepository sqliteRepo,
        Neo4jMigrationRepository? neo4jRepo,
        ILogger<HybridMigrationRepository> logger)
    {
        _sqliteRepo = sqliteRepo;
        _neo4jRepo = neo4jRepo;
        _logger = logger;
    }

    public Task InitializeAsync(CancellationToken cancellationToken = default)
    {
        return _sqliteRepo.InitializeAsync(cancellationToken);
    }

    public Task<int> StartRunAsync(string cobolSourcePath, string javaOutputPath, CancellationToken cancellationToken = default)
    {
        return _sqliteRepo.StartRunAsync(cobolSourcePath, javaOutputPath, cancellationToken);
    }

    public Task CompleteRunAsync(int runId, string status, string? notes = null, CancellationToken cancellationToken = default)
    {
        return _sqliteRepo.CompleteRunAsync(runId, status, notes, cancellationToken);
    }

    public Task SaveCobolFilesAsync(int runId, IEnumerable<CobolFile> cobolFiles, CancellationToken cancellationToken = default)
    {
        return _sqliteRepo.SaveCobolFilesAsync(runId, cobolFiles, cancellationToken);
    }

    public Task SaveAnalysesAsync(int runId, IEnumerable<CobolAnalysis> analyses, CancellationToken cancellationToken = default)
    {
        return _sqliteRepo.SaveAnalysesAsync(runId, analyses, cancellationToken);
    }

    public async Task SaveDependencyMapAsync(int runId, DependencyMap dependencyMap, CancellationToken cancellationToken = default)
    {
        // Save to SQLite for traditional queries
        await _sqliteRepo.SaveDependencyMapAsync(runId, dependencyMap, cancellationToken);

        // Also save to Neo4j for graph queries (if available)
        if (_neo4jRepo != null)
        {
            try
            {
                await _neo4jRepo.SaveDependencyGraphAsync(runId, dependencyMap);
                _logger.LogInformation($"Saved dependency graph to Neo4j for run {runId}");
            }
            catch (Exception ex)
            {
                _logger.LogWarning(ex, $"Failed to save to Neo4j for run {runId}, but SQLite save succeeded");
            }
        }
    }

    public Task<MigrationRunSummary?> GetLatestRunAsync(CancellationToken cancellationToken = default)
    {
        return _sqliteRepo.GetLatestRunAsync(cancellationToken);
    }

    public Task<MigrationRunSummary?> GetRunAsync(int runId, CancellationToken cancellationToken = default)
    {
        return _sqliteRepo.GetRunAsync(runId, cancellationToken);
    }

    public Task<IReadOnlyList<CobolAnalysis>> GetAnalysesAsync(int runId, CancellationToken cancellationToken = default)
    {
        return _sqliteRepo.GetAnalysesAsync(runId, cancellationToken);
    }

    public Task<DependencyMap?> GetDependencyMapAsync(int runId, CancellationToken cancellationToken = default)
    {
        return _sqliteRepo.GetDependencyMapAsync(runId, cancellationToken);
    }

    public Task<IReadOnlyList<DependencyRelationship>> GetDependenciesAsync(int runId, CancellationToken cancellationToken = default)
    {
        return _sqliteRepo.GetDependenciesAsync(runId, cancellationToken);
    }

    public Task<IReadOnlyList<CobolFile>> SearchCobolFilesAsync(int runId, string? searchTerm, CancellationToken cancellationToken = default)
    {
        return _sqliteRepo.SearchCobolFilesAsync(runId, searchTerm, cancellationToken);
    }

    // Graph-specific methods (delegate to Neo4j if available)

    public async Task<IReadOnlyList<CircularDependency>> GetCircularDependenciesAsync(int runId)
    {
        if (_neo4jRepo == null)
        {
            _logger.LogWarning("Neo4j repository not available, returning empty circular dependencies");
            return Array.Empty<CircularDependency>();
        }

        try
        {
            return await _neo4jRepo.GetCircularDependenciesAsync(runId);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Failed to get circular dependencies from Neo4j");
            return Array.Empty<CircularDependency>();
        }
    }

    public async Task<ImpactAnalysis?> GetImpactAnalysisAsync(int runId, string fileName)
    {
        if (_neo4jRepo == null)
        {
            _logger.LogWarning("Neo4j repository not available for impact analysis");
            return null;
        }

        try
        {
            return await _neo4jRepo.GetImpactAnalysisAsync(fileName, runId);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, $"Failed to get impact analysis for {fileName}");
            return null;
        }
    }

    public async Task<IReadOnlyList<CriticalFile>> GetCriticalFilesAsync(int runId)
    {
        if (_neo4jRepo == null)
        {
            _logger.LogWarning("Neo4j repository not available, returning empty critical files");
            return Array.Empty<CriticalFile>();
        }

        try
        {
            return await _neo4jRepo.GetCriticalFilesAsync(runId);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Failed to get critical files from Neo4j");
            return Array.Empty<CriticalFile>();
        }
    }

    public async Task<GraphVisualizationData?> GetDependencyGraphDataAsync(int runId)
    {
        GraphVisualizationData? graphData = null;

        // Try Neo4j first if available
        if (_neo4jRepo != null)
        {
            try
            {
                var neo4jData = await _neo4jRepo.GetDependencyGraphDataAsync(runId);
                if (neo4jData != null && (neo4jData.Nodes.Count > 0 || neo4jData.Edges.Count > 0))
                {
                    _logger.LogInformation("Retrieved graph data from Neo4j for run {RunId}: {NodeCount} nodes, {EdgeCount} edges",
                        runId, neo4jData.Nodes.Count, neo4jData.Edges.Count);
                    graphData = neo4jData;
                }
                else
                {
                    _logger.LogInformation("Neo4j returned no data for run {RunId}, falling back to SQLite", runId);
                    graphData = await _sqliteRepo.GetDependencyGraphDataAsync(runId);
                }
            }
            catch (Exception ex)
            {
                _logger.LogWarning(ex, "Failed to get dependency graph data from Neo4j for run {RunId}, falling back to SQLite", runId);
                graphData = await _sqliteRepo.GetDependencyGraphDataAsync(runId);
            }
        }
        else
        {
            // Fallback to SQLite
            _logger.LogInformation("Building graph data from SQLite for run {RunId}", runId);
            graphData = await _sqliteRepo.GetDependencyGraphDataAsync(runId);
        }

        // Enrich nodes with line counts from SQLite if not already populated
        if (graphData != null && graphData.Nodes.Any(n => n.LineCount == 0))
        {
            await EnrichNodesWithLineCountsAsync(runId, graphData.Nodes);
        }

        return graphData;
    }

    private async Task EnrichNodesWithLineCountsAsync(int runId, List<GraphNode> nodes)
    {
        try
        {
            await using var connection = _sqliteRepo.CreateConnection();
            await connection.OpenAsync();

            foreach (var node in nodes)
            {
                if (node.LineCount == 0)
                {
                    await using var command = connection.CreateCommand();
                    command.CommandText = @"
                        SELECT LENGTH(content) - LENGTH(REPLACE(content, char(10), '')) + 1 as line_count
                        FROM cobol_files
                        WHERE run_id = $runId AND file_name = $fileName
                        LIMIT 1";
                    command.Parameters.AddWithValue("$runId", runId);
                    command.Parameters.AddWithValue("$fileName", node.Id);

                    var result = await command.ExecuteScalarAsync();
                    if (result != null && result != DBNull.Value)
                    {
                        node.LineCount = Convert.ToInt32(result);
                    }
                }
            }

            _logger.LogInformation("Enriched {Count} nodes with line counts from SQLite for run {RunId}", nodes.Count, runId);
        }
        catch (Exception ex)
        {
            _logger.LogWarning(ex, "Failed to enrich nodes with line counts for run {RunId}", runId);
        }
    }

    public async Task<List<int>> GetAvailableRunsAsync()
    {
        if (_neo4jRepo == null)
        {
            _logger.LogWarning("Neo4j repository not available");
            return new List<int>();
        }

        try
        {
            return await _neo4jRepo.GetAvailableRunsAsync();
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Failed to get available runs from Neo4j");
            return new List<int>();
        }
    }
}

