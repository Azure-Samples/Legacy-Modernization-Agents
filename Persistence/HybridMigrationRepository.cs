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
        if (_neo4jRepo == null)
        {
            _logger.LogWarning("Neo4j repository not available for graph visualization");
            return null;
        }

        try
        {
            return await _neo4jRepo.GetDependencyGraphDataAsync(runId);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Failed to get dependency graph data from Neo4j");
            return null;
        }
    }
}
