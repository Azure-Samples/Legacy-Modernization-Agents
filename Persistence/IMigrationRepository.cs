using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Persistence;

/// <summary>
/// Contract for storing and retrieving migration insights.
/// </summary>
public interface IMigrationRepository
{
    /// <summary>
    /// Ensures the underlying database exists and is ready for use.
    /// </summary>
    Task InitializeAsync(CancellationToken cancellationToken = default);

    /// <summary>
    /// Starts a new migration run.
    /// </summary>
    Task<int> StartRunAsync(string cobolSourcePath, string javaOutputPath, CancellationToken cancellationToken = default);

    /// <summary>
    /// Completes a migration run with the final status.
    /// </summary>
    Task CompleteRunAsync(int runId, string status, string? notes = null, CancellationToken cancellationToken = default);

    /// <summary>
    /// Persists the discovered COBOL files for the run.
    /// </summary>
    Task SaveCobolFilesAsync(int runId, IEnumerable<CobolFile> cobolFiles, CancellationToken cancellationToken = default);

    /// <summary>
    /// Persists the COBOL analyses produced by the analyzer agent.
    /// </summary>
    Task SaveAnalysesAsync(int runId, IEnumerable<CobolAnalysis> analyses, CancellationToken cancellationToken = default);

    /// <summary>
    /// Persists dependency information and associated metrics.
    /// </summary>
    Task SaveDependencyMapAsync(int runId, DependencyMap dependencyMap, CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets the most recent migration run summary if available.
    /// </summary>
    Task<MigrationRunSummary?> GetLatestRunAsync(CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets a specific migration run summary.
    /// </summary>
    Task<MigrationRunSummary?> GetRunAsync(int runId, CancellationToken cancellationToken = default);

    /// <summary>
    /// Retrieves all analyses for a run.
    /// </summary>
    Task<IReadOnlyList<CobolAnalysis>> GetAnalysesAsync(int runId, CancellationToken cancellationToken = default);

    /// <summary>
    /// Retrieves the dependency map for a run.
    /// </summary>
    Task<DependencyMap?> GetDependencyMapAsync(int runId, CancellationToken cancellationToken = default);

    /// <summary>
    /// Retrieves dependencies for a run.
    /// </summary>
    Task<IReadOnlyList<DependencyRelationship>> GetDependenciesAsync(int runId, CancellationToken cancellationToken = default);

    /// <summary>
    /// Searches COBOL files for the provided term.
    /// </summary>
    Task<IReadOnlyList<CobolFile>> SearchCobolFilesAsync(int runId, string? searchTerm, CancellationToken cancellationToken = default);
}
