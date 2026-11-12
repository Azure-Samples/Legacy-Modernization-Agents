using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Agents.Interfaces;

/// <summary>
/// Interface for the COBOL dependency mapper agent.
/// </summary>
public interface IDependencyMapperAgent
{
    /// <summary>
    /// Verifies source-relative local REKT dependency evidence before generating a map or AI commentary.
    /// Missing, stale, incomplete or placeholder-backed evidence throws InvalidDataException.
    /// </summary>
    /// <param name="cobolFiles">The COBOL files to analyze.</param>
    /// <param name="analyses">The analysis results for the COBOL files.</param>
    /// <returns>A dependency map with relationships and Mermaid diagram.</returns>
    Task<DependencyMap> AnalyzeDependenciesAsync(List<CobolFile> cobolFiles, List<CobolAnalysis> analyses);

    /// <summary>
    /// Renders the supplied dependency relationships deterministically, without a model call.
    /// </summary>
    /// <param name="dependencyMap">The dependency map to visualize.</param>
    /// <param name="enableAiGeneration">Retained for caller compatibility; neither value enables AI generation.</param>
    /// <returns>A Mermaid diagram as a string.</returns>
    Task<string> GenerateMermaidDiagramAsync(DependencyMap dependencyMap, bool enableAiGeneration = true);

    /// <summary>
    /// Reads copybook usage from verified REKT exports; never infers authoritative edges from text or AI.
    /// </summary>
    /// <param name="cobolFiles">The COBOL files to analyze.</param>
    /// <returns>A copybook usage matrix.</returns>
    Task<Dictionary<string, List<string>>> AnalyzeCopybookUsageAsync(List<CobolFile> cobolFiles);
}
