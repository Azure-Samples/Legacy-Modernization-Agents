using Microsoft.Extensions.Logging;
using Microsoft.SemanticKernel;
using Microsoft.SemanticKernel.Connectors.OpenAI;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Agents.Interfaces;

/// <summary>
/// Interface for the COBOL dependency mapper agent.
/// </summary>
public interface IDependencyMapperAgent
{
    /// <summary>
    /// Analyzes dependencies between COBOL files and generates a dependency map.
    /// </summary>
    /// <param name="cobolFiles">The COBOL files to analyze.</param>
    /// <param name="analyses">The analysis results for the COBOL files.</param>
    /// <returns>A dependency map with relationships and Mermaid diagram.</returns>
    Task<DependencyMap> AnalyzeDependenciesAsync(List<CobolFile> cobolFiles, List<CobolAnalysis> analyses);

    /// <summary>
    /// Generates a Mermaid flowchart for COBOL dependencies.
    /// </summary>
    /// <param name="dependencyMap">The dependency map to visualize.</param>
    /// <returns>A Mermaid diagram as a string.</returns>
    Task<string> GenerateMermaidDiagramAsync(DependencyMap dependencyMap);

    /// <summary>
    /// Analyzes copybook usage patterns across COBOL programs.
    /// </summary>
    /// <param name="cobolFiles">The COBOL files to analyze.</param>
    /// <returns>A copybook usage matrix.</returns>
    Task<Dictionary<string, List<string>>> AnalyzeCopybookUsageAsync(List<CobolFile> cobolFiles);
}
