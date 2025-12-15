using Microsoft.SemanticKernel;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Agents.Interfaces;

/// <summary>
/// Interface for the COBOL analyzer agent.
/// </summary>
public interface ICobolAnalyzerAgent
{
    /// <summary>
    /// Analyzes a COBOL file.
    /// </summary>
    /// <param name="cobolFile">The COBOL file to analyze.</param>
    /// <returns>The analysis of the COBOL file.</returns>
    Task<CobolAnalysis> AnalyzeCobolFileAsync(CobolFile cobolFile);
    
    /// <summary>
    /// Analyzes a collection of COBOL files.
    /// </summary>
    /// <param name="cobolFiles">The COBOL files to analyze.</param>
    /// <param name="progressCallback">Optional callback for progress reporting.</param>
    /// <returns>The analyses of the COBOL files.</returns>
    Task<List<CobolAnalysis>> AnalyzeCobolFilesAsync(List<CobolFile> cobolFiles, Action<int, int>? progressCallback = null);
}
