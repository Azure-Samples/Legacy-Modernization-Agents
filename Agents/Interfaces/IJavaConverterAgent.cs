using Microsoft.SemanticKernel;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Agents.Interfaces;

/// <summary>
/// Interface for the Java converter agent.
/// </summary>
public interface IJavaConverterAgent
{
    /// <summary>
    /// Converts a COBOL file to Java Quarkus.
    /// </summary>
    /// <param name="cobolFile">The COBOL file to convert.</param>
    /// <param name="cobolAnalysis">The analysis of the COBOL file.</param>
    /// <returns>The generated Java file.</returns>
    Task<JavaFile> ConvertToJavaAsync(CobolFile cobolFile, CobolAnalysis cobolAnalysis);
    
    /// <summary>
    /// Converts a collection of COBOL files to Java Quarkus.
    /// </summary>
    /// <param name="cobolFiles">The COBOL files to convert.</param>
    /// <param name="cobolAnalyses">The analyses of the COBOL files.</param>
    /// <param name="progressCallback">Optional callback for progress reporting.</param>
    /// <returns>The generated Java files.</returns>
    Task<List<JavaFile>> ConvertToJavaAsync(List<CobolFile> cobolFiles, List<CobolAnalysis> cobolAnalyses, Action<int, int>? progressCallback = null);
}
