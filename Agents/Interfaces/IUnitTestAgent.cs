using Microsoft.SemanticKernel;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Agents.Interfaces;

/// <summary>
/// Interface for the unit test agent.
/// </summary>
public interface IUnitTestAgent
{
    /// <summary>
    /// Generates unit tests for a Java file.
    /// </summary>
    /// <param name="javaFile">The Java file to generate tests for.</param>
    /// <param name="cobolAnalysis">The analysis of the original COBOL file.</param>
    /// <returns>The generated test file.</returns>
    Task<JavaFile> GenerateUnitTestsAsync(JavaFile javaFile, CobolAnalysis cobolAnalysis);
    
    /// <summary>
    /// Generates unit tests for a collection of Java files.
    /// </summary>
    /// <param name="javaFiles">The Java files to generate tests for.</param>
    /// <param name="cobolAnalyses">The analyses of the original COBOL files.</param>
    /// <param name="progressCallback">Optional callback for progress reporting.</param>
    /// <returns>The generated test files.</returns>
    Task<List<JavaFile>> GenerateUnitTestsAsync(List<JavaFile> javaFiles, List<CobolAnalysis> cobolAnalyses, Action<int, int>? progressCallback = null);
}
