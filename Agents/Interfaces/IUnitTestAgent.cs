using Microsoft.SemanticKernel;
using CobolModernization.Models;

namespace CobolModernization.Agents.Interfaces;

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

    /// <summary>
    /// Generates unit tests for a C# file.
    /// </summary>
    /// <param name="csharpFile">The C# file to generate tests for.</param>
    /// <param name="cobolAnalysis">The analysis of the original COBOL file.</param>
    /// <returns>The generated test file.</returns>
    Task<CSharpFile> GenerateUnitTestsAsync(CSharpFile csharpFile, CobolAnalysis cobolAnalysis);

    /// <summary>
    /// Generates unit tests for a collection of C# files.
    /// </summary>
    /// <param name="csharpFiles">The C# files to generate tests for.</param>
    /// <param name="cobolAnalyses">The analyses of the original COBOL files.</param>
    /// <param name="progressCallback">Optional callback for progress reporting.</param>
    /// <returns>The generated test files.</returns>
    Task<List<CSharpFile>> GenerateUnitTestsAsync(List<CSharpFile> csharpFiles, List<CobolAnalysis> cobolAnalyses, Action<int, int>? progressCallback = null);
}
