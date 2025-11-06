using Microsoft.SemanticKernel;
using CobolModernization.Models;

namespace CobolModernization.Agents.Interfaces;

/// <summary>
/// Interface for the C# converter agent.
/// </summary>
public interface ICSharpConverterAgent
{
    /// <summary>
    /// Converts a COBOL file to C#.
    /// </summary>
    /// <param name="cobolFile">The COBOL file to convert.</param>
    /// <param name="cobolAnalysis">The analysis of the COBOL file.</param>
    /// <returns>The generated C# file.</returns>
    Task<CSharpFile> ConvertToCSharpAsync(CobolFile cobolFile, CobolAnalysis cobolAnalysis);

    /// <summary>
    /// Converts a collection of COBOL files to C#.
    /// </summary>
    /// <param name="cobolFiles">The COBOL files to convert.</param>
    /// <param name="cobolAnalyses">The analyses of the COBOL files.</param>
    /// <param name="progressCallback">Optional callback for progress reporting.</param>
    /// <returns>The generated C# files.</returns>
    Task<List<CSharpFile>> ConvertToCSharpAsync(List<CobolFile> cobolFiles, List<CobolAnalysis> cobolAnalyses, Action<int, int>? progressCallback = null);
}
