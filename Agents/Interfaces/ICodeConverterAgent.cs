using Microsoft.SemanticKernel;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Agents.Interfaces;

/// <summary>
/// Generic interface for code converter agents (Java, C#, etc.).
/// </summary>
public interface ICodeConverterAgent
{
    /// <summary>
    /// Converts a COBOL file to the target language.
    /// </summary>
    /// <param name="cobolFile">The COBOL file to convert.</param>
    /// <param name="cobolAnalysis">The analysis of the COBOL file.</param>
    /// <returns>The generated code file.</returns>
    Task<CodeFile> ConvertAsync(CobolFile cobolFile, CobolAnalysis cobolAnalysis);

    /// <summary>
    /// Converts a collection of COBOL files to the target language.
    /// </summary>
    /// <param name="cobolFiles">The COBOL files to convert.</param>
    /// <param name="cobolAnalyses">The analyses of the COBOL files.</param>
    /// <param name="progressCallback">Optional callback for progress reporting.</param>
    /// <returns>The generated code files.</returns>
    Task<List<CodeFile>> ConvertAsync(List<CobolFile> cobolFiles, List<CobolAnalysis> cobolAnalyses, Action<int, int>? progressCallback = null);

    /// <summary>
    /// Gets the target language for this converter.
    /// </summary>
    string TargetLanguage { get; }

    /// <summary>
    /// Gets the file extension for the target language.
    /// </summary>
    string FileExtension { get; }
}
