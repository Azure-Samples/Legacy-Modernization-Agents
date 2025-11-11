using System.Threading.Tasks;
using CobolModernization.Models;

namespace CobolModernization.Agents.Interfaces
{
    /// <summary>
    /// Interface for validation agent that compares converted code with original COBOL
    /// </summary>
    public interface IValidationAgent
    {
        /// <summary>
        /// Validates converted code against original COBOL source and generates a detailed report
        /// </summary>
        /// <param name="cobolSourcePath">Path to directory containing COBOL source files</param>
        /// <param name="convertedCodePath">Path to directory containing converted code</param>
        /// <param name="targetLanguage">Target language (CSharp or Java)</param>
        /// <param name="outputPath">Path where validation report should be saved</param>
        /// <returns>ValidationReport containing analysis results and accuracy score</returns>
        Task<ValidationReport> ValidateConversionAsync(
            string cobolSourcePath,
            string convertedCodePath,
            string targetLanguage,
            string outputPath);
    }
}
