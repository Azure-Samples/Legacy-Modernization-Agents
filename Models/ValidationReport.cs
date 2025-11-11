using System;
using System.Collections.Generic;

namespace CobolModernization.Models
{
    /// <summary>
    /// Represents a validation report comparing converted code with original COBOL
    /// </summary>
    public class ValidationReport
    {
        /// <summary>
        /// Target language of the conversion (CSharp or Java)
        /// </summary>
        public string TargetLanguage { get; set; } = string.Empty;

        /// <summary>
        /// Overall accuracy score (0-100)
        /// </summary>
        public double AccuracyScore { get; set; }

        /// <summary>
        /// Overall validation status
        /// </summary>
        public ValidationStatus Status { get; set; }

        /// <summary>
        /// Detailed comparison analysis
        /// </summary>
        public string DetailedAnalysis { get; set; } = string.Empty;

        /// <summary>
        /// List of functional differences found
        /// </summary>
        public List<FunctionalDifference> Differences { get; set; } = new();

        /// <summary>
        /// Recommendations for fixing differences
        /// </summary>
        public List<string> Recommendations { get; set; } = new();

        /// <summary>
        /// Features that were correctly converted
        /// </summary>
        public List<string> CorrectConversions { get; set; } = new();

        /// <summary>
        /// Number of COBOL files analyzed
        /// </summary>
        public int CobolFilesAnalyzed { get; set; }

        /// <summary>
        /// Number of converted files analyzed
        /// </summary>
        public int ConvertedFilesAnalyzed { get; set; }

        /// <summary>
        /// Timestamp when validation was performed
        /// </summary>
        public DateTime ValidationTimestamp { get; set; } = DateTime.UtcNow;

        /// <summary>
        /// Path to the generated markdown report
        /// </summary>
        public string ReportPath { get; set; } = string.Empty;
    }

    /// <summary>
    /// Represents a functional difference between COBOL and converted code
    /// </summary>
    public class FunctionalDifference
    {
        /// <summary>
        /// Severity level of the difference
        /// </summary>
        public DifferenceSeverity Severity { get; set; }

        /// <summary>
        /// Category of the difference (e.g., "Data Handling", "Business Logic", "File I/O")
        /// </summary>
        public string Category { get; set; } = string.Empty;

        /// <summary>
        /// COBOL file where the original functionality exists
        /// </summary>
        public string CobolFile { get; set; } = string.Empty;

        /// <summary>
        /// Converted file where the difference appears
        /// </summary>
        public string ConvertedFile { get; set; } = string.Empty;

        /// <summary>
        /// Description of the difference
        /// </summary>
        public string Description { get; set; } = string.Empty;

        /// <summary>
        /// Expected behavior from COBOL
        /// </summary>
        public string ExpectedBehavior { get; set; } = string.Empty;

        /// <summary>
        /// Actual behavior in converted code
        /// </summary>
        public string ActualBehavior { get; set; } = string.Empty;

        /// <summary>
        /// Impact of this difference on functionality
        /// </summary>
        public string Impact { get; set; } = string.Empty;

        /// <summary>
        /// Suggested fix for the difference
        /// </summary>
        public string SuggestedFix { get; set; } = string.Empty;
    }

    /// <summary>
    /// Validation status enum
    /// </summary>
    public enum ValidationStatus
    {
        /// <summary>
        /// Conversion is functionally equivalent to original COBOL
        /// </summary>
        FullyEquivalent,

        /// <summary>
        /// Conversion is mostly correct with minor non-critical differences
        /// </summary>
        MostlyEquivalent,

        /// <summary>
        /// Conversion has some significant functional differences
        /// </summary>
        PartiallyEquivalent,

        /// <summary>
        /// Conversion has major functional differences or missing features
        /// </summary>
        NotEquivalent,

        /// <summary>
        /// Validation could not be completed or failed
        /// </summary>
        ValidationFailed
    }

    /// <summary>
    /// Severity levels for functional differences
    /// </summary>
    public enum DifferenceSeverity
    {
        /// <summary>
        /// Critical - will cause incorrect behavior or failure
        /// </summary>
        Critical,

        /// <summary>
        /// Major - significant functional difference
        /// </summary>
        Major,

        /// <summary>
        /// Moderate - noticeable difference but may work
        /// </summary>
        Moderate,

        /// <summary>
        /// Minor - small difference with minimal impact
        /// </summary>
        Minor,

        /// <summary>
        /// Informational - for awareness only
        /// </summary>
        Info
    }
}
