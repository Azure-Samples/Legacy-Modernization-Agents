namespace CobolToQuarkusMigration.Models;

/// <summary>
/// Represents the analysis of utility code vs business-specific code.
/// </summary>
public class UtilityCodeAnalysis
{
    /// <summary>
    /// Gets or sets the file name.
    /// </summary>
    public string FileName { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the file path.
    /// </summary>
    public string FilePath { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the identified utility code patterns.
    /// </summary>
    public List<UtilityCodePattern> UtilityPatterns { get; set; } = new List<UtilityCodePattern>();
    
    /// <summary>
    /// Gets or sets the modernization opportunities.
    /// </summary>
    public List<ModernizationOpportunity> ModernizationOpportunities { get; set; } = new List<ModernizationOpportunity>();
}

/// <summary>
/// Represents a utility code pattern found in COBOL.
/// </summary>
public class UtilityCodePattern
{
    /// <summary>
    /// Gets or sets the pattern type (DateTime, String, Math, FileIO, etc.).
    /// </summary>
    public UtilityCodeType Type { get; set; }
    
    /// <summary>
    /// Gets or sets the location in the source code.
    /// </summary>
    public string SourceLocation { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the description of what this code does.
    /// </summary>
    public string Description { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the COBOL code snippet.
    /// </summary>
    public string CobolCode { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets whether this is standard utility or business-specific.
    /// </summary>
    public bool IsBusinessSpecific { get; set; }
    
    /// <summary>
    /// Gets or sets the reason for classification.
    /// </summary>
    public string ClassificationReason { get; set; } = string.Empty;
}

/// <summary>
/// Represents a modernization opportunity for utility code.
/// </summary>
public class ModernizationOpportunity
{
    /// <summary>
    /// Gets or sets the current COBOL implementation.
    /// </summary>
    public string CurrentImplementation { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the location in source.
    /// </summary>
    public string SourceLocation { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the recommended modern approach.
    /// </summary>
    public string ModernEquivalent { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the confidence level (HIGH, MEDIUM, LOW).
    /// </summary>
    public ConfidenceLevel Confidence { get; set; }
    
    /// <summary>
    /// Gets or sets the migration effort (LOW, MEDIUM, HIGH).
    /// </summary>
    public MigrationEffort Effort { get; set; }
    
    /// <summary>
    /// Gets or sets the reasoning for this recommendation.
    /// </summary>
    public string Reasoning { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets example modern code.
    /// </summary>
    public string ExampleCode { get; set; } = string.Empty;
}

/// <summary>
/// Types of utility code patterns.
/// </summary>
public enum UtilityCodeType
{
    DateTime,
    StringManipulation,
    MathematicalOperation,
    FileIO,
    DataValidation,
    Formatting,
    Other
}

/// <summary>
/// Confidence level for modernization recommendations.
/// </summary>
public enum ConfidenceLevel
{
    Low,
    Medium,
    High
}

/// <summary>
/// Migration effort estimation.
/// </summary>
public enum MigrationEffort
{
    Low,
    Medium,
    High
}
