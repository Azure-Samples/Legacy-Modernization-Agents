namespace CobolToQuarkusMigration.Chunking.Models;

/// <summary>
/// Report generated after validation of a conversion.
/// </summary>
public class ValidationReport
{
    /// <summary>
    /// The migration run ID.
    /// </summary>
    public int RunId { get; set; }

    /// <summary>
    /// The source file that was validated.
    /// </summary>
    public string SourceFile { get; set; } = string.Empty;

    /// <summary>
    /// Whether the overall validation passed.
    /// </summary>
    public bool IsValid { get; set; }

    /// <summary>
    /// Total number of chunks in the file.
    /// </summary>
    public int TotalChunks { get; set; }

    /// <summary>
    /// Number of chunks that passed validation.
    /// </summary>
    public int ValidChunks { get; set; }

    /// <summary>
    /// Signature consistency issues found.
    /// </summary>
    public List<ConsistencyIssue> SignatureIssues { get; set; } = new();

    /// <summary>
    /// Type mapping consistency issues found.
    /// </summary>
    public List<ConsistencyIssue> TypeMappingIssues { get; set; } = new();

    /// <summary>
    /// Unresolved forward references.
    /// </summary>
    public List<UnresolvedReference> UnresolvedReferences { get; set; } = new();

    /// <summary>
    /// Issues that were automatically fixed.
    /// </summary>
    public List<AutoFixResult> AutoFixes { get; set; } = new();

    /// <summary>
    /// Warnings (non-blocking issues).
    /// </summary>
    public List<string> Warnings { get; set; } = new();

    /// <summary>
    /// When the validation was performed.
    /// </summary>
    public DateTime ValidatedAt { get; set; } = DateTime.UtcNow;

    /// <summary>
    /// Summary statistics.
    /// </summary>
    public ValidationStatistics Statistics { get; set; } = new();
}

/// <summary>
/// Represents a consistency issue found during validation.
/// </summary>
public class ConsistencyIssue
{
    /// <summary>
    /// The type of issue.
    /// </summary>
    public ConsistencyIssueType IssueType { get; set; }

    /// <summary>
    /// The name of the entity with the issue.
    /// </summary>
    public string EntityName { get; set; } = string.Empty;

    /// <summary>
    /// Chunk index where the issue was found.
    /// </summary>
    public int ChunkIndex { get; set; }

    /// <summary>
    /// Line number in the converted code (if applicable).
    /// </summary>
    public int? LineNumber { get; set; }

    /// <summary>
    /// Expected value.
    /// </summary>
    public string Expected { get; set; } = string.Empty;

    /// <summary>
    /// Actual value found.
    /// </summary>
    public string Actual { get; set; } = string.Empty;

    /// <summary>
    /// Description of the issue.
    /// </summary>
    public string Description { get; set; } = string.Empty;

    /// <summary>
    /// Severity of the issue.
    /// </summary>
    public IssueSeverity Severity { get; set; }

    /// <summary>
    /// Whether this issue can be auto-fixed.
    /// </summary>
    public bool CanAutoFix { get; set; }
}

/// <summary>
/// Types of consistency issues.
/// </summary>
public enum ConsistencyIssueType
{
    /// <summary>Method name differs across references.</summary>
    MethodNameMismatch,

    /// <summary>Method signature differs from registration.</summary>
    SignatureMismatch,

    /// <summary>Return type differs from registration.</summary>
    ReturnTypeMismatch,

    /// <summary>Parameter count differs.</summary>
    ParameterCountMismatch,

    /// <summary>Parameter type differs.</summary>
    ParameterTypeMismatch,

    /// <summary>Variable type differs from mapping.</summary>
    VariableTypeMismatch,

    /// <summary>Variable name differs from mapping.</summary>
    VariableNameMismatch,

    /// <summary>Duplicate definition found.</summary>
    DuplicateDefinition,

    /// <summary>Missing definition.</summary>
    MissingDefinition,

    /// <summary>Circular dependency detected.</summary>
    CircularDependency,

    /// <summary>Forward reference not resolved.</summary>
    UnresolvedForwardReference,

    /// <summary>Other consistency issue.</summary>
    Other
}

/// <summary>
/// Severity levels for consistency issues.
/// </summary>
public enum IssueSeverity
{
    /// <summary>Informational only.</summary>
    Info,

    /// <summary>Minor issue that doesn't affect compilation.</summary>
    Warning,

    /// <summary>Significant issue that may cause problems.</summary>
    Error,

    /// <summary>Critical issue that will prevent compilation.</summary>
    Critical
}

/// <summary>
/// Represents an unresolved forward reference.
/// </summary>
public class UnresolvedReference
{
    /// <summary>
    /// The method making the reference.
    /// </summary>
    public string CallerMethod { get; set; } = string.Empty;

    /// <summary>
    /// The method being referenced.
    /// </summary>
    public string TargetMethod { get; set; } = string.Empty;

    /// <summary>
    /// Chunk where the reference was made.
    /// </summary>
    public int CallerChunkIndex { get; set; }

    /// <summary>
    /// The predicted signature that was used.
    /// </summary>
    public string PredictedSignature { get; set; } = string.Empty;

    /// <summary>
    /// The actual signature (if target was found but different).
    /// </summary>
    public string? ActualSignature { get; set; }

    /// <summary>
    /// Why the reference couldn't be resolved.
    /// </summary>
    public string Reason { get; set; } = string.Empty;
}

/// <summary>
/// Result of an automatic fix attempt.
/// </summary>
public class AutoFixResult
{
    /// <summary>
    /// The issue that was fixed.
    /// </summary>
    public ConsistencyIssue OriginalIssue { get; set; } = new();

    /// <summary>
    /// Whether the fix was successful.
    /// </summary>
    public bool Success { get; set; }

    /// <summary>
    /// Description of what was fixed.
    /// </summary>
    public string FixDescription { get; set; } = string.Empty;

    /// <summary>
    /// The original value before fix.
    /// </summary>
    public string OriginalValue { get; set; } = string.Empty;

    /// <summary>
    /// The new value after fix.
    /// </summary>
    public string NewValue { get; set; } = string.Empty;

    /// <summary>
    /// Chunk index where fix was applied.
    /// </summary>
    public int ChunkIndex { get; set; }

    /// <summary>
    /// Line numbers affected by the fix.
    /// </summary>
    public List<int> AffectedLines { get; set; } = new();
}

/// <summary>
/// Statistics from the validation process.
/// </summary>
public class ValidationStatistics
{
    /// <summary>Total methods validated.</summary>
    public int TotalMethods { get; set; }

    /// <summary>Methods with consistent signatures.</summary>
    public int ConsistentMethods { get; set; }

    /// <summary>Total variables validated.</summary>
    public int TotalVariables { get; set; }

    /// <summary>Variables with consistent types.</summary>
    public int ConsistentVariables { get; set; }

    /// <summary>Total forward references.</summary>
    public int TotalForwardReferences { get; set; }

    /// <summary>Forward references resolved.</summary>
    public int ResolvedReferences { get; set; }

    /// <summary>Issues auto-fixed.</summary>
    public int AutoFixedIssues { get; set; }

    /// <summary>Issues requiring manual intervention.</summary>
    public int ManualFixRequired { get; set; }

    /// <summary>Time spent on validation (ms).</summary>
    public long ValidationTimeMs { get; set; }

    /// <summary>
    /// Consistency percentage for methods.
    /// </summary>
    public double MethodConsistencyPercent =>
        TotalMethods == 0 ? 100.0 : (ConsistentMethods * 100.0 / TotalMethods);

    /// <summary>
    /// Consistency percentage for variables.
    /// </summary>
    public double VariableConsistencyPercent =>
        TotalVariables == 0 ? 100.0 : (ConsistentVariables * 100.0 / TotalVariables);

    /// <summary>
    /// Forward reference resolution percentage.
    /// </summary>
    public double ReferenceResolutionPercent =>
        TotalForwardReferences == 0 ? 100.0 : (ResolvedReferences * 100.0 / TotalForwardReferences);
}
