using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Chunking.Interfaces;

/// <summary>
/// Interface for the immutable signature registry.
/// Once a signature is registered, it cannot be changed - ensuring consistency
/// across all chunks and files in a migration run.
/// </summary>
public interface ISignatureRegistry
{
    /// <summary>
    /// Registers a new method signature. If the legacy name already exists,
    /// the existing signature is returned without modification.
    /// </summary>
    /// <param name="runId">The migration run ID.</param>
    /// <param name="sourceFile">The source file containing the method.</param>
    /// <param name="chunkIndex">The chunk index where this was defined.</param>
    /// <param name="signature">The signature to register.</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    /// <returns>The registered signature (may be existing if already registered).</returns>
    Task<MethodSignature> RegisterSignatureAsync(
        int runId,
        string sourceFile,
        int chunkIndex,
        MethodSignature signature,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets an existing signature by legacy name.
    /// </summary>
    /// <param name="runId">The migration run ID.</param>
    /// <param name="sourceFile">The source file (optional, for file-scoped lookups).</param>
    /// <param name="legacyName">The legacy method name.</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    /// <returns>The signature if found, null otherwise.</returns>
    Task<MethodSignature?> GetSignatureAsync(
        int runId,
        string? sourceFile,
        string legacyName,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets all signatures for a run (optionally filtered by source file).
    /// </summary>
    /// <param name="runId">The migration run ID.</param>
    /// <param name="sourceFile">Optional source file filter.</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    /// <returns>All matching signatures.</returns>
    Task<IReadOnlyList<MethodSignature>> GetAllSignaturesAsync(
        int runId,
        string? sourceFile = null,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Checks if a signature exists for the given legacy name.
    /// </summary>
    /// <param name="runId">The migration run ID.</param>
    /// <param name="legacyName">The legacy method name.</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    /// <returns>True if signature exists.</returns>
    Task<bool> SignatureExistsAsync(
        int runId,
        string legacyName,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Validates that a signature matches an existing registration.
    /// Used during reconciliation to detect inconsistencies.
    /// </summary>
    /// <param name="runId">The migration run ID.</param>
    /// <param name="signature">The signature to validate.</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    /// <returns>Validation result with any discrepancies.</returns>
    Task<SignatureValidationResult> ValidateSignatureAsync(
        int runId,
        MethodSignature signature,
        CancellationToken cancellationToken = default);
}

/// <summary>
/// Interface for the type mapping table.
/// Maps legacy variable declarations to target language types consistently.
/// </summary>
public interface ITypeMappingTable
{
    /// <summary>
    /// Registers a type mapping for a variable.
    /// If the variable already has a mapping, the existing mapping is returned.
    /// </summary>
    /// <param name="runId">The migration run ID.</param>
    /// <param name="sourceFile">The source file containing the variable.</param>
    /// <param name="mapping">The type mapping to register.</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    /// <returns>The registered mapping (may be existing if already registered).</returns>
    Task<TypeMapping> RegisterMappingAsync(
        int runId,
        string sourceFile,
        TypeMapping mapping,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets an existing type mapping by legacy variable name.
    /// </summary>
    /// <param name="runId">The migration run ID.</param>
    /// <param name="sourceFile">The source file (optional, for file-scoped lookups).</param>
    /// <param name="legacyVariable">The legacy variable name.</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    /// <returns>The mapping if found, null otherwise.</returns>
    Task<TypeMapping?> GetMappingAsync(
        int runId,
        string? sourceFile,
        string legacyVariable,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets all type mappings for a run (optionally filtered by source file).
    /// </summary>
    /// <param name="runId">The migration run ID.</param>
    /// <param name="sourceFile">Optional source file filter.</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    /// <returns>All matching type mappings.</returns>
    Task<IReadOnlyList<TypeMapping>> GetAllMappingsAsync(
        int runId,
        string? sourceFile = null,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Bulk registers type mappings (efficient for initial analysis phase).
    /// </summary>
    /// <param name="runId">The migration run ID.</param>
    /// <param name="sourceFile">The source file.</param>
    /// <param name="mappings">The mappings to register.</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    /// <returns>Number of new mappings registered (excludes duplicates).</returns>
    Task<int> BulkRegisterMappingsAsync(
        int runId,
        string sourceFile,
        IEnumerable<TypeMapping> mappings,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Infers the target type for a legacy type using deterministic rules.
    /// </summary>
    /// <param name="legacyType">The legacy type (e.g., PIC X(10)).</param>
    /// <param name="targetLanguage">The target language (Java, CSharp).</param>
    /// <returns>The inferred target type.</returns>
    string InferTargetType(string legacyType, TargetLanguage targetLanguage);
}

/// <summary>
/// Result of signature validation.
/// </summary>
public class SignatureValidationResult
{
    /// <summary>Whether the signature is valid.</summary>
    public bool IsValid { get; set; }

    /// <summary>The existing signature if found.</summary>
    public MethodSignature? ExistingSignature { get; set; }

    /// <summary>List of discrepancies found.</summary>
    public List<SignatureDiscrepancy> Discrepancies { get; set; } = new();
}

/// <summary>
/// Represents a discrepancy between two signatures.
/// </summary>
public class SignatureDiscrepancy
{
    /// <summary>What field differs.</summary>
    public string Field { get; set; } = string.Empty;

    /// <summary>The expected value.</summary>
    public string ExpectedValue { get; set; } = string.Empty;

    /// <summary>The actual value.</summary>
    public string ActualValue { get; set; } = string.Empty;

    /// <summary>Severity of the discrepancy.</summary>
    public DiscrepancySeverity Severity { get; set; }
}

/// <summary>
/// Severity levels for signature discrepancies.
/// </summary>
public enum DiscrepancySeverity
{
    /// <summary>Minor difference that can be auto-fixed.</summary>
    Warning,

    /// <summary>Significant difference requiring manual review.</summary>
    Error,

    /// <summary>Critical difference that would break compilation.</summary>
    Critical
}
