namespace CobolToQuarkusMigration.Chunking.Interfaces;

/// <summary>
/// Interface for managing context across chunks during conversion.
/// Responsible for building the context that accompanies each chunk to the LLM.
/// </summary>
public interface IContextManager
{
    /// <summary>
    /// Builds the context to send with a chunk for LLM processing.
    /// Includes relevant signatures, type mappings, and compressed history.
    /// </summary>
    /// <param name="runId">The migration run ID.</param>
    /// <param name="chunk">The chunk being processed.</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    /// <returns>The context to include with the chunk.</returns>
    Task<ChunkContext> BuildContextForChunkAsync(
        int runId,
        ChunkResult chunk,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Records the conversion result from a chunk for future context.
    /// </summary>
    /// <param name="runId">The migration run ID.</param>
    /// <param name="chunk">The chunk that was processed.</param>
    /// <param name="conversionResult">The conversion output from the LLM.</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    Task RecordChunkResultAsync(
        int runId,
        ChunkResult chunk,
        ChunkConversionResult conversionResult,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets the full conversion history for a file (for reconciliation pass).
    /// </summary>
    /// <param name="runId">The migration run ID.</param>
    /// <param name="sourceFile">The source file path.</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    /// <returns>The complete conversion history.</returns>
    Task<FileConversionHistory> GetFileHistoryAsync(
        int runId,
        string sourceFile,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Clears context for a specific run (for restarts).
    /// </summary>
    /// <param name="runId">The migration run ID.</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    Task ClearRunContextAsync(int runId, CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets the last successfully processed chunk index for resumability.
    /// </summary>
    /// <param name="runId">The migration run ID.</param>
    /// <param name="sourceFile">The source file path.</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    /// <returns>The last completed chunk index, or -1 if none completed.</returns>
    Task<int> GetLastCompletedChunkIndexAsync(
        int runId,
        string sourceFile,
        CancellationToken cancellationToken = default);
}

/// <summary>
/// Context information to include with a chunk for LLM processing.
/// </summary>
public class ChunkContext
{
    /// <summary>
    /// Summary of the file being processed.
    /// </summary>
    public string FileSummary { get; set; } = string.Empty;

    /// <summary>
    /// All method signatures defined so far (from previous chunks).
    /// </summary>
    public List<SignatureSummary> PreviousSignatures { get; set; } = new();

    /// <summary>
    /// All type mappings defined so far.
    /// </summary>
    public List<TypeMappingSummary> TypeMappings { get; set; } = new();

    /// <summary>
    /// Compressed summary of previous chunk conversions.
    /// </summary>
    public string CompressedHistory { get; set; } = string.Empty;

    /// <summary>
    /// Forward references that need resolution in this chunk.
    /// </summary>
    public List<ForwardReferenceSummary> PendingForwardReferences { get; set; } = new();

    /// <summary>
    /// Dependencies from this chunk that haven't been converted yet.
    /// </summary>
    public List<string> UnconvertedDependencies { get; set; } = new();

    /// <summary>
    /// Variables defined in previous chunks that may be referenced.
    /// </summary>
    public List<VariableSummary> PreviousVariables { get; set; } = new();

    /// <summary>
    /// Current chunk index (for context).
    /// </summary>
    public int CurrentChunkIndex { get; set; }

    /// <summary>
    /// Total chunk count (for context).
    /// </summary>
    public int TotalChunks { get; set; }

    /// <summary>
    /// Estimated token count for this context.
    /// </summary>
    public int EstimatedTokens { get; set; }
}

/// <summary>
/// Summary of a method signature for context.
/// </summary>
public class SignatureSummary
{
    public string LegacyName { get; set; } = string.Empty;
    public string TargetMethodName { get; set; } = string.Empty;
    public string TargetSignature { get; set; } = string.Empty;
    public string ReturnType { get; set; } = string.Empty;
    public int DefinedInChunk { get; set; }
}

/// <summary>
/// Summary of a type mapping for context.
/// </summary>
public class TypeMappingSummary
{
    public string LegacyVariable { get; set; } = string.Empty;
    public string LegacyType { get; set; } = string.Empty;
    public string TargetType { get; set; } = string.Empty;
    public string TargetFieldName { get; set; } = string.Empty;
}

/// <summary>
/// Summary of a forward reference for context.
/// </summary>
public class ForwardReferenceSummary
{
    public string CallerMethod { get; set; } = string.Empty;
    public string TargetMethod { get; set; } = string.Empty;
    public string PredictedSignature { get; set; } = string.Empty;
    public int CallerChunkIndex { get; set; }
}

/// <summary>
/// Summary of a variable for context.
/// </summary>
public class VariableSummary
{
    public string LegacyName { get; set; } = string.Empty;
    public string TargetName { get; set; } = string.Empty;
    public string TargetType { get; set; } = string.Empty;
}

/// <summary>
/// Result of converting a single chunk.
/// </summary>
public class ChunkConversionResult
{
    /// <summary>
    /// The index of the chunk that was converted.
    /// </summary>
    public int ChunkIndex { get; set; }

    /// <summary>
    /// The source file the chunk came from.
    /// </summary>
    public string SourceFile { get; set; } = string.Empty;

    /// <summary>
    /// The converted code for this chunk.
    /// </summary>
    public string ConvertedCode { get; set; } = string.Empty;

    /// <summary>
    /// Methods defined in this chunk with their signatures.
    /// </summary>
    public List<MethodSignature> DefinedMethods { get; set; } = new();

    /// <summary>
    /// Type mappings established in this chunk.
    /// </summary>
    public List<TypeMapping> TypeMappings { get; set; } = new();

    /// <summary>
    /// Forward references made in this chunk.
    /// </summary>
    public List<ForwardReference> ForwardReferences { get; set; } = new();

    /// <summary>
    /// Whether conversion was successful.
    /// </summary>
    public bool Success { get; set; }

    /// <summary>
    /// Error message if conversion failed.
    /// </summary>
    public string? ErrorMessage { get; set; }

    /// <summary>
    /// Processing time in milliseconds.
    /// </summary>
    public long ProcessingTimeMs { get; set; }

    /// <summary>
    /// Tokens used for the conversion.
    /// </summary>
    public int TokensUsed { get; set; }
}

/// <summary>
/// Represents a method signature in the target language.
/// </summary>
public class MethodSignature
{
    public string LegacyName { get; set; } = string.Empty;
    public string TargetMethodName { get; set; } = string.Empty;
    public string TargetSignature { get; set; } = string.Empty;
    public string ReturnType { get; set; } = string.Empty;
    public List<MethodParameter> Parameters { get; set; } = new();
}

/// <summary>
/// Represents a method parameter.
/// </summary>
public class MethodParameter
{
    public string Name { get; set; } = string.Empty;
    public string Type { get; set; } = string.Empty;
    public bool IsOptional { get; set; }
    public string? DefaultValue { get; set; }
}

/// <summary>
/// Represents a type mapping from legacy to target.
/// </summary>
public class TypeMapping
{
    public string LegacyVariable { get; set; } = string.Empty;
    public string LegacyType { get; set; } = string.Empty;
    public string TargetType { get; set; } = string.Empty;
    public string TargetFieldName { get; set; } = string.Empty;
    public bool IsNullable { get; set; }
    public string? DefaultValue { get; set; }
}

/// <summary>
/// Represents a forward reference to a method not yet converted.
/// </summary>
public class ForwardReference
{
    public string CallerMethod { get; set; } = string.Empty;
    public string TargetMethod { get; set; } = string.Empty;
    public string PredictedSignature { get; set; } = string.Empty;
    public int LineNumber { get; set; }
}

/// <summary>
/// Complete conversion history for a file.
/// </summary>
public class FileConversionHistory
{
    public string SourceFile { get; set; } = string.Empty;
    public int RunId { get; set; }
    public List<ChunkConversionResult> ChunkResults { get; set; } = new();
    public List<MethodSignature> AllSignatures { get; set; } = new();
    public List<TypeMapping> AllTypeMappings { get; set; } = new();
    public List<ForwardReference> UnresolvedReferences { get; set; } = new();
}
