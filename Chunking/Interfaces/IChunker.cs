using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Chunking.Interfaces;

/// <summary>
/// Interface for chunking large source files into manageable pieces
/// while maintaining semantic boundaries and context.
/// </summary>
public interface IChunker
{
    /// <summary>
    /// Splits a source file into chunks suitable for LLM processing.
    /// </summary>
    /// <param name="content">The full source code content.</param>
    /// <param name="filePath">Path to the source file.</param>
    /// <param name="semanticUnits">Pre-identified semantic units from the language adapter.</param>
    /// <param name="settings">Chunking configuration settings.</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    /// <returns>A list of chunks with their metadata.</returns>
    Task<IReadOnlyList<ChunkResult>> ChunkFileAsync(
        string content,
        string filePath,
        IReadOnlyList<SemanticUnit> semanticUnits,
        ChunkingSettings settings,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Estimates the token count for a piece of text.
    /// </summary>
    /// <param name="text">The text to estimate tokens for.</param>
    /// <returns>Estimated token count.</returns>
    int EstimateTokenCount(string text);

    /// <summary>
    /// Determines the optimal chunk boundaries given semantic units and size constraints.
    /// </summary>
    /// <param name="semanticUnits">The semantic units to group into chunks.</param>
    /// <param name="maxTokensPerChunk">Maximum tokens per chunk.</param>
    /// <param name="maxLinesPerChunk">Maximum lines per chunk.</param>
    /// <param name="overlapLines">Number of lines to overlap between chunks.</param>
    /// <returns>A list of chunk boundaries (start/end indices into semantic units).</returns>
    IReadOnlyList<(int StartIndex, int EndIndex)> DetermineChunkBoundaries(
        IReadOnlyList<SemanticUnit> semanticUnits,
        int maxTokensPerChunk,
        int maxLinesPerChunk,
        int overlapLines);
}

/// <summary>
/// Represents a chunk of source code ready for LLM processing.
/// </summary>
public class ChunkResult
{
    /// <summary>
    /// Unique identifier for this chunk within the file processing run.
    /// </summary>
    public string ChunkId { get; set; } = string.Empty;

    /// <summary>
    /// Zero-based index of this chunk in the sequence.
    /// </summary>
    public int ChunkIndex { get; set; }

    /// <summary>
    /// Total number of chunks for this file.
    /// </summary>
    public int TotalChunks { get; set; }

    /// <summary>
    /// The source file this chunk belongs to.
    /// </summary>
    public string SourceFile { get; set; } = string.Empty;

    /// <summary>
    /// Starting line number (1-based) in the original file.
    /// </summary>
    public int StartLine { get; set; }

    /// <summary>
    /// Ending line number (1-based) in the original file.
    /// </summary>
    public int EndLine { get; set; }

    /// <summary>
    /// The actual source code content for this chunk.
    /// </summary>
    public string Content { get; set; } = string.Empty;

    /// <summary>
    /// Estimated token count for the content.
    /// </summary>
    public int EstimatedTokens { get; set; }

    /// <summary>
    /// Line count for this chunk.
    /// </summary>
    public int LineCount => EndLine - StartLine + 1;

    /// <summary>
    /// List of semantic unit IDs included in this chunk.
    /// </summary>
    public List<string> SemanticUnitIds { get; set; } = new();

    /// <summary>
    /// List of semantic unit names included in this chunk (for context).
    /// </summary>
    public List<string> SemanticUnitNames { get; set; } = new();

    /// <summary>
    /// Full semantic units included in this chunk (populated after chunking).
    /// </summary>
    public List<SemanticUnit> SemanticUnits { get; set; } = new();

    /// <summary>
    /// Dependencies from this chunk to other semantic units (not in this chunk).
    /// These are forward references that need to be tracked.
    /// </summary>
    public List<string> OutboundDependencies { get; set; } = new();

    /// <summary>
    /// Dependencies to this chunk from other semantic units (not in this chunk).
    /// These are reverse references for context.
    /// </summary>
    public List<string> InboundDependencies { get; set; } = new();

    /// <summary>
    /// Variables referenced in this chunk.
    /// </summary>
    public List<string> ReferencedVariables { get; set; } = new();

    /// <summary>
    /// Variables defined in this chunk.
    /// </summary>
    public List<string> DefinedVariables { get; set; } = new();

    /// <summary>
    /// Overlap content from the previous chunk (for context continuity).
    /// </summary>
    public string? OverlapFromPrevious { get; set; }

    /// <summary>
    /// Whether this is the first chunk in the file.
    /// </summary>
    public bool IsFirstChunk => ChunkIndex == 0;

    /// <summary>
    /// Whether this is the last chunk in the file.
    /// </summary>
    public bool IsLastChunk => ChunkIndex == TotalChunks - 1;
}
