using CobolToQuarkusMigration.Chunking.Interfaces;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Agents.Interfaces;

/// <summary>
/// Interface for code converter agents that support chunk-based conversion.
/// This extends the standard conversion capability to support large file processing.
/// </summary>
public interface IChunkAwareConverter : ICodeConverterAgent
{
    /// <summary>
    /// Converts a single chunk of COBOL code to the target language.
    /// </summary>
    /// <param name="chunk">The chunk to convert.</param>
    /// <param name="context">The context containing previous chunk results and type mappings.</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    /// <returns>The result of the conversion.</returns>
    Task<ChunkConversionResult> ConvertChunkAsync(
        ChunkResult chunk,
        ChunkContext context,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Applies corrections to a previously converted chunk.
    /// Used during the reconciliation pass to fix consistency issues.
    /// </summary>
    /// <param name="originalChunk">The original chunk.</param>
    /// <param name="originalConversion">The original conversion result.</param>
    /// <param name="corrections">The corrections to apply.</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    /// <returns>The corrected conversion result.</returns>
    Task<ChunkConversionResult> ApplyCorrectionsAsync(
        ChunkResult originalChunk,
        ChunkConversionResult originalConversion,
        IReadOnlyList<string> corrections,
        CancellationToken cancellationToken = default);
}
