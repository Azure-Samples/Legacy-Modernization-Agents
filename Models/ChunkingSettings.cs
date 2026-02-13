using System.Text.Json.Serialization;

namespace CobolToQuarkusMigration.Models;

/// <summary>
/// Configuration settings for smart chunking of large legacy files.
/// These settings control how files are split into manageable chunks
/// for LLM processing while maintaining code context integrity.
/// </summary>
public class ChunkingSettings
{
    /// <summary>
    /// Maximum number of tokens per chunk sent to the LLM.
    /// Default: 28000 (leaves room for response in 32K context window).
    /// </summary>
    public int MaxTokensPerChunk { get; set; } = 28000;

    /// <summary>
    /// Maximum number of source lines per chunk.
    /// Used as a secondary limit alongside token count.
    /// Default: 3000 lines (reduced for API compatibility - ~100K chars).
    /// </summary>
    public int MaxLinesPerChunk { get; set; } = 3000;

    /// <summary>
    /// Number of lines to overlap between consecutive chunks.
    /// This overlap provides context continuity and helps the LLM
    /// understand relationships across chunk boundaries.
    /// Default: 500 lines.
    /// </summary>
    public int OverlapLines { get; set; } = 500;

    /// <summary>
    /// Minimum size (in lines) for a semantic unit to be treated independently.
    /// Smaller units will be grouped together.
    /// Default: 50 lines.
    /// </summary>
    public int MinSemanticUnitSize { get; set; } = 50;

    /// <summary>
    /// Whether chunking is enabled. When false, files are processed whole
    /// (only suitable for smaller files that fit in context window).
    /// Default: true.
    /// </summary>
    public bool EnableChunking { get; set; } = true;

    /// <summary>
    /// Maximum file size (in lines of code) that can be processed.
    /// Files exceeding this limit will be rejected.
    /// Default: 999999 LoC.
    /// </summary>
    public int MaxLinesOfCode { get; set; } = 999999;

    // =====================================================================
    // AUTO-CHUNKING THRESHOLDS
    // =====================================================================

    /// <summary>
    /// Character threshold for automatic chunking.
    /// Files exceeding this character count will automatically use smart chunking.
    /// Default: 150000 chars (~3000-4000 lines of COBOL).
    /// </summary>
    public int AutoChunkCharThreshold { get; set; } = 150_000;

    /// <summary>
    /// Line threshold for automatic chunking.
    /// Files exceeding this line count will automatically use smart chunking.
    /// Default: 3000 lines.
    /// </summary>
    public int AutoChunkLineThreshold { get; set; } = 3000;

    /// <summary>
    /// Whether to use progressive context compression for very large files.
    /// When enabled, older chunk contexts are compressed to save tokens.
    /// Default: true.
    /// </summary>
    public bool EnableProgressiveCompression { get; set; } = true;

    /// <summary>
    /// Compression ratio for older context (e.g., 0.3 means compress to 30% of original).
    /// Only applies when EnableProgressiveCompression is true.
    /// Default: 0.3.
    /// </summary>
    public double CompressionRatio { get; set; } = 0.3;

    /// <summary>
    /// Number of most recent chunks to keep in full detail (uncompressed).
    /// Older chunks beyond this window are compressed.
    /// Default: 3.
    /// </summary>
    public int FullDetailChunkWindow { get; set; } = 3;

    /// <summary>
    /// Whether to enable resumability (restart from last successful chunk on failure).
    /// Default: true.
    /// </summary>
    public bool EnableResumability { get; set; } = true;

    // =====================================================================
    // PARALLEL PROCESSING SETTINGS
    // =====================================================================

    /// <summary>
    /// Maximum number of chunks to process concurrently.
    /// Higher values speed up processing but increase API load.
    /// With 300K TPM limit and ~30K tokens/request, 3 parallel workers is safe.
    /// Default: 3.
    /// </summary>
    public int MaxParallelChunks { get; set; } = 3;

    /// <summary>
    /// Token budget per minute for rate limiting parallel requests.
    /// This should match your Azure OpenAI TPM (Tokens Per Minute) quota.
    /// Default: 300000 (300K TPM).
    /// </summary>
    public int TokenBudgetPerMinute { get; set; } = 300000;

    /// <summary>
    /// Minimum delay in milliseconds between starting parallel chunk processing.
    /// Helps avoid burst requests that could trigger rate limits.
    /// Default: 2000ms (2 seconds stagger between workers).
    /// </summary>
    public int ParallelStaggerDelayMs { get; set; } = 2000;

    /// <summary>
    /// Whether parallel processing is enabled.
    /// Set to false to process chunks sequentially (useful for debugging).
    /// Default: true.
    /// </summary>
    public bool EnableParallelProcessing { get; set; } = true;

    /// <summary>
    /// Safety factor for rate limiting (0.0-1.0).
    /// Uses this fraction of TokenBudgetPerMinute to avoid hitting limits.
    /// Default: 0.7 (use 70% of available capacity for safety margin).
    /// </summary>
    public double RateLimitSafetyFactor { get; set; } = 0.7;

    /// <summary>
    /// Maximum number of files to analyze concurrently during the analysis phase.
    /// Higher values speed up analysis but increase API load.
    /// Default: 6.
    /// </summary>
    public int MaxParallelAnalysis { get; set; } = 6;

    // =====================================================================
    // HELPER METHODS
    // =====================================================================

    /// <summary>
    /// Determines if a file needs chunking based on its content.
    /// </summary>
    /// <param name="content">The file content to evaluate.</param>
    /// <returns>True if the file exceeds auto-chunk thresholds and should be chunked.</returns>
    public bool RequiresChunking(string content)
    {
        if (!EnableChunking)
            return false;

        var charCount = content.Length;
        var lineCount = content.Split('\n').Length;

        return charCount > AutoChunkCharThreshold || lineCount > AutoChunkLineThreshold;
    }

    /// <summary>
    /// Determines if a file needs chunking based on character and line counts.
    /// </summary>
    /// <param name="charCount">Number of characters in the file.</param>
    /// <param name="lineCount">Number of lines in the file.</param>
    /// <returns>True if the file exceeds auto-chunk thresholds.</returns>
    public bool RequiresChunking(int charCount, int lineCount)
    {
        if (!EnableChunking)
            return false;

        return charCount > AutoChunkCharThreshold || lineCount > AutoChunkLineThreshold;
    }
}
