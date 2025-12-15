using CobolToQuarkusMigration.Chunking.Interfaces;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Chunking.Core;

/// <summary>
/// Chunks large source files into manageable pieces while respecting semantic boundaries.
/// When semantic units are too large, falls back to line-based chunking with overlap.
/// </summary>
public class SemanticUnitChunker : IChunker
{
    /// <summary>
    /// Average characters per token (rough estimate for legacy code).
    /// </summary>
    private const int CharsPerToken = 4;

    public Task<IReadOnlyList<ChunkResult>> ChunkFileAsync(
        string content,
        string filePath,
        IReadOnlyList<SemanticUnit> semanticUnits,
        ChunkingSettings settings,
        CancellationToken cancellationToken = default)
    {
        var lines = content.Split('\n');
        var totalLines = lines.Length;

        // If file is small enough, return as single chunk
        if (!settings.EnableChunking || totalLines <= settings.MaxLinesPerChunk)
        {
            var singleChunk = new ChunkResult
            {
                ChunkId = $"{Path.GetFileName(filePath)}:0",
                ChunkIndex = 0,
                TotalChunks = 1,
                SourceFile = filePath,
                StartLine = 1,
                EndLine = totalLines,
                Content = content,
                EstimatedTokens = EstimateTokenCount(content),
                SemanticUnitIds = semanticUnits.Select(u => u.Id).ToList(),
                SemanticUnitNames = semanticUnits.Select(u => u.LegacyName).ToList()
            };
            return Task.FromResult<IReadOnlyList<ChunkResult>>(new[] { singleChunk });
        }

        // Check if any semantic unit is larger than MaxLinesPerChunk
        // If so, we need line-based chunking as a fallback
        var hasOversizedUnits = semanticUnits.Any(u => u.LineCount > settings.MaxLinesPerChunk);
        
        if (hasOversizedUnits)
        {
            // Use line-based chunking with overlap for oversized files
            return Task.FromResult(CreateLineBasedChunks(
                lines, filePath, semanticUnits, settings));
        }

        // Determine chunk boundaries based on semantic units
        var boundaries = DetermineChunkBoundaries(
            semanticUnits,
            settings.MaxTokensPerChunk,
            settings.MaxLinesPerChunk,
            settings.OverlapLines);

        // Create chunks based on boundaries
        var chunks = CreateSemanticChunks(
            lines, filePath, semanticUnits, boundaries, settings);

        return Task.FromResult<IReadOnlyList<ChunkResult>>(chunks);
    }

    /// <summary>
    /// Creates chunks based on semantic unit boundaries.
    /// </summary>
    private IReadOnlyList<ChunkResult> CreateSemanticChunks(
        string[] lines,
        string filePath,
        IReadOnlyList<SemanticUnit> semanticUnits,
        IReadOnlyList<(int StartIndex, int EndIndex)> boundaries,
        ChunkingSettings settings)
    {
        var chunks = new List<ChunkResult>();
        var totalChunks = boundaries.Count;

        for (int i = 0; i < boundaries.Count; i++)
        {
            var (startIdx, endIdx) = boundaries[i];
            var unitsInChunk = semanticUnits.Skip(startIdx).Take(endIdx - startIdx + 1).ToList();

            var startLine = unitsInChunk.Min(u => u.StartLine);
            var endLine = unitsInChunk.Max(u => u.EndLine);

            // Adjust for overlap
            string? overlapContent = null;
            if (i > 0 && settings.OverlapLines > 0)
            {
                var overlapStartLine = Math.Max(1, startLine - settings.OverlapLines);
                overlapContent = ExtractLines(lines, overlapStartLine, startLine - 1);
            }

            var chunkContent = ExtractLines(lines, startLine, endLine);

            // Identify dependencies
            var outbound = new List<string>();
            var inbound = new List<string>();
            var unitsInChunkNames = unitsInChunk.Select(u => u.LegacyName.ToUpperInvariant()).ToHashSet();

            foreach (var unit in unitsInChunk)
            {
                foreach (var dep in unit.Dependencies)
                {
                    if (!unitsInChunkNames.Contains(dep.ToUpperInvariant()))
                    {
                        outbound.Add(dep);
                    }
                }

                foreach (var dep in unit.Dependents)
                {
                    if (!unitsInChunkNames.Contains(dep.ToUpperInvariant()))
                    {
                        inbound.Add(dep);
                    }
                }
            }

            var chunk = new ChunkResult
            {
                ChunkId = $"{Path.GetFileName(filePath)}:{i}",
                ChunkIndex = i,
                TotalChunks = totalChunks,
                SourceFile = filePath,
                StartLine = startLine,
                EndLine = endLine,
                Content = chunkContent,
                EstimatedTokens = EstimateTokenCount(chunkContent),
                SemanticUnitIds = unitsInChunk.Select(u => u.Id).ToList(),
                SemanticUnitNames = unitsInChunk.Select(u => u.LegacyName).ToList(),
                OutboundDependencies = outbound.Distinct().ToList(),
                InboundDependencies = inbound.Distinct().ToList(),
                OverlapFromPrevious = overlapContent
            };

            chunks.Add(chunk);
        }

        return chunks;
    }

    /// <summary>
    /// Creates chunks based on line count when semantic units are too large.
    /// This is the fallback mechanism for files with oversized semantic units.
    /// </summary>
    private IReadOnlyList<ChunkResult> CreateLineBasedChunks(
        string[] lines,
        string filePath,
        IReadOnlyList<SemanticUnit> semanticUnits,
        ChunkingSettings settings)
    {
        var chunks = new List<ChunkResult>();
        var totalLines = lines.Length;
        var effectiveChunkSize = settings.MaxLinesPerChunk - settings.OverlapLines;
        
        // Calculate total chunks needed
        var totalChunks = (int)Math.Ceiling((double)totalLines / effectiveChunkSize);
        
        for (int chunkIndex = 0; chunkIndex < totalChunks; chunkIndex++)
        {
            var startLine = chunkIndex * effectiveChunkSize + 1;
            var endLine = Math.Min(startLine + settings.MaxLinesPerChunk - 1, totalLines);
            
            // Adjust start line for overlap (except first chunk)
            var actualStartLine = startLine;
            string? overlapContent = null;
            
            if (chunkIndex > 0 && settings.OverlapLines > 0)
            {
                var overlapStartLine = Math.Max(1, startLine - settings.OverlapLines);
                overlapContent = ExtractLines(lines, overlapStartLine, startLine - 1);
            }

            var chunkContent = ExtractLines(lines, actualStartLine, endLine);

            // Find semantic units that overlap with this chunk
            var unitsInChunk = semanticUnits
                .Where(u => u.StartLine <= endLine && u.EndLine >= actualStartLine)
                .ToList();

            // Calculate dependencies
            var outbound = new List<string>();
            var inbound = new List<string>();
            var unitsInChunkNames = unitsInChunk.Select(u => u.LegacyName.ToUpperInvariant()).ToHashSet();

            foreach (var unit in unitsInChunk)
            {
                foreach (var dep in unit.Dependencies)
                {
                    if (!unitsInChunkNames.Contains(dep.ToUpperInvariant()))
                    {
                        outbound.Add(dep);
                    }
                }

                foreach (var dep in unit.Dependents)
                {
                    if (!unitsInChunkNames.Contains(dep.ToUpperInvariant()))
                    {
                        inbound.Add(dep);
                    }
                }
            }

            var chunk = new ChunkResult
            {
                ChunkId = $"{Path.GetFileName(filePath)}:{chunkIndex}",
                ChunkIndex = chunkIndex,
                TotalChunks = totalChunks,
                SourceFile = filePath,
                StartLine = actualStartLine,
                EndLine = endLine,
                Content = chunkContent,
                EstimatedTokens = EstimateTokenCount(chunkContent),
                SemanticUnitIds = unitsInChunk.Select(u => u.Id).ToList(),
                SemanticUnitNames = unitsInChunk.Select(u => u.LegacyName).ToList(),
                OutboundDependencies = outbound.Distinct().ToList(),
                InboundDependencies = inbound.Distinct().ToList(),
                OverlapFromPrevious = overlapContent
            };

            chunks.Add(chunk);
        }

        return chunks;
    }

    public int EstimateTokenCount(string text)
    {
        if (string.IsNullOrEmpty(text))
            return 0;

        return text.Length / CharsPerToken;
    }

    public IReadOnlyList<(int StartIndex, int EndIndex)> DetermineChunkBoundaries(
        IReadOnlyList<SemanticUnit> semanticUnits,
        int maxTokensPerChunk,
        int maxLinesPerChunk,
        int overlapLines)
    {
        if (semanticUnits.Count == 0)
            return Array.Empty<(int, int)>();

        var boundaries = new List<(int StartIndex, int EndIndex)>();
        var currentStart = 0;
        var currentTokens = 0;
        var currentLines = 0;

        // Priority order for chunk boundaries:
        // 1. Division boundaries (highest priority - always break here)
        // 2. Section boundaries (high priority)
        // 3. Paragraph boundaries (normal priority)
        // 4. Any semantic unit (fallback)

        for (int i = 0; i < semanticUnits.Count; i++)
        {
            var unit = semanticUnits[i];
            var unitTokens = unit.EstimatedTokens;
            var unitLines = unit.LineCount;

            // Check if adding this unit would exceed limits
            if (currentTokens + unitTokens > maxTokensPerChunk ||
                currentLines + unitLines > maxLinesPerChunk)
            {
                // We need to create a boundary
                if (i > currentStart)
                {
                    // Try to find a good boundary point
                    var boundaryIdx = FindBestBoundary(semanticUnits, currentStart, i - 1);
                    boundaries.Add((currentStart, boundaryIdx));
                    currentStart = boundaryIdx + 1;
                    currentTokens = 0;
                    currentLines = 0;

                    // Recalculate for any units we're carrying over
                    for (int j = currentStart; j <= i; j++)
                    {
                        currentTokens += semanticUnits[j].EstimatedTokens;
                        currentLines += semanticUnits[j].LineCount;
                    }
                }
                else
                {
                    // Single unit exceeds limits - include it anyway and move on
                    boundaries.Add((currentStart, i));
                    currentStart = i + 1;
                    currentTokens = 0;
                    currentLines = 0;
                }
            }
            else
            {
                currentTokens += unitTokens;
                currentLines += unitLines;
            }
        }

        // Add final chunk if there are remaining units
        if (currentStart < semanticUnits.Count)
        {
            boundaries.Add((currentStart, semanticUnits.Count - 1));
        }

        return boundaries;
    }

    private static int FindBestBoundary(IReadOnlyList<SemanticUnit> units, int start, int end)
    {
        // Look for the best boundary point, preferring divisions > sections > paragraphs

        // First, look for division boundaries
        for (int i = end; i >= start; i--)
        {
            if (IsDivisionBoundary(units[i]))
            {
                return i;
            }
        }

        // Then look for section boundaries
        for (int i = end; i >= start; i--)
        {
            if (units[i].UnitType == SemanticUnitType.Section)
            {
                return i;
            }
        }

        // Then look for paragraph boundaries
        for (int i = end; i >= start; i--)
        {
            if (units[i].UnitType == SemanticUnitType.Paragraph)
            {
                return i;
            }
        }

        // Fallback to the end
        return end;
    }

    private static bool IsDivisionBoundary(SemanticUnit unit)
    {
        return unit.UnitType is
            SemanticUnitType.IdentificationDivision or
            SemanticUnitType.EnvironmentDivision or
            SemanticUnitType.DataDivision or
            SemanticUnitType.ProcedureDivision;
    }

    private static string ExtractLines(string[] lines, int startLine, int endLine)
    {
        var start = Math.Max(0, startLine - 1);
        var end = Math.Min(lines.Length, endLine);
        return string.Join("\n", lines.Skip(start).Take(end - start));
    }
}
