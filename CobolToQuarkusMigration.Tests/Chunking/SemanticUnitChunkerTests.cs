using Xunit;
using FluentAssertions;
using CobolToQuarkusMigration.Chunking.Core;
using CobolToQuarkusMigration.Chunking.Interfaces;
using CobolToQuarkusMigration.Models;
using Microsoft.Extensions.Logging;
using Moq;

namespace CobolToQuarkusMigration.Tests.Chunking;

public class SemanticUnitChunkerTests
{
    private readonly SemanticUnitChunker _chunker;

    public SemanticUnitChunkerTests()
    {
        _chunker = new SemanticUnitChunker();
    }

    [Fact]
    public async Task ChunkFileAsync_SmallFile_ReturnsSingleChunk()
    {
        // Arrange
        var content = GenerateCobolContent(50);
        var settings = new ChunkingSettings
        {
            EnableChunking = true,
            MaxLinesPerChunk = 100,
            MaxTokensPerChunk = 4000
        };
        var semanticUnits = new List<SemanticUnit>
        {
            new() { Id = "1", LegacyName = "MAIN-PARA", StartLine = 1, EndLine = 50 }
        };

        // Act
        var chunks = await _chunker.ChunkFileAsync(content, "test.cbl", semanticUnits, settings);

        // Assert
        chunks.Should().HaveCount(1);
        chunks[0].ChunkIndex.Should().Be(0);
        chunks[0].TotalChunks.Should().Be(1);
    }

    [Fact]
    public async Task ChunkFileAsync_LargeFile_ReturnsMultipleChunks()
    {
        // Arrange
        var content = GenerateCobolContent(500);
        var settings = new ChunkingSettings
        {
            EnableChunking = true,
            MaxLinesPerChunk = 100,
            MaxTokensPerChunk = 4000,
            OverlapLines = 10
        };
        var semanticUnits = CreateSemanticUnits(10, 50); // 10 units, 50 lines each

        // Act
        var chunks = await _chunker.ChunkFileAsync(content, "large.cbl", semanticUnits, settings);

        // Assert
        chunks.Should().HaveCountGreaterThan(1);
        chunks.Should().AllSatisfy(c => c.SourceFile.Should().Be("large.cbl"));

        // Verify sequential indices
        for (int i = 0; i < chunks.Count; i++)
        {
            chunks[i].ChunkIndex.Should().Be(i);
            chunks[i].TotalChunks.Should().Be(chunks.Count);
        }
    }

    [Fact]
    public async Task ChunkFileAsync_ChunkingDisabled_ReturnsSingleChunk()
    {
        // Arrange
        var content = GenerateCobolContent(500);
        var settings = new ChunkingSettings
        {
            EnableChunking = false,
            MaxLinesPerChunk = 100
        };
        var semanticUnits = CreateSemanticUnits(10, 50);

        // Act
        var chunks = await _chunker.ChunkFileAsync(content, "test.cbl", semanticUnits, settings);

        // Assert
        chunks.Should().HaveCount(1);
        chunks[0].TotalChunks.Should().Be(1);
    }

    [Fact]
    public async Task ChunkFileAsync_WithDependencies_TracksDependencies()
    {
        // Arrange
        var content = GenerateCobolContent(300);
        var settings = new ChunkingSettings
        {
            EnableChunking = true,
            MaxLinesPerChunk = 100,
            MaxTokensPerChunk = 4000
        };
        var semanticUnits = new List<SemanticUnit>
        {
            new() { Id = "1", LegacyName = "MAIN-PARA", StartLine = 1, EndLine = 100, Dependencies = new List<string> { "HELPER-PARA" }, Dependents = new List<string>() },
            new() { Id = "2", LegacyName = "HELPER-PARA", StartLine = 101, EndLine = 200, Dependencies = new List<string>(), Dependents = new List<string> { "MAIN-PARA" } },
            new() { Id = "3", LegacyName = "CLEANUP-PARA", StartLine = 201, EndLine = 300, Dependencies = new List<string>(), Dependents = new List<string>() }
        };

        // Act
        var chunks = await _chunker.ChunkFileAsync(content, "deps.cbl", semanticUnits, settings);

        // Assert
        chunks.Should().NotBeEmpty();
        // At least one chunk should have outbound dependencies
        chunks.Any(c => c.OutboundDependencies.Count > 0 || c.InboundDependencies.Count > 0).Should().BeTrue();
    }

    [Fact]
    public void EstimateTokenCount_ReturnsReasonableValue()
    {
        // Arrange
        var text = "This is a sample text for token estimation testing purposes.";

        // Act
        var tokens = _chunker.EstimateTokenCount(text);

        // Assert
        // ~4 chars per token, so ~60 chars should be ~15 tokens
        tokens.Should().BeInRange(10, 25);
    }

    [Fact]
    public void EstimateTokenCount_EmptyString_ReturnsZero()
    {
        // Act
        var tokens = _chunker.EstimateTokenCount("");

        // Assert
        tokens.Should().Be(0);
    }

    [Fact]
    public void DetermineChunkBoundaries_SingleUnit_ReturnsSingleBoundary()
    {
        // Arrange
        var units = new List<SemanticUnit>
        {
            new() { Id = "1", LegacyName = "MAIN", EstimatedTokens = 100, StartLine = 1, EndLine = 50 }
        };

        // Act
        var boundaries = _chunker.DetermineChunkBoundaries(units, 1000, 100, 10);

        // Assert
        boundaries.Should().HaveCount(1);
        boundaries[0].Should().Be((0, 0));
    }

    [Fact]
    public void DetermineChunkBoundaries_MultipleUnitsExceedingLimit_CreatesBoundaries()
    {
        // Arrange
        var units = new List<SemanticUnit>
        {
            new() { Id = "1", LegacyName = "UNIT-1", EstimatedTokens = 500, StartLine = 1, EndLine = 40, UnitType = SemanticUnitType.Paragraph },
            new() { Id = "2", LegacyName = "UNIT-2", EstimatedTokens = 500, StartLine = 41, EndLine = 80, UnitType = SemanticUnitType.Paragraph },
            new() { Id = "3", LegacyName = "UNIT-3", EstimatedTokens = 500, StartLine = 81, EndLine = 120, UnitType = SemanticUnitType.Paragraph },
            new() { Id = "4", LegacyName = "UNIT-4", EstimatedTokens = 500, StartLine = 121, EndLine = 160, UnitType = SemanticUnitType.Paragraph }
        };

        // Act - limit to 1000 tokens max
        var boundaries = _chunker.DetermineChunkBoundaries(units, 1000, 100, 10);

        // Assert
        boundaries.Should().HaveCountGreaterThan(1);
    }

    private static string GenerateCobolContent(int lineCount)
    {
        var lines = new List<string>
        {
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. TEST-PROG.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01 WS-VAR PIC X(10).",
            "       PROCEDURE DIVISION."
        };

        while (lines.Count < lineCount)
        {
            lines.Add($"           DISPLAY 'LINE {lines.Count}'.");
        }

        return string.Join("\n", lines.Take(lineCount));
    }

    private static List<SemanticUnit> CreateSemanticUnits(int count, int linesPerUnit)
    {
        var units = new List<SemanticUnit>();
        for (int i = 0; i < count; i++)
        {
            var startLine = i * linesPerUnit + 1;
            units.Add(new SemanticUnit
            {
                Id = $"{i}",
                LegacyName = $"PARAGRAPH-{i}",
                StartLine = startLine,
                EndLine = startLine + linesPerUnit - 1,
                EstimatedTokens = linesPerUnit * 10,
                UnitType = SemanticUnitType.Paragraph
            });
        }
        return units;
    }
}
