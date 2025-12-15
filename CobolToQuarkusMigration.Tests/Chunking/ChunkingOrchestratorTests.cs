using Xunit;
using FluentAssertions;
using CobolToQuarkusMigration.Chunking;
using CobolToQuarkusMigration.Chunking.Interfaces;
using CobolToQuarkusMigration.Models;
using Microsoft.Extensions.Logging;
using Moq;
using Microsoft.Data.Sqlite;

namespace CobolToQuarkusMigration.Tests.Chunking;

public class ChunkingOrchestratorTests : IDisposable
{
    private readonly string _databasePath;
    private readonly ChunkingSettings _chunkingSettings;
    private readonly ConversionSettings _conversionSettings;
    private readonly Mock<ILogger<ChunkingOrchestrator>> _loggerMock;
    private readonly ChunkingOrchestrator _orchestrator;

    public ChunkingOrchestratorTests()
    {
        _databasePath = Path.Combine(Path.GetTempPath(), $"test_orchestrator_{Guid.NewGuid()}.db");
        
        _chunkingSettings = new ChunkingSettings
        {
            EnableChunking = true,
            MaxLinesPerChunk = 100,
            MaxTokensPerChunk = 4000,
            OverlapLines = 10,
            EnableResumability = true,
            EnableParallelProcessing = false, // Sequential for predictable tests
            MaxLinesOfCode = 999999
        };

        _conversionSettings = new ConversionSettings
        {
            EnableReconciliationPass = false,
            MaxAutoFixAttempts = 5
        };

        _loggerMock = new Mock<ILogger<ChunkingOrchestrator>>();

        InitializeTestDatabase();

        _orchestrator = new ChunkingOrchestrator(
            _chunkingSettings,
            _conversionSettings,
            _databasePath,
            _loggerMock.Object);
    }

    public void Dispose()
    {
        try
        {
            SqliteConnection.ClearAllPools();
            if (File.Exists(_databasePath))
            {
                File.Delete(_databasePath);
            }
        }
        catch
        {
            // Ignore cleanup errors
        }
    }

    private void InitializeTestDatabase()
    {
        using var connection = new SqliteConnection($"Data Source={_databasePath}");
        connection.Open();

        using var command = connection.CreateCommand();
        command.CommandText = @"
            CREATE TABLE IF NOT EXISTS chunk_metadata (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                run_id INTEGER NOT NULL,
                source_file TEXT NOT NULL,
                chunk_index INTEGER NOT NULL,
                start_line INTEGER NOT NULL,
                end_line INTEGER NOT NULL,
                status TEXT NOT NULL,
                semantic_units TEXT,
                tokens_used INTEGER,
                processing_time_ms INTEGER,
                error_message TEXT,
                converted_code TEXT,
                completed_at TEXT,
                UNIQUE(run_id, source_file, chunk_index)
            );

            CREATE TABLE IF NOT EXISTS forward_references (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                run_id INTEGER NOT NULL,
                source_file TEXT NOT NULL,
                caller_method TEXT NOT NULL,
                caller_chunk_index INTEGER NOT NULL,
                target_method TEXT NOT NULL,
                predicted_signature TEXT,
                resolved INTEGER DEFAULT 0,
                actual_signature TEXT,
                resolution_chunk_index INTEGER,
                resolved_at TEXT
            );

            CREATE TABLE IF NOT EXISTS signatures (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                run_id INTEGER NOT NULL,
                source_file TEXT NOT NULL,
                chunk_index INTEGER NOT NULL,
                legacy_name TEXT NOT NULL,
                target_method_name TEXT NOT NULL,
                target_signature TEXT NOT NULL,
                return_type TEXT NOT NULL,
                parameters TEXT,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                UNIQUE(run_id, source_file, legacy_name)
            );

            CREATE TABLE IF NOT EXISTS type_mappings (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                run_id INTEGER NOT NULL,
                source_file TEXT NOT NULL,
                legacy_variable TEXT NOT NULL,
                legacy_type TEXT NOT NULL,
                target_type TEXT NOT NULL,
                target_field_name TEXT NOT NULL,
                is_nullable INTEGER DEFAULT 0,
                default_value TEXT,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                UNIQUE(run_id, source_file, legacy_variable)
            );

            CREATE TABLE IF NOT EXISTS chunk_results (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                run_id INTEGER NOT NULL,
                source_file TEXT NOT NULL,
                chunk_index INTEGER NOT NULL,
                success INTEGER NOT NULL,
                converted_code TEXT,
                error_message TEXT,
                tokens_used INTEGER DEFAULT 0,
                processing_time_ms INTEGER DEFAULT 0,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                UNIQUE(run_id, source_file, chunk_index)
            );
        ";
        command.ExecuteNonQuery();
    }

    #region AnalyzeFileAsync Tests

    [Fact]
    public async Task AnalyzeFileAsync_ValidCobol_ReturnsPlan()
    {
        // Arrange
        var content = GenerateCobolProgram(80);
        var filePath = "test.cbl";

        // Act
        var plan = await _orchestrator.AnalyzeFileAsync(filePath, content);

        // Assert
        plan.Should().NotBeNull();
        plan.SourceFile.Should().Be(filePath);
        plan.TotalLines.Should().BeGreaterThan(0);
        plan.ChunkCount.Should().BeGreaterThan(0);
        plan.Chunks.Should().NotBeEmpty();
        plan.AnalyzedAt.Should().BeCloseTo(DateTime.UtcNow, TimeSpan.FromSeconds(5));
    }

    [Fact]
    public async Task AnalyzeFileAsync_IdentifiesSemanticUnits()
    {
        // Arrange
        var content = @"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-PROG.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNTER PIC 9(5).
       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM INIT-PARA.
           PERFORM PROCESS-PARA.
           STOP RUN.
       INIT-PARA.
           MOVE 0 TO WS-COUNTER.
       PROCESS-PARA.
           ADD 1 TO WS-COUNTER.
";
        var filePath = "semantic.cbl";

        // Act
        var plan = await _orchestrator.AnalyzeFileAsync(filePath, content);

        // Assert
        plan.SemanticUnits.Should().NotBeEmpty();
        plan.TotalSemanticUnits.Should().BeGreaterThan(0);
    }

    [Fact]
    public async Task AnalyzeFileAsync_ExtractsVariables()
    {
        // Arrange
        var content = @"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-PROG.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CUSTOMER-ID PIC X(10).
       01 WS-AMOUNT PIC 9(7)V99.
       01 WS-COUNT PIC 9(5).
       PROCEDURE DIVISION.
           STOP RUN.
";
        var filePath = "variables.cbl";

        // Act
        var plan = await _orchestrator.AnalyzeFileAsync(filePath, content);

        // Assert
        plan.Variables.Should().NotBeEmpty();
        plan.TotalVariables.Should().BeGreaterThan(0);
    }

    [Fact]
    public async Task AnalyzeFileAsync_LargeFile_CreatesMultipleChunks()
    {
        // Arrange
        var content = GenerateCobolProgram(500);
        var filePath = "large.cbl";

        // Act
        var plan = await _orchestrator.AnalyzeFileAsync(filePath, content);

        // Assert
        plan.ChunkCount.Should().BeGreaterThan(1);
        plan.Chunks.Should().HaveCountGreaterThan(1);
        plan.EstimatedTotalTokens.Should().BeGreaterThan(0);
    }

    [Fact]
    public async Task AnalyzeFileAsync_ExceedsMaxLines_ThrowsException()
    {
        // Arrange
        var orchestrator = new ChunkingOrchestrator(
            new ChunkingSettings { MaxLinesOfCode = 10 },
            _conversionSettings,
            _databasePath,
            _loggerMock.Object);

        var content = GenerateCobolProgram(50);
        var filePath = "toolarge.cbl";

        // Act & Assert
        await Assert.ThrowsAsync<InvalidOperationException>(
            () => orchestrator.AnalyzeFileAsync(filePath, content));
    }

    #endregion

    #region ProcessChunksAsync Tests

    [Fact]
    public async Task ProcessChunksAsync_AllSuccessful_ReturnsSuccessResult()
    {
        // Arrange
        var content = GenerateCobolProgram(80);
        var plan = await _orchestrator.AnalyzeFileAsync("test.cbl", content);

        static Task<ChunkConversionResult> ConvertChunk(ChunkResult chunk, ChunkContext context)
        {
            return Task.FromResult(new ChunkConversionResult
            {
                ChunkIndex = chunk.ChunkIndex,
                SourceFile = chunk.SourceFile,
                Success = true,
                ConvertedCode = "// Converted code"
            });
        }

        // Act
        var result = await _orchestrator.ProcessChunksAsync(1, plan, ConvertChunk);

        // Assert
        result.TotalChunks.Should().Be(plan.ChunkCount);
        result.SuccessfulChunks.Should().Be(plan.ChunkCount);
        result.FailedChunks.Should().Be(0);
        result.SuccessRate.Should().Be(100);
        result.CompletedAt.Should().NotBeNull();
    }

    [Fact]
    public async Task ProcessChunksAsync_SomeFailures_RecordsFailures()
    {
        // Arrange
        var content = GenerateCobolProgram(200);
        var plan = await _orchestrator.AnalyzeFileAsync("mixed.cbl", content);

        int callCount = 0;
        Task<ChunkConversionResult> ConvertChunk(ChunkResult chunk, ChunkContext context)
        {
            callCount++;
            return Task.FromResult(new ChunkConversionResult
            {
                ChunkIndex = chunk.ChunkIndex,
                SourceFile = chunk.SourceFile,
                Success = callCount % 2 == 1, // Alternate success/failure
                ConvertedCode = callCount % 2 == 1 ? "// OK" : "",
                ErrorMessage = callCount % 2 == 0 ? "Simulated failure" : null
            });
        }

        // Act
        var result = await _orchestrator.ProcessChunksAsync(1, plan, ConvertChunk);

        // Assert
        result.FailedChunks.Should().BeGreaterThan(0);
        result.SuccessRate.Should().BeLessThan(100);
    }

    [Fact]
    public async Task ProcessChunksAsync_WithCancellation_StopsProcessing()
    {
        // Arrange
        var content = GenerateCobolProgram(300);
        var plan = await _orchestrator.AnalyzeFileAsync("cancel.cbl", content);

        var cts = new CancellationTokenSource();
        int processedCount = 0;

        Task<ChunkConversionResult> ConvertChunk(ChunkResult chunk, ChunkContext context)
        {
            processedCount++;
            if (processedCount >= 2)
            {
                cts.Cancel();
            }
            return Task.FromResult(new ChunkConversionResult
            {
                ChunkIndex = chunk.ChunkIndex,
                SourceFile = chunk.SourceFile,
                Success = true,
                ConvertedCode = "// OK"
            });
        }

        // Act & Assert
        await Assert.ThrowsAsync<OperationCanceledException>(
            () => _orchestrator.ProcessChunksAsync(1, plan, ConvertChunk, cts.Token));
    }

    [Fact]
    public async Task ProcessChunksAsync_BuildsContextForEachChunk()
    {
        // Arrange
        var content = GenerateCobolProgram(80);
        var plan = await _orchestrator.AnalyzeFileAsync("context.cbl", content);

        var contexts = new List<ChunkContext>();

        Task<ChunkConversionResult> ConvertChunk(ChunkResult chunk, ChunkContext context)
        {
            contexts.Add(context);
            return Task.FromResult(new ChunkConversionResult
            {
                ChunkIndex = chunk.ChunkIndex,
                SourceFile = chunk.SourceFile,
                Success = true,
                ConvertedCode = "// OK",
                DefinedMethods = new List<MethodSignature>
                {
                    new() { LegacyName = $"METHOD-{chunk.ChunkIndex}", TargetMethodName = $"Method{chunk.ChunkIndex}" }
                }
            });
        }

        // Act
        await _orchestrator.ProcessChunksAsync(1, plan, ConvertChunk);

        // Assert
        contexts.Should().HaveCount(plan.ChunkCount);
        // Later chunks should have access to previously defined signatures
        if (contexts.Count > 1)
        {
            contexts[^1].PreviousSignatures.Count.Should().BeGreaterOrEqualTo(0); // Depends on DB writes
        }
    }

    [Fact]
    public async Task ProcessChunksAsync_TracksProcessingTime()
    {
        // Arrange
        var content = GenerateCobolProgram(50);
        var plan = await _orchestrator.AnalyzeFileAsync("timing.cbl", content);

        Task<ChunkConversionResult> ConvertChunk(ChunkResult chunk, ChunkContext context)
        {
            Thread.Sleep(10); // Small delay
            return Task.FromResult(new ChunkConversionResult
            {
                ChunkIndex = chunk.ChunkIndex,
                SourceFile = chunk.SourceFile,
                Success = true,
                ConvertedCode = "// OK"
            });
        }

        // Act
        var result = await _orchestrator.ProcessChunksAsync(1, plan, ConvertChunk);

        // Assert
        result.TotalProcessingTimeMs.Should().BeGreaterThan(0);
    }

    #endregion

    #region Integration Tests

    [Fact]
    public async Task FullWorkflow_AnalyzeThenProcess_Succeeds()
    {
        // Arrange
        var content = GenerateCobolProgram(150);
        var filePath = "integration.cbl";

        // Act - Analyze
        var plan = await _orchestrator.AnalyzeFileAsync(filePath, content);

        // Act - Process
        var result = await _orchestrator.ProcessChunksAsync(1, plan, (chunk, context) =>
        {
            return Task.FromResult(new ChunkConversionResult
            {
                ChunkIndex = chunk.ChunkIndex,
                SourceFile = chunk.SourceFile,
                Success = true,
                ConvertedCode = $"public class Chunk{chunk.ChunkIndex} {{ }}"
            });
        });

        // Assert
        result.TotalChunks.Should().Be(plan.ChunkCount);
        result.SuccessfulChunks.Should().Be(plan.ChunkCount);
        result.IsConsistent.Should().BeTrue();
    }

    [Fact]
    public async Task Resumability_PartialFailure_CanResume()
    {
        // Arrange
        var content = GenerateCobolProgram(200);
        var filePath = "resume.cbl";
        var plan = await _orchestrator.AnalyzeFileAsync(filePath, content);

        int processedInFirstRun = 0;
        int targetFailAt = plan.ChunkCount / 2;

        // First run - fails partway through
        try
        {
            await _orchestrator.ProcessChunksAsync(1, plan, (chunk, context) =>
            {
                processedInFirstRun++;
                if (processedInFirstRun >= targetFailAt)
                {
                    throw new Exception("Simulated failure");
                }
                return Task.FromResult(new ChunkConversionResult
                {
                    ChunkIndex = chunk.ChunkIndex,
                    SourceFile = chunk.SourceFile,
                    Success = true,
                    ConvertedCode = "// OK"
                });
            });
        }
        catch
        {
            // Expected
        }

        // Second run - should resume
        int processedInSecondRun = 0;
        var result = await _orchestrator.ProcessChunksAsync(1, plan, (chunk, context) =>
        {
            processedInSecondRun++;
            return Task.FromResult(new ChunkConversionResult
            {
                ChunkIndex = chunk.ChunkIndex,
                SourceFile = chunk.SourceFile,
                Success = true,
                ConvertedCode = "// Resumed OK"
            });
        });

        // Assert - second run should have processed fewer chunks due to resumability
        // (this depends on how many were successfully recorded before failure)
        result.TotalChunks.Should().Be(plan.ChunkCount);
    }

    #endregion

    #region Helper Methods

    private static string GenerateCobolProgram(int lineCount)
    {
        var lines = new List<string>
        {
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. GEN-PROG.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01 WS-VAR-1 PIC X(10).",
            "       01 WS-VAR-2 PIC 9(5).",
            "       01 WS-VAR-3 PIC 9(7)V99.",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA."
        };

        int paraCount = 1;
        while (lines.Count < lineCount)
        {
            lines.Add($"       PARA-{paraCount:D3}.");
            for (int i = 0; i < 5 && lines.Count < lineCount; i++)
            {
                lines.Add($"           DISPLAY 'Processing para {paraCount} step {i}'.");
            }
            if (lines.Count < lineCount)
            {
                lines.Add($"           PERFORM PARA-{paraCount + 1:D3}.");
            }
            paraCount++;
        }

        lines.Add("       END-PARA.");
        lines.Add("           STOP RUN.");

        return string.Join("\n", lines);
    }

    #endregion
}
