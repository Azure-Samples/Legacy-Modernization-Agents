using Xunit;
using FluentAssertions;
using CobolToQuarkusMigration.Chunking.Context;
using CobolToQuarkusMigration.Chunking.Interfaces;
using CobolToQuarkusMigration.Models;
using Microsoft.Extensions.Logging;
using Moq;
using Microsoft.Data.Sqlite;

namespace CobolToQuarkusMigration.Tests.Chunking;

public class ChunkContextManagerTests : IDisposable
{
    private readonly string _databasePath;
    private readonly ChunkingSettings _settings;
    private readonly Mock<ILogger<ChunkContextManager>> _loggerMock;
    private readonly Mock<ISignatureRegistry> _signatureRegistryMock;
    private readonly Mock<ITypeMappingTable> _typeMappingTableMock;
    private readonly ChunkContextManager _contextManager;

    public ChunkContextManagerTests()
    {
        _databasePath = Path.Combine(Path.GetTempPath(), $"test_chunking_{Guid.NewGuid()}.db");
        _settings = new ChunkingSettings
        {
            EnableProgressiveCompression = true,
            CompressionRatio = 0.3,
            FullDetailChunkWindow = 3,
            MaxTokensPerChunk = 4000
        };
        _loggerMock = new Mock<ILogger<ChunkContextManager>>();
        _signatureRegistryMock = new Mock<ISignatureRegistry>();
        _typeMappingTableMock = new Mock<ITypeMappingTable>();

        // Initialize test database
        InitializeTestDatabase();

        _contextManager = new ChunkContextManager(
            _databasePath,
            _signatureRegistryMock.Object,
            _typeMappingTableMock.Object,
            _settings,
            _loggerMock.Object);
    }

    public void Dispose()
    {
        try
        {
            if (File.Exists(_databasePath))
            {
                File.Delete(_databasePath);
            }
        }
        catch
        {
            // Ignore cleanup errors in tests
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
                UNIQUE(run_id, source_file, legacy_variable)
            );
        ";
        command.ExecuteNonQuery();
    }

    [Fact]
    public async Task BuildContextForChunk_FirstChunk_ReturnsEmptyPreviousContext()
    {
        // Arrange
        var chunk = new ChunkResult
        {
            ChunkIndex = 0,
            TotalChunks = 5,
            SourceFile = "test.cbl",
            StartLine = 1,
            EndLine = 100,
            SemanticUnitNames = new List<string> { "MAIN-PARAGRAPH" },
            OutboundDependencies = new List<string>()
        };

        _signatureRegistryMock
            .Setup(x => x.GetAllSignaturesAsync(It.IsAny<int>(), It.IsAny<string>(), It.IsAny<CancellationToken>()))
            .ReturnsAsync(new List<MethodSignature>());

        _typeMappingTableMock
            .Setup(x => x.GetAllMappingsAsync(It.IsAny<int>(), It.IsAny<string>(), It.IsAny<CancellationToken>()))
            .ReturnsAsync(new List<TypeMapping>());

        // Act
        var context = await _contextManager.BuildContextForChunkAsync(1, chunk);

        // Assert
        context.CurrentChunkIndex.Should().Be(0);
        context.TotalChunks.Should().Be(5);
        context.PreviousSignatures.Should().BeEmpty();
        context.TypeMappings.Should().BeEmpty();
        context.FileSummary.Should().Contain("chunk 1 of 5");
    }

    [Fact]
    public async Task BuildContextForChunk_WithPreviousSignatures_IncludesSignatures()
    {
        // Arrange
        var chunk = new ChunkResult
        {
            ChunkIndex = 2,
            TotalChunks = 5,
            SourceFile = "test.cbl",
            StartLine = 201,
            EndLine = 300,
            SemanticUnitNames = new List<string> { "PROCESS-DATA" },
            OutboundDependencies = new List<string> { "VALIDATE-INPUT" }
        };

        var previousSignatures = new List<MethodSignature>
        {
            new() { LegacyName = "VALIDATE-INPUT", TargetMethodName = "ValidateInput", TargetSignature = "bool ValidateInput(string input)", ReturnType = "bool" }
        };

        _signatureRegistryMock
            .Setup(x => x.GetAllSignaturesAsync(It.IsAny<int>(), "test.cbl", It.IsAny<CancellationToken>()))
            .ReturnsAsync(previousSignatures);

        _typeMappingTableMock
            .Setup(x => x.GetAllMappingsAsync(It.IsAny<int>(), "test.cbl", It.IsAny<CancellationToken>()))
            .ReturnsAsync(new List<TypeMapping>());

        // Act
        var context = await _contextManager.BuildContextForChunkAsync(1, chunk);

        // Assert
        context.PreviousSignatures.Should().HaveCount(1);
        context.PreviousSignatures[0].LegacyName.Should().Be("VALIDATE-INPUT");
        context.UnconvertedDependencies.Should().BeEmpty(); // Already converted
    }

    [Fact]
    public async Task BuildContextForChunk_WithUnconvertedDependency_MarksAsUnconverted()
    {
        // Arrange
        var chunk = new ChunkResult
        {
            ChunkIndex = 1,
            TotalChunks = 5,
            SourceFile = "test.cbl",
            StartLine = 101,
            EndLine = 200,
            SemanticUnitNames = new List<string> { "PROCESS-DATA" },
            OutboundDependencies = new List<string> { "FUTURE-METHOD" }
        };

        _signatureRegistryMock
            .Setup(x => x.GetAllSignaturesAsync(It.IsAny<int>(), "test.cbl", It.IsAny<CancellationToken>()))
            .ReturnsAsync(new List<MethodSignature>()); // No previous signatures

        _typeMappingTableMock
            .Setup(x => x.GetAllMappingsAsync(It.IsAny<int>(), "test.cbl", It.IsAny<CancellationToken>()))
            .ReturnsAsync(new List<TypeMapping>());

        // Act
        var context = await _contextManager.BuildContextForChunkAsync(1, chunk);

        // Assert
        context.UnconvertedDependencies.Should().Contain("FUTURE-METHOD");
    }

    [Fact]
    public async Task RecordChunkResult_Success_StoresInDatabase()
    {
        // Arrange
        var chunk = new ChunkResult
        {
            ChunkIndex = 0,
            TotalChunks = 1,
            SourceFile = "test.cbl",
            StartLine = 1,
            EndLine = 100,
            SemanticUnitNames = new List<string> { "MAIN-PARA" }
        };

        var result = new ChunkConversionResult
        {
            Success = true,
            ConvertedCode = "public class Test { }",
            TokensUsed = 500,
            ProcessingTimeMs = 1000,
            DefinedMethods = new List<MethodSignature>(),
            TypeMappings = new List<TypeMapping>(),
            ForwardReferences = new List<ForwardReference>()
        };

        // Act
        await _contextManager.RecordChunkResultAsync(1, chunk, result);

        // Assert
        using var connection = new SqliteConnection($"Data Source={_databasePath}");
        connection.Open();
        using var cmd = connection.CreateCommand();
        cmd.CommandText = "SELECT status, tokens_used FROM chunk_metadata WHERE run_id = 1 AND chunk_index = 0";
        using var reader = cmd.ExecuteReader();
        reader.Read().Should().BeTrue();
        reader.GetString(0).Should().Be("Completed");
        reader.GetInt32(1).Should().Be(500);
    }

    [Fact]
    public async Task RecordChunkResult_Failure_StoresErrorMessage()
    {
        // Arrange
        var chunk = new ChunkResult
        {
            ChunkIndex = 0,
            TotalChunks = 1,
            SourceFile = "test.cbl",
            StartLine = 1,
            EndLine = 100,
            SemanticUnitNames = new List<string> { "MAIN-PARA" }
        };

        var result = new ChunkConversionResult
        {
            Success = false,
            ErrorMessage = "API timeout",
            TokensUsed = 0,
            ProcessingTimeMs = 30000,
            DefinedMethods = new List<MethodSignature>(),
            TypeMappings = new List<TypeMapping>(),
            ForwardReferences = new List<ForwardReference>()
        };

        // Act
        await _contextManager.RecordChunkResultAsync(1, chunk, result);

        // Assert
        using var connection = new SqliteConnection($"Data Source={_databasePath}");
        connection.Open();
        using var cmd = connection.CreateCommand();
        cmd.CommandText = "SELECT status, error_message FROM chunk_metadata WHERE run_id = 1 AND chunk_index = 0";
        using var reader = cmd.ExecuteReader();
        reader.Read().Should().BeTrue();
        reader.GetString(0).Should().Be("Failed");
        reader.GetString(1).Should().Be("API timeout");
    }

    [Fact]
    public async Task GetLastCompletedChunkIndex_NoChunks_ReturnsMinusOne()
    {
        // Act
        var lastIndex = await _contextManager.GetLastCompletedChunkIndexAsync(1, "test.cbl");

        // Assert
        lastIndex.Should().Be(-1);
    }

    [Fact]
    public async Task GetLastCompletedChunkIndex_WithCompletedChunks_ReturnsLastIndex()
    {
        // Arrange - Insert some completed chunks
        using (var connection = new SqliteConnection($"Data Source={_databasePath}"))
        {
            connection.Open();
            using var cmd = connection.CreateCommand();
            cmd.CommandText = @"
                INSERT INTO chunk_metadata (run_id, source_file, chunk_index, start_line, end_line, status)
                VALUES (1, 'test.cbl', 0, 1, 100, 'Completed');
                INSERT INTO chunk_metadata (run_id, source_file, chunk_index, start_line, end_line, status)
                VALUES (1, 'test.cbl', 1, 101, 200, 'Completed');
                INSERT INTO chunk_metadata (run_id, source_file, chunk_index, start_line, end_line, status)
                VALUES (1, 'test.cbl', 2, 201, 300, 'Failed');
            ";
            cmd.ExecuteNonQuery();
        }

        // Act
        var lastIndex = await _contextManager.GetLastCompletedChunkIndexAsync(1, "test.cbl");

        // Assert
        lastIndex.Should().Be(1); // Chunk 2 failed, so last completed is 1
    }

    [Fact]
    public async Task ClearRunContext_RemovesAllData()
    {
        // Arrange - Insert some data
        using (var connection = new SqliteConnection($"Data Source={_databasePath}"))
        {
            connection.Open();
            using var cmd = connection.CreateCommand();
            cmd.CommandText = @"
                INSERT INTO chunk_metadata (run_id, source_file, chunk_index, start_line, end_line, status)
                VALUES (1, 'test.cbl', 0, 1, 100, 'Completed');
                INSERT INTO forward_references (run_id, source_file, caller_method, caller_chunk_index, target_method)
                VALUES (1, 'test.cbl', 'CALLER', 0, 'TARGET');
            ";
            cmd.ExecuteNonQuery();
        }

        // Act
        await _contextManager.ClearRunContextAsync(1);

        // Assert
        using (var connection = new SqliteConnection($"Data Source={_databasePath}"))
        {
            connection.Open();
            using var cmd = connection.CreateCommand();
            cmd.CommandText = "SELECT COUNT(*) FROM chunk_metadata WHERE run_id = 1";
            var count = Convert.ToInt32(cmd.ExecuteScalar());
            count.Should().Be(0);
        }
    }
}
