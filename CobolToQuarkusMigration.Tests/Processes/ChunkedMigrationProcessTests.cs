using CobolToQuarkusMigration.Chunking;
using CobolToQuarkusMigration.Chunking.Interfaces;
using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Processes;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Processes;

public class ChunkedMigrationProcessTests
{
    [Fact]
    public void CreateFailedChunkDiagnosticFiles_PersistsFailedStub()
    {
        var result = new ChunkedFileResult
        {
            SourceFile = "ACCOUNTS.cbl",
            Result = new ChunkingResult
            {
                ChunkResults =
                [
                    new ChunkProcessingResult
                    {
                        ChunkIndex = 1,
                        Success = false,
                        ConversionResult = new ChunkConversionResult
                        {
                            Success = false,
                            ConvertedCode = "// CHUNK CONVERSION DID NOT PRODUCE USABLE C#"
                        }
                    }
                ]
            }
        };

        var diagnostics = ChunkedMigrationProcess.CreateFailedChunkDiagnosticFiles(
            result,
            isJava: false);

        diagnostics.Should().ContainSingle();
        diagnostics[0].FileName.Should().Be("ACCOUNTS.chunk-2.failed.cs");
        diagnostics[0].OriginalCobolFileName.Should().Be("ACCOUNTS.cbl");
        diagnostics[0].Content.Should().Contain("CHUNK CONVERSION DID NOT PRODUCE USABLE");
    }

    [Fact]
    public void CreateFailedChunkDiagnosticFiles_IgnoresFailuresWithoutDiagnosticContent()
    {
        var result = new ChunkedFileResult
        {
            SourceFile = "ACCOUNTS.cbl",
            Result = new ChunkingResult
            {
                ChunkResults =
                [
                    new ChunkProcessingResult
                    {
                        ChunkIndex = 0,
                        Success = false,
                        ConversionResult = new ChunkConversionResult
                        {
                            Success = false,
                            ErrorMessage = "Provider unavailable"
                        }
                    }
                ]
            }
        };

        ChunkedMigrationProcess.CreateFailedChunkDiagnosticFiles(result, isJava: true)
            .Should().BeEmpty();
    }
}
