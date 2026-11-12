using System.Reflection;
using System.Text.Json;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using CobolToQuarkusMigration.Agents.Interfaces;
using CobolToQuarkusMigration.Chunking;
using CobolToQuarkusMigration.Chunking.Interfaces;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Persistence;
using CobolToQuarkusMigration.Processes;
using CobolToQuarkusMigration.Tests.Helpers;
using FluentAssertions;
using Microsoft.Data.Sqlite;
using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging.Abstractions;
using Moq;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Processes;

[Collection("EnvironmentSensitive")]
public sealed class ChunkedMigrationAcceptanceTests
{
    [Theory]
    [InlineData(false, false, "missing")]
    [InlineData(false, true, "missing")]
    [InlineData(true, false, "missing")]
    [InlineData(true, true, "missing")]
    [InlineData(false, true, "source")]
    [InlineData(true, true, "source")]
    [InlineData(false, false, "definition")]
    [InlineData(true, false, "definition")]
    [InlineData(false, false, "stub")]
    [InlineData(true, false, "stub")]
    [InlineData(false, true, "incomplete")]
    [InlineData(true, true, "incomplete")]
    [InlineData(false, false, "draft")]
    [InlineData(true, false, "draft")]
    public async Task RunAsync_RejectsDependencyEvidenceBeforeAnyModelAndPersistsFailure(
        bool chunked, bool reuse, string damage)
    {
        var root = Path.GetFullPath(Path.Combine(".test-artifacts", "evidence-run-" + Guid.NewGuid()));
        var oldRoot = Environment.GetEnvironmentVariable("REPO_ROOT");
        var oldRunId = MetricsSink.CurrentRunId;
        var oldProfile = PromptLoader.CodebaseProfile;
        var source = Path.Combine(root, "source");
        Directory.CreateDirectory(source);
        var sourcePath = Path.Combine(source, "MAIN.cbl");
        await File.WriteAllTextAsync(sourcePath,
            "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. MAIN.\n       COPY MISSING.\n");
        if (damage is not ("missing" or "draft"))
        {
            var bookPath = Path.Combine(root, "MISSING.cpy");
            await File.WriteAllTextAsync(bookPath, "       01 GENUINE-TEST-RECORD PIC X.");
            var evidence = DependencyEvidenceFixture.Write(root, source, "MAIN.cbl", new DependencyDefinition
            {
                Kind = "COPY", Name = "MISSING.cpy", Path = bookPath, Sha256 = DependencyEvidenceFixture.Hash(bookPath)
            });
            if (damage == "source") await File.AppendAllTextAsync(sourcePath, "\n       01 NEW-INPUT PIC X.");
            else if (damage == "definition") await File.AppendAllTextAsync(bookPath, "\n       01 NEW-FIELD PIC X.");
            else if (damage == "incomplete")
            {
                evidence.Coverage["COPY"] = "partial";
                DependencyEvidenceFixture.Save(root, evidence);
            }
            else
            {
                await File.WriteAllTextAsync(bookPath, "      * AUTO-GENERATED STUB COPYBOOK\n       01 MISSING-STUB PIC X.");
                DependencyEvidenceFixture.Save(root, evidence with
                {
                    Definitions = [evidence.Definitions[0] with { Sha256 = DependencyEvidenceFixture.Hash(bookPath) }]
                });
            }
        }
        try
        {
            Environment.SetEnvironmentVariable("REPO_ROOT", root);
            var database = Path.Combine(root, "migration.db");
            var repository = new SqliteMigrationRepository(database, NullLogger<SqliteMigrationRepository>.Instance);
            await repository.InitializeAsync();
            var settings = new AppSettings
            {
                ApplicationSettings = new ApplicationSettings { MigrationDatabasePath = database },
                ConversionSettings = new ConversionSettings { DependencyMode = damage == "draft" ? "draft" : "strict" },
                JclJobs = new JclJobsSettings { Enabled = false }
            };
            var execution = new MigrationExecutionOptions { RepositoryRoot = root, LogDirectory = Path.Combine(root, "logs") };
            var client = new Mock<IChatClient>(MockBehavior.Strict);
            var helper = new FileHelper(NullLogger<FileHelper>.Instance);
            var output = Path.Combine(root, "generated");
            Task Run()
            {
                if (chunked)
                {
                    var process = new ChunkedMigrationProcess(client.Object, null,
                        NullLogger<ChunkedMigrationProcess>.Instance, helper, settings, repository, execution);
                    process.InitializeAgents();
                    if (reuse) process.SetDependencyMap(new DependencyMap());
                    return process.RunAsync(source, output);
                }
                var direct = new MigrationProcess(null, client.Object,
                    NullLogger<MigrationProcess>.Instance, helper, settings, repository, execution);
                direct.InitializeAgents();
                if (reuse) direct.SetDependencyMap(new DependencyMap());
                return direct.RunAsync(source, output);
            }

            await ((Func<Task>)Run).Should().ThrowAsync<InvalidDataException>()
                .WithMessage(damage == "draft" ? "*No independently justified whole programs*"
                    : "*Required REKT dependency evidence rejected before model conversion:*");
            if (damage == "draft")
                File.Exists(Path.Combine(output, "draft-conversion-plan.json")).Should().BeTrue();
            client.VerifyNoOtherCalls();
            var run = await repository.GetLatestRunAsync();
            run!.Status.Should().Be("Failed");
            run.CompletedAt.Should().NotBeNull();
            var finished = File.ReadAllLines(Path.Combine(root, "output", ".metrics", $"{run.RunId}.jsonl"))
                .Select(line => JsonDocument.Parse(line).RootElement.Clone())
                .Single(e => e.GetProperty("event").GetString() == "run_finished");
            finished.GetProperty("status").GetString().Should().Be("failed");
            if (Directory.Exists(output))
                Directory.GetFiles(output, "*", SearchOption.AllDirectories)
                    .Should().NotContain(path => path.EndsWith(".cs") || path.EndsWith(".java"));
        }
        finally
        {
            Environment.SetEnvironmentVariable("REPO_ROOT", oldRoot);
            MetricsSink.CurrentRunId = oldRunId;
            PromptLoader.CodebaseProfile = oldProfile;
            SqliteConnection.ClearAllPools();
            Directory.Delete(root, recursive: true);
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public async Task RunAsync_CancellationPersistsTheSameTerminalStatusAsTelemetry(bool chunked)
    {
        var root = Path.GetFullPath(Path.Combine(".test-artifacts", "cancelled-run-" + Guid.NewGuid()));
        var oldRoot = Environment.GetEnvironmentVariable("REPO_ROOT");
        var oldRunId = MetricsSink.CurrentRunId;
        Directory.CreateDirectory(root);
        try
        {
            Environment.SetEnvironmentVariable("REPO_ROOT", root);
            var database = Path.Combine(root, "migration.db");
            var repository = new SqliteMigrationRepository(database, NullLogger<SqliteMigrationRepository>.Instance);
            await repository.InitializeAsync();
            var settings = new AppSettings
            {
                ApplicationSettings = new ApplicationSettings { MigrationDatabasePath = database },
                JclJobs = new JclJobsSettings { Enabled = false }
            };
            var execution = new MigrationExecutionOptions { RepositoryRoot = root, LogDirectory = Path.Combine(root, "logs") };
            var client = new Mock<IChatClient>(MockBehavior.Strict).Object;
            var helper = new FileHelper(NullLogger<FileHelper>.Instance);
            using var cancellation = new CancellationTokenSource();
            cancellation.Cancel();
            Task Run()
            {
                if (chunked)
                {
                    var process = new ChunkedMigrationProcess(client, null,
                        NullLogger<ChunkedMigrationProcess>.Instance, helper, settings, repository, execution);
                    process.InitializeAgents();
                    return process.RunAsync(root, Path.Combine(root, "generated"),
                        (_, _, _, _) => cancellation.Token.ThrowIfCancellationRequested(),
                        cancellationToken: cancellation.Token);
                }
                var direct = new MigrationProcess(null, client,
                    NullLogger<MigrationProcess>.Instance, helper, settings, repository, execution);
                direct.InitializeAgents();
                return direct.RunAsync(root, Path.Combine(root, "generated"),
                    (_, _, _) => cancellation.Token.ThrowIfCancellationRequested(),
                    cancellationToken: cancellation.Token);
            }

            await ((Func<Task>)Run).Should().ThrowAsync<OperationCanceledException>();

            var run = await repository.GetLatestRunAsync();
            run!.Status.Should().Be("Cancelled");
            run.CompletedAt.Should().NotBeNull();
            var events = File.ReadAllLines(Path.Combine(root, "output", ".metrics", $"{run.RunId}.jsonl"))
                .Select(line => JsonDocument.Parse(line).RootElement.Clone()).ToList();
            events.Where(e => e.GetProperty("event").GetString() == "run_finished").Should().ContainSingle()
                .Which.GetProperty("status").GetString().Should().Be("cancelled");
            Directory.Exists(Path.Combine(root, "generated")).Should().BeFalse();
        }
        finally
        {
            Environment.SetEnvironmentVariable("REPO_ROOT", oldRoot);
            MetricsSink.CurrentRunId = oldRunId;
            SqliteConnection.ClearAllPools();
            Directory.Delete(root, recursive: true);
        }
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public async Task RunAsync_PersistsFailureAndThrowsForFailedMiddleOrConflictingSuccessfulChunks(bool failedMiddle)
    {
        var root = Path.GetFullPath(Path.Combine(".test-artifacts", "assembly-run-" + Guid.NewGuid()));
        var oldRoot = Environment.GetEnvironmentVariable("REPO_ROOT");
        var oldProfile = PromptLoader.CodebaseProfile;
        var oldRunId = MetricsSink.CurrentRunId;
        Directory.CreateDirectory(root);
        try
        {
            Environment.SetEnvironmentVariable("REPO_ROOT", root);
            var source = Path.Combine(root, "source");
            var output = Path.Combine(root, "converted");
            Directory.CreateDirectory(source);
            Directory.CreateDirectory(output);
            await File.WriteAllTextAsync(Path.Combine(source, "ACCOUNTS.cbl"),
                "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. ACCOUNTS.\n" +
                "       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n       01 TOTAL PIC 9(9).\n" +
                "       PROCEDURE DIVISION.\n" +
                string.Join("\n", Enumerable.Range(1, 50).Select(i => $"       PART-{i:D3}.\n           ADD 1 TO TOTAL.")) +
                "\n           STOP RUN.\n");
            DependencyEvidenceFixture.Write(root, source, "ACCOUNTS.cbl");

            var database = Path.Combine(root, "migration.db");
            var repository = new SqliteMigrationRepository(database, NullLogger<SqliteMigrationRepository>.Instance);
            await repository.InitializeAsync();
            var settings = new AppSettings
            {
                ApplicationSettings = new ApplicationSettings { TargetLanguage = TargetLanguage.Java, MigrationDatabasePath = database },
                ChunkingSettings = new ChunkingSettings
                {
                    MaxLinesPerChunk = 12,
                    MaxTokensPerChunk = 500,
                    MinSemanticUnitSize = 1,
                    OverlapLines = 0,
                    EnableParallelProcessing = false,
                    EnableResumability = true,
                    EnableProgressiveCompression = false
                },
                ConversionSettings = new ConversionSettings { EnableReconciliationPass = false },
                JclJobs = new JclJobsSettings { Enabled = false }
            };
            var received = new List<(int Index, int Total, string Code)>();
            var converter = new Mock<IChunkAwareConverter>(MockBehavior.Strict);
            converter.Setup(c => c.SetRunId(It.IsAny<int>()));
            converter.Setup(c => c.ConvertChunkAsync(It.IsAny<ChunkResult>(), It.IsAny<ChunkContext>(), It.IsAny<CancellationToken>()))
                .Returns((ChunkResult chunk, ChunkContext context, CancellationToken _) =>
                {
                    var code = chunk.ChunkIndex == 0 ? "package bank; public final class Account {"
                        : chunk.ChunkIndex == context.TotalChunks - 1 ? "}"
                        : chunk.ChunkIndex == 1 && failedMiddle ? "provider partial response without sentinel"
                        : !failedMiddle && chunk.ChunkIndex is 1 or 2 ? $"int value() {{ return {chunk.ChunkIndex}; }}"
                        : $"int field{chunk.ChunkIndex};";
                    received.Add((chunk.ChunkIndex, context.TotalChunks, code));
                    return Task.FromResult(new ChunkConversionResult
                    {
                        ChunkIndex = chunk.ChunkIndex,
                        SourceFile = chunk.SourceFile,
                        Success = !(failedMiddle && chunk.ChunkIndex == 1),
                        ConvertedCode = code,
                        ErrorMessage = failedMiddle && chunk.ChunkIndex == 1 ? "Middle response refused" : null
                    });
                });

            var process = new ChunkedMigrationProcess(new Mock<IChatClient>(MockBehavior.Strict).Object, null,
                NullLogger<ChunkedMigrationProcess>.Instance,
                new FileHelper(NullLogger<FileHelper>.Instance), settings, repository,
                chatLogger: new ChatLogger(NullLogger<ChatLogger>.Instance, Path.Combine(root, "logs")));
            var dependencyMap = new DependencyMap
            {
                MermaidDiagram = "legacy model diagram",
                CopybookUsage = new() { ["ACCOUNTS.cbl"] = [] }
            };
            var dependencyMapper = new Mock<IDependencyMapperAgent>(MockBehavior.Strict);
            dependencyMapper.Setup(m => m.GenerateMermaidDiagramAsync(dependencyMap, false))
                .ReturnsAsync("graph TB");
            process.SetDependencyMap(dependencyMap);
            SetField(process, "_dependencyMapperAgent", dependencyMapper.Object);
            SetField(process, "_chunkAwareConverter", converter.Object);
            SetField(process, "_chunkingOrchestrator", new ChunkingOrchestrator(settings.ChunkingSettings,
                settings.ConversionSettings, database, NullLogger<ChunkingOrchestrator>.Instance));

            var run = () => process.RunAsync(source, output);
            await run.Should().ThrowAsync<InvalidDataException>().WithMessage("*Chunk assembly incomplete*");

            dependencyMapper.Verify(m => m.GenerateMermaidDiagramAsync(dependencyMap, false), Times.Once);
            dependencyMap.MermaidDiagram.Should().Be("graph TB");
            received.Count.Should().BeGreaterThan(3);
            received.Select(r => r.Index).Should().Equal(Enumerable.Range(0, received.Count));
            received.Select(r => r.Total).Should().OnlyContain(total => total == received.Count);
            (await repository.GetLatestRunAsync())!.Status.Should().Be("Failed");
            Directory.GetFiles(output, "*.java", SearchOption.AllDirectories).Should().BeEmpty();
            Directory.GetFiles(output, "*.cs", SearchOption.AllDirectories).Should().BeEmpty();
            var diagnostics = Directory.GetFiles(output, "*.rejected.txt", SearchOption.AllDirectories);
            diagnostics.Should().HaveCount(received.Count);
            (await Task.WhenAll(diagnostics.Select(path => File.ReadAllTextAsync(path))))
                .Should().BeEquivalentTo(received.Select(r => r.Code));
            var report = await File.ReadAllTextAsync(Path.Combine(output, "chunked-migration-report.md"));
            report.Should().Contain("Assembly rejected").And.NotContain("| ✅ |");
            var errors = await File.ReadAllTextAsync(Directory.GetFiles(output, "assembly-errors.txt", SearchOption.AllDirectories).Single());
            errors.Should().Contain(Path.Combine(source, "ACCOUNTS.cbl"))
                .And.Contain(failedMiddle ? "Middle response refused" : "conflicting member");
        }
        finally
        {
            Environment.SetEnvironmentVariable("REPO_ROOT", oldRoot);
            PromptLoader.CodebaseProfile = oldProfile;
            MetricsSink.CurrentRunId = oldRunId;
            SqliteConnection.ClearAllPools();
            Directory.Delete(root, recursive: true);
        }
    }

    private static void SetField(ChunkedMigrationProcess process, string name, object value) =>
        typeof(ChunkedMigrationProcess).GetField(name, BindingFlags.Instance | BindingFlags.NonPublic)!.SetValue(process, value);
}
