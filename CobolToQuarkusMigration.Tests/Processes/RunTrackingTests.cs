using System.Text.Json;
using CobolToQuarkusMigration.Agents;
using CobolToQuarkusMigration.Agents.Interfaces;
using CobolToQuarkusMigration.Chunking;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Persistence;
using CobolToQuarkusMigration.Processes;
using FluentAssertions;
using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging.Abstractions;
using Moq;
using Xunit;
using ChatMessage = Microsoft.Extensions.AI.ChatMessage;

namespace CobolToQuarkusMigration.Tests.Processes;

[Collection("EnvironmentSensitive")]
public sealed class RunTrackingTests : IDisposable
{
    private readonly string _root = Path.Combine(AppContext.BaseDirectory, ".run-tracking-" + Guid.NewGuid().ToString("N"));
    private readonly string? _previousRoot = Environment.GetEnvironmentVariable("REPO_ROOT");
    private readonly string? _previousProfile = PromptLoader.CodebaseProfile;
    private readonly RejectingClient _client = new();

    public RunTrackingTests()
    {
        Directory.CreateDirectory(Path.Combine(_root, "source"));
        Environment.SetEnvironmentVariable("REPO_ROOT", _root);
    }

    [Theory]
    [InlineData("standard")]
    [InlineData("chunked")]
    [InlineData("smart")]
    [InlineData("reverse")]
    [InlineData("chunked-reverse")]
    public async Task EmptyRunsHaveDurableTerminalStateAndNoModelCalls(string kind)
    {
        var repository = await Repository();
        await Run(kind, repository);

        var run = await repository.GetLatestRunAsync();
        run!.Status.Should().Be("NoFiles");
        AssertTerminal(run.RunId, "no_files");
        _client.Calls.Should().Be(0);
        MetricsSink.CurrentRunId.Should().BeNull();
    }

    [Theory]
    [InlineData("standard")]
    [InlineData("chunked")]
    [InlineData("smart")]
    [InlineData("reverse")]
    [InlineData("chunked-reverse")]
    public async Task PreCancelledRunsArePersistedAndNeverLeftRunning(string kind)
    {
        var repository = await Repository();
        using var cancellation = new CancellationTokenSource();
        cancellation.Cancel();

        await Assert.ThrowsAnyAsync<OperationCanceledException>(() => Run(kind, repository, cancellation.Token));

        var run = await repository.GetLatestRunAsync();
        run!.Status.Should().Be("Cancelled");
        AssertTerminal(run.RunId, "cancelled");
        _client.Calls.Should().Be(0);
    }

    [Theory]
    [InlineData("standard")]
    [InlineData("chunked")]
    [InlineData("reverse")]
    [InlineData("chunked-reverse")]
    [InlineData("smart")]
    public async Task EarlyFailuresAreTerminalAndDoNotPersistExceptionContent(string kind)
    {
        var repository = await Repository();

        await Assert.ThrowsAsync<InvalidOperationException>(() => Run(kind, repository,
            callback: (_, _, _) => throw new InvalidOperationException("private config secret")));

        var run = await repository.GetLatestRunAsync();
        run!.Status.Should().Be("Failed");
        run.Notes.Should().Be("InvalidOperationException");
        AssertTerminal(run.RunId, "failed");
        File.ReadAllText(MetricsPath(run.RunId)).Should().NotContain("private config secret");
    }

    [Theory]
    [InlineData("standard")]
    [InlineData("chunked")]
    public async Task UninitializedProcessesStillRecordTheirFailedRun(string kind)
    {
        var repository = await Repository();
        await Assert.ThrowsAsync<InvalidOperationException>(() => Run(kind, repository, initialize: false));
        var run = await repository.GetLatestRunAsync();
        run!.Status.Should().Be("Failed");
        AssertTerminal(run.RunId, "failed");
    }

    [Fact]
    public async Task ResumedRunsAppendDistinctAttemptsWithoutReplacingPriorEvents()
    {
        var repository = await Repository();
        await Run("standard", repository);
        var run = await repository.GetLatestRunAsync();
        var original = File.ReadAllText(MetricsPath(run!.RunId));

        await Run("standard", repository, existingRunId: run.RunId);

        (await repository.GetLatestRunAsync())!.RunId.Should().Be(run.RunId);
        File.ReadAllText(MetricsPath(run.RunId)).Should().StartWith(original);
        var events = File.ReadAllLines(MetricsPath(run.RunId))
            .Select(line => JsonDocument.Parse(line).RootElement.Clone()).ToList();
        events.Count(e => e.GetProperty("event").GetString() == AiLoopEvents.RunStarted).Should().Be(2);
        events.Count(e => e.GetProperty("event").GetString() == AiLoopEvents.RunFinished).Should().Be(2);
        events.Select(e => e.GetProperty("attemptId").GetString()).Distinct().Should().HaveCount(2);
    }

    [Fact]
    public async Task SmartDirectRoutingSharesOneAttemptAndPropagatesCancellation()
    {
        var repository = await Repository();
        await File.WriteAllTextAsync(Path.Combine(_root, "source", "A.cbl"),
            "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. A.\n       PROCEDURE DIVISION.\n           STOP RUN.");
        using var cancellation = new CancellationTokenSource();

        await Assert.ThrowsAnyAsync<OperationCanceledException>(() => Run("smart", repository, cancellation.Token,
            (status, _, _) =>
            {
                if (status.Contains("files directly", StringComparison.Ordinal)) cancellation.Cancel();
            }));

        var run = await repository.GetLatestRunAsync();
        run!.Status.Should().Be("Cancelled");
        AssertTerminal(run.RunId, "cancelled");
        _client.Calls.Should().Be(0);
    }

    private async Task<SqliteMigrationRepository> Repository()
    {
        var repository = new SqliteMigrationRepository(Path.Combine(_root, "migration.db"),
            NullLogger<SqliteMigrationRepository>.Instance);
        await repository.InitializeAsync();
        return repository;
    }

    private async Task Run(string kind, IMigrationRepository repository, CancellationToken cancellationToken = default,
        Action<string, int, int>? callback = null, bool initialize = true, int? existingRunId = null)
    {
        var source = Path.Combine(_root, "source");
        var output = Path.Combine(_root, "output", "generated");
        var helper = new FileHelper(NullLogger<FileHelper>.Instance);
        var settings = new AppSettings
        {
            ApplicationSettings = new ApplicationSettings { MigrationDatabasePath = Path.Combine(_root, "migration.db") }
        };
        var execution = new MigrationExecutionOptions { RepositoryRoot = _root, LogDirectory = Path.Combine(_root, "logs") };
        switch (kind)
        {
            case "standard":
                var standard = new MigrationProcess(null, _client, NullLogger<MigrationProcess>.Instance,
                    helper, settings, repository, execution);
                if (initialize) standard.InitializeAgents();
                await standard.RunAsync(source, output, callback, existingRunId, cancellationToken: cancellationToken);
                break;
            case "chunked":
                var chunked = new ChunkedMigrationProcess(_client, null, NullLogger<ChunkedMigrationProcess>.Instance,
                    helper, settings, repository, execution);
                if (initialize) chunked.InitializeAgents();
                await chunked.RunAsync(source, output, (status, current, total, _) => callback?.Invoke(status, current, total),
                    cancellationToken: cancellationToken);
                break;
            case "smart":
                var smart = new SmartMigrationOrchestrator(null, _client, NullLoggerFactory.Instance, helper, settings, repository);
                await smart.RunAsync(source, output, callback, cancellationToken: cancellationToken);
                break;
            default:
                var analyzer = Mock.Of<ICobolAnalyzerAgent>();
                var dependency = Mock.Of<IDependencyMapperAgent>();
                var business = new BusinessLogicExtractorAgent(_client, NullLogger<BusinessLogicExtractorAgent>.Instance, "fake-model");
                if (kind == "reverse")
                {
                    var logger = NullLogger<ReverseEngineeringProcess>.Instance;
                    await new ReverseEngineeringProcess(analyzer, business, dependency, helper, logger,
                        new EnhancedLogger(logger), repository).RunAsync(source, output, callback,
                        cancellationToken: cancellationToken);
                }
                else
                {
                    var logger = NullLogger<ChunkedReverseEngineeringProcess>.Instance;
                    var orchestrator = new ChunkingOrchestrator(settings.ChunkingSettings, settings.ConversionSettings,
                        settings.ApplicationSettings.MigrationDatabasePath, NullLogger<ChunkingOrchestrator>.Instance);
                    await new ChunkedReverseEngineeringProcess(analyzer, business, dependency, helper,
                        settings.ChunkingSettings, orchestrator, logger, new EnhancedLogger(logger),
                        settings.ApplicationSettings.MigrationDatabasePath, repository).RunAsync(source, output, callback,
                        cancellationToken: cancellationToken);
                }
                break;
        }
    }

    private string MetricsPath(int runId) => Path.Combine(_root, "output", ".metrics", $"{runId}.jsonl");

    private void AssertTerminal(int runId, string status)
    {
        var events = File.ReadAllLines(MetricsPath(runId)).Select(line =>
            JsonDocument.Parse(line).RootElement.Clone()).ToList();
        events.Count(e => e.GetProperty("event").GetString() == AiLoopEvents.RunStarted).Should().Be(1);
        events.Where(e => e.GetProperty("event").GetString() == AiLoopEvents.RunFinished)
            .Should().ContainSingle().Which.GetProperty("status").GetString().Should().Be(status);
        events.Select(e => e.GetProperty("attemptId").GetString()).Distinct().Should().ContainSingle();
    }

    private sealed class RejectingClient : IChatClient
    {
        public int Calls { get; private set; }
        public Task<ChatResponse> GetResponseAsync(IEnumerable<ChatMessage> messages, ChatOptions? options = null,
            CancellationToken cancellationToken = default)
        {
            Calls++;
            throw new InvalidOperationException("Tests must not call a model");
        }
        public IAsyncEnumerable<ChatResponseUpdate> GetStreamingResponseAsync(IEnumerable<ChatMessage> messages,
            ChatOptions? options = null, CancellationToken cancellationToken = default) => throw new NotSupportedException();
        public object? GetService(Type serviceType, object? serviceKey = null) => null;
        public void Dispose() { }
    }

    public void Dispose()
    {
        Environment.SetEnvironmentVariable("REPO_ROOT", _previousRoot);
        PromptLoader.CodebaseProfile = _previousProfile;
        if (Directory.Exists(_root)) Directory.Delete(_root, true);
    }
}
