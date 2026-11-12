using System.Text.Json;
using CobolToQuarkusMigration.Agents;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Persistence;
using CobolToQuarkusMigration.Processes;
using CobolToQuarkusMigration.Tests.Helpers;
using FluentAssertions;
using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging.Abstractions;
using Xunit;
using ChatMessage = Microsoft.Extensions.AI.ChatMessage;

namespace CobolToQuarkusMigration.Tests.Processes;

[Collection("EnvironmentSensitive")]
public sealed class JavaMigrationValidationTests : IDisposable
{
    private readonly string _root = Path.GetFullPath(Path.Combine(".test-artifacts", $"java-process-{Guid.NewGuid():N}"));
    private readonly int _originalExit = Environment.ExitCode;
    private readonly string? _originalRoot = Environment.GetEnvironmentVariable("REPO_ROOT");
    private readonly string? _originalGate = Environment.GetEnvironmentVariable("ON_LOW_SCORE");
    private readonly string? _originalRounds = Environment.GetEnvironmentVariable("PARITY_REPAIR_MAX_ROUNDS");
    private readonly string? _originalRektContext = Environment.GetEnvironmentVariable("ENABLE_REKT_CONTEXT");
    private readonly string? _originalProfile = PromptLoader.CodebaseProfile;

    [Theory]
    [InlineData(false, false)]
    [InlineData(true, false)]
    [InlineData(false, true)]
    [InlineData(true, true)]
    public async Task Run_MavenValidationAndRepairAreWiredOnBothMigrationEntrypoints(bool chunked, bool repair)
    {
        var sourceRoot = Path.Combine(_root, "source");
        var output = Path.Combine(_root, "generated");
        Directory.CreateDirectory(sourceRoot);
        Directory.CreateDirectory(output);
        await File.WriteAllTextAsync(Path.Combine(sourceRoot, "SAMPLE.cbl"),
            "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. SAMPLE.\n" +
            "       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n" +
            "       01 SAMPLE.\n          05 NEEDED-VALUE PIC 9 VALUE 1.\n" +
            "       PROCEDURE DIVISION.\n           EXEC CICS RETURN END-EXEC.\n");
        var evidence = DependencyEvidenceFixture.Write(_root, sourceRoot, "SAMPLE.cbl");
        DependencyEvidenceFixture.BindExport(_root, evidence, JsonSerializer.Serialize(new
        {
            callTarget = new { name = "SAMPLE.cbl" },
            dependencies = Array.Empty<object>(),
            dependencyDialectStatements = new[] { DependencyEvidenceFixture.DialectStatement("EXEC_CICS", "RETURN") }
        }));
        var dependencyMap = new DependencyEvidenceGate(Path.Combine(_root, "output", "rekt"))
            .RequireVerified([new CobolFile { FileName = "SAMPLE.cbl", FilePath = Path.Combine(sourceRoot, "SAMPLE.cbl") }], sourceRoot);
        var executable = Path.Combine(_root, OperatingSystem.IsWindows() ? "maven.cmd" : "maven");
        await File.WriteAllTextAsync(executable, OperatingSystem.IsWindows()
            ? repair
                ? "@echo off\r\nfindstr /S /M broken *.java >nul\r\nif errorlevel 1 exit /b 0\r\n" +
                  "for /r %%f in (Sample.java) do if exist \"%%f\" echo [ERROR] %%f:[1,1] cannot find symbol: broken\r\nexit /b 1\r\n"
                : "@echo off\r\necho Could not resolve dependencies\r\nexit /b 9\r\n"
            : repair
                ? "#!/bin/sh\nif ! grep -R -q --include='*.java' broken .; then exit 0; fi\n" +
                  "find . -name Sample.java -exec printf '[ERROR] %s:[1,1] cannot find symbol: broken\\n' '{}' \\;\nexit 1\n"
                : "#!/bin/sh\nprintf 'Could not resolve dependencies\\n'\nexit 9\n");
        if (!OperatingSystem.IsWindows())
            File.SetUnixFileMode(executable, UnixFileMode.UserRead | UnixFileMode.UserWrite | UnixFileMode.UserExecute);
        Environment.SetEnvironmentVariable("REPO_ROOT", _root);
        Environment.SetEnvironmentVariable("ON_LOW_SCORE", "warn");
        Environment.SetEnvironmentVariable("PARITY_REPAIR_MAX_ROUNDS", "0");
        if (repair) Environment.SetEnvironmentVariable("ENABLE_REKT_CONTEXT", "false");
        Environment.ExitCode = 0;
        var database = Path.Combine(_root, "migration.db");
        var repository = new SqliteMigrationRepository(database, NullLogger<SqliteMigrationRepository>.Instance);
        await repository.InitializeAsync();
        var settings = new AppSettings
        {
            ApplicationSettings = new ApplicationSettings
            {
                TargetLanguage = TargetLanguage.Java, MigrationDatabasePath = database,
                CobolSourceFolder = sourceRoot, JavaOutputFolder = output
            },
            ConversionSettings = new ConversionSettings { JavaFramework = "Spring" },
            CompileGate = new CompileGateSettings { Enabled = true, MaxRepairRounds = repair ? 1 : 0 },
            JclJobs = new JclJobsSettings { Enabled = false }
        };
        var execution = new MigrationExecutionOptions
        {
            RepositoryRoot = _root, LogDirectory = Path.Combine(_root, "logs"), MavenExecutable = executable
        };
        using var client = new ControlledClient(repair);
        var helper = new FileHelper(NullLogger<FileHelper>.Instance);
        if (chunked)
        {
            var process = new ChunkedMigrationProcess(client, null, NullLogger<ChunkedMigrationProcess>.Instance,
                helper, settings, repository, execution);
            process.InitializeAgents();
            process.SetDependencyMap(dependencyMap);
            await process.RunAsync(sourceRoot, output);
        }
        else
        {
            var process = new MigrationProcess(null, client, NullLogger<MigrationProcess>.Instance,
                helper, settings, repository, execution);
            process.InitializeAgents();
            process.SetDependencyMap(dependencyMap);
            await process.RunAsync(sourceRoot, output);
        }
        var run = await repository.GetRunAsync(1);
        run!.Status.Should().Be(repair ? "Completed" : "Failed");
        if (!repair) run.Notes.Should().Contain("failed validation");
        var events = File.ReadAllLines(Path.Combine(_root, "output", ".metrics", "1.jsonl"))
            .Select(line => JsonDocument.Parse(line).RootElement.Clone()).ToList();
        events.Where(e => e.TryGetProperty("event", out var name) && name.GetString() == AiLoopEvents.LlmCall)
            .Should().OnlyContain(e => e.GetProperty("success").GetBoolean());
        var validation = events.Single(e => e.TryGetProperty("event", out var name) && name.GetString() == "validation_completed");
        validation.GetProperty("accepted").GetBoolean().Should().Be(repair);
        validation.GetProperty("compileMeasured").GetBoolean().Should().BeTrue();
        validation.GetProperty("compiled").GetBoolean().Should().Be(repair);
        if (!repair)
            events.Single(e => e.TryGetProperty("event", out var name) && name.GetString() == AiLoopEvents.RunFinished)
                .GetProperty("errorType").GetString().Should().Be("ValidationFailed");
        client.Conversions.Should().Be(1);
        client.Repairs.Should().Be(repair ? 1 : 0);
        var status = JsonSerializer.Deserialize<JavaCompileResult>(
            await File.ReadAllTextAsync(Path.Combine(output, "java-compile-status.json")))!;
        status.Compiled.Should().Be(repair);
        status.Measured.Should().BeTrue();
        status.ExitCode.Should().Be(repair ? 0 : 9);
        status.RepairRounds.Should().Be(repair ? 1 : 0);
        File.Exists(Path.Combine(output, "pom.xml")).Should().BeTrue();
        using var runtime = JsonDocument.Parse(await File.ReadAllTextAsync(
            Path.Combine(output, "runtime-implementation-requirements.json")));
        runtime.RootElement.GetProperty("behaviorVerified").GetBoolean().Should().BeFalse();
        runtime.RootElement.GetProperty("requirements").EnumerateArray().Should().ContainSingle();
        using var map = JsonDocument.Parse(await File.ReadAllTextAsync(Path.Combine(output, "generated-source-map.json")));
        map.RootElement.EnumerateArray().Should().ContainSingle()
            .Which.GetProperty("source").GetString().Should().Be("SAMPLE.cbl");
        Environment.ExitCode.Should().Be(repair ? 0 : JavaCompileGate.FailureExitCode);
    }

    private sealed class ControlledClient(bool repair) : IChatClient
    {
        public int Conversions { get; private set; }
        public int Repairs { get; private set; }

        public Task<ChatResponse> GetResponseAsync(IEnumerable<ChatMessage> messages, ChatOptions? options = null,
            CancellationToken cancellationToken = default)
        {
            var prompts = messages.ToList();
            if (prompts.First().Text.StartsWith("Repair converted code", StringComparison.Ordinal))
            {
                prompts.First().Text.Should().Contain("Java 17 with Spring");
                var request = JsonSerializer.Deserialize<ParityRepairRequest>(prompts.Last().Text)!;
                request.Language.Should().Be("Java");
                request.CompilerDiagnostics.Should().ContainSingle();
                request.RelatedFiles!.Keys.Should().Contain(path => Path.GetFileName(path) == "pom.xml");
                request.RuntimeRequirements.Should().ContainSingle().Which.Operation.Should().Be("RETURN");
                Repairs++;
                return Task.FromResult(new ChatResponse(new ChatMessage(ChatRole.Assistant,
                    JsonSerializer.Serialize(request.Files.ToDictionary(pair => pair.Key,
                        pair => pair.Value.Replace("broken", "1", StringComparison.Ordinal)))))
                    { FinishReason = ChatFinishReason.Stop });
            }
            var analyze = messages.First().Text.StartsWith("Analyze", StringComparison.Ordinal);
            if (!analyze)
            {
                Conversions++;
                string.Join("\n", prompts.Select(prompt => prompt.Text)).Should()
                    .Contain("Source-backed runtime implementation requirements").And.Contain("must IMPLEMENT");
            }
            var content = analyze ? "A record with NEEDED-VALUE initialized to 1." :
                $"```java\npackage com.sample;\npublic class Sample {{ int neededValue = {(repair ? "broken" : "1")}; }}\n```";
            return Task.FromResult(new ChatResponse(new ChatMessage(ChatRole.Assistant, content))
                { FinishReason = ChatFinishReason.Stop });
        }

        public IAsyncEnumerable<ChatResponseUpdate> GetStreamingResponseAsync(IEnumerable<ChatMessage> messages,
            ChatOptions? options = null, CancellationToken cancellationToken = default) => throw new NotSupportedException();
        public object? GetService(Type serviceType, object? serviceKey = null) => null;
        public void Dispose() { }
    }

    public void Dispose()
    {
        Environment.ExitCode = _originalExit;
        Environment.SetEnvironmentVariable("REPO_ROOT", _originalRoot);
        Environment.SetEnvironmentVariable("ON_LOW_SCORE", _originalGate);
        Environment.SetEnvironmentVariable("PARITY_REPAIR_MAX_ROUNDS", _originalRounds);
        Environment.SetEnvironmentVariable("ENABLE_REKT_CONTEXT", _originalRektContext);
        PromptLoader.CodebaseProfile = _originalProfile;
        if (Directory.Exists(_root)) Directory.Delete(_root, true);
    }
}
