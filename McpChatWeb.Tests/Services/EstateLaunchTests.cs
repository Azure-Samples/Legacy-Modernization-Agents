using System;
using System.IO;
using System.Linq;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using CobolToQuarkusMigration.Tests.Helpers;
using McpChatWeb.Endpoints;
using McpChatWeb.Services;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Logging.Abstractions;
using Xunit;

namespace McpChatWeb.Tests.Services;

// No configured application host, real estate, AI calls, database or production output.
public sealed class EstateLaunchTests : IDisposable
{
    [Theory]
    [InlineData("1792699201")]
    [InlineData("all")]
    [InlineData("../../other-artifacts")]
    public async Task SelectedSnapshotAssessmentRefusesUnapprovedBindingBeforeReadingDefaultSource(string scanRunId)
    {
        var reader = new RektEstateReader(NullLogger<RektEstateReader>.Instance, _root);
        var response = await EstateEndpoints.AssessDraftPlanAsync(reader, CancellationToken.None, scanRunId);
        Assert.Equal(409, Assert.IsAssignableFrom<IStatusCodeHttpResult>(response).StatusCode);
        var payload = JsonSerializer.SerializeToElement(Assert.IsAssignableFrom<IValueHttpResult>(response).Value,
            new JsonSerializerOptions(JsonSerializerDefaults.Web));
        Assert.Equal("snapshot-evidence-binding-unavailable", payload.GetProperty("code").GetString());
        Assert.Equal(scanRunId, payload.GetProperty("scanRunId").GetString());
        Assert.Contains("No default-path assessment", payload.GetProperty("reason").GetString());
        Assert.Contains("dependency-plan", payload.GetProperty("action").GetString());
        Assert.False(Directory.Exists(_root));
    }

    [Fact]
    public async Task DraftPlanEndpointReportsExactMissingEvidenceWithoutStartingConversion()
    {
        Prepare();
        var reader = new RektEstateReader(NullLogger<RektEstateReader>.Instance, _root);
        var response = await EstateEndpoints.AssessDraftPlanAsync(reader, CancellationToken.None);
        var plan = Assert.IsType<CobolToQuarkusMigration.Agents.Infrastructure.Facts.DraftConversionPlan>(
            Assert.IsAssignableFrom<IValueHttpResult>(response).Value);
        Assert.False(plan.Verified);
        Assert.Equal("blocked", plan.Status);
        Assert.Equal(2, plan.Programs.Count);
        Assert.All(plan.Programs, program =>
        {
            Assert.Equal("blocked", program.Status);
            Assert.Contains(program.Blockers, blocker => blocker.MissingEvidence.Contains("dependency-evidence.json"));
        });
        Assert.Contains(plan.Programs, p => p.Program == "batch/PAY.cbl");
        Assert.Contains(plan.Programs, p => p.Program == "online/PAY.cbl");
        Assert.False(Directory.Exists(Path.Combine(_root, "Logs")));
        Assert.False(Directory.Exists(Path.Combine(_root, "output")));
    }

    private readonly string _root = Path.Combine(Directory.GetCurrentDirectory(), "output", "launch-tests",
        Guid.NewGuid().ToString("N"));

    private EstateGraphService Service() => new(
        new RektEstateReader(NullLogger<RektEstateReader>.Instance, _root),
        new ConversionParityReader(NullLogger<ConversionParityReader>.Instance, _root),
        NullLogger<EstateGraphService>.Instance);

    private string Prepare(string exitCode = "0")
    {
        Directory.CreateDirectory(Path.Combine(_root, "source", "batch"));
        Directory.CreateDirectory(Path.Combine(_root, "source", "online"));
        File.WriteAllText(Path.Combine(_root, "source", "batch", "PAY.cbl"),
            "       PROGRAM-ID. PAYB.\n       PROCEDURE DIVISION.\n           CALL 'PAYO'.\n");
        File.WriteAllText(Path.Combine(_root, "source", "online", "PAY.cbl"),
            "       PROGRAM-ID. PAYO.\n       PROCEDURE DIVISION.\n           STOP RUN.\n");
        var script = Path.Combine(_root, "test-child.sh");
        File.WriteAllText(script, """
            #!/bin/sh
            printf 'ARG=%s\n' "$@"
            printf 'MANAGED=%s\n' "$AI_LOOP_MANAGED_RUN_ID"
            printf 'TARGET=%s\n' "$TARGET_LANGUAGE"
            printf 'JAVA_FRAMEWORK=%s\n' "$JAVA_FRAMEWORK"
            printf 'REKT_FACTS=%s\n' "$REKT_FACTS_FOLDER"
            printf 'MODEL=%s\n' "$AZURE_OPENAI_MODEL_ID"
            printf 'CONVERTER=%s\n' "$AZURE_OPENAI_JAVA_CONVERTER_MODEL"
            printf 'BOUND_MODEL=%s\n' "$AISETTINGS__MODELID"
            printf 'BOUND_DEPLOYMENT=%s\n' "$AISETTINGS__DEPLOYMENTNAME"
            printf 'BOUND_ANALYZER=%s\n' "$AISETTINGS__COBOLANALYZERMODELID"
            printf 'BOUND_CONVERTER=%s\n' "$AISETTINGS__JAVACONVERTERMODELID"
            printf 'BOUND_MAPPER=%s\n' "$AISETTINGS__DEPENDENCYMAPPERMODELID"
            printf 'BOUND_UNITTEST=%s\n' "$AISETTINGS__UNITTESTMODELID"
            printf 'BOUND_CHAT=%s\n' "$AISETTINGS__CHATMODELID"
            printf 'BOUND_CHAT_DEPLOYMENT=%s\n' "$AISETTINGS__CHATDEPLOYMENTNAME"
            printf 'JAVA_OUTPUT=%s\n' "$JAVA_OUTPUT_FOLDER"
            printf 'CSHARP_OUTPUT=%s\n' "$CSHARP_OUTPUT_FOLDER"
            printf 'child stdout final line\n'
            printf 'child stderr final line\n' >&2
            """ + $"\nexit {exitCode}\n");
        if (!OperatingSystem.IsWindows())
            File.SetUnixFileMode(script, UnixFileMode.UserRead | UnixFileMode.UserWrite | UnixFileMode.UserExecute);
        return script;
    }

    private void PrepareEvidence()
    {
        var source = Path.Combine(_root, "source");
        var callee = Path.Combine(source, "online", "PAY.cbl");
        DependencyEvidenceFixture.Write(_root, source, "online/PAY.cbl");
        DependencyEvidenceFixture.Write(_root, source, "batch/PAY.cbl", new DependencyDefinition
        {
            Kind = "CALL",
            Name = "PAYO",
            Path = callee,
            Sha256 = DependencyEvidenceFixture.Hash(callee)
        });
    }

    [Fact]
    public async Task InventoryDistinguishesVerifiedZeroDependenciesFromMissingEvidence()
    {
        Prepare();
        DependencyEvidenceFixture.Write(_root, Path.Combine(_root, "source"), "online/PAY.cbl");
        using var manager = new ProcessManager(_root, "never-executed");
        var readiness = await manager.AssessDependenciesAsync();
        Assert.False(readiness.StrictAllowed);
        Assert.True(readiness.DraftAllowed);
        Assert.Equal("eligible", Assert.Single(readiness.Programs, p => p.Program == "online/PAY.cbl").Status);
        Assert.Contains(Assert.Single(readiness.Programs, p => p.Program == "batch/PAY.cbl").Blockers,
            b => b.Raw!.Contains("missing dependency-evidence.json"));
        Assert.Empty(manager.GetAllRuns());
    }

    [Fact]
    public async Task SelectionPropagatesBlockedCalleeWithoutMislabelingIndependentCallerInventory()
    {
        Prepare();
        PrepareEvidence();
        File.Delete(DependencyEvidenceGate.GetEvidencePath(Path.Combine(_root, "output", "rekt"), "online/PAY.cbl"));
        using var manager = new ProcessManager(_root, "never-executed");
        var inventory = await manager.AssessDependenciesAsync();
        Assert.Equal("eligible", Assert.Single(inventory.Programs, p => p.Program == "batch/PAY.cbl").Status);
        var selected = await manager.AssessDependenciesAsync(programs: ["batch/PAY.cbl", "online/PAY.cbl"]);
        Assert.All(selected.Programs, p => Assert.Equal("blocked", p.Status));
        Assert.False(selected.DraftAllowed);
        await Assert.ThrowsAsync<ArgumentException>(() => manager.AssessDependenciesAsync(programs: ["absent.cbl"]));
        Assert.Empty(manager.GetAllRuns());
    }

    [Theory]
    [InlineData("strict")]
    [InlineData("draft")]
    public async Task EstateEndpointRejectsMissingEvidenceBeforeStartingAChild(string mode)
    {
        using var manager = new ProcessManager(_root, Prepare());
        var service = Service();
        var cluster = Assert.Single((await service.GetSummaryAsync()).Clusters);
        var result = await EstateEndpoints.ConvertSliceAsync(new EstateConvertRequest(cluster.Id, DependencyMode: mode),
            service, manager, CancellationToken.None);
        Assert.Equal(409, Assert.IsAssignableFrom<IStatusCodeHttpResult>(result).StatusCode);
        var run = Assert.Single(manager.GetAllRuns());
        Assert.Null(run.ProcessId);
        Assert.Equal("failed", run.Status);
        Assert.Contains("dependency evidence", run.Error);
    }

    [Fact]
    public async Task ReadinessRejectsStaleSourceAndKeepsSourceRelativeDuplicateIdentities()
    {
        Prepare();
        PrepareEvidence();
        using var manager = new ProcessManager(_root, "never-executed");
        var valid = await manager.AssessDependenciesAsync(programs: ["batch/PAY.cbl", "online/PAY.cbl"]);
        Assert.True(valid.StrictAllowed);
        Assert.Equal(2, valid.Programs.Count);
        File.AppendAllText(Path.Combine(_root, "source", "online", "PAY.cbl"), "\n      * Changed after parse\n");
        var stale = await manager.AssessDependenciesAsync(programs: ["online/PAY.cbl"]);
        Assert.False(stale.StrictAllowed);
        Assert.NotEmpty(Assert.Single(stale.Programs).Blockers);
    }

    [UnixFact]
    public async Task ExplicitPartialLaunchKeepsItsScopeAndPartialTerminalStatus()
    {
        using var manager = new ProcessManager(_root, Prepare());
        var run = manager.StartRun("convert-only", "partial source tree", extraEnv: new()
        {
            ["JAVA_FRAMEWORK"] = "Spring",
            ["CONVERSION_DEPENDENCY_MODE"] = "partial"
        });
        await run.Completion.WaitAsync(TimeSpan.FromSeconds(10));
        Assert.Equal("partial", run.DependencyMode);
        Assert.Equal("partial", run.Status);
        Assert.Equal(0, run.ExitCode);
    }

    [UnixFact]
    public async Task ImmutableCliLaunchDoesNotBuildTheSourceProject()
    {
        var executable = Prepare();
        var assembly = Path.Combine(_root, "tested-cli.dll");
        File.WriteAllText(assembly, "controlled child records arguments; not an executable assembly");
        using var manager = new ProcessManager(_root, executable, assembly);
        var run = manager.StartRun("convert-only", "immutable CLI");
        await run.Completion.WaitAsync(TimeSpan.FromSeconds(10));
        var log = File.ReadAllText(run.LogPath!);
        Assert.Contains("ARG=" + assembly, log);
        Assert.Contains("ARG=--skip-reverse-engineering", log);
        Assert.DoesNotContain("ARG=--project", log);
    }

    [UnixFact]
    public async Task ExplicitSpringRunPassesItsOwnArtifactRootToTheActualChild()
    {
        var executable = Prepare();
        var facts = Path.Combine(_root, "output", "rekt-scans", "fresh", "artifacts");
        Directory.CreateDirectory(facts);
        using var manager = new ProcessManager(_root, executable);
        var run = manager.StartRun("convert-only", "spring snapshot", extraEnv: new()
        {
            ["JAVA_FRAMEWORK"] = "Spring",
            ["REKT_FACTS_FOLDER"] = "output/rekt-scans/fresh/artifacts"
        });
        await run.Completion.WaitAsync(TimeSpan.FromSeconds(10));
        var log = File.ReadAllText(run.LogPath!);
        Assert.Contains("JAVA_FRAMEWORK=Spring", log);
        Assert.Contains("REKT_FACTS=output/rekt-scans/fresh/artifacts", log);
    }

    [Theory]
    [InlineData("../other")]
    [InlineData("/tmp/other")]
    [InlineData("output/missing")]
    public void ExplicitArtifactRootCannotEscapeTheRepositoryOrSilentlyFallBack(string facts)
    {
        var executable = Prepare();
        using var manager = new ProcessManager(_root, executable);
        Assert.Throws<ManagedRunValidationException>(() =>
            manager.StartRun("convert-only", "invalid facts", extraEnv: new()
            {
                ["REKT_FACTS_FOLDER"] = facts
            }));
        Assert.Equal("failed", Assert.Single(manager.GetAllRuns()).Status);
    }

    [UnixFact]
    public async Task EstateEndpointLaunchesTheActualSelectedPathsAndCorrelatesTheChild()
    {
        var executable = Prepare();
        PrepareEvidence();
        var service = Service();
        var cluster = Assert.Single((await service.GetSummaryAsync()).Clusters);
        using var manager = new ProcessManager(_root, executable);

        var response = await EstateEndpoints.ConvertSliceAsync(
            new EstateConvertRequest(cluster.Id, "CSharp", Provider: "GitHubCopilot", ModelId: "chosen-model"),
            service, manager, CancellationToken.None);
        var run = Assert.Single(manager.GetAllRuns());
        await run.Completion.WaitAsync(TimeSpan.FromSeconds(10));

        Assert.Equal(200, Assert.IsAssignableFrom<IStatusCodeHttpResult>(response).StatusCode);
        var payload = JsonSerializer.SerializeToElement(Assert.IsAssignableFrom<IValueHttpResult>(response).Value,
            new JsonSerializerOptions(JsonSerializerDefaults.Web));
        Assert.Equal(run.RunId, payload.GetProperty("runId").GetString());
        Assert.Equal<string>(new[] { "batch/PAY.cbl", "online/PAY.cbl" }, run.Programs.Order());
        Assert.Equal("completed", run.Status);
        Assert.Equal(0, run.ExitCode);
        var log = File.ReadAllText(run.LogPath!);
        var arguments = string.Join('\n', File.ReadAllLines(run.LogPath!).Where(line => line.StartsWith("ARG=")));
        Assert.Contains("ARG=--skip-reverse-engineering", log);
        Assert.Contains("ARG=--programs\nARG=batch/PAY.cbl,online/PAY.cbl", arguments);
        Assert.Contains($"ARG={Path.Combine(_root, "source")}", log);
        Assert.Contains($"MANAGED={run.RunId}", log);
        Assert.Contains("TARGET=CSharp", log);
        Assert.Contains("MODEL=chosen-model", log);
        Assert.Contains("CONVERTER=chosen-model", log);
        Assert.Equal(run.OutputFolder, payload.GetProperty("outputFolder").GetString());
        Assert.StartsWith("output/csharp/", run.OutputFolder);
        Assert.Contains($"CSHARP_OUTPUT={run.OutputFolder}", log);
        Assert.True(Directory.Exists(Path.Combine(_root, run.OutputFolder!)));
        Assert.Contains("child stdout final line", log);
        Assert.Contains("child stderr final line", log);
        Assert.EndsWith("[PORTAL] Process exited with code 0" + Environment.NewLine, log);
    }

    [UnixFact]
    public async Task FailureBeforeNumericTelemetryIsTerminalAndKeepsBothOutputStreams()
    {
        using var manager = new ProcessManager(_root, Prepare("23"));

        var run = manager.StartRun("convert-only", "preflight-failure", programs: ["batch/PAY.cbl"]);
        await run.Completion.WaitAsync(TimeSpan.FromSeconds(10));

        Assert.Equal("failed", run.Status);
        Assert.Equal(23, run.ExitCode);
        Assert.NotNull(run.CompletedAt);
        Assert.Contains("23", run.Error);
        Assert.Same(run, manager.GetRun(run.RunId));
        var log = File.ReadAllText(run.LogPath!);
        Assert.Contains("child stdout final line", log);
        Assert.Contains("child stderr final line", log);
        Assert.Contains("[PORTAL] Process exited with code 23", log);
        Assert.DoesNotContain("run_started", log);
    }

    [UnixFact]
    public async Task OmittedModelDoesNotReplaceConfiguredPerAgentModelsWithAHardcodedDefault()
    {
        using var manager = new ProcessManager(_root, Prepare());
        var run = manager.StartRun("convert-only", "configured-models", extraEnv: new()
        {
            ["AZURE_OPENAI_MODEL_ID"] = "configured-main",
            ["AZURE_OPENAI_JAVA_CONVERTER_MODEL"] = "configured-converter",
            ["AISETTINGS__MODELID"] = "configured-bound-main",
            ["AISETTINGS__JAVACONVERTERMODELID"] = "configured-bound-converter",
            ["AISETTINGS__CHATMODELID"] = "configured-bound-chat"
        });
        await run.Completion.WaitAsync(TimeSpan.FromSeconds(10));

        Assert.Equal("completed", run.Status);
        var log = File.ReadAllText(run.LogPath!);
        Assert.Contains("MODEL=configured-main", log);
        Assert.Contains("CONVERTER=configured-converter", log);
        Assert.Contains("BOUND_MODEL=configured-bound-main", log);
        Assert.Contains("BOUND_CONVERTER=configured-bound-converter", log);
        Assert.Contains("BOUND_CHAT=configured-bound-chat", log);
    }

    [UnixFact]
    public async Task ExplicitModelOverridesConflictingInheritedAliasesAndPerAgentModels()
    {
        using var manager = new ProcessManager(_root, Prepare());
        var run = manager.StartRun("convert-only", "selected-model", modelId: "selected-model", extraEnv: new()
        {
            ["AZURE_OPENAI_MODEL_ID"] = "old-main",
            ["AZURE_OPENAI_JAVA_CONVERTER_MODEL"] = "old-converter",
            ["AISETTINGS__MODELID"] = "old-bound-main",
            ["AISETTINGS__DEPLOYMENTNAME"] = "old-deployment",
            ["AISETTINGS__COBOLANALYZERMODELID"] = "old-analyzer",
            ["AISETTINGS__JAVACONVERTERMODELID"] = "old-bound-converter",
            ["AISETTINGS__DEPENDENCYMAPPERMODELID"] = "old-mapper",
            ["AISETTINGS__UNITTESTMODELID"] = "old-unittest",
            ["AISETTINGS__CHATMODELID"] = "old-chat",
            ["AISETTINGS__CHATDEPLOYMENTNAME"] = "old-chat-deployment"
        });
        await run.Completion.WaitAsync(TimeSpan.FromSeconds(10));

        Assert.Equal("completed", run.Status);
        var lines = File.ReadAllLines(run.LogPath!);
        foreach (var label in new[]
        {
            "MODEL", "CONVERTER", "BOUND_MODEL", "BOUND_DEPLOYMENT", "BOUND_ANALYZER",
            "BOUND_CONVERTER", "BOUND_MAPPER", "BOUND_UNITTEST", "BOUND_CHAT", "BOUND_CHAT_DEPLOYMENT"
        })
            Assert.Contains($"{label}=selected-model", lines);
    }

    [UnixFact]
    public async Task AnAbsoluteInRepositorySourceIsNotPrefixedWithDotSlash()
    {
        using var manager = new ProcessManager(_root, Prepare());
        var source = Path.Combine(_root, "source");

        var run = manager.StartRun("convert-only", "absolute-source", sourceFolder: source);
        await run.Completion.WaitAsync(TimeSpan.FromSeconds(10));

        Assert.Equal("completed", run.Status);
        var log = File.ReadAllText(run.LogPath!);
        var arguments = string.Join('\n', File.ReadAllLines(run.LogPath!).Where(line => line.StartsWith("ARG=")));
        Assert.Contains($"ARG=--source\nARG={source}", arguments);
        Assert.DoesNotContain($"ARG=./{source}", log);
    }

    [UnixFact]
    public async Task EveryLaunchReservesItsOwnPersistentOutputEvenWhenAnOverrideWouldReuseOldArtifacts()
    {
        using var manager = new ProcessManager(_root, Prepare());
        Directory.CreateDirectory(Path.Combine(_root, "JavaOutput"));
        var oldArtifact = Path.Combine(_root, "JavaOutput", "run74.txt");
        File.WriteAllText(oldArtifact, "historical evidence");

        var first = manager.StartRun("convert-only", "first", extraEnv: new()
        {
            ["JAVA_OUTPUT_FOLDER"] = "JavaOutput"
        });
        var second = manager.StartRun("convert-only", "second", extraEnv: new()
        {
            ["JAVA_OUTPUT_FOLDER"] = "JavaOutput"
        });
        await Task.WhenAll(first.Completion, second.Completion).WaitAsync(TimeSpan.FromSeconds(10));

        Assert.Equal("completed", first.Status);
        Assert.Equal("completed", second.Status);
        Assert.NotEqual(first.OutputFolder, second.OutputFolder);
        foreach (var run in new[] { first, second })
        {
            Assert.StartsWith("output/java/", run.OutputFolder);
            Assert.True(Directory.Exists(Path.Combine(_root, run.OutputFolder!)));
            Assert.True(CobolToQuarkusMigration.Helpers.ConversionOutputFolder.LooksLikeRun(
                Path.GetFileName(run.OutputFolder!)));
            Assert.Contains($"JAVA_OUTPUT={run.OutputFolder}", File.ReadAllText(run.LogPath!));
        }
        Assert.Equal("historical evidence", File.ReadAllText(oldArtifact));
    }

    [UnixFact]
    public async Task UnlaunchableLogDestinationReturnsServiceUnavailableAndStartsNoChild()
    {
        var executable = Prepare();
        PrepareEvidence();
        Directory.CreateDirectory(Path.Combine(_root, "Logs"));
        File.WriteAllText(Path.Combine(_root, "Logs", "portal"), "not a directory");
        var service = Service();
        var cluster = Assert.Single((await service.GetSummaryAsync()).Clusters);
        using var manager = new ProcessManager(_root, executable);

        var response = await EstateEndpoints.ConvertSliceAsync(new EstateConvertRequest(cluster.Id),
            service, manager, CancellationToken.None);

        Assert.Equal(503, Assert.IsAssignableFrom<IStatusCodeHttpResult>(response).StatusCode);
        var run = Assert.Single(manager.GetAllRuns());
        Assert.Equal("failed", run.Status);
        Assert.NotNull(run.CompletedAt);
        Assert.NotEmpty(run.Error!);
        Assert.Null(run.ProcessId);
        Assert.Null(run.LogPath);
        Assert.Contains(run.GetLogLines(), l => l.StartsWith("ERROR:"));
    }

    [UnixFact]
    public async Task InvalidLanguageAndUnknownClusterAreRecordedWithoutStartingAChild()
    {
        using var manager = new ProcessManager(_root, Prepare());
        var service = Service();
        var cluster = Assert.Single((await service.GetSummaryAsync()).Clusters);

        var invalid = await EstateEndpoints.ConvertSliceAsync(new EstateConvertRequest(cluster.Id, "invalid"),
            service, manager, CancellationToken.None);
        var missing = await EstateEndpoints.ConvertSliceAsync(new EstateConvertRequest("NO-SUCH-CLUSTER"),
            service, manager, CancellationToken.None);

        Assert.Equal(400, Assert.IsAssignableFrom<IStatusCodeHttpResult>(invalid).StatusCode);
        Assert.Equal(404, Assert.IsAssignableFrom<IStatusCodeHttpResult>(missing).StatusCode);
        Assert.Equal(2, manager.GetAllRuns().Count);
        Assert.All(manager.GetAllRuns(), run =>
        {
            Assert.Equal("failed", run.Status);
            Assert.Null(run.ProcessId);
            Assert.NotNull(run.Error);
        });
        using var recovered = new ProcessManager(_root, "never-executed");
        Assert.Equal(2, recovered.GetAllRuns().Count);
    }

    [UnixFact]
    public async Task CompletedLaunchRetainsSettingsOutputAndDurableLogsAcrossPortalRestart()
    {
        ManagedRun run;
        using (var manager = new ProcessManager(_root, Prepare()))
        {
            run = manager.StartRun("convert-only", "keep-history", programs: ["batch/PAY.cbl"],
                extraEnv: new() { ["PRIVATE_TEST_SECRET"] = "do-not-record-config" });
            await run.Completion.WaitAsync(TimeSpan.FromSeconds(10));
        }
        using var recovered = new ProcessManager(_root, "never-executed");
        var historical = recovered.GetRun(run.RunId)!;
        Assert.Equal("completed", historical.Status);
        Assert.Equal(run.OutputFolder, historical.OutputFolder);
        Assert.Equal(run.Programs, historical.Programs);
        Assert.Null(historical.ProcessId);
        Assert.Contains("child stdout final line", historical.GetLogLines());
        Assert.Single(historical.GetLogLines(1));
        var metadata = File.ReadAllText(Path.Combine(_root, "Logs", "portal-history", run.RunId + ".json"));
        Assert.DoesNotContain("PRIVATE_TEST_SECRET", metadata);
        Assert.DoesNotContain("do-not-record-config", metadata);
    }

    [Fact]
    public void RejectedLaunchSurvivesRestartWithActionableReasonAndNoInventedProcessOrLog()
    {
        Directory.CreateDirectory(Path.Combine(_root, "source"));
        string id;
        using (var manager = new ProcessManager(_root, "never-executed"))
        {
            var error = Assert.Throws<ManagedRunValidationException>(() =>
                manager.StartRun("convert-only", "invalid-speed", speedProfile: "unsupported"));
            id = error.Run.RunId;
        }
        using var recovered = new ProcessManager(_root, "never-executed");
        var run = recovered.GetRun(id)!;
        Assert.Equal("failed", run.Status);
        Assert.Contains("Speed profile", run.Error);
        Assert.NotNull(run.CompletedAt);
        Assert.Null(run.ProcessId);
        Assert.Null(run.LogPath);
    }

    [Fact]
    public void RestartMarksUnfinishedLaunchInterruptedWithoutReattachingAPossiblyReusedPid()
    {
        var folder = Path.Combine(_root, "Logs", "portal-history");
        Directory.CreateDirectory(folder);
        File.WriteAllText(Path.Combine(folder, "active.json"), JsonSerializer.Serialize(new ManagedRun
        {
            RunId = "active",
            Status = "running",
            ProcessId = 1234567,
            StartedAt = DateTime.UtcNow.AddMinutes(-1)
        }));
        using var manager = new ProcessManager(_root, "never-executed");
        var run = manager.GetRun("active")!;
        Assert.Equal("interrupted", run.Status);
        Assert.Null(run.ProcessId);
        Assert.Contains("may still be running", run.Error);
        Assert.Contains("recovery detection", run.HistoryWarning);
        Assert.Null(run.CompletedAt);
        Assert.NotNull(run.RecoveredAt);
        using var restartedAgain = new ProcessManager(_root, "never-executed");
        Assert.Equal(run.CompletedAt, restartedAgain.GetRun("active")!.CompletedAt);
    }

    [UnixFact]
    public async Task UserCancellationIsDurableAndKeepsOutputWithoutTurningIntoAnUnexplainedExitFailure()
    {
        var child = Prepare();
        File.WriteAllText(child, "#!/bin/sh\nprintf 'child ready\\n'\nwhile :; do sleep 1; done\n");
        string id;
        using (var manager = new ProcessManager(_root, child))
        {
            var run = manager.StartRun("convert-only", "cancel-me");
            id = run.RunId;
            for (var i = 0; i < 100 && !run.GetLogLines().Contains("child ready"); i++)
                await Task.Delay(20);
            Assert.Contains("child ready", run.GetLogLines());
            Assert.True(manager.StopRun(id));
            await run.Completion.WaitAsync(TimeSpan.FromSeconds(10));
            Assert.Equal("stopped", run.Status);
            Assert.Contains("Stopped by user", run.Error);
        }
        using var recovered = new ProcessManager(_root, "never-executed");
        Assert.Equal("stopped", recovered.GetRun(id)!.Status);
        Assert.Contains("child ready", recovered.GetRun(id)!.GetLogLines());
    }

    [Fact]
    public void LegacyLogsRemainDiscoverableWithoutGuessingStatusFromTheirContents()
    {
        var folder = Path.Combine(_root, "Logs", "portal");
        Directory.CreateDirectory(folder);
        File.WriteAllText(Path.Combine(folder, "old.log"), "An old process printed ERROR and later succeeded\n");
        using var manager = new ProcessManager(_root, "never-executed");
        var run = manager.GetRun("old")!;
        Assert.Equal("unknown", run.Status);
        Assert.Null(run.CompletedAt);
        Assert.Contains("not recorded", run.HistoryWarning);
        Assert.Single(run.GetLogLines());
    }

    [Fact]
    public void UnwritableHistoryRefusesToStartAProcessAndExplainsThatTheFailureIsNotDurable()
    {
        Directory.CreateDirectory(Path.Combine(_root, "Logs"));
        File.WriteAllText(Path.Combine(_root, "Logs", "portal-history"), "not a directory");
        using var manager = new ProcessManager(_root, "never-executed");

        var run = manager.StartRun("convert-only", "tracking-storage-failure");

        Assert.Equal("failed", run.Status);
        Assert.Null(run.ProcessId);
        Assert.Contains("Launch refused", run.Error);
        Assert.Contains("may not survive restart", run.HistoryWarning);
    }

    [Fact]
    public async Task CancellationDuringEstatePreparationIsRecordedBeforeAnyChildLaunch()
    {
        Directory.CreateDirectory(Path.Combine(_root, "source"));
        using var manager = new ProcessManager(_root, "never-executed");
        using var cancellation = new CancellationTokenSource();
        cancellation.Cancel();

        var response = await EstateEndpoints.ConvertSliceAsync(new EstateConvertRequest("cluster"),
            Service(), manager, cancellation.Token);

        Assert.Equal(409, Assert.IsAssignableFrom<IStatusCodeHttpResult>(response).StatusCode);
        var run = Assert.Single(manager.GetAllRuns());
        Assert.Equal("cancelled", run.Status);
        Assert.Null(run.ProcessId);
        using var recovered = new ProcessManager(_root, "never-executed");
        Assert.Equal("cancelled", recovered.GetRun(run.RunId)!.Status);
    }

    [UnixFact]
    public async Task MissingExecutableFailsRatherThanReportingSuccessfulConversion()
    {
        Prepare();
        using var manager = new ProcessManager(_root, Path.Combine(_root, "missing-executable"));

        var run = manager.StartRun("convert-only", "missing");
        await run.Completion.WaitAsync(TimeSpan.FromSeconds(10));

        Assert.Equal("failed", run.Status);
        Assert.Equal(127, run.ExitCode);
        Assert.Contains("missing-executable", File.ReadAllText(run.LogPath!));
    }

    public void Dispose()
    {
        if (Directory.Exists(_root)) Directory.Delete(_root, recursive: true);
    }
}

internal sealed class UnixFactAttribute : FactAttribute
{
    public UnixFactAttribute()
    {
        if (OperatingSystem.IsWindows()) Skip = "The portal's Unix process-wrapper integration requires /bin/sh.";
    }
}
