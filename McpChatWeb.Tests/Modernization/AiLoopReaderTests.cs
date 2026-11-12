using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http.Json;
using System.Text.Json;
using System.Threading.Tasks;
using CobolToQuarkusMigration.Agents;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Jcl;
using CobolToQuarkusMigration.Jcl.Generation;
using CobolToQuarkusMigration.Persistence;
using McpChatWeb.Services;
using Microsoft.Extensions.Logging.Abstractions;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.TestHost;
using Microsoft.Extensions.DependencyInjection;
using McpChatWeb.Endpoints;
using Xunit;

namespace McpChatWeb.Tests.Modernization;

public sealed class AiLoopReaderTests : IDisposable
{
    private readonly string _root = Path.Combine(Directory.GetCurrentDirectory(), ".ai-loop-" + Guid.NewGuid().ToString("N"));
    private const string RunFolder = "output/csharp/20260101-000000";

    public AiLoopReaderTests()
    {
        Directory.CreateDirectory(Path.Combine(_root, "output", ".metrics"));
        Directory.CreateDirectory(Path.Combine(_root, RunFolder));
        File.WriteAllText(Path.Combine(_root, "doctor.sh"), "#!/usr/bin/env bash\n");
    }

    public void Dispose()
    {
        try { Directory.Delete(_root, recursive: true); } catch (IOException) { }
    }

    private AiLoopReader Reader() => new(NullLogger<AiLoopReader>.Instance, _root);

    private static readonly DateTime T0 = new(2026, 1, 1, 10, 0, 0, DateTimeKind.Utc);

    private string NativeManifest(string name, object manifest)
    {
        var folder = Path.Combine(_root, "output", "rekt-scans", name);
        Directory.CreateDirectory(folder);
        var path = Path.Combine(folder, "scan.json");
        File.WriteAllText(path, JsonSerializer.Serialize(manifest));
        return path;
    }

    [Fact]
    public async Task NativeLogTailIsBoundedOriginalTextAndRejectsMissingChangedAndLinkedFiles()
    {
        var path = NativeManifest("log-test", new
        {
            schemaVersion = 1, activityKind = "native-rekt", name = "log-test", startedAt = T0,
            status = "completed", finishedAt = T0.AddMinutes(1), logPath = "native.log", programs = Array.Empty<object>()
        });
        var reader = Reader();
        var locator = Assert.Single(await reader.ListRunsAsync()).RunId;
        Assert.Equal(404, (await reader.ReadNativeLogAsync(locator)).StatusCode);
        Assert.Equal(400, (await reader.ReadNativeLogAsync("../doctor.sh")).StatusCode);
        Assert.Equal(404, (await reader.ReadNativeLogAsync("native-missing")).StatusCode);
        var log = Path.Combine(Path.GetDirectoryName(path)!, "native.log");
        await File.WriteAllTextAsync(log, new string('a', 100000) + "\noriginal <compiler> diagnostic");
        var result = await reader.ReadNativeLogAsync(locator);
        Assert.Equal(200, result.StatusCode);
        Assert.True(result.Log!.Truncated);
        Assert.Equal(65536, result.Log.ReadBytes);
        Assert.EndsWith("original <compiler> diagnostic", result.Log.Text);
        Assert.True(result.Log.TotalBytes > result.Log.ReadBytes);
        Assert.True(result.Log.RefreshedAt >= result.Log.LastWriteAt);
        await File.WriteAllTextAsync(path, File.ReadAllText(path).Replace("native.log", "../../doctor.sh"));
        Assert.Equal(409, (await reader.ReadNativeLogAsync(locator)).StatusCode);
        await File.WriteAllTextAsync(path, File.ReadAllText(path).Replace("../../doctor.sh", "native.log"));
        if (!OperatingSystem.IsWindows())
        {
            var permissions = File.GetUnixFileMode(log);
            try
            {
                File.SetUnixFileMode(log, UnixFileMode.None);
                Assert.Equal(403, (await reader.ReadNativeLogAsync(locator)).StatusCode);
            }
            finally { File.SetUnixFileMode(log, permissions); }
            File.Delete(log);
            File.CreateSymbolicLink(log, Path.Combine(_root, "doctor.sh"));
            Assert.Equal(409, (await reader.ReadNativeLogAsync(locator)).StatusCode);
            File.Delete(log);
            await File.WriteAllTextAsync(log, "restored");
            File.Delete(path);
            File.CreateSymbolicLink(path, Path.Combine(_root, "doctor.sh"));
            Assert.NotEqual(200, (await reader.ReadNativeLogAsync(locator)).StatusCode);
        }
    }

    [Fact]
    public async Task NativeActivityBoundKeepsTheNewestManifests()
    {
        // Twice the bound, so an unordered bound drops some of the newest whatever the directory order.
        for (var i = 0; i < 1024; i++)
        {
            var at = T0.AddMinutes(i);
            var path = NativeManifest($"scan-{i:D4}", new
            {
                schemaVersion = 1, activityKind = "native-rekt", name = $"scan-{i:D4}", startedAt = at,
                status = "completed", finishedAt = at.AddSeconds(30), logPath = "native.log", programs = Array.Empty<object>()
            });
            File.SetLastWriteTimeUtc(path, at);
        }

        var runs = await Reader().ListRunsAsync(limit: 50);

        Assert.Equal(Enumerable.Range(974, 50).Reverse().Select(i => $"native:scan-{i:D4}"), runs.Select(run => run.ActivityId));
    }

    [Fact]
    public async Task NativeLogRejectsMalformedManifestTypesWithoutServingCachedSuccess()
    {
        var path = NativeManifest("changed-types", new
        {
            schemaVersion = 1, activityKind = "native-rekt", name = "changed-types", startedAt = T0,
            status = "running", expectedPrograms = 1, logPath = "native.log", programs = Array.Empty<object>()
        });
        await File.WriteAllTextAsync(Path.Combine(Path.GetDirectoryName(path)!, "native.log"), "original log");
        var original = await File.ReadAllTextAsync(path);
        var reader = Reader();
        var locator = Assert.Single(await reader.ListRunsAsync()).RunId;
        Assert.Equal(200, (await reader.ReadNativeLogAsync(locator)).StatusCode);
        foreach (var invalid in new[]
        {
            "[]",
            original.Replace("\"expectedPrograms\":1", "\"expectedPrograms\":\"one\""),
            original.Replace("\"schemaVersion\":1", "\"schemaVersion\":{}"),
            original.Replace("\"programs\":[]", "\"programs\":[false]"),
            original.Replace("\"programs\":[]", "\"programs\":[],\"stages\":[null]")
        })
        {
            await File.WriteAllTextAsync(path, invalid);
            var cached = Assert.Single(await reader.ListRunsAsync());
            Assert.Equal(locator, cached.RunId);
            var result = await reader.ReadNativeLogAsync(locator);
            Assert.Equal(409, result.StatusCode);
            Assert.Null(result.Log);
            Assert.Contains("changed", result.Error);
        }
    }

    [Fact]
    public async Task NativeMeasuredRowsReadCheckedExportExitAndReasonsWithoutInventingStructureExit()
    {
        NativeManifest("measured", new
        {
            schemaVersion = 1, activityKind = "native-rekt", name = "measured", startedAt = T0,
            status = "running", expectedPrograms = 2,
            programs = new[] { new { relativePath = "A.cbl", status = "Full", hasAst = true, hasCfg = true,
                hasData = true, strictDependencyComplete = false, dependencyExportExitCode = 0,
                startedAt = T0, finishedAt = T0.AddSeconds(2),
                measurementReasons = new { strictDependencyComplete = "Missing COPY evidence" } } }
        });
        var reader = Reader();
        var summary = Assert.Single(await reader.ListRunsAsync());
        var row = Assert.Single((await reader.GetRunAsync(summary.RunId))!.Native!.Programs);
        Assert.True(row.HasAst);
        Assert.False(row.StrictDependencyComplete);
        Assert.Null(row.StructureExitCode);
        Assert.Equal(0, row.DependencyExitCode);
        Assert.Equal("Missing COPY evidence", row.MeasurementReasons!["strictDependencyComplete"]);
        Assert.Null(row.DependencyInputs);
    }

    [Fact]
    public async Task NativeRecordedInputsRemainHistoricalAndDiagnosticsHaveReadableCauses()
    {
        var sourceRoot = Path.Combine(_root, "source");
        var absentNow = Path.Combine(sourceRoot, "common", "SHARED.cpy");
        NativeManifest("recorded-inputs", new
        {
            schemaVersion = 1, activityKind = "native-rekt", name = "recorded-inputs", startedAt = T0,
            status = "completed-with-gaps",
            programs = new[] { new { relativePath = "app/A.cbl", status = "Partial", inputSourceRoot = sourceRoot,
                resolvedDefinitions = new[] { new { kind = "COPY", name = "SHARED", path = absentNow } },
                dependencyDiagnostics = new[] { "native:unresolved-copybook:HEADER",
                    "native:unresolved-EXEC_CICS-dependency-bindings:Cics_newContext" },
                dependencyDiagnosticsTruncated = true, resolvedDefinitionsTruncated = true } }
        });
        var reader = Reader();
        var summary = Assert.Single(await reader.ListRunsAsync());
        var row = Assert.Single((await reader.GetRunAsync(summary.RunId))!.Native!.Programs);
        var input = Assert.Single(row.DependencyInputs!.Items);
        Assert.False(File.Exists(absentNow));
        Assert.Equal("recorded", input.Status);
        Assert.Equal("shared", input.Scope);
        Assert.Contains("truncated", row.DependencyInputs.Warning);
        Assert.True(row.DependencyDiagnosticsTruncated);
        Assert.Contains(row.DependencyIssues!, issue => issue.Category == "missing-copybook" && issue.Item == "HEADER");
        Assert.Contains(row.DependencyIssues!, issue => issue.Category == "binding-gap");
    }

    [Fact]
    public async Task LegacyNativeNotesCanBeExplainedWithoutInventingAnInputInventory()
    {
        NativeManifest("legacy-inputs", new
        {
            schemaVersion = 1, activityKind = "native-rekt", name = "legacy-inputs", startedAt = T0,
            status = "completed-with-gaps",
            programs = new[] { new { relativePath = "A.cbl", status = "Partial",
                measurementReasons = new { dependencyDetails = "native:unresolved-copybook:HEADER | native:partial-derived-bms-layout:SCREEN.cpy" } } }
        });
        var reader = Reader();
        var summary = Assert.Single(await reader.ListRunsAsync());
        var row = Assert.Single((await reader.GetRunAsync(summary.RunId))!.Native!.Programs);
        Assert.Null(row.DependencyInputs);
        Assert.Contains(row.DependencyIssues!, issue => issue.Category == "missing-copybook" && issue.Item == "HEADER");
        Assert.Contains(row.DependencyIssues!, issue => issue.Category == "derived-layout");
        Assert.Contains("native:unresolved-copybook", row.MeasurementReasons!["dependencyDetails"]);
    }

    [Fact]
    public async Task NativeFullPipelineExposesRecordedStagesAndFinalIngestionFailureWithoutGraphIdInference()
    {
        NativeManifest("doctor-full", new
        {
            schemaVersion = 1, activityKind = "native-rekt", name = "doctor-full", command = "rekt-full",
            startedAt = T0, finishedAt = T0.AddSeconds(30), status = "failed", currentStage = "Native graph ingestion",
            programs = new[] { new { relativePath = "A.cbl", status = "Full" } },
            stages = new[]
            {
                new { name = "Native parsing", startedAt = T0, finishedAt = T0.AddSeconds(10) },
                new { name = "Native JCL scan", startedAt = T0.AddSeconds(10), finishedAt = T0.AddSeconds(15) },
                new { name = "Native graph ingestion", startedAt = T0.AddSeconds(15), finishedAt = T0.AddSeconds(30) }
            }
        });
        var reader = Reader();
        var summary = Assert.Single(await reader.ListRunsAsync());
        var detail = (await reader.GetRunAsync(summary.RunId))!;
        Assert.Equal("rekt-full", summary.Mode);
        Assert.Equal("failed", summary.Status);
        Assert.Equal(T0.AddSeconds(30), summary.FinishedAt);
        Assert.Equal(new long?[] { 10000, 5000, 15000 }, detail.Stages.Select(s => s.DurationMs));
        Assert.Equal("Native graph ingestion", detail.Stages.Last().Name);
        Assert.Equal("Full", detail.Native!.Programs[0].Status);
        Assert.Null(detail.Native.GraphSnapshotId);
        Assert.Empty(detail.Agents);
    }

    [Fact]
    public async Task NativeManifestHistoryUsesRecordedTimesAndIndependentIdentityWithoutModelSuccess()
    {
        ACompletedRun();
        var path = NativeManifest("evidence", new
        {
            name = "evidence", imageId = "sha256:" + new string('a', 64), expectedPrograms = 2,
            startedAt = T0.AddDays(1), finishedAt = T0.AddDays(1).AddHours(1),
            status = "completed-with-gaps",
            programs = new[] { new { relativePath = "A.cbl", structureStatus = "failed",
                hasAst = false, hasCfg = false, hasData = false, strictDependencyComplete = false,
                structure = new { exitCode = 1, stdout = "logs/A.log", stderr = "../private.log", command = "raw-source-must-not-escape" } } }
        });
        File.SetLastWriteTimeUtc(path, T0.AddYears(5));
        var reader = Reader();
        var runs = await reader.ListRunsAsync();
        Assert.Equal(2, runs.Count);
        var native = runs[0];
        Assert.Equal("native-rekt", native.ActivityKind);
        Assert.Equal("native:evidence", native.ActivityId);
        Assert.Equal(T0.AddDays(1).AddHours(1), native.LastEventAt);
        Assert.Null(native.ManagedRunId);
        Assert.Null(native.AttemptId);
        Assert.Equal("not-applicable", native.RequestCoverage);
        Assert.Equal(0, native.Calls);
        var detail = (await reader.GetRunAsync(native.RunId))!;
        Assert.Empty(detail.Agents);
        Assert.Empty(detail.Events);
        Assert.DoesNotContain(detail.Gates, g => g.Id == "model");
        Assert.Equal(1, detail.Native!.FailedPrograms);
        Assert.Null(detail.Native.Programs[0].UpdatedAt);
        Assert.Contains(detail.Logs!, l => l.Path.EndsWith("/logs/A.log"));
        Assert.DoesNotContain(detail.Logs!, l => l.Path.Contains("private"));
        Assert.DoesNotContain("raw-source-must-not-escape", JsonSerializer.Serialize(detail));
        Assert.Null(await reader.GetRunAsync(native.RunId, attemptId: "invented"));
    }

    [Fact]
    public async Task NativeJournalUpdatesRefreshCachedRunningFailedAndCancelledWithoutMtimeFinishes()
    {
        var reader = Reader();
        var start = DateTime.UtcNow;
        var path = NativeManifest("doctor-test", new
        {
            schemaVersion = 1, activityKind = "native-rekt", name = "doctor-test",
            startedAt = start, updatedAt = start, status = "running", programs = Array.Empty<object>()
        });
        var running = Assert.Single(await reader.ListRunsAsync());
        Assert.Equal("running", running.Status);
        Assert.Null(running.FinishedAt);
        await File.WriteAllTextAsync(path, "{");
        var partial = (await reader.GetRunAsync(running.RunId))!;
        Assert.Contains("last valid", partial.Warning);
        Assert.Null(partial.Summary.FinishedAt);
        await File.WriteAllTextAsync(path, "{}");
        var incomplete = (await reader.GetRunAsync(running.RunId))!;
        Assert.Contains("incomplete or invalid", incomplete.Warning);
        Assert.Null(incomplete.Summary.FinishedAt);
        foreach (var status in new[] { "failed", "cancelled", "partial" })
        {
            NativeManifest("doctor-test", new
            {
                schemaVersion = 1, activityKind = "native-rekt", name = "doctor-test",
                startedAt = start, finishedAt = start.AddSeconds(status.Length),
                status, error = "Recorded native outcome: " + status, programs = Array.Empty<object>()
            });
            var ended = (await reader.GetRunAsync(running.RunId))!;
            Assert.Equal(status, ended.Summary.Status);
            Assert.Equal(start.AddSeconds(status.Length), ended.Summary.FinishedAt);
        }
    }

    [Fact]
    public async Task NativeDiscoveryRejectsUnrelatedNestedAndIncompleteManifestsAndMarksStaleUnknownFinish()
    {
        NativeManifest("artifact", new { name = "artifact", startedAt = T0, programs = Array.Empty<object>() });
        NativeManifest("nested", new { name = "wrong", schemaVersion = 1, activityKind = "native-rekt", startedAt = T0, programs = Array.Empty<object>() });
        NativeManifest("journal", new { name = "journal", schemaVersion = 1, activityKind = "native-rekt",
            startedAt = T0, status = "running", programs = Array.Empty<object>() });
        var unrelated = Path.Combine(_root, "output", "rekt-scans", "artifacts", "child");
        Directory.CreateDirectory(unrelated);
        File.WriteAllText(Path.Combine(unrelated, "scan.json"), """{"schemaVersion":1,"activityKind":"native-rekt","name":"child","startedAt":"2026-01-01T00:00:00Z","programs":[]}""");
        var single = Assert.Single(await Reader().ListRunsAsync());
        Assert.Equal("native:journal", single.ActivityId);
        Assert.Equal("interrupted", single.Status);
        Assert.Null(single.FinishedAt);
        Assert.Equal(T0, single.LastEventAt);
    }

    private static string Line(int seconds, object payload)
    {
        var dict = new Dictionary<string, object?> { ["ts"] = T0.AddSeconds(seconds).ToString("o"), ["runId"] = "7" };
        foreach (var p in payload.GetType().GetProperties()) dict[p.Name] = p.GetValue(payload);
        return JsonSerializer.Serialize(dict);
    }

    private void Metrics(string runId, params string[] lines)
        => File.WriteAllLines(Path.Combine(_root, "output", ".metrics", runId + ".jsonl"), lines);

    private void Artifact(string name, string json) => File.WriteAllText(Path.Combine(_root, RunFolder, name), json);

    private void ACompletedRun(string folder = RunFolder, string targetLanguage = "C#") => Metrics("7",
        Line(0, new { @event = "run_started", mode = "standard", targetLanguage, outputFolder = folder }),
        Line(1, new { @event = "stage", number = 1, total = 6, name = "File Discovery" }),
        Line(4, new { @event = "stage", number = 2, total = 6, name = "Dependency Analysis" }),
        Line(5, new { @event = "llm_call", agent = "Converter", model = "m1", context = "A.cbl", durationMs = 100, success = true, promptChars = 400, responseChars = 80, inputTokens = 100, outputTokens = 20 }),
        Line(6, new { @event = "llm_call", agent = "Converter", model = "m1", context = "B.cbl", durationMs = 300, success = false, promptChars = 400, responseChars = 0, error = "TimeoutException: slow" }),
        Line(7, new { @event = "llm_retry", agent = "Converter", context = "B.cbl", reason = "transient_error", attempt = 1 }),
        Line(9, new { @event = "llm_call", agent = "Converter", model = "m1", context = "B.cbl", durationMs = 200, success = true, promptChars = 400, responseChars = 90, inputTokens = 100, outputTokens = 25 }),
        Line(10, new { @event = "llm_call", agent = "Analyzer", model = "m2", context = "A.cbl", durationMs = 50, success = true, promptChars = 200, responseChars = 40 }),
        Line(11, new { @event = "llm_fallback", agent = "Analyzer", context = "C.cbl", reason = "content_filter", detail = "blocked" }),
        Line(12, new { @event = "projection_metrics", agent = "Converter", file = "A.cbl", projectionMode = "projection" }),
        Line(20, new { @event = "run_finished", status = "completed", durationMs = 20000 }));

    [Fact]
    public async Task OffsetAndLegacyTimestampsAreUtcAndTimelineUsesEventTimeNotAppendOrder()
    {
        Metrics("7",
            """{"ts":"2026-01-01T12:00:00+02:00","event":"run_started","attemptId":"a"}""",
            """{"ts":"2026-01-01T10:00:03Z","event":"stage","name":"Later"}""",
            """{"ts":"2026-01-01T10:00:02","event":"llm_call","success":true}""");

        var detail = (await Reader().GetRunAsync("7"))!;

        Assert.Equal(T0, detail.Summary.StartedAt);
        Assert.Equal(T0.AddSeconds(3), detail.Summary.LastEventAt);
        Assert.All(detail.Events, e => Assert.Equal(DateTimeKind.Utc, e.Ts.Kind));
        Assert.Equal(["run_started", "llm_call", "stage"], detail.Events.Select(e => e.Kind));
        Assert.Equal(T0.AddSeconds(2), detail.Events[1].Ts);
    }

    [Fact]
    public async Task PartialAppendsAreReportedAndRetriedWithoutCachingOrLosingEarlierRecords()
    {
        Metrics("7", Line(0, new { @event = "run_started", attemptId = "a" }));
        var path = Path.Combine(_root, "output", ".metrics", "7.jsonl");
        var line = Line(2, new { @event = "llm_call", attemptId = "a", success = true });
        await File.AppendAllTextAsync(path, line[..20]);

        var partial = (await Reader().GetRunAsync("7"))!;
        Assert.Equal(1, partial.Telemetry!.UnreadableLines);
        Assert.Contains("unreadable line", partial.Warning);
        Assert.Equal(0, partial.Summary.Calls);

        await File.AppendAllTextAsync(path, line[20..] + "\n");
        var complete = (await Reader().GetRunAsync("7"))!;
        Assert.Equal(0, complete.Telemetry!.UnreadableLines);
        Assert.Equal(1, complete.Summary.Calls);
        Assert.Equal(2, complete.Telemetry.RecordedEvents);
        Assert.Equal(T0.AddSeconds(2), complete.Summary.LastEventAt);
    }

    [Fact]
    public async Task MissingTimestampsRemainExplicitAndDoNotDiscardRecordedOutcomes()
    {
        Metrics("7", Line(0, new { @event = "run_started" }),
            """{"event":"llm_call","success":true,"ts":"invalid"}""");

        var detail = (await Reader().GetRunAsync("7"))!;

        Assert.Equal(1, detail.Summary.Calls);
        Assert.Equal(2, detail.Telemetry!.RecordedEvents);
        Assert.Equal(1, detail.Telemetry.InvalidTimestamps);
        Assert.Contains("invalid timestamp", detail.Warning);
        Assert.Equal(T0, detail.Summary.LastEventAt);
    }

    [Fact]
    public async Task BoundedTimelinesReportTotalsAndPostFinishExportsAreRecordedActivityNotCalls()
    {
        Directory.CreateDirectory(Path.Combine(_root, "Config"));
        File.WriteAllText(Path.Combine(_root, "Config", "appsettings.json"),
            """{"AiLoop":{"MaxTimelineEvents":2}}""");
        Metrics("7", Line(0, new { @event = "run_started", attemptId = "a" }),
            Line(1, new { @event = "projection_metrics", attemptId = "a" }),
            Line(2, new { @event = "run_finished", status = "completed", attemptId = "a" }),
            Line(3, new { @event = "chat_log_saved", path = "Logs/chat.json", attemptId = "a" }));

        var detail = (await Reader().GetRunAsync("7"))!;

        Assert.Equal(4, detail.Telemetry!.RecordedEvents);
        Assert.Equal(3, detail.Telemetry.TimelineEvents);
        Assert.Equal(1, detail.Telemetry.ContextEvents);
        Assert.Equal(["run_finished", "chat_log_saved"], detail.Events.Select(e => e.Kind));
        Assert.Equal(T0.AddSeconds(2), detail.Summary.FinishedAt);
        Assert.Equal(T0.AddSeconds(3), detail.Summary.LastEventAt);
        Assert.Equal(0, detail.Summary.Calls);
    }

    [Fact]
    public async Task RunsWithoutLoopEventsAreNotListed()
    {
        ACompletedRun();
        Metrics("6", Line(0, new { @event = "projection_metrics", agent = "X" }));

        var runs = await Reader().ListRunsAsync();

        var run = Assert.Single(runs);
        Assert.Equal("7", run.RunId);
        Assert.Equal("completed", run.Status);
        Assert.Equal("C#", run.TargetLanguage);
        Assert.Equal(4, run.Calls);
        Assert.Equal(1, run.FailedCalls);
        Assert.Equal(1, run.Retries);
        Assert.Equal(1, run.Fallbacks);
        Assert.Equal(20000, run.DurationMs);
    }

    [Fact]
    public async Task AnUnfinishedRunIsRunningUntilItGoesQuiet()
    {
        Metrics("8", Line(0, new { @event = "run_started", mode = "chunked", targetLanguage = "Java", outputFolder = RunFolder }),
            Line(1, new { @event = "stage", number = 3, total = 6, name = "Small File Conversion" }));
        var path = Path.Combine(_root, "output", ".metrics", "8.jsonl");

        Assert.Equal("running", Assert.Single(await Reader().ListRunsAsync()).Status);
        Assert.Equal("Small File Conversion", (await Reader().GetRunAsync("8"))!.Summary.CurrentStage);

        File.SetLastWriteTimeUtc(path, DateTime.UtcNow.AddHours(-2));
        Assert.Equal("interrupted", Assert.Single(await Reader().ListRunsAsync()).Status);
    }

    [Fact]
    public async Task AKilledRunTakesItsEndedStatusFromTheRunDatabase()
    {
        // Killed before run_finished, and its successor's start marked it terminated.
        Metrics("8", Line(0, new { @event = "run_started", mode = "standard", targetLanguage = "Java", outputFolder = RunFolder }));
        Metrics("9", Line(0, new { @event = "run_started", mode = "standard", targetLanguage = "Java", outputFolder = RunFolder }));
        Directory.CreateDirectory(Path.Combine(_root, "Data"));
        var db = Path.Combine(_root, "Data", "migration.db");
        await new SqliteMigrationRepository(db, NullLogger<SqliteMigrationRepository>.Instance).InitializeAsync();
        using (var connection = new Microsoft.Data.Sqlite.SqliteConnection($"Data Source={db};Pooling=False"))
        {
            connection.Open();
            using var command = connection.CreateCommand();
            command.CommandText = "INSERT INTO runs (id, started_at, status, completed_at) VALUES"
                                  + " (8, '2026-01-01', 'Terminated', '2026-01-01T11:00:00Z'), (9, '2026-01-01', 'Running', NULL);";
            command.ExecuteNonQuery();
        }

        var runs = (await Reader().ListRunsAsync()).ToDictionary(r => r.RunId, r => r.Status);

        Assert.Equal("terminated", runs["8"]);
        Assert.Equal("running", runs["9"]);
        Assert.Equal("terminated", (await Reader().GetRunAsync("8"))!.Summary.Status);
        Assert.Equal(T0.AddHours(1), (await Reader().GetRunAsync("8"))!.Summary.FinishedAt);
        Assert.Equal(3600000, (await Reader().GetRunAsync("8"))!.Summary.DurationMs);
    }

    [Fact]
    public async Task AgentsAreSummarisedFromTheirCalls()
    {
        ACompletedRun();

        var detail = await Reader().GetRunAsync("7");

        Assert.NotNull(detail);
        var converter = detail!.Agents.First();
        Assert.Equal("Converter", converter.Agent);
        Assert.Equal(["m1"], converter.Models);
        Assert.Equal(3, converter.Calls);
        Assert.Equal(1, converter.FailedCalls);
        Assert.Equal(1, converter.Retries);
        Assert.Equal(200, converter.AvgMs);
        Assert.Equal(300, converter.P95Ms);
        Assert.Equal(200, converter.InputTokens);
        Assert.Equal(45, converter.OutputTokens);

        var analyzer = detail.Agents.Single(a => a.Agent == "Analyzer");
        Assert.Equal(1, analyzer.Fallbacks);
        Assert.Null(analyzer.InputTokens);
    }

    [Fact]
    public async Task StagesLastUntilTheNextOneOrTheEndOfTheRun()
    {
        ACompletedRun();

        var stages = (await Reader().GetRunAsync("7"))!.Stages;

        Assert.Equal(["File Discovery", "Dependency Analysis"], stages.Select(s => s.Name));
        Assert.Equal(3000, stages[0].DurationMs);
        Assert.Equal(16000, stages[1].DurationMs);
    }

    [Fact]
    public async Task TheTimelineHoldsLifecycleSuccessfulAndFailedCallsInOrderWithoutErrorContent()
    {
        ACompletedRun();

        var events = (await Reader().GetRunAsync("7"))!.Events;

        Assert.Equal(["run_started", "stage", "stage", "llm_call", "llm_call", "llm_retry", "llm_call",
            "llm_call", "llm_fallback", "run_finished"], events.Select(e => e.Kind));
        Assert.Equal("succeeded", events[3].Outcome);
        Assert.Equal("failed", events[4].Reason);
        Assert.Equal("transient_error", events[5].Reason);
        Assert.Equal("content_filter", events[8].Reason);
    }

    [Fact]
    public async Task AFallbackWarnsOnTheModelGateUnlessConfigAllowsIt()
    {
        ACompletedRun();
        Assert.Equal("warn", Gate(await Reader().GetRunAsync("7"), "model").Status);

        Directory.CreateDirectory(Path.Combine(_root, "Config"));
        File.WriteAllText(Path.Combine(_root, "Config", "appsettings.json"),
            """{ "AiLoop": { "MaxFallbacks": 1, "MaxFailedCallRate": 0.5 } }""");
        Assert.Equal("pass", Gate(await Reader().GetRunAsync("7"), "model").Status);
        Assert.Contains("1 fallback(s)", Gate(await Reader().GetRunAsync("7"), "model").Headline);
    }

    [Fact]
    public async Task GatesThatDidNotRunSaySo()
    {
        ACompletedRun();

        var detail = await Reader().GetRunAsync("7");

        Assert.Equal("not-run", Gate(detail, "compile").Status);
        Assert.Equal("not-run", Gate(detail, "parity").Status);
        Assert.Equal("not-run", Gate(detail, "jcl").Status);
    }

    [Fact]
    public async Task TheCompileGateReadsCompileStatus()
    {
        ACompletedRun();
        Artifact(CSharpCompileGate.StatusFile, JsonSerializer.Serialize(new CompileGateResult(true, true, null,
            [new CompileRound(0, 3, 2, 0, new Dictionary<string, int>()), new CompileRound(1, 0, 0, 0, new Dictionary<string, int>())], [], [])));
        var pass = Gate(await Reader().GetRunAsync("7"), "compile");
        Assert.Equal("pass", pass.Status);
        Assert.Equal("Compiles after 1 repair round(s)", pass.Headline);
        Assert.Equal($"{RunFolder}/compile-status.json", pass.Source);

        Artifact(CSharpCompileGate.StatusFile, JsonSerializer.Serialize(new CompileGateResult(false, true, "did not converge",
            [new CompileRound(0, 2, 0, 0, new Dictionary<string, int>())],
            [new CompilerDiagnostic("A.cs", 3, 1, "CS0103", "x"), new CompilerDiagnostic("B.cs", 4, 1, "CS0103", "y")], [])));
        var fail = Gate(await Reader().GetRunAsync("7"), "compile");
        Assert.Equal("fail", fail.Status);
        Assert.Contains("2 error(s) remain", fail.Headline);
        Assert.Contains(fail.Details, d => d.Contains("CS0103"));

        Artifact(CSharpCompileGate.StatusFile, JsonSerializer.Serialize(new CompileGateResult(false, true, "project file missing", [], [], [])));
        Assert.Equal("project file missing", Gate(await Reader().GetRunAsync("7"), "compile").Headline);
    }

    [Theory]
    [InlineData(true, true, "pass", "Java")]
    [InlineData(false, true, "fail", "Java")]
    [InlineData(false, false, "warn", "Java")]
    [InlineData(true, true, "pass", "JAVA QUARKUS")]
    [InlineData(false, true, "fail", "Java Quarkus")]
    [InlineData(false, false, "warn", "Java Quarkus")]
    [InlineData(true, true, "pass", "Java Spring Boot")]
    [InlineData(false, true, "fail", "Java Spring Boot")]
    [InlineData(false, false, "warn", "JAVA SPRING BOOT")]
    public async Task JavaCompileGateUsesStructuredMeasuredOutcomes(bool compiled, bool measured, string expected, string language)
    {
        ACompletedRun(targetLanguage: language);
        Artifact("java-compile-status.json", JsonSerializer.Serialize(new
        {
            Compiled = compiled,
            Measured = measured,
            ExitCode = compiled ? 0 : 1,
            FailureReason = measured ? null : "build tool unavailable",
            RepairRounds = 1,
            Errors = compiled ? Array.Empty<object>() : [new { File = "A.java", Line = 7, Code = "javac", Message = "private source snippet" }],
        }));

        var gate = Gate(await Reader().GetRunAsync("7"), "compile");

        Assert.Equal(expected, gate.Status);
        Assert.Equal($"{RunFolder}/java-compile-status.json", gate.Source);
        Assert.DoesNotContain(gate.Details, x => x.Contains("private source"));
        if (compiled) Assert.Equal("Compiles after 1 repair round(s)", gate.Headline);
        else if (!measured) Assert.Equal("Not measured: build tool unavailable", gate.Headline);
        else
        {
            Assert.Equal("1 compiler error(s) remain", gate.Headline);
            Assert.Contains("A.java:7 javac", gate.Details);
        }
    }

    [Theory]
    [InlineData("warn", "warn")]
    [InlineData("fail", "fail")]
    public async Task ParityFailuresFollowTheReportsPolicy(string onLowScore, string expected)
    {
        ACompletedRun();
        Artifact(ConversionParityPostPass.ArtifactName, JsonSerializer.Serialize(new ConversionParityReport
        {
            Threshold = 0.8,
            OnLowScore = onLowScore,
            Programs =
            [
                new ProgramParityResult { Program = "A.cbl", Outcome = ParityOutcome.Evaluated, Score = 0.9 },
                new ProgramParityResult { Program = "B.cbl", Outcome = ParityOutcome.Evaluated, Score = 0.5, Failed = true },
            ],
        }));

        var gate = Gate(await Reader().GetRunAsync("7"), "parity");

        Assert.Equal(expected, gate.Status);
        Assert.StartsWith("1/2 at or above 0.80", gate.Headline);
        Assert.Contains(gate.Details, d => d.StartsWith("B.cbl: 0.50"));
    }

    [Fact]
    public async Task ParityPassesWhenEveryProgramIsEvaluatedAndAboveThreshold()
    {
        ACompletedRun();
        Artifact(ConversionParityPostPass.ArtifactName, JsonSerializer.Serialize(new ConversionParityReport
        {
            Threshold = 0.8,
            Programs = [new ProgramParityResult { Program = "A.cbl", Outcome = ParityOutcome.Evaluated, Score = 0.95 }],
        }));

        Assert.Equal("pass", Gate(await Reader().GetRunAsync("7"), "parity").Status);
    }

    [Fact]
    public async Task JclJobsThatCannotRunEndToEndWarn()
    {
        ACompletedRun();
        Artifact(JclJobWriter.ManifestFile, JsonSerializer.Serialize(new[]
        {
            new JclJobManifestEntry("JOBA", "Jobs/JobA.cs", "JobA", ["A"], ["A"], [], [], []),
            new JclJobManifestEntry("JOBB", "Jobs/JobB.cs", "JobB", ["B", "Z"], ["B"], ["Z"], [], []),
        }, JclEstate.JsonOptions));

        var gate = Gate(await Reader().GetRunAsync("7"), "jcl");

        Assert.Equal("warn", gate.Status);
        Assert.Equal("1/2 job(s) can run end to end", gate.Headline);
        Assert.Contains("JOBB: needs Z", gate.Details);
    }

    [Theory]
    [InlineData("source")]
    [InlineData("../elsewhere")]
    [InlineData("output/../Config")]
    public async Task AnOutputFolderOutsideOutputIsNotRead(string folder)
    {
        ACompletedRun(folder);

        var detail = await Reader().GetRunAsync("7");

        var compile = Gate(detail, "compile");
        Assert.Equal("not-run", compile.Status);
        Assert.Contains("outside", compile.Headline);
    }

    [Fact]
    public async Task AnAbsoluteOutputFolderInsideOutputIsRead()
    {
        ACompletedRun(Path.Combine(_root, RunFolder));
        Artifact(CSharpCompileGate.StatusFile, JsonSerializer.Serialize(new CompileGateResult(true, true, null, [], [], [])));

        Assert.Equal("pass", Gate(await Reader().GetRunAsync("7"), "compile").Status);
    }

    [Fact]
    public async Task AHalfWrittenLastLineIsSkipped()
    {
        ACompletedRun();
        File.AppendAllText(Path.Combine(_root, "output", ".metrics", "7.jsonl"), "{\"ts\":\"2026-01-01T10:00:30Z\",\"event\":\"llm_ca");

        var detail = await Reader().GetRunAsync("7");

        Assert.Equal(4, detail!.Summary.Calls);
    }

    [Theory]
    [InlineData("../7")]
    [InlineData("7.jsonl")]
    [InlineData("")]
    [InlineData("a/b")]
    public async Task UnsafeRunIdsAreRejected(string runId)
    {
        ACompletedRun();

        Assert.False(AiLoopReader.IsValidRunId(runId));
        Assert.Null(await Reader().GetRunAsync(runId));
    }

    [Fact]
    public async Task AnUnknownRunIsNull() => Assert.Null(await Reader().GetRunAsync("404"));

    [Fact]
    public async Task NoMetricsFolderMeansNoRuns()
    {
        Directory.Delete(Path.Combine(_root, "output", ".metrics"));

        Assert.Empty(await Reader().ListRunsAsync());
    }

    [Fact]
    public async Task AResumedAttemptDoesNotInheritThePreviousFinishCallsOrGateArtifacts()
    {
        ACompletedRun();
        Artifact(CSharpCompileGate.StatusFile, JsonSerializer.Serialize(new CompileGateResult(true, true, null, [], [], [])));
        File.SetLastWriteTimeUtc(Path.Combine(_root, RunFolder, CSharpCompileGate.StatusFile), T0.AddSeconds(19));
        File.AppendAllLines(Path.Combine(_root, AiLoopReader.MetricsFolder, "7.jsonl"),
        [
            Line(30, new { @event = "run_started", mode = "resume", attemptId = "second", telemetryVersion = 2, outputFolder = RunFolder }),
            Line(31, new { @event = "llm_request_started", attemptId = "second", requestId = "new", agent = "Converter", context = "B.cbl" }),
            Line(32, new { @event = "run_finished", attemptId = "first", status = "failed" }),
        ]);

        var detail = (await Reader().GetRunAsync("7"))!;

        Assert.Equal("running", detail.Summary.Status);
        Assert.Equal("second", detail.Summary.AttemptId);
        Assert.Null(detail.Summary.FinishedAt);
        Assert.Equal(0, detail.Summary.Calls);
        Assert.Equal(1, detail.Summary.StartedRequests);
        Assert.Equal("in-flight", Assert.Single(detail.Requests).Status);
        Assert.Empty(detail.Stages);
        Assert.Empty(detail.ContextHandoffs);
        Assert.Equal("not-run", Gate(detail, "compile").Status);
        Assert.Contains("predates", Gate(detail, "compile").Headline);
    }

    [Fact]
    public async Task LegacyStartsAreUnavailableWhereasInstrumentedZeroIsKnown()
    {
        ACompletedRun();
        var old = (await Reader().GetRunAsync("7"))!;
        Assert.Null(old.Summary.StartedRequests);
        Assert.Equal("legacy-outcomes-only", old.Summary.RequestCoverage);
        Assert.Null(old.Agents.First().StartedRequests);

        Metrics("8", Line(0, new { @event = "run_started", telemetryVersion = 2, attemptId = "zero", managedRunId = "portal-8" }));
        var current = (await Reader().GetRunAsync("8"))!;
        Assert.Equal(0, current.Summary.StartedRequests);
        Assert.Equal("agent-boundary", current.Summary.RequestCoverage);
        Assert.Equal("portal-8", current.Summary.ManagedRunId);
    }

    [Fact]
    public async Task ManagedIdentityDoesNotGuessNumericIdsOrInheritAResumedAttempt()
    {
        Metrics("7",
            Line(0, new { @event = "run_started", attemptId = "old", managedRunId = "portal-old" }),
            Line(1, new { @event = "run_finished", attemptId = "old", status = "completed" }),
            Line(2, new { @event = "run_started", attemptId = "new", managedRunId = "portal-new" }));

        Assert.Null(await Reader().GetManagedRunAsync("7"));
        var previous = (await Reader().GetManagedRunAsync("portal-old"))!;
        Assert.Equal("old", previous.Summary.AttemptId);
        Assert.Equal("completed", previous.Summary.Status);
        var current = (await Reader().GetManagedRunAsync("portal-new"))!;
        Assert.Equal("7", current.Summary.RunId);
        Assert.Equal("new", current.Summary.AttemptId);
        Assert.Equal("running", current.Summary.Status);
        Assert.Equal(0, current.Summary.Calls);
    }

    [Fact]
    public async Task OlderAttemptsRemainReadableWithoutInventingMissingAttemptIdsOrReusingLaterArtifacts()
    {
        ACompletedRun();
        File.AppendAllLines(Path.Combine(_root, AiLoopReader.MetricsFolder, "7.jsonl"),
        [
            Line(30, new { @event = "run_started", attemptId = "new", outputFolder = RunFolder }),
            Line(31, new { @event = "llm_call", attemptId = "new", success = true }),
        ]);
        Artifact(CSharpCompileGate.StatusFile, JsonSerializer.Serialize(new CompileGateResult(true, true, null, [], [], [])));

        var summaries = await Reader().ListRunsAsync();
        Assert.Equal(2, summaries.Count);
        var previous = (await Reader().GetRunAsync("7", attemptIndex: 0))!;
        Assert.Null(previous.Summary.AttemptId);
        Assert.Equal(0, previous.Summary.AttemptIndex);
        Assert.Equal("completed", previous.Summary.Status);
        Assert.Equal(4, previous.Summary.Calls);
        Assert.Equal("unknown", Gate(previous, "compile").Status);
        Assert.Contains("snapshot", Gate(previous, "compile").Headline);
        Assert.Equal(1, (await Reader().GetRunAsync("7"))!.Summary.Calls);
        Assert.Null(await Reader().GetRunAsync("7", attemptId: "absent"));
    }

    [Fact]
    public async Task DatabaseOnlyRunsArePagedAndExposeMissingTelemetryInsteadOfFakeActivity()
    {
        Directory.CreateDirectory(Path.Combine(_root, "Data"));
        using (var db = new Microsoft.Data.Sqlite.SqliteConnection($"Data Source={Path.Combine(_root, "Data", "migration.db")}"))
        {
            db.Open();
            using var command = db.CreateCommand();
            command.CommandText = """
                CREATE TABLE runs(id INTEGER PRIMARY KEY, status TEXT, started_at TEXT, completed_at TEXT);
                INSERT INTO runs VALUES (1,'Completed','2025-01-01','2025-01-02'),
                    (2,'Failed','2025-02-01','2025-02-02'), (3,'Terminated','2025-03-01','2025-03-02');
                """;
            command.ExecuteNonQuery();
        }
        Metrics("2", Line(0, new { @event = "projection_metrics" }));
        Metrics("1");

        Assert.Equal("3", Assert.Single(await Reader().ListRunsAsync(offset: 0, limit: 1)).RunId);
        Assert.Equal("2", Assert.Single(await Reader().ListRunsAsync(offset: 1, limit: 1)).RunId);
        Assert.Equal("1", Assert.Single(await Reader().ListRunsAsync(offset: 2, limit: 1)).RunId);
        var detail = (await Reader().GetRunAsync("2"))!;
        Assert.False(detail.Summary.TelemetryAvailable);
        Assert.Equal("failed", detail.Summary.Status);
        Assert.Null(detail.Summary.StartedRequests);
        Assert.Null(detail.OutputFolder);
        Assert.Empty(detail.Gates);
        Assert.Empty(detail.Events);
        Assert.Contains("cannot be reconstructed", detail.Warning);
        Assert.False((await Reader().GetRunAsync("1"))!.Summary.TelemetryAvailable);
    }

    [Fact]
    public async Task RecordedValidationRejectionExplainsFailureWithoutReclassifyingSuccessfulModelCalls()
    {
        Metrics("7",
            Line(0, new { @event = "run_started", attemptId = "a", telemetryVersion = 2 }),
            Line(1, new { @event = "llm_call", attemptId = "a", success = true }),
            Line(2, new { @event = "llm_retry", attemptId = "a", attempt = 2, reason = "transient_error" }),
            Line(3, new { @event = "validation_completed", accepted = false, outputComplete = false,
                compileEnabled = true, compileMeasured = true, compiled = true, strictParityGate = false }),
            Line(4, new { @event = "run_finished", status = "failed", errorType = "ValidationFailed", reason = "run_error" }));

        var detail = (await Reader().GetRunAsync("7"))!;
        Assert.Equal(0, detail.Summary.FailedCalls);
        Assert.Equal(1, detail.Summary.Calls);
        Assert.Contains("validation rejected", detail.Summary.FailureReason);
        Assert.Equal("pass", Gate(detail, "model").Status);
        Assert.Equal("fail", Gate(detail, "acceptance").Status);
        Assert.Contains(Gate(detail, "acceptance").Details, line => line.Contains("compiled: true"));
        Assert.Equal(2, detail.Events.Single(e => e.Kind == "llm_retry").Iteration);
        Assert.Contains(detail.Events, e => e.Kind == "validation_completed");
    }

    [Fact]
    public async Task ConversationExportReferencesFollowRecordedAttemptCorrelationEvenWhenExportedAfterAResume()
    {
        Directory.CreateDirectory(Path.Combine(_root, "Logs"));
        File.WriteAllText(Path.Combine(_root, "Logs", "current.json"), "[{\"content\":\"private prompt\"}]");
        Metrics("7",
            Line(0, new { @event = "run_started", attemptId = "old", managedRunId = "portal-old" }),
            Line(1, new { @event = "run_finished", attemptId = "old", status = "completed" }),
            Line(2, new { @event = "run_started", attemptId = "current", managedRunId = "portal-current" }),
            Line(3, new { @event = "chat_log_saved", attemptId = "old", path = "Logs/missing.json", messageCount = 2 }),
            Line(4, new { @event = "chat_log_saved", attemptId = "current", path = "Logs/current.json", messageCount = 4 }),
            Line(5, new { @event = "chat_log_saved", attemptId = "current", path = "../outside.json", messageCount = 100 }));

        var previous = (await Reader().GetManagedRunAsync("portal-old"))!;
        var oldLog = Assert.Single(previous.Logs!.Where(log => log.Kind == "Correlated model conversation export"));
        Assert.Equal("Logs/missing.json", oldLog.Path);
        Assert.False(oldLog.Available);
        Assert.Equal(2, oldLog.Messages);
        var current = (await Reader().GetRunAsync("7"))!;
        var log = Assert.Single(current.Logs!.Where(log => log.Kind == "Correlated model conversation export"));
        Assert.True(log.Available);
        Assert.Equal(4, log.Messages);
        Assert.DoesNotContain("private prompt", JsonSerializer.Serialize(current));
    }

    [Fact]
    public async Task AManagedLaunchShowsEveryExplicitlyAssociatedPersistedMigrationRun()
    {
        Metrics("7",
            Line(0, new { @event = "run_started", attemptId = "analysis", managedRunId = "portal" }),
            Line(1, new { @event = "run_finished", attemptId = "analysis", status = "completed" }));
        Metrics("8",
            Line(2, new { @event = "run_started", attemptId = "conversion", managedRunId = "portal" }),
            Line(3, new { @event = "run_finished", attemptId = "conversion", status = "failed" }));
        Metrics("9", Line(4, new { @event = "run_started", attemptId = "unrelated", managedRunId = "other" }));

        var detail = (await Reader().GetManagedRunAsync("portal"))!;

        Assert.Equal("8", detail.Summary.RunId);
        Assert.Equal(new[] { "8", "7" }, detail.RelatedAttempts!.Select(attempt => attempt.RunId));
        Assert.DoesNotContain(detail.RelatedAttempts, attempt => attempt.RunId == "9");
    }

    [Fact]
    public async Task ManagedResolutionIsNotLimitedByThePickerAndCacheMetricsDoNotHideRuns()
    {
        Directory.CreateDirectory(Path.Combine(_root, "Config"));
        File.WriteAllText(Path.Combine(_root, "Config", "appsettings.json"), """{"AiLoop":{"MaxRuns":1}}""");
        Metrics("7", Line(0, new { @event = "run_started", managedRunId = "portal-old" }));
        Metrics("8", Line(10, new { @event = "run_started", managedRunId = "portal-new" }));
        Metrics("cache", Line(20, new { @event = "projection_metrics" }));
        File.SetLastWriteTimeUtc(Path.Combine(_root, AiLoopReader.MetricsFolder, "cache.jsonl"), DateTime.UtcNow.AddMinutes(1));

        Assert.Equal("8", Assert.Single(await Reader().ListRunsAsync()).RunId);
        Assert.Equal("7", (await Reader().GetManagedRunAsync("portal-old"))!.Summary.RunId);
    }

    [Fact]
    public async Task AnUnmeasuredCompileArtifactCannotEstablishSuccess()
    {
        ACompletedRun();
        Artifact(CSharpCompileGate.StatusFile,
            JsonSerializer.Serialize(new CompileGateResult(true, false, "No compiler evidence", [], [], [])));

        Assert.Equal("warn", Gate(await Reader().GetRunAsync("7"), "compile").Status);
        Assert.Contains("Not measured", Gate(await Reader().GetRunAsync("7"), "compile").Headline);
    }

    [Fact]
    public async Task UnmatchedFinishedRequestsAreUnknownAndCancelledCallsAreNotFailures()
    {
        Metrics("7",
            Line(0, new { @event = "run_started", telemetryVersion = 2, attemptId = "a" }),
            Line(1, new { @event = "llm_request_started", requestId = "cancelled", agent = "A" }),
            Line(2, new { @event = "llm_request_started", requestId = "unfinished", agent = "A" }),
            Line(3, new { @event = "llm_call", requestId = "cancelled", agent = "A", success = false, outcome = "cancelled" }),
            Line(4, new { @event = "llm_call", requestId = "unknown", agent = "A" }),
            Line(5, new { @event = "run_finished", status = "cancelled" }));

        var detail = (await Reader().GetRunAsync("7"))!;

        Assert.Equal("cancelled", detail.Summary.Status);
        Assert.Equal(0, detail.Summary.FailedCalls);
        Assert.Equal(1, detail.Summary.CancelledCalls);
        Assert.Equal(1, detail.Summary.UnknownCalls);
        var unmatched = Assert.Single(detail.Requests);
        Assert.Equal("unfinished", unmatched.RequestId);
        Assert.Equal("unknown", unmatched.Status);
        Assert.Equal(1, Assert.Single(detail.Agents).CancelledCalls);
        Assert.Equal("warn", Gate(detail, "model").Status);
    }

    [Theory]
    [InlineData("failed")]
    [InlineData("cancelled")]
    [InlineData("unknown")]
    public async Task TerminalStatusesRemainDistinct(string status)
    {
        Metrics("7", Line(0, new { @event = "run_started" }), Line(1, new { @event = "run_finished", status }));

        Assert.Equal(status, (await Reader().GetRunAsync("7"))!.Summary.Status);
    }

    [Fact]
    public async Task PreparedReusedFactsAreNotAnExecutedParserOrDispatchedRequest()
    {
        ACompletedRun();
        var prepared = Assert.Single((await Reader().GetRunAsync("7"))!.ContextHandoffs);
        Assert.Equal("prepared", prepared.State);
        Assert.Equal("reused", prepared.Execution);
        Assert.Null(prepared.RequestId);

        File.AppendAllLines(Path.Combine(_root, AiLoopReader.MetricsFolder, "7.jsonl"),
        [
            Line(21, new { @event = "context_dispatched", agent = "Converter", file = "A.cbl", projectionMode = "projection",
                requestId = "request-a", contextId = "context-a" }),
        ]);
        var dispatched = (await Reader().GetRunAsync("7"))!.ContextHandoffs.Last();
        Assert.Equal("dispatched", dispatched.State);
        Assert.Equal("reused", dispatched.Execution);
        Assert.Equal("request-a", dispatched.RequestId);
        Assert.Equal("context-a", dispatched.ContextId);
    }

    [Fact]
    public async Task ValidJsonWithTheWrongShapeDoesNotBreakTheRunReader()
    {
        ACompletedRun();
        File.AppendAllLines(Path.Combine(_root, AiLoopReader.MetricsFolder, "7.jsonl"),
            ["null", "[]", "1", """{"ts":123,"event":4}"""]);

        Assert.Equal(4, (await Reader().GetRunAsync("7"))!.Summary.Calls);
    }

    [Fact]
    public async Task ACompletedAttemptWithAnUnmatchedStartCannotPassTheModelGate()
    {
        Metrics("7",
            Line(0, new { @event = "run_started", telemetryVersion = 2 }),
            Line(1, new { @event = "llm_request_started", requestId = "done", agent = "A" }),
            Line(2, new { @event = "llm_request_started", requestId = "missing", agent = "A" }),
            Line(3, new { @event = "llm_call", requestId = "done", agent = "A", success = true }),
            Line(4, new { @event = "run_finished", status = "completed" }));

        var detail = (await Reader().GetRunAsync("7"))!;

        Assert.Equal(1, detail.Summary.OutstandingRequests);
        Assert.Equal("warn", Gate(detail, "model").Status);
        Assert.Equal("unknown", Assert.Single(detail.Requests).Status);
        Assert.Null(Assert.Single(detail.Agents).AvgMs);
        Assert.Null(Assert.Single(detail.Agents).InputTokens);
    }

    [Fact]
    public async Task ACompletedAttemptWithNoRequestOutcomesWarnsRatherThanClaimingNoRequestsRan()
    {
        Metrics("7",
            Line(0, new { @event = "run_started", telemetryVersion = 2 }),
            Line(1, new { @event = "llm_request_started", requestId = "unfinished", agent = "A" }),
            Line(2, new { @event = "run_finished", status = "completed" }));

        var detail = (await Reader().GetRunAsync("7"))!;
        Assert.Equal("warn", Gate(detail, "model").Status);
        Assert.Equal("unknown", Assert.Single(detail.Requests).Status);
        Assert.Equal(0, detail.Summary.Calls);
        Assert.Equal(1, detail.Summary.OutstandingRequests);
    }

    private static AiLoopGate Gate(AiLoopRunDetail? detail, string id)
    {
        Assert.NotNull(detail);
        return detail!.Gates.Single(g => g.Id == id);
    }
}

public class AiLoopEndpointsTests : IAsyncLifetime
{
    private readonly string _root = Path.Combine(Directory.GetCurrentDirectory(), ".ai-loop-endpoints-" + Guid.NewGuid().ToString("N"));
    private WebApplication _app = null!;
    private ProcessManager _manager = null!;

    public async Task InitializeAsync()
    {
        Directory.CreateDirectory(Path.Combine(_root, "source"));
        Directory.CreateDirectory(Path.Combine(_root, AiLoopReader.MetricsFolder));
        File.WriteAllText(Path.Combine(_root, AiLoopReader.MetricsFolder, "7.jsonl"),
            """{"ts":"2026-01-01T10:00:00Z","event":"run_started","attemptId":"a","managedRunId":"portal-7","telemetryVersion":2,"targetLanguage":"Java"}""");
        var child = Path.Combine(_root, "isolated-child.sh");
        File.WriteAllText(child, """
            #!/bin/sh
            if [ "$TEST_AI_LOOP_WRITE" = "1" ]; then
              printf '{"ts":"2026-01-01T10:00:00Z","event":"run_started","managedRunId":"%s","telemetryVersion":2}\n' "$AI_LOOP_MANAGED_RUN_ID" > output/.metrics/9.jsonl
              printf '{"ts":"2026-01-01T10:00:01Z","event":"llm_request_started","requestId":"unfinished","agent":"Test"}\n' >> output/.metrics/9.jsonl
            fi
            printf 'isolated startup diagnostic\n' >&2
            exit "${TEST_EXIT_CODE:-23}"
            """);
        if (!OperatingSystem.IsWindows())
            File.SetUnixFileMode(child, UnixFileMode.UserRead | UnixFileMode.UserWrite | UnixFileMode.UserExecute);
        _manager = new ProcessManager(_root, child);
        var builder = WebApplication.CreateBuilder(new WebApplicationOptions { ContentRootPath = _root, EnvironmentName = "Testing" });
        builder.WebHost.UseTestServer();
        builder.Services.AddSingleton(new AiLoopReader(NullLogger<AiLoopReader>.Instance, _root));
        builder.Services.AddSingleton(_manager);
        _app = builder.Build();
        _app.MapAiLoopEndpoints();
        await _app.StartAsync();
    }

    public async Task DisposeAsync()
    {
        await _app.DisposeAsync();
        Directory.Delete(_root, recursive: true);
    }

    [Fact]
    public async Task NativeLogRouteReturnsNoStoreBoundedTextAndExplicitErrorsOnly()
    {
        var folder = Path.Combine(_root, "output", "rekt-scans", "route-log");
        Directory.CreateDirectory(folder);
        await File.WriteAllTextAsync(Path.Combine(folder, "scan.json"), JsonSerializer.Serialize(new
        {
            schemaVersion = 1, activityKind = "native-rekt", name = "route-log",
            startedAt = DateTime.UtcNow, status = "running", logPath = "native.log", programs = Array.Empty<object>()
        }));
        var client = _app.GetTestClient();
        var catalogue = await client.GetFromJsonAsync<JsonElement>("/api/ai-loop/runs");
        var locator = catalogue.GetProperty("runs").EnumerateArray().First().GetProperty("runId").GetString();
        var url = "/api/ai-loop/" + locator + "/native-log";
        var missing = await client.GetAsync(url);
        Assert.Equal(HttpStatusCode.NotFound, missing.StatusCode);
        Assert.Contains("missing", await missing.Content.ReadAsStringAsync());
        await File.WriteAllTextAsync(Path.Combine(folder, "native.log"), "original native output");
        var response = await client.GetAsync(url);
        Assert.Equal(HttpStatusCode.OK, response.StatusCode);
        Assert.Equal("no-store", response.Headers.CacheControl!.ToString());
        var payload = await response.Content.ReadFromJsonAsync<JsonElement>();
        Assert.Equal("original native output", payload.GetProperty("text").GetString());
        Assert.Equal(locator, payload.GetProperty("runId").GetString());
        Assert.Equal(HttpStatusCode.BadRequest, (await client.GetAsync("/api/ai-loop/7/native-log")).StatusCode);
    }

    [Fact]
    public async Task NativeCatalogueAndDetailKeepManifestIdentitySeparateFromModelAndGraphIds()
    {
        var folder = Path.Combine(_root, "output", "rekt-scans", "doctor-native");
        Directory.CreateDirectory(folder);
        await File.WriteAllTextAsync(Path.Combine(folder, "scan.json"), JsonSerializer.Serialize(new
        {
            schemaVersion = 1, activityKind = "native-rekt", name = "doctor-native",
            startedAt = DateTime.UtcNow.AddMinutes(-1), finishedAt = DateTime.UtcNow,
            status = "cancelled", programs = new[] { new { relativePath = "A.cbl", status = "running" } }
        }));
        var client = _app.GetTestClient();
        var payload = await client.GetFromJsonAsync<JsonElement>("/api/ai-loop/runs");
        var summary = payload.GetProperty("runs").EnumerateArray().First();
        Assert.Equal("native-rekt", summary.GetProperty("activityKind").GetString());
        Assert.Equal("native:doctor-native", summary.GetProperty("activityId").GetString());
        var locator = summary.GetProperty("runId").GetString();
        var response = await client.GetAsync("/api/ai-loop/" + locator);
        Assert.Equal("no-store", response.Headers.CacheControl!.ToString());
        var detail = await response.Content.ReadFromJsonAsync<JsonElement>();
        Assert.Equal("cancelled", detail.GetProperty("summary").GetProperty("status").GetString());
        Assert.Equal("running", detail.GetProperty("native").GetProperty("programs")[0].GetProperty("status").GetString());
        Assert.Empty(detail.GetProperty("agents").EnumerateArray());
        Assert.Null(detail.GetProperty("native").GetProperty("graphSnapshotId").GetString());
        Assert.Equal(HttpStatusCode.NotFound, (await client.GetAsync("/api/ai-loop/" + locator + "?attemptId=fake")).StatusCode);
    }

    [Fact]
    public async Task DraftExitZeroPersistsPartialManagedHistoryWithoutInventingModelTelemetry()
    {
        if (OperatingSystem.IsWindows()) return;
        var run = _manager.StartRun("convert-only", "draft fixture", extraEnv: new()
        { ["CONVERSION_DEPENDENCY_MODE"] = "draft", ["TEST_EXIT_CODE"] = "0" });
        var historyPath = Path.Combine(_root, "Logs", "portal-history", run.RunId + ".json");
        for (var i = 0; i < 100; i++)
        {
            if (File.ReadAllText(historyPath).Contains("\"Status\":\"partial\"")) break;
            await Task.Delay(20);
        }
        Assert.Equal("partial", run.Status);
        Assert.Equal("draft", run.DependencyMode);
        var recovered = new ProcessManager(_root).GetRun(run.RunId)!;
        Assert.Equal("partial", recovered.Status);
        Assert.Equal("draft", recovered.DependencyMode);
        var payload = await _app.GetTestClient().GetFromJsonAsync<JsonElement>("/api/ai-loop/runs");
        var managed = payload.GetProperty("managedRuns").EnumerateArray().Single(r => r.GetProperty("runId").GetString() == run.RunId);
        Assert.Equal("partial", managed.GetProperty("status").GetString());
        Assert.Equal("managed-launch", managed.GetProperty("activityKind").GetString());
        Assert.Equal("draft", managed.GetProperty("dependencyMode").GetString());
        Assert.DoesNotContain(payload.GetProperty("runs").EnumerateArray(),
            r => r.GetProperty("managedRunId").GetString() == run.RunId);
    }

    [Fact]
    public async Task RunsAreListedWithThePollInterval()
    {
        var payload = await _app.GetTestClient().GetFromJsonAsync<JsonElement>("/api/ai-loop/runs");

        Assert.Equal(JsonValueKind.Array, payload.GetProperty("runs").ValueKind);
        Assert.True(payload.GetProperty("pollSeconds").GetInt32() >= 1);
        var summary = Assert.Single(payload.GetProperty("runs").EnumerateArray());
        Assert.Equal("7", summary.GetProperty("runId").GetString());
        Assert.Equal("a", summary.GetProperty("attemptId").GetString());
        Assert.Equal(0, summary.GetProperty("startedRequests").GetInt32());
    }

    [Theory]
    [InlineData("running", "in-flight")]
    [InlineData("paused", "paused")]
    public async Task AssociatedLiveProcessDoesNotBecomeInterruptedJustBecauseTelemetryIsQuiet(string status, string requestStatus)
    {
        var run = _manager.RecordRejectedLaunch("status fixture", "fixture");
        run.Status = status;
        run.CompletedAt = null;
        run.Error = null;
        var path = Path.Combine(_root, AiLoopReader.MetricsFolder, "9.jsonl");
        File.WriteAllLines(path, [
            JsonSerializer.Serialize(new { ts = T0().ToString("o"), @event = "run_started", managedRunId = run.RunId, telemetryVersion = 2 }),
            JsonSerializer.Serialize(new { ts = T0().AddSeconds(1).ToString("o"), @event = "llm_request_started", requestId = "quiet" })
        ]);
        File.SetLastWriteTimeUtc(path, DateTime.UtcNow.AddHours(-2));

        var response = await _app.GetTestClient().GetAsync($"/api/ai-loop/managed/{run.RunId}");
        var detail = await response.Content.ReadFromJsonAsync<JsonElement>();
        Assert.True(response.Headers.CacheControl!.NoStore);
        Assert.Equal(status, detail.GetProperty("summary").GetProperty("status").GetString());
        Assert.Equal(requestStatus, Assert.Single(detail.GetProperty("requests").EnumerateArray()).GetProperty("status").GetString());
        Assert.Equal(0, detail.GetProperty("summary").GetProperty("calls").GetInt32());
        Assert.Equal(T0().AddSeconds(1), detail.GetProperty("summary").GetProperty("lastEventAt").GetDateTime());

        static DateTime T0() => new(2026, 1, 1, 10, 0, 0, DateTimeKind.Utc);
    }

    [Theory]
    [InlineData("/api/ai-loop/no-such-run-xyz", HttpStatusCode.NotFound)]
    [InlineData("/api/ai-loop/bad.id", HttpStatusCode.BadRequest)]
    [InlineData("/api/ai-loop/managed/7", HttpStatusCode.NotFound)]
    [InlineData("/api/ai-loop/managed/no-such-launch", HttpStatusCode.NotFound)]
    [InlineData("/api/ai-loop/managed/bad.id", HttpStatusCode.BadRequest)]
    public async Task UnknownAndInvalidRunsAreRejected(string url, HttpStatusCode expected)
    {
        var response = await _app.GetTestClient().GetAsync(url);

        Assert.Equal(expected, response.StatusCode);
    }

    [Fact]
    public async Task ManagedLookupReturnsOnlyTheExplicitlyAssociatedAttempt()
    {
        var detail = await _app.GetTestClient().GetFromJsonAsync<JsonElement>("/api/ai-loop/managed/portal-7");

        Assert.Equal("7", detail.GetProperty("summary").GetProperty("runId").GetString());
        Assert.Equal("portal-7", detail.GetProperty("summary").GetProperty("managedRunId").GetString());
    }

    [Fact]
    public async Task HistoryPagesAndExplicitAttemptSelectionRemainAvailableAfterAResume()
    {
        Directory.CreateDirectory(Path.Combine(_root, "Config"));
        File.WriteAllText(Path.Combine(_root, "Config", "appsettings.json"), """{"AiLoop":{"MaxRuns":1}}""");
        File.AppendAllText(Path.Combine(_root, AiLoopReader.MetricsFolder, "7.jsonl"), "\n" +
            """{"ts":"2026-01-01T10:01:00Z","event":"run_started","attemptId":"b","managedRunId":"portal-new","telemetryVersion":2}""");
        var client = _app.GetTestClient();

        var first = await client.GetFromJsonAsync<JsonElement>("/api/ai-loop/runs");
        Assert.True(first.GetProperty("hasMore").GetBoolean());
        Assert.Equal(1, first.GetProperty("nextOffset").GetInt32());
        Assert.Equal("b", Assert.Single(first.GetProperty("runs").EnumerateArray()).GetProperty("attemptId").GetString());
        var second = await client.GetFromJsonAsync<JsonElement>("/api/ai-loop/runs?offset=1");
        Assert.False(second.GetProperty("hasMore").GetBoolean());
        Assert.Equal("a", Assert.Single(second.GetProperty("runs").EnumerateArray()).GetProperty("attemptId").GetString());
        var older = await client.GetFromJsonAsync<JsonElement>("/api/ai-loop/7?attemptId=a");
        Assert.Equal("a", older.GetProperty("summary").GetProperty("attemptId").GetString());
        Assert.Equal("interrupted", older.GetProperty("summary").GetProperty("status").GetString());
        Assert.Equal(HttpStatusCode.BadRequest, (await client.GetAsync("/api/ai-loop/7?attemptIndex=-1")).StatusCode);
    }

    [Fact]
    public async Task ARealChildFailureBeforeTelemetryIsListedWithoutBorrowingAHistoricalAttempt()
    {
        if (OperatingSystem.IsWindows()) return;
        var run = _manager.StartRun("convert-only", "isolated preflight failure");
        await run.Completion.WaitAsync(TimeSpan.FromSeconds(10));

        var payload = await _app.GetTestClient().GetFromJsonAsync<JsonElement>("/api/ai-loop/runs");
        var process = Assert.Single(payload.GetProperty("managedRuns").EnumerateArray());
        Assert.Equal(run.RunId, process.GetProperty("runId").GetString());
        Assert.Equal("failed", process.GetProperty("status").GetString());
        Assert.Equal(23, process.GetProperty("exitCode").GetInt32());
        Assert.Equal("7", Assert.Single(payload.GetProperty("runs").EnumerateArray()).GetProperty("runId").GetString());
        var missing = await _app.GetTestClient().GetAsync($"/api/ai-loop/managed/{run.RunId}");
        Assert.Equal(HttpStatusCode.NotFound, missing.StatusCode);
        Assert.Contains("isolated startup diagnostic", File.ReadAllText(run.LogPath!));
    }

    [Theory]
    [InlineData("0", "unknown")]
    [InlineData("23", "failed")]
    public async Task ARealChildExitWithoutAttemptFinishEndsOutstandingRequestsWithoutInventingSuccess(string exitCode, string status)
    {
        if (OperatingSystem.IsWindows()) return;
        var run = _manager.StartRun("convert-only", "isolated incomplete attempt",
            extraEnv: new() { ["TEST_AI_LOOP_WRITE"] = "1", ["TEST_EXIT_CODE"] = exitCode });
        await run.Completion.WaitAsync(TimeSpan.FromSeconds(10));

        var listed = await _app.GetTestClient().GetFromJsonAsync<JsonElement>("/api/ai-loop/runs");
        var summary = listed.GetProperty("runs").EnumerateArray().Single(r => r.GetProperty("runId").GetString() == "9");
        Assert.Equal(status, summary.GetProperty("status").GetString());
        foreach (var path in new[] { $"/api/ai-loop/managed/{run.RunId}", "/api/ai-loop/9" })
        {
            var detail = await _app.GetTestClient().GetFromJsonAsync<JsonElement>(path);
            Assert.Equal(run.RunId, detail.GetProperty("summary").GetProperty("managedRunId").GetString());
            Assert.Equal(status, detail.GetProperty("summary").GetProperty("status").GetString());
            Assert.Equal(JsonValueKind.String, detail.GetProperty("summary").GetProperty("finishedAt").ValueKind);
            Assert.Equal("unknown", Assert.Single(detail.GetProperty("requests").EnumerateArray()).GetProperty("status").GetString());
            var model = detail.GetProperty("gates").EnumerateArray().Single(g => g.GetProperty("id").GetString() == "model");
            Assert.Equal("warn", model.GetProperty("status").GetString());
        }
    }
}
