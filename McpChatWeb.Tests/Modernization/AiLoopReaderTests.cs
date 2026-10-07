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
using McpChatWeb.Services;
using Microsoft.Extensions.Logging.Abstractions;
using Xunit;

namespace McpChatWeb.Tests.Modernization;

public sealed class AiLoopReaderTests : IDisposable
{
    private readonly string _root = Path.Combine(Path.GetTempPath(), "ai-loop-" + Guid.NewGuid().ToString("N"));
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

    private static string Line(int seconds, object payload)
    {
        var dict = new Dictionary<string, object?> { ["ts"] = T0.AddSeconds(seconds).ToString("o"), ["runId"] = "7" };
        foreach (var p in payload.GetType().GetProperties()) dict[p.Name] = p.GetValue(payload);
        return JsonSerializer.Serialize(dict);
    }

    private void Metrics(string runId, params string[] lines)
        => File.WriteAllLines(Path.Combine(_root, "output", ".metrics", runId + ".jsonl"), lines);

    private void Artifact(string name, string json) => File.WriteAllText(Path.Combine(_root, RunFolder, name), json);

    private void ACompletedRun(string folder = RunFolder) => Metrics("7",
        Line(0, new { @event = "run_started", mode = "standard", targetLanguage = "C#", outputFolder = folder }),
        Line(1, new { @event = "stage", number = 1, total = 6, name = "File Discovery" }),
        Line(4, new { @event = "stage", number = 2, total = 6, name = "Dependency Analysis" }),
        Line(5, new { @event = "llm_call", agent = "Converter", model = "m1", context = "A.cbl", durationMs = 100, success = true, promptChars = 400, responseChars = 80, inputTokens = 100, outputTokens = 20 }),
        Line(6, new { @event = "llm_call", agent = "Converter", model = "m1", context = "B.cbl", durationMs = 300, success = false, promptChars = 400, responseChars = 0, error = "TimeoutException: slow" }),
        Line(7, new { @event = "llm_retry", agent = "Converter", context = "B.cbl", reason = "transient_error", attempt = 1 }),
        Line(9, new { @event = "llm_call", agent = "Converter", model = "m1", context = "B.cbl", durationMs = 200, success = true, promptChars = 400, responseChars = 90, inputTokens = 100, outputTokens = 25 }),
        Line(10, new { @event = "llm_call", agent = "Analyzer", model = "m2", context = "A.cbl", durationMs = 50, success = true, promptChars = 200, responseChars = 40 }),
        Line(11, new { @event = "llm_fallback", agent = "Analyzer", context = "C.cbl", reason = "content_filter", detail = "blocked" }),
        Line(12, new { @event = "projection_metrics", agent = "Converter", file = "A.cbl" }),
        Line(20, new { @event = "run_finished", status = "completed", durationMs = 20000 }));

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
        using (var connection = new Microsoft.Data.Sqlite.SqliteConnection($"Data Source={db};Pooling=False"))
        {
            connection.Open();
            using var command = connection.CreateCommand();
            command.CommandText = "CREATE TABLE runs (id INTEGER PRIMARY KEY, started_at TEXT NOT NULL, status TEXT NOT NULL);"
                                  + "INSERT INTO runs VALUES (8, '2026-01-01', 'Terminated'), (9, '2026-01-01', 'Running');";
            command.ExecuteNonQuery();
        }

        var runs = (await Reader().ListRunsAsync()).ToDictionary(r => r.RunId, r => r.Status);

        Assert.Equal("terminated", runs["8"]);
        Assert.Equal("running", runs["9"]);
        Assert.Equal("terminated", (await Reader().GetRunAsync("8"))!.Summary.Status);
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
    public async Task TheTimelineHoldsRetriesFallbacksAndFailedCallsInOrder()
    {
        ACompletedRun();

        var events = (await Reader().GetRunAsync("7"))!.Events;

        Assert.Equal(["llm_call", "llm_retry", "llm_fallback"], events.Select(e => e.Kind));
        Assert.Equal("TimeoutException: slow", events[0].Reason);
        Assert.Equal("transient_error", events[1].Reason);
        Assert.Equal("content_filter: blocked", events[2].Reason);
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

    private static AiLoopGate Gate(AiLoopRunDetail? detail, string id)
    {
        Assert.NotNull(detail);
        return detail!.Gates.Single(g => g.Id == id);
    }
}

public class AiLoopEndpointsTests : IClassFixture<Integration.WebAppFactory>
{
    private readonly Integration.WebAppFactory _factory;

    public AiLoopEndpointsTests(Integration.WebAppFactory factory) => _factory = factory;

    [Fact]
    public async Task RunsAreListedWithThePollInterval()
    {
        var payload = await _factory.CreateClient().GetFromJsonAsync<JsonElement>("/api/ai-loop/runs");

        Assert.Equal(JsonValueKind.Array, payload.GetProperty("runs").ValueKind);
        Assert.True(payload.GetProperty("pollSeconds").GetInt32() >= 1);
    }

    [Theory]
    [InlineData("/api/ai-loop/no-such-run-xyz", HttpStatusCode.NotFound)]
    [InlineData("/api/ai-loop/bad.id", HttpStatusCode.BadRequest)]
    public async Task UnknownAndInvalidRunsAreRejected(string url, HttpStatusCode expected)
    {
        var response = await _factory.CreateClient().GetAsync(url);

        Assert.Equal(expected, response.StatusCode);
    }
}
