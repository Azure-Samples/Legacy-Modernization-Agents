using System.Net;
using System.Text.Json.Nodes;
using CobolToQuarkusMigration.Cli;
using FluentAssertions;
using Microsoft.Data.Sqlite;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Cli;

public sealed class AiLoopCheckCommandTests : IDisposable
{
    private readonly string _root = Path.Join(Path.GetTempPath(), $"ai-loop-check-{Guid.NewGuid():N}");
    private readonly Dictionary<string, JsonNode> _responses = new();

    public AiLoopCheckCommandTests()
    {
        var scan = Directory.CreateDirectory(Path.Join(_root, "output", "rekt-scans", "scan-a")).FullName;
        File.WriteAllText(Path.Join(scan, "scan.json"), """
            {"programs":[{"relativePath":"A.cbl"},{"relativePath":"B.cbl"}],"expectedPrograms":2,"logPath":"native.log",
             "status":"complete","finishedAt":"2026-01-02T03:04:05+00:00"}
            """);
        var metrics = Directory.CreateDirectory(Path.Join(_root, "output", ".metrics")).FullName;
        File.WriteAllText(Path.Join(metrics, "7.jsonl"), "{}\n{}\n");
        Directory.CreateDirectory(Path.Join(_root, "Data"));
        using (var db = new SqliteConnection($"Data Source={Path.Join(_root, "Data", "migration.db")};Pooling=false"))
        {
            db.Open();
            using var create = db.CreateCommand();
            create.CommandText = "CREATE TABLE runs (id INTEGER); INSERT INTO runs VALUES (1), (2), (3);";
            create.ExecuteNonQuery();
        }

        // The native summary sits on the second page so pagination is exercised.
        _responses["api/ai-loop/runs"] = JsonNode.Parse("""{"runs":[{"activityId":"model:7"}],"hasMore":true,"nextOffset":1}""")!;
        _responses["api/ai-loop/runs?offset=1"] = new JsonObject { ["runs"] = new JsonArray(NativeSummary()), ["hasMore"] = false };
        _responses["api/ai-loop/native-1"] = NativeDetail();
        _responses["api/ai-loop/7"] = JsonNode.Parse("""
            {"summary":{"activityKind":"model-attempt","startedRequests":5,"calls":4,"finishedAt":"2026-01-02T05:00:00Z"}}
            """)!;
    }

    public void Dispose()
    {
        SqliteConnection.ClearAllPools();
        Directory.Delete(_root, recursive: true);
    }

    [Fact]
    public async Task CataloguedScanAndModelRun_PassWithSummaryAndTotals()
    {
        var output = await Run(modelRun: "7");

        var lines = output.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries);
        lines.Should().HaveCount(3);
        JsonNode.Parse(lines[0])!["activityId"]!.GetValue<string>().Should().Be("native:scan-a");
        var totals = JsonNode.Parse(lines[1])!;
        totals["migrationRows"]!.GetValue<long>().Should().Be(3);
        totals["metricsRecords"]!.GetValue<long>().Should().Be(2);
        totals["modelStarts"]!.GetValue<int>().Should().Be(5);
        lines[2].Should().StartWith("PASS:");
    }

    [Fact]
    public async Task UncataloguedScan_Fails()
    {
        _responses["api/ai-loop/runs?offset=1"] = JsonNode.Parse("""{"runs":[],"hasMore":false}""")!;

        var act = () => Run();

        (await act.Should().ThrowAsync<AiLoopCheckException>()).WithMessage("*native activity missing*");
    }

    [Theory]
    [InlineData("calls", "1")]
    [InlineData("attemptId", "\"attempt-1\"")]
    [InlineData("finishedAt", "\"2026-01-02T03:04:06Z\"")]
    public async Task NativeSummaryThatDisagreesOrCarriesModelActivity_Fails(string field, string json)
    {
        var summary = NativeSummary();
        summary[field] = JsonNode.Parse(json);
        _responses["api/ai-loop/runs?offset=1"] = new JsonObject { ["runs"] = new JsonArray(summary), ["hasMore"] = false };

        var act = () => Run();

        await act.Should().ThrowAsync<AiLoopCheckException>();
    }

    [Fact]
    public async Task RecordedProgramStderrThatIsNotCatalogued_Fails()
    {
        var manifestPath = Path.Join(_root, "output", "rekt-scans", "scan-a", "scan.json");
        var manifest = JsonNode.Parse(File.ReadAllText(manifestPath))!;
        manifest["programs"]![0]!["structure"] = JsonNode.Parse("""{"stderr":"logs/A.err","exitCode":0}""");
        File.WriteAllText(manifestPath, manifest.ToJsonString());

        var act = () => Run();

        (await act.Should().ThrowAsync<AiLoopCheckException>()).WithMessage("*structure stderr*");
    }

    [Fact]
    public async Task ModelRunWithMoreOutcomesThanStarts_Fails()
    {
        _responses["api/ai-loop/7"]["summary"]!["calls"] = 6;

        var act = () => Run(modelRun: "7");

        (await act.Should().ThrowAsync<AiLoopCheckException>()).WithMessage("*fewer started requests*");
    }

    [Fact]
    public void UnreachablePortal_ExitsNonZeroThroughTheRealProcess()
    {
        var psi = new System.Diagnostics.ProcessStartInfo("dotnet")
        {
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            WorkingDirectory = _root,
        };
        foreach (var arg in new[]
                 {
                     Path.Join(AppContext.BaseDirectory, "CobolToQuarkusMigration.dll"), "ai-loop-check",
                     "--url", "http://127.0.0.1:9", "--repo-root", _root, "--scan", "scan-a",
                 })
            psi.ArgumentList.Add(arg);

        using var process = System.Diagnostics.Process.Start(psi)!;
        var stdErr = process.StandardError.ReadToEndAsync();
        var stdOut = process.StandardOutput.ReadToEnd();
        process.WaitForExit(60_000).Should().BeTrue();

        process.ExitCode.Should().Be(1);
        stdErr.Result.Should().Contain("FAIL:");
        stdOut.Should().NotContain("PASS");
    }

    private static JsonObject NativeSummary() => JsonNode.Parse("""
        {"activityId":"native:scan-a","runId":"native-1","activityKind":"native-rekt","status":"complete",
         "requestCoverage":"not-applicable","calls":0,"attemptId":null,"managedRunId":null,
         "startedAt":"2026-01-02T03:00:00Z","finishedAt":"2026-01-02T03:04:05Z","lastEventAt":"2026-01-02T03:04:05Z"}
        """)!.AsObject();

    private static JsonNode NativeDetail() => JsonNode.Parse("""
        {"native":{"recordedPrograms":2,"expectedPrograms":2},"summary":{"status":"complete"},
         "agents":[],"requests":[],"logs":[{"kind":"native manifest"},{"kind":"native process log"}]}
        """)!;

    private async Task<string> Run(string? modelRun = null)
    {
        using var http = new HttpClient(new StubPortal(_responses)) { BaseAddress = new Uri("http://portal/") };
        using var output = new StringWriter();
        await AiLoopCheckCommand.RunAsync(http, _root, ["scan-a"], modelRun, output);
        return output.ToString();
    }

    private sealed class StubPortal(Dictionary<string, JsonNode> responses) : HttpMessageHandler
    {
        protected override Task<HttpResponseMessage> SendAsync(HttpRequestMessage request, CancellationToken cancellationToken)
        {
            var path = request.RequestUri!.PathAndQuery.TrimStart('/');
            return Task.FromResult(responses.TryGetValue(path, out var body)
                ? new HttpResponseMessage(HttpStatusCode.OK) { Content = new StringContent(body.ToJsonString()) }
                : new HttpResponseMessage(HttpStatusCode.NotFound));
        }
    }
}
