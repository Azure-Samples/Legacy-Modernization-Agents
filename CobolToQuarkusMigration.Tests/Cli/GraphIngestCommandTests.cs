using System.Diagnostics;
using System.Text.Json;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Cli;

// doctor.sh runs graph-ingest after every parse and stops on a non-zero exit, so refusal and
// planning are pinned through the real process without AI settings or a reachable Neo4j.
public sealed class GraphIngestCommandTests : IDisposable
{
    private readonly string _root = Directory.CreateDirectory(
        Path.Join(Path.GetTempPath(), $"graph-ingest-cli-{Guid.NewGuid():N}")).FullName;

    public void Dispose() => Directory.Delete(_root, recursive: true);

    [Fact]
    public void FailedJclRefreshIsRefusedBeforeConnecting()
    {
        var output = Directory.CreateDirectory(Path.Join(_root, "rekt")).FullName;
        File.WriteAllText(Path.Join(output, ".jcl-scan-failed"), "");

        // A password and an unreachable URI prove the refusal happens before any connection attempt.
        var result = Run(("NEO4J_PASSWORD", "unused"), ("NEO4J_URI", "bolt://127.0.0.1:1"),
            "--rekt-output", output, "--source-dir", _root, "--run-id", "5");

        result.ExitCode.Should().Be(1);
        result.StdErr.Should().Contain("JCL refresh did not complete; rerun jcl-facts before ingestion.");
        result.StdOut.Should().NotContain("Applying schema");
    }

    [Fact]
    public void MissingNeo4jPasswordFailsWithAClearMessage()
    {
        var result = Run(null, null, "--rekt-output", _root, "--source-dir", _root, "--run-id", "5");

        result.ExitCode.Should().Be(1);
        result.StdErr.Should().Contain("NEO4J_PASSWORD is required");
    }

    [Fact]
    public void DryRunPrintsTheGraphItWouldWrite()
    {
        var source = Directory.CreateDirectory(Path.Join(_root, "source")).FullName;
        File.WriteAllText(Path.Join(source, "DEMO.cbl"), "IDENTIFICATION DIVISION.\n");
        var report = Directory.CreateDirectory(Path.Join(_root, "rekt", "DEMO.cbl.report")).FullName;
        File.WriteAllText(Path.Join(report, "flow-ast-DEMO.cbl.json"), """{"id":"root","type":"PROGRAM"}""");

        var result = Run(null, null, "--rekt-output", Path.GetDirectoryName(report)!, "--source-dir", source, "--run-id", "5", "--dry-run");

        result.ExitCode.Should().Be(0, result.StdErr);
        using var plan = JsonDocument.Parse(result.StdOut);
        plan.RootElement.GetProperty("nodes").EnumerateArray().Select(n => n.GetProperty("label").GetString())
            .Should().Contain(["CobolFile", "ASTNode", "SourceBlock"]);
        plan.RootElement.GetProperty("relationships").EnumerateArray().Select(r => r.GetProperty("type").GetString())
            .Should().Contain("HAS_AST");
    }

    private (int ExitCode, string StdOut, string StdErr) Run(
        (string, string)? password, (string, string)? uri, params string[] args)
    {
        var psi = new ProcessStartInfo("dotnet")
        {
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            // An empty working directory keeps a developer's Config/ and Logs/ out of the result.
            WorkingDirectory = Directory.CreateDirectory(Path.Join(_root, $"cwd-{Guid.NewGuid():N}")).FullName,
        };
        foreach (var name in new[] { "NEO4J_PASSWORD", "NEO4J_URI", "NEO4J_USER", "NEO4J_BOLT_PORT",
                     "AZURE_OPENAI_API_KEY", "AZURE_OPENAI_ENDPOINT" })
        {
            psi.Environment.Remove(name);
        }
        foreach (var (name, value) in new[] { password, uri }.OfType<(string, string)>()) psi.Environment[name] = value;
        psi.ArgumentList.Add(Path.Join(AppContext.BaseDirectory, "CobolToQuarkusMigration.dll"));
        psi.ArgumentList.Add("graph-ingest");
        foreach (var arg in args) psi.ArgumentList.Add(arg);

        using var process = Process.Start(psi)!;
        var stdErr = process.StandardError.ReadToEndAsync();
        var stdOut = process.StandardOutput.ReadToEnd();
        process.WaitForExit(60_000).Should().BeTrue("the CLI should not hang");
        return (process.ExitCode, stdOut, stdErr.Result);
    }
}
