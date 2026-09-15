using System.Diagnostics;
using System.Text.Json;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Cli;

// doctor.sh redirects these commands' stdout to a file and parses it positionally, so any
// startup chatter on stdout is consumed as data rather than ignored.
public sealed class MachineReadableStdoutTests : IDisposable
{
    private readonly string _root = Path.Combine(
        Path.GetTempPath(), "machine-stdout-" + Guid.NewGuid().ToString("N"));

    [Fact]
    public void RektScanCachePlan_EmitsOnlyTabSeparatedRows()
    {
        var staging = Path.Combine(_root, "staging");
        Directory.CreateDirectory(Path.Combine(staging, "finance"));
        File.WriteAllText(Path.Combine(staging, "finance", "LEDGER.cbl"), "       PROGRAM-ID. LEDGER.\n");

        var result = RunCli(
            "rekt-scan-cache", "plan", staging,
            "--db", Path.Combine(_root, "rekt.db"));

        result.ExitCode.Should().Be(0);
        foreach (var line in StdoutLines(result.Stdout))
        {
            line.Should().Contain("\t",
                "doctor.sh splits every stdout line on tabs into action/basename/reason");
        }
    }

    [Fact]
    public void ResolvePrograms_EmitsOnlyProgramPaths()
    {
        var staging = Path.Combine(_root, "resolve");
        Directory.CreateDirectory(Path.Combine(staging, "finance"));
        File.WriteAllText(Path.Combine(staging, "finance", "LEDGER.cbl"), "       PROGRAM-ID. LEDGER.\n");

        var result = RunCli("resolve-programs", staging, "--program", "LEDGER");

        result.ExitCode.Should().Be(0);
        StdoutLines(result.Stdout).Should().Equal("finance/LEDGER.cbl");
    }

    [Fact]
    // The hint fires only when Config/ai-config.local.env is absent, so a developer machine that
    // has one would not reveal the regression.
    public void ResolvePrograms_StdoutStaysCleanWithoutLocalConfig()
    {
        var staging = Path.Combine(_root, "no-config");
        Directory.CreateDirectory(Path.Combine(staging, "finance"));
        File.WriteAllText(Path.Combine(staging, "finance", "LEDGER.cbl"), "       PROGRAM-ID. LEDGER.\n");

        var emptyCwd = Path.Combine(_root, "empty-cwd");
        Directory.CreateDirectory(emptyCwd);

        var result = RunCli(
            new[] { "resolve-programs", staging, "--program", "LEDGER" },
            workingDirectory: emptyCwd);

        result.ExitCode.Should().Be(0);
        StdoutLines(result.Stdout).Should().Equal("finance/LEDGER.cbl");
    }

    private static IReadOnlyList<string> StdoutLines(string stdout) =>
        stdout.Split('\n', StringSplitOptions.RemoveEmptyEntries)
            .Select(line => line.TrimEnd('\r'))
            .Where(line => line.Length > 0)
            .ToList();

    private static (int ExitCode, string Stdout, string Stderr) RunCli(params string[] args)
        => RunCli(args, workingDirectory: null);

    private static (int ExitCode, string Stdout, string Stderr) RunCli(
        string[] args, string? workingDirectory)
    {
        var dll = Path.Combine(AppContext.BaseDirectory, "CobolToQuarkusMigration.dll");
        File.Exists(dll).Should().BeTrue($"the CLI assembly should sit beside the test assembly at {dll}");

        var psi = new ProcessStartInfo("dotnet")
        {
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            WorkingDirectory = workingDirectory ?? AppContext.BaseDirectory,
        };
        psi.ArgumentList.Add(dll);
        foreach (var a in args) psi.ArgumentList.Add(a);

        using var process = Process.Start(psi)!;
        var stdout = process.StandardOutput.ReadToEnd();
        var stderr = process.StandardError.ReadToEnd();
        process.WaitForExit(60_000).Should().BeTrue("the CLI should not hang");
        return (process.ExitCode, stdout, stderr);
    }

    public void Dispose()
    {
        if (Directory.Exists(_root)) Directory.Delete(_root, recursive: true);
    }
}
