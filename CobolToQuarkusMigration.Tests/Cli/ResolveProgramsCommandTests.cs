using System.Diagnostics;
using System.Text.Json;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Cli;

public sealed class ResolveProgramsCommandTests : IDisposable
{
    private readonly string _root = Path.Combine(
        AppContext.BaseDirectory,
        "test-artifacts",
        $"resolve-programs-{Guid.NewGuid():N}");

    private string StagingDir => Path.Combine(_root, "staging");

    private string FactsDir => Path.Combine(_root, "facts");

    [Fact]
    public void ResolvesSelectorToSourceRelativePathOnStdout()
    {
        WriteProgram("finance/ACCOUNTS.cbl");
        WriteProgram("shared/UTILITY.cbl");

        var result = RunCli("resolve-programs", StagingDir, "--program", "UTILITY");

        result.ExitCode.Should().Be(0);
        StdoutLines(result.Stdout).Should().ContainSingle().Which.Should().Be("shared/UTILITY.cbl");
    }

    // A selector that quietly picks one of two same-named programs converts the wrong file,
    // so the command must refuse and print nothing selectable.
    [Fact]
    public void AmbiguousBasenameFailsWithoutEmittingACandidate()
    {
        WriteProgram("finance/ACCOUNTS.cbl");
        WriteProgram("archive/ACCOUNTS.cbl");

        var result = RunCli("resolve-programs", StagingDir, "--program", "ACCOUNTS.cbl");

        result.ExitCode.Should().Be(2);
        result.Stderr.Should().Contain("Use a source-relative path");
        StdoutLines(result.Stdout).Should().BeEmpty();
    }

    [Fact]
    public void MissingStagingDirectoryFails()
    {
        var result = RunCli("resolve-programs", Path.Combine(_root, "absent"), "--program", "ANY");

        result.ExitCode.Should().Be(2);
        StdoutLines(result.Stdout).Should().BeEmpty();
    }

    [Fact]
    public void NoSelectorFails()
    {
        WriteProgram("finance/ACCOUNTS.cbl");

        var result = RunCli("resolve-programs", StagingDir);

        result.ExitCode.Should().Be(2);
        StdoutLines(result.Stdout).Should().BeEmpty();
    }

    [Fact]
    public void ClosureExpandsThroughRecordedCallEdges()
    {
        WriteProgram("finance/ACCOUNTS.cbl");
        WriteProgram("finance/LEDGER.cbl");
        WriteFacts("finance/ACCOUNTS.cbl", callees: ["finance/LEDGER.cbl"]);
        WriteFacts("finance/LEDGER.cbl", callers: ["finance/ACCOUNTS.cbl"]);

        var result = RunCli(
            "resolve-programs", StagingDir,
            "--program", "ACCOUNTS",
            "--include-callees",
            "--facts-dir", FactsDir);

        result.ExitCode.Should().Be(0);
        StdoutLines(result.Stdout).Should().Equal("finance/ACCOUNTS.cbl", "finance/LEDGER.cbl");
    }

    [Fact]
    public void ClosureWithoutFactsFailsRatherThanConvertingASubset()
    {
        WriteProgram("finance/ACCOUNTS.cbl");

        var result = RunCli(
            "resolve-programs", StagingDir,
            "--program", "ACCOUNTS",
            "--include-callees",
            "--facts-dir", FactsDir);

        result.ExitCode.Should().Be(2);
        result.Stderr.Should().Contain("rekt-scan");
        StdoutLines(result.Stdout).Should().BeEmpty();
    }

    [Fact]
    public void ManifestRecordsSelectorsResolvedProgramsAndReasons()
    {
        WriteProgram("finance/ACCOUNTS.cbl");
        WriteProgram("finance/LEDGER.cbl");
        WriteFacts("finance/ACCOUNTS.cbl", callees: ["finance/LEDGER.cbl", "PAYROLL"]);
        WriteFacts("finance/LEDGER.cbl", callers: ["finance/ACCOUNTS.cbl"]);
        var manifestPath = Path.Combine(_root, "selection.json");

        var result = RunCli(
            "resolve-programs", StagingDir,
            "--program", "ACCOUNTS",
            "--include-callees",
            "--facts-dir", FactsDir,
            "--manifest", manifestPath);

        result.ExitCode.Should().Be(0);
        using var manifest = JsonDocument.Parse(File.ReadAllText(manifestPath));
        var root = manifest.RootElement;

        root.GetProperty("selectors").GetProperty("programs")
            .EnumerateArray().Select(e => e.GetString()).Should().Equal("ACCOUNTS");
        root.GetProperty("selectors").GetProperty("includeCallees").GetBoolean().Should().BeTrue();
        root.GetProperty("programs").EnumerateArray().Select(e => e.GetString())
            .Should().Equal("finance/ACCOUNTS.cbl", "finance/LEDGER.cbl");
        root.GetProperty("unresolvedCallTargets").EnumerateArray().Select(e => e.GetString())
            .Should().ContainSingle().Which.Should().Be("PAYROLL");

        var reasons = root.GetProperty("matches").EnumerateArray()
            .ToDictionary(e => e.GetProperty("program").GetString()!, e => e.GetProperty("reason").GetString());
        reasons["finance/ACCOUNTS.cbl"].Should().Be("program selector 'ACCOUNTS'");
        reasons["finance/LEDGER.cbl"].Should().Be("called by finance/ACCOUNTS.cbl");
    }

    [Fact]
    public void FailedResolutionWritesNoManifest()
    {
        WriteProgram("finance/ACCOUNTS.cbl");
        WriteProgram("archive/ACCOUNTS.cbl");
        var manifestPath = Path.Combine(_root, "selection.json");

        var result = RunCli(
            "resolve-programs", StagingDir,
            "--program", "ACCOUNTS.cbl",
            "--manifest", manifestPath);

        result.ExitCode.Should().Be(2);
        File.Exists(manifestPath).Should().BeFalse("a refused selection must not leave a manifest claiming a scope");
    }

    private static string[] StdoutLines(string stdout) =>
        stdout.Split('\n', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries);

    private void WriteProgram(string relativePath)
    {
        var path = Path.Combine(StagingDir, relativePath.Replace('/', Path.DirectorySeparatorChar));
        Directory.CreateDirectory(Path.GetDirectoryName(path)!);
        File.WriteAllText(path, "       IDENTIFICATION DIVISION.\n");
    }

    private void WriteFacts(
        string relativePath,
        IReadOnlyList<string>? callees = null,
        IReadOnlyList<string>? callers = null)
    {
        var facts = new ProgramFacts
        {
            Basename = Path.GetFileName(relativePath),
            Stem = Path.GetFileNameWithoutExtension(relativePath),
            RelativePath = relativePath,
            SourceHash = "test",
            Confidence = FactConfidence.High,
            Summary = new ProgramSummary { ProgramId = Path.GetFileNameWithoutExtension(relativePath) },
            Callees = callees ?? Array.Empty<string>(),
            Callers = callers ?? Array.Empty<string>(),
        };

        var path = ProgramFactsArtifactLocator.GetFactsFilePath(FactsDir, relativePath);
        Directory.CreateDirectory(Path.GetDirectoryName(path)!);
        File.WriteAllText(path, JsonSerializer.Serialize(facts));
    }

    private static (int ExitCode, string Stdout, string Stderr) RunCli(params string[] args)
    {
        var dll = Path.Combine(AppContext.BaseDirectory, "CobolToQuarkusMigration.dll");
        var psi = new ProcessStartInfo("dotnet")
        {
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            WorkingDirectory = AppContext.BaseDirectory,
        };
        psi.ArgumentList.Add(dll);
        foreach (var a in args) psi.ArgumentList.Add(a);

        using var process = Process.Start(psi)!;
        var stdout = process.StandardOutput.ReadToEnd();
        var stderr = process.StandardError.ReadToEnd();
        process.WaitForExit(milliseconds: 120_000).Should().BeTrue("the CLI should not hang");

        return (process.ExitCode, stdout, stderr);
    }

    public void Dispose()
    {
        if (Directory.Exists(_root))
            Directory.Delete(_root, recursive: true);
    }
}
