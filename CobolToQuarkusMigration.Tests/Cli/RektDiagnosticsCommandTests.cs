using System.Diagnostics;
using System.Text.Json;
using CobolToQuarkusMigration.Agents.Infrastructure;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Cli;

// doctor.sh trusts these exit codes and this stdout to decide whether a program parsed, so
// both are pinned through a real process as well as the library.
public sealed class RektDiagnosticsCommandTests : IDisposable
{
    private readonly string _root = Path.Join(Path.GetTempPath(), $"rekt-diagnostics-{Guid.NewGuid():N}");

    public RektDiagnosticsCommandTests() => Directory.CreateDirectory(_root);

    public void Dispose() => Directory.Delete(_root, recursive: true);

    [Fact]
    public void HasStructure_MissingArtifactsAreNotStructuralSuccess()
    {
        RektParseDiagnostics.HasStructure(_root).Should().BeFalse();
        WriteArtifact("flow_ast", """{"id":"PROGRAM"}""");
        RektParseDiagnostics.HasStructure(_root).Should().BeFalse("the control-flow graph is still absent");
    }

    [Theory]
    [InlineData("{}")]
    [InlineData("[]")]
    [InlineData("null")]
    [InlineData("\"\"")]
    [InlineData("42")]
    public void HasStructure_EmptyOrScalarPayloadIsNotStructuralSuccess(string payload)
    {
        WriteArtifact("flow_ast", """{"id":"PROGRAM"}""");
        WriteArtifact("cfg", payload);

        RektParseDiagnostics.HasStructure(_root).Should().BeFalse();
    }

    [Fact]
    public void HasStructure_MalformedArtifactThrowsInsteadOfReadingAsAbsent()
    {
        WriteArtifact("flow_ast", """{"id":"PROGRAM"}""");
        WriteArtifact("cfg", "broken");

        var act = () => RektParseDiagnostics.HasStructure(_root);
        act.Should().Throw<JsonException>();
    }

    [Fact]
    public void MissingCopybookWarnings_AreDeduplicatedSortedAndSeparatorSafe()
    {
        var path = WriteDiagnostics("""
            [{"code":"missing copybook","message":"ZETA: Copybook not found"},
             {"code":"missing copybook","message":"ALPHA|B: Copybook not found"},
             {"code":"missing copybook","message":"ZETA: Copybook not found"},
             {"code":null,"message":"Warning"},
             {"code":"syntax","message":"ignored"}]
            """);

        RektParseDiagnostics.MissingCopybookWarnings(path).Should().Equal(
            "missing-copybook:ALPHA/B: Copybook not found",
            "missing-copybook:ZETA: Copybook not found");
    }

    [Theory]
    [InlineData("""{"code":"missing copybook"}""")]
    [InlineData("""["not an object"]""")]
    [InlineData("""[{"code":"missing copybook"}]""")]
    public void MissingCopybookWarnings_RejectDiagnosticsTheyCannotReport(string json)
    {
        var act = () => RektParseDiagnostics.MissingCopybookWarnings(WriteDiagnostics(json));
        act.Should().Throw<InvalidDataException>();
    }

    [Fact]
    public void StructureCommand_ExitCodesDistinguishPresentAbsentAndUnreadable()
    {
        Run("structure", _root).ExitCode.Should().Be(1);

        WriteArtifact("flow_ast", """{"id":"PROGRAM"}""");
        WriteArtifact("cfg", """{"nodes":[{"id":"known"}]}""");
        var present = Run("structure", _root);
        present.ExitCode.Should().Be(0);
        present.StdOut.Should().BeEmpty("doctor.sh runs this per program and must not log anything");

        WriteArtifact("cfg", "broken");
        var unreadable = Run("structure", _root);
        unreadable.ExitCode.Should().Be(2);
        unreadable.StdErr.Should().Contain("unreadable artifact");
    }

    [Fact]
    public void MissingCopybooksCommand_PrintsOnlyThePipeJoinedWarnings()
    {
        var path = WriteDiagnostics("""
            [{"code":"missing copybook","message":"B"},{"code":"missing copybook","message":"A"}]
            """);

        var result = Run("missing-copybooks", path);

        result.ExitCode.Should().Be(0);
        result.StdOut.Should().Be("missing-copybook:A|missing-copybook:B" + Environment.NewLine);
    }

    [Fact]
    public void MissingCopybooksCommand_BadInputExitsTwo()
    {
        Run("missing-copybooks", WriteDiagnostics("{}")).ExitCode.Should().Be(2);
        Run("missing-copybooks", Path.Join(_root, "absent.json")).ExitCode.Should().Be(2);
    }

    private void WriteArtifact(string directory, string json)
    {
        Directory.CreateDirectory(Path.Join(_root, directory));
        File.WriteAllText(Path.Join(_root, directory, "artifact.json"), json);
    }

    private string WriteDiagnostics(string json)
    {
        var path = Path.Join(_root, $"parse-diagnostics-{Guid.NewGuid():N}.json");
        File.WriteAllText(path, json);
        return path;
    }

    private (int ExitCode, string StdOut, string StdErr) Run(params string[] args)
    {
        var psi = new ProcessStartInfo("dotnet")
        {
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            // An empty working directory keeps a developer's Config/ and Logs/ out of the result.
            WorkingDirectory = Directory.CreateDirectory(Path.Join(_root, $"cwd-{Guid.NewGuid():N}")).FullName,
        };
        psi.ArgumentList.Add(Path.Join(AppContext.BaseDirectory, "CobolToQuarkusMigration.dll"));
        psi.ArgumentList.Add("rekt-diagnostics");
        foreach (var arg in args) psi.ArgumentList.Add(arg);

        using var process = Process.Start(psi)!;
        var stdErr = process.StandardError.ReadToEndAsync();
        var stdOut = process.StandardOutput.ReadToEnd();
        process.WaitForExit(60_000).Should().BeTrue("the CLI should not hang");
        return (process.ExitCode, stdOut, stdErr.Result);
    }
}
