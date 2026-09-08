using System.Diagnostics;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Cli;

// Only a real process can establish an exit code: an int-returning Main overrides
// Environment.ExitCode, so every command that reports failure that way is silently a success
// in-process. The parity gate and the existing Cli/ commands both depend on this.
public class ExitCodePropagationTests
{
    [Fact]
    public void FailingCommand_PropagatesEnvironmentExitCodeToTheProcess()
    {
        var result = RunCli("program-facts", "extract", "/nonexistent-staging-directory");

        result.ExitCode.Should().Be(2, "Cli/ProgramFactsCommand reports a missing input with Environment.ExitCode = 2");
    }

    [Fact]
    public void SucceedingCommand_ExitsZero()
    {
        var result = RunCli("--help");

        result.ExitCode.Should().Be(0);
    }

    private static (int ExitCode, string Output) RunCli(params string[] args)
    {
        var dll = Path.Combine(AppContext.BaseDirectory, "CobolToQuarkusMigration.dll");
        File.Exists(dll).Should().BeTrue($"the CLI assembly should sit beside the test assembly at {dll}");

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

        return (process.ExitCode, stdout + stderr);
    }
}
