using System.Diagnostics;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Cli;

// Selector flags travel alongside the subcommand, so the parser has to strip them without
// disturbing the positional arguments that pick which doctor.sh command runs.
public sealed class ConversionSelectorFlagTests
{
    [Fact]
    public void NoSelectorFlagsLeavesTheSelectorEmptyAndArgumentsUntouched()
    {
        var result = ParseFlags("convert-only", "--resume");

        result.ExitCode.Should().Be(0);
        result.Selector.Should().BeEmpty();
        result.IncludeCallers.Should().Be("false");
        result.IncludeCallees.Should().Be("false");
        result.Remaining.Should().Equal(["convert-only", "--resume"]);
    }

    [Fact]
    public void ProgramFlagIsCapturedAndRemovedFromTheCommandArguments()
    {
        var result = ParseFlags("convert-only", "--program", "LEDGER.cbl");

        result.ExitCode.Should().Be(0);
        result.Selector.Should().Be("LEDGER.cbl");
        result.Remaining.Should().Equal(["convert-only"],
            "leaving --program in place would make it the subcommand on the next parse");
    }

    [Fact]
    public void ProgramFlagAcceptsAnEqualsForm()
    {
        var result = ParseFlags("convert-only", "--program=finance/LEDGER.cbl");

        result.ExitCode.Should().Be(0);
        result.Selector.Should().Be("finance/LEDGER.cbl");
        result.Remaining.Should().Equal(["convert-only"]);
    }

    [Fact]
    public void RepeatedProgramFlagsAccumulateAsACommaSeparatedSelector()
    {
        var result = ParseFlags("convert-only", "--program", "LEDGER.cbl", "--program", "INVOICE.cbl");

        result.ExitCode.Should().Be(0);
        result.Selector.Should().Be("LEDGER.cbl,INVOICE.cbl",
            "dropping the second selector would silently convert less than asked for");
    }

    [Fact]
    public void ClosureFlagsAreCapturedAndRemoved()
    {
        var result = ParseFlags("convert-only", "--program", "LEDGER.cbl", "--include-callers", "--include-callees");

        result.ExitCode.Should().Be(0);
        result.IncludeCallers.Should().Be("true");
        result.IncludeCallees.Should().Be("true");
        result.Remaining.Should().Equal(["convert-only"]);
    }

    [Fact]
    public void ProgramFlagWithoutAValueIsRejected()
    {
        var result = ParseFlags("convert-only", "--program");

        result.ExitCode.Should().NotBe(0,
            "an empty selector would silently fall back to converting the whole estate");
        result.Output.Should().Contain("--program");
    }

    [Fact]
    public void ClosureFlagWithoutAProgramSelectorIsRejected()
    {
        var result = ParseFlags("convert-only", "--include-callees");

        result.ExitCode.Should().NotBe(0,
            "a closure has no seed without --program, so honouring it would convert everything");
        result.Output.Should().Contain("--program");
    }

    private static (int ExitCode, string Selector, string IncludeCallers, string IncludeCallees,
        string[] Remaining, string Output) ParseFlags(params string[] args)
    {
        var repoRoot = LocateRepoRoot();
        var quoted = string.Join(' ', args.Select(a => "'" + a.Replace("'", "'\\''") + "'"));
        var script = $$"""
            DOCTOR_SOURCE_ONLY=1 source "{{Path.Combine(repoRoot, "doctor.sh")}}"
            parse_conversion_selector_flags {{quoted}} || exit $?
            echo "SELECTOR=${CONVERSION_PROGRAM_SELECTOR:-}"
            echo "CALLERS=${CONVERSION_INCLUDE_CALLERS:-}"
            echo "CALLEES=${CONVERSION_INCLUDE_CALLEES:-}"
            for arg in "${CONVERSION_REMAINING_ARGS[@]}"; do echo "ARG=$arg"; done
            """;

        var psi = new ProcessStartInfo("bash")
        {
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            WorkingDirectory = repoRoot,
        };
        psi.ArgumentList.Add("-c");
        psi.ArgumentList.Add(script);

        using var process = Process.Start(psi)!;
        var stdout = process.StandardOutput.ReadToEnd();
        var stderr = process.StandardError.ReadToEnd();
        process.WaitForExit(60_000).Should().BeTrue("flag parsing should not hang");

        var lines = stdout.Split('\n', StringSplitOptions.RemoveEmptyEntries)
            .Select(line => line.TrimEnd('\r'))
            .ToList();

        string Value(string prefix) => lines
            .LastOrDefault(l => l.StartsWith(prefix, StringComparison.Ordinal))?[prefix.Length..] ?? "";

        return (
            process.ExitCode,
            Value("SELECTOR="),
            Value("CALLERS="),
            Value("CALLEES="),
            lines.Where(l => l.StartsWith("ARG=", StringComparison.Ordinal)).Select(l => l[4..]).ToArray(),
            stdout + stderr);
    }

    private static string LocateRepoRoot()
    {
        var dir = new DirectoryInfo(AppContext.BaseDirectory);
        while (dir is not null && !File.Exists(Path.Combine(dir.FullName, "doctor.sh")))
            dir = dir.Parent;

        dir.Should().NotBeNull("the tests should run from inside the repository");
        return dir!.FullName;
    }
}
