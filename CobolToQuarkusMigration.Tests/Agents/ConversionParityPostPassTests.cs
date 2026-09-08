using CobolToQuarkusMigration.Agents;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Agents;

public class ConversionParityPostPassTests : IDisposable
{
    private readonly string? _originalThreshold = Environment.GetEnvironmentVariable("MIN_PROGRAM_SCORE");
    private readonly string? _originalGate = Environment.GetEnvironmentVariable("ON_LOW_SCORE");
    private readonly int _originalExitCode = Environment.ExitCode;

    public void Dispose()
    {
        Environment.SetEnvironmentVariable("MIN_PROGRAM_SCORE", _originalThreshold);
        Environment.SetEnvironmentVariable("ON_LOW_SCORE", _originalGate);
        Environment.ExitCode = _originalExitCode;
    }

    [Theory]
    [InlineData("warn", 0)]
    [InlineData("stop", ConversionParityPostPass.LowScoreExitCode)]
    public async Task RunAsync_SetsExitCodeOnlyWhenGateIsStop(string gate, int expectedExitCode)
    {
        Environment.SetEnvironmentVariable("ON_LOW_SCORE", gate);
        Environment.SetEnvironmentVariable("MIN_PROGRAM_SCORE", "0.9");
        Environment.ExitCode = 0;

        var dir = Path.Combine(Path.GetTempPath(), $"parity-gate-{Guid.NewGuid():N}");
        Directory.CreateDirectory(dir);
        try
        {
            var path = Path.Combine(dir, "Thing.java");
            // A guard stub always scores 0, so it is below any non-zero threshold.
            await File.WriteAllTextAsync(path, "// CONVERSION DID NOT PRODUCE USABLE OUTPUT");

            await ConversionParityPostPass.RunAsync(
                [new CobolToQuarkusMigration.Models.CodeFile
                {
                    FileName = "Thing.java", FilePath = path,
                    OriginalCobolFileName = "THING.cbl", Content = "x"
                }],
                dir, "Java");

            Environment.ExitCode.Should().Be(expectedExitCode);
            File.Exists(Path.Combine(dir, ConversionParityPostPass.ArtifactName))
                .Should().BeTrue("the gate must not suppress the artifact");
        }
        finally
        {
            Directory.Delete(dir, recursive: true);
        }
    }

    [Fact]
    public void ReadThreshold_Unset_UsesDefault()
    {
        Environment.SetEnvironmentVariable("MIN_PROGRAM_SCORE", null);

        ConversionParityPostPass.ReadThreshold().Should().Be(0.75);
    }

    [Theory]
    [InlineData("0.9", 0.9)]
    [InlineData("0", 0.0)]
    [InlineData("1", 1.0)]
    [InlineData("2.5", 1.0)]
    [InlineData("-3", 0.0)]
    public void ReadThreshold_ParsesAndClamps(string raw, double expected)
    {
        Environment.SetEnvironmentVariable("MIN_PROGRAM_SCORE", raw);

        ConversionParityPostPass.ReadThreshold().Should().Be(expected);
    }

    [Fact]
    public void ReadThreshold_Unparseable_FallsBackToDefault()
    {
        Environment.SetEnvironmentVariable("MIN_PROGRAM_SCORE", "high");

        ConversionParityPostPass.ReadThreshold().Should().Be(0.75);
    }

    [Theory]
    [InlineData(null, ConversionParityPostPass.ParityGate.Warn)]
    [InlineData("warn", ConversionParityPostPass.ParityGate.Warn)]
    [InlineData("stop", ConversionParityPostPass.ParityGate.Stop)]
    [InlineData("STOP", ConversionParityPostPass.ParityGate.Stop)]
    [InlineData("abort", ConversionParityPostPass.ParityGate.Warn)]
    internal void ReadGate_ParsesKnownValuesAndDefaultsToWarn(
        string? raw, ConversionParityPostPass.ParityGate expected)
    {
        Environment.SetEnvironmentVariable("ON_LOW_SCORE", raw);

        ConversionParityPostPass.ReadGate().Should().Be(expected);
    }

    [Fact]
    public void BuildReport_NotEvaluatedProgramsAreExcludedFromTheAverage()
    {
        var report = ConversionParityPostPass.BuildReport(
            new List<ProgramParityResult>
            {
                Evaluated("A.cbl", 1.0),
                Evaluated("B.cbl", 0.5),
                new() { Program = "C.cbl", Outcome = ParityOutcome.NotEvaluated, NotEvaluatedReason = "no context" },
            },
            "Java", 0.75, ConversionParityPostPass.ParityGate.Warn);

        report.EvaluatedCount.Should().Be(2);
        report.NotEvaluatedCount.Should().Be(1);
        report.AverageScore.Should().Be(0.75);
        report.BelowThresholdCount.Should().Be(1);
    }

    [Fact]
    public void BuildMarkdown_ListsNotEvaluatedProgramsRatherThanImplyingSuccess()
    {
        var report = ConversionParityPostPass.BuildReport(
            new List<ProgramParityResult>
            {
                Evaluated("A.cbl", 1.0),
                new() { Program = "C.cbl", Outcome = ParityOutcome.NotEvaluated, NotEvaluatedReason = "no structural context" },
            },
            "Java", 0.75, ConversionParityPostPass.ParityGate.Warn);

        var markdown = ConversionParityPostPass.BuildMarkdown(report);

        markdown.Should().Contain("Not evaluated");
        markdown.Should().Contain("C.cbl");
        markdown.Should().Contain("no structural context");
        markdown.Should().Contain("not evidence of a good conversion");
    }

    [Fact]
    public void BuildMarkdown_NothingEvaluated_DoesNotClaimSuccess()
    {
        var report = ConversionParityPostPass.BuildReport(
            new List<ProgramParityResult>
            {
                new() { Program = "C.cbl", Outcome = ParityOutcome.NotEvaluated, NotEvaluatedReason = "no context" },
            },
            "Java", 0.75, ConversionParityPostPass.ParityGate.Warn);

        var markdown = ConversionParityPostPass.BuildMarkdown(report);

        markdown.Should().Contain("No program could be evaluated");
        markdown.Should().NotContain("meet the threshold");
        markdown.Should().Contain("doctor.sh rekt-full");
    }

    [Fact]
    public void BuildMarkdown_BelowThreshold_NamesTheMissingSymbols()
    {
        var low = new ProgramParityResult
        {
            Program = "B.cbl",
            Outcome = ParityOutcome.Evaluated,
            Score = 0.4,
            GeneratedFile = "/tmp/out/B.java",
            Gaps = new List<ParityGap>
            {
                new() { Axis = "procedures", Symbol = "2000-PROCESS-CUSTOMER", Kind = ParityGapKind.Missing },
                new() { Axis = "sqlTables", Symbol = "CUSTOMER_MASTER", Kind = ParityGapKind.Missing, Detail = "SELECT, UPDATE" },
                new() { Axis = "procedures", Symbol = "3000-EXIT", Kind = ParityGapKind.PossiblyRenamedOrMerged },
            },
        };

        var report = ConversionParityPostPass.BuildReport(
            new List<ProgramParityResult> { low }, "Java", 0.75, ConversionParityPostPass.ParityGate.Warn);

        var markdown = ConversionParityPostPass.BuildMarkdown(report);

        markdown.Should().Contain("2000-PROCESS-CUSTOMER");
        markdown.Should().Contain("CUSTOMER_MASTER");
        markdown.Should().Contain("SELECT, UPDATE");
        // Comment-only survivors are not losses, so they must not appear in the missing list.
        markdown.Should().NotContain("3000-EXIT");
    }

    [Fact]
    public void BuildMarkdown_StatesThatParityIsStructuralNotBehavioural()
    {
        var report = ConversionParityPostPass.BuildReport(
            new List<ProgramParityResult> { Evaluated("A.cbl", 1.0) },
            "Java", 0.75, ConversionParityPostPass.ParityGate.Warn);

        ConversionParityPostPass.BuildMarkdown(report)
            .Should().Contain("not behavioural equivalence");
    }

    private static ProgramParityResult Evaluated(string program, double score) => new()
    {
        Program = program,
        Outcome = ParityOutcome.Evaluated,
        Score = score,
    };
}
