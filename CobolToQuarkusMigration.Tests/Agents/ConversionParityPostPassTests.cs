using System.Globalization;
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

    [Fact]
    public async Task RunAsync_StopGateFailsWhenNothingCouldBeEvaluated()
    {
        Environment.SetEnvironmentVariable("ON_LOW_SCORE", "stop");
        Environment.ExitCode = 0;

        var dir = Path.Combine(Path.GetTempPath(), $"parity-noeval-{Guid.NewGuid():N}");
        Directory.CreateDirectory(dir);
        try
        {
            var path = Path.Combine(dir, "Thing.java");
            // Real output, but no structural context in a temp dir, so parity is unmeasurable.
            await File.WriteAllTextAsync(path, "public class Thing { void run() { } }");

            await ConversionParityPostPass.RunAsync(
                [new CobolToQuarkusMigration.Models.CodeFile
                {
                    FileName = "Thing.java", FilePath = path,
                    OriginalCobolFileName = "THING.cbl", Content = "x"
                }],
                dir, "Java");

            var json = await File.ReadAllTextAsync(Path.Combine(dir, ConversionParityPostPass.ArtifactName));
            json.Should().Contain("\"NotEvaluated\"", "the premise is that nothing was evaluable");

            Environment.ExitCode.Should().Be(
                ConversionParityPostPass.LowScoreExitCode,
                "exiting 0 would report no parity failures when parity was never measured");
        }
        finally
        {
            Directory.Delete(dir, recursive: true);
        }
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
    // The threshold the report prints is meant to be copied into MIN_PROGRAM_SCORE, which is
    // parsed invariantly. Under a comma-decimal culture a localised value would not round-trip.
    public void BuildMarkdown_FormatsNumbersInvariantlyUnderAnyCulture()
    {
        var original = CultureInfo.CurrentCulture;
        try
        {
            CultureInfo.CurrentCulture = new CultureInfo("de-DE");

            var report = ConversionParityPostPass.BuildReport(
                new List<ProgramParityResult> { Evaluated("A.cbl", 0.94) },
                "Java", 0.75, ConversionParityPostPass.ParityGate.Warn);

            var markdown = ConversionParityPostPass.BuildMarkdown(report);

            markdown.Should().Contain("MIN_PROGRAM_SCORE=0.75");
            markdown.Should().Contain("0.94");
            markdown.Should().NotContain("0,75");
            markdown.Should().NotContain("0,94");
        }
        finally
        {
            CultureInfo.CurrentCulture = original;
        }
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

    [Fact]
    // Chunked assembly emits one file per generated class, all carrying the same source. Scoring
    // them individually reported a correct service/DTO split as several deficient conversions.
    public async Task RunAsync_ScoresAllFilesOfOneProgramTogether()
    {
        Environment.SetEnvironmentVariable("ON_LOW_SCORE", "warn");
        Environment.SetEnvironmentVariable("MIN_PROGRAM_SCORE", "0.75");

        var dir = Path.Combine(Path.GetTempPath(), $"parity-group-{Guid.NewGuid():N}");
        Directory.CreateDirectory(dir);
        try
        {
            var service = Path.Combine(dir, "CustomerService.java");
            var dto = Path.Combine(dir, "CustomerDto.java");
            await File.WriteAllTextAsync(service, "class CustomerService { void mainLogic() {} }");
            await File.WriteAllTextAsync(dto, "class CustomerDto { int customerTotal; }");

            await ConversionParityPostPass.RunAsync(
                [Generated("CustomerService.java", service, "CUSTOMER-INQUIRY.cbl"),
                 Generated("CustomerDto.java", dto, "CUSTOMER-INQUIRY.cbl")],
                dir, "Java");

            var report = await ReadReportAsync(dir);
            report.Programs.Should().ContainSingle()
                .Which.Program.Should().Be("CUSTOMER-INQUIRY.cbl");
        }
        finally
        {
            Directory.Delete(dir, recursive: true);
        }
    }

    [Fact]
    // A source that produced no output at all is the worst parity failure there is, and it was
    // invisible while the report only walked generated files.
    public async Task RunAsync_ReportsSourceProgramsThatProducedNoOutput()
    {
        Environment.SetEnvironmentVariable("ON_LOW_SCORE", "stop");
        Environment.SetEnvironmentVariable("MIN_PROGRAM_SCORE", "0.75");
        Environment.ExitCode = 0;

        var dir = Path.Combine(Path.GetTempPath(), $"parity-missing-{Guid.NewGuid():N}");
        Directory.CreateDirectory(dir);
        try
        {
            var path = Path.Combine(dir, "Converted.java");
            await File.WriteAllTextAsync(path, "class Converted {}");

            await ConversionParityPostPass.RunAsync(
                [Generated("Converted.java", path, "CONVERTED.cbl")],
                dir, "Java",
                sourcePrograms: new[] { "CONVERTED.cbl", "NEVER-CONVERTED.cbl" });

            var report = await ReadReportAsync(dir);
            var missing = report.Programs.Single(p => p.Program == "NEVER-CONVERTED.cbl");

            missing.Outcome.Should().Be(ParityOutcome.Evaluated);
            missing.Score.Should().Be(0);
            missing.Gaps.Should().ContainSingle()
                .Which.Detail.Should().Contain("no output file");

            Environment.ExitCode.Should().Be(ConversionParityPostPass.LowScoreExitCode);
        }
        finally
        {
            Directory.Delete(dir, recursive: true);
        }
    }

    [Theory]
    [InlineData("NaN")]
    [InlineData("Infinity")]
    // Every comparison against NaN is false, so an unparseable-but-accepted threshold silently
    // disabled the gate rather than falling back to the default.
    public void ReadThreshold_RejectsNonFiniteValues(string raw)
    {
        Environment.SetEnvironmentVariable("MIN_PROGRAM_SCORE", raw);
        ConversionParityPostPass.ReadThreshold().Should().Be(0.75);
    }

    private static CobolToQuarkusMigration.Models.CodeFile Generated(
        string name, string path, string program) => new()
    {
        FileName = name,
        FilePath = path,
        OriginalCobolFileName = program,
        Content = "x",
    };

    private static async Task<ConversionParityReport> ReadReportAsync(string dir)
    {
        var json = await System.IO.File.ReadAllTextAsync(
            Path.Combine(dir, ConversionParityPostPass.ArtifactName));
        return System.Text.Json.JsonSerializer.Deserialize<ConversionParityReport>(json)!;
    }

    private static ProgramParityResult Evaluated(string program, double score) => new()
    {
        Program = program,
        Outcome = ParityOutcome.Evaluated,
        Score = score,
    };
}
