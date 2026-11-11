using System.Globalization;
using CobolToQuarkusMigration.Agents;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Agents;

[Collection("EnvironmentSensitive")]
public class ConversionParityPostPassTests : IDisposable
{
    private readonly string? _originalThreshold = Environment.GetEnvironmentVariable("MIN_PROGRAM_SCORE");
    private readonly string? _originalGate = Environment.GetEnvironmentVariable("ON_LOW_SCORE");
    private readonly int _originalExitCode = Environment.ExitCode;
    private readonly string? _originalRepairRounds = Environment.GetEnvironmentVariable("PARITY_REPAIR_MAX_ROUNDS");

    public void Dispose()
    {
        Environment.SetEnvironmentVariable("MIN_PROGRAM_SCORE", _originalThreshold);
        Environment.SetEnvironmentVariable("ON_LOW_SCORE", _originalGate);
        Environment.ExitCode = _originalExitCode;
        Environment.SetEnvironmentVariable("PARITY_REPAIR_MAX_ROUNDS", _originalRepairRounds);
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public async Task Repair_RevalidatesCandidateAndPreservesOriginalWhenItDoesNotImprove(bool improves)
    {
        var path = Path.Combine(Path.GetTempPath(), $"parity-repair-{Guid.NewGuid():N}.java");
        Environment.SetEnvironmentVariable("PARITY_REPAIR_MAX_ROUNDS", "2");
        var before = "package example; class Record { int existingValue; }";
        var candidate = improves
            ? "package example; class Record { int existingValue; int requiredValue; }"
            : "package example; class Record { int existingValue; /* REQUIRED-VALUE */ }";
        var context = new CobolToQuarkusMigration.Helpers.StructuralContext
        {
            Provenance = CobolToQuarkusMigration.Helpers.StructuralProvenance.SourceDeclared,
            Context = new()
            {
                IsCopybook = true,
                DataStructure = [new() { Level = 5, Name = "REQUIRED-VALUE" }]
            }
        };
        ProgramParityResult Evaluate(IReadOnlyDictionary<string, string> files) =>
            ConversionParityValidator.Evaluate("RECORD.cpy", path,
                string.Join("\n", files.Values), context, null);
        var current = new Dictionary<string, string> { [path] = before };
        var calls = 0;
        try
        {
            await File.WriteAllTextAsync(path, before);
            var result = await ConversionParityPostPass.RepairAsync(Evaluate(current), "05 REQUIRED-VALUE PIC 9.",
                current, "Java", 0.75, request =>
                {
                    request.Source.Should().Be("05 REQUIRED-VALUE PIC 9.");
                    calls++;
                    return Task.FromResult<IReadOnlyDictionary<string, string>?>(
                        new Dictionary<string, string> { [path] = candidate });
                }, Evaluate);
            (await File.ReadAllTextAsync(path)).Should().Be(improves ? candidate : before);
            result.AcceptedRepairs.Should().Be(improves ? 1 : 0);
            calls.Should().Be(improves ? 1 : 2);
            result.RepairAttempts.Should().Be(calls);
        }
        finally { File.Delete(path); }
    }

    [Fact]
    public async Task RunAsync_StrictGateRejectsPartiallyUnknownEvidenceAndArtifactWriteFailure()
    {
        var root = Path.Combine(Path.GetTempPath(), $"parity-strict-{Guid.NewGuid():N}");
        var sourceRoot = Path.Combine(root, "source");
        Directory.CreateDirectory(sourceRoot);
        Environment.SetEnvironmentVariable("ON_LOW_SCORE", "stop");
        Environment.ExitCode = 0;
        try
        {
            var knownSource = Path.Combine(sourceRoot, "KNOWN.cpy");
            await File.WriteAllTextAsync(knownSource, "       01 RECORD.\n          05 VALUE PIC 9.");
            var knownOutput = Path.Combine(root, "Known.java");
            var unknownOutput = Path.Combine(root, "Unknown.java");
            await File.WriteAllTextAsync(knownOutput, "class Known { int value; }");
            await File.WriteAllTextAsync(unknownOutput, "class Unknown { void run() {} }");
            var files = new[]
            {
                new CobolToQuarkusMigration.Models.CodeFile { FilePath = knownOutput, OriginalSourcePath = knownSource },
                new CobolToQuarkusMigration.Models.CodeFile
                    { FilePath = unknownOutput, OriginalSourcePath = Path.Combine(sourceRoot, "UNKNOWN.cbl") }
            };
            await ConversionParityPostPass.RunAsync(files, root, "Java",
                sourcePrograms: files.Select(file => file.OriginalSourcePath), repositoryRoot: root, sourceDirectory: sourceRoot);
            var report = await ReadReportAsync(root);
            report.EvaluatedCount.Should().Be(1);
            report.NotEvaluatedCount.Should().Be(1);
            report.BelowThresholdCount.Should().Be(0);
            Environment.ExitCode.Should().Be(ConversionParityPostPass.LowScoreExitCode);
            File.Delete(Path.Combine(root, ConversionParityPostPass.ArtifactName));
            Directory.CreateDirectory(Path.Combine(root, ConversionParityPostPass.ArtifactName));
            await FluentActions.Awaiting(() => ConversionParityPostPass.RunAsync(files, root, "Java",
                repositoryRoot: root, sourceDirectory: sourceRoot)).Should().ThrowAsync<Exception>()
                .Where(exception => exception is IOException || exception is UnauthorizedAccessException);
        }
        finally { Directory.Delete(root, true); }
    }

    [Fact]
    public async Task RunAsync_PathMetadataSeparatesSameNamedSourcesAndScoresCopybookFields()
    {
        var root = Path.Combine(Path.GetTempPath(), $"parity-identity-{Guid.NewGuid():N}");
        try
        {
            Environment.SetEnvironmentVariable("ON_LOW_SCORE", "warn");
            var generated = new List<CobolToQuarkusMigration.Models.CodeFile>();
            foreach (var (directory, field) in new[] { ("first", "FIRST-VALUE"), ("second", "SECOND-VALUE") })
            {
                var source = Path.Combine(root, "source", directory, "SHARED.cpy");
                Directory.CreateDirectory(Path.GetDirectoryName(source)!);
                await File.WriteAllTextAsync(source, $"       01 RECORD.\n          05 {field} PIC 9(4).\n");
                var output = Path.Combine(root, directory + ".java");
                await File.WriteAllTextAsync(output, $"class Record {{ int {field.Replace("-", "")}; }}");
                generated.Add(new()
                {
                    FilePath = output,
                    OriginalCobolFileName = "SHARED.cpy",
                    OriginalSourcePath = source
                });
            }
            await ConversionParityPostPass.RunAsync(generated, root, "Java",
                sourcePrograms: generated.Select(f => f.OriginalSourcePath), repositoryRoot: root,
                sourceDirectory: Path.Combine(root, "source"));
            var report = await ReadReportAsync(root);
            report.Programs.Select(p => p.Program).Should().BeEquivalentTo("first/SHARED.cpy", "second/SHARED.cpy");
            report.Programs.Should().OnlyContain(p => p.Outcome == ParityOutcome.Evaluated && p.Score == 1);
            report.Programs.Should().OnlyContain(p => p.Axes.Single(a => a.Name == "procedures").Expected == 0);
            using var map = System.Text.Json.JsonDocument.Parse(
                await File.ReadAllTextAsync(Path.Combine(root, "generated-source-map.json")));
            map.RootElement.EnumerateArray().Select(row => row.GetProperty("source").GetString())
                .Should().BeEquivalentTo("first/SHARED.cpy", "second/SHARED.cpy");
            map.RootElement.EnumerateArray().Select(row => row.GetProperty("kind").GetString())
                .Should().OnlyContain(kind => kind == "copybook");
        }
        finally { if (Directory.Exists(root)) Directory.Delete(root, true); }
    }

    [Fact]
    public async Task RunAsync_RefusesToScoreOverwrittenOutputAgainstMultipleSources()
    {
        var root = Path.Combine(Path.GetTempPath(), $"parity-collision-{Guid.NewGuid():N}");
        Directory.CreateDirectory(root);
        Environment.SetEnvironmentVariable("ON_LOW_SCORE", "warn");
        try
        {
            var output = Path.Combine(root, "Shared.java");
            await File.WriteAllTextAsync(output, "class Shared { int survivingValue; }");
            var files = new[]
            {
                new CobolToQuarkusMigration.Models.CodeFile { FilePath = output, OriginalSourcePath = Path.Combine(root, "first", "SHARED.cpy") },
                new CobolToQuarkusMigration.Models.CodeFile { FilePath = output, OriginalSourcePath = Path.Combine(root, "second", "SHARED.cpy") }
            };
            await ConversionParityPostPass.RunAsync(files, root, "Java",
                sourcePrograms: files.Select(file => file.OriginalSourcePath), repositoryRoot: root,
                sourceDirectory: root);
            var report = await ReadReportAsync(root);
            report.Programs.Should().HaveCount(2).And.OnlyContain(program =>
                program.Outcome == ParityOutcome.NotEvaluated && program.Score == null
                && program.NotEvaluatedReason!.Contains("multiple source identities"));
        }
        finally { Directory.Delete(root, true); }
    }

    [Fact]
    // Generated files record only the source basename, so two programs of the same name in
    // different folders merged into one entry and the second vanished from the report.
    public async Task RunAsync_RefusesToMergeProgramsSharingAFileName()
    {
        Environment.SetEnvironmentVariable("ON_LOW_SCORE", "warn");

        var dir = Path.Join(Path.GetTempPath(), $"parity-dupe-{Guid.NewGuid():N}");
        Directory.CreateDirectory(dir);
        try
        {
            var billing = Path.Join(dir, "BillingCustomer.java");
            var claims = Path.Join(dir, "ClaimsCustomer.java");
            await File.WriteAllTextAsync(billing, "class BillingCustomer { void billIt() {} }");
            await File.WriteAllTextAsync(claims, "class ClaimsCustomer { void claimIt() {} }");

            // Production passes cobolFiles.Select(f => f.FileName), and FileName is a basename,
            // so two programs in different folders arrive here as the same string twice.
            await ConversionParityPostPass.RunAsync(
                [Generated("BillingCustomer.java", billing, "CUSTOMER.cbl"),
                 Generated("ClaimsCustomer.java", claims, "CUSTOMER.cbl")],
                dir, "Java", null, ["CUSTOMER.cbl", "CUSTOMER.cbl"], repositoryRoot: dir);

            var program = (await ReadReportAsync(dir)).Programs.Should().ContainSingle().Subject;
            program.Outcome.Should().Be(ParityOutcome.NotEvaluated);
            program.NotEvaluatedReason.Should().Contain("2 source programs share the file name");
            program.Score.Should().BeNull();
        }
        finally
        {
            Directory.Delete(dir, true);
        }
    }

    [Fact]
    // Guards the test above: the same shape with one source program must still be measured,
    // or the collision check would be silently suppressing legitimate chunked splits.
    public async Task RunAsync_StillScoresOneProgramSplitAcrossFiles()
    {
        Environment.SetEnvironmentVariable("ON_LOW_SCORE", "warn");

        var dir = Path.Join(Path.GetTempPath(), $"parity-split-{Guid.NewGuid():N}");
        Directory.CreateDirectory(dir);
        try
        {
            var billing = Path.Join(dir, "BillingCustomer.java");
            var claims = Path.Join(dir, "ClaimsCustomer.java");
            await File.WriteAllTextAsync(billing, "class BillingCustomer { void billIt() {} }");
            await File.WriteAllTextAsync(claims, "class ClaimsCustomer { void claimIt() {} }");

            await ConversionParityPostPass.RunAsync(
                [Generated("BillingCustomer.java", billing, "CUSTOMER.cbl"),
                 Generated("ClaimsCustomer.java", claims, "CUSTOMER.cbl")],
                dir, "Java", null, ["CUSTOMER.cbl"], repositoryRoot: dir);

            var program = (await ReadReportAsync(dir)).Programs.Should().ContainSingle().Subject;
            program.NotEvaluatedReason.Should().NotContain("share the file name");
        }
        finally
        {
            Directory.Delete(dir, true);
        }
    }

    [Fact]
    public async Task RunAsync_StopGateFailsWhenNothingCouldBeEvaluated()
    {
        Environment.SetEnvironmentVariable("ON_LOW_SCORE", "stop");
        Environment.ExitCode = 0;

        var dir = Path.Join(Path.GetTempPath(), $"parity-noeval-{Guid.NewGuid():N}");
        Directory.CreateDirectory(dir);
        try
        {
            var path = Path.Join(dir, "Thing.java");
            // Real output, but no structural context in a temp dir, so parity is unmeasurable.
            await File.WriteAllTextAsync(path, "public class Thing { void run() { } }");

            await ConversionParityPostPass.RunAsync(
                [new CobolToQuarkusMigration.Models.CodeFile
                {
                    FileName = "Thing.java", FilePath = path,
                    OriginalCobolFileName = "THING.cbl", Content = "x"
                }],
                dir, "Java", repositoryRoot: dir);

            var json = await File.ReadAllTextAsync(Path.Join(dir, ConversionParityPostPass.ArtifactName));
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

        var dir = Path.Join(Path.GetTempPath(), $"parity-gate-{Guid.NewGuid():N}");
        Directory.CreateDirectory(dir);
        try
        {
            var path = Path.Join(dir, "Thing.java");
            // A guard stub always scores 0, so it is below any non-zero threshold.
            await File.WriteAllTextAsync(path, "// CONVERSION DID NOT PRODUCE USABLE OUTPUT");

            await ConversionParityPostPass.RunAsync(
                [new CobolToQuarkusMigration.Models.CodeFile
                {
                    FileName = "Thing.java", FilePath = path,
                    OriginalCobolFileName = "THING.cbl", Content = "x"
                }],
                dir, "Java", repositoryRoot: dir);

            Environment.ExitCode.Should().Be(expectedExitCode);
            File.Exists(Path.Join(dir, ConversionParityPostPass.ArtifactName))
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

        var dir = Path.Join(Path.GetTempPath(), $"parity-group-{Guid.NewGuid():N}");
        Directory.CreateDirectory(dir);
        try
        {
            var service = Path.Join(dir, "CustomerService.java");
            var dto = Path.Join(dir, "CustomerDto.java");
            await File.WriteAllTextAsync(service, "class CustomerService { void mainLogic() {} }");
            await File.WriteAllTextAsync(dto, "class CustomerDto { int customerTotal; }");

            await ConversionParityPostPass.RunAsync(
                [Generated("CustomerService.java", service, "CUSTOMER-INQUIRY.cbl"),
                 Generated("CustomerDto.java", dto, "CUSTOMER-INQUIRY.cbl")],
                dir, "Java", repositoryRoot: dir);

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

        var dir = Path.Join(Path.GetTempPath(), $"parity-missing-{Guid.NewGuid():N}");
        Directory.CreateDirectory(dir);
        try
        {
            var path = Path.Join(dir, "Converted.java");
            await File.WriteAllTextAsync(path, "class Converted {}");

            await ConversionParityPostPass.RunAsync(
                [Generated("Converted.java", path, "CONVERTED.cbl")],
                dir, "Java",
                sourcePrograms: new[] { "CONVERTED.cbl", "NEVER-CONVERTED.cbl" }, repositoryRoot: dir);

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
            Path.Join(dir, ConversionParityPostPass.ArtifactName));
        return System.Text.Json.JsonSerializer.Deserialize<ConversionParityReport>(json)!;
    }

    private static ProgramParityResult Evaluated(string program, double score) => new()
    {
        Program = program,
        Outcome = ParityOutcome.Evaluated,
        Score = score,
    };
}
