using System;
using System.IO;
using System.Linq;
using System.Text.Json;
using System.Threading.Tasks;
using CobolToQuarkusMigration.Agents;
using McpChatWeb.Services;
using Microsoft.Extensions.Logging.Abstractions;
using Xunit;

namespace McpChatWeb.Tests.Modernization;

public class ConversionParityReaderTests : IDisposable
{
    private readonly string _root = Path.Combine(
        Path.GetTempPath(), "parity-reader-" + Guid.NewGuid().ToString("N"));

    public void Dispose()
    {
        Environment.SetEnvironmentVariable("JAVA_OUTPUT_FOLDER", _originalJavaFolder);
        if (Directory.Exists(_root)) Directory.Delete(_root, recursive: true);
    }

    private readonly string? _originalJavaFolder =
        Environment.GetEnvironmentVariable("JAVA_OUTPUT_FOLDER");

    [Fact]
    // The folder is configurable, so hardcoding output/java made the panel report no data at
    // all for an estate that had been converted into a different folder.
    public async Task ReadAsync_HonoursConfiguredOutputFolder()
    {
        var dir = Path.Combine(_root, "build", "quarkus");
        Directory.CreateDirectory(dir);
        File.WriteAllText(
            Path.Combine(dir, ConversionParityPostPass.ArtifactName),
            SampleReport("Java", 0.9, ("CUSTOMER.cbl", 0.9)));

        // Proves the report is invisible at the default location, so the assertion below can
        // only pass because the configured folder was read.
        Assert.Contains("java", (await Reader().ReadAsync()).MissingTargets);

        Environment.SetEnvironmentVariable("JAVA_OUTPUT_FOLDER", "build/quarkus");

        var estate = await Reader().ReadAsync();
        Assert.DoesNotContain("java", estate.MissingTargets);
        Assert.Single(estate.Reports);
    }

    private ConversionParityReader Reader() =>
        new(NullLogger<ConversionParityReader>.Instance, _root);

    private void WriteReport(string target, string json)
    {
        var dir = Path.Combine(_root, "output", target);
        Directory.CreateDirectory(dir);
        File.WriteAllText(Path.Combine(dir, ConversionParityPostPass.ArtifactName), json);
    }

    private static string SampleReport(string language, double? average, params (string Program, double? Score)[] programs)
    {
        var report = new ConversionParityReport
        {
            TargetLanguage = language,
            Threshold = 0.75,
            OnLowScore = "warn",
            AverageScore = average,
            EvaluatedCount = programs.Count(p => p.Score is not null),
            NotEvaluatedCount = programs.Count(p => p.Score is null),
            Programs = programs.Select(p => new ProgramParityResult
            {
                Program = p.Program,
                GeneratedFile = $"output/{language.ToLowerInvariant()}/{p.Program}.java",
                Outcome = p.Score is null ? ParityOutcome.NotEvaluated : ParityOutcome.Evaluated,
                Score = p.Score,
                NotEvaluatedReason = p.Score is null ? "No structural context." : null,
            }).ToList(),
        };
        return JsonSerializer.Serialize(report);
    }

    [Fact]
    public async Task NoOutputFolders_ReportsBothTargetsMissing()
    {
        var estate = await Reader().ReadAsync();

        Assert.Empty(estate.Reports);
        Assert.Equal(["java", "csharp"], estate.MissingTargets);
        Assert.Empty(estate.UnreadableTargets);
    }

    [Fact]
    public async Task ReadsJavaReport_AndRecordsCsharpAsMissing()
    {
        WriteReport("java", SampleReport("Java", 0.9, ("CUSTOMER.cbl", 0.9)));

        var estate = await Reader().ReadAsync();

        var report = Assert.Single(estate.Reports);
        Assert.Equal("Java", report.TargetLanguage);
        Assert.Equal(0.9, report.AverageScore);
        Assert.Equal(["csharp"], estate.MissingTargets);
    }

    [Fact]
    public async Task ReadsBothTargets()
    {
        WriteReport("java", SampleReport("Java", 0.9, ("CUSTOMER.cbl", 0.9)));
        WriteReport("csharp", SampleReport("C#", 0.6, ("CUSTOMER.cbl", 0.6)));

        var estate = await Reader().ReadAsync();

        Assert.Equal(2, estate.Reports.Count);
        Assert.Empty(estate.MissingTargets);
    }

    [Fact]
    public async Task MalformedJson_IsUnreadableNotMissing()
    {
        WriteReport("java", "{ this is not json");

        var estate = await Reader().ReadAsync();

        Assert.Empty(estate.Reports);
        Assert.Equal(["java"], estate.UnreadableTargets);
        // A corrupt report must not be reported as "never converted".
        Assert.DoesNotContain("java", estate.MissingTargets);
    }

    [Fact]
    public async Task NullJsonLiteral_IsUnreadable()
    {
        WriteReport("java", "null");

        var estate = await Reader().ReadAsync();

        Assert.Empty(estate.Reports);
        Assert.Equal(["java"], estate.UnreadableTargets);
    }

    [Fact]
    public async Task SourcePath_IsRelativeAndIdentifiesTarget()
    {
        WriteReport("csharp", SampleReport("C#", 0.8, ("CUSTOMER.cbl", 0.8)));

        var estate = await Reader().ReadAsync();

        var report = Assert.Single(estate.Reports);
        Assert.Equal(
            Path.Combine("output", "csharp", ConversionParityPostPass.ArtifactName),
            report.SourcePath);
    }

    [Fact]
    public async Task NotEvaluatedPrograms_SurviveTheRoundTrip()
    {
        WriteReport("java", SampleReport("Java", null, ("CUSTOMER.cbl", null)));

        var estate = await Reader().ReadAsync();

        var program = Assert.Single(Assert.Single(estate.Reports).Programs);
        Assert.Equal(ParityOutcome.NotEvaluated, program.Outcome);
        Assert.Null(program.Score);
        Assert.Equal("No structural context.", program.NotEvaluatedReason);
    }

    [Fact]
    public async Task EnumsAreSerialisedAsStrings()
    {
        // The portal switches on the string form; numeric enums would silently break it.
        var report = new ConversionParityReport
        {
            Programs =
            [
                new ProgramParityResult
                {
                    Program = "CUSTOMER.cbl",
                    Outcome = ParityOutcome.Evaluated,
                    Gaps = [new ParityGap { Axis = "procedures", Symbol = "1000-INIT", Kind = ParityGapKind.Missing }],
                }
            ],
        };
        WriteReport("java", JsonSerializer.Serialize(report));

        var json = await File.ReadAllTextAsync(
            Path.Combine(_root, "output", "java", ConversionParityPostPass.ArtifactName));

        Assert.Contains("\"Evaluated\"", json);
        Assert.Contains("\"Missing\"", json);

        var estate = await Reader().ReadAsync();
        Assert.Equal(
            ParityGapKind.Missing,
            Assert.Single(Assert.Single(Assert.Single(estate.Reports).Programs).Gaps).Kind);
    }
}
