using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

// Every run wrote to output/csharp, so a second run overwrote the first: two runs could not be
// compared and a demo could not be kept. Dating the run folder only helps if a reader can still
// find the current output without being told the timestamp, because the portal starts
// independently of any conversion.
public sealed class ConversionOutputFolderTests : IDisposable
{
    private const string Marker = "conversion-parity.json";

    private readonly string _root = Path.Join(
        Path.GetTempPath(), "output-folder-" + Guid.NewGuid().ToString("N"));

    public ConversionOutputFolderTests() => Directory.CreateDirectory(_root);

    public void Dispose()
    {
        try { Directory.Delete(_root, recursive: true); }
        catch (IOException ex) { Console.Error.WriteLine($"Could not remove {_root}: {ex.Message}"); }
    }

    private void Run(string language, string stamp, bool withMarker = true)
    {
        var dir = Path.Join(_root, "output", language, stamp);
        Directory.CreateDirectory(dir);
        if (withMarker) File.WriteAllText(Path.Join(dir, Marker), "{}");
    }

    private string Resolve(string language = "csharp") =>
        ConversionOutputFolder.ResolveLatest(_root, Path.Join("output", language), Marker);

    [Fact]
    public void ARunNameSortsChronologicallyAsText()
    {
        var earlier = ConversionOutputFolder.NewRunName(new DateTimeOffset(2026, 9, 17, 8, 5, 0, TimeSpan.Zero));
        var later = ConversionOutputFolder.NewRunName(new DateTimeOffset(2026, 9, 17, 14, 5, 0, TimeSpan.Zero));

        earlier.Should().Be("20260917-080500");
        string.CompareOrdinal(earlier, later).Should().BeNegative();
    }

    [Fact]
    public void TheNewestRunIsResolved()
    {
        Run("csharp", "20260917-080000");
        Run("csharp", "20260917-140000");
        Run("csharp", "20260916-235959");

        Resolve().Should().Be(Path.Join("output", "csharp", "20260917-140000"));
    }

    // Output produced before runs were dated sits flat in the language folder and must still work.
    [Fact]
    public void FlatOutputFromBeforeThisChangeIsStillFound()
    {
        var flat = Path.Join(_root, "output", "csharp");
        Directory.CreateDirectory(flat);
        File.WriteAllText(Path.Join(flat, Marker), "{}");

        Resolve().Should().Be(Path.Join("output", "csharp"));
    }

    // A generated package directory is not a run.
    [Fact]
    public void ADirectoryThatIsNotARunStampIsIgnored()
    {
        Directory.CreateDirectory(Path.Join(_root, "output", "csharp", "Modernized"));
        File.WriteAllText(Path.Join(_root, "output", "csharp", "Modernized", Marker), "{}");

        Resolve().Should().Be(Path.Join("output", "csharp"));
    }

    // A run that failed before writing its report should not shadow the last good one.
    [Fact]
    public void ARunFolderWithoutItsReportIsNotTheAnswer()
    {
        Run("csharp", "20260917-080000");
        Run("csharp", "20260917-140000", withMarker: false);

        Resolve().Should().Be(Path.Join("output", "csharp", "20260917-080000"));
    }

    [Fact]
    public void AMissingLanguageFolderResolvesToItself()
    {
        Resolve("java").Should().Be(Path.Join("output", "java"));
    }

    [Fact]
    public void LanguagesDoNotSeeEachOthersRuns()
    {
        Run("csharp", "20260917-140000");
        Run("java", "20260101-000000");

        Resolve("csharp").Should().Be(Path.Join("output", "csharp", "20260917-140000"));
        Resolve("java").Should().Be(Path.Join("output", "java", "20260101-000000"));
    }

    [Fact]
    public void EveryRunIsListedNewestFirst()
    {
        Run("csharp", "20260917-080000");
        Run("csharp", "20260917-140000");
        Directory.CreateDirectory(Path.Join(_root, "output", "csharp", "Modernized"));

        ConversionOutputFolder.RunsIn(_root, Path.Join("output", "csharp"))
            .Should().ContainInOrder("20260917-140000", "20260917-080000")
            .And.NotContain("Modernized");
    }

    [Theory]
    [InlineData("20260917-140000", true)]
    [InlineData("Modernized", false)]
    [InlineData("20260917140000", false)]   // no separator
    [InlineData("20260917-14000", false)]   // too short
    [InlineData("2026091x-140000", false)]  // not all digits
    [InlineData(null, false)]
    public void ARunStampIsRecognisedByShape(string? name, bool expected)
    {
        ConversionOutputFolder.LooksLikeRun(name).Should().Be(expected);
    }
}
