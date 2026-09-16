using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

// Focused conversion stages copies of selected programs under source/.conversion-staging. Every
// reader built on this registry sees them, so an unfiltered scratch folder inflates the estate and
// duplicates each staged basename.
public sealed class SourceTypeRegistryTests : IDisposable
{
    private readonly string _source = Path.Combine(
        AppContext.BaseDirectory,
        "test-artifacts",
        $"source-type-registry-{Guid.NewGuid():N}");

    [Theory]
    [InlineData(".conversion-staging")]
    [InlineData(".rekt-staging")]
    [InlineData(".preprocessed")]
    public void ExcludesStagedCopiesFromTheEstate(string scratchFolder)
    {
        WriteProgram("finance/LEDGER.cbl");
        WriteProgram($"{scratchFolder}/finance/LEDGER.cbl");

        var programs = SourceTypeRegistry.EnumerateProgramFiles(_source).ToList();

        programs.Should().ContainSingle()
            .Which.Should().Be(Path.Combine(_source, "finance", "LEDGER.cbl"));
    }

    // The consequence that reaches an operator: a staged copy carries the same basename as its
    // original, so leaving it in the catalog refuses a selector that is unique in the source.
    [Fact]
    public void LeavesABasenameSelectorResolvableAfterAFocusedRunHasStaged()
    {
        WriteProgram("finance/LEDGER.cbl");
        WriteProgram(".conversion-staging/finance/LEDGER.cbl");

        var catalog = ProgramSourceCatalog.FromStagingDirectory(_source);

        catalog.ResolveSelector("LEDGER.cbl").Should().Be("finance/LEDGER.cbl");
    }

    private void WriteProgram(string relativePath)
    {
        var path = Path.Combine(_source, relativePath.Replace('/', Path.DirectorySeparatorChar));
        Directory.CreateDirectory(Path.GetDirectoryName(path)!);
        File.WriteAllText(path, "       IDENTIFICATION DIVISION.\n");
    }

    public void Dispose()
    {
        if (Directory.Exists(_source))
            Directory.Delete(_source, recursive: true);
    }
}
