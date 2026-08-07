using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Cli;

public sealed class ProgramFactsCommandTests : IDisposable
{
    private readonly string _root = Path.Combine(
        AppContext.BaseDirectory,
        "test-artifacts",
        $"program-facts-command-{Guid.NewGuid():N}");

    [Fact]
    public void ResolveSelectors_AcceptsExactRelativePathAndUniqueStem()
    {
        var catalog = new ProgramSourceCatalog(new[]
        {
            "finance/ACCOUNTS.cbl",
            "shared/UTILITY.cbl",
        });

        var resolved = catalog.ResolveSelectors("finance/ACCOUNTS.cbl,UTILITY");

        resolved.Should().Equal("finance/ACCOUNTS.cbl", "shared/UTILITY.cbl");
    }

    [Fact]
    public void ResolveSelectors_RejectsAmbiguousBasename()
    {
        var catalog = new ProgramSourceCatalog(new[]
        {
            "finance/ACCOUNTS.cbl",
            "archive/ACCOUNTS.cbl",
        });

        var act = () => catalog.ResolveSelector("ACCOUNTS.cbl");

        act.Should().Throw<InvalidOperationException>()
            .WithMessage("*Use a source-relative path*");
    }

    [Fact]
    public void ResolveFactsFileToProgram_UsesNestedArtifactPath()
    {
        var factsDir = Path.Combine(_root, "facts");
        var artifactPath = Path.Combine(factsDir, "finance", "ACCOUNTS.cbl.facts.json");
        Directory.CreateDirectory(Path.GetDirectoryName(artifactPath)!);
        File.WriteAllText(artifactPath, "{}");

        var catalog = new ProgramSourceCatalog(new[]
        {
            "finance/ACCOUNTS.cbl",
            "shared/UTILITY.cbl",
        });

        var resolved = catalog.ResolveFactsFileToProgram(artifactPath, factsDir);

        resolved.Should().Be("finance/ACCOUNTS.cbl");
    }

    public void Dispose()
    {
        if (Directory.Exists(_root))
            Directory.Delete(_root, recursive: true);
    }
}
