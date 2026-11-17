using CobolToQuarkusMigration.Persistence.GraphIngest;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Persistence.GraphIngest;

public sealed class GraphSourcePathsTests
{
    [Theory]
    [InlineData("/source/finance/batch/ACCOUNTS.cbl", "finance/batch/ACCOUNTS.cbl")]
    [InlineData("/source/ACCOUNTS.cbl", "ACCOUNTS.cbl")]
    public void SourceRelativePathKeepsTheFolders(string file, string expected) =>
        GraphSourcePaths.SourceRelativePath(file, "/source").Should().Be(expected);

    [Fact]
    public void ReportDirectoryPreservesTheNestedSourcePath() =>
        GraphSourcePaths.ArtifactSourcePath(
                "/output/finance/batch/ACCOUNTS.cbl.report/flow-ast-ACCOUNTS.cbl.json", "/output",
                ["finance/batch/ACCOUNTS.cbl", "archive/ACCOUNTS.cbl"])
            .Should().Be("finance/batch/ACCOUNTS.cbl");

    [Fact]
    public void ArtifactNameResolvesAUniqueSourceBasename() =>
        GraphSourcePaths.ArtifactSourcePath(
                "/output/ACCOUNTS.cbl.report/cfg-ACCOUNTS.cbl.json", "/output", ["finance/batch/ACCOUNTS.cbl"])
            .Should().Be("finance/batch/ACCOUNTS.cbl");

    [Fact]
    public void DepsOnlyParseResolvesWithoutTheSuffix() =>
        GraphSourcePaths.ArtifactSourcePath("/output/finance/ACCOUNTS.cbl-deps.json", "/output", ["finance/ACCOUNTS.cbl"])
            .Should().Be("finance/ACCOUNTS.cbl");

    [Fact]
    public void AmbiguousBasenameIsUnresolvedAndNamesEveryFolder()
    {
        string[] sources = ["finance/ACCOUNTS.cbl", "archive/ACCOUNTS.cbl"];
        const string artifact = "/output/ACCOUNTS.cbl.report/flow-ast-ACCOUNTS.cbl.json";

        GraphSourcePaths.ArtifactSourcePath(artifact, "/output", sources).Should().BeNull();
        GraphSourcePaths.UnresolvedArtifactReason(artifact, "/output", sources).Should().Be(
            "ambiguous source: ACCOUNTS.cbl exists in more than one folder (archive/ACCOUNTS.cbl, finance/ACCOUNTS.cbl)");
    }

    [Fact]
    public void UnresolvedReasonReportsAMissingProgram() =>
        GraphSourcePaths.UnresolvedArtifactReason("/output/finance/LEDGER.cbl.report/cfg-LEDGER.cbl.json", "/output",
                ["finance/ACCOUNTS.cbl"])
            .Should().Be("no matching source: expected a program in source/ named LEDGER.cbl or finance/LEDGER.cbl " +
                         "(was it removed, or is this not a parse artifact?)");

    [Fact]
    public void GraphIdentityIsScopedByRunAndProgram()
    {
        var first = GraphSourcePaths.ScopedGraphId(1, "finance/ACCOUNTS.cbl", "ROOT");

        first.Should().Be(GraphSourcePaths.ScopedGraphId(1, "finance/ACCOUNTS.cbl", "ROOT"));
        first.Should().NotBe(GraphSourcePaths.ScopedGraphId(1, "archive/ACCOUNTS.cbl", "ROOT"));
        first.Should().NotBe(GraphSourcePaths.ScopedGraphId(2, "finance/ACCOUNTS.cbl", "ROOT"));
    }
}
