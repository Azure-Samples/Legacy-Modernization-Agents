using CobolToQuarkusMigration.Cli;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Cli;

public sealed class RektScanCacheCommandTests : IDisposable
{
    private readonly string _directory =
        Path.Combine(Path.GetTempPath(), $"rekt-artifacts-{Guid.NewGuid():N}");

    public RektScanCacheCommandTests()
    {
        Directory.CreateDirectory(_directory);
    }

    [Fact]
    public void HasRektArtifacts_DerivedFactsFileDoesNotCount()
    {
        File.WriteAllText(Path.Combine(_directory, "FOO.facts.json"), "{}");

        RektScanCacheCommand.HasRektArtifacts(_directory, "FOO.cbl")
            .Should().BeFalse();
    }

    [Fact]
    public void HasRektArtifacts_OtherProgramPrefixDoesNotCount()
    {
        File.WriteAllText(Path.Combine(_directory, "flow-ast-FOOBAR.json"), "{}");

        RektScanCacheCommand.HasRektArtifacts(_directory, "FOO.cbl")
            .Should().BeFalse();
    }

    [Fact]
    public void HasRektArtifacts_ExactFlatArtifactCounts()
    {
        File.WriteAllText(Path.Combine(_directory, "flow-ast-FOO.json"), "{}");

        RektScanCacheCommand.HasRektArtifacts(_directory, "FOO.cbl")
            .Should().BeTrue();
    }

    [Fact]
    public void HasRektArtifacts_ExactReportDirectoryCounts()
    {
        var reportDir = Path.Combine(_directory, "FOO.cbl.report", "flow_ast");
        Directory.CreateDirectory(reportDir);
        File.WriteAllText(Path.Combine(reportDir, "flow-ast-FOO.cbl.json"), "{}");

        RektScanCacheCommand.HasRektArtifacts(_directory, "FOO.cbl")
            .Should().BeTrue();
    }

    public void Dispose()
    {
        Directory.Delete(_directory, recursive: true);
    }
}
