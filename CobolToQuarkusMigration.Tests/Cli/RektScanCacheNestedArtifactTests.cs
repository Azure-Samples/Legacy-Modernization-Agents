using CobolToQuarkusMigration.Cli;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Cli;

public sealed class RektScanCacheNestedArtifactTests : IDisposable
{
    private readonly string _directory = Path.Combine(
        AppContext.BaseDirectory,
        "test-artifacts",
        $"rekt-artifacts-nested-{Guid.NewGuid():N}");

    public RektScanCacheNestedArtifactTests()
    {
        Directory.CreateDirectory(_directory);
    }

    [Fact]
    public void HasRektArtifacts_NestedReportDirectoryCounts()
    {
        var reportDir = Path.Combine(_directory, "finance", "ACCOUNTS.cbl.report", "flow_ast");
        Directory.CreateDirectory(reportDir);
        File.WriteAllText(Path.Combine(reportDir, "flow-ast-ACCOUNTS.cbl.json"), "{}");

        RektScanCacheCommand.HasRektArtifacts(_directory, "ACCOUNTS.cbl")
            .Should().BeTrue();
    }

    public void Dispose()
    {
        if (Directory.Exists(_directory))
            Directory.Delete(_directory, recursive: true);
    }
}
