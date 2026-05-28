using CobolToQuarkusMigration.Agents.Infrastructure.RektCache;
using CobolToQuarkusMigration.Cli;
using FluentAssertions;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Logging.Abstractions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Cli;

/// <summary>
/// Behaviour tests for the doctor.sh-facing helpers in RektScanCacheCommand.
/// The CLI parser itself is owned by System.CommandLine; we test the contract
/// helpers (graph build, artifact verify) that doctor.sh depends on.
/// </summary>
public class RektScanCacheCommandTests : IDisposable
{
    private readonly string _root;

    public RektScanCacheCommandTests()
    {
        _root = Path.Combine(Path.GetTempPath(), $"pr2b-cli-{Guid.NewGuid():N}");
        Directory.CreateDirectory(_root);
    }

    public void Dispose()
    {
        if (Directory.Exists(_root)) Directory.Delete(_root, recursive: true);
    }

    [Fact]
    public void BuildGraphFromStagingDir_PicksUpProgramsCopybooksAndCob()
    {
        var stage = Path.Combine(_root, "stage");
        Directory.CreateDirectory(stage);
        File.WriteAllText(Path.Combine(stage, "PROG.cbl"),
            "       PROGRAM-ID. PROG.\n           COPY BOOK.\n");
        File.WriteAllText(Path.Combine(stage, "OTHER.cob"), "       PROGRAM-ID. OTHER.\n");
        File.WriteAllText(Path.Combine(stage, "BOOK.cpy"), "01 WS-A PIC X.\n");

        var graph = RektScanCacheCommand.BuildGraphFromStagingDir(stage, NullLogger.Instance);

        graph.GetHash("PROG.cbl").Should().NotBeNullOrEmpty();
        graph.GetHash("OTHER.cob").Should().NotBeNullOrEmpty();
        graph.GetHash("BOOK.cpy").Should().NotBeNullOrEmpty();

        var snap = graph.BuildDependencySnapshot("PROG.cbl");
        snap.Should().ContainKey("BOOK.cpy");
    }

    [Fact]
    public void BuildGraphFromStagingDir_ThrowsWhenMissing()
    {
        var act = () => RektScanCacheCommand.BuildGraphFromStagingDir(
            Path.Combine(_root, "nope"), NullLogger.Instance);
        act.Should().Throw<DirectoryNotFoundException>();
    }

    [Fact]
    public void HasRektArtifacts_TrueWhenStemMatchedJsonExists()
    {
        var outDir = Path.Combine(_root, "output");
        Directory.CreateDirectory(outDir);
        File.WriteAllText(Path.Combine(outDir, "PROG-deps.json"), "{}");

        RektScanCacheCommand.HasRektArtifacts(outDir, "PROG.cbl").Should().BeTrue();
    }

    [Fact]
    public void HasRektArtifacts_FalseWhenNoMatch()
    {
        var outDir = Path.Combine(_root, "output");
        Directory.CreateDirectory(outDir);
        File.WriteAllText(Path.Combine(outDir, "OTHER-deps.json"), "{}");

        RektScanCacheCommand.HasRektArtifacts(outDir, "PROG.cbl").Should().BeFalse();
    }

    [Fact]
    public void HasRektArtifacts_TrueWhenVerifyDirUnreadable()
    {
        // Defensive: when we can't enumerate the verify dir, assume artifacts
        // are present so we don't re-parse needlessly. Documented behaviour.
        var nonexistent = Path.Combine(_root, "does-not-exist");
        RektScanCacheCommand.HasRektArtifacts(nonexistent, "PROG.cbl").Should().BeTrue();
    }

    [Fact]
    public void HasRektArtifacts_FalseForEmptyBasename()
    {
        var outDir = Path.Combine(_root, "output");
        Directory.CreateDirectory(outDir);
        RektScanCacheCommand.HasRektArtifacts(outDir, "").Should().BeFalse();
    }

    [Fact]
    public void IdentityScheme_PinnedToV1Basename()
    {
        RektScanCacheCommand.IdentityScheme.Should().Be("v1-basename",
            "PR2.b commits to this identity scheme until the ProgramKey migration; bumping it should be deliberate");
        CacheKeyIdentity.V1Basename.Should().Be("v1-basename");
    }
}
