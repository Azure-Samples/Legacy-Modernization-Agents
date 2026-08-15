using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

public sealed class SourceTypeRegistryTests : IDisposable
{
    private readonly string _root = Path.Combine(
        Path.GetTempPath(),
        $"source-type-registry-{Guid.NewGuid():N}");

    // ── KnownProgramExtensions ──────────────────────────────────────────────

    [Fact]
    public void KnownProgramExtensions_ContainsCblAndCob()
    {
        SourceTypeRegistry.KnownProgramExtensions.Should().Contain(".cbl");
        SourceTypeRegistry.KnownProgramExtensions.Should().Contain(".cob");
    }

    // ── KnownCopybookExtensions ─────────────────────────────────────────────

    [Fact]
    public void KnownCopybookExtensions_ContainsCpy()
    {
        SourceTypeRegistry.KnownCopybookExtensions.Should().Contain(".cpy");
    }

    // ── AllKnownExtensions ──────────────────────────────────────────────────

    [Fact]
    public void AllKnownExtensions_ContainsAllProgramAndCopybookExtensions()
    {
        var all = SourceTypeRegistry.AllKnownExtensions.ToList();

        all.Should().Contain(".cbl");
        all.Should().Contain(".cob");
        all.Should().Contain(".cpy");
    }

    // ── IsCopybook ──────────────────────────────────────────────────────────

    [Theory]
    [InlineData("CUSTOMER-DATA.cpy", true)]
    [InlineData("CUSTOMER-DATA.CPY", true)]
    [InlineData("CUSTOMER-DATA.cbl", false)]
    [InlineData("CUSTOMER-DATA.txt", false)]
    public void IsCopybook_RecognizesCpyExtensionCaseInsensitively(string path, bool expected)
    {
        SourceTypeRegistry.IsCopybook(path).Should().Be(expected);
    }

    // ── IsCobolProgram ──────────────────────────────────────────────────────

    [Theory]
    [InlineData("PROGRAM.cbl", true)]
    [InlineData("PROGRAM.CBL", true)]
    [InlineData("PROGRAM.cob", true)]
    [InlineData("PROGRAM.COB", true)]
    [InlineData("PROGRAM.cpy", false)]
    [InlineData("PROGRAM.java", false)]
    public void IsCobolProgram_RecognizesCblAndCobExtensionsCaseInsensitively(string path, bool expected)
    {
        SourceTypeRegistry.IsCobolProgram(path).Should().Be(expected);
    }

    // ── IsKnown ─────────────────────────────────────────────────────────────

    [Theory]
    [InlineData("PROGRAM.cbl", true)]
    [InlineData("PROGRAM.cob", true)]
    [InlineData("COPYBOOK.cpy", true)]
    [InlineData("file.java", false)]
    [InlineData("file.cs", false)]
    [InlineData("file.txt", false)]
    public void IsKnown_ReturnsTrueForCobolAndCopybookFiles(string path, bool expected)
    {
        SourceTypeRegistry.IsKnown(path).Should().Be(expected);
    }

    // ── Classify ────────────────────────────────────────────────────────────

    [Theory]
    [InlineData("PROGRAM.cbl", SourceKind.CobolProgram)]
    [InlineData("PROGRAM.cob", SourceKind.CobolProgram)]
    [InlineData("COPYBOOK.cpy", SourceKind.Copybook)]
    [InlineData("unknown.txt", SourceKind.Unknown)]
    public void Classify_ReturnsCorrectSourceKind(string path, SourceKind expected)
    {
        SourceTypeRegistry.Classify(path).Should().Be(expected);
    }

    // ── ProgramSearchPatterns ───────────────────────────────────────────────

    [Fact]
    public void ProgramSearchPatterns_ContainsGlobPatternsForCblAndCob()
    {
        var patterns = SourceTypeRegistry.ProgramSearchPatterns.ToList();

        patterns.Should().Contain("*.cbl");
        patterns.Should().Contain("*.cob");
    }

    // ── CopybookSearchPatterns ──────────────────────────────────────────────

    [Fact]
    public void CopybookSearchPatterns_ContainsGlobPatternForCpy()
    {
        var patterns = SourceTypeRegistry.CopybookSearchPatterns.ToList();

        patterns.Should().Contain("*.cpy");
    }

    // ── EnumerateCopybookFiles ──────────────────────────────────────────────

    [Fact]
    public void EnumerateCopybookFiles_FindsCpyFilesRecursively()
    {
        Directory.CreateDirectory(Path.Combine(_root, "copybooks"));
        File.WriteAllText(Path.Combine(_root, "copybooks", "CUSTOMER-DATA.cpy"), "");
        File.WriteAllText(Path.Combine(_root, "PROGRAM.cbl"), "");

        var result = SourceTypeRegistry.EnumerateCopybookFiles(_root).ToList();

        result.Should().HaveCount(1);
        result[0].Should().EndWith("CUSTOMER-DATA.cpy");
    }

    [Fact]
    public void EnumerateCopybookFiles_SkipsRektStagingDirectory()
    {
        var stagingDir = Path.Combine(_root, ".rekt-staging");
        Directory.CreateDirectory(stagingDir);
        File.WriteAllText(Path.Combine(stagingDir, "CUSTOMER-DATA.cpy"), "");

        var result = SourceTypeRegistry.EnumerateCopybookFiles(_root).ToList();

        result.Should().BeEmpty();
    }

    [Fact]
    public void EnumerateCopybookFiles_SkipsPreprocessedDirectory()
    {
        var preprocessedDir = Path.Combine(_root, ".preprocessed");
        Directory.CreateDirectory(preprocessedDir);
        File.WriteAllText(Path.Combine(preprocessedDir, "CUSTOMER-DATA.cpy"), "");

        var result = SourceTypeRegistry.EnumerateCopybookFiles(_root).ToList();

        result.Should().BeEmpty();
    }

    [Fact]
    public void EnumerateCopybookFiles_ReturnsEmpty_WhenRootDoesNotExist()
    {
        var result = SourceTypeRegistry.EnumerateCopybookFiles(
            Path.Combine(_root, "nonexistent")).ToList();

        result.Should().BeEmpty();
    }

    // ── EnumerateProgramFiles (already partially covered, extra edge case) ──

    [Fact]
    public void EnumerateProgramFiles_FindsCblAndCobFilesRecursively()
    {
        Directory.CreateDirectory(Path.Combine(_root, "batch"));
        File.WriteAllText(Path.Combine(_root, "batch", "BATCH.cbl"), "");
        File.WriteAllText(Path.Combine(_root, "ONLINE.cob"), "");
        File.WriteAllText(Path.Combine(_root, "NOTES.txt"), "");

        var result = SourceTypeRegistry.EnumerateProgramFiles(_root).ToList();

        result.Should().HaveCount(2);
        result.Should().Contain(f => f.EndsWith("BATCH.cbl"));
        result.Should().Contain(f => f.EndsWith("ONLINE.cob"));
    }

    public void Dispose()
    {
        if (Directory.Exists(_root))
            Directory.Delete(_root, recursive: true);
    }
}
