using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

public class SourcePathHelperTests
{
    [Theory]
    [InlineData(@"a\b\C.cbl", "a/b/C.cbl")]
    [InlineData("./././a/b.cbl", "a/b.cbl")]
    [InlineData("/a/b.cbl", "a/b.cbl")]
    [InlineData("  ./a.cbl ", "a.cbl")]
    [InlineData("   ", "")]
    public void NormalizeRelativePath_ProducesForwardSlashRootlessPath(string input, string expected)
    {
        SourcePathHelper.NormalizeRelativePath(input).Should().Be(expected);
    }

    [Fact]
    public void ToOsRelativePath_UsesPlatformSeparator()
    {
        SourcePathHelper.ToOsRelativePath(@".\a\b.cbl")
            .Should().Be(Path.Combine("a", "b.cbl"));
    }

    [Fact]
    public void EnumerateProgramRelativePaths_ReturnsSortedProgramsAndSkipsScratchFolders()
    {
        var root = Path.Combine(Path.GetTempPath(), "sph-" + Guid.NewGuid().ToString("N"));
        try
        {
            Directory.CreateDirectory(Path.Combine(root, "sub"));
            Directory.CreateDirectory(Path.Combine(root, ".preprocessed"));
            File.WriteAllText(Path.Combine(root, "B.cbl"), "x");
            File.WriteAllText(Path.Combine(root, "sub", "A.cbl"), "x");
            File.WriteAllText(Path.Combine(root, ".preprocessed", "Z.cbl"), "x");
            File.WriteAllText(Path.Combine(root, "notes.txt"), "x");

            SourcePathHelper.EnumerateProgramRelativePaths(root)
                .Should().Equal("B.cbl", "sub/A.cbl");
        }
        finally
        {
            Directory.Delete(root, true);
        }
    }

    [Fact]
    public void EnumerateProgramRelativePaths_ReturnsEmptyForMissingRoot()
    {
        SourcePathHelper.EnumerateProgramRelativePaths(Path.Combine(Path.GetTempPath(), "missing-" + Guid.NewGuid().ToString("N")))
            .Should().BeEmpty();
    }
}
