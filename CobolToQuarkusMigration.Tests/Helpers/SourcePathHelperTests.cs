using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

public class SourcePathHelperTests
{
    [Theory]
    [InlineData(@"src\prog\A.cbl", "src/prog/A.cbl")]
    [InlineData("./././A.cbl", "A.cbl")]
    [InlineData("/abs/A.cbl", "abs/A.cbl")]
    [InlineData("  .\\A.cbl  ", "A.cbl")]
    [InlineData("   ", "")]
    [InlineData("", "")]
    public void NormalizeRelativePath_ProducesForwardSlashRelativeForm(string input, string expected)
    {
        SourcePathHelper.NormalizeRelativePath(input).Should().Be(expected);
    }

    [Fact]
    public void ToOsRelativePath_UsesPlatformSeparator()
    {
        SourcePathHelper.ToOsRelativePath(@".\a\b.cbl")
            .Should().Be("a" + Path.DirectorySeparatorChar + "b.cbl");
    }

    [Fact]
    public void EnumerateProgramRelativePaths_MissingRoot_ReturnsEmpty()
    {
        var missing = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        SourcePathHelper.EnumerateProgramRelativePaths(missing).Should().BeEmpty();
    }

    [Fact]
    public void EnumerateProgramRelativePaths_ReturnsSortedForwardSlashPathsExcludingNonPrograms()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        try
        {
            Directory.CreateDirectory(Path.Combine(root, "sub"));
            File.WriteAllText(Path.Combine(root, "sub", "B.cbl"), "");
            File.WriteAllText(Path.Combine(root, "A.cbl"), "");
            File.WriteAllText(Path.Combine(root, "notes.txt"), "");

            SourcePathHelper.EnumerateProgramRelativePaths(root)
                .Should().Equal("A.cbl", "sub/B.cbl");
        }
        finally
        {
            Directory.Delete(root, true);
        }
    }
}
