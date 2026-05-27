using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

public class SourceTypeRegistryTests
{
    [Theory]
    [InlineData("prog.cbl", true)]
    [InlineData("prog.CBL", true)]
    [InlineData("prog.cob", true)]
    [InlineData("prog.COB", true)]
    [InlineData("prog.cpy", false)]
    [InlineData("prog.txt", false)]
    [InlineData("prog", false)]
    public void IsCobolProgram_RecognisesAllProgramExtensions(string path, bool expected)
    {
        SourceTypeRegistry.IsCobolProgram(path).Should().Be(expected);
    }

    [Theory]
    [InlineData("book.cpy", true)]
    [InlineData("book.CPY", true)]
    [InlineData("book.cbl", false)]
    [InlineData("book.cob", false)]
    public void IsCopybook_RecognisesCopybookExtensions(string path, bool expected)
    {
        SourceTypeRegistry.IsCopybook(path).Should().Be(expected);
    }

    [Theory]
    [InlineData("a.cbl", SourceKind.CobolProgram)]
    [InlineData("a.cob", SourceKind.CobolProgram)]
    [InlineData("a.cpy", SourceKind.Copybook)]
    [InlineData("a.bms", SourceKind.Unknown)]
    [InlineData("a", SourceKind.Unknown)]
    public void Classify_MapsKindCorrectly(string path, SourceKind expected)
    {
        SourceTypeRegistry.Classify(path).Should().Be(expected);
    }

    [Fact]
    public void EnumerateProgramFiles_RecursesAndPicksUpCobAndCbl()
    {
        var root = Path.Combine(Path.GetTempPath(), "stnr-" + Guid.NewGuid().ToString("N"));
        try
        {
            Directory.CreateDirectory(Path.Combine(root, "FUENTES", "src"));
            Directory.CreateDirectory(Path.Combine(root, ".rekt-staging"));

            File.WriteAllText(Path.Combine(root, "flat.cbl"), "");
            File.WriteAllText(Path.Combine(root, "FUENTES", "src", "nested.cob"), "");
            File.WriteAllText(Path.Combine(root, "FUENTES", "src", "book.cpy"), "");
            File.WriteAllText(Path.Combine(root, ".rekt-staging", "should-be-skipped.cbl"), "");

            var programs = SourceTypeRegistry.EnumerateProgramFiles(root).ToList();
            programs.Should().HaveCount(2);
            programs.Should().Contain(p => p.EndsWith("flat.cbl"));
            programs.Should().Contain(p => p.EndsWith("nested.cob"));
            programs.Should().NotContain(p => p.Contains(".rekt-staging"));

            var copybooks = SourceTypeRegistry.EnumerateCopybookFiles(root).ToList();
            copybooks.Should().ContainSingle(p => p.EndsWith("book.cpy"));
        }
        finally
        {
            if (Directory.Exists(root)) Directory.Delete(root, recursive: true);
        }
    }
}
