using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

/// <summary>
/// Regression coverage for the nested-source-directory bug: <c>source/FUENTES/src/X.cbl</c>
/// was previously invisible to <c>resolve-programs.py</c> and <c>FileHelper</c>. P0
/// fixed both; this test pins the C# half so the regression cannot return silently.
/// </summary>
public class NestedDiscoveryTests
{
    [Fact]
    public void EnumerateProgramFiles_FindsDeeplyNestedFiles()
    {
        var root = Path.Combine(Path.GetTempPath(), "nestdisc-" + Guid.NewGuid().ToString("N"));
        try
        {
            Directory.CreateDirectory(Path.Combine(root, "FUENTES", "src", "subA"));
            Directory.CreateDirectory(Path.Combine(root, "FUENTES", "src", "subB"));
            Directory.CreateDirectory(Path.Combine(root, "FUENTES", "cpy"));

            File.WriteAllText(Path.Combine(root, "FUENTES", "src", "subA", "A.cbl"), "");
            File.WriteAllText(Path.Combine(root, "FUENTES", "src", "subB", "B.cob"), "");
            File.WriteAllText(Path.Combine(root, "FUENTES", "cpy", "BOOK.cpy"), "");

            var programs = SourceTypeRegistry.EnumerateProgramFiles(root).ToList();
            programs.Should().HaveCount(2);
            programs.Should().Contain(p => p.EndsWith("A.cbl"));
            programs.Should().Contain(p => p.EndsWith("B.cob"));

            var copybooks = SourceTypeRegistry.EnumerateCopybookFiles(root).ToList();
            copybooks.Should().ContainSingle(p => p.EndsWith("BOOK.cpy"));
        }
        finally
        {
            if (Directory.Exists(root)) Directory.Delete(root, recursive: true);
        }
    }

    [Fact]
    public void EnumerateProgramFiles_SkipsInternalStagingDirs()
    {
        var root = Path.Combine(Path.GetTempPath(), "nestdisc-skip-" + Guid.NewGuid().ToString("N"));
        try
        {
            Directory.CreateDirectory(Path.Combine(root, ".rekt-staging"));
            Directory.CreateDirectory(Path.Combine(root, ".preprocessed"));
            File.WriteAllText(Path.Combine(root, "real.cbl"), "");
            File.WriteAllText(Path.Combine(root, ".rekt-staging", "stage.cbl"), "");
            File.WriteAllText(Path.Combine(root, ".preprocessed", "pre.cob"), "");

            var programs = SourceTypeRegistry.EnumerateProgramFiles(root).ToList();
            programs.Should().HaveCount(1);
            programs.Should().Contain(p => p.EndsWith("real.cbl"));
        }
        finally
        {
            if (Directory.Exists(root)) Directory.Delete(root, recursive: true);
        }
    }

    [Fact]
    public void CobAndCblHaveParity_InClassification()
    {
        // Both extensions must classify identically — explicit assertion so
        // an accidental divergence in SourceTypeRegistry fails loudly.
        SourceTypeRegistry.Classify("X.cbl").Should().Be(SourceKind.CobolProgram);
        SourceTypeRegistry.Classify("X.cob").Should().Be(SourceKind.CobolProgram);
        SourceTypeRegistry.Classify("X.CBL").Should().Be(SourceKind.CobolProgram);
        SourceTypeRegistry.Classify("X.COB").Should().Be(SourceKind.CobolProgram);
    }
}
