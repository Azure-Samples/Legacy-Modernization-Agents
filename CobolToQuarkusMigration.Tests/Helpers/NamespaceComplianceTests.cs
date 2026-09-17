using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

// The namespace is assigned in a block this repository injects, but the line telling the model to
// use it sits in the User section of the converter prompt — the one section Prompt Studio rewrites.
// If that line is lost nothing fails: the block still arrives, nothing insists on it, and the model
// goes back to inventing a root. Prompt wording cannot be relied on to survive editing; the output
// can be checked instead.
public sealed class NamespaceComplianceTests : IDisposable
{
    private readonly string _root = Path.Join(
        Path.GetTempPath(), "ns-compliance-" + Guid.NewGuid().ToString("N"));

    public NamespaceComplianceTests() => Directory.CreateDirectory(_root);

    public void Dispose()
    {
        try { Directory.Delete(_root, recursive: true); }
        catch (IOException ex) { Console.Error.WriteLine($"Could not remove {_root}: {ex.Message}"); }
    }

    private void Write(string name, string body) => File.WriteAllText(Path.Join(_root, name), body);

    [Fact]
    public void CodeInTheAssignedRootPasses()
    {
        Write("A.cs", "namespace Modernized.Banking.Bd;\npublic class Alpha { }\n");
        Write("B.cs", "namespace Modernized.Banking.Shared;\npublic class Beta { }\n");

        NamespaceCompliance.Check(_root, "Modernized.Banking").Should().BeEmpty();
    }

    // The exact failure this exists to catch: the instruction was dropped and the model invented.
    [Fact]
    public void CodeUnderAnInventedRootIsReported()
    {
        Write("A.cs", "namespace Modernized.Banking.Bd;\npublic class Alpha { }\n");
        Write("B.cs", "namespace CobolMigration.Something;\npublic class Beta { }\n");

        var deviations = NamespaceCompliance.Check(_root, "Modernized.Banking");

        var deviation = deviations.Should().ContainSingle().Subject;
        deviation.File.Should().EndWith("B.cs");
        deviation.Declared.Should().Be("CobolMigration.Something");
    }

    [Fact]
    public void AFileWithNoNamespaceAtAllIsReported()
    {
        Write("A.cs", "public class Orphan { }\n");

        NamespaceCompliance.Check(_root, "Modernized")
            .Should().ContainSingle().Which.Declared.Should().BeNull();
    }

    [Fact]
    public void JavaPackagesAreCheckedToo()
    {
        Write("A.java", "package com.modernized.bd;\npublic class Alpha { }\n");
        Write("B.java", "package com.example.cobol.kyghr002;\npublic class Beta { }\n");

        NamespaceCompliance.Check(_root, "com.modernized")
            .Should().ContainSingle().Which.Declared.Should().Be("com.example.cobol.kyghr002");
    }

    // A prefix test that ignores segment boundaries would let a neighbouring root pass.
    [Theory]
    [InlineData("Modernized", "Modernized", true)]
    [InlineData("Modernized.Bd", "Modernized", true)]
    [InlineData("Modernizedx.Bd", "Modernized", false)]
    [InlineData("ModernizedBanking", "Modernized", false)]
    [InlineData("Other.Modernized", "Modernized", false)]
    [InlineData(null, "Modernized", false)]
    [InlineData("", "Modernized", false)]
    public void ContainmentIsTestedOnASegmentBoundary(string? declared, string root, bool expected)
    {
        NamespaceCompliance.IsUnder(declared, root).Should().Be(expected);
    }

    [Fact]
    public void CasingDoesNotMatter()
    {
        Write("A.cs", "namespace modernized.banking.bd;\npublic class Alpha { }\n");

        NamespaceCompliance.Check(_root, "Modernized.Banking").Should().BeEmpty();
    }

    [Fact]
    public void FilesAreCheckedRecursively()
    {
        Directory.CreateDirectory(Path.Join(_root, "sub"));
        File.WriteAllText(Path.Join(_root, "sub", "B.cs"), "namespace Wrong.Place;\npublic class B { }\n");

        NamespaceCompliance.Check(_root, "Modernized").Should().ContainSingle();
    }

    [Fact]
    public void NonSourceFilesAreIgnored()
    {
        Write("notes.md", "namespace Wrong.Place\n");
        Write("data.json", "{}");

        NamespaceCompliance.Check(_root, "Modernized").Should().BeEmpty();
    }

    [Fact]
    public void AMissingDirectoryIsNotAnError()
    {
        NamespaceCompliance.Check(Path.Join(_root, "absent"), "Modernized").Should().BeEmpty();
    }

    [Fact]
    public void TheReportNamesTheFileAndWhatItDeclaredInstead()
    {
        Write("B.cs", "namespace CobolMigration.Something;\npublic class Beta { }\n");

        var text = NamespaceCompliance.Describe(
            NamespaceCompliance.Check(_root, "Modernized.Banking"), "Modernized.Banking", _root);

        text.Should().Contain("B.cs");
        text.Should().Contain("CobolMigration.Something");
        text.Should().Contain("Modernized.Banking");
    }

    [Fact]
    public void NothingToReportProducesNoReport()
    {
        NamespaceCompliance.Describe(Array.Empty<NamespaceDeviation>(), "Modernized", _root)
            .Should().BeEmpty();
    }
}
