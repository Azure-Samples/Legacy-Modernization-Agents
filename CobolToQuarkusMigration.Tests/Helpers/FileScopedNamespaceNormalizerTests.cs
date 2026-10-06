using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

// A copybook's file owns its shared type, and a copybook containing a CALL also carries service
// code, so one generated file ends up declaring `namespace …Shared;` and then `namespace …Bd;`.
// C# allows one file-scoped namespace per file; the compiler nests the second inside the first and
// every shared type the second names stops resolving. On a measured run that was 13 of 71 files.
public sealed class FileScopedNamespaceNormalizerTests : IDisposable
{
    private const string TwoNamespaces =
        "// header\n" +
        "namespace Modernized.Banking.Shared;\n" +
        "\n" +
        "using System;\n" +
        "\n" +
        "public sealed class Batommik { }\n" +
        "\n" +
        "namespace Modernized.Banking.Bd;\n" +
        "\n" +
        "using Modernized.Banking.Shared;\n" +
        "\n" +
        "public sealed class BatommikService { public Batommik Area { get; } = new(); }\n";

    private readonly string _run = Path.Join(
        Path.GetTempPath(), "nsnorm-" + Guid.NewGuid().ToString("N"));

    public FileScopedNamespaceNormalizerTests() => Directory.CreateDirectory(_run);

    public void Dispose()
    {
        try { Directory.Delete(_run, recursive: true); }
        catch (IOException ex) { Console.Error.WriteLine($"Could not remove {_run}: {ex.Message}"); }
    }

    [Fact]
    public void AFileWithOneNamespaceIsLeftAlone()
    {
        FileScopedNamespaceNormalizer
            .Normalize("namespace A;\npublic class X { }\n")
            .Should().BeNull();
    }

    [Fact]
    public void AFileWithNoNamespaceIsLeftAlone()
    {
        FileScopedNamespaceNormalizer.Normalize("public class X { }\n").Should().BeNull();
    }

    [Fact]
    public void EachFileScopedNamespaceBecomesABlockAroundWhatFollowedIt()
    {
        var result = FileScopedNamespaceNormalizer.Normalize(TwoNamespaces);

        result.Should().Be(
            "// header\n" +
            "namespace Modernized.Banking.Shared\n" +
            "{\n" +
            "\n" +
            "using System;\n" +
            "\n" +
            "public sealed class Batommik { }\n" +
            "}\n" +
            "\n" +
            "namespace Modernized.Banking.Bd\n" +
            "{\n" +
            "\n" +
            "using Modernized.Banking.Shared;\n" +
            "\n" +
            "public sealed class BatommikService { public Batommik Area { get; } = new(); }\n" +
            "}\n");
    }

    [Fact]
    public void AFileScopedNamespaceFollowedByABlockOneIsClosedBeforeIt()
    {
        // Ordda23k.cs's shape: the compiler nests the block, and the phantom
        // Modernized.Banking.Bd.Modernized.Banking.Shared then captures every
        // `using Modernized.Banking.Shared;` in Modernized.Banking.Bd.
        const string mixed =
            "namespace Modernized.Banking.Bd;\n" +
            "public class Ordda23k { }\n" +
            "namespace Modernized.Banking.Shared\n" +
            "{\n" +
            "    public class Ordda23kRecord { }\n" +
            "}\n";

        var result = FileScopedNamespaceNormalizer.Normalize(mixed);

        result.Should().Be(
            "namespace Modernized.Banking.Bd\n" +
            "{\n" +
            "public class Ordda23k { }\n" +
            "}\n" +
            "\n" +
            "namespace Modernized.Banking.Shared\n" +
            "{\n" +
            "    public class Ordda23kRecord { }\n" +
            "}\n");
    }

    [Fact]
    public void AFileOfBlockNamespacesOnlyIsLeftAlone()
    {
        FileScopedNamespaceNormalizer
            .Normalize("namespace A\n{\nclass X { }\n}\nnamespace B {\nclass Y { }\n}\n")
            .Should().BeNull();
    }

    [Fact]
    public void RewrittenOutputIsNotRewrittenAgain()
    {
        var once = FileScopedNamespaceNormalizer.Normalize(TwoNamespaces)!;

        FileScopedNamespaceNormalizer.Normalize(once).Should().BeNull();
    }

    [Fact]
    public void ATrailingCommentOnTheDeclarationIsKept()
    {
        var result = FileScopedNamespaceNormalizer.Normalize(
            "namespace A; // shared\nclass X { }\nnamespace B;\nclass Y { }\n");

        result.Should().StartWith("namespace A // shared\n{").And.Contain("namespace B\n{");
    }

    [Fact]
    public void ANamespaceMentionedInACommentIsNotADeclaration()
    {
        FileScopedNamespaceNormalizer
            .Normalize("namespace A;\n// namespace B;\n/// see namespace C;\nclass X { }\n")
            .Should().BeNull();
    }

    [Fact]
    public void WindowsLineEndingsArePreserved()
    {
        var result = FileScopedNamespaceNormalizer.Normalize(TwoNamespaces.Replace("\n", "\r\n"))!;

        result.Replace("\r\n", "").Should().NotContain("\n");
        result.Should().Contain("namespace Modernized.Banking.Bd\r\n{");
    }

    [Fact]
    public void TheFolderPassRewritesOnlyTheFilesThatNeedIt()
    {
        var mixed = Path.Join(_run, "Shared", "Batommik.cs");
        var single = Path.Join(_run, "Bd", "Ordda01.cs");
        Directory.CreateDirectory(Path.GetDirectoryName(mixed)!);
        Directory.CreateDirectory(Path.GetDirectoryName(single)!);
        File.WriteAllText(mixed, TwoNamespaces);
        const string singleText = "namespace Modernized.Banking.Bd;\npublic class Ordda01 { }\n";
        File.WriteAllText(single, singleText);

        var rewritten = FileScopedNamespaceNormalizer.NormalizeFolder(_run);

        rewritten.Should().Equal(mixed);
        File.ReadAllText(mixed).Should().Contain("namespace Modernized.Banking.Bd\n{");
        File.ReadAllText(single).Should().Be(singleText);
    }

    [Fact]
    public void TheScaffoldReportsWhichFilesItRewrote()
    {
        var mixed = Path.Join(_run, "Batommik.cs");
        File.WriteAllText(mixed, TwoNamespaces);
        File.WriteAllText(Path.Join(_run, "Plain.cs"), "namespace A;\npublic class Plain { }\n");

        var result = GeneratedProjectScaffold.Write(_run, "Estate");

        result.Normalized.Should().Equal(mixed);
        File.ReadAllText(mixed).Should().NotContain("namespace Modernized.Banking.Bd;");
    }

    [Fact]
    public void AMissingFolderRewritesNothing()
    {
        FileScopedNamespaceNormalizer
            .NormalizeFolder(Path.Join(_run, "absent"))
            .Should().BeEmpty();
    }
}
