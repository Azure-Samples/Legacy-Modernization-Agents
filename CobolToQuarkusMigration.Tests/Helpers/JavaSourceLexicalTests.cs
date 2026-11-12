using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using FluentAssertions;
using Microsoft.Extensions.Logging.Abstractions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

public sealed class JavaSourceLexicalTests
{
    [Fact]
    public async Task CommentsStringsAndTextBlocksCannotChangeTheSavedPackageOrContent()
    {
        const string code = """"
            /*
            package fake.comment;
            public class Wrong {}
            */
            package real.output;
            public class Sample {
                String text = """
            package fake.textblock;
            public class AlsoWrong {}}}
            """;
                String literal = "package fake.literal; {";
            }
            """";
        JavaSourceLexical.ExtractResponse(code).Should().Be(code);
        JavaSourceLexical.ClassName(code).Should().Be("Sample");
        JavaSourceLexical.PackageName(code).Should().Be("real.output");
        var root = Path.Combine(AppContext.BaseDirectory, "test-artifacts", "lexical-" + Guid.NewGuid().ToString("N"));
        try
        {
            var file = new JavaFile { FileName = "Sample.java", Content = code, OriginalSourcePath = "SAMPLE.cbl" };
            var saved = await new FileHelper(NullLogger<FileHelper>.Instance).SaveJavaFileAsync(file, root);
            saved.Should().Be(Path.Combine(root, "real", "output", "Sample.java"));
            file.FilePath.Should().Be(saved);
            (await File.ReadAllTextAsync(saved)).Should().Be(code.ReplaceLineEndings());
        }
        finally { if (Directory.Exists(root)) Directory.Delete(root, true); }
    }

    [Fact]
    public void TruncatedRestartIsRecoveredOnlyWhenItIsTheSameUnit()
    {
        const string complete = "package p; public class Sample { String brace = \"}\"; }";
        JavaSourceLexical.ExtractResponse("package p; public class Sample { void partial() {\n" + complete)
            .Should().Be(complete);
        Action other = () => JavaSourceLexical.ExtractResponse("package p; public class Different {\n" + complete);
        other.Should().Throw<InvalidOperationException>().WithMessage("*different compilation unit*");
    }

    [Theory]
    [InlineData("package p; public class A {} package q; public class B {}")]
    [InlineData("package p; public class A {} package p; public class A {}")]
    [InlineData("```java\npackage p; public class A {}\n```\n```java\npackage q; public class B {}\n```")]
    [InlineData("```java\npublic class A {}\n```\n```java\npublic class B {}\n```")]
    [InlineData("public class A {} public class B {}")]
    [InlineData("package p; public class A {} package p; public class A {")]
    public void GenuineOrAmbiguousMultipleUnitsFailExplicitlyRatherThanDroppingCode(string response)
    {
        Action extract = () => JavaSourceLexical.ExtractResponse(response);
        extract.Should().Throw<InvalidOperationException>();
    }

    [Fact]
    public void FencedJavaAndTruncatedFencesKeepEveryCodeLine()
    {
        JavaSourceLexical.ExtractResponse("Explanation\n```java\npackage p;\npublic class A {}\n```\nEnd")
            .Should().Be("package p;\npublic class A {}");
        JavaSourceLexical.ExtractResponse("```java\npackage p;\npublic class A {")
            .Should().Be("package p;\npublic class A {");
    }

    [Fact]
    public void FenceLikeLinesInsideTextBlocksAreSourceNotResponseFraming()
    {
        const string unit = "package p;\npublic class Docs {\n    String json = \"\"\"\n```json\n{\"a\": 1}\n```\n\"\"\";\n    String java = \"\"\"\n    ```java\n    int x;\n    ```\n    \"\"\";\n}";
        JavaSourceLexical.ExtractResponse(unit).Should().Be(unit);
        JavaSourceLexical.ExtractResponse("Here's the class:\n```java\n" + unit + "\n```\nDone")
            .Should().Be(unit);
    }

    [Fact]
    public void IdentifierRenamePreservesCommentsAndLiteralContents()
    {
        const string source = "class Old { Old self; String s = \"Old\"; /* Old */ String b = \"\"\"\nOld\n\"\"\"; }";
        JavaSourceLexical.RenameIdentifier(source, "Old", "New").Should().Be(
            "class New { New self; String s = \"Old\"; /* Old */ String b = \"\"\"\nOld\n\"\"\"; }");
    }

    [Fact]
    public void PackagePrivateHelpersAndNestedPublicTypesArePreservedWithoutChangingTheOutputClassName()
    {
        const string source = "package p; class Helper { public class Nested {} } public final class Sample {}";
        JavaSourceLexical.ClassName(source).Should().Be("Sample");
        JavaSourceLexical.ExtractResponse(source).Should().Be(source);
    }
}
