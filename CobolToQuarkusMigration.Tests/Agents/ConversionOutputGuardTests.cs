using CobolToQuarkusMigration.Agents;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Agents;

public class ConversionOutputGuardTests
{
    [Fact]
    public void ExtractFencedCode_IncompleteInitialResponse_PreservesCodeForContinuation()
    {
        var output = "```csharp\nnamespace Converted;\npublic class Program\n{\n    void Run() {";

        var extracted = ConversionOutputGuard.ExtractFencedCode(output, "```csharp", "```c#");

        extracted.Should().StartWith("namespace Converted;");
        extracted.Should().EndWith("void Run() {");
        extracted.Should().NotContain("CONVERSION DID NOT PRODUCE");
    }

    [Fact]
    public void ExtractFencedCode_ContinuationFragment_DoesNotRequireTypeDeclaration()
    {
        var output = "```java\n        finishWork();\n    }\n}\n```";

        var extracted = ConversionOutputGuard.ExtractFencedCode(output, "```java");

        extracted.Should().Be("finishWork();\n    }\n}");
        extracted.Should().NotContain("CONVERSION DID NOT PRODUCE");
    }

    [Theory]
    [InlineData(null)]
    [InlineData("")]
    [InlineData("   ")]
    public void IsUsableChunk_EmptyCSharpOutput_ReturnsFalse(string? output)
    {
        var result = ConversionOutputGuard.IsUsableChunk(
            output,
            "namespace ",
            "class ",
            "C#",
            requireStructure: true,
            out var reason);

        result.Should().BeFalse();
        reason.Should().StartWith("EMPTY_LLM_RESPONSE");
    }

    [Fact]
    public void IsUsableChunk_CSharpProse_ReturnsFalse()
    {
        var result = ConversionOutputGuard.IsUsableChunk(
            "Here is the converted application.",
            "namespace ",
            "class ",
            "C#",
            requireStructure: true,
            out var reason);

        result.Should().BeFalse();
        reason.Should().StartWith("NO_CSHARP_STRUCTURE");
    }

    [Theory]
    [InlineData("namespace Converted { }")]
    [InlineData("public class Converted { }")]
    public void IsUsableChunk_CSharpStructureWithBraces_ReturnsTrue(string output)
    {
        var result = ConversionOutputGuard.IsUsableChunk(
            output,
            "namespace ",
            "class ",
            "C#",
            requireStructure: true,
            out var reason);

        result.Should().BeTrue();
        reason.Should().BeEmpty();
    }

    [Fact]
    public void IsUsableChunk_CSharpStructureWithoutBraces_ReturnsFalse()
    {
        var result = ConversionOutputGuard.IsUsableChunk(
            "public class Converted",
            "namespace ",
            "class ",
            "C#",
            requireStructure: true,
            out var reason);

        result.Should().BeFalse();
        reason.Should().StartWith("NO_CSHARP_STRUCTURE");
    }

    [Theory]
    [InlineData("private int counter;")]
    [InlineData("}")]
    public void IsUsableChunk_LaterChunkWithoutTypeDeclaration_ReturnsTrue(string output)
    {
        var result = ConversionOutputGuard.IsUsableChunk(
            output,
            "namespace ",
            "class ",
            "C#",
            requireStructure: false,
            out var reason);

        result.Should().BeTrue();
        reason.Should().BeEmpty();
    }

    [Fact]
    public void EscapeBlockCommentContent_EscapesCommentTerminators()
    {
        var escaped = ConversionOutputGuard.EscapeBlockCommentContent(
            "model output */ trailing code */");

        escaped.Should().Be("model output * / trailing code * /");
        escaped.Should().NotContain("*/");
    }

    [Theory]
    [InlineData(null)]
    [InlineData("")]
    [InlineData("   ")]
    public void EscapeBlockCommentContent_EmptyOutput_UsesPlaceholder(string? output)
    {
        ConversionOutputGuard.EscapeBlockCommentContent(output)
            .Should().Be("(no output)");
    }

    [Fact]
    public void ShouldCreateWholeFileStub_LongBraceImbalancedClass_ReturnsTrue()
    {
        var output = "public class Converted { public void Run() { }";

        ConversionOutputGuard.ShouldCreateWholeFileStub(
                output,
                hasClass: true,
                openingBraces: 2,
                closingBraces: 1)
            .Should().BeTrue();
    }

    [Fact]
    public void ShouldCreateWholeFileStub_LongBalancedClass_ReturnsFalse()
    {
        var output = "public class Converted { public void Run() { } }";

        ConversionOutputGuard.ShouldCreateWholeFileStub(
                output,
                hasClass: true,
                openingBraces: 2,
                closingBraces: 2)
            .Should().BeFalse();
    }

    [Fact]
    public void BuildChunkDiagnosticStub_IncludesChunkContextAndRemediation()
    {
        var stub = ConversionOutputGuard.BuildChunkDiagnosticStub(
            "C#",
            "nested/ACCOUNTS.cbl",
            chunkIndex: 1,
            totalChunks: 3,
            startLine: 101,
            endLine: 200,
            reason: "EMPTY_LLM_RESPONSE");

        stub.Should().Contain("CHUNK CONVERSION DID NOT PRODUCE USABLE C#");
        stub.Should().Contain("Chunk: 2/3 (lines 101-200)");
        stub.Should().Contain("output/rekt/ACCOUNTS.cbl.report/");
        stub.Should().Contain("EMPTY_LLM_RESPONSE");
    }

    [Fact]
    public void BuildWholeFileDiagnosticStub_PreservesEscapedOriginalOutput()
    {
        var stub = ConversionOutputGuard.BuildWholeFileDiagnosticStub(
            "C#",
            "BRACE_IMBALANCE",
            "public class Broken { /* comment */ trailing */");

        stub.Should().Contain("CONVERSION DID NOT PRODUCE USABLE C#");
        stub.Should().Contain("BRACE_IMBALANCE");
        stub.Should().Contain("public class Broken");
        stub.Should().NotContain("trailing */");
        stub.Should().Contain("trailing * /");
    }
}
