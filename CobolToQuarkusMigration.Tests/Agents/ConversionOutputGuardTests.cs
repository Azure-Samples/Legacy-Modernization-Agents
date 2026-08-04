using CobolToQuarkusMigration.Agents;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Agents;

public class ConversionOutputGuardTests
{
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
            out var reason);

        result.Should().BeFalse();
        reason.Should().StartWith("NO_CSHARP_STRUCTURE");
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
}
