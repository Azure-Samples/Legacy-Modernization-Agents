using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

// Structural context was opt-in, so every conversion ran without the facts the parser had already
// produced — while conversion parity scored the output against those same facts. Measured on a
// 36-program estate with reverse engineering held constant, turning it on moved mean parity from
// 0.859 to 0.957 and cleared all three sub-threshold programs.
public class RektContextEnabledByDefaultTests
{
    [Fact]
    public void AnUnsetValueLeavesItOn()
    {
        RektPromptInjector.IsEnabled(null).Should().BeTrue();
        RektPromptInjector.IsEnabled("").Should().BeTrue();
        RektPromptInjector.IsEnabled("   ").Should().BeTrue();
    }

    [Theory]
    [InlineData("false")]
    [InlineData("False")]
    [InlineData("FALSE")]
    [InlineData(" false ")]
    public void OnlyAnExplicitFalseTurnsItOff(string configured)
    {
        RektPromptInjector.IsEnabled(configured).Should().BeFalse();
    }

    [Theory]
    [InlineData("true")]
    [InlineData("1")]
    [InlineData("no")]
    [InlineData("disabled")]
    public void AnUnrecognisedValueLeavesItOn(string configured)
    {
        // A typo should not quietly return a run to the weaker behaviour: the failure mode is
        // invisible, since a conversion without structural context still succeeds and still
        // produces code, only worse.
        RektPromptInjector.IsEnabled(configured).Should().BeTrue();
    }
}
