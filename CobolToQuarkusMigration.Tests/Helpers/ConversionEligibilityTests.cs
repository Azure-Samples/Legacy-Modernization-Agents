using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

[Collection("RektFactsFolderEnvironment")]
public sealed class ConversionEligibilityTests : IDisposable
{
    private const string FactsVariable = "REKT_FACTS_FOLDER";
    private readonly string? _originalFacts = Environment.GetEnvironmentVariable(FactsVariable);
    private readonly string _root = Path.Combine(Path.GetTempPath(), "eligibility-" + Guid.NewGuid().ToString("N"));

    public ConversionEligibilityTests() => Directory.CreateDirectory(_root);

    public void Dispose()
    {
        Environment.SetEnvironmentVariable(FactsVariable, _originalFacts);
        Directory.Delete(_root, recursive: true);
    }

    [Fact]
    public void FactsDirectory_DefaultsToOutputRektUnderRepositoryRoot()
    {
        Environment.SetEnvironmentVariable(FactsVariable, null);

        ConversionEligibility.FactsDirectory(_root).Should().Be(Path.Combine(_root, "output", "rekt"));
    }

    [Fact]
    public void FactsDirectory_ResolvesRelativeOverrideAgainstRepositoryRootAndKeepsAbsoluteOverride()
    {
        Environment.SetEnvironmentVariable(FactsVariable, Path.Combine("custom", "..", "facts"));
        ConversionEligibility.FactsDirectory(_root).Should().Be(Path.Combine(_root, "facts"));

        var absolute = Path.Combine(Path.GetTempPath(), "elsewhere-facts");
        Environment.SetEnvironmentVariable(FactsVariable, absolute);
        ConversionEligibility.FactsDirectory(_root).Should().Be(absolute);
    }

    [Fact]
    public void FactsDirectory_TreatsBlankOverrideAsUnset()
    {
        Environment.SetEnvironmentVariable(FactsVariable, "   ");

        ConversionEligibility.FactsDirectory(_root).Should().Be(Path.Combine(_root, "output", "rekt"));
    }

    [Theory]
    [InlineData("strict")]
    [InlineData("draft")]
    [InlineData("partial")]
    public void ValidateMode_AcceptsSupportedModes(string mode) =>
        ConversionEligibility.ValidateMode(mode).Should().Be(mode);

    [Theory]
    [InlineData("")]
    [InlineData("Strict")]
    [InlineData("lenient")]
    public void ValidateMode_RejectsUnknownModesInsteadOfFallingBackToAnother(string mode)
    {
        var act = () => ConversionEligibility.ValidateMode(mode);

        act.Should().Throw<ArgumentException>().WithMessage("*strict*draft*partial*");
    }

    [Fact]
    public void Select_PartialMode_IsRejectedBecauseItNeedsThePartialTreeProcess()
    {
        var settings = new ConversionSettings { DependencyMode = "partial" };

        var act = () => ConversionEligibility.Select([], _root, _root, Path.Combine(_root, "out"), settings, null);

        act.Should().Throw<InvalidDataException>().WithMessage("*partial-tree*");
    }

    [Fact]
    public void Select_DraftMode_RefusesNonEmptyOutputFolderAndLeavesItUntouched()
    {
        var output = Path.Combine(_root, "out");
        Directory.CreateDirectory(output);
        var previous = Path.Combine(output, "Previous.java");
        File.WriteAllText(previous, "class Previous {}");
        var settings = new ConversionSettings { DependencyMode = "draft" };

        var act = () => ConversionEligibility.Select([], _root, Path.Combine(_root, "facts"), output, settings, null);

        act.Should().Throw<InvalidDataException>().WithMessage("*fresh empty output*");
        Directory.EnumerateFileSystemEntries(output).Should().ContainSingle().Which.Should().Be(previous);
        File.ReadAllText(previous).Should().Be("class Previous {}");
    }
}
