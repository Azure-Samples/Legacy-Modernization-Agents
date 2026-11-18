using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Microsoft.Extensions.Logging.Abstractions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

public sealed class SharedTypeShadowingRepairTests
{
    private const string Shadowing = "namespace Modernized.BankOfZ;\npublic class Inqtranl\n{\n    string F = \"{0}\";\n}";
    private const string Renamed = "namespace Modernized.BankOfZ;\npublic class ProctranEntity\n{\n    string F = \"{0}\";\n}";

    private static Task<string> Repair(string code, string? correction, List<IReadOnlyList<string>>? asked = null) =>
        SharedTypeShadowingRepair.RepairAsync(code, ["Inqtranl"], names =>
        {
            asked?.Add(names);
            return Task.FromResult(correction);
        }, "INQTRANL.cbl", NullLogger.Instance);

    [Fact]
    public async Task AShadowingConversionIsReplacedByItsRenamedCorrection()
    {
        var asked = new List<IReadOnlyList<string>>();

        (await Repair(Shadowing, Renamed, asked)).Should().Be(Renamed);
        asked.Should().ContainSingle().Which.Should().Equal("Inqtranl");
    }

    [Fact]
    public async Task AConversionThatDoesNotShadowIsNeverSentBack()
    {
        var asked = new List<IReadOnlyList<string>>();

        (await Repair(Renamed, Shadowing, asked)).Should().Be(Renamed);
        asked.Should().BeEmpty();
    }

    [Theory]
    [InlineData(null)]
    [InlineData("namespace Modernized.BankOfZ;\npublic class Inqtranl\n{\n}")]
    [InlineData("namespace Modernized.BankOfZ;\n// nothing")]
    [InlineData("namespace Modernized.BankOfZ;\npublic class ProctranEntity\n{\n")]
    public async Task AnUnusableCorrectionKeepsTheOriginal(string? correction) =>
        (await Repair(Shadowing, correction)).Should().Be(Shadowing);
}
