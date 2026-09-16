using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

// A full estate run costs hours, so converting a chosen program or a chosen service is how anyone
// iterates on a prompt or a model. The selection has to be exact: converting fewer programs than
// asked for, or the wrong one of two sharing a name, looks identical to a successful run.
public class ProgramSelectionTests
{
    private static CobolFile Program(string path) => new()
    {
        FileName = Path.GetFileName(path),
        FilePath = path,
        Content = "IDENTIFICATION DIVISION.",
        IsCopybook = false,
    };

    private static CobolFile Copybook(string path) => new()
    {
        FileName = Path.GetFileName(path),
        FilePath = path,
        Content = "01 REC.",
        IsCopybook = true,
    };

    private static List<CobolFile> Estate() => new()
    {
        Program("billing/ORDER.cbl"),
        Program("billing/PRICING.cbl"),
        Program("claims/ORDER.cbl"),
        Copybook("copy/CUSTREC.cpy"),
        Copybook("copy/ORDREC.cpy"),
    };

    [Fact]
    public void NoSelectionConvertsTheWholeEstate()
    {
        var kept = ProgramSelection.Apply(Estate(), null, out var unmatched);

        kept.Should().HaveCount(5);
        unmatched.Should().BeEmpty();
    }

    [Fact]
    public void SelectingOneProgramKeepsItAndEveryCopybook()
    {
        // The copybooks travel with it: a program without its record layouts is converted against
        // layouts the model invents.
        var kept = ProgramSelection.Apply(Estate(), new[] { "billing/ORDER.cbl" }, out var unmatched);

        kept.Where(f => !f.IsCopybook).Select(f => f.FilePath)
            .Should().BeEquivalentTo(new[] { "billing/ORDER.cbl" });
        kept.Count(f => f.IsCopybook).Should().Be(2);
        unmatched.Should().BeEmpty();
    }

    [Fact]
    public void SeveralProgramsCanBeSelectedAsOneService()
    {
        var kept = ProgramSelection.Apply(
            Estate(), new[] { "billing/ORDER.cbl", "billing/PRICING.cbl" }, out _);

        kept.Where(f => !f.IsCopybook).Select(f => f.FilePath)
            .Should().BeEquivalentTo(new[] { "billing/ORDER.cbl", "billing/PRICING.cbl" });
    }

    [Fact]
    public void ABasenameSelectsEveryProgramWithThatName()
    {
        // Two estates staged under one root legitimately share a basename. Choosing between them
        // is the caller's job, so a bare name takes both rather than picking one silently.
        var kept = ProgramSelection.Apply(Estate(), new[] { "ORDER.cbl" }, out _);

        kept.Where(f => !f.IsCopybook).Select(f => f.FilePath)
            .Should().BeEquivalentTo(new[] { "billing/ORDER.cbl", "claims/ORDER.cbl" });
    }

    [Fact]
    public void ASourceRelativePathSelectsExactlyOne()
    {
        var kept = ProgramSelection.Apply(Estate(), new[] { "claims/ORDER.cbl" }, out _);

        kept.Where(f => !f.IsCopybook).Select(f => f.FilePath)
            .Should().BeEquivalentTo(new[] { "claims/ORDER.cbl" });
    }

    [Fact]
    public void AStemWithoutAnExtensionWorks()
    {
        var kept = ProgramSelection.Apply(Estate(), new[] { "PRICING" }, out _);

        kept.Where(f => !f.IsCopybook).Select(f => f.FilePath)
            .Should().BeEquivalentTo(new[] { "billing/PRICING.cbl" });
    }

    [Fact]
    public void ANameMatchingNothingIsReportedRatherThanIgnored()
    {
        var kept = ProgramSelection.Apply(
            Estate(), new[] { "billing/ORDER.cbl", "NOSUCH.cbl" }, out var unmatched);

        unmatched.Should().BeEquivalentTo(new[] { "NOSUCH.cbl" });
        kept.Should().Contain(f => f.FilePath == "billing/ORDER.cbl");
    }

    [Fact]
    public void ThesameProgramNamedTwiceIsKeptOnce()
    {
        var kept = ProgramSelection.Apply(
            Estate(), new[] { "billing/ORDER.cbl", "ORDER" }, out _);

        kept.Count(f => f.FilePath == "billing/ORDER.cbl").Should().Be(1);
    }

    [Fact]
    public void ACopybookCannotBeSelectedAsAProgram()
    {
        // Nothing converts a copybook on its own, so naming one is a mistake worth reporting
        // rather than a request that quietly converts nothing.
        var kept = ProgramSelection.Apply(Estate(), new[] { "CUSTREC.cpy" }, out var unmatched);

        unmatched.Should().BeEquivalentTo(new[] { "CUSTREC.cpy" });
        kept.Should().NotContain(f => !f.IsCopybook);
    }
}
