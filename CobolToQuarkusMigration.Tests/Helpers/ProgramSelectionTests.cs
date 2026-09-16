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
    private static CobolFile Program(string path, params string[] copies) => new()
    {
        FileName = Path.GetFileName(path),
        FilePath = path,
        Content = "IDENTIFICATION DIVISION.\n"
                  + string.Join("\n", copies.Select(c => $"       COPY {c}.")),
        IsCopybook = false,
    };

    private static CobolFile Copybook(string path, params string[] copies) => new()
    {
        FileName = Path.GetFileName(path),
        FilePath = path,
        Content = "01 REC.\n" + string.Join("\n", copies.Select(c => $"       COPY {c}.")),
        IsCopybook = true,
    };

    private static List<CobolFile> Estate() => new()
    {
        Program("billing/ORDER.cbl", "CUSTREC", "ORDREC"),
        Program("billing/PRICING.cbl", "ORDREC"),
        Program("claims/ORDER.cbl", "CUSTREC"),
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
    public void SelectingOneProgramKeepsTheCopybooksItReaches()
    {
        // The copybooks travel with it: a program without its record layouts is converted against
        // layouts the model invents.
        var kept = ProgramSelection.Apply(Estate(), new[] { "billing/ORDER.cbl" }, out var unmatched);

        kept.Where(f => !f.IsCopybook).Select(f => f.FilePath)
            .Should().BeEquivalentTo(new[] { "billing/ORDER.cbl" });
        kept.Where(f => f.IsCopybook).Select(f => f.FilePath)
            .Should().BeEquivalentTo(new[] { "copy/CUSTREC.cpy", "copy/ORDREC.cpy" });
        unmatched.Should().BeEmpty();
    }

    // Retaining every copybook in the estate made converting one program analyse the copybooks of
    // every unrelated application beside it, which on a multi-estate source drop is most of the work.
    [Fact]
    public void ACopybookNoChosenProgramReachesIsLeftBehind()
    {
        var estate = Estate();
        estate.Add(Copybook("copy/UNRELATED.cpy"));

        var kept = ProgramSelection.Apply(estate, new[] { "billing/PRICING.cbl" }, out _);

        kept.Where(f => f.IsCopybook).Select(f => f.FilePath)
            .Should().BeEquivalentTo(new[] { "copy/ORDREC.cpy" });
    }

    // The case the blanket rule existed for: the program names one copybook, which names another.
    [Fact]
    public void ACopybookReachedOnlyThroughAnotherCopybookStillTravels()
    {
        var estate = new List<CobolFile>
        {
            Program("billing/ORDER.cbl", "OUTER"),
            Copybook("copy/OUTER.cpy", "INNER"),
            Copybook("copy/INNER.cpy", "DEEPEST"),
            Copybook("copy/DEEPEST.cpy"),
            Copybook("copy/UNRELATED.cpy"),
        };

        var kept = ProgramSelection.Apply(estate, new[] { "ORDER" }, out _);

        kept.Where(f => f.IsCopybook).Select(f => f.FilePath)
            .Should().BeEquivalentTo(new[] { "copy/OUTER.cpy", "copy/INNER.cpy", "copy/DEEPEST.cpy" });
    }

    [Fact]
    public void ACopybookCycleDoesNotHangTheClosure()
    {
        var estate = new List<CobolFile>
        {
            Program("billing/ORDER.cbl", "A"),
            Copybook("copy/A.cpy", "B"),
            Copybook("copy/B.cpy", "A"),
        };

        var kept = ProgramSelection.Apply(estate, new[] { "ORDER" }, out _);

        kept.Count(f => f.IsCopybook).Should().Be(2);
    }

    // An absent COPY target is a missing copybook, reported by the scan. It must not stop the rest
    // of the closure being collected.
    [Fact]
    public void AnUnresolvedCopyTargetDoesNotLoseTheOthers()
    {
        var estate = new List<CobolFile>
        {
            Program("billing/ORDER.cbl", "PRESENT", "ABSENT"),
            Copybook("copy/PRESENT.cpy"),
        };

        var kept = ProgramSelection.Apply(estate, new[] { "ORDER" }, out _);

        kept.Where(f => f.IsCopybook).Select(f => f.FilePath)
            .Should().BeEquivalentTo(new[] { "copy/PRESENT.cpy" });
    }

    // Dropping one would pick a record layout on the caller's behalf.
    [Fact]
    public void TwoCopybooksSharingANameAreBothKept()
    {
        var estate = new List<CobolFile>
        {
            Program("billing/ORDER.cbl", "SHARED"),
            Copybook("a/SHARED.cpy"),
            Copybook("b/SHARED.cpy"),
        };

        var kept = ProgramSelection.Apply(estate, new[] { "ORDER" }, out _);

        kept.Where(f => f.IsCopybook).Select(f => f.FilePath)
            .Should().BeEquivalentTo(new[] { "a/SHARED.cpy", "b/SHARED.cpy" });
    }

    [Fact]
    public void ACommentedOutCopyIsNotFollowed()
    {
        var estate = new List<CobolFile>
        {
            new()
            {
                FileName = "ORDER.cbl",
                FilePath = "billing/ORDER.cbl",
                Content = "IDENTIFICATION DIVISION.\n      * COPY GHOST.\n       COPY REAL.",
                IsCopybook = false,
            },
            Copybook("copy/REAL.cpy"),
            Copybook("copy/GHOST.cpy"),
        };

        var kept = ProgramSelection.Apply(estate, new[] { "ORDER" }, out _);

        kept.Where(f => f.IsCopybook).Select(f => f.FilePath)
            .Should().BeEquivalentTo(new[] { "copy/REAL.cpy" });
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
