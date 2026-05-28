using CobolToQuarkusMigration.Agents.Infrastructure.RektCache;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.RektCache;

public class RektCopybookGraphTests
{
    [Fact]
    public void Hash_ChangesWhenContentChanges()
    {
        var g = new RektCopybookGraph();
        g.AddFile("A.cbl", "PROGRAM-ID. A.", isCopybook: false);
        var h1 = g.GetHash("A.cbl");

        var g2 = new RektCopybookGraph();
        g2.AddFile("A.cbl", "PROGRAM-ID. A. *changed*", isCopybook: false);
        var h2 = g2.GetHash("A.cbl");

        h1.Should().NotBe(h2);
    }

    [Fact]
    public void DirectDependencies_ExtractsCopyTargets()
    {
        var g = new RektCopybookGraph();
        g.AddFile("BOOK.cpy", "01 WS-A PIC X(10).", isCopybook: true);
        g.AddFile("OTHER.cpy", "01 WS-B PIC X(10).", isCopybook: true);
        g.AddFile("PROG.cbl",
            "       IDENTIFICATION DIVISION.\n" +
            "       PROGRAM-ID. PROG.\n" +
            "       DATA DIVISION.\n" +
            "       WORKING-STORAGE SECTION.\n" +
            "           COPY BOOK.\n" +
            "           COPY 'OTHER'.\n",
            isCopybook: false);

        var snap = g.BuildDependencySnapshot("PROG.cbl");
        snap.Keys.Should().Contain(new[] { "BOOK.cpy", "OTHER.cpy" });
    }

    [Fact]
    public void DirectDependencies_IgnoreCommentLines()
    {
        var g = new RektCopybookGraph();
        g.AddFile("BOOK.cpy", "01 WS-A PIC X(10).", isCopybook: true);
        g.AddFile("PROG.cbl",
            "      *      COPY BOOK.       <- this is a comment\n" +
            "       PROGRAM-ID. PROG.\n",
            isCopybook: false);

        var snap = g.BuildDependencySnapshot("PROG.cbl");
        snap.Should().BeEmpty();
    }

    [Fact]
    public void TransitiveDependency_AppearsInSnapshot()
    {
        var g = new RektCopybookGraph();
        g.AddFile("LEAF.cpy", "01 WS-LEAF PIC X.", isCopybook: true);
        g.AddFile("MID.cpy",
            "      *header\n" +
            "           COPY LEAF.\n",
            isCopybook: true);
        g.AddFile("PROG.cbl",
            "       PROGRAM-ID. PROG.\n" +
            "           COPY MID.\n",
            isCopybook: false);

        var snap = g.BuildDependencySnapshot("PROG.cbl");
        snap.Keys.Should().Contain(new[] { "MID.cpy", "LEAF.cpy" });
    }

    [Fact]
    public void MissingCopybook_SurfacedSeparately()
    {
        var g = new RektCopybookGraph();
        g.AddFile("PROG.cbl",
            "       PROGRAM-ID. PROG.\n" +
            "           COPY MISSING.\n",
            isCopybook: false);

        var snap = g.BuildDependencySnapshot("PROG.cbl");
        snap.Should().BeEmpty("the missing copybook can't contribute a hash");

        var missing = g.GetMissingCopybooks("PROG.cbl");
        missing.Should().Contain("MISSING");
    }
}
