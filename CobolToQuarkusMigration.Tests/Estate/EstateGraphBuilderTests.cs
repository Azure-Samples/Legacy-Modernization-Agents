using CobolToQuarkusMigration.Estate;
using FluentAssertions;
using Xunit;
using static CobolToQuarkusMigration.Tests.Estate.EstateSourceScannerTests;

namespace CobolToQuarkusMigration.Tests.Estate;

public sealed class EstateGraphBuilderTests : IDisposable
{
    private readonly string _root = Directory.CreateTempSubdirectory("estate-graph-").FullName;

    public void Dispose() => Directory.Delete(_root, recursive: true);

    private void Write(string relativePath, string text)
    {
        var path = Path.Join(_root, relativePath);
        Directory.CreateDirectory(Path.GetDirectoryName(path)!);
        File.WriteAllText(path, text);
    }

    private static EstateEdge Edge(EstateGraph g, string from, string to, string kind) =>
        g.Edges.Single(e => e.From == from && e.To == to && e.Kind == kind);

    private EstateGraph BatchEstate()
    {
        Write("src/PAYMAIN.cbl", Cobol(
            "PROGRAM-ID. PAYMAIN.",
            "FILE-CONTROL.",
            "    SELECT IN-FILE ASSIGN TO PAYIN.",
            "    SELECT OUT-FILE ASSIGN TO PAYOUT.",
            "PROCEDURE DIVISION.",
            "    COPY PAYREC.",
            "    CALL 'PAYCALC'."));
        Write("src/PAYCALC.cbl", Cobol("PROGRAM-ID. PAYCALC.", "    EXEC SQL UPDATE EMP SET X = 1 END-EXEC."));
        Write("src/RPTPGM.cbl", Cobol(
            "PROGRAM-ID. RPTPGM.",
            "FILE-CONTROL.",
            "    SELECT R-FILE ASSIGN TO RPTIN.",
            "PROCEDURE DIVISION.",
            "    CALL 'GHOST'."));
        Write("copy/PAYREC.cpy", Cobol("01 PAY-REC PIC X(80)."));
        Write("jcl/PAYJOB.jcl", string.Join("\n",
            "//PAYJOB   JOB (ACCT),'PAY'",
            "//STEP1    EXEC PGM=PAYMAIN",
            "//PAYIN    DD DSN=PAY.IN,DISP=SHR",
            "//PAYOUT   DD DSN=PAY.OUT,DISP=(NEW,CATLG,DELETE)",
            "//SYSOUT   DD SYSOUT=*", ""));
        Write("jcl/RPTJOB.jcl", string.Join("\n",
            "//RPTJOB   JOB (ACCT),'RPT'",
            "//STEP1    EXEC PGM=RPTPGM",
            "//RPTIN    DD DSN=PAY.OUT,DISP=SHR", ""));
        return EstateGraphBuilder.Build(_root);
    }

    [Fact]
    public void ProgramsCopybooksAndCallsBecomeNodesAndEdgesWithEvidence()
    {
        var g = BatchEstate();

        g.Nodes.Should().Contain(n => n.Id == "program:PAYMAIN" && n.InSource && n.File == "src/PAYMAIN.cbl");
        g.Nodes.Should().Contain(n => n.Id == "copybook:PAYREC" && n.InSource);
        Edge(g, "program:PAYMAIN", "program:PAYCALC", EstateEdgeKind.Calls).Evidence.Single().Line.Should().Be(7);
        Edge(g, "program:PAYMAIN", "copybook:PAYREC", EstateEdgeKind.Copies);
        Edge(g, "program:PAYCALC", "table:EMP", EstateEdgeKind.Updates);
    }

    [Fact]
    public void AReferenceTheSourceLacksIsAMissingNode()
    {
        var g = BatchEstate();

        g.Nodes.Single(n => n.Id == "program:GHOST").InSource.Should().BeFalse();
        g.Clusters.SelectMany(c => c.Missing).Should().Contain("GHOST");
    }

    [Fact]
    public void JclRunsProgramsAndConnectsThemToTheDatasetsTheirDdsName()
    {
        var g = BatchEstate();

        Edge(g, "job:PAYJOB", "program:PAYMAIN", EstateEdgeKind.Runs).Via.Should().Be("STEP1");
        Edge(g, "job:PAYJOB", "dataset:PAY.IN", EstateEdgeKind.Reads);
        Edge(g, "job:PAYJOB", "dataset:PAY.OUT", EstateEdgeKind.Writes);
        Edge(g, "program:PAYMAIN", "dataset:PAY.OUT", EstateEdgeKind.Writes);
        Edge(g, "program:RPTPGM", "dataset:PAY.OUT", EstateEdgeKind.Reads);
        g.Nodes.Should().NotContain(n => n.Kind == EstateNodeKind.Dataset && n.Name.Contains("SYSOUT"));
    }

    [Fact]
    public void AJobThatWritesWhatAnotherReadsFeedsItWithTheDdLinesAsEvidence()
    {
        var g = BatchEstate();

        var feeds = Edge(g, "job:PAYJOB", "job:RPTJOB", EstateEdgeKind.Feeds);
        feeds.Via.Should().Be("PAY.OUT");
        feeds.Evidence.Select(e => e.File).Should().Equal("jcl/PAYJOB.jcl", "jcl/RPTJOB.jcl");
    }

    [Fact]
    public void TheCsdSaysWhichProgramATransactionRuns()
    {
        Write("src/ACCTINQ.cbl", Cobol("PROGRAM-ID. ACCTINQ."));
        Write("src/MENU.cbl", Cobol("PROGRAM-ID. MENU.", "    EXEC CICS RETURN TRANSID('AC01') END-EXEC."));
        Write("csd/online.csd", string.Join("\n",
            "* online definitions",
            "DEFINE TRANSACTION(AC01) GROUP(ACCT)",
            "       PROGRAM(ACCTINQ)",
            "DEFINE FILE(ACCTFIL) GROUP(ACCT) DSNAME(ACCT.MASTER)", ""));

        var g = EstateGraphBuilder.Build(_root);

        Edge(g, "transaction:AC01", "program:ACCTINQ", EstateEdgeKind.Runs).Evidence.Single().Line.Should().Be(2);
        Edge(g, "file:ACCTFIL", "dataset:ACCT.MASTER", EstateEdgeKind.BackedBy);
        // MENU starts AC01, which runs ACCTINQ: the two belong in one cluster.
        g.Clusters.Single(c => c.Programs.Contains("program:MENU")).Programs.Should().Contain("program:ACCTINQ");
    }

    [Fact]
    public void TwoFilesWithOneNameAreBothKeptAndACallToThatNameReachesBoth()
    {
        Write("a/DUP.cbl", Cobol("PROGRAM-ID. DUP."));
        Write("b/DUP.cbl", Cobol("PROGRAM-ID. DUP."));
        Write("MAIN.cbl", Cobol("PROGRAM-ID. MAIN.", "    CALL 'DUP'."));

        var g = EstateGraphBuilder.Build(_root);

        g.Edges.Where(e => e.From == "program:MAIN" && e.Kind == EstateEdgeKind.Calls).Select(e => e.To)
            .Should().BeEquivalentTo("program:a/DUP.cbl", "program:b/DUP.cbl");
        g.Diagnostics.Should().Contain(d => d.Contains("DUP") && d.Contains("2 source files"));

        var slice = EstateAnalysis.Slice(g, g.Clusters.Single(c => c.Programs.Contains("program:MAIN")).Id)!;
        slice.ProgramSelectors.Concat(slice.NeedSelectors).Should().Contain(["a/DUP.cbl", "b/DUP.cbl", "MAIN.cbl"]);
    }

    [Fact]
    public void TheGraphIsTheSameEveryTime()
    {
        var first = BatchEstate();
        var second = EstateGraphBuilder.Build(_root);

        System.Text.Json.JsonSerializer.Serialize(second with { GeneratedAtUtc = first.GeneratedAtUtc }, EstateGraph.JsonOptions)
            .Should().Be(System.Text.Json.JsonSerializer.Serialize(first, EstateGraph.JsonOptions));
    }

    [Fact]
    public void ThePartialConfigSectionKeepsTheOtherDefaults()
    {
        Write("appsettings.json", """{ "EstateGraph": { "CouplingWeights": { "call": 9 }, "TargetSliceSize": 4 } }""");

        var options = EstateGraphOptions.Load(Path.Join(_root, "appsettings.json"), out var warning);

        warning.Should().BeNull();
        options.Coupling("call").Should().Be(9);
        options.Coupling("sameJob").Should().Be(2);
        options.Carve("cohesion").Should().Be(0.4);
        options.TargetSliceSize.Should().Be(4);
    }

    [Fact]
    public void AnUnreadableConfigSectionIsReportedAndTheDefaultsUsed()
    {
        Write("appsettings.json", """{ "EstateGraph": { "TargetSliceSize": "many" } }""");

        var options = EstateGraphOptions.Load(Path.Join(_root, "appsettings.json"), out var warning);

        warning.Should().Contain("EstateGraph");
        options.TargetSliceSize.Should().Be(15);
    }
}
