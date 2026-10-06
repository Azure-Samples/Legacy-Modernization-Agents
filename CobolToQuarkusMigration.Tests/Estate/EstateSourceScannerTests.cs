using CobolToQuarkusMigration.Estate;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Estate;

public class EstateSourceScannerTests
{
    // Fixed format: sequence area, indicator column, code from column 8.
    internal static string Cobol(params string[] lines) =>
        string.Join("\n", lines.Select(l => l.StartsWith('*') ? "      " + l : "       " + l)) + "\n";

    private static ScannedReference Ref(ProgramScan scan, string kind, string target) =>
        scan.References.Single(r => r.Kind == kind && r.Target == target);

    [Fact]
    public void ALiteralCallIsAnEdgeToThatProgramWithItsLine()
    {
        var scan = EstateSourceScanner.Scan("src/PAY.cbl", Cobol(
            "IDENTIFICATION DIVISION.",
            "PROGRAM-ID. PAY.",
            "PROCEDURE DIVISION.",
            "    CALL 'TAXCALC' USING WS-REC."));

        scan.ProgramId.Should().Be("PAY");
        var call = Ref(scan, EstateEdgeKind.Calls, "TAXCALC");
        call.TargetKind.Should().Be(EstateNodeKind.Program);
        call.Evidence.Single().Should().Be(new EstateEvidence("src/PAY.cbl", 4, "CALL 'TAXCALC' USING WS-REC."));
    }

    [Fact]
    public void ACallThroughAVariableResolvesToTheLiteralsMovedIntoIt()
    {
        var scan = EstateSourceScanner.Scan("PAY.cbl", Cobol(
            "PROGRAM-ID. PAY.",
            "WORKING-STORAGE SECTION.",
            "01 WS-PGM PIC X(8) VALUE 'RATEA'.",
            "01 WS-MSG PIC X(30).",
            "PROCEDURE DIVISION.",
            "    MOVE 'RATEB' TO WS-PGM",
            "    MOVE 'NOT-A-PROGRAM-NAME' TO WS-PGM",
            "    CALL WS-PGM",
            "    CALL WS-OTHER."));

        scan.References.Where(r => r.Kind == EstateEdgeKind.Calls).Select(r => r.Target)
            .Should().BeEquivalentTo("RATEA", "RATEB");
        scan.UnresolvedDynamic.Select(r => r.Target).Should().Equal("WS-OTHER");
    }

    [Fact]
    public void CommentLinesAndInlineCommentsAreNotReferences()
    {
        var scan = EstateSourceScanner.Scan("PAY.cbl", Cobol(
            "PROGRAM-ID. PAY.",
            "* CALL 'OLDPGM'",
            "    COPY PAYREC. *> COPY GHOST",
            "    CALL 'LIVE'."));

        scan.References.Select(r => r.Target).Should().BeEquivalentTo("PAYREC", "LIVE");
    }

    [Fact]
    public void SystemCopybooksAreSkipped()
    {
        var scan = EstateSourceScanner.Scan("PAY.cbl", Cobol(
            "PROGRAM-ID. PAY.",
            "    EXEC SQL INCLUDE SQLCA END-EXEC.",
            "    EXEC SQL INCLUDE PAYTAB END-EXEC.",
            "    COPY PAYREC."));

        scan.References.Where(r => r.Kind == EstateEdgeKind.Copies).Select(r => r.Target)
            .Should().BeEquivalentTo("PAYTAB", "PAYREC");
    }

    [Fact]
    public void SqlStatementsReadAndWriteTables()
    {
        var scan = EstateSourceScanner.Scan("PAY.cbl", Cobol(
            "PROGRAM-ID. PAY.",
            "    EXEC SQL",
            "      SELECT A.X INTO :WS-X",
            "      FROM PAYROLL.EMP A, DEPT D",
            "      JOIN SYSIBM.SYSDUMMY1 ON 1 = 1",
            "    END-EXEC.",
            "    EXEC SQL INSERT INTO AUDIT_LOG VALUES (:WS-X) END-EXEC.",
            "    EXEC SQL UPDATE PAYROLL.EMP SET X = 1 END-EXEC.",
            "    EXEC SQL DELETE FROM HIST END-EXEC."));

        scan.References.Where(r => r.TargetKind == EstateNodeKind.Table)
            .Select(r => (r.Kind, r.Target))
            .Should().BeEquivalentTo(new[]
            {
                (EstateEdgeKind.Reads, "PAYROLL.EMP"),
                (EstateEdgeKind.Reads, "DEPT"),
                (EstateEdgeKind.Writes, "AUDIT_LOG"),
                (EstateEdgeKind.Updates, "PAYROLL.EMP"),
                (EstateEdgeKind.Deletes, "HIST"),
            });
        Ref(scan, EstateEdgeKind.Reads, "DEPT").Evidence.Single().Line.Should().Be(4);
    }

    [Fact]
    public void CicsCommandsLinkMapsStartTransactionsAndUseFiles()
    {
        var scan = EstateSourceScanner.Scan("ONL.cbl", Cobol(
            "PROGRAM-ID. ONL.",
            "    EXEC CICS LINK PROGRAM('ACCTINQ') COMMAREA(WS-CA) END-EXEC.",
            "    EXEC CICS XCTL PROGRAM(WS-NEXT) END-EXEC.",
            "    EXEC CICS SEND MAP('ACCTM1') MAPSET('ACCTSET') END-EXEC.",
            "    EXEC CICS RETURN TRANSID('AC01') END-EXEC.",
            "    EXEC CICS READ FILE('ACCTFIL') INTO(WS-REC) END-EXEC.",
            "    EXEC CICS REWRITE FILE('ACCTFIL') FROM(WS-REC) END-EXEC."));

        Ref(scan, EstateEdgeKind.Links, "ACCTINQ").TargetKind.Should().Be(EstateNodeKind.Program);
        scan.UnresolvedDynamic.Select(r => r.Target).Should().Equal("WS-NEXT");
        Ref(scan, EstateEdgeKind.UsesMap, "ACCTSET").TargetKind.Should().Be(EstateNodeKind.Map);
        Ref(scan, EstateEdgeKind.Starts, "AC01").TargetKind.Should().Be(EstateNodeKind.Transaction);
        Ref(scan, EstateEdgeKind.Reads, "ACCTFIL").TargetKind.Should().Be(EstateNodeKind.File);
        Ref(scan, EstateEdgeKind.Updates, "ACCTFIL").Evidence.Single().Line.Should().Be(7);
    }

    [Fact]
    public void TextPastColumn72IsTheSequenceAreaNotCode()
    {
        var scan = EstateSourceScanner.Scan("PAY.cbl", Cobol(
            "PROGRAM-ID. PAY.",
            "    CALL 'REAL'.                                             CALL 'NOTCODE'"));

        scan.References.Select(r => r.Target).Should().Equal("REAL");
    }

    [Fact]
    public void SelectAssignGivesTheDdNameWithoutDevicePrefix()
    {
        var scan = EstateSourceScanner.Scan("PAY.cbl", Cobol(
            "PROGRAM-ID. PAY.",
            "FILE-CONTROL.",
            "    SELECT IN-FILE ASSIGN TO UT-S-PAYIN.",
            "    SELECT OUT-FILE ASSIGN TO PAYOUT."));

        scan.AssignedDds.Should().BeEquivalentTo("PAYIN", "PAYOUT");
    }

    [Fact]
    public void FreeFormatSourceIsReadWhole()
    {
        var scan = EstateSourceScanner.Scan("PAY.cbl",
            ">>SOURCE FORMAT FREE\nPROGRAM-ID. PAY.\nPROCEDURE DIVISION.\nCALL 'FARAWAYPGM'.\n");

        Ref(scan, EstateEdgeKind.Calls, "FARAWAYPGM").Evidence.Single().Line.Should().Be(4);
    }
}
