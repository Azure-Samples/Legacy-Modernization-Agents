using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

public class CallParameterTests
{
    [Fact]
    public void ParametersAreTheUsingListTypedByTheCopybookEachLinkageRecordCopies()
    {
        const string program =
            "000100 LINKAGE SECTION.\n" +
            "000200 01  BDSMFJL-PARM.\n" +
            "000300     COPY BDSMFJLI.\n" +
            "000400 01  BDSDATO-PARM.\n" +
            "000500     COPY BDSDATOI.\n" +
            "000550*01  OLD-PARM.\n" +
            "000600 01  RAW-AREA.\n" +
            "000700     05 RAW-X PIC X(10).\n" +
            "000800 PROCEDURE DIVISION USING BDSDATO-PARM\n" +
            "000900                          BY REFERENCE BDSMFJL-PARM RAW-AREA.\n" +
            "001000 000-START SECTION.\n";

        CallTargetRegistry.UsingParameters(program).Should().Equal(
            new CallParameter("BDSDATO-PARM", "bdsdatoParm", "Bdsdatoi"),
            new CallParameter("BDSMFJL-PARM", "bdsmfjlParm", "Bdsmfjli"),
            new CallParameter("RAW-AREA", "rawArea", null));
    }

    [Fact]
    public void AProgramWithoutUsingHasNoParameters() =>
        CallTargetRegistry.UsingParameters("       PROCEDURE DIVISION.\n       MAIN.\n").Should().BeEmpty();

    private static string Folder(params (string Name, string Text)[] files)
    {
        var dir = Directory.CreateTempSubdirectory("callparams").FullName;
        foreach (var (name, text) in files) File.WriteAllText(Path.Combine(dir, name), text);
        return dir;
    }

    [Fact]
    public void ACalleeOutsideTheSourceIsTypedByTheRecordsItsCallersAgreeOn()
    {
        var dir = Folder(
            ("PARMC.cpy", "000100 01   PARMC.\n000200     05 P-TYPE PIC X.\n"),
            ("A.cbl", "       WORKING-STORAGE SECTION.\n           COPY PARMC.\n       PROCEDURE DIVISION.\n           CALL 'EXT' USING PARMC\n           END-CALL\n"),
            ("B.cpy", "000800     CALL 'EXT' USING BY REFERENCE PARMC\n000900     END-CALL\n"));
        try
        {
            CallTargetRegistry.Build(dir).Contracts.Should().ContainSingle(c => c.Target == "EXT")
                .Which.Parameters.Should().Equal(new CallParameter("PARMC", "parmc", "Parmc"));
        }
        finally { Directory.Delete(dir, true); }
    }

    [Fact]
    public void ACallPassingACallersOwnFieldGetsNoSignature()
    {
        var dir = Folder(
            ("PARMC.cpy", "000100 01   PARMC.\n000200     05 P PIC X.\n"),
            ("A.cpy", "003100     CALL 'EXT' USING PARMC WS-COUNT\n003200\n003300     IF P = 'X'\n"));
        try
        {
            CallTargetRegistry.Build(dir).Contracts.Should().ContainSingle(c => c.Target == "EXT")
                .Which.Parameters.Should().BeEmpty();
        }
        finally { Directory.Delete(dir, true); }
    }

    [Fact]
    public void ASequenceNumberOnlyLineEndsNoArgumentList()
    {
        var dir = Folder(
            ("PARMC.cpy", "000100 01   PARMC.\n000200     05 P PIC X.\n"),
            ("A.cpy", "003100     CALL 'EXT' USING PARMC\n003200\n003300     IF P = 'X'\n"));
        try
        {
            CallTargetRegistry.Build(dir).Contracts.Should().ContainSingle(c => c.Target == "EXT")
                .Which.Parameters.Should().Equal(new CallParameter("PARMC", "parmc", "Parmc"));
        }
        finally { Directory.Delete(dir, true); }
    }

    [Fact]
    public void CallersThatPassDifferentRecordsGetNoSignature()
    {
        var dir = Folder(
            ("PARMC.cpy", "       01  PARMC.\n           05 P PIC X.\n"),
            ("PARMD.cpy", "       01  PARMD.\n           05 Q PIC X.\n"),
            ("A.cbl", "       PROCEDURE DIVISION.\n           CALL 'EXT' USING PARMC.\n"),
            ("B.cbl", "       PROCEDURE DIVISION.\n           CALL 'EXT' USING PARMD.\n"));
        try
        {
            CallTargetRegistry.Build(dir).Contracts.Should().ContainSingle(c => c.Target == "EXT")
                .Which.Parameters.Should().BeEmpty();
        }
        finally { Directory.Delete(dir, true); }
    }
}
