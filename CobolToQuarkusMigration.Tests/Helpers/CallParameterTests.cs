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
}
