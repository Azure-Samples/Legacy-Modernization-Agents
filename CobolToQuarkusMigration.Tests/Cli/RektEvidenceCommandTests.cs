using CobolToQuarkusMigration.Cli;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Cli;

public class RektEvidenceCommandTests
{
    [Theory]
    [InlineData("commarea-closure-carrier-unsupported:BNKMENU:COMMUNICATION-AREA", "BNKMENU passes COMMUNICATION-AREA in a form the closure cannot follow")]
    [InlineData("commarea-closure-incompatible-layouts:8", "8 different COMMAREA layouts share the record")]
    [InlineData("commarea-closure-foreign-send:COSGN00C:WS-AREA:120", "COSGN00C sends WS-AREA (line 120) to a program outside the estate")]
    [InlineData("something-new:X", "something-new:X")]
    public void ClosureErrors_AreDescribedInPlainWords(string error, string expected)
    {
        RektEvidenceCommand.DescribeClosureError(error).Should().Be(expected);
    }
}
