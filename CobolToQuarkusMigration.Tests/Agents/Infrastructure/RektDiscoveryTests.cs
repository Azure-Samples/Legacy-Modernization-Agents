using CobolToQuarkusMigration.Agents.Infrastructure;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Agents.Infrastructure;

public sealed class RektDiscoveryTests
{
    [Theory]
    [InlineData("COPY")]
    [InlineData("CALL")]
    [InlineData("SQL")]
    public void SourceFreeLegacyLookupsCannotElevateFactsOrInvokeScanner(string kind)
    {
        var discovery = new RektDiscovery(".");
        var scanned = false;
        List<string> Fallback() { scanned = true; return ["INVENTED"]; }
        var discover = () => kind switch
        {
            "COPY" => discovery.Copybooks("billing/ORDER.cbl", Fallback),
            "CALL" => discovery.Callees("billing/ORDER.cbl", Fallback),
            _ => discovery.DbTables("billing/ORDER.cbl", Fallback)
        };
        discover.Should().Throw<InvalidDataException>().WithMessage("*original source identity/hash and per-kind coverage*");
        scanned.Should().BeFalse();
        discovery.AnsweredByTextScan.Should().Be(0);
    }
}
