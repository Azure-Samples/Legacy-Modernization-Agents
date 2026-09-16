using System.Linq;
using System.Threading.Tasks;
using CobolToQuarkusMigration.Agents.Infrastructure.RektCache;
using McpChatWeb.Services;
using Microsoft.Extensions.Logging.Abstractions;
using Xunit;

namespace McpChatWeb.Tests.Modernization;

// The view a person uses when deciding whether to convert one program. Deciding that from the
// direct edges alone is a guess: converting a program without its callees leaves calls pointing at
// nothing, and converting one whose copybooks are missing produces invented record layouts.
public class ProgramSnapshotTests
{
    private static ModernizationIntelligenceService ServiceFor(EstateFixture fixture) =>
        new(new RektEstateReader(NullLogger<RektEstateReader>.Instance, fixture.RepoRoot));

    [Fact]
    public async Task ReportsWhatTheProgramCallsAndWhatCallsIt()
    {
        using var fixture = new EstateFixture();
        fixture.AddProgram("billing/ORDER.cbl").AddProgram("billing/PRICING.cbl").AddProgram("billing/ENTRY.cbl");
        fixture.AddFacts("billing/ORDER.cbl", confidence: 3, callees: new[] { "PRICING" });
        fixture.AddFacts("billing/ENTRY.cbl", confidence: 3, callees: new[] { "ORDER" });

        var snapshot = await ServiceFor(fixture).GetProgramAsync("billing/ORDER.cbl");

        Assert.Equal("ORDER.cbl", snapshot.Basename);
        Assert.Contains("PRICING", snapshot.Calls);
        Assert.Contains("ENTRY.cbl", snapshot.CalledBy);
    }

    [Fact]
    public async Task TheClosureReachesBeyondTheDirectCallees()
    {
        // Converting ORDER alone leaves its call to PRICING pointing at unconverted code, and
        // PRICING's call to TAX likewise. The closure is what has to move together.
        using var fixture = new EstateFixture();
        fixture.AddProgram("billing/ORDER.cbl").AddProgram("billing/PRICING.cbl").AddProgram("billing/TAX.cbl");
        fixture.AddFacts("billing/ORDER.cbl", confidence: 3, callees: new[] { "PRICING" });
        fixture.AddFacts("billing/PRICING.cbl", confidence: 3, callees: new[] { "TAX" });

        var snapshot = await ServiceFor(fixture).GetProgramAsync("billing/ORDER.cbl");

        Assert.Contains("billing/PRICING.cbl", snapshot.CallClosure);
        Assert.Contains("billing/TAX.cbl", snapshot.CallClosure);
        Assert.DoesNotContain("billing/ORDER.cbl", snapshot.CallClosure);
    }

    [Fact]
    public async Task ACycleDoesNotHangTheClosure()
    {
        // COBOL estates legitimately contain them.
        using var fixture = new EstateFixture();
        fixture.AddProgram("a/A.cbl").AddProgram("a/B.cbl");
        fixture.AddFacts("a/A.cbl", confidence: 3, callees: new[] { "B" });
        fixture.AddFacts("a/B.cbl", confidence: 3, callees: new[] { "A" });

        var snapshot = await ServiceFor(fixture).GetProgramAsync("a/A.cbl");

        Assert.Contains("a/B.cbl", snapshot.CallClosure);
        Assert.Single(snapshot.CallClosure);
    }

    [Fact]
    public async Task AnAmbiguousNameAsksWhichOneRatherThanPickingOne()
    {
        using var fixture = new EstateFixture();
        fixture.AddProgram("billing/CUSTOMER.cbl").AddProgram("claims/CUSTOMER.cbl");

        var snapshot = await ServiceFor(fixture).GetProgramAsync("CUSTOMER.cbl");

        Assert.True(snapshot.AmbiguousBasename);
        Assert.Equal(2, snapshot.Candidates.Count);
        Assert.Null(snapshot.Basename);
        Assert.Contains("which one", snapshot.Note);
    }

    [Fact]
    public async Task AnUnknownProgramSaysSoRatherThanReturningAnEmptyView()
    {
        using var fixture = new EstateFixture();
        fixture.AddProgram("billing/ORDER.cbl");

        var snapshot = await ServiceFor(fixture).GetProgramAsync("NOSUCH.cbl");

        Assert.Null(snapshot.Basename);
        Assert.Contains("No source file matches", snapshot.Note);
    }

    [Fact]
    public async Task ADepsOnlyProgramSaysWhatIsMissingFromIt()
    {
        using var fixture = new EstateFixture();
        fixture.AddProgram("billing/ORDER.cbl").AddDeps("ORDER", "PRICING");
        await fixture.AddScanEntryAsync("ORDER.cbl", RektParseOutcome.DepsOnly, RektScanConfidence.Low);

        var snapshot = await ServiceFor(fixture).GetProgramAsync("billing/ORDER.cbl");

        Assert.Contains("deps-only", snapshot.Note);
    }
}
