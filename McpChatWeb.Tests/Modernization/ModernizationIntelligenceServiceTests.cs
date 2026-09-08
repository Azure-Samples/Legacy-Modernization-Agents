using System.Linq;
using System.Threading.Tasks;
using CobolToQuarkusMigration.Agents.Infrastructure.RektCache;
using McpChatWeb.Services;
using Microsoft.Extensions.Logging.Abstractions;
using Xunit;

namespace McpChatWeb.Tests.Modernization;

public class ModernizationIntelligenceServiceTests
{
    private static ModernizationIntelligenceService ServiceFor(EstateFixture fixture) =>
        new(new RektEstateReader(NullLogger<RektEstateReader>.Instance, fixture.RepoRoot));

    [Fact]
    public async Task DependencyHealth_CountsEachFidelityBucket()
    {
        using var fixture = new EstateFixture();
        fixture.AddProgram("FULL.cbl").AddProgram("PART.cbl")
               .AddProgram("DEPS.cbl").AddProgram("BAD.cbl").AddProgram("NONE.cbl");
        await fixture.AddScanEntryAsync("FULL.cbl", RektParseOutcome.Full, RektScanConfidence.High);
        await fixture.AddScanEntryAsync("PART.cbl", RektParseOutcome.StubBacked, RektScanConfidence.Partial);
        await fixture.AddScanEntryAsync("DEPS.cbl", RektParseOutcome.DepsOnly, RektScanConfidence.Low);
        await fixture.AddScanEntryAsync("BAD.cbl", RektParseOutcome.Failed, RektScanConfidence.None);

        var health = await ServiceFor(fixture).GetDependencyHealthAsync();

        Assert.Equal(5, health.TotalPrograms);
        Assert.Equal(1, health.FullFidelityCount);
        Assert.Equal(1, health.PartialFidelityCount);
        Assert.Equal(1, health.DepsOnlyCount);
        Assert.Equal(1, health.FailedCount);
        Assert.Equal(1, health.NotParsedCount);
        Assert.Equal(4, health.ScanCacheBackedCount);
        Assert.Equal(20d, health.CoveragePct, 3);
        // (1*1.0 + 1*0.5 + 1*0.25) / 5 * 100
        Assert.Equal(35d, health.ReadinessScore, 3);
    }

    [Fact]
    public async Task DependencyHealth_ReportsProgramsBlockedByMissingCopybooks()
    {
        using var fixture = new EstateFixture();
        fixture.AddProgram("CUSTOMER.cbl").AddProgram("BILLING.cbl").AddProgram("ACCOUNT.cbl");
        fixture.AddMissingCopybooks(
            "CUSTREC\treferenced by: CUSTOMER.cbl, BILLING.cbl\n");

        var health = await ServiceFor(fixture).GetDependencyHealthAsync();

        Assert.Equal(1, health.TotalMissingCopybooks);
        Assert.Equal(2, health.ProgramsBlockedByMissing);
        Assert.Equal(1, health.Programs.Single(p => p.Basename == "CUSTOMER.cbl").MissingCopybookCount);
        Assert.Equal(0, health.Programs.Single(p => p.Basename == "ACCOUNT.cbl").MissingCopybookCount);
    }

    [Fact]
    public async Task Topology_ResolvesCallAndCopyEdges()
    {
        using var fixture = new EstateFixture();
        fixture.AddProgram("CUSTOMER.cbl").AddProgram("ACCOUNT.cbl").AddProgram("CUSTREC.cpy");
        fixture.AddDeps("CUSTOMER", "ACCOUNT", "CUSTREC.cpy");

        var topology = await ServiceFor(fixture).GetTopologyAsync();

        Assert.Contains(topology.Edges, e =>
            e.Source == "CUSTOMER.cbl" && e.Target == "ACCOUNT.cbl" && e.Kind == "call");
        Assert.Contains(topology.Edges, e =>
            e.Source == "CUSTOMER.cbl" && e.Target == "CUSTREC.cpy" && e.Kind == "copy");
        Assert.Empty(topology.UnresolvedEdges);
    }

    [Fact]
    public async Task Topology_SurfacesUnresolvedTargetsSeparately()
    {
        using var fixture = new EstateFixture();
        fixture.AddProgram("CUSTOMER.cbl");
        fixture.AddDeps("CUSTOMER", "NOSUCHPGM", "GONE.cpy");

        var topology = await ServiceFor(fixture).GetTopologyAsync();

        Assert.Empty(topology.Edges);
        Assert.Equal(2, topology.UnresolvedEdges.Count);
        Assert.Contains(topology.UnresolvedEdges, e => e.Target == "NOSUCHPGM");
    }

    [Fact]
    public async Task ServiceChain_LinksJobsToProgramsAndCopybooks()
    {
        using var fixture = new EstateFixture();
        fixture.AddProgram("CUSTOMER.cbl").AddProgram("CUSTREC.cpy");
        fixture.AddDeps("CUSTOMER", "CUSTREC.cpy");
        fixture.AddJcl("jcl/NIGHTLY.jcl", """
            //NIGHTLY  JOB (ACCT),'NIGHTLY BATCH',CLASS=A
            //STEP010  EXEC PGM=CUSTOMER
            //STEP020  EXEC PGM=IDCAMS
            """);

        var chain = await ServiceFor(fixture).GetServiceChainAsync(null, null, includeUtilities: false);

        Assert.Equal(1, chain.TotalJobs);
        var job = Assert.Single(chain.Jobs);
        Assert.Equal("NIGHTLY", job.JobName);
        // IDCAMS is an MVS utility, not a migration target.
        Assert.Equal(new[] { "CUSTOMER" }, job.PrimaryPrograms);

        var program = Assert.Single(chain.Programs, p => p.Basename == "CUSTOMER.cbl");
        Assert.Contains("CUSTREC.cpy", program.Copybooks);
        Assert.Contains("NIGHTLY", program.CalledByJobs);
    }

    [Fact]
    public async Task ServiceChain_IncludeUtilities_RetainsSystemPrograms()
    {
        using var fixture = new EstateFixture();
        fixture.AddJcl("jcl/NIGHTLY.jcl", """
            //NIGHTLY  JOB (ACCT),'NIGHTLY BATCH',CLASS=A
            //STEP020  EXEC PGM=IDCAMS
            """);

        var chain = await ServiceFor(fixture).GetServiceChainAsync(null, null, includeUtilities: true);

        Assert.Contains("IDCAMS", Assert.Single(chain.Jobs).PrimaryPrograms);
    }

    [Fact]
    public async Task ServiceChain_JobFilter_NarrowsToOneJob()
    {
        using var fixture = new EstateFixture();
        fixture.AddProgram("CUSTOMER.cbl").AddProgram("BILLING.cbl");
        fixture.AddJcl("jcl/NIGHTLY.jcl", """
            //NIGHTLY  JOB (ACCT),'A',CLASS=A
            //STEP010  EXEC PGM=CUSTOMER
            """);
        fixture.AddJcl("jcl/MONTHLY.jcl", """
            //MONTHLY  JOB (ACCT),'B',CLASS=A
            //STEP010  EXEC PGM=BILLING
            """);

        var chain = await ServiceFor(fixture).GetServiceChainAsync("NIGHTLY", null, includeUtilities: false);

        Assert.Equal("NIGHTLY", Assert.Single(chain.Jobs).JobName);
        Assert.DoesNotContain(chain.Programs, p => p.Basename == "BILLING.cbl");
    }

    [Fact]
    public async Task ServiceChain_HyphenatedProgramNames_StillLinkJobsToPrograms()
    {
        using var fixture = new EstateFixture();
        // A capture stopping at the hyphen yields CUSTOMER, which matches no program,
        // so every job-to-program edge vanishes without an error being reported.
        fixture.AddProgram("CUSTOMER-INQUIRY.cbl").AddProgram("CUSTOMER-DISPLAY.cbl");
        fixture.AddJcl("jcl/NIGHTLY.jcl", """
            //NIGHTLY  JOB (ACCT),'A',CLASS=A
            //STEP010  EXEC PGM=CUSTOMER-INQUIRY
            //STEP020  EXEC PGM=CUSTOMER-DISPLAY
            """);

        var chain = await ServiceFor(fixture).GetServiceChainAsync(null, null, includeUtilities: false);

        var job = Assert.Single(chain.Jobs);
        Assert.Equal(new[] { "CUSTOMER-INQUIRY", "CUSTOMER-DISPLAY" }, job.PrimaryPrograms);
        Assert.Contains(
            "NIGHTLY",
            Assert.Single(chain.Programs, p => p.Basename == "CUSTOMER-INQUIRY.cbl").CalledByJobs);
    }

    [Fact]
    public async Task ServiceChain_JobFilter_KeepsProgramsThatJobRuns()
    {
        using var fixture = new EstateFixture();
        fixture.AddProgram("CUSTOMER-INQUIRY.cbl").AddProgram("BILLING.cbl");
        fixture.AddJcl("jcl/NIGHTLY.jcl", """
            //NIGHTLY  JOB (ACCT),'A',CLASS=A
            //STEP010  EXEC PGM=CUSTOMER-INQUIRY
            """);

        var chain = await ServiceFor(fixture).GetServiceChainAsync("NIGHTLY", null, includeUtilities: false);

        Assert.Equal("CUSTOMER-INQUIRY.cbl", Assert.Single(chain.Programs).Basename);
    }

    [Fact]
    public async Task ServiceChain_ProgramFilter_KeepsJobsThatRunIt()
    {
        using var fixture = new EstateFixture();
        fixture.AddProgram("CUSTOMER-DISPLAY.cbl");
        fixture.AddJcl("jcl/NIGHTLY.jcl", """
            //NIGHTLY  JOB (ACCT),'A',CLASS=A
            //STEP010  EXEC PGM=CUSTOMER-DISPLAY
            """);
        fixture.AddJcl("jcl/WEEKEND.jcl", """
            //WEEKEND  JOB (ACCT),'B',CLASS=A
            //STEP010  EXEC PGM=CUSTOMER-DISPLAY
            """);

        var chain = await ServiceFor(fixture)
            .GetServiceChainAsync(null, "CUSTOMER-DISPLAY", includeUtilities: false);

        Assert.Equal(
            new[] { "NIGHTLY", "WEEKEND" },
            chain.Jobs.Select(j => j.JobName).OrderBy(n => n).ToArray());
        Assert.Equal("CUSTOMER-DISPLAY.cbl", Assert.Single(chain.Programs).Basename);
    }

    [Fact]
    public async Task ServiceChain_JobAndProgramFilter_ThatDisagree_ReturnNothing()
    {
        using var fixture = new EstateFixture();
        fixture.AddProgram("CUSTOMER-INQUIRY.cbl").AddProgram("CUSTOMER-DISPLAY.cbl");
        fixture.AddJcl("jcl/WEEKEND.jcl", """
            //WEEKEND  JOB (ACCT),'B',CLASS=A
            //STEP010  EXEC PGM=CUSTOMER-DISPLAY
            """);

        // WEEKEND never runs CUSTOMER-INQUIRY, so returning it would imply a
        // schedule relationship that does not exist.
        var chain = await ServiceFor(fixture)
            .GetServiceChainAsync("WEEKEND", "CUSTOMER-INQUIRY", includeUtilities: false);

        Assert.Empty(chain.Jobs);
        Assert.Empty(chain.Programs);
    }

    [Fact]
    public async Task ServiceChain_EmptyEstate_ReturnsNoteNotFailure()
    {
        using var fixture = new EstateFixture();

        var chain = await ServiceFor(fixture).GetServiceChainAsync(null, null, includeUtilities: false);

        Assert.Equal(0, chain.TotalJobs);
        Assert.NotNull(chain.Note);
    }

    [Fact]
    public async Task Flow_UnknownProgram_ReportsNoteWithoutThrowing()
    {
        using var fixture = new EstateFixture();
        fixture.AddProgram("CUSTOMER.cbl");

        var flow = await ServiceFor(fixture).GetProgramFlowAsync("NOSUCH.cbl");

        Assert.NotNull(flow.Note);
        Assert.False(flow.HasFlowAst);
    }

    [Fact]
    public async Task Flow_ResolvesByBasenameAndRelativePath()
    {
        using var fixture = new EstateFixture();
        fixture.AddProgram("billing/CUSTOMER.cbl");
        fixture.AddFacts("billing/CUSTOMER.cbl", confidence: 3);

        var service = ServiceFor(fixture);
        var byPath = await service.GetProgramFlowAsync("billing/CUSTOMER.cbl");
        var byBasename = await service.GetProgramFlowAsync("CUSTOMER.cbl");

        Assert.Equal("CUSTOMER.cbl", byPath.Basename);
        Assert.Equal(byPath.RelativePath, byBasename.RelativePath);
    }

    // Dependency Health flags shared basenames as ambiguous and the UI answers by sending
    // the exact relative path, so an exact path must win outright instead of re-matching
    // every sibling by stem and reporting the ambiguity straight back.
    [Fact]
    public async Task Flow_ExactRelativePathWinsOverSiblingSharingBasename()
    {
        using var fixture = new EstateFixture();
        fixture.AddProgram("billing/CUSTOMER.cbl");
        fixture.AddProgram("legacy/CUSTOMER.cbl");

        var flow = await ServiceFor(fixture).GetProgramFlowAsync("billing/CUSTOMER.cbl");

        Assert.Equal("billing/CUSTOMER.cbl", flow.RelativePath);
        Assert.Empty(flow.Candidates);
    }

    [Fact]
    public async Task Flow_AmbiguousBasenameStillOffersCandidates()
    {
        using var fixture = new EstateFixture();
        fixture.AddProgram("billing/CUSTOMER.cbl");
        fixture.AddProgram("legacy/CUSTOMER.cbl");

        var flow = await ServiceFor(fixture).GetProgramFlowAsync("CUSTOMER.cbl");

        Assert.Equal(2, flow.Candidates.Count);
        Assert.Contains("billing/CUSTOMER.cbl", flow.Candidates);
    }
}
