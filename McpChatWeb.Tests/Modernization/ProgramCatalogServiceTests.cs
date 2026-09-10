using System.Linq;
using System.Threading.Tasks;
using McpChatWeb.Services;
using Microsoft.Extensions.Logging.Abstractions;
using Xunit;

namespace McpChatWeb.Tests.Modernization;

// The catalog is the portal's decision surface for focused conversion: what it shows determines
// what a user picks, so an invented or missing entry converts the wrong thing.
public class ProgramCatalogServiceTests
{
    [Fact]
    public async Task Catalog_ListsProgramsByTheirSourceRelativePath()
    {
        using var fixture = new EstateFixture()
            .AddProgram("finance/LEDGER.cbl")
            .AddProgram("billing/INVOICE.cbl");

        var catalog = await BuildAsync(fixture);

        // A basename alone cannot address a program in an estate with nested folders.
        Assert.Equal(
            new[] { "billing/INVOICE.cbl", "finance/LEDGER.cbl" },
            catalog.Programs.Select(p => p.RelativePath).OrderBy(p => p).ToArray());
    }

    [Fact]
    public async Task Catalog_DoesNotOfferCopybooksAsConversionTargets()
    {
        using var fixture = new EstateFixture()
            .AddProgram("finance/LEDGER.cbl")
            .AddProgram("shared/ACCOUNT.cpy", "       01 ACCOUNT-REC.\n");

        var catalog = await BuildAsync(fixture);

        Assert.DoesNotContain(catalog.Programs, p => p.RelativePath == "shared/ACCOUNT.cpy");
    }

    [Fact]
    public async Task Catalog_FlagsAmbiguousBasenamesSoTheUiCanRequireAPath()
    {
        using var fixture = new EstateFixture()
            .AddProgram("finance/LEDGER.cbl")
            .AddProgram("billing/LEDGER.cbl");

        var catalog = await BuildAsync(fixture);

        Assert.Equal(2, catalog.Programs.Count);
        Assert.All(catalog.Programs, p => Assert.True(p.AmbiguousBasename));
    }

    [Fact]
    public async Task Catalog_ReportsClosureCountsOnlyWhereScanEvidenceExists()
    {
        using var fixture = new EstateFixture()
            .AddProgram("finance/LEDGER.cbl")
            .AddProgram("shared/POSTING.cbl");
        fixture.AddFacts("finance/LEDGER.cbl", confidence: 90, callees: ["POSTING"]);

        var catalog = await BuildAsync(fixture);

        var ledger = catalog.Programs.Single(p => p.RelativePath == "finance/LEDGER.cbl");
        Assert.True(ledger.HasClosureEvidence);
        Assert.Equal(1, ledger.CalleeCount);

        // Reporting zero callees for an unscanned program would look like a measured result.
        var posting = catalog.Programs.Single(p => p.RelativePath == "shared/POSTING.cbl");
        Assert.False(posting.HasClosureEvidence);
        Assert.Null(posting.CalleeCount);
    }

    [Fact]
    public async Task Catalog_DeclaresClosureUnusableWhenNoFactsExist()
    {
        using var fixture = new EstateFixture().AddProgram("finance/LEDGER.cbl");

        var catalog = await BuildAsync(fixture);

        Assert.False(catalog.ClosureAvailable);
        Assert.Contains("rekt-scan", catalog.ClosureUnavailableReason);
    }

    [Fact]
    public async Task Catalog_DeclaresClosureUsableOnceAnyProgramHasFacts()
    {
        using var fixture = new EstateFixture().AddProgram("finance/LEDGER.cbl");
        fixture.AddFacts("finance/LEDGER.cbl", confidence: 90);

        var catalog = await BuildAsync(fixture);

        Assert.True(catalog.ClosureAvailable);
        Assert.Equal("", catalog.ClosureUnavailableReason);
    }

    [Fact]
    public async Task Catalog_DeclaresDeferredSelectorKindsRatherThanFabricatingThem()
    {
        using var fixture = new EstateFixture().AddProgram("finance/LEDGER.cbl");

        var catalog = await BuildAsync(fixture);

        // The portal must label these as not shipped rather than render plausible empty filters.
        Assert.Equal(
            new[] { "component", "transaction", "wave" },
            catalog.DeferredSelectors.OrderBy(s => s).ToArray());
    }

    [Fact]
    public async Task Catalog_CountsMissingCopybooksReportedAgainstTheProgram()
    {
        using var fixture = new EstateFixture().AddProgram("finance/LEDGER.cbl");
        fixture.AddMissingCopybooks(
            "ACCOUNT\treferenced by: LEDGER\nTAXRATE\treferenced by: LEDGER\nUNUSED\treferenced by: INVOICE\n");

        var catalog = await BuildAsync(fixture);

        // Drives the modal's "needs AI fallback" hint, so a placeholder count would mislead.
        Assert.Equal(2, Assert.Single(catalog.Programs).MissingCopybookCount);
    }

    [Fact]
    public async Task Search_MatchesBasenameAndPathCaseInsensitively()
    {
        using var fixture = new EstateFixture()
            .AddProgram("finance/LEDGER.cbl")
            .AddProgram("billing/INVOICE.cbl");

        var service = Create(fixture);
        var catalog = await service.BuildCatalogAsync();

        Assert.Equal("finance/LEDGER.cbl", Assert.Single(service.Search(catalog, "ledg")).RelativePath);
        Assert.Equal("billing/INVOICE.cbl", Assert.Single(service.Search(catalog, "BILLING")).RelativePath);
        Assert.Equal(2, service.Search(catalog, "").Count);
    }

    private static async Task<ProgramCatalog> BuildAsync(EstateFixture fixture) =>
        await Create(fixture).BuildCatalogAsync();

    private static ProgramCatalogService Create(EstateFixture fixture) =>
        new(new RektEstateReader(NullLogger<RektEstateReader>.Instance, fixture.RepoRoot));
}
