using System.Linq;
using System.Threading.Tasks;
using CobolToQuarkusMigration.Agents.Infrastructure.RektCache;
using McpChatWeb.Services;
using Microsoft.Extensions.Logging.Abstractions;
using Xunit;

namespace McpChatWeb.Tests.Modernization;

/// <summary>
/// Covers how parse fidelity is resolved and where it is claimed to come from.
/// The reported source matters as much as the value: the portal presents this as
/// a deterministic decision surface, so an inferred verdict must never be shown
/// as a measured one.
/// </summary>
public class RektEstateReaderTests
{
    private static RektEstateReader ReaderFor(EstateFixture fixture) =>
        new(NullLogger<RektEstateReader>.Instance, fixture.RepoRoot);

    [Fact]
    public async Task ScanCache_TakesPrecedenceOverFacts()
    {
        using var fixture = new EstateFixture();
        fixture.AddProgram("CUSTOMER.cbl");
        // Facts claim a full parse; the scan cache is the authoritative record
        // and says the dialect was unavailable.
        fixture.AddFacts("CUSTOMER.cbl", confidence: 3);
        await fixture.AddScanEntryAsync("CUSTOMER.cbl", RektParseOutcome.NoDialect, RektScanConfidence.Partial);

        var estate = await ReaderFor(fixture).ReadAsync();
        var record = Assert.Single(estate.Programs);

        Assert.Equal(ParseFidelity.Partial, record.ParseFidelity);
        Assert.Equal(FidelitySources.ScanCache, record.FidelitySource);
    }

    [Fact]
    public async Task Facts_UsedWhenScanCacheHasNoEntry()
    {
        using var fixture = new EstateFixture();
        fixture.AddProgram("CUSTOMER.cbl").AddFacts("CUSTOMER.cbl", confidence: 3);

        var estate = await ReaderFor(fixture).ReadAsync();
        var record = Assert.Single(estate.Programs);

        Assert.Equal(ParseFidelity.Full, record.ParseFidelity);
        Assert.Equal(FidelitySources.Facts, record.FidelitySource);
    }

    [Fact]
    public async Task DepsOnlyArtifacts_ReportDepsOnlyFidelity()
    {
        using var fixture = new EstateFixture();
        // A dependency file without facts or a report is the deps-only fallback:
        // the program's call graph is known but its body was never parsed.
        fixture.AddProgram("CUSTOMER.cbl").AddDeps("CUSTOMER", "ACCOUNT", "CUSTREC.cpy");

        var estate = await ReaderFor(fixture).ReadAsync();
        var record = Assert.Single(estate.Programs);

        Assert.Equal(ParseFidelity.DepsOnly, record.ParseFidelity);
        Assert.Equal(FidelitySources.Artifacts, record.FidelitySource);
        Assert.Contains("ACCOUNT", record.Callees);
        Assert.Contains("CUSTREC.cpy", record.Copybooks);
    }

    [Fact]
    public async Task ReportWithoutScanCacheOrFacts_ReportsPartialNotFull()
    {
        using var fixture = new EstateFixture();
        // What `./doctor.sh rekt-full` alone leaves on disk: a report directory,
        // no scan cache and no facts. A stub-backed parse with degraded
        // structural facts emits a report directory identical to a clean one, so
        // artifacts cannot establish Full without overstating readiness.
        fixture.AddProgram("CUSTOMER.cbl").AddReportDirectory("CUSTOMER.cbl");

        var estate = await ReaderFor(fixture).ReadAsync();
        var record = Assert.Single(estate.Programs);

        Assert.Equal(ParseFidelity.Partial, record.ParseFidelity);
        Assert.Equal(FidelitySources.Artifacts, record.FidelitySource);
    }

    [Fact]
    public async Task NoArtifacts_ReportsNotParsedWithNoSource()
    {
        using var fixture = new EstateFixture();
        fixture.AddProgram("CUSTOMER.cbl");

        var estate = await ReaderFor(fixture).ReadAsync();
        var record = Assert.Single(estate.Programs);

        Assert.Equal(ParseFidelity.NotParsed, record.ParseFidelity);
        Assert.Equal(FidelitySources.None, record.FidelitySource);
        Assert.NotNull(estate.Note);
    }

    [Fact]
    public async Task AmbiguousBasename_DoesNotConsumeScanCacheEntry()
    {
        using var fixture = new EstateFixture();
        // The scan cache is keyed by basename while sources are keyed by
        // relative path. Two files sharing a basename cannot be told apart, so
        // neither may claim the single cache row.
        fixture.AddProgram("billing/CUSTOMER.cbl");
        fixture.AddProgram("legacy/CUSTOMER.cbl");
        await fixture.AddScanEntryAsync("CUSTOMER.cbl", RektParseOutcome.Full, RektScanConfidence.High);

        var estate = await ReaderFor(fixture).ReadAsync();

        Assert.Equal(2, estate.Programs.Count);
        Assert.All(estate.Programs, r => Assert.True(r.AmbiguousBasename));
        Assert.All(estate.Programs, r => Assert.NotEqual(FidelitySources.ScanCache, r.FidelitySource));
    }

    [Fact]
    public async Task NestedFactsArtifacts_AreResolvedBySourceRelativePath()
    {
        using var fixture = new EstateFixture();
        fixture.AddProgram("billing/CUSTOMER.cbl");
        fixture.AddFacts("billing/CUSTOMER.cbl", confidence: 3, loc: 120);

        var estate = await ReaderFor(fixture).ReadAsync();
        var record = Assert.Single(estate.Programs);

        Assert.Equal(ParseFidelity.Full, record.ParseFidelity);
        Assert.Equal(120, record.LinesOfCode);
    }

    [Fact]
    public async Task FailedParse_IsDistinctFromNotParsed()
    {
        using var fixture = new EstateFixture();
        fixture.AddProgram("BROKEN.cbl");
        await fixture.AddScanEntryAsync("BROKEN.cbl", RektParseOutcome.Failed, RektScanConfidence.None);

        var estate = await ReaderFor(fixture).ReadAsync();
        var record = Assert.Single(estate.Programs);

        Assert.Equal(ParseFidelity.Failed, record.ParseFidelity);
    }

    [Fact]
    public async Task ScratchDirectories_AreExcluded()
    {
        using var fixture = new EstateFixture();
        fixture.AddProgram("CUSTOMER.cbl");
        fixture.AddProgram(".convert-abc123/TEMP.cbl");
        fixture.AddProgram(".rekt-staging/STAGED.cbl");
        fixture.AddProgram(".preprocessed/PRE.cbl");

        var estate = await ReaderFor(fixture).ReadAsync();

        Assert.Single(estate.Programs);
        Assert.Equal("CUSTOMER.cbl", estate.Programs[0].Basename);
    }

    [Fact]
    public void MissingCopybooks_AreParsedWithReferencingPrograms()
    {
        using var fixture = new EstateFixture();
        fixture.AddMissingCopybooks(
            "# generated by preprocess-for-rekt.sh\n" +
            "CUSTREC\treferenced by: CUSTOMER.cbl, BILLING.cbl\n" +
            "ACCTREC\treferenced by: ACCOUNT.cbl\n");

        var rows = ReaderFor(fixture).ReadMissingCopybooks();

        Assert.Equal(2, rows.Count);
        var custrec = rows.Single(r => r.Copybook == "CUSTREC");
        Assert.Equal(new[] { "CUSTOMER.cbl", "BILLING.cbl" }, custrec.ReferencedBy);
    }

    [Fact]
    public async Task MissingSourceDirectory_ReturnsEmptyEstateWithNote()
    {
        using var fixture = new EstateFixture();
        System.IO.Directory.Delete(fixture.SourceRoot, recursive: true);

        var estate = await ReaderFor(fixture).ReadAsync();

        Assert.Empty(estate.Programs);
        Assert.NotNull(estate.Note);
    }
}
