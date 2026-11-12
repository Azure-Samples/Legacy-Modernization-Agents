using CobolToQuarkusMigration.Agents.Infrastructure.RektCache;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Agents.Infrastructure.RektCache;

public sealed class IncrementalScanPlannerTests
{
    [Fact]
    public async Task RecoveredAst_RetainsPartialCacheEvidenceUntilMissingCopybookArrives()
    {
        var root = Path.Combine(AppContext.BaseDirectory, "test-artifacts", $"partial-scan-{Guid.NewGuid():N}");
        Directory.CreateDirectory(root);
        try
        {
            var database = Path.Combine(root, "scan.db");
            var graph = new RektCopybookGraph();
            graph.AddFile("RECOVERY.cbl", "       COPY ABSENT.", isCopybook: false);
            var planner = new IncrementalScanPlanner(new SqliteRektScanCache(database), graph, "v1-basename");
            var decision = (await planner.PlanAsync(new[] { "RECOVERY.cbl" })).ToParse.Should().ContainSingle().Which;
            decision.MissingCopybooks.Should().Equal("ABSENT");

            await planner.RecordParseAsync(decision, RektParseOutcome.Partial,
                new[] { "missing-copybook:ABSENT: Copybook not found" });

            var persisted = await new SqliteRektScanCache(database).TryGetAsync("RECOVERY.cbl", "v1-basename");
            persisted.Should().NotBeNull();
            persisted!.ParseOutcome.Should().Be(RektParseOutcome.Partial);
            persisted.Confidence.Should().Be(RektScanConfidence.Partial);
            persisted.Warnings.Should().Equal("missing-copybook:ABSENT: Copybook not found");

            graph.AddFile("ABSENT.cpy", "       01 GENUINE-FIELD PIC 9.", isCopybook: true);
            var refreshed = (await planner.PlanAsync(new[] { "RECOVERY.cbl" })).ToParse.Should().ContainSingle().Which;
            refreshed.Reason.Should().Be(ScanReason.DependencyMissingFromCache);
            refreshed.MissingCopybooks.Should().BeEmpty();
        }
        finally
        {
            Directory.Delete(root, recursive: true);
        }
    }

    [Fact]
    public void ConfidenceFromOutcome_StubBackedIsPartial()
    {
        IncrementalScanPlanner.ConfidenceFromOutcome(RektParseOutcome.StubBacked)
            .Should().Be(RektScanConfidence.Partial);
    }
}
