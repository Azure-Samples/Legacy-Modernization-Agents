using CobolToQuarkusMigration.Agents.Infrastructure.RektCache;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Agents.Infrastructure.RektCache;

public sealed class IncrementalScanPlannerTests
{
    [Fact]
    public void ConfidenceFromOutcome_StubBackedIsPartial()
    {
        IncrementalScanPlanner.ConfidenceFromOutcome(RektParseOutcome.StubBacked)
            .Should().Be(RektScanConfidence.Partial);
    }
}
