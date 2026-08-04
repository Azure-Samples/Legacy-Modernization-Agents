using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using CobolToQuarkusMigration.Agents.Infrastructure.RektCache;
using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Agents.Infrastructure.Facts;

public class ProgramFactsExtractorTests
{
    [Fact]
    public void ResolveConfidence_FailedCacheEntryPreservesNone()
    {
        var entry = CreateEntry(RektParseOutcome.Failed, RektScanConfidence.None);
        var context = CreateFullContext();

        var confidence = ProgramFactsExtractor.ResolveConfidence(
            entry,
            context,
            Array.Empty<string>());

        confidence.Should().Be(FactConfidence.None);
    }

    [Fact]
    public void ResolveConfidence_NoCacheEntryInfersFromArtifacts()
    {
        var confidence = ProgramFactsExtractor.ResolveConfidence(
            cacheEntry: null,
            CreateFullContext(),
            Array.Empty<string>());

        confidence.Should().Be(FactConfidence.High);
    }

    private static RektScanEntry CreateEntry(
        RektParseOutcome outcome,
        RektScanConfidence confidence) =>
        new()
        {
            Basename = "TEST.cbl",
            IdentitySchemeVersion = ProgramFacts.CurrentIdentitySchemeVersion,
            PreprocessedHash = "hash",
            ParseOutcome = outcome,
            Confidence = confidence,
            ParsedAtUtc = DateTime.UtcNow,
        };

    private static RektContext CreateFullContext()
    {
        var context = new RektContext();
        context.Sections.Add(new RektSection { Name = "MAIN" });
        context.DataStructure.Add(new RektDataItem { Name = "ACCOUNT", Level = 1 });
        return context;
    }
}
