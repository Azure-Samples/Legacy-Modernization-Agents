using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using CobolToQuarkusMigration.Agents.Infrastructure.RektCache;
using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Agents.Infrastructure.Facts;

public sealed class ProgramFactsExtractorTests : IDisposable
{
    private readonly string _root = Path.Combine(
        AppContext.BaseDirectory,
        "test-artifacts",
        $"program-facts-{Guid.NewGuid():N}");

    [Fact]
    public void ResolveConfidence_StubBackedCacheEntryIsPartial()
    {
        var entry = CreateEntry(RektParseOutcome.StubBacked, RektScanConfidence.Partial);

        var confidence = ProgramFactsExtractor.ResolveConfidence(
            entry,
            CreateFullContext(),
            Array.Empty<string>());

        confidence.Should().Be(FactConfidence.Partial);
    }

    [Fact]
    public void ResolveConfidence_GeneratedStubCapsHighConfidenceAtPartial()
    {
        var entry = CreateEntry(RektParseOutcome.Full, RektScanConfidence.High);

        var confidence = ProgramFactsExtractor.ResolveConfidence(
            entry,
            CreateFullContext(),
            new[] { "generated-copybook-stub:MISSING" });

        confidence.Should().Be(FactConfidence.Partial);
    }

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

    [Fact]
    public async Task ExtractAllAsync_PreservesSourceRelativeIdentityAndNestedArtifacts()
    {
        var stagingDir = Path.Combine(_root, "staging");
        var rektDir = Path.Combine(_root, "rekt");
        var outputDir = Path.Combine(_root, "facts");
        Directory.CreateDirectory(Path.Combine(stagingDir, "finance"));
        Directory.CreateDirectory(Path.Combine(stagingDir, "shared"));
        Directory.CreateDirectory(Path.Combine(rektDir, "finance", "SENDER.cbl.report"));

        await File.WriteAllTextAsync(
            Path.Combine(stagingDir, "finance", "SENDER.cbl"),
            """
            IDENTIFICATION DIVISION.
            PROGRAM-ID. SENDER.
            PROCEDURE DIVISION.
                CALL 'RECEIVER'
                GOBACK.
            """);
        await File.WriteAllTextAsync(
            Path.Combine(stagingDir, "shared", "RECEIVER.cbl"),
            """
            IDENTIFICATION DIVISION.
            PROGRAM-ID. RECEIVER.
            PROCEDURE DIVISION.
                GOBACK.
            """);
        await File.WriteAllTextAsync(
            Path.Combine(stagingDir, "finance", "SENDER.cbl.preprocess.json"),
            """
            {
              "transforms": [
                {
                  "rule": "normalize-call",
                  "line": 4,
                  "before": "CALL 'RECEIVER'",
                  "after": "CALL 'RECEIVER'"
                }
              ]
            }
            """);
        await File.WriteAllTextAsync(
            Path.Combine(rektDir, "finance", "SENDER.cbl.report", "SENDER-deps.json"),
            """
            {
              "dependencies": [
                { "name": "RECEIVER.cbl" }
              ]
            }
            """);

        var extractor = new ProgramFactsExtractor(
            repoRoot: _root,
            stagingDir: stagingDir,
            rektDir: rektDir);

        var written = await extractor.ExtractAllAsync(
            new[] { "finance/SENDER.cbl", "shared/RECEIVER.cbl" },
            outputDir);

        written.Should().Be(2);

        var senderFactsPath = Path.Combine(outputDir, "finance", "SENDER.cbl.facts.json");
        var receiverFactsPath = Path.Combine(outputDir, "shared", "RECEIVER.cbl.facts.json");
        File.Exists(senderFactsPath).Should().BeTrue();
        File.Exists(receiverFactsPath).Should().BeTrue();

        var senderFacts = ProgramFactsArtifactLocator.TryLoad(outputDir, "finance/SENDER.cbl");
        var receiverFacts = ProgramFactsArtifactLocator.TryLoad(outputDir, "shared/RECEIVER.cbl");

        senderFacts.Should().NotBeNull();
        receiverFacts.Should().NotBeNull();
        senderFacts!.IdentitySchemeVersion.Should().Be(ProgramFacts.CurrentIdentitySchemeVersion);
        senderFacts.RelativePath.Should().Be("finance/SENDER.cbl");
        senderFacts.PreprocessNotes.Should().ContainSingle();
        senderFacts.Callees.Should().Equal("shared/RECEIVER.cbl");
        receiverFacts!.RelativePath.Should().Be("shared/RECEIVER.cbl");
        receiverFacts.Callers.Should().Equal("finance/SENDER.cbl");
    }

    [Fact]
    public async Task ExtractAllAsync_PropagatesGeneratedStubWarning()
    {
        var stagingDir = Path.Combine(_root, "staging");
        var outputDir = Path.Combine(_root, "facts");
        Directory.CreateDirectory(stagingDir);
        await File.WriteAllTextAsync(
            Path.Combine(stagingDir, "PROGRAM.cbl"),
            "       COPY MISSING.");
        await File.WriteAllTextAsync(
            Path.Combine(stagingDir, ".generated-stubs"),
            "MISSING\n");

        var extractor = new ProgramFactsExtractor(
            repoRoot: _root,
            stagingDir: stagingDir,
            rektDir: Path.Combine(_root, "rekt"));

        await extractor.ExtractAllAsync(new[] { "PROGRAM.cbl" }, outputDir);

        var facts = ProgramFactsArtifactLocator.TryLoad(outputDir, "PROGRAM.cbl");
        facts.Should().NotBeNull();
        facts!.Warnings.Should().Contain("generated-copybook-stub:MISSING");
        facts.Confidence.Should().NotBe(FactConfidence.High);
    }

    public void Dispose()
    {
        if (Directory.Exists(_root))
            Directory.Delete(_root, recursive: true);
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
