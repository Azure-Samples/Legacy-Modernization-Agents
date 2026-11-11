using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using CobolToQuarkusMigration.Agents.Infrastructure.RektCache;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Tests.Helpers;
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
        await File.WriteAllTextAsync(
            Path.Combine(rektDir, "finance", "SENDER.cbl.report", "parse-diagnostics.json"),
            """[{"code":"missing copybook","message":"ABSENT: Copybook not found","severity":"ERROR"}]""");
        var scanCache = new SqliteRektScanCache(Path.Combine(_root, "scan.db"));
        await scanCache.UpsertAsync(new RektScanEntry
        {
            Basename = "SENDER.cbl",
            RelativePath = "finance/SENDER.cbl",
            IdentitySchemeVersion = "v1-basename",
            PreprocessedHash = "hash",
            ParseOutcome = RektParseOutcome.Full,
            Confidence = RektScanConfidence.High,
            ParsedAtUtc = DateTime.UtcNow,
        });

        var extractor = new ProgramFactsExtractor(
            repoRoot: _root,
            stagingDir: stagingDir,
            rektDir: rektDir,
            scanCache: scanCache);

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
        senderFacts.Warnings.Should().Contain("missing-copybook:ABSENT: Copybook not found");
        senderFacts.Confidence.Should().Be(FactConfidence.Partial);
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

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public async Task ExtractAllAsync_FilteredExtractionResolvesCalleesAgainstTheWholeSourceCatalog(
        bool ambiguousCallee)
    {
        var staging = Path.Combine(_root, "staging");
        Directory.CreateDirectory(Path.Combine(staging, "finance"));
        Directory.CreateDirectory(Path.Combine(staging, "shared"));
        var callerPath = Path.Combine(staging, "finance", "SENDER.cbl");
        var callerContent = "       PROGRAM-ID. SENDER.\n           CALL 'RECEIVER'.";
        await File.WriteAllTextAsync(callerPath, callerContent);
        var calleePath = Path.Combine(staging, "shared", "RECEIVER.cbl");
        await File.WriteAllTextAsync(calleePath, "       PROGRAM-ID. RECEIVER.\n           GOBACK.");
        if (ambiguousCallee)
        {
            Directory.CreateDirectory(Path.Combine(staging, "archive"));
            File.Copy(calleePath, Path.Combine(staging, "archive", "RECEIVER.cbl"));
        }
        DependencyEvidenceFixture.Write(_root, staging, "finance/SENDER.cbl", new DependencyDefinition
        {
            Kind = "CALL", Name = "RECEIVER", Path = calleePath,
            Sha256 = DependencyEvidenceFixture.Hash(calleePath)
        });
        var factsRoot = Path.Combine(_root, "output", "rekt");

        var written = await new ProgramFactsExtractor(_root, staging)
            .ExtractAllAsync(["finance/SENDER.cbl"], factsRoot);

        written.Should().Be(1);
        var facts = ProgramFactsArtifactLocator.TryLoad(factsRoot, "finance/SENDER.cbl")!;
        facts.Callees.Should().Equal(ambiguousCallee ? "RECEIVER" : "shared/RECEIVER.cbl");
        ProgramFactsArtifactLocator.TryLoad(factsRoot, "shared/RECEIVER.cbl").Should().BeNull();
        if (!ambiguousCallee)
        {
            var caller = new CobolFile
            {
                FileName = "SENDER.cbl", FilePath = callerPath, Content = callerContent
            };
            new DependencyEvidenceGate(factsRoot).RequireVerified([caller], staging)
                .Dependencies.Should().ContainSingle().Which.TargetFile.Should().Be("shared/RECEIVER.cbl");
        }
    }

    [Fact]
    public async Task ExtractAllAsync_PreservesCallerOriginInsteadOfFlatteningCalledProgramsSubtrees()
    {
        var staging = Path.Combine(_root, "staging");
        Directory.CreateDirectory(staging);
        CobolFile Source(string name, string content)
        {
            var path = Path.Combine(staging, name);
            File.WriteAllText(path, content);
            return new() { FileName = name, FilePath = path, Content = content };
        }
        DependencyDefinition Definition(string kind, CobolFile file) => new()
        {
            Kind = kind, Name = file.FileName, Path = file.FilePath, Sha256 = DependencyEvidenceFixture.Hash(file.FilePath)
        };
        Source("A.cbl", "           CALL 'B'.");
        var b = Source("B.cbl", "           CALL 'C'.\n       COPY Q.");
        var c = Source("C.cbl", "           STOP RUN.");
        var q = Source("Q.cpy", "       01 Q-RECORD PIC X.");
        var evidence = DependencyEvidenceFixture.Write(_root, staging, "A.cbl",
            Definition("CALL", b), Definition("CALL", c), Definition("COPY", q));
        DependencyEvidenceFixture.BindExport(_root, evidence, """
            {"callTarget":{"name":"A.cbl"},"dependencies":[
              {"dependencyKind":"CALL","callTarget":{"name":"B.cbl"},"dependencies":[
                {"dependencyKind":"CALL","callTarget":{"name":"C.cbl"},"dependencies":[]},
                {"dependencyKind":"COPY","callTarget":{"name":"Q.cpy"},"dependencies":[]}]}]}
            """);
        DependencyEvidenceFixture.Write(_root, staging, "B.cbl", Definition("CALL", c), Definition("COPY", q));
        DependencyEvidenceFixture.Write(_root, staging, "C.cbl");
        var output = Path.Combine(_root, "facts");
        await new ProgramFactsExtractor(_root, staging).ExtractAllAsync(["A.cbl", "B.cbl", "C.cbl"], output);

        var aFacts = ProgramFactsArtifactLocator.TryLoad(output, "A.cbl")!;
        var bFacts = ProgramFactsArtifactLocator.TryLoad(output, "B.cbl")!;
        var cFacts = ProgramFactsArtifactLocator.TryLoad(output, "C.cbl")!;
        aFacts.Callees.Should().Equal("B.cbl");
        aFacts.Data.CopybooksUsed.Should().BeEmpty();
        bFacts.Callees.Should().Equal("C.cbl");
        bFacts.Data.CopybooksUsed.Should().Equal("Q.cpy");
        bFacts.Callers.Should().Equal("A.cbl");
        cFacts.Callers.Should().Equal("B.cbl");
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
