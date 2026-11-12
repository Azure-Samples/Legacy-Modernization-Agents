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
        facts.Callees.Should().Equal("shared/RECEIVER.cbl");
        ProgramFactsArtifactLocator.TryLoad(factsRoot, "shared/RECEIVER.cbl").Should().BeNull();
        var caller = new CobolFile
        {
            FileName = "SENDER.cbl", FilePath = callerPath, Content = callerContent
        };
        new DependencyEvidenceGate(factsRoot).RequireVerified([caller], staging)
            .Dependencies.Should().ContainSingle().Which.TargetFile.Should().Be("shared/RECEIVER.cbl");
    }

    [Fact]
    public async Task ExtractAllAsync_AnUnboundRootProgramDoesNotShadowTheHashBoundCallee()
    {
        var staging = Path.Combine(_root, "staging");
        Directory.CreateDirectory(Path.Combine(staging, "shared"));
        var callerContent = "           CALL 'B'.";
        var callerPath = Path.Combine(staging, "A.cbl");
        File.WriteAllText(callerPath, callerContent);
        var boundPath = Path.Combine(staging, "shared", "B.cbl");
        File.WriteAllText(boundPath, "           STOP RUN.");
        DependencyEvidenceFixture.Write(_root, staging, "A.cbl", new DependencyDefinition
        {
            Kind = "CALL", Name = "B.cbl", Path = boundPath, Sha256 = DependencyEvidenceFixture.Hash(boundPath)
        });
        DependencyEvidenceFixture.Write(_root, staging, "shared/B.cbl");
        File.WriteAllText(Path.Combine(staging, "B.cbl"), "           STOP RUN.");
        var factsRoot = Path.Combine(_root, "output", "rekt");

        await new ProgramFactsExtractor(_root, staging)
            .ExtractAllAsync(["A.cbl", "shared/B.cbl", "B.cbl"], factsRoot);

        ProgramFactsArtifactLocator.TryLoad(factsRoot, "A.cbl")!.Callees.Should().Equal("shared/B.cbl");
        ProgramFactsArtifactLocator.TryLoad(factsRoot, "shared/B.cbl")!.Callers.Should().Equal("A.cbl");
        ProgramFactsArtifactLocator.TryLoad(factsRoot, "B.cbl")!.Callers.Should().BeEmpty();
        var caller = new CobolFile { FileName = "A.cbl", FilePath = callerPath, Content = callerContent };
        new DependencyEvidenceGate(factsRoot).RequireVerified([caller], staging)
            .Dependencies.Should().ContainSingle().Which.TargetFile.Should().Be("shared/B.cbl");
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

    [Fact]
    public async Task ExtractAllAsync_DoesNotRecordADynamicCallOperandAsACaller()
    {
        var staging = Path.Combine(_root, "staging");
        var rektDir = Path.Combine(_root, "rekt");
        Directory.CreateDirectory(staging);
        Directory.CreateDirectory(rektDir);
        File.WriteAllText(Path.Combine(staging, "A.cbl"), "           CALL WS-PGM.");
        File.WriteAllText(Path.Combine(staging, "WS-PGM.cbl"), "           STOP RUN.");
        File.WriteAllText(Path.Combine(rektDir, "flow-ast-A.cbl.json"), """
            {"type":"PROCEDURE_DIVISION_BODY","name":"body","children":[
              {"type":"PARAGRAPH","name":"MAIN","children":[
                {"type":"CALL","name":"CALLWS-PGM","originalText":"CALL WS-PGM"}]}]}
            """);
        var output = Path.Combine(_root, "facts");
        await new ProgramFactsExtractor(_root, staging, rektDir).ExtractAllAsync(["A.cbl", "WS-PGM.cbl"], output);

        ProgramFactsArtifactLocator.TryLoad(output, "A.cbl")!.Callees.Should().BeEmpty();
        ProgramFactsArtifactLocator.TryLoad(output, "WS-PGM.cbl")!.Callers.Should().BeEmpty();
    }

    [Fact]
    public void RequireVerified_MatchesAResolvedDynamicCallByItsProvedTarget()
    {
        var staging = Path.Combine(_root, "staging");
        Directory.CreateDirectory(staging);
        var content = "           CALL LIT-DSNTIAC USING SQLCA.";
        var path = Path.Combine(staging, "A.cbl");
        File.WriteAllText(path, content);
        object Entry(params string[] tokens) => new
        {
            rule = "DataDescriptionEntryFormat1Context", children = tokens.Select(token => new { token }).ToArray()
        };
        var parameters = new[] { "eib", "commarea", "sqlca", "message", "lrecl" }
            .Select(name => new { name, passing = "BY REFERENCE", direction = "in", representation = name }).ToArray();
        var definition = new
        {
            kind = "documented-platform-service", platform = "IBM Db2 for z/OS", parameters,
            documentation = new[] { "https://www.ibm.com/docs/en/db2-for-zos/13.0.0?topic=statements-handling-sql-error-codes-in-cobol-applications" }
        };
        var profilePath = Path.Combine(_root, "profile.json");
        File.WriteAllText(profilePath, System.Text.Json.JsonSerializer.Serialize(new
        {
            schemaVersion = 1, profileKind = "target-port", profileName = "test-v1",
            runtimeContracts = new { DSNTIAC = definition }
        }));
        var profileHash = DependencyEvidenceFixture.Hash(profilePath);
        var evidence = DependencyEvidenceFixture.Write(_root, staging, "A.cbl", new DependencyDefinition
        {
            Kind = "RUNTIME_CONTRACT", Name = "DSNTIAC", Path = profilePath, Sha256 = profileHash
        });
        var runtime = new
        {
            dependencyKind = "EXTERNAL_RUNTIME", runtimeContractBindingComplete = true, dependencyAnalysisComplete = false,
            sourcePath = (string?)null, sourceSha256 = (string?)null, dependencies = Array.Empty<object>(),
            callTarget = new { name = "DSNTIAC", programReferenceType = "STATIC" },
            runtimeContract = new
            {
                profileName = "test-v1", profileKind = "target-port", sourcePath = profilePath, sourceSha256 = profileHash,
                name = "DSNTIAC", resolutionKind = "documented-platform-contract", runtimeMapping = "unverified",
                definition,
                targetResolution = new
                {
                    operand = "LIT-DSNTIAC",
                    targetResolution = new
                    {
                        kind = "immutable-working-storage-literal", operand = "LIT-DSNTIAC",
                        definitionSyntax = Entry("05", "LIT-DSNTIAC", "PIC", "X(7)", "VALUE", "'DSNTIAC'", "."),
                        ancestorSyntax = new[] { Entry("01", "WS-LITERALS", ".") },
                        usageProof = "single-native-program:read-only-constant-references"
                    },
                    callSyntax = new
                    {
                        rule = "CallStatementContext",
                        children = new[] { "CALL", "LIT-DSNTIAC", "USING", "SQLCA" }.Select(token => new { token }).ToArray()
                    }
                }
            }
        };
        DependencyEvidenceFixture.BindExport(_root, evidence, System.Text.Json.JsonSerializer.Serialize(new
        {
            callTarget = new { name = "A.cbl" }, dependencies = new[] { runtime }
        }));
        var file = new CobolFile { FileName = "A.cbl", FilePath = path, Content = content };

        new DependencyEvidenceGate(Path.Combine(_root, "output", "rekt")).RequireVerified([file], staging)
            .Dependencies.Should().ContainSingle().Which.TargetFile.Should().Be("DSNTIAC");
    }

    [Theory]
    [InlineData("none")]
    [InlineData("added-participant")]
    [InlineData("changed-participant")]
    [InlineData("changed-inventory")]
    public void RequireVerified_RevalidatesTheCommareaClosureAgainstCurrentEstateParticipants(string mutation)
    {
        var staging = Path.Combine(_root, "staging");
        var factsRoot = Path.Combine(_root, "output", "rekt");
        Directory.CreateDirectory(Path.Combine(staging, "est"));
        var content = "           STOP RUN.";
        var aPath = Path.Combine(staging, "est", "A.cbl");
        var bPath = Path.Combine(staging, "est", "B.cbl");
        File.WriteAllText(aPath, content);
        File.WriteAllText(bPath, content);
        // Python json.dumps(inventory, sort_keys=True) hashes to InventorySha; the gate must reproduce it.
        const string Inventory = """
            {"schema":1,"records":[{"name":"WS-COMM","loaded":true,"layout":["01WS-COMMPIC X(8)"],
              "slots":{"WS-PGM":{"values":["PGMA",null],"copies":[],"entry":false,"unsupported":false}},"issues":[]}],
             "carriers":[],"sends":[{"kind":"record","record":"WS-COMM","command":"LINK","line":12}]}
            """;
        const string InventorySha = "4729fe5620e7386aabdea25c3e5a826de1173c22d3424a4cd8f81d73a8ddef1c";
        var bEvidence = DependencyEvidenceFixture.Write(_root, staging, "est/B.cbl");
        DependencyEvidenceFixture.BindExport(_root, bEvidence,
            $$"""{"callTarget":{"name":"B.cbl"},"dependencies":[],"commareaInventory":{{Inventory}}}""");
        var closurePath = Path.Combine(factsRoot, "est", ".commarea-closure.json");
        File.WriteAllText(closurePath, System.Text.Json.JsonSerializer.Serialize(new
        {
            schema = 1, estate = "est", errors = Array.Empty<string>(),
            participants = new object[]
            {
                new { relativePath = "est/A.cbl", sourceSha256 = DependencyEvidenceFixture.Hash(aPath).ToLowerInvariant(), mode = "lexical" },
                new
                {
                    relativePath = "est/B.cbl", sourceSha256 = DependencyEvidenceFixture.Hash(bPath).ToLowerInvariant(),
                    mode = "inventory", inventorySha256 = InventorySha
                }
            },
            programs = new { }
        }));
        var evidence = DependencyEvidenceFixture.Write(_root, staging, "est/A.cbl", new DependencyDefinition
        {
            Kind = "COMMAREA_CLOSURE", Name = "est", Path = closurePath, Sha256 = DependencyEvidenceFixture.Hash(closurePath)
        });
        DependencyEvidenceFixture.BindExport(_root, evidence, """{"callTarget":{"name":"A.cbl"},"dependencies":[]}""");
        switch (mutation)
        {
            case "added-participant": File.WriteAllText(Path.Combine(staging, "est", "C.cbl"), content); break;
            case "changed-participant": File.WriteAllText(bPath, content + "\n           GOBACK."); break;
            case "changed-inventory":
                File.WriteAllText(bEvidence.ExportPath, """{"callTarget":{"name":"B.cbl"},"dependencies":[],"commareaInventory":{"schema":1,"records":[],"carriers":[],"sends":[]}}""");
                break;
        }
        var file = new CobolFile { FileName = "A.cbl", FilePath = aPath, Content = content };

        var act = () => new DependencyEvidenceGate(factsRoot).RequireVerified([file], staging);

        if (mutation == "none")
            act.Should().NotThrow();
        else
            act.Should().Throw<InvalidDataException>().WithMessage("*est/A.cbl: COMMAREA closure is stale*");
    }

    [Theory]
    [InlineData("""{"callTarget":{"name":"A.cbl"},"dependencies":[{"dependencyKind":"CALL","callTarget":"B"}]}""")]
    [InlineData("""{"callTarget":{"name":"A.cbl"},"dependencies":[{"dependencyKind":"CALL"}]}""")]
    [InlineData("""{"callTarget":{"name":"A.cbl"},"dependencies":[{"dependencyKind":7,"callTarget":{"name":"B"}}]}""")]
    public void RequireVerified_ReportsAMalformedExportAsAProgramIssue(string export)
    {
        var staging = Path.Combine(_root, "staging");
        Directory.CreateDirectory(staging);
        var path = Path.Combine(staging, "A.cbl");
        File.WriteAllText(path, "           STOP RUN.");
        DependencyEvidenceFixture.BindExport(_root, DependencyEvidenceFixture.Write(_root, staging, "A.cbl"), export);
        var file = new CobolFile { FileName = "A.cbl", FilePath = path, Content = "           STOP RUN." };

        var act = () => new DependencyEvidenceGate(Path.Combine(_root, "output", "rekt")).RequireVerified([file], staging);

        act.Should().Throw<InvalidDataException>().WithMessage("*A.cbl: dependency evidence unreadable or invalid*");
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
