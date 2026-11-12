using System.Text.Json;
using System.Text.Json.Nodes;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Tests.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Agents.Infrastructure.Facts;

public sealed class DependencyEvidenceGateTests : IDisposable
{
    private readonly string _root = Path.GetFullPath(Path.Combine(".test-artifacts", $"dependency-gate-{Guid.NewGuid():N}"));
    private string SourceRoot => Path.Combine(_root, "source");
    private string FactsRoot => Path.Combine(_root, "output", "rekt");
    private DependencyEvidenceGate Gate => new(FactsRoot);

    [Theory]
    [InlineData("")]
    [InlineData("profile")]
    [InlineData("projection")]
    [InlineData("aggregate")]
    [InlineData("omitted-use")]
    [InlineData("symbol-comparison")]
    [InlineData("business-copy")]
    [InlineData("move-masquerade")]
    [InlineData("source-text")]
    [InlineData("and-conjunct")]
    [InlineData("and-conjunct-whole-condition")]
    [InlineData("evaluate")]
    [InlineData("evaluate-when-first")]
    [InlineData("container-path")]
    [InlineData("container-path-escape")]
    [InlineData("copybook")]
    [InlineData("copybook-replacing")]
    [InlineData("copybook-hash")]
    [InlineData("copybook-omitted-use")]
    public void SymbolicPlatformReplacementIsNotVendorStorageAndCannotHideUnsupportedUses(string damage)
    {
        var source = "       COPY DFHAID.\n           IF EIBAID = DFHENTER\n               CONTINUE\n           END-IF.\n";
        if (damage.StartsWith("and-conjunct", StringComparison.Ordinal))
            source = source.Replace("IF EIBAID = DFHENTER\n", "IF EIBAID = DFHENTER AND FLAG = 'Y'\n");
        if (damage.StartsWith("evaluate", StringComparison.Ordinal))
            source = "       COPY DFHAID.\n           EVALUATE EIBAID\n               WHEN DFHENTER\n               CONTINUE\n" +
                "               WHEN DFHPF3\n               CONTINUE\n           END-EVALUATE.\n";
        if (damage == "aggregate") source += "           IF EIBAID = DFHAID CONTINUE END-IF.\n";
        if (damage == "omitted-use") source += "           MOVE DFHPF3 TO RESULT.\n";
        if (damage == "business-copy") source += "       COPY BUSINESS.\n";
        if (damage == "move-masquerade")
            source = "       COPY DFHAID.\n           MOVE EIBAID TO DFHENTER.\n";
        var viaCopybook = damage.StartsWith("copybook", StringComparison.Ordinal);
        if (viaCopybook)
            source = "       COPY DFHAID.\n       PROCEDURE DIVISION.\n" + (damage == "copybook-replacing"
                ? "           COPY KEYS REPLACING ==(SEL)== BY ==EIBA==\n           .\n"
                : "           COPY 'KEYS'\n           .\n");
        var keys = Source("KEYS.cpy", (damage == "copybook-replacing" ? "           IF (SEL)ID = DFHENTER\n" : "           IF EIBAID = DFHENTER\n") +
            "               CONTINUE\n           END-IF.\n" + (damage == "copybook-omitted-use" ? "           MOVE DFHPF3 TO RESULT.\n" : ""));
        var file = Source("MAIN.cbl", source);
        var symbols = new[] { ("DFHENTER", "ENTER"), ("DFHCLEAR", "CLEAR") }
            .Concat(Enumerable.Range(1, 3).Select(index => ($"DFHPA{index}", $"PA{index}")))
            .Concat(Enumerable.Range(1, 24).Select(index => ($"DFHPF{index}", $"PF{index}")))
            .ToDictionary(pair => pair.Item1, pair => pair.Item2);
        var recipe = new
        {
            recipe = "symbolic-terminal-events-v1", selector = "EIBAID", symbols,
            documentation = new[] { "https://www.ibm.com/docs/en/cics-ts/6.x" },
            targetSemantics = "named-events-preserve-source-branches"
        };
        var profile = Source("platform.json", JsonSerializer.Serialize(new
        {
            schemaVersion = 1, profileKind = "platform-replacement", profileName = "test-events",
            copybooks = new { DFHAID = recipe }
        }));
        var projection = Source(".preprocessed/DFHAID.cpy",
            "      * ANALYSIS-ONLY PLATFORM EVENT CONTRACT; NOT A VENDOR COPYBOOK\n" +
            "      * No VALUE clauses: these names are symbolic, not blank data.\n" +
            "       01 DFHAID.\n" +
            string.Concat(symbols.Keys.Order(StringComparer.Ordinal).Select(name => $"          02 {name} PIC X.\n")));
        var evidence = DependencyEvidenceFixture.Write(_root, SourceRoot, "MAIN.cbl",
            new[]
            {
                new DependencyDefinition { Kind = "PLATFORM_COPY", Name = "DFHAID", Path = profile.FilePath, Sha256 = DependencyEvidenceFixture.Hash(profile.FilePath) },
                new DependencyDefinition { Kind = "PLATFORM_INPUT", Name = "DFHAID", Path = projection.FilePath, Sha256 = DependencyEvidenceFixture.Hash(projection.FilePath) },
                new DependencyDefinition { Kind = "COPY", Name = "KEYS.cpy", Path = keys.FilePath, Sha256 = DependencyEvidenceFixture.Hash(keys.FilePath) }
            }.Where(definition => viaCopybook || definition.Kind != "COPY").ToArray());
        var leaf = JsonSerializer.SerializeToNode(new
        {
            callTarget = new { name = "DFHAID", programReferenceType = "STATIC" },
            dependencyKind = "PLATFORM_COPY", dependencyAnalysisComplete = false, runtimeContractBindingComplete = true,
            dependencies = Array.Empty<object>(),
            runtimeContract = new
            {
                name = "DFHAID", profileKind = "platform-replacement", profileName = "test-events",
                sourcePath = profile.FilePath, sourceSha256 = DependencyEvidenceFixture.Hash(profile.FilePath),
                outputPath = "/source/.rekt-staging/DFHAID.cpy", outputSha256 = DependencyEvidenceFixture.Hash(projection.FilePath),
                definition = recipe, resolutionKind = "symbolic-platform-replacement",
                runtimeMapping = "implementation-required", sourceValues = "not-supplied", bindingStatus = "complete",
                usages = new[]
                {
                    new
                    {
                        grammarRule = "ConditionContext", sourceText = "EIBAID = DFHENTER", line = 2,
                        coordinateSpace = "expanded-parser-input",
                        syntax = new { rule = "ConditionContext", children = new[] {
                            new { rule = "SimpleConditionContext", children = new[] {
                                new { token = damage == "symbol-comparison" ? "DFHPF1" : "EIBAID" },
                                new { token = "=" }, new { token = "DFHENTER" } } } } }
                    }
                }
            }
        })!;
        if (damage == "profile") File.AppendAllText(profile.FilePath, " ");
        if (damage == "copybook-hash") File.AppendAllText(keys.FilePath, "           MOVE DFHPF3 TO RESULT.\n");
        if (damage == "projection") File.AppendAllText(projection.FilePath, "       02 FAKE PIC X.\n");
        if (damage == "source-text") leaf["runtimeContract"]!["usages"]![0]!["sourceText"] = "DFHENTER = EIBAID";
        // The parser names a per-estate snapshot by its container path; /source is the program's source root.
        if (damage == "container-path")
            leaf["runtimeContract"]!["sourcePath"] = "/source/" + Path.GetRelativePath(SourceRoot, profile.FilePath).Replace('\\', '/');
        if (damage == "container-path-escape")
            leaf["runtimeContract"]!["sourcePath"] = "/source/../" + Path.GetFileName(profile.FilePath);
        if (damage.StartsWith("and-conjunct", StringComparison.Ordinal))
        {
            var usage = leaf["runtimeContract"]!["usages"]![0]!;
            usage["grammarRule"] = "SimpleConditionContext";
            usage["conditionRole"] = "application-and-conjunct";
            if (damage == "and-conjunct") usage["syntax"] = usage["syntax"]!["children"]![0]!.DeepClone();
        }
        if (damage.StartsWith("evaluate", StringComparison.Ordinal))
        {
            JsonNode Usage(string role, string rule, string text, int line) => JsonSerializer.SerializeToNode(new
            {
                grammarRule = rule, conditionRole = role, sourceText = text, line, column = 12,
                coordinateSpace = "expanded-parser-input",
                syntax = new { rule, children = text.Split(' ').Select(token => new { token }).ToArray() }
            })!;
            var subject = Usage("evaluate-subject", "EvaluateSelectContext", "EIBAID", 2);
            var whens = new[] { Usage("evaluate-when", "EvaluateWhenContext", "WHEN DFHENTER", 3),
                Usage("evaluate-when", "EvaluateWhenContext", "WHEN DFHPF3", 5) };
            leaf["runtimeContract"]!["usages"] = damage == "evaluate"
                ? new JsonArray(subject, whens[0], whens[1]) : new JsonArray(whens[0], subject, whens[1]);
        }
        DependencyEvidenceFixture.BindExport(_root, evidence, JsonSerializer.Serialize(new
        {
            callTarget = new { name = "MAIN.cbl" },
            dependencies = viaCopybook ? new JsonNode[] { leaf, JsonSerializer.SerializeToNode(new { name = "KEYS.cpy" })! } : [leaf]
        }));
        var verify = () => Gate.RequireVerified([file], SourceRoot);
        if (damage.Length > 0 && damage is not ("and-conjunct" or "evaluate" or "container-path" or "copybook" or "copybook-replacing"))
        {
            verify.Should().Throw<InvalidDataException>();
            return;
        }
        var map = verify();
        if (viaCopybook)
        {
            map.RuntimeRequirements.Should().ContainSingle().Which.Kind.Should().Be("PLATFORM_COPY");
            return;
        }
        map.CopybookUsage["MAIN.cbl"].Should().BeEmpty();
        map.Dependencies.Should().BeEmpty();
        map.RuntimeRequirements.Should().ContainSingle().Which.Kind.Should().Be("PLATFORM_COPY");
        var prompt = CobolToQuarkusMigration.Helpers.VerifiedRuntimeContext.Build(FactsRoot, SourceRoot, "MAIN.cbl");
        prompt.Should().Contain("never allocate or blank-initialize").And.Contain("EIBAID");
        File.WriteAllText(Path.Combine(FactsRoot, "MAIN.cbl.report", "flow-data-MAIN.cbl.json"),
            JsonSerializer.Serialize(new[] { new { name = "DFHAID", level = 1 }, new { name = "ACCOUNT", level = 1 } }));
        var context = new CobolToQuarkusMigration.Helpers.RektContextLoader(_root, FactsRoot).Load("MAIN.cbl", SourceRoot);
        context.DataStructure.Should().ContainSingle().Which.Name.Should().Be("ACCOUNT");
        context.CopybookUsage.Should().BeEmpty();
        context.CallTargets.Should().BeEmpty();
        var reused = () => Gate.RequireVerified([file], SourceRoot, new DependencyMap());
        reused.Should().Throw<InvalidDataException>();
    }

    [Fact]
    public void LegacyHighEmptyCopyIsUnknownButFreshExplicitCompleteZeroCopyIsValid()
    {
        var file = Source("MAIN.cbl", "       PROCEDURE DIVISION.\n           STOP RUN.");
        Directory.CreateDirectory(FactsRoot);
        File.WriteAllText(Path.Combine(FactsRoot, "MAIN.cbl.facts.json"), JsonSerializer.Serialize(new ProgramFacts
        {
            Basename = "MAIN.cbl", Stem = "MAIN", SourceHash = DependencyEvidenceFixture.Hash(file.FilePath),
            Confidence = FactConfidence.High, Summary = new ProgramSummary(), Data = new DataFacts()
        }));
        var verify = () => Gate.RequireVerified([file], SourceRoot);
        verify.Should().Throw<InvalidDataException>().WithMessage("*legacy High confidence*");

        var evidence = DependencyEvidenceFixture.Write(_root, SourceRoot, "MAIN.cbl");
        var staleFacts = () => Gate.RequireVerified([file], SourceRoot);
        staleFacts.Should().Throw<InvalidDataException>().WithMessage("*existing program facts are stale*");
        var factsPath = Path.Combine(FactsRoot, "MAIN.cbl.facts.json");
        var facts = JsonSerializer.Deserialize<ProgramFacts>(File.ReadAllText(factsPath))!;
        File.WriteAllText(factsPath, JsonSerializer.Serialize(facts with { DependencyEvidence = evidence }));
        var map = Gate.RequireVerified([file], SourceRoot);
        map.Dependencies.Should().BeEmpty();
        map.CopybookUsage.Should().ContainKey("MAIN.cbl").WhoseValue.Should().BeEmpty();
        map.EvidenceDiagnostics.Single().Should().Contain("local REKT disk exports").And.Contain("Neo4j was not queried");
    }

    [Theory]
    [InlineData("source", "original source missing or SHA256 mismatch")]
    [InlineData("definition", "COPY definition BOOK.cpy missing or SHA256 mismatch")]
    [InlineData("export", "dependency export missing or SHA256 mismatch")]
    [InlineData("stub", "placeholder/stub-backed")]
    [InlineData("coverage", "COPY coverage is missing")]
    [InlineData("dynamic", "unresolved dependency: dynamic CALL WS-TARGET")]
    [InlineData("producer", "unsupported dependency evidence schema or producer")]
    [InlineData("contamination", "placeholder/stub contamination")]
    public void StaleMissingAndPlaceholderEvidenceCannotAuthorizeConversion(string damage, string diagnostic)
    {
        var file = Source("MAIN.cbl", "       COPY BOOK.");
        var book = Source("BOOK.cpy", "       01 BOOK-RECORD PIC X(4).");
        var evidence = DependencyEvidenceFixture.Write(_root, SourceRoot, "MAIN.cbl",
            new DependencyDefinition
            {
                Kind = "COPY", Name = "BOOK.cpy", Path = book.FilePath,
                Sha256 = DependencyEvidenceFixture.Hash(book.FilePath)
            });
        switch (damage)
        {
            case "source": File.AppendAllText(file.FilePath, "\n           DISPLAY 'changed'."); break;
            case "definition": File.AppendAllText(book.FilePath, "\n       01 EXTRA PIC X."); break;
            case "export": File.AppendAllText(evidence.ExportPath, " "); break;
            case "stub":
                File.WriteAllText(book.FilePath, "       01 BOOK-STUB PIC X.");
                evidence = evidence with
                {
                    Definitions = [evidence.Definitions[0] with { Sha256 = DependencyEvidenceFixture.Hash(book.FilePath) }]
                };
                break;
            case "coverage": evidence.Coverage.Remove("COPY"); break;
            case "dynamic": evidence.UnresolvedDiagnostics.Add("dynamic CALL WS-TARGET"); break;
            case "producer": evidence = evidence with { Producer = "regex-scanner" }; break;
            case "contamination": evidence = evidence with { PlaceholderContaminated = true }; break;
        }
        DependencyEvidenceFixture.Save(_root, evidence);
        var verify = () => Gate.RequireVerified([file], SourceRoot);
        verify.Should().Throw<InvalidDataException>().WithMessage($"*{diagnostic}*");
    }

    [Fact]
    public void StrictRunGetsTheLayoutsOfBoundCopiesThatNoConvertedFileDeclares()
    {
        var file = Source("MAIN.cbl", "       COPY BOOK.\n       COPY MAP.");
        var book = Source("BOOK.cpy", "       01 BOOK-RECORD PIC X(4).");
        var map = Source("MAP.cpy", "       01 MAP-INPUT PIC X(7).");
        DependencyEvidenceFixture.Write(_root, SourceRoot, "MAIN.cbl",
            new DependencyDefinition { Kind = "COPY", Name = "BOOK.cpy", Path = book.FilePath, Sha256 = DependencyEvidenceFixture.Hash(book.FilePath) },
            new DependencyDefinition { Kind = "COPY", Name = "MAP.cpy", Path = map.FilePath, Sha256 = DependencyEvidenceFixture.Hash(map.FilePath) });

        CobolToQuarkusMigration.Helpers.VerifiedCopybookContext.BuildUnconverted(FactsRoot, SourceRoot, "MAIN.cbl").Should().BeEmpty();
        CobolToQuarkusMigration.Helpers.ConversionSourceScope.Set([file, book]);
        try
        {
            var context = CobolToQuarkusMigration.Helpers.VerifiedCopybookContext.BuildUnconverted(FactsRoot, SourceRoot, "MAIN.cbl");
            context.Should().Contain("MAP-INPUT PIC X(7)").And.NotContain("BOOK-RECORD");
        }
        finally
        {
            CobolToQuarkusMigration.Helpers.ConversionSourceScope.Clear();
        }
    }

    [Fact]
    public void SameNamedCopybookAtAnotherPathIsExcludedButUnrelatedUnboundCopybookStaysForTheGate()
    {
        var file = Source("app/MAIN.cbl", "       COPY BOOK.");
        var book = Source("app/copy/BOOK.cpy", "       01 BOOK-RECORD PIC X(4).");
        var duplicate = Source("other/BOOK.cpy", "       01 OTHER-RECORD PIC X(9).");
        var unrelated = Source("app/copy/LONE.cpy", "       01 LONE-RECORD PIC X.");
        DependencyEvidenceFixture.Write(_root, SourceRoot, "app/MAIN.cbl",
            new DependencyDefinition
            {
                Kind = "COPY", Name = "BOOK", Path = book.FilePath,
                Sha256 = DependencyEvidenceFixture.Hash(book.FilePath)
            });
        var excluded = new List<string>();

        var kept = Gate.ExcludeSupersededCopybooks([file, book, duplicate, unrelated], SourceRoot, excluded);

        kept.Should().Equal(file, book, unrelated);
        excluded.Should().Equal("other/BOOK.cpy");
    }

    [Fact]
    public void NullExportHashRemainsAnExplicitUnboundArtifactNotANullReferenceFailure()
    {
        var file = Source("MAIN.cbl", "       PROCEDURE DIVISION.\n           STOP RUN.");
        DependencyEvidenceFixture.Write(_root, SourceRoot, "MAIN.cbl");
        var path = DependencyEvidenceGate.GetEvidencePath(FactsRoot, "MAIN.cbl");
        var document = JsonNode.Parse(File.ReadAllText(path))!;
        document["exportSha256"] = null;
        File.WriteAllText(path, document.ToJsonString());
        var verify = () => Gate.RequireVerified([file], SourceRoot);
        verify.Should().Throw<InvalidDataException>()
            .WithMessage("*dependency export missing or SHA256 mismatch*");
    }

    [Fact]
    public void SourceRelativeEvidenceDoesNotLeakAcrossDuplicateBasenamesOrCachedRequests()
    {
        var billing = Source("billing/MAIN.cbl", "       STOP RUN.");
        var claims = Source("claims/MAIN.cbl", "       STOP RUN.");
        DependencyEvidenceFixture.Write(_root, SourceRoot, "billing/MAIN.cbl");
        var verify = () => Gate.RequireVerified([billing, claims], SourceRoot);
        verify.Should().Throw<InvalidDataException>().WithMessage("*claims/MAIN.cbl: missing*");
        DependencyEvidenceFixture.Write(_root, SourceRoot, "claims/MAIN.cbl");
        Gate.RequireVerified([billing, claims], SourceRoot).CopybookUsage.Keys.Should()
            .BeEquivalentTo("billing/MAIN.cbl", "claims/MAIN.cbl");
        File.AppendAllText(claims.FilePath, "\n           DISPLAY 'changed'.");
        verify.Should().Throw<InvalidDataException>().WithMessage("*claims/MAIN.cbl: original source missing or SHA256 mismatch*");
    }

    [Fact]
    public void ReusedMapCannotAuthorizeInventedEdgesOrSkipFreshness()
    {
        var file = Source("MAIN.cbl", "       STOP RUN.");
        DependencyEvidenceFixture.Write(_root, SourceRoot, "MAIN.cbl");
        var map = Gate.RequireVerified([file], SourceRoot);
        map.Dependencies.Add(new() { SourceFile = "MAIN.cbl", TargetFile = "INVENTED.cpy", DependencyType = "COPY" });
        var reuse = () => Gate.RequireVerified([file], SourceRoot, map);
        reuse.Should().Throw<InvalidDataException>().WithMessage("*Reused dependency map differs*");
        map.Dependencies.Clear();
        File.AppendAllText(file.FilePath, "\n           DISPLAY 'changed'.");
        reuse.Should().Throw<InvalidDataException>().WithMessage("*original source missing or SHA256 mismatch*");
    }

    [Fact]
    public void ClaimedCompleteEmptyExportContradictedByCopySourceIsRejectedNotFilled()
    {
        var file = Source("MAIN.cbl", "       COPY REQUIRED.");
        DependencyEvidenceFixture.Write(_root, SourceRoot, "MAIN.cbl");
        var verify = () => Gate.RequireVerified([file], SourceRoot);
        verify.Should().Throw<InvalidDataException>().WithMessage("*source COPY REQUIRED contradicts*no scanner edge substituted*");
    }

    [Theory]
    [InlineData("           DISPLAY \"CALL WS-TARGET\".\n           DISPLAY \"COPY REQUIRED\".\n           DISPLAY \"EXEC CICS\".")]
    [InlineData("           DISPLAY 'CALL ''REQUIRED'' COPY \"BOOK\" EXEC SQL READ FILE'.")]
    [InlineData("           DISPLAY \"*> is literal, not a comment\".\n      * CALL WS-TARGET COPY REQUIRED\n           STOP RUN. *> EXEC CICS")]
    [InlineData("           DISPLAY \"CALL WS-\n      * COPY REQUIRED\n      -    \"TARGET COPY REQUIRED EXEC CICS\".")]
    [InlineData("DISPLAY \"CALL WS-TARGET\" *> COPY REQUIRED\n*> CALL WS-OTHER\nDISPLAY \"EXEC SQL\".")]
    [InlineData("           DISPLAY 'DFHVALUE(ABEND) DFHRESP(NORMAL)'.")]
    public void DependencyWordsInDisplayLiteralsAndCommentsDoNotContradictCompleteEmptyEvidence(string content)
    {
        var file = Source("MAIN.cbl", content);
        DependencyEvidenceFixture.Write(_root, SourceRoot, "MAIN.cbl");
        Gate.RequireVerified([file], SourceRoot).Dependencies.Should().BeEmpty();
    }

    [Theory]
    [InlineData("", "", true)]
    [InlineData("", "           CALL 'MISSING'.", false)]
    [InlineData("       SOURCE-COMPUTER. MAINFRAME WITH DEBUGGING MODE.\n", "", false)]
    public void InactiveDebugLineCannotCorruptLiteralsOrHideRealStatementsButActiveDebugModeStaysBlocked(
        string directive, string statement, bool accepted)
    {
        var file = Source("MAIN.cbl", directive
            + "      D    DISPLAY 'unterminated ignored debug text\n"
            + "           DISPLAY 'Close'.\n" + statement);
        DependencyEvidenceFixture.Write(_root, SourceRoot, "MAIN.cbl");
        if (accepted)
            Gate.RequireVerified([file], SourceRoot).Dependencies.Should().BeEmpty();
        else
        {
            var verify = () => Gate.RequireVerified([file], SourceRoot);
            verify.Should().Throw<InvalidDataException>().WithMessage(
                directive.Length > 0 ? "*WITH DEBUGGING MODE*" : "*source CALL MISSING*");
        }
    }

    [Theory]
    [InlineData("       PROCEDURE DIVISION.")]
    [InlineData("       PROCEDURE\n       DIVISION.")]
    public void DocumentaryApostrophesCannotHideAnUnrecordedCallInTheProcedureDivision(string header)
    {
        var file = Source("MAIN.cbl",
            "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. MAIN.\n"
            + "       AUTHOR. Example O'Name.\n"
            + "           A documentary entry mentions CALL 'NOT-A-CALL'.\n"
            + header + "\n           CALL 'MISSING'.");
        DependencyEvidenceFixture.Write(_root, SourceRoot, "MAIN.cbl");
        var verify = () => Gate.RequireVerified([file], SourceRoot);
        verify.Should().Throw<InvalidDataException>().WithMessage("*source CALL MISSING*");
    }

    [Theory]
    [InlineData("           DISPLAY \"*>\".\n           CALL 'REQUIRED'.", "source CALL REQUIRED contradicts")]
    [InlineData("           CALL\n      * Ignore COPY NOT-A-DEPENDENCY here\n      -    'REQUIRED'.", "source CALL REQUIRED contradicts")]
    [InlineData("           CALL 'REQU\n      -    'IRED'.", "source CALL REQUIRED contradicts")]
    [InlineData("           DISPLAY \"COPY IGNORED\".\n           COPY \"REQUIRED\".", "source COPY REQUIRED contradicts")]
    [InlineData("           DISPLAY \"CALL 'IGNORED'\".\n           CALL WS-TARGET.", "dynamic CALL target remains unresolved")]
    [InlineData("           DISPLAY \"EXEC CICS\".\n           EXEC CICS RETURN END-EXEC.", "EXEC_CICS source evidence")]
    [InlineData("           DISPLAY \"EXEC SQL\".\n           EXEC SQL SELECT X END-EXEC.", "EXEC_SQL source evidence")]
    [InlineData("           DISPLAY \"READ FILE\".\n           READ INPUT-FILE.", "FILE_IO source evidence")]
    [InlineData("           IF STATUS = DFHVALUE(ABEND) CONTINUE END-IF.", "source expression DFHVALUE")]
    [InlineData("           IF STATUS = DFHRESP(NORMAL) CONTINUE END-IF.", "source expression DFHRESP")]
    public void ActualStatementsStillContradictEmptyEvidenceAfterLiteralsAndContinuations(string content, string diagnostic)
    {
        var file = Source("MAIN.cbl", content);
        DependencyEvidenceFixture.Write(_root, SourceRoot, "MAIN.cbl");
        var verify = () => Gate.RequireVerified([file], SourceRoot);
        verify.Should().Throw<InvalidDataException>().WithMessage($"*{diagnostic}*");
    }

    [Theory]
    [InlineData("EXEC_CICS", "RETURN", "unverified")]
    [InlineData("EXEC_CICS", "RETURN", "verified")]
    [InlineData("EXEC_SQL", "SELECT", "unverified")]
    [InlineData("FILE_IO", "READ", "unverified")]
    public void CompleteNativeDialectDependenciesBecomeImplementationRequirementsBeforeGeneration(
        string kind, string operation, string runtimeMapping)
    {
        var file = Source("MAIN.cbl",
            "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. MAIN.\n" +
            "       AUTHOR. Example O'Name.\n       PROCEDURE DIVISION.\n" +
            (kind == "FILE_IO" ? $"           {operation} INPUT-FILE."
                : $"           EXEC {(kind == "EXEC_CICS" ? "CICS" : "SQL")} {operation} END-EXEC."));
        var evidence = DependencyEvidenceFixture.Write(_root, SourceRoot, "MAIN.cbl");
        DependencyEvidenceFixture.BindExport(_root, evidence, JsonSerializer.Serialize(new
        {
            callTarget = new { name = "MAIN.cbl" },
            dependencies = Array.Empty<object>(),
            dependencyDialectStatements = new[] { DependencyEvidenceFixture.DialectStatement(kind, operation, runtimeMapping) }
        }));
        var map = Gate.RequireVerified([file], SourceRoot);
        var requirement = map.RuntimeRequirements.Should().ContainSingle().Subject;
        requirement.Kind.Should().Be(kind);
        requirement.Operation.Should().Be(operation);
        requirement.Evidence.GetProperty("runtimeMapping").GetString().Should().Be(runtimeMapping);
        var prompt = CobolToQuarkusMigration.Helpers.VerifiedRuntimeContext.Build(FactsRoot, SourceRoot, "MAIN.cbl");
        prompt.Should().Contain("must IMPLEMENT").And.Contain(operation)
            .And.Contain("behavioral equivalence requires separate evidence");
        prompt.Should().Contain($"\"syntax\":\"{operation}\"").And.NotContain("\"rule\":");
        requirement.Evidence.GetProperty("syntax").GetProperty("rule").GetString().Should().NotBeNull();
        map.RuntimeRequirements.Clear();
        var reuse = () => Gate.RequireVerified([file], SourceRoot, map);
        reuse.Should().Throw<InvalidDataException>().WithMessage("*Reused dependency map differs*");
    }

    [Theory]
    [InlineData("none")]
    [InlineData("reset")]
    [InlineData("source-text")]
    [InlineData("invented-label")]
    public void TerminalCancelRequiresItsOwnNativeControlEvidenceRatherThanALabel(string damage)
    {
        var file = Source("MAIN.cbl", "           EXEC CICS HANDLE ABEND CANCEL END-EXEC.");
        var evidence = DependencyEvidenceFixture.Write(_root, SourceRoot, "MAIN.cbl");
        var statement = new
        {
            kind = "EXEC_CICS",
            operation = "HANDLE",
            grammarRule = "Cics_handleContext",
            sourceText = damage == "source-text" ? "HANDLE ABEND LABEL(FAKE)" : "HANDLE ABEND CANCEL",
            bindingStatus = "complete",
            bindingBasis = "parsed-cics-control-binding",
            resourceReferences = Array.Empty<object>(),
            resourceBindings = damage == "invented-label" ? new object[] { new { kind = "CICS_LABEL", name = "FAKE" } } : [],
            syntax = new
            {
                rule = "Cics_handleContext",
                children = new object[]
                {
                    new { token = "HANDLE" },
                    new { rule = "Cics_handle_abendContext", children = new[]
                        { new { token = "ABEND" }, new { token = damage == "reset" ? "RESET" : "CANCEL" } } }
                }
            }
        };
        DependencyEvidenceFixture.BindExport(_root, evidence, JsonSerializer.Serialize(new
        {
            callTarget = new { name = "MAIN.cbl" },
            dependencies = Array.Empty<object>(),
            dependencyDialectStatements = new[] { statement }
        }));
        var verify = () => Gate.RequireVerified([file], SourceRoot);
        if (damage == "none")
            verify().RuntimeRequirements.Should().ContainSingle(requirement => requirement.Operation == "HANDLE");
        else
            verify.Should().Throw<InvalidDataException>().WithMessage("*terminal*");
    }

    [Fact]
    public void AHashBoundCicsFileDefinitionAllowsWriteGenerationButStillRequiresTheUnchangedDefinition()
    {
        var file = Source("MAIN.cbl", "           EXEC CICS WRITE FILE('TESTFILE') END-EXEC.");
        var catalog = Source("resources.yaml",
            "resourceDefinitions:\n  - file:\n      name: TESTFILE\n      dsname: TEST.DATA\n");
        var evidence = DependencyEvidenceFixture.Write(_root, SourceRoot, "MAIN.cbl", new DependencyDefinition
        {
            Kind = "CICS_FILE",
            Name = "TESTFILE",
            Path = catalog.FilePath,
            Sha256 = DependencyEvidenceFixture.Hash(catalog.FilePath)
        });
        DependencyEvidenceFixture.BindExport(_root, evidence, JsonSerializer.Serialize(new
        {
            callTarget = new { name = "MAIN.cbl" },
            dependencies = Array.Empty<object>(),
            dependencyDialectStatements = new[] { DependencyEvidenceFixture.DialectStatement("EXEC_CICS", "WRITE") }
        }));
        Gate.RequireVerified([file], SourceRoot).RuntimeRequirements.Should().ContainSingle()
            .Which.Operation.Should().Be("WRITE");
        var verify = () => Gate.RequireVerified([file], SourceRoot);
        File.WriteAllText(catalog.FilePath, "changed");
        verify.Should().Throw<InvalidDataException>().WithMessage("*CICS_FILE definition TESTFILE*SHA256 mismatch*");
    }

    [Theory]
    [InlineData("SQLCA", true)]
    [InlineData("MISSING", false)]
    public void SqlIncludeRequiresItsActualBoundCopyDefinitionNotAnExecutableSqlStatement(string include, bool accepted)
    {
        var file = Source("MAIN.cbl", $"           EXEC SQL INCLUDE {include} END-EXEC.");
        var copy = Source("SQLCA.cpy", "       01 SQLCA.\n          05 SQLCODE PIC S9(9) COMP.");
        var evidence = DependencyEvidenceFixture.Write(_root, SourceRoot, "MAIN.cbl", new DependencyDefinition
        {
            Kind = "COPY", Name = "SQLCA", Path = copy.FilePath,
            Sha256 = DependencyEvidenceFixture.Hash(copy.FilePath)
        });
        DependencyEvidenceFixture.BindExport(_root, evidence, JsonSerializer.Serialize(new
        {
            callTarget = new { name = "MAIN.cbl" },
            dependencies = new[] { new { dependencyKind = "COPY", callTarget = new { name = "SQLCA" }, dependencies = Array.Empty<object>() } }
        }));
        if (accepted)
            Gate.RequireVerified([file], SourceRoot).CopybookUsage["MAIN.cbl"].Should().Contain("SQLCA");
        else
        {
            var verify = () => Gate.RequireVerified([file], SourceRoot);
            verify.Should().Throw<InvalidDataException>().WithMessage("*EXEC_SQL source evidence*");
        }
    }

    public static IEnumerable<object[]> DocumentedRuntimeCases()
    {
        foreach (var (name, topic, count) in new[]
        {
            ("CEEGMT", "ceegmtget-current-greenwich-mean-time", 3),
            ("CEEDAYS", "ceedaysconvert-date-lilian-format", 4),
            ("CEELOCT", "ceeloctget-current-local-date-time", 4)
        })
            foreach (var damage in new[] { "", "changed-profile", "mismatched-contract", "invented-body", "dynamic", "unknown-service" })
                yield return [name, topic, count, damage];
    }

    [Theory]
    [InlineData("")]
    [InlineData("missing-definition")]
    [InlineData("changed-catalog")]
    [InlineData("host-variable")]
    public void SharedSqlDeclarationRequiresTheActualCatalogAndCannotSupplyHostVariables(string damage)
    {
        var file = Source("MAIN.cbl", "           EXEC SQL SELECT VALUE FROM CONTROL END-EXEC.");
        var catalog = Source("catalog.cpy", "           EXEC SQL DECLARE CONTROL TABLE (VALUE INTEGER) END-EXEC.");
        var definition = new DependencyDefinition
        {
            Kind = "SQL_DECLARATION",
            Name = "CONTROL",
            Path = catalog.FilePath,
            Sha256 = DependencyEvidenceFixture.Hash(catalog.FilePath)
        };
        var evidence = DependencyEvidenceFixture.Write(_root, SourceRoot, "MAIN.cbl",
            damage == "missing-definition" ? [] : [definition]);
        var statement = JsonSerializer.SerializeToNode(DependencyEvidenceFixture.DialectStatement("EXEC_SQL", "SELECT"))!;
        statement["resourceBindings"] = new JsonArray(new JsonObject
        {
            ["kind"] = damage == "host-variable" ? "SQL_HOST_VARIABLE" : "SQL_RELATION",
            ["name"] = "CONTROL",
            ["definitionScope"] = "explicit-sql-catalog",
            ["sourcePath"] = "/source/catalog.cpy",
            ["sourceSha256"] = definition.Sha256,
            ["definitionSyntax"] = new JsonObject
            {
                ["rule"] = "Dbs_declare_tableContext",
                ["children"] = new JsonArray(new JsonObject { ["token"] = "DECLARE" })
            }
        });
        DependencyEvidenceFixture.BindExport(_root, evidence, JsonSerializer.Serialize(new
        {
            callTarget = new { name = "MAIN.cbl" },
            dependencies = Array.Empty<object>(),
            dependencyDialectStatements = new[] { statement }
        }));
        if (damage == "changed-catalog") File.AppendAllText(catalog.FilePath, "\nchanged");
        var verify = () => Gate.RequireVerified([file], SourceRoot);
        if (damage == "")
            verify.Should().NotThrow();
        else
            verify.Should().Throw<InvalidDataException>();
    }

    [Theory]
    [InlineData("")]
    [InlineData("wrong-copy-hash")]
    [InlineData("missing-native-binding")]
    public void DerivedBmsLayoutsRequireMatchingOriginalCopyAndNativeManifestHashes(string damage)
    {
        var file = Source("MAIN.cbl", "           COPY SCREEN.");
        var bms = Source("SCREEN.bms", "SCREEN DFHMSD TYPE=DSECT,LANG=COBOL");
        var copy = Source("SCREEN.cpy", "       01 SCREENI.\n          02 FIELDI PIC X(4).");
        var sourceHash = DependencyEvidenceFixture.Hash(bms.FilePath);
        var outputHash = DependencyEvidenceFixture.Hash(copy.FilePath);
        var proof = Source("SCREEN.cpy.bms-layout.json", JsonSerializer.Serialize(new
        {
            schemaVersion = 1,
            producer = "bms-symbolic-map/layout-v1",
            status = "complete",
            sourceSha256 = sourceHash,
            outputSha256 = damage == "wrong-copy-hash" ? new string('0', 64) : outputHash,
            maps = new[] { new { name = "SCREEN", byteLength = 7 } }
        }));
        var layoutHash = DependencyEvidenceFixture.Hash(proof.FilePath);
        var evidence = DependencyEvidenceFixture.Write(_root, SourceRoot, "MAIN.cbl",
            new DependencyDefinition { Kind = "COPY", Name = "SCREEN", Path = copy.FilePath, Sha256 = outputHash },
            new DependencyDefinition { Kind = "BMS", Name = "SCREEN", Path = bms.FilePath, Sha256 = sourceHash },
            new DependencyDefinition { Kind = "BMS_LAYOUT", Name = "SCREEN", Path = proof.FilePath, Sha256 = layoutHash });
        var binding = new
        {
            kind = "BMS_LAYOUT",
            name = "SCREEN.cpy",
            sourceSha256 = sourceHash,
            outputSha256 = outputHash,
            layoutSha256 = layoutHash
        };
        DependencyEvidenceFixture.BindExport(_root, evidence, JsonSerializer.Serialize(new
        {
            callTarget = new { name = "MAIN.cbl" },
            dependencies = new[] { new { dependencyKind = "COPY", callTarget = new { name = "SCREEN" }, dependencies = Array.Empty<object>() } },
            dependencyDerivedInputs = damage == "missing-native-binding" ? [] : new[] { binding }
        }));
        var verify = () => Gate.RequireVerified([file], SourceRoot);
        if (damage == "")
            verify.Should().NotThrow();
        else
            verify.Should().Throw<InvalidDataException>().WithMessage("*BMS SCREEN layout differs*");
    }

    [Theory]
    [InlineData("")]
    [InlineData("missing-transaction")]
    [InlineData("missing-program-source")]
    [InlineData("missing-callee")]
    [InlineData("remote")]
    [InlineData("unproven-target")]
    [InlineData("wrong-source")]
    [InlineData("program-dynamic-default")]
    [InlineData("program-dynamic-yes")]
    [InlineData("program-dynamic-missing")]
    [InlineData("program-dynamic-guessed-provenance")]
    public void AsyncRunRequiresCatalogBindingsAndTheActualParsedChild(string damage)
    {
        object programAttributes = damage switch
        {
            "program-dynamic-default" => new { language = "COBOL", dynamic = "NO", dynamicProvenance = "cics-default" },
            "program-dynamic-yes" => new { language = "COBOL", dynamic = "YES" },
            "program-dynamic-missing" => new { language = "COBOL" },
            "program-dynamic-guessed-provenance" => new { language = "COBOL", dynamic = "NO", dynamicProvenance = "guessed" },
            _ => new { language = "COBOL", dynamic = "NO" }
        };
        var file = Source("MAIN.cbl", "           EXEC CICS RUN TRANSID('TST1') CHILD(TOKEN) END-EXEC.");
        var child = Source("SUB.cbl", "           GOBACK.");
        var catalog = Source("resources.yaml", "resourceDefinitions: []");
        var catalogHash = DependencyEvidenceFixture.Hash(catalog.FilePath);
        var programHash = DependencyEvidenceFixture.Hash(child.FilePath);
        var definitions = new List<DependencyDefinition>
        {
            new() { Kind = "CALL", Name = "SUB", Path = child.FilePath, Sha256 = programHash },
            new() { Kind = "CICS_PROGRAM", Name = "SUB", Path = catalog.FilePath, Sha256 = catalogHash },
            new() { Kind = "CICS_TRANSACTION", Name = "TST1", Path = catalog.FilePath, Sha256 = catalogHash }
        };
        if (damage == "missing-transaction") definitions.RemoveAll(definition => definition.Kind == "CICS_TRANSACTION");
        if (damage == "missing-program-source") definitions.RemoveAll(definition => definition.Kind == "CALL");
        var evidence = DependencyEvidenceFixture.Write(_root, SourceRoot, "MAIN.cbl", definitions.ToArray());
        var statement = JsonSerializer.SerializeToNode(DependencyEvidenceFixture.DialectStatement("EXEC_CICS", "RUN"))!;
        statement["grammarRule"] = "Cics_runContext";
        statement["syntax"]!["rule"] = "Cics_runContext";
        statement["resourceBindings"] = JsonSerializer.SerializeToNode(new[]
        {
            new
            {
                kind = "CICS_TRANSACTION",
                name = "TST1",
                sourcePath = "/source/resources.yaml",
                sourceSha256 = catalogHash,
                attributes = new { program = "SUB", dynamic = "NO", routable = damage == "remote" ? "YES" : "NO", status = "ENABLED" },
                targetResolution = new { kind = damage == "unproven-target" ? "dynamic" : "literal" },
                programBinding = new
                {
                    kind = "CICS_PROGRAM",
                    name = "SUB",
                    sourcePath = "/source/resources.yaml",
                    sourceSha256 = catalogHash,
                    programSourcePath = damage == "wrong-source" ? "/source/OTHER.cbl" : "/source/SUB.cbl",
                    programSourceSha256 = programHash,
                    attributes = programAttributes
                }
            }
        });
        DependencyEvidenceFixture.BindExport(_root, evidence, JsonSerializer.Serialize(new
        {
            callTarget = new { name = "MAIN.cbl" },
            dependencyDialectStatements = new[] { statement },
            dependencies = damage == "missing-callee" ? [] : new[]
            {
                new
                {
                    dependencyKind = "CICS_RUN",
                    callTarget = new { name = "SUB", programReferenceType = "STATIC" },
                    dependencies = Array.Empty<object>()
                }
            }
        }));
        var verify = () => Gate.RequireVerified([file], SourceRoot);
        if (damage is "" or "program-dynamic-default")
            verify().Dependencies.Should().ContainSingle().Which.DependencyType.Should().Be("CALL");
        else
            verify.Should().Throw<InvalidDataException>();
    }

    [Theory]
    [MemberData(nameof(DocumentedRuntimeCases))]
    public void DocumentedRuntimeLeafRequiresTheActualUnchangedProfileAndNeverClaimsParsedSource(
        string service, string topic, int count, string damage)
    {
        var arguments = string.Join(" ", Enumerable.Range(1, count).Select(index => $"ARG-{index}"));
        var file = Source("MAIN.cbl", $"           CALL '{service}' USING {arguments}.");
        var profile = JsonSerializer.SerializeToNode(new
        {
            schemaVersion = 1, profileKind = "target-port", profileName = "test-target",
            runtimeContracts = new Dictionary<string, object>
            {
                [service] = new
                {
                    kind = "documented-platform-service", platform = "IBM z/OS Language Environment",
                    documentation = new[] { "https://www.ibm.com/docs/en/zos/3.1.0?topic=services-" + topic },
                    parameters = Enumerable.Range(1, count).Select(index => new
                    {
                        name = $"argument-{index}", passing = "BY REFERENCE", direction = "out",
                        representation = "Synthetic test parameter; not executable platform evidence"
                    }).ToArray()
                }
            }
        })!;
        var catalog = Source("profile.json", profile.ToJsonString());
        var hash = DependencyEvidenceFixture.Hash(catalog.FilePath);
        var definition = new DependencyDefinition
        {
            Kind = "RUNTIME_CONTRACT", Name = service, Path = catalog.FilePath, Sha256 = hash
        };
        var leaf = JsonSerializer.SerializeToNode(new
        {
            callTarget = new { name = service, programReferenceType = "STATIC" },
            dependencyKind = "EXTERNAL_RUNTIME", dependencyAnalysisComplete = false,
            runtimeContractBindingComplete = true, dependencies = Array.Empty<object>(),
            runtimeContract = new
            {
                name = service, profileKind = "target-port", profileName = "test-target",
                sourcePath = "/source/profile.json", sourceSha256 = hash,
                runtimeMapping = "unverified", resolutionKind = "documented-platform-contract",
                definition = profile["runtimeContracts"]![service]!.DeepClone()
            }
        })!;
        if (damage == "changed-profile") File.AppendAllText(catalog.FilePath, " ");
        if (damage == "mismatched-contract") leaf["runtimeContract"]!["definition"]!["platform"] = "Invented platform";
        if (damage == "invented-body") leaf["sourcePath"] = file.FilePath;
        if (damage == "dynamic") leaf["callTarget"]!["programReferenceType"] = "DYNAMIC";
        if (damage == "unknown-service")
        {
            leaf["callTarget"]!["name"] = "NOTREAL";
            leaf["runtimeContract"]!["name"] = "NOTREAL";
        }
        var evidence = DependencyEvidenceFixture.Write(_root, SourceRoot, "MAIN.cbl", definition);
        DependencyEvidenceFixture.BindExport(_root, evidence, JsonSerializer.Serialize(new
        {
            callTarget = new { name = "MAIN.cbl" }, dependencies = new[] { leaf }
        }));
        if (damage.Length > 0)
        {
            var verify = () => Gate.RequireVerified([file], SourceRoot);
            verify.Should().Throw<InvalidDataException>();
            return;
        }
        var map = Gate.RequireVerified([file], SourceRoot);
        map.Dependencies.Should().ContainSingle().Which.TargetFile.Should().Be(service);
        map.RuntimeRequirements.Should().ContainSingle().Which.Kind.Should().Be("EXTERNAL_RUNTIME");
        map.RuntimeRequirements.Single().Evidence.GetProperty("runtimeMapping").GetString().Should().Be("unverified");
    }

    [Theory]
    [InlineData("CALL")]
    [InlineData("CICS_LINK")]
    public void AClassicCallerRetainsItsSupportingCalleesRuntimeImplementationRequirements(string kind)
    {
        var main = Source("MAIN.cbl", kind == "CALL" ? "           CALL 'SUB'."
            : "           EXEC CICS LINK PROGRAM('SUB') END-EXEC.");
        var sub = Source("SUB.cbl", "           EXEC CICS RETURN END-EXEC.");
        var evidence = DependencyEvidenceFixture.Write(_root, SourceRoot, "MAIN.cbl", new DependencyDefinition
        {
            Kind = "CALL",
            Name = "SUB",
            Path = sub.FilePath,
            Sha256 = DependencyEvidenceFixture.Hash(sub.FilePath)
        });
        DependencyEvidenceFixture.BindExport(_root, evidence, JsonSerializer.Serialize(new
        {
            callTarget = new { name = "MAIN.cbl" },
            dependencyDialectStatements = kind == "CICS_LINK"
                ? new[] { DependencyEvidenceFixture.DialectStatement("EXEC_CICS", "LINK") } : Array.Empty<object>(),
            dependencies = new[]
            {
                new
                {
                    dependencyKind = kind, callTarget = new { name = "SUB", programReferenceType = "STATIC" },
                    dependencyDialectStatements = new[] { DependencyEvidenceFixture.DialectStatement("EXEC_CICS", "RETURN") },
                    dependencies = Array.Empty<object>()
                }
            }
        }));
        var map = Gate.RequireVerified([main], SourceRoot);
        map.Dependencies.Should().ContainSingle().Which.DependencyType.Should().Be("CALL");
        map.RuntimeRequirements.Where(requirement => requirement.Program == "SUB").Should().ContainSingle()
            .Which.Should().Match<RuntimeImplementationRequirement>(requirement =>
                requirement.Program == "SUB" && requirement.Kind == "EXEC_CICS" && requirement.Operation == "RETURN");
        if (kind == "CICS_LINK")
            map.RuntimeRequirements.Should().Contain(requirement => requirement.Program == "MAIN.cbl" && requirement.Operation == "LINK");
    }

    [Theory]
    [InlineData("support/SUB.cbl", true)]
    [InlineData("other/SUB.cbl", false)]
    public void CallerOnlySelectionUsesEstateIdentitiesWithoutAcceptingAnIncorrectFactsPath(
        string factsTarget, bool valid)
    {
        var main = Source("bank/MAIN.cbl", "           CALL 'SUB'.");
        var sub = Source("support/SUB.cbl", "           GOBACK.");
        Source(".preprocessed/support/SUB.cbl", "           GOBACK.");
        var evidence = DependencyEvidenceFixture.Write(_root, SourceRoot, "bank/MAIN.cbl",
            new DependencyDefinition
            {
                Kind = "CALL", Name = "SUB", Path = sub.FilePath,
                Sha256 = DependencyEvidenceFixture.Hash(sub.FilePath)
            });
        File.WriteAllText(Path.Combine(FactsRoot, "bank/MAIN.cbl.facts.json"),
            JsonSerializer.Serialize(new ProgramFacts
            {
                Basename = "MAIN.cbl", Stem = "MAIN", RelativePath = "bank/MAIN.cbl",
                SourceHash = evidence.OriginalSourceSha256, Confidence = FactConfidence.High,
                DependencyEvidence = evidence, Summary = new ProgramSummary(),
                Data = new DataFacts(), Callees = [factsTarget]
            }));

        if (!valid)
        {
            var verify = () => Gate.RequireVerified([main], SourceRoot);
            verify.Should().Throw<InvalidDataException>().WithMessage("*dependency entries differ*");
            return;
        }
        Gate.RequireVerified([main], SourceRoot).Dependencies.Should().ContainSingle()
            .Which.TargetFile.Should().Be("support/SUB.cbl");
    }

    [Fact]
    public void AParsedExecBlockDoesNotHideOtherUnrecordedOperations()
    {
        var file = Source("MAIN.cbl",
            "           EXEC CICS WRITE FILE('TESTFILE') END-EXEC.\n" +
            "           READ INPUT-FILE.\n" +
            "           EXEC CICS WRITE FILE('OTHERFILE') END-EXEC.");
        var evidence = DependencyEvidenceFixture.Write(_root, SourceRoot, "MAIN.cbl");
        DependencyEvidenceFixture.BindExport(_root, evidence, JsonSerializer.Serialize(new
        {
            callTarget = new { name = "MAIN.cbl" },
            dependencies = Array.Empty<object>(),
            dependencyDialectStatements = new[] { DependencyEvidenceFixture.DialectStatement("EXEC_CICS", "WRITE") }
        }));
        var verify = () => Gate.RequireVerified([file], SourceRoot);
        verify.Should().Throw<InvalidDataException>().WithMessage("*FILE_IO source evidence*EXEC_CICS source evidence*");
    }

    [Fact]
    public void MissingNativeDialectSyntaxCannotAuthorizeGeneration()
    {
        var file = Source("MAIN.cbl", "           EXEC CICS RETURN END-EXEC.");
        var evidence = DependencyEvidenceFixture.Write(_root, SourceRoot, "MAIN.cbl");
        DependencyEvidenceFixture.BindExport(_root, evidence, JsonSerializer.Serialize(new
        {
            callTarget = new { name = "MAIN.cbl" }, dependencies = Array.Empty<object>(),
            dependencyDialectStatements = new[] { new { kind = "EXEC_CICS", operation = "RETURN", bindingStatus = "complete" } }
        }));
        var verify = () => Gate.RequireVerified([file], SourceRoot);
        verify.Should().Throw<InvalidDataException>().WithMessage("*lacks complete parsed syntax/bindings*");
    }

    [Fact]
    public void QuotedCopyAndContinuedStaticCallOperandsMatchBoundExports()
    {
        var file = Source("MAIN.cbl",
            "           DISPLAY \"*> CALL WS-IGNORED COPY UNKNOWN\".\n" +
            "           COPY 'BOOK'.\n           CALL \"TAR\n      -    \"GET\".");
        var book = Source("BOOK.cpy", "       01 BOOK-RECORD PIC X.");
        var target = Source("TARGET.cbl", "       STOP RUN.");
        DependencyEvidenceFixture.Write(_root, SourceRoot, "MAIN.cbl",
            new DependencyDefinition
            {
                Kind = "COPY", Name = "BOOK.cpy", Path = book.FilePath,
                Sha256 = DependencyEvidenceFixture.Hash(book.FilePath)
            },
            new DependencyDefinition
            {
                Kind = "CALL", Name = "TARGET.cbl", Path = target.FilePath,
                Sha256 = DependencyEvidenceFixture.Hash(target.FilePath)
            });
        Gate.RequireVerified([file], SourceRoot).Dependencies.Select(edge => edge.DependencyType)
            .Should().BeEquivalentTo("COPY", "CALL");
    }

    [Theory]
    [InlineData("stale-call", "CALL definition C.cbl missing or SHA256 mismatch")]
    [InlineData("stale-copy", "COPY definition Q.cpy missing or SHA256 mismatch")]
    [InlineData("missing-copy", "COPY Q.cpy has no hash-bound resolved definition")]
    [InlineData("stub-copy", "placeholder/stub-backed")]
    [InlineData("diagnostic", "unresolved parser dependency diagnostics at depth 1")]
    [InlineData("dynamic", "unresolved dynamic CALL C.cbl")]
    public void SupportingSubtreeInputsAndDiagnosticsRemainEnforced(string damage, string diagnostic)
    {
        var a = Source("A.cbl", "           CALL 'B'.");
        var b = Source("B.cbl", "           CALL 'C'.\n       COPY Q.");
        var c = Source("C.cbl", "           STOP RUN.");
        var q = Source("Q.cpy", "       01 Q-RECORD PIC X.");
        DependencyDefinition Definition(string kind, CobolFile file) => new()
        {
            Kind = kind, Name = file.FileName, Path = file.FilePath, Sha256 = DependencyEvidenceFixture.Hash(file.FilePath)
        };
        var evidence = DependencyEvidenceFixture.Write(_root, SourceRoot, "A.cbl",
            Definition("CALL", b), Definition("CALL", c), Definition("COPY", q));
        var export = """
            {"callTarget":{"name":"A.cbl"},"dependencies":[
              {"dependencyKind":"CALL","callTarget":{"name":"B.cbl","programReferenceType":"STATIC"},
               "dependencyDiagnostics":[],"dependencies":[
                {"dependencyKind":"CALL","callTarget":{"name":"C.cbl","programReferenceType":"STATIC"},"dependencies":[]},
                {"dependencyKind":"COPY","callTarget":{"name":"Q.cpy"},"dependencies":[]}]}]}
            """;
        evidence = DependencyEvidenceFixture.BindExport(_root, evidence, export);
        switch (damage)
        {
            case "stale-call": File.AppendAllText(c.FilePath, "\n           DISPLAY 'changed'."); break;
            case "stale-copy": File.AppendAllText(q.FilePath, "\n       01 EXTRA PIC X."); break;
            case "missing-copy": evidence.Definitions.RemoveAll(definition => definition.Kind == "COPY"); break;
            case "stub-copy":
                File.WriteAllText(q.FilePath, "       01 Q-STUB PIC X.");
                evidence = evidence with
                {
                    Definitions = evidence.Definitions.Select(definition => definition.Kind == "COPY"
                        ? definition with { Sha256 = DependencyEvidenceFixture.Hash(q.FilePath) } : definition).ToList()
                };
                break;
            case "diagnostic":
                evidence = DependencyEvidenceFixture.BindExport(_root, evidence,
                    export.Replace("\"dependencyDiagnostics\":[]", "\"dependencyDiagnostics\":[\"missing nested input\"]"));
                break;
            case "dynamic":
                evidence = DependencyEvidenceFixture.BindExport(_root, evidence,
                    export.Replace("\"name\":\"C.cbl\",\"programReferenceType\":\"STATIC\"",
                        "\"name\":\"C.cbl\",\"programReferenceType\":\"DYNAMIC\""));
                break;
        }
        DependencyEvidenceFixture.Save(_root, evidence);
        var verify = () => Gate.RequireVerified([a], SourceRoot);
        verify.Should().Throw<InvalidDataException>().WithMessage($"*{diagnostic}*");
    }

    [Fact]
    public void SelectedCopybookUsesVerifiedProgramDefinitionContextWithoutStandaloneManifestOrSelfEdge()
    {
        var main = Source("MAIN.cbl", "       COPY BOOK.");
        var book = Source("BOOK.cpy", "       01 BOOK-RECORD PIC X.");
        var hash = DependencyEvidenceFixture.Hash(book.FilePath);
        var evidence = DependencyEvidenceFixture.Write(_root, SourceRoot, "MAIN.cbl", new DependencyDefinition
        {
            Kind = "COPY", Name = "BOOK.cpy", Path = book.FilePath, Sha256 = hash
        });
        DependencyEvidenceFixture.BindExport(_root, evidence, JsonSerializer.Serialize(new
        {
            dependencyExportSchemaVersion = 1,
            callTarget = new { name = "MAIN.cbl" },
            dependencies = new[]
            {
                new
                {
                    dependencyKind = "COPY", callTarget = new { name = "BOOK.cpy" },
                    sourcePath = book.FilePath, sourceSha256 = hash, dependencies = Array.Empty<object>()
                }
            }
        }));
        var map = Gate.RequireVerified([main, book], SourceRoot);
        map.Dependencies.Should().ContainSingle().Which.SourceFile.Should().Be("MAIN.cbl");
        map.CopybookUsage.Should().ContainKey("MAIN.cbl").WhoseValue.Should().Equal("BOOK.cpy");
        map.CopybookUsage.Should().NotContainKey("BOOK.cpy");
        map.EvidenceDiagnostics.Should().Contain(diagnostic => diagnostic.StartsWith("BOOK.cpy:")
            && diagnostic.Contains("no standalone parser coverage"));
        File.Exists(DependencyEvidenceGate.GetEvidencePath(FactsRoot, "BOOK.cpy")).Should().BeFalse();
        var withoutProgramContext = () => Gate.RequireVerified([book], SourceRoot);
        withoutProgramContext.Should().Throw<InvalidDataException>().WithMessage("*not an exact-path, hash-bound COPY definition*");
    }

    [Theory]
    [InlineData("unreferenced", "not an exact-path, hash-bound COPY definition")]
    [InlineData("changed", "COPY definition BOOK.cpy missing or SHA256 mismatch")]
    [InlineData("stub", "placeholder/stub-backed")]
    [InlineData("ambiguous", "ambiguous COPY definition")]
    [InlineData("stale-facts", "standalone copybook facts lack verified definition-context")]
    [InlineData("placeholder-facts", "standalone copybook facts lack verified definition-context")]
    public void SelectedCopybookRequiresUnambiguousCurrentDefinitionAndCannotInjectUnboundFacts(string damage, string diagnostic)
    {
        var main = Source("MAIN.cbl", "       COPY BOOK.");
        var book = Source("BOOK.cpy", "       01 BOOK-RECORD PIC X.");
        var definition = new DependencyDefinition
        {
            Kind = "COPY", Name = "BOOK.cpy", Path = book.FilePath, Sha256 = DependencyEvidenceFixture.Hash(book.FilePath)
        };
        var evidence = DependencyEvidenceFixture.Write(_root, SourceRoot, "MAIN.cbl", definition);
        switch (damage)
        {
            case "unreferenced":
                book = Source("other/BOOK.cpy", "       01 UNBOUND-RECORD PIC X.");
                break;
            case "changed": File.AppendAllText(book.FilePath, "\n       01 EXTRA PIC X."); break;
            case "stub":
                File.WriteAllText(book.FilePath, "       01 BOOK-STUB PIC X.");
                evidence = evidence with { Definitions = [definition with { Sha256 = DependencyEvidenceFixture.Hash(book.FilePath) }] };
                break;
            case "ambiguous":
                var other = Source("other/BOOK.cpy", "       01 DIFFERENT-RECORD PIC 9.");
                evidence.Definitions.Add(definition with
                {
                    Path = other.FilePath, Sha256 = DependencyEvidenceFixture.Hash(other.FilePath)
                });
                break;
            case "stale-facts":
            case "placeholder-facts":
                File.WriteAllText(Path.Combine(FactsRoot, "BOOK.cpy.facts.json"), JsonSerializer.Serialize(new ProgramFacts
                {
                    Basename = "BOOK.cpy", Stem = "BOOK", RelativePath = "BOOK.cpy",
                    SourceHash = "unbound-staged-hash", Confidence = FactConfidence.High,
                    Summary = new ProgramSummary { IsCopybook = true },
                    Warnings = damage == "placeholder-facts" ? ["generated-copybook-stub:BOOK"] : []
                }));
                break;
        }
        DependencyEvidenceFixture.Save(_root, evidence);
        var verify = () => Gate.RequireVerified([main, book], SourceRoot);
        verify.Should().Throw<InvalidDataException>().WithMessage($"*{diagnostic}*");
    }

    private CobolFile Source(string relative, string content)
    {
        var path = Path.Combine(SourceRoot, relative);
        Directory.CreateDirectory(Path.GetDirectoryName(path)!);
        File.WriteAllText(path, content);
        return new CobolFile { FileName = Path.GetFileName(relative), FilePath = path, Content = content };
    }

    public void Dispose()
    {
        if (Directory.Exists(_root)) Directory.Delete(_root, true);
    }
}
