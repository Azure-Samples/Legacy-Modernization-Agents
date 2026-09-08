using CobolToQuarkusMigration.Agents;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Agents;

public class ConversionParityValidatorTests
{
    [Theory]
    [InlineData("getCustomerRecord", new[] { "get", "customer", "record" })]
    [InlineData("SQLQuery", new[] { "sql", "query" })]
    [InlineData("subPgm01Service", new[] { "sub", "pgm", "01", "service" })]
    [InlineData("WS-CUSTOMER-RECORD", new[] { "ws", "customer", "record" })]
    [InlineData("1000-INIT", new[] { "1000", "init" })]
    [InlineData("SUBPGM01", new[] { "subpgm", "01" })]
    public void SplitIdentifier_ProducesWordTokens(string input, string[] expected)
    {
        ConversionParityValidator.Tokenizer.SplitIdentifier(input).Should().Equal(expected);
    }

    [Fact]
    public void SplitCodeAndComments_SeparatesLineAndBlockComments()
    {
        const string source = """
            public class Foo {
                // computeTotals was here
                void run() { /* readCustomer */ callThing(); }
            }
            """;

        var (code, comments) = ConversionParityValidator.Tokenizer.SplitCodeAndComments(source);

        code.Should().Contain("callThing").And.Contain("run");
        code.Should().NotContain("computeTotals").And.NotContain("readCustomer");
        comments.Should().Contain("computeTotals").And.Contain("readCustomer");
    }

    [Fact]
    public void SplitCodeAndComments_StringLiteralsCountAsComments()
    {
        const string source = "var q = \"SELECT * FROM CUSTOMER_MASTER\"; runQuery(q);";

        var (code, comments) = ConversionParityValidator.Tokenizer.SplitCodeAndComments(source);

        code.Should().NotContain("CUSTOMER_MASTER");
        comments.Should().Contain("CUSTOMER_MASTER");
        code.Should().Contain("runQuery");
    }

    [Fact]
    public void SplitCodeAndComments_UnterminatedStringDoesNotSwallowRestOfFile()
    {
        const string source = "String bad = \"oops;\nvoid keepMe() {}";

        var (code, _) = ConversionParityValidator.Tokenizer.SplitCodeAndComments(source);

        code.Should().Contain("keepMe");
    }

    [Fact]
    public void Evaluate_FullyRepresentedConversion_ScoresOne()
    {
        var context = Context(
            sections: new[] { "1000-INIT", "2000-PROCESS-CUSTOMER" },
            fields: new[] { "WS-CUSTOMER-ID", "WS-TOTAL-AMOUNT" },
            calls: new[] { "SUBPGM01" },
            tables: new[] { "CUSTOMER_MASTER" });

        const string java = """
            public class CustomerBatch {
                private String customerId;
                private BigDecimal totalAmount;
                void init() {}
                void processCustomer() { subPgm01Service.run(); customerMasterRepository.findAll(); }
            }
            """;

        var result = ConversionParityValidator.Evaluate("CUST.cbl", "CustomerBatch.java", java, context, null);

        result.Outcome.Should().Be(ParityOutcome.Evaluated);
        result.Score.Should().Be(1.0);
        result.Gaps.Should().BeEmpty();
    }

    [Fact]
    public void Evaluate_DroppedSectionIsReportedMissing()
    {
        var context = Context(
            sections: new[] { "1000-INIT", "2000-PROCESS-CUSTOMER" },
            fields: Array.Empty<string>(),
            calls: Array.Empty<string>(),
            tables: Array.Empty<string>());

        const string java = "public class C { void init() {} }";

        var result = ConversionParityValidator.Evaluate("CUST.cbl", "C.java", java, context, null);

        result.Outcome.Should().Be(ParityOutcome.Evaluated);
        result.Score.Should().Be(0.5);
        result.Gaps.Should().ContainSingle()
            .Which.Should().Match<ParityGap>(g =>
                g.Symbol == "2000-PROCESS-CUSTOMER" && g.Kind == ParityGapKind.Missing);
    }

    [Fact]
    public void Evaluate_SymbolOnlyInCommentScoresPartialCreditNotFull()
    {
        var context = Context(
            sections: new[] { "2000-PROCESS-CUSTOMER" },
            fields: Array.Empty<string>(),
            calls: Array.Empty<string>(),
            tables: Array.Empty<string>());

        const string java = """
            public class C {
                // 2000-PROCESS-CUSTOMER was folded into run()
                void run() {}
            }
            """;

        var result = ConversionParityValidator.Evaluate("CUST.cbl", "C.java", java, context, null);

        result.Score.Should().Be(0.5);
        result.Gaps.Should().ContainSingle()
            .Which.Kind.Should().Be(ParityGapKind.PossiblyRenamedOrMerged);
    }

    [Fact]
    // A handful of renames must stay above the gate, or the validator gets switched off.
    public void Evaluate_MostlyConvertedWithOneRenameStaysAboveThreshold()
    {
        var context = Context(
            sections: new[] { "1000-INIT", "2000-PROCESS", "3000-REPORT", "4000-CLEANUP" },
            fields: Array.Empty<string>(),
            calls: Array.Empty<string>(),
            tables: Array.Empty<string>());

        const string java = """
            public class C {
                void init() {}
                void process() {}
                void report() {}
                // 4000-CLEANUP was folded into process()
            }
            """;

        var result = ConversionParityValidator.Evaluate("CUST.cbl", "C.java", java, context, null);

        result.Score.Should().Be(0.875);
        result.Score.Should().BeGreaterThan(0.75);
    }

    [Fact]
    // The degenerate adversarial case: a file that converts nothing but names everything in a
    // comment scored a perfect 1.0 before comment evidence was down-weighted.
    public void Evaluate_HollowFileNamingEverythingInCommentsFails()
    {
        var context = Context(
            sections: new[] { "1000-INIT", "2000-PROCESS" },
            fields: new[] { "WS-CUSTOMER-TOTAL", "WS-ACCOUNT-BALANCE" },
            calls: Array.Empty<string>(),
            tables: Array.Empty<string>());

        const string java = """
            public class Empty {
                // 1000-INIT 2000-PROCESS WS-CUSTOMER-TOTAL WS-ACCOUNT-BALANCE
            }
            """;

        var result = ConversionParityValidator.Evaluate("CUST.cbl", "Empty.java", java, context, null);

        result.Score.Should().Be(0.5);
        result.Score.Should().BeLessThan(0.75);
        result.Axes.Should().OnlyContain(a => a.Coverage == null || a.IsTotalLoss);
    }

    [Fact]
    public void Evaluate_DiagnosticStubScoresZeroDespiteEmbeddedCobol()
    {
        var context = Context(
            sections: new[] { "1000-INIT" },
            fields: new[] { "WS-CUSTOMER-ID" },
            calls: new[] { "SUBPGM01" },
            tables: new[] { "CUSTOMER_MASTER" });

        // The guard preserves the rejected model output inside a block comment, which is why
        // whole-file substring matching reports a failed conversion as a pass.
        var stub = ConversionOutputGuard.BuildWholeFileDiagnosticStub(
            "Java",
            "NO_JAVA_STRUCTURE",
            """
            public class CustomerBatch {
                void init() {}
                String customerId;
                void call() { subPgm01Service.run(); customerMasterRepository.findAll(); }
            }
            """);

        var result = ConversionParityValidator.Evaluate("CUST.cbl", "C.java", stub, context, null);

        result.Outcome.Should().Be(ParityOutcome.Evaluated);
        result.IsDiagnosticStub.Should().BeTrue();
        result.Score.Should().Be(0.0);
        result.Gaps.Should().Contain(g => g.Axis == "file" && g.Kind == ParityGapKind.Missing);
    }

    [Fact]
    public void Evaluate_NoStructuralContextIsNotEvaluatedRatherThanPassing()
    {
        var result = ConversionParityValidator.Evaluate("CUST.cbl", "C.java", "public class C {}", null, null);

        result.Outcome.Should().Be(ParityOutcome.NotEvaluated);
        result.Score.Should().BeNull();
        result.NotEvaluatedReason.Should().NotBeNullOrWhiteSpace();
    }

    [Fact]
    public void Evaluate_StubWithoutStructuralContextIsMeasuredZeroNotUnmeasurable()
    {
        // The stub marker is direct evidence the conversion failed, so it outranks the
        // missing-context rule; otherwise a known failure is reported as unmeasurable.
        var result = ConversionParityValidator.Evaluate(
            "CUST.cbl", "C.java", "// CONVERSION DID NOT PRODUCE USABLE OUTPUT", null, null);

        result.Outcome.Should().Be(ParityOutcome.Evaluated);
        result.Score.Should().Be(0);
        result.IsDiagnosticStub.Should().BeTrue();
        result.Gaps.Should().ContainSingle().Which.Axis.Should().Be("file");
    }

    [Fact]
    public void Evaluate_ProvenanceNoneIsNotEvaluated()
    {
        var context = Context(new[] { "1000-INIT" }, Array.Empty<string>(), Array.Empty<string>(), Array.Empty<string>());
        context.Provenance = StructuralProvenance.None;

        var result = ConversionParityValidator.Evaluate("CUST.cbl", "C.java", "public class C {}", context, null);

        result.Outcome.Should().Be(ParityOutcome.NotEvaluated);
        result.Score.Should().BeNull();
    }

    [Fact]
    public void Evaluate_EmptyStructuralContextIsNotEvaluated()
    {
        var context = Context(Array.Empty<string>(), Array.Empty<string>(), Array.Empty<string>(), Array.Empty<string>());

        var result = ConversionParityValidator.Evaluate("CUST.cbl", "C.java", "public class C {}", context, null);

        result.Outcome.Should().Be(ParityOutcome.NotEvaluated);
        result.Score.Should().BeNull();
    }

    [Fact]
    public void Evaluate_AbsentAxisNeitherRewardsNorPenalises()
    {
        // Only fields present; the score must come from that axis alone, not be diluted
        // toward zero by three empty axes.
        var context = Context(
            sections: Array.Empty<string>(),
            fields: new[] { "WS-CUSTOMER-ID", "WS-TOTAL-AMOUNT" },
            calls: Array.Empty<string>(),
            tables: Array.Empty<string>());

        const string java = "public class C { String customerId; }";

        var result = ConversionParityValidator.Evaluate("CUST.cbl", "C.java", java, context, null);

        result.Score.Should().Be(0.5);
        result.Axes.Single(a => a.Name == "dataFields").Coverage.Should().Be(0.5);
        result.Axes.Single(a => a.Name == "procedures").Coverage.Should().BeNull();
    }

    [Fact]
    public void Evaluate_FillerAndConditionNamesAreExcluded()
    {
        var context = new StructuralContext
        {
            Program = "CUST.cbl",
            Provenance = StructuralProvenance.RektNative,
            Confidence = 0.95,
            Context = new RektContext
            {
                DataStructure =
                {
                    new RektDataItem { Level = 1, Name = "WS-CUSTOMER-ID" },
                    new RektDataItem { Level = 5, Name = "FILLER" },
                    new RektDataItem { Level = 88, Name = "WS-CUSTOMER-VALID" },
                    new RektDataItem { Level = 66, Name = "WS-RENAMED-BLOCK" },
                    new RektDataItem { Level = 5, Name = "TypedRecordNoise" },
                },
            },
        };

        const string java = "public class C { String customerId; }";

        var result = ConversionParityValidator.Evaluate("CUST.cbl", "C.java", java, context, null);

        result.Score.Should().Be(1.0);
        result.Axes.Single(a => a.Name == "dataFields").Expected.Should().Be(1);
    }

    [Fact]
    public void Evaluate_ProcedureDivisionRegistersAreExcluded()
    {
        var context = new StructuralContext
        {
            Program = "CUST.cbl",
            Provenance = StructuralProvenance.RektNative,
            Confidence = 0.95,
            Context = new RektContext
            {
                DataStructure =
                {
                    new RektDataItem { Level = 1, Name = "WS-CUSTOMER-ID", SourceSection = "WORKING_STORAGE" },
                    new RektDataItem { Level = 1, Name = "WHEN-COMPILED", SourceSection = "PROCEDURE_DIVISION" },
                },
            },
        };

        var result = ConversionParityValidator.Evaluate(
            "CUST.cbl", "C.java", "public class C { String customerId; }", context, null);

        result.Axes.Single(a => a.Name == "dataFields").Expected.Should().Be(1);
        result.Gaps.Should().NotContain(g => g.Symbol == "WHEN-COMPILED");
        result.Score.Should().Be(1.0);
    }

    [Fact]
    public void Evaluate_LinkageFieldGapRecordsWhereTheFieldCameFrom()
    {
        var context = new StructuralContext
        {
            Program = "CUST.cbl",
            Provenance = StructuralProvenance.RektNative,
            Confidence = 0.95,
            Context = new RektContext
            {
                DataStructure =
                {
                    new RektDataItem
                    {
                        Level = 1,
                        Name = "CUSTOMER-RECORD",
                        SourceSection = "LINKAGE",
                        Children = { new RektDataItem { Level = 5, Name = "CUST-STATUS" } },
                    },
                },
            },
        };

        var result = ConversionParityValidator.Evaluate(
            "CUST.cbl", "C.java", "public class C { CustomerRecord customerRecord; }", context, null);

        // The section is declared on the parent, so the child must inherit it.
        result.Gaps.Single(g => g.Symbol == "CUST-STATUS")
            .Detail.Should().Contain("LINKAGE");
    }

    [Fact]
    public void Evaluate_StubCopybookFieldsAreExcludedAndReportedAsWeakenedEvidence()
    {
        using var estate = new TempEstate();
        estate.WritePreprocessedCopybook(
            "ERROR-CODES.cpy",
            $"      *> {StubCopybookCatalog.Marker}\n       01 ERROR-CODES-STUB PIC X.\n       01 ERROR-CODES-VAL PIC X.\n");

        var catalog = StubCopybookCatalog.Load(estate.Root, "source");

        var context = new StructuralContext
        {
            Program = "CUST.cbl",
            Provenance = StructuralProvenance.RektNative,
            Confidence = 0.95,
            Context = new RektContext
            {
                DataStructure =
                {
                    new RektDataItem { Level = 1, Name = "WS-CUSTOMER-ID" },
                    new RektDataItem { Level = 1, Name = "ERROR-CODES-STUB" },
                    new RektDataItem { Level = 1, Name = "ERROR-CODES-VAL" },
                },
            },
        };

        var result = ConversionParityValidator.Evaluate(
            "CUST.cbl", "C.java", "public class C { String customerId; }", context, null, catalog);

        result.Axes.Single(a => a.Name == "dataFields").Expected.Should().Be(1);
        result.Gaps.Should().NotContain(g => g.Symbol.StartsWith("ERROR-CODES"));
        result.EvidenceNotes.Should().ContainSingle()
            .Which.Should().Contain("ERROR-CODES-STUB").And.Contain("incomplete evidence");
    }

    [Fact]
    public void Load_TreatsOnlyMarkedCopybooksAsStubs()
    {
        using var estate = new TempEstate();
        estate.WritePreprocessedCopybook("REAL.cpy", "       01 REAL-FIELD PIC X.\n");
        estate.WritePreprocessedCopybook(
            "STUBBED.cpy", $"      *> {StubCopybookCatalog.Marker}\n       01 STUB-FIELD PIC X.\n");

        var catalog = StubCopybookCatalog.Load(estate.Root, "source");

        catalog.Contains("STUB-FIELD").Should().BeTrue();
        catalog.Contains("REAL-FIELD").Should().BeFalse("a real copybook must never have its fields excluded");
        catalog.Copybooks.Should().ContainSingle().Which.Should().Be("STUBBED");
    }

    private sealed class TempEstate : IDisposable
    {
        public string Root { get; } = Directory.CreateTempSubdirectory("parity-stub-").FullName;

        public void WritePreprocessedCopybook(string name, string content)
        {
            var dir = Path.Combine(Root, "source", ".preprocessed");
            Directory.CreateDirectory(dir);
            File.WriteAllText(Path.Combine(dir, name), content);
        }

        public void Dispose()
        {
            try { Directory.Delete(Root, recursive: true); } catch (IOException) { }
        }
    }

    [Fact]
    public void Evaluate_ImplicitSectionIsNotCountedAsExpectedSymbol()
    {
        // The REKT loader synthesises "(implicit)" for paragraphs with no enclosing section.
        var context = new StructuralContext
        {
            Program = "CUST.cbl",
            Provenance = StructuralProvenance.RektNative,
            Confidence = 0.95,
            Context = new RektContext
            {
                Sections = { new RektSection { Name = "(implicit)" } },
                DataStructure = { new RektDataItem { Level = 1, Name = "WS-CUSTOMER-ID" } },
            },
        };

        var result = ConversionParityValidator.Evaluate(
            "CUST.cbl", "C.java", "public class C { String customerId; }", context, null);

        result.Axes.Single(a => a.Name == "procedures").Coverage.Should().BeNull();
        result.Gaps.Should().NotContain(g => g.Symbol == "(implicit)");
    }

    [Fact]
    public void Evaluate_PerformTargetsSupplyParagraphNames()
    {
        var context = new StructuralContext
        {
            Program = "CUST.cbl",
            Provenance = StructuralProvenance.RektNative,
            Confidence = 0.95,
            Context = new RektContext
            {
                PerformGraph =
                {
                    new RektPerformEdge { From = "", To = "0100-READ-CUSTOMER THRU 0199-EXIT" },
                    new RektPerformEdge { From = "", To = "5 TIMES" },
                },
            },
        };

        var result = ConversionParityValidator.Evaluate(
            "CUST.cbl", "C.java", "public class C { void readCustomer() {} }", context, null);

        var procedures = result.Axes.Single(a => a.Name == "procedures");
        procedures.Expected.Should().Be(2);
        result.Gaps.Should().ContainSingle().Which.Symbol.Should().Be("0199-EXIT");
    }

    [Fact]
    public void Evaluate_SqlTablesPreferProgramFactsAndCarryOperations()
    {
        var context = Context(
            Array.Empty<string>(), Array.Empty<string>(), Array.Empty<string>(),
            tables: new[] { "STALE_TABLE" });

        var facts = Facts(new DbTableAccess("CUSTOMER_MASTER", new[] { "SELECT", "UPDATE" }));

        var result = ConversionParityValidator.Evaluate(
            "CUST.cbl", "C.java", "public class C {}", context, facts);

        result.Gaps.Should().ContainSingle()
            .Which.Should().Match<ParityGap>(g =>
                g.Symbol == "CUSTOMER_MASTER" && g.Detail!.Contains("SELECT") && g.Detail.Contains("UPDATE"));
    }

    [Fact]
    public void Evaluate_ShortNamesAreExcludedRatherThanMatchedLoosely()
    {
        var context = Context(
            Array.Empty<string>(),
            fields: new[] { "WS-A", "WS-CUSTOMER-ID" },
            Array.Empty<string>(), Array.Empty<string>());

        const string java = "public class C { String customerId; }";

        var result = ConversionParityValidator.Evaluate("CUST.cbl", "C.java", java, context, null);

        var fields = result.Axes.Single(a => a.Name == "dataFields");
        fields.Expected.Should().Be(1);
        fields.Excluded.Should().Be(1);
        result.Score.Should().Be(1.0);
    }

    [Fact]
    // ProgramFacts records callees as source-relative paths for identity. Comparing the raw
    // path tokenised to [shared, receiver, cbl], which no faithful conversion can match, so
    // every program with facts lost the whole call-target axis.
    public void Evaluate_CalleeRecordedAsSourceRelativePathMatchesGeneratedClass()
    {
        var context = Context(
            sections: new[] { "1000-MAIN" },
            fields: Array.Empty<string>(),
            calls: Array.Empty<string>(),
            tables: Array.Empty<string>());

        const string java = """
            public class C {
                void main() { receiverService.call(); }
            }
            """;

        var result = ConversionParityValidator.Evaluate(
            "CUST.cbl", "C.java", java, context, FactsWithCallees("shared/RECEIVER.cbl"));

        var axis = result.Axes.Single(a => a.Name == "callTargets");
        axis.Expected.Should().Be(1);
        axis.MatchedInCode.Should().Be(1);
        axis.IsTotalLoss.Should().BeFalse();

        // The path stays in the gap text so identity is not lost when a gap is real.
        result.Gaps.Should().NotContain(g => g.Axis == "callTargets");
    }

    [Fact]
    // A target resolved from a variable at runtime has no name a converter could emit, so
    // expecting one invents a gap that can never be closed.
    public void Evaluate_DynamicCallTargetIsNotExpectedInGeneratedCode()
    {
        var ctx = new RektContext();
        ctx.Sections.Add(new RektSection { Name = "1000-MAIN" });
        ctx.CallTargets.Add(new RektCallTarget { TargetProgram = "WS-PROGRAM-NAME", IsDynamic = true });
        ctx.CallTargets.Add(new RektCallTarget { TargetProgram = "FORMAT-BALANCE" });

        var context = new StructuralContext
        {
            Program = "CUST.cbl",
            Provenance = StructuralProvenance.RektNative,
            Confidence = 0.95,
            Context = ctx,
        };

        const string java = """
            public class C {
                void main() { formatBalance(); }
            }
            """;

        var result = ConversionParityValidator.Evaluate("CUST.cbl", "C.java", java, context, null);

        var axis = result.Axes.Single(a => a.Name == "callTargets");
        axis.Expected.Should().Be(1);
        axis.MatchedInCode.Should().Be(1);
    }

    [Fact]
    // Renormalising over present axes let a total loss stay above the gate: dropping every CALL
    // target with no SQL present scores 0.65/0.85 = 0.76 against a 0.75 threshold.
    public void Evaluate_TotalLossOfAnAxisIsFlaggedEvenWhenTheScoreClearsTheThreshold()
    {
        var context = Context(
            sections: new[] { "1000-MAIN" },
            fields: new[] { "WS-CUSTOMER-TOTAL" },
            calls: new[] { "FORMAT-BALANCE", "AUDIT-LOG" },
            tables: Array.Empty<string>());

        const string java = """
            public class C {
                int customerTotal;
                void main() {}
            }
            """;

        var result = ConversionParityValidator.Evaluate("CUST.cbl", "C.java", java, context, null);

        result.Score.Should().BeApproximately(0.7647, 0.0005);
        result.Score.Should().BeGreaterThan(0.75);
        result.LostAxes.Should().Equal("callTargets");
        ConversionParityPostPass.Fails(result, 0.75).Should().BeTrue();
    }

    private static StructuralContext Context(
        IEnumerable<string> sections,
        IEnumerable<string> fields,
        IEnumerable<string> calls,
        IEnumerable<string> tables)
    {
        var ctx = new RektContext();
        foreach (var s in sections) ctx.Sections.Add(new RektSection { Name = s });
        foreach (var f in fields) ctx.DataStructure.Add(new RektDataItem { Level = 5, Name = f });
        foreach (var c in calls) ctx.CallTargets.Add(new RektCallTarget { TargetProgram = c });
        foreach (var t in tables)
            ctx.SqlStatements.Add(new RektSqlStatement { Operation = "SELECT", Tables = { t } });

        return new StructuralContext
        {
            Program = "CUST.cbl",
            Provenance = StructuralProvenance.RektNative,
            Confidence = 0.95,
            Context = ctx,
        };
    }

    private static ProgramFacts Facts(params DbTableAccess[] tables) => new()
    {
        Basename = "CUST.cbl",
        Stem = "CUST",
        SourceHash = "hash",
        Confidence = FactConfidence.High,
        Summary = new ProgramSummary(),
        Io = new IoFacts { DbTables = tables },
    };

    private static ProgramFacts FactsWithCallees(params string[] callees) => new()
    {
        Basename = "CUST.cbl",
        Stem = "CUST",
        SourceHash = "hash",
        Confidence = FactConfidence.High,
        Summary = new ProgramSummary(),
        Io = new IoFacts(),
        Callees = callees,
    };
}
