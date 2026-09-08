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
    public void Evaluate_SymbolOnlyInCommentIsNotScoredAsLoss()
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

        result.Score.Should().Be(1.0);
        result.Gaps.Should().ContainSingle()
            .Which.Kind.Should().Be(ParityGapKind.PossiblyRenamedOrMerged);
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
}
