using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

public sealed class RektContextLoaderTests : IDisposable
{
    [Fact]
    public void Load_CopybookDoesNotReadSameStemProgramArtifacts()
    {
        var report = Path.Combine(_root, "output", "rekt", "nested", "SHARED.cbl.report", "flow_ast");
        Directory.CreateDirectory(report);
        File.WriteAllText(Path.Combine(report, "flow-ast-SHARED.cbl.json"),
            """{"type":"PARAGRAPH","name":"PROGRAM-ONLY"}""");
        var loader = new RektContextLoader(_root);
        loader.Load("SHARED.cpy", "source").Sections.Should().BeEmpty();
        loader.Load("nested/SHARED.cpy", "source").Sections.Should().BeEmpty();
        loader.Load("nested/SHARED.cbl", "source").Sections
            .SelectMany(s => s.Paragraphs).Select(p => p.Name).Should().Equal("PROGRAM-ONLY");
    }

    [Fact]
    public void Load_DependenciesPreservePathAndSourceKind()
    {
        var rekt = Path.Combine(_root, "output", "rekt");
        Directory.CreateDirectory(rekt);
        File.WriteAllText(Path.Combine(rekt, "SHARED.cbl-deps.json"),
            """{"dependencies":[{"name":"PROGRAM-ONLY"}]}""");
        var loader = new RektContextLoader(_root);
        loader.Load("SHARED.cpy", "source").CallTargets.Should().BeEmpty();
        loader.Load("nested/SHARED.cbl", "source").CallTargets.Should().BeEmpty();
        loader.Load("SHARED.cbl", "source").CallTargets.Select(target => target.TargetProgram)
            .Should().Equal("PROGRAM-ONLY");
    }

    private readonly string _root =
        Path.GetFullPath(Path.Combine(".test-artifacts", $"rekt-loader-{Guid.NewGuid():N}"));

    [Fact]
    public void Load_ArchivedOrInProgressReportsAreNotActiveEvidence()
    {
        foreach (var reserved in new[] { ".previous", ".work" })
        {
            var path = Path.Combine(_root, "output", "rekt", reserved, "old", "MAIN.cbl.report", "flow_ast");
            Directory.CreateDirectory(path);
            File.WriteAllText(Path.Combine(path, "flow-ast-MAIN.cbl.json"),
                """{"type":"PARAGRAPH","name":"HISTORICAL"}""");
        }
        var loader = new RektContextLoader(_root);
        loader.HasAnyRektOutput().Should().BeFalse();
        loader.Load("MAIN.cbl", "source").Sections.Should().BeEmpty();
    }

    [Fact]
    public void Load_DependencyExportPreservesOnlyRootCopyAndCallEntries()
    {
        var rekt = Path.Combine(_root, "output", "rekt", "billing");
        Directory.CreateDirectory(rekt);
        File.WriteAllText(Path.Combine(rekt, "MAIN.cbl-deps.json"), """
            {"callTarget":{"name":"MAIN.cbl"},"dependencies":[
              {"callTarget":{"name":"BOOK.cpy"},"dependencies":[{"name":"NESTED.cpy"}]},
              {"callTarget":{"name":"TARGET.cbl","programReferenceType":"STATIC"},"dependencies":[]}]}
            """);
        var context = new RektContextLoader(_root).Load("billing/MAIN.cbl", "source");
        context.CopybookUsage.Should().Equal("BOOK.cpy");
        context.CallTargets.Select(target => target.TargetProgram).Should().Equal("TARGET.cbl");
    }

    [Fact]
    public void Load_NativeExternalControlIsNotInventedAsCallDependency()
    {
        var rekt = Path.Combine(_root, "output", "rekt");
        Directory.CreateDirectory(rekt);
        File.WriteAllText(Path.Combine(rekt, "MAIN.cbl-deps.json"), """
            {"callTarget":{"name":"MAIN.cbl"},"dependencyExportSchemaVersion":1,"dependencies":[
              {"dependencyKind":"COPY","callTarget":{"name":"BOOK.cpy"},"dependencies":[]},
              {"dependencyKind":"EXTERNAL_CONTROL","callTarget":{"name":"CICS-TARGET"},"dependencies":[]},
              {"dependencyKind":"CALL","callTarget":{"name":"STATIC-TARGET","programReferenceType":"STATIC"},"dependencies":[]}]}
            """);
        var context = new RektContextLoader(_root).Load("MAIN.cbl", "source");
        context.CopybookUsage.Should().Equal("BOOK.cpy");
        context.CallTargets.Select(target => target.TargetProgram).Should().Equal("STATIC-TARGET");
    }

    [Fact]
    public void HasAnyRektOutput_UsesCustomRektDirectory()
    {
        var customRektDir = Path.Combine(_root, "custom-rekt");
        Directory.CreateDirectory(customRektDir);
        File.WriteAllText(Path.Combine(customRektDir, "flow-ast-TEST.json"), "{}");

        var loader = new RektContextLoader(_root, customRektDir);

        loader.HasAnyRektOutput().Should().BeTrue();
    }

    [Fact]
    public void Load_UsesUntruncatedOriginalTextForPerformTargets()
    {
        // smojol truncates statement `name` to 15 chars: "PERFORM SEARCH-CUSTOMER" arrives as
        // "PERFORMSEARCH-C". These are the exact shapes emitted by a real scan.
        var ctx = LoadAst("""
        {
          "type": "PROCEDURE_DIVISION_BODY", "name": "body", "children": [
            { "type": "PARAGRAPH", "name": "MAIN-LOGIC", "children": [
              { "type": "SENTENCE", "name": "PERFORMSEARCH-C",
                "originalText": "PERFORM SEARCH-CUSTOMER." }
            ]},
            { "type": "PARAGRAPH", "name": "SEARCH-CUSTOMER", "children": [] }
          ]
        }
        """);

        ctx.PerformGraph.Select(e => e.To).Should().ContainSingle().Which.Should().Be("SEARCH-CUSTOMER");
    }

    [Fact]
    public void Load_UsesUntruncatedOriginalTextForCallTargets()
    {
        var ctx = LoadAst("""
        {
          "type": "PROCEDURE_DIVISION_BODY", "name": "body", "children": [
            { "type": "PARAGRAPH", "name": "MAIN-LOGIC", "children": [
              { "type": "CALL", "name": "CALL'FORMAT-BAL",
                "originalText": "CALL 'FORMAT-BALANCE' USING CUST-BALANCE" }
            ]}
          ]
        }
        """);

        ctx.CallTargets.Select(c => c.TargetProgram)
           .Should().ContainSingle().Which.Should().Be("FORMAT-BALANCE");
    }

    [Fact]
    public void Load_RecordsOnlyRealParagraphsNotStatements()
    {
        var ctx = LoadAst("""
        {
          "type": "PROCEDURE_DIVISION_BODY", "name": "body", "children": [
            { "type": "PARAGRAPH", "name": "MAIN-LOGIC", "children": [
              { "type": "SENTENCE", "name": "OPENINPUTCUSTOM",
                "originalText": "OPEN INPUT CUSTOMER-FILE." },
              { "type": "SENTENCE", "name": "DISPLAY'ENTERCU",
                "originalText": "DISPLAY 'Enter Customer ID: '." }
            ]}
          ]
        }
        """);

        ctx.Sections.SelectMany(s => s.Paragraphs).Select(p => p.Name)
           .Should().BeEquivalentTo("MAIN-LOGIC");
    }

    [Fact]
    public void Load_HarvestsCallNestedInsideConditionalStatement()
    {
        // A CALL inside READ ... NOT INVALID KEY is emitted only as GENERIC_STATEMENT text,
        // with no CALL node anywhere in the tree.
        var ctx = LoadAst("""
        {
          "type": "PROCEDURE_DIVISION_BODY", "name": "body", "children": [
            { "type": "PARAGRAPH", "name": "SEARCH-CUSTOMER", "children": [
              { "type": "GENERIC_STATEMENT", "name": "READCUSTOMER-FI",
                "originalText": "READ CUSTOMER-FILE\n NOT INVALID KEY\n CALL 'CUSTOMER-DISPLAY' USING CUSTOMER-RECORD\n END-READ." }
            ]}
          ]
        }
        """);

        ctx.CallTargets.Select(c => c.TargetProgram)
           .Should().ContainSingle().Which.Should().Be("CUSTOMER-DISPLAY");
    }

    [Theory]
    [InlineData("PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10")]
    [InlineData("PERFORM UNTIL WS-EOF = 'Y'")]
    [InlineData("PERFORM 10 TIMES")]
    [InlineData("PERFORM WITH TEST AFTER UNTIL WS-DONE")]
    public void Load_InlinePerformDoesNotInventAProcedureTarget(string statement)
    {
        var ctx = LoadAst($$"""
        {
          "type": "PROCEDURE_DIVISION_BODY", "name": "body", "children": [
            { "type": "PARAGRAPH", "name": "MAIN-LOGIC", "children": [
              { "type": "SENTENCE", "name": "PERFORMVARYINGW", "originalText": "{{statement}}" }
            ]}
          ]
        }
        """);

        ctx.PerformGraph.Should().BeEmpty();
    }

    [Fact]
    public void Load_FindsCallNestedInsideInlinePerform()
    {
        var ctx = LoadAst("""
        {
          "type": "PROCEDURE_DIVISION_BODY", "name": "body", "children": [
            { "type": "PARAGRAPH", "name": "MAIN-LOGIC", "children": [
              { "type": "SENTENCE", "name": "PERFORMUNTILWS",
                "originalText": "PERFORM UNTIL WS-EOF = 'Y' CALL 'FORMAT-BALANCE' END-PERFORM." }
            ]}
          ]
        }
        """);

        ctx.CallTargets.Should().ContainSingle().Which.TargetProgram.Should().Be("FORMAT-BALANCE");
    }

    [Fact]
    public void Load_IgnoresDynamicCallThroughVariable()
    {
        var ctx = LoadAst("""
        {
          "type": "PROCEDURE_DIVISION_BODY", "name": "body", "children": [
            { "type": "PARAGRAPH", "name": "MAIN-LOGIC", "children": [
              { "type": "GENERIC_STATEMENT", "name": "CALLWS-PROGRAM",
                "originalText": "CALL WS-PROGRAM-NAME USING WS-ARGS." }
            ]}
          ]
        }
        """);

        ctx.CallTargets.Should().BeEmpty();
    }

    [Fact]
    public void Load_MarksCallNodeThroughVariableAsDynamicRatherThanAProgram()
    {
        var ctx = LoadAst("""
        {
          "type": "PROCEDURE_DIVISION_BODY", "name": "body", "children": [
            { "type": "PARAGRAPH", "name": "MAIN-LOGIC", "children": [
              { "type": "CALL", "name": "CALLLIT-DSNTIAC",
                "originalText": "CALL LIT-DSNTIAC USING SQLCA" },
              { "type": "CALL", "name": "CALL'SUBPROG",
                "originalText": "CALL 'SUBPROG' USING SQLCA" }
            ]}
          ]
        }
        """);

        ctx.CallTargets.Select(c => (c.TargetProgram, c.IsDynamic))
            .Should().BeEquivalentTo(new[] { ("LIT-DSNTIAC", true), ("SUBPROG", false) });
    }

    [Fact]
    public void Load_DoesNotDuplicateCallReportedByBothSentenceAndCallNode()
    {
        var ctx = LoadAst("""
        {
          "type": "PROCEDURE_DIVISION_BODY", "name": "body", "children": [
            { "type": "PARAGRAPH", "name": "MAIN-LOGIC", "children": [
              { "type": "SENTENCE", "name": "CALL'FORMAT-BAL",
                "originalText": "CALL 'FORMAT-BALANCE' USING CUST-BALANCE.",
                "children": [
                  { "type": "CALL", "name": "CALL'FORMAT-BAL",
                    "originalText": "CALL 'FORMAT-BALANCE' USING CUST-BALANCE" }
                ]}
            ]}
          ]
        }
        """);

        ctx.CallTargets.Should().ContainSingle();
    }

    private RektContext LoadAst(string flowAstJson)
    {
        var rektDir = Path.Combine(_root, "rekt");
        Directory.CreateDirectory(rektDir);
        File.WriteAllText(Path.Combine(rektDir, "flow-ast-PROG.cbl.json"), flowAstJson);

        return new RektContextLoader(_root, rektDir).Load("PROG.cbl", "source");
    }

    [Fact]
    public void Load_DoesNotAnswerANestedProgramFromTheFlatLayout()
    {
        // Only a flat artifact exists, named by basename alone, so nothing in it says whether it
        // describes the billing or the claims copy. Answering either request from it gives both
        // programs the same paragraphs, PERFORM edges and CALL targets.
        var rektDir = Path.Join(_root, "rekt");
        Directory.CreateDirectory(rektDir);
        File.WriteAllText(
            Path.Join(rektDir, "flow-ast-CUSTOMER.json"),
            """
            {
              "type": "PROCEDURE_DIVISION_BODY", "name": "body", "children": [
                { "type": "PARAGRAPH", "name": "BILLING-ONLY-PARAGRAPH", "children": [] }
              ]
            }
            """);

        var loader = new RektContextLoader(_root, rektDir);

        var nested = loader.Load("billing/CUSTOMER.cbl", "source");
        nested.Sections.SelectMany(s => s.Paragraphs).Should().BeEmpty(
            "a flat artifact cannot be attributed to one of several programs sharing its basename");

        // A caller naming only the basename expresses no preference, so the flat layout still
        // answers it and estates that were never nested keep working.
        var flat = loader.Load("CUSTOMER.cbl", "source");
        flat.Sections.SelectMany(s => s.Paragraphs).Select(p => p.Name)
            .Should().Contain("BILLING-ONLY-PARAGRAPH");
    }

    [Fact]
    public void Load_FindsANestedReportWhenAskedByBasename()
    {
        // The conversion identifies a program by file name, while the parser writes its report
        // under the program's source-relative path. On an estate that stages programs in
        // subdirectories the two never met, so every such program was reported as having no
        // structural context and parity could not score it.
        var rektDir = Path.Join(_root, "rekt");
        var reportDir = Path.Join(rektDir, "LEGACY", "PAYGR005.cbl.report", "flow_ast");
        Directory.CreateDirectory(reportDir);
        File.WriteAllText(
            Path.Join(reportDir, "flow-ast-PAYGR005.cbl.json"),
            """
            {
              "type": "PROCEDURE_DIVISION_BODY", "name": "body", "children": [
                { "type": "PARAGRAPH", "name": "MAIN-LOGIC", "children": [] }
              ]
            }
            """);

        var ctx = new RektContextLoader(_root, rektDir).Load("PAYGR005.cbl", "source");

        ctx.Sections.SelectMany(s => s.Paragraphs).Select(p => p.Name)
            .Should().Contain("MAIN-LOGIC");
    }

    [Fact]
    public void Load_DoesNotGuessBetweenTwoNestedReportsSharingABasename()
    {
        // Two programs with the same basename, each with its own report. A request naming only the
        // basename cannot say which is meant, and answering with either would give one program the
        // other's paragraphs. Absent structural context is the safe answer.
        var rektDir = Path.Join(_root, "rekt");
        foreach (var (dir, para) in new[] { ("billing", "BILLING-ONLY"), ("claims", "CLAIMS-ONLY") })
        {
            var reportDir = Path.Join(rektDir, dir, "CUSTOMER.cbl.report", "flow_ast");
            Directory.CreateDirectory(reportDir);
            File.WriteAllText(
                Path.Join(reportDir, "flow-ast-CUSTOMER.cbl.json"),
                $$"""
                {
                  "type": "PROCEDURE_DIVISION_BODY", "name": "body", "children": [
                    { "type": "PARAGRAPH", "name": "{{para}}", "children": [] }
                  ]
                }
                """);
        }

        var ctx = new RektContextLoader(_root, rektDir).Load("CUSTOMER.cbl", "source");

        ctx.Sections.SelectMany(s => s.Paragraphs).Should().BeEmpty();
    }

    public void Dispose()
    {
        if (Directory.Exists(_root))
            Directory.Delete(_root, recursive: true);
    }
}
