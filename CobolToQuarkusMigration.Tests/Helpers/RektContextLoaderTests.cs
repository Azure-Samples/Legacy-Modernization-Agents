using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

public sealed class RektContextLoaderTests : IDisposable
{
    private readonly string _root =
        Path.Combine(Path.GetTempPath(), $"rekt-loader-{Guid.NewGuid():N}");

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

    public void Dispose()
    {
        if (Directory.Exists(_root))
            Directory.Delete(_root, recursive: true);
    }
}
