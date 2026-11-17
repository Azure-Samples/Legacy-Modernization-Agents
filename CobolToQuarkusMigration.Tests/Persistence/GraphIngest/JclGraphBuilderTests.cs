using System.Text.Json;
using CobolToQuarkusMigration.Persistence.GraphIngest;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Persistence.GraphIngest;

public sealed class JclGraphBuilderTests
{
    internal const string Facts = """
        {
          "schemaVersion": 1,
          "job": {
            "name": "PAYJOB", "file": "jcl/PAYJOB.jcl", "jobClass": "A",
            "steps": [
              {"name": "SORT1", "line": 2, "kind": "Utility", "program": "SORT",
               "controlStatements": ["SORT FIELDS=(1,8,CH,A)"]},
              {"name": "RUN1.CALC", "line": 3, "kind": "Program", "program": "PAYCALC",
               "procedure": "PAYPROC", "procedureStep": "CALC", "parm": "X",
               "conditions": [{"expression": "SORT1.RC = 0", "negated": false}]},
              {"name": "DB2", "line": 9, "kind": "TsoBatch", "program": "IKJEFT01",
               "runs": [{"program": "PAYDB2", "plan": "PAYPLAN"}]},
              {"name": "MISSING", "line": 12, "kind": "UnresolvedProcedure", "procedure": "NOPROC"}
            ],
            "diagnostics": [{"line": 12, "code": "PROC_NOT_FOUND", "message": "x"}]
          },
          "programs": ["PAYCALC", "PAYDB2"],
          "upstream": ["FEEDJOB"],
          "downstream": []
        }
        """;

    internal const string Lineage = """
        {
          "datasets": [
            {"dataset": "PAY.IN", "temporary": false, "uses": [
              {"job": "PAYJOB", "step": "SORT1", "access": "Read", "via": "SORTIN"},
              {"job": "FEEDJOB", "step": "S1", "access": "Create", "via": "OUT"}]},
            {"dataset": "PAYJOB:&&WORK", "temporary": true, "uses": [
              {"job": "PAYJOB", "step": "SORT1", "access": "Create", "via": "SORTOUT"},
              {"job": "PAYJOB", "step": "RUN1.CALC", "access": "Read", "via": "IN"}]},
            {"dataset": "PAY.HIST", "temporary": false, "uses": [
              {"job": "PAYJOB", "step": "RUN1.CALC", "access": "Append", "generation": "+1", "via": "HIST"}]}
          ],
          "dependencies": [{"upstream": "FEEDJOB", "downstream": "PAYJOB", "datasets": ["PAY.IN"]}],
          "externalInputs": []
        }
        """;

    private readonly Dictionary<string, Dictionary<string, object?>> _nodes;
    private readonly Dictionary<(string, string, string), string> _edges;

    public JclGraphBuilderTests()
    {
        (_nodes, _edges) = Build(withLineage: true, runId: 7);
    }

    [Fact]
    public void NodesAreScopedToTheRunAndTheJobFile()
    {
        var job = _nodes["JOB"];
        job["uid"].Should().Be(GraphSourcePaths.ScopedGraphId(7, "jcl/PAYJOB.jcl", "JOB"));
        (job["nodeType"], job["name"], job["program"]).Should().Be(("JCL_JOB", "PAYJOB", "jcl/PAYJOB.jcl"));
        job["originalText"].Should().Be("//PAYJOB JOB CLASS=A\n1 diagnostic(s): PROC_NOT_FOUND ×1");
    }

    [Fact]
    public void StepsFollowEachOtherAndSayWhatTheyRun()
    {
        new[] { "SORT1", "RUN1.CALC", "DB2", "MISSING" }.Select(s => _nodes[$"STEP:{s}"]["nodeType"])
            .Should().Equal("JCL_UTILITY_STEP", "JCL_STEP", "JCL_TSO_STEP", "JCL_UNRESOLVED_STEP");
        _edges.Should().ContainKey(("STEP:SORT1", "STEP:RUN1.CALC", "FOLLOWED_BY"));
        _edges.Should().ContainKey(("JOB", "STEP:DB2", "CONTAINS"));
        _nodes["PGM:SORT"]["nodeType"].Should().Be("JCL_UTILITY");
        _nodes["PGM:IKJEFT01"]["nodeType"].Should().Be("JCL_UTILITY");
        _nodes["PGM:PAYDB2"]["nodeType"].Should().Be("JCL_PROGRAM");
        _edges[("STEP:DB2", "PGM:PAYDB2", "RUNS")].Should().Be("RUN PROGRAM PLAN(PAYPLAN)");
        _nodes["STEP:RUN1.CALC"]["originalText"].Should().Be(
            "EXEC PGM=PAYCALC PARM='X'\nProcedure PAYPROC, step CALC\nIF SORT1.RC = 0");
        _nodes["STEP:MISSING"]["originalText"].Should().Be("Procedure NOPROC (not in the source)");
    }

    [Fact]
    public void OnlyStepsWrittenInTheFileCarryItsLines()
    {
        _nodes["STEP:SORT1"]["startLine"].Should().Be(2L);
        _nodes["STEP:RUN1.CALC"]["startLine"].Should().Be(-1L);
    }

    [Fact]
    public void DatasetsComeFromTheLineageForThisJobOnly()
    {
        _edges[("STEP:SORT1", "DS:PAY.IN", "READS")].Should().Be("SORTIN");
        _edges[("STEP:RUN1.CALC", "DS:PAY.HIST", "APPENDS")].Should().Be("HIST (+1)");
        var work = _nodes["DS:PAYJOB:&&WORK"];
        (work["nodeType"], work["name"]).Should().Be(("JCL_TEMP_DATASET", "&&WORK"));
        _nodes.Should().NotContainKey("STEP:S1");
    }

    [Fact]
    public void JobsThatFeedItAreLinkedByTheirDatasets()
    {
        _nodes["JOBREF:FEEDJOB"]["nodeType"].Should().Be("JCL_JOB_REF");
        _edges[("JOBREF:FEEDJOB", "JOB", "FEEDS")].Should().Be("PAY.IN");
    }

    [Fact]
    public void WithoutLineageTheJobStillHasItsSteps()
    {
        var (nodes, _) = Build(withLineage: false, runId: 1);
        nodes.Values.Should().NotContain(n => ((string)n["nodeType"]!).EndsWith("DATASET"));
        nodes.Keys.Count(id => id.StartsWith("STEP:")).Should().Be(4);
    }

    [Theory]
    [InlineData("PAYJOB.jcl.job.json", true)]
    [InlineData("jcl-lineage.json", true)]
    [InlineData("flow-ast-PAYCALC.cbl.json", false)]
    public void JclArtifactsAreRecognisedByName(string name, bool expected) =>
        JclGraphBuilder.IsJclArtifact(name).Should().Be(expected);

    private static (Dictionary<string, Dictionary<string, object?>>, Dictionary<(string, string, string), string>) Build(
        bool withLineage, long runId)
    {
        using var facts = JsonDocument.Parse(Facts);
        using var lineage = JsonDocument.Parse(Lineage);
        var (nodes, edges) = JclGraphBuilder.Build(facts.RootElement, withLineage ? lineage.RootElement : null, runId);
        var byUid = nodes.ToDictionary(n => (string)n["uid"]!, n => (string)n["id"]!);
        return (nodes.ToDictionary(n => (string)n["id"]!),
                edges.ToDictionary(e => (byUid[e.FromId], byUid[e.ToId], e.Type), e => e.Label));
    }
}
