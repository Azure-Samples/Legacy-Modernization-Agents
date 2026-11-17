using System.Text.Json;
using CobolToQuarkusMigration.Persistence.GraphIngest;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Persistence.GraphIngest;

// Runs ingestion against the in-memory target, which applies the same MERGE/MATCH rules as the
// Cypher, so ordering and missing-endpoint behavior show up as they would in Neo4j.
public sealed class GraphIngestionTests : IDisposable
{
    private readonly string _root = Directory.CreateDirectory(
        Path.Join(Path.GetTempPath(), $"graph-ingest-{Guid.NewGuid():N}")).FullName;
    private readonly RecordingGraphTarget _graph = new();
    private readonly StringWriter _log = new();
    private readonly GraphIngestion _ingestion;

    public GraphIngestionTests() => _ingestion = new GraphIngestion(_graph, _log);

    public void Dispose() => Directory.Delete(_root, recursive: true);

    [Fact]
    public async Task MissingEndpointsAreReportedOnceAsATotalAndNotCountedAsMerged()
    {
        await _ingestion.MergeNodesAsync("ASTNode", [Row("uid", "start"), Row("uid", "end")], "uid");

        var matched = await _ingestion.MergeRelationshipsAsync(RelationshipBatch.ByUid("ASTNode", "FOLLOWED_BY", "ASTNode",
            [RektGraphRows.Link("start", "end"), RektGraphRows.Link("missing", "end")]));
        await _ingestion.MergeRelationshipsAsync(RelationshipBatch.ByUid("ASTNode", "FOLLOWED_BY", "ASTNode",
            [RektGraphRows.Link("start", "gone")]));
        _ingestion.ReportSkippedRelationships();

        matched.Should().Be(1);
        _log.ToString().Split('\n', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries)
            .Should().ContainSingle().Which.Should().Be("Skipped 2 FOLLOWED_BY relationship rows with missing endpoints");
    }

    [Fact]
    public async Task OccupiedRunIsRejectedWithoutWritingScanMetadata()
    {
        // JCL-only ingests leave only JclNode data behind.
        await _ingestion.MergeNodesAsync("JclNode", [new() { ["uid"] = "j", ["runId"] = 7L }], "uid");

        var act = () => _ingestion.BeginRektScanAsync(7, "snapshot-v2", _root, _root, null);

        await act.Should().ThrowAsync<GraphIngestException>().WithMessage("*already contains data*");
        _graph.Nodes.Should().NotContain(n => n.Label == "RektScan");
    }

    [Fact]
    public async Task FreshScanIsNamedWithoutCreatingAMigrationRun()
    {
        await _ingestion.BeginRektScanAsync(8, "  snapshot-v2 ", _root, _root, null);

        var scan = _graph.Nodes.Should().ContainSingle().Which;
        scan.Label.Should().Be("RektScan");
        scan.Properties["name"].Should().Be("snapshot-v2");
        scan.Properties["id"].Should().Be(8L);
        scan.Properties.Should().NotContainKey("manifestPath");
    }

    [Fact]
    public async Task NamedScanRecordsItsManifestBesideTheArtifacts()
    {
        var folder = Path.Join(_root, "snapshot-v2");
        var parsed = Directory.CreateDirectory(Path.Join(folder, "inputs", "parsed-source")).FullName;
        var artifacts = Directory.CreateDirectory(Path.Join(folder, "artifacts")).FullName;
        File.WriteAllText(Path.Join(folder, "scan.json"), "{}");
        var explicitManifest = Path.Join(_root, "elsewhere.json");
        File.WriteAllText(explicitManifest, "{}");

        await _ingestion.BeginRektScanAsync(8, "snapshot-v2", parsed, artifacts, null);
        await _ingestion.BeginRektScanAsync(9, "snapshot-v2", parsed, artifacts, explicitManifest);

        _graph.Nodes.Select(n => n.Properties.GetValueOrDefault("manifestPath")).Should().Equal(
            GraphIngestion.RealPath(Path.Join(folder, "scan.json")), GraphIngestion.RealPath(explicitManifest));
    }

    [Fact]
    public async Task MissingArtifactDirectoryIsRejectedBeforeAccessingTheGraph()
    {
        var act = () => _ingestion.BeginRektScanAsync(8, "snapshot-v2", _root, Path.Join(_root, "absent"), null);

        await act.Should().ThrowAsync<GraphIngestException>().WithMessage("*existing source and artifact directories*");
        _graph.Nodes.Should().BeEmpty();
    }

    [Fact]
    public async Task NamedScanWithNothingIngestedIsMarkedFailedAndKeepsItsData()
    {
        var source = Directory.CreateDirectory(Path.Join(_root, "source")).FullName;
        var output = Directory.CreateDirectory(Path.Join(_root, "output")).FullName;

        var act = () => _ingestion.RunAsync(new GraphIngestOptions(source, output, 5, ScanName: "empty"));

        await act.Should().ThrowAsync<GraphIngestException>().WithMessage("*no ingested AST or JCL nodes*");
        _graph.ScanStatus[5].Should().Be("failed");
        _log.ToString().Should().Contain("Named scan ingestion failed; partial data has been retained.");
    }

    [Fact]
    public async Task AstKeepsNativeTextVerbatimAndHangsTheRootOffItsFile()
    {
        const string text = "  DISPLAY \"café\".\r\n\tMOVE A TO B.  \n";
        await _ingestion.MergeNodesAsync("CobolFile", [Row("uid", RektGraphRows.Uid(7L, "demo.cbl"))], "uid");
        using var ast = JsonDocument.Parse(JsonSerializer.Serialize(new
        {
            id = "root", type = "PROGRAM", originalText = text, text = "legacy root",
            children = new[] { new { id = "child", type = "DISPLAY", originalText = "DISPLAY \"café\"." } },
        }));

        (await _ingestion.IngestAstAsync("demo.cbl", ast.RootElement, 7)).Should().Be(2);

        var nodes = _graph.Nodes.Where(n => n.Label == "ASTNode").ToList();
        nodes.Select(n => n.Properties["originalText"]).Should().Equal(text, "DISPLAY \"café\".");
        _graph.Relationships.Select(r => (r.Type, r.From.Properties["uid"], r.To.Properties["uid"])).Should().BeEquivalentTo(new[]
        {
            ("CONTAINS", nodes[0].Properties["uid"], nodes[1].Properties["uid"]),
            ("HAS_AST", (object?)RektGraphRows.Uid(7L, "demo.cbl"), nodes[0].Properties["uid"]),
        });
    }

    [Theory]
    [InlineData("""{"originalText": "", "text": "legacy"}""", "")]
    [InlineData("""{"originalText": " \n", "text": "legacy"}""", " \n")]
    [InlineData("""{"originalText": null, "text": " legacy\n"}""", " legacy\n")]
    [InlineData("""{"text": " legacy\n"}""", " legacy\n")]
    [InlineData("""{"originalText": null, "text": ""}""", "")]
    [InlineData("""{"originalText": null, "text": null}""", "")]
    [InlineData("""{"originalText": null}""", "")]
    [InlineData("""{"text": null}""", "")]
    [InlineData("{}", "")]
    public void AstTextFallbackKeepsEmptyStringsAndNormalizesNull(string fields, string expected)
    {
        using var node = JsonDocument.Parse(fields.Insert(1, "\"id\": \"root\"" + (fields == "{}" ? "" : ", ")));
        RektGraphRows.Ast("demo.cbl", node.RootElement, 7).Nodes[0]["originalText"].Should().Be(expected);
    }

    [Fact]
    public void IdlessNativeAndLegacyNodesUseTheSameTextForIdentity()
    {
        using var legacy = JsonDocument.Parse("""{"type": "DISPLAY", "text": "DISPLAY A."}""");
        using var native = JsonDocument.Parse("""{"type": "DISPLAY", "originalText": "DISPLAY A."}""");

        RektGraphRows.Ast("demo.cbl", legacy.RootElement, 7).Nodes[0]["uid"]
            .Should().Be(RektGraphRows.Ast("demo.cbl", native.RootElement, 7).Nodes[0]["uid"]);
    }

    [Fact]
    public async Task DataStructuresAreScopedByRunAndProgramWithTheirEdges()
    {
        using var ds = JsonDocument.Parse("""
            {"records": [
               {"id": "100", "name": "CUSTOMER-REC", "level": 1, "children": [{"id": "200", "name": "CUSTOMER-ID", "level": 5}]},
               {"id": "300", "name": "ORDER-ID", "level": 5},
               {"id": "400", "name": "LEGACY-ORDER-ID", "level": 5}],
             "edges": [{"from": "200", "to": "300", "type": "FLOWS_INTO"}, {"from": "300", "to": "400", "type": "REDEFINES"}]}
            """);

        (await _ingestion.IngestDataStructuresAsync("finance/ACCOUNTS.cbl", ds.RootElement, 7)).Should().Be(4);

        string Uid(string id) => GraphSourcePaths.ScopedGraphId(7, "finance/ACCOUNTS.cbl", id);
        var root = Uid("100");
        _graph.Nodes.Should().Contain(n => Equals(n.Properties["uid"], root) && Equals(n.Properties["id"], "100"));
        _graph.Relationships.Select(r => (r.Type, r.From.Properties["uid"], r.To.Properties["uid"])).Should().BeEquivalentTo(new[]
        {
            ("CONTAINS", (object?)Uid("100"), (object?)Uid("200")),
            ("FLOWS_INTO", Uid("200"), Uid("300")),
            ("REDEFINES", Uid("300"), Uid("400")),
        });
    }

    [Fact]
    public void SameUpstreamIdInAnotherProgramOrRunIsADistinctNode()
    {
        using var ds = JsonDocument.Parse("""{"id": "ROOT", "name": "CUSTOMER-REC", "level": 1}""");
        object? UidOf(string program, long run) => RektGraphRows.DataStructures(program, ds.RootElement, run).Nodes.Single()["uid"];

        new[] { UidOf("finance/ACCOUNTS.cbl", 1), UidOf("archive/ACCOUNTS.cbl", 1), UidOf("finance/ACCOUNTS.cbl", 2) }
            .Should().OnlyHaveUniqueItems();
    }

    [Fact]
    public async Task NativeCfgIsLoadedAfterItsAstEndpoints()
    {
        var (source, output) = Layout();
        // cfg/ sorts before flow_ast/, so a path-ordered load would find no endpoints.
        WriteFile(output, "ACCOUNTS.cbl.report/cfg/cfg-ACCOUNTS.cbl.json", """{"edges":[{"fromNodeID":"start","toNodeID":"end"}]}""");
        WriteFile(output, "ACCOUNTS.cbl.report/flow_ast/flow-ast-ACCOUNTS.cbl.json", """{"id":"start","children":[{"id":"end"}]}""");

        var counts = await _ingestion.IngestRektOutputsAsync(output, source, 8);

        Count(counts, "CFGEdge").Should().Be(1);
        _graph.Relationships.Should().Contain(r => r.Type == "FOLLOWED_BY" && Equals(r.From.Properties["program"], "ACCOUNTS.cbl"));
    }

    [Fact]
    public async Task FileMetadataKeepsUnresolvedDependencyDiagnosticsVisible()
    {
        var (source, output) = Layout();
        WriteFile(output, "ACCOUNTS.cbl.report/dependency-evidence.json",
            """{"unresolvedDiagnostics":["native:unresolved-copybook:ABSENT"]}""");

        await _ingestion.IngestRektOutputsAsync(output, source, 8);

        var file = _graph.Nodes.Single(n => n.Label == "CobolFile").Properties;
        file["dependencyEvidenceAvailable"].Should().Be(true);
        file["dependencyDiagnostics"].Should().BeEquivalentTo(new[] { "native:unresolved-copybook:ABSENT" });
    }

    [Fact]
    public async Task QuarantinedAndWorkInProgressArtifactsAreNotActiveGraphEvidence()
    {
        var (source, output) = Layout();
        foreach (var prefix in new[] { ".previous/old", ".work/current" })
        {
            WriteFile(output, $"{prefix}/ACCOUNTS.cbl.report/flow_ast/flow-ast-ACCOUNTS.cbl.json", """{"id":"OLD"}""");
        }

        Count(await _ingestion.IngestRektOutputsAsync(output, source, 1), "ASTNode").Should().Be(0);
        _graph.Nodes.Should().NotContain(n => n.Label == "ASTNode");
    }

    [Fact]
    public async Task NullArtifactFromADegenerateParseIsSkipped()
    {
        var (source, output) = Layout();
        WriteFile(output, "ACCOUNTS.cbl.report/flow-ast-ACCOUNTS.cbl.json", "null");

        Count(await _ingestion.IngestRektOutputsAsync(output, source, 1), "ASTNode").Should().Be(0);
        _log.ToString().Should().Contain("Skipping empty artifact (degenerate parse): ACCOUNTS.cbl.report/flow-ast-ACCOUNTS.cbl.json");
    }

    [Fact]
    public async Task FactsAndParserDiagnosticsAreNotTreatedAsParseArtifacts()
    {
        var (source, output) = Layout();
        WriteFile(output, "ACCOUNTS.cbl.facts.json", "{}");
        WriteFile(output, "ACCOUNTS.cbl.report/parse-diagnostics.json",
            """[{"code":"missing copybook","severity":"ERROR","message":"ABSENT"}]""");

        var counts = await _ingestion.IngestRektOutputsAsync(output, source, 1);

        _log.ToString().Should().NotContain("Skipping");
        counts.Select(c => c.Key).Should().Equal("CobolFile", "ASTNode", "CFGEdge", "DataStructure", "JclNode");
        counts.Skip(1).Should().OnlyContain(c => c.Value == 0);
    }

    [Fact]
    public async Task JclJobsAreIngestedWithTheirSourceAndNewerSchemasAreSkipped()
    {
        var output = Directory.CreateDirectory(Path.Join(_root, "out")).FullName;
        var source = Directory.CreateDirectory(Path.Join(_root, "src")).FullName;
        WriteFile(source, "jcl/PAYJOB.jcl", "//PAYJOB JOB\n");
        WriteFile(output, "jcl/PAYJOB.jcl.job.json", JclGraphBuilderTests.Facts);
        WriteFile(output, "jcl/NEWER.jcl.job.json", JclGraphBuilderTests.Facts.Replace("\"schemaVersion\": 1", "\"schemaVersion\": 99"));
        WriteFile(output, "jcl-lineage.json", JclGraphBuilderTests.Lineage);

        var total = await _ingestion.IngestJclOutputsAsync(output, source, 3);

        total.Should().Be(_graph.Nodes.Count(n => n.Label == "JclNode")).And.BeGreaterThan(0);
        _graph.Nodes.Should().OnlyContain(n => n.Label == "SourceBlock" || Equals(n.Properties["job"], "PAYJOB"));
        _graph.Relationships.Should().Contain(r => r.Type == "FEEDS" && Equals(r.Properties["label"], "PAY.IN"));
        _graph.Nodes.Single(n => n.Label == "SourceBlock").Properties["program"].Should().Be("jcl/PAYJOB.jcl");
        _log.ToString().Should().Contain("Skipping job facts from a newer jcl-facts: jcl/NEWER.jcl.job.json");
    }

    [Fact]
    public void SchemaMigratesDataStructuresToTheUidConstraint()
    {
        GraphSchema.Statements.Should().Contain("DROP CONSTRAINT datastructure_id IF EXISTS");
        GraphSchema.Statements.Should().Contain(s =>
            s.StartsWith("CREATE CONSTRAINT datastructure_uid") && s.EndsWith("FOR (n:DataStructure) REQUIRE n.uid IS UNIQUE"));
        GraphSchema.Statements.Should().NotContain(s => s.Contains("REQUIRE n.id IS UNIQUE") && s.Contains("DataStructure"));
    }

    private (string Source, string Output) Layout()
    {
        var source = Directory.CreateDirectory(Path.Join(_root, "source")).FullName;
        File.WriteAllText(Path.Join(source, "ACCOUNTS.cbl"), "IDENTIFICATION DIVISION.\n");
        return (source, Directory.CreateDirectory(Path.Join(_root, "output")).FullName);
    }

    private static void WriteFile(string root, string relative, string content)
    {
        var path = Path.Join(root, relative);
        Directory.CreateDirectory(Path.GetDirectoryName(path)!);
        File.WriteAllText(path, content);
    }

    private static long Count(List<KeyValuePair<string, long>> counts, string key) => counts.Single(c => c.Key == key).Value;

    private static Dictionary<string, object?> Row(string key, object value) => new() { [key] = value };
}
