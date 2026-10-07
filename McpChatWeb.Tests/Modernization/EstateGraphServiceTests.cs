using System;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http.Json;
using System.Text.Json;
using System.Threading.Tasks;
using CobolToQuarkusMigration.Estate;
using McpChatWeb.Services;
using Microsoft.Extensions.Logging.Abstractions;
using Xunit;

namespace McpChatWeb.Tests.Modernization;

public sealed class EstateGraphServiceTests : IDisposable
{
    private readonly EstateFixture _fixture = new();

    public void Dispose() => _fixture.Dispose();

    private EstateGraphService Service() => new(
        new RektEstateReader(NullLogger<RektEstateReader>.Instance, _fixture.RepoRoot),
        new ConversionParityReader(NullLogger<ConversionParityReader>.Instance, _fixture.RepoRoot),
        NullLogger<EstateGraphService>.Instance);

    private static string Cobol(string id, params string[] calls) =>
        string.Join("\n", new[] { $"       PROGRAM-ID. {id}.", "       PROCEDURE DIVISION." }
            .Concat(calls.Select(c => $"           CALL '{c}'."))) + "\n";

    private void Estate()
    {
        _fixture.AddProgram("ORDA.cbl", Cobol("ORDA", "ORDB", "ORDC"));
        _fixture.AddProgram("ORDB.cbl", Cobol("ORDB", "ORDC"));
        _fixture.AddProgram("ORDC.cbl", Cobol("ORDC", "GHOST"));
        _fixture.AddJcl("ORDJOB.jcl", "//ORDJOB   JOB (A),'X'\n//S1       EXEC PGM=ORDA\n//S2       EXEC PGM=ORDB\n");
    }

    [Fact]
    public async Task TheSummaryHasClustersWavesAndWhichClusterEachProgramIsIn()
    {
        Estate();

        var summary = await Service().GetSummaryAsync();

        var cluster = Assert.Single(summary.Clusters);
        Assert.Equal(["program:ORDA", "program:ORDB", "program:ORDC"], cluster.Programs);
        Assert.Contains("GHOST", cluster.Missing);
        Assert.Equal(cluster.Id, summary.ClusterOf["program:ORDB"]);
        Assert.Equal(1, Assert.Single(summary.Waves).Number);
        Assert.Null(summary.Warning);
    }

    [Fact]
    public async Task ProgramsCarryWhatRektKnowsAboutThem()
    {
        Estate();
        _fixture.AddFacts("ORDA.cbl", confidence: 87);

        var summary = await Service().GetSummaryAsync();

        Assert.Equal(87, summary.Programs["program:ORDA"].FactsConfidence);
        Assert.Null(summary.Programs["program:ORDB"].FactsConfidence);
    }

    [Fact]
    public async Task TheGraphIsBuiltOnceAndRebuiltWhenTheSourceChanges()
    {
        Estate();
        var service = Service();

        var (first, _) = await service.GetGraphAsync();
        var (second, _) = await service.GetGraphAsync();
        Assert.Same(first, second);

        _fixture.AddProgram("ORDD.cbl", Cobol("ORDD", "ORDA"));
        var (third, _) = await service.GetGraphAsync();
        Assert.NotSame(first, third);
        Assert.Contains(third.Nodes, n => n.Id == "program:ORDD");
    }

    [Fact]
    public async Task RebuildDropsTheCachedGraph()
    {
        Estate();
        var service = Service();

        var (first, _) = await service.GetGraphAsync();
        var (rebuilt, _) = await service.RebuildAsync();

        Assert.NotSame(first, rebuilt);
        Assert.Equal(first.Counts, rebuilt.Counts);
    }

    [Fact]
    public async Task TheMissionViewCarriesProgramsStatusAndCarveOutClusters()
    {
        Estate();
        _fixture.AddFacts("ORDA.cbl", confidence: 87);

        var mission = await Service().GetMissionAsync();

        Assert.Equal(3, mission.Kpis["programs"]);
        var orda = Assert.Single(mission.Nodes, n => n.Id == "program:ORDA");
        Assert.Equal("batch", orda.Kind);
        Assert.NotNull(orda.Status);
        Assert.Contains(mission.Nodes, n => n.Id == "job:ORDJOB" && n.Type == "job");
        var cluster = Assert.Single(mission.Clusters, c => c.Members.Contains("program:ORDA"));
        Assert.Contains("job:ORDJOB", cluster.EntryPoints);
        Assert.Contains("GHOST", cluster.Missing);
        Assert.Equal(cluster.Id, cluster.SliceId);
    }

    [Fact]
    public async Task ANodeComesWithTheSourceLinesBehindEachEdge()
    {
        Estate();

        var node = await Service().GetNodeAsync("program:ORDB");

        Assert.NotNull(node);
        var call = Assert.Single(node!.Outgoing, e => e.Kind == EstateEdgeKind.Calls);
        Assert.Equal("program:ORDC", call.To);
        Assert.Equal(new EstateEvidence("ORDB.cbl", 3, "CALL 'ORDC'."), Assert.Single(call.Evidence));
        Assert.Contains(node.Incoming, e => e.From == "job:ORDJOB" && e.Kind == EstateEdgeKind.Runs);
        Assert.NotNull(node.Cluster);
    }

    [Fact]
    public async Task ASliceGivesTheSelectorsAndTheDoctorCommandThatConvertsIt()
    {
        Estate();
        var service = Service();
        var id = (await service.GetSummaryAsync()).Clusters.Single().Id;

        var slice = await service.GetSliceAsync(id);

        Assert.NotNull(slice);
        Assert.Equal(["ORDA.cbl", "ORDB.cbl", "ORDC.cbl"], slice!.Selectors);
        Assert.Equal("./doctor.sh convert-only --program ORDA.cbl,ORDB.cbl,ORDC.cbl", slice.Command);
        Assert.Equal(["ORDJOB"], slice.Slice.Jobs);
        Assert.Null(await service.GetSliceAsync("NOPE"));
    }

    [Fact]
    public async Task OneClustersViewHasItsProgramsAndTheirNeighboursButNotTheRest()
    {
        Estate();
        _fixture.AddProgram("LONER.cbl", Cobol("LONER"));
        var service = Service();
        var id = (await service.GetSummaryAsync()).Clusters.Single(c => !c.Standalone).Id;

        var view = await service.GetGraphViewAsync(id);

        Assert.NotNull(view);
        Assert.Contains(view!.Nodes, n => n.Id == "program:GHOST");
        Assert.Contains(view.Nodes, n => n.Id == "job:ORDJOB");
        Assert.DoesNotContain(view.Nodes, n => n.Id == "program:LONER");
        Assert.All(view.Edges, e => Assert.True(e.EvidenceCount > 0));
        Assert.Null(await service.GetGraphViewAsync("NOPE"));
    }

    [Fact]
    public async Task NoSourceFolderIsAWarningNotAnError()
    {
        Directory.Delete(_fixture.SourceRoot, recursive: true);

        var summary = await Service().GetSummaryAsync();

        Assert.Empty(summary.Clusters);
        Assert.Contains("source", summary.Warning);
    }

    [Fact]
    public void TheRunUsesTheSourceFolderTheGraphWasBuiltFrom() =>
        Assert.Equal("source", Service().SourceFolderForRun);
}

public class ProcessManagerProgramSelectionTests
{
    [Fact]
    public void NoSelectionConvertsEverything()
    {
        Assert.Empty(ProcessManager.ValidatePrograms("convert-only", null));
        Assert.Empty(ProcessManager.ValidatePrograms("convert-only", ["", "  "]));
    }

    [Fact]
    public void NamesAndSourceRelativePathsAreAcceptedOnceEach() =>
        Assert.Equal(["PAY#01.cbl", "batch/sub/RPT$1.cbl"],
            ProcessManager.ValidatePrograms("migrate", ["PAY#01.cbl", " batch/sub/RPT$1.cbl ", "pay#01.CBL"]));

    [Theory]
    [InlineData("../etc/passwd")]
    [InlineData("a/../../b.cbl")]
    [InlineData("./a.cbl")]
    [InlineData("/abs/a.cbl")]
    [InlineData("--resume")]
    [InlineData("a.cbl,b.cbl")]
    [InlineData("a.cbl;rm -rf")]
    [InlineData("a b.cbl")]
    public void AnythingElseIsRejected(string selector) =>
        Assert.Throws<ArgumentException>(() => ProcessManager.ValidatePrograms("convert-only", [selector]));

    [Theory]
    [InlineData("reverse-engineer")]
    [InlineData("resume")]
    public void CommandsThatDoNotConvertTakeNoSelection(string command) =>
        Assert.Throws<ArgumentException>(() => ProcessManager.ValidatePrograms(command, ["A.cbl"]));
}

public class EstateEndpointsTests : IClassFixture<Integration.WebAppFactory>
{
    private readonly Integration.WebAppFactory _factory;

    public EstateEndpointsTests(Integration.WebAppFactory factory) => _factory = factory;

    [Fact]
    public async Task TheSummaryIsJson()
    {
        var payload = await _factory.CreateClient().GetFromJsonAsync<JsonElement>("/api/estate/summary");

        Assert.Equal(JsonValueKind.Array, payload.GetProperty("clusters").ValueKind);
        Assert.Equal(JsonValueKind.Array, payload.GetProperty("waves").ValueKind);
        Assert.Equal(JsonValueKind.Object, payload.GetProperty("counts").ValueKind);
    }

    [Fact]
    public async Task TheMissionIsJson()
    {
        var payload = await _factory.CreateClient().GetFromJsonAsync<JsonElement>("/api/estate/mission");

        Assert.Equal(JsonValueKind.Array, payload.GetProperty("nodes").ValueKind);
        Assert.Equal(JsonValueKind.Array, payload.GetProperty("edges").ValueKind);
        Assert.Equal(JsonValueKind.Array, payload.GetProperty("clusters").ValueKind);
        Assert.Equal(JsonValueKind.Object, payload.GetProperty("kpis").ValueKind);
    }

    [Fact]
    public async Task RebuildReturnsTheFreshCounts()
    {
        var response = await _factory.CreateClient().PostAsync("/api/estate/rebuild", null);

        Assert.Equal(HttpStatusCode.OK, response.StatusCode);
        var payload = await response.Content.ReadFromJsonAsync<JsonElement>();
        Assert.Equal(JsonValueKind.Object, payload.GetProperty("counts").ValueKind);
    }

    [Theory]
    [InlineData("/api/estate/cluster/NO-SUCH-CLUSTER/slice")]
    [InlineData("/api/estate/node/program:NO-SUCH-PROGRAM")]
    [InlineData("/api/estate/graph?cluster=NO-SUCH-CLUSTER")]
    public async Task UnknownIdsAreNotFound(string url) =>
        Assert.Equal(HttpStatusCode.NotFound, (await _factory.CreateClient().GetAsync(url)).StatusCode);

    [Fact]
    public async Task ConvertingAnUnknownClusterStartsNothing()
    {
        var response = await _factory.CreateClient().PostAsJsonAsync("/api/estate/slice/convert", new { clusterId = "NO-SUCH-CLUSTER" });

        Assert.Equal(HttpStatusCode.NotFound, response.StatusCode);
    }

    [Fact]
    public async Task ConvertingWithoutAClusterIsABadRequest()
    {
        var response = await _factory.CreateClient().PostAsJsonAsync("/api/estate/slice/convert", new { clusterId = "" });

        Assert.Equal(HttpStatusCode.BadRequest, response.StatusCode);
    }
}
