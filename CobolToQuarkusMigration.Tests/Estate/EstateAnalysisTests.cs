using CobolToQuarkusMigration.Estate;
using FluentAssertions;
using Xunit;
using static CobolToQuarkusMigration.Tests.Estate.EstateSourceScannerTests;

namespace CobolToQuarkusMigration.Tests.Estate;

public sealed class EstateAnalysisTests : IDisposable
{
    private readonly string _root = Directory.CreateTempSubdirectory("estate-analysis-").FullName;

    public void Dispose() => Directory.Delete(_root, recursive: true);

    private void Program(string name, params string[] calls) =>
        File.WriteAllText(Path.Join(_root, name + ".cbl"),
            Cobol([$"PROGRAM-ID. {name}.", "PROCEDURE DIVISION.", .. calls.Select(c => $"    CALL '{c}'.")]));

    private void Job(string name, params string[] programs) =>
        File.WriteAllText(Path.Join(_root, name + ".jcl"), string.Join("\n",
            [$"//{name,-8} JOB (ACCT),'X'", .. programs.Select((p, i) => $"//STEP{i + 1,-4} EXEC PGM={p}"), ""]));

    // Two tight triangles, ORD* and INV*, with one call from the first into the second.
    private void TwoTriangles(bool backCall = false)
    {
        Program("ORDA", "ORDB", "ORDC", "INVA");
        Program("ORDB", "ORDC");
        Program("ORDC");
        Program("INVA", "INVB", "INVC");
        Program("INVB", "INVC");
        Program("INVC", backCall ? ["ORDC"] : []);
    }

    private static EstateCluster ClusterOf(EstateGraph g, string program) =>
        g.Clusters.Single(c => c.Programs.Contains("program:" + program));

    [Fact]
    public void LouvainSplitsTwoTrianglesJoinedByOneEdge()
    {
        var adj = new Dictionary<int, Dictionary<int, double>>();
        void Link(int a, int b) { (adj.TryGetValue(a, out var x) ? x : adj[a] = [])[b] = 1; (adj.TryGetValue(b, out var y) ? y : adj[b] = [])[a] = 1; }
        Link(0, 1); Link(1, 2); Link(0, 2); Link(3, 4); Link(4, 5); Link(3, 5); Link(2, 3);

        var membership = EstateAnalysis.Louvain(6, adj, 1.0);

        membership[0].Should().Be(membership[1]).And.Be(membership[2]);
        membership[3].Should().Be(membership[4]).And.Be(membership[5]);
        membership[0].Should().NotBe(membership[3]);
    }

    [Fact]
    public void ClustersFollowTheCallGraphAndAreNamedByTheirSharedPrefix()
    {
        TwoTriangles();

        var g = EstateGraphBuilder.Build(_root);

        g.Clusters.Should().HaveCount(2);
        ClusterOf(g, "ORDA").Label.Should().Be("ORD*");
        ClusterOf(g, "INVA").Programs.Should().BeEquivalentTo("program:INVA", "program:INVB", "program:INVC");
    }

    [Fact]
    public void ACalleeClusterComesInAnEarlierWaveThanItsCaller()
    {
        TwoTriangles();

        var g = EstateGraphBuilder.Build(_root);

        var (ord, inv) = (ClusterOf(g, "ORDA"), ClusterOf(g, "INVA"));
        ord.DependsOn.Should().Equal(inv.Id);
        inv.DependedOnBy.Should().Equal(ord.Id);
        inv.Wave.Should().Be(1);
        ord.Wave.Should().Be(2);
        g.Waves.Select(w => w.Number).Should().Equal(1, 2);
    }

    [Fact]
    public void ClustersThatCallEachOtherShareAWaveAndTheCycleIsReported()
    {
        TwoTriangles(backCall: true);

        var g = EstateGraphBuilder.Build(_root);

        var (ord, inv) = (ClusterOf(g, "ORDA"), ClusterOf(g, "INVA"));
        ord.Wave.Should().Be(inv.Wave);
        g.Waves.Single().Cycles.Single().Should().BeEquivalentTo(ord.Id, inv.Id);
    }

    [Fact]
    public void AProgramEverythingCallsIsAHubAndDoesNotMergeItsCallers()
    {
        TwoTriangles();
        Program("ORDD", "ORDA", "UTIL");
        Program("INVD", "INVA", "UTIL");
        Program("ORDE", "ORDB", "UTIL");
        Program("INVE", "INVB", "UTIL");
        Program("INVF", "INVC", "UTIL");
        Program("UTIL");

        // A toy graph is too small for the percentile alone to separate UTIL from busy members.
        var g = EstateGraphBuilder.Build(_root, options: new EstateGraphOptions { HubMinDegree = 5 });

        g.Hubs.Should().Contain(h => h.NodeId == "program:UTIL" && h.FanIn == 5);
        g.Hubs.Select(h => h.NodeId).Should().Equal("program:UTIL");
        ClusterOf(g, "ORDD").Id.Should().Be(ClusterOf(g, "ORDA").Id);
        ClusterOf(g, "INVD").Id.Should().Be(ClusterOf(g, "INVA").Id);
        ClusterOf(g, "ORDD").Id.Should().NotBe(ClusterOf(g, "INVD").Id);
    }

    [Fact]
    public void AProgramWithNoRelationshipsIsStandalone()
    {
        TwoTriangles();
        Program("LONER");

        var g = EstateGraphBuilder.Build(_root);

        var c = ClusterOf(g, "LONER");
        c.Id.Should().Be(EstateAnalysis.StandaloneClusterId);
        c.Standalone.Should().BeTrue();
    }

    [Fact]
    public void ASelfContainedCompleteClusterScoresHigherThanOneThatReachesOut()
    {
        TwoTriangles();
        Program("ORDC", "MISSING1");

        var g = EstateGraphBuilder.Build(_root);

        var (ord, inv) = (ClusterOf(g, "ORDA"), ClusterOf(g, "INVA"));
        inv.ScoreBreakdown["completeness"].Should().Be(1);
        inv.ScoreBreakdown["independence"].Should().Be(1);
        ord.ScoreBreakdown["independence"].Should().BeLessThan(1);
        ord.Missing.Should().Equal("MISSING1");
        inv.CarveScore.Should().BeGreaterThan(ord.CarveScore);
    }

    [Fact]
    public void ASliceNeedsWhatItsProgramsCallAndRunsOnlyTheJobsItFullyCovers()
    {
        TwoTriangles();
        Program("OTHER");
        Program("SOLO", "OTHER");
        Job("ORDJOB", "ORDA", "ORDB");
        Job("MIXJOB", "ORDA", "SOLO");

        var g = EstateGraphBuilder.Build(_root);
        var slice = EstateAnalysis.Slice(g, ClusterOf(g, "ORDA").Id)!;

        slice.Needs.Should().BeEquivalentTo("program:INVA", "program:INVB", "program:INVC");
        slice.Jobs.Should().Equal("ORDJOB");
        slice.ProgramSelectors.Should().BeEquivalentTo("ORDA.cbl", "ORDB.cbl", "ORDC.cbl");
        slice.NeedSelectors.Should().BeEquivalentTo("INVA.cbl", "INVB.cbl", "INVC.cbl");
    }

    [Fact]
    public void AnUnknownClusterHasNoSlice()
    {
        TwoTriangles();
        EstateAnalysis.Slice(EstateGraphBuilder.Build(_root), "C99").Should().BeNull();
    }

    [Theory]
    [InlineData(new[] { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 }, 0.9, 9)]
    [InlineData(new[] { 5 }, 0.9, 5)]
    [InlineData(new[] { 3, 1, 2 }, 0.5, 2)]
    public void PercentileIsTheNearestRank(int[] values, double p, double expected) =>
        EstateAnalysis.Percentile(values, p).Should().Be(expected);
}
