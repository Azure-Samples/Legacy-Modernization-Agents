using CobolToQuarkusMigration.Agents.Infrastructure.RektCache;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.RektCache;

public class IncrementalScanPlannerTests : IDisposable
{
    private readonly string _dbPath;
    private readonly SqliteRektScanCache _cache;
    private readonly RektCopybookGraph _graph;
    private readonly IncrementalScanPlanner _planner;

    private const string Scheme = "v1-basename";

    public IncrementalScanPlannerTests()
    {
        _dbPath = Path.Combine(Path.GetTempPath(), $"rekt-planner-{Guid.NewGuid():N}.db");
        _cache = new SqliteRektScanCache(_dbPath, logger: null);
        _graph = new RektCopybookGraph();
        _planner = new IncrementalScanPlanner(_cache, _graph, Scheme, logger: null);
    }

    public void Dispose()
    {
        Microsoft.Data.Sqlite.SqliteConnection.ClearAllPools();
        if (File.Exists(_dbPath)) File.Delete(_dbPath);
        var wal = _dbPath + "-wal"; if (File.Exists(wal)) File.Delete(wal);
        var shm = _dbPath + "-shm"; if (File.Exists(shm)) File.Delete(shm);
    }

    [Fact]
    public async Task EmptyCache_AllProgramsMustParse_WithReasonNotCached()
    {
        _graph.AddFile("PROG.cbl", "       PROGRAM-ID. PROG.", isCopybook: false);

        var plan = await _planner.PlanAsync(new[] { "PROG.cbl" });

        plan.ToParse.Should().HaveCount(1);
        plan.ToParse[0].Reason.Should().Be(ScanReason.NotCached);
        plan.ToSkip.Should().BeEmpty();
    }

    [Fact]
    public async Task UnchangedSource_ProducesSkip()
    {
        _graph.AddFile("PROG.cbl", "PROGRAM-ID. PROG.", isCopybook: false);
        var plan1 = await _planner.PlanAsync(new[] { "PROG.cbl" });
        await _planner.RecordParseAsync(plan1.ToParse[0], RektParseOutcome.Full);

        // Same graph state — should skip.
        var plan2 = await _planner.PlanAsync(new[] { "PROG.cbl" });
        plan2.ToSkip.Should().HaveCount(1);
        plan2.ToParse.Should().BeEmpty();
    }

    [Fact]
    public async Task ChangedSource_InvalidatesAndReParses()
    {
        _graph.AddFile("PROG.cbl", "PROGRAM-ID. PROG.", isCopybook: false);
        var plan1 = await _planner.PlanAsync(new[] { "PROG.cbl" });
        await _planner.RecordParseAsync(plan1.ToParse[0], RektParseOutcome.Full);

        // New graph with a changed program — same name, different content.
        var newGraph = new RektCopybookGraph();
        newGraph.AddFile("PROG.cbl", "PROGRAM-ID. PROG-V2.", isCopybook: false);
        var newPlanner = new IncrementalScanPlanner(_cache, newGraph, Scheme);

        var plan2 = await newPlanner.PlanAsync(new[] { "PROG.cbl" });
        plan2.ToParse.Should().HaveCount(1);
        plan2.ToParse[0].Reason.Should().Be(ScanReason.SourceChanged);
    }

    [Fact]
    public async Task ChangedCopybook_InvalidatesAllDependents()
    {
        _graph.AddFile("BOOK.cpy", "01 WS-A PIC X.", isCopybook: true);
        _graph.AddFile("A.cbl",
            "       PROGRAM-ID. A.\n           COPY BOOK.\n", isCopybook: false);
        _graph.AddFile("B.cbl",
            "       PROGRAM-ID. B.\n           COPY BOOK.\n", isCopybook: false);

        var plan1 = await _planner.PlanAsync(new[] { "A.cbl", "B.cbl" });
        foreach (var d in plan1.ToParse) await _planner.RecordParseAsync(d, RektParseOutcome.Full);

        // Change the copybook content.
        var newGraph = new RektCopybookGraph();
        newGraph.AddFile("BOOK.cpy", "01 WS-A PIC X(20).", isCopybook: true);  // changed
        newGraph.AddFile("A.cbl",
            "       PROGRAM-ID. A.\n           COPY BOOK.\n", isCopybook: false);
        newGraph.AddFile("B.cbl",
            "       PROGRAM-ID. B.\n           COPY BOOK.\n", isCopybook: false);
        var newPlanner = new IncrementalScanPlanner(_cache, newGraph, Scheme);

        var plan2 = await newPlanner.PlanAsync(new[] { "A.cbl", "B.cbl" });
        plan2.ToParse.Should().HaveCount(2);
        plan2.ToParse.Select(d => d.Reason)
            .Should().AllSatisfy(r => r.Should().Be(ScanReason.DependencyChanged));
    }

    [Fact]
    public async Task TransitiveCopybookChange_InvalidatesProgram()
    {
        _graph.AddFile("LEAF.cpy", "01 WS-LEAF PIC X.", isCopybook: true);
        _graph.AddFile("MID.cpy", "           COPY LEAF.", isCopybook: true);
        _graph.AddFile("PROG.cbl",
            "       PROGRAM-ID. PROG.\n           COPY MID.\n", isCopybook: false);

        var plan1 = await _planner.PlanAsync(new[] { "PROG.cbl" });
        await _planner.RecordParseAsync(plan1.ToParse[0], RektParseOutcome.Full);

        // Change LEAF — PROG depends on it transitively via MID.
        var ng = new RektCopybookGraph();
        ng.AddFile("LEAF.cpy", "01 WS-LEAF PIC X(99).", isCopybook: true);   // changed
        ng.AddFile("MID.cpy", "           COPY LEAF.", isCopybook: true);
        ng.AddFile("PROG.cbl",
            "       PROGRAM-ID. PROG.\n           COPY MID.\n", isCopybook: false);
        var np = new IncrementalScanPlanner(_cache, ng, Scheme);

        var plan2 = await np.PlanAsync(new[] { "PROG.cbl" });
        plan2.ToParse.Should().HaveCount(1);
        plan2.ToParse[0].Reason.Should().Be(ScanReason.DependencyChanged);
    }

    [Fact]
    public async Task PreviousLowConfidence_TriggersRetry()
    {
        _graph.AddFile("PROG.cbl", "PROGRAM-ID. PROG.", isCopybook: false);
        var plan1 = await _planner.PlanAsync(new[] { "PROG.cbl" });
        await _planner.RecordParseAsync(plan1.ToParse[0], RektParseOutcome.DepsOnly);

        var plan2 = await _planner.PlanAsync(new[] { "PROG.cbl" });
        plan2.ToParse.Should().HaveCount(1);
        plan2.ToParse[0].Reason.Should().Be(ScanReason.PreviousParseLowConfidence);
    }

    [Fact]
    public async Task DifferentIdentityScheme_TreatsCacheAsEmpty()
    {
        _graph.AddFile("PROG.cbl", "PROGRAM-ID. PROG.", isCopybook: false);
        var plan1 = await _planner.PlanAsync(new[] { "PROG.cbl" });
        await _planner.RecordParseAsync(plan1.ToParse[0], RektParseOutcome.Full);

        // New planner under a different identity scheme.
        var otherPlanner = new IncrementalScanPlanner(_cache, _graph, "v2-relative-path");
        var plan2 = await otherPlanner.PlanAsync(new[] { "PROG.cbl" });

        plan2.ToParse.Should().HaveCount(1);
        plan2.ToParse[0].Reason.Should().Be(ScanReason.NotCached);
    }

    [Fact]
    public async Task DependencyClosure_IncludesAllTransitiveCopybooks()
    {
        _graph.AddFile("LEAF.cpy", "01 X PIC X.", isCopybook: true);
        _graph.AddFile("MID.cpy", "           COPY LEAF.", isCopybook: true);
        _graph.AddFile("ROOT.cbl",
            "       PROGRAM-ID. ROOT.\n           COPY MID.\n", isCopybook: false);

        var closure = _planner.ComputeDependencyClosure(new[] { "ROOT.cbl" });

        closure.Should().BeEquivalentTo(new[] { "ROOT.cbl", "MID.cpy", "LEAF.cpy" });
        await Task.CompletedTask;
    }

    [Fact]
    public async Task MissingCopybookFromCorpus_AfterCache_InvalidatesProgram()
    {
        _graph.AddFile("BOOK.cpy", "01 X PIC X.", isCopybook: true);
        _graph.AddFile("PROG.cbl",
            "       PROGRAM-ID. PROG.\n           COPY BOOK.\n", isCopybook: false);

        var plan1 = await _planner.PlanAsync(new[] { "PROG.cbl" });
        await _planner.RecordParseAsync(plan1.ToParse[0], RektParseOutcome.Full);

        // New graph WITHOUT the copybook (someone deleted it).
        var ng = new RektCopybookGraph();
        ng.AddFile("PROG.cbl",
            "       PROGRAM-ID. PROG.\n           COPY BOOK.\n", isCopybook: false);
        var np = new IncrementalScanPlanner(_cache, ng, Scheme);

        var plan2 = await np.PlanAsync(new[] { "PROG.cbl" });
        plan2.ToParse.Should().HaveCount(1);
        plan2.ToParse[0].Reason.Should().Be(ScanReason.DependencyMissingFromCorpus);
        plan2.ToParse[0].MissingCopybooks.Should().Contain("BOOK");
    }

    [Fact]
    public async Task UnchangedSourceWithSameDeps_SkipsEvenAfterMultipleRuns()
    {
        _graph.AddFile("BOOK.cpy", "01 X PIC X.", isCopybook: true);
        _graph.AddFile("PROG.cbl",
            "       PROGRAM-ID. PROG.\n           COPY BOOK.\n", isCopybook: false);

        var plan1 = await _planner.PlanAsync(new[] { "PROG.cbl" });
        await _planner.RecordParseAsync(plan1.ToParse[0], RektParseOutcome.Full);

        for (int i = 0; i < 3; i++)
        {
            var p = await _planner.PlanAsync(new[] { "PROG.cbl" });
            p.ToSkip.Should().HaveCount(1, $"iteration {i} should keep skipping");
            p.ToParse.Should().BeEmpty();
        }
    }
}
