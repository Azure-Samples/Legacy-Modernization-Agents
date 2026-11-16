using System.Text.Json.Nodes;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Doctor;

public sealed class GraphIngestRefusedException(string message) : Exception(message);

// doctor.sh runs with the genuine jcl-facts/jcl-jobs, and its output is consumed by the genuine
// graph-ingest --dry-run, which plans what it would write without a database.
public sealed class DoctorJclCliIngestTests : IDisposable
{
    private const string CobolProgram = "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. PROGRAM.\n";

    private readonly DoctorFixture _doctor = new();
    private List<(string Label, JsonObject Properties)> _nodes = [];

    public DoctorJclCliIngestTests() => _doctor.UseRealJclCli();

    public void Dispose() => _doctor.Dispose();

    private string Root => _doctor.Root;

    private IEnumerable<JsonObject> Jobs() =>
        _nodes.Where(n => n.Label == "JclNode" && (string?)n.Properties["nodeType"] == "JCL_JOB").Select(n => n.Properties);

    private int Consume()
    {
        var result = DoctorFixture.RunProcess(DoctorFixture.RealDotnet,
            [DoctorFixture.CliAssembly, "graph-ingest", "--rekt-output", Path.Join(Root, "output/rekt"),
                "--source-dir", Path.Join(Root, "source"), "--run-id", "42", "--dry-run"],
            Root, _doctor.Env, TimeSpan.FromSeconds(90));
        if (result.ExitCode != 0) throw new GraphIngestRefusedException(result.StdErr);
        _nodes = JsonNode.Parse(result.StdOut)!["nodes"]!.AsArray()
            .Select(node => ((string)node!["label"]!, node["properties"]!.AsObject()))
            .ToList();
        return _nodes.Count(n => n.Label == "JclNode");
    }

    private static IEnumerable<string> FilesUnder(string directory, string pattern) =>
        Directory.Exists(directory) ? Directory.EnumerateFiles(directory, pattern, SearchOption.AllDirectories) : [];

    [Fact]
    public void ExternalNestedUppercaseJob_RoundtripsExactSourceNotLocalSameName()
    {
        _doctor.Job(text: "//WRONG JOB\n//S1 EXEC PGM=IEFBR14\n");
        var external = _doctor.Job("external jobs/nested jobs/SAMPLE.JCL", "//RIGHT JOB\n//S1 EXEC PGM=IEFBR14\n");
        _doctor.Env["JCL_SOURCE_FOLDER"] = Path.Join(Root, "external jobs");

        _doctor.Run("rekt-full").ShouldSucceed();

        var jclNodes = Consume();
        var job = Jobs().Should().ContainSingle().Which;
        ((string?)job["name"]).Should().Be("RIGHT");
        ((string?)job["program"]).Should().Be("nested jobs/SAMPLE.JCL");
        jclNodes.Should().BeGreaterThan(0);
        _nodes.Where(n => n.Label == "SourceBlock")
            .Select(n => ((string?)n.Properties["program"], (string?)n.Properties["content"], (int?)n.Properties["runId"]))
            .Should().Equal(("nested jobs/SAMPLE.JCL", File.ReadAllText(external), 42));
        var output = Path.Join(Root, "output");
        FilesUnder(output, "*.cs").Should().BeEmpty();
        FilesUnder(output, "*.java").Should().BeEmpty();
        FilesUnder(output, "jobs-manifest.json").Should().BeEmpty();
    }

    [Fact]
    public void MixedScan_RetiresDeletedJobFactsIncludingLastJob()
    {
        _doctor.Job("source/PROGRAM.cbl", CobolProgram);
        var first = _doctor.Job();
        var second = _doctor.Job("source/second.jcl", "//SECOND JOB\n//S1 EXEC PGM=IEFBR14\n");
        _doctor.Run("rekt-full").ShouldSucceed();

        File.Delete(first);
        _doctor.Run("rekt-full").ShouldSucceed();
        Consume();
        Jobs().Select(job => (string?)job["name"]).Should().Equal("SECOND");

        File.Delete(second);
        _doctor.Run("rekt-full").ShouldSucceed();
        Consume().Should().Be(0);
        FilesUnder(Path.Join(Root, "output/rekt"), "*.job.json").Should().BeEmpty();
        _nodes.Should().Contain(n => n.Label == "CobolFile");
        _doctor.Calls("docker").Should().Contain(args => args.Contains("run") && args.Contains("java"));
    }

    [Fact]
    public void FailedMixedRefresh_RetainsJobEvidenceButConsumerRefusesIt()
    {
        _doctor.Job("source/PROGRAM.cbl", CobolProgram);
        _doctor.Job();
        _doctor.Run("rekt-full").ShouldSucceed();
        var factsFile = Path.Join(Root, "output/rekt/nested jobs/SAMPLE.JCL.job.json");
        var retained = File.ReadAllBytes(factsFile);
        File.Delete(Path.Join(Root, "ingested.json"));
        _doctor.Env["JCL_TEST_FAIL"] = "1";

        _doctor.Run("rekt-full").ExitCode.Should().NotBe(0);

        File.ReadAllBytes(factsFile).Should().Equal(retained);
        File.Exists(Path.Join(Root, "ingested.json")).Should().BeFalse();
        FluentActions.Invoking(Consume).Should().Throw<GraphIngestRefusedException>()
            .WithMessage("*JCL refresh did not complete*");
        _doctor.Run("rekt-ingest").ExitCode.Should().NotBe(0);
    }

    [Theory]
    [InlineData("//P PROC\n//S1 EXEC PGM=IEFBR14\n// PEND\n")]
    [InlineData("not JCL at all\n")]
    public void ProcedureOrGarbageExtension_IsNotASuccessfulJobScan(string text)
    {
        _doctor.Job(text: text);

        var result = _doctor.Run("rekt-full");

        result.ExitCode.Should().NotBe(0, "{0}", result.Output);
        result.StdErr.Should().Contain("No JCL jobs were parsed");
        File.Exists(Path.Join(Root, "ingested.json")).Should().BeFalse();
    }

    [Fact]
    public void UnresolvedProcedureAndUnsupportedUtility_AreNotLabelledReady()
    {
        _doctor.Job(text: "//MISSING JOB\n//S1 EXEC ABSENT\n");
        _doctor.Job("source/sort.jcl", "//SORTJOB JOB\n//S1 EXEC PGM=SORT\n");
        _doctor.Env["TARGET_LANGUAGE"] = "CSharp";

        var result = _doctor.Run("jcl");

        result.ShouldSucceed();
        var manifest = FilesUnder(Path.Join(Root, "output"), "jobs-manifest.json").ToList();
        manifest.Should().NotBeEmpty();
        var entries = JsonNode.Parse(File.ReadAllText(manifest[^1]))!.AsArray()
            .ToDictionary(entry => (string)entry!["job"]!, entry => entry!);
        entries["MISSING"]["stepsNotRunnable"]!.AsArray().Select(step => (string?)step).Should().Equal("S1");
        entries["SORTJOB"]["stepsNotRunnable"]!.AsArray().Select(step => (string?)step).Should().Equal("S1");
        result.StdOut.Should().Contain("0 of 2 job(s) have every program and step they need");
        result.StdOut.Should().NotContain("runs utilities only");
        result.StdOut.Should().Contain("unsupported utility");
    }
}
