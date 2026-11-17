using System.Text.Json;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Doctor;

public sealed class DoctorJclScanTests : IDisposable
{
    private readonly DoctorFixture _doctor = new();

    public void Dispose() => _doctor.Dispose();

    private string Root => _doctor.Root;

    private IReadOnlyList<string> FirstJclFacts() =>
        _doctor.Calls("dotnet").First(args => args.Contains("jcl-facts"));

    [Fact]
    public void NativeJclScan_UsesTheExplicitIsolatedCliWithoutBuilding()
    {
        _doctor.Job();
        var assembly = Path.Join(Root, "isolated output/Migration.dll");
        Directory.CreateDirectory(Path.GetDirectoryName(assembly)!);
        File.WriteAllText(assembly, "");
        _doctor.Env["MIGRATION_CLI_ASSEMBLY"] = assembly;

        _doctor.Run("rekt-parse").ShouldSucceed();

        var facts = FirstJclFacts();
        facts[0].Should().Be(assembly);
        facts.Should().NotContain("run").And.NotContain("--project");
    }

    [Fact]
    public void JclOnlyFull_DispatchesNestedUppercaseFactsThenIngestWithoutScaffolding()
    {
        _doctor.Job();
        var evidence = _doctor.Job("output/rekt/old.report/retained.log", "retained");

        _doctor.Run("rekt-full").ShouldSucceed();

        var calls = _doctor.Calls("dotnet");
        var facts = FirstJclFacts();
        DoctorFixture.ShouldBeSamePath(DoctorFixture.After(facts, "jcl-facts"), Path.Join(Root, "source"));
        DoctorFixture.ShouldBeSamePath(DoctorFixture.After(facts, "--output-dir"), Path.Join(Root, "output/rekt"));
        // Resolving the CLI assembly builds the main project; a scaffolded job project is never built.
        calls.Should().NotContain(args => args.Contains("jcl-jobs")
            || (args.Contains("build") && !args.Contains("CobolToQuarkusMigration.csproj")));
        var ingest = _doctor.Ingests().Last();
        DoctorFixture.ShouldBeSamePath(DoctorFixture.After(ingest, "--rekt-output"), Path.Join(Root, "output/rekt"));
        JsonSerializer.Deserialize<string[]>(File.ReadAllText(Path.Join(Root, "ingested.json")))
            .Should().Equal("SAMPLE.JCL.job.json");
        File.ReadAllText(evidence).Should().Be("retained");
        _doctor.Calls("docker").Should().NotContain(args => args.Contains("java"));
    }

    [Theory]
    [InlineData("relative")]
    [InlineData("absolute")]
    [InlineData("backslash")]
    public void RelativeAbsoluteAndBackslashRoots_PreserveSpaceArguments(string form)
    {
        var absolute = Path.Join(Root, "custom jobs");
        var configured = form switch
        {
            "relative" => "custom jobs",
            "absolute" => absolute,
            _ => absolute.Replace('/', '\\'),
        };
        _doctor.Job("custom jobs/nested/SAMPLE.JCL");
        _doctor.Env["JCL_SOURCE_FOLDER"] = configured;

        _doctor.Run("rekt-parse").ShouldSucceed();

        var facts = _doctor.Calls("dotnet").Last(args => args.Contains("jcl-facts"));
        DoctorFixture.ShouldBeSamePath(DoctorFixture.After(facts, "jcl-facts"), absolute);
        _doctor.Ingests().Should().BeEmpty();
        _doctor.Calls("docker").Should().BeEmpty();
    }

    [Theory]
    [InlineData("Q:\\external jobs")]
    [InlineData("Q:/external jobs")]
    [InlineData("//host/share/jobs")]
    public void WindowsDriveAndUncRoots_AreNotPrefixedWithCheckout(string configured)
    {
        _doctor.Env["JCL_SOURCE_FOLDER"] = configured;

        var result = _doctor.Run("rekt-parse");

        result.ExitCode.Should().NotBe(0);
        result.StdOut.Should().NotContain(Root + "/" + configured.Replace('\\', '/'));
        _doctor.Ingests().Should().BeEmpty();
    }

    [Fact]
    public void LocalConfigRoot_IsUsedWithoutAiConfiguration()
    {
        _doctor.Job("configured jobs/SAMPLE.jcl");
        File.WriteAllText(Path.Join(Root, "Config/ai-config.local.env"), "JCL_SOURCE_FOLDER=\"configured jobs\"\n");

        _doctor.Run("rekt-parse").ShouldSucceed();

        DoctorFixture.ShouldBeSamePath(DoctorFixture.After(FirstJclFacts(), "jcl-facts"), Path.Join(Root, "configured jobs"));
    }

    [Fact]
    public void EmptyRootSetting_DefaultsToSource()
    {
        _doctor.Job();
        _doctor.Env["JCL_SOURCE_FOLDER"] = "";

        _doctor.Run("rekt-parse").ShouldSucceed();
    }

    [Fact]
    public void NonEmptyShellRoot_WinsOverLocalConfig()
    {
        _doctor.Job("environment jobs/SAMPLE.JCL");
        File.WriteAllText(Path.Join(Root, "Config/ai-config.local.env"), "JCL_SOURCE_FOLDER=\"missing folder\"\n");
        _doctor.Env["JCL_SOURCE_FOLDER"] = "environment jobs";

        _doctor.Run("rekt-parse").ShouldSucceed();

        DoctorFixture.ShouldBeSamePath(DoctorFixture.After(FirstJclFacts(), "jcl-facts"), Path.Join(Root, "environment jobs"));
    }

    [Theory]
    [InlineData("absent", "configuration is missing")]
    [InlineData("empty", "NEO4J_PASSWORD is empty")]
    [InlineData("bad password", "does not authenticate")]
    public void AbsentEmptyOrRejectedGraphConfig_NeverRunsIngest(string setting, string expectedMessage)
    {
        _doctor.Job();
        _doctor.Env.Remove("NEO4J_PASSWORD");
        if (setting == "empty")
            File.WriteAllText(Path.Join(Root, "Config/ai-config.local.env"), "NEO4J_PASSWORD=\"\"\n");
        else if (setting == "bad password")
        {
            _doctor.Env["NEO4J_PASSWORD"] = "rejected-test-only";
            _doctor.Env["JCL_TEST_AUTH_FAIL"] = "1";
        }

        var result = _doctor.Run("rekt-full");

        result.ExitCode.Should().NotBe(0);
        _doctor.Ingests().Should().BeEmpty();
        result.StdOut.Should().Contain(expectedMessage);
    }

    [Theory]
    [InlineData("missing folder")]
    [InlineData("empty folder")]
    public void MissingOrEmptyConfiguredRoot_RefusesBeforeDestructiveWork(string configured)
    {
        Directory.CreateDirectory(Path.Join(Root, "empty folder"));
        var evidence = _doctor.Job("output/rekt/retained.job.json", "retained");
        _doctor.Env["JCL_SOURCE_FOLDER"] = configured;

        _doctor.Run("rekt-full").ExitCode.Should().NotBe(0);

        File.ReadAllText(evidence).Should().Be("retained");
        _doctor.Ingests().Should().BeEmpty();
        _doctor.Calls("docker").Should().BeEmpty();
    }

    [Fact]
    public void FilterRefusal_PreservesEvidenceAndNeverExtractsOrIngests()
    {
        _doctor.Job();
        var evidence = _doctor.Job("output/rekt/retained.job.json", "retained");
        _doctor.Env["_REKT_PROGRAM_FILTER"] = "ABSENT.cbl";

        var result = _doctor.Run("rekt-full");

        result.ExitCode.Should().NotBe(0);
        result.StdOut.Should().Contain("program filter");
        File.ReadAllText(evidence).Should().Be("retained");
        _doctor.Calls("dotnet").Should().NotContain(args => args.Contains("jcl-facts"));
        _doctor.Ingests().Should().BeEmpty();
    }

    [Fact]
    public void ExtractionFailure_StopsFullPipelineAndPreservesEvidence()
    {
        _doctor.Job();
        var evidence = _doctor.Job("output/rekt/retained.job.json", "retained");
        _doctor.Env["JCL_TEST_FAIL"] = "1";

        var result = _doctor.Run("rekt-full");

        result.ExitCode.Should().NotBe(0);
        result.StdOut.Should().Contain("JCL scan failed");
        File.ReadAllText(evidence).Should().Be("retained");
        File.Exists(Path.Join(Root, "output/rekt/.jcl-scan-failed")).Should().BeTrue();
        _doctor.Ingests().Should().BeEmpty();
    }

    [Fact]
    public void NoSources_IsNotSuccess()
    {
        _doctor.Run("rekt-full").ExitCode.Should().NotBe(0);
        _doctor.Ingests().Should().BeEmpty();
    }

    [Fact]
    public void DocumentedCobolRootLimit_IsPreserved()
    {
        _doctor.Job();
        _doctor.Env["COBOL_SOURCE_FOLDER"] = "other";

        var result = _doctor.Run("rekt-full");

        result.ExitCode.Should().NotBe(0);
        result.StdOut.Should().Contain("parser reads source/");
        _doctor.Calls("docker").Should().BeEmpty();
    }

    [Fact]
    public void CobolOnly_StillParsesAndExplicitlyRefreshesEmptyJcl()
    {
        _doctor.Job("source/PROGRAM.cbl", "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. PROGRAM.\n");

        _doctor.Run("rekt-full").ShouldSucceed();

        _doctor.Calls("docker").Should().Contain(args => args.Contains("java") && args.Contains("run"));
        FirstJclFacts().Should().Contain("--allow-empty");
        File.Exists(Path.Join(Root, "ingested.json")).Should().BeTrue();
    }

    [Fact]
    public void FilteredCobolParse_ResolvesAndReportsThroughDoctorReport()
    {
        _doctor.Job("source/PROGRAM.cbl", "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. PROGRAM.\n       COPY ABSENT.\n");
        _doctor.Job("source/OTHER.cbl", "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. OTHER.\n");
        _doctor.Env["_REKT_PROGRAM_FILTER"] = "program";

        var result = _doctor.Run("rekt-parse");

        result.ShouldSucceed();
        result.StdOut.Should().Contain("Missing copybooks: 1 unresolved COPY target(s)");
        var reports = _doctor.Calls("dotnet").Where(args => args.Contains("doctor-report")).Select(args => args.SkipWhile(arg => arg != "doctor-report").ElementAt(1));
        reports.Should().Contain(["program-filter", "program-duplicates", "missing-copybook-report", "commarea-pending"]);
    }
}
