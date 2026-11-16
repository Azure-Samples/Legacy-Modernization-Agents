using System.Text.Json;
using System.Text.RegularExpressions;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Doctor;

public sealed class DoctorJavaFrameworkTests : IDisposable
{
    private readonly DoctorFixture _doctor = new();

    public void Dispose() => _doctor.Dispose();

    [Fact]
    public void SpringFlag_SelectsJavaAndDryRunReportsFramework()
    {
        _doctor.Env.Remove("JAVA_FRAMEWORK");

        var result = _doctor.Run("convert-only", "--java-framework", "spring", "--dry-run");

        result.ShouldSucceed();
        result.StdOut.Should().Contain("Spring");
        _doctor.Calls("dotnet").Should().NotContain(args => args.Contains("build"));
    }

    [Theory]
    [InlineData("--java-framework", "Spring", "--language", "CSharp")]
    [InlineData("--language", "CSharp", "--java-framework", "Spring")]
    public void SpringAndCSharp_AreRejectedInEitherFlagOrder(params string[] flags)
    {
        var result = _doctor.Run(["convert-only", .. flags, "--dry-run"]);

        result.ExitCode.Should().NotBe(0);
        result.StdErr.Should().Contain("non-Java target");
    }

    [Fact]
    public void InvalidFramework_IsNotSilentlyChangedToQuarkus()
    {
        var result = _doctor.Run("convert-only", "--java-framework", "Sping", "--dry-run");

        result.ExitCode.Should().NotBe(0);
        result.StdErr.Should().Contain("Unknown Java framework");
    }
}

public sealed class DoctorInteractiveTargetTests
{
    private static DoctorRun Select(string answer = "", params (string Key, string Value)[] variables)
    {
        var source = File.ReadAllText(Path.Join(DoctorFixture.RepoRoot, "doctor.sh")).ReplaceLineEndings("\n");
        var function = Regex.Match(source, @"^select_conversion_target\(\) \{.*?^\}", RegexOptions.Multiline | RegexOptions.Singleline);
        function.Success.Should().BeTrue("doctor.sh defines select_conversion_target");
        var env = DoctorFixture.EnvironmentWithout("TARGET_LANGUAGE", "JAVA_FRAMEWORK");
        foreach (var (key, value) in variables) env[key] = value;
        var script = function.Value + "\nselect_conversion_target && printf \"RESULT:%s:%s\\n\" \"$TARGET_LANGUAGE\" \"${JAVA_FRAMEWORK:-}\"";
        return DoctorFixture.RunProcess(DoctorFixture.Bash, ["-c", script], DoctorFixture.RepoRoot, env,
            TimeSpan.FromSeconds(10), answer);
    }

    [Fact]
    public void InteractiveSpring_AndExistingQuarkusDefault()
    {
        Select("3\n").StdOut.Should().Contain("RESULT:Java:Spring");
        Select("\n").StdOut.Should().Contain("RESULT:Java:Quarkus");
    }

    [Fact]
    public void PreselectedSpring_DoesNotConsumeAnotherLanguagePrompt()
    {
        var result = Select(variables: [("TARGET_LANGUAGE", "Java"), ("JAVA_FRAMEWORK", "Spring")]);

        result.ExitCode.Should().Be(0, "{0}", result.StdErr);
        result.StdOut.Should().Contain("RESULT:Java:Spring");
        result.StdOut.Should().NotContain("Select Target Language");
    }
}

public sealed class DoctorConversionCommandTests : IDisposable
{
    private const string Program = "batch jobs/PAY'ONE.cbl";

    private readonly DoctorFixture _doctor = new();
    private readonly string _selectedSource;

    public DoctorConversionCommandTests()
    {
        File.WriteAllText(Path.Join(_doctor.Root, "Config/load-config.sh"), """

            load_ai_config() { return 0; }
            check_ai_connectivity() { echo "CHECKED-CONNECTIVITY"; [[ "${TEST_CONNECTIVITY_FAIL:-}" != true ]]; }
            select_speed_profile() { return 0; }
            print_jcl_job_summary() { return 0; }
            get_migration_db_path() { printf '%s' "$REPO_ROOT/Data/migration.db"; }

            """);
        _doctor.Env["MCP_AUTO_LAUNCH"] = "0";
        _doctor.WriteExecutable("pgrep", "#!/bin/bash\nexit 1\n");
        _selectedSource = Path.Join(_doctor.Root, "selected source's");
        _doctor.Job(Path.Join("selected source's", Program),
            "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. PAYONE.\n");
    }

    public void Dispose() => _doctor.Dispose();

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void CopiedSelection_PreservesCustomRootsQuotesPolicyAndLanguageThroughDoctor(bool absoluteRoot)
    {
        var root = absoluteRoot ? _selectedSource : Path.GetFileName(_selectedSource);
        var factsFolder = Path.Join(_doctor.Root, "snapshot evidence");
        _doctor.Env["REKT_FACTS_FOLDER"] = factsFolder;
        _doctor.Env["CONVERSION_DEPENDENCY_MODE"] = "strict";

        var result = _doctor.Run("convert-only", "--source", root, "--language", "Java", "--programs", Program);

        result.ShouldSucceed();
        result.StdOut.Should().Contain("CHECKED-CONNECTIVITY");
        var args = _doctor.Calls("dotnet").Last(call => call.Contains("--source"));
        DoctorFixture.ShouldBeSamePath(DoctorFixture.After(args, "--source"), _selectedSource);
        DoctorFixture.After(args, "--programs").Should().Be(Program);
        args.Should().Contain("--skip-reverse-engineering");
        JsonSerializer.Deserialize<Dictionary<string, string?>>(File.ReadAllText(Path.Join(_doctor.Root, "conversion-env.json")))
            .Should().BeEquivalentTo(new Dictionary<string, string?>
            {
                ["REKT_FACTS_FOLDER"] = factsFolder,
                ["CONVERSION_DEPENDENCY_MODE"] = "strict",
                ["TARGET_LANGUAGE"] = "Java",
            });
    }

    [Fact]
    public void MissingSelectionOrFailedConnectivity_DoesNotRunConversion()
    {
        var missing = _doctor.Run("convert-only", "--source", _selectedSource, "--programs", "MISSING.cbl");
        missing.ExitCode.Should().NotBe(0);
        missing.StdOut.Should().Contain("No source file matches");

        _doctor.Env["TEST_CONNECTIVITY_FAIL"] = "true";
        var unreachable = _doctor.Run("convert-only", "--source", _selectedSource, "--programs", Program);
        unreachable.ExitCode.Should().NotBe(0);
        unreachable.StdOut.Should().Contain("fix connection issues");

        _doctor.Calls("dotnet").Should().NotContain(call => call.Contains("--source"));
    }

    [Fact]
    public void CustomSource_IsReportedInDryRunAndRejectedForNativeScan()
    {
        var result = _doctor.Run("convert-only", "--source", _selectedSource, "--dry-run", "--programs", Program);

        result.ShouldSucceed();
        result.StdOut.Should().Contain(_selectedSource);
        _doctor.Calls("dotnet").Should().NotContain(call => call.Contains("--source"));

        var rejected = _doctor.Run("rekt-full", "--source", _selectedSource);
        rejected.ExitCode.Should().NotBe(0);
        rejected.StdErr.Should().Contain("--source is supported only");
    }
}
