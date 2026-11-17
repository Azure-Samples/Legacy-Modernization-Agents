using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Doctor;

// A helper crash once surfaced as a silent exit 1 that doctor.sh read as "no answer", so a scan
// finished on partial evidence. These pin that a crash is always recorded and fails the scan.
public sealed class DoctorHelperCrashTests : IDisposable
{
    private readonly DoctorFixture _doctor = new();

    public DoctorHelperCrashTests()
    {
        // Stands in for "dotnet <assembly> <command>": $1 is the assembly.
        _doctor.WriteExecutable("dotnet", """
            #!/bin/bash
            shift
            case "$1" in
              crash) echo partial; echo "Unhandled exception: System.NullReferenceException: boom" >&2; exit 1 ;;
              answer) echo "doctor-report: selector did not resolve" >&2; exit 2 ;;
              rekt-activity) [[ "$2" == start ]] && echo "$SCAN_DIR/scan.json"; exit 0 ;;
            esac
            """);
        Directory.CreateDirectory(ScanDir);
        _doctor.Env["SCAN_DIR"] = DoctorFixture.Posix(ScanDir);
        _doctor.Env["LMA_HELPER_LOG"] = DoctorFixture.Posix(HelperLog);
    }

    public void Dispose() => _doctor.Dispose();

    private string ScanDir => Path.Join(_doctor.Root, "scan");
    private string HelperLog => Path.Join(_doctor.Root, "Logs", "doctor-helpers.log");

    [Fact]
    public void Crash_IsLoggedWithItsError_EvenWhenTheCallerDiscardsStderr()
    {
        var run = Bash("out=$(lma_cli_run asm crash --estate x 2>/dev/null); rc=$?; echo \"rc=$rc out=$out\"");

        run.StdOut.Should().Contain("rc=1 out=partial");
        File.ReadAllText(HelperLog).Should().Contain("lma_cli crash --estate x (exit 1)")
            .And.Contain("NullReferenceException: boom");
    }

    [Fact]
    public void HandledNonZeroExit_IsAnAnswer_NotACrash()
    {
        var run = Bash("lma_cli_run asm answer; echo \"rc=$?\"");

        run.StdOut.Should().Contain("rc=2");
        run.StdErr.Should().Contain("selector did not resolve").And.NotContain("Internal helper crashed");
        File.Exists(HelperLog).Should().BeFalse();
    }

    [Fact]
    public void Scan_Fails_WhenAHelperCrashed_EvenIfTheCallerCarriedOn()
    {
        var function = Between("run_native_activity() (", "\nrun_rekt_parse() {");
        var run = Bash("RED= NC=\nensure_lma_cli() { :; }\nlma_cli() { lma_cli_run asm \"$@\"; }\n" + function
            + "\nscan_body() { lma_cli crash 2>/dev/null || true; echo body-done; }"
            + "\nrun_native_activity rekt-parse 'Native parsing' scan_body; echo \"rc=$?\"");

        run.StdOut.Should().Contain("body-done").And.Contain("rc=1");
        run.StdErr.Should().Contain("1 internal helper crash(es) in this run");
        File.ReadAllText(Path.Join(ScanDir, "helper-crashes.log")).Should().Contain("NullReferenceException");
        File.ReadAllText(Path.Join(ScanDir, "native.log")).Should().Contain("internal helper crash");
    }

    private DoctorRun Bash(string script) => DoctorFixture.RunProcess(DoctorFixture.Bash,
        ["-c", $"source '{DoctorFixture.Posix(Path.Join(_doctor.Root, "tools/lib/lma-cli.sh"))}'\n{script}"], _doctor.Root, _doctor.Env, TimeSpan.FromSeconds(30));

    private static string Between(string start, string end)
    {
        var script = File.ReadAllText(Path.Join(DoctorFixture.RepoRoot, "doctor.sh"));
        var from = script.IndexOf(start, StringComparison.Ordinal);
        from.Should().BeGreaterThanOrEqualTo(0, "doctor.sh should define {0}", start);
        return script[from..script.IndexOf(end, from, StringComparison.Ordinal)];
    }
}
