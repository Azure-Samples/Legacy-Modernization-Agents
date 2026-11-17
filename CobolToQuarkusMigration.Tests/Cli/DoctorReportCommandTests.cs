using System.Diagnostics;
using CobolToQuarkusMigration.Agents.Infrastructure.Doctor;
using FluentAssertions;
using Microsoft.Data.Sqlite;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Cli;

public sealed class DoctorReportCommandTests : IDisposable
{
    private readonly string _root = Directory.CreateTempSubdirectory("doctor-report-").FullName;

    public void Dispose()
    {
        SqliteConnection.ClearAllPools();
        Directory.Delete(_root, recursive: true);
    }

    [Fact]
    public void SourcePrograms_ListFolderFilesBeforeSubfoldersAndSkipStaging()
    {
        foreach (var file in new[] { "b/Z.cbl", "b/a/Y.COB", "B.cbl", "a.cbl", "notes.txt", ".rekt-staging/S.cbl",
                     ".preprocessed/P.cbl", ".convert-1/C.cbl", ".other/H.cbl" })
        {
            var path = Path.Join(_root, file);
            Directory.CreateDirectory(Path.GetDirectoryName(path)!);
            File.WriteAllText(path, "");
        }

        DoctorReports.SourcePrograms(_root).Should().Equal("B.cbl", "a.cbl", ".other/H.cbl", "b/Z.cbl", "b/a/Y.COB");
    }

    [Fact]
    public void MigrationDbPath_ResolvesConfiguredRelativePathAgainstRepoRoot()
    {
        Directory.CreateDirectory(Path.Join(_root, "Config"));
        File.WriteAllText(Path.Join(_root, "Config", "appsettings.json"),
            """{"ApplicationSettings":{"MigrationDatabasePath":"../shared/./runs.db"}}""");

        DoctorReports.MigrationDbPath(_root, null).Should().Be(Path.GetFullPath(Path.Join(_root, "..", "shared", "runs.db")));
        DoctorReports.MigrationDbPath(_root, "env.db").Should().Be(Path.GetFullPath(Path.Join(_root, "env.db")));
    }

    [Fact]
    public void LatestRun_ReportsNewestRunAndFallsBackToStartTime()
    {
        var db = Path.Join(_root, "migration.db");
        using (var connection = new SqliteConnection($"Data Source={db}"))
        {
            connection.Open();
            using var command = connection.CreateCommand();
            command.CommandText = """
                CREATE TABLE runs (id INTEGER, status TEXT, started_at TEXT, completed_at TEXT);
                INSERT INTO runs VALUES (1, 'Completed', '2026-01-01', '2026-01-02'), (2, 'Running', '2026-02-01', NULL);
                """;
            command.ExecuteNonQuery();
        }

        DoctorReports.LatestRun(db).Should().Be("2|Running|2026-02-01");
    }

    [Fact]
    public void JclJobSummary_MatchesMemberAndRepeatedNamesAndWarnsOnUnknownJobs()
    {
        var manifest = Path.Join(_root, "jobs-manifest.json");
        File.WriteAllText(manifest, """
            [{"job":"NIGHTLY_2","file":"jcl/NIGHT.jcl","programs":["A","B"],"programsImplemented":["A"],"programsMissing":["B"],"stepsNotRunnable":[{}]},
             {"job":"CLEANUP","file":"jcl/CLEANUP.jcl","programs":[],"stepsNotRunnable":[]}]
            """);

        var summary = DoctorReports.JclJobSummary(manifest, ["nightly", " Night ", "Ghost"]);

        summary.Should().StartWith("⚠️  No JCL job is named Ghost.\n\n🧾 JCL jobs (1): ");
        summary.Should().Contain("  ⚠️  NIGHTLY_2  1/2 program(s) converted, not yet: B; 1 step(s) cannot run");
        summary.Should().Contain("  0 of 1 job(s) have every program and step they need.");
        DoctorReports.JclJobSummary(manifest, [""]).Should().Contain("  ✅ CLEANUP    runs utilities only, every step runnable\n");
    }

    [Fact]
    public void ProgramList_ShowsMissingCopybooks()
    {
        var table = DoctorReports.ProgramList("""
            {"programs":[{"basename":"PAY.cbl","linesOfCode":120,"parseFidelity":"full","callClosureCount":2,
              "calledByCount":0,"relativePath":"est/PAY.cbl","missingCopybookCount":3}],"totalPrograms":1,"totalCopybooks":4}
            """);

        table.Should().Contain("PAY.cbl                      120  full           2          0  est/PAY.cbl  ⚠ 3 missing cpy\n");
        table.Should().Contain("1 programs, 4 copybooks.");
    }

    [Fact]
    public void CallClosureAndSliceSelectors_RunAsProcessesOverStdin()
    {
        Run("call-closure", """{"callClosure":["B.cbl","C.cbl"]}""").Should().Be((0, "B.cbl\nC.cbl\n"));
        Run("call-closure", "<html>portal error</html>").Should().Be((0, ""));
        Run("slice-selectors", """{"programSelectors":["A.cbl"],"needSelectors":["B.cbl"]}""").Should().Be((0, "A.cbl,B.cbl\n"));
        Run("program-list", "not json").ExitCode.Should().Be(2);
    }

    [Fact]
    public void ProgramFilter_ResolvesPathsBasenamesAndStemsAndExitsOneWhenAmbiguous()
    {
        var programs = Write("programs.txt", "est/PAY.cbl\nother/PAY.cbl\nest/LEDGER.cob\n\n");

        Run("program-filter", " ./est/pay.CBL , ledger,LEDGER.cob,", programs).Should().Be((0, "est/PAY.cbl\nest/LEDGER.cob\n"));
        Run("program-filter", "@pay", programs).Should().Be((1,
            "Program selector '@pay' did not match any program.\n"));
        Run("program-filter", "PAY", programs).Should().Be((1,
            "Program selector 'PAY' matched multiple programs by stem: est/PAY.cbl, other/PAY.cbl. Use a source-relative path.\n"));
        Run("program-filter", " , ", programs).Should().Be((1, "Program filter did not resolve to any programs.\n"));
    }

    [Fact]
    public void ProgramDuplicates_ListsCaseInsensitiveBasenameClashesInOrder()
    {
        var programs = Write("programs.txt", "b/ZED.cbl\na/pay.cbl\nb/PAY.cbl\na/ZED.cbl\nc/ONLY.cbl\n");

        Run("program-duplicates", "", programs).Should().Be((0,
            "program-duplicate\tpay.cbl\ta/pay.cbl\tb/PAY.cbl\nprogram-duplicate\tzed.cbl\tb/ZED.cbl\ta/ZED.cbl\n"));
    }

    [Fact]
    public void MissingCopybookReport_IgnoresPlaceholdersCommentsAndOtherEstates()
    {
        var staging = Path.Join(_root, "staging");
        Write("staging/one/PAY.cbl", "       COPY CUSTREC.\n           INITIALIZE MSG-COPY  REPLACING NUMERIC BY ZEROES.\n      * COPY COMMENTED.\n           COPY 'stubbed'.\n       COPY PRESENT.\n");
        Write("staging/one/sub/AUX.cbl", "       copy custrec.\n");
        Write("staging/one/PRESENT.cpy", "");
        Write("staging/one/STUBBED.cpy", "");
        Write("staging/one/.generated-stubs", "stubbed\n");
        Write("staging/two/OTHER.cbl", "       COPY PRESENT.\n");
        var report = Path.Join(_root, "missing copybooks.txt");

        Run("missing-copybook-report", "", staging, report).Should().Be((0,
            "3|3\n  CUSTREC ← PAY.cbl, sub/AUX.cbl\n  PRESENT ← OTHER.cbl\n  STUBBED ← PAY.cbl\n"));
        File.ReadAllText(report).Should().EndWith(
            "CUSTREC\treferenced by: PAY.cbl, sub/AUX.cbl\nPRESENT\treferenced by: OTHER.cbl\nSTUBBED\treferenced by: PAY.cbl\n");

        File.Delete(Path.Join(staging, "two", "OTHER.cbl"));
        File.Delete(Path.Join(staging, "one", ".generated-stubs"));
        File.WriteAllText(Path.Join(staging, "one", "CUSTREC.cpy"), "");
        Run("missing-copybook-report", "", staging, report).Should().Be((0, ""));
        File.ReadAllText(report).Should().BeEmpty();
    }

    [Fact]
    public void StubWarnings_ListsPlaceholdersTheProgramCopiesOutsideComments()
    {
        var program = Write("PAY.cbl", "       COPY ZETA.\n           INITIALIZE WS-COPY BETA.\n      *COPY ALPHA.\n      *> COPY BETA.\n       COPY \"alpha\".\n       COPY REAL.\n");
        var stubs = Write(".generated-stubs", "ZETA\nalpha\nBETA\n");

        Run("stub-warnings", "", program, stubs).Should().Be((0, "generated-copybook-stub:ALPHA|generated-copybook-stub:ZETA\n"));
    }

    [Fact]
    public void DependencyWarnings_JoinsDiagnosticsAndFailsWithoutTheField()
    {
        var evidence = Write("dependency-evidence.json", """{"unresolvedDiagnostics":["missing a|b","two\nlines",3,null,true]}""");
        Run("dependency-warnings", "", evidence).Should().Be((0, "missing a/b|two lines|3|None|True\n"));

        File.WriteAllText(evidence, """{"definitions":[]}""");
        Run("dependency-warnings", "", evidence).ExitCode.Should().NotBe(0);
        Run("dependency-warnings", "", Path.Join(_root, "absent.json")).ExitCode.Should().NotBe(0);
    }

    [Fact]
    public void CommareaPending_ListsPendingAndStaleClosuresAndSkipsUnreadableEvidence()
    {
        var output = Path.Join(_root, "rekt");
        var results = Path.Join(_root, "results");
        Write("rekt/est/.commarea-closure.json", "{}");
        var digest = Convert.ToHexStringLower(System.Security.Cryptography.SHA256.HashData(File.ReadAllBytes(Path.Join(output, "est", ".commarea-closure.json"))));
        Write("rekt/est/PEND.cbl.report/dependency-evidence.json", """{"unresolvedDiagnostics":["native:cics-commarea-closure-missing"]}""");
        Write("rekt/est/OLD.cbl.report/dependency-evidence.json", """{"definitions":[{"kind":"COMMAREA_CLOSURE","sha256":"old"}]}""");
        Write("rekt/est/CURRENT.cbl.report/dependency-evidence.json",
            $$"""{"definitions":[{"kind":"COMMAREA_CLOSURE","sha256":"{{digest}}"}],"unresolvedDiagnostics":[]}""");
        Write("rekt/est/BAD.cbl.report/dependency-evidence.json", "not json");
        Write("results/1.status", "0\test/PEND.cbl\n");
        Write("results/2.status", "0\test/OLD.cbl\n");
        Write("results/3.status", "0\test/CURRENT.cbl\n");
        Write("results/4.status", "0\test/BAD.cbl\n");
        Write("results/5.status", "0\test/NOEVIDENCE.cbl\n");

        Run("commarea-pending", "", results, output).Should().Be((0,
            $"{Path.Join(results, "1")}\test/PEND.cbl\n{Path.Join(results, "2")}\test/OLD.cbl\n"));
        Run("commarea-pending", "", Path.Join(_root, "absent"), output).Should().Be((0, ""));

        Write("results/0.status", "no program field\n");
        Run("commarea-pending", "", results, output).ExitCode.Should().NotBe(0);
    }

    private string Write(string relative, string content)
    {
        var path = Path.Join(_root, relative);
        Directory.CreateDirectory(Path.GetDirectoryName(path)!);
        File.WriteAllText(path, content);
        return path;
    }

    private (int ExitCode, string StdOut) Run(string subcommand, string stdin, params string[] arguments)
    {
        var psi = new ProcessStartInfo("dotnet")
        {
            RedirectStandardInput = true,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            WorkingDirectory = _root,
        };
        psi.ArgumentList.Add(Path.Join(AppContext.BaseDirectory, "CobolToQuarkusMigration.dll"));
        psi.ArgumentList.Add("doctor-report");
        psi.ArgumentList.Add(subcommand);
        foreach (var argument in arguments) psi.ArgumentList.Add(argument);

        using var process = Process.Start(psi)!;
        process.StandardInput.Write(stdin);
        process.StandardInput.Close();
        var stdErr = process.StandardError.ReadToEndAsync();
        var stdOut = process.StandardOutput.ReadToEnd();
        process.WaitForExit(60_000).Should().BeTrue("the CLI should not hang");
        _ = stdErr.Result;
        return (process.ExitCode, stdOut.Replace("\r\n", "\n"));
    }
}
