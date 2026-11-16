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

    private (int ExitCode, string StdOut) Run(string subcommand, string stdin)
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
