using System.CommandLine;
using System.Text.Json;
using CobolToQuarkusMigration.Agents.Infrastructure.Doctor;
using CobolToQuarkusMigration.Agents.Infrastructure.Rekt;
using Microsoft.Data.Sqlite;

namespace CobolToQuarkusMigration.Cli;

// Small offline queries doctor.sh makes while it runs, so the shell needs no other runtime.
public static class DoctorReportCommand
{
    public static Command Build()
    {
        var cmd = new Command("doctor-report", "Offline helpers doctor.sh uses to read configuration, runs and portal responses.");

        var repoRoot = new Option<string>("--repo-root", () => Directory.GetCurrentDirectory(), "Repository root.");
        var dbPath = new Command("db-path", "Print the absolute migration database path (MIGRATION_DB_PATH, else Config/appsettings.json).");
        dbPath.AddOption(repoRoot);
        dbPath.SetHandler((string root) => Run(() =>
            Console.Out.WriteLine(DoctorReports.MigrationDbPath(root, Environment.GetEnvironmentVariable("MIGRATION_DB_PATH")))), repoRoot);
        cmd.AddCommand(dbPath);

        var db = new Argument<string>("db", "The migration SQLite database.");
        var latest = new Command("latest-run", "Print '<id>|<status>|<completed>' for the newest run; exit 1 when there is none.");
        latest.AddArgument(db);
        latest.SetHandler((string path) => Run(() =>
        {
            var run = DoctorReports.LatestRun(path);
            if (run is null) Environment.ExitCode = 1;
            else Console.Out.WriteLine(run);
        }), db);
        cmd.AddCommand(latest);

        var source = new Argument<string>("source-root", "The COBOL source folder.");
        var programs = new Command("source-programs", "List .cbl/.cob files relative to the source folder, skipping staging folders.");
        programs.AddArgument(source);
        programs.SetHandler((string root) => Run(() =>
        {
            foreach (var program in DoctorReports.SourcePrograms(root)) Console.Out.WriteLine(program);
        }), source);
        cmd.AddCommand(programs);

        var closure = new Command("call-closure", "Read a portal program response on stdin and print its callClosure, one per line.");
        closure.SetHandler(() => Run(() =>
        {
            foreach (var program in DoctorReports.CallClosure(Console.In.ReadToEnd())) Console.Out.WriteLine(program);
        }));
        cmd.AddCommand(closure);

        var selectors = new Command("slice-selectors", "Read an estate slice on stdin and print its program and need selectors, comma-separated.");
        selectors.SetHandler(() => Run(() => Console.Out.WriteLine(DoctorReports.SliceSelectors(Console.In.ReadToEnd()))));
        cmd.AddCommand(selectors);

        var list = new Command("program-list", "Read the portal program list on stdin and print it as a table.");
        list.SetHandler(() => Run(() => Console.Out.Write(DoctorReports.ProgramList(Console.In.ReadToEnd()))));
        cmd.AddCommand(list);

        var manifest = new Argument<string>("manifest", "A run's jobs-manifest.json.");
        var jobs = new Option<string>("--jobs", () => "", "Comma-separated --job names to report on.");
        var summary = new Command("jcl-jobs", "Print, per JCL job, what it still needs before it runs end to end.");
        summary.AddArgument(manifest);
        summary.AddOption(jobs);
        summary.SetHandler((string path, string names) => Run(() =>
            Console.Out.Write(DoctorReports.JclJobSummary(path, names.Split(',')))), manifest, jobs);
        cmd.AddCommand(summary);

        AddRektParseCommands(cmd);
        return cmd;
    }

    private static void AddRektParseCommands(Command cmd)
    {
        var programsFile = new Argument<string>("programs-file", "Source-relative program paths, one per line.");
        // Selectors arrive on stdin: as an argument, a leading '@' would be read as a response file.
        var filter = new Command("program-filter", "Resolve the comma-separated selectors on stdin to program paths; exit 1 with the reason when they do not resolve.");
        filter.AddArgument(programsFile);
        filter.SetHandler((string path) => Run(() =>
        {
            using var stdin = new StreamReader(Console.OpenStandardInput(), PyCompat.Utf8, detectEncodingFromByteOrderMarks: false);
            var resolved = RektParseReports.ResolveProgramFilter(RektParseReports.NonBlankLines(path), stdin.ReadToEnd(), out var lines);
            Console.Out.Write(string.Join('\n', lines) + "\n");
            if (!resolved) Environment.ExitCode = 1;
        }), programsFile);
        cmd.AddCommand(filter);

        var duplicates = new Command("program-duplicates", "Print 'program-duplicate<TAB>basename<TAB>paths…' for programs sharing a basename.");
        duplicates.AddArgument(programsFile);
        duplicates.SetHandler((string path) => Run(() =>
        {
            foreach (var line in RektParseReports.ProgramDuplicates(RektParseReports.NonBlankLines(path))) Console.Out.Write(line + "\n");
        }), programsFile);
        cmd.AddCommand(duplicates);

        var stagingBase = new Argument<string>("staging-base", "The REKT staging folder holding one folder per estate.");
        var report = new Argument<string>("report", "The missing-copybooks.txt to write.");
        var missing = new Command("missing-copybook-report", "Write the copybooks staged sources COPY but no estate provides, and print a summary.");
        missing.AddArgument(stagingBase);
        missing.AddArgument(report);
        missing.SetHandler((string staging, string path) => Run(() =>
        {
            foreach (var line in RektParseReports.WriteMissingCopybookReport(staging, path)) Console.Out.Write(line + "\n");
        }), stagingBase, report);
        cmd.AddCommand(missing);

        var program = new Argument<string>("program", "The staged program source.");
        var stubs = new Argument<string>("stubs-file", "The estate's .generated-stubs list.");
        var stubWarnings = new Command("stub-warnings", "Print the placeholder copybooks the program COPYs, '|'-separated.");
        stubWarnings.AddArgument(program);
        stubWarnings.AddArgument(stubs);
        stubWarnings.SetHandler((string source, string list) => Run(() =>
            Console.Out.Write(RektParseReports.StubWarnings(source, list) + "\n")), program, stubs);
        cmd.AddCommand(stubWarnings);

        var evidence = new Argument<string>("evidence", "A program's dependency-evidence.json.");
        var dependencyWarnings = new Command("dependency-warnings", "Print the evidence's unresolvedDiagnostics, '|'-separated.");
        dependencyWarnings.AddArgument(evidence);
        dependencyWarnings.SetHandler((string path) => Run(() =>
            Console.Out.Write(RektParseReports.DependencyWarnings(path) + "\n")), evidence);
        cmd.AddCommand(dependencyWarnings);

        var results = new Argument<string>("results", "The parse-results folder of *.status files.");
        var outputRoot = new Argument<string>("output-root", "The REKT output folder.");
        var pending = new Command("commarea-pending", "Print '<result><TAB><program>' for parses to re-export with the COMMAREA closure.");
        pending.AddArgument(results);
        pending.AddArgument(outputRoot);
        pending.SetHandler((string dir, string output) => Run(() =>
        {
            foreach (var line in RektParseReports.CommareaPending(dir, output)) Console.Out.Write(line + "\n");
        }), results, outputRoot);
        cmd.AddCommand(pending);
    }

    private static void Run(Action action)
    {
        // Redirected Windows stdout defaults to the OEM code page; doctor.sh reads paths and messages as UTF-8.
        using var stdout = new StreamWriter(Console.OpenStandardOutput(), PyCompat.Utf8) { AutoFlush = true };
        var original = Console.Out;
        Console.SetOut(stdout);
        try
        {
            action();
        }
        catch (Exception ex) when (ex is JsonException or InvalidDataException or InvalidOperationException
                                       or FormatException or IOException or UnauthorizedAccessException or SqliteException
                                       or PyError)
        {
            Console.Error.WriteLine($"doctor-report: {ex.Message}");
            Environment.ExitCode = 2;
        }
        finally
        {
            Console.SetOut(original);
        }
    }
}
