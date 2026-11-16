using System.CommandLine;
using System.Text.Json;
using CobolToQuarkusMigration.Agents.Infrastructure;

namespace CobolToQuarkusMigration.Cli;

// Called by doctor.sh once per parsed program, so it stays offline and writes nothing.
public static class RektDiagnosticsCommand
{
    public const int Present = 0;
    public const int Absent = 1;
    public const int Unreadable = 2;

    public static Command Build()
    {
        var cmd = new Command("rekt-diagnostics", "Inspect the artifacts and diagnostics the REKT parser wrote for one program.");

        var structure = new Command("structure",
            "Exit 0 when the report holds non-empty AST and control-flow JSON, 1 when either is missing or empty, 2 when unreadable.");
        var reportArg = new Argument<string>("report-dir", "The program's <name>.report directory under output/rekt.");
        structure.AddArgument(reportArg);
        structure.SetHandler((string reportDir) =>
        {
            try
            {
                Environment.ExitCode = RektParseDiagnostics.HasStructure(reportDir) ? Present : Absent;
            }
            catch (Exception ex) when (ex is JsonException or IOException or UnauthorizedAccessException)
            {
                Console.Error.WriteLine($"rekt-diagnostics: unreadable artifact under {reportDir}: {ex.Message}");
                Environment.ExitCode = Unreadable;
            }
        }, reportArg);
        cmd.AddCommand(structure);

        var missing = new Command("missing-copybooks",
            "Print the program's unresolved copybooks as 'missing-copybook:<message>' warnings joined by '|'.");
        var diagnosticsArg = new Argument<string>("diagnostics-json", "The report's parse-diagnostics.json.");
        missing.AddArgument(diagnosticsArg);
        missing.SetHandler((string diagnosticsPath) =>
        {
            try
            {
                Console.Out.WriteLine(string.Join('|', RektParseDiagnostics.MissingCopybookWarnings(diagnosticsPath)));
            }
            catch (Exception ex) when (ex is JsonException or InvalidDataException or IOException or UnauthorizedAccessException)
            {
                Console.Error.WriteLine($"rekt-diagnostics: {ex.Message}");
                Environment.ExitCode = Unreadable;
            }
        }, diagnosticsArg);
        cmd.AddCommand(missing);

        return cmd;
    }
}
