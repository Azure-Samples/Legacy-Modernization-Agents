using System.CommandLine;
using CobolToQuarkusMigration.Agents.Infrastructure.Rekt;

namespace CobolToQuarkusMigration.Cli;

// doctor.sh records native REKT scan progress in scan.json, which the portal polls; parallel parse jobs call
// this concurrently, so it stays offline and fast.
public static class RektActivityCommand
{
    public static Command Build()
    {
        var cmd = new Command("rekt-activity", "Start and update the native REKT scan lifecycle journal (scan.json).");
        var operation = new Argument<string>("operation", "start, program, stage or finish.").FromAmong("start", "program", "stage", "finish");
        var path = new Argument<string>("path", "Scans folder for start; the scan.json journal otherwise.");
        var program = new Option<string?>("--program", "Program path relative to the source root.");
        var status = new Option<string?>("--status", "Program status (running, Full, NoDialect, cached, ...).");
        var stage = new Option<string?>("--stage", "Stage name.");
        var exitCode = new Option<int?>("--exit-code", "Native command exit code.");
        var expectedPrograms = new Option<int?>("--expected-programs", "Number of programs the scan will parse.");
        var sourceRoot = new Option<string?>("--source-root", "Source root, to measure a finished program.");
        var outputRoot = new Option<string?>("--output-root", "REKT output root, to measure a finished program.");
        var command = new Option<string>("--command", () => "rekt-parse", "Native command being journalled.").FromAmong(NativeRunActivity.Commands);
        cmd.AddArgument(operation);
        cmd.AddArgument(path);
        foreach (var option in new Option[] { program, status, stage, exitCode, expectedPrograms, sourceRoot, outputRoot, command })
            cmd.AddOption(option);

        cmd.SetHandler(context =>
        {
            var parse = context.ParseResult;
            var target = parse.GetValueForArgument(path);
            try
            {
                switch (parse.GetValueForArgument(operation))
                {
                    case "start":
                        Console.Out.Write(NativeRunActivity.Start(target, parse.GetValueForOption(command)!, parse.GetValueForOption(stage)) + "\n");
                        break;
                    case "program":
                        NativeRunActivity.Program(target, parse.GetValueForOption(program), parse.GetValueForOption(status),
                            parse.GetValueForOption(sourceRoot), parse.GetValueForOption(outputRoot));
                        break;
                    case "stage":
                        NativeRunActivity.Stage(target, parse.GetValueForOption(stage), parse.GetValueForOption(expectedPrograms));
                        break;
                    default:
                        NativeRunActivity.Finish(target, parse.GetValueForOption(exitCode));
                        break;
                }
            }
            catch (Exception error) when (RektEvidence.Caught(error))
            {
                Console.Error.WriteLine($"rekt-activity: {error.Message}");
                context.ExitCode = 1;
            }
        });
        return cmd;
    }
}
