using System.CommandLine;
using System.CommandLine.Invocation;
using CobolToQuarkusMigration.Agents.Infrastructure.Rekt;

namespace CobolToQuarkusMigration.Cli;

// preprocess-for-rekt.sh rewrites each copybook and program the native parser would reject. Exit 0 means a
// rewritten copy was written to the output directory; exit 1 means the file needs none (or could not be read).
public static class RektPreprocessCommand
{
    public static Command Build()
    {
        var cmd = new Command("rekt-preprocess", "Normalise COBOL copybooks and programs for the REKT parser.");
        var action = new Argument<string>("action", "copybook, program or check-rules.").FromAmong("copybook", "program", "check-rules");
        var source = new Option<string?>("--source", "Copybook or program to normalise.");
        var sourceRoot = new Option<string?>("--source-root", "Source tree the program belongs to.");
        var outputRoot = new Option<string?>("--output-root", "Preprocessed output directory.");
        var rules = new Option<string?>("--rules", "Estate-specific rewrite rules (Config/rekt-preprocess.local.json).");
        cmd.AddArgument(action);
        foreach (var option in new Option[] { source, sourceRoot, outputRoot, rules }) cmd.AddOption(option);
        cmd.SetHandler(context =>
        {
            var parse = context.ParseResult;
            string Required(Option<string?> option) =>
                parse.GetValueForOption(option) ?? throw new ArgumentException($"{option.Name} is required");
            try
            {
                context.ExitCode = parse.GetValueForArgument(action) switch
                {
                    "copybook" => RektPreprocessor.PreprocessCopybook(Required(source), Required(outputRoot)) ? 0 : 1,
                    "program" => RektPreprocessor.PreprocessProgram(Required(source), Required(sourceRoot), Required(outputRoot),
                        parse.GetValueForOption(rules) is { Length: > 0 } path ? RektPreprocessor.LoadRules(path) : []) ? 0 : 1,
                    _ => CheckRules(Required(rules)),
                };
            }
            catch (Exception error) when (error is IOException or UnauthorizedAccessException or ArgumentException
                                              or InvalidDataException or System.Text.Json.JsonException)
            {
                Console.Error.WriteLine($"rekt-preprocess: {error.Message}");
                context.ExitCode = 1;
            }
        });
        return cmd;
    }

    private static int CheckRules(string path)
    {
        try
        {
            RektPreprocessor.LoadRules(path, problem => Console.Out.Write($"  ⚠️  Ignoring preprocess {problem} in {path}\n"));
            return 0;
        }
        catch (Exception error) when (error is InvalidDataException or System.Text.Json.JsonException or IOException)
        {
            return 1;
        }
    }
}
