using System.CommandLine;
using CobolToQuarkusMigration.Jcl;

namespace CobolToQuarkusMigration.Cli;

public static class JclFactsCommand
{
    public static Command Build()
    {
        var cmd = new Command("jcl-facts",
            "Parse every JCL job under a source directory and write <source-relative>.job.json per job plus jcl-lineage.json.");

        var sourceArg = new Argument<string>("source-dir", "Directory containing the JCL, and any catalogued procedures and INCLUDE members.");
        cmd.AddArgument(sourceArg);

        var outputOption = new Option<string?>("--output-dir", "Where to write the facts. Defaults to <repo-root>/output/rekt.")
        { Arity = ArgumentArity.ZeroOrOne };
        cmd.AddOption(outputOption);
        var allowEmptyOption = new Option<bool>("--allow-empty", "Allow an empty JCL refresh when scanning a COBOL-only estate.");
        cmd.AddOption(allowEmptyOption);

        cmd.SetHandler((string sourceDir, string? outputDir, bool allowEmpty) =>
        {
            var output = outputDir ?? Path.Join(Directory.GetCurrentDirectory(), "output", "rekt");
            try
            {
                var written = JclEstate.Write(sourceDir, output, allowEmpty);
                Console.Error.WriteLine($"jcl-facts: wrote {written} job fact file(s) and {JclEstate.LineageFileName} to {output}");
            }
            catch (Exception ex) when (ex is IOException or UnauthorizedAccessException or InvalidDataException)
            {
                Console.Error.WriteLine($"jcl-facts failed: {ex.Message}");
                Environment.ExitCode = 2;
            }
        }, sourceArg, outputOption, allowEmptyOption);

        return cmd;
    }
}
