using System.CommandLine;
using CobolToQuarkusMigration.Jcl.Generation;

namespace CobolToQuarkusMigration.Cli;

public static class JclJobsCommand
{
    public static Command Build()
    {
        var cmd = new Command("jcl-jobs",
            "Generate a runnable job per JCL job under a source directory: a .NET job for C#, a Spring Batch job for Java.");

        var sourceArg = new Argument<string>("source-dir", "Directory containing the JCL, and any catalogued procedures and INCLUDE members.");
        cmd.AddArgument(sourceArg);

        var languageOption = new Option<string>("--language", () => "CSharp", "Target language: CSharp or Java.");
        cmd.AddOption(languageOption);

        var outputOption = new Option<string?>("--output-dir",
            "The converted output to add the jobs to. Defaults to <repo-root>/output/<language>.")
        { Arity = ArgumentArity.ZeroOrOne };
        cmd.AddOption(outputOption);

        cmd.SetHandler((string sourceDir, string language, string? outputDir) =>
        {
            if (!Directory.Exists(sourceDir))
            {
                Console.Error.WriteLine($"Source dir not found: {sourceDir}");
                Environment.ExitCode = 2;
                return;
            }

            var csharp = CobolToQuarkusMigration.Helpers.ConversionNamespacePolicy.IsCSharp(language);
            var output = outputDir ?? Path.Join(Directory.GetCurrentDirectory(), "output", csharp ? "csharp" : "java");
            var written = JclJobWriter.WriteTo(sourceDir, output, language);
            Console.Error.WriteLine($"jcl-jobs: wrote {written} job(s) and {JclJobWriter.ManifestFile} to {output}");
        }, sourceArg, languageOption, outputOption);

        return cmd;
    }
}
