using System.CommandLine;
using System.Globalization;
using CobolToQuarkusMigration.Helpers;
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
            "The run folder to add the jobs to, such as an earlier conversion's. Defaults to a new run folder, output/<language>/<timestamp>.")
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

            var csharp = ConversionNamespacePolicy.IsCSharp(language);
            if (!csharp && !language.Equals("Java", StringComparison.OrdinalIgnoreCase))
            {
                Console.Error.WriteLine($"Unknown language '{language}'. Use CSharp or Java.");
                Environment.ExitCode = 2;
                return;
            }
            // A run folder of its own, as a conversion gets: output/<language> holds every run, and
            // the C# project written below would otherwise compile all of them together.
            var output = outputDir ?? Path.Join(Directory.GetCurrentDirectory(), "output", csharp ? "csharp" : "java",
                DateTime.Now.ToString("yyyyMMdd-HHmmss", CultureInfo.InvariantCulture));
            var written = JclJobWriter.WriteTo(sourceDir, output, language);
            Console.Error.WriteLine($"jcl-jobs: wrote {written} job(s) and {JclJobWriter.ManifestFile} to {output}");

            // The same project file a conversion writes, so jobs generated on their own can be built.
            if (csharp && written > 0)
            {
                var scaffold = GeneratedProjectScaffold.Write(output, ConversionNamespacePolicy.Root("C#"));
                if (scaffold.ProjectPath is not null)
                    Console.Error.WriteLine($"jcl-jobs: wrote {Path.GetFileName(scaffold.ProjectPath)}");
            }
        }, sourceArg, languageOption, outputOption);

        return cmd;
    }
}
