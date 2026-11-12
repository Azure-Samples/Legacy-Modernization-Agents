using System.CommandLine;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using CobolToQuarkusMigration.Helpers;
using Microsoft.Extensions.Logging;

namespace CobolToQuarkusMigration.Cli;

public static class DependencyPlanCommand
{
    public static Command Build(ILoggerFactory loggerFactory)
    {
        var command = new Command("dependency-plan", "Assess draft eligibility from existing disk evidence without model calls, parsing or graph writes.");
        var source = new Option<string>("--source") { IsRequired = true };
        var facts = new Option<string>("--facts") { IsRequired = true };
        var output = new Option<string>("--output") { IsRequired = true };
        command.AddOption(source);
        command.AddOption(facts);
        command.AddOption(output);
        command.SetHandler(async (string sourceRoot, string factsRoot, string outputRoot) =>
        {
            sourceRoot = Path.GetFullPath(sourceRoot);
            factsRoot = Path.GetFullPath(factsRoot);
            outputRoot = Path.GetFullPath(outputRoot);
            if (!Directory.Exists(sourceRoot) || !Directory.Exists(factsRoot))
                throw new ArgumentException("Supply existing source and facts directories; missing inputs cannot authorize conversion.");
            if (Directory.Exists(outputRoot) && Directory.EnumerateFileSystemEntries(outputRoot).Any())
                throw new ArgumentException("Planning output must be a fresh empty directory, separate from source/evidence.");
            var helper = new FileHelper(loggerFactory.CreateLogger<FileHelper>());
            var files = (await helper.ScanDirectoryForCobolFilesAsync(sourceRoot))
                .Where(file => SourceTypeRegistry.IsCobolProgram(file.FileName)).ToList();
            var plan = DraftConversionPlanner.Assess(new(factsRoot), factsRoot, files, sourceRoot);
            plan.Save(outputRoot);
            Console.WriteLine($"Draft plan: {plan.Status}; {plan.Programs.Count(p => p.Status == "eligible")} eligible whole programs. NOT VERIFIED. No model calls.");
        }, source, facts, output);
        return command;
    }
}
