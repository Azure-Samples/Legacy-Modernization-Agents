using System.CommandLine;
using System.Text.Json;
using System.Text.Json.Serialization;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using Microsoft.Extensions.Logging;

namespace CobolToQuarkusMigration.Cli;

public static class ResolveProgramsCommand
{
    public static Command Build(ILoggerFactory loggerFactory)
    {
        var cmd = new Command("resolve-programs",
            "Resolve conversion scope to source-relative program paths (preview). Prints one path per line.");

        var stagingDirArg = new Argument<string>("staging-dir",
            "Directory containing the COBOL programs that conversion selects from.");
        cmd.AddArgument(stagingDirArg);

        var programOption = new Option<string[]>("--program",
            "Source-relative path, basename, or stem. Repeatable, and accepts a comma-separated list. Basename/stem selectors must be unambiguous.")
        { AllowMultipleArgumentsPerToken = false };
        cmd.AddOption(programOption);

        var includeCallersOption = new Option<bool>("--include-callers", () => false,
            "Also select every program that reaches the named programs through recorded CALL edges.");
        cmd.AddOption(includeCallersOption);

        var includeCalleesOption = new Option<bool>("--include-callees", () => false,
            "Also select every program the named programs reach through recorded CALL edges.");
        cmd.AddOption(includeCalleesOption);

        var factsDirOption = new Option<string?>("--facts-dir",
            "Directory holding *.facts.json used as CALL-edge evidence. Defaults to <repo-root>/output/rekt.")
        { Arity = ArgumentArity.ZeroOrOne };
        cmd.AddOption(factsDirOption);

        var manifestOption = new Option<string?>("--manifest",
            "Write the resolved selection, match reasons and unresolved CALL targets to this JSON file.")
        { Arity = ArgumentArity.ZeroOrOne };
        cmd.AddOption(manifestOption);

        var repoRootOption = new Option<string?>("--repo-root", "Repo root (defaults to the current working directory).")
        { Arity = ArgumentArity.ZeroOrOne };
        cmd.AddOption(repoRootOption);

        var stageOption = new Option<string?>("--stage",
            "Copy the resolved scope into this directory and use it as the conversion source. Programs keep their source-relative folders; copybooks stage flat.")
        { Arity = ArgumentArity.ZeroOrOne };
        cmd.AddOption(stageOption);

        cmd.SetHandler(context =>
        {
            var parsed = context.ParseResult;
            context.ExitCode = Execute(
                loggerFactory,
                parsed.GetValueForArgument(stagingDirArg),
                parsed.GetValueForOption(programOption) ?? Array.Empty<string>(),
                parsed.GetValueForOption(includeCallersOption),
                parsed.GetValueForOption(includeCalleesOption),
                parsed.GetValueForOption(factsDirOption),
                parsed.GetValueForOption(manifestOption),
                parsed.GetValueForOption(repoRootOption),
                parsed.GetValueForOption(stageOption));
        });

        return cmd;
    }

    internal static int Execute(
        ILoggerFactory loggerFactory,
        string stagingDir,
        IReadOnlyList<string> programOptionValues,
        bool includeCallers,
        bool includeCallees,
        string? factsDir,
        string? manifestPath,
        string? repoRoot,
        string? stageDir = null)
    {
        var logger = loggerFactory.CreateLogger("ResolvePrograms");

        if (!Directory.Exists(stagingDir))
        {
            Console.Error.WriteLine($"Staging dir not found: {stagingDir}");
            return 2;
        }

        var resolvedRepoRoot = repoRoot ?? Directory.GetCurrentDirectory();
        var resolvedFactsDir = factsDir ?? Path.Combine(resolvedRepoRoot, "output", "rekt");

        var selection = new ProgramSelection
        {
            Programs = SplitSelectors(programOptionValues),
            IncludeCallers = includeCallers,
            IncludeCallees = includeCallees,
        };

        ProgramSelectionResult result;
        var stagedCopybooks = 0;
        try
        {
            if (string.IsNullOrWhiteSpace(stageDir))
            {
                var resolver = new ProgramSelectionResolver(
                    ProgramSourceCatalog.FromStagingDirectory(stagingDir),
                    new FactsClosureSource(resolvedFactsDir));

                result = resolver.Resolve(selection);
            }
            else
            {
                var staged = new ConversionScopeStager(stagingDir, stageDir)
                    .Stage(selection, resolvedFactsDir, logger);

                result = staged.Selection;
                stagedCopybooks = staged.Copybooks;
            }
        }
        catch (InvalidOperationException ex)
        {
            Console.Error.WriteLine(ex.Message);
            return 2;
        }

        foreach (var target in result.UnresolvedCallTargets)
        {
            logger.LogWarning(
                "[ResolvePrograms] unresolved CALL target {Target} has no program in {StagingDir}; it cannot be converted.",
                target, stagingDir);
        }

        if (!string.IsNullOrWhiteSpace(manifestPath))
            SelectionManifestWriter.Write(manifestPath, stagingDir, resolvedFactsDir, selection, result);

        foreach (var program in result.Programs)
            Console.Out.WriteLine(program);

        var stagedNote = string.IsNullOrWhiteSpace(stageDir)
            ? ""
            : $" staged into {stageDir} with {stagedCopybooks} copybook(s);";

        Console.Error.WriteLine(
            $"resolve-programs: selected {result.Programs.Count} program(s);{stagedNote} " +
            $"{result.UnresolvedCallTargets.Count} unresolved CALL target(s).");

        return 0;
    }

    private static IReadOnlyList<string> SplitSelectors(IReadOnlyList<string> values) =>
        values
            .SelectMany(value => value.Split(',', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries))
            .ToList();

}
