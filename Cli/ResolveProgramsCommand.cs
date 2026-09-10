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
                parsed.GetValueForOption(repoRootOption));
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
        string? repoRoot)
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

        var resolver = new ProgramSelectionResolver(
            ProgramSourceCatalog.FromStagingDirectory(stagingDir),
            new FactsClosureSource(resolvedFactsDir));

        ProgramSelectionResult result;
        try
        {
            result = resolver.Resolve(selection);
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
            WriteManifest(manifestPath, stagingDir, resolvedFactsDir, selection, result);

        foreach (var program in result.Programs)
            Console.Out.WriteLine(program);

        Console.Error.WriteLine(
            $"resolve-programs: selected {result.Programs.Count} program(s); " +
            $"{result.UnresolvedCallTargets.Count} unresolved CALL target(s).");

        return 0;
    }

    private static IReadOnlyList<string> SplitSelectors(IReadOnlyList<string> values) =>
        values
            .SelectMany(value => value.Split(',', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries))
            .ToList();

    private static void WriteManifest(
        string manifestPath,
        string stagingDir,
        string factsDir,
        ProgramSelection selection,
        ProgramSelectionResult result)
    {
        var manifest = new SelectionManifest
        {
            GeneratedUtc = DateTimeOffset.UtcNow,
            StagingDir = stagingDir,
            FactsDir = factsDir,
            Selectors = new SelectionManifestSelectors
            {
                Programs = selection.Programs,
                IncludeCallers = selection.IncludeCallers,
                IncludeCallees = selection.IncludeCallees,
            },
            Programs = result.Programs,
            Matches = result.Matches
                .Select(match => new SelectionManifestMatch(match.Program, match.Reason))
                .ToList(),
            UnresolvedCallTargets = result.UnresolvedCallTargets,
        };

        var directory = Path.GetDirectoryName(Path.GetFullPath(manifestPath));
        if (!string.IsNullOrEmpty(directory))
            Directory.CreateDirectory(directory);

        File.WriteAllText(
            manifestPath,
            JsonSerializer.Serialize(manifest, new JsonSerializerOptions { WriteIndented = true }));
    }
}

public sealed record SelectionManifest
{
    public const int CurrentSchemaVersion = 1;

    [JsonPropertyName("schemaVersion")]
    public int SchemaVersion { get; init; } = CurrentSchemaVersion;

    [JsonPropertyName("generatedUtc")]
    public DateTimeOffset GeneratedUtc { get; init; }

    [JsonPropertyName("stagingDir")]
    public string StagingDir { get; init; } = "";

    [JsonPropertyName("factsDir")]
    public string FactsDir { get; init; } = "";

    [JsonPropertyName("selectors")]
    public SelectionManifestSelectors Selectors { get; init; } = new();

    [JsonPropertyName("programs")]
    public IReadOnlyList<string> Programs { get; init; } = Array.Empty<string>();

    [JsonPropertyName("matches")]
    public IReadOnlyList<SelectionManifestMatch> Matches { get; init; } = Array.Empty<SelectionManifestMatch>();

    [JsonPropertyName("unresolvedCallTargets")]
    public IReadOnlyList<string> UnresolvedCallTargets { get; init; } = Array.Empty<string>();
}

public sealed record SelectionManifestSelectors
{
    [JsonPropertyName("programs")]
    public IReadOnlyList<string> Programs { get; init; } = Array.Empty<string>();

    [JsonPropertyName("includeCallers")]
    public bool IncludeCallers { get; init; }

    [JsonPropertyName("includeCallees")]
    public bool IncludeCallees { get; init; }
}

public sealed record SelectionManifestMatch(
    [property: JsonPropertyName("program")] string Program,
    [property: JsonPropertyName("reason")] string Reason);
