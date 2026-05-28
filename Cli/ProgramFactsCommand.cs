using System.CommandLine;
using System.Text.Json;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using CobolToQuarkusMigration.Agents.Infrastructure.RektCache;
using CobolToQuarkusMigration.Helpers;
using Microsoft.Extensions.Logging;

namespace CobolToQuarkusMigration.Cli;

/// <summary>
/// PR3 CLI surface for the <c>program-facts.json</c> extractor.
/// </summary>
/// <remarks>
/// Two verbs:
/// <list type="bullet">
///   <item><c>extract &lt;staging-dir&gt;</c> — builds <c>&lt;stem&gt;.facts.json</c>
///         for every program in the staging dir (or those in <c>--programs</c>).
///         Writes to <c>--output-dir</c> (defaults to <c>output/rekt/</c>).</item>
///   <item><c>read &lt;facts-json&gt;</c> — pretty-prints a single facts file
///         for human inspection. Useful for debugging.</item>
/// </list>
/// </remarks>
public static class ProgramFactsCommand
{
    public const string DefaultScanCacheDb = "Data/rekt-scan.db";

    public static Command Build(ILoggerFactory loggerFactory)
    {
        var root = new Command("program-facts",
            "Extract curated REKT facts (program-facts.json) for every program in a staging dir.");

        root.AddCommand(BuildExtractCommand(loggerFactory));
        root.AddCommand(BuildReadCommand(loggerFactory));

        return root;
    }

    private static Command BuildExtractCommand(ILoggerFactory loggerFactory)
    {
        var cmd = new Command("extract", "Build <stem>.facts.json for every program in the staging dir.");

        var stagingDirArg = new Argument<string>("staging-dir", "Directory containing preprocessed COBOL programs and copybooks (used as the source-bytes input).");
        cmd.AddArgument(stagingDirArg);

        var rektDirOption = new Option<string?>("--rekt-dir", "REKT output directory containing flow-ast / flow-cfg / *-deps JSONs. Defaults to <repo-root>/output/rekt.")
        { Arity = ArgumentArity.ZeroOrOne };
        cmd.AddOption(rektDirOption);

        var outputDirOption = new Option<string?>("--output-dir", "Directory to write <stem>.facts.json files. Defaults to --rekt-dir.")
        { Arity = ArgumentArity.ZeroOrOne };
        cmd.AddOption(outputDirOption);

        var programsOption = new Option<string?>("--programs", "Comma-separated basenames (with extension) to extract for. Defaults to every program in the staging dir.")
        { Arity = ArgumentArity.ZeroOrOne };
        cmd.AddOption(programsOption);

        var scanCacheDbOption = new Option<string>("--scan-cache-db", () => DefaultScanCacheDb,
            "Optional rekt-scan cache DB. When supplied, confidence is drawn from the cache entry's parse outcome.");
        cmd.AddOption(scanCacheDbOption);

        var repoRootOption = new Option<string?>("--repo-root", "Repo root (defaults to the current working directory).")
        { Arity = ArgumentArity.ZeroOrOne };
        cmd.AddOption(repoRootOption);

        cmd.SetHandler(async (string stagingDir, string? rektDir, string? outputDir, string? programs, string scanCacheDb, string? repoRoot) =>
        {
            var logger = loggerFactory.CreateLogger("ProgramFacts.extract");
            var resolvedRepoRoot = repoRoot ?? Directory.GetCurrentDirectory();
            var resolvedRektDir = rektDir ?? Path.Combine(resolvedRepoRoot, "output", "rekt");
            var resolvedOutputDir = outputDir ?? resolvedRektDir;

            if (!Directory.Exists(stagingDir))
            {
                Console.Error.WriteLine($"Staging dir not found: {stagingDir}");
                Environment.ExitCode = 2;
                return;
            }

            IRektScanCache? cache = null;
            if (!string.IsNullOrWhiteSpace(scanCacheDb) && File.Exists(scanCacheDb))
            {
                cache = new SqliteRektScanCache(scanCacheDb, logger);
            }

            var allPrograms = SourceTypeRegistry.EnumerateProgramFiles(stagingDir)
                .Select(Path.GetFileName)
                .Where(n => !string.IsNullOrEmpty(n))
                .Select(n => n!)
                .OrderBy(n => n, StringComparer.OrdinalIgnoreCase)
                .ToList();

            var targets = programs is null
                ? allPrograms
                : programs.Split(',', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries).ToList();

            var extractor = new ProgramFactsExtractor(
                repoRoot: resolvedRepoRoot,
                stagingDir: stagingDir,
                rektDir: resolvedRektDir,
                scanCache: cache,
                logger: logger);

            var written = await extractor.ExtractAllAsync(targets, resolvedOutputDir);
            Console.Error.WriteLine($"program-facts extract: wrote {written} *.facts.json to {resolvedOutputDir}");
        }, stagingDirArg, rektDirOption, outputDirOption, programsOption, scanCacheDbOption, repoRootOption);

        return cmd;
    }

    private static Command BuildReadCommand(ILoggerFactory loggerFactory)
    {
        var cmd = new Command("read", "Pretty-print a single program-facts.json file.");
        var pathArg = new Argument<string>("facts-json", "Path to a *.facts.json file.");
        cmd.AddArgument(pathArg);

        cmd.SetHandler((string path) =>
        {
            if (!File.Exists(path))
            {
                Console.Error.WriteLine($"Not found: {path}");
                Environment.ExitCode = 2;
                return;
            }
            var json = File.ReadAllText(path);
            // Re-serialise to normalise indentation and field ordering.
            try
            {
                using var doc = JsonDocument.Parse(json);
                var pretty = JsonSerializer.Serialize(doc.RootElement, new JsonSerializerOptions
                {
                    WriteIndented = true,
                });
                Console.Out.WriteLine(pretty);
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine($"Failed to parse {path}: {ex.Message}");
                Environment.ExitCode = 3;
            }
        }, pathArg);

        return cmd;
    }
}
