using System.CommandLine;
using System.Text.Json;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using CobolToQuarkusMigration.Agents.Infrastructure.RektCache;
using CobolToQuarkusMigration.Helpers;
using Microsoft.Extensions.Logging;

namespace CobolToQuarkusMigration.Cli;

public static class ProgramFactsCommand
{
    public const string DefaultScanCacheDb = "Data/rekt-scan.db";

    public static Command Build(ILoggerFactory loggerFactory)
    {
        var root = new Command("program-facts",
            "Extract curated REKT facts (program-facts.json) for every program in a staging dir.");

        root.AddCommand(BuildExtractCommand(loggerFactory));
        root.AddCommand(BuildReadCommand(loggerFactory));
        root.AddCommand(BuildPruneOrphansCommand(loggerFactory));

        return root;
    }

    private static Command BuildPruneOrphansCommand(ILoggerFactory loggerFactory)
    {
        var cmd = new Command("prune-orphans",
            "Delete *.facts.json files whose source-relative program is no longer present in the staging dir.");

        var factsDirArg = new Argument<string>("facts-dir", "Directory holding existing *.facts.json files.");
        cmd.AddArgument(factsDirArg);

        var stagingDirOption = new Option<string>("--staging-dir",
            "Authoritative source-of-truth dir. Any *.facts.json whose stored source identity has no matching program file here is deleted.")
        { IsRequired = true };
        cmd.AddOption(stagingDirOption);

        var dryRunOption = new Option<bool>("--dry-run", () => false,
            "Print what would be deleted without removing anything.");
        cmd.AddOption(dryRunOption);

        cmd.SetHandler((string factsDir, string stagingDir, bool dryRun) =>
        {
            var logger = loggerFactory.CreateLogger("ProgramFacts.prune-orphans");
            if (!Directory.Exists(factsDir))
            {
                Console.Error.WriteLine($"facts dir not found: {factsDir}");
                Environment.ExitCode = 2;
                return;
            }
            if (!Directory.Exists(stagingDir))
            {
                Console.Error.WriteLine($"staging dir not found: {stagingDir}");
                Environment.ExitCode = 2;
                return;
            }

            var catalog = ProgramSourceCatalog.FromStagingDirectory(stagingDir);

            var deleted = 0;
            foreach (var f in Directory.EnumerateFiles(factsDir, "*.facts.json", SearchOption.AllDirectories))
            {
                var resolvedProgram = catalog.ResolveFactsFileToProgram(f, factsDir);
                if (resolvedProgram is not null) continue;

                var artifactIdentity = ProgramFactsArtifactLocator.TryGetProgramRelativePath(f, factsDir)
                    ?? Path.GetFileNameWithoutExtension(Path.GetFileNameWithoutExtension(f));
                logger.LogInformation(
                    "[ProgramFacts] decision=prune-orphan path={Path} identity={Identity} {Suffix}",
                    f, artifactIdentity, dryRun ? "(dry-run)" : "");
                if (!dryRun)
                {
                    try { File.Delete(f); deleted++; }
                    catch (Exception ex)
                    {
                        logger.LogWarning(ex,
                            "[ProgramFacts] failed to delete orphan {Path}: {Msg}", f, ex.Message);
                    }
                }
                else deleted++;
            }

            Console.Error.WriteLine(
                $"program-facts prune-orphans: {(dryRun ? "would delete" : "deleted")} {deleted} orphan(s).");
        }, factsDirArg, stagingDirOption, dryRunOption);

        return cmd;
    }

    private static Command BuildExtractCommand(ILoggerFactory loggerFactory)
    {
        var cmd = new Command("extract", "Build <source-relative>.facts.json for every program in the staging dir.");

        var stagingDirArg = new Argument<string>("staging-dir", "Directory containing preprocessed COBOL programs and copybooks (used as the source-bytes input).");
        cmd.AddArgument(stagingDirArg);

        var rektDirOption = new Option<string?>("--rekt-dir", "REKT output directory containing flow-ast / flow-cfg / *-deps JSONs. Defaults to <repo-root>/output/rekt.")
        { Arity = ArgumentArity.ZeroOrOne };
        cmd.AddOption(rektDirOption);

        var outputDirOption = new Option<string?>("--output-dir", "Directory to write <source-relative>.facts.json files. Defaults to --rekt-dir.")
        { Arity = ArgumentArity.ZeroOrOne };
        cmd.AddOption(outputDirOption);

        var programsOption = new Option<string?>("--programs", "Comma-separated source-relative paths, basenames, or stems to extract for. Basename/stem selectors must be unambiguous. Defaults to every program in the staging dir.")
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

            var catalog = ProgramSourceCatalog.FromStagingDirectory(stagingDir);

            IReadOnlyList<string> targets;
            try
            {
                targets = catalog.ResolveSelectors(programs);
            }
            catch (InvalidOperationException ex)
            {
                Console.Error.WriteLine(ex.Message);
                Environment.ExitCode = 2;
                return;
            }

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
