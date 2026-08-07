using System.CommandLine;
using System.Text.Json;
using CobolToQuarkusMigration.Agents.Infrastructure.RektCache;
using CobolToQuarkusMigration.Helpers;
using Microsoft.Extensions.Logging;

namespace CobolToQuarkusMigration.Cli;

public static class RektScanCacheCommand
{
    public const string DefaultDbPath = "Data/rekt-scan.db";

    public const string IdentityScheme = CacheKeyIdentity.V1Basename;

    public static Command Build(ILoggerFactory loggerFactory)
    {
        var root = new Command("rekt-scan-cache",
            "Incremental REKT scan cache. Inspects a staging dir and emits a parse/skip plan, or records a parse outcome.");

        root.AddCommand(BuildPlanCommand(loggerFactory));
        root.AddCommand(BuildRecordCommand(loggerFactory));
        root.AddCommand(BuildRecordBatchCommand(loggerFactory));
        root.AddCommand(BuildPruneCommand(loggerFactory));

        return root;
    }

    private static Command BuildPlanCommand(ILoggerFactory loggerFactory)
    {
        var cmd = new Command("plan", "Emit a parse/skip plan for the supplied staging dir to stdout.");

        var stagingDirArg = new Argument<string>("staging-dir", "Directory containing preprocessed COBOL programs and copybooks.");
        cmd.AddArgument(stagingDirArg);

        var dbPathOption = new Option<string>("--db", () => DefaultDbPath, "Cache DB path.");
        cmd.AddOption(dbPathOption);

        var programsOnlyOption = new Option<string?>("--programs",
            "Comma-separated list of program basenames to plan for. When omitted, all programs in the staging dir are planned.")
        { Arity = ArgumentArity.ZeroOrOne };
        cmd.AddOption(programsOnlyOption);

        var verifyArtifactsOption = new Option<string?>("--verify-artifacts-in",
            "If set, skip decisions are downgraded to parse when the named output dir lacks <stem>.* artifacts for the program. Use this from doctor.sh so a deleted output/rekt/ doesn't silently confuse downstream.")
        { Arity = ArgumentArity.ZeroOrOne };
        cmd.AddOption(verifyArtifactsOption);

        cmd.SetHandler(async (string stagingDir, string dbPath, string? programs, string? verifyDir) =>
        {
            var logger = loggerFactory.CreateLogger("RektScanCache.plan");
            var cache = new SqliteRektScanCache(dbPath, logger);
            var graph = BuildGraphFromStagingDir(stagingDir, logger);

            var allPrograms = SourceTypeRegistry.EnumerateProgramFiles(stagingDir)
                .Select(Path.GetFileName)
                .Where(n => !string.IsNullOrEmpty(n))
                .Select(n => n!)
                .OrderBy(n => n, StringComparer.OrdinalIgnoreCase)
                .ToList();

            var targets = programs is null
                ? allPrograms
                : programs.Split(',', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries).ToList();

            var planner = new IncrementalScanPlanner(cache, graph, IdentityScheme, logger);
            var plan = await planner.PlanAsync(targets);

            // Reparse cached programs whose parser artifacts are missing.
            var toSkip = plan.ToSkip.ToList();
            var toParse = plan.ToParse.ToList();
            if (!string.IsNullOrEmpty(verifyDir) && Directory.Exists(verifyDir))
            {
                for (int i = toSkip.Count - 1; i >= 0; i--)
                {
                    var d = toSkip[i];
                    if (!HasRektArtifacts(verifyDir, d.Basename))
                    {
                        toSkip.RemoveAt(i);
                        toParse.Add(d with { MustParse = true, Reason = ScanReason.NotCached });
                        logger.LogInformation(
                            "[{Event}] basename={Basename} decision=parse reason=artifacts-missing-on-disk verifyDir={Dir}",
                            IncrementalScanPlanner.LogEventName, d.Basename, verifyDir);
                    }
                }
            }

            // Emit a stable line-oriented format: <action>\t<basename>\t<reason>
            // Bash consumes with `while IFS=$'\t' read -r action basename reason`.
            foreach (var d in toParse)
                Console.Out.WriteLine($"parse\t{d.Basename}\t{d.Reason?.ToString() ?? "Unknown"}");
            foreach (var d in toSkip)
                Console.Out.WriteLine($"skip\t{d.Basename}\t-");

            // Summary to stderr so stdout stays machine-clean.
            Console.Error.WriteLine(
                $"rekt-scan-cache plan: {toParse.Count} to parse, {toSkip.Count} to skip " +
                $"(total considered: {targets.Count})");
        }, stagingDirArg, dbPathOption, programsOnlyOption, verifyArtifactsOption);

        return cmd;
    }

    private static Command BuildRecordCommand(ILoggerFactory loggerFactory)
    {
        var cmd = new Command("record", "Record a parse outcome into the cache.");

        var basenameArg = new Argument<string>("basename", "Program basename (with extension) that was parsed.");
        cmd.AddArgument(basenameArg);

        var outcomeOption = new Option<string>("--outcome", "Parse outcome: Full | StubBacked | NoDialect | RawAst | DepsOnly | Failed")
        { IsRequired = true };
        cmd.AddOption(outcomeOption);

        var dbPathOption = new Option<string>("--db", () => DefaultDbPath, "Cache DB path.");
        cmd.AddOption(dbPathOption);

        var stagingDirOption = new Option<string>("--staging-dir",
            "Staging dir — used to re-hash the preprocessed bytes and the dependency snapshot deterministically.")
        { IsRequired = true };
        cmd.AddOption(stagingDirOption);

        var warningsOption = new Option<string?>("--warnings-file",
            "Optional file containing warnings to persist (one per line).")
        { Arity = ArgumentArity.ZeroOrOne };
        cmd.AddOption(warningsOption);

        cmd.SetHandler(async (string basename, string outcomeStr, string dbPath, string stagingDir, string? warningsFile) =>
        {
            var logger = loggerFactory.CreateLogger("RektScanCache.record");
            if (!Enum.TryParse<RektParseOutcome>(outcomeStr, ignoreCase: true, out var outcome))
            {
                Console.Error.WriteLine($"Unknown outcome '{outcomeStr}'. Use one of: " +
                    string.Join(", ", Enum.GetNames<RektParseOutcome>()));
                Environment.ExitCode = 2;
                return;
            }

            var cache = new SqliteRektScanCache(dbPath, logger);
            var graph = BuildGraphFromStagingDir(stagingDir, logger);
            var planner = new IncrementalScanPlanner(cache, graph, IdentityScheme, logger);

            // Re-derive the decision so we record against the exact same hash /
            // snapshot the planner saw — avoids drift if the file changed mid-parse.
            var plan = await planner.PlanAsync(new[] { basename });
            var decision = plan.ToParse.FirstOrDefault() ?? plan.ToSkip.First();

            IReadOnlyList<string>? warnings = null;
            if (!string.IsNullOrEmpty(warningsFile) && File.Exists(warningsFile))
            {
                warnings = (await File.ReadAllLinesAsync(warningsFile))
                    .Where(l => !string.IsNullOrWhiteSpace(l))
                    .ToList();
            }

            await planner.RecordParseAsync(decision, outcome, warnings);
        }, basenameArg, outcomeOption, dbPathOption, stagingDirOption, warningsOption);

        return cmd;
    }

    private static Command BuildRecordBatchCommand(ILoggerFactory loggerFactory)
    {
        var cmd = new Command("record-batch", "Persist many parse outcomes from a TSV manifest in one process.");

        var manifestArg = new Argument<string>("manifest",
            "Path to a TSV file: <basename>\\t<outcome>\\t<warning>|<warning> per line. " +
            "Outcome ∈ {Full, StubBacked, NoDialect, RawAst, DepsOnly, Failed}.");
        cmd.AddArgument(manifestArg);

        var stagingDirOption = new Option<string>("--staging-dir",
            "Staging dir — used to re-hash the preprocessed bytes and the dependency snapshot deterministically.")
        { IsRequired = true };
        cmd.AddOption(stagingDirOption);

        var dbPathOption = new Option<string>("--db", () => DefaultDbPath, "Cache DB path.");
        cmd.AddOption(dbPathOption);

        cmd.SetHandler(async (string manifest, string stagingDir, string dbPath) =>
        {
            var logger = loggerFactory.CreateLogger("RektScanCache.record-batch");
            if (!File.Exists(manifest))
            {
                Console.Error.WriteLine($"Manifest not found: {manifest}");
                Environment.ExitCode = 2;
                return;
            }

            var cache = new SqliteRektScanCache(dbPath, logger);
            var graph = BuildGraphFromStagingDir(stagingDir, logger);
            var planner = new IncrementalScanPlanner(cache, graph, IdentityScheme, logger);

            var lines = await File.ReadAllLinesAsync(manifest);
            var ok = 0;
            var skipped = 0;
            foreach (var raw in lines)
            {
                var line = raw.Trim();
                if (string.IsNullOrEmpty(line) || line.StartsWith('#')) continue;
                var parts = line.Split('\t');
                if (parts.Length < 2)
                {
                    Console.Error.WriteLine($"Skipping malformed manifest line: {line}");
                    skipped++;
                    continue;
                }
                var basename = parts[0].Trim();
                var outcomeStr = parts[1].Trim();
                var warnings = parts.Length >= 3
                    ? parts[2]
                        .Split('|', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries)
                        .ToList()
                    : null;
                if (!Enum.TryParse<RektParseOutcome>(outcomeStr, ignoreCase: true, out var outcome))
                {
                    Console.Error.WriteLine($"Unknown outcome '{outcomeStr}' for {basename}");
                    skipped++;
                    continue;
                }

                // Recompute the snapshot; a file deleted after parsing is a soft skip.
                if (graph.GetHash(basename) is null)
                {
                    Console.Error.WriteLine($"No graph entry for {basename} — file gone from staging? Skipping record.");
                    skipped++;
                    continue;
                }

                var plan = await planner.PlanAsync(new[] { basename });
                var decision = plan.ToParse.FirstOrDefault() ?? plan.ToSkip.First();
                await planner.RecordParseAsync(decision, outcome, warnings);
                ok++;
            }

            Console.Error.WriteLine($"rekt-scan-cache record-batch: recorded {ok}, skipped {skipped} (of {lines.Length} lines).");
        }, manifestArg, stagingDirOption, dbPathOption);

        return cmd;
    }

    private static Command BuildPruneCommand(ILoggerFactory loggerFactory)
    {
        var cmd = new Command("prune",
            "Cache housekeeping: drop entries by age, by row cap, by stale semantic version, or by old identity scheme.");

        var dbPathOption = new Option<string>("--db", () => DefaultDbPath, "Cache DB path.");
        cmd.AddOption(dbPathOption);

        var ttlDaysOption = new Option<int?>("--ttl-days",
            "Delete entries whose parsed_at_utc is older than this many days.")
        { Arity = ArgumentArity.ZeroOrOne };
        cmd.AddOption(ttlDaysOption);

        var maxEntriesOption = new Option<int?>("--max-entries",
            "If the cache holds more than N rows, delete the oldest until the cap is met.")
        { Arity = ArgumentArity.ZeroOrOne };
        cmd.AddOption(maxEntriesOption);

        var dropStaleSemanticOption = new Option<bool>("--drop-stale-semantic", () => false,
            "Delete rows whose stored semantic-invalidation version is not the current one.");
        cmd.AddOption(dropStaleSemanticOption);

        var dropOtherIdentityOption = new Option<bool>("--drop-other-identity", () => false,
            "Delete rows whose identity scheme is not the current one (post-migration cleanup).");
        cmd.AddOption(dropOtherIdentityOption);

        cmd.SetHandler(async (string dbPath, int? ttlDays, int? maxEntries,
                              bool dropStaleSemantic, bool dropOtherIdentity) =>
        {
            var logger = loggerFactory.CreateLogger("RektScanCache.prune");
            var cache = new SqliteRektScanCache(dbPath, logger);
            var total = 0;
            if (ttlDays is int days && days > 0)
                total += await cache.PruneByAgeAsync(TimeSpan.FromDays(days));
            if (dropStaleSemantic)
                total += await cache.PruneStaleSemanticVersionsAsync();
            if (dropOtherIdentity)
                total += await cache.PruneOtherIdentitySchemesAsync(IdentityScheme);
            if (maxEntries is int cap && cap >= 0)
                total += await cache.PruneToMaxEntriesAsync(cap);

            if (ttlDays is null && maxEntries is null && !dropStaleSemantic && !dropOtherIdentity)
            {
                Console.Error.WriteLine(
                    "rekt-scan-cache prune: no policy supplied. " +
                    "Pass at least one of --ttl-days, --max-entries, --drop-stale-semantic, --drop-other-identity.");
                Environment.ExitCode = 2;
                return;
            }

            Console.Error.WriteLine($"rekt-scan-cache prune: deleted {total} row(s) total.");
        }, dbPathOption, ttlDaysOption, maxEntriesOption, dropStaleSemanticOption, dropOtherIdentityOption);

        return cmd;
    }

    internal static RektCopybookGraph BuildGraphFromStagingDir(string stagingDir, ILogger logger)
    {
        if (!Directory.Exists(stagingDir))
            throw new DirectoryNotFoundException($"Staging dir not found: {stagingDir}");

        var graph = new RektCopybookGraph();
        var copybooks = SourceTypeRegistry.EnumerateCopybookFiles(stagingDir).ToList();
        var programs = SourceTypeRegistry.EnumerateProgramFiles(stagingDir).ToList();

        foreach (var c in copybooks)
        {
            try { graph.AddFile(Path.GetFileName(c), File.ReadAllText(c), isCopybook: true); }
            catch (Exception ex) { logger.LogWarning(ex, "Skipping unreadable copybook {Path}", c); }
        }
        foreach (var p in programs)
        {
            try { graph.AddFile(Path.GetFileName(p), File.ReadAllText(p), isCopybook: false); }
            catch (Exception ex) { logger.LogWarning(ex, "Skipping unreadable program {Path}", p); }
        }
        return graph;
    }

    internal static bool HasRektArtifacts(string verifyDir, string basename)
    {
        var stem = Path.GetFileNameWithoutExtension(basename);
        if (string.IsNullOrEmpty(stem)) return false;

        var sourceName = Path.GetFileName(basename);
        var flatCandidates = new[]
        {
            $"flow-ast-{stem}.json",
            $"flow-ast-{stem}.cbl.json",
            $"flow-cfg-{stem}.json",
            $"flow-cfg-{stem}.cbl.json",
            $"cfg-{stem}.cbl.json",
            $"flow-data-{stem}.json",
            $"flow-data-{stem}.cbl.json",
            $"{stem}.cbl-data.json",
            $"{stem}-deps.json",
            $"{stem}.cbl-deps.json",
            $"{sourceName}-deps.json",
        };

        try
        {
            var existingFiles = Directory.EnumerateFiles(
                    verifyDir,
                    "*",
                    SearchOption.AllDirectories)
                .Select(Path.GetFileName)
                .Where(name => !string.IsNullOrEmpty(name))
                .ToHashSet(StringComparer.OrdinalIgnoreCase);
            if (flatCandidates.Any(existingFiles.Contains))
                return true;

            var reportDirs = new[]
            {
                $"{sourceName}.report",
                $"{stem}.cbl.report",
                $"{stem}.CBL.report",
                $"{stem}.report",
            };

            var existingDirectories = Directory.EnumerateDirectories(
                    verifyDir,
                    "*",
                    SearchOption.AllDirectories)
                .ToList();
            foreach (var reportDir in reportDirs.Distinct(StringComparer.OrdinalIgnoreCase))
            {
                var path = existingDirectories.FirstOrDefault(dir =>
                    string.Equals(
                        Path.GetFileName(dir),
                        reportDir,
                        StringComparison.OrdinalIgnoreCase));
                if (path is not null
                    && Directory.EnumerateFiles(path, "*.json", SearchOption.AllDirectories).Any())
                    return true;
            }
        }
        catch
        {
            return false;
        }
        return false;
    }
}

public static class CacheKeyIdentity
{
    public const string V1Basename = "v1-basename";
}
