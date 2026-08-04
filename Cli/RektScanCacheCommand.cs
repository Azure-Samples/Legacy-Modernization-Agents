using System.CommandLine;
using System.Text.Json;
using CobolToQuarkusMigration.Agents.Infrastructure.RektCache;
using CobolToQuarkusMigration.Helpers;
using Microsoft.Extensions.Logging;

namespace CobolToQuarkusMigration.Cli;

/// <summary>
/// Bash-callable CLI surface for the incremental REKT scan cache (PR2.b).
/// Two verbs:
/// <list type="bullet">
///   <item><c>plan &lt;staging-dir&gt;</c> — emits a newline-tagged plan to stdout.</item>
///   <item><c>record &lt;basename&gt; --outcome ...</c> — persists a parse result.</item>
///   <item><c>prune</c> — drops rows whose identity scheme is not the current one.</item>
/// </list>
/// The plan output is intentionally line-oriented (one decision per line, three
/// fields per line separated by '\t') so <c>doctor.sh</c> can consume it with
/// a single <c>while read</c> loop — no JSON parser required in bash.
/// </summary>
public static class RektScanCacheCommand
{
    public const string DefaultDbPath = "Data/rekt-scan.db";

    /// <summary>Identity scheme that the planner and cache currently agree on.</summary>
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

    // ─────────────────────────── plan ───────────────────────────

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

            // Optional artifact-existence sanity check — downgrades skip→parse for
            // programs whose REKT output JSONs are missing on disk. Closes the gap
            // documented in docs/p2-rekt-scan-cache.md §6.3.
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

    // ─────────────────────────── record ───────────────────────────

    private static Command BuildRecordCommand(ILoggerFactory loggerFactory)
    {
        var cmd = new Command("record", "Record a parse outcome into the cache.");

        var basenameArg = new Argument<string>("basename", "Program basename (with extension) that was parsed.");
        cmd.AddArgument(basenameArg);

        var outcomeOption = new Option<string>("--outcome", "Parse outcome: Full | NoDialect | RawAst | DepsOnly | Failed")
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

    // ─────────────────────────── record-batch ───────────────────────────

    /// <summary>
    /// Persists many parse outcomes in a single process — avoids paying the
    /// dotnet startup cost per program when doctor.sh has just parsed dozens
    /// or hundreds of files. Reads a manifest file with one
    /// <c>&lt;basename&gt;TAB&lt;outcome&gt;</c> line per program.
    /// </summary>
    private static Command BuildRecordBatchCommand(ILoggerFactory loggerFactory)
    {
        var cmd = new Command("record-batch", "Persist many parse outcomes from a TSV manifest in one process.");

        var manifestArg = new Argument<string>("manifest",
            "Path to a TSV file: <basename>\\t<outcome> per line. Outcome ∈ {Full, NoDialect, RawAst, DepsOnly, Failed}.");
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
                if (!Enum.TryParse<RektParseOutcome>(outcomeStr, ignoreCase: true, out var outcome))
                {
                    Console.Error.WriteLine($"Unknown outcome '{outcomeStr}' for {basename}");
                    skipped++;
                    continue;
                }

                // Re-derive the decision so the recorded snapshot matches what
                // the planner would see right now. If the file was deleted
                // between parse and record, this is a soft skip.
                if (graph.GetHash(basename) is null)
                {
                    Console.Error.WriteLine($"No graph entry for {basename} — file gone from staging? Skipping record.");
                    skipped++;
                    continue;
                }

                var plan = await planner.PlanAsync(new[] { basename });
                var decision = plan.ToParse.FirstOrDefault() ?? plan.ToSkip.First();
                await planner.RecordParseAsync(decision, outcome);
                ok++;
            }

            Console.Error.WriteLine($"rekt-scan-cache record-batch: recorded {ok}, skipped {skipped} (of {lines.Length} lines).");
        }, manifestArg, stagingDirOption, dbPathOption);

        return cmd;
    }

    // ─────────────────────────── prune ───────────────────────────

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

    // ─────────────────────────── helpers (internal for tests) ───────────────────────────

    /// <summary>Builds an in-memory copybook graph by reading every program + copybook from the staging dir.</summary>
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

    /// <summary>
    /// Returns true when the verify dir contains at least one REKT artifact for the
    /// given program — heuristic match on <c>&lt;stem&gt;</c> in filename + <c>.json</c> extension.
    /// </summary>
    internal static bool HasRektArtifacts(string verifyDir, string basename)
    {
        var stem = Path.GetFileNameWithoutExtension(basename);
        if (string.IsNullOrEmpty(stem)) return false;
        try
        {
            foreach (var f in Directory.EnumerateFiles(verifyDir, "*", SearchOption.TopDirectoryOnly))
            {
                var name = Path.GetFileName(f);
                if (name.Contains(stem, StringComparison.OrdinalIgnoreCase) &&
                    name.EndsWith(".json", StringComparison.OrdinalIgnoreCase))
                    return true;
            }
        }
        catch
        {
            // If we can't read the verify dir, assume artifacts present so we don't
            // re-parse needlessly — the next REKT run will surface the real error.
            return true;
        }
        return false;
    }
}

/// <summary>Constants for the current identity-scheme contract.</summary>
public static class CacheKeyIdentity
{
    /// <summary>Basename-only identity — see docs/basename-coupling-map.md.</summary>
    public const string V1Basename = "v1-basename";
}
