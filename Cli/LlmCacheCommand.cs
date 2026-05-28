using System.CommandLine;
using CobolToQuarkusMigration.Agents.Infrastructure.Caching;
using Microsoft.Extensions.Logging;

namespace CobolToQuarkusMigration.Cli;

/// <summary>
/// PR2.d CLI surface for response-cache housekeeping. The response cache itself
/// is opt-in via <see cref="LlmCacheGate"/>; this CLI just exposes its
/// <see cref="IResponseCache.PruneAsync"/> verb to the shell.
/// </summary>
public static class LlmCacheCommand
{
    public const string DefaultDbPath = "Data/llm-cache.db";

    public static Command Build(ILoggerFactory loggerFactory)
    {
        var root = new Command("llm-cache",
            "Maintenance verbs for the deterministic response cache (PR1).");
        root.AddCommand(BuildPruneCommand(loggerFactory));
        return root;
    }

    private static Command BuildPruneCommand(ILoggerFactory loggerFactory)
    {
        var cmd = new Command("prune",
            "Drop entries past TTL and optionally LRU-cap to a byte limit.");

        var dbPathOption = new Option<string>("--db", () => DefaultDbPath, "Cache DB path.");
        cmd.AddOption(dbPathOption);

        var ttlDaysOption = new Option<int>("--ttl-days", () => 7,
            "Delete cache entries older than this many days. Default 7.");
        cmd.AddOption(ttlDaysOption);

        var maxBytesOption = new Option<long?>("--max-bytes",
            "Optional size cap; when exceeded, oldest entries by last_hit_at_utc are evicted until under the cap.")
        { Arity = ArgumentArity.ZeroOrOne };
        cmd.AddOption(maxBytesOption);

        cmd.SetHandler(async (string dbPath, int ttlDays, long? maxBytes) =>
        {
            var logger = loggerFactory.CreateLogger("LlmCache.prune");
            var cache = new SqliteResponseCache(dbPath, logger);
            var deleted = await cache.PruneAsync(TimeSpan.FromDays(ttlDays), maxBytes);
            Console.Error.WriteLine(
                $"llm-cache prune: deleted {deleted} entries (ttlDays={ttlDays}, maxBytes={maxBytes?.ToString() ?? "-"}).");
        }, dbPathOption, ttlDaysOption, maxBytesOption);

        return cmd;
    }
}
