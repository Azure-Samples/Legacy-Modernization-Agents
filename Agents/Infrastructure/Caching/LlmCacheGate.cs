using CobolToQuarkusMigration.Agents.Infrastructure.Caching;
using CobolToQuarkusMigration.Helpers;
using Microsoft.Extensions.Logging;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Caching;

/// <summary>
/// Process-wide, opt-in response cache wiring. The cache is built lazily on
/// first <see cref="TryGet"/> call and only when <c>_LLM_CACHE_ENABLED=true</c>.
/// </summary>
/// <remarks>
/// The factory is intentionally a thin static so the integration story per agent is:
/// <code>
/// var cache = LlmCacheGate.Cache;
/// var enabled = LlmCacheGate.Enabled;
/// await CachedLlmInvoker.GetOrInvokeAsync(cache, enabled, deterministic: true, key, invoke, logger, ct);
/// </code>
/// No DI plumbing, no constructor changes to existing agents. When the cache is
/// not enabled, <see cref="Cache"/> may be null and <see cref="CachedLlmInvoker"/>
/// degrades to direct invoke.
/// </remarks>
public static class LlmCacheGate
{
    private const string EnabledEnvVar = "_LLM_CACHE_ENABLED";
    private const string DbPathEnvVar = "_LLM_CACHE_DB";
    private const string DefaultDbPath = "Data/llm-cache.db";

    private static readonly object _lock = new();
    private static IResponseCache? _cache;
    private static bool _initAttempted;

    /// <summary>True when the cache is configured to run (env-var opt-in).</summary>
    public static bool Enabled => string.Equals(
        Environment.GetEnvironmentVariable(EnabledEnvVar), "true",
        StringComparison.OrdinalIgnoreCase);

    /// <summary>The process-wide cache, or null when disabled or not initialised.</summary>
    public static IResponseCache? Cache
    {
        get
        {
            if (!Enabled) return null;
            EnsureInit(logger: null);
            return _cache;
        }
    }

    /// <summary>
    /// Eagerly initialises the cache with a logger so the first store/lookup is
    /// not silent. Safe to call multiple times.
    /// </summary>
    public static IResponseCache? EnsureCache(ILogger? logger)
    {
        if (!Enabled) return null;
        EnsureInit(logger);
        return _cache;
    }

    private static void EnsureInit(ILogger? logger)
    {
        if (_initAttempted) return;
        lock (_lock)
        {
            if (_initAttempted) return;
            _initAttempted = true;
            try
            {
                var path = Environment.GetEnvironmentVariable(DbPathEnvVar);
                if (string.IsNullOrWhiteSpace(path)) path = DefaultDbPath;
                _cache = new SqliteResponseCache(path, logger);
                logger?.LogInformation(
                    "[LlmCacheGate] Response cache enabled at {Path}", path);
            }
            catch (Exception ex)
            {
                // Cache failure must never crash a conversion. Fail open.
                logger?.LogWarning(ex, "[LlmCacheGate] Failed to initialise response cache — running uncached");
                _cache = null;
            }
        }
    }

    /// <summary>
    /// Test-only reset. Public so test fixtures can install a custom in-memory
    /// or temp-file cache; not intended for production paths.
    /// </summary>
    internal static void ResetForTests(IResponseCache? cache)
    {
        lock (_lock)
        {
            _cache = cache;
            _initAttempted = cache is not null;
        }
    }
}
