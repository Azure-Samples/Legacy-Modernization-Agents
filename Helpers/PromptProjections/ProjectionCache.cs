using System.Globalization;
using System.Text.Json;
using Microsoft.Data.Sqlite;
using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;

namespace CobolToQuarkusMigration.Helpers.PromptProjections;

// Projection cache failures fall back to rebuilding the projection.
public static class ProjectionCache
{
    private const int StorageSchemaVersion = 1;
    private const string LogEventName = "ProjectionCache";

    private static readonly object _initLock = new();
    private static bool _schemaReady;
    private static string? _resolvedDbPath;

    private static readonly JsonSerializerOptions _canonicalJson = new()
    {
        // Canonical: no whitespace, fixed property naming, predictable null handling.
        WriteIndented = false,
        DefaultIgnoreCondition = System.Text.Json.Serialization.JsonIgnoreCondition.Never,
    };

    public static (string Block, string InputHash, string ProjectionHash, bool WasHit) GetOrBuild(
        string targetLanguage,
        ProgramFacts facts,
        Func<string> builder,
        int? runId = null,
        ILogger? logger = null)
    {
        var enabled = !string.Equals(
            Environment.GetEnvironmentVariable("_PROJECTION_CACHE_DISABLED"),
            "true",
            StringComparison.OrdinalIgnoreCase);

        var inputHash = ComputeInputHash(targetLanguage, facts);

        if (!enabled)
        {
            var block = builder();
            var projectionHash = CanonicalHasher.HashUtf8(block);
            EmitCacheEvent(runId, targetLanguage, inputHash, projectionHash, "bypass-disabled", facts.Basename);
            return (block, inputHash, projectionHash, false);
        }

        try
        {
            EnsureSchema(logger);

            // Lookup
            using (var conn = Open())
            using (var read = conn.CreateCommand())
            {
                read.CommandText = "SELECT projection_block, output_hash FROM projection_cache WHERE input_hash = $h LIMIT 1";
                read.Parameters.AddWithValue("$h", inputHash);
                using var reader = read.ExecuteReader();
                if (reader.Read())
                {
                    var cachedBlock = reader.GetString(0);
                    var storedHash = reader.GetString(1);
                    // Update hit counters in a non-blocking way (best-effort).
                    TryUpdateHitCounters(inputHash, logger);
                    EmitCacheEvent(runId, targetLanguage, inputHash, storedHash, "hit", facts.Basename);
                    logger?.LogInformation(
                        "[{Event}] runId={RunId} decision=hit lang={Lang} inputHash={InHash} outputHash={OutHash} basename={Basename}",
                        LogEventName, runId, targetLanguage, inputHash.Substring(0, 12), storedHash.Substring(0, 12), facts.Basename);
                    return (cachedBlock, inputHash, storedHash, true);
                }
            }

            // Miss → build → store
            var fresh = builder();
            var freshHash = CanonicalHasher.HashUtf8(fresh);
            try
            {
                using var conn2 = Open();
                using var ins = conn2.CreateCommand();
                ins.CommandText = @"
                    INSERT OR REPLACE INTO projection_cache
                        (input_hash, target_language, schema_version, basename,
                         identity_scheme_version, projection_block, output_hash,
                         created_at_utc, last_hit_at_utc, hit_count, byte_size)
                    VALUES ($h, $lang, $ver, $bn, $ident, $block, $oh,
                            $ts, $ts, 0, $size);";
                var now = DateTime.UtcNow.ToString("o", CultureInfo.InvariantCulture);
                ins.Parameters.AddWithValue("$h", inputHash);
                ins.Parameters.AddWithValue("$lang", targetLanguage);
                ins.Parameters.AddWithValue("$ver", facts.SchemaVersion);
                ins.Parameters.AddWithValue("$bn", (object?)facts.Basename ?? DBNull.Value);
                ins.Parameters.AddWithValue("$ident", facts.IdentitySchemeVersion ?? "v1-basename");
                ins.Parameters.AddWithValue("$block", fresh);
                ins.Parameters.AddWithValue("$oh", freshHash);
                ins.Parameters.AddWithValue("$ts", now);
                ins.Parameters.AddWithValue("$size", System.Text.Encoding.UTF8.GetByteCount(fresh));
                ins.ExecuteNonQuery();
                EmitCacheEvent(runId, targetLanguage, inputHash, freshHash, "miss-store", facts.Basename);
                logger?.LogInformation(
                    "[{Event}] runId={RunId} decision=miss-store lang={Lang} inputHash={InHash} outputHash={OutHash} basename={Basename} bytes={Bytes}",
                    LogEventName, runId, targetLanguage, inputHash.Substring(0, 12), freshHash.Substring(0, 12), facts.Basename, System.Text.Encoding.UTF8.GetByteCount(fresh));
            }
            catch (Exception ex)
            {
                logger?.LogWarning("[{Event}] Failed to persist cache entry for {Basename}: {Msg}",
                    LogEventName, facts.Basename, ex.Message);
                EmitCacheEvent(runId, targetLanguage, inputHash, freshHash, "miss-store-failed", facts.Basename);
            }
            return (fresh, inputHash, freshHash, false);
        }
        catch (Exception ex)
        {
            logger?.LogWarning("[{Event}] Cache lookup failed for {Basename}: {Msg} — falling through to builder",
                LogEventName, facts.Basename, ex.Message);
            var block = builder();
            var hash = CanonicalHasher.HashUtf8(block);
            EmitCacheEvent(runId, targetLanguage, inputHash, hash, "error-fallback", facts.Basename);
            return (block, inputHash, hash, false);
        }
    }

    private static string ComputeInputHash(string targetLanguage, ProgramFacts facts)
    {
        var canonical = JsonSerializer.Serialize(facts, _canonicalJson);
        return CanonicalHasher.HashFields(
            targetLanguage,
            facts.SchemaVersion.ToString(CultureInfo.InvariantCulture),
            facts.IdentitySchemeVersion ?? "v1-basename",
            canonical);
    }

    private static string ResolveDbPath()
    {
        if (_resolvedDbPath != null) return _resolvedDbPath;
        var repoRoot = Environment.GetEnvironmentVariable("REPO_ROOT")
                       ?? Directory.GetCurrentDirectory();
        var dir = Path.Combine(repoRoot, "Data");
        Directory.CreateDirectory(dir);
        _resolvedDbPath = Path.Combine(dir, "projection-cache.db");
        return _resolvedDbPath;
    }

    private static SqliteConnection Open()
    {
        var conn = new SqliteConnection($"Data Source={ResolveDbPath()};");
        conn.Open();
        using var pragma = conn.CreateCommand();
        pragma.CommandText = "PRAGMA journal_mode=WAL; PRAGMA busy_timeout=5000; PRAGMA synchronous=NORMAL;";
        pragma.ExecuteNonQuery();
        return conn;
    }

    private static void EnsureSchema(ILogger? logger)
    {
        if (_schemaReady) return;
        lock (_initLock)
        {
            if (_schemaReady) return;
            using var conn = Open();
            using var read = conn.CreateCommand();
            read.CommandText = "PRAGMA user_version;";
            var current = Convert.ToInt32(read.ExecuteScalar() ?? 0, CultureInfo.InvariantCulture);
            if (current != StorageSchemaVersion)
            {
                logger?.LogWarning(
                    "[{Event}] decision=storage-schema-recreate existingVersion={Existing} targetVersion={Target}",
                    LogEventName, current, StorageSchemaVersion);
                using var tx = conn.BeginTransaction();
                using var drop = conn.CreateCommand();
                drop.Transaction = tx;
                drop.CommandText = "DROP TABLE IF EXISTS projection_cache;";
                drop.ExecuteNonQuery();

                using var create = conn.CreateCommand();
                create.Transaction = tx;
                create.CommandText = @"
                    CREATE TABLE projection_cache (
                        input_hash              TEXT PRIMARY KEY,
                        target_language         TEXT NOT NULL,
                        schema_version          INTEGER NOT NULL,
                        basename                TEXT,
                        identity_scheme_version TEXT NOT NULL,
                        projection_block        TEXT NOT NULL,
                        output_hash             TEXT NOT NULL,
                        created_at_utc          TEXT NOT NULL,
                        last_hit_at_utc         TEXT NOT NULL,
                        hit_count               INTEGER NOT NULL DEFAULT 0,
                        byte_size               INTEGER NOT NULL DEFAULT 0
                    );
                    CREATE INDEX idx_projection_cache_lang ON projection_cache(target_language);
                    CREATE INDEX idx_projection_cache_bn ON projection_cache(basename);
                ";
                create.ExecuteNonQuery();

                using var setVer = conn.CreateCommand();
                setVer.Transaction = tx;
                setVer.CommandText = $"PRAGMA user_version={StorageSchemaVersion};";
                setVer.ExecuteNonQuery();

                tx.Commit();
            }
            _schemaReady = true;
        }
    }

    private static void TryUpdateHitCounters(string inputHash, ILogger? logger)
    {
        try
        {
            using var conn = Open();
            using var upd = conn.CreateCommand();
            upd.CommandText = @"
                UPDATE projection_cache
                SET hit_count = hit_count + 1, last_hit_at_utc = $ts
                WHERE input_hash = $h;";
            upd.Parameters.AddWithValue("$ts", DateTime.UtcNow.ToString("o", CultureInfo.InvariantCulture));
            upd.Parameters.AddWithValue("$h", inputHash);
            upd.ExecuteNonQuery();
        }
        catch (Exception ex)
        {
            logger?.LogDebug("[{Event}] Hit counter update failed (non-fatal): {Msg}", LogEventName, ex.Message);
        }
    }

    private static void EmitCacheEvent(int? runId, string targetLanguage, string inputHash,
        string projectionHash, string decision, string? basename)
    {
        MetricsSink.Emit(runId?.ToString(), new
        {
            Agent = "ProjectionCache",
            Event = "cache_event",
            CacheKind = "projection-block",
            Decision = decision,
            TargetLanguage = targetLanguage,
            InputHash = inputHash,
            ProjectionHash = projectionHash,
            Basename = basename
        });
    }
}
