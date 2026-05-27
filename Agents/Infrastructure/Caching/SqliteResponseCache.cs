using System.Globalization;
using Microsoft.Data.Sqlite;
using Microsoft.Extensions.Logging;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Caching;

/// <summary>
/// SQLite-backed <see cref="IResponseCache"/>. Safe for in-process concurrency:
/// each operation opens its own connection from the Microsoft.Data.Sqlite pool,
/// uses WAL journaling, and applies a busy-timeout so concurrent writers serialize
/// cleanly rather than failing.
/// </summary>
/// <remarks>
/// Two independent schema versions:
/// <list type="bullet">
///   <item><see cref="StorageSchemaVersion"/> — table layout. Mismatch ⇒ DROP and recreate
///         (one-time data loss; this is derived data).</item>
///   <item><see cref="CacheKey.KeySchemaVersion"/> — key construction. Mismatch ⇒ entries
///         simply become unreachable (no DB action needed).</item>
/// </list>
/// </remarks>
public sealed class SqliteResponseCache : IResponseCache
{
    public const int StorageSchemaVersion = 1;
    public const string LogEventName = "LlmResponseCache";

    private readonly string _connectionString;
    private readonly ILogger? _logger;

    /// <summary>
    /// Creates a new cache at <paramref name="dbPath"/>. The file is created on
    /// first use; the schema is verified and (if version mismatched) recreated.
    /// </summary>
    public SqliteResponseCache(string dbPath, ILogger? logger = null)
    {
        if (string.IsNullOrWhiteSpace(dbPath))
            throw new ArgumentException("dbPath required", nameof(dbPath));

        var dir = Path.GetDirectoryName(Path.GetFullPath(dbPath));
        if (!string.IsNullOrEmpty(dir) && !Directory.Exists(dir))
            Directory.CreateDirectory(dir);

        // Pooling=True (default) means the underlying SqliteConnection objects are
        // reused — fast for our open-per-op pattern.
        _connectionString = new SqliteConnectionStringBuilder
        {
            DataSource = dbPath,
            Mode = SqliteOpenMode.ReadWriteCreate,
            Cache = SqliteCacheMode.Shared,
            Pooling = true,
        }.ToString();

        _logger = logger;
        EnsureSchema();
    }

    private SqliteConnection Open()
    {
        var conn = new SqliteConnection(_connectionString);
        conn.Open();
        // Per-connection pragmas. WAL + busy_timeout = clean concurrent behaviour.
        using var pragma = conn.CreateCommand();
        pragma.CommandText = "PRAGMA journal_mode=WAL; PRAGMA busy_timeout=5000; PRAGMA synchronous=NORMAL;";
        pragma.ExecuteNonQuery();
        return conn;
    }

    private void EnsureSchema()
    {
        using var conn = Open();
        using var read = conn.CreateCommand();
        read.CommandText = "PRAGMA user_version;";
        var current = Convert.ToInt32(read.ExecuteScalar() ?? 0, CultureInfo.InvariantCulture);

        if (current == StorageSchemaVersion)
            return;

        _logger?.LogWarning(
            "[{Event}] runId={RunId} correlationId={CorrelationId} " +
            "decision=storage-schema-recreate reason=version-mismatch existingVersion={Existing} targetVersion={Target}",
            LogEventName, LlmCorrelationContext.RunId, LlmCorrelationContext.CorrelationId,
            current, StorageSchemaVersion);

        using var tx = conn.BeginTransaction();
        using var drop = conn.CreateCommand();
        drop.Transaction = tx;
        drop.CommandText = "DROP TABLE IF EXISTS response_cache;";
        drop.ExecuteNonQuery();

        using var create = conn.CreateCommand();
        create.Transaction = tx;
        create.CommandText = @"
            CREATE TABLE response_cache (
                key_hash               TEXT PRIMARY KEY,
                provider_key           TEXT NOT NULL,
                model                  TEXT NOT NULL,
                identity_scheme        TEXT NOT NULL,
                key_schema_version     TEXT NOT NULL,
                basename               TEXT,
                relative_path          TEXT,
                target_language        TEXT NOT NULL,
                prompt_template_id     TEXT NOT NULL,
                prompt_template_ver    TEXT NOT NULL,
                source_hash            TEXT NOT NULL,
                rekt_hash              TEXT NOT NULL,
                response_text          TEXT NOT NULL,
                created_at_utc         TEXT NOT NULL,
                last_hit_at_utc        TEXT NOT NULL,
                hit_count              INTEGER NOT NULL DEFAULT 0,
                byte_size              INTEGER NOT NULL
            );
            CREATE INDEX idx_response_cache_lru ON response_cache(last_hit_at_utc);
            CREATE INDEX idx_response_cache_provider_model ON response_cache(provider_key, model);";
        create.ExecuteNonQuery();

        using var ver = conn.CreateCommand();
        ver.Transaction = tx;
        ver.CommandText = $"PRAGMA user_version = {StorageSchemaVersion};";
        ver.ExecuteNonQuery();

        tx.Commit();
    }

    /// <inheritdoc />
    public async Task<CacheLookupResult> TryGetAsync(CacheKey key, CancellationToken cancellationToken = default)
    {
        CacheKey.Build(key);
        var keyHash = key.Compute();

        using var conn = Open();
        using var read = conn.CreateCommand();
        read.CommandText = @"
            SELECT response_text, created_at_utc, hit_count
            FROM response_cache
            WHERE key_hash = $kh;";
        read.Parameters.AddWithValue("$kh", keyHash);

        using var reader = await read.ExecuteReaderAsync(cancellationToken);
        if (!await reader.ReadAsync(cancellationToken))
        {
            LogLookup(decision: "miss", missReason: CacheMissReason.KeyNotFound, keyHash, key, age: null, hitCount: 0);
            return new CacheLookupResult(null, CacheMissReason.KeyNotFound);
        }

        var text = reader.GetString(0);
        var createdAt = DateTime.Parse(reader.GetString(1), CultureInfo.InvariantCulture,
            DateTimeStyles.AssumeUniversal | DateTimeStyles.AdjustToUniversal);
        var hitCount = reader.GetInt32(2) + 1;
        reader.Close();

        // Bump LRU timestamp and hit count.
        var nowIso = DateTime.UtcNow.ToString("O", CultureInfo.InvariantCulture);
        using var touch = conn.CreateCommand();
        touch.CommandText = @"
            UPDATE response_cache
            SET last_hit_at_utc = $now, hit_count = hit_count + 1
            WHERE key_hash = $kh;";
        touch.Parameters.AddWithValue("$now", nowIso);
        touch.Parameters.AddWithValue("$kh", keyHash);
        await touch.ExecuteNonQueryAsync(cancellationToken);

        var age = DateTime.UtcNow - createdAt;
        var entry = new CacheEntry(text, createdAt, keyHash, hitCount, age);
        LogLookup(decision: "hit", missReason: null, keyHash, key, age: age, hitCount: hitCount);
        return new CacheLookupResult(entry, null);
    }

    /// <inheritdoc />
    public async Task PutAsync(CacheKey key, string responseText, CancellationToken cancellationToken = default)
    {
        CacheKey.Build(key);
        var keyHash = key.Compute();
        var nowIso = DateTime.UtcNow.ToString("O", CultureInfo.InvariantCulture);
        var byteSize = System.Text.Encoding.UTF8.GetByteCount(responseText);

        using var conn = Open();
        using var cmd = conn.CreateCommand();
        cmd.CommandText = @"
            INSERT OR REPLACE INTO response_cache (
                key_hash, provider_key, model, identity_scheme, key_schema_version,
                basename, relative_path, target_language, prompt_template_id, prompt_template_ver,
                source_hash, rekt_hash, response_text, created_at_utc, last_hit_at_utc,
                hit_count, byte_size
            ) VALUES (
                $kh, $pk, $m, $ids, $ksv,
                $bn, $rp, $tl, $pti, $ptv,
                $sh, $rh, $rt, $now, $now,
                0, $sz
            );";
        cmd.Parameters.AddWithValue("$kh", keyHash);
        cmd.Parameters.AddWithValue("$pk", key.ProviderKey);
        cmd.Parameters.AddWithValue("$m", key.Model);
        cmd.Parameters.AddWithValue("$ids", key.IdentitySchemeVersion);
        cmd.Parameters.AddWithValue("$ksv", CacheKey.KeySchemaVersion);
        cmd.Parameters.AddWithValue("$bn", (object?)key.Basename ?? DBNull.Value);
        cmd.Parameters.AddWithValue("$rp", (object?)key.RelativePath ?? DBNull.Value);
        cmd.Parameters.AddWithValue("$tl", key.TargetLanguage);
        cmd.Parameters.AddWithValue("$pti", key.PromptTemplateId);
        cmd.Parameters.AddWithValue("$ptv", key.PromptTemplateVersion);
        cmd.Parameters.AddWithValue("$sh", key.SourceHash);
        cmd.Parameters.AddWithValue("$rh", key.RektFactsHash);
        cmd.Parameters.AddWithValue("$rt", responseText);
        cmd.Parameters.AddWithValue("$now", nowIso);
        cmd.Parameters.AddWithValue("$sz", byteSize);
        await cmd.ExecuteNonQueryAsync(cancellationToken);

        LogStore(keyHash, key, byteSize);
    }

    /// <inheritdoc />
    public async Task<int> PruneAsync(TimeSpan ttl, long? maxBytes = null, CancellationToken cancellationToken = default)
    {
        using var conn = Open();
        var cutoff = DateTime.UtcNow.Subtract(ttl).ToString("O", CultureInfo.InvariantCulture);
        var deleted = 0;

        using (var ttlCmd = conn.CreateCommand())
        {
            ttlCmd.CommandText = "DELETE FROM response_cache WHERE created_at_utc < $cutoff;";
            ttlCmd.Parameters.AddWithValue("$cutoff", cutoff);
            deleted += await ttlCmd.ExecuteNonQueryAsync(cancellationToken);
        }

        if (maxBytes is { } cap)
        {
            using var sizeCmd = conn.CreateCommand();
            sizeCmd.CommandText = "SELECT COALESCE(SUM(byte_size), 0) FROM response_cache;";
            var total = Convert.ToInt64(await sizeCmd.ExecuteScalarAsync(cancellationToken) ?? 0L,
                CultureInfo.InvariantCulture);
            if (total > cap)
            {
                // LRU: delete oldest by last_hit_at_utc until we're under cap.
                var toFree = total - cap;
                using var lruCmd = conn.CreateCommand();
                lruCmd.CommandText = @"
                    DELETE FROM response_cache
                    WHERE key_hash IN (
                        SELECT key_hash FROM response_cache
                        ORDER BY last_hit_at_utc ASC
                        LIMIT (
                            SELECT COUNT(*) FROM (
                                SELECT byte_size, SUM(byte_size) OVER (ORDER BY last_hit_at_utc ASC) AS running
                                FROM response_cache
                            ) WHERE running <= $toFree
                        ) + 1
                    );";
                lruCmd.Parameters.AddWithValue("$toFree", toFree);
                deleted += await lruCmd.ExecuteNonQueryAsync(cancellationToken);
            }
        }

        _logger?.LogInformation(
            "[{Event}] runId={RunId} correlationId={CorrelationId} " +
            "decision=prune deletedEntries={Deleted} ttlSeconds={Ttl} maxBytes={Cap}",
            LogEventName, LlmCorrelationContext.RunId, LlmCorrelationContext.CorrelationId,
            deleted, ttl.TotalSeconds, maxBytes ?? -1);
        return deleted;
    }

    private void LogLookup(string decision, CacheMissReason? missReason, string keyHash,
        CacheKey key, TimeSpan? age, int hitCount)
    {
        _logger?.LogInformation(
            "[{Event}] runId={RunId} correlationId={CorrelationId} provider={Provider} model={Model} " +
            "decision={Decision} missReason={MissReason} keyHash={KeyHashShort} " +
            "ageSeconds={Age:F0} hitCount={HitCount} identityScheme={IdScheme} basename={Basename} " +
            "template={Tpl}/{TplVer} sourceHash={Sh} rektHash={Rh}",
            LogEventName, LlmCorrelationContext.RunId, LlmCorrelationContext.CorrelationId,
            key.ProviderKey, key.Model, decision, missReason?.ToString() ?? "-",
            keyHash[..Math.Min(12, keyHash.Length)],
            age?.TotalSeconds ?? 0, hitCount, key.IdentitySchemeVersion, key.Basename ?? "-",
            key.PromptTemplateId, key.PromptTemplateVersion,
            Short(key.SourceHash), Short(key.RektFactsHash));
    }

    private void LogStore(string keyHash, CacheKey key, int byteSize)
    {
        _logger?.LogInformation(
            "[{Event}] runId={RunId} correlationId={CorrelationId} provider={Provider} model={Model} " +
            "decision=stored keyHash={KeyHashShort} byteSize={Size} " +
            "identityScheme={IdScheme} basename={Basename} template={Tpl}/{TplVer}",
            LogEventName, LlmCorrelationContext.RunId, LlmCorrelationContext.CorrelationId,
            key.ProviderKey, key.Model, keyHash[..Math.Min(12, keyHash.Length)], byteSize,
            key.IdentitySchemeVersion, key.Basename ?? "-",
            key.PromptTemplateId, key.PromptTemplateVersion);
    }

    private static string Short(string h) =>
        string.IsNullOrEmpty(h) ? "-" : h[..Math.Min(8, h.Length)];
}
