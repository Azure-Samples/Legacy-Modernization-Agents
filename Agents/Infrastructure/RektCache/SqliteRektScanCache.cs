using System.Globalization;
using System.Text.Json;
using Microsoft.Data.Sqlite;
using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Agents.Infrastructure;

namespace CobolToQuarkusMigration.Agents.Infrastructure.RektCache;

/// <summary>
/// SQLite-backed <see cref="IRektScanCache"/>. Same discipline as
/// <see cref="Caching.SqliteResponseCache"/>: WAL mode, per-operation connections,
/// busy_timeout, two independent schema versions.
/// </summary>
/// <remarks>
/// <para>Schema versions:</para>
/// <list type="bullet">
///   <item><see cref="StorageSchemaVersion"/> — table layout. Mismatch ⇒ DROP and recreate
///         (one-time data loss; this is derived data).</item>
///   <item><see cref="SemanticInvalidationVersion"/> — bumped when the planner's
///         decision logic changes in a way that should treat all existing entries
///         as stale (without dropping rows). Stored on each entry; mismatched
///         rows are treated as cache misses.</item>
/// </list>
/// <para>
/// All read/write paths catch IO/SQLite exceptions and return defaults (null /
/// empty map / no-op) so a corrupt or missing cache file always falls back to
/// a full parse rather than crashing the pipeline.
/// </para>
/// </remarks>
public sealed class SqliteRektScanCache : IRektScanCache
{
    public const int StorageSchemaVersion = 1;

    /// <summary>
    /// Bump when the planner's invalidation logic changes (e.g. starts considering
    /// new fields, changes hash construction). Old rows with a different value
    /// become cache misses; the DB is otherwise untouched.
    /// </summary>
    public const string SemanticInvalidationVersion = "1";

    public const string LogEventName = "RektScanCache";

    private readonly string _connectionString;
    private readonly ILogger? _logger;

    public SqliteRektScanCache(string dbPath, ILogger? logger = null)
    {
        if (string.IsNullOrWhiteSpace(dbPath))
            throw new ArgumentException("dbPath required", nameof(dbPath));

        var dir = Path.GetDirectoryName(Path.GetFullPath(dbPath));
        if (!string.IsNullOrEmpty(dir) && !Directory.Exists(dir))
            Directory.CreateDirectory(dir);

        _connectionString = new SqliteConnectionStringBuilder
        {
            DataSource = dbPath,
            Mode = SqliteOpenMode.ReadWriteCreate,
            Cache = SqliteCacheMode.Shared,
            Pooling = true,
        }.ToString();

        _logger = logger;
        TryEnsureSchema();
    }

    private SqliteConnection Open()
    {
        var conn = new SqliteConnection(_connectionString);
        conn.Open();
        using var pragma = conn.CreateCommand();
        pragma.CommandText = "PRAGMA journal_mode=WAL; PRAGMA busy_timeout=5000; PRAGMA synchronous=NORMAL;";
        pragma.ExecuteNonQuery();
        return conn;
    }

    /// <summary>
    /// Wraps schema setup with a try/catch so a corrupt DB does not throw out of
    /// the constructor. The cache will simply behave as empty until the next clean
    /// upsert (which will recreate the table).
    /// </summary>
    private void TryEnsureSchema()
    {
        try
        {
            EnsureSchema();
        }
        catch (Exception ex)
        {
            _logger?.LogWarning(ex,
                "[{Event}] runId={RunId} correlationId={CorrelationId} " +
                "decision=storage-init-failed reason=fail-open",
                LogEventName, LlmCorrelationContext.RunId, LlmCorrelationContext.CorrelationId);
        }
    }

    private void EnsureSchema()
    {
        using var conn = Open();
        using var read = conn.CreateCommand();
        read.CommandText = "PRAGMA user_version;";
        var current = Convert.ToInt32(read.ExecuteScalar() ?? 0, CultureInfo.InvariantCulture);

        if (current == StorageSchemaVersion) return;

        _logger?.LogWarning(
            "[{Event}] runId={RunId} correlationId={CorrelationId} " +
            "decision=storage-schema-recreate existingVersion={Existing} targetVersion={Target}",
            LogEventName, LlmCorrelationContext.RunId, LlmCorrelationContext.CorrelationId,
            current, StorageSchemaVersion);

        using var tx = conn.BeginTransaction();
        using var drop = conn.CreateCommand();
        drop.Transaction = tx;
        drop.CommandText = "DROP TABLE IF EXISTS scan_entry;";
        drop.ExecuteNonQuery();

        using var create = conn.CreateCommand();
        create.Transaction = tx;
        create.CommandText = @"
            CREATE TABLE scan_entry (
                basename                    TEXT NOT NULL,
                identity_scheme             TEXT NOT NULL,
                relative_path               TEXT,
                preprocessed_hash           TEXT NOT NULL,
                source_hash                 TEXT,
                parse_outcome               TEXT NOT NULL,
                confidence                  TEXT NOT NULL,
                parsed_at_utc               TEXT NOT NULL,
                warnings_json               TEXT NOT NULL DEFAULT '[]',
                dependency_snapshot_json    TEXT NOT NULL DEFAULT '{}',
                semantic_invalidation_ver   TEXT NOT NULL,
                PRIMARY KEY (basename, identity_scheme)
            );
            CREATE INDEX idx_scan_entry_scheme ON scan_entry(identity_scheme);";
        create.ExecuteNonQuery();

        using var ver = conn.CreateCommand();
        ver.Transaction = tx;
        ver.CommandText = $"PRAGMA user_version = {StorageSchemaVersion};";
        ver.ExecuteNonQuery();

        tx.Commit();
    }

    /// <inheritdoc />
    public async Task<RektScanEntry?> TryGetAsync(
        string basename, string identityScheme, CancellationToken cancellationToken = default)
    {
        try
        {
            using var conn = Open();
            using var cmd = conn.CreateCommand();
            cmd.CommandText = @"
                SELECT relative_path, preprocessed_hash, source_hash, parse_outcome, confidence,
                       parsed_at_utc, warnings_json, dependency_snapshot_json, semantic_invalidation_ver
                FROM scan_entry
                WHERE basename = $b AND identity_scheme = $is;";
            cmd.Parameters.AddWithValue("$b", basename);
            cmd.Parameters.AddWithValue("$is", identityScheme);
            using var reader = await cmd.ExecuteReaderAsync(cancellationToken);
            if (!await reader.ReadAsync(cancellationToken)) return null;

            var semanticVer = reader.GetString(8);
            if (semanticVer != SemanticInvalidationVersion)
            {
                // Stale by semantic version — treat as a miss without deleting.
                _logger?.LogInformation(
                    "[{Event}] runId={RunId} correlationId={CorrelationId} basename={Basename} " +
                    "decision=stale-entry reason=semantic-version-mismatch storedVersion={Stored} currentVersion={Current}",
                    LogEventName, LlmCorrelationContext.RunId, LlmCorrelationContext.CorrelationId,
                    basename, semanticVer, SemanticInvalidationVersion);
                return null;
            }

            return BuildEntry(basename, identityScheme, reader);
        }
        catch (Exception ex)
        {
            _logger?.LogWarning(ex,
                "[{Event}] runId={RunId} correlationId={CorrelationId} basename={Basename} " +
                "decision=lookup-failed reason=fail-open",
                LogEventName, LlmCorrelationContext.RunId, LlmCorrelationContext.CorrelationId, basename);
            return null;
        }
    }

    /// <inheritdoc />
    public async Task<IReadOnlyDictionary<string, RektScanEntry>> GetManyAsync(
        IReadOnlyCollection<string> basenames, string identityScheme,
        CancellationToken cancellationToken = default)
    {
        var result = new Dictionary<string, RektScanEntry>(StringComparer.OrdinalIgnoreCase);
        if (basenames.Count == 0) return result;

        try
        {
            using var conn = Open();
            // Use IN with a temp table for arbitrary-size inputs. Simpler: do one
            // query per basename — at our scale (≤1k) the per-query overhead is
            // dominated by the WAL setup, and the connection is reused via pooling.
            foreach (var b in basenames)
            {
                using var cmd = conn.CreateCommand();
                cmd.CommandText = @"
                    SELECT relative_path, preprocessed_hash, source_hash, parse_outcome, confidence,
                           parsed_at_utc, warnings_json, dependency_snapshot_json, semantic_invalidation_ver
                    FROM scan_entry
                    WHERE basename = $b AND identity_scheme = $is;";
                cmd.Parameters.AddWithValue("$b", b);
                cmd.Parameters.AddWithValue("$is", identityScheme);
                using var reader = await cmd.ExecuteReaderAsync(cancellationToken);
                if (!await reader.ReadAsync(cancellationToken)) continue;
                if (reader.GetString(8) != SemanticInvalidationVersion) continue;
                result[b] = BuildEntry(b, identityScheme, reader);
            }
        }
        catch (Exception ex)
        {
            _logger?.LogWarning(ex,
                "[{Event}] runId={RunId} correlationId={CorrelationId} " +
                "decision=bulk-lookup-failed reason=fail-open",
                LogEventName, LlmCorrelationContext.RunId, LlmCorrelationContext.CorrelationId);
        }
        return result;
    }

    /// <inheritdoc />
    public async Task UpsertAsync(RektScanEntry entry, CancellationToken cancellationToken = default)
    {
        try
        {
            using var conn = Open();
            using var cmd = conn.CreateCommand();
            cmd.CommandText = @"
                INSERT OR REPLACE INTO scan_entry (
                    basename, identity_scheme, relative_path, preprocessed_hash, source_hash,
                    parse_outcome, confidence, parsed_at_utc, warnings_json,
                    dependency_snapshot_json, semantic_invalidation_ver
                ) VALUES (
                    $b, $is, $rp, $ph, $sh,
                    $po, $cf, $at, $w,
                    $ds, $sv
                );";
            cmd.Parameters.AddWithValue("$b", entry.Basename);
            cmd.Parameters.AddWithValue("$is", entry.IdentitySchemeVersion);
            cmd.Parameters.AddWithValue("$rp", (object?)entry.RelativePath ?? DBNull.Value);
            cmd.Parameters.AddWithValue("$ph", entry.PreprocessedHash);
            cmd.Parameters.AddWithValue("$sh", (object?)entry.SourceHash ?? DBNull.Value);
            cmd.Parameters.AddWithValue("$po", entry.ParseOutcome.ToString());
            cmd.Parameters.AddWithValue("$cf", entry.Confidence.ToString());
            cmd.Parameters.AddWithValue("$at", entry.ParsedAtUtc.ToString("O", CultureInfo.InvariantCulture));
            cmd.Parameters.AddWithValue("$w", JsonSerializer.Serialize(entry.Warnings));
            cmd.Parameters.AddWithValue("$ds", JsonSerializer.Serialize(entry.DependencySnapshot));
            cmd.Parameters.AddWithValue("$sv", SemanticInvalidationVersion);
            await cmd.ExecuteNonQueryAsync(cancellationToken);
        }
        catch (Exception ex)
        {
            _logger?.LogWarning(ex,
                "[{Event}] runId={RunId} correlationId={CorrelationId} basename={Basename} " +
                "decision=upsert-failed reason=fail-open",
                LogEventName, LlmCorrelationContext.RunId, LlmCorrelationContext.CorrelationId, entry.Basename);
        }
    }

    /// <inheritdoc />
    public async Task<int> PruneOtherIdentitySchemesAsync(
        string currentIdentityScheme, CancellationToken cancellationToken = default)
    {
        try
        {
            using var conn = Open();
            using var cmd = conn.CreateCommand();
            cmd.CommandText = "DELETE FROM scan_entry WHERE identity_scheme != $is;";
            cmd.Parameters.AddWithValue("$is", currentIdentityScheme);
            var n = await cmd.ExecuteNonQueryAsync(cancellationToken);
            _logger?.LogInformation(
                "[{Event}] runId={RunId} correlationId={CorrelationId} " +
                "decision=prune-other-identity-schemes deletedEntries={N} currentScheme={Scheme}",
                LogEventName, LlmCorrelationContext.RunId, LlmCorrelationContext.CorrelationId,
                n, currentIdentityScheme);
            return n;
        }
        catch (Exception ex)
        {
            _logger?.LogWarning(ex,
                "[{Event}] runId={RunId} correlationId={CorrelationId} " +
                "decision=prune-failed reason=fail-open",
                LogEventName, LlmCorrelationContext.RunId, LlmCorrelationContext.CorrelationId);
            return 0;
        }
    }

    private static RektScanEntry BuildEntry(string basename, string identityScheme, SqliteDataReader reader)
    {
        var warnings = JsonSerializer.Deserialize<List<string>>(reader.GetString(6)) ?? new();
        var deps = JsonSerializer.Deserialize<Dictionary<string, string>>(reader.GetString(7))
                   ?? new(StringComparer.OrdinalIgnoreCase);

        // Re-key into a case-insensitive dictionary in case the serialized JSON used default comparer.
        var depsCi = new Dictionary<string, string>(deps, StringComparer.OrdinalIgnoreCase);

        return new RektScanEntry
        {
            Basename = basename,
            IdentitySchemeVersion = identityScheme,
            RelativePath = reader.IsDBNull(0) ? null : reader.GetString(0),
            PreprocessedHash = reader.GetString(1),
            SourceHash = reader.IsDBNull(2) ? null : reader.GetString(2),
            ParseOutcome = Enum.Parse<RektParseOutcome>(reader.GetString(3)),
            Confidence = Enum.Parse<RektScanConfidence>(reader.GetString(4)),
            ParsedAtUtc = DateTime.Parse(reader.GetString(5), CultureInfo.InvariantCulture,
                DateTimeStyles.AssumeUniversal | DateTimeStyles.AdjustToUniversal),
            Warnings = warnings,
            DependencySnapshot = depsCi,
        };
    }
}
