using System.Globalization;
using Microsoft.Data.Sqlite;

namespace McpChatWeb.Services;

/// <summary>
/// PR-Portal-P2: Migration Wave Planner — first WRITE capability in the portal.
///
/// <para>
/// Persists user wave assignments in <c>Data/migration-waves.db</c> (SQLite WAL).
/// The auto-suggested waves shown in the Insights Hub Lead view are computed
/// live from topology; this service persists USER overrides + notes per program.
/// </para>
///
/// <para>
/// Schema: one row per (program_basename, wave_number) assignment. Latest
/// row per basename is the active assignment. Unassigned programs simply
/// have no row.
/// </para>
///
/// <para>
/// Fail-soft: missing DB file is auto-created on first write. Read with
/// missing DB returns empty list.
/// </para>
/// </summary>
public sealed class MigrationWaveService
{
    private const int StorageSchemaVersion = 1;
    private readonly string _dbPath;
    private readonly ILogger<MigrationWaveService> _logger;
    private readonly object _initLock = new();
    private bool _schemaReady;

    public MigrationWaveService(IConfiguration config, ILogger<MigrationWaveService> logger)
    {
        var repoRoot = Environment.GetEnvironmentVariable("REPO_ROOT");
        if (string.IsNullOrEmpty(repoRoot) || !Directory.Exists(repoRoot))
        {
            var dir = new DirectoryInfo(Directory.GetCurrentDirectory());
            while (dir != null && !File.Exists(Path.Combine(dir.FullName, "doctor.sh"))) dir = dir.Parent;
            repoRoot = dir?.FullName ?? Directory.GetCurrentDirectory();
        }
        var dataDir = Path.Combine(repoRoot, "Data");
        Directory.CreateDirectory(dataDir);
        _dbPath = Path.Combine(dataDir, "migration-waves.db");
        _logger = logger;
    }

    public IEnumerable<WaveAssignment> GetAll()
    {
        EnsureSchema();
        SqliteConnection conn;
        try
        {
            conn = Open();
        }
        catch (Exception ex)
        {
            _logger.LogWarning("[MigrationWaves] Open failed: {Msg}", ex.Message);
            yield break;
        }

        using (conn)
        {
            // Pick the LATEST assignment per program (one program may have history)
            using var c = conn.CreateCommand();
            c.CommandText = @"
                SELECT program_basename, wave_number, notes, assigned_at_utc, source
                  FROM wave_assignment
                 WHERE id IN (
                     SELECT MAX(id) FROM wave_assignment GROUP BY program_basename
                 )
                 ORDER BY wave_number, program_basename";
            using var r = c.ExecuteReader();
            while (r.Read())
            {
                yield return new WaveAssignment(
                    Basename: r.GetString(0),
                    WaveNumber: r.GetInt32(1),
                    Notes: r.IsDBNull(2) ? null : r.GetString(2),
                    AssignedAt: r.IsDBNull(3) ? "" : r.GetString(3),
                    Source: r.IsDBNull(4) ? "user" : r.GetString(4)
                );
            }
        }
    }

    public WaveAssignment Upsert(string basename, int waveNumber, string? notes, string source = "user")
    {
        EnsureSchema();
        var now = DateTime.UtcNow.ToString("o", CultureInfo.InvariantCulture);
        using var conn = Open();
        using var c = conn.CreateCommand();
        c.CommandText = @"
            INSERT INTO wave_assignment (program_basename, wave_number, notes, assigned_at_utc, source)
            VALUES ($bn, $wave, $notes, $ts, $src)";
        c.Parameters.AddWithValue("$bn", basename);
        c.Parameters.AddWithValue("$wave", waveNumber);
        c.Parameters.AddWithValue("$notes", (object?)notes ?? DBNull.Value);
        c.Parameters.AddWithValue("$ts", now);
        c.Parameters.AddWithValue("$src", source);
        c.ExecuteNonQuery();
        _logger.LogInformation("[MigrationWaves] Assigned {Basename} → wave {Wave} ({Src})", basename, waveNumber, source);
        return new WaveAssignment(basename, waveNumber, notes, now, source);
    }

    public int ClearAll()
    {
        EnsureSchema();
        using var conn = Open();
        using var c = conn.CreateCommand();
        c.CommandText = "DELETE FROM wave_assignment";
        var n = c.ExecuteNonQuery();
        _logger.LogInformation("[MigrationWaves] Cleared {N} assignments", n);
        return n;
    }

    public int RemoveProgram(string basename)
    {
        EnsureSchema();
        using var conn = Open();
        using var c = conn.CreateCommand();
        c.CommandText = "DELETE FROM wave_assignment WHERE program_basename = $bn";
        c.Parameters.AddWithValue("$bn", basename);
        var n = c.ExecuteNonQuery();
        return n;
    }

    private SqliteConnection Open()
    {
        var conn = new SqliteConnection($"Data Source={_dbPath};");
        conn.Open();
        using var pragma = conn.CreateCommand();
        pragma.CommandText = "PRAGMA journal_mode=WAL; PRAGMA busy_timeout=5000; PRAGMA synchronous=NORMAL;";
        pragma.ExecuteNonQuery();
        return conn;
    }

    private void EnsureSchema()
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
                _logger.LogWarning(
                    "[MigrationWaves] schema recreate: existingVersion={Existing} targetVersion={Target}",
                    current, StorageSchemaVersion);
                using var tx = conn.BeginTransaction();
                using var drop = conn.CreateCommand();
                drop.Transaction = tx;
                drop.CommandText = "DROP TABLE IF EXISTS wave_assignment;";
                drop.ExecuteNonQuery();

                using var create = conn.CreateCommand();
                create.Transaction = tx;
                create.CommandText = @"
                    CREATE TABLE wave_assignment (
                        id                  INTEGER PRIMARY KEY AUTOINCREMENT,
                        program_basename    TEXT NOT NULL,
                        wave_number         INTEGER NOT NULL,
                        notes               TEXT,
                        assigned_at_utc     TEXT NOT NULL,
                        source              TEXT NOT NULL DEFAULT 'user'
                    );
                    CREATE INDEX idx_wave_basename ON wave_assignment(program_basename);
                    CREATE INDEX idx_wave_number   ON wave_assignment(wave_number);";
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
}
