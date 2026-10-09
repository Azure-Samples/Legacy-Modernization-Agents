using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Persistence;
using FluentAssertions;
using Microsoft.Data.Sqlite;
using Microsoft.Extensions.Logging.Abstractions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Persistence;

// Bank-of-Z keeps COMMAREA.cpy, INQACCSZ.cpy and generated request/response copybooks in several
// folders. UNIQUE(run_id, file_name) failed the whole reverse-engineering save after every
// file had already been analysed.
public class BusinessLogicKeyTests : IDisposable
{
    private readonly string _dir = Path.Combine(Path.GetTempPath(), "bl-key-" + Guid.NewGuid().ToString("N"));
    private string DbPath => Path.Combine(_dir, "migration.db");

    public BusinessLogicKeyTests() => Directory.CreateDirectory(_dir);

    public void Dispose()
    {
        SqliteConnection.ClearAllPools();
        try { Directory.Delete(_dir, recursive: true); } catch (IOException) { }
    }

    private static BusinessLogic Extract(string path) => new()
    {
        FileName = Path.GetFileName(path),
        FilePath = path,
        IsCopybook = true,
        BusinessPurpose = "purpose of " + path,
    };

    private async Task<SqliteMigrationRepository> OpenAsync()
    {
        var repo = new SqliteMigrationRepository(DbPath, NullLogger<SqliteMigrationRepository>.Instance);
        await repo.InitializeAsync();
        return repo;
    }

    [Fact]
    public async Task SameNameInTwoFolders_IsSavedAndReadBack()
    {
        var repo = await OpenAsync();
        var runId = await repo.StartRunAsync("/src", "/out");

        await repo.SaveBusinessLogicAsync(runId, new[]
        {
            Extract("/src/api/CRECUST/COMMAREA.cpy"),
            Extract("/src/api/DBCRFUN/COMMAREA.cpy"),
        });

        var saved = await repo.GetBusinessLogicAsync(runId);
        saved.Select(b => b.FilePath).Should().BeEquivalentTo(
            "/src/api/CRECUST/COMMAREA.cpy", "/src/api/DBCRFUN/COMMAREA.cpy");
    }

    [Fact]
    public async Task OldDatabase_IsUpgradedAndKeepsItsRows()
    {
        await using (var connection = new SqliteConnection($"Data Source={DbPath}"))
        {
            await connection.OpenAsync();
            await using var cmd = connection.CreateCommand();
            cmd.CommandText = @"
CREATE TABLE business_logic (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    run_id INTEGER NOT NULL,
    file_name TEXT NOT NULL,
    file_path TEXT NOT NULL,
    is_copybook INTEGER NOT NULL DEFAULT 0,
    business_purpose TEXT,
    user_stories_json TEXT,
    features_json TEXT,
    business_rules_json TEXT,
    created_at TEXT DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(run_id, file_name)
);
INSERT INTO business_logic (run_id, file_name, file_path, business_purpose)
VALUES (7, 'OLD.cbl', '/src/OLD.cbl', 'kept');";
            await cmd.ExecuteNonQueryAsync();
        }

        var repo = await OpenAsync();
        (await repo.GetBusinessLogicAsync(7)).Should().ContainSingle(b => b.BusinessPurpose == "kept");

        var runId = await repo.StartRunAsync("/src", "/out");
        await repo.SaveBusinessLogicAsync(runId, new[] { Extract("/a/X.cpy"), Extract("/b/X.cpy") });
        (await repo.GetBusinessLogicAsync(runId)).Should().HaveCount(2);

        // A second start must not rebuild again.
        await OpenAsync();
        (await repo.GetBusinessLogicAsync(7)).Should().ContainSingle();
    }
}
