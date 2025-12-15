using System.IO;
using System.Text.Json;
using Microsoft.Data.Sqlite;
using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Persistence;

/// <summary>
/// SQLite-backed implementation of <see cref="IMigrationRepository"/>.
/// </summary>
public class SqliteMigrationRepository : IMigrationRepository
{
    private static readonly JsonSerializerOptions JsonOptions = new(JsonSerializerDefaults.Web)
    {
        WriteIndented = false
    };

    private readonly string _databasePath;
    private readonly string _connectionString;
    private readonly ILogger<SqliteMigrationRepository> _logger;

    public SqliteMigrationRepository(string databasePath, ILogger<SqliteMigrationRepository> logger)
    {
        _databasePath = databasePath;
        _connectionString = $"Data Source={_databasePath};Cache=Shared";
        _logger = logger;
    }

    public async Task InitializeAsync(CancellationToken cancellationToken = default)
    {
        var directory = Path.GetDirectoryName(_databasePath);
        if (!string.IsNullOrEmpty(directory) && !Directory.Exists(directory))
        {
            Directory.CreateDirectory(directory);
        }

        await using var connection = CreateConnection();
        await connection.OpenAsync(cancellationToken);
        await using var command = connection.CreateCommand();
        command.CommandText = @"
CREATE TABLE IF NOT EXISTS runs (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    started_at TEXT NOT NULL,
    completed_at TEXT,
    status TEXT NOT NULL,
    cobol_source TEXT,
    java_output TEXT,
    notes TEXT
);
CREATE TABLE IF NOT EXISTS cobol_files (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    run_id INTEGER NOT NULL,
    file_name TEXT NOT NULL,
    file_path TEXT NOT NULL,
    is_copybook INTEGER NOT NULL,
    content TEXT,
    FOREIGN KEY(run_id) REFERENCES runs(id)
);
CREATE TABLE IF NOT EXISTS analyses (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    cobol_file_id INTEGER NOT NULL,
    program_description TEXT,
    raw_analysis TEXT,
    data_divisions_json TEXT,
    procedure_divisions_json TEXT,
    variables_json TEXT,
    paragraphs_json TEXT,
    copybooks_json TEXT,
    FOREIGN KEY(cobol_file_id) REFERENCES cobol_files(id)
);
CREATE TABLE IF NOT EXISTS dependencies (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    run_id INTEGER NOT NULL,
    source_file TEXT NOT NULL,
    target_file TEXT NOT NULL,
    dependency_type TEXT,
    line_number INTEGER,
    context TEXT,
    FOREIGN KEY(run_id) REFERENCES runs(id)
);
CREATE TABLE IF NOT EXISTS copybook_usage (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    run_id INTEGER NOT NULL,
    program TEXT NOT NULL,
    copybook TEXT NOT NULL,
    FOREIGN KEY(run_id) REFERENCES runs(id)
);
CREATE TABLE IF NOT EXISTS metrics (
    run_id INTEGER PRIMARY KEY,
    total_programs INTEGER,
    total_copybooks INTEGER,
    total_dependencies INTEGER,
    avg_dependencies_per_program REAL,
    most_used_copybook TEXT,
    most_used_copybook_count INTEGER,
    circular_dependencies_json TEXT,
    analysis_insights TEXT,
    mermaid_diagram TEXT,
    FOREIGN KEY(run_id) REFERENCES runs(id)
);";
        await command.ExecuteNonQueryAsync(cancellationToken);
        _logger.LogInformation("SQLite database ready at {DatabasePath}", _databasePath);
    }

    public async Task<int> StartRunAsync(string cobolSourcePath, string javaOutputPath, CancellationToken cancellationToken = default)
    {
        await using var connection = CreateConnection();
        await connection.OpenAsync(cancellationToken);
        await using var command = connection.CreateCommand();
        command.CommandText = @"
INSERT INTO runs (started_at, status, cobol_source, java_output)
VALUES ($startedAt, $status, $cobolSource, $javaOutput);
SELECT last_insert_rowid();";
        command.Parameters.AddWithValue("$startedAt", DateTime.UtcNow.ToString("O"));
        command.Parameters.AddWithValue("$status", "Running");
        command.Parameters.AddWithValue("$cobolSource", cobolSourcePath);
        command.Parameters.AddWithValue("$javaOutput", javaOutputPath);

        var result = await command.ExecuteScalarAsync(cancellationToken);
        var runId = Convert.ToInt32(result);
        _logger.LogInformation("Started migration run {RunId}", runId);
        return runId;
    }

    public async Task CompleteRunAsync(int runId, string status, string? notes = null, CancellationToken cancellationToken = default)
    {
        await using var connection = CreateConnection();
        await connection.OpenAsync(cancellationToken);
        await using var command = connection.CreateCommand();
        command.CommandText = @"
UPDATE runs
SET completed_at = $completedAt, status = $status, notes = $notes
WHERE id = $runId";
        command.Parameters.AddWithValue("$completedAt", DateTime.UtcNow.ToString("O"));
        command.Parameters.AddWithValue("$status", status);
        command.Parameters.AddWithValue("$notes", notes ?? (object)DBNull.Value);
        command.Parameters.AddWithValue("$runId", runId);
        await command.ExecuteNonQueryAsync(cancellationToken);
        _logger.LogInformation("Marked migration run {RunId} as {Status}", runId, status);
    }

    public async Task SaveCobolFilesAsync(int runId, IEnumerable<CobolFile> cobolFiles, CancellationToken cancellationToken = default)
    {
        var files = cobolFiles.ToList();
        await using var connection = CreateConnection();
        await connection.OpenAsync(cancellationToken);
        await using var transaction = await connection.BeginTransactionAsync(cancellationToken);
        var sqliteTransaction = (SqliteTransaction)transaction;

        await using (var deleteCommand = connection.CreateCommand())
        {
            deleteCommand.Transaction = sqliteTransaction;
            deleteCommand.CommandText = "DELETE FROM cobol_files WHERE run_id = $runId";
            deleteCommand.Parameters.AddWithValue("$runId", runId);
            await deleteCommand.ExecuteNonQueryAsync(cancellationToken);
        }

        foreach (var file in files)
        {
            await using var insertCommand = connection.CreateCommand();
            insertCommand.Transaction = sqliteTransaction;
            insertCommand.CommandText = @"
INSERT INTO cobol_files (run_id, file_name, file_path, is_copybook, content)
VALUES ($runId, $fileName, $filePath, $isCopybook, $content);";
            insertCommand.Parameters.AddWithValue("$runId", runId);
            insertCommand.Parameters.AddWithValue("$fileName", file.FileName);
            insertCommand.Parameters.AddWithValue("$filePath", file.FilePath);
            insertCommand.Parameters.AddWithValue("$isCopybook", file.IsCopybook ? 1 : 0);
            insertCommand.Parameters.AddWithValue("$content", file.Content);
            await insertCommand.ExecuteNonQueryAsync(cancellationToken);
        }

        await transaction.CommitAsync(cancellationToken);
        _logger.LogInformation("Persisted {Count} COBOL files for run {RunId}", files.Count, runId);
    }

    public async Task SaveAnalysesAsync(int runId, IEnumerable<CobolAnalysis> analyses, CancellationToken cancellationToken = default)
    {
        var analysisList = analyses.ToList();
        await using var connection = CreateConnection();
        await connection.OpenAsync(cancellationToken);
        await using var transaction = await connection.BeginTransactionAsync(cancellationToken);
        var sqliteTransaction = (SqliteTransaction)transaction;

        await using (var deleteCommand = connection.CreateCommand())
        {
            deleteCommand.Transaction = sqliteTransaction;
            deleteCommand.CommandText = @"
DELETE FROM analyses
WHERE cobol_file_id IN (SELECT id FROM cobol_files WHERE run_id = $runId)";
            deleteCommand.Parameters.AddWithValue("$runId", runId);
            await deleteCommand.ExecuteNonQueryAsync(cancellationToken);
        }

        foreach (var analysis in analysisList)
        {
            var cobolFileId = await GetCobolFileIdAsync(connection, sqliteTransaction, runId, analysis.FileName, cancellationToken);
            if (cobolFileId == null)
            {
                _logger.LogWarning("Missing COBOL file entry for analysis {FileName} in run {RunId}. Creating placeholder record.", analysis.FileName, runId);
                cobolFileId = await InsertPlaceholderCobolFileAsync(connection, sqliteTransaction, runId, analysis, cancellationToken);
            }

            await using var insertCommand = connection.CreateCommand();
            insertCommand.Transaction = sqliteTransaction;
            insertCommand.CommandText = @"
INSERT INTO analyses (
    cobol_file_id,
    program_description,
    raw_analysis,
    data_divisions_json,
    procedure_divisions_json,
    variables_json,
    paragraphs_json,
    copybooks_json)
VALUES (
    $cobolFileId,
    $programDescription,
    $rawAnalysis,
    $dataDivisions,
    $procedureDivisions,
    $variables,
    $paragraphs,
    $copybooks);";
            insertCommand.Parameters.AddWithValue("$cobolFileId", cobolFileId.Value);
            insertCommand.Parameters.AddWithValue("$programDescription", analysis.ProgramDescription ?? string.Empty);
            insertCommand.Parameters.AddWithValue("$rawAnalysis", analysis.RawAnalysisData ?? string.Empty);
            insertCommand.Parameters.AddWithValue("$dataDivisions", SerializeOrNull(analysis.DataDivisions) ?? (object)DBNull.Value);
            insertCommand.Parameters.AddWithValue("$procedureDivisions", SerializeOrNull(analysis.ProcedureDivisions) ?? (object)DBNull.Value);
            insertCommand.Parameters.AddWithValue("$variables", SerializeOrNull(analysis.Variables) ?? (object)DBNull.Value);
            insertCommand.Parameters.AddWithValue("$paragraphs", SerializeOrNull(analysis.Paragraphs) ?? (object)DBNull.Value);
            insertCommand.Parameters.AddWithValue("$copybooks", SerializeOrNull(analysis.CopybooksReferenced) ?? (object)DBNull.Value);
            await insertCommand.ExecuteNonQueryAsync(cancellationToken);
        }

        await transaction.CommitAsync(cancellationToken);
        _logger.LogInformation("Persisted {Count} COBOL analyses for run {RunId}", analysisList.Count, runId);
    }

    public async Task SaveDependencyMapAsync(int runId, DependencyMap dependencyMap, CancellationToken cancellationToken = default)
    {
        await using var connection = CreateConnection();
        await connection.OpenAsync(cancellationToken);
        await using var transaction = await connection.BeginTransactionAsync(cancellationToken);
        var sqliteTransaction = (SqliteTransaction)transaction;

        await using (var deleteDependencies = connection.CreateCommand())
        {
            deleteDependencies.Transaction = sqliteTransaction;
            deleteDependencies.CommandText = "DELETE FROM dependencies WHERE run_id = $runId";
            deleteDependencies.Parameters.AddWithValue("$runId", runId);
            await deleteDependencies.ExecuteNonQueryAsync(cancellationToken);
        }

        await using (var deleteCopybooks = connection.CreateCommand())
        {
            deleteCopybooks.Transaction = sqliteTransaction;
            deleteCopybooks.CommandText = "DELETE FROM copybook_usage WHERE run_id = $runId";
            deleteCopybooks.Parameters.AddWithValue("$runId", runId);
            await deleteCopybooks.ExecuteNonQueryAsync(cancellationToken);
        }

        await using (var deleteMetrics = connection.CreateCommand())
        {
            deleteMetrics.Transaction = sqliteTransaction;
            deleteMetrics.CommandText = "DELETE FROM metrics WHERE run_id = $runId";
            deleteMetrics.Parameters.AddWithValue("$runId", runId);
            await deleteMetrics.ExecuteNonQueryAsync(cancellationToken);
        }

        foreach (var dependency in dependencyMap.Dependencies)
        {
            await using var insertDependency = connection.CreateCommand();
            insertDependency.Transaction = sqliteTransaction;
            insertDependency.CommandText = @"
INSERT INTO dependencies (run_id, source_file, target_file, dependency_type, line_number, context)
VALUES ($runId, $source, $target, $type, $line, $context);";
            AddParameter(insertDependency, "$runId", runId);
            AddParameter(insertDependency, "$source", string.IsNullOrWhiteSpace(dependency.SourceFile) ? null : dependency.SourceFile);
            AddParameter(insertDependency, "$target", string.IsNullOrWhiteSpace(dependency.TargetFile) ? null : dependency.TargetFile);
            AddParameter(insertDependency, "$type", string.IsNullOrWhiteSpace(dependency.DependencyType) ? null : dependency.DependencyType);
            AddParameter(insertDependency, "$line", dependency.LineNumber > 0 ? dependency.LineNumber : null);
            AddParameter(insertDependency, "$context", string.IsNullOrWhiteSpace(dependency.Context) ? null : dependency.Context);
            await insertDependency.ExecuteNonQueryAsync(cancellationToken);
        }

        foreach (var kvp in dependencyMap.CopybookUsage)
        {
            foreach (var copybook in kvp.Value)
            {
                await using var insertUsage = connection.CreateCommand();
                insertUsage.Transaction = sqliteTransaction;
                insertUsage.CommandText = @"
INSERT INTO copybook_usage (run_id, program, copybook)
VALUES ($runId, $program, $copybook);";
                AddParameter(insertUsage, "$runId", runId);
                AddParameter(insertUsage, "$program", string.IsNullOrWhiteSpace(kvp.Key) ? null : kvp.Key);
                AddParameter(insertUsage, "$copybook", string.IsNullOrWhiteSpace(copybook) ? null : copybook);
                await insertUsage.ExecuteNonQueryAsync(cancellationToken);
            }
        }

        await using (var insertMetrics = connection.CreateCommand())
        {
            insertMetrics.Transaction = sqliteTransaction;
            insertMetrics.CommandText = @"
INSERT INTO metrics (
    run_id,
    total_programs,
    total_copybooks,
    total_dependencies,
    avg_dependencies_per_program,
    most_used_copybook,
    most_used_copybook_count,
    circular_dependencies_json,
    analysis_insights,
    mermaid_diagram)
VALUES (
    $runId,
    $totalPrograms,
    $totalCopybooks,
    $totalDependencies,
    $avgDependencies,
    $mostUsedCopybook,
    $mostUsedCopybookCount,
    $circularDependencies,
    $analysisInsights,
    $mermaidDiagram);";
            AddParameter(insertMetrics, "$runId", runId);
            AddParameter(insertMetrics, "$totalPrograms", dependencyMap.Metrics.TotalPrograms);
            AddParameter(insertMetrics, "$totalCopybooks", dependencyMap.Metrics.TotalCopybooks);
            AddParameter(insertMetrics, "$totalDependencies", dependencyMap.Metrics.TotalDependencies);
            AddParameter(insertMetrics, "$avgDependencies", dependencyMap.Metrics.AverageDependenciesPerProgram);
            AddParameter(insertMetrics, "$mostUsedCopybook", string.IsNullOrWhiteSpace(dependencyMap.Metrics.MostUsedCopybook) ? null : dependencyMap.Metrics.MostUsedCopybook);
            AddParameter(insertMetrics, "$mostUsedCopybookCount", dependencyMap.Metrics.MostUsedCopybookCount);
            AddParameter(insertMetrics, "$circularDependencies", SerializeOrNull(dependencyMap.Metrics.CircularDependencies));
            AddParameter(insertMetrics, "$analysisInsights", string.IsNullOrWhiteSpace(dependencyMap.AnalysisInsights) ? null : dependencyMap.AnalysisInsights);
            AddParameter(insertMetrics, "$mermaidDiagram", string.IsNullOrWhiteSpace(dependencyMap.MermaidDiagram) ? null : dependencyMap.MermaidDiagram);
            await insertMetrics.ExecuteNonQueryAsync(cancellationToken);
        }

        await transaction.CommitAsync(cancellationToken);
        _logger.LogInformation("Persisted dependency map for run {RunId} ({DependencyCount} dependencies)", runId, dependencyMap.Dependencies.Count);
    }

    public async Task<MigrationRunSummary?> GetLatestRunAsync(CancellationToken cancellationToken = default)
    {
        await using var connection = CreateConnection();
        await connection.OpenAsync(cancellationToken);
        await using var command = connection.CreateCommand();
        command.CommandText = @"
SELECT id, started_at, completed_at, status, cobol_source, java_output, notes
FROM runs
ORDER BY started_at DESC
LIMIT 1";

        await using var reader = await command.ExecuteReaderAsync(cancellationToken);
        if (!await reader.ReadAsync(cancellationToken))
        {
            return null;
        }

        var summary = await MapRunSummaryAsync(connection, reader, cancellationToken);
        _logger.LogInformation("Fetched latest run {RunId}", summary.RunId);
        return summary;
    }

    public async Task<MigrationRunSummary?> GetRunAsync(int runId, CancellationToken cancellationToken = default)
    {
        await using var connection = CreateConnection();
        await connection.OpenAsync(cancellationToken);
        await using var command = connection.CreateCommand();
        command.CommandText = @"
SELECT id, started_at, completed_at, status, cobol_source, java_output, notes
FROM runs
WHERE id = $runId";
        command.Parameters.AddWithValue("$runId", runId);

        await using var reader = await command.ExecuteReaderAsync(cancellationToken);
        if (!await reader.ReadAsync(cancellationToken))
        {
            return null;
        }

        return await MapRunSummaryAsync(connection, reader, cancellationToken);
    }

    public async Task<IReadOnlyList<CobolAnalysis>> GetAnalysesAsync(int runId, CancellationToken cancellationToken = default)
    {
        var result = new List<CobolAnalysis>();
        await using var connection = CreateConnection();
        await connection.OpenAsync(cancellationToken);
        await using var command = connection.CreateCommand();
        command.CommandText = @"
SELECT cf.file_name,
       cf.file_path,
       a.program_description,
       a.raw_analysis,
       a.data_divisions_json,
       a.procedure_divisions_json,
       a.variables_json,
       a.paragraphs_json,
       a.copybooks_json
FROM analyses a
JOIN cobol_files cf ON a.cobol_file_id = cf.id
WHERE cf.run_id = $runId";
        command.Parameters.AddWithValue("$runId", runId);

        await using var reader = await command.ExecuteReaderAsync(cancellationToken);
        while (await reader.ReadAsync(cancellationToken))
        {
            var analysis = new CobolAnalysis
            {
                FileName = reader.GetString(0),
                FilePath = reader.GetString(1),
                ProgramDescription = reader.IsDBNull(2) ? string.Empty : reader.GetString(2),
                RawAnalysisData = reader.IsDBNull(3) ? string.Empty : reader.GetString(3),
                DataDivisions = DeserializeList<string>(reader, 4) ?? new List<string>(),
                ProcedureDivisions = DeserializeList<string>(reader, 5) ?? new List<string>(),
                Variables = DeserializeList<CobolVariable>(reader, 6) ?? new List<CobolVariable>(),
                Paragraphs = DeserializeList<CobolParagraph>(reader, 7) ?? new List<CobolParagraph>(),
                CopybooksReferenced = DeserializeList<string>(reader, 8) ?? new List<string>()
            };
            result.Add(analysis);
        }

        return result;
    }

    public async Task<DependencyMap?> GetDependencyMapAsync(int runId, CancellationToken cancellationToken = default)
    {
        var dependencyMap = new DependencyMap();
        await using var connection = CreateConnection();
        await connection.OpenAsync(cancellationToken);

        // Dependencies
        await using (var dependencyCommand = connection.CreateCommand())
        {
            dependencyCommand.CommandText = @"SELECT source_file, target_file, dependency_type, line_number, context FROM dependencies WHERE run_id = $runId";
            dependencyCommand.Parameters.AddWithValue("$runId", runId);
            await using var reader = await dependencyCommand.ExecuteReaderAsync(cancellationToken);
            while (await reader.ReadAsync(cancellationToken))
            {
                dependencyMap.Dependencies.Add(new DependencyRelationship
                {
                    SourceFile = reader.GetString(0),
                    TargetFile = reader.GetString(1),
                    DependencyType = reader.IsDBNull(2) ? string.Empty : reader.GetString(2),
                    LineNumber = reader.IsDBNull(3) ? 0 : reader.GetInt32(3),
                    Context = reader.IsDBNull(4) ? string.Empty : reader.GetString(4)
                });
            }
        }

        // Copybook usage
        await using (var usageCommand = connection.CreateCommand())
        {
            usageCommand.CommandText = @"SELECT program, copybook FROM copybook_usage WHERE run_id = $runId";
            usageCommand.Parameters.AddWithValue("$runId", runId);
            await using var reader = await usageCommand.ExecuteReaderAsync(cancellationToken);
            while (await reader.ReadAsync(cancellationToken))
            {
                var program = reader.GetString(0);
                var copybook = reader.GetString(1);
                if (!dependencyMap.CopybookUsage.TryGetValue(program, out var list))
                {
                    list = new List<string>();
                    dependencyMap.CopybookUsage[program] = list;
                }

                if (!list.Contains(copybook))
                {
                    list.Add(copybook);
                }
            }
        }

        foreach (var kvp in dependencyMap.CopybookUsage)
        {
            foreach (var copybook in kvp.Value)
            {
                if (!dependencyMap.ReverseDependencies.TryGetValue(copybook, out var list))
                {
                    list = new List<string>();
                    dependencyMap.ReverseDependencies[copybook] = list;
                }

                if (!list.Contains(kvp.Key))
                {
                    list.Add(kvp.Key);
                }
            }
        }

        // Metrics
        await using (var metricsCommand = connection.CreateCommand())
        {
            metricsCommand.CommandText = @"SELECT total_programs, total_copybooks, total_dependencies, avg_dependencies_per_program, most_used_copybook, most_used_copybook_count, circular_dependencies_json, analysis_insights, mermaid_diagram FROM metrics WHERE run_id = $runId";
            metricsCommand.Parameters.AddWithValue("$runId", runId);
            await using var reader = await metricsCommand.ExecuteReaderAsync(cancellationToken);
            if (await reader.ReadAsync(cancellationToken))
            {
                dependencyMap.Metrics.TotalPrograms = reader.IsDBNull(0) ? 0 : reader.GetInt32(0);
                dependencyMap.Metrics.TotalCopybooks = reader.IsDBNull(1) ? 0 : reader.GetInt32(1);
                dependencyMap.Metrics.TotalDependencies = reader.IsDBNull(2) ? 0 : reader.GetInt32(2);
                dependencyMap.Metrics.AverageDependenciesPerProgram = reader.IsDBNull(3) ? 0 : reader.GetDouble(3);
                dependencyMap.Metrics.MostUsedCopybook = reader.IsDBNull(4) ? string.Empty : reader.GetString(4);
                dependencyMap.Metrics.MostUsedCopybookCount = reader.IsDBNull(5) ? 0 : reader.GetInt32(5);
                dependencyMap.Metrics.CircularDependencies = DeserializeJson<List<string>>(reader.IsDBNull(6) ? null : reader.GetString(6)) ?? new List<string>();
                dependencyMap.AnalysisInsights = reader.IsDBNull(7) ? string.Empty : reader.GetString(7);
                dependencyMap.MermaidDiagram = reader.IsDBNull(8) ? string.Empty : reader.GetString(8);
            }
        }

        dependencyMap.CreatedAt = DateTime.UtcNow;
        return dependencyMap;
    }

    public async Task<IReadOnlyList<DependencyRelationship>> GetDependenciesAsync(int runId, CancellationToken cancellationToken = default)
    {
        var result = new List<DependencyRelationship>();
        await using var connection = CreateConnection();
        await connection.OpenAsync(cancellationToken);
        await using var command = connection.CreateCommand();
        command.CommandText = @"SELECT source_file, target_file, dependency_type, line_number, context FROM dependencies WHERE run_id = $runId";
        command.Parameters.AddWithValue("$runId", runId);
        await using var reader = await command.ExecuteReaderAsync(cancellationToken);
        while (await reader.ReadAsync(cancellationToken))
        {
            result.Add(new DependencyRelationship
            {
                SourceFile = reader.GetString(0),
                TargetFile = reader.GetString(1),
                DependencyType = reader.IsDBNull(2) ? string.Empty : reader.GetString(2),
                LineNumber = reader.IsDBNull(3) ? 0 : reader.GetInt32(3),
                Context = reader.IsDBNull(4) ? string.Empty : reader.GetString(4)
            });
        }

        return result;
    }

    public async Task<IReadOnlyList<CobolFile>> SearchCobolFilesAsync(int runId, string? searchTerm, CancellationToken cancellationToken = default)
    {
        await using var connection = CreateConnection();
        await connection.OpenAsync(cancellationToken);
        await using var command = connection.CreateCommand();

        if (string.IsNullOrWhiteSpace(searchTerm))
        {
            command.CommandText = @"SELECT file_name, file_path, is_copybook, content FROM cobol_files WHERE run_id = $runId";
            command.Parameters.AddWithValue("$runId", runId);
        }
        else
        {
            command.CommandText = @"
SELECT file_name, file_path, is_copybook, content
FROM cobol_files
WHERE run_id = $runId AND (file_name LIKE $term OR content LIKE $term)";
            command.Parameters.AddWithValue("$runId", runId);
            command.Parameters.AddWithValue("$term", $"%{searchTerm}%");
        }

        var result = new List<CobolFile>();
        await using var reader = await command.ExecuteReaderAsync(cancellationToken);
        while (await reader.ReadAsync(cancellationToken))
        {
            result.Add(new CobolFile
            {
                FileName = reader.GetString(0),
                FilePath = reader.GetString(1),
                IsCopybook = reader.GetInt32(2) == 1,
                Content = reader.IsDBNull(3) ? string.Empty : reader.GetString(3)
            });
        }

        return result;
    }

    public SqliteConnection CreateConnection() => new(_connectionString);

    private static string? SerializeOrNull<T>(IEnumerable<T>? value)
    {
        if (value == null)
        {
            return null;
        }

        var list = value.ToList();
        if (list.Count == 0)
        {
            return null;
        }

        return JsonSerializer.Serialize(list, JsonOptions);
    }

    private static List<T>? DeserializeList<T>(SqliteDataReader reader, int ordinal)
    {
        if (reader.IsDBNull(ordinal))
        {
            return null;
        }

        var json = reader.GetString(ordinal);
        return DeserializeJson<List<T>>(json);
    }

    private static T? DeserializeJson<T>(string? json)
    {
        if (string.IsNullOrWhiteSpace(json))
        {
            return default;
        }

        try
        {
            return JsonSerializer.Deserialize<T>(json, JsonOptions);
        }
        catch
        {
            return default;
        }
    }

    private static async Task<int?> GetCobolFileIdAsync(SqliteConnection connection, SqliteTransaction transaction, int runId, string fileName, CancellationToken cancellationToken)
    {
        await using var command = connection.CreateCommand();
        command.Transaction = transaction;
        command.CommandText = "SELECT id FROM cobol_files WHERE run_id = $runId AND file_name = $fileName LIMIT 1";
        command.Parameters.AddWithValue("$runId", runId);
        command.Parameters.AddWithValue("$fileName", fileName);
        var result = await command.ExecuteScalarAsync(cancellationToken);
        return result == null ? null : Convert.ToInt32(result);
    }

    private static async Task<int> InsertPlaceholderCobolFileAsync(SqliteConnection connection, SqliteTransaction transaction, int runId, CobolAnalysis analysis, CancellationToken cancellationToken)
    {
        await using var command = connection.CreateCommand();
        command.Transaction = transaction;
        command.CommandText = @"
INSERT INTO cobol_files (run_id, file_name, file_path, is_copybook, content)
VALUES ($runId, $fileName, $filePath, 0, $content);
SELECT last_insert_rowid();";
        command.Parameters.AddWithValue("$runId", runId);
        command.Parameters.AddWithValue("$fileName", analysis.FileName);
        command.Parameters.AddWithValue("$filePath", analysis.FilePath ?? analysis.FileName);
        command.Parameters.AddWithValue("$content", string.Empty);
        var result = await command.ExecuteScalarAsync(cancellationToken);
        return Convert.ToInt32(result);
    }

    private static void AddParameter(SqliteCommand command, string name, object? value)
    {
        var parameter = command.CreateParameter();
        parameter.ParameterName = name;

        switch (value)
        {
            case null:
                parameter.Value = DBNull.Value;
                break;
            case double d when double.IsNaN(d) || double.IsInfinity(d):
                parameter.Value = DBNull.Value;
                break;
            default:
                parameter.Value = value;
                break;
        }

        command.Parameters.Add(parameter);
    }

    private async Task<MigrationRunSummary> MapRunSummaryAsync(SqliteConnection connection, SqliteDataReader reader, CancellationToken cancellationToken)
    {
        var runId = reader.GetInt32(0);
        var summary = new MigrationRunSummary
        {
            RunId = runId,
            StartedAt = DateTime.Parse(reader.GetString(1)),
            CompletedAt = reader.IsDBNull(2) ? null : DateTime.Parse(reader.GetString(2)),
            Status = reader.GetString(3),
            CobolSourcePath = reader.IsDBNull(4) ? string.Empty : reader.GetString(4),
            JavaOutputPath = reader.IsDBNull(5) ? string.Empty : reader.GetString(5),
            Notes = reader.IsDBNull(6) ? null : reader.GetString(6)
        };

        await using var metricsCommand = connection.CreateCommand();
        metricsCommand.CommandText = @"SELECT total_programs, total_copybooks, total_dependencies, avg_dependencies_per_program, most_used_copybook, most_used_copybook_count, circular_dependencies_json, analysis_insights, mermaid_diagram FROM metrics WHERE run_id = $runId";
        metricsCommand.Parameters.AddWithValue("$runId", runId);
        await using var metricsReader = await metricsCommand.ExecuteReaderAsync(cancellationToken);
        if (await metricsReader.ReadAsync(cancellationToken))
        {
            summary.Metrics = new DependencyMetrics
            {
                TotalPrograms = metricsReader.IsDBNull(0) ? 0 : metricsReader.GetInt32(0),
                TotalCopybooks = metricsReader.IsDBNull(1) ? 0 : metricsReader.GetInt32(1),
                TotalDependencies = metricsReader.IsDBNull(2) ? 0 : metricsReader.GetInt32(2),
                AverageDependenciesPerProgram = metricsReader.IsDBNull(3) ? 0 : metricsReader.GetDouble(3),
                MostUsedCopybook = metricsReader.IsDBNull(4) ? string.Empty : metricsReader.GetString(4),
                MostUsedCopybookCount = metricsReader.IsDBNull(5) ? 0 : metricsReader.GetInt32(5),
                CircularDependencies = DeserializeJson<List<string>>(metricsReader.IsDBNull(6) ? null : metricsReader.GetString(6)) ?? new List<string>()
            };
            summary.AnalysisInsights = metricsReader.IsDBNull(7) ? null : metricsReader.GetString(7);
            summary.MermaidDiagram = metricsReader.IsDBNull(8) ? null : metricsReader.GetString(8);
        }

        return summary;
    }

    public async Task<GraphVisualizationData?> GetDependencyGraphDataAsync(int runId)
    {
        await using var connection = CreateConnection();
        await connection.OpenAsync();

        // Get all dependencies for this run
        var query = @"
            SELECT DISTINCT source_file, target_file, dependency_type 
            FROM dependencies 
            WHERE run_id = $runId";

        await using var command = connection.CreateCommand();
        command.CommandText = query;
        command.Parameters.AddWithValue("$runId", runId);

        var nodes = new HashSet<string>();
        var edges = new List<GraphEdge>();

        await using var reader = await command.ExecuteReaderAsync();
        while (await reader.ReadAsync())
        {
            var source = reader.GetString(0);
            var target = reader.GetString(1);
            var depType = reader.IsDBNull(2) ? "DEPENDS_ON" : reader.GetString(2);

            nodes.Add(source);
            nodes.Add(target);

            edges.Add(new GraphEdge
            {
                Source = source,
                Target = target,
                Type = depType
            });
        }

        if (nodes.Count == 0 && edges.Count == 0)
        {
            _logger.LogWarning("No dependencies found in SQLite for run {RunId}", runId);
            return null;
        }

        var graphNodes = nodes.Select(n => new GraphNode
        {
            Id = n,
            Label = n,
            IsCopybook = n.EndsWith(".cpy", StringComparison.OrdinalIgnoreCase)
        }).ToList();

        _logger.LogInformation("Built graph from SQLite for run {RunId}: {NodeCount} nodes, {EdgeCount} edges",
            runId, graphNodes.Count, edges.Count);

        return new GraphVisualizationData
        {
            Nodes = graphNodes,
            Edges = edges
        };
    }
}
