using Microsoft.Data.Sqlite;
using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Chunking.Interfaces;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Chunking.Core;

/// <summary>
/// SQLite-backed implementation of the type mapping table.
/// Ensures variable type mappings are consistent across chunks.
/// </summary>
public class TypeMappingTable : ITypeMappingTable
{
    private readonly string _connectionString;
    private readonly ILogger<TypeMappingTable> _logger;

    public TypeMappingTable(string databasePath, ILogger<TypeMappingTable> logger)
    {
        _connectionString = $"Data Source={databasePath};Cache=Shared";
        _logger = logger;
    }

    public async Task<TypeMapping> RegisterMappingAsync(
        int runId,
        string sourceFile,
        TypeMapping mapping,
        CancellationToken cancellationToken = default)
    {
        // First check if mapping already exists
        var existing = await GetMappingAsync(runId, sourceFile, mapping.LegacyVariable, cancellationToken);
        if (existing != null)
        {
            _logger.LogDebug(
                "Type mapping for {Variable} already exists in run {RunId}, returning existing",
                mapping.LegacyVariable, runId);
            return existing;
        }

        await using var connection = new SqliteConnection(_connectionString);
        await connection.OpenAsync(cancellationToken);

        await using var command = connection.CreateCommand();
        command.CommandText = @"
INSERT INTO type_mappings (run_id, source_file, legacy_variable, legacy_type, target_type, target_field_name, is_nullable, default_value)
VALUES ($runId, $sourceFile, $legacyVariable, $legacyType, $targetType, $targetFieldName, $isNullable, $defaultValue)
ON CONFLICT(run_id, source_file, legacy_variable) DO NOTHING;

SELECT id, legacy_variable, legacy_type, target_type, target_field_name, is_nullable, default_value
FROM type_mappings
WHERE run_id = $runId AND source_file = $sourceFile AND legacy_variable = $legacyVariable;";

        command.Parameters.AddWithValue("$runId", runId);
        command.Parameters.AddWithValue("$sourceFile", sourceFile);
        command.Parameters.AddWithValue("$legacyVariable", mapping.LegacyVariable);
        command.Parameters.AddWithValue("$legacyType", mapping.LegacyType);
        command.Parameters.AddWithValue("$targetType", mapping.TargetType);
        command.Parameters.AddWithValue("$targetFieldName", mapping.TargetFieldName);
        command.Parameters.AddWithValue("$isNullable", mapping.IsNullable ? 1 : 0);
        command.Parameters.AddWithValue("$defaultValue", mapping.DefaultValue ?? (object)DBNull.Value);

        await using var reader = await command.ExecuteReaderAsync(cancellationToken);
        if (await reader.ReadAsync(cancellationToken))
        {
            var result = MapTypeMapping(reader);
            _logger.LogDebug(
                "Registered type mapping: {Variable} ({LegacyType}) → {TargetType}",
                result.LegacyVariable, result.LegacyType, result.TargetType);
            return result;
        }

        return mapping;
    }

    public async Task<TypeMapping?> GetMappingAsync(
        int runId,
        string? sourceFile,
        string legacyVariable,
        CancellationToken cancellationToken = default)
    {
        await using var connection = new SqliteConnection(_connectionString);
        await connection.OpenAsync(cancellationToken);

        await using var command = connection.CreateCommand();

        if (sourceFile != null)
        {
            command.CommandText = @"
SELECT id, legacy_variable, legacy_type, target_type, target_field_name, is_nullable, default_value
FROM type_mappings
WHERE run_id = $runId AND source_file = $sourceFile AND legacy_variable = $legacyVariable;";
            command.Parameters.AddWithValue("$sourceFile", sourceFile);
        }
        else
        {
            command.CommandText = @"
SELECT id, legacy_variable, legacy_type, target_type, target_field_name, is_nullable, default_value
FROM type_mappings
WHERE run_id = $runId AND legacy_variable = $legacyVariable
LIMIT 1;";
        }

        command.Parameters.AddWithValue("$runId", runId);
        command.Parameters.AddWithValue("$legacyVariable", legacyVariable);

        await using var reader = await command.ExecuteReaderAsync(cancellationToken);
        if (await reader.ReadAsync(cancellationToken))
        {
            return MapTypeMapping(reader);
        }

        return null;
    }

    public async Task<IReadOnlyList<TypeMapping>> GetAllMappingsAsync(
        int runId,
        string? sourceFile = null,
        CancellationToken cancellationToken = default)
    {
        var results = new List<TypeMapping>();
        await using var connection = new SqliteConnection(_connectionString);
        await connection.OpenAsync(cancellationToken);

        await using var command = connection.CreateCommand();

        if (sourceFile != null)
        {
            command.CommandText = @"
SELECT id, legacy_variable, legacy_type, target_type, target_field_name, is_nullable, default_value
FROM type_mappings
WHERE run_id = $runId AND source_file = $sourceFile
ORDER BY id;";
            command.Parameters.AddWithValue("$sourceFile", sourceFile);
        }
        else
        {
            command.CommandText = @"
SELECT id, legacy_variable, legacy_type, target_type, target_field_name, is_nullable, default_value
FROM type_mappings
WHERE run_id = $runId
ORDER BY id;";
        }

        command.Parameters.AddWithValue("$runId", runId);

        await using var reader = await command.ExecuteReaderAsync(cancellationToken);
        while (await reader.ReadAsync(cancellationToken))
        {
            results.Add(MapTypeMapping(reader));
        }

        return results;
    }

    public async Task<int> BulkRegisterMappingsAsync(
        int runId,
        string sourceFile,
        IEnumerable<TypeMapping> mappings,
        CancellationToken cancellationToken = default)
    {
        var mappingList = mappings.ToList();
        if (mappingList.Count == 0) return 0;

        int registered = 0;

        await using var connection = new SqliteConnection(_connectionString);
        await connection.OpenAsync(cancellationToken);
        await using var transaction = await connection.BeginTransactionAsync(cancellationToken);

        try
        {
            foreach (var mapping in mappingList)
            {
                await using var command = connection.CreateCommand();
                command.Transaction = (SqliteTransaction)transaction;
                command.CommandText = @"
INSERT INTO type_mappings (run_id, source_file, legacy_variable, legacy_type, target_type, target_field_name, is_nullable, default_value)
VALUES ($runId, $sourceFile, $legacyVariable, $legacyType, $targetType, $targetFieldName, $isNullable, $defaultValue)
ON CONFLICT(run_id, source_file, legacy_variable) DO NOTHING;";

                command.Parameters.AddWithValue("$runId", runId);
                command.Parameters.AddWithValue("$sourceFile", sourceFile);
                command.Parameters.AddWithValue("$legacyVariable", mapping.LegacyVariable);
                command.Parameters.AddWithValue("$legacyType", mapping.LegacyType);
                command.Parameters.AddWithValue("$targetType", mapping.TargetType);
                command.Parameters.AddWithValue("$targetFieldName", mapping.TargetFieldName);
                command.Parameters.AddWithValue("$isNullable", mapping.IsNullable ? 1 : 0);
                command.Parameters.AddWithValue("$defaultValue", mapping.DefaultValue ?? (object)DBNull.Value);

                var affected = await command.ExecuteNonQueryAsync(cancellationToken);
                if (affected > 0) registered++;
            }

            await transaction.CommitAsync(cancellationToken);
            _logger.LogInformation(
                "Bulk registered {Count} type mappings for {File} in run {RunId}",
                registered, sourceFile, runId);
        }
        catch
        {
            await transaction.RollbackAsync(cancellationToken);
            throw;
        }

        return registered;
    }

    public string InferTargetType(string legacyType, TargetLanguage targetLanguage)
    {
        var normalizedType = legacyType.ToUpperInvariant().Trim();

        // COBOL PIC clause type inference
        if (normalizedType.StartsWith("PIC") || normalizedType.StartsWith("PICTURE"))
        {
            return InferFromCobolPic(normalizedType, targetLanguage);
        }

        // PL/I type inference
        if (normalizedType.Contains("FIXED") || normalizedType.Contains("FLOAT") ||
            normalizedType.Contains("CHAR") || normalizedType.Contains("BIT"))
        {
            return InferFromPliType(normalizedType, targetLanguage);
        }

        // FORTRAN type inference
        if (normalizedType.StartsWith("INTEGER") || normalizedType.StartsWith("REAL") ||
            normalizedType.StartsWith("DOUBLE") || normalizedType.StartsWith("CHARACTER") ||
            normalizedType.StartsWith("LOGICAL"))
        {
            return InferFromFortranType(normalizedType, targetLanguage);
        }

        // Default fallback
        return targetLanguage == TargetLanguage.CSharp ? "string" : "String";
    }

    private static string InferFromCobolPic(string picClause, TargetLanguage targetLanguage)
    {
        var isCSharp = targetLanguage == TargetLanguage.CSharp;

        // Extract the picture string (e.g., "PIC X(10)" → "X(10)")
        var pic = picClause.Replace("PIC", "").Replace("PICTURE", "").Trim();

        // Numeric with decimal (S9(n)V9(m) or 9(n)V9(m))
        if (pic.Contains('V') || pic.Contains('.'))
        {
            // Count digits before and after V
            var beforeV = CountDigits(pic.Split('V', '.')[0]);
            var afterV = pic.Contains('V') || pic.Contains('.') 
                ? CountDigits(pic.Split('V', '.').Last()) 
                : 0;

            if (afterV > 0)
            {
                // Decimal type for precision
                return isCSharp ? "decimal" : "BigDecimal";
            }
        }

        // Signed or unsigned integer (S9(n) or 9(n))
        if (pic.Contains('9') && !pic.Contains('X') && !pic.Contains('A'))
        {
            var digits = CountDigits(pic);

            if (digits <= 4)
                return isCSharp ? "short" : "short";
            if (digits <= 9)
                return isCSharp ? "int" : "int";
            if (digits <= 18)
                return isCSharp ? "long" : "long";

            return isCSharp ? "decimal" : "BigDecimal";
        }

        // Alphanumeric (X(n))
        if (pic.Contains('X'))
        {
            return isCSharp ? "string" : "String";
        }

        // Alphabetic (A(n))
        if (pic.Contains('A'))
        {
            return isCSharp ? "string" : "String";
        }

        // Binary/COMP
        if (picClause.Contains("COMP") || picClause.Contains("BINARY"))
        {
            return isCSharp ? "int" : "int";
        }

        // Default to string
        return isCSharp ? "string" : "String";
    }

    private static string InferFromPliType(string pliType, TargetLanguage targetLanguage)
    {
        var isCSharp = targetLanguage == TargetLanguage.CSharp;
        var normalized = pliType.ToUpperInvariant();

        if (normalized.Contains("FIXED") && normalized.Contains("BIN"))
        {
            // FIXED BINARY - integer types
            return isCSharp ? "int" : "int";
        }

        if (normalized.Contains("FIXED") && normalized.Contains("DEC"))
        {
            // FIXED DECIMAL - decimal type
            return isCSharp ? "decimal" : "BigDecimal";
        }

        if (normalized.Contains("FLOAT"))
        {
            return isCSharp ? "double" : "double";
        }

        if (normalized.Contains("CHAR"))
        {
            return isCSharp ? "string" : "String";
        }

        if (normalized.Contains("BIT"))
        {
            return isCSharp ? "bool" : "boolean";
        }

        return isCSharp ? "string" : "String";
    }

    private static string InferFromFortranType(string fortranType, TargetLanguage targetLanguage)
    {
        var isCSharp = targetLanguage == TargetLanguage.CSharp;
        var normalized = fortranType.ToUpperInvariant();

        if (normalized.StartsWith("INTEGER"))
        {
            // Check for size specification
            if (normalized.Contains("*8") || normalized.Contains("(8)"))
                return isCSharp ? "long" : "long";
            if (normalized.Contains("*2") || normalized.Contains("(2)"))
                return isCSharp ? "short" : "short";
            return isCSharp ? "int" : "int";
        }

        if (normalized.StartsWith("REAL") || normalized.StartsWith("FLOAT"))
        {
            if (normalized.Contains("*8") || normalized.Contains("(8)"))
                return isCSharp ? "double" : "double";
            return isCSharp ? "float" : "float";
        }

        if (normalized.StartsWith("DOUBLE"))
        {
            return isCSharp ? "double" : "double";
        }

        if (normalized.StartsWith("CHARACTER"))
        {
            return isCSharp ? "string" : "String";
        }

        if (normalized.StartsWith("LOGICAL"))
        {
            return isCSharp ? "bool" : "boolean";
        }

        if (normalized.StartsWith("COMPLEX"))
        {
            // C# doesn't have a built-in complex type (unless using System.Numerics)
            return isCSharp ? "System.Numerics.Complex" : "Complex";
        }

        return isCSharp ? "string" : "String";
    }

    private static int CountDigits(string pic)
    {
        // Handle forms like 9(5) or 99999 or S9(7)V99
        var count = 0;
        var i = 0;

        while (i < pic.Length)
        {
            if (pic[i] == '9')
            {
                if (i + 1 < pic.Length && pic[i + 1] == '(')
                {
                    // Find the closing paren
                    var closeIdx = pic.IndexOf(')', i + 2);
                    if (closeIdx > i + 2)
                    {
                        var numStr = pic.Substring(i + 2, closeIdx - i - 2);
                        if (int.TryParse(numStr, out var num))
                        {
                            count += num;
                        }
                        i = closeIdx + 1;
                        continue;
                    }
                }
                count++;
            }
            i++;
        }

        return count;
    }

    private static TypeMapping MapTypeMapping(SqliteDataReader reader)
    {
        return new TypeMapping
        {
            LegacyVariable = reader.GetString(1),
            LegacyType = reader.GetString(2),
            TargetType = reader.GetString(3),
            TargetFieldName = reader.GetString(4),
            IsNullable = reader.GetInt32(5) == 1,
            DefaultValue = reader.IsDBNull(6) ? null : reader.GetString(6)
        };
    }
}
