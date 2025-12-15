using System.Text.Json;
using Microsoft.Data.Sqlite;
using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Chunking.Interfaces;

namespace CobolToQuarkusMigration.Chunking.Core;

/// <summary>
/// SQLite-backed implementation of the signature registry.
/// Ensures method signatures are immutable once registered.
/// </summary>
public class SignatureRegistry : ISignatureRegistry
{
    private readonly string _connectionString;
    private readonly ILogger<SignatureRegistry> _logger;
    private static readonly JsonSerializerOptions JsonOptions = new(JsonSerializerDefaults.Web);

    public SignatureRegistry(string databasePath, ILogger<SignatureRegistry> logger)
    {
        _connectionString = $"Data Source={databasePath};Cache=Shared";
        _logger = logger;
    }

    public async Task<MethodSignature> RegisterSignatureAsync(
        int runId,
        string sourceFile,
        int chunkIndex,
        MethodSignature signature,
        CancellationToken cancellationToken = default)
    {
        // First check if signature already exists
        var existing = await GetSignatureAsync(runId, sourceFile, signature.LegacyName, cancellationToken);
        if (existing != null)
        {
            _logger.LogDebug(
                "Signature for {LegacyName} already exists in run {RunId}, returning existing",
                signature.LegacyName, runId);
            return existing;
        }

        await using var connection = new SqliteConnection(_connectionString);
        await connection.OpenAsync(cancellationToken);

        await using var command = connection.CreateCommand();
        command.CommandText = @"
INSERT INTO signatures (run_id, source_file, chunk_index, legacy_name, target_method_name, target_signature, return_type, parameters)
VALUES ($runId, $sourceFile, $chunkIndex, $legacyName, $targetMethodName, $targetSignature, $returnType, $parameters)
ON CONFLICT(run_id, source_file, legacy_name) DO NOTHING;

SELECT id, legacy_name, target_method_name, target_signature, return_type, parameters
FROM signatures
WHERE run_id = $runId AND source_file = $sourceFile AND legacy_name = $legacyName;";

        command.Parameters.AddWithValue("$runId", runId);
        command.Parameters.AddWithValue("$sourceFile", sourceFile);
        command.Parameters.AddWithValue("$chunkIndex", chunkIndex);
        command.Parameters.AddWithValue("$legacyName", signature.LegacyName);
        command.Parameters.AddWithValue("$targetMethodName", signature.TargetMethodName);
        command.Parameters.AddWithValue("$targetSignature", signature.TargetSignature);
        command.Parameters.AddWithValue("$returnType", signature.ReturnType);
        command.Parameters.AddWithValue("$parameters", JsonSerializer.Serialize(signature.Parameters, JsonOptions));

        await using var reader = await command.ExecuteReaderAsync(cancellationToken);
        if (await reader.ReadAsync(cancellationToken))
        {
            var result = MapSignature(reader);
            _logger.LogInformation(
                "Registered signature for {LegacyName} → {TargetName} in run {RunId}",
                result.LegacyName, result.TargetMethodName, runId);
            return result;
        }

        // This shouldn't happen, but fallback to return the input
        return signature;
    }

    public async Task<MethodSignature?> GetSignatureAsync(
        int runId,
        string? sourceFile,
        string legacyName,
        CancellationToken cancellationToken = default)
    {
        await using var connection = new SqliteConnection(_connectionString);
        await connection.OpenAsync(cancellationToken);

        await using var command = connection.CreateCommand();
        
        if (sourceFile != null)
        {
            command.CommandText = @"
SELECT id, legacy_name, target_method_name, target_signature, return_type, parameters
FROM signatures
WHERE run_id = $runId AND source_file = $sourceFile AND legacy_name = $legacyName;";
            command.Parameters.AddWithValue("$sourceFile", sourceFile);
        }
        else
        {
            command.CommandText = @"
SELECT id, legacy_name, target_method_name, target_signature, return_type, parameters
FROM signatures
WHERE run_id = $runId AND legacy_name = $legacyName
LIMIT 1;";
        }

        command.Parameters.AddWithValue("$runId", runId);
        command.Parameters.AddWithValue("$legacyName", legacyName);

        await using var reader = await command.ExecuteReaderAsync(cancellationToken);
        if (await reader.ReadAsync(cancellationToken))
        {
            return MapSignature(reader);
        }

        return null;
    }

    public async Task<IReadOnlyList<MethodSignature>> GetAllSignaturesAsync(
        int runId,
        string? sourceFile = null,
        CancellationToken cancellationToken = default)
    {
        var results = new List<MethodSignature>();
        await using var connection = new SqliteConnection(_connectionString);
        await connection.OpenAsync(cancellationToken);

        await using var command = connection.CreateCommand();
        
        if (sourceFile != null)
        {
            command.CommandText = @"
SELECT id, legacy_name, target_method_name, target_signature, return_type, parameters
FROM signatures
WHERE run_id = $runId AND source_file = $sourceFile
ORDER BY id;";
            command.Parameters.AddWithValue("$sourceFile", sourceFile);
        }
        else
        {
            command.CommandText = @"
SELECT id, legacy_name, target_method_name, target_signature, return_type, parameters
FROM signatures
WHERE run_id = $runId
ORDER BY id;";
        }

        command.Parameters.AddWithValue("$runId", runId);

        await using var reader = await command.ExecuteReaderAsync(cancellationToken);
        while (await reader.ReadAsync(cancellationToken))
        {
            results.Add(MapSignature(reader));
        }

        return results;
    }

    public async Task<bool> SignatureExistsAsync(
        int runId,
        string legacyName,
        CancellationToken cancellationToken = default)
    {
        await using var connection = new SqliteConnection(_connectionString);
        await connection.OpenAsync(cancellationToken);

        await using var command = connection.CreateCommand();
        command.CommandText = @"
SELECT COUNT(*) FROM signatures
WHERE run_id = $runId AND legacy_name = $legacyName;";
        command.Parameters.AddWithValue("$runId", runId);
        command.Parameters.AddWithValue("$legacyName", legacyName);

        var count = Convert.ToInt64(await command.ExecuteScalarAsync(cancellationToken));
        return count > 0;
    }

    public async Task<SignatureValidationResult> ValidateSignatureAsync(
        int runId,
        MethodSignature signature,
        CancellationToken cancellationToken = default)
    {
        var result = new SignatureValidationResult { IsValid = true };

        var existing = await GetSignatureAsync(runId, null, signature.LegacyName, cancellationToken);
        if (existing == null)
        {
            // No existing signature - valid (will be registered)
            return result;
        }

        result.ExistingSignature = existing;

        // Check for discrepancies
        if (existing.TargetMethodName != signature.TargetMethodName)
        {
            result.IsValid = false;
            result.Discrepancies.Add(new SignatureDiscrepancy
            {
                Field = "TargetMethodName",
                ExpectedValue = existing.TargetMethodName,
                ActualValue = signature.TargetMethodName,
                Severity = DiscrepancySeverity.Error
            });
        }

        if (existing.ReturnType != signature.ReturnType)
        {
            result.IsValid = false;
            result.Discrepancies.Add(new SignatureDiscrepancy
            {
                Field = "ReturnType",
                ExpectedValue = existing.ReturnType,
                ActualValue = signature.ReturnType,
                Severity = DiscrepancySeverity.Error
            });
        }

        if (existing.Parameters.Count != signature.Parameters.Count)
        {
            result.IsValid = false;
            result.Discrepancies.Add(new SignatureDiscrepancy
            {
                Field = "ParameterCount",
                ExpectedValue = existing.Parameters.Count.ToString(),
                ActualValue = signature.Parameters.Count.ToString(),
                Severity = DiscrepancySeverity.Critical
            });
        }
        else
        {
            for (int i = 0; i < existing.Parameters.Count; i++)
            {
                if (existing.Parameters[i].Type != signature.Parameters[i].Type)
                {
                    result.IsValid = false;
                    result.Discrepancies.Add(new SignatureDiscrepancy
                    {
                        Field = $"Parameter[{i}].Type",
                        ExpectedValue = existing.Parameters[i].Type,
                        ActualValue = signature.Parameters[i].Type,
                        Severity = DiscrepancySeverity.Error
                    });
                }
            }
        }

        if (!result.IsValid)
        {
            _logger.LogWarning(
                "Signature validation failed for {LegacyName}: {Count} discrepancies",
                signature.LegacyName, result.Discrepancies.Count);
        }

        return result;
    }

    private static MethodSignature MapSignature(SqliteDataReader reader)
    {
        var parametersJson = reader.IsDBNull(5) ? "[]" : reader.GetString(5);
        var parameters = JsonSerializer.Deserialize<List<MethodParameter>>(parametersJson, JsonOptions) ?? new();

        return new MethodSignature
        {
            LegacyName = reader.GetString(1),
            TargetMethodName = reader.GetString(2),
            TargetSignature = reader.GetString(3),
            ReturnType = reader.GetString(4),
            Parameters = parameters
        };
    }
}
