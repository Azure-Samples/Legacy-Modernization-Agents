using System.Text;
using System.Text.Json;
using Microsoft.Data.Sqlite;
using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Chunking.Interfaces;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Chunking.Context;

/// <summary>
/// Manages context across chunks during conversion.
/// Builds context for each chunk and records conversion results.
/// </summary>
public class ChunkContextManager : IContextManager
{
    private readonly string _connectionString;
    private readonly ISignatureRegistry _signatureRegistry;
    private readonly ITypeMappingTable _typeMappingTable;
    private readonly ChunkingSettings _chunkingSettings;
    private readonly ILogger<ChunkContextManager> _logger;

    private static readonly JsonSerializerOptions JsonOptions = new(JsonSerializerDefaults.Web);

    public ChunkContextManager(
        string databasePath,
        ISignatureRegistry signatureRegistry,
        ITypeMappingTable typeMappingTable,
        ChunkingSettings chunkingSettings,
        ILogger<ChunkContextManager> logger)
    {
        _connectionString = $"Data Source={databasePath};Cache=Shared";
        _signatureRegistry = signatureRegistry;
        _typeMappingTable = typeMappingTable;
        _chunkingSettings = chunkingSettings;
        _logger = logger;
    }

    public async Task<ChunkContext> BuildContextForChunkAsync(
        int runId,
        ChunkResult chunk,
        CancellationToken cancellationToken = default)
    {
        var context = new ChunkContext
        {
            CurrentChunkIndex = chunk.ChunkIndex,
            TotalChunks = chunk.TotalChunks
        };

        // Build file summary
        context.FileSummary = BuildFileSummary(chunk);

        // Get all signatures from previous chunks
        var allSignatures = await _signatureRegistry.GetAllSignaturesAsync(runId, chunk.SourceFile, cancellationToken);
        context.PreviousSignatures = allSignatures.Select(s => new SignatureSummary
        {
            LegacyName = s.LegacyName,
            TargetMethodName = s.TargetMethodName,
            TargetSignature = s.TargetSignature,
            ReturnType = s.ReturnType
        }).ToList();

        // Get type mappings
        var allMappings = await _typeMappingTable.GetAllMappingsAsync(runId, chunk.SourceFile, cancellationToken);
        context.TypeMappings = allMappings.Select(m => new TypeMappingSummary
        {
            LegacyVariable = m.LegacyVariable,
            LegacyType = m.LegacyType,
            TargetType = m.TargetType,
            TargetFieldName = m.TargetFieldName
        }).ToList();

        // Get pending forward references
        context.PendingForwardReferences = await GetPendingForwardReferencesAsync(
            runId, chunk.SourceFile, cancellationToken);

        // Get compressed history if progressive compression is enabled
        if (_chunkingSettings.EnableProgressiveCompression && chunk.ChunkIndex > 0)
        {
            context.CompressedHistory = await BuildCompressedHistoryAsync(
                runId, chunk.SourceFile, chunk.ChunkIndex, cancellationToken);
        }

        // Mark unconverted dependencies
        context.UnconvertedDependencies = chunk.OutboundDependencies
            .Where(dep => !context.PreviousSignatures.Any(s =>
                s.LegacyName.Equals(dep, StringComparison.OrdinalIgnoreCase)))
            .ToList();

        // Estimate token count
        context.EstimatedTokens = EstimateContextTokens(context);

        _logger.LogDebug(
            "Built context for chunk {Index}/{Total} of {File}: {Tokens} estimated tokens",
            chunk.ChunkIndex + 1, chunk.TotalChunks, chunk.SourceFile, context.EstimatedTokens);

        return context;
    }

    public async Task RecordChunkResultAsync(
        int runId,
        ChunkResult chunk,
        ChunkConversionResult conversionResult,
        CancellationToken cancellationToken = default)
    {
        await using var connection = new SqliteConnection(_connectionString);
        await connection.OpenAsync(cancellationToken);
        await using var transaction = await connection.BeginTransactionAsync(cancellationToken);

        try
        {
            // Update chunk metadata
            await using (var cmd = connection.CreateCommand())
            {
                cmd.Transaction = (SqliteTransaction)transaction;
                cmd.CommandText = @"
INSERT INTO chunk_metadata (run_id, source_file, chunk_index, start_line, end_line, status, semantic_units, tokens_used, processing_time_ms, error_message, converted_code, completed_at)
VALUES ($runId, $sourceFile, $chunkIndex, $startLine, $endLine, $status, $semanticUnits, $tokensUsed, $processingTime, $errorMessage, $convertedCode, $completedAt)
ON CONFLICT(run_id, source_file, chunk_index) DO UPDATE SET
    status = $status,
    tokens_used = $tokensUsed,
    processing_time_ms = $processingTime,
    error_message = $errorMessage,
    converted_code = $convertedCode,
    completed_at = $completedAt;";

                cmd.Parameters.AddWithValue("$runId", runId);
                cmd.Parameters.AddWithValue("$sourceFile", chunk.SourceFile);
                cmd.Parameters.AddWithValue("$chunkIndex", chunk.ChunkIndex);
                cmd.Parameters.AddWithValue("$startLine", chunk.StartLine);
                cmd.Parameters.AddWithValue("$endLine", chunk.EndLine);
                cmd.Parameters.AddWithValue("$status", conversionResult.Success ? "Completed" : "Failed");
                cmd.Parameters.AddWithValue("$semanticUnits", JsonSerializer.Serialize(chunk.SemanticUnitNames, JsonOptions));
                cmd.Parameters.AddWithValue("$tokensUsed", conversionResult.TokensUsed);
                cmd.Parameters.AddWithValue("$processingTime", conversionResult.ProcessingTimeMs);
                cmd.Parameters.AddWithValue("$errorMessage", conversionResult.ErrorMessage ?? (object)DBNull.Value);
                cmd.Parameters.AddWithValue("$convertedCode", conversionResult.ConvertedCode);
                cmd.Parameters.AddWithValue("$completedAt", DateTime.UtcNow.ToString("O"));

                await cmd.ExecuteNonQueryAsync(cancellationToken);
            }

            // Register signatures from this chunk (using same connection)
            foreach (var signature in conversionResult.DefinedMethods)
            {
                // Skip registration if LegacyName is missing
                if (string.IsNullOrWhiteSpace(signature.LegacyName))
                {
                    continue;
                }

                await RegisterSignatureInTransactionAsync(
                    connection, (SqliteTransaction)transaction, 
                    runId, chunk.SourceFile, chunk.ChunkIndex, signature, cancellationToken);
            }

            // Register type mappings from this chunk (using same connection)
            foreach (var mapping in conversionResult.TypeMappings)
            {
                await RegisterTypeMappingInTransactionAsync(
                    connection, (SqliteTransaction)transaction,
                    runId, chunk.SourceFile, mapping, cancellationToken);
            }

            // Record forward references
            foreach (var forwardRef in conversionResult.ForwardReferences)
            {
                await using var cmd = connection.CreateCommand();
                cmd.Transaction = (SqliteTransaction)transaction;
                cmd.CommandText = @"
INSERT INTO forward_references (run_id, source_file, caller_method, caller_chunk_index, target_method, predicted_signature)
VALUES ($runId, $sourceFile, $callerMethod, $callerChunkIndex, $targetMethod, $predictedSignature);";

                cmd.Parameters.AddWithValue("$runId", runId);
                cmd.Parameters.AddWithValue("$sourceFile", chunk.SourceFile);
                cmd.Parameters.AddWithValue("$callerMethod", forwardRef.CallerMethod);
                cmd.Parameters.AddWithValue("$callerChunkIndex", chunk.ChunkIndex);
                cmd.Parameters.AddWithValue("$targetMethod", forwardRef.TargetMethod);
                cmd.Parameters.AddWithValue("$predictedSignature", forwardRef.PredictedSignature);

                await cmd.ExecuteNonQueryAsync(cancellationToken);
            }

            // Resolve any forward references that were defined in this chunk
            foreach (var signature in conversionResult.DefinedMethods)
            {
                await using var cmd = connection.CreateCommand();
                cmd.Transaction = (SqliteTransaction)transaction;
                cmd.CommandText = @"
UPDATE forward_references
SET resolved = 1, actual_signature = $actualSignature, resolution_chunk_index = $chunkIndex, resolved_at = $resolvedAt
WHERE run_id = $runId AND source_file = $sourceFile AND target_method = $targetMethod AND resolved = 0;";

                cmd.Parameters.AddWithValue("$runId", runId);
                cmd.Parameters.AddWithValue("$sourceFile", chunk.SourceFile);
                cmd.Parameters.AddWithValue("$targetMethod", signature.LegacyName);
                cmd.Parameters.AddWithValue("$actualSignature", signature.TargetSignature);
                cmd.Parameters.AddWithValue("$chunkIndex", chunk.ChunkIndex);
                cmd.Parameters.AddWithValue("$resolvedAt", DateTime.UtcNow.ToString("O"));

                await cmd.ExecuteNonQueryAsync(cancellationToken);
            }

            await transaction.CommitAsync(cancellationToken);

            _logger.LogInformation(
                "Recorded chunk {Index} result for {File}: {Status}, {Methods} methods, {Mappings} type mappings",
                chunk.ChunkIndex, chunk.SourceFile,
                conversionResult.Success ? "Success" : "Failed",
                conversionResult.DefinedMethods.Count,
                conversionResult.TypeMappings.Count);
        }
        catch
        {
            await transaction.RollbackAsync(cancellationToken);
            throw;
        }
    }

    public async Task<FileConversionHistory> GetFileHistoryAsync(
        int runId,
        string sourceFile,
        CancellationToken cancellationToken = default)
    {
        var history = new FileConversionHistory
        {
            RunId = runId,
            SourceFile = sourceFile
        };

        await using var connection = new SqliteConnection(_connectionString);
        await connection.OpenAsync(cancellationToken);

        // Get chunk results
        await using (var cmd = connection.CreateCommand())
        {
            cmd.CommandText = @"
SELECT chunk_index, status, converted_code, tokens_used, processing_time_ms, error_message
FROM chunk_metadata
WHERE run_id = $runId AND source_file = $sourceFile
ORDER BY chunk_index;";

            cmd.Parameters.AddWithValue("$runId", runId);
            cmd.Parameters.AddWithValue("$sourceFile", sourceFile);

            await using var reader = await cmd.ExecuteReaderAsync(cancellationToken);
            while (await reader.ReadAsync(cancellationToken))
            {
                history.ChunkResults.Add(new ChunkConversionResult
                {
                    Success = reader.GetString(1) == "Completed",
                    ConvertedCode = reader.IsDBNull(2) ? string.Empty : reader.GetString(2),
                    TokensUsed = reader.IsDBNull(3) ? 0 : reader.GetInt32(3),
                    ProcessingTimeMs = reader.IsDBNull(4) ? 0 : reader.GetInt64(4),
                    ErrorMessage = reader.IsDBNull(5) ? null : reader.GetString(5)
                });
            }
        }

        // Get all signatures
        history.AllSignatures = (await _signatureRegistry.GetAllSignaturesAsync(runId, sourceFile, cancellationToken)).ToList();

        // Get all type mappings
        history.AllTypeMappings = (await _typeMappingTable.GetAllMappingsAsync(runId, sourceFile, cancellationToken)).ToList();

        // Get unresolved forward references
        await using (var cmd = connection.CreateCommand())
        {
            cmd.CommandText = @"
SELECT caller_method, target_method, predicted_signature, caller_chunk_index
FROM forward_references
WHERE run_id = $runId AND source_file = $sourceFile AND resolved = 0;";

            cmd.Parameters.AddWithValue("$runId", runId);
            cmd.Parameters.AddWithValue("$sourceFile", sourceFile);

            await using var reader = await cmd.ExecuteReaderAsync(cancellationToken);
            while (await reader.ReadAsync(cancellationToken))
            {
                history.UnresolvedReferences.Add(new ForwardReference
                {
                    CallerMethod = reader.GetString(0),
                    TargetMethod = reader.GetString(1),
                    PredictedSignature = reader.IsDBNull(2) ? string.Empty : reader.GetString(2),
                    LineNumber = 0 // Not stored in DB
                });
            }
        }

        return history;
    }

    public async Task ClearRunContextAsync(int runId, CancellationToken cancellationToken = default)
    {
        await using var connection = new SqliteConnection(_connectionString);
        await connection.OpenAsync(cancellationToken);
        await using var transaction = await connection.BeginTransactionAsync(cancellationToken);

        try
        {
            var tables = new[] { "chunk_metadata", "forward_references", "signatures", "type_mappings" };
            foreach (var table in tables)
            {
                await using var cmd = connection.CreateCommand();
                cmd.Transaction = (SqliteTransaction)transaction;
                cmd.CommandText = $"DELETE FROM {table} WHERE run_id = $runId;";
                cmd.Parameters.AddWithValue("$runId", runId);
                await cmd.ExecuteNonQueryAsync(cancellationToken);
            }

            await transaction.CommitAsync(cancellationToken);
            _logger.LogInformation("Cleared context for run {RunId}", runId);
        }
        catch
        {
            await transaction.RollbackAsync(cancellationToken);
            throw;
        }
    }

    public async Task<int> GetLastCompletedChunkIndexAsync(
        int runId,
        string sourceFile,
        CancellationToken cancellationToken = default)
    {
        await using var connection = new SqliteConnection(_connectionString);
        await connection.OpenAsync(cancellationToken);

        await using var cmd = connection.CreateCommand();
        cmd.CommandText = @"
SELECT MAX(chunk_index)
FROM chunk_metadata
WHERE run_id = $runId AND source_file = $sourceFile AND status = 'Completed';";

        cmd.Parameters.AddWithValue("$runId", runId);
        cmd.Parameters.AddWithValue("$sourceFile", sourceFile);

        var result = await cmd.ExecuteScalarAsync(cancellationToken);
        return result == DBNull.Value ? -1 : Convert.ToInt32(result);
    }

    private async Task<List<ForwardReferenceSummary>> GetPendingForwardReferencesAsync(
        int runId,
        string sourceFile,
        CancellationToken cancellationToken)
    {
        var references = new List<ForwardReferenceSummary>();

        await using var connection = new SqliteConnection(_connectionString);
        await connection.OpenAsync(cancellationToken);

        await using var cmd = connection.CreateCommand();
        cmd.CommandText = @"
SELECT caller_method, target_method, predicted_signature, caller_chunk_index
FROM forward_references
WHERE run_id = $runId AND source_file = $sourceFile AND resolved = 0;";

        cmd.Parameters.AddWithValue("$runId", runId);
        cmd.Parameters.AddWithValue("$sourceFile", sourceFile);

        await using var reader = await cmd.ExecuteReaderAsync(cancellationToken);
        while (await reader.ReadAsync(cancellationToken))
        {
            references.Add(new ForwardReferenceSummary
            {
                CallerMethod = reader.GetString(0),
                TargetMethod = reader.GetString(1),
                PredictedSignature = reader.IsDBNull(2) ? string.Empty : reader.GetString(2),
                CallerChunkIndex = reader.GetInt32(3)
            });
        }

        return references;
    }

    private async Task<string> BuildCompressedHistoryAsync(
        int runId,
        string sourceFile,
        int currentChunkIndex,
        CancellationToken cancellationToken)
    {
        var sb = new StringBuilder();
        sb.AppendLine("// Compressed history from previous chunks:");

        await using var connection = new SqliteConnection(_connectionString);
        await connection.OpenAsync(cancellationToken);

        // Get chunk summaries
        await using var cmd = connection.CreateCommand();
        cmd.CommandText = @"
SELECT chunk_index, semantic_units, status
FROM chunk_metadata
WHERE run_id = $runId AND source_file = $sourceFile AND chunk_index < $currentChunkIndex
ORDER BY chunk_index;";

        cmd.Parameters.AddWithValue("$runId", runId);
        cmd.Parameters.AddWithValue("$sourceFile", sourceFile);
        cmd.Parameters.AddWithValue("$currentChunkIndex", currentChunkIndex);

        var fullDetailWindow = _chunkingSettings.FullDetailChunkWindow;
        var cutoffIndex = currentChunkIndex - fullDetailWindow;

        await using var reader = await cmd.ExecuteReaderAsync(cancellationToken);
        while (await reader.ReadAsync(cancellationToken))
        {
            var chunkIndex = reader.GetInt32(0);
            var semanticUnitsJson = reader.IsDBNull(1) ? "[]" : reader.GetString(1);
            var status = reader.GetString(2);

            var semanticUnits = JsonSerializer.Deserialize<List<string>>(semanticUnitsJson, JsonOptions) ?? new();

            if (chunkIndex < cutoffIndex)
            {
                // Compressed summary
                sb.AppendLine($"// Chunk {chunkIndex}: {status} - {semanticUnits.Count} units: {string.Join(", ", semanticUnits.Take(5))}...");
            }
            else
            {
                // Recent chunk - more detail
                sb.AppendLine($"// Chunk {chunkIndex}: {status}");
                foreach (var unit in semanticUnits)
                {
                    sb.AppendLine($"//   - {unit}");
                }
            }
        }

        return sb.ToString();
    }

    private static string BuildFileSummary(ChunkResult chunk)
    {
        return $"Processing chunk {chunk.ChunkIndex + 1} of {chunk.TotalChunks} " +
               $"from {chunk.SourceFile} (lines {chunk.StartLine}-{chunk.EndLine}). " +
               $"Contains {chunk.SemanticUnitNames.Count} semantic units.";
    }

    private static int EstimateContextTokens(ChunkContext context)
    {
        var estimate = context.FileSummary.Length / 4;
        estimate += context.PreviousSignatures.Sum(s => s.TargetSignature.Length / 4 + 20);
        estimate += context.TypeMappings.Sum(m => 30);
        estimate += context.CompressedHistory.Length / 4;
        estimate += context.PendingForwardReferences.Count * 40;
        return estimate;
    }

    /// <summary>
    /// Register a signature within an existing transaction to avoid SQLite locking.
    /// </summary>
    private async Task RegisterSignatureInTransactionAsync(
        SqliteConnection connection,
        SqliteTransaction transaction,
        int runId,
        string sourceFile,
        int chunkIndex,
        MethodSignature signature,
        CancellationToken cancellationToken)
    {
        await using var cmd = connection.CreateCommand();
        cmd.Transaction = transaction;
        cmd.CommandText = @"
INSERT INTO signatures (run_id, source_file, chunk_index, legacy_name, target_method_name, target_signature, return_type, parameters)
VALUES ($runId, $sourceFile, $chunkIndex, $legacyName, $targetMethodName, $targetSignature, $returnType, $parameters)
ON CONFLICT(run_id, source_file, legacy_name) DO NOTHING;";

        cmd.Parameters.AddWithValue("$runId", runId);
        cmd.Parameters.AddWithValue("$sourceFile", sourceFile);
        cmd.Parameters.AddWithValue("$chunkIndex", chunkIndex);
        cmd.Parameters.AddWithValue("$legacyName", signature.LegacyName);
        cmd.Parameters.AddWithValue("$targetMethodName", signature.TargetMethodName);
        cmd.Parameters.AddWithValue("$targetSignature", signature.TargetSignature);
        cmd.Parameters.AddWithValue("$returnType", signature.ReturnType);
        cmd.Parameters.AddWithValue("$parameters", JsonSerializer.Serialize(signature.Parameters, JsonOptions));

        await cmd.ExecuteNonQueryAsync(cancellationToken);
        
        _logger.LogDebug(
            "Registered signature for {LegacyName} → {TargetName} in run {RunId}",
            signature.LegacyName, signature.TargetMethodName, runId);
    }

    /// <summary>
    /// Register a type mapping within an existing transaction to avoid SQLite locking.
    /// </summary>
    private async Task RegisterTypeMappingInTransactionAsync(
        SqliteConnection connection,
        SqliteTransaction transaction,
        int runId,
        string sourceFile,
        TypeMapping mapping,
        CancellationToken cancellationToken)
    {
        await using var cmd = connection.CreateCommand();
        cmd.Transaction = transaction;
        cmd.CommandText = @"
INSERT INTO type_mappings (run_id, source_file, legacy_variable, legacy_type, target_type, target_field_name)
VALUES ($runId, $sourceFile, $legacyVariable, $legacyType, $targetType, $targetFieldName)
ON CONFLICT(run_id, source_file, legacy_variable) DO NOTHING;";

        cmd.Parameters.AddWithValue("$runId", runId);
        cmd.Parameters.AddWithValue("$sourceFile", sourceFile);
        cmd.Parameters.AddWithValue("$legacyVariable", mapping.LegacyVariable);
        cmd.Parameters.AddWithValue("$legacyType", mapping.LegacyType);
        cmd.Parameters.AddWithValue("$targetType", mapping.TargetType);
        cmd.Parameters.AddWithValue("$targetFieldName", mapping.TargetFieldName);

        await cmd.ExecuteNonQueryAsync(cancellationToken);
    }
}
