using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Chunking.Interfaces;
using CobolToQuarkusMigration.Chunking.Adapters;
using CobolToQuarkusMigration.Chunking.Core;
using CobolToQuarkusMigration.Chunking.Context;
using CobolToQuarkusMigration.Chunking.Validation;
using CobolToQuarkusMigration.Chunking.Models;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Chunking;

/// <summary>
/// Orchestrates the smart chunking process for large file migration.
/// </summary>
public class ChunkingOrchestrator
{
    private readonly ILogger<ChunkingOrchestrator> _logger;
    private readonly ChunkingSettings _chunkingSettings;
    private readonly ConversionSettings _conversionSettings;
    private readonly string _databasePath;
    private readonly TargetLanguage _targetLanguage;

    // Lazy-initialized components
    private ILanguageAdapter? _languageAdapter;
    private IChunker? _chunker;
    private ISignatureRegistry? _signatureRegistry;
    private ITypeMappingTable? _typeMappingTable;
    private IContextManager? _contextManager;
    private ConversionValidator? _validator;
    private NamingConventionEnforcer? _namingEnforcer;

    public ChunkingOrchestrator(
        ChunkingSettings chunkingSettings,
        ConversionSettings conversionSettings,
        string databasePath,
        ILogger<ChunkingOrchestrator> logger,
        TargetLanguage targetLanguage = TargetLanguage.Java)
    {
        _chunkingSettings = chunkingSettings;
        _conversionSettings = conversionSettings;
        _databasePath = databasePath;
        _logger = logger;
        _targetLanguage = targetLanguage;
    }

    /// <summary>
    /// Analyzes a source file and returns chunking plan without executing conversion.
    /// </summary>
    public async Task<ChunkingPlan> AnalyzeFileAsync(
        string filePath,
        string content,
        CancellationToken cancellationToken = default)
    {
        var adapter = GetLanguageAdapter(filePath, content);
        var chunker = GetChunker();

        _logger.LogInformation("Analyzing {File} for chunking...", filePath);

        // Identify semantic units
        var semanticUnits = await adapter.IdentifySemanticUnitsAsync(content, filePath, cancellationToken);
        _logger.LogInformation("Identified {Count} semantic units in {File}", semanticUnits.Count, filePath);

        // Extract variables
        var variables = await adapter.ExtractVariablesAsync(content, cancellationToken);
        _logger.LogInformation("Found {Count} variables in {File}", variables.Count, filePath);

        // Extract dependencies
        var dependencies = await adapter.ExtractCallDependenciesAsync(content, cancellationToken);
        _logger.LogInformation("Found {Count} call dependencies in {File}", dependencies.Count, filePath);

        // Extract external references
        var externalRefs = await adapter.ExtractExternalReferencesAsync(content, cancellationToken);
        _logger.LogInformation("Found {Count} external references in {File}", externalRefs.Count, filePath);

        // Create chunks
        var chunks = await chunker.ChunkFileAsync(content, filePath, semanticUnits, _chunkingSettings, cancellationToken);
        _logger.LogInformation("Created {Count} chunks for {File}", chunks.Count, filePath);

        var plan = new ChunkingPlan
        {
            SourceFile = filePath,
            TotalLines = content.Split('\n').Length,
            TotalSemanticUnits = semanticUnits.Count,
            TotalVariables = variables.Count,
            TotalDependencies = dependencies.Count,
            TotalExternalReferences = externalRefs.Count,
            ChunkCount = chunks.Count,
            Chunks = chunks.ToList(),
            SemanticUnits = semanticUnits.ToList(),
            Variables = variables.ToList(),
            CallDependencies = dependencies.ToList(),
            ExternalReferences = externalRefs.ToList(),
            EstimatedTotalTokens = chunks.Sum(c => c.EstimatedTokens),
            AnalyzedAt = DateTime.UtcNow
        };

        // Validate plan
        ValidatePlan(plan);

        return plan;
    }

    /// <summary>
    /// Processes chunks for conversion, calling the conversion callback for each chunk.
    /// Supports parallel processing based on ChunkingSettings.EnableParallelProcessing.
    /// </summary>
    public async Task<ChunkingResult> ProcessChunksAsync(
        int runId,
        ChunkingPlan plan,
        Func<ChunkResult, ChunkContext, Task<ChunkConversionResult>> convertChunkAsync,
        CancellationToken cancellationToken = default)
    {
        var result = new ChunkingResult
        {
            RunId = runId,
            SourceFile = plan.SourceFile,
            TotalChunks = plan.ChunkCount
        };

        var stopwatch = System.Diagnostics.Stopwatch.StartNew();
        var contextManager = GetContextManager();
        var validator = GetValidator();

        // Check for resumability
        int startChunk = 0;
        if (_chunkingSettings.EnableResumability)
        {
            startChunk = await contextManager.GetLastCompletedChunkIndexAsync(
                runId, plan.SourceFile, cancellationToken) + 1;

            if (startChunk > 0)
            {
                _logger.LogInformation("Resuming from chunk {Index} for {File}", startChunk, plan.SourceFile);
            }
        }

        // Pre-register type mappings from analysis
        var typeMappingTable = GetTypeMappingTable();
        var adapter = GetLanguageAdapter(plan.SourceFile, null);

        foreach (var variable in plan.Variables)
        {
            var targetType = typeMappingTable.InferTargetType(
                variable.LegacyType,
                _targetLanguage);

            var mapping = new TypeMapping
            {
                LegacyVariable = variable.LegacyName,
                LegacyType = variable.LegacyType,
                TargetType = targetType,
                TargetFieldName = adapter.ConvertNameDeterministic(variable.LegacyName, NameType.Variable),
                IsNullable = !variable.IsGroup,
                DefaultValue = variable.InitialValue
            };

            await typeMappingTable.RegisterMappingAsync(runId, plan.SourceFile, mapping, cancellationToken);
        }

        // Determine processing mode
        var chunksToProcess = plan.Chunks.Skip(startChunk).ToList();
        
        if (_chunkingSettings.EnableParallelProcessing && chunksToProcess.Count > 1)
        {
            _logger.LogInformation(
                "Starting parallel chunk processing: {Count} chunks with max {Parallel} concurrent workers",
                chunksToProcess.Count, _chunkingSettings.MaxParallelChunks);
            
            await ProcessChunksParallelAsync(
                runId, plan, chunksToProcess, startChunk, contextManager, validator, 
                convertChunkAsync, result, cancellationToken);
        }
        else
        {
            _logger.LogInformation(
                "Starting sequential chunk processing: {Count} chunks",
                chunksToProcess.Count);
            
            await ProcessChunksSequentialAsync(
                runId, plan, chunksToProcess, startChunk, contextManager, validator,
                convertChunkAsync, result, cancellationToken);
        }

        // Run reconciliation pass
        if (_conversionSettings.EnableReconciliationPass && result.SuccessfulChunks > 0)
        {
            _logger.LogInformation("Running reconciliation pass for {File}...", plan.SourceFile);

            var history = await contextManager.GetFileHistoryAsync(runId, plan.SourceFile, cancellationToken);
            result.ReconciliationReport = await validator.ReconcileFileAsync(
                runId, plan.SourceFile, history, cancellationToken);

            result.IsConsistent = result.ReconciliationReport.IsValid;
        }
        else
        {
            result.IsConsistent = result.FailedChunks == 0;
        }

        stopwatch.Stop();
        result.TotalProcessingTimeMs = stopwatch.ElapsedMilliseconds;
        result.CompletedAt = DateTime.UtcNow;

        _logger.LogInformation(
            "Completed processing {File}: {Success}/{Total} chunks successful, consistent: {Consistent}, time: {Time}ms",
            plan.SourceFile, result.SuccessfulChunks, result.TotalChunks, result.IsConsistent, result.TotalProcessingTimeMs);

        return result;
    }

    /// <summary>
    /// Processes chunks sequentially (original behavior).
    /// </summary>
    private async Task ProcessChunksSequentialAsync(
        int runId,
        ChunkingPlan plan,
        List<ChunkResult> chunksToProcess,
        int startIndex,
        IContextManager contextManager,
        ConversionValidator validator,
        Func<ChunkResult, ChunkContext, Task<ChunkConversionResult>> convertChunkAsync,
        ChunkingResult result,
        CancellationToken cancellationToken)
    {
        for (int i = 0; i < chunksToProcess.Count; i++)
        {
            cancellationToken.ThrowIfCancellationRequested();

            var chunkIndex = startIndex + i;
            var chunk = chunksToProcess[i];
            
            var processingResult = await ProcessSingleChunkAsync(
                runId, plan.SourceFile, chunkIndex, plan.ChunkCount, chunk,
                contextManager, validator, convertChunkAsync, cancellationToken);
            
            lock (result)
            {
                result.ChunkResults.Add(processingResult);
                if (processingResult.Success)
                    result.SuccessfulChunks++;
                else
                    result.FailedChunks++;
            }

            if (!processingResult.Success && !_chunkingSettings.EnableResumability)
            {
                throw new InvalidOperationException($"Chunk {chunkIndex} failed and resumability is disabled");
            }
        }
    }

    /// <summary>
    /// Processes chunks in parallel with rate limiting.
    /// </summary>
    private async Task ProcessChunksParallelAsync(
        int runId,
        ChunkingPlan plan,
        List<ChunkResult> chunksToProcess,
        int startIndex,
        IContextManager contextManager,
        ConversionValidator validator,
        Func<ChunkResult, ChunkContext, Task<ChunkConversionResult>> convertChunkAsync,
        ChunkingResult result,
        CancellationToken cancellationToken)
    {
        var maxParallel = Math.Min(_chunkingSettings.MaxParallelChunks, chunksToProcess.Count);
        var semaphore = new SemaphoreSlim(maxParallel, maxParallel);
        var staggerDelay = _chunkingSettings.ParallelStaggerDelayMs;
        var processingLock = new object();
        var completedCount = 0;

        _logger.LogInformation(
            "Parallel processing: {Total} chunks with {Parallel} workers, {Stagger}ms stagger delay",
            chunksToProcess.Count, maxParallel, staggerDelay);

        // Create tasks for all chunks
        var tasks = new List<Task>();
        
        for (int i = 0; i < chunksToProcess.Count; i++)
        {
            var localIndex = i;
            var chunkIndex = startIndex + i;
            var chunk = chunksToProcess[i];

            // Stagger task starts to avoid burst requests
            if (i > 0 && staggerDelay > 0)
            {
                await Task.Delay(Math.Min(staggerDelay, 500), cancellationToken);
            }

            var task = Task.Run(async () =>
            {
                await semaphore.WaitAsync(cancellationToken);
                try
                {
                    _logger.LogInformation(
                        "🚀 Starting chunk {Index}/{Total} (worker slot acquired)",
                        chunkIndex + 1, plan.ChunkCount);

                    var processingResult = await ProcessSingleChunkAsync(
                        runId, plan.SourceFile, chunkIndex, plan.ChunkCount, chunk,
                        contextManager, validator, convertChunkAsync, cancellationToken);

                    lock (processingLock)
                    {
                        result.ChunkResults.Add(processingResult);
                        if (processingResult.Success)
                            result.SuccessfulChunks++;
                        else
                            result.FailedChunks++;
                        
                        completedCount++;
                        
                        // Progress update
                        var percent = (completedCount * 100) / chunksToProcess.Count;
                        _logger.LogInformation(
                            "📊 Progress: {Completed}/{Total} chunks ({Percent}%) - Success: {Success}, Failed: {Failed}",
                            completedCount, chunksToProcess.Count, percent, 
                            result.SuccessfulChunks, result.FailedChunks);
                    }

                    if (!processingResult.Success && !_chunkingSettings.EnableResumability)
                    {
                        _logger.LogError("Chunk {Index} failed and resumability is disabled", chunkIndex);
                    }
                }
                finally
                {
                    semaphore.Release();
                }
            }, cancellationToken);

            tasks.Add(task);
        }

        // Wait for all tasks to complete
        await Task.WhenAll(tasks);

        _logger.LogInformation(
            "✅ Parallel processing complete: {Success}/{Total} chunks successful",
            result.SuccessfulChunks, chunksToProcess.Count);
    }

    /// <summary>
    /// Processes a single chunk with context building, conversion, and validation.
    /// </summary>
    private async Task<ChunkProcessingResult> ProcessSingleChunkAsync(
        int runId,
        string sourceFile,
        int chunkIndex,
        int totalChunks,
        ChunkResult chunk,
        IContextManager contextManager,
        ConversionValidator validator,
        Func<ChunkResult, ChunkContext, Task<ChunkConversionResult>> convertChunkAsync,
        CancellationToken cancellationToken)
    {
        var stopwatch = System.Diagnostics.Stopwatch.StartNew();
        
        _logger.LogInformation(
            "Processing chunk {Index}/{Total} for {File} (lines {Start}-{End})",
            chunkIndex + 1, totalChunks, sourceFile, chunk.StartLine, chunk.EndLine);

        try
        {
            // Build context for this chunk
            var context = await contextManager.BuildContextForChunkAsync(runId, chunk, cancellationToken);

            // Call the conversion callback
            var conversionResult = await convertChunkAsync(chunk, context);

            // Record the result
            await contextManager.RecordChunkResultAsync(runId, chunk, conversionResult, cancellationToken);

            // Validate the chunk
            var validation = await validator.ValidateChunkAsync(
                runId, sourceFile, chunkIndex, conversionResult, cancellationToken);

            stopwatch.Stop();
            
            if (conversionResult.Success)
            {
                _logger.LogInformation(
                    "✅ Chunk {Index}/{Total} completed successfully in {Time}ms",
                    chunkIndex + 1, totalChunks, stopwatch.ElapsedMilliseconds);
            }
            else
            {
                _logger.LogWarning(
                    "⚠️ Chunk {Index}/{Total} failed in {Time}ms: {Error}",
                    chunkIndex + 1, totalChunks, stopwatch.ElapsedMilliseconds, conversionResult.ErrorMessage);
            }

            return new ChunkProcessingResult
            {
                ChunkIndex = chunkIndex,
                Success = conversionResult.Success,
                ConversionResult = conversionResult,
                ValidationReport = validation
            };
        }
        catch (Exception ex)
        {
            stopwatch.Stop();
            _logger.LogError(ex, 
                "❌ Error processing chunk {Index}/{Total} for {File} in {Time}ms", 
                chunkIndex + 1, totalChunks, sourceFile, stopwatch.ElapsedMilliseconds);

            return new ChunkProcessingResult
            {
                ChunkIndex = chunkIndex,
                Success = false,
                ConversionResult = new ChunkConversionResult
                {
                    Success = false,
                    ErrorMessage = ex.Message
                }
            };
        }
    }

    private ILanguageAdapter GetLanguageAdapter(string filePath, string? content)
    {
        // For now, always return COBOL adapter
        // In the future, detect language and return appropriate adapter
        _languageAdapter ??= new CobolAdapter();
        return _languageAdapter;
    }

    private IChunker GetChunker()
    {
        _chunker ??= new SemanticUnitChunker();
        return _chunker;
    }

    private ISignatureRegistry GetSignatureRegistry()
    {
        if (_signatureRegistry == null)
        {
            var loggerFactory = LoggerFactory.Create(builder => builder.AddConsole());
            _signatureRegistry = new SignatureRegistry(
                _databasePath,
                loggerFactory.CreateLogger<SignatureRegistry>());
        }
        return _signatureRegistry;
    }

    private ITypeMappingTable GetTypeMappingTable()
    {
        if (_typeMappingTable == null)
        {
            var loggerFactory = LoggerFactory.Create(builder => builder.AddConsole());
            _typeMappingTable = new TypeMappingTable(
                _databasePath,
                loggerFactory.CreateLogger<TypeMappingTable>());
        }
        return _typeMappingTable;
    }

    private IContextManager GetContextManager()
    {
        if (_contextManager == null)
        {
            var loggerFactory = LoggerFactory.Create(builder => builder.AddConsole());
            _contextManager = new ChunkContextManager(
                _databasePath,
                GetSignatureRegistry(),
                GetTypeMappingTable(),
                _chunkingSettings,
                loggerFactory.CreateLogger<ChunkContextManager>());
        }
        return _contextManager;
    }

    private ConversionValidator GetValidator()
    {
        if (_validator == null)
        {
            var loggerFactory = LoggerFactory.Create(builder => builder.AddConsole());
            _validator = new ConversionValidator(
                GetSignatureRegistry(),
                GetTypeMappingTable(),
                _conversionSettings,
                loggerFactory.CreateLogger<ConversionValidator>());
        }
        return _validator;
    }

    private NamingConventionEnforcer GetNamingEnforcer()
    {
        _namingEnforcer ??= new NamingConventionEnforcer(_conversionSettings);
        return _namingEnforcer;
    }

    private void ValidatePlan(ChunkingPlan plan)
    {
        if (plan.TotalLines > _chunkingSettings.MaxLinesOfCode)
        {
            throw new InvalidOperationException(
                $"File has {plan.TotalLines} lines, exceeding maximum of {_chunkingSettings.MaxLinesOfCode}");
        }

        if (plan.ChunkCount == 0)
        {
            throw new InvalidOperationException("No chunks were created from the file");
        }

        _logger.LogDebug(
            "Plan validated: {Lines} lines, {Chunks} chunks, {Tokens} estimated tokens",
            plan.TotalLines, plan.ChunkCount, plan.EstimatedTotalTokens);
    }
}

/// <summary>
/// Plan for chunking a file.
/// </summary>
public class ChunkingPlan
{
    public string SourceFile { get; set; } = string.Empty;
    public int TotalLines { get; set; }
    public int TotalSemanticUnits { get; set; }
    public int TotalVariables { get; set; }
    public int TotalDependencies { get; set; }
    public int TotalExternalReferences { get; set; }
    public int ChunkCount { get; set; }
    public int EstimatedTotalTokens { get; set; }
    public DateTime AnalyzedAt { get; set; }

    public List<ChunkResult> Chunks { get; set; } = new();
    public List<SemanticUnit> SemanticUnits { get; set; } = new();
    public List<VariableDeclaration> Variables { get; set; } = new();
    public List<CallDependency> CallDependencies { get; set; } = new();
    public List<ExternalReference> ExternalReferences { get; set; } = new();
}

/// <summary>
/// Result of chunk processing.
/// </summary>
public class ChunkingResult
{
    public int RunId { get; set; }
    public string SourceFile { get; set; } = string.Empty;
    public int TotalChunks { get; set; }
    public int SuccessfulChunks { get; set; }
    public int FailedChunks { get; set; }
    public bool IsConsistent { get; set; }
    public long TotalProcessingTimeMs { get; set; }
    public DateTime? CompletedAt { get; set; }

    public List<ChunkProcessingResult> ChunkResults { get; set; } = new();
    public ValidationReport? ReconciliationReport { get; set; }

    public double SuccessRate => TotalChunks > 0 ? (SuccessfulChunks * 100.0 / TotalChunks) : 0;
}

/// <summary>
/// Result of processing a single chunk.
/// </summary>
public class ChunkProcessingResult
{
    public int ChunkIndex { get; set; }
    public bool Success { get; set; }
    public ChunkConversionResult ConversionResult { get; set; } = new();
    public ValidationReport? ValidationReport { get; set; }
}
