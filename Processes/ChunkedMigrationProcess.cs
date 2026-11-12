using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Agents;
using CobolToQuarkusMigration.Agents.Infrastructure;
using CobolToQuarkusMigration.Agents.Interfaces;
using CobolToQuarkusMigration.Chunking;
using CobolToQuarkusMigration.Chunking.Interfaces;
using CobolToQuarkusMigration.Chunking.Models;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Jcl.Generation;
using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Persistence;
using System.Text;

namespace CobolToQuarkusMigration.Processes;

/// <summary>
/// Enhanced migration process with smart chunking support for large files.
/// Uses Microsoft Agent Framework (IChatClient) for AI operations.
/// </summary>
public class ChunkedMigrationProcess
{
    private readonly IChatClient _chatClient;
    private readonly ResponsesApiClient? _responsesApiClient;
    private readonly ILogger<ChunkedMigrationProcess> _logger;
    private readonly FileHelper _fileHelper;
    private readonly AppSettings _settings;
    private readonly EnhancedLogger _enhancedLogger;
    private readonly ChatLogger _chatLogger;
    private readonly IMigrationRepository _migrationRepository;
    private readonly MigrationExecutionOptions _execution;

    private ChunkingOrchestrator? _chunkingOrchestrator;
    private IChunkAwareConverter? _chunkAwareConverter;
    private CompileRepairAgent? _compileRepairAgent;
    private CompileGateResult? _compileGate;
    private ParityRepairAgent? _parityRepairAgent;
    private GeneratedOutputValidationResult? _validation;
    private string? _sourceRoot;
    private CallTargetRegistry SourceCallTargets => CallTargetRegistryHolder.GetOrBuild(
        _execution.RepositoryRoot ?? Directory.GetCurrentDirectory(), _sourceRoot!);
    private ICobolAnalyzerAgent? _cobolAnalyzerAgent;
    private IDependencyMapperAgent? _dependencyMapperAgent;

    /// <summary>
    /// Initializes a new instance of the ChunkedMigrationProcess class.
    /// </summary>
    public ChunkedMigrationProcess(
        IChatClient chatClient,
        ResponsesApiClient? responsesApiClient,
        ILogger<ChunkedMigrationProcess> logger,
        FileHelper fileHelper,
        AppSettings settings,
        IMigrationRepository migrationRepository,
        MigrationExecutionOptions? execution = null,
        ChatLogger? chatLogger = null)
    {
        _chatClient = chatClient;
        _responsesApiClient = responsesApiClient;
        _logger = logger;
        _fileHelper = fileHelper;
        _settings = settings;
        _execution = execution ?? new();
        _enhancedLogger = new EnhancedLogger(logger);
        var providerName = chatClient is Agents.Infrastructure.CopilotChatClient ? "GitHub Copilot" : "Azure OpenAI";
        _chatLogger = chatLogger ?? new ChatLogger(LoggerFactory.Create(builder => builder.AddConsole()).CreateLogger<ChatLogger>(),
            logDirectory: _execution.LogDirectory, providerName: providerName);
        _migrationRepository = migrationRepository;
    }

    /// <summary>
    /// Initializes agents with chunking support.
    /// </summary>
    public void InitializeAgents()
    {
        _enhancedLogger.ShowSectionHeader("INITIALIZING CHUNKED MIGRATION AGENTS", 
            "Setting up AI agents with smart chunking support");

        var loggerFactory = LoggerFactory.Create(builder => builder.AddConsole());

        // Initialize chunking orchestrator
        _enhancedLogger.ShowStep(1, 4, "ChunkingOrchestrator", "Smart file chunking engine");
        
        // Use SQLite database path from settings (consistent with SqliteMigrationRepository)
        var databasePath = _settings.ApplicationSettings.MigrationDatabasePath;
        var targetLang = _settings.ApplicationSettings.TargetLanguage;
        
        _chunkingOrchestrator = new ChunkingOrchestrator(
            _settings.ChunkingSettings,
            _settings.ConversionSettings,
            databasePath,
            loggerFactory.CreateLogger<ChunkingOrchestrator>(),
            targetLang);

        // Initialize chunk-aware converter
        _enhancedLogger.ShowStep(2, 4, "ChunkAwareConverter", "Consistency-aware code conversion");
        
        if (targetLang == TargetLanguage.CSharp)
        {
            _chunkAwareConverter = ChunkAwareCSharpConverter.Create(
                _responsesApiClient, _chatClient,
                loggerFactory.CreateLogger<ChunkAwareCSharpConverter>(),
                _settings.AISettings.ResolveJavaConverterModelId(),
                _settings.ConversionSettings,
                _enhancedLogger, _chatLogger, settings: _settings);
            _compileRepairAgent = CompileRepairAgent.Create(
                _responsesApiClient, _chatClient,
                loggerFactory.CreateLogger<CompileRepairAgent>(),
                _settings.AISettings.ResolveJavaConverterModelId(),
                _enhancedLogger, _chatLogger, _settings);
        }
        else
        {
            _chunkAwareConverter = ChunkAwareJavaConverter.Create(
                _responsesApiClient, _chatClient,
                loggerFactory.CreateLogger<ChunkAwareJavaConverter>(),
                _settings.AISettings.ResolveJavaConverterModelId(),
                _settings.ConversionSettings,
                _enhancedLogger, _chatLogger, settings: _settings);
        }

        _parityRepairAgent = ParityRepairAgent.Create(_responsesApiClient, _chatClient,
            loggerFactory.CreateLogger<ParityRepairAgent>(), _settings.AISettings.ResolveJavaConverterModelId(), _settings);

        // Initialize standard agents
        _enhancedLogger.ShowStep(3, 4, "CobolAnalyzerAgent", "COBOL structure analysis");
        _cobolAnalyzerAgent = CobolAnalyzerAgent.Create(
            _responsesApiClient, _chatClient,
            loggerFactory.CreateLogger<CobolAnalyzerAgent>(),
            _settings.AISettings.ResolveCobolAnalyzerModelId(),
            _enhancedLogger, _chatLogger, settings: _settings);

        _enhancedLogger.ShowStep(4, 4, "DependencyMapperAgent", "Cross-file dependency mapping");
        _dependencyMapperAgent = DependencyMapperAgent.Create(
            _responsesApiClient, _chatClient,
            loggerFactory.CreateLogger<DependencyMapperAgent>(),
            _settings.AISettings.ResolveDependencyMapperModelId(),
            _enhancedLogger, _chatLogger, settings: _settings);

        _enhancedLogger.ShowSuccess("All chunked migration agents initialized");
    }

    /// <summary>
    /// Sets the business logic context extracted during reverse engineering so that
    /// the chunk-aware converter can inject it into its conversion prompts.
    /// Must be called after <see cref="InitializeAgents"/>.
    /// </summary>
    /// <param name="businessLogicExtracts">Per-file business logic from reverse engineering.</param>
    public void SetDependencyMap(DependencyMap dependencyMap)
    {
        _existingDependencyMap = dependencyMap;
    }

    private DependencyMap? _existingDependencyMap;

    public void SetBusinessLogicContext(List<BusinessLogic> businessLogicExtracts)
    {
        _chunkAwareConverter?.SetBusinessLogicContext(businessLogicExtracts);
    }

    /// <summary>
    /// Runs the chunked migration process.
    /// </summary>
    public async Task RunAsync(
        string cobolSourceFolder,
        string outputFolder,
        Action<string, int, int, double?>? progressCallback = null,
        int? existingRunId = null,
        CancellationToken cancellationToken = default,
        string? runType = null)
    {
        _validation = null;
        _compileGate = null;
        var targetName = _settings.ApplicationSettings.TargetLanguage == TargetLanguage.CSharp 
            ? "C# .NET" : JavaGeneration.TargetName(_settings.ConversionSettings);
        
        _enhancedLogger.ShowSectionHeader(
            $"CHUNKED COBOL TO {targetName} MIGRATION",
            $"Smart chunking enabled (max {_settings.ChunkingSettings.MaxLinesPerChunk} lines/chunk)");

        var startTime = DateTime.UtcNow;
        var runId = existingRunId ?? await _migrationRepository.StartRunAsync(cobolSourceFolder, outputFolder);
        using var attempt = AiLoopEvents.BeginRun(runId, existingRunId.HasValue ? "resume" : "chunked",
            targetName, outputFolder, cancellationToken, reuseExistingAttempt: true);

        try
        {
            cancellationToken.ThrowIfCancellationRequested();
            _sourceRoot = Path.GetFullPath(cobolSourceFolder);
            if (_chunkingOrchestrator == null || _chunkAwareConverter == null)
                throw new InvalidOperationException("Agents not initialized. Call InitializeAgents() first.");
            // Pass run ID to converter for spec lookup
            _chunkAwareConverter.SetRunId(runId);
            var repositoryRoot = _execution.RepositoryRoot
                ?? Environment.GetEnvironmentVariable("REPO_ROOT") ?? Directory.GetCurrentDirectory();
            foreach (var agent in new AgentBase?[]
                     {
                         _chunkAwareConverter as AgentBase, _cobolAnalyzerAgent as AgentBase,
                         _dependencyMapperAgent as AgentBase, _compileRepairAgent, _parityRepairAgent
                     })
            {
                if (agent is null) continue;
                agent.SourceDirectory = _sourceRoot;
                agent.RepositoryRoot = repositoryRoot;
            }

            // Show initial dashboard
            _enhancedLogger.ShowDashboardSummary(runId, targetName, "Running", "1/6: File Discovery", 5);

            // Step 1: Discover files
            _enhancedLogger.ShowStep(1, 6, "File Discovery", "Scanning for COBOL programs");
            progressCallback?.Invoke("Scanning for COBOL files", 1, 6, null);

            var cobolFiles = await _fileHelper.ScanDirectoryForCobolFilesAsync(cobolSourceFolder);
            cancellationToken.ThrowIfCancellationRequested();
            PromptLoader.CodebaseProfile = PromptLoader.GenerateCodebaseProfile(cobolFiles);
            await _migrationRepository.SaveCobolFilesAsync(runId, cobolFiles);

            if (cobolFiles.Count == 0)
            {
                _enhancedLogger.ShowWarning("No COBOL files found");
                await _migrationRepository.CompleteRunAsync(runId, "NoFiles");
                AiLoopEvents.Finished(runId, "no_files", startTime);
                return;
            }

            _enhancedLogger.ShowSuccess($"Found {cobolFiles.Count} COBOL files");

            // Categorize files by size
            var (smallFiles, largeFiles) = CategorizeBySize(cobolFiles);
            _logger.LogInformation(
                "File categorization: {Small} small files, {Large} large files requiring chunking",
                smallFiles.Count, largeFiles.Count);

            // Step 2: Dependency analysis (reused from RE when available)
            _enhancedLogger.ShowDashboardSummary(runId, targetName, "Running", "2/6: Dependency Analysis", 15);
            _enhancedLogger.ShowStep(2, 6, "Dependency Analysis", "Mapping file relationships");
            progressCallback?.Invoke("Analyzing dependencies", 2, 6, null);

            cobolFiles = ConversionEligibility.Select(cobolFiles, _sourceRoot,
                ConversionEligibility.FactsDirectory(repositoryRoot), outputFolder,
                _settings.ConversionSettings, _existingDependencyMap);
            if (ConversionEligibility.IsDraft(_settings.ConversionSettings))
            {
                _existingDependencyMap = null;
                (smallFiles, largeFiles) = CategorizeBySize(cobolFiles);
                PromptLoader.CodebaseProfile = PromptLoader.GenerateCodebaseProfile(cobolFiles);
            }

            DependencyMap dependencyMap;
            if (_existingDependencyMap != null)
            {
                dependencyMap = _existingDependencyMap;
                dependencyMap.MermaidDiagram = await _dependencyMapperAgent!.GenerateMermaidDiagramAsync(
                    dependencyMap, enableAiGeneration: false);
                _enhancedLogger.ShowSuccess($"Reusing dependency map from reverse engineering ({dependencyMap.Dependencies.Count} relationships)");
            }
            else
            {
                dependencyMap = await _dependencyMapperAgent!.AnalyzeDependenciesAsync(
                    cobolFiles, new List<CobolAnalysis>());
                _enhancedLogger.ShowSuccess($"Dependency analysis complete - {dependencyMap.Dependencies.Count} relationships found");
            }
            await _fileHelper.SaveDependencyOutputsAsync(dependencyMap, outputFolder);
            await _migrationRepository.SaveDependencyMapAsync(runId, dependencyMap);

            // Determine processing order based on dependencies
            var processingOrder = DetermineProcessingOrder(cobolFiles, dependencyMap);
            _logger.LogInformation("Processing order determined: {Count} files", processingOrder.Count);

            // Step 3: Process small files (standard conversion)
            _enhancedLogger.ShowDashboardSummary(runId, targetName, "Running", "3/6: Small File Conversion", 30);
            _enhancedLogger.ShowStep(3, 6, "Small File Conversion", 
                $"Converting {smallFiles.Count} small files");
            
            var smallFileResults = new List<CodeFile>();
            if (smallFiles.Count > 0)
            {
                smallFileResults = await ProcessSmallFilesAsync(
                    runId, smallFiles, progressCallback, cancellationToken);
            }

            // Step 4: Process large files (chunked conversion) - in parallel
            _enhancedLogger.ShowDashboardSummary(runId, targetName, "Running", "4/6: Large File Chunking", 50);
            _enhancedLogger.ShowStep(4, 6, "Large File Chunking", 
                $"Processing {largeFiles.Count} large files with smart chunking");

            var largeFileResults = new List<ChunkedFileResult>();
            
            if (largeFiles.Count > 0)
            {
                var maxParallel = Math.Min(_settings.ChunkingSettings.MaxParallelAnalysis, largeFiles.Count);
                var enableParallel = _settings.ChunkingSettings.EnableParallelProcessing;

                if (enableParallel && largeFiles.Count > 1 && maxParallel > 1)
                {
                    _logger.LogInformation("🚀 Processing {Count} large files in parallel with {Workers} workers",
                        largeFiles.Count, maxParallel);

                    using var semaphore = new SemaphoreSlim(maxParallel, maxParallel);
                    var processedCount = 0;
                    var lockObj = new object();

                    // Use indexed tuples to preserve order
                    var indexedTasks = new List<Task<(int Index, ChunkedFileResult Result)>>();

                    for (int i = 0; i < largeFiles.Count; i++)
                    {
                        var file = largeFiles[i];
                        var index = i;

                        var task = Task.Run(async () =>
                        {
                            await semaphore.WaitAsync(cancellationToken);
                            try
                            {
                                var result = await ProcessLargeFileAsync(runId, file, cancellationToken);

                                lock (lockObj)
                                {
                                    processedCount++;
                                    var progress = (double)processedCount / largeFiles.Count * 100;
                                    progressCallback?.Invoke(
                                        $"Chunked {file.FileName} ({processedCount}/{largeFiles.Count})",
                                        4, 6, progress);
                                }

                                return (Index: index, Result: result);
                            }
                            finally
                            {
                                semaphore.Release();
                            }
                        }, cancellationToken);

                        indexedTasks.Add(task);
                    }

                    var results = await Task.WhenAll(indexedTasks);

                    // Sort by original index to preserve order
                    largeFileResults = results.OrderBy(r => r.Index).Select(r => r.Result).ToList();

                    _logger.LogInformation("✅ Completed parallel processing of {Count} large files (order preserved)",
                        largeFiles.Count);
                }
                else
                {
                    // Sequential processing
                    for (int i = 0; i < largeFiles.Count; i++)
                    {
                        var file = largeFiles[i];
                        var progress = (double)(i + 1) / largeFiles.Count * 100;
                        progressCallback?.Invoke(
                            $"Chunking {file.FileName} ({i + 1}/{largeFiles.Count})",
                            4, 6, progress);

                        var result = await ProcessLargeFileAsync(runId, file, cancellationToken);
                        largeFileResults.Add(result);
                    }
                }
            }

            // Step 5: Assembly & validation
            _enhancedLogger.ShowDashboardSummary(runId, targetName, "Running", "5/6: Assembly & Validation", 80);
            _enhancedLogger.ShowStep(5, 6, "Assembly & Validation", 
                "Combining chunks and validating consistency");
            progressCallback?.Invoke("Assembling converted code", 5, 6, null);

            var assembledFiles = AssembleChunkedFiles(largeFileResults);
            await SaveAssemblyDiagnosticsAsync(largeFileResults, outputFolder);

            // Step 6: Output generation
            _enhancedLogger.ShowDashboardSummary(runId, targetName, "Running", "6/6: Output Generation", 95);
            _enhancedLogger.ShowStep(6, 6, "Output Generation", "Writing converted files");
            progressCallback?.Invoke("Saving output files", 6, 6, null);

            var allFiles = smallFileResults.Concat(assembledFiles).ToList();
            await SaveOutputFilesAsync(allFiles, outputFolder);
            if (!ConversionEligibility.IsDraft(_settings.ConversionSettings))
                JclJobGeneration.Run(_settings.JclJobs, cobolSourceFolder, outputFolder, _settings.ApplicationSettings.TargetLanguage, _logger);

            if (_settings.ApplicationSettings.TargetLanguage == TargetLanguage.CSharp)
            {
                try
                {
                    var scaffold = GeneratedProjectScaffold.Write(
                        outputFolder, ConversionNamespacePolicy.Root("C#"),
                        callTargets: SourceCallTargets,
                        sharedNamespace: ConversionNamespacePolicy.ForSharedTypes("C#"));

                    if (scaffold.WroteAnything)
                    {
                        _logger.LogInformation(
                            "Wrote build scaffolding for the generated output: {Usings} global using(s), {Packages} package reference(s), {Normalized} file(s) rewritten to block-scoped namespaces",
                            scaffold.Usings.Count, scaffold.Packages.Count, scaffold.Normalized.Count);
                    }
                }
                catch (IOException ex)
                {
                    _logger.LogWarning("Could not write build scaffolding: {Message}", ex.Message);
                }

                if (_settings.CompileGate.Enabled && largeFileResults.All(result => result.Success))
                {
                    progressCallback?.Invoke("Compiling generated code", 6, 6, null);
                    _compileGate = await CSharpCompileGate.RunAsync(
                        outputFolder,
                        ConversionNamespacePolicy.Root("C#"),
                        ConversionNamespacePolicy.ForSharedTypes("C#"),
                        _compileRepairAgent,
                        _settings.CompileGate,
                        _logger,
                        callTargets: SourceCallTargets,
                        cancellationToken: cancellationToken,
                        generatedFiles: allFiles,
                        sourceFiles: cobolFiles,
                        sourceDirectory: cobolSourceFolder);
                }
            }

            // Generate reports
            await GenerateChunkedMigrationReportAsync(
                cobolFiles, allFiles, largeFileResults, dependencyMap, outputFolder, startTime, cancellationToken);

            // Save full chat log for traceability
            await _chatLogger.SaveChatLogAsync();
            await _chatLogger.SaveChatLogJsonAsync();
            _logger.LogInformation("Full chat logs saved to Logs folder");

            EnsureAssemblyComplete(largeFileResults);

            if (_validation is { Accepted: false })
            {
                const string failure = "Generated output failed validation; see the migration report.";
                _enhancedLogger.ShowWarning(failure);
                _enhancedLogger.ShowDashboardSummary(runId, targetName, "Failed", "Validation failed", 100);
                await _migrationRepository.CompleteRunAsync(runId, "Failed", failure);
                AiLoopEvents.Finished(runId, "failed", startTime, failure, "ValidationFailed");
                progressCallback?.Invoke(failure, 6, 6, 100);
                return;
            }
            _enhancedLogger.ShowDashboardSummary(runId, targetName,
                ConversionEligibility.IsDraft(_settings.ConversionSettings) ? "Partial — NOT VERIFIED" : "Completed", "Done", 100);
            var terminalStatus = ConversionEligibility.IsDraft(_settings.ConversionSettings) ? "Partial" : "Completed";
            await _migrationRepository.CompleteRunAsync(runId, terminalStatus,
                terminalStatus == "Partial" ? "Draft subset; not verified. See draft-conversion-plan.json." : runType);
            AiLoopEvents.Finished(runId, terminalStatus.ToLowerInvariant(), startTime);

            _enhancedLogger.ShowSuccess(
                $"Chunked migration completed: {allFiles.Count} files generated");

            progressCallback?.Invoke(terminalStatus == "Partial" ? "Partial draft — not verified" : "Migration completed successfully", 6, 6, 100);
        }
        catch (Exception ex)
        {
            if (AiLoopEvents.IsFinished) throw;
            AiLoopEvents.Finished(runId, ex is OperationCanceledException ? "cancelled" : "failed", startTime,
                ex.GetType().Name, ex.GetType().Name);
            _enhancedLogger.ShowError($"Migration failed: {ex.Message}", ex);
            try
            {
                await _migrationRepository.CompleteRunAsync(runId,
                    ex is OperationCanceledException ? "Cancelled" : "Failed", ex.GetType().Name);
            }
            catch (Exception persistenceError)
            {
                _logger.LogWarning(persistenceError, "Failed to finalize migration run {RunId}", runId);
            }
            throw;
        }
    }

    private (List<CobolFile> small, List<CobolFile> large) CategorizeBySize(List<CobolFile> files)
    {
        var threshold = _settings.ChunkingSettings.MaxLinesPerChunk;
        
        var small = new List<CobolFile>();
        var large = new List<CobolFile>();

        foreach (var file in files)
        {
            var lineCount = file.Content.Split('\n').Length;
            if (lineCount > threshold)
            {
                large.Add(file);
                _logger.LogInformation(
                    "Large file detected: {File} ({Lines} lines) - will use chunking",
                    file.FileName, lineCount);
            }
            else
            {
                small.Add(file);
            }
        }

        return (small, large);
    }

    private List<CobolFile> DetermineProcessingOrder(
        List<CobolFile> files, 
        DependencyMap dependencyMap)
    {
        // Sort by dependency depth - process dependencies first
        var processed = new HashSet<string>();
        var ordered = new List<CobolFile>();

        while (ordered.Count < files.Count)
        {
            foreach (var file in files)
            {
                if (processed.Contains(file.FileName))
                    continue;

                // Check if all dependencies are processed
                // Use SourceFile from DependencyRelationship
                var deps = dependencyMap.Dependencies
                    .Where(d => d.SourceFile == file.FileName)
                    .Select(d => d.TargetFile)
                    .ToList();

                var allDepsProcessed = deps.All(d => 
                    processed.Contains(d) || !files.Any(f => f.FileName == d));

                if (allDepsProcessed)
                {
                    ordered.Add(file);
                    processed.Add(file.FileName);
                }
            }

            // Break circular dependencies by adding remaining files
            if (ordered.Count < files.Count && 
                ordered.Count == processed.Count)
            {
                var remaining = files.Where(f => !processed.Contains(f.FileName)).First();
                ordered.Add(remaining);
                processed.Add(remaining.FileName);
            }
        }

        return ordered;
    }

    private async Task<List<CodeFile>> ProcessSmallFilesAsync(
        int runId,
        List<CobolFile> files,
        Action<string, int, int, double?>? progressCallback,
        CancellationToken cancellationToken)
    {
        var results = new List<CodeFile>();
        
        // Use standard analysis and conversion for small files
        var analyses = await _cobolAnalyzerAgent!.AnalyzeCobolFilesAsync(files);
        await _migrationRepository.SaveAnalysesAsync(runId, analyses);

        for (int i = 0; i < files.Count; i++)
        {
            cancellationToken.ThrowIfCancellationRequested();

            var file = files[i];
            var analysis = analyses[i];

            progressCallback?.Invoke(
                $"Converting {file.FileName} ({i + 1}/{files.Count})",
                3, 6, (double)(i + 1) / files.Count * 100);

            // Create a simple chunk for small files
            var chunk = new ChunkResult
            {
                ChunkIndex = 0,
                TotalChunks = 1,
                Content = file.Content,
                SourceFile = SourcePathHelper.ForFile(file),
                StartLine = 1,
                EndLine = file.Content.Split('\n').Length
            };

            var context = new ChunkContext
            {
                CurrentChunkIndex = 0,
                TotalChunks = 1
            };

            var conversionResult = await _chunkAwareConverter!.ConvertChunkAsync(
                chunk, context, cancellationToken);

            if (conversionResult.Success)
            {
                results.Add(new CodeFile
                {
                    FileName = GetOutputFileName(file.FileName),
                    Content = conversionResult.ConvertedCode,
                    ClassName = ExtractClassName(conversionResult.ConvertedCode),
                    OriginalCobolFileName = file.FileName,
                    OriginalSourcePath = SourcePathHelper.ForFile(file),
                    TargetLanguage = _chunkAwareConverter.TargetLanguage
                });
            }
            else
            {
                _logger.LogWarning(
                    "Failed to convert {File}: {Error}",
                    file.FileName, conversionResult.ErrorMessage);
            }
        }

        return results;
    }

    private async Task<ChunkedFileResult> ProcessLargeFileAsync(
        int runId,
        CobolFile file,
        CancellationToken cancellationToken)
    {
        _logger.LogInformation("Processing large file with chunking: {File}", file.FileName);

        // Analyze and create chunking plan
        var plan = await _chunkingOrchestrator!.AnalyzeFileAsync(
            file.FilePath, file.Content, cancellationToken);

        _logger.LogInformation(
            "Chunking plan for {File}: {Chunks} chunks, {Lines} lines, {Units} semantic units",
            file.FileName, plan.ChunkCount, plan.TotalLines, plan.TotalSemanticUnits);

        // Process chunks
        var chunkingResult = await _chunkingOrchestrator.ProcessChunksAsync(
            runId,
            plan,
            async (chunk, context) =>
            {
                _logger.LogDebug(
                    "Converting chunk {Index}/{Total} of {File}",
                    chunk.ChunkIndex + 1, plan.ChunkCount, file.FileName);

                return await _chunkAwareConverter!.ConvertChunkAsync(
                    chunk, context, cancellationToken);
            },
            cancellationToken);

        return new ChunkedFileResult
        {
            SourceFile = SourcePathHelper.ForFile(file),
            Plan = plan,
            Result = chunkingResult
        };
    }

    internal List<CodeFile> AssembleChunkedFiles(List<ChunkedFileResult> chunkedResults)
    {
        var results = new List<CodeFile>();
        var isJava = _settings.ApplicationSettings.TargetLanguage == TargetLanguage.Java;

        foreach (var chunkedResult in chunkedResults)
        {
            results.AddRange(ChunkedOutputAssembler.Assemble(chunkedResult, isJava));
            if (!chunkedResult.Success)
                _logger.LogWarning("Assembly rejected {Source}: {Errors}", chunkedResult.SourceFile,
                    string.Join("; ", chunkedResult.AssemblyErrors));
        }

        return results;
    }

    internal static IReadOnlyList<CodeFile> CreateFailedChunkDiagnosticFiles(
        ChunkedFileResult chunkedResult,
        bool isJava)
    {
        var language = isJava ? "Java" : "CSharp";
        var sourceStem = Path.GetFileNameWithoutExtension(chunkedResult.SourceFile);

        return chunkedResult.Result.ChunkResults
            .Where(r => !chunkedResult.Success || !r.Success)
            .OrderBy(r => r.ChunkIndex)
            .Select((r, arrival) => new CodeFile
            {
                FileName = $"{sourceStem}.chunk-{r.ChunkIndex + 1}.arrival-{arrival + 1}.rejected.txt",
                ClassName = $"{sourceStem}Chunk{r.ChunkIndex + 1}Failed",
                Content = r.ConversionResult?.ConvertedCode ?? "",
                OriginalCobolFileName = Path.GetFileName(chunkedResult.SourceFile),
                OriginalSourcePath = chunkedResult.SourceFile,
                TargetLanguage = language
            })
            .ToList();
    }

    internal static async Task SaveAssemblyDiagnosticsAsync(
        IReadOnlyList<ChunkedFileResult> results, string outputFolder)
    {
        for (var i = 0; i < results.Count; i++)
        {
            var result = results[i];
            if (result.Success) continue;
            // Diagnostics never enter the generated-source tree or a compiler input list.
            var directory = Path.Combine(outputFolder, "assembly-diagnostics", $"source-{i + 1}");
            Directory.CreateDirectory(directory);
            await File.WriteAllTextAsync(Path.Combine(directory, "assembly-errors.txt"),
                $"Source: {result.SourceFile}\nExpected chunks: {result.Plan.ChunkCount}\nReceived indices: " +
                string.Join(", ", result.Result.ChunkResults.Select(c => c.ChunkIndex)) + "\n" +
                string.Join("\n", result.AssemblyErrors.Concat(result.Result.ChunkResults
                    .Where(c => !c.Success || c.ConversionResult is not { Success: true })
                    .Select(c => $"Chunk {c.ChunkIndex}: {c.ConversionResult?.ErrorMessage ?? "conversion failed"}"))));
            foreach (var diagnostic in CreateFailedChunkDiagnosticFiles(result, isJava: false))
                await File.WriteAllTextAsync(Path.Combine(directory, diagnostic.FileName), diagnostic.Content);
        }
    }

    internal static void EnsureAssemblyComplete(IReadOnlyList<ChunkedFileResult> results)
    {
        var failures = results.Where(r => !r.Success).Select(r => r.SourceFile).ToList();
        if (failures.Count > 0)
            throw new InvalidDataException($"Chunk assembly incomplete for: {string.Join(", ", failures)}. See assembly-diagnostics; generated output is not a complete migration.");
    }

    private async Task SaveOutputFilesAsync(List<CodeFile> files, string outputFolder)
    {
        var extension = _settings.ApplicationSettings.TargetLanguage == TargetLanguage.CSharp
            ? ".cs" : ".java";

        foreach (var file in files)
        {
            // Recorded so the parity post-pass can re-open what actually shipped.
            file.FilePath = await _fileHelper.SaveCodeFileAsync(file, outputFolder, extension);
        }
    }

    private async Task GenerateChunkedMigrationReportAsync(
        List<CobolFile> cobolFiles,
        List<CodeFile> generatedFiles,
        List<ChunkedFileResult> chunkedResults,
        DependencyMap dependencyMap,
        string outputFolder,
        DateTime startTime,
        CancellationToken cancellationToken = default)
    {
        _validation = chunkedResults.Any(result => !result.Success)
            ? new(false, "\n## Conversion validation\nNot evaluated: chunk assembly is incomplete.\n", null, null)
            : await GeneratedOutputValidator.RunAsync(generatedFiles, cobolFiles,
                _sourceRoot!, outputFolder, _settings.ApplicationSettings.TargetLanguage, _settings.CompileGate, _logger,
                _parityRepairAgent is null ? null : _parityRepairAgent.RepairAsync, _execution, cancellationToken,
                _settings.ConversionSettings);
        var totalTime = DateTime.UtcNow - startTime;
        var reportPath = Path.Combine(outputFolder, "chunked-migration-report.md");

        var report = new StringBuilder();
        var langName = _settings.ApplicationSettings.TargetLanguage == TargetLanguage.CSharp 
            ? "C#" : "Java";

        report.AppendLine($"# Chunked COBOL to {langName} Migration Report");
        report.AppendLine($"Generated: {DateTime.UtcNow:yyyy-MM-dd HH:mm:ss} UTC");
        report.AppendLine($"Total Migration Time: {totalTime}");
        report.AppendLine();

        // Chunking statistics
        report.AppendLine("## 📊 Chunking Statistics");
        if (ConversionEligibility.IsDraft(_settings.ConversionSettings))
            report.AppendLine("\n> PARTIAL DRAFT — NOT VERIFIED. Results cover only eligible whole programs. Blocked areas have no generated implementation. See draft-conversion-plan.json; routine impact is not measured. JCL entry points are not generated.\n");
        report.AppendLine($"- **Total Files**: {cobolFiles.Count}");
        report.AppendLine($"- **Chunked Files**: {chunkedResults.Count}");
        report.AppendLine($"- **Total Chunks Processed**: {chunkedResults.Sum(r => r.Result.TotalChunks)}");
        report.AppendLine($"- **Successful Chunks**: {chunkedResults.Sum(r => r.Result.SuccessfulChunks)}");
        report.AppendLine($"- **Failed Chunks**: {chunkedResults.Sum(r => r.Result.FailedChunks)}");
        report.AppendLine();

        // Per-file breakdown
        if (chunkedResults.Any())
        {
            report.AppendLine("## 📁 Chunked File Details");
            report.AppendLine("| File | Lines | Chunks | Success | Time |");
            report.AppendLine("|------|-------|--------|---------|------|");

            foreach (var result in chunkedResults)
            {
                var success = result.Success
                    ? "✅" : $"❌ Assembly rejected: {string.Join("; ", result.AssemblyErrors)}";
                report.AppendLine(
                    $"| {result.SourceFile} | {result.Plan.TotalLines} | {result.Plan.ChunkCount} | {success} | {result.Result.TotalProcessingTimeMs}ms |");
            }
            report.AppendLine();
        }

        // Consistency report
        report.AppendLine("## 🔒 Consistency Status");
        var consistentFiles = chunkedResults.Count(r => r.Result.IsConsistent);
        report.AppendLine($"- **Consistent Files**: {consistentFiles}/{chunkedResults.Count}");
        
        var inconsistentFiles = chunkedResults.Where(r => !r.Result.IsConsistent).ToList();
        if (inconsistentFiles.Any())
        {
            report.AppendLine("### Files with Consistency Issues:");
            foreach (var file in inconsistentFiles)
            {
                report.AppendLine($"- **{file.SourceFile}**");
                if (file.Result.ReconciliationReport != null)
                {
                    foreach (var issue in file.Result.ReconciliationReport.SignatureIssues.Take(5))
                    {
                        report.AppendLine($"  - {issue.IssueType}: {issue.Description}");
                    }
                }
            }
        }
        report.AppendLine();

        // Settings used
        report.AppendLine("## ⚙️ Chunking Settings Used");
        report.AppendLine($"- **Max Lines per Chunk**: {_settings.ChunkingSettings.MaxLinesPerChunk}");
        report.AppendLine($"- **Max Tokens per Chunk**: {_settings.ChunkingSettings.MaxTokensPerChunk}");
        report.AppendLine($"- **Overlap Lines**: {_settings.ChunkingSettings.OverlapLines}");
        report.AppendLine($"- **Naming Strategy**: {_settings.ConversionSettings.NamingStrategy}");
        report.AppendLine($"- **Progressive Compression**: {_settings.ChunkingSettings.EnableProgressiveCompression}");
        report.AppendLine();

        report.Append(_validation.Markdown);
        if (_compileGate is not null)
        {
            report.AppendLine("\n### Initial C# compiler-repair history\n");
            report.Append(_compileGate.ToMarkdown(_settings.CompileGate.MaxErrorsInReport));
        }

        await File.WriteAllTextAsync(reportPath, report.ToString());
        _logger.LogInformation("Chunked migration report saved to {Path}", reportPath);
    }

    private string GetOutputFileName(string cobolFileName)
    {
        var className = GetClassNameFromFile(cobolFileName);
        var extension = _settings.ApplicationSettings.TargetLanguage == TargetLanguage.CSharp
            ? ".cs" : ".java";
        return $"{className}{extension}";
    }

    private string GetClassNameFromFile(string fileName)
    {
        var baseName = Path.GetFileNameWithoutExtension(fileName);
        // Convert COBOL naming to PascalCase
        var parts = baseName.Split(new[] { '-', '_' }, StringSplitOptions.RemoveEmptyEntries);
        return string.Join("", parts.Select(p => 
            char.ToUpper(p[0]) + (p.Length > 1 ? p.Substring(1).ToLower() : "")));
    }

    private string GetNamespaceFromFile(string fileName)
    {
        return GetClassNameFromFile(fileName);
    }

    private string ExtractClassName(string code)
    {
        var lines = code.Split('\n');
        foreach (var line in lines)
        {
            var trimmed = line.Trim();
            if (trimmed.Contains("class "))
            {
                var classIndex = trimmed.IndexOf("class ") + 6;
                var endIndex = trimmed.IndexOfAny(new[] { ' ', '{', ':' }, classIndex);
                if (endIndex > classIndex)
                {
                    return trimmed.Substring(classIndex, endIndex - classIndex);
                }
            }
        }
        return "ConvertedProgram";
    }

    private string ExtractClassContent(string code)
    {
        // Simple extraction - get content between class braces
        var classStart = code.IndexOf('{');
        var classEnd = code.LastIndexOf('}');
        
        if (classStart >= 0 && classEnd > classStart)
        {
            return code.Substring(classStart + 1, classEnd - classStart - 1).Trim();
        }
        
        return code;
    }
}

/// <summary>
/// Result of processing a large file with chunking.
/// </summary>
public class ChunkedFileResult
{
    public string SourceFile { get; set; } = string.Empty;
    public ChunkingPlan Plan { get; set; } = new();
    public ChunkingResult Result { get; set; } = new();
    public bool Success { get; set; }
    public List<string> AssemblyErrors { get; } = [];
}
