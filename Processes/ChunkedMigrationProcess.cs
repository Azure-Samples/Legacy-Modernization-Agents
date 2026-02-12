using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Agents;
using CobolToQuarkusMigration.Agents.Infrastructure;
using CobolToQuarkusMigration.Agents.Interfaces;
using CobolToQuarkusMigration.Chunking;
using CobolToQuarkusMigration.Chunking.Interfaces;
using CobolToQuarkusMigration.Chunking.Models;
using CobolToQuarkusMigration.Helpers;
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
    private readonly ResponsesApiClient _responsesApiClient;
    private readonly ILogger<ChunkedMigrationProcess> _logger;
    private readonly FileHelper _fileHelper;
    private readonly AppSettings _settings;
    private readonly EnhancedLogger _enhancedLogger;
    private readonly ChatLogger _chatLogger;
    private readonly IMigrationRepository _migrationRepository;

    private ChunkingOrchestrator? _chunkingOrchestrator;
    private IChunkAwareConverter? _chunkAwareConverter;
    private ICobolAnalyzerAgent? _cobolAnalyzerAgent;
    private IDependencyMapperAgent? _dependencyMapperAgent;

    /// <summary>
    /// Initializes a new instance of the ChunkedMigrationProcess class.
    /// </summary>
    public ChunkedMigrationProcess(
        IChatClient chatClient,
        ResponsesApiClient responsesApiClient,
        ILogger<ChunkedMigrationProcess> logger,
        FileHelper fileHelper,
        AppSettings settings,
        IMigrationRepository migrationRepository)
    {
        _chatClient = chatClient;
        _responsesApiClient = responsesApiClient;
        _logger = logger;
        _fileHelper = fileHelper;
        _settings = settings;
        _enhancedLogger = new EnhancedLogger(logger);
        _chatLogger = new ChatLogger(LoggerFactory.Create(builder => builder.AddConsole()).CreateLogger<ChatLogger>());
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
            _chunkAwareConverter = new ChunkAwareCSharpConverter(
                _responsesApiClient,
                loggerFactory.CreateLogger<ChunkAwareCSharpConverter>(),
                _settings.AISettings.JavaConverterModelId,
                _settings.ConversionSettings,
                _enhancedLogger,
                _chatLogger);
        }
        else
        {
            _chunkAwareConverter = new ChunkAwareJavaConverter(
                _responsesApiClient,
                loggerFactory.CreateLogger<ChunkAwareJavaConverter>(),
                _settings.AISettings.JavaConverterModelId,
                _settings.ConversionSettings,
                _enhancedLogger,
                _chatLogger);
        }

        // Initialize standard agents
        _enhancedLogger.ShowStep(3, 4, "CobolAnalyzerAgent", "COBOL structure analysis");
        _cobolAnalyzerAgent = new CobolAnalyzerAgent(
            _responsesApiClient,
            loggerFactory.CreateLogger<CobolAnalyzerAgent>(),
            _settings.AISettings.CobolAnalyzerModelId,
            _enhancedLogger,
            _chatLogger);

        _enhancedLogger.ShowStep(4, 4, "DependencyMapperAgent", "Cross-file dependency mapping");
        _dependencyMapperAgent = new DependencyMapperAgent(
            _responsesApiClient,
            loggerFactory.CreateLogger<DependencyMapperAgent>(),
            _settings.AISettings.DependencyMapperModelId ?? _settings.AISettings.CobolAnalyzerModelId,
            _enhancedLogger,
            _chatLogger);

        _enhancedLogger.ShowSuccess("All chunked migration agents initialized");
    }

    /// <summary>
    /// Runs the chunked migration process.
    /// </summary>
    public async Task RunAsync(
        string cobolSourceFolder,
        string outputFolder,
        Action<string, int, int, double?>? progressCallback = null,
        int? existingRunId = null,
        CancellationToken cancellationToken = default)
    {
        var targetName = _settings.ApplicationSettings.TargetLanguage == TargetLanguage.CSharp 
            ? "C# .NET" : "Java Quarkus";
        
        _enhancedLogger.ShowSectionHeader(
            $"CHUNKED COBOL TO {targetName} MIGRATION",
            $"Smart chunking enabled (max {_settings.ChunkingSettings.MaxLinesPerChunk} lines/chunk)");

        if (_chunkingOrchestrator == null || _chunkAwareConverter == null)
        {
            throw new InvalidOperationException("Agents not initialized. Call InitializeAgents() first.");
        }

        var startTime = DateTime.UtcNow;
        var runId = existingRunId ?? await _migrationRepository.StartRunAsync(cobolSourceFolder, outputFolder);

        // Pass run ID to converter for spec lookup
        _chunkAwareConverter.SetRunId(runId);
        
        // Show initial dashboard
        _enhancedLogger.ShowDashboardSummary(runId, targetName, "Running", "1/6: File Discovery", 5);

        try
        {
            // Step 1: Discover files
            _enhancedLogger.ShowStep(1, 6, "File Discovery", "Scanning for COBOL programs");
            progressCallback?.Invoke("Scanning for COBOL files", 1, 6, null);

            var cobolFiles = await _fileHelper.ScanDirectoryForCobolFilesAsync(cobolSourceFolder);
            await _migrationRepository.SaveCobolFilesAsync(runId, cobolFiles);

            if (cobolFiles.Count == 0)
            {
                _enhancedLogger.ShowWarning("No COBOL files found");
                await _migrationRepository.CompleteRunAsync(runId, "NoFiles");
                return;
            }

            _enhancedLogger.ShowSuccess($"Found {cobolFiles.Count} COBOL files");

            // Categorize files by size
            var (smallFiles, largeFiles) = CategorizeBySize(cobolFiles);
            _logger.LogInformation(
                "File categorization: {Small} small files, {Large} large files requiring chunking",
                smallFiles.Count, largeFiles.Count);

            // Step 2: Analyze dependencies
            _enhancedLogger.ShowDashboardSummary(runId, targetName, "Running", "2/6: Dependency Analysis", 15);
            _enhancedLogger.ShowStep(2, 6, "Dependency Analysis", "Mapping file relationships");
            progressCallback?.Invoke("Analyzing dependencies", 2, 6, null);

            var dependencyMap = await _dependencyMapperAgent!.AnalyzeDependenciesAsync(
                cobolFiles, new List<CobolAnalysis>());
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

            // Step 6: Output generation
            _enhancedLogger.ShowDashboardSummary(runId, targetName, "Running", "6/6: Output Generation", 95);
            _enhancedLogger.ShowStep(6, 6, "Output Generation", "Writing converted files");
            progressCallback?.Invoke("Saving output files", 6, 6, null);

            var allFiles = smallFileResults.Concat(assembledFiles).ToList();
            await SaveOutputFilesAsync(allFiles, outputFolder);

            // Generate reports
            await GenerateChunkedMigrationReportAsync(
                cobolFiles, allFiles, largeFileResults, dependencyMap, outputFolder, startTime);

            // Save full chat log for traceability
            await _chatLogger.SaveChatLogAsync();
            await _chatLogger.SaveChatLogJsonAsync();
            _logger.LogInformation("Full chat logs saved to Logs folder");

            _enhancedLogger.ShowDashboardSummary(runId, targetName, "Completed", "Done", 100);
            await _migrationRepository.CompleteRunAsync(runId, "Completed");

            _enhancedLogger.ShowSuccess(
                $"Chunked migration completed: {allFiles.Count} files generated");

            progressCallback?.Invoke("Migration completed successfully", 6, 6, 100);
        }
        catch (Exception ex)
        {
            _enhancedLogger.ShowError($"Migration failed: {ex.Message}", ex);
            await _migrationRepository.CompleteRunAsync(runId, "Failed", ex.Message);
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
                SourceFile = file.FileName,
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
            SourceFile = file.FileName,
            Plan = plan,
            Result = chunkingResult,
            Success = chunkingResult.IsConsistent && chunkingResult.FailedChunks == 0
        };
    }

    private List<CodeFile> AssembleChunkedFiles(List<ChunkedFileResult> chunkedResults)
    {
        var results = new List<CodeFile>();
        var isJava = _settings.ApplicationSettings.TargetLanguage == TargetLanguage.Java;

        foreach (var chunkedResult in chunkedResults)
        {
            // Get successful chunks - we'll generate output even if some failed
            var successfulChunks = chunkedResult.Result.ChunkResults
                .Where(r => r.Success && !string.IsNullOrEmpty(r.ConversionResult?.ConvertedCode))
                .OrderBy(r => r.ChunkIndex)
                .ToList();

            if (!successfulChunks.Any())
            {
                _logger.LogWarning(
                    "Skipping assembly of {File} - no successful chunks to assemble",
                    chunkedResult.SourceFile);
                continue;
            }

            if (chunkedResult.Result.FailedChunks > 0)
            {
                _logger.LogWarning(
                    "Assembling {File} with {Success}/{Total} successful chunks ({Failed} failed)",
                    chunkedResult.SourceFile,
                    successfulChunks.Count,
                    chunkedResult.Result.TotalChunks,
                    chunkedResult.Result.FailedChunks);
            }

            if (isJava)
            {
                // Java: Generate multiple files based on detected classes
                var javaFiles = AssembleJavaFiles(chunkedResult, successfulChunks);
                results.AddRange(javaFiles);
            }
            else
            {
                // C#: Generate multiple files based on detected classes (similar to Java)
                var csharpFiles = AssembleCSharpFiles(chunkedResult, successfulChunks);
                results.AddRange(csharpFiles);
            }
        }

        return results;
    }

    private List<CodeFile> AssembleCSharpFiles(ChunkedFileResult chunkedResult, List<ChunkProcessingResult> successfulChunks)
    {
        var results = new List<CodeFile>();
        var baseClassName = GetClassNameFromFile(chunkedResult.SourceFile);
        var namespacePrefix = _settings.AssemblySettings?.CSharp?.NamespacePrefix ?? "CobolMigration";
        var namespaceName = $"{namespacePrefix}.{baseClassName}";

        // Collect all using statements and detect distinct classes
        var allUsings = new HashSet<string>();
        var classSegments = new Dictionary<string, StringBuilder>();
        var mainClassName = baseClassName;

        foreach (var chunkResult in successfulChunks)
        {
            var code = chunkResult.ConversionResult?.ConvertedCode ?? "";
            
            // Extract using statements
            foreach (var line in code.Split('\n'))
            {
                var trimmed = line.Trim();
                if (trimmed.StartsWith("using ") && trimmed.EndsWith(";"))
                {
                    allUsings.Add(trimmed);
                }
            }

            // Detect class names in this chunk
            var detectedClasses = ExtractCSharpClassNames(code);
            
            if (detectedClasses.Count == 0)
            {
                // No class detected, add to main class
                if (!classSegments.ContainsKey(mainClassName))
                {
                    classSegments[mainClassName] = new StringBuilder();
                }
                classSegments[mainClassName].AppendLine(ExtractCSharpClassContent(code));
            }
            else
            {
                // Add content to detected classes
                foreach (var className in detectedClasses)
                {
                    var actualClassName = SanitizeCSharpClassName(className);
                    if (!classSegments.ContainsKey(actualClassName))
                    {
                        classSegments[actualClassName] = new StringBuilder();
                    }
                    classSegments[actualClassName].AppendLine(ExtractCSharpClassContentForClass(code, className));
                }
            }
        }

        // If no classes detected at all, create a main class
        if (classSegments.Count == 0)
        {
            classSegments[mainClassName] = new StringBuilder();
            foreach (var chunkResult in successfulChunks)
            {
                classSegments[mainClassName].AppendLine(chunkResult.ConversionResult?.ConvertedCode ?? "// Empty chunk");
            }
        }

        // Generate a file for each class
        foreach (var kvp in classSegments)
        {
            var className = kvp.Key;
            var content = kvp.Value.ToString();
            
            var csharpFile = new StringBuilder();
            
            // Using statements
            foreach (var usingStmt in allUsings.OrderBy(u => u))
            {
                csharpFile.AppendLine(usingStmt);
            }
            
            // Add common C# usings if not present
            var commonUsings = new[]
            {
                "using System;",
                "using System.Collections.Generic;",
                "using System.Linq;",
                "using System.Text;"
            };
            
            foreach (var commonUsing in commonUsings)
            {
                if (!allUsings.Any(u => u.Equals(commonUsing, StringComparison.OrdinalIgnoreCase)))
                {
                    csharpFile.AppendLine(commonUsing);
                }
            }
            
            csharpFile.AppendLine();
            
            // Namespace declaration
            csharpFile.AppendLine($"namespace {namespaceName};");
            csharpFile.AppendLine();
            
            // Class declaration
            csharpFile.AppendLine($"/// <summary>");
            csharpFile.AppendLine($"/// Converted from COBOL source: {chunkedResult.SourceFile}");
            csharpFile.AppendLine($"/// </summary>");
            csharpFile.AppendLine($"public partial class {className}");
            csharpFile.AppendLine("{");
            csharpFile.AppendLine(content);
            csharpFile.AppendLine("}");

            results.Add(new CodeFile
            {
                FileName = $"{className}.cs",
                Content = csharpFile.ToString(),
                ClassName = className,
                OriginalCobolFileName = chunkedResult.SourceFile,
                TargetLanguage = "CSharp"
            });
        }

        _logger.LogInformation(
            "Assembled {FileCount} C# files from {ChunkCount} chunks for {SourceFile}",
            results.Count, successfulChunks.Count, chunkedResult.SourceFile);

        return results;
    }

    private List<string> ExtractCSharpClassNames(string code)
    {
        var classNames = new List<string>();
        // Regex to match class definitions, ignoring comments and other text
        // Matches: [modifiers] class ClassName
        var regex = new System.Text.RegularExpressions.Regex(
            @"^\s*(?:public|protected|private|internal)?\s*(?:static)?\s*(?:sealed|abstract|partial)?\s*class\s+([a-zA-Z0-9_]+)",
            System.Text.RegularExpressions.RegexOptions.Multiline);
        
        foreach (System.Text.RegularExpressions.Match match in regex.Matches(code))
        {
            if (match.Success)
            {
                classNames.Add(match.Groups[1].Value);
            }
        }
        
        return classNames.Distinct().ToList();
    }

    private string SanitizeCSharpClassName(string className)
    {
        // Remove any invalid characters and ensure valid C# class name
        var sanitized = new StringBuilder();
        bool capitalizeNext = true;
        
        foreach (var c in className)
        {
            if (char.IsLetterOrDigit(c))
            {
                sanitized.Append(capitalizeNext ? char.ToUpper(c) : c);
                capitalizeNext = false;
            }
            else if (c == '_' || c == '-')
            {
                capitalizeNext = true;
            }
        }
        
        // Ensure it starts with a letter
        var result = sanitized.ToString();
        if (result.Length > 0 && char.IsDigit(result[0]))
        {
            result = "Class" + result;
        }
        
        return string.IsNullOrEmpty(result) ? "ConvertedClass" : result;
    }

    private string ExtractCSharpClassContent(string code)
    {
        // Extract content between class braces
        var classStart = code.IndexOf('{');
        var classEnd = code.LastIndexOf('}');
        
        if (classStart >= 0 && classEnd > classStart)
        {
            return code.Substring(classStart + 1, classEnd - classStart - 1).Trim();
        }
        
        // If no braces found, return cleaned code without using/namespace statements
        var lines = code.Split('\n')
            .Where(l => !l.Trim().StartsWith("using ") && 
                       !l.Trim().StartsWith("namespace ") &&
                       !l.Trim().StartsWith("public class ") &&
                       !l.Trim().StartsWith("public partial class ") &&
                       !l.Trim().StartsWith("class "));
        
        return string.Join("\n", lines);
    }

    private string ExtractCSharpClassContentForClass(string code, string targetClassName)
    {
        // Find the specific class and extract its content
        var lines = code.Split('\n');
        var inTargetClass = false;
        var braceCount = 0;
        var content = new StringBuilder();
        
        // Regex to match the specific class definition
        var classRegex = new System.Text.RegularExpressions.Regex(
            @"^\s*(?:public|protected|private|internal)?\s*(?:static)?\s*(?:sealed|abstract|partial)?\s*class\s+" + 
            System.Text.RegularExpressions.Regex.Escape(targetClassName) + @"\b");

        foreach (var line in lines)
        {
            var trimmed = line.Trim();
            
            if (!inTargetClass && classRegex.IsMatch(line))
            {
                inTargetClass = true;
                braceCount = 0;
            }
            
            if (inTargetClass)
            {
                foreach (var c in line)
                {
                    if (c == '{') braceCount++;
                    else if (c == '}') braceCount--;
                }
                
                // Skip the class declaration line itself
                if (!classRegex.IsMatch(line) && braceCount >= 0)
                {
                    // Don't include the final closing brace
                    if (braceCount > 0 || !trimmed.StartsWith("}"))
                    {
                        content.AppendLine(line);
                    }
                }
                
                if (braceCount == 0 && content.Length > 0)
                {
                    break; // End of class
                }
            }
        }
        
        return content.ToString().Trim();
    }

    private List<CodeFile> AssembleJavaFiles(ChunkedFileResult chunkedResult, List<ChunkProcessingResult> successfulChunks)
    {
        var results = new List<CodeFile>();
        var baseClassName = GetClassNameFromFile(chunkedResult.SourceFile);
        var packagePrefix = _settings.AssemblySettings?.Java?.PackagePrefix ?? "com.example.cobol";
        var packageName = $"{packagePrefix}.{baseClassName.ToLower()}";

        // Collect all imports and detect distinct classes
        var allImports = new HashSet<string>();
        var classSegments = new Dictionary<string, StringBuilder>();
        var mainClassName = baseClassName;

        foreach (var chunkResult in successfulChunks)
        {
            var code = chunkResult.ConversionResult?.ConvertedCode ?? "";
            
            // Extract imports
            foreach (var line in code.Split('\n'))
            {
                var trimmed = line.Trim();
                if (trimmed.StartsWith("import ") && trimmed.EndsWith(";"))
                {
                    allImports.Add(trimmed);
                }
            }

            // Detect class names in this chunk
            var detectedClasses = ExtractJavaClassNames(code);
            
            if (detectedClasses.Count == 0)
            {
                // No class detected, add to main class
                if (!classSegments.ContainsKey(mainClassName))
                {
                    classSegments[mainClassName] = new StringBuilder();
                }
                classSegments[mainClassName].AppendLine(ExtractJavaClassContent(code));
            }
            else
            {
                // Add content to detected classes
                foreach (var className in detectedClasses)
                {
                    var actualClassName = SanitizeJavaClassName(className);
                    if (!classSegments.ContainsKey(actualClassName))
                    {
                        classSegments[actualClassName] = new StringBuilder();
                    }
                    classSegments[actualClassName].AppendLine(ExtractJavaClassContentForClass(code, className));
                }
            }
        }

        // If no classes detected at all, create a main class
        if (classSegments.Count == 0)
        {
            classSegments[mainClassName] = new StringBuilder();
            foreach (var chunkResult in successfulChunks)
            {
                classSegments[mainClassName].AppendLine(chunkResult.ConversionResult?.ConvertedCode ?? "// Empty chunk");
            }
        }

        // Generate a file for each class
        foreach (var kvp in classSegments)
        {
            var className = kvp.Key;
            var content = kvp.Value.ToString();
            
            var javaFile = new StringBuilder();
            
            // Package declaration
            javaFile.AppendLine($"package {packageName};");
            javaFile.AppendLine();
            
            // Imports
            foreach (var import in allImports.OrderBy(i => i))
            {
                javaFile.AppendLine(import);
            }
            
            // Add common Java imports if not present
            var commonImports = new[]
            {
                "import java.math.BigDecimal;",
                "import java.util.*;",
                "import jakarta.enterprise.context.ApplicationScoped;"
            };
            
            foreach (var commonImport in commonImports)
            {
                if (!allImports.Any(i => i.Contains(commonImport.Split(' ')[1].TrimEnd(';'))))
                {
                    javaFile.AppendLine(commonImport);
                }
            }
            
            javaFile.AppendLine();
            
            // Class with Quarkus annotation
            javaFile.AppendLine("@ApplicationScoped");
            javaFile.AppendLine($"public class {className} {{");
            javaFile.AppendLine();
            javaFile.AppendLine(content);
            javaFile.AppendLine("}");

            results.Add(new CodeFile
            {
                FileName = $"{className}.java",
                Content = javaFile.ToString(),
                ClassName = className,
                OriginalCobolFileName = chunkedResult.SourceFile,
                TargetLanguage = "Java"
            });
        }

        _logger.LogInformation(
            "Assembled {FileCount} Java files from {ChunkCount} chunks for {SourceFile}",
            results.Count, successfulChunks.Count, chunkedResult.SourceFile);

        return results;
    }

    private List<string> ExtractJavaClassNames(string code)
    {
        var classNames = new List<string>();
        // Regex to match class definitions, ignoring comments and other text
        // Matches: [modifiers] class ClassName
        var regex = new System.Text.RegularExpressions.Regex(
            @"^\s*(?:public|protected|private)?\s*(?:static)?\s*(?:final)?\s*(?:abstract)?\s*class\s+([a-zA-Z0-9_]+)",
            System.Text.RegularExpressions.RegexOptions.Multiline);
        
        foreach (System.Text.RegularExpressions.Match match in regex.Matches(code))
        {
            if (match.Success)
            {
                classNames.Add(match.Groups[1].Value);
            }
        }
        
        return classNames.Distinct().ToList();
    }

    private string SanitizeJavaClassName(string className)
    {
        // Remove any invalid characters and ensure valid Java class name
        var sanitized = new StringBuilder();
        bool capitalizeNext = true;
        
        foreach (var c in className)
        {
            if (char.IsLetterOrDigit(c))
            {
                sanitized.Append(capitalizeNext ? char.ToUpper(c) : c);
                capitalizeNext = false;
            }
            else if (c == '_' || c == '-')
            {
                capitalizeNext = true;
            }
        }
        
        // Ensure it starts with a letter
        var result = sanitized.ToString();
        if (result.Length > 0 && char.IsDigit(result[0]))
        {
            result = "Class" + result;
        }
        
        return string.IsNullOrEmpty(result) ? "ConvertedClass" : result;
    }

    private string ExtractJavaClassContent(string code)
    {
        // Extract content between class braces
        var classStart = code.IndexOf('{');
        var classEnd = code.LastIndexOf('}');
        
        if (classStart >= 0 && classEnd > classStart)
        {
            return code.Substring(classStart + 1, classEnd - classStart - 1).Trim();
        }
        
        // If no braces found, return cleaned code without package/import statements
        var lines = code.Split('\n')
            .Where(l => !l.Trim().StartsWith("package ") && 
                       !l.Trim().StartsWith("import ") &&
                       !l.Trim().StartsWith("public class ") &&
                       !l.Trim().StartsWith("class "));
        
        return string.Join("\n", lines);
    }

    private string ExtractJavaClassContentForClass(string code, string targetClassName)
    {
        // Find the specific class and extract its content
        var lines = code.Split('\n');
        var inTargetClass = false;
        var braceCount = 0;
        var content = new StringBuilder();
        
        // Regex to match the specific class definition
        var classRegex = new System.Text.RegularExpressions.Regex(
            @"^\s*(?:public|protected|private)?\s*(?:static)?\s*(?:final)?\s*(?:abstract)?\s*class\s+" + 
            System.Text.RegularExpressions.Regex.Escape(targetClassName) + @"\b");

        foreach (var line in lines)
        {
            var trimmed = line.Trim();
            
            if (!inTargetClass && classRegex.IsMatch(line))
            {
                inTargetClass = true;
                braceCount = 0;
            }
            
            if (inTargetClass)
            {
                foreach (var c in line)
                {
                    if (c == '{') braceCount++;
                    else if (c == '}') braceCount--;
                }
                
                // Skip the class declaration line itself
                if (!classRegex.IsMatch(line) && braceCount >= 0)
                {
                    // Don't include the final closing brace
                    if (braceCount > 0 || !trimmed.StartsWith("}"))
                    {
                        content.AppendLine(line);
                    }
                }
                
                if (braceCount == 0 && content.Length > 0)
                {
                    break; // End of class
                }
            }
        }
        
        return content.ToString().Trim();
    }

    private async Task SaveOutputFilesAsync(List<CodeFile> files, string outputFolder)
    {
        var extension = _settings.ApplicationSettings.TargetLanguage == TargetLanguage.CSharp
            ? ".cs" : ".java";

        foreach (var file in files)
        {
            await _fileHelper.SaveCodeFileAsync(file, outputFolder, extension);
        }
    }

    private async Task GenerateChunkedMigrationReportAsync(
        List<CobolFile> cobolFiles,
        List<CodeFile> generatedFiles,
        List<ChunkedFileResult> chunkedResults,
        DependencyMap dependencyMap,
        string outputFolder,
        DateTime startTime)
    {
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
                var success = result.Result.SuccessfulChunks == result.Result.TotalChunks
                    ? "✅" : $"⚠️ {result.Result.SuccessfulChunks}/{result.Result.TotalChunks}";
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
}
