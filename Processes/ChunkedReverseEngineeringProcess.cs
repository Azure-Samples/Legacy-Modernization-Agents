using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Agents;
using CobolToQuarkusMigration.Agents.Interfaces;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Chunking;
using CobolToQuarkusMigration.Chunking.Models;
using System.Text.Json;
using Microsoft.Data.Sqlite;

namespace CobolToQuarkusMigration.Processes;

/// <summary>
/// Orchestrates chunked reverse engineering for large COBOL files.
/// Uses smart chunking to analyze files that exceed the API character limit.
/// </summary>
public class ChunkedReverseEngineeringProcess
{
    private readonly ICobolAnalyzerAgent _cobolAnalyzerAgent;
    private readonly BusinessLogicExtractorAgent _businessLogicExtractorAgent;
    private readonly FileHelper _fileHelper;
    private readonly ILogger<ChunkedReverseEngineeringProcess> _logger;
    private readonly EnhancedLogger _enhancedLogger;
    private readonly ChunkingSettings _chunkingSettings;
    private readonly ChunkingOrchestrator _chunkingOrchestrator;
    private readonly string _databasePath;
    private Glossary? _glossary;

    public ChunkedReverseEngineeringProcess(
        ICobolAnalyzerAgent cobolAnalyzerAgent,
        BusinessLogicExtractorAgent businessLogicExtractorAgent,
        FileHelper fileHelper,
        ChunkingSettings chunkingSettings,
        ChunkingOrchestrator chunkingOrchestrator,
        ILogger<ChunkedReverseEngineeringProcess> logger,
        EnhancedLogger enhancedLogger,
        string databasePath)
    {
        _cobolAnalyzerAgent = cobolAnalyzerAgent;
        _businessLogicExtractorAgent = businessLogicExtractorAgent;
        _fileHelper = fileHelper;
        _chunkingSettings = chunkingSettings;
        _chunkingOrchestrator = chunkingOrchestrator;
        _logger = logger;
        _enhancedLogger = enhancedLogger;
        _databasePath = databasePath;
    }

    /// <summary>
    /// Runs the chunked reverse engineering process.
    /// </summary>
    public async Task<ReverseEngineeringResult> RunAsync(
        string cobolSourceFolder,
        string outputFolder,
        Action<string, int, int>? progressCallback = null,
        int? runId = null)
    {
        var result = new ReverseEngineeringResult();

        try
        {
            _enhancedLogger.ShowSectionHeader("CHUNKED REVERSE ENGINEERING PROCESS", "Extracting Business Logic with Smart Chunking");
            _logger.LogInformation("Starting chunked reverse engineering process");
            _logger.LogInformation("Source folder: {SourceFolder}", cobolSourceFolder);
            _logger.LogInformation("Output folder: {OutputFolder}", outputFolder);

            // Load glossary if available
            await LoadGlossaryAsync();

            var totalSteps = 4; // Added chunking step

            // Step 1: Scan for COBOL files
            _enhancedLogger.ShowStep(1, totalSteps, "File Discovery", "Scanning for COBOL files");
            progressCallback?.Invoke("Scanning for COBOL files", 1, totalSteps);

            var cobolFiles = await _fileHelper.ScanDirectoryForCobolFilesAsync(cobolSourceFolder);
            _enhancedLogger.ShowSuccess($"Found {cobolFiles.Count} COBOL files");
            _logger.LogInformation("Found {Count} COBOL files", cobolFiles.Count);

            result.TotalFilesAnalyzed = cobolFiles.Count;

            if (cobolFiles.Count == 0)
            {
                _enhancedLogger.ShowWarning("No COBOL files found. Nothing to reverse engineer.");
                return result;
            }

            // Separate small files from large files needing chunking
            var smallFiles = new List<CobolFile>();
            var largeFiles = new List<CobolFile>();

            foreach (var file in cobolFiles)
            {
                var lineCount = file.Content.Split('\n').Length;
                if (_chunkingSettings.RequiresChunking(file.Content.Length, lineCount))
                {
                    largeFiles.Add(file);
                    _logger.LogInformation("File {FileName} ({Chars:N0} chars, {Lines:N0} lines) requires chunking", 
                        file.FileName, file.Content.Length, lineCount);
                }
                else
                {
                    smallFiles.Add(file);
                    _logger.LogInformation("File {FileName} ({Chars:N0} chars, {Lines:N0} lines) - direct processing", 
                        file.FileName, file.Content.Length, lineCount);
                }
            }

            _enhancedLogger.ShowSuccess($"Small files (direct): {smallFiles.Count}, Large files (chunked): {largeFiles.Count}");

            // Step 2: Process small files directly
            if (smallFiles.Count > 0)
            {
                _enhancedLogger.ShowStep(2, totalSteps, "Technical Analysis (Direct)", $"Analyzing {smallFiles.Count} small files");
                progressCallback?.Invoke("Analyzing small files", 2, totalSteps);

                var smallAnalyses = await _cobolAnalyzerAgent.AnalyzeCobolFilesAsync(
                    smallFiles,
                    (processed, total) => _enhancedLogger.ShowProgressBar(processed, total, "files analyzed"));

                result.TechnicalAnalyses.AddRange(smallAnalyses);

                var smallBusinessLogic = await _businessLogicExtractorAgent.ExtractBusinessLogicAsync(
                    smallFiles,
                    smallAnalyses,
                    _glossary,
                    (processed, total) => _enhancedLogger.ShowProgressBar(processed, total, "files processed"));

                result.BusinessLogicExtracts.AddRange(smallBusinessLogic);
            }

            // Step 3: Process large files with chunking (in parallel)
            if (largeFiles.Count > 0)
            {
                _enhancedLogger.ShowStep(3, totalSteps, "Chunked Analysis", $"Processing {largeFiles.Count} large files with smart chunking");
                progressCallback?.Invoke("Processing large files with chunking", 3, totalSteps);

                var maxParallel = Math.Min(_chunkingSettings.MaxParallelAnalysis, largeFiles.Count);
                var enableParallel = _chunkingSettings.EnableParallelProcessing;

                if (enableParallel && largeFiles.Count > 1 && maxParallel > 1)
                {
                    _logger.LogInformation("🚀 Processing {Count} large files in parallel with {Workers} workers", 
                        largeFiles.Count, maxParallel);

                    using var semaphore = new SemaphoreSlim(maxParallel, maxParallel);
                    var processedLarge = 0;
                    var lockObj = new object();

                    // Use indexed tuples to preserve order
                    var indexedTasks = new List<Task<(int Index, CobolAnalysis Analysis, BusinessLogic Logic)>>();

                    for (int i = 0; i < largeFiles.Count; i++)
                    {
                        var largeFile = largeFiles[i];
                        var index = i;

                        var task = Task.Run(async () =>
                        {
                            await semaphore.WaitAsync();
                            try
                            {
                                var (analysis, businessLogic) = await ProcessLargeFileWithChunkingAsync(largeFile, runId);

                                lock (lockObj)
                                {
                                    processedLarge++;
                                    _enhancedLogger.ShowProgressBar(processedLarge, largeFiles.Count, "large files chunked");
                                }

                                return (Index: index, Analysis: analysis, Logic: businessLogic);
                            }
                            finally
                            {
                                semaphore.Release();
                            }
                        });

                        indexedTasks.Add(task);
                    }

                    var results = await Task.WhenAll(indexedTasks);

                    // Sort by original index and add to result
                    foreach (var item in results.OrderBy(r => r.Index))
                    {
                        result.TechnicalAnalyses.Add(item.Analysis);
                        result.BusinessLogicExtracts.Add(item.Logic);
                    }

                    _logger.LogInformation("✅ Completed parallel processing of {Count} large files (order preserved)", largeFiles.Count);
                }
                else
                {
                    // Sequential processing
                    int processedLarge = 0;
                    foreach (var largeFile in largeFiles)
                    {
                        var (analysis, businessLogic) = await ProcessLargeFileWithChunkingAsync(largeFile, runId);
                        result.TechnicalAnalyses.Add(analysis);
                        result.BusinessLogicExtracts.Add(businessLogic);

                        processedLarge++;
                        _enhancedLogger.ShowProgressBar(processedLarge, largeFiles.Count, "large files chunked");
                    }
                }
            }

            // Step 4: Generate documentation
            _enhancedLogger.ShowStep(4, totalSteps, "Documentation Generation", "Generating reverse engineering report");
            progressCallback?.Invoke("Generating documentation", 4, totalSteps);

            // Count feature descriptions and features
            result.TotalUserStories = result.BusinessLogicExtracts.Sum(bl => bl.UserStories.Count);
            result.TotalFeatures = result.BusinessLogicExtracts.Sum(bl => bl.Features.Count);
            result.TotalBusinessRules = result.BusinessLogicExtracts.Sum(bl => bl.BusinessRules.Count);

            await GenerateOutputAsync(outputFolder, result);

            _enhancedLogger.ShowSuccess("✓ Chunked reverse engineering complete!");
            _logger.LogInformation("Output location: {OutputFolder}", outputFolder);
            Console.WriteLine($"📂 Output location: {outputFolder}");

            result.Success = true;
            result.OutputFolder = outputFolder;

            return result;
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error during chunked reverse engineering process");
            _enhancedLogger.ShowError($"Chunked reverse engineering failed: {ex.Message}");
            result.Success = false;
            result.ErrorMessage = ex.Message;
            throw;
        }
    }

    /// <summary>
    /// Processes a large file by chunking it and analyzing each chunk.
    /// </summary>
    private async Task<(CobolAnalysis analysis, BusinessLogic businessLogic)> ProcessLargeFileWithChunkingAsync(CobolFile largeFile, int? runId)
    {
        _logger.LogInformation("Chunking large file {FileName} ({Chars:N0} chars)", 
            largeFile.FileName, largeFile.Content.Length);

        // Create chunking plan
        var plan = await _chunkingOrchestrator.AnalyzeFileAsync(largeFile.FilePath, largeFile.Content);
        
        _logger.LogInformation("Created {ChunkCount} chunks for {FileName}", plan.ChunkCount, largeFile.FileName);

        // Seed chunk metadata so dashboards have visibility even before conversion runs
        if (runId.HasValue)
        {
            await SeedPendingChunksAsync(runId.Value, plan);
        }

        // Analyze each chunk - use parallel processing for multiple chunks
        var maxParallel = Math.Min(_chunkingSettings.MaxParallelAnalysis, plan.ChunkCount);
        var enableParallel = _chunkingSettings.EnableParallelProcessing;

        List<CobolAnalysis> chunkAnalyses;
        List<BusinessLogic> chunkBusinessLogics;

        if (enableParallel && plan.ChunkCount > 1 && maxParallel > 1)
        {
            _logger.LogInformation("🚀 Analyzing {ChunkCount} chunks in parallel with {Workers} workers for {FileName}",
                plan.ChunkCount, maxParallel, largeFile.FileName);

            using var semaphore = new SemaphoreSlim(maxParallel, maxParallel);
            var indexedTasks = new List<Task<(int Index, CobolAnalysis Analysis, BusinessLogic Logic)>>();

            for (int i = 0; i < plan.Chunks.Count; i++)
            {
                var chunk = plan.Chunks[i];
                var index = i;

                var task = Task.Run(async () =>
                {
                    await semaphore.WaitAsync();
                    try
                    {
                        _logger.LogInformation("Processing chunk {Index}/{Total} for {FileName} (lines {Start}-{End})",
                            index + 1, plan.ChunkCount, largeFile.FileName, chunk.StartLine, chunk.EndLine);

                        // Create a virtual CobolFile for this chunk
                        var chunkFile = new CobolFile
                        {
                            FileName = $"{largeFile.FileName}_chunk_{index + 1}",
                            FilePath = largeFile.FilePath,
                            Content = chunk.Content,
                            IsCopybook = largeFile.IsCopybook
                        };

                        // Analyze the chunk
                        var chunkAnalysis = await _cobolAnalyzerAgent.AnalyzeCobolFileAsync(chunkFile);

                        // Extract business logic from chunk
                        var chunkBusinessLogic = await _businessLogicExtractorAgent.ExtractBusinessLogicAsync(
                            chunkFile, chunkAnalysis, _glossary);

                        return (Index: index, Analysis: chunkAnalysis, Logic: chunkBusinessLogic);
                    }
                    finally
                    {
                        semaphore.Release();
                    }
                });

                indexedTasks.Add(task);
            }

            var results = await Task.WhenAll(indexedTasks);

            // Sort by original index to preserve chunk order for merging
            var orderedResults = results.OrderBy(r => r.Index).ToList();
            chunkAnalyses = orderedResults.Select(r => r.Analysis).ToList();
            chunkBusinessLogics = orderedResults.Select(r => r.Logic).ToList();

            _logger.LogInformation("✅ Completed parallel chunk analysis for {FileName} (order preserved)", largeFile.FileName);
        }
        else
        {
            // Sequential processing
            chunkAnalyses = new List<CobolAnalysis>();
            chunkBusinessLogics = new List<BusinessLogic>();

            for (int i = 0; i < plan.Chunks.Count; i++)
            {
                var chunk = plan.Chunks[i];
                
                _logger.LogInformation("Processing chunk {Index}/{Total} for {FileName} (lines {Start}-{End})",
                    i + 1, plan.ChunkCount, largeFile.FileName, chunk.StartLine, chunk.EndLine);

                // Create a virtual CobolFile for this chunk
                var chunkFile = new CobolFile
                {
                    FileName = $"{largeFile.FileName}_chunk_{i + 1}",
                    FilePath = largeFile.FilePath,
                    Content = chunk.Content,
                    IsCopybook = largeFile.IsCopybook
                };

                // Analyze the chunk
                var chunkAnalysis = await _cobolAnalyzerAgent.AnalyzeCobolFileAsync(chunkFile);
                chunkAnalyses.Add(chunkAnalysis);

                // Extract business logic from chunk
                var chunkBusinessLogic = await _businessLogicExtractorAgent.ExtractBusinessLogicAsync(
                    chunkFile, chunkAnalysis, _glossary);
                chunkBusinessLogics.Add(chunkBusinessLogic);
            }
        }

        // Merge chunk results into single analysis and business logic
        var mergedAnalysis = MergeAnalyses(largeFile, chunkAnalyses);
        var mergedBusinessLogic = MergeBusinessLogics(largeFile, chunkBusinessLogics);

        return (mergedAnalysis, mergedBusinessLogic);
    }

    /// <summary>
    /// Merges analyses from multiple chunks into a single analysis.
    /// </summary>
    private CobolAnalysis MergeAnalyses(CobolFile originalFile, List<CobolAnalysis> chunkAnalyses)
    {
        var merged = new CobolAnalysis
        {
            FileName = originalFile.FileName,
            FilePath = originalFile.FilePath,
            IsCopybook = originalFile.IsCopybook,
            ProgramDescription = $"Large file analyzed in {chunkAnalyses.Count} chunks. " +
                string.Join(" ", chunkAnalyses
                    .Where(a => !string.IsNullOrWhiteSpace(a.ProgramDescription) && !a.ProgramDescription.StartsWith("❌"))
                    .Select(a => a.ProgramDescription))
        };

        // Merge all divisions and paragraphs
        foreach (var analysis in chunkAnalyses)
        {
            foreach (var div in analysis.DataDivisions)
            {
                if (!merged.DataDivisions.Contains(div))
                    merged.DataDivisions.Add(div);
            }
            foreach (var div in analysis.ProcedureDivisions)
            {
                if (!merged.ProcedureDivisions.Contains(div))
                    merged.ProcedureDivisions.Add(div);
            }
            foreach (var copybook in analysis.CopybooksReferenced)
            {
                if (!merged.CopybooksReferenced.Contains(copybook))
                    merged.CopybooksReferenced.Add(copybook);
            }
            foreach (var para in analysis.Paragraphs)
            {
                if (!merged.Paragraphs.Any(p => p.Name == para.Name))
                    merged.Paragraphs.Add(para);
            }
            foreach (var variable in analysis.Variables)
            {
                if (!merged.Variables.Any(v => v.Name == variable.Name))
                    merged.Variables.Add(variable);
            }
        }

        // Combine raw analysis data
        merged.RawAnalysisData = string.Join("\n\n---\n\n", 
            chunkAnalyses.Select((a, i) => $"### Chunk {i + 1}\n{a.RawAnalysisData}"));

        return merged;
    }

    private async Task SeedPendingChunksAsync(int runId, ChunkingPlan plan)
    {
        try
        {
            await using var connection = new SqliteConnection($"Data Source={_databasePath};Cache=Shared");
            await connection.OpenAsync();

            foreach (var chunk in plan.Chunks)
            {
                await using var cmd = connection.CreateCommand();
                cmd.CommandText = @"
INSERT INTO chunk_metadata (run_id, source_file, chunk_index, start_line, end_line, status, semantic_units, tokens_used, processing_time_ms, created_at)
VALUES ($runId, $sourceFile, $chunkIndex, $startLine, $endLine, 'Pending', $semanticUnits, $tokensUsed, 0, $createdAt)
ON CONFLICT(run_id, source_file, chunk_index) DO NOTHING;";

                cmd.Parameters.AddWithValue("$runId", runId);
                cmd.Parameters.AddWithValue("$sourceFile", plan.SourceFile);
                cmd.Parameters.AddWithValue("$chunkIndex", chunk.ChunkIndex);
                cmd.Parameters.AddWithValue("$startLine", chunk.StartLine);
                cmd.Parameters.AddWithValue("$endLine", chunk.EndLine);
                cmd.Parameters.AddWithValue("$semanticUnits", JsonSerializer.Serialize(chunk.SemanticUnitNames, JsonSerializerOptions.Default));
                cmd.Parameters.AddWithValue("$tokensUsed", chunk.EstimatedTokens);
                cmd.Parameters.AddWithValue("$createdAt", DateTime.UtcNow.ToString("O"));

                await cmd.ExecuteNonQueryAsync();
            }
        }
        catch (Exception ex)
        {
            _logger.LogWarning(ex, "Failed to seed chunk metadata for run {RunId}", runId);
        }
    }

    /// <summary>
    /// Merges business logic from multiple chunks.
    /// </summary>
    private BusinessLogic MergeBusinessLogics(CobolFile originalFile, List<BusinessLogic> chunkBusinessLogics)
    {
        var merged = new BusinessLogic
        {
            FileName = originalFile.FileName,
            FilePath = originalFile.FilePath,
            IsCopybook = originalFile.IsCopybook,
            BusinessPurpose = $"Large file analyzed in {chunkBusinessLogics.Count} chunks. " +
                string.Join(" ", chunkBusinessLogics
                    .Where(bl => !string.IsNullOrWhiteSpace(bl.BusinessPurpose) && !bl.BusinessPurpose.StartsWith("❌"))
                    .Select(bl => bl.BusinessPurpose)
                    .Distinct())
        };

        // Merge user stories with unique IDs
        int storyId = 1;
        foreach (var bl in chunkBusinessLogics)
        {
            foreach (var story in bl.UserStories)
            {
                var newStory = new UserStory
                {
                    Id = $"US-{storyId++}",
                    Title = story.Title,
                    Role = story.Role,
                    Action = story.Action,
                    Benefit = story.Benefit,
                    SourceLocation = story.SourceLocation,
                    AcceptanceCriteria = story.AcceptanceCriteria
                };
                merged.UserStories.Add(newStory);
            }
        }

        // Merge features with unique IDs
        int featureId = 1;
        foreach (var bl in chunkBusinessLogics)
        {
            foreach (var feature in bl.Features)
            {
                var newFeature = new FeatureDescription
                {
                    Id = $"F-{featureId++}",
                    Name = feature.Name,
                    Description = feature.Description,
                    SourceLocation = feature.SourceLocation,
                    BusinessRules = feature.BusinessRules,
                    Inputs = feature.Inputs,
                    Outputs = feature.Outputs,
                    ProcessingSteps = feature.ProcessingSteps
                };
                merged.Features.Add(newFeature);
            }
        }

        // Merge business rules with unique IDs
        int ruleId = 1;
        foreach (var bl in chunkBusinessLogics)
        {
            foreach (var rule in bl.BusinessRules)
            {
                // Avoid duplicate rules
                if (!merged.BusinessRules.Any(r => r.Description == rule.Description))
                {
                    var newRule = new BusinessRule
                    {
                        Id = $"BR-{ruleId++}",
                        Description = rule.Description,
                        Condition = rule.Condition,
                        Action = rule.Action,
                        SourceLocation = rule.SourceLocation
                    };
                    merged.BusinessRules.Add(newRule);
                }
            }
        }

        return merged;
    }

    private async Task LoadGlossaryAsync()
    {
        try
        {
            var glossaryPath = Path.Combine("Data", "glossary.json");

            if (!File.Exists(glossaryPath))
            {
                _logger.LogInformation("No glossary file found at {Path}", glossaryPath);
                return;
            }

            var json = await File.ReadAllTextAsync(glossaryPath);
            _glossary = JsonSerializer.Deserialize<Glossary>(json, new JsonSerializerOptions
            {
                PropertyNameCaseInsensitive = true
            });

            if (_glossary?.Terms?.Any() == true)
            {
                _logger.LogInformation("Loaded glossary with {Count} terms", _glossary.Terms.Count);
                Console.WriteLine($"📖 Loaded glossary with {_glossary.Terms.Count} terms");
            }
        }
        catch (Exception ex)
        {
            _logger.LogWarning(ex, "Failed to load glossary file");
        }
    }

    private async Task GenerateOutputAsync(string outputFolder, ReverseEngineeringResult result)
    {
        Directory.CreateDirectory(outputFolder);

        // Generate single unified reverse-engineering-details.md
        var content = GenerateReverseEngineeringDetailsMarkdown(result);
        var outputPath = Path.Combine(outputFolder, "reverse-engineering-details.md");
        await File.WriteAllTextAsync(outputPath, content);
        _logger.LogInformation("Generated reverse engineering documentation: {Path}", outputPath);
    }

    private string GenerateReverseEngineeringDetailsMarkdown(ReverseEngineeringResult result)
    {
        var sb = new System.Text.StringBuilder();

        // Calculate program vs copybook counts
        var programCount = result.BusinessLogicExtracts.Count(bl => !bl.IsCopybook);
        var copybookCount = result.BusinessLogicExtracts.Count(bl => bl.IsCopybook);

        sb.AppendLine("# Reverse Engineering Details");
        sb.AppendLine();
        sb.AppendLine($"**Generated**: {DateTime.Now:yyyy-MM-dd HH:mm:ss}");
        sb.AppendLine($"**Total Files Analyzed**: {result.TotalFilesAnalyzed} ({programCount} programs, {copybookCount} copybooks)");
        sb.AppendLine($"**Total Feature Descriptions**: {result.TotalUserStories}");
        sb.AppendLine($"**Total Features**: {result.TotalFeatures}");
        sb.AppendLine($"**Total Business Rules**: {result.TotalBusinessRules}");
        sb.AppendLine();
        sb.AppendLine("---");
        sb.AppendLine();
        sb.AppendLine("## Business Logic");
        sb.AppendLine();

        foreach (var businessLogic in result.BusinessLogicExtracts)
        {
            var fileTypeLabel = businessLogic.IsCopybook ? " [Copybook]" : "";
            sb.AppendLine($"## {businessLogic.FileName}{fileTypeLabel}");
            sb.AppendLine();

            if (!string.IsNullOrWhiteSpace(businessLogic.BusinessPurpose))
            {
                sb.AppendLine("### Business Purpose");
                sb.AppendLine(businessLogic.BusinessPurpose);
                sb.AppendLine();
            }

            // Feature Descriptions (User Stories)
            if (businessLogic.UserStories.Any())
            {
                sb.AppendLine("### Feature Descriptions");
                sb.AppendLine();

                foreach (var story in businessLogic.UserStories)
                {
                    sb.AppendLine($"#### {story.Id}: {story.Title}");
                    sb.AppendLine();
                    if (!string.IsNullOrWhiteSpace(story.Role))
                    {
                        sb.AppendLine($"**Trigger:** {story.Role}");
                    }
                    if (!string.IsNullOrWhiteSpace(story.Action))
                    {
                        sb.AppendLine($"**Description:** {story.Action}");
                    }
                    if (!string.IsNullOrWhiteSpace(story.Benefit))
                    {
                        sb.AppendLine($"**Benefit:** {story.Benefit}");
                    }
                    sb.AppendLine();

                    if (story.AcceptanceCriteria.Any())
                    {
                        sb.AppendLine("**Business Rules:**");
                        foreach (var criteria in story.AcceptanceCriteria)
                        {
                            sb.AppendLine($"- {criteria}");
                        }
                        sb.AppendLine();
                    }

                    if (!string.IsNullOrWhiteSpace(story.SourceLocation))
                    {
                        sb.AppendLine($"*Source: {story.SourceLocation}*");
                        sb.AppendLine();
                    }
                }
            }

            // Features
            if (businessLogic.Features.Any())
            {
                sb.AppendLine("### Features");
                sb.AppendLine();

                foreach (var feature in businessLogic.Features)
                {
                    sb.AppendLine($"#### {feature.Id}: {feature.Name}");
                    sb.AppendLine();
                    if (!string.IsNullOrWhiteSpace(feature.Description))
                    {
                        sb.AppendLine($"**Description:** {feature.Description}");
                        sb.AppendLine();
                    }

                    if (feature.BusinessRules.Any())
                    {
                        sb.AppendLine("**Business Rules:**");
                        foreach (var rule in feature.BusinessRules)
                        {
                            sb.AppendLine($"- {rule}");
                        }
                        sb.AppendLine();
                    }

                    if (feature.Inputs.Any())
                    {
                        sb.AppendLine("**Inputs:**");
                        foreach (var input in feature.Inputs)
                        {
                            sb.AppendLine($"- {input}");
                        }
                        sb.AppendLine();
                    }

                    if (feature.Outputs.Any())
                    {
                        sb.AppendLine("**Outputs:**");
                        foreach (var output in feature.Outputs)
                        {
                            sb.AppendLine($"- {output}");
                        }
                        sb.AppendLine();
                    }

                    if (feature.ProcessingSteps.Any())
                    {
                        sb.AppendLine("**Processing Steps:**");
                        for (int i = 0; i < feature.ProcessingSteps.Count; i++)
                        {
                            sb.AppendLine($"{i + 1}. {feature.ProcessingSteps[i]}");
                        }
                        sb.AppendLine();
                    }

                    if (!string.IsNullOrWhiteSpace(feature.SourceLocation))
                    {
                        sb.AppendLine($"*Source: {feature.SourceLocation}*");
                        sb.AppendLine();
                    }
                }
            }

            // Business Rules
            if (businessLogic.BusinessRules.Any())
            {
                sb.AppendLine("### Business Rules");
                sb.AppendLine();

                foreach (var rule in businessLogic.BusinessRules)
                {
                    sb.AppendLine($"- **{rule.Id}**: {rule.Description}");
                    if (!string.IsNullOrWhiteSpace(rule.Condition))
                    {
                        sb.AppendLine($"  - Condition: {rule.Condition}");
                    }
                    if (!string.IsNullOrWhiteSpace(rule.Action))
                    {
                        sb.AppendLine($"  - Action: {rule.Action}");
                    }
                }
                sb.AppendLine();
            }

            sb.AppendLine("---");
            sb.AppendLine();
        }

        // Add technical analysis section
        sb.AppendLine("---");
        sb.AppendLine();
        sb.AppendLine("## Technical Analysis");
        sb.AppendLine();

        foreach (var analysis in result.TechnicalAnalyses)
        {
            var fileTypeLabel = analysis.IsCopybook ? " [Copybook]" : "";
            sb.AppendLine($"### {analysis.FileName}{fileTypeLabel}");
            sb.AppendLine();

            if (!string.IsNullOrWhiteSpace(analysis.ProgramDescription))
            {
                sb.AppendLine($"**Program Description:** {analysis.ProgramDescription}");
                sb.AppendLine();
            }

            if (analysis.DataDivisions.Any())
            {
                sb.AppendLine("**Data Divisions:**");
                foreach (var div in analysis.DataDivisions)
                {
                    sb.AppendLine($"- {div}");
                }
                sb.AppendLine();
            }

            if (analysis.ProcedureDivisions.Any())
            {
                sb.AppendLine("**Procedure Divisions:**");
                foreach (var div in analysis.ProcedureDivisions)
                {
                    sb.AppendLine($"- {div}");
                }
                sb.AppendLine();
            }

            if (analysis.CopybooksReferenced.Any())
            {
                sb.AppendLine("**Copybooks Referenced:**");
                foreach (var copybook in analysis.CopybooksReferenced)
                {
                    sb.AppendLine($"- {copybook}");
                }
                sb.AppendLine();
            }

            if (analysis.Paragraphs.Any())
            {
                sb.AppendLine("**Paragraphs:**");
                foreach (var para in analysis.Paragraphs.Take(20)) // Limit for large files
                {
                    sb.AppendLine($"- {para.Name}: {para.Description}");
                }
                if (analysis.Paragraphs.Count > 20)
                {
                    sb.AppendLine($"- ... and {analysis.Paragraphs.Count - 20} more");
                }
                sb.AppendLine();
            }

            sb.AppendLine("---");
            sb.AppendLine();
        }

        return sb.ToString();
    }
}
