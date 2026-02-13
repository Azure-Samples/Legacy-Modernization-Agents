using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Agents.Infrastructure;
using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Persistence;
using CobolToQuarkusMigration.Chunking;
using CobolToQuarkusMigration.Chunking.Models;
using System.Diagnostics;

using System.Text.Json;

namespace CobolToQuarkusMigration.Agents;

/// <summary>
/// Agent that extracts business logic from COBOL code using Microsoft Agent Framework (IChatClient).
/// </summary>
public class BusinessLogicExtractorAgent : AgentBase
{
    private readonly ChunkingOrchestrator? _chunkingOrchestrator;

    /// <inheritdoc/>
    protected override string AgentName => "BusinessLogicExtractorAgent";

    public BusinessLogicExtractorAgent(
        IChatClient chatClient,
        ILogger<BusinessLogicExtractorAgent> logger,
        string modelId,
        EnhancedLogger? enhancedLogger = null,
        ChatLogger? chatLogger = null,
        RateLimiter? rateLimiter = null,
        AppSettings? settings = null,
        ChunkingOrchestrator? chunkingOrchestrator = null)
        : base(chatClient, logger, modelId, enhancedLogger, chatLogger, rateLimiter, settings)
    {
        _chunkingOrchestrator = chunkingOrchestrator;
    }

    /// <summary>
    /// Initializes a new instance using Responses API (for codex models).
    /// </summary>
    public BusinessLogicExtractorAgent(
        ResponsesApiClient responsesClient,
        ILogger<BusinessLogicExtractorAgent> logger,
        string modelId,
        EnhancedLogger? enhancedLogger = null,
        ChatLogger? chatLogger = null,
        RateLimiter? rateLimiter = null,
        AppSettings? settings = null,
        ChunkingOrchestrator? chunkingOrchestrator = null)
        : base(responsesClient, logger, modelId, enhancedLogger, chatLogger, rateLimiter, settings)
    {
        _chunkingOrchestrator = chunkingOrchestrator;
    }

    /// <summary>
    /// Extracts business logic from a COBOL file.
    /// </summary>
    public async Task<BusinessLogic> ExtractBusinessLogicAsync(CobolFile cobolFile, CobolAnalysis analysis, Glossary? glossary = null, int? runId = null)
    {
        var stopwatch = Stopwatch.StartNew();

        Logger.LogInformation("Extracting business logic from: {FileName}", cobolFile.FileName);
        EnhancedLogger?.LogBehindTheScenes("REVERSE_ENGINEERING", "BUSINESS_LOGIC_EXTRACTION_START",
            $"Starting business logic extraction for {cobolFile.FileName}", cobolFile.FileName);

        try
        {
            // Check file size (use configured threshold or default to 150K)
            int maxContentChars = Settings?.ChunkingSettings?.AutoChunkCharThreshold ?? 150_000;
            var contentToAnalyze = cobolFile.Content;

            if (contentToAnalyze.Length > maxContentChars)
            {
                // Attempt to use smart chunking if available
                if (_chunkingOrchestrator != null)
                {
                    Logger.LogInformation("File {FileName} is too large ({Chars:N0} chars). Using Smart Chunking.", cobolFile.FileName, contentToAnalyze.Length);
                    return await ExtractWithChunkingAsync(cobolFile, analysis, glossary);
                }

                var errorMsg = $"❌ FILE TOO LARGE: {cobolFile.FileName} has {contentToAnalyze.Length:N0} chars (max: {maxContentChars:N0}).";
                Logger.LogError(errorMsg);
                return new BusinessLogic
                {
                    FileName = cobolFile.FileName,
                    FilePath = cobolFile.FilePath,
                    BusinessPurpose = errorMsg
                };
            }

            var systemPrompt = PromptLoader.LoadSection("BusinessLogicExtractor", "System");

            // Build glossary context
            var glossaryContext = "";
            if (glossary?.Terms?.Any() == true)
            {
                glossaryContext = "\n\n## Business Glossary\nUse these business-friendly translations:\n";
                foreach (var term in glossary.Terms)
                {
                    glossaryContext += $"- {term.Term} = {term.Translation}\n";
                }
                glossaryContext += "\n";
            }

            var userPrompt = PromptLoader.LoadSection("BusinessLogicExtractor", "User", new Dictionary<string, string>
            {
                ["GlossaryContext"] = glossaryContext,
                ["FileName"] = cobolFile.FileName,
                ["CobolContent"] = contentToAnalyze
            });

            var (analysisText, usedFallback, fallbackReason) = await ExecuteWithFallbackAsync(
                systemPrompt,
                userPrompt,
                cobolFile.FileName);

            if (usedFallback)
            {
                return new BusinessLogic
                {
                    FileName = cobolFile.FileName,
                    FilePath = cobolFile.FilePath,
                    BusinessPurpose = $"Business logic extraction unavailable: {fallbackReason}"
                };
            }

            stopwatch.Stop();
            EnhancedLogger?.LogPerformanceMetrics($"Business Logic Extraction - {cobolFile.FileName}", stopwatch.Elapsed, 1);

            var businessLogic = ParseBusinessLogic(cobolFile, analysisText);

            Logger.LogInformation("Completed business logic extraction for: {FileName}", cobolFile.FileName);
            return businessLogic;
        }
        catch (Exception ex)
        {
            stopwatch.Stop();
            EnhancedLogger?.LogBehindTheScenes("ERROR", "BUSINESS_LOGIC_EXTRACTION_FAILED",
                $"Failed to extract business logic from {cobolFile.FileName}: {ex.Message}", ex.GetType().Name);
            Logger.LogError(ex, "Error extracting business logic from: {FileName}", cobolFile.FileName);
            throw;
        }
    }

    /// <summary>
    /// Chunking-enabled extraction method.
    /// </summary>
    private async Task<BusinessLogic> ExtractWithChunkingAsync(CobolFile cobolFile, CobolAnalysis analysis, Glossary? glossary)
    {
        if (_chunkingOrchestrator == null)
            throw new InvalidOperationException("ChunkingOrchestrator not initialized");

        Logger.LogInformation("Chunking large file {FileName} ({Chars:N0} chars)", cobolFile.FileName, cobolFile.Content.Length);

        // 1. Plan chunks
        var plan = await _chunkingOrchestrator.AnalyzeFileAsync(cobolFile.FilePath, cobolFile.Content);
        Logger.LogInformation("Created {ChunkCount} chunks for {FileName}", plan.ChunkCount, cobolFile.FileName);

        // 2. Process chunks in parallel
        var maxParallel = Math.Min(Settings?.ChunkingSettings?.MaxParallelAnalysis ?? 4, plan.ChunkCount);
        using var semaphore = new SemaphoreSlim(maxParallel, maxParallel);
        var chunkResults = new List<(int Index, BusinessLogic Logic)>();
        var lockObj = new object();

        var tasks = plan.Chunks.Select((chunk, index) => Task.Run(async () => 
        {
            await semaphore.WaitAsync();
            try
            {
                // Create virtual chunk file
                var chunkFile = new CobolFile
                {
                    FileName = $"{cobolFile.FileName}_chunk_{index + 1}",
                    FilePath = cobolFile.FilePath,
                    Content = chunk.Content,
                    IsCopybook = cobolFile.IsCopybook
                };

                // Recursively call standard extraction (will pass size check now)
                // Note: We reuse the main file analysis for now, or we could chunk analysis too.
                // Reusing main analysis is safer if it exists, but usually large file analysis fails too.
                // Ideally we should Chunk Analysis too, but BusinessLogicExtractorAgent doesn't own CobolAnalyzerAgent.
                // We will proceed with main analysis (it might be empty or basic)
                var chunkLogic = await ExtractBusinessLogicAsync(chunkFile, analysis, glossary);
                
                lock (lockObj)
                {
                    chunkResults.Add((index, chunkLogic));
                }
            }
            finally
            {
                semaphore.Release();
            }
        }));

        await Task.WhenAll(tasks);

        // 3. Merge results
        var orderedLogics = chunkResults.OrderBy(x => x.Index).Select(x => x.Logic).ToList();
        return MergeChunkedLogic(cobolFile, orderedLogics);
    }

    private BusinessLogic MergeChunkedLogic(CobolFile originalFile, List<BusinessLogic> chunkLogics)
    {
        var merged = new BusinessLogic
        {
            FileName = originalFile.FileName,
            FilePath = originalFile.FilePath,
            IsCopybook = originalFile.IsCopybook,
            BusinessPurpose = $"Extracted from {chunkLogics.Count} chunks. " +
                string.Join(" ", chunkLogics
                    .Select(bl => bl.BusinessPurpose)
                    .Where(p => !string.IsNullOrWhiteSpace(p) && !p.Contains("ERROR"))
                    .Distinct())
        };

        int storyId = 1, featureId = 1, ruleId = 1;

        foreach (var chunk in chunkLogics)
        {
            foreach (var story in chunk.UserStories)
            {
                story.Id = $"US-{storyId++}";
                merged.UserStories.Add(story);
            }
            foreach (var feature in chunk.Features)
            {
                feature.Id = $"F-{featureId++}";
                merged.Features.Add(feature);
            }
            foreach (var rule in chunk.BusinessRules)
            {
                if (!merged.BusinessRules.Any(r => r.Description == rule.Description))
                {
                    rule.Id = $"BR-{ruleId++}";
                    merged.BusinessRules.Add(rule);
                }
            }
        }

        return merged;
    }

    /// <summary>
    /// Extracts business logic from multiple COBOL files.
    /// </summary>
    public async Task<List<BusinessLogic>> ExtractBusinessLogicAsync(
        List<CobolFile> cobolFiles,
        List<CobolAnalysis> analyses,
        Glossary? glossary = null,
        Action<int, int>? progressCallback = null)
    {
        Logger.LogInformation("Extracting business logic from {Count} COBOL files", cobolFiles.Count);

        int processedCount = 0;
        var lockObj = new object();

        var maxParallel = Math.Min(Settings?.ChunkingSettings?.MaxParallelAnalysis ?? 6, cobolFiles.Count);
        var enableParallel = Settings?.ChunkingSettings?.EnableParallelProcessing ?? true;

        if (enableParallel && cobolFiles.Count > 1 && maxParallel > 1)
        {
            Logger.LogInformation("🚀 Using parallel extraction with {Workers} workers", maxParallel);

            using var semaphore = new SemaphoreSlim(maxParallel, maxParallel);
            var staggerDelay = Settings?.ChunkingSettings?.ParallelStaggerDelayMs ?? 500;

            var indexedTasks = new List<Task<(int Index, BusinessLogic? Logic)>>();

            for (int i = 0; i < cobolFiles.Count; i++)
            {
                var cobolFile = cobolFiles[i];
                var index = i;
                var analysis = analyses.FirstOrDefault(a => a.FileName == cobolFile.FileName);

                if (analysis == null)
                {
                    Logger.LogWarning("No analysis found for {FileName}", cobolFile.FileName);
                    indexedTasks.Add(Task.FromResult<(int Index, BusinessLogic? Logic)>((index, null)));
                    continue;
                }

                var task = Task.Run(async () =>
                {
                    await semaphore.WaitAsync();
                    try
                    {
                        var businessLogic = await ExtractBusinessLogicAsync(cobolFile, analysis, glossary);
                        lock (lockObj)
                        {
                            processedCount++;
                            progressCallback?.Invoke(processedCount, cobolFiles.Count);
                        }
                        return (Index: index, Logic: (BusinessLogic?)businessLogic);
                    }
                    finally
                    {
                        semaphore.Release();
                    }
                });

                indexedTasks.Add(task);
                await Task.Delay(staggerDelay);
            }

            var results = await Task.WhenAll(indexedTasks);

            return results
                .OrderBy(r => r.Index)
                .Where(r => r.Logic != null)
                .Select(r => r.Logic!)
                .ToList();
        }
        else
        {
            var businessLogicList = new List<BusinessLogic>();

            foreach (var cobolFile in cobolFiles)
            {
                var analysis = analyses.FirstOrDefault(a => a.FileName == cobolFile.FileName);
                if (analysis == null)
                {
                    Logger.LogWarning("No analysis found for {FileName}", cobolFile.FileName);
                    continue;
                }

                var businessLogic = await ExtractBusinessLogicAsync(cobolFile, analysis, glossary);
                businessLogicList.Add(businessLogic);

                processedCount++;
                progressCallback?.Invoke(processedCount, cobolFiles.Count);
            }

            return businessLogicList;
        }
    }

    private BusinessLogic ParseBusinessLogic(CobolFile cobolFile, string analysisText)
    {
        var businessLogic = new BusinessLogic
        {
            FileName = cobolFile.FileName,
            FilePath = cobolFile.FilePath,
            IsCopybook = cobolFile.IsCopybook,
            BusinessPurpose = ExtractBusinessPurpose(analysisText)
        };

        businessLogic.UserStories = ExtractUserStories(analysisText, cobolFile.FileName);
        businessLogic.Features = ExtractFeatures(analysisText, cobolFile.FileName);
        businessLogic.BusinessRules = ExtractBusinessRules(analysisText, cobolFile.FileName);

        return businessLogic;
    }

    private string ExtractBusinessPurpose(string analysisText)
    {
        var lines = analysisText.Split('\n');
        var purposeSection = new List<string>();
        bool inPurposeSection = false;

        foreach (var line in lines)
        {
            if (line.Contains("Business Purpose", StringComparison.OrdinalIgnoreCase))
            {
                inPurposeSection = true;
                continue;
            }

            if (inPurposeSection)
            {
                if (string.IsNullOrWhiteSpace(line) ||
                    line.Contains("Use Cases", StringComparison.OrdinalIgnoreCase) ||
                    line.Contains("Features", StringComparison.OrdinalIgnoreCase))
                {
                    break;
                }
                purposeSection.Add(line.Trim());
            }
        }

        return string.Join(" ", purposeSection).Trim();
    }

    private List<UserStory> ExtractUserStories(string analysisText, string fileName)
    {
        var stories = new List<UserStory>();
        var lines = analysisText.Split('\n');

        UserStory? currentStory = null;
        string currentSection = "";
        var descriptionLines = new List<string>();
        var stepLines = new List<string>();

        for (int i = 0; i < lines.Length; i++)
        {
            var line = lines[i].Trim();

            if (line.StartsWith("###") && (line.Contains("Use Case", StringComparison.OrdinalIgnoreCase) ||
                                          line.Contains("User Story", StringComparison.OrdinalIgnoreCase)))
            {
                if (currentStory != null)
                {
                    if (descriptionLines.Count > 0)
                        currentStory.Action = string.Join(" ", descriptionLines);
                    if (stepLines.Count > 0)
                        currentStory.AcceptanceCriteria.AddRange(stepLines);
                    stories.Add(currentStory);
                }

                currentStory = new UserStory
                {
                    Id = $"US-{stories.Count + 1}",
                    Title = line.Replace("###", "").Trim(':').Trim(),
                    SourceLocation = fileName
                };
                currentSection = "title";
                descriptionLines.Clear();
                stepLines.Clear();
            }
            else if (currentStory != null)
            {
                if (line.StartsWith("**Trigger:**", StringComparison.OrdinalIgnoreCase))
                {
                    currentStory.Role = line.Replace("**Trigger:**", "").Trim();
                    currentSection = "trigger";
                }
                else if (line.StartsWith("**Description:**", StringComparison.OrdinalIgnoreCase))
                {
                    descriptionLines.Add(line.Replace("**Description:**", "").Trim());
                    currentSection = "description";
                }
                else if (line.StartsWith("**Key Steps:**", StringComparison.OrdinalIgnoreCase))
                {
                    currentSection = "steps";
                }
                else if (!string.IsNullOrWhiteSpace(line) && !line.StartsWith("##"))
                {
                    if (currentSection == "description" && !line.StartsWith("**"))
                    {
                        descriptionLines.Add(line);
                    }
                    else if (currentSection == "steps" && (line.StartsWith("-") || char.IsDigit(line[0])))
                    {
                        stepLines.Add(line.TrimStart('-', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '.', ' ').Trim());
                    }
                }
            }
        }

        if (currentStory != null)
        {
            if (descriptionLines.Count > 0)
                currentStory.Action = string.Join(" ", descriptionLines);
            if (stepLines.Count > 0)
                currentStory.AcceptanceCriteria.AddRange(stepLines);
            stories.Add(currentStory);
        }

        return stories;
    }

    private List<FeatureDescription> ExtractFeatures(string analysisText, string fileName)
    {
        var features = new List<FeatureDescription>();
        var lines = analysisText.Split('\n');

        FeatureDescription? currentFeature = null;

        for (int i = 0; i < lines.Length; i++)
        {
            var line = lines[i].Trim();

            if (line.StartsWith("###") && line.Contains("Feature", StringComparison.OrdinalIgnoreCase))
            {
                if (currentFeature != null) features.Add(currentFeature);
                currentFeature = new FeatureDescription
                {
                    Id = $"F-{features.Count + 1}",
                    Name = line.Replace("###", "").Replace("Feature:", "").Trim(),
                    SourceLocation = fileName
                };
            }
            else if (currentFeature != null)
            {
                if (line.StartsWith("Description:", StringComparison.OrdinalIgnoreCase))
                {
                    currentFeature.Description = line.Replace("Description:", "").Trim();
                }
                else if (line.StartsWith("-") || line.StartsWith("•"))
                {
                    currentFeature.ProcessingSteps.Add(line.TrimStart('-', '•').Trim());
                }
            }
        }

        if (currentFeature != null) features.Add(currentFeature);
        return features;
    }

    private List<BusinessRule> ExtractBusinessRules(string analysisText, string fileName)
    {
        var rules = new List<BusinessRule>();
        var lines = analysisText.Split('\n');
        bool inRulesSection = false;

        for (int i = 0; i < lines.Length; i++)
        {
            var line = lines[i].Trim();

            if (line.Contains("Business Rules", StringComparison.OrdinalIgnoreCase) ||
                line.Contains("Validations", StringComparison.OrdinalIgnoreCase))
            {
                inRulesSection = true;
                continue;
            }

            if (inRulesSection)
            {
                if (line.StartsWith("##") && !line.Contains("Business Rules") && !line.Contains("Validations"))
                    break;

                if (!string.IsNullOrWhiteSpace(line) && (line.StartsWith("-") || line.StartsWith("•")))
                {
                    var ruleText = line.TrimStart('-', '•', ' ').Trim();
                    if (ruleText.StartsWith("**")) ruleText = ruleText.Replace("**", "").Trim();

                    if (!string.IsNullOrWhiteSpace(ruleText) && ruleText.Length > 3)
                    {
                        rules.Add(new BusinessRule
                        {
                            Id = $"BR-{rules.Count + 1}",
                            Description = ruleText,
                            SourceLocation = fileName
                        });
                    }
                }
            }
        }

        return rules;
    }
}
