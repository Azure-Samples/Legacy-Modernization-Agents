using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Agents.Infrastructure;
using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Helpers;
using System.Diagnostics;

namespace CobolToQuarkusMigration.Agents;

/// <summary>
/// Agent that extracts business logic from COBOL code using Microsoft Agent Framework (IChatClient).
/// </summary>
public class BusinessLogicExtractorAgent : AgentBase
{
    /// <inheritdoc/>
    protected override string AgentName => "BusinessLogicExtractorAgent";

    public BusinessLogicExtractorAgent(
        IChatClient chatClient,
        ILogger<BusinessLogicExtractorAgent> logger,
        string modelId,
        EnhancedLogger? enhancedLogger = null,
        ChatLogger? chatLogger = null,
        RateLimiter? rateLimiter = null,
        AppSettings? settings = null)
        : base(chatClient, logger, modelId, enhancedLogger, chatLogger, rateLimiter, settings)
    {
    }

    /// <summary>
    /// Extracts business logic from a COBOL file.
    /// </summary>
    public async Task<BusinessLogic> ExtractBusinessLogicAsync(CobolFile cobolFile, CobolAnalysis analysis, Glossary? glossary = null)
    {
        var stopwatch = Stopwatch.StartNew();

        Logger.LogInformation("Extracting business logic from: {FileName}", cobolFile.FileName);
        EnhancedLogger?.LogBehindTheScenes("REVERSE_ENGINEERING", "BUSINESS_LOGIC_EXTRACTION_START",
            $"Starting business logic extraction for {cobolFile.FileName}", cobolFile.FileName);

        try
        {
            var systemPrompt = @"
You are a business analyst extracting business logic from COBOL code.
Focus on identifying business use cases, operations, and validation rules.
Use business-friendly terminology from the provided glossary when available.
";

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

            // Check file size
            const int MaxContentChars = 150_000;
            var contentToAnalyze = cobolFile.Content;

            if (contentToAnalyze.Length > MaxContentChars)
            {
                var errorMsg = $"❌ FILE TOO LARGE: {cobolFile.FileName} has {contentToAnalyze.Length:N0} chars (max: {MaxContentChars:N0}).";
                Logger.LogError(errorMsg);
                return new BusinessLogic
                {
                    FileName = cobolFile.FileName,
                    FilePath = cobolFile.FilePath,
                    BusinessPurpose = errorMsg
                };
            }

            var userPrompt = $@"
Analyze this COBOL program and extract the business logic:
Your goal: Identify WHAT the business does, not HOW the code works.{glossaryContext}

## What to Extract:

### 1. Use Cases / Operations
Identify each business operation the program performs:
- CREATE / Register / Add operations
- UPDATE / Change / Modify operations  
- DELETE / Remove operations
- READ / Query / Fetch operations
- VALIDATE / Check operations

### 2. Validations as Business Rules
Extract ALL validation rules including:
- Field validations (required, format, length, range)
- Business logic validations
- Error codes and their meanings

### 3. Business Purpose
What business problem does this solve? (1-2 sentences)

## Format Your Response:

## Business Purpose
[1-2 sentences]

## Use Cases
### Use Case 1: [Operation Name]
**Trigger:** [What initiates this operation]
**Description:** [What happens]
**Key Steps:**
1. [Step 1]
2. [Step 2]

## Business Rules & Validations
### Data Validations
- [Field name] must be [requirement] - Error: [code/message]

### Business Logic Rules
- [Rule description]

File: {cobolFile.FileName}

COBOL Code:
```cobol
{contentToAnalyze}
```
";

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

            var semaphore = new SemaphoreSlim(maxParallel, maxParallel);
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
