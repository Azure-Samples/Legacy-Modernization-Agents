using Microsoft.Extensions.Logging;
using Microsoft.SemanticKernel;
using Microsoft.SemanticKernel.Connectors.OpenAI;
using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Helpers;
using System.Diagnostics;

namespace CobolToQuarkusMigration.Agents;

/// <summary>
/// Agent that extracts business logic from COBOL code and generates user stories and feature descriptions.
/// </summary>
public class BusinessLogicExtractorAgent
{
    private readonly IKernelBuilder _kernelBuilder;
    private readonly ILogger<BusinessLogicExtractorAgent> _logger;
    private readonly string _modelId;
    private readonly EnhancedLogger? _enhancedLogger;
    private readonly ChatLogger? _chatLogger;

    public BusinessLogicExtractorAgent(
        IKernelBuilder kernelBuilder, 
        ILogger<BusinessLogicExtractorAgent> logger, 
        string modelId, 
        EnhancedLogger? enhancedLogger = null, 
        ChatLogger? chatLogger = null)
    {
        _kernelBuilder = kernelBuilder;
        _logger = logger;
        _modelId = modelId;
        _enhancedLogger = enhancedLogger;
        _chatLogger = chatLogger;
    }

    /// <summary>
    /// Extracts business logic from a COBOL file.
    /// </summary>
    public async Task<BusinessLogic> ExtractBusinessLogicAsync(CobolFile cobolFile, CobolAnalysis analysis)
    {
        var stopwatch = Stopwatch.StartNew();
        
        _logger.LogInformation("Extracting business logic from: {FileName}", cobolFile.FileName);
        _enhancedLogger?.LogBehindTheScenes("REVERSE_ENGINEERING", "BUSINESS_LOGIC_EXTRACTION_START", 
            $"Starting business logic extraction for {cobolFile.FileName}", cobolFile.FileName);
        
        var kernel = _kernelBuilder.Build();
        var apiCallId = 0;
        
        try
        {
            var systemPrompt = @"
You are an expert business analyst specializing in reverse engineering COBOL applications to extract business logic.
Your task is to analyze COBOL code and identify the business purpose, user stories, and feature descriptions.

Focus on WHAT the code does from a business perspective, not HOW it does it technically.

For interactive/transactional logic, create user stories in this format:
- Title: Brief name for the story
- Role: Who performs this action (e.g., 'Sales Manager', 'System User', 'Batch Process')
- Action: What they want to do (business action, not technical)
- Benefit: Why they want to do it (business value)
- Acceptance Criteria: Observable business outcomes (Given/When/Then format)

For batch/calculation processes, create feature descriptions with:
- Name: Feature name
- Description: What it does from business perspective
- Business Rules: Key rules that govern the process
- Inputs: Business data inputs
- Outputs: Business data outputs
- Processing Steps: High-level business steps

Also identify:
- Business Rules: Conditions and actions that implement business policy
- Data Entities: Business domain objects (with their business meaning)

Be thorough but concise. Focus on extracting actual business requirements, not technical implementation details.
";

            var prompt = $@"
Analyze this COBOL program and extract the business logic:

File: {cobolFile.FileName}

COBOL Code:
```cobol
{cobolFile.Content}
```

Previous Technical Analysis:
{analysis.RawAnalysisData}

Extract and structure:
1. Overall Business Purpose (1-2 sentences)
2. User Stories (for interactive/transactional logic)
3. Feature Descriptions (for batch/calculation processes)
4. Business Rules (conditions and actions)
5. Data Entities (business domain objects with their meaning)

Provide the analysis in a structured format that can be parsed.
";

            apiCallId = _enhancedLogger?.LogApiCallStart(
                "BusinessLogicExtractorAgent", 
                "POST", 
                "Azure OpenAI Chat Completion", 
                _modelId, 
                $"Extracting business logic from {cobolFile.FileName}"
            ) ?? 0;

            _chatLogger?.LogUserMessage("BusinessLogicExtractorAgent", cobolFile.FileName, prompt, systemPrompt);

            var executionSettings = new OpenAIPromptExecutionSettings
            {
                ExtensionData = new Dictionary<string, object>
                {
                    ["max_completion_tokens"] = 32768
                }
            };

            var fullPrompt = $"{systemPrompt}\n\n{prompt}";
            var kernelArguments = new KernelArguments(executionSettings);
            
            var functionResult = await kernel.InvokePromptAsync(fullPrompt, kernelArguments);
            var analysisText = functionResult.GetValue<string>() ?? string.Empty;
            
            _chatLogger?.LogAIResponse("BusinessLogicExtractorAgent", cobolFile.FileName, analysisText);
            
            stopwatch.Stop();
            _enhancedLogger?.LogApiCallEnd(apiCallId, analysisText, analysisText.Length / 4, 0.001m);
            _enhancedLogger?.LogPerformanceMetrics($"Business Logic Extraction - {cobolFile.FileName}", stopwatch.Elapsed, 1);

            // Parse the response into structured business logic
            var businessLogic = ParseBusinessLogic(cobolFile, analysisText);
            
            _logger.LogInformation("Completed business logic extraction for: {FileName}", cobolFile.FileName);
            
            return businessLogic;
        }
        catch (Exception ex)
        {
            stopwatch.Stop();
            
            if (apiCallId > 0)
            {
                _enhancedLogger?.LogApiCallError(apiCallId, ex.Message);
            }
            
            _enhancedLogger?.LogBehindTheScenes("ERROR", "BUSINESS_LOGIC_EXTRACTION_FAILED", 
                $"Failed to extract business logic from {cobolFile.FileName}: {ex.Message}", ex.GetType().Name);
            
            _logger.LogError(ex, "Error extracting business logic from: {FileName}", cobolFile.FileName);
            throw;
        }
    }

    /// <summary>
    /// Extracts business logic from multiple COBOL files.
    /// </summary>
    public async Task<List<BusinessLogic>> ExtractBusinessLogicAsync(
        List<CobolFile> cobolFiles, 
        List<CobolAnalysis> analyses,
        Action<int, int>? progressCallback = null)
    {
        _logger.LogInformation("Extracting business logic from {Count} COBOL files", cobolFiles.Count);
        
        var businessLogicList = new List<BusinessLogic>();
        int processedCount = 0;
        
        foreach (var cobolFile in cobolFiles)
        {
            var analysis = analyses.FirstOrDefault(a => a.FileName == cobolFile.FileName);
            if (analysis == null)
            {
                _logger.LogWarning("No analysis found for {FileName}, skipping business logic extraction", cobolFile.FileName);
                continue;
            }
            
            var businessLogic = await ExtractBusinessLogicAsync(cobolFile, analysis);
            businessLogicList.Add(businessLogic);
            
            processedCount++;
            progressCallback?.Invoke(processedCount, cobolFiles.Count);
        }
        
        _logger.LogInformation("Completed business logic extraction for {Count} files", businessLogicList.Count);
        
        return businessLogicList;
    }

    private BusinessLogic ParseBusinessLogic(CobolFile cobolFile, string analysisText)
    {
        // For now, we'll store the raw analysis and do basic parsing
        // In a production system, we'd have more sophisticated parsing
        var businessLogic = new BusinessLogic
        {
            FileName = cobolFile.FileName,
            FilePath = cobolFile.FilePath,
            BusinessPurpose = ExtractBusinessPurpose(analysisText)
        };

        // Parse user stories, features, rules, and entities
        businessLogic.UserStories = ExtractUserStories(analysisText, cobolFile.FileName);
        businessLogic.Features = ExtractFeatures(analysisText, cobolFile.FileName);
        businessLogic.BusinessRules = ExtractBusinessRules(analysisText, cobolFile.FileName);
        businessLogic.DataEntities = ExtractDataEntities(analysisText);

        return businessLogic;
    }

    private string ExtractBusinessPurpose(string analysisText)
    {
        // Look for business purpose section
        var lines = analysisText.Split('\n');
        var purposeSection = new List<string>();
        bool inPurposeSection = false;

        foreach (var line in lines)
        {
            if (line.Contains("Business Purpose", StringComparison.OrdinalIgnoreCase) ||
                line.Contains("Overall Purpose", StringComparison.OrdinalIgnoreCase))
            {
                inPurposeSection = true;
                continue;
            }

            if (inPurposeSection)
            {
                if (string.IsNullOrWhiteSpace(line) || 
                    line.Contains("User Stories", StringComparison.OrdinalIgnoreCase) ||
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
        
        // Simple parsing - in production, this would be more robust
        UserStory? currentStory = null;
        string currentSection = "";

        for (int i = 0; i < lines.Length; i++)
        {
            var line = lines[i].Trim();

            if (line.StartsWith("###") || line.StartsWith("**User Story"))
            {
                if (currentStory != null)
                {
                    stories.Add(currentStory);
                }
                currentStory = new UserStory
                {
                    Id = $"US-{stories.Count + 1}",
                    Title = line.Replace("###", "").Replace("**", "").Replace("User Story", "").Trim(':').Trim(),
                    SourceLocation = fileName
                };
                currentSection = "title";
            }
            else if (currentStory != null)
            {
                if (line.StartsWith("Role:", StringComparison.OrdinalIgnoreCase) || 
                    line.StartsWith("As a", StringComparison.OrdinalIgnoreCase))
                {
                    currentStory.Role = line.Replace("Role:", "").Replace("As a", "").Trim();
                    currentSection = "role";
                }
                else if (line.StartsWith("Action:", StringComparison.OrdinalIgnoreCase) || 
                         line.StartsWith("I want to", StringComparison.OrdinalIgnoreCase))
                {
                    currentStory.Action = line.Replace("Action:", "").Replace("I want to", "").Trim();
                    currentSection = "action";
                }
                else if (line.StartsWith("Benefit:", StringComparison.OrdinalIgnoreCase) || 
                         line.StartsWith("So that", StringComparison.OrdinalIgnoreCase))
                {
                    currentStory.Benefit = line.Replace("Benefit:", "").Replace("So that", "").Trim();
                    currentSection = "benefit";
                }
                else if (line.StartsWith("Acceptance Criteria", StringComparison.OrdinalIgnoreCase))
                {
                    currentSection = "criteria";
                }
                else if (currentSection == "criteria" && line.StartsWith("-"))
                {
                    currentStory.AcceptanceCriteria.Add(line.TrimStart('-').Trim());
                }
            }
        }

        if (currentStory != null)
        {
            stories.Add(currentStory);
        }

        return stories;
    }

    private List<FeatureDescription> ExtractFeatures(string analysisText, string fileName)
    {
        var features = new List<FeatureDescription>();
        var lines = analysisText.Split('\n');
        
        FeatureDescription? currentFeature = null;
        string currentSection = "";

        for (int i = 0; i < lines.Length; i++)
        {
            var line = lines[i].Trim();

            if (line.StartsWith("###") && line.Contains("Feature", StringComparison.OrdinalIgnoreCase))
            {
                if (currentFeature != null)
                {
                    features.Add(currentFeature);
                }
                currentFeature = new FeatureDescription
                {
                    Id = $"F-{features.Count + 1}",
                    Name = line.Replace("###", "").Replace("Feature:", "").Trim(),
                    SourceLocation = fileName
                };
                currentSection = "name";
            }
            else if (currentFeature != null)
            {
                if (line.StartsWith("Description:", StringComparison.OrdinalIgnoreCase))
                {
                    currentFeature.Description = line.Replace("Description:", "").Trim();
                    currentSection = "description";
                }
                else if (line.StartsWith("Business Rules", StringComparison.OrdinalIgnoreCase))
                {
                    currentSection = "rules";
                }
                else if (line.StartsWith("Inputs:", StringComparison.OrdinalIgnoreCase))
                {
                    currentSection = "inputs";
                }
                else if (line.StartsWith("Outputs:", StringComparison.OrdinalIgnoreCase))
                {
                    currentSection = "outputs";
                }
                else if (line.StartsWith("Processing Steps", StringComparison.OrdinalIgnoreCase))
                {
                    currentSection = "steps";
                }
                else if (line.StartsWith("-") || line.StartsWith("•"))
                {
                    var item = line.TrimStart('-', '•').Trim();
                    switch (currentSection)
                    {
                        case "rules":
                            currentFeature.BusinessRules.Add(item);
                            break;
                        case "inputs":
                            currentFeature.Inputs.Add(item);
                            break;
                        case "outputs":
                            currentFeature.Outputs.Add(item);
                            break;
                        case "steps":
                            currentFeature.ProcessingSteps.Add(item);
                            break;
                    }
                }
            }
        }

        if (currentFeature != null)
        {
            features.Add(currentFeature);
        }

        return features;
    }

    private List<BusinessRule> ExtractBusinessRules(string analysisText, string fileName)
    {
        var rules = new List<BusinessRule>();
        var lines = analysisText.Split('\n');
        
        bool inRulesSection = false;

        foreach (var line in lines)
        {
            if (line.Contains("Business Rules", StringComparison.OrdinalIgnoreCase) && 
                !line.Contains("Feature", StringComparison.OrdinalIgnoreCase))
            {
                inRulesSection = true;
                continue;
            }

            if (inRulesSection)
            {
                if (line.StartsWith("##") || (string.IsNullOrWhiteSpace(line) && rules.Count > 0))
                {
                    break;
                }

                if (line.StartsWith("-") || line.StartsWith("•"))
                {
                    var ruleText = line.TrimStart('-', '•').Trim();
                    rules.Add(new BusinessRule
                    {
                        Id = $"BR-{rules.Count + 1}",
                        Description = ruleText,
                        SourceLocation = fileName
                    });
                }
            }
        }

        return rules;
    }

    private List<DataEntity> ExtractDataEntities(string analysisText)
    {
        var entities = new List<DataEntity>();
        // Basic extraction - can be enhanced based on needs
        // This would parse data entity sections from the AI response
        return entities;
    }
}
