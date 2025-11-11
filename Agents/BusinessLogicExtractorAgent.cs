using Microsoft.Extensions.Logging;
using Microsoft.SemanticKernel;
using Microsoft.SemanticKernel.Connectors.OpenAI;
using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Helpers;
using System.Diagnostics;

namespace CobolToQuarkusMigration.Agents;

/// <summary>
/// Agent that extracts business logic from COBOL code and generates feature descriptions and use cases.
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
    public async Task<BusinessLogic> ExtractBusinessLogicAsync(CobolFile cobolFile, CobolAnalysis analysis, Glossary? glossary = null)
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
You are a business analyst extracting business logic from COBOL code.
Focus on identifying business use cases, operations, and validation rules.
Use business-friendly terminology from the provided glossary when available.
";

            // Build glossary context if available
            var glossaryContext = "";
            if (glossary?.Terms?.Any() == true)
            {
                glossaryContext = "\n\n## Business Glossary\nUse these business-friendly translations when describing the code:\n";
                foreach (var term in glossary.Terms)
                {
                    glossaryContext += $"- {term.Term} = {term.Translation}\n";
                }
                glossaryContext += "\n";
            }

            var prompt = $@"
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
- Any other business operations
- Describe each operation as a use case.

For each use case, describe:
- What is being created/updated/deleted
- What triggers this operation
- What are the key steps

### 2. Validations as Business Rules
Extract ALL validation rules including:
- Field validations (required, format, length, range)
- Business logic validations (eligibility, authorization, state)
- Data integrity checks (duplicates, referential integrity)
- Authorization checks (user permissions, access control)
- Error codes (IDFEL) and their meanings
- Error messages (BEFEL) and conditions

### 3. Business Purpose
What business problem does this solve? (1-2 sentences)

## Format Your Response:

## Business Purpose
[1-2 sentences describing what this does for the business]

## Use Cases

### Use Case 1: [Operation Name - e.g., Create Variant Family]
**Trigger:** [What initiates this operation]
**Description:** [What happens in this use case]
**Key Steps:**
1. [Step 1]
2. [Step 2]
...

### Use Case 2: [Operation Name - e.g., Update Variant Family]
[Same format as above]

## Business Rules & Validations

### Data Validations
- [Field name] must be [requirement] - Error: [code/message]
- [Field name] cannot be [constraint] - Error: [code/message]

### Business Logic Rules
- [Rule description with condition] - Error: [code/message]
- [Rule description with action]

### Authorization Rules
- [Permission/access requirement]

### Data Integrity Rules
- [Referential integrity or consistency rule]

Focus on actual rules and operations found in the code. Don't invent rules that aren't there.
File: {cobolFile.FileName}

COBOL Code:
```cobol
{cobolFile.Content}
```


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
        Glossary? glossary = null,
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

            var businessLogic = await ExtractBusinessLogicAsync(cobolFile, analysis, glossary);
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
            IsCopybook = cobolFile.IsCopybook,
            BusinessPurpose = ExtractBusinessPurpose(analysisText)
        };

        // Parse feature descriptions, features, rules, and entities
        businessLogic.UserStories = ExtractUserStories(analysisText, cobolFile.FileName);
        businessLogic.Features = ExtractFeatures(analysisText, cobolFile.FileName);
        businessLogic.BusinessRules = ExtractBusinessRules(analysisText, cobolFile.FileName);
        // TODO: Extract data entities when DataEntity model is defined
        // businessLogic.DataEntities = ExtractDataEntities(analysisText);

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
                    line.Contains("Feature Descriptions", StringComparison.OrdinalIgnoreCase) ||
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

        UserStory? currentStory = null;
        string currentSection = "";
        var descriptionLines = new List<string>();
        var stepLines = new List<string>();

        for (int i = 0; i < lines.Length; i++)
        {
            var line = lines[i].Trim();

            // Match "### Use Case X:" pattern from new prompt format
            if (line.StartsWith("###") && (line.Contains("Use Case", StringComparison.OrdinalIgnoreCase) ||
                                          line.Contains("Feature Description", StringComparison.OrdinalIgnoreCase) ||
                                          line.Contains("User Story", StringComparison.OrdinalIgnoreCase)))
            {
                if (currentStory != null)
                {
                    // Finalize previous story
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
                // Look for new format fields
                if (line.StartsWith("**Trigger:**", StringComparison.OrdinalIgnoreCase))
                {
                    currentStory.Role = line.Replace("**Trigger:**", "").Replace("Trigger:", "").Trim();
                    currentSection = "trigger";
                }
                else if (line.StartsWith("**Description:**", StringComparison.OrdinalIgnoreCase))
                {
                    descriptionLines.Add(line.Replace("**Description:**", "").Replace("Description:", "").Trim());
                    currentSection = "description";
                }
                else if (line.StartsWith("**Key Steps:**", StringComparison.OrdinalIgnoreCase) ||
                         line.StartsWith("Key Steps:", StringComparison.OrdinalIgnoreCase))
                {
                    currentSection = "steps";
                }
                // Old format support
                else if (line.StartsWith("Role:", StringComparison.OrdinalIgnoreCase) ||
                    line.StartsWith("As a", StringComparison.OrdinalIgnoreCase))
                {
                    currentStory.Role = line.Replace("Role:", "").Replace("As a", "").Trim();
                }
                else if (line.StartsWith("Action:", StringComparison.OrdinalIgnoreCase) ||
                         line.StartsWith("I want to", StringComparison.OrdinalIgnoreCase))
                {
                    currentStory.Action = line.Replace("Action:", "").Replace("I want to", "").Trim();
                }
                else if (line.StartsWith("Benefit:", StringComparison.OrdinalIgnoreCase) ||
                         line.StartsWith("So that", StringComparison.OrdinalIgnoreCase))
                {
                    currentStory.Benefit = line.Replace("Benefit:", "").Replace("So that", "").Trim();
                }
                else if (line.StartsWith("Acceptance Criteria", StringComparison.OrdinalIgnoreCase) ||
                         line.StartsWith("Business Rules", StringComparison.OrdinalIgnoreCase))
                {
                    currentSection = "criteria";
                }
                // Collect content based on current section
                else if (!string.IsNullOrWhiteSpace(line) && !line.StartsWith("##"))
                {
                    if (currentSection == "description" && !line.StartsWith("**"))
                    {
                        descriptionLines.Add(line);
                    }
                    else if ((currentSection == "steps" || currentSection == "criteria") &&
                             (line.StartsWith("-") || line.StartsWith("•") || char.IsDigit(line[0])))
                    {
                        stepLines.Add(line.TrimStart('-', '•', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '.', ' ').Trim());
                    }
                }
            }
        }

        // Don't forget the last story
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
        string currentCategory = "";

        for (int i = 0; i < lines.Length; i++)
        {
            var line = lines[i].Trim();

            // Start of Business Rules section - look for several possible formats
            if ((line.Contains("Business Rules", StringComparison.OrdinalIgnoreCase) &&
                line.Contains("Validations", StringComparison.OrdinalIgnoreCase)) ||
                (line.StartsWith("##") && line.Contains("Business Rules", StringComparison.OrdinalIgnoreCase)) ||
                (line.Contains("Data Validations", StringComparison.OrdinalIgnoreCase) && line.StartsWith("###")) ||
                (line.Contains("Business Logic Rules", StringComparison.OrdinalIgnoreCase) && line.StartsWith("###")))
            {
                inRulesSection = true;
                if (line.StartsWith("###"))
                {
                    currentCategory = line.Replace("###", "").Trim();
                }
                continue;
            }

            if (inRulesSection)
            {
                // Check for end of rules section (next major section or source marker)
                if ((line.StartsWith("##") && !line.Contains("Business Rules", StringComparison.OrdinalIgnoreCase) &&
                     !line.Contains("Validations", StringComparison.OrdinalIgnoreCase)) ||
                    line.Contains("*Source:", StringComparison.OrdinalIgnoreCase))
                {
                    break;
                }

                // Track sub-categories
                if (line.StartsWith("###"))
                {
                    currentCategory = line.Replace("###", "").Trim();
                    continue;
                }

                // Extract rules (lines starting with -, •, **, or specific patterns)
                if (!string.IsNullOrWhiteSpace(line) &&
                    (line.StartsWith("-") || line.StartsWith("•") || line.StartsWith("**")))
                {
                    var ruleText = line.TrimStart('-', '•', ' ').Trim();

                    // Handle **Field:** pattern
                    if (ruleText.StartsWith("**"))
                    {
                        ruleText = ruleText.Replace("**", "").Trim();
                    }

                    if (!string.IsNullOrWhiteSpace(ruleText) && ruleText.Length > 3)
                    {
                        var rule = new BusinessRule
                        {
                            Id = $"BR-{rules.Count + 1}",
                            Description = ruleText,
                            SourceLocation = fileName
                        };

                        // Add category as a condition if available
                        if (!string.IsNullOrWhiteSpace(currentCategory))
                        {
                            rule.Condition = $"[{currentCategory}]";
                        }

                        rules.Add(rule);
                    }
                }
            }
        }

        return rules;
    }

    // TODO: Implement when DataEntity model is defined
    // private List<DataEntity> ExtractDataEntities(string analysisText)
    // {
    //     var entities = new List<DataEntity>();
    //     // Basic extraction - can be enhanced based on needs
    //     // This would parse data entity sections from the AI response
    //     return entities;
    // }
}
