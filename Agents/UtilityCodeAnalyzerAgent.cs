using Microsoft.Extensions.Logging;
using Microsoft.SemanticKernel;
using Microsoft.SemanticKernel.Connectors.OpenAI;
using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Helpers;
using System.Diagnostics;

namespace CobolToQuarkusMigration.Agents;

/// <summary>
/// Agent that analyzes COBOL code to detect utility patterns and recommend modern equivalents.
/// </summary>
public class UtilityCodeAnalyzerAgent
{
    private readonly IKernelBuilder _kernelBuilder;
    private readonly ILogger<UtilityCodeAnalyzerAgent> _logger;
    private readonly string _modelId;
    private readonly EnhancedLogger? _enhancedLogger;
    private readonly ChatLogger? _chatLogger;

    public UtilityCodeAnalyzerAgent(
        IKernelBuilder kernelBuilder,
        ILogger<UtilityCodeAnalyzerAgent> logger,
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
    /// Analyzes a COBOL file for utility code patterns and modernization opportunities.
    /// </summary>
    public async Task<UtilityCodeAnalysis> AnalyzeUtilityCodeAsync(CobolFile cobolFile, CobolAnalysis analysis)
    {
        var stopwatch = Stopwatch.StartNew();

        _logger.LogInformation("Analyzing utility code patterns in: {FileName}", cobolFile.FileName);
        _enhancedLogger?.LogBehindTheScenes("REVERSE_ENGINEERING", "UTILITY_CODE_ANALYSIS_START",
            $"Starting utility code analysis for {cobolFile.FileName}", cobolFile.FileName);

        var kernel = _kernelBuilder.Build();
        var apiCallId = 0;

        try
        {
            var systemPrompt = @"
You are an expert code modernization analyst specializing in identifying utility code patterns in legacy COBOL applications.

Your task is to analyze COBOL code and identify:

1. **Date/Time Operations**: Date formatting, date arithmetic, timestamp handling, calendar calculations
2. **String Manipulation**: String concatenation, substring operations, trimming, padding, case conversion
3. **Mathematical Operations**: Basic arithmetic, rounding, calculations that could use standard math libraries

For each pattern found, determine:
- Is it STANDARD UTILITY (generic pattern that modern libraries handle)?
- Is it BUSINESS-SPECIFIC (contains custom business rules or domain logic)?

Classification Criteria:
- **Pattern Matching**: Does it look like a common utility function?
- **Business Constants**: Does it use business-specific constants or magic numbers?
- **Domain Data**: Does it operate on domain-specific data structures or business entities?

For STANDARD UTILITY patterns, recommend:
- Modern equivalent (Java standard library, commons-lang, java.time, etc.)
- Confidence level (HIGH/MEDIUM/LOW)
- Migration effort (LOW/MEDIUM/HIGH)
- Example modern code

For BUSINESS-SPECIFIC patterns, explain:
- Why it's business-specific
- What business rules it contains
- Recommendation to migrate as-is or extract to utility class

Be specific about locations (paragraph names, line ranges) and provide concrete examples.
";

            var prompt = $@"
Analyze this COBOL program for utility code patterns:

File: {cobolFile.FileName}

COBOL Code:
```cobol
{cobolFile.Content}
```

Technical Analysis Context:
{analysis.RawAnalysisData}

Identify and classify all utility code patterns:

1. **Date/Time Operations**
   - Pattern description
   - Classification (Standard vs Business-Specific)
   - Reasoning
   - Modern equivalent (if standard)

2. **String Manipulation**
   - Pattern description
   - Classification
   - Reasoning
   - Modern equivalent (if standard)

3. **Mathematical Operations**
   - Pattern description
   - Classification
   - Reasoning
   - Modern equivalent (if standard)

For each modernization opportunity, provide:
- Current COBOL implementation (with location)
- Recommended modern approach
- Example code
- Confidence level
- Migration effort estimate
- Detailed reasoning

Be thorough and specific. Include actual code snippets and paragraph names.
";

            apiCallId = _enhancedLogger?.LogApiCallStart(
                "UtilityCodeAnalyzerAgent",
                "POST",
                "Azure OpenAI Chat Completion",
                _modelId,
                $"Analyzing utility code in {cobolFile.FileName}"
            ) ?? 0;

            _chatLogger?.LogUserMessage("UtilityCodeAnalyzerAgent", cobolFile.FileName, prompt, systemPrompt);

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

            _chatLogger?.LogAIResponse("UtilityCodeAnalyzerAgent", cobolFile.FileName, analysisText);

            stopwatch.Stop();
            _enhancedLogger?.LogApiCallEnd(apiCallId, analysisText, analysisText.Length / 4, 0.001m);
            _enhancedLogger?.LogPerformanceMetrics($"Utility Code Analysis - {cobolFile.FileName}", stopwatch.Elapsed, 1);

            // Parse the response into structured analysis
            var utilityAnalysis = ParseUtilityAnalysis(cobolFile, analysisText);

            _logger.LogInformation("Completed utility code analysis for: {FileName}", cobolFile.FileName);

            return utilityAnalysis;
        }
        catch (Exception ex)
        {
            stopwatch.Stop();

            if (apiCallId > 0)
            {
                _enhancedLogger?.LogApiCallError(apiCallId, ex.Message);
            }

            _enhancedLogger?.LogBehindTheScenes("ERROR", "UTILITY_CODE_ANALYSIS_FAILED",
                $"Failed to analyze utility code in {cobolFile.FileName}: {ex.Message}", ex.GetType().Name);

            _logger.LogError(ex, "Error analyzing utility code in: {FileName}", cobolFile.FileName);
            throw;
        }
    }

    /// <summary>
    /// Analyzes utility code patterns in multiple COBOL files.
    /// </summary>
    public async Task<List<UtilityCodeAnalysis>> AnalyzeUtilityCodeAsync(
        List<CobolFile> cobolFiles,
        List<CobolAnalysis> analyses,
        Action<int, int>? progressCallback = null)
    {
        _logger.LogInformation("Analyzing utility code in {Count} COBOL files", cobolFiles.Count);

        var analysisList = new List<UtilityCodeAnalysis>();
        int processedCount = 0;

        foreach (var cobolFile in cobolFiles)
        {
            var analysis = analyses.FirstOrDefault(a => a.FileName == cobolFile.FileName);
            if (analysis == null)
            {
                _logger.LogWarning("No technical analysis found for {FileName}, skipping utility code analysis", cobolFile.FileName);
                continue;
            }

            var utilityAnalysis = await AnalyzeUtilityCodeAsync(cobolFile, analysis);
            analysisList.Add(utilityAnalysis);

            processedCount++;
            progressCallback?.Invoke(processedCount, cobolFiles.Count);
        }

        _logger.LogInformation("Completed utility code analysis for {Count} files", analysisList.Count);

        return analysisList;
    }

    private UtilityCodeAnalysis ParseUtilityAnalysis(CobolFile cobolFile, string analysisText)
    {
        var analysis = new UtilityCodeAnalysis
        {
            FileName = cobolFile.FileName,
            FilePath = cobolFile.FilePath
        };

        // Parse utility patterns
        analysis.UtilityPatterns = ParseUtilityPatterns(analysisText);

        // Parse modernization opportunities
        analysis.ModernizationOpportunities = ParseModernizationOpportunities(analysisText);

        return analysis;
    }

    private List<UtilityCodePattern> ParseUtilityPatterns(string analysisText)
    {
        var patterns = new List<UtilityCodePattern>();
        var lines = analysisText.Split('\n');

        UtilityCodePattern? currentPattern = null;
        UtilityCodeType currentType = UtilityCodeType.Other;

        for (int i = 0; i < lines.Length; i++)
        {
            var line = lines[i].Trim();

            // Detect section headers
            if (line.Contains("Date/Time", StringComparison.OrdinalIgnoreCase))
            {
                currentType = UtilityCodeType.DateTime;
            }
            else if (line.Contains("String Manipulation", StringComparison.OrdinalIgnoreCase))
            {
                currentType = UtilityCodeType.StringManipulation;
            }
            else if (line.Contains("Mathematical", StringComparison.OrdinalIgnoreCase))
            {
                currentType = UtilityCodeType.MathematicalOperation;
            }

            // Detect pattern entries
            if (line.StartsWith("-") || line.StartsWith("•") || line.StartsWith("**Pattern"))
            {
                if (currentPattern != null)
                {
                    patterns.Add(currentPattern);
                }

                currentPattern = new UtilityCodePattern
                {
                    Type = currentType,
                    Description = line.TrimStart('-', '•', '*').Replace("Pattern:", "").Trim()
                };
            }
            else if (currentPattern != null)
            {
                if (line.StartsWith("Location:", StringComparison.OrdinalIgnoreCase))
                {
                    currentPattern.SourceLocation = line.Replace("Location:", "").Trim();
                }
                else if (line.StartsWith("Classification:", StringComparison.OrdinalIgnoreCase))
                {
                    var classification = line.Replace("Classification:", "").Trim();
                    currentPattern.IsBusinessSpecific = classification.Contains("Business-Specific", StringComparison.OrdinalIgnoreCase);
                }
                else if (line.StartsWith("Reasoning:", StringComparison.OrdinalIgnoreCase) ||
                         line.StartsWith("Reason:", StringComparison.OrdinalIgnoreCase))
                {
                    currentPattern.ClassificationReason = line.Replace("Reasoning:", "").Replace("Reason:", "").Trim();
                }
                else if (line.Contains("```cobol", StringComparison.OrdinalIgnoreCase))
                {
                    // Extract code block
                    var codeLines = new List<string>();
                    i++;
                    while (i < lines.Length && !lines[i].Contains("```"))
                    {
                        codeLines.Add(lines[i]);
                        i++;
                    }
                    currentPattern.CobolCode = string.Join("\n", codeLines);
                }
            }
        }

        if (currentPattern != null)
        {
            patterns.Add(currentPattern);
        }

        return patterns;
    }

    private List<ModernizationOpportunity> ParseModernizationOpportunities(string analysisText)
    {
        var opportunities = new List<ModernizationOpportunity>();
        var lines = analysisText.Split('\n');

        ModernizationOpportunity? currentOpp = null;

        for (int i = 0; i < lines.Length; i++)
        {
            var line = lines[i].Trim();

            // Detect opportunity headers
            if (line.StartsWith("###") && (line.Contains("Opportunity", StringComparison.OrdinalIgnoreCase) ||
                                           line.Contains("Can Replace", StringComparison.OrdinalIgnoreCase)))
            {
                if (currentOpp != null)
                {
                    opportunities.Add(currentOpp);
                }

                currentOpp = new ModernizationOpportunity
                {
                    CurrentImplementation = line.Replace("###", "").Trim()
                };
            }
            else if (currentOpp != null)
            {
                if (line.StartsWith("Location:", StringComparison.OrdinalIgnoreCase))
                {
                    currentOpp.SourceLocation = line.Replace("Location:", "").Trim();
                }
                else if (line.StartsWith("Modern Equivalent:", StringComparison.OrdinalIgnoreCase) ||
                         line.StartsWith("Recommended:", StringComparison.OrdinalIgnoreCase))
                {
                    currentOpp.ModernEquivalent = line.Replace("Modern Equivalent:", "").Replace("Recommended:", "").Trim();
                }
                else if (line.StartsWith("Confidence:", StringComparison.OrdinalIgnoreCase))
                {
                    var confidence = line.Replace("Confidence:", "").Trim().ToUpperInvariant();
                    currentOpp.Confidence = confidence switch
                    {
                        var c when c.Contains("HIGH") => ConfidenceLevel.High,
                        var c when c.Contains("LOW") => ConfidenceLevel.Low,
                        _ => ConfidenceLevel.Medium
                    };
                }
                else if (line.StartsWith("Effort:", StringComparison.OrdinalIgnoreCase) ||
                         line.StartsWith("Migration Effort:", StringComparison.OrdinalIgnoreCase))
                {
                    var effort = line.Replace("Effort:", "").Replace("Migration Effort:", "").Trim().ToUpperInvariant();
                    currentOpp.Effort = effort switch
                    {
                        var e when e.Contains("HIGH") => MigrationEffort.High,
                        var e when e.Contains("LOW") => MigrationEffort.Low,
                        _ => MigrationEffort.Medium
                    };
                }
                else if (line.StartsWith("Reasoning:", StringComparison.OrdinalIgnoreCase))
                {
                    currentOpp.Reasoning = line.Replace("Reasoning:", "").Trim();
                }
                else if (line.Contains("```java", StringComparison.OrdinalIgnoreCase))
                {
                    // Extract example code
                    var codeLines = new List<string>();
                    i++;
                    while (i < lines.Length && !lines[i].Contains("```"))
                    {
                        codeLines.Add(lines[i]);
                        i++;
                    }
                    currentOpp.ExampleCode = string.Join("\n", codeLines);
                }
            }
        }

        if (currentOpp != null)
        {
            opportunities.Add(currentOpp);
        }

        return opportunities;
    }
}
