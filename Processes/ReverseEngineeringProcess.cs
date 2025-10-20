using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Agents;
using CobolToQuarkusMigration.Agents.Interfaces;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Processes;

/// <summary>
/// Orchestrates the reverse engineering process for COBOL applications.
/// Can run standalone or as part of the full migration pipeline.
/// </summary>
public class ReverseEngineeringProcess
{
    private readonly ICobolAnalyzerAgent _cobolAnalyzerAgent;
    private readonly BusinessLogicExtractorAgent _businessLogicExtractorAgent;
    private readonly UtilityCodeAnalyzerAgent _utilityCodeAnalyzerAgent;
    private readonly FileHelper _fileHelper;
    private readonly ILogger<ReverseEngineeringProcess> _logger;
    private readonly EnhancedLogger _enhancedLogger;

    public ReverseEngineeringProcess(
        ICobolAnalyzerAgent cobolAnalyzerAgent,
        BusinessLogicExtractorAgent businessLogicExtractorAgent,
        UtilityCodeAnalyzerAgent utilityCodeAnalyzerAgent,
        FileHelper fileHelper,
        ILogger<ReverseEngineeringProcess> logger,
        EnhancedLogger enhancedLogger)
    {
        _cobolAnalyzerAgent = cobolAnalyzerAgent;
        _businessLogicExtractorAgent = businessLogicExtractorAgent;
        _utilityCodeAnalyzerAgent = utilityCodeAnalyzerAgent;
        _fileHelper = fileHelper;
        _logger = logger;
        _enhancedLogger = enhancedLogger;
    }

    /// <summary>
    /// Runs the reverse engineering process.
    /// </summary>
    /// <param name="cobolSourceFolder">The folder containing COBOL source files.</param>
    /// <param name="outputFolder">The folder for reverse engineering output.</param>
    /// <param name="progressCallback">Optional callback for progress reporting.</param>
    /// <returns>A task representing the asynchronous operation.</returns>
    public async Task<ReverseEngineeringResult> RunAsync(
        string cobolSourceFolder,
        string outputFolder,
        Action<string, int, int>? progressCallback = null)
    {
        var result = new ReverseEngineeringResult();

        try
        {
            _enhancedLogger.ShowHeader("REVERSE ENGINEERING PROCESS");
            _logger.LogInformation("Starting reverse engineering process");
            _logger.LogInformation("Source folder: {SourceFolder}", cobolSourceFolder);
            _logger.LogInformation("Output folder: {OutputFolder}", outputFolder);

            var totalSteps = 4;

            // Step 1: Scan for COBOL files
            _enhancedLogger.ShowStep(1, totalSteps, "File Discovery", "Scanning for COBOL files");
            progressCallback?.Invoke("Scanning for COBOL files", 1, totalSteps);

            var cobolFiles = _fileHelper.GetCobolFiles(cobolSourceFolder);
            _enhancedLogger.ShowSuccess($"Found {cobolFiles.Count} COBOL files");
            _logger.LogInformation("Found {Count} COBOL files", cobolFiles.Count);

            result.TotalFilesAnalyzed = cobolFiles.Count;

            if (cobolFiles.Count == 0)
            {
                _enhancedLogger.ShowWarning("No COBOL files found. Nothing to reverse engineer.");
                return result;
            }

            // Step 2: Analyze COBOL structure
            _enhancedLogger.ShowStep(2, totalSteps, "Technical Analysis", "Analyzing COBOL code structure");
            progressCallback?.Invoke("Analyzing COBOL structure", 2, totalSteps);

            var analyses = await _cobolAnalyzerAgent.AnalyzeCobolFilesAsync(
                cobolFiles,
                (processed, total) => _enhancedLogger.ShowProgress(processed, total, "files analyzed"));

            _enhancedLogger.ShowSuccess($"Completed technical analysis of {analyses.Count} files");
            result.TechnicalAnalyses = analyses;

            // Step 3: Extract business logic
            _enhancedLogger.ShowStep(3, totalSteps, "Business Logic Extraction", "Extracting user stories and features");
            progressCallback?.Invoke("Extracting business logic", 3, totalSteps);

            var businessLogicList = await _businessLogicExtractorAgent.ExtractBusinessLogicAsync(
                cobolFiles,
                analyses,
                (processed, total) => _enhancedLogger.ShowProgress(processed, total, "files processed"));

            _enhancedLogger.ShowSuccess($"Extracted business logic from {businessLogicList.Count} files");
            result.BusinessLogicExtracts = businessLogicList;

            // Count user stories and features
            result.TotalUserStories = businessLogicList.Sum(bl => bl.UserStories.Count);
            result.TotalFeatures = businessLogicList.Sum(bl => bl.Features.Count);
            result.TotalBusinessRules = businessLogicList.Sum(bl => bl.BusinessRules.Count);

            // Step 4: Analyze utility code
            _enhancedLogger.ShowStep(4, totalSteps, "Utility Code Analysis", "Identifying modernization opportunities");
            progressCallback?.Invoke("Analyzing utility code", 4, totalSteps);

            var utilityAnalyses = await _utilityCodeAnalyzerAgent.AnalyzeUtilityCodeAsync(
                cobolFiles,
                analyses,
                (processed, total) => _enhancedLogger.ShowProgress(processed, total, "files analyzed"));

            _enhancedLogger.ShowSuccess($"Completed utility code analysis for {utilityAnalyses.Count} files");
            result.UtilityCodeAnalyses = utilityAnalyses;

            // Count modernization opportunities
            result.TotalModernizationOpportunities = utilityAnalyses.Sum(ua => ua.ModernizationOpportunities.Count);

            // Generate output files
            _enhancedLogger.ShowInfo("Generating documentation...");
            await GenerateOutputAsync(outputFolder, result);

            _enhancedLogger.ShowSuccess("✓ Reverse engineering complete!");
            _enhancedLogger.ShowInfo($"Output location: {outputFolder}");

            result.Success = true;
            result.OutputFolder = outputFolder;

            return result;
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error during reverse engineering process");
            _enhancedLogger.ShowError($"Reverse engineering failed: {ex.Message}");
            result.Success = false;
            result.ErrorMessage = ex.Message;
            throw;
        }
    }

    private async Task GenerateOutputAsync(string outputFolder, ReverseEngineeringResult result)
    {
        Directory.CreateDirectory(outputFolder);

        // Generate business-logic.md
        var businessLogicContent = GenerateBusinessLogicMarkdown(result);
        var businessLogicPath = Path.Combine(outputFolder, "business-logic.md");
        await File.WriteAllTextAsync(businessLogicPath, businessLogicContent);
        _logger.LogInformation("Generated business logic documentation: {Path}", businessLogicPath);

        // Generate technical-details.md
        var technicalDetailsContent = GenerateTechnicalDetailsMarkdown(result);
        var technicalDetailsPath = Path.Combine(outputFolder, "technical-details.md");
        await File.WriteAllTextAsync(technicalDetailsPath, technicalDetailsContent);
        _logger.LogInformation("Generated technical details documentation: {Path}", technicalDetailsPath);

        // Generate summary.md
        var summaryContent = GenerateSummaryMarkdown(result);
        var summaryPath = Path.Combine(outputFolder, "summary.md");
        await File.WriteAllTextAsync(summaryPath, summaryContent);
        _logger.LogInformation("Generated summary: {Path}", summaryPath);
    }

    private string GenerateBusinessLogicMarkdown(ReverseEngineeringResult result)
    {
        var sb = new System.Text.StringBuilder();

        sb.AppendLine("# Business Logic Documentation");
        sb.AppendLine();
        sb.AppendLine($"**Generated**: {DateTime.Now:yyyy-MM-dd HH:mm:ss}");
        sb.AppendLine($"**Total Files Analyzed**: {result.TotalFilesAnalyzed}");
        sb.AppendLine($"**Total User Stories**: {result.TotalUserStories}");
        sb.AppendLine($"**Total Features**: {result.TotalFeatures}");
        sb.AppendLine($"**Total Business Rules**: {result.TotalBusinessRules}");
        sb.AppendLine();
        sb.AppendLine("---");
        sb.AppendLine();

        foreach (var businessLogic in result.BusinessLogicExtracts)
        {
            sb.AppendLine($"## {businessLogic.FileName}");
            sb.AppendLine();

            if (!string.IsNullOrWhiteSpace(businessLogic.BusinessPurpose))
            {
                sb.AppendLine("### Business Purpose");
                sb.AppendLine(businessLogic.BusinessPurpose);
                sb.AppendLine();
            }

            // User Stories
            if (businessLogic.UserStories.Any())
            {
                sb.AppendLine("### User Stories");
                sb.AppendLine();

                foreach (var story in businessLogic.UserStories)
                {
                    sb.AppendLine($"#### {story.Id}: {story.Title}");
                    sb.AppendLine();
                    if (!string.IsNullOrWhiteSpace(story.Role))
                    {
                        sb.AppendLine($"**As a** {story.Role}");
                    }
                    if (!string.IsNullOrWhiteSpace(story.Action))
                    {
                        sb.AppendLine($"**I want to** {story.Action}");
                    }
                    if (!string.IsNullOrWhiteSpace(story.Benefit))
                    {
                        sb.AppendLine($"**So that** {story.Benefit}");
                    }
                    sb.AppendLine();

                    if (story.AcceptanceCriteria.Any())
                    {
                        sb.AppendLine("**Acceptance Criteria:**");
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
                    sb.AppendLine($"#### {rule.Id}");
                    sb.AppendLine($"**Description:** {rule.Description}");
                    if (!string.IsNullOrWhiteSpace(rule.Condition))
                    {
                        sb.AppendLine($"**Condition:** {rule.Condition}");
                    }
                    if (!string.IsNullOrWhiteSpace(rule.Action))
                    {
                        sb.AppendLine($"**Action:** {rule.Action}");
                    }
                    if (!string.IsNullOrWhiteSpace(rule.SourceLocation))
                    {
                        sb.AppendLine($"*Source: {rule.SourceLocation}*");
                    }
                    sb.AppendLine();
                }
            }

            sb.AppendLine("---");
            sb.AppendLine();
        }

        return sb.ToString();
    }

    private string GenerateTechnicalDetailsMarkdown(ReverseEngineeringResult result)
    {
        var sb = new System.Text.StringBuilder();

        sb.AppendLine("# Technical Details");
        sb.AppendLine();
        sb.AppendLine($"**Generated**: {DateTime.Now:yyyy-MM-dd HH:mm:ss}");
        sb.AppendLine($"**Total Files**: {result.TotalFilesAnalyzed}");
        sb.AppendLine($"**Modernization Opportunities**: {result.TotalModernizationOpportunities}");
        sb.AppendLine();
        sb.AppendLine("---");
        sb.AppendLine();

        sb.AppendLine("## Utility Code Analysis & Modernization Opportunities");
        sb.AppendLine();

        foreach (var utilityAnalysis in result.UtilityCodeAnalyses)
        {
            sb.AppendLine($"### {utilityAnalysis.FileName}");
            sb.AppendLine();

            // Group patterns by type
            var patternsByType = utilityAnalysis.UtilityPatterns.GroupBy(p => p.Type);

            foreach (var group in patternsByType)
            {
                sb.AppendLine($"#### {group.Key} Patterns");
                sb.AppendLine();

                foreach (var pattern in group)
                {
                    sb.AppendLine($"**{pattern.Description}**");
                    sb.AppendLine($"- **Location:** {pattern.SourceLocation}");
                    sb.AppendLine($"- **Classification:** {(pattern.IsBusinessSpecific ? "Business-Specific" : "Standard Utility")}");
                    if (!string.IsNullOrWhiteSpace(pattern.ClassificationReason))
                    {
                        sb.AppendLine($"- **Reasoning:** {pattern.ClassificationReason}");
                    }
                    if (!string.IsNullOrWhiteSpace(pattern.CobolCode))
                    {
                        sb.AppendLine("```cobol");
                        sb.AppendLine(pattern.CobolCode);
                        sb.AppendLine("```");
                    }
                    sb.AppendLine();
                }
            }

            // Modernization Opportunities
            if (utilityAnalysis.ModernizationOpportunities.Any())
            {
                sb.AppendLine("#### Modernization Recommendations");
                sb.AppendLine();

                foreach (var opp in utilityAnalysis.ModernizationOpportunities)
                {
                    sb.AppendLine($"##### {opp.CurrentImplementation}");
                    sb.AppendLine($"- **Location:** {opp.SourceLocation}");
                    sb.AppendLine($"- **Modern Equivalent:** {opp.ModernEquivalent}");
                    sb.AppendLine($"- **Confidence:** {opp.Confidence}");
                    sb.AppendLine($"- **Migration Effort:** {opp.Effort}");
                    if (!string.IsNullOrWhiteSpace(opp.Reasoning))
                    {
                        sb.AppendLine($"- **Reasoning:** {opp.Reasoning}");
                    }
                    if (!string.IsNullOrWhiteSpace(opp.ExampleCode))
                    {
                        sb.AppendLine();
                        sb.AppendLine("**Example Modern Implementation:**");
                        sb.AppendLine("```java");
                        sb.AppendLine(opp.ExampleCode);
                        sb.AppendLine("```");
                    }
                    sb.AppendLine();
                }
            }

            sb.AppendLine("---");
            sb.AppendLine();
        }

        // Add technical analysis details
        sb.AppendLine("## COBOL Structure Analysis");
        sb.AppendLine();

        foreach (var analysis in result.TechnicalAnalyses)
        {
            sb.AppendLine($"### {analysis.FileName}");
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

            sb.AppendLine("---");
            sb.AppendLine();
        }

        return sb.ToString();
    }

    private string GenerateSummaryMarkdown(ReverseEngineeringResult result)
    {
        var sb = new System.Text.StringBuilder();

        sb.AppendLine("# Reverse Engineering Summary");
        sb.AppendLine();
        sb.AppendLine($"**Generated**: {DateTime.Now:yyyy-MM-dd HH:mm:ss}");
        sb.AppendLine($"**Status**: {(result.Success ? "✓ Completed Successfully" : "✗ Failed")}");
        sb.AppendLine();

        sb.AppendLine("## Overview");
        sb.AppendLine();
        sb.AppendLine($"- **Files Analyzed**: {result.TotalFilesAnalyzed}");
        sb.AppendLine($"- **User Stories Extracted**: {result.TotalUserStories}");
        sb.AppendLine($"- **Features Identified**: {result.TotalFeatures}");
        sb.AppendLine($"- **Business Rules Documented**: {result.TotalBusinessRules}");
        sb.AppendLine($"- **Modernization Opportunities**: {result.TotalModernizationOpportunities}");
        sb.AppendLine();

        sb.AppendLine("## Output Files");
        sb.AppendLine();
        sb.AppendLine("1. **business-logic.md** - User stories, features, and business rules extracted from COBOL code");
        sb.AppendLine("2. **technical-details.md** - Utility code analysis, modernization recommendations, and technical structure");
        sb.AppendLine("3. **summary.md** - This file");
        sb.AppendLine();

        sb.AppendLine("## Next Steps");
        sb.AppendLine();
        sb.AppendLine("### Option 1: Documentation Only");
        sb.AppendLine("- Review the business logic documentation");
        sb.AppendLine("- Use for RFP/contractor briefing");
        sb.AppendLine("- Plan modernization strategy");
        sb.AppendLine();

        sb.AppendLine("### Option 2: Continue with Full Migration");
        sb.AppendLine("- Review the reverse engineering output");
        sb.AppendLine("- Run the full migration process to convert to Java/Quarkus");
        sb.AppendLine("- Use business logic docs for validation");
        sb.AppendLine();

        sb.AppendLine("### Option 3: Selective Modernization");
        sb.AppendLine("- Review modernization opportunities");
        sb.AppendLine("- Prioritize based on confidence and effort");
        sb.AppendLine("- Implement high-value, low-effort improvements first");
        sb.AppendLine();

        return sb.ToString();
    }
}

/// <summary>
/// Result of the reverse engineering process.
/// </summary>
public class ReverseEngineeringResult
{
    public bool Success { get; set; }
    public string ErrorMessage { get; set; } = string.Empty;
    public string OutputFolder { get; set; } = string.Empty;
    public int TotalFilesAnalyzed { get; set; }
    public int TotalUserStories { get; set; }
    public int TotalFeatures { get; set; }
    public int TotalBusinessRules { get; set; }
    public int TotalModernizationOpportunities { get; set; }
    public List<CobolAnalysis> TechnicalAnalyses { get; set; } = new List<CobolAnalysis>();
    public List<BusinessLogic> BusinessLogicExtracts { get; set; } = new List<BusinessLogic>();
    public List<UtilityCodeAnalysis> UtilityCodeAnalyses { get; set; } = new List<UtilityCodeAnalysis>();
}
