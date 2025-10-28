using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Agents;
using CobolToQuarkusMigration.Agents.Interfaces;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using System.Text.Json;

namespace CobolToQuarkusMigration.Processes;

/// <summary>
/// Orchestrates the reverse engineering process for COBOL applications.
/// Can run standalone or as part of the full migration pipeline.
/// </summary>
public class ReverseEngineeringProcess
{
    private readonly ICobolAnalyzerAgent _cobolAnalyzerAgent;
    private readonly BusinessLogicExtractorAgent _businessLogicExtractorAgent;
    private readonly FileHelper _fileHelper;
    private readonly ILogger<ReverseEngineeringProcess> _logger;
    private readonly EnhancedLogger _enhancedLogger;
    private Glossary? _glossary;

    public ReverseEngineeringProcess(
        ICobolAnalyzerAgent cobolAnalyzerAgent,
        BusinessLogicExtractorAgent businessLogicExtractorAgent,
        FileHelper fileHelper,
        ILogger<ReverseEngineeringProcess> logger,
        EnhancedLogger enhancedLogger)
    {
        _cobolAnalyzerAgent = cobolAnalyzerAgent;
        _businessLogicExtractorAgent = businessLogicExtractorAgent;
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
            _enhancedLogger.ShowSectionHeader("REVERSE ENGINEERING PROCESS", "Extracting Business Logic and Technical Details");
            _logger.LogInformation("Starting reverse engineering process");
            _logger.LogInformation("Source folder: {SourceFolder}", cobolSourceFolder);
            _logger.LogInformation("Output folder: {OutputFolder}", outputFolder);

            // Load glossary if available
            await LoadGlossaryAsync();

            var totalSteps = 3;

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

            // Step 2: Analyze COBOL structure
            _enhancedLogger.ShowStep(2, totalSteps, "Technical Analysis", "Analyzing COBOL code structure");
            progressCallback?.Invoke("Analyzing COBOL structure", 2, totalSteps);

            var analyses = await _cobolAnalyzerAgent.AnalyzeCobolFilesAsync(
                cobolFiles,
                (processed, total) => _enhancedLogger.ShowProgressBar(processed, total, "files analyzed"));

            _enhancedLogger.ShowSuccess($"Completed technical analysis of {analyses.Count} files");
            result.TechnicalAnalyses = analyses;

            // Step 3: Extract business logic
            _enhancedLogger.ShowStep(3, totalSteps, "Business Logic Extraction", "Extracting feature descriptions and use cases");
            progressCallback?.Invoke("Extracting business logic", 3, totalSteps);

            var businessLogicList = await _businessLogicExtractorAgent.ExtractBusinessLogicAsync(
                cobolFiles,
                analyses,
                _glossary,
                (processed, total) => _enhancedLogger.ShowProgressBar(processed, total, "files processed"));

            _enhancedLogger.ShowSuccess($"Extracted business logic from {businessLogicList.Count} files");
            result.BusinessLogicExtracts = businessLogicList;

            // Count feature descriptions and features
            result.TotalUserStories = businessLogicList.Sum(bl => bl.UserStories.Count);
            result.TotalFeatures = businessLogicList.Sum(bl => bl.Features.Count);
            result.TotalBusinessRules = businessLogicList.Sum(bl => bl.BusinessRules.Count);

            // Generate output files
            _logger.LogInformation("Generating documentation...");
            Console.WriteLine("📝 Generating documentation...");
            await GenerateOutputAsync(outputFolder, result);

            _enhancedLogger.ShowSuccess("✓ Reverse engineering complete!");
            _logger.LogInformation("Output location: {OutputFolder}", outputFolder);
            Console.WriteLine($"📂 Output location: {outputFolder}");

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

            // Feature Descriptions
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

            sb.AppendLine("---");
            sb.AppendLine();
        }

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
}
