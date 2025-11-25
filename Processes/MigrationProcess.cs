using Microsoft.Extensions.Logging;
using Microsoft.SemanticKernel;
using CobolToQuarkusMigration.Agents;
using CobolToQuarkusMigration.Agents.Interfaces;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Persistence;
using System.Text;

namespace CobolToQuarkusMigration.Processes;

/// <summary>
/// Main class for the COBOL to Java Quarkus migration process.
/// </summary>
public class MigrationProcess
{
    private readonly IKernelBuilder _kernelBuilder;
    private readonly ILogger<MigrationProcess> _logger;
    private readonly FileHelper _fileHelper;
    private readonly AppSettings _settings;
    private readonly EnhancedLogger _enhancedLogger;
    private readonly ChatLogger _chatLogger;
    private readonly IMigrationRepository _migrationRepository;
    private int? _activeRunId;

    private ICobolAnalyzerAgent? _cobolAnalyzerAgent;
    private IJavaConverterAgent? _javaConverterAgent;
    private ICodeConverterAgent? _codeConverterAgent;
    private IDependencyMapperAgent? _dependencyMapperAgent;

    /// <summary>
    /// Initializes a new instance of the <see cref="MigrationProcess"/> class.
    /// </summary>
    /// <param name="kernelBuilder">The kernel builder.</param>
    /// <param name="logger">The logger.</param>
    /// <param name="fileHelper">The file helper.</param>
    /// <param name="settings">The application settings.</param>
    public MigrationProcess(
        IKernelBuilder kernelBuilder,
        ILogger<MigrationProcess> logger,
        FileHelper fileHelper,
        AppSettings settings,
        IMigrationRepository migrationRepository)
    {
        _kernelBuilder = kernelBuilder;
        _logger = logger;
        _fileHelper = fileHelper;
        _settings = settings;
        _enhancedLogger = new EnhancedLogger(logger);
        _chatLogger = new ChatLogger(LoggerFactory.Create(builder => builder.AddConsole()).CreateLogger<ChatLogger>());
        _migrationRepository = migrationRepository;
    }

    /// <summary>
    /// Initializes the agents.
    /// </summary>
    public void InitializeAgents()
    {
        _enhancedLogger.ShowSectionHeader("INITIALIZING AI AGENTS", "Setting up COBOL migration agents with Azure OpenAI");

        var loggerFactory = LoggerFactory.Create(builder =>
        {
            builder.AddConsole();
        });

        _enhancedLogger.ShowStep(1, 3, "CobolAnalyzerAgent", "Analyzing COBOL code structure and patterns");
        _cobolAnalyzerAgent = new CobolAnalyzerAgent(
            _kernelBuilder,
            loggerFactory.CreateLogger<CobolAnalyzerAgent>(),
            _settings.AISettings.CobolAnalyzerModelId,
            _enhancedLogger,
            _chatLogger);

        // Initialize converter based on target language
        var targetLang = _settings.ApplicationSettings.TargetLanguage;
        var targetName = targetLang == TargetLanguage.CSharp ? "C#" : "Java Quarkus";

        _enhancedLogger.ShowStep(2, 3, $"{targetName}ConverterAgent", $"Converting COBOL to {targetName}");
        if (targetLang == TargetLanguage.CSharp)
        {
            _codeConverterAgent = new CSharpConverterAgent(
                _kernelBuilder,
                loggerFactory.CreateLogger<CSharpConverterAgent>(),
                _settings.AISettings.JavaConverterModelId,
                _enhancedLogger,
                _chatLogger);
        }
        else
        {
            _javaConverterAgent = new JavaConverterAgent(
                _kernelBuilder,
                loggerFactory.CreateLogger<JavaConverterAgent>(),
                _settings.AISettings.JavaConverterModelId,
                _enhancedLogger,
                _chatLogger);
            _codeConverterAgent = (ICodeConverterAgent)_javaConverterAgent;
        }

        _enhancedLogger.ShowStep(3, 3, "DependencyMapperAgent", "Mapping COBOL dependencies and generating diagrams");
        _dependencyMapperAgent = new DependencyMapperAgent(
            _kernelBuilder,
            loggerFactory.CreateLogger<DependencyMapperAgent>(),
            _settings.AISettings.DependencyMapperModelId ?? _settings.AISettings.CobolAnalyzerModelId,
            _enhancedLogger,
            _chatLogger);

        _enhancedLogger.ShowSuccess("All agents initialized successfully with API call tracking");
    }

    /// <summary>
    /// Runs the migration process.
    /// </summary>
    /// <param name="cobolSourceFolder">The folder containing COBOL source files.</param>
    /// <param name="javaOutputFolder">The folder for Java output files.</param>
    /// <param name="progressCallback">Optional callback for progress reporting.</param>
    /// <returns>A task representing the asynchronous operation.</returns>
    public async Task RunAsync(
        string cobolSourceFolder,
        string javaOutputFolder,
        Action<string, int, int>? progressCallback = null)
    {
        var migrationTargetLang = _settings.ApplicationSettings.TargetLanguage;
        var targetName = migrationTargetLang == TargetLanguage.CSharp ? "C# .NET" : "JAVA QUARKUS";
        _enhancedLogger.ShowSectionHeader($"COBOL TO {targetName} MIGRATION", "AI-Powered Legacy Code Modernization");

        _logger.LogInformation("COBOL source folder: {CobolSourceFolder}", cobolSourceFolder);
        _logger.LogInformation("{TargetLanguage} output folder: {OutputFolder}", targetName, javaOutputFolder);

        if (_cobolAnalyzerAgent == null || _codeConverterAgent == null || _dependencyMapperAgent == null)
        {
            _enhancedLogger.ShowError("Agents not initialized. Call InitializeAgents() first.");
            throw new InvalidOperationException("Agents not initialized. Call InitializeAgents() first.");
        }

        var totalSteps = 6;
        var startTime = DateTime.UtcNow;
        var runId = await _migrationRepository.StartRunAsync(cobolSourceFolder, javaOutputFolder);
        _activeRunId = runId;

        try
        {
            // Step 1: Scan the COBOL source folder for COBOL files
            _enhancedLogger.ShowStep(1, totalSteps, "File Discovery", "Scanning for COBOL programs and copybooks");
            _enhancedLogger.LogBehindTheScenes("MIGRATION", "STEP_1_START",
                $"Starting file discovery in {cobolSourceFolder}");
            progressCallback?.Invoke("Scanning for COBOL files", 1, totalSteps);

            var cobolFiles = await _fileHelper.ScanDirectoryForCobolFilesAsync(cobolSourceFolder);
            await _migrationRepository.SaveCobolFilesAsync(runId, cobolFiles);

            if (cobolFiles.Count == 0)
            {
                _enhancedLogger.LogBehindTheScenes("WARNING", "NO_FILES_FOUND",
                    $"No COBOL files discovered in {cobolSourceFolder}");
                _enhancedLogger.ShowWarning($"No COBOL files found in folder: {cobolSourceFolder}");
                await _migrationRepository.CompleteRunAsync(runId, "NoFiles", "No COBOL programs discovered");
                return;
            }

            _enhancedLogger.LogBehindTheScenes("MIGRATION", "FILES_DISCOVERED",
                $"Discovered {cobolFiles.Count} COBOL files ({cobolFiles.Count(f => f.FileName.EndsWith(".cbl"))} programs, {cobolFiles.Count(f => f.FileName.EndsWith(".cpy"))} copybooks)");
            _enhancedLogger.ShowSuccess($"Found {cobolFiles.Count} COBOL files");

            // Step 2: Analyze dependencies
            _enhancedLogger.ShowStep(2, totalSteps, "Dependency Analysis", "Mapping COBOL relationships and dependencies");
            _enhancedLogger.LogBehindTheScenes("MIGRATION", "STEP_2_START",
                "Starting AI-powered dependency analysis");
            progressCallback?.Invoke("Analyzing dependencies", 2, totalSteps);

            var dependencyMap = await _dependencyMapperAgent.AnalyzeDependenciesAsync(cobolFiles, new List<CobolAnalysis>());

            // Save dependency map and Mermaid diagram
            var dependencyMapPath = Path.Combine(javaOutputFolder, "dependency-map.json");
            var mermaidDiagramPath = Path.Combine(javaOutputFolder, "dependency-diagram.md");

            _enhancedLogger.LogBehindTheScenes("FILE_OUTPUT", "DEPENDENCY_EXPORT",
                $"Saving dependency map to {dependencyMapPath}");
            await _fileHelper.SaveDependencyMapAsync(dependencyMap, dependencyMapPath);
            await File.WriteAllTextAsync(mermaidDiagramPath, $"# COBOL Dependency Diagram\n\n```mermaid\n{dependencyMap.MermaidDiagram}\n```");
            await _migrationRepository.SaveDependencyMapAsync(runId, dependencyMap);

            _enhancedLogger.LogBehindTheScenes("MIGRATION", "DEPENDENCIES_ANALYZED",
                $"Found {dependencyMap.Dependencies.Count} dependencies, {dependencyMap.CopybookUsage.Count} copybook relationships");
            _enhancedLogger.ShowSuccess($"Dependency analysis complete - {dependencyMap.Dependencies.Count} relationships found");

            // Step 3: Analyze the COBOL files
            _enhancedLogger.ShowStep(3, totalSteps, "COBOL Analysis", "AI-powered code structure analysis");
            _enhancedLogger.LogBehindTheScenes("MIGRATION", "STEP_3_START",
                $"Starting COBOL analysis for {cobolFiles.Count} files using AI model");
            progressCallback?.Invoke("Analyzing COBOL files", 3, totalSteps);

            var cobolAnalyses = await _cobolAnalyzerAgent.AnalyzeCobolFilesAsync(
                cobolFiles,
                (current, total) =>
                {
                    _enhancedLogger.ShowProgressBar(current, total, "Analyzing COBOL files");
                    _enhancedLogger.LogBehindTheScenes("PROGRESS", "COBOL_ANALYSIS",
                        $"Analyzing file {current}/{total}");
                    progressCallback?.Invoke($"Analyzing COBOL files ({current}/{total})", 3, totalSteps);
                });
            await _migrationRepository.SaveAnalysesAsync(runId, cobolAnalyses);

            _enhancedLogger.LogBehindTheScenes("MIGRATION", "COBOL_ANALYSIS_COMPLETE",
                $"Completed analysis of {cobolAnalyses.Count} COBOL files");
            _enhancedLogger.ShowSuccess($"COBOL analysis complete - {cobolAnalyses.Count} files analyzed");

            // Step 4: Convert the COBOL files to target language
            var targetLang = _settings.ApplicationSettings.TargetLanguage;
            var langName = targetLang == TargetLanguage.CSharp ? "C#" : "Java";
            var frameworkName = targetLang == TargetLanguage.CSharp ? ".NET" : "Quarkus";

            _enhancedLogger.ShowStep(4, totalSteps, $"{langName} Conversion", $"Converting to {langName} {frameworkName} microservices");
            _enhancedLogger.LogBehindTheScenes("MIGRATION", "STEP_4_START",
                $"Starting AI-powered COBOL to {langName} conversion");
            progressCallback?.Invoke($"Converting to {langName}", 4, totalSteps);

            var codeFiles = await _codeConverterAgent.ConvertAsync(
                cobolFiles,
                cobolAnalyses,
                (current, total) =>
                {
                    _enhancedLogger.ShowProgressBar(current, total, $"Converting to {langName}");
                    _enhancedLogger.LogBehindTheScenes("PROGRESS", "CODE_CONVERSION",
                        $"Converting file {current}/{total} to {langName} {frameworkName}");
                    progressCallback?.Invoke($"Converting to {langName} ({current}/{total})", 4, totalSteps);
                });

            // Keep original CodeFile type for C# files to preserve extensions
            // Only wrap in JavaFile when target is Java
            var javaFiles = codeFiles.Select(cf =>
            {
                // CRITICAL: Check target language FIRST, before type checking
                // This prevents C# files from being wrapped in JavaFile objects
                if (targetLang == TargetLanguage.CSharp)
                {
                    // For C# target, always return as CodeFile (even if it's a JavaFile)
                    if (cf is JavaFile jf)
                    {
                        // Unwrap JavaFile to CodeFile for C# targets
                        return new CodeFile
                        {
                            FileName = jf.FileName.Replace(".java", ".cs"),
                            Content = jf.Content,
                            ClassName = jf.ClassName,
                            NamespaceName = jf.PackageName,
                            OriginalCobolFileName = jf.OriginalCobolFileName
                        };
                    }
                    return cf; // Already a CodeFile, keep it
                }

                // For Java target, wrap in JavaFile if needed
                if (cf is JavaFile javaFile) return javaFile;
                return new JavaFile
                {
                    FileName = cf.FileName,
                    Content = cf.Content,
                    ClassName = cf.ClassName,
                    PackageName = cf.NamespaceName,
                    OriginalCobolFileName = cf.OriginalCobolFileName
                };
            }).ToList();

            _enhancedLogger.LogBehindTheScenes("MIGRATION", "CODE_CONVERSION_COMPLETE",
                $"Generated {codeFiles.Count} {langName} files from COBOL sources");
            _enhancedLogger.ShowSuccess($"{langName} conversion complete - {codeFiles.Count} {langName} files generated");

            // Step 5: Save the generated files
            var saveLangName = targetLang == TargetLanguage.CSharp ? "C#" : "Java";
            var fileExtension = targetLang == TargetLanguage.CSharp ? ".cs" : ".java";
            _enhancedLogger.ShowStep(5, totalSteps, "File Generation", $"Writing {saveLangName} output files");
            _enhancedLogger.LogBehindTheScenes("MIGRATION", "STEP_5_START",
                $"Writing {javaFiles.Count} {saveLangName} files to {javaOutputFolder}");
            progressCallback?.Invoke($"Saving {saveLangName} files", 5, totalSteps);

            for (int i = 0; i < javaFiles.Count; i++)
            {
                var javaFile = javaFiles[i];

                // Save with correct extension based on actual file type
                if (javaFile is JavaFile jf && targetLang == TargetLanguage.Java)
                {
                    // Java files use JavaFile-specific save method
                    await _fileHelper.SaveJavaFileAsync(jf, javaOutputFolder);
                }
                else if (javaFile is CodeFile codeFile)
                {
                    // C# or other CodeFiles use generic save with explicit extension
                    await _fileHelper.SaveCodeFileAsync(codeFile, javaOutputFolder, fileExtension);
                }
                else
                {
                    // Fallback shouldn't happen, but handle gracefully
                    _logger.LogWarning("Unexpected file type: {Type} for {FileName}", javaFile.GetType().Name, javaFile.FileName);
                    await _fileHelper.SaveCodeFileAsync(javaFile, javaOutputFolder, fileExtension);
                }

                _enhancedLogger.ShowProgressBar(i + 1, javaFiles.Count, $"Saving {saveLangName} files");
                _enhancedLogger.LogBehindTheScenes("FILE_OUTPUT", "CODE_FILE_SAVED",
                    $"Saved {javaFile.FileName} ({javaFile.Content.Length} chars)");
                progressCallback?.Invoke($"Saving {saveLangName} files ({i + 1}/{javaFiles.Count})", 5, totalSteps);
            }            // Step 6: Generate migration report
            _enhancedLogger.ShowStep(6, totalSteps, "Report Generation", "Creating migration summary and metrics");
            _enhancedLogger.LogBehindTheScenes("MIGRATION", "STEP_6_START",
                "Generating comprehensive migration report and documentation");
            progressCallback?.Invoke("Generating reports", 6, totalSteps);

            await GenerateMigrationReportAsync(cobolFiles, javaFiles, dependencyMap, javaOutputFolder, startTime);

            // Export conversation logs
            var logPath = Path.Combine(javaOutputFolder, "migration-conversation-log.md");
            _enhancedLogger.LogBehindTheScenes("FILE_OUTPUT", "LOG_EXPORT",
                $"Exporting conversation logs to {logPath}");
            await _enhancedLogger.ExportConversationLogAsync(logPath);

            // Show comprehensive API statistics and analytics
            _enhancedLogger.ShowSectionHeader("MIGRATION ANALYTICS", "API Call Statistics and Performance Analysis");
            _enhancedLogger.LogBehindTheScenes("MIGRATION", "ANALYTICS_DISPLAY",
                "Displaying comprehensive API call statistics and performance metrics");
            _enhancedLogger.ShowApiStatistics();
            _enhancedLogger.ShowCostAnalysis();
            _enhancedLogger.ShowRecentApiCalls(5);

            _enhancedLogger.ShowConversationSummary();

            // Export chat logs for Azure OpenAI conversations
            try
            {
                _enhancedLogger.ShowStep(99, 100, "Exporting Chat Logs", "Generating readable Azure OpenAI conversation logs");
                await _chatLogger.SaveChatLogAsync();
                await _chatLogger.SaveChatLogJsonAsync();

                _logger.LogInformation("Chat logs exported to Logs/ directory");

                // Show chat statistics
                var stats = _chatLogger.GetStatistics();
                _enhancedLogger.ShowSuccess($"Chat Logging Complete: {stats.TotalMessages} messages, {stats.TotalTokens} tokens, {stats.AgentBreakdown.Count} agents");
            }
            catch (Exception ex)
            {
                _logger.LogWarning(ex, "Failed to export chat logs, but migration completed successfully");
            }

            _enhancedLogger.ShowSuccess("Migration process completed successfully!");

            var totalTime = DateTime.UtcNow - startTime;
            _enhancedLogger.LogBehindTheScenes("MIGRATION", "COMPLETION",
                $"Total migration completed in {totalTime.TotalSeconds:F1} seconds");
            _logger.LogInformation("Total migration time: {TotalTime}", totalTime);
            await _migrationRepository.CompleteRunAsync(runId, "Completed");

            progressCallback?.Invoke("Migration completed successfully", totalSteps, totalSteps);
        }
        catch (Exception ex)
        {
            _enhancedLogger.ShowError($"Error in migration process: {ex.Message}", ex);
            progressCallback?.Invoke($"Error: {ex.Message}", 0, 0);
            if (_activeRunId.HasValue)
            {
                try
                {
                    await _migrationRepository.CompleteRunAsync(_activeRunId.Value, "Failed", ex.Message);
                }
                catch (Exception repoEx)
                {
                    _logger.LogWarning(repoEx, "Failed to mark migration run {RunId} as failed", _activeRunId.Value);
                }
            }
            throw;
        }
    }

    /// <summary>
    /// Generates a comprehensive migration report.
    /// </summary>
    /// <param name="cobolFiles">The original COBOL files.</param>
    /// <param name="generatedFiles">The generated code files (Java or C#).</param>
    /// <param name="dependencyMap">The dependency analysis results.</param>
    /// <param name="outputFolder">The output folder for the report.</param>
    /// <param name="startTime">The migration start time.</param>
    private async Task GenerateMigrationReportAsync(
        List<CobolFile> cobolFiles,
        List<CodeFile> generatedFiles,
        DependencyMap dependencyMap,
        string outputFolder,
        DateTime startTime)
    {
        var totalTime = DateTime.UtcNow - startTime;
        var reportPath = Path.Combine(outputFolder, "migration-report.md");

        var report = new StringBuilder();
        var targetLang = _settings.ApplicationSettings.TargetLanguage;
        var langName = targetLang == TargetLanguage.CSharp ? "C#" : "Java";
        var framework = targetLang == TargetLanguage.CSharp ? ".NET" : "Quarkus";

        report.AppendLine($"# COBOL to {langName} {framework} Migration Report");
        report.AppendLine($"Generated: {DateTime.UtcNow:yyyy-MM-dd HH:mm:ss} UTC");
        report.AppendLine($"Total Migration Time: {totalTime}");
        report.AppendLine();

        // Overview section
        report.AppendLine("## 📊 Migration Overview");
        report.AppendLine($"- **Source Files**: {cobolFiles.Count} COBOL files");
        report.AppendLine($"- **Generated Files**: {generatedFiles.Count} {langName} files");
        report.AppendLine($"- **Dependencies Found**: {dependencyMap.Dependencies.Count}");
        report.AppendLine($"- **Copybooks Analyzed**: {dependencyMap.Metrics.TotalCopybooks}");
        report.AppendLine($"- **Average Dependencies per Program**: {dependencyMap.Metrics.AverageDependenciesPerProgram:F1}");
        report.AppendLine();

        // File mapping section
        report.AppendLine("## 🗂️ File Mapping");
        report.AppendLine($"| COBOL File | {langName} File | Type |");
        report.AppendLine("|------------|-----------|------|");

        foreach (var cobolFile in cobolFiles.Take(20)) // Limit to first 20 for readability
        {
            var generatedFile = generatedFiles.FirstOrDefault(j => j.OriginalCobolFileName == cobolFile.FileName);
            var generatedFileName = generatedFile?.ClassName ?? "Not Generated";
            var fileType = cobolFile.FileName.EndsWith(".cpy") ? "Copybook" : "Program";
            report.AppendLine($"| {cobolFile.FileName} | {generatedFileName} | {fileType} |");
        }

        if (cobolFiles.Count > 20)
        {
            report.AppendLine($"| ... and {cobolFiles.Count - 20} more files | ... | ... |");
        }
        report.AppendLine();

        // Dependency analysis section
        report.AppendLine("## 🔗 Dependency Analysis");
        if (dependencyMap.Metrics.CircularDependencies.Any())
        {
            report.AppendLine("### ⚠️ Circular Dependencies Found");
            foreach (var circular in dependencyMap.Metrics.CircularDependencies)
            {
                report.AppendLine($"- {circular}");
            }
            report.AppendLine();
        }

        report.AppendLine("### Most Used Copybooks");
        var topCopybooks = dependencyMap.ReverseDependencies
            .OrderByDescending(kv => kv.Value.Count)
            .Take(10);

        foreach (var copybook in topCopybooks)
        {
            report.AppendLine($"- **{copybook.Key}**: Used by {copybook.Value.Count} programs");
        }
        report.AppendLine();

        // Migration metrics
        report.AppendLine("## 📈 Migration Metrics");
        report.AppendLine($"- **Files per Minute**: {(cobolFiles.Count / Math.Max(totalTime.TotalMinutes, 1)):F1}");
        report.AppendLine($"- **Average File Size**: {cobolFiles.Average(f => f.Content.Length):F0} characters");
        report.AppendLine($"- **Total Lines of Code**: {cobolFiles.Sum(f => f.Content.Split('\n').Length):N0}");
        report.AppendLine();

        // Next steps
        report.AppendLine("## 🚀 Next Steps");
        report.AppendLine("1. Review generated Java files for accuracy");
        report.AppendLine("2. Run unit tests (if UnitTestAgent is configured)");
        report.AppendLine("3. Check dependency diagram for architecture insights");
        report.AppendLine("4. Validate business logic in converted code");
        report.AppendLine("5. Configure Quarkus application properties");
        report.AppendLine();

        // Files generated
        report.AppendLine("## 📁 Generated Files");
        report.AppendLine("- `dependency-map.json` - Complete dependency analysis");
        report.AppendLine("- `dependency-diagram.md` - Mermaid dependency visualization");
        report.AppendLine("- `migration-conversation-log.md` - AI agent conversation log");
        report.AppendLine("- Individual Java files in respective packages");

        await File.WriteAllTextAsync(reportPath, report.ToString());
        _logger.LogInformation("Migration report generated: {ReportPath}", reportPath);
    }
}
