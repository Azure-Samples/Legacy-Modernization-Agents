using Microsoft.Extensions.Logging;
using Microsoft.SemanticKernel;
using CobolModernization.Agents;
using CobolModernization.Agents.Interfaces;
using CobolModernization.Helpers;
using CobolModernization.Models;
using System.Text;

namespace CobolModernization;

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

    private ICobolAnalyzerAgent? _cobolAnalyzerAgent;
    private IJavaConverterAgent? _javaConverterAgent;
    private ICSharpConverterAgent? _csharpConverterAgent;
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
        AppSettings settings)
    {
        _kernelBuilder = kernelBuilder;
        _logger = logger;
        _fileHelper = fileHelper;
        _settings = settings;
        _enhancedLogger = new EnhancedLogger(logger);
        _chatLogger = new ChatLogger(LoggerFactory.Create(builder => builder.AddConsole()).CreateLogger<ChatLogger>());
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

        _enhancedLogger.ShowStep(1, 4, "CobolAnalyzerAgent", "Analyzing COBOL code structure and patterns");
        _cobolAnalyzerAgent = new CobolAnalyzerAgent(
            _kernelBuilder,
            loggerFactory.CreateLogger<CobolAnalyzerAgent>(),
            _settings.AISettings.CobolAnalyzerModelId,
            _enhancedLogger,
            _chatLogger);

        _enhancedLogger.ShowStep(2, 4, "JavaConverterAgent", "Converting COBOL to Java Quarkus");
        _javaConverterAgent = new JavaConverterAgent(
            _kernelBuilder,
            loggerFactory.CreateLogger<JavaConverterAgent>(),
            _settings.AISettings.JavaConverterModelId,
            _enhancedLogger,
            _chatLogger);

        _enhancedLogger.ShowStep(3, 4, "CSharpConverterAgent", "Converting COBOL to C# .NET");
        _csharpConverterAgent = new CSharpConverterAgent(
            _kernelBuilder,
            loggerFactory.CreateLogger<CSharpConverterAgent>(),
            _settings.AISettings.JavaConverterModelId, // Reuse same model
            _enhancedLogger,
            _chatLogger);

        _enhancedLogger.ShowStep(4, 4, "DependencyMapperAgent", "Mapping COBOL dependencies and generating diagrams");
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
        await RunAsync(cobolSourceFolder, javaOutputFolder, null, "Java", progressCallback);
    }

    /// <summary>
    /// Runs the migration process with support for both Java and C# output.
    /// </summary>
    /// <param name="cobolSourceFolder">The folder containing COBOL source files.</param>
    /// <param name="javaOutputFolder">The folder for Java output files.</param>
    /// <param name="csharpOutputFolder">The folder for C# output files.</param>
    /// <param name="targetLanguage">Target language: Java, CSharp, or Both.</param>
    /// <param name="progressCallback">Optional callback for progress reporting.</param>
    /// <returns>A task representing the asynchronous operation.</returns>
    public async Task RunAsync(
        string cobolSourceFolder,
        string? javaOutputFolder,
        string? csharpOutputFolder,
        string targetLanguage = "Java",
        Action<string, int, int>? progressCallback = null)
    {
        var convertToJava = targetLanguage.Equals("Java", StringComparison.OrdinalIgnoreCase) ||
                           targetLanguage.Equals("Both", StringComparison.OrdinalIgnoreCase);
        var convertToCSharp = targetLanguage.Equals("CSharp", StringComparison.OrdinalIgnoreCase) ||
                             targetLanguage.Equals("Both", StringComparison.OrdinalIgnoreCase);

        _enhancedLogger.ShowSectionHeader($"COBOL TO {targetLanguage.ToUpper()} MIGRATION", "AI-Powered Legacy Code Modernization");

        _logger.LogInformation("COBOL source folder: {CobolSourceFolder}", cobolSourceFolder);
        if (convertToJava)
            _logger.LogInformation("Java output folder: {JavaOutputFolder}", javaOutputFolder);
        if (convertToCSharp)
            _logger.LogInformation("C# output folder: {CSharpOutputFolder}", csharpOutputFolder);

        if (_cobolAnalyzerAgent == null || _dependencyMapperAgent == null)
        {
            _enhancedLogger.ShowError("Agents not initialized. Call InitializeAgents() first.");
            throw new InvalidOperationException("Agents not initialized. Call InitializeAgents() first.");
        }

        if (convertToJava && _javaConverterAgent == null)
        {
            _enhancedLogger.ShowError("Java converter agent not initialized.");
            throw new InvalidOperationException("Java converter agent not initialized.");
        }

        if (convertToCSharp && _csharpConverterAgent == null)
        {
            _enhancedLogger.ShowError("C# converter agent not initialized.");
            throw new InvalidOperationException("C# converter agent not initialized.");
        }

        var baseSteps = 4; // Scan, Analyze, Dependencies, Report
        var conversionSteps = (convertToJava ? 2 : 0) + (convertToCSharp ? 2 : 0); // Convert + Save for each language
        var totalSteps = baseSteps + conversionSteps;
        var currentStep = 0;
        var startTime = DateTime.UtcNow;

        try
        {
            // Step 1: Scan the COBOL source folder for COBOL files
            _enhancedLogger.ShowStep(1, totalSteps, "File Discovery", "Scanning for COBOL programs and copybooks");
            _enhancedLogger.LogBehindTheScenes("MIGRATION", "STEP_1_START",
                $"Starting file discovery in {cobolSourceFolder}");
            progressCallback?.Invoke("Scanning for COBOL files", 1, totalSteps);

            var cobolFiles = await _fileHelper.ScanDirectoryForCobolFilesAsync(cobolSourceFolder);

            if (cobolFiles.Count == 0)
            {
                _enhancedLogger.LogBehindTheScenes("WARNING", "NO_FILES_FOUND",
                    $"No COBOL files discovered in {cobolSourceFolder}");
                _enhancedLogger.ShowWarning($"No COBOL files found in folder: {cobolSourceFolder}");
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

            // Save dependency map and Mermaid diagram to appropriate output folder(s)
            if (convertToJava && convertToCSharp)
            {
                // Save to both folders
                var javaDependencyMapPath = Path.Combine(javaOutputFolder!, "dependency-map.json");
                var javaMermaidDiagramPath = Path.Combine(javaOutputFolder!, "dependency-diagram.md");
                var csharpDependencyMapPath = Path.Combine(csharpOutputFolder!, "dependency-map.json");
                var csharpMermaidDiagramPath = Path.Combine(csharpOutputFolder!, "dependency-diagram.md");

                _enhancedLogger.LogBehindTheScenes("FILE_OUTPUT", "DEPENDENCY_EXPORT",
                    $"Saving dependency maps to both Java and C# output folders");

                await _fileHelper.SaveDependencyMapAsync(dependencyMap, javaDependencyMapPath);
                await File.WriteAllTextAsync(javaMermaidDiagramPath, $"# COBOL Dependency Diagram\n\n```mermaid\n{dependencyMap.MermaidDiagram}\n```");

                await _fileHelper.SaveDependencyMapAsync(dependencyMap, csharpDependencyMapPath);
                await File.WriteAllTextAsync(csharpMermaidDiagramPath, $"# COBOL Dependency Diagram\n\n```mermaid\n{dependencyMap.MermaidDiagram}\n```");
            }
            else if (convertToJava)
            {
                var dependencyMapPath = Path.Combine(javaOutputFolder!, "dependency-map.json");
                var mermaidDiagramPath = Path.Combine(javaOutputFolder!, "dependency-diagram.md");

                _enhancedLogger.LogBehindTheScenes("FILE_OUTPUT", "DEPENDENCY_EXPORT",
                    $"Saving dependency map to {dependencyMapPath}");
                await _fileHelper.SaveDependencyMapAsync(dependencyMap, dependencyMapPath);
                await File.WriteAllTextAsync(mermaidDiagramPath, $"# COBOL Dependency Diagram\n\n```mermaid\n{dependencyMap.MermaidDiagram}\n```");
            }
            else if (convertToCSharp)
            {
                var dependencyMapPath = Path.Combine(csharpOutputFolder!, "dependency-map.json");
                var mermaidDiagramPath = Path.Combine(csharpOutputFolder!, "dependency-diagram.md");

                _enhancedLogger.LogBehindTheScenes("FILE_OUTPUT", "DEPENDENCY_EXPORT",
                    $"Saving dependency map to {dependencyMapPath}");
                await _fileHelper.SaveDependencyMapAsync(dependencyMap, dependencyMapPath);
                await File.WriteAllTextAsync(mermaidDiagramPath, $"# COBOL Dependency Diagram\n\n```mermaid\n{dependencyMap.MermaidDiagram}\n```");
            }

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

            _enhancedLogger.LogBehindTheScenes("MIGRATION", "COBOL_ANALYSIS_COMPLETE",
                $"Completed analysis of {cobolAnalyses.Count} COBOL files");
            _enhancedLogger.ShowSuccess($"COBOL analysis complete - {cobolAnalyses.Count} files analyzed");

            currentStep = 3;
            List<JavaFile>? javaFiles = null;
            List<CSharpFile>? csharpFiles = null;

            // Convert to Java if requested
            if (convertToJava && javaOutputFolder != null)
            {
                currentStep++;
                _enhancedLogger.ShowStep(currentStep, totalSteps, "Java Conversion", "Converting to Java Quarkus microservices");
                _enhancedLogger.LogBehindTheScenes("MIGRATION", "JAVA_CONVERSION_START",
                    "Starting AI-powered COBOL to Java conversion");
                progressCallback?.Invoke("Converting to Java", currentStep, totalSteps);

                javaFiles = await _javaConverterAgent!.ConvertToJavaAsync(
                    cobolFiles,
                    cobolAnalyses,
                    (current, total) =>
                    {
                        _enhancedLogger.ShowProgressBar(current, total, "Converting to Java");
                        _enhancedLogger.LogBehindTheScenes("PROGRESS", "JAVA_CONVERSION",
                            $"Converting file {current}/{total} to Java Quarkus");
                        progressCallback?.Invoke($"Converting to Java ({current}/{total})", currentStep, totalSteps);
                    });

                _enhancedLogger.LogBehindTheScenes("MIGRATION", "JAVA_CONVERSION_COMPLETE",
                    $"Generated {javaFiles.Count} Java files from COBOL sources");
                _enhancedLogger.ShowSuccess($"Java conversion complete - {javaFiles.Count} Java files generated");

                // Save Java files
                currentStep++;
                _enhancedLogger.ShowStep(currentStep, totalSteps, "Java File Generation", "Writing Java Quarkus output files");
                _enhancedLogger.LogBehindTheScenes("MIGRATION", "JAVA_SAVE_START",
                    $"Writing {javaFiles.Count} Java files to {javaOutputFolder}");
                progressCallback?.Invoke("Saving Java files", currentStep, totalSteps);

                for (int i = 0; i < javaFiles.Count; i++)
                {
                    var javaFile = javaFiles[i];
                    await _fileHelper.SaveJavaFileAsync(javaFile, javaOutputFolder);
                    _enhancedLogger.ShowProgressBar(i + 1, javaFiles.Count, "Saving Java files");
                    _enhancedLogger.LogBehindTheScenes("FILE_OUTPUT", "JAVA_FILE_SAVED",
                        $"Saved {javaFile.FileName} ({javaFile.Content.Length} chars)");
                    progressCallback?.Invoke($"Saving Java files ({i + 1}/{javaFiles.Count})", currentStep, totalSteps);
                }
                _enhancedLogger.ShowSuccess($"Saved {javaFiles.Count} Java files");
            }

            // Convert to C# if requested
            if (convertToCSharp && csharpOutputFolder != null)
            {
                currentStep++;
                _enhancedLogger.ShowStep(currentStep, totalSteps, "C# Conversion", "Converting to C# .NET applications");
                _enhancedLogger.LogBehindTheScenes("MIGRATION", "CSHARP_CONVERSION_START",
                    "Starting AI-powered COBOL to C# conversion");
                progressCallback?.Invoke("Converting to C#", currentStep, totalSteps);

                csharpFiles = await _csharpConverterAgent!.ConvertToCSharpAsync(
                    cobolFiles,
                    cobolAnalyses,
                    (current, total) =>
                    {
                        _enhancedLogger.ShowProgressBar(current, total, "Converting to C#");
                        _enhancedLogger.LogBehindTheScenes("PROGRESS", "CSHARP_CONVERSION",
                            $"Converting file {current}/{total} to C# .NET");
                        progressCallback?.Invoke($"Converting to C# ({current}/{total})", currentStep, totalSteps);
                    });

                _enhancedLogger.LogBehindTheScenes("MIGRATION", "CSHARP_CONVERSION_COMPLETE",
                    $"Generated {csharpFiles.Count} C# files from COBOL sources");
                _enhancedLogger.ShowSuccess($"C# conversion complete - {csharpFiles.Count} C# files generated");

                // Save C# files
                currentStep++;
                _enhancedLogger.ShowStep(currentStep, totalSteps, "C# File Generation", "Writing C# .NET output files");
                _enhancedLogger.LogBehindTheScenes("MIGRATION", "CSHARP_SAVE_START",
                    $"Writing {csharpFiles.Count} C# files to {csharpOutputFolder}");
                progressCallback?.Invoke("Saving C# files", currentStep, totalSteps);

                for (int i = 0; i < csharpFiles.Count; i++)
                {
                    var csharpFile = csharpFiles[i];
                    await _fileHelper.SaveCSharpFileAsync(csharpFile, csharpOutputFolder);
                    _enhancedLogger.ShowProgressBar(i + 1, csharpFiles.Count, "Saving C# files");
                    _enhancedLogger.LogBehindTheScenes("FILE_OUTPUT", "CSHARP_FILE_SAVED",
                        $"Saved {csharpFile.FileName} ({csharpFile.Content.Length} chars)");
                    progressCallback?.Invoke($"Saving C# files ({i + 1}/{csharpFiles.Count})", currentStep, totalSteps);
                }
                _enhancedLogger.ShowSuccess($"Saved {csharpFiles.Count} C# files");
            }

            // Generate migration report
            currentStep++;
            _enhancedLogger.ShowStep(currentStep, totalSteps, "Report Generation", "Creating migration summary and metrics");
            _enhancedLogger.LogBehindTheScenes("MIGRATION", "REPORT_START",
                "Generating comprehensive migration report and documentation");
            progressCallback?.Invoke("Generating reports", currentStep, totalSteps);

            // Generate separate reports for each language when converting to both
            if (convertToJava && convertToCSharp)
            {
                // Generate Java report
                await GenerateMigrationReportAsync(cobolFiles, javaFiles, null, dependencyMap,
                    javaOutputFolder!, startTime, "Java");

                // Generate C# report
                await GenerateMigrationReportAsync(cobolFiles, null, csharpFiles, dependencyMap,
                    csharpOutputFolder!, startTime, "CSharp");

                _enhancedLogger.ShowSuccess("Generated separate migration reports for Java and C#");
            }
            else if (convertToJava)
            {
                await GenerateMigrationReportAsync(cobolFiles, javaFiles, null, dependencyMap,
                    javaOutputFolder!, startTime, "Java");
            }
            else if (convertToCSharp)
            {
                await GenerateMigrationReportAsync(cobolFiles, null, csharpFiles, dependencyMap,
                    csharpOutputFolder!, startTime, "CSharp");
            }

            // Export conversation logs to both folders when converting to both languages
            if (convertToJava && convertToCSharp)
            {
                var javaLogPath = Path.Combine(javaOutputFolder!, "migration-conversation-log.md");
                var csharpLogPath = Path.Combine(csharpOutputFolder!, "migration-conversation-log.md");

                _enhancedLogger.LogBehindTheScenes("FILE_OUTPUT", "LOG_EXPORT",
                    $"Exporting conversation logs to {javaLogPath} and {csharpLogPath}");
                await _enhancedLogger.ExportConversationLogAsync(javaLogPath);
                await _enhancedLogger.ExportConversationLogAsync(csharpLogPath);
            }
            else if (convertToJava)
            {
                var logPath = Path.Combine(javaOutputFolder!, "migration-conversation-log.md");
                _enhancedLogger.LogBehindTheScenes("FILE_OUTPUT", "LOG_EXPORT",
                    $"Exporting conversation logs to {logPath}");
                await _enhancedLogger.ExportConversationLogAsync(logPath);
            }
            else if (convertToCSharp)
            {
                var logPath = Path.Combine(csharpOutputFolder!, "migration-conversation-log.md");
                _enhancedLogger.LogBehindTheScenes("FILE_OUTPUT", "LOG_EXPORT",
                    $"Exporting conversation logs to {logPath}");
                await _enhancedLogger.ExportConversationLogAsync(logPath);
            }

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

            progressCallback?.Invoke("Migration completed successfully", totalSteps, totalSteps);
        }
        catch (Exception ex)
        {
            _enhancedLogger.ShowError($"Error in migration process: {ex.Message}", ex);
            progressCallback?.Invoke($"Error: {ex.Message}", 0, 0);
            throw;
        }
    }

    /// <summary>
    /// Generates a comprehensive migration report.
    /// </summary>
    /// <param name="cobolFiles">The original COBOL files.</param>
    /// <param name="javaFiles">The generated Java files.</param>
    /// <param name="csharpFiles">The generated C# files.</param>
    /// <param name="dependencyMap">The dependency analysis results.</param>
    /// <param name="outputFolder">The output folder for the report.</param>
    /// <param name="startTime">The migration start time.</param>
    /// <param name="targetLanguage">The target language(s).</param>
    private async Task GenerateMigrationReportAsync(
        List<CobolFile> cobolFiles,
        List<JavaFile>? javaFiles,
        List<CSharpFile>? csharpFiles,
        DependencyMap dependencyMap,
        string outputFolder,
        DateTime startTime,
        string targetLanguage)
    {
        var totalTime = DateTime.UtcNow - startTime;
        var reportPath = Path.Combine(outputFolder, "migration-report.md");

        var report = new StringBuilder();
        report.AppendLine($"# COBOL to {targetLanguage} Migration Report");
        report.AppendLine($"Generated: {DateTime.UtcNow:yyyy-MM-dd HH:mm:ss} UTC");
        report.AppendLine($"Total Migration Time: {totalTime}");
        report.AppendLine();

        // Overview section
        report.AppendLine("## 📊 Migration Overview");
        report.AppendLine($"- **Source Files**: {cobolFiles.Count} COBOL files");
        if (javaFiles != null)
            report.AppendLine($"- **Generated Java Files**: {javaFiles.Count}");
        if (csharpFiles != null)
            report.AppendLine($"- **Generated C# Files**: {csharpFiles.Count}");
        report.AppendLine($"- **Dependencies Found**: {dependencyMap.Dependencies.Count}");
        report.AppendLine($"- **Copybooks Analyzed**: {dependencyMap.Metrics.TotalCopybooks}");
        report.AppendLine($"- **Average Dependencies per Program**: {dependencyMap.Metrics.AverageDependenciesPerProgram:F1}");
        report.AppendLine();

        // File mapping section
        if (javaFiles != null && javaFiles.Count > 0)
        {
            report.AppendLine("## 🗂️ Java File Mapping");
            report.AppendLine("| COBOL File | Java File | Type |");
            report.AppendLine("|------------|-----------|------|");

            foreach (var cobolFile in cobolFiles.Take(20)) // Limit to first 20 for readability
            {
                var javaFile = javaFiles.FirstOrDefault(j => j.OriginalCobolFileName == cobolFile.FileName);
                var javaFileName = javaFile?.ClassName ?? "Not Generated";
                var fileType = cobolFile.FileName.EndsWith(".cpy") ? "Copybook" : "Program";
                report.AppendLine($"| {cobolFile.FileName} | {javaFileName} | {fileType} |");
            }

            if (cobolFiles.Count > 20)
            {
                report.AppendLine($"| ... and {cobolFiles.Count - 20} more files | ... | ... |");
            }
            report.AppendLine();
        }

        if (csharpFiles != null && csharpFiles.Count > 0)
        {
            report.AppendLine("## 🗂️ C# File Mapping");
            report.AppendLine("| COBOL File | C# File | Type |");
            report.AppendLine("|------------|---------|------|");

            foreach (var cobolFile in cobolFiles.Take(20)) // Limit to first 20 for readability
            {
                var csharpFile = csharpFiles.FirstOrDefault(c => c.OriginalCobolFileName == cobolFile.FileName);
                var csharpFileName = csharpFile?.ClassName ?? "Not Generated";
                var fileType = cobolFile.FileName.EndsWith(".cpy") ? "Copybook" : "Program";
                report.AppendLine($"| {cobolFile.FileName} | {csharpFileName} | {fileType} |");
            }

            if (cobolFiles.Count > 20)
            {
                report.AppendLine($"| ... and {cobolFiles.Count - 20} more files | ... | ... |");
            }
            report.AppendLine();
        }

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
        report.AppendLine("1. Review generated files for accuracy");
        report.AppendLine("2. Run unit tests (if UnitTestAgent is configured)");
        report.AppendLine("3. Check dependency diagram for architecture insights");
        report.AppendLine("4. Validate business logic in converted code");
        if (javaFiles != null && javaFiles.Count > 0)
            report.AppendLine("5. Configure Quarkus application properties for Java");
        if (csharpFiles != null && csharpFiles.Count > 0)
            report.AppendLine("6. Configure appsettings.json and dependency injection for C#");
        report.AppendLine();

        // Files generated
        report.AppendLine("## 📁 Generated Files");
        report.AppendLine("- `dependency-map.json` - Complete dependency analysis");
        report.AppendLine("- `dependency-diagram.md` - Mermaid dependency visualization");
        report.AppendLine("- `migration-conversation-log.md` - AI agent conversation log");
        if (javaFiles != null && javaFiles.Count > 0)
            report.AppendLine("- Individual Java files in respective packages");
        if (csharpFiles != null && csharpFiles.Count > 0)
            report.AppendLine("- Individual C# files in respective namespaces");

        await File.WriteAllTextAsync(reportPath, report.ToString());
        _logger.LogInformation("Migration report generated: {ReportPath}", reportPath);
    }
}
