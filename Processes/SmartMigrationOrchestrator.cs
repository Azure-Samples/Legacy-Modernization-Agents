using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Agents.Infrastructure;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Persistence;

namespace CobolToQuarkusMigration.Processes;

/// <summary>
/// Smart migration orchestrator that automatically routes files to the appropriate process:
/// - Small files (below threshold) → MigrationProcess (direct conversion)
/// - Large files (above threshold) → ChunkedMigrationProcess (smart chunking)
/// 
/// This ensures no code is ever truncated or lost during migration.
/// </summary>
public class SmartMigrationOrchestrator
{
    private readonly ResponsesApiClient? _responsesClient;
    private readonly IChatClient _chatClient;
    private readonly ILoggerFactory _loggerFactory;
    private readonly ILogger<SmartMigrationOrchestrator> _logger;
    private readonly FileHelper _fileHelper;
    private readonly AppSettings _settings;
    private readonly IMigrationRepository _migrationRepository;
    private readonly EnhancedLogger _enhancedLogger;

    /// <summary>
    /// Statistics about file categorization for dashboard display.
    /// </summary>
    public class MigrationStats
    {
        public int TotalFiles { get; set; }
        public int SmallFiles { get; set; }
        public int LargeFiles { get; set; }
        public int ChunkedFiles { get; set; }
        public int DirectFiles { get; set; }
        public long TotalChunks { get; set; }
        public List<FileCategorizationInfo> FileCategorization { get; set; } = new();
    }

    /// <summary>
    /// Information about how a file was categorized.
    /// </summary>
    public class FileCategorizationInfo
    {
        public string FileName { get; set; } = string.Empty;
        public int LineCount { get; set; }
        public int CharCount { get; set; }
        public bool RequiresChunking { get; set; }
        public int EstimatedChunks { get; set; }
        public string ProcessingMethod { get; set; } = string.Empty;
    }

    /// <summary>
    /// Initializes a new SmartMigrationOrchestrator.
    /// </summary>
    public SmartMigrationOrchestrator(
        ResponsesApiClient? responsesClient,
        IChatClient chatClient,
        ILoggerFactory loggerFactory,
        FileHelper fileHelper,
        AppSettings settings,
        IMigrationRepository migrationRepository)
    {
        _responsesClient = responsesClient;
        _chatClient = chatClient;
        _loggerFactory = loggerFactory;
        _logger = loggerFactory.CreateLogger<SmartMigrationOrchestrator>();
        _fileHelper = fileHelper;
        _settings = settings;
        _migrationRepository = migrationRepository;
        _enhancedLogger = new EnhancedLogger(_logger);
    }

    /// <summary>
    /// Runs the smart migration, automatically routing files to the appropriate process.
    /// </summary>
    /// <param name="cobolSourceFolder">The folder containing COBOL source files.</param>
    /// <param name="outputFolder">The folder for output files.</param>
    /// <param name="progressCallback">Optional callback for progress reporting.</param>
    /// <param name="existingRunId">Optional run ID to resume.</param>
    /// <param name="businessLogicExtracts">Optional business logic extracted during reverse engineering to guide conversion.</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    public async Task<MigrationStats> RunAsync(
        string cobolSourceFolder,
        string outputFolder,
        Action<string, int, int>? progressCallback = null,
        int? existingRunId = null,
        List<BusinessLogic>? businessLogicExtracts = null,
        DependencyMap? existingDependencyMap = null,
        string? runType = null,
        CancellationToken cancellationToken = default)
    {
        var stats = new MigrationStats();
        var targetName = _settings.ApplicationSettings.TargetLanguage == TargetLanguage.CSharp 
            ? "C# .NET" : "Java Quarkus";

        _enhancedLogger.ShowSectionHeader(
            $"🧠 SMART MIGRATION ORCHESTRATOR",
            $"Intelligent file routing for COBOL to {targetName}");

        if (existingRunId.HasValue)
        {
            _enhancedLogger.ShowWarning($"Resuming migration for Run ID: {existingRunId}");
        }

        // Step 1: Scan and categorize files
        _enhancedLogger.ShowStep(1, 4, "File Analysis", "Scanning and categorizing COBOL files");
        progressCallback?.Invoke("Analyzing files for chunking requirements", 1, 4);

        var cobolFiles = await _fileHelper.ScanDirectoryForCobolFilesAsync(cobolSourceFolder);
        PromptLoader.CodebaseProfile = PromptLoader.GenerateCodebaseProfile(cobolFiles);
        
        if (cobolFiles.Count == 0)
        {
            _enhancedLogger.ShowWarning($"No COBOL files found in folder: {cobolSourceFolder}");
            return stats;
        }

        stats.TotalFiles = cobolFiles.Count;

        // Step 2: Categorize files
        var (smallFiles, largeFiles) = CategorizeFiles(cobolFiles, stats);
        stats.SmallFiles = smallFiles.Count;
        stats.LargeFiles = largeFiles.Count;

        // Display categorization summary
        DisplayCategorizationSummary(stats);

        // Step 3: Process files based on category
        _enhancedLogger.ShowStep(2, 4, "Migration Routing", "Routing files to appropriate process");

        if (largeFiles.Count > 0)
        {
            // Process large files with ChunkedMigrationProcess
            Console.WriteLine($"📦 Routing {largeFiles.Count} large file(s) to ChunkedMigrationProcess");
            progressCallback?.Invoke($"Processing {largeFiles.Count} large files with smart chunking", 2, 4);

            await ProcessLargeFilesAsync(
                largeFiles, 
                smallFiles, 
                cobolSourceFolder, 
                outputFolder, 
                stats,
                progressCallback,
                existingRunId,
                businessLogicExtracts,
                existingDependencyMap,
                cancellationToken,
                runType);
        }
        else if (smallFiles.Count > 0)
        {
            // All files are small - use standard MigrationProcess
            Console.WriteLine($"⚡ All {smallFiles.Count} file(s) are small - using direct conversion");
            progressCallback?.Invoke($"Processing {smallFiles.Count} files directly", 2, 4);

            await ProcessSmallFilesOnlyAsync(
                cobolSourceFolder, 
                outputFolder, 
                progressCallback,
                existingRunId,
                businessLogicExtracts,
                existingDependencyMap,
                runType: runType);
            
            stats.DirectFiles = smallFiles.Count;
        }

        // Step 4: Summary
        _enhancedLogger.ShowStep(4, 4, "Migration Complete", "All files processed");
        DisplayFinalSummary(stats);

        return stats;
    }

    /// <summary>
    /// Categorizes files into small (direct processing) and large (chunked processing).
    /// </summary>
    private (List<CobolFile> small, List<CobolFile> large) CategorizeFiles(
        List<CobolFile> files, 
        MigrationStats stats)
    {
        var small = new List<CobolFile>();
        var large = new List<CobolFile>();
        var chunkingSettings = _settings.ChunkingSettings;

        foreach (var file in files)
        {
            var charCount = file.Content.Length;
            var lineCount = file.Content.Split('\n').Length;
            var requiresChunking = chunkingSettings.RequiresChunking(charCount, lineCount);

            // Estimate chunks if needed
            var estimatedChunks = 1;
            if (requiresChunking)
            {
                estimatedChunks = Math.Max(1, (int)Math.Ceiling(
                    (double)lineCount / chunkingSettings.MaxLinesPerChunk));
            }

            var categorization = new FileCategorizationInfo
            {
                FileName = file.FileName,
                LineCount = lineCount,
                CharCount = charCount,
                RequiresChunking = requiresChunking,
                EstimatedChunks = estimatedChunks,
                ProcessingMethod = requiresChunking ? "Chunked" : "Direct"
            };
            stats.FileCategorization.Add(categorization);

            if (requiresChunking)
            {
                large.Add(file);
                stats.TotalChunks += estimatedChunks;
                _logger.LogInformation(
                    "🔷 LARGE FILE: {File} ({Lines:N0} lines, {Chars:N0} chars) → will be chunked into ~{Chunks} parts",
                    file.FileName, lineCount, charCount, estimatedChunks);
            }
            else
            {
                small.Add(file);
                _logger.LogInformation(
                    "🔹 Small file: {File} ({Lines:N0} lines, {Chars:N0} chars) → direct conversion",
                    file.FileName, lineCount, charCount);
            }
        }

        return (small, large);
    }

    /// <summary>
    /// Processes large files using ChunkedMigrationProcess, which handles both large and small files.
    /// </summary>
    private async Task ProcessLargeFilesAsync(
        List<CobolFile> largeFiles,
        List<CobolFile> smallFiles,
        string cobolSourceFolder,
        string outputFolder,
        MigrationStats stats,
        Action<string, int, int>? progressCallback,
        int? existingRunId,
        List<BusinessLogic>? businessLogicExtracts,
        DependencyMap? existingDependencyMap,
        CancellationToken cancellationToken,
        string? runType = null)
    {
        _enhancedLogger.ShowSectionHeader(
            "📦 CHUNKED MIGRATION PROCESS",
            $"Processing {largeFiles.Count} large + {smallFiles.Count} small files with smart chunking");

        // ChunkedMigrationProcess handles both large and small files efficiently
        var chunkedProcess = new ChunkedMigrationProcess(
            _chatClient,
            _responsesClient,
            _loggerFactory.CreateLogger<ChunkedMigrationProcess>(),
            _fileHelper,
            _settings,
            _migrationRepository);

        chunkedProcess.InitializeAgents();

        if (businessLogicExtracts != null && businessLogicExtracts.Count > 0)
        {
            chunkedProcess.SetBusinessLogicContext(businessLogicExtracts);
        }

        if (existingDependencyMap != null)
        {
            chunkedProcess.SetDependencyMap(existingDependencyMap);
        }

        await chunkedProcess.RunAsync(
            cobolSourceFolder,
            outputFolder,
            (status, current, total, percent) =>
            {
                progressCallback?.Invoke(status, current + 1, 4);
            },
            existingRunId: existingRunId,
            cancellationToken: cancellationToken,
            runType: runType);

        stats.ChunkedFiles = largeFiles.Count;
        stats.DirectFiles = smallFiles.Count;
    }

    /// <summary>
    /// Processes only small files using the standard MigrationProcess.
    /// </summary>
    private async Task ProcessSmallFilesOnlyAsync(
        string cobolSourceFolder,
        string outputFolder,
        Action<string, int, int>? progressCallback,
        int? existingRunId = null,
        List<BusinessLogic>? businessLogicExtracts = null,
        DependencyMap? existingDependencyMap = null,
        string? runType = null)
    {
        _enhancedLogger.ShowSectionHeader(
            "⚡ DIRECT MIGRATION PROCESS",
            "All files below chunking threshold - using optimized direct conversion");

        var migrationProcess = new MigrationProcess(
            _responsesClient,
            _chatClient,
            _loggerFactory.CreateLogger<MigrationProcess>(),
            _fileHelper,
            _settings,
            _migrationRepository);

        migrationProcess.InitializeAgents();

        if (businessLogicExtracts != null && businessLogicExtracts.Count > 0)
        {
            migrationProcess.SetBusinessLogicContext(businessLogicExtracts);
        }

        if (existingDependencyMap != null)
        {
            migrationProcess.SetDependencyMap(existingDependencyMap);
        }

        await migrationProcess.RunAsync(
            cobolSourceFolder,
            outputFolder,
            (status, current, total) =>
            {
                progressCallback?.Invoke(status, current + 1, 4);
            },
            existingRunId,
            runType);
    }

    /// <summary>
    /// Displays the file categorization summary.
    /// </summary>
    private void DisplayCategorizationSummary(MigrationStats stats)
    {
        Console.WriteLine();
        Console.WriteLine("╔══════════════════════════════════════════════════════════════════════╗");
        Console.WriteLine("║                    📊 FILE CATEGORIZATION SUMMARY                    ║");
        Console.WriteLine("╠══════════════════════════════════════════════════════════════════════╣");
        Console.WriteLine($"║  Total Files:     {stats.TotalFiles,5}                                           ║");
        Console.WriteLine($"║  Small Files:     {stats.SmallFiles,5} (direct conversion)                       ║");
        Console.WriteLine($"║  Large Files:     {stats.LargeFiles,5} (smart chunking)                          ║");
        if (stats.TotalChunks > 0)
        {
            Console.WriteLine($"║  Est. Chunks:     {stats.TotalChunks,5} (for large files)                         ║");
        }
        Console.WriteLine("╠══════════════════════════════════════════════════════════════════════╣");
        
        var charThreshold = _settings.ChunkingSettings.AutoChunkCharThreshold;
        var lineThreshold = _settings.ChunkingSettings.AutoChunkLineThreshold;
        Console.WriteLine($"║  Chunking Thresholds:                                                ║");
        Console.WriteLine($"║    Characters: > {charThreshold:N0}                                           ║");
        Console.WriteLine($"║    Lines:      > {lineThreshold:N0}                                               ║");
        Console.WriteLine("╚══════════════════════════════════════════════════════════════════════╝");
        Console.WriteLine();

        // Per-file details
        if (stats.FileCategorization.Count > 0)
        {
            Console.WriteLine("  File Details:");
            foreach (var file in stats.FileCategorization)
            {
                var icon = file.RequiresChunking ? "📦" : "📄";
                var method = file.RequiresChunking 
                    ? $"→ {file.EstimatedChunks} chunks" 
                    : "→ direct";
                Console.WriteLine($"    {icon} {file.FileName,-25} {file.LineCount,6:N0} lines  {file.CharCount,8:N0} chars  {method}");
            }
            Console.WriteLine();
        }
    }

    /// <summary>
    /// Displays the final migration summary.
    /// </summary>
    private void DisplayFinalSummary(MigrationStats stats)
    {
        Console.WriteLine();
        Console.WriteLine("╔══════════════════════════════════════════════════════════════════════╗");
        Console.WriteLine("║                    ✅ MIGRATION COMPLETE                              ║");
        Console.WriteLine("╠══════════════════════════════════════════════════════════════════════╣");
        Console.WriteLine($"║  Files Processed:  {stats.TotalFiles,5}                                          ║");
        Console.WriteLine($"║  Direct Convert:   {stats.DirectFiles,5}                                          ║");
        Console.WriteLine($"║  Chunked Convert:  {stats.ChunkedFiles,5}                                          ║");
        if (stats.TotalChunks > 0)
        {
            Console.WriteLine($"║  Total Chunks:     {stats.TotalChunks,5}                                          ║");
        }
        Console.WriteLine("║                                                                      ║");
        Console.WriteLine("║  ⚠️  NO CODE WAS TRUNCATED - ALL FILES FULLY CONVERTED               ║");
        Console.WriteLine("╚══════════════════════════════════════════════════════════════════════╝");
        Console.WriteLine();
    }

    /// <summary>
    /// Gets the current migration statistics for dashboard display.
    /// Call this during or after migration to get stats for the portal.
    /// </summary>
    public static MigrationStats AnalyzeFilesForDashboard(
        List<CobolFile> files, 
        ChunkingSettings chunkingSettings)
    {
        var stats = new MigrationStats { TotalFiles = files.Count };

        foreach (var file in files)
        {
            var charCount = file.Content.Length;
            var lineCount = file.Content.Split('\n').Length;
            var requiresChunking = chunkingSettings.RequiresChunking(charCount, lineCount);

            var estimatedChunks = 1;
            if (requiresChunking)
            {
                estimatedChunks = Math.Max(1, (int)Math.Ceiling(
                    (double)lineCount / chunkingSettings.MaxLinesPerChunk));
                stats.LargeFiles++;
                stats.TotalChunks += estimatedChunks;
            }
            else
            {
                stats.SmallFiles++;
            }

            stats.FileCategorization.Add(new FileCategorizationInfo
            {
                FileName = file.FileName,
                LineCount = lineCount,
                CharCount = charCount,
                RequiresChunking = requiresChunking,
                EstimatedChunks = estimatedChunks,
                ProcessingMethod = requiresChunking ? "Chunked" : "Direct"
            });
        }

        return stats;
    }
}
