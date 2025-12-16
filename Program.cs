using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Persistence;
using CobolToQuarkusMigration.Processes;
using CobolToQuarkusMigration.Agents;
using CobolToQuarkusMigration.Agents.Infrastructure;
using CobolToQuarkusMigration.Mcp;
using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging;
using System.CommandLine;

namespace CobolToQuarkusMigration;

/// <summary>
/// Main entry point for the COBOL to Java/C# migration tool.
/// Uses dual-API architecture:
/// - ResponsesApiClient for code agents (gpt-5.1-codex-mini via Responses API)
/// - IChatClient for chat/reports (gpt-5.1-chat via Chat Completions API)
/// </summary>
internal static class Program
{
    public static async Task<int> Main(string[] args)
    {
        // Start live log capture for portal's Live Run Log panel
        var logsDirectory = Path.Combine(Directory.GetCurrentDirectory(), "Logs");
        Directory.CreateDirectory(logsDirectory);
        
        // Only enable live logging for migration runs (not MCP server or conversation modes)
        var isMigrationRun = !args.Contains("mcp") && !args.Contains("conversation");
        LiveLogWriter? liveLogWriter = null;
        
        if (isMigrationRun)
        {
            liveLogWriter = LiveLogWriter.Start(logsDirectory);
        }
        
        try
        {
            using var loggerFactory = LoggerFactory.Create(builder => builder.AddConsole());
            var logger = loggerFactory.CreateLogger(nameof(Program));
            var fileHelper = new FileHelper(loggerFactory.CreateLogger<FileHelper>());
            var settingsHelper = new SettingsHelper(loggerFactory.CreateLogger<SettingsHelper>());

            if (!ValidateAndLoadConfiguration())
            {
                return 1;
            }

            var rootCommand = BuildRootCommand(loggerFactory, logger, fileHelper, settingsHelper);
            return await rootCommand.InvokeAsync(args);
        }
        finally
        {
            // Stop live logging and restore console
            if (liveLogWriter != null)
            {
                LiveLogWriter.Stop();
            }
        }
    }

    private static RootCommand BuildRootCommand(ILoggerFactory loggerFactory, ILogger logger, FileHelper fileHelper, SettingsHelper settingsHelper)
    {
        var rootCommand = new RootCommand("COBOL to Java Quarkus Migration Tool (Agent Framework)");

        var cobolSourceOption = new Option<string>("--source", "Path to the folder containing COBOL source files and copybooks")
        {
            Arity = ArgumentArity.ZeroOrOne
        };
        cobolSourceOption.AddAlias("-s");
        rootCommand.AddOption(cobolSourceOption);

        var javaOutputOption = new Option<string>("--java-output", () => "output", "Path to the folder for Java output files")
        {
            Arity = ArgumentArity.ZeroOrOne
        };
        javaOutputOption.AddAlias("-j");
        rootCommand.AddOption(javaOutputOption);

        var reverseEngineerOutputOption = new Option<string>("--reverse-engineer-output", () => "output", "Path to the folder for reverse engineering output")
        {
            Arity = ArgumentArity.ZeroOrOne
        };
        reverseEngineerOutputOption.AddAlias("-reo");
        rootCommand.AddOption(reverseEngineerOutputOption);

        var reverseEngineerOnlyOption = new Option<bool>("--reverse-engineer-only", () => false, "Run only reverse engineering (skip Java conversion)")
        {
            Arity = ArgumentArity.ZeroOrOne
        };
        reverseEngineerOnlyOption.AddAlias("-reo-only");
        rootCommand.AddOption(reverseEngineerOnlyOption);

        var skipReverseEngineeringOption = new Option<bool>("--skip-reverse-engineering", () => false, "Skip reverse engineering and run only Java conversion")
        {
            Arity = ArgumentArity.ZeroOrOne
        };
        skipReverseEngineeringOption.AddAlias("-skip-re");
        rootCommand.AddOption(skipReverseEngineeringOption);

        var configOption = new Option<string>("--config", () => "Config/appsettings.json", "Path to the configuration file")
        {
            Arity = ArgumentArity.ZeroOrOne
        };
        configOption.AddAlias("-c");
        rootCommand.AddOption(configOption);

        var conversationCommand = BuildConversationCommand(loggerFactory);
        rootCommand.AddCommand(conversationCommand);

        var mcpCommand = BuildMcpCommand(loggerFactory, settingsHelper);
        rootCommand.AddCommand(mcpCommand);

        var reverseEngineerCommand = BuildReverseEngineerCommand(loggerFactory, fileHelper, settingsHelper);
        rootCommand.AddCommand(reverseEngineerCommand);

        rootCommand.SetHandler(async (string cobolSource, string javaOutput, string reverseEngineerOutput, bool reverseEngineerOnly, bool skipReverseEngineering, string configPath) =>
        {
            await RunMigrationAsync(loggerFactory, logger, fileHelper, settingsHelper, cobolSource, javaOutput, reverseEngineerOutput, reverseEngineerOnly, skipReverseEngineering, configPath);
        }, cobolSourceOption, javaOutputOption, reverseEngineerOutputOption, reverseEngineerOnlyOption, skipReverseEngineeringOption, configOption);

        return rootCommand;
    }

    private static Command BuildConversationCommand(ILoggerFactory loggerFactory)
    {
        var conversationCommand = new Command("conversation", "Generate a readable conversation log from migration logs");

        var sessionIdOption = new Option<string>("--session-id", "Specific session ID to generate conversation for (optional)")
        {
            Arity = ArgumentArity.ZeroOrOne
        };
        sessionIdOption.AddAlias("-sid");
        conversationCommand.AddOption(sessionIdOption);

        var logDirOption = new Option<string>("--log-dir", () => "Logs", "Path to the logs directory")
        {
            Arity = ArgumentArity.ZeroOrOne
        };
        logDirOption.AddAlias("-ld");
        conversationCommand.AddOption(logDirOption);

        var liveOption = new Option<bool>("--live", () => false, "Enable live conversation feed that updates in real-time");
        liveOption.AddAlias("-l");
        conversationCommand.AddOption(liveOption);

        conversationCommand.SetHandler(async (string sessionId, string logDir, bool live) =>
        {
            await GenerateConversationAsync(loggerFactory, sessionId, logDir, live);
        }, sessionIdOption, logDirOption, liveOption);

        return conversationCommand;
    }

    private static Command BuildMcpCommand(ILoggerFactory loggerFactory, SettingsHelper settingsHelper)
    {
        var mcpCommand = new Command("mcp", "Expose migration insights over the Model Context Protocol");

        var runIdOption = new Option<int?>("--run-id", () => null, "Specific run ID to expose via MCP (defaults to latest)")
        {
            Arity = ArgumentArity.ZeroOrOne
        };
        runIdOption.AddAlias("-r");
        mcpCommand.AddOption(runIdOption);

        var configOption = new Option<string>("--config", () => "Config/appsettings.json", "Path to the configuration file")
        {
            Arity = ArgumentArity.ZeroOrOne
        };
        configOption.AddAlias("-c");
        mcpCommand.AddOption(configOption);

        mcpCommand.SetHandler(async (int? runId, string configPath) =>
        {
            await RunMcpServerAsync(loggerFactory, settingsHelper, runId, configPath);
        }, runIdOption, configOption);

        return mcpCommand;
    }

    private static Command BuildReverseEngineerCommand(ILoggerFactory loggerFactory, FileHelper fileHelper, SettingsHelper settingsHelper)
    {
        var reverseEngineerCommand = new Command("reverse-engineer", "Extract business logic from COBOL applications");

        var cobolSourceOption = new Option<string>("--source", "Path to the folder containing COBOL source files")
        {
            Arity = ArgumentArity.ExactlyOne
        };
        cobolSourceOption.AddAlias("-s");
        reverseEngineerCommand.AddOption(cobolSourceOption);

        var outputOption = new Option<string>("--output", () => "output", "Path to the output folder")
        {
            Arity = ArgumentArity.ZeroOrOne
        };
        outputOption.AddAlias("-o");
        reverseEngineerCommand.AddOption(outputOption);

        var configOption = new Option<string>("--config", () => "Config/appsettings.json", "Path to the configuration file")
        {
            Arity = ArgumentArity.ZeroOrOne
        };
        configOption.AddAlias("-c");
        reverseEngineerCommand.AddOption(configOption);

        reverseEngineerCommand.SetHandler(async (string cobolSource, string output, string configPath) =>
        {
            await RunReverseEngineeringAsync(loggerFactory, fileHelper, settingsHelper, cobolSource, output, configPath);
        }, cobolSourceOption, outputOption, configOption);

        return reverseEngineerCommand;
    }

    private static async Task GenerateConversationAsync(ILoggerFactory loggerFactory, string sessionId, string logDir, bool live)
    {
        try
        {
            var enhancedLogger = new EnhancedLogger(loggerFactory.CreateLogger<EnhancedLogger>());
            var logCombiner = new LogCombiner(logDir, enhancedLogger);

            Console.WriteLine("🤖 Generating conversation log from migration data...");

            string outputPath;
            if (live)
            {
                Console.WriteLine("📡 Starting live conversation feed...");
                outputPath = await logCombiner.CreateLiveConversationFeedAsync();
                Console.WriteLine($"✅ Live conversation feed created: {outputPath}");
                Console.WriteLine("📝 The conversation will update automatically as new logs are generated.");
                Console.WriteLine("Press Ctrl+C to stop monitoring.");

                await Task.Delay(-1);
            }
            else
            {
                outputPath = await logCombiner.CreateConversationNarrativeAsync(sessionId);
                Console.WriteLine($"✅ Conversation narrative created: {outputPath}");

                if (File.Exists(outputPath))
                {
                    var preview = await File.ReadAllTextAsync(outputPath);
                    var lines = preview.Split('\n').Take(20).ToArray();

                    Console.WriteLine("\n📖 Preview of conversation:");
                    Console.WriteLine("═══════════════════════════════════════");
                    foreach (var line in lines)
                    {
                        Console.WriteLine(line);
                    }

                    if (preview.Split('\n').Length > 20)
                    {
                        Console.WriteLine("... (and more)");
                    }

                    Console.WriteLine("═══════════════════════════════════════");
                }
            }
        }
        catch (Exception ex)
        {
            loggerFactory.CreateLogger(nameof(Program)).LogError(ex, "Error generating conversation log");
            Environment.Exit(1);
        }
    }

    private static async Task RunMcpServerAsync(ILoggerFactory loggerFactory, SettingsHelper settingsHelper, int? runId, string configPath)
    {
        try
        {
            AppSettings? loadedSettings = await settingsHelper.LoadSettingsAsync<AppSettings>(configPath);
            var settings = loadedSettings ?? new AppSettings();
            LoadEnvironmentVariables();
            OverrideSettingsFromEnvironment(settings);

            var databasePath = settings.ApplicationSettings.MigrationDatabasePath;
            if (!Path.IsPathRooted(databasePath))
            {
                databasePath = Path.GetFullPath(databasePath);
            }

            var repositoryLogger = loggerFactory.CreateLogger<SqliteMigrationRepository>();
            var sqliteRepository = new SqliteMigrationRepository(databasePath, repositoryLogger);
            await sqliteRepository.InitializeAsync();

            // Initialize Neo4j if enabled
            Neo4jMigrationRepository? neo4jRepository = null;
            var mcpLogger = loggerFactory.CreateLogger(nameof(Program));
            if (settings.ApplicationSettings.Neo4j?.Enabled == true)
            {
                try
                {
                    var neo4jDriver = Neo4j.Driver.GraphDatabase.Driver(
                        settings.ApplicationSettings.Neo4j.Uri,
                        Neo4j.Driver.AuthTokens.Basic(
                            settings.ApplicationSettings.Neo4j.Username,
                            settings.ApplicationSettings.Neo4j.Password
                        )
                    );
                    var neo4jLogger = loggerFactory.CreateLogger<Neo4jMigrationRepository>();
                    neo4jRepository = new Neo4jMigrationRepository(neo4jDriver, neo4jLogger);
                    mcpLogger.LogInformation("Neo4j graph database enabled at {Uri}", settings.ApplicationSettings.Neo4j.Uri);
                }
                catch (Exception ex)
                {
                    mcpLogger.LogWarning(ex, "Failed to connect to Neo4j, continuing with SQLite only");
                }
            }

            var hybridLogger = loggerFactory.CreateLogger<HybridMigrationRepository>();
            var repository = new HybridMigrationRepository(sqliteRepository, neo4jRepository, hybridLogger);
            await repository.InitializeAsync();

            var targetRunId = runId;
            if (!targetRunId.HasValue)
            {
                var latest = await repository.GetLatestRunAsync();
                if (latest is null)
                {
                    Console.Error.WriteLine("No migration runs available in the database. Run the migration process first.");
                    return;
                }

                targetRunId = latest.RunId;
            }

            // Create IChatClient for MCP server
            IChatClient? chatClient = null;
            var chatEndpoint = settings.AISettings?.ChatEndpoint ?? settings.AISettings?.Endpoint;
            var chatApiKey = settings.AISettings?.ChatApiKey ?? settings.AISettings?.ApiKey;
            var chatDeployment = settings.AISettings?.ChatDeploymentName ?? settings.AISettings?.ChatModelId ?? settings.AISettings?.DeploymentName;

            if (!string.IsNullOrEmpty(chatEndpoint) && !string.IsNullOrEmpty(chatApiKey) && !string.IsNullOrEmpty(chatDeployment))
            {
                chatClient = ChatClientFactory.CreateAzureOpenAIChatClient(chatEndpoint, chatApiKey, chatDeployment);
                mcpLogger.LogInformation("IChatClient initialized for MCP server with deployment: {Deployment}", chatDeployment);
            }

            var serverLogger = loggerFactory.CreateLogger<McpServer>();
            var server = new McpServer(repository, targetRunId.Value, serverLogger, settings.AISettings, chatClient);
            var cts = new CancellationTokenSource();

            Console.CancelKeyPress += (sender, args) =>
            {
                args.Cancel = true;
                cts.Cancel();
            };

            await server.RunAsync(cts.Token);
        }
        catch (Exception ex)
        {
            loggerFactory.CreateLogger(nameof(Program)).LogError(ex, "Error running MCP server");
            Environment.Exit(1);
        }
    }

    private static async Task RunMigrationAsync(ILoggerFactory loggerFactory, ILogger logger, FileHelper fileHelper, SettingsHelper settingsHelper, string cobolSource, string javaOutput, string reverseEngineerOutput, bool reverseEngineerOnly, bool skipReverseEngineering, string configPath)
    {
        try
        {
            logger.LogInformation("Loading settings from {ConfigPath}", configPath);
            AppSettings? loadedSettings = await settingsHelper.LoadSettingsAsync<AppSettings>(configPath);
            var settings = loadedSettings ?? new AppSettings();

            LoadEnvironmentVariables();
            OverrideSettingsFromEnvironment(settings);

            if (string.IsNullOrEmpty(settings.ApplicationSettings.CobolSourceFolder))
            {
                logger.LogError("COBOL source folder not specified. Use --source option or set in config file.");
                Environment.Exit(1);
            }

            // Output folder validation - check both Java and C# based on target
            var targetLanguage = settings.ApplicationSettings.TargetLanguage;
            if (targetLanguage == TargetLanguage.CSharp)
            {
                // For C#, use CSharpOutputFolder if set, otherwise fall back to JavaOutputFolder or default
                if (string.IsNullOrEmpty(settings.ApplicationSettings.CSharpOutputFolder) && 
                    string.IsNullOrEmpty(settings.ApplicationSettings.JavaOutputFolder))
                {
                    settings.ApplicationSettings.CSharpOutputFolder = "output/csharp";
                    logger.LogInformation("Using default C# output folder: output/csharp");
                }
            }
            else
            {
                // For Java, require JavaOutputFolder
                if (string.IsNullOrEmpty(settings.ApplicationSettings.JavaOutputFolder))
                {
                    logger.LogError("Java output folder not specified. Use --java-output option or set in config file.");
                    Environment.Exit(1);
                }
            }

            if (string.IsNullOrEmpty(settings.AISettings.ApiKey) ||
                string.IsNullOrEmpty(settings.AISettings.Endpoint) ||
                string.IsNullOrEmpty(settings.AISettings.DeploymentName))
            {
                logger.LogError("Azure OpenAI configuration incomplete. Please ensure API key, endpoint, and deployment name are configured.");
                logger.LogError("You can set them in Config/ai-config.local.env or as environment variables.");
                Environment.Exit(1);
            }

            // Create EnhancedLogger early so it can track ALL API calls
            var enhancedLogger = new EnhancedLogger(loggerFactory.CreateLogger<EnhancedLogger>());
            var chatLogger = new ChatLogger(loggerFactory.CreateLogger<ChatLogger>());

            // Get API version from environment or default
            var apiVersion = Environment.GetEnvironmentVariable("AZURE_OPENAI_API_VERSION") ?? "2025-04-01-preview";

            // Create ResponsesApiClient for code agents (codex model via Responses API)
            // gpt-5.1-codex-mini uses Responses API at /openai/responses, NOT Chat Completions
            var responsesApiClient = new ResponsesApiClient(
                settings.AISettings.Endpoint,
                settings.AISettings.ApiKey,
                settings.AISettings.DeploymentName,
                loggerFactory.CreateLogger<ResponsesApiClient>(),
                enhancedLogger,
                apiVersion: apiVersion);  // Pass EnhancedLogger for API call tracking

            logger.LogInformation("ResponsesApiClient initialized for codex model: {DeploymentName} (API: {ApiVersion})", 
                settings.AISettings.DeploymentName, apiVersion);

            // Create IChatClient for chat agents (RE reports, chat via Chat Completions API)
            var chatEndpoint = settings.AISettings.ChatEndpoint ?? settings.AISettings.Endpoint;
            var chatApiKey = settings.AISettings.ChatApiKey ?? settings.AISettings.ApiKey;
            var chatDeployment = settings.AISettings.ChatDeploymentName ?? settings.AISettings.DeploymentName;

            IChatClient chatClient = ChatClientFactory.CreateAzureOpenAIChatClient(chatEndpoint, chatApiKey, chatDeployment);
            logger.LogInformation("IChatClient initialized for chat model: {ChatDeployment}", chatDeployment);

            var databasePath = settings.ApplicationSettings.MigrationDatabasePath;
            if (!Path.IsPathRooted(databasePath))
            {
                databasePath = Path.GetFullPath(databasePath);
            }

            var migrationRepositoryLogger = loggerFactory.CreateLogger<SqliteMigrationRepository>();
            var sqliteMigrationRepository = new SqliteMigrationRepository(databasePath, migrationRepositoryLogger);
            await sqliteMigrationRepository.InitializeAsync();

            // Initialize Neo4j if enabled
            Neo4jMigrationRepository? neo4jMigrationRepository = null;
            if (settings.ApplicationSettings.Neo4j?.Enabled == true)
            {
                try
                {
                    var neo4jDriver = Neo4j.Driver.GraphDatabase.Driver(
                        settings.ApplicationSettings.Neo4j.Uri,
                        Neo4j.Driver.AuthTokens.Basic(
                            settings.ApplicationSettings.Neo4j.Username,
                            settings.ApplicationSettings.Neo4j.Password
                        )
                    );
                    var neo4jLogger = loggerFactory.CreateLogger<Neo4jMigrationRepository>();
                    neo4jMigrationRepository = new Neo4jMigrationRepository(neo4jDriver, neo4jLogger);
                    logger.LogInformation("✅ Neo4j graph database connected at {Uri}", settings.ApplicationSettings.Neo4j.Uri);
                }
                catch (Exception ex)
                {
                    logger.LogWarning(ex, "⚠️  Neo4j connection failed, using SQLite only");
                }
            }

            var hybridLogger = loggerFactory.CreateLogger<HybridMigrationRepository>();
            var migrationRepository = new HybridMigrationRepository(sqliteMigrationRepository, neo4jMigrationRepository, hybridLogger);
            await migrationRepository.InitializeAsync();

            // Cleanup stale runs on startup
            await migrationRepository.CleanupStaleRunsAsync();

            // Step 1: Run reverse engineering if requested (and not skipped)
            if (!skipReverseEngineering || reverseEngineerOnly)
            {
                // EnhancedLogger and ChatLogger already created above for API tracking

                // CobolAnalyzerAgent uses Responses API client (codex for code analysis)
                var cobolAnalyzerAgent = new CobolAnalyzerAgent(
                    responsesApiClient,
                    loggerFactory.CreateLogger<CobolAnalyzerAgent>(),
                    settings.AISettings.CobolAnalyzerModelId,
                    enhancedLogger,
                    chatLogger);

                // BusinessLogicExtractorAgent uses ResponsesApiClient (codex for RE reports)
                // This ensures compatibility with gpt-5.2-chat which requires Responses API
                var businessLogicExtractorAgent = new BusinessLogicExtractorAgent(
                    responsesApiClient,
                    loggerFactory.CreateLogger<BusinessLogicExtractorAgent>(),
                    chatDeployment,
                    enhancedLogger,
                    chatLogger);

                // Smart routing: check for large files to decide between chunked vs direct RE
                var cobolFiles = await fileHelper.ScanDirectoryForCobolFilesAsync(settings.ApplicationSettings.CobolSourceFolder);
                var hasLargeFiles = cobolFiles.Any(f => 
                    settings.ChunkingSettings.RequiresChunking(f.Content.Length, f.Content.Split('\n').Length));

                ReverseEngineeringResult reverseEngResult;

                if (hasLargeFiles)
                {
                    // Use ChunkedReverseEngineeringProcess for large files
                    Console.WriteLine("📦 Large files detected - using smart chunked reverse engineering");
                    logger.LogInformation("Large files detected, using ChunkedReverseEngineeringProcess");

                    var targetLang = settings.ApplicationSettings.TargetLanguage;
                    
                    var chunkingOrchestrator = new CobolToQuarkusMigration.Chunking.ChunkingOrchestrator(
                        settings.ChunkingSettings,
                        settings.ConversionSettings,
                        databasePath,
                        loggerFactory.CreateLogger<CobolToQuarkusMigration.Chunking.ChunkingOrchestrator>(),
                        targetLang);

                    var chunkedREProcess = new ChunkedReverseEngineeringProcess(
                        cobolAnalyzerAgent,
                        businessLogicExtractorAgent,
                        fileHelper,
                        settings.ChunkingSettings,
                        chunkingOrchestrator,
                        loggerFactory.CreateLogger<ChunkedReverseEngineeringProcess>(),
                        enhancedLogger,
                        databasePath);

                    reverseEngResult = await chunkedREProcess.RunAsync(
                        settings.ApplicationSettings.CobolSourceFolder,
                        reverseEngineerOutput,
                        (status, current, total) =>
                        {
                            Console.WriteLine($"{status} - {current}/{total}");
                        });
                }
                else
                {
                    // Use standard ReverseEngineeringProcess for small files
                    Console.WriteLine("⚡ All files below chunking threshold - using direct reverse engineering");

                    var reverseEngineeringProcess = new ReverseEngineeringProcess(
                        cobolAnalyzerAgent,
                        businessLogicExtractorAgent,
                        fileHelper,
                        loggerFactory.CreateLogger<ReverseEngineeringProcess>(),
                        enhancedLogger);

                    reverseEngResult = await reverseEngineeringProcess.RunAsync(
                        settings.ApplicationSettings.CobolSourceFolder,
                        reverseEngineerOutput,
                        (status, current, total) =>
                        {
                            Console.WriteLine($"{status} - {current}/{total}");
                        });
                }

                if (!reverseEngResult.Success)
                {
                    logger.LogError("Reverse engineering failed: {Error}", reverseEngResult.ErrorMessage);
                    Environment.Exit(1);
                }

                // If reverse-engineer-only mode, exit here
                if (reverseEngineerOnly)
                {
                    Console.WriteLine("Reverse engineering completed successfully. Skipping Java conversion as requested.");
                    return;
                }
            }

            // Step 2: Run code conversion (unless reverse-engineer-only mode)
            if (!reverseEngineerOnly)
            {
                // Determine output folder based on target language
                var targetLang = settings.ApplicationSettings.TargetLanguage;
                var outputFolder = targetLang == TargetLanguage.CSharp 
                    ? settings.ApplicationSettings.CSharpOutputFolder 
                    : settings.ApplicationSettings.JavaOutputFolder;

                // Ensure output folder has a default value
                if (string.IsNullOrEmpty(outputFolder))
                {
                    outputFolder = targetLang == TargetLanguage.CSharp ? "output/csharp" : "output/java";
                }

                var langName = targetLang == TargetLanguage.CSharp ? "C# .NET" : "Java Quarkus";
                
                // ═══════════════════════════════════════════════════════════════════
                // QUALITY GATE: Final verification before migration starts
                // ═══════════════════════════════════════════════════════════════════
                Console.WriteLine("");
                Console.WriteLine("═══════════════════════════════════════════════════════════════════");
                Console.WriteLine("                    🔒 QUALITY GATE: PRE-MIGRATION CHECK");
                Console.WriteLine("═══════════════════════════════════════════════════════════════════");
                Console.WriteLine($"  Target Language:  {langName}");
                Console.WriteLine($"  Output Folder:    {outputFolder}");
                Console.WriteLine($"  ENV Variable:     TARGET_LANGUAGE={Environment.GetEnvironmentVariable("TARGET_LANGUAGE") ?? "(not set)"}");
                Console.WriteLine($"  Smart Chunking:   ENABLED (auto-routes large files)");
                Console.WriteLine($"  Chunk Threshold:  {settings.ChunkingSettings.AutoChunkCharThreshold:N0} chars / {settings.ChunkingSettings.AutoChunkLineThreshold:N0} lines");
                
                // Verify env var matches what we're about to do
                var envLang = Environment.GetEnvironmentVariable("TARGET_LANGUAGE");
                if (!string.IsNullOrEmpty(envLang))
                {
                    var expectedCSharp = envLang.Equals("CSharp", StringComparison.OrdinalIgnoreCase) || 
                                         envLang.Equals("C#", StringComparison.OrdinalIgnoreCase);
                    var actualCSharp = targetLang == TargetLanguage.CSharp;
                    
                    if (expectedCSharp != actualCSharp)
                    {
                        Console.WriteLine($"  ❌ MISMATCH DETECTED!");
                        Console.WriteLine($"     ENV says: {(expectedCSharp ? "CSharp" : "Java")}");
                        Console.WriteLine($"     Settings say: {targetLang}");
                        Console.WriteLine("═══════════════════════════════════════════════════════════════════");
                        Console.WriteLine("❌ QUALITY GATE FAILED: Language mismatch between env var and settings!");
                        Console.WriteLine("   This indicates a bug in configuration loading. Aborting.");
                        Environment.Exit(1);
                    }
                }
                
                Console.WriteLine($"  ✅ Quality Gate PASSED");
                Console.WriteLine("═══════════════════════════════════════════════════════════════════");
                Console.WriteLine("");
                
                Console.WriteLine($"Starting COBOL to {langName} migration with Smart Orchestration...");
                Console.WriteLine($"Target language: {langName}");
                Console.WriteLine($"Output folder: {outputFolder}");

                // Use SmartMigrationOrchestrator for intelligent file routing
                // - Small files → direct MigrationProcess (fast, no chunking overhead)
                // - Large files → ChunkedMigrationProcess (preserves ALL code, no truncation)
                var smartOrchestrator = new SmartMigrationOrchestrator(
                    responsesApiClient,
                    chatClient,
                    loggerFactory,
                    fileHelper,
                    settings,
                    migrationRepository);

                var migrationStats = await smartOrchestrator.RunAsync(
                    settings.ApplicationSettings.CobolSourceFolder,
                    outputFolder,
                    (status, current, total) =>
                    {
                        Console.WriteLine($"{status} - {current}/{total}");
                    });

                Console.WriteLine("Migration process completed successfully.");
                Console.WriteLine($"  📊 Stats: {migrationStats.TotalFiles} files ({migrationStats.DirectFiles} direct, {migrationStats.ChunkedFiles} chunked)");
                if (migrationStats.TotalChunks > 0)
                {
                    Console.WriteLine($"  📦 Total chunks processed: {migrationStats.TotalChunks}");
                }
            }
        }
        catch (Exception ex)
        {
            logger.LogError(ex, "Error in migration process");
            Environment.Exit(1);
        }
    }

    private static void LoadEnvironmentVariables()
    {
        try
        {
            string currentDir = Directory.GetCurrentDirectory();
            string configDir = Path.Combine(currentDir, "Config");
            string localConfigFile = Path.Combine(configDir, "ai-config.local.env");
            string templateConfigFile = Path.Combine(configDir, "ai-config.env");

            if (File.Exists(templateConfigFile))
            {
                LoadEnvFile(templateConfigFile);
            }

            if (File.Exists(localConfigFile))
            {
                LoadEnvFile(localConfigFile);
            }
            else
            {
                Console.WriteLine("💡 Consider creating Config/ai-config.local.env for your personal settings");
                Console.WriteLine("   You can copy from Config/ai-config.local.env.template");
            }
        }
        catch (Exception ex)
        {
            Console.WriteLine($"❌ Error loading environment configuration: {ex.Message}");
        }
    }

    private static void LoadEnvFile(string filePath)
    {
        foreach (string line in File.ReadAllLines(filePath))
        {
            string trimmedLine = line.Trim();

            if (string.IsNullOrEmpty(trimmedLine) || trimmedLine.StartsWith('#'))
            {
                continue;
            }

            var parts = trimmedLine.Split('=', 2);
            if (parts.Length == 2)
            {
                string key = parts[0].Trim();
                string value = parts[1].Trim().Trim('"', '\'');
                
                // CRITICAL: Do NOT overwrite environment variables that are already set
                // This preserves values exported by doctor.sh (like TARGET_LANGUAGE)
                var existingValue = Environment.GetEnvironmentVariable(key);
                if (string.IsNullOrEmpty(existingValue))
                {
                    Environment.SetEnvironmentVariable(key, value);
                }
                else if (key == "TARGET_LANGUAGE")
                {
                    // Special case: Log when we preserve the user's TARGET_LANGUAGE selection
                    Console.WriteLine($"🔒 Preserving TARGET_LANGUAGE from shell: '{existingValue}' (ignoring config file value: '{value}')");
                }
            }
        }
    }

    private static void OverrideSettingsFromEnvironment(AppSettings settings)
    {
        var aiSettings = settings.AISettings ??= new AISettings();
        var applicationSettings = settings.ApplicationSettings ??= new ApplicationSettings();
        
        // Primary deployment (for codex models via Responses API)
        var endpoint = Environment.GetEnvironmentVariable("AZURE_OPENAI_ENDPOINT");
        if (!string.IsNullOrEmpty(endpoint))
        {
            aiSettings.Endpoint = endpoint;
        }

        var apiKey = Environment.GetEnvironmentVariable("AZURE_OPENAI_API_KEY");
        if (!string.IsNullOrEmpty(apiKey))
        {
            aiSettings.ApiKey = apiKey;
        }

        var deploymentName = Environment.GetEnvironmentVariable("AZURE_OPENAI_DEPLOYMENT_NAME");
        if (!string.IsNullOrEmpty(deploymentName))
        {
            aiSettings.DeploymentName = deploymentName;
        }

        var modelId = Environment.GetEnvironmentVariable("AZURE_OPENAI_MODEL_ID");
        if (!string.IsNullOrEmpty(modelId))
        {
            aiSettings.ModelId = modelId;
        }

        // Chat deployment (for gpt-5.1-chat models via Chat Completions API)
        var chatEndpoint = Environment.GetEnvironmentVariable("AZURE_OPENAI_CHAT_ENDPOINT");
        if (!string.IsNullOrEmpty(chatEndpoint))
        {
            aiSettings.ChatEndpoint = chatEndpoint;
        }

        var chatApiKey = Environment.GetEnvironmentVariable("AZURE_OPENAI_CHAT_API_KEY");
        if (!string.IsNullOrEmpty(chatApiKey))
        {
            aiSettings.ChatApiKey = chatApiKey;
        }

        var chatDeploymentName = Environment.GetEnvironmentVariable("AZURE_OPENAI_CHAT_DEPLOYMENT_NAME");
        if (!string.IsNullOrEmpty(chatDeploymentName))
        {
            aiSettings.ChatDeploymentName = chatDeploymentName;
        }

        var chatModelId = Environment.GetEnvironmentVariable("AZURE_OPENAI_CHAT_MODEL_ID");
        if (!string.IsNullOrEmpty(chatModelId))
        {
            aiSettings.ChatModelId = chatModelId;
        }

        // Specialized model IDs
        var cobolModel = Environment.GetEnvironmentVariable("AZURE_OPENAI_COBOL_ANALYZER_MODEL");
        if (!string.IsNullOrEmpty(cobolModel))
        {
            aiSettings.CobolAnalyzerModelId = cobolModel;
        }

        var javaModel = Environment.GetEnvironmentVariable("AZURE_OPENAI_JAVA_CONVERTER_MODEL");
        if (!string.IsNullOrEmpty(javaModel))
        {
            aiSettings.JavaConverterModelId = javaModel;
        }

        var depModel = Environment.GetEnvironmentVariable("AZURE_OPENAI_DEPENDENCY_MAPPER_MODEL");
        if (!string.IsNullOrEmpty(depModel))
        {
            aiSettings.DependencyMapperModelId = depModel;
        }

        var testModel = Environment.GetEnvironmentVariable("AZURE_OPENAI_UNIT_TEST_MODEL");
        if (!string.IsNullOrEmpty(testModel))
        {
            aiSettings.UnitTestModelId = testModel;
        }

        var serviceType = Environment.GetEnvironmentVariable("AZURE_OPENAI_SERVICE_TYPE");
        if (!string.IsNullOrEmpty(serviceType))
        {
            aiSettings.ServiceType = serviceType;
        }

        if (Environment.GetEnvironmentVariable("COBOL_SOURCE_FOLDER") is { Length: > 0 } cobolSource)
        {
            applicationSettings.CobolSourceFolder = cobolSource;
        }

        if (Environment.GetEnvironmentVariable("JAVA_OUTPUT_FOLDER") is { Length: > 0 } javaOutput)
        {
            applicationSettings.JavaOutputFolder = javaOutput;
        }

        if (Environment.GetEnvironmentVariable("TEST_OUTPUT_FOLDER") is { Length: > 0 } testOutput)
        {
            applicationSettings.TestOutputFolder = testOutput;
        }

        if (Environment.GetEnvironmentVariable("MIGRATION_DB_PATH") is { Length: > 0 } migrationDb)
        {
            applicationSettings.MigrationDatabasePath = migrationDb;
        }

        // Target language selection (from doctor.sh or config file)
        if (Environment.GetEnvironmentVariable("TARGET_LANGUAGE") is { Length: > 0 } targetLang)
        {
            var trimmedLang = targetLang.Trim();
            var isCSharp = trimmedLang.Equals("CSharp", StringComparison.OrdinalIgnoreCase) ||
                           trimmedLang.Equals("C#", StringComparison.OrdinalIgnoreCase);
            applicationSettings.TargetLanguage = isCSharp ? TargetLanguage.CSharp : TargetLanguage.Java;
            
            // Quality Gate: Verify the language was correctly parsed
            Console.WriteLine($"🎯 TARGET_LANGUAGE env var: '{trimmedLang}' → {applicationSettings.TargetLanguage}");
            Console.WriteLine($"🔒 Quality Gate: Language selection = {applicationSettings.TargetLanguage}");
            
            // Additional verification: Check if the env var value matches expected values
            if (!trimmedLang.Equals("Java", StringComparison.OrdinalIgnoreCase) &&
                !trimmedLang.Equals("CSharp", StringComparison.OrdinalIgnoreCase) &&
                !trimmedLang.Equals("C#", StringComparison.OrdinalIgnoreCase))
            {
                Console.WriteLine($"⚠️  WARNING: TARGET_LANGUAGE='{trimmedLang}' is not a recognized value.");
                Console.WriteLine($"   Expected: 'Java' or 'CSharp'. Falling back to: {applicationSettings.TargetLanguage}");
            }
        }
        else
        {
            Console.WriteLine($"🎯 TARGET_LANGUAGE env var not set, using config default: {applicationSettings.TargetLanguage}");
        }

        // C# output folder (separate from Java output)
        if (Environment.GetEnvironmentVariable("CSHARP_OUTPUT_FOLDER") is { Length: > 0 } csharpOutput)
        {
            applicationSettings.CSharpOutputFolder = csharpOutput;
        }
    }

    private static bool ValidateAndLoadConfiguration()
    {
        try
        {
            LoadEnvironmentVariables();

            var requiredSettings = new Dictionary<string, string?>
            {
                ["AZURE_OPENAI_ENDPOINT"] = Environment.GetEnvironmentVariable("AZURE_OPENAI_ENDPOINT"),
                ["AZURE_OPENAI_API_KEY"] = Environment.GetEnvironmentVariable("AZURE_OPENAI_API_KEY"),
                ["AZURE_OPENAI_DEPLOYMENT_NAME"] = Environment.GetEnvironmentVariable("AZURE_OPENAI_DEPLOYMENT_NAME"),
                ["AZURE_OPENAI_MODEL_ID"] = Environment.GetEnvironmentVariable("AZURE_OPENAI_MODEL_ID")
            };

            var missingSettings = new List<string>();
            var invalidSettings = new List<string>();

            foreach (var setting in requiredSettings)
            {
                if (string.IsNullOrWhiteSpace(setting.Value))
                {
                    missingSettings.Add(setting.Key);
                }
                else
                {
                    if (setting.Key == "AZURE_OPENAI_ENDPOINT" && !Uri.TryCreate(setting.Value, UriKind.Absolute, out _))
                    {
                        invalidSettings.Add($"{setting.Key} (invalid URL format)");
                    }
                    else if (setting.Key == "AZURE_OPENAI_API_KEY" && setting.Value.Contains("your-api-key"))
                    {
                        invalidSettings.Add($"{setting.Key} (contains template placeholder)");
                    }
                    else if (setting.Key == "AZURE_OPENAI_ENDPOINT" && setting.Value.Contains("your-resource"))
                    {
                        invalidSettings.Add($"{setting.Key} (contains template placeholder)");
                    }
                }
            }

            if (missingSettings.Any() || invalidSettings.Any())
            {
                Console.WriteLine("❌ Configuration Validation Failed");
                Console.WriteLine("=====================================");

                if (missingSettings.Any())
                {
                    Console.WriteLine("Missing required settings:");
                    foreach (var setting in missingSettings)
                    {
                        Console.WriteLine($"  • {setting}");
                    }

                    Console.WriteLine();
                }

                if (invalidSettings.Any())
                {
                    Console.WriteLine("Invalid settings detected:");
                    foreach (var setting in invalidSettings)
                    {
                        Console.WriteLine($"  • {setting}");
                    }

                    Console.WriteLine();
                }

                Console.WriteLine("Configuration Setup Instructions:");
                Console.WriteLine("1. Run: ./setup.sh (for interactive setup)");
                Console.WriteLine("2. Or manually copy Config/ai-config.local.env.template to Config/ai-config.local.env");
                Console.WriteLine("3. Edit Config/ai-config.local.env with your actual Azure OpenAI credentials");
                Console.WriteLine("4. Ensure your model deployment names match your Azure OpenAI setup");
                Console.WriteLine();
                Console.WriteLine("For detailed instructions, see: CONFIGURATION_GUIDE.md");

                return false;
            }

            var endpoint = Environment.GetEnvironmentVariable("AZURE_OPENAI_ENDPOINT");
            var modelId = Environment.GetEnvironmentVariable("AZURE_OPENAI_MODEL_ID");
            var deployment = Environment.GetEnvironmentVariable("AZURE_OPENAI_DEPLOYMENT_NAME");
            var apiKey = Environment.GetEnvironmentVariable("AZURE_OPENAI_API_KEY");

            Console.WriteLine("✅ Configuration Validation Successful (Agent Framework)");
            Console.WriteLine("=====================================");
            Console.WriteLine($"Endpoint: {endpoint}");
            Console.WriteLine($"Model: {modelId}");
            Console.WriteLine($"Deployment: {deployment}");
            Console.WriteLine($"API Key: {apiKey?.Substring(0, Math.Min(8, apiKey.Length))}... ({apiKey?.Length} chars)");
            Console.WriteLine();

            return true;
        }
        catch (Exception ex)
        {
            Console.WriteLine($"❌ Error during configuration validation: {ex.Message}");
            Console.WriteLine("Please check your configuration files and try again.");
            return false;
        }
    }

    private static async Task RunReverseEngineeringAsync(ILoggerFactory loggerFactory, FileHelper fileHelper, SettingsHelper settingsHelper, string cobolSource, string output, string configPath)
    {
        var logger = loggerFactory.CreateLogger("ReverseEngineering");

        try
        {
            logger.LogInformation("Loading settings from {ConfigPath}", configPath);
            AppSettings? loadedSettings = await settingsHelper.LoadSettingsAsync<AppSettings>(configPath);
            var settings = loadedSettings ?? new AppSettings();

            LoadEnvironmentVariables();
            OverrideSettingsFromEnvironment(settings);

            // Override with CLI arguments
            if (!string.IsNullOrEmpty(cobolSource))
            {
                settings.ApplicationSettings.CobolSourceFolder = cobolSource;
            }

            if (string.IsNullOrEmpty(settings.ApplicationSettings.CobolSourceFolder))
            {
                logger.LogError("COBOL source folder not specified. Use --source option.");
                Environment.Exit(1);
            }

            if (string.IsNullOrEmpty(settings.AISettings.ApiKey) ||
                string.IsNullOrEmpty(settings.AISettings.Endpoint) ||
                string.IsNullOrEmpty(settings.AISettings.DeploymentName))
            {
                logger.LogError("Azure OpenAI configuration incomplete. Please ensure API key, endpoint, and deployment name are configured.");
                Environment.Exit(1);
            }

            // Create EnhancedLogger and ChatLogger early for API tracking
            var enhancedLogger = new EnhancedLogger(
                loggerFactory.CreateLogger<EnhancedLogger>());
            var chatLogger = new ChatLogger(
                loggerFactory.CreateLogger<ChatLogger>());

            // Get API version from environment or default
            var apiVersion = Environment.GetEnvironmentVariable("AZURE_OPENAI_API_VERSION") ?? "2025-04-01-preview";

            // Create ResponsesApiClient for code agents (codex models via Responses API)
            var responsesApiClient = new ResponsesApiClient(
                settings.AISettings.Endpoint,
                settings.AISettings.ApiKey,
                settings.AISettings.DeploymentName,
                loggerFactory.CreateLogger<ResponsesApiClient>(),
                enhancedLogger,
                apiVersion: apiVersion);  // Pass EnhancedLogger for API call tracking

            logger.LogInformation("ResponsesApiClient initialized for codex model: {DeploymentName} (API: {ApiVersion})", 
                settings.AISettings.DeploymentName, apiVersion);

            // Create IChatClient for chat agents (RE reports via Chat Completions API)
            var chatEndpoint = settings.AISettings.ChatEndpoint ?? settings.AISettings.Endpoint;
            var chatApiKey = settings.AISettings.ChatApiKey ?? settings.AISettings.ApiKey;
            var chatDeployment = settings.AISettings.ChatDeploymentName ?? settings.AISettings.DeploymentName;

            IChatClient chatClient = ChatClientFactory.CreateAzureOpenAIChatClient(chatEndpoint, chatApiKey, chatDeployment);
            logger.LogInformation("IChatClient initialized for chat model: {ChatDeployment}", chatDeployment);

            // CobolAnalyzerAgent uses ResponsesApiClient (codex for code analysis)
            var cobolAnalyzerAgent = new CobolAnalyzerAgent(
                responsesApiClient,
                loggerFactory.CreateLogger<CobolAnalyzerAgent>(),
                settings.AISettings.CobolAnalyzerModelId,
                enhancedLogger,
                chatLogger);

            // BusinessLogicExtractorAgent uses ResponsesApiClient (codex for RE reports)
            // This ensures compatibility with gpt-5.2-chat which requires Responses API
            var businessLogicExtractorAgent = new BusinessLogicExtractorAgent(
                responsesApiClient,
                loggerFactory.CreateLogger<BusinessLogicExtractorAgent>(),
                chatDeployment,
                enhancedLogger,
                chatLogger);

            // Use ReverseEngineeringProcess with agents
            var reverseEngineeringProcess = new ReverseEngineeringProcess(
                cobolAnalyzerAgent,
                businessLogicExtractorAgent,
                fileHelper,
                loggerFactory.CreateLogger<ReverseEngineeringProcess>(),
                enhancedLogger);

            Console.WriteLine("Starting reverse engineering process...");
            Console.WriteLine();

            var result = await reverseEngineeringProcess.RunAsync(
                settings.ApplicationSettings.CobolSourceFolder,
                output,
                (status, current, total) =>
                {
                    Console.WriteLine($"{status} - {current}/{total}");
                });

            Console.WriteLine();
            Console.WriteLine("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
            Console.WriteLine("✨ Reverse Engineering Complete!");
            Console.WriteLine("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
            Console.WriteLine();
            Console.WriteLine($"📊 Summary:");
            Console.WriteLine($"   • Files Analyzed: {result.TotalFilesAnalyzed}");
            Console.WriteLine($"   • Feature Descriptions: {result.TotalUserStories}");
            Console.WriteLine($"   • Features: {result.TotalFeatures}");
            Console.WriteLine($"   • Business Rules: {result.TotalBusinessRules}");
            Console.WriteLine();
            Console.WriteLine($"📁 Output Location: {Path.GetFullPath(output)}");
            Console.WriteLine("   • reverse-engineering-details.md - Complete analysis with business logic and technical details");
            Console.WriteLine();
            Console.WriteLine("🎯 Next Steps:");
            Console.WriteLine("   1. Review the generated documentation");
            Console.WriteLine("   2. Decide on your modernization strategy");
            Console.WriteLine("   3. Run full migration if desired: dotnet run --source <path> --java-output <path>");
            Console.WriteLine();
        }
        catch (Exception ex)
        {
            logger.LogError(ex, "Error during reverse engineering");
            Environment.Exit(1);
        }
    }
}
