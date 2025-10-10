using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Mcp;
using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Persistence;
using Microsoft.Extensions.Logging;
using Microsoft.SemanticKernel;
using System.CommandLine;

namespace CobolToQuarkusMigration;

internal static class Program
{
    public static async Task<int> Main(string[] args)
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

    private static RootCommand BuildRootCommand(ILoggerFactory loggerFactory, ILogger logger, FileHelper fileHelper, SettingsHelper settingsHelper)
    {
        var rootCommand = new RootCommand("COBOL to Java Quarkus Migration Tool");

        var cobolSourceOption = new Option<string>("--cobol-source", "Path to the folder containing COBOL source files and copybooks")
        {
            Arity = ArgumentArity.ZeroOrOne
        };
        cobolSourceOption.AddAlias("-s");
        rootCommand.AddOption(cobolSourceOption);

        var javaOutputOption = new Option<string>("--java-output", "Path to the folder for Java output files")
        {
            Arity = ArgumentArity.ZeroOrOne
        };
        javaOutputOption.AddAlias("-j");
        rootCommand.AddOption(javaOutputOption);

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

        rootCommand.SetHandler(async (string cobolSource, string javaOutput, string configPath) =>
        {
            await RunMigrationAsync(loggerFactory, logger, fileHelper, settingsHelper, cobolSource, javaOutput, configPath);
        }, cobolSourceOption, javaOutputOption, configOption);

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

        var runIdOption = new Option<int?>("--run-id", description: "Specific migration run id to expose")
        {
            Arity = ArgumentArity.ZeroOrOne
        };
        mcpCommand.AddOption(runIdOption);

        var mcpConfigOption = new Option<string>("--config", () => "Config/appsettings.json", "Path to the configuration file");
        mcpCommand.AddOption(mcpConfigOption);

        mcpCommand.SetHandler(async (int? runId, string configPath) =>
        {
            await RunMcpServerAsync(loggerFactory, settingsHelper, runId, configPath);
        }, runIdOption, mcpConfigOption);

        return mcpCommand;
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

            var serverLogger = loggerFactory.CreateLogger<McpServer>();
            var server = new McpServer(repository, targetRunId.Value, serverLogger, settings.AISettings);
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

    private static async Task RunMigrationAsync(ILoggerFactory loggerFactory, ILogger logger, FileHelper fileHelper, SettingsHelper settingsHelper, string cobolSource, string javaOutput, string configPath)
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
                logger.LogError("COBOL source folder not specified. Use --cobol-source option or set in config file.");
                Environment.Exit(1);
            }

            if (string.IsNullOrEmpty(settings.ApplicationSettings.JavaOutputFolder))
            {
                logger.LogError("Java output folder not specified. Use --java-output option or set in config file.");
                Environment.Exit(1);
            }

            if (string.IsNullOrEmpty(settings.AISettings.ApiKey) ||
                string.IsNullOrEmpty(settings.AISettings.Endpoint) ||
                string.IsNullOrEmpty(settings.AISettings.DeploymentName))
            {
                logger.LogError("Azure OpenAI configuration incomplete. Please ensure API key, endpoint, and deployment name are configured.");
                logger.LogError("You can set them in Config/ai-config.local.env or as environment variables.");
                Environment.Exit(1);
            }

            var kernelBuilder = Kernel.CreateBuilder();

            if (settings.AISettings.ServiceType.Equals("OpenAI", StringComparison.OrdinalIgnoreCase))
            {
                kernelBuilder.AddOpenAIChatCompletion(
                    modelId: settings.AISettings.ModelId,
                    apiKey: settings.AISettings.ApiKey);
            }
            else if (settings.AISettings.ServiceType.Equals("AzureOpenAI", StringComparison.OrdinalIgnoreCase))
            {
                var httpClient = new HttpClient
                {
                    Timeout = TimeSpan.FromMinutes(10)
                };

                kernelBuilder.AddAzureOpenAIChatCompletion(
                    deploymentName: settings.AISettings.DeploymentName,
                    endpoint: settings.AISettings.Endpoint,
                    apiKey: settings.AISettings.ApiKey,
                    httpClient: httpClient);

                logger.LogInformation("Using Azure OpenAI service with endpoint: {Endpoint} and deployment: {DeploymentName}",
                    settings.AISettings.Endpoint,
                    settings.AISettings.DeploymentName);
            }
            else
            {
                logger.LogError("Unsupported AI service type: {ServiceType}", settings.AISettings.ServiceType);
                Environment.Exit(1);
            }

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

            var migrationProcess = new MigrationProcess(
                kernelBuilder,
                loggerFactory.CreateLogger<MigrationProcess>(),
                fileHelper,
                settings,
                migrationRepository);

            migrationProcess.InitializeAgents();

            Console.WriteLine("Starting COBOL to Java Quarkus migration process...");

            await migrationProcess.RunAsync(
                settings.ApplicationSettings.CobolSourceFolder,
                settings.ApplicationSettings.JavaOutputFolder,
                (status, current, total) =>
                {
                    Console.WriteLine($"{status} - {current}/{total}");
                });

            Console.WriteLine("Migration process completed successfully.");
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
                Environment.SetEnvironmentVariable(key, value);
            }
        }
    }

    private static void OverrideSettingsFromEnvironment(AppSettings settings)
    {
        var aiSettings = settings.AISettings ??= new AISettings();
        var applicationSettings = settings.ApplicationSettings ??= new ApplicationSettings();
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

            Console.WriteLine("✅ Configuration Validation Successful");
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
}
