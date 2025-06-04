using Microsoft.Extensions.Logging;
using Microsoft.SemanticKernel;
using System.CommandLine;
using CobolToQuarkusMigration;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;

// Create logger factory
var loggerFactory = LoggerFactory.Create(builder =>
{
    builder.AddConsole();
});

var logger = loggerFactory.CreateLogger<Program>();
var fileHelper = new FileHelper(loggerFactory.CreateLogger<FileHelper>());
var settingsHelper = new SettingsHelper(loggerFactory.CreateLogger<SettingsHelper>());

// Load and validate configuration early
if (!ValidateAndLoadConfiguration())
{
    Environment.Exit(1);
}

// Parse command line arguments
var rootCommand = new RootCommand("COBOL to Java Quarkus Migration Tool");

// Define command-line options
var cobolSourceOption = new Option<string>(
    "--cobol-source",
    "Path to the folder containing COBOL source files and copybooks");
cobolSourceOption.AddAlias("-s");
rootCommand.AddOption(cobolSourceOption);

var javaOutputOption = new Option<string>(
    "--java-output",
    "Path to the folder for Java output files");
javaOutputOption.AddAlias("-j");
rootCommand.AddOption(javaOutputOption);

var configOption = new Option<string>(
    "--config",
    () => "Config/appsettings.json",
    "Path to the configuration file");
configOption.AddAlias("-c");
rootCommand.AddOption(configOption);

// Add conversation log generation command
var conversationCommand = new Command("conversation", "Generate a readable conversation log from migration logs");

var sessionIdOption = new Option<string>(
    "--session-id",
    "Specific session ID to generate conversation for (optional)");
sessionIdOption.AddAlias("-sid");
conversationCommand.AddOption(sessionIdOption);

var logDirOption = new Option<string>(
    "--log-dir",
    () => "Logs",
    "Path to the logs directory");
logDirOption.AddAlias("-ld");
conversationCommand.AddOption(logDirOption);

var liveOption = new Option<bool>(
    "--live",
    () => false,
    "Enable live conversation feed that updates in real-time");
liveOption.AddAlias("-l");
conversationCommand.AddOption(liveOption);

conversationCommand.SetHandler(async (string sessionId, string logDir, bool live) =>
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
            
            // Keep the application running for live updates
            await Task.Delay(-1);
        }
        else
        {
            outputPath = await logCombiner.CreateConversationNarrativeAsync(sessionId);
            Console.WriteLine($"✅ Conversation narrative created: {outputPath}");
            
            // Show preview of the conversation
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
        logger.LogError(ex, "Error generating conversation log");
        Environment.Exit(1);
    }
}, sessionIdOption, logDirOption, liveOption);

rootCommand.AddCommand(conversationCommand);

// Set up the command handler
rootCommand.SetHandler(async (string cobolSource, string javaOutput, string configPath) =>
{
    try
    {
        // Load settings
        logger.LogInformation("Loading settings from {ConfigPath}", configPath);
        var settings = await settingsHelper.LoadSettingsAsync<AppSettings>(configPath);
        
        // Load environment variables from centralized config
        LoadEnvironmentVariables();
        
        // Override settings with environment variables if they exist
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
        
        // Validate API configuration
        if (string.IsNullOrEmpty(settings.AISettings.ApiKey) || 
            string.IsNullOrEmpty(settings.AISettings.Endpoint) ||
            string.IsNullOrEmpty(settings.AISettings.DeploymentName))
        {
            logger.LogError("Azure OpenAI configuration incomplete. Please ensure API key, endpoint, and deployment name are configured.");
            logger.LogError("You can set them in Config/ai-config.local.env or as environment variables.");
            Environment.Exit(1);
        }
        
        // Initialize kernel builder
        var kernelBuilder = Kernel.CreateBuilder();
        
        if (settings.AISettings.ServiceType.Equals("OpenAI", StringComparison.OrdinalIgnoreCase))
        {
            kernelBuilder.AddOpenAIChatCompletion(
                modelId: settings.AISettings.ModelId,
                apiKey: settings.AISettings.ApiKey);
        }
        else if (settings.AISettings.ServiceType.Equals("AzureOpenAI", StringComparison.OrdinalIgnoreCase))
        {
            // Create HttpClient with extended timeout for large COBOL files
            var httpClient = new HttpClient();
            httpClient.Timeout = TimeSpan.FromMinutes(10); // 10 minute timeout instead of default 100 seconds
            
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
        
        // Initialize migration process
        var migrationProcess = new MigrationProcess(
            kernelBuilder,
            loggerFactory.CreateLogger<MigrationProcess>(),
            fileHelper,
            settings);
        
        migrationProcess.InitializeAgents();
        
        // Run migration process
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
}, cobolSourceOption, javaOutputOption, configOption);

// Execute the command
return await rootCommand.InvokeAsync(args);

/// <summary>
/// Loads environment variables from the centralized configuration system
/// </summary>
static void LoadEnvironmentVariables()
{
    try
    {
        // Get current directory and config directory paths
        string currentDir = Directory.GetCurrentDirectory();
        string configDir = Path.Combine(currentDir, "Config"); // Use current directory instead of base directory
        string localConfigFile = Path.Combine(configDir, "ai-config.local.env");
        string templateConfigFile = Path.Combine(configDir, "ai-config.env");
        
        // Load template configuration first (provides defaults)
        if (File.Exists(templateConfigFile))
        {
            LoadEnvFile(templateConfigFile);
        }
        
        // Load local configuration (overrides template)
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

/// <summary>
/// Loads environment variables from a .env file
/// </summary>
static void LoadEnvFile(string filePath)
{
    foreach (string line in File.ReadAllLines(filePath))
    {
        string trimmedLine = line.Trim();
        
        // Skip comments and empty lines
        if (string.IsNullOrEmpty(trimmedLine) || trimmedLine.StartsWith('#'))
            continue;
            
        var parts = trimmedLine.Split('=', 2);
        if (parts.Length == 2)
        {
            string key = parts[0].Trim();
            string value = parts[1].Trim().Trim('"', '\'');
            
            // Set the environment variable (local config overrides template config)
            Environment.SetEnvironmentVariable(key, value);
        }
    }
}

/// <summary>
/// Overrides configuration settings with environment variables if they exist
/// </summary>
static void OverrideSettingsFromEnvironment(AppSettings settings)
{
    // AI Settings
    var endpoint = Environment.GetEnvironmentVariable("AZURE_OPENAI_ENDPOINT");
    if (!string.IsNullOrEmpty(endpoint))
        settings.AISettings.Endpoint = endpoint;
        
    var apiKey = Environment.GetEnvironmentVariable("AZURE_OPENAI_API_KEY");
    if (!string.IsNullOrEmpty(apiKey))
        settings.AISettings.ApiKey = apiKey;
        
    var deploymentName = Environment.GetEnvironmentVariable("AZURE_OPENAI_DEPLOYMENT_NAME");
    if (!string.IsNullOrEmpty(deploymentName))
        settings.AISettings.DeploymentName = deploymentName;
        
    var modelId = Environment.GetEnvironmentVariable("AZURE_OPENAI_MODEL_ID");
    if (!string.IsNullOrEmpty(modelId))
        settings.AISettings.ModelId = modelId;
        
    var cobolModel = Environment.GetEnvironmentVariable("AZURE_OPENAI_COBOL_ANALYZER_MODEL");
    if (!string.IsNullOrEmpty(cobolModel))
        settings.AISettings.CobolAnalyzerModelId = cobolModel;
        
    var javaModel = Environment.GetEnvironmentVariable("AZURE_OPENAI_JAVA_CONVERTER_MODEL");
    if (!string.IsNullOrEmpty(javaModel))
        settings.AISettings.JavaConverterModelId = javaModel;
        
    var depModel = Environment.GetEnvironmentVariable("AZURE_OPENAI_DEPENDENCY_MAPPER_MODEL");
    if (!string.IsNullOrEmpty(depModel))
        settings.AISettings.DependencyMapperModelId = depModel;
        
    var testModel = Environment.GetEnvironmentVariable("AZURE_OPENAI_UNIT_TEST_MODEL");
    if (!string.IsNullOrEmpty(testModel))
        settings.AISettings.UnitTestModelId = testModel;
        
    var serviceType = Environment.GetEnvironmentVariable("AZURE_OPENAI_SERVICE_TYPE");
    if (!string.IsNullOrEmpty(serviceType))
        settings.AISettings.ServiceType = serviceType;
        
    // Application Settings
    var cobolSource = Environment.GetEnvironmentVariable("COBOL_SOURCE_FOLDER");
    if (!string.IsNullOrEmpty(cobolSource))
        settings.ApplicationSettings.CobolSourceFolder = cobolSource;
        
    var javaOutput = Environment.GetEnvironmentVariable("JAVA_OUTPUT_FOLDER");
    if (!string.IsNullOrEmpty(javaOutput))
        settings.ApplicationSettings.JavaOutputFolder = javaOutput;
        
    var testOutput = Environment.GetEnvironmentVariable("TEST_OUTPUT_FOLDER");
    if (!string.IsNullOrEmpty(testOutput))
        settings.ApplicationSettings.TestOutputFolder = testOutput;
}

/// <summary>
/// Validates and loads the centralized configuration with detailed error reporting
/// </summary>
/// <returns>True if configuration is valid and loaded successfully</returns>
static bool ValidateAndLoadConfiguration()
{
    try
    {
        // Load environment variables from centralized config files
        LoadEnvironmentVariables();
        
        // Validate required Azure OpenAI settings
        var requiredSettings = new Dictionary<string, string>
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
                // Additional validation
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

        // Display configuration summary for validation
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
