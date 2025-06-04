namespace CobolToQuarkusMigration.Models;

/// <summary>
/// Represents the application settings.
/// </summary>
public class AppSettings
{
    /// <summary>
    /// Gets or sets the AI settings.
    /// </summary>
    public AISettings AISettings { get; set; } = new AISettings();
    
    /// <summary>
    /// Gets or sets the application-specific settings.
    /// </summary>
    public ApplicationSettings ApplicationSettings { get; set; } = new ApplicationSettings();

    /// <summary>
    /// Gets or sets the chat logging settings.
    /// </summary>
    public ChatLoggingSettings ChatLogging { get; set; } = new ChatLoggingSettings();

    /// <summary>
    /// Gets or sets the API call logging settings.
    /// </summary>
    public ApiCallLoggingSettings ApiCallLogging { get; set; } = new ApiCallLoggingSettings();
}

/// <summary>
/// Represents the AI-specific settings.
/// </summary>
public class AISettings
{
    /// <summary>
    /// Gets or sets the service type (e.g., OpenAI, Azure OpenAI).
    /// </summary>
    public string ServiceType { get; set; } = "OpenAI";
    
    /// <summary>
    /// Gets or sets the endpoint for the AI service.
    /// </summary>
    public string Endpoint { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the API key for the AI service.
    /// </summary>
    public string ApiKey { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the model ID for general use.
    /// </summary>
    public string ModelId { get; set; } = "gpt-4.1";
    
    /// <summary>
    /// Gets or sets the model ID for the COBOL analyzer.
    /// </summary>
    public string CobolAnalyzerModelId { get; set; } = "gpt-4.1";
    
    /// <summary>
    /// Gets or sets the model ID for the Java converter.
    /// <summary>
    /// Gets or sets the model ID for the Java converter.
    /// </summary>
    public string JavaConverterModelId { get; set; } = "gpt-4.1";
    
    /// <summary>
    /// Gets or sets the model ID for the dependency mapper.
    /// </summary>
    public string? DependencyMapperModelId { get; set; }
    
    /// <summary>
    /// Gets or sets the model ID for the unit test generator.
    /// </summary>
    public string UnitTestModelId { get; set; } = "gpt-4.1";

    /// <summary>
    /// Gets or sets the deployment name for Azure OpenAI.
    /// </summary>
    public string DeploymentName { get; set; } = "gpt-4.1";

    /// <summary>
    /// Gets or sets the maximum number of tokens for AI responses.
    /// </summary>
    public int MaxTokens { get; set; } = 4000;

    /// <summary>
    /// Gets or sets the temperature for AI responses (0.0 to 2.0).
    /// </summary>
    public double Temperature { get; set; } = 0.1;
}

/// <summary>
/// Represents chat logging settings.
/// </summary>
public class ChatLoggingSettings
{
    /// <summary>
    /// Gets or sets whether chat logging is enabled.
    /// </summary>
    public bool Enabled { get; set; } = true;
}

/// <summary>
/// Represents API call logging settings.
/// </summary>
public class ApiCallLoggingSettings
{
    /// <summary>
    /// Gets or sets whether API call logging is enabled.
    /// </summary>
    public bool Enabled { get; set; } = true;
}

/// <summary>
/// Represents the application-specific settings.
/// </summary>
public class ApplicationSettings
{
    /// <summary>
    /// Gets or sets the folder containing COBOL source files.
    /// </summary>
    public string CobolSourceFolder { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the folder for Java output files.
    /// </summary>
    public string JavaOutputFolder { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the folder for test output files.
    /// </summary>
    public string TestOutputFolder { get; set; } = string.Empty;
}
