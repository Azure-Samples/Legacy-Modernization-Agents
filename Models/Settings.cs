using System.IO;
using System.Text.Json.Serialization;

namespace CobolToQuarkusMigration.Models;

/// <summary>
/// Represents the target language for code conversion.
/// </summary>
[JsonConverter(typeof(JsonStringEnumConverter))]
public enum TargetLanguage
{
    /// <summary>
    /// Java with Quarkus framework.
    /// </summary>
    Java,

    /// <summary>
    /// C# with .NET.
    /// </summary>
    CSharp
}

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
    /// Gets or sets the chunking settings for large file processing.
    /// </summary>
    public ChunkingSettings ChunkingSettings { get; set; } = new ChunkingSettings();

    /// <summary>
    /// Gets or sets the conversion settings for naming and consistency.
    /// </summary>
    public ConversionSettings ConversionSettings { get; set; } = new ConversionSettings();

    /// <summary>
    /// Gets or sets the chat logging settings.
    /// </summary>
    public ChatLoggingSettings ChatLogging { get; set; } = new ChatLoggingSettings();

    /// <summary>
    /// Gets or sets the API call logging settings.
    /// </summary>
    public ApiCallLoggingSettings ApiCallLogging { get; set; } = new ApiCallLoggingSettings();

    /// <summary>
    /// Gets or sets the assembly settings for file organization, namespaces, and class splitting.
    /// Controls how converted code is organized into files, packages/namespaces, and classes.
    /// </summary>
    public AssemblySettings AssemblySettings { get; set; } = new AssemblySettings();

    /// <summary>
    /// Gets or sets the model profile for Codex/Responses API models.
    /// Loaded from appsettings.json "CodexProfile" section.
    /// </summary>
    public ModelProfileSettings CodexProfile { get; set; } = new ModelProfileSettings();

    /// <summary>
    /// Gets or sets the model profile for Chat Completions API models.
    /// Loaded from appsettings.json "ChatProfile" section.
    /// </summary>
    public ModelProfileSettings ChatProfile { get; set; } = new ModelProfileSettings();
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
    /// Must be configured in appsettings.json or env vars — no default model name.
    /// </summary>
    public string ModelId { get; set; } = string.Empty;

    /// <summary>
    /// Gets or sets the model ID for the COBOL analyzer.
    /// Falls back to ModelId when empty.
    /// </summary>
    public string CobolAnalyzerModelId { get; set; } = string.Empty;

    /// <summary>
    /// Gets or sets the model ID for the Java converter.
    /// </summary>
    public string JavaConverterModelId { get; set; } = string.Empty;

    /// <summary>
    /// Gets or sets the model ID for the dependency mapper.
    /// </summary>
    public string? DependencyMapperModelId { get; set; }

    /// <summary>
    /// Gets or sets the model ID for the unit test generator.
    /// Falls back to ModelId when empty.
    /// </summary>
    public string UnitTestModelId { get; set; } = string.Empty;

    /// <summary>
    /// Gets or sets the deployment name for Azure OpenAI.
    /// Must be configured in appsettings.json or env vars — no default deployment name.
    /// </summary>
    public string DeploymentName { get; set; } = string.Empty;

    // Optional chat-specific settings (used for portal/chat/report); falls back to DeploymentName/Endpoint/ApiKey when not set
    public string ChatDeploymentName { get; set; } = string.Empty;
    public string ChatModelId { get; set; } = string.Empty;
    public string ChatEndpoint { get; set; } = string.Empty;
    public string ChatApiKey { get; set; } = string.Empty;

    /// <summary>
    /// Gets or sets the maximum number of tokens for AI responses.
    /// </summary>
    public int MaxTokens { get; set; } = 4000;

    /// <summary>
    /// Optional: The estimated context window size of the model (e.g., 128000 for gpt-4o).
    /// Used to intelligently configure chunking thresholds.
    /// If not present, the system will attempt to detect it from the model name.
    /// </summary>
    public int? ContextWindowSize { get; set; }
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
    /// Gets or sets the folder for C# output files.
    /// </summary>
    public string CSharpOutputFolder { get; set; } = string.Empty;

    /// <summary>
    /// Gets or sets the folder for test output files.
    /// </summary>
    public string TestOutputFolder { get; set; } = string.Empty;

    /// <summary>
    /// Gets or sets the target language for code conversion.
    /// </summary>
    public TargetLanguage TargetLanguage { get; set; } = TargetLanguage.Java;

    /// <summary>
    /// Gets or sets the path to the migration insights database.
    /// </summary>
    public string MigrationDatabasePath { get; set; } = Path.Combine("Data", "migration.db");

    /// <summary>
    /// Gets or sets the Neo4j graph database settings.
    /// </summary>
    public Neo4jSettings? Neo4j { get; set; }
}

/// <summary>
/// Represents Neo4j graph database settings.
/// </summary>
public class Neo4jSettings
{
    /// <summary>
    /// Gets or sets whether Neo4j integration is enabled.
    /// </summary>
    public bool Enabled { get; set; } = false;

    /// <summary>
    /// Gets or sets the Neo4j connection URI (e.g., bolt://localhost:7687).
    /// </summary>
    public string Uri { get; set; } = "bolt://localhost:7687";

    /// <summary>
    /// Gets or sets the Neo4j username.
    /// </summary>
    public string Username { get; set; } = "neo4j";

    /// <summary>
    /// Gets or sets the Neo4j password.
    /// </summary>
    public string Password { get; set; } = string.Empty;

    /// <summary>
    /// Gets or sets the database name (default is "neo4j").
    /// </summary>
    public string Database { get; set; } = "neo4j";
}

// ============================================================================
// Three-Tier Content-Aware Reasoning — Model Profile Settings
// ============================================================================

/// <summary>
/// A single complexity indicator: a regex pattern with a weight.
/// Matched against COBOL source to calculate a complexity score.
/// Loaded from appsettings.json CodexProfile.ComplexityIndicators array.
/// </summary>
public class ComplexityIndicator
{
    /// <summary>
    /// Regex pattern to match in the COBOL source (case-insensitive).
    /// Example: "EXEC\\s+SQL", "EXEC\\s+CICS", "PERFORM\\s+VARYING"
    /// </summary>
    public string Pattern { get; set; } = string.Empty;

    /// <summary>
    /// Weight added to complexity score per match.
    /// Typical range: 1-5. Higher = more complex.
    /// </summary>
    public int Weight { get; set; } = 1;
}

/// <summary>
/// Model profile settings for content-aware reasoning effort and token management.
/// One instance per API type (Codex vs Chat). No hardcoded model names —
/// all tuning comes from appsettings.json / env vars.
/// 
/// C# defaults are a conservative safety net. appsettings.json provides
/// the actual model-appropriate values.
/// </summary>
public class ModelProfileSettings
{
    // ── Reasoning effort labels ──────────────────────────────────────────
    /// <summary>Reasoning effort string for LOW complexity. e.g. "low" or "medium".</summary>
    public string LowReasoningEffort { get; set; } = "medium";

    /// <summary>Reasoning effort string for MEDIUM complexity.</summary>
    public string MediumReasoningEffort { get; set; } = "medium";

    /// <summary>Reasoning effort string for HIGH complexity.</summary>
    public string HighReasoningEffort { get; set; } = "high";

    // ── Complexity score thresholds ──────────────────────────────────────
    /// <summary>Score at or above which we jump from low → medium tier.</summary>
    public int MediumThreshold { get; set; } = 5;

    /// <summary>Score at or above which we jump from medium → high tier.</summary>
    public int HighThreshold { get; set; } = 15;

    // ── Token multipliers per tier ───────────────────────────────────────
    /// <summary>Output-token multiplier for LOW complexity (relative to estimated input).</summary>
    public double LowMultiplier { get; set; } = 1.5;

    /// <summary>Output-token multiplier for MEDIUM complexity.</summary>
    public double MediumMultiplier { get; set; } = 2.0;

    /// <summary>Output-token multiplier for HIGH complexity.</summary>
    public double HighMultiplier { get; set; } = 3.0;

    // ── Token limits ─────────────────────────────────────────────────────
    /// <summary>Minimum max_output_tokens regardless of estimation.</summary>
    public int MinOutputTokens { get; set; } = 16384;

    /// <summary>Maximum max_output_tokens cap.</summary>
    public int MaxOutputTokens { get; set; } = 65536;

    // ── Operational limits ───────────────────────────────────────────────
    /// <summary>HTTP timeout in seconds for this model profile.</summary>
    public int TimeoutSeconds { get; set; } = 600;

    /// <summary>Tokens-per-minute rate limit.</summary>
    public int TokensPerMinute { get; set; } = 300_000;

    /// <summary>Requests-per-minute rate limit.</summary>
    public int RequestsPerMinute { get; set; } = 1_000;

    // ── Structural baseline floors ───────────────────────────────────────
    /// <summary>
    /// PIC density floor: if (PIC count / meaningful lines) exceeds this,
    /// add +3 to complexity score. Catches data-heavy programs.
    /// </summary>
    public double PicDensityFloor { get; set; } = 0.25;

    /// <summary>
    /// Level-number density floor: if (level-number count / meaningful lines) exceeds this,
    /// add +2 to complexity score.
    /// </summary>
    public double LevelDensityFloor { get; set; } = 0.30;

    // ── COPY/EXEC amplifiers ─────────────────────────────────────────────
    /// <summary>Enable COPY/EXEC amplifier bonuses.</summary>
    public bool EnableAmplifiers { get; set; } = true;

    /// <summary>Bonus added when COPY appears near WORKING-STORAGE/LINKAGE data.</summary>
    public int CopyNearStorageBonus { get; set; } = 3;

    /// <summary>Bonus added for EXEC SQL or EXEC DLI presence.</summary>
    public int ExecSqlDliBonus { get; set; } = 4;

    // ── Reasoning exhaustion retry ───────────────────────────────────────
    /// <summary>Max retries when reasoning exhaustion is detected.</summary>
    public int ReasoningExhaustionMaxRetries { get; set; } = 2;

    /// <summary>Multiplier for max_output_tokens on each retry (e.g. 2.0 = double).</summary>
    public double ReasoningExhaustionRetryMultiplier { get; set; } = 2.0;

    // ── Complexity indicators (config-driven regex) ──────────────────────
    /// <summary>
    /// List of regex patterns + weights for complexity scoring.
    /// Loaded from appsettings.json. Empty list = no content-based scoring (baseline only).
    /// </summary>
    public List<ComplexityIndicator> ComplexityIndicators { get; set; } = new();
}
