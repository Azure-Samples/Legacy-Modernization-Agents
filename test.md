# Three-Tier Content-Aware Reasoning System — Implementation Plan

## Design Principles

1. **Zero hardcoded model names in C# code** — all model names come from `appsettings.json` / env vars
2. **No `DefaultCodex()` / `DefaultChat()` factory methods** — plain `new ModelProfileSettings()` with conservative safe-fallback defaults only
3. **All tuning values from config** — C# defaults are a structural safety net; `appsettings.json` is the single source of truth
4. **Config-driven complexity scoring** — regex indicators, density floors, and amplifiers all configurable

## Files to Change (6 total)

| # | File | What Changes |
|---|------|-------------|
| 1 | `Models/Settings.cs` | Remove hardcoded `gpt-4.1` defaults → `string.Empty`; add `ComplexityIndicator` + `ModelProfileSettings` classes; add `CodexProfile` + `ChatProfile` to `AppSettings` |
| 2 | `Agents/Infrastructure/ResponsesApiClient.cs` | Add `ReasoningExhaustionException`; constructor takes `ModelProfileSettings?`; add `CalculateComplexityScore()` with config-driven regex; three-tier `CalculateTokenSettings`; throw typed exception on exhaustion |
| 3 | `Agents/Infrastructure/AgentBase.cs` | Add `catch (ReasoningExhaustionException)` in `ExecuteWithFallbackAsync`; escalation loop doubles tokens + promotes effort |
| 4 | `Config/appsettings.json` | Add `CodexProfile` and `ChatProfile` top-level sections with all tuning values |
| 5 | `Config/ai-config.env` | Add section 4 with commented-out profile env vars |
| 6 | `Program.cs` | Add profile env var overrides in `OverrideSettingsFromEnvironment()`; pass `profile:` to all 3 `ResponsesApiClient` constructors |

---

## File 1: `Models/Settings.cs`

**Changes:**
- All model ID defaults → `string.Empty` (was `"gpt-4.1"`)
- Add `ComplexityIndicator` class
- Add `ModelProfileSettings` class (no factory methods, conservative defaults)
- Add `CodexProfile` and `ChatProfile` properties to `AppSettings`

```csharp
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
    /// Falls back to ModelId when empty.
    /// </summary>
    public string JavaConverterModelId { get; set; } = string.Empty;

    /// <summary>
    /// Gets or sets the model ID for the dependency mapper.
    /// Falls back to ModelId when empty.
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
    /// PIC density floor: if (PIC count / total lines) exceeds this,
    /// add +3 to complexity score. Catches data-heavy programs.
    /// </summary>
    public double PicDensityFloor { get; set; } = 0.25;

    /// <summary>
    /// Level-number density floor: if (level-number count / total lines) exceeds this,
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
```

---

## File 2: `Agents/Infrastructure/ResponsesApiClient.cs`

**Changes:**
- Add `ReasoningExhaustionException` typed exception class (replaces `InvalidOperationException` for exhaustion)
- Constructor takes optional `ModelProfileSettings? profile` parameter
- Add `CalculateComplexityScore()` method with config-driven regex matching, PIC density floor, level-number density floor, COPY/EXEC amplifiers
- `CalculateTokenSettings` uses complexity-driven tier selection
- Pre-compiled regex cache at construction
- Reasoning exhaustion throws `ReasoningExhaustionException`
- Factory simplified to accept `ModelProfileSettings? profile`

```csharp
using System.Net.Http.Json;
using System.Net.Http.Headers;
using System.Text;
using System.Text.Json;
using System.Text.Json.Nodes;
using System.Text.RegularExpressions;
using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using Azure.Identity;
using Azure.Core;

namespace CobolToQuarkusMigration.Agents.Infrastructure;

/// <summary>
/// Thrown when the model exhausts max_output_tokens on internal reasoning
/// with minimal text output. Caught by AgentBase for retry with escalated tokens.
/// </summary>
public class ReasoningExhaustionException : Exception
{
    public int MaxOutputTokens { get; }
    public int ReasoningTokens { get; }
    public int ActualOutputTokens { get; }
    public string ReasoningEffort { get; }

    public ReasoningExhaustionException(
        int maxOutputTokens, int reasoningTokens, int actualOutputTokens, string reasoningEffort)
        : base($"Model exhausted max_output_tokens ({maxOutputTokens}) on reasoning " +
               $"({reasoningTokens} tokens) with minimal text output ({actualOutputTokens} tokens). " +
               $"Reasoning effort was '{reasoningEffort}'.")
    {
        MaxOutputTokens = maxOutputTokens;
        ReasoningTokens = reasoningTokens;
        ActualOutputTokens = actualOutputTokens;
        ReasoningEffort = reasoningEffort;
    }
}

/// <summary>
/// Client for Azure OpenAI Responses API (used by codex/reasoning models like gpt-5.1-codex-mini).
/// This is separate from the Chat Completions API used by chat models.
/// 
/// Key differences from Chat Completions:
/// - Uses max_output_tokens (NOT max_tokens or max_completion_tokens)
/// - Supports reasoning.effort parameter ("low", "medium", "high")
/// - Returns output in a different JSON structure
/// 
/// Now supports three-tier content-aware reasoning via ModelProfileSettings:
/// - Calculates complexity score from COBOL source using config-driven regex indicators
/// - Maps score to low/medium/high reasoning tier with per-tier token multipliers
/// - Throws ReasoningExhaustionException (caught by AgentBase) when model burns all tokens on reasoning
/// </summary>
public class ResponsesApiClient : IDisposable
{
    private readonly string _apiVersion;
    private readonly HttpClient _httpClient;
    private readonly string _endpoint;
    private readonly string _apiKey;
    private readonly string _deploymentName;
    private readonly ILogger? _logger;
    private readonly EnhancedLogger? _enhancedLogger;
    private readonly JsonSerializerOptions _jsonOptions;
    
    // Auth state - Semaphore for thread-safe token acquisition
    private readonly SemaphoreSlim _authLock = new(1, 1);
    private bool _hasSwitchedToEntraId;
    private string? _cachedAccessToken;
    private DateTimeOffset _accessTokenExpiresOn;

    // Rate limiting
    private readonly RateLimitTracker _rateLimitTracker;

    // Three-tier reasoning profile
    /// <summary>The model profile controlling reasoning effort and token limits.</summary>
    public ModelProfileSettings Profile { get; }

    // Pre-compiled structural regexes (always available, not config-driven)
    private static readonly Regex PicRegex = new(@"\bPIC\b", RegexOptions.Compiled | RegexOptions.IgnoreCase);
    private static readonly Regex LevelRegex = new(@"^\s*\d{2}\s+", RegexOptions.Compiled | RegexOptions.Multiline);
    private static readonly Regex DirectiveRegex = new(@"\b(COPY|EXEC)\b", RegexOptions.Compiled | RegexOptions.IgnoreCase);
    private static readonly Regex CopyNearStorageRegex = new(
        @"(WORKING-STORAGE|LINKAGE)\s+SECTION[\s\S]{0,500}COPY\b",
        RegexOptions.Compiled | RegexOptions.IgnoreCase);
    private static readonly Regex ExecSqlDliRegex = new(
        @"EXEC\s+(SQL|DLI)\b", RegexOptions.Compiled | RegexOptions.IgnoreCase);

    // Pre-compiled config-driven indicator regexes (built at construction)
    private readonly List<(Regex regex, int weight)> _compiledIndicators;

    /// <summary>
    /// Creates a new Responses API client with optional content-aware reasoning profile.
    /// </summary>
    /// <param name="endpoint">Azure OpenAI endpoint</param>
    /// <param name="apiKey">Azure OpenAI API key (empty = Entra ID)</param>
    /// <param name="deploymentName">Deployment name</param>
    /// <param name="logger">Optional logger</param>
    /// <param name="enhancedLogger">Optional enhanced logger for API call tracking</param>
    /// <param name="profile">Optional model profile for three-tier reasoning. Falls back to conservative defaults.</param>
    /// <param name="apiVersion">API version (default: 2025-04-01-preview)</param>
    public ResponsesApiClient(
        string endpoint, 
        string apiKey, 
        string deploymentName, 
        ILogger? logger = null,
        EnhancedLogger? enhancedLogger = null,
        ModelProfileSettings? profile = null,
        string apiVersion = "2025-04-01-preview")
    {
        if (string.IsNullOrEmpty(endpoint))
            throw new ArgumentNullException(nameof(endpoint));
        if (string.IsNullOrEmpty(deploymentName))
            throw new ArgumentNullException(nameof(deploymentName));

        _endpoint = endpoint.TrimEnd('/');
        _apiKey = apiKey ?? "";
        _deploymentName = deploymentName;
        _logger = logger;
        _enhancedLogger = enhancedLogger;
        _apiVersion = apiVersion;

        // Use provided profile or conservative defaults
        Profile = profile ?? new ModelProfileSettings();

        _rateLimitTracker = new RateLimitTracker(
            Profile.TokensPerMinute, Profile.RequestsPerMinute, logger);

        _httpClient = new HttpClient();
        if (!string.IsNullOrEmpty(_apiKey))
        {
            _httpClient.DefaultRequestHeaders.Add("api-key", _apiKey);
        }
        else
        {
            _hasSwitchedToEntraId = true;
            _logger?.LogInformation("No API Key provided, using Microsoft Entra ID (DefaultAzureCredential) authentication.");
        }

        _httpClient.Timeout = TimeSpan.FromSeconds(Profile.TimeoutSeconds);

        _jsonOptions = new JsonSerializerOptions
        {
            PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
            WriteIndented = false
        };

        // Pre-compile config-driven complexity indicator regexes
        _compiledIndicators = new List<(Regex, int)>();
        foreach (var indicator in Profile.ComplexityIndicators)
        {
            if (!string.IsNullOrWhiteSpace(indicator.Pattern))
            {
                try
                {
                    var regex = new Regex(indicator.Pattern, RegexOptions.Compiled | RegexOptions.IgnoreCase);
                    _compiledIndicators.Add((regex, indicator.Weight));
                }
                catch (RegexParseException ex)
                {
                    _logger?.LogWarning("Invalid complexity indicator regex '{Pattern}': {Error}",
                        indicator.Pattern, ex.Message);
                }
            }
        }

        _logger?.LogInformation(
            "Created Responses API client for {Deployment} (timeout: {Timeout}s, TPM: {TPM:N0}, RPM: {RPM:N0}, " +
            "indicators: {Count}, thresholds: {Med}/{High}, API: {ApiVersion})",
            deploymentName, Profile.TimeoutSeconds, Profile.TokensPerMinute, Profile.RequestsPerMinute,
            _compiledIndicators.Count, Profile.MediumThreshold, Profile.HighThreshold, apiVersion);
    }

    /// <summary>
    /// Estimates the number of tokens in a text string.
    /// Uses ~3.5 characters per token for code (conservative).
    /// </summary>
    public static int EstimateTokens(string text)
    {
        if (string.IsNullOrEmpty(text)) return 0;
        return (int)Math.Ceiling(text.Length / 3.5);
    }

    /// <summary>
    /// Calculates a complexity score for COBOL source using config-driven indicators
    /// plus structural baseline floors (PIC density, level-number density) and
    /// COPY/EXEC amplifiers.
    /// </summary>
    /// <param name="cobolSource">The raw COBOL source text.</param>
    /// <returns>A non-negative complexity score. Higher = more complex.</returns>
    public int CalculateComplexityScore(string cobolSource)
    {
        if (string.IsNullOrWhiteSpace(cobolSource))
            return 0;

        int score = 0;

        // 1. Config-driven regex indicators
        foreach (var (regex, weight) in _compiledIndicators)
        {
            var matches = regex.Matches(cobolSource);
            if (matches.Count > 0)
            {
                score += weight * matches.Count;
            }
        }

        // 2. Structural baseline floors
        var lines = cobolSource.Split('\n');
        var totalLines = Math.Max(lines.Length, 1);

        // PIC density floor
        var picCount = PicRegex.Matches(cobolSource).Count;
        var picDensity = (double)picCount / totalLines;
        if (picDensity > Profile.PicDensityFloor)
        {
            score += 3;
            _logger?.LogDebug("PIC density {Density:P1} exceeds floor {Floor:P1}, +3",
                picDensity, Profile.PicDensityFloor);
        }

        // Level-number density floor
        var levelCount = LevelRegex.Matches(cobolSource).Count;
        var levelDensity = (double)levelCount / totalLines;
        if (levelDensity > Profile.LevelDensityFloor)
        {
            score += 2;
            _logger?.LogDebug("Level-number density {Density:P1} exceeds floor {Floor:P1}, +2",
                levelDensity, Profile.LevelDensityFloor);
        }

        // 3. COPY/EXEC amplifiers
        if (Profile.EnableAmplifiers)
        {
            if (CopyNearStorageRegex.IsMatch(cobolSource))
            {
                score += Profile.CopyNearStorageBonus;
                _logger?.LogDebug("COPY near WORKING-STORAGE/LINKAGE detected, +{Bonus}",
                    Profile.CopyNearStorageBonus);
            }

            if (ExecSqlDliRegex.IsMatch(cobolSource))
            {
                score += Profile.ExecSqlDliBonus;
                _logger?.LogDebug("EXEC SQL/DLI detected, +{Bonus}", Profile.ExecSqlDliBonus);
            }
        }

        _logger?.LogInformation("Complexity score: {Score} (thresholds: med={Med}, high={High})",
            score, Profile.MediumThreshold, Profile.HighThreshold);

        return score;
    }

    /// <summary>
    /// Calculates optimal max_output_tokens and reasoning effort based on content complexity.
    /// Uses three-tier system: low / medium / high based on complexity score.
    /// </summary>
    public (int maxOutputTokens, string reasoningEffort) CalculateTokenSettings(
        string systemPrompt, 
        string userPrompt)
    {
        var inputTokens = EstimateTokens(systemPrompt) + EstimateTokens(userPrompt);

        // Calculate complexity score from the user prompt (which contains COBOL source)
        var complexityScore = CalculateComplexityScore(userPrompt);

        // Determine tier based on complexity score
        string reasoningEffort;
        double multiplier;

        if (complexityScore >= Profile.HighThreshold)
        {
            reasoningEffort = Profile.HighReasoningEffort;
            multiplier = Profile.HighMultiplier;
        }
        else if (complexityScore >= Profile.MediumThreshold)
        {
            reasoningEffort = Profile.MediumReasoningEffort;
            multiplier = Profile.MediumMultiplier;
        }
        else
        {
            reasoningEffort = Profile.LowReasoningEffort;
            multiplier = Profile.LowMultiplier;
        }

        // Calculate output tokens with tier-specific multiplier
        var estimatedOutputNeeded = (int)(inputTokens * multiplier);

        // Clamp to profile limits
        var maxOutputTokens = Math.Clamp(estimatedOutputNeeded, Profile.MinOutputTokens, Profile.MaxOutputTokens);

        _logger?.LogInformation(
            "Token settings: Input ~{InputTokens}, complexity={Score} → {Tier} (effort='{Effort}', " +
            "multiplier={Mult:F1}×), max_output_tokens={MaxOutput}",
            inputTokens, complexityScore,
            complexityScore >= Profile.HighThreshold ? "HIGH" :
            complexityScore >= Profile.MediumThreshold ? "MEDIUM" : "LOW",
            reasoningEffort, multiplier, maxOutputTokens);

        return (maxOutputTokens, reasoningEffort);
    }

    /// <summary>
    /// Executes a Responses API call with automatic token optimization.
    /// </summary>
    public async Task<string> GetResponseAutoAsync(
        string systemPrompt,
        string userPrompt,
        CancellationToken cancellationToken = default)
    {
        var (maxOutputTokens, reasoningEffort) = CalculateTokenSettings(systemPrompt, userPrompt);
        return await GetResponseAsync(systemPrompt, userPrompt, maxOutputTokens, reasoningEffort, cancellationToken);
    }

    /// <summary>
    /// Executes a Responses API call with system and user prompts.
    /// </summary>
    public async Task<string> GetResponseAsync(
        string systemPrompt,
        string userPrompt,
        int maxOutputTokens = 32768,
        string reasoningEffort = "medium",
        CancellationToken cancellationToken = default)
    {
        var estimatedInputTokens = EstimateTokens(systemPrompt) + EstimateTokens(userPrompt);
        var estimatedTotalTokens = estimatedInputTokens + maxOutputTokens;
        
        // Wait for rate limit capacity
        await _rateLimitTracker.WaitForCapacityAsync(estimatedTotalTokens, cancellationToken);
        
        _logger?.LogInformation(
            "Responses API: ~{Input} input + {MaxOutput} max output = ~{Total} total tokens, reasoning='{Effort}'",
            estimatedInputTokens, maxOutputTokens, estimatedTotalTokens, reasoningEffort);
        
        if (estimatedInputTokens > 50000)
        {
            _logger?.LogWarning(
                "Large input ({InputTokens} tokens). Consider chunking for better results.",
                estimatedInputTokens);
        }

        var uri = $"{_endpoint}/openai/responses?api-version={_apiVersion}";

        var requestBody = new
        {
            model = _deploymentName,
            input = new object[]
            {
                new { type = "message", role = "system", content = systemPrompt },
                new { type = "message", role = "user", content = userPrompt }
            },
            max_output_tokens = maxOutputTokens,
            reasoning = new
            {
                effort = reasoningEffort
            }
        };

        var json = JsonSerializer.Serialize(requestBody, _jsonOptions);
        
        var startTime = DateTime.UtcNow;
        
        var apiCallId = _enhancedLogger?.LogApiCallStart(
            "ResponsesAPI", 
            "POST", 
            uri, 
            _deploymentName,
            $"Input: {estimatedInputTokens} tokens, MaxOutput: {maxOutputTokens}, Reasoning: {reasoningEffort}") ?? 0;

        int attempts = 0;
        int maxAttempts = 2;

        while (attempts < maxAttempts)
        {
            attempts++;
            
            try
            {
                using var requestMessage = new HttpRequestMessage(HttpMethod.Post, uri);
                requestMessage.Content = new StringContent(json, Encoding.UTF8, "application/json");

                if (_hasSwitchedToEntraId)
                {
                    await EnsureEntraIdTokenAsync(cancellationToken);
                    requestMessage.Headers.Authorization = new AuthenticationHeaderValue("Bearer", _cachedAccessToken);
                    if (_httpClient.DefaultRequestHeaders.Contains("api-key"))
                    {
                        _httpClient.DefaultRequestHeaders.Remove("api-key");
                    }
                }
                
                using var response = await _httpClient.SendAsync(requestMessage, cancellationToken);
                var responseText = await response.Content.ReadAsStringAsync(cancellationToken);

                if (!response.IsSuccessStatusCode)
                {
                    if (response.StatusCode == System.Net.HttpStatusCode.Forbidden && !_hasSwitchedToEntraId)
                    {
                        if (responseText.Contains("AuthenticationTypeDisabled") || responseText.Contains("Key based authentication is disabled"))
                        {
                            _logger?.LogWarning("⚠️ Azure Resource has disabled API Key authentication. Switching to Entra ID...");
                            _hasSwitchedToEntraId = true;
                            _enhancedLogger?.LogBehindTheScenes("AUTH_SWITCH", "EntraID", "Switched to Entra ID auth due to 403", "System");
                            continue;
                        }
                    }

                    _enhancedLogger?.LogApiCallError(apiCallId, $"HTTP {response.StatusCode}");
                    
                    if (response.StatusCode == System.Net.HttpStatusCode.BadRequest && 
                        responseText.Contains("Tenant provided in token does not match resource token"))
                    {
                        var tenantMsg = "🛑 Tenant Mismatch Error: The authentication token is for the wrong Azure Tenant.\n" +
                                      "   To fix: az login --tenant <RESOURCE_TENANT_ID>\n" +
                                      "   See azlogin-auth-guide.md for details.";
                        _logger?.LogError(tenantMsg);
                        throw new InvalidOperationException($"Azure Authentication Failed: Tenant mismatch. {responseText}");
                    }

                    _logger?.LogError("Responses API failed ({StatusCode}): {Body}", response.StatusCode, responseText);
                    throw new HttpRequestException($"Responses API failed with status {response.StatusCode}: {responseText}");
                }

                var parsed = JsonNode.Parse(responseText);
                
                var usage = parsed?["usage"];
                var actualInputTokens = usage?["input_tokens"]?.GetValue<int>() ?? estimatedInputTokens;
                var actualOutputTokens = usage?["output_tokens"]?.GetValue<int>() ?? 0;
                var reasoningTokens = usage?["output_tokens_details"]?["reasoning_tokens"]?.GetValue<int>() ?? 0;
                var actualTotalTokens = actualInputTokens + actualOutputTokens;
                
                _rateLimitTracker.RecordUsage(actualTotalTokens);
                
                var elapsed = DateTime.UtcNow - startTime;
                
                _enhancedLogger?.LogApiCallEnd(
                    apiCallId, 
                    $"Output: {actualOutputTokens} tokens ({reasoningTokens} reasoning)", 
                    actualTotalTokens);
                
                _logger?.LogInformation(
                    "Responses API completed in {Elapsed:F1}s: {Input} input + {Output} output ({Reasoning} reasoning) = {Total} tokens",
                    elapsed.TotalSeconds, actualInputTokens, actualOutputTokens, reasoningTokens, actualTotalTokens);
                
                // Check for incomplete status — throw TYPED exception for reasoning exhaustion
                var status = parsed?["status"]?.GetValue<string>();
                if (status == "incomplete")
                {
                    var reason = parsed["incomplete_details"]?["reason"]?.GetValue<string>();
                    
                    if (reason == "max_output_tokens" && reasoningTokens >= actualOutputTokens * 0.9)
                    {
                        // TYPED exception — caught by AgentBase for retry with escalated tokens
                        throw new ReasoningExhaustionException(
                            maxOutputTokens, reasoningTokens, actualOutputTokens, reasoningEffort);
                    }
                    
                    _logger?.LogWarning(
                        "Response incomplete: {Reason}. Output={Output}, Reasoning={Reasoning}",
                        reason, actualOutputTokens, reasoningTokens);
                }
                
                return ParseResponseOutput(parsed, responseText);
            }
            catch (Exception ex) when (ex is not InvalidOperationException && 
                                        ex is not ReasoningExhaustionException &&
                                        !(ex is HttpRequestException && attempts < maxAttempts))
            {
                _logger?.LogError(ex, "Responses API error after {Elapsed:F1}s", (DateTime.UtcNow - startTime).TotalSeconds);
                throw;
            }
        }
        
        throw new InvalidOperationException("Should not reach here");
    }

    /// <summary>
    /// Parses the Responses API output structure to extract the text content.
    /// </summary>
    private string ParseResponseOutput(JsonNode? parsed, string rawResponse)
    {
        var output = parsed?["output"];
        if (output is JsonArray outputArray)
        {
            var sb = new StringBuilder();
            foreach (var item in outputArray)
            {
                var type = item?["type"]?.GetValue<string>();
                if (type == "message")
                {
                    var role = item?["role"]?.GetValue<string>();
                    if (role == "assistant")
                    {
                        var contentArray = item["content"];
                        if (contentArray is JsonArray contents)
                        {
                            foreach (var c in contents)
                            {
                                var cType = c?["type"]?.GetValue<string>();
                                if (cType == "output_text" || cType == "text")
                                {
                                    sb.Append(c?["text"]?.GetValue<string>() ?? "");
                                }
                            }
                        }
                    }
                }
            }
            
            var result = sb.ToString();
            if (!string.IsNullOrEmpty(result))
            {
                _logger?.LogDebug("Parsed {Length} chars from Responses API output", result.Length);
                return result;
            }
        }

        var outputText = parsed?["output_text"]?.GetValue<string>();
        if (!string.IsNullOrEmpty(outputText))
            return outputText;

        _logger?.LogWarning("Could not parse Responses API output, returning raw response");
        return rawResponse;
    }

    private async Task EnsureEntraIdTokenAsync(CancellationToken cancellationToken)
    {
        if (!string.IsNullOrEmpty(_cachedAccessToken) && DateTimeOffset.UtcNow < _accessTokenExpiresOn)
            return;

        try 
        {
            await _authLock.WaitAsync(cancellationToken);
            
            if (!string.IsNullOrEmpty(_cachedAccessToken) && DateTimeOffset.UtcNow < _accessTokenExpiresOn)
                return;

            _logger?.LogInformation("Acquiring new Entra ID access token for Azure Cognitive Services...");

            var credential = new DefaultAzureCredential();
            var context = new TokenRequestContext(new[] { "https://cognitiveservices.azure.com/.default" });
            var tokenResult = await credential.GetTokenAsync(context, cancellationToken);

            _cachedAccessToken = tokenResult.Token;
            _accessTokenExpiresOn = tokenResult.ExpiresOn.AddMinutes(-2);
            
            _logger?.LogInformation("Successfully acquired Entra ID access token (Expires: {Expires})", _accessTokenExpiresOn);
        }
        catch (Exception ex)
        {
            _logger?.LogError(ex, "Failed to acquire Entra ID token");
            throw;
        }
        finally
        {
            _authLock.Release();
        }
    }

    public void Dispose()
    {
        _httpClient.Dispose();
        _authLock.Dispose();
    }
}

// RateLimitTracker class unchanged from current implementation...

/// <summary>
/// Factory for creating ResponsesApiClient instances.
/// </summary>
public static class ResponsesApiClientFactory
{
    /// <summary>
    /// Creates a ResponsesApiClient for Azure OpenAI with profile-based configuration.
    /// </summary>
    public static ResponsesApiClient CreateAzureClient(
        string endpoint,
        string apiKey,
        string deploymentName,
        ILogger? logger = null,
        EnhancedLogger? enhancedLogger = null,
        ModelProfileSettings? profile = null)
    {
        return new ResponsesApiClient(
            endpoint, apiKey, deploymentName, logger, enhancedLogger,
            profile: profile);
    }
}
```

---

## File 3: `Agents/Infrastructure/AgentBase.cs`

**Changes:**
- Add `catch (ReasoningExhaustionException rex)` clause BEFORE the `IsTransientError` catch in `ExecuteWithFallbackAsync`
- Implements escalation loop: doubles `max_output_tokens` via `profile.ReasoningExhaustionRetryMultiplier`, promotes effort (low→medium→high)
- Up to `profile.ReasoningExhaustionMaxRetries` attempts
- Logs via `EnhancedLogger` with "REASONING_EXHAUSTION" and "REASONING_EXHAUSTION_RECOVERED" events

The only method that changes is `ExecuteWithFallbackAsync`. Add this catch block **before** the `IsTransientError` catch:

```csharp
    protected async Task<(string Response, bool UsedFallback, string? FallbackReason)> ExecuteWithFallbackAsync(
        string systemPrompt,
        string userPrompt,
        string contextIdentifier,
        int maxRetries = 3)
    {
        int attempt = 0;
        Exception? lastException = null;

        while (attempt < maxRetries)
        {
            attempt++;

            try
            {
                var response = await ExecuteChatCompletionAsync(systemPrompt, userPrompt, contextIdentifier);
                return (response, false, null);
            }
            // ── NEW: Reasoning exhaustion catch (before transient error catch) ──
            catch (ReasoningExhaustionException rex) when (UseResponsesApi && ResponsesClient != null)
            {
                var profile = ResponsesClient.Profile;
                var maxExhaustionRetries = profile.ReasoningExhaustionMaxRetries;

                Logger.LogWarning(
                    "[{Agent}] Reasoning exhaustion for {Context}: {Message}",
                    AgentName, contextIdentifier, rex.Message);

                EnhancedLogger?.LogBehindTheScenes("REASONING_EXHAUSTION", "DETECTED",
                    $"max_output_tokens={rex.MaxOutputTokens}, reasoning={rex.ReasoningTokens}, " +
                    $"output={rex.ActualOutputTokens}, effort='{rex.ReasoningEffort}'", AgentName);

                // Escalation loop: increase tokens and promote reasoning effort
                var currentMaxTokens = rex.MaxOutputTokens;
                var currentEffort = rex.ReasoningEffort;

                for (int exhaustionRetry = 0; exhaustionRetry < maxExhaustionRetries; exhaustionRetry++)
                {
                    // Double the output tokens
                    currentMaxTokens = (int)(currentMaxTokens * profile.ReasoningExhaustionRetryMultiplier);
                    // Cap at profile maximum
                    currentMaxTokens = Math.Min(currentMaxTokens, profile.MaxOutputTokens);

                    // Promote reasoning effort: low → medium → high
                    if (currentEffort == profile.LowReasoningEffort && currentEffort != profile.MediumReasoningEffort)
                        currentEffort = profile.MediumReasoningEffort;
                    else if (currentEffort == profile.MediumReasoningEffort && currentEffort != profile.HighReasoningEffort)
                        currentEffort = profile.HighReasoningEffort;

                    Logger.LogInformation(
                        "[{Agent}] Reasoning exhaustion retry {Retry}/{MaxRetries} for {Context}: " +
                        "max_output_tokens={Tokens}, effort='{Effort}'",
                        AgentName, exhaustionRetry + 1, maxExhaustionRetries,
                        contextIdentifier, currentMaxTokens, currentEffort);

                    try
                    {
                        var retryResponse = await ResponsesClient.GetResponseAsync(
                            systemPrompt, userPrompt, currentMaxTokens, currentEffort);

                        EnhancedLogger?.LogBehindTheScenes("REASONING_EXHAUSTION_RECOVERED", "SUCCESS",
                            $"Recovered on retry {exhaustionRetry + 1} with tokens={currentMaxTokens}, effort='{currentEffort}'",
                            AgentName);

                        ChatLogger?.LogAIResponse(AgentName, contextIdentifier, retryResponse);
                        return (retryResponse, false, null);
                    }
                    catch (ReasoningExhaustionException)
                    {
                        // Still exhausted, continue escalation loop
                        Logger.LogWarning(
                            "[{Agent}] Still exhausted after retry {Retry} with tokens={Tokens}",
                            AgentName, exhaustionRetry + 1, currentMaxTokens);
                    }
                }

                // All exhaustion retries failed — fall through to normal retry logic
                lastException = rex;
                Logger.LogError(
                    "[{Agent}] All {MaxRetries} reasoning exhaustion retries failed for {Context}",
                    AgentName, maxExhaustionRetries, contextIdentifier);

                return (string.Empty, true, $"Reasoning exhaustion: all {maxExhaustionRetries} escalation retries failed");
            }
            // ── END NEW ──
            catch (Exception ex) when (IsTransientError(ex) && attempt < maxRetries)
            {
                lastException = ex;
                var delay = TimeSpan.FromSeconds(Math.Pow(2, attempt));

                Logger.LogWarning(
                    "[{Agent}] Transient error on attempt {Attempt}/{MaxRetries} for {Context}. Retrying in {Delay}s. Error: {Error}",
                    AgentName, attempt, maxRetries, contextIdentifier, delay.TotalSeconds, ex.Message);

                EnhancedLogger?.LogBehindTheScenes("RETRY", "TRANSIENT_ERROR",
                    $"Retry {attempt}/{maxRetries} for {contextIdentifier}: {ex.Message}");

                await Task.Delay(delay);
            }
            catch (Exception ex) when (IsContentFilterError(ex))
            {
                Logger.LogWarning(
                    "[{Agent}] Content filter triggered for {Context}: {Error}",
                    AgentName, contextIdentifier, ex.Message);

                EnhancedLogger?.LogBehindTheScenes("CONTENT_FILTER", "BLOCKED",
                    $"Content filter blocked request for {contextIdentifier}: {ex.Message}");

                return (string.Empty, true, $"Content filter: {ex.Message}");
            }
            catch (Exception ex) when (IsRateLimitError(ex) && attempt < maxRetries)
            {
                lastException = ex;
                var delay = TimeSpan.FromSeconds(Math.Pow(2, attempt + 2));

                Logger.LogWarning(
                    "[{Agent}] Rate limited on attempt {Attempt}/{MaxRetries} for {Context}. Retrying in {Delay}s",
                    AgentName, attempt, maxRetries, contextIdentifier, delay.TotalSeconds);

                EnhancedLogger?.LogBehindTheScenes("RATE_LIMIT", "THROTTLED",
                    $"Rate limited, waiting {delay.TotalSeconds}s before retry");

                await Task.Delay(delay);
            }
            catch (Exception ex)
            {
                Logger.LogError(ex,
                    "[{Agent}] Non-retryable error for {Context}: {Error}",
                    AgentName, contextIdentifier, ex.Message);

                return (string.Empty, true, ex.Message);
            }
        }

        var finalReason = $"Max retries ({maxRetries}) exhausted. Last error: {lastException?.Message}";
        Logger.LogError("[{Agent}] {Reason}", AgentName, finalReason);

        return (string.Empty, true, finalReason);
    }
```

---

## File 4: `Config/appsettings.json`

**Changes:**
- Add `CodexProfile` and `ChatProfile` top-level sections after `AssemblySettings`
- CodexProfile: `LowReasoningEffort="low"`, min 32768, max 100000, timeout 900s, TPM 500000, 19 complexity indicators
- ChatProfile: `LowReasoningEffort="medium"` (chat doesn't support "low"), min 16384, max 65536, timeout 600s, TPM 300000, empty indicators

Add these two sections at the end of appsettings.json (after `AssemblySettings`, before the closing `}`):

```json
  "CodexProfile": {
    "_description": "Model profile for Codex/Responses API models (gpt-5.1-codex-mini). Controls three-tier content-aware reasoning.",
    "LowReasoningEffort": "low",
    "MediumReasoningEffort": "medium",
    "HighReasoningEffort": "high",
    "MediumThreshold": 5,
    "HighThreshold": 15,
    "LowMultiplier": 1.5,
    "MediumMultiplier": 2.5,
    "HighMultiplier": 3.5,
    "MinOutputTokens": 32768,
    "MaxOutputTokens": 100000,
    "TimeoutSeconds": 900,
    "TokensPerMinute": 500000,
    "RequestsPerMinute": 1000,
    "PicDensityFloor": 0.25,
    "LevelDensityFloor": 0.30,
    "EnableAmplifiers": true,
    "CopyNearStorageBonus": 3,
    "ExecSqlDliBonus": 4,
    "ReasoningExhaustionMaxRetries": 2,
    "ReasoningExhaustionRetryMultiplier": 2.0,
    "ComplexityIndicators": [
      { "Pattern": "EXEC\\s+SQL", "Weight": 3 },
      { "Pattern": "EXEC\\s+CICS", "Weight": 4 },
      { "Pattern": "EXEC\\s+DLI", "Weight": 4 },
      { "Pattern": "PERFORM\\s+VARYING", "Weight": 2 },
      { "Pattern": "PERFORM\\s+UNTIL", "Weight": 1 },
      { "Pattern": "EVALUATE\\s+TRUE", "Weight": 2 },
      { "Pattern": "SEARCH\\s+ALL", "Weight": 2 },
      { "Pattern": "REDEFINES", "Weight": 2 },
      { "Pattern": "OCCURS\\s+\\d+.*DEPENDING", "Weight": 3 },
      { "Pattern": "OCCURS\\s+\\d+", "Weight": 1 },
      { "Pattern": "COMPUTE\\b", "Weight": 1 },
      { "Pattern": "INSPECT\\b", "Weight": 1 },
      { "Pattern": "STRING\\b", "Weight": 1 },
      { "Pattern": "UNSTRING\\b", "Weight": 2 },
      { "Pattern": "CALL\\s+'[^']+'", "Weight": 2 },
      { "Pattern": "ALTER\\b", "Weight": 3 },
      { "Pattern": "GO\\s+TO\\s+DEPENDING", "Weight": 3 },
      { "Pattern": "COPY\\b", "Weight": 1 },
      { "Pattern": "REPLACE\\b", "Weight": 2 }
    ]
  },
  "ChatProfile": {
    "_description": "Model profile for Chat Completions API models (gpt-5.2-chat). Chat doesn't support 'low' reasoning.",
    "LowReasoningEffort": "medium",
    "MediumReasoningEffort": "medium",
    "HighReasoningEffort": "high",
    "MediumThreshold": 5,
    "HighThreshold": 15,
    "LowMultiplier": 1.5,
    "MediumMultiplier": 2.0,
    "HighMultiplier": 2.5,
    "MinOutputTokens": 16384,
    "MaxOutputTokens": 65536,
    "TimeoutSeconds": 600,
    "TokensPerMinute": 300000,
    "RequestsPerMinute": 1000,
    "PicDensityFloor": 0.25,
    "LevelDensityFloor": 0.30,
    "EnableAmplifiers": false,
    "CopyNearStorageBonus": 0,
    "ExecSqlDliBonus": 0,
    "ReasoningExhaustionMaxRetries": 1,
    "ReasoningExhaustionRetryMultiplier": 1.5,
    "ComplexityIndicators": []
  }
```

---

## File 5: `Config/ai-config.env`

**Changes:**
- Add section 4 "MODEL PROFILE SETTINGS" before Security Notes
- Commented-out env vars for overriding CodexProfile and ChatProfile values at runtime

Add this section before the Security Notes:

```bash
# =============================================================================
# 4. MODEL PROFILE SETTINGS (Three-Tier Reasoning)
# =============================================================================
# Override CodexProfile or ChatProfile values from appsettings.json at runtime.
# All values are optional — defaults come from appsettings.json.

# --- Codex Profile (Responses API) ---
# CODEX_LOW_REASONING_EFFORT="low"
# CODEX_MEDIUM_REASONING_EFFORT="medium"
# CODEX_HIGH_REASONING_EFFORT="high"
# CODEX_MEDIUM_THRESHOLD="5"
# CODEX_HIGH_THRESHOLD="15"
# CODEX_LOW_MULTIPLIER="1.5"
# CODEX_MEDIUM_MULTIPLIER="2.5"
# CODEX_HIGH_MULTIPLIER="3.5"
# CODEX_MIN_OUTPUT_TOKENS="32768"
# CODEX_MAX_OUTPUT_TOKENS="100000"
# CODEX_TIMEOUT_SECONDS="900"
# CODEX_TOKENS_PER_MINUTE="500000"
# CODEX_REQUESTS_PER_MINUTE="1000"
# CODEX_PIC_DENSITY_FLOOR="0.25"
# CODEX_LEVEL_DENSITY_FLOOR="0.30"
# CODEX_ENABLE_AMPLIFIERS="true"
# CODEX_EXHAUSTION_MAX_RETRIES="2"
# CODEX_EXHAUSTION_RETRY_MULTIPLIER="2.0"

# --- Chat Profile (Chat Completions API) ---
# CHAT_TIMEOUT_SECONDS="600"
# CHAT_TOKENS_PER_MINUTE="300000"
# CHAT_MIN_OUTPUT_TOKENS="16384"
# CHAT_MAX_OUTPUT_TOKENS="65536"
```

---

## File 6: `Program.cs`

### 6a: `OverrideSettingsFromEnvironment()` — Add profile env var overrides

Add these lines at the end of `OverrideSettingsFromEnvironment()`, before the closing `}`:

```csharp
        // ── Model Profile Overrides (Three-Tier Reasoning) ──────────────────

        // Codex Profile overrides
        var codexProfile = settings.CodexProfile ??= new ModelProfileSettings();

        if (Environment.GetEnvironmentVariable("CODEX_LOW_REASONING_EFFORT") is { Length: > 0 } codexLowEffort)
            codexProfile.LowReasoningEffort = codexLowEffort;
        if (Environment.GetEnvironmentVariable("CODEX_MEDIUM_REASONING_EFFORT") is { Length: > 0 } codexMedEffort)
            codexProfile.MediumReasoningEffort = codexMedEffort;
        if (Environment.GetEnvironmentVariable("CODEX_HIGH_REASONING_EFFORT") is { Length: > 0 } codexHighEffort)
            codexProfile.HighReasoningEffort = codexHighEffort;

        if (Environment.GetEnvironmentVariable("CODEX_MEDIUM_THRESHOLD") is { Length: > 0 } codexMedThresh
            && int.TryParse(codexMedThresh, out var cmtVal))
            codexProfile.MediumThreshold = cmtVal;
        if (Environment.GetEnvironmentVariable("CODEX_HIGH_THRESHOLD") is { Length: > 0 } codexHighThresh
            && int.TryParse(codexHighThresh, out var chtVal))
            codexProfile.HighThreshold = chtVal;

        if (Environment.GetEnvironmentVariable("CODEX_LOW_MULTIPLIER") is { Length: > 0 } codexLowMult
            && double.TryParse(codexLowMult, out var clmVal))
            codexProfile.LowMultiplier = clmVal;
        if (Environment.GetEnvironmentVariable("CODEX_MEDIUM_MULTIPLIER") is { Length: > 0 } codexMedMult
            && double.TryParse(codexMedMult, out var cmmVal))
            codexProfile.MediumMultiplier = cmmVal;
        if (Environment.GetEnvironmentVariable("CODEX_HIGH_MULTIPLIER") is { Length: > 0 } codexHighMult
            && double.TryParse(codexHighMult, out var chmVal))
            codexProfile.HighMultiplier = chmVal;

        if (Environment.GetEnvironmentVariable("CODEX_MIN_OUTPUT_TOKENS") is { Length: > 0 } codexMinTokens
            && int.TryParse(codexMinTokens, out var cminVal))
            codexProfile.MinOutputTokens = cminVal;
        if (Environment.GetEnvironmentVariable("CODEX_MAX_OUTPUT_TOKENS") is { Length: > 0 } codexMaxTokens
            && int.TryParse(codexMaxTokens, out var cmaxVal))
            codexProfile.MaxOutputTokens = cmaxVal;

        if (Environment.GetEnvironmentVariable("CODEX_TIMEOUT_SECONDS") is { Length: > 0 } codexTimeout
            && int.TryParse(codexTimeout, out var ctVal))
            codexProfile.TimeoutSeconds = ctVal;
        if (Environment.GetEnvironmentVariable("CODEX_TOKENS_PER_MINUTE") is { Length: > 0 } codexTpm
            && int.TryParse(codexTpm, out var ctpmVal))
            codexProfile.TokensPerMinute = ctpmVal;
        if (Environment.GetEnvironmentVariable("CODEX_REQUESTS_PER_MINUTE") is { Length: > 0 } codexRpm
            && int.TryParse(codexRpm, out var crpmVal))
            codexProfile.RequestsPerMinute = crpmVal;

        if (Environment.GetEnvironmentVariable("CODEX_PIC_DENSITY_FLOOR") is { Length: > 0 } codexPicFloor
            && double.TryParse(codexPicFloor, out var cpfVal))
            codexProfile.PicDensityFloor = cpfVal;
        if (Environment.GetEnvironmentVariable("CODEX_LEVEL_DENSITY_FLOOR") is { Length: > 0 } codexLevelFloor
            && double.TryParse(codexLevelFloor, out var clfVal))
            codexProfile.LevelDensityFloor = clfVal;

        if (Environment.GetEnvironmentVariable("CODEX_ENABLE_AMPLIFIERS") is { Length: > 0 } codexAmps
            && bool.TryParse(codexAmps, out var caVal))
            codexProfile.EnableAmplifiers = caVal;

        if (Environment.GetEnvironmentVariable("CODEX_EXHAUSTION_MAX_RETRIES") is { Length: > 0 } codexExRetries
            && int.TryParse(codexExRetries, out var cerVal))
            codexProfile.ReasoningExhaustionMaxRetries = cerVal;
        if (Environment.GetEnvironmentVariable("CODEX_EXHAUSTION_RETRY_MULTIPLIER") is { Length: > 0 } codexExMult
            && double.TryParse(codexExMult, out var cemVal))
            codexProfile.ReasoningExhaustionRetryMultiplier = cemVal;

        // Chat Profile overrides (subset — chat profiles are simpler)
        var chatProfile = settings.ChatProfile ??= new ModelProfileSettings();

        if (Environment.GetEnvironmentVariable("CHAT_TIMEOUT_SECONDS") is { Length: > 0 } chatTimeout
            && int.TryParse(chatTimeout, out var chatTVal))
            chatProfile.TimeoutSeconds = chatTVal;
        if (Environment.GetEnvironmentVariable("CHAT_TOKENS_PER_MINUTE") is { Length: > 0 } chatTpm
            && int.TryParse(chatTpm, out var chatTpmVal))
            chatProfile.TokensPerMinute = chatTpmVal;
        if (Environment.GetEnvironmentVariable("CHAT_MIN_OUTPUT_TOKENS") is { Length: > 0 } chatMinTokens
            && int.TryParse(chatMinTokens, out var chatMinVal))
            chatProfile.MinOutputTokens = chatMinVal;
        if (Environment.GetEnvironmentVariable("CHAT_MAX_OUTPUT_TOKENS") is { Length: > 0 } chatMaxTokens
            && int.TryParse(chatMaxTokens, out var chatMaxVal))
            chatProfile.MaxOutputTokens = chatMaxVal;
```

### 6b: `ResponsesApiClient` Constructor Calls — Add `profile:` parameter

There are 3 constructor call sites to update:

**Site 1: MCP Server (~line 352)**

```csharp
// BEFORE:
responsesApiClient = new ResponsesApiClient(
    settings.AISettings.Endpoint,
    string.Empty,
    settings.AISettings.DeploymentName,
    loggerFactory.CreateLogger<ResponsesApiClient>(),
    enhancedLogger,
    apiVersion: apiVersion);

// AFTER:
responsesApiClient = new ResponsesApiClient(
    settings.AISettings.Endpoint,
    string.Empty,
    settings.AISettings.DeploymentName,
    loggerFactory.CreateLogger<ResponsesApiClient>(),
    enhancedLogger,
    profile: settings.CodexProfile,
    apiVersion: apiVersion);
```

**Site 2: RunMigrationAsync (~line 503)**

```csharp
// BEFORE:
var responsesApiClient = new ResponsesApiClient(
    settings.AISettings.Endpoint,
    string.Empty,
    settings.AISettings.DeploymentName,
    loggerFactory.CreateLogger<ResponsesApiClient>(),
    enhancedLogger,
    apiVersion: apiVersion);

// AFTER:
var responsesApiClient = new ResponsesApiClient(
    settings.AISettings.Endpoint,
    string.Empty,
    settings.AISettings.DeploymentName,
    loggerFactory.CreateLogger<ResponsesApiClient>(),
    enhancedLogger,
    profile: settings.CodexProfile,
    apiVersion: apiVersion);
```

**Site 3: RunReverseEngineeringAsync (~line 1190)**

```csharp
// BEFORE:
var responsesApiClient = new ResponsesApiClient(
    settings.AISettings.Endpoint,
    string.Empty,
    settings.AISettings.DeploymentName,
    loggerFactory.CreateLogger<ResponsesApiClient>(),
    enhancedLogger,
    apiVersion: apiVersion);

// AFTER:
var responsesApiClient = new ResponsesApiClient(
    settings.AISettings.Endpoint,
    string.Empty,
    settings.AISettings.DeploymentName,
    loggerFactory.CreateLogger<ResponsesApiClient>(),
    enhancedLogger,
    profile: settings.CodexProfile,
    apiVersion: apiVersion);
```

---

## Summary

| File | Key Change | Status |
|------|-----------|--------|
| `Models/Settings.cs` | `string.Empty` defaults + `ComplexityIndicator` + `ModelProfileSettings` classes + `CodexProfile`/`ChatProfile` on `AppSettings` | **Not yet applied** |
| `ResponsesApiClient.cs` | `ReasoningExhaustionException` + profile-aware constructor + `CalculateComplexityScore()` + three-tier `CalculateTokenSettings` | **Not yet applied** |
| `AgentBase.cs` | `catch (ReasoningExhaustionException)` with escalation loop | **Not yet applied** |
| `appsettings.json` | `CodexProfile` + `ChatProfile` JSON sections with 19 indicators | **Not yet applied** |
| `ai-config.env` | Section 4 with commented-out profile env vars | **Not yet applied** |
| `Program.cs` | Profile env var overrides + `profile: settings.CodexProfile` on 3 constructors | **Not yet applied** |

### Design Principles Recap

- **Zero hardcoded model names in C#** — all `string.Empty` defaults
- **No `DefaultCodex()`/`DefaultChat()` factory methods** — plain `new ModelProfileSettings()` with conservative behavioral-only defaults
- **`appsettings.json` is the single source of truth** — C# defaults are just a structural safety net
- **Env vars override everything** — for runtime tuning without redeploying
- **Config-driven regex complexity indicators** — add/remove/tune patterns without code changes
- **Typed exception (`ReasoningExhaustionException`)** — enables structured retry with escalation in `AgentBase`
