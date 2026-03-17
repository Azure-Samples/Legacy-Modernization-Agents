namespace CobolToQuarkusMigration.Models;

/// <summary>
/// Model family classification. Determines which reasoning strategy to use.
/// </summary>
public enum ModelFamily
{
    /// <summary>OpenAI GPT models (gpt-4o, gpt-5, gpt-5.2-chat). No special reasoning mode.</summary>
    OpenAI,

    /// <summary>OpenAI reasoning models (o1, o3, codex). Support reasoning.effort parameter.</summary>
    Codex,

    /// <summary>Anthropic Claude models. Support extended thinking with budget_tokens.</summary>
    Claude,

    /// <summary>xAI Grok models. No special reasoning mode.</summary>
    Grok,

    /// <summary>Unknown model — conservative defaults.</summary>
    Unknown
}

/// <summary>
/// How the model handles reasoning/thinking.
/// </summary>
public enum ReasoningStrategy
{
    /// <summary>No special reasoning — just system/user messages with max_tokens.</summary>
    None,

    /// <summary>OpenAI Responses API reasoning.effort parameter ("low"/"medium"/"high").</summary>
    EffortBased,

    /// <summary>Anthropic extended thinking with budget_tokens.</summary>
    ThinkingBudget
}

/// <summary>
/// Describes what a specific model supports so the client layer can adapt its API
/// calls accordingly. Detected from model ID string — no hardcoded model names
/// in callers.
/// </summary>
public class ModelCapabilities
{
    public ModelFamily Family { get; init; }
    public ReasoningStrategy Reasoning { get; init; }

    /// <summary>Whether the model supports the Responses API (vs Chat Completions only).</summary>
    public bool SupportsResponsesApi { get; init; }

    /// <summary>Whether the model supports temperature parameter.</summary>
    public bool SupportsTemperature { get; init; }

    /// <summary>Default max output tokens for this model family.</summary>
    public int DefaultMaxOutputTokens { get; init; }

    /// <summary>Maximum context window size (tokens).</summary>
    public int ContextWindowSize { get; init; }

    /// <summary>
    /// Detects model capabilities from a model ID string.
    /// Handles Azure deployment names, GitHub Models names, and direct model names.
    /// </summary>
    public static ModelCapabilities Detect(string modelId)
    {
        if (string.IsNullOrWhiteSpace(modelId))
            return Default();

        var id = modelId.ToLowerInvariant();

        // ── Codex / O-series reasoning models ────────────────────────────
        if (id.Contains("codex") || id.Contains("o1-") || id.Contains("o3-") ||
            id.Contains("o1") && !id.Contains("proto") ||
            id.Contains("o3") && !id.Contains("proto"))
        {
            return new ModelCapabilities
            {
                Family = ModelFamily.Codex,
                Reasoning = ReasoningStrategy.EffortBased,
                SupportsResponsesApi = true,
                SupportsTemperature = false,
                DefaultMaxOutputTokens = 100_000,
                ContextWindowSize = 200_000
            };
        }

        // ── Claude models ────────────────────────────────────────────────
        if (id.Contains("claude"))
        {
            var isOpus = id.Contains("opus");
            return new ModelCapabilities
            {
                Family = ModelFamily.Claude,
                Reasoning = ReasoningStrategy.ThinkingBudget,
                SupportsResponsesApi = false,
                SupportsTemperature = true,
                DefaultMaxOutputTokens = isOpus ? 32_000 : 16_384,
                ContextWindowSize = 200_000
            };
        }

        // ── Grok models ─────────────────────────────────────────────────
        if (id.Contains("grok"))
        {
            return new ModelCapabilities
            {
                Family = ModelFamily.Grok,
                Reasoning = ReasoningStrategy.None,
                SupportsResponsesApi = false,
                SupportsTemperature = true,
                DefaultMaxOutputTokens = 32_768,
                ContextWindowSize = 131_072
            };
        }

        // ── OpenAI GPT models ───────────────────────────────────────────
        if (id.Contains("gpt-5") || id.Contains("gpt-4") || id.Contains("gpt-4o"))
        {
            var isGpt5Plus = id.Contains("gpt-5");
            return new ModelCapabilities
            {
                Family = ModelFamily.OpenAI,
                Reasoning = ReasoningStrategy.None,
                SupportsResponsesApi = false,
                SupportsTemperature = !isGpt5Plus,
                DefaultMaxOutputTokens = 16_384,
                ContextWindowSize = isGpt5Plus ? 200_000 : 128_000
            };
        }

        return Default();
    }

    private static ModelCapabilities Default() => new()
    {
        Family = ModelFamily.Unknown,
        Reasoning = ReasoningStrategy.None,
        SupportsResponsesApi = false,
        SupportsTemperature = true,
        DefaultMaxOutputTokens = 16_384,
        ContextWindowSize = 128_000
    };
}
