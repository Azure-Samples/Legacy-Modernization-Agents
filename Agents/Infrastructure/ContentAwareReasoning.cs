using Microsoft.Extensions.AI;
using CobolToQuarkusMigration.Models;
using System.Text.RegularExpressions;

namespace CobolToQuarkusMigration.Agents.Infrastructure;

/// <summary>
/// Shared logic for three-tier content-aware reasoning and output truncation detection.
/// Used by both <see cref="AgentBase"/> and <see cref="CobolAnalyzerAgent"/> (which does not inherit AgentBase).
/// </summary>
public static class ContentAwareReasoning
{
    // ═══════════════════════════════════════════════════════════════════════
    // STRUCTURAL REGEXES (pre-compiled, shared)
    // ═══════════════════════════════════════════════════════════════════════

    internal static readonly Regex PicRegex = new(@"\bPIC\b", RegexOptions.Compiled | RegexOptions.IgnoreCase);
    internal static readonly Regex LevelRegex = new(@"^\s*\d{2}\s+", RegexOptions.Compiled | RegexOptions.Multiline);
    internal static readonly Regex CopyNearStorageRegex = new(
        @"(WORKING-STORAGE|LINKAGE)\s+SECTION[\s\S]{0,500}COPY\b",
        RegexOptions.Compiled | RegexOptions.IgnoreCase);
    internal static readonly Regex ExecSqlDliRegex = new(
        @"EXEC\s+(SQL|DLI)\b", RegexOptions.Compiled | RegexOptions.IgnoreCase);
    internal static readonly Regex NoiseLineRegex = new(
        @"^\s*$|^.{6}\*|^.{0,5}\*>|^.{7}\s*(CBL|PROCESS|EJECT|SKIP1|SKIP2|SKIP3)\b",
        RegexOptions.Compiled | RegexOptions.IgnoreCase);

    /// <summary>
    /// Regex patterns that signal truncated output (trailing ellipsis, TODO stubs, etc.).
    /// </summary>
    internal static readonly Regex[] TruncationPatterns = new[]
    {
        new Regex(@"//\s*\.\.\.\s*(remaining|rest|continue|more|etc)", RegexOptions.IgnoreCase | RegexOptions.Compiled),
        new Regex(@"//\s*TODO:?\s*(implement|add|complete|remaining)", RegexOptions.IgnoreCase | RegexOptions.Compiled),
        new Regex(@"//\s*(omitted|truncated|abbreviated|skipped)", RegexOptions.IgnoreCase | RegexOptions.Compiled),
        new Regex(@"\.\.\.\s*$", RegexOptions.Multiline | RegexOptions.Compiled),
    };

    // ═══════════════════════════════════════════════════════════════════════
    // THREE-TIER TOKEN / REASONING CALCULATION
    // ═══════════════════════════════════════════════════════════════════════

    /// <summary>
    /// Calculates optimal max_output_tokens and reasoning effort based on content complexity.
    /// Uses three-tier system: low / medium / high based on complexity score.
    /// </summary>
    public static (int maxOutputTokens, string reasoningEffort) CalculateTokenSettings(
        string systemPrompt,
        string userPrompt,
        ModelProfileSettings profile,
        ModelCapabilities capabilities,
        List<(Regex regex, int weight)> compiledIndicators)
    {
        var inputTokens = EstimateTokens(systemPrompt) + EstimateTokens(userPrompt);

        var cobolSource = ExtractCobolSourceFromPrompt(userPrompt);
        var complexityScore = CalculateComplexityScore(cobolSource, profile, compiledIndicators);

        string reasoningEffort;
        double multiplier;

        if (complexityScore >= profile.HighThreshold)
        {
            reasoningEffort = profile.HighReasoningEffort;
            multiplier = profile.HighMultiplier;
        }
        else if (complexityScore >= profile.MediumThreshold)
        {
            reasoningEffort = profile.MediumReasoningEffort;
            multiplier = profile.MediumMultiplier;
        }
        else
        {
            reasoningEffort = profile.LowReasoningEffort;
            multiplier = profile.LowMultiplier;
        }

        var estimatedOutputNeeded = (int)(inputTokens * multiplier);

        int effectiveMinTokens =
            (complexityScore >= profile.MediumThreshold || inputTokens >= profile.MinOutputTokens / 2)
                ? profile.MinOutputTokens
                : Math.Max(4096, estimatedOutputNeeded);

        var maxOutputTokens = Math.Clamp(estimatedOutputNeeded, effectiveMinTokens, profile.MaxOutputTokens);

        if (capabilities.DefaultMaxOutputTokens > 0)
        {
            maxOutputTokens = Math.Min(maxOutputTokens, capabilities.DefaultMaxOutputTokens);
        }

        return (maxOutputTokens, reasoningEffort);
    }

    /// <summary>
    /// Applies model-specific reasoning options to ChatOptions based on detected capabilities.
    /// Claude → extended thinking budget, Codex/o-series → reasoning_effort, standard → temperature.
    /// </summary>
    public static void ApplyModelSpecificOptions(
        ChatOptions options,
        string reasoningEffort,
        int maxOutputTokens,
        ModelCapabilities capabilities)
    {
        switch (capabilities.Reasoning)
        {
            case ReasoningStrategy.ThinkingBudget:
                options.AdditionalProperties ??= new AdditionalPropertiesDictionary();
                var thinkingBudget = CalculateThinkingBudget(maxOutputTokens, reasoningEffort);
                options.AdditionalProperties["thinking"] = new Dictionary<string, object>
                {
                    ["type"] = "enabled",
                    ["budget_tokens"] = thinkingBudget
                };
                break;

            case ReasoningStrategy.EffortBased:
                options.AdditionalProperties ??= new AdditionalPropertiesDictionary();
                options.AdditionalProperties["reasoning_effort"] = reasoningEffort;
                break;

            case ReasoningStrategy.None:
            default:
                if (capabilities.SupportsTemperature)
                {
                    options.Temperature = 0.1f;
                }
                break;
        }
    }

    /// <summary>
    /// Calculates the thinking budget for Claude models based on reasoning effort tier.
    /// </summary>
    public static int CalculateThinkingBudget(int maxOutputTokens, string reasoningEffort)
    {
        var ratio = reasoningEffort.ToLowerInvariant() switch
        {
            "high" => 0.7,
            "medium" => 0.5,
            "low" => 0.3,
            _ => 0.5
        };
        return (int)(maxOutputTokens * ratio);
    }

    /// <summary>
    /// Calculates a complexity score for COBOL source using config-driven regex indicators
    /// plus structural baselines.
    /// </summary>
    public static int CalculateComplexityScore(
        string cobolSource,
        ModelProfileSettings profile,
        List<(Regex regex, int weight)> compiledIndicators)
    {
        if (string.IsNullOrWhiteSpace(cobolSource)) return 0;

        int score = 0;

        foreach (var (regex, weight) in compiledIndicators)
        {
            var matches = regex.Matches(cobolSource);
            if (matches.Count > 0)
                score += weight * matches.Count;
        }

        var lines = cobolSource.Split('\n');
        var meaningfulLines = Math.Max(CountMeaningfulLines(lines), 1);

        var picDensity = (double)PicRegex.Matches(cobolSource).Count / meaningfulLines;
        if (picDensity > profile.PicDensityFloor) score += 3;

        var levelDensity = (double)LevelRegex.Matches(cobolSource).Count / meaningfulLines;
        if (levelDensity > profile.LevelDensityFloor) score += 2;

        if (profile.EnableAmplifiers)
        {
            if (CopyNearStorageRegex.IsMatch(cobolSource)) score += profile.CopyNearStorageBonus;
            if (ExecSqlDliRegex.IsMatch(cobolSource)) score += profile.ExecSqlDliBonus;
        }

        return score;
    }

    /// <summary>
    /// Extracts COBOL source code from a user prompt that may contain instructions and markdown markers.
    /// </summary>
    public static string ExtractCobolSourceFromPrompt(string userPrompt)
    {
        if (string.IsNullOrWhiteSpace(userPrompt))
            return string.Empty;

        var cobolStart = userPrompt.IndexOf("```cobol", StringComparison.OrdinalIgnoreCase);
        if (cobolStart >= 0)
        {
            var contentStart = userPrompt.IndexOf('\n', cobolStart);
            if (contentStart < 0) contentStart = cobolStart + 8;
            else contentStart++;

            var contentEnd = userPrompt.IndexOf("```", contentStart, StringComparison.Ordinal);
            if (contentEnd < 0) contentEnd = userPrompt.Length;

            return userPrompt[contentStart..contentEnd].Trim();
        }

        var genericStart = userPrompt.IndexOf("```", StringComparison.Ordinal);
        if (genericStart >= 0)
        {
            var contentStart = userPrompt.IndexOf('\n', genericStart);
            if (contentStart < 0) contentStart = genericStart + 3;
            else contentStart++;

            var contentEnd = userPrompt.IndexOf("```", contentStart, StringComparison.Ordinal);
            if (contentEnd < 0) contentEnd = userPrompt.Length;

            return userPrompt[contentStart..contentEnd].Trim();
        }

        return userPrompt;
    }

    /// <summary>Estimates the number of tokens in a text string (~3.5 chars/token for code).</summary>
    public static int EstimateTokens(string text)
    {
        if (string.IsNullOrEmpty(text)) return 0;
        return (int)Math.Ceiling(text.Length / 3.5);
    }

    public static int CountMeaningfulLines(string[] lines)
    {
        return lines.Count(line => !NoiseLineRegex.IsMatch(line));
    }

    public static List<(Regex, int)> CompileIndicators(ModelProfileSettings profile)
    {
        var compiled = new List<(Regex, int)>();
        foreach (var indicator in profile.ComplexityIndicators
            .Where(i => !string.IsNullOrWhiteSpace(i.Pattern)))
        {
            try
            {
                var regex = new Regex(indicator.Pattern, RegexOptions.Compiled | RegexOptions.IgnoreCase);
                compiled.Add((regex, indicator.Weight));
            }
            catch (ArgumentException) { /* invalid regex — skip */ }
        }
        return compiled;
    }

    // ═══════════════════════════════════════════════════════════════════════
    // OUTPUT TRUNCATION DETECTION
    // ═══════════════════════════════════════════════════════════════════════

    /// <summary>
    /// Checks a ChatResponse for truncation via FinishReason and text-based signals.
    /// Throws <see cref="OutputTruncationException"/> if truncation is detected.
    /// </summary>
    public static void DetectTruncation(
        ChatResponse response,
        string responseText,
        int maxOutputTokens,
        string reasoningEffort,
        string contextIdentifier)
    {
        // 1. Check FinishReason — most reliable signal
        if (response.FinishReason == ChatFinishReason.Length)
        {
            throw new OutputTruncationException(
                maxOutputTokens, responseText.Length, reasoningEffort, "FinishReason=Length");
        }

        // 2. Check for text-based truncation signals
        if (responseText.Length > 500)
        {
            var tail = responseText[^500..];

            foreach (var pattern in TruncationPatterns)
            {
                var match = pattern.Match(tail);
                if (match.Success)
                {
                    throw new OutputTruncationException(
                        maxOutputTokens, responseText.Length, reasoningEffort,
                        $"Text signal: {match.Value.Trim()}");
                }
            }
        }

        // 3. Check for unclosed code blocks (model cut off mid-output)
        var openBlocks = CountOccurrences(responseText, "```");
        if (openBlocks % 2 != 0)
        {
            throw new OutputTruncationException(
                maxOutputTokens, responseText.Length, reasoningEffort, "Unclosed code block (odd ``` count)");
        }
    }

    public static int CountOccurrences(string text, string pattern)
    {
        int count = 0;
        int idx = 0;
        while ((idx = text.IndexOf(pattern, idx, StringComparison.Ordinal)) >= 0)
        {
            count++;
            idx += pattern.Length;
        }
        return count;
    }

    /// <summary>
    /// Clamps profile token limits to model's actual capabilities.
    /// Prevents HTTP 400 "max_tokens too large" errors.
    /// </summary>
    public static void ClampProfileToModel(ModelProfileSettings profile, ModelCapabilities capabilities)
    {
        if (capabilities.DefaultMaxOutputTokens > 0)
        {
            profile.MaxOutputTokens = Math.Min(profile.MaxOutputTokens, capabilities.DefaultMaxOutputTokens);
            profile.MinOutputTokens = Math.Min(profile.MinOutputTokens, capabilities.DefaultMaxOutputTokens);
        }
    }
}
