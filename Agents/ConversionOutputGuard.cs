namespace CobolToQuarkusMigration.Agents;

internal static class ConversionOutputGuard
{
    internal static bool IsUsableChunk(
        string? code,
        string primaryStructure,
        string secondaryStructure,
        string language,
        out string reason)
    {
        if (string.IsNullOrWhiteSpace(code))
        {
            reason = "EMPTY_LLM_RESPONSE — model returned no usable output (likely provider timeout / 0-token response). Re-run with ENABLE_REKT_CONTEXT=true and a smaller chunk threshold.";
            return false;
        }

        var hasPrimaryStructure = code.Contains(primaryStructure, StringComparison.Ordinal);
        var hasSecondaryStructure = code.Contains(secondaryStructure, StringComparison.Ordinal);
        var opens = code.Count(c => c == '{');
        var closes = code.Count(c => c == '}');

        if ((!hasPrimaryStructure && !hasSecondaryStructure) || (opens == 0 && closes == 0))
        {
            var reasonLanguage = language.Equals("C#", StringComparison.OrdinalIgnoreCase)
                ? "CSHARP"
                : language.ToUpperInvariant();
            reason = $"NO_{reasonLanguage}_STRUCTURE — model emitted prose or non-{language} content. Re-run with full REKT context enabled.";
            return false;
        }

        reason = string.Empty;
        return true;
    }

    internal static string EscapeBlockCommentContent(string? content)
    {
        return string.IsNullOrWhiteSpace(content)
            ? "(no output)"
            : content.Replace("*/", "* /", StringComparison.Ordinal);
    }
}
