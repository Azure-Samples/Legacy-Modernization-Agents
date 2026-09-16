using System.Text.RegularExpressions;

namespace CobolToQuarkusMigration.Helpers;

/// <summary>
/// Guards the placeholders a prompt template depends on.
///
/// Prompt templates are data to the editor but contract to the code that renders them: the chunked
/// converter cannot say which lines it is converting without <c>{{StartLine}}</c>, and the analyzer
/// loses the codebase summary without <c>{{CodebaseProfile}}</c>. A rewrite that drops one produces
/// a prompt that still reads sensibly and still renders, so nothing fails; the model is simply given
/// less, and the output is quietly worse.
///
/// That is not hypothetical. An AI-assisted rewrite of every prompt removed <c>{{CodebaseProfile}}</c>,
/// <c>{{StartLine}}</c>, <c>{{EndLine}}</c>, <c>{{SemanticUnitsContext}}</c> and a whole CICS section,
/// and the loss was noticed only because the files happened to be under version control.
/// </summary>
public static class PromptPlaceholders
{
    private static readonly Regex Token = new(@"\{\{(\w+)\}\}", RegexOptions.Compiled);

    /// <summary>Placeholder names appearing in the template, without the braces.</summary>
    public static IReadOnlyCollection<string> In(string? content)
    {
        if (string.IsNullOrEmpty(content))
            return Array.Empty<string>();

        return Token.Matches(content)
            .Select(m => m.Groups[1].Value)
            .ToHashSet(StringComparer.Ordinal);
    }

    /// <summary>
    /// Placeholders present before a rewrite and missing after it. Empty when nothing was lost,
    /// including when the file is new.
    /// </summary>
    public static IReadOnlyCollection<string> Dropped(string? before, string? after)
    {
        var had = In(before);
        if (had.Count == 0)
            return Array.Empty<string>();

        var has = In(after);
        return had.Where(p => !has.Contains(p))
            .OrderBy(p => p, StringComparer.Ordinal)
            .ToList();
    }

    /// <summary>
    /// Describes the loss in terms an operator can act on. The caller decides whether to refuse the
    /// write or record the refusal and carry on with the rest of a batch.
    /// </summary>
    public static string DescribeLoss(string promptName, IReadOnlyCollection<string> dropped) =>
        $"'{promptName}' would lose {(dropped.Count == 1 ? "placeholder" : "placeholders")} "
        + string.Join(", ", dropped.Select(p => "{{" + p + "}}"))
        + ". The renderer substitutes these at run time, so a template without them still renders "
        + "and still reads correctly while silently giving the model less to work with. Reapply the "
        + "edit with the placeholders left in place.";
}
