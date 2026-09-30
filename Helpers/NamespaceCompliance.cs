// Checks that generated code landed in the namespace it was assigned.
//
// The namespace is assigned in a block this repository injects, but the line telling the model to
// use it lives in the User section of the converter prompt — the one section Prompt Studio
// rewrites. An AI rewrite can drop that line, and nothing fails: the block still arrives, nothing
// insists on it, and the model goes back to inventing com.example or CobolMigration. The output
// still compiles, still looks ordinary, and is no longer a service.
//
// Prompt wording cannot be relied on to survive editing. The output can be checked instead, and a
// check is indifferent to how the prompt was worded.

namespace CobolToQuarkusMigration.Helpers;

using System.Text.RegularExpressions;

public sealed record NamespaceDeviation(string File, string? Declared);

public static class NamespaceCompliance
{
    private static readonly Regex Declaration = new(
        @"^\s*(?:namespace|package)\s+([\w.]+)",
        RegexOptions.Compiled | RegexOptions.Multiline);

    private static readonly string[] Extensions = [".cs", ".java"];

    /// <summary>
    /// Generated files whose namespace is not under <paramref name="expectedRoot"/>, including any
    /// that declare none at all.
    /// </summary>
    public static IReadOnlyList<NamespaceDeviation> Check(string generatedDirectory, string expectedRoot)
    {
        if (!Directory.Exists(generatedDirectory) || string.IsNullOrWhiteSpace(expectedRoot))
            return Array.Empty<NamespaceDeviation>();

        var deviations = new List<NamespaceDeviation>();

        foreach (var file in Directory
                     .EnumerateFiles(generatedDirectory, "*", SearchOption.AllDirectories)
                     .Where(f => Extensions.Contains(Path.GetExtension(f), StringComparer.OrdinalIgnoreCase))
                     .OrderBy(f => f, StringComparer.Ordinal))
        {
            string text;
            try { text = File.ReadAllText(file); }
            catch (IOException) { continue; }

            var match = Declaration.Match(text);
            var declared = match.Success ? match.Groups[1].Value : null;

            if (!IsUnder(declared, expectedRoot))
                deviations.Add(new NamespaceDeviation(file, declared));
        }

        return deviations;
    }

    /// <summary>
    /// Whether a declared namespace sits under the expected root. The comparison stops on a
    /// segment boundary so that "Modernizedx" does not pass for "Modernized".
    /// </summary>
    public static bool IsUnder(string? declared, string expectedRoot)
    {
        if (string.IsNullOrWhiteSpace(declared)) return false;

        return declared.Equals(expectedRoot, StringComparison.OrdinalIgnoreCase)
            || declared.StartsWith(expectedRoot + ".", StringComparison.OrdinalIgnoreCase);
    }

    /// <summary>A report naming the files that went elsewhere, or an empty string when none did.</summary>
    public static string Describe(
        IReadOnlyList<NamespaceDeviation> deviations, string expectedRoot, string rootDirectory)
    {
        if (deviations.Count == 0) return string.Empty;

        var lines = new List<string>
        {
            $"{deviations.Count} generated file(s) are not under the assigned root '{expectedRoot}'.",
            "The namespace is assigned to the converter rather than requested from it, so this",
            "means the instruction did not reach the model — most often because the prompt's User",
            "section was rewritten. The output will still compile; it is simply no longer one service.",
            "",
        };

        foreach (var deviation in deviations.Take(20))
        {
            var where = Path.GetRelativePath(rootDirectory, deviation.File);
            lines.Add($"  {where}  →  {deviation.Declared ?? "(no namespace declared)"}");
        }

        if (deviations.Count > 20)
            lines.Add($"  … and {deviations.Count - 20} more.");

        return string.Join(Environment.NewLine, lines);
    }
}
