using System.Text;
using System.Text.RegularExpressions;

namespace McpChatWeb.Services;

/// <summary>
/// Rewrites Config/ai-config.local.env without losing settings the writer does not own.
/// Portal setup and doctor.sh setup both own only the AI provider keys; Neo4j passwords,
/// container names, ports and folders must survive, because the passwords are the only
/// values that open the existing graph volumes. doctor.sh applies the same rule.
/// </summary>
public static partial class LocalConfigFile
{
    [GeneratedRegex(@"^(_[A-Z0-9_]*|AZURE_OPENAI_[A-Z0-9_]*|AISETTINGS__[A-Z0-9_]*|COPILOT_[A-Z0-9_]*|GITHUB_HOST)$")]
    private static partial Regex OwnedKey();

    [GeneratedRegex(@"^([A-Za-z_][A-Za-z0-9_]*)=")]
    private static partial Regex Assignment();

    public static bool IsOwnedBySetup(string key) => OwnedKey().IsMatch(key);

    /// <summary>
    /// Returns <paramref name="generated"/> with every non-owned assignment from
    /// <paramref name="previous"/> restored: replacing the generated line when the key is
    /// present, otherwise appended at the end.
    /// </summary>
    public static string MergeUnownedKeys(string generated, string? previous)
    {
        if (string.IsNullOrEmpty(previous))
        {
            return generated;
        }

        var keep = new Dictionary<string, string>(StringComparer.Ordinal);
        var order = new List<string>();
        foreach (var line in SplitLines(previous))
        {
            if (KeyOf(line) is { } key && !IsOwnedBySetup(key))
            {
                if (!keep.ContainsKey(key)) order.Add(key);
                keep[key] = line;
            }
        }

        var written = new HashSet<string>(StringComparer.Ordinal);
        var sb = new StringBuilder();
        foreach (var line in SplitLines(generated))
        {
            if (KeyOf(line) is { } key && keep.TryGetValue(key, out var old))
            {
                if (written.Add(key)) sb.AppendLine(old);
                continue;
            }
            sb.AppendLine(line);
        }

        var remaining = order.Where(k => !written.Contains(k)).ToList();
        if (remaining.Count > 0)
        {
            sb.AppendLine();
            sb.AppendLine("# Kept from the previous configuration");
            foreach (var key in remaining) sb.AppendLine(keep[key]);
        }
        return sb.ToString();
    }

    private static string? KeyOf(string line) =>
        Assignment().Match(line) is { Success: true } m ? m.Groups[1].Value : null;

    private static IEnumerable<string> SplitLines(string text) =>
        text.Replace("\r\n", "\n").TrimEnd('\n').Split('\n');
}
