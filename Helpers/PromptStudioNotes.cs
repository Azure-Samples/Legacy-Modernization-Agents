using System.Text;
using System.Text.RegularExpressions;

namespace CobolToQuarkusMigration.Helpers;

// Prompt Studio's Run writes codebase notes into a marked block at the end of the System section.
// The hand-written instructions and their placeholders stay untouched, so every agent can be refreshed.
public static class PromptStudioNotes
{
    public const string BeginMarker = "<!-- prompt-studio:begin -->";
    public const string EndMarker = "<!-- prompt-studio:end -->";

    private const string SectionPrefix = "## SECTION: ";
    private static readonly Regex Placeholder = new(@"\{\{(\w+)\}\}", RegexOptions.Compiled);

    public static bool IsMarker(string line)
    {
        var trimmed = line.Trim();
        return trimmed == BeginMarker || trimmed == EndMarker;
    }

    // Replaces the notes block in the System section, or appends one. Every other byte of the file is kept.
    public static string Apply(string fileContent, string notes)
    {
        var newline = fileContent.Contains("\r\n") ? "\r\n" : "\n";
        var lines = fileContent.Split('\n').Select(l => l.TrimEnd('\r')).ToList();

        var systemStart = lines.FindIndex(l => l.StartsWith(SectionPrefix + "System", StringComparison.OrdinalIgnoreCase)
            && l[SectionPrefix.Length..].Trim().Equals("System", StringComparison.OrdinalIgnoreCase));
        if (systemStart < 0)
            throw new InvalidOperationException("Prompt has no '## SECTION: System' section.");

        var systemEnd = lines.FindIndex(systemStart + 1, l => l.StartsWith(SectionPrefix, StringComparison.Ordinal));
        if (systemEnd < 0) systemEnd = lines.Count;

        var body = lines.GetRange(systemStart + 1, systemEnd - systemStart - 1);
        var begin = body.FindIndex(IsMarker);
        if (begin >= 0)
        {
            var end = body.FindIndex(begin + 1, IsMarker);
            body.RemoveRange(begin, (end < 0 ? body.Count - 1 : end) - begin + 1);
        }
        while (body.Count > 0 && string.IsNullOrWhiteSpace(body[^1])) body.RemoveAt(body.Count - 1);

        var block = new List<string> { "", BeginMarker };
        block.AddRange(Sanitize(notes).Split('\n').Select(l => l.TrimEnd('\r')));
        block.Add(EndMarker);
        if (systemEnd < lines.Count || fileContent.EndsWith('\n')) block.Add("");

        var rebuilt = new List<string>(lines.Take(systemStart + 1));
        rebuilt.AddRange(body);
        rebuilt.AddRange(block);
        rebuilt.AddRange(lines.Skip(systemEnd));
        return string.Join(newline, rebuilt);
    }

    // Notes come from regex output or a model; they must not resolve as placeholders, open sections or nest markers.
    public static string Sanitize(string notes)
    {
        var sb = new StringBuilder();
        foreach (var raw in notes.Trim().Split('\n'))
        {
            var line = raw.TrimEnd('\r');
            if (IsMarker(line)) continue;
            if (line.StartsWith(SectionPrefix, StringComparison.Ordinal)) line = "#" + line;
            sb.Append(Placeholder.Replace(line, "$1")).Append('\n');
        }
        return sb.ToString().TrimEnd();
    }
}
