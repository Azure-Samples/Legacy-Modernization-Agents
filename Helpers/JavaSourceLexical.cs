using System.Text.RegularExpressions;

namespace CobolToQuarkusMigration.Helpers;

public static class JavaSourceLexical
{
    private static readonly Regex Package = new(
        @"\bpackage\s+([A-Za-z_$][\w$]*(?:\s*\.\s*[A-Za-z_$][\w$]*)*)\s*;",
        RegexOptions.CultureInvariant);
    private static readonly Regex Type = new(@"\b(?:class|interface|enum|record)\s+([A-Za-z_$][\w$]*)");
    private static readonly Regex PublicType = new(
        @"\bpublic\s+(?:(?:abstract|final|sealed|non-sealed|strictfp)\s+)*(?:class|interface|enum|record)\s+([A-Za-z_$][\w$]*)");

    // Preserve offsets and newlines so matches map back to the original response without rewriting literals.
    public static string MaskNonCode(string source)
    {
        var masked = source.ToCharArray();
        Scan(source, masked, 0);
        return new string(masked);
    }

    // States: 0 code, 1 line comment, 2 block comment, 3 string, 4 char, 5 text block.
    // String and char literals cannot span lines in Java, so they end at a newline; this also
    // keeps prose apostrophes around fenced responses from swallowing later lines.
    private static int Scan(string source, char[]? masked, int state)
    {
        void Blank(int index)
        {
            if (masked is not null) masked[index] = ' ';
        }

        for (var i = 0; i < source.Length; i++)
        {
            var ch = source[i];
            var next = i + 1 < source.Length ? source[i + 1] : '\0';
            if (state == 0)
            {
                if (ch == '/' && (next == '/' || next == '*'))
                {
                    state = next == '/' ? 1 : 2;
                    Blank(i); Blank(i + 1);
                    i++;
                }
                else if (ch == '"' && source.AsSpan(i).StartsWith("\"\"\""))
                {
                    state = 5;
                    Blank(i); Blank(i + 1); Blank(i + 2);
                    i += 2;
                }
                else if (ch == '"' || ch == '\'')
                {
                    state = ch == '"' ? 3 : 4;
                    Blank(i);
                }
                continue;
            }
            if (ch != '\r' && ch != '\n') Blank(i);
            if ((state == 1 || state == 3 || state == 4) && (ch == '\n' || ch == '\r')) state = 0;
            else if (state == 2 && ch == '*' && next == '/')
            {
                Blank(++i);
                state = 0;
            }
            else if (state >= 3 && ch == '\\' && i + 1 < source.Length)
            {
                if (next != '\r' && next != '\n') Blank(i + 1);
                i++;
            }
            else if (state == 3 && ch == '"' || state == 4 && ch == '\'') state = 0;
            else if (state == 5 && ch == '"' && source.AsSpan(i).StartsWith("\"\"\""))
            {
                Blank(i + 1); Blank(i + 2);
                i += 2;
                state = 0;
            }
        }
        return state;
    }

    public static string? PackageName(string source)
    {
        var match = Package.Match(MaskNonCode(source));
        return match.Success ? Regex.Replace(match.Groups[1].Value, @"\s+", "") : null;
    }

    public static string? ClassName(string source)
    {
        var code = MaskNonCode(source);
        var match = TopLevelPublicTypes(code).FirstOrDefault() ?? Type.Match(code);
        return match.Success ? match.Groups[1].Value : null;
    }

    private static List<Match> TopLevelPublicTypes(string code)
    {
        var declarations = PublicType.Matches(code).Cast<Match>().ToDictionary(match => match.Index);
        var depth = 0;
        var result = new List<Match>();
        for (var i = 0; i < code.Length; i++)
        {
            if (depth == 0 && declarations.TryGetValue(i, out var match)) result.Add(match);
            if (code[i] == '{') depth++;
            else if (code[i] == '}') depth--;
        }
        return result;
    }

    public static string RenameIdentifier(string source, string oldName, string newName)
    {
        var masked = MaskNonCode(source);
        var matches = Regex.Matches(masked, @"(?<![\w$])" + Regex.Escape(oldName) + @"(?![\w$])");
        var result = new System.Text.StringBuilder(source);
        foreach (Match match in matches.Cast<Match>().Reverse())
            result.Remove(match.Index, match.Length).Insert(match.Index, newName);
        return result.ToString();
    }

    public static string ExtractResponse(string response)
    {
        var input = ExtractFences(response);
        var masked = MaskNonCode(input);
        var packages = Package.Matches(masked).Cast<Match>().ToArray();
        if (packages.Length < 2)
        {
            if (TopLevelPublicTypes(masked).Count > 1)
                throw new InvalidOperationException("Multiple public Java types require separate output files.");
            return input;
        }

        var units = packages.Select((match, i) => input[
            (i == 0 ? 0 : match.Index)..(i + 1 < packages.Length ? packages[i + 1].Index : input.Length)]).ToArray();
        var complete = units.Where(IsComplete).ToArray();
        // A single-file contract cannot silently discard another genuine compilation unit.
        if (complete.Length != 1 || !IsComplete(units[^1]))
            throw new InvalidOperationException("Multiple Java compilation units or ambiguous output restarts; preserve the response and split explicitly.");
        var selected = units[^1];
        var selectedName = ClassName(selected);
        var selectedPackage = PackageName(selected);
        if (selectedName is null || units[..^1].Any(unit =>
                ClassName(unit) != selectedName || PackageName(unit) != selectedPackage))
            throw new InvalidOperationException("Java restart belongs to a different compilation unit; no source was discarded.");
        return selected.Trim();
    }

    private static bool IsComplete(string source)
    {
        var code = MaskNonCode(source);
        var depth = 0;
        var opened = false;
        foreach (var ch in code)
        {
            if (ch == '{') { depth++; opened = true; }
            if (ch == '}' && --depth < 0) return false;
        }
        return opened && depth == 0 && Type.IsMatch(code) && TopLevelPublicTypes(code).Count <= 1;
    }

    private static string ExtractFences(string response)
    {
        var lines = response.Replace("\r\n", "\n").Split('\n');
        var blocks = new List<string>();
        List<string>? current = null;
        var sawFence = false;
        var state = 0;
        foreach (var line in lines)
        {
            var trimmed = line.Trim();
            // Backtick lines inside Java comments, strings or text blocks are source, not framing.
            if (state == 0 && trimmed.StartsWith("```", StringComparison.Ordinal))
            {
                sawFence = true;
                if (current is not null)
                {
                    blocks.Add(string.Join('\n', current));
                    current = null;
                }
                else if (trimmed == "```" || trimmed.Equals("```java", StringComparison.OrdinalIgnoreCase))
                    current = [];
                else
                    throw new InvalidOperationException("Non-Java fenced output cannot be treated as a Java compilation unit.");
            }
            else
            {
                current?.Add(line);
                state = Scan(line + "\n", null, state);
            }
        }
        if (current is not null) blocks.Add(string.Join('\n', current));
        if (blocks.Count > 1 && blocks.Count(IsComplete) > 1)
            throw new InvalidOperationException("Multiple complete fenced Java units require separate output files.");
        return sawFence ? string.Join("\n", blocks).Trim() : response;
    }
}
