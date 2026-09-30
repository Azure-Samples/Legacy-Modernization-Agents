// Models sometimes emit `&amp;&amp;` or `&lt;` where code needs `&&` or `<`, most likely carried over
// from the XML doc comments they write next to it. In a doc comment the entity is correct XML, and
// inside a string it is data, so only the code between them is decoded. The scan is lexical: it
// tracks comments and string/char literals for C# and Java, which is all it needs to know.

using System.Text;

namespace CobolToQuarkusMigration.Agents;

internal static class GeneratedCodeEntities
{
    private static readonly (string Entity, char Value)[] Entities =
    [
        // Quote entities are left alone: decoding one would open a literal this scan never saw.
        ("&amp;", '&'), ("&lt;", '<'), ("&gt;", '>'),
    ];

    /// <summary>
    /// <paramref name="code"/> with XML entities in code decoded, or the same instance when there
    /// are none outside comments and literals.
    /// </summary>
    public static string DecodeInCode(string code)
    {
        if (string.IsNullOrEmpty(code) || code.IndexOf('&') < 0) return code;

        var sb = new StringBuilder(code.Length);
        var changed = false;
        var i = 0;
        while (i < code.Length)
        {
            var c = code[i];
            var next = i + 1 < code.Length ? code[i + 1] : '\0';

            if (c == '/' && next == '/')
            {
                i = CopyUntil(code, i, sb, code.IndexOf('\n', i));
            }
            else if (c == '/' && next == '*')
            {
                var end = code.IndexOf("*/", i + 2, StringComparison.Ordinal);
                i = CopyUntil(code, i, sb, end < 0 ? -1 : end + 2);
            }
            else if (c == '"' && Starts(code, i, "\"\"\""))
            {
                // C# raw string / Java text block.
                var end = code.IndexOf("\"\"\"", i + 3, StringComparison.Ordinal);
                i = CopyUntil(code, i, sb, end < 0 ? -1 : end + 3);
            }
            else if (c == '"' || c == '\'')
            {
                var verbatim = c == '"' && IsVerbatimPrefix(code, i);
                i = CopyUntil(code, i, sb, EndOfLiteral(code, i, verbatim));
            }
            else if (c == '&' && TryDecode(code, i, out var value, out var length))
            {
                sb.Append(value);
                i += length;
                changed = true;
            }
            else
            {
                sb.Append(c);
                i++;
            }
        }

        return changed ? sb.ToString() : code;
    }

    private static bool TryDecode(string code, int at, out char value, out int length)
    {
        foreach (var (entity, decoded) in Entities)
        {
            if (Starts(code, at, entity))
            {
                value = decoded;
                length = entity.Length;
                return true;
            }
        }

        value = '\0';
        length = 0;
        return false;
    }

    private static bool Starts(string code, int at, string text) =>
        string.CompareOrdinal(code, at, text, 0, text.Length) == 0;

    // @"..." and $@"..." / @$"..." keep backslashes literal and double their quotes instead.
    private static bool IsVerbatimPrefix(string code, int quote)
    {
        for (var j = quote - 1; j >= 0 && j >= quote - 2; j--)
        {
            if (code[j] == '@') return true;
            if (code[j] != '$') return false;
        }

        return false;
    }

    private static int EndOfLiteral(string code, int open, bool verbatim)
    {
        var quote = code[open];
        for (var j = open + 1; j < code.Length; j++)
        {
            var ch = code[j];
            if (verbatim)
            {
                if (ch != quote) continue;
                if (j + 1 < code.Length && code[j + 1] == quote) { j++; continue; }
                return j + 1;
            }

            if (ch == '\\') { j++; continue; }
            if (ch == quote) return j + 1;
            if (ch == '\n') return j; // Unterminated: never swallow the rest of the file.
        }

        return -1;
    }

    private static int CopyUntil(string code, int from, StringBuilder sb, int end)
    {
        if (end < 0) end = code.Length;
        sb.Append(code, from, end - from);
        return end;
    }
}
