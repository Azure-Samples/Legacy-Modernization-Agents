namespace CobolToQuarkusMigration.Jcl;

// The operand field of a statement, split at top-level commas into positional and keyword
// parameters. Values are kept as written; Sublist and Unquote read them.
public sealed class JclOperands
{
    public List<string> Positional { get; } = [];

    // Keyword order is kept: the first PARM on an EXEC applies to the first procedure step only.
    public List<KeyValuePair<string, string>> Keywords { get; } = [];

    public string? Keyword(string key) =>
        Keywords.LastOrDefault(k => k.Key.Equals(key, StringComparison.OrdinalIgnoreCase)) is { Key: not null } kv ? kv.Value : null;

    public bool Has(string key) => Keywords.Any(k => k.Key.Equals(key, StringComparison.OrdinalIgnoreCase));

    public static JclOperands Parse(string operands)
    {
        var result = new JclOperands();
        foreach (var item in SplitTopLevel(operands))
        {
            var eq = TopLevelEquals(item);
            if (eq > 0) result.Keywords.Add(new(item[..eq].ToUpperInvariant(), item[(eq + 1)..]));
            else result.Positional.Add(item);
        }

        return result;
    }

    // The items of a parenthesised list; a value with no parentheses is a list of itself.
    public static IReadOnlyList<string> Sublist(string? value)
    {
        if (string.IsNullOrEmpty(value)) return [];
        var v = value.Trim();
        return v.StartsWith('(') && v.EndsWith(')') && MatchingClose(v, 0) == v.Length - 1
            ? SplitTopLevel(v[1..^1])
            : [v];
    }

    public static string Unquote(string value)
    {
        var v = value.Trim();
        return v.Length >= 2 && v[0] == '\'' && v[^1] == '\'' ? v[1..^1].Replace("''", "'") : v;
    }

    public static List<string> SplitTopLevel(string text)
    {
        var items = new List<string>();
        var depth = 0;
        var inQuote = false;
        var start = 0;
        for (var i = 0; i < text.Length; i++)
        {
            var c = text[i];
            if (c == '\'') inQuote = !inQuote;
            else if (inQuote) continue;
            else if (c == '(') depth++;
            else if (c == ')') depth--;
            else if (c == ',' && depth == 0)
            {
                items.Add(text[start..i]);
                start = i + 1;
            }
        }

        if (start < text.Length || text.EndsWith(',')) items.Add(text[start..]);
        return items;
    }

    private static int TopLevelEquals(string item)
    {
        var inQuote = false;
        for (var i = 0; i < item.Length; i++)
        {
            var c = item[i];
            if (c == '\'') inQuote = !inQuote;
            else if (inQuote) continue;
            else if (c == '(') return -1;
            else if (c == '=') return i;
        }

        return -1;
    }

    private static int MatchingClose(string text, int open)
    {
        var depth = 0;
        var inQuote = false;
        for (var i = open; i < text.Length; i++)
        {
            var c = text[i];
            if (c == '\'') inQuote = !inQuote;
            else if (inQuote) continue;
            else if (c == '(') depth++;
            else if (c == ')' && --depth == 0) return i;
        }

        return -1;
    }
}
