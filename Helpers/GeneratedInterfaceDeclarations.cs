namespace CobolToQuarkusMigration.Helpers;

using System.Text.RegularExpressions;

// Removes model-written declarations of interfaces the scaffolding generates, so each is declared once
// with the signature read from the COBOL. Only whole interface blocks with a generated name are touched.
public static class GeneratedInterfaceDeclarations
{
    public static string RemoveFrom(string code, IReadOnlyCollection<string> interfaceNames)
    {
        if (interfaceNames.Count == 0) return code;
        var names = string.Join("|", interfaceNames.Select(Regex.Escape));
        var header = new Regex(
            @"(?<docs>(?:^[ \t]*(?:///|\[)[^\n]*\n)*)^[ \t]*(?:(?:public|internal|partial)\s+)*interface\s+(?:" + names + @")\b[^{;]*\{",
            RegexOptions.Multiline);

        while (header.Match(code) is { Success: true } match)
        {
            var end = MatchingBrace(code, match.Index + match.Length - 1);
            if (end < 0) break;
            var stop = end + 1;
            while (stop < code.Length && code[stop] is ' ' or '\t' or '\r') stop++;
            if (stop < code.Length && code[stop] == '\n') stop++;
            code = code[..match.Index] + code[stop..];
        }
        return code;
    }

    private static int MatchingBrace(string code, int open)
    {
        var depth = 0;
        for (var i = open; i < code.Length; i++)
        {
            switch (code[i])
            {
                case '/' when i + 1 < code.Length && code[i + 1] == '/':
                    i = code.IndexOf('\n', i); if (i < 0) return -1; break;
                case '/' when i + 1 < code.Length && code[i + 1] == '*':
                    i = code.IndexOf("*/", i + 2, StringComparison.Ordinal); if (i < 0) return -1; i++; break;
                case '"':
                    for (i++; i < code.Length && code[i] != '"'; i++) if (code[i] == '\\') i++;
                    break;
                case '\'':
                    for (i++; i < code.Length && code[i] != '\''; i++) if (code[i] == '\\') i++;
                    break;
                case '{': depth++; break;
                case '}': if (--depth == 0) return i; break;
            }
        }
        return -1;
    }
}
