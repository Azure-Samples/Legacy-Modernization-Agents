using System.Text.RegularExpressions;

namespace CobolToQuarkusMigration.Estate;

// Size and shape of a program from its own text, for Mission Control: how big it is, how branchy,
// and how much of it is CICS, SQL or DL/I. Counts, not judgements.
public static class EstateProgramMetrics
{
    private const RegexOptions Opts = RegexOptions.IgnoreCase | RegexOptions.CultureInvariant;
    // Paragraph and section headers alike: a name alone on its line in area A.
    private static readonly Regex Paragraph = new(@"^\s{0,3}([A-Z0-9][A-Z0-9-]*)(?:\s+SECTION)?\s*\.\s*$", Opts | RegexOptions.Multiline);
    private static readonly Regex If = new(@"(?<![\w-])IF(?![\w-])", Opts);
    private static readonly Regex When = new(@"(?<![\w-])WHEN(?![\w-])", Opts);
    private static readonly Regex Until = new(@"(?<![\w-])UNTIL(?![\w-])", Opts);
    private static readonly Regex Perform = new(@"(?<![\w-])PERFORM(?![\w-])", Opts);
    private static readonly Regex GoTo = new(@"(?<![\w-])GO\s+TO(?![\w-])", Opts);
    private static readonly Regex Exec = new(@"(?<![\w-])EXEC\s+(CICS|SQL|DLI)(?![\w-])", Opts);
    private static readonly Regex Procedure = new(@"(?<![\w-])PROCEDURE\s+DIVISION", Opts);
    private static readonly Regex FreeFormat = new(@">>\s*SOURCE\s+(?:FORMAT\s+)?(?:IS\s+)?FREE", Opts);
    private static readonly Regex Words = new(@"[A-Za-z]{2,}", RegexOptions.CultureInvariant);
    private static readonly Regex Boilerplate = new(@"copyright|licen[cs]|all rights|www\.|https?:|^(program|author|date|installation|source-computer)\b", Opts);
    private static readonly Regex FunctionLine = new(@"^function\s*:\s*(.+)$", Opts);

    public const int MaxDescriptionLength = 180;
    private const int HeaderCommentLines = 80;

    public static SortedDictionary<string, int> Measure(string source)
    {
        var (code, _) = Split(source);
        var proc = Procedure.Match(code);
        var body = proc.Success ? code[proc.Index..] : "";
        var exec = Exec.Matches(body).GroupBy(m => m.Groups[1].Value.ToUpperInvariant()).ToDictionary(g => g.Key, g => g.Count());
        var ifs = If.Matches(body).Count;
        return new SortedDictionary<string, int>(StringComparer.Ordinal)
        {
            ["lines"] = source.Split('\n').Length - (source.EndsWith('\n') ? 1 : 0),
            ["paragraphs"] = Paragraph.Matches(body).Count,
            ["complexity"] = 1 + ifs + When.Matches(body).Count + Until.Matches(body).Count,
            ["perform"] = Perform.Matches(body).Count,
            ["goto"] = GoTo.Matches(body).Count,
            ["execCics"] = exec.GetValueOrDefault("CICS"),
            ["execSql"] = exec.GetValueOrDefault("SQL"),
            ["execDli"] = exec.GetValueOrDefault("DLI"),
        };
    }

    // A 'FUNCTION:' header line when there is one, else the first run of prose in the header comments,
    // skipping licence and banner lines.
    public static string? Description(string source)
    {
        var (_, comments) = Split(source);
        var header = comments.Take(HeaderCommentLines).Select(c => c.Trim(' ', '*', '-', '=')).ToList();
        foreach (var c in header)
            if (FunctionLine.Match(c) is { Success: true } f && f.Groups[1].Value.Trim().Length > 3)
                return Clip(f.Groups[1].Value.Trim());

        var prose = new List<string>();
        foreach (var c in header)
        {
            if (Boilerplate.IsMatch(c))
            {
                if (prose.Count > 0) break;
                continue;
            }
            if (Words.Matches(c).Count >= 4)
            {
                prose.Add(c);
                if (string.Join(' ', prose).Length > 150) break;
            }
            else if (prose.Count > 0) break;
        }
        return prose.Count == 0 ? null : Clip(string.Join(' ', prose));

        static string Clip(string s) => s.Length <= MaxDescriptionLength ? s : s[..MaxDescriptionLength];
    }

    // Code (columns 8-72, or the whole line in free format) and comment lines, kept apart.
    private static (string Code, List<string> Comments) Split(string source)
    {
        var free = FreeFormat.IsMatch(source);
        var code = new System.Text.StringBuilder();
        var comments = new List<string>();
        foreach (var raw in source.Split('\n'))
        {
            var line = raw.TrimEnd('\r');
            if (line.TrimStart().StartsWith("*>"))
            {
                comments.Add(line.TrimStart()[2..]);
                continue;
            }
            if (free)
            {
                code.Append(line).Append('\n');
                continue;
            }
            var indicator = line.Length > 6 ? line[6] : ' ';
            var area = line.Length > 7 ? line[7..Math.Min(line.Length, 72)] : "";
            if (indicator is '*' or '/')
            {
                comments.Add(area);
                continue;
            }
            code.Append(area).Append('\n');
        }
        return (code.ToString(), comments);
    }
}
