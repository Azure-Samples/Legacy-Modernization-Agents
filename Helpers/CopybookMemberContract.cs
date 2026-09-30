// The member names and types a copybook's type exposes, derived from the copybook alone so the
// file that declares the type and every file that uses it agree without seeing each other.

namespace CobolToQuarkusMigration.Helpers;

using System.Text;
using System.Text.RegularExpressions;

public sealed record CopybookMember(string CobolName, string Name, string CSharpType, string JavaType);

public static class CopybookMemberContract
{
    private static readonly Regex Tag = new(@":[A-Za-z0-9_-]+:", RegexOptions.Compiled);
    private static readonly Regex Level = new(@"^(?<level>\d{1,2})(?:\s+(?<name>[A-Za-z0-9:][A-Za-z0-9:_-]*))?", RegexOptions.Compiled);
    private static readonly Regex Occurs = new(@"\bOCCURS\s+(?:\d+\s+TO\s+)?\d+", RegexOptions.IgnoreCase | RegexOptions.Compiled);
    private static readonly Regex Picture = new(@"\bPIC(?:TURE)?\s+(?:IS\s+)?(?<pic>\S+)", RegexOptions.IgnoreCase | RegexOptions.Compiled);
    private static readonly Regex Repeat = new(@"(?<c>[9XAN])\((?<n>\d+)\)", RegexOptions.IgnoreCase | RegexOptions.Compiled);

    private sealed record Entry(int Level, string CobolName, int Dimensions, string? Picture, string Text)
    {
        public bool IsGroup { get; set; }
    }

    /// <summary>
    /// One member per named data item, flat on the copybook's type. A lone 01 record is the type
    /// itself and is not listed; items under an OCCURS are arrays; 88-levels are booleans.
    /// Placeholders such as <c>:XXXX:</c> are dropped, since one type serves every REPLACING.
    /// </summary>
    /// <param name="resolveCopybook">Text of a copybook this one copies, by name; a nested COPY is
    /// part of the record exactly as if its text were written in place.</param>
    public static IReadOnlyList<CopybookMember> Parse(
        string copybookText, string typeName, Func<string, string?>? resolveCopybook = null)
    {
        var entries = new List<Entry>();
        var dimensionsAt = new Stack<(int Level, int Dimensions)>();

        foreach (var text in Entries(copybookText, resolveCopybook, new HashSet<string>(StringComparer.OrdinalIgnoreCase)))
        {
            var match = Level.Match(text);
            if (!match.Success) continue;
            var level = int.Parse(match.Groups["level"].Value);
            var name = match.Groups["name"].Success ? match.Groups["name"].Value : "FILLER";
            if (name.Equals("PIC", StringComparison.OrdinalIgnoreCase) || name.Equals("PICTURE", StringComparison.OrdinalIgnoreCase))
                name = "FILLER";

            if (level == 88)
            {
                var owner = entries.LastOrDefault(e => e.Level != 88);
                entries.Add(new Entry(88, name, owner?.Dimensions ?? 0, null, text));
                continue;
            }

            if (level is 66 or 77) level = 1;
            while (dimensionsAt.Count > 0 && dimensionsAt.Peek().Level >= level) dimensionsAt.Pop();
            var inherited = dimensionsAt.Count > 0 ? dimensionsAt.Peek().Dimensions : 0;
            var dimensions = inherited + (Occurs.IsMatch(text) ? 1 : 0);
            dimensionsAt.Push((level, dimensions));

            var parent = entries.LastOrDefault(e => e.Level != 88);
            if (parent is not null && parent.Level < level) parent.IsGroup = true;

            var pic = Picture.Match(text);
            entries.Add(new Entry(level, name, dimensions, pic.Success ? pic.Groups["pic"].Value.TrimEnd('.') : null, text));
        }

        var records = entries.Count(e => e.Level == 1);
        var members = new List<CopybookMember>();
        var seen = new HashSet<string>(StringComparer.Ordinal);
        foreach (var e in entries)
        {
            if (e.CobolName.Equals("FILLER", StringComparison.OrdinalIgnoreCase)) continue;
            if (e.Level == 1 && records == 1) continue;

            var name = MemberName(e.CobolName, typeName);
            if (name.Length == 0 || !seen.Add(name)) continue;

            var (cs, java) = e.Level == 88 ? ("bool", "boolean")
                : e.IsGroup ? ("string", "String")
                : ScalarTypes(e.Picture, e.Text);
            members.Add(new CopybookMember(e.CobolName, name, cs + Rank(e.Dimensions, cs: true), java + Rank(e.Dimensions, cs: false)));
        }

        return members;
    }

    public static string MemberName(string cobolName, string typeName)
    {
        var name = NamingHelper.ToPascalCase(Tag.Replace(cobolName, "-"));
        if (name.Length > 0 && char.IsDigit(name[0])) name = "_" + name;
        return string.Equals(name, typeName, StringComparison.Ordinal) ? name + "Value" : name;
    }

    /// <summary>
    /// The members grouped by type, one line per type, in the target language's casing. Every file
    /// using a copybook carries this, so it is kept to names and types.
    /// </summary>
    public static string Render(IReadOnlyList<CopybookMember> members, string targetLanguage, string indent)
    {
        var java = !ConversionNamespacePolicy.IsCSharp(targetLanguage);
        return string.Join(Environment.NewLine, members
            .GroupBy(m => java ? m.JavaType : m.CSharpType)
            .Select(g => indent + g.Key + ": " + string.Join(", ",
                g.Select(m => java ? char.ToLowerInvariant(m.Name[0]) + m.Name[1..] : m.Name))));
    }

    private static string Rank(int dimensions, bool cs) =>
        dimensions == 0 ? "" : cs ? "[" + new string(',', dimensions - 1) + "]" : string.Concat(Enumerable.Repeat("[]", dimensions));

    private static (string CSharp, string Java) ScalarTypes(string? picture, string text)
    {
        var upper = text.ToUpperInvariant();
        if (Regex.IsMatch(upper, @"\bCOMP(UTATIONAL)?-1\b")) return ("float", "float");
        if (Regex.IsMatch(upper, @"\bCOMP(UTATIONAL)?-2\b")) return ("double", "double");
        if (picture is null) return ("string", "String");

        var pic = Repeat.Replace(picture.ToUpperInvariant(), m => new string(m.Groups["c"].Value[0], int.Parse(m.Groups["n"].Value)));
        if (pic.IndexOfAny(['X', 'A', 'N']) >= 0) return ("string", "String");
        if (pic.IndexOfAny(['Z', '*', '+', '-', ',', '.', 'B', '0', '/', 'C', 'D', 'E']) >= 0) return ("string", "String");

        var digits = pic.Count(c => c == '9' || c == 'P');
        if (pic.Contains('V')) return ("decimal", "java.math.BigDecimal");
        if (digits <= 9) return ("int", "int");
        if (digits <= 18) return ("long", "long");
        return ("decimal", "java.math.BigDecimal");
    }

    private static readonly Regex CopyStatement = new(
        @"^COPY\s+[""']?(?<name>[A-Za-z0-9_-]+)[""']?(?:\s+(?:OF|IN)\s+\S+)?(?<replacing>\s+REPLACING\s+.*)?$",
        RegexOptions.IgnoreCase | RegexOptions.Compiled);

    private static readonly Regex Replacement = new(
        @"==(?<from>.*?)==\s+BY\s+==(?<to>.*?)==", RegexOptions.IgnoreCase | RegexOptions.Compiled);

    /// <summary>Data entries of a copybook, one string per period-terminated entry, nested COPYs expanded.</summary>
    private static IEnumerable<string> Entries(string copybookText, Func<string, string?>? resolve, HashSet<string> visiting)
    {
        foreach (var entry in RawEntries(copybookText))
        {
            var copy = CopyStatement.Match(entry);
            if (!copy.Success) { yield return entry; continue; }

            var name = copy.Groups["name"].Value;
            var nested = resolve?.Invoke(name);
            if (nested is null || !visiting.Add(name)) continue;
            foreach (Match r in Replacement.Matches(copy.Groups["replacing"].Value))
                nested = nested.Replace(r.Groups["from"].Value.Trim(), r.Groups["to"].Value.Trim(), StringComparison.OrdinalIgnoreCase);
            foreach (var inner in Entries(nested, resolve, visiting)) yield return inner;
            visiting.Remove(name);
        }
    }

    private static IEnumerable<string> RawEntries(string copybookText)
    {
        var area = new StringBuilder();
        foreach (var raw in copybookText.Replace("\r", "").Split('\n'))
        {
            // Fixed format keeps a sequence area in columns 1-6 and an indicator in 7; free format
            // starts the entry anywhere and comments with *>.
            var fixedFormat = raw.Length > 6 && raw.Take(6).All(c => char.IsDigit(c) || c == ' ');
            if (fixedFormat && (raw[6] == '*' || raw[6] == '/')) continue;
            var line = fixedFormat ? raw[7..Math.Min(raw.Length, 72)] : raw.Length > 6 ? raw : raw.Trim();
            var comment = line.IndexOf("*>", StringComparison.Ordinal);
            if (comment >= 0) line = line[..comment];
            if (line.TrimStart().StartsWith('*')) continue;
            area.Append(line).Append(' ');
        }

        var current = new StringBuilder();
        char? quote = null;
        var all = area.ToString();
        for (var i = 0; i < all.Length; i++)
        {
            var c = all[i];
            current.Append(c);
            if (quote is not null) { if (c == quote) quote = null; continue; }
            if (c is '\'' or '"') { quote = c; continue; }
            if (c == '.' && (i + 1 == all.Length || char.IsWhiteSpace(all[i + 1])))
            {
                var entry = Regex.Replace(current.ToString(), @"\s+", " ").Trim().TrimEnd('.').Trim();
                current.Clear();
                if (entry.Length == 0 || entry.StartsWith("EXEC ", StringComparison.OrdinalIgnoreCase)) continue;
                yield return entry;
            }
        }
    }
}
