// Which generated file declares which type, read from the source text. The compiler says a type is
// missing or declared twice; this says where it is declared, which is what a repair needs to know
// and what the compiler does not report.

using System.Text.RegularExpressions;

namespace CobolToQuarkusMigration.Helpers;

/// <param name="Container">The dotted enclosing type for a nested type (<c>Outer.Inner</c>); null when top-level.</param>
/// <param name="VisibleOutside">False for a nested type other files cannot use: no modifier (nested types default to
/// private), private, protected or private protected.</param>
public sealed record TypeDeclaration(string Name, string Namespace, string File, string? Container = null, bool VisibleOutside = true);

public sealed class GeneratedTypeIndex
{
    private static readonly Regex Namespace = new(
        @"^[ \t]*namespace[ \t]+(?<name>[A-Za-z_][A-Za-z0-9_.]*)",
        RegexOptions.Multiline | RegexOptions.Compiled);

    private static readonly Regex Declaration = new(
        @"^[ \t]*(?:\[[^\]\r\n]*\][ \t]*)*(?<mods>(?:(?:public|internal|private|protected|file|static|sealed|abstract|partial|readonly|ref|unsafe|new)[ \t]+)*)" +
        @"(?:record[ \t]+(?:class|struct)|class|record|struct|interface|enum)[ \t]+(?<name>[A-Za-z_][A-Za-z0-9_]*)",
        RegexOptions.Multiline | RegexOptions.Compiled);

    private readonly List<TypeDeclaration> _declarations;

    private GeneratedTypeIndex(List<TypeDeclaration> declarations) => _declarations = declarations;

    public IReadOnlyList<TypeDeclaration> Declarations => _declarations;

    private static readonly Regex ClassDeclaration = new(
        @"^[ \t]*(?:\[[^\]\r\n]*\][ \t]*)*(?:(?:public|internal|file|static|sealed|abstract|partial)[ \t]+)*(?:record[ \t]+)?class[ \t]+(?<name>[A-Za-z_][A-Za-z0-9_]*)",
        RegexOptions.Multiline | RegexOptions.Compiled);

    /// <summary>
    /// The first top-level class in <paramref name="code"/>, which names the output file. A nested class is
    /// never chosen: two programs nesting the same WsCicsWorkArea would otherwise collide on one file.
    /// </summary>
    public static string? PrimaryClassName(string code)
    {
        var topLevel = FromSources(new Dictionary<string, string> { [""] = code }).Declarations
            .Where(d => d.Container is null).Select(d => d.Name).ToHashSet(StringComparer.Ordinal);
        return ClassDeclaration.Matches(code).Select(m => m.Groups["name"].Value).FirstOrDefault(topLevel.Contains);
    }

    /// <summary>Indexes every <c>.cs</c> file under <paramref name="runFolder"/>, outside bin and obj.</summary>
    public static GeneratedTypeIndex Build(string runFolder)
    {
        var files = Directory.EnumerateFiles(runFolder, "*.cs", SearchOption.AllDirectories)
            .Select(path => (Path: path, Relative: Path.GetRelativePath(runFolder, path).Replace('\\', '/')))
            .Where(f => !f.Relative.StartsWith("bin/", StringComparison.Ordinal)
                        && !f.Relative.StartsWith("obj/", StringComparison.Ordinal))
            .ToDictionary(f => f.Relative, f => File.ReadAllText(f.Path), StringComparer.Ordinal);
        return FromSources(files);
    }

    public static GeneratedTypeIndex FromSources(IReadOnlyDictionary<string, string> sources)
    {
        var declarations = new List<TypeDeclaration>();
        foreach (var (file, text) in sources.OrderBy(s => s.Key, StringComparer.Ordinal))
        {
            var namespaces = Namespace.Matches(text).ToList();
            var matches = Declaration.Matches(text).ToList();
            var bodies = matches.Select(m => Body(text, m.Index + m.Length)).ToList();
            for (var i = 0; i < matches.Count; i++)
            {
                var match = matches[i];
                var ns = namespaces.LastOrDefault(n => n.Index < match.Index)?.Groups["name"].Value ?? string.Empty;
                var outer = new List<string>();
                for (var j = 0; j < i; j++)
                    if (bodies[j] is var (start, end) && start < match.Index && match.Index < end)
                        outer.Add(matches[j].Groups["name"].Value);
                var mods = match.Groups["mods"].Value.Split((char[])[' ', '\t'], StringSplitOptions.RemoveEmptyEntries);
                var visible = outer.Count == 0
                    || mods.Contains("public") || (mods.Contains("internal") && !mods.Contains("private"));
                declarations.Add(new TypeDeclaration(match.Groups["name"].Value, ns, file,
                    outer.Count == 0 ? null : string.Join('.', outer), visible));
            }
        }

        return new GeneratedTypeIndex(declarations);
    }

    /// <summary>
    /// The brace span of the type body that follows a declaration header, or null for a bodiless
    /// declaration (<c>record R(int X);</c>). Braces inside strings, chars and comments are ignored.
    /// </summary>
    private static (int Start, int End)? Body(string text, int from)
    {
        int open = -1, depth = 0, parens = 0;
        for (var i = from; i < text.Length; i++)
        {
            var c = text[i];
            if (c == '/' && i + 1 < text.Length && text[i + 1] == '/') { i = SkipTo(text, i, "\n"); continue; }
            if (c == '/' && i + 1 < text.Length && text[i + 1] == '*') { i = SkipTo(text, i + 2, "*/") + 1; continue; }
            if (c is '"' or '\'') { i = SkipLiteral(text, i); continue; }
            if (open < 0)
            {
                if (c == '(') parens++;
                else if (c == ')') parens--;
                else if (c == ';' && parens == 0) return null;
                else if (c == '{' && parens == 0) { open = i; depth = 1; }
                continue;
            }
            if (c == '{') depth++;
            else if (c == '}' && --depth == 0) return (open, i);
        }
        return open < 0 ? null : (open, text.Length);
    }

    private static int SkipTo(string text, int from, string end)
    {
        var at = text.IndexOf(end, from, StringComparison.Ordinal);
        return at < 0 ? text.Length : at;
    }

    private static int SkipLiteral(string text, int at)
    {
        var quote = text[at];
        var verbatim = quote == '"' && at > 0 && (text[at - 1] == '@' || (at > 1 && text[at - 2] == '@'));
        for (var i = at + 1; i < text.Length; i++)
        {
            if (!verbatim && text[i] == '\\') { i++; continue; }
            if (text[i] != quote) continue;
            if (verbatim && i + 1 < text.Length && text[i + 1] == '"') { i++; continue; }
            return i;
        }
        return text.Length;
    }

    public IReadOnlyList<TypeDeclaration> Find(string name) =>
        _declarations.Where(d => d.Name == name).ToList();

    public IReadOnlyList<string> TypesDeclaredIn(string file) =>
        _declarations.Where(d => d.File == file).Select(d => d.Name).ToList();

    /// <summary>
    /// The one file that keeps a type several files declare: the file named after the type when there
    /// is one, since that is the conversion the type was built from, otherwise the first in path order.
    /// The rule has to be stable, or two repairs could each delete the other's declaration.
    /// </summary>
    public static string ChooseOwner(string typeName, IEnumerable<string> files)
    {
        var ordered = files.Distinct(StringComparer.Ordinal).OrderBy(f => f, StringComparer.Ordinal).ToList();
        return ordered.FirstOrDefault(f =>
                   string.Equals(Path.GetFileNameWithoutExtension(f), typeName, StringComparison.OrdinalIgnoreCase))
               ?? ordered.First();
    }
}
