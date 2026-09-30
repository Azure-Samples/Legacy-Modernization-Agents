// Which generated file declares which type, read from the source text. The compiler says a type is
// missing or declared twice; this says where it is declared, which is what a repair needs to know
// and what the compiler does not report.

using System.Text.RegularExpressions;

namespace CobolToQuarkusMigration.Helpers;

public sealed record TypeDeclaration(string Name, string Namespace, string File);

public sealed class GeneratedTypeIndex
{
    private static readonly Regex Namespace = new(
        @"^[ \t]*namespace[ \t]+(?<name>[A-Za-z_][A-Za-z0-9_.]*)",
        RegexOptions.Multiline | RegexOptions.Compiled);

    private static readonly Regex Declaration = new(
        @"^[ \t]*(?:\[[^\]\r\n]*\][ \t]*)*(?:(?:public|internal|private|protected|file|static|sealed|abstract|partial|readonly|ref|unsafe|new)[ \t]+)*" +
        @"(?:record[ \t]+(?:class|struct)|class|record|struct|interface|enum)[ \t]+(?<name>[A-Za-z_][A-Za-z0-9_]*)",
        RegexOptions.Multiline | RegexOptions.Compiled);

    private readonly List<TypeDeclaration> _declarations;

    private GeneratedTypeIndex(List<TypeDeclaration> declarations) => _declarations = declarations;

    public IReadOnlyList<TypeDeclaration> Declarations => _declarations;

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
            foreach (Match match in Declaration.Matches(text))
            {
                var ns = namespaces.LastOrDefault(n => n.Index < match.Index)?.Groups["name"].Value ?? string.Empty;
                declarations.Add(new TypeDeclaration(match.Groups["name"].Value, ns, file));
            }
        }

        return new GeneratedTypeIndex(declarations);
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
