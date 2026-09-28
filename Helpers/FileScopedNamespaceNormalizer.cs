// Rewrites a generated C# file that declares more than one file-scoped namespace.
//
// A copybook's converted file owns the type built from that copybook, and that type lives in the
// shared namespace. A copybook that also contains a CALL is a caller too, so the same file carries
// service code for the program's namespace. The model emits both as `namespace X;` — and C# allows
// one file-scoped namespace per file (CS8954). The compiler then nests the second inside the first,
// so `using Modernized.Banking.Shared;` in the second resolves against a phantom
// `Modernized.Banking.Shared.Modernized.Banking` and every shared type it names goes missing.
// Measured on a five-program C# run, 13 of 71 files had this shape.
//
// Block-scoped namespaces may appear any number of times in one file, so each file-scoped
// declaration becomes a block around the text up to the next declaration. That is a change of
// syntax only: the same types land in the same namespaces. A file-scoped namespace followed by a
// block one (CS8955) is handled the same way. Files with a single namespace are never touched.

namespace CobolToQuarkusMigration.Helpers;

using System.Text;
using System.Text.RegularExpressions;

public static class FileScopedNamespaceNormalizer
{
    private static readonly Regex FileScopedNamespace = new(
        @"^[ \t]*namespace[ \t]+(?<name>[A-Za-z_][A-Za-z0-9_.]*)[ \t]*;[ \t]*(?<comment>//[^\r\n]*)?(?=\r?$)",
        RegexOptions.Multiline | RegexOptions.Compiled);

    // A block declaration, `namespace X` or `namespace X {`, at the start of a line.
    private static readonly Regex BlockNamespace = new(
        @"^[ \t]*namespace[ \t]+[A-Za-z_][A-Za-z0-9_.]*[ \t]*(?:\{[^\r\n]*)?(?://[^\r\n]*)?(?=\r?$)",
        RegexOptions.Multiline | RegexOptions.Compiled);

    /// <summary>
    /// The block-scoped rewrite of <paramref name="source"/>, or <c>null</c> when it declares a
    /// single namespace, or only block-scoped ones, and so needs no change.
    /// </summary>
    public static string? Normalize(string source)
    {
        var fileScoped = FileScopedNamespace.Matches(source);
        if (fileScoped.Count == 0) return null;

        // A block declaration after a file-scoped one is the same defect in another form (CS8955):
        // the compiler nests it, and the phantom `A.B.A.B.Shared` it creates captures every
        // `using A.B.Shared;` in namespace A.B, so real shared types stop resolving estate-wide.
        var declarations = fileScoped
            .Select(m => (Match: m, FileScoped: true))
            .Concat(BlockNamespace.Matches(source).Select(m => (Match: m, FileScoped: false)))
            .OrderBy(d => d.Match.Index)
            .ToList();
        if (declarations.Count < 2) return null;

        var newline = source.Contains("\r\n", StringComparison.Ordinal) ? "\r\n" : "\n";
        var sb = new StringBuilder(source.Length + declarations.Count * 8);

        // Whatever precedes the first declaration (header comments, usings) applies to the whole
        // file in either form, so it stays where it is.
        sb.Append(source, 0, declarations[0].Match.Index);

        for (var i = 0; i < declarations.Count; i++)
        {
            var (declaration, isFileScoped) = declarations[i];
            var segmentEnd = i + 1 < declarations.Count ? declarations[i + 1].Match.Index : source.Length;

            if (!isFileScoped)
            {
                // Already a block; it carries its own braces.
                sb.Append(source, declaration.Index, segmentEnd - declaration.Index);
                continue;
            }

            sb.Append("namespace ").Append(declaration.Groups["name"].Value);
            if (declaration.Groups["comment"].Success)
                sb.Append(' ').Append(declaration.Groups["comment"].Value);
            sb.Append(newline).Append('{');

            var bodyStart = declaration.Index + declaration.Length;
            sb.Append(source[bodyStart..segmentEnd].TrimEnd());
            sb.Append(newline).Append('}').Append(newline);
            if (i + 1 < declarations.Count) sb.Append(newline);
        }

        return sb.ToString();
    }

    /// <summary>
    /// Rewrites, in place, every <c>.cs</c> file under <paramref name="folder"/> that declares more
    /// than one file-scoped namespace, and returns the paths it rewrote.
    /// </summary>
    public static IReadOnlyList<string> NormalizeFolder(string folder)
    {
        if (!Directory.Exists(folder)) return [];

        var rewritten = new List<string>();
        foreach (var file in Directory.EnumerateFiles(folder, "*.cs", SearchOption.AllDirectories))
        {
            string text;
            try { text = File.ReadAllText(file); }
            catch (IOException) { continue; }

            var normalized = Normalize(text);
            if (normalized is null) continue;

            File.WriteAllText(file, normalized);
            rewritten.Add(file);
        }

        rewritten.Sort(StringComparer.Ordinal);
        return rewritten;
    }
}
