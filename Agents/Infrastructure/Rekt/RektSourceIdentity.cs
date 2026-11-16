using System.Text;
using System.Text.RegularExpressions;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Rekt;

// Decides whether source/ can be staged for Cobol-REKT without losing file identity. REKT resolves COPY
// targets by basename from one flat directory, so same-named copybooks are only unsafe when they differ
// and something names them. Findings are tab-separated: kind, lower-cased basename, source-relative paths.
public static class RektSourceIdentity
{
    public const string Summary = "Decide whether source/ can be staged for Cobol-REKT without losing file identity.";

    private static readonly string[] ProgramExtensions = [".cbl", ".cob"];
    private static readonly HashSet<string> SkipDirs = [".preprocessed", ".rekt-staging"];
    // The preprocessor aliases long copybook names to this length, so a reference this long can stand for
    // any longer name it prefixes.
    private const int AliasLength = 8;

    private const RegexOptions Options = RegexOptions.IgnoreCase | RegexOptions.CultureInvariant;
    private const string Name = "['\"]?([A-Za-z0-9#@$_-]+)";
    private static readonly Regex[] ReferencePatterns =
    [
        new(@"(?<![A-Za-z0-9-])COPY\s+" + Name, Options),
        new(@"\bEXEC\s+SQL\s+INCLUDE\s+" + Name, Options),
        new(@"\+\+INCLUDE\s+" + Name, Options),
        new(@"^\s*-INC\s+" + Name, Options),
    ];
    private static readonly Regex Identification = new(@"\G\s*(?:IDENTIFICATION|ID)\s+DIVISION\b", Options);
    private static readonly Regex LaterDivision = new(@"\G\s*(?:ENVIRONMENT|DATA|PROCEDURE)\s+DIVISION\b", Options);
    private static readonly Regex CommentEntry =
        new(@"\G\s*(?:AUTHOR|INSTALLATION|DATE-WRITTEN|DATE-COMPILED|SECURITY|REMARKS)\s*\.", Options);
    private static readonly Regex Paragraph =
        new(@"\G\s*(?:PROGRAM-ID|AUTHOR|INSTALLATION|DATE-WRITTEN|DATE-COMPILED|SECURITY|REMARKS)\b", Options);
    private static readonly Regex FreeFormat = new(@">>\s*SOURCE\s+(?:FORMAT\s+)?(?:IS\s+)?FREE\b", Options);
    private static readonly Regex SqlBoundary = new(@"\G(?:(EXEC\s+SQL)|END-EXEC)(?![A-Za-z0-9-])", Options);
    private static readonly Regex OperandKeyword = new(@"\b(?:COPY|CALL|INCLUDE)\s*$", Options);

    public static string ReadText(string path) => PyCompat.Latin1.GetString(File.ReadAllBytes(path));

    public static HashSet<string> ReferencedNames(string text)
    {
        var names = new HashSet<string>(StringComparer.Ordinal);
        foreach (var line in PyCompat.SplitLines(DependencyGuardText(text)))
            foreach (var pattern in ReferencePatterns)
                foreach (Match match in pattern.Matches(line))
                    names.Add(match.Groups[1].Value.ToUpperInvariant());
        return names;
    }

    private static bool IsCommentIndicator(string line) => line.Length > 6 && line[6] is '*' or '/';

    // Blank sequence areas and IDENTIFICATION comment-entries, keeping line and column positions.
    public static IEnumerable<string> ProgramTextLines(string text)
    {
        var free = FreeFormat.IsMatch(text);
        bool identification = false, documentary = false;
        foreach (var raw in PyCompat.SplitLines(text))
        {
            var fixedForm = !free && raw.Length > 6 && raw[6] is ' ' or '-' or '*' or '/' or 'D' or 'd'
                && raw.Take(6).All(c => c == ' ' || c is >= '0' and <= '9');
            var line = fixedForm ? "      " + Slice(raw, 6, 72) : raw;
            var area = fixedForm ? Slice(line, 7, line.Length) : line;
            // Comment records never change division state; DependencyGuardText drops them later.
            if (fixedForm && line[6] is '*' or '/' || area.TrimStart().StartsWith("*>", StringComparison.Ordinal))
            {
                yield return line;
                continue;
            }
            if (Identification.IsMatch(area)) identification = true;
            else if (LaterDivision.IsMatch(area)) identification = documentary = false;
            else if (documentary && (Paragraph.IsMatch(area) || fixedForm && Slice(area, 0, 4).Trim().Length > 0))
                documentary = false;
            // Comment-entries are free text, so an apostrophe there must not open a literal.
            Match entry;
            if (identification && (entry = CommentEntry.Match(area)).Success)
            {
                documentary = true;
                yield return line[..(line.Length - area.Length + entry.Index + entry.Length)];
            }
            else yield return documentary ? "" : line;
        }
    }

    private static string Slice(string text, int start, int end)
    {
        start = Math.Min(start, text.Length);
        end = Math.Min(end, text.Length);
        return end <= start ? "" : text[start..end];
    }

    // Mask literals/comments for contradiction checks, retaining quoted dependency operands.
    public static string DependencyGuardText(string text)
    {
        var result = new List<string>();
        char? quote = null;
        bool operand = false, sql = false;
        foreach (var line in ProgramTextLines(text))
        {
            if (IsCommentIndicator(line) || line.TrimStart().StartsWith("*>", StringComparison.Ordinal)) continue;
            var row = new StringBuilder();
            var index = 0;
            int? continuationQuote = null;
            if (quote is not null && line.Length > 6 && line[6] == '-')
            {
                var start = 7;
                while (start < line.Length && char.IsWhiteSpace(line[start])) start++;
                if (start < line.Length && line[start] == quote) continuationQuote = start;
            }
            while (index < line.Length)
            {
                var character = line[index];
                Match keyword;
                if (index == continuationQuote) row.Append(' ');
                else if (quote is not null)
                {
                    row.Append(operand ? character : ' ');
                    if (character == quote)
                    {
                        if (index + 1 < line.Length && line[index + 1] == quote)
                        {
                            row.Append(operand ? character : ' ');
                            index++;
                        }
                        else
                        {
                            quote = null;
                            operand = false;
                        }
                    }
                }
                else if (line.AsSpan(index).StartsWith("*>") || sql && line.AsSpan(index).StartsWith("--"))
                    break;
                else if ((index == 0 || !(char.IsLetterOrDigit(line[index - 1]) || line[index - 1] == '-'))
                         && (keyword = SqlBoundary.Match(line, index)).Success)
                {
                    sql = keyword.Groups[1].Success;
                    row.Append(line, index, keyword.Index + keyword.Length - index);
                    index = keyword.Index + keyword.Length;
                    continue;
                }
                else if (character is '\'' or '"')
                {
                    var prefix = (result.Count > 0 ? result[^1] : "") + row;
                    operand = OperandKeyword.IsMatch(prefix);
                    quote = character;
                    row.Append(operand ? character : ' ');
                }
                else row.Append(character);
                index++;
            }
            result.Add(row.ToString());
        }
        return string.Join("\n", result);
    }

    private static string Normalised(string text) =>
        string.Join(" ", text.Split((char[]?)null, StringSplitOptions.RemoveEmptyEntries)
            .SelectMany(part => part.Split(['\x1c', '\x1d', '\x1e', '\x1f'], StringSplitOptions.RemoveEmptyEntries)));

    private static string Stem(string basename) => Path.GetFileNameWithoutExtension(basename).ToUpperInvariant();

    private static bool IsUsed(string stem, Dictionary<string, HashSet<string>> references) =>
        references.ContainsKey(stem) || stem.Length > AliasLength && references.ContainsKey(stem[..AliasLength]);

    private static (Dictionary<string, List<string>> Programs, Dictionary<string, List<string>> Copybooks,
        Dictionary<string, HashSet<string>> References) Scan(string sourceRoot, IReadOnlySet<string> shadowed)
    {
        var programs = new Dictionary<string, List<string>>(StringComparer.Ordinal);
        var copybooks = new Dictionary<string, List<string>>(StringComparer.Ordinal);
        var references = new Dictionary<string, HashSet<string>>(StringComparer.Ordinal);
        var topOnly = Environment.GetEnvironmentVariable("REKT_SOURCE_TOP_LEVEL_ONLY") == "true";
        foreach (var (root, files) in PyCompat.Walk(sourceRoot,
                     name => !topOnly && !SkipDirs.Contains(name) && !name.StartsWith(".convert-", StringComparison.Ordinal)))
        {
            foreach (var name in files)
            {
                var path = Path.Join(root, name);
                var relative = Path.GetRelativePath(sourceRoot, path).Replace(Path.DirectorySeparatorChar, '/');
                var extension = Path.GetExtension(name).ToLowerInvariant();
                if (ProgramExtensions.Contains(extension))
                    Add(programs, name.ToLowerInvariant(), relative);
                else if (extension == ".cpy")
                {
                    if (shadowed.Contains(path)) continue;
                    Add(copybooks, name.ToLowerInvariant(), relative);
                }
                else continue;
                foreach (var referenced in ReferencedNames(ReadText(path)))
                {
                    if (!references.TryGetValue(referenced, out var users))
                        references[referenced] = users = new HashSet<string>(StringComparer.Ordinal);
                    users.Add(relative);
                }
            }
        }
        return (programs, copybooks, references);

        static void Add(Dictionary<string, List<string>> map, string key, string value)
        {
            if (!map.TryGetValue(key, out var list)) map[key] = list = [];
            list.Add(value);
        }
    }

    public static (List<(string Kind, string Basename, List<string> Paths)> Findings,
        List<(string Kind, string Basename, List<string> Paths)> Errors) Check(
        string sourceRoot, string preprocessedDir, IReadOnlySet<string>? shadowed = null)
    {
        var (programs, copybooks, references) = Scan(sourceRoot, shadowed ?? new HashSet<string>());
        var preprocessed = new Dictionary<string, string>(StringComparer.Ordinal);
        if (Directory.Exists(preprocessedDir))
            foreach (var path in Directory.EnumerateFileSystemEntries(preprocessedDir))
                if (File.Exists(path))
                    preprocessed[Path.GetFileName(path).ToLowerInvariant()] = Path.Join(preprocessedDir, Path.GetFileName(path));

        var findings = new List<(string, string, List<string>)>();
        var errors = new List<(string, string, List<string>)>();
        foreach (var (basename, unsorted) in copybooks.OrderBy(pair => pair.Key, StringComparer.Ordinal))
        {
            if (unsorted.Count < 2) continue;
            var paths = unsorted.Order(StringComparer.Ordinal).ToList();
            var contents = paths.Select(path => Normalised(ReadText(Path.Join(sourceRoot, path)))).Distinct().Count();
            var stem = Stem(basename);
            if (contents == 1) findings.Add(("copybook-duplicate-identical", basename, paths));
            else if (!IsUsed(stem, references)) findings.Add(("copybook-duplicate-unused", basename, paths));
            else
            {
                errors.Add(("copybook-collision", basename, paths));
                var users = new HashSet<string>(references.GetValueOrDefault(stem) ?? [], StringComparer.Ordinal);
                if (stem.Length > AliasLength && references.TryGetValue(stem[..AliasLength], out var aliased)) users.UnionWith(aliased);
                users.ExceptWith(paths);
                errors.Add(("copybook-collision-users", basename, users.Order(StringComparer.Ordinal).ToList()));
            }
        }
        foreach (var (basename, unsorted) in programs.OrderBy(pair => pair.Key, StringComparer.Ordinal))
        {
            if (unsorted.Count <= 1) continue;
            var paths = unsorted.Order(StringComparer.Ordinal).ToList();
            findings.Add(("program-duplicate", basename, paths));
            if (!preprocessed.TryGetValue(basename, out var rewrite)) continue;
            var rootProgram = paths.FirstOrDefault(path => !path.Contains('/'));
            if (rootProgram is null || RektEvidence.CurrentTransform(rewrite, Path.Join(sourceRoot, rootProgram)) is null)
                errors.Add(("preprocessed-program-collision", basename, paths));
        }
        return (findings, errors);
    }

    public static HashSet<string> ReadShadowed(string? path)
    {
        if (string.IsNullOrEmpty(path) || !File.Exists(path)) return [];
        var text = File.ReadAllText(path, Encoding.UTF8).Replace("\r\n", "\n").Replace('\r', '\n');
        return text.Split('\n').Where(line => line.Trim().Length > 0).ToHashSet(StringComparer.Ordinal);
    }

    // Returns the process exit code; 1 when any finding blocks staging.
    public static int Run(string sourceRoot, string preprocessedDir, string? shadowedFile, TextWriter output)
    {
        var (findings, errors) = Check(sourceRoot, preprocessedDir, ReadShadowed(shadowedFile));
        foreach (var (kind, basename, paths) in findings.Concat(errors))
            output.Write(string.Join('\t', new[] { kind, basename }.Concat(paths)) + "\n");
        return errors.Count > 0 ? 1 : 0;
    }
}
