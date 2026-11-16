using System.Text;
using System.Text.Json.Nodes;
using System.Text.RegularExpressions;
using CobolToQuarkusMigration.Agents.Infrastructure.Rekt;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Doctor;

// What run_rekt_parse_impl in doctor.sh reads between parser calls. Output, ordering and failure modes
// are the ones the shell already relies on, so a crash part-way still leaves the lines printed before it.
public static class RektParseReports
{
    // COBOL names contain hyphens, so `\b` would read REQUEST-MSG-COPY REPLACING as a COPY of REPLACING.
    private static readonly Regex MissingCopyDirective = PyRegex.Compile(
        @"^[^*]{0,6}[^*\n].*?(?<![A-Z0-9_-])COPY\s+['""]?([A-Z][A-Z0-9_-]*)['""]?", RegexOptions.IgnoreCase);

    private static readonly Regex CopyDirective = PyRegex.Compile(
        @"(?<![A-Z0-9_-])COPY\s+['""]?([A-Z][A-Z0-9_-]*)['""]?", RegexOptions.IgnoreCase);

    // Lines of a UTF-8 text file as Python iterates them: universal newlines, terminators kept.
    public static List<string> ReadLines(string path, Encoding? encoding = null)
    {
        var text = PyCompat.ReadText(path, encoding);
        var lines = new List<string>();
        var start = 0;
        while (start < text.Length)
        {
            var end = text.IndexOf('\n', start);
            if (end < 0) { lines.Add(text[start..]); break; }
            lines.Add(text[start..(end + 1)]);
            start = end + 1;
        }
        return lines;
    }

    public static List<string> NonBlankLines(string path) =>
        ReadLines(path).Select(PyCompat.Strip).Where(line => line.Length > 0).ToList();

    // Resolves _REKT_PROGRAM_FILTER: exact path, then unique basename, then unique stem, all case-insensitive.
    // Returns false with the single line to show when a selector is ambiguous or unknown.
    public static bool ResolveProgramFilter(IReadOnlyList<string> programs, string selectors, out List<string> lines)
    {
        var exact = new Dictionary<string, string>(StringComparer.Ordinal);
        var byBasename = new Dictionary<string, List<string>>(StringComparer.Ordinal);
        var byStem = new Dictionary<string, List<string>>(StringComparer.Ordinal);
        foreach (var rel in programs)
        {
            exact[rel.ToLowerInvariant()] = rel;
            var basename = BaseName(rel);
            Bucket(byBasename, basename.ToLowerInvariant()).Add(rel);
            Bucket(byStem, Stem(basename).ToLowerInvariant()).Add(rel);
        }

        var resolved = new List<string>();
        foreach (var raw in selectors.Split(','))
        {
            var selector = PyCompat.Strip(raw);
            if (selector.Length == 0) continue;
            var normalized = selector.Replace('\\', '/').TrimStart('.', '/');
            if (!exact.TryGetValue(normalized.ToLowerInvariant(), out var match))
            {
                var name = BaseName(normalized);
                match = Unique(byBasename, name.ToLowerInvariant(), selector, "basename", out var error)
                        ?? (error is null ? Unique(byStem, Stem(name).ToLowerInvariant(), selector, "stem", out error) : null);
                if (match is null)
                {
                    lines = [error ?? $"Program selector '{selector}' did not match any program."];
                    return false;
                }
            }
            if (!resolved.Contains(match)) resolved.Add(match);
        }

        if (resolved.Count == 0)
        {
            lines = ["Program filter did not resolve to any programs."];
            return false;
        }
        lines = resolved;
        return true;
    }

    // The scan cache is keyed by basename: one "program-duplicate\t<basename>\t<paths…>" line per clash.
    public static IEnumerable<string> ProgramDuplicates(IReadOnlyList<string> programs)
    {
        var paths = new Dictionary<string, List<string>>(StringComparer.Ordinal);
        foreach (var rel in programs) Bucket(paths, BaseName(rel).ToLowerInvariant()).Add(rel);
        return paths.Where(pair => pair.Value.Count > 1).OrderBy(pair => pair.Key, StringComparer.Ordinal)
            .Select(pair => string.Join('\t', new[] { "program-duplicate", pair.Key }.Concat(pair.Value)));
    }

    // Writes the missing-copybooks report for every estate under the staging base and returns the summary
    // lines. COPY resolves within the estate only, and synthesised placeholders do not count as present.
    public static List<string> WriteMissingCopybookReport(string stagingBase, string reportPath)
    {
        var referencedBy = new SortedDictionary<string, SortedSet<string>>(StringComparer.Ordinal);
        foreach (var estate in Directory.EnumerateFileSystemEntries(stagingBase).Select(Path.GetFileName).Order(StringComparer.Ordinal))
        {
            var stagingDir = Path.Join(stagingBase, estate);
            if (!Directory.Exists(stagingDir)) continue;
            var stubsPath = Path.Join(stagingDir, ".generated-stubs");
            var synthesised = Exists(stubsPath)
                ? NonBlankLines(stubsPath).Select(name => name.ToUpperInvariant()).ToHashSet(StringComparer.Ordinal)
                : [];
            var available = new HashSet<string>(StringComparer.Ordinal);
            var stagedSources = new List<(string Rel, string Path)>();
            foreach (var (root, files) in PyCompat.Walk(stagingDir, _ => true))
            {
                foreach (var name in files)
                {
                    if (Extension(name).Equals(".cpy", StringComparison.OrdinalIgnoreCase)) available.Add(Stem(name).ToUpperInvariant());
                    if (name.ToLowerInvariant() is var lower && (lower.EndsWith(".cbl", StringComparison.Ordinal)
                            || lower.EndsWith(".cob", StringComparison.Ordinal) || lower.EndsWith(".cpy", StringComparison.Ordinal)))
                    {
                        var path = Path.Join(root, name);
                        stagedSources.Add((Path.GetRelativePath(stagingDir, path).Replace(Path.DirectorySeparatorChar, '/'), path));
                    }
                }
            }
            available.ExceptWith(synthesised);

            foreach (var (rel, path) in stagedSources.OrderBy(s => s.Rel, StringComparer.Ordinal).ThenBy(s => s.Path, StringComparer.Ordinal))
            {
                List<string> lines;
                try { lines = ReadLines(path, PyCompat.Latin1); }
                catch (Exception error) when (error is PyError or IOException or UnauthorizedAccessException) { continue; }
                foreach (var line in lines)
                {
                    if (line.Length > 6 && line[6] == '*') continue;
                    if (MissingCopyDirective.Match(PyCompat.RStrip(line)) is { Success: true, Index: 0 } match
                        && match.Groups[1].Value.ToUpperInvariant() is var copybook && !available.Contains(copybook))
                    {
                        if (!referencedBy.TryGetValue(copybook, out var refs))
                            referencedBy[copybook] = refs = new SortedSet<string>(StringComparer.Ordinal);
                        refs.Add(rel);
                    }
                }
            }
        }

        if (referencedBy.Count == 0)
        {
            File.WriteAllText(reportPath, "");
            return [];
        }

        var report = new StringBuilder()
            .Append("# Missing copybooks referenced by staged sources\n")
            .Append("# These COPY targets were not found in source/. The parser will\n")
            .Append("# still attempt the file but may emit MISSING_COPYBOOK errors and\n")
            .Append("# produce reduced AST/data coverage for affected programs.\n\n");
        foreach (var (copybook, refs) in referencedBy) report.Append($"{copybook}\treferenced by: {string.Join(", ", refs)}\n");
        File.WriteAllText(reportPath, report.ToString(), PyCompat.Utf8);

        var summary = new List<string> { $"{referencedBy.Count}|{referencedBy.Values.SelectMany(r => r).Distinct().Count()}" };
        summary.AddRange(referencedBy.Select(pair => $"  {pair.Key} ← {string.Join(", ", pair.Value)}"));
        return summary;
    }

    // The placeholder copybooks this program actually COPYs, as "generated-copybook-stub:<NAME>|…".
    public static string StubWarnings(string sourcePath, string stubsPath)
    {
        var stubs = NonBlankLines(stubsPath).Select(name => name.ToUpperInvariant()).ToHashSet(StringComparer.Ordinal);
        var used = new SortedSet<string>(StringComparer.Ordinal);
        foreach (var line in ReadLines(sourcePath, PyCompat.Latin1))
        {
            if (PyCompat.LStrip(line).StartsWith("*>", StringComparison.Ordinal) || line.Length > 6 && line[6] is '*' or '/') continue;
            if (CopyDirective.Match(line) is { Success: true } match && match.Groups[1].Value.ToUpperInvariant() is var name && stubs.Contains(name))
                used.Add(name);
        }
        return string.Join('|', used.Select(name => $"generated-copybook-stub:{name}"));
    }

    // unresolvedDiagnostics of a dependency-evidence.json, '|'-joined; the shell splits on '|' and on lines.
    public static string DependencyWarnings(string evidencePath)
    {
        var evidence = ReadJson(evidencePath) as JsonObject
                       ?? throw new InvalidDataException("the dependency evidence is not an object");
        if (!evidence.ContainsKey("unresolvedDiagnostics")) throw PyError.Key("unresolvedDiagnostics");
        return string.Join('|', Elements(evidence, "unresolvedDiagnostics").Select(item => PyJson.PyStr(item).Replace("|", "/").Replace("\n", " ")));
    }

    // Pass-1 results whose evidence asks for the COMMAREA closure or used an older one, as "<result>\t<program>".
    // A malformed result stops the listing, as a crash did before, while unreadable evidence is skipped.
    public static IEnumerable<string> CommareaPending(string resultsDir, string outputRoot)
    {
        if (!Directory.Exists(resultsDir)) yield break;
        var results = resultsDir.Length > 1 ? resultsDir.TrimEnd('/') : resultsDir;
        foreach (var name in Directory.EnumerateFileSystemEntries(resultsDir).Select(Path.GetFileName)
                     .Where(name => name!.EndsWith(".status", StringComparison.Ordinal)).Order(StringComparer.Ordinal))
        {
            var status = Path.Join(results, name);
            var fields = PyCompat.ReadText(status).TrimEnd('\n').Split('\t');
            if (fields.Length < 2) throw new InvalidDataException($"{status} names no program");
            var program = fields[1];
            JsonNode? evidence;
            try { evidence = ReadJson(Path.Join(outputRoot, program + ".report", "dependency-evidence.json")); }
            catch (Exception error) when (error is PyError or IOException or UnauthorizedAccessException or System.Text.Json.JsonException) { continue; }
            if (evidence is not JsonObject record) throw new InvalidDataException($"the dependency evidence of {program} is not an object");

            var closure = Path.Join(outputRoot, program.Split('/')[0], ".commarea-closure.json");
            var pending = Elements(record, "unresolvedDiagnostics").Select(PyJson.PyStr)
                .Any(item => (item.StartsWith("native:", StringComparison.Ordinal) ? item[7..] : item)
                    .StartsWith("cics-commarea-closure-", StringComparison.Ordinal));
            var stale = Stale(record, closure);
            if (pending || stale) yield return $"{status[..^".status".Length]}\t{program}";
        }
    }

    private static bool Stale(JsonObject record, string closure)
    {
        string? digest = null;
        foreach (var definition in Elements(record, "definitions"))
        {
            if (definition is not JsonObject item) throw new InvalidDataException("a dependency definition is not an object");
            if (PyJson.Str(item["kind"]) != "COMMAREA_CLOSURE") continue;
            if (!File.Exists(closure)) return true;
            digest ??= PyCompat.Digest(closure);
            if (PyJson.Str(item["sha256"]) != digest) return true;
        }
        return false;
    }

    // json.load: strict UTF-8 that keeps a BOM, which the JSON parser then rejects.
    private static JsonNode? ReadJson(string path) => PyJson.Parse(PyCompat.Decode(PyCompat.ReadBytes(path)));

    // Iterating record.get(key, []) in Python: list items, object keys or the characters of a string.
    private static IEnumerable<JsonNode?> Elements(JsonObject record, string key) =>
        !record.TryGetPropertyValue(key, out var value) ? []
        : value switch
        {
            JsonArray array => array.ToList(),
            JsonObject obj => obj.Select(pair => (JsonNode?)JsonValue.Create(pair.Key)).ToList(),
            _ when PyJson.Str(value) is { } text => text.Select(character => (JsonNode?)JsonValue.Create(character.ToString())).ToList(),
            _ => throw new InvalidDataException($"{key} is not iterable: {PyJson.Repr(value)}"),
        };

    // os.path.exists: a dangling symlink does not exist.
    private static bool Exists(string path)
    {
        FileSystemInfo info = new FileInfo(path);
        if (info.LinkTarget is null) return File.Exists(path) || Directory.Exists(path);
        var target = info.ResolveLinkTarget(returnFinalTarget: true);
        return target is not null && (File.Exists(target.FullName) || Directory.Exists(target.FullName));
    }

    private static string BaseName(string path) => path[(path.LastIndexOf('/') + 1)..];

    // os.path.splitext: leading dots belong to the name, and a trailing dot is an (empty) extension.
    private static int ExtensionStart(string name)
    {
        var dot = name.LastIndexOf('.');
        return dot > 0 && name[..dot].Any(character => character != '.') ? dot : -1;
    }

    private static string Stem(string name) => ExtensionStart(name) is var dot and >= 0 ? name[..dot] : name;

    private static string Extension(string name) => ExtensionStart(name) is var dot and >= 0 ? name[dot..] : "";

    private static string? Unique(Dictionary<string, List<string>> index, string key, string selector, string kind, out string? error)
    {
        error = null;
        if (!index.TryGetValue(key, out var matches)) return null;
        if (matches.Count == 1) return matches[0];
        error = $"Program selector '{selector}' matched multiple programs by {kind}: "
                + $"{string.Join(", ", matches.Order(StringComparer.Ordinal))}. Use a source-relative path.";
        return null;
    }

    private static TValue Bucket<TValue>(IDictionary<string, TValue> index, string key) where TValue : new()
    {
        if (!index.TryGetValue(key, out var bucket)) index[key] = bucket = new TValue();
        return bucket;
    }
}
