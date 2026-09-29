// Turns compiler errors into one repair task per file. Most of what a repair needs is not in the
// error: which file keeps a type declared twice, and which file declares a type nobody declared.
// Both are decided here, deterministically, so repairs of different files cannot contradict each
// other.

using System.Text;
using System.Text.RegularExpressions;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Helpers;

public sealed record CompileRepairTask(
    string File,
    IReadOnlyList<CompilerDiagnostic> Errors,
    IReadOnlyList<string> Instructions,
    IReadOnlyList<string> Declarations,
    IReadOnlySet<string> MayRemove);

public static class CompileRepairPlanner
{
    public const string ExternalContractMarker = "EXTERNAL CONTRACT";

    private static readonly Regex Duplicate = new(
        @"^The namespace '(?<ns>[^']*)' already contains a definition for '(?<name>[^']+)'", RegexOptions.Compiled);

    private static readonly Regex NotFound = new(
        @"^The type or namespace name '(?<name>[A-Za-z_][A-Za-z0-9_]*)(?:<[^']*>)?' could not be found", RegexOptions.Compiled);

    private static readonly Regex Conversion = new(
        @"cannot (?:implicitly )?convert (?:from|type) '(?<from>[A-Za-z_][A-Za-z0-9_.]*)' to '(?<to>[A-Za-z_][A-Za-z0-9_.]*)'",
        RegexOptions.Compiled | RegexOptions.IgnoreCase);

    private static readonly Regex Quoted = new(@"'(?<name>[A-Za-z_][A-Za-z0-9_]*)", RegexOptions.Compiled);

    private static readonly Regex TopLevelUsing = new(@"^\s*using\s+[A-Za-z_][A-Za-z0-9_.]*\s*;", RegexOptions.Compiled);

    /// <summary>
    /// Types a file cannot see only because their one namespace is not imported get the import, without a
    /// model: it is the whole fix, and a model given a large file to change one line may change more.
    /// </summary>
    public static (IReadOnlyDictionary<string, string> Fixed, IReadOnlyList<CompilerDiagnostic> Remaining) ImportUniqueNamespaces(
        IReadOnlyList<CompilerDiagnostic> errors,
        GeneratedTypeIndex index,
        IReadOnlyDictionary<string, string> sources)
    {
        var fixedSources = new Dictionary<string, string>(StringComparer.Ordinal);
        var remaining = new List<CompilerDiagnostic>();

        foreach (var error in errors)
        {
            var match = error.Code == "CS0246" ? NotFound.Match(error.Message) : Match.Empty;
            var namespaces = match.Success
                ? index.Find(match.Groups["name"].Value).Select(d => d.Namespace).Where(n => n.Length > 0).Distinct().ToList()
                : [];
            var source = fixedSources.TryGetValue(error.File, out var f) ? f : sources.GetValueOrDefault(error.File);
            if (namespaces.Count != 1 || source is null) { remaining.Add(error); continue; }

            var ns = namespaces[0];
            var lines = source.Split('\n').ToList();
            if (!lines.Any(l => l.Trim() == $"using {ns};"))
            {
                var firstNamespace = lines.FindIndex(l => l.TrimStart().StartsWith("namespace ", StringComparison.Ordinal));
                var searchEnd = firstNamespace < 0 ? lines.Count : firstNamespace;
                var lastUsing = lines.Take(searchEnd).ToList().FindLastIndex(l => TopLevelUsing.IsMatch(l));
                lines.Insert(lastUsing + 1, $"using {ns};");
                fixedSources[error.File] = string.Join('\n', lines);
            }
        }

        return (fixedSources, remaining);
    }

    public static IReadOnlyList<CompileRepairTask> Plan(
        IReadOnlyList<CompilerDiagnostic> errors,
        GeneratedTypeIndex index,
        IReadOnlyDictionary<string, string> sources,
        string sharedNamespace,
        CompileGateSettings limits,
        IReadOnlyDictionary<string, string>? cobolByName = null)
    {
        var cobol = (cobolByName ?? new Dictionary<string, string>())
            .GroupBy(c => Key(c.Key))
            .ToDictionary(g => g.Key, g => g.First().Value, StringComparer.Ordinal);
        var work = new SortedDictionary<string, Builder>(StringComparer.Ordinal);
        Builder For(string file) => work.TryGetValue(file, out var b) ? b : work[file] = new Builder(limits);

        foreach (var error in errors) For(error.File).Errors.Add(error);

        // Declared more than once in one namespace: every file but the owner removes its copy.
        foreach (var group in errors
                     .Select(e => (Error: e, Match: Duplicate.Match(e.Message)))
                     .Where(x => x.Match.Success)
                     .GroupBy(x => (Ns: x.Match.Groups["ns"].Value, Name: x.Match.Groups["name"].Value)))
        {
            var declarers = index.Find(group.Key.Name)
                .Where(d => d.Namespace == group.Key.Ns)
                .Select(d => d.File)
                .Distinct()
                .ToList();
            if (declarers.Count < 2) continue;

            var owner = GeneratedTypeIndex.ChooseOwner(group.Key.Name, declarers);

            // The owner keeps its declaration, so the error gives it nothing to do; left in, a repair
            // would read it as a request to delete the copy that is meant to stay.
            if (work.TryGetValue(owner, out var kept))
                kept.Errors.RemoveAll(e => group.Any(x => ReferenceEquals(x.Error, e)));

            foreach (var file in declarers.Where(f => f != owner))
            {
                var task = For(file);
                task.MayRemove.Add(group.Key.Name);
                task.Instructions.Add(
                    $"`{group.Key.Ns}.{group.Key.Name}` is also declared in {owner}, which keeps it. Delete this file's " +
                    $"declaration of `{group.Key.Name}` and use that one. Where this file relies on members it does " +
                    "not have, change this file's code to use the members it does have; do not add a second type.");
                task.DeclarationOf(group.Key.Name, owner, sources);
            }
        }

        // One type declared under the same name in two namespaces: the erroring file converts a copy it
        // declared itself, so it removes that copy and uses the declaration the other side expects.
        foreach (var error in errors.Where(e => e.Code is "CS1503" or "CS0029" or "CS0266"))
        {
            var match = Conversion.Match(error.Message);
            if (!match.Success) continue;
            var (from, to) = (Split(match.Groups["from"].Value), Split(match.Groups["to"].Value));
            if (from.Name != to.Name || from.Ns == to.Ns) continue;

            var declarations = index.Find(from.Name);
            var local = declarations.FirstOrDefault(d => d.File == error.File && (d.Namespace == from.Ns || d.Namespace == to.Ns));
            if (local is null) continue;
            var otherNs = local.Namespace == from.Ns ? to.Ns : from.Ns;
            var other = declarations.FirstOrDefault(d => d.File != error.File && d.Namespace == otherNs);
            if (other is null) continue;

            var task = For(error.File);
            if (!task.MayRemove.Add(from.Name)) continue;
            task.Instructions.Add(
                $"`{local.Namespace}.{from.Name}` in this file is a copy of `{otherNs}.{from.Name}` declared in {other.File}. " +
                $"Delete this file's declaration of `{from.Name}` and use that one (add `using {otherNs};` if missing). " +
                "Where this file relies on members it does not have, change this file's code to use the members it does have.");
            task.DeclarationOf(from.Name, other.File, sources);
        }

        // Referenced but not found: either declared somewhere this file cannot see, or nowhere.
        var missing = errors
            .Select(e => (Error: e, Match: NotFound.Match(e.Message)))
            .Where(x => x.Match.Success && x.Error.Code is "CS0246" or "CS0234")
            .GroupBy(x => x.Match.Groups["name"].Value);

        foreach (var group in missing)
        {
            var name = group.Key;
            var referencing = group.Select(x => x.Error.File).Distinct().ToList();
            var declared = index.Find(name);

            if (declared.Count > 0)
            {
                var where = string.Join(", ", declared.Select(d => $"`{d.Namespace}` ({d.File})").Distinct());
                foreach (var file in referencing)
                {
                    For(file).Instructions.Add(
                        $"`{name}` exists: it is declared in {where}. Reference it there (a `using` or a qualified " +
                        "name); do not declare it here.");
                    For(file).DeclarationOf(name, declared[0].File, sources);
                }

                continue;
            }

            var uses = UsesOf(name, referencing, sources, limits.MaxUsesShown);
            string owner;
            var implementer = FindImplementer(name, sources);
            if (implementer is { } impl)
            {
                // A converted class lists it as a base type, so its contract is that class's own
                // public surface and belongs beside it rather than being guessed from call sites.
                owner = impl.File;
                For(owner).Instructions.Add(
                    $"`{name}` is implemented by `{impl.Type}` in this file, but no file declares it. Declare it once, " +
                    $"in namespace `{sharedNamespace}` (a separate block namespace in this file), with exactly the " +
                    $"public members of `{impl.Type}` that the uses below need. Do not change `{impl.Type}`." +
                    Environment.NewLine + uses);
            }
            else if (cobol.TryGetValue(Key(name), out var layout))
            {
                // The COBOL was converted, but its own type was left out. It is declared from the
                // layout, preferably by the file that converted it.
                owner = sources.Keys
                            .Where(f => Key(Path.GetFileNameWithoutExtension(f)) == Key(name))
                            .OrderBy(f => f, StringComparer.Ordinal)
                            .FirstOrDefault()
                        ?? GeneratedTypeIndex.ChooseOwner(name, referencing);
                For(owner).Instructions.Add(
                    $"`{name}` is the type of COBOL member {name}, which was converted, but no file declares it. " +
                    $"Declare it once, in namespace `{sharedNamespace}` (a separate block namespace in this file), " +
                    "with one property per data item of the layout below, so that every use below compiles. " +
                    "Follow the layout; do not invent fields it does not have." +
                    Environment.NewLine + "```cobol" + Environment.NewLine + layout.TrimEnd() +
                    Environment.NewLine + "```" + Environment.NewLine + uses);
            }
            else
            {
                owner = GeneratedTypeIndex.ChooseOwner(name, referencing);
                For(owner).Instructions.Add(
                    $"`{name}` is declared nowhere in this conversion: the COBOL it comes from was not part of it. " +
                    $"Declare it once, in namespace `{sharedNamespace}` (a separate block namespace in this file), as the " +
                    "smallest type that satisfies every use below, and put this comment directly above it: " +
                    $"`// {ExternalContractMarker}: {name} was not produced by this conversion; declared from its uses.`" +
                    Environment.NewLine + uses);
            }

            foreach (var file in referencing.Where(f => f != owner))
            {
                For(file).Instructions.Add(
                    $"`{name}` is declared by {owner} in namespace `{sharedNamespace}`. Reference it from there " +
                    "(add `using " + sharedNamespace + ";` if missing); do not declare it here.");
            }
        }

        // Anything else the errors name that another file declares (interfaces to implement, members
        // to call) is shown, so the repair works from the real declaration rather than a guess.
        foreach (var (file, task) in work)
        {
            foreach (var name in task.Errors
                         .SelectMany(e => Quoted.Matches(e.Message).Select(m => m.Groups["name"].Value))
                         .Distinct())
            {
                var elsewhere = index.Find(name).FirstOrDefault(d => d.File != file);
                if (elsewhere is not null) task.DeclarationOf(name, elsewhere.File, sources);
            }

            if (!sources.TryGetValue(file, out var text)) continue;
            foreach (var name in task.Errors.Where(e => ArgumentErrors.Contains(e.Code))
                         .SelectMany(e => ReceiverTypes(text, e.Line))
                         .Distinct())
            {
                var declared = index.Find(name).FirstOrDefault();
                if (declared is not null) task.DeclarationOf(name, declared.File, sources);
            }
        }

        return work
            .Where(w => sources.ContainsKey(w.Key) && (w.Value.Errors.Count > 0 || w.Value.Instructions.Count > 0))
            .Select(w => new CompileRepairTask(
                w.Key, w.Value.Errors, w.Value.Instructions.Distinct().ToList(),
                w.Value.Declarations.Values.ToList(), w.Value.MayRemove))
            .ToList();
    }

    // These name the arguments' types, never the method's owner, so the callee is found from the call itself.
    private static readonly HashSet<string> ArgumentErrors = new(StringComparer.Ordinal)
    {
        "CS1501", "CS1503", "CS1739", "CS7036",
    };

    private static readonly Regex MemberCall = new(@"\b(?<receiver>[A-Za-z_]\w*)\s*\.\s*\w+\s*\(", RegexOptions.Compiled);

    public static IEnumerable<string> ReceiverTypes(string text, int line)
    {
        var lines = text.Split('\n');
        if (line < 1 || line > lines.Length) yield break;
        foreach (Match call in MemberCall.Matches(lines[line - 1]))
        {
            var receiver = Regex.Escape(call.Groups["receiver"].Value);
            var declaration = Regex.Match(text, @"\b(?<type>[A-Z]\w*)(?:<[^>\n]*>)?\??\s+_?" + receiver + @"\s*[,;=)]");
            if (declaration.Success) yield return declaration.Groups["type"].Value;
        }
    }

    /// <summary>The first generated class, record or struct that lists <paramref name="name"/> as a base type.</summary>
    public static (string File, string Type)? FindImplementer(string name, IReadOnlyDictionary<string, string> sources)
    {
        var baseList = new Regex(
            $@"\b(?:class|record|struct)[ \t]+(?<type>[A-Za-z_][A-Za-z0-9_]*)[^{{;]*?:[^{{;]*?\b{Regex.Escape(name)}\b",
            RegexOptions.Multiline);
        foreach (var (file, text) in sources.OrderBy(s => s.Key, StringComparer.Ordinal))
        {
            var match = baseList.Match(text);
            if (match.Success) return (file, match.Groups["type"].Value);
        }

        return null;
    }

    // COBOL member names and the C# names derived from them differ only in case and separators.
    private static (string Ns, string Name) Split(string qualified)
    {
        var dot = qualified.LastIndexOf('.');
        return dot < 0 ? ("", qualified) : (qualified[..dot], qualified[(dot + 1)..]);
    }

    private static string Key(string name) =>
        new string(name.Where(char.IsLetterOrDigit).ToArray()).ToLowerInvariant();

    private static string UsesOf(string name, IEnumerable<string> files, IReadOnlyDictionary<string, string> sources, int maxUsesShown)
    {
        var word = new Regex($@"\b{Regex.Escape(name)}\b");
        var sb = new StringBuilder("Uses:");
        var shown = 0;
        foreach (var file in files.OrderBy(f => f, StringComparer.Ordinal))
        {
            if (!sources.TryGetValue(file, out var text)) continue;
            var lines = text.Split('\n');
            for (var i = 0; i < lines.Length && shown < maxUsesShown; i++)
            {
                if (!word.IsMatch(lines[i])) continue;
                sb.Append(Environment.NewLine).Append($"  {file}:{i + 1}: {lines[i].Trim()}");
                shown++;
            }
        }

        return sb.ToString();
    }

    /// <summary>
    /// The text of a type declaration, from its line to its matching closing brace, truncated to
    /// <paramref name="maxLines"/> lines when given.
    /// </summary>
    public static string? ExtractDeclaration(string source, string typeName, int? maxLines = null)
    {
        var match = Regex.Match(source,
            $@"^[^\r\n]*\b(?:class|record|struct|interface|enum)[ \t]+{Regex.Escape(typeName)}\b",
            RegexOptions.Multiline);
        if (!match.Success) return null;

        var depth = 0;
        var opened = false;
        for (var i = match.Index; i < source.Length; i++)
        {
            var c = source[i];
            if (c == ';' && !opened && depth == 0)
                return source[match.Index..(i + 1)]; // positional record without a body
            if (c == '{') { depth++; opened = true; }
            else if (c == '}' && --depth == 0 && opened)
            {
                var text = source[match.Index..(i + 1)];
                var lines = text.Split('\n');
                return maxLines is not { } max || lines.Length <= max
                    ? text
                    : string.Join('\n', lines.Take(max)) + "\n    // … truncated";
            }
        }

        return null;
    }

    private sealed class Builder(CompileGateSettings limits)
    {
        public List<CompilerDiagnostic> Errors { get; } = [];
        public List<string> Instructions { get; } = [];
        public SortedDictionary<string, string> Declarations { get; } = new(StringComparer.Ordinal);
        public HashSet<string> MayRemove { get; } = new(StringComparer.Ordinal);

        public void DeclarationOf(string name, string file, IReadOnlyDictionary<string, string> sources)
        {
            var key = $"{file}#{name}";
            if (Declarations.ContainsKey(key) || Declarations.Count >= limits.MaxDeclarationsPerRepair) return;
            if (!sources.TryGetValue(file, out var text)) return;
            var declaration = ExtractDeclaration(text, name, limits.MaxDeclarationLines);
            if (declaration is not null) Declarations[key] = $"// {file}{Environment.NewLine}{declaration}";
        }
    }
}
