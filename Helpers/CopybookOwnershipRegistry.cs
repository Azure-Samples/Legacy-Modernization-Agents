// Says which converted file owns the type built from a copybook.
//
// The shared-copybook rule told every program "do NOT define these types, reference them from the
// shared namespace" and told no one to define them. Each program still needs the type to exist in
// order to compile, so each defined it. Seven files declared Bdsdatoi; the instruction was not
// disobeyed, it was unsatisfiable.
//
// A copybook in the source drop is converted in its own right — BDSDATOI.cpy becomes Bdsdatoi.cs —
// so an owner already exists and only has to be named. Everything that copies it references it.
// This is the same correction the call-target contracts needed: an instruction to reference is
// only coherent alongside an instruction to declare.

namespace CobolToQuarkusMigration.Helpers;

using System.Text;
using System.Text.RegularExpressions;

public sealed record CopybookOwnership(
    string Copybook,
    string TypeName,
    string OwnedBy,
    IReadOnlyList<string> UsedBy)
{
    public bool IsOwnedBy(string stem) =>
        string.Equals(OwnedBy, stem, StringComparison.OrdinalIgnoreCase);
}

public sealed class CopybookOwnershipRegistry
{
    private static readonly Regex CopyDirective = new(
        @"\bCOPY\s+[""']?([A-Za-z][A-Za-z0-9_-]*)",
        RegexOptions.IgnoreCase | RegexOptions.Compiled);

    private readonly Dictionary<string, CopybookOwnership> _byCopybook =
        new(StringComparer.OrdinalIgnoreCase);

    public IReadOnlyCollection<CopybookOwnership> Ownerships => _byCopybook.Values;

    public static CopybookOwnershipRegistry Build(string sourceFolder)
    {
        var registry = new CopybookOwnershipRegistry();
        if (!Directory.Exists(sourceFolder)) return registry;

        var copybooks = Read(SourceTypeRegistry.EnumerateCopybookFiles(sourceFolder));
        var programs = Read(SourceTypeRegistry.EnumerateProgramFiles(sourceFolder));

        // A copybook is copied by programs and by other copybooks alike, and both produce a file
        // that would otherwise declare the type again.
        var everything = copybooks.Concat(programs)
            .ToDictionary(e => e.Key, e => e.Value, StringComparer.OrdinalIgnoreCase);

        var users = everything
            .SelectMany(file => CopyDirective
                .Matches(StripComments(file.Value))
                .Select(match => (Copybook: match.Groups[1].Value, User: file.Key)))
            .Where(edge => !string.Equals(edge.Copybook, edge.User, StringComparison.OrdinalIgnoreCase))
            .GroupBy(edge => edge.Copybook, StringComparer.OrdinalIgnoreCase)
            .ToDictionary(
                group => group.Key,
                group => new SortedSet<string>(group.Select(e => e.User), StringComparer.OrdinalIgnoreCase),
                StringComparer.OrdinalIgnoreCase);

        foreach (var (copybook, usedBy) in users)
        {
            // Without a file of its own there is no owner to name, and naming an arbitrary user
            // would put a layout nobody can see into a type everybody depends on. Those stay as
            // they are, and the missing-copybook reporting already accounts for them.
            if (!copybooks.ContainsKey(copybook)) continue;

            registry._byCopybook[copybook] = new CopybookOwnership(
                Copybook: copybook,
                TypeName: ToPascalCase(copybook),
                OwnedBy: copybook,
                UsedBy: usedBy.ToList());
        }

        return registry;
    }

    /// <summary>
    /// What the file being converted owns, and what it must reference instead of declaring.
    /// </summary>
    public string ToPromptBlock(string stem, string targetLanguage)
    {
        var owned = _byCopybook.Values
            .Where(o => o.IsOwnedBy(stem))
            .OrderBy(o => o.TypeName, StringComparer.OrdinalIgnoreCase)
            .ToList();

        var referenced = _byCopybook.Values
            .Where(o => !o.IsOwnedBy(stem) && o.UsedBy.Contains(stem, StringComparer.OrdinalIgnoreCase))
            .OrderBy(o => o.TypeName, StringComparer.OrdinalIgnoreCase)
            .ToList();

        if (owned.Count == 0 && referenced.Count == 0) return string.Empty;

        var declares = new StringBuilder();
        foreach (var o in owned)
        {
            declares.AppendLine(
                $"  • {o.TypeName} — from {o.Copybook}, used by {o.UsedBy.Count} other file(s). "
                + "You are the only file that declares it.");
        }

        var references = new StringBuilder();
        foreach (var o in referenced)
        {
            references.AppendLine($"  • {o.TypeName} — declared by the conversion of {o.Copybook}.");
        }

        return Environment.NewLine + PromptLoader.LoadSectionValidated(
            "RektContext", "CopybookOwnership", new Dictionary<string, string>
            {
                ["SharedNamespace"] = ConversionNamespacePolicy.ForSharedTypes(targetLanguage),
                ["Declares"] = declares.Length == 0 ? "  (none)" : declares.ToString().TrimEnd(),
                ["References"] = references.Length == 0 ? "  (none)" : references.ToString().TrimEnd(),
            });
    }

    private static Dictionary<string, string> Read(IEnumerable<string> paths)
    {
        var map = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
        foreach (var path in paths)
        {
            try
            {
                map[Path.GetFileNameWithoutExtension(Path.GetFileName(path))] = File.ReadAllText(path);
            }
            catch (IOException)
            {
                // A file that cannot be read states no COPY edges. Skipping it can only lose an
                // ownership record, never invent one.
            }
        }
        return map;
    }

    private static string StripComments(string text) => string.Join('\n',
        text.Split('\n').Where(line =>
            !(line.Length > 6 && (line[6] == '*' || line[6] == '/'))
            && !line.TrimStart().StartsWith("*", StringComparison.Ordinal)));

    private static string ToPascalCase(string name)
    {
        var parts = name.Split(['-', '_'], StringSplitOptions.RemoveEmptyEntries);
        var sb = new StringBuilder();
        foreach (var part in parts)
        {
            sb.Append(char.ToUpperInvariant(part[0]));
            if (part.Length > 1) sb.Append(part[1..].ToLowerInvariant());
        }
        return sb.Length == 0 ? name : sb.ToString();
    }
}

public static class CopybookOwnershipRegistryHolder
{
    private static readonly object Lock = new();
    private static readonly Dictionary<string, CopybookOwnershipRegistry> Cache =
        new(StringComparer.OrdinalIgnoreCase);

    public static CopybookOwnershipRegistry GetOrBuild(string repoRoot, string sourceFolder)
    {
        var key = Path.IsPathRooted(sourceFolder) ? sourceFolder : Path.Join(repoRoot, sourceFolder);
        lock (Lock)
        {
            if (Cache.TryGetValue(key, out var existing)) return existing;
            return Cache[key] = CopybookOwnershipRegistry.Build(key);
        }
    }
}
