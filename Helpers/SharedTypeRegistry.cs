// Identifies copybook types shared across programs to prevent duplicate generated types.

namespace CobolToQuarkusMigration.Helpers;

using System.Text;
using System.Text.RegularExpressions;

public sealed class SharedTypeRegistry
{
    private readonly Dictionary<string, HashSet<string>> _copybookReferences = new(StringComparer.OrdinalIgnoreCase);
    private readonly HashSet<string> _sharedNames = new(StringComparer.OrdinalIgnoreCase);

    public IReadOnlySet<string> SharedTypeNames => _sharedNames;
    public bool IsShared(string name) => _sharedNames.Contains(name);

    public void Scan(string sourceFolder)
    {
        if (!Directory.Exists(sourceFolder)) return;

        var rxCopy = new Regex(@"^[^*]{0,6}[^*\n].*?\bCOPY\s+['""]?([A-Z][A-Z0-9_-]*)['""]?",
            RegexOptions.IgnoreCase | RegexOptions.Multiline);

        foreach (var program in SourceTypeRegistry.EnumerateProgramFiles(sourceFolder))
        {
            string? text = null;
            try { text = File.ReadAllText(program); } catch { continue; }
            var progName = Path.GetRelativePath(sourceFolder, program)
                .Replace(Path.DirectorySeparatorChar, '/');

            foreach (Match m in rxCopy.Matches(text))
            {
                var cpyName = m.Groups[1].Value.ToUpperInvariant();
                if (!_copybookReferences.TryGetValue(cpyName, out var refs))
                    _copybookReferences[cpyName] = refs = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
                refs.Add(progName);
            }
        }

        // A copybook is "shared" when referenced by 2+ programs.
        foreach (var (cpy, refs) in _copybookReferences)
        {
            if (refs.Count >= 2)
            {
                _sharedNames.Add(cpy);
                // Also register the typical converted-name variants the LLM
                // is likely to emit (PascalCase, with/without service suffix).
                _sharedNames.Add(ToPascalCase(cpy));
                _sharedNames.Add(ToPascalCase(cpy) + "Service");
                _sharedNames.Add(ToPascalCase(cpy) + "Data");
                _sharedNames.Add(ToPascalCase(cpy) + "Area");
                _sharedNames.Add(ToPascalCase(cpy) + "Record");
                _sharedNames.Add(ToPascalCase(cpy) + "Dto");
            }
        }
    }

    public string ToPromptBlock(string targetLanguage)
    {
        if (_sharedNames.Count == 0) return string.Empty;

        var shared = _copybookReferences
            .Where(kv => kv.Value.Count >= 2)
            .OrderByDescending(kv => kv.Value.Count)
            .Take(40)
            .ToList();

        var items = new StringBuilder();
        foreach (var (cpy, refs) in shared)
        {
            items.AppendLine($"  • {cpy}  (used by {refs.Count} programs)  →  expected type: {ToPascalCase(cpy)}");
        }

        return Environment.NewLine + PromptLoader.LoadSectionValidated(
            "RektContext", "SharedTypes", new Dictionary<string, string>
            {
                ["TargetLanguage"] = targetLanguage,
                ["SharedTypes"] = items.ToString().TrimEnd()
            });
    }

    private static string ToPascalCase(string name)
    {
        if (string.IsNullOrEmpty(name)) return "";
        var parts = name.Split(new[] { '-', '_' }, StringSplitOptions.RemoveEmptyEntries);
        var sb = new StringBuilder();
        foreach (var p in parts)
        {
            if (p.Length == 0) continue;
            sb.Append(char.ToUpperInvariant(p[0]));
            if (p.Length > 1) sb.Append(p.Substring(1).ToLowerInvariant());
        }
        return sb.ToString();
    }
}

public static class SharedTypeRegistryHolder
{
    private static readonly object _lock = new();
    private static readonly Dictionary<string, SharedTypeRegistry> _cache = new(StringComparer.OrdinalIgnoreCase);

    public static SharedTypeRegistry GetOrBuild(string repoRoot, string sourceFolder)
    {
        var key = Path.Combine(repoRoot, sourceFolder);
        lock (_lock)
        {
            if (_cache.TryGetValue(key, out var existing)) return existing;
            var reg = new SharedTypeRegistry();
            reg.Scan(key);
            _cache[key] = reg;
            return reg;
        }
    }
}
