using System.Text.RegularExpressions;
using CobolToQuarkusMigration.Helpers;

namespace CobolToQuarkusMigration.Agents.Infrastructure.RektCache;

public sealed class RektCopybookGraph
{
    // Accept quoted COPY names; fixed-format comment lines are filtered separately.
    private static readonly Regex CopyDirectiveRegex = new(
        @"\bCOPY\s+['""]?([A-Za-z][A-Za-z0-9_\-]*)['""]?",
        RegexOptions.IgnoreCase | RegexOptions.Compiled);

    // basename → set of direct copybook basenames referenced (upper-cased, no extension).
    private readonly Dictionary<string, HashSet<string>> _directDeps =
        new(StringComparer.OrdinalIgnoreCase);

    // basename → preprocessed-bytes hash (so we can build dependency snapshots).
    private readonly Dictionary<string, string> _hashesByBasename =
        new(StringComparer.OrdinalIgnoreCase);

    // Set of known copybook basenames (with extension) so we can resolve "COPY BOOK"
    // to "BOOK.cpy" / "BOOK.CPY" without scanning the filesystem.
    private readonly Dictionary<string, string> _copybookBasenamesByStem =
        new(StringComparer.OrdinalIgnoreCase);

    public void AddFile(string basename, string preprocessedContent, bool isCopybook)
    {
        var hash = CanonicalHasher.HashUtf8(preprocessedContent);
        _hashesByBasename[basename] = hash;

        if (isCopybook)
        {
            // Index by stem so "COPY BOOK" can resolve to "BOOK.cpy" regardless of case.
            var stem = Path.GetFileNameWithoutExtension(basename);
            _copybookBasenamesByStem[stem] = basename;
        }

        var deps = ExtractDirectDependencyStems(preprocessedContent);
        _directDeps[basename] = deps;
    }

    public string? GetHash(string basename) =>
        _hashesByBasename.TryGetValue(basename, out var h) ? h : null;

    public IReadOnlyDictionary<string, string> BuildDependencySnapshot(string basename)
    {
        var snapshot = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
        if (!_directDeps.TryGetValue(basename, out var direct)) return snapshot;

        var queue = new Queue<string>(direct);   // stems
        var seen = new HashSet<string>(direct, StringComparer.OrdinalIgnoreCase);

        while (queue.Count > 0)
        {
            var stem = queue.Dequeue();
            if (!_copybookBasenamesByStem.TryGetValue(stem, out var cpyBasename))
                continue;   // unknown copybook — see GetMissingCopybooks
            if (!_hashesByBasename.TryGetValue(cpyBasename, out var hash))
                continue;
            snapshot[cpyBasename] = hash;

            if (_directDeps.TryGetValue(cpyBasename, out var nested))
            {
                foreach (var n in nested)
                {
                    if (seen.Add(n)) queue.Enqueue(n);
                }
            }
        }
        return snapshot;
    }

    public IReadOnlySet<string> GetMissingCopybooks(string basename)
    {
        var missing = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        if (!_directDeps.TryGetValue(basename, out var direct)) return missing;

        var queue = new Queue<string>(direct);
        var seen = new HashSet<string>(direct, StringComparer.OrdinalIgnoreCase);
        while (queue.Count > 0)
        {
            var stem = queue.Dequeue();
            if (!_copybookBasenamesByStem.TryGetValue(stem, out var cpyBasename))
            {
                missing.Add(stem);
                continue;
            }
            if (_directDeps.TryGetValue(cpyBasename, out var nested))
            {
                foreach (var n in nested)
                {
                    if (seen.Add(n)) queue.Enqueue(n);
                }
            }
        }
        return missing;
    }

    private static HashSet<string> ExtractDirectDependencyStems(string content)
    {
        var result = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        foreach (var line in content.Split('\n'))
        {
            if (IsCommentLine(line)) continue;
            foreach (Match m in CopyDirectiveRegex.Matches(line))
            {
                var name = m.Groups[1].Value.Trim();
                if (name.Length > 0) result.Add(name);
            }
        }
        return result;
    }

    private static bool IsCommentLine(string line)
    {
        if (line.Length < 7) return false;
        return line[6] == '*' || line[6] == '/';
    }
}
