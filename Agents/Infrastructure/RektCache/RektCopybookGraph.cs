using System.Text.RegularExpressions;
using CobolToQuarkusMigration.Helpers;

namespace CobolToQuarkusMigration.Agents.Infrastructure.RektCache;

/// <summary>
/// Computes the copybook dependency graph for a corpus of preprocessed COBOL
/// files. Operates on the byte content alone — no smojol, no disk traversal
/// beyond what the caller supplies, no implicit copybook search paths.
/// </summary>
/// <remarks>
/// <para>
/// Dependency extraction uses a single regex over each file's text content:
/// <c>COPY [&lt;name&gt;|'&lt;name&gt;'|"&lt;name&gt;"]</c> on non-comment lines (col 7 ≠ '*').
/// The match is case-insensitive; copybook names are upper-cased in the result.
/// </para>
/// <para>
/// Transitive closure is computed iteratively (BFS) over the in-memory map so
/// changes deep in a COPY chain still invalidate dependent programs.
/// </para>
/// <para>
/// COBOL <c>COPY REPLACING</c> is not parsed beyond extracting the copybook name.
/// We do not currently track which copybook variants are used; a change to any
/// copybook in the chain triggers re-parse, which is the safe over-approximation.
/// </para>
/// </remarks>
public sealed class RektCopybookGraph
{
    // Match COPY <name>. We allow optional quotes and a permissive name. Anchored
    // to allow whitespace at line start; column-7 comment lines are excluded by
    // the IsCommentLine pre-check below.
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

    /// <summary>
    /// Add a file's preprocessed content. <paramref name="basename"/> includes
    /// extension. Adds both the hash and the direct dependency list.
    /// </summary>
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

    /// <summary>Hash of the preprocessed bytes of <paramref name="basename"/>, or null if unknown.</summary>
    public string? GetHash(string basename) =>
        _hashesByBasename.TryGetValue(basename, out var h) ? h : null;

    /// <summary>
    /// Returns the transitive copybook dependency snapshot for <paramref name="basename"/>:
    /// every copybook (direct or indirect) it COPYs, mapped to that copybook's current
    /// preprocessed hash. Resolves COPY-by-stem to known copybook basenames; unknown
    /// stems are surfaced via <see cref="GetMissingCopybooks"/> rather than included.
    /// </summary>
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

    /// <summary>
    /// Returns the set of copybook stems referenced (directly or transitively) by
    /// <paramref name="basename"/> that are not present in the corpus. Used by the
    /// planner to surface a stable "missing-copybook" warning per program.
    /// </summary>
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

    /// <summary>Extracts the direct COPY targets (as upper-cased stems) from preprocessed COBOL text.</summary>
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

    /// <summary>Fixed-format COBOL: column 7 (1-based) holds an asterisk to mark a comment line.</summary>
    private static bool IsCommentLine(string line)
    {
        if (line.Length < 7) return false;
        return line[6] == '*' || line[6] == '/';
    }
}
