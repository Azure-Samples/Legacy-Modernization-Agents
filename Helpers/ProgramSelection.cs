using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Helpers;

/// <summary>
/// Narrows a scanned estate to the programs a caller asked for.
///
/// Converting one program at a time is the normal way to work on a large estate: a full run costs
/// hours, and a change to a prompt or a model is judged on one program long before it is trusted on
/// sixty. The portal's program view exists to choose that program; this is what makes the choice
/// mean something to the conversion.
///
/// Copybooks are not filtered out from under a program. They are not converted in their own right,
/// but a program without its copybooks loses the record layouts its data division is built from,
/// and the conversion would invent them instead. Only the copybooks a chosen program actually
/// reaches are kept, following the COPY graph so that a copybook copied by another still travels.
/// </summary>
public static class ProgramSelection
{
    /// <summary>
    /// Returns the files to convert, given a selection of program identities. An identity may be a
    /// basename, a basename without its extension, or a source-relative path.
    /// </summary>
    /// <param name="unmatched">
    /// Identities that matched nothing. A selection is a deliberate instruction, so a name that
    /// matches nothing is reported rather than passed over: silently converting fewer programs
    /// than asked for looks identical to a successful run.
    /// </param>
    public static List<CobolFile> Apply(
        IReadOnlyList<CobolFile> files,
        IEnumerable<string>? selection,
        out IReadOnlyList<string> unmatched)
    {
        var wanted = (selection ?? Enumerable.Empty<string>())
            .Select(s => s?.Trim())
            .Where(s => !string.IsNullOrWhiteSpace(s))
            .Select(s => s!)
            .ToList();

        if (wanted.Count == 0)
        {
            unmatched = Array.Empty<string>();
            return files.ToList();
        }

        var missing = new List<string>();
        var keep = new List<CobolFile>();
        var chosen = new HashSet<string>(StringComparer.OrdinalIgnoreCase);

        foreach (var identity in wanted)
        {
            var matches = files.Where(f => !f.IsCopybook && Matches(f, identity)).ToList();

            if (matches.Count == 0)
            {
                missing.Add(identity);
                continue;
            }

            foreach (var match in matches)
            {
                if (chosen.Add(match.FilePath ?? match.FileName))
                    keep.Add(match);
            }
        }

        // Only the copybooks the chosen programs actually reach travel with them. Retaining every
        // copybook in the estate was safe but made converting five programs analyse the copybooks
        // of every unrelated application alongside them — on a two-estate source drop that is most
        // of the work for none of the benefit. Following the COPY graph keeps the case the blanket
        // rule existed for, a copybook that copies another, without carrying the rest.
        keep.AddRange(ReachableCopybooks(files, keep));

        unmatched = missing;
        return keep;
    }

    /// <summary>
    /// The copybooks the given programs reach, directly or through another copybook.
    /// </summary>
    /// <remarks>
    /// A COPY target with no file behind it is simply absent from the result; that is a missing
    /// copybook, which the scan reports separately and which no amount of retention would fix.
    /// </remarks>
    private static List<CobolFile> ReachableCopybooks(
        IReadOnlyList<CobolFile> files, IReadOnlyList<CobolFile> roots)
    {
        var copybooksByStem = new Dictionary<string, List<CobolFile>>(StringComparer.OrdinalIgnoreCase);
        foreach (var file in files.Where(f => f.IsCopybook))
        {
            var stem = Path.GetFileNameWithoutExtension(Path.GetFileName(file.FileName));
            if (!copybooksByStem.TryGetValue(stem, out var bucket))
                copybooksByStem[stem] = bucket = new List<CobolFile>();
            bucket.Add(file);
        }

        var reached = new List<CobolFile>();
        var seenStems = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        var pending = new Queue<string>();

        foreach (var root in roots)
            foreach (var name in CopyTargets(root.Content))
                pending.Enqueue(name);

        while (pending.Count > 0)
        {
            var stem = pending.Dequeue();
            if (!seenStems.Add(stem)) continue;
            if (!copybooksByStem.TryGetValue(stem, out var matches)) continue;

            // A stem shared by two files is ambiguous, so both are kept: dropping one would
            // silently pick a layout on the caller's behalf.
            foreach (var copybook in matches)
            {
                reached.Add(copybook);
                foreach (var name in CopyTargets(copybook.Content))
                    pending.Enqueue(name);
            }
        }

        return reached;
    }

    private static readonly System.Text.RegularExpressions.Regex CopyDirective = new(
        @"\bCOPY\s+[""']?([A-Za-z][A-Za-z0-9_-]*)",
        System.Text.RegularExpressions.RegexOptions.IgnoreCase
        | System.Text.RegularExpressions.RegexOptions.Compiled);

    /// <summary>
    /// The copybook names a source file COPYs, ignoring commented-out lines.
    /// </summary>
    /// <remarks>
    /// A comment in fixed-format COBOL is an asterisk or slash in column 7, so the indicator is
    /// tested by position. Folding that into the pattern instead invites a match that backtracks
    /// around the asterisk and follows a COPY that was deliberately commented out.
    /// </remarks>
    private static IEnumerable<string> CopyTargets(string? content)
    {
        if (string.IsNullOrEmpty(content)) yield break;

        foreach (var line in content.Split('\n'))
        {
            if (line.Length > 6 && (line[6] == '*' || line[6] == '/')) continue;
            if (line.TrimStart().StartsWith("*", StringComparison.Ordinal)) continue;

            var match = CopyDirective.Match(line);
            if (match.Success) yield return match.Groups[1].Value;
        }
    }

    private static bool Matches(CobolFile file, string identity)
    {
        var normalizedIdentity = SourcePathHelper.NormalizeRelativePath(identity);
        var basename = Path.GetFileName(file.FileName);
        var stem = Path.GetFileNameWithoutExtension(basename);

        if (string.Equals(basename, identity, StringComparison.OrdinalIgnoreCase)) return true;
        if (string.Equals(stem, identity, StringComparison.OrdinalIgnoreCase)) return true;

        if (string.IsNullOrEmpty(file.FilePath)) return false;

        var normalizedPath = SourcePathHelper.NormalizeRelativePath(file.FilePath);
        return normalizedPath.EndsWith("/" + normalizedIdentity, StringComparison.OrdinalIgnoreCase)
               || string.Equals(normalizedPath, normalizedIdentity, StringComparison.OrdinalIgnoreCase);
    }
}
