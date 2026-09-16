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
/// Copybooks are never filtered out. They are not converted in their own right, but a program
/// without its copybooks loses the record layouts its data division is built from, and the
/// conversion would invent them instead.
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

        // Every copybook travels with the selection rather than only those a chosen program names:
        // the reference may be in a copybook that itself copies another, and a conversion missing
        // one silently invents the layout instead of failing.
        keep.AddRange(files.Where(f => f.IsCopybook));

        unmatched = missing;
        return keep;
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
