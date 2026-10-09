using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Helpers;

/// <summary>
/// Rules for reusing business logic persisted by an earlier reverse-engineering run.
/// A report is only trusted for the same source folder, and only for files whose
/// content has not changed since the report was produced.
/// </summary>
public static class BusinessLogicReuse
{
    private static StringComparison PathComparison =>
        OperatingSystem.IsLinux() ? StringComparison.Ordinal : StringComparison.OrdinalIgnoreCase;

    public static string NormalizePath(string path) =>
        Path.TrimEndingDirectorySeparator(Path.GetFullPath(path));

    public static bool SamePath(string? a, string? b)
    {
        if (string.IsNullOrWhiteSpace(a) || string.IsNullOrWhiteSpace(b))
        {
            return false;
        }

        try
        {
            return string.Equals(NormalizePath(a), NormalizePath(b), PathComparison);
        }
        catch (Exception ex) when (ex is ArgumentException or NotSupportedException or PathTooLongException)
        {
            return false;
        }
    }

    /// <summary>
    /// Finds the entry for a source file. A path match wins; a bare file-name match is
    /// used only when it is unambiguous, so two same-named files in different folders
    /// never receive each other's business rules.
    /// </summary>
    public static BusinessLogic? Find(IEnumerable<BusinessLogic> extracts, string? fileName, string? filePath = null)
    {
        var list = extracts as IReadOnlyList<BusinessLogic> ?? extracts.ToList();
        if (list.Count == 0)
        {
            return null;
        }

        if (!string.IsNullOrWhiteSpace(filePath))
        {
            var byPath = list.FirstOrDefault(bl => SamePath(bl.FilePath, filePath));
            if (byPath is not null)
            {
                return byPath;
            }
        }

        if (string.IsNullOrWhiteSpace(fileName))
        {
            return null;
        }

        var name = Path.GetFileName(fileName);
        var byName = list
            .Where(bl => string.Equals(bl.FileName, name, StringComparison.OrdinalIgnoreCase))
            .Take(2)
            .ToList();
        return byName.Count == 1 ? byName[0] : null;
    }

    public sealed record FreshnessResult(
        List<BusinessLogic> Fresh,
        List<string> Changed,
        List<string> Missing);

    /// <summary>
    /// Keeps only entries whose source file still exists with the content captured by
    /// the reverse-engineering run. Entries without a snapshot are kept, since older
    /// runs did not always store file content.
    /// </summary>
    public static FreshnessResult FilterStale(
        IReadOnlyList<BusinessLogic> extracts,
        IReadOnlyList<CobolFile> snapshot,
        Func<string, string?> readCurrentContent)
    {
        var fresh = new List<BusinessLogic>();
        var changed = new List<string>();
        var missing = new List<string>();

        foreach (var bl in extracts)
        {
            var captured = snapshot.FirstOrDefault(f => SamePath(f.FilePath, bl.FilePath))
                ?? (snapshot.Count(f => string.Equals(f.FileName, bl.FileName, StringComparison.OrdinalIgnoreCase)) == 1
                    ? snapshot.First(f => string.Equals(f.FileName, bl.FileName, StringComparison.OrdinalIgnoreCase))
                    : null);

            var current = readCurrentContent(bl.FilePath);
            if (current is null)
            {
                missing.Add(bl.FileName);
                continue;
            }

            if (captured is not null && !string.IsNullOrEmpty(captured.Content) && !SameContent(captured.Content, current))
            {
                changed.Add(bl.FileName);
                continue;
            }

            fresh.Add(bl);
        }

        return new FreshnessResult(fresh, changed, missing);
    }

    private static bool SameContent(string a, string b) =>
        string.Equals(a.ReplaceLineEndings("\n"), b.ReplaceLineEndings("\n"), StringComparison.Ordinal);

    public static string? ReadFileOrNull(string path)
    {
        try
        {
            return File.Exists(path) ? File.ReadAllText(path) : null;
        }
        catch (IOException)
        {
            return null;
        }
        catch (UnauthorizedAccessException)
        {
            return null;
        }
    }
}
