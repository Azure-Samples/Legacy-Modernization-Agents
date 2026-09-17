// Where a conversion writes, and where a reader looks afterwards.
//
// Every run wrote to output/java or output/csharp, so a second run silently overwrote the first.
// Two runs could not be compared, a demo could not be kept, and a partial re-run left the previous
// run's files beside the new ones with nothing to say which was which.
//
// Runs are therefore written to a folder named for when they happened. That only works if readers
// can still find "the current output" without being told the timestamp — the portal starts
// independently of any conversion — so the language folder stays the stable root and the newest
// run beneath it is resolved on read. Output produced before this change sits flat in that root
// and is still found.

namespace CobolToQuarkusMigration.Helpers;

using System.Globalization;

public static class ConversionOutputFolder
{
    /// <summary>Sorts lexicographically as well as chronologically, and reads as a date.</summary>
    public const string TimestampFormat = "yyyyMMdd-HHmmss";

    public static string NewRunName(DateTimeOffset when) =>
        when.ToString(TimestampFormat, CultureInfo.InvariantCulture);

    /// <summary>
    /// The run folder a reader should use, relative to the repository root.
    /// </summary>
    /// <param name="languageRoot">Relative language folder, e.g. <c>output/csharp</c>.</param>
    /// <param name="marker">
    /// A file that identifies a real run folder. Without it any stray directory — a package
    /// namespace, an editor's scratch folder — would be mistaken for the newest run.
    /// </param>
    public static string ResolveLatest(string repoRoot, string languageRoot, string marker)
    {
        var absoluteRoot = Path.Combine(repoRoot, languageRoot);
        if (!Directory.Exists(absoluteRoot)) return languageRoot;

        string[] candidates;
        try { candidates = Directory.GetDirectories(absoluteRoot); }
        catch (IOException) { return languageRoot; }

        var newest = candidates
            .Where(dir => LooksLikeRun(Path.GetFileName(dir)))
            .Where(dir => File.Exists(Path.Combine(dir, marker)))
            .OrderByDescending(dir => Path.GetFileName(dir), StringComparer.Ordinal)
            .FirstOrDefault();

        if (newest is not null)
            return Path.Combine(languageRoot, Path.GetFileName(newest));

        // Output written before runs were dated sits directly in the language folder.
        return languageRoot;
    }

    /// <summary>Every run folder under a language root, newest first.</summary>
    public static IReadOnlyList<string> RunsIn(string repoRoot, string languageRoot)
    {
        var absoluteRoot = Path.Combine(repoRoot, languageRoot);
        if (!Directory.Exists(absoluteRoot)) return Array.Empty<string>();

        try
        {
            return Directory.GetDirectories(absoluteRoot)
                .Select(Path.GetFileName)
                .Where(name => name is not null && LooksLikeRun(name))
                .Select(name => name!)
                .OrderByDescending(name => name, StringComparer.Ordinal)
                .ToList();
        }
        catch (IOException)
        {
            return Array.Empty<string>();
        }
    }

    /// <summary>
    /// Whether a directory name is a run stamp. Checked by shape rather than by parsing, so a
    /// generated package directory such as "Modernized" is never mistaken for a run.
    /// </summary>
    public static bool LooksLikeRun(string? name) =>
        name is { Length: 15 }
        && name[8] == '-'
        && name.Take(8).All(char.IsAsciiDigit)
        && name.Skip(9).All(char.IsAsciiDigit);
}
