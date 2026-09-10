namespace CobolToQuarkusMigration.Helpers;

public enum SourceKind
{
    Unknown = 0,
    CobolProgram,
    Copybook,
}

public static class SourceTypeRegistry
{
    // Stored lower-case; comparisons are case-insensitive.
    private static readonly HashSet<string> ProgramExtensions =
        new(StringComparer.OrdinalIgnoreCase) { ".cbl", ".cob" };

    private static readonly HashSet<string> CopybookExtensions =
        new(StringComparer.OrdinalIgnoreCase) { ".cpy" };

    public static IReadOnlyCollection<string> KnownProgramExtensions => ProgramExtensions;

    public static IReadOnlyCollection<string> KnownCopybookExtensions => CopybookExtensions;

    public static IEnumerable<string> AllKnownExtensions =>
        ProgramExtensions.Concat(CopybookExtensions);

    public static bool IsCobolProgram(string path) =>
        ProgramExtensions.Contains(Path.GetExtension(path));

    public static bool IsCopybook(string path) =>
        CopybookExtensions.Contains(Path.GetExtension(path));

    public static bool IsKnown(string path) =>
        IsCobolProgram(path) || IsCopybook(path);

    public static SourceKind Classify(string path)
    {
        if (IsCobolProgram(path)) return SourceKind.CobolProgram;
        if (IsCopybook(path)) return SourceKind.Copybook;
        return SourceKind.Unknown;
    }

    public static IEnumerable<string> ProgramSearchPatterns =>
        ProgramExtensions.Select(ext => "*" + ext);

    public static IEnumerable<string> CopybookSearchPatterns =>
        CopybookExtensions.Select(ext => "*" + ext);

    public static IEnumerable<string> EnumerateProgramFiles(string root) =>
        EnumerateByPredicate(root, IsCobolProgram);

    public static IEnumerable<string> EnumerateCopybookFiles(string root) =>
        EnumerateByPredicate(root, IsCopybook);

    // Staging, preprocessing and conversion folders hold derived copies of the same
    // programs, so counting them would double the estate. Matches whole segments and
    // both separators, which a substring check on "/.convert-" gets wrong on Windows.
    public static bool IsScratchPath(string path) =>
        path.Split('/', '\\', StringSplitOptions.RemoveEmptyEntries).Any(segment =>
            segment.StartsWith(".convert-", StringComparison.Ordinal)
            || segment.Equals(".rekt-staging", StringComparison.Ordinal)
            || segment.Equals(".preprocessed", StringComparison.Ordinal));

    private static IEnumerable<string> EnumerateByPredicate(string root, Func<string, bool> predicate)
    {
        if (!Directory.Exists(root)) yield break;

        foreach (var path in Directory.EnumerateFiles(root, "*", SearchOption.AllDirectories))
        {
            // Relative to root, so a repository that itself sits under one of these
            // names does not exclude its own estate.
            if (IsScratchPath(Path.GetRelativePath(root, path))) continue;

            if (predicate(path)) yield return path;
        }
    }
}
