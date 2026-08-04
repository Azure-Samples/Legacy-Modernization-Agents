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

    private static IEnumerable<string> EnumerateByPredicate(string root, Func<string, bool> predicate)
    {
        if (!Directory.Exists(root)) yield break;

        foreach (var path in Directory.EnumerateFiles(root, "*", SearchOption.AllDirectories))
        {
            // Skip well-known internal staging dirs — they contain copies of the
            // same files and would inflate counts.
            if (path.Contains($"{Path.DirectorySeparatorChar}.rekt-staging{Path.DirectorySeparatorChar}") ||
                path.Contains($"{Path.DirectorySeparatorChar}.preprocessed{Path.DirectorySeparatorChar}"))
                continue;

            if (predicate(path)) yield return path;
        }
    }
}
