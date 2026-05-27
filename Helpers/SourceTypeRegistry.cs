namespace CobolToQuarkusMigration.Helpers;

/// <summary>
/// Source-file classification by extension.
/// Add new kinds here as parsers mature — see docs/throttling-and-cache-design.md §12.
/// </summary>
public enum SourceKind
{
    Unknown = 0,
    CobolProgram,
    Copybook,
}

/// <summary>
/// Single source of truth for which file extensions the pipeline considers
/// COBOL programs vs copybooks. Centralises what used to be hard-coded
/// <c>.cbl</c>/<c>.cpy</c> string checks scattered across FileHelper, doctor.sh,
/// resolve-programs.py, preprocess-for-rekt.sh, and the graph-populator scripts.
/// </summary>
public static class SourceTypeRegistry
{
    // Stored lower-case; comparisons are case-insensitive.
    private static readonly HashSet<string> ProgramExtensions =
        new(StringComparer.OrdinalIgnoreCase) { ".cbl", ".cob" };

    private static readonly HashSet<string> CopybookExtensions =
        new(StringComparer.OrdinalIgnoreCase) { ".cpy" };

    /// <summary>Returns the set of recognised COBOL program extensions (lower-case, dot-prefixed).</summary>
    public static IReadOnlyCollection<string> KnownProgramExtensions => ProgramExtensions;

    /// <summary>Returns the set of recognised copybook extensions (lower-case, dot-prefixed).</summary>
    public static IReadOnlyCollection<string> KnownCopybookExtensions => CopybookExtensions;

    /// <summary>All recognised extensions (programs + copybooks).</summary>
    public static IEnumerable<string> AllKnownExtensions =>
        ProgramExtensions.Concat(CopybookExtensions);

    /// <summary>True if the path's extension is a recognised COBOL program (.cbl / .cob).</summary>
    public static bool IsCobolProgram(string path) =>
        ProgramExtensions.Contains(Path.GetExtension(path));

    /// <summary>True if the path's extension is a recognised copybook (.cpy).</summary>
    public static bool IsCopybook(string path) =>
        CopybookExtensions.Contains(Path.GetExtension(path));

    /// <summary>True for any recognised source kind.</summary>
    public static bool IsKnown(string path) =>
        IsCobolProgram(path) || IsCopybook(path);

    /// <summary>Classify a path by extension.</summary>
    public static SourceKind Classify(string path)
    {
        if (IsCobolProgram(path)) return SourceKind.CobolProgram;
        if (IsCopybook(path)) return SourceKind.Copybook;
        return SourceKind.Unknown;
    }

    /// <summary>
    /// Glob patterns for <see cref="Directory.EnumerateFiles(string, string, SearchOption)"/>.
    /// Returns lower-case patterns; on case-sensitive filesystems callers should also
    /// search uppercase variants — use <see cref="EnumerateProgramFiles"/> instead when possible.
    /// </summary>
    public static IEnumerable<string> ProgramSearchPatterns =>
        ProgramExtensions.Select(ext => "*" + ext);

    /// <inheritdoc cref="ProgramSearchPatterns" />
    public static IEnumerable<string> CopybookSearchPatterns =>
        CopybookExtensions.Select(ext => "*" + ext);

    /// <summary>
    /// Recursively enumerate COBOL program files under <paramref name="root"/>,
    /// honouring every registered program extension and skipping common staging dirs.
    /// </summary>
    public static IEnumerable<string> EnumerateProgramFiles(string root) =>
        EnumerateByPredicate(root, IsCobolProgram);

    /// <summary>Recursively enumerate copybook files under <paramref name="root"/>.</summary>
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
