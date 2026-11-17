namespace CobolToQuarkusMigration.Persistence.GraphIngest;

// Graph nodes are keyed by the program's path relative to the source root, so two programs
// with the same name in different folders stay distinct and parse artifacts map to exactly one.
public static class GraphSourcePaths
{
    private static readonly string[] ArtifactPrefixes =
        ["flow-ast-", "cfg-", "control-flow-", "data-structure-", "data_structure-"];

    public static string SourceRelativePath(string filePath, string sourceDir) =>
        ToPosix(Path.GetRelativePath(sourceDir, filePath));

    public static string ScopedGraphId(long runId, string program, string upstreamId) =>
        GraphUid.Make(runId, program, upstreamId);

    public static string? ArtifactSourcePath(string artifactPath, string outputDir, IReadOnlyCollection<string> sourceFiles)
    {
        var candidates = ArtifactCandidates(artifactPath, outputDir);
        var sources = sourceFiles.Select(ToPosix).ToList();
        foreach (var candidate in candidates)
        {
            if (sources.Contains(candidate, StringComparer.Ordinal)) return candidate;
        }
        foreach (var candidate in candidates)
        {
            var matches = BasenameMatches(candidate, sources);
            if (matches.Count == 1) return matches[0];
        }
        return null;
    }

    public static string UnresolvedArtifactReason(string artifactPath, string outputDir, IReadOnlyCollection<string> sourceFiles)
    {
        var candidates = ArtifactCandidates(artifactPath, outputDir);
        var sources = sourceFiles.Select(ToPosix).ToList();
        foreach (var candidate in candidates)
        {
            var matches = BasenameMatches(candidate, sources);
            if (matches.Count > 1)
            {
                return $"ambiguous source: {Name(candidate)} exists in more than one folder " +
                       $"({string.Join(", ", matches.Order(StringComparer.Ordinal))})";
            }
        }
        return "no matching source: expected a program in source/ named " +
               string.Join(" or ", candidates.Distinct().Order(StringComparer.Ordinal)) +
               " (was it removed, or is this not a parse artifact?)";
    }

    internal static string[] RelativeParts(string path, string root) =>
        Path.GetRelativePath(root, path).Split(
            [Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar], StringSplitOptions.RemoveEmptyEntries);

    internal static string ToPosix(string path) => path.Replace('\\', '/');

    // pathlib's stem: a leading dot alone is not an extension.
    internal static string Stem(string name)
    {
        var dot = name.LastIndexOf('.');
        return dot > 0 && dot < name.Length - 1 ? name[..dot] : name;
    }

    private static List<string> ArtifactCandidates(string artifactPath, string outputDir)
    {
        var parts = RelativeParts(artifactPath, outputDir);
        var candidates = new List<string>();

        for (var index = 0; index < parts.Length - 1; index++)
        {
            if (parts[index].EndsWith(".report", StringComparison.Ordinal))
            {
                candidates.Add(JoinPosix(parts[..index].Append(parts[index][..^".report".Length])));
            }
        }

        var artifactName = Stem(parts[^1]);
        var prefix = ArtifactPrefixes.FirstOrDefault(p => artifactName.StartsWith(p, StringComparison.OrdinalIgnoreCase));
        if (prefix is not null) artifactName = artifactName[prefix.Length..];
        candidates.Add(artifactName);

        // A deps-only parse writes <source>-deps.json beside the source layout and no .report
        // directory, so leaving the suffix on makes every degraded program unresolvable.
        if (artifactName.EndsWith("-deps", StringComparison.Ordinal))
        {
            var stripped = artifactName[..^"-deps".Length];
            candidates.Add(JoinPosix(parts[..^1].Append(stripped)));
            candidates.Add(stripped);
        }
        return candidates;
    }

    private static List<string> BasenameMatches(string candidate, List<string> sources) =>
        sources.Where(source => Name(source) == Name(candidate)).ToList();

    private static string Name(string posixPath) => posixPath[(posixPath.LastIndexOf('/') + 1)..];

    private static string JoinPosix(IEnumerable<string> segments)
    {
        var joined = string.Join('/', segments.Where(s => s.Length > 0));
        return joined.Length == 0 ? "." : joined;
    }
}
