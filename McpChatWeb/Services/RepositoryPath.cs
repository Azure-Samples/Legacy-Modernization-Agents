// Decides whether a caller-supplied path stays inside the repository.
//
// The chat feature accepts a report path from the browser and reads the file into the model's
// context. Containment was tested with a plain string prefix, which is not a containment test:
// "<repo>-notes" starts with "<repo>", so a sibling directory reads as inside the repository.
// The comparison has to land on a directory boundary.

namespace McpChatWeb.Services;

public static class RepositoryPath
{
    /// <summary>
    /// The full path of <paramref name="relativeOrAbsolute"/> resolved against
    /// <paramref name="repositoryRoot"/>, or null when it does not resolve to a file inside it.
    /// </summary>
    public static string? ResolveInside(string repositoryRoot, string? relativeOrAbsolute)
    {
        if (string.IsNullOrWhiteSpace(relativeOrAbsolute)) return null;

        string root, candidate;
        try
        {
            root = Path.GetFullPath(repositoryRoot);
            // An absolute argument replaces the root entirely under Path.Combine, so the result
            // is still checked for containment rather than trusted for being combined.
            candidate = Path.GetFullPath(Path.Combine(root, relativeOrAbsolute));
        }
        catch (ArgumentException)
        {
            return null;
        }

        return IsInside(root, candidate) ? candidate : null;
    }

    /// <summary>True when <paramref name="candidate"/> sits within <paramref name="root"/>.</summary>
    public static bool IsInside(string root, string candidate)
    {
        var normalisedRoot = Path.GetFullPath(root).TrimEnd(Path.DirectorySeparatorChar);
        var normalisedCandidate = Path.GetFullPath(candidate);

        if (string.Equals(normalisedRoot, normalisedCandidate.TrimEnd(Path.DirectorySeparatorChar),
                PathComparison))
        {
            return true;
        }

        return normalisedCandidate.StartsWith(
            normalisedRoot + Path.DirectorySeparatorChar, PathComparison);
    }

    // macOS and Windows resolve paths case-insensitively; matching that avoids rejecting a path
    // the file system would have accepted.
    private static StringComparison PathComparison =>
        OperatingSystem.IsLinux() ? StringComparison.Ordinal : StringComparison.OrdinalIgnoreCase;
}
