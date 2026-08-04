namespace CobolToQuarkusMigration.Helpers;

internal static class SourcePathHelper
{
    public static string NormalizeRelativePath(string path)
    {
        if (string.IsNullOrWhiteSpace(path)) return string.Empty;

        var normalized = path.Replace('\\', '/').Trim();
        while (normalized.StartsWith("./", StringComparison.Ordinal))
            normalized = normalized[2..];

        return normalized.TrimStart('/');
    }

    public static string ToOsRelativePath(string relativePath) =>
        NormalizeRelativePath(relativePath).Replace('/', Path.DirectorySeparatorChar);

    public static IReadOnlyList<string> EnumerateProgramRelativePaths(string root)
    {
        if (!Directory.Exists(root)) return Array.Empty<string>();

        return SourceTypeRegistry.EnumerateProgramFiles(root)
            .Select(path => NormalizeRelativePath(Path.GetRelativePath(root, path)))
            .Distinct(StringComparer.OrdinalIgnoreCase)
            .OrderBy(path => path, StringComparer.OrdinalIgnoreCase)
            .ToList();
    }
}
