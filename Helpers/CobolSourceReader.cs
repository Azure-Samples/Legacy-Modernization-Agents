using System.Text;

namespace CobolToQuarkusMigration.Helpers;

public static class CobolSourceReader
{
    private static readonly UTF8Encoding StrictUtf8 = new(false, true);

    public static string Read(string path) => Decode(File.ReadAllBytes(path));

    public static async Task<string> ReadAsync(string path, CancellationToken cancellationToken = default) =>
        Decode(await File.ReadAllBytesAsync(path, cancellationToken));

    public static string Decode(byte[] bytes)
    {
        var start = bytes.AsSpan().StartsWith(new byte[] { 0xEF, 0xBB, 0xBF }) ? 3 : 0;
        var end = bytes.Length;
        while (end > start && bytes[end - 1] == 0x1A) end--;
        var source = bytes.AsSpan(start, end - start);
        try { return StrictUtf8.GetString(source); }
        catch (DecoderFallbackException) { return Encoding.Latin1.GetString(source); }
    }

    // Stem-only registries cannot describe ambiguous owners. A selected scope must be unambiguous; an
    // unscoped folder may hold several estates, so its ambiguous stems are left unresolved for every file.
    internal static string[] RequireUniqueStems(IEnumerable<string> paths)
    {
        var ordered = paths.OrderBy(Path.GetFullPath, StringComparer.Ordinal).ToArray();
        var conflicts = ordered.GroupBy(Path.GetFileNameWithoutExtension, StringComparer.OrdinalIgnoreCase)
            .Where(group => group.Count() > 1).ToArray();
        if (conflicts.Length == 0) return ordered;
        if (ConversionSourceScope.IsSet)
            throw new InvalidOperationException(
                $"Ambiguous source stem '{conflicts[0].Key}': {string.Join(", ", conflicts[0])}");
        var ambiguous = conflicts.Select(group => group.Key).ToHashSet(StringComparer.OrdinalIgnoreCase);
        return ordered.Where(path => !ambiguous.Contains(Path.GetFileNameWithoutExtension(path))).ToArray();
    }
}
