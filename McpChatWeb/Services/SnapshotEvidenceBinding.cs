using System.Text.Json;

namespace McpChatWeb.Services;

internal sealed record SnapshotEvidenceBinding(
    string SourceRoot, string FactsRoot, string ManifestPath, IReadOnlySet<string> Programs)
{
    internal static SnapshotEvidenceBinding Resolve(
        string repoRoot, string manifestPath, string graphSourceDirectory, string graphOutputDirectory)
    {
        var scansRoot = Path.GetFullPath(Path.Combine(repoRoot, "output", "rekt-scans"));
        var manifest = ContainedPath(scansRoot, manifestPath);
        var folder = Path.GetDirectoryName(manifest)!;
        if (Path.GetFileName(manifest) != "scan.json" || Path.GetDirectoryName(folder) != scansRoot)
            throw new InvalidDataException("The recorded manifest is not an immediate named scan.json under output/rekt-scans.");
        if (new FileInfo(manifest).Length > 8 * 1024 * 1024)
            throw new InvalidDataException("The recorded native manifest exceeds the supported 8 MiB assessment bound.");
        using var stream = File.OpenRead(manifest);
        using var doc = JsonDocument.Parse(stream, new JsonDocumentOptions { MaxDepth = 48 });
        if (doc.RootElement.ValueKind != JsonValueKind.Object)
            throw new InvalidDataException("The selected native manifest is not a JSON object.");
        string Required(string key) => doc.RootElement.TryGetProperty(key, out var value)
            && value.ValueKind == JsonValueKind.String && !string.IsNullOrWhiteSpace(value.GetString())
                ? value.GetString()!
                : throw new InvalidDataException($"The selected manifest does not record {key}.");
        if (Required("name") != Path.GetFileName(folder))
            throw new InvalidDataException("The recorded native manifest name does not match its evidence folder.");
        var source = ContainedPath(folder, Required("sourceRoot"));
        var parsed = ContainedPath(folder, Required("parsedSourceRoot"));
        var facts = ContainedPath(folder, Required("artifactsRoot"));
        if (!Directory.Exists(source) || !Directory.Exists(parsed) || !Directory.Exists(facts))
            throw new InvalidDataException("A recorded original-source, prepared-source or artifact directory is unavailable.");
        if (Path.GetFullPath(graphSourceDirectory) != parsed || Path.GetFullPath(graphOutputDirectory) != facts)
            throw new InvalidDataException("The graph snapshot's ingested prepared-source/artifact paths differ from this manifest.");
        if (!doc.RootElement.TryGetProperty("programs", out var rows) || rows.ValueKind != JsonValueKind.Array
            || !doc.RootElement.TryGetProperty("expectedPrograms", out var expected)
            || expected.ValueKind != JsonValueKind.Number || !expected.TryGetInt32(out var count)
            || count < 1 || count > 5000 || rows.GetArrayLength() != count)
            throw new InvalidDataException("The selected manifest has no bounded, complete recorded program scope.");
        var programs = new HashSet<string>(StringComparer.Ordinal);
        foreach (var row in rows.EnumerateArray())
        {
            if (row.ValueKind != JsonValueKind.Object || !row.TryGetProperty("relativePath", out var relative)
                || relative.ValueKind != JsonValueKind.String)
                throw new InvalidDataException("A selected native program has no recorded source-relative path.");
            var name = relative.GetString()!.Replace('\\', '/');
            if (Path.IsPathRooted(name) || name.Split('/').Any(part => part is "" or "." or "..")
                || !CobolToQuarkusMigration.Helpers.SourceTypeRegistry.IsCobolProgram(name) || !programs.Add(name))
                throw new InvalidDataException("A selected native program path is unsafe, duplicated or not a COBOL program.");
            if (!File.Exists(ContainedPath(source, Path.Combine(source, name))))
                throw new InvalidDataException($"Recorded original source for {name} is unavailable.");
        }
        return new(source, facts, manifest, programs);
    }

    private static string ContainedPath(string root, string path)
    {
        root = Path.GetFullPath(root).TrimEnd(Path.DirectorySeparatorChar);
        if (!Path.IsPathFullyQualified(path))
            throw new InvalidDataException("The recorded snapshot evidence path is not absolute.");
        path = Path.GetFullPath(path);
        if (!path.StartsWith(root + Path.DirectorySeparatorChar, StringComparison.Ordinal))
            throw new InvalidDataException("The recorded snapshot evidence path escapes its named evidence folder.");
        var current = root;
        if ((File.GetAttributes(current) & FileAttributes.ReparsePoint) != 0)
            throw new InvalidDataException("Linked evidence roots cannot establish an approved snapshot binding.");
        foreach (var part in Path.GetRelativePath(root, path).Split(Path.DirectorySeparatorChar))
        {
            current = Path.Combine(current, part);
            if ((File.GetAttributes(current) & FileAttributes.ReparsePoint) != 0)
                throw new InvalidDataException("Linked evidence paths cannot establish an approved snapshot binding.");
        }
        return path;
    }
}
