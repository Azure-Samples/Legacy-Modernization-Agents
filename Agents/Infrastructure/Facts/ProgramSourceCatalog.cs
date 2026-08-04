using System.Text.Json;
using CobolToQuarkusMigration.Helpers;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Facts;

internal sealed class ProgramSourceCatalog
{
    private readonly Dictionary<string, string> _exactByRelativePath;
    private readonly Dictionary<string, List<string>> _pathsByBasename;
    private readonly Dictionary<string, List<string>> _pathsByStem;

    public ProgramSourceCatalog(IEnumerable<string> relativePaths)
    {
        RelativePaths = relativePaths
            .Select(SourcePathHelper.NormalizeRelativePath)
            .Where(path => !string.IsNullOrWhiteSpace(path))
            .Distinct(StringComparer.OrdinalIgnoreCase)
            .OrderBy(path => path, StringComparer.OrdinalIgnoreCase)
            .ToList();

        _exactByRelativePath = RelativePaths.ToDictionary(
            path => path,
            path => path,
            StringComparer.OrdinalIgnoreCase);
        _pathsByBasename = BuildIndex(RelativePaths, Path.GetFileName);
        _pathsByStem = BuildIndex(RelativePaths, Path.GetFileNameWithoutExtension);
    }

    public IReadOnlyList<string> RelativePaths { get; }

    public static ProgramSourceCatalog FromStagingDirectory(string stagingDir) =>
        new(SourcePathHelper.EnumerateProgramRelativePaths(stagingDir));

    public IReadOnlyList<string> ResolveSelectors(string? selectorsCsv)
    {
        if (string.IsNullOrWhiteSpace(selectorsCsv))
            return RelativePaths;

        var resolved = new List<string>();
        foreach (var selector in selectorsCsv.Split(
                     ',',
                     StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries))
        {
            var match = ResolveSelector(selector);
            if (!resolved.Contains(match, StringComparer.OrdinalIgnoreCase))
                resolved.Add(match);
        }

        return resolved;
    }

    public string ResolveSelector(string selector)
    {
        if (string.IsNullOrWhiteSpace(selector))
            throw new InvalidOperationException("Program selector cannot be empty.");

        var normalized = SourcePathHelper.NormalizeRelativePath(selector);
        if (_exactByRelativePath.TryGetValue(normalized, out var exact))
            return exact;

        var basename = Path.GetFileName(normalized);
        var basenameMatches = Lookup(_pathsByBasename, basename);
        if (basenameMatches.Count == 1)
            return basenameMatches[0];
        if (basenameMatches.Count > 1)
            throw CreateAmbiguousSelectorException(selector, "basename", basenameMatches);

        var stem = Path.GetFileNameWithoutExtension(basename);
        var stemMatches = Lookup(_pathsByStem, stem);
        if (stemMatches.Count == 1)
            return stemMatches[0];
        if (stemMatches.Count > 1)
            throw CreateAmbiguousSelectorException(selector, "stem", stemMatches);

        throw new InvalidOperationException(
            $"Program selector '{selector}' did not match any staged program.");
    }

    public string? ResolveCallTarget(string rawTarget)
    {
        if (string.IsNullOrWhiteSpace(rawTarget)) return null;

        var normalized = SourcePathHelper.NormalizeRelativePath(rawTarget.Trim().Trim('\'', '"'));
        if (_exactByRelativePath.TryGetValue(normalized, out var exact))
            return exact;

        var basename = Path.GetFileName(normalized);
        var basenameMatches = Lookup(_pathsByBasename, basename);
        if (basenameMatches.Count == 1)
            return basenameMatches[0];

        var stem = Path.GetFileNameWithoutExtension(basename);
        var stemMatches = Lookup(_pathsByStem, stem);
        return stemMatches.Count == 1 ? stemMatches[0] : null;
    }

    public bool HasUniqueBasename(string basename, out string relativePath)
    {
        var matches = Lookup(_pathsByBasename, Path.GetFileName(basename));
        if (matches.Count == 1)
        {
            relativePath = matches[0];
            return true;
        }

        relativePath = string.Empty;
        return false;
    }

    public bool TryResolveSourceIdentity(string identity, out string relativePath)
    {
        if (string.IsNullOrWhiteSpace(identity))
        {
            relativePath = string.Empty;
            return false;
        }

        var normalized = SourcePathHelper.NormalizeRelativePath(identity);
        if (_exactByRelativePath.TryGetValue(normalized, out var exact))
        {
            relativePath = exact;
            return true;
        }

        var basenameMatches = Lookup(_pathsByBasename, Path.GetFileName(normalized));
        if (basenameMatches.Count == 1)
        {
            relativePath = basenameMatches[0];
            return true;
        }

        var stemMatches = Lookup(_pathsByStem, Path.GetFileNameWithoutExtension(Path.GetFileName(normalized)));
        if (stemMatches.Count == 1)
        {
            relativePath = stemMatches[0];
            return true;
        }

        relativePath = string.Empty;
        return false;
    }

    public string? ResolveFactsFileToProgram(string factsFilePath, string factsRoot)
    {
        var candidates = new List<string>();
        var pathCandidate = ProgramFactsArtifactLocator.TryGetProgramRelativePath(
            factsFilePath,
            factsRoot);
        if (!string.IsNullOrEmpty(pathCandidate))
            candidates.Add(pathCandidate);

        candidates.AddRange(ReadFactsIdentityCandidates(factsFilePath));

        foreach (var candidate in candidates
                     .Where(candidate => !string.IsNullOrWhiteSpace(candidate))
                     .Distinct(StringComparer.OrdinalIgnoreCase))
        {
            if (TryResolveSourceIdentity(candidate, out var resolved))
                return resolved;
        }

        return null;
    }

    private static Dictionary<string, List<string>> BuildIndex(
        IEnumerable<string> relativePaths,
        Func<string, string?> keySelector)
    {
        var index = new Dictionary<string, List<string>>(StringComparer.OrdinalIgnoreCase);
        foreach (var relativePath in relativePaths)
        {
            var key = keySelector(relativePath);
            if (string.IsNullOrWhiteSpace(key)) continue;

            if (!index.TryGetValue(key, out var matches))
            {
                matches = new List<string>();
                index[key] = matches;
            }

            matches.Add(relativePath);
        }

        return index;
    }

    private static IReadOnlyList<string> Lookup(
        IReadOnlyDictionary<string, List<string>> index,
        string? key)
    {
        if (string.IsNullOrWhiteSpace(key)) return Array.Empty<string>();
        return index.TryGetValue(key, out var matches) ? matches : Array.Empty<string>();
    }

    private static Exception CreateAmbiguousSelectorException(
        string selector,
        string matchKind,
        IReadOnlyCollection<string> matches) =>
        new InvalidOperationException(
            $"Program selector '{selector}' matched multiple staged programs by {matchKind}: " +
            string.Join(", ", matches.OrderBy(match => match, StringComparer.OrdinalIgnoreCase)) +
            ". Use a source-relative path.");

    private static IEnumerable<string> ReadFactsIdentityCandidates(string factsFilePath)
    {
        var candidates = new List<string>();
        try
        {
            using var doc = JsonDocument.Parse(File.ReadAllText(factsFilePath));
            if (doc.RootElement.TryGetProperty("relativePath", out var relativePath)
                && relativePath.ValueKind == JsonValueKind.String
                && !string.IsNullOrWhiteSpace(relativePath.GetString()))
            {
                candidates.Add(relativePath.GetString()!);
            }

            if (doc.RootElement.TryGetProperty("basename", out var basename)
                && basename.ValueKind == JsonValueKind.String
                && !string.IsNullOrWhiteSpace(basename.GetString()))
            {
                candidates.Add(basename.GetString()!);
            }

            if (doc.RootElement.TryGetProperty("stem", out var stem)
                && stem.ValueKind == JsonValueKind.String
                && !string.IsNullOrWhiteSpace(stem.GetString()))
            {
                candidates.Add(stem.GetString()!);
            }
        }
        catch
        {
            return Array.Empty<string>();
        }

        return candidates;
    }
}

internal static class ProgramFactsArtifactLocator
{
    public const string FactsSuffix = ".facts.json";

    public static string GetFactsFilePath(string factsRoot, string programRelativePath) =>
        Path.Combine(
            factsRoot,
            SourcePathHelper.ToOsRelativePath(GetFactsFileRelativePath(programRelativePath)));

    public static string GetFactsFileRelativePath(string programRelativePath) =>
        SourcePathHelper.NormalizeRelativePath(programRelativePath) + FactsSuffix;

    public static string? TryGetProgramRelativePath(string factsFilePath, string factsRoot)
    {
        var relativePath = SourcePathHelper.NormalizeRelativePath(
            Path.GetRelativePath(factsRoot, factsFilePath));
        return relativePath.EndsWith(FactsSuffix, StringComparison.OrdinalIgnoreCase)
            ? relativePath[..^FactsSuffix.Length]
            : null;
    }

    public static ProgramFacts? TryLoad(string factsDir, string programIdentity)
    {
        if (string.IsNullOrWhiteSpace(factsDir) || string.IsNullOrWhiteSpace(programIdentity))
            return null;

        foreach (var candidate in EnumerateCandidatePaths(factsDir, programIdentity))
        {
            var facts = TryLoadFromPath(candidate);
            if (facts is not null)
                return facts;
        }

        if (!Directory.Exists(factsDir)) return null;

        var normalizedIdentity = SourcePathHelper.NormalizeRelativePath(programIdentity);
        var basename = Path.GetFileName(normalizedIdentity);
        if (string.IsNullOrWhiteSpace(basename)) return null;

        var recursiveMatches = Directory.EnumerateFiles(
                factsDir,
                basename + FactsSuffix,
                SearchOption.AllDirectories)
            .OrderBy(path => path, StringComparer.OrdinalIgnoreCase)
            .ToList();

        if (recursiveMatches.Count == 1)
            return TryLoadFromPath(recursiveMatches[0]);

        var exactRelativeMatch = recursiveMatches
            .Select(path => new
            {
                Path = path,
                Facts = TryLoadFromPath(path),
            })
            .FirstOrDefault(match =>
                match.Facts is not null &&
                string.Equals(
                    SourcePathHelper.NormalizeRelativePath(match.Facts.RelativePath ?? ""),
                    normalizedIdentity,
                    StringComparison.OrdinalIgnoreCase));

        return exactRelativeMatch?.Facts;
    }

    private static IEnumerable<string> EnumerateCandidatePaths(string factsDir, string programIdentity)
    {
        var normalizedIdentity = SourcePathHelper.NormalizeRelativePath(programIdentity);
        if (!string.IsNullOrWhiteSpace(normalizedIdentity))
            yield return GetFactsFilePath(factsDir, normalizedIdentity);

        var basename = Path.GetFileName(normalizedIdentity);
        if (!string.IsNullOrWhiteSpace(basename))
            yield return Path.Combine(factsDir, basename + FactsSuffix);

        var stem = Path.GetFileNameWithoutExtension(basename);
        if (!string.IsNullOrWhiteSpace(stem))
            yield return Path.Combine(factsDir, stem + FactsSuffix);
    }

    private static ProgramFacts? TryLoadFromPath(string path)
    {
        if (!File.Exists(path)) return null;

        try
        {
            return JsonSerializer.Deserialize<ProgramFacts>(File.ReadAllText(path));
        }
        catch
        {
            return null;
        }
    }
}
