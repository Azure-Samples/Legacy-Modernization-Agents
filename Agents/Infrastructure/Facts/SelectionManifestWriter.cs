using System.Text.Json;
using System.Text.Json.Serialization;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Facts;

// Focused conversion (preview). Both front doors write this, so a narrow run can be told apart
// from a whole-estate run that lost programs, and can be reproduced from the recorded selectors.
public static class SelectionManifestWriter
{
    public static void Write(
        string manifestPath,
        string stagingDir,
        string factsDir,
        ProgramSelection selection,
        ProgramSelectionResult result)
    {
        var manifest = new SelectionManifest
        {
            GeneratedUtc = DateTimeOffset.UtcNow,
            StagingDir = stagingDir,
            FactsDir = factsDir,
            Selectors = new SelectionManifestSelectors
            {
                Programs = selection.Programs,
                IncludeCallers = selection.IncludeCallers,
                IncludeCallees = selection.IncludeCallees,
            },
            Programs = result.Programs,
            Matches = result.Matches
                .Select(match => new SelectionManifestMatch(match.Program, match.Reason))
                .ToList(),
            UnresolvedCallTargets = result.UnresolvedCallTargets,
        };

        var directory = Path.GetDirectoryName(Path.GetFullPath(manifestPath));
        if (!string.IsNullOrEmpty(directory))
            Directory.CreateDirectory(directory);

        File.WriteAllText(
            manifestPath,
            JsonSerializer.Serialize(manifest, new JsonSerializerOptions { WriteIndented = true }));
    }
}

public sealed record SelectionManifest
{
    public const int CurrentSchemaVersion = 1;

    [JsonPropertyName("schemaVersion")]
    public int SchemaVersion { get; init; } = CurrentSchemaVersion;

    [JsonPropertyName("generatedUtc")]
    public DateTimeOffset GeneratedUtc { get; init; }

    [JsonPropertyName("stagingDir")]
    public string StagingDir { get; init; } = "";

    [JsonPropertyName("factsDir")]
    public string FactsDir { get; init; } = "";

    [JsonPropertyName("selectors")]
    public SelectionManifestSelectors Selectors { get; init; } = new();

    [JsonPropertyName("programs")]
    public IReadOnlyList<string> Programs { get; init; } = Array.Empty<string>();

    [JsonPropertyName("matches")]
    public IReadOnlyList<SelectionManifestMatch> Matches { get; init; } = Array.Empty<SelectionManifestMatch>();

    [JsonPropertyName("unresolvedCallTargets")]
    public IReadOnlyList<string> UnresolvedCallTargets { get; init; } = Array.Empty<string>();
}

public sealed record SelectionManifestSelectors
{
    [JsonPropertyName("programs")]
    public IReadOnlyList<string> Programs { get; init; } = Array.Empty<string>();

    [JsonPropertyName("includeCallers")]
    public bool IncludeCallers { get; init; }

    [JsonPropertyName("includeCallees")]
    public bool IncludeCallees { get; init; }
}

public sealed record SelectionManifestMatch(
    [property: JsonPropertyName("program")] string Program,
    [property: JsonPropertyName("reason")] string Reason);
