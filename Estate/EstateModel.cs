using System.Text.Json;
using System.Text.Json.Serialization;

namespace CobolToQuarkusMigration.Estate;

public static class EstateNodeKind
{
    public const string Program = "program";
    public const string Copybook = "copybook";
    public const string Job = "job";
    public const string Transaction = "transaction";
    public const string Dataset = "dataset";
    public const string Table = "table";
    public const string Map = "map";
    public const string File = "file";
}

public static class EstateEdgeKind
{
    public const string Calls = "calls";
    public const string Links = "links";
    public const string Copies = "copies";
    public const string Reads = "reads";
    public const string Writes = "writes";
    public const string Updates = "updates";
    public const string Deletes = "deletes";
    public const string UsesMap = "uses-map";
    public const string Starts = "starts";
    public const string Runs = "runs";
    public const string Feeds = "feeds";
    public const string BackedBy = "backed-by";
}

// Where a relationship was read from: the file, the 1-based line, and the source text on that line.
public sealed record EstateEvidence(string File, int Line, string Text);

public sealed record EstateNode
{
    public required string Id { get; init; }
    public required string Kind { get; init; }
    public required string Name { get; init; }
    public string? File { get; init; }
    // False when something references the node but the source does not contain it.
    public bool InSource { get; init; } = true;
    public SortedDictionary<string, string> Attributes { get; init; } = new(StringComparer.Ordinal);
}

public sealed record EstateEdge
{
    public required string From { get; init; }
    public required string To { get; init; }
    public required string Kind { get; init; }
    // How the target was resolved when not by a literal name: a variable, a JCL step and DD, a dataset list.
    public string? Via { get; init; }
    public List<EstateEvidence> Evidence { get; init; } = [];
}

public sealed record EstateHub(string NodeId, string Kind, string Name, int FanIn, int FanOut);

public sealed record EstateCluster
{
    public required string Id { get; init; }
    public required string Label { get; init; }
    public required IReadOnlyList<string> Programs { get; init; }
    public IReadOnlyList<string> Jobs { get; init; } = [];
    public IReadOnlyList<string> Tables { get; init; } = [];
    public IReadOnlyList<string> Datasets { get; init; } = [];
    public IReadOnlyList<string> Hubs { get; init; } = [];
    public double InternalWeight { get; init; }
    public double ExternalWeight { get; init; }
    public IReadOnlyList<string> DependsOn { get; init; } = [];
    public IReadOnlyList<string> DependedOnBy { get; init; } = [];
    public IReadOnlyList<string> Missing { get; init; } = [];
    // Programs with no coupling to anything, gathered into one cluster rather than one each.
    public bool Standalone { get; init; }
    public double CarveScore { get; init; }
    public SortedDictionary<string, double> ScoreBreakdown { get; init; } = new(StringComparer.Ordinal);
    public int Wave { get; init; }
}

public sealed record EstateWave(int Number, IReadOnlyList<string> Clusters, IReadOnlyList<IReadOnlyList<string>> Cycles);

public sealed record EstateGraph
{
    public const int CurrentSchemaVersion = 1;

    public int SchemaVersion { get; init; } = CurrentSchemaVersion;
    public DateTime GeneratedAtUtc { get; init; }
    public SortedDictionary<string, int> Counts { get; init; } = new(StringComparer.Ordinal);
    public IReadOnlyList<EstateNode> Nodes { get; init; } = [];
    public IReadOnlyList<EstateEdge> Edges { get; init; } = [];
    public IReadOnlyList<EstateHub> Hubs { get; init; } = [];
    public IReadOnlyList<EstateCluster> Clusters { get; init; } = [];
    public IReadOnlyList<EstateWave> Waves { get; init; } = [];
    public IReadOnlyList<string> Diagnostics { get; init; } = [];

    public static readonly JsonSerializerOptions JsonOptions = new()
    {
        WriteIndented = true,
        PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
        DefaultIgnoreCondition = JsonIgnoreCondition.WhenWritingNull,
        Encoder = System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping,
    };
}

// What a cluster needs to be converted on its own: its programs, the programs they reach that live
// elsewhere, what the source lacks, and the jobs that can then run end to end.
public sealed record EstateSlice
{
    public required string ClusterId { get; init; }
    public required IReadOnlyList<string> Programs { get; init; }
    public IReadOnlyList<string> Needs { get; init; } = [];
    public IReadOnlyList<string> Missing { get; init; } = [];
    public IReadOnlyList<string> Jobs { get; init; } = [];
    // Selectors for --program: the file name, or the source-relative path where two files share it.
    public IReadOnlyList<string> ProgramSelectors { get; init; } = [];
    public IReadOnlyList<string> NeedSelectors { get; init; } = [];
}

// Scoring and clustering are judgement calls, so every constant is configurable under EstateGraph in
// Config/appsettings.json; these defaults are what the section ships with.
public sealed record EstateGraphOptions
{
    public Dictionary<string, double> CouplingWeights { get; init; } = new(StringComparer.Ordinal)
    {
        ["call"] = 3,
        ["sameJob"] = 2,
        ["datasetFlow"] = 2,
        ["sharedTableWrite"] = 2,
        ["sharedTableRead"] = 0.5,
        ["sharedCopybook"] = 0.25,
    };

    // A node is a hub when its degree reaches both the minimum and the percentile of its kind.
    public int HubMinDegree { get; init; } = 4;
    public double HubPercentile { get; init; } = 0.9;
    // Coupling through a hub program is scaled by this, so a shared utility does not merge its callers.
    public double HubEdgeFactor { get; init; } = 0.25;

    public double LouvainResolution { get; init; } = 1.0;
    public int TargetSliceSize { get; init; } = 15;

    public Dictionary<string, double> CarveWeights { get; init; } = new(StringComparer.Ordinal)
    {
        ["cohesion"] = 0.4,
        ["completeness"] = 0.3,
        ["independence"] = 0.2,
        ["size"] = 0.1,
    };

    public List<string> IgnoredTablePrefixes { get; init; } = ["SYSIBM."];
    public List<string> SystemCopybooks { get; init; } = ["SQLCA", "SQLDA"];
    public List<string> MapExtensions { get; init; } = [".bms"];
    public List<string> CicsDefinitionExtensions { get; init; } = [".csd"];
    public List<string> GeneratedCopybookDirectories { get; init; } = ["copy-generated"];
    public int MaxEvidencePerEdge { get; init; } = 20;

    public double Coupling(string name) => CouplingWeights.TryGetValue(name, out var w) ? w : 0;
    public double Carve(string name) => CarveWeights.TryGetValue(name, out var w) ? w : 0;

    public const string SectionName = "EstateGraph";

    private static readonly JsonSerializerOptions ReadOptions = new()
    {
        PropertyNameCaseInsensitive = true,
        ReadCommentHandling = JsonCommentHandling.Skip,
        AllowTrailingCommas = true,
    };

    // A missing file or section gives the defaults; a section that does not parse is reported, not ignored.
    public static EstateGraphOptions Load(string? appSettingsPath, out string? warning)
    {
        warning = null;
        if (string.IsNullOrEmpty(appSettingsPath) || !System.IO.File.Exists(appSettingsPath)) return new();
        try
        {
            using var doc = JsonDocument.Parse(System.IO.File.ReadAllText(appSettingsPath),
                new JsonDocumentOptions { CommentHandling = JsonCommentHandling.Skip, AllowTrailingCommas = true });
            if (!doc.RootElement.TryGetProperty(SectionName, out var section)) return new();
            var loaded = section.Deserialize<EstateGraphOptions>(ReadOptions) ?? new();
            // A section that names only some weights keeps the defaults for the rest.
            var defaults = new EstateGraphOptions();
            foreach (var (k, v) in defaults.CouplingWeights) loaded.CouplingWeights.TryAdd(k, v);
            foreach (var (k, v) in defaults.CarveWeights) loaded.CarveWeights.TryAdd(k, v);
            return loaded;
        }
        catch (Exception ex) when (ex is JsonException or IOException)
        {
            warning = $"{SectionName} in {appSettingsPath} could not be read ({ex.Message}); using defaults.";
            return new();
        }
    }
}
