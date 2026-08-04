using System.Text.Json.Serialization;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Facts;

public sealed record ProgramFacts
{
    public const int CurrentSchemaVersion = 1;

    public const string CurrentIdentitySchemeVersion = "v2-source-relative";

    [JsonPropertyName("schemaVersion")]
    public int SchemaVersion { get; init; } = CurrentSchemaVersion;

    [JsonPropertyName("identitySchemeVersion")]
    public string IdentitySchemeVersion { get; init; } = CurrentIdentitySchemeVersion;

    [JsonPropertyName("basename")]
    public required string Basename { get; init; }

    [JsonPropertyName("stem")]
    public required string Stem { get; init; }

    [JsonPropertyName("relativePath")]
    public string? RelativePath { get; init; }

    [JsonPropertyName("sourceHash")]
    public required string SourceHash { get; init; }

    [JsonPropertyName("confidence")]
    public required FactConfidence Confidence { get; init; }

    [JsonPropertyName("warnings")]
    public IReadOnlyList<string> Warnings { get; init; } = Array.Empty<string>();

    [JsonPropertyName("preprocessNotes")]
    public IReadOnlyList<PreprocessNote> PreprocessNotes { get; init; } = Array.Empty<PreprocessNote>();

    [JsonPropertyName("summary")]
    public required ProgramSummary Summary { get; init; }

    [JsonPropertyName("io")]
    public IoFacts Io { get; init; } = new();

    [JsonPropertyName("data")]
    public DataFacts Data { get; init; } = new();

    [JsonPropertyName("callers")]
    public IReadOnlyList<string> Callers { get; init; } = Array.Empty<string>();

    [JsonPropertyName("callees")]
    public IReadOnlyList<string> Callees { get; init; } = Array.Empty<string>();

    [JsonPropertyName("controlFlow")]
    public ControlFlowFacts ControlFlow { get; init; } = new();

    [JsonPropertyName("externalEffects")]
    public IReadOnlyList<string> ExternalEffects { get; init; } = Array.Empty<string>();
}

public enum FactConfidence
{
    None = 0,
    Low,        // Smojol returned DepsOnly / RawAst
    Partial,    // NoDialect (some structure missing)
    High,       // Full parse
}

public sealed record PreprocessNote(
    [property: JsonPropertyName("rule")] string Rule,
    [property: JsonPropertyName("line")] int Line,
    [property: JsonPropertyName("before")] string? Before = null,
    [property: JsonPropertyName("after")] string? After = null);

public sealed record ProgramSummary
{
    [JsonPropertyName("loc")] public int Loc { get; init; }
    [JsonPropertyName("paragraphs")] public int Paragraphs { get; init; }
    [JsonPropertyName("sections")] public int Sections { get; init; }
    [JsonPropertyName("isCopybook")] public bool IsCopybook { get; init; }

    [JsonPropertyName("programId")] public string ProgramId { get; init; } = "";
}

public sealed record IoFacts
{
    [JsonPropertyName("files")] public IReadOnlyList<FileAccess> Files { get; init; } = Array.Empty<FileAccess>();
    [JsonPropertyName("screens")] public IReadOnlyList<string> Screens { get; init; } = Array.Empty<string>();
    [JsonPropertyName("dbTables")] public IReadOnlyList<DbTableAccess> DbTables { get; init; } = Array.Empty<DbTableAccess>();
    [JsonPropertyName("queues")] public IReadOnlyList<string> Queues { get; init; } = Array.Empty<string>();
}

public sealed record FileAccess(
    [property: JsonPropertyName("name")] string Name,
    [property: JsonPropertyName("operations")] IReadOnlyList<string> Operations);

public sealed record DbTableAccess(
    [property: JsonPropertyName("name")] string Name,
    [property: JsonPropertyName("operations")] IReadOnlyList<string> Operations);

public sealed record DataFacts
{
    [JsonPropertyName("groups")] public IReadOnlyList<DataGroup> Groups { get; init; } = Array.Empty<DataGroup>();
    [JsonPropertyName("copybooksUsed")] public IReadOnlyList<string> CopybooksUsed { get; init; } = Array.Empty<string>();
}

public sealed record DataGroup(
    [property: JsonPropertyName("name")] string Name,
    [property: JsonPropertyName("fields")] int FieldCount,
    [property: JsonPropertyName("redefines")] bool Redefines);

public sealed record ControlFlowFacts
{
    [JsonPropertyName("entryPoints")] public IReadOnlyList<string> EntryPoints { get; init; } = Array.Empty<string>();

    [JsonPropertyName("performChains")] public IReadOnlyList<IReadOnlyList<string>> PerformChains { get; init; }
        = Array.Empty<IReadOnlyList<string>>();

    [JsonPropertyName("exits")] public IReadOnlyList<string> Exits { get; init; } = Array.Empty<string>();
}
