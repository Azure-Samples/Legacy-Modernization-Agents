using System.Text.Json.Serialization;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Facts;

/// <summary>
/// Curated per-program REKT handover (PR3). One <see cref="ProgramFacts"/>
/// instance corresponds to one COBOL program and is persisted as
/// <c>output/rekt/&lt;stem&gt;.facts.json</c>.
/// </summary>
/// <remarks>
/// <para>
/// Schema is versioned: <see cref="SchemaVersion"/> bumps whenever the field
/// set or semantics change. PR4 prompt projections include the schema version
/// in their cache key so a bump invalidates response cache entries.
/// </para>
/// <para>
/// <see cref="IdentitySchemeVersion"/> follows
/// <c>docs/basename-coupling-map.md</c>: pinned to <c>"v1-basename"</c> until
/// the ProgramKey migration. <see cref="RelativePath"/> is the forward-compat
/// hook — populated when available, ignored for identity today.
/// </para>
/// <para>
/// Fields that REKT cannot currently extract (e.g. CICS screens) are present
/// as empty lists with a <see cref="Warnings"/> entry, never silently absent.
/// </para>
/// </remarks>
public sealed record ProgramFacts
{
    /// <summary>Bump when the schema (field set / semantics) changes. PR4 cache key includes this.</summary>
    public const int CurrentSchemaVersion = 1;

    /// <summary>Identity scheme — see docs/basename-coupling-map.md.</summary>
    public const string CurrentIdentitySchemeVersion = "v1-basename";

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

/// <summary>Per-extraction confidence in the structural facts. Drawn from scan-cache outcome.</summary>
public enum FactConfidence
{
    None = 0,
    Low,        // Smojol returned DepsOnly / RawAst
    Partial,    // NoDialect (some structure missing)
    High,       // Full parse
}

/// <summary>Preprocessor transform applied to the source. Recorded so the LLM sees it as a warning.</summary>
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

    /// <summary>The program's own ID/header line, if extractable. Empty when unknown.</summary>
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

    /// <summary>Performance chains: each chain is an ordered list of paragraph names.</summary>
    [JsonPropertyName("performChains")] public IReadOnlyList<IReadOnlyList<string>> PerformChains { get; init; }
        = Array.Empty<IReadOnlyList<string>>();

    [JsonPropertyName("exits")] public IReadOnlyList<string> Exits { get; init; } = Array.Empty<string>();
}
