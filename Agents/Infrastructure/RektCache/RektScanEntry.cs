namespace CobolToQuarkusMigration.Agents.Infrastructure.RektCache;

/// <summary>
/// Outcome of a previous REKT parse. Mirrors the four-stage smojol fallback ladder
/// in <c>doctor.sh run_rekt_parse</c>:
/// <list type="bullet">
///   <item><c>Full</c> — BUILD_BASE_ANALYSIS + WRITE_FLOW_AST + WRITE_CFG + WRITE_DATA_STRUCTURES succeeded.</item>
///   <item><c>NoDialect</c> — same set succeeded without the IDMS dialect jar.</item>
///   <item><c>RawAst</c> — WRITE_RAW_AST only.</item>
///   <item><c>DepsOnly</c> — only the dependency export succeeded (AST writer bug path).</item>
///   <item><c>Failed</c> — nothing succeeded.</item>
/// </list>
/// </summary>
public enum RektParseOutcome
{
    Failed = 0,
    DepsOnly,
    RawAst,
    NoDialect,
    Full,
}

/// <summary>
/// Confidence the planner attaches to a cached parse. Derived from <see cref="RektParseOutcome"/>
/// at upsert time so consumers don't need to know about the fallback ladder.
/// </summary>
public enum RektScanConfidence
{
    None = 0,
    Low,        // DepsOnly / RawAst
    Partial,    // NoDialect
    High,       // Full
}

/// <summary>
/// One row in <see cref="IRektScanCache"/>. Carries every input the planner needs to
/// decide skip-vs-parse on the next run.
/// </summary>
/// <remarks>
/// <see cref="DependencySnapshot"/> is the planner's contract for dependency-aware
/// invalidation: it records the hash of every transitive copybook used by this
/// program at parse time. The planner re-hashes copybooks on the next run and
/// invalidates the program when any snapshot entry mismatches the current hash.
/// </remarks>
public sealed record RektScanEntry
{
    public required string Basename { get; init; }
    public required string IdentitySchemeVersion { get; init; }

    /// <summary>Forward-compat identity column; null today per docs/basename-coupling-map.md.</summary>
    public string? RelativePath { get; init; }

    /// <summary>SHA-256 of the exact preprocessed bytes that were handed to smojol.</summary>
    public required string PreprocessedHash { get; init; }

    /// <summary>SHA-256 of the raw source bytes — diagnostic only, not used for invalidation.</summary>
    public string? SourceHash { get; init; }

    public required RektParseOutcome ParseOutcome { get; init; }
    public required RektScanConfidence Confidence { get; init; }
    public required DateTime ParsedAtUtc { get; init; }

    /// <summary>Free-text warnings surfaced by smojol or the planner. Persisted as JSON list.</summary>
    public IReadOnlyList<string> Warnings { get; init; } = Array.Empty<string>();

    /// <summary>
    /// Map of copybook basename → preprocessed-hash at parse time. Every transitive
    /// dependency goes here, not just the direct <c>COPY</c> targets — so a change
    /// deep in a copybook chain still invalidates the dependent program.
    /// </summary>
    public IReadOnlyDictionary<string, string> DependencySnapshot { get; init; }
        = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
}
