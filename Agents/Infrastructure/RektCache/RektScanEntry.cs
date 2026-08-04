namespace CobolToQuarkusMigration.Agents.Infrastructure.RektCache;

public enum RektParseOutcome
{
    Failed = 0,
    DepsOnly,
    RawAst,
    NoDialect,
    Full,
}

public enum RektScanConfidence
{
    None = 0,
    Low,        // DepsOnly / RawAst
    Partial,    // NoDialect
    High,       // Full
}

public sealed record RektScanEntry
{
    public required string Basename { get; init; }
    public required string IdentitySchemeVersion { get; init; }

    public string? RelativePath { get; init; }

    public required string PreprocessedHash { get; init; }

    public string? SourceHash { get; init; }

    public required RektParseOutcome ParseOutcome { get; init; }
    public required RektScanConfidence Confidence { get; init; }
    public required DateTime ParsedAtUtc { get; init; }

    public IReadOnlyList<string> Warnings { get; init; } = Array.Empty<string>();

    public IReadOnlyDictionary<string, string> DependencySnapshot { get; init; }
        = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
}
