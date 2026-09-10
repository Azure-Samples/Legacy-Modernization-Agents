namespace McpChatWeb.Services;

public static class ParseFidelity
{
    public const string Full = "full";
    public const string Partial = "partial";
    public const string DepsOnly = "deps-only";
    public const string Failed = "failed";
    public const string NotParsed = "not-parsed";
}

public static class FidelitySources
{
    public const string ScanCache = "scan-cache";
    public const string Facts = "facts";
    public const string Artifacts = "artifacts";
    public const string None = "none";
}

public sealed record RektProgramRecord(
    string Basename,
    string RelativePath,
    int LinesOfCode,
    bool IsCopybook,
    bool HasFacts,
    int FactsConfidence,
    int FactsWarnings,
    IReadOnlyList<string> Copybooks,
    IReadOnlyList<string> Callees,
    IReadOnlyList<string> Callers,
    string ParseFidelity,
    string FidelitySource,
    bool HasReport,
    bool HasDepsOnly,
    bool AmbiguousBasename,
    string? ReportDirectory,
    string? ScanOutcome,
    DateTime? ScanParsedAtUtc)
{
    public string Stem => Path.GetFileNameWithoutExtension(Basename);
}

public sealed record RektEstate(
    string RepoRoot,
    string SourceRoot,
    string RektDir,
    IReadOnlyList<RektProgramRecord> Programs,
    IReadOnlyList<MissingCopybookRow> MissingCopybooks,
    string? Note);

public sealed record MissingCopybookRow(string Copybook, IReadOnlyList<string> ReferencedBy);
