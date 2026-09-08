using System.Text.Json;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using CobolToQuarkusMigration.Agents.Infrastructure.RektCache;
using CobolToQuarkusMigration.Cli;
using CobolToQuarkusMigration.Helpers;

namespace McpChatWeb.Services;

/// <summary>
/// Shared read path over the REKT estate: the COBOL inventory under the source
/// folder, the scan cache in <c>Data/rekt-scan.db</c>, and the artifacts under
/// <c>output/rekt</c>.
///
/// <para>
/// Read-only and fail-soft. A missing database, folder or artifact yields an
/// empty or reduced record with an explanatory note rather than an exception,
/// so the portal is usable before any scan has run.
/// </para>
///
/// <para>
/// Parse fidelity resolves in a fixed order:
/// <list type="number">
///   <item>the scan cache entry for the program — the deterministic record of
///         what the parser actually produced;</item>
///   <item>the <c>confidence</c> recorded in the program's facts artifact;</item>
///   <item>artifact presence on disk — a <c>.report</c> directory or a
///         <c>-deps.json</c> file.</item>
/// </list>
/// Each record reports which of the three answered via
/// <see cref="RektProgramRecord.FidelitySource"/>, so the UI never presents an
/// inferred value as a measured one.
/// </para>
///
/// <para>
/// Identity follows the same rules as the CLI: programs are enumerated through
/// <see cref="SourceTypeRegistry"/>, keyed by the normalised source-relative
/// path from <see cref="SourcePathHelper"/>, and facts are resolved through
/// <c>ProgramFactsArtifactLocator</c>, which refuses to guess when one basename
/// maps to several source files. Those collisions are surfaced as
/// <see cref="RektProgramRecord.AmbiguousBasename"/> instead of being silently
/// collapsed.
/// </para>
/// </summary>
public sealed class RektEstateReader
{
    private readonly ILogger<RektEstateReader> _logger;

    /// <param name="repoRoot">
    /// Overrides repository-root discovery. Left null in production; supplied by
    /// tests so they can read a synthetic estate without mutating process-global
    /// environment variables.
    /// </param>
    public RektEstateReader(ILogger<RektEstateReader> logger, string? repoRoot = null)
    {
        _logger = logger;
        RepoRoot = repoRoot ?? ResolveRepoRoot();
    }

    public string RepoRoot { get; }

    public string SourceFolderName =>
        Environment.GetEnvironmentVariable("COBOL_SOURCE_FOLDER") is { Length: > 0 } folder
            ? folder
            : "source";

    public string SourceRoot => Path.Combine(RepoRoot, SourceFolderName);

    public string RektDir => Path.Combine(RepoRoot, "output", "rekt");

    public string ScanCacheDbPath =>
        Environment.GetEnvironmentVariable("REKT_SCAN_DB") is { Length: > 0 } db
            ? db
            : Path.Combine(RepoRoot, RektScanCacheCommand.DefaultDbPath);

    private static string ResolveRepoRoot()
    {
        var envRoot = Environment.GetEnvironmentVariable("REPO_ROOT");
        if (!string.IsNullOrEmpty(envRoot) && Directory.Exists(envRoot)) return envRoot;

        var dir = new DirectoryInfo(Directory.GetCurrentDirectory());
        while (dir != null && !File.Exists(Path.Combine(dir.FullName, "doctor.sh")))
            dir = dir.Parent;
        return dir?.FullName ?? Directory.GetCurrentDirectory();
    }

    /// <summary>
    /// Build the estate snapshot every decision surface reads from.
    /// </summary>
    public async Task<RektEstate> ReadAsync(CancellationToken cancellationToken = default)
    {
        var sourceRoot = SourceRoot;
        if (!Directory.Exists(sourceRoot))
        {
            return new RektEstate(
                RepoRoot, sourceRoot, RektDir,
                Array.Empty<RektProgramRecord>(),
                Array.Empty<MissingCopybookRow>(),
                $"Source folder not found: {sourceRoot}.");
        }

        // Copybooks are enumerated alongside programs: a COPY target must be a
        // resolvable node or every copy edge would be reported as a gap in the
        // estate. Records carry IsCopybook so callers that measure parse
        // fidelity can exclude them — a copybook is never parsed on its own.
        var relativePaths = SourcePathHelper.EnumerateProgramRelativePaths(sourceRoot)
            .Concat(EnumerateCopybookRelativePaths(sourceRoot))
            .Where(IsScannableSource)
            .Distinct(StringComparer.OrdinalIgnoreCase)
            .OrderBy(path => path, StringComparer.OrdinalIgnoreCase)
            .ToList();

        if (relativePaths.Count == 0)
        {
            return new RektEstate(
                RepoRoot, sourceRoot, RektDir,
                Array.Empty<RektProgramRecord>(),
                ReadMissingCopybooks(),
                $"No COBOL programs found under {sourceRoot}.");
        }

        // A basename that maps to more than one source file cannot be resolved
        // by any basename-keyed lookup (scan cache, missing-copybooks.txt,
        // EXEC PGM=). Track them so callers can report the ambiguity instead of
        // attributing one file's state to another.
        var basenameCounts = relativePaths
            .GroupBy(Path.GetFileName, StringComparer.OrdinalIgnoreCase)
            .ToDictionary(g => g.Key!, g => g.Count(), StringComparer.OrdinalIgnoreCase);

        var scanEntries = await LoadScanEntriesAsync(
            basenameCounts.Keys, cancellationToken).ConfigureAwait(false);

        var rektDir = Path.Combine(RepoRoot, "output", "rekt");
        var programs = new List<RektProgramRecord>(relativePaths.Count);

        foreach (var relativePath in relativePaths)
        {
            cancellationToken.ThrowIfCancellationRequested();

            var basename = Path.GetFileName(relativePath);
            var stem = Path.GetFileNameWithoutExtension(basename);
            var ambiguous = basenameCounts.GetValueOrDefault(basename, 0) > 1;

            var facts = TryLoadFacts(rektDir, relativePath);
            var reportDir = ResolveReportDirectory(rektDir, relativePath, basename, stem);
            var depsPath = ResolveDependencyFile(rektDir, relativePath, basename, stem);

            // A basename-keyed cache entry cannot be trusted when the basename
            // is ambiguous — two source files would share one row.
            RektScanEntry? scanEntry = null;
            if (!ambiguous) scanEntries.TryGetValue(basename, out scanEntry);

            var (fidelity, fidelitySource) = ResolveFidelity(
                scanEntry, facts, reportDir is not null, depsPath is not null);

            programs.Add(new RektProgramRecord(
                Basename: basename,
                RelativePath: relativePath,
                LinesOfCode: facts?.Summary.Loc ?? CountLines(Path.Combine(sourceRoot, SourcePathHelper.ToOsRelativePath(relativePath))),
                IsCopybook: facts?.Summary.IsCopybook ?? SourceTypeRegistry.IsCopybook(relativePath),
                HasFacts: facts is not null,
                FactsConfidence: (int)(facts?.Confidence ?? FactConfidence.None),
                FactsWarnings: facts?.Warnings.Count ?? 0,
                Copybooks: facts?.Data.CopybooksUsed.ToList()
                    ?? ReadDependencies(depsPath, copybooks: true)
                    ?? ReadCopyStatements(sourceRoot, relativePath),
                Callees: facts?.Callees.ToList()
                    ?? ReadDependencies(depsPath, copybooks: false)
                    ?? new List<string>(),
                Callers: facts?.Callers.ToList() ?? new List<string>(),
                ParseFidelity: fidelity,
                FidelitySource: fidelitySource,
                HasReport: reportDir is not null,
                HasDepsOnly: reportDir is null && depsPath is not null,
                AmbiguousBasename: ambiguous,
                ReportDirectory: reportDir,
                ScanOutcome: scanEntry?.ParseOutcome.ToString(),
                ScanParsedAtUtc: scanEntry?.ParsedAtUtc));
        }

        string? note = null;
        if (!Directory.Exists(rektDir))
            note = $"REKT output directory not found: {rektDir}. Run ./doctor.sh rekt-full first.";
        else if (scanEntries.Count == 0 && !File.Exists(ScanCacheDbPath))
            note = $"Scan cache not found: {ScanCacheDbPath}. Parse fidelity falls back to artifacts on disk.";

        return new RektEstate(RepoRoot, sourceRoot, rektDir, programs, ReadMissingCopybooks(), note);
    }

    /// <summary>
    /// Staging, preprocessing and conversion scratch folders hold derived copies
    /// of the same programs; counting them would double the estate.
    /// </summary>
    private static bool IsScannableSource(string relativePath)
    {
        var segments = relativePath.Split('/', StringSplitOptions.RemoveEmptyEntries);
        return !segments.Any(segment =>
            segment.StartsWith(".convert-", StringComparison.Ordinal)
            || segment.Equals(".rekt-staging", StringComparison.Ordinal)
            || segment.Equals(".preprocessed", StringComparison.Ordinal));
    }

    private async Task<Dictionary<string, RektScanEntry>> LoadScanEntriesAsync(
        IReadOnlyCollection<string> basenames,
        CancellationToken cancellationToken)
    {
        var empty = new Dictionary<string, RektScanEntry>(StringComparer.OrdinalIgnoreCase);
        var dbPath = ScanCacheDbPath;
        if (!File.Exists(dbPath)) return empty;

        try
        {
            var cache = new SqliteRektScanCache(dbPath);
            var entries = await cache
                .GetManyAsync(basenames, RektScanCacheCommand.IdentityScheme, cancellationToken)
                .ConfigureAwait(false);
            return new Dictionary<string, RektScanEntry>(entries, StringComparer.OrdinalIgnoreCase);
        }
        catch (Exception ex)
        {
            _logger.LogWarning(ex, "Scan cache unreadable at {DbPath}; falling back to artifacts.", dbPath);
            return empty;
        }
    }

    private static (string Fidelity, string Source) ResolveFidelity(
        RektScanEntry? scanEntry,
        ProgramFacts? facts,
        bool hasReport,
        bool hasDeps)
    {
        if (scanEntry is not null)
        {
            var fidelity = scanEntry.ParseOutcome switch
            {
                RektParseOutcome.Full => ParseFidelity.Full,
                RektParseOutcome.NoDialect or RektParseOutcome.StubBacked => ParseFidelity.Partial,
                RektParseOutcome.RawAst => ParseFidelity.Partial,
                RektParseOutcome.DepsOnly => ParseFidelity.DepsOnly,
                _ => ParseFidelity.Failed,
            };
            return (fidelity, FidelitySources.ScanCache);
        }

        if (facts is not null)
        {
            var fidelity = facts.Confidence switch
            {
                FactConfidence.High => ParseFidelity.Full,
                FactConfidence.Partial => ParseFidelity.Partial,
                FactConfidence.Low => ParseFidelity.DepsOnly,
                _ => ParseFidelity.NotParsed,
            };
            return (fidelity, FidelitySources.Facts);
        }

        if (hasReport) return (ParseFidelity.Full, FidelitySources.Artifacts);
        if (hasDeps) return (ParseFidelity.DepsOnly, FidelitySources.Artifacts);
        return (ParseFidelity.NotParsed, FidelitySources.None);
    }

    private static ProgramFacts? TryLoadFacts(string rektDir, string relativePath)
    {
        if (!Directory.Exists(rektDir)) return null;
        try { return ProgramFactsArtifactLocator.TryLoad(rektDir, relativePath); }
        catch { return null; }
    }

    /// <summary>
    /// Mirrors the report-directory candidates the CLI's REKT context loader
    /// accepts, covering both the flat and source-relative artifact layouts.
    /// </summary>
    private static string? ResolveReportDirectory(
        string rektDir, string relativePath, string basename, string stem)
    {
        if (!Directory.Exists(rektDir)) return null;

        var normalized = SourcePathHelper.NormalizeRelativePath(relativePath);
        foreach (var candidate in new[]
                 {
                     normalized + ".report",
                     basename + ".report",
                     $"{stem}.cbl.report",
                     $"{stem}.report",
                     $"{stem}.CBL.report",
                 }.Distinct(StringComparer.OrdinalIgnoreCase))
        {
            var full = Path.Combine(rektDir, SourcePathHelper.ToOsRelativePath(candidate));
            if (Directory.Exists(full)) return full;
        }
        return null;
    }

    private static string? ResolveDependencyFile(
        string rektDir, string relativePath, string basename, string stem)
    {
        if (!Directory.Exists(rektDir)) return null;

        var normalized = SourcePathHelper.NormalizeRelativePath(relativePath);
        var candidates = new List<string>
        {
            Path.Combine(rektDir, SourcePathHelper.ToOsRelativePath($"{normalized}-deps.json")),
            Path.Combine(rektDir, SourcePathHelper.ToOsRelativePath($"{basename}-deps.json")),
            Path.Combine(rektDir, $"{stem}-deps.json"),
            Path.Combine(rektDir, $"{stem}.cbl-deps.json"),
        };

        var reportDir = ResolveReportDirectory(rektDir, relativePath, basename, stem);
        if (reportDir is not null)
        {
            candidates.Add(Path.Combine(reportDir, $"{basename}-deps.json"));
            candidates.Add(Path.Combine(reportDir, $"{stem}-deps.json"));
            candidates.Add(Path.Combine(reportDir, $"{stem}.cbl-deps.json"));
        }

        return candidates.FirstOrDefault(File.Exists);
    }

    /// <summary>
    /// <c>output/rekt/missing-copybooks.txt</c> lines are
    /// <c>COPYBOOK\treferenced by: A.cbl, B.cbl</c>.
    /// </summary>
    public IReadOnlyList<MissingCopybookRow> ReadMissingCopybooks()
    {
        var path = Path.Combine(RektDir, "missing-copybooks.txt");
        if (!File.Exists(path)) return Array.Empty<MissingCopybookRow>();

        var rows = new List<MissingCopybookRow>();
        try
        {
            foreach (var raw in File.ReadLines(path))
            {
                var line = raw.Trim();
                if (line.Length == 0 || line.StartsWith('#')) continue;

                var tab = line.IndexOf('\t');
                if (tab < 0) continue;

                var copybook = line[..tab].Trim();
                if (copybook.Length == 0) continue;

                var rest = line[(tab + 1)..].Trim();
                const string prefix = "referenced by:";
                var index = rest.IndexOf(prefix, StringComparison.OrdinalIgnoreCase);
                var references = index >= 0
                    ? rest[(index + prefix.Length)..]
                        .Split(',', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries)
                        .Distinct(StringComparer.OrdinalIgnoreCase)
                        .ToList()
                    : new List<string>();

                rows.Add(new MissingCopybookRow(copybook, references));
            }
        }
        catch (Exception ex)
        {
            _logger.LogWarning(ex, "Could not read {Path}.", path);
        }
        return rows;
    }

    /// <summary>
    /// Counts line feeds rather than using <c>File.ReadAllLines</c>. The COBOL
    /// corpus contains files with unpaired CR and LF characters, which .NET's
    /// universal-newline handling would split on independently and double-count.
    /// </summary>
    private static int CountLines(string path)
    {
        try
        {
            if (!File.Exists(path)) return 0;
            var bytes = File.ReadAllBytes(path);
            var lines = bytes.Count(b => b == (byte)'\n');
            if (bytes.Length > 0 && bytes[^1] != (byte)'\n') lines++;
            return lines;
        }
        catch { return 0; }
    }

    /// <summary>
    /// Last-resort copybook discovery by scanning COPY statements when REKT
    /// produced no dependency export. Names are kept as written in the source;
    /// the trailing statement period is not part of the name.
    /// </summary>
    private static List<string> ReadCopyStatements(string sourceRoot, string relativePath)
    {
        var path = Path.Combine(sourceRoot, SourcePathHelper.ToOsRelativePath(relativePath));
        if (!File.Exists(path)) return new List<string>();
        try
        {
            return CopyStatementRegex.Matches(File.ReadAllText(path))
                .Select(m => m.Groups[1].Value.TrimEnd('.'))
                .Where(name => name.Length > 0)
                .Distinct(StringComparer.OrdinalIgnoreCase)
                .ToList();
        }
        catch { return new List<string>(); }
    }

    private static IEnumerable<string> EnumerateCopybookRelativePaths(string root) =>
        SourceTypeRegistry.EnumerateCopybookFiles(root)
            .Select(path => SourcePathHelper.NormalizeRelativePath(Path.GetRelativePath(root, path)));

    /// <summary>
    /// A REKT <c>-deps.json</c> holds a flat <c>dependencies</c> array of
    /// <c>{ "name": … }</c> entries where a <c>.cpy</c> suffix marks a copybook
    /// and everything else is a CALL target. Returns null when no dependency
    /// export exists, so callers can distinguish "no artifact" from
    /// "artifact says none".
    /// </summary>
    private static List<string>? ReadDependencies(string? depsPath, bool copybooks)
    {
        if (depsPath is null || !File.Exists(depsPath)) return null;
        try
        {
            using var doc = JsonDocument.Parse(File.ReadAllText(depsPath));
            if (!doc.RootElement.TryGetProperty("dependencies", out var deps)
                || deps.ValueKind != JsonValueKind.Array)
                return null;

            // Names are preserved as REKT emitted them. A copybook name is a
            // filename that has to match a file on disk, and case-folding it
            // would both break that match and display a name the source never
            // contained. Comparisons are case-insensitive instead.
            return deps.EnumerateArray()
                .Select(d => d.TryGetProperty("name", out var n) && n.ValueKind == JsonValueKind.String
                    ? n.GetString()
                    : null)
                .Where(name => !string.IsNullOrEmpty(name))
                .Select(name => name!)
                .Where(name => name.EndsWith(".cpy", StringComparison.OrdinalIgnoreCase) == copybooks)
                .Distinct(StringComparer.OrdinalIgnoreCase)
                .ToList();
        }
        catch { return null; }
    }

    internal static readonly System.Text.RegularExpressions.Regex CopyStatementRegex = new(
        @"^\s*COPY\s+([A-Z0-9$@#\-_]+)",
        System.Text.RegularExpressions.RegexOptions.IgnoreCase
        | System.Text.RegularExpressions.RegexOptions.Multiline
        | System.Text.RegularExpressions.RegexOptions.Compiled);
}

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
