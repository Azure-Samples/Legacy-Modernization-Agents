using System.Text.Json;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using CobolToQuarkusMigration.Agents.Infrastructure.RektCache;
using CobolToQuarkusMigration.Cli;
using CobolToQuarkusMigration.Helpers;

namespace McpChatWeb.Services;

public sealed class RektEstateReader
{
    private readonly ILogger<RektEstateReader> _logger;

    // repoRoot is supplied by tests so a synthetic estate can be read without
    // mutating process-global environment variables.
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

        // Copybooks are enumerated too, or every COPY edge would be reported as a gap.
        var relativePaths = SourcePathHelper.EnumerateProgramRelativePaths(sourceRoot)
            .Concat(EnumerateCopybookRelativePaths(sourceRoot))
            .Where(path => !SourceTypeRegistry.IsScratchPath(path))
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

        // A basename mapping to several files defeats every basename-keyed lookup, so track
        // it and report the ambiguity rather than attributing one file's state to another.
        var basenameCounts = relativePaths
            .GroupBy(Path.GetFileName, StringComparer.OrdinalIgnoreCase)
            .ToDictionary(g => g.Key!, g => g.Count(), StringComparer.OrdinalIgnoreCase);

        var scanEntries = await LoadScanEntriesAsync(
            basenameCounts.Keys, cancellationToken).ConfigureAwait(false);

        var rektDir = RektDir;
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

        // Artifact presence proves a parse ran, not that it succeeded: a stub-backed parse
        // emits a report directory indistinguishable from a clean one. Never infer Full.
        if (hasReport) return (ParseFidelity.Partial, FidelitySources.Artifacts);
        if (hasDeps) return (ParseFidelity.DepsOnly, FidelitySources.Artifacts);
        return (ParseFidelity.NotParsed, FidelitySources.None);
    }

    private static ProgramFacts? TryLoadFacts(string rektDir, string relativePath)
    {
        if (!Directory.Exists(rektDir)) return null;
        try { return ProgramFactsArtifactLocator.TryLoad(rektDir, relativePath); }
        catch { return null; }
    }

    // Mirrors the CLI's REKT context loader, so both agree which layout belongs to a program.
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

    // Line format: COPYBOOK\treferenced by: A.cbl, B.cbl
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

    // Counts line feeds rather than File.ReadAllLines: the corpus contains unpaired CR
    // and LF characters, which .NET's universal-newline handling would double-count.
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

    // Fallback for when REKT produced no dependency export. Names are kept as written;
    // the trailing statement period is not part of the name.
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

    // Returns null when no dependency export exists, so callers can distinguish
    // "no artifact" from "artifact says none".
    private static List<string>? ReadDependencies(string? depsPath, bool copybooks)
    {
        if (depsPath is null || !File.Exists(depsPath)) return null;
        try
        {
            using var doc = JsonDocument.Parse(File.ReadAllText(depsPath));
            if (!doc.RootElement.TryGetProperty("dependencies", out var deps)
                || deps.ValueKind != JsonValueKind.Array)
                return null;

            // Names are preserved as emitted: a copybook name must match a file on disk,
            // so case-folding would break the match. Comparisons are case-insensitive.
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
