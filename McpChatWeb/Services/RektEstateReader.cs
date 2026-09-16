using System.Text.Json;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using CobolToQuarkusMigration.Agents.Infrastructure.RektCache;
using CobolToQuarkusMigration.Cli;
using CobolToQuarkusMigration.Helpers;

namespace McpChatWeb.Services;

public sealed class RektEstateReader
{
    private readonly ILogger<RektEstateReader> _logger;
    private readonly SemaphoreSlim _cacheGate = new(1, 1);
    private RektEstate? _cached;
    private string? _cachedStamp;

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

    // Path.Combine, not Path.Join: COBOL_SOURCE_FOLDER may be an absolute path to an estate
    // outside the repository, and letting a rooted value win is the intended behaviour here.
    public string SourceRoot => Path.Combine(RepoRoot, SourceFolderName);

    public string RektDir => Path.Join(RepoRoot, "output", "rekt");

    public string ScanCacheDbPath =>
        Environment.GetEnvironmentVariable("REKT_SCAN_DB") is { Length: > 0 } db
            ? db
            : Path.Join(RepoRoot, RektScanCacheCommand.DefaultDbPath);

    private static string ResolveRepoRoot()
    {
        var envRoot = Environment.GetEnvironmentVariable("REPO_ROOT");
        if (!string.IsNullOrEmpty(envRoot) && Directory.Exists(envRoot)) return envRoot;

        var dir = new DirectoryInfo(Directory.GetCurrentDirectory());
        while (dir != null && !File.Exists(Path.Join(dir.FullName, "doctor.sh")))
            dir = dir.Parent;
        return dir?.FullName ?? Directory.GetCurrentDirectory();
    }

    // One dashboard load calls four endpoints, and each one walked the whole estate: without
    // facts every program was re-read to count lines and re-scanned for COPY statements.
    // The stamp is recomputed each call, which is one directory walk instead of four full reads.
    public async Task<RektEstate> ReadAsync(CancellationToken cancellationToken = default)
    {
        var stamp = ComputeEstateStamp();

        await _cacheGate.WaitAsync(cancellationToken).ConfigureAwait(false);
        try
        {
            if (_cached is not null && _cachedStamp == stamp) return _cached;

            var estate = await ReadUncachedAsync(cancellationToken).ConfigureAwait(false);
            _cached = estate;
            _cachedStamp = stamp;
            return estate;
        }
        finally
        {
            _cacheGate.Release();
        }
    }

    // Size and write time of every source and artifact entry. A re-parse rewrites artifacts,
    // and an edited program changes its own entry, so either invalidates the cache.
    private string ComputeEstateStamp()
    {
        var builder = new System.Text.StringBuilder();
        foreach (var root in new[] { SourceRoot, RektDir })
        {
            builder.Append(root).Append('|');
            if (!Directory.Exists(root)) continue;
            try
            {
                foreach (var entry in Directory
                    .EnumerateFileSystemEntries(root, "*", SearchOption.AllDirectories)
                    .OrderBy(p => p, StringComparer.Ordinal))
                {
                    var info = new FileInfo(entry);
                    builder.Append(entry).Append(':')
                        .Append(info.Exists ? info.Length : -1).Append(':')
                        .Append(info.LastWriteTimeUtc.Ticks).Append('|');
                }
            }
            catch (Exception ex) when (ex is IOException or UnauthorizedAccessException or System.Security.SecurityException)
            {
                // An unreadable tree cannot be stamped reliably, so fall back to always reloading.
                // Narrow deliberately: anything else is a defect and should surface, not be
                // silently downgraded to a cache miss.
                _logger.LogDebug(ex, "Could not stamp {Root}; estate cache disabled for this call.", root);
                return Guid.NewGuid().ToString();
            }
        }

        var db = ScanCacheDbPath;
        if (File.Exists(db))
        {
            var info = new FileInfo(db);
            builder.Append(db).Append(':').Append(info.Length).Append(':').Append(info.LastWriteTimeUtc.Ticks);
        }

        return StableDigest(builder.ToString());
    }

    private static string StableDigest(string value)
    {
        const ulong offset = 14695981039346656037;
        const ulong prime = 1099511628211;
        var hash = offset;
        foreach (var b in System.Text.Encoding.UTF8.GetBytes(value))
        {
            hash ^= b;
            hash *= prime;
        }
        return hash.ToString("x16");
    }

    private async Task<RektEstate> ReadUncachedAsync(CancellationToken cancellationToken)
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
            var reportDir = ResolveReportDirectory(rektDir, relativePath, basename, stem, ambiguous);
            var depsPath = ResolveDependencyFile(rektDir, relativePath, basename, stem, ambiguous);

            RektScanEntry? scanEntry = null;
            if (!ambiguous) scanEntries.TryGetValue(basename, out scanEntry);

            var (fidelity, fidelitySource) = ResolveFidelity(
                scanEntry, facts, reportDir, depsPath is not null);

            programs.Add(new RektProgramRecord(
                Basename: basename,
                RelativePath: relativePath,
                LinesOfCode: facts?.Summary.Loc ?? CountLines(Path.Join(sourceRoot, SourcePathHelper.ToOsRelativePath(relativePath))),
                IsCopybook: facts?.Summary.IsCopybook ?? SourceTypeRegistry.IsCopybook(relativePath),
                HasFacts: facts is not null,
                FactsConfidence: (int)(facts?.Confidence ?? FactConfidence.None),
                FactsWarnings: facts?.Warnings.Count ?? 0,
                // Each source is tried until one yields something. An empty list is treated as
                // absent rather than as an answer: the fact extractor records copybooksUsed as
                // empty for programs it parsed with stubs, and taking that at face value reports
                // "no copybooks" for a program with fifteen COPY statements — which reads as a
                // program that is safe to convert alone.
                Copybooks: FirstNonEmpty(
                    facts?.Data.CopybooksUsed.ToList(),
                    ReadDependencies(depsPath, copybooks: true),
                    ReadCopyStatements(sourceRoot, relativePath)),
                Callees: FirstNonEmpty(
                    facts?.Callees.ToList(),
                    ReadDependencies(depsPath, copybooks: false)),
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
        catch (Exception ex) when (ex is IOException or UnauthorizedAccessException or System.Security.SecurityException
            or Microsoft.Data.Sqlite.SqliteException)
        {
            _logger.LogWarning(ex, "Scan cache unreadable at {DbPath}; falling back to artifacts.", dbPath);
            return empty;
        }
    }

    private static (string Fidelity, string Source) ResolveFidelity(
        RektScanEntry? scanEntry,
        ProgramFacts? facts,
        string? reportDir,
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

        // Graded by content rather than presence. Still never Full: a stub-backed parse writes
        // the same directories as a clean one, so only a measured outcome can establish that.
        // A report without a flow AST is a degenerate parse, which presence alone overstates.
        if (reportDir is not null)
        {
            return HasFlowAstContent(reportDir)
                ? (ParseFidelity.Partial, FidelitySources.Artifacts)
                : (ParseFidelity.DepsOnly, FidelitySources.Artifacts);
        }

        if (hasDeps) return (ParseFidelity.DepsOnly, FidelitySources.Artifacts);
        return (ParseFidelity.NotParsed, FidelitySources.None);
    }

    private static bool HasFlowAstContent(string reportDir)
    {
        try
        {
            var flowAstDir = Path.Join(reportDir, "flow_ast");
            return Directory.Exists(flowAstDir) && Directory.EnumerateFiles(flowAstDir, "*.json").Any();
        }
        // An unreadable directory is indistinguishable from an absent flow AST, and both mean
        // the same thing here. Anything else is a defect and must not be reported as fidelity.
        catch (Exception ex) when (ex is IOException or UnauthorizedAccessException or System.Security.SecurityException)
        {
            return false;
        }
    }

    private static ProgramFacts? TryLoadFacts(string rektDir, string relativePath)
    {
        if (!Directory.Exists(rektDir)) return null;
        try { return ProgramFactsArtifactLocator.TryLoad(rektDir, relativePath); }
        // Unreadable or malformed facts are indistinguishable from absent ones here. Anything
        // else is a defect, and must not be reported as a program lacking facts.
        catch (Exception ex) when (ex is IOException or UnauthorizedAccessException
            or System.Security.SecurityException or JsonException)
        {
            return null;
        }
    }

    // Mirrors the CLI's REKT context loader, so both agree which layout belongs to a program.
    // When the basename is ambiguous only the source-relative layout is trusted: a flat artifact
    // cannot say which of the same-named sources produced it, and guessing gives both the wrong facts.
    private static string? ResolveReportDirectory(
        string rektDir, string relativePath, string basename, string stem, bool ambiguous)
    {
        if (!Directory.Exists(rektDir)) return null;

        var normalized = SourcePathHelper.NormalizeRelativePath(relativePath);
        var candidates = ambiguous
            ? new[] { normalized + ".report" }
            : new[]
            {
                normalized + ".report",
                basename + ".report",
                $"{stem}.cbl.report",
                $"{stem}.report",
                $"{stem}.CBL.report",
            };

        foreach (var candidate in candidates.Distinct(StringComparer.OrdinalIgnoreCase))
        {
            var full = Path.Join(rektDir, SourcePathHelper.ToOsRelativePath(candidate));
            if (Directory.Exists(full)) return full;
        }
        return null;
    }

    private static string? ResolveDependencyFile(
        string rektDir, string relativePath, string basename, string stem, bool ambiguous)
    {
        if (!Directory.Exists(rektDir)) return null;

        var normalized = SourcePathHelper.NormalizeRelativePath(relativePath);
        var candidates = new List<string>
        {
            Path.Join(rektDir, SourcePathHelper.ToOsRelativePath($"{normalized}-deps.json")),
        };

        if (!ambiguous)
        {
            candidates.Add(Path.Join(rektDir, SourcePathHelper.ToOsRelativePath($"{basename}-deps.json")));
            candidates.Add(Path.Join(rektDir, $"{stem}-deps.json"));
            candidates.Add(Path.Join(rektDir, $"{stem}.cbl-deps.json"));
        }

        // Safe even when ambiguous: the report directory itself was resolved source-relative.
        var reportDir = ResolveReportDirectory(rektDir, relativePath, basename, stem, ambiguous);
        if (reportDir is not null)
        {
            candidates.Add(Path.Join(reportDir, $"{basename}-deps.json"));
            candidates.Add(Path.Join(reportDir, $"{stem}-deps.json"));
            candidates.Add(Path.Join(reportDir, $"{stem}.cbl-deps.json"));
        }

        return candidates.FirstOrDefault(File.Exists);
    }

    // Line format: COPYBOOK\treferenced by: A.cbl, B.cbl
    // Returns the first candidate that actually carries values. Null and empty are both
    // treated as "this source did not know", so a later source still gets a chance.
    private static List<string> FirstNonEmpty(params List<string>?[] candidates)
    {
        foreach (var candidate in candidates)
        {
            if (candidate is { Count: > 0 })
            {
                return candidate;
            }
        }

        return new List<string>();
    }

    public IReadOnlyList<MissingCopybookRow> ReadMissingCopybooks()
    {
        var path = Path.Join(RektDir, "missing-copybooks.txt");
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
        catch (Exception ex) when (ex is IOException or UnauthorizedAccessException
            or System.Security.SecurityException)
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
        catch (Exception ex) when (ex is IOException or UnauthorizedAccessException or System.Security.SecurityException) { return 0; }
    }

    // Fallback for when REKT produced no dependency export. Names are kept as written;
    // the trailing statement period is not part of the name.
    private static List<string> ReadCopyStatements(string sourceRoot, string relativePath)
    {
        var path = Path.Join(sourceRoot, SourcePathHelper.ToOsRelativePath(relativePath));
        if (!File.Exists(path)) return new List<string>();
        try
        {
            return CopyStatementRegex.Matches(File.ReadAllText(path))
                .Select(m => m.Groups[1].Value.TrimEnd('.'))
                .Where(name => name.Length > 0)
                .Distinct(StringComparer.OrdinalIgnoreCase)
                .ToList();
        }
        catch (Exception ex) when (ex is IOException or UnauthorizedAccessException or System.Security.SecurityException) { return new List<string>(); }
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
        catch (Exception ex) when (ex is IOException or UnauthorizedAccessException or System.Security.SecurityException or JsonException) { return null; }
    }

    internal static readonly System.Text.RegularExpressions.Regex CopyStatementRegex = new(
        @"^\s*COPY\s+([A-Z0-9$@#\-_]+)",
        System.Text.RegularExpressions.RegexOptions.IgnoreCase
        | System.Text.RegularExpressions.RegexOptions.Multiline
        | System.Text.RegularExpressions.RegexOptions.Compiled);
}
