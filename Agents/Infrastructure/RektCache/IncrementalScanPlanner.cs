using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Agents.Infrastructure;
using CobolToQuarkusMigration.Helpers;

namespace CobolToQuarkusMigration.Agents.Infrastructure.RektCache;

/// <summary>Why a program is in the parse list.</summary>
public enum ScanReason
{
    NotCached,                  // No previous entry
    SourceChanged,              // Preprocessed bytes hash differs
    DependencyChanged,          // A copybook in the snapshot has a new hash
    DependencyMissingFromCache, // A copybook referenced today wasn't snapshotted before
    DependencyMissingFromCorpus,// A copybook referenced today is not present on disk
    PreviousParseLowConfidence, // Previous parse was DepsOnly/RawAst; force a retry
    SchemaOrIdentityMismatch,   // Cache returned no entry due to semantic/identity version
}

/// <summary>A single planner decision for one program.</summary>
public sealed record ScanDecision(
    string Basename,
    bool MustParse,
    ScanReason? Reason,
    IReadOnlyList<string> MissingCopybooks,
    IReadOnlyDictionary<string, string> DependencySnapshot,
    string PreprocessedHash);

/// <summary>The planner's output for one run.</summary>
public sealed record ScanPlan(
    IReadOnlyList<ScanDecision> ToParse,
    IReadOnlyList<ScanDecision> ToSkip)
{
    public int TotalConsidered => ToParse.Count + ToSkip.Count;
}

/// <summary>
/// Builds a <see cref="ScanPlan"/> from a corpus of preprocessed files plus the
/// existing scan cache. Pure decision logic — no filesystem walks, no smojol
/// calls, no doctor.sh integration.
/// </summary>
/// <remarks>
/// The planner is deliberately stateless. Callers:
/// <list type="number">
///   <item>Build a <see cref="RektCopybookGraph"/> by adding each preprocessed file.</item>
///   <item>Call <see cref="PlanAsync"/> with the cache.</item>
///   <item>Iterate <see cref="ScanPlan.ToParse"/>, invoking smojol per file.</item>
///   <item>After each parse, call <see cref="RecordParseAsync"/> with the outcome.</item>
/// </list>
/// </remarks>
public sealed class IncrementalScanPlanner
{
    private readonly IRektScanCache _cache;
    private readonly RektCopybookGraph _graph;
    private readonly string _identityScheme;
    private readonly ILogger? _logger;

    public const string LogEventName = "RektScanCache";

    public IncrementalScanPlanner(
        IRektScanCache cache,
        RektCopybookGraph graph,
        string identityScheme,
        ILogger? logger = null)
    {
        _cache = cache;
        _graph = graph;
        _identityScheme = identityScheme;
        _logger = logger;
    }

    /// <summary>
    /// Produces the parse/skip plan for the supplied program basenames.
    /// Order of the plan mirrors the input order so downstream parsing is deterministic.
    /// </summary>
    public async Task<ScanPlan> PlanAsync(
        IReadOnlyList<string> programBasenames,
        CancellationToken cancellationToken = default)
    {
        var existing = await _cache.GetManyAsync(programBasenames, _identityScheme, cancellationToken);

        var toParse = new List<ScanDecision>(programBasenames.Count);
        var toSkip = new List<ScanDecision>(programBasenames.Count);

        foreach (var basename in programBasenames)
        {
            cancellationToken.ThrowIfCancellationRequested();

            var currentHash = _graph.GetHash(basename)
                ?? throw new InvalidOperationException(
                    $"RektCopybookGraph has no hash for '{basename}'. Add the file before planning.");
            var currentSnapshot = _graph.BuildDependencySnapshot(basename);
            var missing = _graph.GetMissingCopybooks(basename).ToList();

            existing.TryGetValue(basename, out var entry);

            ScanReason? reason = entry switch
            {
                null => ScanReason.NotCached,
                { PreprocessedHash: var h } when !string.Equals(h, currentHash, StringComparison.Ordinal)
                    => ScanReason.SourceChanged,
                { Confidence: RektScanConfidence.Low or RektScanConfidence.None }
                    => ScanReason.PreviousParseLowConfidence,
                _ => DependencyMismatch(entry!, currentSnapshot)
            };

            if (reason is not null)
            {
                var decision = new ScanDecision(basename, MustParse: true, reason,
                    missing, currentSnapshot, currentHash);
                LogDecision(decision);
                toParse.Add(decision);
            }
            else
            {
                var decision = new ScanDecision(basename, MustParse: false, Reason: null,
                    missing, currentSnapshot, currentHash);
                LogDecision(decision);
                toSkip.Add(decision);
            }
        }

        return new ScanPlan(toParse, toSkip);
    }

    /// <summary>
    /// Computes the transitive closure of programs reachable from <paramref name="seeds"/>
    /// through the copybook graph. Used by <c>--program X</c> to determine the
    /// minimum set of files whose copybooks must also be scanned/hashed even when
    /// only one program is being analysed.
    /// </summary>
    public IReadOnlySet<string> ComputeDependencyClosure(IEnumerable<string> seeds)
    {
        // For incremental scans, the closure is "every copybook the seeds use".
        // Programs do not typically depend on other programs at parse time — CALL
        // resolution is a runtime concern smojol handles per-file. So the closure
        // is purely the union of dependency snapshots.
        var closure = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        foreach (var seed in seeds)
        {
            closure.Add(seed);
            foreach (var dep in _graph.BuildDependencySnapshot(seed).Keys)
            {
                closure.Add(dep);
            }
        }
        return closure;
    }

    /// <summary>
    /// Records the outcome of a parse against the cache. Idempotent; safe to call
    /// even when the cache is unreachable (the underlying store fails open).
    /// </summary>
    public async Task RecordParseAsync(
        ScanDecision decision,
        RektParseOutcome outcome,
        IReadOnlyList<string>? warnings = null,
        string? sourceHash = null,
        string? relativePath = null,
        CancellationToken cancellationToken = default)
    {
        var entry = new RektScanEntry
        {
            Basename = decision.Basename,
            IdentitySchemeVersion = _identityScheme,
            RelativePath = relativePath,
            PreprocessedHash = decision.PreprocessedHash,
            SourceHash = sourceHash,
            ParseOutcome = outcome,
            Confidence = ConfidenceFromOutcome(outcome),
            ParsedAtUtc = DateTime.UtcNow,
            Warnings = warnings ?? Array.Empty<string>(),
            DependencySnapshot = decision.DependencySnapshot,
        };
        await _cache.UpsertAsync(entry, cancellationToken);

        _logger?.LogInformation(
            "[{Event}] runId={RunId} correlationId={CorrelationId} basename={Basename} " +
            "decision=record-parse outcome={Outcome} confidence={Confidence} " +
            "preprocessedHash={HashShort} dependencyCount={Deps} " +
            "identityScheme={IdScheme} relativePath={Rel}",
            LogEventName, LlmCorrelationContext.RunId, LlmCorrelationContext.CorrelationId,
            decision.Basename, outcome, ConfidenceFromOutcome(outcome),
            Short(decision.PreprocessedHash), decision.DependencySnapshot.Count,
            _identityScheme, relativePath ?? "-");
    }

    private static ScanReason? DependencyMismatch(
        RektScanEntry entry, IReadOnlyDictionary<string, string> currentSnapshot)
    {
        // If any current dependency hash differs from the snapshot, invalidate.
        foreach (var (copybook, currentHash) in currentSnapshot)
        {
            if (!entry.DependencySnapshot.TryGetValue(copybook, out var stored))
                return ScanReason.DependencyMissingFromCache;
            if (!string.Equals(stored, currentHash, StringComparison.Ordinal))
                return ScanReason.DependencyChanged;
        }
        // Also: if a copybook in the snapshot was removed from the corpus today,
        // invalidate to force a re-parse that will surface the new missing-copybook warning.
        foreach (var copybook in entry.DependencySnapshot.Keys)
        {
            if (!currentSnapshot.ContainsKey(copybook))
                return ScanReason.DependencyMissingFromCorpus;
        }
        return null;
    }

    private static RektScanConfidence ConfidenceFromOutcome(RektParseOutcome outcome) => outcome switch
    {
        RektParseOutcome.Full       => RektScanConfidence.High,
        RektParseOutcome.NoDialect  => RektScanConfidence.Partial,
        RektParseOutcome.RawAst     => RektScanConfidence.Low,
        RektParseOutcome.DepsOnly   => RektScanConfidence.Low,
        _                           => RektScanConfidence.None,
    };

    private void LogDecision(ScanDecision d)
    {
        _logger?.LogInformation(
            "[{Event}] runId={RunId} correlationId={CorrelationId} basename={Basename} " +
            "decision={Decision} reason={Reason} preprocessedHash={HashShort} " +
            "dependencyCount={Deps} missingCopybooks={Missing} identityScheme={IdScheme}",
            LogEventName, LlmCorrelationContext.RunId, LlmCorrelationContext.CorrelationId,
            d.Basename, d.MustParse ? "parse" : "skip", d.Reason?.ToString() ?? "-",
            Short(d.PreprocessedHash), d.DependencySnapshot.Count, d.MissingCopybooks.Count,
            _identityScheme);
    }

    private static string Short(string h) => h.Length <= 8 ? h : h[..8];
}
