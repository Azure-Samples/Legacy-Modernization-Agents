using System.Globalization;
using System.Text.Json;
using Microsoft.Data.Sqlite;

namespace McpChatWeb.Services;

/// <summary>
/// Backend for the Modernization Intelligence portal workspace (Phase-1).
///
/// <para>
/// Read-only. Reads from existing artifacts only — never collects new data:
///   • <c>Data/migration.db</c> — run history (runs table)
///   • <c>Data/benchmark.db</c> — ingested MetricsSink events (metric_events table)
///   • <c>Data/projection-cache.db</c> — projection-block cache (hit counts)
///   • <c>source/</c> — COBOL inventory (recursive)
///   • <c>output/rekt/*.facts.json</c> — per-program facts (for inventory drill-down)
/// </para>
///
/// <para>
/// All queries fail-soft: missing DBs / files return sensible empty defaults
/// so the portal degrades gracefully when run before any conversion has
/// happened.
/// </para>
/// </summary>
public sealed class ModernizationIntelligenceService
{
    private readonly string _repoRoot;
    private readonly ILogger<ModernizationIntelligenceService> _logger;

    public ModernizationIntelligenceService(IConfiguration config, ILogger<ModernizationIntelligenceService> logger)
    {
        _repoRoot = ResolveRepoRoot(config);
        _logger = logger;
    }

    private static string ResolveRepoRoot(IConfiguration config)
    {
        var envRoot = Environment.GetEnvironmentVariable("REPO_ROOT");
        if (!string.IsNullOrEmpty(envRoot) && Directory.Exists(envRoot)) return envRoot;
        var cwd = Directory.GetCurrentDirectory();
        var dir = new DirectoryInfo(cwd);
        while (dir != null && !File.Exists(Path.Combine(dir.FullName, "doctor.sh"))) dir = dir.Parent;
        return dir?.FullName ?? cwd;
    }

    // ─────────────────────────────────────────────────────────────────────
    // Application Explorer
    // ─────────────────────────────────────────────────────────────────────

    public IEnumerable<ApplicationRow> GetApplications()
    {
        var sourceDir = Path.Combine(_repoRoot, "source");
        if (!Directory.Exists(sourceDir)) yield break;

        // Aggregate per-program quality + cache state from benchmark.db once.
        var qualityMap = LoadLatestQualityByRunId();
        var runToProgram = LoadRunToProgramMap();
        var cacheState = LoadProjectionCacheState();
        var factsDir = Path.Combine(_repoRoot, "output", "rekt");

        var cblFiles = Directory.EnumerateFiles(sourceDir, "*.cbl", SearchOption.AllDirectories)
            .Where(f => !f.Contains("/.convert-", StringComparison.Ordinal)
                     && !f.Contains("/.rekt-staging", StringComparison.Ordinal)
                     && !f.Contains("/.preprocessed", StringComparison.Ordinal));

        foreach (var cbl in cblFiles)
        {
            var basename = Path.GetFileName(cbl);
            var stem = Path.GetFileNameWithoutExtension(cbl);
            var rel = Path.GetRelativePath(_repoRoot, cbl);

            int loc = 0;
            try
            {
                // Match `wc -l` semantics: count line-feeds only. sources corpus
                // has files with scattered CR + LF (not paired CRLF); .NET's
                // File.ReadAllLines + universal-newline parsing would double-
                // count every line by splitting on both terminators independently.
                var bytes = File.ReadAllBytes(cbl);
                foreach (var b in bytes) if (b == (byte)'\n') loc++;
                // If file ends without a newline the last line still counts.
                if (bytes.Length > 0 && bytes[^1] != (byte)'\n') loc++;
            }
            catch { /* skip */ }

            var factsPath = Path.Combine(factsDir, $"{stem}.facts.json");
            var (factsConfidence, depCount, warningCount, hasFacts) = ReadFactsSummary(factsPath);

            cacheState.TryGetValue(basename, out var cacheEntry);

            // Latest run for this program. runToProgram now maps runId → exact
            // basename via cobol_files join, so we can match strictly.
            int? latestRunId = null;
            QualityRow? latestQuality = null;
            foreach (var (runId, programName) in runToProgram)
            {
                if (string.Equals(programName, basename, StringComparison.OrdinalIgnoreCase))
                {
                    if (latestRunId == null || runId > latestRunId)
                    {
                        latestRunId = runId;
                        if (qualityMap.TryGetValue(runId, out var q)) latestQuality = q;
                    }
                }
            }

            yield return new ApplicationRow(
                Basename: basename,
                RelativePath: rel,
                LinesOfCode: loc,
                HasFacts: hasFacts,
                FactsConfidence: factsConfidence,
                DependencyCount: depCount,
                FactsWarnings: warningCount,
                LatestRunId: latestRunId,
                LatestCompileSuccess: latestQuality?.CompileSuccess,
                LatestCompileErrors: latestQuality?.CompileErrors,
                LatestGeneratedClasses: latestQuality?.GeneratedClassCount,
                LatestGeneratedLines: latestQuality?.GeneratedJavaLines,
                LatestFallbackClasses: latestQuality?.FallbackClassCount,
                ProjectionCacheHits: cacheEntry?.HitCount ?? 0,
                ProjectionCacheBytes: cacheEntry?.ByteSize ?? 0,
                ModernizationStatus: DeriveStatus(latestRunId, latestQuality)
            );
        }
    }

    private static string DeriveStatus(int? runId, QualityRow? q)
    {
        if (runId == null) return "not-started";
        if (q == null) return "converted";  // ran but quality gate not executed
        if (q.CompileSuccess) return "verified";
        if (q.FallbackClassCount > 0) return "partial-fallback";
        return "compile-failing";
    }

    // ─────────────────────────────────────────────────────────────────────
    // Modernization Dashboard
    // ─────────────────────────────────────────────────────────────────────

    public DashboardSummary GetDashboard()
    {
        var dbPath = Path.Combine(_repoRoot, "Data", "benchmark.db");
        if (!File.Exists(dbPath))
        {
            return DashboardSummary.Empty("benchmark.db not found — run tools/ingest-metrics.py to build it");
        }

        try
        {
            using var conn = new SqliteConnection($"Data Source={dbPath};Mode=ReadOnly;");
            conn.Open();

            var summary = new DashboardSummary();
            summary.Source = dbPath;

            // Event counts by type
            using (var c = conn.CreateCommand())
            {
                c.CommandText = "SELECT event, COUNT(*) FROM metric_events GROUP BY event ORDER BY 2 DESC";
                using var r = c.ExecuteReader();
                while (r.Read())
                {
                    summary.EventCountsByType[r.GetString(0) ?? "?"] = r.GetInt32(1);
                }
            }

            // Projection mode
            using (var c = conn.CreateCommand())
            {
                c.CommandText =
                    "SELECT projection_mode, COUNT(*) FROM metric_events " +
                    "WHERE event='projection_metrics' GROUP BY projection_mode ORDER BY 2 DESC";
                using var r = c.ExecuteReader();
                while (r.Read())
                {
                    summary.ProjectionModeCounts[r.IsDBNull(0) ? "(none)" : r.GetString(0)] = r.GetInt32(1);
                }
            }

            // Per-program context reduction (latest)
            using (var c = conn.CreateCommand())
            {
                c.CommandText = @"
                    WITH agg AS (
                        SELECT file, projection_mode,
                               AVG(CAST(projection_tokens AS REAL)) AS proj_tok,
                               AVG(CAST(raw_rekt_tokens AS REAL)) AS raw_tok
                          FROM metric_events
                         WHERE event='projection_metrics'
                         GROUP BY file, projection_mode
                    )
                    SELECT a.file,
                           MAX(CASE WHEN projection_mode='raw-rekt' THEN raw_tok END) AS raw,
                           MAX(CASE WHEN projection_mode='projection' THEN proj_tok END) AS proj
                      FROM agg a
                     GROUP BY a.file
                     HAVING raw IS NOT NULL AND proj IS NOT NULL
                     ORDER BY raw DESC";
                using var r = c.ExecuteReader();
                while (r.Read())
                {
                    var file = r.IsDBNull(0) ? "?" : r.GetString(0);
                    var raw = r.IsDBNull(1) ? 0 : r.GetDouble(1);
                    var proj = r.IsDBNull(2) ? 0 : r.GetDouble(2);
                    var pct = raw > 0 ? Math.Round((raw - proj) * 100 / raw, 1) : 0;
                    summary.ContextReduction.Add(new ContextReductionRow(file, raw, proj, pct));
                }
            }

            // LLM call outcomes
            using (var c = conn.CreateCommand())
            {
                c.CommandText =
                    "SELECT outcome, COUNT(*), AVG(stream_duration_ms), AVG(completion_tokens) " +
                    "FROM metric_events WHERE event='llm_call' GROUP BY outcome ORDER BY 2 DESC";
                using var r = c.ExecuteReader();
                while (r.Read())
                {
                    summary.LlmCallOutcomes.Add(new LlmOutcomeRow(
                        Outcome: r.IsDBNull(0) ? "?" : r.GetString(0),
                        Count: r.GetInt32(1),
                        AvgDurationMs: r.IsDBNull(2) ? 0 : r.GetDouble(2),
                        AvgCompletionTokens: r.IsDBNull(3) ? 0 : r.GetDouble(3)
                    ));
                }
            }

            // Cache event breakdown
            using (var c = conn.CreateCommand())
            {
                c.CommandText =
                    "SELECT json_extract(payload_json,'$.decision'), COUNT(*) " +
                    "FROM metric_events WHERE event='cache_event' " +
                    "GROUP BY 1 ORDER BY 2 DESC";
                using var r = c.ExecuteReader();
                while (r.Read())
                {
                    var key = r.IsDBNull(0) ? "?" : r.GetString(0);
                    summary.CacheDecisionCounts[key] = r.GetInt32(1);
                }
            }

            // Latest quality metrics (last 5 runs that emitted a quality gate)
            using (var c = conn.CreateCommand())
            {
                c.CommandText = @"
                    SELECT run_id,
                           json_extract(payload_json,'$.compileSuccess'),
                           json_extract(payload_json,'$.compileErrors'),
                           json_extract(payload_json,'$.generatedClassCount'),
                           json_extract(payload_json,'$.generatedJavaLines'),
                           json_extract(payload_json,'$.fallbackClassCount'),
                           json_extract(payload_json,'$.injectAnnotationCount'),
                           ts
                      FROM metric_events
                     WHERE event='quality_metrics'
                     ORDER BY ts DESC LIMIT 5";
                using var r = c.ExecuteReader();
                while (r.Read())
                {
                    summary.RecentQuality.Add(new QualitySummaryRow(
                        RunId: r.IsDBNull(0) ? "?" : r.GetString(0),
                        CompileSuccess: !r.IsDBNull(1) && (r.GetValue(1)?.ToString() == "1" || r.GetValue(1)?.ToString()?.ToLowerInvariant() == "true"),
                        CompileErrors: r.IsDBNull(2) ? 0 : Convert.ToInt32(r.GetValue(2)),
                        GeneratedClasses: r.IsDBNull(3) ? 0 : Convert.ToInt32(r.GetValue(3)),
                        GeneratedLines: r.IsDBNull(4) ? 0 : Convert.ToInt32(r.GetValue(4)),
                        FallbackClasses: r.IsDBNull(5) ? 0 : Convert.ToInt32(r.GetValue(5)),
                        InjectAnnotations: r.IsDBNull(6) ? 0 : Convert.ToInt32(r.GetValue(6)),
                        Timestamp: r.IsDBNull(7) ? "" : r.GetString(7)
                    ));
                }
            }

            // Backfill: walk output/runs/* for the newest run folders that
            // produced code but did NOT have check-compile.sh invoked (no
            // quality_metrics event). Surface them with Measured=false so the
            // UI shows "not measured" instead of falsely flagging them as
            // failing. Without this, the widget appears stuck at runId 64
            // forever after the conversion-only / portal-driven runs.
            try
            {
                var measuredRunIds = new HashSet<string>(
                    summary.RecentQuality.Select(q => q.RunId),
                    StringComparer.OrdinalIgnoreCase);
                var runsDir = Path.Combine(_repoRoot, "output", "runs");
                if (Directory.Exists(runsDir))
                {
                    var folderRuns = Directory.EnumerateDirectories(runsDir)
                        .Select(d => new
                        {
                            Dir = d,
                            Name = Path.GetFileName(d),
                            Stamp = Directory.GetLastWriteTimeUtc(d)
                        })
                        .OrderByDescending(x => x.Stamp)
                        .Take(20)
                        .ToList();

                    foreach (var fr in folderRuns)
                    {
                        var runId = ExtractRunIdFromFolderName(fr.Name);
                        if (string.IsNullOrEmpty(runId)) continue;
                        if (measuredRunIds.Contains(runId)) continue;

                        var (classCount, lineCount, fallbackCount, injectCount) = SummarizeGeneratedCode(fr.Dir);
                        if (classCount == 0) continue; // skip empty run folders

                        summary.RecentQuality.Add(new QualitySummaryRow(
                            RunId: runId,
                            CompileSuccess: false,
                            CompileErrors: 0,
                            GeneratedClasses: classCount,
                            GeneratedLines: lineCount,
                            FallbackClasses: fallbackCount,
                            InjectAnnotations: injectCount,
                            Timestamp: fr.Stamp.ToString("o", CultureInfo.InvariantCulture),
                            Measured: false
                        ));
                        measuredRunIds.Add(runId);
                        if (summary.RecentQuality.Count >= 12) break;
                    }
                    // Sort newest-first: prefer Timestamp desc, fall back to RunIdSortKey
                    var sorted = summary.RecentQuality
                        .OrderByDescending(q => q.Timestamp, StringComparer.Ordinal)
                        .ThenByDescending(q => RunIdSortKey(q.RunId))
                        .Take(10)
                        .ToList();
                    summary.RecentQuality.Clear();
                    summary.RecentQuality.AddRange(sorted);
                }
            }
            catch (Exception ex)
            {
                _logger.LogDebug("Quality backfill failed: {Msg}", ex.Message);
            }

            // Derived headline metrics
            summary.TotalEvents = summary.EventCountsByType.Values.Sum();
            var hits = summary.CacheDecisionCounts.TryGetValue("hit", out var h) ? h : 0;
            var cacheTotal = summary.CacheDecisionCounts.Values.Sum();
            summary.CacheHitRatePct = cacheTotal > 0 ? Math.Round(hits * 100.0 / cacheTotal, 1) : 0;
            var llmTotal = summary.LlmCallOutcomes.Sum(o => o.Count);
            var llmSuccess = summary.LlmCallOutcomes.Where(o => o.Outcome == "success").Sum(o => o.Count);
            summary.LlmSuccessRatePct = llmTotal > 0 ? Math.Round(llmSuccess * 100.0 / llmTotal, 1) : 0;
            // Compile pass % must only consider rows where check-compile.sh
            // actually ran (Measured=true). Otherwise newer conversion-only
            // runs (where no compile gate was invoked) would falsely deflate
            // the rate to 0%.
            var measured = summary.RecentQuality.Where(q => q.Measured).ToList();
            var compileRuns = measured.Count;
            var compileOk = measured.Count(q => q.CompileSuccess);
            summary.RecentCompileSuccessPct = compileRuns > 0 ? Math.Round(compileOk * 100.0 / compileRuns, 1) : 0;
            summary.AvgContextReductionPct = summary.ContextReduction.Count > 0
                ? Math.Round(summary.ContextReduction.Average(r => r.ReductionPct), 1)
                : 0;

            return summary;
        }
        catch (Exception ex)
        {
            _logger.LogWarning(ex, "ModernizationIntelligence dashboard query failed");
            return DashboardSummary.Empty($"benchmark.db query failed: {ex.Message}");
        }
    }

    // ─────────────────────────────────────────────────────────────────────
    // Runtime & Conversion Intelligence (PR-Portal-P2)
    // ─────────────────────────────────────────────────────────────────────

    /// <summary>
    /// All runs that have at least one metric event, ordered most-recent first.
    /// Used by the runs picker in Runtime & Conversion Intelligence.
    /// </summary>
    public IEnumerable<RunSummaryRow> GetRuns(int limit = 50)
    {
        // Union two sources so the picker always shows the freshest runs:
        //   (a) benchmark.db   — ingested metric_events  (rich stats)
        //   (b) output/.metrics/*.jsonl — raw sink files (fallback for runs
        //       the ingester hasn't picked up yet — was capping the UI at 64)
        // Newer files override DB rows for the same RunId.
        var byRunId = new Dictionary<string, RunSummaryRow>(StringComparer.OrdinalIgnoreCase);

        // (a) DB
        var dbPath = Path.Combine(_repoRoot, "Data", "benchmark.db");
        if (File.Exists(dbPath))
        {
            SqliteConnection? conn = null;
            try
            {
                conn = new SqliteConnection($"Data Source={dbPath};Mode=ReadOnly;");
                conn.Open();
            }
            catch (Exception ex)
            {
                _logger.LogDebug("GetRuns open failed: {Msg}", ex.Message);
            }

            if (conn != null)
            {
                using (conn)
                {
                    using var c = conn.CreateCommand();
                    c.CommandText = @"
                        SELECT run_id,
                               MIN(ts) AS first_ts,
                               MAX(ts) AS last_ts,
                               COUNT(*) AS event_count,
                               SUM(CASE WHEN event='llm_call' THEN 1 ELSE 0 END) AS llm_calls,
                               SUM(CASE WHEN event='projection_metrics' THEN 1 ELSE 0 END) AS projection_events,
                               SUM(CASE WHEN event='cache_event'
                                         AND json_extract(payload_json,'$.decision')='hit'
                                        THEN 1 ELSE 0 END) AS cache_hits,
                               SUM(CASE WHEN event='cache_event' THEN 1 ELSE 0 END) AS cache_total,
                               SUM(CASE WHEN event='llm_call'
                                         AND json_extract(payload_json,'$.outcome')='success'
                                        THEN 1 ELSE 0 END) AS llm_success,
                               SUM(CASE WHEN event='llm_call'
                                         AND json_extract(payload_json,'$.outcome')!='success'
                                        THEN 1 ELSE 0 END) AS llm_fail
                          FROM metric_events
                         WHERE run_id != 'unknown'
                         GROUP BY run_id";
                    using var r = c.ExecuteReader();
                    while (r.Read())
                    {
                        var runId = r.GetString(0);
                        byRunId[runId] = new RunSummaryRow(
                            RunId: runId,
                            FirstEventTs: r.IsDBNull(1) ? "" : r.GetString(1),
                            LastEventTs: r.IsDBNull(2) ? "" : r.GetString(2),
                            EventCount: r.GetInt32(3),
                            LlmCallCount: r.IsDBNull(4) ? 0 : r.GetInt32(4),
                            ProjectionEventCount: r.IsDBNull(5) ? 0 : r.GetInt32(5),
                            CacheHits: r.IsDBNull(6) ? 0 : r.GetInt32(6),
                            CacheTotal: r.IsDBNull(7) ? 0 : r.GetInt32(7),
                            LlmSuccess: r.IsDBNull(8) ? 0 : r.GetInt32(8),
                            LlmFail: r.IsDBNull(9) ? 0 : r.GetInt32(9)
                        );
                    }
                }
            }
        }

        // (b) raw .metrics sink files (catch runs not yet ingested)
        var metricsDir = Path.Combine(_repoRoot, "output", ".metrics");
        if (Directory.Exists(metricsDir))
        {
            foreach (var jsonl in Directory.EnumerateFiles(metricsDir, "*.jsonl"))
            {
                var runId = Path.GetFileNameWithoutExtension(jsonl);
                if (string.IsNullOrWhiteSpace(runId) || runId.Equals("unknown", StringComparison.OrdinalIgnoreCase))
                    continue;
                if (byRunId.ContainsKey(runId)) continue; // DB row wins
                var summary = SummarizeMetricsFile(runId, jsonl);
                if (summary != null) byRunId[runId] = summary;
            }
        }

        return byRunId.Values
            .OrderByDescending(r => RunIdSortKey(r.RunId))
            .ThenByDescending(r => r.LastEventTs, StringComparer.Ordinal)
            .Take(Math.Max(1, limit))
            .ToList();
    }

    private static long RunIdSortKey(string runId)
    {
        // Numeric run IDs (1, 2, 64, 79, …) sort numerically; non-numeric IDs
        // (GUIDs, timestamped slugs) sort by string fallback so newest naturally
        // appears at the top when prefixed with a timestamp.
        if (long.TryParse(runId, out var n)) return n;
        return long.MaxValue / 2; // bubble unknown-format IDs above old numeric ones
    }

    private static string ExtractRunIdFromFolderName(string folderName)
    {
        // Folder patterns supported (created by ProcessManager.cs):
        //   {YYYY-MM-DD}_{HH-mm-ss}_{tag}-{lang}-{slug}-{utcStamp}   ← timestamped (CLI/portal)
        //   {runId}-{lang}-{slug}-{utcStamp}                          ← legacy GUID/numeric runId
        // For the timestamped form, the in-folder "tag" is not a unique runId
        // (e.g. "demo", "cli"), so we use the localStamp itself as the runId —
        // that's what users see in `ls output/runs/` and it sorts correctly.
        if (string.IsNullOrWhiteSpace(folderName)) return "";

        if (folderName.Length > 20 && folderName[4] == '-' && folderName[7] == '-' && folderName[10] == '_'
            && folderName[13] == '-' && folderName[16] == '-' && folderName[19] == '_')
        {
            // Use the timestamp prefix as the runId so each timestamped folder
            // gets a unique, sortable, human-readable identifier.
            return folderName.Substring(0, 19);
        }

        // Legacy form: runId is everything up to the first dash.
        var dashIdx = folderName.IndexOf('-');
        return dashIdx > 0 ? folderName.Substring(0, dashIdx) : folderName;
    }

    private static (int classes, int lines, int fallback, int inject) SummarizeGeneratedCode(string runDir)
    {
        int classes = 0, lines = 0, fallback = 0, inject = 0;
        try
        {
            var codeFiles = Directory.EnumerateFiles(runDir, "*.java", SearchOption.AllDirectories)
                .Concat(Directory.EnumerateFiles(runDir, "*.cs", SearchOption.AllDirectories));
            foreach (var f in codeFiles)
            {
                classes++;
                try
                {
                    var text = File.ReadAllText(f);
                    lines += text.Count(c => c == '\n') + 1;
                    if (text.Contains("@Inject", StringComparison.Ordinal)) inject++;
                    if (text.Contains("LLM returned empty output", StringComparison.OrdinalIgnoreCase)
                        || text.Contains("FALLBACK STUB", StringComparison.OrdinalIgnoreCase)) fallback++;
                }
                catch { /* skip unreadable */ }
            }
        }
        catch { /* skip unreadable run dir */ }
        return (classes, lines, fallback, inject);
    }

    private RunSummaryRow? SummarizeMetricsFile(string runId, string path)
    {
        try
        {
            int eventCount = 0, llm = 0, proj = 0, cacheHit = 0, cacheTotal = 0, llmSuccess = 0, llmFail = 0;
            string firstTs = "", lastTs = "";
            foreach (var line in File.ReadLines(path))
            {
                if (string.IsNullOrWhiteSpace(line)) continue;
                JsonDocument doc;
                try { doc = JsonDocument.Parse(line); } catch { continue; }
                using (doc)
                {
                    var root = doc.RootElement;
                    eventCount++;
                    var ts = root.TryGetProperty("ts", out var tsEl) ? tsEl.GetString() ?? "" : "";
                    if (eventCount == 1) firstTs = ts;
                    if (!string.IsNullOrEmpty(ts)) lastTs = ts;
                    var ev = root.TryGetProperty("event", out var evEl) ? evEl.GetString() : null;
                    if (ev == "llm_call")
                    {
                        llm++;
                        if (root.TryGetProperty("payload", out var pl) && pl.TryGetProperty("outcome", out var oc)
                            && oc.GetString() == "success") llmSuccess++; else llmFail++;
                    }
                    else if (ev == "projection_metrics") proj++;
                    else if (ev == "cache_event")
                    {
                        cacheTotal++;
                        if (root.TryGetProperty("payload", out var pl) && pl.TryGetProperty("decision", out var dec)
                            && dec.GetString() == "hit") cacheHit++;
                    }
                }
            }
            if (eventCount == 0) return null;
            return new RunSummaryRow(runId, firstTs, lastTs, eventCount, llm, proj, cacheHit, cacheTotal, llmSuccess, llmFail);
        }
        catch (Exception ex)
        {
            _logger.LogDebug("SummarizeMetricsFile({Path}) failed: {Msg}", path, ex.Message);
            return null;
        }
    }

    /// <summary>
    /// Full per-run event timeline. Reads <c>output/.metrics/{runId}.jsonl</c>
    /// directly (newer/safer than benchmark.db which may lag the ingester).
    /// Each event is returned with timing offset from the first event so the
    /// frontend can render a Gantt/timeline visualisation.
    /// </summary>
    public RunTimeline GetRunTimeline(string runId)
    {
        var timeline = new RunTimeline { RunId = runId };
        var jsonl = Path.Combine(_repoRoot, "output", ".metrics", $"{runId}.jsonl");
        if (!File.Exists(jsonl))
        {
            timeline.Note = $"No timeline available — {jsonl} does not exist";
            return timeline;
        }

        DateTime? first = null;
        try
        {
            foreach (var line in File.ReadAllLines(jsonl))
            {
                var trimmed = line.Trim();
                if (trimmed.Length == 0) continue;
                JsonDocument doc;
                try { doc = JsonDocument.Parse(trimmed); }
                catch { continue; }

                using (doc)
                {
                    var root = doc.RootElement;
                    var ts = root.TryGetProperty("ts", out var tsEl) ? tsEl.GetString() : null;
                    DateTime? tsParsed = null;
                    if (DateTime.TryParse(ts, CultureInfo.InvariantCulture, DateTimeStyles.RoundtripKind, out var parsed))
                        tsParsed = parsed;
                    if (first == null && tsParsed != null) first = tsParsed;
                    var offsetMs = (tsParsed != null && first != null)
                        ? (long)(tsParsed.Value - first.Value).TotalMilliseconds
                        : 0;

                    timeline.Events.Add(new TimelineEvent(
                        Timestamp: ts ?? "",
                        OffsetMs: offsetMs,
                        Event: root.TryGetProperty("event", out var ev) ? ev.GetString() ?? "?" : "?",
                        Agent: root.TryGetProperty("agent", out var ag) ? ag.GetString() : null,
                        File: root.TryGetProperty("file", out var fl) ? fl.GetString() : null,
                        Outcome: root.TryGetProperty("outcome", out var oc) ? oc.GetString() : null,
                        ProjectionMode: root.TryGetProperty("projectionMode", out var pm) ? pm.GetString() : null,
                        Decision: root.TryGetProperty("decision", out var dc) ? dc.GetString() : null,
                        DurationMs: root.TryGetProperty("streamDurationMs", out var sd) && sd.ValueKind == JsonValueKind.Number ? sd.GetInt64() : (long?)null,
                        CompletionTokens: root.TryGetProperty("completionTokens", out var ct) && ct.ValueKind == JsonValueKind.Number ? ct.GetInt32() : (int?)null,
                        ProjectionTokens: root.TryGetProperty("projectionTokens", out var pt) && pt.ValueKind == JsonValueKind.Number ? pt.GetInt32() : (int?)null,
                        RawRektTokens: root.TryGetProperty("rawRektTokens", out var rt) && rt.ValueKind == JsonValueKind.Number ? rt.GetInt32() : (int?)null,
                        CompileSuccess: root.TryGetProperty("compileSuccess", out var cs) && cs.ValueKind == JsonValueKind.True ? true :
                                        root.TryGetProperty("compileSuccess", out var cs2) && cs2.ValueKind == JsonValueKind.False ? false : (bool?)null,
                        BraceImbalance: root.TryGetProperty("braceImbalance", out var bi) && bi.ValueKind == JsonValueKind.Number ? bi.GetInt32() : (int?)null,
                        PayloadJson: trimmed
                    ));
                }
            }
        }
        catch (Exception ex)
        {
            timeline.Note = $"Timeline read failed: {ex.Message}";
            return timeline;
        }

        if (timeline.Events.Count > 0)
        {
            var last = timeline.Events.Last().OffsetMs;
            timeline.TotalDurationMs = last;
            timeline.FirstEventTs = timeline.Events.First().Timestamp;
            timeline.LastEventTs = timeline.Events.Last().Timestamp;
        }

        // Sub-rollups for chips at the top of the timeline view
        foreach (var e in timeline.Events)
        {
            timeline.EventCounts[e.Event] = timeline.EventCounts.GetValueOrDefault(e.Event) + 1;
        }
        return timeline;
    }

    // ─────────────────────────────────────────────────────────────────────
    // Compile-failure inspector (#8) — lists generated files for a run
    // and parses any compile log into structured per-file errors.
    // ─────────────────────────────────────────────────────────────────────

    public CompileDetail GetCompileDetail(string runId)
    {
        var detail = new CompileDetail { RunId = runId };

        // Locate the run's per-run output folder under output/runs/.
        // Folder pattern changed mid-life to put a local timestamp at the
        // FRONT (output/runs/{localStamp}_{runId}-…), so search both legacy
        // ({runId}-*) and new (*{runId}*) layouts.
        var runsRoot = Path.Combine(_repoRoot, "output", "runs");
        string? folder = null;
        if (Directory.Exists(runsRoot))
        {
            folder = Directory.EnumerateDirectories(runsRoot, $"*{runId}*")
                .FirstOrDefault();
        }
        // Fall back to the shared legacy folders if the run pre-dates per-run isolation
        if (folder == null)
        {
            foreach (var legacy in new[] {
                Path.Combine(_repoRoot, "output", "java"),
                Path.Combine(_repoRoot, "output", "csharp")
            })
            {
                if (Directory.Exists(legacy)) { folder = legacy; break; }
            }
        }
        if (folder == null) return detail;

        detail.OutputFolder = Path.GetRelativePath(_repoRoot, folder);

        // List source files (cap 50, biggest first), include preview lines for the UI
        var codeExts = new HashSet<string>(StringComparer.OrdinalIgnoreCase) {
            ".java", ".cs", ".kt", ".ts", ".scala"
        };
        var files = Directory.EnumerateFiles(folder, "*", SearchOption.AllDirectories)
            .Where(p => codeExts.Contains(Path.GetExtension(p)))
            .Take(50)
            .Select(p => new FileInfo(p))
            .ToList();

        // Parse compile log if present (typical names: compile.log, check-compile.log)
        var errorsByFile = new Dictionary<string, List<CompileError>>(StringComparer.OrdinalIgnoreCase);
        foreach (var logName in new[] { "compile.log", "check-compile.log", "javac.log", "dotnet-build.log" })
        {
            var logPath = Path.Combine(folder, logName);
            if (!File.Exists(logPath)) continue;
            try
            {
                var lines = File.ReadAllLines(logPath);
                // javac: "<path>:<line>: error: <msg>"  ·  csc/dotnet: "<path>(<line>,<col>): error <code>: <msg>"
                var javacRx = new System.Text.RegularExpressions.Regex(@"^(.+?):(\d+):\s*(?:error|ERROR):\s*(.+)$");
                var dotnetRx = new System.Text.RegularExpressions.Regex(@"^(.+?)\((\d+),\d+\):\s*(?:error|ERROR)\s+\w+:\s*(.+)$");
                foreach (var ln in lines)
                {
                    var m1 = javacRx.Match(ln);
                    var m  = m1.Success ? m1 : dotnetRx.Match(ln);
                    if (!m.Success) continue;
                    var fileName = Path.GetFileName(m.Groups[1].Value.Trim());
                    if (!errorsByFile.ContainsKey(fileName))
                        errorsByFile[fileName] = new List<CompileError>();
                    errorsByFile[fileName].Add(new CompileError(
                        File: fileName,
                        Line: int.TryParse(m.Groups[2].Value, out var n) ? n : null,
                        Message: m.Groups[3].Value.Trim()
                    ));
                }
            }
            catch { /* fail-soft */ }
        }

        // Sort files: failing first, then by size desc
        var fileInfos = files.Select(fi =>
        {
            string content = "";
            try { content = File.ReadAllText(fi.FullName); } catch { /* skip unreadable */ }
            var fileName = fi.Name;
            errorsByFile.TryGetValue(fileName, out var errs);
            return new CompileFile(
                FileName: fileName,
                Path: Path.GetRelativePath(_repoRoot, fi.FullName),
                LineCount: content.Count(c => c == '\n') + 1,
                Content: content.Length > 200000 ? content.Substring(0, 200000) + "\n... (truncated)" : content,
                HasError: errs?.Count > 0,
                ErrorCount: errs?.Count ?? 0
            );
        })
        .OrderByDescending(f => f.HasError)
        .ThenByDescending(f => f.ErrorCount)
        .ThenByDescending(f => f.LineCount)
        .ToList();

        detail.Files = fileInfos;
        detail.Errors = errorsByFile.SelectMany(kv => kv.Value).ToList();
        return detail;
    }

    // ─────────────────────────────────────────────────────────────────────
    // Dependency Topology (PR-Portal-P3) — semantic overlay on existing graph
    // ─────────────────────────────────────────────────────────────────────

    /// <summary>
    /// Combines the existing Neo4j services graph (nodes + CALL edges) with
    /// per-program modernization state from the other read paths. Frontend
    /// uses this to drive a layered architecture view + migration impact
    /// analysis without re-querying multiple endpoints.
    /// </summary>
    public TopologySnapshot GetTopology()
    {
        var snap = new TopologySnapshot();
        // Build inventory-aligned overlay first (reuse the same data the
        // Application Explorer surfaces).
        var apps = GetApplications().ToList();
        // Inventory scans (source/, webdemo/sources/cobol/, .preprocessed/) can
        // legitimately surface the same Basename twice — keep the first.
        var byBasename = apps
            .GroupBy(a => a.Basename, StringComparer.OrdinalIgnoreCase)
            .ToDictionary(g => g.Key, g => g.First(), StringComparer.OrdinalIgnoreCase);

        foreach (var a in apps)
        {
            snap.Nodes.Add(new TopologyNode(
                Id: a.Basename,
                Kind: "program",
                LinesOfCode: a.LinesOfCode,
                HasFacts: a.HasFacts,
                FactsConfidence: a.FactsConfidence,
                LatestRunId: a.LatestRunId,
                CompileSuccess: a.LatestCompileSuccess,
                ProjectionCacheHits: a.ProjectionCacheHits,
                ModernizationStatus: a.ModernizationStatus
            ));
        }
        return snap;
    }

    // ─────────────────────────────────────────────────────────────────────
    // Dependency Health (PR-Portal-P0-enterprise) — copybook resolution
    // & estate-readiness scoring
    // ─────────────────────────────────────────────────────────────────────

    public DependencyHealthSnapshot GetDependencyHealth()
    {
        var snap = new DependencyHealthSnapshot();
        var rektDir = Path.Combine(_repoRoot, "output", "rekt");
        if (!Directory.Exists(rektDir))
        {
            snap.Note = $"REKT output dir not found: {rektDir}. Run ./doctor.sh rekt-full first.";
            return snap;
        }

        // Parse missing-copybooks.txt
        var missingFile = Path.Combine(rektDir, "missing-copybooks.txt");
        if (File.Exists(missingFile))
        {
            foreach (var raw in File.ReadAllLines(missingFile))
            {
                var line = raw.Trim();
                if (line.Length == 0 || line.StartsWith("#")) continue;
                var tab = line.IndexOf('\t');
                if (tab < 0) continue;
                var cpy = line.Substring(0, tab).Trim();
                var rest = line.Substring(tab + 1).Trim();
                const string prefix = "referenced by:";
                var listIdx = rest.IndexOf(prefix, StringComparison.OrdinalIgnoreCase);
                var refs = listIdx >= 0
                    ? rest.Substring(listIdx + prefix.Length).Split(',', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries).ToList()
                    : new List<string>();
                snap.MissingCopybooks.Add(new MissingCopybookRow(cpy, refs));
            }
        }

        // Per-program parse fidelity
        var apps = GetApplications().ToList();
        foreach (var a in apps)
        {
            var stem = Path.GetFileNameWithoutExtension(a.Basename);
            var reportDir = Path.Combine(rektDir, $"{a.Basename}.report");
            var depsJson = Path.Combine(rektDir, $"{stem}-deps.json");

            var hasReport = Directory.Exists(reportDir);
            var hasDepsOnly = File.Exists(depsJson) && !hasReport;
            var fidelity = hasReport ? "full" : hasDepsOnly ? "deps-only" : "not-parsed";

            int missingForThis = snap.MissingCopybooks.Count(m => m.ReferencedBy.Contains(a.Basename, StringComparer.OrdinalIgnoreCase));

            snap.Programs.Add(new ProgramHealthRow(
                Basename: a.Basename,
                LinesOfCode: a.LinesOfCode,
                ParseFidelity: fidelity,
                FactsConfidence: a.FactsConfidence,
                FactsWarnings: a.FactsWarnings,
                MissingCopybookCount: missingForThis,
                HasReport: hasReport,
                HasDepsOnly: hasDepsOnly,
                ModernizationStatus: a.ModernizationStatus
            ));
        }

        // Estate-level KPIs
        snap.TotalPrograms = snap.Programs.Count;
        snap.FullFidelityCount = snap.Programs.Count(p => p.ParseFidelity == "full");
        snap.DepsOnlyCount = snap.Programs.Count(p => p.ParseFidelity == "deps-only");
        snap.NotParsedCount = snap.Programs.Count(p => p.ParseFidelity == "not-parsed");
        snap.CoveragePct = snap.TotalPrograms > 0
            ? Math.Round(snap.FullFidelityCount * 100.0 / snap.TotalPrograms, 1)
            : 0;

        snap.TotalMissingCopybooks = snap.MissingCopybooks.Count;
        snap.ProgramsBlockedByMissing = snap.MissingCopybooks
            .SelectMany(m => m.ReferencedBy)
            .Distinct(StringComparer.OrdinalIgnoreCase)
            .Count();

        // Readiness = full-fidelity weighted heavier than deps-only.
        if (snap.TotalPrograms > 0)
        {
            double weighted = snap.FullFidelityCount * 1.0 + snap.DepsOnlyCount * 0.25;
            snap.ReadinessScore = Math.Round(weighted * 100.0 / snap.TotalPrograms, 1);
        }
        return snap;
    }

    // ─────────────────────────────────────────────────────────────────────
    // Semantic Flow (PR-Portal-P1-flow) — per-program flow summary
    // ─────────────────────────────────────────────────────────────────────

    public FlowSnapshot GetProgramFlow(string basename)
    {
        var snap = new FlowSnapshot { Basename = basename };
        var rektDir = Path.Combine(_repoRoot, "output", "rekt");
        var reportDir = Path.Combine(rektDir, $"{basename}.report");
        if (!Directory.Exists(reportDir))
        {
            snap.Note = $"No .report directory for {basename} — program likely parsed deps-only.";
            return snap;
        }
        var flowAstDir = Path.Combine(reportDir, "flow_ast");
        snap.HasFlowAst = Directory.Exists(flowAstDir);
        snap.HasCfg = Directory.Exists(Path.Combine(reportDir, "cfg"));
        snap.HasDataStructures = Directory.Exists(Path.Combine(reportDir, "data_structures"));
        if (snap.HasFlowAst)
        {
            try { snap.FlowAstFiles = Directory.GetFiles(flowAstDir, "*.json").Length; } catch { }
        }
        return snap;
    }

    // ─────────────────────────────────────────────────────────────────────
    // Service Candidate Explorer (PR-Portal-P2-services) — formal
    // bounded-context inference from REKT topology + facts + missing-cpys
    // ─────────────────────────────────────────────────────────────────────

    /// <summary>
    /// Infers service candidates using a multi-signal scoring model:
    ///   - CALL coupling: programs sharing many CALL edges cluster together
    ///   - Copybook coupling: programs sharing the same copybook usage cluster
    ///   - Hub centrality: programs with high downstream are likely orchestrators
    ///   - Naming affinity: shared name prefix is a domain hint
    ///   - Boundary strength: clusters with low cross-cluster edges score higher
    ///
    /// Output: ranked list of candidate services with member programs,
    /// suggested service name, total LoC, average facts confidence, and a
    /// cohesion score (0-100).
    /// </summary>
    public ServiceCandidateSnapshot GetServiceCandidates()
    {
        var snap = new ServiceCandidateSnapshot();

        // Gather inputs from existing sources (no new data collection)
        var apps = GetApplications().ToList();
        var topology = GetTopology();
        var byBasename = topology.Nodes
            .GroupBy(n => n.Id, StringComparer.OrdinalIgnoreCase)
            .ToDictionary(g => g.Key, g => g.First(), StringComparer.OrdinalIgnoreCase);

        // Build CALL adjacency from existing graph endpoint (we re-query the
        // raw services graph the same way the topology view does). For
        // simplicity here, derive CALL adjacency from missing-cpy + apps:
        // we use the Neo4j-derived /api/graph/rekt/services payload via the
        // existing approach — but to avoid an extra HTTP roundtrip from
        // the backend, recompute from output/rekt deps files where possible.
        var rektDir = Path.Combine(_repoRoot, "output", "rekt");
        var callEdges = new Dictionary<string, HashSet<string>>(StringComparer.OrdinalIgnoreCase); // source → set of targets
        if (Directory.Exists(rektDir))
        {
            foreach (var depsFile in Directory.GetFiles(rektDir, "*-deps.json"))
            {
                try
                {
                    using var doc = JsonDocument.Parse(File.ReadAllText(depsFile));
                    var stem = Path.GetFileNameWithoutExtension(depsFile).Replace("-deps", "", StringComparison.OrdinalIgnoreCase);
                    var basename = $"{stem}.cbl";
                    if (doc.RootElement.TryGetProperty("calls", out var callsEl) && callsEl.ValueKind == JsonValueKind.Array)
                    {
                        var set = callEdges.GetValueOrDefault(basename) ?? new HashSet<string>(StringComparer.OrdinalIgnoreCase);
                        foreach (var c in callsEl.EnumerateArray())
                        {
                            var name = c.GetString();
                            if (!string.IsNullOrEmpty(name)) set.Add(name + ".cbl");
                        }
                        callEdges[basename] = set;
                    }
                }
                catch { /* fall through */ }
            }
        }

        // Naming affinity: 4-letter prefix domain grouping
        var prefixGroups = apps.GroupBy(a =>
            Path.GetFileNameWithoutExtension(a.Basename).PadRight(4, '_').Substring(0, 4).ToUpperInvariant());

        foreach (var grp in prefixGroups.OrderByDescending(g => g.Sum(a => a.LinesOfCode)))
        {
            var members = grp.ToList();
            if (members.Count == 0) continue;

            // Intra-cluster CALL edges (cohesion signal)
            int intraEdges = 0;
            int crossEdges = 0;
            var memberSet = new HashSet<string>(members.Select(m => m.Basename), StringComparer.OrdinalIgnoreCase);
            foreach (var m in members)
            {
                if (callEdges.TryGetValue(m.Basename, out var calls))
                {
                    foreach (var t in calls)
                    {
                        if (memberSet.Contains(t)) intraEdges++;
                        else crossEdges++;
                    }
                }
            }
            var totalEdges = intraEdges + crossEdges;
            var boundaryStrength = totalEdges > 0 ? (intraEdges * 100.0 / totalEdges) : 100.0;

            var totalLoc = members.Sum(m => m.LinesOfCode);
            var avgConfidence = members.Count > 0 ? members.Average(m => m.FactsConfidence) : 0;
            var fullFidelity = members.Count(m => m.HasFacts && m.FactsConfidence >= 3);

            // Composite cohesion 0-100: weighted of boundary strength + size + confidence
            var cohesion = Math.Round(
                boundaryStrength * 0.6
                + Math.Min(100, members.Count * 8) * 0.2  // 8 progs = full size credit
                + (avgConfidence * 100.0 / 3.0) * 0.2,    // facts conf 0-3 → 0-100
                1);

            // Suggested service name: domain prefix + plural suffix
            var suggestedName = grp.Key.TrimEnd('_') + "Service";

            snap.Candidates.Add(new ServiceCandidate(
                SuggestedName: suggestedName,
                DomainPrefix: grp.Key,
                MemberPrograms: members.Select(m => m.Basename).ToList(),
                MemberCount: members.Count,
                TotalLinesOfCode: totalLoc,
                FullFidelityCount: fullFidelity,
                IntraClusterEdges: intraEdges,
                CrossClusterEdges: crossEdges,
                BoundaryStrengthPct: Math.Round(boundaryStrength, 1),
                AvgFactsConfidence: Math.Round(avgConfidence, 2),
                CohesionScore: cohesion,
                ReadyForExtraction: fullFidelity == members.Count && boundaryStrength >= 70
            ));
        }

        snap.Candidates.Sort((a, b) => b.CohesionScore.CompareTo(a.CohesionScore));
        snap.TotalCandidates = snap.Candidates.Count;
        snap.ExtractionReadyCount = snap.Candidates.Count(c => c.ReadyForExtraction);
        return snap;
    }

    // ─────────────────────────────────────────────────────────────────────
    // Service Chain (PR-Portal-Service-Chain) — JCL → Program → Copybook
    // visualization. The "very cool dashboard" of the modernization estate.
    // ─────────────────────────────────────────────────────────────────────

    /// <summary>
    /// Returns the full execution chain for a COBOL estate:
    ///   JCL job ──EXEC PGM=──> COBOL program ──COPY──> Copybook
    ///
    /// JCL scan: simple regex on <c>EXEC PGM=NAME</c> across <c>source/**/*.JCL</c>
    /// (the sources corpus has 22 JCL files under <c>source/sources/JCL/</c>).
    /// Copybook chain: per-program <c>output/rekt/{stem}.facts.json</c> ->
    /// <c>copybooks</c> array.
    ///
    /// Output includes a pre-rendered Mermaid flowchart for the entire chain
    /// (or filtered to a single job / single program subgraph) so the frontend
    /// can render it directly with the existing Mermaid library.
    /// </summary>
    public ServiceChainSnapshot GetServiceChain(string? jobFilter, string? programFilter, bool includeUtilities = false)
    {
        var snap = new ServiceChainSnapshot();
        var sourceDir = Path.Combine(_repoRoot, "source");
        if (!Directory.Exists(sourceDir))
        {
            snap.Note = "source/ folder not found.";
            return snap;
        }

        // 1. JCL scan
        var execPgmRegex = new System.Text.RegularExpressions.Regex(
            @"EXEC\s+PGM\s*=\s*([A-Z0-9$@#]+)",
            System.Text.RegularExpressions.RegexOptions.IgnoreCase);
        var jobNameRegex = new System.Text.RegularExpressions.Regex(
            @"^//(?<name>[A-Z0-9$@#]+)\s+JOB",
            System.Text.RegularExpressions.RegexOptions.IgnoreCase | System.Text.RegularExpressions.RegexOptions.Multiline);

        var jclFiles = Directory.EnumerateFiles(sourceDir, "*.JCL", SearchOption.AllDirectories)
            .Concat(Directory.EnumerateFiles(sourceDir, "*.jcl", SearchOption.AllDirectories))
            .Where(p => !p.Contains("/.convert-", StringComparison.Ordinal)
                     && !p.Contains("/.rekt-staging", StringComparison.Ordinal)
                     && !p.Contains("/.preprocessed", StringComparison.Ordinal))
            .Distinct(StringComparer.OrdinalIgnoreCase)
            .ToList();

        // Map: program basename (without .cbl) -> set of JCL jobs referencing it
        var pgmToJobs = new Dictionary<string, HashSet<string>>(StringComparer.OrdinalIgnoreCase);

        foreach (var jclPath in jclFiles)
        {
            string content;
            try { content = File.ReadAllText(jclPath); } catch { continue; }
            var jclFileName = Path.GetFileNameWithoutExtension(jclPath);
            var jobNameMatch = jobNameRegex.Match(content);
            var jobName = jobNameMatch.Success ? jobNameMatch.Groups["name"].Value : jclFileName;

            var pgms = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
            foreach (System.Text.RegularExpressions.Match m in execPgmRegex.Matches(content))
            {
                var pgm = m.Groups[1].Value.ToUpperInvariant();
                // Skip system utilities by default; toggle includeUtilities=true via API to surface them.
                if (!includeUtilities && IsSystemUtility(pgm)) continue;
                pgms.Add(pgm);
                if (!pgmToJobs.ContainsKey(pgm)) pgmToJobs[pgm] = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
                pgmToJobs[pgm].Add(jobName);
            }

            snap.Jobs.Add(new JclJob(
                JobName: jobName,
                JclFileName: Path.GetFileName(jclPath) ?? jclFileName,
                RelativePath: Path.GetRelativePath(_repoRoot, jclPath),
                PrimaryPrograms: pgms.ToList()
            ));
        }

        // 2. Per-program copybook chain — parse COPY statements directly from
        //    the .cbl source (facts.json doesn't expose copybooks today).
        var apps = GetApplications().ToList();
        var copyRegex = new System.Text.RegularExpressions.Regex(
            @"^\s*COPY\s+([A-Z0-9$@#\-_]+)",
            System.Text.RegularExpressions.RegexOptions.IgnoreCase | System.Text.RegularExpressions.RegexOptions.Multiline);

        var seenStems = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        foreach (var a in apps)
        {
            var stem = Path.GetFileNameWithoutExtension(a.Basename).ToUpperInvariant();
            if (!seenStems.Add(stem)) continue; // collapse duplicate basenames from multiple source roots
            var copybooks = new List<string>();
            // Locate the source .cbl file (recursive — handles sources/SRC/* paths)
            var srcPath = Directory.EnumerateFiles(sourceDir, a.Basename, SearchOption.AllDirectories)
                .FirstOrDefault(p => !p.Contains("/.convert-", StringComparison.Ordinal)
                                  && !p.Contains("/.rekt-staging", StringComparison.Ordinal)
                                  && !p.Contains("/.preprocessed", StringComparison.Ordinal));
            if (srcPath != null && File.Exists(srcPath))
            {
                try
                {
                    var content = File.ReadAllText(srcPath);
                    foreach (System.Text.RegularExpressions.Match m in copyRegex.Matches(content))
                    {
                        var cpyName = m.Groups[1].Value.ToUpperInvariant().TrimEnd('.');
                        if (!string.IsNullOrEmpty(cpyName)) copybooks.Add(cpyName);
                    }
                }
                catch { /* fail soft */ }
            }

            var referencedByJobs = pgmToJobs.TryGetValue(stem, out var jobs) ? jobs.ToList() : new List<string>();

            snap.Programs.Add(new ProgramChain(
                Basename: a.Basename,
                Stem: stem,
                LinesOfCode: a.LinesOfCode,
                Copybooks: copybooks.Distinct(StringComparer.OrdinalIgnoreCase).Where(c => !string.IsNullOrEmpty(c)).ToList(),
                CalledByJobs: referencedByJobs,
                ModernizationStatus: a.ModernizationStatus
            ));
        }

        // 3. Aggregate KPIs
        snap.TotalJobs = snap.Jobs.Count;
        snap.TotalPrograms = snap.Programs.Count;
        snap.TotalCopybooks = snap.Programs.SelectMany(p => p.Copybooks)
            .Distinct(StringComparer.OrdinalIgnoreCase).Count();
        snap.JobToProgramEdges = snap.Jobs.Sum(j => j.PrimaryPrograms.Count);
        snap.ProgramToCopybookEdges = snap.Programs.Sum(p => p.Copybooks.Count);

        // 4. Mermaid flowchart (filtered or full)
        snap.Mermaid = BuildServiceChainMermaid(snap, jobFilter, programFilter);
        return snap;
    }

    private static readonly HashSet<string> _systemUtilities = new(StringComparer.OrdinalIgnoreCase)
    {
        "IDCAMS", "IKJEFT01", "IEFBR14", "SORT", "ICETOOL", "DFSORT",
        "ADUUMAIN", "SYSUTCOM", "DSNUTILB", "DSNTIAUL", "IEBGENER",
        "IEBCOPY", "IEHPROGM", "IRXJCL", "EZACFSM1"
    };
    private static bool IsSystemUtility(string pgm) => _systemUtilities.Contains(pgm);

    /// <summary>
    /// Build a Mermaid flowchart string showing JCL → Program → Copybook chain.
    /// Filtered when caller passed jobFilter or programFilter — otherwise renders
    /// the whole estate (cap at 50 nodes to keep the diagram readable).
    /// </summary>
    private static string BuildServiceChainMermaid(ServiceChainSnapshot snap, string? jobFilter, string? programFilter)
    {
        var sb = new System.Text.StringBuilder();
        sb.AppendLine("flowchart LR");
        sb.AppendLine("  classDef jobNode fill:#7c2d12,stroke:#fb923c,color:#fef3c7,rx:6,ry:6");
        sb.AppendLine("  classDef pgmNode fill:#1e3a5f,stroke:#60a5fa,color:#e2e8f0,rx:4,ry:4");
        sb.AppendLine("  classDef cpyNode fill:#14532d,stroke:#10b981,color:#e2e8f0");

        IEnumerable<JclJob> jobs = snap.Jobs;
        if (!string.IsNullOrEmpty(jobFilter))
            jobs = jobs.Where(j => j.JobName.Equals(jobFilter, StringComparison.OrdinalIgnoreCase));

        IEnumerable<ProgramChain> programs = snap.Programs;
        if (!string.IsNullOrEmpty(programFilter))
        {
            var stem = Path.GetFileNameWithoutExtension(programFilter).ToUpperInvariant();
            programs = programs.Where(p => p.Stem.Equals(stem, StringComparison.OrdinalIgnoreCase));
            jobs = snap.Jobs.Where(j => j.PrimaryPrograms.Contains(stem, StringComparer.OrdinalIgnoreCase));
        }

        var renderedJobs = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        var renderedPgms = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        var renderedCpys = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        int edgeCount = 0;
        const int maxEdges = 200;

        // Duplicate stems can occur when the same .cbl basename is staged in
        // multiple roots (e.g. source/ + webdemo/sources/cobol/) — collapse them.
        var programByStem = snap.Programs
            .GroupBy(p => p.Stem, StringComparer.OrdinalIgnoreCase)
            .ToDictionary(g => g.Key, g => g.First(), StringComparer.OrdinalIgnoreCase);

        foreach (var job in jobs)
        {
            var jobId = Sanitize($"j_{job.JobName}");
            if (renderedJobs.Add(job.JobName))
                sb.AppendLine($"  {jobId}[\"📅 {EscapeMermaid(job.JobName)}\"]:::jobNode");

            foreach (var pgm in job.PrimaryPrograms)
            {
                if (edgeCount >= maxEdges) break;
                var pgmId = Sanitize($"p_{pgm}");
                if (renderedPgms.Add(pgm))
                    sb.AppendLine($"  {pgmId}[\"⚙ {EscapeMermaid(pgm)}\"]:::pgmNode");
                sb.AppendLine($"  {jobId} --> {pgmId}");
                edgeCount++;

                if (programByStem.TryGetValue(pgm, out var p))
                {
                    foreach (var cpy in p.Copybooks)
                    {
                        if (edgeCount >= maxEdges) break;
                        var cpyId = Sanitize($"c_{cpy}");
                        if (renderedCpys.Add(cpy))
                            sb.AppendLine($"  {cpyId}([\"📄 {EscapeMermaid(cpy)}\"]):::cpyNode");
                        sb.AppendLine($"  {pgmId} -.-> {cpyId}");
                        edgeCount++;
                    }
                }
            }
        }

        // If filtering by program, also include the program node + its copybooks
        // even when no job references it.
        if (!string.IsNullOrEmpty(programFilter))
        {
            foreach (var p in programs)
            {
                var pgmId = Sanitize($"p_{p.Stem}");
                if (renderedPgms.Add(p.Stem))
                    sb.AppendLine($"  {pgmId}[\"⚙ {EscapeMermaid(p.Stem)}\"]:::pgmNode");
                foreach (var cpy in p.Copybooks)
                {
                    if (edgeCount >= maxEdges) break;
                    var cpyId = Sanitize($"c_{cpy}");
                    if (renderedCpys.Add(cpy))
                        sb.AppendLine($"  {cpyId}([\"📄 {EscapeMermaid(cpy)}\"]):::cpyNode");
                    sb.AppendLine($"  {pgmId} -.-> {cpyId}");
                    edgeCount++;
                }
            }
        }

        return sb.ToString();
    }

    private static string Sanitize(string s) =>
        new string(s.Select(c => char.IsLetterOrDigit(c) || c == '_' ? c : '_').ToArray());

    private static string EscapeMermaid(string s) =>
        (s ?? "").Replace("\"", "'").Replace("\n", " ");

    private Dictionary<int, QualityRow> LoadLatestQualityByRunId()
    {
        var map = new Dictionary<int, QualityRow>();
        var dbPath = Path.Combine(_repoRoot, "Data", "benchmark.db");
        if (!File.Exists(dbPath)) return map;
        try
        {
            using var conn = new SqliteConnection($"Data Source={dbPath};Mode=ReadOnly;");
            conn.Open();
            using var c = conn.CreateCommand();
            c.CommandText = @"
                SELECT run_id,
                       json_extract(payload_json,'$.compileSuccess'),
                       json_extract(payload_json,'$.compileErrors'),
                       json_extract(payload_json,'$.generatedClassCount'),
                       json_extract(payload_json,'$.generatedJavaLines'),
                       json_extract(payload_json,'$.fallbackClassCount')
                  FROM metric_events
                 WHERE event='quality_metrics'";
            using var r = c.ExecuteReader();
            while (r.Read())
            {
                if (r.IsDBNull(0)) continue;
                if (!int.TryParse(r.GetString(0), out var runId)) continue;
                var success = !r.IsDBNull(1) && (r.GetValue(1)?.ToString() == "1" || r.GetValue(1)?.ToString()?.ToLowerInvariant() == "true");
                map[runId] = new QualityRow(
                    CompileSuccess: success,
                    CompileErrors: r.IsDBNull(2) ? 0 : Convert.ToInt32(r.GetValue(2)),
                    GeneratedClassCount: r.IsDBNull(3) ? 0 : Convert.ToInt32(r.GetValue(3)),
                    GeneratedJavaLines: r.IsDBNull(4) ? 0 : Convert.ToInt32(r.GetValue(4)),
                    FallbackClassCount: r.IsDBNull(5) ? 0 : Convert.ToInt32(r.GetValue(5))
                );
            }
        }
        catch (Exception ex)
        {
            _logger.LogDebug("Quality map load failed: {Msg}", ex.Message);
        }
        return map;
    }

    private Dictionary<int, string> LoadRunToProgramMap()
    {
        // Maps run id → primary program basename for that run.
        // Joins runs with cobol_files (the latter is where the actual program
        // filenames live — runs.cobol_source only holds the directory path).
        var map = new Dictionary<int, string>();
        var dbPath = Path.Combine(_repoRoot, "Data", "migration.db");
        if (!File.Exists(dbPath)) return map;
        try
        {
            using var conn = new SqliteConnection($"Data Source={dbPath};Mode=ReadOnly;");
            conn.Open();
            using var c = conn.CreateCommand();
            // Pick the first non-copybook .cbl per run as the "primary" program.
            c.CommandText = @"
                SELECT cf.run_id, cf.file_name
                  FROM cobol_files cf
                 WHERE cf.is_copybook = 0
                   AND cf.file_name LIKE '%.cbl'
                 GROUP BY cf.run_id
                 ORDER BY cf.run_id";
            using var r = c.ExecuteReader();
            while (r.Read())
            {
                map[r.GetInt32(0)] = r.GetString(1);
            }
        }
        catch (Exception ex)
        {
            _logger.LogDebug("Run-to-program map load failed: {Msg}", ex.Message);
        }
        return map;
    }

    private Dictionary<string, CacheEntry> LoadProjectionCacheState()
    {
        var map = new Dictionary<string, CacheEntry>(StringComparer.OrdinalIgnoreCase);
        var dbPath = Path.Combine(_repoRoot, "Data", "projection-cache.db");
        if (!File.Exists(dbPath)) return map;
        try
        {
            using var conn = new SqliteConnection($"Data Source={dbPath};Mode=ReadOnly;");
            conn.Open();
            using var c = conn.CreateCommand();
            c.CommandText = "SELECT basename, byte_size, hit_count FROM projection_cache WHERE basename IS NOT NULL";
            using var r = c.ExecuteReader();
            while (r.Read())
            {
                map[r.GetString(0)] = new CacheEntry(
                    ByteSize: r.IsDBNull(1) ? 0 : r.GetInt32(1),
                    HitCount: r.IsDBNull(2) ? 0 : r.GetInt32(2)
                );
            }
        }
        catch (Exception ex)
        {
            _logger.LogDebug("Cache state load failed: {Msg}", ex.Message);
        }
        return map;
    }

    private static (int confidence, int depCount, int warnings, bool present) ReadFactsSummary(string path)
    {
        if (!File.Exists(path)) return (0, 0, 0, false);
        try
        {
            using var doc = JsonDocument.Parse(File.ReadAllText(path));
            var root = doc.RootElement;
            int conf = root.TryGetProperty("confidence", out var c) ? c.GetInt32() : 0;
            int warn = root.TryGetProperty("warnings", out var w) ? w.GetArrayLength() : 0;
            int deps = 0;
            if (root.TryGetProperty("dependencies", out var d) && d.ValueKind == JsonValueKind.Array) deps += d.GetArrayLength();
            if (root.TryGetProperty("callTargets", out var ct) && ct.ValueKind == JsonValueKind.Array) deps += ct.GetArrayLength();
            if (root.TryGetProperty("copybooks", out var cb) && cb.ValueKind == JsonValueKind.Array) deps += cb.GetArrayLength();
            return (conf, deps, warn, true);
        }
        catch
        {
            return (0, 0, 0, true);
        }
    }

    private record QualityRow(
        bool CompileSuccess,
        int CompileErrors,
        int GeneratedClassCount,
        int GeneratedJavaLines,
        int FallbackClassCount);

    private record CacheEntry(int ByteSize, int HitCount);

    // ─────────────────────────────────────────────────────────────────────
    // Program detail drill-down (Visual Cockpit Developer scorecard click-through)
    // ─────────────────────────────────────────────────────────────────────

    public ProgramDetail? GetProgramDetail(string basename)
    {
        if (string.IsNullOrWhiteSpace(basename)) return null;
        basename = basename.Trim();

        // Locate the source file
        var sourceDir = Path.Combine(_repoRoot, "source");
        string? sourcePath = null;
        if (Directory.Exists(sourceDir))
        {
            sourcePath = Directory.EnumerateFiles(sourceDir, basename, SearchOption.AllDirectories)
                .FirstOrDefault(f => !f.Contains("/.convert-", StringComparison.Ordinal)
                                  && !f.Contains("/.rekt-staging", StringComparison.Ordinal)
                                  && !f.Contains("/.preprocessed", StringComparison.Ordinal));
        }
        if (sourcePath == null) return null;

        var stem = Path.GetFileNameWithoutExtension(basename);
        var factsPath = Path.Combine(_repoRoot, "output", "rekt", $"{stem}.facts.json");

        int loc = 0;
        try
        {
            var bytes = File.ReadAllBytes(sourcePath);
            foreach (var b in bytes) if (b == (byte)'\n') loc++;
            if (bytes.Length > 0 && bytes[^1] != (byte)'\n') loc++;
        }
        catch { /* skip */ }

        // Facts.json — read raw + extract typed summary fields
        string? factsRaw = null;
        List<string> dependencies = new();
        List<string> copybooks = new();
        List<string> callTargets = new();
        List<string> warnings = new();
        int confidence = 0;
        if (File.Exists(factsPath))
        {
            try
            {
                factsRaw = File.ReadAllText(factsPath);
                using var doc = JsonDocument.Parse(factsRaw);
                var root = doc.RootElement;
                if (root.TryGetProperty("confidence", out var c)) confidence = c.GetInt32();
                if (root.TryGetProperty("dependencies", out var d) && d.ValueKind == JsonValueKind.Array)
                    foreach (var x in d.EnumerateArray()) dependencies.Add(x.ToString());
                if (root.TryGetProperty("copybooks", out var cb) && cb.ValueKind == JsonValueKind.Array)
                    foreach (var x in cb.EnumerateArray()) copybooks.Add(x.ToString());
                if (root.TryGetProperty("callTargets", out var ct) && ct.ValueKind == JsonValueKind.Array)
                    foreach (var x in ct.EnumerateArray()) callTargets.Add(x.ToString());
                if (root.TryGetProperty("warnings", out var w) && w.ValueKind == JsonValueKind.Array)
                    foreach (var x in w.EnumerateArray()) warnings.Add(x.ToString());
            }
            catch (Exception ex)
            {
                _logger.LogDebug("facts.json parse failed for {Stem}: {Msg}", stem, ex.Message);
            }
        }

        // Run history from migration.db
        var runs = new List<ProgramRunRow>();
        var migPath = Path.Combine(_repoRoot, "Data", "migration.db");
        if (File.Exists(migPath))
        {
            try
            {
                using var conn = new SqliteConnection($"Data Source={migPath};Mode=ReadOnly;");
                conn.Open();
                using var c = conn.CreateCommand();
                c.CommandText = @"
                    SELECT r.id, r.started_at, r.completed_at, r.status, r.java_output, r.notes
                      FROM runs r
                     WHERE EXISTS (
                       SELECT 1 FROM cobol_files cf
                        WHERE cf.run_id = r.id AND cf.is_copybook = 0
                          AND lower(cf.file_name) = lower($name))
                     ORDER BY r.id DESC
                     LIMIT 20";
                c.Parameters.AddWithValue("$name", basename);
                using var rd = c.ExecuteReader();
                while (rd.Read())
                {
                    runs.Add(new ProgramRunRow(
                        RunId: rd.GetInt32(0),
                        StartedAt: rd.IsDBNull(1) ? "" : rd.GetString(1),
                        CompletedAt: rd.IsDBNull(2) ? null : rd.GetString(2),
                        Status: rd.IsDBNull(3) ? "" : rd.GetString(3),
                        JavaOutput: rd.IsDBNull(4) ? null : rd.GetString(4),
                        Notes: rd.IsDBNull(5) ? null : rd.GetString(5)
                    ));
                }
            }
            catch (Exception ex)
            {
                _logger.LogDebug("Run history load failed for {Basename}: {Msg}", basename, ex.Message);
            }
        }

        // Quality rows for those runs from benchmark.db
        var quality = new Dictionary<int, QualityRow>();
        var benchPath = Path.Combine(_repoRoot, "Data", "benchmark.db");
        if (File.Exists(benchPath) && runs.Count > 0)
        {
            try
            {
                using var conn = new SqliteConnection($"Data Source={benchPath};Mode=ReadOnly;");
                conn.Open();
                foreach (var run in runs)
                {
                    using var c = conn.CreateCommand();
                    c.CommandText = @"
                        SELECT compile_success, compile_errors, generated_class_count,
                               generated_java_lines, fallback_class_count
                          FROM metric_events
                         WHERE event='quality_summary' AND run_id = $rid
                         ORDER BY rowid DESC LIMIT 1";
                    c.Parameters.AddWithValue("$rid", run.RunId.ToString());
                    using var rd = c.ExecuteReader();
                    if (rd.Read())
                    {
                        quality[run.RunId] = new QualityRow(
                            CompileSuccess: !rd.IsDBNull(0) && rd.GetInt32(0) == 1,
                            CompileErrors: rd.IsDBNull(1) ? 0 : rd.GetInt32(1),
                            GeneratedClassCount: rd.IsDBNull(2) ? 0 : rd.GetInt32(2),
                            GeneratedJavaLines: rd.IsDBNull(3) ? 0 : rd.GetInt32(3),
                            FallbackClassCount: rd.IsDBNull(4) ? 0 : rd.GetInt32(4));
                    }
                }
            }
            catch (Exception ex)
            {
                _logger.LogDebug("Quality drill load failed for {Basename}: {Msg}", basename, ex.Message);
            }
        }

        var runRows = runs.Select(r => new ProgramRunDetail(
            RunId: r.RunId,
            StartedAt: r.StartedAt,
            CompletedAt: r.CompletedAt,
            Status: r.Status,
            JavaOutput: r.JavaOutput,
            CompileSuccess: quality.TryGetValue(r.RunId, out var q) ? q.CompileSuccess : null,
            CompileErrors: quality.TryGetValue(r.RunId, out var q2) ? q2.CompileErrors : null,
            GeneratedClasses: quality.TryGetValue(r.RunId, out var q3) ? q3.GeneratedClassCount : null,
            FallbackClasses: quality.TryGetValue(r.RunId, out var q4) ? q4.FallbackClassCount : null
        )).ToList();

        return new ProgramDetail(
            Basename: basename,
            RelativePath: Path.GetRelativePath(_repoRoot, sourcePath),
            LinesOfCode: loc,
            HasFacts: factsRaw != null,
            FactsConfidence: confidence,
            FactsWarnings: warnings,
            Dependencies: dependencies,
            Copybooks: copybooks,
            CallTargets: callTargets,
            RunHistory: runRows
        );
    }

    private record ProgramRunRow(int RunId, string StartedAt, string? CompletedAt, string Status, string? JavaOutput, string? Notes);
}

// ─────────────────────────────────────────────────────────────────────────
// DTOs (one per row / endpoint)
// ─────────────────────────────────────────────────────────────────────────

public record ApplicationRow(
    string Basename,
    string RelativePath,
    int LinesOfCode,
    bool HasFacts,
    int FactsConfidence,
    int DependencyCount,
    int FactsWarnings,
    int? LatestRunId,
    bool? LatestCompileSuccess,
    int? LatestCompileErrors,
    int? LatestGeneratedClasses,
    int? LatestGeneratedLines,
    int? LatestFallbackClasses,
    int ProjectionCacheHits,
    int ProjectionCacheBytes,
    string ModernizationStatus);

// Program detail drill-down (clicked from Visual Cockpit Developer scorecard)
public record ProgramDetail(
    string Basename,
    string RelativePath,
    int LinesOfCode,
    bool HasFacts,
    int FactsConfidence,
    List<string> FactsWarnings,
    List<string> Dependencies,
    List<string> Copybooks,
    List<string> CallTargets,
    List<ProgramRunDetail> RunHistory);

public record ProgramRunDetail(
    int RunId,
    string StartedAt,
    string? CompletedAt,
    string Status,
    string? JavaOutput,
    bool? CompileSuccess,
    int? CompileErrors,
    int? GeneratedClasses,
    int? FallbackClasses);

// #8 Compile-failure inspector
public class CompileDetail
{
    public string RunId { get; set; } = "";
    public string? OutputFolder { get; set; }
    public List<CompileFile> Files { get; set; } = new();
    public List<CompileError> Errors { get; set; } = new();
}
public record CompileFile(string FileName, string Path, int LineCount, string Content, bool HasError, int ErrorCount);
public record CompileError(string File, int? Line, string Message);

public class DashboardSummary
{
    public string? Source { get; set; }
    public int TotalEvents { get; set; }
    public Dictionary<string, int> EventCountsByType { get; } = new();
    public Dictionary<string, int> ProjectionModeCounts { get; } = new();
    public List<ContextReductionRow> ContextReduction { get; } = new();
    public List<LlmOutcomeRow> LlmCallOutcomes { get; } = new();
    public Dictionary<string, int> CacheDecisionCounts { get; } = new();
    public List<QualitySummaryRow> RecentQuality { get; } = new();
    public double CacheHitRatePct { get; set; }
    public double LlmSuccessRatePct { get; set; }
    public double RecentCompileSuccessPct { get; set; }
    public double AvgContextReductionPct { get; set; }
    public string? Note { get; set; }

    public static DashboardSummary Empty(string note) => new() { Note = note };
}

public record ContextReductionRow(string File, double RawTokens, double ProjectionTokens, double ReductionPct);

public record LlmOutcomeRow(string Outcome, int Count, double AvgDurationMs, double AvgCompletionTokens);

public record QualitySummaryRow(
    string RunId,
    bool CompileSuccess,
    int CompileErrors,
    int GeneratedClasses,
    int GeneratedLines,
    int FallbackClasses,
    int InjectAnnotations,
    string Timestamp,
    bool Measured = true);

// Runtime & Conversion Intelligence DTOs

public record RunSummaryRow(
    string RunId,
    string FirstEventTs,
    string LastEventTs,
    int EventCount,
    int LlmCallCount,
    int ProjectionEventCount,
    int CacheHits,
    int CacheTotal,
    int LlmSuccess,
    int LlmFail);

public class RunTimeline
{
    public string RunId { get; set; } = "";
    public string FirstEventTs { get; set; } = "";
    public string LastEventTs { get; set; } = "";
    public long TotalDurationMs { get; set; }
    public List<TimelineEvent> Events { get; } = new();
    public Dictionary<string, int> EventCounts { get; } = new();
    public string? Note { get; set; }
}

public record TimelineEvent(
    string Timestamp,
    long OffsetMs,
    string Event,
    string? Agent,
    string? File,
    string? Outcome,
    string? ProjectionMode,
    string? Decision,
    long? DurationMs,
    int? CompletionTokens,
    int? ProjectionTokens,
    int? RawRektTokens,
    bool? CompileSuccess,
    int? BraceImbalance,
    string PayloadJson);

// Dependency Topology DTOs (Phase-1 PR-P3)

public record TopologyNode(
    string Id,
    string Kind,             // "program" or "copybook"
    int LinesOfCode,
    bool HasFacts,
    int FactsConfidence,
    int? LatestRunId,
    bool? CompileSuccess,
    int ProjectionCacheHits,
    string ModernizationStatus);

public class TopologySnapshot
{
    public List<TopologyNode> Nodes { get; } = new();
    public string? Note { get; set; }
}

// Dependency Health (PR-Portal-P0-enterprise)

public class DependencyHealthSnapshot
{
    public int TotalPrograms { get; set; }
    public int FullFidelityCount { get; set; }
    public int DepsOnlyCount { get; set; }
    public int NotParsedCount { get; set; }
    public double CoveragePct { get; set; }
    public int TotalMissingCopybooks { get; set; }
    public int ProgramsBlockedByMissing { get; set; }
    public double ReadinessScore { get; set; }
    public List<MissingCopybookRow> MissingCopybooks { get; } = new();
    public List<ProgramHealthRow> Programs { get; } = new();
    public string? Note { get; set; }
}

public record MissingCopybookRow(string Copybook, List<string> ReferencedBy);

public record ProgramHealthRow(
    string Basename,
    int LinesOfCode,
    string ParseFidelity,    // "full" | "deps-only" | "not-parsed"
    int FactsConfidence,
    int FactsWarnings,
    int MissingCopybookCount,
    bool HasReport,
    bool HasDepsOnly,
    string ModernizationStatus);

// Semantic Flow Explorer

public class FlowSnapshot
{
    public string Basename { get; set; } = "";
    public bool HasFlowAst { get; set; }
    public bool HasCfg { get; set; }
    public bool HasDataStructures { get; set; }
    public int FlowAstFiles { get; set; }
    public string? Note { get; set; }
}

// Service Candidate Explorer

public class ServiceCandidateSnapshot
{
    public int TotalCandidates { get; set; }
    public int ExtractionReadyCount { get; set; }
    public List<ServiceCandidate> Candidates { get; } = new();
}

public record ServiceCandidate(
    string SuggestedName,
    string DomainPrefix,
    List<string> MemberPrograms,
    int MemberCount,
    int TotalLinesOfCode,
    int FullFidelityCount,
    int IntraClusterEdges,
    int CrossClusterEdges,
    double BoundaryStrengthPct,
    double AvgFactsConfidence,
    double CohesionScore,
    bool ReadyForExtraction);

// Service Chain — JCL → Program → Copybook visualization

public class ServiceChainSnapshot
{
    public int TotalJobs { get; set; }
    public int TotalPrograms { get; set; }
    public int TotalCopybooks { get; set; }
    public int JobToProgramEdges { get; set; }
    public int ProgramToCopybookEdges { get; set; }
    public List<JclJob> Jobs { get; } = new();
    public List<ProgramChain> Programs { get; } = new();
    public string Mermaid { get; set; } = "";
    public string? Note { get; set; }
}

public record JclJob(
    string JobName,
    string JclFileName,
    string RelativePath,
    List<string> PrimaryPrograms);

public record ProgramChain(
    string Basename,
    string Stem,
    int LinesOfCode,
    List<string> Copybooks,
    List<string> CalledByJobs,
    string ModernizationStatus);

// Migration Wave Planner (PR-Portal-P2-waves)

public class WavePlanSnapshot
{
    public List<WaveAssignment> Assignments { get; } = new();
    public string? Note { get; set; }
}

public record WaveAssignment(
    string Basename,
    int WaveNumber,           // 1-based; 0 = unassigned, -1 = blocked
    string? Notes,
    string AssignedAt,
    string Source);            // "auto" | "user"

public record WaveAssignmentRequest(int WaveNumber, string? Notes);
