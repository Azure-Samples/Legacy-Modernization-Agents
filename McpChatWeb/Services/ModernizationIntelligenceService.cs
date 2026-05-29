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
                // Match `wc -l` semantics: count line-feeds only. FUENTES corpus
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

            // Latest quality metrics (last 5 runs)
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

            // Derived headline metrics
            summary.TotalEvents = summary.EventCountsByType.Values.Sum();
            var hits = summary.CacheDecisionCounts.TryGetValue("hit", out var h) ? h : 0;
            var cacheTotal = summary.CacheDecisionCounts.Values.Sum();
            summary.CacheHitRatePct = cacheTotal > 0 ? Math.Round(hits * 100.0 / cacheTotal, 1) : 0;
            var llmTotal = summary.LlmCallOutcomes.Sum(o => o.Count);
            var llmSuccess = summary.LlmCallOutcomes.Where(o => o.Outcome == "success").Sum(o => o.Count);
            summary.LlmSuccessRatePct = llmTotal > 0 ? Math.Round(llmSuccess * 100.0 / llmTotal, 1) : 0;
            var compileRuns = summary.RecentQuality.Count;
            var compileOk = summary.RecentQuality.Count(q => q.CompileSuccess);
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
        var dbPath = Path.Combine(_repoRoot, "Data", "benchmark.db");
        if (!File.Exists(dbPath)) yield break;
        SqliteConnection? conn = null;
        try
        {
            conn = new SqliteConnection($"Data Source={dbPath};Mode=ReadOnly;");
            conn.Open();
        }
        catch (Exception ex)
        {
            _logger.LogDebug("GetRuns open failed: {Msg}", ex.Message);
            yield break;
        }

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
                 GROUP BY run_id
                 ORDER BY CAST(run_id AS INTEGER) DESC
                 LIMIT $lim";
            c.Parameters.AddWithValue("$lim", limit);
            using var r = c.ExecuteReader();
            while (r.Read())
            {
                yield return new RunSummaryRow(
                    RunId: r.GetString(0),
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
        var byBasename = apps.ToDictionary(a => a.Basename, a => a, StringComparer.OrdinalIgnoreCase);

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
    string Timestamp);

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
