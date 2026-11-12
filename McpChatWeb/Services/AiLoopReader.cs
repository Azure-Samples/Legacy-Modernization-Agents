using System.Text.Json;
using Microsoft.Data.Sqlite;
using System.Text.RegularExpressions;
using CobolToQuarkusMigration.Agents;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Jcl.Generation;

namespace McpChatWeb.Services;

public sealed record AiLoopRunSummary(
    string RunId,
    string? Mode,
    string? TargetLanguage,
    string Status,
    DateTime? StartedAt,
    DateTime? FinishedAt,
    long? DurationMs,
    int Calls,
    int FailedCalls,
    int Retries,
    int Fallbacks,
    string? CurrentStage,
    string? AttemptId = null,
    string? ManagedRunId = null,
    string RequestCoverage = "not-recorded",
    int? StartedRequests = null,
    int CancelledCalls = 0,
    int UnknownCalls = 0,
    int? OutstandingRequests = null,
    string? FailureReason = null,
    bool TelemetryAvailable = true,
    int? AttemptIndex = null,
    DateTime? LastEventAt = null,
    string ActivityKind = "model-attempt",
    string? ActivityId = null);

public sealed record AiLoopStage(int Number, int Total, string Name, DateTime StartedAt, long? DurationMs);

public sealed record AiLoopAgentStats(
    string Agent,
    IReadOnlyList<string> Models,
    int Calls,
    int FailedCalls,
    int Retries,
    int Fallbacks,
    long TotalMs,
    long? AvgMs,
    long? P95Ms,
    long PromptChars,
    long ResponseChars,
    long? InputTokens,
    long? OutputTokens,
    int? StartedRequests = null,
    int SucceededCalls = 0,
    int CancelledCalls = 0,
    int UnknownCalls = 0,
    int CallsWithTokenUsage = 0,
    int CallsWithDuration = 0);

public sealed record AiLoopGate(string Id, string Title, string Status, string Headline, IReadOnlyList<string> Details, string? Source);

public sealed record AiLoopEvent(DateTime Ts, string Kind, string? Agent, string? Context, string? Reason,
    string? RequestId = null, string? Outcome = null, int? Iteration = null);

public sealed record AiLoopRequest(string RequestId, string? Agent, string? Context, string? Model,
    DateTime StartedAt, string Status);

public sealed record AiLoopContext(DateTime Ts, string? Agent, string? File, string? Mode,
    string State, string Execution, string? ContextId = null, string? RequestId = null);

public sealed record AiLoopLogReference(string Path, string Kind, long? Messages, bool Available);

public sealed record AiLoopTelemetry(
    int RecordedEvents, int TimelineEvents, int ContextEvents, int UnreadableLines, int InvalidTimestamps);

public sealed record AiLoopRunDetail(
    AiLoopRunSummary Summary,
    string? OutputFolder,
    IReadOnlyList<AiLoopStage> Stages,
    IReadOnlyList<AiLoopAgentStats> Agents,
    IReadOnlyList<AiLoopGate> Gates,
    IReadOnlyList<AiLoopEvent> Events,
    string? Warning,
    IReadOnlyList<AiLoopRequest> Requests,
    IReadOnlyList<AiLoopContext> ContextHandoffs,
    IReadOnlyList<AiLoopRunSummary>? RelatedAttempts = null,
    IReadOnlyList<AiLoopLogReference>? Logs = null,
    AiLoopTelemetry? Telemetry = null,
    NativeScanProgress? Native = null);

public sealed class AiLoopOptions
{
    public const string SectionName = "AiLoop";

    // Newest recorded attempts listed; older ones stay on disk and are still readable by id.
    public int MaxRuns { get; set; } = 50;
    // Retries, fallbacks and failed calls kept for the timeline, newest last.
    public int MaxTimelineEvents { get; set; } = 200;
    // A run with no finish event and nothing written for this long is reported as interrupted.
    public int StaleRunMinutes { get; set; } = 30;
    // Fallbacks a run may take before the model gate warns. Each one is a stub where code should be.
    public int MaxFallbacks { get; set; } = 0;
    // Share of model calls that may fail (and be retried) before the model gate warns.
    public double MaxFailedCallRate { get; set; } = 0.1;
    // How often the open AI Loop tab refreshes a run that is still going.
    public int PollSeconds { get; set; } = 5;

    public static AiLoopOptions Load(string appSettingsPath, out string? warning)
    {
        warning = null;
        if (!File.Exists(appSettingsPath)) return new();
        try
        {
            using var doc = JsonDocument.Parse(File.ReadAllText(appSettingsPath),
                new JsonDocumentOptions { CommentHandling = JsonCommentHandling.Skip, AllowTrailingCommas = true });
            return doc.RootElement.TryGetProperty(SectionName, out var section)
                ? section.Deserialize<AiLoopOptions>(new JsonSerializerOptions { PropertyNameCaseInsensitive = true }) ?? new()
                : new();
        }
        catch (Exception ex) when (ex is JsonException or IOException)
        {
            warning = $"{SectionName} in {appSettingsPath} could not be read ({ex.Message}); using defaults.";
            return new();
        }
    }
}

// Reads the AI loop of a run back from the events it emitted (Helpers/AiLoopEvents) and the quality
// gate artifacts it left in its output folder. Nothing here calls a model or changes a file.
public sealed partial class AiLoopReader
{
    public const string MetricsFolder = "output/.metrics";

    private readonly ILogger<AiLoopReader> _logger;

    public AiLoopReader(ILogger<AiLoopReader> logger, string? repoRoot = null)
    {
        _logger = logger;
        RepoRoot = repoRoot ?? RepositoryRoot.Resolve();
    }

    public string RepoRoot { get; }

    [GeneratedRegex("^[A-Za-z0-9_-]{1,64}$")]
    private static partial Regex RunIdPattern();

    public static bool IsValidRunId(string? runId) => runId is not null && RunIdPattern().IsMatch(runId);

    public int PollSeconds => Math.Max(1, LoadOptions(out _).PollSeconds);

    private AiLoopOptions LoadOptions(out string? warning)
        => AiLoopOptions.Load(Path.Join(RepoRoot, "Config", "appsettings.json"), out warning);

    public async Task<IReadOnlyList<AiLoopRunSummary>> ListRunsAsync(CancellationToken ct = default,
        int offset = 0, int? limit = null)
    {
        var options = LoadOptions(out _);
        var dir = Path.Join(RepoRoot, MetricsFolder);
        var ended = await EndedRunStatusesAsync(ct);
        var result = new List<AiLoopRunSummary>();
        foreach (var file in (Directory.Exists(dir) ? new DirectoryInfo(dir).EnumerateFiles("*.jsonl") : [])
                     .OrderByDescending(f => f.LastWriteTimeUtc))
        {
            var runId = Path.GetFileNameWithoutExtension(file.Name);
            if (!IsValidRunId(runId)) continue;
            var attempts = Attempts((await ReadEventsAsync(file.FullName, ct)).Events);
            foreach (var events in attempts)
            {
                // Older runs only have projection and cache metrics; they say nothing about the loop.
                if (!events.Any(e => e.Event is AiLoopEvents.RunStarted or AiLoopEvents.LlmCall)) continue;
                result.Add(Summarise(runId, events, file.LastWriteTimeUtc, options, ended.GetValueOrDefault(runId),
                    !ReferenceEquals(events, attempts.Last())) with
                { AttemptIndex = attempts.IndexOf(events),
                    ActivityId = $"model:{runId}:{events.FirstOrDefault(e => e.Event == AiLoopEvents.RunStarted)?.Str("attemptId") ?? "@" + attempts.IndexOf(events)}" });
            }
        }
        foreach (var historical in await DatabaseRunsAsync(ct))
            if (!result.Any(r => r.RunId == historical.RunId)) result.Add(historical);
        result.AddRange(NativeActivities().Select(d => d.Summary));
        return result.OrderByDescending(r => r.LastEventAt ?? r.FinishedAt ?? r.StartedAt).ThenByDescending(r => r.RunId)
            .Skip(Math.Max(0, offset)).Take(Math.Clamp(limit ?? options.MaxRuns, 1, 201)).ToList();
    }

    public async Task<AiLoopRunDetail?> GetManagedRunAsync(string managedRunId, CancellationToken ct = default)
    {
        if (!IsValidRunId(managedRunId)) return null;
        var dir = Path.Join(RepoRoot, MetricsFolder);
        if (!Directory.Exists(dir)) return null;

        // Managed process ids and numeric migration ids are separate namespaces. Only an explicit
        // run_started association establishes identity, including for runs outside the list limit.
        var matches = new List<AiLoopRunSummary>();
        var options = LoadOptions(out _);
        var ended = await EndedRunStatusesAsync(ct);
        foreach (var file in new DirectoryInfo(dir).EnumerateFiles("*.jsonl"))
        {
            var runId = Path.GetFileNameWithoutExtension(file.Name);
            if (!IsValidRunId(runId)) continue;
            var attempts = Attempts((await ReadEventsAsync(file.FullName, ct)).Events);
            foreach (var events in attempts)
            {
                if (events.FirstOrDefault(e => e.Event == AiLoopEvents.RunStarted)?.Str("managedRunId") != managedRunId)
                    continue;
                matches.Add(Summarise(runId, events, file.LastWriteTimeUtc, options, ended.GetValueOrDefault(runId),
                    !ReferenceEquals(events, attempts.Last())) with
                { AttemptIndex = attempts.IndexOf(events),
                    ActivityId = $"model:{runId}:{events.FirstOrDefault(e => e.Event == AiLoopEvents.RunStarted)?.Str("attemptId") ?? "@" + attempts.IndexOf(events)}" });
            }
        }
        var latest = matches.OrderByDescending(a => a.StartedAt).FirstOrDefault();
        if (latest is null) return null;
        var detail = await GetRunAsync(latest.RunId, ct, latest.AttemptId, latest.AttemptIndex);
        return detail?.Summary.ManagedRunId == managedRunId
            ? detail with { RelatedAttempts = matches.OrderByDescending(a => a.StartedAt).ToList() } : null;
    }

    public async Task<AiLoopRunDetail?> GetRunAsync(string runId, CancellationToken ct = default,
        string? attemptId = null, int? attemptIndex = null)
    {
        if (!IsValidRunId(runId)) return null;
        if (runId.StartsWith("native-", StringComparison.Ordinal))
            return attemptId is null && attemptIndex is null
                ? NativeActivities().FirstOrDefault(d => d.Summary.RunId == runId) : null;
        var path = Path.Join(RepoRoot, MetricsFolder, runId + ".jsonl");
        if (!File.Exists(path))
        {
            if (attemptId is not null || attemptIndex is not null) return null;
            return await HistoricalRunAsync(runId, ct);
        }

        var options = LoadOptions(out var warning);
        var read = await ReadEventsAsync(path, ct);
        var attempts = Attempts(read.Events);
        if (read.UnreadableLines > 0 || read.InvalidTimestamps > 0)
            warning = string.Join(" ", new[] { warning,
                $"Metrics file contains {read.UnreadableLines} unreadable line(s) (possibly an in-progress append) and {read.InvalidTimestamps} unrecorded/invalid timestamp(s). Refresh retries the whole file; missing evidence is not reconstructed." }
                .Where(s => !string.IsNullOrEmpty(s)));
        var events = attemptId is not null
            ? attempts.LastOrDefault(a => a.FirstOrDefault(e => e.Event == AiLoopEvents.RunStarted)?.Str("attemptId") == attemptId)
            : attemptIndex is { } index ? attempts.ElementAtOrDefault(index) : attempts.LastOrDefault();
        if (events is null)
            return attemptId is null && attemptIndex is null ? await HistoricalRunAsync(runId, ct) : null;
        if (!events.Any(e => e.Event is AiLoopEvents.RunStarted or AiLoopEvents.LlmCall))
            return attemptId is null ? await HistoricalRunAsync(runId, ct) : null;
        var ended = await EndedRunStatusesAsync(ct);
        var summary = Summarise(runId, events, File.GetLastWriteTimeUtc(path), options, ended.GetValueOrDefault(runId),
            !ReferenceEquals(events, attempts.Last())) with
        { AttemptIndex = attempts.IndexOf(events),
            ActivityId = $"model:{runId}:{events.FirstOrDefault(e => e.Event == AiLoopEvents.RunStarted)?.Str("attemptId") ?? "@" + attempts.IndexOf(events)}" };
        var started = events.LastOrDefault(e => e.Event == AiLoopEvents.RunStarted);
        var folder = started?.Str("outputFolder");

        var gates = new List<AiLoopGate> { ModelGate(summary, options) };
        var validation = events.LastOrDefault(e => e.Event == "validation_completed");
        if (validation is not null)
        {
            string BoolText(string key) => validation.Bool(key)?.ToString().ToLowerInvariant() ?? "not recorded";
            gates.Add(new AiLoopGate("acceptance", "Recorded output acceptance policy",
                validation.Bool("accepted") is true ? "pass" : validation.Bool("accepted") is false ? "fail" : "unknown",
                validation.Bool("accepted") is true ? "Accepted by the configured policy; not proof of behavioral equivalence"
                    : "Output was not accepted by the configured policy",
                [$"Complete output: {BoolText("outputComplete")}",
                    $"Compilation enabled: {BoolText("compileEnabled")}; measured: {BoolText("compileMeasured")}; compiled: {BoolText("compiled")}",
                    $"Parity evaluated: {validation.Long("parityEvaluated")?.ToString() ?? "not recorded"}; below threshold: {validation.Long("parityBelowThreshold")?.ToString() ?? "not recorded"}; not evaluated: {validation.Long("parityNotEvaluated")?.ToString() ?? "not recorded"}; strict gate: {BoolText("strictParityGate")}"],
                Rel(path)));
        }
        var resolved = ResolveOutputFolder(folder, out var folderNote);
        if (!ReferenceEquals(events, attempts.Last()) && attempts.Skip(attempts.IndexOf(events) + 1)
            .Any(a => a.FirstOrDefault(e => e.Event == AiLoopEvents.RunStarted)?.Str("outputFolder") == folder))
        {
            foreach (var (id, title) in new[] { ("compile", "Compile"), ("parity", "Parity"), ("jcl", "JCL") })
                gates.Add(new AiLoopGate(id, title, "unknown",
                    "Historical artifact snapshot not recorded",
                    ["A later attempt reused this output folder. Its current artifacts cannot establish this attempt's verification result."], null));
        }
        else
        {
            gates.Add(await CompileGateAsync(resolved, summary.TargetLanguage, folderNote, summary.StartedAt, ct));
            gates.Add(await ParityGateAsync(resolved, folderNote, summary.StartedAt, ct));
            gates.Add(await JclGateAsync(resolved, folderNote, summary.StartedAt, ct));
        }

        var timelineEvents = events
            .Where(e => e.Event is AiLoopEvents.RunStarted or AiLoopEvents.RunFinished or AiLoopEvents.Stage
                or AiLoopEvents.LlmRequestStarted or AiLoopEvents.LlmRetry or AiLoopEvents.LlmFallback or AiLoopEvents.LlmCall or "validation_completed" or "chat_log_saved")
            .OrderBy(e => e.Ts).ToList();
        var timeline = timelineEvents
            .TakeLast(Math.Max(0, options.MaxTimelineEvents))
            .Select(e => new AiLoopEvent(e.Ts, e.Event!, e.Str("agent"), e.Str("context"),
                TimelineReason(e), e.Str("requestId"), Outcome(e), e.Long("attempt") is { } iteration ? (int)Math.Clamp(iteration, 0, int.MaxValue) : null))
            .ToList();

        var completed = events.Where(e => e.Event == AiLoopEvents.LlmCall && e.Str("requestId") is not null)
            .Select(e => e.Str("requestId")!).ToHashSet(StringComparer.Ordinal);
        var requests = events.Where(e => e.Event == AiLoopEvents.LlmRequestStarted && e.Str("requestId") is not null
                && !completed.Contains(e.Str("requestId")!))
            .Select(e => new AiLoopRequest(e.Str("requestId")!, e.Str("agent"), e.Str("context"), e.Str("model"),
                e.Ts, summary.Status == "running" ? "in-flight" : "unknown")).ToList();
        var contextEvents = events.Where(e => e.Event is "projection_metrics" or "context_dispatched").ToList();
        var contexts = contextEvents
            .OrderBy(e => e.Ts)
            .Select(e => new AiLoopContext(e.Ts, e.Str("agent"), e.Str("file"), e.Str("projectionMode"),
                e.Event == "context_dispatched" ? "dispatched" : "prepared",
                e.Str("projectionMode") is "projection" or "raw-rekt" ? "reused"
                    : e.Str("projectionMode") == "none" ? "unavailable" : "not-recorded",
                e.Str("contextId"), e.Str("requestId")))
            .TakeLast(Math.Max(0, options.MaxTimelineEvents)).ToList();
        return new AiLoopRunDetail(summary, folder, Stages(events, summary), AgentStats(events), gates, timeline, warning,
            requests, contexts, attempts.Select(a => Summarise(runId, a, File.GetLastWriteTimeUtc(path), options,
                ended.GetValueOrDefault(runId), !ReferenceEquals(a, attempts.Last())) with
            { AttemptIndex = attempts.IndexOf(a) }).ToList(), LogReferences(events, runId),
            new AiLoopTelemetry(events.Count, timelineEvents.Count, contextEvents.Count, read.UnreadableLines, read.InvalidTimestamps));
    }

    private static string? TimelineReason(MetricEvent e)
        => e.Event == AiLoopEvents.Stage ? e.Str("name")
            : e.Event == AiLoopEvents.RunFinished ? FailureReason(e) ?? e.Str("status")
            : e.Event == "validation_completed" ? e.Bool("accepted") is true ? "Accepted by configured output policy, not behavioral equivalence" : "Rejected by configured output policy"
            : e.Event == AiLoopEvents.LlmCall ? Outcome(e) : e.Str("reason");

    private static string? Outcome(MetricEvent e)
        => e.Event != AiLoopEvents.LlmCall ? null
            : e.Str("outcome") is "succeeded" or "failed" or "cancelled" or "unknown" ? e.Str("outcome")
            : e.Bool("success") is true ? "succeeded"
            : e.Bool("success") is false ? "failed" : "unknown";

    private static string? FailureReason(MetricEvent? finished)
        => finished?.Str("errorType") is { } error
            ? error == "ValidationFailed" ? "Output validation rejected this conversion. Inspect recorded acceptance, compile and parity evidence."
                : $"Run ended with {error}. Inspect the associated process log and last recorded stage."
            : finished?.Str("reason");

    private IReadOnlyList<AiLoopLogReference> LogReferences(List<MetricEvent> events, string runId)
    {
        var result = new List<AiLoopLogReference>
        {
            new($"{MetricsFolder}/{runId}.jsonl", "Structured attempt events", null, true)
        };
        foreach (var entry in events.Where(e => e.Event == "chat_log_saved").TakeLast(200))
        {
            var relative = entry.Str("path");
            if (string.IsNullOrWhiteSpace(relative) || relative.Contains('\0') || Path.IsPathRooted(relative)
                || relative.Split('/', '\\').Any(segment => segment == "..")) continue;
            var full = Path.GetFullPath(Path.Combine(RepoRoot, relative));
            if (!full.StartsWith(Path.GetFullPath(RepoRoot) + Path.DirectorySeparatorChar, StringComparison.Ordinal)) continue;
            result.Add(new(relative.Replace('\\', '/'), "Correlated model conversation export", entry.Long("messageCount"),
                File.Exists(full)));
        }
        return result.DistinctBy(log => log.Path).ToList();
    }

    private static List<List<MetricEvent>> Attempts(List<MetricEvent> events)
    {
        var result = new List<List<MetricEvent>>();
        var byId = new Dictionary<string, List<MetricEvent>>(StringComparer.Ordinal);
        List<MetricEvent>? current = null;
        foreach (var entry in events)
        {
            if (entry.Event == AiLoopEvents.RunStarted || current is null)
            {
                current = [];
                result.Add(current);
                if (entry.Event == AiLoopEvents.RunStarted && entry.Str("attemptId") is { } id)
                    byId[id] = current;
            }
            if (entry.Str("attemptId") is { } attemptId && byId.TryGetValue(attemptId, out var associated))
                associated.Add(entry);
            else if (entry.Str("attemptId") is null)
                current.Add(entry);
        }
        return result;
    }

    public int PageSize => Math.Clamp(LoadOptions(out _).MaxRuns, 1, 200);

    private async Task<AiLoopRunDetail?> HistoricalRunAsync(string runId, CancellationToken ct)
    {
        var historical = (await DatabaseRunsAsync(ct)).FirstOrDefault(r => r.RunId == runId);
        return historical is null ? null : new AiLoopRunDetail(historical, null, [], [], [], [],
            "This persisted migration run has no recorded AI Loop telemetry. Model calls, stages, iterations, logs and artifact association cannot be reconstructed. Status is the persisted database value, not proof that a process is still active or verification passed.", [], []);
    }

    private async Task<List<AiLoopRunSummary>> DatabaseRunsAsync(CancellationToken ct)
    {
        var result = new List<AiLoopRunSummary>();
        var path = Path.Combine(RepoRoot, "Data", "migration.db");
        if (!File.Exists(path)) return result;
        try
        {
            await using var connection = new SqliteConnection(new SqliteConnectionStringBuilder
            {
                DataSource = path,
                Mode = SqliteOpenMode.ReadOnly,
                Pooling = false
            }.ToString());
            await connection.OpenAsync(ct);
            await using var command = connection.CreateCommand();
            command.CommandText = "SELECT id, status, started_at, completed_at FROM runs ORDER BY id DESC";
            await using var reader = await command.ExecuteReaderAsync(ct);
            while (await reader.ReadAsync(ct))
            {
                DateTime? Time(int index) => !reader.IsDBNull(index) && DateTime.TryParse(reader.GetString(index),
                    System.Globalization.CultureInfo.InvariantCulture,
                    System.Globalization.DateTimeStyles.AssumeUniversal | System.Globalization.DateTimeStyles.AdjustToUniversal,
                    out var time) ? time : null;
                result.Add(new AiLoopRunSummary(reader.GetInt64(0).ToString(System.Globalization.CultureInfo.InvariantCulture),
                    null, null, reader.IsDBNull(1) ? "unknown" : reader.GetString(1).ToLowerInvariant(),
                    Time(2), Time(3), null, 0, 0, 0, 0, null, TelemetryAvailable: false,
                    ActivityKind: "migration-history", ActivityId: "migration:" + reader.GetInt64(0)));
            }
        }
        catch (SqliteException ex)
        {
            _logger.LogDebug("Historical run metadata unavailable: {Message}", ex.Message);
        }
        return result;
    }

    // ── events ──────────────────────────────────────────────────────────────

    internal sealed record MetricEvent(DateTime Ts, string? Event, JsonElement Root)
    {
        public string? Str(string name) => Root.TryGetProperty(name, out var v) && v.ValueKind == JsonValueKind.String ? v.GetString() : null;
        public long? Long(string name) => Root.TryGetProperty(name, out var v) && v.ValueKind == JsonValueKind.Number && v.TryGetInt64(out var n) ? n : null;
        public bool? Bool(string name) => Root.TryGetProperty(name, out var v) && v.ValueKind is JsonValueKind.True or JsonValueKind.False ? v.GetBoolean() : null;
    }

    private sealed record EventReadResult(List<MetricEvent> Events, int UnreadableLines, int InvalidTimestamps);

    private async Task<EventReadResult> ReadEventsAsync(string path, CancellationToken ct)
    {
        var list = new List<MetricEvent>();
        var unreadable = 0;
        var invalidTimestamps = 0;
        try
        {
            // The run may still be appending; share the file rather than lock it.
            await using var stream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite | FileShare.Delete);
            using var reader = new StreamReader(stream);
            string? line;
            while ((line = await reader.ReadLineAsync(ct)) is not null)
            {
                if (string.IsNullOrWhiteSpace(line)) continue;
                try
                {
                    using var doc = JsonDocument.Parse(line);
                    var root = doc.RootElement.Clone();
                    if (root.ValueKind != JsonValueKind.Object) { unreadable++; continue; }
                    var ts = root.TryGetProperty("ts", out var t) && t.ValueKind == JsonValueKind.String
                        && DateTimeOffset.TryParse(t.GetString(), System.Globalization.CultureInfo.InvariantCulture,
                            System.Globalization.DateTimeStyles.AssumeUniversal | System.Globalization.DateTimeStyles.AdjustToUniversal,
                            out var d) ? d.UtcDateTime : DateTime.SpecifyKind(DateTime.MinValue, DateTimeKind.Utc);
                    if (ts == DateTime.MinValue) invalidTimestamps++;
                    var ev = root.TryGetProperty("event", out var e) && e.ValueKind == JsonValueKind.String ? e.GetString() : null;
                    list.Add(new MetricEvent(ts, ev, root));
                }
                catch (JsonException)
                {
                    // A line cut short by a run that is still writing is skipped, not fatal.
                    unreadable++;
                }
            }
        }
        catch (IOException ex)
        {
            _logger.LogWarning("Could not read metrics {Path}: {Message}", path, ex.Message);
        }
        return new(list, unreadable, invalidTimestamps);
    }

    // A killed run never writes run_finished, but the next start marks it ended in the run database.
    // That record wins over the stale-time guess, so a dead run does not show as running beside its successor.
    private async Task<Dictionary<string, (string Status, DateTime? FinishedAt)>> EndedRunStatusesAsync(CancellationToken ct)
    {
        var result = new Dictionary<string, (string Status, DateTime? FinishedAt)>(StringComparer.Ordinal);
        var db = Path.Join(RepoRoot, "Data", "migration.db");
        if (!File.Exists(db)) return result;
        try
        {
            await using var connection = new SqliteConnection(new SqliteConnectionStringBuilder
            {
                DataSource = db,
                Mode = SqliteOpenMode.ReadOnly,
                Pooling = false,
            }.ToString());
            await connection.OpenAsync(ct);
            await using var command = connection.CreateCommand();
            command.CommandText = "SELECT id, status, completed_at FROM runs WHERE status IS NOT NULL AND lower(status) <> 'running'";
            await using var reader = await command.ExecuteReaderAsync(ct);
            while (await reader.ReadAsync(ct))
            {
                var finished = !reader.IsDBNull(2) && DateTime.TryParse(reader.GetString(2),
                    System.Globalization.CultureInfo.InvariantCulture,
                    System.Globalization.DateTimeStyles.AdjustToUniversal | System.Globalization.DateTimeStyles.AssumeUniversal,
                    out var time) ? time : (DateTime?)null;
                result[reader.GetInt64(0).ToString(System.Globalization.CultureInfo.InvariantCulture)] =
                    (reader.GetString(1).ToLowerInvariant(), finished);
            }
        }
        catch (SqliteException ex)
        {
            _logger.LogDebug("Run database not readable for AI Loop status: {Message}", ex.Message);
        }
        return result;
    }

    private static AiLoopRunSummary Summarise(string runId, List<MetricEvent> events, DateTime lastWriteUtc, AiLoopOptions options,
        (string Status, DateTime? FinishedAt) ended = default, bool superseded = false)
    {
        var started = events.LastOrDefault(e => e.Event == AiLoopEvents.RunStarted);
        var finished = events.LastOrDefault(e => e.Event == AiLoopEvents.RunFinished);
        var calls = events.Where(e => e.Event == AiLoopEvents.LlmCall).ToList();
        var completedRequestIds = calls.Select(c => c.Str("requestId")).OfType<string>().ToHashSet(StringComparer.Ordinal);
        var stage = events.LastOrDefault(e => e.Event == AiLoopEvents.Stage);

        var endedStatus = !superseded && ended.FinishedAt >= started?.Ts ? ended.Status : null;
        var status = finished is not null ? finished.Str("status") ?? "unknown"
                     : endedStatus
                     ?? (superseded ? "interrupted" : started is null ? "unknown"
                         : DateTime.UtcNow - lastWriteUtc > TimeSpan.FromMinutes(Math.Max(1, options.StaleRunMinutes))
                         ? "interrupted"
                         : "running");
        var startedAt = started?.Ts ?? events.FirstOrDefault()?.Ts;

        return new AiLoopRunSummary(
            runId,
            started?.Str("mode"),
            started?.Str("targetLanguage"),
            status,
            startedAt,
            finished?.Ts ?? (endedStatus is not null ? ended.FinishedAt : null),
            finished?.Long("durationMs") ?? (endedStatus is not null && ended.FinishedAt is { } end && startedAt is { } start
                ? Math.Max(0, (long)(end - start).TotalMilliseconds) : null),
            calls.Count,
            calls.Count(c => Outcome(c) == "failed"),
            events.Count(e => e.Event == AiLoopEvents.LlmRetry),
            events.Count(e => e.Event == AiLoopEvents.LlmFallback),
            stage?.Str("name"),
            started?.Str("attemptId"),
            started?.Str("managedRunId"),
            started?.Long("telemetryVersion") >= 2 ? "agent-boundary"
                : calls.Count > 0 ? "legacy-outcomes-only" : "not-recorded",
            started?.Long("telemetryVersion") >= 2 ? events.Count(e => e.Event == AiLoopEvents.LlmRequestStarted) : null,
            calls.Count(c => Outcome(c) == "cancelled"),
            calls.Count(c => Outcome(c) == "unknown"),
            started?.Long("telemetryVersion") >= 2 ? events.Count(e => e.Event == AiLoopEvents.LlmRequestStarted
                && e.Str("requestId") is { } id && !completedRequestIds.Contains(id)) : null,
            FailureReason(finished) ?? (superseded && finished is null
                ? "A later attempt started without a recorded finish for this attempt. Its completion outcome and exit time are unknown." : null),
            LastEventAt: events.Where(e => e.Ts != DateTime.MinValue).Select(e => (DateTime?)e.Ts).Max());
    }

    private static List<AiLoopStage> Stages(List<MetricEvent> events, AiLoopRunSummary summary)
    {
        var stages = events.Where(e => e.Event == AiLoopEvents.Stage).ToList();
        var end = summary.FinishedAt;
        return stages.Select((s, i) =>
        {
            DateTime? until = i + 1 < stages.Count ? stages[i + 1].Ts : end;
            return new AiLoopStage((int)(s.Long("number") ?? 0), (int)(s.Long("total") ?? 0), s.Str("name") ?? "",
                s.Ts, until is { } u ? (long)(u - s.Ts).TotalMilliseconds : null);
        }).ToList();
    }

    private static List<AiLoopAgentStats> AgentStats(List<MetricEvent> events)
    {
        var byAgent = events
            .Where(e => e.Event is AiLoopEvents.LlmRequestStarted or AiLoopEvents.LlmCall or AiLoopEvents.LlmRetry or AiLoopEvents.LlmFallback)
            .GroupBy(e => e.Str("agent") ?? "unknown", StringComparer.Ordinal);

        return byAgent.Select(g =>
        {
            var calls = g.Where(e => e.Event == AiLoopEvents.LlmCall).ToList();
            var durations = calls.Select(c => c.Long("durationMs")).OfType<long>().Where(d => d >= 0).OrderBy(x => x).ToList();
            var withTokens = calls.Where(c => c.Long("inputTokens") is not null && c.Long("outputTokens") is not null).ToList();
            return new AiLoopAgentStats(
                g.Key,
                calls.Select(c => c.Str("model")).Where(m => !string.IsNullOrEmpty(m)).Distinct().Order().ToList()!,
                calls.Count,
                calls.Count(c => Outcome(c) == "failed"),
                g.Count(e => e.Event == AiLoopEvents.LlmRetry),
                g.Count(e => e.Event == AiLoopEvents.LlmFallback),
                durations.Sum(),
                durations.Count == 0 ? null : durations.Sum() / durations.Count,
                durations.Count == 0 ? null : durations[Math.Clamp((int)Math.Ceiling(0.95 * durations.Count) - 1, 0, durations.Count - 1)],
                calls.Sum(c => c.Long("promptChars") ?? 0),
                calls.Sum(c => c.Long("responseChars") ?? 0),
                withTokens.Count == 0 ? null : withTokens.Sum(c => c.Long("inputTokens") ?? 0),
                withTokens.Count == 0 ? null : withTokens.Sum(c => c.Long("outputTokens") ?? 0),
                events.FirstOrDefault(e => e.Event == AiLoopEvents.RunStarted)?.Long("telemetryVersion") >= 2
                    ? g.Count(e => e.Event == AiLoopEvents.LlmRequestStarted) : null,
                calls.Count(c => Outcome(c) == "succeeded"),
                calls.Count(c => Outcome(c) == "cancelled"),
                calls.Count(c => Outcome(c) == "unknown"),
                withTokens.Count,
                durations.Count);
        })
        .OrderByDescending(a => a.Calls).ThenBy(a => a.Agent, StringComparer.Ordinal)
        .ToList();
    }

    // ── gates ───────────────────────────────────────────────────────────────

    private static AiLoopGate ModelGate(AiLoopRunSummary s, AiLoopOptions options)
    {
        if (s.Calls == 0)
            return s.OutstandingRequests > 0
                ? new("model", "Model loop", "warn", "Recorded requests have no outcomes",
                    [$"{s.OutstandingRequests} outstanding; run status: {s.Status}"], null)
                : new("model", "Model loop", "not-run", "No model outcomes recorded", [], null);
        if (s.UnknownCalls > 0 || s.CancelledCalls > 0 || s.OutstandingRequests > 0)
            return new("model", "Model loop", "warn", "The recorded calls do not establish successful completion",
                [$"{s.CancelledCalls} cancelled, {s.UnknownCalls} unknown, {s.OutstandingRequests?.ToString() ?? "unrecorded"} outstanding; run status: {s.Status}"], null);

        var details = new List<string>
        {
            $"{s.Calls} call(s), {s.FailedCalls} failed, {s.Retries} retried, {s.Fallbacks} fell back to a stub",
            $"Coverage: {s.RequestCoverage}; only recorded agent outcomes are evaluated.",
            $"Run status: {s.Status}. Model-call outcomes do not establish output acceptance, compilation or behavioral equivalence.",
        };
        var failedRate = (double)s.FailedCalls / s.Calls;
        string status = "pass";
        if (s.Fallbacks > options.MaxFallbacks)
        {
            status = "warn";
            details.Add($"{s.Fallbacks} fallback(s): those programs have a stub where converted code should be.");
        }
        if (failedRate > options.MaxFailedCallRate)
        {
            status = "warn";
            details.Add($"{failedRate:P0} of calls failed (allowed {options.MaxFailedCallRate:P0}).");
        }
        var headline = status == "pass"
            ? $"{s.Calls} call(s), {s.FailedCalls} failed, {s.Fallbacks} fallback(s)"
            : $"{s.Fallbacks} fallback(s), {s.FailedCalls} failed call(s)";
        return new("model", "Model loop", status, headline, details, null);
    }

    private string? ResolveOutputFolder(string? folder, out string? note)
    {
        note = null;
        if (string.IsNullOrWhiteSpace(folder))
        {
            note = "The run did not record its output folder.";
            return null;
        }
        // The folder comes from a file on disk; it is only followed if it stays under output/.
        var outputRoot = Path.GetFullPath(Path.Join(RepoRoot, "output")) + Path.DirectorySeparatorChar;
        var full = Path.GetFullPath(Path.IsPathRooted(folder) ? folder : Path.Join(RepoRoot, folder));
        if (!full.StartsWith(outputRoot, StringComparison.Ordinal))
        {
            note = "The run's output folder is outside the repository's output folder, so it is not read.";
            return null;
        }
        if (!Directory.Exists(full))
        {
            note = "The run's output folder no longer exists.";
            return null;
        }
        return full;
    }

    private string Rel(string full) => Path.GetRelativePath(RepoRoot, full).Replace('\\', '/');

    private static bool IsEarlierArtifact(string path, DateTime? attemptStart)
        => File.Exists(path) && attemptStart is { } start && File.GetLastWriteTimeUtc(path) < start;

    private async Task<AiLoopGate> CompileGateAsync(string? folder, string? language, string? folderNote,
        DateTime? attemptStart, CancellationToken ct)
    {
        const string id = "compile", title = "Compile gate";
        var isCSharp = language is not null && (language.Contains('#') || language.Contains("sharp", StringComparison.OrdinalIgnoreCase));
        if (folder is null) return new(id, title, "not-run", folderNote ?? "No output folder", [], null);

        var isJava = string.Equals(language, "Java", StringComparison.OrdinalIgnoreCase)
            || string.Equals(language, "Java Quarkus", StringComparison.OrdinalIgnoreCase)
            || string.Equals(language, "Java Spring Boot", StringComparison.OrdinalIgnoreCase);
        var path = Path.Join(folder, isJava ? "java-compile-status.json" : CSharpCompileGate.StatusFile);
        if (IsEarlierArtifact(path, attemptStart))
            return new(id, title, "not-run", "The compile artifact predates this attempt", [], Rel(path));
        if (!File.Exists(path))
            return new(id, title, "not-run",
                isCSharp || isJava ? "No compile result: the gate is disabled or the run has not reached it" : "No compile result for this target",
                [], null);
        if (isJava) return await ReadJavaCompileGateAsync(path, ct);

        try
        {
            var r = JsonSerializer.Deserialize<CompileGateResult>(await File.ReadAllTextAsync(path, ct));
            if (r is null) return new(id, title, "warn", "compile-status.json is empty", [], Rel(path));
            var repairs = Math.Max(0, r.Rounds.Count - 1);
            var details = r.Rounds.Select(x =>
                $"Round {x.Round}: {x.Errors} error(s), {x.FilesRepaired} file(s) repaired{(x.RolledBack ? ", rolled back" : "")}").ToList();
            details.AddRange(r.RemainingErrors.Take(10).Select(e => e.ToString()));
            if (!r.Measured) return new(id, title, "warn", $"Not measured: {r.FailureReason}", details, Rel(path));
            if (r.Compiled) return new(id, title, "pass", $"Compiles after {repairs} repair round(s)", details, Rel(path));
            return new(id, title, "fail",
                r.RemainingErrors.Count > 0
                    ? $"{r.RemainingErrors.Count} error(s) remain after {repairs} repair round(s)"
                    : r.FailureReason ?? "Does not compile",
                details, Rel(path));
        }
        catch (Exception ex) when (ex is JsonException or IOException)
        {
            return new(id, title, "warn", $"compile-status.json could not be read ({ex.Message})", [], Rel(path));
        }
    }

    private async Task<AiLoopGate> ReadJavaCompileGateAsync(string path, CancellationToken ct)
    {
        const string id = "compile", title = "Compile gate";
        try
        {
            using var doc = JsonDocument.Parse(await File.ReadAllTextAsync(path, ct));
            var root = doc.RootElement;
            if (root.ValueKind != JsonValueKind.Object
                || !root.TryGetProperty("Measured", out var measured) || measured.ValueKind is not (JsonValueKind.True or JsonValueKind.False)
                || !root.TryGetProperty("Compiled", out var compiled) || compiled.ValueKind is not (JsonValueKind.True or JsonValueKind.False))
                return new(id, title, "warn", "Java compile artifact has no measured outcome", [], Rel(path));
            var failure = root.TryGetProperty("FailureReason", out var reason) && reason.ValueKind == JsonValueKind.String
                ? reason.GetString() : null;
            if (!measured.GetBoolean())
                return new(id, title, "warn", $"Not measured: {failure ?? "reason not recorded"}", [], Rel(path));
            var errors = root.TryGetProperty("Errors", out var items) && items.ValueKind == JsonValueKind.Array
                ? items.EnumerateArray().Where(x => x.ValueKind == JsonValueKind.Object).ToList() : [];
            var repairs = root.TryGetProperty("RepairRounds", out var rounds) && rounds.ValueKind == JsonValueKind.Number
                && rounds.TryGetInt32(out var n) ? n : (int?)null;
            var details = errors.Take(10).Select(e =>
                $"{PropertyText(e, "File")}:{PropertyText(e, "Line")} {PropertyText(e, "Code")}").ToList();
            if (root.TryGetProperty("Rounds", out var history) && history.ValueKind == JsonValueKind.Array)
                details.AddRange(history.EnumerateArray().Where(r => r.ValueKind == JsonValueKind.Object).TakeLast(20)
                    .Select(r => $"Round {PropertyText(r, "Round")}: compiled {PropertyText(r, "Compiled")}, exit {PropertyText(r, "ExitCode")}, rolled back {PropertyText(r, "RolledBack")}"));
            if (root.TryGetProperty("ExitCode", out var exit) && exit.ValueKind == JsonValueKind.Number)
                details.Add($"Build exit code: {exit}");
            return compiled.GetBoolean()
                ? new(id, title, "pass", repairs is { } count ? $"Compiles after {count} repair round(s)" : "Compiles; repair rounds not recorded",
                    details, Rel(path))
                : new(id, title, "fail", errors.Count > 0 ? $"{errors.Count} compiler error(s) remain" : failure ?? "Does not compile",
                    details, Rel(path));
        }
        catch (Exception ex) when (ex is JsonException or IOException)
        {
            return new(id, title, "warn", "java-compile-status.json could not be read", [], Rel(path));
        }
    }

    private static string PropertyText(JsonElement element, string name)
        => element.TryGetProperty(name, out var value) && value.ValueKind is JsonValueKind.String or JsonValueKind.Number or JsonValueKind.True or JsonValueKind.False
            ? value.ToString() : "?";

    private async Task<AiLoopGate> ParityGateAsync(string? folder, string? folderNote, DateTime? attemptStart, CancellationToken ct)
    {
        const string id = "parity", title = "Conversion parity";
        if (folder is null) return new(id, title, "not-run", folderNote ?? "No output folder", [], null);
        var path = Path.Join(folder, ConversionParityPostPass.ArtifactName);
        if (IsEarlierArtifact(path, attemptStart))
            return new(id, title, "not-run", "The parity artifact predates this attempt", [], Rel(path));
        if (!File.Exists(path)) return new(id, title, "not-run", "No parity report for this run", [], null);

        try
        {
            var report = JsonSerializer.Deserialize<ConversionParityReport>(await File.ReadAllTextAsync(path, ct));
            if (report is null) return new(id, title, "warn", "conversion-parity.json is empty", [], Rel(path));
            var evaluated = report.Programs.Where(p => p.Outcome == ParityOutcome.Evaluated).ToList();
            var failed = evaluated.Where(p => p.Failed).ToList();
            var notEvaluated = report.Programs.Count - evaluated.Count;
            var scored = evaluated.Where(p => p.Score is not null).ToList();
            var mean = scored.Count == 0 ? (double?)null : scored.Average(p => p.Score!.Value);

            var details = failed.Take(10)
                .Select(p => $"{p.Program}: {(p.Score is { } s ? s.ToString("0.00", System.Globalization.CultureInfo.InvariantCulture) : "no score")}"
                             + (p.LostAxes.Count > 0 ? $", lost {string.Join(", ", p.LostAxes)}" : ""))
                .ToList();
            if (notEvaluated > 0) details.Add($"{notEvaluated} program(s) not evaluated");

            var status = failed.Count > 0
                ? (string.Equals(report.OnLowScore, "fail", StringComparison.OrdinalIgnoreCase) ? "fail" : "warn")
                : notEvaluated > 0 ? "warn" : evaluated.Count == 0 ? "not-run" : "pass";
            var headline = $"{evaluated.Count - failed.Count}/{evaluated.Count} at or above {report.Threshold.ToString("0.00", System.Globalization.CultureInfo.InvariantCulture)}"
                           + (mean is { } m ? $" · mean {m.ToString("0.00", System.Globalization.CultureInfo.InvariantCulture)}" : "");
            return new(id, title, status, headline, details, Rel(path));
        }
        catch (Exception ex) when (ex is JsonException or IOException)
        {
            return new(id, title, "warn", $"conversion-parity.json could not be read ({ex.Message})", [], Rel(path));
        }
    }

    private async Task<AiLoopGate> JclGateAsync(string? folder, string? folderNote, DateTime? attemptStart, CancellationToken ct)
    {
        const string id = "jcl", title = "JCL jobs";
        if (folder is null) return new(id, title, "not-run", folderNote ?? "No output folder", [], null);
        var path = Path.Join(folder, JclJobWriter.ManifestFile);
        if (IsEarlierArtifact(path, attemptStart))
            return new(id, title, "not-run", "The JCL artifact predates this attempt", [], Rel(path));
        if (!File.Exists(path)) return new(id, title, "not-run", "No JCL jobs generated for this run", [], null);

        try
        {
            var jobs = JsonSerializer.Deserialize<List<JclJobManifestEntry>>(await File.ReadAllTextAsync(path, ct),
                new JsonSerializerOptions { PropertyNameCaseInsensitive = true }) ?? [];
            if (jobs.Count == 0) return new(id, title, "not-run", "The manifest lists no jobs", [], Rel(path));
            var blocked = jobs.Where(j => j.ProgramsMissing.Count > 0 || j.StepsNotRunnable.Count > 0).ToList();
            var details = blocked.Take(10).Select(j =>
                $"{j.Job}: " + string.Join("; ", new[]
                {
                    j.ProgramsMissing.Count > 0 ? $"needs {string.Join(", ", j.ProgramsMissing)}" : null,
                    j.StepsNotRunnable.Count > 0 ? $"{j.StepsNotRunnable.Count} step(s) not runnable" : null,
                }.Where(x => x is not null))).ToList();
            var status = blocked.Count == 0 ? "pass" : "warn";
            return new(id, title, status, $"{jobs.Count - blocked.Count}/{jobs.Count} job(s) can run end to end", details, Rel(path));
        }
        catch (Exception ex) when (ex is JsonException or IOException)
        {
            return new(id, title, "warn", $"jobs-manifest.json could not be read ({ex.Message})", [], Rel(path));
        }
    }
}
