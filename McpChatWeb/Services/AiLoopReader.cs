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
    string? CurrentStage);

public sealed record AiLoopStage(int Number, int Total, string Name, DateTime StartedAt, long? DurationMs);

public sealed record AiLoopAgentStats(
    string Agent,
    IReadOnlyList<string> Models,
    int Calls,
    int FailedCalls,
    int Retries,
    int Fallbacks,
    long TotalMs,
    long AvgMs,
    long P95Ms,
    long PromptChars,
    long ResponseChars,
    long? InputTokens,
    long? OutputTokens);

public sealed record AiLoopGate(string Id, string Title, string Status, string Headline, IReadOnlyList<string> Details, string? Source);

public sealed record AiLoopEvent(DateTime Ts, string Kind, string? Agent, string? Context, string? Reason);

public sealed record AiLoopRunDetail(
    AiLoopRunSummary Summary,
    string? OutputFolder,
    IReadOnlyList<AiLoopStage> Stages,
    IReadOnlyList<AiLoopAgentStats> Agents,
    IReadOnlyList<AiLoopGate> Gates,
    IReadOnlyList<AiLoopEvent> Events,
    string? Warning);

public sealed class AiLoopOptions
{
    public const string SectionName = "AiLoop";

    // Newest metrics files listed; older ones stay on disk and are still readable by id.
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

    public async Task<IReadOnlyList<AiLoopRunSummary>> ListRunsAsync(CancellationToken ct = default)
    {
        var options = LoadOptions(out _);
        var dir = Path.Join(RepoRoot, MetricsFolder);
        if (!Directory.Exists(dir)) return [];

        var ended = await EndedRunStatusesAsync(ct);
        var result = new List<AiLoopRunSummary>();
        foreach (var file in new DirectoryInfo(dir).EnumerateFiles("*.jsonl")
                     .OrderByDescending(f => f.LastWriteTimeUtc)
                     .Take(Math.Max(1, options.MaxRuns)))
        {
            var runId = Path.GetFileNameWithoutExtension(file.Name);
            if (!IsValidRunId(runId)) continue;
            var events = await ReadEventsAsync(file.FullName, ct);
            // Older runs only have projection and cache metrics; they say nothing about the loop.
            if (!events.Any(e => e.Event is AiLoopEvents.RunStarted or AiLoopEvents.LlmCall)) continue;
            result.Add(Summarise(runId, events, file.LastWriteTimeUtc, options, ended.GetValueOrDefault(runId)));
        }
        return result;
    }

    public async Task<AiLoopRunDetail?> GetRunAsync(string runId, CancellationToken ct = default)
    {
        if (!IsValidRunId(runId)) return null;
        var path = Path.Join(RepoRoot, MetricsFolder, runId + ".jsonl");
        if (!File.Exists(path)) return null;

        var options = LoadOptions(out var warning);
        var events = await ReadEventsAsync(path, ct);
        var ended = await EndedRunStatusesAsync(ct);
        var summary = Summarise(runId, events, File.GetLastWriteTimeUtc(path), options, ended.GetValueOrDefault(runId));
        var started = events.LastOrDefault(e => e.Event == AiLoopEvents.RunStarted);
        var folder = started?.Str("outputFolder");

        var gates = new List<AiLoopGate> { ModelGate(summary, options) };
        var resolved = ResolveOutputFolder(folder, out var folderNote);
        gates.Add(await CompileGateAsync(resolved, summary.TargetLanguage, folderNote, ct));
        gates.Add(await ParityGateAsync(resolved, folderNote, ct));
        gates.Add(await JclGateAsync(resolved, folderNote, ct));

        var timeline = events
            .Where(e => e.Event is AiLoopEvents.LlmRetry or AiLoopEvents.LlmFallback
                        || (e.Event == AiLoopEvents.LlmCall && e.Bool("success") == false))
            .TakeLast(Math.Max(0, options.MaxTimelineEvents))
            .Select(e => new AiLoopEvent(e.Ts, e.Event!, e.Str("agent"), e.Str("context"),
                e.Str("reason") is { } r ? (e.Str("detail") is { } d ? $"{r}: {d}" : r) : e.Str("error")))
            .ToList();

        return new AiLoopRunDetail(summary, folder, Stages(events, summary), AgentStats(events), gates, timeline, warning);
    }

    // ── events ──────────────────────────────────────────────────────────────

    internal sealed record MetricEvent(DateTime Ts, string? Event, JsonElement Root)
    {
        public string? Str(string name) => Root.TryGetProperty(name, out var v) && v.ValueKind == JsonValueKind.String ? v.GetString() : null;
        public long? Long(string name) => Root.TryGetProperty(name, out var v) && v.ValueKind == JsonValueKind.Number && v.TryGetInt64(out var n) ? n : null;
        public bool? Bool(string name) => Root.TryGetProperty(name, out var v) && v.ValueKind is JsonValueKind.True or JsonValueKind.False ? v.GetBoolean() : null;
    }

    private async Task<List<MetricEvent>> ReadEventsAsync(string path, CancellationToken ct)
    {
        var list = new List<MetricEvent>();
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
                    var ts = root.TryGetProperty("ts", out var t) && t.TryGetDateTime(out var d) ? d.ToUniversalTime() : DateTime.MinValue;
                    var ev = root.TryGetProperty("event", out var e) && e.ValueKind == JsonValueKind.String ? e.GetString() : null;
                    list.Add(new MetricEvent(ts, ev, root));
                }
                catch (JsonException)
                {
                    // A line cut short by a run that is still writing is skipped, not fatal.
                }
            }
        }
        catch (IOException ex)
        {
            _logger.LogWarning("Could not read metrics {Path}: {Message}", path, ex.Message);
        }
        return list;
    }

    // A killed run never writes run_finished, but the next start marks it ended in the run database.
    // That record wins over the stale-time guess, so a dead run does not show as running beside its successor.
    private async Task<Dictionary<string, string>> EndedRunStatusesAsync(CancellationToken ct)
    {
        var result = new Dictionary<string, string>(StringComparer.Ordinal);
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
            command.CommandText = "SELECT id, status FROM runs WHERE status IS NOT NULL AND lower(status) <> 'running'";
            await using var reader = await command.ExecuteReaderAsync(ct);
            while (await reader.ReadAsync(ct))
                result[reader.GetInt64(0).ToString(System.Globalization.CultureInfo.InvariantCulture)] = reader.GetString(1).ToLowerInvariant();
        }
        catch (SqliteException ex)
        {
            _logger.LogDebug("Run database not readable for AI Loop status: {Message}", ex.Message);
        }
        return result;
    }

    private static AiLoopRunSummary Summarise(string runId, List<MetricEvent> events, DateTime lastWriteUtc, AiLoopOptions options,
        string? endedStatus = null)
    {
        var started = events.LastOrDefault(e => e.Event == AiLoopEvents.RunStarted);
        var finished = events.LastOrDefault(e => e.Event == AiLoopEvents.RunFinished);
        var calls = events.Where(e => e.Event == AiLoopEvents.LlmCall).ToList();
        var stage = events.LastOrDefault(e => e.Event == AiLoopEvents.Stage);

        var status = finished?.Str("status")
                     ?? endedStatus
                     ?? (DateTime.UtcNow - lastWriteUtc > TimeSpan.FromMinutes(Math.Max(1, options.StaleRunMinutes))
                         ? "interrupted"
                         : "running");
        var startedAt = started?.Ts ?? events.FirstOrDefault()?.Ts;

        return new AiLoopRunSummary(
            runId,
            started?.Str("mode"),
            started?.Str("targetLanguage"),
            status,
            startedAt,
            finished?.Ts,
            finished?.Long("durationMs"),
            calls.Count,
            calls.Count(c => c.Bool("success") == false),
            events.Count(e => e.Event == AiLoopEvents.LlmRetry),
            events.Count(e => e.Event == AiLoopEvents.LlmFallback),
            stage?.Str("name"));
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
            .Where(e => e.Event is AiLoopEvents.LlmCall or AiLoopEvents.LlmRetry or AiLoopEvents.LlmFallback)
            .GroupBy(e => e.Str("agent") ?? "unknown", StringComparer.Ordinal);

        return byAgent.Select(g =>
        {
            var calls = g.Where(e => e.Event == AiLoopEvents.LlmCall).ToList();
            var durations = calls.Select(c => c.Long("durationMs") ?? 0).OrderBy(x => x).ToList();
            var withTokens = calls.Where(c => c.Long("inputTokens") is not null).ToList();
            return new AiLoopAgentStats(
                g.Key,
                calls.Select(c => c.Str("model")).Where(m => !string.IsNullOrEmpty(m)).Distinct().Order().ToList()!,
                calls.Count,
                calls.Count(c => c.Bool("success") == false),
                g.Count(e => e.Event == AiLoopEvents.LlmRetry),
                g.Count(e => e.Event == AiLoopEvents.LlmFallback),
                durations.Sum(),
                durations.Count == 0 ? 0 : durations.Sum() / durations.Count,
                durations.Count == 0 ? 0 : durations[Math.Clamp((int)Math.Ceiling(0.95 * durations.Count) - 1, 0, durations.Count - 1)],
                calls.Sum(c => c.Long("promptChars") ?? 0),
                calls.Sum(c => c.Long("responseChars") ?? 0),
                withTokens.Count == 0 ? null : withTokens.Sum(c => c.Long("inputTokens") ?? 0),
                withTokens.Count == 0 ? null : withTokens.Sum(c => c.Long("outputTokens") ?? 0));
        })
        .OrderByDescending(a => a.Calls).ThenBy(a => a.Agent, StringComparer.Ordinal)
        .ToList();
    }

    // ── gates ───────────────────────────────────────────────────────────────

    private static AiLoopGate ModelGate(AiLoopRunSummary s, AiLoopOptions options)
    {
        if (s.Calls == 0)
            return new("model", "Model loop", "not-run", "No model calls recorded", [], null);

        var details = new List<string>
        {
            $"{s.Calls} call(s), {s.FailedCalls} failed, {s.Retries} retried, {s.Fallbacks} fell back to a stub",
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
            ? $"{s.Calls} call(s), no fallbacks"
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

    private async Task<AiLoopGate> CompileGateAsync(string? folder, string? language, string? folderNote, CancellationToken ct)
    {
        const string id = "compile", title = "Compile gate";
        var isCSharp = language is not null && (language.Contains('#') || language.Contains("sharp", StringComparison.OrdinalIgnoreCase));
        if (folder is null) return new(id, title, "not-run", folderNote ?? "No output folder", [], null);

        var path = Path.Join(folder, CSharpCompileGate.StatusFile);
        if (!File.Exists(path))
            return new(id, title, "not-run",
                isCSharp ? "No compile result: the gate is disabled or the run has not reached it" : "The compile gate covers C# output only",
                [], null);

        try
        {
            var r = JsonSerializer.Deserialize<CompileGateResult>(await File.ReadAllTextAsync(path, ct));
            if (r is null) return new(id, title, "warn", "compile-status.json is empty", [], Rel(path));
            var repairs = Math.Max(0, r.Rounds.Count - 1);
            var details = r.Rounds.Select(x =>
                $"Round {x.Round}: {x.Errors} error(s), {x.FilesRepaired} file(s) repaired{(x.RolledBack ? ", rolled back" : "")}").ToList();
            details.AddRange(r.RemainingErrors.Take(10).Select(e => e.ToString()));
            if (r.Compiled) return new(id, title, "pass", $"Compiles after {repairs} repair round(s)", details, Rel(path));
            if (!r.Measured) return new(id, title, "warn", $"Not measured: {r.FailureReason}", details, Rel(path));
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

    private async Task<AiLoopGate> ParityGateAsync(string? folder, string? folderNote, CancellationToken ct)
    {
        const string id = "parity", title = "Conversion parity";
        if (folder is null) return new(id, title, "not-run", folderNote ?? "No output folder", [], null);
        var path = Path.Join(folder, ConversionParityPostPass.ArtifactName);
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

    private async Task<AiLoopGate> JclGateAsync(string? folder, string? folderNote, CancellationToken ct)
    {
        const string id = "jcl", title = "JCL jobs";
        if (folder is null) return new(id, title, "not-run", folderNote ?? "No output folder", [], null);
        var path = Path.Join(folder, JclJobWriter.ManifestFile);
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
