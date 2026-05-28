using System.Text.Json;

namespace CobolToQuarkusMigration.Helpers;

/// <summary>
/// Logger-independent metrics writer. Appends one JSON object per line to
/// output/.metrics/{runId}.jsonl so PR4 A/B comparisons can be reconstructed
/// even when ILogger output is buffered or dropped at process exit.
///
/// Use this for anything that absolutely must survive process termination:
///   - PROJECTION_METRICS (projectionMode, projectionTokens, rawRektTokens)
///   - LLM call metrics (firstTokenLatencyMs, streamDurationMs, completionTokens)
///   - Agent-level timing spans
///
/// Writes are append-only, fail-soft (any I/O exception is swallowed and logged
/// to stderr — we never want metrics emission to break the conversion).
/// </summary>
public static class MetricsSink
{
    private static readonly object _lock = new();
    private static readonly JsonSerializerOptions _json = new()
    {
        WriteIndented = false,
        PropertyNamingPolicy = JsonNamingPolicy.CamelCase
    };

    private static string ResolveDir()
    {
        var repoRoot = Environment.GetEnvironmentVariable("REPO_ROOT")
                       ?? Directory.GetCurrentDirectory();
        return Path.Combine(repoRoot, "output", ".metrics");
    }

    /// <summary>
    /// Append a metrics event for the given runId. Pass null/empty runId to use
    /// "unknown" — events still get persisted so they're never silently lost.
    /// </summary>
    public static void Emit(string? runId, object payload)
    {
        try
        {
            var dir = ResolveDir();
            Directory.CreateDirectory(dir);
            var safeRunId = string.IsNullOrWhiteSpace(runId) ? "unknown" : runId.Replace("/", "_").Replace("\\", "_");
            var path = Path.Combine(dir, $"{safeRunId}.jsonl");

            // Wrap payload with timestamp so post-hoc analysis can order events.
            var wrapped = new Dictionary<string, object?>
            {
                ["ts"] = DateTime.UtcNow.ToString("o"),
                ["runId"] = safeRunId
            };
            // Spread payload props alongside ts/runId.
            foreach (var prop in payload.GetType().GetProperties())
            {
                wrapped[CamelCase(prop.Name)] = prop.GetValue(payload);
            }

            var line = JsonSerializer.Serialize(wrapped, _json) + Environment.NewLine;
            lock (_lock)
            {
                File.AppendAllText(path, line);
            }
        }
        catch (Exception ex)
        {
            // Fail-soft: metrics emission must never break conversion.
            Console.Error.WriteLine($"[MetricsSink] Failed to emit metrics for run '{runId}': {ex.Message}");
        }
    }

    private static string CamelCase(string s)
        => string.IsNullOrEmpty(s) ? s : char.ToLowerInvariant(s[0]) + s.Substring(1);
}
