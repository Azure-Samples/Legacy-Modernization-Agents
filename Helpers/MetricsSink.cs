using System.Text.Json;

namespace CobolToQuarkusMigration.Helpers;

// Metrics are append-only and fail-soft so emission cannot break conversion.
public static class MetricsSink
{
    private static readonly object _lock = new();
    private static readonly JsonSerializerOptions _json = new()
    {
        WriteIndented = false,
        PropertyNamingPolicy = JsonNamingPolicy.CamelCase
    };

    private static readonly AsyncLocal<int?> _currentRunId = new();
    private static readonly AsyncLocal<string?> _currentAttemptId = new();

    public static string? CurrentAttemptId
    {
        get => _currentAttemptId.Value;
        set => _currentAttemptId.Value = value;
    }

    public static int? CurrentRunId
    {
        get => _currentRunId.Value;
        set
        {
            if (_currentRunId.Value != value) CurrentAttemptId = null;
            _currentRunId.Value = value;
        }
    }

    private static string ResolveDir()
    {
        var repoRoot = Environment.GetEnvironmentVariable("REPO_ROOT")
                       ?? Directory.GetCurrentDirectory();
        return Path.Combine(repoRoot, "output", ".metrics");
    }

    public static void Emit(string? runId, object payload)
    {
        try
        {
            var dir = ResolveDir();
            Directory.CreateDirectory(dir);
            var safeRunId = string.IsNullOrWhiteSpace(runId) ? "unknown" : runId.Replace("/", "_").Replace("\\", "_");
            var path = Path.Combine(dir, $"{safeRunId}.jsonl");

            var wrapped = new Dictionary<string, object?>
            {
                ["runId"] = safeRunId,
                ["attemptId"] = CurrentRunId?.ToString() == runId ? CurrentAttemptId : null
            };
            // Spread payload props alongside ts/runId.
            foreach (var prop in payload.GetType().GetProperties())
            {
                wrapped[CamelCase(prop.Name)] = prop.GetValue(payload);
            }

            lock (_lock)
            {
                // Stamp at append, not before payload preparation or waiting behind another writer.
                wrapped["ts"] = DateTime.UtcNow.ToString("o");
                var line = JsonSerializer.Serialize(wrapped, _json) + Environment.NewLine;
                File.AppendAllText(path, line);
            }
        }
        catch (Exception ex)
        {
            // Fail-soft: metrics emission must never break conversion.
            Console.Error.WriteLine($"[MetricsSink] Failed to emit metrics for run '{runId}': {ex.Message}");
        }
    }

    public static void EmitAmbient(object payload)
        => Emit(CurrentRunId?.ToString(), payload);

    private static string CamelCase(string s)
        => string.IsNullOrEmpty(s) ? s : char.ToLowerInvariant(s[0]) + s.Substring(1);
}
