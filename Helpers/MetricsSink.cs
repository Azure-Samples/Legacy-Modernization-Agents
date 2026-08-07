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

    public static int? CurrentRunId
    {
        get => _currentRunId.Value;
        set => _currentRunId.Value = value;
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

    public static void EmitAmbient(object payload)
        => Emit(CurrentRunId?.ToString(), payload);

    private static string CamelCase(string s)
        => string.IsNullOrEmpty(s) ? s : char.ToLowerInvariant(s[0]) + s.Substring(1);
}
