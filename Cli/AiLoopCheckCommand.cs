using System.CommandLine;
using System.Text.Json;
using System.Text.Json.Nodes;
using Microsoft.Data.Sqlite;

namespace CobolToQuarkusMigration.Cli;

// Read-only acceptance after activating a new portal: each recorded native REKT scan must be
// catalogued in AI Loop exactly as its manifest says, without fabricated model activity.
public static class AiLoopCheckCommand
{
    private const int MaxExtraPages = 20;

    public static Command Build()
    {
        var cmd = new Command("ai-loop-check",
            "Check that the running portal's AI Loop catalogues recorded native REKT scans without fabricated model activity.");

        var urlOption = new Option<string>("--url", () => "http://localhost:5028", "Portal base URL.");
        var repoRootOption = new Option<string>("--repo-root", () => Directory.GetCurrentDirectory(),
            "Repository root holding output/rekt-scans, output/.metrics and Data/migration.db.");
        var scanOption = new Option<string[]>("--scan", "A directory under output/rekt-scans expected in the catalogue (repeatable).")
        {
            IsRequired = true,
            AllowMultipleArgumentsPerToken = false,
        };
        var modelRunOption = new Option<string?>("--model-run", "Migration run ID whose model-attempt summary must be catalogued.");
        cmd.AddOption(urlOption);
        cmd.AddOption(repoRootOption);
        cmd.AddOption(scanOption);
        cmd.AddOption(modelRunOption);

        cmd.SetHandler(async (string url, string repoRoot, string[] scans, string? modelRun) =>
        {
            using var http = new HttpClient { BaseAddress = new Uri(url.TrimEnd('/') + "/"), Timeout = TimeSpan.FromSeconds(30) };
            try
            {
                await RunAsync(http, Path.GetFullPath(repoRoot), scans, modelRun, Console.Out);
            }
            catch (Exception ex) when (ex is AiLoopCheckException or HttpRequestException or TaskCanceledException
                                           or JsonException or IOException or SqliteException or InvalidOperationException)
            {
                Console.Error.WriteLine($"FAIL: {ex.Message}");
                Environment.ExitCode = 1;
            }
        }, urlOption, repoRootOption, scanOption, modelRunOption);

        return cmd;
    }

    public static async Task RunAsync(HttpClient http, string repoRoot, IReadOnlyList<string> scans, string? modelRun, TextWriter output)
    {
        var payload = await GetAsync(http, "api/ai-loop/runs");
        var runs = Array(payload, "runs").ToList();
        for (var page = 0; page < MaxExtraPages && payload["hasMore"]?.GetValue<bool>() == true; page++)
        {
            payload = await GetAsync(http, "api/ai-loop/runs?offset=" + payload["nextOffset"]);
            runs.AddRange(Array(payload, "runs"));
        }

        foreach (var name in scans)
        {
            var manifest = Parse(File.ReadAllText(Path.Join(repoRoot, "output", "rekt-scans", name, "scan.json")));
            var matches = runs.Where(r => r?["activityId"]?.GetValue<string>() == "native:" + name).ToList();
            Check(matches.Count == 1, $"{name}: native activity missing; activate the new portal assembly first");
            var summary = matches[0]!;
            var detail = await GetAsync(http, "api/ai-loop/" + Uri.EscapeDataString(Text(summary, "runId")));

            Check(Text(summary, "activityKind") == "native-rekt", $"{name}: activityKind is not native-rekt");
            Check(Text(summary, "requestCoverage") == "not-applicable" && Number(summary, "calls") == 0,
                $"{name}: a native scan must record no model requests");
            Check(IsNull(summary, "attemptId") && IsNull(summary, "managedRunId"),
                $"{name}: a native scan must not carry a model attempt or managed run");
            var native = Required(detail, "native");
            Check(Number(native, "recordedPrograms") == Array(manifest, "programs").Count,
                $"{name}: recordedPrograms differs from the manifest");
            Check(Number(native, "expectedPrograms") == Number(manifest, "expectedPrograms"),
                $"{name}: expectedPrograms differs from the manifest");
            Check(Text(Required(detail, "summary"), "status") == Text(manifest, "status"),
                $"{name}: status differs from the manifest");
            Check(Instant(Text(summary, "finishedAt")) == Instant(Text(manifest, "finishedAt")),
                $"{name}: finishedAt differs from the manifest");
            Check(IsEmpty(detail, "agents") && IsEmpty(detail, "requests"),
                $"{name}: a native scan must list no agents or requests");
            var kinds = Array(detail, "logs").Select(log => log?["kind"]?.GetValue<string>()).ToHashSet();
            foreach (var kind in ExpectedLogKinds(manifest))
                Check(kinds.Contains(kind), $"{name}: the {kind} is not catalogued");

            var picked = new JsonObject();
            foreach (var key in new[] { "activityId", "runId", "activityKind", "status", "startedAt", "finishedAt", "lastEventAt" })
                picked[key] = summary[key]?.DeepClone();
            output.WriteLine(picked.ToJsonString());
        }

        JsonNode? model = null;
        if (modelRun is not null)
        {
            model = Required(await GetAsync(http, "api/ai-loop/" + Uri.EscapeDataString(modelRun)), "summary");
            Check(Text(model, "activityKind") == "model-attempt", $"{modelRun}: activityKind is not model-attempt");
            Check(Number(model, "startedRequests") >= Number(model, "calls"),
                $"{modelRun}: fewer started requests than recorded calls");
        }

        var metricsDir = Path.Join(repoRoot, "output", ".metrics");
        var metrics = Directory.Exists(metricsDir) ? Directory.GetFiles(metricsDir, "*.jsonl") : [];
        long migrationRows;
        // Read-only so a check against a live portal cannot lock or alter its database.
        using (var connection = new SqliteConnection(new SqliteConnectionStringBuilder
               {
                   DataSource = Path.Join(repoRoot, "Data", "migration.db"),
                   Mode = SqliteOpenMode.ReadOnly,
                   Pooling = false,
               }.ToString()))
        {
            connection.Open();
            using var count = connection.CreateCommand();
            count.CommandText = "SELECT COUNT(*) FROM runs";
            migrationRows = (long)count.ExecuteScalar()!;
        }

        output.WriteLine(new JsonObject
        {
            ["modelFinishedAt"] = model?["finishedAt"]?.DeepClone(),
            ["modelStarts"] = model?["startedRequests"]?.DeepClone(),
            ["modelOutcomes"] = model?["calls"]?.DeepClone(),
            ["migrationRows"] = migrationRows,
            ["metricsFiles"] = metrics.Length,
            ["metricsRecords"] = metrics.Sum(path => File.ReadLines(path).LongCount()),
        }.ToJsonString());
        output.WriteLine("PASS: recorded native manifests are catalogued without fabricated model activity.");
    }

    // Only logs the manifest recorded can be expected: doctor.sh scans record a process log but no
    // per-program command output, so requiring "structure stderr" failed every real scan.
    private static IEnumerable<string> ExpectedLogKinds(JsonNode manifest)
    {
        yield return "native manifest";
        if (manifest["logPath"] is JsonValue) yield return "native process log";
        foreach (var program in Array(manifest, "programs"))
            if (program?["structure"]?["stderr"] is JsonValue) yield return "structure stderr";
    }

    private static async Task<JsonNode> GetAsync(HttpClient http, string path)
    {
        using var response = await http.GetAsync(path);
        response.EnsureSuccessStatusCode();
        return Parse(await response.Content.ReadAsStringAsync());
    }

    private static JsonNode Parse(string json) =>
        JsonNode.Parse(json) ?? throw new AiLoopCheckException("expected a JSON object, got null");

    private static void Check(bool condition, string message)
    {
        if (!condition) throw new AiLoopCheckException(message);
    }

    // A missing field fails the check: the catalogue contract requires every one of them.
    private static JsonNode Required(JsonNode node, string key) =>
        node[key] ?? throw new AiLoopCheckException($"missing or null field '{key}'");

    private static string Text(JsonNode node, string key) => Required(node, key).GetValue<string>();

    private static double Number(JsonNode node, string key) => Required(node, key).GetValue<double>();

    private static IList<JsonNode?> Array(JsonNode node, string key) => Required(node, key).AsArray();

    private static bool IsNull(JsonNode node, string key) =>
        node is JsonObject obj && obj.ContainsKey(key) ? obj[key] is null
            : throw new AiLoopCheckException($"missing field '{key}'");

    private static bool IsEmpty(JsonNode node, string key) =>
        node is JsonObject obj && obj.ContainsKey(key) ? obj[key] is not JsonArray array || array.Count == 0
            : throw new AiLoopCheckException($"missing field '{key}'");

    private static DateTimeOffset Instant(string value) =>
        DateTimeOffset.TryParse(value, System.Globalization.CultureInfo.InvariantCulture,
            System.Globalization.DateTimeStyles.RoundtripKind, out var instant)
            ? instant
            : throw new AiLoopCheckException($"'{value}' is not an ISO-8601 timestamp");
}

public sealed class AiLoopCheckException(string message) : Exception(message);
