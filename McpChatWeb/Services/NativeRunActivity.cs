using System.Globalization;
using System.Security.Cryptography;
using System.Text;
using System.Text.Json;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using FileAccess = System.IO.FileAccess;

namespace McpChatWeb.Services;

public sealed record NativeProgramActivity(string RelativePath, string Status, bool? HasAst,
    bool? HasCfg, bool? HasData, bool? StrictDependencyComplete, int? StructureExitCode,
    int? DependencyExitCode, DateTime? UpdatedAt, DateTime? StartedAt, DateTime? FinishedAt,
    IReadOnlyDictionary<string, string>? MeasurementReasons = null,
    IReadOnlyList<DependencyDiagnosticExplanation>? DependencyIssues = null,
    DependencyInputInventoryResult? DependencyInputs = null, bool DependencyDiagnosticsTruncated = false);
public sealed record NativeScanProgress(string ManifestPath, string RecordedName, int? ExpectedPrograms,
    int RecordedPrograms, int StructuralPrograms, int FailedPrograms, bool Truncated,
    IReadOnlyList<NativeProgramActivity> Programs, string? GraphSnapshotId);
public sealed record NativeLogTail(string RunId, string Path, string Text, long TotalBytes, int ReadBytes,
    bool Truncated, DateTime LastWriteAt, DateTime RefreshedAt);
public sealed record NativeLogResult(int StatusCode, string? Error, NativeLogTail? Log = null);

public sealed partial class AiLoopReader
{
    public async Task<NativeLogResult> ReadNativeLogAsync(string runId, CancellationToken ct = default)
    {
        if (!IsValidRunId(runId) || !runId.StartsWith("native-", StringComparison.Ordinal))
            return new(400, "Select an existing native catalogue locator, not a filesystem path.");
        var detail = NativeActivities().FirstOrDefault(d => d.Summary.RunId == runId);
        if (detail?.Native is null) return new(404, "Native activity is missing or its manifest is unavailable; refresh the catalogue.");
        var directory = Path.GetDirectoryName(Path.Join(RepoRoot, detail.Native.ManifestPath))!;
        var manifest = Path.Join(directory, "scan.json");
        var log = Path.Join(directory, "native.log");
        try
        {
            // Validate every component, including parent folders, before opening either file.
            foreach (var path in new[] { Path.Join(RepoRoot, "output"), Path.Join(RepoRoot, "output", "rekt-scans"), directory, manifest, log })
                if ((File.GetAttributes(path) & FileAttributes.ReparsePoint) != 0)
                    return new(409, "Linked native log or manifest paths are not readable. Restore regular files in this scan directory.");
            var info = new FileInfo(manifest);
            if (info.Length > 8 * 1024 * 1024) return new(409, "Native manifest changed or exceeds the read limit; refresh the catalogue.");
            using (var stream = File.Open(manifest, FileMode.Open, FileAccess.Read, FileShare.ReadWrite | FileShare.Delete))
            using (var doc = await JsonDocument.ParseAsync(stream, new JsonDocumentOptions { MaxDepth = 48 }, ct))
            {
                var validated = ReadNative(doc.RootElement, directory);
                if (validated?.Summary.RunId != runId || !doc.RootElement.TryGetProperty("logPath", out var value)
                    || value.ValueKind != JsonValueKind.String || value.GetString() != "native.log")
                    return new(409, "Native manifest changed or has no supported native.log reference; refresh the catalogue.");
            }
            const int limit = 64 * 1024;
            var before = new FileInfo(log);
            var created = before.CreationTimeUtc;
            using var input = new FileStream(log, FileMode.Open, FileAccess.Read, FileShare.ReadWrite | FileShare.Delete,
                4096, FileOptions.Asynchronous);
            var length = input.Length;
            var size = (int)Math.Min(limit, length);
            input.Seek(length - size, SeekOrigin.Begin);
            var buffer = new byte[size];
            var read = 0;
            while (read < size)
            {
                var count = await input.ReadAsync(buffer.AsMemory(read, size - read), ct);
                if (count == 0) break;
                read += count;
            }
            var after = new FileInfo(log);
            if (read != size || input.Length < length || !after.Exists || after.CreationTimeUtc != created
                || (after.Attributes & FileAttributes.ReparsePoint) != 0)
                return new(409, "Native log was replaced or truncated while reading; retry the refresh.");
            var text = Encoding.UTF8.GetString(buffer, 0, read);
            return new(200, null, new(runId, Path.GetRelativePath(RepoRoot, log).Replace('\\', '/'),
                text, length, read, length > read, File.GetLastWriteTimeUtc(log), DateTime.UtcNow));
        }
        catch (FileNotFoundException) { return new(404, "Native log or manifest is missing. Check the original scan directory, then refresh."); }
        catch (DirectoryNotFoundException) { return new(404, "Native scan directory is missing. Refresh the catalogue."); }
        catch (UnauthorizedAccessException) { return new(403, "Native log is not readable. Check scan-directory access permissions."); }
        catch (JsonException) { return new(409, "Native manifest is being updated or is invalid; retry the refresh."); }
        catch (IOException) { return new(409, "Native log is unavailable or changed while reading; retry the refresh."); }
    }

    private readonly object _nativeLock = new();
    private readonly Dictionary<string, (long Length, DateTime Write, AiLoopRunDetail Detail)> _nativeCache = [];

    // Only immediate, explicitly shaped scan manifests. Never walk artifacts, source or logs.
    private IReadOnlyList<AiLoopRunDetail> NativeActivities()
    {
        lock (_nativeLock)
        {
            var root = Path.Join(RepoRoot, "output", "rekt-scans");
            if (!Directory.Exists(root) || (File.GetAttributes(Path.Join(RepoRoot, "output")) & FileAttributes.ReparsePoint) != 0
                || (File.GetAttributes(root) & FileAttributes.ReparsePoint) != 0) return [];
            var result = new List<AiLoopRunDetail>();
            var seen = new HashSet<string>();
            long bytesRead = 0;
            // Bound the newest manifests, so a scan started after many older ones is never dropped.
            var newest = Directory.EnumerateDirectories(root)
                .Select(directory => (Directory: directory, Write: File.GetLastWriteTimeUtc(Path.Join(directory, "scan.json"))))
                .OrderByDescending(candidate => candidate.Write)
                .ThenBy(candidate => candidate.Directory, StringComparer.Ordinal)
                .Take(512)
                .Select(candidate => candidate.Directory);
            foreach (var directory in newest)
            {
                var path = Path.Join(directory, "scan.json");
                seen.Add(path);
                try
                {
                    if ((File.GetAttributes(directory) & FileAttributes.ReparsePoint) != 0 || !File.Exists(path)) continue;
                    var info = new FileInfo(path);
                    if (info.Length > 8 * 1024 * 1024 || (info.Attributes & FileAttributes.ReparsePoint) != 0) continue;
                    if (_nativeCache.TryGetValue(path, out var cached)
                        && cached.Length == info.Length && cached.Write == info.LastWriteTimeUtc)
                    { result.Add(RefreshNativeStatus(cached.Detail)); continue; }
                    bytesRead += info.Length;
                    if (bytesRead > 64 * 1024 * 1024) continue;
                    using var stream = File.Open(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite | FileShare.Delete);
                    using var document = JsonDocument.Parse(stream, new JsonDocumentOptions { MaxDepth = 48 });
                    var detail = ReadNative(document.RootElement, directory);
                    if (detail is null)
                    {
                        if (_nativeCache.TryGetValue(path, out var previous))
                            result.Add(RefreshNativeStatus(previous.Detail) with
                            { Warning = "Native manifest update is incomplete or invalid; displaying the last valid recorded observation." });
                        continue;
                    }
                    _nativeCache[path] = (info.Length, info.LastWriteTimeUtc, detail);
                    result.Add(RefreshNativeStatus(detail));
                }
                catch (Exception ex) when (ex is IOException or JsonException or UnauthorizedAccessException or InvalidOperationException)
                {
                    // An atomic journal update never exposes half a record. For external writers,
                    // retain the last valid observation and explicitly mark it stale.
                    if (_nativeCache.TryGetValue(path, out var prior))
                        result.Add(RefreshNativeStatus(prior.Detail) with
                        { Warning = "Native manifest is currently unreadable; displaying the last valid recorded observation." });
                    _logger.LogDebug(ex, "Cannot read native activity manifest {Path}", path);
                }
            }
            foreach (var key in _nativeCache.Keys.Where(k => !seen.Contains(k)).ToList()) _nativeCache.Remove(key);
            return result;
        }
    }

    private AiLoopRunDetail RefreshNativeStatus(AiLoopRunDetail detail)
        => detail.Summary.Status == "running" && detail.Summary.LastEventAt is { } last
            && DateTime.UtcNow - last > TimeSpan.FromMinutes(Math.Max(1, LoadOptions(out _).StaleRunMinutes))
            ? detail with
            {
                Summary = detail.Summary with { Status = "interrupted" },
                Warning = "No recent native journal update or recorded finish. The process may have stopped; completion is not inferred."
            }
            : detail;

    private AiLoopRunDetail? ReadNative(JsonElement doc, string directory)
    {
        if (doc.ValueKind != JsonValueKind.Object) return null;
        static string? Text(JsonElement e, string key) => e.TryGetProperty(key, out var v) && v.ValueKind == JsonValueKind.String ? v.GetString() : null;
        static int? Number(JsonElement e, string key) => e.TryGetProperty(key, out var v) && v.ValueKind == JsonValueKind.Number && v.TryGetInt32(out var n) ? n : null;
        static bool? Flag(JsonElement e, string key) => e.TryGetProperty(key, out var v) && v.ValueKind is JsonValueKind.True or JsonValueKind.False ? v.GetBoolean() : null;
        static DateTime? Time(JsonElement e, string key)
            => DateTimeOffset.TryParse(Text(e, key), CultureInfo.InvariantCulture,
                DateTimeStyles.AssumeUniversal, out var t) ? t.UtcDateTime : null;
        var name = Text(doc, "name");
        if (new[] { "expectedPrograms", "schemaVersion" }.Any(key =>
            doc.TryGetProperty(key, out var value) && (value.ValueKind != JsonValueKind.Number || !value.TryGetInt32(out _))))
            return null;
        var start = Time(doc, "startedAt");
        var expected = Number(doc, "expectedPrograms");
        var journal = Text(doc, "activityKind") == "native-rekt" && Number(doc, "schemaVersion") == 1;
        var image = Text(doc, "imageId");
        var nativeImage = image is { Length: 71 } && image.StartsWith("sha256:", StringComparison.Ordinal)
            && image[7..].All(Uri.IsHexDigit);
        if (name != Path.GetFileName(directory) || start is null || expected < 0
            || !doc.TryGetProperty("programs", out var programs) || programs.ValueKind != JsonValueKind.Array
            || (!journal && !(nativeImage && expected is not null)))
            return null;
        var finish = Time(doc, "finishedAt");
        var status = Text(doc, "status") ?? "unknown";
        if (status is not ("running" or "completed" or "completed-with-gaps" or "failed" or "cancelled" or "partial" or "interrupted"))
            status = "unknown";
        if (finish < start) { finish = null; status = "unknown"; }
        if (finish is null && status is "completed" or "completed-with-gaps") status = "partial";
        var last = new[] { start, finish, Time(doc, "updatedAt") }.Max();
        var rows = new List<NativeProgramActivity>();
        var logs = new List<AiLoopLogReference>();
        var relative = Path.GetRelativePath(RepoRoot, directory).Replace('\\', '/');
        void Log(string? value, string kind)
        {
            if (string.IsNullOrWhiteSpace(value) || logs.Count >= 2000) return;
            // References only, confined to this evidence folder or the journal's output/rekt logs.
            if (Path.IsPathRooted(value) || value.Replace('\\', '/').Split('/').Contains("..")) return;
            var logPath = relative + "/" + value.Replace('\\', '/');
            logs.Add(new(logPath, kind, null, File.Exists(Path.Join(RepoRoot, logPath))));
        }
        Log("scan.json", "native manifest");
        foreach (var filename in new[] { "totals.json", "artifact-hashes.json", "verification.json" })
            if (File.Exists(Path.Join(directory, filename))) Log(filename, "native evidence reference (not revalidated)");
        Log(Text(doc, "logPath"), "native process log");
        foreach (var program in programs.EnumerateArray().Take(5000))
        {
            if (program.ValueKind != JsonValueKind.Object) return null;
            var source = Text(program, "relativePath");
            if (source is null) continue;
            var updated = Time(program, "updatedAt");
            if (updated > last) last = updated;
            int? Exit(string key)
            {
                if (!program.TryGetProperty(key, out var command) || command.ValueKind != JsonValueKind.Object) return null;
                Log(Text(command, "stdout"), key + " stdout");
                Log(Text(command, "stderr"), key + " stderr");
                return Number(command, "exitCode");
            }
            Dictionary<string, string>? reasons = null;
            if (program.TryGetProperty("measurementReasons", out var measured) && measured.ValueKind == JsonValueKind.Object)
                reasons = measured.EnumerateObject().Take(16).Where(p => p.Value.ValueKind == JsonValueKind.String)
                    .DistinctBy(p => p.Name)
                    .ToDictionary(p => p.Name, p => (p.Value.GetString() ?? "")[..Math.Min(1000, p.Value.GetString()?.Length ?? 0)]);
            var diagnostics = Array.Empty<string>();
            var diagnosticsTruncated = Flag(program, "dependencyDiagnosticsTruncated") == true;
            if (program.TryGetProperty("dependencyDiagnostics", out var recordedDiagnostics))
            {
                if (recordedDiagnostics.ValueKind != JsonValueKind.Array
                    || recordedDiagnostics.EnumerateArray().Any(value => value.ValueKind != JsonValueKind.String)) return null;
                diagnostics = recordedDiagnostics.EnumerateArray().Take(32)
                    .Select(value => value.GetString()![..Math.Min(512, value.GetString()!.Length)]).ToArray();
                diagnosticsTruncated |= recordedDiagnostics.GetArrayLength() > 32
                    || recordedDiagnostics.EnumerateArray().Take(32).Any(value => value.GetString()!.Length > 512);
            }
            else if (reasons?.TryGetValue("dependencyDetails", out var legacyDetails) == true)
            {
                diagnostics = legacyDetails.Split(" | ", StringSplitOptions.RemoveEmptyEntries).Take(32).ToArray();
                diagnosticsTruncated |= legacyDetails.Length >= 1000;
            }
            var issues = diagnostics.Select(raw => DependencyBlockerDescriptions.DescribeDiagnostic(raw)
                ?? new DependencyDiagnosticExplanation("unclassified", "recorded-condition", null,
                    "Classified native diagnostic evidence", raw,
                    "Inspect the original measurement notes and native log; this condition is not proof of missing source."))
                .DistinctBy(issue => (issue.Category, issue.Item, issue.Reason)).ToArray();
            DependencyInputInventoryResult? inputs = null;
            if (program.TryGetProperty("resolvedDefinitions", out var recordedInputs))
            {
                if (recordedInputs.ValueKind != JsonValueKind.Array || recordedInputs.EnumerateArray().Any(input =>
                        input.ValueKind != JsonValueKind.Object || Text(input, "kind") is null
                        || Text(input, "name") is null || Text(input, "path") is null)) return null;
                var inputRoot = Text(program, "inputSourceRoot");
                if (inputRoot is not null && (!Path.IsPathFullyQualified(inputRoot) || inputRoot.Contains('\0'))
                    || recordedInputs.EnumerateArray().Any(input => Text(input, "path")!.Contains('\0'))) return null;
                inputs = new(recordedInputs.EnumerateArray().Take(128).Select(input =>
                    new DependencyInputAvailability(Text(input, "kind")!, Text(input, "name")!, "recorded",
                        Text(input, "path"), inputRoot is null ? "unknown"
                            : DependencyInputInventory.Scope(inputRoot, source, Text(input, "path")!),
                        "Input hash matched when this scan was recorded; this viewer does not recheck current files.")).ToArray(),
                    Flag(program, "resolvedDefinitionsTruncated") == true || recordedInputs.GetArrayLength() > 128
                        ? "Recorded input list is truncated; consult the original evidence." : null);
            }
            rows.Add(new(source, Text(program, "structureStatus") ?? Text(program, "status") ?? "unknown",
                Flag(program, "hasAst"), Flag(program, "hasCfg"), Flag(program, "hasData"),
                Flag(program, "strictDependencyComplete"), Number(program, "structureExitCode") ?? Exit("structure"),
                Number(program, "dependencyExportExitCode") ?? Exit("dependency"), updated,
                Time(program, "startedAt"), Time(program, "finishedAt"), reasons, issues, inputs,
                diagnosticsTruncated));
        }
        // This is a catalogue locator derived from the manifest path, not a migration/graph run ID.
        var locator = "native-" + Convert.ToHexString(SHA256.HashData(Encoding.UTF8.GetBytes(relative)))[..24].ToLowerInvariant();
        var summary = new AiLoopRunSummary(locator, Text(doc, "command") ?? "native-rekt", null, status, start, finish,
            finish is { } end ? Math.Max(0, (long)(end - start.Value).TotalMilliseconds) : null,
            0, 0, 0, 0, Text(doc, "currentStage") ?? "Native REKT scan", RequestCoverage: "not-applicable",
            FailureReason: Text(doc, "error"), LastEventAt: last, ActivityKind: "native-rekt",
            ActivityId: "native:" + name);
        var stages = new List<AiLoopStage>();
        if (doc.TryGetProperty("stages", out var recordedStages) && recordedStages.ValueKind == JsonValueKind.Array)
        {
            foreach (var stage in recordedStages.EnumerateArray().Take(128))
            {
                if (stage.ValueKind != JsonValueKind.Object) return null;
                if (Time(stage, "startedAt") is not { } stageStart || Text(stage, "name") is not { } stageName) continue;
                var stageEnd = Time(stage, "finishedAt");
                stages.Add(new(stages.Count + 1, recordedStages.GetArrayLength(), stageName, stageStart,
                    stageEnd >= stageStart ? (long)(stageEnd.Value - stageStart).TotalMilliseconds : null));
            }
        }
        if (stages.Count == 0) stages.Add(new(1, 1, "Native REKT operation", start.Value, summary.DurationMs));
        return new(summary, relative, stages,
            [], [new("native", "Native evidence", status == "completed" ? "unknown" : "warn",
                "Native scan results are not model-call success, migration acceptance or behavioral equivalence.", [], relative + "/scan.json")],
            [], "Recorded native operation metadata only. Program timestamps absent from the manifest remain unrecorded; checksums are referenced, not recomputed by polling.",
            [], [], Logs: logs, Native: new(relative + "/scan.json", name!, expected,
                programs.GetArrayLength(), rows.Count(p => p.HasAst == true && p.HasCfg == true && p.HasData == true),
                rows.Count(p => p.Status.Equals("failed", StringComparison.OrdinalIgnoreCase) || p.StructureExitCode is > 0
                    || p.HasAst is false || p.HasCfg is false || p.HasData is false),
                programs.GetArrayLength() > rows.Count, rows, Text(doc, "graphSnapshotId")));
    }
}
