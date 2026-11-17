using System.Text.Json;
using System.Text.Json.Nodes;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Rekt;

// Atomic, metadata-only native REKT lifecycle journal (scan.json) that doctor.sh updates and the portal reads.
// Completion measurements reuse the dependency-evidence validators; null means unrecorded or unverified, with
// bounded reasons. Parser exit codes are never reconstructed from artifact availability.
public static class NativeRunActivity
{
    public static readonly string[] Commands = ["rekt-parse", "rekt-full", "rekt-ingest"];

    private static readonly string[] MeasurementKeys =
    [
        "hasAst", "hasCfg", "hasData", "strictDependencyComplete", "dependencyExportExitCode", "measurementReasons", "measuredAt",
        "dependencyDiagnostics", "dependencyDiagnosticsTruncated", "inputSourceRoot", "resolvedDefinitions",
        "resolvedDefinitionsTruncated", "reportPath", "dependencyEvidencePath", "parseLogPath",
    ];

    private static readonly HashSet<string> CompleteStatuses = ["Full", "NoDialect", "cached"];

    private static void Write(string path, JsonNode data)
    {
        var pending = Path.Join(Path.GetDirectoryName(path), $"{PyCompat.Stem(path)}.{Environment.ProcessId}.{PyCompat.NewHex()}.pending");
        File.WriteAllText(pending, PyJson.Dumps(data, indent: 2) + "\n", PyCompat.Utf8);
        File.Move(pending, path, overwrite: true);
    }

    public static string Start(string root, string command, string? stage)
    {
        var folder = Path.Join(root, "doctor-" + PyCompat.NewHex());
        if (Directory.Exists(folder)) throw new IOException($"[Errno 17] File exists: '{folder}'");
        Directory.CreateDirectory(folder);
        var path = Path.Join(folder, "scan.json");
        var stamp = PyCompat.UtcNowIso();
        stage ??= "Native parsing";
        Write(path, new JsonObject
        {
            ["schemaVersion"] = 1,
            ["activityKind"] = "native-rekt",
            ["name"] = Path.GetFileName(folder),
            ["command"] = command,
            ["startedAt"] = stamp,
            ["updatedAt"] = stamp,
            ["status"] = "running",
            ["currentStage"] = stage,
            ["stages"] = new JsonArray(new JsonObject { ["name"] = stage, ["startedAt"] = stamp }),
            ["programs"] = new JsonArray(),
            ["logPath"] = "native.log",
        });
        return RektEvidence.Norm(path);
    }

    // Parallel parse jobs update the same journal; serialise each read-modify-write.
    private static void Locked(string path, Action update)
    {
        var lockPath = Path.ChangeExtension(path, ".lock");
        for (var attempt = 0; ; attempt++)
        {
            FileStream handle;
            try
            {
                handle = new FileStream(lockPath, FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.None);
            }
            catch (IOException) when (attempt < 30_000)
            {
                Thread.Sleep(10);
                continue;
            }
            using (handle) update();
            return;
        }
    }

    private static JsonObject Load(string path)
    {
        var data = PyJson.Parse(PyCompat.ReadText(path)) as JsonObject;
        if (!PyJson.PyEquals(data?["schemaVersion"], 1) || PyJson.Str(data?["activityKind"]) != "native-rekt")
            throw PyError.Value("Not a native lifecycle journal");
        return data!;
    }

    public static void Program(string path, string? program, string? status, string? sourceRoot, string? outputRoot)
    {
        JsonObject? measured = status != "running" && sourceRoot is not null && outputRoot is not null
            ? Measurements(sourceRoot, outputRoot, program!)
            : null;
        Locked(path, () =>
        {
            var data = Load(path);
            var updated = PyCompat.UtcNowIso();
            data["updatedAt"] = updated;
            var programs = (JsonArray)data["programs"]!;
            var row = programs.FirstOrDefault(item => PyJson.Str(item!["relativePath"]) == program) is JsonObject existing
                ? (JsonObject)existing.DeepClone() : new JsonObject();
            row["relativePath"] = program;
            row["status"] = status;
            row["updatedAt"] = updated;
            if (status == "running")
            {
                row["startedAt"] = updated;
                row.Remove("finishedAt");
                foreach (var key in MeasurementKeys) row.Remove(key);
            }
            else
            {
                row["finishedAt"] = updated;
                if (measured is not null)
                    foreach (var (key, value) in measured) row[key] = value?.DeepClone();
            }
            var kept = programs.Where(item => PyJson.Str(item!["relativePath"]) != program).Select(item => item!.DeepClone()).ToList();
            kept.Add(row);
            data["programs"] = new JsonArray(kept.ToArray());
            data["currentStage"] = "Native parsing: " + program;
            Write(path, data);
        });
    }

    public static void Stage(string path, string? stage, int? expectedPrograms) => Locked(path, () =>
    {
        var data = Load(path);
        var updated = PyCompat.UtcNowIso();
        data["updatedAt"] = updated;
        data["currentStage"] = stage;
        if (data["stages"] is not JsonArray stages) data["stages"] = stages = new JsonArray();
        if (stages.Count == 0 || PyJson.Str(stages[^1]!["name"]) != stage)
        {
            if (stages.Count > 0) stages[^1]!["finishedAt"] = updated;
            stages.Add(new JsonObject { ["name"] = stage, ["startedAt"] = updated });
        }
        if (expectedPrograms is not null) data["expectedPrograms"] = expectedPrograms;
        Write(path, data);
    });

    public static void Finish(string path, int? exitCode) => Locked(path, () =>
    {
        var data = Load(path);
        var updated = PyCompat.UtcNowIso();
        data["updatedAt"] = updated;
        data["finishedAt"] = updated;
        data["exitCode"] = exitCode;
        var statuses = ((JsonArray)data["programs"]!).Select(item => PyJson.Str(item!["status"])).ToHashSet();
        var status = exitCode is 130 or 143 ? "cancelled"
            : exitCode is not null and not 0 ? "failed"
            : statuses.Any(item => item is null || !CompleteStatuses.Contains(item)) ? "completed-with-gaps"
            : "completed";
        data["status"] = status;
        if (exitCode is not null and not 0) data["error"] = "Native command exited with code " + exitCode;
        if (data["stages"] is JsonArray { Count: > 0 } stages)
        {
            stages[^1]!["finishedAt"] = updated;
            stages[^1]!["status"] = status;
        }
        Write(path, data);
    });

    private static IEnumerable<string> JsonArtifacts(string directory) =>
        Directory.Exists(directory)
            ? Directory.EnumerateFileSystemEntries(directory).Where(path => Path.GetFileName(path).EndsWith(".json", StringComparison.Ordinal))
            : [];

    private static bool HasStructure(string directory)
    {
        var artifacts = JsonArtifacts(directory).ToList();
        if (artifacts.Count == 0) return false;
        foreach (var artifact in artifacts)
        {
            var value = PyJson.Parse(PyCompat.ReadText(artifact));
            if (value is not (JsonObject or JsonArray) || !PyJson.Truthy(value)) return false;
        }
        return true;
    }

    public static JsonObject Measurements(string sourceRoot, string outputRoot, string relativePath)
    {
        var report = RektEvidence.Norm(Path.Join(outputRoot, relativePath + ".report"));
        var evidencePath = Path.Join(report, "dependency-evidence.json");
        var result = new JsonObject
        {
            ["reportPath"] = report,
            ["dependencyEvidencePath"] = evidencePath,
            ["parseLogPath"] = RektEvidence.Norm(Path.Join(outputRoot, relativePath + ".parse.log")),
            ["hasAst"] = null,
            ["hasCfg"] = null,
            ["hasData"] = null,
            ["strictDependencyComplete"] = null,
            ["dependencyExportExitCode"] = null,
        };
        var reasons = new JsonObject();
        foreach (var (field, directory) in new[] { ("hasAst", "flow_ast"), ("hasCfg", "cfg"), ("hasData", "data_structures") })
        {
            if (!JsonArtifacts(Path.Join(report, directory)).Any())
            {
                reasons[field] = "artifact-not-recorded";
                continue;
            }
            try
            {
                var present = HasStructure(Path.Join(report, directory));
                result[field] = present;
                if (!present) reasons[field] = "artifact-structure-empty-or-invalid";
            }
            catch (Exception error) when (error is PyError or IOException or UnauthorizedAccessException or JsonException or ArgumentException)
            {
                reasons[field] = "artifact-unreadable-or-invalid-json";
            }
        }
        try
        {
            MeasureEvidence(sourceRoot, outputRoot, relativePath, evidencePath, result, reasons);
        }
        catch (Exception error) when (error is PyError { Kind: "FileNotFoundError" } or FileNotFoundException or DirectoryNotFoundException)
        {
            reasons["strictDependencyComplete"] = "dependency-evidence-or-bound-input-not-recorded";
        }
        catch (Exception error) when (RektEvidence.Caught(error))
        {
            reasons["strictDependencyComplete"] = "dependency-evidence-invalid-or-integrity-mismatch";
        }
        result["measurementReasons"] = reasons;
        result["measuredAt"] = PyCompat.UtcNowIso();
        return result;
    }

    private static void MeasureEvidence(string sourceRoot, string outputRoot, string relativePath, string evidencePath,
        JsonObject result, JsonObject reasons)
    {
        var evidence = PyJson.Parse(PyCompat.ReadText(evidencePath));
        JsonNode? Field(string key) => RektEvidence.Subscript(evidence, key);
        var source = PyCompat.Resolve(Path.Join(sourceRoot, relativePath));
        if (!PyJson.PyEquals(Field("schemaVersion"), 1) || PyJson.Str(Field("producer")) != RektEvidence.Producer
            || PyJson.Str(Field("relativePath")) != relativePath
            || PyCompat.Resolve(RektEvidence.PathArg(Field("originalSourcePath"))) != source
            || !PyJson.PyEquals(Field("originalSourceSha256"), PyCompat.Digest(source)))
            throw PyError.Value("source binding");
        if (Field("coverage") is not JsonObject coverage
            || RektEvidence.Kinds.Any(kind => PyJson.Str(coverage[kind]) is not ("complete" or "partial" or "unavailable"))
            || Field("definitions") is not JsonArray definitionItems
            || Field("unresolvedDiagnostics") is not JsonArray diagnosticItems
            || diagnosticItems.Any(issue => !PyJson.IsStr(issue))
            || !PyJson.IsBool(Field("placeholderContaminated")))
            throw PyError.Value("incomplete evidence record");
        var export = Path.Join(outputRoot, relativePath + "-deps.json");
        if (PyCompat.Resolve(RektEvidence.PathArg(Field("exportPath"))) != PyCompat.Resolve(export)
            || !PyJson.PyEquals(Field("exportSha256"), File.Exists(export) ? PyCompat.Digest(export) : null))
            throw PyError.Value("export integrity");
        foreach (var definition in definitionItems)
            if (!PyJson.PyEquals(RektEvidence.Subscript(definition, "sha256"), PyCompat.Digest(RektEvidence.PathArg(RektEvidence.Subscript(definition, "path")))))
                throw PyError.Value("definition integrity");
        var code = Field("exportExitCode");
        if (!PyJson.IsInt(code) || PyJson.IsBool(code)) throw PyError.Value("unrecorded exporter exit");
        var issues = diagnosticItems.Select(issue => PyJson.Str(issue)!).Distinct().ToList();
        var order = new List<string>();
        var unique = new Dictionary<string, JsonObject>();
        foreach (var item in definitionItems)
        {
            var kind = RektEvidence.Subscript(item, "kind");
            var name = RektEvidence.Subscript(item, "name");
            var path = RektEvidence.Subscript(item, "path");
            if (kind is JsonArray or JsonObject || name is JsonArray or JsonObject || path is JsonArray or JsonObject)
                throw new PyError("TypeError", "unhashable type");
            var key = RektEvidence.PyKey(kind) + "\0" + RektEvidence.PyKey(name) + "\0" + RektEvidence.PyKey(path);
            if (!unique.ContainsKey(key)) order.Add(key);
            unique[key] = new JsonObject { ["kind"] = kind?.DeepClone(), ["name"] = name?.DeepClone(), ["path"] = path?.DeepClone() };
        }
        var definitions = order.Select(key => unique[key]).ToList();
        if (definitions.Any(item => item.Any(pair => !PyJson.IsStr(pair.Value)))) throw PyError.Value("invalid input metadata");
        var strict = RektEvidence.EvidenceCurrent(sourceRoot, relativePath, outputRoot);
        result["strictDependencyComplete"] = strict;
        result["dependencyExportExitCode"] = code!.DeepClone();
        result["dependencyDiagnostics"] = RektEvidence.ToArray(issues.Take(32).Select(issue => PyCompat.Slice(issue, 0, 512)));
        result["dependencyDiagnosticsTruncated"] = issues.Count > 32 || issues.Take(32).Any(issue => issue.Length > 512);
        result["inputSourceRoot"] = PyCompat.Resolve(sourceRoot);
        result["resolvedDefinitions"] = new JsonArray(definitions.Take(128).Select(item => (JsonNode?)item).ToArray());
        result["resolvedDefinitionsTruncated"] = definitions.Count > 128;
        if (!strict)
        {
            reasons["strictDependencyComplete"] = "dependency-evidence-not-strictly-complete";
            if (issues.Count > 0) reasons["dependencyDetails"] = PyCompat.Slice(string.Join(" | ", issues.Take(8)), 0, 1000);
        }
    }
}
