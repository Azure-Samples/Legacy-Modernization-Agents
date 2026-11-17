using System.Text.Json;
using Microsoft.Data.Sqlite;

namespace CobolToQuarkusMigration.Persistence.GraphIngest;

public sealed class GraphIngestException(string message) : Exception(message);

public sealed record GraphIngestOptions(
    string SourceDir, string RektOutput, long RunId, string? SqliteDb = null, string? ScanName = null, string? ScanManifest = null);

// Loads parsed REKT output, JCL job facts and optionally a migration SQLite database into the
// graph. The order of steps, the console lines and every node and relationship written match the
// Python populator this replaces, because the portal, MCP server and CLI read the same graph.
public sealed class GraphIngestion(IGraphTarget target, TextWriter log)
{
    public const int BatchSize = 10_000;
    public const string JclScanFailedMarker = ".jcl-scan-failed";
    public const string JclScanFailedMessage = "JCL refresh did not complete; rerun jcl-facts before ingestion.";

    private static readonly string[] SkippedDirectories = [".previous", ".work"];
    private readonly SortedDictionary<string, long> _skippedByType = new(StringComparer.Ordinal);

    public static bool JclScanFailed(string rektOutput) => Path.Exists(Path.Combine(rektOutput, JclScanFailedMarker));

    public async Task<(long Nodes, long Relationships)> RunAsync(GraphIngestOptions options)
    {
        if (JclScanFailed(options.RektOutput)) throw new GraphIngestException(JclScanFailedMessage);
        var scanStarted = false;
        try
        {
            log.WriteLine("Applying schema...");
            await target.ApplySchemaAsync(GraphSchema.Statements);
            log.WriteLine("Schema applied successfully");
            if (options.ScanName is not null)
            {
                await BeginRektScanAsync(options.RunId, options.ScanName, options.SourceDir, options.RektOutput, options.ScanManifest);
                scanStarted = true;
            }

            if (!string.IsNullOrEmpty(options.SqliteDb))
            {
                log.WriteLine($"Migrating SQLite: {options.SqliteDb}");
                foreach (var (label, count) in await MigrateSqliteAsync(options.SqliteDb))
                {
                    log.WriteLine($"  {label}: {count} nodes");
                }
            }

            log.WriteLine("Ingesting rekt outputs...");
            var counts = await IngestRektOutputsAsync(options.RektOutput, options.SourceDir, options.RunId);
            ReportSkippedRelationships();
            foreach (var (label, count) in counts) log.WriteLine($"  {label}: {count}");
            if (scanStarted)
            {
                var lookup = counts.ToDictionary(c => c.Key, c => c.Value);
                if (lookup.GetValueOrDefault("ASTNode") == 0 && lookup.GetValueOrDefault("JclNode") == 0)
                {
                    throw new GraphIngestException("The named scan contains no ingested AST or JCL nodes; partial data was retained.");
                }
                await target.FinishScanAsync(options.RunId, "complete");
            }

            var totals = await target.TotalsAsync();
            log.WriteLine($"\nDone. Total: {totals.Nodes} nodes, {totals.Relationships} relationships");
            return totals;
        }
        catch when (scanStarted)
        {
            await target.FinishScanAsync(options.RunId, "failed");
            log.WriteLine("Named scan ingestion failed; partial data has been retained.");
            throw;
        }
    }

    public async Task BeginRektScanAsync(long runId, string name, string sourceDir, string outputDir, string? manifestPath)
    {
        name = name.Trim();
        if (name.Length == 0) throw new GraphIngestException("A named REKT scan requires a nonempty name.");
        if (!Directory.Exists(sourceDir) || !Directory.Exists(outputDir))
        {
            throw new GraphIngestException("A named REKT scan requires existing source and artifact directories.");
        }
        string? manifest;
        if (manifestPath is not null)
        {
            if (!File.Exists(manifestPath)) throw new GraphIngestException("The named REKT scan manifest does not exist.");
            manifest = RealPath(manifestPath);
        }
        else
        {
            // Named scans keep scan.json beside their artifacts directory (output/rekt-scans/<name>/).
            var candidate = Path.Combine(Path.GetDirectoryName(RealPath(outputDir)) ?? "/", "scan.json");
            manifest = File.Exists(candidate) ? candidate : null;
        }

        if (!await target.TryCreateScanAsync(runId, name, RealPath(sourceDir), RealPath(outputDir), manifest))
        {
            throw new GraphIngestException($"Run ID {runId} already contains data; choose a fresh ID to preserve it.");
        }
    }

    public async Task<long> MergeNodesAsync(string label, List<Dictionary<string, object?>> rows, string mergeKey = "id")
    {
        foreach (var batch in rows.Chunk(BatchSize)) await target.MergeNodesAsync(label, mergeKey, batch);
        return rows.Count;
    }

    public async Task<long> MergeRelationshipsAsync(RelationshipBatch relationships)
    {
        long total = 0;
        foreach (var batch in relationships.Rows.Chunk(BatchSize))
        {
            var matched = await target.MergeRelationshipsAsync(relationships, batch);
            total += matched;
            if (matched < batch.Length)
            {
                _skippedByType[relationships.Type] = _skippedByType.GetValueOrDefault(relationships.Type) + batch.Length - matched;
            }
        }
        return total;
    }

    // Per-program ingest skips a row or two each; one total per type keeps the scan output readable.
    public void ReportSkippedRelationships()
    {
        foreach (var (type, count) in _skippedByType)
        {
            log.WriteLine($"Skipped {count} {type} relationship rows with missing endpoints");
        }
        _skippedByType.Clear();
    }

    public async Task<long> IngestAstAsync(string program, JsonElement ast, long runId)
    {
        var (nodes, contains) = RektGraphRows.Ast(program, ast, runId);
        var count = await MergeNodesAsync("ASTNode", nodes, "uid");
        await MergeRelationshipsAsync(contains);
        if (nodes.Count > 0)
        {
            // A single MERGE without the batch helper: a missing CobolFile is not worth a warning.
            var root = RelationshipBatch.ByUid("CobolFile", "HAS_AST", "ASTNode",
                [RektGraphRows.Link(RektGraphRows.Uid(runId, program), nodes[0]["uid"]!)]);
            await target.MergeRelationshipsAsync(root, root.Rows);
        }
        return count;
    }

    public async Task<long> IngestCfgAsync(string program, JsonElement cfg, long runId)
    {
        long count = 0;
        foreach (var batch in RektGraphRows.Cfg(program, cfg, runId)) count += await MergeRelationshipsAsync(batch);
        return count;
    }

    public async Task<long> IngestDataStructuresAsync(string program, JsonElement ds, long runId)
    {
        var (nodes, relationships) = RektGraphRows.DataStructures(program, ds, runId);
        var count = await MergeNodesAsync("DataStructure", nodes, "uid");
        foreach (var batch in relationships) await MergeRelationshipsAsync(batch);
        return count;
    }

    public async Task<long> CreateSourceBlocksAsync(string program, string content, object runId)
    {
        var (blocks, links) = RektGraphRows.SourceBlocks(program, content, runId);
        var count = await MergeNodesAsync("SourceBlock", blocks, "uid");
        await MergeRelationshipsAsync(links);
        return count;
    }

    public async Task<List<KeyValuePair<string, long>>> IngestRektOutputsAsync(string rektOutputDir, string sourceDir, long runId)
    {
        var counts = new List<KeyValuePair<string, long>>();
        if (JclScanFailed(rektOutputDir)) throw new GraphIngestException(JclScanFailedMessage);
        if (!Path.Exists(rektOutputDir))
        {
            log.WriteLine($"Rekt output dir not found: {rektOutputDir}");
            return counts;
        }

        var sources = WalkFiles(sourceDir)
            .Where(path => !GraphSourcePaths.RelativeParts(path, sourceDir).Any(part => part is ".rekt-staging" or ".preprocessed"))
            .ToList();
        var programs = sources.Where(path => Suffix(path) is ".cbl" or ".cob").Order(StringComparer.Ordinal);
        var copybooks = sources.Where(path => Suffix(path) == ".cpy").Order(StringComparer.Ordinal);

        var fileNodes = new List<Dictionary<string, object?>>();
        foreach (var file in programs.Concat(copybooks))
        {
            var content = GraphJson.ReadSourceText(file);
            var identity = GraphSourcePaths.SourceRelativePath(file, sourceDir);
            var evidencePath = Path.Combine(rektOutputDir, $"{identity}.report", "dependency-evidence.json");
            var evidenceAvailable = File.Exists(evidencePath);
            var diagnostics = evidenceAvailable ? DependencyDiagnostics(evidencePath) : [];
            fileNodes.Add(new Dictionary<string, object?>
            {
                ["uid"] = RektGraphRows.Uid(runId, identity),
                ["runId"] = runId,
                ["fileName"] = identity,
                ["baseName"] = Path.GetFileName(file),
                ["filePath"] = PythonPath(sourceDir) + "/" + identity,
                ["isCopybook"] = Suffix(file) == ".cpy",
                ["content"] = content,
                ["lineCount"] = (long)(content.Count(c => c == '\n') + 1),
                ["dependencyEvidenceAvailable"] = evidenceAvailable,
                ["dependencyDiagnostics"] = diagnostics,
            });
        }
        counts.Add(new("CobolFile", await MergeNodesAsync("CobolFile", fileNodes, "uid")));
        foreach (var node in fileNodes)
        {
            await CreateSourceBlocksAsync((string)node["fileName"]!, (string)node["content"]!, runId);
        }

        long astTotal = 0, cfgTotal = 0, dsTotal = 0;
        var sourceFiles = fileNodes.Select(n => (string)n["fileName"]!).ToList();
        // Native cfg/ sorts before flow_ast/; load AST endpoints before CFG relationships.
        var artifacts = WalkFiles(rektOutputDir)
            .Where(path => Path.GetFileName(path).EndsWith(".json", StringComparison.Ordinal))
            .Select(path => (Path: path, Parts: GraphSourcePaths.RelativeParts(path, rektOutputDir)))
            .OrderBy(a => !Path.GetFileName(a.Path).Contains("ast", StringComparison.OrdinalIgnoreCase))
            .ThenBy(a => a.Parts, PartsComparer.Instance)
            .ToList();

        foreach (var (artifact, parts) in artifacts)
        {
            if (parts.Any(part => SkippedDirectories.Contains(part))) continue;
            var name = Path.GetFileName(artifact);
            var nameLower = name.ToLowerInvariant();
            // Facts and diagnostics describe evidence; they are not structural graph artifacts.
            if (JclGraphBuilder.IsJclArtifact(name) || nameLower.EndsWith(".facts.json", StringComparison.Ordinal)
                || nameLower is "parse-diagnostics.json" or "dependency-evidence.json")
            {
                continue;
            }
            var relative = string.Join('/', parts);
            var program = GraphSourcePaths.ArtifactSourcePath(artifact, rektOutputDir, sourceFiles);
            if (program is null)
            {
                log.WriteLine($"Skipping {relative}: {GraphSourcePaths.UnresolvedArtifactReason(artifact, rektOutputDir, sourceFiles)}");
                continue;
            }

            JsonDocument document;
            try
            {
                document = GraphJson.Parse(artifact);
            }
            catch (JsonException)
            {
                log.WriteLine($"Skipping invalid JSON: {relative}");
                continue;
            }
            using (document)
            {
                // Degenerate programs (no PROCEDURE DIVISION body) make the patched parser emit a
                // literal null artifact; the walkers expect an object.
                if (document.RootElement.ValueKind != JsonValueKind.Object)
                {
                    log.WriteLine($"Skipping empty artifact (degenerate parse): {relative}");
                    continue;
                }
                var data = document.RootElement;
                if (nameLower.Contains("flow-ast") || nameLower.Contains("ast"))
                    astTotal += await IngestAstAsync(program, data, runId);
                else if (nameLower.Contains("cfg"))
                    cfgTotal += await IngestCfgAsync(program, data, runId);
                else if (nameLower.Contains("data-structure") || nameLower.Contains("data_structure") || nameLower.EndsWith("-data.json"))
                    dsTotal += await IngestDataStructuresAsync(program, data, runId);
            }
        }

        counts.Add(new("ASTNode", astTotal));
        counts.Add(new("CFGEdge", cfgTotal));
        counts.Add(new("DataStructure", dsTotal));
        counts.Add(new("JclNode", await IngestJclOutputsAsync(rektOutputDir, sourceDir, runId)));
        return counts;
    }

    public async Task<long> IngestJclOutputsAsync(string outputDir, string sourceDir, long runId)
    {
        if (JclScanFailed(outputDir)) throw new GraphIngestException(JclScanFailedMessage);
        using var lineage = LoadLineage(Path.Combine(outputDir, JclGraphBuilder.LineageFile));

        long total = 0;
        var factsFiles = WalkFiles(outputDir)
            .Where(path => Path.GetFileName(path).EndsWith(JclGraphBuilder.FactsSuffix, StringComparison.Ordinal))
            .Select(path => (Path: path, Parts: GraphSourcePaths.RelativeParts(path, outputDir)))
            .OrderBy(f => f.Parts, PartsComparer.Instance);
        foreach (var (factsFile, parts) in factsFiles)
        {
            if (parts.Any(part => SkippedDirectories.Contains(part))) continue;
            var relative = string.Join('/', parts);
            JsonDocument document;
            try
            {
                document = GraphJson.Parse(factsFile);
            }
            catch (JsonException)
            {
                log.WriteLine($"Skipping invalid JSON: {relative}");
                continue;
            }
            using (document)
            {
                var facts = document.RootElement;
                if (!GraphJson.TryGet(facts, "job", out var job) || job.ValueKind != JsonValueKind.Object)
                {
                    log.WriteLine($"Skipping job facts without a job: {relative}");
                    continue;
                }
                // A newer schema may change meaning, not just add fields; loading it wrongly is worse than not at all.
                if (GraphJson.TryGet(facts, "schemaVersion", out var version) && version.ValueKind == JsonValueKind.Number
                    && version.GetDouble() > JclGraphBuilder.SupportedSchema)
                {
                    log.WriteLine($"Skipping job facts from a newer jcl-facts: {relative}");
                    continue;
                }

                var (nodes, edges) = JclGraphBuilder.Build(facts, lineage?.RootElement, runId);
                total += await MergeNodesAsync("JclNode", nodes, "uid");
                foreach (var type in edges.Select(e => e.Type).Distinct().Order(StringComparer.Ordinal))
                {
                    var rows = edges.Where(e => e.Type == type)
                        .Select(e => new Dictionary<string, object?> { ["from_id"] = e.FromId, ["to_id"] = e.ToId, ["label"] = e.Label })
                        .ToList();
                    await MergeRelationshipsAsync(RelationshipBatch.ByUid("JclNode", type, "JclNode", rows, "label"));
                }

                // The AST Explorer falls back to SourceBlocks when the source file is not on the portal's disk.
                var program = GraphJson.GetOrNull(job, "file") is { } file && GraphJson.Truthy(file) ? GraphJson.Str(file) : null;
                var root = GraphJson.GetOrNull(facts, "sourceRoot") is { } r && GraphJson.Truthy(r) ? GraphJson.Str(r) : sourceDir;
                if (program is null) continue;
                var sourceFile = Path.Combine(root, program);
                if (!File.Exists(sourceFile)) continue;
                await CreateSourceBlocksAsync(program, GraphJson.ReadSourceText(sourceFile), runId);
            }
        }
        return total;
    }

    public async Task<List<KeyValuePair<string, long>>> MigrateSqliteAsync(string dbPath)
    {
        var counts = new List<KeyValuePair<string, long>>();
        if (!Path.Exists(dbPath))
        {
            log.WriteLine($"SQLite DB not found: {dbPath}");
            return counts;
        }

        using var connection = new SqliteConnection(new SqliteConnectionStringBuilder
        {
            DataSource = dbPath, Mode = SqliteOpenMode.ReadOnly,
        }.ToString());
        connection.Open();
        var sql = new SqliteMigration(connection);

        var runs = sql.Rows("runs")!;
        counts.Add(new("Run", await MergeNodesAsync("Run", runs.Select(r => new Dictionary<string, object?>
        {
            ["id"] = r.Required("id"),
            ["startedAt"] = r.Required("started_at"),
            ["completedAt"] = r.Get("completed_at", ""),
            ["status"] = r.Required("status"),
            ["cobolSource"] = r.Get("cobol_source", ""),
            ["javaOutput"] = r.Get("java_output", ""),
            ["notes"] = r.Get("notes", ""),
        }).ToList())));

        var files = sql.Rows("cobol_files")!;
        counts.Add(new("CobolFile", await MergeNodesAsync("CobolFile", files.Select(f => new Dictionary<string, object?>
        {
            ["uid"] = RektGraphRows.Uid(f.Required("run_id"), f.Required("file_name")),
            ["runId"] = f.Required("run_id"),
            ["fileName"] = f.Required("file_name"),
            ["filePath"] = f.Required("file_path"),
            ["isCopybook"] = SqliteMigration.Truthy(f.Required("is_copybook")),
            ["content"] = f.Get("content", ""),
            ["lineCount"] = (long)Content(f).Split('\n').Length,
        }).ToList(), "uid")));
        await MergeRelationshipsAsync(new RelationshipBatch("Run", "id", "ANALYZED", "CobolFile", "uid",
            files.Select(f => RektGraphRows.Link(f.Required("run_id"), RektGraphRows.Uid(f.Required("run_id"), f.Required("file_name")))).ToList(),
            []));

        foreach (var file in files)
        {
            var content = Content(file);
            if (GraphJson.CodePointLength(content) > 50_000)
            {
                await CreateSourceBlocksAsync(GraphJson.Str(file.Required("file_name")), content, file.Required("run_id")!);
            }
        }

        var dependencies = sql.Rows("dependencies")!;
        counts.Add(new("dependencies", await MergeRelationshipsAsync(new RelationshipBatch(
            "CobolFile", "uid", "DEPENDS_ON", "CobolFile", "uid",
            dependencies.Select(d => new Dictionary<string, object?>
            {
                ["from_id"] = RektGraphRows.Uid(d.Required("run_id"), d.Required("source_file")),
                ["to_id"] = RektGraphRows.Uid(d.Required("run_id"), d.Required("target_file")),
                ["type"] = d.Get("dependency_type", "UNKNOWN"),
                ["lineNumber"] = d.Get("line_number", -1L),
                ["context"] = d.Get("context", ""),
            }).ToList(),
            ["type", "lineNumber", "context"]))));

        // The remaining tables arrived in later schema versions and may not exist yet.
        if (sql.Rows("signatures") is { } signatures)
        {
            counts.Add(new("Signature", await MergeNodesAsync("Signature", signatures.Select(s => new Dictionary<string, object?>
            {
                ["uid"] = RektGraphRows.Uid(s.Required("run_id"), s.Required("source_file"), s.Required("legacy_name")),
                ["runId"] = s.Required("run_id"),
                ["sourceFile"] = s.Required("source_file"),
                ["chunkIndex"] = s.Required("chunk_index"),
                ["legacyName"] = s.Required("legacy_name"),
                ["targetMethodName"] = s.Required("target_method_name"),
                ["targetSignature"] = s.Required("target_signature"),
                ["returnType"] = s.Required("return_type"),
                ["parameters"] = s.Get("parameters", ""),
            }).ToList(), "uid")));
            await MergeRelationshipsAsync(RelationshipBatch.ByUid("CobolFile", "DEFINES", "Signature", signatures.Select(s =>
                RektGraphRows.Link(RektGraphRows.Uid(s.Required("run_id"), s.Required("source_file")),
                    RektGraphRows.Uid(s.Required("run_id"), s.Required("source_file"), s.Required("legacy_name")))).ToList()));
        }

        if (sql.Rows("business_logic") is { } businessLogic)
        {
            counts.Add(new("BusinessLogic", await MergeNodesAsync("BusinessLogic", businessLogic.Select(b => new Dictionary<string, object?>
            {
                ["uid"] = RektGraphRows.Uid(b.Required("run_id"), b.Required("file_name"), "bl"),
                ["runId"] = b.Required("run_id"),
                ["fileName"] = b.Required("file_name"),
                ["isCopybook"] = SqliteMigration.Truthy(b.Get("is_copybook", 0L)),
                ["businessPurpose"] = b.Get("business_purpose", ""),
                ["userStoriesJson"] = b.Get("user_stories_json", ""),
                ["featuresJson"] = b.Get("features_json", ""),
                ["businessRulesJson"] = b.Get("business_rules_json", ""),
            }).ToList(), "uid")));
            await MergeRelationshipsAsync(RelationshipBatch.ByUid("CobolFile", "HAS_BUSINESS_LOGIC", "BusinessLogic", businessLogic.Select(b =>
                RektGraphRows.Link(RektGraphRows.Uid(b.Required("run_id"), b.Required("file_name")),
                    RektGraphRows.Uid(b.Required("run_id"), b.Required("file_name"), "bl"))).ToList()));
        }

        if (sql.Rows("chunk_metadata") is { } chunks)
        {
            counts.Add(new("Chunk", await MergeNodesAsync("Chunk", chunks.Select(c => new Dictionary<string, object?>
            {
                ["uid"] = RektGraphRows.Uid(c.Required("run_id"), c.Required("source_file"), "chunk", c.Required("chunk_index")),
                ["runId"] = c.Required("run_id"),
                ["sourceFile"] = c.Required("source_file"),
                ["chunkIndex"] = c.Required("chunk_index"),
                ["startLine"] = c.Required("start_line"),
                ["endLine"] = c.Required("end_line"),
                ["status"] = c.Required("status"),
                ["tokensUsed"] = c.Get("tokens_used", 0L),
                ["processingTimeMs"] = c.Get("processing_time_ms", 0L),
                ["convertedCode"] = c.Get("converted_code", ""),
            }).ToList(), "uid")));
            await MergeRelationshipsAsync(RelationshipBatch.ByUid("CobolFile", "HAS_CHUNK", "Chunk", chunks.Select(c =>
                RektGraphRows.Link(RektGraphRows.Uid(c.Required("run_id"), c.Required("source_file")),
                    RektGraphRows.Uid(c.Required("run_id"), c.Required("source_file"), "chunk", c.Required("chunk_index")))).ToList()));
        }

        if (sql.Rows("type_mappings") is { } typeMappings)
        {
            counts.Add(new("TypeMapping", await MergeNodesAsync("TypeMapping", typeMappings.Select(t => new Dictionary<string, object?>
            {
                ["uid"] = RektGraphRows.Uid(t.Required("run_id"), t.Required("source_file"), t.Required("legacy_variable")),
                ["runId"] = t.Required("run_id"),
                ["sourceFile"] = t.Required("source_file"),
                ["legacyVariable"] = t.Required("legacy_variable"),
                ["legacyType"] = t.Required("legacy_type"),
                ["targetType"] = t.Required("target_type"),
                ["targetFieldName"] = t.Required("target_field_name"),
                ["isNullable"] = SqliteMigration.Truthy(t.Get("is_nullable", 0L)),
                ["defaultValue"] = t.Get("default_value", ""),
            }).ToList(), "uid")));
            await MergeRelationshipsAsync(RelationshipBatch.ByUid("CobolFile", "HAS_TYPE_MAP", "TypeMapping", typeMappings.Select(t =>
                RektGraphRows.Link(RektGraphRows.Uid(t.Required("run_id"), t.Required("source_file")),
                    RektGraphRows.Uid(t.Required("run_id"), t.Required("source_file"), t.Required("legacy_variable")))).ToList()));
        }

        if (sql.Rows("metrics") is { } metrics)
        {
            counts.Add(new("Metrics", await MergeNodesAsync("Metrics", metrics.Select(m => new Dictionary<string, object?>
            {
                ["uid"] = RektGraphRows.Uid(m.Required("run_id"), "metrics"),
                ["runId"] = m.Required("run_id"),
                ["totalPrograms"] = m.Get("total_programs", 0L),
                ["totalCopybooks"] = m.Get("total_copybooks", 0L),
                ["totalDependencies"] = m.Get("total_dependencies", 0L),
                ["avgDependenciesPerProgram"] = m.Get("avg_dependencies_per_program", 0.0),
                ["mostUsedCopybook"] = m.Get("most_used_copybook", ""),
                ["circularDependenciesJson"] = m.Get("circular_dependencies_json", ""),
                ["mermaidDiagram"] = m.Get("mermaid_diagram", ""),
            }).ToList(), "uid")));
            await MergeRelationshipsAsync(new RelationshipBatch("Run", "id", "HAS_METRICS", "Metrics", "uid", metrics.Select(m =>
                RektGraphRows.Link(m.Required("run_id"), RektGraphRows.Uid(m.Required("run_id"), "metrics"))).ToList(), []));
        }
        return counts;
    }

    private static string Content(SqliteMigration.Row file) => file.Get("content", "") is string { Length: > 0 } text ? text : "";

    private JsonDocument? LoadLineage(string path)
    {
        if (!Path.Exists(path)) return null;
        try
        {
            return GraphJson.Parse(path);
        }
        catch (JsonException)
        {
            log.WriteLine($"Skipping invalid JSON: {JclGraphBuilder.LineageFile}");
            return null;
        }
    }

    private static List<string> DependencyDiagnostics(string evidencePath)
    {
        using var evidence = GraphJson.Parse(evidencePath);
        if (!GraphJson.TryGet(evidence.RootElement, "unresolvedDiagnostics", out var listed)) return [];
        if (listed.ValueKind != JsonValueKind.Array || listed.EnumerateArray().Any(item => item.ValueKind != JsonValueKind.String))
        {
            throw new GraphIngestException($"Invalid dependency diagnostics in {evidencePath}");
        }
        return listed.EnumerateArray().Select(item => item.GetString()!).ToList();
    }

    // pathlib's suffix, lowercased.
    private static string Suffix(string path)
    {
        var name = Path.GetFileName(path);
        var stem = GraphSourcePaths.Stem(name);
        return name[stem.Length..].ToLowerInvariant();
    }

    // str(Path(dir)) drops a trailing separator and "." segments.
    private static string PythonPath(string directory)
    {
        var segments = directory.Replace('\\', '/').Split('/').Where((s, i) => s != "." && (s.Length > 0 || i == 0)).ToList();
        var joined = string.Join('/', segments);
        return joined.Length == 0 ? (directory.StartsWith('/') ? "/" : ".") : joined;
    }

    // rglob: every file below root, hidden ones included, without following directory symlinks.
    internal static IEnumerable<string> WalkFiles(string root)
    {
        if (!Directory.Exists(root)) yield break;
        var options = new EnumerationOptions { AttributesToSkip = 0, IgnoreInaccessible = true };
        var pending = new Stack<string>();
        pending.Push(root);
        while (pending.Count > 0)
        {
            var directory = pending.Pop();
            foreach (var entry in new DirectoryInfo(directory).EnumerateFileSystemInfos("*", options))
            {
                if (entry is DirectoryInfo { LinkTarget: null }) pending.Push(Path.Combine(directory, entry.Name));
                else if (entry is FileInfo) yield return Path.Combine(directory, entry.Name);
            }
        }
    }

    // Path.resolve(): absolute, with symlinks followed (macOS /var is /private/var).
    internal static string RealPath(string path)
    {
        var full = Path.GetFullPath(path);
        var root = Path.GetPathRoot(full)!;
        var resolved = root;
        foreach (var segment in full[root.Length..].Split(Path.DirectorySeparatorChar, StringSplitOptions.RemoveEmptyEntries))
        {
            resolved = Path.Combine(resolved, segment);
            FileSystemInfo info = Directory.Exists(resolved) ? new DirectoryInfo(resolved) : new FileInfo(resolved);
            if (info.Exists && info.LinkTarget is not null)
            {
                resolved = info.ResolveLinkTarget(returnFinalTarget: true)?.FullName ?? resolved;
            }
        }
        return resolved.Length > root.Length ? resolved.TrimEnd(Path.DirectorySeparatorChar) : resolved;
    }

    private sealed class PartsComparer : IComparer<string[]>
    {
        public static readonly PartsComparer Instance = new();

        public int Compare(string[]? x, string[]? y)
        {
            for (var i = 0; i < Math.Min(x!.Length, y!.Length); i++)
            {
                var order = string.CompareOrdinal(x[i], y[i]);
                if (order != 0) return order;
            }
            return x.Length.CompareTo(y.Length);
        }
    }
}
