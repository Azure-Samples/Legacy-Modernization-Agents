using System.Text.Json.Nodes;
using System.Text.RegularExpressions;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Rekt;

// Writes dependency-evidence.json: what the parser exported, bound to hashes, with every gap kept as a diagnostic.
public static partial class RektEvidence
{
    // COBOL Language Support reports, at INFO severity, that a COPY text-name exceeds the
    // 8-character z/OS PDS member limit. It is not a resolution failure.
    private const string CopyNameLengthInfo = "Copybook declaration has more than 8 characters for: ([A-Z0-9][A-Z0-9_-]*)";

    private static readonly string[] CopyResolutionProblems =
        ["unresolved-copybook", "unverified-copybook", "ambiguous-copybook", "unverified-staged-copybook", "native:unresolved-copybook"];

    // Keep the name-length note as a blocker unless that exact COPY was resolved and hash-bound.
    private static void DropBoundCopyNameLengthNotes(JsonArray definitions, List<string> errors, List<string> hostNotes)
    {
        var bound = definitions.Where(item => Str(item, "kind") == "COPY" && PyJson.Truthy(Get(item, "sha256")))
            .Select(item => PyJson.Str(Subscript(item, "name"))!.ToUpperInvariant()).ToHashSet();
        var problems = errors.Where(item => CopyResolutionProblems.Contains(item.LastIndexOf(':') is var colon and >= 0 ? item[..colon] : item))
            .Select(item => item.Split(':')[^1].ToUpperInvariant()).ToHashSet();
        foreach (var item in errors)
            foreach (Match match in Rx("missing copybook:([A-Z0-9][A-Z0-9_-]*)", true).Matches(item))
                problems.Add(match.Groups[1].Value.ToUpperInvariant());
        bool Benign(string diagnostic, string prefix)
        {
            if (!diagnostic.StartsWith(prefix, StringComparison.Ordinal)) return false;
            var match = Full(CopyNameLengthInfo, diagnostic[prefix.Length..], ignoreCase: true);
            return match is not null && bound.Contains(match.Groups[1].Value.ToUpperInvariant())
                && !problems.Contains(match.Groups[1].Value.ToUpperInvariant());
        }
        errors.RemoveAll(item => Benign(item, "native:parser-diagnostic::"));
        errors.AddRange(hostNotes.Where(item => !Benign(item, "parser-diagnostic:None:")));
    }

    public static JsonObject BuildEvidence(string sourceRoot, string relativePath, string outputRoot, int exportExitCode,
        string? stagingRoot = null, string? parserSourceRoot = null, string? dependencyProfile = null, string? sqlDeclarations = null)
    {
        sourceRoot = PyCompat.Resolve(sourceRoot);
        outputRoot = PyCompat.Resolve(outputRoot);
        var source = PyCompat.Resolve(Path.Join(sourceRoot, relativePath));
        if (!IsRelativeTo(source, sourceRoot) || IsAbsolute(relativePath))
            throw PyError.Value("source-relative identity required");
        relativePath = PyCompat.RelPosix(source, sourceRoot);
        var exportPath = Path.Join(outputRoot, relativePath + "-deps.json");
        var report = Path.Join(outputRoot, relativePath + ".report");
        var coverage = new JsonObject();
        foreach (var kind in Kinds) coverage[kind] = "unavailable";
        var result = new JsonObject
        {
            ["schemaVersion"] = 1,
            ["producer"] = Producer,
            ["relativePath"] = relativePath,
            ["originalSourcePath"] = source,
            ["originalSourceSha256"] = PyCompat.Digest(source),
            ["exportPath"] = exportPath,
            ["exportSha256"] = File.Exists(exportPath) ? PyCompat.Digest(exportPath) : null,
            ["exportExitCode"] = exportExitCode,
            ["coverage"] = coverage,
            ["definitions"] = new JsonArray(),
            ["unresolvedDiagnostics"] = new JsonArray(),
            ["placeholderContaminated"] = false,
            ["provenance"] = "parser-dependency-export; source scans are contradiction guards only",
        };
        var errors = new List<string>();
        if (stagingRoot is not null)
        {
            try
            {
                var stagedSource = Path.Join(stagingRoot, relativePath);
                var record = ReadJson(stagedSource + ".rekt-source.json");
                if (PyCompat.Resolve(PathArg(Subscript(record, "originalSourcePath"))) != source
                    || DigestMismatch(Subscript(record, "originalSourceSha256"), source)
                    || DigestMismatch(Subscript(record, "stagedSha256"), stagedSource)
                    || Stub.IsMatch(PyCompat.ReadText(stagedSource, PyCompat.Latin1)))
                    throw PyError.Value("stale-or-placeholder-source");
            }
            catch (Exception error) when (Caught(error))
            {
                errors.Add("unverified-staged-source");
            }
        }
        var text = PyCompat.ReadText(source, PyCompat.Latin1);
        if (Stub.IsMatch(text))
        {
            result["placeholderContaminated"] = true;
            errors.Add("placeholder-source");
        }
        if (ContaminatedReport(report))
        {
            result["placeholderContaminated"] = true;
            errors.Add("placeholder-parser-artifact");
        }
        var logPath = Path.Join(outputRoot, relativePath + ".parse.log");
        if (File.Exists(logPath))
        {
            result["parserLogPath"] = logPath;
            result["parserLogSha256"] = PyCompat.Digest(logPath);
        }
        HashSet<string> copies = [], calls = [];
        var exportValid = false;
        JsonNode? exportDocument = null;
        if (exportExitCode != 0) errors.Add($"dependency-export-failed:{exportExitCode}");
        if (File.Exists(exportPath) || exportExitCode == 0)
        {
            try
            {
                exportDocument = ReadJson(exportPath);
                (copies, calls) = ExportedNames(exportDocument, Path.GetFileName(source));
                exportValid = true;
                coverage["COPY"] = "partial";
                coverage["CALL"] = "partial";
            }
            catch (Exception error) when (Caught(error))
            {
                errors.Add($"dependency-export-invalid:{error.Message}");
            }
        }
        var diagnosticPath = Path.Join(report, "parse-diagnostics.json");
        var nameLengthNotes = new List<string>();
        if (File.Exists(diagnosticPath))
        {
            try
            {
                if (ReadJson(diagnosticPath) is not JsonArray diagnostics) throw PyError.Value("expected diagnostics array");
                foreach (var item in diagnostics)
                {
                    if (item is not JsonObject) throw PyError.Value("invalid diagnostic");
                    var diagnostic = $"parser-diagnostic:{PyJson.PyStr(Get(item, "code"))}:{PyJson.PyStr(Get(item, "message"))}";
                    if (Str(item, "severity") == "INFO" && Get(item, "code") is null
                        && Full(CopyNameLengthInfo, PyJson.PyStr(Get(item, "message")), ignoreCase: true) is not null)
                        nameLengthNotes.Add(diagnostic);
                    else
                        errors.Add(diagnostic);
                }
            }
            catch (Exception error) when (Caught(error))
            {
                errors.Add($"invalid-parser-diagnostics:{error.Message}");
            }
        }
        var expected = RektSourceIdentity.ReferencedNames(text);
        foreach (var missing in expected.Except(copies).Order(StringComparer.Ordinal)) errors.Add($"copy-not-exported:{missing}");
        if (expected.Count > 0 && copies.Count == 0) errors.Add("empty-COPY-export-contradiction");
        var guarded = RektSourceIdentity.DependencyGuardText(text);
        foreach (Match match in Rx(@"(?<![\w-])CALL\s+['""]([^'""]+)['""]", true).Matches(guarded))
            if (!calls.Contains(match.Groups[1].Value.ToUpperInvariant())) errors.Add($"call-not-exported:{match.Groups[1].Value}");
        // A dynamic operand passes only when an exported runtime leaf carries its constant proof; that proof is
        // re-verified by NativeRuntimeContractBound before the leaf is trusted.
        var resolved = Items(exportDocument, "dependencies")
            .Where(node => node is JsonObject && Str(node, "dependencyKind") == "EXTERNAL_RUNTIME"
                && Obj(Obj(node, "runtimeContract"), "targetResolution") is { } resolution && Str(resolution, "operand") is not null)
            .Select(node => Str(Obj(Obj(node, "runtimeContract"), "targetResolution"), "operand")!.ToUpperInvariant()).ToHashSet();
        if (Rx(@"(?<![\w-])CALL\s+(?!['""])([A-Z0-9_-]+)", true).Matches(guarded).Any(match => !resolved.Contains(match.Groups[1].Value.ToUpperInvariant())))
            errors.Add("dynamic-CALL-target-unresolved");

        var byName = new Dictionary<string, List<string>>();
        foreach (var path in OriginalFiles(sourceRoot))
            if (PyCompat.Suffix(path).ToLowerInvariant() is ".cpy" or ".cpb")
            {
                var stem = PyCompat.Stem(path).ToUpperInvariant();
                if (!byName.TryGetValue(stem, out var list)) byName[stem] = list = [];
                list.Add(path);
            }
        // Include original references for missing/placeholder diagnostics, never as exported deps.
        var native = false;
        var definitions = (JsonArray)result["definitions"]!;
        if (exportValid && PyJson.PyEquals(Get(exportDocument, "dependencyExportSchemaVersion"), 1))
        {
            (native, definitions) = NativeBindings(exportDocument, sourceRoot, relativePath, stagingRoot, parserSourceRoot, errors,
                dependencyProfile, sqlDeclarations);
            result["definitions"] = definitions;
            var (closureDefinitions, closureProblem) = CommareaClosureBindings(exportDocument!, outputRoot, relativePath);
            foreach (var definition in closureDefinitions) definitions.Add(definition);
            if (closureProblem is not null) errors.Add(closureProblem);
            if (errors.Any(item => item.EndsWith(":placeholder-definition", StringComparison.Ordinal)))
                result["placeholderContaminated"] = true;
        }
        var pending = copies.Union(expected).Order(StringComparer.Ordinal).ToList();
        var visited = new HashSet<string>();
        while (pending.Count > 0)
        {
            var name = pending[^1];
            pending.RemoveAt(pending.Count - 1);
            if (!visited.Add(name)) continue;
            if (definitions.Any(item => Str(item, "kind") == "PLATFORM_COPY" && Str(item, "name")?.ToUpperInvariant() == name)) continue;
            var candidates = byName.GetValueOrDefault(name) ?? [];
            var genuine = candidates.Where(path => DefinitionProblem(path) is null).ToList();
            if (genuine.Count == 0)
            {
                if (stagingRoot is not null && DerivedDefinition(stagingRoot, name, copies, native, definitions, errors)) continue;
                if (candidates.Any(path => DefinitionProblem(path) == "placeholder-definition"))
                    result["placeholderContaminated"] = true;
                errors.Add(candidates.Count == 0 ? $"unresolved-copybook:{name}" : $"unverified-copybook:{name}");
                continue;
            }
            if (genuine.Select(path => PyCompat.Digest(path)).Distinct().Count() != 1)
            {
                errors.Add($"ambiguous-copybook:{name}");
                continue;
            }
            var selected = genuine.Order(PathOrder).First();
            var stagedVerified = stagingRoot is null;
            if (stagingRoot is not null)
            {
                var staged = Path.Join(stagingRoot, Path.GetFileName(selected));
                try
                {
                    var stagedRecord = ReadJson(staged + ".rekt-source.json");
                    if (PyCompat.Resolve(PathArg(Subscript(stagedRecord, "originalSourcePath"))) != PyCompat.Resolve(selected)
                        || DigestMismatch(Subscript(stagedRecord, "originalSourceSha256"), selected)
                        || DigestMismatch(Subscript(stagedRecord, "stagedSha256"), staged)
                        || Stub.IsMatch(PyCompat.ReadText(staged, PyCompat.Latin1)))
                        throw PyError.Value("stale-or-placeholder-staging");
                    stagedVerified = true;
                }
                catch (Exception error) when (Caught(error))
                {
                    errors.Add($"unverified-staged-copybook:{name}");
                }
            }
            if (copies.Contains(name) && stagedVerified && !native) errors.Add($"definition-resolution-not-exported:{name}");
            var nested = RektSourceIdentity.ReferencedNames(PyCompat.ReadText(selected, PyCompat.Latin1));
            foreach (var missing in nested.Except(copies).Order(StringComparer.Ordinal)) errors.Add($"nested-copy-not-exported:{missing}");
            pending.AddRange(nested.Except(visited).Order(StringComparer.Ordinal));
        }

        DropBoundCopyNameLengthNotes(definitions, errors, nameLengthNotes);

        // The legacy tree has no per-kind completeness guarantee. Even an empty, exit-0
        // export is not proof that all dependency kinds were visited by the parser.
        if (native)
        {
            errors.AddRange(MissingNativeOperations(source, exportDocument));
            var declared = GetOr(exportDocument, "dependencyCoverage", new JsonObject());
            if (declared is not JsonObject)
                errors.Add("invalid-native-coverage");
            else
            {
                foreach (var kind in Kinds)
                {
                    var status = Str(declared, kind);
                    if (status is not ("complete" or "partial" or "unavailable"))
                    {
                        errors.Add($"native-coverage-not-declared:{kind}");
                        continue;
                    }
                    if (status == "complete" && AbsenceProofs.ContainsKey(kind))
                    {
                        if (!NativeDomainEvidence(exportDocument, kind))
                        {
                            errors.Add($"native-absence-proof-missing:{kind}");
                            status = "partial";
                        }
                        else if (!PyJson.IsTrue(Get(exportDocument, "dependencyAnalysisComplete")))
                        {
                            errors.Add($"native-analysis-incomplete:{kind}");
                            status = "partial";
                        }
                    }
                    coverage[kind] = status;
                }
                if (errors.Count > 0)
                    foreach (var kind in Kinds)
                        if (Str(coverage, kind) == "complete") coverage[kind] = "partial";
            }
        }
        else if (exportValid)
            errors.Add("exporter-coverage-not-declared:COPY,CALL");
        result["unresolvedDiagnostics"] = ToArray(errors);
        return result;
    }

    // Grounded BMS outputs and bundled definitions carry an input/output hash chain; directory names or
    // unmarked historical files do not.
    private static bool DerivedDefinition(string stagingRoot, string name, HashSet<string> copies, bool native, JsonArray definitions, List<string> errors)
    {
        try
        {
            var staged = Directory.EnumerateFileSystemEntries(stagingRoot).Select(Norm)
                .FirstOrDefault(path => PyCompat.Stem(path).ToUpperInvariant() == name && PyCompat.Suffix(path).ToLowerInvariant() is ".cpy" or ".cpb");
            if (staged is null) return false;
            var record = ReadJson(staged + ".rekt-source.json");
            var transformed = PathArg(Subscript(record, "transformedPath"));
            var original = CurrentTransform(transformed);
            if (original is null || PyCompat.Resolve(original) != PyCompat.Resolve(PathArg(Subscript(record, "originalSourcePath")))
                || DigestMismatch(Subscript(record, "originalSourceSha256"), original)
                || DigestMismatch(Subscript(record, "stagedSha256"), staged)
                || DigestMismatch(Subscript(record, "transformedSha256"), transformed))
                throw PyError.Value("invalid-derived-definition");
            if (copies.Contains(name) && !native)
            {
                errors.Add($"definition-resolution-not-exported:{name}");
                if (PyCompat.Suffix(original).ToLowerInvariant() == ".bms")
                    BmsLayoutDefinition(original, transformed, name, definitions, errors);
            }
            return true;
        }
        catch (Exception error) when (Caught(error))
        {
            return false;
        }
    }

    // pathlib orders paths part by part, so "a/b" sorts before "a-c/x".
    internal static readonly Comparer<string> PathOrder = Comparer<string>.Create((left, right) =>
    {
        var leftParts = Parts(left);
        var rightParts = Parts(right);
        if (left.StartsWith('/')) leftParts = ["/", .. leftParts];
        if (right.StartsWith('/')) rightParts = ["/", .. rightParts];
        for (var index = 0; index < Math.Min(leftParts.Length, rightParts.Length); index++)
        {
            var compared = string.CompareOrdinal(leftParts[index], rightParts[index]);
            if (compared != 0) return compared;
        }
        return leftParts.Length.CompareTo(rightParts.Length);
    });

    public static int Write(string sourceRoot, string relativePath, string outputRoot, int exportExitCode, string? stagingRoot,
        string? parserSourceRoot, string? dependencyProfile, string? sqlDeclarations)
    {
        var evidence = BuildEvidence(sourceRoot, relativePath, outputRoot, exportExitCode, stagingRoot, parserSourceRoot,
            dependencyProfile, sqlDeclarations);
        WriteJson(Path.Join(outputRoot, relativePath + ".report", "dependency-evidence.json"), evidence);
        return Arr(evidence, "unresolvedDiagnostics")!.Any(item => PyJson.Str(item) is { } text
            && (text.StartsWith("dependency-export-failed:", StringComparison.Ordinal) || text.StartsWith("dependency-export-invalid:", StringComparison.Ordinal)))
            ? 1 : 0;
    }

    public static void ResetPreprocessed(string outputRoot)
    {
        Directory.CreateDirectory(outputRoot);
        PreserveAll(Directory.EnumerateFileSystemEntries(outputRoot).Where(path => Path.GetFileName(path) != ".previous").ToList(), outputRoot);
    }

    public static void Quarantine(string outputRoot, string relativePath) =>
        PreserveAll(new[] { ".report", "-deps.json", ".facts.json", ".parse.log" }.Select(suffix => Path.Join(outputRoot, relativePath + suffix)).ToList(), outputRoot);
}
