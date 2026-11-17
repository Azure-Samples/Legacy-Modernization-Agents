using System.Text.Json;
using System.Text.Json.Nodes;
using System.Text.RegularExpressions;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Rekt;

// Records that bind staged and preprocessed files to their exact original bytes.
public static partial class RektEvidence
{
    public const string Producer = "cobol-rekt/dependency";

    private static readonly HashSet<string> SkipDirs =
        [".preprocessed", ".rekt-staging", ".git", ".parser-input", ".dependency-input", ".previous", ".work"];

    internal static readonly Regex Stub = new(
        @"AUTO-GENERATED STUB COPYBOOK|\b(?:DFHAID|DFHBMSCA|SQLCA)-STUB\b|^\s*(?:\d{6}\s*)?(?:01|77)\s+[\w-]+-STUB\b",
        RegexOptions.IgnoreCase | RegexOptions.Multiline | RegexOptions.CultureInvariant);

    // The exceptions the Python tool caught as OSError, ValueError, KeyError or TypeError.
    internal static bool Caught(Exception error) => error is PyError or IOException or UnauthorizedAccessException
        or JsonException or InvalidOperationException or KeyNotFoundException or ArgumentException
        or FormatException or InvalidCastException or NullReferenceException or OverflowException;

    public static JsonNode? ReadJson(string path)
    {
        var text = PyCompat.ReadText(path);
        try
        {
            return PyJson.Parse(text);
        }
        catch (JsonException error)
        {
            throw PyError.Value(JsonDecodeMessage(text, error));
        }
    }

    // json.JSONDecodeError text; the column arithmetic is approximate for multi-line input.
    private static string JsonDecodeMessage(string text, JsonException error)
    {
        var line = (int)(error.LineNumber ?? 0) + 1;
        var column = (int)(error.BytePositionInLine ?? 0) + 1;
        var lines = text.Split('\n');
        var position = lines.Take(line - 1).Sum(item => item.Length + 1) + column - 1;
        return $"Expecting value: line {line} column {column} (char {position})";
    }

    public static void WriteJson(string path, JsonNode? value)
    {
        Directory.CreateDirectory(Path.GetDirectoryName(Path.GetFullPath(path))!);
        // Refuse to overwrite evidence from a previous attempt without preserving it.
        if (File.Exists(path) || Directory.Exists(path)) PreserveAll([path], Path.GetDirectoryName(path) is { Length: > 0 } parent ? parent : ".");
        File.WriteAllText(path, PyJson.Dumps(value, indent: 2) + "\n", PyCompat.Utf8);
    }

    public static void PreserveAll(IEnumerable<string> paths, string root)
    {
        var existing = paths.Where(path => File.Exists(path) || Directory.Exists(path)).ToList();
        if (existing.Count == 0) return;
        var archive = Path.Join(root, ".previous",
            DateTime.UtcNow.ToString("yyyyMMdd'T'HHmmss", System.Globalization.CultureInfo.InvariantCulture) + "-" + PyCompat.NewHex());
        foreach (var path in existing)
        {
            if (!IsRelativeTo(path, root)) throw PyError.Value($"{PyJson.ReprString(Norm(path))} is not in the subpath of {PyJson.ReprString(Norm(root))}");
            var target = Path.Join(archive, Path.GetRelativePath(root, path));
            Directory.CreateDirectory(Path.GetDirectoryName(target)!);
            if (Directory.Exists(path) && new DirectoryInfo(path).LinkTarget is null) Directory.Move(path, target);
            else File.Move(path, target);
        }
    }

    public static IEnumerable<string> OriginalFiles(string root)
    {
        foreach (var (directory, files) in PyCompat.Walk(root, name => !SkipDirs.Contains(name) && !name.StartsWith(".convert-", StringComparison.Ordinal), sorted: true))
            foreach (var name in files.Order(StringComparer.Ordinal))
                yield return Norm(Path.Join(directory, name));
    }

    internal static bool ContaminatedReport(string report) =>
        OriginalFiles(report).Any(path => PyCompat.Suffix(path).ToLowerInvariant() == ".json"
            && Path.GetFileName(path) != "dependency-evidence.json"
            && !Parts(path).Contains(".previous")
            && Stub.IsMatch(PyCompat.ReadText(path, PyCompat.Latin1)));

    internal static string[] Parts(string path) => Norm(path).Split('/', StringSplitOptions.RemoveEmptyEntries);

    public static string? DefinitionProblem(string path)
    {
        if (Stub.IsMatch(PyCompat.ReadText(path, PyCompat.Latin1))) return "placeholder-definition";
        var generatedDirs = (Environment.GetEnvironmentVariable("REKT_GENERATED_COPYBOOK_DIRS") ?? "copy-generated").Split(',')
            .Select(name => name.Trim().ToLowerInvariant()).Where(name => name.Length > 0).ToHashSet();
        if (PyCompat.Suffix(path).ToLowerInvariant() is ".cpy" or ".cpb" or ".copy"
            && Parts(path).Any(part => generatedDirs.Contains(part.ToLowerInvariant())))
        {
            // A generated-folder name establishes neither authenticity nor that a file is a stub.
            try
            {
                var record = ReadJson(path + ".preprocess.json");
                var original = PathArg(Subscript(record, "originalSourcePath"));
                if (PyJson.PyEquals(Subscript(record, "producer"), "bms-symbolic-map") && PyCompat.Suffix(original).ToLowerInvariant() == ".bms"
                    && PyJson.PyEquals(Subscript(record, "originalSourceSha256"), PyCompat.Digest(original))
                    && PyJson.PyEquals(Subscript(record, "outputSha256"), PyCompat.Digest(path)))
                    return null;
            }
            catch (Exception error) when (Caught(error))
            {
            }
            return "unverified-definition-provenance";
        }
        return null;
    }

    public static void Provenance(string source, string output, string producer = "rekt-preprocess") =>
        WriteJson(output + ".preprocess.json", new JsonObject
        {
            ["producer"] = producer,
            ["originalSourcePath"] = PyCompat.Resolve(source),
            ["originalSourceSha256"] = PyCompat.Digest(source),
            ["outputSha256"] = PyCompat.Digest(output),
        });

    public static string? CurrentTransform(string output, string? source = null)
    {
        try
        {
            var record = ReadJson(output + ".preprocess.json");
            var original = PathArg(Subscript(record, "originalSourcePath"));
            if (source is not null && PyCompat.Resolve(original) != PyCompat.Resolve(source)) return null;
            if (PyJson.PyEquals(Subscript(record, "producer"), "platform-event-contract/v1"))
            {
                var profile = ReadJson(original);
                PlatformCopybookContracts.Validate(profile);
                var contract = Subscript(Subscript(profile, "copybooks"), PyCompat.Stem(output).ToUpperInvariant());
                if (!PyCompat.ReadBytes(output).AsSpan().SequenceEqual(PlatformCopybookContracts.Projection(contract))) return null;
            }
            else if (!(PyJson.PyEquals(Subscript(record, "producer"), "rekt-preprocess") || PyJson.PyEquals(Subscript(record, "producer"), "bms-symbolic-map")))
                return null;
            if (!PyJson.PyEquals(Subscript(record, "originalSourceSha256"), PyCompat.Digest(original))
                || !PyJson.PyEquals(Subscript(record, "outputSha256"), PyCompat.Digest(output)))
                return null;
            if (DefinitionProblem(original) is not null || Stub.IsMatch(PyCompat.ReadText(output, PyCompat.Latin1))) return null;
            return original;
        }
        catch (Exception error) when (Caught(error))
        {
            return null;
        }
    }

    // record[key] on a decoded JSON value, raising what Python raises.
    internal static JsonNode? Subscript(JsonNode? node, string key)
    {
        if (node is JsonObject obj)
            return obj.TryGetPropertyValue(key, out var value) ? value : throw PyError.Key(key);
        throw new PyError("TypeError", node switch
        {
            null => "'NoneType' object is not subscriptable",
            JsonArray => "list indices must be integers or slices, not str",
            JsonValue when PyJson.IsStr(node) => "string indices must be integers, not 'str'",
            JsonValue when PyJson.IsBool(node) => "'bool' object is not subscriptable",
            JsonValue when PyJson.IsInt(node) => "'int' object is not subscriptable",
            _ => "'float' object is not subscriptable",
        });
    }

    // Path(value) accepts only strings.
    internal static string PathArg(JsonNode? node)
    {
        if (PyJson.Str(node) is { } text) return text;
        var type = node switch
        {
            null => "NoneType",
            JsonArray => "list",
            JsonObject => "dict",
            _ when PyJson.IsBool(node) => "bool",
            _ when PyJson.IsInt(node) => "int",
            _ => "float",
        };
        throw new PyError("TypeError", $"argument should be a str or an os.PathLike object where __fspath__ returns a str, not {PyJson.ReprString(type)}");
    }

    internal static string? BmsGrounding(string output)
    {
        var original = CurrentTransform(output);
        if (original is null) return null;
        if (PyCompat.Suffix(original).ToLowerInvariant() == ".bms") return original;
        try
        {
            var record = ReadJson(output + ".preprocess.json");
            if (!Has(record, "bmsSourcePath")) return null;
            var bms = PathArg(Subscript(record, "bmsSourcePath"));
            if (PyCompat.Suffix(bms).ToLowerInvariant() != ".bms" || !PyJson.PyEquals(Subscript(record, "bmsSourceSha256"), PyCompat.Digest(bms)))
                return null;
            return bms;
        }
        catch (Exception error) when (Caught(error))
        {
            return null;
        }
    }

    public static void Stage(string source, string target, string? transformed, bool derived, TextWriter stderr)
    {
        string original, selected;
        if (derived)
        {
            original = CurrentTransform(source) ?? throw PyError.Value($"unverified preprocessed definition: {Norm(source)}");
            selected = source;
        }
        else
        {
            original = source;
            var copybook = PyCompat.Suffix(source).ToLowerInvariant() is ".cpy" or ".cpb";
            if (copybook && DefinitionProblem(source) is { } problem) throw PyError.Value($"{problem}: {Norm(source)}");
            selected = !string.IsNullOrEmpty(transformed) && CurrentTransform(transformed, source) is not null ? transformed : source;
            if (copybook && CurrentTransform(source) is { } grounding && PyCompat.Suffix(grounding).ToLowerInvariant() == ".bms")
                (original, selected) = (grounding, source);
        }
        var targetDirectory = Path.GetDirectoryName(Path.GetFullPath(target))!;
        Directory.CreateDirectory(targetDirectory);
        CopyFile(selected, target);
        var transformedSelection = !SamePath(selected, original);
        if (transformedSelection && File.Exists(selected + ".preprocess.json"))
            CopyFile(selected + ".preprocess.json", target + ".preprocess.json");
        var bms = PyCompat.Suffix(original).ToLowerInvariant() == ".bms" ? original
            : !SamePath(selected, source) ? BmsGrounding(selected) : null;
        if (bms is not null)
        {
            foreach (var suffix in new[] { ".bms-layout.json", ".bms-source" })
                if (File.Exists(target + suffix)) File.Delete(target + suffix);
            JsonObject? proof = null;
            try
            {
                proof = BmsSymbolicMap.CompleteLayoutEvidence(bms, selected);
            }
            catch (Exception error) when (BmsSymbolicMap.Caught(error))
            {
                stderr.WriteLine($"BMS layout remains partial: {Norm(bms)}: {error.Message}");
            }
            if (proof is not null)
            {
                if (!File.Exists(selected + ".bms-layout.json") || !PyJson.PyEquals(ReadJson(selected + ".bms-layout.json"), proof))
                    WriteJson(selected + ".bms-layout.json", proof);
                CopyFile(selected + ".bms-layout.json", target + ".bms-layout.json");
                CopyFile(bms, target + ".bms-source");
            }
        }
        WriteJson(target + ".rekt-source.json", new JsonObject
        {
            ["originalSourcePath"] = PyCompat.Resolve(original),
            ["originalSourceSha256"] = PyCompat.Digest(original),
            ["stagedSha256"] = PyCompat.Digest(target),
            ["transformedPath"] = transformedSelection ? PyCompat.Resolve(selected) : null,
            ["transformedSha256"] = transformedSelection ? PyCompat.Digest(selected) : null,
        });
    }

    // shutil.copyfile: contents only, raising Python's OSError text for a missing source.
    internal static void CopyFile(string source, string target)
    {
        var bytes = PyCompat.ReadBytes(source);
        File.WriteAllBytes(target, bytes);
    }

    internal static void BmsLayoutDefinition(string source, string copybook, string name, JsonArray definitions, List<string> errors)
    {
        try
        {
            var path = copybook + ".bms-layout.json";
            if (!PyJson.PyEquals(ReadJson(path), BmsSymbolicMap.CompleteLayoutEvidence(source, copybook)))
                throw PyError.Value("symbolic layout manifest differs from the original BMS recipe");
            var definition = new JsonObject
            {
                ["kind"] = "BMS_LAYOUT",
                ["name"] = PyCompat.Stem(name).ToUpperInvariant(),
                ["path"] = PyCompat.Resolve(path),
                ["sha256"] = PyCompat.Digest(path),
            };
            AppendUnique(definitions, definition);
        }
        catch (Exception error) when (Caught(error))
        {
            errors.Add($"partial-derived-bms-layout:{name}");
            errors.Add($"bms-layout-unverified:{name}:{error.Message}");
        }
    }

    internal static void AppendUnique(JsonArray definitions, JsonNode definition)
    {
        if (!definitions.Any(item => PyJson.PyEquals(item, definition))) definitions.Add(definition);
    }

    public static string DependencySearchView(string stagingRoot, string relativePath)
    {
        stagingRoot = PyCompat.Resolve(stagingRoot);
        var selected = PyCompat.Resolve(Path.Join(stagingRoot, relativePath));
        if (!IsRelativeTo(selected, stagingRoot) || Path.IsPathRooted(relativePath))
            throw PyError.Value("source-relative identity required");
        var target = Path.Join(stagingRoot, ".dependency-input", relativePath);
        var candidates = new SortedDictionary<string, List<string>>(StringComparer.Ordinal);
        var order = new List<string>();
        foreach (var path in OriginalFiles(stagingRoot))
            if (PyCompat.Suffix(path).ToLowerInvariant() is ".cbl" or ".cob")
            {
                var stem = PyCompat.Stem(path).ToUpperInvariant();
                if (!candidates.TryGetValue(stem, out var list)) { candidates[stem] = list = []; order.Add(stem); }
                list.Add(path);
            }
        var selectedStem = PyCompat.Stem(selected).ToUpperInvariant();
        if (!candidates.TryGetValue(selectedStem, out var own) || !own.Any(path => SamePath(path, selected)))
            throw PyError.Value("selected program is not a staged program");
        PreserveAll([target], Path.GetDirectoryName(target)!);
        if (Directory.Exists(target) || File.Exists(target)) throw new PyError("FileExistsError", $"[Errno 17] File exists: {PyJson.ReprString(target)}");
        Directory.CreateDirectory(target);
        var ambiguous = new JsonObject();
        foreach (var name in order)
            if (candidates[name].Count != 1)
                ambiguous[name] = ToArray(candidates[name].Select(path => PyCompat.RelPosix(path, stagingRoot)));
        // The selected entry is explicit even if another estate has the same basename.
        var inputs = new List<string> { selected };
        inputs.AddRange(candidates.Where(pair => pair.Value.Count == 1 && pair.Key != selectedStem).Select(pair => pair.Value[0]));
        foreach (var source in inputs)
        {
            var record = ReadJson(source + ".rekt-source.json");
            if (!PyJson.PyEquals(Subscript(record, "stagedSha256"), PyCompat.Digest(source)))
                throw PyError.Value($"stale staged program: {Norm(source)}");
            CopyFile(source, Path.Join(target, Path.GetFileName(source)));
            CopyFile(source + ".rekt-source.json", Path.Join(target, Path.GetFileName(source)) + ".rekt-source.json");
        }
        var programs = new JsonObject();
        foreach (var path in inputs) programs[Path.GetFileName(path)] = PyCompat.RelPosix(path, stagingRoot);
        WriteJson(Path.Join(target, ".rekt-search.json"), new JsonObject
        {
            ["relativePath"] = PyCompat.RelPosix(selected, stagingRoot),
            ["ambiguousPrograms"] = ambiguous,
            ["programs"] = programs,
        });
        return target;
    }

    internal static List<JsonObject> ExportedNodes(JsonNode? export, string basename)
    {
        if (export is not JsonObject || Arr(export, "dependencies") is not { } dependencies)
            throw PyError.Value("invalid-export-schema");
        var callTarget = Get(export, "callTarget");
        if (callTarget is not JsonObject || PyJson.Str(Has(callTarget, "name") ? Get(callTarget, "name") : "") is not { } targetName
            || targetName.ToLowerInvariant() != basename.ToLowerInvariant())
            throw PyError.Value("export-source-identity-mismatch");
        var nodes = new List<JsonObject>();
        void Walk(JsonArray items)
        {
            foreach (var node in items)
            {
                if (node is not JsonObject obj) throw PyError.Value("invalid-export-dependency");
                var target = Has(obj, "callTarget") ? Get(obj, "callTarget") : obj;
                if (target is not JsonObject || Str(target, "name") is not { Length: > 0 })
                    throw PyError.Value("invalid-export-target");
                nodes.Add(obj);
                if (Has(obj, "dependencies"))
                {
                    if (Get(obj, "dependencies") is not JsonArray children) throw PyError.Value("invalid-export-dependency");
                    Walk(children);
                }
            }
        }
        Walk(dependencies);
        return nodes;
    }

    internal static (HashSet<string> Copies, HashSet<string> Calls) ExportedNames(JsonNode? export, string basename)
    {
        var copies = new HashSet<string>(StringComparer.Ordinal);
        var calls = new HashSet<string>(StringComparer.Ordinal);
        ExportedNodes(export, basename);
        foreach (var node in Arr(export, "dependencies")!)
        {
            var target = Has(node, "callTarget") ? Get(node, "callTarget") : node;
            var name = Str(target, "name")!;
            var kind = Has(node, "dependencyKind") ? Get(node, "dependencyKind") : "CALL";
            if (StrEquals(Get(node, "dependencyKind"), "COPY") || StrEquals(Get(node, "dependencyKind"), "PLATFORM_COPY")
                || PyCompat.Suffix(name).ToLowerInvariant() is ".cpy" or ".cpb")
                copies.Add(PyCompat.Stem(name).ToUpperInvariant());
            else if (!StrEquals(Get(target, "programReferenceType"), "DYNAMIC")
                && PyJson.Str(kind) is "CALL" or "EXTERNAL_RUNTIME" or "CICS_LINK" or "CICS_RUN")
                calls.Add(name.ToUpperInvariant());
        }
        return (copies, calls);
    }

    // pathlib's lexical normalisation: repeated and trailing separators and "." parts disappear, ".." stays.
    internal static string Norm(string path)
    {
        if (path.Length == 0) return ".";
        path = path.Replace('\\', '/');
        var absolute = path.StartsWith('/');
        var parts = path.Split('/', StringSplitOptions.RemoveEmptyEntries).Where(part => part != ".");
        var joined = string.Join('/', parts);
        return absolute ? "/" + joined : joined.Length == 0 ? "." : joined;
    }

    internal static bool SamePath(string left, string right) => Norm(left) == Norm(right);

    // PurePath.is_relative_to: a lexical prefix check on whole parts.
    internal static bool IsRelativeTo(string path, string root)
    {
        var child = Norm(path);
        var parent = Norm(root);
        if (parent == ".") return !child.StartsWith('/');
        return child == parent || child.StartsWith(parent == "/" ? "/" : parent + "/", StringComparison.Ordinal);
    }
}
