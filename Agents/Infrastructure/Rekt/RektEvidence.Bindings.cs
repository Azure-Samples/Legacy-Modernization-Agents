using System.Text.Json.Nodes;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Rekt;

// Accepts only the definitions the native parser resolved and that hash back to genuine estate inputs.
public static partial class RektEvidence
{
    private sealed class BindingScope(string sourceRoot, string relativePath, string? stagingRoot, string parserSourceRoot,
        string? dependencyProfile)
    {
        public string SourceRoot { get; } = sourceRoot;
        public string Source { get; } = Path.Join(sourceRoot, relativePath);
        public string? StagingRoot { get; } = stagingRoot;
        public string ParserSourceRoot { get; } = parserSourceRoot;

        public string HostPath(JsonNode? value, bool profile = false)
        {
            var path = PathArg(value);
            if (!IsAbsolute(path)) throw PyError.Value("native input path must be absolute");
            if (IsRelativeTo(path, ParserSourceRoot)) path = Rebase(path, ParserSourceRoot, SourceRoot);
            else if (!profile || dependencyProfile is null) throw PyError.Value("native input outside declared parser source root");
            if (profile && dependencyProfile is not null)
            {
                var configured = dependencyProfile;
                if (!IsAbsolute(configured)) throw PyError.Value("configured dependency profile path must be absolute");
                if (IsRelativeTo(configured, ParserSourceRoot)) configured = Rebase(configured, ParserSourceRoot, SourceRoot);
                if (PyCompat.Resolve(path) != PyCompat.Resolve(configured))
                    throw PyError.Value("native profile does not match explicitly configured dependency profile");
            }
            return path;
        }

        public string Host(JsonNode? value, bool profile = false) => PyCompat.Resolve(HostPath(value, profile));
    }

    // root / path.relative_to(parent)
    private static string Rebase(string path, string parent, string root)
    {
        var relative = Norm(path)[Norm(parent).Length..].TrimStart('/');
        return relative.Length == 0 ? Norm(root) : Norm(Path.Join(root, relative));
    }

    private static JsonObject Definition(string kind, JsonNode? name, string path, JsonNode? sha256) => new()
    {
        ["kind"] = kind,
        ["name"] = name?.DeepClone(),
        ["path"] = path,
        ["sha256"] = sha256?.DeepClone(),
    };

    // digest(path) != record[key], evaluated in Python's order so the first failure names the same cause.
    private static bool DigestDiffers(string path, JsonNode? record, string key)
    {
        var digest = PyCompat.Digest(path);
        return !PyJson.PyEquals(digest, Subscript(record, key));
    }

    private static string Parent(string path) => Path.GetDirectoryName(Norm(path)) is { Length: > 0 } parent ? Norm(parent) : ".";

    internal static (bool Native, JsonArray Definitions) NativeBindings(JsonNode? document, string sourceRoot, string relativePath,
        string? stagingRoot, string? parserSourceRoot, List<string> errors, string? dependencyProfile = null, string? sqlDeclarations = null)
    {
        var definitions = new JsonArray();
        if (!PyJson.PyEquals(Get(document, "dependencyExportSchemaVersion"), 1)) return (false, definitions);
        var staged = !string.IsNullOrEmpty(stagingRoot);
        var scope = new BindingScope(sourceRoot, relativePath, stagingRoot,
            Norm(!string.IsNullOrEmpty(parserSourceRoot) ? parserSourceRoot : staged ? "/source" : sourceRoot), dependencyProfile);
        var source = scope.Source;
        var sourceName = Path.GetFileName(source);
        var parsedSource = staged ? Path.Join(stagingRoot, ".parser-input", relativePath, sourceName) : source;
        var searchView = staged ? Path.Join(stagingRoot, ".dependency-input", relativePath) : null;
        JsonNode? searchRecord;
        JsonNode? ambiguous;
        try
        {
            var nativeSource = scope.Host(Subscript(document, "sourcePath"));
            var expectedSource = searchView is not null && Directory.Exists(searchView) ? Path.Join(searchView, sourceName) : parsedSource;
            if (nativeSource != PyCompat.Resolve(expectedSource)
                || DigestMismatch(Subscript(document, "sourceSha256"), parsedSource)
                || PyCompat.Digest(nativeSource) != PyCompat.Digest(parsedSource)
                || (staged && PyCompat.Digest(nativeSource) != PyCompat.Digest(Path.Join(stagingRoot, relativePath))))
                throw PyError.Value("native parser source identity/hash mismatch");
            searchRecord = searchView is not null && Directory.Exists(searchView) ? ReadJson(Path.Join(searchView, ".rekt-search.json")) : new JsonObject();
            ambiguous = GetOr(searchRecord, "ambiguousPrograms", new JsonObject());
        }
        catch (Exception error) when (Caught(error))
        {
            errors.Add($"native-export-source-unverified:{error.Message}");
            return (false, definitions);
        }
        var owners = new List<JsonObject> { (JsonObject)document! };
        owners.AddRange(ExportedNodes(document, sourceName));
        foreach (var node in owners)
        {
            var diagnostics = GetOr(node, "dependencyDiagnostics", new JsonArray());
            if (diagnostics is not JsonArray diagnosticItems || diagnosticItems.Any(item => !PyJson.IsStr(item)))
                errors.Add("invalid-native-dependency-diagnostics");
            else
                errors.AddRange(diagnosticItems.Select(item => $"native:{PyJson.Str(item)}"));
            var derivedInputs = GetOr(node, "dependencyDerivedInputs", new JsonArray());
            if (derivedInputs is not JsonArray)
            {
                errors.Add("invalid-native-derived-inputs");
                derivedInputs = new JsonArray();
            }
            foreach (var binding in (JsonArray)derivedInputs)
                DerivedInput(scope, node, binding, errors);
            foreach (var statement in GetOr(node, "dependencyDialectStatements", new JsonArray()) as JsonArray ?? [])
                DialectStatement(scope, node, statement, searchView, searchRecord, definitions, errors, sqlDeclarations);
            if (ReferenceEquals(node, document)) continue;
            var target = Has(node, "callTarget") ? Get(node, "callTarget") : node;
            var kindNode = Get(node, "dependencyKind");
            var kind = PyJson.Str(kindNode) is "CICS_LINK" or "CICS_RUN" ? "CALL" : PyJson.Str(kindNode);
            var name = PyJson.Str(Subscript(target, "name"))!;
            if (kind == "PLATFORM_COPY")
            {
                PlatformCopy(scope, document!, node, name, definitions, errors);
                continue;
            }
            if (kind == "EXTERNAL_RUNTIME")
            {
                ExternalRuntime(scope, node, name, relativePath, definitions, errors);
                continue;
            }
            var targetStem = PyCompat.Suffix(name).ToLowerInvariant() is ".cbl" or ".cob" ? PyCompat.Stem(name) : name;
            if (kind == "CALL" && Has(ambiguous, targetStem.ToUpperInvariant()))
                errors.Add($"ambiguous-call-target:{name}");
            if (PyJson.IsTrue(Get(document, "dependencyAnalysisComplete")) && kind != "COPY")
            {
                var childCoverage = GetOr(node, "dependencyCoverage", new JsonObject());
                if (!PyJson.IsTrue(Get(node, "dependencyAnalysisComplete"))
                    || childCoverage is not JsonObject
                    || Kinds.Any(item => Str(childCoverage, item) != "complete")
                    || AbsenceProofs.Keys.Any(item => !NativeDomainEvidence(node, item)))
                    errors.Add($"native-incomplete-callee:{name}");
            }
            if (kind is not ("COPY" or "CALL")) continue;
            if (Str(target, "programReferenceType") != "STATIC")
            {
                errors.Add($"dynamic-call-target-unresolved:{name}");
                continue;
            }
            ResolvedDefinition(scope, document!, owners, node, kind, name, searchView, searchRecord, definitions, errors);
        }
        foreach (var owner in owners)
            if (Str(owner, "dependencyKind") == "PLATFORM_COPY" && Str(GetOr(owner, "callTarget", new JsonObject()), "name") == "DFHBMSCA")
                BmsAttributeContract(scope, document!, owners, owner, definitions, errors);
        return (true, definitions);
    }

    private static bool DigestMismatch(JsonNode? expected, string path) => !PyJson.PyEquals(expected, PyCompat.Digest(path));

    private static void DerivedInput(BindingScope scope, JsonObject node, JsonNode? binding, List<string> errors)
    {
        try
        {
            if (binding is not JsonObject || Str(binding, "kind") != "BMS_LAYOUT")
                throw PyError.Value("unsupported native derived input");
            foreach (var (key, hashKey) in new[] { ("sourcePath", "sourceSha256"), ("layoutPath", "layoutSha256"), ("outputPath", "outputSha256") })
            {
                var path = scope.Host(Subscript(binding, key));
                if (!IsRelativeTo(path, scope.SourceRoot) || DigestDiffers(path, binding, hashKey))
                    throw PyError.Value("native derived input path/hash mismatch");
            }
            var output = Subscript(binding, "outputPath");
            if (!PyJson.PyEquals(Subscript(binding, "layoutPath"), Concat(output, ".bms-layout.json"))
                || !PyJson.PyEquals(Subscript(binding, "sourcePath"), Concat(output, ".bms-source"))
                || !Items(node, "dependencies").Any(child => Str(child, "dependencyKind") == "COPY"
                    && PyJson.PyEquals(Get(child, "sourcePath"), Subscript(binding, "outputPath"))
                    && PyJson.PyEquals(Get(child, "sourceSha256"), Subscript(binding, "outputSha256"))))
                throw PyError.Value("native BMS layout is not bound to a parsed COPY");
        }
        catch (Exception error) when (Caught(error))
        {
            errors.Add($"native-bms-layout-unverified:{error.Message}");
        }
    }

    // value + suffix, which only strings support.
    private static string Concat(JsonNode? value, string suffix) =>
        PyJson.Str(value) is { } text ? text + suffix
            : throw new PyError("TypeError", $"unsupported operand type(s) for +: {PyJson.ReprString(TypeName(value))} and 'str'");

    private static string TypeName(JsonNode? node) => node switch
    {
        null => "NoneType",
        JsonArray => "list",
        JsonObject => "dict",
        _ when PyJson.IsStr(node) => "str",
        _ when PyJson.IsBool(node) => "bool",
        _ when PyJson.IsInt(node) => "int",
        _ => "float",
    };

    private static void DialectStatement(BindingScope scope, JsonObject node, JsonNode? statement, string? searchView,
        JsonNode? searchRecord, JsonArray definitions, List<string> errors, string? sqlDeclarations)
    {
        if (statement is not JsonObject)
        {
            errors.Add("invalid-native-dialect-statement");
            return;
        }
        var bindings = GetOr(statement, "resourceBindings", new JsonArray());
        if (bindings is not JsonArray bindingItems || bindingItems.Any(binding => binding is not JsonObject))
        {
            errors.Add("invalid-native-resource-bindings");
            return;
        }
        foreach (var binding in bindingItems)
        {
            if (Str(binding, "definitionScope") == "explicit-sql-catalog")
            {
                try
                {
                    var path = scope.Host(Subscript(binding, "sourcePath"));
                    var configured = (sqlDeclarations ?? "").Split(Path.PathSeparator).Where(value => value.Length > 0)
                        .Select(value => scope.Host(value)).ToHashSet();
                    if (!IsRelativeTo(path, scope.SourceRoot) || !configured.Contains(path)
                        || DigestDiffers(path, binding, "sourceSha256") || DefinitionProblem(path) is not null
                        || !NativeSqlStatementBound(statement))
                        throw PyError.Value("SQL declaration does not match an explicit native catalog input");
                    AppendUnique(definitions, Definition("SQL_DECLARATION", Subscript(binding, "name"), path, PyCompat.Digest(path)));
                }
                catch (Exception error) when (Caught(error))
                {
                    errors.Add($"native-sql-catalog-unverified:{error.Message}");
                }
            }
            if (!Has(binding, "qualification")) continue;
            var qualification = Get(binding, "qualification");
            try
            {
                var path = scope.Host(Subscript(qualification, "sourcePath"), profile: true);
                var profile = ReadBoundProfile(qualification, path);
                if (!NativeSqlStatementBound(statement) || !PyJson.PyEquals(Get(profile, "sqlDefaultSchema"), Subscript(qualification, "schema")))
                    throw PyError.Value("configured SQL qualification is not supported by the native declaration");
                AppendUnique(definitions, Definition("ANALYSIS_PROFILE", Subscript(profile, "profileName"), path, PyCompat.Digest(path)));
            }
            catch (Exception error) when (Caught(error))
            {
                errors.Add($"native-sql-profile-unverified:{error.Message}");
            }
        }
        if (!NativeCicsStatementBound(statement)) return;
        foreach (var binding in NativeCicsResources(statement))
        {
            if (Str(binding, "kind") == "CICS_MAP")
            {
                CicsMap(scope, binding, definitions, errors);
                continue;
            }
            if (Str(binding, "kind") is not ("CICS_FILE" or "CICS_PROGRAM" or "CICS_TRANSACTION" or "CICS_TDQUEUE")) continue;
            try
            {
                CicsResource(scope, node, statement, binding, searchView, searchRecord, definitions);
            }
            catch (Exception error) when (Caught(error))
            {
                errors.Add($"native-cics-resource-definition-unverified:{PyJson.PyStr(Get(binding, "name"))}:{error.Message}");
            }
        }
    }

    private static void CicsMap(BindingScope scope, JsonNode? binding, JsonArray definitions, List<string> errors)
    {
        try
        {
            var output = scope.Host(Subscript(binding, "outputPath"));
            var layout = scope.Host(Subscript(binding, "layoutPath"));
            var snapshot = scope.Host(Subscript(binding, "sourcePath"));
            foreach (var (path, key) in new[] { (output, "outputSha256"), (layout, "layoutSha256"), (snapshot, "sourceSha256") })
                if (!IsRelativeTo(path, scope.SourceRoot) || DigestDiffers(path, binding, key))
                    throw PyError.Value("native map resource path/hash mismatch");
            var record = ReadJson(output + ".rekt-source.json");
            var original = PathArg(Subscript(record, "originalSourcePath"));
            var transformed = PathArg(Subscript(record, "transformedPath"));
            if (!IsRelativeTo(PyCompat.Resolve(original), scope.SourceRoot)
                || !IsRelativeTo(PyCompat.Resolve(transformed), scope.SourceRoot)
                || DigestMismatch(Subscript(record, "originalSourceSha256"), original)
                || DigestMismatch(Subscript(record, "transformedSha256"), transformed)
                || DigestMismatch(Subscript(record, "stagedSha256"), output)
                || CurrentTransform(transformed, original) is null)
                throw PyError.Value("native map resource lacks current source provenance");
            var bms = BmsGrounding(transformed) ?? throw PyError.Value("native map resource lacks a grounded BMS source");
            var name = Subscript(binding, "name");
            var additions = new List<JsonObject>
            {
                Definition("CICS_MAP", name, PyCompat.Resolve(bms), PyCompat.Digest(bms)),
                Definition("CICS_MAP_LAYOUT", name, PyCompat.Resolve(transformed + ".bms-layout.json"), Subscript(binding, "layoutSha256")),
                Definition("COPY_INPUT", name, PyCompat.Resolve(transformed), PyCompat.Digest(transformed)),
            };
            if (!CicsMapEvidenceCurrent(binding, additions, scope.SourceRoot))
                throw PyError.Value("native map differs from its source-backed BMS recipe");
            foreach (var definition in additions) AppendUnique(definitions, definition);
        }
        catch (Exception error) when (Caught(error))
        {
            errors.Add($"native-cics-map-unverified:{PyJson.PyStr(Get(binding, "name"))}:{error.Message}");
        }
    }

    private static readonly string[] ContinuationRules = ["Cics_returnContext", "Cics_xctlContext", "Cics_inquireContext"];

    private static void CicsResource(BindingScope scope, JsonObject node, JsonNode statement, JsonNode? binding, string? searchView,
        JsonNode? searchRecord, JsonArray definitions)
    {
        var path = scope.Host(Subscript(binding, "sourcePath"));
        if (!IsRelativeTo(path, scope.SourceRoot) || DigestDiffers(path, binding, "sourceSha256"))
            throw PyError.Value("native CICS resource definition identity/hash mismatch");
        var kind = Subscript(binding, "kind");
        if (PyJson.PyEquals(kind, "CICS_PROGRAM"))
        {
            var programPath = scope.Host(Subscript(binding, "programSourcePath"));
            var continuation = ContinuationRules.Contains(Str(statement, "grammarRule"));
            if (!IsRelativeTo(programPath, scope.SourceRoot)
                || DigestDiffers(programPath, binding, "programSourceSha256")
                || !continuation && !Items(node, "dependencies").Any(child => Str(child, "dependencyKind") is "CALL" or "CICS_LINK" or "CICS_RUN"
                    && PyJson.PyEquals(Get(GetOr(child, "callTarget", new JsonObject()), "name"), Subscript(binding, "name"))
                    && PyJson.PyEquals(Get(child, "sourcePath"), Subscript(binding, "programSourcePath"))
                    && PyJson.PyEquals(Get(child, "sourceSha256"), Subscript(binding, "programSourceSha256"))))
                throw PyError.Value("native CICS callee does not match its parsed source");
            if (continuation)
            {
                string original = programPath, current = programPath;
                if (scope.StagingRoot is { Length: > 0 } stagingRoot)
                {
                    var record = ReadJson(programPath + ".rekt-source.json");
                    original = PyCompat.Resolve(PathArg(Subscript(record, "originalSourcePath")));
                    current = PyCompat.Resolve(PyJson.Truthy(Get(record, "transformedPath")) ? PathArg(Get(record, "transformedPath")) : original);
                    var relative = PathArg(Subscript(Subscript(searchRecord, "programs"), Path.GetFileName(programPath)));
                    var canonical = PyCompat.Resolve(Path.Join(stagingRoot, relative));
                    if (IsAbsolute(relative) || !IsRelativeTo(canonical, PyCompat.Resolve(stagingRoot))
                        || Parent(programPath) != Norm(PyCompat.Resolve(searchView!))
                        || !PyJson.PyEquals(record, ReadJson(canonical + ".rekt-source.json"))
                        || original != PyCompat.Resolve(Path.Join(scope.SourceRoot, relative))
                        || DigestMismatch(Subscript(record, "stagedSha256"), programPath)
                        || DigestMismatch(Subscript(record, "originalSourceSha256"), original)
                        || current != original && CurrentTransform(current, original) is null)
                        throw PyError.Value("continuation source provenance invalid");
                }
                if (!IsRelativeTo(current, scope.SourceRoot) || !IsRelativeTo(original, scope.SourceRoot)
                    || DigestDiffers(current, binding, "programSourceSha256")
                    || DefinitionProblem(current) is not null || DefinitionProblem(original) is not null)
                    throw PyError.Value("continuation does not match genuine supplied program source");
                foreach (var (continuationKind, selected) in new[] { ("CICS_CONTINUATION", current), ("CICS_CONTINUATION_SOURCE", original) })
                    AppendUnique(definitions, Definition(continuationKind, Subscript(binding, "name"), selected, PyCompat.Digest(selected)));
            }
        }
        AppendUnique(definitions, Definition(PyJson.Str(kind)!, Subscript(binding, "name"), path, PyCompat.Digest(path)));
        if (!PyJson.PyEquals(kind, "CICS_FILE")) return;
        foreach (var dataset in Items(binding, "datasetDefinitions"))
        {
            var datasetPath = scope.Host(Subscript(dataset, "sourcePath"));
            if (!IsRelativeTo(datasetPath, scope.SourceRoot) || DigestDiffers(datasetPath, dataset, "sourceSha256"))
                throw PyError.Value("native VSAM dataset definition identity/hash mismatch");
            AppendUnique(definitions, Definition("CICS_DATASET", Subscript(dataset, "name"), datasetPath, Subscript(dataset, "sourceSha256")));
        }
    }

    private static void PlatformCopy(BindingScope scope, JsonNode document, JsonObject node, string name, JsonArray definitions, List<string> errors)
    {
        try
        {
            var binding = Subscript(node, "runtimeContract");
            var configured = Environment.GetEnvironmentVariable("REKT_PLATFORM_CONTRACTS");
            if (string.IsNullOrEmpty(configured) || !IsAbsolute(configured))
                throw PyError.Value("no explicitly configured platform replacement profile");
            var profilePath = PyCompat.Resolve(configured);
            var nativeProfile = PathArg(Subscript(binding, "sourcePath"));
            if (IsRelativeTo(nativeProfile, scope.ParserSourceRoot)) nativeProfile = Rebase(nativeProfile, scope.ParserSourceRoot, scope.SourceRoot);
            var profile = ReadJson(profilePath);
            if (PyCompat.Resolve(nativeProfile) != profilePath)
                throw PyError.Value("platform replacement profile path mismatch");
            if (!PyJson.PyEquals(Subscript(binding, "sourceSha256"), PyCompat.Digest(profilePath)))
                throw PyError.Value("platform replacement profile SHA256 mismatch");
            if (!NativePlatformContractBound(node, profile))
            {
                if (Items(document, "dependencyDiagnostics").Any(issue => PyJson.Str(issue)?.Contains("UNRESOLVED_CICS_AID_SEMANTIC:") == true))
                    throw PyError.Value("UNRESOLVED_CICS_AID_SEMANTIC: unsupported standalone DFHAID comparison");
                throw PyError.Value("platform replacement projection or symbol usage is unsupported");
            }
            var staged = scope.Host(Subscript(binding, "outputPath"));
            var record = ReadJson(staged + ".rekt-source.json");
            var transformed = PyCompat.Resolve(PathArg(Subscript(record, "transformedPath")));
            if (scope.StagingRoot is null || staged != PyCompat.Resolve(Path.Join(scope.StagingRoot, name + ".cpy"))
                || transformed != PyCompat.Resolve(Path.Join(Parent(profilePath), name + ".cpy"))
                || PyCompat.Resolve(PathArg(Subscript(record, "originalSourcePath"))) != profilePath
                || DigestMismatch(Subscript(record, "originalSourceSha256"), profilePath)
                || DigestMismatch(Subscript(record, "stagedSha256"), staged)
                || DigestMismatch(Subscript(record, "transformedSha256"), transformed)
                || CurrentTransform(transformed, profilePath) is null
                || !PyCompat.ReadBytes(staged).AsSpan().SequenceEqual(PlatformCopybookContracts.Projection(Subscript(Subscript(profile, "copybooks"), name)))
                || !PyJson.PyEquals(PyCompat.Digest(transformed), Subscript(binding, "outputSha256")))
                throw PyError.Value("platform projection does not match the explicit symbolic recipe");
            AppendUnique(definitions, Definition("PLATFORM_COPY", name, profilePath, PyCompat.Digest(profilePath)));
            AppendUnique(definitions, Definition("PLATFORM_INPUT", name, transformed, PyCompat.Digest(transformed)));
        }
        catch (Exception error) when (Caught(error))
        {
            errors.Add($"native-platform-contract-unverified:{name}:{error.Message}");
        }
    }

    private static void ExternalRuntime(BindingScope scope, JsonObject node, string name, string relativePath, JsonArray definitions, List<string> errors)
    {
        try
        {
            var binding = Subscript(node, "runtimeContract");
            var path = scope.Host(Subscript(binding, "sourcePath"), profile: true);
            var profile = ReadBoundProfile(binding, path);
            if (!NativeRuntimeContractBound(node, profile))
                throw PyError.Value("invalid documented runtime-contract leaf");
            var contract = Subscript(binding, "definition");
            var additions = new List<JsonObject> { Definition("RUNTIME_CONTRACT", name, path, PyCompat.Digest(path)) };
            if (Str(contract, "kind") == "estate-assembler-routine")
            {
                var estate = Norm(relativePath).Split('/')[0];
                if (AssemblerRoutineProblem(name, contract, scope.SourceRoot, estate) is { Length: > 0 } problem)
                    throw PyError.Value(problem);
                var items = new List<JsonNode?> { contract };
                items.AddRange((JsonArray)Subscript(contract, "includes")!);
                foreach (var item in items)
                    additions.Add(Definition("ASSEMBLER_SOURCE", Has(item, "name") ? Get(item, "name") : name,
                        PyCompat.Resolve(Path.Join(scope.SourceRoot, PathArg(Subscript(item, "source")))), Subscript(item, "sourceSha256")));
            }
            foreach (var definition in additions) AppendUnique(definitions, definition);
        }
        catch (Exception error) when (Caught(error))
        {
            errors.Add($"native-runtime-contract-unverified:{name}:{error.Message}");
        }
    }

    private static void ResolvedDefinition(BindingScope scope, JsonNode document, List<JsonObject> owners, JsonObject node, string kind,
        string name, string? searchView, JsonNode? searchRecord, JsonArray definitions, List<string> errors)
    {
        try
        {
            var path = scope.Host(Subscript(node, "sourcePath"));
            if (DigestDiffers(path, node, "sourceSha256")) throw PyError.Value("native resolved input hash mismatch");
            if (Stub.IsMatch(PyCompat.ReadText(path, PyCompat.Latin1))) throw PyError.Value("placeholder-definition");
            string? original = null, transformed = null;
            if (scope.StagingRoot is { Length: > 0 } stagingRoot)
            {
                var record = ReadJson(path + ".rekt-source.json");
                original = PathArg(Subscript(record, "originalSourcePath"));
                if (kind == "CALL" && searchView is not null && IsRelativeTo(path, searchView))
                {
                    var stagedRelative = PathArg(Subscript(Subscript(searchRecord, "programs"), Path.GetFileName(path)));
                    var canonical = PyCompat.Resolve(Path.Join(stagingRoot, stagedRelative));
                    if (IsAbsolute(stagedRelative) || !IsRelativeTo(canonical, PyCompat.Resolve(stagingRoot))
                        || Parent(path) != Norm(PyCompat.Resolve(searchView))
                        || !PyJson.PyEquals(record, ReadJson(canonical + ".rekt-source.json"))
                        || PyCompat.Digest(canonical) != PyCompat.Digest(path)
                        || PyCompat.Resolve(original) != PyCompat.Resolve(Path.Join(scope.SourceRoot, stagedRelative)))
                        throw PyError.Value("callee search view provenance invalid");
                }
                if (DefinitionProblem(original) is { } originalProblem) throw PyError.Value(originalProblem);
                if (DigestMismatch(Subscript(record, "originalSourceSha256"), original) || DigestMismatch(Subscript(record, "stagedSha256"), path))
                    throw PyError.Value("staged definition provenance invalid");
                var transformedNode = Get(record, "transformedPath");
                if (PyJson.Truthy(transformedNode))
                {
                    transformed = PathArg(transformedNode);
                    if (CurrentTransform(transformed, original) is null
                        || DigestMismatch(Subscript(record, "transformedSha256"), transformed)
                        || PyCompat.Digest(transformed) != PyCompat.Digest(path))
                        throw PyError.Value("derived definition provenance invalid");
                    if (kind == "CALL")
                        definitions.Add(Definition("CALL", name, PyCompat.Resolve(transformed), PyCompat.Digest(transformed)));
                    else if (PyCompat.Suffix(original).ToLowerInvariant() == ".bms")
                    {
                        definitions.Add(Definition("BMS", PyCompat.Stem(name).ToUpperInvariant(), PyCompat.Resolve(original), PyCompat.Digest(original)));
                        original = transformed;
                    }
                    else
                        definitions.Add(Definition("COPY_INPUT", PyCompat.Stem(name).ToUpperInvariant(), PyCompat.Resolve(transformed), PyCompat.Digest(transformed)));
                }
                path = PyCompat.Resolve(original);
            }
            if (DefinitionProblem(path) is { } problem) throw PyError.Value(problem);
            var grounding = kind == "COPY" ? CurrentTransform(path) : null;
            var layoutOutput = path;
            if (kind == "COPY" && transformed is not null && PyCompat.Suffix(original!).ToLowerInvariant() != ".bms"
                && (grounding is null || PyCompat.Suffix(grounding).ToLowerInvariant() != ".bms"))
                (grounding, layoutOutput) = (BmsGrounding(transformed), PyCompat.Resolve(transformed));
            if (grounding is not null && PyCompat.Suffix(grounding).ToLowerInvariant() == ".bms")
            {
                var groundingPath = PyCompat.Resolve(grounding);
                if (!definitions.Any(item => PyJson.PyEquals(Subscript(item, "kind"), "BMS") && PyJson.PyEquals(Subscript(item, "path"), groundingPath)))
                    definitions.Add(Definition("BMS", PyCompat.Stem(name).ToUpperInvariant(), groundingPath, PyCompat.Digest(grounding)));
                BmsLayoutDefinition(grounding, layoutOutput, name, definitions, errors);
                var proof = layoutOutput + ".bms-layout.json";
                var nodes = new List<JsonObject> { (JsonObject)document };
                nodes.AddRange(ExportedNodes(document, Path.GetFileName(scope.Source)));
                var bound = false;
                foreach (var owner in nodes)
                {
                    foreach (var binding in Items(owner, "dependencyDerivedInputs"))
                    {
                        if (binding is not JsonObject) continue;
                        if (Str(binding, "kind") == "BMS_LAYOUT"
                            && PyJson.PyEquals(Get(binding, "outputPath"), Subscript(node, "sourcePath"))
                            && PyJson.PyEquals(Get(binding, "outputSha256"), Subscript(node, "sourceSha256"))
                            && PyJson.PyEquals(Get(binding, "layoutSha256"), File.Exists(proof) ? PyCompat.Digest(proof) : null))
                        {
                            bound = true;
                            break;
                        }
                    }
                    if (bound) break;
                }
                if (!bound) errors.Add($"native-bms-layout-binding-missing:{name}");
            }
            definitions.Add(Definition(kind, kind == "COPY" ? PyCompat.Stem(name).ToUpperInvariant() : name, path, PyCompat.Digest(path)));
        }
        catch (Exception error) when (Caught(error))
        {
            errors.Add($"native-resolved-definition-unverified:{name}:{error.Message}");
        }
    }

    private static void BmsAttributeContract(BindingScope scope, JsonNode document, List<JsonObject> owners, JsonObject owner,
        JsonArray definitions, List<string> errors)
    {
        try
        {
            if (!PlatformCopybookContracts.BmsLayoutsBound(Subscript(owner, "runtimeContract"), definitions.ToList()))
                throw PyError.Value("attribute assignments lack hash-bound BMS field layouts");
            var parent = owners.FirstOrDefault(program => Items(program, "dependencies").Any(child => ReferenceEquals(child, owner)))
                ?? throw new InvalidOperationException("StopIteration");
            var original = scope.Source;
            if (!ReferenceEquals(parent, document))
            {
                var selected = scope.HostPath(Subscript(parent, "sourcePath"));
                original = scope.StagingRoot is { Length: > 0 }
                    ? PathArg(Subscript(ReadJson(selected + ".rekt-source.json"), "originalSourcePath"))
                    : CurrentTransform(selected) ?? selected;
            }
            if (!PlatformCopybookContracts.BmsSourceUsesBound(Subscript(owner, "runtimeContract"), PyCompat.ReadText(original, PyCompat.Latin1), definitions.ToList()))
                throw PyError.Value("BMS attribute uses omit or differ from the original source");
        }
        catch (Exception error) when (Caught(error))
        {
            errors.Add($"native-platform-contract-unverified:DFHBMSCA:{error.Message}");
        }
    }
}
