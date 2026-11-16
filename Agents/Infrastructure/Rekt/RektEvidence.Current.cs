using System.Text.Json.Nodes;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Rekt;

// Re-proves a written dependency-evidence.json against the current sources, export and definitions.
public static partial class RektEvidence
{
    private static readonly string[] CicsResourceKinds = ["CICS_FILE", "CICS_PROGRAM", "CICS_TRANSACTION", "CICS_TDQUEUE"];

    public static bool EvidenceCurrent(string sourceRoot, string relativePath, string outputRoot)
    {
        try
        {
            return EvidenceCurrentOrThrow(sourceRoot, relativePath, outputRoot);
        }
        catch (Exception error) when ((Caught(error) && error is not PyError { Kind: "AttributeError" }) || error is Rejected)
        {
            return false;
        }
    }

    // {item["path"]: item for item in items}.values(): one entry per path, first position, last value.
    private static List<JsonNode?> ByPath(IEnumerable<JsonNode?> items)
    {
        var order = new List<string>();
        var latest = new Dictionary<string, JsonNode?>();
        foreach (var item in items)
        {
            var key = PyKey(Subscript(item, "path"));
            if (!latest.ContainsKey(key)) order.Add(key);
            latest[key] = item;
        }
        return order.Select(key => latest[key]).ToList();
    }

    private static bool EvidenceCurrentOrThrow(string sourceRoot, string relativePath, string outputRoot)
    {
        var source = PyCompat.Resolve(Path.Join(sourceRoot, relativePath));
        var evidence = ReadJson(Path.Join(outputRoot, relativePath + ".report", "dependency-evidence.json"));
        if (!PyJson.PyEquals(Subscript(evidence, "schemaVersion"), 1) || !StrEquals(Subscript(evidence, "producer"), Producer)
            || !StrEquals(Subscript(evidence, "relativePath"), relativePath)
            || PyCompat.Resolve(PathArg(Subscript(evidence, "originalSourcePath"))) != source
            || DigestMismatch(Subscript(evidence, "originalSourceSha256"), source)
            || !PyJson.PyEquals(Subscript(evidence, "exportExitCode"), 0)
            || PyCompat.Resolve(PathArg(Subscript(evidence, "exportPath"))) != PyCompat.Resolve(Path.Join(outputRoot, relativePath + "-deps.json"))
            || DigestMismatch(Subscript(evidence, "exportSha256"), PathArg(Subscript(evidence, "exportPath")))
            || PyJson.Truthy(Subscript(evidence, "placeholderContaminated")) || PyJson.Truthy(Subscript(evidence, "unresolvedDiagnostics"))
            || Kinds.Any(kind => Str(Subscript(evidence, "coverage"), kind) != "complete"))
            return false;
        if (ContaminatedReport(Path.Join(outputRoot, relativePath + ".report"))) return false;
        var definitions = ((JsonArray)Subscript(evidence, "definitions")!).ToList();
        foreach (var definition in definitions)
        {
            if (StrEquals(Subscript(definition, "kind"), "COMMAREA_CLOSURE"))
            {
                // Every participant must still match: recompute from current sources and exports.
                var current = CommareaClosure(sourceRoot, outputRoot, PyJson.Str(Subscript(definition, "name"))!);
                var path = PathArg(Subscript(definition, "path"));
                if (DigestMismatch(Subscript(definition, "sha256"), path) || !PyJson.PyEquals(ReadJson(path), current))
                    return false;
                continue;
            }
            if (DigestMismatch(Subscript(definition, "sha256"), PathArg(Subscript(definition, "path")))
                || DefinitionProblem(PathArg(Subscript(definition, "path"))) is not null)
                return false;
        }
        // Recheck original references; old "complete" empty COPY exports are invalid.
        var document = ReadJson(PathArg(Subscript(evidence, "exportPath")));
        // A non-object export escapes as an error so the activity journal reports it as unverified, not incomplete.
        if (document is not JsonObject)
            throw new PyError("AttributeError", "export is not a JSON object");
        if (!PyJson.PyEquals(Get(document, "dependencyExportSchemaVersion"), 1)
            || CommareaClosureBindings(document!, outputRoot, relativePath).Problem is not null
            || !PyJson.PyEquals(Get(document, "dependencyCoverage"), Subscript(evidence, "coverage"))
            || MissingNativeOperations(source, document).Count > 0)
            return false;
        var sourceName = Path.GetFileName(source);
        var nodes = new List<JsonObject> { (JsonObject)document! };
        nodes.AddRange(ExportedNodes(document, sourceName));
        IEnumerable<JsonNode?> Named(string kind, JsonNode? name) =>
            definitions.Where(item => StrEquals(Subscript(item, "kind"), kind) && PyJson.PyEquals(Subscript(item, "name"), name));
        foreach (var definition in definitions)
        {
            if (!StrEquals(Subscript(definition, "kind"), "BMS")) continue;
            var name = Subscript(definition, "name");
            var copies = ByPath(Named("COPY_INPUT", name));
            if (copies.Count == 0) copies = ByPath(Named("COPY", name));
            var layouts = ByPath(Named("BMS_LAYOUT", name));
            if (copies.Count != 1 || layouts.Count != 1) return false;
            var failures = new List<string>();
            BmsLayoutDefinition(PathArg(Subscript(definition, "path")), PathArg(Subscript(copies[0], "path")),
                PyJson.PyStr(name), new JsonArray(definitions.Select(item => item?.DeepClone()).ToArray()), failures);
            if (failures.Count > 0 || !nodes.Any(owner => Items(owner, "dependencyDerivedInputs").Any(binding => Str(binding, "kind") == "BMS_LAYOUT"
                    && PyJson.PyEquals(Get(binding, "sourceSha256"), Subscript(definition, "sha256"))
                    && PyJson.PyEquals(Get(binding, "outputSha256"), Subscript(copies[0], "sha256"))
                    && PyJson.PyEquals(Get(binding, "layoutSha256"), Subscript(layouts[0], "sha256")))))
                return false;
        }
        foreach (var node in nodes)
        {
            if (!ReferenceEquals(node, document) && Str(node, "dependencyKind") == "COPY") continue;
            if (Str(node, "dependencyKind") == "PLATFORM_COPY")
            {
                if (!PlatformCopyCurrent(node, nodes, document!, source, definitions)) return false;
                continue;
            }
            if (Str(node, "dependencyKind") == "EXTERNAL_RUNTIME")
            {
                if (!RuntimeContractCurrent(node, definitions)) return false;
                continue;
            }
            var coverage = GetOr(node, "dependencyCoverage", new JsonObject());
            if (!PyJson.IsTrue(Get(node, "dependencyAnalysisComplete")) || PyJson.Truthy(Get(node, "dependencyDiagnostics"))
                || coverage is not JsonObject || Kinds.Any(kind => Str(coverage, kind) != "complete")
                || AbsenceProofs.Keys.Any(kind => !NativeDomainEvidence(node, kind)))
                return false;
            foreach (var statement in Items(node, "dependencyDialectStatements"))
                foreach (var binding in NativeCicsResources(statement))
                    if (!ResourceCurrent(node, statement, binding, definitions, sourceRoot)) return false;
        }
        var (exportedCopies, _) = ExportedNames(document, sourceName);
        if (RektSourceIdentity.ReferencedNames(PyCompat.ReadText(source, PyCompat.Latin1)).Except(exportedCopies).Any()) return false;
        var defined = definitions.Where(item => Str(item, "kind") is "COPY" or "PLATFORM_COPY")
            .Select(item => PyJson.Str(Subscript(item, "name"))!.ToUpperInvariant()).ToHashSet();
        return exportedCopies.IsSubsetOf(defined);
    }

    private static bool PlatformCopyCurrent(JsonObject node, List<JsonObject> nodes, JsonNode document, string source, List<JsonNode?> definitions)
    {
        var binding = GetOr(node, "runtimeContract", new JsonObject());
        var profiles = definitions.Where(item => StrEquals(Subscript(item, "kind"), "PLATFORM_COPY")
            && PyJson.PyEquals(Subscript(item, "name"), Get(binding, "name")) && PyJson.PyEquals(Subscript(item, "sha256"), Get(binding, "sourceSha256"))).ToList();
        var inputs = definitions.Where(item => StrEquals(Subscript(item, "kind"), "PLATFORM_INPUT")
            && PyJson.PyEquals(Subscript(item, "name"), Get(binding, "name")) && PyJson.PyEquals(Subscript(item, "sha256"), Get(binding, "outputSha256"))).ToList();
        if (profiles.Count != 1 || inputs.Count != 1 || !NativePlatformContractBound(node, ReadJson(PathArg(Subscript(profiles[0], "path")))))
            return false;
        if (Str(binding, "name") != "DFHBMSCA") return true;
        if (!PlatformCopybookContracts.BmsLayoutsBound(binding, definitions)) return false;
        var parent = nodes.First(owner => Items(owner, "dependencies").Any(child => ReferenceEquals(child, node)));
        var original = source;
        if (!ReferenceEquals(parent, document))
        {
            var originals = definitions.Where(item => StrEquals(Subscript(item, "kind"), "CALL")
                    && PyJson.PyEquals(Subscript(item, "name"), Subscript(Subscript(parent, "callTarget"), "name"))
                    && !Parts(PathArg(Subscript(item, "path"))).Contains(".preprocessed"))
                .Select(item => PyKey(Subscript(item, "path"))).ToList();
            if (originals.Distinct().Count() != 1) return false;
            original = PathArg(Subscript(definitions.First(item => StrEquals(Subscript(item, "kind"), "CALL")
                && PyJson.PyEquals(Subscript(item, "name"), Subscript(Subscript(parent, "callTarget"), "name"))
                && !Parts(PathArg(Subscript(item, "path"))).Contains(".preprocessed")), "path"));
        }
        return PlatformCopybookContracts.BmsSourceUsesBound(binding, PyCompat.ReadText(original, PyCompat.Latin1), definitions);
    }

    private static bool RuntimeContractCurrent(JsonObject node, List<JsonNode?> definitions)
    {
        var binding = GetOr(node, "runtimeContract", new JsonObject());
        var matches = definitions.Where(definition => StrEquals(Subscript(definition, "kind"), "RUNTIME_CONTRACT")
            && PyJson.PyEquals(Subscript(definition, "name"), Get(binding, "name"))
            && PyJson.PyEquals(Subscript(definition, "sha256"), Get(binding, "sourceSha256"))).ToList();
        if (matches.Count != 1 || !NativeRuntimeContractBound(node, ReadBoundProfile(binding, PathArg(Subscript(matches[0], "path")))))
            return false;
        var contract = Subscript(binding, "definition");
        if (Str(contract, "kind") != "estate-assembler-routine") return true;
        var sources = new List<JsonNode?> { contract };
        sources.AddRange((JsonArray)Subscript(contract, "includes")!);
        return sources.All(item => definitions.Any(definition => StrEquals(Subscript(definition, "kind"), "ASSEMBLER_SOURCE")
            && PyJson.PyEquals(Subscript(definition, "sha256"), Subscript(item, "sourceSha256"))
            && PathArg(Subscript(definition, "path")).Replace('\\', '/').EndsWith("/" + PyJson.Str(Subscript(item, "source")), StringComparison.Ordinal)
            && PyJson.PyEquals(PyCompat.Digest(PathArg(Subscript(definition, "path"))), Subscript(definition, "sha256"))));
    }

    private static bool ResourceCurrent(JsonObject node, JsonNode? statement, JsonNode? binding, List<JsonNode?> definitions, string sourceRoot)
    {
        if (Str(binding, "kind") == "CICS_MAP")
            return CicsMapEvidenceCurrent(binding, definitions.Cast<JsonObject>().ToList(), sourceRoot);
        if (Str(binding, "definitionScope") == "explicit-sql-catalog")
        {
            var declarations = definitions.Where(item => StrEquals(Subscript(item, "kind"), "SQL_DECLARATION")
                && PyJson.PyEquals(Subscript(item, "name"), Get(binding, "name"))
                && PyJson.PyEquals(Subscript(item, "sha256"), Get(binding, "sourceSha256"))).ToList();
            if (declarations.Count != 1 || !IsRelativeTo(PyCompat.Resolve(PathArg(Subscript(declarations[0], "path"))), PyCompat.Resolve(sourceRoot)))
                return false;
        }
        if (Has(binding, "qualification"))
        {
            var qualification = Get(binding, "qualification");
            var profiles = definitions.Where(definition => StrEquals(Subscript(definition, "kind"), "ANALYSIS_PROFILE")
                && PyJson.PyEquals(Subscript(definition, "name"), Get(qualification, "profileName"))
                && PyJson.PyEquals(Subscript(definition, "sha256"), Get(qualification, "sourceSha256"))).ToList();
            if (profiles.Count != 1
                || !PyJson.PyEquals(Get(ReadBoundProfile(qualification, PathArg(Subscript(profiles[0], "path"))), "sqlDefaultSchema"), Get(qualification, "schema")))
                return false;
        }
        if (!CicsResourceKinds.Contains(Str(binding, "kind"))) return true;
        var kind = Subscript(binding, "kind");
        if (!definitions.Any(definition => PyJson.PyEquals(Subscript(definition, "kind"), kind)
                && PyJson.PyEquals(Subscript(definition, "name"), Get(binding, "name"))
                && PyJson.PyEquals(Subscript(definition, "sha256"), Get(binding, "sourceSha256"))))
            return false;
        if (StrEquals(kind, "CICS_FILE") && !Items(binding, "datasetDefinitions").All(dataset => definitions.Any(definition =>
                StrEquals(Subscript(definition, "kind"), "CICS_DATASET") && PyJson.PyEquals(Subscript(definition, "name"), Get(dataset, "name"))
                && PyJson.PyEquals(Subscript(definition, "sha256"), Get(dataset, "sourceSha256")))))
            return false;
        var transfer = ContinuationRules.Contains(Str(statement, "grammarRule"));
        return !StrEquals(kind, "CICS_PROGRAM") || (definitions.Any(definition =>
                StrEquals(Subscript(definition, "kind"), transfer ? "CICS_CONTINUATION" : "CALL")
                && PyJson.PyEquals(Subscript(definition, "name"), Get(binding, "name"))
                && PyJson.PyEquals(Subscript(definition, "sha256"), Get(binding, "programSourceSha256")))
            && (transfer || Items(node, "dependencies").Any(child => Str(child, "dependencyKind") is "CALL" or "CICS_LINK" or "CICS_RUN"
                && PyJson.PyEquals(Get(GetOr(child, "callTarget", new JsonObject()), "name"), Get(binding, "name"))
                && PyJson.PyEquals(Get(child, "sourcePath"), Get(binding, "programSourcePath"))
                && PyJson.PyEquals(Get(child, "sourceSha256"), Get(binding, "programSourceSha256")))));
    }
}
