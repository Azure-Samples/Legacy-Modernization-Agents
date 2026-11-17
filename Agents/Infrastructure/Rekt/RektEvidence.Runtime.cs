using System.Text.Json.Nodes;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Rekt;

public static partial class RektEvidence
{
    internal static readonly IReadOnlyDictionary<string, string> SqlReferenceRules = new Dictionary<string, string>
    {
        ["Dbs_table_nameContext"] = "SQL_RELATION", ["Dbs_view_nameContext"] = "SQL_RELATION",
        ["Dbs_cursor_nameContext"] = "SQL_CURSOR", ["Dbs_host_var_identifierContext"] = "SQL_HOST_VARIABLE",
        ["Dbs_host_indicator_identifierContext"] = "SQL_HOST_VARIABLE",
        ["Dbs_host_variable_arrayContext"] = "SQL_HOST_VARIABLE", ["Dbs_sequence_nameContext"] = "SQL_SEQUENCE",
        ["Dbs_statement_nameContext"] = "SQL_STATEMENT", ["Dbs_distinct_type_nameContext"] = "SQL_TYPE",
        ["Dbs_function_nameContext"] = "SQL_FUNCTION",
    };

    private const string LanguageEnvironment = "IBM z/OS Language Environment";
    private const string LeServices = "https://www.ibm.com/docs/en/zos/3.1.0?topic=services-";

    // Name -> (platform, required documentation reference, declared parameter count).
    internal static readonly IReadOnlyDictionary<string, (string Platform, string Reference, int Count)> RuntimeDocumentation =
        new Dictionary<string, (string, string, int)>
        {
            ["CEEGMT"] = (LanguageEnvironment, LeServices + "ceegmtget-current-greenwich-mean-time", 3),
            ["CEEDATM"] = (LanguageEnvironment, LeServices + "ceedatmconvert-seconds-character-timestamp", 4),
            ["CEEDAYS"] = (LanguageEnvironment, LeServices + "ceedaysconvert-date-lilian-format", 4),
            ["CEELOCT"] = (LanguageEnvironment, LeServices + "ceeloctget-current-local-date-time", 4),
            ["CEE3ABD"] = (LanguageEnvironment, LeServices + "cee3abdterminate-enclave-abend", 2),
            // DB PCB form: function, PCB, I/O area and the optional repeating SSA.
            ["CBLTDLI"] = ("IBM IMS", "https://www.ibm.com/docs/en/SSEPH2_15.4.0/com.ibm.ims154.doc.apg/ims_imsdbcobolapp.htm", 4),
            // CICS form of DSNTIAR: eib, commarea, sqlca, message, lrecl.
            ["DSNTIAC"] = ("IBM Db2 for z/OS", "https://www.ibm.com/docs/en/db2-for-zos/13.0.0?topic=statements-handling-sql-error-codes-in-cobol-applications", 5),
        };

    // Documented z/OS system macros an estate assembler routine may invoke.
    internal static readonly IReadOnlyDictionary<string, string> AssemblerServices = new Dictionary<string, string>
    {
        ["STIMER"] = "https://www.ibm.com/docs/en/SSLTBW_3.1.0/com.ibm.zos.v3r1.ieaa900/stimer.htm",
    };

    // Machine instructions and HLASM directives whose meaning needs no further source.
    internal static readonly HashSet<string> HlasmOperations =
    [
        "START", "CSECT", "DSECT", "USING", "DROP", "EQU", "DS", "DC", "LTORG", "END", "COPY", "MACRO", "MEND",
        "STM", "LM", "L", "ST", "LA", "LR", "SR", "BR", "B", "BE", "BNE", "MVI", "MVC", "CLI", "CLC",
    ];

    internal static IEnumerable<string[]> AssemblerStatements(string text)
    {
        foreach (var raw in PyCompat.SplitLines(text))
        {
            var line = PyCompat.Slice(raw, 0, 71).TrimEnd();
            if (line.Trim().Length == 0 || line.StartsWith('*') || line.StartsWith(".*")) continue;
            var fields = line.Split((char[]?)null, StringSplitOptions.RemoveEmptyEntries);
            yield return line[0] != ' ' ? fields : ["", .. fields];
        }
    }

    private static bool ParametersValid(JsonNode? parameters) =>
        parameters is JsonArray items && items.All(item => item is JsonObject
            && Str(item, "passing") == "BY REFERENCE" && Str(item, "direction") is "in" or "out" or "inout"
            && PyJson.Truthy(Get(item, "name")) && PyJson.Truthy(Get(item, "representation")));

    // Why an estate assembler routine is not a closed, hash-bound leaf; null when it is.
    public static string? AssemblerRoutineProblem(string name, JsonNode? definition, string? sourceRoot = null, string? estate = null)
    {
        const string malformed = "assembler routine contract is malformed";
        try
        {
            if (definition is not JsonObject || !Has(definition, "includes") || !Has(definition, "systemServices")
                || !Has(definition, "parameters"))
                return malformed;
            var includes = Get(definition, "includes") as JsonArray;
            var services = Get(definition, "systemServices") as JsonArray;
            var parameters = Get(definition, "parameters") as JsonArray;
            if (Str(definition, "kind") != "estate-assembler-routine"
                || Str(definition, "language") != "IBM High Level Assembler" || !StrEquals(Get(definition, "entry"), name)
                || includes is null || services is null || parameters is null || parameters.Count == 0
                || !ParametersValid(parameters)
                || Obj(definition, "semantics") is not { Count: > 0 }
                || !includes.All(item => item is JsonObject && Str(item, "kind") is "macro" or "copy" && PyJson.IsStr(Get(item, "name")))
                || includes.Select(item => Str(item, "name")).Distinct().Count() != includes.Count
                || !services.All(item => item is JsonObject && Str(item, "name") is { } service
                    && AssemblerServices.TryGetValue(service, out var reference)
                    && PyJson.PyEquals(Get(item, "documentation"), ToArray([reference]))))
                return malformed;
            var files = new List<(JsonNode? Relative, JsonNode? Sha)>();
            foreach (var item in new[] { definition }.Concat(includes))
            {
                if (!Has(item, "source") || !Has(item, "sourceSha256")) return malformed;
                files.Add((Get(item, "source"), Get(item, "sourceSha256")));
            }
            foreach (var (relativeNode, shaNode) in files)
            {
                var relative = PyJson.Str(relativeNode);
                if (relative is null || relative.StartsWith('/') || Parts(relative).Contains("..")
                    || !PyJson.IsStr(shaNode) || !IsHex64(shaNode))
                    return $"assembler source {PyJson.PyStr(relativeNode)} is not an estate-relative hash-bound path";
                if (estate is not null)
                {
                    var parts = Parts(relative);
                    if (parts.Length == 0) return malformed;
                    if (parts[0] != estate) return $"assembler source {relative} is not an estate-relative hash-bound path";
                }
            }
            if (sourceRoot is null) return null;
            var texts = new List<string>();
            var root = PyCompat.Resolve(sourceRoot);
            foreach (var (relativeNode, shaNode) in files)
            {
                var path = PyCompat.Resolve(Path.Join(sourceRoot, PyJson.Str(relativeNode)));
                if (!IsRelativeTo(path, root)) return $"assembler source {PyJson.Str(relativeNode)} differs from its contract hash";
                if (!File.Exists(path)) return malformed;
                if (PyCompat.Digest(path) != PyJson.Str(shaNode))
                    return $"assembler source {PyJson.Str(relativeNode)} differs from its contract hash";
                texts.Add(PyCompat.ReadText(path, PyCompat.Latin1));
            }
            var statements = AssemblerStatements(texts[0]).ToList();
            if (!statements.Any(fields => fields[0] == name && fields.Length > 1 && fields[1] is "START" or "CSECT"))
                return $"assembler source does not define entry {name}";
            if (statements.Any(fields => fields.Length < 2)) return malformed;
            var used = statements.Select(fields => fields[1]).ToHashSet(StringComparer.Ordinal);
            foreach (var (item, text) in includes.Zip(texts.Skip(1)))
            {
                var body = AssemblerStatements(text).ToList();
                if (Str(item, "kind") == "macro")
                {
                    if (body.Count >= 3 && (body[1].Length < 2 || body[^1].Length < 2)) return malformed;
                    if (body.Count < 3 || !body[0].Skip(1).SequenceEqual(["MACRO"]) || body[1][1] != Str(item, "name")
                        || body[^1][1] != "MEND")
                        return $"macro {Str(item, "name")} does not define its prototype";
                    body = body[2..^1];
                }
                if (body.Any(fields => fields.Length < 2)) return malformed;
                used.UnionWith(body.Select(fields => fields[1]));
                statements.AddRange(body);
            }
            var copies = statements.Where(fields => fields[1] == "COPY" && fields.Length > 2).Select(fields => fields[2]).ToHashSet();
            if (!copies.SetEquals(includes.Where(item => Str(item, "kind") == "copy").Select(item => Str(item, "name")!)))
                return "assembler COPY members differ from the declared includes";
            var declared = includes.Where(item => Str(item, "kind") == "macro").Select(item => Str(item, "name")!)
                .Concat(services.Select(item => Str(item, "name")!)).ToHashSet(StringComparer.Ordinal);
            var outside = used.Except(HlasmOperations).ToHashSet(StringComparer.Ordinal);
            if (!outside.SetEquals(declared))
            {
                outside.SymmetricExceptWith(declared);
                return "assembler operations outside the closed contract: ["
                    + string.Join(", ", outside.Order(StringComparer.Ordinal).Select(PyJson.ReprString)) + "]";
            }
            return null;
        }
        catch (Exception ex) when (ex is IOException or UnauthorizedAccessException or PyError)
        {
            return malformed;
        }
    }

    internal static bool NativeProfileReference(JsonNode? binding) =>
        binding is JsonObject && Str(binding, "profileKind") == "target-port"
        && !string.IsNullOrEmpty(Str(binding, "profileName"))
        && IsAbsolute(Str(binding, "sourcePath"))
        && PyJson.IsStr(Get(binding, "sourceSha256")) && IsHex64(Get(binding, "sourceSha256"));

    internal static JsonNode ReadBoundProfile(JsonNode? binding, string path)
    {
        var profile = ReadJson(path);
        if (profile is not JsonObject || !NativeProfileReference(binding) || PyCompat.Digest(path) != Str(binding, "sourceSha256")
            || !PyJson.PyEquals(Get(profile, "schemaVersion"), 1)
            || !PyJson.PyEquals(Get(profile, "profileKind"), Get(binding, "profileKind"))
            || !PyJson.PyEquals(Get(profile, "profileName"), Get(binding, "profileName")))
            throw PyError.Value("configured profile identity/hash mismatch");
        return profile;
    }

    private static readonly string[] PlatformOperators = ["EIBAID", "=", "NOT", "AND", "OR", "IS", "EQUAL", "TO", "(", ")"];

    private static List<string> PlatformSyntaxTokens(JsonNode? syntax, string? parent = null)
    {
        if (syntax is not JsonObject)
            throw PyError.Value("invalid native platform syntax");
        if (KeysAre(syntax, "token") && Str(syntax, "token") is { } token)
            return [token.ToUpperInvariant()];
        if (!KeysAre(syntax, "rule", "children") || Get(syntax, "children") is not JsonArray children)
            throw PyError.Value("invalid native platform grammar");
        var rule = Str(syntax, "rule");
        var tokens = children.SelectMany(child => PlatformSyntaxTokens(child, rule)).ToList();
        if (rule == "SimpleConditionContext")
        {
            if (tokens.Count == 1)
            {
                if (parent != "AdditionalConditionContext" || !PlatformCopybookContracts.Symbols.ContainsKey(tokens[0]))
                    throw PyError.Value("unsupported event predicate");
            }
            else if (tokens.Count < 3 || tokens[0] != "EIBAID" || !PlatformCopybookContracts.Symbols.ContainsKey(tokens[^1]))
                throw PyError.Value("event equality must compare EIBAID to a named event");
        }
        return tokens;
    }

    private static List<string> PlatformFlatTokens(JsonNode? syntax)
    {
        if (KeysAre(syntax, "token") && Str(syntax, "token") is { } token)
            return [token.ToUpperInvariant()];
        if (!KeysAre(syntax, "rule", "children") || Get(syntax, "children") is not JsonArray children)
            throw PyError.Value("invalid native platform grammar");
        return children.SelectMany(PlatformFlatTokens).ToList();
    }

    internal static bool NativePlatformContractBound(JsonNode? node, JsonNode? profile)
    {
        try
        {
            PlatformCopybookContracts.Validate(profile);
            var binding = Get(node, "runtimeContract");
            if (binding is not JsonObject || !Has(binding, "name") || Str(binding, "name") is not { } name
                || Obj(profile, "copybooks") is not { } copybooks || !copybooks.TryGetPropertyValue(name, out var contract))
                return false;
            var target = Get(node, "callTarget");
            if (Has(node, "callTarget") && target is not JsonObject) return false;
            if (Str(node, "dependencyKind") != "PLATFORM_COPY"
                || !PyJson.IsTrue(Get(node, "runtimeContractBindingComplete"))
                || !IsFalse(Get(node, "dependencyAnalysisComplete"))
                || !StrEquals(Get(node, "sourcePath"), null) || !StrEquals(Get(node, "sourceSha256"), null)
                || Get(node, "dependencies") is not JsonArray { Count: 0 }
                || Str(target, "name") != name
                || Str(target, "programReferenceType") != "STATIC"
                || Str(binding, "profileKind") != "platform-replacement"
                || !PyJson.PyEquals(Get(binding, "profileName"), Get(profile, "profileName"))
                || !PyJson.PyEquals(Get(binding, "definition"), contract)
                || Str(binding, "bindingStatus") != "complete"
                || Str(binding, "resolutionKind") != "symbolic-platform-replacement"
                || Str(binding, "runtimeMapping") != "implementation-required"
                || Str(binding, "sourceValues") != "not-supplied"
                || Str(binding, "outputSha256") != PyCompat.Digest(PlatformCopybookContracts.Projection(contract))
                || Get(binding, "usages") is not JsonArray usages)
                return false;
            if (name == "DFHBMSCA")
                return usages.All(PlatformCopybookContracts.BmsUsageBound);
            var symbols = PlatformCopybookContracts.Symbols;
            var allowed = symbols.Keys.Concat(PlatformOperators).ToHashSet(StringComparer.Ordinal);
            string? previous = null;
            for (var position = 0; position < usages.Count; position++)
            {
                var usage = usages[position];
                if (usage is not JsonObject) return false;
                var role = Str(usage, "conditionRole");
                var syntax = Get(usage, "syntax");
                if (!Has(usage, "syntax")) return false;
                var line = IsInt(Get(usage, "line"), out var number) ? number : (long?)null;
                var sourceText = Str(usage, "sourceText");
                // EVALUATE EIBAID: one subject usage followed by one usage per WHEN <named event>.
                if (role is "evaluate-subject" or "evaluate-when")
                {
                    var evaluateRule = role == "evaluate-subject" ? "EvaluateSelectContext" : "EvaluateWhenContext";
                    var flat = PlatformFlatTokens(syntax);
                    string? following = null;
                    if (position + 1 < usages.Count)
                    {
                        if (usages[position + 1] is not JsonObject) return false;
                        following = Str(usages[position + 1], "conditionRole");
                    }
                    if (Str(usage, "grammarRule") != evaluateRule || Str(syntax, "rule") != evaluateRule
                        || Str(usage, "coordinateSpace") != "expanded-parser-input"
                        || line is null or < 1 || sourceText is null || sourceText.Trim().Length == 0
                        || role == "evaluate-subject" && (!flat.SequenceEqual(["EIBAID"]) || following != "evaluate-when")
                        || role == "evaluate-when" && (flat.Count != 2 || flat[0] != "WHEN" || !symbols.ContainsKey(flat[1])
                                                       || previous is not ("evaluate-subject" or "evaluate-when")))
                        return false;
                    previous = role;
                    continue;
                }
                previous = role;
                var tokens = PlatformSyntaxTokens(syntax);
                // An EIBAID equality ANDed with application relations is recorded as its own conjunct.
                var rule = role == "application-and-conjunct" ? "SimpleConditionContext" : "ConditionContext";
                var set = tokens.ToHashSet(StringComparer.Ordinal);
                if (Str(usage, "grammarRule") != rule
                    || Str(syntax, "rule") != rule
                    || rule == "SimpleConditionContext"
                    && (tokens.Count == 0 || tokens[0] != "EIBAID" || tokens.Count(symbols.ContainsKey) != 1)
                    || Str(usage, "coordinateSpace") != "expanded-parser-input"
                    || line is null or < 1 || sourceText is null || sourceText.Trim().Length == 0
                    || !set.IsSubsetOf(allowed) || !set.Contains("EIBAID")
                    || !set.Contains("=") && !set.Contains("EQUAL") || !set.Any(symbols.ContainsKey))
                    return false;
            }
            return true;
        }
        catch (PyError)
        {
            return false;
        }
    }

    private static bool IsFalse(JsonNode? node) =>
        node is JsonValue value && value.GetValueKind() == System.Text.Json.JsonValueKind.False;

    private static readonly HashSet<string> CallOperandFollowers =
        ["USING", "RETURNING", "ON", "NOT", "EXCEPTION", "OVERFLOW", "END-CALL", "."];

    // CALL identifier proved by the native parser to hold one never-modified program-name constant.
    internal static bool DynamicCallResolutionBound(JsonNode? resolution, string? name)
    {
        if (!KeysAre(resolution, "operand", "targetResolution", "callSyntax")) return false;
        var operand = Str(resolution, "operand");
        var syntax = Get(resolution, "callSyntax");
        if (operand is null || Full("[A-Z][A-Z0-9-]*", operand) is null
            || syntax is not JsonObject || Str(syntax, "rule") != "CallStatementContext")
            return false;
        var tokens = Tokens(SyntaxNodes(syntax)).Select(token => token.ToUpperInvariant()).ToList();
        if (tokens.Count < 2 || tokens[0] != "CALL" || tokens[1] != operand
            || tokens.Count > 2 && !CallOperandFollowers.Contains(tokens[2]))
            return false;
        var synthetic = new JsonObject
        {
            ["rule"] = "Cics_nameContext",
            ["children"] = new JsonArray(new JsonObject { ["token"] = "(" }, new JsonObject { ["token"] = operand }, new JsonObject { ["token"] = ")" }),
        };
        var value = CicsConstantOperand(synthetic, Get(resolution, "targetResolution"), 1, 8);
        return value is not null && value.ToUpperInvariant() == name;
    }

    internal static bool NativeRuntimeContractBound(JsonNode? node, JsonNode? profile = null)
    {
        var target = Has(node, "callTarget") ? Get(node, "callTarget") : new JsonObject();
        var binding = Get(node, "runtimeContract");
        if (target is not JsonObject || Str(node, "dependencyKind") != "EXTERNAL_RUNTIME"
            || !PyJson.IsTrue(Get(node, "runtimeContractBindingComplete"))
            || !IsFalse(Get(node, "dependencyAnalysisComplete"))
            || !StrEquals(Get(node, "sourcePath"), null) || !StrEquals(Get(node, "sourceSha256"), null)
            || Str(target, "programReferenceType") != "STATIC"
            || PyJson.Truthy(Get(node, "dependencies")) || PyJson.Truthy(Get(node, "dependencyDiagnostics"))
            || !NativeProfileReference(binding)
            || !PyJson.PyEquals(Get(binding, "name"), Get(target, "name"))
            || Str(binding, "resolutionKind") != "documented-platform-contract"
            || Str(binding, "runtimeMapping") != "unverified"
            || Has(binding, "targetResolution") && !DynamicCallResolutionBound(Get(binding, "targetResolution"), Str(binding, "name")))
            return false;
        var name = Str(binding, "name") ?? "";
        var known = RuntimeDocumentation.TryGetValue(name, out var expected);
        var definition = Get(binding, "definition");
        if (definition is not JsonObject) return false;
        var profiled = profile is null || PyJson.PyEquals(Get(Get(profile, "runtimeContracts"), name), definition);
        if (Str(definition, "kind") == "estate-assembler-routine")
            return !known && AssemblerRoutineProblem(name, definition) is null && profiled;
        if (!known) return false;
        return Str(definition, "kind") == "documented-platform-service"
            && Str(definition, "platform") == expected.Platform
            && Get(definition, "documentation") is JsonArray references && references.Any(reference => PyJson.Str(reference) == expected.Reference)
            && Get(definition, "parameters") is JsonArray parameters && parameters.Count == expected.Count
            && ParametersValid(parameters)
            && Get(definition, "semantics") is JsonObject
            && profiled;
    }
}
