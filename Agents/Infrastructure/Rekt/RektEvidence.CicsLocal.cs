using System.Text.Json.Nodes;
using System.Text.RegularExpressions;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Rekt;

public static partial class RektEvidence
{
    private static readonly JsonObject[] CicsProgramAttributes =
    [
        new() { ["language"] = "COBOL", ["dynamic"] = "NO" },
        // Omitted DYNAMIC is IBM CICS DYNAMIC(NO); the exporter records that provenance.
        new() { ["language"] = "COBOL", ["dynamic"] = "NO", ["dynamicProvenance"] = "cics-default" },
        // IBM CICS runs COBOL under Language Environment with LANGUAGE(COBOL) or a blank LANGUAGE.
        new() { ["language"] = "COBOL", ["dynamic"] = "NO", ["languageProvenance"] = "cics-blank-language-environment" },
    ];

    internal static bool CicsProgramAttributesValid(JsonNode? attributes) =>
        CicsProgramAttributes.Any(allowed => PyJson.PyEquals(attributes, allowed));

    private static string BindingBasis(bool terminal, bool fileOperation, bool asynchronous, bool queue, bool control, bool lockOperation) =>
        terminal ? "parsed-cics-terminal-operation"
        : fileOperation ? "parsed-cics-file-operation"
        : asynchronous ? "parsed-cics-async-transaction"
        : queue ? "parsed-cics-queue-operation"
        : control ? "parsed-cics-control-binding"
        : lockOperation ? "parsed-cics-lock-operation" : "parsed-current-task-operation";

    private static bool NativeCicsLocalBound(JsonNode statement, JsonNode syntax, List<JsonObject> nodes, List<string> tokens)
    {
        var rule = Str(statement, "grammarRule")!;
        var control = rule is "Cics_handleContext" or "Cics_linkContext" or "Cics_xctlContext";
        var fileOperation = CicsFileOperations.ContainsKey(rule);
        var lockOperation = rule is "Cics_enqContext" or "Cics_deqContext";
        var asynchronous = rule == "Cics_runContext";
        var queue = rule == "Cics_writeqContext";
        var terminal = CicsTerminalOperations.Contains(rule);
        var inquiry = rule == "Cics_inquireContext" && nodes.Any(node => Str(node, "rule") == "Cics_inquire_programContext");
        var operations = fileOperation ? CicsFileOperations[rule] : [CicsLocalOperations[rule]];
        var operation = Str(statement, "operation");
        if (!operations.Contains(operation) || tokens.Count == 0 || tokens[0].ToUpperInvariant() != operation
            || Str(statement, "bindingBasis") != BindingBasis(terminal, fileOperation, asynchronous, queue, control, lockOperation)
            || PyJson.Truthy(Get(statement, "bindingDiagnostics")))
            return false;
        if (terminal && !CicsTerminalVariant(syntax)) return false;
        if (terminal && CicsCompact(Str(statement, "sourceText")!) != CicsCompact(string.Concat(tokens))) return false;
        if (lockOperation && tokens.Count(token => token.ToUpperInvariant() == "RESOURCE") != 1) return false;
        if (rule is "Cics_getContext" or "Cics_putContext" && !nodes.Any(node =>
                Str(node, "rule") == (rule == "Cics_getContext" ? "Cics_get_channelContext" : "Cics_put_channelContext")))
            return false;
        var children = Items(syntax, "children").ToList();
        var condition = rule == "Cics_handleContext" && children.Count == 2 ? children[1] : null;
        if (Str(condition, "rule") == "Cics_handle_conditionContext")
        {
            // HANDLE CONDITION name[(label)]...: labels are checked below as CICS_LABEL bindings.
            var parts = Items(condition, "children").ToList();
            if (parts.Count < 2 || parts.Skip(1).Any(child => Str(child, "rule") == "Cics_handle_responseContext")) return false;
        }
        else if (rule == "Cics_handleContext")
        {
            var labels = CicsOperands(syntax, "LABEL").Count;
            var cancels = tokens.Count(token => token.ToUpperInvariant() == "CANCEL");
            if (!nodes.Any(node => Str(node, "rule") == "Cics_handle_abendContext")
                || tokens.Any(token => token.ToUpperInvariant() is "PROGRAM" or "RESET")
                || !(labels == 1 && cancels == 0 || labels == 0 && cancels == 1))
                return false;
        }
        JsonNode? operand = null;
        foreach (var node in nodes.Where(node => Str(node, "rule") == "Cics_link_programContext"))
        {
            var parts = Items(node, "children").ToList();
            for (var index = 1; index < parts.Count && operand is null; index++)
                if ((Str(parts[index - 1], "token") ?? "").ToUpperInvariant() == "PROGRAM" && Str(parts[index], "rule") == "Cics_nameContext")
                    operand = parts[index];
            if (operand is not null) break;
        }
        var programNodes = Identities(operand is not null ? SyntaxNodes(operand) : []);
        var terminalNodes = Identities(CicsOperands(syntax, "TERMINAL").SelectMany(SyntaxNodes));
        var associationNodes = Identities(CicsOperands(syntax, "ASSOCIATION").SelectMany(SyntaxNodes));
        var references = new JsonArray();
        var referenceNodes = new List<JsonObject>();
        foreach (var node in nodes)
        {
            var label = Str(node, "rule") == "ParagraphNameUsageContext";
            var data = Str(node, "rule") == "VariableNameUsageContext" && Arr(node, "children") is { Count: > 0 } first
                && Str(first[0], "rule") == "GeneralIdentifierContext";
            if (!(label || data) || programNodes.Contains(node)) continue;
            var name = SyntaxText(node).ToUpperInvariant();
            var context = rule is "Cics_inquireContext" or "Cics_set_terminalContext" && name == "EIBTRMID" && terminalNodes.Contains(node)
                || rule == "Cics_inquireContext" && name == "EIBTASKN" && associationNodes.Contains(node);
            references.Add(new JsonObject
            {
                ["kind"] = context ? "CICS_CONTEXT" : label ? "CICS_LABEL" : "CICS_DATA",
                ["name"] = name,
                ["grammarRule"] = Str(node, "rule"),
            });
            referenceNodes.Add(node);
        }
        if (!PyJson.PyEquals(Get(statement, "resourceReferences"), references)
            || Get(statement, "resourceBindings") is not JsonArray bindingArray || bindingArray.Any(binding => binding is not JsonObject))
            return false;
        var bindings = bindingArray.ToList();
        var local = bindings.Where(binding => Str(binding, "kind") is "CICS_DATA" or "CICS_LABEL" or "CICS_CONTEXT").ToList();
        if (local.Count != references.Count
            || local.Zip(references).Any(pair => Str(pair.First, "kind") != Str(pair.Second, "kind")
                || !PyJson.PyEquals(Get(pair.First, "name"), Get(pair.Second, "name"))))
            return false;
        foreach (var (binding, reference) in local.Zip(referenceNodes))
        {
            var kind = Str(binding, "kind");
            if (kind == "CICS_CONTEXT")
            {
                var role = Str(binding, "name") switch { "EIBTRMID" => "terminal-id", "EIBTASKN" => "task-number", _ => null };
                if (role is null || Str(binding, "definitionScope") != "cics-execution-interface" || Str(binding, "role") != role)
                    return false;
                continue;
            }
            var definition = Get(binding, "definitionSyntax");
            if (Str(binding, "definitionScope") != "expanded-parser-input" || definition is not JsonObject || SyntaxNodes(definition).Count == 0)
                return false;
            if (kind == "CICS_DATA")
            {
                if (Str(definition, "rule") is not ("DataDescriptionEntryFormat1Context" or "DataDescriptionEntryFormat1Level77Context")
                    || !NativeCicsDataBound(binding, reference))
                    return false;
            }
            else if (Str(definition, "rule") is not ("ParagraphDefinitionNameContext" or "SectionNameContext")
                     || !StrEquals(Get(binding, "name"), SyntaxText(definition).ToUpperInvariant()))
                return false;
        }
        List<JsonNode?> OfKind(string kind) => bindings.Where(binding => Str(binding, "kind") == kind).ToList();
        var programs = OfKind("CICS_PROGRAM");
        var transactions = OfKind("CICS_TRANSACTION");
        var maps = OfKind("CICS_MAP");
        var files = OfKind("CICS_FILE");
        var queues = OfKind("CICS_TDQUEUE");
        if (queue)
            return local.Count + queues.Count == bindings.Count && queues.Count == 1 && NativeCicsTdqueueBound(syntax, queues[0]);
        var expectedPrograms = rule == "Cics_linkContext" && !programs.Any(binding => Has(binding, "controlEffect")) ? 1
            : inquiry || rule is "Cics_xctlContext" or "Cics_linkContext" ? programs.Count : 0;
        if (local.Count + programs.Count + transactions.Count + maps.Count + files.Count != bindings.Count
            || programs.Count != expectedPrograms
            || (rule is "Cics_xctlContext" or "Cics_linkContext" || inquiry) && programs.Count == 0
            || files.Count != (fileOperation ? 1 : 0))
            return false;
        if (fileOperation)
            return transactions.Count == 0 && maps.Count == 0 && NativeCicsFileBound(syntax, operation!, files[0]);
        if (rule == "Cics_xctlContext")
            return transactions.Count == 0 && maps.Count == 0
                && !children.Where(child => Has(child, "token"))
                    .Any(child => PyJson.PyStr(Get(child, "token")).ToUpperInvariant() is "CHANNEL" or "INPUTMSG" or "INPUTMSGLEN")
                && CicsProgramTargetsBound(syntax, programs, "transfer-control");
        if (inquiry)
            return transactions.Count == 0 && maps.Count == 0 && CicsProgramTargetsBound(syntax, programs, "program-definition-inquiry");
        if (maps.Count != CicsOperands(syntax, "MAP").Count || maps.Any(binding => !NativeCicsMapBound(syntax, binding)))
            return false;
        if (rule == "Cics_returnContext") return NativeCicsContinuationBound(syntax, transactions);
        if (asynchronous) return NativeCicsTransactionsBound(nodes, tokens, transactions);
        if (transactions.Count > 0) return false;
        if (programs.Count == 0) return true;
        if (tokens.Any(token => token.ToUpperInvariant() is "SYSID" or "CHANNEL" or "INPUTEVENT" or "ACTIVITY")) return false;
        if (programs.Any(binding => Has(binding, "controlEffect"))) return CicsProgramTargetsBound(syntax, programs, "call");
        var program = programs[0];
        var resolution = Has(program, "targetResolution") ? Get(program, "targetResolution") : new JsonObject();
        if (operand is null || programs.Count != 1 || !CicsProgramAttributesValid(Get(program, "attributes"))
            || !new[] { "sourcePath", "programSourcePath" }.All(key => IsAbsolute(Str(program, key)))
            || !new[] { "sourceSha256", "programSourceSha256" }.All(key => Full("[0-9a-f]{64}", Str(program, key)) is not null)
            || resolution is not JsonObject)
            return false;
        var text = SyntaxText(operand);
        var value = text.Length >= 2 ? text[1..^1] : "";
        if (Str(resolution, "kind") == "literal")
            return value.Length <= 10 && Full("""(['"])[A-Za-z0-9@$#_-]{1,8} *\1""", value) is not null
                && StrEquals(Get(program, "name"), RStrip(value[1..^1]));
        var definitionSyntax = Get(resolution, "definitionSyntax");
        if (Str(resolution, "kind") is not ("immutable-working-storage-literal" or "immutable-local-storage-literal")
            || Str(resolution, "operand") != value
            || Str(resolution, "usageProof") != "single-native-program:all-symbol-references-are-cics-program-operands"
            || Str(definitionSyntax, "rule") != "DataDescriptionEntryFormat1Context")
            return false;
        var match = Full("01" + Regex.Escape(value) + """PIC(?:TURE)?(?:IS)?X\(0*8\)VALUE(?:IS)?(['"])([A-Z0-9@$#_-]{1,8})( *)\1\.""",
            SyntaxText(definitionSyntax), true);
        return match is not null && match.Groups[2].Length + match.Groups[3].Length <= 8 && StrEquals(Get(program, "name"), match.Groups[2].Value);
    }

    private static HashSet<JsonObject> Identities(IEnumerable<JsonObject> nodes) => new(nodes, ReferenceEqualityComparer.Instance);

    private static bool NativeCicsTransactionsBound(List<JsonObject> nodes, List<string> tokens, List<JsonNode?> transactions)
    {
        var variants = nodes.Where(node => Str(node, "rule") == "Cics_run_transidContext").ToList();
        if (variants.Count != 1 || tokens.Count(token => token.ToUpperInvariant() == "CHILD") != 1 || transactions.Count == 0) return false;
        var operand = Items(variants[0], "children").FirstOrDefault(child => Str(child, "rule") == "Cics_nameContext");
        var text = SyntaxText(operand);
        var value = text.Length >= 2 ? text[1..^1] : "";
        var resolution = Has(transactions[0], "targetResolution") ? Get(transactions[0], "targetResolution") : new JsonObject();
        List<string> names;
        if (Str(resolution, "kind") == "literal")
        {
            if (Full("""(['"])[A-Za-z0-9@$#_-]{4}\1""", value) is null) return false;
            names = [value[1..^1]];
        }
        else if (Str(resolution, "kind") == "bounded-varying-string")
        {
            if (Str(resolution, "operand") != value
                || Str(resolution, "usageProof") != "single-native-program:unconditional-string-with-read-only-target-and-counter-uses")
                return false;
            var texts = new Dictionary<string, string>();
            foreach (var (key, rule) in new[] { ("definitionSyntax", "DataDescriptionEntryFormat1Context"),
                         ("counterDefinitionSyntax", "DataDescriptionEntryFormat1Context"),
                         ("loopHeaderSyntax", "PerformTypeContext"), ("stringSyntax", "StringStatementContext") })
            {
                var syntax = Get(resolution, key);
                if (syntax is not JsonObject || Str(syntax, "rule") != rule || SyntaxNodes(syntax).Count == 0) return false;
                texts[key] = SyntaxText(syntax);
            }
            var counter = Str(resolution, "counter");
            if (counter is null || Full("[A-Z][A-Z0-9_-]*", counter) is null) return false;
            var bounds = Full("(?:WITH)?(?:TESTBEFORE)?VARYING" + Regex.Escape(counter) + "FROM([1-8])BY1UNTIL"
                + Regex.Escape(counter) + ">([1-8])(?:WITHTESTBEFORE)?", texts["loopHeaderSyntax"]);
            var construction = Full("""STRING(['"])([A-Za-z0-9@$#_-]{3})\1DELIMITED(?:BY)?SIZE,?""" + Regex.Escape(counter)
                + "DELIMITED(?:BY)?SIZEINTO" + Regex.Escape(value) + "(?:END-STRING)?", texts["stringSyntax"]);
            if (bounds is null || construction is null || Int(bounds, 1) > Int(bounds, 2)
                || Full("01" + Regex.Escape(value) + @"PIC(?:TURE)?(?:IS)?X\(0*4\)(?:VALUE(?:IS)?SPACES?)?\.", texts["definitionSyntax"]) is null
                || Full("01" + Regex.Escape(counter) + @"PIC(?:TURE)?(?:IS)?9(?:VALUE(?:IS)?0)?\.", texts["counterDefinitionSyntax"]) is null)
                return false;
            names = Enumerable.Range(Int(bounds, 1), Int(bounds, 2) - Int(bounds, 1) + 1)
                .Select(number => construction.Groups[2].Value + number).ToList();
            if (!PyJson.PyEquals(Get(resolution, "transactionNames"), ToArray(names))) return false;
        }
        else return false;
        if (transactions.Count != names.Count || transactions.Zip(names).Any(pair => !StrEquals(Get(pair.First, "name"), pair.Second)))
            return false;
        foreach (var binding in transactions)
        {
            var attributes = Has(binding, "attributes") ? Get(binding, "attributes") : new JsonObject();
            var program = Has(binding, "programBinding") ? Get(binding, "programBinding") : new JsonObject();
            if (!PyJson.PyEquals(Get(binding, "targetResolution"), resolution)
                || !KeysAre(attributes, "program", "dynamic", "routable", "status")
                || Str(attributes, "dynamic") != "NO" || Str(attributes, "routable") != "NO" || Str(attributes, "status") != "ENABLED"
                || program is not JsonObject || Str(program, "kind") != "CICS_PROGRAM"
                || !PyJson.PyEquals(Get(program, "name"), Get(attributes, "program"))
                || !CicsProgramAttributesValid(Get(program, "attributes")))
                return false;
            foreach (var resource in new[] { binding, program })
                if (!IsAbsolute(Str(resource, "sourcePath")) || !IsHex64(Get(resource, "sourceSha256"))) return false;
            if (!IsAbsolute(Str(program, "programSourcePath")) || !IsHex64(Get(program, "programSourceSha256"))) return false;
        }
        return true;
    }

    internal static List<JsonNode?> NativeCicsResources(JsonNode? statement)
    {
        var resources = new List<JsonNode?>();
        foreach (var binding in Items(statement, "resourceBindings"))
        {
            resources.Add(binding);
            if (Str(binding, "kind") == "CICS_TRANSACTION") resources.Add(Get(binding, "programBinding"));
        }
        return resources;
    }

    internal static bool CicsMapEvidenceCurrent(JsonNode? binding, List<JsonObject> definitions, string sourceRoot)
    {
        var selected = new List<string>();
        foreach (var (kind, key) in new[] { ("CICS_MAP", "sourceSha256"), ("CICS_MAP_LAYOUT", "layoutSha256"), ("COPY_INPUT", "outputSha256") })
        {
            var matches = definitions.Where(item => Str(item, "kind") == kind && PyJson.PyEquals(Get(item, "name"), Get(binding, "name"))
                && PyJson.PyEquals(Get(item, "sha256"), Get(binding, key))).ToList();
            if (matches.Count != 1 || !IsRelativeTo(PyCompat.Resolve(Str(matches[0], "path")!), PyCompat.Resolve(sourceRoot)))
                return false;
            selected.Add(Str(matches[0], "path")!);
        }
        var proof = BmsSymbolicMap.CompleteLayoutEvidence(selected[0], selected[2]);
        var maps = Arr(proof, "maps")!.Where(screen => PyJson.PyEquals(Get(screen, "name"), Get(binding, "name"))
            && PyJson.PyEquals(Get(screen, "mapset"), Get(binding, "mapset"))).ToList();
        return PyJson.PyEquals(ReadJson(selected[1]), proof) && PyCompat.Digest(selected[1]) == Str(binding, "layoutSha256")
            && PyJson.PyEquals(Get(proof, "sourceSha256"), Get(binding, "sourceSha256"))
            && PyJson.PyEquals(Get(proof, "outputSha256"), Get(binding, "outputSha256"))
            && maps.Count == 1 && PyJson.PyEquals(maps[0], Get(binding, "definition"));
    }
}
