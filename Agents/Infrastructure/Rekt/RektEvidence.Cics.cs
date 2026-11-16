using System.Text;
using System.Text.Json.Nodes;
using System.Text.RegularExpressions;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Rekt;

// Native CICS statements count as bound only when their recorded syntax, resource bindings and operand proofs agree.
public static partial class RektEvidence
{
    internal static readonly Dictionary<string, string> CicsLocalOperations = new()
    {
        ["Cics_assignContext"] = "ASSIGN", ["Cics_abendContext"] = "ABEND", ["Cics_syncpointContext"] = "SYNCPOINT",
        ["Cics_asktimeContext"] = "ASKTIME", ["Cics_formattimeContext"] = "FORMATTIME",
        ["Cics_handleContext"] = "HANDLE", ["Cics_linkContext"] = "LINK",
        ["Cics_enqContext"] = "ENQ", ["Cics_deqContext"] = "DEQ",
        ["Cics_delayContext"] = "DELAY", ["Cics_getContext"] = "GET", ["Cics_putContext"] = "PUT",
        ["Cics_fetchContext"] = "FETCH", ["Cics_runContext"] = "RUN", ["Cics_xctlContext"] = "XCTL",
        ["Cics_sendContext"] = "SEND", ["Cics_receiveContext"] = "RECEIVE", ["Cics_returnContext"] = "RETURN",
        ["Cics_inquireContext"] = "INQUIRE", ["Cics_set_terminalContext"] = "SET", ["Cics_bifContext"] = "BIF",
        ["Cics_writeqContext"] = "WRITEQ",
    };

    internal static readonly Dictionary<string, string[]> CicsFileOperations = new()
    {
        ["Cics_readContext"] = ["READ"], ["Cics_readnextContext"] = ["READNEXT", "READPREV"],
        ["Cics_writeContext"] = ["WRITE"], ["Cics_rewriteContext"] = ["REWRITE"], ["Cics_deleteContext"] = ["DELETE"],
        ["Cics_startbrContext"] = ["STARTBR"], ["Cics_endbrContext"] = ["ENDBR"], ["Cics_unlockContext"] = ["UNLOCK"],
    };

    // Keyed VSAM access only; matches the native parser's unsupported file options.
    private static readonly HashSet<string> CicsFileUnsupported =
    [
        "SYSID", "RBA", "RRN", "RBN", "XRBA", "SET", "TOKEN", "MASSINSERT", "GENERIC", "REQID",
        "DEBKEY", "DEBREC", "UNCOMMITTED", "CONSISTENT", "REPEATABLE", "REPETABLE",
    ];

    private static readonly Dictionary<string, string> CicsFileServices = new()
    {
        ["READNEXT"] = "browse", ["READPREV"] = "browse", ["STARTBR"] = "browse", ["ENDBR"] = "browse",
        ["WRITE"] = "add", ["REWRITE"] = "update", ["UNLOCK"] = "update", ["DELETE"] = "delete",
    };

    private static readonly HashSet<string> CicsTerminalOperations =
    [
        "Cics_sendContext", "Cics_receiveContext", "Cics_returnContext",
        "Cics_inquireContext", "Cics_set_terminalContext", "Cics_bifContext",
    ];

    private static readonly HashSet<string> PlainSendTail =
    [
        "Cics_send_from_waitContext", "Cics_send_defaultmaxContext", "Cics_send_lu23Context",
        "Cics_send_from_ctlcharContext", "Cics_send_3560_3270Context", "Cics_send_eraseContext",
    ];

    private static List<string> UpperTokens(JsonNode? syntax) =>
        SyntaxNodes(syntax).Where(node => node.ContainsKey("token")).Select(node => PyJson.Str(node["token"])!.ToUpperInvariant()).ToList();

    internal static bool NativeCicsStatementBound(JsonNode? statement)
    {
        var syntax = Get(statement, "syntax");
        if (syntax is not JsonObject || Str(statement, "bindingStatus") != "complete" || Str(statement, "sourceText") is null)
            return false;
        var rule = Get(statement, "grammarRule");
        if (!PyJson.PyEquals(Get(syntax, "rule"), rule)) return false;
        var ruleText = PyJson.Str(rule);
        var nodes = SyntaxNodes(syntax);
        var tokens = Tokens(nodes);
        if (ruleText == "Cics_returnContext" && tokens.Select(token => token.ToUpperInvariant()).SequenceEqual(["RETURN"]))
            return Str(statement, "operation") == "RETURN" && Items(syntax, "children").Count() == 1
                && Str(statement, "sourceText")!.Trim().ToUpperInvariant() == "RETURN"
                && !PyJson.Truthy(Get(statement, "resourceBindings")) && !PyJson.Truthy(Get(statement, "bindingDiagnostics"));
        if (ruleText is "CicsDfhRespLiteralContext" or "CicsDfhValueLiteralContext")
        {
            var operation = ruleText == "CicsDfhRespLiteralContext" ? "DFHRESP" : "DFHVALUE";
            string[] symbols = operation == "DFHRESP"
                ? ["NORMAL", "INVREQ", "MAPFAIL", "NOTFINISHED", "NOTFND", "ENDFILE", "DUPREC", "DUPKEY"]
                : ["NORMAL", "ABEND", "SECERROR", "UCTRAN", "TRANIDONLY", "NOUCTRAN"];
            return Str(statement, "operation") == operation && tokens.Count == 4
                && tokens[0].ToUpperInvariant() == operation && tokens[1] == "(" && tokens[3] == ")"
                && symbols.Contains(tokens[2].ToUpperInvariant())
                && Full(operation + @"\s*\(\s*" + Regex.Escape(tokens[2]) + @"\s*\)", Str(statement, "sourceText")!.Trim(), true) is not null;
        }
        if (ruleText is not null && (CicsLocalOperations.ContainsKey(ruleText)
                || CicsFileOperations.ContainsKey(ruleText) && Str(statement, "bindingBasis") == "parsed-cics-file-operation"))
            return NativeCicsLocalBound(statement!, syntax, nodes, tokens);
        if (ruleText != "Cics_writeContext" || Str(statement, "operation") != "WRITE") return false;
        var files = nodes.Where(node => Str(node, "rule") == "Cics_file_nameContext").ToList();
        if (files.Count != 1 || !nodes.Any(node => Str(node, "rule") == "Cics_write_fileContext")
            || tokens.Any(token => token.ToUpperInvariant() == "SYSID")
            || Get(statement, "resourceBindings") is not JsonArray { Count: 1 } bindings || bindings[0] is not JsonObject binding)
            return false;
        var fileTokens = Tokens(SyntaxNodes(files[0]));
        if (fileTokens.Count != 4 || fileTokens[0].ToUpperInvariant() is not ("FILE" or "DATASET")
            || fileTokens[1] != "(" || fileTokens[3] != ")")
            return false;
        var operand = fileTokens[2];
        if (operand.Length < 2 || operand[0] is not ('\'' or '"') || operand[^1] != operand[0]) return false;
        var attributes = Get(binding, "attributes");
        return Str(binding, "kind") == "CICS_FILE" && Str(binding, "name") == operand[1..^1]
            && IsAbsolute(Str(binding, "sourcePath")) && Str(binding, "sourceSha256") is { } sha && Full("[0-9a-f]{64}", sha) is not null
            && attributes is JsonObject && Str(attributes, "dsname") is { } dsname && dsname.Trim().Length > 0
            && IsInt(Get(attributes, "recordsize"), out var recordSize) && IsInt(Get(attributes, "keylength"), out var keyLength)
            && 0 <= keyLength && keyLength <= recordSize && recordSize > 0;
    }

    private static List<string> CicsFileOptions(JsonNode syntax)
    {
        string[] parents = ["Cics_write_fileContext", "Cics_delete_fileContext", "Cics_delete_ridfldContext", "Cics_intoContext"];
        var options = new List<string>();
        void Visit(JsonNode node, string? parentRule)
        {
            foreach (var child in Items(node, "children"))
            {
                if (Has(child, "token") && (ReferenceEquals(node, syntax) || parents.Contains(parentRule)))
                    options.Add(PyJson.PyStr(Get(child, "token")).ToUpperInvariant());
                else if (Has(child, "rule"))
                    Visit(child!, Str(child, "rule"));
            }
        }
        Visit(syntax, Str(syntax, "rule"));
        return options;
    }

    // WRITEQ TD QUEUE(name) FROM(area) [LENGTH(n)] to a local fixed-record extrapartition output queue.
    private static bool NativeCicsTdqueueBound(JsonNode syntax, JsonNode? binding)
    {
        var children = Items(syntax, "children").ToList();
        var queues = CicsOperands(syntax, "QUEUE");
        if (children.Count != 2 || Str(children[1], "rule") != "Cics_writeq_tdContext" || queues.Count != 1
            || CicsOperands(syntax, "FROM").Count != 1 || CicsOperands(syntax, "LENGTH").Count > 1
            || CicsOperands(syntax, "SYSID").Count > 0)
            return false;
        var name = CicsConstantOperand(queues[0], Get(binding, "targetResolution"), 1, 4);
        var attributes = Get(binding, "attributes");
        return name is not null && Str(binding, "name") == name
            && KeysAre(binding, "kind", "name", "sourcePath", "sourceSha256", "attributes", "targetResolution")
            && IsAbsolute(Str(binding, "sourcePath")) && IsHex64(Get(binding, "sourceSha256"))
            && KeysAre(attributes, "type", "typefile", "ddname", "recordsize", "recordformat")
            && StrEquals(Get(attributes, "type"), "EXTRA") && StrEquals(Get(attributes, "typefile"), "OUTPUT")
            && StrEquals(Get(attributes, "recordformat"), "FIXED")
            && Full("[A-Z@$#][A-Z0-9@$#]{0,7}", Str(attributes, "ddname")) is not null
            && IsInt(Get(attributes, "recordsize"), out var size) && size > 0;
    }

    private static bool NativeCicsFileBound(JsonNode syntax, string operation, JsonNode? binding)
    {
        var names = SyntaxNodes(syntax).Where(node => Str(node, "rule") == "Cics_file_nameContext").ToList();
        var options = CicsFileOptions(syntax);
        if (names.Count != 1 || Items(names[0], "children").Count() != 2
            || Str(Arr(names[0], "children")![1], "rule") != "Cics_nameContext"
            || options.Any(CicsFileUnsupported.Contains)
            || Str(syntax, "rule") == "Cics_deleteContext" && Items(syntax, "children").Count() != 2)
            return false;
        var name = CicsConstantOperand(Arr(names[0], "children")![1], Get(binding, "targetResolution"), 1, 8);
        var attributes = Get(binding, "attributes") as JsonObject;
        if (name is null || Str(binding, "kind") != "CICS_FILE" || Str(binding, "name") != name
            || !IsAbsolute(Str(binding, "sourcePath")) || !IsHex64(Get(binding, "sourceSha256"))
            || attributes is null || Str(attributes, "dsname") is not { } dsname || dsname.Trim().Length == 0
            || !IsInt(Get(attributes, "recordsize"), out var recordSize) || !IsInt(Get(attributes, "keylength"), out var keyLength)
            || !(0 <= keyLength && keyLength <= recordSize && recordSize > 0))
            return false;
        if (!attributes.ContainsKey("services"))
            return KeysAre(attributes, "dsname", "recordsize", "keylength") && !Has(binding, "datasetDefinitions");
        var services = attributes["services"];
        var sources = Get(binding, "datasetDefinitions");
        var service = operation == "READ" ? options.Contains("UPDATE") ? "update" : "read" : CicsFileServices[operation];
        var path = Str(attributes, "accessPath");
        if (!KeysAre(attributes, "dsname", "organization", "accessPath", "recordsize", "keylength", "keyOffset",
                "uniqueKeys", "services", "lengthProvenance")
            || !StrEquals(attributes["organization"], "KSDS") || !StrEquals(attributes["lengthProvenance"], "idcams-define")
            || !KeysAre(services, "add", "browse", "delete", "read", "update")
            || !((JsonObject)services!).All(pair => PyJson.Str(pair.Value) is "YES" or "NO") || !StrEquals(services![service], "YES")
            || !IsInt(attributes["keyOffset"], out var keyOffset) || !PyJson.IsBool(attributes["uniqueKeys"])
            || keyOffset + keyLength > recordSize
            || path is not ("base-cluster" or "alternate-index-path")
            || sources is not JsonArray { Count: > 0 } list)
            return false;
        foreach (var source in list)
        {
            if (!KeysAre(source, "type", "name", "sourcePath", "sourceSha256", "command")
                || Str(source, "type") is not ("CLUSTER" or "ALTERNATEINDEX" or "PATH") || !IsAbsolute(Str(source, "sourcePath"))
                || !IsHex64(Get(source, "sourceSha256")) || Str(source, "command") is not { } command)
                return false;
            var type = Str(source, "type")!;
            if (Start(@"DEFINE\s+(?:" + (type == "ALTERNATEINDEX" ? "AIX|" : "") + type + @")\s*\(\s*NAME\s*\("
                    + Regex.Escape(PyJson.PyStr(Get(source, "name"))) + @"\)", command, true) is null)
                return false;
        }
        var types = list.Select(source => Str(source, "type")!).ToHashSet();
        return types.SetEquals(path == "base-cluster" ? ["CLUSTER"] : ["CLUSTER", "ALTERNATEINDEX", "PATH"])
            && list.Any(source => Str(source, "type") == (path == "base-cluster" ? "CLUSTER" : "PATH")
                && PyJson.PyEquals(Get(source, "name"), attributes["dsname"]));
    }

    internal static List<JsonNode> CicsOperands(JsonNode? syntax, string option)
    {
        var operands = new List<JsonNode>();
        foreach (var node in SyntaxNodes(syntax))
        {
            var children = Items(node, "children").ToList();
            for (var index = 1; index < children.Count; index++)
            {
                if ((Str(children[index - 1], "token") ?? "").ToUpperInvariant() != option || !Has(children[index], "rule")) continue;
                operands.Add(option == "MAP" && Str(children[index], "rule") == "Cics_receive_mapContext"
                    ? Arr(children[index], "children")![0]! : children[index]!);
            }
        }
        return operands;
    }

    // Ordered distinct program names a PROGRAM operand can denote, or null.
    internal static List<string>? CicsProgramTargets(JsonNode? operand, JsonNode? resolution, int minimum, int maximum)
    {
        var value = CicsConstantOperand(operand, resolution, minimum, maximum);
        if (value is not null) return [value];
        var values = CicsTableOperand(operand, resolution, minimum, maximum) is { Count: > 0 } table ? table
            : CicsReachingOperand(operand, resolution, minimum, maximum) is { Count: > 0 } reaching ? reaching
            : CicsSlotOperand(operand, resolution, minimum, maximum);
        return values is { Count: > 0 } ? values.Where(item => !string.IsNullOrEmpty(item)).Select(item => item!).Distinct().ToList() : null;
    }

    private static bool CicsProgramTargetsBound(JsonNode syntax, List<JsonNode?> programs, string effect)
    {
        var operands = CicsOperands(syntax, "PROGRAM");
        if (operands.Count != 1 || programs.Count == 0
            || programs.Any(binding => !PyJson.PyEquals(Get(binding, "targetResolution"), Get(programs[0], "targetResolution"))))
            return false;
        var proof = Get(programs[0], "targetResolution");
        var targets = proof is JsonObject && (effect == "transfer-control"
                || Str(proof, "kind") is not ("structured-reaching-assignments" or "flow-insensitive-slot-writes" or "closed-world-commarea-slot"))
            ? CicsProgramTargets(operands[0], proof, 1, 8) : null;
        return targets is not null && programs.Select(binding => Get(binding, "name")).Count() == targets.Count
            && programs.Select(binding => Get(binding, "name")).Zip(targets).All(pair => StrEquals(pair.First, pair.Second))
            && programs.All(binding => Str(binding, "controlEffect") == effect
                && CicsProgramAttributesValid(Get(binding, "attributes"))
                && new[] { "sourcePath", "programSourcePath" }.All(key => IsAbsolute(Str(binding, key)))
                && new[] { "sourceSha256", "programSourceSha256" }.All(key => Full("[0-9a-f]{64}", Str(binding, key)) is not null));
    }

    // SEND FROM(area) LENGTH|FLENGTH(n) [WAIT] [ERASE] [RESP/NOHANDLE]: a raw write to the principal facility.
    private static bool CicsPlainSend(JsonNode syntax)
    {
        static bool Tail(JsonNode? node)
        {
            if (Str(node, "rule") == "Cics_handle_responseContext") return true;
            if (Has(node, "token")) return PyJson.PyStr(Get(node, "token")).ToUpperInvariant() is "WAIT" or "ERASE";
            return Str(node, "rule") is { } rule && PlainSendTail.Contains(rule)
                && (rule != "Cics_send_eraseContext" || Items(node, "children").Count() == 1)
                && Items(node, "children").All(Tail);
        }
        var children = Items(syntax, "children").ToList();
        var group = children.Count == 2 ? children[1] : null;
        var parts = Str(group, "rule") == "Cics_send_groupContext" ? Items(group, "children").ToList() : [];
        return parts.Count == 2 && Str(parts[0], "rule") == "Cics_send_fromContext" && Tail(parts[1]);
    }

    private static bool CicsTerminalVariant(JsonNode syntax)
    {
        var nodes = SyntaxNodes(syntax);
        var rules = nodes.Select(node => Str(node, "rule")).ToHashSet();
        var tokens = UpperTokens(syntax);
        var supported = Str(syntax, "rule") switch
        {
            "Cics_sendContext" => rules.Overlaps(["Cics_send_control_minContext", "Cics_send_map_minContext", "Cics_send_text_stdContext"])
                || CicsPlainSend(syntax),
            "Cics_receiveContext" => rules.Contains("Cics_receive_mapContext") && !rules.Contains("Cics_receive_mappingdevContext")
                && CicsOperands(syntax, "INPARTN").Count == 0,
            "Cics_returnContext" => !rules.Contains("Cics_return_inputmsgContext") && !tokens.Any(token => token is "CHANNEL" or "ENDACTIVITY"),
            "Cics_inquireContext" => rules.Contains("Cics_inquire_programContext") && CicsOperands(syntax, "PROGRAM").Count == 1
                || rules.Contains("Cics_inquire_terminalContext")
                || rules.Contains("Cics_inquire_associationContext") && CicsOperands(syntax, "ASSOCIATION").Count == 1,
            "Cics_set_terminalContext" => true,
            "Cics_bifContext" => rules.Contains("Cics_bif_deeditContext") && CicsOperands(syntax, "FIELD").Count == 1,
            _ => false,
        };
        return supported && new[]
        {
            "TRANSID", "MAP", "MAPSET", "TERMINAL", "UCTRANST", "COMMAREA", "LENGTH", "FROM", "INTO", "FIELD", "RESP", "RESP2",
            "PROGRAM", "ASSOCIATION", "ODAPPLID", "ODUSERID", "ODFACILNAME", "ODNETWORKID", "ODFACILTYPE",
        }.All(option => CicsOperands(syntax, option).Count <= 1);
    }

    internal static string CicsCompact(string text) =>
        string.Concat(Rx("""'(?:''|[^'])*'|"(?:""|[^"])*"|\S""").Matches(text).Select(match => match.Value)).ToUpperInvariant();

    private static bool NativeCicsMapBound(JsonNode syntax, JsonNode? binding)
    {
        var maps = CicsOperands(syntax, "MAP");
        var mapsets = CicsOperands(syntax, "MAPSET");
        if (maps.Count != 1 || mapsets.Count > 1) return false;
        var resolution = Get(binding, "targetResolution");
        if (resolution is not null && !(mapsets.Count > 0 ? KeysAre(resolution, "map", "mapset") : KeysAre(resolution, "map")))
            return false;
        string? Screen(JsonNode operand, JsonNode? proof)
        {
            var value = CicsConstantOperand(operand, proof, 1, 7);
            if (value is null && Str(proof, "kind") == "structured-reaching-assignments")
            {
                // One SEND names one screen: only a reaching proof with a single possible name binds it.
                var reaching = Str(syntax, "rule") == "Cics_sendContext" ? CicsReachingOperand(operand, proof, 1, 7) : null;
                value = reaching is { Count: 1 } ? reaching[0] : null;
            }
            return value;
        }
        var name = Screen(maps[0], Get(resolution, "map"));
        var mapset = mapsets.Count > 0 ? Screen(mapsets[0], Get(resolution, "mapset")) : name;
        var definition = Has(binding, "definition") ? Get(binding, "definition") : new JsonObject();
        string[] modes = Str(syntax, "rule") == "Cics_receiveContext" ? ["IN", "INOUT"] : ["OUT", "INOUT"];
        return name is not null && mapset is not null && Str(binding, "kind") == "CICS_MAP"
            && StrEquals(Get(binding, "name"), name) && StrEquals(Get(binding, "mapset"), mapset)
            && definition is JsonObject && StrEquals(Get(definition, "name"), name) && StrEquals(Get(definition, "mapset"), mapset)
            && modes.Contains(Str(definition, "mode"))
            && new[] { "sourcePath", "layoutPath", "outputPath" }.All(key => IsAbsolute(Str(binding, key)))
            && new[] { "sourceSha256", "layoutSha256", "outputSha256" }.All(key => IsHex64(Get(binding, key)));
    }

    private static bool NativeCicsContinuationBound(JsonNode syntax, List<JsonNode?> transactions)
    {
        var operands = CicsOperands(syntax, "TRANSID");
        if (operands.Count == 0) return transactions.Count == 0;
        if (operands.Count != 1 || transactions.Count != 1) return false;
        var transaction = transactions[0];
        var name = Get(transaction, "targetResolution") is JsonObject proof ? CicsConstantOperand(operands[0], proof, 4, 4) : null;
        var attributes = Has(transaction, "attributes") ? Get(transaction, "attributes") : new JsonObject();
        var program = Has(transaction, "programBinding") ? Get(transaction, "programBinding") : new JsonObject();
        return name is not null && StrEquals(Get(transaction, "name"), name)
            && Str(transaction, "controlEffect") == "transaction-continuation"
            && KeysAre(attributes, "program", "dynamic", "routable", "status")
            && Str(attributes, "dynamic") == "NO" && Str(attributes, "routable") == "NO" && Str(attributes, "status") == "ENABLED"
            && program is JsonObject && Str(program, "kind") == "CICS_PROGRAM"
            && PyJson.PyEquals(Get(program, "name"), Get(attributes, "program"))
            && CicsProgramAttributesValid(Get(program, "attributes"))
            && IsAbsolute(Str(transaction, "sourcePath")) && IsAbsolute(Str(program, "sourcePath"))
            && IsHex64(Get(transaction, "sourceSha256")) && IsHex64(Get(program, "sourceSha256"))
            && IsAbsolute(Str(program, "programSourcePath")) && IsHex64(Get(program, "programSourceSha256"));
    }

    private static string? EntryName(JsonNode? syntax, string? none)
    {
        var entries = SyntaxNodes(syntax).Where(node => Str(node, "rule") == "EntryNameContext").ToList();
        return entries.Count == 1 ? SyntaxText(entries[0]).ToUpperInvariant() : entries.Count == 0 ? none : null;
    }

    private static List<string> FirstTokens(JsonArray path) =>
        path.Select(item => SyntaxNodes(item).Select(node => Str(node, "token")).FirstOrDefault(token => token is not null) ?? "").ToList();

    private static bool LevelsValid(List<string> levels) =>
        levels.All(level => PyCompat.IsDigits(level) && int.TryParse(level, out var value) && value is >= 1 and <= 49)
        && levels.Zip(levels.Skip(1)).All(pair => int.Parse(pair.First) < int.Parse(pair.Second));

    internal static bool NativeCicsDataBound(JsonNode? binding, JsonNode? reference)
    {
        var tokens = UpperTokens(reference);
        // LENGTH OF is a special register over the named item; LENGTH is reserved, never a data name.
        if (tokens.Count > 2 && tokens[0] == "LENGTH" && tokens[1] == "OF") tokens = tokens[2..];
        if (tokens.Count == 4 && tokens[1] == "(" && tokens[3] == ")") return NativeCicsSubscriptedBound(binding, tokens);
        if (tokens.Count == 0 || tokens.Count % 2 == 0
            || tokens.Select((token, index) => index % 2 == 0 ? Full("[A-Z][A-Z0-9_-]*", token) is null : token is not ("OF" or "IN")).Any(bad => bad))
            return false;
        var names = tokens.Where((_, index) => index % 2 == 0).ToList();
        var definition = Get(binding, "definitionSyntax");
        if (EntryName(definition, "FILLER") != names[0]) return false;
        if (names.Count == 1) return true;
        if (Get(binding, "qualificationPath") is not JsonArray { Count: >= 2 } path || !PyJson.PyEquals(path[^1], definition)
            || path.Any(item => item is not JsonObject || Str(item, "rule") != "DataDescriptionEntryFormat1Context" || SyntaxNodes(item).Count == 0))
            return false;
        if (!LevelsValid(FirstTokens(path))
            || path.Take(path.Count - 1).Any(item => SyntaxNodes(item).Any(node => Str(node, "rule") == "DataPictureClauseContext")))
            return false;
        var ancestors = path.Take(path.Count - 1).Reverse().Select(item => EntryName(item, "FILLER")).ToList();
        var cursor = 0;
        foreach (var qualifier in names.Skip(1))
        {
            while (cursor < ancestors.Count && ancestors[cursor] != qualifier) cursor++;
            if (cursor >= ancestors.Count) return false;
            cursor++;
        }
        return true;
    }

    // NAME(SUBSCRIPT): NAME sits under exactly one OCCURS entry; an identifier subscript is a declared item.
    private static bool NativeCicsSubscriptedBound(JsonNode? binding, List<string> tokens)
    {
        var definition = Get(binding, "definitionSyntax");
        if (Full("[A-Z][A-Z0-9_-]*", tokens[0]) is null || Full("[A-Z][A-Z0-9_-]*|[1-9][0-9]{0,3}", tokens[2]) is null
            || EntryName(definition, null) != tokens[0] || Get(binding, "qualificationPath") is not JsonArray { Count: > 0 } path
            || !PyJson.PyEquals(path[^1], definition)
            || path.Any(item => item is not JsonObject || Str(item, "rule") != "DataDescriptionEntryFormat1Context")
            || path.Count(item => SyntaxNodes(item).Any(node => Str(node, "rule") == "DataOccursClauseContext")) != 1)
            return false;
        var levels = FirstTokens(path);
        if (!LevelsValid(levels) || int.Parse(levels[0]) != 1) return false;
        if (PyCompat.IsDigits(tokens[2])) return !Has(binding, "subscriptSyntax");
        var subscript = Get(binding, "subscriptSyntax");
        return Str(subscript, "rule") is "DataDescriptionEntryFormat1Context" or "DataDescriptionEntryFormat1Level77Context"
            && EntryName(subscript, null) == tokens[2];
    }
}
