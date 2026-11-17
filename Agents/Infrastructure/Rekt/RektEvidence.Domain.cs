using System.Text.Json.Nodes;
using System.Text.RegularExpressions;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Rekt;

public static partial class RektEvidence
{
    public static readonly string[] Kinds = ["COPY", "CALL", "EXEC_CICS", "EXEC_SQL", "FILE_IO"];

    internal static readonly IReadOnlyDictionary<string, string> AbsenceProofs = new Dictionary<string, string>
    {
        ["EXEC_CICS"] = "antlr-tree:no-dialect-contexts",
        ["EXEC_SQL"] = "antlr-tree:no-dialect-contexts",
        ["FILE_IO"] = "antlr-tree:no-file-io-contexts",
    };

    // Python Counter in insertion order; subtraction keeps only positive counts.
    private sealed class Counter
    {
        private readonly List<(string, string)> order = [];
        private readonly Dictionary<(string, string), int> counts = [];

        public void Add((string, string) key)
        {
            if (!counts.TryAdd(key, 1)) counts[key]++;
            else order.Add(key);
        }

        public int this[(string, string) key] => counts.GetValueOrDefault(key);

        public IEnumerable<((string Kind, string Operation) Key, int Count)> Minus(Counter other) =>
            order.Select(key => (key, counts[key] - other[key])).Where(pair => pair.Item2 > 0);
    }

    internal static List<string> MissingNativeOperations(string source, JsonNode? document)
    {
        var lines = new List<string>();
        bool identification = false, documentary = false;
        foreach (var raw in PyCompat.SplitLines(PyCompat.ReadText(source, PyCompat.Latin1)))
        {
            var isFixed = raw.Length > 7 && raw[..6].All(character => character == ' ' || char.IsAsciiDigit(character))
                && " -*/Dd".Contains(raw[6]);
            if (isFixed && raw[6] is '*' or '/') continue;
            var line = isFixed ? PyCompat.Slice(raw, 7, 72) : raw;
            if (Start(@"\s*(?:IDENTIFICATION|ID)\s+DIVISION\b", line, true) is not null) identification = true;
            if (Start(@"\s*(?:ENVIRONMENT|DATA|PROCEDURE)\s+DIVISION\b", line, true) is not null) identification = documentary = false;
            if (identification && Start(@"\s*(?:AUTHOR|INSTALLATION|DATE-WRITTEN|DATE-COMPILED|SECURITY|REMARKS)\s*\.", line, true) is not null)
                documentary = true;
            if (!documentary) lines.Add((isFixed ? "      " + char.ToUpperInvariant(raw[6]) : "       ") + line);
        }
        var masked = RektSourceIdentity.DependencyGuardText(string.Join('\n', lines));
        masked = Rx("'(?:[^']|'')*'|\"(?:[^\"]|\"\")*\"").Replace(masked, " ");
        var ordinary = Rx("(?m)^      D.*$").Replace(masked, "");
        if (!Rx(@"\bWITH\s+DEBUGGING\s+MODE\b", true).IsMatch(Rx("(?m)^      -").Replace(ordinary, "       ")))
            masked = ordinary;
        masked = Rx("(?m)^      [-D]").Replace(masked, "       ");
        var original = new Counter();
        foreach (Match match in Rx(@"\bEXEC\s+(CICS|SQL)\s+([A-Z][A-Z0-9-]*)", true).Matches(masked))
        {
            var kind = match.Groups[1].Value.ToUpperInvariant();
            var operation = match.Groups[2].Value.ToUpperInvariant();
            if (!(kind == "SQL" && operation == "INCLUDE")) original.Add(("EXEC_" + kind, operation));
        }
        foreach (Match match in Rx(@"\b(DFHRESP|DFHVALUE)\s*\(", true).Matches(masked))
            original.Add(("EXEC_CICS", match.Groups[1].Value.ToUpperInvariant()));
        var native = new Counter();
        foreach (var statement in Items(document, "dependencyDialectStatements"))
            if (statement is JsonObject)
                native.Add((Str(statement, "kind") ?? "\0" + PyKey(Get(statement, "kind")),
                    (Has(statement, "operation") ? PyJson.PyStr(Get(statement, "operation")) : "").ToUpperInvariant()));
        return original.Minus(native)
            .Select(pair => $"original-operation-not-exported:{pair.Key.Kind}:{pair.Key.Operation}:{pair.Count}").ToList();
    }

    // Unqualified Db2 13 for z/OS built-in scalar functions missing from the grammar's dbs_inbuild_functions rule
    // (https://www.ibm.com/docs/en/db2-for-zos/13?topic=functions-timestamp-format); SYSIBM leads the default SQL path.
    private static readonly HashSet<string> Db2ZosBuiltinsOutsideGrammar = ["TIMESTAMP_FORMAT"];
    // Fully qualified Db2 for z/OS catalog tables whose contents carry no estate data
    // (https://www.ibm.com/docs/en/db2-for-zos/13.0.0?topic=tables-sysdummy1: one row, IBMREQD CHAR(1)).
    private static readonly HashSet<string> Db2ZosPlatformRelations = ["SYSIBM.SYSDUMMY1"];

    internal static List<JsonObject> NativeSqlReferences(JsonNode? syntax, string? parentRule = null)
    {
        var rule = Str(syntax, "rule");
        var kind = rule is not null && SqlReferenceRules.TryGetValue(rule, out var found) ? found : null;
        var nodes = SyntaxNodes(syntax);
        var joined = string.Concat(Tokens(nodes));
        if (kind == "SQL_FUNCTION" && (nodes.Any(node => Str(node, "rule") == "Dbs_inbuild_functionsContext")
                                       || Db2ZosBuiltinsOutsideGrammar.Contains(joined.ToUpperInvariant())))
            kind = null;
        if (kind == "SQL_RELATION" && parentRule != "Dbs_declare_tableContext"
            && Db2ZosPlatformRelations.Contains(joined.ToUpperInvariant()))
            kind = null;
        if (!string.IsNullOrEmpty(kind))
        {
            var name = joined.StartsWith(':') ? joined[1..] : joined;
            if (!name.Contains('"')) name = name.ToUpperInvariant();
            var declaration = parentRule == "Dbs_declare_tableContext"
                || kind == "SQL_CURSOR" && parentRule == "Dbs_declare_cursorContext";
            return [new JsonObject
            {
                ["kind"] = kind, ["name"] = name, ["role"] = declaration ? "declaration" : "reference", ["grammarRule"] = rule,
            }];
        }
        return Items(syntax, "children").Where(child => Has(child, "rule"))
            .SelectMany(child => NativeSqlReferences(child, rule)).ToList();
    }

    private static readonly string[] SqlOperations = ["DECLARE", "SELECT", "INSERT", "UPDATE", "DELETE", "OPEN", "FETCH", "CLOSE", "COMMIT", "ROLLBACK"];

    internal static bool NativeSqlStatementBound(JsonNode? statement)
    {
        var syntax = Get(statement, "semanticSyntax");
        if (Str(statement, "kind") != "EXEC_SQL" || Str(statement, "grammarRule") != "SqlCodeContext"
            || Str(statement, "bindingStatus") != "complete" || Str(statement, "semanticStatus") != "complete"
            || syntax is not JsonObject || Str(syntax, "rule") != "StartSqlRuleContext"
            || PyJson.Truthy(Get(statement, "bindingDiagnostics")))
            return false;
        var nodes = SyntaxNodes(syntax);
        if (nodes.Count == 0) return false;
        var tokens = Tokens(nodes);
        var operation = Str(statement, "operation");
        if (operation is null || !SqlOperations.Contains(operation) || tokens.Count == 0 || tokens[0].ToUpperInvariant() != operation)
            return false;
        var references = NativeSqlReferences(syntax);
        if (!PyJson.PyEquals(Get(statement, "resourceReferences"), new JsonArray(references.Select(item => (JsonNode?)item.DeepClone()).ToArray())))
            return false;
        if (operation == "DECLARE" && !references.Any(reference => Str(reference, "role") == "declaration")) return false;
        if (Get(statement, "resourceBindings") is not JsonArray bindings || bindings.Any(binding => binding is not JsonObject))
            return false;
        var required = references.Where(reference => Str(reference, "role") == "reference")
            .Select(reference => PyKey(Get(reference, "kind")) + "\0" + PyKey(Get(reference, "name"))).ToHashSet();
        if (!bindings.Select(binding => PyKey(Get(binding, "kind")) + "\0" + PyKey(Get(binding, "name"))).ToHashSet().SetEquals(required))
            return false;
        foreach (var binding in bindings)
        {
            var definition = Get(binding, "definitionSyntax");
            var scope = Str(binding, "definitionScope");
            if (scope is not ("expanded-parser-input" or "explicit-sql-catalog") || definition is not JsonObject
                || SyntaxNodes(definition).Count == 0)
                return false;
            if (scope == "explicit-sql-catalog" && (Str(binding, "kind") != "SQL_RELATION"
                    || !IsAbsolute(Str(binding, "sourcePath")) || !IsHex64(Get(binding, "sourceSha256"))))
                return false;
            var kind = Str(binding, "kind");
            if (kind == "SQL_HOST_VARIABLE")
            {
                if (Str(definition, "rule") is not ("DataDescriptionEntryFormat1Context" or "DataDescriptionEntryFormat1Level77Context"))
                    return false;
                var names = SyntaxNodes(definition).Where(entry => Str(entry, "rule") == "EntryNameContext")
                    .Select(entry => string.Concat(Tokens(SyntaxNodes(entry))).ToUpperInvariant()).ToList();
                if (names.Count != 1 || !StrEquals(Get(binding, "name"), names[0])) return false;
            }
            else if (kind is "SQL_RELATION" or "SQL_CURSOR")
            {
                if (Str(definition, "rule") != (kind == "SQL_RELATION" ? "Dbs_declare_tableContext" : "Dbs_declare_cursorContext"))
                    return false;
                var declarationName = Str(binding, "name");
                if (Has(binding, "qualification"))
                {
                    var qualification = Get(binding, "qualification");
                    if (kind != "SQL_RELATION" || !NativeProfileReference(qualification)
                        || Full("[A-Z][A-Z0-9_]*", declarationName) is null
                        || Full("[A-Z][A-Z0-9_]*", Str(qualification, "schema")) is null)
                        return false;
                    declarationName = Str(qualification, "schema") + "." + declarationName;
                }
                if (!NativeSqlReferences(definition).Any(reference => Str(reference, "kind") == kind
                        && Str(reference, "name") == declarationName && Str(reference, "role") == "declaration"))
                    return false;
            }
            else
                return false;
        }
        return true;
    }

    private static readonly IReadOnlyDictionary<string, string> NativeFileRules = new Dictionary<string, string>
    {
        ["OPEN"] = "OpenStatementContext", ["CLOSE"] = "CloseStatementContext", ["READ"] = "ReadStatementContext",
        ["WRITE"] = "WriteStatementContext", ["REWRITE"] = "RewriteStatementContext",
        ["DELETE"] = "DeleteStatementContext", ["START"] = "StartStatementContext",
    };

    // A native file statement must bind every operand to a parsed SELECT entry and FD.
    internal static bool NativeFileStatementBound(JsonNode? statement)
    {
        var rule = Str(statement, "operation") is { } operation && NativeFileRules.TryGetValue(operation, out var found) ? found : null;
        var syntax = Get(statement, "syntax");
        if (rule is null || Str(statement, "grammarRule") != rule || Str(statement, "bindingStatus") != "complete"
            || Str(statement, "sourceText") is not { } text || text.Trim().Length == 0
            || syntax is not JsonObject || Str(syntax, "rule") != rule || SyntaxNodes(syntax).Count == 0
            || Get(statement, "fileOperands") is not JsonArray { Count: > 0 } operands || !operands.All(PyJson.IsStr)
            || Get(statement, "resourceBindings") is not JsonArray { Count: > 0 } bindings)
            return false;
        foreach (var binding in bindings)
        {
            if (binding is not JsonObject || Str(binding, "kind") != "COBOL_FILE"
                || Str(binding, "definitionScope") != "expanded-parser-input"
                || Str(binding, "name") is not { } name || string.IsNullOrEmpty(Str(binding, "assignment"))
                || Get(binding, "records") is not JsonArray)
                return false;
            var definition = Get(binding, "definitionSyntax");
            var description = Get(binding, "descriptionSyntax");
            if (definition is not JsonObject || Str(definition, "rule") != "FileControlEntryContext"
                || description is not JsonObject || Str(description, "rule") != "FileDescriptionEntryClausesContext")
                return false;
            var declared = SyntaxNodes(definition).Where(node => Str(node, "rule") == "FileNameContext").ToList();
            var described = SyntaxNodes(description).Where(node => Str(node, "rule") == "CobolWordContext").ToList();
            if (declared.Count == 0 || described.Count == 0
                || string.Concat(Tokens(SyntaxNodes(declared[0]))).ToUpperInvariant() != name
                || string.Concat(Tokens(SyntaxNodes(described[0]))).ToUpperInvariant() != name)
                return false;
        }
        var names = bindings.Select(binding => Str(binding, "name")).ToHashSet();
        var records = bindings.SelectMany(binding => Items(binding, "records")).Select(PyJson.Str).OfType<string>()
            .Select(record => record.ToUpperInvariant()).ToHashSet();
        return operands.Select(PyJson.Str).All(operand => names.Contains(operand!.ToUpperInvariant()) || records.Contains(operand!.ToUpperInvariant()));
    }

    // document.get(key, default) where a present null stays null.
    internal static JsonNode? GetOr(JsonNode? node, string key, JsonNode fallback) => Has(node, key) ? Get(node, key) : fallback;

    internal static bool NativeDomainEvidence(JsonNode? document, string kind)
    {
        var absence = GetOr(document, "dependencyAbsenceEvidence", new JsonObject());
        var presence = GetOr(document, "dependencyPresenceEvidence", new JsonObject());
        var statementsNode = GetOr(document, "dependencyDialectStatements", new JsonArray());
        if (absence is not JsonObject absent || presence is not JsonObject || statementsNode is not JsonArray statementArray
            || statementArray.Any(statement => statement is not JsonObject
                || Str(statement, "kind") is not ("EXEC_CICS" or "EXEC_SQL" or "FILE_IO")))
            return false;
        var statements = statementArray.ToList();
        var matching = statements.Where(statement => Str(statement, "kind") == kind).ToList();
        var nativeFiles = statements.Where(statement => Str(statement, "kind") == "FILE_IO").ToList();
        if (nativeFiles.Count > 0 && !nativeFiles.All(NativeFileStatementBound)) return false;
        if (!AbsenceProofs.TryGetValue(kind, out var absenceProof)) return true;
        var boundFiles = statements.Where(statement => Str(statement, "kind") == "EXEC_CICS"
            && GetOr(statement, "resourceBindings", new JsonArray()) is JsonArray resources
            && resources.Any(binding => binding is JsonObject && Str(binding, "kind") == "CICS_FILE")).ToList();
        var present = Str(presence, kind);
        var hasSql = statements.Any(statement => Str(statement, "kind") == "EXEC_SQL");
        bool CicsBound() => statements.Where(statement => Str(statement, "kind") == "EXEC_CICS").All(NativeCicsStatementBound);
        if (kind == "FILE_IO" && present == "antlr-tree:bound-file-control-resources")
            return !absent.ContainsKey(kind) && nativeFiles.Count > 0 && boundFiles.Count == 0
                && (!hasSql || NativeDomainEvidence(document, "EXEC_SQL")) && CicsBound();
        if (kind == "FILE_IO" && present == "antlr-tree:bound-cics-file-resources")
            return !absent.ContainsKey(kind) && boundFiles.Count > 0 && nativeFiles.Count == 0
                && (!hasSql || NativeDomainEvidence(document, "EXEC_SQL"))
                && statements.All(statement => Str(statement, "kind") == "EXEC_CICS"
                    ? NativeCicsStatementBound(statement) : NativeSqlStatementBound(statement));
        if (kind == "FILE_IO" && present == "antlr-tree:bound-sql-resources")
            return !absent.ContainsKey(kind) && boundFiles.Count == 0 && nativeFiles.Count == 0
                && NativeDomainEvidence(document, "EXEC_SQL") && CicsBound();
        if (Str(absent, kind) == absenceProof)
        {
            if (kind == "FILE_IO" && statements.Any(statement => SyntaxNodes(Get(statement, "syntax"))
                    .Any(node => Str(node, "rule") == "Cics_file_nameContext")))
                return false;
            if (kind == "FILE_IO" && hasSql) return false;
            return matching.Count == 0 && (kind == "FILE_IO" || statements.Count == 0);
        }
        var perDomain = kind switch { "EXEC_SQL" => "antlr-tree:no-sql-contexts", "EXEC_CICS" => "antlr-tree:no-cics-contexts", _ => null };
        if (perDomain is not null && Str(absent, kind) == perDomain)
            return statements.Count > 0 && matching.Count == 0;
        if (kind == "EXEC_SQL" && !absent.ContainsKey(kind) && matching.Count > 0 && present == "antlr-tree:bound-sql-resources")
        {
            var declarations = matching.SelectMany(statement => SyntaxNodes(Get(statement, "semanticSyntax")))
                .Where(node => Str(node, "rule") is "Dbs_declare_tableContext" or "Dbs_declare_cursorContext").ToList();
            return matching.All(NativeSqlStatementBound)
                && matching.SelectMany(statement => Items(statement, "resourceBindings"))
                    .Where(binding => Str(binding, "kind") is "SQL_RELATION" or "SQL_CURSOR")
                    .All(binding => declarations.Any(declaration => PyJson.PyEquals(Get(binding, "definitionSyntax"), declaration))
                        || Str(binding, "definitionScope") == "explicit-sql-catalog" && Str(binding, "kind") == "SQL_RELATION");
        }
        if (kind != "EXEC_CICS" || absent.ContainsKey(kind) || matching.Count == 0) return false;
        if (present == "antlr-tree:bound-cics-resources") return matching.All(NativeCicsStatementBound);
        if (present != "antlr-tree:resource-free-cics-return") return false;
        foreach (var statement in matching)
        {
            var syntax = Get(statement, "syntax");
            if (Str(statement, "operation") != "RETURN" || Str(statement, "grammarRule") != "Cics_returnContext"
                || Str(statement, "bindingStatus") != "complete"
                || Str(statement, "sourceText") is not { } text || text.Trim().ToUpperInvariant() != "RETURN"
                || syntax is not JsonObject || Str(syntax, "rule") != "Cics_returnContext")
                return false;
            if (Get(syntax, "children") is not JsonArray { Count: 1 } children || !KeysAre(children[0], "token")
                || Str(children[0], "token") is not { } token || token.ToUpperInvariant() != "RETURN")
                return false;
        }
        return true;
    }
}
