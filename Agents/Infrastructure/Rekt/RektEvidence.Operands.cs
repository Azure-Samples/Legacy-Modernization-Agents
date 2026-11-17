using System.Text.Json.Nodes;
using System.Text.RegularExpressions;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Rekt;

// CICS name operands are only trusted when the value is a literal or the native parser proved which literal
// values can reach it; each proof is re-derived here from the syntax it cites.
public static partial class RektEvidence
{
    private const string Level = "(0?[1-9]|[1-4][0-9])";
    private const string NamePattern = "([A-Za-z][A-Za-z0-9-]*)";
    private static readonly string[] ConstantOperandKinds = ["immutable-working-storage-literal", "immutable-local-storage-literal"];
    internal const string ConstantOperandProof = "single-native-program:read-only-constant-references";
    private const string TableOperandProof = "single-native-program:read-only-table-references";
    private const string ReachingOperandProof = "single-native-program:structured-reaching-definitions";
    private const string StraightLineOperandProof = "single-native-program:paragraph-straight-line-assignment";
    private const string SlotLocalProof = "single-native-program:flow-insensitive-slot-writes";
    private const string SlotClosureProof = "estate-closed-world:flow-insensitive-commarea-slot-closure";
    private static readonly Dictionary<string, string> SlotKinds = new()
    {
        ["flow-insensitive-slot-writes"] = SlotLocalProof, ["closed-world-commarea-slot"] = SlotClosureProof,
    };

    internal static string? CicsLiteral(JsonNode? operand, int minimum, int maximum)
    {
        var match = Full("""\((['"])([A-Za-z0-9@$#_-]+)\1\)""", SyntaxText(operand));
        return match is not null && minimum <= match.Groups[2].Length && match.Groups[2].Length <= maximum
            ? match.Groups[2].Value : null;
    }

    private static bool IsLiteralResolution(JsonNode? resolution) =>
        resolution is null || PyJson.PyEquals(resolution, new JsonObject { ["kind"] = "literal" });

    // Value of a CICS name operand: a literal, or a VALUE proved read-only by the native parser.
    internal static string? CicsConstantOperand(JsonNode? operand, JsonNode? resolution, int minimum, int maximum)
    {
        var literal = CicsLiteral(operand, minimum, maximum);
        if (literal is not null) return IsLiteralResolution(resolution) ? literal : null;
        var name = Full(@"\(([A-Za-z][A-Za-z0-9-]*)\)", SyntaxText(operand));
        if (name is null || !KeysAre(resolution, "kind", "operand", "definitionSyntax", "ancestorSyntax", "usageProof")
            || !ConstantOperandKinds.Contains(Str(resolution, "kind")) || Str(resolution, "operand") != name.Groups[1].Value
            || Str(resolution, "usageProof") != ConstantOperandProof)
            return null;
        var definition = Get(resolution, "definitionSyntax");
        if (definition is not JsonObject || Get(resolution, "ancestorSyntax") is not JsonArray ancestors
            || Str(definition, "rule") is not ("DataDescriptionEntryFormat1Context" or "DataDescriptionEntryFormat1Level77Context"))
            return null;
        var declaration = Full("(0?[1-9]|[1-4][0-9]|77)" + Regex.Escape(name.Groups[1].Value)
            + """PIC(?:TURE)?(?:IS)?X\(0*([1-9][0-9]?)\)VALUE(?:IS)?(['"])([A-Za-z0-9@$#_-]+)( *)\3\.""",
            SyntaxText(definition), ignoreCase: true);
        if (declaration is null || declaration.Groups[4].Length < minimum || declaration.Groups[4].Length > maximum
            || declaration.Groups[4].Length + declaration.Groups[5].Length > Int(declaration, 2))
            return null;
        var levels = new List<int>();
        foreach (var ancestor in ancestors)
        {
            var group = Str(ancestor, "rule") == "DataDescriptionEntryFormat1Context"
                ? Full(@"(0?[1-9]|[1-4][0-9])[A-Za-z][A-Za-z0-9-]*\.", SyntaxText(ancestor)) : null;
            if (group is null) return null;
            levels.Add(Int(group, 1));
        }
        var level = Int(declaration, 1);
        var value = declaration.Groups[4].Value;
        if (level is 1 or 77) return levels.Count == 0 ? value : null;
        var chain = levels.Append(level).ToList();
        return levels.Count > 0 && levels[0] == 1 && chain.Zip(chain.Skip(1)).All(pair => pair.First < pair.Second) ? value : null;
    }

    private static bool AscendingGroups(JsonNode? ancestors, List<int> levels, List<string>? groups)
    {
        foreach (var ancestor in (JsonArray)ancestors!)
        {
            var group = Str(ancestor, "rule") == "DataDescriptionEntryFormat1Context"
                ? Full(Level + NamePattern + @"\.", SyntaxText(ancestor)) : null;
            if (group is null || Int(group, 1) <= (levels.Count > 0 ? levels[^1] : 0) || levels.Count == 0 && Int(group, 1) != 1)
                return false;
            levels.Add(Int(group, 1));
            groups?.Add(group.Groups[2].Value.ToUpperInvariant());
        }
        return true;
    }

    // Per-occurrence values of NAME(SUBSCRIPT) over a REDEFINES of literal VALUE items; null marks storage outside them.
    internal static List<string?>? CicsTableOperand(JsonNode? operand, JsonNode? resolution, int minimum, int maximum)
    {
        static bool Entry(JsonNode? syntax) =>
            Str(syntax, "rule") == "DataDescriptionEntryFormat1Context" && SyntaxNodes(syntax).Count > 0;
        string[] lists = ["ancestorSyntax", "memberSyntax", "valueSyntax"];
        var operandText = Full(@"\(" + NamePattern + @"\(" + NamePattern + @"\)\)", SyntaxText(operand));
        if (operandText is null || !KeysAre(resolution, "kind", "operand", "subscript", "definitionSyntax", "ancestorSyntax",
                "redefiningSyntax", "occursSyntax", "memberSyntax", "redefinedSyntax", "valueSyntax", "occurrenceValues", "usageProof")
            || Str(resolution, "kind") != "immutable-redefined-table-values" || Str(resolution, "usageProof") != TableOperandProof
            || Str(resolution, "operand") != operandText.Groups[1].Value || Str(resolution, "subscript") != operandText.Groups[2].Value
            || !lists.All(key => Get(resolution, key) is JsonArray)
            || !lists.All(key => Arr(resolution, key)!.All(Entry))
            || !new[] { "definitionSyntax", "redefiningSyntax", "occursSyntax", "redefinedSyntax" }.All(key => Entry(Get(resolution, key)))
            || Arr(resolution, "ancestorSyntax")!.Count == 0 || Arr(resolution, "valueSyntax")!.Count == 0)
            return null;
        var levels = new List<int>();
        if (!AscendingGroups(Get(resolution, "ancestorSyntax"), levels, null)) return null;
        var redefining = Full(Level + NamePattern + "REDEFINES" + NamePattern + @"\.", SyntaxText(Get(resolution, "redefiningSyntax")), true);
        var occurs = Full(Level + NamePattern + @"OCCURS0*([1-9][0-9]{0,3})(?:TIMES)?\.", SyntaxText(Get(resolution, "occursSyntax")), true);
        var redefined = Full(Level + NamePattern + @"\.", SyntaxText(Get(resolution, "redefinedSyntax")));
        if (redefining is null || occurs is null || redefined is null || Int(redefining, 1) <= levels[^1]
            || Int(occurs, 1) <= Int(redefining, 1)
            || redefined.Groups[1].Value.TrimStart('0') != redefining.Groups[1].Value.TrimStart('0')
            || redefined.Groups[2].Value.ToUpperInvariant() != redefining.Groups[3].Value.ToUpperInvariant())
            return null;
        int size = 0, width = 0;
        int? offset = null;
        foreach (var member in Arr(resolution, "memberSyntax")!)
        {
            var shape = Full(Level + NamePattern + @"PIC(?:TURE)?(?:IS)?([X9])\(0*([1-9][0-9]{0,3})\)\.", SyntaxText(member), true);
            if (shape is null || Int(shape, 1) <= Int(occurs, 1)) return null;
            if (PyJson.PyEquals(member, Get(resolution, "definitionSyntax")))
            {
                if (shape.Groups[2].Value.ToUpperInvariant() != operandText.Groups[1].Value.ToUpperInvariant()
                    || shape.Groups[3].Value.ToUpperInvariant() != "X" || offset is not null)
                    return null;
                offset = size;
                width = Int(shape, 4);
            }
            size += Int(shape, 4);
        }
        if (offset is null) return null;
        var area = new System.Text.StringBuilder();
        foreach (var item in Arr(resolution, "valueSyntax")!)
        {
            var body = SyntaxText(item);
            var alphanumeric = Full(Level + @"(?:[A-Za-z][A-Za-z0-9-]*)?PIC(?:TURE)?(?:IS)?X\(0*([1-9][0-9]{0,3})\)"
                + """VALUE(?:IS)?(['"])([^'"]*)\3\.""", body, true);
            var numeric = Full(Level + @"(?:[A-Za-z][A-Za-z0-9-]*)?PIC(?:TURE)?(?:IS)?9\(0*([1-9][0-9]?)\)VALUE(?:IS)?([0-9]+)\.", body, true);
            if (alphanumeric is not null && alphanumeric.Groups[4].Length <= Int(alphanumeric, 2) && Int(alphanumeric, 1) > Int(redefined, 1))
                area.Append(alphanumeric.Groups[4].Value.PadRight(Int(alphanumeric, 2)));
            else if (numeric is not null && numeric.Groups[3].Length <= Int(numeric, 2) && Int(numeric, 1) > Int(redefined, 1))
                area.Append(numeric.Groups[3].Value.PadLeft(Int(numeric, 2), '0'));
            else return null;
        }
        var text = area.ToString();
        var values = new List<string?>();
        for (var index = 0; index < Int(occurs, 3); index++)
        {
            var start = index * size + offset.Value;
            if (start >= text.Length)
            {
                values.Add(null);
                continue;
            }
            var value = RStrip(text.Substring(start, Math.Min(width, text.Length - start)));
            if (start + width > text.Length || Full("[A-Za-z0-9@$#]+", value) is null || value.Length < minimum || value.Length > maximum)
                return null;
            values.Add(value);
        }
        return PyJson.PyEquals(ToArray(values), Get(resolution, "occurrenceValues")) && values.Any(value => !string.IsNullOrEmpty(value))
            ? values : null;
    }

    // Values the native parser proved can reach an XCTL operand from structured MOVE/INITIALIZE statements.
    internal static List<string?>? CicsReachingOperand(JsonNode? operand, JsonNode? resolution, int minimum, int maximum)
    {
        var name = Full(@"\(" + NamePattern + @"\)", SyntaxText(operand));
        var proof = Str(resolution, "usageProof");
        if (name is null || !KeysAre(resolution, "kind", "operand", "definitionSyntax", "ancestorSyntax", "assignments",
                "initialReaches", "reachingValues", "usageProof")
            || Str(resolution, "kind") != "structured-reaching-assignments"
            || proof is not (ReachingOperandProof or StraightLineOperandProof)
            || proof == StraightLineOperandProof && (!PyJson.IsBool(Get(resolution, "initialReaches"))
                || PyJson.IsTrue(Get(resolution, "initialReaches"))
                || Arr(resolution, "assignments") is not { Count: 1 })
            || Str(resolution, "operand") != name.Groups[1].Value || Get(resolution, "ancestorSyntax") is not JsonArray
            || Get(resolution, "assignments") is not JsonArray assignments || !PyJson.IsBool(Get(resolution, "initialReaches"))
            || Get(resolution, "reachingValues") is not JsonArray
            || Get(resolution, "definitionSyntax") is not JsonObject definitionSyntax
            || Str(definitionSyntax, "rule") is not ("DataDescriptionEntryFormat1Context" or "DataDescriptionEntryFormat1Level77Context"))
            return null;
        var definition = Full("(0?[1-9]|[1-4][0-9]|77)" + Regex.Escape(name.Groups[1].Value)
            + """PIC(?:TURE)?(?:IS)?X\(0*([1-9][0-9]{0,3})\)(?:VALUE(?:IS)?(['"])([^'"]*)\3)?\.""",
            SyntaxText(definitionSyntax), true);
        if (definition is null) return null;
        var groups = new List<string> { name.Groups[1].Value.ToUpperInvariant() };
        var levels = new List<int>();
        if (!AscendingGroups(Get(resolution, "ancestorSyntax"), levels, groups)) return null;
        if ((Int(definition, 1) is 1 or 77) != (levels.Count == 0) || levels.Count > 0 && Int(definition, 1) <= levels[^1])
            return null;
        string? Program(string literal)
        {
            var value = RStrip(literal);
            if (value.Length == 0) return null;
            if (Full("[A-Za-z0-9@$#]+", value) is null || value.Length < minimum || value.Length > maximum
                || value.Length > Int(definition, 2))
                throw new Rejected();
            return value;
        }
        var alternatives = string.Join("|", groups.Select(Regex.Escape));
        var qualifier = "(?:(?:OF|IN)(?:" + alternatives + "))*";
        var target = "(?:" + alternatives + ")" + qualifier;
        var values = new List<string?>();
        try
        {
            foreach (var assignment in assignments)
            {
                if (assignment is not JsonObject || !KeysWithin(assignment, "syntax", "sourceResolution")
                    || Get(assignment, "syntax") is not JsonObject syntax)
                    return null;
                var rule = Str(syntax, "rule");
                var body = SyntaxText(syntax);
                var own = Regex.Escape(name.Groups[1].Value) + qualifier;
                var literal = Full("""MOVE(['"])([^'"]*)\1TO""" + own, body, true);
                var figurative = Full("MOVE(?:SPACES?|LOW-VALUES?)TO" + target, body, true);
                var initialize = Full("INITIALIZE" + target, body, true);
                var constant = Full("MOVE" + NamePattern + "TO" + own, body, true);
                var resolved = Has(assignment, "sourceResolution");
                if (rule == "MoveStatementContext" && literal is not null && !resolved)
                    values.Add(Program(literal.Groups[2].Value));
                else if ((rule == "MoveStatementContext" && figurative is not null
                          || rule == "InitializeStatementContext" && initialize is not null) && !resolved)
                    values.Add(null);
                else if (rule == "MoveStatementContext" && constant is not null && resolved)
                {
                    var value = CicsConstantOperand(NameOperand(constant.Groups[1].Value), Get(assignment, "sourceResolution"), minimum, maximum);
                    if (value is null || value.Length > Int(definition, 2)) return null;
                    values.Add(value);
                }
                else return null;
            }
            if (PyJson.IsTrue(Get(resolution, "initialReaches")))
                values.Add(definition.Groups[4].Success ? Program(definition.Groups[4].Value) : null);
        }
        catch (Rejected)
        {
            return null;
        }
        var reaching = values.Distinct().ToList();
        return PyJson.PyEquals(ToArray(reaching), Get(resolution, "reachingValues")) && reaching.Any(value => !string.IsNullOrEmpty(value))
            ? reaching : null;
    }

    private static JsonObject NameOperand(string name) => new()
    {
        ["rule"] = "Cics_nameContext",
        ["children"] = new JsonArray(new JsonObject { ["token"] = "(" }, new JsonObject { ["token"] = name },
            new JsonObject { ["token"] = ")" }),
    };

    // Universe of a PIC X slot from every write the native parser listed; closure proofs are rechecked separately.
    internal static List<string?>? CicsSlotOperand(JsonNode? operand, JsonNode? resolution, int minimum, int maximum)
    {
        var name = Full(@"\(" + NamePattern + @"\)", SyntaxText(operand));
        var kind = Str(resolution, "kind");
        if (name is null || resolution is not JsonObject || kind is null || !SlotKinds.ContainsKey(kind))
            return null;
        string[] keys = ["kind", "operand", "record", "definitionSyntax", "ancestorSyntax", "slots", "universe", "usageProof"];
        if (kind == "closed-world-commarea-slot") keys = [.. keys, "closureSha256", "programSourceSha256"];
        var upperName = name.Groups[1].Value.ToUpperInvariant();
        if (!KeysAre(resolution, keys) || Str(resolution, "usageProof") != SlotKinds[kind]
            || Str(resolution, "operand") != upperName || Get(resolution, "slots") is not JsonObject slots
            || Get(resolution, "universe") is not JsonArray universeRecord || Get(resolution, "ancestorSyntax") is not JsonArray { Count: > 0 }
            || !slots.ContainsKey(upperName))
            return null;
        var groups = new List<string>();
        if (!AscendingGroups(Get(resolution, "ancestorSyntax"), [], groups)) return null;
        if (groups[0] != PyJson.PyStr(Get(resolution, "record")).ToUpperInvariant()) return null;
        string? Program(JsonNode? value)
        {
            if (value is null || PyJson.Scalar(value) is null) return null;
            if (PyJson.Str(value) is not { } text || Full("[A-Za-z0-9@$#]+", text) is null || text.Length < minimum || text.Length > maximum)
                throw new Rejected();
            return text;
        }
        const string slotPattern = @"(0?[2-9]|[1-4][0-9])([A-Z0-9-]+)PIC(?:TURE)?(?:IS)?(?:X\(0*([1-8])\)|(X{1,8}))"
            + """(?:VALUE(?:IS)?(?:(['"])([^'"]*)\5|(SPACES?|LOW-VALUES?)))?\.""";
        bool Initialized(JsonNode syntax, string slotName)
        {
            var tokens = SyntaxNodes(syntax).Where(node => node.ContainsKey("token"))
                .Select(node => PyJson.Str(node["token"])!.ToUpperInvariant()).ToList();
            if (tokens.Count == 0 || tokens[0] != "INITIALIZE"
                || tokens.Any(token => token is "REPLACING" or "VALUE" or "DEFAULT" or "FILLER" or "(" or ":"))
                return false;
            var operands = new List<List<string>>();
            var index = 1;
            while (index < tokens.Count)
            {
                if (tokens[index] == ",") { index++; continue; }
                var path = new List<string> { tokens[index] };
                index++;
                while (index + 1 < tokens.Count && tokens[index] is "OF" or "IN")
                {
                    path.Add(tokens[index + 1]);
                    index += 2;
                }
                operands.Add(path);
            }
            return operands.Any(path => (groups.Contains(path[0]) || path[0] == slotName) && path.Skip(1).All(groups.Contains));
        }
        var record = Regex.Escape(groups[0]);
        var entryBody = @"MOVEDFHCOMMAREA\(1:(?:EIBCALEN|LENGTHOF" + record + @")\)TO" + record;
        var values = new Dictionary<string, List<string?>>();
        var sources = new Dictionary<string, List<string>>();
        var entrySeen = new Dictionary<string, bool>();
        try
        {
            foreach (var (slotName, slot) in slots)
            {
                // Gson omits null map values, so an absent initialValue means the slot starts as SPACES.
                if (slot is not JsonObject || !new[] { "definitionSyntax", "entry", "writes" }.All(key => Has(slot, key))
                    || !KeysWithin(slot, "definitionSyntax", "initialValue", "entry", "writes")
                    || Get(slot, "writes") is not JsonArray writes || !PyJson.IsBool(Get(slot, "entry"))
                    || Get(slot, "definitionSyntax") is not JsonObject definition
                    || Str(definition, "rule") != "DataDescriptionEntryFormat1Context")
                    return null;
                var shape = Full(slotPattern, SyntaxText(definition), true);
                if (shape is null || shape.Groups[2].Value.ToUpperInvariant() != slotName) return null;
                var initialText = RStrip(Group(shape, 6) ?? "");
                var initial = initialText.Length == 0 ? null : initialText;
                if (!StrEquals(Get(slot, "initialValue"), initial)) return null;
                var alternatives = string.Join("|", groups.Select(Regex.Escape));
                var own = Regex.Escape(slotName) + "(?:(?:OF|IN)(?:" + alternatives + "))*";
                var holders = "(?:" + own + "|" + alternatives + ")";
                var found = new List<string?> { Program(initial) };
                var copies = new List<string>();
                var entered = false;
                foreach (var write in writes)
                {
                    if (write is not JsonObject || Get(write, "syntax") is not JsonObject syntax) return null;
                    var rule = Str(syntax, "rule");
                    var body = SyntaxText(syntax);
                    var effect = Str(write, "effect");
                    if (!KeysWithin(write, "effect", "value", "source", "line", "syntax", "sourceResolution")) return null;
                    var literal = Full("""MOVE(['"])([^'"]*)\1TO""" + own, body, true);
                    var constant = Full("MOVE" + NamePattern + "TO" + own, body, true);
                    var resolved = Has(write, "sourceResolution");
                    if (effect == "entry" && rule == "MoveStatementContext" && Full(entryBody, body, true) is not null)
                        entered = true;
                    else if (effect == "null" && !resolved
                             && (rule == "MoveStatementContext" && Full("MOVE(?:SPACES?|LOW-VALUES?)TO" + holders, body, true) is not null
                                 || rule == "InitializeStatementContext" && Initialized(syntax, slotName)))
                        found.Add(null);
                    else if (effect == "value" && literal is not null && !resolved)
                    {
                        var stripped = RStrip(literal.Groups[2].Value);
                        if (!StrEquals(Get(write, "value"), stripped.Length == 0 ? null : stripped)) return null;
                        found.Add(Program(Get(write, "value")));
                    }
                    else if (effect == "value" && constant is not null && resolved)
                    {
                        var value = CicsConstantOperand(NameOperand(constant.Groups[1].Value), Get(write, "sourceResolution"), minimum, maximum);
                        if (value is null || !StrEquals(Get(write, "value"), value)) return null;
                        found.Add(value);
                    }
                    else if (effect == "copy" && constant is not null && !resolved
                             && Str(write, "source") is { } source && constant.Groups[1].Value.ToUpperInvariant() == source
                             && slots.ContainsKey(source))
                        copies.Add(source);
                    else return null;
                }
                if (entered != PyJson.IsTrue(Get(slot, "entry"))) return null;
                values[slotName] = found;
                sources[slotName] = copies;
                entrySeen[slotName] = entered;
            }
        }
        catch (Rejected)
        {
            return null;
        }
        var universe = new List<string?>();
        var pending = new List<string> { upperName };
        var seen = new HashSet<string>();
        var dependsOnEntry = false;
        while (pending.Count > 0)
        {
            var current = pending[^1];
            pending.RemoveAt(pending.Count - 1);
            if (!seen.Add(current)) continue;
            dependsOnEntry = dependsOnEntry || entrySeen[current];
            universe.AddRange(values[current]);
            pending.AddRange(sources[current]);
        }
        if (!seen.SetEquals(slots.Select(pair => pair.Key))) return null;
        var mine = universe.Select(value => PyKey(value)).ToHashSet();
        var recorded = universeRecord.Select(PyKey).ToHashSet();
        if (kind == "flow-insensitive-slot-writes")
        {
            if (dependsOnEntry || !mine.SetEquals(recorded)) return null;
        }
        else if (!dependsOnEntry || !mine.IsSubsetOf(recorded)) return null;
        if (recorded.Count != universeRecord.Count) return null;
        try
        {
            return universeRecord.Any(PyJson.Truthy) ? universeRecord.Select(Program).ToList() : null;
        }
        catch (Rejected)
        {
            return null;
        }
    }
}
