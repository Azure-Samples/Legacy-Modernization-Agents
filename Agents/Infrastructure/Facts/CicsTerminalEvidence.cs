using System.Text.Json;
using System.Text.RegularExpressions;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Facts;

public sealed partial class DependencyEvidenceGate
{
    private static IEnumerable<JsonElement> CicsNodes(JsonElement syntax)
    {
        yield return syntax;
        if (syntax.TryGetProperty("children", out var children))
            foreach (var child in children.EnumerateArray())
                foreach (var node in CicsNodes(child))
                    yield return node;
    }

    private static List<JsonElement> CicsOperands(JsonElement syntax, string option)
    {
        var result = new List<JsonElement>();
        foreach (var node in CicsNodes(syntax))
        {
            if (!node.TryGetProperty("children", out var children)) continue;
            var items = children.EnumerateArray().ToArray();
            for (var index = 1; index < items.Length; index++)
                if (Text(items[index - 1], "token")?.ToUpperInvariant() == option
                    && Text(items[index], "rule") is not null)
                    result.Add(option == "MAP" && Text(items[index], "rule") == "Cics_receive_mapContext"
                        ? items[index].GetProperty("children")[0] : items[index]);
        }
        return result;
    }

    private static string CicsText(JsonElement syntax) =>
        string.Concat(CicsNodes(syntax).Select(node => Text(node, "token") ?? ""));

    private static string? CicsLiteral(JsonElement syntax, int minimum, int maximum)
    {
        var match = Regex.Match(CicsText(syntax), """^\((['"])([A-Za-z0-9@$#_-]+)\1\)$""");
        return match.Success && match.Groups[2].Length >= minimum && match.Groups[2].Length <= maximum
            ? match.Groups[2].Value : null;
    }

    // Value of a CICS name operand: a literal, or a VALUE that the native parser proved read-only.
    private static string? CicsConstantOperand(JsonElement operand, JsonElement? resolution, int minimum, int maximum)
    {
        var literal = CicsLiteral(operand, minimum, maximum);
        if (literal is not null)
            return resolution is null || resolution.Value.ValueKind == JsonValueKind.Object
                && resolution.Value.EnumerateObject().Count() == 1 && Text(resolution.Value, "kind") == "literal"
                ? literal : null;
        var name = Regex.Match(CicsText(operand), "^\\(([A-Za-z][A-Za-z0-9-]*)\\)$");
        if (!name.Success || resolution is not { ValueKind: JsonValueKind.Object } proof
            || !proof.EnumerateObject().Select(property => property.Name).Order()
                .SequenceEqual(new[] { "ancestorSyntax", "definitionSyntax", "kind", "operand", "usageProof" })
            || Text(proof, "kind") is not ("immutable-working-storage-literal" or "immutable-local-storage-literal")
            || Text(proof, "operand") != name.Groups[1].Value
            || Text(proof, "usageProof") != "single-native-program:read-only-constant-references"
            || !proof.TryGetProperty("definitionSyntax", out var definition) || !ValidSyntax(definition)
            || Text(definition, "rule") is not ("DataDescriptionEntryFormat1Context" or "DataDescriptionEntryFormat1Level77Context")
            || !proof.TryGetProperty("ancestorSyntax", out var ancestors) || ancestors.ValueKind != JsonValueKind.Array)
            return null;
        var declaration = Regex.Match(CicsText(definition), "^(0?[1-9]|[1-4][0-9]|77)" + Regex.Escape(name.Groups[1].Value)
            + """PIC(?:TURE)?(?:IS)?X\(0*([1-9][0-9]?)\)VALUE(?:IS)?(['"])([A-Za-z0-9@$#_-]+)( *)\3\.$""",
            RegexOptions.IgnoreCase);
        var value = declaration.Groups[4].Value;
        if (!declaration.Success || value.Length < minimum || value.Length > maximum
            || value.Length + declaration.Groups[5].Length > int.Parse(declaration.Groups[2].Value))
            return null;
        var levels = new List<int>();
        foreach (var ancestor in ancestors.EnumerateArray())
        {
            var group = ancestor.ValueKind == JsonValueKind.Object && ValidSyntax(ancestor)
                && Text(ancestor, "rule") == "DataDescriptionEntryFormat1Context"
                ? Regex.Match(CicsText(ancestor), "^(0?[1-9]|[1-4][0-9])[A-Za-z][A-Za-z0-9-]*\\.$") : Match.Empty;
            if (!group.Success) return null;
            levels.Add(int.Parse(group.Groups[1].Value));
        }
        var level = int.Parse(declaration.Groups[1].Value);
        if (level is 1 or 77) return levels.Count == 0 ? value : null;
        var chain = levels.Append(level).ToArray();
        return levels.Count > 0 && levels[0] == 1 && chain.Zip(chain.Skip(1)).All(pair => pair.First < pair.Second)
            ? value : null;
    }

    // Values the native parser proved reach a name operand through structured MOVE/INITIALIZE statements.
    private static List<string?>? CicsReachingOperand(JsonElement operand, JsonElement resolution, int minimum, int maximum)
    {
        var name = Regex.Match(CicsText(operand), "^\\(([A-Za-z][A-Za-z0-9-]*)\\)$");
        string[] keys = ["ancestorSyntax", "assignments", "definitionSyntax", "initialReaches", "kind", "operand",
            "reachingValues", "usageProof"];
        if (!name.Success || resolution.ValueKind != JsonValueKind.Object
            || !resolution.EnumerateObject().Select(property => property.Name).Order(StringComparer.Ordinal).SequenceEqual(keys)
            || Text(resolution, "kind") != "structured-reaching-assignments"
            || Text(resolution, "usageProof") is not ("single-native-program:structured-reaching-definitions"
                or "single-native-program:paragraph-straight-line-assignment")
            || Text(resolution, "operand") != name.Groups[1].Value
            || resolution.GetProperty("ancestorSyntax").ValueKind != JsonValueKind.Array
            || resolution.GetProperty("assignments").ValueKind != JsonValueKind.Array
            || resolution.GetProperty("initialReaches").ValueKind is not (JsonValueKind.True or JsonValueKind.False)
            || resolution.GetProperty("reachingValues").ValueKind != JsonValueKind.Array
            || resolution.GetProperty("definitionSyntax") is not { ValueKind: JsonValueKind.Object } definition
            || !ValidSyntax(definition)
            || Text(definition, "rule") is not ("DataDescriptionEntryFormat1Context" or "DataDescriptionEntryFormat1Level77Context"))
            return null;
        var declaration = Regex.Match(CicsText(definition), "^(0?[1-9]|[1-4][0-9]|77)" + Regex.Escape(name.Groups[1].Value)
            + """PIC(?:TURE)?(?:IS)?X\(0*([1-9][0-9]{0,3})\)(?:VALUE(?:IS)?(['"])([^'"]*)\3)?\.$""", RegexOptions.IgnoreCase);
        if (!declaration.Success) return null;
        if (Text(resolution, "usageProof") == "single-native-program:paragraph-straight-line-assignment"
            && (resolution.GetProperty("initialReaches").GetBoolean() || resolution.GetProperty("assignments").GetArrayLength() != 1))
            return null;
        var groups = new List<string> { name.Groups[1].Value.ToUpperInvariant() };
        var levels = new List<int>();
        foreach (var ancestor in resolution.GetProperty("ancestorSyntax").EnumerateArray())
        {
            var group = ancestor.ValueKind == JsonValueKind.Object && ValidSyntax(ancestor)
                && Text(ancestor, "rule") == "DataDescriptionEntryFormat1Context"
                ? Regex.Match(CicsText(ancestor), "^(0?[1-9]|[1-4][0-9])([A-Za-z][A-Za-z0-9-]*)\\.$") : Match.Empty;
            if (!group.Success) return null;
            var groupLevel = int.Parse(group.Groups[1].Value);
            if (groupLevel <= (levels.Count > 0 ? levels[^1] : 0) || levels.Count == 0 && groupLevel != 1) return null;
            levels.Add(groupLevel);
            groups.Add(group.Groups[2].Value.ToUpperInvariant());
        }
        var width = int.Parse(declaration.Groups[2].Value);
        var level = int.Parse(declaration.Groups[1].Value);
        if (level is 1 or 77 != (levels.Count == 0) || levels.Count > 0 && level <= levels[^1]) return null;
        bool Program(string literal, out string? value)
        {
            value = literal.TrimEnd(' ');
            if (value.Length == 0) { value = null; return true; }
            return Regex.IsMatch(value, "^[A-Za-z0-9@$#]+$") && value.Length >= minimum && value.Length <= maximum
                && value.Length <= width;
        }
        var qualifier = "(?:(?:OF|IN)(?:" + string.Join("|", groups.Select(Regex.Escape)) + "))*";
        var target = "(?:" + string.Join("|", groups.Select(Regex.Escape)) + ")" + qualifier;
        var own = Regex.Escape(name.Groups[1].Value) + qualifier;
        var values = new List<string?>();
        foreach (var assignment in resolution.GetProperty("assignments").EnumerateArray())
        {
            if (assignment.ValueKind != JsonValueKind.Object
                || assignment.EnumerateObject().Any(property => property.Name is not ("syntax" or "sourceResolution"))
                || !assignment.TryGetProperty("syntax", out var syntax) || syntax.ValueKind != JsonValueKind.Object)
                return null;
            var rule = Text(syntax, "rule");
            var body = CicsText(syntax);
            var sourced = assignment.TryGetProperty("sourceResolution", out var source);
            var literal = Regex.Match(body, """^MOVE(['"])([^'"]*)\1TO""" + own + "$", RegexOptions.IgnoreCase);
            var constant = Regex.Match(body, "^MOVE([A-Za-z][A-Za-z0-9-]*)TO" + own + "$", RegexOptions.IgnoreCase);
            if (rule == "MoveStatementContext" && literal.Success && !sourced)
            {
                if (!Program(literal.Groups[2].Value, out var value)) return null;
                values.Add(value);
            }
            else if ((rule == "MoveStatementContext" && Regex.IsMatch(body, "^MOVE(?:SPACES?|LOW-VALUES?)TO" + target + "$", RegexOptions.IgnoreCase)
                      || rule == "InitializeStatementContext" && Regex.IsMatch(body, "^INITIALIZE" + target + "$", RegexOptions.IgnoreCase))
                     && !sourced)
                values.Add(null);
            else if (rule == "MoveStatementContext" && constant.Success && sourced)
            {
                using var synthetic = JsonDocument.Parse(JsonSerializer.Serialize(new
                {
                    rule = "Cics_nameContext",
                    children = new object[] { new { token = "(" }, new { token = constant.Groups[1].Value }, new { token = ")" } }
                }));
                var value = CicsConstantOperand(synthetic.RootElement, source, minimum, maximum);
                if (value is null || value.Length > width) return null;
                values.Add(value);
            }
            else
                return null;
        }
        if (resolution.GetProperty("initialReaches").GetBoolean())
        {
            string? initial = null;
            if (declaration.Groups[4].Success && !Program(declaration.Groups[4].Value, out initial)) return null;
            values.Add(initial);
        }
        var reaching = values.Distinct().ToList();
        var recorded = resolution.GetProperty("reachingValues").EnumerateArray()
            .Select(item => item.ValueKind == JsonValueKind.Null ? null : item.ValueKind == JsonValueKind.String ? item.GetString() : "\0").ToList();
        return reaching.Any(value => value is not null) && recorded.SequenceEqual(reaching) ? reaching : null;
    }

    private static string CicsCompact(string text) =>
        string.Concat(Regex.Matches(text, """'(?:''|[^'])*'|"(?:""|[^"])*"|\S""")
            .Select(match => match.Value)).ToUpperInvariant();

    // Per-occurrence values of NAME(SUBSCRIPT) over a REDEFINES of literal VALUE items; null marks storage outside them.
    private static List<string?>? CicsTableOperand(JsonElement operand, JsonElement resolution, int minimum, int maximum)
    {
        const string level = "(0?[1-9]|[1-4][0-9])", name = "([A-Za-z][A-Za-z0-9-]*)";
        var operandText = Regex.Match(CicsText(operand), $"^\\({name}\\({name}\\)\\)$");
        string[] keys = ["ancestorSyntax", "definitionSyntax", "kind", "memberSyntax", "occurrenceValues", "occursSyntax",
            "operand", "redefinedSyntax", "redefiningSyntax", "subscript", "usageProof", "valueSyntax"];
        bool Entry(JsonElement item) => item.ValueKind == JsonValueKind.Object && ValidSyntax(item)
            && Text(item, "rule") == "DataDescriptionEntryFormat1Context";
        if (!operandText.Success || resolution.ValueKind != JsonValueKind.Object
            || !resolution.EnumerateObject().Select(property => property.Name).Order(StringComparer.Ordinal).SequenceEqual(keys)
            || Text(resolution, "kind") != "immutable-redefined-table-values"
            || Text(resolution, "usageProof") != "single-native-program:read-only-table-references"
            || Text(resolution, "operand") != operandText.Groups[1].Value || Text(resolution, "subscript") != operandText.Groups[2].Value
            || new[] { "ancestorSyntax", "memberSyntax", "valueSyntax" }.Any(key => resolution.GetProperty(key).ValueKind != JsonValueKind.Array
                || resolution.GetProperty(key).GetArrayLength() == 0 || resolution.GetProperty(key).EnumerateArray().Any(item => !Entry(item)))
            || new[] { "definitionSyntax", "redefiningSyntax", "occursSyntax", "redefinedSyntax" }.Any(key => !Entry(resolution.GetProperty(key)))
            || resolution.GetProperty("occurrenceValues").ValueKind != JsonValueKind.Array)
            return null;
        var previous = 0;
        foreach (var ancestor in resolution.GetProperty("ancestorSyntax").EnumerateArray())
        {
            var group = Regex.Match(CicsText(ancestor), $"^{level}{name}\\.$");
            if (!group.Success || int.Parse(group.Groups[1].Value) <= previous
                || previous == 0 && int.Parse(group.Groups[1].Value) != 1) return null;
            previous = int.Parse(group.Groups[1].Value);
        }
        var redefining = Regex.Match(CicsText(resolution.GetProperty("redefiningSyntax")), $"^{level}{name}REDEFINES{name}\\.$", RegexOptions.IgnoreCase);
        var occurs = Regex.Match(CicsText(resolution.GetProperty("occursSyntax")), $"^{level}{name}OCCURS0*([1-9][0-9]{{0,3}})(?:TIMES)?\\.$", RegexOptions.IgnoreCase);
        var redefined = Regex.Match(CicsText(resolution.GetProperty("redefinedSyntax")), $"^{level}{name}\\.$");
        if (!redefining.Success || !occurs.Success || !redefined.Success
            || int.Parse(redefining.Groups[1].Value) <= previous || int.Parse(occurs.Groups[1].Value) <= int.Parse(redefining.Groups[1].Value)
            || int.Parse(redefined.Groups[1].Value) != int.Parse(redefining.Groups[1].Value)
            || !string.Equals(redefined.Groups[2].Value, redefining.Groups[3].Value, StringComparison.OrdinalIgnoreCase))
            return null;
        int size = 0, offset = -1, width = 0;
        foreach (var member in resolution.GetProperty("memberSyntax").EnumerateArray())
        {
            var shape = Regex.Match(CicsText(member), $"^{level}{name}PIC(?:TURE)?(?:IS)?([X9])\\(0*([1-9][0-9]{{0,3}})\\)\\.$", RegexOptions.IgnoreCase);
            if (!shape.Success || int.Parse(shape.Groups[1].Value) <= int.Parse(occurs.Groups[1].Value)) return null;
            if (JsonElement.DeepEquals(member, resolution.GetProperty("definitionSyntax")))
            {
                if (!string.Equals(shape.Groups[2].Value, operandText.Groups[1].Value, StringComparison.OrdinalIgnoreCase)
                    || !shape.Groups[3].Value.Equals("X", StringComparison.OrdinalIgnoreCase) || offset >= 0) return null;
                offset = size;
                width = int.Parse(shape.Groups[4].Value);
            }
            size += int.Parse(shape.Groups[4].Value);
        }
        if (offset < 0) return null;
        var area = new System.Text.StringBuilder();
        foreach (var item in resolution.GetProperty("valueSyntax").EnumerateArray())
        {
            var body = CicsText(item);
            var text = Regex.Match(body, $"^{level}(?:[A-Za-z][A-Za-z0-9-]*)?PIC(?:TURE)?(?:IS)?X\\(0*([1-9][0-9]{{0,3}})\\)VALUE(?:IS)?(['\"])([^'\"]*)\\3\\.$", RegexOptions.IgnoreCase);
            var number = Regex.Match(body, $"^{level}(?:[A-Za-z][A-Za-z0-9-]*)?PIC(?:TURE)?(?:IS)?9\\(0*([1-9][0-9]?)\\)VALUE(?:IS)?([0-9]+)\\.$", RegexOptions.IgnoreCase);
            if (text.Success && text.Groups[4].Length <= int.Parse(text.Groups[2].Value)
                && int.Parse(text.Groups[1].Value) > int.Parse(redefined.Groups[1].Value))
                area.Append(text.Groups[4].Value.PadRight(int.Parse(text.Groups[2].Value)));
            else if (number.Success && number.Groups[3].Length <= int.Parse(number.Groups[2].Value)
                && int.Parse(number.Groups[1].Value) > int.Parse(redefined.Groups[1].Value))
                area.Append(number.Groups[3].Value.PadLeft(int.Parse(number.Groups[2].Value), '0'));
            else return null;
        }
        var values = new List<string?>();
        for (var index = 0; index < int.Parse(occurs.Groups[3].Value); index++)
        {
            var start = index * size + offset;
            if (start >= area.Length)
            {
                values.Add(null);
                continue;
            }
            if (start + width > area.Length) return null;
            var value = area.ToString(start, width).TrimEnd(' ');
            if (!Regex.IsMatch(value, "^[A-Za-z0-9@$#]+$") || value.Length < minimum || value.Length > maximum) return null;
            values.Add(value);
        }
        var recorded = resolution.GetProperty("occurrenceValues").EnumerateArray()
            .Select(item => item.ValueKind == JsonValueKind.Null ? null : item.ValueKind == JsonValueKind.String ? item.GetString() : "\0").ToList();
        return values.Any(value => value is not null) && recorded.SequenceEqual(values) ? values : null;
    }

    // SEND FROM(area) LENGTH|FLENGTH(n) [WAIT] [ERASE] [RESP/NOHANDLE]: a raw write to the principal facility.
    private static bool PlainSend(JsonElement syntax)
    {
        string[] tail = ["Cics_send_from_waitContext", "Cics_send_defaultmaxContext", "Cics_send_lu23Context",
            "Cics_send_from_ctlcharContext", "Cics_send_3560_3270Context", "Cics_send_eraseContext"];
        bool Tail(JsonElement node)
        {
            if (Text(node, "rule") == "Cics_handle_responseContext") return true;
            if (Text(node, "token") is { } token) return token.ToUpperInvariant() is "WAIT" or "ERASE";
            var children = node.TryGetProperty("children", out var items) ? items.EnumerateArray().ToArray() : [];
            return tail.Contains(Text(node, "rule"))
                && (Text(node, "rule") != "Cics_send_eraseContext" || children.Length == 1) && children.All(Tail);
        }
        var children = syntax.GetProperty("children").EnumerateArray().ToArray();
        if (children.Length != 2 || Text(children[1], "rule") != "Cics_send_groupContext") return false;
        var parts = children[1].GetProperty("children").EnumerateArray().ToArray();
        return parts.Length == 2 && Text(parts[0], "rule") == "Cics_send_fromContext" && Tail(parts[1]);
    }

    // HANDLE CONDITION name[(label)]...: labels are validated as CICS_LABEL bindings; no RESP/NOHANDLE/WAIT.
    private static bool HandleCondition(JsonElement syntax)
    {
        var children = syntax.GetProperty("children").EnumerateArray().ToArray();
        if (children.Length != 2 || Text(children[1], "rule") != "Cics_handle_conditionContext") return false;
        var parts = children[1].GetProperty("children").EnumerateArray().ToArray();
        return parts.Length > 1 && parts.Skip(1).All(part => Text(part, "rule") != "Cics_handle_responseContext");
    }

    private static void ValidateCicsTerminalEvidence(string identity, DependencyEvidence evidence,
        IReadOnlyList<RuntimeImplementationRequirement> requirements, string sourceRoot, List<string> issues)
    {
        foreach (var requirement in requirements.Where(requirement => requirement.Kind == "EXEC_CICS"))
        {
            var statement = requirement.Evidence;
            var rule = Text(statement, "grammarRule");
            if (rule is not ("Cics_sendContext" or "Cics_receiveContext" or "Cics_returnContext"
                or "Cics_inquireContext" or "Cics_set_terminalContext" or "Cics_bifContext" or "Cics_handleContext"))
                continue;
            var syntax = statement.GetProperty("syntax");
            var nodes = CicsNodes(syntax).ToArray();
            var rules = nodes.Select(node => Text(node, "rule")).ToHashSet();
            var tokens = nodes.Select(node => Text(node, "token")?.ToUpperInvariant()).OfType<string>().ToArray();
            if (rule == "Cics_returnContext" && tokens.SequenceEqual(["RETURN"]))
            {
                if (Text(statement, "sourceText")?.Trim().ToUpperInvariant() != "RETURN"
                    || statement.TryGetProperty("resourceBindings", out var bareBindings)
                    && (bareBindings.ValueKind != JsonValueKind.Array || bareBindings.GetArrayLength() > 0))
                    issues.Add($"{identity}: bare RETURN has inconsistent terminal evidence.");
                continue;
            }
            var supported = rule switch
            {
                "Cics_sendContext" => rules.Overlaps(["Cics_send_control_minContext", "Cics_send_map_minContext",
                    "Cics_send_text_stdContext"]) || PlainSend(syntax),
                "Cics_receiveContext" => rules.Contains("Cics_receive_mapContext")
                    && !rules.Contains("Cics_receive_mappingdevContext") && CicsOperands(syntax, "INPARTN").Count == 0,
                "Cics_returnContext" => !rules.Contains("Cics_return_inputmsgContext")
                    && !tokens.Intersect(["CHANNEL", "ENDACTIVITY"]).Any(),
                "Cics_inquireContext" => rules.Contains("Cics_inquire_programContext") && CicsOperands(syntax, "PROGRAM").Count == 1
                    || rules.Contains("Cics_inquire_terminalContext")
                    || rules.Contains("Cics_inquire_associationContext") && CicsOperands(syntax, "ASSOCIATION").Count == 1,
                "Cics_set_terminalContext" => true,
                "Cics_bifContext" => rules.Contains("Cics_bif_deeditContext") && CicsOperands(syntax, "FIELD").Count == 1,
                "Cics_handleContext" => HandleCondition(syntax) || rules.Contains("Cics_handle_abendContext")
                    && !tokens.Intersect(["PROGRAM", "RESET"]).Any()
                    && (CicsOperands(syntax, "LABEL").Count, tokens.Count(token => token == "CANCEL")) is (1, 0) or (0, 1),
                _ => false
            };
            if (!supported || CicsCompact(Text(statement, "sourceText") ?? "") != CicsCompact(CicsText(syntax))
                || Text(statement, "bindingBasis") != (rule == "Cics_handleContext"
                    ? "parsed-cics-control-binding" : "parsed-cics-terminal-operation")
                || statement.TryGetProperty("bindingDiagnostics", out var diagnostics)
                    && (diagnostics.ValueKind != JsonValueKind.Array || diagnostics.GetArrayLength() > 0)
                || new[] { "TRANSID", "MAP", "MAPSET", "TERMINAL", "UCTRANST", "COMMAREA", "LENGTH",
                    "FROM", "INTO", "FIELD", "RESP", "RESP2", "PROGRAM",
                    "ASSOCIATION", "ODAPPLID", "ODUSERID", "ODFACILNAME", "ODNETWORKID", "ODFACILTYPE"
                }.Any(option => CicsOperands(syntax, option).Count > 1)
                || !statement.TryGetProperty("resourceBindings", out var bindings)
                || bindings.ValueKind != JsonValueKind.Array
                || !statement.TryGetProperty("resourceReferences", out var references)
                || references.ValueKind != JsonValueKind.Array)
            {
                issues.Add($"{identity}: {requirement.Operation} has unsupported or inconsistent terminal binding evidence.");
                continue;
            }
            var resources = bindings.EnumerateArray().ToArray();
            var local = resources.Where(binding => Text(binding, "kind") is "CICS_DATA" or "CICS_LABEL" or "CICS_CONTEXT").ToArray();
            var usages = nodes.Where(node => Text(node, "rule") == "ParagraphNameUsageContext"
                || Text(node, "rule") == "VariableNameUsageContext"
                && node.GetProperty("children").GetArrayLength() > 0
                && Text(node.GetProperty("children")[0], "rule") == "GeneralIdentifierContext").ToArray();
            var refs = references.EnumerateArray().ToArray();
            var terminals = CicsOperands(syntax, "TERMINAL").SelectMany(CicsNodes).ToArray();
            var associations = CicsOperands(syntax, "ASSOCIATION").SelectMany(CicsNodes).ToArray();
            if (local.Length != usages.Length || refs.Length != usages.Length
                || local.Where((binding, index) => !ValidLocal(binding, refs[index], usages[index])).Any())
                issues.Add($"{identity}: {requirement.Operation} terminal operands lack exact native declaration/context bindings.");
            var maps = resources.Where(binding => Text(binding, "kind") == "CICS_MAP").ToArray();
            if (maps.Length != CicsOperands(syntax, "MAP").Count
                || maps.Any(binding => !ValidMap(binding)))
                issues.Add($"{identity}: {requirement.Operation} map lacks a source-backed, matching BMS layout.");
            var inquiry = rule == "Cics_inquireContext" && rules.Contains("Cics_inquire_programContext");
            var programs = resources.Where(binding => Text(binding, "kind") == "CICS_PROGRAM").ToArray();
            if (inquiry && !ValidInquiry(programs))
                issues.Add($"{identity}: {requirement.Operation} program inquiry lacks a finite, source-backed target proof.");
            if (resources.Length != local.Length + maps.Length + (inquiry ? programs.Length : 0)
                + resources.Count(binding => Text(binding, "kind") == "CICS_TRANSACTION"
                    && rule == "Cics_returnContext")
                || rule == "Cics_returnContext" && resources.Count(binding => Text(binding, "kind") == "CICS_TRANSACTION")
                    != CicsOperands(syntax, "TRANSID").Count)
                issues.Add($"{identity}: {requirement.Operation} has unexpected terminal resource bindings.");

            bool ValidInquiry(JsonElement[] programs)
            {
                var operands = CicsOperands(syntax, "PROGRAM");
                if (operands.Count != 1 || programs.Length == 0
                    || !programs[0].TryGetProperty("targetResolution", out var proof)
                    || programs.Any(program => !program.TryGetProperty("targetResolution", out var other)
                        || !JsonElement.DeepEquals(other, proof)))
                    return false;
                var targets = CicsConstantOperand(operands[0], proof, 1, 8) is { } single
                    ? [single] : CicsTableOperand(operands[0], proof, 1, 8)?.OfType<string>().Distinct().ToList();
                return targets is not null && programs.Select(program => Text(program, "name")).SequenceEqual(targets)
                    && programs.All(program => Text(program, "controlEffect") == "program-definition-inquiry"
                        && program.TryGetProperty("attributes", out var attributes) && ValidProgramAttributes(attributes)
                        && Text(program, "programSourcePath") is { } path && Path.IsPathFullyQualified(path)
                        && evidence.Definitions.Any(definition => definition.Kind == "CICS_CONTINUATION"
                            && SameTarget(definition.Name, Text(program, "name") ?? "")
                            && definition.Sha256 == Text(program, "programSourceSha256")
                            && Path.GetFileName(definition.Path) == Path.GetFileName(path)
                            && IsWithin(definition.Path, sourceRoot)));
            }

            bool ValidLocal(JsonElement binding, JsonElement reference, JsonElement usage)
            {
                var name = CicsText(usage).ToUpperInvariant();
                var context = rule is "Cics_inquireContext" or "Cics_set_terminalContext"
                    && name == "EIBTRMID" && terminals.Any(node => JsonElement.DeepEquals(node, usage))
                    || rule == "Cics_inquireContext" && name == "EIBTASKN"
                    && associations.Any(node => JsonElement.DeepEquals(node, usage));
                var kind = context
                        ? "CICS_CONTEXT" : Text(usage, "rule") == "ParagraphNameUsageContext" ? "CICS_LABEL" : "CICS_DATA";
                if (Text(binding, "name") != name || Text(binding, "kind") != kind
                    || Text(reference, "name") != name || Text(reference, "kind") != kind
                    || Text(reference, "grammarRule") != Text(usage, "rule")) return false;
                if (kind == "CICS_CONTEXT")
                    return Text(binding, "definitionScope") == "cics-execution-interface"
                        && Text(binding, "role") == (name == "EIBTASKN" ? "task-number" : "terminal-id");
                if (Text(binding, "definitionScope") != "expanded-parser-input"
                    || !binding.TryGetProperty("definitionSyntax", out var definition) || !ValidSyntax(definition))
                    return false;
                if (kind == "CICS_LABEL")
                    return Text(definition, "rule") is "ParagraphDefinitionNameContext" or "SectionNameContext"
                        && CicsText(definition).ToUpperInvariant() == name;
                if (Text(definition, "rule") is not ("DataDescriptionEntryFormat1Context" or "DataDescriptionEntryFormat1Level77Context"))
                    return false;
                var names = CicsNodes(usage).Select(node => Text(node, "token")?.ToUpperInvariant())
                    .OfType<string>().ToArray();
                // LENGTH OF is a special register over the named item; LENGTH is reserved, never a data name.
                if (names.Length > 2 && names[0] == "LENGTH" && names[1] == "OF") names = names[2..];
                if (names.Length == 4 && names[1] == "(" && names[3] == ")")
                    return ValidSubscripted(binding, definition, names);
                if (names.Length % 2 == 0 || names.Where((token, index) => index % 2 == 0
                        ? !Regex.IsMatch(token, "^[A-Z][A-Z0-9_-]*$") : token is not ("OF" or "IN")).Any()
                    || EntryName(definition) != names[0]) return false;
                if (names.Length == 1) return true;
                if (!binding.TryGetProperty("qualificationPath", out var path) || path.ValueKind != JsonValueKind.Array)
                    return false;
                var ancestors = path.EnumerateArray().ToArray();
                if (ancestors.Length < 2 || !JsonElement.DeepEquals(ancestors[^1], definition)
                    || ancestors.Any(item => !ValidSyntax(item) || Text(item, "rule") != "DataDescriptionEntryFormat1Context"))
                    return false;
                var levels = ancestors.Select(item => CicsNodes(item).Select(node => Text(node, "token"))
                    .FirstOrDefault(token => token is not null)).ToArray();
                if (levels.Any(level => !int.TryParse(level, out var number) || number is < 1 or > 49)
                    || levels.Zip(levels.Skip(1)).Any(pair => int.Parse(pair.First!) >= int.Parse(pair.Second!))
                    || ancestors[..^1].Any(item => CicsNodes(item).Any(node => Text(node, "rule") == "DataPictureClauseContext")))
                    return false;
                var search = ancestors[..^1].Reverse().Select(EntryName).ToList();
                var offset = 0;
                foreach (var qualifier in names.Where((_, index) => index > 0 && index % 2 == 0))
                {
                    var found = search.FindIndex(offset, item => item == qualifier);
                    if (found < 0) return false;
                    offset = found + 1;
                }
                return true;
            }

            // One SEND names one screen: a reaching proof binds it only when exactly one name reaches.
            string? Screen(JsonElement operand, JsonElement? proof) =>
                CicsConstantOperand(operand, proof, 1, 7)
                ?? (rule == "Cics_sendContext" && proof is { ValueKind: JsonValueKind.Object } reaching
                    && CicsReachingOperand(operand, reaching, 1, 7) is [{ } single] ? single : null);

            bool ValidMap(JsonElement binding)
            {
                var mapOperands = CicsOperands(syntax, "MAP");
                var mapsets = CicsOperands(syntax, "MAPSET");
                if (mapOperands.Count != 1 || mapsets.Count > 1) return false;
                JsonElement? mapProof = null, mapsetProof = null;
                if (binding.TryGetProperty("targetResolution", out var roles))
                {
                    if (roles.ValueKind != JsonValueKind.Object
                        || !roles.EnumerateObject().Select(property => property.Name).Order()
                            .SequenceEqual(mapsets.Count == 0 ? new[] { "map" } : ["map", "mapset"]))
                        return false;
                    mapProof = roles.GetProperty("map");
                    if (mapsets.Count > 0) mapsetProof = roles.GetProperty("mapset");
                }
                var map = Screen(mapOperands[0], mapProof);
                var mapset = mapsets.Count == 0 ? map : Screen(mapsets[0], mapsetProof);
                if (map is null || mapset is null || Text(binding, "name") != map || Text(binding, "mapset") != mapset
                    || !binding.TryGetProperty("definition", out var definition) || Text(definition, "name") != map
                    || Text(definition, "mapset") != mapset
                    || !(rule == "Cics_receiveContext" ? new[] { "IN", "INOUT" } : ["OUT", "INOUT"]).Contains(Text(definition, "mode"))
                    || Text(binding, "sourcePath") != Text(binding, "outputPath") + ".bms-source"
                    || Text(binding, "layoutPath") != Text(binding, "outputPath") + ".bms-layout.json") return false;
                var selected = new List<DependencyDefinition>();
                foreach (var (kind, key) in new[] { ("CICS_MAP", "sourceSha256"), ("CICS_MAP_LAYOUT", "layoutSha256"),
                    ("COPY_INPUT", "outputSha256") })
                {
                    var matches = evidence.Definitions.Where(item => item.Kind == kind && item.Name == map
                        && item.Sha256 == Text(binding, key) && IsWithin(item.Path, sourceRoot)).Distinct().ToArray();
                    if (matches.Length != 1) return false;
                    selected.Add(matches[0]);
                }
                using var document = JsonDocument.Parse(File.ReadAllText(selected[1].Path));
                var proof = document.RootElement;
                return Text(proof, "producer") == "bms-symbolic-map/layout-v1" && Text(proof, "status") == "complete"
                    && proof.TryGetProperty("schemaVersion", out var version) && version.ValueKind == JsonValueKind.Number
                    && version.TryGetInt32(out var number) && number == 1
                    && Text(proof, "sourceSha256") == selected[0].Sha256 && Text(proof, "outputSha256") == selected[2].Sha256
                    && proof.TryGetProperty("maps", out var screens) && screens.ValueKind == JsonValueKind.Array
                    && screens.EnumerateArray().Count(screen => JsonElement.DeepEquals(screen, definition)) == 1;
            }
        }

        // NAME(SUBSCRIPT): NAME sits under exactly one OCCURS entry; an identifier subscript is a declared item.
        static bool ValidSubscripted(JsonElement binding, JsonElement definition, string[] names)
        {
            if (!Regex.IsMatch(names[0], "^[A-Z][A-Z0-9_-]*$") || !Regex.IsMatch(names[2], "^([A-Z][A-Z0-9_-]*|[1-9][0-9]{0,3})$")
                || EntryName(definition) != names[0]
                || !binding.TryGetProperty("qualificationPath", out var path) || path.ValueKind != JsonValueKind.Array)
                return false;
            var entries = path.EnumerateArray().ToArray();
            if (entries.Length == 0 || !JsonElement.DeepEquals(entries[^1], definition)
                || entries.Any(item => !ValidSyntax(item) || Text(item, "rule") != "DataDescriptionEntryFormat1Context")
                || entries.Count(item => CicsNodes(item).Any(node => Text(node, "rule") == "DataOccursClauseContext")) != 1)
                return false;
            var levels = entries.Select(item => CicsNodes(item).Select(node => Text(node, "token"))
                .FirstOrDefault(token => token is not null)).ToArray();
            if (levels.Any(level => !int.TryParse(level, out var number) || number is < 1 or > 49) || int.Parse(levels[0]!) != 1
                || levels.Zip(levels.Skip(1)).Any(pair => int.Parse(pair.First!) >= int.Parse(pair.Second!)))
                return false;
            if (char.IsDigit(names[2][0])) return !binding.TryGetProperty("subscriptSyntax", out _);
            return binding.TryGetProperty("subscriptSyntax", out var subscript) && ValidSyntax(subscript)
                && Text(subscript, "rule") is "DataDescriptionEntryFormat1Context" or "DataDescriptionEntryFormat1Level77Context"
                && EntryName(subscript) == names[2];
        }

        static string? EntryName(JsonElement definition)
        {
            var entries = CicsNodes(definition).Where(node => Text(node, "rule") == "EntryNameContext").ToArray();
            return entries.Length == 1 ? CicsText(entries[0]).ToUpperInvariant() : entries.Length == 0 ? "FILLER" : null;
        }
    }
}
