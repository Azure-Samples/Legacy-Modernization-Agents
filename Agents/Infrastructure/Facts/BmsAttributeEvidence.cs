using System.Text.Json;
using System.Text.RegularExpressions;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Facts;

public sealed partial class DependencyEvidenceGate
{
    private static readonly IReadOnlyDictionary<string, string> BmsAttributes = new Dictionary<string, string>
    {
        ["DFHBMASK"] = "A:AUTOSKIP", ["DFHBMPRF"] = "A:PROTECTED_MDT_SET", ["DFHBMFSE"] = "A:MDT_SET",
        ["DFHNEUTR"] = "C:NEUTRAL", ["DFHGREEN"] = "C:GREEN", ["DFHUNDLN"] = "H:UNDERSCORE",
        ["DFHBMUNP"] = "A:UNPROTECTED", ["DFHBMPRO"] = "A:PROTECTED", ["DFHBMBRY"] = "A:BRIGHT",
        ["DFHBMDAR"] = "A:DARK", ["DFHBMASF"] = "A:AUTOSKIP_MDT_SET", ["DFHBMASB"] = "A:AUTOSKIP_BRIGHT",
        ["DFHDFCOL"] = "C:DEFAULT", ["DFHBLUE"] = "C:BLUE", ["DFHRED"] = "C:RED", ["DFHPINK"] = "C:PINK",
        ["DFHTURQ"] = "C:TURQUOISE", ["DFHYELLO"] = "C:YELLOW", ["DFHBLINK"] = "H:BLINK", ["DFHREVRS"] = "H:REVERSE"
    };

    private static readonly string[] BmsDocumentation =
    [
        "https://www.ibm.com/docs/en/cics-ts/6.x?topic=characteristics-attribute-value-definitions-dfhbmsca",
        "https://www.ibm.com/docs/en/cics-ts/6.x?topic=output-setting-display-characteristics",
        "https://www.ibm.com/docs/en/cics-tx/11.1.0?topic=constants-field-attributes-printer-control-characters"
    ];

    private static bool ValidBmsDefinition(JsonElement definition) =>
        Keys(definition, "recipe", "selector", "symbols", "documentation", "targetSemantics")
        && Text(definition, "recipe") == "symbolic-bms-attributes-v1"
        && Text(definition, "selector") == "bms-output-field"
        && Text(definition, "targetSemantics") == "preserve-field-attribute-assignments"
        && definition.TryGetProperty("symbols", out var symbols) && Keys(symbols, BmsAttributes.Keys.ToArray())
        && BmsAttributes.All(pair => Text(symbols, pair.Key) == pair.Value)
        && definition.TryGetProperty("documentation", out var documentation) && documentation.ValueKind == JsonValueKind.Array
        && documentation.EnumerateArray().Select(value => value.ValueKind == JsonValueKind.String ? value.GetString() : null)
            .SequenceEqual(BmsDocumentation);

    // Single unqualified targets keep fieldBinding; qualified or multi-target MOVEs list them.
    private static (JsonElement[] Fields, JsonElement[] Declarations)? BmsBindings(JsonElement usage)
    {
        var single = usage.TryGetProperty("fieldBinding", out var field);
        var multiple = usage.TryGetProperty("fieldBindings", out var fields);
        if (single == multiple) return null;
        if (single)
            return usage.TryGetProperty("definitionSyntax", out var declaration) ? ([field], [declaration]) : null;
        if (fields.ValueKind != JsonValueKind.Array || !usage.TryGetProperty("definitionSyntaxes", out var declarations)
            || declarations.ValueKind != JsonValueKind.Array || fields.GetArrayLength() == 0
            || fields.GetArrayLength() != declarations.GetArrayLength()) return null;
        return (fields.EnumerateArray().ToArray(), declarations.EnumerateArray().ToArray());
    }

    private sealed class OverlayEntry
    {
        public int Level, Occurs = 1, Size = -1, Offset;
        public string Name = "";
        public string? Redefines;
        public OverlayEntry? Parent;
        public List<OverlayEntry> Children { get; } = [];
    }

    private static string OverlayToken(string token) =>
        Regex.IsMatch(token.ToUpperInvariant(), "^FIL-R[0-9]+$") ? "FILLER" : token.ToUpperInvariant();

    // Mirrors BmsOverlayLayout.java / overlay_layout: DISPLAY X fields, binary halfwords, plain groups, group
    // REDEFINES of an earlier named sibling and exactly one group OCCURS above the one-byte target.
    internal static (string Redefined, int Base, int Stride, int Count)? BmsOverlayLayout(
        IReadOnlyList<string> record, IReadOnlyList<IReadOnlyList<string>> members, int target)
    {
        const string name = "[A-Z][A-Z0-9-]*";
        // FIL-Rnnn is the preprocessor's layout-neutral rename of FILLER REDEFINES.
        var head = record.Select(OverlayToken).ToArray();
        if (head.Length != 5 || head[0] is not ("1" or "01") || head[1] != "FILLER" || head[2] != "REDEFINES"
            || !Regex.IsMatch(head[3], $"^{name}$") || head[4] != "." || target < 0 || target >= members.Count) return null;
        var root = new OverlayEntry { Level = 1, Name = "FILLER" };
        var entries = new List<OverlayEntry>();
        var current = root;
        foreach (var member in members)
        {
            var tokens = member.Select(OverlayToken).ToArray();
            if (tokens.Length < 3 || !Regex.IsMatch(tokens[0], "^(0?[1-9]|[1-4][0-9])$") || !Regex.IsMatch(tokens[1], $"^{name}$")
                || tokens[1] is "REDEFINES" or "OCCURS") return null;
            var entry = new OverlayEntry { Level = int.Parse(tokens[0]), Name = tokens[1] };
            var rest = tokens[2..];
            var clause = string.Concat(rest);
            Match match;
            if (rest is ["."]) { }
            else if (rest.Length == 3 && rest[0] == "REDEFINES" && Regex.IsMatch(rest[1], $"^{name}$") && rest[2] == ".")
                entry.Redefines = rest[1];
            else if (rest[0] == "OCCURS" && (match = Regex.Match(clause, @"^OCCURS0*([1-9][0-9]{0,3})(?:TIMES)?\.$")).Success)
                entry.Occurs = int.Parse(match.Groups[1].Value);
            else if ((match = Regex.Match(clause, @"^PIC(?:TURE)?(?:IS)?X(?:\(0*([1-9][0-9]{0,3})\))?\.$")).Success)
                entry.Size = match.Groups[1].Success ? int.Parse(match.Groups[1].Value) : 1;
            else if (Regex.IsMatch(clause, @"^(?:PIC(?:TURE)?(?:IS)?S9\(0*4\)(?:COMP|COMPUTATIONAL|BINARY|COMP-4|COMPUTATIONAL-4)"
                         + @"|(?:COMP|COMPUTATIONAL|BINARY|COMP-4|COMPUTATIONAL-4)PIC(?:TURE)?(?:IS)?S9\(0*4\))\.$"))
                entry.Size = 2;
            else return null;
            if (entry.Level <= 1) return null;
            while (current != root && current.Level >= entry.Level) current = current.Parent!;
            if (current.Size >= 0) return null;
            entry.Parent = current;
            current.Children.Add(entry);
            entries.Add(entry);
            if (entry.Size < 0) current = entry;
        }
        static int Size(OverlayEntry entry)
        {
            if (entry.Children.Count == 0) return entry.Size;
            var total = 0;
            foreach (var child in entry.Children)
            {
                var width = Size(child);
                if (width <= 0) return -1;
                if (child.Redefines is null) total += width;
            }
            return total * entry.Occurs;
        }
        static bool Place(OverlayEntry group, int start)
        {
            var cursor = start;
            for (var index = 0; index < group.Children.Count; index++)
            {
                var child = group.Children[index];
                if (child.Redefines is not null)
                {
                    var origin = group.Children.Take(index).LastOrDefault(sibling => sibling.Redefines is null && sibling.Name == child.Redefines);
                    if (child.Occurs != 1 || origin is null || origin.Occurs != 1 || Size(child) > Size(origin)) return false;
                    child.Offset = origin.Offset;
                }
                else
                {
                    child.Offset = cursor;
                    cursor += Size(child);
                }
                if (child.Children.Count > 0 && !Place(child, child.Offset)) return false;
            }
            return true;
        }
        if (Size(root) < 0 || !Place(root, 0)) return null;
        var field = entries[target];
        if (field.Size != 1 || field.Name == "FILLER" || entries.Count(entry => entry.Name == field.Name) != 1) return null;
        var tables = new List<OverlayEntry>();
        for (var ancestor = field.Parent; ancestor is not null; ancestor = ancestor.Parent)
            if (ancestor.Occurs > 1) tables.Add(ancestor);
        if (tables.Count != 1) return null;
        return (head[3], field.Offset, Size(tables[0]) / tables[0].Occurs, tables[0].Occurs);
    }

    // Symbolic map field: length halfword at +0, attribute byte at +2, extended attributes from +3 in DSATTS order.
    internal static string? BmsAttributeSuffix(int delta, IReadOnlyList<string> extended)
    {
        if (delta == 2) return "A";
        var position = delta - 3;
        if (position < 0 || position >= extended.Count) return null;
        return extended[position] switch { "COLOR" => "C", "HILIGHT" => "H", _ => null };
    }

    private static readonly string[] OverlayKeys =
    [
        "definitionSyntax", "layoutPath", "layoutSha256", "map", "mapset", "memberSyntaxes", "name", "occurrences",
        "outputPath", "outputSha256", "recordSyntax", "subscript", "suffix"
    ];

    private static bool ValidBmsOverlayUsage(JsonElement usage, string[] tokens, string value)
    {
        if (usage.TryGetProperty("fieldBinding", out _) || usage.TryGetProperty("fieldBindings", out _)
            || usage.GetProperty("overlayBindings") is not { ValueKind: JsonValueKind.Array } bindings
            || (tokens.Length - 3) % 4 != 0 || bindings.GetArrayLength() == 0
            || bindings.GetArrayLength() != (tokens.Length - 3) / 4) return false;
        string[]? Words(JsonElement syntax) =>
            syntax.ValueKind == JsonValueKind.Object && ValidSyntax(syntax) && Text(syntax, "rule") == "DataDescriptionEntryFormat1Context"
                ? PlatformSyntaxTokens(syntax).Select(token => token.Trim()).Where(token => token.Length > 0).ToArray() : null;
        var position = 3;
        foreach (var binding in bindings.EnumerateArray())
        {
            var name = tokens[position];
            var subscript = tokens[position + 2];
            if (binding.ValueKind != JsonValueKind.Object
                || !binding.EnumerateObject().Select(property => property.Name).Order(StringComparer.Ordinal).SequenceEqual(OverlayKeys)
                || tokens[position + 1] != "(" || tokens[position + 3] != ")" || !Regex.IsMatch(name, "^[A-Z][A-Z0-9-]*$")
                || !Regex.IsMatch(subscript, "^([A-Z][A-Z0-9-]*|[1-9][0-9]{0,3})$")
                || Text(binding, "name") != name || Text(binding, "subscript") != subscript
                || binding.GetProperty("memberSyntaxes").ValueKind != JsonValueKind.Array
                || binding.GetProperty("occurrences").ValueKind != JsonValueKind.Array
                || new[] { "map", "mapset", "layoutPath", "layoutSha256", "outputPath", "outputSha256" }
                    .Any(key => string.IsNullOrWhiteSpace(Text(binding, key)))) return false;
            var record = Words(binding.GetProperty("recordSyntax"));
            var memberSyntax = binding.GetProperty("memberSyntaxes").EnumerateArray().ToArray();
            var members = memberSyntax.Select(Words).ToArray();
            var targets = Enumerable.Range(0, memberSyntax.Length)
                .Where(index => JsonElement.DeepEquals(memberSyntax[index], binding.GetProperty("definitionSyntax"))).ToArray();
            if (record is null || members.Any(member => member is null) || targets.Length != 1
                || !string.Equals(members[targets[0]]![1], name, StringComparison.OrdinalIgnoreCase)) return false;
            var located = BmsOverlayLayout(record, members.Select(member => (IReadOnlyList<string>)member!).ToArray(), targets[0]);
            if (located is null || located.Value.Redefined != Text(binding, "map") + "I" && located.Value.Redefined != Text(binding, "map") + "O")
                return false;
            var occurrences = binding.GetProperty("occurrences").EnumerateArray().ToArray();
            if (occurrences.Length != located.Value.Count || occurrences.Select((item, index) => (item, index)).Any(pair =>
                    pair.item.ValueKind != JsonValueKind.Object || !Keys(pair.item, "field", "offset")
                    || string.IsNullOrWhiteSpace(Text(pair.item, "field"))
                    || !pair.item.GetProperty("offset").TryGetInt32(out var offset)
                    || offset != located.Value.Base + pair.index * located.Value.Stride)) return false;
            var suffix = Text(binding, "suffix");
            var named = BmsAttributes.TryGetValue(value, out var meaning);
            if (suffix is not ("A" or "C" or "H")
                || !(named && meaning!.StartsWith(suffix + ":", StringComparison.Ordinal)
                    || value is "'A'" or "\"A\"" && suffix == "A"
                    || value is "HIGH-VALUE" or "HIGH-VALUES" && suffix is "C" or "H")) return false;
            position += 4;
        }
        return true;
    }

    private static bool ValidBmsUsage(JsonElement usage)
    {
        if (usage.TryGetProperty("overlayBindings", out _))
        {
            if (Text(usage, "grammarRule") != "MoveStatementContext"
                || Text(usage, "coordinateSpace") != "expanded-parser-input"
                || string.IsNullOrWhiteSpace(Text(usage, "sourceText"))
                || !usage.TryGetProperty("line", out var overlayLine) || !overlayLine.TryGetInt32(out var overlayNumber) || overlayNumber < 1
                || !usage.TryGetProperty("syntax", out var overlaySyntax) || !ValidSyntax(overlaySyntax)
                || Text(overlaySyntax, "rule") != "MoveStatementContext") return false;
            var overlayTokens = PlatformSyntaxTokens(overlaySyntax).Select(NormalizeBmsToken).ToArray();
            if (overlayTokens.Length < 7 || overlayTokens[0] != "MOVE" || overlayTokens[2] != "TO") return false;
            var overlayValue = overlayTokens[1];
            var overlayKind = BmsAttributes.ContainsKey(overlayValue) ? "named-attribute"
                : overlayValue is "'A'" or "\"A\"" ? "source-character-literal" : "figurative-constant";
            return Text(usage, "valueKind") == overlayKind && ValidBmsOverlayUsage(usage, overlayTokens, overlayValue);
        }
        var bindings = BmsBindings(usage);
        if (bindings is null
            || Text(usage, "grammarRule") != "MoveStatementContext"
            || Text(usage, "coordinateSpace") != "expanded-parser-input"
            || string.IsNullOrWhiteSpace(Text(usage, "sourceText"))
            || !usage.TryGetProperty("line", out var line) || !line.TryGetInt32(out var number) || number < 1
            || !usage.TryGetProperty("syntax", out var syntax) || !ValidSyntax(syntax)
            || Text(syntax, "rule") != "MoveStatementContext"
            || bindings.Value.Declarations.Any(declaration => !ValidSyntax(declaration)
                || Text(declaration, "rule") != "DataDescriptionEntryFormat1Context")) return false;
        var tokens = PlatformSyntaxTokens(syntax).Select(NormalizeBmsToken).ToArray();
        if (tokens.Length < 4 || tokens[0] != "MOVE" || tokens[2] != "TO") return false;
        var targets = new List<(string Name, string? Group)>();
        for (var index = 3; index < tokens.Length;)
        {
            var name = tokens[index++];
            string? group = null;
            if (index < tokens.Length && tokens[index] is "OF" or "IN")
            {
                if (index + 1 >= tokens.Length) return false;
                group = tokens[index + 1];
                index += 2;
            }
            targets.Add((name, group));
        }
        var simple = targets.Count == 1 && targets[0].Group is null;
        if (targets.Count != bindings.Value.Fields.Length || simple != usage.TryGetProperty("fieldBinding", out _)) return false;
        var value = tokens[1];
        var named = BmsAttributes.TryGetValue(value, out var meaning);
        var kind = named ? "named-attribute" : value is "'A'" or "\"A\"" ? "source-character-literal" : "figurative-constant";
        if (Text(usage, "valueKind") != kind) return false;
        foreach (var ((name, group), field) in targets.Zip(bindings.Value.Fields))
        {
            var suffix = Text(field, "suffix");
            if (name != Text(field, "name") || Text(field, "name") != Text(field, "field") + suffix
                || suffix is not ("A" or "C" or "H")
                || group is not null && group != Text(field, "map") + "O" && group != Text(field, "map") + "I"
                || new[] { "map", "mapset", "layoutPath", "layoutSha256", "outputPath", "outputSha256" }
                    .Any(key => string.IsNullOrWhiteSpace(Text(field, key)))
                || !(named && meaning!.StartsWith(suffix + ":", StringComparison.Ordinal)
                    || value is "'A'" or "\"A\"" && suffix == "A"
                    || value is "HIGH-VALUE" or "HIGH-VALUES" && suffix is "C" or "H")) return false;
        }
        return true;
    }

    private static IEnumerable<(JsonElement Field, JsonElement? Occurrence)> BmsBoundFields(JsonElement usage)
    {
        if (!usage.TryGetProperty("overlayBindings", out var overlays))
            return (BmsBindings(usage)?.Fields ?? []).Select(field => (field, (JsonElement?)null));
        return overlays.EnumerateArray().SelectMany(overlay => overlay.GetProperty("occurrences").EnumerateArray()
            .Select(item => (overlay, (JsonElement?)item)));
    }

    private static string NormalizeBmsToken(string token) =>
        token.StartsWith('\'') || token.StartsWith('"') ? token : token.ToUpperInvariant();

    private static void ValidateBmsAttributes(string identity, DependencyEvidence evidence,
        JsonElement binding, string source, List<string> issues)
    {
        var usages = binding.GetProperty("usages").EnumerateArray().ToArray();
        var original = ExpandedContradictionTokens(source, evidence);
        if (original is null)
        {
            issues.Add($"{identity}: BMS attribute uses cannot be compared against hash-bound COPY-expanded source.");
            return;
        }
        var allFields = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        foreach (var path in evidence.Definitions.Where(item => item.Kind is "BMS_LAYOUT" or "CICS_MAP_LAYOUT")
                     .Select(item => item.Path).Distinct())
        {
            using var layout = JsonDocument.Parse(File.ReadAllText(path));
            foreach (var map in layout.RootElement.GetProperty("maps").EnumerateArray())
                foreach (var item in map.GetProperty("fields").EnumerateArray())
                    foreach (var suffix in new[] { "A", "C", "H" })
                        allFields.Add(Text(item, "name") + suffix);
        }
        foreach (var usage in usages)
        foreach (var (field, occurrence) in BmsBoundFields(usage))
        {
            var layouts = evidence.Definitions.Where(item => item.Kind is "BMS_LAYOUT" or "CICS_MAP_LAYOUT"
                && item.Sha256 == Text(field, "layoutSha256")).Select(item => item.Path).Distinct().ToArray();
            var outputs = evidence.Definitions.Where(item => item.Kind is "COPY" or "COPY_INPUT"
                && item.Sha256 == Text(field, "outputSha256")).Select(item => item.Path).Distinct().ToArray();
            var valid = layouts.Length == 1 && outputs.Length == 1
                && layouts[0] == outputs[0] + ".bms-layout.json";
            if (valid)
            {
                using var document = JsonDocument.Parse(File.ReadAllText(layouts[0]));
                var proof = document.RootElement;
                valid = Text(proof, "producer") == "bms-symbolic-map/layout-v1" && Text(proof, "status") == "complete"
                    && Text(proof, "outputSha256") == Text(field, "outputSha256")
                    && Text(field, "layoutPath") == Text(field, "outputPath") + ".bms-layout.json";
                var maps = proof.GetProperty("maps").EnumerateArray().Where(map => Text(map, "name") == Text(field, "map")
                    && Text(map, "mapset") == Text(field, "mapset") && Text(map, "mode") is "OUT" or "INOUT").ToArray();
                valid &= maps.Length == 1;
                if (maps.Length == 1)
                {
                    var fields = maps[0].GetProperty("fields").EnumerateArray().ToArray();
                    var fieldName = occurrence is { } at ? Text(at, "field") : Text(field, "field");
                    var selected = fields.Where(item => Text(item, "name") == fieldName).ToArray();
                    var suffix = Text(field, "suffix");
                    valid &= selected.Length == 1 && selected[0].GetProperty("occurs").GetInt32() == 1
                        && (suffix == "A" || maps[0].GetProperty("extendedAttributes").EnumerateArray()
                            .Any(value => value.GetString() == (suffix == "C" ? "COLOR" : "HILIGHT")));
                    if (valid && occurrence is { } placed)
                        valid = BmsAttributeSuffix(placed.GetProperty("offset").GetInt32() - selected[0].GetProperty("offset").GetInt32(),
                            maps[0].GetProperty("extendedAttributes").EnumerateArray().Select(value => value.GetString() ?? "").ToArray()) == suffix;
                }
            }
            var syntaxTokens = PlatformSyntaxTokens(usage.GetProperty("syntax")).ToArray();
            var expected = ContradictionTokens([string.Join(" ", syntaxTokens)]);
            var text = ContradictionTokens(Text(usage, "sourceText")!.Split('\n'));
            var sourceTokens = original;
            if (!valid || !SameTokens(expected, text) || !Enumerable.Range(0, Math.Max(0, sourceTokens.Count - expected.Count + 1))
                    .Any(index => SameTokens(expected, sourceTokens.Skip(index).Take(expected.Count).ToArray())))
                issues.Add($"{identity}: BMS attribute assignment lacks matching source text or hash-bound output field.");
        }
        bool Relevant(SourceToken token) => !token.IsLiteral
            && (BmsAttributes.ContainsKey(token.Text.ToUpperInvariant()) || allFields.Contains(token.Text));
        var sourceUses = original.Where(Relevant).Select(token => token.Text.ToUpperInvariant()).ToArray();
        var reported = usages.SelectMany(usage => ContradictionTokens(Text(usage, "sourceText")!.Split('\n')))
            .Where(Relevant).Select(token => token.Text.ToUpperInvariant()).ToArray();
        if (!sourceUses.SequenceEqual(reported)
            || original.Select((token, index) => (token, index)).Any(item =>
                item.token.Text.Equals("DFHBMSCA", StringComparison.OrdinalIgnoreCase)
                && !(item.index > 0 && original[item.index - 1].Text.Equals("COPY", StringComparison.OrdinalIgnoreCase))))
            issues.Add($"{identity}: BMS attribute symbol/field uses do not cover the original source; omitted or aggregate uses remain unresolved.");

        static bool SameTokens(IReadOnlyList<SourceToken> left, IReadOnlyList<SourceToken> right) =>
            left.Count == right.Count && left.Zip(right).All(pair => pair.First.IsLiteral == pair.Second.IsLiteral
                && string.Equals(pair.First.Text, pair.Second.Text,
                    pair.First.IsLiteral ? StringComparison.Ordinal : StringComparison.OrdinalIgnoreCase));
    }
}
