using System.Text;
using System.Text.Json.Nodes;
using System.Text.RegularExpressions;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using static CobolToQuarkusMigration.Agents.Infrastructure.Rekt.RektEvidence;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Rekt;

// Explicit analysis-only projections of documented platform event symbols.
public static class PlatformCopybookContracts
{
    public const string Marker = "ANALYSIS-ONLY PLATFORM EVENT CONTRACT; NOT A VENDOR COPYBOOK";
    public const string Documentation = "https://www.ibm.com/docs/en/cics-ts/6.x";

    public static readonly IReadOnlyDictionary<string, string> Symbols = BuildSymbols();

    private static readonly string[] BmsDocumentation =
    [
        "https://www.ibm.com/docs/en/cics-ts/6.x?topic=characteristics-attribute-value-definitions-dfhbmsca",
        "https://www.ibm.com/docs/en/cics-ts/6.x?topic=output-setting-display-characteristics",
        "https://www.ibm.com/docs/en/cics-tx/11.1.0?topic=constants-field-attributes-printer-control-characters",
    ];

    public static readonly IReadOnlyDictionary<string, string> BmsSymbols = new Dictionary<string, string>
    {
        ["DFHBMASK"] = "A:AUTOSKIP",
        ["DFHBMPRF"] = "A:PROTECTED_MDT_SET",
        ["DFHBMFSE"] = "A:MDT_SET",
        ["DFHNEUTR"] = "C:NEUTRAL",
        ["DFHGREEN"] = "C:GREEN",
        ["DFHUNDLN"] = "H:UNDERSCORE",
        ["DFHBMUNP"] = "A:UNPROTECTED",
        ["DFHBMPRO"] = "A:PROTECTED",
        ["DFHBMBRY"] = "A:BRIGHT",
        ["DFHBMDAR"] = "A:DARK",
        ["DFHBMASF"] = "A:AUTOSKIP_MDT_SET",
        ["DFHBMASB"] = "A:AUTOSKIP_BRIGHT",
        ["DFHDFCOL"] = "C:DEFAULT",
        ["DFHBLUE"] = "C:BLUE",
        ["DFHRED"] = "C:RED",
        ["DFHPINK"] = "C:PINK",
        ["DFHTURQ"] = "C:TURQUOISE",
        ["DFHYELLO"] = "C:YELLOW",
        ["DFHBLINK"] = "H:BLINK",
        ["DFHREVRS"] = "H:REVERSE",
    };

    private static Dictionary<string, string> BuildSymbols()
    {
        var symbols = new Dictionary<string, string> { ["DFHENTER"] = "ENTER", ["DFHCLEAR"] = "CLEAR" };
        for (var i = 1; i < 4; i++) symbols[$"DFHPA{i}"] = $"PA{i}";
        for (var i = 1; i < 25; i++) symbols[$"DFHPF{i}"] = $"PF{i}";
        return symbols;
    }

    private static JsonObject ToObject(IReadOnlyDictionary<string, string> values) =>
        new(values.Select(pair => KeyValuePair.Create(pair.Key, (JsonNode?)pair.Value)));

    internal static JsonObject BmsContract() => new()
    {
        ["recipe"] = "symbolic-bms-attributes-v1",
        ["selector"] = "bms-output-field",
        ["symbols"] = ToObject(BmsSymbols),
        ["documentation"] = ToArray(BmsDocumentation),
        ["targetSemantics"] = "preserve-field-attribute-assignments",
    };

    public static JsonNode? Validate(JsonNode? document)
    {
        var copybooks = Get(document, "copybooks") as JsonObject;
        if (!KeysAre(document, "schemaVersion", "profileKind", "profileName", "copybooks")
            || !PyJson.PyEquals(Get(document, "schemaVersion"), 1)
            || !StrEquals(Get(document, "profileKind"), "platform-replacement")
            || Str(document, "profileName") is not { } profileName || profileName.Trim().Length == 0
            || copybooks is null || copybooks.Count == 0
            || copybooks.Any(pair => pair.Key is not ("DFHAID" or "DFHBMSCA")))
            throw PyError.Value("Unsupported platform replacement contract schema");
        if (copybooks.ContainsKey("DFHBMSCA") && !PyJson.PyEquals(copybooks["DFHBMSCA"], BmsContract()))
            throw PyError.Value("Unsupported DFHBMSCA attribute contract");
        if (!copybooks.TryGetPropertyValue("DFHAID", out var contract)) return null;
        if (!KeysAre(contract, "recipe", "selector", "symbols", "documentation", "targetSemantics")
            || !StrEquals(Get(contract, "recipe"), "symbolic-terminal-events-v1")
            || !StrEquals(Get(contract, "selector"), "EIBAID")
            || !PyJson.PyEquals(Get(contract, "symbols"), ToObject(Symbols))
            || !PyJson.PyEquals(Get(contract, "documentation"), ToArray([Documentation]))
            || !StrEquals(Get(contract, "targetSemantics"), "named-events-preserve-source-branches"))
            throw PyError.Value("Unsupported DFHAID event contract; no source bytes or business actions may be inferred");
        return contract;
    }

    public static byte[] Projection(JsonNode? contract)
    {
        var name = PyJson.PyEquals(contract, BmsContract()) ? "DFHBMSCA" : "DFHAID";
        var text = new StringBuilder()
            .Append($"      * {Marker}\n")
            .Append("      * No VALUE clauses: these names are symbolic, not blank data.\n")
            .Append($"       01 {name}.\n");
        foreach (var symbol in (Obj(contract, "symbols") ?? []).Select(pair => pair.Key).Order(StringComparer.Ordinal))
            text.Append($"          02 {symbol} PIC X.\n");
        return Encoding.ASCII.GetBytes(text.ToString());
    }

    // Single unqualified targets keep fieldBinding; qualified or multi-target MOVEs list them.
    internal static (List<JsonNode?> Bindings, List<JsonNode?> Declarations) BmsBindings(JsonNode? usage)
    {
        if (Has(usage, "fieldBinding") == Has(usage, "fieldBindings"))
            throw PyError.Value("BMS usage must carry exactly one binding shape");
        if (Has(usage, "fieldBinding"))
            return ([Get(usage, "fieldBinding")], [Get(usage, "definitionSyntax")]);
        if (Get(usage, "fieldBindings") is not JsonArray bindings || bindings.Count == 0
            || Get(usage, "definitionSyntaxes") is not JsonArray declarations || bindings.Count != declarations.Count)
            throw PyError.Value("BMS usage bindings and declarations differ");
        return (bindings.ToList(), declarations.ToList());
    }

    private static readonly string[] OverlayKeys =
    [
        "name", "subscript", "map", "mapset", "suffix", "layoutPath", "layoutSha256", "outputPath",
        "outputSha256", "recordSyntax", "memberSyntaxes", "definitionSyntax", "occurrences",
    ];

    private static readonly string[] BoundKeys = ["map", "mapset", "layoutPath", "layoutSha256", "outputPath", "outputSha256"];

    private static bool ValueFitsSuffix(string value, string? suffix) =>
        BmsSymbols.TryGetValue(value, out var symbol) && symbol.StartsWith(suffix + ":", StringComparison.Ordinal)
        || value is "'A'" or "\"A\"" && suffix == "A"
        || value is "HIGH-VALUE" or "HIGH-VALUES" && suffix is "C" or "H";

    private static string ValueKind(string value) =>
        BmsSymbols.ContainsKey(value) ? "named-attribute"
        : value is "'A'" or "\"A\"" ? "source-character-literal" : "figurative-constant";

    private static List<string>? Words(JsonNode? syntax)
    {
        if (syntax is not JsonObject || Str(syntax, "rule") != "DataDescriptionEntryFormat1Context") return null;
        var nodes = SyntaxNodes(syntax);
        return nodes.Count == 0 ? null
            : Tokens(nodes).Select(token => token.Trim()).Where(token => token.Length > 0).ToList();
    }

    private static bool BmsOverlayUsageBound(JsonNode? usage, List<string> tokens, string value)
    {
        if (Has(usage, "fieldBinding") || Has(usage, "fieldBindings") || Get(usage, "overlayBindings") is not JsonArray bindings
            || (tokens.Count - 3) % 4 != 0 || bindings.Count != (tokens.Count - 3) / 4 || bindings.Count == 0)
            return false;
        for (var (position, slot) = (3, 0); position < tokens.Count && slot < bindings.Count; position += 4, slot++)
        {
            var binding = bindings[slot];
            var (name, subscript) = (tokens[position], tokens[position + 2]);
            if (!KeysAre(binding, OverlayKeys) || tokens[position + 1] != "(" || tokens[position + 3] != ")"
                || Full("[A-Z][A-Z0-9-]*", name) is null || Full("[A-Z][A-Z0-9-]*|[1-9][0-9]{0,3}", subscript) is null
                || !StrEquals(Get(binding, "name"), name) || !StrEquals(Get(binding, "subscript"), subscript)
                || Get(binding, "memberSyntaxes") is not JsonArray memberSyntaxes
                || Get(binding, "occurrences") is not JsonArray occurrences
                || BoundKeys.Any(key => string.IsNullOrEmpty(Str(binding, key))))
                return false;
            var record = Words(Get(binding, "recordSyntax"));
            var members = memberSyntaxes.Select(Words).ToList();
            var targets = Enumerable.Range(0, memberSyntaxes.Count)
                .Where(index => PyJson.PyEquals(memberSyntaxes[index], Get(binding, "definitionSyntax"))).ToList();
            if (record is null || members.Any(item => item is null) || targets.Count != 1
                || members[targets[0]]!.Count < 2 || members[targets[0]]![1].ToUpperInvariant() != name)
                return false;
            var located = DependencyEvidenceGate.BmsOverlayLayout(record, members.Select(item => (IReadOnlyList<string>)item!).ToList(), targets[0]);
            var map = Str(binding, "map");
            if (located is not { } layout || layout.Redefined != map + "I" && layout.Redefined != map + "O")
                return false;
            if (occurrences.Count != layout.Count) return false;
            for (var occurrence = 0; occurrence < layout.Count; occurrence++)
                if (!PyJson.PyEquals(occurrences[occurrence] is JsonObject item ? Get(item, "offset") : null,
                        layout.Base + occurrence * layout.Stride))
                    return false;
            if (!occurrences.All(item => KeysAre(item, "field", "offset") && PyJson.IsStr(Get(item, "field"))))
                return false;
            var suffix = Str(binding, "suffix");
            if (suffix is not ("A" or "C" or "H") || !ValueFitsSuffix(value, suffix)) return false;
        }
        return true;
    }

    public static bool BmsUsageBound(JsonNode? usage)
    {
        if (!Has(usage, "syntax")) return false;
        var tokens = Tokens(SyntaxNodes(Get(usage, "syntax")))
            .Select(token => token.StartsWith('\'') || token.StartsWith('"') ? token : token.ToUpperInvariant()).ToList();
        var line = IsInt(Get(usage, "line"), out var number) ? number : (long?)null;
        var sourceText = Str(usage, "sourceText");
        if (Has(usage, "overlayBindings"))
            return Str(usage, "grammarRule") == "MoveStatementContext"
                && Str(Get(usage, "syntax"), "rule") == "MoveStatementContext"
                && Str(usage, "coordinateSpace") == "expanded-parser-input"
                && line >= 1 && sourceText is not null && sourceText.Trim().Length > 0
                && tokens.Count >= 7 && tokens[0] == "MOVE" && tokens[2] == "TO"
                && CicsCompact(sourceText) == CicsCompact(string.Join(' ', tokens))
                && Str(usage, "valueKind") == ValueKind(tokens[1])
                && BmsOverlayUsageBound(usage, tokens, tokens[1]);
        List<JsonNode?> bindings, declarations;
        try
        {
            (bindings, declarations) = BmsBindings(usage);
        }
        catch (PyError)
        {
            return false;
        }
        if (Str(usage, "grammarRule") != "MoveStatementContext"
            || Str(Get(usage, "syntax"), "rule") != "MoveStatementContext"
            || Str(usage, "coordinateSpace") != "expanded-parser-input"
            || line is null or < 1 || sourceText is null || sourceText.Trim().Length == 0
            || tokens.Count < 4 || tokens[0] != "MOVE" || tokens[2] != "TO"
            || declarations.Any(declaration => Str(declaration, "rule") != "DataDescriptionEntryFormat1Context"))
            return false;
        var targets = new List<(string Name, string? Group)>();
        var index = 3;
        while (index < tokens.Count)
        {
            var name = tokens[index];
            string? group = null;
            index++;
            if (index < tokens.Count && tokens[index] is "OF" or "IN")
            {
                if (index + 1 >= tokens.Count) return false;
                group = tokens[index + 1];
                index += 2;
            }
            targets.Add((name, group));
        }
        if (targets.Count != bindings.Count || Has(usage, "fieldBinding") != (targets.Count == 1 && targets[0].Group is null))
            return false;
        if (CicsCompact(sourceText) != CicsCompact(string.Join(' ', tokens))) return false;
        var value = tokens[1];
        if (Str(usage, "valueKind") != ValueKind(value)) return false;
        foreach (var ((name, group), field) in targets.Zip(bindings))
        {
            var suffix = Str(field, "suffix");
            var map = Str(field, "map");
            if (!StrEquals(Get(field, "name"), name) || Str(field, "name") != Str(field, "field") + suffix
                || suffix is not ("A" or "C" or "H")
                || group is not null && group != map + "O" && group != map + "I"
                || BoundKeys.Any(key => string.IsNullOrEmpty(Str(field, key))))
                return false;
            if (!ValueFitsSuffix(value, suffix)) return false;
        }
        return true;
    }

    private static IEnumerable<JsonNode?> BoundFields(JsonNode? usage)
    {
        if (!Has(usage, "overlayBindings")) return BmsBindings(usage).Bindings;
        return Items(usage, "overlayBindings").SelectMany(overlay => Items(overlay, "occurrences").Select(item =>
        {
            var field = (JsonObject)overlay!.DeepClone();
            field["name"] = Str(item, "field") + Str(overlay, "suffix");
            field["field"] = Get(item, "field")?.DeepClone();
            field["offset"] = Get(item, "offset")?.DeepClone();
            return (JsonNode?)field;
        }));
    }

    public static bool BmsLayoutsBound(JsonNode? binding, IReadOnlyList<JsonNode?> definitions)
    {
        foreach (var field in Items(binding, "usages").SelectMany(BoundFields))
        {
            var layouts = definitions.Where(item => Str(item, "kind") is "BMS_LAYOUT" or "CICS_MAP_LAYOUT"
                && PyJson.PyEquals(Get(item, "sha256"), Get(field, "layoutSha256"))).Select(item => Str(item, "path")).Distinct().ToList();
            var outputs = definitions.Where(item => Str(item, "kind") is "COPY" or "COPY_INPUT"
                && PyJson.PyEquals(Get(item, "sha256"), Get(field, "outputSha256"))).Select(item => Str(item, "path")).Distinct().ToList();
            if (layouts.Count != 1 || outputs.Count != 1 || layouts[0] is not { } layout || outputs[0] is not { } output)
                return false;
            if (layout != output + ".bms-layout.json"
                || PyCompat.Digest(layout) != Str(field, "layoutSha256")
                || PyCompat.Digest(output) != Str(field, "outputSha256")
                || Str(field, "layoutPath") != Str(field, "outputPath") + ".bms-layout.json")
                return false;
            var proof = PyJson.Parse(PyCompat.ReadText(layout));
            var maps = Items(proof, "maps").Where(screen => PyJson.PyEquals(Get(screen, "name"), Get(field, "map"))
                && PyJson.PyEquals(Get(screen, "mapset"), Get(field, "mapset"))
                && Str(screen, "mode") is "OUT" or "INOUT").ToList();
            if (Str(proof, "producer") != "bms-symbolic-map/layout-v1" || Str(proof, "status") != "complete"
                || !PyJson.PyEquals(Get(proof, "outputSha256"), Get(field, "outputSha256")) || maps.Count != 1)
                return false;
            var fields = Items(maps[0], "fields").Where(item => PyJson.PyEquals(Get(item, "name"), Get(field, "field"))
                && PyJson.PyEquals(Get(item, "occurs"), 1)).ToList();
            var extended = Items(maps[0], "extendedAttributes").Select(PyJson.Str).ToList();
            var suffix = Str(field, "suffix");
            if (fields.Count != 1 || suffix != "A"
                && !extended.Contains(suffix switch { "C" => "COLOR", "H" => "HILIGHT", _ => throw PyError.Key(suffix ?? "None") }))
                return false;
            if (Has(field, "offset"))
            {
                IsInt(Get(field, "offset"), out var offset);
                IsInt(Get(fields[0], "offset"), out var start);
                if (DependencyEvidenceGate.BmsAttributeSuffix((int)(offset - start), extended!) != suffix) return false;
            }
        }
        return true;
    }

    private const string CopyStatement =
        @"(?<![A-Za-z0-9-])COPY\s+(['""]?)([A-Za-z0-9#@$_-]+)\1(?:\s+REPLACING\b(.*?))?\s*\.";
    private const string CopyOperand = "(?:==(.*?)==|([A-Za-z0-9-]+))";
    private static readonly Regex CopyPair = new(@"\G\s*" + CopyOperand + @"\s+BY\s+" + CopyOperand,
        RegexOptions.IgnoreCase | RegexOptions.Singleline | RegexOptions.CultureInvariant);

    internal static List<(string Source, string Target, bool Tag)>? CopyReplacements(string clause)
    {
        var pairs = new List<(string Source, string Target, bool Tag)>();
        var index = 0;
        while (clause[index..].Trim().Length > 0)
        {
            var match = CopyPair.Match(clause, index);
            if (!match.Success) return null;
            var source = (Group(match, 1) ?? Group(match, 2)!).Trim();
            var target = (Group(match, 3) ?? Group(match, 4)!).Trim();
            var tag = Full(@"\([A-Za-z0-9-]+\)|:[A-Za-z0-9-]+:", source) is not null;
            if (!(tag || Full("[A-Za-z0-9-]+", source) is not null) || Full("[A-Za-z0-9-]*", target) is null) return null;
            pairs.Add((source, target, tag));
            index = match.Index + match.Length;
        }
        if (pairs.Count == 0 || pairs.Any(pair => pairs.Any(other =>
                pair.Target.ToUpperInvariant().Contains(other.Source.ToUpperInvariant(), StringComparison.Ordinal))))
            return null;
        return pairs;
    }

    private static bool Fixed(string raw) => raw.Length > 6 && raw[..6].All(character => character == ' ' || PyCompat.IsDigit(character));

    internal static string? ReplaceCopyText(string text, List<(string Source, string Target, bool Tag)> pairs)
    {
        var lines = new List<string>();
        foreach (var raw in PyCompat.SplitLines(text))
        {
            var isFixed = Fixed(raw);
            if (isFixed && raw[6] is '*' or '/')
            {
                lines.Add(raw);
                continue;
            }
            var code = isFixed ? PyCompat.Slice(raw, 7, 72) : raw;
            var replaced = code;
            foreach (var (source, target, tag) in pairs)
            {
                var pattern = tag ? Regex.Escape(source) : "(?<![A-Za-z0-9_-])" + Regex.Escape(source) + "(?![A-Za-z0-9_-])";
                replaced = Rx(pattern, ignoreCase: true).Replace(replaced, _ => target);
            }
            if (replaced == code)
            {
                lines.Add(raw);
                continue;
            }
            if (isFixed && raw[6] != ' ' || replaced.Contains('\'') || replaced.Contains('"')) return null;
            lines.Add("       " + replaced);
        }
        return string.Join('\n', lines);
    }

    // Splice hash-bound PROCEDURE DIVISION COPY bodies in parser-input order; unsupported forms fail closed (null).
    internal static string? CopyExpandedText(string text, IReadOnlyList<JsonNode?> definitions, IReadOnlyList<string>? active = null)
    {
        active ??= [];
        if (active.Count > 16) return null;
        // Fixed-format sequence areas (columns 1-6 and 73+) are not program text.
        text = string.Join('\n', PyCompat.SplitLines(text).Select(line => Fixed(line) ? "      " + PyCompat.Slice(line, 6, 72) : line));
        var guarded = RektSourceIdentity.DependencyGuardText(text);
        var output = new StringBuilder();
        var cursor = 0;
        // Symbol uses are procedural; DATA DIVISION copybooks contribute declarations, not uses.
        var division = Rx(@"(?<![A-Za-z0-9-])PROCEDURE\s+DIVISION\b", ignoreCase: true).Match(guarded);
        var start = active.Count > 0 ? 0 : division.Success ? division.Index + division.Length : guarded.Length;
        var pattern = new Regex(CopyStatement, RegexOptions.IgnoreCase | RegexOptions.Singleline | RegexOptions.CultureInvariant);
        for (var match = pattern.Match(guarded, start); match.Success; match = match.NextMatch())
        {
            var name = match.Groups[2].Value.ToUpperInvariant();
            var bound = definitions.Where(item => Str(item, "kind") == "COPY"
                && PyCompat.Stem(Str(item, "name") ?? "").ToUpperInvariant() == name).ToList();
            if (bound.Count == 0) continue;
            var path = Str(bound[0], "path");
            if (bound.Count != 1 || active.Contains(name) || path is null || !File.Exists(path)
                || PyCompat.Digest(path) != Str(bound[0], "sha256"))
                return null;
            string? body = PyCompat.ReadText(path, PyCompat.Latin1);
            if (match.Groups[3].Success)
            {
                var pairs = CopyReplacements(match.Groups[3].Value);
                body = pairs is not null ? ReplaceCopyText(body, pairs) : null;
                if (body is null) return null;
            }
            var expanded = CopyExpandedText(body, definitions, [.. active, name]);
            if (expanded is null) return null;
            output.Append(guarded, cursor, match.Index - cursor).Append('\n').Append(expanded).Append('\n');
            cursor = match.Index + match.Length;
        }
        return output.Append(guarded[cursor..]).ToString();
    }

    public static bool BmsSourceUsesBound(JsonNode? binding, string source, IReadOnlyList<JsonNode?> definitions)
    {
        var fields = new HashSet<string>(StringComparer.Ordinal);
        foreach (var path in definitions.Where(item => Str(item, "kind") is "BMS_LAYOUT" or "CICS_MAP_LAYOUT")
                     .Select(item => Str(item, "path")).Distinct())
            foreach (var screen in Items(PyJson.Parse(PyCompat.ReadText(path!)), "maps"))
                foreach (var field in Items(screen, "fields"))
                    foreach (var suffix in new[] { "A", "C", "H" })
                        fields.Add(Str(field, "name") + suffix);
        List<string> Relevant(string text) =>
            Rx(@"\b[A-Z][A-Z0-9_-]*\b").Matches(RektSourceIdentity.DependencyGuardText(text).ToUpperInvariant())
                .Select(match => match.Value).Where(token => BmsSymbols.ContainsKey(token) || fields.Contains(token)).ToList();
        var expanded = CopyExpandedText(source, definitions);
        return expanded is not null && Relevant(expanded).SequenceEqual(
            Relevant(string.Join('\n', Items(binding, "usages").Select(usage => Str(usage, "sourceText")))));
    }

    public static void Generate(string sourceRoot, string outputRoot, string? configured, TextWriter stdout)
    {
        if (string.IsNullOrEmpty(configured)) return;
        if (!Path.IsPathRooted(configured))
            throw PyError.Value("REKT_PLATFORM_CONTRACTS must name an absolute contract path");
        var document = PyJson.Parse(PyCompat.ReadText(configured, PyCompat.StrictUtf8));
        Validate(document);
        var topLevelOnly = Environment.GetEnvironmentVariable("REKT_SOURCE_TOP_LEVEL_ONLY") == "true";
        foreach (var (name, contract) in (JsonObject)Get(document, "copybooks")!)
        {
            var candidates = RektEvidence.OriginalFiles(sourceRoot).Where(path =>
                (!topLevelOnly || RektEvidence.SamePath(Path.GetDirectoryName(path)!, sourceRoot))
                && PyCompat.Suffix(path).ToLowerInvariant() is ".cpy" or ".cpb" or ".copy"
                && PyCompat.Stem(path).ToUpperInvariant() == name).ToList();
            if (candidates.Any(path => RektEvidence.DefinitionProblem(path) is null))
            {
                stdout.WriteLine($"  Using supplied {name}; no platform projection generated.");
                continue;
            }
            var output = Path.Join(outputRoot, name + ".cpy");
            if (File.Exists(output) || Directory.Exists(output))
                throw PyError.Value($"Refusing to overwrite an existing analysis input: {output}");
            File.WriteAllBytes(output, Projection(contract));
            RektEvidence.Provenance(configured, output, "platform-event-contract/v1");
            stdout.WriteLine($"  {name}: explicit symbolic platform replacement; vendor values/layout NOT supplied.");
        }
    }
}
