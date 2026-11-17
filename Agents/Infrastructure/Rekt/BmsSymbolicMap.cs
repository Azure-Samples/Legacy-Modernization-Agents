using System.Text;
using System.Text.Json.Nodes;
using System.Text.RegularExpressions;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Rekt;

// Repositories ship CICS mapsets as *.bms but not the symbolic map copybooks the BMS assembler writes, so every
// program that COPYs one would parse against an empty stub. The copybook is named after the BMS source file (as
// z/OS builds name the member); a real copybook always wins, and a supplied one is only bound to a layout proof.
public static class BmsSymbolicMap
{
    public const string GeneratedMarker = "SYMBOLIC MAP GENERATED FROM BMS";
    internal const string StubMarker = "AUTO-GENERATED STUB COPYBOOK";
    private const int TioaPrefixBytes = 12;
    public const string LayoutRecipe = "bms-symbolic-map/layout-v1";
    private static readonly string[] CopybookExtensions = [".cpy", ".cpb", ".copy"];
    private static readonly string[] SkippedDirs = [".rekt-staging", ".preprocessed"];

    // Extended attribute bytes, in the order CICS lays them out, and their name suffixes.
    private static readonly (string Name, string Suffix)[] AttributeSuffixes =
        [("COLOR", "C"), ("PS", "P"), ("HILIGHT", "H"), ("VALIDN", "V"), ("OUTLINE", "U"), ("SOSI", "M"), ("TRANSP", "T")];
    private static readonly string[] ExtattYes = ["COLOR", "HILIGHT", "PS", "VALIDN"];

    private static readonly Encoding IgnoreUtf8 = Encoding.GetEncoding("utf-8", EncoderFallback.ExceptionFallback, new DecoderReplacementFallback(""));
    private static readonly Regex Picture = new(@"([AX9Z0B,./+*SVP-])(?:\((\d+)\))?", RegexOptions.CultureInvariant);
    private static readonly Regex FieldName = new(@"^[A-Z][A-Z0-9_-]{0,28}\z", RegexOptions.CultureInvariant);
    private static readonly Regex RenamedFiller = new(@"^FIL-R\d+\z", RegexOptions.CultureInvariant);

    internal sealed class BmsField
    {
        public string Name = "";
        public int Length;
        public int Occurs = 1;
        public string? Picin;
        public string? Picout;
        public int[]? Position;
        public string? Initial;
        public string[] Attributes = [];
    }

    internal sealed class BmsMap
    {
        public string Name = "";
        public string[] Attributes = [];
        public bool Tioapfx;
        public List<BmsField> Fields = [];
        public int[]? Size;
        public List<BmsField> ScreenFields = [];
    }

    internal sealed class BmsMapset
    {
        public string Name = "";
        public string Mode = "OUT";
        public string Lang = "ASM";
        public List<BmsMap> Maps = [];
        public bool StorageAuto;
    }

    // Operand values are a string or, for a parenthesised list, a string[] standing for a Python tuple.
    internal sealed record Statement(string? Label, string Macro, Dictionary<string, object> Operands);

    // The failures the original tool treated as an unparseable or unprovable map (OSError, UnicodeError, ValueError).
    public static bool Caught(Exception error) => error is IOException or UnauthorizedAccessException or DecoderFallbackException
        || error is PyError { Kind: "ValueError" or "UnicodeDecodeError" or "FileNotFoundError" or "IsADirectoryError" or "OSError" };

    private static string Text(object value) => value is string[] tuple ? TupleRepr(tuple) : (string)value;

    private static string TupleRepr(string[] tuple) => tuple.Length == 1
        ? "(" + PyJson.ReprString(tuple[0]) + ",)"
        : "(" + string.Join(", ", tuple.Select(PyJson.ReprString)) + ")";

    private static bool EndsInsideQuote(string text) => text.Count(character => character == '\'') % 2 == 1;

    // Join assembler continuation lines (non-blank column 72, resume in column 16).
    internal static List<string> LogicalLines(string text, bool strict = false)
    {
        var result = new List<string>();
        string? current = null;
        foreach (var raw in PyCompat.SplitLines(text))
        {
            var line = PyCompat.ExpandTabs(raw.TrimEnd('\r', '\n'));
            if (current is null && (line.Trim().Length == 0 || line.StartsWith('*') || line.StartsWith(".*", StringComparison.Ordinal)))
                continue;
            var continued = line.Length > 71 && line[71] != ' ';
            var body = PyCompat.Slice(line, 0, 71);
            if (current is null)
            {
                current = body;
            }
            else
            {
                var part = body.Length > 15 ? body[15..] : "";
                current += EndsInsideQuote(current) ? part : part.TrimStart();
            }
            if (continued)
            {
                if (!EndsInsideQuote(current)) current = current.TrimEnd();
                continue;
            }
            if (strict && EndsInsideQuote(current)) throw PyError.Value("Unterminated quoted BMS operand");
            result.Add(current.TrimEnd());
            current = null;
        }
        if (current is not null)
        {
            if (strict) throw PyError.Value("Unterminated BMS continuation");
            result.Add(current.TrimEnd());
        }
        return result;
    }

    private static List<string> SplitTopLevel(string text, char separator)
    {
        var parts = new List<string>();
        int depth = 0, start = 0;
        var quoted = false;
        for (var index = 0; index < text.Length; index++)
        {
            var character = text[index];
            if (character == '\'') quoted = !quoted;
            else if (!quoted)
            {
                if (character == '(') depth++;
                else if (character == ')') depth--;
                else if (character == separator && depth == 0)
                {
                    parts.Add(text[start..index]);
                    start = index + 1;
                }
            }
        }
        parts.Add(text[start..]);
        return parts;
    }

    // The operand field ends at the first blank outside quotes and parentheses.
    private static string OperandField(string text)
    {
        var depth = 0;
        var quoted = false;
        for (var index = 0; index < text.Length; index++)
        {
            var character = text[index];
            if (character == '\'') quoted = !quoted;
            else if (!quoted)
            {
                if (character == '(') depth++;
                else if (character == ')') depth--;
                else if (character == ' ' && depth == 0) return text[..index];
            }
        }
        return text;
    }

    private static object Value(string raw)
    {
        raw = raw.Trim();
        if (raw.StartsWith('\'') && raw.EndsWith('\'') && raw.Length >= 2) return raw[1..^1].Replace("''", "'");
        if (raw.StartsWith('(') && raw.EndsWith(')'))
            return SplitTopLevel(raw[1..^1], ',').Where(item => item.Trim().Length > 0).Select(item => item.Trim().ToUpperInvariant()).ToArray();
        return raw.ToUpperInvariant();
    }

    internal static Statement? ParseStatement(string line, bool strict = false)
    {
        string? label = null;
        var rest = line;
        if (!line.StartsWith(' '))
        {
            var space = line.IndexOf(' ');
            label = (space < 0 ? line : line[..space]).ToUpperInvariant();
            rest = space < 0 ? "" : line[(space + 1)..];
        }
        rest = rest.Trim();
        if (rest.Length == 0) return null;
        var split = rest.AsSpan().IndexOfAny(" \t\n\v\f\r\x1c\x1d\x1e\x1f\u0085\u00a0");
        var macro = (split < 0 ? rest : rest[..split]).ToUpperInvariant();
        var operands = new Dictionary<string, object>();
        if (split >= 0)
        {
            foreach (var operand in SplitTopLevel(OperandField(rest[split..].TrimStart()), ','))
            {
                var equals = operand.IndexOf('=');
                var key = equals < 0 ? operand : operand[..equals];
                var value = equals < 0 ? "" : operand[(equals + 1)..];
                if (strict && (equals < 0 || key.Trim().Length == 0 || value.Trim().Length == 0
                               || operands.ContainsKey(key.Trim().ToUpperInvariant())))
                    throw PyError.Value("Malformed or duplicate BMS operand: " + operand);
                if (equals >= 0 && key.Trim().Length > 0) operands[key.Trim().ToUpperInvariant()] = Value(value);
            }
        }
        return new Statement(label, macro, operands);
    }

    private static object? Get(Dictionary<string, object> operands, string key) => operands.GetValueOrDefault(key);

    private static bool Is(object? value, string text) => value is string actual && actual == text;

    private static string[]? Attributes(Dictionary<string, object> operands)
    {
        var dsatts = Get(operands, "DSATTS");
        string[]? names = dsatts is not null ? dsatts as string[] ?? [Text(dsatts)]
            : Is(Get(operands, "EXTATT"), "YES") ? ExtattYes : null;
        if (names is not null) return AttributeSuffixes.Select(pair => pair.Name).Where(names.Contains).ToArray();
        return Get(operands, "EXTATT") is "NO" or "MAPONLY" ? [] : null;
    }

    private static bool? Flag(Dictionary<string, object> operands, string key) =>
        Get(operands, key) is { } value ? Is(value, "YES") : null;

    // int(str(x)): surrounding whitespace, a sign and single underscores between digits are accepted.
    private static int PyInt(string text)
    {
        var trimmed = text.Trim();
        var digits = trimmed.TrimStart('+', '-');
        var valid = trimmed.Length - digits.Length <= 1 && digits.Length > 0 && char.IsAsciiDigit(digits[0])
                    && char.IsAsciiDigit(digits[^1]) && !digits.Contains("__", StringComparison.Ordinal)
                    && digits.All(character => char.IsAsciiDigit(character) || character == '_');
        if (!valid || !long.TryParse(trimmed.Replace("_", ""), out var value) || value is > int.MaxValue or < int.MinValue)
            throw PyError.Value("invalid literal for int() with base 10: " + PyJson.ReprString(text));
        return (int)value;
    }

    private static int Integer(Dictionary<string, object> operands, string key, int minimum = 1)
    {
        var value = PyInt(Get(operands, key) is { } raw ? Text(raw) : "1");
        if (value < minimum) throw PyError.Value($"{key} must be at least {minimum}, got {value}");
        return value;
    }

    private static int[]? Coordinates(Dictionary<string, object> operands, string key)
    {
        var value = Get(operands, key);
        if (value is null) return null;
        if (value is not string[] { Length: 2 } pair) throw PyError.Value($"{key} requires two coordinates, got {Text(value)}");
        var coordinates = pair.Select(PyInt).ToArray();
        if (coordinates.Any(part => part < 1)) throw PyError.Value($"{key} coordinates must be positive, got {Text(value)}");
        return coordinates;
    }

    internal static List<BmsMapset> ParseBms(string text)
    {
        var mapsets = new List<BmsMapset>();
        BmsMapset? mapset = null;
        string[] mapsetAttributes = [];
        var mapsetTioapfx = false;
        BmsMap? current = null;
        foreach (var line in LogicalLines(text))
        {
            var statement = ParseStatement(line);
            if (statement is null) continue;
            var operands = statement.Operands;
            if (statement.Macro == "DFHMSD")
            {
                if (Is(Get(operands, "TYPE"), "FINAL"))
                {
                    mapset = null;
                    current = null;
                    continue;
                }
                mapset = new BmsMapset
                {
                    Name = statement.Label ?? "",
                    Mode = Get(operands, "MODE") is { } mode ? Text(mode) : "OUT",
                    Lang = Get(operands, "LANG") is { } lang ? Text(lang) : "ASM",
                    StorageAuto = Is(Get(operands, "STORAGE"), "AUTO"),
                };
                mapsets.Add(mapset);
                mapsetAttributes = Attributes(operands) is { Length: > 0 } attributes ? attributes : [];
                mapsetTioapfx = Flag(operands, "TIOAPFX") ?? mapset.StorageAuto;
                current = null;
            }
            else if (statement.Macro == "DFHMDI" && mapset is not null && !string.IsNullOrEmpty(statement.Label))
            {
                current = new BmsMap
                {
                    Name = statement.Label,
                    Attributes = Attributes(operands) ?? mapsetAttributes,
                    Tioapfx = Flag(operands, "TIOAPFX") ?? mapsetTioapfx,
                    Size = Coordinates(operands, "SIZE"),
                };
                mapset.Maps.Add(current);
            }
            else if (statement.Macro == "DFHMDF" && current is not null)
            {
                if (!operands.ContainsKey("LENGTH"))
                    throw PyError.Value("DFHMDF requires an explicit supported LENGTH; no default size is invented");
                var length = Integer(operands, "LENGTH", minimum: 0);
                var occurs = Integer(operands, "OCCURS");
                if (length == 0 && !string.IsNullOrEmpty(statement.Label))
                    throw PyError.Value("Named zero-length symbolic fields are not supported: " + statement.Label);
                var picin = Get(operands, "PICIN");
                var picout = Get(operands, "PICOUT");
                var attributes = Get(operands, "ATTRB") ?? Array.Empty<string>();
                var screenField = new BmsField
                {
                    Name = statement.Label ?? "",
                    Length = length,
                    Occurs = occurs,
                    Picin = Truthy(picin) ? Text(picin!) : null,
                    Picout = Truthy(picout) ? Text(picout!) : null,
                    Position = Coordinates(operands, "POS"),
                    Initial = operands.TryGetValue("INITIAL", out var initial) ? Text(initial) : null,
                    Attributes = attributes as string[] ?? [Text(attributes)],
                };
                current.ScreenFields.Add(screenField);
                if (!string.IsNullOrEmpty(statement.Label)) current.Fields.Add(screenField);
            }
        }
        return mapsets;
    }

    private static bool Truthy(object? value) => value switch
    {
        null => false,
        string text => text.Length > 0,
        string[] tuple => tuple.Length > 0,
        _ => true,
    };

    private static bool IsCobol(BmsMapset mapset) => mapset.Lang.ToUpperInvariant() is "COBOL" or "COBOL2";

    private static JsonArray Strings(IEnumerable<string> values) => RektEvidence.ToArray(values);

    private static JsonNode? Pair(int[]? pair) => pair is null ? null : new JsonArray(pair[0], pair[1]);

    private static JsonObject FieldJson(BmsField field) => new()
    {
        ["name"] = field.Name,
        ["length"] = field.Length,
        ["occurs"] = field.Occurs,
        ["picin"] = field.Picin,
        ["picout"] = field.Picout,
        ["position"] = Pair(field.Position),
        ["initial"] = field.Initial,
        ["attributes"] = Strings(field.Attributes),
    };

    private static JsonObject MapsetJson(BmsMapset mapset) => new()
    {
        ["name"] = mapset.Name,
        ["mode"] = mapset.Mode,
        ["lang"] = mapset.Lang,
        ["maps"] = new JsonArray(mapset.Maps.Select(map => (JsonNode?)new JsonObject
        {
            ["name"] = map.Name,
            ["attributes"] = Strings(map.Attributes),
            ["tioapfx"] = map.Tioapfx,
            ["fields"] = new JsonArray(map.Fields.Select(field => (JsonNode?)FieldJson(field)).ToArray()),
            ["size"] = Pair(map.Size),
            ["screen_fields"] = new JsonArray(map.ScreenFields.Select(field => (JsonNode?)FieldJson(field)).ToArray()),
        }).ToArray()),
        ["storage_auto"] = mapset.StorageAuto,
    };

    internal static JsonObject ScreenDefinition(string source, string sourceDir)
    {
        var data = PyCompat.ReadBytes(source);
        return new JsonObject
        {
            ["path"] = PyCompat.RelPosix(source, sourceDir),
            ["sha256"] = PyCompat.Digest(data),
            ["resourceName"] = PyCompat.Stem(source).ToUpperInvariant(),
            ["mapsets"] = new JsonArray(ParseBms(PyCompat.Decode(data)).Where(IsCobol).Select(mapset => (JsonNode?)MapsetJson(mapset)).ToArray()),
        };
    }

    private static List<string> InputEntries(BmsField field, string[] attributes, int level, string indent)
    {
        var lines = new List<string>
        {
            $"{indent}{level:00}  {field.Name}L    COMP  PIC  S9(4).",
            $"{indent}{level:00}  {field.Name}F    PICTURE X.",
            $"{indent}{level:00}  FILLER REDEFINES {field.Name}F.",
            $"{indent}  {level + 1:00}  {field.Name}A    PICTURE X.",
        };
        if (attributes.Length > 0) lines.Add($"{indent}{level:00}  FILLER   PICTURE X({attributes.Length}).");
        lines.Add($"{indent}{level:00}  {field.Name}I  PIC {field.Picin ?? $"X({field.Length})"}.");
        return lines;
    }

    private static List<string> OutputEntries(BmsField field, string[] attributes, int level, string indent)
    {
        var lines = new List<string> { $"{indent}{level:00}  FILLER PICTURE X(3)." };
        foreach (var name in attributes)
            lines.Add($"{indent}{level:00}  {field.Name}{AttributeSuffixes.First(pair => pair.Name == name).Suffix}    PICTURE X.");
        lines.Add($"{indent}{level:00}  {field.Name}O  PIC {field.Picout ?? $"X({field.Length})"}.");
        return lines;
    }

    private static List<string> Structure(BmsMap map, bool input, string header, ref int counter)
    {
        var indent = new string(' ', 11);
        var lines = new List<string> { header };
        if (map.Tioapfx) lines.Add($"{indent}02  FILLER PIC X({TioaPrefixBytes}).");
        if (map.Fields.Count == 0 && !map.Tioapfx)
            throw PyError.Value($"{map.Name}: empty symbolic storage cannot be represented without inventing a byte");
        foreach (var field in map.Fields)
        {
            if (field.Occurs > 1)
            {
                counter++;
                lines.Add($"{indent}02  DFHMS{counter} OCCURS {field.Occurs} TIMES.");
                lines.AddRange(input ? InputEntries(field, map.Attributes, 3, indent + "  ") : OutputEntries(field, map.Attributes, 3, indent + "  "));
            }
            else
            {
                lines.AddRange(input ? InputEntries(field, map.Attributes, 2, indent) : OutputEntries(field, map.Attributes, 2, indent));
            }
        }
        return lines;
    }

    internal static string RenderCopybook(List<BmsMapset> mapsets, string sourceName)
    {
        var lines = new List<string>
        {
            $"      *> {GeneratedMarker} {sourceName}",
            "      *> Written by the rekt-bms-map command; edit the .bms instead.",
        };
        var counter = 0;
        foreach (var mapset in mapsets)
        {
            var mode = mapset.Mode.ToUpperInvariant();
            string? baseName = null;
            foreach (var map in mapset.Maps)
            {
                var hasIn = mode is "IN" or "INOUT";
                var hasOut = mode is "OUT" or "INOUT";
                if (hasIn)
                {
                    var header = $"       01  {map.Name}I";
                    if (baseName is { Length: > 0 } && !mapset.StorageAuto) header += $" REDEFINES {baseName}";
                    lines.AddRange(Structure(map, true, header + ".", ref counter));
                    baseName = string.IsNullOrEmpty(baseName) ? map.Name + "I" : baseName;
                }
                if (hasOut)
                {
                    var header = hasIn
                        ? $"       01  {map.Name}O REDEFINES {map.Name}I."
                        : $"       01  {map.Name}O" + (baseName is { Length: > 0 } && !mapset.StorageAuto ? $" REDEFINES {baseName}" : "") + ".";
                    lines.AddRange(Structure(map, false, header, ref counter));
                    baseName = string.IsNullOrEmpty(baseName) ? map.Name + "O" : baseName;
                }
            }
        }
        return string.Join("\n", lines) + "\n";
    }

    private static void PictureBytes(string picture, int length)
    {
        var upper = picture.ToUpperInvariant();
        var parts = Picture.Matches(upper).Select(match => (Symbol: match.Groups[1].Value, Count: match.Groups[2].Value)).ToList();
        if (parts.Count == 0 || string.Concat(parts.Select(part => part.Symbol + (part.Count.Length > 0 ? $"({part.Count})" : ""))) != upper)
            throw PyError.Value("Unsupported symbolic-map picture: " + picture);
        var size = parts.Where(part => !"SVP".Contains(part.Symbol, StringComparison.Ordinal))
            .Sum(part => part.Count.Length > 0 ? long.Parse(part.Count, System.Globalization.CultureInfo.InvariantCulture) : 1);
        if (size != length) throw PyError.Value($"Picture {picture} uses {size} bytes, but LENGTH is {length}");
    }

    private static readonly Dictionary<string, HashSet<string>> Allowed = new()
    {
        ["DFHMSD"] = ["TYPE", "MODE", "LANG", "STORAGE", "CTRL", "EXTATT", "TERM", "TIOAPFX",
                      "DSATTS", "MAPATTS", "COLOR", "HILIGHT", "OUTLINE", "PS", "SOSI", "TRANSP", "VALIDN"],
        ["DFHMDI"] = ["SIZE", "COLUMN", "LINE", "CTRL", "TIOAPFX", "EXTATT", "DSATTS", "MAPATTS",
                      "COLOR", "HILIGHT", "OUTLINE", "PS", "SOSI", "TRANSP", "VALIDN"],
        ["DFHMDF"] = ["POS", "LENGTH", "OCCURS", "PICIN", "PICOUT", "INITIAL", "ATTRB",
                      "JUSTIFY", "COLOR", "HILIGHT", "OUTLINE", "PS", "SOSI", "TRANSP", "VALIDN"],
        ["END"] = [],
    };

    // Proves the supported symbolic storage recipe against exact original and generated bytes.
    public static JsonObject CompleteLayoutEvidence(string source, string copybook)
    {
        var data = PyCompat.ReadBytes(source);
        var text = PyCompat.Decode(data);
        var active = false;
        var currentMap = false;
        foreach (var line in LogicalLines(text, strict: true))
        {
            // HLASM listing-control instructions only format the assembly listing.
            var first = line.Trim().Split((char[]?)null, 2, StringSplitOptions.RemoveEmptyEntries);
            if (first.Length > 0 && first[0].ToUpperInvariant() is "TITLE" or "EJECT" or "SPACE") continue;
            var statement = ParseStatement(line, strict: true);
            if (statement is null) continue;
            var values = statement.Operands;
            if (!Allowed.TryGetValue(statement.Macro, out var allowed) || values.Keys.Any(key => !allowed.Contains(key)))
                throw PyError.Value("Unsupported symbolic layout statement or operands: " + line.Trim());
            if (values.TryGetValue("EXTATT", out var extatt) && extatt is not ("YES" or "NO" or "MAPONLY"))
                throw PyError.Value("Unsupported EXTATT: " + Text(extatt));
            foreach (var key in new[] { "DSATTS", "MAPATTS" })
            {
                var raw = Get(values, key) ?? Array.Empty<string>();
                var names = raw as string[] ?? [(string)raw];
                if (names.Any(name => AttributeSuffixes.All(pair => pair.Name != name)))
                    throw PyError.Value($"Unsupported {key}: {TupleRepr(names)}");
            }
            if (values.TryGetValue("TIOAPFX", out var prefix) && prefix is not ("YES" or "NO"))
                throw PyError.Value("Unsupported TIOAPFX: " + Text(prefix));
            if (values.TryGetValue("PS", out var ps) && !Is(ps, "BASE"))
                throw PyError.Value("Programmed-symbol character sets need separate encoding/layout support");
            switch (statement.Macro)
            {
                case "DFHMSD" when Is(Get(values, "TYPE"), "FINAL"):
                    if (!active) throw PyError.Value("DFHMSD FINAL without an active mapset");
                    active = currentMap = false;
                    break;
                case "DFHMSD":
                    if (active || string.IsNullOrEmpty(statement.Label)) throw PyError.Value("Nested or unnamed mapset");
                    // &&SYSPARM is the escaped form used where the source passes through another substitution.
                    if (Get(values, "TYPE") is not ("&SYSPARM" or "&&SYSPARM" or "MAP" or "DSECT"))
                        throw PyError.Value("Unsupported mapset assembly type");
                    if (Get(values, "LANG") is not ("COBOL" or "COBOL2")) throw PyError.Value("Only COBOL symbolic layouts are supported");
                    if ((Get(values, "MODE") ?? "OUT") is not ("IN" or "OUT" or "INOUT")) throw PyError.Value("Unsupported symbolic map mode");
                    if (Get(values, "STORAGE") is not (null or "AUTO")) throw PyError.Value("Unsupported symbolic map storage");
                    if ((Get(values, "TERM") ?? "3270") is not ("3270" or "3270-1" or "3270-2"))
                        throw PyError.Value("Only 3270 symbolic layouts are supported");
                    active = true;
                    currentMap = false;
                    break;
                case "DFHMDI":
                    if (!active || string.IsNullOrEmpty(statement.Label)) throw PyError.Value("DFHMDI without an active mapset or map name");
                    currentMap = true;
                    break;
                case "DFHMDF":
                    if (!active || !currentMap) throw PyError.Value("DFHMDF without an active map");
                    var attributes = Get(values, "ATTRB") ?? Array.Empty<string>();
                    if ((attributes as string[] ?? [(string)attributes]).Contains("DET"))
                        throw PyError.Value("Detectable-field symbolic input layout needs separate support");
                    break;
                case "END" when active:
                    throw PyError.Value("END before DFHMSD FINAL");
            }
        }
        if (active) throw PyError.Value("Missing DFHMSD FINAL");
        var mapsets = ParseBms(text);
        if (mapsets.Count == 0 || mapsets.Any(mapset => mapset.Maps.Count == 0)) throw PyError.Value("No complete mapset");
        var maps = new JsonArray();
        foreach (var mapset in mapsets)
        {
            var names = new HashSet<string>();
            foreach (var screen in mapset.Maps)
            {
                if (!names.Add(screen.Name)) throw PyError.Value("Duplicate symbolic map name: " + screen.Name);
                var offset = screen.Tioapfx ? TioaPrefixBytes : 0;
                var fields = new JsonArray();
                var seen = new HashSet<string>();
                foreach (var entry in screen.Fields)
                {
                    if (!FieldName.IsMatch(entry.Name) || !seen.Add(entry.Name))
                        throw PyError.Value("Duplicate or unsupported symbolic field name: " + entry.Name);
                    if (entry.Length is < 1 or > 256) throw PyError.Value("Unsupported symbolic field length: " + entry.Length);
                    foreach (var picture in new[] { entry.Picin, entry.Picout })
                        if (!string.IsNullOrEmpty(picture)) PictureBytes(picture, entry.Length);
                    var stride = 3 + screen.Attributes.Length + entry.Length;
                    fields.Add(new JsonObject
                    {
                        ["name"] = entry.Name,
                        ["offset"] = offset,
                        ["dataOffset"] = offset + 3 + screen.Attributes.Length,
                        ["length"] = entry.Length,
                        ["occurs"] = entry.Occurs,
                        ["stride"] = stride,
                    });
                    offset += stride * entry.Occurs;
                }
                maps.Add(new JsonObject
                {
                    ["name"] = screen.Name,
                    ["mapset"] = mapset.Name,
                    ["mode"] = mapset.Mode,
                    ["byteLength"] = offset,
                    ["extendedAttributes"] = Strings(screen.Attributes),
                    ["fields"] = fields,
                });
            }
        }
        var rendered = PyCompat.Utf8.GetBytes(RenderCopybook(mapsets, Path.GetFileName(source)));
        var output = PyCompat.ReadBytes(copybook);
        string match;
        if (output.AsSpan().SequenceEqual(rendered)) match = "exact-render";
        else if (SymbolicTokens(PyCompat.Latin1.GetString(output)).SequenceEqual(SymbolicTokens(PyCompat.Utf8.GetString(rendered))))
            match = "token-equivalent";
        else throw PyError.Value("Symbolic copybook differs from its original BMS recipe");
        return new JsonObject
        {
            ["schemaVersion"] = 1,
            ["producer"] = LayoutRecipe,
            ["status"] = "complete",
            ["sourceSha256"] = PyCompat.Digest(data),
            ["outputSha256"] = PyCompat.Digest(output),
            ["copybookMatch"] = match,
            ["maps"] = maps,
        };
    }

    // Fixed-format COBOL tokens, ignoring comments, sequence/identification areas and spacing.
    // FIL-Rnnn is the preprocessor's layout-neutral rename of FILLER REDEFINES.
    public static List<string> SymbolicTokens(string text)
    {
        var tokens = new List<string>();
        foreach (var line in PyCompat.SplitLines(text))
        {
            if (line.Length > 6 && line[6] is '*' or '/') continue;
            tokens.AddRange(PyCompat.Slice(line, 7, 72).Split((char[]?)null, StringSplitOptions.RemoveEmptyEntries)
                .Select(token => RenamedFiller.IsMatch(token) ? "FILLER" : token));
        }
        return tokens;
    }

    private static bool IsCopybook(string path) => CopybookExtensions.Contains(PyCompat.Suffix(path).ToLowerInvariant());

    // Prove a supplied symbolic map's preprocessed copy against its same-named BMS source.
    private static void BindSupplied(string bms, string name, string outputDir, List<string> warnings)
    {
        if (!Directory.Exists(outputDir)) return;
        var copies = Directory.EnumerateFileSystemEntries(outputDir)
            .Where(path => File.Exists(path) && PyCompat.Stem(path).ToUpperInvariant() == name && IsCopybook(path))
            .Select(RektEvidence.Norm)
            .Where(path => RektEvidence.CurrentTransform(path) is { } original && IsCopybook(original))
            .ToList();
        if (copies.Count != 1) return;
        var copy = copies[0];
        JsonObject proof;
        try
        {
            proof = CompleteLayoutEvidence(bms, copy);
        }
        catch (Exception error) when (Caught(error))
        {
            warnings.Add($"{bms}: supplied {Path.GetFileName(copy)} is not proven equivalent; layout unbound ({error.Message})");
            return;
        }
        var record = RektEvidence.ReadJson(copy + ".preprocess.json")!.AsObject();
        record["bmsSourcePath"] = PyCompat.Resolve(bms);
        record["bmsSourceSha256"] = PyCompat.Digest(bms);
        RektEvidence.WriteJson(copy + ".preprocess.json", record);
        RektEvidence.WriteJson(copy + ".bms-layout.json", proof);
    }

    private static bool KeepDir(string name) =>
        Environment.GetEnvironmentVariable("REKT_SOURCE_TOP_LEVEL_ONLY") != "true"
        && !SkippedDirs.Contains(name) && !name.StartsWith(".convert-", StringComparison.Ordinal);

    private static HashSet<string> RealCopybooks(string sourceDir)
    {
        var names = new HashSet<string>();
        foreach (var (root, files) in PyCompat.Walk(sourceDir, KeepDir))
            foreach (var file in files)
                if (IsCopybook(file) && string.IsNullOrEmpty(RektEvidence.DefinitionProblem(Path.Join(root, file))))
                    names.Add(PyCompat.Stem(file).ToUpperInvariant());
        return names;
    }

    private static List<string> BmsSources(string sourceDir) =>
        PyCompat.Walk(sourceDir, KeepDir)
            .SelectMany(entry => entry.Files.Where(file => file.ToLowerInvariant().EndsWith(".bms", StringComparison.Ordinal))
                .Select(file => RektEvidence.Norm(Path.Join(entry.Root, file))))
            .Order(RektEvidence.PathOrder)
            .ToList();

    private static bool Replaceable(string path)
    {
        if (!File.Exists(path) && !Directory.Exists(path)) return true;
        var head = PyCompat.Slice(IgnoreUtf8.GetString(PyCompat.ReadBytes(path)).Replace("\r\n", "\n").Replace('\r', '\n'), 0, 2000);
        return head.Contains(GeneratedMarker, StringComparison.Ordinal) || head.Contains(StubMarker, StringComparison.Ordinal);
    }

    // Writes one copybook per BMS source and the screen definitions the portal and parsers read.
    public static (List<string> Generated, List<string> Warnings) Generate(string sourceDir, string outputDir)
    {
        sourceDir = RektEvidence.Norm(sourceDir);
        outputDir = RektEvidence.Norm(outputDir);
        var real = RealCopybooks(sourceDir);
        var generated = new List<string>();
        var warnings = new List<string>();
        var claimed = new Dictionary<string, string>();
        var screens = new JsonArray();
        foreach (var bms in BmsSources(sourceDir))
        {
            var name = PyCompat.Stem(bms).ToUpperInvariant();
            JsonObject screen;
            try
            {
                screen = ScreenDefinition(bms, sourceDir);
            }
            catch (Exception error) when (Caught(error))
            {
                warnings.Add($"{bms}: could not parse screen definition ({error.Message})");
                continue;
            }
            if (screen["mapsets"]!.AsArray().Any(mapset => mapset!["maps"]!.AsArray().Count > 0)) screens.Add(screen);
            if (real.Contains(name))
            {
                if (claimed.TryGetValue(name, out var owner))
                {
                    warnings.Add($"{bms} and {owner} both describe supplied {name}; layout unbound");
                    continue;
                }
                BindSupplied(bms, name, outputDir, warnings);
                claimed[name] = bms;
                continue;
            }
            if (claimed.TryGetValue(name, out var first))
            {
                warnings.Add($"{bms} and {first} both produce {name}.cpy; kept the first");
                continue;
            }
            List<BmsMapset> mapsets;
            try
            {
                mapsets = ParseBms(IgnoreUtf8.GetString(PyCompat.ReadBytes(bms)).Replace("\r\n", "\n").Replace('\r', '\n'));
            }
            catch (Exception error) when (Caught(error))
            {
                warnings.Add($"{bms}: could not parse ({error.Message})");
                continue;
            }
            mapsets = mapsets.Where(IsCobol).ToList();
            if (!mapsets.Any(mapset => mapset.Maps.Count > 0)) continue;
            var target = Path.Join(outputDir, name + ".cpy");
            if (!Replaceable(target)) continue;
            Directory.CreateDirectory(outputDir);
            var proofPath = target + ".bms-layout.json";
            RektEvidence.PreserveAll([target, target + ".preprocess.json", proofPath], outputDir);
            try
            {
                File.WriteAllText(target, RenderCopybook(mapsets, Path.GetFileName(bms)), PyCompat.Utf8);
            }
            catch (Exception error) when (error is IOException or UnauthorizedAccessException or PyError { Kind: "ValueError" })
            {
                warnings.Add($"{bms}: could not generate symbolic storage ({error.Message})");
                continue;
            }
            RektEvidence.Provenance(bms, target, producer: "bms-symbolic-map");
            JsonObject proof;
            try
            {
                proof = CompleteLayoutEvidence(bms, target);
            }
            catch (Exception error) when (Caught(error))
            {
                warnings.Add($"{bms}: symbolic storage remains partial ({error.Message})");
                proof = new JsonObject
                {
                    ["schemaVersion"] = 1,
                    ["producer"] = LayoutRecipe,
                    ["status"] = "partial",
                    ["diagnostics"] = new JsonArray(error.Message),
                };
            }
            RektEvidence.WriteJson(proofPath, proof);
            claimed[name] = bms;
            generated.Add(name);
        }
        Directory.CreateDirectory(outputDir);
        var definitions = new JsonObject
        {
            ["schemaVersion"] = 1,
            ["producer"] = "bms-symbolic-map/screen-v1",
            ["sourceRoot"] = Path.GetRelativePath(PyCompat.Resolve(outputDir), PyCompat.Resolve(sourceDir)),
            ["definitions"] = screens,
        };
        File.WriteAllText(Path.Join(outputDir, "bms-screen-definitions.json"), PyJson.Dumps(definitions, indent: 2) + "\n", PyCompat.Utf8);
        return (generated, warnings);
    }
}
