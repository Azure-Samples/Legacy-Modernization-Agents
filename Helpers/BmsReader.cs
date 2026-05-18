// BmsReader.cs — deterministic parser for CICS BMS map source.
//
// BMS source uses three macro types:
//   DFHMSD  — mapset definition (header)
//   DFHMDI  — map definition (per screen)
//   DFHMDF  — field definition (per input/output field on a map)
//
// Output is REKT-shaped so it can flow through StructuralContextProvider exactly
// like a parsed COBOL program. Each BMS map becomes a "section" and each field a
// "data item" so downstream UI generators can render a form per map.

namespace CobolToQuarkusMigration.Helpers;

public sealed class BmsMapset
{
    public string SourceFile { get; set; } = "";
    public string Name { get; set; } = "";
    public List<BmsMap> Maps { get; set; } = new();
}

public sealed class BmsMap
{
    public string Name { get; set; } = "";
    public int? SizeRows { get; set; }
    public int? SizeCols { get; set; }
    public List<BmsField> Fields { get; set; } = new();
}

public sealed class BmsField
{
    public string Name { get; set; } = "";
    public int? Row { get; set; }
    public int? Col { get; set; }
    public int? Length { get; set; }
    public string? Attrib { get; set; }
    public string? Initial { get; set; }
    public string? Justify { get; set; }
    public string? Color { get; set; }
}

public static class BmsReader
{
    /// <summary>
    /// Parse a BMS source file. Returns null if the file is not BMS (no DFHMSD found).
    /// </summary>
    public static BmsMapset? ParseFile(string path)
    {
        if (!File.Exists(path)) return null;
        return Parse(File.ReadAllText(path), Path.GetFileName(path));
    }

    public static BmsMapset? Parse(string content, string sourceFile)
    {
        // Quick pre-check.
        if (content.IndexOf("DFHMSD", StringComparison.OrdinalIgnoreCase) < 0) return null;

        // 1) Strip COBOL/Assembler comments (col 1 = '*' on assembler-style BMS or col 7 = '*' on COBOL-style).
        var rawLines = content.Replace("\r", "").Split('\n');

        // 2) Join continuation lines. Assembler-style continuation has a non-blank in col 72 and the
        //    next line continues in col 16. We keep it simple: any line ending with a comma followed by
        //    whitespace + optional sequence-number area triggers a continuation.
        var stitched = StitchContinuations(rawLines);

        var mapset = new BmsMapset { SourceFile = sourceFile };
        BmsMap? currentMap = null;

        foreach (var line in stitched)
        {
            if (string.IsNullOrWhiteSpace(line)) continue;
            // Drop comment-only lines.
            var stripped = line.TrimStart();
            if (stripped.StartsWith("*")) continue;

            // Tokenise: "NAME    MACRO   ARG1=VAL,ARG2=VAL,…"
            // The label (mapset/map/field name) is in the first column area.
            var parsed = ParseMacroLine(line);
            if (parsed is null) continue;
            var (label, macro, args) = parsed.Value;

            switch (macro.ToUpperInvariant())
            {
                case "DFHMSD":
                    mapset.Name = label;
                    break;

                case "DFHMDI":
                    currentMap = new BmsMap { Name = label };
                    if (args.TryGetValue("SIZE", out var size))
                    {
                        var parts = size.Trim('(', ')').Split(',');
                        if (parts.Length == 2)
                        {
                            if (int.TryParse(parts[0], out var r)) currentMap.SizeRows = r;
                            if (int.TryParse(parts[1], out var c)) currentMap.SizeCols = c;
                        }
                    }
                    mapset.Maps.Add(currentMap);
                    break;

                case "DFHMDF":
                    if (currentMap is null) break;
                    var field = new BmsField { Name = label };
                    if (args.TryGetValue("POS", out var pos))
                    {
                        var parts = pos.Trim('(', ')').Split(',');
                        if (parts.Length == 2)
                        {
                            if (int.TryParse(parts[0], out var r)) field.Row = r;
                            if (int.TryParse(parts[1], out var c)) field.Col = c;
                        }
                    }
                    if (args.TryGetValue("LENGTH", out var len) && int.TryParse(len, out var l)) field.Length = l;
                    if (args.TryGetValue("ATTRB", out var attrib)) field.Attrib = attrib.Trim('(', ')');
                    if (args.TryGetValue("INITIAL", out var init)) field.Initial = init.Trim('\'');
                    if (args.TryGetValue("JUSTIFY", out var just)) field.Justify = just.Trim('(', ')');
                    if (args.TryGetValue("COLOR", out var col)) field.Color = col;
                    currentMap.Fields.Add(field);
                    break;
            }
        }

        return mapset.Maps.Count == 0 && string.IsNullOrEmpty(mapset.Name) ? null : mapset;
    }

    /// <summary>
    /// Converts a parsed mapset into a RektContext so downstream consumers can treat
    /// it uniformly. Each map becomes a "section", each field a level-05 data item.
    /// </summary>
    public static RektContext ToRektContext(BmsMapset m)
    {
        var ctx = new RektContext
        {
            Program = m.SourceFile,
            IsCopybook = false,
        };

        foreach (var map in m.Maps)
        {
            ctx.Sections.Add(new RektSection { Name = map.Name, StartLine = 0, EndLine = 0 });
            foreach (var f in map.Fields)
            {
                ctx.DataStructure.Add(new RektDataItem
                {
                    Level = 5,
                    Name = f.Name,
                    PicClause = f.Length.HasValue ? $"PIC X({f.Length})" : null,
                });
            }
        }
        return ctx;
    }

    // ── Internals ─────────────────────────────────────────────────────────

    private static IEnumerable<string> StitchContinuations(string[] lines)
    {
        var sb = new System.Text.StringBuilder();
        foreach (var raw in lines)
        {
            var line = raw.TrimEnd();
            // Continuation: line ends with a comma OR a non-blank in col 72.
            var endsWithComma = line.TrimEnd().EndsWith(",");
            if (sb.Length == 0) sb.Append(line);
            else sb.Append(line.TrimStart());
            if (!endsWithComma)
            {
                yield return sb.ToString();
                sb.Clear();
            }
        }
        if (sb.Length > 0) yield return sb.ToString();
    }

    private static (string label, string macro, Dictionary<string, string> args)? ParseMacroLine(string line)
    {
        // Find the macro token (one of DFHMSD/DFHMDI/DFHMDF).
        var idx = line.IndexOf("DFHM", StringComparison.OrdinalIgnoreCase);
        if (idx < 0) return null;
        var macroEnd = idx;
        while (macroEnd < line.Length && !char.IsWhiteSpace(line[macroEnd])) macroEnd++;
        var macro = line.Substring(idx, macroEnd - idx);

        // Label is whatever non-whitespace token sits before the macro.
        var labelArea = line.Substring(0, idx).Trim();
        var label = labelArea;

        // Args after the macro: comma-separated KEY=VAL, where VAL may be parenthesised.
        var argsText = macroEnd < line.Length ? line.Substring(macroEnd).Trim() : "";
        var args = ParseArgs(argsText);

        return (label, macro, args);
    }

    private static Dictionary<string, string> ParseArgs(string s)
    {
        var dict = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
        if (string.IsNullOrEmpty(s)) return dict;

        var i = 0;
        while (i < s.Length)
        {
            // Skip whitespace and stray commas.
            while (i < s.Length && (char.IsWhiteSpace(s[i]) || s[i] == ',')) i++;
            if (i >= s.Length) break;

            // Key
            var keyStart = i;
            while (i < s.Length && s[i] != '=' && s[i] != ',' && !char.IsWhiteSpace(s[i])) i++;
            var key = s.Substring(keyStart, i - keyStart);
            if (i >= s.Length || s[i] != '=') continue;
            i++; // skip '='

            // Value — handles (X,Y), 'literal', plain token
            string val;
            if (i < s.Length && s[i] == '(')
            {
                var depth = 1; var start = i; i++;
                while (i < s.Length && depth > 0)
                {
                    if (s[i] == '(') depth++;
                    else if (s[i] == ')') depth--;
                    i++;
                }
                val = s.Substring(start, i - start);
            }
            else if (i < s.Length && s[i] == '\'')
            {
                var start = i; i++;
                while (i < s.Length && s[i] != '\'') i++;
                if (i < s.Length) i++; // skip closing quote
                val = s.Substring(start, i - start);
            }
            else
            {
                var start = i;
                while (i < s.Length && s[i] != ',' && !char.IsWhiteSpace(s[i])) i++;
                val = s.Substring(start, i - start);
            }
            if (!string.IsNullOrWhiteSpace(key))
                dict[key] = val;
        }
        return dict;
    }
}
