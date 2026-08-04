// Parses common IMS DBD and PSB GEN macros into structural context.

namespace CobolToQuarkusMigration.Helpers;

public sealed class ImsDbd
{
    public string SourceFile { get; set; } = "";
    public string Name { get; set; } = "";
    public string? Access { get; set; }
    public List<ImsSegment> Segments { get; set; } = new();
}

public sealed class ImsSegment
{
    public string Name { get; set; } = "";
    public int? Bytes { get; set; }
    public string? Parent { get; set; }              // "0" for root
    public List<ImsField> Fields { get; set; } = new();
}

public sealed class ImsField
{
    public string Name { get; set; } = "";
    public bool IsSequence { get; set; }              // FIELD NAME=(X,SEQ,…)
    public int? Bytes { get; set; }
    public int? Start { get; set; }
    public string? Type { get; set; }                 // C (char), P (packed), …
}

public sealed class ImsPsb
{
    public string SourceFile { get; set; } = "";
    public string Name { get; set; } = "";
    public string? Lang { get; set; }
    public List<ImsPcb> Pcbs { get; set; } = new();
}

public sealed class ImsPcb
{
    public string Type { get; set; } = "DB";
    public string DbdName { get; set; } = "";
    public int? KeyLen { get; set; }
    public string? Procopt { get; set; }
    public List<ImsSenseg> Sensegs { get; set; } = new();
}

public sealed class ImsSenseg
{
    public string Name { get; set; } = "";
    public string? Parent { get; set; }
    public string? Procopt { get; set; }
}

public static class ImsDbdReader
{
    public static ImsDbd? ParseFile(string path)
    {
        if (!File.Exists(path)) return null;
        return Parse(File.ReadAllText(path), Path.GetFileName(path));
    }

    public static ImsDbd? Parse(string content, string sourceFile)
    {
        if (content.IndexOf("DBD", StringComparison.OrdinalIgnoreCase) < 0) return null;

        var dbd = new ImsDbd { SourceFile = sourceFile };
        ImsSegment? currentSegment = null;

        foreach (var line in StitchContinuations(content.Replace("\r", "").Split('\n')))
        {
            if (string.IsNullOrWhiteSpace(line)) continue;
            var stripped = line.TrimStart();
            if (stripped.StartsWith("*")) continue;

            var (label, macro, args) = ParseMacroLine(line);

            switch (macro?.ToUpperInvariant())
            {
                case "DBD":
                    dbd.Name = TryGet(args, "NAME") ?? label;
                    dbd.Access = TryGet(args, "ACCESS")?.Trim('(', ')');
                    break;

                case "SEGM":
                    currentSegment = new ImsSegment
                    {
                        Name = TryGet(args, "NAME") ?? "",
                        Bytes = TryGetInt(args, "BYTES"),
                        Parent = TryGet(args, "PARENT"),
                    };
                    dbd.Segments.Add(currentSegment);
                    break;

                case "FIELD":
                    if (currentSegment is null) break;
                    var nameArg = TryGet(args, "NAME") ?? "";
                    var (fname, isSeq) = ParseFieldName(nameArg);
                    currentSegment.Fields.Add(new ImsField
                    {
                        Name = fname,
                        IsSequence = isSeq,
                        Bytes = TryGetInt(args, "BYTES"),
                        Start = TryGetInt(args, "START"),
                        Type = TryGet(args, "TYPE"),
                    });
                    break;
            }
        }

        return dbd.Segments.Count == 0 && string.IsNullOrEmpty(dbd.Name) ? null : dbd;
    }

    public static RektContext ToRektContext(ImsDbd dbd)
    {
        var ctx = new RektContext { Program = dbd.SourceFile, IsCopybook = true };
        foreach (var seg in dbd.Segments)
        {
            ctx.Sections.Add(new RektSection { Name = seg.Name });
            var groupItem = new RektDataItem
            {
                Level = 1,
                Name = seg.Name,
            };
            foreach (var f in seg.Fields)
            {
                groupItem.Children.Add(new RektDataItem
                {
                    Level = 5,
                    Name = f.Name + (f.IsSequence ? " [SEQ]" : ""),
                    PicClause = f.Bytes.HasValue ? FormatPic(f.Type, f.Bytes.Value) : null,
                });
            }
            ctx.DataStructure.Add(groupItem);
        }
        return ctx;
    }

    private static string FormatPic(string? type, int bytes) =>
        type?.ToUpperInvariant() switch
        {
            "C" => $"PIC X({bytes})",
            "P" => $"PIC S9({bytes * 2 - 1}) COMP-3",
            "H" => $"PIC S9(4) COMP",
            "F" => $"PIC S9(9) COMP",
            _ => $"PIC X({bytes})",
        };

    private static (string, bool) ParseFieldName(string raw)
    {
        if (string.IsNullOrEmpty(raw)) return ("", false);
        var t = raw.Trim('(', ')').Split(',');
        var name = t[0].Trim();
        var isSeq = t.Skip(1).Any(x => x.Trim().Equals("SEQ", StringComparison.OrdinalIgnoreCase));
        return (name, isSeq);
    }

    internal static IEnumerable<string> StitchContinuations(string[] lines)
    {
        var sb = new System.Text.StringBuilder();
        foreach (var raw in lines)
        {
            var line = raw.TrimEnd();
            if (sb.Length == 0) sb.Append(line);
            else sb.Append(line.TrimStart());
            if (!line.TrimEnd().EndsWith(","))
            {
                yield return sb.ToString();
                sb.Clear();
            }
        }
        if (sb.Length > 0) yield return sb.ToString();
    }

    internal static (string label, string? macro, Dictionary<string, string> args) ParseMacroLine(string line)
    {
        // Macros are uppercase tokens after the label area.
        var trimmed = line.TrimStart();
        var labelEnd = 0;
        if (!char.IsWhiteSpace(line[0]))
        {
            // Label in col 1
            labelEnd = 0;
            while (labelEnd < line.Length && !char.IsWhiteSpace(line[labelEnd])) labelEnd++;
        }
        var label = line.Substring(0, labelEnd).Trim();
        var rest = line.Substring(labelEnd).TrimStart();
        if (string.IsNullOrEmpty(rest)) return (label, null, new());

        var macroEnd = 0;
        while (macroEnd < rest.Length && !char.IsWhiteSpace(rest[macroEnd])) macroEnd++;
        var macro = rest.Substring(0, macroEnd);
        var argsText = macroEnd < rest.Length ? rest.Substring(macroEnd).Trim() : "";
        return (label, macro, ParseArgs(argsText));
    }

    internal static Dictionary<string, string> ParseArgs(string s)
    {
        // Same parser as BMS: KEY=VAL, KEY=(A,B), KEY='literal'
        var dict = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
        if (string.IsNullOrEmpty(s)) return dict;

        var i = 0;
        while (i < s.Length)
        {
            while (i < s.Length && (char.IsWhiteSpace(s[i]) || s[i] == ',')) i++;
            if (i >= s.Length) break;

            var keyStart = i;
            while (i < s.Length && s[i] != '=' && s[i] != ',' && !char.IsWhiteSpace(s[i])) i++;
            var key = s.Substring(keyStart, i - keyStart);
            if (i >= s.Length || s[i] != '=') continue;
            i++;

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
                if (i < s.Length) i++;
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

    internal static string? TryGet(Dictionary<string, string> args, string key)
        => args.TryGetValue(key, out var v) ? v : null;

    internal static int? TryGetInt(Dictionary<string, string> args, string key)
        => args.TryGetValue(key, out var v) && int.TryParse(v.Trim('(', ')'), out var i) ? i : null;
}

public static class ImsPsbReader
{
    public static ImsPsb? ParseFile(string path)
    {
        if (!File.Exists(path)) return null;
        return Parse(File.ReadAllText(path), Path.GetFileName(path));
    }

    public static ImsPsb? Parse(string content, string sourceFile)
    {
        if (content.IndexOf("PSBGEN", StringComparison.OrdinalIgnoreCase) < 0 &&
            content.IndexOf("PCB", StringComparison.OrdinalIgnoreCase) < 0)
            return null;

        var psb = new ImsPsb { SourceFile = sourceFile };
        ImsPcb? currentPcb = null;

        foreach (var line in ImsDbdReader.StitchContinuations(content.Replace("\r", "").Split('\n')))
        {
            if (string.IsNullOrWhiteSpace(line)) continue;
            var stripped = line.TrimStart();
            if (stripped.StartsWith("*")) continue;

            var (_, macro, args) = ImsDbdReader.ParseMacroLine(line);

            switch (macro?.ToUpperInvariant())
            {
                case "PCB":
                    currentPcb = new ImsPcb
                    {
                        Type = ImsDbdReader.TryGet(args, "TYPE") ?? "DB",
                        DbdName = ImsDbdReader.TryGet(args, "DBDNAME") ?? "",
                        KeyLen = ImsDbdReader.TryGetInt(args, "KEYLEN"),
                        Procopt = ImsDbdReader.TryGet(args, "PROCOPT"),
                    };
                    psb.Pcbs.Add(currentPcb);
                    break;

                case "SENSEG":
                    if (currentPcb is null) break;
                    currentPcb.Sensegs.Add(new ImsSenseg
                    {
                        Name = ImsDbdReader.TryGet(args, "NAME") ?? "",
                        Parent = ImsDbdReader.TryGet(args, "PARENT"),
                        Procopt = ImsDbdReader.TryGet(args, "PROCOPT"),
                    });
                    break;

                case "PSBGEN":
                    psb.Name = ImsDbdReader.TryGet(args, "PSBNAME") ?? "";
                    psb.Lang = ImsDbdReader.TryGet(args, "LANG");
                    break;
            }
        }

        return psb.Pcbs.Count == 0 && string.IsNullOrEmpty(psb.Name) ? null : psb;
    }
}
