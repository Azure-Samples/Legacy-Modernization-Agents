using System.Text;
using System.Text.RegularExpressions;

namespace CobolToQuarkusMigration.Estate;

public sealed record ScannedReference(string Target, string TargetKind, string Kind, string? Via, IReadOnlyList<EstateEvidence> Evidence);

public sealed record ProgramScan
{
    public required string RelativePath { get; init; }
    public string? ProgramId { get; init; }
    public List<ScannedReference> References { get; } = [];
    // DD names from SELECT ... ASSIGN, which is what ties the program to a JCL step's DD statements.
    public SortedSet<string> AssignedDds { get; } = new(StringComparer.OrdinalIgnoreCase);
    // CALL or LINK through a variable that no literal in the program is moved into.
    public List<ScannedReference> UnresolvedDynamic { get; } = [];
}

// Reads a COBOL program for the relationships it states: CALL, COPY, EXEC SQL tables, EXEC CICS
// LINK/XCTL/maps/transactions/files, and SELECT ... ASSIGN. Deterministic and line-accurate, so every
// edge in the estate graph can point at the line that put it there.
public static class EstateSourceScanner
{
    private const RegexOptions Opts = RegexOptions.IgnoreCase | RegexOptions.CultureInvariant;
    private const string Name = @"[A-Z0-9#@$][A-Z0-9#@$_-]*";
    private const string Table = @"[A-Z#@$][A-Z0-9#@$_]*(?:\.[A-Z#@$][A-Z0-9#@$_]*)?";

    private static readonly Regex ProgramIdRx = new(@"\bPROGRAM-ID\s*\.?\s*['""]?(" + Name + ")", Opts);
    private static readonly Regex CallLiteral = new(@"(?<![\w-])CALL\s+['""](" + Name + @")['""]", Opts);
    private static readonly Regex CallVariable = new(@"(?<![\w-])CALL\s+(" + Name + @")\b", Opts);
    private static readonly Regex CopyRx = new(@"(?<![\w-])COPY\s+['""]?(" + Name + @")['""]?", Opts);
    private static readonly Regex MoveLiteral = new(@"(?<![\w-])MOVE\s+['""](" + Name + @")['""]\s+TO\s+(" + Name + ")", Opts);
    private static readonly Regex ValueLiteral = new(@"\b\d{1,2}\s+(" + Name + @")\b[^.]*?\bVALUE\s+(?:IS\s+)?['""](" + Name + @")['""]", Opts);
    private static readonly Regex SelectAssign = new(@"(?<![\w-])SELECT\s+(?:OPTIONAL\s+)?" + Name + @"\s+ASSIGN\s+(?:TO\s+)?['""]?(" + Name + ")", Opts);
    private static readonly Regex ExecBlock = new(@"(?<![\w-])EXEC\s+(SQL|CICS)\b(.*?)\bEND-EXEC", Opts | RegexOptions.Singleline);

    private static readonly Regex SqlWrite = new(@"\b(?:INSERT\s+INTO|UPDATE|DELETE\s+FROM|MERGE\s+INTO)\s+(" + Table + ")", Opts);
    private static readonly Regex SqlFrom = new(@"\b(?:FROM|JOIN)\s+(" + Table + @"(?:\s+(?:AS\s+)?[A-Z][A-Z0-9_]*)?(?:\s*,\s*" + Table + @"(?:\s+(?:AS\s+)?[A-Z][A-Z0-9_]*)?)*)", Opts);
    private static readonly Regex SqlInclude = new(@"^\s*INCLUDE\s+(" + Name + ")", Opts);
    private static readonly HashSet<string> SqlNotTables = new(StringComparer.OrdinalIgnoreCase)
    {
        "WHERE", "GROUP", "ORDER", "HAVING", "FETCH", "FOR", "WITH", "UNION", "EXCEPT", "INTERSECT",
        "INNER", "LEFT", "RIGHT", "FULL", "OUTER", "CROSS", "JOIN", "ON", "SET", "VALUES", "SELECT",
    };

    private static readonly Regex CicsProgram = new(@"^\s*(LINK|XCTL|LOAD)\b.*?\bPROGRAM\s*\(\s*(?:['""](" + Name + @")['""]|(" + Name + @"))\s*\)", Opts | RegexOptions.Singleline);
    private static readonly Regex CicsMap = new(@"^\s*(?:SEND|RECEIVE)\s+MAP\s*\(\s*['""](" + Name + @")['""]\s*\)(?:.*?\bMAPSET\s*\(\s*['""](" + Name + @")['""]\s*\))?", Opts | RegexOptions.Singleline);
    private static readonly Regex CicsTransid = new(@"^\s*(?:START|RETURN)\b.*?\bTRANSID\s*\(\s*['""]([A-Z0-9#@$]{1,4})['""]\s*\)", Opts | RegexOptions.Singleline);
    private static readonly Regex CicsFile = new(@"^\s*(READ|READNEXT|READPREV|STARTBR|WRITE|REWRITE|DELETE)\b.*?\b(?:FILE|DATASET)\s*\(\s*['""](" + Name + @")['""]\s*\)", Opts | RegexOptions.Singleline);

    // Prefixes ASSIGN carries before the DD name on older compilers: UT-S-INFILE is DD INFILE.
    private static readonly HashSet<string> AssignPrefixes = new(StringComparer.OrdinalIgnoreCase) { "UT", "UR", "DA", "S", "AS", "D", "R", "I" };

    // A program name on z/OS is at most eight characters; a literal moved into a CALL variable that is
    // longer, or has a hyphen, is a message or a flag rather than a target.
    private static readonly Regex ProgramNameShape = new(@"^[A-Z#@$][A-Z0-9#@$]{0,7}$", Opts);

    public static ProgramScan Scan(string relativePath, string text, EstateGraphOptions? options = null)
    {
        options ??= new EstateGraphOptions();
        var code = CodeText.From(text);
        var scan = new ProgramScan
        {
            RelativePath = relativePath,
            ProgramId = ProgramIdRx.Match(code.Text) is { Success: true } pid ? pid.Groups[1].Value.ToUpperInvariant() : null,
        };

        // Literals moved into or initialised in a variable, so CALL WS-PGM resolves to what WS-PGM can hold.
        var literals = new Dictionary<string, List<(string Value, EstateEvidence Evidence)>>(StringComparer.OrdinalIgnoreCase);
        foreach (Match m in MoveLiteral.Matches(code.Text)) AddLiteral(m.Groups[2].Value, m.Groups[1].Value, code.Evidence(relativePath, m.Index));
        foreach (Match m in ValueLiteral.Matches(code.Text)) AddLiteral(m.Groups[1].Value, m.Groups[2].Value, code.Evidence(relativePath, m.Index));

        void AddLiteral(string variable, string value, EstateEvidence evidence)
        {
            if (!ProgramNameShape.IsMatch(value)) return;
            if (!literals.TryGetValue(variable, out var list)) literals[variable] = list = [];
            list.Add((value.ToUpperInvariant(), evidence));
        }

        void AddDynamic(string variable, string kind, EstateEvidence at)
        {
            if (literals.TryGetValue(variable, out var values))
            {
                foreach (var group in values.GroupBy(v => v.Value, StringComparer.Ordinal))
                    scan.References.Add(new ScannedReference(group.Key, EstateNodeKind.Program, kind, variable.ToUpperInvariant(),
                        [at, .. group.Select(g => g.Evidence)]));
            }
            else
            {
                scan.UnresolvedDynamic.Add(new ScannedReference(variable.ToUpperInvariant(), EstateNodeKind.Program, kind, variable.ToUpperInvariant(), [at]));
            }
        }

        // EXEC blocks first, then blanked out, so a CALL or COPY word inside SQL is not read twice.
        var outside = new StringBuilder(code.Text);
        foreach (Match block in ExecBlock.Matches(code.Text))
        {
            var body = block.Groups[2].Value;
            var bodyStart = block.Groups[2].Index;
            if (block.Groups[1].Value.Equals("SQL", StringComparison.OrdinalIgnoreCase))
                ScanSql(scan, body, bodyStart, code, relativePath, options);
            else
                ScanCics(scan, body, bodyStart, code, relativePath, AddDynamic);
            for (var i = block.Index; i < block.Index + block.Length; i++)
                if (outside[i] != '\n') outside[i] = ' ';
        }

        var rest = outside.ToString();
        foreach (Match m in CallLiteral.Matches(rest))
            scan.References.Add(new ScannedReference(m.Groups[1].Value.ToUpperInvariant(), EstateNodeKind.Program, EstateEdgeKind.Calls, null, [code.Evidence(relativePath, m.Index)]));
        foreach (Match m in CallVariable.Matches(rest))
            AddDynamic(m.Groups[1].Value, EstateEdgeKind.Calls, code.Evidence(relativePath, m.Index));
        foreach (Match m in CopyRx.Matches(rest))
        {
            var name = m.Groups[1].Value.ToUpperInvariant();
            if (options.SystemCopybooks.Contains(name, StringComparer.OrdinalIgnoreCase)) continue;
            scan.References.Add(new ScannedReference(name, EstateNodeKind.Copybook, EstateEdgeKind.Copies, null, [code.Evidence(relativePath, m.Index)]));
        }
        foreach (Match m in SelectAssign.Matches(rest))
        {
            var parts = m.Groups[1].Value.Split('-');
            var dd = parts.Length > 1 && AssignPrefixes.Contains(parts[0]) ? parts[^1] : m.Groups[1].Value;
            scan.AssignedDds.Add(dd.ToUpperInvariant());
        }

        return scan;
    }

    private static void ScanSql(ProgramScan scan, string body, int offset, CodeText code, string file, EstateGraphOptions options)
    {
        if (SqlInclude.Match(body) is { Success: true } include)
        {
            var name = include.Groups[1].Value.ToUpperInvariant();
            if (!options.SystemCopybooks.Contains(name, StringComparer.OrdinalIgnoreCase))
                scan.References.Add(new ScannedReference(name, EstateNodeKind.Copybook, EstateEdgeKind.Copies, "EXEC SQL INCLUDE", [code.Evidence(file, offset + include.Index)]));
            return;
        }

        var written = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        foreach (Match m in SqlWrite.Matches(body))
        {
            var table = m.Groups[1].Value.ToUpperInvariant();
            if (Ignored(table)) continue;
            written.Add(table);
            var verb = m.Value.TrimStart().Split(' ', 2)[0].ToUpperInvariant();
            var kind = verb == "DELETE" ? EstateEdgeKind.Deletes : verb == "UPDATE" ? EstateEdgeKind.Updates : EstateEdgeKind.Writes;
            scan.References.Add(new ScannedReference(table, EstateNodeKind.Table, kind, "EXEC SQL", [code.Evidence(file, offset + m.Index)]));
        }

        foreach (Match m in SqlFrom.Matches(body))
        {
            // DELETE FROM t names the target, already recorded as a write.
            var before = body[..m.Index].TrimEnd();
            if (before.EndsWith("DELETE", StringComparison.OrdinalIgnoreCase)) continue;
            foreach (var item in m.Groups[1].Value.Split(','))
            {
                var table = item.Trim().Split((char[])[' ', '\t', '\n', '\r'], StringSplitOptions.RemoveEmptyEntries)[0].ToUpperInvariant();
                if (SqlNotTables.Contains(table) || Ignored(table) || written.Contains(table)) continue;
                scan.References.Add(new ScannedReference(table, EstateNodeKind.Table, EstateEdgeKind.Reads, "EXEC SQL", [code.Evidence(file, offset + m.Index)]));
            }
        }

        bool Ignored(string table) =>
            options.IgnoredTablePrefixes.Any(p => table.StartsWith(p, StringComparison.OrdinalIgnoreCase));
    }

    private static void ScanCics(ProgramScan scan, string body, int offset, CodeText code, string file,
        Action<string, string, EstateEvidence> addDynamic)
    {
        var at = code.Evidence(file, offset);
        if (CicsProgram.Match(body) is { Success: true } p)
        {
            if (p.Groups[2].Success)
                scan.References.Add(new ScannedReference(p.Groups[2].Value.ToUpperInvariant(), EstateNodeKind.Program, EstateEdgeKind.Links,
                    "EXEC CICS " + p.Groups[1].Value.ToUpperInvariant(), [at]));
            else
                addDynamic(p.Groups[3].Value, EstateEdgeKind.Links, at);
        }
        if (CicsMap.Match(body) is { Success: true } map)
        {
            var name = (map.Groups[2].Success ? map.Groups[2].Value : map.Groups[1].Value).ToUpperInvariant();
            scan.References.Add(new ScannedReference(name, EstateNodeKind.Map, EstateEdgeKind.UsesMap,
                map.Groups[2].Success ? "MAP " + map.Groups[1].Value.ToUpperInvariant() : null, [at]));
        }
        if (CicsTransid.Match(body) is { Success: true } t)
            scan.References.Add(new ScannedReference(t.Groups[1].Value.ToUpperInvariant(), EstateNodeKind.Transaction, EstateEdgeKind.Starts, "EXEC CICS", [at]));
        if (CicsFile.Match(body) is { Success: true } f)
        {
            var verb = f.Groups[1].Value.ToUpperInvariant();
            var kind = verb switch
            {
                "WRITE" => EstateEdgeKind.Writes,
                "REWRITE" => EstateEdgeKind.Updates,
                "DELETE" => EstateEdgeKind.Deletes,
                _ => EstateEdgeKind.Reads,
            };
            scan.References.Add(new ScannedReference(f.Groups[2].Value.ToUpperInvariant(), EstateNodeKind.File, kind, "EXEC CICS FILE", [at]));
        }
    }

    // The program's code area as one string, with comments and sequence numbers removed and each
    // character mapped back to its source line, so a match anywhere can be cited by line.
    internal sealed class CodeText
    {
        private readonly List<int> _lineStarts = [];
        private readonly string[] _lines;
        public string Text { get; }

        private CodeText(string text, string[] lines, List<int> lineStarts)
        {
            Text = text;
            _lines = lines;
            _lineStarts = lineStarts;
        }

        private static readonly Regex FreeFormat = new(@">>\s*SOURCE\s+(?:FORMAT\s+)?(?:IS\s+)?FREE", Opts);

        public static CodeText From(string source)
        {
            var lines = source.Replace("\r\n", "\n").Split('\n');
            var free = FreeFormat.IsMatch(source);
            var sb = new StringBuilder();
            var starts = new List<int>(lines.Length);
            foreach (var raw in lines)
            {
                starts.Add(sb.Length);
                sb.Append(free ? FreeCode(raw) : CodeArea(raw)).Append('\n');
            }
            return new CodeText(sb.ToString(), lines, starts);
        }

        // Fixed format: columns 1-6 are a sequence number, column 7 the indicator, 8-72 the code.
        // A '*' or '/' indicator is a comment line, and so is 'D' (debugging), which normal
        // compiles skip. A '*>' starts an inline comment in either format.
        private static string CodeArea(string line)
        {
            string code;
            if (line.Length >= 7)
            {
                var indicator = line[6];
                if (indicator is '*' or '/' or 'D' or 'd') return "";
                code = line.Length > 7 ? line[7..Math.Min(line.Length, 72)] : "";
            }
            else
            {
                code = "";
            }
            var inline = code.IndexOf("*>", StringComparison.Ordinal);
            return inline >= 0 ? code[..inline] : code;
        }

        private static string FreeCode(string line)
        {
            if (line.TrimStart().StartsWith(">>", StringComparison.Ordinal)) return "";
            var inline = line.IndexOf("*>", StringComparison.Ordinal);
            return inline >= 0 ? line[..inline] : line;
        }

        public int LineAt(int index)
        {
            var i = _lineStarts.BinarySearch(index);
            return (i >= 0 ? i : ~i - 1) + 1;
        }

        public EstateEvidence Evidence(string file, int index)
        {
            var line = LineAt(index);
            var text = _lines[line - 1].Trim();
            return new EstateEvidence(file, line, text.Length > 160 ? text[..160] : text);
        }
    }
}
