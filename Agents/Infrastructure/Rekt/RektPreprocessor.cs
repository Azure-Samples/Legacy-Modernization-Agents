using System.Text;
using System.Text.Json.Nodes;
using System.Text.RegularExpressions;
using static CobolToQuarkusMigration.Agents.Infrastructure.Rekt.PyCompat;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Rekt;

// Source normalisations preprocess-for-rekt.sh applies so the native parser accepts real estates. Every
// rewrite keeps line structure and columns 1-72; files that need no rewrite are not written at all.
public static class RektPreprocessor
{
    private const RegexOptions M = RegexOptions.Multiline;
    private const RegexOptions I = RegexOptions.IgnoreCase;

    public sealed record Rule(string Stage, bool Literal, string Find, string Replace, Regex? Pattern);

    // Text mode with universal newlines, as the original tool read sources.
    private static string ReadSource(string path) =>
        File.ReadAllText(path, Latin1).Replace("\r\n", "\n").Replace('\r', '\n');

    public static bool PreprocessCopybook(string sourcePath, string outputDir)
    {
        var original = ReadSource(sourcePath);
        var content = Copybook(original, Path.GetFileName(sourcePath));
        if (content == original) return false;
        File.WriteAllText(Path.Combine(outputDir, Path.GetFileName(sourcePath)), FitFixedFormat(content), Latin1);
        return true;
    }

    public static bool PreprocessProgram(string sourcePath, string sourceDir, string outputDir, IReadOnlyList<Rule> rules)
    {
        var original = ReadSource(sourcePath);
        var content = Program(original, sourceDir, rules);
        if (content == original) return false;
        var output = Path.Combine(outputDir, Path.GetRelativePath(sourceDir, sourcePath));
        Directory.CreateDirectory(Path.GetDirectoryName(output)!);
        File.WriteAllText(output, FitFixedFormat(content), Latin1);
        return true;
    }

    public static string Copybook(string content, string fileName)
    {
        content = StripTrailingSequence(content);
        content = PseudoTextTokens(content);
        content = PyRegex.Sub(content, @"^(\d{6}\s+|\s+)(\d{1,2})(\s{1,})(PIC\b)", InsertFiller, M);
        content = PyRegex.Sub(content, @"^(\d{6}\s+|\s+)(\d{1,2})(\s{1,})(REDEFINES\b)", InsertFiller, M);
        content = EnforceColumn72(content, dropAuditStamps: true);
        // Endevor date/time audit stamps, with and without a preceding period.
        content = PyRegex.Sub(content, @"(\.)\s+(\d{1,4}[.\-/]\d{1,2}[.\-/]\d{1,4})\s*$", "$1", M);
        content = PyRegex.Sub(content, @"\s{2,}(\d{1,4}[.\-/]\d{1,2}[.\-/]\d{1,4})\s*$", "", M);
        content = UniqueRedefines(content);
        // Section labels become invalid when SAMPLE copybooks are included inline.
        var name = fileName.ToUpperInvariant();
        if (name.StartsWith("SAMPLE", StringComparison.Ordinal) && !name.EndsWith("I.CPY", StringComparison.Ordinal))
            content = PyRegex.Sub(content, @"^(\s*\d{0,6})( )([A-Z][A-Z0-9-]+\s+SECTION\s*\.\s*)$", "${1}*${3}", M | I);
        content = CopybookMoveCorresponding(content);
        return DecimalCommas(content);
    }

    public static string Program(string content, string sourceDir, IReadOnlyList<Rule> rules)
    {
        content = StripTrailingSequence(content);
        content = DecimalCommas(content);
        // IMS/DL/I calls and SQL diagnostics have no parser support; keep them visible as comments.
        content = PyRegex.Sub(content, @"([ ]{6,})EXEC\s+DLI\b.*?END-EXEC\.?",
            match => PyRegex.Sub(match.Value, "^(.{6})", "${1}*IMS>", M), RegexOptions.Singleline | I);
        content = PyRegex.Sub(content, @"EXEC\s+SQL\s+INCLUDE\s+(\w+)\s*END-EXEC\.?", "COPY ${1}.", I);
        content = PyRegex.Sub(content, @"([ ]{6,})EXEC\s+SQL\s+GET\s+DIAGNOSTICS\b.*?END-EXEC\.?",
            match => PyRegex.Sub(match.Value, "^(.{6})", "${1}*DB2>", M), RegexOptions.Singleline | I);
        // COBOL names contain hyphens; REQ-COPY 'A' or MSG-COPY REPLACING is a data name, not a COPY.
        content = PyRegex.Sub(content, @"(?<![A-Za-z0-9_-])COPY\s+'([A-Z0-9]+)'", "COPY ${1}");
        // The parser limits COPY names to eight characters; shorten only when that alias exists.
        content = PyRegex.Sub(content, @"(?<![A-Za-z0-9_-])(COPY\s+)([A-Z][A-Z0-9_-]{8,})([\s.])", match =>
        {
            var copyName = match.Groups[2].Value;
            var alias = Path.Combine(sourceDir, copyName[..8] + ".cpy");
            return File.Exists(alias) || Directory.Exists(alias)
                ? match.Groups[1].Value + copyName[..8] + match.Groups[3].Value
                : match.Value;
        }, I);
        content = PseudoTextTokens(content);
        content = PyRegex.Sub(content, @"^.{0,6}[ ]+MOVE\s+CORR(?:ESPONDING)?\s.*$",
            match => match.Value.Length >= 7 ? match.Value[..6] + "*" + match.Value[7..] : match.Value, M | I);
        // Arithmetic in reference modification, e.g. (TALLY + 1:LEN), is not parsed.
        content = PyRegex.Sub(content, @"\(([A-Z][A-Z0-9_-]*\s*[+\-]\s*\d+):", "(1:", I);
        content = content.Replace("MOVE 0(1) TO", "MOVE 0 TO").Replace("MOVE 1(1) TO", "MOVE 1 TO");
        content = PyRegex.Sub(content, @"^(\d{6}\s+|\s+)(\d{1,2})(\s{1,})(PIC\b)", InsertFiller, M);
        content = PyRegex.Sub(content, @"^(\d{6}\s+|\s+)(\d{1,2})(\s{1,})(REDEFINES\b)", InsertFiller, M);
        content = string.Join('\n', content.Split('\n').Select(CommentCopyWithPrefix));
        content = string.Join('\n', content.Split('\n').Select(line => IsComment(line) ? line
            : PyRegex.Sub(line, @"(\bMOVE\s+)\bALL\s+\'([^\']+)\'",
                match => match.Groups[1].Value + "'" + match.Groups[2].Value + "'", I)));
        content = ApplyRules(content, rules, "early");
        content = EnforceColumn72(content, dropAuditStamps: false);
        // An apostrophe in AUTHOR text confuses the parser's paragraph scanner.
        content = PyRegex.Sub(content, @"^(\s*AUTHOR\s*\.\s*)([^\n]*)$",
            match => match.Groups[2].Value.Contains('\'') ? match.Groups[1].Value + "UNKNOWN." : match.Value, M);
        content = UniqueRedefines(content);
        content = NormalizeFiguratives(content);
        content = content.Replace("INTRTI-PIC X(4) USAGE", "INTRTI-PICX4").Replace("INTRTI-PIC X(4)", "INTRTI-PICX4")
            .Replace("INTRTI-PICX4 USAGE", "INTRTI-PICX4");
        content = ApplyRules(content, rules, "late");
        // An IF whose THEN branch holds only comments before ELSE is rejected; CONTINUE is a no-op.
        content = PyRegex.Sub(content,
            @"(^(?:\d{6})?[ \t]*IF\s[^\n]*[^.\s][ \t]*)((?:\n[ \t]{0,6}\*[^\n]*|\n[ \t]*(?=\n))*)(\n[ \t]+ELSE\b)",
            "${1}\n           CONTINUE${2}${3}", M | I);
        content = PyRegex.Sub(content, "^(      )D(.*)$", "${1}*DBG${2}", M);
        content = CommentOrphanedIfBodies(content);
        content = CommentDisplayExecCics(content);
        content = OpaqueInConditions(content);
        content = AddMissingEndString(content);
        return string.Join('\n', content.Split('\n').Select(line => IsComment(line) ? line
            : PyRegex.Sub(line, @"\s+\d{2}\.\d{2}\.\d{2}\s*$", "")));
    }

    private static bool IsComment(string line) => line.Length > 6 && line[6] == '*';

    private static string CommentColumn7(string line) => line.Length >= 7 ? line[..6] + "*" + line[7..] : line;

    private static string StripTrailingSequence(string text) => string.Join('\n', text.Split('\n').Select(line =>
    {
        var raw = RStrip(line);
        if (IsComment(raw)) return line;
        // Columns 73-80 are the identification area. Unless a token straddles column 72, drop it here,
        // before later steps shift text left into code.
        if (raw.Length > 72 && (raw[71] == ' ' || raw[72] == ' ')) return RStrip(raw[..72]);
        if (raw.Length > 72 && PyRegex.MatchStart(raw, @"^(.+?)\s+(\d{8})$") is { } separated) return separated.Groups[1].Value;
        return PyRegex.MatchStart(raw, @"^(.+\.)(\d{8})$") is { } joined ? joined.Groups[1].Value : line;
    }));

    // :TOKEN: pseudo-text becomes the valid identifier XTOKEN.
    private static string PseudoTextTokens(string content)
    {
        content = PyRegex.Sub(content, @":\'([A-Z][A-Z0-9_-]+)\':", match => "'X" + match.Groups[1].Value.Replace("-", "") + "'");
        return PyRegex.Sub(content, ":([A-Z][A-Z0-9_-]+):", match => "X" + match.Groups[1].Value.Replace("-", ""));
    }

    // Anonymous entries get FILLER; the gap shrinks so the line keeps its width.
    private static string InsertFiller(Match match) =>
        match.Groups[1].Value + match.Groups[2].Value + new string(' ', Math.Max(1, match.Groups[3].Value.Length - 7))
        + "FILLER " + match.Groups[4].Value;

    private static string DecimalCommas(string content) => PyRegex.Sub(content, @"(\b\d+),(\d+\b)", "${1}.${2}");

    private static string EnforceColumn72(string text, bool dropAuditStamps) => string.Join('\n', text.Split('\n').Select(line =>
    {
        var raw = RStrip(line);
        if (raw.Length <= 72 || LStrip(raw).StartsWith('*')) return line;
        if (dropAuditStamps && PyRegex.MatchStart(raw[72..], @"^[0-9.\-:/ ]+\s*$") is not null) return RStrip(raw[..72]);
        var compressed = PyRegex.Sub(raw, @"(\S)(  +)(\S)", match => match.Groups[1].Value
            + new string(' ', Math.Max(1, match.Groups[2].Value.Length - (raw.Length - 72))) + match.Groups[3].Value);
        while (compressed.Length > 72)
        {
            compressed = PyRegex.Sub(compressed, "(  +)", match => new string(' ', Math.Max(1, match.Groups[1].Value.Length - 1)), count: 1);
            if (!compressed.Contains("  ", StringComparison.Ordinal)) break;
        }
        return compressed;
    }));

    // More than ten consecutive FILLER REDEFINES of one field break the parser; give each a unique name.
    private static string UniqueRedefines(string content)
    {
        var counter = 0;
        return PyRegex.Sub(content, @"^(\s*\d{0,6}\s+)(\d{2})\s+FILLER(\s+)(REDEFINES\s+\S+)", match =>
        {
            counter++;
            var uniqueName = $"FIL-R{counter:D3}";
            var middle = match.Groups[3].Value;
            var adjust = uniqueName.Length - "FILLER".Length;
            if (adjust > 0 && middle.Length > adjust) middle = middle[..^adjust];
            return match.Groups[1].Value + match.Groups[2].Value + " " + uniqueName + middle + match.Groups[4].Value;
        }, M);
    }

    // MOVE CORRESPONDING is unsupported; it becomes CONTINUE and a TO clause on the next line is commented out.
    private static string CopybookMoveCorresponding(string content)
    {
        var result = new StringBuilder();
        var skipToLine = false;
        foreach (var line in SplitLines(content, keepEnds: true))
        {
            if (skipToLine)
            {
                result.Append(PyRegex.MatchStart(line, @"^(\d{6})") is { } sequence
                    ? sequence.Groups[1].Value + "*COR>" + line[6..]
                    : PyRegex.Compile("^(.{6})").Replace(line, "${1}*COR>", 1));
                skipToLine = false;
                continue;
            }
            if (PyRegex.Search(line, @"\bMOVE\s+CORR(ESPONDING)?\b", I))
            {
                var indent = PyRegex.MatchStart(line, @"^(\s*\d{0,6}\s+)") is { } match ? match.Groups[1].Value : "           ";
                result.Append(indent).Append("CONTINUE\n");
                if (!PyRegex.Search(line, @"\bTO\b", I)) skipToLine = true;
            }
            else result.Append(line);
        }
        return result.ToString();
    }

    // Copy-with-prefix directives would otherwise stop the parser's copybook recursion.
    // The directive is `-COPY NAME -PRE X-.`; a data name such as REQ-COPY beside WS-PREV is not one.
    private static readonly Regex CopyWithPrefix = new(@"(?<![A-Za-z0-9_-])-COPY\s.*\s-PRE\b", RegexOptions.IgnoreCase);

    private static string CommentCopyWithPrefix(string line)
    {
        if (IsComment(line) || !CopyWithPrefix.IsMatch(line))
            return line;
        return line.Length >= 7 ? line[..6] + "*" + line[7..] : line.PadRight(6)[..6] + "*" + line[Math.Min(6, line.Length)..];
    }

    // The parser's figurative-constant table only knows the upper-case spellings.
    private static string NormalizeFiguratives(string text) => string.Join('\n', text.Split('\n').Select(line =>
    {
        if (IsComment(line)) return line;
        var parts = PyRegex.Compile("((?:'[^']*')+)").Split(line);
        for (var index = 0; index < parts.Length; index += 2)
        {
            parts[index] = PyRegex.Sub(parts[index], @"\bzero\b", "ZERO");
            parts[index] = PyRegex.Sub(parts[index], @"\bspaces\b", "SPACES");
            parts[index] = PyRegex.Sub(parts[index], @"\bspace\b", "SPACE");
        }
        return string.Concat(parts);
    }));

    private static string CommentOrphanedIfBodies(string text)
    {
        var result = new List<string>();
        var inOrphan = false;
        foreach (var line in text.Split('\n'))
        {
            if (PyRegex.MatchStart(line, @"^.{6}\*.*\bIF\b.*\bDFHRESP\b", I) is not null)
            {
                inOrphan = true;
                result.Add(line);
            }
            else if (!inOrphan || PyRegex.MatchStart(line, @"^.{6}\*") is not null || Strip(line).Length == 0) result.Add(line);
            else if (PyRegex.MatchStart(line, @"^\s*END-IF\b", I) is not null)
            {
                result.Add(CommentColumn7(line));
                inOrphan = false;
            }
            else if (PyRegex.MatchStart(line, @"^\s{7,8}\S.*\.\s*$") is not null)
            {
                inOrphan = false;
                result.Add(line);
            }
            else result.Add(CommentColumn7(line));
        }
        return string.Join('\n', result);
    }

    // DISPLAY literals containing EXEC CICS would be read as dialect statements.
    private static string CommentDisplayExecCics(string text)
    {
        var result = new List<string>();
        int? displayIndent = null;
        foreach (var line in text.Split('\n'))
        {
            if (displayIndent is null)
            {
                var display = PyRegex.MatchStart(line, @"^(\s+)DISPLAY\s", I);
                if (!(line.Length >= 7 && line[6] == '*') && display is not null && PyRegex.Search(line, @"'[^']*EXEC\s+CICS[^']*'", I))
                {
                    displayIndent = display.Groups[1].Value.Length;
                    result.Add(CommentColumn7(line));
                }
                else result.Add(line);
            }
            else if (Strip(line).Length == 0) result.Add(line);
            else if (line.Length - LStrip(line).Length > displayIndent) result.Add(CommentColumn7(line));
            else
            {
                displayIndent = null;
                result.Add(line);
            }
        }
        return string.Join('\n', result);
    }

    private const string StatementKeywords =
        @"^(IF\b|ELSE\b|END-IF\b|MOVE\b|COMPUTE\b|PERFORM\b|CONTINUE\b|ADD\b|SUBTRACT\b|MULTIPLY\b|DIVIDE\b|SET\b|DISPLAY\b|"
        + @"STRING\b|UNSTRING\b|EVALUATE\b|WHEN\b|STOP\b|GO\s+TO\b|INITIALIZE\b|INSPECT\b|READ\b|WRITE\b|REWRITE\b|DELETE\b|"
        + @"OPEN\b|CLOSE\b|ACCEPT\b|CALL\b|EXEC\b)";

    // Arithmetic or comparison on IN-qualified names is unsupported; the condition becomes opaque and
    // the original lines stay as comments.
    private static string OpaqueInConditions(string text)
    {
        var lines = text.Split('\n');
        var result = new List<string>();
        var index = 0;
        while (index < lines.Length)
        {
            var line = lines[index];
            if (!(line.Length >= 7 && line[6] == '*') && PyRegex.MatchStart(line, @"^(\s{6,})(IF\s+)", I) is { } condition)
            {
                var afterIf = line[condition.Length..];
                if (PyRegex.Search(afterIf, @"\b\w+(?:-\w+)*\s+IN\s+\w+(?:-\w+)*\s*[-+*/]", I)
                    || PyRegex.Search(afterIf, @"\b\w+(?:-\w+)*\s+IN\s+\w+(?:-\w+)*\s*(?:>|<|NOT\s*=|=\s*(?!>)|>=|<=)", I))
                {
                    var preserved = new List<string> { line.Length >= 7 ? CommentColumn7(line) : "*" + line };
                    index++;
                    while (index < lines.Length)
                    {
                        var continuation = lines[index];
                        if (continuation.Length >= 7 && continuation[6] == '*')
                        {
                            preserved.Add(continuation);
                            index++;
                            continue;
                        }
                        var stripped = Strip(continuation);
                        if (PyRegex.MatchStart(stripped, @"^(AND|OR)\b", I) is not null
                            || stripped.Length > 0 && PyRegex.MatchStart(stripped, StatementKeywords, I) is null)
                        {
                            preserved.Add(continuation.Length >= 7 ? CommentColumn7(continuation) : "*" + continuation);
                            index++;
                            continue;
                        }
                        break;
                    }
                    result.AddRange(preserved);
                    result.Add(condition.Groups[1].Value + condition.Groups[2].Value + "REKT-OPAQUE-IN-CONDITION");
                    continue;
                }
            }
            result.Add(line);
            index++;
        }
        return string.Join('\n', result);
    }

    // A STRING inside IF/ELSE without END-STRING needs explicit scope termination. WHEN is not a scope close:
    // STRING cannot span EVALUATE branches, and treating it as one would corrupt the EVALUATE.
    private static string AddMissingEndString(string text)
    {
        const string endString = @"^\s*END-STRING\s*$";
        var lines = text.Split('\n');
        var result = new List<string>();
        var index = 0;
        var inString = false;
        while (index < lines.Length)
        {
            var line = lines[index];
            var stripped = Strip(line);
            if (!(line.Length >= 7 && line[6] == '*'))
            {
                if (PyRegex.MatchStart(stripped, @"STRING\s", I) is not null) inString = true;
                else if (PyRegex.MatchStart(line, endString, I) is not null) inString = false;
                else if (inString && PyRegex.Search(line, @"DELIMITED\s+BY\s+(?:SIZE|\S+)\s+INTO\s+\S", I))
                {
                    result.Add(line);
                    index++;
                    var buffered = new List<string>();
                    var foundEndString = false;
                    while (index < lines.Length)
                    {
                        var next = lines[index];
                        if (Strip(next).Length == 0 || next.Length >= 7 && next[6] == '*')
                        {
                            buffered.Add(next);
                            index++;
                            continue;
                        }
                        if (PyRegex.MatchStart(next, endString, I) is not null) foundEndString = true;
                        else if (PyRegex.MatchStart(next, @"^\s*(END-IF|ELSE)\b", I) is not null && !foundEndString)
                        {
                            result.AddRange(buffered);
                            result.Add(PyRegex.MatchStart(next, @"^(\s*)")!.Groups[1].Value + "END-STRING");
                            buffered.Clear();
                        }
                        inString = false;
                        break;
                    }
                    result.AddRange(buffered);
                    continue;
                }
            }
            result.Add(line);
            index++;
        }
        return string.Join('\n', result);
    }

    // Rewrites that add characters push space-padded lines past column 80, which the parser rejects.
    // Only the identification area of such lines is dropped; columns 1-72 are kept.
    private static string FitFixedFormat(string text) => string.Join('\n', text.Split('\n').Select(line =>
    {
        if (line.Length <= 80) return line;
        if (Strip(line[72..]).Length == 0) return line[..72];
        return line[6] is '*' or '/' ? line[..80] : line;
    }));

    private static string ApplyRules(string text, IReadOnlyList<Rule> rules, string stage)
    {
        foreach (var rule in rules.Where(rule => rule.Stage == stage))
            text = rule.Literal ? PyReplace(text, rule.Find, rule.Replace) : rule.Pattern!.Replace(text, rule.Replace);
        return text;
    }

    // str.replace inserts the replacement between every character when the search string is empty.
    private static string PyReplace(string text, string find, string replace) =>
        find.Length > 0 ? text.Replace(find, replace, StringComparison.Ordinal)
            : replace + string.Concat(text.Select(character => character + replace));

    // Rules come from Config/rekt-preprocess.local.json (programRewrites). A rule that cannot be applied with
    // its documented meaning is reported and skipped rather than run with a different one.
    public static List<Rule> LoadRules(string path, Action<string>? report = null)
    {
        var root = PyJson.Parse(File.ReadAllText(path, Utf8)) as JsonObject
            ?? throw new InvalidDataException("not a JSON object");
        if (!root.TryGetPropertyValue("programRewrites", out var list) || list is not JsonArray items)
            throw new InvalidDataException("programRewrites is not a list");
        var rules = new List<Rule>();
        for (var index = 0; index < items.Count; index++)
        {
            try
            {
                rules.Add(ParseRule(items[index]));
            }
            catch (Exception error) when (error is NotSupportedException or ArgumentException)
            {
                report?.Invoke($"rule {index + 1}: {error.Message}");
            }
        }
        return rules;
    }

    private static Rule ParseRule(JsonNode? node)
    {
        if (node is not JsonObject rule) throw new NotSupportedException("not a JSON object");
        var stage = rule.TryGetPropertyValue("stage", out var stageNode) ? PyJson.Str(stageNode) : "early";
        if (stage is null) throw new NotSupportedException("stage is not a string");
        var literal = PyJson.Truthy(rule["literal"]);
        var find = PyJson.Str(rule["find"]) ?? throw new NotSupportedException("find is missing or not a string");
        var replace = PyJson.Str(rule["replace"]) ?? throw new NotSupportedException("replace is missing or not a string");
        if (literal) return new Rule(stage, true, find, replace, null);
        var flags = rule["flags"] switch
        {
            null => [],
            JsonArray names => names.Select(name => PyJson.Str(name) ?? throw new NotSupportedException("flags must be names")).ToList(),
            _ => throw new NotSupportedException("flags is not a list"),
        };
        var options = PyRegex.Flags(flags);
        Regex pattern;
        try
        {
            pattern = PyRegex.Compile(find, options);
        }
        catch (ArgumentException error)
        {
            throw new NotSupportedException($"find is not a supported regular expression ({error.Message})");
        }
        return new Rule(stage, false, find, PyRegex.TranslateReplacement(replace, find, options), pattern);
    }
}
