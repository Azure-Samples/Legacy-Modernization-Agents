using System.Security.Cryptography;
using System.Text;
using System.Text.Json;
using System.Text.RegularExpressions;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Helpers;

public sealed record PartialField(string Name, string Declaration)
{
    public string Kind { get; init; } = "elementary";
}
public sealed record PartialDefinition(string Path, string Sha256);
public sealed record PartialRoutine(string Name, int StartLine, int EndLine, string Source,
    IReadOnlyList<string> Blockers)
{
    public string Kind { get; init; } = "routine";
}
public sealed record PartialProgram(string Program, string SourceSha256, IReadOnlyList<PartialField> Fields,
    IReadOnlyList<PartialRoutine> Routines, IReadOnlyList<string> Warnings)
{
    public IEnumerable<PartialRoutine> Selected => Routines.Where(r => r.Blockers.Count == 0);
    public IReadOnlyList<PartialDefinition> Definitions { get; init; } = [];
}

public static class PartialTreePlan
{
    private static readonly Regex Header = new(@"^ {0,3}([A-Z0-9][A-Z0-9-]*)\s+(SECTION)\s*\.\s*$|^ {0,3}([A-Z0-9][A-Z0-9-]*)\.\s*$", RegexOptions.IgnoreCase);
    private static readonly Regex Declaration = new(@"^\s*(\d{2})\s+([A-Z0-9][A-Z0-9-]*)(?:\s+(.*)|\.)\s*$", RegexOptions.IgnoreCase);
    private static readonly Regex Words = new(@"[A-Z][A-Z0-9-]*");
    private static readonly Regex Literals = new(@"'([^']|'')*'|""([^""]|"""")*""");
    private static readonly HashSet<string> Keywords = new(
        ("MOVE TO ALL SPACE SPACES ZERO ZEROS ZEROES HIGH-VALUES LOW-VALUES QUOTE QUOTES " +
         "IF ELSE END-IF NOT AND OR IS ARE EQUAL EQUALS GREATER LESS THAN NUMERIC ALPHABETIC " +
         "ADD SUBTRACT MULTIPLY DIVIDE COMPUTE GIVING BY FROM ROUNDED REMAINDER " +
         "ON SIZE ERROR END-ADD END-SUBTRACT END-MULTIPLY END-DIVIDE END-COMPUTE " +
         "CONTINUE EXIT SECTION PARAGRAPH PERFORM END-PERFORM UNTIL VARYING AFTER TIMES " +
         "WITH TEST BEFORE TRUE FALSE DISPLAY UPON CONSOLE NO ADVANCING INITIALIZE").Split(' '),
        StringComparer.Ordinal);

    public static PartialProgram Assess(CobolFile file, string sourceRoot)
    {
        var source = Path.GetFullPath(file.FilePath);
        var identity = SourcePathHelper.NormalizeRelativePath(Path.GetRelativePath(sourceRoot, source));
        if (identity.StartsWith("../", StringComparison.Ordinal) || Path.IsPathRooted(identity))
            throw new InvalidDataException("Partial source lies outside the selected tree.");
        var raw = File.ReadAllBytes(source);
        var lines = Encoding.UTF8.GetString(raw).Replace("\r", "").Split('\n');
        var code = lines.Select(Normalize).ToArray();
        var procedure = Array.FindIndex(code, l => l.Contains("PROCEDURE DIVISION", StringComparison.OrdinalIgnoreCase));
        var warnings = new List<string>
        {
            "Restricted source-level routine extraction, NOT parser coverage or behavioral equivalence. " +
            "No application entry point or missing integration implementation is generated."
        };
        if (code.Any(l => l.Contains("WITH DEBUGGING MODE", StringComparison.OrdinalIgnoreCase)))
            return new(identity, Hash(raw), [], [], [.. warnings, "Active COBOL debugging mode requires a separate execution contract."]);
        if (procedure < 0)
            return new(identity, Hash(raw), [], [], [.. warnings, "No supported PROCEDURE DIVISION found."]);
        var definitions = new List<PartialDefinition>();
        var expanded = ExpandCopies(code[..procedure], sourceRoot, definitions, [], warnings).ToArray();
        var procedureTokens = Words.Matches(Literals.Replace(string.Join("\n", code[procedure..]).ToUpperInvariant(), " "))
            .Select(m => m.Value).ToHashSet();
        var fields = ReadFields(expanded, procedureTokens);
        var fieldNames = fields.Select(f => f.Name).ToHashSet(StringComparer.Ordinal);
        var headers = Enumerable.Range(procedure + 1, code.Length - procedure - 1)
            .Where(i => Header.IsMatch(code[i])).ToArray();
        var sections = headers.Where(i => Header.Match(code[i]).Groups[2].Success).ToArray();
        var starts = sections.Length > 0 ? sections : headers;
        var names = starts.Select(i => Name(code[i])).ToArray();
        var uniqueNames = names.GroupBy(n => n).Where(g => g.Count() == 1).Select(g => g.Key).ToHashSet();
        var routines = new List<PartialRoutine>();
        for (var i = 0; i < starts.Length; i++)
        {
            var start = starts[i];
            var end = i + 1 < starts.Length ? starts[i + 1] : code.Length;
            var body = string.Join("\n", code[(start + 1)..end].Where(l => !string.IsNullOrWhiteSpace(l)));
            var executable = Literals.Replace(body.ToUpperInvariant(), " ");
            var blockers = new List<string>();
            if (!uniqueNames.Contains(names[i])) blockers.Add("Duplicate routine identity.");
            if (code[(start + 1)..end].Any(l => Header.IsMatch(l)))
                blockers.Add("Nested paragraph fall-through requires section-level control-flow evidence.");
            if (lines[start..end].Any(l => l.Length > 6 && l[6] == '-'))
                blockers.Add("Continuation record requires complete literal/token reconstruction.");
            if (executable.Contains("THRU", StringComparison.Ordinal) || executable.Contains("THROUGH", StringComparison.Ordinal))
                blockers.Add("PERFORM range requires verified intervening control flow.");
            if (Regex.IsMatch(executable, @"\b(EXIT\s+(PROGRAM|PERFORM)|PERFORM\s+.*\bUNTIL\b)"))
                blockers.Add("Nonlocal/loop control requires separate control-flow evidence.");
            if (Literals.Replace(body, "").Contains('\'') || Literals.Replace(body, "").Contains('"'))
                blockers.Add("Unbalanced or continued literal.");
            foreach (var token in Words.Matches(executable).Select(m => m.Value).Distinct())
                if (!Keywords.Contains(token) && !fieldNames.Contains(token) && !uniqueNames.Contains(token))
                    blockers.Add($"Unresolved identifier or unsupported operation: {token}");
            if (string.IsNullOrWhiteSpace(body)) blockers.Add("No executable source.");
            else if (!Regex.IsMatch(executable, @"\b(MOVE|ADD|SUBTRACT|MULTIPLY|DIVIDE|COMPUTE|DISPLAY|INITIALIZE|PERFORM)\b"))
                blockers.Add("No state-changing or observable operation; terminator-only source is not a conversion deliverable.");
            routines.Add(new(names[i], start + 1, end, body, blockers));
        }
        bool changed;
        do
        {
            changed = false;
            for (var i = 0; i < routines.Count; i++)
            {
                var routine = routines[i];
                if (routine.Blockers.Count != 0) continue;
                var tokens = Words.Matches(Literals.Replace(routine.Source.ToUpperInvariant(), " ")).Select(m => m.Value).ToHashSet();
                var blocked = routines.Where(r => r.Blockers.Count > 0 && tokens.Contains(r.Name)).Select(r => r.Name).ToArray();
                if (blocked.Length == 0) continue;
                routines[i] = routine with { Blockers = [$"Depends on blocked routines: {string.Join(", ", blocked)}"] };
                changed = true;
            }
        } while (changed);
        // Only a straight-line prefix can be extracted before an unsupported effect/control
        // boundary. Never resume after that boundary or pretend the parent was converted.
        var qualifiers = expanded.Select(l => Declaration.Match(l))
            .Where(m => m.Success && !m.Groups[3].Value.Contains("PIC", StringComparison.OrdinalIgnoreCase))
            .Select(m => m.Groups[2].Value.ToUpperInvariant()).ToHashSet();
        foreach (var routine in routines.Where(r => r.Blockers.Count > 0).ToArray())
        {
            var prefix = new List<string>();
            var firstLine = 0;
            var lastLine = 0;
            for (var line = routine.StartLine; line < routine.EndLine; line++)
            {
                var text = code[line];
                if (string.IsNullOrWhiteSpace(text) || Header.IsMatch(text)) continue;
                var statementStart = line;
                while (!text.TrimEnd().EndsWith('.') && line + 1 < routine.EndLine)
                    text += "\n" + code[++line];
                var clean = Literals.Replace(text.ToUpperInvariant(), " ");
                if (prefix.Count == 0) firstLine = statementStart + 1;
                if (!Regex.IsMatch(clean, @"^\s*(MOVE|ADD|SUBTRACT|MULTIPLY|DIVIDE|COMPUTE)\b")) break;
                var logical = Regex.Replace(clean, @"\b(?:OF|IN)\s+([A-Z0-9-]+)", m =>
                    qualifiers.Contains(m.Groups[1].Value) ? "" : m.Value);
                if (Words.Matches(logical).Any(m => !Keywords.Contains(m.Value) && !fieldNames.Contains(m.Value))) break;
                if (!text.TrimEnd().EndsWith('.')) break;
                prefix.Add(text);
                lastLine = line + 1;
            }
            if (prefix.Count > 0 && fields.Any(f => f.Name != "W-SECTION" &&
                Words.Matches(Literals.Replace(string.Join("\n", prefix).ToUpperInvariant(), " ")).Any(m => m.Value == f.Name)))
                routines.Add(new(routine.Name + "-PREFIX-" + firstLine, firstLine, lastLine, string.Join("\n", prefix), [])
                { Kind = "straight-line-prefix" });
        }
        return new(identity, Hash(raw), fields, routines, warnings) { Definitions = definitions.Distinct().ToArray() };
    }

    public static string ConversionInput(PartialProgram plan)
    {
        var routines = plan.Selected.ToArray();
        var used = Words.Matches(Literals.Replace(string.Join("\n", routines.Select(r => r.Source)).ToUpperInvariant(), " "))
            .Select(m => m.Value).ToHashSet();
        return JsonSerializer.Serialize(new
        {
            plan.Program,
            plan.SourceSha256,
            Fields = plan.Fields.Where(f => used.Contains(f.Name)),
            Routines = routines.Select(r => new { r.Name, JavaMethodName = MethodName(r.Name), r.Kind, r.StartLine, r.EndLine, r.Source }),
            ExcludedRoutines = plan.Routines.Where(r => r.Blockers.Count > 0).Select(r => new { r.Name, r.Blockers }),
            plan.Definitions,
            plan.Warnings
        });
    }

    public static string MethodName(string routine) =>
        (char.IsDigit(routine[0]) ? "P_" : "") + routine.Replace('-', '_');

    private static PartialField[] ReadFields(string[] lines, HashSet<string> procedureTokens)
    {
        var declarations = new List<(int Level, string Name, string Body, string Root)>();
        string root = "";
        foreach (var line in lines)
        {
            var match = Declaration.Match(line);
            if (match.Success)
            {
                var level = int.Parse(match.Groups[1].Value);
                if (level is 1 or 77) root = match.Groups[2].Value.ToUpperInvariant();
                declarations.Add((level, match.Groups[2].Value.ToUpperInvariant(), match.Groups[3].Value, root));
            }
            else if (declarations.Count > 0 && !string.IsNullOrWhiteSpace(line)
                && !declarations[^1].Body.TrimEnd().EndsWith('.')
                && !Regex.IsMatch(line, @"\b(SECTION|DIVISION|EJECT|SKIP[123]|EXEC)\b"))
            {
                var last = declarations[^1];
                declarations[^1] = (last.Level, last.Name, last.Body + " " + line.Trim(), last.Root);
            }
        }
        var unsafeRoots = declarations.Where(d => Regex.IsMatch(d.Body, @"\b(REDEFINES|OCCURS|RENAMES|EXTERNAL|BASED)\b", RegexOptions.IgnoreCase))
            .Select(d => d.Root).ToHashSet();
        var aliases = declarations.SelectMany(d => Regex.Matches(d.Body, @"\b(?:REDEFINES|RENAMES)\s+([A-Z0-9-]+)", RegexOptions.IgnoreCase)
            .Select(m => m.Groups[1].Value.ToUpperInvariant())).ToHashSet();
        unsafeRoots.UnionWith(declarations.Where(d => aliases.Contains(d.Name)).Select(d => d.Root));
        var fields = declarations.Where(d => !unsafeRoots.Contains(d.Root) && d.Name != "FILLER" && (d.Level < 66 || d.Level == 77)
                && Regex.IsMatch(d.Body, @"\bPIC(?:TURE)?\s+(?:IS\s+)?[SX9AZ][A-Z0-9()VSP.+-]*", RegexOptions.IgnoreCase)
                && !d.Body.Contains('\uFFFD'))
            .GroupBy(d => d.Name).Where(g => g.Count() == 1)
            .Select(g => new PartialField(g.Key, $"{g.First().Level:00} {g.Key} {g.First().Body}")).ToList();
        foreach (var group in declarations.Where(d => d.Level == 1 && !unsafeRoots.Contains(d.Root)
            && string.IsNullOrWhiteSpace(d.Body.Trim('.')) && declarations.Count(other => other.Name == d.Name) == 1))
        {
            var children = declarations.Where(d => d.Root == group.Name && d.Level is > 1 and < 66).ToArray();
            if (children.Length == 0 || children.Any(d => procedureTokens.Contains(d.Name)
                || !Regex.IsMatch(d.Body.Trim(), @"^PIC(?:TURE)?\s+X(?:\(\d+\)|X*)\s*\.?$", RegexOptions.IgnoreCase)))
                continue;
            fields.Add(new(group.Name, $"01 {group.Name}.\n" +
                string.Join("\n", children.Select(d => $"{d.Level:00} {d.Name} {d.Body}")))
            { Kind = "raw-alphanumeric-group" });
        }
        return fields.ToArray();
    }

    private static string Name(string line)
    {
        var m = Header.Match(line);
        return (m.Groups[1].Success ? m.Groups[1].Value : m.Groups[3].Value).ToUpperInvariant();
    }

    private static string Normalize(string line)
    {
        if (line.Length < 7) return "";
        if (line[..6].Any(c => !char.IsDigit(c) && c != ' ')) return line.TrimEnd('\u001a');
        if (line[6] is '*' or '/' or 'D' or 'd') return "";
        var code = line[7..Math.Min(line.Length, 72)].TrimEnd().TrimEnd('\u001a');
        return Regex.IsMatch(code.Trim(), @"^(EJECT|SKIP[123])\.?$", RegexOptions.IgnoreCase) ? "" : code;
    }

    private static string Hash(byte[] bytes) => Convert.ToHexString(SHA256.HashData(bytes)).ToLowerInvariant();

    private static IEnumerable<string> ExpandCopies(IEnumerable<string> lines, string root,
        List<PartialDefinition> definitions, HashSet<string> active, List<string> warnings)
    {
        foreach (var line in lines)
        {
            var copy = Regex.Match(line, @"^\s*COPY\s+['""]?([A-Z0-9-]+)['""]?(.*?)\.\s*$", RegexOptions.IgnoreCase);
            if (!copy.Success) { yield return line; continue; }
            var name = copy.Groups[1].Value;
            var matches = Directory.EnumerateFiles(root, "*", SearchOption.AllDirectories)
                .Where(p => Path.GetExtension(p).Equals(".cpy", StringComparison.OrdinalIgnoreCase)
                    && Path.GetFileNameWithoutExtension(p).Equals(name, StringComparison.OrdinalIgnoreCase)
                    && !Path.GetRelativePath(root, p).Split(Path.DirectorySeparatorChar).Any(s => s.StartsWith('.')))
                .ToArray();
            if (matches.Length != 1 || active.Contains(matches[0]))
            {
                warnings.Add($"COPY {name}: {matches.Length} exact-name candidates or cyclic include; no guessed definition.");
                continue;
            }
            var tail = copy.Groups[2].Value.Trim();
            var replacement = Regex.Match(tail, @"^REPLACING\s+==([A-Z0-9-]+)==\s+BY\s+==([A-Z0-9-]+)==$", RegexOptions.IgnoreCase);
            if (tail.Length > 0 && !replacement.Success)
            {
                warnings.Add($"COPY {name}: unsupported replacement syntax; no expansion.");
                continue;
            }
            var bytes = File.ReadAllBytes(matches[0]);
            definitions.Add(new(matches[0], Hash(bytes)));
            var text = Encoding.UTF8.GetString(bytes);
            var included = ExpandCopies(text.Replace("\r", "").Split('\n').Select(Normalize), root,
                definitions, new HashSet<string>(active) { matches[0] }, warnings);
            foreach (var expanded in included)
                yield return replacement.Success ? ReplaceToken(expanded, replacement.Groups[1].Value, replacement.Groups[2].Value) : expanded;
        }
    }

    private static string ReplaceToken(string line, string from, string to)
    {
        var result = new StringBuilder();
        var start = 0;
        foreach (Match literal in Literals.Matches(line))
        {
            result.Append(Regex.Replace(line[start..literal.Index], @"(?<![A-Z0-9-])" + Regex.Escape(from) +
                @"(?![A-Z0-9-])", to, RegexOptions.IgnoreCase));
            result.Append(literal.Value);
            start = literal.Index + literal.Length;
        }
        result.Append(Regex.Replace(line[start..], @"(?<![A-Z0-9-])" + Regex.Escape(from) +
            @"(?![A-Z0-9-])", to, RegexOptions.IgnoreCase));
        return result.ToString();
    }
}
