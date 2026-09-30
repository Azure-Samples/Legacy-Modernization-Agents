using System.Text;
using System.Text.RegularExpressions;

namespace CobolToQuarkusMigration.Jcl;

// Reads one job: substitutes symbols, splices INCLUDE members, expands in-stream and catalogued
// procedures with their overrides, and records the IF/THEN/ELSE conditions each step runs under.
// What cannot be resolved from the source is reported as a diagnostic, never guessed.
public sealed class JclParser
{
    // EXEC keywords that are parameters rather than symbolic overrides for a procedure.
    private static readonly HashSet<string> ExecParameters = new(StringComparer.OrdinalIgnoreCase)
    {
        "PGM", "PROC", "PARM", "PARMDD", "COND", "REGION", "REGIONX", "TIME", "ACCT", "ADDRSPC",
        "DPRTY", "DYNAMNBR", "PERFORM", "RD", "CCSID", "MEMLIMIT", "TVSMSG", "TVSAMCOM",
    };

    private static readonly Regex Generation = new(@"^[+-]?\d+$", RegexOptions.Compiled);
    private static readonly Regex InStreamSymbol = new(@"(?<!&)&(?<name>[A-Z@#$][A-Z0-9@#$]{0,7})", RegexOptions.Compiled | RegexOptions.IgnoreCase);

    private readonly JclMemberLibrary _library;

    public JclParser(JclMemberLibrary? library = null) => _library = library ?? JclMemberLibrary.Empty;

    public JclJob Parse(string text, string file)
    {
        var statements = JclLexer.Lex(text);
        var card = statements.FirstOrDefault(s => s.Operation == "JOB");
        var jobOperands = card is null ? new JclOperands() : JclOperands.Parse(card.Operands);
        var job = new JclJob
        {
            Name = card?.Name ?? Path.GetFileNameWithoutExtension(file).ToUpperInvariant(),
            File = file,
            JobClass = jobOperands.Keyword("CLASS"),
            Cond = jobOperands.Keyword("COND"),
        };
        if (card is null) job.Diagnostics.Add(new(0, "NO_JOB_CARD", "No JOB statement; the file name is used as the job name."));

        var state = new ParseState(job);
        var body = Prepare(statements, state, [], topLevel: true);
        var root = new Scope(new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase), [], null, null, [], new HashSet<string>(StringComparer.OrdinalIgnoreCase));
        Walk(body, root, state);

        foreach (var step in job.Steps)
        {
            JclControlCards.Apply(step);
            foreach (var dd in step.Dds.SelectMany(d => d.Concatenated.Prepend(d)))
            foreach (var line in dd.InStream ?? [])
            foreach (Match m in InStreamSymbol.Matches(line))
                job.InStreamSymbols.Add(m.Groups["name"].Value.ToUpperInvariant());
        }

        return job;
    }

    private sealed class ParseState(JclJob job)
    {
        public JclJob Job { get; } = job;
        public Dictionary<string, List<JclStatement>> InStreamProcedures { get; } = new(StringComparer.OrdinalIgnoreCase);
    }

    private sealed record Scope(
        Dictionary<string, string> Symbols,
        IReadOnlyList<JclCondition> Conditions,
        string? Procedure,
        string? Caller,
        IReadOnlyList<KeyValuePair<string, string>> Overrides,
        IReadOnlySet<string> CallStack);

    private sealed class ProcedureCall
    {
        public List<JclStep> Steps { get; } = [];
        public JclStep? Unresolved { get; init; }
        public JclStep? LastTarget { get; set; }
    }

    // Splices INCLUDE members, records JCLLIB, and lifts in-stream procedures out of the job.
    private List<JclStatement> Prepare(IReadOnlyList<JclStatement> statements, ParseState state, HashSet<string> including, bool topLevel)
    {
        var result = new List<JclStatement>();
        for (var i = 0; i < statements.Count; i++)
        {
            var s = statements[i];
            switch (s.Operation)
            {
                case "JCLLIB":
                    state.Job.ProcLibraries.AddRange(JclOperands.Sublist(JclOperands.Parse(s.Operands).Keyword("ORDER"))
                        .Select(l => JclOperands.Unquote(l).ToUpperInvariant()));
                    break;

                case "INCLUDE":
                    var member = JclOperands.Parse(s.Operands).Keyword("MEMBER") is { } m ? JclOperands.Unquote(m).ToUpperInvariant() : "";
                    state.Job.Includes.Add(member);
                    if (!including.Add(member))
                    {
                        state.Job.Diagnostics.Add(new(s.Line, "INCLUDE_RECURSION", $"INCLUDE {member} includes itself."));
                        break;
                    }

                    var found = _library.Find(member, state.Job.ProcLibraries, _ => true, out var candidates);
                    if (found is null)
                        state.Job.Diagnostics.Add(new(s.Line, "INCLUDE_NOT_FOUND", $"INCLUDE member {member} is not in the source."));
                    else
                    {
                        Ambiguity(state.Job, s.Line, member, candidates, found);
                        result.AddRange(Prepare(JclLexer.Lex(found.Text), state, including, topLevel));
                    }

                    including.Remove(member);
                    break;

                case "PROC" when topLevel && s.Name is not null:
                    var body = new List<JclStatement> { s };
                    while (++i < statements.Count && statements[i].Operation != "PEND") body.Add(statements[i]);
                    state.InStreamProcedures[s.Name] = body;
                    break;

                default:
                    result.Add(s);
                    break;
            }
        }

        return result;
    }

    private void Walk(IReadOnlyList<JclStatement> statements, Scope scope, ParseState state)
    {
        var job = state.Job;
        var ifs = new List<JclCondition>();
        var ifLines = new List<int>();
        JclStep? current = null;
        ProcedureCall? call = null;
        JclDd? last = null;
        var stepIndex = 0;

        foreach (var s in statements)
        {
            var unresolved = job.UnresolvedSymbols;
            switch (s.Operation)
            {
                case "SET":
                    foreach (var (key, value) in JclOperands.Parse(s.Operands).Keywords)
                    {
                        var resolved = JclOperands.Unquote(Substitute(value, scope.Symbols, unresolved));
                        scope.Symbols[key] = resolved;
                        if (scope.Procedure is null) job.Symbols[key] = resolved;
                    }
                    break;

                case "IF":
                    ifs.Add(new JclCondition(Substitute(s.Operands, scope.Symbols, unresolved), false));
                    ifLines.Add(s.Line);
                    break;

                case "ELSE":
                    if (ifs.Count == 0) job.Diagnostics.Add(new(s.Line, "UNBALANCED_IF", "ELSE without IF."));
                    else ifs[^1] = ifs[^1] with { Negated = true };
                    break;

                case "ENDIF":
                    if (ifs.Count == 0) job.Diagnostics.Add(new(s.Line, "UNBALANCED_IF", "ENDIF without IF."));
                    else { ifs.RemoveAt(ifs.Count - 1); ifLines.RemoveAt(ifLines.Count - 1); }
                    break;

                case "EXEC":
                    last = null;
                    current = null;
                    call = null;
                    var operands = JclOperands.Parse(Substitute(s.Operands, scope.Symbols, unresolved));
                    var conditions = scope.Conditions.Concat(ifs).ToList();
                    var name = QualifiedName(scope, s.Name ?? $"@{s.Line}");

                    if (operands.Keyword("PGM") is { } pgm)
                    {
                        current = new JclStep
                        {
                            Name = name,
                            Line = s.Line,
                            Program = JclOperands.Unquote(pgm).ToUpperInvariant(),
                            Procedure = scope.Procedure,
                            ProcedureStep = scope.Procedure is null ? null : s.Name,
                            Parm = Override(scope, "PARM", s.Name, stepIndex) ?? operands.Keyword("PARM"),
                            Cond = Override(scope, "COND", s.Name, stepIndex) ?? operands.Keyword("COND"),
                            Conditions = conditions,
                        };
                        job.Steps.Add(current);
                        stepIndex++;
                    }
                    else
                    {
                        var procedure = (operands.Keyword("PROC") ?? operands.Positional.FirstOrDefault() ?? "").ToUpperInvariant();
                        call = Expand(procedure, name, s, operands, conditions, scope, state);
                        stepIndex += Math.Max(1, call.Steps.Count);
                    }
                    break;

                case "DD":
                    var ddOperands = JclOperands.Parse(Substitute(s.Operands, scope.Symbols, unresolved));
                    if (s.Name is null)
                    {
                        if (last is not null) last.Concatenated.Add(BuildDd(last.Name, s, ddOperands, job));
                        break;
                    }

                    if (call is not null) last = ApplyOverride(call, s, ddOperands, job);
                    else if (current is not null)
                    {
                        last = BuildDd(s.Name, s, ddOperands, job);
                        current.Dds.Add(last);
                    }
                    break;
            }
        }

        for (var i = 0; i < ifs.Count; i++)
            job.Diagnostics.Add(new(ifLines[i], "UNBALANCED_IF", "IF without ENDIF."));
    }

    private ProcedureCall Expand(
        string procedure, string stepName, JclStatement exec, JclOperands operands,
        IReadOnlyList<JclCondition> conditions, Scope scope, ParseState state)
    {
        var job = state.Job;
        List<JclStatement>? body = null;
        if (scope.CallStack.Contains(procedure))
            job.Diagnostics.Add(new(exec.Line, "PROC_RECURSION", $"Procedure {procedure} calls itself."));
        else if (state.InStreamProcedures.TryGetValue(procedure, out var inStream))
            body = inStream;
        else if (_library.Find(procedure, job.ProcLibraries, IsProcedure, out var candidates) is { } member)
        {
            Ambiguity(job, exec.Line, procedure, candidates, member);
            body = Prepare(JclLexer.Lex(member.Text), state, [], topLevel: false);
        }
        else
            job.Diagnostics.Add(new(exec.Line, "PROC_NOT_FOUND", $"Procedure {procedure} is not in the source."));

        if (body is null)
        {
            var step = new JclStep
            {
                Name = stepName,
                Line = exec.Line,
                Kind = JclStepKind.UnresolvedProcedure,
                Procedure = procedure,
                Parm = operands.Keyword("PARM"),
                Cond = operands.Keyword("COND"),
                Conditions = conditions,
            };
            job.Steps.Add(step);
            return new ProcedureCall { Unresolved = step };
        }

        var symbols = new Dictionary<string, string>(scope.Symbols, StringComparer.OrdinalIgnoreCase);
        var header = body.FirstOrDefault(s => s.Operation == "PROC");
        if (header is not null)
            foreach (var (key, value) in JclOperands.Parse(header.Operands).Keywords)
                symbols[key] = JclOperands.Unquote(value);

        var overrides = new List<KeyValuePair<string, string>>();
        foreach (var kv in operands.Keywords)
        {
            var baseKey = kv.Key.Split('.')[0];
            if (ExecParameters.Contains(baseKey)) { if (baseKey is not ("PGM" or "PROC")) overrides.Add(kv); }
            else symbols[kv.Key] = JclOperands.Unquote(kv.Value);
        }

        var inner = new Scope(symbols, conditions, procedure, stepName, overrides,
            new HashSet<string>(scope.CallStack, StringComparer.OrdinalIgnoreCase) { procedure });
        var before = job.Steps.Count;
        Walk(body.Where(s => s.Operation != "PROC").ToList(), inner, state);

        var expanded = new ProcedureCall();
        expanded.Steps.AddRange(job.Steps.Skip(before));
        return expanded;
    }

    private static bool IsProcedure(string text) => JclLexer.Lex(text).Any(s => s.Operation == "PROC");

    // PARM without a step name applies to the first procedure step and nullifies the rest; other
    // parameters without a step name apply to every step.
    private static string? Override(Scope scope, string key, string? procStep, int index)
    {
        if (scope.Procedure is null) return null;
        var qualified = scope.Overrides.LastOrDefault(o => o.Key.Equals($"{key}.{procStep}", StringComparison.OrdinalIgnoreCase));
        if (qualified.Key is not null) return qualified.Value;
        var plain = scope.Overrides.LastOrDefault(o => o.Key.Equals(key, StringComparison.OrdinalIgnoreCase));
        if (plain.Key is null) return null;
        return key == "PARM" && index > 0 ? "" : plain.Value;
    }

    private static string QualifiedName(Scope scope, string name) =>
        scope.Caller is null ? name : $"{scope.Caller}.{name}";

    // A DD after a procedure call overrides or adds to a procedure step: the one it names, else the
    // one the previous override named, else the first.
    private static JclDd ApplyOverride(ProcedureCall call, JclStatement s, JclOperands operands, JclJob job)
    {
        var name = s.Name!;
        if (call.Unresolved is not null)
        {
            var dd = BuildDd(name, s, operands, job);
            call.Unresolved.Dds.Add(dd);
            return dd;
        }

        var dot = name.LastIndexOf('.');
        var ddName = dot < 0 ? name : name[(dot + 1)..];
        JclStep? target;
        if (dot < 0) target = call.LastTarget ?? call.Steps.FirstOrDefault();
        else
        {
            var procStep = name[..dot];
            target = call.Steps.FirstOrDefault(st => st.ProcedureStep?.Equals(procStep, StringComparison.OrdinalIgnoreCase) == true
                                                    || st.Name.EndsWith("." + procStep, StringComparison.OrdinalIgnoreCase));
            if (target is null)
                job.Diagnostics.Add(new(s.Line, "OVERRIDE_TARGET_MISSING", $"{name} names a step the procedure does not have."));
            else call.LastTarget = target;
        }

        if (target is null)
        {
            var orphan = BuildDd(name, s, operands, job);
            return orphan;
        }

        var index = target.Dds.FindIndex(d => d.Name.Equals(ddName, StringComparison.OrdinalIgnoreCase));
        if (index < 0)
        {
            var added = BuildDd(ddName, s, operands, job);
            target.Dds.Add(added);
            return added;
        }

        var merged = BuildDd(ddName, s, Merge(target.Dds[index].Operands, operands), job,
            s.InStream ?? (Replaces(operands) ? null : target.Dds[index].InStream));
        target.Dds[index] = merged;
        return merged;
    }

    private static bool Replaces(JclOperands o) =>
        o.Positional.FirstOrDefault()?.ToUpperInvariant() is "*" or "DATA" or "DUMMY" || o.Has("SYSOUT");

    private static JclOperands Merge(JclOperands original, JclOperands overriding)
    {
        if (Replaces(overriding)) return overriding;

        var merged = new JclOperands();
        var dropDummy = overriding.Has("DSN") || overriding.Has("DSNAME");
        merged.Positional.AddRange((overriding.Positional.Count > 0 ? overriding.Positional : original.Positional)
            .Where(p => !(dropDummy && p.Equals("DUMMY", StringComparison.OrdinalIgnoreCase))));
        merged.Keywords.AddRange(original.Keywords.Where(k => !overriding.Has(k.Key)));
        // An override with no value nullifies the parameter.
        merged.Keywords.AddRange(overriding.Keywords.Where(k => k.Value.Length > 0));
        return merged;
    }

    private static JclDd BuildDd(string name, JclStatement s, JclOperands o, JclJob job, IReadOnlyList<string>? inStream = null)
    {
        var positional = o.Positional.FirstOrDefault()?.ToUpperInvariant();
        var dsn = o.Keyword("DSN") ?? o.Keyword("DSNAME");
        var dataset = dsn is null ? null : ParseDataset(dsn, s.Line, job);
        var dummy = positional == "DUMMY" || dsn?.Trim().Equals("NULLFILE", StringComparison.OrdinalIgnoreCase) == true;
        if (dummy) dataset = null;

        var disp = JclOperands.Sublist(o.Keyword("DISP"));
        string? Item(int i) => i < disp.Count && disp[i].Trim().Length > 0 ? disp[i].Trim().ToUpperInvariant() : null;

        var dcb = JclOperands.Parse(string.Join(',', JclOperands.Sublist(o.Keyword("DCB"))));
        var recfm = o.Keyword("RECFM") ?? dcb.Keyword("RECFM");
        var lrecl = o.Keyword("LRECL") ?? dcb.Keyword("LRECL");

        return new JclDd
        {
            Name = name,
            Line = s.Line,
            Dataset = dataset,
            Status = Item(0),
            NormalDisposition = Item(1),
            AbnormalDisposition = Item(2),
            SysOut = o.Keyword("SYSOUT"),
            Dummy = dummy,
            RecordFormat = recfm?.ToUpperInvariant(),
            RecordLength = int.TryParse(lrecl, out var l) ? l : null,
            InStream = inStream ?? s.InStream,
            Operands = o,
        };
    }

    private static JclDataset? ParseDataset(string dsn, int line, JclJob job)
    {
        var name = JclOperands.Unquote(dsn).ToUpperInvariant();

        // A backward reference names an earlier DD: *.step.dd or *.step.procstep.dd.
        if (name.StartsWith("*.", StringComparison.Ordinal))
        {
            var path = name[2..];
            var dot = path.LastIndexOf('.');
            var (stepPath, ddName) = dot < 0 ? ("", path) : (path[..dot], path[(dot + 1)..]);
            var referenced = job.Steps
                .Where(st => st.Name.Equals(stepPath, StringComparison.OrdinalIgnoreCase)
                             || st.Name.EndsWith("." + stepPath, StringComparison.OrdinalIgnoreCase))
                .SelectMany(st => st.Dds)
                .LastOrDefault(d => d.Name.Equals(ddName, StringComparison.OrdinalIgnoreCase))?.Dataset;
            if (referenced is null)
                job.Diagnostics.Add(new(line, "BACKREF_UNRESOLVED", $"DSN={name} refers to a DD this job does not have."));
            return referenced;
        }

        string? member = null, generation = null;
        var paren = name.IndexOf('(');
        if (paren > 0 && name.EndsWith(')'))
        {
            var inner = name[(paren + 1)..^1];
            if (Generation.IsMatch(inner)) generation = inner;
            else member = inner;
            name = name[..paren];
        }

        return new JclDataset(name, member, generation, name.StartsWith('&'));
    }

    // &NAME or &NAME. is replaced by the symbol's value; && starts a temporary dataset name.
    private static string Substitute(string text, IReadOnlyDictionary<string, string> symbols, ISet<string> unresolved)
    {
        if (!text.Contains('&')) return text;
        var sb = new StringBuilder(text.Length);
        var i = 0;
        while (i < text.Length)
        {
            if (text[i] != '&') { sb.Append(text[i++]); continue; }
            if (i + 1 < text.Length && text[i + 1] == '&')
            {
                sb.Append("&&");
                i += 2;
                while (i < text.Length && IsNameChar(text[i])) sb.Append(text[i++]);
                continue;
            }

            var start = i + 1;
            var end = start;
            while (end < text.Length && end - start < 8 && IsNameChar(text[end])) end++;
            var name = text[start..end];
            if (name.Length == 0 || char.IsDigit(name[0])) { sb.Append('&'); i++; continue; }

            if (symbols.TryGetValue(name, out var value))
            {
                sb.Append(value);
                i = end < text.Length && text[end] == '.' ? end + 1 : end;
            }
            else
            {
                unresolved.Add(name.ToUpperInvariant());
                sb.Append('&').Append(name);
                i = end;
            }
        }

        return sb.ToString();
    }

    private static bool IsNameChar(char c) => char.IsAsciiLetterOrDigit(c) || c is '@' or '#' or '$';

    private static void Ambiguity(JclJob job, int line, string member, int candidates, JclMember chosen)
    {
        if (candidates > 1)
            job.Diagnostics.Add(new(line, "MEMBER_AMBIGUOUS", $"{candidates} files are named {member}; used {chosen.File}."));
    }
}
