using System.Text;
using System.Text.RegularExpressions;

namespace CobolToQuarkusMigration.Jcl;

// Reads the control statements of the utilities a step runs: which program the TSO monitor runs
// under Db2, which datasets IDCAMS deletes, copies or defines, and a sort's statements.
public static class JclControlCards
{
    private static readonly HashSet<string> TsoMonitors = new(StringComparer.OrdinalIgnoreCase)
    {
        "IKJEFT01", "IKJEFT1A", "IKJEFT1B",
    };

    private static readonly HashSet<string> Sorts = new(StringComparer.OrdinalIgnoreCase)
    {
        "SORT", "DFSORT", "ICEMAN", "SYNCSORT", "ICETOOL", "SYNCTOOL",
    };

    // Programs supplied by the system rather than the estate. None of them is converted; the
    // generated job reproduces what they do from their DDs and control statements.
    private static readonly HashSet<string> Utilities = new(StringComparer.OrdinalIgnoreCase)
    {
        "IDCAMS", "IEFBR14", "IEBGENER", "ICEGENER", "IEBCOPY", "IEHPROGM", "IEBCOMPR", "IEBUPDTE",
        "ADUUMAIN", "SYSUTCOM", "DSNUTILB", "DSNTIAUL", "DSNTEP2", "DSNTEP4", "IRXJCL", "EZACFSM1",
        "IKJEFT01", "IKJEFT1A", "IKJEFT1B", "SORT", "DFSORT", "ICEMAN", "SYNCSORT", "ICETOOL", "SYNCTOOL",
    };

    private static readonly Regex Keyword = new(@"\b(?<key>[A-Z]+)\s*\(\s*(?<value>'[^']*'|[^()]*(?:\([^()]*\)[^()]*)*)\s*\)", RegexOptions.IgnoreCase | RegexOptions.Compiled);

    private static readonly Regex DefinedName = new(@"\bNAME\s*\(\s*(?<name>[^()\s]+)\s*\)", RegexOptions.IgnoreCase | RegexOptions.Compiled);

    public static bool IsUtility(string program) => Utilities.Contains(program);

    public static IReadOnlyCollection<string> KnownUtilities => Utilities;

    public static void Apply(JclStep step)
    {
        // The procedure is unknown, and so is which of its DDs is the TSO command stream, but a
        // RUN PROGRAM command in any of its in-stream data still names what it runs.
        if (step.Kind == JclStepKind.UnresolvedProcedure || step.Program is null)
        {
            ReadTso(step, InStream(step.Dds), recordStatements: false);
            return;
        }

        if (TsoMonitors.Contains(step.Program))
        {
            step.Kind = JclStepKind.TsoBatch;
            ReadTso(step, Statements(step, "SYSTSIN"), recordStatements: true);
            return;
        }

        if (!Utilities.Contains(step.Program))
        {
            step.Kind = JclStepKind.Program;
            return;
        }

        step.Kind = JclStepKind.Utility;
        if (Sorts.Contains(step.Program))
            step.ControlStatements.AddRange(Statements(step, "SYSIN", "TOOLIN").Where(l => !l.TrimStart().StartsWith('*')));
        else if (step.Program.Equals("IDCAMS", StringComparison.OrdinalIgnoreCase))
            ReadIdcams(step);
    }

    private static void ReadTso(JclStep step, IEnumerable<string> lines, bool recordStatements)
    {
        string? subsystem = null;
        foreach (var command in Commands(lines))
        {
            var verb = command.Split(' ', 2, StringSplitOptions.RemoveEmptyEntries).FirstOrDefault()?.ToUpperInvariant();
            var args = Arguments(command);
            if (verb == "DSN") subsystem = args.GetValueOrDefault("SYSTEM") ?? subsystem;
            else if (verb == "RUN" && args.TryGetValue("PROGRAM", out var program))
            {
                step.Runs.Add(new JclDb2Run(
                    program.ToUpperInvariant(),
                    args.GetValueOrDefault("PLAN")?.ToUpperInvariant(),
                    subsystem?.ToUpperInvariant(),
                    args.TryGetValue("PARMS", out var parms) ? JclOperands.Unquote(parms) : null));
            }

            if (recordStatements) step.ControlStatements.Add(command);
        }
    }

    private static void ReadIdcams(JclStep step)
    {
        foreach (var command in Commands(Statements(step, "SYSIN")))
        {
            step.ControlStatements.Add(command);
            var words = command.Split(' ', StringSplitOptions.RemoveEmptyEntries);
            var verb = words.FirstOrDefault()?.ToUpperInvariant();
            var args = Arguments(command);

            switch (verb)
            {
                case "DELETE" or "DEL":
                    var target = words.Length > 1 && !words[1].Contains('(') ? words[1] : args.GetValueOrDefault("ENTRY");
                    if (target is not null) step.Effects.Add(new(Dataset(target), JclDatasetAccess.Delete, command));
                    break;

                case "REPRO":
                    if (Resolve(step, args, "INDATASET", "IDS", "INFILE", "IFILE") is { } input)
                        step.Effects.Add(new(input, JclDatasetAccess.Read, command));
                    if (Resolve(step, args, "OUTDATASET", "ODS", "OUTFILE", "OFILE") is { } output)
                        step.Effects.Add(new(output, JclDatasetAccess.Create, command));
                    break;

                case "DEFINE" or "DEF":
                    // NAME sits inside CLUSTER(...) or GDG(...), which Arguments reads as one value.
                    if (DefinedName.Match(command) is { Success: true } defined)
                        step.Effects.Add(new(Dataset(defined.Groups["name"].Value), JclDatasetAccess.Create, command));
                    break;
            }
        }
    }

    // A dataset named directly, or through a DD of the step.
    private static JclDataset? Resolve(JclStep step, IReadOnlyDictionary<string, string> args, string dsnKey, string dsnShort, string ddKey, string ddShort)
    {
        if ((args.GetValueOrDefault(dsnKey) ?? args.GetValueOrDefault(dsnShort)) is { } dsn) return Dataset(dsn);
        var dd = args.GetValueOrDefault(ddKey) ?? args.GetValueOrDefault(ddShort);
        return dd is null ? null : step.Dds.FirstOrDefault(d => d.Name.Equals(dd.Trim(), StringComparison.OrdinalIgnoreCase))?.Dataset;
    }

    private static JclDataset Dataset(string name)
    {
        var n = JclOperands.Unquote(name.Trim()).ToUpperInvariant();
        var paren = n.IndexOf('(');
        return paren > 0 && n.EndsWith(')')
            ? new JclDataset(n[..paren], n[(paren + 1)..^1], null, false)
            : new JclDataset(n, null, null, false);
    }

    private static Dictionary<string, string> Arguments(string command)
    {
        var args = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
        foreach (Match m in Keyword.Matches(command)) args.TryAdd(m.Groups["key"].Value, m.Groups["value"].Value.Trim());
        return args;
    }

    private static IEnumerable<string> Statements(JclStep step, params string[] ddNames) =>
        InStream(step.Dds.Where(d => ddNames.Any(n => d.Name.Split('.')[^1].Equals(n, StringComparison.OrdinalIgnoreCase))));

    private static IEnumerable<string> InStream(IEnumerable<JclDd> dds) =>
        dds.SelectMany(d => d.Concatenated.Prepend(d))
            .SelectMany(d => d.InStream ?? [])
            .Where(l => l.Trim().Length > 0);

    // TSO and IDCAMS continue a command with a trailing '-' or '+'; /* ... */ is a comment.
    private static IEnumerable<string> Commands(IEnumerable<string> lines)
    {
        var current = new StringBuilder();
        foreach (var line in lines.Select(raw => Regex.Replace(raw, @"/\*.*?\*/", " ").Trim()).Where(l => l.Length > 0))
        {
            var continued = line.EndsWith('-') || line.EndsWith('+');
            if (current.Length > 0) current.Append(' ');
            current.Append(continued ? line[..^1].TrimEnd() : line);
            if (continued) continue;
            yield return current.ToString();
            current.Clear();
        }

        if (current.Length > 0) yield return current.ToString();
    }
}
