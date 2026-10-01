using System.Text;

namespace CobolToQuarkusMigration.Jcl.Generation;

public enum JclPlanStepKind
{
    // Runs a converted program through the batch-program contract.
    Program,
    // Runs the programs named by RUN PROGRAM under the TSO monitor, in order.
    Tso,
    // Runs a system utility through the job utilities.
    Utility,
    // Calls a procedure that is not in the source; it fails when reached rather than guess.
    Unresolved,
}

public sealed record JclPlanStep
{
    public required string Name { get; init; }
    public required JclPlanStepKind Kind { get; init; }
    public string? Program { get; init; }
    public string? Procedure { get; init; }
    public string? Parm { get; init; }
    public IReadOnlyList<JclDb2Run> Runs { get; init; } = [];
    public JclExpr? Guard { get; init; }
    public string? GuardSource { get; init; }
    // Set when a condition could not be read; the step fails when reached.
    public string? GuardError { get; init; }
    public JclCondParameter? Cond { get; init; }
    public IReadOnlyList<JclDd> Dds { get; init; } = [];
    public IReadOnlyList<string> ControlStatements { get; init; } = [];
}

public sealed record JclJobPlan
{
    public required string JobName { get; init; }
    public required string TypeName { get; init; }
    public required string File { get; init; }
    public JclCondParameter? JobCond { get; init; }
    public required IReadOnlyList<JclPlanStep> Steps { get; init; }
    public required IReadOnlyList<string> Programs { get; init; }
    public required IReadOnlyList<JclDiagnostic> Diagnostics { get; init; }

    public static JclJobPlan From(JclJob job)
    {
        var diagnostics = new List<JclDiagnostic>(job.Diagnostics);
        var names = job.Steps.Select(s => s.Name).ToList();
        var steps = job.Steps.Select(step => PlanStep(step, names, diagnostics)).ToList();

        var jobCond = JclConditions.ParseCond(job.Cond, out var jobCondError);
        if (jobCondError is not null)
        {
            // The job may have ended early on the mainframe; stop before the first step rather than run it all.
            diagnostics.Add(new(0, "COND_UNREADABLE", $"JOB COND: {jobCondError} The job stops before its first step."));
            if (steps.Count > 0 && steps[0].GuardError is null)
                steps[0] = steps[0] with { GuardError = $"JOB COND={job.Cond}: {jobCondError}" };
        }

        return new JclJobPlan
        {
            JobName = job.Name,
            TypeName = ToTypeName(job.Name),
            File = job.File,
            JobCond = jobCond,
            Steps = steps,
            Programs = JclEstate.Programs(job),
            Diagnostics = diagnostics,
        };
    }

    // NITEJ014 becomes Nitej014; characters a type name cannot hold become '_'.
    public static string ToTypeName(string jobName)
    {
        var sb = new StringBuilder();
        foreach (var c in jobName)
            sb.Append(char.IsLetterOrDigit(c) ? (sb.Length == 0 ? char.ToUpperInvariant(c) : char.ToLowerInvariant(c)) : '_');
        if (sb.Length == 0 || char.IsDigit(sb[0])) sb.Insert(0, 'J');
        return sb.ToString();
    }

    private static JclPlanStep PlanStep(JclStep step, IReadOnlyList<string> names, List<JclDiagnostic> diagnostics)
    {
        var kind = step.Kind switch
        {
            JclStepKind.Program => JclPlanStepKind.Program,
            JclStepKind.TsoBatch when step.Runs.Count > 0 => JclPlanStepKind.Tso,
            JclStepKind.UnresolvedProcedure => JclPlanStepKind.Unresolved,
            _ => JclPlanStepKind.Utility,
        };

        JclExpr? guard = null;
        string? guardError = null;
        foreach (var condition in step.Conditions)
        {
            var parsed = JclConditions.ParseExpression(condition.Expression, out var error);
            if (parsed is null)
            {
                guardError = $"IF {condition.Expression}: {error}";
                diagnostics.Add(new(step.Line, "IF_UNREADABLE", $"Step {step.Name}: {guardError}"));
                break;
            }
            parsed = Qualify(parsed, step.Name, names, diagnostics, step.Line);
            var term = condition.Negated ? new JclNot(parsed) : parsed;
            guard = guard is null ? term : new JclLogical(guard, true, term);
        }

        var cond = JclConditions.ParseCond(step.Cond, out var condError);
        if (condError is not null)
        {
            guardError ??= $"COND {step.Cond}: {condError}";
            diagnostics.Add(new(step.Line, "COND_UNREADABLE", $"Step {step.Name}: {condError}"));
        }
        if (cond is not null)
            cond = cond with { Tests = cond.Tests.Select(t => t.Step is null ? t : t with { Step = Resolve(t.Step, step.Name, names, diagnostics, step.Line) }).ToList() };

        return new JclPlanStep
        {
            Name = step.Name,
            Kind = kind,
            Program = step.Program,
            Procedure = step.Procedure,
            Parm = step.Parm is null ? null : JclOperands.Unquote(step.Parm),
            Runs = step.Runs,
            Guard = guardError is null ? guard : null,
            GuardSource = step.Conditions.Count == 0 ? null
                : string.Join(" AND ", step.Conditions.Select(c => (c.Negated ? "NOT " : "") + "(" + c.Expression + ")")),
            GuardError = guardError,
            Cond = cond,
            Dds = step.Dds,
            ControlStatements = step.ControlStatements,
        };
    }

    private static JclExpr Qualify(JclExpr expression, string current, IReadOnlyList<string> names, List<JclDiagnostic> diagnostics, int line) =>
        expression switch
        {
            JclReturnCodeTest { Step: { } s } t => t with { Step = Resolve(s, current, names, diagnostics, line) },
            JclAbendCodeTest { Step: { } s } t => t with { Step = Resolve(s, current, names, diagnostics, line) },
            JclAbendTest { Step: { } s } t => t with { Step = Resolve(s, current, names, diagnostics, line) },
            JclRunTest t => t with { Step = Resolve(t.Step, current, names, diagnostics, line) },
            JclNot n => new JclNot(Qualify(n.Operand, current, names, diagnostics, line)),
            JclLogical l => l with
            {
                Left = Qualify(l.Left, current, names, diagnostics, line),
                Right = Qualify(l.Right, current, names, diagnostics, line),
            },
            _ => expression,
        };

    // Inside an expanded procedure a condition names its sibling steps without the calling step.
    // A name that calls a procedure stands for all of its steps, which the runtime aggregates.
    private static string Resolve(string reference, string current, IReadOnlyList<string> names, List<JclDiagnostic> diagnostics, int line)
    {
        if (names.Contains(reference, StringComparer.OrdinalIgnoreCase)) return reference.ToUpperInvariant();
        var dot = current.IndexOf('.');
        if (dot > 0)
        {
            var sibling = current[..dot] + "." + reference;
            if (names.Contains(sibling, StringComparer.OrdinalIgnoreCase)) return sibling.ToUpperInvariant();
        }
        if (names.Any(n => n.StartsWith(reference + ".", StringComparison.OrdinalIgnoreCase))) return reference.ToUpperInvariant();
        // step.procstep where the procedure is not in the source: only the calling step exists, and
        // PROC_NOT_FOUND already says why.
        var caller = reference.Split('.')[0];
        if (caller.Length < reference.Length && names.Contains(caller, StringComparer.OrdinalIgnoreCase)) return caller.ToUpperInvariant();

        diagnostics.Add(new(line, "STEP_REFERENCE_MISSING",
            $"Step {current} tests step {reference}, which the job does not have; the step counts as never run."));
        return reference.ToUpperInvariant();
    }
}
