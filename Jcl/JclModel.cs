using System.Text.Json.Serialization;

namespace CobolToQuarkusMigration.Jcl;

[JsonConverter(typeof(JsonStringEnumConverter))]
public enum JclStepKind
{
    // Runs a program that is part of the estate, or one nothing classifies as a utility.
    Program,
    // Runs a system utility (sort, IDCAMS, a copy program, a Db2 utility).
    Utility,
    // Runs the TSO monitor, which runs the program named in SYSTSIN.
    TsoBatch,
    // Calls a procedure that is not in the source, so what it runs is unknown.
    UnresolvedProcedure,
}

[JsonConverter(typeof(JsonStringEnumConverter))]
public enum JclDatasetAccess
{
    Read,
    Create,
    Append,
    // DISP=OLD: exclusive, and the JCL alone does not say whether the program reads or writes.
    Exclusive,
    Delete,
}

public sealed record JclCondition(string Expression, bool Negated);

public sealed record JclDataset(
    string Name,
    string? Member,
    string? Generation,
    bool Temporary);

public sealed record JclDd
{
    public required string Name { get; init; }
    public int Line { get; init; }
    public JclDataset? Dataset { get; init; }
    public string? Status { get; init; }
    public string? NormalDisposition { get; init; }
    public string? AbnormalDisposition { get; init; }
    public string? SysOut { get; init; }
    public bool Dummy { get; init; }
    public string? RecordFormat { get; init; }
    public int? RecordLength { get; init; }
    public IReadOnlyList<string>? InStream { get; init; }

    // Unnamed DDs that follow are read as one file with this one.
    public List<JclDd> Concatenated { get; init; } = [];

    [JsonIgnore]
    public JclOperands Operands { get; init; } = new();

    public JclDatasetAccess? Access => Dataset is null ? null : Status switch
    {
        "SHR" => JclDatasetAccess.Read,
        "OLD" => JclDatasetAccess.Exclusive,
        "MOD" => JclDatasetAccess.Append,
        _ => JclDatasetAccess.Create,
    };
}

public sealed record JclDatasetEffect(JclDataset Dataset, JclDatasetAccess Access, string Source);

public sealed record JclDb2Run(string Program, string? Plan, string? Subsystem, string? Parms);

public sealed record JclStep
{
    public required string Name { get; init; }
    public int Line { get; init; }
    public JclStepKind Kind { get; set; }
    public string? Program { get; init; }
    public string? Procedure { get; init; }
    public string? ProcedureStep { get; init; }
    public string? Parm { get; init; }
    public string? Cond { get; init; }
    public IReadOnlyList<JclCondition> Conditions { get; init; } = [];
    public List<JclDd> Dds { get; init; } = [];

    // Programs the step runs that its EXEC does not name: RUN PROGRAM in SYSTSIN.
    public List<JclDb2Run> Runs { get; init; } = [];

    // Control statements for the utility the step runs (sort cards, IDCAMS commands).
    public List<string> ControlStatements { get; init; } = [];

    // Datasets a utility touches through its control statements rather than a DD's DISP.
    public List<JclDatasetEffect> Effects { get; init; } = [];

    // The programs this step runs: the EXEC's program unless it is only a monitor, and every RUN PROGRAM.
    [JsonIgnore]
    public IEnumerable<string> ProgramsRun =>
        (Kind == JclStepKind.Program && Program is not null ? [Program] : Array.Empty<string>())
            .Concat(Runs.Select(r => r.Program));
}

public sealed record JclDiagnostic(int Line, string Code, string Message);

public sealed record JclJob
{
    public required string Name { get; init; }
    public required string File { get; init; }
    public string? JobClass { get; init; }
    public string? Cond { get; init; }
    public List<string> ProcLibraries { get; init; } = [];
    public List<string> Includes { get; init; } = [];
    public List<JclStep> Steps { get; init; } = [];
    public SortedDictionary<string, string> Symbols { get; init; } = new(StringComparer.Ordinal);

    // Symbols the JCL uses but never sets: usually scheduler variables filled in at submission.
    public SortedSet<string> UnresolvedSymbols { get; init; } = new(StringComparer.Ordinal);

    // Symbols in in-stream data. JCL does not substitute these; a scheduler usually does.
    public SortedSet<string> InStreamSymbols { get; init; } = new(StringComparer.Ordinal);
    public List<JclDiagnostic> Diagnostics { get; init; } = [];
}
