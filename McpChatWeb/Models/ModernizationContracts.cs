namespace McpChatWeb.Services;

public sealed class DependencyHealthSnapshot
{
    public int TotalPrograms { get; set; }
    public int FullFidelityCount { get; set; }
    public int PartialFidelityCount { get; set; }
    public int DepsOnlyCount { get; set; }
    public int FailedCount { get; set; }
    public int NotParsedCount { get; set; }
    public int ScanCacheBackedCount { get; set; }
    // False when nothing in the estate carries a measured outcome, so a zero coverage figure
    // would report absent evidence as a bad result.
    public bool CoverageMeasured { get; set; }
    public double CoveragePct { get; set; }
    public int TotalMissingCopybooks { get; set; }
    public int ProgramsBlockedByMissing { get; set; }
    public double ReadinessScore { get; set; }
    public List<MissingCopybookRow> MissingCopybooks { get; } = new();
    public List<ProgramHealthRow> Programs { get; } = new();
    public string? Note { get; set; }
}

public sealed record ProgramHealthRow(
    string Basename,
    string RelativePath,
    int LinesOfCode,
    string ParseFidelity,
    string FidelitySource,
    string? ScanOutcome,
    int FactsConfidence,
    int FactsWarnings,
    int MissingCopybookCount,
    bool HasReport,
    bool HasDepsOnly,
    bool AmbiguousBasename);

public sealed class TopologySnapshot
{
    public List<TopologyNode> Nodes { get; } = new();
    public List<TopologyEdge> Edges { get; } = new();

    // Targets with no matching source file, or whose name maps to several.
    public List<TopologyEdge> UnresolvedEdges { get; } = new();

    public string? Note { get; set; }
}

public sealed record TopologyNode(
    string Id,
    string Basename,
    string Kind,
    int LinesOfCode,
    bool HasFacts,
    int FactsConfidence,
    string ParseFidelity,
    string FidelitySource,
    bool AmbiguousBasename);

public sealed record TopologyEdge(string Source, string Target, string Kind);

public sealed class FlowSnapshot
{
    public string Identity { get; set; } = "";
    public string Basename { get; set; } = "";
    public string RelativePath { get; set; } = "";
    public string ParseFidelity { get; set; } = Services.ParseFidelity.NotParsed;
    public string FidelitySource { get; set; } = FidelitySources.None;
    public bool HasFlowAst { get; set; }
    public bool HasCfg { get; set; }
    public bool HasDataStructures { get; set; }
    public int FlowAstFiles { get; set; }
    public List<string> FlowAstNames { get; set; } = new();
    public List<string> Candidates { get; set; } = new();
    public string? ReportDirectory { get; set; }
    public string? Note { get; set; }

    public int ParagraphCount { get; set; }
    public List<FlowSection> Sections { get; set; } = new();
    public List<FlowPerformEdge> PerformEdges { get; set; } = new();
    public List<FlowSqlStatement> SqlStatements { get; set; } = new();
    public List<FlowCallTarget> CallTargets { get; set; } = new();
}

public sealed record FlowParagraph(string Name, int StartLine, int EndLine);

public sealed record FlowSection(
    string Name, int StartLine, int EndLine, List<FlowParagraph> Paragraphs);

public sealed record FlowPerformEdge(string From, string To, bool Conditional);

public sealed record FlowSqlStatement(
    string Operation, List<string> Tables, int LineNumber, string? Excerpt);

public sealed record FlowCallTarget(string TargetProgram, bool IsDynamic, int LineNumber);

public sealed class ServiceChainSnapshot
{
    public int TotalJobs { get; set; }
    public int TotalPrograms { get; set; }
    public int TotalCopybooks { get; set; }
    public int JobToProgramEdges { get; set; }
    public int ProgramToCopybookEdges { get; set; }
    public int MermaidEdgeCount { get; set; }
    public bool MermaidTruncated { get; set; }
    public List<JclJob> Jobs { get; } = new();
    // Every job name in the estate, so a filtered response can still populate the picker.
    public List<string> AllJobNames { get; } = new();
    public List<ProgramChain> Programs { get; } = new();
    // Steps whose program could not be determined. Without these a PROC-driven estate
    // looks unscheduled, which reads as a finding rather than as missing evidence.
    public List<UnresolvedStep> UnresolvedSteps { get; } = new();
    public string Mermaid { get; set; } = "";
    public string? Note { get; set; }
}

public sealed record UnresolvedStep(string JobName, string StepName, string ProcName);

public sealed record JclJob(
    string JobName,
    string JclFileName,
    string RelativePath,
    List<string> PrimaryPrograms);

public sealed record ProgramChain(
    string Basename,
    string Stem,
    string RelativePath,
    int LinesOfCode,
    string ParseFidelity,
    List<string> Copybooks,
    List<string> CalledByJobs);

/// <summary>
/// Everything known about one program, gathered for the view a person uses when deciding whether
/// to convert it: what it depends on, what depends on it, and how much of that is actually known.
/// </summary>
/// <summary>
/// One row in the program picker. Carries only what a conversion decision needs: how big the
/// program is, how well it parsed, and whether converting it drags other programs along.
/// </summary>
public sealed class ProgramListEntry
{
    public string Basename { get; set; } = "";
    public string RelativePath { get; set; } = "";
    public int LinesOfCode { get; set; }
    public string ParseFidelity { get; set; } = "";
    public bool HasFacts { get; set; }
    public int MissingCopybookCount { get; set; }

    /// <summary>Programs this one calls, directly or transitively.</summary>
    public int CallClosureCount { get; set; }

    public int CalledByCount { get; set; }

    /// <summary>True when the basename is not unique, so only the path identifies it.</summary>
    public bool AmbiguousBasename { get; set; }
}

public sealed class ProgramListSnapshot
{
    public List<ProgramListEntry> Programs { get; } = new();
    public int TotalPrograms { get; set; }
    public int TotalCopybooks { get; set; }
    public string? Note { get; set; }
}

public sealed class ProgramSnapshot
{
    public string Identity { get; set; } = "";
    public string? RelativePath { get; set; }
    public string? Basename { get; set; }
    public int LinesOfCode { get; set; }
    public bool IsCopybook { get; set; }

    // How far the parser got. A conversion decision made without this is a guess: a program with
    // deps-only output has no paragraphs behind it, however confident the rest of the view looks.
    public string? ParseFidelity { get; set; }
    public string? FidelitySource { get; set; }
    public bool HasFacts { get; set; }
    public int FactsConfidence { get; set; }
    public int FactsWarnings { get; set; }

    public List<string> Calls { get; } = new();
    public List<string> CalledBy { get; } = new();
    public List<string> Copybooks { get; } = new();
    public List<string> MissingCopybooks { get; } = new();
    public List<string> SqlTables { get; } = new();
    public List<string> CalledByJobs { get; } = new();

    /// <summary>Programs reachable from this one, so a caller can convert a whole closure.</summary>
    public List<string> CallClosure { get; } = new();

    public bool AmbiguousBasename { get; set; }
    public List<string> Candidates { get; } = new();
    public string? Note { get; set; }
}
