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
}

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
    public string Mermaid { get; set; } = "";
    public string? Note { get; set; }
}

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
