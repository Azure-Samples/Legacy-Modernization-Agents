using System.Text;
using System.Text.RegularExpressions;
using CobolToQuarkusMigration.Helpers;

namespace McpChatWeb.Services;

public sealed class ModernizationIntelligenceService
{
    // '-' is accepted beyond the JCL-legal set: real MVS member names cannot contain one,
    // but COBOL estates do, and truncating at the hyphen drops every job-to-program edge.
    private static readonly Regex ExecPgmRegex = new(
        @"EXEC\s+PGM\s*=\s*([A-Z0-9$@#-]+)",
        RegexOptions.IgnoreCase | RegexOptions.Compiled);

    private static readonly Regex JobCardRegex = new(
        @"^//(?<name>[A-Z0-9$@#-]+)\s+JOB",
        RegexOptions.IgnoreCase | RegexOptions.Multiline | RegexOptions.Compiled);

    // MVS system utilities appear in nearly every JCL chain and would swamp the graph;
    // none of them is a program to be converted.
    private static readonly HashSet<string> SystemUtilities = new(StringComparer.OrdinalIgnoreCase)
    {
        "IDCAMS", "IKJEFT01", "IEFBR14", "SORT", "ICETOOL", "DFSORT", "ADUUMAIN",
        "SYSUTCOM", "DSNUTILB", "DSNTIAUL", "IEBGENER", "IEBCOPY", "IEHPROGM",
        "IRXJCL", "EZACFSM1",
    };

    private readonly RektEstateReader _estate;

    public ModernizationIntelligenceService(RektEstateReader estate) => _estate = estate;

    // ── Dependency health ────────────────────────────────────────────────

    public async Task<DependencyHealthSnapshot> GetDependencyHealthAsync(
        CancellationToken cancellationToken = default)
    {
        var estate = await _estate.ReadAsync(cancellationToken).ConfigureAwait(false);
        var snapshot = new DependencyHealthSnapshot { Note = estate.Note };

        foreach (var row in estate.MissingCopybooks) snapshot.MissingCopybooks.Add(row);

        var missingByProgram = new Dictionary<string, HashSet<string>>(StringComparer.OrdinalIgnoreCase);
        foreach (var row in estate.MissingCopybooks)
        {
            foreach (var program in row.ReferencedBy)
            {
                if (!missingByProgram.TryGetValue(program, out var set))
                    missingByProgram[program] = set = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
                set.Add(row.Copybook);
            }
        }

        var programs = estate.Programs.Where(p => !p.IsCopybook).ToList();

        foreach (var program in programs)
        {
            var missing = CountMissingFor(program, missingByProgram);
            snapshot.Programs.Add(new ProgramHealthRow(
                Basename: program.Basename,
                RelativePath: program.RelativePath,
                LinesOfCode: program.LinesOfCode,
                ParseFidelity: program.ParseFidelity,
                FidelitySource: program.FidelitySource,
                ScanOutcome: program.ScanOutcome,
                FactsConfidence: program.FactsConfidence,
                FactsWarnings: program.FactsWarnings,
                MissingCopybookCount: missing,
                HasReport: program.HasReport,
                HasDepsOnly: program.HasDepsOnly,
                AmbiguousBasename: program.AmbiguousBasename));
        }

        snapshot.TotalPrograms = programs.Count;
        snapshot.FullFidelityCount = programs.Count(p => p.ParseFidelity == ParseFidelity.Full);
        snapshot.PartialFidelityCount = programs.Count(p => p.ParseFidelity == ParseFidelity.Partial);
        snapshot.DepsOnlyCount = programs.Count(p => p.ParseFidelity == ParseFidelity.DepsOnly);
        snapshot.FailedCount = programs.Count(p => p.ParseFidelity == ParseFidelity.Failed);
        snapshot.NotParsedCount = programs.Count(p => p.ParseFidelity == ParseFidelity.NotParsed);
        snapshot.TotalMissingCopybooks = estate.MissingCopybooks.Count;

        snapshot.ProgramsBlockedByMissing = estate.MissingCopybooks
            .SelectMany(row => row.ReferencedBy)
            .Distinct(StringComparer.OrdinalIgnoreCase)
            .Count();

        snapshot.ScanCacheBackedCount = programs.Count(p => p.FidelitySource == FidelitySources.ScanCache);

        if (programs.Count > 0)
        {
            snapshot.CoveragePct = Math.Round(
                snapshot.FullFidelityCount * 100.0 / programs.Count, 1);

            // Weighted: only a full parse gives the converter everything. Partial is lossy;
            // deps-only gives the edges and nothing about the logic.
            var weighted =
                snapshot.FullFidelityCount * 1.0
                + snapshot.PartialFidelityCount * 0.5
                + snapshot.DepsOnlyCount * 0.25;
            snapshot.ReadinessScore = Math.Round(weighted * 100.0 / programs.Count, 1);
        }

        return snapshot;
    }

    // The preprocessor records references by whatever name it saw, so match both
    // basename and stem.
    private static int CountMissingFor(
        RektProgramRecord program,
        IReadOnlyDictionary<string, HashSet<string>> missingByProgram)
    {
        if (missingByProgram.TryGetValue(program.Basename, out var byBasename)) return byBasename.Count;
        if (missingByProgram.TryGetValue(program.Stem, out var byStem)) return byStem.Count;
        return 0;
    }

    // ── Topology ─────────────────────────────────────────────────────────

    public async Task<TopologySnapshot> GetTopologyAsync(CancellationToken cancellationToken = default)
    {
        var estate = await _estate.ReadAsync(cancellationToken).ConfigureAwait(false);
        var snapshot = new TopologySnapshot { Note = estate.Note };

        foreach (var program in estate.Programs)
        {
            snapshot.Nodes.Add(new TopologyNode(
                Id: program.RelativePath,
                Basename: program.Basename,
                Kind: program.IsCopybook ? "copybook" : "program",
                LinesOfCode: program.LinesOfCode,
                HasFacts: program.HasFacts,
                FactsConfidence: program.FactsConfidence,
                ParseFidelity: program.ParseFidelity,
                FidelitySource: program.FidelitySource,
                AmbiguousBasename: program.AmbiguousBasename));
        }

        // Keyed by basename/stem because that is all a CALL or COPY gives us. Resolve only
        // when unambiguous — a guess would draw an edge that does not exist.
        var nodesByKey = BuildNodeIndex(estate.Programs);

        foreach (var program in estate.Programs)
        {
            foreach (var callee in program.Callees)
            {
                if (TryResolve(nodesByKey, callee, out var target))
                    snapshot.Edges.Add(new TopologyEdge(program.RelativePath, target, "call"));
                else
                    snapshot.UnresolvedEdges.Add(new TopologyEdge(program.RelativePath, callee, "call"));
            }

            foreach (var copybook in program.Copybooks)
            {
                if (TryResolve(nodesByKey, copybook, out var target))
                    snapshot.Edges.Add(new TopologyEdge(program.RelativePath, target, "copy"));
                else
                    snapshot.UnresolvedEdges.Add(new TopologyEdge(program.RelativePath, copybook, "copy"));
            }
        }

        return snapshot;
    }

    private static Dictionary<string, string> BuildNodeIndex(IReadOnlyList<RektProgramRecord> programs)
    {
        var candidates = new Dictionary<string, List<string>>(StringComparer.OrdinalIgnoreCase);

        void Add(string key, string relativePath)
        {
            if (string.IsNullOrWhiteSpace(key)) return;
            if (!candidates.TryGetValue(key, out var list))
                candidates[key] = list = new List<string>();
            if (!list.Contains(relativePath, StringComparer.OrdinalIgnoreCase))
                list.Add(relativePath);
        }

        foreach (var program in programs)
        {
            Add(program.Basename, program.RelativePath);
            Add(program.Stem, program.RelativePath);
        }

        return candidates
            .Where(kv => kv.Value.Count == 1)
            .ToDictionary(kv => kv.Key, kv => kv.Value[0], StringComparer.OrdinalIgnoreCase);
    }

    private static bool TryResolve(
        IReadOnlyDictionary<string, string> index, string name, out string relativePath)
    {
        relativePath = "";
        if (string.IsNullOrWhiteSpace(name)) return false;
        if (index.TryGetValue(name, out var direct)) { relativePath = direct; return true; }

        var stem = Path.GetFileNameWithoutExtension(name);
        if (!string.IsNullOrEmpty(stem) && index.TryGetValue(stem, out var byStem))
        {
            relativePath = byStem;
            return true;
        }
        return false;
    }

    // ── Semantic flow ────────────────────────────────────────────────────

    // Absence is the useful signal: no flow AST means the converter has no
    // control-flow model to work from.
    public async Task<FlowSnapshot> GetProgramFlowAsync(
        string identity, CancellationToken cancellationToken = default)
    {
        var estate = await _estate.ReadAsync(cancellationToken).ConfigureAwait(false);
        var normalized = SourcePathHelper.NormalizeRelativePath(identity ?? "");

        // Accept a source-relative path, a basename or a stem, in that order of
        // specificity, so the UI can link with whatever identity it holds.
        var matches = estate.Programs
            .Where(p =>
                p.RelativePath.Equals(normalized, StringComparison.OrdinalIgnoreCase)
                || p.Basename.Equals(normalized, StringComparison.OrdinalIgnoreCase)
                || p.Stem.Equals(Path.GetFileNameWithoutExtension(normalized), StringComparison.OrdinalIgnoreCase))
            .ToList();

        if (matches.Count == 0)
            return new FlowSnapshot { Identity = identity ?? "", Note = $"No source file matches '{identity}'." };

        if (matches.Count > 1)
        {
            return new FlowSnapshot
            {
                Identity = identity ?? "",
                Candidates = matches.Select(m => m.RelativePath).ToList(),
                Note = $"'{identity}' matches {matches.Count} source files. "
                       + "Request a source-relative path to disambiguate.",
            };
        }

        var program = matches[0];
        var snapshot = new FlowSnapshot
        {
            Identity = identity ?? "",
            Basename = program.Basename,
            RelativePath = program.RelativePath,
            ParseFidelity = program.ParseFidelity,
            FidelitySource = program.FidelitySource,
        };

        if (program.ReportDirectory is null || !Directory.Exists(program.ReportDirectory))
        {
            snapshot.Note = program.HasDepsOnly
                ? $"{program.Basename} was parsed deps-only — dependency edges are known, procedural flow is not."
                : $"No REKT report directory for {program.Basename}. Run ./doctor.sh rekt-full to generate one.";
            return snapshot;
        }

        snapshot.ReportDirectory = Path.GetRelativePath(estate.RepoRoot, program.ReportDirectory);

        var flowAstDir = Path.Combine(program.ReportDirectory, "flow_ast");
        snapshot.HasFlowAst = Directory.Exists(flowAstDir);
        snapshot.HasCfg = Directory.Exists(Path.Combine(program.ReportDirectory, "cfg"));
        snapshot.HasDataStructures = Directory.Exists(Path.Combine(program.ReportDirectory, "data_structures"));

        if (snapshot.HasFlowAst)
        {
            try
            {
                var files = Directory.GetFiles(flowAstDir, "*.json");
                snapshot.FlowAstFiles = files.Length;
                snapshot.FlowAstNames = files
                    .Select(Path.GetFileNameWithoutExtension)
                    .Where(n => !string.IsNullOrEmpty(n))
                    .Select(n => n!)
                    .OrderBy(n => n, StringComparer.OrdinalIgnoreCase)
                    .ToList();
            }
            catch { /* unreadable artifact directory — report what we have */ }
        }

        return snapshot;
    }

    // ── Service chain ────────────────────────────────────────────────────

    public async Task<ServiceChainSnapshot> GetServiceChainAsync(
        string? jobFilter = null,
        string? programFilter = null,
        bool includeUtilities = false,
        CancellationToken cancellationToken = default)
    {
        var estate = await _estate.ReadAsync(cancellationToken).ConfigureAwait(false);
        var snapshot = new ServiceChainSnapshot { Note = estate.Note };

        var jclFiles = EnumerateJclFiles(estate.SourceRoot);
        if (jclFiles.Count == 0)
        {
            snapshot.Note ??= $"No JCL files found under {estate.SourceRoot}.";
            snapshot.Mermaid = BuildServiceChainMermaid(snapshot);
            return snapshot;
        }

        var allJobs = new List<JclJob>();

        foreach (var jclFile in jclFiles)
        {
            cancellationToken.ThrowIfCancellationRequested();

            string content;
            try { content = File.ReadAllText(jclFile); }
            catch { continue; }

            var jobName = JobCardRegex.Match(content) is { Success: true } m
                ? m.Groups["name"].Value.ToUpperInvariant()
                : Path.GetFileNameWithoutExtension(jclFile).ToUpperInvariant();

            var steps = ExecPgmRegex.Matches(content)
                .Select(match => match.Groups[1].Value.ToUpperInvariant())
                .Where(pgm => includeUtilities || !SystemUtilities.Contains(pgm))
                .Distinct(StringComparer.OrdinalIgnoreCase)
                .ToList();

            allJobs.Add(new JclJob(
                JobName: jobName,
                JclFileName: Path.GetFileName(jclFile),
                RelativePath: SourcePathHelper.NormalizeRelativePath(
                    Path.GetRelativePath(estate.RepoRoot, jclFile)),
                PrimaryPrograms: steps));
        }

        // Filters narrow the payload, not just the diagram: a caller asking for
        // one job's chain must not receive the whole estate's counts.
        var programStem = string.IsNullOrWhiteSpace(programFilter)
            ? null
            : Path.GetFileNameWithoutExtension(programFilter).ToUpperInvariant();

        IEnumerable<JclJob> selectedJobs = allJobs;
        if (!string.IsNullOrWhiteSpace(jobFilter))
            selectedJobs = selectedJobs.Where(
                j => j.JobName.Equals(jobFilter, StringComparison.OrdinalIgnoreCase));
        if (programStem is not null)
            selectedJobs = selectedJobs.Where(
                j => j.PrimaryPrograms.Contains(programStem, StringComparer.OrdinalIgnoreCase));

        foreach (var job in selectedJobs) snapshot.Jobs.Add(job);

        var jobsByProgram = new Dictionary<string, List<string>>(StringComparer.OrdinalIgnoreCase);
        foreach (var job in snapshot.Jobs)
        {
            foreach (var pgm in job.PrimaryPrograms)
            {
                if (!jobsByProgram.TryGetValue(pgm, out var list))
                    jobsByProgram[pgm] = list = new List<string>();
                if (!list.Contains(job.JobName, StringComparer.OrdinalIgnoreCase))
                    list.Add(job.JobName);
            }
        }

        // The same basename staged under several roots is still one program in the chain.
        var seenStems = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        foreach (var program in estate.Programs.Where(p => !p.IsCopybook))
        {
            if (!seenStems.Add(program.Stem)) continue;

            var calledBy = jobsByProgram.GetValueOrDefault(program.Stem)
                           ?? jobsByProgram.GetValueOrDefault(program.Basename)
                           ?? new List<string>();

            if (programStem is not null
                && !program.Stem.Equals(programStem, StringComparison.OrdinalIgnoreCase))
            {
                continue;
            }

            // Applied even when a program filter is present, so asking for a program inside
            // a job that never runs it returns nothing rather than implying the job runs it.
            if (!string.IsNullOrWhiteSpace(jobFilter) && calledBy.Count == 0) continue;

            snapshot.Programs.Add(new ProgramChain(
                Basename: program.Basename,
                Stem: program.Stem,
                RelativePath: program.RelativePath,
                LinesOfCode: program.LinesOfCode,
                ParseFidelity: program.ParseFidelity,
                Copybooks: program.Copybooks.ToList(),
                CalledByJobs: calledBy));
        }

        snapshot.TotalJobs = snapshot.Jobs.Count;
        snapshot.TotalPrograms = snapshot.Programs.Count;
        snapshot.TotalCopybooks = snapshot.Programs
            .SelectMany(p => p.Copybooks)
            .Distinct(StringComparer.OrdinalIgnoreCase)
            .Count();
        snapshot.JobToProgramEdges = snapshot.Jobs.Sum(j => j.PrimaryPrograms.Count);
        snapshot.ProgramToCopybookEdges = snapshot.Programs.Sum(p => p.Copybooks.Count);

        snapshot.Mermaid = BuildServiceChainMermaid(snapshot);
        return snapshot;
    }

    private static List<string> EnumerateJclFiles(string sourceRoot)
    {
        if (!Directory.Exists(sourceRoot)) return new List<string>();
        try
        {
            return Directory.EnumerateFiles(sourceRoot, "*", SearchOption.AllDirectories)
                .Where(path => Path.GetExtension(path).Equals(".jcl", StringComparison.OrdinalIgnoreCase))
                .Where(path => !IsScratchPath(sourceRoot, path))
                // Case-insensitive filesystems return the same file for the
                // *.JCL and *.jcl patterns REKT tooling uses.
                .Distinct(StringComparer.OrdinalIgnoreCase)
                .OrderBy(path => path, StringComparer.OrdinalIgnoreCase)
                .ToList();
        }
        catch { return new List<string>(); }
    }

    private static bool IsScratchPath(string root, string path)
    {
        var relative = SourcePathHelper.NormalizeRelativePath(Path.GetRelativePath(root, path));
        return relative.Split('/', StringSplitOptions.RemoveEmptyEntries).Any(segment =>
            segment.StartsWith(".convert-", StringComparison.Ordinal)
            || segment.Equals(".rekt-staging", StringComparison.Ordinal)
            || segment.Equals(".preprocessed", StringComparison.Ordinal));
    }

    // Capped at MaxMermaidEdges because the client-side renderer becomes unusable well
    // before a full estate is drawn; the JSON payload still carries every edge.
    private const int MaxMermaidEdges = 200;

    // Filtering happens upstream so the diagram cannot disagree with the JSON beside it.
    private static string BuildServiceChainMermaid(ServiceChainSnapshot snapshot)
    {
        var sb = new StringBuilder();
        sb.AppendLine("flowchart LR");
        sb.AppendLine("  classDef jobNode fill:#7c2d12,stroke:#fb923c,color:#fef3c7,rx:6,ry:6");
        sb.AppendLine("  classDef pgmNode fill:#1e3a5f,stroke:#60a5fa,color:#e2e8f0,rx:4,ry:4");
        sb.AppendLine("  classDef cpyNode fill:#14532d,stroke:#10b981,color:#e2e8f0");

        var programByStem = snapshot.Programs
            .GroupBy(p => p.Stem, StringComparer.OrdinalIgnoreCase)
            .ToDictionary(g => g.Key, g => g.First(), StringComparer.OrdinalIgnoreCase);

        var renderedJobs = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        var renderedPrograms = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        var renderedCopybooks = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        var edges = 0;

        foreach (var job in snapshot.Jobs)
        {
            if (edges >= MaxMermaidEdges) break;

            var jobId = Sanitize($"j_{job.JobName}");
            if (renderedJobs.Add(job.JobName))
                sb.AppendLine($"  {jobId}[\"{Escape(job.JobName)}\"]:::jobNode");

            foreach (var pgm in job.PrimaryPrograms)
            {
                if (edges >= MaxMermaidEdges) break;

                var pgmId = Sanitize($"p_{pgm}");
                if (renderedPrograms.Add(pgm))
                    sb.AppendLine($"  {pgmId}[\"{Escape(pgm)}\"]:::pgmNode");
                sb.AppendLine($"  {jobId} --> {pgmId}");
                edges++;

                if (!programByStem.TryGetValue(pgm, out var chain)) continue;
                foreach (var copybook in chain.Copybooks)
                {
                    if (edges >= MaxMermaidEdges) break;
                    edges += AppendCopybook(sb, renderedCopybooks, pgmId, copybook);
                }
            }
        }

        // A program no job runs is itself a finding, so render it standalone
        // rather than dropping it.
        foreach (var program in snapshot.Programs)
        {
            if (edges >= MaxMermaidEdges) break;
            if (renderedPrograms.Contains(program.Stem)) continue;

            var pgmId = Sanitize($"p_{program.Stem}");
            renderedPrograms.Add(program.Stem);
            sb.AppendLine($"  {pgmId}[\"{Escape(program.Stem)}\"]:::pgmNode");

            foreach (var copybook in program.Copybooks)
            {
                if (edges >= MaxMermaidEdges) break;
                edges += AppendCopybook(sb, renderedCopybooks, pgmId, copybook);
            }
        }

        snapshot.MermaidEdgeCount = edges;
        snapshot.MermaidTruncated = edges >= MaxMermaidEdges;
        return sb.ToString();
    }

    private static int AppendCopybook(
        StringBuilder sb, HashSet<string> rendered, string programId, string copybook)
    {
        var copybookId = Sanitize($"c_{copybook}");
        if (rendered.Add(copybook))
            sb.AppendLine($"  {copybookId}([\"{Escape(copybook)}\"]):::cpyNode");
        sb.AppendLine($"  {programId} -.-> {copybookId}");
        return 1;
    }

    private static string Sanitize(string value) =>
        new(value.Select(c => char.IsLetterOrDigit(c) || c == '_' ? c : '_').ToArray());

    private static string Escape(string value) =>
        (value ?? "").Replace("\"", "'").Replace('\n', ' ').Replace('\r', ' ');
}

// ── Contracts ────────────────────────────────────────────────────────────

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
