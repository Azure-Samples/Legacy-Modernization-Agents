using System.Text;
using System.Text.Json;
using System.Text.RegularExpressions;
using CobolToQuarkusMigration.Helpers;

namespace McpChatWeb.Services;

public sealed class ModernizationIntelligenceService
{
    // A step card opens a block that runs until the next one, so in-stream data stays with
    // the step that submitted it. '//' in columns 1-2 with a name that cannot start with '*'
    // keeps commented-out cards from becoming steps.
    private static readonly Regex ExecStepRegex = new(
        @"^//(?<step>[A-Z0-9$@#-]*)\s+EXEC\s+(?<operand>[^\r\n]*)",
        RegexOptions.IgnoreCase | RegexOptions.Multiline | RegexOptions.Compiled);

    // '-' is accepted beyond the JCL-legal set: real MVS member names cannot contain one,
    // but COBOL estates do, and truncating at the hyphen drops every job-to-program edge.
    private static readonly Regex PgmOperandRegex = new(
        @"^PGM\s*=\s*(?<pgm>[A-Z0-9$@#-]+)",
        RegexOptions.IgnoreCase | RegexOptions.Compiled);

    private static readonly Regex ProcOperandRegex = new(
        @"^(?:PROC\s*=\s*)?(?<proc>[A-Z0-9$@#-]+)",
        RegexOptions.IgnoreCase | RegexOptions.Compiled);

    // DB2 batch names the program in in-stream SYSTSIN; the EXEC card only ever names the
    // TSO monitor or the PROC wrapping it, so the edge is invisible without reading this.
    private static readonly Regex RunProgramRegex = new(
        @"\bRUN\s+PROGRAM\s*\(\s*(?<pgm>[A-Z0-9$@#]+)\s*\)",
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

        snapshot.ScanCacheBackedCount = programs.Count(p => p.FidelitySource == FidelitySources.ScanCache);

        // Only a measured outcome can establish Full, so without one a 0% coverage figure
        // describes the evidence rather than the estate.
        snapshot.CoverageMeasured = programs.Count > 0
            && programs.Any(p => p.FidelitySource is FidelitySources.ScanCache or FidelitySources.Facts);

        // Derived from the rows above so the headline cannot disagree with the table: the
        // report also names copybooks, which are not conversion units and not counted here.
        snapshot.ProgramsBlockedByMissing = snapshot.Programs.Count(p => p.MissingCopybookCount > 0);

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

        var matches = ResolveByIdentity(estate.Programs, normalized);

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
            // The loader also reads the older flat layout, which has no report directory,
            // so only report the gap when it turns up nothing either.
            PopulateProceduralDetail(snapshot, program.RelativePath);
            if (snapshot.ParagraphCount == 0 && snapshot.PerformEdges.Count == 0
                && snapshot.SqlStatements.Count == 0 && snapshot.CallTargets.Count == 0)
            {
                snapshot.Note = program.HasDepsOnly
                    ? $"{program.Basename} was parsed deps-only — dependency edges are known, procedural flow is not."
                    : $"No REKT report directory for {program.Basename}. Run ./doctor.sh rekt-full to generate one.";
            }
            return snapshot;
        }

        snapshot.ReportDirectory = Path.GetRelativePath(estate.RepoRoot, program.ReportDirectory);

        var flowAstDir = Path.Join(program.ReportDirectory, "flow_ast");
        snapshot.HasFlowAst = Directory.Exists(flowAstDir);
        snapshot.HasCfg = Directory.Exists(Path.Join(program.ReportDirectory, "cfg"));
        snapshot.HasDataStructures = Directory.Exists(Path.Join(program.ReportDirectory, "data_structures"));

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
            catch (Exception ex) when (ex is IOException or UnauthorizedAccessException or System.Security.SecurityException)
            {
                // Unreadable artifact directory — report what we have.
            }
        }

        PopulateProceduralDetail(snapshot, program.RelativePath);

        return snapshot;
    }

    // The AST artifacts are the whole point of this surface: presence flags alone tell a reader
    // that a program was parsed but not what the parser actually saw.
    private void PopulateProceduralDetail(FlowSnapshot snapshot, string relativePath)
    {
        RektContext context;
        try
        {
            var loader = new RektContextLoader(_estate.RepoRoot, _estate.RektDir);
            context = loader.Load(relativePath, _estate.SourceFolderName);
        }
        catch (Exception ex) when (ex is IOException or UnauthorizedAccessException or System.Security.SecurityException or JsonException)
        {
            snapshot.Note ??= "REKT artifacts for this program could not be read.";
            return;
        }

        snapshot.Sections = context.Sections
            .Select(s => new FlowSection(
                s.Name, s.StartLine, s.EndLine,
                s.Paragraphs.Select(p => new FlowParagraph(p.Name, p.StartLine, p.EndLine)).ToList()))
            .ToList();

        snapshot.ParagraphCount = snapshot.Sections.Sum(s => s.Paragraphs.Count);

        snapshot.PerformEdges = context.PerformGraph
            .Select(e => new FlowPerformEdge(e.From, e.To, e.Conditional))
            .ToList();

        snapshot.SqlStatements = context.SqlStatements
            .Select(s => new FlowSqlStatement(s.Operation, s.Tables.ToList(), s.LineNumber, s.Excerpt))
            .ToList();

        snapshot.CallTargets = context.CallTargets
            .Select(c => new FlowCallTarget(c.TargetProgram, c.IsDynamic, c.LineNumber))
            .ToList();

        if (snapshot.HasFlowAst && snapshot.ParagraphCount == 0 && snapshot.PerformEdges.Count == 0)
            snapshot.Note ??= "Flow AST artifacts exist but contain no paragraphs or PERFORM edges.";

        // The flat layout has no report directory, so HasFlowAst was false above. Harvested
        // procedural detail proves an AST was read; leaving the flag off contradicts the data.
        if (snapshot.ParagraphCount > 0 || snapshot.PerformEdges.Count > 0)
            snapshot.HasFlowAst = true;
    }

    // Tiers are tried in order and the first non-empty one wins. A flat OR would let a
    // sibling sharing the stem make an exact path look ambiguous, which no caller can resolve.
    private static List<RektProgramRecord> ResolveByIdentity(
        IReadOnlyList<RektProgramRecord> programs, string normalized)
    {
        if (string.IsNullOrWhiteSpace(normalized)) return new List<RektProgramRecord>();

        var byPath = programs
            .Where(p => p.RelativePath.Equals(normalized, StringComparison.OrdinalIgnoreCase))
            .ToList();
        if (byPath.Count > 0) return byPath;

        var byBasename = programs
            .Where(p => p.Basename.Equals(normalized, StringComparison.OrdinalIgnoreCase))
            .ToList();
        if (byBasename.Count > 0) return byBasename;

        var stem = Path.GetFileNameWithoutExtension(normalized);
        return string.IsNullOrEmpty(stem)
            ? new List<RektProgramRecord>()
            : programs.Where(p => p.Stem.Equals(stem, StringComparison.OrdinalIgnoreCase)).ToList();
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
            ApplyMermaid(snapshot);
            return snapshot;
        }

        var allJobs = new List<JclJob>();

        // A PROC step is only conventionally named after the program it runs, so that
        // inference is accepted only against source that actually exists.
        var knownProgramStems = estate.Programs
            .Where(p => !p.IsCopybook)
            .Select(p => p.Stem)
            .ToHashSet(StringComparer.OrdinalIgnoreCase);

        var unresolvedSteps = new List<UnresolvedStep>();

        foreach (var jclFile in jclFiles)
        {
            cancellationToken.ThrowIfCancellationRequested();

            string content;
            try { content = File.ReadAllText(jclFile); }
            catch (Exception ex) when (ex is IOException or UnauthorizedAccessException or System.Security.SecurityException) { continue; }

            var jobName = JobCardRegex.Match(content) is { Success: true } m
                ? m.Groups["name"].Value.ToUpperInvariant()
                : Path.GetFileNameWithoutExtension(jclFile).ToUpperInvariant();

            var steps = ExtractStepPrograms(
                content, jobName, knownProgramStems, includeUtilities, unresolvedSteps);

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

        foreach (var name in allJobs
                     .Select(j => j.JobName)
                     .Distinct(StringComparer.OrdinalIgnoreCase)
                     .OrderBy(n => n, StringComparer.OrdinalIgnoreCase))
        {
            snapshot.AllJobNames.Add(name);
        }

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

        var selectedJobNames = snapshot.Jobs
            .Select(j => j.JobName)
            .ToHashSet(StringComparer.OrdinalIgnoreCase);
        foreach (var step in unresolvedSteps.Where(s => selectedJobNames.Contains(s.JobName)))
            snapshot.UnresolvedSteps.Add(step);

        // A standalone verdict is only sound once every step is accounted for; while steps
        // remain unresolved, absence of a caller is missing evidence rather than a finding.
        if (snapshot.UnresolvedSteps.Count > 0)
        {
            var warning = $"{snapshot.UnresolvedSteps.Count} step(s) invoke a PROC that is not in "
                + "source, so programs they run cannot be attributed and may appear standalone.";
            // Appended, not coalesced: the estate note is usually already set, which would
            // otherwise discard the one caveat that qualifies the standalone column.
            snapshot.Note = string.IsNullOrWhiteSpace(snapshot.Note)
                ? warning
                : $"{snapshot.Note} {warning}";
        }

        ApplyMermaid(snapshot);
        return snapshot;
    }

    // Three sources of evidence, strongest first: the EXEC card, in-stream SYSTSIN, then the
    // step name. Anything still unattributed is recorded rather than dropped.
    private static List<string> ExtractStepPrograms(
        string content,
        string jobName,
        IReadOnlySet<string> knownProgramStems,
        bool includeUtilities,
        List<UnresolvedStep> unresolved)
    {
        var programs = new List<string>();

        void Accept(string name)
        {
            if (name.Length == 0) return;
            if (!includeUtilities && SystemUtilities.Contains(name)) return;
            if (!programs.Contains(name, StringComparer.OrdinalIgnoreCase)) programs.Add(name);
        }

        foreach (var (step, operand, body) in EnumerateSteps(content))
        {
            // Read before the card is classified: IKJEFT01 and the PROCs wrapping it are the
            // TSO monitor, so filtering the step as a utility discards the real workload.
            var inStream = RunProgramRegex.Matches(body)
                .Select(match => match.Groups["pgm"].Value.ToUpperInvariant())
                .ToList();
            foreach (var program in inStream) Accept(program);

            var pgmOperand = PgmOperandRegex.Match(operand);
            if (pgmOperand.Success)
            {
                Accept(pgmOperand.Groups["pgm"].Value.ToUpperInvariant());
                continue;
            }

            if (inStream.Count > 0) continue;
            if (knownProgramStems.Contains(step)) { Accept(step); continue; }

            var proc = ProcOperandRegex.Match(operand);
            unresolved.Add(new UnresolvedStep(
                JobName: jobName,
                StepName: step,
                ProcName: proc.Success ? proc.Groups["proc"].Value.ToUpperInvariant() : operand));
        }

        return programs;
    }

    private static IEnumerable<(string Step, string Operand, string Body)> EnumerateSteps(string content)
    {
        var matches = ExecStepRegex.Matches(content);
        for (var i = 0; i < matches.Count; i++)
        {
            var match = matches[i];
            var bodyStart = match.Index + match.Length;
            var bodyEnd = i + 1 < matches.Count ? matches[i + 1].Index : content.Length;
            yield return (
                match.Groups["step"].Value.ToUpperInvariant(),
                match.Groups["operand"].Value.Trim(),
                content[bodyStart..bodyEnd]);
        }
    }

    private static List<string> EnumerateJclFiles(string sourceRoot)
    {
        if (!Directory.Exists(sourceRoot)) return new List<string>();
        try
        {
            return Directory.EnumerateFiles(sourceRoot, "*", SearchOption.AllDirectories)
                .Where(path => Path.GetExtension(path).Equals(".jcl", StringComparison.OrdinalIgnoreCase))
                .Where(path => !SourceTypeRegistry.IsScratchPath(Path.GetRelativePath(sourceRoot, path)))
                // Case-insensitive filesystems return the same file for the
                // *.JCL and *.jcl patterns REKT tooling uses.
                .Distinct(StringComparer.OrdinalIgnoreCase)
                .OrderBy(path => path, StringComparer.OrdinalIgnoreCase)
                .ToList();
        }
        catch (Exception ex) when (ex is IOException or UnauthorizedAccessException or System.Security.SecurityException) { return new List<string>(); }
    }

    // Capped at MaxMermaidEdges because the client-side renderer becomes unusable well
    // before a full estate is drawn; the JSON payload still carries every edge.
    private const int MaxMermaidEdges = 200;

    private static void ApplyMermaid(ServiceChainSnapshot snapshot)
    {
        var diagram = BuildServiceChainMermaid(snapshot);
        snapshot.Mermaid = diagram.Text;
        snapshot.MermaidEdgeCount = diagram.EdgeCount;
        snapshot.MermaidTruncated = diagram.Truncated;
    }

    // Filtering happens upstream so the diagram cannot disagree with the JSON beside it.
    private static MermaidDiagram BuildServiceChainMermaid(ServiceChainSnapshot snapshot)
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
        // Set only where a break leaves an unrendered item, so a diagram that ends exactly
        // on the cap is not reported as truncated.
        var truncated = false;

        foreach (var job in snapshot.Jobs)
        {
            if (edges >= MaxMermaidEdges) { truncated = true; break; }

            var jobId = Sanitize($"j_{job.JobName}");
            if (renderedJobs.Add(job.JobName))
                sb.AppendLine($"  {jobId}[\"{Escape(job.JobName)}\"]:::jobNode");

            foreach (var pgm in job.PrimaryPrograms)
            {
                if (edges >= MaxMermaidEdges) { truncated = true; break; }

                var pgmId = Sanitize($"p_{pgm}");
                if (renderedPrograms.Add(pgm))
                    sb.AppendLine($"  {pgmId}[\"{Escape(pgm)}\"]:::pgmNode");
                sb.AppendLine($"  {jobId} --> {pgmId}");
                edges++;

                if (!programByStem.TryGetValue(pgm, out var chain)) continue;
                foreach (var copybook in chain.Copybooks)
                {
                    if (edges >= MaxMermaidEdges) { truncated = true; break; }
                    AppendCopybook(sb, renderedCopybooks, pgmId, copybook);
                    edges++;
                }
            }
        }

        // A program no job runs is itself a finding, so render it standalone
        // rather than dropping it.
        foreach (var program in snapshot.Programs)
        {
            if (edges >= MaxMermaidEdges) { truncated = true; break; }
            if (renderedPrograms.Contains(program.Stem)) continue;

            var pgmId = Sanitize($"p_{program.Stem}");
            renderedPrograms.Add(program.Stem);
            sb.AppendLine($"  {pgmId}[\"{Escape(program.Stem)}\"]:::pgmNode");

            foreach (var copybook in program.Copybooks)
            {
                if (edges >= MaxMermaidEdges) { truncated = true; break; }
                AppendCopybook(sb, renderedCopybooks, pgmId, copybook);
                edges++;
            }
        }

        return new MermaidDiagram(sb.ToString(), edges, truncated);
    }

    private static void AppendCopybook(
        StringBuilder sb, HashSet<string> rendered, string programId, string copybook)
    {
        var copybookId = Sanitize($"c_{copybook}");
        if (rendered.Add(copybook))
            sb.AppendLine($"  {copybookId}([\"{Escape(copybook)}\"]):::cpyNode");
        sb.AppendLine($"  {programId} -.-> {copybookId}");
    }

    private readonly record struct MermaidDiagram(string Text, int EdgeCount, bool Truncated);

    // Two names differing only in punctuation would otherwise collapse to the same node id
    // and silently merge into one box, so a short digest of the original disambiguates them.
    private static string Sanitize(string value)
    {
        var mapped = new string(value.Select(c => char.IsLetterOrDigit(c) || c == '_' ? c : '_').ToArray());
        return $"{mapped}_{StableDigest(value)}";
    }

    // FNV-1a: stable across processes, unlike string.GetHashCode.
    private static string StableDigest(string value)
    {
        const uint offset = 2166136261;
        const uint prime = 16777619;
        var hash = offset;
        foreach (var b in System.Text.Encoding.UTF8.GetBytes(value))
        {
            hash ^= b;
            hash *= prime;
        }
        return hash.ToString("x8");
    }

    private static string Escape(string value) =>
        (value ?? "").Replace("\"", "'").Replace('\n', ' ').Replace('\r', ' ');
}
