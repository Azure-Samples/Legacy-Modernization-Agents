// Post-conversion parity check. Runs after files are written so it can see the diagnostic
// stubs ConversionOutputGuard produces at write time.

using System.Text;
using System.Text.Json;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using Microsoft.Extensions.Logging;

namespace CobolToQuarkusMigration.Agents;

public static class ConversionParityPostPass
{
    public const string ArtifactName = "conversion-parity.json";
    public const int LowScoreExitCode = 4;

    private const double DefaultThreshold = 0.75;

    public static async Task<string> RunAsync(
        IEnumerable<CodeFile> generatedFiles,
        string outputFolder,
        string targetLanguage,
        ILogger? logger = null,
        IEnumerable<string>? sourcePrograms = null,
        string? repositoryRoot = null,
        Func<ParityRepairRequest, Task<IReadOnlyDictionary<string, string>?>>? repair = null,
        string? sourceDirectory = null,
        bool applyGate = true,
        bool preserveRepairHistory = false,
        CancellationToken cancellationToken = default)
    {
        var repoRoot = repositoryRoot ?? FindRepoRoot();
        if (repoRoot is null)
        {
            logger?.LogWarning("[ConversionParity] Repo root (doctor.sh) not found; parity check skipped.");
            return string.Empty;
        }
        var sourceFolder = sourceDirectory ?? Environment.GetEnvironmentVariable("COBOL_SOURCE_FOLDER") ?? "source";
        var sourceRoot = Path.GetFullPath(Path.Combine(repoRoot, sourceFolder));
        string Identity(string path) => SourcePathHelper.RelativeToSource(path, sourceRoot);
        string FileIdentity(CodeFile file) => Identity(
            string.IsNullOrWhiteSpace(file.OriginalSourcePath) ? file.OriginalCobolFileName : file.OriginalSourcePath);
        var files = generatedFiles
            .Where(f => !string.IsNullOrWhiteSpace(f.FilePath))
            .ToList();
        Directory.CreateDirectory(outputFolder);
        await File.WriteAllTextAsync(Path.Combine(outputFolder, "generated-source-map.json"),
            JsonSerializer.Serialize(files.Select(file => new
            {
                source = FileIdentity(file),
                generated = Path.GetRelativePath(outputFolder, file.FilePath).Replace('\\', '/'),
                kind = Path.GetExtension(FileIdentity(file)).Equals(".cpy", StringComparison.OrdinalIgnoreCase)
                    ? "copybook" : "program"
            }), new JsonSerializerOptions { WriteIndented = true }), cancellationToken);

        var namedPrograms = (sourcePrograms ?? Enumerable.Empty<string>())
            .Where(p => !string.IsNullOrWhiteSpace(p))
            .Select(Identity)
            .ToList();

        // Legacy outputs without path metadata must not silently merge ambiguous basenames.
        var ambiguousPrograms = namedPrograms
            .GroupBy(p => p, StringComparer.OrdinalIgnoreCase)
            .Where(g => g.Count() > 1)
            .ToDictionary(g => g.Key, g => g.Count(), StringComparer.OrdinalIgnoreCase);

        var expectedPrograms = namedPrograms
            .Distinct(StringComparer.OrdinalIgnoreCase)
            .ToList();

        if (files.Count == 0 && expectedPrograms.Count == 0) return string.Empty;

        var threshold = ReadThreshold(logger);
        var gate = ReadGate(logger);
        var factsDir = ConversionEligibility.FactsDirectory(repoRoot);

        var provider = new StructuralContextProvider(repoRoot, sourceFolder, fallbackToAi: false);
        var stubCopybooks = StubCopybookCatalog.Load(repoRoot, sourceFolder);
        if (stubCopybooks.Copybooks.Count > 0)
        {
            logger?.LogWarning(
                "[ConversionParity] {Count} stub copybook(s) in use ({Names}); field-level parity is " +
                "measured against incomplete evidence.",
                stubCopybooks.Copybooks.Count, string.Join(", ", stubCopybooks.Copybooks));
        }
        var results = new List<ProgramParityResult>();

        var unmapped = files.Where(f => string.IsNullOrWhiteSpace(FileIdentity(f))).ToList();
        var conflictingOutputs = files.GroupBy(file => Path.GetFullPath(file.FilePath))
            .Where(group => group.Select(FileIdentity).Distinct(StringComparer.OrdinalIgnoreCase).Count() > 1)
            .Select(group => group.Key).ToHashSet(StringComparer.Ordinal);
        foreach (var file in unmapped)
        {
            results.Add(NotEvaluated(
                Path.GetFileName(file.FilePath!),
                file.FilePath,
                "Generated file could not be mapped back to a COBOL source program."));
        }

        // Chunked assembly emits one file per generated class, all carrying the same source
        // program. Scoring them individually would report a correct split as several failures.
        var byProgram = files
            .Where(f => !string.IsNullOrWhiteSpace(FileIdentity(f)))
            .GroupBy(FileIdentity, StringComparer.OrdinalIgnoreCase);

        var converted = new HashSet<string>(StringComparer.OrdinalIgnoreCase);

        foreach (var group in byProgram)
        {
            var program = group.Key;
            converted.Add(program);
            cancellationToken.ThrowIfCancellationRequested();
            if (group.Any(file => conflictingOutputs.Contains(Path.GetFullPath(file.FilePath))))
            {
                var reason = "The same generated output path is assigned to multiple source identities; " +
                    "the overwritten output cannot be attributed or repaired safely.";
                logger?.LogError("Parity not evaluated for {Program}: {Reason}", program, reason);
                results.Add(NotEvaluated(program, DescribeFiles(group), reason));
                continue;
            }

            if (ambiguousPrograms.TryGetValue(program, out var shareCount))
            {
                results.Add(NotEvaluated(
                    program, DescribeFiles(group),
                    $"{shareCount} source programs share the file name '{program}'. Generated files " +
                    "record only the source file name, so their output cannot be told apart and " +
                    "parity would be measured against the wrong program."));
                continue;
            }

            var parts = new List<string>();
            var readFailures = new List<string>();
            foreach (var file in group)
            {
                try
                {
                    parts.Add(await File.ReadAllTextAsync(file.FilePath!));
                }
                catch (Exception ex)
                {
                    logger?.LogDebug(ex, "[ConversionParity] Could not read {File}", file.FilePath);
                    readFailures.Add($"{Path.GetFileName(file.FilePath)}: {ex.Message}");
                }
            }

            var generatedFile = DescribeFiles(group);
            if (parts.Count == 0)
            {
                results.Add(NotEvaluated(
                    program, generatedFile,
                    $"No generated file could be read ({string.Join("; ", readFailures)})."));
                continue;
            }

            StructuralContext? context = null;
            try
            {
                context = await provider.GetAsync(program);
            }
            catch (Exception ex)
            {
                logger?.LogDebug(ex, "[ConversionParity] No structural context for {Program}", program);
            }

            var facts = ProgramFactsArtifactLocator.TryLoad(factsDir, program);
            var code = string.Join("\n", parts);
            var result = ConversionParityValidator.Evaluate(
                program, generatedFile, code, context, facts, stubCopybooks);
            if (repair is not null && readFailures.Count == 0 && Fails(result, threshold))
            {
                var sourcePath = Path.GetFullPath(Path.Combine(sourceRoot, SourcePathHelper.ToOsRelativePath(program)));
                if (File.Exists(sourcePath))
                {
                    var source = await File.ReadAllTextAsync(sourcePath, cancellationToken);
                    var current = group.ToDictionary(f => f.FilePath, f => File.ReadAllText(f.FilePath));
                    result = await RepairAsync(result, source, current, targetLanguage, threshold,
                        repair, candidate => ConversionParityValidator.Evaluate(
                            program, generatedFile, string.Join("\n", candidate.Values), context, facts, stubCopybooks),
                        logger, cancellationToken);
                    foreach (var file in group) file.Content = await File.ReadAllTextAsync(file.FilePath, cancellationToken);
                }
                else logger?.LogWarning("Parity repair skipped: authoritative source not found for {Program}", program);
            }
            results.Add(result);
        }

        // A source that produced no output at all is the worst parity failure there is, and it
        // is invisible if the report only walks generated files.
        foreach (var program in expectedPrograms.Where(p => !converted.Contains(p)))
        {
            results.Add(NoOutput(program, ambiguousPrograms.TryGetValue(program, out var n) ? n : 1));
        }

        if (preserveRepairHistory)
        {
            var previous = JsonSerializer.Deserialize<ConversionParityReport>(
                await File.ReadAllTextAsync(Path.Combine(outputFolder, ArtifactName), cancellationToken))
                ?? throw new InvalidOperationException("Cannot revalidate a missing parity report.");
            var history = previous.Programs.ToDictionary(program => program.Program, StringComparer.OrdinalIgnoreCase);
            results = results.Select(result => history.TryGetValue(result.Program, out var old)
                ? result with { RepairAttempts = old.RepairAttempts, AcceptedRepairs = old.AcceptedRepairs, RepairNote = old.RepairNote }
                : result).ToList();
        }
        var report = BuildReport(results, targetLanguage, threshold, gate);
        await WriteArtifactAsync(report, outputFolder, logger);

        if (applyGate && gate == ParityGate.Stop && report.BelowThresholdCount > 0)
        {
            if (Environment.ExitCode == 0) Environment.ExitCode = LowScoreExitCode;
            logger?.LogError(
                "[ConversionParity] {Count} program(s) failed parity (MIN_PROGRAM_SCORE={Threshold}); ON_LOW_SCORE=stop failed with process exit code {Code}.",
                report.BelowThresholdCount, threshold, Environment.ExitCode);
        }
        else if (applyGate && gate == ParityGate.Stop && report.NotEvaluatedCount > 0)
        {
            // Measured neighbors cannot turn a source with missing evidence into a strict pass.
            if (Environment.ExitCode == 0) Environment.ExitCode = LowScoreExitCode;
            logger?.LogError(
                "[ConversionParity] {Count} program(s) could not be evaluated; ON_LOW_SCORE=stop failed with process exit code {Code}.",
                report.NotEvaluatedCount, Environment.ExitCode);
        }

        return BuildMarkdown(report);
    }

    private static string DescribeFiles(IEnumerable<CodeFile> group)
    {
        var names = group.Select(f => Path.GetFileName(f.FilePath!)).OrderBy(n => n).ToList();
        return names.Count == 1 ? names[0] : $"{names[0]} (+{names.Count - 1} more)";
    }

    internal static async Task<ProgramParityResult> RepairAsync(
        ProgramParityResult result, string source, Dictionary<string, string> current,
        string language, double threshold,
        Func<ParityRepairRequest, Task<IReadOnlyDictionary<string, string>?>> repair,
        Func<IReadOnlyDictionary<string, string>, ProgramParityResult> evaluate,
        ILogger? logger = null, CancellationToken cancellationToken = default)
    {
        var raw = Environment.GetEnvironmentVariable("PARITY_REPAIR_MAX_ROUNDS");
        var rounds = 2;
        if (!string.IsNullOrWhiteSpace(raw) && (!int.TryParse(raw, out rounds) || rounds < 0))
            throw new InvalidOperationException("PARITY_REPAIR_MAX_ROUNDS must be a non-negative integer.");
        var attempts = 0;
        var accepted = 0;
        string? note = null;
        while (attempts < rounds && Fails(result, threshold))
        {
            cancellationToken.ThrowIfCancellationRequested();
            attempts++;
            IReadOnlyDictionary<string, string>? candidate;
            try { candidate = await repair(new(result.Program, language, source, current, result)).WaitAsync(cancellationToken); }
            catch (Exception ex) when (ex is not OperationCanceledException)
            {
                logger?.LogWarning(ex, "Parity repair failed for {Program}; retaining original output.", result.Program);
                note = "Repair failed; original output retained.";
                break;
            }
            if (candidate is null)
            {
                note = "Repair produced no usable candidate.";
                break;
            }
            if (!current.Keys.ToHashSet(StringComparer.Ordinal).SetEquals(candidate.Keys)
                || candidate.Any(pair => string.IsNullOrWhiteSpace(pair.Value)
                    || (language.Equals("Java", StringComparison.OrdinalIgnoreCase)
                        ? JavaCompileGate.RejectCandidate(current[pair.Key], pair.Value, preserveAllTypes: true)
                        : CompileRepairAgent.Reject(current[pair.Key], pair.Value, new HashSet<string>())) is not null))
            {
                note = "Repair rejected: file identities, public APIs or type declarations were lost.";
                logger?.LogWarning("{Note} Program: {Program}", note, result.Program);
                continue;
            }
            var measured = evaluate(candidate);
            if (measured.Outcome != ParityOutcome.Evaluated || measured.IsDiagnosticStub
                || measured.Score < result.Score
                || measured.Axes.Any(axis => result.Axes.Any(old =>
                    old.Name == axis.Name && axis.MatchedInCode < old.MatchedInCode))
                || !measured.Axes.Any(axis => result.Axes.Any(old =>
                    old.Name == axis.Name && axis.MatchedInCode > old.MatchedInCode)))
            {
                note = "Repair rejected: revalidation found no code-coverage improvement or a regression.";
                logger?.LogWarning("{Note} Program: {Program}", note, result.Program);
                continue;
            }
            try
            {
                foreach (var pair in candidate) await File.WriteAllTextAsync(pair.Key, pair.Value, cancellationToken);
            }
            catch
            {
                foreach (var pair in current) await File.WriteAllTextAsync(pair.Key, pair.Value, CancellationToken.None);
                throw;
            }
            current = candidate.ToDictionary(pair => pair.Key, pair => pair.Value);
            result = measured;
            accepted++;
            note = null;
        }
        return result with { RepairAttempts = attempts, AcceptedRepairs = accepted, RepairNote = note };
    }

    internal static ConversionParityReport BuildReport(
        List<ProgramParityResult> results, string targetLanguage, double threshold, ParityGate gate)
    {
        var evaluated = results.Where(r => r.Outcome == ParityOutcome.Evaluated && r.Score.HasValue).ToList();
        var stamped = results.Select(r => r with { Failed = Fails(r, threshold) }).ToList();

        return new ConversionParityReport
        {
            GeneratedAtUtc = DateTime.UtcNow,
            TargetLanguage = targetLanguage,
            Threshold = threshold,
            OnLowScore = gate == ParityGate.Stop ? "stop" : "warn",
            Programs = stamped,
            EvaluatedCount = evaluated.Count,
            NotEvaluatedCount = results.Count - evaluated.Count,
            BelowThresholdCount = stamped.Count(r => r.Failed),
            AverageScore = evaluated.Count == 0 ? null : Math.Round(evaluated.Average(r => r.Score!.Value), 4),
            ReducedEvidenceCount = evaluated.Count(r => r.EvidenceNotes.Count > 0),
        };
    }

    private static async Task WriteArtifactAsync(
        ConversionParityReport report, string outputFolder, ILogger? logger)
    {
        try
        {
            Directory.CreateDirectory(outputFolder);
            var path = Path.Join(outputFolder, ArtifactName);
            var json = JsonSerializer.Serialize(report, new JsonSerializerOptions { WriteIndented = true });
            await File.WriteAllTextAsync(path, json);
            logger?.LogInformation("[ConversionParity] Wrote {Path}", path);
        }
        catch (Exception ex)
        {
            logger?.LogError(ex, "[ConversionParity] Could not persist {Artifact}; validation cannot be accepted.", ArtifactName);
            throw;
        }
    }

    internal static string BuildMarkdown(ConversionParityReport report)
    {
        var sb = new StringBuilder();
        sb.AppendLine("## 🔍 Conversion Parity (preview)");
        sb.AppendLine();
        var attemptedRepairs = report.Programs.Where(program => program.RepairAttempts > 0).ToList();
        if (attemptedRepairs.Count > 0)
        {
            sb.AppendLine("### Bounded parity repairs");
            sb.AppendLine();
            sb.AppendLine("| Source | Attempts | Accepted | Remaining issue |");
            sb.AppendLine("|---|---|---|---|");
            foreach (var program in attemptedRepairs)
                sb.AppendLine($"| {program.Program} | {program.RepairAttempts} | {program.AcceptedRepairs} | {program.RepairNote ?? (program.Failed ? "Parity gaps remain." : "-")} |");
            sb.AppendLine();
        }

        if (report.EvaluatedCount == 0)
        {
            sb.AppendLine($"**No program could be evaluated** ({report.NotEvaluatedCount} skipped). Parity needs structural context — run `./doctor.sh rekt-full` before converting.");
            sb.AppendLine();
            AppendNotEvaluated(sb, report);
            return sb.ToString();
        }

        // MIN_PROGRAM_SCORE is parsed invariantly, so the value shown must round-trip under any locale.
        sb.AppendLine(FormattableString.Invariant(
            $"Structural coverage of the generated {report.TargetLanguage} against the COBOL each file came from. Threshold `MIN_PROGRAM_SCORE={report.Threshold:0.##}`, gate `ON_LOW_SCORE={report.OnLowScore}`."));
        sb.AppendLine();
        sb.AppendLine($"- Evaluated: **{report.EvaluatedCount}**, not evaluated: **{report.NotEvaluatedCount}**");
        sb.AppendLine(FormattableString.Invariant($"- Average score: **{report.AverageScore:0.00}**"));
        if (report.ReducedEvidenceCount > 0)
        {
            sb.AppendLine(FormattableString.Invariant(
                $"- Scored against reduced evidence: **{report.ReducedEvidenceCount}** of {report.EvaluatedCount}"));
            sb.AppendLine("  (see each program's evidence notes: unavailable copybook fields are excluded, "
                + "and directly declared copybook members do not establish nested COPY or storage-layout equivalence)");
        }
        sb.AppendLine($"- Failing parity: **{report.BelowThresholdCount}**");
        sb.AppendLine();

        var flagged = report.Programs
            .Where(p => Fails(p, report.Threshold))
            .OrderBy(p => p.Score ?? 0)
            .ToList();

        if (flagged.Count > 0)
        {
            sb.AppendLine("### Programs failing parity");
            sb.AppendLine();
            sb.AppendLine("| COBOL Source | Generated File | Score | Missing | Lost axes |");
            sb.AppendLine("|---|---|---|---|---|");
            foreach (var p in flagged.Take(20))
            {
                var missing = p.Gaps.Count(g => g.Kind == ParityGapKind.Missing);
                var lost = p.LostAxes.Count == 0 ? "-" : string.Join(", ", p.LostAxes);
                sb.AppendLine(FormattableString.Invariant(
                    $"| {p.Program} | {Path.GetFileName(p.GeneratedFile ?? "-")} | {p.Score:0.00} | {missing} | {lost} |"));
            }
            sb.AppendLine();

            foreach (var p in flagged.Take(5))
            {
                var missing = p.Gaps.Where(g => g.Kind == ParityGapKind.Missing).Take(12).ToList();
                if (missing.Count == 0) continue;
                sb.AppendLine($"**{p.Program}** — not represented in the generated code:");
                sb.AppendLine();
                foreach (var g in missing)
                {
                    var detail = string.IsNullOrWhiteSpace(g.Detail) ? string.Empty : $" — {g.Detail}";
                    sb.AppendLine($"- `{g.Axis}`: `{g.Symbol}`{detail}");
                }
                sb.AppendLine();
            }
        }
        else
        {
            sb.AppendLine("All evaluated programs meet the threshold.");
            sb.AppendLine();
        }

        AppendNotEvaluated(sb, report);

        sb.AppendLine("Parity measures structural representation, not behavioural equivalence. A passing score means the COBOL procedures, fields, `CALL` targets and SQL tables are visible in the generated code — it does not mean the logic is correct.");
        sb.AppendLine();
        return sb.ToString();
    }

    private static void AppendNotEvaluated(StringBuilder sb, ConversionParityReport report)
    {
        var skipped = report.Programs.Where(p => p.Outcome == ParityOutcome.NotEvaluated).ToList();
        if (skipped.Count == 0) return;

        sb.AppendLine("### Not evaluated");
        sb.AppendLine();
        sb.AppendLine("These programs have **no parity result** — absence of a gap here is not evidence of a good conversion.");
        sb.AppendLine();
        sb.AppendLine("| COBOL Source | Reason |");
        sb.AppendLine("|---|---|");
        foreach (var p in skipped.Take(20))
        {
            sb.AppendLine($"| {p.Program} | {p.NotEvaluatedReason} |");
        }
        sb.AppendLine();
    }

    private static ProgramParityResult NotEvaluated(string program, string? generatedFile, string reason) => new()
    {
        Program = program,
        GeneratedFile = generatedFile,
        Outcome = ParityOutcome.NotEvaluated,
        NotEvaluatedReason = reason,
    };

    private static ProgramParityResult NoOutput(string program, int sharingCount = 1) => new()
    {
        Program = program,
        Outcome = ParityOutcome.Evaluated,
        Score = 0,
        Gaps = [new ParityGap
        {
            Axis = "file",
            Symbol = program,
            Kind = ParityGapKind.Missing,
            Detail = sharingCount > 1
                ? $"Conversion produced no output file for any of the {sharingCount} source " +
                  $"programs named '{program}'."
                : "Conversion produced no output file for this source program.",
        }],
    };

    // A program fails parity if its weighted score is short, or if any axis that had expected
    // symbols has none of them in code. Renormalisation can otherwise keep a whole-axis loss
    // above the threshold: dropping every CALL target scores 0.76 against a 0.75 gate.
    internal static bool Fails(ProgramParityResult result, double threshold) =>
        result.Outcome == ParityOutcome.Evaluated
        && (result.Score is { } score && score < threshold || result.LostAxes.Count > 0);

    internal static double ReadThreshold(ILogger? logger = null)
    {
        var raw = Environment.GetEnvironmentVariable("MIN_PROGRAM_SCORE");
        if (string.IsNullOrWhiteSpace(raw)) return DefaultThreshold;

        if (!double.TryParse(raw, System.Globalization.NumberStyles.Float,
                System.Globalization.CultureInfo.InvariantCulture, out var value)
            || !double.IsFinite(value))
        {
            logger?.LogWarning(
                "[ConversionParity] MIN_PROGRAM_SCORE='{Raw}' is not a finite number; using {Default}.", raw, DefaultThreshold);
            return DefaultThreshold;
        }

        return Math.Clamp(value, 0.0, 1.0);
    }

    internal static ParityGate ReadGate(ILogger? logger = null)
    {
        var raw = Environment.GetEnvironmentVariable("ON_LOW_SCORE");
        if (string.IsNullOrWhiteSpace(raw)) return ParityGate.Warn;

        if (string.Equals(raw, "stop", StringComparison.OrdinalIgnoreCase)) return ParityGate.Stop;
        if (string.Equals(raw, "warn", StringComparison.OrdinalIgnoreCase)) return ParityGate.Warn;

        logger?.LogWarning("[ConversionParity] ON_LOW_SCORE='{Raw}' is not 'warn' or 'stop'; using 'warn'.", raw);
        return ParityGate.Warn;
    }

    private static string? FindRepoRoot()
    {
        var d = new DirectoryInfo(AppContext.BaseDirectory);
        while (d != null && !File.Exists(Path.Join(d.FullName, "doctor.sh"))) d = d.Parent;
        return d?.FullName;
    }

    internal enum ParityGate
    {
        Warn,
        Stop,
    }
}
