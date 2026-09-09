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
        IEnumerable<string>? sourcePrograms = null)
    {
        var files = generatedFiles
            .Where(f => !string.IsNullOrWhiteSpace(f.FilePath))
            .ToList();

        var expectedPrograms = (sourcePrograms ?? Enumerable.Empty<string>())
            .Where(p => !string.IsNullOrWhiteSpace(p))
            .Distinct(StringComparer.OrdinalIgnoreCase)
            .ToList();

        if (files.Count == 0 && expectedPrograms.Count == 0) return string.Empty;

        var repoRoot = FindRepoRoot();
        if (repoRoot is null)
        {
            logger?.LogWarning("[ConversionParity] Repo root (doctor.sh) not found; parity check skipped.");
            return string.Empty;
        }

        var threshold = ReadThreshold(logger);
        var gate = ReadGate(logger);
        var sourceFolder = Environment.GetEnvironmentVariable("COBOL_SOURCE_FOLDER") ?? "source";
        var factsDir = Path.Combine(repoRoot, "output", "rekt");

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

        var unmapped = files.Where(f => string.IsNullOrWhiteSpace(f.OriginalCobolFileName)).ToList();
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
            .Where(f => !string.IsNullOrWhiteSpace(f.OriginalCobolFileName))
            .GroupBy(f => f.OriginalCobolFileName!, StringComparer.OrdinalIgnoreCase);

        var converted = new HashSet<string>(StringComparer.OrdinalIgnoreCase);

        foreach (var group in byProgram)
        {
            var program = group.Key;
            converted.Add(program);

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
            results.Add(ConversionParityValidator.Evaluate(
                program, generatedFile, code, context, facts, stubCopybooks));
        }

        // A source that produced no output at all is the worst parity failure there is, and it
        // is invisible if the report only walks generated files.
        foreach (var program in expectedPrograms.Where(p => !converted.Contains(p)))
        {
            results.Add(NoOutput(program));
        }

        var report = BuildReport(results, targetLanguage, threshold, gate);
        await WriteArtifactAsync(report, outputFolder, logger);

        if (gate == ParityGate.Stop && report.BelowThresholdCount > 0)
        {
            Environment.ExitCode = LowScoreExitCode;
            logger?.LogError(
                "[ConversionParity] {Count} program(s) failed parity (MIN_PROGRAM_SCORE={Threshold}); ON_LOW_SCORE=stop set exit code {Code}.",
                report.BelowThresholdCount, threshold, LowScoreExitCode);
        }
        else if (gate == ParityGate.Stop && results.Count > 0 && report.EvaluatedCount == 0)
        {
            // Exiting 0 here would report "no parity failures" when the truth is that parity was
            // never measured. Under an explicit gate, absent evidence is not a pass.
            Environment.ExitCode = LowScoreExitCode;
            logger?.LogError(
                "[ConversionParity] No program could be evaluated ({Count} attempted); ON_LOW_SCORE=stop set exit code {Code}.",
                results.Count, LowScoreExitCode);
        }

        return BuildMarkdown(report);
    }

    private static string DescribeFiles(IEnumerable<CodeFile> group)
    {
        var names = group.Select(f => Path.GetFileName(f.FilePath!)).OrderBy(n => n).ToList();
        return names.Count == 1 ? names[0] : $"{names[0]} (+{names.Count - 1} more)";
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
        };
    }

    private static async Task WriteArtifactAsync(
        ConversionParityReport report, string outputFolder, ILogger? logger)
    {
        try
        {
            Directory.CreateDirectory(outputFolder);
            var path = Path.Combine(outputFolder, ArtifactName);
            var json = JsonSerializer.Serialize(report, new JsonSerializerOptions { WriteIndented = true });
            await File.WriteAllTextAsync(path, json);
            logger?.LogInformation("[ConversionParity] Wrote {Path}", path);
        }
        catch (Exception ex)
        {
            logger?.LogWarning(ex, "[ConversionParity] Could not write {Artifact}", ArtifactName);
        }
    }

    internal static string BuildMarkdown(ConversionParityReport report)
    {
        var sb = new StringBuilder();
        sb.AppendLine("## 🔍 Conversion Parity (preview)");
        sb.AppendLine();

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

    private static ProgramParityResult NoOutput(string program) => new()
    {
        Program = program,
        Outcome = ParityOutcome.Evaluated,
        Score = 0,
        Gaps = [new ParityGap
        {
            Axis = "file",
            Symbol = program,
            Kind = ParityGapKind.Missing,
            Detail = "Conversion produced no output file for this source program.",
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
        while (d != null && !File.Exists(Path.Combine(d.FullName, "doctor.sh"))) d = d.Parent;
        return d?.FullName;
    }

    internal enum ParityGate
    {
        Warn,
        Stop,
    }
}
