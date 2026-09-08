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
        ILogger? logger = null)
    {
        var files = generatedFiles
            .Where(f => !string.IsNullOrWhiteSpace(f.FilePath))
            .ToList();

        if (files.Count == 0) return string.Empty;

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
        var results = new List<ProgramParityResult>();

        foreach (var file in files)
        {
            var program = file.OriginalCobolFileName;
            if (string.IsNullOrWhiteSpace(program))
            {
                results.Add(NotEvaluated(
                    Path.GetFileName(file.FilePath!),
                    file.FilePath,
                    "Generated file could not be mapped back to a COBOL source program."));
                continue;
            }

            string code;
            try
            {
                code = await File.ReadAllTextAsync(file.FilePath!);
            }
            catch (Exception ex)
            {
                logger?.LogDebug(ex, "[ConversionParity] Could not read {File}", file.FilePath);
                results.Add(NotEvaluated(program, file.FilePath, $"Generated file could not be read: {ex.Message}"));
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
            results.Add(ConversionParityValidator.Evaluate(program, file.FilePath, code, context, facts));
        }

        var report = BuildReport(results, targetLanguage, threshold, gate);
        await WriteArtifactAsync(report, outputFolder, logger);

        if (gate == ParityGate.Stop && report.BelowThresholdCount > 0)
        {
            Environment.ExitCode = LowScoreExitCode;
            logger?.LogError(
                "[ConversionParity] {Count} program(s) below MIN_PROGRAM_SCORE={Threshold}; ON_LOW_SCORE=stop set exit code {Code}.",
                report.BelowThresholdCount, threshold, LowScoreExitCode);
        }

        return BuildMarkdown(report);
    }

    internal static ConversionParityReport BuildReport(
        List<ProgramParityResult> results, string targetLanguage, double threshold, ParityGate gate)
    {
        var evaluated = results.Where(r => r.Outcome == ParityOutcome.Evaluated && r.Score.HasValue).ToList();

        return new ConversionParityReport
        {
            GeneratedAtUtc = DateTime.UtcNow,
            TargetLanguage = targetLanguage,
            Threshold = threshold,
            OnLowScore = gate == ParityGate.Stop ? "stop" : "warn",
            Programs = results,
            EvaluatedCount = evaluated.Count,
            NotEvaluatedCount = results.Count - evaluated.Count,
            BelowThresholdCount = evaluated.Count(r => r.Score!.Value < threshold),
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

        sb.AppendLine($"Structural coverage of the generated {report.TargetLanguage} against the COBOL each file came from. Threshold `MIN_PROGRAM_SCORE={report.Threshold:0.##}`, gate `ON_LOW_SCORE={report.OnLowScore}`.");
        sb.AppendLine();
        sb.AppendLine($"- Evaluated: **{report.EvaluatedCount}**, not evaluated: **{report.NotEvaluatedCount}**");
        sb.AppendLine($"- Average score: **{report.AverageScore:0.00}**");
        sb.AppendLine($"- Below threshold: **{report.BelowThresholdCount}**");
        sb.AppendLine();

        var flagged = report.Programs
            .Where(p => p.Outcome == ParityOutcome.Evaluated && p.Score < report.Threshold)
            .OrderBy(p => p.Score)
            .ToList();

        if (flagged.Count > 0)
        {
            sb.AppendLine("### Programs below threshold");
            sb.AppendLine();
            sb.AppendLine("| COBOL Source | Generated File | Score | Missing |");
            sb.AppendLine("|---|---|---|---|");
            foreach (var p in flagged.Take(20))
            {
                var missing = p.Gaps.Count(g => g.Kind == ParityGapKind.Missing);
                sb.AppendLine($"| {p.Program} | {Path.GetFileName(p.GeneratedFile ?? "-")} | {p.Score:0.00} | {missing} |");
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

    internal static double ReadThreshold(ILogger? logger = null)
    {
        var raw = Environment.GetEnvironmentVariable("MIN_PROGRAM_SCORE");
        if (string.IsNullOrWhiteSpace(raw)) return DefaultThreshold;

        if (!double.TryParse(raw, System.Globalization.NumberStyles.Float,
                System.Globalization.CultureInfo.InvariantCulture, out var value))
        {
            logger?.LogWarning(
                "[ConversionParity] MIN_PROGRAM_SCORE='{Raw}' is not a number; using {Default}.", raw, DefaultThreshold);
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
