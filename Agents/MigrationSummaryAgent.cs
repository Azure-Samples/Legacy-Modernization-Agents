// MigrationSummaryAgent.cs — Produces a per-program migration summary that
// stitches together the target plan, parity report, code-review findings, and
// optional data-mapping + test artefacts into a stakeholder-friendly Markdown
// document. Optional portfolio-level rollup via Aggregate().

using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Agents.Infrastructure;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using AIChatMessage = Microsoft.Extensions.AI.ChatMessage;

namespace CobolToQuarkusMigration.Agents;

public sealed class MigrationSummary
{
    public string Program { get; set; } = "";
    public string Markdown { get; set; } = "";
    public double RiskScore { get; set; }       // 0..1, higher = riskier
}

public class MigrationSummaryAgent : AgentBase
{
    protected override string AgentName => "MigrationSummaryAgent";

    public MigrationSummaryAgent(IChatClient c, ILogger<MigrationSummaryAgent> l, string m,
        EnhancedLogger? el = null, ChatLogger? cl = null, RateLimiter? rl = null, AppSettings? s = null)
        : base(c, l, m, el, cl, rl, s) { }

    public MigrationSummaryAgent(ResponsesApiClient r, ILogger<MigrationSummaryAgent> l, string m,
        EnhancedLogger? el = null, ChatLogger? cl = null, RateLimiter? rl = null, AppSettings? s = null)
        : base(r, l, m, el, cl, rl, s) { }

    public async Task<MigrationSummary> SummarizeAsync(
        StructuralContext sc,
        ConversionParityReport? parity,
        CodeReviewReport? review,
        DataMappingResult? dataMapping,
        TestSynthesisResult? tests)
    {
        var result = new MigrationSummary { Program = sc.Program };

        var planText = sc.Context.TargetPlan is { } p
            ? $"Component: {p.TargetComponentName} ({p.TargetLayer})\nTech: {p.TargetTech}\nStrategy: {p.Strategy} (wave {p.Wave})\nComplexity: {p.Complexity:F2}\nRationale: {p.Rationale}\nNotes:\n - {string.Join("\n - ", p.MigrationNotes)}"
            : "(no target plan saved — run Target Architecture view → 💾 Save for AI agent first)";

        var parityText = parity != null
            ? $"Score: {parity.Score:F2}\nGaps:\n - {string.Join("\n - ", parity.Gaps)}\nRetries used: {parity.RetriesUsed}\nStopRequested: {parity.StopRequested}"
            : "(parity not computed)";

        var reviewText = review != null
            ? $"Score: {review.Score:F2}\nFindings: {review.Findings.Count} ({review.Findings.Count(f => f.Severity == "error")} error, {review.Findings.Count(f => f.Severity == "warning")} warning)\nSummary: {review.Summary}"
            : "(reviewer not run)";

        var dmText = dataMapping != null
            ? $"Entities: {dataMapping.Entities.Count}, Repos: {dataMapping.Repositories.Count}\nNotes:\n - {string.Join("\n - ", dataMapping.Notes)}"
            : "(no data mapping needed)";

        var testText = tests != null
            ? $"Tests: {tests.Tests.Count}, Fixtures: {tests.Fixtures.Count}\nCovers: {string.Join(", ", tests.Tests.SelectMany(t => t.CoversMethods).Distinct())}"
            : "(no tests generated)";

        var systemPrompt = PromptLoader.Load("MigrationSummary", new Dictionary<string, string>
        {
            ["Program"] = sc.Program,
            ["TargetPlan"] = planText,
            ["Provenance"] = $"{sc.Provenance} (confidence {sc.Confidence:F2})",
            ["StructuralContext"] = RektContextFormatter.ToPromptBlock(sc),
            ["ParityReport"] = parityText,
            ["ReviewReport"] = reviewText,
            ["DataMappingSummary"] = dmText,
            ["TestSummary"] = testText,
        });

        try
        {
            if (ChatClient is not null)
            {
                var messages = new List<AIChatMessage>
                {
                    new(ChatRole.System, systemPrompt),
                    new(ChatRole.User, "Produce the Markdown now."),
                };
                var options = new ChatOptions { ModelId = ModelId, MaxOutputTokens = 4096 };
                var r = await ChatClient.GetResponseAsync(messages, options);
                result.Markdown = r.Text ?? "";
            }
            else if (ResponsesClient is not null)
            {
                result.Markdown = await ResponsesClient.GetResponseAsync(systemPrompt, "Produce the Markdown now.", maxOutputTokens: 4096);
            }
        }
        catch (Exception ex)
        {
            Logger.LogWarning("MigrationSummaryAgent failed for {Program}: {Msg}", sc.Program, ex.Message);
        }

        // Risk heuristic: weighted combination of parity gap count + reviewer severity.
        var gapCount = parity?.Gaps.Count ?? 0;
        var errorCount = review?.Findings.Count(f => f.Severity == "error") ?? 0;
        var warningCount = review?.Findings.Count(f => f.Severity == "warning") ?? 0;
        var parityScore = parity?.Score ?? 1.0;
        var reviewScore = review?.Score ?? 1.0;
        result.RiskScore = Math.Min(1.0,
            0.40 * (1 - parityScore) +
            0.30 * (1 - reviewScore) +
            0.10 * Math.Min(1.0, gapCount / 10.0) +
            0.15 * Math.Min(1.0, errorCount / 5.0) +
            0.05 * Math.Min(1.0, warningCount / 10.0));
        return result;
    }

    /// <summary>
    /// Aggregate per-program summaries into a portfolio-level markdown report.
    /// </summary>
    public static string AggregatePortfolio(IEnumerable<MigrationSummary> summaries)
    {
        var list = summaries.ToList();
        if (list.Count == 0) return "# Portfolio summary\n\n_No programs._\n";
        var sb = new System.Text.StringBuilder();
        sb.AppendLine("# Portfolio migration summary");
        sb.AppendLine();
        sb.AppendLine($"**Total programs:** {list.Count}");
        sb.AppendLine($"**Average risk score:** {list.Average(s => s.RiskScore):F2}");
        sb.AppendLine();
        sb.AppendLine("## Per-program risk ranking");
        sb.AppendLine();
        sb.AppendLine("| Risk | Program |");
        sb.AppendLine("|------|---------|");
        foreach (var s in list.OrderByDescending(s => s.RiskScore))
            sb.AppendLine($"| {s.RiskScore:F2} | {s.Program} |");
        sb.AppendLine();
        sb.AppendLine("## Per-program summaries");
        sb.AppendLine();
        foreach (var s in list.OrderByDescending(s => s.RiskScore))
        {
            sb.AppendLine($"---");
            sb.AppendLine();
            sb.AppendLine(s.Markdown);
            sb.AppendLine();
        }
        return sb.ToString();
    }
}
