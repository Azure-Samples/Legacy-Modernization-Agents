// CodeReviewerAgent.cs — LLM-based idiomatic-code reviewer for converted output.
// Returns structured findings with severity + suggestion. Optional repair pass
// asks the same prompt template (with --repair flag) to fix the highest-severity
// findings.

using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Agents.Infrastructure;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using System.Text.Json;
using AIChatMessage = Microsoft.Extensions.AI.ChatMessage;

namespace CobolToQuarkusMigration.Agents;

public sealed class CodeReviewFinding
{
    public string Severity { get; set; } = "info";       // error|warning|info
    public int? Line { get; set; }
    public string Rule { get; set; } = "";
    public string Message { get; set; } = "";
    public string Suggestion { get; set; } = "";
}

public sealed class CodeReviewReport
{
    public string Program { get; set; } = "";
    public double Score { get; set; }
    public string Summary { get; set; } = "";
    public List<CodeReviewFinding> Findings { get; set; } = new();
}

public class CodeReviewerAgent : AgentBase
{
    protected override string AgentName => "CodeReviewerAgent";

    public CodeReviewerAgent(IChatClient c, ILogger<CodeReviewerAgent> l, string m,
        EnhancedLogger? el = null, ChatLogger? cl = null, RateLimiter? rl = null, AppSettings? s = null)
        : base(c, l, m, el, cl, rl, s) { }

    public CodeReviewerAgent(ResponsesApiClient r, ILogger<CodeReviewerAgent> l, string m,
        EnhancedLogger? el = null, ChatLogger? cl = null, RateLimiter? rl = null, AppSettings? s = null)
        : base(r, l, m, el, cl, rl, s) { }

    public async Task<CodeReviewReport> ReviewAsync(
        StructuralContext sc, string convertedCode, string targetLanguage)
    {
        var report = new CodeReviewReport { Program = sc.Program };
        if (string.IsNullOrWhiteSpace(convertedCode))
        {
            report.Score = 0.0;
            report.Summary = "No code to review.";
            return report;
        }

        var systemPrompt = PromptLoader.Load("CodeReviewer", new Dictionary<string, string>
        {
            ["TargetLanguage"] = targetLanguage,
            ["StructuralContext"] = RektContextFormatter.ToPromptBlock(sc),
            ["Code"] = convertedCode,
        });

        string response;
        try
        {
            if (ChatClient is not null)
            {
                var messages = new List<AIChatMessage>
                {
                    new(ChatRole.System, systemPrompt),
                    new(ChatRole.User, "Produce the review JSON now."),
                };
                var options = new ChatOptions { ModelId = ModelId, MaxOutputTokens = 4096 };
                var result = await ChatClient.GetResponseAsync(messages, options);
                response = result.Text ?? "";
            }
            else if (ResponsesClient is not null)
            {
                response = await ResponsesClient.GetResponseAsync(systemPrompt, "Produce the review JSON now.", maxOutputTokens: 4096);
            }
            else return report;

            var json = ExtractJsonObject(response);
            if (string.IsNullOrWhiteSpace(json)) return report;

            using var doc = JsonDocument.Parse(json);
            if (doc.RootElement.TryGetProperty("score", out var sc1) && sc1.ValueKind == JsonValueKind.Number)
                report.Score = sc1.GetDouble();
            if (doc.RootElement.TryGetProperty("summary", out var sm) && sm.ValueKind == JsonValueKind.String)
                report.Summary = sm.GetString() ?? "";
            if (doc.RootElement.TryGetProperty("findings", out var fs) && fs.ValueKind == JsonValueKind.Array)
            {
                foreach (var f in fs.EnumerateArray())
                {
                    var finding = new CodeReviewFinding();
                    if (f.TryGetProperty("severity",   out var v1)) finding.Severity   = v1.GetString() ?? "info";
                    if (f.TryGetProperty("line",       out var v2) && v2.ValueKind == JsonValueKind.Number) finding.Line = v2.GetInt32();
                    if (f.TryGetProperty("rule",       out var v3)) finding.Rule       = v3.GetString() ?? "";
                    if (f.TryGetProperty("message",    out var v4)) finding.Message    = v4.GetString() ?? "";
                    if (f.TryGetProperty("suggestion", out var v5)) finding.Suggestion = v5.GetString() ?? "";
                    report.Findings.Add(finding);
                }
            }
        }
        catch (Exception ex)
        {
            Logger.LogWarning("CodeReviewerAgent failed for {Program}: {Msg}", sc.Program, ex.Message);
        }
        return report;
    }

    public static string FormatReport(CodeReviewReport r)
    {
        var sb = new System.Text.StringBuilder();
        sb.AppendLine($"# Code review — {r.Program}");
        sb.AppendLine();
        sb.AppendLine($"**Score:** {r.Score:F2}    **Findings:** {r.Findings.Count}");
        sb.AppendLine();
        sb.AppendLine($"> {r.Summary}");
        sb.AppendLine();
        var grouped = r.Findings.GroupBy(f => f.Severity).OrderBy(g => g.Key switch
        {
            "error" => 0, "warning" => 1, _ => 2,
        });
        foreach (var grp in grouped)
        {
            sb.AppendLine($"## {grp.Key.ToUpperInvariant()} ({grp.Count()})");
            foreach (var f in grp)
            {
                var line = f.Line.HasValue ? $"L{f.Line}" : "—";
                sb.AppendLine($"- **[{f.Rule}]** ({line}) {f.Message}");
                if (!string.IsNullOrEmpty(f.Suggestion)) sb.AppendLine($"  > _Suggestion:_ {f.Suggestion}");
            }
            sb.AppendLine();
        }
        return sb.ToString();
    }

    private static string ExtractJsonObject(string text)
    {
        if (string.IsNullOrEmpty(text)) return "";
        var fenceStart = text.IndexOf("```json", StringComparison.OrdinalIgnoreCase);
        if (fenceStart < 0) fenceStart = text.IndexOf("```");
        if (fenceStart >= 0)
        {
            var bodyStart = text.IndexOf('\n', fenceStart) + 1;
            var fenceEnd  = text.IndexOf("```", bodyStart);
            if (fenceEnd > bodyStart) return text.Substring(bodyStart, fenceEnd - bodyStart).Trim();
        }
        var first = text.IndexOf('{');
        var last  = text.LastIndexOf('}');
        return first >= 0 && last > first ? text.Substring(first, last - first + 1) : "";
    }
}
