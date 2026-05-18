// ConversionParityAgent.cs — Compares converted Java/C# against a REKT structural
// context and reports missing translations. Optional repair pass (LLM) closes
// the gaps, up to MAX_VALIDATOR_RETRIES iterations. Score is per-program in [0..1].
//
// Honoured env vars (set by ProcessManager / doctor.sh):
//   MAX_VALIDATOR_RETRIES   — int, default 1
//   MIN_PROGRAM_SCORE       — double in [0..1], default 0 (off)
//   ON_LOW_SCORE            — "continue" | "stop", default "continue"

using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Agents.Infrastructure;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using System.Text.RegularExpressions;
using AIChatMessage = Microsoft.Extensions.AI.ChatMessage;

namespace CobolToQuarkusMigration.Agents;

public sealed class ConversionParityReport
{
    public string Program { get; set; } = "";
    public double Score { get; set; }              // 0.0..1.0
    public List<string> Gaps { get; set; } = new();
    public string CorrectedCode { get; set; } = "";
    public int RetriesUsed { get; set; }
    public bool StopRequested { get; set; }        // true if score < min and on_low_score=stop
}

public class ConversionParityAgent : AgentBase
{
    protected override string AgentName => "ConversionParityAgent";

    public ConversionParityAgent(
        IChatClient chatClient, ILogger<ConversionParityAgent> logger, string modelId,
        EnhancedLogger? el = null, ChatLogger? cl = null, RateLimiter? rl = null, AppSettings? settings = null)
        : base(chatClient, logger, modelId, el, cl, rl, settings) { }

    public ConversionParityAgent(
        ResponsesApiClient responsesClient, ILogger<ConversionParityAgent> logger, string modelId,
        EnhancedLogger? el = null, ChatLogger? cl = null, RateLimiter? rl = null, AppSettings? settings = null)
        : base(responsesClient, logger, modelId, el, cl, rl, settings) { }

    /// <summary>
    /// Validate + optionally repair. Returns the (possibly repaired) converted
    /// code and a parity report.
    /// </summary>
    public async Task<ConversionParityReport> ValidateAndRepairAsync(
        StructuralContext sc,
        string cobolSource,
        string convertedCode,
        string targetLanguage)
    {
        var maxRetries = ReadIntEnv("MAX_VALIDATOR_RETRIES", 1);
        var minScore   = ReadDoubleEnv("MIN_PROGRAM_SCORE", 0.0);
        var onLowScore = (Environment.GetEnvironmentVariable("ON_LOW_SCORE") ?? "continue").ToLowerInvariant();

        var report = new ConversionParityReport
        {
            Program = sc.Program,
            CorrectedCode = convertedCode,
        };

        for (var attempt = 0; attempt <= maxRetries; attempt++)
        {
            var (score, gaps) = ScoreParity(sc, report.CorrectedCode, targetLanguage);
            report.Score = score;
            report.Gaps = gaps;

            if (gaps.Count == 0 || score >= 0.95) break;
            if (attempt == maxRetries) break;

            Logger.LogInformation(
                "[ConversionParity] {Program} attempt {Attempt}/{Max}: score={Score:F2}, {Gaps} gap(s) — attempting repair",
                sc.Program, attempt + 1, maxRetries, score, gaps.Count);

            // Repair via LLM.
            try
            {
                report.CorrectedCode = await RepairAsync(sc, cobolSource, report.CorrectedCode, targetLanguage, gaps);
                report.RetriesUsed = attempt + 1;
            }
            catch (Exception ex)
            {
                Logger.LogWarning("[ConversionParity] repair failed for {Program}: {Msg}", sc.Program, ex.Message);
                break;
            }
        }

        if (minScore > 0 && report.Score < minScore)
        {
            Logger.LogWarning(
                "[ConversionParity] {Program} score {Score:F2} below threshold {Min:F2} (action: {Action})",
                sc.Program, report.Score, minScore, onLowScore);
            report.StopRequested = onLowScore == "stop";
        }
        return report;
    }

    // ── Deterministic parity scoring ──────────────────────────────────────

    /// <summary>
    /// Compare a REKT structural context to converted target code. Score is a
    /// weighted average over (sections→methods, copybooks→fields,
    /// calls→service-calls, sql→repo-methods). Gap list is human-readable.
    /// </summary>
    public static (double score, List<string> gaps) ScoreParity(
        StructuralContext sc, string convertedCode, string targetLanguage)
    {
        var gaps = new List<string>();
        if (string.IsNullOrWhiteSpace(convertedCode))
        {
            gaps.Add("Converted code is empty.");
            return (0.0, gaps);
        }

        var ctx = sc.Context;
        var lc = convertedCode.ToLowerInvariant();

        // 1) Sections → methods (presence by name).
        int sectionsExpected = 0, sectionsFound = 0;
        foreach (var s in ctx.Sections)
        {
            sectionsExpected++;
            var camel = ToCamel(s.Name);
            var pascal = ToPascal(s.Name);
            if (lc.Contains(camel.ToLowerInvariant() + "(") ||
                lc.Contains(pascal.ToLowerInvariant() + "("))
                sectionsFound++;
            else
                gaps.Add($"Missing method for SECTION '{s.Name}' (expected {camel}() or {pascal}()).");
        }

        // 2) Copybook fields → DTO fields. Walk top-level group items.
        int fieldsExpected = 0, fieldsFound = 0;
        void WalkFields(IEnumerable<RektDataItem> items)
        {
            foreach (var d in items)
            {
                if (!string.IsNullOrEmpty(d.Name) && d.Level >= 3)
                {
                    fieldsExpected++;
                    var camel = ToCamel(d.Name);
                    if (lc.Contains(camel.ToLowerInvariant())) fieldsFound++;
                    else gaps.Add($"Missing field for COPYBOOK '{d.Name}' (expected {camel}).");
                }
                if (d.Children.Count > 0) WalkFields(d.Children);
            }
        }
        WalkFields(ctx.DataStructure);

        // 3) CALL targets → service calls.
        int callsExpected = ctx.CallTargets.Count, callsFound = 0;
        foreach (var c in ctx.CallTargets)
        {
            var stem = System.IO.Path.GetFileNameWithoutExtension(c.TargetProgram);
            if (lc.Contains(stem.ToLowerInvariant()))
                callsFound++;
            else
                gaps.Add($"Missing service call for CALL '{c.TargetProgram}'.");
        }

        // 4) SQL → repository/method.
        int sqlExpected = ctx.SqlStatements.Count, sqlFound = 0;
        foreach (var s in ctx.SqlStatements)
        {
            // Heuristic: look for repository/JPA hints near the table name
            var tableHit = s.Tables.Any(t =>
                lc.Contains(t.ToLowerInvariant()) ||
                lc.Contains(ToPascal(t).ToLowerInvariant() + "repository") ||
                lc.Contains("repository<" + ToPascal(t).ToLowerInvariant()));
            if (tableHit) sqlFound++;
            else gaps.Add($"Missing data access for SQL {s.Operation} on {string.Join(",", s.Tables)} (line {s.LineNumber}).");
        }

        // Weighted average. Skip categories with zero expected to avoid divide-by-zero.
        var components = new List<(double weight, double pct)>();
        if (sectionsExpected > 0) components.Add((0.40, sectionsFound / (double)sectionsExpected));
        if (fieldsExpected  > 0)  components.Add((0.25, fieldsFound  / (double)fieldsExpected));
        if (callsExpected   > 0)  components.Add((0.20, callsFound   / (double)callsExpected));
        if (sqlExpected     > 0)  components.Add((0.15, sqlFound     / (double)sqlExpected));

        double score;
        if (components.Count == 0)
        {
            // No structural context to compare against — be lenient (assume OK).
            score = string.IsNullOrWhiteSpace(convertedCode) ? 0.0 : 0.8;
        }
        else
        {
            var totalW = components.Sum(c => c.weight);
            score = components.Sum(c => c.weight * c.pct) / totalW;
        }
        return (score, gaps);
    }

    // ── LLM-driven repair pass ────────────────────────────────────────────

    private async Task<string> RepairAsync(
        StructuralContext sc, string cobolSource, string convertedCode, string targetLanguage, List<string> gaps)
    {
        var systemPrompt = PromptLoader.Load("ConversionParity", new Dictionary<string, string>
        {
            ["Gaps"] = string.Join("\n", gaps.Select(g => "- " + g)),
            ["StructuralContext"] = RektContextFormatter.ToPromptBlock(sc),
            ["CobolSource"] = TruncateForPrompt(cobolSource, 40_000),
            ["ConvertedCode"] = TruncateForPrompt(convertedCode, 30_000),
            ["TargetLanguage"] = targetLanguage,
        });

        string response;
        if (ChatClient is not null)
        {
            var messages = new List<AIChatMessage>
            {
                new(ChatRole.System, systemPrompt),
                new(ChatRole.User, "Produce the corrected code now."),
            };
            var options = new ChatOptions { ModelId = ModelId, MaxOutputTokens = 16384 };
            var result = await ChatClient.GetResponseAsync(messages, options);
            response = result.Text ?? "";
        }
        else if (ResponsesClient is not null)
        {
            response = await ResponsesClient.GetResponseAsync(systemPrompt, "Produce the corrected code now.", maxOutputTokens: 16384);
        }
        else return convertedCode;

        // Strip any accidental ``` fences the model added.
        return StripCodeFences(response);
    }

    private static string TruncateForPrompt(string s, int max) =>
        s.Length <= max ? s : s.Substring(0, max) + "\n// [TRUNCATED]\n";

    private static string StripCodeFences(string s)
    {
        if (string.IsNullOrEmpty(s)) return s;
        var trimmed = s.Trim();
        if (trimmed.StartsWith("```"))
        {
            var firstNewline = trimmed.IndexOf('\n');
            if (firstNewline > 0) trimmed = trimmed.Substring(firstNewline + 1);
            if (trimmed.EndsWith("```")) trimmed = trimmed.Substring(0, trimmed.Length - 3);
        }
        return trimmed.TrimEnd();
    }

    // ── Naming utilities ──────────────────────────────────────────────────

    private static readonly Regex KebabSplit = new(@"[-_\s]+", RegexOptions.Compiled);

    private static string ToCamel(string s)
    {
        if (string.IsNullOrEmpty(s)) return s;
        var parts = KebabSplit.Split(s.Trim().ToLowerInvariant()).Where(p => p.Length > 0).ToArray();
        if (parts.Length == 0) return s;
        var sb = new System.Text.StringBuilder(parts[0]);
        for (var i = 1; i < parts.Length; i++)
            sb.Append(char.ToUpperInvariant(parts[i][0])).Append(parts[i].AsSpan(1));
        return sb.ToString();
    }

    private static string ToPascal(string s)
    {
        var camel = ToCamel(s);
        return string.IsNullOrEmpty(camel) ? camel : char.ToUpperInvariant(camel[0]) + camel.Substring(1);
    }

    // ── env helpers ───────────────────────────────────────────────────────

    private static int ReadIntEnv(string name, int fallback)
        => int.TryParse(Environment.GetEnvironmentVariable(name), out var v) ? v : fallback;

    private static double ReadDoubleEnv(string name, double fallback)
        => double.TryParse(Environment.GetEnvironmentVariable(name),
            System.Globalization.NumberStyles.Float,
            System.Globalization.CultureInfo.InvariantCulture, out var v) ? v : fallback;
}
