// TestSynthesizerAgent.cs — Generates unit + integration tests for converted code
// using REKT structural context (sections, perform graph, SQL) as the test-case
// inventory. Output bundle: test classes + fixture files + notes.

using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Agents.Infrastructure;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using System.Text.Json;
using AIChatMessage = Microsoft.Extensions.AI.ChatMessage;

namespace CobolToQuarkusMigration.Agents;

public sealed class GeneratedTestFile
{
    public string File { get; set; } = "";
    public string Code { get; set; } = "";
    public string Framework { get; set; } = "";
    public List<string> CoversMethods { get; set; } = new();
}

public sealed class GeneratedFixtureFile
{
    public string File { get; set; } = "";
    public string Content { get; set; } = "";
}

public sealed class TestSynthesisResult
{
    public string Program { get; set; } = "";
    public List<GeneratedTestFile> Tests { get; set; } = new();
    public List<GeneratedFixtureFile> Fixtures { get; set; } = new();
    public List<string> Notes { get; set; } = new();
}

public class TestSynthesizerAgent : AgentBase
{
    protected override string AgentName => "TestSynthesizerAgent";

    public TestSynthesizerAgent(IChatClient c, ILogger<TestSynthesizerAgent> l, string m,
        EnhancedLogger? el = null, ChatLogger? cl = null, RateLimiter? rl = null, AppSettings? s = null)
        : base(c, l, m, el, cl, rl, s) { }

    public TestSynthesizerAgent(ResponsesApiClient r, ILogger<TestSynthesizerAgent> l, string m,
        EnhancedLogger? el = null, ChatLogger? cl = null, RateLimiter? rl = null, AppSettings? s = null)
        : base(r, l, m, el, cl, rl, s) { }

    public async Task<TestSynthesisResult> SynthesizeAsync(
        StructuralContext sc, string convertedCode, string targetLanguage)
    {
        var result = new TestSynthesisResult { Program = sc.Program };
        if (string.IsNullOrWhiteSpace(convertedCode)) return result;

        var systemPrompt = PromptLoader.Load("TestSynthesizer", new Dictionary<string, string>
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
                    new(ChatRole.User, "Produce the JSON now."),
                };
                var options = new ChatOptions { ModelId = ModelId, MaxOutputTokens = 12288 };
                var r = await ChatClient.GetResponseAsync(messages, options);
                response = r.Text ?? "";
            }
            else if (ResponsesClient is not null)
            {
                response = await ResponsesClient.GetResponseAsync(systemPrompt, "Produce the JSON now.", maxOutputTokens: 12288);
            }
            else return result;

            var json = ExtractJsonObject(response);
            if (string.IsNullOrWhiteSpace(json)) return result;

            using var doc = JsonDocument.Parse(json);
            if (doc.RootElement.TryGetProperty("tests", out var tests) && tests.ValueKind == JsonValueKind.Array)
            {
                foreach (var t in tests.EnumerateArray())
                {
                    var gt = new GeneratedTestFile();
                    if (t.TryGetProperty("file",      out var f)  && f.ValueKind  == JsonValueKind.String) gt.File      = f.GetString() ?? "";
                    if (t.TryGetProperty("code",      out var co) && co.ValueKind == JsonValueKind.String) gt.Code      = co.GetString() ?? "";
                    if (t.TryGetProperty("framework", out var fr) && fr.ValueKind == JsonValueKind.String) gt.Framework = fr.GetString() ?? "";
                    if (t.TryGetProperty("coversMethods", out var cm) && cm.ValueKind == JsonValueKind.Array)
                        gt.CoversMethods = cm.EnumerateArray()
                            .Where(x => x.ValueKind == JsonValueKind.String)
                            .Select(x => x.GetString()!).ToList();
                    if (!string.IsNullOrEmpty(gt.File) && !string.IsNullOrEmpty(gt.Code)) result.Tests.Add(gt);
                }
            }
            if (doc.RootElement.TryGetProperty("fixtures", out var fxs) && fxs.ValueKind == JsonValueKind.Array)
            {
                foreach (var fx in fxs.EnumerateArray())
                {
                    var gf = new GeneratedFixtureFile();
                    if (fx.TryGetProperty("file",    out var f)  && f.ValueKind  == JsonValueKind.String) gf.File    = f.GetString() ?? "";
                    if (fx.TryGetProperty("content", out var co) && co.ValueKind == JsonValueKind.String) gf.Content = co.GetString() ?? "";
                    if (!string.IsNullOrEmpty(gf.File)) result.Fixtures.Add(gf);
                }
            }
            if (doc.RootElement.TryGetProperty("notes", out var notes) && notes.ValueKind == JsonValueKind.Array)
                result.Notes = notes.EnumerateArray()
                    .Where(x => x.ValueKind == JsonValueKind.String)
                    .Select(x => x.GetString()!).ToList();
        }
        catch (Exception ex)
        {
            Logger.LogWarning("TestSynthesizerAgent failed for {Program}: {Msg}", sc.Program, ex.Message);
        }
        return result;
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
