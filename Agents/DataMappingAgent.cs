// DataMappingAgent.cs — Generates JPA / EF Core entities + repositories from
// REKT data structures and SQL statements. Output is a structured bundle that
// callers can write to disk under output/<lang>/<program>/persistence/.
// Only invoked for programs with non-zero SQL count.

using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Agents.Infrastructure;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using System.Text.Json;
using AIChatMessage = Microsoft.Extensions.AI.ChatMessage;

namespace CobolToQuarkusMigration.Agents;

public sealed class DataMappingArtifact
{
    public string File { get; set; } = "";
    public string TableName { get; set; } = "";
    public string Code { get; set; } = "";
}

public sealed class DataMappingResult
{
    public string Program { get; set; } = "";
    public List<DataMappingArtifact> Entities { get; set; } = new();
    public List<DataMappingArtifact> Repositories { get; set; } = new();
    public List<string> Notes { get; set; } = new();
}

public class DataMappingAgent : AgentBase
{
    protected override string AgentName => "DataMappingAgent";

    public DataMappingAgent(IChatClient c, ILogger<DataMappingAgent> l, string m,
        EnhancedLogger? el = null, ChatLogger? cl = null, RateLimiter? rl = null, AppSettings? s = null)
        : base(c, l, m, el, cl, rl, s) { }

    public DataMappingAgent(ResponsesApiClient r, ILogger<DataMappingAgent> l, string m,
        EnhancedLogger? el = null, ChatLogger? cl = null, RateLimiter? rl = null, AppSettings? s = null)
        : base(r, l, m, el, cl, rl, s) { }

    public async Task<DataMappingResult> GenerateAsync(
        StructuralContext sc, string cobolSource, string targetLanguage)
    {
        var result = new DataMappingResult { Program = sc.Program };
        // Skip programs with no SQL — DataMappingAgent is wasteful otherwise.
        if (sc.Context.SqlStatements.Count == 0)
        {
            Logger.LogDebug("[DataMapping] {Program} has no EXEC SQL — skipping", sc.Program);
            return result;
        }

        var systemPrompt = PromptLoader.Load("DataMapping", new Dictionary<string, string>
        {
            ["TargetLanguage"] = targetLanguage,
            ["StructuralContext"] = RektContextFormatter.ToPromptBlock(sc),
            ["CobolSource"] = cobolSource.Length > 40_000 ? cobolSource.Substring(0, 40_000) + "\n// [TRUNCATED]\n" : cobolSource,
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
            result.Entities = ReadArtifacts(doc.RootElement, "entities");
            result.Repositories = ReadArtifacts(doc.RootElement, "repositories");
            if (doc.RootElement.TryGetProperty("notes", out var notes) && notes.ValueKind == JsonValueKind.Array)
                result.Notes = notes.EnumerateArray()
                    .Where(x => x.ValueKind == JsonValueKind.String)
                    .Select(x => x.GetString()!)
                    .ToList();
        }
        catch (Exception ex)
        {
            Logger.LogWarning("DataMappingAgent failed for {Program}: {Msg}", sc.Program, ex.Message);
        }
        return result;
    }

    private static List<DataMappingArtifact> ReadArtifacts(JsonElement root, string property)
    {
        var list = new List<DataMappingArtifact>();
        if (!root.TryGetProperty(property, out var arr) || arr.ValueKind != JsonValueKind.Array) return list;
        foreach (var e in arr.EnumerateArray())
        {
            var a = new DataMappingArtifact();
            if (e.TryGetProperty("file",      out var f)  && f.ValueKind  == JsonValueKind.String) a.File      = f.GetString()  ?? "";
            if (e.TryGetProperty("tableName", out var t)  && t.ValueKind  == JsonValueKind.String) a.TableName = t.GetString()  ?? "";
            if (e.TryGetProperty("code",      out var co) && co.ValueKind == JsonValueKind.String) a.Code      = co.GetString() ?? "";
            if (!string.IsNullOrEmpty(a.File) && !string.IsNullOrEmpty(a.Code)) list.Add(a);
        }
        return list;
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
