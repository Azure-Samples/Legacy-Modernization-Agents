// StructuralExtractorAgent.cs — LLM-based extractor that produces REKT-shaped JSON
// from raw COBOL source when REKT itself can't parse the file (deps-only output or
// total parse failure). Used by StructuralContextProvider as a last-resort fallback;
// results are cached to output/rekt/llm-derived/.

using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Agents.Infrastructure;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using System.Text.Json;
using AIChatMessage = Microsoft.Extensions.AI.ChatMessage;

namespace CobolToQuarkusMigration.Agents;

public class StructuralExtractorAgent : AgentBase
{
    protected override string AgentName => "StructuralExtractorAgent";

    public StructuralExtractorAgent(
        IChatClient chatClient,
        ILogger<StructuralExtractorAgent> logger,
        string modelId,
        EnhancedLogger? enhancedLogger = null,
        ChatLogger? chatLogger = null,
        RateLimiter? rateLimiter = null,
        AppSettings? settings = null)
        : base(chatClient, logger, modelId, enhancedLogger, chatLogger, rateLimiter, settings)
    {
    }

    public StructuralExtractorAgent(
        ResponsesApiClient responsesClient,
        ILogger<StructuralExtractorAgent> logger,
        string modelId,
        EnhancedLogger? enhancedLogger = null,
        ChatLogger? chatLogger = null,
        RateLimiter? rateLimiter = null,
        AppSettings? settings = null)
        : base(responsesClient, logger, modelId, enhancedLogger, chatLogger, rateLimiter, settings)
    {
    }

    /// <summary>
    /// Extracts a REKT-shaped structure from raw COBOL source. Returns null on
    /// unrecoverable failure (timeout, malformed JSON after one retry).
    /// </summary>
    public async Task<RektContext?> ExtractAsync(string programFileName, string source)
    {
        var lineCount = source.Count(c => c == '\n') + 1;

        // Truncate massively large programs — we just need structure, not every line.
        var truncated = source;
        if (source.Length > 60_000)
        {
            truncated = source.Substring(0, 60_000)
                      + "\n\n*  [TRUNCATED — file is " + source.Length + " chars; first 60 KB shown for structural extraction]\n";
        }

        var systemPrompt = PromptLoader.Load("StructuralExtractor", new Dictionary<string, string>
        {
            ["Program"] = programFileName,
            ["LineCount"] = lineCount.ToString(),
            ["Source"] = truncated,
        });

        try
        {
            var response = await CallChatAsync(systemPrompt, "Produce the JSON now.");
            var json = ExtractJsonBlock(response);
            if (string.IsNullOrWhiteSpace(json)) return null;

            var ctx = JsonSerializer.Deserialize<RektContext>(json, new JsonSerializerOptions
            {
                PropertyNameCaseInsensitive = true,
            });
            if (ctx is null) return null;
            ctx.Program = programFileName;
            ctx.LineCount = lineCount;
            return ctx;
        }
        catch (Exception ex)
        {
            Logger.LogWarning("StructuralExtractorAgent failed for {Program}: {Message}", programFileName, ex.Message);
            return null;
        }
    }

    private async Task<string> CallChatAsync(string systemPrompt, string userPrompt)
    {
        if (ChatClient is not null)
        {
            var messages = new List<AIChatMessage>
            {
                new(ChatRole.System, systemPrompt),
                new(ChatRole.User, userPrompt),
            };
            var options = new ChatOptions { ModelId = ModelId, MaxOutputTokens = 8192 };
            var result = await ChatClient.GetResponseAsync(messages, options);
            return result.Text ?? "";
        }
        if (ResponsesClient is not null)
        {
            // ResponsesApiClient.GetResponseAsync returns the response text directly.
            return await ResponsesClient.GetResponseAsync(systemPrompt, userPrompt, maxOutputTokens: 8192);
        }
        return "";
    }

    private static string ExtractJsonBlock(string text)
    {
        if (string.IsNullOrEmpty(text)) return "";
        // Try to find a fenced ```json block first.
        var fenceStart = text.IndexOf("```json", StringComparison.OrdinalIgnoreCase);
        if (fenceStart < 0) fenceStart = text.IndexOf("```");
        if (fenceStart >= 0)
        {
            var bodyStart = text.IndexOf('\n', fenceStart) + 1;
            var fenceEnd = text.IndexOf("```", bodyStart);
            if (fenceEnd > bodyStart) return text.Substring(bodyStart, fenceEnd - bodyStart).Trim();
        }
        // Otherwise grab from first '{' to matching '}'.
        var firstBrace = text.IndexOf('{');
        var lastBrace = text.LastIndexOf('}');
        return firstBrace >= 0 && lastBrace > firstBrace
            ? text.Substring(firstBrace, lastBrace - firstBrace + 1)
            : "";
    }
}
