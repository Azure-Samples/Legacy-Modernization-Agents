// DocumentationAgent.cs — Wraps converted code with JavaDoc / XML-doc comments
// using the structural context so every public method/class references its
// COBOL origin (section name, line range, SQL ops, CALL targets).
//
// Output is the same file content with doc comments inserted — no semantic edits.

using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Agents.Infrastructure;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using AIChatMessage = Microsoft.Extensions.AI.ChatMessage;

namespace CobolToQuarkusMigration.Agents;

public class DocumentationAgent : AgentBase
{
    protected override string AgentName => "DocumentationAgent";

    public DocumentationAgent(IChatClient c, ILogger<DocumentationAgent> l, string m,
        EnhancedLogger? el = null, ChatLogger? cl = null, RateLimiter? rl = null, AppSettings? s = null)
        : base(c, l, m, el, cl, rl, s) { }

    public DocumentationAgent(ResponsesApiClient r, ILogger<DocumentationAgent> l, string m,
        EnhancedLogger? el = null, ChatLogger? cl = null, RateLimiter? rl = null, AppSettings? s = null)
        : base(r, l, m, el, cl, rl, s) { }

    public async Task<string> DocumentAsync(
        StructuralContext sc, string convertedCode, string targetLanguage)
    {
        if (string.IsNullOrWhiteSpace(convertedCode)) return convertedCode;

        var systemPrompt = PromptLoader.Load("DocumentationAgent", new Dictionary<string, string>
        {
            ["TargetLanguage"] = targetLanguage,
            ["StructuralContext"] = RektContextFormatter.ToPromptBlock(sc),
            ["Code"] = convertedCode,
        });

        try
        {
            string response;
            if (ChatClient is not null)
            {
                var messages = new List<AIChatMessage>
                {
                    new(ChatRole.System, systemPrompt),
                    new(ChatRole.User, "Produce the fully-documented code now."),
                };
                var options = new ChatOptions { ModelId = ModelId, MaxOutputTokens = 16384 };
                var r = await ChatClient.GetResponseAsync(messages, options);
                response = r.Text ?? "";
            }
            else if (ResponsesClient is not null)
            {
                response = await ResponsesClient.GetResponseAsync(systemPrompt, "Produce the fully-documented code now.", maxOutputTokens: 16384);
            }
            else return convertedCode;

            // Strip accidental fences.
            var trimmed = response.Trim();
            if (trimmed.StartsWith("```"))
            {
                var firstNL = trimmed.IndexOf('\n');
                if (firstNL > 0) trimmed = trimmed.Substring(firstNL + 1);
                if (trimmed.EndsWith("```")) trimmed = trimmed.Substring(0, trimmed.Length - 3);
            }
            return string.IsNullOrWhiteSpace(trimmed) ? convertedCode : trimmed.TrimEnd();
        }
        catch (Exception ex)
        {
            Logger.LogWarning("DocumentationAgent failed for {Program}: {Msg}", sc.Program, ex.Message);
            return convertedCode;
        }
    }
}
