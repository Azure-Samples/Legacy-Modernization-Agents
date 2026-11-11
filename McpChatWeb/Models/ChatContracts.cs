namespace McpChatWeb.Models;

public sealed record ChatHistoryMessage(string Role, string Content);

public sealed record ChatRequest(
    string Prompt,
    string? ReportContext = null,
    IReadOnlyList<ChatHistoryMessage>? History = null);

public sealed record ChatResponse(string Response, int? RunId = null);

public sealed record SwitchRunRequest(int RunId);
