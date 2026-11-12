namespace McpChatWeb.Models;

public sealed record ChatHistoryMessage(string Role, string Content);

public sealed record ChatRequest(
    string Prompt,
    string? ReportContext = null,
    IReadOnlyList<ChatHistoryMessage>? History = null);

// ReportPath is the repository-relative report a report-mode answer was read from, resolved per
// request, so "latest" is labelled with the report actually used.
public sealed record ChatResponse(string Response, int? RunId = null, string? ReportPath = null);

public sealed record SwitchRunRequest(int RunId);
