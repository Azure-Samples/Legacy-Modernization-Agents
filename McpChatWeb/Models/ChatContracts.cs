namespace McpChatWeb.Models;

public sealed record ChatRequest(string Prompt, string? ReportContext = null);

public sealed record ChatResponse(string Response, int? RunId = null);

public sealed record SwitchRunRequest(int RunId);
