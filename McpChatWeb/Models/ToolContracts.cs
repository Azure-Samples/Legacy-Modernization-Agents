namespace McpChatWeb.Models;

public record ToolCallRequest(string Name, Dictionary<string, object>? Arguments);
