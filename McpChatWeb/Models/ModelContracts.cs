namespace McpChatWeb.Models;

public sealed record ModelInfo(
    string Id,
    string Name,
    string Publisher,
    string Family,
    string? Description = null,
    int? ContextWindow = null
);

public sealed record SetActiveModelRequest(string ModelId);

// ── Model Discovery & Setup Contracts ────────────────────────────────────────

public sealed record ConnectProviderRequest(
    string ServiceType,            // "AzureOpenAI" | "GitHubCopilotSDK"
    string? Endpoint = null,       // Azure OpenAI endpoint URL
    string? ApiKey = null,         // Azure API key or GitHub PAT
    bool UseDefaultCredential = false  // true = use az login / gh auth login
);

public sealed record SaveModelConfigRequest(
    string ServiceType,            // "AzureOpenAI" | "GitHubCopilotSDK"
    string? Endpoint = null,
    string? ApiKey = null,
    bool UseDefaultCredential = false,
    string? ChatModelId = null,
    string? CodeModelId = null
);

public sealed record PromptInfo(
    string Id,
    string Name,
    string SystemPrompt,
    string UserPromptTemplate,
    bool Enabled,
    int QualityScore = 0,
    string Observations = ""
);

public sealed record UpdatePromptRequest(
    string Id,
    string? SystemPrompt = null,
    string? UserPromptTemplate = null,
    bool? Enabled = null
);

public sealed record GeneratePromptRequest(string PromptId);

public sealed record SourceFileInfo(
    string FileName,
    string FileType,
    int LineCount,
    long SizeBytes,
    string RelativePath
);

// ── Run Management Contracts ─────────────────────────────────────────────────

public sealed record StartRunRequest(
    string Command,        // "migrate", "reverse-engineer", "convert-only", "resume"
    string? Name = null,
    string TargetLanguage = "Java",
    string SpeedProfile = "balanced",
    string? SourceFolder = null,
    string Provider = "AzureOpenAI",  // "AzureOpenAI", "GitHubModels", "CopilotSDK"
    string? ModelId = null             // e.g. "openai/gpt-4o", "claude-opus-4", "gpt-5.3-codex"
);

public sealed record StopRunRequest(string RunId);

public sealed record RunStatusDto(
    string RunId,
    string Name,
    string Command,
    string TargetLanguage,
    string SpeedProfile,
    string Status,
    DateTime StartedAt,
    DateTime? CompletedAt,
    int? ExitCode,
    int? ProcessId
);

// ── File Upload Contracts ────────────────────────────────────────────────────

public sealed record FolderContentsDto(
    string Path,
    List<FolderItemDto> Items,
    int TotalFiles,
    long TotalSizeBytes
);

public sealed record FolderItemDto(
    string Name,
    string Type,          // "file" or "directory"
    string RelativePath,
    long SizeBytes,
    int? LineCount,
    DateTime LastModified
);
