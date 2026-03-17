using Azure.AI.OpenAI;
using Azure.Identity;
using GitHub.Copilot.SDK;
using Microsoft.Extensions.AI;
using System.Text.Json;

namespace McpChatWeb.Services;

/// <summary>
/// Creates IChatClient instances for Prompt Studio.
/// Supports Azure OpenAI (API key or Entra ID) and GitHub Copilot SDK.
/// Uses the active model selected in the portal setup modal.
/// </summary>
public static class PromptStudioAI
{
    /// <summary>
    /// Creates an IChatClient from current environment/config, resolving the
    /// model and provider automatically. Returns null if no provider is available.
    /// </summary>
    public static (IChatClient? Client, string ModelUsed, string Error) CreateClient()
    {
        var activeModel = Environment.GetEnvironmentVariable("AZURE_OPENAI_CHAT_MODEL_ID")
            ?? Environment.GetEnvironmentVariable("AZURE_OPENAI_MODEL_ID") ?? "";
        var serviceType = (Environment.GetEnvironmentVariable("AZURE_OPENAI_SERVICE_TYPE") ?? "AzureOpenAI").ToLowerInvariant();
        var apiKey = Environment.GetEnvironmentVariable("AZURE_OPENAI_API_KEY") ?? "";
        var azureEndpoint = Environment.GetEnvironmentVariable("AZURE_OPENAI_ENDPOINT") ?? "";

        if (string.IsNullOrWhiteSpace(activeModel))
            return (null, "", "No AI model selected. Use 🔧 Setup in the portal to connect a provider.");

        // ── GitHub Copilot SDK ──────────────────────────────────────────────
        if (serviceType is "githubcopilotsdk" or "githubcopilot")
        {
            // Codex models don't support Chat Completions — swap to chat model
            if (activeModel.Contains("codex", StringComparison.OrdinalIgnoreCase))
            {
                var chatModel = ReadChatModelFromConfig();
                if (!string.IsNullOrWhiteSpace(chatModel) && !chatModel.Contains("codex", StringComparison.OrdinalIgnoreCase))
                {
                    Console.WriteLine($"🔄 Codex model '{activeModel}' → using chat model '{chatModel}' for Prompt Studio");
                    activeModel = chatModel;
                }
            }

            Console.WriteLine($"🔌 Prompt Studio AI: model='{activeModel}', provider='GitHubCopilotSDK'");

            try
            {
                var options = new CopilotClientOptions { UseStdio = true };

                // Use PAT if configured
                var copilotToken = Environment.GetEnvironmentVariable("GITHUB_COPILOT_TOKEN") ?? "";
                if (!string.IsNullOrWhiteSpace(copilotToken))
                    options.GitHubToken = copilotToken;

                var client = new CopilotChatClient(activeModel, options);
                return (client, activeModel, "");
            }
            catch (Exception ex)
            {
                return (null, activeModel, $"Failed to create Copilot SDK client: {ex.Message}. Ensure 'gh auth login' or a PAT is configured.");
            }
        }

        // ── Azure OpenAI ────────────────────────────────────────────────────

        // Codex models don't support Chat Completions — swap to chat model
        if (serviceType == "azureopenai" && activeModel.Contains("codex", StringComparison.OrdinalIgnoreCase))
        {
            var chatModel = ReadChatModelFromConfig();
            if (!string.IsNullOrWhiteSpace(chatModel) && !chatModel.Contains("codex", StringComparison.OrdinalIgnoreCase))
            {
                Console.WriteLine($"🔄 Codex model '{activeModel}' → using chat model '{chatModel}' for Prompt Studio");
                activeModel = chatModel;
            }
        }

        // Resolve endpoint from env or config
        if (string.IsNullOrWhiteSpace(azureEndpoint))
            azureEndpoint = ReadEndpointFromConfig();

        if (string.IsNullOrWhiteSpace(azureEndpoint) || azureEndpoint.Contains("placeholder"))
            return (null, activeModel, "No Azure endpoint configured. Use 🔧 Setup in the portal to connect Azure OpenAI.");

        // Clear GitHub/placeholder tokens — use Entra ID for Azure
        if (IsGitHubToken(apiKey) || string.IsNullOrWhiteSpace(apiKey) || apiKey.Contains("placeholder"))
            apiKey = "";

        Console.WriteLine($"🔌 Prompt Studio AI: model='{activeModel}', provider='AzureOpenAI'");

        try
        {
            AzureOpenAIClient azureClient;
            if (!string.IsNullOrWhiteSpace(apiKey))
            {
                azureClient = new AzureOpenAIClient(
                    new Uri(azureEndpoint),
                    new System.ClientModel.ApiKeyCredential(apiKey));
            }
            else
            {
                // Entra ID auth
                azureClient = new AzureOpenAIClient(
                    new Uri(azureEndpoint),
                    new DefaultAzureCredential());
            }

            var client = azureClient.GetChatClient(activeModel).AsIChatClient();

            return (client, activeModel, "");
        }
        catch (Exception ex)
        {
            return (null, activeModel, $"Failed to create AI client: {ex.Message}");
        }
    }

    private static bool IsGitHubToken(string key) =>
        key.StartsWith("gho_") || key.StartsWith("ghp_") || key.StartsWith("ghu_") || key.StartsWith("ghs_");

    private static string ReadChatModelFromConfig()
    {
        // Try env var first
        var envChat = Environment.GetEnvironmentVariable("AZURE_OPENAI_CHAT_MODEL_ID") ?? "";
        
        // Read from appsettings.json
        try
        {
            var settingsPath = FindSettingsPath();
            if (settingsPath != null)
            {
                using var doc = JsonDocument.Parse(File.ReadAllText(settingsPath));
                var configChat = doc.RootElement.GetProperty("AISettings").GetProperty("ChatModelId").GetString() ?? "";
                if (!string.IsNullOrWhiteSpace(configChat) && !configChat.Contains("codex", StringComparison.OrdinalIgnoreCase))
                    return configChat;
            }
        }
        catch { }

        // Fall back to env var if it's not a codex model
        if (!string.IsNullOrWhiteSpace(envChat) && !envChat.Contains("codex", StringComparison.OrdinalIgnoreCase))
            return envChat;

        return "";
    }

    private static string ReadEndpointFromConfig()
    {
        try
        {
            var settingsPath = FindSettingsPath();
            if (settingsPath != null)
            {
                using var doc = JsonDocument.Parse(File.ReadAllText(settingsPath));
                return doc.RootElement.GetProperty("AISettings").GetProperty("Endpoint").GetString() ?? "";
            }
        }
        catch { }
        return "";
    }

    private static string? FindSettingsPath()
    {
        var candidates = new[]
        {
            Path.Combine(Directory.GetCurrentDirectory(), "..", "Config", "appsettings.json"),
            Path.Combine(Directory.GetCurrentDirectory(), "Config", "appsettings.json")
        };
        foreach (var p in candidates)
        {
            if (File.Exists(p)) return p;
        }
        return null;
    }
}
