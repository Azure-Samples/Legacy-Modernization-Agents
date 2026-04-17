using CobolToQuarkusMigration.Agents.Infrastructure;
using CobolToQuarkusMigration.Models;
using Microsoft.Extensions.AI;
using System.Text.Json;

namespace McpChatWeb.Services;

/// <summary>
/// Creates IChatClient instances for Prompt Studio by delegating to
/// <see cref="ChatClientFactory"/>. Resolves provider, endpoint, and model
/// from environment variables and config files.
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
        var serviceType = Environment.GetEnvironmentVariable("AZURE_OPENAI_SERVICE_TYPE") ?? "AzureOpenAI";
        var apiKey = Environment.GetEnvironmentVariable("AZURE_OPENAI_API_KEY") ?? "";
        var endpoint = Environment.GetEnvironmentVariable("AZURE_OPENAI_ENDPOINT") ?? "";

        if (string.IsNullOrWhiteSpace(activeModel))
            return (null, "", "No AI model selected. Use 🔧 Setup in the portal to connect a provider.");

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

        // Resolve endpoint from config if not in env
        if (string.IsNullOrWhiteSpace(endpoint))
            endpoint = ReadEndpointFromConfig();

        // Clear GitHub/placeholder tokens for Azure — Entra ID handles auth
        if (serviceType.Equals("AzureOpenAI", StringComparison.OrdinalIgnoreCase) &&
            (IsGitHubToken(apiKey) || apiKey.Contains("placeholder")))
            apiKey = "";

        Console.WriteLine($"🔌 Prompt Studio AI: model='{activeModel}', provider='{serviceType}'");

        var settings = new AISettings
        {
            ServiceType = serviceType,
            Endpoint = endpoint,
            ApiKey = apiKey,
            ModelId = activeModel,
            DeploymentName = activeModel
        };

        try
        {
            var client = ChatClientFactory.CreateFromSettings(settings, activeModel);
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
        var envChat = Environment.GetEnvironmentVariable("AZURE_OPENAI_CHAT_MODEL_ID") ?? "";

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
