using Azure.AI.OpenAI;
using Azure.Identity;
using Microsoft.Extensions.AI;
using OpenAI;
using System.Text.Json;

namespace McpChatWeb.Services;

/// <summary>
/// Creates IChatClient instances for Prompt Studio.
/// Uses the same SDK clients as mission control — handles Azure OpenAI,
/// GitHub Models, and Copilot SDK (falls back to GitHub Models REST API).
/// </summary>
public static class PromptStudioAI
{
    /// <summary>
    /// Creates an IChatClient from current environment/config, resolving the
    /// model and provider automatically. Returns null if no provider is available.
    /// </summary>
    public static (IChatClient? Client, string ModelUsed, string Error) CreateClient()
    {
        var activeModel = Environment.GetEnvironmentVariable("AZURE_OPENAI_MODEL_ID") ?? "";
        var serviceType = (Environment.GetEnvironmentVariable("AZURE_OPENAI_SERVICE_TYPE") ?? "AzureOpenAI").ToLowerInvariant();
        var apiKey = Environment.GetEnvironmentVariable("AZURE_OPENAI_API_KEY") ?? "";
        var azureEndpoint = Environment.GetEnvironmentVariable("AZURE_OPENAI_ENDPOINT") ?? "";

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

        // Copilot SDK → fall back to GitHub Models REST API
        if (serviceType == "githubcopilotsdk")
        {
            var ghToken = ResolveGitHubToken();
            if (!string.IsNullOrWhiteSpace(ghToken))
            {
                serviceType = "githubcopilot";
                apiKey = ghToken;
            }
            else
            {
                return (null, activeModel, "Copilot SDK — no GitHub token found. Run 'gh auth login'.");
            }
        }

        // No API key and no Azure Entra ID? Try GitHub token
        if (IsGitHubToken(apiKey) || string.IsNullOrWhiteSpace(apiKey) || apiKey.Contains("placeholder"))
        {
            if (serviceType == "azureopenai" && !string.IsNullOrWhiteSpace(azureEndpoint))
            {
                // Azure with Entra ID — clear the GitHub token, we'll use DefaultAzureCredential
                apiKey = "";
            }
            else
            {
                var ghToken = ResolveGitHubToken();
                if (!string.IsNullOrWhiteSpace(ghToken))
                {
                    serviceType = "githubcopilot";
                    apiKey = ghToken;
                }
            }
        }

        // Azure endpoint missing? Load from config
        if (serviceType == "azureopenai" && string.IsNullOrWhiteSpace(azureEndpoint))
        {
            azureEndpoint = ReadEndpointFromConfig();
            if (string.IsNullOrWhiteSpace(azureEndpoint))
            {
                // Last resort: try GitHub Models
                var ghToken = ResolveGitHubToken();
                if (!string.IsNullOrWhiteSpace(ghToken))
                {
                    serviceType = "githubcopilot";
                    apiKey = ghToken;
                }
                else
                {
                    return (null, activeModel, "No Azure endpoint configured and no GitHub token available.");
                }
            }
        }

        if (string.IsNullOrWhiteSpace(activeModel))
            return (null, "", "No AI model selected.");

        Console.WriteLine($"🔌 Prompt Studio AI: model='{activeModel}', provider='{serviceType}'");

        try
        {
            IChatClient client;

            if (serviceType is "githubcopilot" or "github" or "githubmodels")
            {
                // GitHub Models — OpenAI SDK pointed at models.github.ai
                var options = new OpenAIClientOptions
                {
                    Endpoint = new Uri("https://models.github.ai/inference")
                };
                var openaiClient = new OpenAIClient(
                    new System.ClientModel.ApiKeyCredential(apiKey), options);
                client = openaiClient.GetChatClient(activeModel).AsIChatClient();
            }
            else
            {
                // Azure OpenAI
                AzureOpenAIClient azureClient;
                if (!string.IsNullOrWhiteSpace(apiKey) && !IsGitHubToken(apiKey))
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
                client = azureClient.GetChatClient(activeModel).AsIChatClient();
            }

            return (client, activeModel, "");
        }
        catch (Exception ex)
        {
            return (null, activeModel, $"Failed to create AI client: {ex.Message}");
        }
    }

    private static bool IsGitHubToken(string key) =>
        key.StartsWith("gho_") || key.StartsWith("ghp_") || key.StartsWith("ghu_") || key.StartsWith("ghs_");

    private static string ResolveGitHubToken()
    {
        var envToken = Environment.GetEnvironmentVariable("GITHUB_TOKEN");
        if (!string.IsNullOrWhiteSpace(envToken) && !envToken.Contains("placeholder"))
            return envToken;

        try
        {
            var psi = new System.Diagnostics.ProcessStartInfo("gh", "auth token")
            {
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                CreateNoWindow = true
            };
            using var proc = System.Diagnostics.Process.Start(psi);
            if (proc != null)
            {
                var output = proc.StandardOutput.ReadToEnd().Trim();
                proc.WaitForExit(5000);
                if (proc.ExitCode == 0 && !string.IsNullOrWhiteSpace(output))
                    return output;
            }
        }
        catch { }

        return "";
    }

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
