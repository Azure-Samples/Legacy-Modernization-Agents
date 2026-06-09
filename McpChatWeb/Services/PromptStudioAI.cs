using CobolToQuarkusMigration.Agents.Infrastructure;
using CobolToQuarkusMigration.Models;
using Microsoft.Extensions.AI;
using System.Text.Json;

namespace McpChatWeb.Services;

/// <summary>
/// Creates IChatClient instances for web-side AI calls by resolving provider,
/// endpoint, key, and model from environment variables and Config/appsettings.json.
/// </summary>
public static class PromptStudioAI
{
    private static bool _envLoaded;

    public static (IChatClient? Client, string ModelUsed, string Error) CreateClient()
    {
        LoadEnvFilesOnce();
        var configSettings = ReadAISettingsFromConfig();

        var activeModel = FirstNonEmpty(
            Environment.GetEnvironmentVariable("AZURE_OPENAI_CHAT_MODEL_ID"),
            Environment.GetEnvironmentVariable("AISETTINGS__CHATMODELID"),
            configSettings?.ChatModelId,
            configSettings?.ChatDeploymentName,
            Environment.GetEnvironmentVariable("AZURE_OPENAI_MODEL_ID"),
            Environment.GetEnvironmentVariable("AISETTINGS__MODELID"),
            configSettings?.ModelId) ?? "";
        var serviceType = FirstNonEmpty(
            Environment.GetEnvironmentVariable("AZURE_OPENAI_SERVICE_TYPE"),
            Environment.GetEnvironmentVariable("AISETTINGS__SERVICETYPE"),
            configSettings?.ServiceType,
            "AzureOpenAI")!;
        var apiKey = FirstNonEmpty(
            Environment.GetEnvironmentVariable("AZURE_OPENAI_CHAT_API_KEY"),
            Environment.GetEnvironmentVariable("AISETTINGS__CHATAPIKEY"),
            configSettings?.ChatApiKey,
            Environment.GetEnvironmentVariable("AZURE_OPENAI_API_KEY"),
            Environment.GetEnvironmentVariable("AISETTINGS__APIKEY"),
            configSettings?.ApiKey) ?? "";
        var endpoint = FirstNonEmpty(
            Environment.GetEnvironmentVariable("AZURE_OPENAI_CHAT_ENDPOINT"),
            Environment.GetEnvironmentVariable("AISETTINGS__CHATENDPOINT"),
            configSettings?.ChatEndpoint,
            Environment.GetEnvironmentVariable("AZURE_OPENAI_ENDPOINT"),
            Environment.GetEnvironmentVariable("AISETTINGS__ENDPOINT"),
            configSettings?.Endpoint) ?? "";

        if (string.IsNullOrWhiteSpace(activeModel))
            return (null, "", "No AI model selected. Use Setup in the portal to connect a provider.");

        if (activeModel.Contains("codex", StringComparison.OrdinalIgnoreCase))
        {
            var chatModel = FirstNonEmpty(configSettings?.ChatModelId, Environment.GetEnvironmentVariable("AZURE_OPENAI_CHAT_MODEL_ID"));
            if (!string.IsNullOrWhiteSpace(chatModel) && !chatModel.Contains("codex", StringComparison.OrdinalIgnoreCase))
            {
                Console.WriteLine($"Codex model '{activeModel}' -> using chat model '{chatModel}' for web AI calls");
                activeModel = chatModel;
            }
        }

        if (serviceType.Equals("AzureOpenAI", StringComparison.OrdinalIgnoreCase) &&
            (IsGitHubToken(apiKey) || apiKey.Contains("placeholder", StringComparison.OrdinalIgnoreCase)))
        {
            apiKey = "";
        }

        Console.WriteLine($"Web AI client: model='{activeModel}', provider='{serviceType}', endpoint='{endpoint}'");

        var settings = new AISettings
        {
            ServiceType = serviceType,
            Endpoint = endpoint,
            ApiKey = apiKey,
            ModelId = activeModel,
            DeploymentName = activeModel,
            ChatEndpoint = endpoint,
            ChatApiKey = apiKey,
            ChatModelId = activeModel,
            ChatDeploymentName = activeModel
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

    private static string? FirstNonEmpty(params string?[] values) =>
        values.FirstOrDefault(value => !string.IsNullOrWhiteSpace(value));

    private static AISettings? ReadAISettingsFromConfig()
    {
        try
        {
            var settingsPath = FindSettingsPath();
            if (settingsPath == null) return null;

            using var doc = JsonDocument.Parse(File.ReadAllText(settingsPath));
            if (!doc.RootElement.TryGetProperty("AISettings", out var aiSettingsElement))
                return null;

            string GetString(string name) =>
                aiSettingsElement.TryGetProperty(name, out var value) ? value.GetString() ?? "" : "";

            return new AISettings
            {
                ServiceType = FirstNonEmpty(GetString("ServiceType"), "AzureOpenAI")!,
                Endpoint = GetString("Endpoint"),
                ApiKey = GetString("ApiKey"),
                ModelId = GetString("ModelId"),
                DeploymentName = GetString("DeploymentName"),
                ChatEndpoint = GetString("ChatEndpoint"),
                ChatApiKey = GetString("ChatApiKey"),
                ChatModelId = GetString("ChatModelId"),
                ChatDeploymentName = GetString("ChatDeploymentName")
            };
        }
        catch
        {
            return null;
        }
    }

    private static void LoadEnvFilesOnce()
    {
        if (_envLoaded) return;
        _envLoaded = true;

        foreach (var path in FindEnvFilePaths())
        {
            if (File.Exists(path))
                LoadEnvFile(path);
        }
    }

    private static IEnumerable<string> FindEnvFilePaths()
    {
        var settingsPath = FindSettingsPath();
        var configDir = settingsPath == null ? null : Path.GetDirectoryName(settingsPath);
        if (configDir == null) yield break;

        yield return Path.Combine(configDir, "ai-config.local.env");
        yield return Path.Combine(configDir, "ai-config.env");
    }

    private static void LoadEnvFile(string filePath)
    {
        var fileVars = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);

        foreach (var rawLine in File.ReadAllLines(filePath))
        {
            var line = rawLine.Trim();
            if (string.IsNullOrWhiteSpace(line) || line.StartsWith("#"))
                continue;

            var parts = line.Split('=', 2);
            if (parts.Length != 2)
                continue;

            var key = parts[0].Trim();
            var value = parts[1].Trim().Trim('"', '\'');
            fileVars[key] = value;

            if (value.Contains('$'))
                value = ExpandVariables(value, fileVars);

            Environment.SetEnvironmentVariable(key, value);
        }
    }

    private static string ExpandVariables(string value, Dictionary<string, string> fileVars)
    {
        foreach (var kvp in fileVars)
        {
            value = value.Replace($"${{{kvp.Key}}}", kvp.Value)
                .Replace($"${kvp.Key}", kvp.Value);
        }

        foreach (System.Collections.DictionaryEntry env in Environment.GetEnvironmentVariables())
        {
            var key = env.Key?.ToString();
            var envValue = env.Value?.ToString();
            if (string.IsNullOrEmpty(key) || envValue == null)
                continue;

            value = value.Replace($"${{{key}}}", envValue)
                .Replace($"${key}", envValue);
        }

        return value;
    }

    private static string? FindSettingsPath()
    {
        var candidates = new[]
        {
            Path.Combine(Directory.GetCurrentDirectory(), "..", "Config", "appsettings.json"),
            Path.Combine(Directory.GetCurrentDirectory(), "Config", "appsettings.json"),
            Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, "..", "..", "..", "..", "Config", "appsettings.json"))
        };

        foreach (var path in candidates)
        {
            if (File.Exists(path)) return path;
        }

        return null;
    }
}
