using Azure.AI.OpenAI;
using Azure.Identity;
using Azure.Core;
using GitHub.Copilot;
using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging;
using OpenAI;
using CobolToQuarkusMigration.Models;
using AzureOpenAIOptions = Azure.AI.OpenAI.AzureOpenAIClientOptions;
using AzureServiceVersion = Azure.AI.OpenAI.AzureOpenAIClientOptions.ServiceVersion;

namespace CobolToQuarkusMigration.Agents.Infrastructure;

/// <summary>
/// Factory for creating IChatClient instances for multiple AI providers:
///   - Azure OpenAI (existing)
///   - GitHub Copilot / GitHub Models (new — access Claude, Codex, Grok, GPT, etc.)
///   - Direct OpenAI API
///
/// All methods return Microsoft.Extensions.AI.IChatClient, keeping the rest
/// of the application provider-agnostic.
/// </summary>
public static class ChatClientFactory
{
    private static readonly AzureServiceVersion AzureApiVersion = AzureServiceVersion.V2024_06_01;

    // ═══════════════════════════════════════════════════════════════════════
    // PRIMARY FACTORY — Creates the right client based on AISettings
    // ═══════════════════════════════════════════════════════════════════════

    /// <summary>
    /// Creates an IChatClient based on the configured service type in AISettings.
    /// This is the recommended entry point — it auto-selects the right provider.
    /// </summary>
    /// <param name="settings">The AI settings with provider config.</param>
    /// <param name="modelId">Model ID override (uses settings.ModelId if null).</param>
    /// <param name="logger">Optional logger.</param>
    /// <returns>An IChatClient instance for the configured provider.</returns>
    public static IChatClient CreateFromSettings(
        AISettings settings,
        string? modelId = null,
        ILogger? logger = null)
    {
        var model = modelId ?? settings.ModelId;
        var serviceType = settings.ServiceType?.Trim() ?? "AzureOpenAI";

        return serviceType.ToLowerInvariant() switch
        {
            // GitHub Copilot SDK: authenticates via logged-in Copilot CLI user
            "githubcopilotsdk" or "githubcopilot" =>
                CreateGitHubCopilotChatClient(model, logger: logger),

            _ => // AzureOpenAI or anything with an endpoint
                CreateAzureClient(settings, model, logger)
        };
    }

    /// <summary>
    /// Creates an IChatClient for the chat/report model (uses ChatEndpoint/ChatApiKey if set).
    /// </summary>
    public static IChatClient CreateChatClientFromSettings(
        AISettings settings,
        ILogger? logger = null)
    {
        var chatEndpoint = settings.ChatEndpoint ?? settings.Endpoint;
        var chatApiKey = settings.ChatApiKey ?? settings.ApiKey;
        var chatModel = settings.ChatModelId ?? settings.ChatDeploymentName ?? settings.ModelId;
        var serviceType = settings.ServiceType?.Trim() ?? "AzureOpenAI";

        // Route through CreateFromSettings which handles AzureOpenAI and CopilotSDK
        var chatSettings = new AISettings
        {
            ServiceType = serviceType,
            Endpoint = chatEndpoint,
            ApiKey = chatApiKey,
            ModelId = chatModel,
            DeploymentName = settings.ChatDeploymentName ?? settings.DeploymentName
        };

        return CreateFromSettings(chatSettings, chatModel, logger);
    }

    // ═══════════════════════════════════════════════════════════════════════
    // AZURE OPENAI
    // ═══════════════════════════════════════════════════════════════════════

    /// <summary>
    /// Creates an IChatClient for Azure OpenAI using API key authentication.
    /// </summary>
    public static IChatClient CreateAzureOpenAIChatClient(
        string endpoint,
        string apiKey,
        string modelId,
        ILogger? logger = null)
    {
        if (string.IsNullOrEmpty(endpoint))
            throw new ArgumentNullException(nameof(endpoint));
        if (string.IsNullOrEmpty(apiKey))
            throw new ArgumentNullException(nameof(apiKey));
        if (string.IsNullOrEmpty(modelId))
            throw new ArgumentNullException(nameof(modelId));

        logger?.LogInformation("Creating Azure OpenAI chat client for endpoint: {Endpoint}, model: {Model}",
            endpoint, modelId);

        var client = new AzureOpenAIClient(
            new Uri(endpoint),
            new System.ClientModel.ApiKeyCredential(apiKey),
            CreateAzureOptions());

        return client.GetChatClient(modelId).AsIChatClient();
    }

    /// <summary>
    /// Creates an IChatClient for Azure OpenAI using a TokenCredential (e.g. DefaultAzureCredential).
    /// </summary>
    public static IChatClient CreateAzureOpenAIChatClient(
        string endpoint,
        TokenCredential credential,
        string modelId,
        ILogger? logger = null)
    {
        if (string.IsNullOrEmpty(endpoint))
            throw new ArgumentNullException(nameof(endpoint));
        if (credential == null)
            throw new ArgumentNullException(nameof(credential));
        if (string.IsNullOrEmpty(modelId))
            throw new ArgumentNullException(nameof(modelId));

        logger?.LogInformation("Creating Azure OpenAI chat client with TokenCredential for endpoint: {Endpoint}, model: {Model}",
            endpoint, modelId);

        var client = new AzureOpenAIClient(
            new Uri(endpoint),
            credential,
            CreateAzureOptions());

        return client.GetChatClient(modelId).AsIChatClient();
    }

    /// <summary>
    /// Creates an IChatClient for Azure OpenAI using DefaultAzureCredential (managed identity, etc.).
    /// </summary>
    public static IChatClient CreateAzureOpenAIChatClientWithDefaultCredential(
        string endpoint,
        string modelId,
        ILogger? logger = null)
    {
        return CreateAzureOpenAIChatClient(endpoint, new DefaultAzureCredential(), modelId, logger);
    }

    // ═══════════════════════════════════════════════════════════════════════
    // DIRECT OPENAI
    // ═══════════════════════════════════════════════════════════════════════

    /// <summary>
    /// Creates an IChatClient for OpenAI (not Azure).
    /// </summary>
    public static IChatClient CreateOpenAIChatClient(
        string apiKey,
        string modelId,
        ILogger? logger = null)
    {
        if (string.IsNullOrEmpty(apiKey))
            throw new ArgumentNullException(nameof(apiKey));
        if (string.IsNullOrEmpty(modelId))
            throw new ArgumentNullException(nameof(modelId));

        logger?.LogInformation("Creating OpenAI chat client for model: {Model}", modelId);

        var client = new OpenAIClient(apiKey);
        return client.GetChatClient(modelId).AsIChatClient();
    }

    // ═══════════════════════════════════════════════════════════════════════
    // GITHUB COPILOT SDK (Copilot CLI)
    // ═══════════════════════════════════════════════════════════════════════

    // Single shared limiter for the GitHub provider. Conservative defaults —
    // GitHub Models tends to be stricter than Azure. Override values are not
    // currently exposed via config; tracked in docs/throttling-and-cache-design.md §6.
    private const int GitHubTokensPerMinute = 200_000;
    private const int GitHubRequestsPerMinute = 60;
    private static readonly Lazy<RateLimitTracker> GitHubLimiter = new(() =>
        new RateLimitTracker(GitHubTokensPerMinute, GitHubRequestsPerMinute, logger: null));

    /// <summary>
    /// Creates an IChatClient for GitHub Copilot SDK.
    /// Requires the Copilot CLI in PATH.
    /// </summary>
    public static IChatClient CreateGitHubCopilotChatClient(
        string modelId,
        string? githubToken = null,
        ILogger? logger = null)
    {
        if (string.IsNullOrEmpty(modelId))
            throw new ArgumentNullException(nameof(modelId));

        logger?.LogInformation("Creating GitHub Copilot SDK chat client for model: {Model}", modelId);

        var options = new CopilotClientOptions
        {
            Mode = CopilotClientMode.CopilotCli
        };

        if (!string.IsNullOrEmpty(githubToken))
        {
            options.GitHubToken = githubToken;
        }

        // Pass the shared GitHub limiter so concurrent CopilotChatClient
        // instances share a single TPM/RPM bucket and adaptive cooldown.
        // App logger is used for our structured throttling logs only, not piped
        // into the SDK (that would flood console with JSON-RPC traces).
        return new CopilotChatClient(modelId, options, logger, limiter: GitHubLimiter.Value);
    }

    // ═══════════════════════════════════════════════════════════════════════
    // GENERIC / BACKWARD-COMPATIBLE
    // ═══════════════════════════════════════════════════════════════════════

    /// <summary>
    /// Creates an IChatClient by routing to Azure OpenAI or GitHub Copilot SDK based on serviceType.
    /// Kept for backward compatibility — prefer CreateFromSettings for new code.
    /// </summary>
    public static IChatClient CreateChatClient(
        string? endpoint,
        string apiKey,
        string modelId,
        bool useDefaultCredential = false,
        ILogger? logger = null,
        string? serviceType = null)
    {
        if (string.Equals(serviceType, "GitHubCopilot", StringComparison.OrdinalIgnoreCase) ||
            string.Equals(serviceType, "GitHubCopilotSDK", StringComparison.OrdinalIgnoreCase))
        {
            return CreateGitHubCopilotChatClient(modelId, githubToken: null, logger);
        }

        if (!string.IsNullOrEmpty(endpoint))
        {
            if (useDefaultCredential)
                return CreateAzureOpenAIChatClientWithDefaultCredential(endpoint, modelId, logger);
            return CreateAzureOpenAIChatClient(endpoint, apiKey, modelId, logger);
        }

        return CreateOpenAIChatClient(apiKey, modelId, logger);
    }

    // ═══════════════════════════════════════════════════════════════════════
    // HELPERS
    // ═══════════════════════════════════════════════════════════════════════

    private static IChatClient CreateAzureClient(
        AISettings settings,
        string modelId,
        ILogger? logger)
    {
        var endpoint = settings.Endpoint;
        var apiKey = settings.ApiKey;
        var deployment = !string.IsNullOrEmpty(settings.DeploymentName)
            ? settings.DeploymentName
            : modelId;

        bool useEntraId = string.IsNullOrEmpty(apiKey) || apiKey.Contains("your-api-key") || apiKey.Contains("placeholder");

        if (useEntraId)
            return CreateAzureOpenAIChatClientWithDefaultCredential(endpoint, deployment, logger);
        
        return CreateAzureOpenAIChatClient(endpoint, apiKey, deployment, logger);
    }

    private static AzureOpenAIOptions CreateAzureOptions() => new AzureOpenAIOptions(AzureApiVersion);
}
