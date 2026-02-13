using Azure.AI.OpenAI;
using Azure.Identity;
using Azure.Core;
using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging;
using OpenAI;
using AzureOpenAIOptions = Azure.AI.OpenAI.AzureOpenAIClientOptions;
using AzureServiceVersion = Azure.AI.OpenAI.AzureOpenAIClientOptions.ServiceVersion;

namespace CobolToQuarkusMigration.Agents.Infrastructure;

/// <summary>
/// Factory for creating IChatClient instances for Azure OpenAI or OpenAI.
/// </summary>
public static class ChatClientFactory
{
    private static readonly AzureServiceVersion AzureApiVersion = AzureServiceVersion.V2024_06_01;

    /// <summary>
    /// Creates an IChatClient for Azure OpenAI using API key authentication.
    /// </summary>
    /// <param name="endpoint">The Azure OpenAI endpoint.</param>
    /// <param name="apiKey">The API key.</param>
    /// <param name="modelId">The deployment/model name.</param>
    /// <param name="logger">Optional logger.</param>
    /// <returns>An IChatClient instance.</returns>
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
            CreateOptions());

        return client.GetChatClient(modelId).AsIChatClient();
    }

    /// <summary>
    /// Creates an IChatClient for Azure OpenAI using a TokenCredential (e.g. DefaultAzureCredential).
    /// </summary>
    /// <param name="endpoint">The Azure OpenAI endpoint.</param>
    /// <param name="credential">The token credential.</param>
    /// <param name="modelId">The deployment/model name.</param>
    /// <param name="logger">Optional logger.</param>
    /// <returns>An IChatClient instance.</returns>
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
            CreateOptions());

        return client.GetChatClient(modelId).AsIChatClient();
    }

    /// <summary>
    /// Creates an IChatClient for Azure OpenAI using DefaultAzureCredential (managed identity, etc.).
    /// </summary>
    /// <param name="endpoint">The Azure OpenAI endpoint.</param>
    /// <param name="modelId">The deployment/model name.</param>
    /// <param name="logger">Optional logger.</param>
    /// <returns>An IChatClient instance.</returns>
    public static IChatClient CreateAzureOpenAIChatClientWithDefaultCredential(
        string endpoint,
        string modelId,
        ILogger? logger = null)
    {
        return CreateAzureOpenAIChatClient(endpoint, new DefaultAzureCredential(), modelId, logger);
    }

    /// <summary>
    /// Creates an IChatClient for OpenAI (not Azure).
    /// </summary>
    /// <param name="apiKey">The OpenAI API key.</param>
    /// <param name="modelId">The model name (e.g., "gpt-4").</param>
    /// <param name="logger">Optional logger.</param>
    /// <returns>An IChatClient instance.</returns>
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

    /// <summary>
    /// Creates an IChatClient based on configuration settings.
    /// Automatically determines whether to use Azure OpenAI or OpenAI based on the presence of an endpoint.
    /// </summary>
    /// <param name="endpoint">The Azure OpenAI endpoint (null/empty for OpenAI).</param>
    /// <param name="apiKey">The API key.</param>
    /// <param name="modelId">The model/deployment name.</param>
    /// <param name="useDefaultCredential">Whether to use DefaultAzureCredential for Azure OpenAI.</param>
    /// <param name="logger">Optional logger.</param>
    /// <returns>An IChatClient instance.</returns>
    public static IChatClient CreateChatClient(
        string? endpoint,
        string apiKey,
        string modelId,
        bool useDefaultCredential = false,
        ILogger? logger = null)
    {
        // If endpoint is provided, use Azure OpenAI
        if (!string.IsNullOrEmpty(endpoint))
        {
            if (useDefaultCredential)
            {
                return CreateAzureOpenAIChatClientWithDefaultCredential(endpoint, modelId, logger);
            }
            return CreateAzureOpenAIChatClient(endpoint, apiKey, modelId, logger);
        }

        // Otherwise, use OpenAI
        return CreateOpenAIChatClient(apiKey, modelId, logger);
    }

    private static AzureOpenAIOptions CreateOptions() => new AzureOpenAIOptions(AzureApiVersion);
}
