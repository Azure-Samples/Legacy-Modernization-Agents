using System.Net.Http.Json;
using System.Text;
using System.Text.Json;
using System.Text.Json.Nodes;
using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Helpers;

namespace CobolToQuarkusMigration.Agents.Infrastructure;

/// <summary>
/// Client for Azure OpenAI Responses API (used by codex/reasoning models like gpt-5.1-codex-mini).
/// This is separate from the Chat Completions API used by chat models.
/// 
/// Key differences from Chat Completions:
/// - Uses max_output_tokens (NOT max_tokens or max_completion_tokens)
/// - Supports reasoning.effort parameter ("low", "medium", "high")
/// - Returns output in a different JSON structure
/// </summary>
public class ResponsesApiClient : IDisposable
{
    private readonly string _apiVersion;
    private readonly HttpClient _httpClient;
    private readonly string _endpoint;
    private readonly string _apiKey;
    private readonly string _deploymentName;
    private readonly ILogger? _logger;
    private readonly EnhancedLogger? _enhancedLogger;
    private readonly JsonSerializerOptions _jsonOptions;
    
    // Rate limiting for 1M TPM / 1K RPM limits
    private readonly RateLimitTracker _rateLimitTracker;

    /// <summary>
    /// Creates a new Responses API client with rate limiting.
    /// </summary>
    /// <param name="endpoint">Azure OpenAI endpoint (e.g., https://your-resource.openai.azure.com/)</param>
    /// <param name="apiKey">Azure OpenAI API key</param>
    /// <param name="deploymentName">Deployment name (e.g., gpt-5.1-codex-mini)</param>
    /// <param name="logger">Optional logger</param>
    /// <param name="enhancedLogger">Optional enhanced logger for API call tracking</param>
    /// <param name="timeoutSeconds">HTTP timeout in seconds (default 600 for large code generation)</param>
    /// <param name="tokensPerMinute">TPM limit (default 1,000,000)</param>
    /// <param name="requestsPerMinute">RPM limit (default 1,000)</param>
    /// <param name="apiVersion">API version to use (default: 2025-04-01-preview)</param>
    public ResponsesApiClient(
        string endpoint, 
        string apiKey, 
        string deploymentName, 
        ILogger? logger = null,
        EnhancedLogger? enhancedLogger = null,
        int timeoutSeconds = 600,
        int tokensPerMinute = 1_000_000,
        int requestsPerMinute = 1_000,
        string apiVersion = "2025-04-01-preview")
    {
        if (string.IsNullOrEmpty(endpoint))
            throw new ArgumentNullException(nameof(endpoint));
        if (string.IsNullOrEmpty(apiKey))
            throw new ArgumentNullException(nameof(apiKey));
        if (string.IsNullOrEmpty(deploymentName))
            throw new ArgumentNullException(nameof(deploymentName));

        _endpoint = endpoint.TrimEnd('/');
        _apiKey = apiKey;
        _deploymentName = deploymentName;
        _logger = logger;
        _enhancedLogger = enhancedLogger;
        _apiVersion = apiVersion;
        
        _rateLimitTracker = new RateLimitTracker(tokensPerMinute, requestsPerMinute, logger);

        _httpClient = new HttpClient();
        _httpClient.DefaultRequestHeaders.Add("api-key", apiKey);
        _httpClient.Timeout = TimeSpan.FromSeconds(timeoutSeconds);

        _jsonOptions = new JsonSerializerOptions
        {
            PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
            WriteIndented = false
        };

        _logger?.LogInformation(
            "Created Responses API client for {Deployment} (timeout: {Timeout}s, TPM: {TPM:N0}, RPM: {RPM:N0}, API: {ApiVersion})", 
            deploymentName, timeoutSeconds, tokensPerMinute, requestsPerMinute, apiVersion);
    }

    /// <summary>
    /// Estimates the number of tokens in a text string.
    /// Uses ~3.5 characters per token for code (conservative).
    /// </summary>
    public static int EstimateTokens(string text)
    {
        if (string.IsNullOrEmpty(text)) return 0;
        return (int)Math.Ceiling(text.Length / 3.5);
    }

    /// <summary>
    /// Calculates optimal max_output_tokens based on input size.
    /// For code conversion, output is typically 1.5-2x the input size.
    /// </summary>
    public (int maxOutputTokens, string reasoningEffort) CalculateTokenSettings(
        string systemPrompt, 
        string userPrompt)
    {
        var inputTokens = EstimateTokens(systemPrompt) + EstimateTokens(userPrompt);
        
        // For code conversion, expect output to be ~2x input (COBOL → C#/Java expansion)
        // Plus some headroom for reasoning (which comes out of max_output_tokens)
        var estimatedOutputNeeded = (int)(inputTokens * 2.5);
        
        // Clamp to reasonable limits
        // - Minimum: 16K to handle small files properly
        // - Maximum: 64K (model limit is higher but diminishing returns)
        var maxOutputTokens = Math.Clamp(estimatedOutputNeeded, 16384, 65536);
        
        // Use "medium" reasoning effort as "low" is not supported by gpt-5.2-chat
        const string reasoningEffort = "medium";
        
        _logger?.LogInformation(
            "Token settings: Input ~{InputTokens}, max_output_tokens={MaxOutput}, reasoning.effort='{Effort}'",
            inputTokens, maxOutputTokens, reasoningEffort);
        
        return (maxOutputTokens, reasoningEffort);
    }

    /// <summary>
    /// Executes a Responses API call with automatic token optimization.
    /// </summary>
    public async Task<string> GetResponseAutoAsync(
        string systemPrompt,
        string userPrompt,
        CancellationToken cancellationToken = default)
    {
        var (maxOutputTokens, reasoningEffort) = CalculateTokenSettings(systemPrompt, userPrompt);
        return await GetResponseAsync(systemPrompt, userPrompt, maxOutputTokens, reasoningEffort, cancellationToken);
    }

    /// <summary>
    /// Executes a Responses API call with system and user prompts.
    /// </summary>
    /// <param name="systemPrompt">The system instruction.</param>
    /// <param name="userPrompt">The user input/prompt.</param>
    /// <param name="maxOutputTokens">Maximum tokens for the response (includes reasoning + text output).</param>
    /// <param name="reasoningEffort">Reasoning effort: "low", "medium", or "high". Use "medium" for gpt-5.2-chat.</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    /// <returns>The response text from the model.</returns>
    public async Task<string> GetResponseAsync(
        string systemPrompt,
        string userPrompt,
        int maxOutputTokens = 32768,
        string reasoningEffort = "medium",
        CancellationToken cancellationToken = default)
    {
        var estimatedInputTokens = EstimateTokens(systemPrompt) + EstimateTokens(userPrompt);
        var estimatedTotalTokens = estimatedInputTokens + maxOutputTokens;
        
        // Wait for rate limit capacity (TPM + RPM)
        await _rateLimitTracker.WaitForCapacityAsync(estimatedTotalTokens, cancellationToken);
        
        _logger?.LogInformation(
            "Responses API: ~{Input} input + {MaxOutput} max output = ~{Total} total tokens, reasoning='{Effort}'",
            estimatedInputTokens, maxOutputTokens, estimatedTotalTokens, reasoningEffort);
        
        if (estimatedInputTokens > 50000)
        {
            _logger?.LogWarning(
                "Large input ({InputTokens} tokens). Consider chunking for better results.",
                estimatedInputTokens);
        }

        var uri = $"{_endpoint}/openai/responses?api-version={_apiVersion}";

        // Build request body for Responses API
        // IMPORTANT: Use max_output_tokens (NOT max_tokens or max_completion_tokens)
        var requestBody = new
        {
            model = _deploymentName,
            input = new object[]
            {
                new { type = "message", role = "system", content = systemPrompt },
                new { type = "message", role = "user", content = userPrompt }
            },
            max_output_tokens = maxOutputTokens,
            temperature = 1.0,
            reasoning = new
            {
                effort = reasoningEffort  // "medium" required for gpt-5.2-chat
            }
        };

        var json = JsonSerializer.Serialize(requestBody, _jsonOptions);
        using var content = new StringContent(json, Encoding.UTF8, "application/json");

        var startTime = DateTime.UtcNow;
        
        // Track API call start for statistics
        var apiCallId = _enhancedLogger?.LogApiCallStart(
            "ResponsesAPI", 
            "POST", 
            uri, 
            _deploymentName,
            $"Input: {estimatedInputTokens} tokens, MaxOutput: {maxOutputTokens}, Reasoning: {reasoningEffort}") ?? 0;
        
        try
        {
            using var response = await _httpClient.PostAsync(uri, content, cancellationToken);
            var responseText = await response.Content.ReadAsStringAsync(cancellationToken);

            if (!response.IsSuccessStatusCode)
            {
                _enhancedLogger?.LogApiCallError(apiCallId, $"HTTP {response.StatusCode}");
                _logger?.LogError("Responses API failed ({StatusCode}): {Body}", response.StatusCode, responseText);
                throw new HttpRequestException($"Responses API failed with status {response.StatusCode}: {responseText}");
            }

            var parsed = JsonNode.Parse(responseText);
            
            // Extract actual token usage for rate limiting
            var usage = parsed?["usage"];
            var actualInputTokens = usage?["input_tokens"]?.GetValue<int>() ?? estimatedInputTokens;
            var actualOutputTokens = usage?["output_tokens"]?.GetValue<int>() ?? 0;
            var reasoningTokens = usage?["output_tokens_details"]?["reasoning_tokens"]?.GetValue<int>() ?? 0;
            var actualTotalTokens = actualInputTokens + actualOutputTokens;
            
            // Record actual usage for rate limiting
            _rateLimitTracker.RecordUsage(actualTotalTokens);
            
            var elapsed = DateTime.UtcNow - startTime;
            
            // Track API call completion for statistics
            _enhancedLogger?.LogApiCallEnd(
                apiCallId, 
                $"Output: {actualOutputTokens} tokens ({reasoningTokens} reasoning)", 
                actualTotalTokens);
            
            _logger?.LogInformation(
                "Responses API completed in {Elapsed:F1}s: {Input} input + {Output} output ({Reasoning} reasoning) = {Total} tokens",
                elapsed.TotalSeconds, actualInputTokens, actualOutputTokens, reasoningTokens, actualTotalTokens);
            
            // Check for incomplete status
            var status = parsed?["status"]?.GetValue<string>();
            if (status == "incomplete")
            {
                var reason = parsed?["incomplete_details"]?["reason"]?.GetValue<string>();
                
                if (reason == "max_output_tokens" && reasoningTokens >= actualOutputTokens * 0.9)
                {
                    throw new InvalidOperationException(
                        $"Model exhausted max_output_tokens ({maxOutputTokens}) on reasoning ({reasoningTokens} tokens) " +
                        $"with minimal text output. Solutions: 1) Chunk the input, 2) Use reasoning.effort='low', " +
                        $"3) Increase max_output_tokens.");
                }
                
                _logger?.LogWarning(
                    "Response incomplete: {Reason}. Output={Output}, Reasoning={Reasoning}",
                    reason, actualOutputTokens, reasoningTokens);
            }
            
            // Parse the output
            return ParseResponseOutput(parsed, responseText);
        }
        catch (Exception ex) when (ex is not InvalidOperationException)
        {
            _logger?.LogError(ex, "Responses API error after {Elapsed:F1}s", (DateTime.UtcNow - startTime).TotalSeconds);
            throw;
        }
    }

    /// <summary>
    /// Parses the Responses API output structure to extract the text content.
    /// </summary>
    private string ParseResponseOutput(JsonNode? parsed, string rawResponse)
    {
        var output = parsed?["output"];
        if (output is JsonArray outputArray)
        {
            var sb = new StringBuilder();
            foreach (var item in outputArray)
            {
                var type = item?["type"]?.GetValue<string>();
                if (type == "message")
                {
                    var role = item?["role"]?.GetValue<string>();
                    if (role == "assistant")
                    {
                        var contentArray = item?["content"];
                        if (contentArray is JsonArray contents)
                        {
                            foreach (var c in contents)
                            {
                                var cType = c?["type"]?.GetValue<string>();
                                if (cType == "output_text" || cType == "text")
                                {
                                    sb.Append(c?["text"]?.GetValue<string>() ?? "");
                                }
                            }
                        }
                    }
                }
            }
            
            var result = sb.ToString();
            if (!string.IsNullOrEmpty(result))
            {
                _logger?.LogDebug("Parsed {Length} chars from Responses API output", result.Length);
                return result;
            }
        }

        // Fallback: try direct output_text field
        var outputText = parsed?["output_text"]?.GetValue<string>();
        if (!string.IsNullOrEmpty(outputText))
            return outputText;

        _logger?.LogWarning("Could not parse Responses API output, returning raw response");
        return rawResponse;
    }

    public void Dispose()
    {
        _httpClient.Dispose();
    }
}

/// <summary>
/// Tracks token and request usage for rate limiting.
/// Optimized for 1M TPM / 1K RPM limits.
/// </summary>
internal class RateLimitTracker
{
    private readonly int _tokensPerMinute;
    private readonly int _requestsPerMinute;
    private readonly ILogger? _logger;
    private readonly object _lock = new();
    
    private readonly Queue<(DateTime time, int tokens)> _tokenHistory = new();
    private readonly Queue<DateTime> _requestHistory = new();
    
    // Safety margin: stay at 90% of limits to avoid hitting them
    private const double SafetyMargin = 0.90;

    public RateLimitTracker(int tokensPerMinute, int requestsPerMinute, ILogger? logger)
    {
        _tokensPerMinute = (int)(tokensPerMinute * SafetyMargin);
        _requestsPerMinute = (int)(requestsPerMinute * SafetyMargin);
        _logger = logger;
    }

    /// <summary>
    /// Waits until there's capacity for the estimated token usage.
    /// </summary>
    public async Task WaitForCapacityAsync(int estimatedTokens, CancellationToken cancellationToken)
    {
        while (true)
        {
            cancellationToken.ThrowIfCancellationRequested();
            
            var (canProceed, waitTime, reason) = CheckCapacity(estimatedTokens);
            
            if (canProceed)
                return;
            
            _logger?.LogInformation(
                "Rate limit: waiting {Wait:F1}s ({Reason}). Current: {Tokens:N0}/{TPM:N0} TPM, {Requests}/{RPM} RPM",
                waitTime.TotalSeconds, reason, GetCurrentTokensPerMinute(), _tokensPerMinute, 
                GetCurrentRequestsPerMinute(), _requestsPerMinute);
            
            await Task.Delay(waitTime, cancellationToken);
        }
    }

    private (bool canProceed, TimeSpan waitTime, string reason) CheckCapacity(int estimatedTokens)
    {
        lock (_lock)
        {
            PruneOldEntries();
            
            var currentTokens = GetCurrentTokensPerMinute();
            var currentRequests = GetCurrentRequestsPerMinute();
            
            // Check TPM
            if (currentTokens + estimatedTokens > _tokensPerMinute)
            {
                var oldestToken = _tokenHistory.Count > 0 ? _tokenHistory.Peek().time : DateTime.UtcNow;
                var waitUntil = oldestToken.AddMinutes(1);
                var waitTime = waitUntil - DateTime.UtcNow;
                if (waitTime < TimeSpan.Zero) waitTime = TimeSpan.FromSeconds(1);
                return (false, waitTime, $"TPM: {currentTokens:N0}+{estimatedTokens:N0} > {_tokensPerMinute:N0}");
            }
            
            // Check RPM
            if (currentRequests + 1 > _requestsPerMinute)
            {
                var oldestRequest = _requestHistory.Count > 0 ? _requestHistory.Peek() : DateTime.UtcNow;
                var waitUntil = oldestRequest.AddMinutes(1);
                var waitTime = waitUntil - DateTime.UtcNow;
                if (waitTime < TimeSpan.Zero) waitTime = TimeSpan.FromSeconds(1);
                return (false, waitTime, $"RPM: {currentRequests}+1 > {_requestsPerMinute}");
            }
            
            return (true, TimeSpan.Zero, "OK");
        }
    }

    /// <summary>
    /// Records actual token usage after a request completes.
    /// </summary>
    public void RecordUsage(int actualTokens)
    {
        lock (_lock)
        {
            var now = DateTime.UtcNow;
            _tokenHistory.Enqueue((now, actualTokens));
            _requestHistory.Enqueue(now);
            
            _logger?.LogDebug(
                "Recorded: {Tokens:N0} tokens. Window: {TotalTokens:N0}/{TPM:N0} TPM, {Requests}/{RPM} RPM",
                actualTokens, GetCurrentTokensPerMinute(), _tokensPerMinute,
                GetCurrentRequestsPerMinute(), _requestsPerMinute);
        }
    }

    private void PruneOldEntries()
    {
        var oneMinuteAgo = DateTime.UtcNow.AddMinutes(-1);
        
        while (_tokenHistory.Count > 0 && _tokenHistory.Peek().time < oneMinuteAgo)
            _tokenHistory.Dequeue();
        
        while (_requestHistory.Count > 0 && _requestHistory.Peek() < oneMinuteAgo)
            _requestHistory.Dequeue();
    }

    private int GetCurrentTokensPerMinute()
    {
        return _tokenHistory.Sum(x => x.tokens);
    }

    private int GetCurrentRequestsPerMinute()
    {
        return _requestHistory.Count;
    }
}

/// <summary>
/// Factory for creating ResponsesApiClient instances.
/// </summary>
public static class ResponsesApiClientFactory
{
    /// <summary>
    /// Creates a ResponsesApiClient for Azure OpenAI with default rate limits.
    /// </summary>
    public static ResponsesApiClient CreateAzureClient(
        string endpoint,
        string apiKey,
        string deploymentName,
        ILogger? logger = null,
        EnhancedLogger? enhancedLogger = null,
        int tokensPerMinute = 1_000_000,
        int requestsPerMinute = 1_000)
    {
        return new ResponsesApiClient(
            endpoint, apiKey, deploymentName, logger, enhancedLogger,
            timeoutSeconds: 600,
            tokensPerMinute: tokensPerMinute,
            requestsPerMinute: requestsPerMinute);
    }
}
