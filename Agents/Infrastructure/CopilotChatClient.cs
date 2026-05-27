using GitHub.Copilot.SDK;
using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging;
using System.Runtime.CompilerServices;
using System.Text;
using System.Text.RegularExpressions;

using AIChatMessage = Microsoft.Extensions.AI.ChatMessage;

namespace CobolToQuarkusMigration.Agents.Infrastructure;

/// <summary>
/// IChatClient adapter over the GitHub Copilot SDK.
/// Translates Microsoft.Extensions.AI chat completions into Copilot SDK session calls,
/// so the rest of the codebase can use it seamlessly alongside Azure OpenAI clients.
/// </summary>
public sealed class CopilotChatClient : IChatClient, IAsyncDisposable
{
    private readonly CopilotClient _client;
    private readonly string _model;
    private readonly ILogger? _logger;
    private readonly IRateLimiter? _limiter;
    private readonly TimeSpan _callWaitCeiling;
    private readonly SemaphoreSlim _startLock = new(1, 1);
    private bool _started;
    private bool _disposed;

    // Best-effort detection of throttling messages from the Copilot SDK,
    // which surfaces errors as strings without HTTP status codes.
    private static readonly Regex RateLimitMessageRegex = new(
        @"\b(429|rate[\s-]?limit(ed|ing)?|too\s+many\s+requests|quota\s+exceeded)\b",
        RegexOptions.IgnoreCase | RegexOptions.Compiled);

    /// <summary>
    /// Per-request timeout. Prevents infinite hangs if the SDK never fires
    /// SessionIdleEvent (e.g. auth failure, network issues).
    /// </summary>
    private static readonly TimeSpan RequestTimeout = TimeSpan.FromMinutes(5);

    /// <summary>
    /// Creates a new CopilotChatClient.
    /// </summary>
    /// <param name="model">Model name (e.g. "gpt-5", "claude-sonnet-4.5").</param>
    /// <param name="options">Optional CopilotClientOptions for CLI path, auth, etc.</param>
    /// <param name="logger">Optional logger.</param>
    /// <param name="limiter">Optional rate limiter shared via <see cref="IRateLimiter"/>.</param>
    /// <param name="callWaitCeiling">Per-call wait ceiling for Retry-After honouring. Defaults to <see cref="LlmRetryHelper.DefaultWaitCeilingSeconds"/>.</param>
    public CopilotChatClient(
        string model,
        CopilotClientOptions? options = null,
        ILogger? logger = null,
        IRateLimiter? limiter = null,
        TimeSpan? callWaitCeiling = null)
    {
        _model = model ?? throw new ArgumentNullException(nameof(model));
        _logger = logger;
        _limiter = limiter;
        _callWaitCeiling = callWaitCeiling ?? TimeSpan.FromSeconds(LlmRetryHelper.DefaultWaitCeilingSeconds);

        // NOTE: deliberately do NOT attach the app logger to CopilotClientOptions.Logger
        // — the SDK emits very verbose JSON-RPC tracing that floods the console.
        // Use the app logger here only for our own structured throttling logs.
        _client = new CopilotClient(options ?? new CopilotClientOptions());
    }

    /// <summary>
    /// Ensures the underlying Copilot CLI server is started.
    /// </summary>
    private async Task EnsureStartedAsync()
    {
        await _startLock.WaitAsync();
        try
        {
            if (_started) return;
            await _client.StartAsync();
            _started = true;
        }
        finally
        {
            _startLock.Release();
        }
    }

    /// <inheritdoc />
    public ChatClientMetadata Metadata => new(nameof(CopilotChatClient), null, _model);

    /// <inheritdoc />
    public async Task<ChatResponse> GetResponseAsync(
        IEnumerable<AIChatMessage> messages,
        ChatOptions? options = null,
        CancellationToken cancellationToken = default)
    {
        ObjectDisposedException.ThrowIf(_disposed, this);
        await EnsureStartedAsync();

        var model = options?.ModelId ?? _model;
        _logger?.LogDebug("CopilotChatClient: sending request to model {Model}", model);

        // Extract system message and build user prompt from the conversation
        string? systemMessage = null;
        var userPromptBuilder = new StringBuilder();

        foreach (var msg in messages)
        {
            var text = msg.Text;
            if (string.IsNullOrWhiteSpace(text)) continue;

            if (msg.Role == ChatRole.System)
            {
                systemMessage = text;
            }
            else
            {
                if (userPromptBuilder.Length > 0) userPromptBuilder.AppendLine();
                userPromptBuilder.Append(text);
            }
        }

        // Guard: Copilot SDK requires non-whitespace content
        if (userPromptBuilder.Length == 0)
        {
            throw new InvalidOperationException("Cannot send empty prompt to Copilot SDK");
        }

        var userPrompt = userPromptBuilder.ToString();

        // Best-effort token estimate so the shared limiter can budget for GitHub
        // Models calls in the same way it does for Azure. The SDK does not expose
        // exact usage, so we approximate at ~4 chars per token.
        var estimatedInputTokens = EstimateTokens(systemMessage) + EstimateTokens(userPrompt);
        // Reserve a conservative 4× input as upper bound for response (output is
        // bounded by the model's max_output_tokens; we don't have a way to know it
        // here, so we use a heuristic — this is for budgeting only, not for capping).
        var estimatedTotalTokens = estimatedInputTokens + Math.Min(estimatedInputTokens * 4, 32_000);

        if (_limiter is not null)
        {
            await _limiter.AcquireAsync(estimatedTotalTokens, cancellationToken);
        }

        try
        {
            var responseText = await SendOnceWithRetryAsync(
                model, systemMessage, userPrompt, cancellationToken);

            var actualOutputTokens = EstimateTokens(responseText);
            _limiter?.RecordUsage(estimatedInputTokens + actualOutputTokens);

            var responseMessage = new AIChatMessage(ChatRole.Assistant, responseText);
            return new ChatResponse(responseMessage);
        }
        catch
        {
            // Limiter records were taken at acquire; if we don't reach RecordUsage
            // they will age out of the rolling window naturally. RateLimitTracker
            // exposes ReleaseReservation() for the Azure client; the GitHub
            // limiter (a separate instance) just lets the reservation expire.
            throw;
        }
    }

    /// <summary>
    /// Sends a single prompt, applying our shared retry policy. The Copilot SDK
    /// signals errors via string messages without HTTP status codes, so we
    /// pattern-match for throttling indicators and surface a typed
    /// <see cref="RateLimitedException"/> after bounded retries.
    /// </summary>
    private async Task<string> SendOnceWithRetryAsync(
        string model,
        string? systemMessage,
        string userPrompt,
        CancellationToken cancellationToken)
    {
        return await LlmRetryHelper.ExecuteAsync<string>(
            provider: "github-copilot-sdk",
            model: model,
            attempt: async (attemptIndex, ct) =>
            {
                try
                {
                    var text = await IssueRequestAsync(model, systemMessage, userPrompt, ct);
                    return new CallOutcome.Success<string>(text);
                }
                catch (OperationCanceledException) { throw; }
                catch (TimeoutException tex)
                {
                    return new CallOutcome.Fatal(tex, tex.Message);
                }
                catch (InvalidOperationException ex)
                {
                    // The SDK throws InvalidOperationException for all error paths.
                    // Best-effort throttling detection via the message text.
                    if (RateLimitMessageRegex.IsMatch(ex.Message))
                    {
                        return new CallOutcome.RateLimited(
                            RetryAfter: null,
                            Reason: $"github-sdk-message: {Truncate(ex.Message, 200)}");
                    }
                    return new CallOutcome.Fatal(ex, ex.Message);
                }
            },
            limiter: _limiter,
            waitCeiling: _callWaitCeiling,
            logger: _logger,
            cancellationToken: cancellationToken);
    }

    /// <summary>
    /// One round-trip to the Copilot SDK — no retry. Returns the assembled
    /// assistant text or throws on SDK error / timeout.
    /// </summary>
    private async Task<string> IssueRequestAsync(
        string model, string? systemMessage, string userPrompt, CancellationToken cancellationToken)
    {
        // Create a session per request (stateless adapter pattern)
        var sessionConfig = new SessionConfig
        {
            Model = model,
            InfiniteSessions = new InfiniteSessionConfig { Enabled = false },
            OnPermissionRequest = PermissionHandler.ApproveAll
        };

        if (systemMessage != null)
        {
            sessionConfig.SystemMessage = new SystemMessageConfig
            {
                Mode = SystemMessageMode.Replace,
                Content = systemMessage
            };
        }

        // Disable all built-in tools — we only want raw LLM completions
        sessionConfig.AvailableTools = new List<string>();

        await using var session = await _client.CreateSessionAsync(sessionConfig);

        var responseBuilder = new StringBuilder();
        var done = new TaskCompletionSource();
        string? errorMessage = null;

        using var _ = session.On(evt =>
        {
            switch (evt)
            {
                case AssistantMessageEvent msg:
                    responseBuilder.Append(msg.Data.Content);
                    break;
                case SessionErrorEvent err:
                    errorMessage = err.Data.Message;
                    if (!done.Task.IsCompleted) done.TrySetResult();
                    break;
                case SessionIdleEvent:
                    if (!done.Task.IsCompleted) done.TrySetResult();
                    break;
                default:
                    _logger?.LogDebug("CopilotChatClient: unhandled event {EventType}", evt.GetType().Name);
                    break;
            }
        });

        await session.SendAsync(new MessageOptions { Prompt = userPrompt });

        using var timeoutCts = CancellationTokenSource.CreateLinkedTokenSource(cancellationToken);
        timeoutCts.CancelAfter(RequestTimeout);
        using var ctsReg = timeoutCts.Token.Register(() =>
        {
            if (!done.Task.IsCompleted)
            {
                if (cancellationToken.IsCancellationRequested)
                    done.TrySetCanceled(cancellationToken);
                else
                    done.TrySetException(new TimeoutException(
                        $"Copilot SDK did not respond within {RequestTimeout.TotalMinutes} minutes. " +
                        "This usually indicates an authentication issue — ensure you are logged in via 'gh auth login'."));
            }
        });

        try
        {
            await done.Task;
        }
        catch (TimeoutException)
        {
            _logger?.LogError("CopilotChatClient: request timed out after {Minutes}m for model {Model}", RequestTimeout.TotalMinutes, model);
            throw;
        }

        cancellationToken.ThrowIfCancellationRequested();

        if (errorMessage != null)
        {
            throw new InvalidOperationException($"Copilot SDK error: {errorMessage}");
        }

        return responseBuilder.ToString();
    }

    private static int EstimateTokens(string? text) =>
        string.IsNullOrEmpty(text) ? 0 : Math.Max(1, text.Length / 4);

    private static string Truncate(string s, int max) =>
        s.Length <= max ? s : s.Substring(0, max) + "…";

    /// <inheritdoc />
    public async IAsyncEnumerable<ChatResponseUpdate> GetStreamingResponseAsync(
        IEnumerable<AIChatMessage> messages,
        ChatOptions? options = null,
        [EnumeratorCancellation] CancellationToken cancellationToken = default)
    {
        ObjectDisposedException.ThrowIf(_disposed, this);
        await EnsureStartedAsync();

        var model = options?.ModelId ?? _model;

        string? systemMessage = null;
        var userPromptBuilder = new StringBuilder();

        foreach (var msg in messages)
        {
            if (msg.Role == ChatRole.System)
                systemMessage = msg.Text;
            else
            {
                if (userPromptBuilder.Length > 0) userPromptBuilder.AppendLine();
                userPromptBuilder.Append(msg.Text);
            }
        }

        var sessionConfig = new SessionConfig
        {
            Model = model,
            Streaming = true,
            InfiniteSessions = new InfiniteSessionConfig { Enabled = false },
            OnPermissionRequest = PermissionHandler.ApproveAll
        };

        if (systemMessage != null)
        {
            sessionConfig.SystemMessage = new SystemMessageConfig
            {
                Mode = SystemMessageMode.Replace,
                Content = systemMessage
            };
        }

        sessionConfig.AvailableTools = new List<string>();

        await using var session = await _client.CreateSessionAsync(sessionConfig);

        var channel = System.Threading.Channels.Channel.CreateUnbounded<ChatResponseUpdate>();
        var writer = channel.Writer;

        using var subscription = session.On(evt =>
        {
            switch (evt)
            {
                case AssistantMessageDeltaEvent delta:
                    writer.TryWrite(new ChatResponseUpdate
                    {
                        Role = ChatRole.Assistant,
                        Contents = [new TextContent(delta.Data.DeltaContent)]
                    });
                    break;
                case SessionErrorEvent err:
                    writer.TryComplete(new InvalidOperationException($"Copilot SDK error: {err.Data.Message}"));
                    break;
                case SessionIdleEvent:
                    writer.TryComplete();
                    break;
            }
        });

        await session.SendAsync(new MessageOptions { Prompt = userPromptBuilder.ToString() });

        await foreach (var update in channel.Reader.ReadAllAsync(cancellationToken))
        {
            yield return update;
        }
    }

    /// <inheritdoc />
    public object? GetService(Type serviceType, object? serviceKey = null) => null;

    /// <inheritdoc />
    public void Dispose()
    {
        if (_disposed) return;
        _disposed = true;
        try { _client.ForceStopAsync().GetAwaiter().GetResult(); } catch { /* best-effort */ }
    }

    /// <inheritdoc />
    public async ValueTask DisposeAsync()
    {
        if (_disposed) return;
        _disposed = true;
        try
        {
            await _client.StopAsync();
        }
        catch (OperationCanceledException ex)
        {
            _logger?.LogWarning(ex, "CopilotChatClient: graceful stop was canceled, force-stopping");
            await _client.ForceStopAsync();
        }
        catch (InvalidOperationException ex)
        {
            _logger?.LogWarning(ex, "CopilotChatClient: graceful stop failed due to invalid state, force-stopping");
            await _client.ForceStopAsync();
        }
    }
}
