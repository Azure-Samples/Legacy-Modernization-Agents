using GitHub.Copilot;
using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging;
using System.Runtime.CompilerServices;
using System.Text;

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
    private readonly SemaphoreSlim _startLock = new(1, 1);
    private bool _started;
    private bool _disposed;

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
    public CopilotChatClient(string model, CopilotClientOptions? options = null, ILogger? logger = null)
    {
        _model = model ?? throw new ArgumentNullException(nameof(model));
        _logger = logger;

        // Merge logger into options if provided
        var clientOptions = options ?? new CopilotClientOptions();
        if (logger != null)
        {
            clientOptions.Logger = logger;
        }

        _client = new CopilotClient(clientOptions);
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

        // SDK 1.x replaced the single non-generic On(evt => switch) overload with one typed
        // subscription per event, so each case below becomes its own handler.
        using var subMessage = session.On<AssistantMessageEvent>(msg =>
        {
            responseBuilder.Append(msg.Data.Content);
        });

        using var subError = session.On<SessionErrorEvent>(err =>
        {
            errorMessage = err.Data.Message;
            if (!done.Task.IsCompleted) done.TrySetResult();
        });

        using var subIdle = session.On<SessionIdleEvent>(_ =>
        {
            if (!done.Task.IsCompleted) done.TrySetResult();
        });

        await session.SendAsync(new MessageOptions { Prompt = userPromptBuilder.ToString() });

        // Wait for completion, cancellation, or timeout
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
                        "The Copilot CLI holds this credential, not the GitHub CLI, so 'gh auth login' " +
                        "does not affect it; sign in from the CLI with /login if the session has expired. " +
                        "A long reasoning request on a large program, or a machine resuming from sleep, " +
                        "can also exceed the timeout without anything being wrong with the credential."));
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

        var responseText = responseBuilder.ToString();
        _logger?.LogDebug("CopilotChatClient: received {Length} chars from model {Model}", responseText.Length, model);

        var responseMessage = new AIChatMessage(ChatRole.Assistant, responseText);
        return new ChatResponse(responseMessage);
    }

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

        using var subDelta = session.On<AssistantMessageDeltaEvent>(delta =>
        {
            writer.TryWrite(new ChatResponseUpdate
            {
                Role = ChatRole.Assistant,
                Contents = [new TextContent(delta.Data.DeltaContent)]
            });
        });

        using var subError = session.On<SessionErrorEvent>(err =>
        {
            writer.TryComplete(new InvalidOperationException($"Copilot SDK error: {err.Data.Message}"));
        });

        using var subIdle = session.On<SessionIdleEvent>(_ =>
        {
            writer.TryComplete();
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
    /// <remarks>
    /// Marks the instance as disposed so subsequent API calls throw
    /// <see cref="ObjectDisposedException"/>. For a graceful shutdown of
    /// the underlying Copilot CLI process, prefer <c>await using</c> or
    /// call <see cref="DisposeAsync"/> directly.
    /// </remarks>
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
