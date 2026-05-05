using GitHub.Copilot.SDK;
using Microsoft.Extensions.AI;
using System.Runtime.CompilerServices;
using System.Text;

using AIChatMessage = Microsoft.Extensions.AI.ChatMessage;

namespace McpChatWeb.Services;

/// <summary>
/// IChatClient adapter over the GitHub Copilot SDK for Prompt Studio.
/// Translates Microsoft.Extensions.AI chat completions into Copilot SDK session calls.
/// </summary>
public sealed class CopilotChatClient : IChatClient, IAsyncDisposable
{
    private readonly CopilotClient _client;
    private readonly string _model;
    private readonly SemaphoreSlim _startLock = new(1, 1);
    private bool _started;
    private bool _disposed;

    private static readonly TimeSpan RequestTimeout = TimeSpan.FromMinutes(5);

    public CopilotChatClient(string model, CopilotClientOptions? options = null)
    {
        _model = model ?? throw new ArgumentNullException(nameof(model));
        _client = new CopilotClient(options ?? CopilotCliResolver.BuildOptions(useStdio: true));
    }

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

    public ChatClientMetadata Metadata => new(nameof(CopilotChatClient), null, _model);

    public async Task<ChatResponse> GetResponseAsync(
        IEnumerable<AIChatMessage> messages,
        ChatOptions? options = null,
        CancellationToken cancellationToken = default)
    {
        ObjectDisposedException.ThrowIf(_disposed, this);
        await EnsureStartedAsync();

        var model = options?.ModelId ?? _model;

        string? systemMessage = null;
        var userPromptBuilder = new StringBuilder();

        foreach (var msg in messages)
        {
            var text = msg.Text;
            if (string.IsNullOrWhiteSpace(text)) continue;

            if (msg.Role == ChatRole.System)
                systemMessage = text;
            else
            {
                if (userPromptBuilder.Length > 0) userPromptBuilder.AppendLine();
                userPromptBuilder.Append(text);
            }
        }

        if (userPromptBuilder.Length == 0)
            throw new InvalidOperationException("Cannot send empty prompt to Copilot SDK");

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
            }
        });

        await session.SendAsync(new MessageOptions { Prompt = userPromptBuilder.ToString() });

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
                        $"Copilot SDK did not respond within {RequestTimeout.TotalMinutes} minutes."));
            }
        });

        await done.Task;
        cancellationToken.ThrowIfCancellationRequested();

        if (errorMessage != null)
            throw new InvalidOperationException($"Copilot SDK error: {errorMessage}");

        var responseText = responseBuilder.ToString();
        var responseMessage = new AIChatMessage(ChatRole.Assistant, responseText);
        return new ChatResponse(responseMessage);
    }

    public async IAsyncEnumerable<ChatResponseUpdate> GetStreamingResponseAsync(
        IEnumerable<AIChatMessage> messages,
        ChatOptions? options = null,
        [EnumeratorCancellation] CancellationToken cancellationToken = default)
    {
        // For Prompt Studio we don't need streaming — delegate to non-streaming
        var response = await GetResponseAsync(messages, options, cancellationToken);
        yield return new ChatResponseUpdate
        {
            Role = ChatRole.Assistant,
            Contents = response.Messages.LastOrDefault()?.Contents ?? []
        };
    }

    public object? GetService(Type serviceType, object? serviceKey = null) => null;

    public void Dispose()
    {
        if (_disposed) return;
        _disposed = true;
        try { _client.ForceStopAsync().GetAwaiter().GetResult(); } catch { }
    }

    public async ValueTask DisposeAsync()
    {
        if (_disposed) return;
        _disposed = true;
        try { await _client.StopAsync(); }
        catch { try { await _client.ForceStopAsync(); } catch { } }
    }
}
