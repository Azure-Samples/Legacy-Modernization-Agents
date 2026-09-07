using System.Text;
using System.Text.RegularExpressions;
using GitHub.Copilot;

namespace CobolToQuarkusMigration.Helpers;

public sealed record CopilotDiagnosticResult(
    bool Success,
    string Category,
    string Message,
    string Host,
    IReadOnlyList<string>? Models = null,
    string? Model = null);

public static class CopilotModelDiagnostics
{
    public static async Task<CopilotDiagnosticResult> ListModelsAsync(
        CopilotClientOptions options,
        TimeSpan timeout,
        CancellationToken cancellationToken = default)
    {
        var route = CopilotRouting.ApplyTo(options);
        using var timeoutCts = CancellationTokenSource.CreateLinkedTokenSource(cancellationToken);
        timeoutCts.CancelAfter(timeout);
        var client = new CopilotClient(options);

        try
        {
            await client.StartAsync(timeoutCts.Token);
            var models = await client.ListModelsAsync(timeoutCts.Token);
            var ids = models
                .Select(model => model.Id ?? model.Name)
                .Where(id => !string.IsNullOrWhiteSpace(id))
                .Select(id => id!)
                .Distinct(StringComparer.OrdinalIgnoreCase)
                .OrderBy(id => id, StringComparer.OrdinalIgnoreCase)
                .ToArray();

            return new CopilotDiagnosticResult(
                true,
                "success",
                ids.Length == 0 ? "The Copilot model catalog was empty." : $"Discovered {ids.Length} model(s).",
                route.Hostname,
                ids);
        }
        catch (Exception ex)
        {
            return Failure(ex, route.Hostname, timeoutCts, cancellationToken);
        }
        finally
        {
            await StopClientAsync(client);
        }
    }

    public static async Task<CopilotDiagnosticResult> ValidateModelAsync(
        string model,
        CopilotClientOptions options,
        TimeSpan timeout,
        CancellationToken cancellationToken = default)
    {
        if (string.IsNullOrWhiteSpace(model))
            return new(false, "usage", "A model ID is required.", CopilotRouting.Resolve().Hostname, Model: model);

        var route = CopilotRouting.ApplyTo(options);
        using var timeoutCts = CancellationTokenSource.CreateLinkedTokenSource(cancellationToken);
        timeoutCts.CancelAfter(timeout);
        var client = new CopilotClient(options);
        CopilotSession? session = null;

        try
        {
            await client.StartAsync(timeoutCts.Token);
            session = await client.CreateSessionAsync(new SessionConfig
            {
                Model = model.Trim(),
                AvailableTools = new List<string>(),
                InfiniteSessions = new InfiniteSessionConfig { Enabled = false },
                OnPermissionRequest = PermissionHandler.ApproveAll
            }, timeoutCts.Token);

            var response = new StringBuilder();
            string? error = null;
            var completed = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);

            using var messageSubscription = session.On<AssistantMessageEvent>(
                message => response.Append(message.Data.Content));
            using var errorSubscription = session.On<SessionErrorEvent>(sessionError =>
            {
                error = sessionError.Data.Message;
                completed.TrySetResult();
            });
            using var idleSubscription = session.On<SessionIdleEvent>(_ => completed.TrySetResult());

            await session.SendAsync(
                new MessageOptions { Prompt = "Reply with exactly OK." },
                timeoutCts.Token);
            await completed.Task.WaitAsync(timeoutCts.Token);

            if (!string.IsNullOrWhiteSpace(error))
                throw new InvalidOperationException(error);
            if (response.Length == 0)
                throw new InvalidOperationException("The model returned an empty response.");

            return new(true, "success", "Model validation succeeded.", route.Hostname, Model: model.Trim());
        }
        catch (Exception ex)
        {
            return Failure(ex, route.Hostname, timeoutCts, cancellationToken, model.Trim());
        }
        finally
        {
            if (session is not null)
            {
                try { await session.DisposeAsync().AsTask().WaitAsync(TimeSpan.FromSeconds(5)); }
                catch { }
            }
            await StopClientAsync(client);
        }
    }

    internal static async Task<T> RunBoundedAsync<T>(
        Func<CancellationToken, Task<T>> operation,
        TimeSpan timeout,
        CancellationToken cancellationToken = default)
    {
        using var timeoutCts = CancellationTokenSource.CreateLinkedTokenSource(cancellationToken);
        timeoutCts.CancelAfter(timeout);
        return await operation(timeoutCts.Token).WaitAsync(timeoutCts.Token);
    }

    public static int ExitCodeFor(string category) => category switch
    {
        "success" => 0,
        "usage" => 2,
        "authentication" => 3,
        "routing" => 4,
        "unavailable_or_policy" => 5,
        "network" => 6,
        "timeout" => 7,
        "runtime_or_protocol" => 8,
        _ => 9
    };

    private static CopilotDiagnosticResult Failure(
        Exception exception,
        string host,
        CancellationTokenSource timeoutCts,
        CancellationToken callerToken,
        string? model = null)
    {
        var category = timeoutCts.IsCancellationRequested && !callerToken.IsCancellationRequested
            ? "timeout"
            : Classify(exception);
        var message = category == "timeout"
            ? "The Copilot SDK operation timed out."
            : SafeMessage(exception);
        return new(false, category, message, host, Model: model);
    }

    internal static string Classify(Exception exception)
    {
        if (exception is TimeoutException or TaskCanceledException or OperationCanceledException)
            return "timeout";

        var message = exception.ToString().ToLowerInvariant();
        if (ContainsAny(message, "unauthorized", "authentication", "not authenticated", "login", "credential", "token", "401", "403"))
            return "authentication";
        if (ContainsAny(message, "copilot_gh_host", "invalid host", "unknown host", "routing", "not found at host", "404"))
            return "routing";
        if (ContainsAny(message, "model is not", "model not", "unavailable", "unsupported model", "policy", "not enabled", "access denied"))
            return "unavailable_or_policy";
        if (ContainsAny(message, "network", "socket", "connection", "connect", "dns", "name resolution", "tls", "ssl", "http request"))
            return "network";
        if (ContainsAny(message, "json-rpc", "protocol", "broken pipe", "process exited", "runtime", "copilot cli", "executable"))
            return "runtime_or_protocol";
        return "unexpected";
    }

    private static bool ContainsAny(string value, params string[] terms) =>
        terms.Any(value.Contains);

    private static string SafeMessage(Exception exception)
    {
        var message = exception.Message.Replace('\r', ' ').Replace('\n', ' ').Trim();
        message = Regex.Replace(
            message,
            @"(?i)\b(gh[pousr]_[A-Za-z0-9_]+|github_pat_[A-Za-z0-9_]+|bearer\s+\S+)\b",
            "[redacted]");
        return message.Length <= 500 ? message : message[..500] + "…";
    }

    private static async Task StopClientAsync(CopilotClient client)
    {
        try
        {
            await client.StopAsync().WaitAsync(TimeSpan.FromSeconds(5));
        }
        catch
        {
            try { await client.ForceStopAsync().WaitAsync(TimeSpan.FromSeconds(5)); }
            catch { }
        }
    }
}
