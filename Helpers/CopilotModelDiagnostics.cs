using System.Net.Http;
using System.Net.Sockets;
using System.Text;
using System.Text.Json;
using System.Text.RegularExpressions;
using GitHub.Copilot;

namespace CobolToQuarkusMigration.Helpers;

public sealed record CopilotDiagnosticResult(
    bool Success,
    string Category,
    string Message,
    string Host,
    IReadOnlyList<string>? Models = null,
    string? Model = null,
    string? CliPath = null,
    string? CliVersion = null,
    string? ExpectedCliVersion = null);

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

        // Always report which CLI was used. Protocol failures are almost always
        // a CLI/SDK payload-shape mismatch, and without the version this is
        // indistinguishable from a genuine outage.
        var cliPath = CopilotCliInfo.ResolvePath();
        var cliVersion = CopilotCliInfo.TryGetVersion(cliPath, TimeSpan.FromSeconds(10));
        var expected = CopilotCliInfo.ExpectedVersion;

        if (CopilotModelDiagnostics.IsPayloadShapeMismatch(exception))
            message = AppendRemedy(message, cliVersion, expected);

        return new(false, category, message, host, Model: model,
            CliPath: cliPath, CliVersion: cliVersion,
            ExpectedCliVersion: string.IsNullOrEmpty(expected) ? null : expected);
    }

    /// <summary>
    /// Turns a raw protocol error into guidance. The SDK binds the CLI's
    /// JSON-RPC payloads to fixed DTOs, so a CLI newer or older than the one the
    /// SDK was built against can fail the startup handshake on a field the SDK
    /// cannot parse. The remedy is always to align the two versions.
    /// </summary>
    private static string AppendRemedy(string message, string? cliVersion, string? expectedVersion)
    {
        var found = string.IsNullOrWhiteSpace(cliVersion) ? "unknown" : cliVersion;
        var wanted = string.IsNullOrWhiteSpace(expectedVersion) ? "unknown" : expectedVersion;
        var mismatch = !string.IsNullOrWhiteSpace(cliVersion)
            && !string.IsNullOrWhiteSpace(expectedVersion)
            && !cliVersion.StartsWith(expectedVersion, StringComparison.OrdinalIgnoreCase);

        var remedy = mismatch
            ? $" The Copilot CLI on this machine ({found}) is not the version this build of the GitHub.Copilot.SDK expects ({wanted}), so the CLI returned a response the SDK could not parse."
            : $" This is a Copilot CLI/SDK payload mismatch, not an authentication problem (CLI {found}, SDK expects {wanted}).";

        return message + remedy +
            " Align them by installing the expected CLI (npm i -g @github/copilot@" + wanted + ")," +
            " or let the build supply a matching CLI by removing CopilotSkipCliDownload=true" +
            " (set CopilotNpmRegistryUrl to an internal mirror, or CopilotCliBinaryPath to a" +
            " pre-downloaded binary, on restricted networks).";
    }

    internal static string Classify(Exception exception)
    {
        if (exception is TimeoutException or TaskCanceledException or OperationCanceledException)
            return "timeout";

        // Classify by exception TYPE before looking at any text.
        //
        // A JSON shape mismatch between the Copilot CLI and the SDK's DTOs is a
        // protocol problem, but its text is a minefield for keyword matching:
        // System.Text.Json reports "Cannot get the value of a token type
        // 'Number' as a string" and its stack contains JsonTokenType, so a
        // naive search for "token" reported these as authentication failures
        // and sent users off to re-login for a problem that has nothing to do
        // with credentials.
        if (IsPayloadShapeMismatch(exception))
            return "runtime_or_protocol";
        if (HasInner<HttpRequestException>(exception) || HasInner<SocketException>(exception))
            return "network";

        // Match only the exception message chain — never ToString(), which
        // appends stack frames whose type and method names (JsonTokenType,
        // ThrowInvalidOperationException_ExpectedString, …) are not evidence
        // about the failure and trigger false positives.
        var message = MessageChain(exception).ToLowerInvariant();

        // Word-boundary matching for short, ambiguous terms so that "token"
        // does not match "JsonTokenType" and "401"/"403" do not match a
        // timestamp or byte offset that merely contains those digits.
        if (ContainsAny(message, "unauthorized", "authentication", "not authenticated", "credential") ||
            ContainsAnyWord(message, "login", "token", "401", "403"))
            return "authentication";
        if (ContainsAny(message, "copilot_gh_host", "invalid host", "unknown host", "routing", "not found at host") ||
            ContainsAnyWord(message, "404"))
            return "routing";
        if (ContainsAny(message, "model is not", "model not", "unavailable", "not available", "unsupported model", "policy", "not enabled", "access denied"))
            return "unavailable_or_policy";
        if (ContainsAny(message, "network", "socket", "connection", "connect", "dns", "name resolution", "tls", "ssl", "http request"))
            return "network";
        if (ContainsAny(message, "json-rpc", "protocol", "broken pipe", "process exited", "runtime", "copilot cli", "executable"))
            return "runtime_or_protocol";
        return "unexpected";
    }

    /// <summary>
    /// True when the CLI sent JSON the SDK could not bind to its DTOs — the
    /// signature of a CLI whose payload shape has drifted from the SDK build.
    /// </summary>
    internal static bool IsPayloadShapeMismatch(Exception exception) =>
        HasInner<JsonException>(exception);

    private static bool HasInner<T>(Exception exception) where T : Exception
    {
        for (var current = exception; current is not null; current = current.InnerException)
        {
            if (current is T)
                return true;
            if (current is AggregateException aggregate &&
                aggregate.InnerExceptions.Any(inner => HasInner<T>(inner)))
                return true;
        }
        return false;
    }

    private static string MessageChain(Exception exception)
    {
        var builder = new StringBuilder();
        for (var current = exception; current is not null; current = current.InnerException)
            builder.Append(current.Message).Append(' ');
        return builder.ToString();
    }

    private static bool ContainsAny(string value, params string[] terms) =>
        terms.Any(value.Contains);

    private static bool ContainsAnyWord(string value, params string[] terms) =>
        terms.Any(term => Regex.IsMatch(value, $@"\b{Regex.Escape(term)}\b"));

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
