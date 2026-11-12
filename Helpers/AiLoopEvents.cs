using CobolToQuarkusMigration.Agents.Infrastructure;

namespace CobolToQuarkusMigration.Helpers;

// Agent-boundary telemetry shares the per-run metrics sink; prompt tools outside a run emit nothing.
public static class AiLoopEvents
{
    private sealed class AttemptState
    {
        public bool Finished;
        public CancellationToken CancellationToken;
    }

    private static readonly AsyncLocal<AttemptState?> CurrentAttempt = new();
    public static CancellationToken CancellationToken => CurrentAttempt.Value?.CancellationToken ?? default;
    public static bool IsFinished => CurrentAttempt.Value?.Finished == true;

    // Routing and the selected process share one attempt; unrelated and resumed invocations do not.
    public static IDisposable BeginRun(int runId, string mode, string targetLanguage, string outputFolder,
        CancellationToken cancellationToken = default, bool reuseExistingAttempt = false)
    {
        var previousRun = MetricsSink.CurrentRunId;
        var previousAttempt = MetricsSink.CurrentAttemptId;
        var previousContexts = PreparedContexts.Value;
        var previousState = CurrentAttempt.Value;
        if (!reuseExistingAttempt || previousRun != runId || previousAttempt is null
            || previousState is null || previousState.Finished)
        {
            Started(runId, mode, targetLanguage, outputFolder);
            CurrentAttempt.Value!.CancellationToken = cancellationToken;
        }
        var correlation = LlmCorrelationContext.Begin(runId.ToString(), MetricsSink.CurrentAttemptId);
        return new RunScope(() =>
        {
            correlation.Dispose();
            MetricsSink.CurrentRunId = previousRun;
            MetricsSink.CurrentAttemptId = previousAttempt;
            PreparedContexts.Value = previousContexts;
            CurrentAttempt.Value = previousState;
        });
    }

    private sealed class RunScope(Action restore) : IDisposable
    {
        public void Dispose() => restore();
    }

    private static readonly AsyncLocal<System.Collections.Concurrent.ConcurrentDictionary<(string Agent, string File),
        (string Id, string Mode, string Block)>?> PreparedContexts = new();
    public const string RunStarted = "run_started";
    public const string RunFinished = "run_finished";
    public const string Stage = "stage";
    public const string LlmCall = "llm_call";
    public const string LlmRetry = "llm_retry";
    public const string LlmFallback = "llm_fallback";

    public const string LlmRequestStarted = "llm_request_started";

    public static void Started(int runId, string mode, string targetLanguage, string outputFolder)
    {
        MetricsSink.CurrentRunId = runId;
        MetricsSink.CurrentAttemptId = Guid.NewGuid().ToString("N");
        PreparedContexts.Value = new();
        CurrentAttempt.Value = new();
        MetricsSink.Emit(runId.ToString(), new
        {
            Event = RunStarted,
            AttemptId = MetricsSink.CurrentAttemptId,
            TelemetryVersion = 2,
            RequestCoverage = "agent-boundary",
            ManagedRunId = Environment.GetEnvironmentVariable("AI_LOOP_MANAGED_RUN_ID"),
            Mode = mode,
            TargetLanguage = targetLanguage,
            OutputFolder = RelativeToRepo(outputFolder),
        });
    }

    public static void Finished(int runId, string status, DateTime startedUtc, string? reason = null,
        string? errorType = null)
    {
        if (MetricsSink.CurrentRunId == runId && CurrentAttempt.Value is { } attempt)
        {
            if (attempt.Finished) return;
            attempt.Finished = true;
        }
        MetricsSink.Emit(runId.ToString(), new
        {
            Event = RunFinished,
            Status = status,
            DurationMs = (long)(DateTime.UtcNow - startedUtc).TotalMilliseconds,
            Reason = reason is null ? null : "run_error",
            ErrorType = errorType,
        });
        PreparedContexts.Value?.Clear();
    }

    public static string? RequestStarted(string agent, string provider, string model, string context,
        string systemPrompt, string userPrompt)
    {
        if (MetricsSink.CurrentRunId is null) return null;
        var requestId = Guid.NewGuid().ToString("N");
        MetricsSink.EmitAmbient(new
        {
            Event = LlmRequestStarted,
            RequestId = requestId,
            Agent = agent,
            Provider = provider,
            Model = model,
            Context = context,
        });
        // Preparation is not a handoff: only an exact block in the dispatched prompt is proof. The
        // block, not the request label (a basename or chunk label), binds dispatch to its source.
        if (MetricsSink.CurrentAttemptId is null || PreparedContexts.Value is not { } contexts) return requestId;
        foreach (var ((preparedAgent, file), prepared) in contexts)
        {
            if (preparedAgent != agent
                || !systemPrompt.Contains(prepared.Block, StringComparison.Ordinal)
                && !userPrompt.Contains(prepared.Block, StringComparison.Ordinal))
                continue;
            MetricsSink.EmitAmbient(new
            {
                Event = "context_dispatched",
                RequestId = requestId,
                ContextId = prepared.Id,
                Agent = agent,
                File = file,
                ProjectionMode = prepared.Mode,
            });
        }
        return requestId;
    }

    public static string? PreparedContext(string agent, string file, string mode, string? block = null)
    {
        if (MetricsSink.CurrentRunId is null || MetricsSink.CurrentAttemptId is null
            || PreparedContexts.Value is not { } contexts || string.IsNullOrEmpty(block)) return null;
        var id = Guid.NewGuid().ToString("N");
        contexts[(agent, file)] = (id, mode, block);
        return id;
    }

    public static void StageStarted(int number, int total, string name)
    {
        if (MetricsSink.CurrentRunId is null || IsFinished) return;
        MetricsSink.EmitAmbient(new { Event = Stage, Number = number, Total = total, Name = name });
    }

    public static void Call(string agent, string provider, string model, string context, long durationMs,
        bool success, int promptChars, int responseChars, long? inputTokens, long? outputTokens, string? error,
        string? requestId = null, bool cancelled = false)
    {
        if (MetricsSink.CurrentRunId is null) return;
        MetricsSink.EmitAmbient(new
        {
            Event = LlmCall,
            RequestId = requestId,
            Outcome = cancelled ? "cancelled" : success ? "succeeded" : "failed",
            Agent = agent,
            Provider = provider,
            Model = model,
            Context = context,
            DurationMs = durationMs,
            Success = success,
            PromptChars = promptChars,
            ResponseChars = responseChars,
            InputTokens = inputTokens,
            OutputTokens = outputTokens,
            Error = error?.Split(':', 2)[0],
        });
    }

    public static void Retry(string agent, string context, string reason, int attempt)
    {
        if (MetricsSink.CurrentRunId is null) return;
        MetricsSink.EmitAmbient(new { Event = LlmRetry, Agent = agent, Context = context, Reason = reason, Attempt = attempt });
    }

    public static void Fallback(string agent, string context, string reason, string? detail)
    {
        if (MetricsSink.CurrentRunId is null) return;
        MetricsSink.EmitAmbient(new { Event = LlmFallback, Agent = agent, Context = context, Reason = reason });
    }

    // External output paths must not expose the machine's directory layout.
    private static string RelativeToRepo(string folder)
    {
        try
        {
            var root = Environment.GetEnvironmentVariable("REPO_ROOT") ?? Directory.GetCurrentDirectory();
            var full = Path.GetFullPath(folder, root);
            var relative = Path.GetRelativePath(root, full);
            return (relative.StartsWith("..", StringComparison.Ordinal) || Path.IsPathRooted(relative)
                ? "[outside-output]"
                : relative).Replace('\\', '/');
        }
        catch (Exception ex) when (ex is ArgumentException or NotSupportedException or System.Security.SecurityException)
        {
            return "[invalid-output]";
        }
    }
}
