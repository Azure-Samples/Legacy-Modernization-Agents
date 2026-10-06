namespace CobolToQuarkusMigration.Helpers;

// The events behind the portal's AI Loop view: what a run set out to do, the stages it went
// through, every model call with its outcome, and every retry or fallback along the way. They
// go to the same per-run metrics file as the projection and cache events, and they are only
// emitted inside a run, so a call made outside one (the portal's prompt tools) adds nothing.
public static class AiLoopEvents
{
    public const string RunStarted = "run_started";
    public const string RunFinished = "run_finished";
    public const string Stage = "stage";
    public const string LlmCall = "llm_call";
    public const string LlmRetry = "llm_retry";
    public const string LlmFallback = "llm_fallback";

    // Provider messages can quote the request back; the reason is kept to its gist.
    private const int MaxReasonLength = 200;

    public static void Started(int runId, string mode, string targetLanguage, string outputFolder)
        => MetricsSink.Emit(runId.ToString(), new
        {
            Event = RunStarted,
            Mode = mode,
            TargetLanguage = targetLanguage,
            OutputFolder = RelativeToRepo(outputFolder),
        });

    public static void Finished(int runId, string status, DateTime startedUtc, string? reason = null)
        => MetricsSink.Emit(runId.ToString(), new
        {
            Event = RunFinished,
            Status = status,
            DurationMs = (long)(DateTime.UtcNow - startedUtc).TotalMilliseconds,
            Reason = Trim(reason),
        });

    public static void StageStarted(int number, int total, string name)
    {
        if (MetricsSink.CurrentRunId is null) return;
        MetricsSink.EmitAmbient(new { Event = Stage, Number = number, Total = total, Name = name });
    }

    public static void Call(string agent, string provider, string model, string context, long durationMs,
        bool success, int promptChars, int responseChars, long? inputTokens, long? outputTokens, string? error)
    {
        if (MetricsSink.CurrentRunId is null) return;
        MetricsSink.EmitAmbient(new
        {
            Event = LlmCall,
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
            Error = Trim(error),
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
        MetricsSink.EmitAmbient(new { Event = LlmFallback, Agent = agent, Context = context, Reason = reason, Detail = Trim(detail) });
    }

    private static string? Trim(string? s)
        => s is null ? null : s.Length <= MaxReasonLength ? s : s[..MaxReasonLength] + "…";

    // The reader resolves the folder against the repository, and an absolute path would carry
    // the machine's directory layout into the metrics.
    private static string RelativeToRepo(string folder)
    {
        var root = Environment.GetEnvironmentVariable("REPO_ROOT") ?? Directory.GetCurrentDirectory();
        var full = Path.GetFullPath(folder, root);
        var relative = Path.GetRelativePath(root, full);
        return (relative.StartsWith("..", StringComparison.Ordinal) || Path.IsPathRooted(relative)
            ? full
            : relative).Replace('\\', '/');
    }
}
