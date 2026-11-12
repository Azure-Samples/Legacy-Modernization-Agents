using System.Text.Json;
using CobolToQuarkusMigration.Agents;
using CobolToQuarkusMigration.Agents.Infrastructure;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using FluentAssertions;
using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging.Abstractions;
using Xunit;
using ChatMessage = Microsoft.Extensions.AI.ChatMessage;

namespace CobolToQuarkusMigration.Tests.Agents.Infrastructure;

// The portal's AI Loop view is only as good as what the agents write. These pin that a call made
// inside a run is recorded with its outcome, that its retries and fallbacks are recorded, and that a
// call outside a run writes nothing.
[Collection("EnvironmentSensitive")]
public sealed class AiLoopEventsTests : IDisposable
{
    private readonly string _root = Path.Combine(Directory.GetCurrentDirectory(), ".ai-loop-events-" + Guid.NewGuid().ToString("N"));
    private readonly string? _originalRoot = Environment.GetEnvironmentVariable("REPO_ROOT");

    public AiLoopEventsTests()
    {
        Directory.CreateDirectory(_root);
        Environment.SetEnvironmentVariable("REPO_ROOT", _root);
    }

    public void Dispose()
    {
        Environment.SetEnvironmentVariable("REPO_ROOT", _originalRoot);
        MetricsSink.CurrentRunId = null;
        MetricsSink.CurrentAttemptId = null;
        try { Directory.Delete(_root, recursive: true); } catch (IOException) { }
    }

    private sealed class ScriptedClient(params Func<ChatResponse>[] replies) : IChatClient
    {
        private int _next;
        public TaskCompletionSource<ChatResponse>? Pending { get; init; }
        public TaskCompletionSource Invoked { get; } = new(TaskCreationOptions.RunContinuationsAsynchronously);

        public Task<ChatResponse> GetResponseAsync(IEnumerable<ChatMessage> messages, ChatOptions? options = null,
            CancellationToken cancellationToken = default)
        {
            Invoked.TrySetResult();
            return Pending?.Task ?? Task.FromResult(replies[Math.Min(_next++, replies.Length - 1)]());
        }

        public IAsyncEnumerable<ChatResponseUpdate> GetStreamingResponseAsync(IEnumerable<ChatMessage> messages,
            ChatOptions? options = null, CancellationToken cancellationToken = default) => throw new NotSupportedException();

        public object? GetService(Type serviceType, object? serviceKey = null) => null;
        public void Dispose() { }
    }

    private sealed class RecordingLimiter : IRateLimiter
    {
        public int Acquired { get; private set; }
        public int Committed { get; private set; }
        public Exception? CommitError { get; init; }
        public TaskCompletionSource? Permit { get; init; }
        public async Task<IRateLimitReservation> AcquireAsync(int estimatedTokens, CancellationToken cancellationToken = default)
        {
            if (Permit is not null) await Permit.Task;
            Acquired++;
            return new Reservation(this);
        }
        public void NoteRateLimitResponse(TimeSpan retryAfter) { }
        private sealed class Reservation(RecordingLimiter owner) : IRateLimitReservation
        {
            public void Commit(int actualTokens)
            {
                owner.Committed++;
                if (owner.CommitError is { } error) throw error;
            }
            public void Cancel() { }
            public void Dispose() { }
        }
    }

    private sealed class ProbeAgent(IChatClient client, RecordingLimiter? limiter = null, ChatLogger? chatLogger = null)
        : AgentBase(client, NullLogger.Instance, "probe-model", chatLogger: chatLogger, rateLimiter: limiter ?? new RecordingLimiter())
    {
        protected override string AgentName => "Probe";

        public Task<(string Response, bool UsedFallback, string? FallbackReason)> Ask(string context) =>
            ExecuteWithFallbackAsync("system", "user prompt", context, maxRetries: 2);
    }

    private static ChatResponse Ok(string text) => new(new ChatMessage(ChatRole.Assistant, text))
    {
        FinishReason = ChatFinishReason.Stop,
        Usage = new UsageDetails { InputTokenCount = 12, OutputTokenCount = 3 },
    };

    private sealed class ResponsesProbe(ResponsesApiClient client) : AgentBase(client, NullLogger.Instance, "probe-model")
    {
        protected override string AgentName => "Probe";
        public Task<(string Response, bool UsedFallback, string? FallbackReason)> Ask() =>
            ExecuteWithFallbackAsync("system", "user prompt", "G.cbl");
    }

    private sealed class ScriptedTransport(params string[] replies) : HttpMessageHandler
    {
        public int Calls { get; private set; }
        protected override Task<HttpResponseMessage> SendAsync(HttpRequestMessage request, CancellationToken cancellationToken)
            => Task.FromResult(new HttpResponseMessage(System.Net.HttpStatusCode.OK)
            {
                Content = new StringContent(replies[Calls++], System.Text.Encoding.UTF8, "application/json")
            });
    }

    private List<JsonElement> Events(int runId)
    {
        var path = Path.Combine(_root, "output", ".metrics", $"{runId}.jsonl");
        return File.Exists(path)
            ? File.ReadAllLines(path).Select(l => JsonDocument.Parse(l).RootElement.Clone()).ToList()
            : [];
    }

    private sealed class DelayedMetric(TaskCompletionSource preparing, TaskCompletionSource release)
    {
        public string Event
        {
            get
            {
                preparing.TrySetResult();
                if (!release.Task.Wait(TimeSpan.FromSeconds(10)))
                    throw new TimeoutException("The test did not release payload preparation.");
                return "slow";
            }
        }
    }

    [Fact]
    public async Task ConcurrentMetricsAreTimestampedAtAppendRatherThanBeforeSlowPayloadPreparation()
    {
        var preparing = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);
        var release = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);
        var slow = Task.Run(() => MetricsSink.Emit("424260", new DelayedMetric(preparing, release)));
        try
        {
            await preparing.Task.WaitAsync(TimeSpan.FromSeconds(10));
            MetricsSink.Emit("424260", new { Event = "fast" });
        }
        finally { release.TrySetResult(); }
        await slow;

        var events = Events(424260);
        events.Select(e => e.GetProperty("event").GetString()).Should().Equal("fast", "slow");
        var timestamps = events.Select(e => e.GetProperty("ts").GetDateTime()).ToList();
        timestamps.Should().BeInAscendingOrder();
        timestamps.Should().OnlyContain(t => t.Kind == DateTimeKind.Utc);
    }

    [Fact]
    public async Task PostCallBookkeepingFailureDoesNotInventAFailedModelCall()
    {
        using var attempt = AiLoopEvents.BeginRun(424256, "standard", "Java", _root);
        var agent = new ProbeAgent(new ScriptedClient(() => Ok("successful response")),
            new RecordingLimiter { CommitError = new InvalidOperationException("bookkeeping failed") });

        var (_, fallback, _) = await agent.Ask("A.cbl");

        fallback.Should().BeTrue();
        Events(424256).Where(e => e.GetProperty("event").GetString() == AiLoopEvents.LlmCall)
            .Should().ContainSingle().Which.GetProperty("success").GetBoolean().Should().BeTrue();
    }

    [Fact]
    public async Task ExportedChatLogsRetainRunAttemptAndRequestIdentityWithoutOverwritingOtherSessions()
    {
        using var attempt = AiLoopEvents.BeginRun(424257, "standard", "Java", _root);
        var logs = Path.Combine(_root, "logs");
        var first = new ChatLogger(NullLogger<ChatLogger>.Instance, logs);
        var second = new ChatLogger(NullLogger<ChatLogger>.Instance, logs);
        await new ProbeAgent(new ScriptedClient(() => Ok("first")), chatLogger: first).Ask("A.cbl");
        await first.SaveChatLogJsonAsync();
        await new ProbeAgent(new ScriptedClient(() => Ok("second")), chatLogger: second).Ask("B.cbl");
        await second.SaveChatLogJsonAsync();

        var files = Directory.GetFiles(logs, "*.json");
        files.Should().HaveCount(2);
        foreach (var file in files)
        {
            using var log = JsonDocument.Parse(await File.ReadAllTextAsync(file));
            var messages = log.RootElement.EnumerateArray().ToList();
            messages.Should().HaveCount(2);
            messages.Select(m => m.GetProperty("runId").GetInt32()).Should().OnlyContain(id => id == 424257);
            messages.Select(m => m.GetProperty("attemptId").GetString()).Distinct().Should().ContainSingle()
                .Which.Should().Be(MetricsSink.CurrentAttemptId);
            messages.Select(m => m.GetProperty("requestId").GetString()).Distinct().Should().ContainSingle()
                .Which.Should().NotBeNullOrEmpty();
        }
        var exports = Events(424257).Where(e => e.GetProperty("event").GetString() == "chat_log_saved").ToList();
        exports.Should().HaveCount(2);
        exports.Select(e => e.GetProperty("path").GetString()).Should()
            .BeEquivalentTo(files.Select(file => Path.GetRelativePath(_root, file).Replace('\\', '/')));
        exports.Should().OnlyContain(e => e.GetProperty("messageCount").GetInt32() == 2);
        exports.Should().OnlyContain(e => e.GetProperty("format").GetString() == "json");
    }

    [Fact]
    public async Task ChatExportUsesRecordedAttemptsRatherThanTheAmbientAttemptAtSaveTime()
    {
        var logger = new ChatLogger(NullLogger<ChatLogger>.Instance, Path.Combine(_root, "logs"));
        var attempts = new List<string>();
        foreach (var mode in new[] { "standard", "resume" })
        {
            using var attempt = AiLoopEvents.BeginRun(424258, mode, "Java", _root);
            attempts.Add(MetricsSink.CurrentAttemptId!);
            await new ProbeAgent(new ScriptedClient(() => Ok("private response")), chatLogger: logger).Ask("A.cbl");
            AiLoopEvents.Finished(424258, "completed", DateTime.UtcNow);
        }
        MetricsSink.CurrentRunId = null;
        MetricsSink.CurrentAttemptId = null;

        await logger.SaveChatLogJsonAsync();

        var exports = Events(424258).Where(e => e.GetProperty("event").GetString() == "chat_log_saved").ToList();
        exports.Select(e => e.GetProperty("attemptId").GetString()).Should().Equal(attempts);
        exports.Should().OnlyContain(e => e.GetProperty("messageCount").GetInt32() == 2);
        File.ReadAllText(Path.Combine(_root, "output", ".metrics", "424258.jsonl"))
            .Should().NotContain("private response");
    }

    [Fact]
    public async Task AnalyzerRetriesAreRecordedAndSuccessfulCallsAreNotCountedAsFailures()
    {
        using var attempt = AiLoopEvents.BeginRun(424253, "reverse-engineering", "analysis", _root);
        var agent = new CobolAnalyzerAgent(
            new ScriptedClient(() => throw new TimeoutException("private provider response"), () => Ok("analysis")),
            NullLogger<CobolAnalyzerAgent>.Instance, "probe-model", rateLimiter: new RecordingLimiter());

        var analysis = await agent.AnalyzeCobolFileAsync(new CobolFile { FileName = "A.cbl", Content = "STOP RUN." });

        analysis.RawAnalysisData.Should().Be("analysis");
        var calls = Events(424253).Where(e => e.GetProperty("event").GetString() == AiLoopEvents.LlmCall).ToList();
        calls.Select(e => e.GetProperty("success").GetBoolean()).Should().Equal(false, true);
        calls[1].GetProperty("inputTokens").GetInt64().Should().Be(12);
        Events(424253).Where(e => e.GetProperty("event").GetString() == AiLoopEvents.LlmRetry)
            .Should().ContainSingle();
        Events(424253).Select(e => e.GetProperty("attemptId").GetString()).Distinct().Should().ContainSingle();
        File.ReadAllText(Path.Combine(_root, "output", ".metrics", "424253.jsonl"))
            .Should().NotContain("private provider response");
    }

    [Fact]
    public async Task AnalyzerCancellationIsNotConvertedToFallbackAnalysis()
    {
        using var attempt = AiLoopEvents.BeginRun(424254, "reverse-engineering", "analysis", _root);
        var agent = new CobolAnalyzerAgent(
            new ScriptedClient(() => throw new OperationCanceledException("private cancellation detail")),
            NullLogger<CobolAnalyzerAgent>.Instance, "probe-model", rateLimiter: new RecordingLimiter());

        await Assert.ThrowsAsync<OperationCanceledException>(() =>
            agent.AnalyzeCobolFileAsync(new CobolFile { FileName = "A.cbl", Content = "STOP RUN." }));

        var events = Events(424254);
        events.Last().GetProperty("outcome").GetString().Should().Be("cancelled");
        events.Should().NotContain(e => e.GetProperty("event").GetString() == AiLoopEvents.LlmFallback);
    }

    [Fact]
    public async Task ScopedCancellationReachesTheModelClientAndRestoresPreviousCorrelation()
    {
        MetricsSink.CurrentRunId = 99;
        MetricsSink.CurrentAttemptId = "previous";
        using var cancellation = new CancellationTokenSource();
        var client = new CancellationClient(cancellation);
        using (AiLoopEvents.BeginRun(424255, "standard", "Java", _root, cancellation.Token))
        {
            await Assert.ThrowsAnyAsync<OperationCanceledException>(() => new ProbeAgent(client).Ask("A.cbl"));
        }

        client.Token.Should().Be(cancellation.Token);
        Events(424255).Last().GetProperty("outcome").GetString().Should().Be("cancelled");
        MetricsSink.CurrentRunId.Should().Be(99);
        MetricsSink.CurrentAttemptId.Should().Be("previous");
    }

    private sealed class CancellationClient(CancellationTokenSource source) : IChatClient
    {
        public CancellationToken Token { get; private set; }
        public Task<ChatResponse> GetResponseAsync(IEnumerable<ChatMessage> messages, ChatOptions? options = null,
            CancellationToken cancellationToken = default)
        {
            Token = cancellationToken;
            source.Cancel();
            cancellationToken.ThrowIfCancellationRequested();
            return Task.FromResult(Ok("not cancelled"));
        }
        public IAsyncEnumerable<ChatResponseUpdate> GetStreamingResponseAsync(IEnumerable<ChatMessage> messages,
            ChatOptions? options = null, CancellationToken cancellationToken = default) => throw new NotSupportedException();
        public object? GetService(Type serviceType, object? serviceKey = null) => null;
        public void Dispose() { }
    }

    [Fact]
    public async Task ACallInsideARunIsRecordedWithItsOutcomeAndTokens()
    {
        MetricsSink.CurrentRunId = 424242;

        var (response, fellBack, _) = await new ProbeAgent(new ScriptedClient(() => Ok("done"))).Ask("A.cbl");

        response.Should().Be("done");
        fellBack.Should().BeFalse();
        var events = Events(424242);
        events.Select(e => e.GetProperty("event").GetString()).Should()
            .Equal(AiLoopEvents.LlmRequestStarted, AiLoopEvents.LlmCall);
        var call = events[1];
        call.GetProperty("requestId").GetString().Should().Be(events[0].GetProperty("requestId").GetString());
        call.GetProperty("event").GetString().Should().Be(AiLoopEvents.LlmCall);
        call.GetProperty("agent").GetString().Should().Be("Probe");
        call.GetProperty("model").GetString().Should().Be("probe-model");
        call.GetProperty("context").GetString().Should().Be("A.cbl");
        call.GetProperty("success").GetBoolean().Should().BeTrue();
        call.GetProperty("inputTokens").GetInt64().Should().Be(12);
        call.GetProperty("outputTokens").GetInt64().Should().Be(3);
        call.GetProperty("responseChars").GetInt32().Should().Be(4);
    }

    [Fact]
    public async Task ARetriedFailureRecordsTheFailedCallTheRetryAndTheRecovery()
    {
        MetricsSink.CurrentRunId = 424243;
        var client = new ScriptedClient(() => throw new TimeoutException("slow"), () => Ok("second time"));

        var (response, _, _) = await new ProbeAgent(client).Ask("B.cbl");

        response.Should().Be("second time");
        Events(424243).Select(e => e.GetProperty("event").GetString()).Should()
            .Equal(AiLoopEvents.LlmRequestStarted, AiLoopEvents.LlmCall, AiLoopEvents.LlmRetry,
                AiLoopEvents.LlmRequestStarted, AiLoopEvents.LlmCall);
        var failed = Events(424243)[1];
        failed.GetProperty("success").GetBoolean().Should().BeFalse();
        failed.GetProperty("error").GetString().Should().Be("TimeoutException");
        Events(424243)[2].GetProperty("reason").GetString().Should().Be("transient_error");
        Events(424243)[3].GetProperty("requestId").GetString().Should().NotBe(failed.GetProperty("requestId").GetString());
    }

    [Fact]
    public async Task APermanentFailureIsRecordedAsAFallback()
    {
        MetricsSink.CurrentRunId = 424244;
        var client = new ScriptedClient(() => throw new InvalidOperationException("The COBOL source could not be parsed."));

        var (_, fellBack, _) = await new ProbeAgent(client).Ask("C.cbl");

        fellBack.Should().BeTrue();
        var fallback = Events(424244).Last();
        fallback.GetProperty("event").GetString().Should().Be(AiLoopEvents.LlmFallback);
        fallback.GetProperty("reason").GetString().Should().Be("non_retryable_error");
        File.ReadAllText(Path.Combine(_root, "output", ".metrics", "424244.jsonl"))
            .Should().NotContain("The COBOL source could not be parsed.");
    }

    [Fact]
    public async Task ACallOutsideARunWritesNothing()
    {
        MetricsSink.CurrentRunId = null;

        await new ProbeAgent(new ScriptedClient(() => Ok("x"))).Ask("D.cbl");

        Directory.Exists(Path.Combine(_root, "output", ".metrics")).Should().BeFalse();
    }

    [Fact]
    public void TheRunRecordsItsOutputFolderRelativeToTheRepository()
    {
        AiLoopEvents.Started(424245, "standard", "C#", Path.Combine(_root, "output", "csharp", "20260101-000000"));

        Events(424245).Single().GetProperty("outputFolder").GetString().Should().Be("output/csharp/20260101-000000");
    }

    [Fact]
    public void FailureReasonsCannotLeakProviderContent()
    {
        AiLoopEvents.Finished(424246, "failed", DateTime.UtcNow, new string('x', 5000));

        Events(424246).Single().GetProperty("reason").GetString().Should().Be("run_error");
    }

    [Fact]
    public async Task TruncationEscalationHasItsOwnCorrelatedStartAndOutcome()
    {
        MetricsSink.CurrentRunId = 424248;
        var truncated = Ok("partial");
        truncated.FinishReason = ChatFinishReason.Length;
        var limiter = new RecordingLimiter();
        var agent = new ProbeAgent(new ScriptedClient(() => truncated, () => Ok("complete")), limiter);

        var (response, fallback, _) = await agent.Ask("E.cbl");

        response.Should().Be("complete");
        fallback.Should().BeFalse();
        var events = Events(424248);
        var starts = events.Where(e => e.GetProperty("event").GetString() == AiLoopEvents.LlmRequestStarted).ToList();
        var ends = events.Where(e => e.GetProperty("event").GetString() == AiLoopEvents.LlmCall).ToList();
        starts.Should().HaveCount(2);
        ends.Should().HaveCount(2);
        ends.Select(e => e.GetProperty("requestId").GetString()).Should()
            .Equal(starts.Select(e => e.GetProperty("requestId").GetString()));
        ends.Select(e => e.GetProperty("success").GetBoolean()).Should().Equal(false, true);
        limiter.Acquired.Should().Be(2);
        limiter.Committed.Should().Be(1);
    }

    [Fact]
    public async Task ReasoningExhaustionEscalationIsInstrumentedWithoutUsingANetwork()
    {
        MetricsSink.CurrentRunId = 424251;
        var transport = new ScriptedTransport(
            """{"status":"incomplete","incomplete_details":{"reason":"max_output_tokens"},"usage":{"input_tokens":10,"output_tokens":100,"output_tokens_details":{"reasoning_tokens":100}}}""",
            """{"status":"completed","output_text":"recovered","usage":{"input_tokens":10,"output_tokens":3}}""");
        using var client = new ResponsesApiClient($"https://unit.invalid/{Guid.NewGuid():N}", "fake-key", "fake-model",
            profile: new ModelProfileSettings { TokensPerMinute = 1000000, RequestsPerMinute = 10000 }, transport: transport);

        var (response, fallback, _) = await new ResponsesProbe(client).Ask();

        response.Should().Be("recovered");
        fallback.Should().BeFalse();
        transport.Calls.Should().Be(2);
        var events = Events(424251);
        events.Select(e => e.GetProperty("event").GetString()).Should().Equal(
            AiLoopEvents.LlmRequestStarted, AiLoopEvents.LlmCall, AiLoopEvents.LlmRetry,
            AiLoopEvents.LlmRequestStarted, AiLoopEvents.LlmCall);
        events[1].GetProperty("error").GetString().Should().Be("ReasoningExhaustionException");
        events[4].GetProperty("requestId").GetString().Should().Be(events[3].GetProperty("requestId").GetString());
        events[4].GetProperty("inputTokens").ValueKind.Should().Be(JsonValueKind.Null);
    }
    [Fact]
    public async Task CancellationIsRecordedWithoutExceptionContentAndPropagates()
    {
        MetricsSink.CurrentRunId = 424249;
        var agent = new ProbeAgent(new ScriptedClient(() => throw new OperationCanceledException("private prompt or secret")));

        await Assert.ThrowsAsync<OperationCanceledException>(() => agent.Ask("F.cbl"));

        var outcome = Events(424249).Last();
        outcome.GetProperty("outcome").GetString().Should().Be("cancelled");
        outcome.GetProperty("error").GetString().Should().Be("OperationCanceledException");
        File.ReadAllText(Path.Combine(_root, "output", ".metrics", "424249.jsonl")).Should().NotContain("private prompt");
    }

    [Fact]
    public async Task AttemptsAreDistinctAndOnlyAnExactPreparedBlockProvesDispatch()
    {
        AiLoopEvents.Started(424250, "standard", "Java", Path.Combine(_root, "output", "java"));
        var first = MetricsSink.CurrentAttemptId;
        AiLoopEvents.PreparedContext("Probe", "A.cbl", "projection", "user prompt");
        AiLoopEvents.RequestStarted("Probe", "fake", "model", "A.cbl", "system", "unrelated");
        await new ProbeAgent(new ScriptedClient(() => Ok("done"))).Ask("A.cbl");
        AiLoopEvents.Finished(424250, "failed", DateTime.UtcNow);
        AiLoopEvents.Started(424250, "resume", "Java", Path.Combine(_root, "output", "java"));
        AiLoopEvents.RequestStarted("Probe", "fake", "model", "A.cbl", "system", "user prompt");

        MetricsSink.CurrentAttemptId.Should().NotBe(first);
        Events(424250).Where(e => e.GetProperty("event").GetString() == "context_dispatched")
            .Should().ContainSingle().Which.GetProperty("attemptId").GetString().Should().Be(first);
        File.ReadAllText(Path.Combine(_root, "output", ".metrics", "424250.jsonl")).Should().NotContain("user prompt");
    }

    [Fact]
    public void DispatchIsBoundToThePreparedSourceNotTheRequestLabel()
    {
        AiLoopEvents.Started(424290, "standard", "Java", Path.Combine(_root, "output", "java"));
        var id = AiLoopEvents.PreparedContext("Probe", "bank/nested/A.cbl", "projection", "context block");
        AiLoopEvents.RequestStarted("Probe", "fake", "model", "A.cbl (chunk 2/3)", "system", "prefix context block suffix");
        AiLoopEvents.RequestStarted("Other", "fake", "model", "A.cbl", "system", "context block");

        var dispatched = Events(424290).Where(e => e.GetProperty("event").GetString() == "context_dispatched")
            .Should().ContainSingle().Subject;
        dispatched.GetProperty("contextId").GetString().Should().Be(id);
        dispatched.GetProperty("file").GetString().Should().Be("bank/nested/A.cbl");
    }

    [Fact]
    public async Task ARequestBecomesInFlightOnlyAfterTheLimiterAllowsActualDispatch()
    {
        MetricsSink.CurrentRunId = 424252;
        var pending = new TaskCompletionSource<ChatResponse>(TaskCreationOptions.RunContinuationsAsynchronously);
        var permit = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);
        var client = new ScriptedClient { Pending = pending };
        var completion = new ProbeAgent(client, new RecordingLimiter { Permit = permit }).Ask("H.cbl");
        try
        {
            Events(424252).Should().BeEmpty();
            permit.SetResult();
            await client.Invoked.Task.WaitAsync(TimeSpan.FromSeconds(5));
            Events(424252).Should().ContainSingle().Which.GetProperty("event").GetString()
                .Should().Be(AiLoopEvents.LlmRequestStarted);
        }
        finally
        {
            permit.TrySetResult();
            pending.TrySetResult(Ok("complete"));
            await completion.WaitAsync(TimeSpan.FromSeconds(5));
        }
        var events = Events(424252);
        events.Should().HaveCount(2);
        events[1].GetProperty("requestId").GetString().Should().Be(events[0].GetProperty("requestId").GetString());
    }

    [Fact]
    public void StagesAreOnlyRecordedInsideARun()
    {
        MetricsSink.CurrentRunId = null;
        AiLoopEvents.StageStarted(1, 3, "Init");
        MetricsSink.CurrentRunId = 424247;
        AiLoopEvents.StageStarted(2, 3, "Convert");

        var stage = Events(424247).Should().ContainSingle().Subject;
        stage.GetProperty("name").GetString().Should().Be("Convert");
        Directory.GetFiles(Path.Combine(_root, "output", ".metrics")).Should().ContainSingle();
    }
}
