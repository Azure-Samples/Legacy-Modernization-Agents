using System.Text.Json;
using CobolToQuarkusMigration.Agents.Infrastructure;
using CobolToQuarkusMigration.Helpers;
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
    private readonly string _root = Path.Combine(Path.GetTempPath(), "ai-loop-events-" + Guid.NewGuid().ToString("N"));
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
        try { Directory.Delete(_root, recursive: true); } catch (IOException) { }
    }

    private sealed class ScriptedClient(params Func<ChatResponse>[] replies) : IChatClient
    {
        private int _next;

        public Task<ChatResponse> GetResponseAsync(IEnumerable<ChatMessage> messages, ChatOptions? options = null,
            CancellationToken cancellationToken = default) => Task.FromResult(replies[Math.Min(_next++, replies.Length - 1)]());

        public IAsyncEnumerable<ChatResponseUpdate> GetStreamingResponseAsync(IEnumerable<ChatMessage> messages,
            ChatOptions? options = null, CancellationToken cancellationToken = default) => throw new NotSupportedException();

        public object? GetService(Type serviceType, object? serviceKey = null) => null;
        public void Dispose() { }
    }

    private sealed class ProbeAgent(IChatClient client) : AgentBase(client, NullLogger.Instance, "probe-model")
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

    private List<JsonElement> Events(int runId)
    {
        var path = Path.Combine(_root, "output", ".metrics", $"{runId}.jsonl");
        return File.Exists(path)
            ? File.ReadAllLines(path).Select(l => JsonDocument.Parse(l).RootElement.Clone()).ToList()
            : [];
    }

    [Fact]
    public async Task ACallInsideARunIsRecordedWithItsOutcomeAndTokens()
    {
        MetricsSink.CurrentRunId = 424242;

        var (response, fellBack, _) = await new ProbeAgent(new ScriptedClient(() => Ok("done"))).Ask("A.cbl");

        response.Should().Be("done");
        fellBack.Should().BeFalse();
        var call = Events(424242).Should().ContainSingle().Subject;
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
            .Equal(AiLoopEvents.LlmCall, AiLoopEvents.LlmRetry, AiLoopEvents.LlmCall);
        var failed = Events(424243)[0];
        failed.GetProperty("success").GetBoolean().Should().BeFalse();
        failed.GetProperty("error").GetString().Should().Be("TimeoutException: slow");
        Events(424243)[1].GetProperty("reason").GetString().Should().Be("transient_error");
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
    public void LongReasonsAreCut()
    {
        AiLoopEvents.Finished(424246, "failed", DateTime.UtcNow, new string('x', 5000));

        Events(424246).Single().GetProperty("reason").GetString()!.Length.Should().BeLessThan(250);
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
