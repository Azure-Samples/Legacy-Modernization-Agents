using CobolToQuarkusMigration.Agents.Infrastructure;
using FluentAssertions;
using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging.Abstractions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Agents.Infrastructure;

// A failure that is retried costs one more attempt; a failure wrongly called permanent drops the
// program from the run entirely, and the migration still reports the remaining files as converted.
// These are the two the Copilot path actually produces.
public sealed class TransientErrorClassificationTests
{
    private sealed class ProbeAgent : AgentBase
    {
        public ProbeAgent() : base(new NullChatClient(), NullLogger.Instance, "model") { }

        protected override string AgentName => "Probe";

        public bool Classify(Exception ex) => IsTransientError(ex);
    }

    private sealed class NullChatClient : IChatClient
    {
        public Task<ChatResponse> GetResponseAsync(
            IEnumerable<ChatMessage> messages, ChatOptions? options = null,
            CancellationToken cancellationToken = default) => throw new NotSupportedException();

        public IAsyncEnumerable<ChatResponseUpdate> GetStreamingResponseAsync(
            IEnumerable<ChatMessage> messages, ChatOptions? options = null,
            CancellationToken cancellationToken = default) => throw new NotSupportedException();

        public object? GetService(Type serviceType, object? serviceKey = null) => null;

        public void Dispose() { }
    }

    [Fact]
    public void ATimeoutIsTransientEvenWhenItsMessageNeverSaysTimeout()
    {
        // The Copilot client words its own timeout as "did not respond within 5 minutes", so a
        // substring search for "timeout" misses the one exception most clearly worth retrying.
        var ex = new TimeoutException(
            "Copilot SDK did not respond within 5 minutes. The Copilot CLI holds this credential.");

        new ProbeAgent().Classify(ex).Should().BeTrue();
    }

    [Fact]
    public void ALostCatalogueLookupIsTransient()
    {
        // Observed on a laptop resuming from sleep: the CLI cannot reach the model catalogue for
        // a moment. Six programs were abandoned by a single two-second maintenance wake.
        var ex = new InvalidOperationException(
            "Copilot SDK error: Execution failed: Error: Failed to list models");

        new ProbeAgent().Classify(ex).Should().BeTrue();
    }

    [Theory]
    [InlineData("Service temporarily unavailable")]
    [InlineData("503 backend error")]
    [InlineData("connection reset by peer")]
    public void TheExistingSignaturesStillClassify(string message)
    {
        new ProbeAgent().Classify(new InvalidOperationException(message)).Should().BeTrue();
    }

    [Theory]
    [InlineData("Model 'gpt-5.1-codex-mini' is not available.")]
    [InlineData("The COBOL source could not be parsed.")]
    public void APermanentFailureIsNotRetried(string message)
    {
        // Retrying these burns the whole estate against a fault no attempt can clear.
        new ProbeAgent().Classify(new InvalidOperationException(message)).Should().BeFalse();
    }
}
