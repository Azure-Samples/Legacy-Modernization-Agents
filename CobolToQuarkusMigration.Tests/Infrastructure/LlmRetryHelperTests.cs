using CobolToQuarkusMigration.Agents.Infrastructure;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Infrastructure;

public class LlmRetryHelperTests
{
    [Theory]
    [InlineData("30", 30)]
    [InlineData("0", 0)]
    [InlineData("   ", null)]
    [InlineData(null, null)]
    [InlineData("not-a-number", null)]
    public void ParseRetryAfter_ParsesSecondsForm(string? header, int? expectedSeconds)
    {
        var result = LlmRetryHelper.ParseRetryAfter(header);
        if (expectedSeconds is null)
            result.Should().BeNull();
        else
            result.Should().Be(TimeSpan.FromSeconds(expectedSeconds.Value));
    }

    [Fact]
    public void ParseRetryAfter_ParsesHttpDateForm()
    {
        var future = DateTimeOffset.UtcNow.AddSeconds(45);
        var header = future.ToString("R");
        var result = LlmRetryHelper.ParseRetryAfter(header);
        result.Should().NotBeNull();
        result!.Value.Should().BeGreaterThan(TimeSpan.FromSeconds(20));
        result.Value.Should().BeLessThan(TimeSpan.FromSeconds(90));
    }

    [Fact]
    public async Task ExecuteAsync_ReturnsSuccessOnFirstAttempt()
    {
        var result = await LlmRetryHelper.ExecuteAsync<int>(
            provider: "test", model: "m",
            attempt: (_, _) => Task.FromResult<CallOutcome>(new CallOutcome.Success<int>(42)),
            limiter: null,
            waitCeiling: TimeSpan.FromSeconds(120),
            logger: null,
            cancellationToken: default);
        result.Should().Be(42);
    }

    [Fact]
    public async Task ExecuteAsync_FastFailsWhenRetryAfterExceedsCeiling()
    {
        var attempts = 0;
        var act = async () => await LlmRetryHelper.ExecuteAsync<int>(
            provider: "azure-openai", model: "gpt-test",
            attempt: (_, _) =>
            {
                attempts++;
                return Task.FromResult<CallOutcome>(
                    new CallOutcome.RateLimited(TimeSpan.FromSeconds(1800), "server"));
            },
            limiter: null,
            waitCeiling: TimeSpan.FromSeconds(60),
            logger: null,
            cancellationToken: default);

        var ex = await act.Should().ThrowAsync<RateLimitedException>();
        ex.Which.RetryAfter.Should().Be(TimeSpan.FromSeconds(1800));
        attempts.Should().Be(1, "fast-fail must not retry when Retry-After exceeds ceiling");
    }

    [Fact]
    public async Task ExecuteAsync_HonoursRetryAfterUnderCeilingThenSucceeds()
    {
        var attempts = 0;
        var result = await LlmRetryHelper.ExecuteAsync<string>(
            provider: "azure-openai", model: "gpt-test",
            attempt: (_, _) =>
            {
                attempts++;
                if (attempts == 1)
                    return Task.FromResult<CallOutcome>(
                        new CallOutcome.RateLimited(TimeSpan.FromMilliseconds(50), "test"));
                return Task.FromResult<CallOutcome>(new CallOutcome.Success<string>("ok"));
            },
            limiter: null,
            waitCeiling: TimeSpan.FromSeconds(60),
            logger: null,
            cancellationToken: default);

        result.Should().Be("ok");
        attempts.Should().Be(2);
    }

    [Fact]
    public async Task ExecuteAsync_FatalSurfacesOriginalException()
    {
        var underlying = new InvalidOperationException("boom");
        var act = async () => await LlmRetryHelper.ExecuteAsync<int>(
            provider: "test", model: "m",
            attempt: (_, _) => Task.FromResult<CallOutcome>(
                new CallOutcome.Fatal(underlying, "boom")),
            limiter: null,
            waitCeiling: TimeSpan.FromSeconds(120),
            logger: null,
            cancellationToken: default);

        var ex = await act.Should().ThrowAsync<InvalidOperationException>();
        ex.Which.Should().BeSameAs(underlying);
    }

    [Fact]
    public async Task ExecuteAsync_TransientFailuresEventuallySucceed()
    {
        var attempts = 0;
        var result = await LlmRetryHelper.ExecuteAsync<int>(
            provider: "test", model: "m",
            attempt: (_, _) =>
            {
                attempts++;
                if (attempts <= 1)
                    return Task.FromResult<CallOutcome>(new CallOutcome.TransientFailure("503"));
                return Task.FromResult<CallOutcome>(new CallOutcome.Success<int>(7));
            },
            limiter: null,
            waitCeiling: TimeSpan.FromSeconds(120),
            logger: null,
            cancellationToken: default);

        result.Should().Be(7);
        attempts.Should().Be(2);
    }
}
