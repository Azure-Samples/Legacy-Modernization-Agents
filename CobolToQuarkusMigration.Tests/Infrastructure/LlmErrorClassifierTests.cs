using System.Net;
using CobolToQuarkusMigration.Agents.Infrastructure;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Infrastructure;

public class LlmErrorClassifierTests
{
    [Theory]
    [InlineData("30", 30)]
    [InlineData("0", 0)]
    [InlineData("   ", null)]
    [InlineData(null, null)]
    [InlineData("not-a-number", null)]
    public void ParseRetryAfter_ParsesSecondsForm(string? header, int? expectedSeconds)
    {
        var result = LlmErrorClassifier.ParseRetryAfter(header);
        if (expectedSeconds is null)
            result.Should().BeNull();
        else
            result.Should().Be(TimeSpan.FromSeconds(expectedSeconds.Value));
    }

    [Fact]
    public void ParseRetryAfter_ParsesHttpDateForm()
    {
        var header = DateTimeOffset.UtcNow.AddSeconds(45).ToString("R");
        var result = LlmErrorClassifier.ParseRetryAfter(header);
        result.Should().NotBeNull();
        result!.Value.Should().BeGreaterThan(TimeSpan.FromSeconds(20));
        result.Value.Should().BeLessThan(TimeSpan.FromSeconds(90));
    }

    [Fact]
    public void FromHeaders_PrefersMillisecondHeader()
    {
        var headers = new Dictionary<string, string> { ["retry-after-ms"] = "1500", ["Retry-After"] = "9" };
        LlmErrorClassifier.FromHeaders(n => headers.GetValueOrDefault(n))
            .Should().Be(TimeSpan.FromMilliseconds(1500));
    }

    [Fact]
    public void A429HttpRequestException_IsRateLimit_NotTransient()
    {
        // Before, the type check made this a 2s transient retry instead of a rate-limit wait.
        var ex = new HttpRequestException("Responses API failed with status TooManyRequests", null, HttpStatusCode.TooManyRequests);
        LlmErrorClassifier.IsRateLimit(ex).Should().BeTrue();
        LlmErrorClassifier.IsTransient(ex).Should().BeFalse();
    }

    [Fact]
    public void A503_IsTransient()
    {
        var ex = new HttpRequestException("unavailable", null, HttpStatusCode.ServiceUnavailable);
        LlmErrorClassifier.IsTransient(ex).Should().BeTrue();
        LlmErrorClassifier.IsRateLimit(ex).Should().BeFalse();
    }

    [Fact]
    public void RateLimitedException_CarriesRetryAfter()
    {
        var ex = new InvalidOperationException("wrapped",
            new RateLimitedException("p", "m", TimeSpan.FromSeconds(42), "throttled"));
        LlmErrorClassifier.IsRateLimit(ex).Should().BeTrue();
        LlmErrorClassifier.GetRetryAfter(ex).Should().Be(TimeSpan.FromSeconds(42));
    }

    [Theory]
    [InlineData("Rate limit is exceeded. Please retry after 20 seconds.", 20_000)]
    [InlineData("Too many requests, try again in 1500 ms", 1_500)]
    public void GetRetryAfter_ReadsProviderMessage(string message, int expectedMs)
    {
        LlmErrorClassifier.GetRetryAfter(new Exception(message))
            .Should().Be(TimeSpan.FromMilliseconds(expectedMs));
    }

    [Fact]
    public void RateLimitDelay_HonoursRetryAfterUnderCeiling()
    {
        LlmErrorClassifier.RateLimitDelay(1, TimeSpan.FromSeconds(30), TimeSpan.FromSeconds(120))
            .Should().Be(TimeSpan.FromSeconds(30));
    }

    [Fact]
    public void RateLimitDelay_GivesUpWhenRetryAfterExceedsCeiling()
    {
        LlmErrorClassifier.RateLimitDelay(1, TimeSpan.FromSeconds(1800), TimeSpan.FromSeconds(120))
            .Should().BeNull();
    }

    [Fact]
    public void RateLimitDelay_BacksOffWithoutHeader_CappedAtCeiling()
    {
        LlmErrorClassifier.RateLimitDelay(1, null, TimeSpan.FromSeconds(120)).Should().Be(TimeSpan.FromSeconds(8));
        LlmErrorClassifier.RateLimitDelay(2, null, TimeSpan.FromSeconds(120)).Should().Be(TimeSpan.FromSeconds(16));
        LlmErrorClassifier.RateLimitDelay(9, null, TimeSpan.FromSeconds(10)).Should().Be(TimeSpan.FromSeconds(10));
    }
}
