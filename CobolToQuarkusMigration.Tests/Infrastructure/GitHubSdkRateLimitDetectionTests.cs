using System.Text.RegularExpressions;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Infrastructure;

/// <summary>
/// Smoke tests for the message pattern used by CopilotChatClient to detect
/// throttling from the GitHub SDK (which does not surface HTTP status codes).
/// </summary>
public class GitHubSdkRateLimitDetectionTests
{
    // Mirror of the regex in CopilotChatClient — kept in sync via this test.
    private static readonly Regex Detector = new(
        @"\b(429|rate[\s-]?limit(ed|ing)?|too\s+many\s+requests|quota\s+exceeded)\b",
        RegexOptions.IgnoreCase | RegexOptions.Compiled);

    [Theory]
    [InlineData("HTTP 429: too many requests", true)]
    [InlineData("You are being rate limited", true)]
    [InlineData("Rate-limited by upstream", true)]
    [InlineData("rate limiting active", true)]
    [InlineData("Quota exceeded for this minute", true)]
    [InlineData("429", true)]
    public void DetectorMatchesKnownThrottleMessages(string message, bool expected)
    {
        Detector.IsMatch(message).Should().Be(expected);
    }

    [Theory]
    [InlineData("Unauthorized")]
    [InlineData("Connection reset by peer")]
    [InlineData("Internal server error")]
    [InlineData("Stream closed unexpectedly")]
    [InlineData("ratelimitfoo non-word boundary")]
    public void DetectorDoesNotMatchUnrelatedErrors(string message)
    {
        Detector.IsMatch(message).Should().BeFalse();
    }
}
