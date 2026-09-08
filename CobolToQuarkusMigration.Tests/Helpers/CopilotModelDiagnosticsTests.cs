using System.Text.Json;
using System.Text.Json.Serialization;
using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

public class CopilotModelDiagnosticsTests
{
    private sealed record PingLike(
        [property: JsonPropertyName("message")] string Message,
        [property: JsonPropertyName("timestamp")] DateTimeOffset Timestamp);

    /// <summary>
    /// Produces the real exception the Copilot SDK raises when the CLI answers
    /// the startup ping with an epoch-millisecond number while the SDK's DTO
    /// declares DateTimeOffset. Generated rather than hard-coded so the test
    /// keeps matching whatever System.Text.Json actually reports.
    /// </summary>
    private static Exception PingTimestampMismatch()
    {
        try
        {
            JsonSerializer.Deserialize<PingLike>(
                """{"message":"pong","timestamp":1757315678901}""");
            throw new InvalidOperationException("Expected deserialization to fail.");
        }
        catch (JsonException ex)
        {
            return ex;
        }
    }

    [Fact]
    public void Classify_TreatsCliPayloadMismatchAsProtocolNotAuthentication()
    {
        var exception = PingTimestampMismatch();

        // Guard the regression: the SDK's inner message literally contains the
        // word "token" ("Cannot get the value of a token type 'Number' as a
        // string"), which previously matched the authentication keyword list.
        exception.ToString().Should().Contain("token");

        CopilotModelDiagnostics.Classify(exception).Should().Be("runtime_or_protocol");
    }

    [Fact]
    public void ExitCodeFor_DoesNotReportProtocolFailureAsAuthentication()
    {
        CopilotModelDiagnostics.ExitCodeFor("runtime_or_protocol")
            .Should().NotBe(CopilotModelDiagnostics.ExitCodeFor("authentication"));
    }

    [Theory]
    [InlineData("Bad credentials")]
    [InlineData("HTTP 401 returned from host")]
    [InlineData("You are not authenticated. Run copilot login.")]
    public void Classify_StillDetectsGenuineAuthenticationFailures(string message)
    {
        CopilotModelDiagnostics.Classify(new InvalidOperationException(message))
            .Should().Be("authentication");
    }

    [Fact]
    public void Classify_DoesNotMatchAuthKeywordsInsideLongerWords()
    {
        // "JsonTokenType" must not be read as "token".
        CopilotModelDiagnostics.Classify(
                new InvalidOperationException("Unexpected JsonTokenType while reading the response."))
            .Should().NotBe("authentication");
    }

    [Fact]
    public void Classify_IgnoresStackFrameNoiseAndUsesTheMessageChain()
    {
        var inner = new InvalidOperationException("Cannot get the value of a token type 'Number' as a string.");
        var outer = new JsonException("The JSON value could not be converted to System.DateTimeOffset.", inner);

        CopilotModelDiagnostics.Classify(outer).Should().Be("runtime_or_protocol");
    }

    [Theory]
    [InlineData("Request session.create failed: Model \"bogus\" is not available.")]
    [InlineData("The requested model is unavailable for your plan.")]
    public void Classify_ReportsModelProblemsRatherThanProtocol(string message)
    {
        // These arrive wrapped in "Communication error with Copilot CLI: …",
        // whose "copilot cli" text would otherwise win and hide the real cause.
        var wrapped = new InvalidOperationException($"Communication error with Copilot CLI: {message}");
        CopilotModelDiagnostics.Classify(wrapped).Should().Be("unavailable_or_policy");
    }

    [Fact]
    public void IsPayloadShapeMismatch_OnlyTrueForJsonBindingFailures()
    {
        CopilotModelDiagnostics.IsPayloadShapeMismatch(PingTimestampMismatch()).Should().BeTrue();

        // A plain protocol/transport error must not be blamed on a CLI version
        // mismatch, otherwise the remedy text misleads exactly like the old
        // authentication misclassification did.
        CopilotModelDiagnostics.IsPayloadShapeMismatch(
                new InvalidOperationException("Communication error with Copilot CLI: broken pipe"))
            .Should().BeFalse();
    }

    [Theory]
    [InlineData(typeof(TimeoutException))]
    [InlineData(typeof(TaskCanceledException))]
    public void Classify_KeepsTimeoutClassification(Type exceptionType)
    {
        var exception = (Exception)Activator.CreateInstance(exceptionType)!;
        CopilotModelDiagnostics.Classify(exception).Should().Be("timeout");
    }
}
