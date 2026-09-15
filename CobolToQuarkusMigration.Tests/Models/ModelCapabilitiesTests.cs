using CobolToQuarkusMigration.Models;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Models;

public class ModelCapabilitiesTests
{
    // gpt-6 matched none of the literals the detector looked for, so it fell through to the
    // permissive default and every call was rejected with "temperature does not support 0.1".
    [Theory]
    [InlineData("gpt-5-mini")]
    [InlineData("gpt-5.2-chat")]
    [InlineData("gpt-6-astra")]
    [InlineData("gpt-7-whatever-comes-next")]
    public void Gpt5AndNewer_DoNotAcceptATemperature(string modelId)
    {
        ModelCapabilities.Detect(modelId).SupportsTemperature.Should().BeFalse();
    }

    [Theory]
    [InlineData("gpt-4o")]
    [InlineData("gpt-4-turbo")]
    public void Gpt4AndOlder_StillAcceptATemperature(string modelId)
    {
        ModelCapabilities.Detect(modelId).SupportsTemperature.Should().BeTrue();
    }

    [Theory]
    [InlineData("gpt-6-astra")]
    [InlineData("gpt-5.2-chat")]
    public void Gpt5AndNewer_GetTheLargerContextWindow(string modelId)
    {
        ModelCapabilities.Detect(modelId).ContextWindowSize.Should().Be(200_000);
    }

    [Fact]
    public void AModelWithNoGptVersion_IsNotTreatedAsOpenAI()
    {
        ModelCapabilities.Detect("claude-sonnet-4").Family.Should().NotBe(ModelFamily.OpenAI);
    }
}
