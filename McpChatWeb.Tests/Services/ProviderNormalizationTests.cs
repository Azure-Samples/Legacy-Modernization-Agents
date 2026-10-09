using System;
using McpChatWeb.Services;
using Xunit;

namespace McpChatWeb.Tests.Services;

public class ProviderNormalizationTests
{
    [Theory]
    [InlineData(null)]
    [InlineData("")]
    [InlineData("  ")]
    public void NoProvider_KeepsThePortalsConfiguredProvider(string? provider)
    {
        Assert.Null(ProcessManager.NormalizeProvider(provider));
    }

    [Theory]
    [InlineData("AzureOpenAI", "AzureOpenAI")]
    [InlineData("GitHubCopilot", "GitHubCopilot")]
    [InlineData("GitHubCopilotSDK", "GitHubCopilot")]
    [InlineData("CopilotSDK", "GitHubCopilot")]
    [InlineData("openai", "OpenAI")]
    public void KnownProviders_MapToTheCliServiceType(string provider, string expected)
    {
        Assert.Equal(expected, ProcessManager.NormalizeProvider(provider));
    }

    // GitHubModels used to run on the Copilot SDK while claiming to use models.github.ai.
    [Fact]
    public void GitHubModels_IsRejected()
    {
        Assert.Throws<ArgumentException>(() => ProcessManager.NormalizeProvider("GitHubModels"));
    }
}
