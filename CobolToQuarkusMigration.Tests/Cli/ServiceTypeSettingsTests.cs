using CobolToQuarkusMigration.Models;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Cli;

// GITHUB_TOKEN used to be copied into ApiKey for every provider, so an Azure setup relying on
// Entra ID switched to key auth with a GitHub token whenever one was exported in the shell.
[Collection("EnvironmentSensitive")]
public class ServiceTypeSettingsTests
{
    private static readonly string[] Keys =
    {
        "AZURE_OPENAI_SERVICE_TYPE", "AZURE_OPENAI_API_KEY", "AZURE_OPENAI_CHAT_API_KEY",
        "AZURE_OPENAI_ENDPOINT", "AZURE_OPENAI_CHAT_ENDPOINT", "GITHUB_TOKEN",
    };

    private static AISettings Override(string serviceType, string? githubToken)
    {
        var saved = Keys.ToDictionary(k => k, Environment.GetEnvironmentVariable);
        try
        {
            foreach (var k in Keys) Environment.SetEnvironmentVariable(k, null);
            Environment.SetEnvironmentVariable("AZURE_OPENAI_SERVICE_TYPE", serviceType);
            Environment.SetEnvironmentVariable("GITHUB_TOKEN", githubToken);

            var settings = new AppSettings { AISettings = new AISettings() };
            Program.OverrideSettingsFromEnvironment(settings);
            return settings.AISettings;
        }
        finally
        {
            foreach (var (k, v) in saved) Environment.SetEnvironmentVariable(k, v);
        }
    }

    [Fact]
    public void GitHubToken_DoesNotBecomeTheAzureKey()
    {
        var ai = Override("AzureOpenAI", "gho_example");

        ai.ApiKey.Should().BeEmpty("an empty key is what selects Entra ID");
        ai.ChatApiKey.Should().BeNullOrEmpty();
    }

    [Theory]
    [InlineData("GitHubCopilot")]
    [InlineData("GitHubCopilotSDK")]
    public void Copilot_IgnoresGitHubTokenAndGitHubModelsEndpoint(string serviceType)
    {
        var ai = Override(serviceType, "gho_example");

        ai.ApiKey.Should().BeEmpty();
        ai.Endpoint.Should().NotContain("models.github.ai");
    }
}
