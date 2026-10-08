using McpChatWeb.Services;
using Xunit;

namespace McpChatWeb.Tests.Services;

public class LocalConfigFileTests
{
    private const string Previous = """
        _CHAT_MODEL="old-model"
        AZURE_OPENAI_SERVICE_TYPE="AzureOpenAI"
        COPILOT_GITHUB_TOKEN="old-token"
        NEO4J_PASSWORD="volume-password"
        REKT_NEO4J_PASSWORD="rekt-volume-password"
        NEO4J_CONTAINER="migration-neo4j-second"
        COBOL_SOURCE_FOLDER="estates/one"
        """;

    private const string Generated = """
        # Portal setup
        AZURE_OPENAI_SERVICE_TYPE="GitHubCopilot"
        _CHAT_MODEL="new-model"
        COBOL_SOURCE_FOLDER="source"
        COPILOT_AUTH="login"
        """;

    [Fact]
    public void Merge_ReplacesProviderKeysAndKeepsEverythingElse()
    {
        var merged = LocalConfigFile.MergeUnownedKeys(Generated, Previous);

        Assert.Contains("AZURE_OPENAI_SERVICE_TYPE=\"GitHubCopilot\"", merged);
        Assert.Contains("_CHAT_MODEL=\"new-model\"", merged);
        Assert.Contains("COPILOT_AUTH=\"login\"", merged);
        Assert.DoesNotContain("old-token", merged);
        Assert.DoesNotContain("old-model", merged);

        Assert.Contains("NEO4J_PASSWORD=\"volume-password\"", merged);
        Assert.Contains("REKT_NEO4J_PASSWORD=\"rekt-volume-password\"", merged);
        Assert.Contains("NEO4J_CONTAINER=\"migration-neo4j-second\"", merged);
        Assert.Contains("COBOL_SOURCE_FOLDER=\"estates/one\"", merged);
        Assert.DoesNotContain("COBOL_SOURCE_FOLDER=\"source\"", merged);
    }

    [Fact]
    public void Merge_WritesEachKeyOnce()
    {
        var merged = LocalConfigFile.MergeUnownedKeys(Generated, Previous);

        var lines = merged.Split('\n');
        Assert.Single(lines, l => l.StartsWith("COBOL_SOURCE_FOLDER="));
        Assert.Single(lines, l => l.StartsWith("NEO4J_PASSWORD="));
    }

    [Fact]
    public void Merge_WithoutPreviousFile_ReturnsGeneratedContent()
    {
        Assert.Equal(Generated, LocalConfigFile.MergeUnownedKeys(Generated, null));
    }

    [Theory]
    [InlineData("_MAIN_API_KEY", true)]
    [InlineData("AISETTINGS__CHATMODELID", true)]
    [InlineData("COPILOT_AUTH", true)]
    [InlineData("GITHUB_HOST", true)]
    [InlineData("NEO4J_PASSWORD", false)]
    [InlineData("REKT_NEO4J_BOLT_PORT", false)]
    [InlineData("JAVA_OUTPUT_FOLDER", false)]
    public void OwnedKeys_MatchDoctorSetup(string key, bool owned)
    {
        Assert.Equal(owned, LocalConfigFile.IsOwnedBySetup(key));
    }
}
