using System.Collections.Generic;
using System.Text;
using McpChatWeb.Services;
using Xunit;

namespace McpChatWeb.Tests;

public class ProcessManagerModelSelectionTests
{
    [Fact]
    public void ResolveModelSelection_PreservesDistinctChatAndCodeModels()
    {
        var environment = new Dictionary<string, string?>
        {
            ["AZURE_OPENAI_MODEL_ID"] = "code-model",
            ["AZURE_OPENAI_CHAT_MODEL_ID"] = "chat-model"
        };

        var result = ProcessManager.ResolveModelSelection(null, environment);

        Assert.Equal("code-model", result.CodeModel);
        Assert.Equal("chat-model", result.ChatModel);
    }

    [Fact]
    public void ResolveModelSelection_DefaultsChatToCode()
    {
        var environment = new Dictionary<string, string?>
        {
            ["AISETTINGS__MODELID"] = "code-model"
        };

        var result = ProcessManager.ResolveModelSelection(null, environment);

        Assert.Equal("code-model", result.CodeModel);
        Assert.Equal("code-model", result.ChatModel);
    }

    [Fact]
    public void ResolveModelSelection_OnlyOverridesCodeRole()
    {
        var environment = new Dictionary<string, string?>
        {
            ["AZURE_OPENAI_MODEL_ID"] = "configured-code",
            ["AZURE_OPENAI_CHAT_MODEL_ID"] = "configured-chat"
        };

        var result = ProcessManager.ResolveModelSelection("run-code", environment);

        Assert.Equal("run-code", result.CodeModel);
        Assert.Equal("configured-chat", result.ChatModel);
    }

    [Fact]
    public void CopilotConfig_PersistsDistinctModelsAndCanonicalHostWithoutToken()
    {
        var builder = new StringBuilder();

        ModelConfigurationWriter.AppendCopilot(
            builder, "chat-model", "code-model", "tenant.ghe.com");

        var config = builder.ToString();
        Assert.Contains("_CHAT_MODEL=\"chat-model\"", config);
        Assert.Contains("_CODE_MODEL=\"code-model\"", config);
        Assert.Contains("AZURE_OPENAI_CHAT_MODEL_ID=\"$_CHAT_MODEL\"", config);
        Assert.Contains("AZURE_OPENAI_MODEL_ID=\"$_CODE_MODEL\"", config);
        Assert.Contains("COPILOT_GH_HOST=\"tenant.ghe.com\"", config);
        Assert.DoesNotContain("GITHUB_COPILOT_TOKEN", config);
    }
}
