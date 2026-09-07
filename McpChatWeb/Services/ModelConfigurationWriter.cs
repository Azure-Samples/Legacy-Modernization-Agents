using System.Text;

namespace McpChatWeb.Services;

internal static class ModelConfigurationWriter
{
    internal static void AppendCopilot(
        StringBuilder builder,
        string chatModel,
        string codeModel,
        string githubHost)
    {
        builder.AppendLine("# Provider: GitHub Copilot SDK");
        builder.AppendLine("AZURE_OPENAI_SERVICE_TYPE=\"GitHubCopilot\"");
        builder.AppendLine();
        builder.AppendLine("# Model Selection");
        builder.AppendLine($"_CHAT_MODEL=\"{chatModel}\"");
        builder.AppendLine($"_CODE_MODEL=\"{codeModel}\"");
        builder.AppendLine();
        builder.AppendLine("# System mapping (model IDs for the application)");
        builder.AppendLine("AZURE_OPENAI_MODEL_ID=\"$_CODE_MODEL\"");
        builder.AppendLine("AZURE_OPENAI_DEPLOYMENT_NAME=\"$_CODE_MODEL\"");
        builder.AppendLine("AZURE_OPENAI_CHAT_MODEL_ID=\"$_CHAT_MODEL\"");
        builder.AppendLine("AZURE_OPENAI_CHAT_DEPLOYMENT_NAME=\"$_CHAT_MODEL\"");
        builder.AppendLine("AISETTINGS__MODELID=\"$_CODE_MODEL\"");
        builder.AppendLine("AISETTINGS__DEPLOYMENTNAME=\"$_CODE_MODEL\"");
        builder.AppendLine("AISETTINGS__CHATMODELID=\"$_CHAT_MODEL\"");
        builder.AppendLine("AISETTINGS__CHATDEPLOYMENTNAME=\"$_CHAT_MODEL\"");
        builder.AppendLine();
        builder.AppendLine("# Specialized Agent Models (defaults to Code Model)");
        builder.AppendLine("AZURE_OPENAI_COBOL_ANALYZER_MODEL=\"$_CODE_MODEL\"");
        builder.AppendLine("AZURE_OPENAI_JAVA_CONVERTER_MODEL=\"$_CODE_MODEL\"");
        builder.AppendLine("AZURE_OPENAI_DEPENDENCY_MAPPER_MODEL=\"$_CODE_MODEL\"");
        builder.AppendLine("AZURE_OPENAI_UNIT_TEST_MODEL=\"$_CODE_MODEL\"");
        builder.AppendLine("AISETTINGS__COBOLANALYZERMODELID=\"$_CODE_MODEL\"");
        builder.AppendLine("AISETTINGS__JAVACONVERTERMODELID=\"$_CODE_MODEL\"");
        builder.AppendLine("AISETTINGS__UNITTESTMODELID=\"$_CODE_MODEL\"");
        builder.AppendLine("AISETTINGS__DEPENDENCYMAPPERMODELID=\"$_CODE_MODEL\"");
        builder.AppendLine();
        builder.AppendLine("# Not needed for Copilot SDK but set to avoid validation errors");
        builder.AppendLine("AZURE_OPENAI_ENDPOINT=\"https://copilot-sdk-placeholder\"");
        builder.AppendLine("AISETTINGS__ENDPOINT=\"https://copilot-sdk-placeholder\"");
        builder.AppendLine("AISETTINGS__CHATENDPOINT=\"https://copilot-sdk-placeholder\"");
        builder.AppendLine();
        builder.AppendLine("# GitHub Copilot routing");
        builder.AppendLine($"COPILOT_GH_HOST=\"{githubHost}\"");
        builder.AppendLine($"GITHUB_HOST=\"{githubHost}\"");
        builder.AppendLine();
        builder.AppendLine("# Application Settings");
        builder.AppendLine("COBOL_SOURCE_FOLDER=\"source\"");
        builder.AppendLine("JAVA_OUTPUT_FOLDER=\"output/java\"");
        builder.AppendLine("CSHARP_OUTPUT_FOLDER=\"output/csharp\"");
    }
}
