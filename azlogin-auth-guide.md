# Azure OpenAI Authentication Guide: Using `az login` (Entra ID)

This guide explains how to authenticate the Legacy Modernization Agents with Azure OpenAI using your Azure CLI credentials (`az login`), eliminating the need for hardcoded API keys.

## Overview

The application is designed to automatically support Microsoft Entra ID (formerly Azure AD) authentication when no API key is provided. This leverages the `DefaultAzureCredential` class from the Azure SDK, which attempts multiple authentication methods in order, including your local Azure CLI login.

## Prerequisites

1.  **Azure CLI**: Ensure you have the [Azure CLI installed](https://learn.microsoft.com/en-us/cli/azure/install-azure-cli).
2.  **Role Assignment**: Your user account must have the **"Cognitive Services OpenAI User"** role assigned on the Azure OpenAI resource.
    *   *Note: Being "Owner" or "Contributor" is NOT sufficient for data plane operations.*

## Setup Steps

### 1. Authenticate with Azure CLI

Open your terminal and run:

```bash
az login
```

If you have multiple subscriptions, set the active one where your Azure OpenAI resource resides:

```bash
az account set --subscription "Your-Subscription-Name-or-ID"
```

### 2. Configure Application Settings

To force the application to use `DefaultAzureCredential`, ensure the **API Key** is left empty or strictly omitted in your configuration.

**In `Config/appsettings.json`:**

```json
{
  "AISettings": {
    "ServiceType": "AzureOpenAI",
    "Endpoint": "https://your-resource-name.openai.azure.com/",
    "ApiKey": "",  // <--- Leave EMPTY to trigger DefaultAzureCredential
    "ModelId": "gpt-4o",
    "DeploymentName": "gpt-4o",
    // ...
  }
}
```

**In Environment Variables (`.env` files):**

Ensure `AZURE_OPENAI_API_KEY` is either not set or empty.

### 3. Verify Role Assignment

If you receive a `401 Unauthorized` or `403 Forbidden` error, verify your role assignment:

```bash
# Get your User Object ID
az ad signed-in-user show --query id -o tsv

# Assign the 'Cognitive Services OpenAI User' role
az role assignment create \
    --role "Cognitive Services OpenAI User" \
    --assignee <YOUR_USER_OBJECT_ID> \
    --scope /subscriptions/<SUBSCRIPTION_ID>/resourceGroups/<RESOURCE_GROUP>/providers/Microsoft.CognitiveServices/accounts/<OPENAI_RESOURCE_NAME>
```

## How It Works in Code

The application logic automatically detects if an API key is present. If missing, it falls back to `DefaultAzureCredential`.

**Logic in `Program.cs`:**

```csharp
// Program.cs
// API Key is optional if using Entra ID / DefaultAzureCredential
var apiKey = Environment.GetEnvironmentVariable("AZURE_OPENAI_API_KEY");

if (string.IsNullOrWhiteSpace(apiKey))
{
    Console.WriteLine("ℹ️  No valid API Key found. Assuming Microsoft Entra ID (DefaultAzureCredential) authentication.");
    // Triggers creation of client with DefaultAzureCredential
    chatClient = ChatClientFactory.CreateAzureOpenAIChatClientWithDefaultCredential(chatEndpoint, chatDeployment);
}
```

**Implementation in `ChatClientFactory.cs`:**

The factory method uses `DefaultAzureCredential` which checks your environment credential sources (VS Code, Visual Studio, Azure CLI, Environment Variables).

```csharp
// Agents/Infrastructure/ChatClientFactory.cs
public static IChatClient CreateAzureOpenAIChatClientWithDefaultCredential(
    string endpoint,
    string modelId,
    ILogger? logger = null)
{
    // DefaultAzureCredential automatically picks up the 'az login' context
    return CreateAzureOpenAIChatClient(endpoint, new DefaultAzureCredential(), modelId, logger);
}
```

## Troubleshooting

*   **"CredentialUnavailableException"**: Ensure you have run `az login` successfully.
*   **"401 Unauthorized"**: Check that the endpoint URL is correct and matches the resource you have access to.
*   **"403 Forbidden"**: You are missing the "Cognitive Services OpenAI User" role. Wait 5-10 minutes after assignment for propagation.
