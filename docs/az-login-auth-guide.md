**Last updated**: 2026-04-28

# Azure AD / Entra ID Authentication Guide

This guide explains how to authenticate to Azure OpenAI using Azure Active Directory (Entra ID) instead of an API key. Keyless authentication is the recommended approach for developer workstations and CI/CD environments.

---

## Prerequisites

- [Azure CLI](https://learn.microsoft.com/en-us/cli/azure/install-azure-cli) installed (`az --version`)
- An Azure OpenAI resource in your subscription
- The **Cognitive Services OpenAI User** role assigned to your account on the Azure OpenAI resource

---

## Step 1: Log in with Azure CLI

```bash
az login
```

This opens a browser window for interactive login. For headless environments (CI, SSH):

```bash
az login --use-device-code
```

Verify the correct subscription is active:

```bash
az account show
az account set --subscription "<subscription-id>"
```

---

## Step 2: Assign the Required Role

Your Azure AD identity (user or service principal) must have the **Cognitive Services OpenAI User** role on the Azure OpenAI resource.

```bash
# Get the resource ID of your Azure OpenAI resource
RESOURCE_ID=$(az cognitiveservices account show \
  --name "<your-openai-resource-name>" \
  --resource-group "<your-resource-group>" \
  --query id -o tsv)

# Assign the role to your current signed-in identity
az role assignment create \
  --assignee "$(az ad signed-in-user show --query id -o tsv)" \
  --role "Cognitive Services OpenAI User" \
  --scope "$RESOURCE_ID"
```

Role assignments can take a few minutes to propagate.

---

## Step 3: Configure the Application

In `Config/ai-config.local.env`, set the endpoint but leave `_MAIN_API_KEY` empty:

```env
_MAIN_ENDPOINT=https://<your-resource-name>.openai.azure.com/
_CODE_MODEL=gpt-5.1-codex-mini
_CHAT_MODEL=gpt-5.1-chat
# _MAIN_API_KEY is intentionally omitted — az login token is used instead
```

When `_MAIN_API_KEY` is empty, the application uses `DefaultAzureCredential`, which resolves credentials in this order:

1. `AZURE_*` environment variables (service principal)
2. Workload identity (Azure Kubernetes Service)
3. Managed identity (Azure VM / App Service)
4. Azure CLI (`az login`)
5. Azure PowerShell
6. Interactive browser login

For local development, `az login` (step 1) is the simplest option.

---

## Step 4: Verify Authentication

Run the health check to confirm credentials are working:

```bash
./doctor.sh
```

Look for:

```
✅ Azure OpenAI endpoint reachable
✅ Authentication: DefaultAzureCredential (az login)
```

If you see `401 Unauthorized`, verify:
- The role assignment has propagated (wait a few minutes and retry)
- The endpoint URL is correct and includes the trailing `/`
- You are logged in to the correct tenant (`az account show`)

---

## Service Principal Authentication (CI/CD)

For automated pipelines, use a service principal instead of interactive login:

```bash
az login --service-principal \
  --username "<app-id>" \
  --password "<client-secret>" \
  --tenant "<tenant-id>"
```

Or set environment variables before running the application:

```env
AZURE_CLIENT_ID=<app-id>
AZURE_CLIENT_SECRET=<client-secret>
AZURE_TENANT_ID=<tenant-id>
```

Ensure the service principal has the **Cognitive Services OpenAI User** role on the Azure OpenAI resource (see Step 2).

---

## Further Reading

- [Azure OpenAI — Authentication](https://learn.microsoft.com/en-us/azure/ai-services/openai/how-to/managed-identity)
- [DefaultAzureCredential documentation](https://learn.microsoft.com/en-us/dotnet/api/azure.identity.defaultazurecredential)
- [Role-based access control for Azure OpenAI](https://learn.microsoft.com/en-us/azure/ai-services/openai/how-to/role-based-access-control)
