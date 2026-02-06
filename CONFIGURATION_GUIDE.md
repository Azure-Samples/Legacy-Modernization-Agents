# Configuration Guide

This project uses a layered configuration system designed to separate **secrets** (credentials) from **structure** (application behavior).

## 🚀 The 3-Layer Configuration System

| Layer | File Path | Git Tracked? | Purpose |
| :--- | :--- | :--- | :--- |
| **1. Defaults** | `Config/appsettings.json` | ✅ YES | Defines the **structure** and **default behavior** (paths, timeouts, boolean flags). |
| **2. Logic** | `Config/ai-config.env` | ✅ YES | Defines **variable mappings** and environment logic. Links generic Env Vars to Azure settings. |
| **3. Secrets** | `Config/ai-config.local.env` | ❌ NO | Stores **private keys**, **endpoints**, and **overrides**. This is your personal config. |

---

## 🚦 "Where do I check/change X?"

Use this cheat sheet to find the right file:

| I want to change... | Go to File... | Why? |
| :--- | :--- | :--- |
| **Azure Auth** | `ai-config.local.env` | Use `az login` (Recommended) or API Key. |
| **Azure Endpoint** | `ai-config.local.env` | It's specific to your environment. |
| **Model ID** (e.g., gpt-5.1 vs gpt-5.2) | `ai-config.local.env` | You might have different deployments than other devs. |
| **Context Window Size** | `appsettings.json` | It's a tuning parameter for the application logic. |
| **Folder Paths** | `appsettings.json` | It's part of the project structure. |
| **Chunking Settings** | `appsettings.json` | Controls algorithm behavior (max lines, overlap). |
| **Feature Flags** | `appsettings.json` | Enables/Disables features like Neo4j, Logging, etc. |

---

## 🛠 Detailed Breakdown

### 1. `appsettings.json` (The "Knobs")
This file is the specific configuration for the .NET application. It controls **how** the migration runs.
*   **ChunkingSettings**: Controls how we split large files.
*   **ConversionSettings**: Naming conventions (PascalCase vs camelCase).
*   **AssemblySettings**: How output files are organized (Layered vs Single file).

**Example:**
```json
"ChunkingSettings": {
    "MaxTokensPerChunk": 28000,
    "EnableParallelProcessing": true
}
```

### 2. `ai-config.env` (The "Connector")
This file acts as a bridge. It tells the application "Here is the standard name for the API Key variable".
*   It generally shouldn't be edited unless you are changing the fundamental way the app reads environment variables.
*   它 maps `_MAIN_API_KEY` -> `AZURE_OPENAI_API_KEY`.

### 3. `ai-config.local.env` (The "Keys")
**Create this file by copying `ai-config.local.env.template`.**
This is where you put the actual values that make the thing run.

**✅ Recommended: Azure Entra ID (No Keys)**
1.  Run `az login` in your terminal.
2.  Leave `_MAIN_API_KEY` empty in your local config.
3.  The app will automatically pick up your credentials.

**Example:**
```bash
_MAIN_ENDPOINT="https://my-resource.openai.azure.com/"
# Leave empty to use 'az login' (Entra ID)
_MAIN_API_KEY=""
_CHAT_MODEL="gpt-5.2-chat"
```

---

## ⚡ Precedence Rules

When the application starts, it loads configuration in this specific order (Run `doctor.sh setup` to verify):

1.  **`appsettings.json`** is loaded first (Defaults).
2.  **`ai-config.env`** is processed (Mappings).
3.  **`ai-config.local.env`** is loaded last (Overrides).
4.  **OS Environment Variables** (CLI flags) determine final overrides.

**Rule of Thumb:**
> If you set it in `ai-config.local.env`, it wins.
