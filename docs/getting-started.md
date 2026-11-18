# Getting started

**Last updated**: 2026-11-18

Reference for the [Quick Start](../README.md#-quick-start) in the README: prerequisites, Windows, AI providers and sign-in, manual setup, the folder layout and what a run generates.

## Prerequisites

| Requirement | Version | Notes |
|-------------|---------|-------|
| **.NET SDK** | 10.0+ | [Download](https://dotnet.microsoft.com/download) |
| **Docker Desktop** | Latest | Must be running for Neo4j |
| **AI Endpoint** | — | Azure endpoint + `az login`, or GitHub Copilot `copilot login`, or API Key |

### Windows

`doctor.sh` runs in **Git Bash** (from [Git for Windows](https://git-scm.com/download/win)) or **WSL2**.

| | Git Bash | WSL2 |
|---|---|---|
| Install | Git for Windows, .NET 10 SDK for Windows, Docker Desktop | .NET 10 SDK inside the distribution; Docker Desktop with WSL integration |
| Clone into | Any Windows folder | The Linux file system (`~/...`), not `/mnt/c/...`, which is slow and loses file permissions |
| Optional | `winget install jqlang.jq SQLite.SQLite` | `sudo apt install jq sqlite3` |

Notes for Git Bash:
- Scripts, patches and Dockerfiles are checked out with LF line endings whatever `core.autocrlf` says. If a clone made before this setting still fails with `$'\r': command not found`, refresh it once: `git rm --cached -r -q . && git reset --hard`.
- `doctor.sh` uses `netstat` and `taskkill` in place of `lsof`, and turns off Git Bash path conversion for commands run inside containers.
- The projects build without a native `.exe` launcher (`UseAppHost=false`), so endpoint security that blocks newly built executables does not stop `dotnet run`.

## Supported AI Providers

This project supports **three AI providers** with automatic model capability detection:

| Provider | ServiceType | Models | Auth | Interface |
|----------|------------|--------|------|-----------|
| **Azure OpenAI** | `AzureOpenAI` | `gpt-5.1-codex-mini`, `gpt-5.2-chat` | API Key or `az login` (Entra ID) | `ResponsesApiClient` (Codex) + `IChatClient` |
| **GitHub Copilot SDK** | `GitHubCopilot` or `GitHubCopilotSDK` | All Copilot models (Claude, GPT, Codex, Grok, ...) | `copilot login` or fine-grained token (`COPILOT_GITHUB_TOKEN`) | `CopilotChatClient` via stdio |
| **OpenAI** | `OpenAI` | GPT-4o, o3, etc. | OpenAI API key | `IChatClient` |

### Copilot sign-in

The Copilot SDK runs its own bundled Copilot runtime, which signs in one of two ways. `./doctor.sh setup` (or the portal's setup dialog) asks which and writes it to `Config/ai-config.local.env`:

| `COPILOT_AUTH` | Uses | Notes |
|---|---|---|
| `login` | The account from `copilot login` | `GH_TOKEN` and `GITHUB_TOKEN` in your shell are ignored, so an unrelated token cannot take over. |
| `token` | `COPILOT_GITHUB_TOKEN` | A fine-grained token (`github_pat_`) on your personal account with the **Copilot Requests** account permission. Classic `ghp_` tokens are rejected. Create one at github.com/settings/personal-access-tokens/new. |
| unset | `COPILOT_GITHUB_TOKEN` if set, otherwise the Copilot CLI's own order (`GH_TOKEN`, `GITHUB_TOKEN`, `copilot login`) | Classic `ghp_` tokens in `GH_TOKEN`/`GITHUB_TOKEN` are skipped. |

Runs print the credential they use (`🔐 GitHub Copilot auth: ...`), and `dotnet run -- list-models` shows it before listing the models your account can use. `GITHUB_COPILOT_TOKEN`, written by older versions of setup, is still read.

Setup retains both GitHub.com and GitHub Data Residency (custom host) sign-in.
For a custom host it runs `copilot login --host https://<host>` and saves
`GITHUB_HOST` in the local configuration. Login-mode token isolation preserves
that host environment for the SDK runtime. `GitHubCopilot` is the provider for
both choices; removing the unimplemented `GitHubModels` provider does not remove
custom-host Copilot authentication.

Each Copilot request times out after 15 minutes, which only guards against a request that never finishes. Set `COPILOT_REQUEST_TIMEOUT_SECONDS` to change it, for example when high-effort conversions of very large programs need longer.

**Model-Aware Reasoning** — The framework auto-detects model capabilities from the model ID and adapts its reasoning strategy:

| Model Family | Detection | Reasoning Strategy | Applied Via |
|-------------|-----------|-------------------|-------------|
| **Codex/o-series** | `codex`, `o1`, `o3` in model ID | `reasoning.effort` (low/medium/high) | Responses API or `AdditionalProperties` |
| **Claude** | `claude` in model ID | Extended thinking with `budget_tokens` | `AdditionalProperties["thinking"]` |
| **GPT** | `gpt-4`, `gpt-5` in model ID | Standard (temperature=0.1) | `ChatOptions.Temperature` |
| **Grok** | `grok` in model ID | Standard (temperature=0.1) | `ChatOptions.Temperature` |

> **All models get the same three-tier content-aware complexity scoring** — COBOL source is analyzed for SQL, CICS, REDEFINES, etc. to determine LOW/MEDIUM/HIGH complexity. The complexity tier drives both `MaxOutputTokens` sizing and the model-specific reasoning parameter.

> ⚠️ **Want to use different models?** Just change `AZURE_OPENAI_MODEL_ID` and `AZURE_OPENAI_SERVICE_TYPE`. The framework auto-detects capabilities — no code changes needed.

## Framework: Microsoft Agent Framework

This project uses **Microsoft Agent Framework** (`Microsoft.Agents.AI.*`), **not** Semantic Kernel.

```xml
<!-- From CobolToQuarkusMigration.csproj -->
<PackageReference Include="Microsoft.Agents.AI.AzureAI" Version="1.0.0-preview.*" />
<PackageReference Include="Microsoft.Agents.AI.OpenAI" Version="1.0.0-preview.*" />
<PackageReference Include="Microsoft.Extensions.AI" Version="10.0.1" />
```

**Why Agent Framework over Semantic Kernel?**
- Simpler `IChatClient` abstraction
- Native support for both Responses API and Chat Completions API which is key for being future proof for LLM Api's
- Better streaming and async patterns
- Lighter dependency footprint

## Setup details

Setup first prepares the Docker bind folders: it creates missing runtime folders
(`Logs`, `output`, `output/rekt`) and asks before repairing a folder you do not own
(`sudo` only when ownership must change; files inside are not touched). Tracked
folders (`Data`, `Config`, `source`, `tools/cobol-rekt/contracts`) must already exist.

<details>
<summary>Manual setup without the setup wizard</summary>

```bash
# 1. Configure Azure OpenAI
cp Config/ai-config.env.example Config/ai-config.local.env
# Edit: _MAIN_ENDPOINT (required), _CODE_MODEL / _CHAT_MODEL (optional)
# Auth: use 'az login' (recommended) OR set _MAIN_API_KEY
# See docs/az-login-auth-guide.md for Entra ID setup details

# 3. Start Neo4j (the password is configured in ai-config.local.env)
export NEO4J_PASSWORD="$(sed -n 's/^NEO4J_PASSWORD=//p' Config/ai-config.local.env | tr -d '"')"
docker-compose up -d neo4j

# 4. Build
dotnet build

# 5. Run the migration
./doctor.sh run
```

</details>

## 📁 Folder Structure

```
Legacy-Modernization-Agents/
├── source/                    # ⬅️ DROP YOUR COBOL AND JCL FILES HERE
│   ├── CUSTOMER.cbl
│   ├── PAYMENT.cbl
│   ├── COMMON.cpy
│   ├── NIGHTLY.jcl            # JCL jobs
│   └── PAYPROC.proc           # catalogued procedures / INCLUDE members
│
├── output/                    # ⬅️ GENERATED CODE APPEARS HERE
│   ├── java/                  # Java Quarkus output (+ <root>/jobs/ from JCL)
│   │   └── com/example/generated/
│   ├── csharp/                # C# .NET output
│   │   ├── Generated/
│   │   └── Jobs/              # jobs generated from JCL
│   └── rekt/                  # REKT and JCL analysis facts
│
├── Agents/                    # AI agent implementations
├── Config/                    # Configuration files
├── Data/                      # SQLite database (migration.db)
└── Logs/                      # Execution logs
```

**Workflow:**
1. Drop COBOL files (`.cbl`, `.cpy`) and any JCL (`.jcl`, `.proc`, `.prc`, `.inc`) into `source/`
2. Run `./doctor.sh rekt-full` to parse the COBOL and JCL before converting
3. Run `./doctor.sh run`
4. Choose target language (Java or C#)
5. Collect generated code from `output/java/` or `output/csharp/`; `jobs-manifest.json` there lists the jobs generated from JCL and the programs they still need

## 📊 What Gets Generated

| Input | Output |
|-------|--------|
| `source/CUSTOMER.cbl` | `output/java/com/example/generated/CustomerService.java` |
| `source/PAYMENT.cbl` | `output/csharp/Generated/PaymentProcessor.cs` |
| Analysis | `output/reverse-engineering-details.md` |
| Report | `output/migration_report_run_X.md` |
