# Troubleshooting setup (`./doctor.sh setup`)

**Last updated**: 2026-05-08

This guide covers setup failures where `./doctor.sh setup` exits with:

```text
❌ Example configuration file not found: .../Config/ai-config.local.env.example
```

## Why this happens

`doctor.sh setup` requires `Config/ai-config.local.env.example` as its source template.  
In some clones, that file is missing, so setup cannot create `Config/ai-config.local.env`.

The most common cause is repository ignore rules: `Config/` is broadly ignored in `.gitignore`, and if `ai-config.local.env.example` is not explicitly tracked, fresh clones do not get it.

## Quick diagnosis

From repository root:

```bash
ls -la Config/ai-config.local.env.example
```

- If it exists, re-run `./doctor.sh setup`.
- If it does not exist, continue with recovery below.

## Recovery options

### Option 1 (recommended): seed from tracked template

```bash
cp Config/ai-config.env.example Config/ai-config.local.env.example
cp Config/ai-config.local.env.example Config/ai-config.local.env
./doctor.sh setup
```

Then open `Config/ai-config.local.env` and set your real provider values (`_MAIN_ENDPOINT`, auth, model names).

### Option 2: create only the local file (skip local example)

```bash
cp Config/ai-config.env.example Config/ai-config.local.env
./doctor.sh
```

Edit `Config/ai-config.local.env` with your credentials, then continue with normal commands (`./doctor.sh test`, `./doctor.sh run`).

## Verify whether gitignore is the root cause

```bash
git check-ignore -v Config/ai-config.local.env.example
```

If output is shown, the file is being ignored by git rules in your clone.

## Preventive fix for maintainers

Ensure `Config/ai-config.local.env.example` is tracked in the repository and exempted in `.gitignore`:

```gitignore
Config/
!Config/.gitkeep
!Config/ai-config.env.example
!Config/ai-config.local.env.example
```

This keeps secret files ignored while making setup templates consistently available to new contributors.

---

## `⚠️ Could not fetch user-specific models, falling back to CLI model list`

### What triggers this

During `./doctor.sh setup`, the script calls `dotnet run -- list-models` which invokes `CopilotClient.ListModelsAsync()` from the **GitHub Copilot .NET SDK** (`GitHub.Copilot.SDK`). The SDK's `ListModelsAsync` queries the GitHub Copilot API at:

```
https://api.github.com/copilot_internal/...
```

If that call fails or returns nothing, `doctor.sh` falls back to scraping the model list from `copilot --model invalid` error output (the static CLI allow-list).

### Root cause on GitHub Enterprise (GHE)

When your organisation uses a GitHub Enterprise Server instance at `companyname.ghe.com`, the Copilot API lives at a different host:

```
https://companyname.ghe.com/api/v3/copilot_internal/...
```

The GitHub Copilot .NET SDK (`CopilotClientOptions`) has no property to override this base URL — it is hardcoded to `api.github.com`. The `CopilotCliResolver.BuildOptions()` in this project only sets `UseStdio`, `GitHubToken`, and `CliPath`; it does not (and cannot) set a custom API host.

As a result:

1. `ListModelsAsync()` is called against `api.github.com` with your GHE token.
2. The GHE token is scoped to `companyname.ghe.com` and is not accepted by `api.github.com`.
3. The call returns HTTP 401 / empty, the SDK throws, and `doctor.sh` catches the empty output and prints the warning.

### Impact

The fallback static list (scraped from the Copilot CLI binary) shows the CLI's built-in model allow-list, **not** your enterprise-approved models. You can still select a model manually — just type a model ID that is available on your GHE Copilot plan.

### Workaround

Your `Config/ai-config.local.env` already has a `GITHUB_HOST` variable:

```env
GITHUB_HOST="companyname.ghe.com"
```

The **Copilot CLI** (`copilot`) respects this variable and routes correctly to the GHE instance. However, the **GitHub Copilot .NET SDK** (`GitHub.Copilot.SDK`) does **not** read `GITHUB_HOST` — it is hardcoded to `api.github.com`. So the `list-models` call during `doctor.sh setup` still fails.

**Workaround:** after the setup wizard completes (even with the fallback list), manually edit `Config/ai-config.local.env` and set the model IDs that your enterprise Copilot plan provisions:

```env
_CHAT_MODEL="claude-sonnet-4"
_CODE_MODEL="claude-sonnet-4"
```

Everything except the model-listing step works correctly with GHE — the Copilot CLI handles actual inference calls through `GITHUB_HOST`.

> **Security note:** Never put a live PAT in a file that may be shared or screenshotted. Use a token with the minimum required scope (`copilot` for classic PATs). Rotate any token that has been exposed.

### For SDK maintainers / contributors

`CopilotClientOptions` would need a `GitHubApiBaseUrl` property to support GHE. Until the upstream SDK exposes that, the model-listing step cannot query GHE instances. Relevant code is in:

- `Program.cs` line ~258 — `BuildListModelsCommand`
- `McpChatWeb/Program.cs` line ~6215 — `/api/ai/models` endpoint
- `Agents/Infrastructure/ChatClientFactory.cs` line ~193 — chat client construction
