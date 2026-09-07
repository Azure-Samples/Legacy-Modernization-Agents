# Troubleshooting setup (`./doctor.sh setup`)

**Last updated**: 2026-09-07

`./doctor.sh setup` is the primary setup path. For Copilot, it writes a complete
`Config/ai-config.local.env` only after sign-in and model validation succeed.
For Azure OpenAI, it starts from the local template when present, otherwise the
tracked `Config/ai-config.env.example`. The wizard performs provider selection,
sign-in, model discovery or manual entry, validation, and persistence. Separate
pre-login commands and manual config editing are not required.

## GitHub Copilot routing and model setup

### GitHub Enterprise Cloud with Data Residency is not GHES

GitHub Enterprise Cloud with Data Residency uses a tenant hostname such as
`company.ghe.com`. It is a GitHub.com cloud deployment, not GitHub Enterprise
Server (GHES). Do not add `/api/v3` or another path to the host value.

The project accepts either a bare hostname or an HTTPS root URL:

```bash
export COPILOT_GH_HOST="company.ghe.com"
# Equivalent input during setup: https://company.ghe.com/
```

Values containing credentials, a non-root path, query, fragment, HTTP scheme,
or custom port are rejected.

### Host-variable precedence

Copilot routing is resolved in this order:

1. Explicit host supplied to a diagnostic or portal request.
2. `COPILOT_GH_HOST` — authoritative Copilot CLI routing variable.
3. `GITHUB_HOST` — legacy project compatibility.
4. `GH_HOST` — final compatibility fallback.
5. `github.com`.

The resolved hostname is exported to the Copilot CLI subprocess as
`COPILOT_GH_HOST`. The project does not set `GH_HOST`, because `GH_HOST` may
identify a separate GHES repository host used by the `gh` CLI.

`GitHub.Copilot.SDK` 1.0.0 launches Copilot CLI over stdio. It does not expose a
GitHub API base-URL option. Leaving `CopilotClientOptions.Environment` unset
inherits the parent process environment; this project supplies a complete copy
of that environment with only `COPILOT_GH_HOST` overlaid, preserving `PATH`,
`HOME`, proxy settings, and credential-store variables.

### Authentication

The setup wizard invokes the host-specific sign-in flow itself:

```bash
copilot login --host "https://${COPILOT_GH_HOST}"
```

No separate pre-login command is required.

For token-based automation, current Copilot CLI supports fine-grained personal
access tokens with the **Copilot Requests** permission. Classic PATs are not
supported. The wizard exports the supplied token as `COPILOT_GITHUB_TOKEN` for
discovery and validation. It does not print token fragments. To preserve the
existing compatibility mechanism, the generated local configuration stores the
token once as `GITHUB_COPILOT_TOKEN` and maps `COPILOT_GITHUB_TOKEN` to that
value. Keep `Config/ai-config.local.env` private.

Runtime token precedence is `COPILOT_GITHUB_TOKEN`, `GH_TOKEN`, `GITHUB_TOKEN`,
then legacy `GITHUB_COPILOT_TOKEN`.

### Model discovery and manual selection

The current Copilot CLI has no documented non-interactive catalog command;
`/model` is interactive. Automatic discovery therefore uses the SDK's
`ListModelsAsync` path:

```bash
dotnet run --project CobolToQuarkusMigration.csproj -- \
  list-models --format json --timeout-seconds 45
```

Discovery is bounded. If it fails or returns no models, `doctor.sh setup` and
the portal allow immediate manual entry instead of launching an interactive
fallback or selecting an arbitrary model.

Each distinct selected model is validated with a minimal tool-free request
before configuration is saved:

```bash
dotnet run --project CobolToQuarkusMigration.csproj -- \
  validate-model --model MODEL_ID --format json --timeout-seconds 60
```

The code model defaults to the chat model when omitted. When both are provided,
they remain separate through persisted configuration, portal active-chat
selection, managed conversion runs, Prompt Studio, and MCP subprocesses.

### Diagnostic categories

JSON diagnostics classify failures as `authentication`, `routing`,
`unavailable_or_policy`, `network`, `timeout`, `runtime_or_protocol`, or
`unexpected`. Classification is conservative because SDK 1.0.0 often surfaces
runtime failures as message-only exceptions.

The portal keeps non-AI views available when Copilot readiness fails. Re-open
**Setup** to correct the host, authentication, or model ID.

### Validation status

Local tests cover hostname normalization and precedence, environment
propagation, bounded timeout behavior, provider aliases, and separate chat/code
model handling. Real authentication, model discovery, and inference against a
GitHub Enterprise Cloud Data Residency tenant remain pending validation by a
contributor with access to that tenant.
