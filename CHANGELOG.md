# Changelog

All notable changes to this repository are documented here.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [3.2.0] - 2026-03-17

### Added
- **Portal AI Provider Setup Modal** — Browser-based setup replaces the need to run `./doctor.sh setup` for initial configuration. Users connect to their AI provider directly from the portal UI:
  - **Azure OpenAI**: Authenticate with API key or Azure CLI (`az login`). Auto-discovers actual deployed models via ARM management API (not the full catalog). Shows deployment name, base model, version, and SKU capacity.
  - **GitHub Copilot SDK**: Authenticate with CLI login (`gh auth login`) or Personal Access Token. Lists all available models via `CopilotClient.ListModelsAsync()` grouped by publisher (Anthropic, OpenAI, xAI, Google, etc.).
  - Assign separate **Chat** and **Code** models from the discovered list
  - Saves configuration to `Config/ai-config.local.env` — fully compatible with `./doctor.sh` CLI flow
  - Auto-opens on first visit when no models are configured (`needsSetup` detection)
  - 🔧 Setup button in the Model & Prompts config panel for reconfiguration
- **Prompt Studio Multi-Provider Support** — AI Enhance and Re-Score now work with both Azure OpenAI and GitHub Copilot SDK (previously Azure-only)
  - New `CopilotChatClient` adapter in `McpChatWeb/Services/` for Copilot SDK `IChatClient` support
  - `PromptStudioAI.CreateClient()` auto-detects provider and creates the right client
- **Model Discovery API Endpoints**:
  - `POST /api/models/connect` — Authenticate and list models from Azure OpenAI or Copilot SDK
  - `POST /api/models/save-config` — Persist model selection to env vars and config file, restart MCP
  - `GET /api/models/available` — Enhanced with `needsSetup`, `isConnected`, `connectedEndpoint` fields

### Changed
- **`/api/models/available`** — Now returns discovered models from the connect flow (not just env var configured ones). Falls back to env vars when no connect has been done.
- **Portal config output** — Includes all per-agent model overrides (`AZURE_OPENAI_COBOL_ANALYZER_MODEL`, `AISETTINGS__COBOLANALYZERMODELID`, etc.) and application settings, matching the full `ai-config.env.example` template
- **Mission Control provider dropdown** — Changing provider now opens the Setup Modal instead of showing a console message
- **Architecture Mermaid diagrams** — Updated all diagrams (architecture, sequence, config flow, process flow, legacy-modernization-flow) to reflect portal setup, dual provider paths, and expanded portal features

### Fixed
- **`FakeMcpClient`** in `McpChatWeb.Tests` — Added missing `RestartAsync` method for `IMcpClient` interface
- **Temperature not written to portal config** — Removed hardcoded `AZURE_OPENAI_TEMPERATURE` from portal-generated config (respects per-model auto-detection via `ModelCapabilities`)
- **XSS protection** — HTML-escapes all API-supplied model names/IDs before DOM insertion in the setup modal
- **URL validation** — Client-side (HTTPS check) and server-side (`Uri.TryCreate`) validation for Azure endpoints

### Security
- API keys entered in the setup modal are only stored server-side (env vars + gitignored config file), never persisted in browser storage
- Azure ARM API calls use separate `management.azure.com` token scope (not the data-plane token)

## [3.1.0] - 2026-03-10

### Added
- **GitHub Copilot SDK Support** — New `ServiceType=GitHubCopilotSDK` uses the GitHub Copilot CLI (`github-copilot-cli`) for stdio-based AI communication. Authentication is handled by the GitHub CLI login — no API keys needed.
  - `CopilotChatClient` (`Agents/Infrastructure/CopilotChatClient.cs`) — `IChatClient` adapter wrapping `GitHub.Copilot.SDK.CopilotClient`, with session-per-request and event-driven streaming via Channels
  - `ChatClientFactory.CreateGitHubCopilotChatClient()` — Factory method for Copilot SDK clients
  - `list-models` CLI command — Lists available models via `CopilotClient.ListModelsAsync()`
  - `IsGitHubCopilotSdkMode()` helper for service type detection
  - Updated `GitHub.Copilot.SDK` NuGet from 0.1.26 → 0.1.32
- **Model-Aware Reasoning for ALL Providers** — The three-tier content-aware reasoning system now works for every model family, not just Azure OpenAI Codex:
  - **Claude** → Extended thinking with `budget_tokens` (30%/50%/70% of max tokens based on effort tier)
  - **Codex/o-series via IChatClient** → `reasoning_effort` additional property
  - **GPT/Grok/standard** → `temperature=0.1` for deterministic output
  - `ModelCapabilities.Detect()` auto-classifies models from ID string (Codex, Claude, Grok, OpenAI, Unknown)
  - `CalculateTokenSettings()` on `AgentBase` and `CobolAnalyzerAgent` replaces hardcoded `MaxOutputTokens=16384`
  - `ApplyModelSpecificOptions()` adapts `ChatOptions` per model family
- **Output Truncation Detection & Recovery** — New safety net for ALL IChatClient providers:
  - `OutputTruncationException` — Thrown on `FinishReason=Length`, text-based truncation signals (`// ... remaining`, `// TODO: implement`, trailing `...`), or unclosed code blocks (odd ` ``` ` count)
  - `DetectTruncation()` in both `AgentBase` and `CobolAnalyzerAgent` — Checks every IChatClient response
  - Escalation loop: doubles `MaxOutputTokens` + promotes reasoning effort (low→medium→high) with thrash guard
  - Falls back to adaptive re-chunking (split at COBOL DIVISION/SECTION boundary) when escalation fails
- **`Create()` Static Factories** — All 7 agents now have `Create(responsesApiClient, chatClient, ...)` factory methods that auto-route to the correct constructor
- **Portal Prompt Studio: Entra ID & GitHub CLI Auth** — Phase 3 AI enhancement and score endpoints now support:
  - Azure Entra ID (`DefaultAzureCredential`) when no API key is set
  - `gh auth token` via `GetGitHubToken()` — automatically picks up GitHub CLI login
  - Added `Azure.Identity` NuGet to McpChatWeb

### Changed
- **`CODEX_*` → `AI_*` Environment Variables** — All env var names renamed from `CODEX_*` to `AI_*` (e.g., `AI_SPEED_PROFILE`, `AI_LOW_REASONING_EFFORT`, `AI_MAX_OUTPUT_TOKENS`). `CodexProfile` settings section renamed to `ModelProfile`.
- **`AgentBase` IChatClient Path** — Replaced hardcoded `MaxOutputTokens=16384` with content-aware `CalculateTokenSettings()` + `ApplyModelSpecificOptions()`. Added `ModelCapabilities`, `ModelProfileSettings Profile`, complexity scoring regexes, and `CompileIndicators()` to base class.
- **`CobolAnalyzerAgent`** — Same model-aware reasoning upgrade (own implementation since it doesn't inherit `AgentBase`)
- **`ChatLogger`** — Now accepts `providerName` parameter; log strings dynamically show "GitHub Copilot" or "Azure OpenAI" based on client type
- **`MigrationProcess` & `ChunkedMigrationProcess`** — `InitializeAgents()` simplified from ~80-line if/else blocks to one-liner `Agent.Create()` calls; ChatLogger gets `providerName`
- **`Program.cs`** — Agent creation in `RunMigrationAsync` and `RunReverseEngineeringAsync` simplified with `Create()` factories; added Copilot SDK log filter, `GitHubCopilotSDK` in all service type checks
- **`doctor.sh`** — `check_ai_connectivity()` and `run_doctor()` now handle `GitHubCopilotSDK` provider (checks for CLI presence, validates MODEL_ID only)
- **`OverrideSettingsFromEnvironment`** — Sets placeholder endpoint for `GitHubCopilotSDK`
- **`ValidateAndLoadConfiguration`** — Handles `GitHubCopilotSDK` (only requires MODEL_ID; auth via CLI)

### Removed
- **`UnifiedAIClient`** (`Agents/Infrastructure/UnifiedAIClient.cs`) — Deleted. Its functionality (model-aware reasoning for all providers) is now built directly into `AgentBase` and `CobolAnalyzerAgent`.

### Fixed
- **`_runId` Bug** — `JavaConverterAgent`, `ChunkAwareJavaConverter`, and `ChunkAwareCSharpConverter` never assigned `_runId` in one or both constructors. All fixed.
- **`ModelCapabilities` Ambiguity** — Fully qualified `CobolToQuarkusMigration.Models.ModelCapabilities.Detect()` in `Program.cs` to resolve namespace collision with `GitHub.Copilot.SDK.ModelCapabilities`
- **Missing `GitHubCopilotSDK` Endpoint Validation** — `RunMigrationAsync` endpoint check was missing `GitHubCopilotSDK`, causing valid SDK configs to be rejected
- **Portal AI Enhancement Blocked by Entra ID** — Portal Prompt Studio Phase 3 always showed "Regex Only" when using Entra ID auth (empty API key). Now supports `DefaultAzureCredential` and `gh auth token`

## [3.0.0] - 2026-02-27

### Added
- **Multi-Provider AI Support** — The application now supports three AI providers:
  - **AzureOpenAI** (existing) — Azure OpenAI deployments with Responses API for Codex models
  - **GitHubCopilot** (new) — GitHub Models catalog via `models.github.ai` endpoint, giving access to Claude Opus/Sonnet, Codex, GPT, Grok, and all other GitHub-hosted models
  - **OpenAI** (new) — Direct OpenAI API access
- **`ModelCapabilities` auto-detection** — New `Models/ModelCapabilities.cs` automatically detects model family (Codex, Claude, Grok, OpenAI) and adapts reasoning strategy accordingly:
  - Codex/o-series → `reasoning.effort` parameter via Responses API
  - Claude → extended thinking with `budget_tokens`
  - GPT/Grok/Others → standard chat completions with adaptive max tokens
- **`ChatClientFactory.CreateFromSettings()`** — Single entry point that auto-selects the right provider based on `AISettings.ServiceType`
- **`ChatClientFactory.CreateGitHubCopilotClient()`** — Creates `IChatClient` instances via the GitHub Models OpenAI-compatible endpoint
- **`GITHUB_TOKEN` environment variable** — Automatically mapped to API key for GitHub Copilot authentication
- **`GitHub.Copilot.SDK` NuGet package** — Added as a dependency
- **`Microsoft.Extensions.AI.OpenAI` NuGet 10.3.0** — Added for OpenAI-compatible `IChatClient` support

### Changed
- **All agent initialization** is now provider-aware — uses `ResponsesApiClient` when available (Azure Codex models), falls back to `IChatClient` for GitHub Copilot, OpenAI, or non-Codex Azure models
- **`MigrationProcess`**, **`ChunkedMigrationProcess`**, **`SmartMigrationOrchestrator`** — Accept nullable `ResponsesApiClient?` to support non-Azure providers
- **`ConfigureSmartChunking`** — Uses `ModelCapabilities.Detect()` instead of magic string matching for model context window detection
- **`ValidateAndLoadConfiguration`** — Provider-aware validation: GitHub Copilot needs only a GitHub PAT + model name, Azure needs endpoint + deployment
- **`OverrideSettingsFromEnvironment`** — Supports `AZURE_OPENAI_SERVICE_TYPE`, `GITHUB_TOKEN`, auto-sets GitHub Models endpoint
- **`Config/appsettings.json`** — Updated descriptions to reflect multi-provider support with model examples per provider
- **`Config/ai-config.env.example`** — Added `_SERVICE_TYPE` selector and GitHub Copilot configuration examples
- **`Config/ai-config.local.env.template`** — Added GitHub Copilot provider setup instructions and model examples
- **`CodexProfile` config section** — Description updated to clarify it works with all model families, not just Codex

## [2.5.0] - 2026-02-23

### Added
- **Business Logic Persistence** — `ReverseEngineeringProcess` and `ChunkedReverseEngineeringProcess` now persist extracted `BusinessLogic` records to a new `business_logic` SQLite table via `IMigrationRepository.SaveBusinessLogicAsync`. Added `GetBusinessLogicAsync` and `DeleteBusinessLogicAsync` to `IMigrationRepository`, `SqliteMigrationRepository`, and `HybridMigrationRepository`.
- **Business Logic Injection into Conversion Prompts** — All four converter agents (`JavaConverterAgent`, `CSharpConverterAgent`, `ChunkAwareJavaConverter`, `ChunkAwareCSharpConverter`) now receive extracted `BusinessLogic` records via `SetBusinessLogicContext()` (new method on `ICodeConverterAgent`). In full-pipeline runs, `SmartMigrationOrchestrator` wires RE output directly into conversion; `--reuse-re` loads the same context from a previous persisted RE run. A shared `FormatBusinessLogicContext()` helper in `AgentBase` formats the context for all four converters.
- **`--reuse-re` CLI flag** — When combined with `--skip-reverse-engineering`, loads business logic from the latest persisted RE run and injects it into conversion prompts. `doctor.sh convert-only` now prompts interactively for this choice.
- **REST API: `GET/DELETE /api/runs/{runId}/business-logic`** — Returns per-file business logic summary (story/feature/rule counts); DELETE removes persisted results to allow re-running RE for that run.
- **Portal: per-run `🔬 RE Results` button** — Shows the business logic summary table for a run and allows deletion of persisted results directly from the UI.
- **RE Results in Portal Chat** — Chat endpoint injects business purpose, user stories, features, and business rules from the `business_logic` table into the AI prompt context. Updated AI system prompt accordingly.

### Fixed
- **Empty Technical Analysis in RE output** — `ReverseEngineeringProcess` and `ChunkedReverseEngineeringProcess` now fall back to rendering `RawAnalysisData` when structured `CobolAnalysis` fields are unpopulated.
- **Total Features always 0** — `BusinessLogicExtractorAgent.ExtractFeatures()` now matches `### Use Case N:` and `### Operation` headings in addition to `### Feature:`, reflecting the actual AI prompt output.

### Changed
- **Dependency mapping runs once per full run** — RE processes (`ReverseEngineeringProcess`, `ChunkedReverseEngineeringProcess`) now include a dedicated dependency mapping step (step 4/5) and store the result on `ReverseEngineeringResult.DependencyMap`. `MigrationProcess` and `ChunkedMigrationProcess` accept a `SetDependencyMap()` call and skip `AnalyzeDependenciesAsync` when a map is already provided. `SmartMigrationOrchestrator.RunAsync` threads `existingDependencyMap` through to both migration paths. Dependency output files (`dependency-map.json`, `dependency-diagram.md`) are now generated in the RE output folder as well as the migration output folder.
- **`doctor.sh`** — Updated `convert-only` to prompt for `--reuse-re`; corrected portal navigation references to match current UI (`'📄 Reverse Engineering Results'`).

## [2.4.0] - 2026-02-16

### Added
- **Automated Documentation Checker** — New GitHub Actions workflow (`documentation-updater`) that reviews code changes on every push and PR to `main`, identifies missing or outdated documentation, and notifies the responsible author via PR comments or issues.
- **Speed Profile Selection** - New interactive prompt in `doctor.sh` lets you choose between four speed profiles before running migrations, reverse engineering, or conversion-only:
  - **TURBO** — Low reasoning on ALL files with no exceptions. 65K token ceiling, parallel file conversion (4 workers), 200ms stagger delay. Designed for testing and smoke runs where speed matters more than quality.
  - **FAST** — Low reasoning on most files, medium only on the most complex ones. 32K token cap, parallel conversion (3 workers), 500ms stagger. Good for quick iterations and proof-of-concept runs.
  - **BALANCED** (default) — Uses the three-tier content-aware reasoning system. Simple files get low effort, complex files get high effort. Parallel conversion (2 workers), 1s stagger.
  - **THOROUGH** — Maximum reasoning on all files regardless of complexity. Parallel conversion (2 workers), 1.5s stagger. Best for critical codebases where accuracy matters more than speed.
- **Shared `select_speed_profile()` function** — Called from `run_migration()`, `run_reverse_engineering()`, and `run_conversion_only()`. Sets `CODEX_*` environment variables that are picked up by `Program.cs` `OverrideSettingsFromEnvironment()` at startup — no C# changes needed.
- **Adaptive Re-Chunking on Output Exhaustion** — When reasoning exhaustion retries fail (all escalation attempts exhausted), `AgentBase` now automatically splits the COBOL source at the best semantic boundary (DIVISION > SECTION > paragraph > midpoint) and processes each half independently with a 50-line context window (second half begins 50 lines before the split point for continuity). Results are merged with duplicate package/import/class removal and validated for truncation signals. This solves the TURBO/FAST paradox where small output token caps caused repeated exhaustion failures rather than triggering the existing input-size-based chunking.
- **Parallel File Conversion** — All 4 converter agents (`ChunkAwareJavaConverter`, `ChunkAwareCSharpConverter`, `JavaConverterAgent`, `CSharpConverterAgent`) now support parallel file conversion via `SemaphoreSlim`-based concurrency control. Controlled by `MaxParallelConversion` setting (default: 2). TURBO uses 4 workers, FAST uses 3, BALANCED/THOROUGH use 2.
- **Environment Variable Overrides for Timing** — New env vars `CODEX_STAGGER_DELAY_MS`, `CODEX_MAX_PARALLEL_CONVERSION`, and `CODEX_RATE_LIMIT_SAFETY_FACTOR` allow fine-tuning of parallelism and rate limiting without code changes.

### Fixed
- **Settings Injection Bug** — All agent constructors in `MigrationProcess.cs`, `ChunkedMigrationProcess.cs`, and `Program.cs` were missing the `settings` parameter, causing `AppSettings` to always be `null` inside agents. As a result, runtime configuration (including environment variable overrides such as `CODEX_MAX_PARALLEL_CONVERSION`) could not be applied, and agents fell back to the default `MaxParallelConversion` value of 1 (sequential). All 10 constructor call sites now pass `settings` correctly so both static config and env var overrides take effect as intended.
- **Hardcoded Rate Limit Safety Margin** — `RateLimitTracker.SafetyMargin` was hardcoded at 0.90, ignoring the configurable `RateLimitSafetyFactor` from `ChunkingSettings`. Now accepts a `safetyMargin` parameter wired from settings (TURBO=0.85, default=0.70).

### Changed
- **README.md** — Added Speed Profile documentation with profile comparison table
- **doctor.sh** — Added `select_speed_profile()` function and integrated into all three run commands. TURBO/FAST profiles now export parallel conversion and stagger delay env vars.
- **TokenHelper.cs** — `CalculateRequestDelay` delay floor lowered from hardcoded 15s to configurable (default 2s, minimum 500ms)
- **ChunkingSettings.cs** — Added `MaxParallelConversion` property (default 1)

## [2.3.1] - 2026-02-12

### Fixed
- Line-based chunking fallback for data-only copybooks (no DIVISION/SECTION/PARAGRAPH)
- `SemaphoreSlim` disposal (`using var`) and over-release prevention (`lockHeld` flag)
- Config script injection: `eval` → `envsubst` in `load-config.sh`
- Port cleanup: `lsof -sTCP:LISTEN` to avoid killing client connections

### Added
- Chunking stress test for line-based fallback on large copybooks

## [2.3.0] - 2026-02-06

### Changed
- Removed "Spec-Driven Migration" workflow; focused on "Deep Code Analysis" pipeline
- Updated architecture diagrams for Deep SQL Analysis flow (Regex → SQLite → Portal)
- Cleaned up deprecated `doctor.sh` functions

## [2.2.1 – 2.2.2] - 2025-12-16

### Fixed
- `BusinessLogicExtractorAgent` auth: switched to `ResponsesApiClient` (HTTP 401 fix)
- Strict regex for class extraction, preventing AI comment artifacts (e.g., `Completes.java`)

## [2.2.0] - 2025-12-15

### Added
- **Smart Chunking** - Semantic chunking for large files (>3K lines), parallel processing (6 workers), cross-chunk `SignatureRegistry`
- Portal chunks tab with real-time progress; `doctor.sh chunking-health` command
- DB tables: `chunk_metadata`, `forward_references`, `signatures`, `type_mappings`

### Fixed
- 88% code loss on files >50K LOC (now routed through chunked process)
- Stale run status, duplicate DB paths, portal port conflicts

### Configuration
- `MaxLinesPerChunk`: 1500, `OverlapLines`: 300, `MaxParallelAnalysis`: 6, `TokenBudgetPerMinute`: 300K

## [2.1.0 – 2.1.1] - 2025-11-13 to 2025-11-24

### Added
- **C# .NET Support** - Dual-language output (Java Quarkus or C# .NET) via `CSharpConverterAgent`
- **Migration Reports** - Portal, CLI, or API (`/api/runs/{runId}/report`)
- **Mermaid Diagrams** - Interactive flowcharts, sequence, class, and ER diagrams
- Enhanced dependency tracking (CALL, COPY, PERFORM, EXEC SQL, READ/WRITE)

### Changed
- Unified `output/` directory; renamed `cobol-source/` → `source/`
- GPT-5 Mini (32K tokens) configuration

## [2.0.0] - 2025-11-11

### Added
- **Reverse Engineering** - `reverse-engineer` command, `BusinessLogicExtractorAgent`, glossary support
- **Hybrid Database** - SQLite + Neo4j via `HybridMigrationRepository`
- **Portal UI** - Three-panel dashboard with run selector, graphs, AI chat (port 5028)
- **REST API** - `/api/runinfo`, `/api/runs/all`, `/api/graph`, `/api/chat`
- DevContainer auto-start, 9 MCP resources per run

### Changed
- Port standardization: 5028 / 7474 / 7687
- `doctor.sh` auto-fixes, .NET 9 detection, Windows compatibility

## [1.0.0 – 1.3.0] - 2025-10-01 to 2025-10-23

### Core (1.0.0)
- Initial release: COBOL → Java Quarkus migration with AI agents (CobolAnalyzer, JavaConverter, DependencyMapper)
- SQLite persistence, MCP server, `doctor.sh` CLI, Azure OpenAI (GPT-4), Dev container

### Incremental (1.1.0 – 1.3.0)
- Neo4j integration → hybrid database (SQLite + Neo4j), dependency graph visualization
- McpChatWeb portal (three-panel dashboard, 9 MCP resources, run selector, dynamic graphs)
- .NET 9 standardization, multi-run query support

