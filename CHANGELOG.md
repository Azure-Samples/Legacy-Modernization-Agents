# Changelog

All notable changes to this repository are documented here.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- **Conversion Parity Validator** (preview) — Deterministic post-conversion check that each generated file actually represents its COBOL source. Compares procedures, data fields, `CALL` targets and SQL tables against identifiers present in the output, scoring each axis separately so a failure names the axis that lost structure. Runs in both the direct and chunked migration paths, writes `conversion-parity.json` beside the generated code, and surfaces in the Modernization Intelligence Runtime subview via `/api/modernization/conversion-parity`, which resolves reports through `JAVA_OUTPUT_FOLDER`/`CSHARP_OUTPUT_FOLDER` rather than assuming the default folders. Scoring starts from the inventory of source programs, so a program that produced no output at all is reported as a zero rather than omitted, and an axis that lost every symbol fails the check regardless of the overall score. A symbol surviving only in comments earns half credit and is reported as possibly renamed, except on the `CALL` target and SQL axes where a string literal is the executable form and counts as code. Each expected symbol consumes at most one match, so a single surviving identifier cannot satisfy a whole family of numerically-suffixed names. A `ConversionOutputGuard` stub or an AI-unavailable converter fallback scores zero even without structural context, rather than earning credit for its own scaffolding. A `CALL` whose target is resolved from a variable at runtime is never expected in generated code, on both the scan-facts and structural-context paths, since no converter can emit a name the source does not contain. Fields that exist only because an unresolved `COPY` was replaced by a generated stub copybook are excluded, with the weakened evidence stated on the affected program rather than silently absorbed into the score. Programs with no scan evidence are reported as not evaluated rather than assigned a score, as are two or more source programs sharing a file name, whose generated output cannot be told apart and would otherwise merge into one entry with the rest dropped from the report. Configured with `MIN_PROGRAM_SCORE` (default `0.75`) and `ON_LOW_SCORE` (`warn` by default, `stop` sets exit code `4`, including when programs were found but none could be evaluated). See [docs/conversion-parity-validation.md](docs/conversion-parity-validation.md).
- **Dependency Health & Semantic Flow Explorer** (preview) — Deterministic decision surfaces for judging conversion readiness from REKT scan evidence rather than model output. Adds `/api/modernization/{dependency-health,topology,service-chain,flow}` and `/api/graph/rekt/{runs,architect,services}`, plus two portal panels covering parse-fidelity buckets, readiness and coverage scoring, missing copybooks, the CALL/COPY topology, JCL→program→copybook chains, and per-program procedural flow. Fidelity is reported alongside its evidence source (`scan-cache`, `facts`, `artifacts`, `none`) so an inferred value is never presented as a measured one, and unresolved CALL targets are surfaced rather than dropped. Six subviews are labelled placeholders pending later features. See [docs/dependency-health-and-flow-explorer.md](docs/dependency-health-and-flow-explorer.md).
- **External Prompt Templates** — All agent system prompts are now loaded from Markdown files in `Agents/Prompts/` (`CobolAnalyzer.md`, `BusinessLogicExtractor.md`, `JavaConverter.md`, `CSharpConverter.md`, `DependencyMapper.md`, `ChunkAwareJavaConverter.md`, `ChunkAwareCSharpConverter.md`). Prompts are no longer embedded as inline strings in `.cs` agent files.
- **`{{CodebaseProfile}}` auto-injection** — Prompt templates may include the `{{CodebaseProfile}}` placeholder, which is replaced at runtime with a codebase summary (detected features, file stats, SQL usage). See `## SECTION: Name` delimiter syntax in prompt files.

### Removed
- **`Config/ai-config.env`** — The tracked placeholder config file is gone. `Config/ai-config.env.example` is now a copy source only and is never loaded at runtime; `Config/ai-config.local.env` is the sole file read. Loading the placeholder file filled unset keys with values like `your-api-key-here`, so a missing credential surfaced later as an authentication failure from the model endpoint instead of immediately as a missing setting. The file was already listed in `.gitignore` and remained tracked only because it predated that rule. `LOG_LEVEL`, `ENABLE_CHAT_LOGGING`, `ENABLE_API_CALL_LOGGING` and `AZURE_OPENAI_TEMPERATURE` are also dropped from the example file — they were documented, and in three cases actively set, but read by nothing.

### Fixed
- **Unedited config template passed validation** — The placeholder guard checked `AZURE_OPENAI_ENDPOINT` for `your-resource` and `AZURE_OPENAI_API_KEY` for `your-api-key`, but the shipped template contains `your-endpoint` and `key-placeholder`. Neither guard could fire on the file it existed to catch, so copying `Config/ai-config.env.example` and running without editing it reported "Configuration Validation Successful" and then failed four retries later with a DNS resolution error. Both literals are now matched. The failure guidance also pointed at `./setup.sh` and `CONFIGURATION_GUIDE.md`, neither of which exists; it now points at `./doctor.sh setup` and the Configuration Reference in `README.md`.
- **AI credentials required for commands that make no model call** — Configuration validation ran before command dispatch, so `--help`, `program-facts`, `rekt-scan-cache` and `conversation` all refused to start without `AZURE_OPENAI_ENDPOINT`, `AZURE_OPENAI_DEPLOYMENT_NAME` and `AZURE_OPENAI_MODEL_ID`, despite reading and writing only files. The placeholder values in `Config/ai-config.env` had masked this. Configuration is still loaded for every command; only the AI-specific requirement is now conditional on the command invoked.
- **CLI exit codes never reached the shell** — `Main` returns `int`, which overrides `Environment.ExitCode` entirely. Every command that reported failure by setting `Environment.ExitCode` — the `program-facts` commands' `2` for missing input or bad arguments, `3` for a parse failure, and the new parity gate's `4` — exited `0`, so no caller or CI job could detect the failure. `Main` now returns the environment code when its own is zero.
- **COBOL symbol names truncated to 15 characters** — `RektContextLoader` read paragraph and statement identifiers from the parser's `name` field, which the flow AST truncates (`SEARCH-CUSTOMER` → `SEARCH-C`, `FORMAT-BALANCE` → `FORMAT-BAL`). The untruncated `originalText` is now used, so PERFORM edges in the flow explorer, `CALL` targets in dependency topology, and extracted program facts carry the real names. Statement sentences are also no longer filed as paragraph names, which had inflated `Summary.Paragraphs`; an inline `PERFORM VARYING`/`UNTIL`/`n TIMES` no longer produces a phantom PERFORM edge to a non-existent procedure; and a `CALL` nested inside a conditional or an inline `PERFORM` — previously emitted only as untyped statement text and therefore dropped entirely — is now recovered for quoted targets.
- **`EXEC SQL` operation misreported as `EXEC`** — `RektContextLoader.ExtractSqlOperation` took the first word of a statement, so every embedded `EXEC SQL SELECT …` was recorded with the operation `EXEC` instead of `SELECT`. This propagated into extracted program facts, where `DbTableAccess` listed `EXEC` as the only access mode for affected tables. The `EXEC SQL` preamble is now skipped and the real verb is reported.
- **`ChatClientFactory` `InvalidOperationException`** — `CreateFromSettings()` now throws a descriptive `InvalidOperationException` when called with an unrecognised `ServiceType`, instead of returning `null` and causing a downstream null-reference crash.
- **Provider label display** — AI provider labels in the portal header now correctly reflect the active provider (was showing `AzureOpenAI` for all providers).
- **XSS hardening** — API-supplied model names are HTML-escaped before DOM insertion in the portal setup modal.
- **Business logic report — Use Cases output** — The `reverse-engineering-details.md` report now consistently uses `### Use Cases` as the section heading. Each entry now renders `Trigger:`, `Description:`, `Benefit:`, and `Key Steps:` fields, and markdown generation is centralized in `BusinessLogicMarkdownFormatter`.

### Security
- **Command injection fix in `ProcessManager`** — `ProcessManager.StartRun()` now uses `ProcessStartInfo.ArgumentList` instead of `ProcessStartInfo.Arguments` to pass arguments to `doctor.sh` subprocesses. This eliminates the command injection risk that existed when user-controlled values were interpolated into the `Arguments` string. Resolves [code-scanning alert #6](https://github.com/Azure-Samples/Legacy-Modernization-Agents/security/code-scanning/6).

## [3.0.0] - 2026-03-17

### Added
- **Multi-Provider AI Support** — Three AI providers now supported:
  - **AzureOpenAI** (existing) — Azure OpenAI deployments with Responses API for Codex models
  - **GitHubCopilot** (new) — GitHub Models catalog via `models.github.ai` endpoint
  - **OpenAI** (new) — Direct OpenAI API access
  - **GitHubCopilotSDK** (new) — GitHub Copilot CLI (`github-copilot-cli`) for stdio-based AI communication, no API keys needed
- **Portal AI Provider Setup Modal** — Browser-based setup for initial configuration:
  - **Azure OpenAI**: API key or Azure CLI (`az login`) auth, auto-discovers deployed models via ARM API
  - **GitHub Copilot SDK**: CLI login or PAT auth, lists models via `CopilotClient.ListModelsAsync()`
  - Assign separate **Chat** and **Code** models; saves to `Config/ai-config.local.env`
  - Auto-opens on first visit when no models are configured
- **Model-Aware Reasoning for ALL Providers** — Three-tier content-aware reasoning adapts per model family:
  - **Claude** → Extended thinking with `budget_tokens` (30%/50%/70% based on effort)
  - **Codex/o-series** → `reasoning_effort` additional property
  - **GPT/Grok/standard** → `temperature=0.1` for deterministic output
  - `ModelCapabilities.Detect()` auto-classifies models from ID string
- **Output Truncation Detection & Recovery** — Safety net for all IChatClient providers:
  - `OutputTruncationException` on `FinishReason=Length`, text-based truncation signals, or unclosed code blocks
  - Escalation loop: doubles `MaxOutputTokens` + promotes reasoning effort with thrash guard
  - Falls back to adaptive re-chunking at COBOL DIVISION/SECTION boundary
- **`ChatClientFactory.CreateFromSettings()`** — Single entry point that auto-selects provider based on `AISettings.ServiceType`
- **`CopilotChatClient`** — `IChatClient` adapter wrapping `GitHub.Copilot.SDK.CopilotClient`
- **`Create()` Static Factories** — All 7 agents now have `Create()` factory methods that auto-route to the correct constructor
- **Model Discovery API Endpoints**: `POST /api/models/connect`, `POST /api/models/save-config`, enhanced `GET /api/models/available`
- **Prompt Studio Multi-Provider Support** — AI Enhance and Re-Score work with both Azure OpenAI and GitHub Copilot SDK

### Changed
- **`CODEX_*` → `AI_*` Environment Variables** — All env vars renamed (e.g., `AI_SPEED_PROFILE`, `AI_LOW_REASONING_EFFORT`, `AI_MAX_OUTPUT_TOKENS`)
- **`AgentBase` IChatClient Path** — Replaced hardcoded `MaxOutputTokens=16384` with content-aware `CalculateTokenSettings()` + `ApplyModelSpecificOptions()`
- **All agent initialization** is now provider-aware — uses `ResponsesApiClient` when available, falls back to `IChatClient`
- **`MigrationProcess`**, **`ChunkedMigrationProcess`**, **`SmartMigrationOrchestrator`** — Accept nullable `ResponsesApiClient?`
- **Architecture diagrams** — Updated to reflect portal setup, dual provider paths, and expanded portal features

### Removed
- **`UnifiedAIClient`** — Deleted; model-aware reasoning is now built into `AgentBase` and `CobolAnalyzerAgent`

### Fixed
- **`_runId` Bug** — Three converter agents never assigned `_runId` in one or both constructors
- **`ModelCapabilities` namespace collision** — Fully qualified to resolve collision with `GitHub.Copilot.SDK.ModelCapabilities`
- **Missing `GitHubCopilotSDK` endpoint validation** — Valid SDK configs were being rejected
- **Portal AI blocked by Entra ID** — Prompt Studio Phase 3 now supports `DefaultAzureCredential` and `gh auth token`
- **Temperature not written to portal config** — Respects per-model auto-detection via `ModelCapabilities`
- **XSS protection** — HTML-escapes API-supplied model names before DOM insertion
- **URL validation** — Client-side (HTTPS) and server-side (`Uri.TryCreate`) for Azure endpoints

### Security
- API keys in setup modal stored server-side only (env vars + gitignored config), never in browser storage
- Azure ARM API calls use separate `management.azure.com` token scope

## [2.5.0] - 2026-03-05

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

