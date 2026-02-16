# Changelog

All notable changes to this repository are documented here.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [2.4.0] - 2026-02-16

### Added
- **Speed Profile Selection** - New interactive prompt in `doctor.sh` lets you choose between four speed profiles before running migrations, reverse engineering, or conversion-only:
  - **TURBO** — Low reasoning on ALL files with no exceptions. 65K token ceiling, parallel file conversion (4 workers), 200ms stagger delay. Designed for testing and smoke runs where speed matters more than quality.
  - **FAST** — Low reasoning on most files, medium only on the most complex ones. 32K token cap, parallel conversion (3 workers), 500ms stagger. Good for quick iterations and proof-of-concept runs.
  - **BALANCED** (default) — Uses the three-tier content-aware reasoning system. Simple files get low effort, complex files get high effort. Uses `appsettings.json` defaults.
  - **THOROUGH** — Maximum reasoning on all files regardless of complexity. Best for critical codebases where accuracy matters more than speed.
- **Shared `select_speed_profile()` function** — Called from `run_migration()`, `run_reverse_engineering()`, and `run_conversion_only()`. Sets `CODEX_*` environment variables that are picked up by `Program.cs` `OverrideSettingsFromEnvironment()` at startup — no C# changes needed.
- **Adaptive Re-Chunking on Output Exhaustion** — When reasoning exhaustion retries fail (all escalation attempts exhausted), `AgentBase` now automatically splits the COBOL source at the best semantic boundary (DIVISION > SECTION > paragraph > midpoint) and processes each half independently with 50-line overlap for context continuity. Results are merged with duplicate package/import/class removal and validated for truncation signals. This solves the TURBO/FAST paradox where small output token caps caused repeated exhaustion failures rather than triggering the existing input-size-based chunking.
- **Parallel File Conversion** — All 4 converter agents (`ChunkAwareJavaConverter`, `ChunkAwareCSharpConverter`, `JavaConverterAgent`, `CSharpConverterAgent`) now support parallel file conversion via `SemaphoreSlim`-based concurrency control. Controlled by `MaxParallelConversion` setting (default: 1 = sequential for backward compatibility). TURBO uses 4 workers, FAST uses 3.
- **Environment Variable Overrides for Timing** — New env vars `CODEX_STAGGER_DELAY_MS`, `CODEX_MAX_PARALLEL_CONVERSION`, and `CODEX_RATE_LIMIT_SAFETY_FACTOR` allow fine-tuning of parallelism and rate limiting without code changes.

### Fixed
- **Settings Injection Bug** — All agent constructors in `MigrationProcess.cs`, `ChunkedMigrationProcess.cs`, and `Program.cs` were missing the `settings` parameter, causing `AppSettings` to always be `null` inside agents. This meant `MaxParallelConversion` always defaulted to 1 (sequential), making the parallel conversion feature dead code. All 10 constructor call sites now pass `settings` correctly, enabling TURBO's 4-worker and FAST's 3-worker parallelism.

### Changed
- **README.md** — Added Speed Profile documentation with profile comparison table
- **doctor.sh** — Added `select_speed_profile()` function and integrated into all three run commands. TURBO/FAST profiles now export parallel conversion and stagger delay env vars.
- **TokenHelper.cs** — `CalculateRequestDelay` delay floor lowered from hardcoded 15s to configurable (default 2s, minimum 500ms)
- **ChunkingSettings.cs** — Added `MaxParallelConversion` property (default 1)

## [2.3.1] - 2026-02-12

### Fixed
- **Chunking Fallback for Data-Only Copybooks** - Added line-based chunking fallback in `SemanticUnitChunker` when no semantic units (DIVISION/SECTION/PARAGRAPH) are found, fixing crash on pure-data copybooks like `STRESSCOPY.cpy` (10K lines)
- **SemaphoreSlim Disposal** - Added `using var` to `SemaphoreSlim` across 5 files to ensure proper disposal
- **RateLimiter Over-Release** - Added `lockHeld` flag pattern to prevent `SemaphoreSlim.Release()` being called without a matching `WaitAsync()`
- **Config Script Injection** - Replaced `eval` with `envsubst` in `Config/load-config.sh` to prevent command injection
- **Port Cleanup Stability** - Fixed `doctor.sh` port cleanup in both `launch_portal_background()` and `launch_mcp_web_ui()` to use `lsof -sTCP:LISTEN` instead of `lsof -ti:`, preventing accidental killing of client connections (e.g., VS Code browser tabs)

### Added
- **Chunking Stress Test** - New unit test `ChunkFileAsync_NoSemanticUnits_LargeFile_FallsBackToLineBased` validating line-based fallback for large copybooks

### Changed
- **doctor.sh** - Updated to 2,274 LoC with improved port cleanup logic

## [2.3.0] - 2026-02-06
### Changed
- **Streamlined Workflow** - Removed "Spec-Driven Migration" (MITM) workflow to focus on the "Deep Code Analysis" pipeline.
- **Improved Documentation** - Updated architecture diagrams in `README.md` to reflect the new "Deep SQL Analysis" flow (Regex -> SQLite -> Portal).
- **Cleanup** - Refactored `doctor.sh` to remove deprecated functions and menu options.

## [2.2.2] - 2025-12-16

### Fixed
- **Business Logic Extraction Authentication** - Updated `BusinessLogicExtractorAgent` to use `ResponsesApiClient` instead of `IChatClient` to resolve HTTP 401 errors with `gpt-5.2-chat` model deployments.

## [2.2.1] - 2025-12-16

### Fixed
- **Parsing Artifacts** - Fixed issue where comments in AI responses (e.g., "class completes") were incorrectly parsed as class definitions, generating invalid files like `Completes.java`.
- **Class Extraction Logic** - Updated `ChunkedMigrationProcess` to use strict Regex for identifying Java and C# class definitions, ignoring comments and non-code text.

## [2.2.0] - 2025-12-15

### Added
- **Smart Chunking (v0.2)** - Automatic large file detection and semantic chunking for files >150K chars or >3000 lines
- **SmartMigrationOrchestrator** - Intelligent file routing to appropriate process based on size/complexity
- **ChunkedReverseEngineeringProcess** - Handles large file reverse engineering analysis with parallel chunk processing
- **ChunkedMigrationProcess** - Handles large file code conversion with chunk reassembly
- **ChunkingOrchestrator** - Semantic boundary detection (paragraphs, sections, divisions) with configurable overlap
- **Parallel Processing** - Up to 6 parallel workers for chunk analysis with rate limiting
- **SignatureRegistry** - Cross-chunk consistency for method signatures and type mappings
- **Portal Chunks Tab** - Real-time chunk progress monitoring with overall progress bar
- **Chunking Health Check** - New `./doctor.sh chunking-health` command for infrastructure diagnostics
- **Database Tables** - `chunk_metadata`, `forward_references`, `signatures`, `type_mappings` tables

### Changed
- **doctor.sh** - Updated help text with Smart Chunking info, added component validation for chunking infrastructure
- **Run Selector** - Fixed auto-select behavior to only trigger on initial page load, preserves user selection on refresh
- **REVERSE_ENGINEERING_ARCHITECTURE.md** - Complete rewrite with chunking architecture, AI models (gpt-5.1-codex-mini), Mermaid diagrams
- **Portal UI** - Simplified Chunks tab (removed 4 sub-cards, added single progress bar)
- **AI Models** - Documented use of `gpt-5.1-codex-mini` for agents and `gpt-5.1-chat` for portal

### Fixed
- **Large File Truncation** - Fixed 88% code loss bug for files >50K LOC by routing through chunked process
- **Stale Run Status** - Fixed interrupted runs showing as "Running" in portal dropdown
- **Duplicate Database Path** - Fixed MIGRATION_DB_PATH export issues causing duplicate databases
- **Portal Port Conflicts** - Improved cleanup of zombie processes on port 5028

### Configuration
- `ChunkingSettings.MaxLinesPerChunk`: 1500 (default)
- `ChunkingSettings.OverlapLines`: 300 (default)
- `ChunkingSettings.MaxParallelAnalysis`: 6 (default)
- `ChunkingSettings.TokenBudgetPerMinute`: 300000

## [2.1.1] - 2025-11-24

### Changed
- **Folder Structure** - Unified `output/` directory (removed `java-output/` and `dotnet-output/` subdirectories)
- **Source Directory** - Renamed `cobol-source/` to `source/` for consistency
- **Documentation** - Removed `QUICK_START.md`, integrated content into expanded README.md
- **Configuration Files** - Updated all config files and scripts to reflect new folder paths

### Fixed
- Path references in `.gitignore`, `doctor.sh`, config files, and DevContainer documentation

## [2.1.0] - 2025-11-13

### Added
- **C# .NET Support** - Dual-language output (Java Quarkus or C# .NET), unified `output/` folder, `CSharpConverterAgent`
- **Enhanced Dependency Tracking** - CALL, COPY, PERFORM, EXEC SQL, READ/WRITE, OPEN/CLOSE with line numbers and context
- **Migration Reports** - Generate via portal, CLI, or API (`/api/runs/{runId}/report`), auto-saved to `output/`
- **Mermaid Diagrams** - Interactive flowcharts, sequence, class, and ER diagrams with zoom/pan
- **Collapsible UI** - Organized dashboard with toggle sections, filterable dependency types
- **GPT-5 Mini Config** - Updated docs and examples for GPT-5 Mini (32K tokens)
- **API Endpoints** - `/api/documentation/architecture`, enhanced graph API with edge metadata

### Changed
- **Architecture Diagrams** - Updated for dual-language support (Java/C# code paths)
- **FileHelper.cs** - Fixed extension handling (`.cs` instead of `.cs.java`)
- **DevContainer** - Portal auto-start on launch, Neo4j health checks

### Fixed
- C# file extension bug, portal auto-start reliability, local GIF demo path

## [2.0.0] - 2025-11-11

### Added
- **Reverse Engineering** - `reverse-engineer` command, `BusinessLogicExtractorAgent`, glossary support, business logic extraction
- **Hybrid Database** - SQLite (metadata, runs, analyses) + Neo4j (dependency graphs), `HybridMigrationRepository`
- **Portal UI** - Three-panel layout, run selector, dynamic graphs, AI chat, http://localhost:5028
- **DevContainer Auto-Start** - Services auto-launch, pre-configured extensions, bash aliases
- **Simplified Structure** - `source/` for COBOL, `output/` for all generated files (no subfolders)
- **MCP Resources** - 9 resources per run (summary, files, graph, analyses, etc.)
- **REST API** - `/api/runinfo`, `/api/runs/all`, `/api/switch-run`, `/api/graph`, `/api/chat`

### Changed
- **Port Standardization** - 5028 (portal), 7474 (Neo4j HTTP), 7687 (Neo4j Bolt)
- **doctor.sh** - Auto-fixes, .NET 9 detection, Azure config validation
- **Windows Compatibility** - File locking retry, MAX_PATH handling

### Fixed
- Port conflicts, JavaScript caching, graph layout config, MCP buffering, graph deduplication (128→49 nodes)

## [1.3.0] - 2025-10-23

### Added
- Run selector dropdown in portal, dynamic graph updates per run
- API: `/api/runs/all`, `/api/switch-run`

### Fixed
- Port standardization (5028), JavaScript null reference errors, graph layout config, run switching

## [1.2.0] - 2025-10-10

### Added
- File content analysis - Natural language queries for COBOL internals
- Multi-run query support - "show me run 42"
- Dynamic graph updates - Auto-refresh on run change
- Data retrieval guide modal
- McpChatWeb portal, three-panel dashboard, 9 MCP resources
- Dev container enhancements (Neo4j, SQLite extensions, aliases)

### Changed
- .NET 9 standardization, MCP stream handling, cross-platform compatibility

### Fixed
- Zero build warnings, MCP connection hangs, duplicate nodes (128→49), JSON parse errors

## [1.1.0] - 2025-10-08

### Added
- Neo4j 5.15.0 integration, hybrid repository pattern, Docker Compose config
- Dependency graph visualization, vis-network library

### Changed
- Dual-database storage (SQLite + Neo4j), updated architecture docs

## [1.0.0] - 2025-10-01

### Added
- Initial release: COBOL to Java Quarkus migration framework
- Semantic Kernel orchestration, AI agents (CobolAnalyzer, JavaConverter, DependencyMapper)
- SQLite persistence, MCP server, `doctor.sh` CLI
- Azure OpenAI (GPT-4), logging (EnhancedLogger, ChatLogger)
- Dev container, comprehensive documentation

