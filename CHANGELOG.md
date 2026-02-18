# Changelog

All notable changes to this repository are documented here.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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

