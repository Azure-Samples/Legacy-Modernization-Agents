# Changelog

All notable changes to this repository are documented here.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [2.1.0] - 2025-11-13

### Added - Major Features

- **☕/C# Dual Language Support** - Choose between Java Quarkus or C# .NET output
  - Interactive target language selection during `./doctor.sh run`
  - New `CSharpConverterAgent` alongside existing `JavaConverterAgent`
  - Separate output folders: `output/java-output/` and `output/dotnet-output/`
  - Agent personas customized for Java/Quarkus or C#/.NET expertise
  - All documentation updated to reflect dual-language capability
  - Migration process tracks target language in run metadata

- **📊 Enhanced Dependency Tracking** - Comprehensive COBOL statement detection
  - **CALL** - Program invocations with line numbers
  - **COPY** - Copybook inclusions
  - **PERFORM** - Procedure calls and loops
  - **EXEC SQL** - Embedded SQL statements
  - **READ/WRITE** - File I/O operations
  - **OPEN/CLOSE** - File handling
  - Color-coded edges in dependency graph (CALL=green, COPY=blue, etc.)
  - Edge labels show type and line numbers (e.g., "CALL (L42)")
  - Tooltips display full context from source code
  - Filterable by edge type with checkboxes
  - All relationship types stored in Neo4j with metadata

- **📄 Migration Report Generation** - Comprehensive documentation per run
  - **Via Portal**: Click "📄 Generate Report" on any migration run
  - **Via CLI**: Prompted after each `./doctor.sh run` completion
  - **Via API**: `GET /api/runs/{runId}/report`
  - **Report Contents**:
    - Migration summary (file counts, target language)
    - Dependency breakdown by type (CALL, COPY, PERFORM, etc.)
    - Complete file inventory with line counts
    - Detailed dependency relationships table
    - Line numbers and context for each dependency
  - **Report Formats**:
    - View rendered in portal with Markdown formatting
    - Download as `.md` file for documentation
    - Auto-saved to `output/migration_report_run_{runId}.md`

- **🎨 Mermaid Diagram Support** - Interactive architecture documentation
  - All Mermaid flowcharts, sequence diagrams, and visualizations render automatically
  - Dark theme matching portal design
  - Zoomable and pan-able diagrams
  - Syntax highlighting for code blocks
  - **Supported Diagram Types**:
    - Flowcharts (system architecture)
    - Sequence diagrams (process flows)
    - Class diagrams (data models)
    - ER diagrams (database schemas)
  - **Technologies**: Mermaid.js 10.x, Marked.js 11.x
  - Integrated into "📄 Architecture Documentation" portal view

- **🎯 Collapsible UI Components** - Clean, organized dashboard
  - Filter sections collapse/expand with toggle controls
  - Organized dependency type filters (CALL, COPY, PERFORM, etc.)
  - Cleaner visual hierarchy in portal interface
  - Improved user experience for complex visualizations
  - Edge type filtering with color-coded visualization
  - Line number context for all dependencies

- **🔄 GPT-5 Mini Default Configuration** - Updated AI model recommendations
  - All documentation now defaults to GPT-5 Mini (gpt-5-mini-2 deployment)
  - Updated prerequisites in README.md, QUICK_START.md, .devcontainer/README.md
  - Example configurations show gpt-5-mini-2 deployment names
  - Token limit documentation updated to 32K for GPT-5 Mini
  - Cost-optimized recommendations for testing and production

### Added - Portal & API Enhancements

- **📄 Report API Endpoints**:
  - `GET /api/runs/{runId}/report` - Generate/retrieve migration report
  - Returns JSON with content, lastModified, and file path
  - Markdown content rendered in portal with full formatting

- **📚 Documentation API Endpoints**:
  - `GET /api/documentation/architecture` - Architecture docs with Mermaid diagrams
  - Returns content with automatic diagram rendering
  - Dark theme with syntax highlighting

- **🔗 Enhanced Graph API**:
  - Edge objects now include `type`, `lineNumber`, and `context` fields
  - Support for multiple dependency relationship types
  - Filterable graph data by edge type
  - Color-coded visualization by relationship type

### Changed

- **Architecture Diagrams** - Updated all flow diagrams to show dual-language support
  - Migration process diagram shows target language selection step
  - Storage layer updated to reflect "Java/C# code" instead of "Java code"
  - Output section shows "Java Quarkus Code OR C# .NET Code"
  - Converter agent renamed to "CodeConverterAgent" with language choice

- **Documentation Updates** - Comprehensive updates across all docs
  - Main title: "COBOL to Java or C#" instead of "COBOL to Java"
  - Step-by-step guide includes target language selection
  - Sequence diagrams show user prompt for language choice
  - Semantic Kernel section mentions both converter agents

- **FileHelper.cs** - Fixed file extension handling
  - Removed hardcoded `.java` extension enforcement
  - Fixed extension replacement to replace instead of append
  - C# files now correctly saved as `.cs` instead of `.cs.java`

- **DevContainer Configuration** - Portal auto-start improvements
  - Updated `postStartCommand` to auto-start portal on container launch
  - Portal reliably starts at http://localhost:5028 after rebuild
  - Neo4j health checks before portal launch
  - Improved reliability for development workflow

### Fixed

- **✅ C# File Extension Bug** - Files now generate with correct `.cs` extension
  - Previously: `CustomerDisplayException.cs.java`
  - Now: `CustomerDisplayException.cs`
  - Fixed `SanitizeFileName` method in `Helpers/FileHelper.cs`

- **✅ Portal Auto-Start** - Portal now starts automatically in dev container
  - Fixed issue where portal didn't load after `./helper-scripts/demo.sh`
  - Services now reliably start after devcontainer rebuild
  - Added helper scripts: `open-portal.sh` and `status.sh`

- **✅ Local GIF for Demo** - README now uses local GIF instead of GitHub URL
  - Changed image src from GitHub assets to local workspace path
  - Path: `/workspaces/Legacy-Modernization-Agents/gifdemowithgraphandreportign.gif`
  - Better demonstration with animated portal features

## [2.0.0] - 2025-11-11

### Added
- **🔍 Reverse Engineering** - Standalone business logic extraction without full migration
  - New `reverse-engineer` command extracts business rules, user stories, and domain models
  - `BusinessLogicExtractorAgent` generates human-readable documentation from COBOL
  - Output: Unified markdown file with features, use cases, and business rules
  - `--skip-reverse-engineering` flag to optimize API costs in migration pipeline
  - Glossary support (`Data/glossary.json`) translates technical terms to business language

- **🗄️ SQLite Integration** - Dual database architecture with SQLite + Neo4j
  - Real-time LOC calculation from COBOL content (excludes comments and blank lines)
  - Multi-run queries: "show me run 42" automatically switches context
  - File content analysis: "what functions are in X.cbl" returns detailed structure
  - 44 migration runs persisted with metadata, file counts, and analysis data
  - Chat augments prompts with SQLite context for comprehensive answers

- **🌐 Enhanced Portal UI**
  - Run selector dropdown with historical runs (filters to runs with graph data)
  - Dynamic dependency graph syncs with selected run
  - Example queries section for COBOL analysis and migration planning
  - Dark theme consistency with rich colors and connection-based node sizing
  - Data retrieval guide modal with SQLite, Neo4j, and MCP API examples

- **🚀 DevContainer Auto-Start** - Services launch automatically on container restart
  - Portal auto-starts at http://localhost:5028 when `Data/migration.db` exists
  - Neo4j restarts automatically with health checks
  - Locked ports (5028, 7474, 7687) prevent configuration drift

- **📁 Directory Cleanup** - Simplified from `cobol-source/`/`java-output/` to `source/`/`output/`
  - `.gitkeep` files preserve directory structure on clone
  - Scripts moved to `helper-scripts/` folder
  - Documentation consolidated: 7 files → 4 (README, CHANGELOG, QUICK_START, architecture)

### Changed
- **Port Standardization** - All services use consistent ports (5028, 7474, 7687)
- **doctor.sh Enhancements** - Auto-fixes Neo4j container issues, port conflicts, Azure config
- **Windows Compatibility** - Retry logic for file locking, MAX_PATH handling, reserved filename detection

### Fixed
- Port conflicts resolved (launchSettings.json 5250 → 5028)
- JavaScript caching issues with DOM ready checks
- Graph layout configuration errors (invalid `parentCentralization` option)
- Zero build warnings (CS1998, CS8602 resolved)
- MCP server stream buffering and duplicate node errors

## [1.3.0] - 2025-10-23

### Added - Major Features
- **🔄 Run Selector Dropdown**: Interactive migration run selection in portal header
  - Dropdown displays all historical runs (1-44) sorted by most recent
  - Shows current run with "(Current)" label
  - Refresh button to reload available runs
  - Integrated with graph visualization for automatic updates
  - API endpoint: `GET /api/runs/all` queries SQLite database directly
  - API endpoint: `POST /api/switch-run` changes active migration run

- **📊 Dynamic Dependency Graph Updates**: Graph syncs with selected run
  - Selecting run from dropdown automatically reloads graph data
  - Graph title badge updates to show current run ("Run X")
  - Backend `/api/graph?runId=X` endpoint fetches run-specific data
  - Console logging for debugging graph load operations
  - Retry logic ensures graph loads even when initially unavailable

### Fixed - Critical Issues
- **✅ Port Standardization**: All configurations now use port 5028 consistently
  - Fixed port conflict between launchSettings.json (5250) and demo.sh (5028)
  - Updated McpChatWeb/Properties/launchSettings.json to use 5028
  - Updated demo.sh references from 5250 to 5028 (7 occurrences)
  - Updated all documentation to reference http://localhost:5028

- **✅ Browser JavaScript Caching Issues**: Resolved null reference errors
  - Fixed "Cannot read properties of null (reading 'addEventListener')" errors
  - Added proper DOM ready checks with DOMContentLoaded event handling
  - Wrapped all element access in `initializeApp()` function
  - Added null checks for `loadingIndicator`, `loadingStages`, and `resourcesList`
  - Fixed race condition where scripts loaded before DOM elements existed
  - Hard refresh (Cmd+Shift+R) now recommended after updates

- **✅ Graph Layout Configuration**: Fixed hierarchical layout errors
  - Removed invalid `parentCentralization` option from vis-network config
  - Properly nested hierarchical options under `layout.hierarchical`
  - Fixed "Unknown option detected: levelSeparation" error
  - Both Force Directed and Hierarchical layouts now work correctly

- **✅ Run Switching Functionality**: Fixed errors when changing runs
  - Added missing `responseBody` variable declaration in `run-selector.js`
  - Fixed element ID mismatches (`resources` → `resources-list`)
  - Added proper null checks in `renderResources()` function
  - Graph now properly reloads with `loadAndRender(newRunId)` parameter
  - Success/error messages now display correctly in response card

### Changed
- **MCP Web Portal Port**: Standardized on 5028 (was inconsistent 5250/5028)
- **Graph Initialization**: Now passes `runId` to `loadAndRender()` on initial load
- **Run Selector Integration**: Dropdown added to header with live run detection
- **JavaScript Architecture**: Refactored to use initialization pattern with DOM ready checks

## [1.2.0] - 2025-10-10

### Added - Major Features
- **🔍 File Content Analysis**: Natural language queries for COBOL file internals
  - Query syntax: "what functions are in BDSDA23.cbl", "what does copybook RENI033.cpy contain"
  - Returns comprehensive analysis: program purpose, functions/paragraphs, variables with PIC clauses, copybooks
  - Regex-based detection in chat endpoint with MCP resource integration
  - Parses `rawAnalysisData` JSON field from MCP responses
  - Extracts from `paragraphs-and-sections-summary`, `variables`, `copybooksReferenced` arrays
  - SQLite fallback using sqlite3 subprocess for direct database queries
  - MCP resource URI: `insights://runs/{runId}/analyses/{fileName}`

- **🔄 Multi-Run Query Support**: Query any historical migration run
  - Chat detection: "show me run 42", "run id 40" automatically routes to correct run
  - `/api/search/run/{runId}` endpoint queries both SQLite and Neo4j
  - Clear source indicators in response (SQLite vs Neo4j data)
  - Dual-database query with comprehensive instructions for manual access
  - `ChatResponse` model enhanced with optional `RunId` field
  - Frontend automatically updates graph when different run mentioned

- **📊 Dynamic Graph Updates**: Graph visualization reflects queried run
  - `/api/graph` endpoint accepts optional `runId` parameter
  - `graph.js` exposes `loadGraphForRun(runId)` for external calls
  - `main.js` detects `runId` in chat responses and triggers graph reload
  - Graph title updates to show "Dependency Graph | Run X"
  - Retry logic with 500ms delay if graph not ready when chat completes

- **📚 Data Retrieval Guide Modal**: Comprehensive data access documentation
  - `/api/data-retrieval-guide` endpoint with structured guide data
  - Modal UI with examples for SQLite, Neo4j, and MCP API queries
  - Copy-paste ready commands for all three data access methods
  - Includes connection details, query examples, and tool recommendations

### Added - Infrastructure
- `McpChatWeb` ASP.NET Core minimal API that exposes a resources listing and MCP-backed chat interface
- `McpChatWeb.Tests` integration test project with a fake `IMcpClient` to validate the REST endpoints
- **Three-panel web dashboard** with resources list, AI chat, and interactive dependency graph visualization (vis-network)
- **Dynamic run detection** - `/api/runinfo` endpoint automatically detects current migration run ID
- **MCP resource reading** - `ReadResourceAsync` method in `IMcpClient` to fetch individual resource content
- **Graph data endpoint** - `/api/graph` serves deduplicated dependency graph data from MCP resources
- **Real-time graph rendering** - Interactive visualization with zoom, pan, node filtering, and layout options
- **9 MCP resources** exposed per run:
  - `insights://runs/{id}/summary` - Migration run overview
  - `insights://runs/{id}/files` - All COBOL files list
  - `insights://runs/{id}/graph` - Full dependency graph
  - `insights://runs/{id}/circular-dependencies` - Circular dependency analysis
  - `insights://runs/{id}/critical-files` - High-impact files
  - `insights://runs/{id}/cobol-file/{filename}` - Individual file content
  - `insights://runs/{id}/java-file/{filename}` - Generated Java code
  - `insights://runs/{id}/dependencies/{filename}` - Per-file dependencies
  - `insights://runs/{id}/analyses/{fileName}` - **NEW** File analysis with detailed structure
- Configuration documentation describing how to launch the web UI and execute the new tests
- **GRAPH_DEDUPLICATION_FIX.md** - Detailed documentation of duplicate node resolution
- **Dev container enhancements**:
  - Neo4j VS Code extension for database queries
  - SQLite VS Code extension for database inspection
  - Node.js LTS for frontend development
  - cypher-shell for Neo4j CLI queries
  - Helpful bash aliases: `demo`, `migration-run`, `portal-start`, `neo4j-status`
  - Auto-start Neo4j on container start (`postStartCommand`)
  - Welcome message with quick command guide

### Changed
- Console project build excludes web and test source files to avoid cross-compilation issues
- README prerequisites now highlight the .NET 9.0 SDK requirement (project standardized on .NET 9)
- `doctor.sh` updated to prefer .NET 9 SDK instead of .NET 8
- Test project (`McpChatWeb.Tests`) upgraded to target .NET 9.0 with preview packages for full framework alignment
- **MCP server stream handling** - Fixed `WriteMessageAsync` to use consistent `_writer` for both headers and payload (previously mixed `_writer` and `_outputStream` causing buffering issues)
- **McpOptions configuration** - Changed `DotnetExecutable` from hardcoded .NET 8 path to "dotnet" for cross-platform compatibility
- **Graph visualization** - Changed from hardcoded runId=37 to dynamic detection from API
- **Dashboard UI** - Added "Run {id}" badge to graph panel header showing current migration run
- **Dev container ports** - Changed McpChatWeb auto-forward from "notify" to "openBrowser" for immediate access
- **Dev container VS Code settings** - Added better defaults for .NET development and file watching exclusions
- **Portal URL standardized** - All documentation now references http://localhost:5028 (was inconsistent 5028/5250)

### Fixed
- **✅ Zero build warnings** - Project now builds cleanly with 0 warnings, 0 errors
  - Fixed CS1998 async warning in `/api/runs/{runId}/combined-data` endpoint by adding `await Task.CompletedTask;`
  - Fixed CS8602 null reference warnings in file analysis by adding proper null checks to `purpose` extraction
  - Removed all compiler warnings for professional code quality
- Removed lingering Markdown fences in `wwwroot/main.js` that previously broke the frontend script loader
- Addressed `dotnet build` failures caused by top-level statements and missing namespace references in the original console entry point
- Fixed .NET SDK detection in `doctor.sh` that was forcing .NET 8 when project targets .NET 9, causing NETSDK1045 errors
- Resolved 3 CS8604 nullable reference warnings in `Mcp/McpServer.cs` by adding null-forgiving operators to `BuildGraphPayloadAsync`, `BuildCircularDependenciesPayloadAsync`, and `BuildCriticalFilesPayloadAsync` calls (these methods always return non-null JsonObject)
- **Fixed MCP connection hangs** - MCP server stream buffering issue resolved (see MCP_CONNECTION_FIX.md)
- **Fixed duplicate node error** - Graph data now properly deduplicated (128 duplicate nodes → 49 unique nodes)
  - Server-side deduplication in `/api/graph` endpoint using Dictionary with JsonObject cloning
  - Client-side safety net in `graph.js` using Map for additional deduplication
  - Resolved "Cannot add item: item with id already exists" vis-network error
  - Fixed "node already has a parent" JSON manipulation error by cloning nodes
- **Fixed JSON parse error** - Added content-type validation in `fetchGraphData()` to detect HTML error responses
- **Graph now displays correct data** - 49 unique nodes (5 programs + 44 copybooks) with 64 edges instead of 5 sample nodes

## [1.1.0] - 2025-10-08

### Added
- Neo4j 5.15.0 integration for graph-based dependency storage
- Hybrid repository pattern with SQLite + Neo4j dual-database architecture
- Docker Compose configuration for Neo4j container management
- `HybridMigrationRepository` coordinating both database backends
- `Neo4jMigrationRepository` for graph operations
- Dependency graph visualization in web portal
- Interactive graph controls (zoom, pan, filter, layout options)
- vis-network library integration for graph rendering

### Changed
- Migration process now stores dependencies in both SQLite and Neo4j
- Updated architecture documentation with hybrid database flow diagrams
- Enhanced README with Neo4j setup instructions

## [1.0.0] - 2025-10-01

### Added
- Initial release of COBOL to Java Quarkus migration framework
- Semantic Kernel-based AI agent orchestration
- Three specialized AI agents: CobolAnalyzerAgent, JavaConverterAgent, DependencyMapperAgent
- SQLite persistence for migration history
- MCP (Model Context Protocol) server implementation
- Command-line interface via `doctor.sh` script
- Azure OpenAI integration with GPT-4 support
- Comprehensive logging with EnhancedLogger and ChatLogger
- File-based output (Java code, reports, diagrams)
- Dev container configuration for VS Code

### Documentation
- Complete README with architecture diagrams
- Step-by-step setup and usage guide
- Semantic Kernel architecture explanation
- AI agent persona customization guide
