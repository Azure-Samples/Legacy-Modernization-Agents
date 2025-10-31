# Changelog

All notable changes to this repository are documented here.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- **🎨 Enhanced Portal UI** - Improved visualization and user experience (Oct 29, 2025)
  - Fixed dark theme consistency for legend box and tooltips
  - Enhanced dependency graph with rich colors and dynamic node sizing
  - Added connection-based metrics showing node importance
  - Fixed run selection bug - correctly filters by runId from URI
  - Added Run 49 to SQLite database for consistency with Neo4j
  - Run dropdown now only shows runs with actual graph data (44, 49)
  - Added example queries section with COBOL analysis and migration planning prompts
  - Removed technical MCP URIs from user-facing interface
  
- **🔍 Intelligent Multi-Source Chat** - Combined SQLite metadata with Neo4j graph insights
  - Chat augments prompts with SQLite context (run metadata, file counts, copybook lists)
  - Fixed SQLite schema queries to match actual database structure
  - Seamlessly combines relational metadata with graph dependencies
  - Provides comprehensive answers using both data sources
  - Generic model reference ("Processing with AI") instead of hardcoded model names

- **📁 Directory Structure Preservation** - Ensure folders exist on clone and devcontainer load (Oct 29, 2025)
  - Added `.gitkeep` files to `cobol-source/` and `java-output/` directories
  - Updated `.gitignore` to preserve directory structure while ignoring contents
  - Directories now present immediately after repo clone and devcontainer creation
  - No manual directory creation needed before running migrations

- **📊 Real-time LOC Calculation** - AI can now answer Lines of Code questions (Oct 31, 2025)
  - Added SQLite query to calculate LOC from COBOL file content
  - Excludes comments (lines starting with `*`) and blank lines
  - Provides top 5 files by LOC when user asks about file size or complexity
  - AI receives actual LOC data for accurate ASCII tables and analysis

### Changed
- **📚 Complete Documentation Alignment** - All docs updated with current architecture (Oct 31, 2025)
  - README.md: Enhanced "Three Ways to Run" with database connection details
  - README.md: Added comprehensive "Complete End-to-End Architecture Flow" section
  - README.md: Updated DevContainer section with auto-connect database information
  - DEVCONTAINER_AUTO_START.md: Added "View Portal Without Running Migration" guide
  - DEVCONTAINER_AUTO_START.md: Documented all three options (demo.sh, manual, auto-start)
  - All documentation now reflects SQLite + Neo4j dual database architecture
  - Step-by-step guides for portal access without running migrations
  - Complete troubleshooting section for portal-only mode

### Added
- **� Smart Neo4j Detection** - Both demo.sh and doctor.sh now detect ANY Neo4j container
  - Scripts detect any Neo4j container on ports 7474/7687 (not just specific container names)
  - Gracefully handles existing Neo4j instances (neo4j-test, cobol-migration-neo4j, etc.)
  - Port conflict detection with helpful error messages
  - Auto-uses accessible Neo4j instead of failing with port conflicts
  - Consistent behavior across demo.sh and doctor.sh
  - Better diagnostics when Neo4j ports are blocked but not accessible
  - Updated DEMO.md with Neo4j troubleshooting section
  
- **�💾 Data Persistence Documentation** - Comprehensive guide to data storage and persistence
  - New documentation: `DATA_PERSISTENCE.md` (complete 15KB guide)
  - Explains SQLite (Data/migration.db) and Neo4j (Docker volumes) storage
  - Confirms 44 migration runs currently stored and persisted
  - Details what survives restarts, shutdowns, and container deletions
  - Backup and restore procedures for both databases
  - Security considerations and .gitignore exclusions
  - Added explicit Data/ directory exclusion to .gitignore
  - Documented database schema and volume configuration
  
- **🌐 Portal Preview Mode** - Run portal without COBOL migration to explore UI
  - New documentation: `PORTAL_PREVIEW.md` with complete preview guide
  - Command: `cd McpChatWeb && dotnet run --urls http://localhost:5028`
  - Perfect for first-time users, testing, development, and demos
  - Shows portal interface, API docs, and empty graph visualization
  - Updated `doctor.sh` help text to include preview command
  - Added preview sections to `DEVCONTAINER_AUTO_START.md` and `DEMO.md`
  
- **🚀 DevContainer Auto-Start** - Portal and dashboards start automatically after migration
  - `postStartCommand` detects previous migration (`Data/migration.db`) and auto-starts portal
  - Portal launches automatically on container restart at http://localhost:5028
  - Neo4j restarts automatically on container reopen
  - Clear status messages show which services are running
  - New documentation: `DEVCONTAINER_AUTO_START.md` with full configuration guide
  
- **🔒 Locked Port Configuration** - Ports cannot change to ensure consistency
  - Portal permanently locked to port 5028 (was inconsistent 5250/5028)
  - Neo4j Browser locked to port 7474
  - Neo4j Bolt locked to port 7687
  - All scripts enforce exact ports with validation
  - DevContainer `portsAttributes` configured with labels and auto-forward behavior

- **🛠️ Enhanced Doctor.sh Auto-Fixing** - Automatic issue detection and resolution
  - Detects Neo4j container not running → Auto-starts it
  - Detects Azure OpenAI misconfiguration → Shows setup command
  - Detects port 5028 conflicts → Kills conflicting processes
  - Comprehensive diagnostics run automatically on migration failure
  - Better error messages with actionable fix commands
  - Port validation ensures 5028 is available before portal launch
  - Neo4j health check before starting portal
  - Explicit port enforcement: `export MCP_WEB_PORT=5028`, `ASPNETCORE_URLS`, `ASPNETCORE_HTTP_PORTS`

### Fixed
- **Windows File Writing Issues** - Comprehensive cross-platform file writing improvements
  - Added retry logic for Windows file locking issues (3 retries with 100ms delay)
  - Added Windows reserved filename detection (CON, PRN, AUX, NUL, COM1-9, LPT1-9)
  - Improved Windows MAX_PATH (260 char) handling with automatic path shortening
  - Added fallback to flat directory structure when paths exceed limits
  - Fixed UTF-8 encoding to not include BOM for better Java compatibility
  - Normalized line endings to platform-specific format (CRLF on Windows, LF on Unix/Mac)
  - Enhanced directory creation with retry logic for transient failures
  - Improved error messages for path-too-long, access-denied, and I/O errors
  - Added detection of Windows platform to apply OS-specific path length validation
  - Sanitized filenames to remove leading/trailing spaces and dots (Windows requirement)

- **Port Consistency Across All Files** - Standardized to port 5028
  - Fixed `demo.sh` port check from 5250 to 5028 (line 26: `portal_running()` function)
  - Updated `QUICK_START.md` - All 7 references changed from 5250 to 5028
  - Updated `McpChatWeb/wwwroot/index.html` - All API examples use 5028
  - Updated `QUERY_GUIDE.md` - Portal URL changed to 5028
  - Updated `DOCS_UPDATE_2025_10_08.md` - Port verification updated to 5028
  - Updated `MCP_CONNECTION_FIX.md` - All curl examples use 5028
  - `doctor.sh` already used correct port 5028 (no changes needed)

### Changed
- **DevContainer Startup Messages** - Enhanced with clear instructions and service URLs
  - `postCreateCommand` shows Quick Start guide with fixed ports
  - `postStartCommand` detects previous migration and shows auto-start status
  - All messages include service URLs: Portal (5028), Neo4j (7474), Bolt (7687)
- **Doctor.sh Launch Flow** - Portal starts with better feedback and validation
  - Always uses port 5028 with explicit environment variables
  - Shows success message: "✅ Migration completed successfully!"
  - Displays "🚀 Starting Portal and Dashboards..."
  - Outputs service URLs: Portal and Neo4j Browser
  - Working directory changes to `McpChatWeb` before launch

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
- **Portal URL standardized** - All documentation now references http://localhost:5250 (was inconsistent 5028/5250)

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
