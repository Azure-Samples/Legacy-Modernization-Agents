# Changelog

All notable changes to this repository are documented here.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased] - 2025-10-21

### Added - Reverse Engineering Feature
- **🔍 Business Logic Extraction**: Standalone reverse engineering capability
  - New `reverse-engineer` command for extracting business logic without full migration
  - Generates user stories from interactive/transactional COBOL logic
  - Creates feature descriptions for batch/calculation processes
  - Extracts business rules, data entities, and domain models
  - Output format: Markdown files in `reverse-engineering-output/` folder

- **🛠️ Utility Code Analysis**: Identifies modernization opportunities
  - Detects date/time operations, string manipulation, and mathematical operations
  - Classifies code as standard utility vs. business-specific
  - Multi-criteria detection using pattern matching, business constants, and domain data analysis
  - Provides modern Java equivalents with confidence levels (HIGH/MEDIUM/LOW)
  - Estimates migration effort and provides example code

- **📝 New Agents**:
  - `BusinessLogicExtractorAgent` - Extracts WHAT the code does (business perspective)
  - `UtilityCodeAnalyzerAgent` - Identifies utility patterns and modernization opportunities

- **🏗️ New Data Models**:
  - `BusinessLogic` - Structured business logic with user stories, features, and rules
  - `UserStory` - User story format with role, action, benefit, and acceptance criteria
  - `FeatureDescription` - Feature format for batch processes with inputs/outputs
  - `BusinessRule` - Business rules with conditions and actions
  - `UtilityCodeAnalysis` - Utility code patterns and modernization recommendations
  - `ModernizationOpportunity` - Specific recommendations with confidence and effort estimates

- **📊 Reverse Engineering Process**:
  - `ReverseEngineeringProcess` - Standalone orchestrator that can run independently
  - Four-step process: File Discovery → Technical Analysis → Business Logic Extraction → Utility Analysis
  - Generates three output files:
    - `business-logic.md` - User stories, features, and business rules
    - `technical-details.md` - Utility code analysis and modernization recommendations
    - `summary.md` - Overview, statistics, and next steps

- **💻 CLI Commands**:
  - `dotnet run reverse-engineer --cobol-source <path>` - Run reverse engineering only
  - `--reverse-engineer-output <path>` - Specify custom output folder
  - `--reverse-engineer-only` - Skip Java conversion in integrated mode

- **🩺 Enhanced Configuration Doctor**: Added reverse engineering component validation
  - Checks for all 5 reverse engineering components (models, agents, process)
  - Displays individual component status in doctor diagnostics
  - Shows component count (e.g., "5/5 components present")
  - Reports incomplete installations with warning messages

- **🧪 Enhanced Test Suite**: Added reverse engineering validation
  - Component counting and availability checks
  - Output directory detection and file counting
  - Updated migration options to include reverse engineering
  - Shows expected results for reverse engineering runs

- **✅ Enhanced Validation**: Added reverse engineering feature check
  - Validates all 5 components are present
  - Auto-creates `reverse-engineering-output` directory
  - Reports feature status (Complete/Incomplete/Not Installed)

- **🚀 New `reverse-eng` Command**: Dedicated command for reverse engineering
  - Usage: `./doctor.sh reverse-eng` (aliases: reverse-engineer, reverse)
  - Configuration validation before execution
  - Component availability check with helpful error messages
  - COBOL file count verification
  - Detailed progress output with next steps guidance
  - Success/failure reporting with exit codes

- **📚 Documentation Updates**:
  - Updated help text with reverse-eng command
  - Added usage examples for reverse engineering
  - Reference to REVERSE_ENGINEERING.md in available commands
  - Clear separation between technical and business analysis

- **🛠️ Directory Management**:
  - Auto-creates `reverse-engineering-output` directory
  - Validates directory structure on all commands
  - Checks for existing reverse engineering output

### Changed
- **doctor.sh**: Comprehensive reverse engineering integration
  - Added reverse engineering section to doctor diagnostics
  - Enhanced test command with component validation
  - Enhanced validate command with feature detection
  - Added `run_reverse_engineering()` function
  - Updated main routing with reverse-eng aliases
  - Updated directory creation to include reverse-engineering-output

### Use Cases Supported
1. **Documentation Only**: Extract business logic for RFP/contractor briefing without migration
2. **Assessment Before Migration**: Review what needs migration before committing to full process
3. **Selective Modernization**: Identify high-value, low-effort improvements
4. **Integrated Workflow**: Run as part of full migration pipeline with review step

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
