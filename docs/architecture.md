# Architecture

**Last updated**: 2026-11-18

> For the overview of what runs where and the Docker containers, see [How it fits together](../README.md#-how-it-fits-together) in the README. This page goes deeper into the scan, the conversion loop, storage, agents and concurrency.

## Inside `rekt-full`

```mermaid
flowchart TB
    SRC["📁 source/<br/>COBOL · copybooks · BMS · JCL"] --> ID["rekt-source-identity<br/>name collisions, shadowed copybooks"]
    ID --> STAGE["rekt-evidence stage<br/>+ rekt-preprocess<br/>BMS symbolic maps, platform copybook contracts"]
    STAGE --> PARSE["cobol-rekt container<br/>REKT parser (Java), one AST per program"]
    PARSE --> EVID["rekt-evidence validate · quarantine · write<br/>rekt-diagnostics · doctor-report<br/>missing copybooks, call and COMMAREA closure"]
    EVID --> INGEST["graph-ingest<br/>ASTs + JCL facts → Neo4j"]
    INGEST --> DONE["output/rekt/&lt;scan&gt;/scan.json<br/>Full · Partial · Failed per program"]
    EVID -.->|helper crash| FAIL["Scan marked failed<br/>helper-crashes.log + Logs/doctor-helpers.log"]
    INGEST -.->|helper crash| FAIL
```

Every helper runs through `lma_cli_run` in `tools/lib/lma-cli.sh`. A helper that crashes (an unhandled .NET exception or a runtime abort) fails the scan even when the calling step would have tolerated an ordinary non-zero exit, and its stack trace is kept in both logs. Run `./doctor.sh` to check prerequisites and rerun the scan. See [Estate Mission Control](estate-mission-control.md) for the scan record.

## Inside a conversion

```mermaid
flowchart TB
    S["Selected programs<br/>(--program, --job, or a staged slice)"] --> FACTS["Program facts from the REKT parse,<br/>copybooks and call contracts<br/>(deterministic, no model)"]
    FACTS --> ANALYZE["CobolAnalyzerAgent<br/>structure and logic"]
    ANALYZE --> DEPS["DependencyMapper<br/>CALL / COPY / file / SQL edges → migration Neo4j"]
    DEPS --> CONV["Java or C# converter agent<br/>(chunked for large programs)"]
    CONV --> GUARD["Output guard<br/>each answer must be complete code,<br/>not truncated or empty"]
    GUARD --> GATE{"C#: compile gate<br/>dotnet build"}
    GATE -->|errors| REPAIR["CompileRepairAgent<br/>fixes file by file,<br/>up to CompileGate.MaxRepairRounds,<br/>undoes rounds that make it worse"]
    REPAIR --> GATE
    GATE -->|builds, or rounds used up| PARITY["Conversion parity check<br/>procedures · fields · calls · SQL<br/>vs the COBOL (deterministic)"]
    PARITY -->|gaps| PREPAIR["ParityRepairAgent<br/>source-backed fixes, up to PARITY_REPAIR_MAX_ROUNDS;<br/>kept only if rescoring improves"]
    PREPAIR --> PARITY
    PARITY -->|passes, or rounds used up| JOBS["Jobs from JCL, if any<br/>.NET jobs or Spring Batch"]
    JOBS --> OUT["output/{language}/{run}/<br/>compile-status.json · conversion-parity.json<br/>jobs-manifest.json · migration report"]
    OUT --> LOOP["Portal: 🔁 AI Loop tab<br/>model calls, retries, gates"]
```

Java output has no compile gate yet: build it with `mvn compile` in the output folder. The parity check can stop the run (`ON_LOW_SCORE=stop`); its score is always measured again from the files on disk after repairs. See [Conversion parity](conversion-parity-validation.md) and [Using the generated output](using-generated-output.md).

## Hybrid Database Architecture

This project uses a **dual-database approach** for optimal performance, enhanced with Regex-based deep analysis:

```mermaid
flowchart TB
    subgraph INPUT["📁 Input"]
        COBOL["COBOL Files<br/>source/*.cbl, *.cpy"]
    end
    
    subgraph CONFIG["🔧 Configuration"]
        SETUP_CLI["./doctor.sh setup<br/>(CLI)"]
        SETUP_PORTAL["Portal Setup Modal<br/>(Browser)"]
        SETUP_CLI --> CONFIG_FILE["Config/ai-config.local.env"]
        SETUP_PORTAL --> CONFIG_FILE
        CONFIG_FILE --> PROVIDERS
        subgraph PROVIDERS["AI Providers"]
            AZURE["☁️ Azure OpenAI<br/>(API Key / Entra ID)"]
            COPILOT["🤖 GitHub Copilot SDK<br/>(CLI / PAT)"]
        end
    end

    subgraph PROCESS["⚙️ Processing Pipeline"]
        REGEX["Regex / Syntax Parsing<br/>(Deep SQL/Variable Extraction)"]
        AGENTS["🤖 AI Agents<br/>(MS Agent Framework)"]
        ANALYZER["CobolAnalyzerAgent"]
        EXTRACTOR["BusinessLogicExtractor"]
        CONVERTER["Java/C# Converter"]
        MAPPER["DependencyMapper"]
    end
    
    subgraph STORAGE["💾 Hybrid Storage"]
        SQLITE[("SQLite<br/>Data/migration.db<br/><br/>• Run metadata<br/>• File content<br/>• Raw AI analysis<br/>• Generated code")]
        NEO4J[("Neo4j<br/>bolt://localhost:7687<br/><br/>• Dependencies<br/>• Relationship Graph<br/>• Impact Analysis")]
    end
    
    subgraph OUTPUT["📦 Output"]
        CODE["Java/C# Code<br/>output/java or output/csharp"]
        PORTAL["Web Portal<br/>localhost:5028<br/><br/>• Model Setup &amp; Discovery<br/>• Mission Control<br/>• Prompt Studio<br/>• Chat &amp; Graph"]
    end
    
    COBOL --> REGEX
    REGEX --> AGENTS
    PROVIDERS --> AGENTS
    
    AGENTS --> ANALYZER
    AGENTS --> EXTRACTOR
    AGENTS --> CONVERTER
    AGENTS --> MAPPER
    
    ANALYZER --> SQLITE
    EXTRACTOR --> SQLITE
    CONVERTER --> SQLITE
    CONVERTER --> CODE
    MAPPER --> NEO4J
    
    SQLITE --> PORTAL
    NEO4J --> PORTAL
```

### Why Two Databases?

| Aspect | SQLite | Neo4j |
|--------|--------|-------|
| **Purpose** | Document storage | Relationship mapping |
| **Strength** | Fast queries, simple setup | Graph traversal, visualization |
| **Use Case** | "What's in this file?" | "What depends on this file?" |
| **Query Style** | SQL SELECT | Cypher graph queries |

**Together:** Fast metadata access + Powerful dependency insights 🚀

### Why Dependency Graphs Matter

The Neo4j dependency graph enables:
- **Impact Analysis** - "If I change CUSTOMER.cbl, what else breaks?"
- **Circular Dependency Detection** - Find problematic CALL/COPY cycles
- **Critical File Identification** - Most-connected files = highest risk
- **Migration Planning** - Convert files in dependency order
- **Visual Understanding** - See relationships at a glance in the portal

---

## Agent Pipeline

The migration follows a strict **Deep Code Analysis** pipeline:

```mermaid
sequenceDiagram
    participant U as User
    participant O as Orchestrator
    participant AA as Analyzer Agent
    participant DA as Dependency Agent
    participant SQ as SQLite
    participant CA as Converter Agent

    U->>O: Run "analyze" (Step 1)
    
    rect rgb(240, 248, 255)
        Note over O, SQ: 1. Deep Analysis Phase
        O->>O: Determine File Type<br/>(Program vs Copybook)
        O->>O: Regex Parse (SQL, Variables)
        O->>SQ: Store raw metadata
        O->>AA: Analyze Structure & Logic
        AA->>SQ: Save Analysis Result
    end
    
    rect rgb(255, 240, 245)
        Note over O, SQ: 2. Dependency Phase
        U->>O: Run "dependencies" (Step 2)
        O->>DA: Resolve Calls/Includes
        DA->>SQ: Read definitions
        DA->>SQ: Write graph nodes
    end

    rect rgb(240, 255, 240)
        Note over O, SQ: 3. Conversion Phase
        U->>O: Run "convert" (Step 3)
        O->>SQ: Fetch analysis & deps
        O->>CA: Generate Modern Code
        CA->>SQ: Save generated code
    end
```

## Smart Chunking & Token Strategy

Large COBOL files (>3,000 lines or >150K characters) are automatically split at semantic boundaries (DIVISION → SECTION → paragraph) and processed with content-aware reasoning effort. A three-tier complexity scoring system analyzes each file's COBOL patterns (EXEC SQL, CICS, REDEFINES, etc.) to dynamically allocate reasoning effort and output tokens — simple files get fast processing while complex files get thorough analysis.

The automatic chunking steps, the full token-strategy diagram, constants and complexity scoring weights are in [Smart Chunking & Token Strategy Architecture](smart-chunking-architecture.md).

## 🔄 Agent Flowchart

Seven agents call the model. Everything between them (parsing, chunking, the output guard, the compile gate and the parity score) is deterministic.

```mermaid
flowchart TD
  CLI[["doctor.sh run / convert-only / reverse-eng<br/>or Portal Mission Control"]]
  FACTS["REKT program facts, copybooks,<br/>call contracts (no model)"]

  subgraph RE_PHASE["Reverse engineering (optional)"]
      ANALYZER["CobolAnalyzerAgent<br/>structure and logic"]
      BIZ["BusinessLogicExtractorAgent<br/>use cases, rules, glossary"]
  end

  subgraph DEP_PHASE["Dependencies"]
      MAPPER["DependencyMapperAgent<br/>CALL / COPY / file / SQL"]
  end

  subgraph CONVERT_PHASE["Conversion"]
      CONVERTER["JavaConverterAgent / CSharpConverterAgent<br/>chunk-aware for large programs"]
      OGUARD["Output guard (no model)"]
      CGATE{"C# compile gate"}
      CREPAIR["CompileRepairAgent"]
      PSCORE{"Parity score (no model)"}
      PREPAIR["ParityRepairAgent"]
  end

  SQLITE[("SQLite<br/>Data/migration.db")]
  NEO4J[("Neo4j")]
  OUT["output/{language}/{run}/"]

  CLI --> FACTS --> ANALYZER --> BIZ
  ANALYZER --> SQLITE
  BIZ --> SQLITE
  FACTS --> MAPPER --> NEO4J
  SQLITE --> CONVERTER
  NEO4J --> CONVERTER
  CONVERTER --> OGUARD --> CGATE
  CGATE -->|errors| CREPAIR --> CGATE
  CGATE -->|builds| PSCORE
  PSCORE -->|gaps| PREPAIR --> PSCORE
  PSCORE --> OUT
```

## 🔀 Agent Responsibilities & Interactions

### Advanced Sequence Flow (Mermaid)

```mermaid
sequenceDiagram
  participant User as 🧑 User / doctor.sh
  participant Portal as 🌐 Portal (McpChatWeb)
  participant CLI as CLI Runner
  participant RE as ReverseEngineeringProcess
  participant Analyzer as CobolAnalyzerAgent
  participant BizLogic as BusinessLogicExtractorAgent
  participant Migration as MigrationProcess
  participant DepMap as DependencyMapperAgent
  participant Converter as CodeConverterAgent (Java/C#)
  participant Repo as HybridMigrationRepository
  participant AI as AI Provider (Azure / Copilot SDK)

  rect rgb(245, 240, 255)
      Note over User, AI: 0. Configuration (CLI or Portal)
      alt CLI Setup
          User->>CLI: ./doctor.sh setup
          CLI->>CLI: Select provider, enter credentials
          CLI->>CLI: Write Config/ai-config.local.env
      else Portal Setup
          User->>Portal: Open Setup Modal (🔧)
          Portal->>AI: Connect & discover models
          AI-->>Portal: Available deployments/models
          User->>Portal: Select chat + code models
          Portal->>Portal: Write Config/ai-config.local.env
      end
  end

  User->>CLI: select target language, concurrency flags
  CLI->>RE: start reverse engineering
  RE->>Analyzer: analyze COBOL files (parallel up to max-parallel)
  Analyzer-->>RE: CobolAnalysis[]
  RE->>BizLogic: extract business logic summaries
  BizLogic-->>RE: BusinessLogic[]
  RE->>Repo: persist analyses + documentation
  RE->>Repo: persist BusinessLogic[] to business_logic table
  RE-->>CLI: ReverseEngineeringResult (BusinessLogic[], RunId)
  CLI->>Migration: SetBusinessLogicContext(BusinessLogic[])
  CLI->>Migration: start migration run with latest analyses
  Migration->>Analyzer: reuse or refresh CobolAnalysis
  Migration->>DepMap: build dependency graph (CALL/COPY/...)
  DepMap-->>Migration: DependencyMap
  Migration->>Converter: convert to Java/C# with business logic context
  Converter-->>Migration: CodeFile artifacts
  Migration->>Repo: persist run metadata, graph edges, code files
  Repo-->>Portal: expose MCP resources + REST APIs
  Portal-->>User: portal UI (chat, graph, reports)
```

### CobolAnalyzerAgent
- **Purpose:** Deep structural analysis of COBOL files (divisions, paragraphs, copybooks, metrics).
- **Inputs:** COBOL text from `FileHelper` or cached content.
- **Outputs:** `CobolAnalysis` objects consumed by:
  - `ReverseEngineeringProcess` (for documentation & glossary mapping)
  - `DependencyMapperAgent` (seed data for relationships)
  - `CodeConverterAgent` (guides translation prompts)
- **Interactions:**
  - Uses Azure OpenAI via `ResponsesApiClient` / `IChatClient` with concurrency guard.
  - Results persisted by `SqliteMigrationRepository`.

### BusinessLogicExtractorAgent
- **Purpose:** Convert technical analyses into business language (use cases, user stories, glossary).
- **Inputs:** Output from `CobolAnalyzerAgent` + optional glossary.
- **Outputs:** `BusinessLogic` records and Markdown sections used in `reverse-engineering-details.md`.
- **Interactions:**
  - Runs in parallel with analyzer results.
  - Writes documentation via `FileHelper` and logs via `EnhancedLogger`.
  - Results persisted to the `business_logic` SQLite table via `IMigrationRepository.SaveBusinessLogicAsync`, enabling reuse in subsequent `--skip-reverse-engineering --reuse-re` runs.

### DependencyMapperAgent
- **Purpose:** Identify CALL/COPY/PERFORM/IO relationships and build graph metadata.
- **Inputs:** COBOL files + analyses (line numbers, paragraphs).
- **Outputs:** `DependencyMap` with nodes/edges stored in both SQLite and Neo4j.
- **Interactions:**
  - Feeds the McpChatWeb graph panel and run-selector APIs.
  - Enables multi-run queries (e.g., "show me CALL tree for run 42").

### CodeConverterAgent(s)
- **Variants:** `JavaConverterAgent` or `CSharpConverterAgent` (selected via `TargetLanguage`).
- **Purpose:** Generate target-language code from COBOL analyses and dependency context.
- **Inputs:**
  - `CobolAnalysis` per file
  - Target language settings (Quarkus vs. .NET)
  - Migration run metadata (for logging & metrics)
  - `BusinessLogic` records per file (user stories, features, business rules) — injected only with `--reuse-re`: from the run's own RE in a full run, or loaded from the DB with `--skip-reverse-engineering`
- **Outputs:** `CodeFile` records saved under `output/java/` or `output/csharp/`.
- **Interactions:**
  - Concurrency guards (pipeline slots vs. AI calls) ensure Azure OpenAI limits respected.
  - Results pushed to portal via repositories for browsing/download.

### CompileRepairAgent
- **Purpose:** Fix C# compiler errors in generated files, one file at a time.
- **Inputs:** `dotnet build` diagnostics and the affected files.
- **Outputs:** Repaired files; a round that increases the error count is undone. Limited by `CompileGate.MaxRepairRounds`.

### ParityRepairAgent
- **Purpose:** Close gaps the deterministic parity check found between the COBOL and the generated code.
- **Inputs:** The COBOL source, the generated files, the measured gaps and read-only compiler context.
- **Outputs:** Replacement files, accepted only when rescoring shows higher coverage without loss on another axis. Limited by `PARITY_REPAIR_MAX_ROUNDS` (`0` measures only). See [Conversion parity](conversion-parity-validation.md#bounded-repair-and-final-measurement).

## ⚡ Concurrency Notes
- **Pipeline concurrency (`--max-parallel`)** controls how many files/chunks run simultaneously (e.g., 8).
- **AI concurrency (`--max-ai-parallel`)** caps concurrent Azure OpenAI calls (e.g., 3) to avoid throttling.
- Both values can be surfaced via CLI flags or environment variables to let `doctor.sh` tune runtime.

## 🔄 End-to-End Data Flow
1. `doctor.sh run` → load configs → choose target language
2. **Source scanning** - Reads all `.cbl`/`.cpy` files from `source/`
3. **Analysis** - `CobolAnalyzerAgent` extracts structure; `BusinessLogicExtractorAgent` generates documentation
4. **Dependencies** - `DependencyMapperAgent` maps CALL/COPY/PERFORM relationships → Neo4j
5. **Conversion** - `JavaConverterAgent` or `CSharpConverterAgent` generates target code → `output/`
6. **Storage** - `HybridMigrationRepository` writes metadata to SQLite, graph edges to Neo4j
7. **Portal** - `McpChatWeb` surfaces chat, graphs, and reports at http://localhost:5028

The portal UI is described in [Portal](portal.md).
