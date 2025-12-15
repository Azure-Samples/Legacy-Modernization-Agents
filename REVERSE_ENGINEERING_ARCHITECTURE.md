# Reverse Engineering Process Architecture

> **Version:** 0.2 (Smart Chunking)  
> **Last Updated:** December 2025

## Overview

The reverse engineering process extracts business logic from COBOL source code, generating comprehensive documentation. Version 0.2 introduces **Smart Chunking** for large files (>150K chars or >3000 lines), enabling analysis of enterprise-scale codebases.

## AI Models Used

| Component | Model | Purpose |
|-----------|-------|---------|
| COBOL Analyzer | `gpt-5.1-codex-mini` | Technical structure analysis (divisions, paragraphs, data) |
| Business Logic Extractor | `gpt-5.1-codex-mini` | Business rules, features, user stories extraction |
| Portal Chat | `gpt-5.1-chat` | Interactive Q&A about migration results |

**Configuration:** `Config/appsettings.json`
```json
{
  "AISettings": {
    "CobolAnalyzerModelId": "gpt-5.1-codex-mini",
    "ChatModelId": "gpt-5.1-chat",
    "MaxTokens": 16384,
    "Temperature": 0.1
  }
}
```

## High-Level Flow

```mermaid
flowchart TB
    Start([User Runs Reverse Engineering]) --> Input[/COBOL Source Files/]
    Input --> SizeCheck{File Size Check}
    
    SizeCheck -->|"< 150K chars<br/>< 3000 lines"| Direct[Direct Processing<br/>ReverseEngineeringProcess]
    SizeCheck -->|">= 150K chars<br/>or >= 3000 lines"| Chunked[Smart Chunking<br/>ChunkedReverseEngineeringProcess]
    
    Direct --> Step1[CobolAnalyzerAgent]
    Chunked --> Chunk[ChunkingOrchestrator]
    Chunk --> ParallelAnalysis[Parallel Chunk Analysis<br/>Up to 6 workers]
    ParallelAnalysis --> Step1
    
    Step1 --> Step2[BusinessLogicExtractorAgent]
    Step2 --> Merge[Merge & Deduplicate Results]
    Merge --> Generate[Generate Documentation]
    
    Generate --> Output1[/reverse-engineering-details.md/]
    
    Output1 --> End([Complete])
    
    style SizeCheck fill:#fff4e1,stroke:#333,stroke-width:2px,color:#000
    style Chunked fill:#e1f5ff,stroke:#333,stroke-width:2px,color:#000
    style Direct fill:#e8f5e9,stroke:#333,stroke-width:2px,color:#000
    style ParallelAnalysis fill:#e1f5ff,stroke:#333,stroke-width:2px,color:#000
    style Output1 fill:#f3e5f5,stroke:#333,stroke-width:2px,color:#000
```

## Detailed Architecture

```mermaid
graph TB
    subgraph "Input Layer"
        COBOL[COBOL Source Files<br/>*.cbl, *.cpy]
    end
    
    subgraph "Orchestration Layer"
        FileHelper[FileHelper<br/>Scans directories]
        SizeRouter{Size Router<br/>>150K chars or >3K lines?}
        REProcess[ReverseEngineeringProcess<br/>Small files]
        ChunkedRE[ChunkedReverseEngineeringProcess<br/>Large files]
        ChunkOrch[ChunkingOrchestrator<br/>Creates semantic chunks]
    end
    
    subgraph "AI Agent Layer"
        CobolAnalyzer[CobolAnalyzerAgent<br/>gpt-5.1-codex-mini]
        BusinessLogic[BusinessLogicExtractorAgent<br/>gpt-5.1-codex-mini]
    end
    
    subgraph "AI Service Layer"
        SK[Semantic Kernel]
        Azure[Azure OpenAI<br/>gpt-5.1-codex-mini]
    end
    
    subgraph "Data Models"
        CobolFile[CobolFile]
        CobolAnalysis[CobolAnalysis]
        BusinessLogicModel[BusinessLogic<br/>- UserStories<br/>- Features<br/>- BusinessRules]
    end
    
    subgraph "Chunking Infrastructure"
        ChunkPlan[ChunkingPlan]
        ChunkMeta[chunk_metadata table]
        SigRegistry[SignatureRegistry<br/>Cross-chunk consistency]
    end
    
    subgraph "Output Layer"
        DetailsMD[reverse-engineering-details.md<br/>Business logic & technical analysis]
        DB[(SQLite<br/>migration.db)]
    end
    
    COBOL --> FileHelper
    FileHelper --> SizeRouter
    SizeRouter -->|Small| REProcess
    SizeRouter -->|Large| ChunkedRE
    ChunkedRE --> ChunkOrch
    ChunkOrch --> ChunkPlan
    ChunkPlan --> ChunkMeta
    
    REProcess --> CobolAnalyzer
    ChunkedRE --> CobolAnalyzer
    ChunkedRE --> SigRegistry
    
    CobolAnalyzer --> SK
    BusinessLogic --> SK
    SK --> Azure
    
    CobolAnalyzer --> CobolAnalysis
    BusinessLogic --> BusinessLogicModel
    
    CobolAnalysis --> BusinessLogic
    
    BusinessLogicModel --> DetailsMD
    CobolAnalysis --> DetailsMD
    ChunkMeta --> DB
    
    style ChunkedRE fill:#4fc3f7,stroke:#333,stroke-width:2px,color:#000
    style ChunkOrch fill:#4fc3f7,stroke:#333,stroke-width:2px,color:#000
    style CobolAnalyzer fill:#81c784,stroke:#333,stroke-width:2px,color:#000
    style BusinessLogic fill:#81c784,stroke:#333,stroke-width:2px,color:#000
    style Azure fill:#ff9800,stroke:#333,stroke-width:2px,color:#000
    style DetailsMD fill:#ba68c8,stroke:#333,stroke-width:2px,color:#000
```

## Smart Chunking (v0.2)

### When Chunking is Triggered

Files are automatically routed to `ChunkedReverseEngineeringProcess` when:
- Character count >= 150,000 chars, OR
- Line count >= 3,000 lines

### Chunking Configuration

From `Config/appsettings.json`:
```json
{
  "ChunkingSettings": {
    "MaxTokensPerChunk": 28000,
    "MaxLinesPerChunk": 1500,
    "OverlapLines": 300,
    "MinSemanticUnitSize": 50,
    "EnableChunking": true,
    "EnableParallelProcessing": true,
    "MaxParallelAnalysis": 6,
    "TokenBudgetPerMinute": 300000,
    "ParallelStaggerDelayMs": 2000
  }
}
```

### Chunking Process

```mermaid
sequenceDiagram
    participant Process as ChunkedReverseEngineeringProcess
    participant Orch as ChunkingOrchestrator
    participant DB as SQLite (chunk_metadata)
    participant Agent as CobolAnalyzerAgent
    participant AI as Azure OpenAI (gpt-5.1-codex-mini)
    
    Process->>Orch: AnalyzeFileAsync(largeFile)
    Orch->>Orch: Detect semantic boundaries<br/>(paragraphs, sections)
    Orch-->>Process: ChunkingPlan (N chunks)
    
    Process->>DB: SeedPendingChunksAsync()
    Note over DB: Stores chunk metadata<br/>for portal visibility
    
    par Parallel Processing (up to 6 workers)
        Process->>Agent: AnalyzeChunk(chunk_1)
        Agent->>AI: Technical analysis prompt
        AI-->>Agent: YAML structure
    and
        Process->>Agent: AnalyzeChunk(chunk_2)
        Agent->>AI: Technical analysis prompt
        AI-->>Agent: YAML structure
    and
        Process->>Agent: AnalyzeChunk(chunk_N)
        Agent->>AI: Technical analysis prompt
        AI-->>Agent: YAML structure
    end
    
    Process->>Process: MergeChunkAnalysesAsync()<br/>Deduplicate & order results
    Process->>DB: UpdateChunkStatus("Completed")
```

### Semantic Boundary Detection

Chunks are split at COBOL semantic boundaries to preserve context:

1. **Division boundaries** (IDENTIFICATION, DATA, PROCEDURE)
2. **Section boundaries** (INPUT-OUTPUT SECTION, WORKING-STORAGE, etc.)
3. **Paragraph boundaries** (named paragraphs in PROCEDURE DIVISION)
4. **COPY statement boundaries**

Overlap lines (default: 300) ensure context is maintained across chunks.

## Step-by-Step Process Flow

```mermaid
sequenceDiagram
    participant User
    participant Process as ReverseEngineeringProcess
    participant FileHelper
    participant SizeRouter as Size Router
    participant ChunkedProcess as ChunkedReverseEngineeringProcess
    participant CobolAgent as CobolAnalyzerAgent
    participant BusinessAgent as BusinessLogicExtractorAgent
    participant AI as Azure OpenAI (gpt-5.1-codex-mini)
    participant FS as File System
    
    User->>Process: Run reverse-engineer command
    
    rect rgba(255, 183, 77, 0.3)
    Note over Process,FileHelper: Step 1: File Discovery
    Process->>FileHelper: ScanDirectoryForCobolFilesAsync()
    FileHelper->>FS: Read *.cbl, *.cpy files
    FS-->>FileHelper: File list
    FileHelper-->>Process: List<CobolFile>
    end
    
    rect rgba(100, 181, 246, 0.3)
    Note over Process,ChunkedProcess: Step 2: Size-Based Routing
    Process->>SizeRouter: Categorize files by size
    SizeRouter-->>Process: smallFiles[], largeFiles[]
    
    alt Large files (>150K chars or >3K lines)
        Process->>ChunkedProcess: ProcessLargeFileWithChunkingAsync()
        ChunkedProcess->>ChunkedProcess: Create chunks with overlap
        ChunkedProcess->>AI: Parallel analysis (max 6 workers)
        AI-->>ChunkedProcess: Chunk analyses
        ChunkedProcess->>ChunkedProcess: Merge & deduplicate
        ChunkedProcess-->>Process: Merged CobolAnalysis
    end
    end
    
    rect rgba(255, 183, 77, 0.3)
    Note over Process,AI: Step 3: Technical Analysis (Small Files)
    Process->>CobolAgent: AnalyzeCobolFilesAsync()
    loop For each small COBOL file
        CobolAgent->>AI: Analyze structure (YAML format)
        AI-->>CobolAgent: Program description, divisions, paragraphs
    end
    CobolAgent-->>Process: List<CobolAnalysis>
    end
    
    rect rgba(186, 104, 200, 0.3)
    Note over Process,AI: Step 4: Business Logic Extraction
    Process->>BusinessAgent: ExtractBusinessLogicAsync()
    loop For each COBOL file (all sizes)
        BusinessAgent->>AI: Extract business rules & purpose
        AI-->>BusinessAgent: Business purpose, rules, features
    end
    BusinessAgent-->>Process: List<BusinessLogic>
    end
    
    rect rgba(129, 199, 132, 0.3)
    Note over Process,FS: Generate Documentation
    Process->>Process: GenerateReverseEngineeringDetailsMarkdown()
    Process->>FS: Write reverse-engineering-details.md
    end
    
    Process-->>User: ✅ Complete with statistics
```

## Agent Responsibilities

```mermaid
mindmap
  root((Reverse Engineering))
    CobolAnalyzerAgent
      Technical Structure
        Data Divisions
        Procedure Divisions
        Variables & Paragraphs
        Copybook References
      Model: gpt-5.1-codex-mini
      Output Format: YAML
    BusinessLogicExtractorAgent
      Business Purpose
        What the code does
      Business Rules
        IF/WHEN conditions
        Validations
        Calculations
      Features
        Inputs/Outputs
        Processing steps
      Model: gpt-5.1-codex-mini
      Output: Simple markdown
    ChunkingOrchestrator
      Semantic Chunking
        Division boundaries
        Paragraph boundaries
        Section boundaries
      Overlap Management
        300 lines default
        Context preservation
      Parallel Coordination
        Up to 6 workers
        Rate limit aware
```

## Data Flow

```mermaid
flowchart LR
    subgraph Input
        F1[COBOL File 1<br/>2,000 lines]
        F2[COBOL File 2<br/>50,000 lines]
        F3[COBOL File N<br/>500 lines]
    end
    
    subgraph "Size Classification"
        Small[Small Files<br/>Direct Processing]
        Large[Large Files<br/>Smart Chunking]
    end
    
    subgraph "Chunking (Large Files Only)"
        C1[Chunk 1<br/>Lines 1-1500]
        C2[Chunk 2<br/>Lines 1200-2700]
        C3[Chunk N<br/>Lines ...]
    end
    
    subgraph "Analysis"
        A1[Analysis 1]
        A2[Analysis 2]
        A3[Analysis N]
        Merge[Merge & Dedupe]
    end
    
    subgraph "Business Logic"
        B1[BusinessLogic 1]
        B2[BusinessLogic 2]
        B3[BusinessLogic N]
    end
    
    subgraph "Output"
        BL[reverse-engineering-details.md]
        DB[(chunk_metadata)]
    end
    
    F1 --> Small --> A1
    F3 --> Small --> A3
    F2 --> Large --> C1 & C2 & C3
    C1 & C2 & C3 --> Merge --> A2
    
    A1 --> B1 --> BL
    A2 --> B2 --> BL
    A3 --> B3 --> BL
    
    C1 & C2 & C3 --> DB
    
    style Large fill:#e1f5ff,stroke:#333,stroke-width:2px,color:#000
    style Merge fill:#e1f5ff,stroke:#333,stroke-width:2px,color:#000
    style BL fill:#e1bee7,stroke:#333,stroke-width:2px,color:#000
```

## Key Design Decisions

### 1. **Automatic Size-Based Routing**
- Files are categorized based on character count (150K) and line count (3K)
- No manual chunking flags required - detection is automatic
- `SmartMigrationOrchestrator` handles routing for full migrations

### 2. **Parallel Processing**
- Large files: chunks processed in parallel (up to 6 workers)
- Multiple large files: also processed in parallel
- Rate limiting prevents API throttling (TokenBudgetPerMinute: 300,000)

### 3. **Semantic Boundary Detection**
- Chunks split at COBOL paragraph/section boundaries
- Overlap lines (300) maintain cross-chunk context
- Preserves COBOL structure semantics

### 4. **Simplified Prompts**
- Direct, actionable instructions to AI
- Removed complex classification systems
- Focus on extraction over categorization

### 5. **Unified Output**
- Single markdown file: `reverse-engineering-details.md`
- Combines business logic and technical analysis
- Focus on actionable documentation

### 6. **Agent Specialization**
- **CobolAnalyzerAgent**: Technical structure (what's in the code)
- **BusinessLogicExtractorAgent**: Business intent (what it means)

### 7. **Progress Visibility**
- Chunk metadata stored in SQLite for portal dashboard
- Real-time progress tracking via SSE updates
- Portal available at `http://localhost:5028`

## Performance Characteristics

### Small File (~1,000 lines)
- **File Discovery**: < 1 second
- **Technical Analysis**: ~30 seconds, ~3,000 tokens
- **Business Logic**: ~10 seconds, ~1,300 tokens
- **Documentation**: < 1 second
- **Total**: ~40 seconds per file

### Large File (~50,000 lines, chunked)
- **Chunking**: ~2 seconds
- **Parallel Analysis**: ~3-5 minutes (6 workers)
- **Merge & Dedupe**: ~2 seconds
- **Business Logic**: ~30 seconds
- **Total**: ~5-6 minutes per file

### Parallelization Benefits
| Workers | 50K LOC File | Speedup |
|---------|--------------|---------|
| 1 | ~30 min | 1x |
| 3 | ~12 min | 2.5x |
| 6 | ~6 min | 5x |

## Token Usage Pattern

```mermaid
pie title Token Distribution per Small File
    "CobolAnalyzer (Input)" : 11734
    "CobolAnalyzer (Output)" : 3122
    "BusinessLogic (Input)" : 12071
    "BusinessLogic (Output)" : 1285
```

### Rate Limiting Configuration
```json
{
  "RateLimitSettings": {
    "TokensPerMinute": 300000,
    "MaxInputTokens": 10000,
    "MaxOutputTokens": 16384,
    "MinDelayBetweenRequestsMs": 20000,
    "EnableAutoThrottle": true
  }
}
```

## Database Schema (Chunking)

### chunk_metadata Table
```sql
CREATE TABLE chunk_metadata (
    id INTEGER PRIMARY KEY,
    run_id INTEGER,
    source_file TEXT,
    chunk_index INTEGER,
    start_line INTEGER,
    end_line INTEGER,
    content_hash TEXT,
    status TEXT,  -- Pending, Processing, Completed, Failed
    tokens_used INTEGER,
    processing_time_ms INTEGER,
    created_at TEXT,
    completed_at TEXT
);
```

### signatures Table
```sql
CREATE TABLE signatures (
    id INTEGER PRIMARY KEY,
    run_id INTEGER,
    source_file TEXT,
    signature_type TEXT,  -- Class, Method, Interface
    signature_name TEXT,
    chunk_index INTEGER
);
```

## Portal Integration

The web portal at `http://localhost:5028` provides:

- **Chunks Tab**: Real-time progress of chunk processing
- **RE Report Tab**: Generated business logic documentation
- **Architecture Tab**: Mermaid diagrams from analysis
- **Chat**: Interactive Q&A about results (using `gpt-5.1-chat`)

## Future Enhancements

1. **Incremental Analysis**: Cache results, only re-analyze changed files
2. **Domain Glossary Integration**: Add business term definitions to improve accuracy
3. **Pattern Library**: Build reusable patterns from successful analyses
4. **Quality Metrics**: Score completeness and confidence of extractions
5. **Cross-File Analysis**: Detect patterns across multiple COBOL files
6. **Cost Tracking**: Per-run token usage and API cost reporting

## CLI Commands

```bash
# Run reverse engineering only
./doctor.sh reverse-eng

# Check chunking infrastructure health
./doctor.sh chunking-health

# Start portal to view results
./doctor.sh portal
```

## Troubleshooting

### Large File Not Chunking
- Check file size: `wc -l yourfile.cbl`
- Threshold: 150K chars OR 3000 lines
- Verify ChunkingSettings.EnableChunking = true

### Chunk Processing Slow
- Increase MaxParallelAnalysis (default: 6)
- Check TokenBudgetPerMinute matches your Azure quota
- Monitor rate limiting in logs

### Incomplete Results
- Check chunk_metadata table for Failed status
- Review Logs/ folder for error details
- Run `./doctor.sh chunking-health` for diagnostics
