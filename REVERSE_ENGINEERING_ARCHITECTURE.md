# Reverse Engineering Process Architecture

## High-Level Flow

```mermaid
flowchart TB
    Start([User Runs Reverse Engineering]) --> Input[/COBOL Source Files/]
    Input --> Process[ReverseEngineeringProcess]
    
    Process --> Step1[Step 1: File Discovery]
    Step1 --> Step2[Step 2: Technical Analysis]
    Step2 --> Step3[Step 3: Business Logic Extraction]
    Step3 --> Generate[Generate Documentation]
    
    Generate --> Output1[/reverse-engineering-details.md/]
    
    Output1 --> End([Complete])
    
    style Process fill:#e1f5ff,stroke:#333,stroke-width:2px,color:#000
    style Step1 fill:#fff4e1,stroke:#333,stroke-width:2px,color:#000
    style Step2 fill:#fff4e1,stroke:#333,stroke-width:2px,color:#000
    style Step3 fill:#fff4e1,stroke:#333,stroke-width:2px,color:#000
    style Generate fill:#e8f5e9,stroke:#333,stroke-width:2px,color:#000
    style Output1 fill:#f3e5f5,stroke:#333,stroke-width:2px,color:#000
```

## Detailed Architecture

```mermaid
graph TB
    subgraph "Input Layer"
        COBOL[COBOL Source Files<br/>*.cbl, *.cpy]
    end
    
    subgraph "Orchestration Layer"
        REProcess[ReverseEngineeringProcess<br/>Orchestrates the workflow]
        FileHelper[FileHelper<br/>Scans directories]
    end
    
    subgraph "AI Agent Layer"
        CobolAnalyzer[CobolAnalyzerAgent<br/>Analyzes COBOL structure]
        BusinessLogic[BusinessLogicExtractorAgent<br/>Extracts business rules]
    end
    
    subgraph "AI Service Layer"
        SK[Semantic Kernel]
        Azure[Azure OpenAI<br/>GPT-4o]
    end
    
    subgraph "Data Models"
        CobolFile[CobolFile]
        CobolAnalysis[CobolAnalysis]
        BusinessLogicModel[BusinessLogic<br/>- UserStories<br/>- Features<br/>- BusinessRules]
    end
    
    subgraph "Output Layer"
        DetailsMD[reverse-engineering-details.md<br/>Business logic & technical analysis]
    end
    
    COBOL --> FileHelper
    FileHelper --> REProcess
    REProcess --> CobolAnalyzer
    REProcess --> BusinessLogic
    
    CobolAnalyzer --> SK
    BusinessLogic --> SK
    SK --> Azure
    
    CobolAnalyzer --> CobolAnalysis
    BusinessLogic --> BusinessLogicModel
    
    CobolAnalysis --> BusinessLogic
    
    BusinessLogicModel --> DetailsMD
    CobolAnalysis --> DetailsMD
    
    style REProcess fill:#4fc3f7,stroke:#333,stroke-width:2px,color:#000
    style CobolAnalyzer fill:#81c784,stroke:#333,stroke-width:2px,color:#000
    style BusinessLogic fill:#81c784,stroke:#333,stroke-width:2px,color:#000
    style Azure fill:#ff9800,stroke:#333,stroke-width:2px,color:#000
    style DetailsMD fill:#ba68c8,stroke:#333,stroke-width:2px,color:#000
```

## Step-by-Step Process Flow

```mermaid
sequenceDiagram
    participant User
    participant Process as ReverseEngineeringProcess
    participant FileHelper
    participant CobolAgent as CobolAnalyzerAgent
    participant BusinessAgent as BusinessLogicExtractorAgent
    participant AI as Azure OpenAI GPT-4o
    participant FS as File System
    
    User->>Process: Run reverse-engineer command
    
    rect rgb(255, 244, 225)
    Note over Process,FileHelper: Step 1: File Discovery
    Process->>FileHelper: ScanDirectoryForCobolFilesAsync()
    FileHelper->>FS: Read *.cbl, *.cpy files
    FS-->>FileHelper: File list
    FileHelper-->>Process: List<CobolFile>
    end
    
    rect rgb(255, 244, 225)
    Note over Process,AI: Step 2: Technical Analysis
    Process->>CobolAgent: AnalyzeCobolFilesAsync()
    loop For each COBOL file
        CobolAgent->>AI: Analyze structure (YAML format)
        AI-->>CobolAgent: Program description, divisions, paragraphs
    end
    CobolAgent-->>Process: List<CobolAnalysis>
    end
    
    rect rgb(255, 244, 225)
    Note over Process,AI: Step 3: Business Logic Extraction
    Process->>BusinessAgent: ExtractBusinessLogicAsync()
    loop For each COBOL file
        BusinessAgent->>AI: Extract business rules & purpose
        AI-->>BusinessAgent: Business purpose, rules, features
    end
    BusinessAgent-->>Process: List<BusinessLogic>
    end
    
    rect rgb(232, 245, 233)
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
      Output: Simple markdown
```

## Data Flow

```mermaid
flowchart LR
    subgraph Input
        F1[COBOL File 1]
        F2[COBOL File 2]
        F3[COBOL File N]
    end
    
    subgraph "Stage 1: Technical"
        A1[CobolAnalysis 1]
        A2[CobolAnalysis 2]
        A3[CobolAnalysis N]
    end
    
    subgraph "Stage 2: Business"
        B1[BusinessLogic 1]
        B2[BusinessLogic 2]
        B3[BusinessLogic N]
    end
    
    subgraph "Stage 3: Output"
        BL[reverse-engineering-details.md]
    end
    
    F1 --> A1 --> B1 --> BL
    F2 --> A2 --> B2 --> BL
    F3 --> A3 --> B3 --> BL
    
    A1 --> BL
    A2 --> BL
    A3 --> BL
    
    style BL fill:#e1bee7,stroke:#333,stroke-width:2px,color:#000
```

## Key Design Decisions

### 1. **Sequential Processing**
- Each agent processes files one at a time
- Allows for progress tracking and error handling per file
- Could be parallelized in future for performance

### 2. **Simplified Prompts**
- Direct, actionable instructions to AI
- Removed complex classification systems
- Focus on extraction over categorization

### 3. **Unified Output**
- Single markdown file: `reverse-engineering-details.md`
- Combines business logic and technical analysis
- Focus on actionable documentation

### 4. **Agent Specialization**
- **CobolAnalyzerAgent**: Technical structure (what's in the code)
- **BusinessLogicExtractorAgent**: Business intent (what it means)

### 5. **Model Simplification**
- Removed unused fields and classes
- Cleaner data structures matching actual usage
- Focused on business logic extraction

## Performance Characteristics

For a single COBOL file (~1000 lines):
- **Step 1 (File Discovery)**: < 1 second
- **Step 2 (Technical Analysis)**: ~30 seconds, ~3000 tokens
- **Step 3 (Business Logic)**: ~10 seconds, ~1300 tokens
- **Documentation Generation**: < 1 second

**Total**: ~40 seconds per file

## Token Usage Pattern

```mermaid
pie title Token Distribution per File
    "CobolAnalyzer (Input)" : 11734
    "CobolAnalyzer (Output)" : 3122
    "BusinessLogic (Input)" : 12071
    "BusinessLogic (Output)" : 1285
```

## Future Enhancements

1. **Parallel Processing**: Process multiple files concurrently
2. **Incremental Analysis**: Cache results, only re-analyze changed files
3. **Domain Glossary**: Add business term definitions to improve accuracy
4. **Pattern Library**: Build reusable patterns from successful analyses
5. **Quality Metrics**: Score completeness and confidence of extractions
