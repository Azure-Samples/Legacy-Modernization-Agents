# Smart Chunking: Deep Technical Documentation

**Last updated**: 2026-04-28

## Overview

The **Smart Chunking System v0.2** is an intelligent file processing pipeline that handles legacy COBOL files of any size (tested up to 999,999 lines of code) while maintaining **100% code consistency and integrity**. The system uses semantic boundary detection, SQLite-backed consistency registries, and parallel processing with rate limiting to ensure no code is ever truncated or lost during AI-powered migration.

### Key Capabilities
- **Automatic size detection**: Files routed to direct or chunked processing based on configurable thresholds
- **Semantic boundary detection**: Chunks split at COBOL division/section/paragraph boundaries
- **Cross-chunk consistency**: SQLite-backed SignatureRegistry and TypeMappingTable ensure consistent naming
- **Parallel processing**: Up to 6 chunks processed concurrently with rate limiting
- **Forward reference resolution**: References to not-yet-converted code handled gracefully
- **Progressive context compression**: Older chunk summaries compressed to stay within token budgets
- **No truncation policy**: System fails loudly if chunk too large, NEVER silently drops code

### AI Models Used
| Component | Model | Purpose |
|-----------|-------|---------|
| CobolAnalyzerAgent | `gpt-5.1-codex-mini` | Structural analysis of COBOL files |
| BusinessLogicExtractorAgent | `gpt-5.1-codex-mini` | Extract user stories, features, business rules |
| JavaConverterAgent | `gpt-5.1-codex-mini` | Convert COBOL to Quarkus Java |
| CSharpConverterAgent | `gpt-5.1-codex-mini` | Convert COBOL to C# |
| Portal Q&A | `gpt-5.1-chat` | Interactive migration Q&A |

---

## Architecture Components

### Core Classes
| Class | File | Purpose |
|-------|------|---------|
| `SmartMigrationOrchestrator` | Processes/SmartMigrationOrchestrator.cs | Entry point - routes files to direct/chunked processing |
| `ChunkedMigrationProcess` | Processes/ChunkedMigrationProcess.cs | Full chunked conversion pipeline |
| `ChunkedReverseEngineeringProcess` | Processes/ChunkedReverseEngineeringProcess.cs | Chunked analysis/business logic extraction |
| `ChunkingOrchestrator` | Chunking/ChunkingOrchestrator.cs | Coordinates chunking operations |
| `SemanticUnitChunker` | Chunking/Core/SemanticUnitChunker.cs | Semantic boundary detection |
| `CobolAdapter` | Chunking/Adapters/CobolAdapter.cs | COBOL-specific parsing |
| `SignatureRegistry` | Chunking/Core/SignatureRegistry.cs | SQLite method signature consistency |
| `TypeMappingTable` | Chunking/Core/TypeMappingTable.cs | SQLite variable type consistency |
| `ChunkContextManager` | Chunking/Context/ChunkContextManager.cs | Builds context for each chunk |

---

## Pipeline Flow: SmartMigrationOrchestrator

The `SmartMigrationOrchestrator` is the entry point that routes files to the appropriate processing path:

```
┌─────────────────────────────────────────────────────────────────────────────────┐
│                    SMART MIGRATION ORCHESTRATOR                                  │
│                    (SmartMigrationOrchestrator.cs)                               │
├─────────────────────────────────────────────────────────────────────────────────┤
│                                                                                  │
│   Source Files → CategorizeFiles() → Size Check                                 │
│                                                                                  │
│         ┌────────────────────────────────┬─────────────────────────────┐        │
│         │   RequiresChunking() = false   │  RequiresChunking() = true  │        │
│         ▼                                ▼                              │        │
│   ┌─────────────────┐            ┌─────────────────┐                   │        │
│   │   SMALL FILE    │            │   LARGE FILE    │                   │        │
│   │ < 150K chars    │            │ > 150K chars    │                   │        │
│   │ < 3,000 lines   │            │ > 3,000 lines   │                   │        │
│   └────────┬────────┘            └────────┬────────┘                   │        │
│            │                              │                             │        │
│            ▼                              ▼                             │        │
│   ProcessSmallFilesOnlyAsync()   ProcessLargeFilesAsync()              │        │
│   (MigrationProcess)             (ChunkedMigrationProcess)             │        │
│                                                                         │        │
│   ⚡ Direct conversion            📦 Smart chunking with                │        │
│   (Single API call per agent)     consistency guarantees               │        │
└─────────────────────────────────────────────────────────────────────────────────┘
```

### Threshold Configuration (ChunkingSettings.cs)

```csharp
public class ChunkingSettings
{
    // Primary auto-detection thresholds
    public int AutoChunkCharThreshold { get; set; } = 150_000;   // 150K characters
    public int AutoChunkLineThreshold { get; set; } = 3_000;     // 3,000 lines

    // Chunk sizing
    public int MaxLinesPerChunk { get; set; } = 1500;            // Lines per chunk
    public int OverlapLines { get; set; } = 300;                 // Context overlap
    public int MaxTokensPerChunk { get; set; } = 28_000;         // Token budget

    // Parallel processing
    public int MaxParallelChunks { get; set; } = 6;              // Conversion workers
    public int MaxParallelAnalysis { get; set; } = 6;            // Analysis workers
    public bool EnableParallelProcessing { get; set; } = true;

    // Rate limiting
    public int TokenBudgetPerMinute { get; set; } = 300_000;     // Azure TPM limit
    public double RateLimitSafetyFactor { get; set; } = 0.7;     // 70% utilization

    // Progressive compression
    public double CompressionRatio { get; set; } = 0.3;          // 30% of original
    public int FullDetailChunkWindow { get; set; } = 3;          // Full detail for last 3 chunks

    // Helper method used by orchestrator
    public bool RequiresChunking(int charCount, int lineCount)
    {
        return charCount > AutoChunkCharThreshold || lineCount > AutoChunkLineThreshold;
    }
}
```

---

## Phase 1: Reverse Engineering (Analysis)

### Small Files (< 150K chars, < 3K lines)
```
┌─────────────────────────────────────────────────────────────────────────────────┐
│ SMALL FILE REVERSE ENGINEERING                                                   │
│                                                                                  │
│   file.cbl (50K chars)                                                          │
│        │                                                                         │
│        ▼                                                                         │
│   ┌────────────────────┐     ┌────────────────────┐     ┌──────────────────┐   │
│   │ CobolAnalyzerAgent │ ──► │ BusinessLogic      │ ──► │ Output:          │   │
│   │ (Single API call)  │     │ ExtractorAgent     │     │ Analysis +       │   │
│   │ gpt-5.1-codex-mini │     │ (Single API call)  │     │ Business Rules   │   │
│   └────────────────────┘     └────────────────────┘     └──────────────────┘   │
│                                                                                  │
│   Total API Calls: 2                                                            │
└─────────────────────────────────────────────────────────────────────────────────┘
```

### Large Files (> 150K chars OR > 3K lines)
```
┌─────────────────────────────────────────────────────────────────────────────────┐
│ LARGE FILE REVERSE ENGINEERING (ChunkedReverseEngineeringProcess)               │
│                                                                                  │
│   bigfile.cbl (400K chars, 10K lines)                                           │
│        │                                                                         │
│        ▼                                                                         │
│   ┌────────────────────────────────────────────────────────────────────────┐    │
│   │ STEP 1: CHUNKING ORCHESTRATOR (ChunkingOrchestrator.cs)                │    │
│   │                                                                        │    │
│   │   Settings: MaxLinesPerChunk=1500, OverlapLines=300                   │    │
│   │                                                                        │    │
│   │   10,000 lines ÷ (1,500 - 300 overlap) = ~8 chunks                    │    │
│   │   Each chunk: 1,500 lines with 300-line overlap for context           │    │
│   └────────────────────────────────────────────────────────────────────────┘    │
│        │                                                                         │
│        ▼                                                                         │
│   ┌────────────────────────────────────────────────────────────────────────┐    │
│   │ STEP 2: SEMANTIC BOUNDARY DETECTION (SemanticUnitChunker.cs)           │    │
│   │                                                                        │    │
│   │   Uses CobolAdapter to identify natural split points:                  │    │
│   │   - IDENTIFICATION DIVISION                                            │    │
│   │   - DATA DIVISION / WORKING-STORAGE SECTION                           │    │
│   │   - PROCEDURE DIVISION sections/paragraphs                            │    │
│   │                                                                        │    │
│   │   Splits at semantic boundaries when possible                          │    │
│   │   Falls back to line-based if a single unit is too large              │    │
│   └────────────────────────────────────────────────────────────────────────┘    │
│        │                                                                         │
│        ▼                                                                         │
│   ┌──────────┬──────────┬──────────┬──────────┬──────────┬──────────┬────────┐ │
│   │ Chunk 0  │ Chunk 1  │ Chunk 2  │ Chunk 3  │ Chunk 4  │ Chunk 5  │ Chunk 6│ │
│   │ Lines    │ Lines    │ Lines    │ Lines    │ Lines    │ Lines    │ Lines  │ │
│   │ 1-1500   │ 1201-2700│ 2401-3900│ 3601-5100│ 4801-6300│ 6001-7500│ 7201+  │ │
│   │ (overlap)│ (overlap)│ (overlap)│ (overlap)│ (overlap)│ (overlap)│        │ │
│   └────┬─────┴────┬─────┴────┬─────┴────┬─────┴────┬─────┴────┬─────┴───┬────┘ │
│        │          │          │          │          │          │         │       │
│        └──────────┴──────────┴──────────┼──────────┴──────────┴─────────┘       │
│                                         │                                        │
│                                         ▼                                        │
│   ┌─────────────────────────────────────────────────────────────────────────┐   │
│   │ STEP 3: PARALLEL PROCESSING (MaxParallelAnalysis=6)                     │   │
│   │                                                                         │   │
│   │   SemaphoreSlim limits concurrent API calls                            │   │
│   │   Stagger delay: 500ms between chunk starts                            │   │
│   │                                                                         │   │
│   │   For each chunk (parallel up to 6):                                   │   │
│   │   1. CobolAnalyzerAgent.AnalyzeAsync() → Structure analysis            │   │
│   │   2. BusinessLogicExtractorAgent.ExtractAsync() → Business rules       │   │
│   └─────────────────────────────────────────────────────────────────────────┘   │
│        │                                                                         │
│        ▼                                                                         │
│   ┌─────────────────────────────────────────────────────────────────────────┐   │
│   │ STEP 4: MERGE RESULTS (MergeAnalyses + MergeBusinessLogics)             │   │
│   │                                                                         │   │
│   │   MergeAnalyses():                                                      │   │
│   │   - Combine all DataDivisions (deduplicated by name)                   │   │
│   │   - Combine all ProcedureDivisions (deduplicated)                      │   │
│   │   - Combine all Paragraphs (by unique name)                            │   │
│   │   - Combine all Variables (by unique name)                             │   │
│   │   - Combine all CopybooksReferenced                                    │   │
│   │                                                                         │   │
│   │   MergeBusinessLogics():                                                │   │
│   │   - Combine UserStories with unique IDs (US-1, US-2, ...)             │   │
│   │   - Combine Features with unique IDs (F-1, F-2, ...)                  │   │
│   │   - Combine BusinessRules with unique IDs (BR-1, BR-2, ...)           │   │
│   │   - Deduplicate rules by description to avoid duplicates              │   │
│   └─────────────────────────────────────────────────────────────────────────┘   │
│        │                                                                         │
│        ▼                                                                         │
│   Single unified reverse-engineering-details.md                                  │
└─────────────────────────────────────────────────────────────────────────────────┘
```

---

## Phase 2: Code Conversion (Migration)

### Small Files (Direct Conversion)
```
┌─────────────────────────────────────────────────────────────────────────────────┐
│ SMALL FILE CONVERSION (MigrationProcess)                                         │
│                                                                                  │
│   file.cbl → ChunkAwareJavaConverter OR ChunkAwareCSharpConverter → file.java   │
│              (Single API call with gpt-5.1-codex-mini)                           │
└─────────────────────────────────────────────────────────────────────────────────┘
```

### Large Files (ChunkedMigrationProcess)
```
┌─────────────────────────────────────────────────────────────────────────────────┐
│ LARGE FILE CONVERSION (ChunkedMigrationProcess)                                  │
│                                                                                  │
│   bigfile.cbl (400K chars)                                                      │
│        │                                                                         │
│        ▼                                                                         │
│   ╔════════════════════════════════════════════════════════════════════════╗    │
│   ║ PHASE 1: BUILD SYMBOL TABLE (CobolAdapter - Before any AI calls)       ║    │
│   ║                                                                        ║    │
│   ║   Parse the ENTIRE file using regex patterns to extract:               ║    │
│   ║                                                                        ║    │
│   ║   Regex Patterns Used:                                                 ║    │
│   ║   ┌────────────────────────────────────────────────────────────────┐  ║    │
│   ║   │ Division Pattern:                                              │  ║    │
│   ║   │ ^\s*(IDENTIFICATION|DATA|PROCEDURE|ENVIRONMENT)\s+DIVISION     │  ║    │
│   ║   │                                                                │  ║    │
│   ║   │ Section Pattern:                                               │  ║    │
│   ║   │ ^\s*(WORKING-STORAGE|FILE|LINKAGE|LOCAL-STORAGE)\s+SECTION     │  ║    │
│   ║   │                                                                │  ║    │
│   ║   │ Paragraph Pattern:                                             │  ║    │
│   ║   │ ^\s+([A-Z0-9-]+)\s*\.\s*$                                       │  ║    │
│   ║   │ (Starts with whitespace, alphanumeric-dash name, ends with .)  │  ║    │
│   ║   │                                                                │  ║    │
│   ║   │ COPY Pattern: COPY\s+([A-Z0-9-]+)                              │  ║    │
│   ║   │ CALL Pattern: CALL\s+['"]([A-Z0-9-]+)['"]                      │  ║    │
│   ║   └────────────────────────────────────────────────────────────────┘  ║    │
│   ║                                                                        ║    │
│   ║   Extracts:                                                            ║    │
│   ║   - All variables with types (CUST-ID PIC 9(8) → long)                ║    │
│   ║   - All paragraphs/sections with line ranges                          ║    │
│   ║   - All COPY statements (copybook references)                         ║    │
│   ║   - All CALL statements (external program calls)                      ║    │
│   ║                                                                        ║    │
│   ║   Stored in SQLite-backed registries:                                 ║    │
│   ║   - SignatureRegistry: method signatures                              ║    │
│   ║   - TypeMappingTable: variable→type mappings                          ║    │
│   ╚════════════════════════════════════════════════════════════════════════╝    │
│        │                                                                         │
│        ▼                                                                         │
│   ╔════════════════════════════════════════════════════════════════════════╗    │
│   ║ PHASE 2: CREATE CHUNKS (SemanticUnitChunker)                           ║    │
│   ║                                                                        ║    │
│   ║   Token Estimation: chars / 4 (CharsPerToken = 4)                     ║    │
│   ║                                                                        ║    │
│   ║   Priority Order for Chunking:                                        ║    │
│   ║   1. Semantic boundaries (divisions, sections, paragraphs)            ║    │
│   ║   2. Line-based fallback if single unit > MaxLinesPerChunk            ║    │
│   ║                                                                        ║    │
│   ║   Each chunk contains:                                                ║    │
│   ║   - ChunkIndex (0-based)                                              ║    │
│   ║   - StartLine, EndLine (1-based)                                      ║    │
│   ║   - Content (actual COBOL code)                                       ║    │
│   ║   - SemanticUnitNames (paragraphs/sections in this chunk)            ║    │
│   ║   - EstimatedTokens                                                   ║    │
│   ║   - HasOverlap (true if overlaps with previous chunk)                ║    │
│   ╚════════════════════════════════════════════════════════════════════════╝    │
│        │                                                                         │
│        ▼                                                                         │
│   ╔════════════════════════════════════════════════════════════════════════╗    │
│   ║ PHASE 3: PARALLEL CONVERSION (Rate-limited via SemaphoreSlim)          ║    │
│   ║                                                                        ║    │
│   ║   MaxParallelChunks: 3 workers (conversion is heavier than analysis)  ║    │
│   ║   TokenBudgetPerMinute: 300,000 (Azure OpenAI TPM)                    ║    │
│   ║   RateLimitSafetyFactor: 0.7 (use only 70% of budget)                ║    │
│   ║   StaggerDelay: 1000ms between chunk starts                           ║    │
│   ║                                                                        ║    │
│   ║   For each chunk:                                                      ║    │
│   ║   ┌──────────────────────────────────────────────────────────────┐   ║    │
│   ║   │ ChunkContextManager.BuildContextAsync() provides:             │   ║    │
│   ║   │                                                               │   ║    │
│   ║   │ 1. FileSummary - what this file does overall                 │   ║    │
│   ║   │ 2. PreviousSignatures - all methods converted so far         │   ║    │
│   ║   │ 3. TypeMappings - all variable→type mappings                 │   ║    │
│   ║   │ 4. PendingForwardReferences - refs to not-yet-converted code │   ║    │
│   ║   │ 5. CompressedHistory - summaries of older chunks             │   ║    │
│   ║   │                                                               │   ║    │
│   ║   │ Progressive Compression:                                      │   ║    │
│   ║   │ - Last 3 chunks: full detail (FullDetailChunkWindow=3)       │   ║    │
│   ║   │ - Older chunks: compressed to 30% (CompressionRatio=0.3)     │   ║    │
│   ║   └──────────────────────────────────────────────────────────────┘   ║    │
│   ║                                                                        ║    │
│   ║   AI Prompt includes:                                                 ║    │
│   ║   - ## VARIABLE TYPES (from TypeMappingTable)                        ║    │
│   ║   - ## ALREADY CONVERTED METHODS (from SignatureRegistry)            ║    │
│   ║   - ## CODE TO CONVERT (this chunk's COBOL)                          ║    │
│   ╚════════════════════════════════════════════════════════════════════════╝    │
│        │                                                                         │
│        ▼                                                                         │
│   ╔════════════════════════════════════════════════════════════════════════╗    │
│   ║ PHASE 4: VALIDATION & RECONCILIATION                                   ║    │
│   ║                                                                        ║    │
│   ║   SignatureRegistry.ValidateSignatureAsync():                         ║    │
│   ║   - Check if method signature already exists                          ║    │
│   ║   - Compare TargetMethodName, ReturnType, Parameters                  ║    │
│   ║   - Report discrepancies with severity (Error/Warning)                ║    │
│   ║                                                                        ║    │
│   ║   ReconcileChunks() validates:                                        ║    │
│   ║   ✓ All method signatures match across all chunks                    ║    │
│   ║   ✓ All variable types are consistent                                ║    │
│   ║   ✓ All cross-references are valid                                   ║    │
│   ║   ✓ No forward references remain unresolved                          ║    │
│   ╚════════════════════════════════════════════════════════════════════════╝    │
│        │                                                                         │
│        ▼                                                                         │
│   ╔════════════════════════════════════════════════════════════════════════╗    │
│   ║ PHASE 5: ASSEMBLY (Multi-file output)                                  ║    │
│   ║                                                                        ║    │
│   ║   For Java:                                                            ║    │
│   ║   - Detect multiple classes in converted code                         ║    │
│   ║   - Generate separate .java files per class                           ║    │
│   ║   - Add proper package and import statements                          ║    │
│   ║   - Add Quarkus annotations (@ApplicationScoped, @Transactional)     ║    │
│   ║                                                                        ║    │
│   ║   For C#:                                                              ║    │
│   ║   - Generate namespace structure                                       ║    │
│   ║   - Use partial classes if needed for large files                     ║    │
│   ║   - Add proper using statements                                       ║    │
│   ╚════════════════════════════════════════════════════════════════════════╝    │
│        │                                                                         │
│        ▼                                                                         │
│   Output: Multiple .java or .cs files + chunked-migration-report.md             │
└─────────────────────────────────────────────────────────────────────────────────┘
```

---

## Deep Dive: Consistency Mechanisms

### 1. SignatureRegistry (SQLite-Backed)

The `SignatureRegistry` ensures that method signatures are **immutable once registered**. This prevents the AI from generating different signatures for the same COBOL paragraph across chunks.

**Database Schema:**
```sql
CREATE TABLE signatures (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    run_id INTEGER NOT NULL,
    source_file TEXT NOT NULL,
    chunk_index INTEGER NOT NULL,
    legacy_name TEXT NOT NULL,           -- Original COBOL paragraph name
    target_method_name TEXT NOT NULL,    -- Java/C# method name
    target_signature TEXT NOT NULL,      -- Full signature string
    return_type TEXT NOT NULL,           -- Return type
    parameters TEXT,                     -- JSON array of parameters
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(run_id, source_file, legacy_name)
);
```

**Registration Flow:**
```csharp
// From SignatureRegistry.cs
public async Task<MethodSignature> RegisterSignatureAsync(...)
{
    // 1. Check if signature already exists
    var existing = await GetSignatureAsync(runId, sourceFile, signature.LegacyName, ...);
    if (existing != null)
    {
        // Return EXISTING signature - AI must use this
        return existing;
    }

    // 2. Insert new signature with ON CONFLICT DO NOTHING
    // This ensures first-writer wins in parallel scenarios
    await using var command = connection.CreateCommand();
    command.CommandText = @"
        INSERT INTO signatures (...) VALUES (...)
        ON CONFLICT(run_id, source_file, legacy_name) DO NOTHING;
        SELECT ... FROM signatures WHERE ...;";

    // 3. Return the registered (or existing) signature
}
```

**Validation Flow:**
```csharp
// SignatureValidationResult checks for discrepancies
public async Task<SignatureValidationResult> ValidateSignatureAsync(...)
{
    var existing = await GetSignatureAsync(...);
    if (existing == null) return new SignatureValidationResult { IsValid = true };

    // Check for discrepancies
    if (existing.TargetMethodName != signature.TargetMethodName)
    {
        result.Discrepancies.Add(new SignatureDiscrepancy
        {
            Field = "TargetMethodName",
            ExpectedValue = existing.TargetMethodName,
            ActualValue = signature.TargetMethodName,
            Severity = DiscrepancySeverity.Error
        });
    }
    // Also checks: ReturnType, Parameters
}
```

**Example:**
```
┌─────────────────────────────────────────────────────────────────────────────────┐
│ SIGNATURE REGISTRY IN ACTION                                                     │
│                                                                                  │
│   Chunk 1 converts VALIDATE-CUSTOMER:                                           │
│   → AI generates: public boolean validateCustomer(long customerId)              │
│   → RegisterSignatureAsync() stores in SQLite                                   │
│   → LOCKED: legacy_name="VALIDATE-CUSTOMER", target="validateCustomer"          │
│                                                                                  │
│   Chunk 5 references VALIDATE-CUSTOMER:                                         │
│   → ChunkContextManager includes: "validateCustomer(long customerId)"           │
│   → AI sees the registered signature and MUST use it                            │
│   → If AI generates different signature → ValidateSignatureAsync fails          │
│                                                                                  │
│   Result: 100% consistent method names and signatures across all chunks         │
└─────────────────────────────────────────────────────────────────────────────────┘
```

---

### 2. TypeMappingTable (SQLite-Backed)

The `TypeMappingTable` ensures consistent variable-to-type mappings across all chunks.

**Database Schema:**
```sql
CREATE TABLE type_mappings (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    run_id INTEGER NOT NULL,
    source_file TEXT NOT NULL,
    legacy_variable TEXT NOT NULL,    -- Original COBOL variable name
    legacy_type TEXT NOT NULL,        -- PIC clause (e.g., "PIC 9(8)")
    target_type TEXT NOT NULL,        -- Java/C# type (e.g., "long")
    target_field_name TEXT NOT NULL,  -- camelCase field name
    is_nullable INTEGER DEFAULT 0,
    default_value TEXT,
    UNIQUE(run_id, source_file, legacy_variable)
);
```

**Type Inference Logic (from TypeMappingTable.cs):**
```csharp
public string InferTargetType(string legacyType, TargetLanguage targetLanguage)
{
    // COBOL PIC clause patterns:
    // PIC 9(n)        → int/long based on digits
    // PIC S9(n)V9(m)  → decimal/BigDecimal (has decimal places)
    // PIC X(n)        → string/String (alphanumeric)
    // PIC A(n)        → string/String (alphabetic)
    // COMP/BINARY     → int

    var normalizedType = legacyType.ToUpperInvariant().Trim();

    if (pic.Contains('V') || pic.Contains('.'))
    {
        // Decimal type for values with decimal places
        return isCSharp ? "decimal" : "BigDecimal";
    }

    if (pic.Contains('9') && !pic.Contains('X'))
    {
        var digits = CountDigits(pic);
        if (digits <= 4) return "short";
        if (digits <= 9) return "int";
        if (digits <= 18) return "long";
        return isCSharp ? "decimal" : "BigDecimal";
    }

    // Default to string for X and A types
    return isCSharp ? "string" : "String";
}
```

**Example Mappings:**
```
┌─────────────────────────────────────────────────────────────────────────────────┐
│ TYPE MAPPING TABLE EXAMPLES                                                      │
│                                                                                  │
│   COBOL Variable        Legacy Type        →    Java/C# Type                    │
│   ─────────────────────────────────────────────────────────────                 │
│   CUST-ID               PIC 9(8)           →    long customerId                 │
│   CUST-NAME             PIC X(50)          →    String customerName             │
│   CUST-BAL              PIC S9(9)V99       →    BigDecimal customerBalance      │
│   ORDER-COUNT           PIC 9(4)           →    short orderCount                │
│   ACTIVE-FLAG           PIC X(1)           →    String activeFlag               │
│                                                                                  │
│   This table is:                                                                │
│   1. Pre-computed BEFORE any AI calls                                           │
│   2. LOCKED once registered                                                     │
│   3. Passed to EVERY chunk as context                                           │
│   4. AI cannot invent different types                                           │
└─────────────────────────────────────────────────────────────────────────────────┘
```

---

### 3. ChunkContextManager

The `ChunkContextManager` builds rich context for each chunk's AI prompt:

```csharp
// From ChunkContextManager.cs
public async Task<ChunkContext> BuildContextAsync(
    int runId,
    string sourceFile,
    int currentChunkIndex,
    IReadOnlyList<ChunkResult> previousResults,
    string? fileSummary = null,
    CancellationToken cancellationToken = default)
{
    var context = new ChunkContext
    {
        SourceFile = sourceFile,
        ChunkIndex = currentChunkIndex,
        FileSummary = fileSummary ?? "",

        // Get all registered signatures from SQLite
        PreviousSignatures = await _signatureRegistry
            .GetAllSignaturesAsync(runId, sourceFile, cancellationToken),

        // Get all type mappings from SQLite
        TypeMappings = await _typeMappingTable
            .GetAllMappingsAsync(runId, sourceFile, cancellationToken),

        // Identify forward references (calls to not-yet-converted code)
        PendingForwardReferences = IdentifyForwardReferences(
            previousResults, currentChunkIndex),

        // Compress older chunk summaries to save tokens
        CompressedHistory = CompressHistory(previousResults, currentChunkIndex)
    };

    return context;
}
```

**Progressive Compression:**
```
┌─────────────────────────────────────────────────────────────────────────────────┐
│ PROGRESSIVE COMPRESSION (FullDetailChunkWindow=3, CompressionRatio=0.3)          │
│                                                                                  │
│   Processing Chunk 7:                                                            │
│                                                                                  │
│   Chunk 0: "IDENTIFICATION DIVISION..." → Compressed to ~30% (summary only)     │
│   Chunk 1: "DATA DIVISION vars..."      → Compressed to ~30% (summary only)     │
│   Chunk 2: "PROCEDURE SECTION-A..."     → Compressed to ~30% (summary only)     │
│   Chunk 3: "SECTION-B processing..."    → Compressed to ~30% (summary only)     │
│   Chunk 4: [Full detail - within window]                                        │
│   Chunk 5: [Full detail - within window]                                        │
│   Chunk 6: [Full detail - within window]                                        │
│   Chunk 7: [CURRENT - being converted]                                          │
│                                                                                  │
│   This keeps token usage manageable while preserving recent context             │
└─────────────────────────────────────────────────────────────────────────────────┘
```

---

### 4. Forward Reference Resolution

When code in Chunk 3 calls a paragraph that will be converted in Chunk 7:

```
┌─────────────────────────────────────────────────────────────────────────────────┐
│ FORWARD REFERENCE HANDLING                                                       │
│                                                                                  │
│   Chunk 3 code: PERFORM CALCULATE-TOTALS                                        │
│   (CALCULATE-TOTALS is in Chunk 7, not yet converted)                           │
│                                                                                  │
│   1. ChunkContextManager detects this as a forward reference                    │
│   2. AI is told: "CALCULATE-TOTALS will be converted later"                    │
│   3. AI generates: // TODO: Call calculateTotals() when available              │
│                                                                                  │
│   After Chunk 7 completes:                                                      │
│   1. CALCULATE-TOTALS → calculateTotals(List<Order> orders)                    │
│   2. Registered in SignatureRegistry                                            │
│   3. Reconciliation phase resolves the forward reference                        │
│   4. Final code: calculateTotals(orders);                                       │
└─────────────────────────────────────────────────────────────────────────────────┘
```

---

### 5. No Truncation Policy

The system **NEVER silently drops code**:

```csharp
// If a chunk exceeds limits, the system FAILS with a clear error
if (chunkCharCount > _settings.AutoChunkCharThreshold)
{
    throw new InvalidOperationException(
        $"❌ CHUNK TOO LARGE: Chunk {chunkIndex} has {chunkCharCount:N0} chars " +
        $"(max: {_settings.AutoChunkCharThreshold:N0}).\n" +
        $"Suggested fix: Reduce MaxLinesPerChunk from {_settings.MaxLinesPerChunk} " +
        $"to {_settings.MaxLinesPerChunk * 80 / 100} in appsettings.json");
}
```

---

## Semantic Boundary Detection (CobolAdapter)

The `CobolAdapter` uses regex patterns to identify natural split points in COBOL code:

```csharp
// From CobolAdapter.cs
public class CobolAdapter : ILanguageAdapter
{
    // Division detection
    private static readonly Regex DivisionPattern = new(
        @"^\s*(IDENTIFICATION|DATA|PROCEDURE|ENVIRONMENT)\s+DIVISION",
        RegexOptions.IgnoreCase | RegexOptions.Multiline);

    // Section detection  
    private static readonly Regex SectionPattern = new(
        @"^\s*(WORKING-STORAGE|FILE|LINKAGE|LOCAL-STORAGE|COMMUNICATION|REPORT|SCREEN)\s+SECTION",
        RegexOptions.IgnoreCase | RegexOptions.Multiline);

    // Paragraph detection (name followed by period at start of line)
    private static readonly Regex ParagraphPattern = new(
        @"^\s+([A-Z0-9][A-Z0-9-]*)\s*\.\s*$",
        RegexOptions.Multiline);

    // External references
    private static readonly Regex CopyPattern = new(
        @"COPY\s+([A-Z0-9-]+)",
        RegexOptions.IgnoreCase);

    private static readonly Regex CallPattern = new(
        @"CALL\s+['""]([A-Z0-9-]+)['""]",
        RegexOptions.IgnoreCase);

    public SemanticUnit[] ExtractSemanticUnits(string content)
    {
        var units = new List<SemanticUnit>();
        var lines = content.Split('\n');

        // 1. Find all divisions
        foreach (Match match in DivisionPattern.Matches(content))
        {
            units.Add(new SemanticUnit
            {
                UnitType = SemanticUnitType.Division,
                Name = match.Groups[1].Value.ToUpper() + " DIVISION",
                StartLine = GetLineNumber(content, match.Index),
                // EndLine computed based on next unit
            });
        }

        // 2. Find all sections
        foreach (Match match in SectionPattern.Matches(content))
        {
            units.Add(new SemanticUnit
            {
                UnitType = SemanticUnitType.Section,
                Name = match.Groups[1].Value.ToUpper() + " SECTION",
                StartLine = GetLineNumber(content, match.Index),
            });
        }

        // 3. Find all paragraphs (only in PROCEDURE DIVISION)
        // ... paragraph extraction logic
    }
}
```

**Semantic Unit Priority:**
```
┌─────────────────────────────────────────────────────────────────────────────────┐
│ CHUNK SPLIT PRIORITY                                                             │
│                                                                                  │
│   1. DIVISION boundaries (strongest)                                            │
│      - IDENTIFICATION DIVISION                                                  │
│      - DATA DIVISION                                                            │
│      - PROCEDURE DIVISION                                                       │
│                                                                                  │
│   2. SECTION boundaries                                                         │
│      - WORKING-STORAGE SECTION                                                  │
│      - FILE SECTION                                                             │
│      - Named sections in PROCEDURE DIVISION                                     │
│                                                                                  │
│   3. PARAGRAPH boundaries                                                       │
│      - Named paragraphs (VALIDATE-CUSTOMER, PROCESS-ORDER, etc.)               │
│                                                                                  │
│   4. Line-based fallback                                                        │
│      - If a single unit exceeds MaxLinesPerChunk                               │
│      - Split at line boundaries with overlap                                    │
└─────────────────────────────────────────────────────────────────────────────────┘
```

---

## Database Schema (SQLite)

The chunking system uses SQLite for persistence across all runs:

```sql
-- Chunk processing metadata
CREATE TABLE chunk_metadata (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    run_id INTEGER NOT NULL,
    source_file TEXT NOT NULL,
    chunk_index INTEGER NOT NULL,
    start_line INTEGER NOT NULL,
    end_line INTEGER NOT NULL,
    status TEXT DEFAULT 'Pending',        -- Pending, Processing, Completed, Failed
    semantic_units TEXT,                  -- JSON array of unit names
    tokens_used INTEGER DEFAULT 0,
    processing_time_ms INTEGER DEFAULT 0,
    error_message TEXT,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    completed_at DATETIME,
    UNIQUE(run_id, source_file, chunk_index)
);

-- Method signatures (immutable after registration)
CREATE TABLE signatures (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    run_id INTEGER NOT NULL,
    source_file TEXT NOT NULL,
    chunk_index INTEGER NOT NULL,
    legacy_name TEXT NOT NULL,
    target_method_name TEXT NOT NULL,
    target_signature TEXT NOT NULL,
    return_type TEXT NOT NULL,
    parameters TEXT,                      -- JSON array
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(run_id, source_file, legacy_name)
);

-- Variable type mappings (immutable after registration)
CREATE TABLE type_mappings (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    run_id INTEGER NOT NULL,
    source_file TEXT NOT NULL,
    legacy_variable TEXT NOT NULL,
    legacy_type TEXT NOT NULL,
    target_type TEXT NOT NULL,
    target_field_name TEXT NOT NULL,
    is_nullable INTEGER DEFAULT 0,
    default_value TEXT,
    UNIQUE(run_id, source_file, legacy_variable)
);

-- Forward references (resolved after all chunks complete)
CREATE TABLE forward_references (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    run_id INTEGER NOT NULL,
    source_file TEXT NOT NULL,
    from_chunk INTEGER NOT NULL,
    to_chunk INTEGER,
    reference_name TEXT NOT NULL,
    reference_type TEXT NOT NULL,         -- Method, Variable
    resolved INTEGER DEFAULT 0,
    resolved_signature TEXT,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP
);
```

---

## Configuration Reference (appsettings.json)

```json
{
  "ChunkingSettings": {
    "AutoChunkCharThreshold": 150000,
    "AutoChunkLineThreshold": 3000,
    "MaxLinesPerChunk": 1500,
    "OverlapLines": 300,
    "MaxTokensPerChunk": 28000,
    "MaxParallelChunks": 6,
    "MaxParallelAnalysis": 6,
    "EnableParallelProcessing": true,
    "TokenBudgetPerMinute": 300000,
    "RateLimitSafetyFactor": 0.7,
    "CompressionRatio": 0.3,
    "FullDetailChunkWindow": 3
  },
  "RateLimitSettings": {
    "TokensPerMinute": 300000,
    "MinDelayBetweenRequestsMs": 2000
  }
}
```

---

## Error Handling & Troubleshooting

### Error Messages

| Error | Cause | Solution |
|-------|-------|----------|
| `❌ FILE TOO LARGE: file.cbl has 409,448 chars (max: 150,000)` | File exceeds single-call limit | System auto-routes to chunked processing |
| `❌ CHUNK TOO LARGE: Chunk X has Y chars (max: 150,000)` | Individual chunk still too big | Reduce `MaxLinesPerChunk` in appsettings.json |
| `HTTP 429 (RateLimitReached)` | Too many API calls per minute | Increase `MinDelayBetweenRequestsMs` or reduce `MaxParallelChunks` |
| `❌ Signature mismatch for VALIDATE-CUSTOMER` | AI generated different signature | Check SignatureRegistry, may need manual fix |

### Troubleshooting Steps

1. **File Too Large → Uses Chunking Automatically**
   ```bash
   # SmartMigrationOrchestrator auto-detects and routes
   dotnet run --source source
   ```

2. **Chunk Too Large Error**
   ```bash
   # Edit appsettings.json
   "MaxLinesPerChunk": 1000  # Reduce from 1500 to 1000
   "OverlapLines": 200        # Reduce proportionally
   ```

3. **Rate Limit (429) Error**
   ```bash
   # Edit appsettings.json
   "MinDelayBetweenRequestsMs": 5000  # Increase delay to 5 seconds
   "MaxParallelChunks": 2              # Reduce parallel workers
   ```

4. **Check Chunking Health**
   ```bash
   ./doctor.sh chunking-health
   ```

5. **Monitor Progress in Real-Time**
   ```bash
   # CLI
   ./helper-scripts/track-progress.sh --watch

   # Portal (Enhanced v0.3)
   open http://localhost:5028
   # • Live Activity dashboard now embedded in left panel
   # • View Target Language (Java ☕ / C# ⚙️) in run selector
   # • Real-time tracking of Active Workers and Phases
   # Click "🔄 Migration Monitor" for deep-dive chunk analysis
   ```

---

## Summary Tables

### File Size → Processing Path
| File Size | Processing | API Calls | Output |
|-----------|-----------|-----------|--------|
| **< 150K chars AND < 3K lines** | Direct (MigrationProcess) | 2-3 per file | Single file |
| **> 150K chars OR > 3K lines** | Chunked (ChunkedMigrationProcess) | 2 per chunk | Multiple files |

### Consistency Mechanisms
| Mechanism | Storage | Purpose |
|-----------|---------|---------|
| **SignatureRegistry** | SQLite `signatures` table | Lock method signatures after first conversion |
| **TypeMappingTable** | SQLite `type_mappings` table | Lock variable→type mappings |
| **ChunkContextManager** | In-memory + SQLite | Build context for each chunk's AI prompt |
| **Forward References** | SQLite `forward_references` table | Track calls to not-yet-converted code |
| **Reconciliation** | Post-processing | Validate all chunks are consistent |
| **No Truncation Policy** | Runtime check | Fail loudly if chunk too large |

### Parallel Processing Configuration
| Setting | Default | Purpose |
|---------|---------|---------|
| `MaxParallelAnalysis` | 6 | Workers for reverse engineering |
| `MaxParallelChunks` | 3 | Workers for code conversion |
| `TokenBudgetPerMinute` | 300,000 | Azure OpenAI TPM limit |
| `RateLimitSafetyFactor` | 0.7 | Use 70% of budget for safety margin |
| `StaggerDelay` | 500-1000ms | Delay between parallel chunk starts |
