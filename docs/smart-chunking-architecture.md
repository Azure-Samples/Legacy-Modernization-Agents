# Smart Chunking & Token Strategy Architecture

> **Last updated**: 2026-04-28  
> This document describes the token management and smart chunking architecture used by the Legacy Modernization Agents to process COBOL source files for migration to Java/C#.

---

## Mermaid Diagram

```mermaid
flowchart TD
    subgraph INPUT["📥 FILE INTAKE"]
        A[COBOL Source File] --> B{File Size Check}
        B -->|"≤ 3,000 lines<br>≤ 150,000 chars"| C[Single-File Processing]
        B -->|"> 3,000 lines<br>> 150,000 chars"| D[Smart Chunking Required]
    end

    subgraph TOKEN_EST["🔢 TOKEN ESTIMATION"]
        C --> E[TokenHelper.EstimateCobolTokens]
        D --> E
        E -->|"COBOL: chars ÷ 3.0"| F[Estimated Input Tokens]
        E -->|"General: chars ÷ 3.5"| F
    end

    subgraph COMPLEXITY["🎯 THREE-TIER COMPLEXITY SCORING"]
        F --> G[Complexity Score Calculation]
        G -->|"Σ regex×weight + density bonuses"| H{Score Threshold}
        H -->|"< 5"| I["🟢 LOW<br>effort: low<br>multiplier: 1.5×"]
        H -->|"5 – 14"| J["🟡 MEDIUM<br>effort: medium<br>multiplier: 2.5×"]
        H -->|"≥ 15"| K["🔴 HIGH<br>effort: high<br>multiplier: 3.5×"]
    end

    subgraph OUTPUT_CALC["📐 OUTPUT TOKEN CALCULATION"]
        I --> L[estimatedOutput = input × multiplier]
        J --> L
        K --> L
        L --> M["clamp(estimated, minTokens, maxTokens)"]
        M -->|"Codex: 32,768 – 100,000"| N[Final maxOutputTokens]
        M -->|"Chat: 16,384 – 65,536"| N
    end

    subgraph CHUNKING["✂️ SMART CHUNKING"]
        D --> O[CobolAdapter.IdentifySemanticUnits]
        O --> P[Divisions / Sections / Paragraphs]
        P --> Q[SemanticUnitChunker.ChunkFileAsync]
        Q --> R{Chunking Decision}
        R -->|"≤ MaxLinesPerChunk"| S[Single Chunk]
        R -->|"Semantic units found"| T["Semantic Boundary Split<br>Priority: DIVISION > SECTION > Paragraph"]
        R -->|"No units / oversized units"| U["Line-Based Fallback<br>overlap: 300 lines"]
    end

    subgraph CONTEXT["📋 CONTEXT WINDOW MANAGEMENT"]
        T --> V[ChunkContextManager]
        U --> V
        S --> V
        V --> W["Full Detail Window<br>(last 3 chunks)"]
        V --> X["Compressed History<br>(older → 30% size)"]
        V --> Y["Cross-Chunk State<br>signatures + type mappings"]
        W --> Z[ChunkContext]
        X --> Z
        Y --> Z
    end

    subgraph RATE_LIMIT["⏱️ DUAL RATE LIMITING"]
        direction TB
        Z --> AA["System A: RateLimiter<br>(Token Bucket + Semaphore)"]
        Z --> AB["System B: RateLimitTracker<br>(Sliding Window TPM/RPM)"]
        
        AA --> AC{Capacity Check}
        AB --> AC
        AC -->|"Budget: 300K TPM × 0.7"| AD[Wait / Proceed]
        AC -->|"Concurrency: max 3 parallel"| AD
        AC -->|"Stagger: 2,000ms between workers"| AD
    end

    subgraph API_CALL["🤖 API CALL + ESCALATION"]
        AD --> AE[Azure OpenAI Responses API]
        AE --> AF{Response Status}
        AF -->|"Complete"| AG[✅ Success]
        AF -->|"Reasoning Exhaustion<br>reasoning ≥ 90% of output"| AH["Escalation Loop<br>① Double maxTokens<br>② Promote effort<br>③ Thrash guard"]
        AH -->|"Max 2 retries"| AE
        AH -->|"All retries failed"| AI["Adaptive Re-Chunking<br>Split at semantic midpoint<br>50-line overlap"]
        AI --> AE
        AF -->|"429 Rate Limited"| AJ["Exponential Backoff<br>5s → 60s max<br>up to 5 retries"]
        AJ --> AE
    end

    subgraph RECONCILE["🔗 RECONCILIATION"]
        AG --> AK[Record Chunk Result]
        AK --> AL[Validate Chunk Output]
        AL --> AM{More Chunks?}
        AM -->|Yes| V
        AM -->|No| AN[Reconciliation Pass]
        AN --> AO["Merge Results<br>Resolve forward references<br>Deduplicate imports"]
    end

    subgraph FINAL["📤 FINAL OUTPUT"]
        AO --> AP[Converted Java/C# Code]
        AP --> AQ[Write to Output Directory]
    end

    classDef low fill:#d4edda,stroke:#28a745,color:#000
    classDef medium fill:#fff3cd,stroke:#ffc107,color:#000
    classDef high fill:#f8d7da,stroke:#dc3545,color:#000
    classDef process fill:#d1ecf1,stroke:#17a2b8,color:#000
    classDef rate fill:#e2d5f1,stroke:#6f42c1,color:#000

    class I low
    class J medium
    class K high
    class AA,AB,AC,AD rate
    class AE,AF,AG,AH,AI,AJ process
```

---

## ASCII Diagram

```
╔══════════════════════════════════════════════════════════════════════════════════════╗
║                        SMART CHUNKING & TOKEN STRATEGY                             ║
║                        Legacy Modernization Agents                                 ║
╚══════════════════════════════════════════════════════════════════════════════════════╝

┌─────────────────────────────────────────────────────────────────────────────────────┐
│  📥 FILE INTAKE                                                                    │
│                                                                                     │
│  COBOL Source File ──► File Size Check                                              │
│                          │                                                          │
│              ┌───────────┴───────────┐                                              │
│              │                       │                                              │
│           ≤ 3,000 lines           > 3,000 lines                                     │
│           ≤ 150,000 chars         > 150,000 chars                                   │
│              │                       │                                              │
│         Single-File             Smart Chunking                                      │
│         Processing              Required ──────────────────────────────────┐        │
│              │                                                             │        │
└──────────────┼─────────────────────────────────────────────────────────────┼────────┘
               │                                                             │
               ▼                                                             ▼
┌─────────────────────────────────────┐    ┌──────────────────────────────────────────┐
│  🔢 TOKEN ESTIMATION               │    │  ✂️  SMART CHUNKING                       │
│                                     │    │                                          │
│  TokenHelper.EstimateCobolTokens    │    │  CobolAdapter                            │
│  ┌─────────────────────────────┐    │    │    ├── IdentifySemanticUnits             │
│  │ COBOL:   chars ÷ 3.0       │    │    │    ├── ExtractVariables                  │
│  │ General: chars ÷ 3.5       │    │    │    ├── ExtractCallDependencies           │
│  │ Safety:  0.5 (truncation)  │    │    │    └── ExtractExternalReferences         │
│  └─────────────────────────────┘    │    │                │                         │
│              │                      │    │                ▼                         │
│    Estimated Input Tokens           │    │  SemanticUnitChunker.ChunkFileAsync      │
│              │                      │    │         │                                │
└──────────────┼──────────────────────┘    │    ┌────┴────┬────────────┐              │
               │                           │    │         │            │              │
               ▼                           │  ≤ Max    Semantic     No units/         │
┌─────────────────────────────────────┐    │  Lines    Units OK     Oversized         │
│  🎯 THREE-TIER COMPLEXITY SCORING   │    │    │         │            │              │
│                                     │    │  Single   Semantic    Line-Based         │
│  Score = Σ(regex × weight)          │    │  Chunk    Boundary    Fallback           │
│        + PIC density bonus (+3)     │    │    │      Split         │               │
│        + Level density bonus (+2)   │    │    │         │       overlap:            │
│        + COPY near WS/LINKAGE (+3)  │    │    │         │       300 lines           │
│        + EXEC SQL/DLI bonus (+4)    │    │    │         │            │              │
│                                     │    │    └────┬────┘            │              │
│  ┌──────────────────────────────┐   │    │         │                 │              │
│  │ Weighted Indicators:        │   │    │         ▼                 │              │
│  │  EXEC SQL ......... wt 3    │   │    │  ┌─────────────────┐     │              │
│  │  EXEC CICS ........ wt 4    │   │    │  │ Split Priority: │     │              │
│  │  EXEC DLI ......... wt 4    │   │    │  │ 1. DIVISION     │     │              │
│  │  PERFORM VARYING .. wt 2    │   │    │  │ 2. SECTION      │     │              │
│  │  EVALUATE TRUE .... wt 2    │   │    │  │ 3. Paragraph    │     │              │
│  │  SEARCH ALL ....... wt 2    │   │    │  │ 4. Any unit     │     │              │
│  │  REDEFINES ........ wt 2    │   │    │  └─────────────────┘     │              │
│  │  OCCURS DEPENDING . wt 3    │   │    │         │                 │              │
│  │  UNSTRING ......... wt 2    │   │    └─────────┼─────────────────┼──────────────┘
│  │  ALTER ............ wt 3    │   │              │                 │
│  │  GO TO DEPENDING .. wt 3    │   │              ▼                 ▼
│  │  CALL ............. wt 2    │   │  ┌──────────────────────────────────────────────┐
│  └──────────────────────────────┘   │  │  📋 CONTEXT WINDOW MANAGEMENT               │
│                                     │  │                                              │
│     Score ──► Tier Assignment       │  │  ChunkContextManager (SQLite-backed)         │
│      │                              │  │                                              │
│  ┌───┴──────┬──────────┐            │  │  ┌──────────────────────────────────────┐    │
│  │          │          │            │  │  │ Full Detail Window (last 3 chunks)   │    │
│  ▼          ▼          ▼            │  │  │  → Complete semantic unit listing    │    │
│ ┌────┐  ┌──────┐  ┌──────┐         │  │  ├──────────────────────────────────────┤    │
│ │LOW │  │MEDIUM│  │ HIGH │         │  │  │ Compressed History (older chunks)    │    │
│ │< 5 │  │ 5-14 │  │ ≥ 15 │         │  │  │  → 30% compression ratio            │    │
│ │1.5×│  │ 2.5× │  │ 3.5× │         │  │  │  → 1-line summary per chunk         │    │
│ │low │  │medium│  │ high │         │  │  ├──────────────────────────────────────┤    │
│ └──┬─┘  └──┬───┘  └──┬───┘         │  │  │ Cross-Chunk State                   │    │
│    │        │         │             │  │  │  → Method signatures (legacy→target) │    │
│    └────────┼─────────┘             │  │  │  → Type mappings (COBOL→Java/C#)    │    │
│             │                       │  │  │  → Forward reference tracking        │    │
└─────────────┼───────────────────────┘  │  └──────────────────────────────────────┘    │
              │                          │              │                                │
              ▼                          │              │  Output: ChunkContext           │
┌─────────────────────────────────────┐  └──────────────┼────────────────────────────────┘
│  📐 OUTPUT TOKEN CALCULATION        │                 │
│                                     │                 │
│  estimated = inputTokens × mult     │                 │
│                                     │                 │
│  ┌──────────────────────────────┐   │                 │
│  │ Codex Profile:              │   │                 │
│  │   min: 32,768  max: 100,000 │   │                 │
│  │   timeout: 900s (15 min)    │   │                 │
│  ├──────────────────────────────┤   │                 │
│  │ Chat Profile:               │   │                 │
│  │   min: 16,384  max: 65,536  │   │                 │
│  │   timeout: 600s (10 min)    │   │                 │
│  └──────────────────────────────┘   │                 │
│                                     │                 │
│  Floor Gate:                        │                 │
│    IF score ≥ 5 OR input ≥ min/2:   │                 │
│      enforce MinOutputTokens floor  │                 │
│    ELSE:                            │                 │
│      floor = max(4096, estimated)   │                 │
│                                     │                 │
│  Final = clamp(est, floor, max)     │                 │
│              │                      │                 │
└──────────────┼──────────────────────┘                 │
               │                                        │
               ▼                                        ▼
┌─────────────────────────────────────────────────────────────────────────────────────┐
│  ⏱️  DUAL RATE LIMITING                                                            │
│                                                                                     │
│  ┌─────────────────────────────────┐  ┌─────────────────────────────────────────┐   │
│  │ System A: RateLimiter           │  │ System B: RateLimitTracker              │   │
│  │ (Token Bucket + Semaphore)      │  │ (Sliding Window TPM/RPM)               │   │
│  │                                 │  │                                         │   │
│  │ Budget: 300K TPM × 0.7 = 210K  │  │ Budget: 500K TPM × 0.9 = 450K          │   │
│  │ Concurrency: max 3 parallel    │  │ RPM: 1,000 × 0.9 = 900                 │   │
│  │ Per-worker delay: ≥ 1,000ms    │  │ Window: 60-second sliding               │   │
│  │ Minute-boundary reset          │  │ Queue-based entry pruning               │   │
│  │                                 │  │                                         │   │
│  │ Flow:                           │  │ Flow:                                   │   │
│  │  1. Acquire semaphore           │  │  1. Prune entries > 60s old             │   │
│  │  2. Lock token bucket           │  │  2. Check: current + est > TPM?         │   │
│  │  3. Check minute counter        │  │  3. Check: requests + 1 > RPM?          │   │
│  │  4. Check last-request delay    │  │  4. Wait for oldest entry to expire     │   │
│  │  5. Check budget allows tokens  │  │  5. RecordUsage(actualTokens) after     │   │
│  │  6. Wait or proceed             │  │                                         │   │
│  └─────────────────────────────────┘  └─────────────────────────────────────────┘   │
│                                                                                     │
│  Speed Profiles:                                                                    │
│  ┌──────────┬───────────┬────────────┬─────────────┬────────────┬───────────┐       │
│  │ Profile  │ Reasoning │ MaxOutput  │ Par.Workers │ Stagger    │ Safety    │       │
│  ├──────────┼───────────┼────────────┼─────────────┼────────────┼───────────┤       │
│  │ TURBO    │ low (all) │ 65,536     │ 4           │ 200ms      │ 0.85      │       │
│  │ FAST     │ low/med   │ 32,768     │ 3           │ 500ms      │ —         │       │
│  │ BALANCED │ adaptive  │ 100,000    │ 2           │ 1,000ms    │ 0.70      │       │
│  │ THOROUGH │ med/high  │ 100,000    │ 2           │ 1,500ms    │ —         │       │
│  └──────────┴───────────┴────────────┴─────────────┴────────────┴───────────┘       │
│              │                                                                      │
└──────────────┼──────────────────────────────────────────────────────────────────────┘
               │
               ▼
┌─────────────────────────────────────────────────────────────────────────────────────┐
│  🤖 API CALL + ESCALATION                                                          │
│                                                                                     │
│  ┌──────────────────────────────────────────────────────────────┐                   │
│  │                Azure OpenAI Responses API                    │                   │
│  │                (gpt-5.1-codex-mini)                          │                   │
│  └──────────────────────────┬───────────────────────────────────┘                   │
│                             │                                                       │
│                    ┌────────┴────────┬──────────────────┐                           │
│                    │                 │                  │                            │
│                    ▼                 ▼                  ▼                            │
│               ✅ Complete    Reasoning Exhaustion   429 Rate Limited                │
│                    │         (reason ≥ 90% output)      │                           │
│                    │              │                      │                           │
│                    │              ▼                      ▼                           │
│                    │     ┌─────────────────┐    ┌─────────────────┐                 │
│                    │     │ Escalation Loop │    │ Exp. Backoff    │                 │
│                    │     │                 │    │ 5s → 60s max    │                 │
│                    │     │ ① 2× tokens    │    │ up to 5 retries │                 │
│                    │     │ ② ↑ effort     │    └────────┬────────┘                 │
│                    │     │    low→med→high │             │                           │
│                    │     │ ③ thrash guard  │             │                           │
│                    │     │ max 2 retries   │        ┌────┘                          │
│                    │     └───────┬─────────┘        │                               │
│                    │             │                   │                               │
│                    │        ┌────┴────┐              │                               │
│                    │     Success?  All failed        │                               │
│                    │        │         │              │                               │
│                    │        │         ▼              │                               │
│                    │        │  ┌──────────────┐      │                               │
│                    │        │  │ Adaptive     │      │                               │
│                    │        │  │ Re-Chunking  │      │                               │
│                    │        │  │              │      │                               │
│                    │        │  │ Split at     │      │                               │
│                    │        │  │ semantic     │      │                               │
│                    │        │  │ midpoint     │      │                               │
│                    │        │  │ 50-line      │      │                               │
│                    │        │  │ overlap      │      │                               │
│                    │        │  └──────┬───────┘      │                               │
│                    │        │         │              │                               │
│                    ◄────────┴─────────┴──────────────┘                              │
│                    │                                                                 │
└────────────────────┼────────────────────────────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────────────────────────────────┐
│  🔗 RECONCILIATION & OUTPUT                                                        │
│                                                                                     │
│  ┌────────────────────────────────────────────────────────────────┐                 │
│  │ Per-Chunk Pipeline:                                           │                 │
│  │   Record Result → Validate Chunk → Store in SQLite            │                 │
│  │     └─ signatures, type_mappings, forward_references          │                 │
│  └────────────────────────────────────────────────────────────────┘                 │
│                             │                                                       │
│                    More chunks? ──Yes──► Back to Context Management                 │
│                             │                                                       │
│                            No                                                       │
│                             │                                                       │
│                             ▼                                                       │
│  ┌────────────────────────────────────────────────────────────────┐                 │
│  │ Reconciliation Pass:                                          │                 │
│  │   ├── Merge all chunk outputs                                 │                 │
│  │   ├── Resolve forward references                              │                 │
│  │   ├── Deduplicate imports/package declarations                │                 │
│  │   └── Final validation                                        │                 │
│  └────────────────────────────────────────────────────────────────┘                 │
│                             │                                                       │
│                             ▼                                                       │
│  ┌────────────────────────────────────────────────────────────────┐                 │
│  │ 📤 FINAL OUTPUT                                               │                 │
│  │   Converted Java/C# Code → output/ directory                  │                 │
│  └────────────────────────────────────────────────────────────────┘                 │
│                                                                                     │
└─────────────────────────────────────────────────────────────────────────────────────┘
```

---

## Key Constants Reference

| Constant | Value | Source |
|---|---|---|
| COBOL chars/token | 3.0 | `TokenHelper.cs` |
| General chars/token | 3.5 | `TokenHelper.cs` |
| Safety margin (truncation) | 0.5 | `TokenHelper.cs` |
| Auto-chunk char threshold | 150,000 | `appsettings.json` |
| Auto-chunk line threshold | 3,000 | `appsettings.json` |
| Max tokens per chunk | 28,000 | `appsettings.json` |
| Max lines per chunk | 1,500 | `appsettings.json` |
| Overlap lines | 300 | `appsettings.json` |
| Min semantic unit size | 50 lines | `appsettings.json` |
| Full detail chunk window | 3 chunks | `appsettings.json` |
| Compression ratio | 0.3 (30%) | `appsettings.json` |
| Max parallel chunks | 6 | `appsettings.json` |
| Parallel stagger delay | 2,000ms | `appsettings.json` |
| Token budget per minute | 300,000 | `appsettings.json` |
| Rate limit safety factor | 0.7 | `appsettings.json` |
| LOW threshold | score < 5 | `appsettings.json` |
| MEDIUM threshold | score ≥ 5 | `appsettings.json` |
| HIGH threshold | score ≥ 15 | `appsettings.json` |
| LOW multiplier | 1.5× | `appsettings.json` |
| MEDIUM multiplier | 2.5× | `appsettings.json` |
| HIGH multiplier | 3.5× | `appsettings.json` |
| Codex min output tokens | 32,768 | `appsettings.json` |
| Codex max output tokens | 100,000 | `appsettings.json` |
| Codex timeout | 900s (15 min) | `appsettings.json` |
| Escalation retries (Codex) | 2 | `appsettings.json` |
| Escalation multiplier (Codex) | 2.0× | `appsettings.json` |
| Re-chunk overlap | 50 lines | `AgentBase.cs` |
| Backoff base delay | 5,000ms | `RateLimiter.cs` |
| Backoff max delay | 60,000ms | `RateLimiter.cs` |
| Max 429 retries | 5 | `RateLimiter.cs` |

---

## Complexity Scoring Indicators

| COBOL Pattern | Weight | Notes |
|---|---|---|
| `EXEC SQL` | 3 | Embedded SQL |
| `EXEC CICS` | 4 | CICS transactions |
| `EXEC DLI` | 4 | IMS/DB2 |
| `PERFORM VARYING` | 2 | Loop with counter |
| `PERFORM UNTIL` | 1 | Conditional loop |
| `EVALUATE TRUE` | 2 | Switch/case equivalent |
| `SEARCH ALL` | 2 | Binary search |
| `REDEFINES` | 2 | Memory overlay |
| `OCCURS n DEPENDING` | 3 | Variable-length arrays |
| `OCCURS n` | 1 | Fixed arrays |
| `COMPUTE` | 1 | Arithmetic expression |
| `INSPECT` | 1 | String inspection |
| `STRING` | 1 | String concatenation |
| `UNSTRING` | 2 | String parsing |
| `CALL 'program'` | 2 | External program call |
| `ALTER` | 3 | Dynamic GO TO (legacy) |
| `GO TO DEPENDING` | 3 | Computed GO TO |
| `COPY` | 1 | Copybook include |
| `REPLACE` | 2 | Text substitution |

**Bonus scores:**
- PIC density > 25%: +3
- Level number density > 30%: +2
- COPY near WORKING-STORAGE/LINKAGE: +3
- EXEC SQL/DLI present: +4
