# REKT-grounded conversion

**Last updated**: 2026-05-19

This document describes the structural-context pipeline introduced to make code conversion (`./doctor.sh run`) deterministic and high-quality by grounding every prompt in the static-analysis output of `./doctor.sh rekt-full`.

## Why

The original conversion pipeline sends raw COBOL source to an LLM and asks it to figure out the program's structure on the fly. This forces the model to re-discover sections, paragraphs, perform graphs, copybook layouts, SQL statements, and CALL targets that the static analyser already knows. Results: hallucinated method names, missing fields, swallowed CALL chains.

REKT-grounded conversion injects the **authoritative structural context** into every converter prompt and adds a deterministic parity-check that flags missing translations. The converter LLM gets the conversion blueprint; the human gets a measurable quality score.

## What gets injected

When `ENABLE_REKT_CONTEXT=true` (set automatically when you start a conversion from the portal modal, or by passing the right `doctor.sh` flag), every per-program conversion prompt includes a block like:

```
REKT STRUCTURAL CONTEXT (authoritative — use this as the conversion blueprint):

STRUCTURAL CONTEXT (program: CRECUST.cbl)
SOURCE: RektNative    confidence: 0.95

TARGET COMPONENT: Business Logic Services (Service Layer)
TARGET TECH: Java Spring Boot / .NET 8
STRATEGY: rearchitect    WAVE: 3    COMPLEXITY: 0.62
PATTERNS: Domain-Driven Design, Hexagonal architecture
MIGRATION NOTES:
  - Convert 18 EXEC SQL statements to repository methods (JPA / EF Core).
  - Replace 12 CALL statements with synchronous service-to-service calls.

SECTIONS (8):
  A-INIT (lines 80-120)  paragraphs: 4
    - A-OPEN-FILES (lines 81-90)
    - A-INIT-COUNTERS (lines 91-100)
    ...
  B-MAIN-LOOP (lines 121-340)  paragraphs: 12
    ...

PERFORM GRAPH (32 edges):
  B-MAIN-LOOP → B-READ
  B-MAIN-LOOP → B-PROCESS (conditional)
  ...

PROGRAM CALLS (3):
  → CUSTSVC (line 215)
  → ACCTSVC (line 230)
  → AUDITSVC (line 244)

EXEC SQL (18):
  SELECT CUSTOMER (line 230)
  UPDATE CUSTOMER (line 241)
  ...

COPYBOOKS USED: CUSTREC, ACCTREC, AUDLOG

DATA STRUCTURE (2 top-level groups):
  01 CUSTOMER-RECORD
    03 CUST-ID PIC X(10)
    03 CUST-NAME PIC X(40)
    ...
```

Every converter (Java + C#) reads this block before generating the target code. The LLM uses the section list as its method inventory, the data structure as the DTO blueprint, and the migration notes as the strategy guardrail.

## Confidence ladder (provenance)

`StructuralContextProvider` chooses the best available source and tags the result:

| Provenance | Source | Confidence | Behaviour |
|---|---|---|---|
| `RektNative` | Full `flow-ast-*.json` + `flow-cfg-*.json` + `flow-data-*.json` | 0.95 | Best case — converter trusts everything |
| `RektPartial` | Only `<program>-deps.json` (smojol AST writer failed) | 0.55 | Call graph + copybook usage are reliable; sections/data are missing — converter falls back to source for those |
| `LlmExtracted` | `output/rekt/llm-derived/<program>.json` produced by `StructuralExtractorAgent` | 0.45 | Available only when `--fallback-to-ai` is set. Caller treats output as a hypothesis to verify |
| `None` | No structural context at all | 0.10 | Converter operates on raw source; we still inject the target plan if `target-architecture.json` has one |

Non-COBOL artefacts go through deterministic readers:

| Source kind | Reader | Notes |
|---|---|---|
| `.cbl` / `.cpy` | smojol (REKT) | The default. Goes through `cobol-rekt` Docker container. |
| `.bms` | `Helpers/BmsReader.cs` | DFHMSD / DFHMDI / DFHMDF macros. Each map becomes a section; each field becomes a level-05 data item. Suitable for CICS screen → web UI mapping. |
| `.dbd` | `Helpers/ImsDbdReader.cs` | DBDGEN: SEGM + FIELD with key flags, byte lengths, types. PIC mapping: `C`→`PIC X(n)`, `P`→`PIC S9(2n-1) COMP-3`. |
| `.psb` | `Helpers/ImsPsbReader.cs` | PSBGEN: PCB + SENSEG. Lets `--transaction` resolution walk PSB → DB segments. |

## Quality validation (Phase 2 agents)

After each program is converted, an opt-in pipeline runs:

### ConversionParityAgent

Deterministic + repair pass. Compares REKT context to converted code on four axes:

- **Sections → methods**: every COBOL section name should map to a method (camelCase or PascalCase) in the target.
- **Copybook fields → DTO fields**: every level-03+ field should be a property.
- **CALL → service-call**: every `CALL 'X'` should produce an invocation referencing target program `X`.
- **SQL → repository**: every `EXEC SQL` should have an Entity/Repository touchpoint for the table.

Score is the weighted average (40% sections / 25% fields / 20% calls / 15% SQL). Gaps trigger up to `MAX_VALIDATOR_RETRIES` (default 1) LLM repair passes. If the final score is below `MIN_PROGRAM_SCORE` (default 0 = off) the run either continues with a low-confidence flag or stops, controlled by `ON_LOW_SCORE` (`continue` | `stop`, default `continue`).

### CodeReviewerAgent

Idiomatic-code reviewer. Returns structured findings:

```json
{
  "score": 0.85,
  "findings": [
    { "severity": "warning", "line": 124, "rule": "DI_FIELD_INJECTION",
      "message": "Field-level @Autowired — prefer constructor injection",
      "suggestion": "Inject CustomerRepository via constructor" }
  ],
  "summary": "..."
}
```

Checklist covers naming, DI patterns, annotations, logging (SLF4J / ILogger), exception handling, null safety, concurrency, parameterised SQL.

### DataMappingAgent

For SQL-heavy programs only. Generates entity + repository classes with deterministic PIC → target-type mapping (`PIC S9(15)V99 COMP-3` → `BigDecimal` / `decimal`). Composite keys, REDEFINES handling, OCCURS arrays all documented in `notes`.

## Test synthesis (Phase 3 agents)

### TestSynthesizerAgent

JUnit 5 / xUnit tests per converted class. One happy-path test per top-level method, one boundary test per CFG branch hint, one DB test per repository method (H2 / EF Core InMemory).

### RegressionFixtureAgent

Pure-deterministic generator (no LLM). Walks the REKT data structure and emits happy-path + edge-case JSON fixtures suitable for differential testing between the original COBOL and the converted code.

## Reporting (Phase 4 agents)

### MigrationSummaryAgent

Per-program stakeholder summary with sections **What we converted**, **What we deferred**, **What we couldn't**, **Risk score & next steps**, **Where human input is needed**. Includes a portfolio aggregator that sorts programs by risk.

### DocumentationAgent

Adds JavaDoc / XML-doc that references the COBOL origin:

```java
/**
 * Processes a single customer order row.
 *
 * <p>Maps COBOL SECTION <code>B-MAIN-LOOP</code> (lines 121-340).
 * Replaces <code>EXEC SQL UPDATE CUSTOMER</code> with a repository call.
 *
 * @param order  the order to process
 * @return  the post-processing status
 */
```

## Selectors (CLI)

Run a focused conversion instead of "everything in source/":

```bash
# Convert just one program
./doctor.sh --program CRECUST run

# Convert a CICS transaction and everything it calls
./doctor.sh --transaction CT01 --include-callees run

# Convert all Wave 1 programs targeting the data layer
./doctor.sh --wave 1 --target svc-data run

# Convert with AI fallback when REKT can't parse + strict quality gate
./doctor.sh --program CRECUST \
            --fallback-to-ai \
            --max-validator-retries 2 \
            --min-program-score 0.75 \
            --on-low-score stop \
            run
```

**Combine logic**: same flag repeated = OR within that flag (e.g. `--program A --program B` = A or B). Different flags = AND (e.g. `--program A --wave 1` = A only if it's wave 1).

## Selectors (Portal)

Top-right of the dashboard has a **🛠️ Convert…** button. Opens a shared modal with all the selectors. The Target Architecture tab also has an inline **🛠️ Convert…** button that pre-fills the modal with the currently-selected component (if any).

Live preview while you edit selectors; one-click start. Triggers `POST /api/runs/convert`, which uses the same selector logic as the CLI.

## Environment variables (full list)

| Variable | Used by | Default | Meaning |
|---|---|---|---|
| `ENABLE_REKT_CONTEXT` | JavaConverterAgent, CSharpConverterAgent | unset | When `true`, inject REKT block into every conversion prompt |
| `COBOL_SOURCE_FOLDER` | All file scanners | `source` | Source folder relative to repo root |
| `STRUCTURAL_FALLBACK_TO_AI` | StructuralContextProvider | unset | When `true`, call StructuralExtractorAgent on REKT miss |
| `MAX_VALIDATOR_RETRIES` | ConversionParityAgent | `1` | Max LLM repair passes per program |
| `MIN_PROGRAM_SCORE` | ConversionParityAgent | `0` (off) | Min per-program parity score |
| `ON_LOW_SCORE` | ConversionParityAgent | `continue` | `continue` (mark low-confidence) or `stop` (fail the run) |

## On-disk artefacts

| Path | Producer | Consumer |
|---|---|---|
| `output/rekt/flow-ast-*.json` | smojol | RektContextLoader |
| `output/rekt/flow-cfg-*.json` | smojol | RektContextLoader (future) |
| `output/rekt/flow-data-*.json` | smojol | RektContextLoader |
| `output/rekt/<program>-deps.json` | smojol | RektContextLoader |
| `output/rekt/target-architecture.json` | Portal → 💾 Save for AI agent | RektContextLoader, ProgramSelectorService |
| `output/rekt/missing-copybooks.txt` | doctor.sh (`rekt-full` pre-parse step) | User (informational) |
| `output/rekt/llm-derived/<program>.json` | StructuralExtractorAgent | StructuralContextProvider (cache) |
| `output/fixtures/<program>/*.json` | RegressionFixtureAgent | Test runner (manual today) |

## Why this design

- **One context shape**: every consumer (converter, parity validator, code reviewer, test synthesizer, data mapper, doc agent, summary agent) sees the same `StructuralContext` type. Adding a new agent doesn't need to learn five JSON shapes.
- **Provenance, not exceptions**: when REKT can't parse, the provider still returns a usable context with a low confidence tag. Downstream code adjusts strictness instead of throwing.
- **Opt-in everywhere**: REKT injection (`ENABLE_REKT_CONTEXT`), AI fallback (`STRUCTURAL_FALLBACK_TO_AI`), quality gates (`MIN_PROGRAM_SCORE`) all default off so existing pipelines behave identically. New users get the value gradually.
- **CLI + Portal parity**: every selector and quality flag is available both from `doctor.sh` and from the portal modal. Power users automate via CLI, exploratory users drive via portal.
