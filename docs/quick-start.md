# Quick Start Guide

**Last updated**: 2026-05-20

This guide walks you through a complete COBOL-to-Java/C# conversion using the CLI.

## Prerequisites

| Requirement | Check |
|---|---|
| .NET 10 SDK | `dotnet --version` |
| Docker (for REKT scan) | `docker info` |
| Azure OpenAI or GitHub Copilot | `./doctor.sh setup` |
| COBOL source files | Drop `.cbl` and `.cpy` into `source/` |

## Step 1 — Setup

```bash
./doctor.sh setup
```

Follow the interactive prompts to configure your AI provider (Azure OpenAI or GitHub Copilot) and model deployments.

For **Azure OpenAI with Entra ID** (no API key needed):

```bash
az login
# doctor.sh setup → select Azure OpenAI → leave API key as ENTRA_ID
```

## Step 2 — REKT static analysis

Run the full REKT pipeline (parse → ingest → Neo4j graph):

```bash
./doctor.sh rekt-full
```

This parses all `.cbl` files in `source/`, generates AST/CFG/data-structure JSON under `output/rekt/`, and ingests everything into Neo4j. The Neo4j browser is available at `http://localhost:7475`.

> **Tip:** You only need to run this once per codebase. Re-run after adding or changing COBOL source files.

## Step 3 — Convert a single program

```bash
./doctor.sh convert-only --program ACCTMGR --target java --no-portal
```

| Flag | Description |
|---|---|
| `--program NAME` | Convert a specific program (repeatable) |
| `--target java` | Target language: `java` or `csharp` |
| `--no-portal` | Skip launching the web portal |

This stages the selected program + all copybooks, runs the AI conversion with REKT structural context injected, and writes the output to `output/java/` or `output/csharp/`.

## Step 4 — Convert multiple programs

Repeat `--program` to convert several programs in one run:

```bash
./doctor.sh convert-only --program ACCTMGR --program RPTGEN --target java --no-portal
```

## Selector flags

All selector flags can be combined. Same flag repeated = OR; different flags = AND.

```bash
# By transaction ID
./doctor.sh convert-only --transaction TRAN --target csharp

# By migration wave (requires target-architecture.json from Portal)
./doctor.sh convert-only --wave 1 --target java

# By keyword match in source
./doctor.sh convert-only --keyword SQLCA --target java

# Include programs called by the selection
./doctor.sh convert-only --program ACCTMGR --include-callees --target java

# Include programs that call into the selection
./doctor.sh convert-only --program ACCTMGR --include-callers --target java
```

## Full migration (RE + conversion)

To run reverse engineering first and then convert:

```bash
./doctor.sh run --program ACCTMGR --target java
```

To reuse previously cached RE results:

```bash
./doctor.sh convert-only --program ACCTMGR --target java
# When prompted "Reuse business logic from last RE run?", answer: y
```

## Quality and reliability flags

```bash
# Disable REKT structural context (pure-LLM, legacy mode)
./doctor.sh convert-only --program X --target java --no-rekt-context

# Force Copilot-safe mode (smaller chunks, sequential processing)
./doctor.sh convert-only --program X --target java --copilot-safe

# Set parity validation retries
./doctor.sh convert-only --program X --target java --max-validator-retries 3

# Set minimum quality score gate
./doctor.sh convert-only --program X --target java --min-program-score 0.75
```

## View results in the Portal

```bash
./doctor.sh portal
# Open http://localhost:5028
```

The portal provides:
- **Mission Control** — real-time run monitoring with colour-coded logs
- **AST Explorer** — browse the REKT-analysed program structure
- **C4 Dashboard** — system context, container, and component views
- **Convert modal** — select and convert programs from the UI

## Common workflows

### Smoke test a single program

```bash
./doctor.sh rekt-full                                        # once
./doctor.sh convert-only --program ACCTMGR --target java --no-portal
cat output/java/com/example/something/AcctmgrService.java    # review
```

### Batch convert a wave

```bash
./doctor.sh rekt-full                                        # once
./doctor.sh portal                                           # open Portal
# Portal → Target Architecture → assign programs to waves → 💾 Save
./doctor.sh convert-only --wave 1 --target java --no-portal
```

### Diagnose issues

```bash
./doctor.sh doctor    # check config, connectivity, model deployments
./doctor.sh test      # full system validation
```

## Environment variables

| Variable | Default | Description |
|---|---|---|
| `LLM_CALL_TIMEOUT_SECONDS` | `480` | Per-call hang timeout (seconds) |
| `ENABLE_REKT_CONTEXT` | `true` | Inject REKT AST/CFG/data into prompts |
| `COPILOT_SAFE_MODE` | auto | Force smaller chunks for Copilot provider |
| `COBOL_SOURCE_FOLDER` | `source` | Override source directory |
