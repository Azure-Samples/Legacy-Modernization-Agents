# Quick Guide — 5 minutes from zero to a converted program with full intelligence portal

**Last updated**: 2026-05-31

This guide gets you from a fresh clone to your first converted COBOL program **and** an operational modernization intelligence portal in five minutes of wall-clock time (plus the LLM call itself).

For deep-dive references, see the other files in [`docs/`](.).

---

## TL;DR — three commands

```bash
# 1. Configure provider (interactive: Azure OpenAI / GitHub Copilot)
./doctor.sh setup

# 2. Static analysis: parse every .cbl into AST/CFG/data graphs + Neo4j + portal
./doctor.sh rekt-full

# 3. Convert one program (REKT facts auto-injected into the LLM prompt)
./doctor.sh convert-only --program BDSM043 --target java
```

Converted Java lands in `output/runs/{runId}-java-…/com/example/…/` (per-run isolated folder — never overwrites). Conversion telemetry streams to `output/.metrics/<runId>.jsonl`. The portal stays running on <http://localhost:5028>.

---

## What just happened

```mermaid
flowchart LR
    A[source/**/*.cbl<br/>and *.cpy<br/>recursive] -->|rekt-full| B[REKT parser<br/>smojol]
    B --> C[output/rekt/<br/>AST + CFG + data-flow JSON]
    B --> D[Neo4j graph<br/>:7475]
    C -->|projection| E[output/rekt/<br/>*.facts.json]
    E -->|convert-only<br/>+ REKT context injection| F[Converter Agent<br/>Java or C#]
    F --> G[output/runs/{runId}-…/<br/>per-run isolated]
    F --> H[output/.metrics/<br/>runId.jsonl]
    G --> I[Portal :5028]
    H --> I
    D --> I
    I --> J[🎨 Visual Cockpit<br/>🧭 Modernization Intelligence<br/>🎯 Insights Hub<br/>🌌 AST Galaxy]
```

- `rekt-full` runs once per source change. It **recursively** scans `source/**` (subfolders supported — verified for `FUENTES/SRC/`, `FUENTES/TRX/`, etc.), parses every program, ingests into Neo4j, and writes per-program facts.
- Each subsequent `convert-only` reuses those facts. A converter agent receives the **program-facts projection** (a 60–90 % smaller prompt than raw AST), routes through the appropriate single-shot or chunked path, and emits Java/C# plus structured telemetry — in a brand-new isolated `output/runs/{runId}-…/` folder so you never lose history.
- The portal reads the same graph + metrics for four persona-driven surfaces (cockpit / intelligence / insights / AST galaxy).

---

## 🎨 The four portal surfaces (added through Phase-1 → Phase-3)

| Surface | Audience | What it answers | URL fragment |
|---|---|---|---|
| 🎨 **Visual Cockpit** | Anyone | "Where is the program at right now?" — 5 persona dashboards (Mission Control · Business Owner · Architect · Modernization Lead · Developer) with SVG gauges, heatmaps, Kanban, scorecards. **Live auto-refresh.** | top tab `🎨 Visual Cockpit` |
| 🧭 **Modernization Intelligence** | Engineers / analysts | "Show me the data" — 10 read-only subviews including Dependency Health, Service Chain (JCL→Pgm→Cpy), Migration Wave Planner, Service Candidates, **🎯 Capabilities & Service Locator** | top tab `🧭 Modernization Intelligence` |
| 🎯 **Insights Hub** | Decision makers | Composed-narrative views per persona (Business Owner · Enterprise Architect · Modernization Lead · Developer) | top tab `🎯 Insights` |
| 🌌 **AST Galaxy** | Engineers | Structural force-graph (2D/3D), 6 canonical view modes (Technical · Business Domains · Service Catalog · Modernization Radar · BIAN · C4) | top tab `🌌 AST Galaxy` |

---

## 🔎 Quick wins after first scan

| What you want to do | Where to go |
|---|---|
| See estate health at a glance | 🎨 Visual Cockpit → Mission Control |
| Find blocking copybooks (top investments to unlock more coverage) | 🎨 Visual Cockpit → Business Owner |
| See coupling heatmap + service hubs + SPOFs | 🎨 Visual Cockpit → Architect |
| Plan migration waves with drag/click Kanban | 🎨 Visual Cockpit → Modernization Lead |
| Per-program scorecards + click-through to facts + run history | 🎨 Visual Cockpit → Developer |
| Locate a generated service back to its COBOL paragraph | 🔎 search box in Visual Cockpit header *or* 🧭 Modernization Intelligence → 🎯 Capabilities & Locator |
| Discover business capabilities (fraud / payment / settlement / …) from REKT facts | 🧭 → 🎯 Capabilities & Locator |
| Trace a JCL job → programs → copybooks (Mermaid) | 🧭 → 🔗 Service Chain |
| Convert a program with isolated output | 🚀 Mission Control → **🛠️ Convert** button |

---

## Common follow-ups

| Goal | Command |
|---|---|
| Open the portal UI | `./doctor.sh portal` then <http://localhost:5028> |
| Convert to C# instead of Java | `./doctor.sh convert-only --program BDSM043 --target csharp` |
| Convert several programs | `./doctor.sh convert-only --program A --program B --program C` |
| Convert everything called by a program | `./doctor.sh convert-only --program BDSM043 --include-callees` |
| Convert a migration wave | `./doctor.sh convert-only --wave 1 --target java` |
| Diagnose setup or connectivity | `./doctor.sh doctor` |
| Check what changed in REKT scan | `./doctor.sh rekt-status` |
| Re-run quality gates over output | `tools/run-quality-gates.sh` |
| Inspect telemetry | `python3 tools/ingest-metrics.py --rebuild --report` |
| Edit business capability dictionary | edit `Data/capabilities.json` → refresh portal (no rebuild) |

---

## Required environment

| Tool | Why | Install |
|---|---|---|
| .NET 10 SDK | Runs the converters and the portal | `brew install dotnet` |
| Docker | Hosts Neo4j for REKT | `brew install --cask docker` |
| Java 17+ | Runs the smojol REKT parser | `brew install openjdk` |
| `gh` CLI **or** Azure OpenAI key | Auth for the LLM provider | `brew install gh` or use Azure portal |

Run `./doctor.sh doctor` after setup — it verifies all of the above plus model deployments and connectivity.

---

## What you get back per conversion

- `output/runs/{runId}-{lang}-{slug}-{utc}/` — **isolated, immutable** per-run output:
  - `com/example/.../<Program>Service.java` (or `.cs`) — Quarkus-style service with `@ApplicationScoped`, `@Inject`-ed dependencies, faithful translation of working storage + procedure division
  - `migration-report.md` — human-readable summary
  - `migration-conversation-log.md` — full LLM transcript
  - `dependency-map.json` + `dependency-diagram.md` (Mermaid)
- `output/.metrics/<runId>.jsonl` — JSONL stream with `projection_metrics`, `llm_call`, `quality_metrics`, `cache_event`, `reassembly_metrics`, `continuation_event` (used by the analytics ingester)
- `Data/migration.db` — run history (queryable with `sqlite3`)
- `Data/benchmark.db` — ingested telemetry (powers all portal dashboards)
- `Data/projection-cache.db` — deterministic projection-block cache (80%+ hit rate on chunked paths)
- `Data/migration-waves.db` — explicit wave assignments from the Lead Kanban
- A Neo4j subgraph at <http://localhost:7475> — explore the AST/CFG visually

For deeper detail, jump to the [`README`](../README.md) architecture section or one of the [`docs/`](.) deep-dives.
