# Modernization Intelligence Portal — design notes

**Last updated**: 2026-05-28
**Status**: Design phase — implementation not started

## Why this exists

The current AST Galaxy Explorer is engineering/parser/debugging centric. As the
platform matures into a **semantic modernization infrastructure** (PR4 →
PR5 → PR6 validated), users need a workspace that thinks in
*modernization concepts*, not parser concepts:

- application inventory
- migration waves
- service candidates
- modernization readiness
- dependency topology
- semantic flows
- portfolio-level dashboards

This document captures the design before any code lands so we can evaluate
scope, dependencies, and incremental delivery without rebuilding existing
graph infrastructure.

## Architectural principle

**Do NOT replace the AST Galaxy Explorer.** Introduce a new top-level
workspace beside it. Reuse everything:

| Already exists | Re-used by portal |
|---|---|
| REKT nodes + edges | All views |
| Neo4j topology graph | Dependency Topology, Semantic Flows |
| AST graph | Application Explorer (drill-down) |
| Semantic projections (`output/rekt/*.facts.json`) | Projection Explorer, Modernization Readiness |
| `Data/projection-cache.db` (PR6) | Projection Explorer (cache eligibility) |
| `Data/benchmark.db` (PR5 ingester) | Modernization Dashboard, Runtime & Conversion Intelligence |
| `output/.metrics/*.jsonl` (PR5) | Runtime & Conversion Intelligence |
| `Data/migration.db` runs table | Portfolio Insights, Modernization Dashboard |
| MCP web (`McpChatWeb/`) | Hosts the new workspace |

## Top-level workspace structure

```
McpChatWeb
├── (existing) AST Galaxy Explorer        ← engineer's view
└── Modernization Intelligence             ← new portfolio view
    ├── Application Explorer
    ├── Dependency Topology
    ├── Migration Waves
    ├── Semantic Flows
    ├── Service Candidates
    ├── Modernization Dashboard
    ├── Runtime & Conversion Intelligence
    ├── Projection Explorer
    └── Portfolio Insights
```

## Phase-1 implementation scope (~3 PRs)

Build only the 4 views that have direct data sources already:

### Application Explorer — PR-P1
Data sources:
- `source/**/*.cbl` for inventory
- `output/rekt/*.facts.json` for per-program facts (dependency count,
  copybook reuse, complexity hint)
- `Data/migration.db` runs table for compile-success per program
- `Data/benchmark.db` quality_metrics for latest gate results

Columns:
- program basename / relative path
- LoC, copybook count, dependency count, complexity tier
- modernization status (not started / in progress / converted)
- last compile result (✅ / ❌ / n/a)
- cache eligibility (projection hash, hit count)
- wave assignment (editable)

Delivery: read-only first, wave assignment in PR-P2.

### Modernization Dashboard — PR-P1
Data sources:
- `Data/benchmark.db` aggregated views (already produced by
  `tools/ingest-metrics.py --report`)

Cards:
- compile success % over last N runs
- continuation events count + ratio (when P1 telemetry has data)
- projection reduction (avg / median across corpus)
- cache hit rate (overall + by agent)
- orchestration latency p50 / p95
- modernization throughput (runs/day, lines/min)

Delivery: backend is just SQL views over `benchmark.db`; frontend is a
table + small set of charts.

### Runtime & Conversion Intelligence — PR-P2
Data sources:
- `output/.metrics/{runId}.jsonl` per-run event stream
- `Data/benchmark.db` for cross-run aggregates

Views:
- timeline per run: chunk → projection_metrics → llm_call → quality_metrics
- continuation chains visualized
- cache hit/miss overlay
- compile validation banner per run

Delivery: requires a streaming/timeline UI component.

### Projection Explorer — PR-P2
Data sources:
- `output/rekt/*.facts.json` (raw input)
- `Data/projection-cache.db` (cached output blocks + hit counts)
- `MetricsSink` projection_metrics events

Views:
- side-by-side: raw AST vs program-facts JSON vs projection block
- per-program token reduction
- cache eligibility (input hash, output hash, hit count)
- hash reuse graph (which programs share projection blocks)

Delivery: pure-data view, no Neo4j queries.

## Phase-2 scope (~3 more PRs)

### Dependency Topology — PR-P3
Requires Neo4j read queries (already exists). Renders as layered
architecture diagram with semantic overlays (which nodes have facts.json,
which are cached, which have compile success).

### Migration Waves — PR-P3
Persistent wave assignments per program. Manual today, ML-suggested later.

### Semantic Flows / Service Candidates / Portfolio Insights — PR-P4+
Higher-level views built on top of the above.

## Key UX commitments
- Every chart links back to the underlying data file/event for debugging
- All data is **derived from artifacts the conversion pipeline already produces** — no new data collection layer
- Read-only-first: every view should work without any portal writes; mutations (wave assignment, conversion triggers) come later
- No real-time dependency on a running conversion — works against historical metrics

## Non-goals (explicitly out of scope)
- Replace AST Galaxy Explorer
- Real-time orchestration control
- Custom telemetry collection (use MetricsSink + benchmark.db)
- LLM cost dashboards (PR7-territory — needs the response cache first)
- Multi-tenant portfolio management

## Dependencies before PR-P1 implementation
1. `tools/ingest-metrics.py` needs to expose data via a stable view (CSV
   export or HTTP endpoint in McpChatWeb)
2. McpChatWeb routing for the new top-level workspace
3. A small shared component library for the table + card UI primitives

## Open design questions
- Should portal writes (wave assignment) go to a new table or extend `Data/migration.db`?
- Token-cost estimates: include in dashboard now or wait for PR7?
- Cross-program insights (shared copybooks, shared service interfaces): which view owns them?

## Next concrete action
Build a static prototype of the Modernization Dashboard view using
`tools/ingest-metrics.py --report` output as the data source. Validates
the data plumbing without committing to UI framework choices.
