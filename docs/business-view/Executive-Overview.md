# Executive Overview

**Last updated**: 2026-09-02

This document explains, in plain language, what this project is, the problem it
addresses, who uses it, and the outcomes it is designed to produce. It is the
business-facing entry point for the document set:

- [Sales Folder](Sales-Folder.md) — one-page brief for a first conversation.
- [Technical Overview](Technical-Overview.md) — architecture and honest limits for technical decision-makers.
- [Project Steps](Project-Steps.md) — the phased adoption path and decision gates.
- [Quick Start](../quick-start.md) — hands-on setup and first conversion.
- [Project README](../../README.md) — repository entry point.

## The problem

Organizations that still run COBOL carry decades of business rules inside code
that few current staff can read. The people who wrote it are retiring, the
documentation is incomplete, and every change is slow and risky. Rewriting by
hand is expensive and error-prone; doing nothing raises operational and staffing
risk each year.

The hard part is not typing new code. It is **understanding** the legacy system
well enough to move it safely — which programs call which, which data they touch,
which business capabilities they implement, and in what order they should be
modernized.

## What this solution is

This is an **open-source .NET 10 framework** that helps teams understand and
modernize a COBOL portfolio. It does two connected things:

1. **Assisted conversion.** It converts COBOL programs to **Java** or **C#**
   using a pipeline of specialized AI agents, each grounded in a
   [static analysis](#glossary) of the original code rather than guessing from
   raw source.
2. **A Modernization Intelligence Portal.** A web workspace that turns the same
   analysis into persona-specific views — dashboards, dependency maps, capability
   catalogs, and a wave planner — so business and technical stakeholders can plan
   and track the effort from shared facts.

The active setup workflow supports **Azure OpenAI** and **GitHub Copilot**, giving
organizations a choice between an Azure-hosted model service and their existing
Copilot access.

Importantly, this is an **accelerator and analysis aid**, not an autopilot. It
produces high-quality drafts and structured evidence for humans to review — it
does not certify that converted code behaves identically to the original. See the
[Technical Overview](Technical-Overview.md) for exactly what is and is not
guaranteed.

## Intended users

The portal is built around the personas the project itself serves:

- **Business Owner** — sees progress and readiness gauges, investment context, and
  a risk heatmap.
- **Enterprise Architect** — reviews target architecture and system structure.
- **Modernization Lead** — owns wave planning on a write-capable Kanban board.
- **Developer** — works from per-program scorecards and run history.

A general "Mission Control" view ties these together. These are working views, not
a claim that the tool enforces a formal governance hierarchy; accountability for
decisions stays with your organization (see [Project Steps](Project-Steps.md)).

## Plain-language scenarios

These illustrate how the solution accelerates modernization. They describe the
intended workflow; effort and timing depend on your portfolio.

- **"We don't know what we have."** A team drops its COBOL programs and copybooks
  into a folder and runs the analysis. The portal then shows the program
  inventory, how programs depend on one another, and which business capabilities
  (for example, fraud, KYC, or payment handling) the code implements — without
  spending any AI budget on that discovery step.

- **"Where do we start, and in what order?"** From the analysis, the tool proposes
  a target component and a modernization strategy for each program and groups them
  into migration waves. The Modernization Lead adjusts the plan on a live board and
  exports a strategy workbook for stakeholders.

- **"Prove the approach on something real."** The team converts a single program or
  a small, related set to Java or C#. A portal-managed run produces an isolated
  output folder with the generated code and supporting run artifacts, so reviewers
  can inspect what was produced.

- **"Do it consistently at scale."** Large programs are split at natural boundaries
  so nothing is silently truncated, and the team converts wave by wave.
  Portal-managed runs are preserved separately, giving a clearer audit trail as
  the portfolio moves.

- **"Keep humans in control of quality."** Reviewers read each converted program
  against the original source and available structural facts. They can tune the AI
  instructions (agent prompts) when a pattern needs adjusting, rather than
  accepting a black-box result.

## Expected outcomes

Used as intended, the solution helps a team:

- Build a **shared, evidence-based understanding** of a COBOL portfolio quickly.
- Produce a **defensible, ordered modernization plan** instead of a guess.
- Generate **reviewable Java or C# drafts** with a consistent, auditable process.
- Give business and technical stakeholders **one source of facts** to align on.

What it does **not** do: guarantee that converted code is functionally equivalent
to the original, run or certify tests automatically, or handle deployment, data
migration, or formal sign-off. Those remain program responsibilities. The
[Technical Overview](Technical-Overview.md) and [Project Steps](Project-Steps.md)
are explicit about these boundaries.

## Risk controls

The design keeps humans in control and makes the work auditable:

- **Grounded prompts.** Conversions can be grounded in static-analysis facts, so
  the AI works from a structural blueprint rather than inferring structure from raw
  source.
- **Portal-managed run history.** Portal-managed conversions are written to
  separate timestamped folders. Direct command-line conversions use shared output
  folders and should be archived by the team when retention is required.
- **Optional structural comparison.** The repository contains a parity component,
  but it is not wired into the default conversion run. If explicitly integrated,
  it measures **coverage of structure, not correctness** — an important distinction
  covered in the [Technical Overview](Technical-Overview.md).
- **Human-editable AI instructions.** Agent prompts are open and editable; the
  project states generated prompts are "a starting point, not a final answer."
- **Credential choices.** API keys and optional personal access tokens can be kept
  in a local, git-ignored environment file. GitHub Copilot and key-less Microsoft
  Entra ID modes can instead use their authenticated CLI or default credential
  chains, so credentials are not committed to the repository.
- **Source stays under your control.** Converting or analysing a program sends its
  source code to the AI provider you configure, while the analysis and portal stack
  run locally — see the [Technical Overview](Technical-Overview.md) for detail.
- **Network isolation is required.** The portal currently has no application-level
  authentication and listens on all network interfaces. Run it only on a trusted,
  restricted network, or place it behind an authenticated TLS reverse proxy; do
  not expose it directly to the internet or other untrusted networks.

## Where it fits in a modernization program

Think of this as the **understand, plan, and convert** engine near the front of a
larger effort. It accelerates discovery, planning, and code generation, and it
produces the artifacts other program activities depend on. It does **not** replace
deployment, cutover, data migration, testing, or governance — it feeds them. The
recommended adoption path is in [Project Steps](Project-Steps.md).

## Glossary

Plain-language definitions of terms used across this document set.

- **COBOL** — A programming language widely used since the 1960s for business,
  finance, and administrative systems. Much of it is still in production.
- **Copybook** — A reusable fragment of COBOL, typically a shared data-record
  layout, pulled into many programs (similar to a shared header or schema). One
  copybook can be used by dozens of programs.
- **Static analysis** — Examining code **without running it** to learn its
  structure: its parts, how they connect, what data they use, and what they call.
  It is deterministic and does not consume AI budget.
- **REKT** — The project's static-analysis stage. It parses COBOL with a grammar-
  based toolkit and produces structured facts (program structure, control flow,
  and data flow) that ground the AI conversion and feed the portal. Details in
  [REKT-grounded conversion](../rekt-grounded-conversion.md).
- **Parity (structural / lexical)** — An optional component for estimating how much
  of the original program's structure appears in the converted code (for example,
  whether expected method names, fields, and calls are present). It is not invoked
  by the default conversion workflow and is a **coverage indicator, not a proof of
  correctness or equivalence.** See the
  [Technical Overview](Technical-Overview.md).
- **Agent** — A specialized AI worker with a single job (for example, analyzing
  COBOL or converting to Java), driven by an editable prompt.
- **Migration wave** — A planned group of programs modernized together, sequenced
  by dependency and effort.
