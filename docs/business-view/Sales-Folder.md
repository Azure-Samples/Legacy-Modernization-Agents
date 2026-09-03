# Sales Folder

**Last updated**: 2026-09-02

A one-page brief on what this project is and why it helps. For the fuller
picture, see the [Executive Overview](Executive-Overview.md),
[Technical Overview](Technical-Overview.md), and [Project Steps](Project-Steps.md).

## The challenge

Many organizations still depend on COBOL systems that encode decades of business
rules. The people who understand that code are retiring, documentation is thin,
and every change is slow and risky. The obstacle is rarely typing new code — it is
understanding the legacy system well enough to modernize it safely.

## The proposition

This is an **open-source .NET 10 framework** that helps teams understand and
modernize a COBOL portfolio. It combines two capabilities on one shared base of
facts:

- **Assisted conversion** of COBOL to **Java** or **C#**, using a pipeline of
  specialized AI agents grounded in a static analysis of the original code.
- **A Modernization Intelligence Portal** — a web workspace that turns the same
  analysis into role-specific dashboards, dependency maps, a capability catalog,
  and a migration-wave planner.

The active setup workflow supports **Azure OpenAI** and **GitHub Copilot**, so
teams can use an Azure-hosted model service or their existing Copilot access.

## Business benefits

- **Faster understanding.** Build a shared, evidence-based picture of a COBOL
  portfolio — inventory, dependencies, and business capabilities — quickly.
- **A defensible plan.** Get an ordered, editable modernization plan instead of a
  guess, aligned across business and technical stakeholders.
- **Reviewable output.** Produce Java or C# drafts through a consistent, auditable
  process, with portal-managed runs preserved separately.
- **Control and trust.** Humans review every result; the AI instructions are open
  and editable; credentials are supplied through git-ignored local configuration
  or authenticated CLI/default credential chains rather than committed source.

## Representative scenarios

One line each; full versions in the [Executive Overview](Executive-Overview.md).

- **"We don't know what we have."** Analyze the code and see the inventory,
  dependencies, and business capabilities — with no AI cost for that discovery.
- **"Where do we start?"** Get a per-program target and strategy, grouped into
  editable migration waves.
- **"Prove it on something real."** Convert one program and review the code,
  report, and supporting artifacts in a portal-managed output folder.
- **"Do it at scale."** Convert wave by wave, with large programs split safely and
  portal-managed runs kept separately.

## What sets it apart

Each differentiator is a real, verifiable property of the framework:

- **Grammar-based grounding.** Conversions can be grounded in static-analysis
  facts (program structure, control flow, data flow), giving the AI a structural
  blueprint rather than raw source to guess from.
- **Zero-cost capability discovery.** Business-capability and semantic search over
  the portfolio runs deterministically, without consuming AI budget.
- **Isolated portal-managed outputs.** Conversions launched through the portal
  write to separate timestamped folders. Direct CLI conversions use shared
  language output folders, so teams should archive them when retention is needed.
- **Editable agent prompts.** The instructions driving each AI agent are open and
  tunable, so teams shape output instead of accepting a black box.
- **Provider choice.** Azure OpenAI (including key-less Microsoft Entra ID) or
  GitHub Copilot, with per-agent model choice.

## Honest boundaries

We state limits plainly. The tool **accelerates** understanding, planning, and
code generation; it does **not** certify that converted code behaves identically
to the original, run tests automatically, or handle deployment, data migration, or
sign-off. The repository's optional parity component measures **structural
coverage, not correctness**, and is not wired into the default conversion run.
Converting a program sends its source code to the AI provider you configure, while
the analysis and portal stack are self-hostable via Docker — so provider choice and
any related data-handling requirements stay your decision. The portal has no
application-level authentication and must be restricted to a trusted network or
placed behind an authenticated TLS reverse proxy; it should not be exposed
directly to the internet. See the
[Technical Overview](Technical-Overview.md) for details.

## Call to action

The framework is open source. To evaluate it:

1. Read the [Executive Overview](Executive-Overview.md) for the business case and
   scenarios.
2. Review the [Technical Overview](Technical-Overview.md) for architecture and
   limits.
3. Follow the [Quick Start](../quick-start.md) to run a first analysis and
   conversion on your own COBOL.
4. Plan a pilot using [Project Steps](Project-Steps.md).
