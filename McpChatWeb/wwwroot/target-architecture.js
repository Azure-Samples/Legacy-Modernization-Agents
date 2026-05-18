// Target Architecture View
// ─────────────────────────────────────────────────────────────────────
// Renders a recommended cloud-native target architecture for the scanned
// COBOL portfolio and maps every program to a target component with a
// modernization strategy. The recommendation JSON is also persisted to
// output/rekt/target-architecture.json so downstream conversion agents
// (AI or otherwise) can read it as a deterministic conversion plan.
//
// Industry-neutral: the architecture and heuristics are not banking-
// specific. Programs are classified by their technical signature
// (SQL/CALL/branching/naming) rather than business-domain assumptions.
// ─────────────────────────────────────────────────────────────────────

class TargetArchitectureView {
  constructor(rootId) {
    this.rootId = rootId;
    this.programs = [];
    this.recommendations = null;
    this.selectedComponent = null;
    this.scanRunId = null;
    this._lastSavedAt = null;
    // Explicit per-group expansion overrides (key → true=expanded / false=collapsed).
    this._groupOverrides = new Map();
    // Diagram zoom (1 = fit-to-container). Survives re-renders within the session.
    this._diagramZoom = 1;
    this._diagramFullscreen = false;
    // Diagram filter: false (default) = tailor to the scan; true = full template.
    this._diagramShowAll = false;
  }

  // Domain acronyms most often used in the recommendations. Keys are matched
  // as whole-words (case-sensitive) and rendered with an underline + tooltip
  // so users don't have to look them up. Add new entries here when new terms
  // appear in rationales or migration notes.
  static GLOSSARY = {
    'DTO':      'Data Transfer Object — a plain data class (just fields, no logic) used to move data between layers/services. Copybooks become DTOs in the target.',
    'DTOs':     'Data Transfer Objects — plain data classes (just fields, no logic) used to move data between layers/services. Copybooks become DTOs in the target.',
    'JPA':      'Java Persistence API — Java ORM standard, typically used with Hibernate to map relational tables to Java classes.',
    'EF Core':  'Entity Framework Core — Microsoft\'s ORM for .NET, equivalent to JPA/Hibernate in the Java ecosystem.',
    'JDBC':     'Java Database Connectivity — the low-level Java API for SQL access (below JPA).',
    'BFF':      'Backend-for-Frontend — a thin API tailored to a specific UI surface, sitting in front of the general-purpose services.',
    'CQRS':     'Command Query Responsibility Segregation — design pattern that separates write-side (commands) from read-side (queries), often with different data models.',
    'DDD':      'Domain-Driven Design — modelling approach that organises code around the business domain (bounded contexts, aggregates, entities, value objects).',
    'OIDC':     'OpenID Connect — identity layer built on top of OAuth 2.0, used for sign-on / SSO.',
    'OAuth2':   'OAuth 2.0 — industry-standard authorization protocol used to grant tokens to clients.',
    'JWT':      'JSON Web Token — signed token format commonly used as the bearer token in OAuth2/OIDC flows.',
    'RBAC':     'Role-Based Access Control — authorization model where permissions are attached to roles, and users hold one or more roles.',
    'ABAC':     'Attribute-Based Access Control — authorization model where decisions are based on attributes of the user, resource and environment.',
    'IdP':      'Identity Provider — the service that authenticates users and issues tokens (e.g. Entra ID, Keycloak, Auth0).',
    'SSO':      'Single Sign-On — a user signs in once and gains access to multiple connected systems.',
    'APIM':     'Azure API Management — Microsoft\'s managed API gateway product.',
    'ELT':      'Extract-Load-Transform — modern data pipeline pattern where raw data is loaded first, then transformed inside the warehouse.',
    'ETL':      'Extract-Transform-Load — classical data pipeline where transformations happen before loading into the warehouse.',
    'SPA':      'Single-Page Application — a web app that renders in the browser and updates without full page reloads (React, Vue, Angular).',
    'SLO':      'Service Level Objective — a target value for a reliability metric (e.g. 99.9% availability).',
    'CICS':     'Customer Information Control System — IBM\'s transaction-processing monitor on z/OS; runs COBOL screen programs.',
    'IMS':      'Information Management System — IBM\'s hierarchical database & transaction manager on z/OS.',
    'VSAM':     'Virtual Storage Access Method — IBM\'s file storage system for z/OS, often used for record-oriented data.',
    'QSAM':     'Queued Sequential Access Method — sequential file access on z/OS.',
    'GDG':      'Generation Data Group — z/OS construct for versioned sequential files.',
    'BMS':      'Basic Mapping Support — CICS facility for defining 3270 terminal screen layouts (maps).',
    'COTS':     'Commercial Off-The-Shelf — pre-built software bought instead of written; "Replace" strategy aims at COTS substitution.',
    'SaaS':     'Software-as-a-Service — software delivered as a hosted service, billed by subscription.',
    'AST':      'Abstract Syntax Tree — parsed structure of a program; the analyser produces one per scanned COBOL file.',
    'CFG':      'Control-Flow Graph — graph of execution paths through a program; used for branch analysis.',
    'NPM':      'Node Package Manager — the JavaScript package registry; used here for internal shared TypeScript packages.',
  };

  // Wrap known acronyms in <abbr> tags with tooltips. Whole-word match,
  // applied before HTML escaping is reversed. Input is plain text; output is
  // HTML-safe (escapes everything except our explicit <abbr> markup).
  _glossaryHtml(text) {
    if (!text) return '';
    const escaped = this._esc(text);
    const keys = Object.keys(TargetArchitectureView.GLOSSARY)
      .sort((a, b) => b.length - a.length); // longest first so "DTOs" wins over "DTO"
    let out = escaped;
    for (const k of keys) {
      const pattern = new RegExp(`\\b${k.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}\\b`, 'g');
      const expl = TargetArchitectureView.GLOSSARY[k];
      out = out.replace(pattern,
        `<abbr title="${this._escAttr(expl)}" style="text-decoration:underline dotted;text-decoration-color:#6366f1;cursor:help;">${k}</abbr>`);
    }
    return out;
  }

  // ─── Recommended Target Architecture (cloud-native microservices) ───
  // Intentionally industry-neutral. Components describe **technical
  // capabilities** an AI conversion agent needs to know about, not
  // business domains.
  static ARCHITECTURE = {
    style: 'Cloud-Native Microservices',
    description: 'API-gateway-fronted microservices with event-driven async coordination and managed data services. Default modernization target for COBOL portfolios regardless of industry.',
    layers: [
      {
        name: 'Edge',
        icon: '🌐',
        color: '#1e3a8a',
        border: '#3b82f6',
        components: [
          {
            id: 'web-ui',
            name: 'Web / Mobile UI',
            type: 'frontend',
            tech: 'React / Next.js · TypeScript',
            replaces: 'CICS screens, IMS terminals, 3270 maps',
            responsibilities: ['User interaction', 'Form input', 'Result display'],
            patterns: ['SPA', 'Server-side rendering for SEO-relevant routes'],
            consumes: ['api-gateway'],
          },
          {
            id: 'api-gateway',
            name: 'API Gateway',
            type: 'gateway',
            tech: 'Azure APIM / AWS API Gateway / Kong',
            replaces: 'Direct screen-to-program transaction routing',
            responsibilities: ['Routing', 'Auth enforcement', 'Rate limiting', 'API versioning'],
            patterns: ['BFF (Backend-for-Frontend) where useful'],
            consumes: ['svc-business', 'svc-data', 'svc-identity', 'svc-reporting'],
          },
        ],
      },
      {
        name: 'Service Layer',
        icon: '⚙️',
        color: '#065f46',
        border: '#10b981',
        components: [
          {
            id: 'svc-business',
            name: 'Business Logic Services',
            type: 'service',
            tech: 'Java Spring Boot / .NET 8',
            replaces: 'CALL-heavy orchestrator programs',
            responsibilities: ['Business rules', 'Workflow orchestration', 'Validation'],
            patterns: ['Domain-Driven Design', 'Hexagonal architecture', 'CQRS where it fits'],
            consumes: ['svc-data', 'event-bus'],
          },
          {
            id: 'svc-data',
            name: 'Data Access Services',
            type: 'service',
            tech: 'Java Spring Boot + JPA / .NET EF Core',
            replaces: 'SQL-heavy programs (EXEC SQL chains)',
            responsibilities: ['Persistence', 'Query optimization', 'Referential integrity'],
            patterns: ['Repository', 'DTO at boundary', 'Read replicas for queries'],
            consumes: ['db-relational'],
          },
          {
            id: 'svc-identity',
            name: 'Identity & Access',
            type: 'service',
            tech: 'Keycloak / Entra ID / Cognito',
            replaces: 'Sign-on programs, abend handlers, RACF integration',
            responsibilities: ['Authentication', 'Authorization (RBAC/ABAC)', 'Session management'],
            patterns: ['OAuth2 / OIDC', 'JWT bearer tokens'],
            consumes: [],
          },
        ],
      },
      {
        name: 'Async / Batch',
        icon: '📦',
        color: '#78350f',
        border: '#f59e0b',
        components: [
          {
            id: 'batch-worker',
            name: 'Batch Workers',
            type: 'worker',
            tech: 'Spring Batch / Argo Workflows / Azure Durable Functions',
            replaces: 'Standalone batch programs (nightly runs, periodic loaders)',
            responsibilities: ['Scheduled processing', 'Bulk operations', 'Reprocessing'],
            patterns: ['Chunked processing', 'Idempotent jobs', 'Checkpoint/restart'],
            consumes: ['svc-data', 'object-store', 'event-bus'],
          },
          {
            id: 'svc-reporting',
            name: 'Reporting / Analytics',
            type: 'service',
            tech: 'dbt + Snowflake / Databricks / BigQuery',
            replaces: 'COBOL report programs writing print files',
            responsibilities: ['Aggregations', 'Extracts', 'Dashboards', 'Regulatory output'],
            patterns: ['ELT', 'Star schema for marts'],
            consumes: ['db-relational', 'object-store'],
          },
          {
            id: 'event-bus',
            name: 'Event Bus',
            type: 'infrastructure',
            tech: 'Kafka / Azure Event Hubs / AWS Kinesis',
            replaces: 'File-based handoff between programs (sequential drops)',
            responsibilities: ['Async messaging', 'Event sourcing', 'Service decoupling'],
            patterns: ['At-least-once delivery', 'Schema registry'],
            consumes: [],
          },
        ],
      },
      {
        name: 'Data',
        icon: '🗄️',
        color: '#1c1917',
        border: '#78716c',
        components: [
          {
            id: 'db-relational',
            name: 'Relational Database',
            type: 'database',
            tech: 'PostgreSQL / Aurora / Azure SQL',
            replaces: 'DB2, IMS DB, IDMS',
            responsibilities: ['Transactional storage', 'Referential integrity'],
            patterns: ['Single database per service where ownership is clear'],
            consumes: [],
          },
          {
            id: 'object-store',
            name: 'Object Storage',
            type: 'storage',
            tech: 'S3 / Azure Blob / GCS',
            replaces: 'VSAM, QSAM sequential files, GDG generations',
            responsibilities: ['File archives', 'Large blobs', 'Data lake landing zone'],
            patterns: ['Lifecycle policies', 'Immutable buckets for audit'],
            consumes: [],
          },
        ],
      },
      {
        name: 'Cross-Cutting',
        icon: '🔍',
        color: '#0c4a6e',
        border: '#0ea5e9',
        components: [
          {
            id: 'observability',
            name: 'Observability',
            type: 'platform',
            tech: 'OpenTelemetry + Grafana / Datadog / Application Insights',
            replaces: 'COBOL DISPLAY, manually-tailed SYSOUT, ad-hoc log files',
            responsibilities: ['Metrics', 'Distributed tracing', 'Structured logs'],
            patterns: ['Correlation IDs end-to-end', 'SLO-driven alerting'],
            consumes: [],
          },
          {
            id: 'shared-lib',
            name: 'Shared Libraries',
            type: 'library',
            tech: 'Domain JAR / NuGet package / internal NPM',
            replaces: 'Copybooks (data layouts, constants)',
            responsibilities: ['DTOs', 'Constants', 'Validators', 'Common helpers'],
            patterns: ['Semver', 'No business logic — pure schema and helpers'],
            consumes: [],
          },
        ],
      },
    ],
  };

  // Migration strategies (the 7 Rs, condensed to the ones we recommend in practice).
  static STRATEGIES = {
    retire:     { label: 'Retire',     color: '#475569', icon: '🗑️',
                  description: 'Decommission. Functionality is dead, duplicated, or no longer required.' },
    rehost:     { label: 'Rehost',     color: '#0ea5e9', icon: '🚚',
                  description: 'Lift-and-shift onto a managed COBOL runtime (e.g. Micro Focus Enterprise Server on AKS/EKS). Lowest disruption, no business value uplift.' },
    replatform: { label: 'Replatform', color: '#10b981', icon: '🔧',
                  description: 'Automated translation to a managed runtime (e.g. JCL→Java, EXEC SQL→JDBC) preserving structure. Modest uplift.' },
    rearchitect:{ label: 'Rearchitect',color: '#f59e0b', icon: '🏗️',
                  description: 'Rewrite as a microservice in the target stack. Highest uplift, highest cost. Reserved for valuable or complex code.' },
    replace:    { label: 'Replace',    color: '#a855f7', icon: '🔁',
                  description: 'Substitute with a SaaS or COTS product. Use when the function is non-differentiating commodity (e.g. report scheduler, sign-on).' },
  };

  // ─── Per-program recommendation engine ──────────────────────────────
  recommend(p) {
    const name = (p.program || '').replace(/^flow-ast-/i,'').replace(/\.(cbl|cpy)$/i,'').toUpperCase();
    const sqlCount = p.sqlCount || 0;
    const callCount = p.callCount || 0;
    const lineCount = p.lineCount || 0;
    const sectionCount = p.sectionCount || 0;
    const branchCount = p.branchCount || 0;
    const performCount = p.performCount || 0;

    // ── 1) Target component (technical-signature based, NOT business) ──
    let target;
    if (p.isCopybook) {
      target = 'shared-lib';
    } else if (/(^|[^A-Z])(CICS|SCREEN|TERM|MAP|MENU)/.test(name) || (p.cicsCount || 0) > 0) {
      target = 'web-ui';
    } else if (/(^|[^A-Z])(SGN|SIGN|AUTH|LOGIN|ABND|ABEND)/.test(name)) {
      target = 'svc-identity';
    } else if (/(RPT|REPORT|EXP|DUMP|EXTR|EXTRACT|LIST)/.test(name)) {
      target = 'svc-reporting';
    } else if (/^CB|^BAT|BATCH|^DG|^DO|NIGHT|^T660DG|^T660DO/.test(name)) {
      target = 'batch-worker';
    } else if (sqlCount >= Math.max(10, callCount * 2)) {
      target = 'svc-data';
    } else if (callCount >= 3 || performCount >= 8) {
      target = 'svc-business';
    } else {
      target = 'svc-business'; // safe default for general business code
    }

    // ── 2) Modernization strategy ─────────────────────────────────────
    const complexity = this._complexity(p);
    let strategy;
    const reasons = [];

    if (p.isCopybook) {
      strategy = 'rearchitect';
      reasons.push('Copybooks become DTOs / domain types in the shared library.');
    } else if (lineCount > 0 && lineCount < 50 && sqlCount === 0 && callCount === 0) {
      strategy = 'retire';
      reasons.push(`Tiny stub-like program (${lineCount} LOC, no SQL, no CALL) — likely dead or duplicated.`);
    } else if (target === 'svc-reporting' && complexity < 0.4) {
      strategy = 'replace';
      reasons.push('Reporting is commodity functionality — replace with a managed analytics/BI tool rather than rewriting it.');
    } else if (target === 'svc-identity') {
      strategy = 'replace';
      reasons.push('Sign-on and auth are commodity — replace with a managed IdP (Entra ID, Keycloak) instead of porting custom logic.');
    } else if (complexity >= 0.6 || branchCount > 30 || sqlCount > 25 || callCount > 6) {
      strategy = 'rearchitect';
      reasons.push(`High complexity (score ${complexity.toFixed(2)}, ${branchCount} branches, ${sqlCount} SQL, ${callCount} CALLs) — full rewrite into a domain service.`);
    } else if (complexity >= 0.3) {
      strategy = 'replatform';
      reasons.push(`Moderate complexity (score ${complexity.toFixed(2)}) — translate structure-preserving to the target stack.`);
    } else if (complexity < 0.15 && lineCount > 0) {
      strategy = 'rehost';
      reasons.push('Very low complexity and few external touchpoints — lift-and-shift onto a managed COBOL runtime to defer modernization cost.');
    } else {
      strategy = 'replatform';
      reasons.push('Default for unclassified programs: structure-preserving translation, refine after first wave.');
    }

    // ── 3) Wave (1 = quick win / foundation, 3 = high risk / late) ────
    let wave;
    if (strategy === 'retire' || target === 'shared-lib') {
      wave = 1; // cleanup first
    } else if (target === 'svc-data' && strategy !== 'rearchitect') {
      wave = 1; // unblock services with data layer first
    } else if (target === 'svc-identity' || strategy === 'replace') {
      wave = 1; // commodity replacements are usually low-risk early wins
    } else if (complexity >= 0.6 || strategy === 'rearchitect') {
      wave = 3;
    } else {
      wave = 2;
    }

    // ── 4) Migration notes (concrete guidance for the conversion agent)
    const notes = [];
    if (sqlCount > 0) {
      notes.push(`Convert ${sqlCount} EXEC SQL statements to repository methods (JPA / EF Core).`);
    }
    if (callCount > 0) {
      notes.push(`Replace ${callCount} CALL statements with synchronous service-to-service calls (REST) or async events where decoupling helps.`);
    }
    if (performCount > 5) {
      notes.push(`The ${performCount} PERFORM blocks suggest internal procedural decomposition — map each to a private method or extract into a dedicated class.`);
    }
    if (branchCount > 15) {
      notes.push(`High branch count (${branchCount}) — preserve a decision-table test fixture during conversion to catch regressions.`);
    }
    if (p.isCopybook) {
      notes.push('Generate a typed DTO/record from this copybook. Keep field names; convert PIC clauses to native types (PIC X→String, PIC 9→int/decimal, COMP-3 → BigDecimal).');
    }
    if (target === 'batch-worker') {
      notes.push('Schedule via the orchestrator (Argo / Airflow). Make idempotent. Externalise checkpoint state to the relational DB.');
    }
    if (target === 'web-ui') {
      notes.push('Screen flow → React route. CICS BMS map fields → form components. Persist user state server-side via the API gateway session.');
    }

    const component = this._componentMeta(target);
    return {
      targetComponent: target,
      targetComponentName: component.name,
      targetLayer: component.layer,
      targetTech: component.tech,
      strategy,
      wave,
      complexity: Number(complexity.toFixed(3)),
      rationale: reasons.join(' '),
      patterns: component.patterns || [],
      migrationNotes: notes,
    };
  }

  _complexity(p) {
    const factors = [
      Math.min(1, (p.branchCount  || 0) / 30),
      Math.min(1, (p.sqlCount     || 0) / 20),
      Math.min(1, (p.callCount    || 0) / 10),
      Math.min(1, (p.paraCount    || 0) / 40),
      Math.min(1, (p.nodeCount    || 0) / 500),
    ];
    return factors.reduce((a,b) => a+b, 0) / factors.length;
  }

  _componentMeta(id) {
    for (const layer of TargetArchitectureView.ARCHITECTURE.layers) {
      for (const c of layer.components) {
        if (c.id === id) return { ...c, layer: layer.name };
      }
    }
    return { id, name: id, tech: '?', layer: '?' };
  }

  // Called by dashboard-tabs.js when the scan-run selector changes. Symmetric
  // with migrationPlanner.refresh(): clear cached state, then re-fetch.
  refresh() {
    this.programs = [];
    this.recommendations = null;
    this.selectedComponent = null;
    return this.loadAndRender();
  }

  // ─── Data loading ───────────────────────────────────────────────────
  async loadAndRender() {
    const root = document.getElementById(this.rootId);
    if (!root) return;
    root.innerHTML = '<div style="padding:24px;color:#94a3b8;">Loading scan data…</div>';

    try {
      const runId = (typeof getSelectedScanRunId === 'function') ? getSelectedScanRunId() : 'latest';
      this.scanRunId = runId;
      const qs = (runId && runId !== 'latest' && runId !== 'all') ? `?scanRunId=${encodeURIComponent(runId)}` : '';
      const resp = await fetch(`/api/graph/rekt/galaxy${qs}`);
      if (!resp.ok) throw new Error(`galaxy fetch failed: ${resp.status}`);
      const data = await resp.json();
      this.programs = data.programs || [];
    } catch (e) {
      root.innerHTML = `<div style="padding:24px;color:#ef4444;">Failed to load scan data: ${this._esc(e.message)}<br><span style="color:#94a3b8;">Run <code>./doctor.sh rekt-full</code> first.</span></div>`;
      return;
    }

    this._buildRecommendations();
    this._render();
  }

  _buildRecommendations() {
    const mappings = this.programs.map(p => ({
      program: (p.program || '').replace(/^flow-ast-/i,''),
      displayName: (p.program || '').replace(/^flow-ast-/i,'').replace(/\.cbl$/i,''),
      isCopybook: !!p.isCopybook,
      metrics: {
        lineCount:    p.lineCount    || 0,
        sqlCount:     p.sqlCount     || 0,
        callCount:    p.callCount    || 0,
        sectionCount: p.sectionCount || 0,
        paraCount:    p.paraCount    || 0,
        performCount: p.performCount || 0,
        branchCount:  p.branchCount  || 0,
        nodeCount:    p.nodeCount    || 0,
      },
      recommendation: this.recommend(p),
    }));

    const summary = {
      totalPrograms: mappings.filter(m => !m.isCopybook).length,
      totalCopybooks: mappings.filter(m => m.isCopybook).length,
      byStrategy: {},
      byComponent: {},
      byWave: {1:0, 2:0, 3:0},
    };
    for (const m of mappings) {
      const s = m.recommendation.strategy;
      const c = m.recommendation.targetComponent;
      summary.byStrategy[s] = (summary.byStrategy[s] || 0) + 1;
      summary.byComponent[c] = (summary.byComponent[c] || 0) + 1;
      summary.byWave[m.recommendation.wave] = (summary.byWave[m.recommendation.wave] || 0) + 1;
    }

    this.recommendations = {
      schemaVersion: '1.0',
      generatedAt: new Date().toISOString(),
      scanRunId: this.scanRunId,
      architecture: TargetArchitectureView.ARCHITECTURE,
      strategies: TargetArchitectureView.STRATEGIES,
      programMappings: mappings,
      summary,
    };
  }

  // ─── Rendering ──────────────────────────────────────────────────────
  _render() {
    const root = document.getElementById(this.rootId);
    if (!root) return;

    const r = this.recommendations;
    if (!r) {
      root.innerHTML = '<div style="padding:24px;color:#94a3b8;">No recommendations yet.</div>';
      return;
    }

    const A = TargetArchitectureView.ARCHITECTURE;
    const S = TargetArchitectureView.STRATEGIES;

    let html = `<div style="height:100%;overflow:auto;padding:18px 22px;font-family:'Inter',system-ui,sans-serif;background:#0a0e1a;color:#e2e8f0;">`;

    // ── Header
    html += `<div style="margin-bottom:16px;display:flex;justify-content:space-between;align-items:flex-start;gap:16px;flex-wrap:wrap;">
      <div>
        <h2 style="margin:0 0 4px 0;font-size:20px;color:#f1f5f9;">🎯 Recommended Target Architecture</h2>
        <div style="font-size:12px;color:#94a3b8;line-height:1.5;max-width:780px;">
          <strong style="color:#cbd5e1;">${this._esc(A.style)}</strong> — ${this._esc(A.description)}
        </div>
        <div style="font-size:11px;color:#64748b;margin-top:6px;">
          Generated ${new Date(r.generatedAt).toLocaleString()} · scan run <code>${this._esc(this.scanRunId || 'latest')}</code> ·
          ${r.summary.totalPrograms} programs + ${r.summary.totalCopybooks} copybooks mapped
        </div>
      </div>
      <div style="display:flex;gap:8px;align-items:center;">
        <button id="tarch-save" class="btn-small" title="Persist this plan to output/rekt/target-architecture.json so downstream conversion agents can read it.">💾 Save for AI agent</button>
        <button id="tarch-download" class="btn-small" title="Download the JSON plan to your computer.">⬇️ Download JSON</button>
        <button id="tarch-refresh" class="btn-small" title="Recompute from the latest scan.">↻ Refresh</button>
      </div>
    </div>`;

    html += `<div id="tarch-save-status" style="font-size:11px;color:#64748b;margin-bottom:12px;min-height:14px;"></div>`;

    // ── Mermaid recommended architecture diagram
    html += `<div id="tarch-diagram-card" style="margin-bottom:18px;border:1px solid #1e293b;border-radius:8px;background:#0f172a;">
      <div style="padding:8px 14px;border-bottom:1px solid #1e293b;display:flex;align-items:center;gap:10px;">
        <span style="font-size:13px;font-weight:600;color:#93c5fd;">🏗️ Target Architecture Diagram</span>
        <span style="font-size:10px;color:#64748b;">live program counts per component · click any component below to filter</span>
        <span style="margin-left:auto;display:flex;gap:6px;align-items:center;">
          <label style="display:flex;align-items:center;gap:4px;font-size:11px;color:#94a3b8;cursor:pointer;margin-right:6px;" title="Show every component in the recommended template, including ones with no programs mapped. When off (default) the diagram shows only the components relevant to the current scan plus their dependencies.">
            <input type="checkbox" id="tarch-show-all" ${this._diagramShowAll ? 'checked' : ''} style="accent-color:#3b82f6;">
            Show full template
          </label>
          <button id="tarch-zoom-out" class="btn-small" title="Make diagram smaller">−</button>
          <button id="tarch-zoom-in"  class="btn-small" title="Make diagram larger">＋</button>
          <button id="tarch-fit"      class="btn-small" title="Fit to width">⤢ Fit</button>
          <button id="tarch-fullscreen" class="btn-small" title="Open the diagram in fullscreen">⛶ Fullscreen</button>
        </span>
      </div>
      <div id="tarch-diagram-wrap" style="padding:14px;background:#0a0e1a;overflow:auto;max-height:520px;">
        <pre class="mermaid" id="tarch-mermaid" style="background:transparent;margin:0;min-height:300px;transform-origin:top left;">${this._buildMermaid(r)}</pre>
      </div>
    </div>`;

    // ── Summary stat cards
    html += `<div style="display:grid;grid-template-columns:repeat(auto-fit,minmax(160px,1fr));gap:10px;margin-bottom:16px;">`;
    html += this._statCard('Total programs', r.summary.totalPrograms, '#3b82f6');
    html += this._statCard('Copybooks', r.summary.totalCopybooks, '#ec4899');
    for (const [sKey, cfg] of Object.entries(S)) {
      const cnt = r.summary.byStrategy[sKey] || 0;
      if (cnt === 0) continue;
      html += this._statCard(`${cfg.icon} ${cfg.label}`, cnt, cfg.color, cfg.description);
    }
    html += `</div>`;

    // ── Layer cards (compact)
    html += `<h3 style="margin:14px 0 8px;font-size:15px;color:#93c5fd;">🏛️ Architecture Layers</h3>`;
    html += `<div style="font-size:11px;color:#64748b;margin-bottom:10px;">Click a component to filter the mapping below to programs targeting it.</div>`;

    for (const layer of A.layers) {
      html += `<div style="margin-bottom:10px;border:1px solid ${layer.border};border-radius:8px;overflow:hidden;">
        <div style="background:${layer.color};padding:6px 12px;display:flex;align-items:center;gap:8px;">
          <span style="font-size:14px;">${layer.icon}</span>
          <span style="font-weight:700;color:#e2e8f0;font-size:13px;">${this._esc(layer.name)}</span>
        </div>
        <div style="display:flex;flex-wrap:wrap;gap:0;background:#0f172a;">`;

      for (const c of layer.components) {
        const count = r.summary.byComponent[c.id] || 0;
        const selected = this.selectedComponent === c.id;
        html += `<div data-component="${this._esc(c.id)}" class="tarch-component" style="
            min-width:220px;flex:1;border-right:1px solid #1e293b;border-bottom:1px solid #1e293b;
            padding:8px 12px;cursor:pointer;transition:background .15s;
            background:${selected ? 'rgba(59,130,246,0.15)' : 'transparent'};
          ">
          <div style="display:flex;align-items:center;gap:6px;">
            <span style="font-weight:600;color:#e2e8f0;font-size:12px;">${this._esc(c.name)}</span>
            <span style="margin-left:auto;font-size:11px;font-weight:700;color:${count > 0 ? '#10b981' : '#475569'};">
              ${count}
            </span>
          </div>
          <div style="font-size:10px;color:#64748b;margin-top:2px;">${this._esc(c.tech)}</div>
        </div>`;
      }
      html += `</div></div>`;
    }

    // ── Source → Target mapping (grouped, collapsible)
    html += `<div style="display:flex;justify-content:space-between;align-items:center;margin:18px 0 8px;">
      <h3 style="margin:0;font-size:15px;color:#93c5fd;">📋 Source → Target Mapping</h3>
      <div style="display:flex;gap:6px;">
        <button id="tarch-expand-all" class="btn-small" title="Expand all groups">▼ Expand all</button>
        <button id="tarch-collapse-all" class="btn-small" title="Collapse all groups">▲ Collapse all</button>
      </div>
    </div>`;
    if (this.selectedComponent) {
      const meta = this._componentMeta(this.selectedComponent);
      html += `<div style="font-size:11px;color:#94a3b8;margin-bottom:8px;">
        Filtered to <strong style="color:#cbd5e1;">${this._esc(meta.name)}</strong> ·
        <a href="#" id="tarch-clear-filter" style="color:#3b82f6;">clear filter</a>
      </div>`;
    }

    const rows = this.selectedComponent
      ? r.programMappings.filter(m => m.recommendation.targetComponent === this.selectedComponent)
      : r.programMappings;

    // Group by (wave, targetComponent, strategy) so identical rationale rows
    // collapse into a single header that the user can expand on demand.
    const groups = new Map();
    for (const m of rows) {
      const rec = m.recommendation;
      const key = `${rec.wave}|${rec.targetComponent}|${rec.strategy}`;
      if (!groups.has(key)) {
        groups.set(key, {
          wave: rec.wave,
          targetComponent: rec.targetComponent,
          targetComponentName: rec.targetComponentName,
          targetLayer: rec.targetLayer,
          strategy: rec.strategy,
          rationale: rec.rationale,
          items: [],
        });
      }
      groups.get(key).items.push(m);
    }
    const sortedGroups = [...groups.values()].sort((a, b) => {
      if (a.wave !== b.wave) return a.wave - b.wave;
      if (a.targetComponent !== b.targetComponent) return a.targetComponent.localeCompare(b.targetComponent);
      return a.strategy.localeCompare(b.strategy);
    });

    html += `<div style="display:flex;flex-direction:column;gap:6px;">`;
    for (const g of sortedGroups) {
      const sCfg = S[g.strategy] || { color:'#475569', icon:'?', label:g.strategy };
      const wColors = { 1:'#10b981', 2:'#f59e0b', 3:'#ef4444' };
      const groupKey = `${g.wave}|${g.targetComponent}|${g.strategy}`;
      const expanded = this._isGroupExpanded(groupKey, g.items.length);
      html += `<div data-group-key="${this._esc(groupKey)}" style="border:1px solid #1e293b;border-radius:6px;background:#0f172a;overflow:hidden;">
        <div class="tarch-group-header" data-group-key="${this._esc(groupKey)}" style="
            padding:8px 12px;display:flex;align-items:center;gap:10px;cursor:pointer;
            background:${expanded ? '#1e293b' : '#0f172a'};transition:background .15s;
            border-bottom:${expanded ? '1px solid #1e293b' : '0'};">
          <span style="font-size:11px;width:14px;color:#94a3b8;">${expanded ? '▼' : '▶'}</span>
          <span style="padding:1px 7px;border-radius:8px;background:${wColors[g.wave]};color:#0a0e1a;font-weight:700;font-size:10px;">W${g.wave}</span>
          <span style="padding:1px 7px;border-radius:8px;background:${sCfg.color};color:#0a0e1a;font-weight:700;font-size:10px;">${sCfg.icon} ${sCfg.label}</span>
          <span style="font-size:12px;color:#cbd5e1;font-weight:600;">${this._esc(g.targetComponentName)}</span>
          <span style="font-size:10px;color:#64748b;">${this._esc(g.targetLayer)}</span>
          <span style="margin-left:auto;font-size:11px;color:#94a3b8;font-weight:600;">${g.items.length} item${g.items.length === 1 ? '' : 's'}</span>
        </div>`;
      if (expanded) {
        // Compact chip grid for the group's programs. Rationale is shown once
        // at the top of the group instead of being repeated per row.
        html += `<div style="padding:10px 12px;">
          <div style="font-size:11px;color:#94a3b8;line-height:1.5;margin-bottom:8px;font-style:italic;">${this._glossaryHtml(g.rationale)}</div>
          <div style="display:flex;flex-wrap:wrap;gap:4px;">`;
        // Sort items by complexity desc then name for usefulness
        const sortedItems = [...g.items].sort((a, b) => {
          const c = b.recommendation.complexity - a.recommendation.complexity;
          if (c !== 0) return c;
          return a.displayName.localeCompare(b.displayName);
        });
        for (const m of sortedItems) {
          const cmplx = m.recommendation.complexity;
          const cmplxColor = cmplx > 0.6 ? '#ef4444' : cmplx > 0.3 ? '#f59e0b' : '#10b981';
          const metrics = m.metrics;
          const tip = [
            `${m.displayName}${m.isCopybook ? ' (copybook)' : ''}`,
            `LOC: ${metrics.lineCount}  ·  SQL: ${metrics.sqlCount}  ·  CALLs: ${metrics.callCount}`,
            `Sections: ${metrics.sectionCount}  ·  Paragraphs: ${metrics.paraCount}  ·  Branches: ${metrics.branchCount}`,
            `Complexity: ${cmplx.toFixed(2)}`,
            '',
            'Migration notes:',
            ...m.recommendation.migrationNotes.map(n => '• ' + n),
          ].join('\n');
          html += `<span title="${this._escAttr(tip)}" style="
            display:inline-flex;align-items:center;gap:4px;
            padding:3px 8px;background:#1e293b;border:1px solid #334155;
            border-left:2px solid ${cmplxColor};
            border-radius:10px;font-size:11px;color:#cbd5e1;font-family:monospace;cursor:help;">
            ${m.isCopybook ? '<span style="color:#ec4899;font-size:9px;">CPY</span> ' : ''}${this._esc(m.displayName)}
            <span style="font-size:9px;color:#64748b;">${metrics.lineCount}L</span>
          </span>`;
        }
        html += `</div></div>`;
      }
      html += `</div>`;
    }
    if (sortedGroups.length === 0) {
      html += `<div style="padding:16px;text-align:center;color:#64748b;background:#0f172a;border:1px solid #1e293b;border-radius:6px;">No programs mapped to this component.</div>`;
    }
    html += `</div>`;

    // ── AI agent integration note
    html += `<div style="margin-top:18px;padding:12px 14px;background:#1e1b4b;border:1px solid #6366f1;border-radius:6px;color:#c7d2fe;font-size:11px;line-height:1.5;">
      <strong>🤖 For AI conversion agents:</strong>
      The "Save for AI agent" button persists this plan to <code>output/rekt/target-architecture.json</code>.
      The JSON includes every program's recommended <code>targetComponent</code>, <code>targetTech</code>, <code>strategy</code>,
      <code>migrationNotes</code> and the complete architecture template so an agent can convert each program with the right
      context without re-deriving the plan. See <code>docs/target-architecture-recommendation.md</code> for the schema.
    </div>`;

    html += `</div>`;
    root.innerHTML = html;

    // Render the mermaid diagram (must run after the <pre class="mermaid"> is in the DOM).
    if (typeof mermaid !== 'undefined') {
      try {
        mermaid.run({ nodes: root.querySelectorAll('#tarch-mermaid') }).then(() => {
          // Mermaid sets max-width on its rendered SVG which clamps the diagram
          // to its layout width — that's why it looked tiny. Strip the cap so it
          // honours the parent + our zoom transform.
          const svg = root.querySelector('#tarch-mermaid svg');
          if (svg) {
            svg.style.maxWidth = 'none';
            svg.style.width = '100%';
            svg.style.height = 'auto';
            // Cache the natural dimensions so Fit can restore them.
            const vb = svg.viewBox?.baseVal;
            if (vb) {
              this._diagramNaturalWidth = vb.width;
              this._diagramNaturalHeight = vb.height;
            }
            this._applyDiagramZoom();
          }
        });
      } catch (e) { console.warn('Mermaid render failed:', e); }
    }

    // ── Wire interactions
    root.querySelectorAll('.tarch-component').forEach(el => {
      el.addEventListener('click', () => {
        const id = el.dataset.component;
        this.selectedComponent = this.selectedComponent === id ? null : id;
        this._render();
      });
    });
    root.querySelectorAll('.tarch-group-header').forEach(el => {
      el.addEventListener('click', () => {
        const key = el.dataset.groupKey;
        // Look up the current effective expansion for this specific group
        // (respecting size-based defaults), then flip it.
        const grp = sortedGroups.find(g => `${g.wave}|${g.targetComponent}|${g.strategy}` === key);
        const cur = grp ? this._isGroupExpanded(key, grp.items.length) : true;
        this._groupOverrides.set(key, !cur);
        this._render();
      });
    });
    root.querySelector('#tarch-clear-filter')?.addEventListener('click', (e) => {
      e.preventDefault();
      this.selectedComponent = null;
      this._render();
    });
    root.querySelector('#tarch-save')?.addEventListener('click', () => this._save());
    root.querySelector('#tarch-download')?.addEventListener('click', () => this._download());
    root.querySelector('#tarch-refresh')?.addEventListener('click', () => this.loadAndRender());

    // Diagram zoom & fullscreen controls
    root.querySelector('#tarch-zoom-in')?.addEventListener('click', () => {
      this._diagramZoom = Math.min(4, this._diagramZoom * 1.25);
      this._applyDiagramZoom();
    });
    root.querySelector('#tarch-zoom-out')?.addEventListener('click', () => {
      this._diagramZoom = Math.max(0.5, this._diagramZoom / 1.25);
      this._applyDiagramZoom();
    });
    root.querySelector('#tarch-fit')?.addEventListener('click', () => {
      this._diagramZoom = 1;
      this._applyDiagramZoom();
    });
    root.querySelector('#tarch-fullscreen')?.addEventListener('click', () => this._toggleDiagramFullscreen());
    root.querySelector('#tarch-show-all')?.addEventListener('change', (e) => {
      this._diagramShowAll = !!e.target.checked;
      this._render();
    });
    // Restore fullscreen across re-renders if the user enabled it earlier.
    if (this._diagramFullscreen) this._enterDiagramFullscreen();
    root.querySelector('#tarch-expand-all')?.addEventListener('click', () => {
      for (const g of sortedGroups) {
        this._groupOverrides.set(`${g.wave}|${g.targetComponent}|${g.strategy}`, true);
      }
      this._render();
    });
    root.querySelector('#tarch-collapse-all')?.addEventListener('click', () => {
      for (const g of sortedGroups) {
        this._groupOverrides.set(`${g.wave}|${g.targetComponent}|${g.strategy}`, false);
      }
      this._render();
    });

    // Restore previous save status across re-renders so the user still sees the
    // confirmation after clicking through component filters.
    if (this._lastSavedAt) {
      const el = document.getElementById('tarch-save-status');
      if (el) el.innerHTML = `<span style="color:#10b981;">✓ Saved to output/rekt/target-architecture.json at ${new Date(this._lastSavedAt).toLocaleTimeString()}</span>`;
    }
  }

  // Default-collapse very large groups (e.g. all 20+ copybooks → shared lib).
  // The user can click the header to expand them.
  _isGroupExpanded(key, itemCount) {
    if (this._groupOverrides.has(key)) return this._groupOverrides.get(key);
    // Auto-collapse big homogeneous groups (>8 items) by default to keep the
    // landing view skimmable; the user expands them on demand.
    if (itemCount > 8) return false;
    return true;
  }

  _buildMermaid(r) {
    // Tailor the diagram to the actual scan: include only components that have
    // mapped programs, plus the recommended downstream dependencies they need
    // (transitive closure over `consumes`). Empty layers are dropped entirely.
    // The user can flip a switch to see the full template instead.
    const A = TargetArchitectureView.ARCHITECTURE;
    const showAll = !!this._diagramShowAll;

    // Index components by id and compute the kept set.
    const allComponents = new Map();
    for (const layer of A.layers) {
      for (const c of layer.components) allComponents.set(c.id, { ...c, layerName: layer.name });
    }

    let keep;
    if (showAll) {
      keep = new Set(allComponents.keys());
    } else {
      // Seed with components that have ≥1 program.
      keep = new Set();
      for (const [id, _] of allComponents) {
        if ((r.summary.byComponent[id] || 0) > 0) keep.add(id);
      }
      // Transitive closure over `consumes` so dependencies (DB, gateway, …) of
      // seeded components are also kept. Anything they consume is part of the
      // recommendation even if no program maps to it directly.
      let changed = true;
      while (changed) {
        changed = false;
        for (const id of [...keep]) {
          const c = allComponents.get(id);
          for (const dep of (c?.consumes || [])) {
            if (!keep.has(dep) && allComponents.has(dep)) { keep.add(dep); changed = true; }
          }
        }
      }
      // Fallback: if nothing was scanned yet, keep the full template so we
      // don't render an empty diagram on first load.
      if (keep.size === 0) keep = new Set(allComponents.keys());
    }

    const lines = ['flowchart TB'];

    // Render each layer (only if it has any surviving components)
    for (const layer of A.layers) {
      const kept = layer.components.filter(c => keep.has(c.id));
      if (kept.length === 0) continue;
      const subId = 'L_' + layer.name.replace(/\W+/g, '_');
      lines.push(`  subgraph ${subId}["${layer.icon} ${layer.name}"]`);
      for (const c of kept) {
        const count = r.summary.byComponent[c.id] || 0;
        let open = '[', close = ']';
        if (c.type === 'database') { open = '[('; close = ')]'; }
        else if (c.type === 'storage') { open = '[/'; close = '/]'; }
        else if (c.type === 'gateway' || c.type === 'frontend') { open = '(['; close = '])'; }
        else if (c.type === 'infrastructure') { open = '{{'; close = '}}'; }
        // Show every recommended tech alternative on its own line so users can
        // see all the deployment options at a glance (e.g. S3 · Azure Blob · GCS).
        // Mermaid renders <br/> inside double-quoted labels.
        const techHtml = c.tech
          .split('/')
          .map(s => s.trim())
          .filter(Boolean)
          .map(s => `<i>${s}</i>`)
          .join('<br/>');
        const label = `${c.name}<br/>${techHtml}<br/><b>${count} prog${count===1?'':'s'}</b>`;
        lines.push(`    ${c.id}${open}"${label}"${close}`);
      }
      lines.push('  end');
    }

    // Edges — only between surviving components.
    const seen = new Set();
    for (const c of allComponents.values()) {
      if (!keep.has(c.id)) continue;
      for (const tgt of (c.consumes || [])) {
        if (!keep.has(tgt)) continue;
        const key = `${c.id}->${tgt}`;
        if (seen.has(key)) continue;
        seen.add(key);
        lines.push(`  ${c.id} --> ${tgt}`);
      }
    }

    // Highlight components that actually receive mapped programs vs the
    // transitively-included infrastructure ones.
    for (const id of keep) {
      const count = r.summary.byComponent[id] || 0;
      if (count > 0) {
        lines.push(`  style ${id} fill:#065f46,stroke:#10b981,stroke-width:2px,color:#e2e8f0`);
      } else {
        lines.push(`  style ${id} fill:#1e293b,stroke:#475569,stroke-width:1px,color:#94a3b8`);
      }
    }

    return lines.join('\n');
  }

  _applyDiagramZoom() {
    const wrap = document.getElementById('tarch-diagram-wrap');
    const pre  = document.getElementById('tarch-mermaid');
    if (!pre) return;
    pre.style.transform = `scale(${this._diagramZoom})`;
    // Reserve space for the scaled diagram so it doesn't get clipped by the
    // wrapper (transform doesn't grow the layout box on its own).
    if (this._diagramNaturalWidth && this._diagramNaturalHeight) {
      pre.style.width  = `${this._diagramNaturalWidth}px`;
      pre.style.height = `${this._diagramNaturalHeight * this._diagramZoom}px`;
    }
    // Update the Fit-button label so the user can see current zoom.
    const fitBtn = document.getElementById('tarch-fit');
    if (fitBtn) fitBtn.textContent = `⤢ ${Math.round(this._diagramZoom * 100)}%`;
  }

  _toggleDiagramFullscreen() {
    if (this._diagramFullscreen) this._exitDiagramFullscreen();
    else this._enterDiagramFullscreen();
  }

  _enterDiagramFullscreen() {
    // Drop any existing overlay before creating a new one.
    document.getElementById('tarch-fullscreen-overlay')?.remove();
    const overlay = document.createElement('div');
    overlay.id = 'tarch-fullscreen-overlay';
    overlay.style.cssText = `position:fixed;inset:0;background:rgba(3,7,18,0.96);z-index:9999;display:flex;flex-direction:column;`;

    // Toolbar
    const bar = document.createElement('div');
    bar.style.cssText = 'padding:10px 16px;display:flex;align-items:center;gap:10px;border-bottom:1px solid #1e293b;background:#0f172a;';
    bar.innerHTML = `
      <span style="font-size:14px;font-weight:600;color:#93c5fd;">🏗️ Target Architecture · Fullscreen</span>
      <span style="font-size:11px;color:#64748b;">drag-scroll · scroll-wheel to pan · use + / − to zoom</span>
      <span style="margin-left:auto;display:flex;gap:6px;">
        <button id="tarch-fs-zoom-out" class="btn-small">−</button>
        <button id="tarch-fs-zoom-in"  class="btn-small">＋</button>
        <button id="tarch-fs-fit"      class="btn-small">⤢ Fit</button>
        <button id="tarch-fs-close"    class="btn-small">✕ Close (Esc)</button>
      </span>`;
    overlay.appendChild(bar);

    // Mermaid diagram container — give it the full remaining viewport, with
    // both scrollbars enabled so any zoom level is browseable.
    const wrap = document.createElement('div');
    wrap.id = 'tarch-fs-wrap';
    wrap.style.cssText = 'flex:1;overflow:auto;padding:24px;background:#0a0e1a;';
    const pre = document.createElement('pre');
    pre.className = 'mermaid';
    pre.id = 'tarch-fs-mermaid';
    pre.style.cssText = 'background:transparent;margin:0;transform-origin:top left;';
    pre.textContent = this._buildMermaid(this.recommendations);
    wrap.appendChild(pre);
    overlay.appendChild(wrap);
    document.body.appendChild(overlay);

    this._diagramFullscreen = true;
    // Use a separate zoom for fullscreen so the inline diagram zoom isn't disturbed.
    this._fsZoom = 1.2;

    // Render & strip max-width on the fullscreen SVG too.
    if (typeof mermaid !== 'undefined') {
      mermaid.run({ nodes: [pre] }).then(() => {
        const svg = pre.querySelector('svg');
        if (svg) {
          svg.style.maxWidth = 'none';
          svg.style.width = '100%';
          svg.style.height = 'auto';
          const vb = svg.viewBox?.baseVal;
          if (vb) {
            this._fsNaturalW = vb.width;
            this._fsNaturalH = vb.height;
          }
          this._applyFsZoom();
        }
      });
    }

    // Wire toolbar
    document.getElementById('tarch-fs-zoom-in') ?.addEventListener('click', () => { this._fsZoom = Math.min(6, this._fsZoom * 1.25); this._applyFsZoom(); });
    document.getElementById('tarch-fs-zoom-out')?.addEventListener('click', () => { this._fsZoom = Math.max(0.5, this._fsZoom / 1.25); this._applyFsZoom(); });
    document.getElementById('tarch-fs-fit')    ?.addEventListener('click', () => { this._fsZoom = 1.2; this._applyFsZoom(); });
    document.getElementById('tarch-fs-close')  ?.addEventListener('click', () => this._exitDiagramFullscreen());

    // Esc to close
    this._fsKeyHandler = (e) => { if (e.key === 'Escape') this._exitDiagramFullscreen(); };
    document.addEventListener('keydown', this._fsKeyHandler);
  }

  _exitDiagramFullscreen() {
    document.getElementById('tarch-fullscreen-overlay')?.remove();
    if (this._fsKeyHandler) {
      document.removeEventListener('keydown', this._fsKeyHandler);
      this._fsKeyHandler = null;
    }
    this._diagramFullscreen = false;
  }

  _applyFsZoom() {
    const pre = document.getElementById('tarch-fs-mermaid');
    if (!pre) return;
    pre.style.transform = `scale(${this._fsZoom})`;
    if (this._fsNaturalW && this._fsNaturalH) {
      pre.style.width  = `${this._fsNaturalW}px`;
      pre.style.height = `${this._fsNaturalH * this._fsZoom}px`;
    }
    const fitBtn = document.getElementById('tarch-fs-fit');
    if (fitBtn) fitBtn.textContent = `⤢ ${Math.round(this._fsZoom * 100)}%`;
  }

  _statCard(label, value, color, tooltip) {
    return `<div title="${this._escAttr(tooltip || '')}" style="background:#1e293b;border:1px solid #334155;border-left:3px solid ${color};border-radius:6px;padding:10px 12px;">
      <div style="font-size:11px;color:#94a3b8;text-transform:uppercase;letter-spacing:.04em;">${this._esc(label)}</div>
      <div style="font-size:20px;font-weight:700;color:#f1f5f9;margin-top:2px;">${value}</div>
    </div>`;
  }

  // ─── Persistence ────────────────────────────────────────────────────
  async _save() {
    const statusEl = document.getElementById('tarch-save-status');
    if (statusEl) statusEl.innerHTML = '<span style="color:#94a3b8;">Saving…</span>';
    try {
      const resp = await fetch('/api/graph/rekt/target-architecture', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(this.recommendations),
      });
      if (!resp.ok) throw new Error(`save failed: ${resp.status}`);
      const result = await resp.json();
      this._lastSavedAt = new Date().toISOString();
      if (statusEl) statusEl.innerHTML = `<span style="color:#10b981;">✓ Saved to ${this._esc(result.path || 'output/rekt/target-architecture.json')}</span>`;
    } catch (e) {
      if (statusEl) statusEl.innerHTML = `<span style="color:#ef4444;">✗ Save failed: ${this._esc(e.message)}</span>`;
    }
  }

  _download() {
    const blob = new Blob([JSON.stringify(this.recommendations, null, 2)], { type: 'application/json' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = `target-architecture-${this.scanRunId || 'latest'}.json`;
    a.click();
    URL.revokeObjectURL(url);
  }

  _esc(s)     { return (s == null ? '' : String(s)).replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;'); }
  _escAttr(s) { return (s == null ? '' : String(s)).replace(/&/g,'&amp;').replace(/"/g,'&quot;').replace(/'/g,'&#39;'); }
}

window.TargetArchitectureView = TargetArchitectureView;
