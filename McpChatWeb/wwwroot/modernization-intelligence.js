// ─────────────────────────────────────────────────────────────────────────
// Modernization Intelligence — Phase-1 portal workspace
// ─────────────────────────────────────────────────────────────────────────
// Two views shipping in this phase:
//   1. Modernization Dashboard — operational cockpit (cache hit rate,
//      compile success, projection reduction, LLM outcomes)
//   2. Application Explorer — per-program inventory with modernization
//      status, dependency counts, latest run + quality result
//
// Read-only over /api/modernization/applications and
// /api/modernization/dashboard. See
// McpChatWeb/Services/ModernizationIntelligenceService.cs for the backend.
//
// Future phases (per docs/modernization-intelligence-portal-design.md):
//   - Runtime & Conversion Intelligence (timelines per run)
//   - Dependency Topology (semantic overlays on existing Neo4j graph)
//   - Semantic Flow Explorer (PERFORM chains, transaction flows)
// ─────────────────────────────────────────────────────────────────────────

class ModernizationIntelligenceView {
  constructor(rootId) {
    this.root = document.getElementById(rootId);
    if (!this.root) return;
    this._activeSubview = 'dashboard';
    this._renderShell();
  }

  _renderShell() {
    this.root.innerHTML = `
      <div class="mi-shell">
        <div class="mi-header">
          <div class="mi-title">
            <span class="mi-icon">🧭</span>
            <div>
              <div class="mi-title-main">Modernization Intelligence</div>
              <div class="mi-title-sub">Portfolio-centric workspace · reuses existing REKT graph, MetricsSink telemetry, projection cache</div>
            </div>
          </div>
          <div class="mi-actions">
            <button id="mi-refresh" class="mi-btn" title="Re-fetch from /api/modernization/*">⟳ Refresh</button>
          </div>
        </div>
        <div class="mi-subnav">
          <button class="mi-subtab mi-subtab-active" data-sub="dashboard">📊 Modernization Dashboard</button>
          <button class="mi-subtab" data-sub="applications">📚 Application Explorer</button>
          <button class="mi-subtab" data-sub="runtime">⏱ Runtime &amp; Conversion Intelligence</button>
          <button class="mi-subtab" data-sub="topology">🕸 Dependency Topology</button>
          <button class="mi-subtab mi-subtab-disabled" disabled title="Coming in Phase-1 PR-P3 follow-up">🌊 Semantic Flow Explorer</button>
        </div>
        <div id="mi-body" class="mi-body"></div>
      </div>
    `;
    this.root.querySelectorAll('.mi-subtab').forEach(btn => {
      if (btn.disabled) return;
      btn.addEventListener('click', () => {
        this._activeSubview = btn.dataset.sub;
        this.root.querySelectorAll('.mi-subtab').forEach(b =>
          b.classList.toggle('mi-subtab-active', b.dataset.sub === this._activeSubview));
        this._renderActive();
      });
    });
    const refresh = this.root.querySelector('#mi-refresh');
    if (refresh) refresh.addEventListener('click', () => this.loadAndRender());
  }

  async loadAndRender() {
    this._renderActive();
  }

  async _renderActive() {
    const body = this.root.querySelector('#mi-body');
    body.innerHTML = '<div class="mi-loading">Loading…</div>';
    try {
      if (this._activeSubview === 'dashboard') {
        const data = await fetch('/api/modernization/dashboard').then(r => r.json());
        body.innerHTML = this._renderDashboard(data);
      } else if (this._activeSubview === 'applications') {
        const data = await fetch('/api/modernization/applications').then(r => r.json());
        body.innerHTML = this._renderApplications(data);
      } else if (this._activeSubview === 'runtime') {
        const runs = await fetch('/api/modernization/runs?limit=50').then(r => r.json());
        body.innerHTML = this._renderRuntimeShell(runs);
        // Auto-select most recent run
        if (runs.length > 0) await this._loadRunTimeline(runs[0].runId);
      } else if (this._activeSubview === 'topology') {
        const [topology, services] = await Promise.all([
          fetch('/api/modernization/topology').then(r => r.json()),
          fetch('/api/graph/rekt/services').then(r => r.json()).catch(() => ({ nodes: [], edges: [] })),
        ]);
        body.innerHTML = this._renderTopology(topology, services);
        this._wireTopologyInteractions(topology, services);
      }
    } catch (err) {
      body.innerHTML = `<div class="mi-error">Failed to load: ${this._escape(err.message)}</div>`;
    }
  }

  // ────────────────────────────────────────────────────────────────────
  // Dashboard renderer
  // ────────────────────────────────────────────────────────────────────
  _renderDashboard(d) {
    if (d.note) {
      return `<div class="mi-empty">${this._escape(d.note)}<br><br>
        <code>python3 tools/ingest-metrics.py --rebuild</code> populates the analytics DB after any conversion run.</div>`;
    }
    const kpi = (label, value, sub, color) => `
      <div class="mi-kpi" style="border-left-color:${color || '#475569'};">
        <div class="mi-kpi-value">${value}</div>
        <div class="mi-kpi-label">${label}</div>
        <div class="mi-kpi-sub">${sub || ''}</div>
      </div>`;

    const headlineKpis = `
      <div class="mi-kpi-row">
        ${kpi('Cache hit rate',
              `${d.cacheHitRatePct ?? 0}%`,
              `PR6 projection-block cache · ${(d.cacheDecisionCounts && this._countAll(d.cacheDecisionCounts)) || 0} decisions`,
              '#10b981')}
        ${kpi('Avg ctx reduction',
              `${d.avgContextReductionPct ?? 0}%`,
              `PR4 projection · ${(d.contextReduction || []).length} programs measured`,
              '#3b82f6')}
        ${kpi('LLM success',
              `${d.llmSuccessRatePct ?? 0}%`,
              `${this._sumOutcome(d, 'success')}/${(d.llmCallOutcomes || []).reduce((a,o)=>a+o.count,0)} calls`,
              '#8b5cf6')}
        ${kpi('Compile pass',
              `${d.recentCompileSuccessPct ?? 0}%`,
              `last ${(d.recentQuality || []).length} quality gates`,
              (d.recentCompileSuccessPct ?? 0) > 50 ? '#10b981' : '#ef4444')}
        ${kpi('Total events',
              `${d.totalEvents ?? 0}`,
              `across all event types`,
              '#94a3b8')}
      </div>`;

    const reductionRows = (d.contextReduction || []).map(r => `
      <tr>
        <td><code>${this._escape(r.file)}</code></td>
        <td class="num">${Math.round(r.rawTokens)}</td>
        <td class="num">${Math.round(r.projectionTokens)}</td>
        <td class="num"><b>${r.reductionPct.toFixed(1)}%</b></td>
      </tr>`).join('') || '<tr><td colspan="4" class="mi-muted">no projection_metrics yet</td></tr>';

    const llmRows = (d.llmCallOutcomes || []).map(o => `
      <tr>
        <td>${this._outcomeBadge(o.outcome)}</td>
        <td class="num">${o.count}</td>
        <td class="num">${Math.round(o.avgDurationMs)}ms</td>
        <td class="num">${Math.round(o.avgCompletionTokens)}</td>
      </tr>`).join('') || '<tr><td colspan="4" class="mi-muted">no llm_call events yet</td></tr>';

    const cacheRows = Object.entries(d.cacheDecisionCounts || {}).map(([dec, n]) => `
      <tr>
        <td>${this._cacheBadge(dec)}</td>
        <td class="num">${n}</td>
      </tr>`).join('') || '<tr><td colspan="2" class="mi-muted">no cache_event yet</td></tr>';

    const qualityRows = (d.recentQuality || []).map(q => `
      <tr>
        <td><code>${this._escape(q.runId)}</code></td>
        <td>${q.compileSuccess ? '<span class="mi-ok">✅ pass</span>' : '<span class="mi-bad">❌ fail</span>'}</td>
        <td class="num">${q.compileErrors}</td>
        <td class="num">${q.generatedClasses}</td>
        <td class="num">${q.generatedLines}</td>
        <td class="num">${q.fallbackClasses}</td>
        <td class="num">${q.injectAnnotations}</td>
      </tr>`).join('') || '<tr><td colspan="7" class="mi-muted">no quality_metrics yet</td></tr>';

    return `
      ${headlineKpis}
      <div class="mi-grid">
        <div class="mi-card">
          <h3>Context-token reduction (PR4)</h3>
          <table class="mi-table">
            <thead><tr><th>File</th><th>Raw REKT</th><th>Projection</th><th>Δ</th></tr></thead>
            <tbody>${reductionRows}</tbody>
          </table>
        </div>
        <div class="mi-card">
          <h3>LLM call outcomes</h3>
          <table class="mi-table">
            <thead><tr><th>Outcome</th><th>Count</th><th>Avg duration</th><th>Avg tokens</th></tr></thead>
            <tbody>${llmRows}</tbody>
          </table>
        </div>
        <div class="mi-card">
          <h3>Projection-block cache (PR6)</h3>
          <table class="mi-table">
            <thead><tr><th>Decision</th><th>Count</th></tr></thead>
            <tbody>${cacheRows}</tbody>
          </table>
        </div>
        <div class="mi-card mi-card-wide">
          <h3>Quality gates (last 5 runs)</h3>
          <table class="mi-table">
            <thead><tr><th>Run</th><th>Compile</th><th>Errors</th><th>Classes</th><th>Lines</th><th>Fallback</th><th>@Inject</th></tr></thead>
            <tbody>${qualityRows}</tbody>
          </table>
        </div>
      </div>
      <div class="mi-source">Source: <code>${this._escape(d.source || '')}</code> · re-build with <code>python3 tools/ingest-metrics.py --rebuild</code></div>
    `;
  }

  _sumOutcome(d, name) {
    return (d.llmCallOutcomes || []).filter(o => o.outcome === name).reduce((a, o) => a + o.count, 0);
  }
  _countAll(obj) { return Object.values(obj).reduce((a, b) => a + b, 0); }

  // ────────────────────────────────────────────────────────────────────
  // Application Explorer renderer
  // ────────────────────────────────────────────────────────────────────
  _renderApplications(rows) {
    if (!rows || rows.length === 0) {
      return `<div class="mi-empty">No COBOL programs found under <code>source/</code>.</div>`;
    }

    // Aggregate KPIs
    const total = rows.length;
    const withFacts = rows.filter(r => r.hasFacts).length;
    const converted = rows.filter(r => r.latestRunId).length;
    const verified = rows.filter(r => r.latestCompileSuccess === true).length;
    const totalLoc = rows.reduce((a, r) => a + (r.linesOfCode || 0), 0);
    const cacheHitting = rows.filter(r => r.projectionCacheHits > 0).length;

    const kpi = (label, val, sub) =>
      `<div class="mi-kpi"><div class="mi-kpi-value">${val}</div><div class="mi-kpi-label">${label}</div><div class="mi-kpi-sub">${sub || ''}</div></div>`;

    const head = `
      <div class="mi-kpi-row">
        ${kpi('Programs', total, 'in source/ corpus')}
        ${kpi('Total LoC', totalLoc.toLocaleString(), 'across all programs')}
        ${kpi('REKT-ready', `${withFacts}/${total}`, 'have .facts.json')}
        ${kpi('Converted', `${converted}/${total}`, 'have a completed run')}
        ${kpi('Verified', `${verified}/${total}`, 'compile-gate pass')}
        ${kpi('Cache-hot', `${cacheHitting}/${total}`, 'projection cache hits > 0')}
      </div>
    `;

    // Sort: largest LoC first (typically what users want to triage)
    const sorted = [...rows].sort((a, b) => (b.linesOfCode || 0) - (a.linesOfCode || 0));

    const tableRows = sorted.map(r => {
      const statusClass = `mi-status-${r.modernizationStatus}`;
      const compileCell = r.latestCompileSuccess === true
        ? '<span class="mi-ok">✅</span>'
        : r.latestCompileSuccess === false
          ? `<span class="mi-bad">❌ ${r.latestCompileErrors ?? '?'} err</span>`
          : '<span class="mi-muted">—</span>';
      return `
        <tr>
          <td>
            <div><b>${this._escape(r.basename)}</b></div>
            <div class="mi-path">${this._escape(r.relativePath)}</div>
          </td>
          <td class="num">${r.linesOfCode.toLocaleString()}</td>
          <td>${r.hasFacts ? `<span class="mi-ok">✓</span> <span class="mi-muted">conf=${r.factsConfidence}</span>` : '<span class="mi-muted">—</span>'}</td>
          <td class="num">${r.dependencyCount || 0}</td>
          <td class="num">${r.factsWarnings || 0}</td>
          <td>${r.latestRunId ? `<a href="#" onclick="event.preventDefault();">#${r.latestRunId}</a>` : '<span class="mi-muted">—</span>'}</td>
          <td>${compileCell}</td>
          <td class="num">${r.latestGeneratedClasses ?? '—'}</td>
          <td class="num">${r.latestGeneratedLines ? r.latestGeneratedLines.toLocaleString() : '—'}</td>
          <td class="num">${r.projectionCacheHits || 0}</td>
          <td><span class="mi-status ${statusClass}">${this._statusLabel(r.modernizationStatus)}</span></td>
        </tr>`;
    }).join('');

    return `
      ${head}
      <div class="mi-card mi-card-wide">
        <h3>Application inventory · ${total} programs</h3>
        <table class="mi-table mi-table-dense">
          <thead>
            <tr>
              <th>Program</th>
              <th>LoC</th>
              <th>REKT facts</th>
              <th>Deps</th>
              <th>Warn</th>
              <th>Last run</th>
              <th>Compile</th>
              <th>Classes</th>
              <th>Java LoC</th>
              <th>Cache hits</th>
              <th>Status</th>
            </tr>
          </thead>
          <tbody>${tableRows}</tbody>
        </table>
      </div>
      <div class="mi-source">Sourced from <code>source/</code> + <code>Data/migration.db</code> + <code>Data/benchmark.db</code> + <code>Data/projection-cache.db</code> + <code>output/rekt/*.facts.json</code></div>
    `;
  }

  _statusLabel(s) {
    return ({
      'not-started': 'not started',
      'converted': 'converted (no gate)',
      'verified': '✅ verified',
      'partial-fallback': '⚠ partial (fallback)',
      'compile-failing': '❌ compile failing',
    })[s] || s;
  }

  // ────────────────────────────────────────────────────────────────────
  // Runtime & Conversion Intelligence renderer (Phase-1 PR-P2)
  // ────────────────────────────────────────────────────────────────────
  _renderRuntimeShell(runs) {
    if (!runs || runs.length === 0) {
      return `<div class="mi-empty">No runs with telemetry yet.<br><br>Run a conversion (<code>./doctor.sh convert-only --program X</code>) and the timeline will populate from <code>output/.metrics/&lt;runId&gt;.jsonl</code>.</div>`;
    }
    const runRows = runs.map(r => {
      const hitRate = r.cacheTotal > 0 ? `${Math.round(r.cacheHits * 100 / r.cacheTotal)}%` : '—';
      const llmOk = r.llmCallCount > 0 ? `${r.llmSuccess}/${r.llmCallCount}` : '—';
      return `<tr data-run="${this._escape(r.runId)}" class="mi-run-row">
        <td><b>#${this._escape(r.runId)}</b></td>
        <td>${r.eventCount}</td>
        <td>${llmOk}</td>
        <td>${r.projectionEventCount}</td>
        <td>${hitRate}</td>
        <td class="mi-muted">${this._escape((r.firstEventTs || '').substring(0,19))}</td>
      </tr>`;
    }).join('');

    return `
      <div class="mi-runtime-layout">
        <div class="mi-card mi-runs-panel">
          <h3>Recent runs</h3>
          <table class="mi-table mi-table-dense mi-runs-table">
            <thead><tr><th>Run</th><th>Events</th><th>LLM ok</th><th>Proj</th><th>Cache</th><th>Time (UTC)</th></tr></thead>
            <tbody>${runRows}</tbody>
          </table>
        </div>
        <div id="mi-timeline-panel" class="mi-timeline-panel">
          <div class="mi-loading">Select a run on the left.</div>
        </div>
      </div>
    `;
  }

  async _loadRunTimeline(runId) {
    // Highlight selected row
    this.root.querySelectorAll('.mi-run-row').forEach(tr => {
      tr.classList.toggle('mi-run-row-active', tr.dataset.run === runId);
      tr.onclick = () => this._loadRunTimeline(tr.dataset.run);
    });
    const panel = this.root.querySelector('#mi-timeline-panel');
    panel.innerHTML = '<div class="mi-loading">Loading timeline…</div>';
    try {
      const t = await fetch(`/api/modernization/runs/${encodeURIComponent(runId)}/timeline`).then(r => r.json());
      panel.innerHTML = this._renderTimeline(t);
    } catch (err) {
      panel.innerHTML = `<div class="mi-error">Failed: ${this._escape(err.message)}</div>`;
    }
  }

  _renderTimeline(t) {
    if (t.note) {
      return `<div class="mi-card"><h3>Run #${this._escape(t.runId)}</h3><div class="mi-empty">${this._escape(t.note)}</div></div>`;
    }
    const totalSec = (t.totalDurationMs / 1000).toFixed(1);
    const chips = Object.entries(t.eventCounts || {}).map(([ev, n]) => {
      return `<span class="mi-chip mi-chip-${this._eventColor(ev)}">${this._escape(ev)} <b>${n}</b></span>`;
    }).join('');

    // Build a horizontal timeline-bar visualisation.
    const total = Math.max(t.totalDurationMs, 1);
    const bars = t.events.map((e, i) => {
      const leftPct = (e.offsetMs * 100 / total).toFixed(1);
      const duration = (e.durationMs || 0);
      const widthPct = duration > 0 ? Math.max(0.5, (duration * 100 / total)).toFixed(1) : 0.4;
      const color = this._eventColor(e.event);
      const tipParts = [`+${e.offsetMs}ms`, e.event];
      if (e.agent) tipParts.push(`agent=${e.agent}`);
      if (e.outcome) tipParts.push(`outcome=${e.outcome}`);
      if (e.durationMs) tipParts.push(`${e.durationMs}ms`);
      if (e.completionTokens) tipParts.push(`${e.completionTokens}tok`);
      if (e.projectionMode) tipParts.push(`proj=${e.projectionMode}`);
      if (e.decision) tipParts.push(`cache=${e.decision}`);
      return `<div class="mi-tl-bar mi-tl-bar-${color}" data-idx="${i}"
                   style="left:${leftPct}%; width:${widthPct}%;"
                   title="${this._escape(tipParts.join(' · '))}"></div>`;
    }).join('');

    // Tabular details below the timeline
    const rows = t.events.map(e => {
      const cells = [];
      cells.push(`<td class="num">+${e.offsetMs}ms</td>`);
      cells.push(`<td>${this._eventBadge(e.event)}</td>`);
      cells.push(`<td>${this._escape(e.agent || '')}</td>`);
      const detail = this._timelineEventDetail(e);
      cells.push(`<td>${detail}</td>`);
      return `<tr>${cells.join('')}</tr>`;
    }).join('');

    return `
      <div class="mi-card">
        <h3>Run #${this._escape(t.runId)} · ${totalSec}s · ${t.events.length} events</h3>
        <div class="mi-tl-chips">${chips}</div>
        <div class="mi-tl-track">${bars}</div>
        <div class="mi-tl-axis">
          <span>0</span><span>¼</span><span>½</span><span>¾</span><span>${totalSec}s</span>
        </div>
        <table class="mi-table mi-table-dense" style="margin-top:14px;">
          <thead><tr><th style="width:80px;">Offset</th><th style="width:140px;">Event</th><th style="width:200px;">Agent</th><th>Detail</th></tr></thead>
          <tbody>${rows}</tbody>
        </table>
      </div>
    `;
  }

  _timelineEventDetail(e) {
    const parts = [];
    if (e.outcome) parts.push(this._outcomeBadge(e.outcome));
    if (e.projectionMode) parts.push(`<span class="mi-mini">proj=<b>${this._escape(e.projectionMode)}</b></span>`);
    if (e.decision) parts.push(this._cacheBadge(e.decision));
    if (e.durationMs) parts.push(`<span class="mi-mini">${e.durationMs}ms stream</span>`);
    if (e.completionTokens) parts.push(`<span class="mi-mini">${e.completionTokens} tokens</span>`);
    if (e.projectionTokens) parts.push(`<span class="mi-mini">${e.projectionTokens} proj-tok</span>`);
    if (e.rawRektTokens) parts.push(`<span class="mi-mini">${e.rawRektTokens} rekt-tok</span>`);
    if (e.compileSuccess === true) parts.push('<span class="mi-ok">✅ compile</span>');
    if (e.compileSuccess === false) parts.push('<span class="mi-bad">❌ compile</span>');
    if (e.braceImbalance !== null && e.braceImbalance !== undefined) {
      const cls = e.braceImbalance === 0 ? 'mi-ok' : 'mi-bad';
      parts.push(`<span class="${cls}">braces ${e.braceImbalance}</span>`);
    }
    if (e.file) parts.push(`<code class="mi-path">${this._escape(this._shortFile(e.file))}</code>`);
    return parts.join(' &middot; ') || '<span class="mi-muted">—</span>';
  }

  _shortFile(p) {
    if (!p) return '';
    const parts = p.split('/');
    return parts.length > 2 ? `…/${parts.slice(-2).join('/')}` : p;
  }

  _eventColor(ev) {
    return ({
      'llm_call': 'purple',
      'projection_metrics': 'blue',
      'cache_event': 'green',
      'quality_metrics': 'yellow',
      'reassembly_metrics': 'orange',
      'continuation_event': 'red',
    })[ev] || 'gray';
  }

  _eventBadge(ev) {
    return `<span class="mi-chip mi-chip-${this._eventColor(ev)}">${this._escape(ev)}</span>`;
  }

  // ────────────────────────────────────────────────────────────────────
  // Dependency Topology renderer (Phase-1 PR-P3)
  // ────────────────────────────────────────────────────────────────────
  _renderTopology(topology, services) {
    if (!topology || !topology.nodes || topology.nodes.length === 0) {
      return `<div class="mi-empty">No topology data available. Run <code>./doctor.sh rekt-full</code> to populate the graph.</div>`;
    }

    // Build dependency indexes from services edges (CALL type).
    const edges = (services && services.edges) || [];
    const upstream = {};   // who calls X (basename → [caller])
    const downstream = {}; // who X calls (basename → [callee])
    for (const e of edges) {
      const s = e.source, t = e.target;
      if (!s || !t) continue;
      (downstream[s] = downstream[s] || []).push({ target: t, type: e.type });
      (upstream[t] = upstream[t] || []).push({ source: s, type: e.type });
    }

    // Index modernization nodes by basename for fast lookup.
    const miByBasename = {};
    for (const n of topology.nodes) miByBasename[n.id] = n;

    // Build a unified inventory: merge topology nodes with services nodes
    // (services may include programs not in source/ that REKT discovered via
    // CALL targets — keep them so the impact analysis is complete).
    const allIds = new Set([
      ...topology.nodes.map(n => n.id),
      ...(services?.nodes || []).map(n => n.id),
    ]);

    const items = [];
    for (const id of allIds) {
      const mi = miByBasename[id];
      const svc = (services?.nodes || []).find(n => n.id === id);
      const up = (upstream[id] || []).length;
      const down = (downstream[id] || []).length;
      items.push({
        id,
        loc: mi?.linesOfCode || svc?.lineCount || 0,
        hasFacts: mi?.hasFacts || false,
        status: mi?.modernizationStatus || 'not-in-source',
        compileSuccess: mi?.compileSuccess,
        cacheHits: mi?.projectionCacheHits || 0,
        latestRunId: mi?.latestRunId,
        upstreamCount: up,
        downstreamCount: down,
        impactScore: up + down,
      });
    }
    // Sort by impact (most connected first) for triage value.
    items.sort((a, b) => b.impactScore - a.impactScore || b.loc - a.loc);

    const verified = items.filter(x => x.compileSuccess === true).length;
    const facts = items.filter(x => x.hasFacts).length;
    const totalEdges = edges.length;

    const kpiRow = `
      <div class="mi-kpi-row">
        <div class="mi-kpi"><div class="mi-kpi-value">${items.length}</div><div class="mi-kpi-label">Programs in topology</div><div class="mi-kpi-sub">union of source/ + REKT graph</div></div>
        <div class="mi-kpi"><div class="mi-kpi-value">${totalEdges}</div><div class="mi-kpi-label">CALL edges</div><div class="mi-kpi-sub">from Neo4j services graph</div></div>
        <div class="mi-kpi"><div class="mi-kpi-value">${facts}</div><div class="mi-kpi-label">REKT-ready</div><div class="mi-kpi-sub">have facts.json</div></div>
        <div class="mi-kpi"><div class="mi-kpi-value">${verified}</div><div class="mi-kpi-label">Compile-verified</div><div class="mi-kpi-sub">passed quality gate</div></div>
      </div>
    `;

    const rows = items.slice(0, 200).map(x => {
      const statusBadge = `<span class="mi-status mi-status-${x.status}">${this._statusLabel(x.status)}</span>`;
      const factsCell = x.hasFacts ? '<span class="mi-ok">✓</span>' : '<span class="mi-muted">—</span>';
      const compileCell = x.compileSuccess === true ? '<span class="mi-ok">✅</span>'
        : x.compileSuccess === false ? '<span class="mi-bad">❌</span>' : '<span class="mi-muted">—</span>';
      return `<tr class="mi-topo-row" data-id="${this._escape(x.id)}">
        <td><b>${this._escape(x.id)}</b></td>
        <td class="num">${x.loc.toLocaleString()}</td>
        <td class="num">${x.upstreamCount}</td>
        <td class="num">${x.downstreamCount}</td>
        <td class="num">${x.impactScore}</td>
        <td>${factsCell}</td>
        <td>${compileCell}</td>
        <td class="num">${x.cacheHits}</td>
        <td>${statusBadge}</td>
      </tr>`;
    }).join('');

    return `
      ${kpiRow}
      <div class="mi-topology-layout">
        <div class="mi-card mi-topo-list">
          <h3>Programs by modernization-impact score</h3>
          <div class="mi-muted" style="font-size:11px; margin-bottom:8px;">
            impact = upstream callers + downstream callees. Click a row for migration impact analysis.
          </div>
          <table class="mi-table mi-table-dense">
            <thead>
              <tr>
                <th>Program</th>
                <th>LoC</th>
                <th title="Programs that CALL this one">↑ Callers</th>
                <th title="Programs CALLed by this one">↓ Callees</th>
                <th>Impact</th>
                <th>Facts</th>
                <th>Compile</th>
                <th>Cache</th>
                <th>Status</th>
              </tr>
            </thead>
            <tbody>${rows}</tbody>
          </table>
        </div>
        <div id="mi-topo-detail" class="mi-card mi-topo-detail">
          <div class="mi-empty">Select a program on the left to see its dependency impact.</div>
        </div>
      </div>
    `;
  }

  _wireTopologyInteractions(topology, services) {
    const edges = (services && services.edges) || [];
    const miByBasename = {};
    for (const n of topology.nodes) miByBasename[n.id] = n;

    const buildClosure = (id, direction, depth = 4) => {
      const visited = new Set();
      const out = [];
      const traverse = (cur, hops) => {
        if (visited.has(cur) || hops > depth) return;
        visited.add(cur);
        const neighbours = direction === 'upstream'
          ? edges.filter(e => e.target === cur).map(e => e.source)
          : edges.filter(e => e.source === cur).map(e => e.target);
        for (const n of neighbours) {
          if (!visited.has(n)) out.push({ id: n, hops });
          traverse(n, hops + 1);
        }
      };
      traverse(id, 1);
      return out;
    };

    this.root.querySelectorAll('.mi-topo-row').forEach(tr => {
      tr.onclick = () => {
        const id = tr.dataset.id;
        this.root.querySelectorAll('.mi-topo-row').forEach(r =>
          r.classList.toggle('mi-topo-row-active', r.dataset.id === id));
        const detail = this.root.querySelector('#mi-topo-detail');
        const mi = miByBasename[id];
        const callers = buildClosure(id, 'upstream');
        const callees = buildClosure(id, 'downstream');

        const callerRows = callers.length === 0
          ? '<tr><td colspan="3" class="mi-muted">no upstream callers</td></tr>'
          : callers.map(c => {
            const cmi = miByBasename[c.id];
            const stat = cmi ? `<span class="mi-status mi-status-${cmi.modernizationStatus}">${this._statusLabel(cmi.modernizationStatus)}</span>` : '<span class="mi-muted">not-in-source</span>';
            return `<tr><td><code>${this._escape(c.id)}</code></td><td class="num">${c.hops}</td><td>${stat}</td></tr>`;
          }).join('');

        const calleeRows = callees.length === 0
          ? '<tr><td colspan="3" class="mi-muted">no downstream callees</td></tr>'
          : callees.map(c => {
            const cmi = miByBasename[c.id];
            const stat = cmi ? `<span class="mi-status mi-status-${cmi.modernizationStatus}">${this._statusLabel(cmi.modernizationStatus)}</span>` : '<span class="mi-muted">not-in-source</span>';
            return `<tr><td><code>${this._escape(c.id)}</code></td><td class="num">${c.hops}</td><td>${stat}</td></tr>`;
          }).join('');

        detail.innerHTML = `
          <h3>${this._escape(id)} — migration impact</h3>
          ${mi ? `<div style="margin-bottom:12px;">
            <span class="mi-mini">LoC: <b>${mi.linesOfCode.toLocaleString()}</b></span> &middot;
            <span class="mi-mini">Facts: <b>${mi.hasFacts ? '✓' : '—'}</b></span> &middot;
            <span class="mi-mini">Cache hits: <b>${mi.projectionCacheHits}</b></span> &middot;
            <span class="mi-status mi-status-${mi.modernizationStatus}">${this._statusLabel(mi.modernizationStatus)}</span>
          </div>` : '<div class="mi-muted" style="margin-bottom:12px;">Not present in source/ — discovered as a CALL target.</div>'}

          <h4 style="color:#60a5fa; margin:12px 0 6px; font-size:12px; text-transform:uppercase; letter-spacing:0.5px;">↑ Upstream callers (transitive, max depth 4) — ${callers.length}</h4>
          <table class="mi-table mi-table-dense">
            <thead><tr><th>Caller</th><th>Hops</th><th>Status</th></tr></thead>
            <tbody>${callerRows}</tbody>
          </table>

          <h4 style="color:#60a5fa; margin:16px 0 6px; font-size:12px; text-transform:uppercase; letter-spacing:0.5px;">↓ Downstream callees (transitive, max depth 4) — ${callees.length}</h4>
          <table class="mi-table mi-table-dense">
            <thead><tr><th>Callee</th><th>Hops</th><th>Status</th></tr></thead>
            <tbody>${calleeRows}</tbody>
          </table>

          <div class="mi-source" style="margin-top:14px;">
            Dependency edges sourced from <code>/api/graph/rekt/services</code> (Neo4j CALL relationships).
            Modernization status from <code>/api/modernization/topology</code>.
          </div>
        `;
      };
    });
  }

  _outcomeBadge(o) {
    if (o === 'success') return '<span class="mi-ok">✅ success</span>';
    if (o === 'timeout') return '<span class="mi-bad">⏱ timeout</span>';
    if (o === 'error') return '<span class="mi-bad">❌ error</span>';
    return `<span class="mi-muted">${this._escape(o || '?')}</span>`;
  }
  _cacheBadge(d) {
    if (d === 'hit') return '<span class="mi-ok">🎯 hit</span>';
    if (d === 'miss-store') return '<span class="mi-info">💾 miss-store</span>';
    if (d === 'bypass-disabled') return '<span class="mi-muted">⏸ bypass</span>';
    return `<span class="mi-muted">${this._escape(d)}</span>`;
  }
  _escape(s) {
    if (s == null) return '';
    return String(s).replace(/[&<>"']/g, c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
  }
}

window.ModernizationIntelligenceView = ModernizationIntelligenceView;
