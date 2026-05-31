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
    // #12: auto-refresh — every 30s while this surface is visible
    PortalAutoRefresh?.attach(this, 30000);
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
          <button class="mi-subtab" data-sub="health">🩺 Dependency Health</button>
          <button class="mi-subtab" data-sub="chain">🔗 Service Chain (JCL→Pgm→Cpy)</button>
          <button class="mi-subtab" data-sub="runtime">⏱ Runtime &amp; Conversion Intelligence</button>
          <button class="mi-subtab" data-sub="topology">🕸 Dependency Topology</button>
          <button class="mi-subtab" data-sub="flow">🌊 Semantic Flow Explorer</button>
          <button class="mi-subtab" data-sub="services">🧩 Service Candidates</button>
          <button class="mi-subtab" data-sub="waves">🚀 Migration Wave Planner</button>
          <button class="mi-subtab" data-sub="capabilities">🎯 Capabilities &amp; Locator</button>
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
      } else if (this._activeSubview === 'health') {
        const data = await fetch('/api/modernization/dependency-health').then(r => r.json());
        body.innerHTML = this._renderDependencyHealth(data);
      } else if (this._activeSubview === 'flow') {
        // Load the dependency health snapshot to populate the flow-eligible program picker.
        const health = await fetch('/api/modernization/dependency-health').then(r => r.json());
        body.innerHTML = this._renderFlowShell(health);
        // Auto-select first full-fidelity program
        const firstFull = (health.programs || []).find(p => p.parseFidelity === 'full');
        if (firstFull) await this._loadProgramFlow(firstFull.basename);
      } else if (this._activeSubview === 'services') {
        const data = await fetch('/api/modernization/service-candidates').then(r => r.json());
        body.innerHTML = this._renderServiceCandidates(data);
        this._wireServiceCandidatesInteractions();
      } else if (this._activeSubview === 'waves') {
        const [apps, waves, health] = await Promise.all([
          fetch('/api/modernization/applications').then(r => r.json()),
          fetch('/api/modernization/waves').then(r => r.json()),
          fetch('/api/modernization/dependency-health').then(r => r.json()),
        ]);
        body.innerHTML = this._renderWavePlanner(apps, waves, health);
        this._wireWavePlannerInteractions();
      } else if (this._activeSubview === 'chain') {
        const data = await fetch('/api/modernization/service-chain').then(r => r.json());
        body.innerHTML = this._renderServiceChain(data);
        this._wireServiceChainInteractions(data);
        // Render the Mermaid diagram once the DOM is in place
        setTimeout(() => this._renderMermaidIn(body), 50);
      } else if (this._activeSubview === 'capabilities') {
        const catalog = await fetch('/api/modernization/capabilities').then(r => r.json());
        body.innerHTML = this._renderCapabilities(catalog);
        this._wireCapabilitiesInteractions();
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
          <div class="mi-runs-header">
            <h3 style="margin:0;">Recent runs</h3>
            <button id="mi-compare-runs-btn" class="mi-btn mi-btn-sm"
                    onclick="window.modernizationIntelligenceView._openRunCompareDialog();"
                    title="Pick two runs to compare side-by-side">⇄ Compare A/B</button>
          </div>
          <table class="mi-table mi-table-dense mi-runs-table">
            <thead><tr><th>Run</th><th>Events</th><th>LLM ok</th><th>Proj</th><th>Cache</th><th>Time (UTC)</th></tr></thead>
            <tbody>${runRows}</tbody>
          </table>
        </div>
        <div id="mi-timeline-panel" class="mi-timeline-panel">
          <div class="mi-loading">Select a run on the left, or click <b>⇄ Compare A/B</b> to compare two runs.</div>
        </div>
      </div>
    `;
  }

  // ────────────────────────────────────────────────────────────────────
  // #1 Run comparison — side-by-side A/B diff
  // ────────────────────────────────────────────────────────────────────
  async _openRunCompareDialog() {
    // Pick from the rendered runs table; offer the latest run as A and the second-latest as B by default
    const rows = Array.from(this.root.querySelectorAll('.mi-run-row'));
    const runIds = rows.map(r => r.dataset.run);
    if (runIds.length < 2) {
      alert('Need at least 2 runs with telemetry to compare. Run another conversion first.');
      return;
    }
    const a = runIds[0], b = runIds[1];
    const opts = runIds.map(r => `<option value="${this._escape(r)}">#${this._escape(r)}</option>`).join('');
    let modal = document.getElementById('mi-compare-modal');
    if (!modal) {
      modal = document.createElement('div');
      modal.id = 'mi-compare-modal';
      modal.className = 'mi-modal';
      document.body.appendChild(modal);
      modal.addEventListener('click', (e) => {
        if (e.target === modal) modal.style.display = 'none';
      });
    }
    modal.style.display = 'flex';
    modal.innerHTML = `
      <div class="mi-modal-card mi-modal-card-wide" onclick="event.stopPropagation();">
        <div class="mi-modal-header">
          <div>
            <div class="mi-modal-title">⇄ Run comparison (A/B)</div>
            <div class="mi-modal-sub">Side-by-side delta of two runs — answers "did my prompt / model / config change help?"</div>
          </div>
          <button class="mi-btn" onclick="document.getElementById('mi-compare-modal').style.display='none';">✕ Close</button>
        </div>
        <div class="mi-compare-pickers">
          <label>Run A (baseline) <select id="mi-cmp-a">${opts}</select></label>
          <label>Run B (compare against A) <select id="mi-cmp-b">${opts}</select></label>
          <button class="mi-btn mi-btn-primary" onclick="window.modernizationIntelligenceView._runComparison();">Run comparison →</button>
        </div>
        <div id="mi-compare-body" class="mi-modal-body"><div class="mi-muted vc-pad">Pick A and B, then click "Run comparison →"</div></div>
      </div>
    `;
    document.getElementById('mi-cmp-a').value = a;
    document.getElementById('mi-cmp-b').value = b;
    // Auto-trigger the comparison so the user sees something immediately
    this._runComparison();
  }

  async _runComparison() {
    const a = document.getElementById('mi-cmp-a').value;
    const b = document.getElementById('mi-cmp-b').value;
    const body = document.getElementById('mi-compare-body');
    if (a === b) { body.innerHTML = '<div class="mi-error">Pick two different runs.</div>'; return; }
    body.innerHTML = '<div class="mi-loading">Loading…</div>';
    try {
      const [tA, tB] = await Promise.all([
        fetch(`/api/modernization/runs/${encodeURIComponent(a)}/timeline`).then(r => r.json()),
        fetch(`/api/modernization/runs/${encodeURIComponent(b)}/timeline`).then(r => r.json()),
      ]);
      body.innerHTML = this._renderCompare(a, b, tA, tB);
    } catch (err) {
      body.innerHTML = `<div class="mi-error">Failed: ${this._escape(err.message)}</div>`;
    }
  }

  _renderCompare(idA, idB, tA, tB) {
    // KPI extraction
    const kpis = (t) => {
      const evs = t.events || [];
      const llm = evs.filter(e => e.event === 'llm_call');
      const cache = evs.filter(e => e.event === 'cache_event');
      const cacheHits = cache.filter(e => e.decision === 'hit').length;
      const llmSuccess = llm.filter(e => e.outcome === 'success').length;
      const proj = evs.filter(e => e.event === 'projection_metrics');
      const quality = evs.filter(e => e.event === 'quality_metrics');
      const compileGate = quality[quality.length - 1];
      const tokens = llm.reduce((s, e) => s + (e.completionTokens || 0), 0);
      const avgRedPct = proj.length === 0 ? null : Math.round(proj
        .filter(e => e.rawRektTokens && e.projectionTokens)
        .reduce((s, e, _, a) => s + ((e.rawRektTokens - e.projectionTokens) / e.rawRektTokens * 100 / a.length), 0));
      return {
        totalSec:        (t.totalDurationMs / 1000).toFixed(1),
        eventCount:      evs.length,
        llmCalls:        llm.length,
        llmSuccess,
        cacheHitRate:    cache.length ? Math.round(cacheHits * 100 / cache.length) : null,
        projectionCount: proj.length,
        projectionAvgRedPct: avgRedPct,
        outputTokens:    tokens,
        compilePass:     compileGate ? compileGate.compileSuccess : null,
        compileErrors:   compileGate ? (compileGate.braceImbalance || 0) : 0,
      };
    };
    const A = kpis(tA), B = kpis(tB);
    const delta = (a, b, unit = '', goodIsLower = false) => {
      if (a == null || b == null) return '<span class="mi-muted">—</span>';
      const diff = b - a;
      if (diff === 0) return `<span class="mi-muted">=</span>`;
      const positive = diff > 0;
      const isGood = goodIsLower ? !positive : positive;
      const color = isGood ? 'var(--color-success)' : 'var(--color-fail)';
      const arrow = positive ? '▲' : '▼';
      const pct = a !== 0 ? Math.round(diff * 100 / Math.abs(a)) : 0;
      return `<span style="color:${color};font-weight:600;">${arrow} ${positive ? '+' : ''}${diff}${unit} ${pct ? `(${positive ? '+' : ''}${pct}%)` : ''}</span>`;
    };
    const compileBadge = (v) => v === true ? '<span style="color:var(--color-success);">✅ pass</span>'
                                : v === false ? '<span style="color:var(--color-fail);">❌ fail</span>'
                                : '<span class="mi-muted">no gate</span>';
    return `
      <div class="mi-cmp-grid">
        <table class="mi-cmp-table">
          <thead><tr>
            <th>Metric</th>
            <th class="mi-cmp-a">Run A · #${this._escape(idA)}</th>
            <th class="mi-cmp-b">Run B · #${this._escape(idB)}</th>
            <th>Δ (B - A)</th>
            <th>Verdict</th>
          </tr></thead>
          <tbody>
            <tr>
              <td>Total duration</td>
              <td>${A.totalSec}s</td>
              <td>${B.totalSec}s</td>
              <td>${delta(parseFloat(A.totalSec), parseFloat(B.totalSec), 's', true)}</td>
              <td class="mi-muted">lower = faster</td>
            </tr>
            <tr>
              <td>Telemetry events</td>
              <td>${A.eventCount}</td>
              <td>${B.eventCount}</td>
              <td>${delta(A.eventCount, B.eventCount, '', true)}</td>
              <td class="mi-muted">fewer = simpler path</td>
            </tr>
            <tr>
              <td>LLM calls (success / total)</td>
              <td>${A.llmSuccess} / ${A.llmCalls}</td>
              <td>${B.llmSuccess} / ${B.llmCalls}</td>
              <td>${delta(A.llmCalls, B.llmCalls, '', true)}</td>
              <td class="mi-muted">fewer = less chunking</td>
            </tr>
            <tr>
              <td>Output tokens generated</td>
              <td>${A.outputTokens.toLocaleString()}</td>
              <td>${B.outputTokens.toLocaleString()}</td>
              <td>${delta(A.outputTokens, B.outputTokens, '', true)}</td>
              <td class="mi-muted">fewer = more concise</td>
            </tr>
            <tr>
              <td>Cache hit rate</td>
              <td>${A.cacheHitRate ?? '—'}${A.cacheHitRate != null ? '%' : ''}</td>
              <td>${B.cacheHitRate ?? '—'}${B.cacheHitRate != null ? '%' : ''}</td>
              <td>${delta(A.cacheHitRate, B.cacheHitRate, '%')}</td>
              <td class="mi-muted">higher = better reuse</td>
            </tr>
            <tr>
              <td>Projection events</td>
              <td>${A.projectionCount}</td>
              <td>${B.projectionCount}</td>
              <td>${delta(A.projectionCount, B.projectionCount, '', true)}</td>
              <td class="mi-muted">stable = same chunks</td>
            </tr>
            <tr>
              <td>Avg projection reduction</td>
              <td>${A.projectionAvgRedPct ?? '—'}${A.projectionAvgRedPct != null ? '%' : ''}</td>
              <td>${B.projectionAvgRedPct ?? '—'}${B.projectionAvgRedPct != null ? '%' : ''}</td>
              <td>${delta(A.projectionAvgRedPct, B.projectionAvgRedPct, '%')}</td>
              <td class="mi-muted">higher = denser context</td>
            </tr>
            <tr>
              <td>Compile gate</td>
              <td>${compileBadge(A.compilePass)}</td>
              <td>${compileBadge(B.compilePass)}</td>
              <td>${A.compilePass === B.compilePass ? '<span class="mi-muted">=</span>'
                  : (B.compilePass === true ? '<span style="color:var(--color-success);font-weight:600;">🎉 newly passing</span>'
                     : '<span style="color:var(--color-fail);font-weight:600;">⚠ regressed</span>')}</td>
              <td class="mi-muted">pass = the win</td>
            </tr>
          </tbody>
        </table>

        <div class="mi-cmp-verdict">
          <h4>Bottom line</h4>
          ${this._compareVerdict(A, B)}
        </div>
      </div>
    `;
  }

  _compareVerdict(A, B) {
    const points = [];
    if (A.compilePass === false && B.compilePass === true)  points.push('<li>🎉 <b>B fixed the compile failure.</b> Whatever changed (prompt, model, config) is worth keeping.</li>');
    if (A.compilePass === true  && B.compilePass === false) points.push('<li>⚠ <b>B regressed the compile gate.</b> Revert or investigate immediately.</li>');
    if (B.llmCalls < A.llmCalls)                            points.push('<li>📉 B used <b>${a}</b> fewer LLM calls — likely better projection or chunking.</li>'.replace('${a}', A.llmCalls - B.llmCalls));
    if (B.cacheHitRate != null && A.cacheHitRate != null && B.cacheHitRate > A.cacheHitRate + 5) points.push('<li>💾 Cache hit rate improved by <b>${a}%</b> — your projection determinism is paying off.</li>'.replace('${a}', B.cacheHitRate - A.cacheHitRate));
    if (parseFloat(B.totalSec) < parseFloat(A.totalSec) * 0.8) points.push('<li>⚡ B is <b>${a}% faster</b> end-to-end.</li>'.replace('${a}', Math.round((1 - parseFloat(B.totalSec) / parseFloat(A.totalSec)) * 100)));
    if (B.outputTokens > A.outputTokens * 1.3) points.push('<li>💰 B generated <b>${a}% more output tokens</b> — check whether the model started padding or whether the program is truly larger.</li>'.replace('${a}', Math.round((B.outputTokens / A.outputTokens - 1) * 100)));
    if (points.length === 0) return '<p class="mi-muted">No significant difference between the two runs across the tracked metrics.</p>';
    return `<ul>${points.join('')}</ul>`;
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
      // #8: wire compile-failure deep-link rows
      panel.querySelectorAll('.mi-tl-row-fail').forEach(row => {
        row.addEventListener('click', () => this._openCompileInspector(row.dataset.runid));
      });
    } catch (err) {
      panel.innerHTML = `<div class="mi-error">Failed: ${this._escape(err.message)}</div>`;
    }
  }

  /** #8 compile-failure deep link — opens a modal with the broken generated file. */
  async _openCompileInspector(runId) {
    let modal = document.getElementById('mi-compile-modal');
    if (!modal) {
      modal = document.createElement('div');
      modal.id = 'mi-compile-modal';
      modal.className = 'mi-modal';
      document.body.appendChild(modal);
      // Close on backdrop click + Escape key
      modal.addEventListener('click', (e) => {
        if (e.target === modal) this._closeCompileInspector();
      });
      this._escHandler = (e) => { if (e.key === 'Escape') this._closeCompileInspector(); };
    }
    document.addEventListener('keydown', this._escHandler);
    document.body.style.overflow = 'hidden';   // lock scroll behind modal
    modal.style.display = 'flex';
    modal.innerHTML = `
      <div class="mi-modal-card mi-modal-card-wide" onclick="event.stopPropagation();">
        <div class="mi-modal-header">
          <div>
            <div class="mi-modal-title">🔴 Compile failure inspector — Run #${this._escape(runId)}</div>
            <div class="mi-modal-sub">Loading generated files + compile log…</div>
          </div>
          <button class="mi-btn" onclick="window.modernizationIntelligenceView._closeCompileInspector();">✕ Close (Esc)</button>
        </div>
        <div id="mi-compile-body" class="mi-modal-body"><div class="mi-loading">Loading…</div></div>
      </div>
    `;
    try {
      const d = await fetch(`/api/modernization/runs/${encodeURIComponent(runId)}/compile-detail`).then(r => {
        if (!r.ok) throw new Error(`HTTP ${r.status}`);
        return r.json();
      });
      const body = modal.querySelector('#mi-compile-body');
      body.innerHTML = this._renderCompileDetail(d);
    } catch (err) {
      modal.querySelector('#mi-compile-body').innerHTML =
        `<div class="mi-error">Failed: ${this._escape(err.message)}</div>`;
    }
  }

  _closeCompileInspector() {
    const modal = document.getElementById('mi-compile-modal');
    if (modal) modal.style.display = 'none';
    document.body.style.overflow = '';
    if (this._escHandler) document.removeEventListener('keydown', this._escHandler);
  }

  _renderCompileDetail(d) {
    if (!d.outputFolder) {
      return `<div class="mi-empty">No output folder found for run #${this._escape(d.runId)}.
        The run completed before per-run isolated folders were introduced, or the folder was deleted.</div>`;
    }
    const fileList = (d.files || []).map((f, i) => `
      <button class="mi-compile-file ${i === 0 ? 'mi-compile-file-active' : ''}"
              data-idx="${i}" onclick="window.modernizationIntelligenceView._showCompileFile(${i})">
        <div class="mi-compile-file-name">${this._escape(f.fileName)}</div>
        <div class="mi-compile-file-meta">${f.lineCount} lines${f.hasError ? ` · <b style="color:var(--color-fail);">${f.errorCount} err</b>` : ''}</div>
      </button>
    `).join('');
    this._compileFiles = d.files || [];
    this._compileErrors = d.errors || [];
    const firstFile = (d.files || [])[0];
    const isLegacy = d.outputFolder === 'output/java' || d.outputFolder === 'output/csharp';
    const errCount = (d.errors || []).length;
    return `
      <details class="mi-compile-help" ${errCount === 0 ? 'open' : ''}>
        <summary>ℹ️ What is this inspector and how do I use it? <span class="mi-muted">(click to expand)</span></summary>
        <div class="mi-compile-help-body">
          <p><b>Purpose.</b> When a <code>quality_metrics</code> event in the Runtime timeline reports
          <span class="mi-bad">❌ compile</span>, this inspector lets you look directly at the generated
          code that failed to compile — without leaving the portal. It's the bridge between
          <i>"the agent produced something"</i> and <i>"is the something actually valid Java/C#?"</i>.</p>

          <p><b>What you see.</b></p>
          <ul>
            <li><b>Output folder</b> (top of left pane) — the per-run isolated folder under
              <code>output/runs/{runId}-…/</code> for new runs, or the legacy shared
              <code>output/java</code> / <code>output/csharp</code> for runs before per-run isolation
              shipped.${isLegacy ? ' <b style="color:var(--color-warn);">⚠ Run #' + this._escape(d.runId) + ' is a legacy run — you\'re seeing the shared folder, which contains files from many runs, not just this one.</b>' : ''}</li>
            <li><b>File list (left)</b> — every generated <code>.java</code>/<code>.cs</code>/
              <code>.kt</code>/<code>.ts</code>/<code>.scala</code> file in that folder. Files with
              compile errors float to the top, marked with a red error count.</li>
            <li><b>Source viewer (right)</b> — click a file to view its full source with line numbers.
              Lines flagged by the compile log are highlighted red.</li>
            <li><b>Compile errors panel (bottom right)</b> — structured errors parsed from
              <code>compile.log</code>, <code>check-compile.log</code>, <code>javac.log</code>, or
              <code>dotnet-build.log</code> if present in the run folder. Cross-references back to
              each file/line.</li>
          </ul>

          <p><b>How to use it for debugging a failure.</b></p>
          <ol>
            <li>Find the file with the red error badge in the left pane (failing files are at the top).</li>
            <li>Click it — the source viewer scrolls in.</li>
            <li>Look at the red-highlighted lines. Common chunked-conversion bugs:
              <ul>
                <li><b>Bare <code>return</code> outside a method</b> → chunk boundary fell mid-method</li>
                <li><b>Missing <code>}</code> or extra <code>}</code></b> → brace stitching broke</li>
                <li><b>Duplicate class declarations</b> → SharedTypeRegistry not consulted (CS0101 in C#)</li>
                <li><b>Unresolved symbol</b> → a CALL target wasn't included in the conversion scope</li>
              </ul>
            </li>
            <li>The compile errors panel at the bottom right gives the exact compiler message if a log was found.</li>
          </ol>

          ${errCount === 0 && !isLegacy ? `
          <p class="mi-muted"><b>No structured errors right now</b> because the compile-quality gate ran but
          didn't emit a parseable log to this folder. Likely the gate is using a different log destination,
          or the run actually passed at the file level (but the <code>quality_metrics</code> event was
          flagged for a different reason). Try opening the largest files and looking for visually obvious
          syntax breaks (mismatched braces, stray <code>return</code>).</p>` : ''}

          ${errCount === 0 && isLegacy ? `
          <p class="mi-muted"><b>Why no error highlights?</b> Run #${this._escape(d.runId)} predates the
          per-run isolated output folder feature, so it dumped files into the shared
          <code>output/java</code>/<code>output/csharp</code> folder. There's no per-run compile log
          here to parse. Re-run the conversion using a recent build and the new run's folder will
          contain a dedicated compile.log that this inspector can highlight.</p>` : ''}
        </div>
      </details>

      <div class="mi-compile-grid">
        <div class="mi-compile-sidebar">
          <div class="mi-compile-folder">📁 <code>${this._escape(d.outputFolder)}</code></div>
          <div class="mi-compile-count">${(d.files || []).length} file${(d.files || []).length === 1 ? '' : 's'} · ${errCount} error${errCount === 1 ? '' : 's'}</div>
          ${fileList || '<div class="mi-muted vc-pad">No generated files in this run\'s folder.</div>'}
        </div>
        <div class="mi-compile-main">
          <div id="mi-compile-source">
            ${firstFile ? this._renderCompileSource(firstFile, this._compileErrors) :
              '<div class="mi-muted vc-pad">Pick a file to view its source.</div>'}
          </div>
          ${d.errors && d.errors.length > 0 ? `
            <div class="mi-compile-errors">
              <h4>⚠ Compile errors (${d.errors.length})</h4>
              ${d.errors.slice(0, 20).map(e => `
                <div class="mi-compile-error">
                  <code>${this._escape(e.file || '?')}</code>
                  ${e.line ? `<b>:line ${e.line}</b>` : ''}
                  <div>${this._escape(e.message || '')}</div>
                </div>
              `).join('')}
            </div>` : ''}
        </div>
      </div>
    `;
  }

  _renderCompileSource(file, errors) {
    const errorLines = new Set((errors || []).filter(e => e.file === file.fileName && e.line).map(e => e.line));
    const lines = (file.content || '').split('\n');
    return `
      <div class="mi-compile-source-header">
        <code>${this._escape(file.path)}</code> · ${lines.length} lines
        ${errorLines.size ? `<span style="color:var(--color-fail); margin-left:8px;">${errorLines.size} flagged line${errorLines.size === 1 ? '' : 's'}</span>` : ''}
      </div>
      <pre class="mi-compile-source"><code>${lines.map((ln, i) => {
        const lineNum = i + 1;
        const isErr = errorLines.has(lineNum);
        return `<div class="mi-compile-line ${isErr ? 'mi-compile-line-error' : ''}">` +
          `<span class="mi-compile-lineno">${lineNum}</span>` +
          `<span class="mi-compile-linetext">${this._escape(ln) || '&nbsp;'}</span>` +
          `</div>`;
      }).join('')}</code></pre>
    `;
  }

  _showCompileFile(idx) {
    const f = this._compileFiles[idx];
    if (!f) return;
    document.querySelectorAll('.mi-compile-file').forEach((el, i) =>
      el.classList.toggle('mi-compile-file-active', i === idx));
    document.getElementById('mi-compile-source').innerHTML =
      this._renderCompileSource(f, this._compileErrors);
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
    const rows = t.events.map((e, idx) => {
      const cells = [];
      cells.push(`<td class="num">+${e.offsetMs}ms</td>`);
      cells.push(`<td>${this._eventBadge(e.event)}</td>`);
      cells.push(`<td>${this._escape(e.agent || '')}</td>`);
      const detail = this._timelineEventDetail(e);
      cells.push(`<td>${detail}</td>`);
      // #8: compile-failure rows are clickable → opens the broken file viewer
      const isCompileFail = e.event === 'quality_metrics' && e.compileSuccess === false;
      const rowAttrs = isCompileFail
        ? ` class="mi-tl-row-fail" data-runid="${this._escape(t.runId)}" data-idx="${idx}" title="Click to inspect the failing generated file"`
        : '';
      return `<tr${rowAttrs}>${cells.join('')}</tr>`;
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

  // ────────────────────────────────────────────────────────────────────
  // Dependency Health renderer (PR-Portal-P0-enterprise)
  // ────────────────────────────────────────────────────────────────────
  _renderDependencyHealth(d) {
    if (d.note) {
      return `<div class="mi-empty">${this._escape(d.note)}</div>`;
    }
    const scoreColor = d.readinessScore >= 75 ? '#10b981'
                     : d.readinessScore >= 40 ? '#f59e0b'
                     : '#ef4444';
    const kpi = (label, value, sub, color) => `
      <div class="mi-kpi" style="border-left-color:${color || '#475569'};">
        <div class="mi-kpi-value">${value}</div>
        <div class="mi-kpi-label">${label}</div>
        <div class="mi-kpi-sub">${sub || ''}</div>
      </div>`;

    const kpiRow = `
      <div class="mi-kpi-row">
        ${kpi('Estate readiness', `${d.readinessScore}%`,
              `Full×1.0 + Deps×0.25 weighted across ${d.totalPrograms} programs`,
              scoreColor)}
        ${kpi('Full fidelity', `${d.fullFidelityCount} / ${d.totalPrograms}`,
              `${d.coveragePct}% complete AST coverage`,
              '#10b981')}
        ${kpi('Deps-only', d.depsOnlyCount,
              'partial coverage — missing copybooks limit fidelity',
              '#f59e0b')}
        ${kpi('Missing copybooks', d.totalMissingCopybooks,
              `blocks ${d.programsBlockedByMissing} programs`,
              '#ef4444')}
      </div>
    `;

    // Sort missing copybooks by reference count (most impactful first)
    const sortedMissing = [...(d.missingCopybooks || [])]
      .sort((a, b) => (b.referencedBy?.length || 0) - (a.referencedBy?.length || 0));

    const missingRows = sortedMissing.map(m => `
      <tr>
        <td><code><b>${this._escape(m.copybook)}</b></code></td>
        <td class="num">${m.referencedBy.length}</td>
        <td>
          ${m.referencedBy.slice(0, 5).map(p => `<code class="mi-mini">${this._escape(p)}</code>`).join(' ')}
          ${m.referencedBy.length > 5 ? `<span class="mi-muted">+${m.referencedBy.length - 5} more</span>` : ''}
        </td>
      </tr>`).join('') || '<tr><td colspan="3" class="mi-muted">No missing copybooks — full estate coverage.</td></tr>';

    // Per-program health, sorted: not-parsed first, then deps-only, then full
    const fidelityOrder = { 'not-parsed': 0, 'deps-only': 1, 'full': 2 };
    const sortedPrograms = [...(d.programs || [])]
      .sort((a, b) => (fidelityOrder[a.parseFidelity] ?? 99) - (fidelityOrder[b.parseFidelity] ?? 99)
                   || b.linesOfCode - a.linesOfCode);
    const programRows = sortedPrograms.map(p => {
      const fidelityBadge = this._fidelityBadge(p.parseFidelity);
      const missing = p.missingCopybookCount > 0
        ? `<span class="mi-bad">${p.missingCopybookCount}</span>`
        : '<span class="mi-ok">0</span>';
      return `<tr>
        <td><b>${this._escape(p.basename)}</b></td>
        <td class="num">${p.linesOfCode.toLocaleString()}</td>
        <td>${fidelityBadge}</td>
        <td class="num">${p.factsConfidence}</td>
        <td class="num">${p.factsWarnings}</td>
        <td>${missing}</td>
        <td><span class="mi-status mi-status-${p.modernizationStatus}">${this._statusLabel(p.modernizationStatus)}</span></td>
      </tr>`;
    }).join('');

    return `
      ${kpiRow}
      <div class="mi-card mi-card-wide">
        <h3>Missing copybooks — sorted by impact
          <button class="ih-btn" style="float:right; font-size:11px;"
                  onclick="window.modernizationIntelligenceView._downloadCopybookShoppingList()"
                  title="Download a plain-text shopping list to hand to the source-of-truth team">
            ⬇ Export shopping list
          </button>
        </h3>
        <div class="mi-muted" style="font-size:11px; margin-bottom:8px;">
          Provide these in <code>source/</code> to elevate referencing programs from deps-only to full-fidelity REKT analysis.
          Each resolution increases readiness score and enables high-confidence projection.
        </div>
        <table class="mi-table">
          <thead><tr><th>Copybook</th><th>Programs blocked</th><th>Referenced by</th></tr></thead>
          <tbody>${missingRows}</tbody>
        </table>
      </div>
      <div class="mi-card mi-card-wide">
        <h3>Program-level dependency health</h3>
        <table class="mi-table mi-table-dense">
          <thead>
            <tr>
              <th>Program</th>
              <th>LoC</th>
              <th>Parse fidelity</th>
              <th>Facts conf</th>
              <th>Warn</th>
              <th>Missing cpys</th>
              <th>Modernization status</th>
            </tr>
          </thead>
          <tbody>${programRows}</tbody>
        </table>
      </div>
      <div class="mi-source">
        Sourced from <code>output/rekt/missing-copybooks.txt</code> + <code>output/rekt/*.report/</code> + <code>output/rekt/*-deps.json</code>.
        Re-run <code>./doctor.sh rekt-full</code> after adding copybooks to refresh.
      </div>
    `;
  }

  _fidelityBadge(f) {
    if (f === 'full') return '<span class="mi-status mi-status-verified">✅ full</span>';
    if (f === 'deps-only') return '<span class="mi-status mi-status-partial-fallback">⚠ deps-only</span>';
    return '<span class="mi-status mi-status-not-started">— not parsed</span>';
  }

  // ────────────────────────────────────────────────────────────────────
  // Semantic Flow Explorer renderer (Phase-1 final view)
  // ────────────────────────────────────────────────────────────────────
  _renderFlowShell(health) {
    const programs = (health.programs || []).filter(p => p.parseFidelity === 'full');
    if (programs.length === 0) {
      return `<div class="mi-empty">No full-fidelity programs available. Resolve missing copybooks (see Dependency Health) or run <code>./doctor.sh rekt-full</code> first.</div>`;
    }
    const rows = programs.sort((a, b) => b.linesOfCode - a.linesOfCode).map(p => `
      <tr data-prog="${this._escape(p.basename)}" class="mi-flow-row">
        <td><b>${this._escape(p.basename)}</b></td>
        <td class="num">${p.linesOfCode.toLocaleString()}</td>
        <td class="num">${p.factsConfidence}</td>
        <td><span class="mi-status mi-status-${p.modernizationStatus}">${this._statusLabel(p.modernizationStatus)}</span></td>
      </tr>`).join('');

    return `
      <div class="mi-flow-layout">
        <div class="mi-card mi-flow-picker">
          <h3>Flow-eligible programs (${programs.length})</h3>
          <div class="mi-muted" style="font-size:11px; margin-bottom:8px;">
            Only full-fidelity programs (those with complete REKT AST) can produce flow diagrams.
          </div>
          <table class="mi-table mi-table-dense">
            <thead><tr><th>Program</th><th>LoC</th><th>Conf</th><th>Status</th></tr></thead>
            <tbody>${rows}</tbody>
          </table>
        </div>
        <div id="mi-flow-detail" class="mi-card mi-flow-detail">
          <div class="mi-empty">Select a program on the left to render its semantic flow.</div>
        </div>
      </div>
    `;
  }

  async _loadProgramFlow(basename) {
    this.root.querySelectorAll('.mi-flow-row').forEach(tr => {
      tr.classList.toggle('mi-flow-row-active', tr.dataset.prog === basename);
      tr.onclick = () => this._loadProgramFlow(tr.dataset.prog);
    });
    const panel = this.root.querySelector('#mi-flow-detail');
    panel.innerHTML = '<div class="mi-loading">Loading flow…</div>';
    try {
      // Use the EXISTING /api/graph/rekt/structure endpoint for the
      // section/paragraph data + the EXISTING /api/graph/rekt/mermaid for
      // the pre-rendered flow diagram. No graph engine duplication.
      const [structure, mermaidPayload] = await Promise.all([
        fetch(`/api/graph/rekt/structure?file=${encodeURIComponent(basename)}`).then(r => r.json()).catch(() => null),
        fetch(`/api/graph/rekt/mermaid?file=${encodeURIComponent(basename)}`).then(r => r.json()).catch(() => null),
      ]);
      // mermaid endpoint returns { type, file, program, mermaid: "..." } — extract.
      const mermaidText = (mermaidPayload && typeof mermaidPayload === 'object')
        ? (mermaidPayload.mermaid || '')
        : '';
      panel.innerHTML = this._renderFlowDetail(basename, structure, mermaidText);
      this._renderMermaidIn(panel);
    } catch (err) {
      panel.innerHTML = `<div class="mi-error">Failed: ${this._escape(err.message)}</div>`;
    }
  }

  _renderFlowDetail(basename, structure, mermaidText) {
    if (!structure || !structure.sections || structure.sections.length === 0) {
      return `<div class="mi-card">
        <h3>${this._escape(basename)}</h3>
        <div class="mi-empty">No structural sections found in REKT output.</div>
      </div>`;
    }
    const sections = structure.sections;
    const totalStmts = sections.reduce((a, s) => a + (s.stmtCount || 0), 0);
    const totalPerforms = sections.reduce((a, s) => a + (s.performCount || 0), 0);
    const totalCalls = sections.reduce((a, s) => a + (s.callCount || 0), 0);
    const totalSql = sections.reduce((a, s) => a + (s.sqlCount || 0), 0);
    const totalBranch = sections.reduce((a, s) => a + (s.branchCount || 0), 0);

    const kpiRow = `
      <div class="mi-kpi-row" style="margin-bottom:12px;">
        <div class="mi-kpi"><div class="mi-kpi-value">${sections.length}</div><div class="mi-kpi-label">Sections</div></div>
        <div class="mi-kpi"><div class="mi-kpi-value">${totalStmts}</div><div class="mi-kpi-label">Statements</div></div>
        <div class="mi-kpi"><div class="mi-kpi-value">${totalPerforms}</div><div class="mi-kpi-label">PERFORMs</div></div>
        <div class="mi-kpi"><div class="mi-kpi-value">${totalCalls}</div><div class="mi-kpi-label">CALLs</div></div>
        <div class="mi-kpi"><div class="mi-kpi-value">${totalSql}</div><div class="mi-kpi-label">SQL ops</div></div>
        <div class="mi-kpi"><div class="mi-kpi-value">${totalBranch}</div><div class="mi-kpi-label">Branches</div></div>
      </div>
    `;

    // Group by section, list paragraphs underneath
    const grouped = {};
    for (const s of sections) {
      const key = s.sectionName || '(unnamed)';
      if (!grouped[key]) grouped[key] = [];
      grouped[key].push(s);
    }
    const sectionBlocks = Object.entries(grouped).map(([sec, rows]) => {
      const paras = rows.map(r => `
        <tr>
          <td><code>${this._escape(r.paraName || r.sectionName || '?')}</code></td>
          <td class="num">${r.stmtCount || 0}</td>
          <td class="num">${r.performCount || 0}</td>
          <td class="num">${r.callCount || 0}</td>
          <td class="num">${r.sqlCount || 0}</td>
          <td class="num">${r.moveCount || 0}</td>
          <td class="num">${r.branchCount || 0}</td>
        </tr>`).join('');
      return `<details class="mi-flow-section" open>
        <summary><b>${this._escape(sec)}</b> · ${rows.length} paragraph(s)</summary>
        <table class="mi-table mi-table-dense">
          <thead><tr><th>Paragraph</th><th>Stmts</th><th>PERF</th><th>CALL</th><th>SQL</th><th>MOVE</th><th>BR</th></tr></thead>
          <tbody>${paras}</tbody>
        </table>
      </details>`;
    }).join('');

    // Pre-rendered Mermaid block (if available)
    const mermaidBlock = mermaidText && mermaidText.trim().length > 0
      ? `<div class="mi-card" style="margin-top:12px;">
          <h3>Flow diagram (Mermaid, from REKT CFG)</h3>
          <div class="mi-mermaid-container">
            <pre class="mermaid">${this._escape(mermaidText)}</pre>
          </div>
        </div>`
      : '';

    return `
      <div class="mi-card">
        <h3>${this._escape(basename)} — semantic flow</h3>
        ${kpiRow}
        ${sectionBlocks}
        <div class="mi-source">
          Sourced from <code>/api/graph/rekt/structure</code> (sections + paragraphs) and
          <code>/api/graph/rekt/mermaid</code> (CFG diagram).
        </div>
      </div>
      ${mermaidBlock}
    `;
  }

  _renderMermaidIn(panel) {
    if (typeof window.mermaid === 'undefined') return;
    try {
      window.mermaid.run({ querySelector: 'pre.mermaid', nodes: panel.querySelectorAll('pre.mermaid') });
    } catch (err) {
      console.warn('Mermaid render failed:', err);
    }
  }

  // ────────────────────────────────────────────────────────────────────
  // Service Candidate Explorer renderer (Phase-1 follow-on)
  // ────────────────────────────────────────────────────────────────────
  _renderServiceCandidates(d) {
    const candidates = d.candidates || [];
    if (candidates.length === 0) {
      return `<div class="mi-empty">No service candidates available. Run a REKT scan first.</div>`;
    }
    const kpiRow = `
      <div class="mi-kpi-row">
        <div class="mi-kpi" style="border-left-color:#3b82f6;">
          <div class="mi-kpi-value">${d.totalCandidates}</div>
          <div class="mi-kpi-label">Candidate services</div>
          <div class="mi-kpi-sub">domain-prefix clustering with multi-signal cohesion scoring</div>
        </div>
        <div class="mi-kpi" style="border-left-color:#10b981;">
          <div class="mi-kpi-value">${d.extractionReadyCount}</div>
          <div class="mi-kpi-label">Ready for extraction</div>
          <div class="mi-kpi-sub">full-fidelity + boundary ≥ 70%</div>
        </div>
      </div>
    `;

    const rows = candidates.map(c => {
      const ready = c.readyForExtraction
        ? '<span class="mi-status mi-status-verified">✅ ready</span>'
        : '<span class="mi-status mi-status-partial-fallback">⏸ blocked</span>';
      const cohesionBand = c.cohesionScore >= 75 ? '#10b981'
                         : c.cohesionScore >= 50 ? '#f59e0b'
                         : '#ef4444';
      return `<tr class="mi-svc-row" data-name="${this._escape(c.suggestedName)}">
        <td><b>${this._escape(c.suggestedName)}</b><div class="mi-path">domain prefix: <code>${this._escape(c.domainPrefix)}*</code></div></td>
        <td class="num">${c.memberCount}</td>
        <td class="num">${c.totalLinesOfCode.toLocaleString()}</td>
        <td class="num">${c.intraClusterEdges}</td>
        <td class="num">${c.crossClusterEdges}</td>
        <td class="num">${c.boundaryStrengthPct}%</td>
        <td class="num"><b style="color:${cohesionBand};">${c.cohesionScore}</b></td>
        <td class="num">${c.fullFidelityCount} / ${c.memberCount}</td>
        <td>${ready}</td>
      </tr>`;
    }).join('');

    // Detail panels: one per candidate listing members
    const details = candidates.map(c => `
      <div class="mi-card" data-svc-detail="${this._escape(c.suggestedName)}" style="display:none;">
        <h3>${this._escape(c.suggestedName)} — ${c.memberCount} program${c.memberCount !== 1 ? 's' : ''}</h3>
        <div class="ih-callout" style="font-size:12px;">
          <b>Cohesion:</b> ${c.cohesionScore}/100 ·
          <b>Boundary strength:</b> ${c.boundaryStrengthPct}% ·
          <b>Intra/cross edges:</b> ${c.intraClusterEdges}/${c.crossClusterEdges} ·
          <b>Avg facts confidence:</b> ${c.avgFactsConfidence}/3 ·
          <b>Full-fidelity:</b> ${c.fullFidelityCount}/${c.memberCount} ·
          <b>Total LoC:</b> ${c.totalLinesOfCode.toLocaleString()}
        </div>
        <div style="display:flex; flex-wrap:wrap; gap:4px; margin-top:8px;">
          ${c.memberPrograms.map(m => `<code class="mi-mini" style="background:#1e293b; padding:3px 8px; border-radius:3px;">${this._escape(m)}</code>`).join('')}
        </div>
      </div>
    `).join('');

    return `
      ${kpiRow}
      <div class="mi-card mi-card-wide">
        <h3>Service candidate ranking — by cohesion score</h3>
        <div class="mi-muted" style="font-size:11px; margin-bottom:8px;">
          Multi-signal heuristic: boundary strength (60%) + cluster size (20%) + facts confidence (20%).
          Click a row to see member programs. Programs sharing CALL paths AND prefix are stronger candidates.
        </div>
        <table class="mi-table mi-table-dense">
          <thead>
            <tr>
              <th>Suggested service</th>
              <th>Members</th>
              <th>Total LoC</th>
              <th title="CALL edges WITHIN the cluster">Intra</th>
              <th title="CALL edges OUT of the cluster">Cross</th>
              <th>Boundary</th>
              <th>Cohesion</th>
              <th>Fidelity</th>
              <th>Ready?</th>
            </tr>
          </thead>
          <tbody>${rows}</tbody>
        </table>
      </div>
      <div id="mi-svc-details" style="margin-top:12px;">${details}</div>
    `;
  }

  // ────────────────────────────────────────────────────────────────────
  // Migration Wave Planner renderer (Phase-2 first WRITE capability)
  // ────────────────────────────────────────────────────────────────────
  _renderWavePlanner(apps, waveAssignments, health) {
    // Index user wave assignments by basename
    const userMap = {};
    for (const w of (waveAssignments || [])) userMap[w.basename] = w;

    // Build CALL-derived suggested waves (same logic as Insights Lead view):
    // Wave 1 = leaves, Wave 2 = ≤2 callees, Wave 3 = more, Queued = blocked
    const fidelityByName = {};
    for (const p of (health.programs || [])) fidelityByName[p.basename] = p.parseFidelity;

    const inEstate = new Set(apps.map(a => a.basename));

    // Build user-vs-suggested view per program
    const enriched = apps.map(a => {
      const u = userMap[a.basename];
      const fid = fidelityByName[a.basename];
      let suggested;
      if (fid !== 'full') suggested = -1;  // queued
      else suggested = 1; // simple default — backend uses topology for refined version
      return {
        ...a,
        userWave: u ? u.waveNumber : null,
        userSource: u ? u.source : null,
        userNotes: u ? u.notes : null,
        suggestedWave: suggested,
        parseFidelity: fid,
      };
    });

    // Group by current wave assignment (user > suggested > unassigned)
    const buckets = { wave1: [], wave2: [], wave3: [], queued: [], unassigned: [] };
    for (const e of enriched) {
      const w = e.userWave !== null ? e.userWave : null;
      if (w === 1) buckets.wave1.push(e);
      else if (w === 2) buckets.wave2.push(e);
      else if (w === 3) buckets.wave3.push(e);
      else if (w === -1 || (w === null && e.parseFidelity !== 'full')) buckets.queued.push(e);
      else buckets.unassigned.push(e);
    }

    const totalAssigned = buckets.wave1.length + buckets.wave2.length + buckets.wave3.length;
    const assignedPct = apps.length > 0 ? Math.round(totalAssigned * 100 / apps.length) : 0;

    const kpiRow = `
      <div class="mi-kpi-row">
        <div class="mi-kpi" style="border-left-color:#10b981;"><div class="mi-kpi-value">${buckets.wave1.length}</div><div class="mi-kpi-label">Wave 1</div><div class="mi-kpi-sub">first to migrate</div></div>
        <div class="mi-kpi" style="border-left-color:#f59e0b;"><div class="mi-kpi-value">${buckets.wave2.length}</div><div class="mi-kpi-label">Wave 2</div><div class="mi-kpi-sub">after Wave 1 stable</div></div>
        <div class="mi-kpi" style="border-left-color:#fb923c;"><div class="mi-kpi-value">${buckets.wave3.length}</div><div class="mi-kpi-label">Wave 3</div><div class="mi-kpi-sub">most complex</div></div>
        <div class="mi-kpi" style="border-left-color:#ef4444;"><div class="mi-kpi-value">${buckets.queued.length}</div><div class="mi-kpi-label">Queued</div><div class="mi-kpi-sub">blocked by missing facts</div></div>
        <div class="mi-kpi" style="border-left-color:#475569;"><div class="mi-kpi-value">${buckets.unassigned.length}</div><div class="mi-kpi-label">Unassigned</div><div class="mi-kpi-sub">${assignedPct}% of estate planned</div></div>
      </div>
    `;

    const renderBucket = (rows, label, color, waveNum) => `
      <div class="mi-card mi-wave-col">
        <h3 style="color:${color};">${label}</h3>
        <div class="mi-muted" style="font-size:11px; margin-bottom:8px;">${rows.length} program${rows.length !== 1 ? 's' : ''}</div>
        ${rows.length === 0 ? '<div class="mi-muted" style="padding:12px; text-align:center; font-size:11px;">drop programs here</div>'
          : rows.sort((a,b) => a.linesOfCode - b.linesOfCode).map(r => `
            <div class="mi-wave-card" data-basename="${this._escape(r.basename)}" data-wave="${waveNum}">
              <div style="display:flex; justify-content:space-between; gap:6px;">
                <b style="font-size:12px;">${this._escape(r.basename)}</b>
                <span class="mi-muted" style="font-size:10px;">${r.linesOfCode.toLocaleString()} LoC</span>
              </div>
              <div style="display:flex; align-items:center; gap:6px; margin-top:4px;">
                <select class="mi-wave-select" data-basename="${this._escape(r.basename)}" style="background:#0a0e1a; color:#cbd5e1; border:1px solid #334155; border-radius:3px; padding:2px 6px; font-size:11px; flex:1;">
                  <option value="0" ${r.userWave === null && r.parseFidelity === 'full' ? 'selected' : ''}>Unassigned</option>
                  <option value="1" ${r.userWave === 1 ? 'selected' : ''}>Wave 1</option>
                  <option value="2" ${r.userWave === 2 ? 'selected' : ''}>Wave 2</option>
                  <option value="3" ${r.userWave === 3 ? 'selected' : ''}>Wave 3</option>
                  <option value="-1" ${r.userWave === -1 || (r.userWave === null && r.parseFidelity !== 'full') ? 'selected' : ''}>Queued/blocked</option>
                </select>
                ${r.userWave !== null ? `<button class="mi-wave-clear" data-basename="${this._escape(r.basename)}" title="Clear user assignment (revert to auto)" style="background:#1e293b; color:#94a3b8; border:1px solid #334155; border-radius:3px; padding:2px 6px; cursor:pointer; font-size:10px;">×</button>` : ''}
              </div>
              ${r.parseFidelity !== 'full' ? `<div class="mi-muted" style="font-size:10px; margin-top:3px;">⚠ deps-only (resolve copybooks first)</div>` : ''}
            </div>
          `).join('')}
      </div>
    `;

    return `
      ${kpiRow}
      <div class="ih-callout">
        <b>How this works:</b> change a program's wave assignment with the dropdown — it persists to <code>Data/migration-waves.db</code> via <code>POST /api/modernization/waves/{basename}</code>.
        Wave 1 → Wave 2 → Wave 3 is the recommended order; within each wave migrate smallest-LoC first to build confidence.
        Programs without full REKT facts are "Queued/blocked" — resolve their missing copybooks before assigning to a wave.
      </div>
      <div style="display:flex; gap:10px; margin-bottom:12px;">
        <button class="mi-btn" onclick="window.modernizationIntelligenceView._autoSuggestWaves()" title="Bulk-assign Wave 1 to all full-fidelity programs, Queued to deps-only">🪄 Auto-suggest from fidelity</button>
        <button class="mi-btn" onclick="window.modernizationIntelligenceView._clearAllWaves()" title="Remove all wave assignments (DELETE /api/modernization/waves)" style="background:#7c2d12; border-color:#ea580c; color:#fef3c7;">🗑 Clear all</button>
        <button class="mi-btn" onclick="window.modernizationIntelligenceView._exportWavePlan()" title="Download the current wave plan as CSV">⬇ Export plan (CSV)</button>
      </div>
      <div class="mi-wave-grid">
        ${renderBucket(buckets.wave1, '🚀 Wave 1 — Foundation', '#10b981', 1)}
        ${renderBucket(buckets.wave2, '🌊 Wave 2 — Core', '#f59e0b', 2)}
        ${renderBucket(buckets.wave3, '🌋 Wave 3 — Complex', '#fb923c', 3)}
        ${renderBucket(buckets.queued, '⏸ Queued — Blocked', '#ef4444', -1)}
        ${renderBucket(buckets.unassigned, '— Unassigned', '#475569', 0)}
      </div>
    `;
  }

  _wireWavePlannerInteractions() {
    // Dropdown changes → POST to /api/modernization/waves/{basename}
    this.root.querySelectorAll('.mi-wave-select').forEach(sel => {
      sel.addEventListener('change', async () => {
        const basename = sel.dataset.basename;
        const wave = parseInt(sel.value, 10);
        try {
          await fetch(`/api/modernization/waves/${encodeURIComponent(basename)}`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ waveNumber: wave, notes: null }),
          });
          // Re-render to reflect new bucket assignment
          this._renderActive();
        } catch (err) {
          alert(`Failed to save: ${err.message}`);
        }
      });
    });
    // Clear buttons → DELETE /api/modernization/waves/{basename}
    this.root.querySelectorAll('.mi-wave-clear').forEach(btn => {
      btn.addEventListener('click', async () => {
        const basename = btn.dataset.basename;
        try {
          await fetch(`/api/modernization/waves/${encodeURIComponent(basename)}`, { method: 'DELETE' });
          this._renderActive();
        } catch (err) {
          alert(`Failed to clear: ${err.message}`);
        }
      });
    });
    // Service candidate row clicks → toggle detail panel (also wired separately
    // from the 'services' subview path; this duplicate kept for safety when
    // the Wave Planner shows service-candidate rows in its sidebar).
    this._wireServiceCandidatesInteractions();
  }

  /**
   * Wires click-to-expand for service-candidate rows in the 'services'
   * subview. Each row toggles a detail panel below the table; clicking
   * the active row again collapses it. Also scrolls the detail into view
   * and highlights the active row.
   */
  _wireServiceCandidatesInteractions() {
    const rows = this.root.querySelectorAll('.mi-svc-row');
    if (rows.length === 0) return;
    rows.forEach(row => {
      // Only wire once.
      if (row.dataset.wired === '1') return;
      row.dataset.wired = '1';
      row.style.cursor = 'pointer';
      row.addEventListener('click', () => {
        const name = row.dataset.name;
        // Find current state — second click on active row collapses
        const detail = this.root.querySelector(`[data-svc-detail="${CSS.escape(name)}"]`);
        const wasOpen = detail && detail.style.display === 'block';

        // Reset all rows + panels
        this.root.querySelectorAll('.mi-svc-row').forEach(r =>
          r.classList.toggle('mi-svc-row-active', false));
        this.root.querySelectorAll('[data-svc-detail]').forEach(d => {
          d.style.display = 'none';
        });

        if (!wasOpen && detail) {
          detail.style.display = 'block';
          row.classList.add('mi-svc-row-active');
          // Scroll the detail card into view so the user actually sees the change
          setTimeout(() => detail.scrollIntoView({ behavior: 'smooth', block: 'nearest' }), 50);
        }
      });
    });
  }

  async _autoSuggestWaves() {
    if (!confirm('Auto-assign Wave 1 to all full-fidelity programs and Queued to deps-only programs? This OVERWRITES existing user assignments.')) return;
    try {
      const [apps, health] = await Promise.all([
        fetch('/api/modernization/applications').then(r => r.json()),
        fetch('/api/modernization/dependency-health').then(r => r.json()),
      ]);
      const fidelityMap = {};
      for (const p of (health.programs || [])) fidelityMap[p.basename] = p.parseFidelity;
      const promises = apps.map(a => {
        const wave = fidelityMap[a.basename] === 'full' ? 1 : -1;
        return fetch(`/api/modernization/waves/${encodeURIComponent(a.basename)}`, {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ waveNumber: wave, notes: 'auto-suggested' }),
        });
      });
      await Promise.all(promises);
      this._renderActive();
    } catch (err) { alert(`Auto-suggest failed: ${err.message}`); }
  }

  async _clearAllWaves() {
    if (!confirm('Clear ALL wave assignments? This cannot be undone.')) return;
    try {
      await fetch('/api/modernization/waves', { method: 'DELETE' });
      this._renderActive();
    } catch (err) { alert(`Clear failed: ${err.message}`); }
  }

  async _exportWavePlan() {
    try {
      const waves = await fetch('/api/modernization/waves').then(r => r.json());
      const lines = ['program,wave,notes,assigned_at,source'];
      for (const w of waves) {
        const wave = w.waveNumber === -1 ? 'queued' : w.waveNumber === 0 ? 'unassigned' : `wave-${w.waveNumber}`;
        const notes = (w.notes || '').replace(/[",\n]/g, ' ');
        lines.push(`${w.basename},${wave},"${notes}",${w.assignedAt},${w.source}`);
      }
      const blob = new Blob([lines.join('\n')], { type: 'text/csv;charset=utf-8' });
      const url = URL.createObjectURL(blob);
      const a = document.createElement('a');
      a.href = url;
      a.download = `migration-wave-plan-${new Date().toISOString().substring(0, 10)}.csv`;
      document.body.appendChild(a); a.click(); document.body.removeChild(a);
      URL.revokeObjectURL(url);
    } catch (err) { alert(`Export failed: ${err.message}`); }
  }

  // ────────────────────────────────────────────────────────────────────
  // Service Chain renderer — JCL → Program → Copybook overview
  // ────────────────────────────────────────────────────────────────────
  _renderServiceChain(d) {
    if ((d.totalJobs || 0) === 0 && (d.totalPrograms || 0) === 0) {
      return `<div class="mi-empty">No JCL files found. Drop <code>.jcl</code> files into <code>source/</code> and run <code>./doctor.sh rekt-full</code> for full coverage.</div>`;
    }

    const kpiRow = `
      <div class="mi-kpi-row">
        <div class="mi-kpi" style="border-left-color:#fb923c;">
          <div class="mi-kpi-value">${d.totalJobs}</div>
          <div class="mi-kpi-label">📅 JCL batch jobs</div>
          <div class="mi-kpi-sub">discovered via EXEC PGM=</div>
        </div>
        <div class="mi-kpi" style="border-left-color:#60a5fa;">
          <div class="mi-kpi-value">${d.totalPrograms}</div>
          <div class="mi-kpi-label">⚙ COBOL programs</div>
          <div class="mi-kpi-sub">execution targets</div>
        </div>
        <div class="mi-kpi" style="border-left-color:#10b981;">
          <div class="mi-kpi-value">${d.totalCopybooks}</div>
          <div class="mi-kpi-label">📄 Copybooks (shared structures)</div>
          <div class="mi-kpi-sub">distinct, referenced via COPY</div>
        </div>
        <div class="mi-kpi" style="border-left-color:#8b5cf6;">
          <div class="mi-kpi-value">${d.jobToProgramEdges} / ${d.programToCopybookEdges}</div>
          <div class="mi-kpi-label">Job→Pgm / Pgm→Cpy edges</div>
          <div class="mi-kpi-sub">execution relationships</div>
        </div>
      </div>
    `;

    // Sort jobs by program count desc (most impactful first)
    const sortedJobs = [...(d.jobs || [])].sort((a, b) => b.primaryPrograms.length - a.primaryPrograms.length);
    const jobRows = sortedJobs.map(j => `
      <tr class="mi-chain-job-row" data-job="${this._escape(j.jobName)}">
        <td><b>${this._escape(j.jobName)}</b></td>
        <td><code class="mi-mini">${this._escape(j.jclFileName)}</code></td>
        <td class="num">${j.primaryPrograms.length}</td>
      </tr>`).join('');

    // Sort programs by (jobs+copybooks) desc — most-connected first
    const sortedPgms = [...(d.programs || [])]
      .map(p => ({ ...p, connectivity: (p.calledByJobs?.length || 0) + (p.copybooks?.length || 0) }))
      .sort((a, b) => b.connectivity - a.connectivity);
    const pgmRows = sortedPgms.slice(0, 60).map(p => `
      <tr class="mi-chain-pgm-row" data-program="${this._escape(p.basename)}">
        <td><b>${this._escape(p.basename)}</b></td>
        <td class="num">${p.linesOfCode.toLocaleString()}</td>
        <td class="num">${p.calledByJobs.length}</td>
        <td class="num">${p.copybooks.length}</td>
        <td><span class="mi-status mi-status-${p.modernizationStatus}">${this._statusLabel(p.modernizationStatus)}</span></td>
      </tr>`).join('');

    return `
      ${kpiRow}
      <div class="ih-callout">
        <b>How to read this view:</b> 📅 <i>JCL job</i> launches one or more ⚙ <i>COBOL programs</i> via <code>EXEC PGM=</code>;
        each program then <code>COPY</code>-s one or more 📄 <i>copybooks</i> (shared data structures).
        Click a row in either list to filter the diagram to that subgraph.
        System utilities (IDCAMS, IKJEFT01, SORT, etc.) are excluded.
      </div>

      <div class="mi-chain-layout">
        <div class="mi-card mi-chain-col">
          <h3>📅 JCL jobs (${d.totalJobs})</h3>
          <div class="mi-muted" style="font-size:11px; margin-bottom:8px;">Click to filter the chain to one job.</div>
          <table class="mi-table mi-table-dense">
            <thead><tr><th>Job</th><th>JCL file</th><th>Pgms</th></tr></thead>
            <tbody>${jobRows}</tbody>
          </table>
        </div>
        <div class="mi-card mi-chain-col">
          <h3>⚙ COBOL programs (${d.totalPrograms})</h3>
          <div class="mi-muted" style="font-size:11px; margin-bottom:8px;">Click to filter the chain to one program + its copybooks.</div>
          <table class="mi-table mi-table-dense">
            <thead><tr><th>Program</th><th>LoC</th><th>Jobs</th><th>Cpys</th><th>Status</th></tr></thead>
            <tbody>${pgmRows}</tbody>
          </table>
        </div>
      </div>

      <div class="mi-card mi-card-wide">
        <h3>🎨 Service-chain flowchart
          <button class="mi-btn" id="mi-chain-reset" style="float:right; font-size:11px;" title="Show the full estate">↺ Reset filter</button>
        </h3>
        <div class="mi-muted" style="font-size:11px; margin-bottom:8px;">
          Orange = JCL job · Blue = COBOL program · Green = Copybook. Solid arrow = EXEC PGM=, dashed = COPY.
        </div>
        <div class="mi-mermaid-container" id="mi-chain-mermaid">
          <pre class="mermaid">${this._escape(d.mermaid || 'flowchart LR\n  empty[\"(no data)\"]')}</pre>
        </div>
      </div>

      <div class="mi-source">
        Sourced from <code>source/**/*.JCL</code> (regex on <code>EXEC PGM=</code>) + <code>output/rekt/*.facts.json</code> (copybook lists). System utilities filtered out.
      </div>
    `;
  }

  _wireServiceChainInteractions(initialData) {
    const reloadFiltered = async (filterUrl) => {
      const body = this.root.querySelector('#mi-body');
      const mermContainer = body.querySelector('#mi-chain-mermaid');
      if (mermContainer) mermContainer.innerHTML = '<div class="mi-loading">Loading filtered chain…</div>';
      try {
        const d = await fetch(filterUrl).then(r => r.json());
        if (mermContainer) {
          mermContainer.innerHTML = `<pre class="mermaid">${this._escape(d.mermaid || 'flowchart LR\n  empty[\"(no data)\"]')}</pre>`;
          // Mermaid library needs a re-run on the newly-inserted pre.
          setTimeout(() => this._renderMermaidIn(body), 30);
        }
      } catch (err) {
        if (mermContainer) mermContainer.innerHTML = `<div class="mi-error">${this._escape(err.message)}</div>`;
      }
    };

    this.root.querySelectorAll('.mi-chain-job-row').forEach(tr => {
      tr.style.cursor = 'pointer';
      tr.addEventListener('click', () => {
        this.root.querySelectorAll('.mi-chain-job-row, .mi-chain-pgm-row').forEach(r =>
          r.classList.toggle('mi-row-active', r === tr));
        reloadFiltered(`/api/modernization/service-chain?job=${encodeURIComponent(tr.dataset.job)}`);
      });
    });
    this.root.querySelectorAll('.mi-chain-pgm-row').forEach(tr => {
      tr.style.cursor = 'pointer';
      tr.addEventListener('click', () => {
        this.root.querySelectorAll('.mi-chain-job-row, .mi-chain-pgm-row').forEach(r =>
          r.classList.toggle('mi-row-active', r === tr));
        reloadFiltered(`/api/modernization/service-chain?program=${encodeURIComponent(tr.dataset.program)}`);
      });
    });
    const resetBtn = this.root.querySelector('#mi-chain-reset');
    if (resetBtn) {
      resetBtn.addEventListener('click', () => {
        this.root.querySelectorAll('.mi-row-active').forEach(r => r.classList.remove('mi-row-active'));
        reloadFiltered('/api/modernization/service-chain');
      });
    }
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
  async _downloadCopybookShoppingList() {
    // Re-fetch fresh so the list reflects current state, not the cached snapshot.
    try {
      const d = await fetch('/api/modernization/dependency-health').then(r => r.json());
      const missing = (d.missingCopybooks || []).sort((a, b) => b.referencedBy.length - a.referencedBy.length);
      const lines = [];
      lines.push('# Missing COBOL Copybooks — Modernization Intelligence shopping list');
      lines.push(`# Generated: ${new Date().toISOString()}`);
      lines.push(`# Estate readiness: ${d.readinessScore}% — resolve these to lift coverage.`);
      lines.push('#');
      lines.push('# Format: COPYBOOK_NAME    <programs_blocked>    <comma-separated programs>');
      lines.push('# Order: most-impactful first (highest blast radius).');
      lines.push('#');
      for (const m of missing) {
        lines.push(`${m.copybook.padEnd(16)}\t${String(m.referencedBy.length).padStart(3)} programs\t${m.referencedBy.join(', ')}`);
      }
      lines.push('');
      lines.push(`# Total: ${missing.length} unresolved copybooks blocking ${d.programsBlockedByMissing} programs.`);
      lines.push(`# After acquiring: drop *.cpy files into source/ and re-run ./doctor.sh rekt-full.`);
      const blob = new Blob([lines.join('\n')], { type: 'text/plain;charset=utf-8' });
      const url = URL.createObjectURL(blob);
      const a = document.createElement('a');
      a.href = url;
      a.download = `missing-copybooks-shopping-list-${new Date().toISOString().substring(0, 10)}.txt`;
      document.body.appendChild(a);
      a.click();
      document.body.removeChild(a);
      URL.revokeObjectURL(url);
    } catch (err) {
      alert(`Failed to generate shopping list: ${err.message}`);
    }
  }

  // ────────────────────────────────────────────────────────────────────
  // 🎯 Capabilities & Service Locator
  // ────────────────────────────────────────────────────────────────────
  _renderCapabilities(catalog) {
    const total = (catalog.capabilities || []).reduce((a, b) => a + b.programs.length, 0);
    const populated = (catalog.capabilities || []).filter(b => b.programs.length > 0);
    return `
      <div class="mi-section">
        <h3>🔎 Service Locator — find any generated service back to its COBOL source (Java + C#)</h3>
        <p class="mi-help">
          Type a generated class name, paragraph name, or program-ID (e.g. <code>CalcInterestService</code>,
          <code>CALC_INTEREST</code>, <code>CHECK-FRAUD</code>, or just <code>BDSM043</code>).
          The locator normalises across casing/styles and searches generated <b>Java</b> + <b>C#</b>
          under <code>output/runs/**</code>, <code>output/java/**</code>, <code>output/csharp/**</code>,
          and the original COBOL source.
        </p>
        <div class="mi-locator-row">
          <input id="mi-locator-input" type="text" placeholder="e.g. CALC_INTEREST or CalcInterestService or BDSM043" class="mi-locator-input"/>
          <button id="mi-locator-btn" class="mi-btn-primary">🔎 Locate</button>
        </div>
        <div id="mi-locator-results"></div>
      </div>

      <div class="mi-section">
        <h3>🧠 Semantic Search — find anything by intent, not just name</h3>
        <p class="mi-help">
          Type what you're <em>looking for</em> in plain English (e.g. <code>interest accrual</code>,
          <code>customer onboarding</code>, <code>fraud detection</code>). The search expands your
          query against the capability dictionary, then ranks every COBOL program by hits on
          paragraph names, CALL targets, SQL tables, data groups, copybooks, AND raw source text
          (catches keywords in comments).
        </p>
        <div class="mi-locator-row">
          <input id="mi-semantic-input" type="text" placeholder="e.g. interest accrual · customer onboarding · payment settlement" class="mi-locator-input"/>
          <button id="mi-semantic-btn" class="mi-btn-primary">🧠 Search by intent</button>
        </div>
        <div id="mi-semantic-results"></div>
      </div>

      <div class="mi-section">
        <h3>🎯 Business Capabilities — REKT-driven discovery (${populated.length} active · ${total} classifications)</h3>
        <p class="mi-help">
          Each program is scored against the keyword dictionary in <code>Data/capabilities.json</code>
          using paragraph names, CALL targets, SQL tables, data groups and copybook names from REKT facts.
          Multi-label: a program can serve multiple capabilities.
        </p>
        <div style="margin: 10px 0;">
          <button class="mi-btn" onclick="window.modernizationIntelligenceView._openCapabilityEditor();">✏️ Edit capabilities dictionary</button>
          <span class="mi-help" style="margin-left:10px;">Edit, validate, and save without leaving the portal. Auto-snapshots the previous version to <code>Data/_history/</code>.</span>
        </div>

        <div class="mi-cap-grid">
          ${populated.map(b => this._renderCapabilityCard(b)).join('')}
        </div>

        ${catalog.unclassified.length > 0 ? `
          <div class="mi-cap-unclassified">
            <h4>⚪ Unclassified (${catalog.unclassified.length} programs)</h4>
            <p class="mi-help">
              No keyword hits — likely candidates for a new capability entry, or programs
              that are pure technical plumbing. Consider adding keywords to
              <code>Data/capabilities.json</code> if any of these belong to a business domain.
            </p>
            <div class="mi-cap-chips">
              ${catalog.unclassified.map(b => `<span class="mi-chip">${this._escape(b)}</span>`).join('')}
            </div>
          </div>` : ''}
      </div>
    `;
  }

  _renderCapabilityCard(bucket) {
    const top = bucket.programs.slice(0, 6);
    const more = Math.max(0, bucket.programs.length - top.length);
    return `
      <div class="mi-cap-card" data-cap="${this._escape(bucket.id)}">
        <div class="mi-cap-header">
          <span class="mi-cap-emoji">${bucket.emoji}</span>
          <div>
            <div class="mi-cap-title">${this._escape(bucket.display)}</div>
            <div class="mi-cap-sub">${bucket.programs.length} programs${bucket.bian.length ? ` · BIAN: ${bucket.bian.map(this._escape).join(', ')}` : ''}</div>
          </div>
        </div>
        <table class="mi-cap-table">
          <tbody>
            ${top.map(p => `
              <tr>
                <td><code>${this._escape(p.basename)}</code></td>
                <td><span class="mi-cap-conf" style="background:${this._capColor(p.confidence)};">conf ${(p.confidence * 100).toFixed(0)}%</span></td>
                <td class="mi-cap-hits" title="${this._escape(p.hits.slice(0, 5).map(h => `${h.source}:${h.match} (${h.keyword})`).join('\n'))}">
                  ${p.hits.slice(0, 3).map(h => `<span class="mi-chip mi-chip-tiny" title="${this._escape(h.source + ': ' + h.keyword)}">${this._escape(h.match)}</span>`).join(' ')}
                </td>
              </tr>`).join('')}
            ${more > 0 ? `<tr><td colspan="3" class="mi-muted">… and ${more} more</td></tr>` : ''}
          </tbody>
        </table>
      </div>
    `;
  }

  _capColor(conf) {
    if (conf >= 0.75) return '#10b98144';
    if (conf >= 0.5) return '#f59e0b44';
    return '#64748b44';
  }

  _wireCapabilitiesInteractions() {
    const input = this.root.querySelector('#mi-locator-input');
    const btn = this.root.querySelector('#mi-locator-btn');
    const results = this.root.querySelector('#mi-locator-results');
    if (!input || !btn || !results) return;
    const run = async () => {
      const q = input.value.trim();
      if (!q) return;
      results.innerHTML = '<div class="mi-loading">🔎 Searching…</div>';
      try {
        const r = await fetch(`/api/modernization/locate?q=${encodeURIComponent(q)}`).then(x => x.json());
        results.innerHTML = this._renderLocatorResults(r);
      } catch (err) {
        results.innerHTML = `<div class="mi-error">${this._escape(err.message)}</div>`;
      }
    };
    btn.addEventListener('click', run);
    input.addEventListener('keydown', e => { if (e.key === 'Enter') run(); });

    // #9 semantic search
    const semInput = this.root.querySelector('#mi-semantic-input');
    const semBtn = this.root.querySelector('#mi-semantic-btn');
    const semResults = this.root.querySelector('#mi-semantic-results');
    if (semInput && semBtn && semResults) {
      const runSem = async () => {
        const q = semInput.value.trim();
        if (!q) return;
        semResults.innerHTML = '<div class="mi-loading">🧠 Searching by intent…</div>';
        try {
          const r = await fetch(`/api/modernization/semantic-search?q=${encodeURIComponent(q)}`).then(x => x.json());
          semResults.innerHTML = this._renderSemanticResults(r);
        } catch (err) {
          semResults.innerHTML = `<div class="mi-error">${this._escape(err.message)}</div>`;
        }
      };
      semBtn.addEventListener('click', runSem);
      semInput.addEventListener('keydown', e => { if (e.key === 'Enter') runSem(); });
    }
  }

  _renderSemanticResults(r) {
    if (!r.programs || r.programs.length === 0) {
      return `<div class="mi-cap-empty">
        <b>No programs match "<code>${this._escape(r.query)}</code>"</b>
        <div class="mi-help">Tokens tried: ${(r.tokens || []).map(t => `<code>${this._escape(t)}</code>`).join(', ') || '<i>(none — query too short)</i>'}</div>
        <div class="mi-help">Tip: try broader terms ("interest" instead of "interest accrual calculations") or check the capability dictionary.</div>
      </div>`;
    }
    return `
      <div style="margin-bottom:10px; font-size:11px; color:var(--text-muted);">
        Tokens: ${(r.tokens || []).map(t => `<code>${this._escape(t)}</code>`).join(' · ')}
        ${r.matchedCapabilities.length > 0 ? `· Expanded via capability${r.matchedCapabilities.length === 1 ? '' : 'ies'}: ${r.matchedCapabilities.map(c => `<b>${this._escape(c)}</b>`).join(', ')}` : ''}
        · Expanded keywords (${r.expandedKeywords.length}): <span class="mi-muted">${(r.expandedKeywords || []).slice(0, 12).map(k => this._escape(k)).join(', ')}${r.expandedKeywords.length > 12 ? '…' : ''}</span>
      </div>
      <table class="mi-table mi-table-dense">
        <thead><tr><th>Program</th><th class="num">Score</th><th>Top hits</th><th>Actions</th></tr></thead>
        <tbody>
          ${r.programs.map(p => `
            <tr>
              <td><code>${this._escape(p.basename)}</code></td>
              <td class="num"><b style="color:var(--color-info);">${p.score.toFixed(1)}</b></td>
              <td>
                ${p.hits.slice(0, 5).map(h =>
                  `<span class="mi-chip mi-chip-tiny" title="source=${this._escape(h.source)} · keyword=${this._escape(h.keyword)}">${this._escape(h.match)}</span>`
                ).join(' ')}
              </td>
              <td>${PortalProgramActions.buttons(p.basename)}</td>
            </tr>
          `).join('')}
        </tbody>
      </table>
    `;
  }

  // ────────────────────────────────────────────────────────────────────
  // #15 In-portal capabilities.json editor
  // ────────────────────────────────────────────────────────────────────
  async _openCapabilityEditor() {
    let modal = document.getElementById('mi-cap-edit-modal');
    if (!modal) {
      modal = document.createElement('div');
      modal.id = 'mi-cap-edit-modal';
      modal.className = 'mi-modal';
      document.body.appendChild(modal);
      modal.addEventListener('click', (e) => {
        if (e.target === modal) modal.style.display = 'none';
      });
    }
    modal.style.display = 'flex';
    modal.innerHTML = `
      <div class="mi-modal-card mi-modal-card-wide" onclick="event.stopPropagation();">
        <div class="mi-modal-header">
          <div>
            <div class="mi-modal-title">✏️ Edit Data/capabilities.json</div>
            <div class="mi-modal-sub">Add or tune business capabilities. Save validates JSON + snapshots prev version to <code>Data/_history/</code>.</div>
          </div>
          <button class="mi-btn" onclick="document.getElementById('mi-cap-edit-modal').style.display='none';">✕ Close</button>
        </div>
        <div class="mi-cap-edit-toolbar">
          <button class="mi-btn mi-btn-primary" onclick="window.modernizationIntelligenceView._saveCapabilities();">💾 Save & re-classify</button>
          <button class="mi-btn" onclick="window.modernizationIntelligenceView._validateCapabilitiesJson();">✓ Validate JSON</button>
          <button class="mi-btn" onclick="window.modernizationIntelligenceView._addCapabilityTemplate();">➕ Add new capability template</button>
          <span id="mi-cap-edit-status" class="mi-help" style="margin-left:auto;"></span>
        </div>
        <textarea id="mi-cap-edit-area" class="mi-cap-edit-area" spellcheck="false">Loading…</textarea>
        <div class="mi-help" style="margin-top:8px;font-size:11px;">
          <b>Schema:</b> each capability needs <code>id</code>, <code>emoji</code>, <code>display</code>, and a
          <code>keywords</code> array. Keywords match against paragraph names (×3), CALL targets (×2),
          SQL tables (×2), data groups (×2), copybook names (×1). Short keywords (&lt; 5 chars) require
          a token-boundary match to avoid false positives like <code>str</code> inside <code>TRATAR</code>.
          Optional <code>bian</code> array maps to BIAN service domains.
        </div>
      </div>
    `;
    const area = document.getElementById('mi-cap-edit-area');
    try {
      const raw = await fetch('/api/modernization/capabilities/raw').then(r => r.text());
      area.value = raw;
    } catch (err) {
      area.value = `// Failed to load: ${err.message}`;
    }
  }

  _validateCapabilitiesJson() {
    const area = document.getElementById('mi-cap-edit-area');
    const status = document.getElementById('mi-cap-edit-status');
    try {
      const d = JSON.parse(area.value);
      if (!Array.isArray(d.capabilities))
        throw new Error("missing 'capabilities' array at root");
      const ids = new Set();
      for (const c of d.capabilities) {
        if (!c.id) throw new Error("a capability is missing 'id'");
        if (ids.has(c.id)) throw new Error(`duplicate id: ${c.id}`);
        ids.add(c.id);
        if (!Array.isArray(c.keywords) || c.keywords.length === 0)
          throw new Error(`capability '${c.id}' has empty 'keywords' — add at least one keyword`);
      }
      status.innerHTML = `<span style="color:var(--color-success);">✓ Valid · ${d.capabilities.length} capabilities, ${[...ids].length} unique IDs</span>`;
      return true;
    } catch (err) {
      status.innerHTML = `<span style="color:var(--color-fail);">✗ ${this._escape(err.message)}</span>`;
      return false;
    }
  }

  async _saveCapabilities() {
    if (!this._validateCapabilitiesJson()) return;
    const area = document.getElementById('mi-cap-edit-area');
    const status = document.getElementById('mi-cap-edit-status');
    status.innerHTML = '<span class="mi-muted">Saving…</span>';
    try {
      const resp = await fetch('/api/modernization/capabilities/raw', {
        method: 'PUT',
        headers: { 'Content-Type': 'application/json' },
        body: area.value,
      });
      const result = await resp.json();
      if (!resp.ok) throw new Error(result.error || `HTTP ${resp.status}`);
      status.innerHTML = `<span style="color:var(--color-success);">✓ Saved ${result.bytes} bytes · prev version snapshotted to Data/_history/. Re-classifying…</span>`;
      // Trigger a re-render of the capabilities subview
      setTimeout(async () => {
        document.getElementById('mi-cap-edit-modal').style.display = 'none';
        const catalog = await fetch('/api/modernization/capabilities').then(r => r.json());
        const body = this.root.querySelector('#mi-body') || this.root.querySelector('.mi-body');
        if (body) body.innerHTML = this._renderCapabilities(catalog);
        this._wireCapabilitiesInteractions();
      }, 1200);
    } catch (err) {
      status.innerHTML = `<span style="color:var(--color-fail);">✗ Save failed: ${this._escape(err.message)}</span>`;
    }
  }

  _addCapabilityTemplate() {
    const area = document.getElementById('mi-cap-edit-area');
    try {
      const d = JSON.parse(area.value);
      d.capabilities.push({
        id: 'NEW_CAPABILITY',
        emoji: '🆕',
        display: 'New Capability',
        keywords: ['add', 'your', 'keywords', 'here'],
        bian: [],
      });
      area.value = JSON.stringify(d, null, 2);
      // Scroll to the bottom so the new entry is visible
      area.scrollTop = area.scrollHeight;
      document.getElementById('mi-cap-edit-status').innerHTML =
        '<span style="color:var(--color-info);">➕ Template added at the bottom — edit the id, emoji, display, and keywords before saving.</span>';
    } catch (err) {
      document.getElementById('mi-cap-edit-status').innerHTML =
        `<span style="color:var(--color-fail);">✗ Can't add template — fix JSON first: ${this._escape(err.message)}</span>`;
    }
  }

  _renderLocatorResults(r) {
    if (r.javaMatches.length === 0 && r.cobolMatches.length === 0) {
      return `<div class="mi-cap-empty">
        <b>No matches for <code>${this._escape(r.query)}</code></b>
        <div class="mi-help">Tried these normalised forms: ${r.forms.map(f => `<code>${this._escape(f)}</code>`).join(', ')}</div>
        <div class="mi-help">Tip: search by COBOL paragraph (<code>CALC-INTEREST</code>), generated class name
        (<code>CalcInterestService</code> — Java or C#), or program-ID (<code>BDSM043</code>).</div>
      </div>`;
    }
    return `
      <div class="mi-locator-results">
        ${r.cobolMatches.length > 0 ? `
        <div class="mi-locator-section">
          <h4>📄 COBOL source matches (${r.cobolMatches.length})</h4>
          <table class="mi-table">
            <thead><tr><th>Program</th><th>Path</th><th>Matched paragraphs</th><th>Actions</th></tr></thead>
            <tbody>
              ${r.cobolMatches.map(m => `
                <tr>
                  <td><b>${this._escape(m.basename)}</b>${m.programIdMatch ? ' <span class="mi-chip mi-chip-tiny">PROGRAM-ID</span>' : ''}${m.basenameMatch ? ' <span class="mi-chip mi-chip-tiny mi-chip-blue">basename</span>' : ''}</td>
                  <td><code>${this._escape(m.relativePath)}</code></td>
                  <td>${m.matchedParagraphs.length === 0 ? '<span class="mi-muted">—</span>' : m.matchedParagraphs.map(p => `<span class="mi-chip mi-chip-tiny mi-chip-green">${this._escape(p)}</span>`).join(' ')}</td>
                  <td>
                    ${PortalProgramActions.buttons(m.basename)}
                    ${m.factsPath ? `<a class="ppa-btn" href="/${this._escape(m.factsPath)}" target="_blank" title="View raw facts.json">📄 facts</a>` : ''}
                  </td>
                </tr>`).join('')}
            </tbody>
          </table>
        </div>` : ''}

        ${r.javaMatches.length > 0 ? `
        <div class="mi-locator-section">
          <h4>☕ Generated code matches (${r.javaMatches.length})</h4>
          <table class="mi-table">
            <thead><tr><th>File</th><th>Path</th><th>Run folder</th><th>Language</th></tr></thead>
            <tbody>
              ${r.javaMatches.map(j => `
                <tr>
                  <td><b>${this._escape(j.fileName)}</b></td>
                  <td><code>${this._escape(j.path)}</code></td>
                  <td>${j.runFolder ? `<code class="mi-chip mi-chip-tiny">${this._escape(j.runFolder)}</code>` : '<span class="mi-muted">legacy</span>'}</td>
                  <td>${this._escape(j.language)}</td>
                </tr>`).join('')}
            </tbody>
          </table>
        </div>` : ''}

        <div class="mi-help">Forms tried: ${r.forms.map(f => `<code>${this._escape(f)}</code>`).join(', ')}</div>
      </div>
    `;
  }

  _escape(s) {
    if (s == null) return '';
    return String(s).replace(/[&<>"']/g, c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
  }
}

window.ModernizationIntelligenceView = ModernizationIntelligenceView;
