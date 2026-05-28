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
          <button class="mi-subtab mi-subtab-disabled" disabled title="Coming in Phase-1 PR-P2">⏱ Runtime &amp; Conversion Intelligence</button>
          <button class="mi-subtab mi-subtab-disabled" disabled title="Coming in Phase-1 PR-P3">🕸 Dependency Topology</button>
          <button class="mi-subtab mi-subtab-disabled" disabled title="Coming in Phase-1 PR-P3">🌊 Semantic Flow Explorer</button>
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
