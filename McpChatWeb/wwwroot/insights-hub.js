// ─────────────────────────────────────────────────────────────────────────
// Insights Hub — Phase-2 persona-driven decision surfaces
// ─────────────────────────────────────────────────────────────────────────
// PRINCIPLE: NO new backend collection. Pure presentation layer over the
// same /api/modernization/* + /api/graph/rekt/* endpoints already powering
// Modernization Intelligence. Each persona view is a CURATED COMPOSITION
// of widgets sourced from existing data, framed through the persona's
// decision-making lens.
//
// Personas:
//   💼 Business Owner       — outcomes / risk / progress
//   🏗 Enterprise Architect — structure / coupling / domains
//   🚀 Modernization Lead   — execution / sequencing / blockers
//   👨‍💻 Developer            — understanding / debugging / quality
//
// All data shared via the InsightsHub._data cache so persona switches
// don't re-fetch the same endpoints.
// ─────────────────────────────────────────────────────────────────────────

class InsightsHub {
  constructor(rootId) {
    this.root = document.getElementById(rootId);
    if (!this.root) return;
    this._activePersona = 'business';
    this._data = null;  // shared snapshot across personas
    this._renderShell();
  }

  _renderShell() {
    this.root.innerHTML = `
      <div class="ih-shell">
        <div class="ih-header">
          <div class="ih-title">
            <span class="ih-icon">🎯</span>
            <div>
              <div class="ih-title-main">Insights Hub</div>
              <div class="ih-title-sub">Persona-driven decisions over the same REKT, projection, telemetry, and graph foundations</div>
            </div>
          </div>
          <div class="ih-actions">
            <button id="ih-refresh" class="ih-btn">⟳ Refresh data</button>
          </div>
        </div>

        <div class="ih-persona-bar">
          <button class="ih-persona ih-persona-active" data-persona="business">
            <span class="ih-persona-emoji">💼</span>
            <span class="ih-persona-label">Business Owner</span>
            <span class="ih-persona-sub">Outcomes · risk · progress</span>
          </button>
          <button class="ih-persona" data-persona="architect">
            <span class="ih-persona-emoji">🏗</span>
            <span class="ih-persona-label">Enterprise Architect</span>
            <span class="ih-persona-sub">Structure · domains · services</span>
          </button>
          <button class="ih-persona" data-persona="lead">
            <span class="ih-persona-emoji">🚀</span>
            <span class="ih-persona-label">Modernization Lead</span>
            <span class="ih-persona-sub">Execution · sequencing</span>
          </button>
          <button class="ih-persona" data-persona="developer">
            <span class="ih-persona-emoji">👨‍💻</span>
            <span class="ih-persona-label">Developer</span>
            <span class="ih-persona-sub">Understanding · debugging</span>
          </button>
        </div>

        <div id="ih-body" class="ih-body"></div>
      </div>
    `;
    this.root.querySelectorAll('.ih-persona').forEach(btn => {
      btn.addEventListener('click', () => {
        this._activePersona = btn.dataset.persona;
        this.root.querySelectorAll('.ih-persona').forEach(b =>
          b.classList.toggle('ih-persona-active', b.dataset.persona === this._activePersona));
        this._renderActive();
      });
    });
    const refresh = this.root.querySelector('#ih-refresh');
    if (refresh) refresh.addEventListener('click', () => {
      this._data = null;
      this.loadAndRender();
    });
  }

  async loadAndRender() {
    const body = this.root.querySelector('#ih-body');
    body.innerHTML = '<div class="ih-loading">Loading insights…</div>';
    try {
      if (!this._data) {
        const [dashboard, applications, health, topology, services] = await Promise.all([
          fetch('/api/modernization/dashboard').then(r => r.json()),
          fetch('/api/modernization/applications').then(r => r.json()),
          fetch('/api/modernization/dependency-health').then(r => r.json()),
          fetch('/api/modernization/topology').then(r => r.json()),
          fetch('/api/graph/rekt/services').then(r => r.json()).catch(() => ({ nodes: [], edges: [] })),
        ]);
        this._data = { dashboard, applications, health, topology, services };
      }
      this._renderActive();
    } catch (err) {
      body.innerHTML = `<div class="ih-error">Failed to load: ${this._escape(err.message)}</div>`;
    }
  }

  _renderActive() {
    const body = this.root.querySelector('#ih-body');
    if (!this._data) {
      body.innerHTML = '<div class="ih-loading">Loading…</div>';
      this.loadAndRender();
      return;
    }
    if (this._activePersona === 'business')   body.innerHTML = this._renderBusinessOwner();
    else if (this._activePersona === 'architect') body.innerHTML = this._renderArchitect();
    else if (this._activePersona === 'lead')      body.innerHTML = this._renderLead();
    else if (this._activePersona === 'developer') body.innerHTML = this._renderDeveloper();
  }

  // ───────────────────────────────────────────────────────────────────────
  // 💼 Business Owner — outcomes-centric. Big numbers, risk, blockers.
  // ───────────────────────────────────────────────────────────────────────
  _renderBusinessOwner() {
    const { health, applications, dashboard } = this._data;
    const total = applications.length;
    const ready = applications.filter(a => a.modernizationStatus === 'verified').length;
    const inProgress = applications.filter(a => a.modernizationStatus === 'converted' || a.modernizationStatus === 'partial-fallback').length;
    const blocked = applications.filter(a => a.modernizationStatus === 'compile-failing').length;
    const notStarted = total - ready - inProgress - blocked;
    const progressPct = total > 0 ? Math.round(((ready + inProgress * 0.5) / total) * 100) : 0;
    const readinessColor = health.readinessScore >= 75 ? '#10b981' : health.readinessScore >= 40 ? '#f59e0b' : '#ef4444';

    const top5Blockers = (health.missingCopybooks || [])
      .sort((a, b) => (b.referencedBy?.length || 0) - (a.referencedBy?.length || 0))
      .slice(0, 5);

    // Risk heatmap: high-LoC + blocked + many missing copybooks
    const risky = (applications || [])
      .map(a => ({
        ...a,
        riskScore: (a.modernizationStatus === 'compile-failing' ? 50 : 0)
                 + (a.linesOfCode > 1500 ? 30 : a.linesOfCode > 500 ? 15 : 0)
                 + (a.factsWarnings || 0) * 5
                 + (a.factsConfidence < 3 ? 20 : 0),
      }))
      .filter(a => a.riskScore > 0)
      .sort((a, b) => b.riskScore - a.riskScore)
      .slice(0, 10);

    return `
      <div class="ih-section-title">💼 Business Owner — Modernization Program Visibility</div>
      <div class="ih-kpi-row">
        ${this._bigKpi('Estate readiness', `${health.readinessScore}%`,
                       `${health.fullFidelityCount}/${total} programs full-fidelity`, readinessColor)}
        ${this._bigKpi('Modernization progress', `${progressPct}%`,
                       `${ready} verified · ${inProgress} in-progress · ${notStarted} not-started`, '#3b82f6')}
        ${this._bigKpi('Active blockers', health.programsBlockedByMissing,
                       `programs blocked by ${health.totalMissingCopybooks} missing copybooks`, '#ef4444')}
        ${this._bigKpi('Conversion success rate', `${dashboard.recentCompileSuccessPct || 0}%`,
                       `last ${(dashboard.recentQuality || []).length} quality gates`, '#8b5cf6')}
      </div>

      <div class="ih-grid">
        <div class="ih-card">
          <h3>Top investment-unlock blockers</h3>
          <div class="ih-muted" style="font-size:11px; margin-bottom:8px;">
            Resolving these missing copybooks would jump-start the most programs.
            Each is a single asset acquisition that unlocks tens of programs.
          </div>
          <table class="mi-table">
            <thead><tr><th>Asset to acquire</th><th>Programs unlocked</th></tr></thead>
            <tbody>
              ${top5Blockers.length === 0 ? '<tr><td colspan="2" class="mi-muted">No blockers — estate fully resolved.</td></tr>'
                : top5Blockers.map(m => `<tr>
                    <td><code><b>${this._escape(m.copybook)}.cpy</b></code></td>
                    <td><span class="ih-pill ih-pill-red">${m.referencedBy.length}</span></td>
                  </tr>`).join('')}
            </tbody>
          </table>
        </div>

        <div class="ih-card">
          <h3>Modernization risk heatmap</h3>
          <div class="ih-muted" style="font-size:11px; margin-bottom:8px;">
            High-LoC programs with compile failures, low facts confidence, or many warnings.
          </div>
          <table class="mi-table mi-table-dense">
            <thead><tr><th>Program</th><th>LoC</th><th>Risk</th></tr></thead>
            <tbody>
              ${risky.length === 0 ? '<tr><td colspan="3" class="mi-muted">No high-risk programs identified.</td></tr>'
                : risky.map(a => {
                  const bandClass = a.riskScore > 80 ? 'ih-pill-red' : a.riskScore > 40 ? 'ih-pill-orange' : 'ih-pill-yellow';
                  return `<tr>
                    <td><b>${this._escape(a.basename)}</b></td>
                    <td class="num">${a.linesOfCode.toLocaleString()}</td>
                    <td><span class="ih-pill ${bandClass}">${a.riskScore}</span></td>
                  </tr>`;
                }).join('')}
            </tbody>
          </table>
        </div>

        <div class="ih-card">
          <h3>Programs by status</h3>
          ${this._renderStatusBar(applications)}
          <table class="mi-table mi-table-dense" style="margin-top:8px;">
            <tbody>
              <tr><td>✅ Verified</td><td class="num">${ready}</td></tr>
              <tr><td>🔄 In progress</td><td class="num">${inProgress}</td></tr>
              <tr><td>❌ Compile-failing</td><td class="num">${blocked}</td></tr>
              <tr><td>⏸ Not started</td><td class="num">${notStarted}</td></tr>
            </tbody>
          </table>
        </div>

        <div class="ih-card">
          <h3>Cost & efficiency signals</h3>
          <table class="mi-table mi-table-dense">
            <tbody>
              <tr><td>Avg context-token reduction</td><td class="num"><b>${dashboard.avgContextReductionPct || 0}%</b></td></tr>
              <tr><td>Projection-block cache hit rate</td><td class="num"><b>${dashboard.cacheHitRatePct || 0}%</b></td></tr>
              <tr><td>LLM call success rate</td><td class="num"><b>${dashboard.llmSuccessRatePct || 0}%</b></td></tr>
              <tr><td>Total telemetry events ingested</td><td class="num">${dashboard.totalEvents || 0}</td></tr>
            </tbody>
          </table>
          <div class="ih-muted" style="font-size:11px; margin-top:8px;">
            High cache hit rate + high context reduction = lower per-conversion LLM cost.
          </div>
        </div>
      </div>

      <div class="ih-cta">
        <h4>Recommended next investment</h4>
        ${top5Blockers.length > 0 ? `
          <p>Acquire the top ${Math.min(5, top5Blockers.length)} missing copybook${top5Blockers.length > 1 ? 's' : ''}
             (<code>${top5Blockers.map(m => m.copybook).join('.cpy</code>, <code>')}.cpy</code>) from the source-of-truth team.
             This single action would unlock ${top5Blockers.reduce((a, m) => a + m.referencedBy.length, 0)} program-copybook references
             and dramatically lift estate readiness above ${Math.min(95, health.readinessScore + 40)}%.</p>
        ` : `<p>Estate is fully resolved — proceed to migration wave planning.</p>`}
      </div>
    `;
  }

  // ───────────────────────────────────────────────────────────────────────
  // 🏗 Enterprise Architect — structure & coupling
  // ───────────────────────────────────────────────────────────────────────
  _renderArchitect() {
    const { applications, services, topology, health } = this._data;
    const edges = services.edges || [];
    const nodes = services.nodes || [];

    // Per-program coupling: count incoming + outgoing edges
    const couplingMap = {};
    for (const e of edges) {
      couplingMap[e.source] = couplingMap[e.source] || { upstream: 0, downstream: 0, name: e.source };
      couplingMap[e.target] = couplingMap[e.target] || { upstream: 0, downstream: 0, name: e.target };
      couplingMap[e.source].downstream++;
      couplingMap[e.target].upstream++;
    }
    const coupledPrograms = Object.values(couplingMap)
      .map(p => ({ ...p, total: p.upstream + p.downstream }))
      .sort((a, b) => b.total - a.total)
      .slice(0, 15);

    // Naive domain clustering: group programs by their first 4-letter prefix
    const domainMap = {};
    for (const a of applications) {
      const prefix = a.basename.replace(/\.(cbl|cob)$/i, '').substring(0, 4).toUpperCase();
      if (!domainMap[prefix]) domainMap[prefix] = { prefix, programs: [], totalLoc: 0 };
      domainMap[prefix].programs.push(a.basename);
      domainMap[prefix].totalLoc += a.linesOfCode || 0;
    }
    const domains = Object.values(domainMap).sort((a, b) => b.totalLoc - a.totalLoc);

    // Hub programs (high downstream) = potential service candidates
    const hubs = coupledPrograms.filter(p => p.downstream >= 3).slice(0, 8);

    // Single-points-of-failure: high upstream (many things depend on them)
    const spofs = coupledPrograms.filter(p => p.upstream >= 3).slice(0, 8);

    return `
      <div class="ih-section-title">🏗 Enterprise Architect — Architecture Understanding</div>
      <div class="ih-kpi-row">
        ${this._bigKpi('Estate size', `${applications.length}`, `programs · ${nodes.length} nodes in graph`, '#3b82f6')}
        ${this._bigKpi('CALL edges', edges.length, `dependency relationships`, '#8b5cf6')}
        ${this._bigKpi('Domain clusters', domains.length, `naive grouping by 4-letter prefix`, '#10b981')}
        ${this._bigKpi('Service candidates', hubs.length, `programs with downstream ≥ 3`, '#f59e0b')}
      </div>

      <div class="ih-grid">
        <div class="ih-card">
          <h3>Domain clusters (by name prefix)</h3>
          <div class="ih-muted" style="font-size:11px; margin-bottom:8px;">
            Naive heuristic: programs sharing a 4-letter prefix likely belong to the same business domain.
            More sophisticated bounded-context inference comes with the Service Candidate Explorer.
          </div>
          <table class="mi-table mi-table-dense">
            <thead><tr><th>Domain prefix</th><th>Programs</th><th>Total LoC</th><th>Examples</th></tr></thead>
            <tbody>
              ${domains.map(d => `<tr>
                <td><b>${this._escape(d.prefix)}*</b></td>
                <td class="num">${d.programs.length}</td>
                <td class="num">${d.totalLoc.toLocaleString()}</td>
                <td class="mi-muted">${this._escape(d.programs.slice(0, 3).join(', '))}${d.programs.length > 3 ? ` +${d.programs.length - 3}` : ''}</td>
              </tr>`).join('')}
            </tbody>
          </table>
        </div>

        <div class="ih-card">
          <h3>Coupling hotspots</h3>
          <div class="ih-muted" style="font-size:11px; margin-bottom:8px;">
            Programs with most incoming + outgoing CALL relationships.
            High coupling = high blast radius for any change.
          </div>
          <table class="mi-table mi-table-dense">
            <thead><tr><th>Program</th><th>↑ Callers</th><th>↓ Callees</th><th>Total</th></tr></thead>
            <tbody>
              ${coupledPrograms.map(p => `<tr>
                <td><code>${this._escape(p.name)}</code></td>
                <td class="num">${p.upstream}</td>
                <td class="num">${p.downstream}</td>
                <td class="num"><b>${p.total}</b></td>
              </tr>`).join('')}
            </tbody>
          </table>
        </div>

        <div class="ih-card">
          <h3>🎯 Service candidates (high-downstream hubs)</h3>
          <div class="ih-muted" style="font-size:11px; margin-bottom:8px;">
            Programs that orchestrate ≥ 3 other programs. Strong candidates to become bounded-context services in the target architecture.
          </div>
          <table class="mi-table mi-table-dense">
            <thead><tr><th>Hub program</th><th>Orchestrates</th></tr></thead>
            <tbody>
              ${hubs.length === 0 ? '<tr><td colspan="2" class="mi-muted">No clear hubs in current corpus.</td></tr>'
                : hubs.map(p => `<tr>
                    <td><code>${this._escape(p.name)}</code></td>
                    <td class="num">${p.downstream} programs</td>
                  </tr>`).join('')}
            </tbody>
          </table>
        </div>

        <div class="ih-card">
          <h3>⚠ Single points of failure</h3>
          <div class="ih-muted" style="font-size:11px; margin-bottom:8px;">
            Programs that ≥ 3 others depend on. Migrating these requires careful sequencing
            — they cannot move until all callers are reconciled.
          </div>
          <table class="mi-table mi-table-dense">
            <thead><tr><th>SPOF program</th><th>Callers</th></tr></thead>
            <tbody>
              ${spofs.length === 0 ? '<tr><td colspan="2" class="mi-muted">No SPOFs in current corpus.</td></tr>'
                : spofs.map(p => `<tr>
                    <td><code>${this._escape(p.name)}</code></td>
                    <td class="num">${p.upstream} programs</td>
                  </tr>`).join('')}
            </tbody>
          </table>
        </div>
      </div>

      <div class="ih-cta">
        <h4>Recommended architecture next step</h4>
        <p>Open <b>🕸 Dependency Topology</b> in Modernization Intelligence to drill into per-program transitive impact.
        Service Candidate Explorer (PR-future) will formalize the bounded-context inference using projection + topology + copybook coupling.</p>
      </div>
    `;
  }

  // ───────────────────────────────────────────────────────────────────────
  // 🚀 Modernization Lead — execution planning
  // ───────────────────────────────────────────────────────────────────────
  _renderLead() {
    const { applications, health, services } = this._data;
    const edges = services.edges || [];

    // Topological migration order: leaves first (no callees within the estate)
    const inEstate = new Set(applications.map(a => a.basename));
    const callees = {};
    for (const e of edges) {
      if (!callees[e.source]) callees[e.source] = new Set();
      if (inEstate.has(e.target)) callees[e.source].add(e.target);
    }

    const wave1 = []; // ready + no in-estate callees
    const wave2 = []; // ready + ≤2 in-estate callees
    const wave3 = []; // everything else ready or convertable
    const queued = []; // blocked or low-readiness

    for (const a of applications) {
      const calls = (callees[a.basename] || new Set()).size;
      const isFullFidelity = (health.programs || []).find(p => p.basename === a.basename)?.parseFidelity === 'full';
      if (!isFullFidelity) {
        queued.push({ ...a, calls, reason: 'needs facts (missing copybooks)' });
        continue;
      }
      if (calls === 0) wave1.push({ ...a, calls });
      else if (calls <= 2) wave2.push({ ...a, calls });
      else wave3.push({ ...a, calls });
    }
    [wave1, wave2, wave3].forEach(w => w.sort((a, b) => a.linesOfCode - b.linesOfCode));

    const renderWave = (wave, label, color) => `
      <div class="ih-card">
        <h3 style="color:${color};">${label} — ${wave.length} program${wave.length !== 1 ? 's' : ''}</h3>
        <table class="mi-table mi-table-dense">
          <thead><tr><th>Program</th><th>LoC</th><th>Callees</th><th>Status</th></tr></thead>
          <tbody>
            ${wave.length === 0 ? '<tr><td colspan="4" class="mi-muted">none</td></tr>'
              : wave.map(w => `<tr>
                  <td><b>${this._escape(w.basename)}</b></td>
                  <td class="num">${w.linesOfCode.toLocaleString()}</td>
                  <td class="num">${w.calls}</td>
                  <td><span class="mi-status mi-status-${w.modernizationStatus}">${this._statusLabel(w.modernizationStatus)}</span></td>
                </tr>`).join('')}
          </tbody>
        </table>
      </div>
    `;

    return `
      <div class="ih-section-title">🚀 Modernization Lead — Execution Planning</div>
      <div class="ih-kpi-row">
        ${this._bigKpi('Wave 1 ready', wave1.length, 'leaf programs (no in-estate dependencies)', '#10b981')}
        ${this._bigKpi('Wave 2 candidates', wave2.length, 'small dependency footprint (≤2)', '#f59e0b')}
        ${this._bigKpi('Wave 3 candidates', wave3.length, 'larger dependency footprint', '#fb923c')}
        ${this._bigKpi('Queued (blocked)', queued.length, 'awaiting missing copybooks', '#ef4444')}
      </div>

      <div class="ih-callout">
        <b>Recommended migration order:</b> Wave 1 → Wave 2 → Wave 3.
        Within a wave, smallest LoC first to build confidence and reduce per-program risk.
        Queued programs become eligible only after their missing copybooks are resolved.
      </div>

      <div class="ih-grid">
        ${renderWave(wave1, 'Wave 1 — Leaves', '#10b981')}
        ${renderWave(wave2, 'Wave 2 — Light coupling', '#f59e0b')}
        ${renderWave(wave3, 'Wave 3 — Heavy coupling', '#fb923c')}
      </div>

      <div class="ih-card mi-card-wide">
        <h3 style="color:#ef4444;">Queued — Blocked by missing copybooks (${queued.length})</h3>
        <table class="mi-table mi-table-dense">
          <thead><tr><th>Program</th><th>LoC</th><th>Blocker</th></tr></thead>
          <tbody>
            ${queued.slice(0, 15).map(q => `<tr>
              <td><b>${this._escape(q.basename)}</b></td>
              <td class="num">${q.linesOfCode.toLocaleString()}</td>
              <td class="mi-muted">${this._escape(q.reason)}</td>
            </tr>`).join('')}
            ${queued.length > 15 ? `<tr><td colspan="3" class="mi-muted">+${queued.length - 15} more — see Dependency Health view</td></tr>` : ''}
          </tbody>
        </table>
      </div>

      <div class="ih-cta">
        <h4>Recommended execution next step</h4>
        <p>Start Wave 1 conversions (${wave1.length} programs, ${wave1.reduce((a, w) => a + w.linesOfCode, 0).toLocaleString()} total LoC). Use
        <code>./doctor.sh convert-only --program X --target java</code> for each, monitor via the Runtime &amp; Conversion Intelligence view.
        Once 75% of Wave 1 passes compile validation, move to Wave 2.</p>
      </div>
    `;
  }

  // ───────────────────────────────────────────────────────────────────────
  // 👨‍💻 Developer — debugging & understanding
  // ───────────────────────────────────────────────────────────────────────
  _renderDeveloper() {
    const { dashboard, health } = this._data;
    const recentQuality = (dashboard.recentQuality || []).slice(0, 5);
    const reduction = (dashboard.contextReduction || []);
    const cacheTotal = Object.values(dashboard.cacheDecisionCounts || {}).reduce((a, b) => a + b, 0);
    const llmTotal = (dashboard.llmCallOutcomes || []).reduce((a, o) => a + o.count, 0);

    const flowEligible = (health.programs || []).filter(p => p.parseFidelity === 'full');

    return `
      <div class="ih-section-title">👨‍💻 Developer — Application Understanding &amp; Debugging</div>
      <div class="ih-kpi-row">
        ${this._bigKpi('LLM calls', llmTotal, `avg ${Math.round((dashboard.llmCallOutcomes?.find(o => o.outcome === 'success')?.avgDurationMs || 0))}ms / call`, '#8b5cf6')}
        ${this._bigKpi('Cache decisions', cacheTotal, `${dashboard.cacheHitRatePct || 0}% hit rate`, '#10b981')}
        ${this._bigKpi('Avg ctx reduction', `${dashboard.avgContextReductionPct || 0}%`, `projection vs raw REKT context`, '#3b82f6')}
        ${this._bigKpi('Flow-eligible', flowEligible.length, `programs with full REKT AST`, '#f59e0b')}
      </div>

      <div class="ih-grid">
        <div class="ih-card">
          <h3>What the AI sees — per program</h3>
          <div class="ih-muted" style="font-size:11px; margin-bottom:8px;">
            For each program with a measured projection, raw-REKT token count vs facts-projection token count and the reduction the AI got.
          </div>
          <table class="mi-table mi-table-dense">
            <thead><tr><th>Program</th><th>Raw REKT</th><th>Projection</th><th>Δ</th></tr></thead>
            <tbody>
              ${reduction.length === 0 ? '<tr><td colspan="4" class="mi-muted">no projection_metrics yet</td></tr>'
                : reduction.map(r => `<tr>
                    <td><code>${this._escape(r.file)}</code></td>
                    <td class="num">${Math.round(r.rawTokens)}</td>
                    <td class="num">${Math.round(r.projectionTokens)}</td>
                    <td class="num"><b>${r.reductionPct.toFixed(1)}%</b></td>
                  </tr>`).join('')}
            </tbody>
          </table>
        </div>

        <div class="ih-card">
          <h3>Recent compile gates</h3>
          <div class="ih-muted" style="font-size:11px; margin-bottom:8px;">
            Last 5 quality_metrics events from check-compile.sh. Click into Runtime &amp; Conversion Intelligence to drill into per-run timelines.
          </div>
          <table class="mi-table mi-table-dense">
            <thead><tr><th>Run</th><th>Result</th><th>Errors</th><th>Files</th><th>@Inject</th></tr></thead>
            <tbody>
              ${recentQuality.length === 0 ? '<tr><td colspan="5" class="mi-muted">no quality gates yet</td></tr>'
                : recentQuality.map(q => `<tr>
                    <td><code>#${this._escape(q.runId)}</code></td>
                    <td>${q.compileSuccess ? '<span class="mi-ok">✅ pass</span>' : '<span class="mi-bad">❌ fail</span>'}</td>
                    <td class="num">${q.compileErrors}</td>
                    <td class="num">${q.generatedClasses}</td>
                    <td class="num">${q.injectAnnotations}</td>
                  </tr>`).join('')}
            </tbody>
          </table>
        </div>

        <div class="ih-card">
          <h3>Cache behavior</h3>
          <table class="mi-table mi-table-dense">
            <thead><tr><th>Decision</th><th>Count</th></tr></thead>
            <tbody>
              ${Object.entries(dashboard.cacheDecisionCounts || {}).map(([d, n]) => `<tr>
                <td>${d === 'hit' ? '🎯 hit' : d === 'miss-store' ? '💾 miss-store' : d}</td>
                <td class="num">${n}</td>
              </tr>`).join('') || '<tr><td colspan="2" class="mi-muted">no cache_event yet</td></tr>'}
            </tbody>
          </table>
        </div>

        <div class="ih-card">
          <h3>Where to dig deeper</h3>
          <div class="ih-muted" style="font-size:11px; margin-bottom:12px;">
            All these views are alive in the portal — this is a quick navigation hub for the developer toolchain.
          </div>
          <ul class="ih-quicklinks">
            <li><a href="#" onclick="switchDashboard('galaxy'); return false;"><b>🌌 AST Galaxy Explorer</b> — interactive REKT graph</a></li>
            <li><a href="#" onclick="switchDashboard('ast'); return false;"><b>🔬 AST Explorer</b> — per-program AST drill-down</a></li>
            <li><a href="#" onclick="switchDashboard('mermaid'); return false;"><b>📐 Mermaid Diagrams</b> — CFG visualisation</a></li>
            <li><a href="#" onclick="switchDashboard('modernization'); return false;"><b>🧭 Modernization Intelligence</b> — full data views</a></li>
          </ul>
          <div class="ih-muted" style="font-size:11px; margin-top:12px;">
            Inside Modernization Intelligence: <b>🌊 Semantic Flow</b> for per-program flow; <b>⏱ Runtime &amp; Conversion Intelligence</b> for per-run timelines.
          </div>
        </div>
      </div>

      <div class="ih-cta">
        <h4>Recommended developer next step</h4>
        <p>If a recent conversion failed compile, open <b>⏱ Runtime &amp; Conversion Intelligence</b> in Modernization Intelligence, select the failing run, and inspect the event timeline + projection metrics to identify whether the issue was projection drift, chunked reassembly, or model output drift.</p>
      </div>
    `;
  }

  // ───────────────────────────────────────────────────────────────────────
  // Shared widget helpers
  // ───────────────────────────────────────────────────────────────────────
  _bigKpi(label, value, sub, color) {
    return `
      <div class="ih-kpi" style="border-left-color:${color || '#475569'};">
        <div class="ih-kpi-value">${value}</div>
        <div class="ih-kpi-label">${label}</div>
        <div class="ih-kpi-sub">${sub || ''}</div>
      </div>`;
  }

  _renderStatusBar(apps) {
    const total = apps.length;
    if (total === 0) return '<div class="mi-muted">no programs</div>';
    const counts = { 'verified': 0, 'converted': 0, 'partial-fallback': 0, 'compile-failing': 0, 'not-started': 0 };
    for (const a of apps) counts[a.modernizationStatus] = (counts[a.modernizationStatus] || 0) + 1;
    const colors = {
      'verified': '#10b981', 'converted': '#3b82f6', 'partial-fallback': '#f59e0b',
      'compile-failing': '#ef4444', 'not-started': '#475569'
    };
    const segs = Object.entries(counts).filter(([_, n]) => n > 0).map(([k, n]) => {
      const pct = (n / total * 100).toFixed(1);
      return `<div class="ih-bar-seg" style="width:${pct}%; background:${colors[k]};" title="${k}: ${n} (${pct}%)"></div>`;
    }).join('');
    return `<div class="ih-bar">${segs}</div>`;
  }

  _statusLabel(s) {
    return ({
      'not-started': 'not started',
      'converted': 'converted (no gate)',
      'verified': '✅ verified',
      'partial-fallback': '⚠ partial (fallback)',
      'compile-failing': '❌ compile failing',
      'not-in-source': 'not in source',
    })[s] || s;
  }

  _escape(s) {
    if (s == null) return '';
    return String(s).replace(/[&<>"']/g, c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
  }
}

window.InsightsHub = InsightsHub;
