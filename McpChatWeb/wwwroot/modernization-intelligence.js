// Modernization Intelligence — the deterministic decision surface.
//
// Every number here traces to a measurement: the REKT scan cache, facts.json,
// or the presence of an artifact on disk. Where a value is inferred rather than
// measured, the fidelity source is shown beside it so the two are never
// confused. Subviews whose backends are not part of this build render an
// explicit notice rather than an empty table that reads as "nothing found".

const MI_LIVE_SUBVIEWS = ['health', 'topology', 'flow', 'chain', 'runtime'];

const MI_PENDING_SUBVIEWS = {
  dashboard: 'Modernization Dashboard',
  applications: 'Application Explorer',
  services: 'Service Candidates',
  waves: 'Migration Wave Planner',
  capabilities: 'Capabilities & Locator',
};

const MI_FIDELITY = {
  full: { label: 'Full', color: '#10b981' },
  partial: { label: 'Partial', color: '#f59e0b' },
  'deps-only': { label: 'Deps only', color: '#38bdf8' },
  failed: { label: 'Failed', color: '#ef4444' },
  'not-parsed': { label: 'Not parsed', color: '#64748b' },
};

const MI_FIDELITY_SOURCE = {
  'scan-cache': 'Measured by the REKT scan cache',
  facts: 'Read from facts.json confidence',
  artifacts: 'Inferred from artifacts present on disk',
  none: 'No evidence — program has not been scanned',
};

function miEscape(s) {
  return String(s ?? '').replace(/[&<>"']/g, c =>
    ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
}

function miFidelityBadge(fidelity) {
  const f = MI_FIDELITY[fidelity] || MI_FIDELITY['not-parsed'];
  return `<span class="mi-badge" style="color:${f.color};border-color:${f.color}44;background:${f.color}18;">${f.label}</span>`;
}

function miSourceBadge(source) {
  const title = MI_FIDELITY_SOURCE[source] || source;
  const inferred = source === 'artifacts' || source === 'none';
  return `<span class="mi-src ${inferred ? 'mi-src-weak' : ''}" title="${miEscape(title)}">${miEscape(source)}</span>`;
}

function miStat(label, value, hint, color) {
  return `<div class="mi-stat">
    <div class="mi-stat-value" style="${color ? `color:${color};` : ''}">${miEscape(value)}</div>
    <div class="mi-stat-label">${miEscape(label)}</div>
    ${hint ? `<div class="mi-stat-hint">${miEscape(hint)}</div>` : ''}
  </div>`;
}

function miNotice(text, kind = 'info') {
  return `<div class="mi-notice mi-notice-${kind}">${miEscape(text)}</div>`;
}

// Procedural detail is unbounded in a large program; render a readable prefix and say so.
const MI_FLOW_LIST_CAP = 200;

function miFlowOverflow(total) {
  return total > MI_FLOW_LIST_CAP
    ? `<div class="mi-dim">Showing the first ${MI_FLOW_LIST_CAP} of ${total}.</div>`
    : '';
}

function miPendingPanel(title, { inline = false } = {}) {
  return `<div class="mi-pending${inline ? ' mi-pending-inline' : ''}">
    <div class="mi-pending-icon">🚧</div>
    <div class="mi-pending-title">${miEscape(title)}</div>
    <div class="mi-pending-body">Not wired in this build — ships with a later feature.</div>
  </div>`;
}

class ModernizationIntelligenceView {
  constructor(rootId) {
    this.root = document.getElementById(rootId);
    if (!this.root) return;
    this._activeSubview = 'health';
    this._flowIdentity = null;
    this._chainJob = '';
    this._chainProgram = '';
    this._renderToken = 0;
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
              <div class="mi-title-sub">Deterministic decision surface · REKT scan cache, facts.json, JCL</div>
            </div>
          </div>
          <div class="mi-actions">
            <button id="mi-refresh" class="mi-btn" title="Re-fetch from /api/modernization/*">⟳ Refresh</button>
          </div>
        </div>
        <div class="mi-subnav">
          <button class="mi-subtab" data-sub="dashboard">📊 Modernization Dashboard</button>
          <button class="mi-subtab" data-sub="applications">📚 Application Explorer</button>
          <button class="mi-subtab mi-subtab-active" data-sub="health">⚕️ Dependency Health</button>
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
      btn.addEventListener('click', () => {
        this._activeSubview = btn.dataset.sub;
        this.root.querySelectorAll('.mi-subtab').forEach(b =>
          b.classList.toggle('mi-subtab-active', b.dataset.sub === this._activeSubview));
        this._renderActive();
      });
    });
    this.root.querySelector('#mi-refresh')
      ?.addEventListener('click', () => this.loadAndRender());
  }

  async loadAndRender() {
    await this._renderActive();
  }

  async _renderActive() {
    const body = this.root.querySelector('#mi-body');
    if (!body) return;

    if (!MI_LIVE_SUBVIEWS.includes(this._activeSubview)) {
      body.innerHTML = miPendingPanel(MI_PENDING_SUBVIEWS[this._activeSubview] || 'Subview');
      return;
    }

    body.innerHTML = '<div class="mi-loading">Loading…</div>';
    const token = ++this._renderToken;
    try {
      if (this._activeSubview === 'health') {
        const data = await this._get('/api/modernization/dependency-health');
        if (token !== this._renderToken) return;
        body.innerHTML = this._renderDependencyHealth(data);
      } else if (this._activeSubview === 'topology') {
        const data = await this._get('/api/modernization/topology');
        if (token !== this._renderToken) return;
        body.innerHTML = this._renderTopology(data);
        this._drawTopology(data);
      } else if (this._activeSubview === 'flow') {
        const health = await this._get('/api/modernization/dependency-health');
        if (token !== this._renderToken) return;
        body.innerHTML = this._renderFlowShell(health);
        this._wireFlow(body, health);
      } else if (this._activeSubview === 'chain') {
        await this._renderChain(body, token);
      } else if (this._activeSubview === 'runtime') {
        const data = await this._get('/api/modernization/conversion-parity');
        if (token !== this._renderToken) return;
        body.innerHTML = this._renderRuntime(data);
      }
    } catch (e) {
      if (token !== this._renderToken) return;
      console.error('Modernization Intelligence load error:', e);
      body.innerHTML = miNotice(`Could not load this view: ${e.message}`, 'error');
    }
  }

  async _get(url) {
    const resp = await fetch(url);
    if (!resp.ok) throw new Error(`${resp.status} ${resp.statusText}`);
    return resp.json();
  }

  // ── Dependency Health ───────────────────────────────────────────────
  _renderDependencyHealth(d) {
    const total = d.totalPrograms || 0;
    const note = d.note ? miNotice(d.note, 'warn') : '';

    const stats = `<div class="mi-stats">
      ${miStat('Programs', total, 'Copybooks excluded')}
      ${miStat('Full fidelity', d.fullFidelityCount, 'Parsed with full semantics', MI_FIDELITY.full.color)}
      ${miStat('Partial', d.partialFidelityCount, 'Raw AST, no dialect, or stub-backed', MI_FIDELITY.partial.color)}
      ${miStat('Deps only', d.depsOnlyCount, 'Edges known, logic unknown', MI_FIDELITY['deps-only'].color)}
      ${miStat('Failed', d.failedCount, 'Parser could not complete', MI_FIDELITY.failed.color)}
      ${miStat('Not parsed', d.notParsedCount, 'No scan evidence', MI_FIDELITY['not-parsed'].color)}
    </div>
    <div class="mi-stats">
      ${miStat('Readiness', `${(d.readinessScore ?? 0).toFixed(1)}%`, 'full=1 · partial=0.5 · deps-only=0.25')}
      ${miStat('Coverage', `${(d.coveragePct ?? 0).toFixed(1)}%`, 'Full fidelity share')}
      ${miStat('Scan-cache backed', d.scanCacheBackedCount, 'Measured, not inferred')}
      ${miStat('Missing copybooks', d.totalMissingCopybooks, 'Distinct names unresolved', d.totalMissingCopybooks ? MI_FIDELITY.failed.color : undefined)}
      ${miStat('Blocked programs', d.programsBlockedByMissing, 'Reference a missing copybook', d.programsBlockedByMissing ? MI_FIDELITY.partial.color : undefined)}
    </div>`;

    const missing = (d.missingCopybooks || []).length
      ? `<h4 class="mi-h4">Missing copybooks — blocking parse fidelity</h4>
         <table class="mi-table">
           <thead><tr><th>Copybook</th><th>Referenced by</th></tr></thead>
           <tbody>${d.missingCopybooks.map(m => `<tr>
             <td class="mi-mono">${miEscape(m.copybook)}</td>
             <td class="mi-dim">${miEscape((m.referencedBy || []).join(', ')) || '—'}</td>
           </tr>`).join('')}</tbody>
         </table>`
      : '';

    const rows = (d.programs || []).map(p => `<tr>
      <td class="mi-mono">${miEscape(p.basename)}${p.ambiguousBasename
        ? ' <span class="mi-warn-chip" title="This basename maps to more than one file, so no scan-cache row can be attributed to it">ambiguous</span>' : ''}</td>
      <td class="mi-dim mi-mono">${miEscape(p.relativePath)}</td>
      <td class="mi-num">${p.linesOfCode || 0}</td>
      <td>${miFidelityBadge(p.parseFidelity)}</td>
      <td>${miSourceBadge(p.fidelitySource)}</td>
      <td class="mi-dim">${miEscape(p.scanOutcome || '—')}</td>
      <td class="mi-num">${p.factsWarnings || 0}</td>
      <td class="mi-num">${p.missingCopybookCount || 0}</td>
    </tr>`).join('');

    return `${note}${stats}${missing}
      <h4 class="mi-h4">Programs</h4>
      <table class="mi-table">
        <thead><tr>
          <th>Program</th><th>Path</th><th>LoC</th><th>Fidelity</th>
          <th>Evidence</th><th>Scan outcome</th><th>Warnings</th><th>Missing cpy</th>
        </tr></thead>
        <tbody>${rows || '<tr><td colspan="8" class="mi-dim">No programs found.</td></tr>'}</tbody>
      </table>`;
  }

  // ── Dependency Topology ─────────────────────────────────────────────
  _renderTopology(d) {
    const note = d.note ? miNotice(d.note, 'warn') : '';
    const nodes = d.nodes || [];
    const edges = d.edges || [];
    const unresolved = d.unresolvedEdges || [];

    const unresolvedPanel = unresolved.length
      ? `<h4 class="mi-h4">Unresolved edges — ${unresolved.length}</h4>
         ${miNotice('A CALL or COPY target that matches no file, or matches several, cannot be attributed. These are findings, not noise: each one is a program whose true dependencies are unknown.', 'info')}
         <table class="mi-table">
           <thead><tr><th>From</th><th>Target</th><th>Kind</th></tr></thead>
           <tbody>${unresolved.slice(0, 200).map(e => `<tr>
             <td class="mi-mono">${miEscape(e.source)}</td>
             <td class="mi-mono">${miEscape(e.target)}</td>
             <td class="mi-dim">${miEscape(e.kind)}</td>
           </tr>`).join('')}</tbody>
         </table>`
      : '';

    return `${note}
      <div class="mi-stats">
        ${miStat('Nodes', nodes.length, 'Programs and copybooks')}
        ${miStat('Edges', edges.length, 'CALL and COPY, resolved')}
        ${miStat('Unresolved', unresolved.length, 'Target not attributable', unresolved.length ? MI_FIDELITY.partial.color : undefined)}
      </div>
      <div id="mi-topology-graph" class="mi-graph"></div>
      ${unresolvedPanel}`;
  }

  _drawTopology(d) {
    const host = document.getElementById('mi-topology-graph');
    if (!host || typeof vis === 'undefined') return;
    const nodes = (d.nodes || []).map(n => {
      const f = MI_FIDELITY[n.parseFidelity] || MI_FIDELITY['not-parsed'];
      return {
        id: n.id,
        label: n.basename,
        shape: n.kind === 'copybook' ? 'box' : 'dot',
        size: Math.min(30, 8 + Math.sqrt(n.linesOfCode || 0)),
        color: { background: `${f.color}33`, border: f.color },
        font: { color: '#e2e8f0', size: 11 },
        title: `${n.basename}\n${n.parseFidelity} (${n.fidelitySource})\n${n.linesOfCode || 0} LoC`,
      };
    });
    const edges = (d.edges || []).map(e => ({
      from: e.source,
      to: e.target,
      arrows: 'to',
      dashes: e.kind === 'copy',
      color: { color: e.kind === 'copy' ? '#10b98166' : '#60a5fa66' },
    }));
    this._topologyNetwork?.destroy();
    this._topologyNetwork = new vis.Network(host, { nodes, edges }, {
      physics: {
        solver: 'forceAtlas2Based',
        forceAtlas2Based: { gravitationalConstant: -120, springLength: 180, avoidOverlap: 0.5 },
        stabilization: { iterations: 200 },
      },
      interaction: { hover: true, tooltipDelay: 120 },
    });
  }

  // ── Semantic Flow Explorer ──────────────────────────────────────────
  _renderFlowShell(health) {
    const programs = (health.programs || [])
      .filter(p => p.parseFidelity !== 'not-parsed');

    const rows = programs.map(p => `<tr class="mi-flow-row" data-identity="${miEscape(p.relativePath)}">
      <td class="mi-mono">${miEscape(p.basename)}</td>
      <td>${miFidelityBadge(p.parseFidelity)}</td>
      <td class="mi-num">${p.linesOfCode || 0}</td>
    </tr>`).join('');

    return `<div class="mi-flow">
      <div class="mi-flow-list">
        <h4 class="mi-h4">Programs with scan evidence</h4>
        ${programs.length ? '' : miNotice('No program has been scanned yet. Run the REKT preprocessing pipeline first.', 'warn')}
        <table class="mi-table mi-table-compact">
          <thead><tr><th>Program</th><th>Fidelity</th><th>LoC</th></tr></thead>
          <tbody>${rows}</tbody>
        </table>
      </div>
      <div class="mi-flow-detail" id="mi-flow-detail">
        <div class="mi-dim">Select a program to inspect its procedural flow artifacts.</div>
      </div>
    </div>`;
  }

  _wireFlow(body, health) {
    body.querySelectorAll('.mi-flow-row').forEach(tr => {
      tr.onclick = () => {
        body.querySelectorAll('.mi-flow-row').forEach(r => r.classList.remove('mi-flow-row-active'));
        tr.classList.add('mi-flow-row-active');
        this._loadProgramFlow(tr.dataset.identity);
      };
    });
    // Lead with a program that actually has something to show.
    const first = (health.programs || []).find(p => p.parseFidelity === 'full')
      || (health.programs || []).find(p => p.parseFidelity !== 'not-parsed');
    if (first) {
      const tr = body.querySelector(`.mi-flow-row[data-identity="${CSS.escape(first.relativePath)}"]`);
      tr?.classList.add('mi-flow-row-active');
      this._loadProgramFlow(first.relativePath);
    }
  }

  async _loadProgramFlow(identity) {
    const host = document.getElementById('mi-flow-detail');
    if (!host) return;
    host.innerHTML = '<div class="mi-loading">Loading…</div>';
    try {
      const parts = String(identity).split('/').map(encodeURIComponent).join('/');
      const f = await this._get(`/api/modernization/flow/${parts}`);
      host.innerHTML = this._renderFlow(f);
      host.querySelectorAll('.mi-candidate').forEach(btn => {
        btn.onclick = () => this._loadProgramFlow(btn.dataset.identity);
      });
    } catch (e) {
      host.innerHTML = miNotice(`Could not load flow: ${e.message}`, 'error');
    }
  }

  _renderFlow(f) {
    const yn = v => v
      ? '<span class="mi-yes">present</span>'
      : '<span class="mi-no">absent</span>';

    const astList = (f.flowAstNames || []).length
      ? `<h4 class="mi-h4">Flow AST files — ${f.flowAstFiles}</h4>
         <ul class="mi-list">${f.flowAstNames.map(n => `<li class="mi-mono">${miEscape(n)}</li>`).join('')}</ul>`
      : '';

    const candidates = (f.candidates || []).length
      ? `<h4 class="mi-h4">Matching source files — pick one</h4>
         <ul class="mi-list">${f.candidates.map(c =>
           `<li><button type="button" class="mi-candidate mi-mono" data-identity="${miEscape(c)}">${miEscape(c)}</button></li>`).join('')}</ul>`
      : '';

    return `
      <div class="mi-flow-head">
        <div class="mi-flow-name mi-mono">${miEscape(f.basename)}</div>
        <div>${miFidelityBadge(f.parseFidelity)} ${miSourceBadge(f.fidelitySource)}</div>
      </div>
      <div class="mi-dim mi-mono mi-flow-path">${miEscape(f.relativePath)}</div>
      ${f.note ? miNotice(f.note, 'warn') : ''}
      ${this._renderFlowStats(f)}
      ${this._renderFlowSections(f)}
      ${this._renderPerformEdges(f)}
      ${this._renderFlowSql(f)}
      ${this._renderFlowCalls(f)}
      <table class="mi-table mi-table-compact">
        <tbody>
          <tr><td>Procedural flow AST</td><td>${yn(f.hasFlowAst)}</td></tr>
          <tr><td>Control flow graph</td><td>${yn(f.hasCfg)}</td></tr>
          <tr><td>Data structures</td><td>${yn(f.hasDataStructures)}</td></tr>
          <tr><td>Report directory</td><td class="mi-mono mi-dim">${miEscape(f.reportDirectory || '—')}</td></tr>
        </tbody>
      </table>
      ${astList}
      ${candidates}`;
  }

  _renderFlowStats(f) {
    const sections = (f.sections || []).length;
    const performs = (f.performEdges || []).length;
    const sql = (f.sqlStatements || []).length;
    const calls = (f.callTargets || []).length;
    if (!f.paragraphCount && !sections && !performs && !sql && !calls) return '';

    return `<div class="mi-stats">
      ${miStat('Paragraphs', f.paragraphCount || 0, 'Named procedure blocks')}
      ${miStat('Sections', sections, 'PROCEDURE DIVISION sections')}
      ${miStat('PERFORM edges', performs, 'Resolved control transfers')}
      ${miStat('SQL statements', sql, 'Embedded EXEC SQL')}
      ${miStat('CALL targets', calls, 'Static and dynamic')}
    </div>`;
  }

  _renderFlowSections(f) {
    const sections = f.sections || [];
    if (!sections.length) return '';

    const rows = sections.map(s => `
      <tr>
        <td class="mi-mono">${miEscape(s.name)}</td>
        <td class="mi-dim">${s.startLine}–${s.endLine}</td>
        <td class="mi-mono mi-dim">${
          (s.paragraphs || []).length
            ? s.paragraphs.map(p => miEscape(p.name)).join(', ')
            : '—'}</td>
      </tr>`).join('');

    return `<h4 class="mi-h4">Sections and paragraphs</h4>
      <table class="mi-table mi-table-compact">
        <thead><tr><th>Section</th><th>Lines</th><th>Paragraphs</th></tr></thead>
        <tbody>${rows}</tbody>
      </table>`;
  }

  _renderPerformEdges(f) {
    const edges = f.performEdges || [];
    if (!edges.length) return '';

    const shown = edges.slice(0, MI_FLOW_LIST_CAP);
    const items = shown.map(e =>
      `<li class="mi-mono">${miEscape(e.from)} → ${miEscape(e.to)}${
        e.conditional ? ' <span class="mi-dim">(conditional)</span>' : ''}</li>`).join('');

    return `<h4 class="mi-h4">PERFORM graph — ${edges.length}</h4>
      <ul class="mi-list">${items}</ul>
      ${miFlowOverflow(edges.length)}`;
  }

  _renderFlowSql(f) {
    const statements = f.sqlStatements || [];
    if (!statements.length) return '';

    const rows = statements.slice(0, MI_FLOW_LIST_CAP).map(s => `
      <tr>
        <td class="mi-mono">${miEscape(s.operation || '—')}</td>
        <td class="mi-mono mi-dim">${(s.tables || []).map(miEscape).join(', ') || '—'}</td>
        <td class="mi-dim">${s.lineNumber || '—'}</td>
        <td class="mi-mono mi-dim">${miEscape(s.excerpt || '')}</td>
      </tr>`).join('');

    return `<h4 class="mi-h4">SQL statements — ${statements.length}</h4>
      <table class="mi-table mi-table-compact">
        <thead><tr><th>Operation</th><th>Tables</th><th>Line</th><th>Excerpt</th></tr></thead>
        <tbody>${rows}</tbody>
      </table>
      ${miFlowOverflow(statements.length)}`;
  }

  _renderFlowCalls(f) {
    const calls = f.callTargets || [];
    if (!calls.length) return '';

    const items = calls.slice(0, MI_FLOW_LIST_CAP).map(c =>
      `<li class="mi-mono">${miEscape(c.targetProgram)}${
        c.isDynamic ? ' <span class="mi-dim">(dynamic)</span>' : ''}${
        c.lineNumber ? ` <span class="mi-dim">line ${c.lineNumber}</span>` : ''}</li>`).join('');

    return `<h4 class="mi-h4">CALL targets — ${calls.length}</h4>
      <ul class="mi-list">${items}</ul>
      ${miFlowOverflow(calls.length)}`;
  }

  // ── Service Chain ───────────────────────────────────────────────────
  // Conversion parity is measured; the runtime half has no data source yet, so it stays an
  // explicit pending panel rather than an empty table that reads as "nothing found".
  _renderRuntime(estate) {
    const reports = estate?.reports ?? [];
    const missing = estate?.missingTargets ?? [];
    const unreadable = estate?.unreadableTargets ?? [];

    const parts = ['<div class="mi-h4">Conversion parity <span class="mi-badge" style="color:#a78bfa;border-color:#a78bfa44;background:#a78bfa18;">preview</span></div>'];
    parts.push(miNotice(
      'Structural coverage of generated code against the COBOL it came from. A passing score means procedures, data fields, CALL targets and SQL tables are visible in the output — it is not a statement about behavioural equivalence.'));

    if (reports.length === 0) {
      parts.push(miNotice(
        'No conversion parity report found. Run a conversion — the report is written to output/<target>/conversion-parity.json.',
        'warn'));
    }

    for (const report of reports) {
      parts.push(this._renderParityReport(report));
    }

    if (unreadable.length > 0) {
      parts.push(miNotice(
        `Could not read the parity report for: ${unreadable.join(', ')}. The file exists but is not valid JSON.`,
        'error'));
    }
    if (missing.length > 0 && reports.length > 0) {
      parts.push(`<div class="mi-dim">No parity report for: ${miEscape(missing.join(', '))}.</div>`);
    }

    parts.push('<div class="mi-h4">Runtime intelligence</div>');
    parts.push(miPendingPanel('Runtime telemetry', { inline: true }));

    return parts.join('');
  }

  _renderParityReport(report) {
    const programs = report.programs ?? [];
    const evaluated = report.evaluatedCount ?? 0;
    const notEvaluated = report.notEvaluatedCount ?? 0;
    const below = report.belowThresholdCount ?? 0;
    const threshold = report.threshold ?? 0;

    const avg = report.averageScore == null ? '—' : report.averageScore.toFixed(2);
    const avgHint = report.averageScore == null
      ? 'Nothing could be evaluated'
      : `Across ${evaluated} evaluated program(s)`;

    const parts = [`<div class="mi-h4">${miEscape(report.targetLanguage || 'Target')} <span class="mi-dim">· ${miEscape(report.sourcePath || '')}</span></div>`];

    parts.push(`<div class="mi-stats">
      ${miStat('Evaluated', evaluated, 'Programs with a measured score')}
      ${miStat('Not evaluated', notEvaluated, 'No score — absence of a gap proves nothing', notEvaluated > 0 ? '#f59e0b' : undefined)}
      ${miStat('Below threshold', below, `MIN_PROGRAM_SCORE=${threshold}`, below > 0 ? '#ef4444' : '#10b981')}
      ${miStat('Average score', avg, avgHint)}
    </div>`);

    const scored = programs
      .filter(p => p.outcome === 'Evaluated')
      .sort((a, b) => (a.score ?? 0) - (b.score ?? 0));

    if (scored.length > 0) {
      parts.push(`<table class="mi-table">
        <thead><tr><th>COBOL Source</th><th>Generated</th><th>Score</th><th>Missing</th><th>Renamed / merged</th><th>Provenance</th></tr></thead>
        <tbody>${scored.map(p => this._renderParityRow(p, threshold)).join('')}</tbody>
      </table>`);
    }

    const skipped = programs.filter(p => p.outcome !== 'Evaluated');
    if (skipped.length > 0) {
      parts.push('<div class="mi-h4">Not evaluated</div>');
      parts.push(miNotice(
        'These programs have no parity result. Absence of a gap here is not evidence of a good conversion.',
        'warn'));
      parts.push(`<table class="mi-table">
        <thead><tr><th>COBOL Source</th><th>Reason</th></tr></thead>
        <tbody>${skipped.map(p => `<tr>
          <td>${miEscape(p.program)}</td>
          <td class="mi-dim">${miEscape(p.notEvaluatedReason || 'Not recorded')}</td>
        </tr>`).join('')}</tbody>
      </table>`);
    }

    return parts.join('');
  }

  _renderParityRow(p, threshold) {
    const score = p.score ?? 0;
    // Read the verdict the gate wrote. Colouring on score alone renders a program green when
    // an entire axis is absent but the weighted score still clears the threshold.
    const failed = p.failed ?? (score < threshold);
    const color = failed ? '#ef4444' : '#10b981';
    const gaps = p.gaps ?? [];
    const missing = gaps.filter(g => g.kind === 'Missing');
    const renamed = gaps.filter(g => g.kind === 'PossiblyRenamedOrMerged');
    const file = (p.generatedFile || '').split(/[\\/]/).pop() || '—';
    const stub = p.isDiagnosticStub
      ? ' <span class="mi-badge" style="color:#ef4444;border-color:#ef444444;background:#ef444418;">stub</span>'
      : '';
    const lost = (p.lostAxes ?? []).length
      ? ` <span class="mi-badge" style="color:#ef4444;border-color:#ef444444;background:#ef444418;" title="No symbol on this axis appears in code">${miEscape((p.lostAxes ?? []).join(', '))} lost</span>`
      : '';

    return `<tr>
      <td>${miEscape(p.program)}${stub}</td>
      <td class="mi-dim">${miEscape(file)}</td>
      <td style="color:${color};font-weight:600;">${score.toFixed(2)}${lost}</td>
      <td title="${miEscape(missing.map(g => `${g.axis}: ${g.symbol}`).join('\n'))}">${missing.length}</td>
      <td class="mi-dim" title="${miEscape(renamed.map(g => `${g.axis}: ${g.symbol}`).join('\n'))}">${renamed.length}</td>
      <td class="mi-dim">${miEscape(p.provenance || '—')}</td>
    </tr>`;
  }

  async _renderChain(body, token) {
    const qs = new URLSearchParams();
    if (this._chainJob) qs.set('job', this._chainJob);
    if (this._chainProgram) qs.set('program', this._chainProgram);
    const d = await this._get(`/api/modernization/service-chain${qs.toString() ? `?${qs}` : ''}`);
    if (token !== undefined && token !== this._renderToken) return;

    const jobOptions = ['<option value="">All jobs</option>']
      .concat((d.allJobNames || []).map(name =>
        `<option value="${miEscape(name)}"${name === this._chainJob ? ' selected' : ''}>${miEscape(name)}</option>`))
      .join('');

    body.innerHTML = `
      ${d.note ? miNotice(d.note, 'warn') : ''}
      <div class="mi-filters">
        <label>Job
          <select id="mi-chain-job">${jobOptions}</select>
        </label>
        <label>Program
          <input id="mi-chain-program" type="text" placeholder="e.g. CUSTMAST" value="${miEscape(this._chainProgram)}">
        </label>
        <button class="mi-btn" id="mi-chain-clear">Clear</button>
      </div>
      <div class="mi-stats">
        ${miStat('Jobs', d.totalJobs, 'JCL job cards')}
        ${miStat('Programs', d.totalPrograms, 'Reached from JCL or on disk')}
        ${miStat('Copybooks', d.totalCopybooks, 'Distinct, via COPY')}
        ${miStat('JCL→Pgm', d.jobToProgramEdges, 'EXEC PGM= steps')}
        ${miStat('Pgm→Cpy', d.programToCopybookEdges, 'COPY statements')}
      </div>
      ${d.mermaidTruncated
        ? miNotice(`Diagram capped at ${d.mermaidEdgeCount} edges. The tables and counts below cover the whole estate.`, 'info')
        : ''}
      <div class="mermaid mi-mermaid" id="mi-chain-diagram">${miEscape(d.mermaid || '')}</div>
      <h4 class="mi-h4">Jobs</h4>
      <table class="mi-table">
        <thead><tr><th>Job</th><th>JCL file</th><th>Programs</th></tr></thead>
        <tbody>${(d.jobs || []).map(j => `<tr>
          <td class="mi-mono">${miEscape(j.jobName)}</td>
          <td class="mi-dim mi-mono">${miEscape(j.jclFileName)}</td>
          <td class="mi-mono">${miEscape((j.primaryPrograms || []).join(', ')) || '—'}</td>
        </tr>`).join('') || '<tr><td colspan="3" class="mi-dim">No JCL jobs found.</td></tr>'}</tbody>
      </table>
      <h4 class="mi-h4">Programs</h4>
      <table class="mi-table">
        <thead><tr><th>Program</th><th>Fidelity</th><th>LoC</th><th>Copybooks</th><th>Run by</th></tr></thead>
        <tbody>${(d.programs || []).map(p => `<tr>
          <td class="mi-mono">${miEscape(p.basename)}</td>
          <td>${miFidelityBadge(p.parseFidelity)}</td>
          <td class="mi-num">${p.linesOfCode || 0}</td>
          <td class="mi-mono mi-dim">${miEscape((p.copybooks || []).join(', ')) || '—'}</td>
          <td class="mi-mono mi-dim">${miEscape((p.calledByJobs || []).join(', ')) || '<not run by any job>'}</td>
        </tr>`).join('') || '<tr><td colspan="5" class="mi-dim">No programs found.</td></tr>'}</tbody>
      </table>`;

    body.querySelector('#mi-chain-job').onchange = e => {
      this._chainJob = e.target.value;
      this._renderActive();
    };
    const progInput = body.querySelector('#mi-chain-program');
    // `change` fires on blur, `keydown` gives an immediate response to Enter.
    // Both funnel through the same guard so Enter-then-blur renders once.
    const applyProgram = value => {
      const next = value.trim();
      if (next === this._chainProgram) return;
      this._chainProgram = next;
      this._renderActive();
    };
    progInput.onchange = e => applyProgram(e.target.value);
    progInput.onkeydown = e => {
      if (e.key === 'Enter') applyProgram(e.target.value);
    };
    body.querySelector('#mi-chain-clear').onclick = () => {
      this._chainJob = '';
      this._chainProgram = '';
      this._renderActive();
    };

    await this._renderMermaid(body.querySelector('#mi-chain-diagram'));
  }

  async _renderMermaid(host) {
    if (!host || !window.mermaid) return;
    try {
      // mermaid.run consumes textContent, so the escaped markup we injected for
      // safety has to be handed back as plain text.
      host.textContent = host.textContent;
      host.removeAttribute('data-processed');
      await window.mermaid.run({ nodes: [host] });
    } catch (e) {
      console.error('Mermaid render failed:', e);
      host.innerHTML = miNotice('Diagram could not be rendered.', 'warn');
    }
  }
}

window.ModernizationIntelligenceView = ModernizationIntelligenceView;
