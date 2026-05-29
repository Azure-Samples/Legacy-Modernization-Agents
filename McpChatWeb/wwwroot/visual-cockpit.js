// ─────────────────────────────────────────────────────────────────────────
// Visual Cockpit — Phase-3 highly-visual SVG dashboards per persona
// ─────────────────────────────────────────────────────────────────────────
// Purpose: at-a-glance visual decision surfaces. Where Insights Hub uses
// tables, Visual Cockpit uses SVG charts: gauges, donuts, heatmaps,
// status grids, bar charts. No external chart library — pure inline SVG.
//
// Personas:
//   🌐 Mission Control       — consolidated executive overview
//   💼 Business Owner         — outcomes / risk / progress (visual)
//   🏗 Architect              — coupling heatmap + domain map
//   👨‍💻 Developer              — per-program scorecards + cache/quality sparklines
//
// Reuses existing /api/modernization/* + /api/graph/rekt/services endpoints.
// Data fetched once and cached on persona switch.
// ─────────────────────────────────────────────────────────────────────────

class VisualCockpit {
  constructor(rootId) {
    this.root = document.getElementById(rootId);
    if (!this.root) return;
    this._activePersona = 'mission';
    this._data = null;
    this._renderShell();
  }

  _renderShell() {
    this.root.innerHTML = `
      <div class="vc-shell">
        <div class="vc-header">
          <div class="vc-title">
            <span class="vc-icon">🎨</span>
            <div>
              <div class="vc-title-main">Visual Cockpit</div>
              <div class="vc-title-sub">At-a-glance SVG dashboards · single-screen visibility · same data, decision-grade visuals</div>
            </div>
          </div>
          <button id="vc-refresh" class="vc-btn">⟳ Refresh</button>
        </div>

        <div class="vc-persona-bar">
          <button class="vc-persona vc-persona-active" data-persona="mission">
            <div class="vc-persona-emoji">🌐</div>
            <div class="vc-persona-label">Mission Control</div>
            <div class="vc-persona-sub">Consolidated · all signals</div>
          </button>
          <button class="vc-persona" data-persona="business">
            <div class="vc-persona-emoji">💼</div>
            <div class="vc-persona-label">Business Owner</div>
            <div class="vc-persona-sub">Outcomes · risk · progress</div>
          </button>
          <button class="vc-persona" data-persona="architect">
            <div class="vc-persona-emoji">🏗</div>
            <div class="vc-persona-label">Architect</div>
            <div class="vc-persona-sub">Coupling · domains · structure</div>
          </button>
          <button class="vc-persona" data-persona="developer">
            <div class="vc-persona-emoji">👨‍💻</div>
            <div class="vc-persona-label">Developer</div>
            <div class="vc-persona-sub">Scorecards · cache · gates</div>
          </button>
        </div>

        <div id="vc-body" class="vc-body"></div>
      </div>
    `;
    this.root.querySelectorAll('.vc-persona').forEach(btn => {
      btn.addEventListener('click', () => {
        this._activePersona = btn.dataset.persona;
        this.root.querySelectorAll('.vc-persona').forEach(b =>
          b.classList.toggle('vc-persona-active', b.dataset.persona === this._activePersona));
        this._renderActive();
      });
    });
    const refresh = this.root.querySelector('#vc-refresh');
    if (refresh) refresh.addEventListener('click', () => { this._data = null; this.loadAndRender(); });
  }

  async loadAndRender() {
    const body = this.root.querySelector('#vc-body');
    body.innerHTML = '<div class="vc-loading">Loading…</div>';
    try {
      if (!this._data) {
        const [dashboard, applications, health, topology, services, chain] = await Promise.all([
          fetch('/api/modernization/dashboard').then(r => r.json()),
          fetch('/api/modernization/applications').then(r => r.json()),
          fetch('/api/modernization/dependency-health').then(r => r.json()),
          fetch('/api/modernization/topology').then(r => r.json()),
          fetch('/api/graph/rekt/services').then(r => r.json()).catch(() => ({ nodes: [], edges: [] })),
          fetch('/api/modernization/service-chain').then(r => r.json()).catch(() => null),
        ]);
        this._data = { dashboard, applications, health, topology, services, chain };
      }
      this._renderActive();
    } catch (err) {
      body.innerHTML = `<div class="vc-error">${this._esc(err.message)}</div>`;
    }
  }

  _renderActive() {
    if (!this._data) { this.loadAndRender(); return; }
    const body = this.root.querySelector('#vc-body');
    if (this._activePersona === 'mission')   body.innerHTML = this._renderMission();
    else if (this._activePersona === 'business')  body.innerHTML = this._renderBusiness();
    else if (this._activePersona === 'architect') body.innerHTML = this._renderArchitect();
    else if (this._activePersona === 'developer') body.innerHTML = this._renderDeveloper();
  }

  // ═══════════════════════════════════════════════════════════════════════
  // SVG primitives — inline, no dependencies
  // ═══════════════════════════════════════════════════════════════════════

  /** Half-circle gauge (0-100). Color shifts red→amber→green. */
  _gauge(value, label, sub) {
    const pct = Math.max(0, Math.min(100, value));
    const angle = (pct / 100) * 180 - 90;  // -90 → 90 degrees
    const r = 70, cx = 90, cy = 80;
    const ax = cx + r * Math.cos(angle * Math.PI / 180);
    const ay = cy + r * Math.sin(angle * Math.PI / 180);
    const largeArc = pct > 50 ? 1 : 0;
    const startX = cx - r, startY = cy;
    const valueColor = pct >= 75 ? '#10b981' : pct >= 40 ? '#f59e0b' : '#ef4444';
    return `
      <svg viewBox="0 0 180 110" class="vc-gauge">
        <path d="M ${startX} ${startY} A ${r} ${r} 0 0 1 ${cx + r} ${cy}"
              stroke="#1e293b" stroke-width="14" fill="none" stroke-linecap="round"/>
        <path d="M ${startX} ${startY} A ${r} ${r} 0 ${largeArc} 1 ${ax} ${ay}"
              stroke="${valueColor}" stroke-width="14" fill="none" stroke-linecap="round"/>
        <text x="${cx}" y="${cy - 6}" text-anchor="middle" fill="#f8fafc" font-size="26" font-weight="700">${Math.round(pct)}<tspan font-size="14" fill="#94a3b8">%</tspan></text>
        <text x="${cx}" y="${cy + 14}" text-anchor="middle" fill="#cbd5e1" font-size="11">${this._esc(label || '')}</text>
      </svg>
      ${sub ? `<div class="vc-tile-sub">${this._esc(sub)}</div>` : ''}`;
  }

  /** Donut chart with legend. data = [{label, value, color}]. */
  _donut(data, size = 160, centerLabel = '') {
    const total = data.reduce((a, d) => a + d.value, 0);
    if (total === 0) return `<svg viewBox="0 0 ${size} ${size}" class="vc-donut"><circle cx="${size/2}" cy="${size/2}" r="${size/2-12}" fill="none" stroke="#1e293b" stroke-width="20"/><text x="${size/2}" y="${size/2}" text-anchor="middle" fill="#64748b" font-size="11">no data</text></svg>`;
    const cx = size / 2, cy = size / 2, r = size / 2 - 12;
    let startAngle = -Math.PI / 2; // start at top
    const segs = [];
    for (const d of data) {
      const sweep = (d.value / total) * Math.PI * 2;
      const endAngle = startAngle + sweep;
      const x1 = cx + r * Math.cos(startAngle);
      const y1 = cy + r * Math.sin(startAngle);
      const x2 = cx + r * Math.cos(endAngle);
      const y2 = cy + r * Math.sin(endAngle);
      const large = sweep > Math.PI ? 1 : 0;
      segs.push(`<path d="M ${x1} ${y1} A ${r} ${r} 0 ${large} 1 ${x2} ${y2}" stroke="${d.color}" stroke-width="20" fill="none">
        <title>${this._esc(d.label)}: ${d.value} (${(d.value*100/total).toFixed(0)}%)</title>
      </path>`);
      startAngle = endAngle;
    }
    return `
      <svg viewBox="0 0 ${size} ${size}" class="vc-donut">
        ${segs.join('')}
        <text x="${cx}" y="${cy - 4}" text-anchor="middle" fill="#f8fafc" font-size="22" font-weight="700">${total}</text>
        <text x="${cx}" y="${cy + 14}" text-anchor="middle" fill="#94a3b8" font-size="10">${this._esc(centerLabel)}</text>
      </svg>`;
  }

  _donutLegend(data) {
    const total = data.reduce((a, d) => a + d.value, 0);
    return `<div class="vc-legend">${data.map(d => `
      <div class="vc-legend-row">
        <span class="vc-swatch" style="background:${d.color};"></span>
        <span>${this._esc(d.label)}</span>
        <b style="margin-left:auto;">${d.value}</b>
        <span class="vc-muted">${total>0 ? (d.value*100/total).toFixed(0) : 0}%</span>
      </div>`).join('')}</div>`;
  }

  /** Horizontal bar chart. data = [{label, value, color?}]. */
  _bars(data, opts = {}) {
    const max = Math.max(1, ...data.map(d => d.value));
    const rowH = opts.rowHeight || 22;
    const labelW = opts.labelWidth || 120;
    const w = opts.width || 360;
    const barAreaW = w - labelW - 40;
    return `<svg viewBox="0 0 ${w} ${rowH * data.length + 4}" class="vc-bars">
      ${data.map((d, i) => {
        const y = i * rowH + 2;
        const barW = (d.value / max) * barAreaW;
        const color = d.color || '#3b82f6';
        return `
          <text x="0" y="${y + rowH/2 + 4}" fill="#cbd5e1" font-size="11">${this._esc(d.label)}</text>
          <rect x="${labelW}" y="${y + 2}" width="${barW}" height="${rowH - 6}" fill="${color}" rx="2">
            <title>${this._esc(d.label)}: ${d.value}</title>
          </rect>
          <text x="${labelW + barW + 4}" y="${y + rowH/2 + 4}" fill="#94a3b8" font-size="11" font-weight="600">${d.value}</text>
        `;
      }).join('')}
    </svg>`;
  }

  /** Color-coded status grid (one cell per program). */
  _statusGrid(items, opts = {}) {
    const cell = opts.cellSize || 18;
    const gap = opts.gap || 3;
    const cols = opts.cols || 12;
    const colors = {
      'verified': '#10b981',
      'converted': '#3b82f6',
      'partial-fallback': '#f59e0b',
      'compile-failing': '#ef4444',
      'not-started': '#475569',
      'not-in-source': '#1e293b',
    };
    const rows = Math.ceil(items.length / cols);
    const w = cols * (cell + gap) + gap;
    const h = rows * (cell + gap) + gap;
    return `<svg viewBox="0 0 ${w} ${h}" class="vc-grid">
      ${items.map((item, i) => {
        const cx = (i % cols) * (cell + gap) + gap;
        const cy = Math.floor(i / cols) * (cell + gap) + gap;
        const color = colors[item.modernizationStatus] || '#475569';
        const op = item.linesOfCode > 1500 ? 1 : item.linesOfCode > 500 ? 0.85 : 0.7;
        return `<rect x="${cx}" y="${cy}" width="${cell}" height="${cell}" fill="${color}" rx="2" opacity="${op}">
          <title>${this._esc(item.basename)} · ${item.linesOfCode.toLocaleString()} LoC · ${item.modernizationStatus}</title>
        </rect>`;
      }).join('')}
    </svg>`;
  }

  /** Coupling heatmap N×N matrix from edge list. */
  _heatmap(programs, edges, opts = {}) {
    const cell = opts.cellSize || 12;
    const cols = programs.length;
    const labelW = opts.labelWidth || 100;
    const labelH = opts.labelHeight || 70;
    const matrixSize = cols * cell;
    const w = labelW + matrixSize + 10;
    const h = labelH + matrixSize + 10;
    // Index lookup
    const idx = {};
    programs.forEach((p, i) => idx[p] = i);
    // Build edge count matrix
    const counts = {};
    for (const e of edges) {
      if (idx[e.source] === undefined || idx[e.target] === undefined) continue;
      const key = `${e.source}|${e.target}`;
      counts[key] = (counts[key] || 0) + 1;
    }
    const maxCount = Math.max(1, ...Object.values(counts));
    const cellAt = (s, t) => counts[`${s}|${t}`] || 0;
    return `<svg viewBox="0 0 ${w} ${h}" class="vc-heatmap">
      ${programs.map((p, i) => `
        <text x="${labelW - 4}" y="${labelH + i * cell + cell - 2}" text-anchor="end" fill="#cbd5e1" font-size="9">${this._esc(p.replace(/\.cbl$/i, ''))}</text>
        <text transform="translate(${labelW + i * cell + cell - 2}, ${labelH - 4}) rotate(-60)" fill="#cbd5e1" font-size="9">${this._esc(p.replace(/\.cbl$/i, ''))}</text>
      `).join('')}
      ${programs.map((s, i) => programs.map((t, j) => {
        const cnt = cellAt(s, t);
        if (cnt === 0) return '';
        const intensity = cnt / maxCount;
        const fill = `rgba(96, 165, 250, ${0.2 + intensity * 0.8})`;
        return `<rect x="${labelW + j * cell}" y="${labelH + i * cell}" width="${cell - 1}" height="${cell - 1}" fill="${fill}" rx="1">
          <title>${this._esc(s)} → ${this._esc(t)}: ${cnt}</title>
        </rect>`;
      }).join('')).join('')}
    </svg>`;
  }

  /** Sparkline. values = [number, ...]. */
  _spark(values, color = '#60a5fa') {
    if (!values || values.length === 0) return '';
    const w = 140, h = 36, pad = 2;
    const max = Math.max(...values, 1);
    const min = Math.min(...values, 0);
    const range = max - min || 1;
    const pts = values.map((v, i) => {
      const x = pad + (i / Math.max(1, values.length - 1)) * (w - pad * 2);
      const y = h - pad - ((v - min) / range) * (h - pad * 2);
      return `${x.toFixed(1)},${y.toFixed(1)}`;
    }).join(' ');
    return `<svg viewBox="0 0 ${w} ${h}" class="vc-spark">
      <polyline points="${pts}" fill="none" stroke="${color}" stroke-width="2"/>
    </svg>`;
  }

  // ═══════════════════════════════════════════════════════════════════════
  // 🌐 Mission Control — consolidated executive overview
  // ═══════════════════════════════════════════════════════════════════════
  _renderMission() {
    const { dashboard, applications, health, services } = this._data;
    const total = applications.length;
    const verified = applications.filter(a => a.modernizationStatus === 'verified').length;
    const partial = applications.filter(a => a.modernizationStatus === 'partial-fallback').length;
    const failing = applications.filter(a => a.modernizationStatus === 'compile-failing').length;
    const notStarted = applications.filter(a => a.modernizationStatus === 'not-started').length;

    const statusDonut = [
      { label: 'Verified', value: verified, color: '#10b981' },
      { label: 'Partial/Fallback', value: partial, color: '#f59e0b' },
      { label: 'Compile-failing', value: failing, color: '#ef4444' },
      { label: 'Not started', value: notStarted, color: '#475569' },
    ];

    const top5Blockers = (health.missingCopybooks || [])
      .sort((a, b) => b.referencedBy.length - a.referencedBy.length).slice(0, 5)
      .map(m => ({ label: m.copybook, value: m.referencedBy.length, color: '#ef4444' }));

    const topLoc = [...applications].sort((a, b) => b.linesOfCode - a.linesOfCode).slice(0, 8)
      .map(a => ({ label: a.basename.replace(/\.cbl$/i, ''), value: a.linesOfCode,
                   color: a.modernizationStatus === 'verified' ? '#10b981'
                        : a.modernizationStatus === 'compile-failing' ? '#ef4444'
                        : a.modernizationStatus === 'partial-fallback' ? '#f59e0b' : '#3b82f6' }));

    return `
      <div class="vc-section-title">🌐 Mission Control — Consolidated Modernization Overview</div>

      <div class="vc-tile-row">
        <div class="vc-tile vc-tile-wide">
          <div class="vc-tile-header">Estate readiness</div>
          ${this._gauge(health.readinessScore || 0, 'Readiness score', `${health.fullFidelityCount}/${total} full-fidelity programs`)}
        </div>
        <div class="vc-tile vc-tile-wide">
          <div class="vc-tile-header">Cache effectiveness</div>
          ${this._gauge(dashboard.cacheHitRatePct || 0, 'PR6 hit rate', `${dashboard.totalEvents || 0} events analysed`)}
        </div>
        <div class="vc-tile vc-tile-wide">
          <div class="vc-tile-header">Compile success</div>
          ${this._gauge(dashboard.recentCompileSuccessPct || 0, 'Last 5 gates', `${(dashboard.recentQuality || []).length} runs analysed`)}
        </div>
        <div class="vc-tile vc-tile-wide">
          <div class="vc-tile-header">LLM call success</div>
          ${this._gauge(dashboard.llmSuccessRatePct || 0, 'Reliability', `${(dashboard.llmCallOutcomes || []).reduce((a,o)=>a+o.count,0)} calls`)}
        </div>
      </div>

      <div class="vc-grid">
        <div class="vc-tile">
          <div class="vc-tile-header">Programs by status</div>
          <div class="vc-donut-with-legend">
            ${this._donut(statusDonut, 180, `${total} programs`)}
            ${this._donutLegend(statusDonut)}
          </div>
        </div>
        <div class="vc-tile">
          <div class="vc-tile-header">Estate at-a-glance (${total} programs colored by status, sized by LoC)</div>
          ${this._statusGrid(applications, { cols: 16 })}
          <div class="vc-legend-inline">
            <span><span class="vc-swatch" style="background:#10b981;"></span>verified</span>
            <span><span class="vc-swatch" style="background:#3b82f6;"></span>converted</span>
            <span><span class="vc-swatch" style="background:#f59e0b;"></span>partial</span>
            <span><span class="vc-swatch" style="background:#ef4444;"></span>failing</span>
            <span><span class="vc-swatch" style="background:#475569;"></span>not started</span>
          </div>
        </div>
      </div>

      <div class="vc-grid">
        <div class="vc-tile">
          <div class="vc-tile-header">Top investment-unlock blockers (programs unlocked per copybook)</div>
          ${top5Blockers.length ? this._bars(top5Blockers) : '<div class="vc-muted vc-pad">No blockers — estate fully resolved.</div>'}
        </div>
        <div class="vc-tile">
          <div class="vc-tile-header">Top 8 programs by LoC (colored by status)</div>
          ${this._bars(topLoc)}
        </div>
      </div>

      <div class="vc-callout vc-callout-blue">
        <b>📊 At-a-glance:</b> ${total} programs · ${health.readinessScore || 0}% estate readiness ·
        ${dashboard.cacheHitRatePct || 0}% cache hit rate · ${(services.edges || []).length} CALL edges ·
        ${health.totalMissingCopybooks || 0} missing copybooks blocking ${health.programsBlockedByMissing || 0} programs.
      </div>
    `;
  }

  // ═══════════════════════════════════════════════════════════════════════
  // 💼 Business Owner — outcomes / risk / progress (visual)
  // ═══════════════════════════════════════════════════════════════════════
  _renderBusiness() {
    const { health, applications, dashboard } = this._data;
    const total = applications.length;
    const verified = applications.filter(a => a.modernizationStatus === 'verified').length;
    const inProgress = applications.filter(a => a.modernizationStatus === 'converted' || a.modernizationStatus === 'partial-fallback').length;
    const blocked = applications.filter(a => a.modernizationStatus === 'compile-failing').length;
    const notStarted = total - verified - inProgress - blocked;
    const progressPct = total > 0 ? ((verified + inProgress * 0.5) / total) * 100 : 0;

    const risky = applications.map(a => ({
      ...a,
      riskScore: (a.modernizationStatus === 'compile-failing' ? 50 : 0)
               + (a.linesOfCode > 1500 ? 30 : a.linesOfCode > 500 ? 15 : 0)
               + (a.factsWarnings || 0) * 5
               + (a.factsConfidence < 3 ? 20 : 0),
    })).filter(a => a.riskScore > 0).sort((a, b) => b.riskScore - a.riskScore).slice(0, 8);

    const riskBars = risky.map(a => ({
      label: a.basename.replace(/\.cbl$/i, ''),
      value: a.riskScore,
      color: a.riskScore > 80 ? '#ef4444' : a.riskScore > 40 ? '#fb923c' : '#f59e0b',
    }));

    const top5 = (health.missingCopybooks || []).sort((a, b) => b.referencedBy.length - a.referencedBy.length).slice(0, 5);

    return `
      <div class="vc-section-title">💼 Business Owner — Modernization Program at-a-glance</div>

      <div class="vc-tile-row">
        <div class="vc-tile vc-tile-wide">
          <div class="vc-tile-header">Modernization progress</div>
          ${this._gauge(progressPct, '% of estate modernized', `${verified} verified + ${inProgress} in-progress of ${total}`)}
        </div>
        <div class="vc-tile vc-tile-wide">
          <div class="vc-tile-header">Estate readiness</div>
          ${this._gauge(health.readinessScore || 0, 'Weighted score', `${total - notStarted} of ${total} programs analysed`)}
        </div>
        <div class="vc-tile vc-tile-wide">
          <div class="vc-tile-header">Active blockers</div>
          <div class="vc-big-number" style="color:${health.programsBlockedByMissing > 0 ? '#ef4444' : '#10b981'};">${health.programsBlockedByMissing || 0}</div>
          <div class="vc-tile-sub">programs blocked by ${health.totalMissingCopybooks || 0} missing copybooks</div>
        </div>
      </div>

      <div class="vc-grid">
        <div class="vc-tile">
          <div class="vc-tile-header">🎯 Top 5 investment unlocks</div>
          <div class="vc-tile-sub">Acquiring these copybooks would unlock ${top5.reduce((a,m) => a + m.referencedBy.length, 0)} program-copybook references</div>
          ${top5.length ? this._bars(top5.map(m => ({
            label: m.copybook + '.cpy', value: m.referencedBy.length, color: '#10b981'
          })), { labelWidth: 140 }) : '<div class="vc-muted vc-pad">No blockers — full estate resolution.</div>'}
        </div>

        <div class="vc-tile">
          <div class="vc-tile-header">⚠ Modernization risk heatmap</div>
          <div class="vc-tile-sub">Programs scored on LoC, compile state, and facts confidence</div>
          ${riskBars.length ? this._bars(riskBars, { labelWidth: 140 }) : '<div class="vc-muted vc-pad">No high-risk programs identified.</div>'}
        </div>
      </div>

      <div class="vc-callout vc-callout-green">
        <b>💡 Recommended next investment:</b>
        ${top5.length > 0
          ? `Acquire <code>${top5.slice(0, 3).map(m => m.copybook + '.cpy').join('</code>, <code>')}</code> from the source-of-truth team.
             This single procurement step would mathematically unlock ${top5.slice(0, 3).reduce((a, m) => a + m.referencedBy.length, 0)} program-copybook references.`
          : 'Estate is fully resolved — proceed to migration wave planning.'}
      </div>
    `;
  }

  // ═══════════════════════════════════════════════════════════════════════
  // 🏗 Architect — coupling heatmap + domain map
  // ═══════════════════════════════════════════════════════════════════════
  _renderArchitect() {
    const { services, applications } = this._data;
    const edges = services.edges || [];
    const nodes = services.nodes || [];

    // Coupling per program
    const couplingMap = {};
    for (const e of edges) {
      couplingMap[e.source] = couplingMap[e.source] || { up: 0, down: 0 };
      couplingMap[e.target] = couplingMap[e.target] || { up: 0, down: 0 };
      couplingMap[e.source].down++;
      couplingMap[e.target].up++;
    }
    const couplingItems = Object.entries(couplingMap)
      .map(([name, c]) => ({ name, total: c.up + c.down, up: c.up, down: c.down }))
      .sort((a, b) => b.total - a.total);

    // Heatmap matrix of top-N coupled programs
    const topCoupled = couplingItems.slice(0, 18).map(c => c.name);

    // Domain clusters (4-letter prefix)
    const domains = {};
    for (const a of applications) {
      const pfx = a.basename.replace(/\.(cbl|cob)$/i, '').substring(0, 4).toUpperCase();
      if (!domains[pfx]) domains[pfx] = { prefix: pfx, count: 0, loc: 0 };
      domains[pfx].count++;
      domains[pfx].loc += a.linesOfCode || 0;
    }
    const domainBars = Object.values(domains).sort((a, b) => b.loc - a.loc).slice(0, 10)
      .map(d => ({ label: d.prefix + '*', value: d.loc, color: '#8b5cf6' }));

    const hubs = couplingItems.filter(c => c.down >= 3).slice(0, 6);
    const spofs = couplingItems.filter(c => c.up >= 3).slice(0, 6);

    return `
      <div class="vc-section-title">🏗 Architect — Structure, Coupling, Domains</div>

      <div class="vc-tile-row">
        <div class="vc-tile vc-tile-wide">
          <div class="vc-tile-header">Estate size</div>
          <div class="vc-big-number">${applications.length}</div>
          <div class="vc-tile-sub">programs · ${nodes.length} graph nodes</div>
        </div>
        <div class="vc-tile vc-tile-wide">
          <div class="vc-tile-header">CALL relationships</div>
          <div class="vc-big-number" style="color:#8b5cf6;">${edges.length}</div>
          <div class="vc-tile-sub">edges in the dependency graph</div>
        </div>
        <div class="vc-tile vc-tile-wide">
          <div class="vc-tile-header">Service hubs</div>
          <div class="vc-big-number" style="color:#10b981;">${hubs.length}</div>
          <div class="vc-tile-sub">programs orchestrating ≥3 others</div>
        </div>
        <div class="vc-tile vc-tile-wide">
          <div class="vc-tile-header">Single-points-of-failure</div>
          <div class="vc-big-number" style="color:#ef4444;">${spofs.length}</div>
          <div class="vc-tile-sub">programs ≥3 others depend on</div>
        </div>
      </div>

      <div class="vc-grid">
        <div class="vc-tile vc-tile-wide-2">
          <div class="vc-tile-header">🔥 Coupling heatmap — top ${topCoupled.length} most-connected programs</div>
          <div class="vc-tile-sub">Cells show CALL relationships. Intensity = call count. Hover for details.</div>
          ${topCoupled.length ? this._heatmap(topCoupled, edges) : '<div class="vc-muted vc-pad">No CALL edges yet — run REKT scan</div>'}
        </div>
      </div>

      <div class="vc-grid">
        <div class="vc-tile">
          <div class="vc-tile-header">🏷 Domain clusters by total LoC</div>
          <div class="vc-tile-sub">4-letter prefix heuristic; programs sharing a prefix likely share a domain</div>
          ${this._bars(domainBars, { labelWidth: 90 })}
        </div>

        <div class="vc-tile">
          <div class="vc-tile-header">🎯 Service candidates (downstream callers)</div>
          ${hubs.length ? this._bars(hubs.map(h => ({
            label: h.name.replace(/\.cbl$/i, ''), value: h.down, color: '#10b981'
          })), { labelWidth: 140 }) : '<div class="vc-muted vc-pad">No clear hubs in current corpus</div>'}
        </div>

        <div class="vc-tile">
          <div class="vc-tile-header">⚠ Single points of failure (upstream count)</div>
          ${spofs.length ? this._bars(spofs.map(s => ({
            label: s.name.replace(/\.cbl$/i, ''), value: s.up, color: '#ef4444'
          })), { labelWidth: 140 }) : '<div class="vc-muted vc-pad">No SPOFs in current corpus</div>'}
        </div>
      </div>

      <div class="vc-callout vc-callout-blue">
        <b>🏗 Architecture recommendation:</b> Migrate hubs (high downstream) LAST — they tie the system together.
        Migrate SPOFs (high upstream) with the most caution — sequencing matters.
        Treat each domain prefix as a candidate bounded context.
      </div>
    `;
  }

  // ═══════════════════════════════════════════════════════════════════════
  // 👨‍💻 Developer — per-program scorecards + cache/quality sparklines
  // ═══════════════════════════════════════════════════════════════════════
  _renderDeveloper() {
    const { dashboard, health, applications } = this._data;
    const reduction = dashboard.contextReduction || [];
    const recentQuality = (dashboard.recentQuality || []).slice(0, 8);

    // Per-program scorecard data (top 10 by LoC)
    const cards = [...applications]
      .filter(a => a.hasFacts)
      .sort((a, b) => b.linesOfCode - a.linesOfCode)
      .slice(0, 12)
      .map(a => {
        const reductionRow = reduction.find(r => r.file === a.basename);
        const reductionPct = reductionRow ? reductionRow.reductionPct : null;
        return { ...a, reductionPct };
      });

    // Compile-success "sparkline" over recent runs (oldest→newest, last 8 reversed)
    const compileSeries = recentQuality.slice().reverse().map(q => q.compileSuccess ? 1 : 0);
    const errorSeries = recentQuality.slice().reverse().map(q => q.compileErrors || 0);

    const cacheBars = Object.entries(dashboard.cacheDecisionCounts || {}).map(([k, n]) => ({
      label: k, value: n,
      color: k === 'hit' ? '#10b981' : k === 'miss-store' ? '#3b82f6' : '#94a3b8',
    }));

    const reductionBars = reduction.slice(0, 8).map(r => ({
      label: r.file.replace(/\.cbl$/i, ''),
      value: Math.round(r.reductionPct),
      color: r.reductionPct >= 80 ? '#10b981' : r.reductionPct >= 60 ? '#f59e0b' : '#fb923c',
    }));

    return `
      <div class="vc-section-title">👨‍💻 Developer — Per-Program Scorecards &amp; Pipeline Health</div>

      <div class="vc-tile-row">
        <div class="vc-tile vc-tile-wide">
          <div class="vc-tile-header">LLM avg latency</div>
          <div class="vc-big-number">${Math.round(((dashboard.llmCallOutcomes || []).find(o => o.outcome === 'success')?.avgDurationMs || 0) / 1000)}<span class="vc-unit">s</span></div>
          <div class="vc-tile-sub">per successful call</div>
        </div>
        <div class="vc-tile vc-tile-wide">
          <div class="vc-tile-header">Cache hit rate</div>
          ${this._gauge(dashboard.cacheHitRatePct || 0, 'PR6 cache', `${Object.values(dashboard.cacheDecisionCounts || {}).reduce((a,b)=>a+b,0)} decisions`)}
        </div>
        <div class="vc-tile vc-tile-wide">
          <div class="vc-tile-header">Context reduction</div>
          ${this._gauge(dashboard.avgContextReductionPct || 0, 'PR4 projection', `${reduction.length} programs measured`)}
        </div>
        <div class="vc-tile vc-tile-wide">
          <div class="vc-tile-header">Compile trend (last ${compileSeries.length})</div>
          ${compileSeries.length ? this._spark(compileSeries.map(v => v ? 100 : 0), '#10b981') : '<div class="vc-muted">no gates yet</div>'}
          <div class="vc-tile-sub">1 = pass, 0 = fail</div>
        </div>
        <div class="vc-tile vc-tile-wide">
          <div class="vc-tile-header">Error count trend</div>
          ${errorSeries.length ? this._spark(errorSeries, '#ef4444') : '<div class="vc-muted">no errors recorded</div>'}
          <div class="vc-tile-sub">per recent quality gate</div>
        </div>
      </div>

      <div class="vc-tile">
        <div class="vc-tile-header">📦 Per-program scorecards (top ${cards.length} by LoC)</div>
        <div class="vc-card-grid">
          ${cards.map(c => {
            const statusColor = c.modernizationStatus === 'verified' ? '#10b981'
                              : c.modernizationStatus === 'partial-fallback' ? '#f59e0b'
                              : c.modernizationStatus === 'compile-failing' ? '#ef4444'
                              : '#475569';
            return `<div class="vc-scorecard" style="border-left:3px solid ${statusColor};">
              <div class="vc-scorecard-title">${this._esc(c.basename)}</div>
              <div class="vc-scorecard-stats">
                <div><b>${c.linesOfCode.toLocaleString()}</b><span>LoC</span></div>
                <div><b>${c.factsConfidence || 0}</b><span>facts</span></div>
                <div><b>${c.dependencyCount || 0}</b><span>deps</span></div>
                <div><b>${c.projectionCacheHits || 0}</b><span>cache</span></div>
                <div><b>${c.reductionPct != null ? c.reductionPct.toFixed(0) + '%' : '—'}</b><span>Δ tok</span></div>
              </div>
              <div class="vc-scorecard-status" style="background:${statusColor}33; color:${statusColor};">${this._esc(c.modernizationStatus)}</div>
            </div>`;
          }).join('')}
        </div>
      </div>

      <div class="vc-grid">
        <div class="vc-tile">
          <div class="vc-tile-header">📉 Context reduction (% — projection vs raw REKT)</div>
          ${reductionBars.length ? this._bars(reductionBars) : '<div class="vc-muted vc-pad">no projection_metrics yet</div>'}
        </div>
        <div class="vc-tile">
          <div class="vc-tile-header">💾 Cache behavior</div>
          ${cacheBars.length ? this._bars(cacheBars) : '<div class="vc-muted vc-pad">no cache events yet</div>'}
        </div>
      </div>

      <div class="vc-callout vc-callout-blue">
        <b>👨‍💻 Developer tip:</b> Click a scorecard above (future) to drill into the program's REKT facts, run history, and recent conversion attempts.
        High-reduction (>80%) + many cache hits = projection working as intended.
      </div>
    `;
  }

  _esc(s) {
    if (s == null) return '';
    return String(s).replace(/[&<>"']/g, c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
  }
}

window.VisualCockpit = VisualCockpit;
