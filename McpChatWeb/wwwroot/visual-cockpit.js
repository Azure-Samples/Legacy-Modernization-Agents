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
//   🚀 Modernization Lead     — visual Kanban (Wave 1/2/3/Queued)
//   👨‍💻 Developer              — per-program scorecards + cache/quality sparklines
//
// Reuses existing /api/modernization/* + /api/graph/rekt/services endpoints.
// Data fetched once and cached on persona switch.
//
// Auto-refresh: when the Visual Cockpit tab is visible, data is refreshed
// every AUTO_REFRESH_MS so dashboards stay live during a running conversion.
// Suspended when document is hidden to avoid wasted polling.
// ─────────────────────────────────────────────────────────────────────────

const AUTO_REFRESH_MS = 15000;

class VisualCockpit {
  constructor(rootId) {
    this.root = document.getElementById(rootId);
    if (!this.root) return;
    this._activePersona = 'mission';
    this._data = null;
    this._autoTimer = null;
    this._isVisible = false;
    this._renderShell();
    this._setupAutoRefresh();
  }

  _setupAutoRefresh() {
    // Refresh on tab/visibility change AND on browser-tab visibility change.
    // The portal hides our root via display:none in dashboard-tabs.js when
    // not active. We poll the inline style + the document.hidden flag.
    const tick = () => {
      const visible = !document.hidden &&
        this.root && this.root.offsetParent !== null;
      if (visible !== this._isVisible) {
        this._isVisible = visible;
        if (visible) this._kickAutoRefresh();
        else this._stopAutoRefresh();
      }
    };
    document.addEventListener('visibilitychange', tick);
    // Poll inline-style visibility every 2s — cheap, covers tab switches in
    // the portal's dashboard-tabs router which mutates display:none.
    setInterval(tick, 2000);
    tick();
  }

  _kickAutoRefresh() {
    this._stopAutoRefresh();
    this._autoTimer = setInterval(() => this._autoRefresh(), AUTO_REFRESH_MS);
  }
  _stopAutoRefresh() {
    if (this._autoTimer) { clearInterval(this._autoTimer); this._autoTimer = null; }
  }
  async _autoRefresh() {
    // Background refresh — invalidate cache, re-fetch, re-render in place
    // without the loading spinner so the dashboard doesn't blink.
    //
    // CRITICAL: skip rendering when the user has anything open (drawer,
    // typing in search) — otherwise the entire panel gets nuked every 15s
    // and any drill-down they're inspecting disappears. The LIVE badge
    // still ticks so the user sees the surface is monitored.
    const drawer = this.root.querySelector('.vc-drawer');
    const drawerOpen = drawer && drawer.style.display === 'block';
    const active = document.activeElement;
    const typing = active && (active.tagName === 'INPUT' || active.tagName === 'TEXTAREA');
    if (drawerOpen || typing) {
      // Silent skip — just touch the badge timestamp so it doesn't go stale.
      const badge = this.root.querySelector('#vc-live-badge');
      if (badge) badge._lastTs = Date.now();
      return;
    }
    try {
      const fresh = await this._fetchAll();
      // Shallow-compare dashboards to skip pointless re-renders.
      const before = JSON.stringify(this._data?.dashboard || {});
      const after = JSON.stringify(fresh.dashboard || {});
      this._data = fresh;
      if (before !== after) {
        this._renderActive();
        this._flashLiveBadge();
      }
    } catch { /* silent — next tick will retry */ }
  }
  _flashLiveBadge() {
    const b = this.root.querySelector('#vc-live-badge');
    if (!b) return;
    b.classList.add('vc-live-flash');
    setTimeout(() => b.classList.remove('vc-live-flash'), 1200);
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
          <div class="vc-header-right">
            <input id="vc-locator" type="text" placeholder="🔎 Locate service (e.g. CALC_INTEREST or BDSM043)" class="vc-locator-input"/>
            <span id="vc-live-badge" class="vc-live-badge" title="Auto-refreshes every ${AUTO_REFRESH_MS/1000}s while this tab is visible">
              <span class="vc-live-dot"></span>LIVE
            </span>
            <button id="vc-refresh" class="vc-btn">⟳ Refresh</button>
          </div>
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
          <button class="vc-persona" data-persona="lead">
            <div class="vc-persona-emoji">🚀</div>
            <div class="vc-persona-label">Modernization Lead</div>
            <div class="vc-persona-sub">Waves · execution · Kanban</div>
          </button>
          <button class="vc-persona" data-persona="developer">
            <div class="vc-persona-emoji">👨‍💻</div>
            <div class="vc-persona-label">Developer</div>
            <div class="vc-persona-sub">Scorecards · cache · gates</div>
          </button>
        </div>

        <div id="vc-body" class="vc-body"></div>
        <div id="vc-drawer" class="vc-drawer" style="display:none;"></div>
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
    const locator = this.root.querySelector('#vc-locator');
    if (locator) {
      locator.addEventListener('keydown', async (e) => {
        if (e.key !== 'Enter') return;
        const q = locator.value.trim();
        if (!q) return;
        try {
          const r = await fetch(`/api/modernization/locate?q=${encodeURIComponent(q)}`).then(x => x.json());
          this._openLocatorDrawer(q, r);
        } catch (err) { console.error('locate failed', err); }
      });
    }
  }

  _openLocatorDrawer(q, r) {
    const drawer = this.root.querySelector('#vc-drawer');
    drawer.style.display = 'block';
    const empty = (r.javaMatches.length === 0 && r.cobolMatches.length === 0);
    drawer.innerHTML = `
      <div class="vc-drawer-header">
        <div>
          <div class="vc-drawer-title">🔎 Locate: <code>${this._esc(q)}</code></div>
          <div class="vc-drawer-sub">${r.cobolMatches.length} COBOL · ${r.javaMatches.length} generated · forms: ${r.forms.map(f => this._esc(f)).join(', ')}</div>
        </div>
        <button class="vc-btn" id="vc-drawer-close">✕</button>
      </div>
      ${empty ? `<div class="vc-muted vc-pad">No matches. Try a paragraph name (e.g. <code>CALC-INTEREST</code>), a generated class (<code>CalcInterestService</code> — Java or C#), or a program-ID (<code>BDSM043</code>).</div>` : `
        ${r.cobolMatches.length ? `
          <div class="vc-drawer-section">
            <div class="vc-drawer-section-title">📄 COBOL programs (${r.cobolMatches.length})</div>
            ${r.cobolMatches.map(m => `
              <div class="vc-locator-row-result">
                <div><b>${this._esc(m.basename)}</b> <span class="vc-muted">${this._esc(m.relativePath)}</span></div>
                ${m.matchedParagraphs.length ? `<div class="vc-drawer-chips">${m.matchedParagraphs.map(p => `<span class="vc-chip">${this._esc(p)}</span>`).join('')}</div>` : ''}
                <div style="margin-top:4px;">
                  <button class="vc-btn vc-btn-sm" data-cobol-detail="${this._esc(m.basename)}">📦 Open scorecard</button>
                </div>
              </div>`).join('')}
          </div>` : ''}
        ${r.javaMatches.length ? `
          <div class="vc-drawer-section">
            <div class="vc-drawer-section-title">☕ Generated code (${r.javaMatches.length})</div>
            ${r.javaMatches.map(j => `
              <div class="vc-locator-row-result">
                <div><b>${this._esc(j.fileName)}</b></div>
                <div class="vc-muted" style="font-size:10px;">${this._esc(j.path)}</div>
                ${j.runFolder ? `<div style="margin-top:2px;"><span class="vc-chip vc-chip-blue">run: ${this._esc(j.runFolder)}</span></div>` : ''}
              </div>`).join('')}
          </div>` : ''}
      `}
    `;
    drawer.querySelector('#vc-drawer-close').addEventListener('click', () => drawer.style.display = 'none');
    drawer.querySelectorAll('[data-cobol-detail]').forEach(btn => {
      btn.addEventListener('click', () => this._openProgramDrawer(btn.dataset.cobolDetail));
    });
  }

  async _fetchAll() {
    const [dashboard, applications, health, topology, services, chain, waves] = await Promise.all([
      fetch('/api/modernization/dashboard').then(r => r.json()),
      fetch('/api/modernization/applications').then(r => r.json()),
      fetch('/api/modernization/dependency-health').then(r => r.json()),
      fetch('/api/modernization/topology').then(r => r.json()),
      fetch('/api/graph/rekt/services').then(r => r.json()).catch(() => ({ nodes: [], edges: [] })),
      fetch('/api/modernization/service-chain').then(r => r.json()).catch(() => null),
      fetch('/api/modernization/waves').then(r => r.json()).catch(() => []),
    ]);
    return { dashboard, applications, health, topology, services, chain, waves };
  }

  async loadAndRender() {
    const body = this.root.querySelector('#vc-body');
    body.innerHTML = '<div class="vc-loading">Loading…</div>';
    try {
      if (!this._data) this._data = await this._fetchAll();
      this._renderActive();
    } catch (err) {
      body.innerHTML = `<div class="vc-error">${this._esc(err.message)}</div>`;
    }
  }

  _renderActive() {
    if (!this._data) { this.loadAndRender(); return; }
    const body = this.root.querySelector('#vc-body');
    if (this._activePersona === 'mission')        body.innerHTML = this._renderMission();
    else if (this._activePersona === 'business')  body.innerHTML = this._renderBusiness();
    else if (this._activePersona === 'architect') body.innerHTML = this._renderArchitect();
    else if (this._activePersona === 'lead')      { body.innerHTML = this._renderLead(); this._wireLeadInteractions(); }
    else if (this._activePersona === 'developer') { body.innerHTML = this._renderDeveloper(); this._wireScorecardClicks(); }
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
    // Auto-shrink cells when matrix is large so it stays readable.
    // Large matrices (>30) drop to 8px cells; small (<12) get 14px for clarity.
    const cols = programs.length;
    const autoCell = cols > 50 ? 6 : cols > 30 ? 8 : cols > 18 ? 10 : cols > 12 ? 12 : 14;
    const cell = opts.cellSize || autoCell;
    const labelW = opts.labelWidth || 100;
    const labelH = opts.labelHeight || (cols > 30 ? 90 : 70);
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
    // #14: heatmap is now zoomable. Default to top 18 for legibility, but
    // user can expand via the size slider (rendered just below the heatmap).
    const heatmapLimit = this._heatmapLimit || 18;
    const topCoupled = couplingItems.slice(0, Math.min(heatmapLimit, couplingItems.length)).map(c => c.name);

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
          <div class="vc-tile-header">🔥 Coupling heatmap — showing top ${topCoupled.length} of ${couplingItems.length} most-connected programs</div>
          <div class="vc-tile-sub">Cells show CALL relationships. Intensity = call count. Hover for details.</div>

          ${couplingItems.length > 0 ? `
            <div class="vc-heatmap-controls" style="margin-bottom:8px; margin-top:6px;">
              <label>📏 Show top
                <input type="range" min="6" max="${Math.min(Math.max(couplingItems.length, 18), 200)}" step="2"
                       value="${heatmapLimit}" id="vc-heatmap-zoom"
                       oninput="document.getElementById('vc-heatmap-zoom-val').textContent=this.value;"
                       onchange="window.visualCockpit?._setHeatmapLimit(parseInt(this.value));"
                       style="vertical-align:middle;width:240px; accent-color:var(--color-info);"/>
                <b id="vc-heatmap-zoom-val" style="color:var(--color-info);">${heatmapLimit}</b> / ${couplingItems.length}
              </label>
              <button class="vc-btn vc-btn-sm" onclick="window.visualCockpit?._setHeatmapLimit(${couplingItems.length});">show all (${couplingItems.length})</button>
              <button class="vc-btn vc-btn-sm" onclick="window.visualCockpit?._setHeatmapLimit(18);">reset (18)</button>
            </div>` : ''}

          <div class="vc-heatmap-scroll">
            ${topCoupled.length ? this._heatmap(topCoupled, edges) : '<div class="vc-muted vc-pad">No CALL edges yet — run REKT scan</div>'}
          </div>
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
            return `<div class="vc-scorecard" data-program="${this._esc(c.basename)}" style="border-left:3px solid ${statusColor};" title="Click for full REKT facts + run history">
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
        <b>👨‍💻 Developer tip:</b> Click any scorecard above to drill into REKT facts, dependencies, and run history.
        High-reduction (>80%) + many cache hits = projection working as intended.
      </div>
    `;
  }

  // ═══════════════════════════════════════════════════════════════════════
  // 🚀 Modernization Lead — visual Kanban (Wave 1/2/3/Queued)
  // ═══════════════════════════════════════════════════════════════════════
  _renderLead() {
    const { applications, waves, services } = this._data;

    // Build waveMap: basename → wave number (from POST'd assignments)
    const waveMap = {};
    (waves || []).forEach(w => { waveMap[w.basename || w.programBasename] = w.waveNumber; });

    // Auto-assign defaults for programs not yet placed.
    //   - full-fidelity programs default to wave 1 (ready to migrate)
    //   - converted/in-progress → wave 2
    //   - everything else → 'queued' (blocked / not started)
    const lanes = { 1: [], 2: [], 3: [], queued: [] };
    for (const a of applications) {
      let assigned = waveMap[a.basename];
      if (assigned == null) {
        assigned = a.modernizationStatus === 'verified' ? 1
                 : a.modernizationStatus === 'converted' ? 2
                 : a.modernizationStatus === 'partial-fallback' ? 2
                 : 'queued';
      }
      const key = (assigned === 1 || assigned === 2 || assigned === 3) ? assigned : 'queued';
      lanes[key].push(a);
    }

    // Sort each lane by status (verified first) then LoC ascending (build confidence early).
    Object.values(lanes).forEach(arr => arr.sort((a, b) => {
      const rank = s => s === 'verified' ? 0 : s === 'converted' ? 1 : s === 'partial-fallback' ? 2 : s === 'compile-failing' ? 3 : 4;
      const dr = rank(a.modernizationStatus) - rank(b.modernizationStatus);
      return dr !== 0 ? dr : a.linesOfCode - b.linesOfCode;
    }));

    const totalAssigned = (waves || []).length;
    const totalLoc = applications.reduce((a, x) => a + (x.linesOfCode || 0), 0);
    const wave1Loc = lanes[1].reduce((a, x) => a + (x.linesOfCode || 0), 0);
    const readyCount = applications.filter(a => a.modernizationStatus === 'verified').length;
    const blockedCount = lanes.queued.length;

    return `
      <div class="vc-section-title">🚀 Modernization Lead — Wave-Based Execution Kanban</div>

      <div class="vc-tile-row">
        <div class="vc-tile vc-tile-wide">
          <div class="vc-tile-header">Ready to migrate</div>
          <div class="vc-big-number" style="color:#10b981;">${readyCount}</div>
          <div class="vc-tile-sub">programs full-fidelity</div>
        </div>
        <div class="vc-tile vc-tile-wide">
          <div class="vc-tile-header">User wave assignments</div>
          <div class="vc-big-number" style="color:#3b82f6;">${totalAssigned}</div>
          <div class="vc-tile-sub">explicit (rest auto-suggested)</div>
        </div>
        <div class="vc-tile vc-tile-wide">
          <div class="vc-tile-header">Wave 1 footprint</div>
          <div class="vc-big-number">${wave1Loc.toLocaleString()}<span class="vc-unit">LoC</span></div>
          <div class="vc-tile-sub">${totalLoc > 0 ? Math.round(wave1Loc * 100 / totalLoc) : 0}% of estate</div>
        </div>
        <div class="vc-tile vc-tile-wide">
          <div class="vc-tile-header">Queued / blocked</div>
          <div class="vc-big-number" style="color:${blockedCount > 0 ? '#ef4444' : '#10b981'};">${blockedCount}</div>
          <div class="vc-tile-sub">copybook resolution required</div>
        </div>
      </div>

      <div class="vc-kanban">
        ${['1','2','3','queued'].map(k => this._renderKanbanLane(k, lanes[k] || lanes[parseInt(k)] || [])).join('')}
      </div>

      <div class="vc-callout vc-callout-blue">
        <b>🚀 Execution playbook:</b> Wave 1 = leaves &amp; full-fidelity (build confidence).
        Wave 2 = mid-coupling. Wave 3 = orchestrators / SPOFs (migrate LAST to avoid cascading risk).
        Queued = blocked on copybooks — resolve top blockers in Business Owner view.
        Click any card's wave button to reassign. All assignments persist in <code>Data/migration-waves.db</code>.
      </div>
    `;
  }

  _renderKanbanLane(key, items) {
    const meta = {
      '1':      { title: 'Wave 1 — Foundations',      color: '#10b981', emoji: '🟢', sub: 'leaves · full-fidelity · confidence builders' },
      '2':      { title: 'Wave 2 — Core',             color: '#3b82f6', emoji: '🔵', sub: 'mid-coupling · proven path' },
      '3':      { title: 'Wave 3 — Orchestrators',    color: '#8b5cf6', emoji: '🟣', sub: 'high-coupling · SPOFs · migrate LAST' },
      'queued': { title: 'Queued',                    color: '#64748b', emoji: '⚪', sub: 'blocked on missing deps' },
    };
    const m = meta[String(key)];
    const totalLoc = items.reduce((a, x) => a + (x.linesOfCode || 0), 0);
    return `
      <div class="vc-lane" style="border-top:3px solid ${m.color};">
        <div class="vc-lane-header">
          <span class="vc-lane-emoji">${m.emoji}</span>
          <div>
            <div class="vc-lane-title">${m.title}</div>
            <div class="vc-lane-sub">${m.sub}</div>
          </div>
          <div class="vc-lane-count">${items.length}</div>
        </div>
        <div class="vc-lane-meta">${totalLoc.toLocaleString()} LoC</div>
        <div class="vc-lane-body">
          ${items.length === 0
            ? '<div class="vc-lane-empty">no programs</div>'
            : items.map(a => this._renderKanbanCard(a, key)).join('')}
        </div>
      </div>
    `;
  }

  _renderKanbanCard(a, currentLane) {
    const statusColor = a.modernizationStatus === 'verified' ? '#10b981'
                      : a.modernizationStatus === 'partial-fallback' ? '#f59e0b'
                      : a.modernizationStatus === 'compile-failing' ? '#ef4444'
                      : a.modernizationStatus === 'converted' ? '#3b82f6'
                      : '#475569';
    const moves = ['1','2','3','queued'].filter(k => String(k) !== String(currentLane));
    return `
      <div class="vc-kanban-card" style="border-left:3px solid ${statusColor};">
        <div class="vc-kanban-card-title">${this._esc(a.basename)}</div>
        <div class="vc-kanban-card-meta">
          <span><b>${a.linesOfCode.toLocaleString()}</b> LoC</span>
          <span style="color:${statusColor};">${this._esc(a.modernizationStatus)}</span>
          <span class="vc-muted">conf ${a.factsConfidence || 0}</span>
        </div>
        <div class="vc-kanban-actions">
          ${moves.map(k => `<button class="vc-kanban-btn" data-program="${this._esc(a.basename)}" data-wave="${k}">→ ${k === 'queued' ? '⏸' : 'W' + k}</button>`).join('')}
        </div>
      </div>
    `;
  }

  _wireLeadInteractions() {
    this.root.querySelectorAll('.vc-kanban-btn').forEach(btn => {
      btn.addEventListener('click', async () => {
        const program = btn.dataset.program;
        const lane = btn.dataset.wave;
        btn.disabled = true; btn.textContent = '…';
        try {
          if (lane === 'queued') {
            // Remove explicit assignment → falls back to auto-suggestion
            await fetch(`/api/modernization/waves/${encodeURIComponent(program)}`, { method: 'DELETE' });
          } else {
            await fetch(`/api/modernization/waves/${encodeURIComponent(program)}`, {
              method: 'POST',
              headers: { 'Content-Type': 'application/json' },
              body: JSON.stringify({ waveNumber: parseInt(lane), notes: 'Visual Cockpit Kanban' }),
            });
          }
          // Refresh just the waves data and re-render lane
          const fresh = await fetch('/api/modernization/waves').then(r => r.json()).catch(() => []);
          this._data.waves = fresh;
          this._renderActive();
        } catch (err) {
          btn.disabled = false;
          btn.textContent = '↺';
          console.error('wave assign failed', err);
        }
      });
    });
  }

  // ═══════════════════════════════════════════════════════════════════════
  // Developer scorecard click-through → side drawer with REKT facts + run history
  // ═══════════════════════════════════════════════════════════════════════
  _wireScorecardClicks() {
    this.root.querySelectorAll('.vc-scorecard[data-program]').forEach(card => {
      card.addEventListener('click', () => {
        const basename = card.dataset.program;
        this._openProgramDrawer(basename);
      });
    });
  }

  async _openProgramDrawer(basename) {
    const drawer = this.root.querySelector('#vc-drawer');
    drawer.style.display = 'block';
    drawer.innerHTML = `<div class="vc-drawer-header">
      <div><b>${this._esc(basename)}</b> · loading detail…</div>
      <button class="vc-btn" id="vc-drawer-close">✕</button>
    </div>`;
    drawer.querySelector('#vc-drawer-close').addEventListener('click', () => drawer.style.display = 'none');
    try {
      const d = await fetch(`/api/modernization/programs/${encodeURIComponent(basename)}`).then(r => {
        if (!r.ok) throw new Error(`HTTP ${r.status}`);
        return r.json();
      });
      drawer.innerHTML = this._renderProgramDrawer(d);
      drawer.querySelector('#vc-drawer-close').addEventListener('click', () => drawer.style.display = 'none');
    } catch (err) {
      drawer.innerHTML = `<div class="vc-drawer-header">
        <div><b>${this._esc(basename)}</b> · <span style="color:#ef4444;">${this._esc(err.message)}</span></div>
        <button class="vc-btn" onclick="document.getElementById('vc-drawer').style.display='none'">✕</button>
      </div>`;
    }
  }

  _renderProgramDrawer(d) {
    const recentPass = d.runHistory.filter(r => r.compileSuccess === true).length;
    const recentFail = d.runHistory.filter(r => r.compileSuccess === false).length;
    return `
      <div class="vc-drawer-header">
        <div>
          <div class="vc-drawer-title">📄 ${this._esc(d.basename)}</div>
          <div class="vc-drawer-sub">${this._esc(d.relativePath)} · ${d.linesOfCode.toLocaleString()} LoC · facts confidence ${d.factsConfidence}/5</div>
        </div>
        <button class="vc-btn" id="vc-drawer-close">✕ Close</button>
      </div>

      <div class="vc-drawer-grid">
        <div class="vc-drawer-stat"><b>${d.copybooks.length}</b><span>Copybooks</span></div>
        <div class="vc-drawer-stat"><b>${d.callTargets.length}</b><span>CALL targets</span></div>
        <div class="vc-drawer-stat"><b>${d.dependencies.length}</b><span>Deps</span></div>
        <div class="vc-drawer-stat"><b>${d.factsWarnings.length}</b><span>Warnings</span></div>
        <div class="vc-drawer-stat"><b style="color:#10b981;">${recentPass}</b><span>Pass runs</span></div>
        <div class="vc-drawer-stat"><b style="color:#ef4444;">${recentFail}</b><span>Fail runs</span></div>
      </div>

      <div class="vc-drawer-section">
        <div class="vc-drawer-section-title">📚 Copybooks referenced (${d.copybooks.length})</div>
        <div class="vc-drawer-chips">
          ${d.copybooks.length ? d.copybooks.map(c => `<span class="vc-chip">${this._esc(c)}</span>`).join('') : '<span class="vc-muted">none</span>'}
        </div>
      </div>

      <div class="vc-drawer-section">
        <div class="vc-drawer-section-title">🔗 CALL targets (${d.callTargets.length})</div>
        <div class="vc-drawer-chips">
          ${d.callTargets.length ? d.callTargets.map(c => `<span class="vc-chip vc-chip-blue">${this._esc(c)}</span>`).join('') : '<span class="vc-muted">none</span>'}
        </div>
      </div>

      ${d.factsWarnings.length ? `
      <div class="vc-drawer-section">
        <div class="vc-drawer-section-title">⚠ Warnings (${d.factsWarnings.length})</div>
        <div class="vc-drawer-warnings">
          ${d.factsWarnings.map(w => `<div class="vc-drawer-warning">${this._esc(w)}</div>`).join('')}
        </div>
      </div>` : ''}

      <div class="vc-drawer-section">
        <div class="vc-drawer-section-title">📜 Run history (${d.runHistory.length})</div>
        ${d.runHistory.length === 0 ? '<div class="vc-muted">No conversion runs recorded for this program yet.</div>' : `
          <table class="vc-drawer-table">
            <thead><tr><th>Run</th><th>Started</th><th>Status</th><th>Compile</th><th>Errors</th><th>Classes</th><th>Fallback</th></tr></thead>
            <tbody>
              ${d.runHistory.map(r => `
                <tr>
                  <td><b>#${r.runId}</b></td>
                  <td>${this._esc((r.startedAt || '').replace('T',' ').substring(0,19))}</td>
                  <td>${this._esc(r.status)}</td>
                  <td>${r.compileSuccess === true ? '<span style="color:#10b981;">✓ pass</span>'
                       : r.compileSuccess === false ? '<span style="color:#ef4444;">✗ fail</span>'
                       : '<span class="vc-muted">—</span>'}</td>
                  <td>${r.compileErrors ?? '—'}</td>
                  <td>${r.generatedClasses ?? '—'}</td>
                  <td>${r.fallbackClasses ?? '—'}</td>
                </tr>
              `).join('')}
            </tbody>
          </table>
        `}
      </div>
    `;
  }

  _esc(s) {
    if (s == null) return '';
    return String(s).replace(/[&<>"']/g, c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
  }

  /** #14 heatmap zoom — re-renders the Architect persona with a new limit. */
  _setHeatmapLimit(n) {
    this._heatmapLimit = Math.max(4, Math.min(n, 200));
    if (this._activePersona === 'architect') this._renderActive();
  }
}

window.VisualCockpit = VisualCockpit;
