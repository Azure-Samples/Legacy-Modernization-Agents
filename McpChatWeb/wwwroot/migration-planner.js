// Migration Planner — interactive, sliceable migration-strategy report.
// Sources: /api/graph/rekt/galaxy (programs + dependency edges).
// Lets users tune weights / filters across LoC, complexity, criticality, SQL, CALLs
// and produces a 3-wave migration plan from "lowest-hanging fruit" upward.

class MigrationPlanner {
  constructor(rootId) {
    this.rootId = rootId;
    this.programs = [];      // raw enriched program rows
    this.edges = [];         // raw galaxy edges
    this.sortKey = 'ease';
    this.sortDir = 'desc';   // ease desc = easiest at top
    this.search = '';
    this.domainFilter = 'all';

    // Filter sliders (max thresholds)
    this.filters = {
      maxLoc: 100000,
      maxComplexity: 10000,
      maxSql: 10000,
      maxCalls: 1000,
      maxCriticality: 1000,
      includeCopybooks: false,
    };

    // Weights — how much each axis penalises ease (0 = ignore, 10 = dominant)
    this.weights = {
      loc: 5,
      complexity: 7,
      sql: 4,
      calls: 5,
      criticality: 6,
    };

    // Wave sizes
    this.waveSizes = { w1: 8, w2: 12 }; // remaining → wave 3

    // Replatform recommender — when refactor cost is too high, suggest
    // hosting the COBOL on a managed runtime (e.g. Micro Focus / OpenText
    // Enterprise Server, Heirloom, Raincode, GnuCOBOL on Linux, AWS
    // Blu Insights) instead of rewriting in Java.
    this.replatform = {
      enabled: true,
      easeThreshold: 25,        // ease ≤ this → flag as replatform candidate
      locThreshold: 5000,       // OR loc ≥ this
      criticalityThreshold: 30, // OR criticality ≥ this
    };

    this._lastBuilt = null;
    this._workbookOpen = false;
    this._workbookActiveSheet = null;
    this._workbookEdits = {};

    // Gantt panel state
    this._ganttOpen = true;
    this._ganttExpandedWaves = new Set([1, 2, 3]);

    // BIAN Wave Plan panel
    this._bianPlanOpen = true;

    // BIAN Wave Plan panel  }

  async loadAndRender() {
    const root = document.getElementById(this.rootId);
    if (!root) return;
    const scanId = (typeof _currentScanRunId !== 'undefined') ? _currentScanRunId : 'latest';
    // If the active scan run changed since the last fetch, drop the cache.
    if (this._scanRunIdAtFetch !== undefined && this._scanRunIdAtFetch !== scanId) {
      this.programs = [];
      this.edges = [];
      this._allRowsForBounds = null;
    }
    if (this.programs.length === 0) {
      root.innerHTML = '<div style="padding:24px;color:#94a3b8;">Loading migration data…</div>';
      try {
        const scanParam = (scanId && scanId !== 'latest' && scanId !== 'all') ? `?scanRunId=${scanId}` : '';
        const url = `/api/graph/rekt/galaxy${scanParam}`;
        const resp = await fetch(url);
        if (!resp.ok) throw new Error(`HTTP ${resp.status} from ${url}`);
        const ct = resp.headers.get('content-type') || '';
        if (!ct.includes('application/json')) throw new Error(`Expected JSON from ${url}, got ${ct}`);
        const data = await resp.json();
        this.programs = (data.programs || []).filter(p => p.program);
        this.edges = data.edges || [];
        this._scanRunIdAtFetch = scanId;
        this._lastApiSummary = `${this.programs.length} progs · ${this.edges.length} deps · scan ${data.pinned ? `#${data.scanRunId}` : 'latest'}`;
      } catch (e) {
        console.error('MigrationPlanner fetch error:', e);
        root.innerHTML = `<div style="padding:24px;color:#ef4444;">Failed to load migration data: ${this._esc(e.message)}<br><span style="color:#94a3b8;">Verify the Rekt Neo4j is reachable on bolt://localhost:7688 and that scans exist (<code>./doctor.sh rekt-full</code>).</span></div>`;
        return;
      }
    }
    this._render();
  }

  refresh() {
    this.programs = [];
    this.edges = [];
    this._allRowsForBounds = null;
    this._sliderMaxPrev = null;
    this.loadAndRender();
  }

  // Pre-compute per-program metrics (no filters/weights) so slider bounds stay stable.
  _buildAllRowsForBounds() {
    const inbound = new Map();
    const outbound = new Map();
    const norm = (s) => (s || '').replace(/\.cbl$/i, '').replace(/^flow-ast-/, '').toUpperCase();
    for (const e of this.edges) {
      const s = norm(e.source); const t = norm(e.target);
      outbound.set(s, (outbound.get(s) || 0) + 1);
      inbound.set(t, (inbound.get(t) || 0) + 1);
    }
    // Use deduplicated program list so slider maxes aren't skewed by repeats
    const seen = new Set();
    const unique = this.programs.filter(p => { if (seen.has(p.program)) return false; seen.add(p.program); return true; });
    this._allRowsForBounds = unique.map(p => {
      const key = norm(p.program);
      const inc = inbound.get(key) || 0;
      const out = outbound.get(key) || 0;
      return {
        lineCount: p.lineCount || 0,
        sqlCount: p.sqlCount || 0,
        callCount: p.callCount || 0,
        complexity: Math.round((p.lineCount||0) + 4*(p.branchCount||0) + 2*(p.callCount||0) + 1.5*(p.sqlCount||0) + 0.5*(p.sectionCount||0)),
        criticality: inc * 2 + out,
      };
    });
    return this._allRowsForBounds;
  }

  // ───────── domain classifier (mirrors ast-galaxy.js) ─────────
  _classifyDomain(progName, meta) {
    if (typeof ASTGalaxyView !== 'undefined' && ASTGalaxyView.prototype) {
      // Use the galaxy classifier if available so the labels match
      const tmp = Object.create(ASTGalaxyView.prototype);
      try { return tmp._classifyBusinessDomain(progName, meta); } catch {}
    }
    return 'Infrastructure';
  }

  // ───────── derived metrics + scoring ─────────
  _computeRows() {
    // Defensive dedupe (silent): the backend now filters by latest scan run, but
    // if any stray duplicates ever slip through we collapse them here too — without
    // surfacing it in the dashboard, per design.
    const dedup = new Map();
    for (const p of this.programs) {
      const cur = dedup.get(p.program);
      if (!cur) { dedup.set(p.program, { ...p }); continue; }
      cur.lineCount    = Math.max(cur.lineCount    || 0, p.lineCount    || 0);
      cur.nodeCount    = Math.max(cur.nodeCount    || 0, p.nodeCount    || 0);
      cur.sqlCount     = Math.max(cur.sqlCount     || 0, p.sqlCount     || 0);
      cur.callCount    = Math.max(cur.callCount    || 0, p.callCount    || 0);
      cur.branchCount  = Math.max(cur.branchCount  || 0, p.branchCount  || 0);
      cur.performCount = Math.max(cur.performCount || 0, p.performCount || 0);
      cur.sectionCount = Math.max(cur.sectionCount || 0, p.sectionCount || 0);
      cur.paraCount    = Math.max(cur.paraCount    || 0, p.paraCount    || 0);
    }
    const uniquePrograms = [...dedup.values()];
    this._uniqueProgramCount = uniquePrograms.length;

    // Build connection counts from edges
    const inbound = new Map();
    const outbound = new Map();
    const norm = (s) => (s || '').replace(/\.cbl$/i, '').replace(/^flow-ast-/, '').toUpperCase();
    for (const e of this.edges) {
      const s = norm(e.source); const t = norm(e.target);
      outbound.set(s, (outbound.get(s) || 0) + 1);
      inbound.set(t, (inbound.get(t) || 0) + 1);
    }

    const rows = [];
    for (const p of uniquePrograms) {
      if (!this.filters.includeCopybooks && p.isCopybook) continue;
      const key = norm(p.program);
      const inc = inbound.get(key) || 0;
      const out = outbound.get(key) || 0;
      const criticality = inc * 2 + out; // inbound counts double — being depended-on is riskier
      const complexity = (p.lineCount || 0)
        + 4 * (p.branchCount || 0)
        + 2 * (p.callCount || 0)
        + 1.5 * (p.sqlCount || 0)
        + 0.5 * (p.sectionCount || 0);

      rows.push({
        program: p.program,
        displayName: p.program.replace(/^flow-ast-/, '').replace(/\.cbl$/i, ''),
        domain: this._classifyDomain(p.program, p),
        isCopybook: !!p.isCopybook,
        lineCount: p.lineCount || 0,
        sqlCount: p.sqlCount || 0,
        callCount: p.callCount || 0,
        performCount: p.performCount || 0,
        branchCount: p.branchCount || 0,
        sectionCount: p.sectionCount || 0,
        paraCount: p.paraCount || 0,
        nodeCount: p.nodeCount || 0,
        inbound: inc,
        outbound: out,
        criticality,
        complexity: Math.round(complexity),
      });
    }

    // Apply filters
    let filtered = rows.filter(r =>
      r.lineCount  <= this.filters.maxLoc &&
      r.complexity <= this.filters.maxComplexity &&
      r.sqlCount   <= this.filters.maxSql &&
      r.callCount  <= this.filters.maxCalls &&
      r.criticality<= this.filters.maxCriticality
    );
    if (this.search) {
      const q = this.search.toLowerCase();
      filtered = filtered.filter(r =>
        r.displayName.toLowerCase().includes(q) ||
        r.domain.toLowerCase().includes(q)
      );
    }
    if (this.domainFilter !== 'all') {
      // Copybooks belong to 'Shared Data' domain — when "Include copybooks" is on,
      // they are exempt from the domain filter because they're cross-domain by nature.
      filtered = filtered.filter(r => r.domain === this.domainFilter || (this.filters.includeCopybooks && r.isCopybook));
    }

    // Normalise per-axis (0..1) so weights are commensurable
    const maxOf = (k) => filtered.reduce((m, r) => Math.max(m, r[k] || 0), 0) || 1;
    const norms = {
      loc:        maxOf('lineCount'),
      complexity: maxOf('complexity'),
      sql:        maxOf('sqlCount'),
      calls:      maxOf('callCount'),
      criticality:maxOf('criticality'),
    };
    const w = this.weights;
    const wSum = (w.loc + w.complexity + w.sql + w.calls + w.criticality) || 1;

    for (const r of filtered) {
      // weighted "difficulty" 0..1
      const diff =
        w.loc         * (r.lineCount   / norms.loc) +
        w.complexity  * (r.complexity  / norms.complexity) +
        w.sql         * (r.sqlCount    / norms.sql) +
        w.calls       * (r.callCount   / norms.calls) +
        w.criticality * (r.criticality / norms.criticality);
      r.difficulty = diff / wSum;            // 0 (easy) .. 1 (hard)
      r.ease = Math.round((1 - r.difficulty) * 100); // 0..100, higher = easier

      // Recommendation — rewrite (default) vs replatform (if too hard).
      // Triggers: low ease, OR very large LOC, OR high criticality.
      const rp = this.replatform || { enabled: false };
      const reasons = [];
      if (rp.enabled) {
        if (r.ease <= rp.easeThreshold)               reasons.push(`ease ${r.ease} ≤ ${rp.easeThreshold}`);
        if (r.lineCount >= rp.locThreshold)           reasons.push(`LOC ${r.lineCount} ≥ ${rp.locThreshold}`);
        if (r.criticality >= rp.criticalityThreshold) reasons.push(`criticality ${r.criticality} ≥ ${rp.criticalityThreshold}`);
      }
      if (reasons.length) {
        r.recommendation = 'replatform';
        r.recommendationReason = reasons.join(' · ');
      } else {
        r.recommendation = 'rewrite';
        r.recommendationReason = '';
      }
    }

    // Sort
    const dir = this.sortDir === 'asc' ? 1 : -1;
    filtered.sort((a, b) => {
      const av = a[this.sortKey]; const bv = b[this.sortKey];
      if (typeof av === 'string') return av.localeCompare(bv) * dir;
      return ((av ?? 0) - (bv ?? 0)) * dir;
    });

    return filtered;
  }

  _waveOf(rows) {
    // Sort a copy by ease desc to assign waves deterministically
    const byEase = [...rows].sort((a, b) => b.ease - a.ease);
    const w1 = new Set(byEase.slice(0, this.waveSizes.w1).map(r => r.program));
    const w2 = new Set(byEase.slice(this.waveSizes.w1, this.waveSizes.w1 + this.waveSizes.w2).map(r => r.program));
    return (r) => w1.has(r.program) ? 1 : w2.has(r.program) ? 2 : 3;
  }

  // ───────── rendering ─────────
  _render() {
    const root = document.getElementById(this.rootId);
    if (!root) return;
    const rows = this._computeRows();
    this._lastBuilt = rows;
    const waveOf = this._waveOf(rows);
    const seenDom = new Set();
    const domains = this.programs
      .filter(p => { if (seenDom.has(p.program)) return false; seenDom.add(p.program); return true; })
      .map(p => this._classifyDomain(p.program, p));
    const domainList = [...new Set(domains)].sort();

    // ── Sliders ──
    const slider = (key, label, min, max, step, value, fmt = String) => `
      <label style="display:flex;flex-direction:column;gap:2px;font-size:11px;color:#94a3b8;">
        <span style="display:flex;justify-content:space-between;">
          <span>${label}</span>
          <span style="color:#e2e8f0;font-weight:600;" id="mp-val-${key}">${fmt(value)}</span>
        </span>
        <input type="range" min="${min}" max="${max}" step="${step}" value="${value}" id="mp-input-${key}"
               style="accent-color:#60a5fa;width:100%;">
      </label>`;

    // Compute reasonable slider maxima from the FULL dataset (not filtered),
    // so users can always widen filters back out.
    const allRows = this._allRowsForBounds || this._buildAllRowsForBounds();
    const maxLoc = Math.max(100, ...allRows.map(r => r.lineCount || 0));
    const maxCx  = Math.max(1000, ...allRows.map(r => r.complexity || 0));
    const maxSql = Math.max(10,  ...allRows.map(r => r.sqlCount || 0));
    const maxCal = Math.max(5,   ...allRows.map(r => r.callCount || 0));
    const maxCr  = Math.max(5,   ...allRows.map(r => r.criticality || 0));

    // Auto-grow logic — when a slider is currently parked at the previous max
    // (i.e. user has effectively said "include everything for this axis"), let
    // the threshold follow new larger maxima so a fresh scan with bigger files
    // is automatically included. If the user has dragged the slider away from
    // the max, respect their value (clamp down only if it now exceeds the new
    // range — slider range is data-driven, can't display a value beyond max).
    const grow = (cur, prevMax, newMax) => {
      if (newMax <= 0) return 0;
      // First render: prevMax not yet recorded → snap to newMax (include all).
      if (prevMax == null) return newMax;
      // Slider was at the previous max → user wanted "include everything", follow.
      if (cur >= prevMax) return newMax;
      // User chose a deliberate value below the previous max → respect, clamp.
      return Math.min(cur, newMax);
    };
    const prev = this._sliderMaxPrev || {};
    this.filters.maxLoc         = grow(this.filters.maxLoc,         prev.loc,         maxLoc);
    this.filters.maxComplexity  = grow(this.filters.maxComplexity,  prev.cx,          maxCx);
    this.filters.maxSql         = grow(this.filters.maxSql,         prev.sql,         maxSql);
    this.filters.maxCalls       = grow(this.filters.maxCalls,       prev.cal,         maxCal);
    this.filters.maxCriticality = grow(this.filters.maxCriticality, prev.cr,          maxCr);
    this._sliderMaxPrev = { loc: maxLoc, cx: maxCx, sql: maxSql, cal: maxCal, cr: maxCr };

    // ── Wave summary cards ──
    const waveAgg = { 1:[], 2:[], 3:[] };
    for (const r of rows) waveAgg[waveOf(r)].push(r);
    const waveCard = (n, title, color, items) => {
      const totalLoc = items.reduce((s,r)=>s+r.lineCount,0);
      const totalSql = items.reduce((s,r)=>s+r.sqlCount,0);
      return `<div style="flex:1;background:rgba(15,23,42,0.6);border:1px solid ${color};border-radius:8px;padding:10px;">
        <div style="display:flex;justify-content:space-between;align-items:baseline;">
          <strong style="color:${color};font-size:13px;">Wave ${n} · ${title}</strong>
          <span style="color:#94a3b8;font-size:11px;">${items.length} services</span>
        </div>
        <div style="font-size:11px;color:#cbd5e1;margin-top:4px;">
          ${totalLoc.toLocaleString()} LOC · ${totalSql} SQL stmts
        </div>
        <div style="margin-top:6px;font-size:11px;color:#94a3b8;max-height:60px;overflow:hidden;line-height:1.4;">
          ${items.slice(0,6).map(r=>`<span style="display:inline-block;background:rgba(${n===1?'16,185,129':n===2?'245,158,11':'239,68,68'},0.15);color:#e2e8f0;padding:1px 6px;border-radius:3px;margin:1px 2px 1px 0;">${r.displayName}</span>`).join('')}
          ${items.length>6?`<span style="color:#64748b;">+${items.length-6} more</span>`:''}
        </div>
      </div>`;
    };

    // ── Table ──
    const sortIcon = (k) => this.sortKey === k ? (this.sortDir==='asc'?' ▲':' ▼') : '';
    const th = (k, label, align='left') => `<th onclick="migrationPlanner.toggleSort('${k}')"
      style="text-align:${align};padding:6px 8px;cursor:pointer;color:#60a5fa;border-bottom:1px solid #334155;user-select:none;white-space:nowrap;">${label}${sortIcon(k)}</th>`;

    const easeColor = (e) => e >= 70 ? '#10b981' : e >= 40 ? '#f59e0b' : '#ef4444';
    const waveBadge = (w) => {
      const c = w===1?'#10b981':w===2?'#f59e0b':'#ef4444';
      return `<span style="background:${c};color:#0f172a;padding:2px 6px;border-radius:10px;font-weight:700;font-size:10px;">W${w}</span>`;
    };

    const tableRows = rows.slice(0, 200).map(r => {
      const w = waveOf(r);
      const recBadge = r.recommendation === 'replatform'
        ? `<span title="Suggested replatform — ${this._escAttr(r.recommendationReason)}&#10;&#10;Replatform = host the existing COBOL on a managed runtime (Micro Focus / OpenText, Heirloom, Raincode, GnuCOBOL on Linux, AWS Blu Insights) instead of rewriting in Java." style="background:#7c2d12;color:#fed7aa;padding:1px 6px;border-radius:10px;font-weight:700;font-size:10px;cursor:help;">⇄ REPLATFORM</span>`
        : `<span title="Suggested full rewrite to Java." style="background:#1e3a8a;color:#bfdbfe;padding:1px 6px;border-radius:10px;font-weight:700;font-size:10px;cursor:help;">↻ REWRITE</span>`;
      return `<tr style="border-bottom:1px solid #1e293b;">
        <td style="padding:5px 8px;">${waveBadge(w)}</td>
        <td style="padding:5px 8px;color:#e2e8f0;font-weight:600;">${r.displayName}</td>
        <td style="padding:5px 8px;color:#94a3b8;font-size:11px;">${r.domain}</td>
        <td style="padding:5px 8px;text-align:right;">${r.lineCount.toLocaleString()}</td>
        <td style="padding:5px 8px;text-align:right;">${r.complexity.toLocaleString()}</td>
        <td style="padding:5px 8px;text-align:right;">${r.sqlCount}</td>
        <td style="padding:5px 8px;text-align:right;">${r.callCount}</td>
        <td style="padding:5px 8px;text-align:right;color:${r.criticality>10?'#fbbf24':'#cbd5e1'};" title="${r.inbound} programs depend on this one (inbound) · this one depends on ${r.outbound} others (outbound) · criticality = ${r.criticality}">${r.inbound}↓ / ${r.outbound}↑</td>
        <td style="padding:5px 8px;text-align:right;">
          <div style="display:inline-flex;align-items:center;gap:6px;">
            <div style="width:60px;height:6px;background:#1e293b;border-radius:3px;overflow:hidden;">
              <div style="width:${r.ease}%;height:100%;background:${easeColor(r.ease)};"></div>
            </div>
            <span style="color:${easeColor(r.ease)};font-weight:700;width:28px;text-align:right;">${r.ease}</span>
          </div>
        </td>
        <td style="padding:5px 8px;text-align:center;">${recBadge}</td>
      </tr>`;
    }).join('');

    root.innerHTML = `
      <div style="padding:10px 14px;border-bottom:1px solid #1e293b;background:rgba(15,23,42,0.6);">
        <div style="display:flex;justify-content:space-between;align-items:center;gap:12px;flex-wrap:wrap;">
          <div>
            <strong style="color:#e2e8f0;font-size:14px;">🎯 Migration Planner</strong>
            <span style="color:#94a3b8;font-size:11px;margin-left:8px;">${rows.length} of ${this._uniqueProgramCount || this.programs.length} programs · ${this.edges.length} dependencies${this._lastApiSummary ? ` · <span style="color:#64748b;" title="Source: /api/graph/rekt/galaxy">${this._esc(this._lastApiSummary)}</span>` : ''}</span>
          </div>
          <div style="display:flex;gap:8px;align-items:center;">
            <input type="text" id="mp-search" placeholder="Search programs / domains…" value="${this._escAttr(this.search)}"
              style="padding:5px 10px;background:#1e293b;color:#e2e8f0;border:1px solid #475569;border-radius:6px;font-size:12px;width:200px;">
            <select id="mp-domain" style="padding:5px 8px;background:#1e293b;color:#e2e8f0;border:1px solid #475569;border-radius:6px;font-size:12px;">
              <option value="all">All domains</option>
              ${domainList.map(d=>`<option value="${this._escAttr(d)}" ${d===this.domainFilter?'selected':''}>${d}</option>`).join('')}
            </select>
            <label style="font-size:11px;color:#94a3b8;display:flex;align-items:center;gap:4px;cursor:help;"
                   title="Copybooks (.cpy) are shared COBOL files COPY-included into programs at compile time — they aren't migrated on their own.&#10;&#10;OFF (default): only programs (.cbl) — the actual migration units.&#10;&#10;ON: also list copybooks. They typically appear as Wave 3 'hubs' because many programs depend on them. Useful for auditing shared data structures or planning a copybook-first refactor.">
              <input type="checkbox" id="mp-cpy" ${this.filters.includeCopybooks?'checked':''}> include copybooks ⓘ
            </label>
            <button id="mp-refresh" class="btn-small" title="Re-fetch /api/graph/rekt/galaxy with the active scan run">⟳ Refresh data</button>
            <button id="mp-export" class="btn-small" title="Download migration strategy as JSON">⬇ Strategy</button>
            <button id="mp-export-xlsx" class="btn-small" title="Download multi-sheet Excel workbook with timeline, waves, domains and per-program detail">⬇ Excel</button>
            <button id="mp-reset" class="btn-small" title="Reset all filters/weights">↻ Reset</button>
          </div>
        </div>
      </div>

      <div style="display:grid;grid-template-columns:1fr 1fr;gap:18px;padding:10px 14px;border-bottom:1px solid #1e293b;background:rgba(15,23,42,0.4);">
        <div>
          <div style="font-size:10px;color:#64748b;text-transform:uppercase;letter-spacing:0.05em;margin-bottom:6px;">Filters — exclude programs above threshold</div>
          <div style="display:grid;grid-template-columns:1fr 1fr;gap:8px 18px;">
            ${slider('maxLoc',         'Max LOC',          0, maxLoc, 10,  this.filters.maxLoc,         v=>v.toLocaleString())}
            ${slider('maxComplexity',  'Max Complexity',   0, maxCx,  10,  this.filters.maxComplexity,  v=>v.toLocaleString())}
            ${slider('maxSql',         'Max SQL stmts',    0, maxSql, 1,   this.filters.maxSql)}
            ${slider('maxCalls',       'Max CALLs',        0, maxCal, 1,   this.filters.maxCalls)}
            ${slider('maxCriticality', 'Max Criticality',  0, maxCr,  1,   this.filters.maxCriticality)}
          </div>
        </div>
        <div>
          <div style="font-size:10px;color:#64748b;text-transform:uppercase;letter-spacing:0.05em;margin-bottom:6px;">Weights — how heavily each axis penalises ease (0–10)</div>
          <div style="display:grid;grid-template-columns:1fr 1fr;gap:8px 18px;">
            ${slider('wLoc',         'LOC weight',         0, 10, 1, this.weights.loc)}
            ${slider('wComplexity',  'Complexity weight',  0, 10, 1, this.weights.complexity)}
            ${slider('wSql',         'SQL weight',         0, 10, 1, this.weights.sql)}
            ${slider('wCalls',       'CALL weight',        0, 10, 1, this.weights.calls)}
            ${slider('wCriticality', 'Criticality weight', 0, 10, 1, this.weights.criticality)}
          </div>
        </div>
      </div>

      <div style="display:flex;gap:10px;padding:10px 14px;border-bottom:1px solid #1e293b;">
        ${waveCard(1, 'Lowest-hanging fruit', '#10b981', waveAgg[1])}
        ${waveCard(2, 'Medium effort',         '#f59e0b', waveAgg[2])}
        ${waveCard(3, 'Hubs / hard cases',     '#ef4444', waveAgg[3])}
      </div>

      <!-- ── Replatform recommender ── -->
      <div style="display:flex;gap:14px;align-items:center;padding:8px 14px;border-bottom:1px solid #1e293b;background:rgba(124,45,18,0.12);">
        <label style="display:flex;align-items:center;gap:6px;font-size:12px;color:#fed7aa;font-weight:600;cursor:help;"
               title="When ON, programs that are too costly to rewrite are flagged as REPLATFORM candidates — keep the COBOL and host it on a managed runtime (Micro Focus / OpenText, Heirloom, Raincode, GnuCOBOL on Linux, AWS Blu Insights). The flag is informational and additive: replatformed services still appear in the wave plan.">
          <input type="checkbox" id="mp-rp-enabled" ${this.replatform.enabled?'checked':''} style="accent-color:#f97316;">
          ⇄ Suggest replatform for hard cases
        </label>
        <div style="display:flex;gap:14px;align-items:center;font-size:11px;color:#94a3b8;${this.replatform.enabled?'':'opacity:0.4;pointer-events:none;'}">
          <label style="display:flex;flex-direction:column;gap:2px;min-width:140px;">
            <span style="display:flex;justify-content:space-between;"><span>Ease ≤</span><span style="color:#fed7aa;font-weight:600;" id="mp-val-rpEase">${this.replatform.easeThreshold}</span></span>
            <input type="range" min="0" max="100" step="1" value="${this.replatform.easeThreshold}" id="mp-input-rpEase" style="accent-color:#f97316;">
          </label>
          <label style="display:flex;flex-direction:column;gap:2px;min-width:140px;">
            <span style="display:flex;justify-content:space-between;"><span>OR LOC ≥</span><span style="color:#fed7aa;font-weight:600;" id="mp-val-rpLoc">${this.replatform.locThreshold.toLocaleString()}</span></span>
            <input type="range" min="500" max="${Math.max(20000,maxLoc)}" step="500" value="${this.replatform.locThreshold}" id="mp-input-rpLoc" style="accent-color:#f97316;">
          </label>
          <label style="display:flex;flex-direction:column;gap:2px;min-width:140px;">
            <span style="display:flex;justify-content:space-between;"><span>OR Criticality ≥</span><span style="color:#fed7aa;font-weight:600;" id="mp-val-rpCrit">${this.replatform.criticalityThreshold}</span></span>
            <input type="range" min="0" max="${Math.max(50,maxCr)}" step="1" value="${this.replatform.criticalityThreshold}" id="mp-input-rpCrit" style="accent-color:#f97316;">
          </label>
        </div>
        <div style="margin-left:auto;font-size:12px;color:#fed7aa;font-weight:700;">
          ${this.replatform.enabled
            ? `${rows.filter(r=>r.recommendation==='replatform').length} of ${rows.length} flagged as REPLATFORM`
            : `<span style="color:#64748b;font-weight:400;">Replatform suggestions OFF — every program will be rewritten</span>`}
        </div>
      </div>

      <div style="flex:1;overflow:auto;">
        <table style="width:100%;border-collapse:collapse;font-size:12px;color:#cbd5e1;">
          <thead style="position:sticky;top:0;background:#0f172a;z-index:1;">
            <tr>
              ${th('wave','Wave','left')}
              ${th('displayName','Program','left')}
              ${th('domain','Domain','left')}
              ${th('lineCount','LOC','right')}
              ${th('complexity','Complexity','right')}
              ${th('sqlCount','SQL','right')}
              ${th('callCount','CALLs','right')}
              ${th('criticality','In↓ / Out↑ <span style="color:#64748b;cursor:help;" title="Dependency-edge counts from /api/graph/rekt/galaxy.edges.&#10;&#10;In↓ = inbound: how many other programs DEPEND ON this one (call it / copy from it). Higher = riskier to change.&#10;&#10;Out↑ = outbound: how many other programs THIS ONE depends on. Higher = more entanglement.&#10;&#10;Criticality score (used for ease/wave) = inbound × 2 + outbound — inbound counts double because being depended-on is harder to refactor.">ⓘ</span>','right')}
              ${th('ease','Ease','right')}
              <th style="text-align:center;padding:5px 8px;color:#94a3b8;font-weight:500;font-size:11px;cursor:default;" title="Recommendation: REWRITE = port to Java; REPLATFORM = host as-is on a managed COBOL runtime">Recommendation</th>
            </tr>
          </thead>
          <tbody>${tableRows || '<tr><td colspan="10" style="padding:24px;text-align:center;color:#64748b;">No programs match the current filters.</td></tr>'}</tbody>
        </table>
      </div>

      <!-- ── Gantt chart of suggested wave path ── -->
      <div id="mp-gantt-panel" style="border-top:2px solid #334155;background:linear-gradient(180deg,rgba(15,23,42,0.7),rgba(3,7,18,0.95));">
        <div style="display:flex;justify-content:space-between;align-items:center;padding:12px 16px;border-bottom:1px solid #1e293b;">
          <div>
            <strong style="color:#e2e8f0;font-size:14px;">📅 Migration Path — Gantt</strong>
            <span style="color:#94a3b8;font-size:11px;margin-left:8px;">Suggested timeline grouped by wave · click a wave header to expand/collapse · edits in the Strategy Workbook below feed into this chart live</span>
          </div>
          <div style="display:flex;gap:6px;align-items:center;">
            <button id="mp-gantt-expand-all" class="btn-small" title="Expand every wave">⊞ Expand all</button>
            <button id="mp-gantt-collapse-all" class="btn-small" title="Collapse every wave">⊟ Collapse all</button>
            <button id="mp-gantt-toggle" class="btn-small" title="Show / hide the chart">${this._ganttOpen?'▲ Hide':'▼ Show'}</button>
          </div>
        </div>
        <div id="mp-gantt-body" style="display:${this._ganttOpen?'block':'none'};">${this._ganttOpen ? this._renderGanttHTML() : ''}</div>
      </div>

      <!-- ── Editable Strategy Workbook (mirrors the Excel export) ── -->
      <div id="mp-workbook-panel" style="border-top:2px solid #334155;background:linear-gradient(180deg,rgba(15,23,42,0.7),rgba(3,7,18,0.95));">
        <div style="display:flex;justify-content:space-between;align-items:center;padding:12px 16px;border-bottom:1px solid #1e293b;">
          <div>
            <strong style="color:#e2e8f0;font-size:14px;">📊 Strategy Workbook</strong>
            <span style="color:#94a3b8;font-size:11px;margin-left:8px;">Editable preview of the Excel export — same 6 sheets · click any cell to edit</span>
          </div>
          <div style="display:flex;gap:6px;align-items:center;">
            <span id="mp-wb-saved" style="color:#10b981;font-size:11px;display:none;">✓ Edits saved (in-memory)</span>
            <button id="mp-wb-toggle" class="btn-small" title="Show / hide the editable workbook">${this._workbookOpen?'▲ Collapse':'▼ Expand'}</button>
            <button id="mp-wb-recalc" class="btn-small" title="Reload from current planner state — discards your edits">↻ Recalc</button>
            <button id="mp-wb-export" class="btn-small" style="background:#1e3a8a;color:#bfdbfe;border-color:#3b82f6;" title="Download the edited workbook as .xlsx">⬇ Export Excel</button>
          </div>
        </div>
        <div id="mp-wb-body" style="display:${this._workbookOpen?'block':'none'};">${this._workbookOpen ? this._renderWorkbookHTML() : ''}</div>
      </div>

      <!-- ── BIAN Wave Plan ── -->
      <div id="mp-bian-plan-panel" style="border-top:2px solid #1e3a8a;background:linear-gradient(180deg,rgba(15,23,42,0.7),rgba(3,7,18,0.95));">
        <div style="display:flex;justify-content:space-between;align-items:center;padding:12px 16px;border-bottom:1px solid #1e293b;">
          <div>
            <strong style="color:#93c5fd;font-size:14px;">🏦 BIAN Wave Plan</strong>
            <span style="color:#94a3b8;font-size:11px;margin-left:8px;">Programs positioned in the BIAN service landscape, coloured by migration wave</span>
          </div>
          <button id="mp-bian-plan-toggle" class="btn-small">${this._bianPlanOpen?'▲ Collapse':'▼ Expand'}</button>
        </div>
        <div id="mp-bian-plan-body" style="display:${this._bianPlanOpen?'block':'none'};">${this._bianPlanOpen ? this._renderBianPlanHTML(rows, waveOf) : ''}</div>
      </div>

      <!-- end of panels -->
    `;

    this._wireEvents();
    if (this._workbookOpen) this._wireWorkbookEvents();
    if (this._ganttOpen) this._wireGanttEvents();
  }

  // ─────────────────────────────────────────────────────────────────
  // EDITABLE WORKBOOK RENDERER
  // ─────────────────────────────────────────────────────────────────
  _renderWorkbookHTML() {
    const built = this._sheetsWithEdits();
    if (!built.sheets.length) {
      return '<div style="padding:24px;color:#94a3b8;">No programs to plan — adjust filters above.</div>';
    }
    const active = this._workbookActiveSheet || built.sheets[0].name;

    // Sheet tab strip
    const tabs = built.sheets.map(s => {
      const isActive = s.name === active;
      const editedCount = this._workbookEdits?.[s.name] ? Object.keys(this._workbookEdits[s.name]).length : 0;
      return `<button class="mp-wb-tab" data-sheet="${this._escAttr(s.name)}"
        style="padding:8px 14px;background:${isActive?'#1e293b':'transparent'};color:${isActive?'#60a5fa':'#94a3b8'};
               border:none;border-bottom:2px solid ${isActive?'#3b82f6':'transparent'};cursor:pointer;
               font-size:12px;font-weight:${isActive?'700':'500'};white-space:nowrap;">
        ${s.name}${editedCount?` <span style="color:#fbbf24;font-size:10px;">●${editedCount}</span>`:''}
      </button>`;
    }).join('');

    const sheet = built.sheets.find(s => s.name === active) || built.sheets[0];
    const grid = this._renderSheetGrid(sheet);

    // Headline cards (only on the Summary sheet)
    let hero = '';
    if (sheet.name === 'Summary' && built.totalCalendarWeeks != null) {
      const enriched = built.enriched || [];
      const totalLoc = enriched.reduce((s, r) => s + r.lineCount, 0);
      const totalDw = enriched.reduce((s, r) => s + r.devWeeks, 0).toFixed(1);
      const card = (icon, label, value, color) => `
        <div style="flex:1;background:rgba(15,23,42,0.6);border:1px solid ${color};border-radius:8px;padding:14px;min-width:140px;">
          <div style="font-size:22px;">${icon}</div>
          <div style="font-size:24px;font-weight:700;color:${color};margin-top:4px;">${value}</div>
          <div style="font-size:11px;color:#94a3b8;margin-top:2px;">${label}</div>
        </div>`;
      hero = `<div style="display:flex;gap:12px;padding:14px 16px;flex-wrap:wrap;">
        ${card('📦', 'Programs',          enriched.length,                '#3b82f6')}
        ${card('📏', 'Total LOC',         totalLoc.toLocaleString(),      '#a855f7')}
        ${card('📅', 'Calendar weeks',    built.totalCalendarWeeks,       '#10b981')}
        ${card('🧑‍💻', 'Dev-weeks',        totalDw,                        '#f59e0b')}
        ${this._renderWaveStrip(built.waveCalendar || [])}
      </div>`;
    }

    return `
      ${hero}
      <div style="display:flex;border-bottom:1px solid #1e293b;background:rgba(15,23,42,0.4);overflow-x:auto;">
        ${tabs}
      </div>
      <div style="padding:0;max-height:520px;overflow:auto;background:#0a0e1a;">
        ${grid}
      </div>
      <div style="padding:8px 16px;color:#64748b;font-size:11px;border-top:1px solid #1e293b;">
        Tip: click a cell to edit. <kbd style="background:#1e293b;padding:1px 5px;border-radius:3px;">Tab</kbd> moves to the next column,
        <kbd style="background:#1e293b;padding:1px 5px;border-radius:3px;">Enter</kbd> commits. Edits override the auto-computed values
        and are included in the Excel export. <b>Recalc</b> rebuilds from the planner state and discards edits.
      </div>`;
  }

  _renderWaveStrip(waveCalendar) {
    if (!waveCalendar.length) return '';
    const max = Math.max(1, ...waveCalendar.map(w => w.calendarWeeks));
    const colors = { 1: '#10b981', 2: '#f59e0b', 3: '#ef4444' };
    return `<div style="flex:2;background:rgba(15,23,42,0.6);border:1px solid #334155;border-radius:8px;padding:12px;min-width:300px;">
      <div style="font-size:11px;color:#94a3b8;margin-bottom:6px;">Timeline (calendar weeks per wave)</div>
      <div style="display:flex;gap:4px;height:40px;align-items:flex-end;">
        ${waveCalendar.map(w => `
          <div title="Wave ${w.wave} — ${w.title}: ${w.calendarWeeks} weeks (${w.startDate} → ${w.endDate})"
               style="flex:${Math.max(1, w.calendarWeeks)};background:${colors[w.wave]};border-radius:4px 4px 0 0;
                      height:${Math.max(20, (w.calendarWeeks / max) * 100)}%;display:flex;align-items:center;justify-content:center;
                      color:#0f172a;font-size:10px;font-weight:700;cursor:help;">
            W${w.wave}·${w.calendarWeeks}w
          </div>`).join('')}
      </div>
      <div style="display:flex;justify-content:space-between;font-size:10px;color:#64748b;margin-top:4px;">
        <span>${waveCalendar[0]?.startDate || ''}</span>
        <span>${waveCalendar[waveCalendar.length-1]?.endDate || ''}</span>
      </div>
    </div>`;
  }

  _renderSheetGrid(sheet) {
    const widthFor = i => sheet.cols?.[i]?.wch ? `${Math.max(80, sheet.cols[i].wch * 8)}px` : '120px';
    let html = '<table style="border-collapse:collapse;font-size:12px;color:#cbd5e1;width:100%;">';

    if (sheet.header) {
      html += '<thead style="position:sticky;top:0;background:#0f172a;z-index:1;"><tr>';
      sheet.header.forEach((h, i) => {
        html += `<th style="text-align:left;padding:8px 10px;color:#60a5fa;border-bottom:1px solid #334155;font-weight:600;white-space:nowrap;min-width:${widthFor(i)};">${this._esc(h)}</th>`;
      });
      html += '</tr></thead>';
    }

    html += '<tbody>';
    sheet.rows.forEach((row, ri) => {
      const isBlank = !row || row.length === 0 || row.every(c => c == null || c === '');
      if (isBlank) { html += '<tr><td colspan="20" style="height:8px;background:transparent;"></td></tr>'; return; }
      // Banner-style row (free-form sheets like Summary / Assumptions): single-cell title
      const isBanner = !sheet.header && row.length === 1 && typeof row[0] === 'string';
      if (isBanner) {
        html += `<tr><td colspan="20" style="padding:10px 12px;background:rgba(59,130,246,0.08);border-top:1px solid #1e293b;border-bottom:1px solid #1e293b;">
          <strong style="color:#60a5fa;font-size:12px;text-transform:uppercase;letter-spacing:0.05em;">${this._esc(row[0])}</strong>
        </td></tr>`;
        return;
      }
      html += `<tr style="border-bottom:1px solid #1e293b;">`;
      const colCount = sheet.header?.length || row.length;
      for (let ci = 0; ci < colCount; ci++) {
        const val = row[ci] ?? '';
        const isNum = typeof val === 'number';
        const cellStyle = `padding:6px 10px;text-align:${isNum?'right':'left'};white-space:nowrap;font-variant-numeric:tabular-nums;min-width:${widthFor(ci)};`;
        html += `<td contenteditable="true" data-sheet="${this._escAttr(sheet.name)}" data-r="${ri}" data-c="${ci}"
          style="${cellStyle}cursor:text;outline:none;"
          onfocus="this.style.background='rgba(59,130,246,0.15)';this.style.boxShadow='inset 0 0 0 1px #3b82f6';"
          onblur="this.style.background='';this.style.boxShadow='';"
        >${this._esc(val)}</td>`;
      }
      html += '</tr>';
    });
    html += '</tbody></table>';
    return html;
  }

  _wireWorkbookEvents() {
    const root = document.getElementById(this.rootId);
    if (!root) return;

    root.querySelectorAll('.mp-wb-tab').forEach(btn => {
      btn.addEventListener('click', () => {
        this._workbookActiveSheet = btn.dataset.sheet;
        document.getElementById('mp-wb-body').innerHTML = this._renderWorkbookHTML();
        this._wireWorkbookEvents();
      });
    });

    root.querySelectorAll('[contenteditable="true"][data-sheet]').forEach(td => {
      td.addEventListener('input', () => this._captureEdit(td));
      td.addEventListener('blur',  () => this._captureEdit(td));
      td.addEventListener('keydown', (e) => {
        if (e.key === 'Enter' && !e.shiftKey) {
          e.preventDefault();
          td.blur();
          // Move focus to the cell directly below (same column)
          const next = root.querySelector(`[data-sheet="${this._escAttr(td.dataset.sheet)}"][data-r="${Number(td.dataset.r) + 1}"][data-c="${td.dataset.c}"]`);
          next?.focus();
        }
      });
    });
  }

  _captureEdit(td) {
    const sheet = td.dataset.sheet;
    const r = Number(td.dataset.r), c = Number(td.dataset.c);
    const raw = td.textContent;
    // Coerce to number if it looks numeric (so xlsx exports as a number)
    const parsed = raw.trim() !== '' && !isNaN(Number(raw.replace(/,/g,''))) ? Number(raw.replace(/,/g,'')) : raw;

    this._workbookEdits = this._workbookEdits || {};
    this._workbookEdits[sheet] = this._workbookEdits[sheet] || {};
    const key = `${r}:${c}`;

    // Only record when actually different from the freshly-built value
    const fresh = this._buildWorkbookSheets().sheets.find(s => s.name === sheet)?.rows?.[r]?.[c];
    if (parsed === fresh || (parsed === '' && fresh == null)) {
      delete this._workbookEdits[sheet][key];
      if (!Object.keys(this._workbookEdits[sheet]).length) delete this._workbookEdits[sheet];
    } else {
      this._workbookEdits[sheet][key] = parsed;
    }

    // Subtle "saved" indicator
    const saved = document.getElementById('mp-wb-saved');
    if (saved) {
      saved.style.display = '';
      clearTimeout(this._savedTimer);
      this._savedTimer = setTimeout(() => { saved.style.display = 'none'; }, 1500);
    }
    // Update the dot count on the active tab
    const tab = document.querySelector(`.mp-wb-tab[data-sheet="${this._escAttr(sheet)}"]`);
    if (tab) {
      const n = Object.keys(this._workbookEdits[sheet] || {}).length;
      const dot = tab.querySelector('span');
      if (n) {
        if (dot) dot.textContent = `●${n}`;
        else tab.insertAdjacentHTML('beforeend', ` <span style="color:#fbbf24;font-size:10px;">●${n}</span>`);
      } else if (dot) dot.remove();
    }

    // Reflect edits live into the Gantt chart above (debounced).
    if (this._ganttOpen && (sheet === 'Programs' || sheet === 'Wave Plan')) {
      clearTimeout(this._ganttRefreshTimer);
      this._ganttRefreshTimer = setTimeout(() => {
        const body = document.getElementById('mp-gantt-body');
        if (body) { body.innerHTML = this._renderGanttHTML(); this._wireGanttRowEvents(); }
      }, 250);
    }
  }

  _wireEvents() {
    const root = document.getElementById(this.rootId);
    if (!root) return;
    const fmtMap = {
      maxLoc: v => Number(v).toLocaleString(),
      maxComplexity: v => Number(v).toLocaleString(),
    };
    const filterKeys = ['maxLoc','maxComplexity','maxSql','maxCalls','maxCriticality'];
    const weightKeyMap = { wLoc:'loc', wComplexity:'complexity', wSql:'sql', wCalls:'calls', wCriticality:'criticality' };

    for (const k of filterKeys) {
      const inp = root.querySelector(`#mp-input-${k}`);
      const val = root.querySelector(`#mp-val-${k}`);
      if (!inp) continue;
      inp.addEventListener('input', () => {
        const v = Number(inp.value);
        this.filters[k] = v;
        if (val) val.textContent = (fmtMap[k] || String)(v);
      });
      inp.addEventListener('change', () => this._render());
    }
    for (const [uiKey, fieldKey] of Object.entries(weightKeyMap)) {
      const inp = root.querySelector(`#mp-input-${uiKey}`);
      const val = root.querySelector(`#mp-val-${uiKey}`);
      if (!inp) continue;
      inp.addEventListener('input', () => {
        const v = Number(inp.value);
        this.weights[fieldKey] = v;
        if (val) val.textContent = String(v);
      });
      inp.addEventListener('change', () => this._render());
    }

    root.querySelector('#mp-search')?.addEventListener('input', e => { this.search = e.target.value; this._render(); });
    root.querySelector('#mp-domain')?.addEventListener('change', e => { this.domainFilter = e.target.value; this._render(); });
    root.querySelector('#mp-cpy')?.addEventListener('change', e => { this.filters.includeCopybooks = e.target.checked; this._render(); });

    // Replatform recommender controls
    root.querySelector('#mp-rp-enabled')?.addEventListener('change', e => {
      this.replatform.enabled = e.target.checked; this._render();
    });
    const rpSliders = [
      ['rpEase', 'easeThreshold', String],
      ['rpLoc',  'locThreshold',  v => Number(v).toLocaleString()],
      ['rpCrit', 'criticalityThreshold', String],
    ];
    for (const [uiKey, fieldKey, fmt] of rpSliders) {
      const inp = root.querySelector(`#mp-input-${uiKey}`);
      const val = root.querySelector(`#mp-val-${uiKey}`);
      if (!inp) continue;
      inp.addEventListener('input', () => {
        const v = Number(inp.value);
        this.replatform[fieldKey] = v;
        if (val) val.textContent = fmt(v);
      });
      inp.addEventListener('change', () => this._render());
    }

    root.querySelector('#mp-export')?.addEventListener('click', () => this.exportStrategy());
    root.querySelector('#mp-export-xlsx')?.addEventListener('click', () => this.exportExcel());
    root.querySelector('#mp-reset')?.addEventListener('click', () => this.resetAll());
    root.querySelector('#mp-refresh')?.addEventListener('click', () => this.refresh());

    root.querySelector('#mp-wb-toggle')?.addEventListener('click', () => {
      this._workbookOpen = !this._workbookOpen;
      this._render();
    });
    root.querySelector('#mp-wb-recalc')?.addEventListener('click', () => {
      if (Object.keys(this._workbookEdits || {}).length && !confirm('Recalc will discard your in-memory edits to the workbook. Continue?')) return;
      this._workbookEdits = {};
      const body = document.getElementById('mp-wb-body');
      if (body) { body.innerHTML = this._renderWorkbookHTML(); this._wireWorkbookEvents(); }
      const gbody = document.getElementById('mp-gantt-body');
      if (gbody && this._ganttOpen) { gbody.innerHTML = this._renderGanttHTML(); this._wireGanttRowEvents(); }
    });
    root.querySelector('#mp-wb-export')?.addEventListener('click', () => this.exportExcel());

    // BIAN Wave Plan panel
    root.querySelector('#mp-bian-plan-toggle')?.addEventListener('click', () => {
      this._bianPlanOpen = !this._bianPlanOpen;
      this._render();
    });
  }

  // ─────────────────────────────────────────────────────────────────
  // BIAN WAVE PLAN
  // ─────────────────────────────────────────────────────────────────

  _bianDomainOf(progName) {
    if (typeof ASTGalaxyView === 'undefined') return null;
    const key = (progName||'').replace(/\.cbl$/i,'').replace(/^flow-ast-/i,'').toUpperCase();
    for (const area of ASTGalaxyView.BIAN_LANDSCAPE) {
      for (const domain of area.domains) {
        if (domain.matches.includes(key)) return { area, domain };
      }
    }
    return null;
  }

  _renderBianPlanHTML(rows, waveOf) {
    if (typeof ASTGalaxyView === 'undefined' || !rows.length) {
      return '<div style="padding:20px;color:#64748b;">No data — run a scan first.</div>';
    }
    const norm = r => (r.program||'').replace(/\.cbl$/i,'').replace(/^flow-ast-/i,'').toUpperCase();
    const wc = { 1:'#10b981', 2:'#f59e0b', 3:'#ef4444' };
    const wl = { 1:'W1', 2:'W2', 3:'W3' };

    // Build a quick lookup: normalised program name → row
    const rowMap = new Map(rows.map(r => [norm(r), r]));
    const placed = new Set();

    const chip = (r) => {
      const w = waveOf(r);
      const isRp = r.recommendation === 'replatform';
      const bg = isRp ? '#7c2d12' : wc[w];
      const border = isRp ? '#dc2626' : wc[w];
      const badge = isRp ? '⇄' : wl[w];
      return `<span style="display:inline-flex;align-items:center;gap:3px;margin:2px;padding:3px 8px;
          background:${bg};border:1px solid ${border};border-radius:10px;font-size:10px;color:#e2e8f0;
          white-space:nowrap;cursor:pointer;"
        title="${norm(r)}&#10;Wave ${w} · LOC: ${r.lineCount} · Ease: ${Math.round(r.ease)}&#10;SQL: ${r.sqlCount} · CALLs: ${r.callCount}${isRp?'&#10;⇄ Replatform candidate':''}"
        onclick="if(typeof astExplorer!=='undefined')astExplorer.drillIntoProgram('${norm(r)}.cbl')">
        <span style="font-size:9px;font-weight:700;opacity:.8;">${badge}</span> ${norm(r)}
      </span>`;
    };

    const legend = `<div style="display:flex;gap:8px;margin-bottom:14px;flex-wrap:wrap;align-items:center;">
      <span style="font-size:11px;color:#94a3b8;">Wave →</span>
      ${[1,2,3].map(w=>`<span style="background:${wc[w]};color:#fff;padding:2px 9px;border-radius:10px;font-size:10px;font-weight:700;">${wl[w]} ${['Quick wins','Medium effort','Complex / hubs'][w-1]}</span>`).join('')}
      <span style="background:#7c2d12;color:#fed7aa;border:1px solid #dc2626;padding:2px 9px;border-radius:10px;font-size:10px;font-weight:700;">⇄ Replatform</span>
      <span style="font-size:10px;color:#64748b;margin-left:6px;">Click chip → AST Explorer</span>
    </div>`;

    let html = `<div style="padding:14px;background:#0f172a;">${legend}`;

    for (const area of ASTGalaxyView.BIAN_LANDSCAPE) {
      html += `<div style="margin-bottom:12px;border:1px solid ${area.borderColor};border-radius:8px;overflow:hidden;">
        <div style="background:${area.areaColor};padding:6px 14px;display:flex;align-items:center;gap:8px;">
          <span>${area.icon}</span>
          <span style="font-weight:700;color:#e2e8f0;font-size:13px;">${area.area}</span>
          <span style="font-size:10px;color:#94a3b8;margin-left:auto;">BIAN Business Area</span>
        </div>
        <div style="display:flex;flex-wrap:wrap;background:#0f172a;">`;
      for (const domain of area.domains) {
        const progChips = domain.matches
          .map(k => rowMap.get(k))
          .filter(Boolean)
          .sort((a, b) => waveOf(a) - waveOf(b));
        progChips.forEach(r => placed.add(norm(r)));
        html += `<div style="min-width:180px;flex:1;border-right:1px solid #1e293b;border-bottom:1px solid #1e293b;padding:8px;">
          <div style="font-size:11px;font-weight:600;color:#94a3b8;margin-bottom:6px;display:flex;gap:5px;align-items:center;">
            <span>${domain.icon}</span><span>${domain.name}</span>
            <span style="font-size:9px;color:#475569;margin-left:auto;">${domain.bianRef}</span>
          </div>
          <div style="font-size:10px;color:#475569;margin-bottom:6px;">${domain.desc}</div>
          <div style="display:flex;flex-wrap:wrap;min-height:28px;">
            ${progChips.length ? progChips.map(chip).join('') : '<span style="font-size:10px;color:#334155;font-style:italic;">no programs</span>'}
          </div>
        </div>`;
      }
      html += `</div></div>`;
    }

    // Unplaced rows
    const unplaced = rows.filter(r => !r.isCopybook && !placed.has(norm(r)));
    if (unplaced.length) {
      html += `<div style="border:1px solid #334155;border-radius:8px;overflow:hidden;margin-bottom:12px;">
        <div style="background:#1e293b;padding:6px 14px;font-weight:600;color:#64748b;font-size:12px;">⚠️ Unmapped programs</div>
        <div style="padding:8px;display:flex;flex-wrap:wrap;">${unplaced.map(chip).join('')}</div>
      </div>`;
    }

    html += `</div>`;
    return html;
  }

  // ─────────────────────────────────────────────────────────────────
  // GANTT / WATERFALL CHART
  // ─────────────────────────────────────────────────────────────────

  _wireGanttEvents() {
    const root = document.getElementById(this.rootId);
    if (!root) return;
    const refreshBody = () => {
      const body = document.getElementById('mp-gantt-body');
      if (body) { body.innerHTML = this._renderGanttHTML(); this._wireGanttRowEvents(); }
    };
    root.querySelector('#mp-gantt-toggle')?.addEventListener('click', () => {
      this._ganttOpen = !this._ganttOpen;
      this._render();
    });
    root.querySelector('#mp-gantt-expand-all')?.addEventListener('click', () => {
      this._ganttExpandedWaves = new Set([1, 2, 3]);
      refreshBody();
    });
    root.querySelector('#mp-gantt-collapse-all')?.addEventListener('click', () => {
      this._ganttExpandedWaves = new Set();
      refreshBody();
    });
    this._wireGanttRowEvents();
  }

  _wireGanttRowEvents() {
    const root = document.getElementById(this.rootId);
    if (!root) return;
    root.querySelectorAll('.mp-gantt-wave-toggle').forEach(btn => {
      btn.addEventListener('click', () => {
        const w = Number(btn.dataset.wave);
        if (this._ganttExpandedWaves.has(w)) this._ganttExpandedWaves.delete(w);
        else this._ganttExpandedWaves.add(w);
        const body = document.getElementById('mp-gantt-body');
        if (body) { body.innerHTML = this._renderGanttHTML(); this._wireGanttRowEvents(); }
      });
    });
  }

  _renderGanttHTML() {
    const built = this._sheetsWithEdits();
    if (!built.enriched || !built.enriched.length) {
      return '<div style="padding:24px;color:#94a3b8;">No programs to chart — adjust filters above.</div>';
    }
    const totalWeeks = Math.max(1, built.totalCalendarWeeks || 1);
    const waveColors = { 1: '#10b981', 2: '#f59e0b', 3: '#ef4444' };
    const waveColorsSoft = { 1: 'rgba(16,185,129,0.18)', 2: 'rgba(245,158,11,0.18)', 3: 'rgba(239,68,68,0.18)' };

    // Layout constants
    const labelColPx = 260;
    const trackPx = 12;          // px per week
    const minTrackPx = Math.max(700, totalWeeks * trackPx);
    const rowH = 26;

    // Week ruler (every week label, major every 4 weeks)
    let ruler = '';
    for (let w = 1; w <= totalWeeks; w++) {
      const left = (w - 1) * trackPx;
      const major = (w === 1 || w % 4 === 0 || w === totalWeeks);
      ruler += `<div style="position:absolute;left:${left}px;top:0;bottom:0;border-left:1px solid ${major?'#334155':'rgba(51,65,85,0.4)'};">
        ${major ? `<span style="position:absolute;top:2px;left:3px;font-size:10px;color:#64748b;">W${w}</span>` : ''}
      </div>`;
    }

    // Wave summary bands (always visible at top of chart area)
    let waveBands = '';
    for (const wc of (built.waveCalendar || [])) {
      const left = (wc.startWeek - 1) * trackPx;
      const width = Math.max(2, wc.calendarWeeks * trackPx);
      waveBands += `<div title="Wave ${wc.wave} — ${wc.title}\n${wc.startDate} → ${wc.endDate} (${wc.calendarWeeks} weeks, ${wc.services} services)"
        style="position:absolute;left:${left}px;top:4px;width:${width}px;height:18px;background:${waveColorsSoft[wc.wave]};border:1px solid ${waveColors[wc.wave]};border-radius:4px;color:${waveColors[wc.wave]};font-size:10px;font-weight:700;display:flex;align-items:center;justify-content:center;overflow:hidden;white-space:nowrap;">
        Wave ${wc.wave} · ${wc.title} · ${wc.calendarWeeks}w
      </div>`;
    }

    // Per-wave program rows
    const groupedByWave = { 1: [], 2: [], 3: [] };
    for (const r of built.enriched) (groupedByWave[r.wave] || (groupedByWave[r.wave] = [])).push(r);

    let waveSections = '';
    for (const w of [1, 2, 3]) {
      const items = (groupedByWave[w] || []).slice().sort((a, b) => a.startWeek - b.startWeek || b.devWeeks - a.devWeeks);
      if (!items.length) continue;
      const expanded = this._ganttExpandedWaves.has(w);
      const wc = (built.waveCalendar || []).find(x => x.wave === w);

      // Wave header row (collapsible)
      waveSections += `<div style="display:flex;align-items:center;border-top:1px solid #1e293b;background:rgba(15,23,42,0.6);">
        <button class="mp-gantt-wave-toggle" data-wave="${w}"
          style="width:${labelColPx}px;text-align:left;padding:8px 12px;background:transparent;border:none;color:#e2e8f0;font-size:12px;font-weight:700;cursor:pointer;display:flex;align-items:center;gap:6px;">
          <span style="display:inline-block;width:10px;color:${waveColors[w]};">${expanded?'▼':'▶'}</span>
          <span style="display:inline-block;width:10px;height:10px;background:${waveColors[w]};border-radius:2px;"></span>
          Wave ${w} <span style="color:#94a3b8;font-weight:400;">· ${items.length} progs · ${wc?wc.calendarWeeks+'w':''}</span>
        </button>
        <div style="position:relative;flex:1;height:${rowH}px;min-width:${minTrackPx}px;">
          ${wc ? `<div title="Wave ${w} band: ${wc.startDate} → ${wc.endDate}"
              style="position:absolute;left:${(wc.startWeek-1)*trackPx}px;top:6px;width:${Math.max(2,wc.calendarWeeks*trackPx)}px;height:14px;background:${waveColors[w]};opacity:0.85;border-radius:3px;display:flex;align-items:center;justify-content:center;color:#0f172a;font-size:10px;font-weight:700;overflow:hidden;white-space:nowrap;">
              ${wc.calendarWeeks}w · ${wc.services} services
            </div>` : ''}
        </div>
      </div>`;

      if (!expanded) continue;

      // Program rows
      for (const r of items) {
        const left = (r.startWeek - 1) * trackPx;
        const width = Math.max(2, (r.endWeek - r.startWeek + 1) * trackPx);
        const cpyTag = r.isCopybook ? ' <span style="color:#f59e0b;font-size:9px;">CPY</span>' : '';
        const isReplatform = r.recommendation === 'replatform';
        // Replatform bars: orange diagonal-stripe pattern over the wave color
        const barBg = isReplatform
          ? `repeating-linear-gradient(45deg, #f97316 0, #f97316 5px, #7c2d12 5px, #7c2d12 10px)`
          : waveColors[r.wave];
        const recTag = isReplatform ? ' <span style="color:#fed7aa;font-size:9px;background:#7c2d12;padding:0 4px;border-radius:3px;">⇄ RP</span>' : '';
        waveSections += `<div style="display:flex;align-items:center;border-top:1px solid rgba(30,41,59,0.6);">
          <div style="width:${labelColPx}px;padding:4px 12px 4px 32px;color:#cbd5e1;font-size:11px;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;" title="${this._escAttr(r.displayName)} — ${this._escAttr(r.domain)}${isReplatform?'\nReplatform suggested: '+this._escAttr(r.recommendationReason):''}">
            ${this._esc(r.displayName)}${cpyTag}${recTag}
            <span style="color:#64748b;font-size:10px;margin-left:6px;">${r.assignedDev || ''}</span>
          </div>
          <div style="position:relative;flex:1;height:${rowH-4}px;min-width:${minTrackPx}px;">
            <div title="${this._escAttr(r.displayName)} — ${r.domain}\nRecommendation: ${(r.recommendation||'rewrite').toUpperCase()}${isReplatform?' ('+r.recommendationReason+')':''}\nWave ${r.wave} · ${r.assignedDev}\nWeeks ${r.startWeek}–${r.endWeek} (${r.endWeek-r.startWeek+1}w)\n${r.startDate} → ${r.endDate}\nLOC ${r.lineCount} · SQL ${r.sqlCount} · CALLs ${r.callCount} · ${r.devWeeks}dw"
              style="position:absolute;left:${left}px;top:4px;width:${width}px;height:${rowH-12}px;background:${barBg};border-radius:3px;display:flex;align-items:center;padding:0 6px;color:${isReplatform?'#fff':'#0f172a'};font-size:10px;font-weight:700;cursor:help;overflow:hidden;white-space:nowrap;text-shadow:${isReplatform?'0 1px 2px rgba(0,0,0,0.6)':'none'};">
              ${r.devWeeks}dw${isReplatform?' · ⇄':''}
            </div>
          </div>
        </div>`;
      }
    }

    // Waterfall mode removed — Gantt only.

    // Legend strip
    const legend = `<div style="display:flex;gap:14px;padding:8px 16px;font-size:11px;color:#94a3b8;border-top:1px solid #1e293b;background:rgba(15,23,42,0.4);">
      <span><span style="display:inline-block;width:10px;height:10px;background:${waveColors[1]};border-radius:2px;vertical-align:middle;margin-right:4px;"></span>Wave 1 — lowest-hanging fruit</span>
      <span><span style="display:inline-block;width:10px;height:10px;background:${waveColors[2]};border-radius:2px;vertical-align:middle;margin-right:4px;"></span>Wave 2 — medium effort</span>
      <span><span style="display:inline-block;width:10px;height:10px;background:${waveColors[3]};border-radius:2px;vertical-align:middle;margin-right:4px;"></span>Wave 3 — hubs / hard cases</span>
      <span><span style="display:inline-block;width:14px;height:10px;background:repeating-linear-gradient(45deg,#f97316 0,#f97316 3px,#7c2d12 3px,#7c2d12 6px);border-radius:2px;vertical-align:middle;margin-right:4px;"></span>Replatform candidate</span>
      <span style="margin-left:auto;color:#64748b;">Bars = scheduled program window. Edit Wave / Start wk / End wk / Assigned to in the Strategy Workbook below to update bars.</span>
    </div>`;

    // Header row (ruler) — sticky
    const headerHTML = `<div style="display:flex;align-items:flex-end;border-bottom:1px solid #334155;background:#0f172a;position:sticky;top:0;z-index:2;">
      <div style="width:${labelColPx}px;padding:6px 12px;color:#94a3b8;font-size:11px;font-weight:600;">Program / Wave</div>
      <div style="position:relative;flex:1;height:30px;min-width:${minTrackPx}px;">
        ${ruler}
        ${waveBands}
      </div>
    </div>`;

    return `<div style="overflow:auto;max-height:520px;background:#0a0e1a;">
      ${headerHTML}
      ${waveSections || '<div style="padding:24px;color:#94a3b8;">No scheduled rows — adjust filters above.</div>'}
    </div>${legend}`;
  }

  toggleSort(key) {
    if (this.sortKey === key) this.sortDir = this.sortDir === 'asc' ? 'desc' : 'asc';
    else { this.sortKey = key; this.sortDir = (key === 'displayName' || key === 'domain') ? 'asc' : 'desc'; }
    this._render();
  }

  resetAll() {
    this.filters = { maxLoc: 100000, maxComplexity: 10000, maxSql: 10000, maxCalls: 1000, maxCriticality: 1000, includeCopybooks: false };
    this.weights = { loc: 5, complexity: 7, sql: 4, calls: 5, criticality: 6 };
    this.replatform = { enabled: true, easeThreshold: 25, locThreshold: 5000, criticalityThreshold: 30 };
    this.search = ''; this.domainFilter = 'all';
    this.sortKey = 'ease'; this.sortDir = 'desc';
    this._sliderMaxPrev = null; // re-snap to dataset maxes on next render
    this._render();
  }

  exportStrategy() {
    const rows = this._lastBuilt || this._computeRows();
    const waveOf = this._waveOf(rows);
    const grouped = { wave1: [], wave2: [], wave3: [] };
    for (const r of rows) {
      const w = waveOf(r);
      grouped[`wave${w}`].push({
        program: r.displayName, domain: r.domain,
        loc: r.lineCount, complexity: r.complexity,
        sql: r.sqlCount, calls: r.callCount,
        inbound: r.inbound, outbound: r.outbound,
        ease: r.ease,
      });
    }
    const payload = {
      generatedAt: new Date().toISOString(),
      filters: this.filters, weights: this.weights,
      summary: {
        totalPrograms: rows.length,
        wave1: grouped.wave1.length, wave2: grouped.wave2.length, wave3: grouped.wave3.length,
        totalLoc: rows.reduce((s,r)=>s+r.lineCount,0),
        totalSql: rows.reduce((s,r)=>s+r.sqlCount,0),
      },
      strategy: grouped,
    };
    const blob = new Blob([JSON.stringify(payload, null, 2)], { type: 'application/json' });
    const a = document.createElement('a');
    a.href = URL.createObjectURL(blob);
    a.download = `migration-strategy-${Date.now()}.json`;
    document.body.appendChild(a); a.click(); a.remove();
    setTimeout(() => URL.revokeObjectURL(a.href), 1000);
  }

  // ─────────────────────────────────────────────────────────────────
  // EXCEL EXPORT — multi-sheet .xlsx workbook with timeline + per-wave
  // breakdown so a delivery lead can hand it straight to PMs / leads.
  //
  // Sheets:
  //   1. Summary           — totals, assumptions, wave timeline
  //   2. Wave Plan         — start/end week per wave, services, LOC, effort
  //   3. Programs          — every program ranked, with assigned wave & weeks
  //   4. Domain Breakdown  — services per domain, suggested ownership team
  //   5. Per-Domain Detail — pivoted view: programs grouped by domain × wave
  //   6. Assumptions       — the ratios and weights used for the timeline
  //
  // Effort model (deliberately conservative & explicit so anyone can tune):
  //   base velocity     = 500 LOC / dev / week
  //   wave multiplier   = Wave 1 × 1.0 · Wave 2 × 1.5 · Wave 3 × 2.5
  //   sql multiplier    = +0.05 LOC-equiv per SQL statement
  //   call multiplier   = +20 LOC-equiv per outbound CALL
  //   integration buffer= +30% per wave for testing/integration
  //   team size         = 3 devs (parallel within a wave)
  // ─────────────────────────────────────────────────────────────────
  // ─────────────────────────────────────────────────────────────────
  // WORKBOOK SHEET BUILDER — single source of truth used by both the
  // Excel export and the in-page editable Strategy Workbook dashboard.
  // Returns: { sheets: [{ name, header, rows, cols, freeze? }, ...] }
  //   header: column-name strings (or null for free-form sheets with banners)
  //   rows:   2D array of cell values (string|number|null)
  //   cols:   { wch: N } per-column widths for xlsx export
  // ─────────────────────────────────────────────────────────────────
  _buildWorkbookSheets() {
    const rows = this._lastBuilt || this._computeRows();
    if (!rows.length) return { sheets: [], assumptions: null };
    const waveOf = this._waveOf(rows);

    const A = {
      velocityLocPerDevWeek: 500,
      teamSize: 3,
      waveMultiplier: { 1: 1.0, 2: 1.5, 3: 2.5 },
      sqlEffortLoc: 5,
      callEffortLoc: 20,
      integrationBufferPct: 30,
      startDate: new Date(),
    };

    const enriched = rows.map(r => {
      const w = waveOf(r);
      const baseLoc = r.lineCount + (r.sqlCount * A.sqlEffortLoc) + (r.outbound * A.callEffortLoc);
      const adjustedLoc = baseLoc * A.waveMultiplier[w];
      const devWeeks = adjustedLoc / A.velocityLocPerDevWeek;
      return { ...r, wave: w, baseEffortLoc: Math.round(baseLoc), adjustedEffortLoc: Math.round(adjustedLoc), devWeeks: +devWeeks.toFixed(2) };
    });

    const byWave = { 1: [], 2: [], 3: [] };
    for (const r of enriched) byWave[r.wave].push(r);
    const byDomain = new Map();
    for (const r of enriched) {
      if (!byDomain.has(r.domain)) byDomain.set(r.domain, []);
      byDomain.get(r.domain).push(r);
    }

    const waveCalendar = [];
    let cursorWeek = 0;
    for (const w of [1, 2, 3]) {
      const items = byWave[w];
      const sumDevWeeks = items.reduce((s, r) => s + r.devWeeks, 0);
      const calendarWeeks = items.length === 0 ? 0
        : Math.ceil((sumDevWeeks / A.teamSize) * (1 + A.integrationBufferPct / 100));
      const startWeek = cursorWeek + 1;
      const endWeek = cursorWeek + calendarWeeks;
      const sd = new Date(A.startDate); sd.setDate(sd.getDate() + cursorWeek * 7);
      const ed = new Date(A.startDate); ed.setDate(ed.getDate() + endWeek * 7);
      waveCalendar.push({
        wave: w,
        title: w === 1 ? 'Lowest-hanging fruit' : w === 2 ? 'Medium effort' : 'Hubs / hard cases',
        services: items.length,
        totalLoc: items.reduce((s, r) => s + r.lineCount, 0),
        totalSql: items.reduce((s, r) => s + r.sqlCount, 0),
        sumDevWeeks: +sumDevWeeks.toFixed(1),
        calendarWeeks, startWeek, endWeek,
        startDate: this._fmtDate(sd),
        endDate: this._fmtDate(ed),
      });
      cursorWeek = endWeek;
    }
    const totalCalendarWeeks = cursorWeek;

    for (const w of [1, 2, 3]) {
      const cal = waveCalendar[w - 1];
      const devLoad = Array(A.teamSize).fill(cal.startWeek - 1);
      const items = [...byWave[w]].sort((a, b) => b.devWeeks - a.devWeeks);
      for (const r of items) {
        const dev = devLoad.indexOf(Math.min(...devLoad));
        const start = devLoad[dev] + 1;
        const end = Math.max(start, Math.ceil(devLoad[dev] + r.devWeeks));
        devLoad[dev] = end;
        r.assignedDev = `Dev ${dev + 1}`;
        r.startWeek = start; r.endWeek = end;
        const sd = new Date(A.startDate); sd.setDate(sd.getDate() + (start - 1) * 7);
        const ed = new Date(A.startDate); ed.setDate(ed.getDate() + end * 7);
        r.startDate = this._fmtDate(sd);
        r.endDate = this._fmtDate(ed);
      }
    }

    const sheets = [];

    // 1. Summary
    sheets.push({
      name: 'Summary',
      header: null, // free-form key/value layout
      cols: [{wch:24},{wch:30},{wch:10},{wch:10},{wch:12},{wch:14},{wch:12},{wch:10},{wch:12},{wch:12}],
      rows: [
        ['COBOL → Java Migration Strategy'],
        ['Generated', new Date().toISOString()],
        [],
        ['Scope'],
        ['Total programs',          enriched.length],
        ['Total LOC',               enriched.reduce((s, r) => s + r.lineCount, 0)],
        ['Total SQL statements',    enriched.reduce((s, r) => s + r.sqlCount, 0)],
        ['Domains',                 byDomain.size],
        ['Total dependencies',      this.edges.length],
        [],
        ['Timeline (sequential waves, parallel within a wave)'],
        ['Total calendar weeks',    totalCalendarWeeks],
        ['Total dev-weeks',         +enriched.reduce((s, r) => s + r.devWeeks, 0).toFixed(1)],
        ['Team size (devs)',        A.teamSize],
        ['Velocity (LOC/dev/week)', A.velocityLocPerDevWeek],
        [],
        ['Wave', 'Title', 'Services', 'LOC', 'Dev-weeks', 'Calendar weeks', 'Start week', 'End week', 'Start date', 'End date'],
        ...waveCalendar.map(w => [w.wave, w.title, w.services, w.totalLoc, w.sumDevWeeks, w.calendarWeeks, w.startWeek, w.endWeek, w.startDate, w.endDate]),
      ],
    });

    // 2. Wave Plan
    sheets.push({
      name: 'Wave Plan',
      header: ['Wave', 'Title', 'Services', 'Total LOC', 'Total SQL', 'Sum dev-weeks', 'Calendar weeks', 'Start week', 'End week', 'Start date', 'End date'],
      cols: [{wch:6},{wch:24},{wch:10},{wch:12},{wch:10},{wch:14},{wch:14},{wch:10},{wch:10},{wch:12},{wch:12}],
      rows: waveCalendar.map(w => [w.wave, w.title, w.services, w.totalLoc, w.totalSql, w.sumDevWeeks, w.calendarWeeks, w.startWeek, w.endWeek, w.startDate, w.endDate]),
    });

    // 3. Programs
    sheets.push({
      name: 'Programs',
      header: ['Wave', 'Start week', 'End week', 'Start date', 'End date', 'Assigned to',
               'Program', 'Domain', 'Type', 'LOC', 'Complexity score', 'SQL', 'CALLs',
               'Inbound deps', 'Outbound deps', 'Criticality', 'Ease (0–100)',
               'Adjusted effort (LOC-equiv)', 'Dev-weeks',
               'Recommendation', 'Replatform reason'],
      cols: [{wch:6},{wch:10},{wch:10},{wch:12},{wch:12},{wch:10},
             {wch:22},{wch:24},{wch:10},{wch:8},{wch:14},{wch:6},{wch:6},
             {wch:12},{wch:12},{wch:11},{wch:12},{wch:18},{wch:10},
             {wch:14},{wch:40}],
      freeze: { ySplit: 1 },
      rows: enriched
        .sort((a, b) => a.wave - b.wave || a.startWeek - b.startWeek || b.ease - a.ease)
        .map(r => [r.wave, r.startWeek, r.endWeek, r.startDate, r.endDate, r.assignedDev,
                   r.displayName, r.domain, r.isCopybook ? 'copybook' : 'program',
                   r.lineCount, r.complexity, r.sqlCount, r.callCount,
                   r.inbound, r.outbound, r.criticality, r.ease,
                   r.adjustedEffortLoc, r.devWeeks,
                   (r.recommendation || 'rewrite').toUpperCase(),
                   r.recommendationReason || '']),
    });

    // 4. Domain Breakdown
    const domainSummary = [...byDomain.entries()].map(([dom, items]) => {
      const wavesPresent = [...new Set(items.map(r => r.wave))].sort().join(', ');
      return [
        dom,
        this._suggestSquadName(dom),
        items.length,
        items.reduce((s, r) => s + r.lineCount, 0),
        items.reduce((s, r) => s + r.sqlCount, 0),
        Math.round(items.reduce((s, r) => s + r.ease, 0) / items.length),
        +items.reduce((s, r) => s + r.devWeeks, 0).toFixed(1),
        wavesPresent,
      ];
    }).sort((a, b) => b[6] - a[6]);
    sheets.push({
      name: 'Domain Breakdown',
      header: ['Domain', 'Suggested squad', 'Services', 'Total LOC', 'Total SQL', 'Avg ease', 'Dev-weeks', 'Waves present'],
      cols: [{wch:24},{wch:22},{wch:10},{wch:12},{wch:10},{wch:10},{wch:12},{wch:14}],
      rows: domainSummary,
    });

    // 5. Per-Domain Detail
    const detail = [];
    for (const [dom, items] of byDomain) {
      const sorted = [...items].sort((a, b) => a.wave - b.wave || b.ease - a.ease);
      for (const r of sorted) detail.push([dom, r.wave, r.displayName, r.lineCount, r.sqlCount, r.callCount, r.ease, r.startWeek, r.endWeek, r.assignedDev]);
      detail.push([]);
    }
    sheets.push({
      name: 'Per-Domain Detail',
      header: ['Domain', 'Wave', 'Program', 'LOC', 'SQL', 'CALLs', 'Ease', 'Start week', 'End week', 'Assigned to'],
      cols: [{wch:24},{wch:6},{wch:22},{wch:8},{wch:6},{wch:6},{wch:6},{wch:10},{wch:10},{wch:12}],
      freeze: { ySplit: 1 },
      rows: detail,
    });

    // 6. Replatform Candidates — programs flagged as too costly to rewrite
    const rpRows = enriched.filter(r => r.recommendation === 'replatform')
      .sort((a, b) => a.ease - b.ease)
      .map(r => [r.displayName, r.domain, r.wave, r.lineCount, r.complexity, r.sqlCount,
                 r.callCount, r.criticality, r.ease, r.devWeeks, r.recommendationReason]);
    const rpEnabled = !!(this.replatform && this.replatform.enabled);
    sheets.push({
      name: 'Replatform Candidates',
      header: ['Program', 'Domain', 'Wave', 'LOC', 'Complexity', 'SQL', 'CALLs', 'Criticality',
               'Ease', 'Dev-weeks (rewrite estimate)', 'Trigger reason'],
      cols: [{wch:22},{wch:24},{wch:6},{wch:8},{wch:12},{wch:6},{wch:6},{wch:11},{wch:6},{wch:24},{wch:50}],
      freeze: { ySplit: 1 },
      rows: rpRows.length ? [
        ...rpRows,
        [],
        ['Suggestion: host these programs on a managed COBOL runtime instead of rewriting in Java.'],
        ['Candidate runtimes', '', 'Micro Focus / OpenText Enterprise Server, Heirloom Computing, Raincode, GnuCOBOL on Linux, AWS Blu Insights'],
        ['Why', '', 'Rewrite cost is dominated by long programs, high complexity, or high blast-radius (criticality). Replatforming preserves business logic, removes mainframe lock-in, and frees the team to rewrite the lower-cost programs first.'],
        ['Trade-offs', '', 'Replatformed code remains COBOL — language modernization is deferred. Plan a follow-up rewrite or strangler-pattern migration once dependencies are reduced.'],
      ] : [
        [rpEnabled
          ? 'No programs match the replatform thresholds — every program is recommended for rewrite.'
          : 'Replatform suggestions are OFF — toggle the recommender on in the Migration Planner header to populate this sheet.'],
      ],
    });

    // 7. Assumptions (free-form) — placed after Gantt so the workbook ends on the assumptions reference page.

    // 8. Gantt — one row per program with a timeline column per calendar week
    //    The schedule columns use '█' (active week) / '·' (inactive) so the sheet
    //    renders as a readable horizontal bar chart in Excel/Numbers/Google Sheets.
    const ganttHeader = ['Wave', 'Program', 'Domain', 'Assigned to',
                         'Start wk', 'End wk', 'Duration (wks)', 'Start date', 'End date'];
    const weekCols = [];
    for (let w = 1; w <= totalCalendarWeeks; w++) weekCols.push(`W${w}`);
    const ganttCols = [{wch:6},{wch:22},{wch:22},{wch:10},{wch:8},{wch:8},{wch:14},{wch:12},{wch:12},
                       ...weekCols.map(() => ({wch:3}))];
    const ganttRows = [];
    // Wave summary rows first
    for (const wc of waveCalendar) {
      const lane = weekCols.map((_, i) => {
        const wk = i + 1;
        return (wk >= wc.startWeek && wk <= wc.endWeek) ? '█' : '·';
      });
      ganttRows.push([
        `Wave ${wc.wave}`, wc.title, '— summary —', `${wc.services} progs`,
        wc.startWeek, wc.endWeek, wc.calendarWeeks, wc.startDate, wc.endDate,
        ...lane,
      ]);
    }
    ganttRows.push([]);
    // Per-program rows, sorted by wave then start week
    const ganttPrograms = enriched.slice().sort((a, b) =>
      a.wave - b.wave || a.startWeek - b.startWeek || b.devWeeks - a.devWeeks);
    for (const r of ganttPrograms) {
      const lane = weekCols.map((_, i) => {
        const wk = i + 1;
        return (wk >= r.startWeek && wk <= r.endWeek) ? '█' : '·';
      });
      ganttRows.push([
        r.wave, r.displayName, r.domain, r.assignedDev,
        r.startWeek, r.endWeek, (r.endWeek - r.startWeek + 1), r.startDate, r.endDate,
        ...lane,
      ]);
    }
    sheets.push({
      name: 'Gantt',
      header: [...ganttHeader, ...weekCols],
      cols: ganttCols,
      freeze: { ySplit: 1 },
      rows: ganttRows,
    });

    // 7. Assumptions (free-form)
    sheets.push({
      name: 'Assumptions',
      header: null,
      cols: [{wch:32},{wch:14},{wch:60}],
      rows: [
        ['Migration Estimate Assumptions'],
        [],
        ['Setting', 'Value', 'Notes'],
        ['Base velocity (LOC/dev/week)',   A.velocityLocPerDevWeek, 'COBOL → Java conversion. Includes coding + unit tests; excludes integration.'],
        ['Team size (devs)',               A.teamSize,              'Parallelism within a single wave.'],
        ['Wave 1 multiplier',              A.waveMultiplier[1],     'Lowest-hanging fruit — leaf nodes, low complexity.'],
        ['Wave 2 multiplier',              A.waveMultiplier[2],     'Medium effort.'],
        ['Wave 3 multiplier',              A.waveMultiplier[3],     'Hubs/highly-coupled — extra design time.'],
        ['SQL extra effort (LOC/stmt)',    A.sqlEffortLoc,          'Each embedded SQL statement adds porting cost (JPA/JDBC mapping).'],
        ['CALL extra effort (LOC/CALL)',   A.callEffortLoc,         'Each outbound CALL adds integration cost.'],
        ['Integration buffer (%/wave)',    A.integrationBufferPct,  'Added on top of dev-weeks for QA, integration tests, hardening.'],
        [],
        ['Active filters'],
        ['Max LOC',                        this.filters.maxLoc],
        ['Max complexity',                 this.filters.maxComplexity],
        ['Max SQL',                        this.filters.maxSql],
        ['Max CALLs',                      this.filters.maxCalls],
        ['Max criticality',                this.filters.maxCriticality],
        ['Include copybooks',              this.filters.includeCopybooks ? 'yes' : 'no'],
        ['Domain filter',                  this.domainFilter],
        ['Search',                         this.search || '(none)'],
        [],
        ['Active weights (0–10)'],
        ['LOC weight',                     this.weights.loc],
        ['Complexity weight',              this.weights.complexity],
        ['SQL weight',                     this.weights.sql],
        ['CALL weight',                    this.weights.calls],
        ['Criticality weight',             this.weights.criticality],
        [],
        ['Replatform recommender'],
        ['Enabled',                        this.replatform?.enabled ? 'yes' : 'no'],
        ['Trigger: ease ≤',                this.replatform?.easeThreshold,
          'Below this ease score, the program is flagged as a replatform candidate.'],
        ['Trigger: LOC ≥',                 this.replatform?.locThreshold,
          'Programs at or above this LOC are flagged regardless of ease.'],
        ['Trigger: criticality ≥',         this.replatform?.criticalityThreshold,
          'High-blast-radius programs (many inbound deps) are flagged regardless of ease.'],
        ['Replatform meaning',             '',
          'Host the existing COBOL on a managed runtime (Micro Focus / OpenText, Heirloom, Raincode, GnuCOBOL on Linux, AWS Blu Insights) instead of rewriting in Java.'],
      ],
    });

    return { sheets, totalCalendarWeeks, enriched, waveCalendar };
  }

  // Apply user edits (this._workbookEdits) on top of freshly-built sheets,
  // and propagate scheduling-relevant cell edits back into `enriched` and
  // `waveCalendar` so the Gantt re-renders with the edited values.
  _sheetsWithEdits() {
    const built = this._buildWorkbookSheets();
    if (!this._workbookEdits) return built;
    for (const sheet of built.sheets) {
      const edits = this._workbookEdits[sheet.name];
      if (!edits) continue;
      for (const key of Object.keys(edits)) {
        const [r, c] = key.split(':').map(Number);
        sheet.rows[r] = sheet.rows[r] || [];
        sheet.rows[r][c] = edits[key];
      }
    }

    // ── Push edits from "Programs" sheet back into built.enriched ──
    // Programs header (header row 0): Wave, Start week, End week, Start date,
    // End date, Assigned to, Program, Domain, Type, LOC, Complexity, SQL,
    // CALLs, Inbound, Outbound, Criticality, Ease, Adjusted effort, Dev-weeks
    const progs = built.sheets.find(s => s.name === 'Programs');
    if (progs && Array.isArray(built.enriched)) {
      const byName = new Map(built.enriched.map(r => [r.displayName, r]));
      for (const row of progs.rows) {
        if (!row || row.length === 0) continue;
        const name = row[6];
        const target = byName.get(name);
        if (!target) continue;
        const w = Number(row[0]); if (!isNaN(w) && w >= 1 && w <= 3) target.wave = w;
        const sw = Number(row[1]); if (!isNaN(sw)) target.startWeek = sw;
        const ew = Number(row[2]); if (!isNaN(ew)) target.endWeek = Math.max(sw || ew, ew);
        if (row[3] != null && row[3] !== '') target.startDate = String(row[3]);
        if (row[4] != null && row[4] !== '') target.endDate = String(row[4]);
        if (row[5] != null && row[5] !== '') target.assignedDev = String(row[5]);
      }
    }

    // ── Push edits from "Wave Plan" sheet back into built.waveCalendar ──
    // Wave Plan header: Wave, Title, Services, Total LOC, Total SQL,
    // Sum dev-weeks, Calendar weeks, Start week, End week, Start date, End date
    const wp = built.sheets.find(s => s.name === 'Wave Plan');
    if (wp && Array.isArray(built.waveCalendar)) {
      for (const row of wp.rows) {
        if (!row || row.length === 0) continue;
        const w = Number(row[0]);
        const cal = built.waveCalendar.find(x => x.wave === w);
        if (!cal) continue;
        if (row[1] != null && row[1] !== '') cal.title = String(row[1]);
        const sw = Number(row[7]); if (!isNaN(sw)) cal.startWeek = sw;
        const ew = Number(row[8]); if (!isNaN(ew)) cal.endWeek = Math.max(sw || ew, ew);
        const cw = Number(row[6]); if (!isNaN(cw)) cal.calendarWeeks = cw;
        if (row[9]  != null && row[9]  !== '') cal.startDate = String(row[9]);
        if (row[10] != null && row[10] !== '') cal.endDate   = String(row[10]);
      }
      built.totalCalendarWeeks = built.waveCalendar.reduce(
        (m, c) => Math.max(m, c.endWeek || 0), built.totalCalendarWeeks || 0);
    }
    return built;
  }

  exportExcel() {
    if (typeof XLSX === 'undefined') {
      alert('Excel library failed to load. Check your internet connection (cdn.jsdelivr.net).');
      return;
    }
    const built = this._sheetsWithEdits();
    if (!built.sheets.length) { alert('No programs in current view — adjust filters and try again.'); return; }
    const wb = XLSX.utils.book_new();
    for (const s of built.sheets) {
      const aoa = s.header ? [s.header, ...s.rows] : s.rows;
      const ws = XLSX.utils.aoa_to_sheet(aoa);
      if (s.cols) ws['!cols'] = s.cols;
      if (s.freeze) ws['!freeze'] = { xSplit: 0, ySplit: s.freeze.ySplit || 1 };
      XLSX.utils.book_append_sheet(wb, ws, s.name);
    }
    XLSX.writeFile(wb, `migration-strategy-${this._fmtDate(new Date())}.xlsx`);
  }

  _fmtDate(d) {
    const yyyy = d.getFullYear();
    const mm = String(d.getMonth() + 1).padStart(2, '0');
    const dd = String(d.getDate()).padStart(2, '0');
    return `${yyyy}-${mm}-${dd}`;
  }

  _suggestSquadName(domain) {
    const map = {
      'Customer Management':    'Customer Squad',
      'Account Operations':     'Accounts Squad',
      'Transaction Processing': 'Payments Squad',
      'Credit Card Services':   'Cards Squad',
      'Administration & Auth':  'Platform / IAM Squad',
      'Reporting & Export':     'Analytics Squad',
      'Shared Data':            'Shared Libraries Team',
      'Infrastructure':         'Platform Squad',
    };
    return map[domain] || 'TBD';
  }

  _esc(s) { return String(s ?? '').replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;'); }
  _escAttr(s) { return String(s ?? '').replace(/&/g,'&amp;').replace(/"/g,'&quot;').replace(/</g,'&lt;'); }
}

// Expose globally so dashboard-tabs.js can construct it lazily
window.MigrationPlanner = MigrationPlanner;
