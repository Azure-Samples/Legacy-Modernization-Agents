// Estate Mission Control — estate-wide graph of transactions, APIs, jobs, programs, data and generated code,
// with business-function grouping, carve-out clusters and a migration wave plan.
// All data comes from /api/estate/mission (Estate/EstateMissionView.cs over the deterministic estate
// graph); nothing is invented here. Slices and conversions go through /api/estate/cluster/{id}/slice
// and /api/estate/slice/convert.

(function () {
  const esc = (v) => String(v ?? '').replace(/[&<>"']/g, c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
  const fmt = (n) => typeof n === 'number' ? n.toLocaleString('en-US', { maximumFractionDigits: 1 }) : esc(n ?? '—');
  const pct = (x) => x == null ? '—' : `${(x * 100).toFixed(0)}%`;
  const card = 'background:#111827;border:1px solid #1f2937;border-radius:10px;padding:12px;';
  const muted = 'color:#94a3b8;font-size:12px;';
  const sel = 'background:#0f172a;color:#e2e8f0;border:1px solid #334155;border-radius:6px;padding:3px;';
  const btn = 'background:#1e293b;color:#e2e8f0;border:1px solid #334155;border-radius:6px;padding:3px 8px;cursor:pointer;font-size:11px;';

  const TYPES = {
    transaction: { color: '#f59e0b', shape: 'diamond', label: 'Transactions' },
    api: { color: '#22d3ee', shape: 'hexagon', label: 'APIs' },
    job: { color: '#a78bfa', shape: 'round-rectangle', label: 'Batch jobs' },
    program: { color: '#38bdf8', shape: 'ellipse', label: 'Programs' },
    utility: { color: '#64748b', shape: 'octagon', label: 'Utilities' },
    screen: { color: '#f472b6', shape: 'rectangle', label: 'Screens' },
    table: { color: '#34d399', shape: 'barrel', label: 'Tables / segments' },
    dataset: { color: '#10b981', shape: 'barrel', label: 'Datasets' },
    copybook: { color: '#94a3b8', shape: 'tag', label: 'Copybooks' },
  };
  const EDGES = {
    starts: '#f59e0b', invokes: '#22d3ee', runs: '#a78bfa', triggers: '#c084fc', calls: '#38bdf8',
    reads: '#34d399', writes: '#f87171', sends: '#f472b6', 'backed-by': '#10b981', copies: '#475569',
  };
  const STATUS = {
    parity: ['#22c55e', 'Converted, parity ≥ 90%'], converted: ['#84cc16', 'Converted (parity evaluated)'], parsed: ['#38bdf8', 'Parsed (AST available)'],
    pending: ['#f59e0b', 'Not parsed yet'], missing: ['#ef4444', 'Referenced, not in source'], other: ['#475569', 'n/a'],
  };
  const PALETTE = ['#60a5fa', '#f472b6', '#34d399', '#fbbf24', '#a78bfa', '#f87171', '#22d3ee', '#fb923c', '#4ade80', '#e879f9', '#facc15', '#2dd4bf', '#818cf8', '#fda4af'];
  const TECHS = ['CICS', 'DB2', 'VSAM', 'IMS', 'MQ', 'Files'];
  const KINDS = ['online', 'api', 'batch', 'subroutine'];
  const WAVE_TITLE = (w) => Number(w) === 0 ? 'Wave 0 · Platform & shared services' : `Wave ${w} · after the waves it calls into`;
  const TIER = { 'low-risk': '#22c55e', moderate: '#f59e0b', core: '#ef4444', platform: '#94a3b8' };

  // Deterministic pseudo-random seed position so layouts are stable between reloads.
  function seed(id, spread) {
    let h = 2166136261;
    for (let i = 0; i < id.length; i++) { h ^= id.charCodeAt(i); h = Math.imul(h, 16777619); }
    const a = ((h >>> 0) % 3600) / 3600 * Math.PI * 2, r = (((h >>> 12) % 1000) / 1000) * spread;
    return { x: Math.cos(a) * r, y: Math.sin(a) * r };
  }

  function statusOf(n) {
    if (!n || n.type !== 'program') return 'other';
    if ((n.flags || []).includes('not-in-source')) return 'missing';
    const s = n.status || {};
    if (s.conversions && Object.keys(s.conversions).length) return (s.bestParity || 0) >= 0.9 ? 'parity' : 'converted';
    return s.rekt ? 'parsed' : 'pending';
  }

  class EstateMissionControlView {
    constructor(rootId) {
      this.root = document.getElementById(rootId);
      this.data = null; this.cy = null; this.selected = null;
      this.f = {
        estate: 'all', group: 'domain', mode: 'explore', search: '',
        types: new Set(Object.keys(TYPES).filter(t => t !== 'copybook' && t !== 'dataset')), kinds: new Set(KINDS), techs: new Set(), status: 'all', flagged: false,
      };
    }

    async loadAndRender(force) {
      if (this.data && !force) { this.cy?.resize(); setTimeout(() => this.cy?.resize(), 350); return; }
      this.root.innerHTML = `<div style="padding:20px;${muted}">Loading estate graph…</div>`;
      if (!window.cytoscape) {
        this.root.innerHTML = `<div style="padding:20px;color:#ef4444">Graph library not loaded (lib/cytoscape/cytoscape.min.js).</div>`;
        return;
      }
      try {
        const r = await fetch('/api/estate/mission');
        if (r.status === 404) { this.renderEmpty(); return; }
        if (!r.ok) throw new Error(`HTTP ${r.status}`);
        this.data = await r.json();
        this.index();
        this.renderShell();
        this.draw();
      } catch (e) {
        this.root.innerHTML = `<div style="padding:20px;color:#ef4444">${esc(e.message)}</div>`;
      }
    }

    renderEmpty() {
      this.root.innerHTML = `<div style="padding:24px;${muted}">No estate graph yet. <button id="emc-build" style="${btn}margin-left:8px">Build it now</button></div>`;
      this.root.querySelector('#emc-build').onclick = () => this.rebuild();
    }

    index() {
      const d = this.data;
      this.nodes = new Map(d.nodes.map(n => [n.id, n]));
      this.clusters = new Map((d.clusters || []).map(c => [c.id, c]));
      this.domainColor = {};
      (d.domains || []).forEach((x, i) => { this.domainColor[x.name] = PALETTE[i % PALETTE.length]; });
      this.clusterColor = {};
      (d.clusters || []).filter(c => c.id !== 'shared').forEach((c, i) => { this.clusterColor[c.id] = PALETTE[i % PALETTE.length]; });
      this.clusterColor.shared = '#94a3b8';
      this.adj = new Map();
      for (const e of d.edges) {
        for (const [a, b] of [[e.source, e.target], [e.target, e.source]]) {
          if (!this.adj.has(a)) this.adj.set(a, []);
          this.adj.get(a).push({ other: b, e });
        }
      }
      // Non-program nodes inherit a cluster when all their program neighbours sit in one cluster.
      this.nodeCluster = {};
      for (const n of d.nodes) {
        if (n.type === 'program') { this.nodeCluster[n.id] = n.cluster; continue; }
        const cs = new Set((this.adj.get(n.id) || []).map(x => this.nodes.get(x.other)).filter(o => o?.type === 'program' && o.cluster).map(o => o.cluster));
        this.nodeCluster[n.id] = cs.size === 1 ? [...cs][0] : null;
      }
    }

    renderShell() {
      const K = this.data.kpis || {}, d = this.data, f = this.f;
      const kpi = (t, v, sub, c) => `<div style="${card}padding:8px 12px;min-width:112px"><div style="${muted}font-size:10px;text-transform:uppercase;letter-spacing:.4px">${t}</div><div style="font-size:19px;font-weight:700;color:${c || '#e2e8f0'}">${v}</div>${sub ? `<div style="${muted}font-size:10px">${sub}</div>` : ''}</div>`;
      const parsedPct = K.programs ? K.rektParsed / K.programs : 0;
      const estates = (d.estates || []).filter(e => e.programs && e.id !== '(root)');
      const chip = (group, val, on, label, color) => `<button data-chip="${group}" data-val="${esc(val)}" style="border:1px solid ${on ? (color || '#38bdf8') : '#334155'};background:${on ? (color || '#38bdf8') + '22' : 'transparent'};color:${on ? '#e2e8f0' : '#64748b'};border-radius:999px;padding:2px 9px;font-size:11px;cursor:pointer">${label}</button>`;
      this.root.innerHTML = `
        <div style="display:flex;flex-direction:column;height:100%;color:#e2e8f0;font-family:Inter,system-ui,sans-serif">
          <div style="display:flex;gap:8px;padding:10px 12px 6px;flex-wrap:wrap;align-items:stretch">
            <div style="${card}padding:8px 12px;min-width:170px;border-color:#0e7490"><div style="font-size:15px;font-weight:700">🛰 Estate Mission Control</div><div style="${muted}font-size:10px">${esc(estates.map(e => e.id).join(' · '))}<br>built ${esc(String(d.generatedAtUtc || '').replace('T', ' ').slice(0, 16))} UTC</div></div>
            ${kpi('Programs', fmt(K.programs), `${fmt(K.lines)} lines`)}
            ${kpi('Entry points', fmt((K.transactions || 0) + (K.apis || 0) + (K.jobs || 0)), `${K.transactions} tx · ${K.apis || 0} API · ${K.jobs} jobs`, '#f59e0b')}
            ${kpi('Online / batch', `${K.online}/${K.batch}`, `${K.subroutines} subroutines · ${K.apiPrograms || 0} API-only`)}
            ${kpi('Data stores', fmt((K.tables || 0) + (K.datasets || 0)), `${K.tables} tables · ${K.datasets} datasets`, '#34d399')}
            ${kpi('Parsed', pct(parsedPct), `${K.rektParsed}/${K.programs} programs`, parsedPct > 0.8 ? '#22c55e' : '#f59e0b')}
            ${kpi('Converted', fmt(K.converted), 'programs with AI output', '#4ade80')}
            ${kpi('Carve-out', fmt(K.clusters), `clusters · ${K.hubs || 0} shared hub(s)`, '#a78bfa')}
            ${kpi('Attention', fmt((K.unreferenced || 0) + (K.unreachable || 0)), `${K.unreferenced} unreferenced · ${K.unreachable} unreachable`, '#f87171')}
          </div>
          <div style="display:flex;gap:10px;padding:4px 12px 8px;flex-wrap:wrap;align-items:center;font-size:12px;border-bottom:1px solid #1f2937">
            <select id="emc-estate" style="${sel}">
              <option value="all">All estates</option>${estates.map(e => `<option value="${esc(e.id)}" ${f.estate === e.id ? 'selected' : ''}>${esc(e.id)} (${e.programs})</option>`).join('')}
            </select>
            <span style="${muted}">Group</span>
            <select id="emc-group" style="${sel}">
              ${[['domain', 'Business function'], ['cluster', 'Carve-out cluster'], ['estate', 'Estate'], ['none', 'None']].map(([v, t]) => `<option value="${v}" ${f.group === v ? 'selected' : ''}>${t}</option>`).join('')}
            </select>
            <span style="${muted}">Mode</span>
            <span>${chip('mode', 'explore', f.mode === 'explore', '🔭 Explore')} ${chip('mode', 'carve', f.mode === 'carve', '✂️ Carve-out plan', '#a78bfa')}</span>
            <input id="emc-search" placeholder="Search program, transaction, table…" value="${esc(f.search)}" style="flex:1;min-width:180px;background:#0f172a;color:#e2e8f0;border:1px solid #334155;border-radius:6px;padding:4px 8px">
            <button id="emc-fit" title="Fit to screen" style="${btn}">⤢ Fit</button>
            <button id="emc-relayout" title="Re-run layout" style="${btn}">↻ Layout</button>
            <button id="emc-rebuild" title="Re-scan source and rebuild the graph" style="background:#0e7490;color:#fff;border:0;border-radius:6px;padding:4px 10px;cursor:pointer;font-size:11px">⟳ Rebuild graph</button>
          </div>
          <div style="display:flex;gap:6px;padding:6px 12px;flex-wrap:wrap;align-items:center;border-bottom:1px solid #1f2937">
            <span style="${muted}font-size:11px">Nodes</span>${Object.entries(TYPES).map(([t, v]) => chip('type', t, f.types.has(t), `${v.label} ${this.count(t)}`, v.color)).join('')}
            <span style="${muted}font-size:11px;margin-left:8px">Kind</span>${KINDS.map(k => chip('kind', k, f.kinds.has(k), k)).join('')}
            <span style="${muted}font-size:11px;margin-left:8px">Tech</span>${TECHS.map(t => chip('tech', t, f.techs.has(t), t, '#fbbf24')).join('')}
            <span style="${muted}font-size:11px;margin-left:8px">Status</span>
            <select id="emc-status" style="${sel}font-size:11px">
              ${[['all', 'All'], ...Object.entries(STATUS).filter(([k]) => k !== 'other').map(([k, v]) => [k, v[1]])].map(([v, t]) => `<option value="${v}" ${f.status === v ? 'selected' : ''}>${esc(t)}</option>`).join('')}
            </select>
            ${chip('flagged', '1', f.flagged, '⚠ Unreferenced / unreachable', '#f87171')}
          </div>
          <div style="flex:1;display:flex;min-height:0">
            <div style="flex:1;position:relative;min-width:0">
              <div id="emc-graph" style="position:absolute;inset:0;background:radial-gradient(circle at 50% 40%,#0f1b2d 0%,#060a14 75%)"></div>
              <div id="emc-legend" style="position:absolute;left:10px;bottom:10px;${card}padding:8px 10px;font-size:10px;opacity:.93;max-width:600px"></div>
              <div id="emc-count" style="position:absolute;right:10px;top:8px;${muted}font-size:11px"></div>
            </div>
            <div id="emc-side" style="width:380px;border-left:1px solid #1f2937;overflow:auto;padding:12px;background:#0a0e1a"></div>
          </div>
        </div>`;
      this.bindControls();
      this.renderLegend();
    }

    count(t) { return this.data.nodes.filter(n => n.type === t && (this.f.estate === 'all' || n.estate === this.f.estate || !n.estate)).length; }

    bindControls() {
      const $ = (s) => this.root.querySelector(s);
      $('#emc-estate').onchange = (e) => { this.f.estate = e.target.value; this.selected = null; this.renderShell(); this.draw(); };
      $('#emc-group').onchange = (e) => { this.f.group = e.target.value; this.draw(); };
      $('#emc-status').onchange = (e) => { this.f.status = e.target.value; this.draw(); };
      let t;
      $('#emc-search').oninput = (e) => { clearTimeout(t); t = setTimeout(() => { this.f.search = e.target.value.trim().toUpperCase(); this.highlightSearch(); }, 250); };
      $('#emc-fit').onclick = () => this.cy?.fit(undefined, 30);
      $('#emc-relayout').onclick = () => this.layout();
      $('#emc-rebuild').onclick = () => this.rebuild();
      this.root.querySelectorAll('[data-chip]').forEach(el => el.onclick = () => {
        const g = el.dataset.chip, v = el.dataset.val, f = this.f;
        if (g === 'mode') { f.mode = v; f.group = v === 'carve' ? 'cluster' : 'domain'; this.selected = null; }
        else if (g === 'flagged') f.flagged = !f.flagged;
        else {
          const set = g === 'type' ? f.types : g === 'kind' ? f.kinds : f.techs;
          set.has(v) ? set.delete(v) : set.add(v);
        }
        this.renderShell(); this.draw();
      });
    }

    programVisible(n) {
      const f = this.f;
      const kind = (n.kind || '').startsWith('subroutine') ? 'subroutine' : n.kind;
      if (kind && !f.kinds.has(kind)) return false;
      if (f.techs.size && !(n.tech || []).some(t => f.techs.has(t))) return false;
      if (f.status !== 'all' && statusOf(n) !== f.status) return false;
      if (f.flagged && !((n.flags || []).includes('unreferenced') || n.reachable === false)) return false;
      return true;
    }

    visible(n) {
      const f = this.f;
      if (!f.types.has(n.type)) return false;
      if (f.estate !== 'all' && n.estate && n.estate !== f.estate) return false;
      return n.type !== 'program' || this.programVisible(n);
    }

    groupOf(n) {
      const g = this.f.group;
      if (g === 'none') return null;
      if (g === 'estate') return n.estate ? `grp:estate:${n.estate}` : null;
      if (g === 'cluster') { const c = this.nodeCluster[n.id]; return c ? `grp:cluster:${c}` : null; }
      return n.domain ? `grp:domain:${n.domain}` : null;
    }

    buildElements() {
      const els = [], shown = new Set(), groups = new Map();
      for (const n of this.data.nodes) if (this.visible(n)) shown.add(n.id);
      // Non-program nodes only stay when they touch a visible program (keeps filters meaningful, hides orphans of other estates).
      for (const id of [...shown]) {
        const n = this.nodes.get(id);
        if (n.type === 'program') continue;
        const touches = (this.adj.get(id) || []).some(x => shown.has(x.other) && this.nodes.get(x.other)?.type === 'program');
        const hasProgramNeighbour = (this.adj.get(id) || []).some(x => this.nodes.get(x.other)?.type === 'program');
        if (!touches && (hasProgramNeighbour || this.f.estate !== 'all')) shown.delete(id);
      }
      for (const id of shown) {
        const n = this.nodes.get(id), T = TYPES[n.type] || TYPES.program;
        const lines = n.metrics?.lines || 0;
        const size = n.type === 'program' ? Math.max(18, Math.min(70, 10 + Math.sqrt(lines) * 1.1)) : n.type === 'copybook' ? 10 : n.type === 'dataset' ? 14 : 22;
        const parent = this.groupOf(n);
        if (parent) groups.set(parent, (groups.get(parent) || 0) + 1);
        const flags = n.flags || [];
        const g0 = seed(parent || n.type, 900), p0 = seed(id, 160);
        els.push({
          group: 'nodes',
          position: { x: g0.x + p0.x, y: g0.y + p0.y },
          data: {
            id, label: n.type === 'dataset' ? String(n.label).split('.').slice(-2).join('.') : n.label, type: n.type, parent: parent || undefined,
            color: T.color, shape: T.shape, size, ring: STATUS[statusOf(n)][0], ringW: n.type === 'program' ? 4 : 1.5,
            hub: n.hub ? 1 : 0, warn: flags.includes('unreferenced') || n.reachable === false ? 1 : 0,
          },
        });
      }
      for (const [gid] of groups) {
        const [, kind, ...rest] = gid.split(':');
        const key = rest.join(':');
        let label = key, color = '#334155';
        if (kind === 'domain') color = this.domainColor[key] || '#334155';
        if (kind === 'estate') color = '#0e7490';
        if (kind === 'cluster') { const c = this.clusters.get(key); label = c ? `${c.id === 'shared' ? '' : `W${c.wave} · `}${c.label}` : key; color = this.clusterColor[key] || '#334155'; }
        els.push({ group: 'nodes', data: { id: gid, label, isGroup: 1, color } });
      }
      for (const e of this.data.edges) {
        if (!shown.has(e.source) || !shown.has(e.target)) continue;
        els.push({ group: 'edges', data: { id: `${e.source}→${e.target}→${e.type}`, source: e.source, target: e.target, type: e.type, color: EDGES[e.type] || '#475569', w: Math.min(5, 0.8 + Math.log2(1 + (e.count || 1))) } });
      }
      return els;
    }

    draw() {
      const container = this.root.querySelector('#emc-graph');
      if (!container) return;
      if (this.cy) this.cy.destroy();
      const els = this.buildElements();
      const nNodes = els.filter(e => e.group === 'nodes' && !e.data.isGroup).length;
      this.root.querySelector('#emc-count').textContent = `${nNodes} nodes · ${els.filter(e => e.group === 'edges').length} edges`;
      this.cy = window.cytoscape({
        container, elements: els, wheelSensitivity: 0.25, minZoom: 0.08, maxZoom: 4,
        style: [
          { selector: 'node', style: {
            'background-color': 'data(color)', 'background-opacity': 0.9, 'border-color': 'data(ring)', 'border-width': 'data(ringW)', shape: 'data(shape)',
            width: 'data(size)', height: 'data(size)', label: 'data(label)', color: '#cbd5e1', 'font-size': 9, 'text-valign': 'bottom', 'text-margin-y': 3,
            'min-zoomed-font-size': 7, 'text-outline-color': '#060a14', 'text-outline-width': 2,
          } },
          { selector: 'node[type = "program"]', style: { 'background-color': '#0b1a2e', 'background-opacity': 1 } },
          { selector: 'node[hub = 1]', style: { 'border-style': 'double', 'border-width': 8 } },
          { selector: 'node[warn = 1]', style: { 'border-style': 'dashed' } },
          { selector: 'node[isGroup = 1]', style: {
            shape: 'round-rectangle', 'background-color': 'data(color)', 'background-opacity': 0.06, 'border-color': 'data(color)', 'border-width': 1.5, 'border-opacity': 0.7,
            label: 'data(label)', 'text-valign': 'top', 'text-halign': 'center', 'font-size': 13, 'font-weight': 700, color: 'data(color)', 'text-margin-y': -4, padding: 18, 'min-zoomed-font-size': 4,
          } },
          { selector: 'edge', style: { width: 'data(w)', 'line-color': 'data(color)', 'target-arrow-color': 'data(color)', 'target-arrow-shape': 'triangle', 'arrow-scale': 0.7, 'curve-style': 'bezier', opacity: 0.45 } },
          { selector: 'edge[type = "copies"]', style: { 'line-style': 'dotted', opacity: 0.25 } },
          { selector: '.dim', style: { opacity: 0.08 } },
          { selector: 'edge.hl', style: { opacity: 1, width: 3, 'z-index': 9 } },
          { selector: 'node.hl', style: { 'border-color': '#fde047', 'border-width': 5 } },
          { selector: 'node.sel', style: { 'border-color': '#ffffff', 'border-width': 6, 'overlay-color': '#fde047', 'overlay-opacity': 0.15, 'overlay-padding': 6 } },
          { selector: 'node.shared', style: { 'border-color': '#f87171', 'border-width': 6 } },
        ],
      });
      this.cy.on('tap', 'node', (ev) => {
        const id = ev.target.id();
        if (id.startsWith('grp:')) { const [, kind, ...rest] = id.split(':'); if (kind === 'cluster') this.selectCluster(rest.join(':')); else this.focusGroup(ev.target); return; }
        this.select(id);
      });
      this.cy.on('tap', (ev) => { if (ev.target === this.cy) this.clearSelection(); });
      this.cy.on('mouseover', 'node', () => { container.style.cursor = 'pointer'; });
      this.cy.on('mouseout', 'node', () => { container.style.cursor = ''; });
      this.layout();
      if (this.selected && this.nodes.has(this.selected)) this.select(this.selected);
      else if (this.f.mode === 'carve') this.renderWavePlan();
      else this.renderOverview();
    }

    layout() {
      if (!this.cy) return;
      if (window.cytoscapeFcose && !window.__emcFcose) { window.cytoscape.use(window.cytoscapeFcose); window.__emcFcose = true; }
      const opts = window.__emcFcose
        ? { name: 'fcose', quality: 'proof', randomize: true, animate: false, fit: true, padding: 30, nodeSeparation: 60,
            idealEdgeLength: () => 55, nodeRepulsion: () => 6000, edgeElasticity: () => 0.45, nestingFactor: 0.1, gravity: 0.3,
            gravityCompound: 1.2, gravityRangeCompound: 1.5, tile: true, tilingPaddingVertical: 12, tilingPaddingHorizontal: 12, packComponents: true, numIter: 2500 }
        : { name: 'cose', animate: false, randomize: true, fit: true, padding: 30, componentSpacing: 40, nodeRepulsion: () => 4000, gravity: 1 };
      this.cy.layout(opts).run();
    }

    highlightSearch() {
      if (!this.cy) return;
      this.cy.elements().removeClass('dim hl');
      const q = this.f.search;
      if (!q) return;
      const hits = this.cy.nodes().filter(n => !n.data('isGroup') && String(n.data('label')).toUpperCase().includes(q));
      if (!hits.length) return;
      this.cy.elements().not(hits).not(hits.ancestors()).addClass('dim');
      hits.addClass('hl');
      this.cy.animate({ fit: { eles: hits, padding: 120 }, duration: 400 });
      if (hits.length === 1) this.select(hits[0].id());
    }

    focusGroup(g) { this.cy.animate({ fit: { eles: g, padding: 40 }, duration: 400 }); }

    clearSelection() {
      this.selected = null;
      this.cy?.elements().removeClass('dim hl sel shared');
      if (this.f.mode === 'carve') this.renderWavePlan(); else this.renderOverview();
    }

    select(id) {
      const n = this.nodes.get(id);
      if (!n) return;
      this.selected = id;
      if (this.cy) {
        this.cy.elements().removeClass('dim hl sel shared');
        const el = this.cy.getElementById(id);
        if (el.length) {
          const hood = el.closedNeighborhood();
          this.cy.elements().not(hood).not(hood.ancestors()).addClass('dim');
          hood.edges().addClass('hl');
          el.addClass('sel');
        }
      }
      this.renderNode(n);
    }

    selectCluster(cid) {
      const c = this.clusters.get(cid);
      if (!c) return;
      if (this.cy) {
        this.cy.elements().removeClass('dim hl sel shared');
        const members = this.cy.collection(c.members.map(m => this.cy.getElementById(m)).filter(x => x.length));
        const hood = members.closedNeighborhood();
        this.cy.elements().not(hood).not(hood.ancestors()).addClass('dim');
        members.edgesWith(members).addClass('hl');
        (c.sharedData || []).forEach(s => this.cy.getElementById(s.id).addClass('shared'));
        if (members.length) this.cy.animate({ fit: { eles: hood, padding: 60 }, duration: 400 });
      }
      this.renderCluster(c);
    }

    // ---------------------------------------------------------------- side panels

    side(html) {
      const s = this.root.querySelector('#emc-side');
      s.innerHTML = html;
      s.querySelectorAll('[data-src]').forEach(el => el.onclick = (ev) => { ev.preventDefault(); if (window.__studioShowSource) window.__studioShowSource(el.dataset.src, Number(el.dataset.line) || 1); else navigator.clipboard?.writeText(`${el.dataset.src}:${el.dataset.line || 1}`); });
      s.querySelectorAll('[data-node]').forEach(el => el.onclick = (ev) => { ev.preventDefault(); this.select(el.dataset.node); });
      s.querySelectorAll('[data-cluster]').forEach(el => el.onclick = (ev) => { ev.preventDefault(); this.selectCluster(el.dataset.cluster); });
      s.querySelectorAll('[data-explore]').forEach(el => el.onclick = async (ev) => {
        ev.preventDefault();
        window.switchDashboard?.('explorer');
        const v = window.programExplorerView;
        if (!v) return;
        await v.loadAndRender();
        const file = el.dataset.explore;
        const hit = (v.programs || []).find(p => p.relativePath === file) ? file : v.resolveIdentity?.(file.split('/').pop());
        if (hit) await v.select(hit);
      });
      s.querySelectorAll('[data-stage]').forEach(el => el.onclick = (ev) => { ev.preventDefault(); this.stage(el.dataset.stage, false); });
      s.querySelectorAll('[data-stage-run]').forEach(el => el.onclick = (ev) => { ev.preventDefault(); this.stage(el.dataset.stageRun, true); });
      const back = s.querySelector('#emc-back');
      if (back) back.onclick = (ev) => { ev.preventDefault(); this.clearSelection(); };
    }

    renderLegend() {
      const lg = this.root.querySelector('#emc-legend');
      if (!lg) return;
      const ring = (c, t) => `<span style="display:inline-flex;align-items:center;gap:4px;margin-right:9px"><span style="width:9px;height:9px;border-radius:50%;border:2px solid ${c}"></span>${esc(t)}</span>`;
      lg.innerHTML = `<div style="margin-bottom:3px"><b style="color:#cbd5e1">Program ring</b> ${Object.entries(STATUS).filter(([k]) => k !== 'other').map(([, [c, t]]) => ring(c, t)).join('')}</div>
        <div><b style="color:#cbd5e1">Edges</b> ${Object.entries(EDGES).map(([t, c]) => `<span style="margin-right:7px"><span style="display:inline-block;width:12px;height:2px;background:${c};vertical-align:middle"></span> ${t}</span>`).join('')}</div>
        <div style="color:#64748b;margin-top:3px">size = lines of code · double ring = shared hub · dashed = unreferenced/unreachable · click a group title to zoom, a cluster title for its carve-out card</div>`;
    }

    sourcesCard() {
      const P = window.StudioProvenance;
      if (!P) {
        const row = (what) => `<div style="font-size:11px;margin:4px 0;line-height:1.5;color:#cbd5e1">• ${esc(what)}</div>`;
        return `<div style="${card}margin-bottom:10px"><div style="${muted}font-size:11px;margin-bottom:6px">WHERE THIS DATA COMES FROM</div>
          ${row('Nodes, edges, clusters, hubs and waves: deterministic scan of COBOL, copybooks, JCL, BMS, CSD, CICS yaml, DDL and API yaml.')}
          ${row('Status ring: Cobol-REKT parse fidelity per program.')}
          ${row('Converted: programs whose latest run has an evaluated conversion-parity report.')}
        </div>`;
      }
      const eng = Object.keys(P.PROV).find(k => P.PROV[k][0] === '#f59e0b');
      const row = (id, what) => id ? `<div style="font-size:11px;margin:4px 0;line-height:1.5">${P.prov(id)} <span style="color:#cbd5e1">${esc(what)}</span></div>` : '';
      return `<div style="${card}margin-bottom:10px"><div style="${muted}font-size:11px;margin-bottom:6px">WHERE THIS DATA COMES FROM</div>
        ${row('portal-scan', 'Nodes, edges, clusters, hubs and waves: pattern scan of COBOL, copybooks, JCL, BMS, CSD, DDL and API yaml (no AST).')}
        ${row('rekt', 'Status ring (parsed / partial / failed) and program AST in the side panel.')}
        ${row(eng, 'AST Compare and Findings side panels (second engine outline).')}
        ${row('ai', 'Conversion status per target (C#, Java, Spring) from the agentic AI loop.')}
        ${row('verify', 'Build and parity results that colour conversion status.')}
      </div>`;
    }

    renderOverview() {
      const d = this.data;
      const doms = (d.domains || []).filter(x => x.name);
      const max = Math.max(1, ...doms.map(x => x.lines || 0));
      const ATTN = ['unreferenced', 'not-in-source'];
      const why = (n) => [...(n.flags || []).filter(x => ATTN.includes(x)), n.reachable === false ? 'unreachable' : ''].filter(Boolean);
      const flagged = d.nodes.filter(n => n.type === 'program' && why(n).length && (this.f.estate === 'all' || n.estate === this.f.estate));
      const dyn = d.nodes.filter(n => (n.flags || []).includes('dynamic-call') && (this.f.estate === 'all' || n.estate === this.f.estate)).length;
      this.side(`
        <div style="font-size:13px;font-weight:700;margin-bottom:6px">Estate overview</div>
        <div style="${muted}margin-bottom:10px">Every node and edge comes from the deterministic builder with file:line evidence. Click anything in the graph to inspect it.</div>
        ${d.warning ? `<div style="${card}border-color:#f59e0b;color:#fbbf24;font-size:12px;margin-bottom:10px">${esc(d.warning)}</div>` : ''}
        ${this.sourcesCard()}
        <div style="${card}margin-bottom:10px"><div style="${muted}font-size:11px;margin-bottom:6px">BUSINESS FUNCTIONS (by lines of code)</div>
          ${doms.map(x => `<div style="display:flex;align-items:center;gap:6px;margin:3px 0;font-size:12px"><span style="width:10px;height:10px;border-radius:2px;background:${this.domainColor[x.name]}"></span><span style="flex:1">${esc(x.name)}</span><span style="${muted}">${x.programs} pgm</span><div style="width:80px;height:6px;background:#1f2937;border-radius:3px"><div style="width:${(100 * (x.lines || 0) / max).toFixed(0)}%;height:100%;background:${this.domainColor[x.name]};border-radius:3px"></div></div></div>`).join('')}
        </div>
        <div style="${card}margin-bottom:10px"><div style="${muted}font-size:11px;margin-bottom:6px">INPUTS SCANNED</div>
          <div style="font-size:12px;line-height:1.7">${Object.entries(d.inputs || {}).map(([k, v]) => `<span style="margin-right:10px">${esc(k)} <b>${fmt(v)}</b></span>`).join('')}</div>
        </div>
        <div style="${card}"><div style="${muted}font-size:11px;margin-bottom:6px">NEEDS ATTENTION (${flagged.length})</div>
          ${flagged.slice(0, 40).map(n => `<div style="font-size:12px;margin:2px 0"><a href="#" data-node="${esc(n.id)}" style="color:#60a5fa">${esc(n.label)}</a> <span style="${muted}font-size:10px">${esc(why(n).join(', '))}</span></div>`).join('') || `<div style="${muted}">Nothing flagged.</div>`}
          ${dyn ? `<div style="${muted}font-size:10px;margin-top:6px">${dyn} program(s) also use dynamic CALL/XCTL targets; resolved where the literal is visible.</div>` : ''}
        </div>
        <div style="${muted}font-size:10px;margin-top:10px">Generator: ${esc(d.generator || '')} · schema v${esc(d.schemaVersion)} · ${fmt(d.diagnosticCount || 0)} scanner diagnostic(s)</div>`);
    }

    renderNode(n) {
      const st = statusOf(n), m = n.metrics || {}, s = n.status || {};
      const ev = (e) => (e.evidence || []).slice(0, 3).map(x => `<a href="#" data-src="${esc(x.file)}" data-line="${x.line || 1}" style="color:#64748b;font-size:10px;margin-left:4px">${esc(String(x.file).split('/').pop())}${x.line ? ':' + x.line : ''}</a>`).join('');
      const rel = {};
      for (const { other, e } of this.adj.get(n.id) || []) {
        const key = `${e.source === n.id ? '→' : '←'} ${e.type}`;
        (rel[key] ||= []).push({ other, e });
      }
      const relHtml = Object.entries(rel).sort().map(([k, list]) => `
        <div style="margin-top:8px"><div style="${muted}font-size:11px">${esc(k)} (${list.length})</div>
        ${list.slice(0, 30).map(({ other, e }) => { const o = this.nodes.get(other); return `<div style="font-size:12px;margin:2px 0 2px 8px"><span style="color:${TYPES[o?.type]?.color || '#94a3b8'}">●</span> <a href="#" data-node="${esc(other)}" style="color:#e2e8f0">${esc(o?.label || other)}</a>${e.count > 1 ? ` <span style="${muted}font-size:10px">×${e.count}</span>` : ''}${e.via ? ` <span style="${muted}font-size:10px">${esc(e.via)}</span>` : ''}${ev(e)}</div>`; }).join('')}
        ${list.length > 30 ? `<div style="${muted}font-size:10px;margin-left:8px">…and ${list.length - 30} more</div>` : ''}</div>`).join('');
      const conv = Object.entries(s.conversions || {}).map(([t, c]) => `<div style="font-size:12px;margin:2px 0">${esc(t)} parity <b style="color:${(c.parity || 0) >= 0.9 ? '#22c55e' : '#f59e0b'}">${pct(c.parity)}</b>${c.threshold != null ? ` <span style="${muted}">threshold ${pct(c.threshold)}</span>` : ''}</div>`).join('');
      const c = n.cluster ? this.clusters.get(n.cluster) : null;
      const metric = (k, v) => `<div style="${card}padding:6px 8px;text-align:center"><div style="font-size:15px;font-weight:700">${fmt(v ?? 0)}</div><div style="${muted}font-size:10px">${k}</div></div>`;
      const tag = (t, bg, fg, title) => `<span ${title ? `title="${esc(title)}"` : ''} style="font-size:10px;padding:1px 6px;border-radius:4px;background:${bg};color:${fg || '#cbd5e1'}">${esc(t)}</span>`;
      this.side(`
        <a href="#" id="emc-back" style="${muted}font-size:11px">← ${this.f.mode === 'carve' ? 'wave plan' : 'overview'}</a>
        <div style="display:flex;align-items:center;gap:8px;margin:6px 0">
          <span style="width:12px;height:12px;border-radius:50%;background:${TYPES[n.type]?.color};border:3px solid ${STATUS[st][0]}"></span>
          <span style="font-size:16px;font-weight:700;word-break:break-all">${esc(n.label)}</span><span style="${muted}">${esc(n.type)}${n.kind ? ` · ${esc(n.kind)}` : ''}</span>
        </div>
        ${n.description ? `<div style="font-size:12px;color:#cbd5e1;margin-bottom:6px">${esc(n.description)}</div>` : ''}
        <div style="display:flex;gap:4px;flex-wrap:wrap;margin-bottom:8px">
          ${n.estate ? tag(n.estate, '#1e293b') : ''}
          ${n.domain ? tag(n.domain, (this.domainColor[n.domain] || '#334155') + '33', this.domainColor[n.domain], n.domainReason) : ''}
          ${(n.tech || []).map(t => tag(t, '#fbbf2422', '#fbbf24')).join('')}
          ${(n.flags || []).filter(t => t !== 'hub').map(t => tag(t, '#ef444422', '#f87171')).join('')}
          ${n.hub ? tag(n.hub, '#64748b33') : ''}
          ${n.reachable === false ? tag('unreachable from any entry point', '#ef444422', '#f87171') : ''}
        </div>
        ${n.type === 'program' ? `
          <div style="font-size:12px;margin-bottom:6px"><span style="color:${STATUS[st][0]}">●</span> ${esc(STATUS[st][1])}${s.fidelity ? ` <span style="${muted}">· REKT ${esc(s.fidelity)}</span>` : ''}</div>
          <div style="display:grid;grid-template-columns:repeat(4,1fr);gap:4px;margin-bottom:8px">
            ${metric('lines', m.lines)}${metric('paragraphs', m.paragraphs)}${metric('complexity', m.complexity)}${metric('GO TO', m.goto)}
            ${metric('EXEC CICS', m.execCics)}${metric('EXEC SQL', m.execSql)}${metric('EXEC DLI', m.execDli)}${metric('PERFORM', m.perform)}
          </div>
          ${conv ? `<div style="${card}padding:8px;margin-bottom:8px"><div style="${muted}font-size:11px;margin-bottom:4px">CONVERSION PARITY (latest run)</div>${conv}</div>` : ''}
          ${c ? `<div style="font-size:12px;margin-bottom:8px">Carve-out: <a href="#" data-cluster="${esc(c.id)}" style="color:${this.clusterColor[c.id]}">${esc(c.label)}</a> <span style="${muted}">wave ${c.wave} · ${esc(c.tier || '')}</span></div>` : ''}
          <div style="display:flex;gap:6px;flex-wrap:wrap;margin-bottom:6px">
            ${n.file ? `<button data-src="${esc(n.file)}" data-line="1" title="${esc(n.file)}" style="${btn}">📄 ${esc(String(n.file).split('/').pop())}</button>` : ''}
            ${n.file ? `<button data-explore="${esc(n.file)}" style="${btn}">🔬 Program Explorer</button>` : ''}
          </div>` : n.file ? `<button data-src="${esc(n.file)}" data-line="1" style="${btn}margin-bottom:6px">📄 ${esc(String(n.file).split('/').pop())}</button>` : ''}
        ${n.dsn ? `<div style="${muted}font-size:11px">DSN ${esc(n.dsn)}</div>` : ''}${n.store ? `<div style="${muted}font-size:11px">${esc(n.store)}</div>` : ''}
        <div style="border-top:1px solid #1f2937;margin-top:8px;padding-top:4px">${relHtml || `<div style="${muted}">No relationships.</div>`}</div>`);
    }

    renderCluster(c) {
      const name = (id) => this.nodes.get(id)?.label || String(id).split(':').pop();
      this.side(`
        <a href="#" id="emc-back" style="${muted}font-size:11px">← ${this.f.mode === 'carve' ? 'wave plan' : 'overview'}</a>
        <div style="display:flex;align-items:center;gap:8px;margin:6px 0">
          <span style="width:12px;height:12px;border-radius:3px;background:${this.clusterColor[c.id]}"></span>
          <span style="font-size:15px;font-weight:700">${esc(c.label)}</span>
        </div>
        <div style="font-size:12px;color:#cbd5e1;margin-bottom:8px">${esc(c.rationale || '')}</div>
        ${c.scoreBreakdown ? `<div style="${muted}font-size:10px;margin-bottom:8px">score = ${Object.entries(c.scoreBreakdown).map(([k, v]) => `${esc(k)} ${pct(v)}`).join(' · ')}</div>` : ''}
        <div style="display:grid;grid-template-columns:repeat(4,1fr);gap:4px;margin-bottom:8px">
          ${[['wave', c.wave], ['tier', c.tier || '—'], ['programs', c.programs], ['lines', c.lines], ['cohesion', pct(c.cohesion)], ['carve score', pct(c.carveScore)], ['entry points', (c.entryPoints || []).length], ['owned data', (c.ownedData || []).length], ['shared data', (c.sharedData || []).length], ['missing', (c.missing || []).length]]
            .map(([k, v]) => `<div style="${card}padding:6px;text-align:center"><div style="font-size:14px;font-weight:700">${typeof v === 'number' ? fmt(v) : esc(v)}</div><div style="${muted}font-size:10px">${k}</div></div>`).join('')}
        </div>
        ${c.sliceId ? `<div style="display:flex;gap:6px;margin-bottom:6px;align-items:center">
          <button data-stage="${esc(c.sliceId)}" style="${btn}padding:4px 10px">📦 Stage slice</button>
          <label style="${muted}font-size:11px"><input type="checkbox" id="emc-needs" checked> include what it calls</label>
          <button data-stage-run="${esc(c.sliceId)}" style="border-radius:6px;padding:4px 10px;cursor:pointer;font-size:11px;background:#7c3aed;color:#fff;border:0">🔁 Send to AI loop</button>
        </div><div id="emc-stage-out" style="font-size:11px;margin-bottom:8px"></div>` : ''}
        ${(c.missing || []).length ? `<div style="${muted}font-size:11px">NOT IN SOURCE (${c.missing.length}) · converts as stubs</div><div style="margin:4px 0 8px;font-size:11px;color:#f87171">${c.missing.map(esc).join(', ')}</div>` : ''}
        <div style="${muted}font-size:11px">MEMBERS</div>
        <div style="margin:4px 0 8px">${c.members.map(m => `<a href="#" data-node="${esc(m)}" style="display:inline-block;margin:2px;padding:1px 6px;border-radius:4px;border:1px solid ${STATUS[statusOf(this.nodes.get(m))][0]}88;color:#e2e8f0;font-size:11px;text-decoration:none">${esc(name(m))}</a>`).join('')}</div>
        ${(c.entryPoints || []).length ? `<div style="${muted}font-size:11px">ENTRY POINTS (${c.entryPoints.length}) · become the cluster's front door</div><div style="margin:4px 0 8px;font-size:11px">${c.entryPoints.map(x => { const n = this.nodes.get(x); return `<a href="#" data-node="${esc(x)}" style="color:${TYPES[n?.type]?.color || '#e2e8f0'};margin-right:6px">${esc(name(x))}</a>`; }).join('')}</div>` : ''}
        ${(c.ownedData || []).length ? `<div style="${muted}font-size:11px">OWNED DATA (moves with the cluster)</div><div style="margin:4px 0 8px;font-size:11px">${c.ownedData.map(x => `<a href="#" data-node="${esc(x.id || x)}" style="color:#34d399;margin-right:6px">${esc(name(x.id || x))}</a>`).join('')}</div>` : ''}
        ${(c.sharedData || []).length ? `<div style="${muted}font-size:11px">SHARED DATA (needs a data-access API or sync · red ring)</div><div style="margin:4px 0 8px;font-size:11px">${c.sharedData.map(x => `<a href="#" data-node="${esc(x.id)}" style="color:#f87171;margin-right:6px">${esc(name(x.id))}</a><span style="${muted}font-size:10px">with ${(x.clusters || []).filter(y => y !== c.id).map(y => esc(this.clusters.get(y)?.label || y)).join(', ')}</span><br>`).join('')}</div>` : ''}
        ${(c.apiSurface || []).length ? `<div style="${muted}font-size:11px">INBOUND CALLS → become service APIs (${c.apiSurface.length})</div><div style="margin:4px 0 8px;font-size:11px">${c.apiSurface.slice(0, 25).map(x => `${esc(name(x.from))} → <b>${esc(name(x.to))}</b>`).join('<br>')}</div>` : ''}
        ${(c.dependsOn || []).length ? `<div style="${muted}font-size:11px">OUTBOUND DEPENDENCIES (${c.dependsOn.length})</div><div style="margin:4px 0 8px;font-size:11px">${[...new Set(c.dependsOn.map(x => name(x.to)))].map(esc).join(', ')}</div>` : ''}
        ${(c.usedByClusters || []).length ? `<div style="${muted}font-size:11px">USED BY</div><div style="margin:4px 0 8px;font-size:11px">${c.usedByClusters.map(y => `<a href="#" data-cluster="${esc(y)}" style="color:${this.clusterColor[y]};margin-right:6px">${esc(this.clusters.get(y)?.label || y)}</a>`).join('')}</div>` : ''}`);
    }

    renderWavePlan() {
      const cl = (this.data.clusters || []).filter(c => this.f.estate === 'all' || c.members.some(m => this.nodes.get(m)?.estate === this.f.estate));
      const waves = {};
      cl.forEach(c => (waves[c.wave] ||= []).push(c));
      const totals = (list) => `${list.reduce((a, c) => a + c.programs, 0)} pgm · ${fmt(list.reduce((a, c) => a + (c.lines || 0), 0))} loc`;
      this.side(`
        <div style="font-size:13px;font-weight:700;margin-bottom:4px">✂️ Carve-out wave plan</div>
        <div style="${muted}margin-bottom:10px">Deterministic clustering on calls, shared data, screens, transactions and jobs; hub programs go to wave 0 as shared services. A cluster's wave comes after every cluster it calls into; within a wave, the highest carve score (cohesion, completeness, independence, size) goes first. The tier badge says how risky the carve is. Click a cluster to highlight it.</div>
        ${Object.keys(waves).sort((a, b) => a - b).map(w => `
          <div style="margin-bottom:12px"><div style="display:flex;justify-content:space-between;font-size:11px;color:#a78bfa;font-weight:700;margin-bottom:4px;text-transform:uppercase"><span>${esc(WAVE_TITLE(w))}</span><span style="${muted}font-size:10px;text-transform:none">${totals(waves[w])}</span></div>
          ${waves[w].map(c => `
            <div data-cluster="${esc(c.id)}" style="${card}padding:8px;margin-bottom:5px;cursor:pointer;border-left:4px solid ${this.clusterColor[c.id]}">
              <div style="display:flex;justify-content:space-between;font-size:12px"><b>${esc(c.label)}</b><span style="${muted}">${c.programs} pgm · ${fmt(c.lines)} loc</span></div>
              <div style="display:flex;gap:10px;${muted}font-size:10px;margin-top:3px;flex-wrap:wrap">
                <span style="color:${TIER[c.tier] || '#94a3b8'}">${esc(c.tier || '')}</span><span>cohesion ${pct(c.cohesion)}</span><span>score ${pct(c.carveScore)}</span>
                <span>${(c.ownedData || []).length} owned / ${(c.sharedData || []).length} shared data</span>${c.standalone ? '<span style="color:#22c55e">standalone</span>' : ''}
              </div>
            </div>`).join('')}</div>`).join('')}`);
    }

    async stage(cid, run) {
      const out = this.root.querySelector('#emc-stage-out');
      const say = (h) => { if (out) out.innerHTML = h; };
      const needs = this.root.querySelector('#emc-needs')?.checked !== false;
      const strip = (id) => String(id).replace(/^[a-z-]+:/, '');
      say(`<span style="${muted}">Staging cluster…</span>`);
      try {
        const r = await fetch(`/api/estate/cluster/${encodeURIComponent(cid)}/slice?includeNeeds=${needs}`);
        const d = await r.json().catch(() => ({}));
        if (!r.ok) throw new Error(d.error || `HTTP ${r.status}`);
        const s = d.slice || {};
        const list = (t, xs, c) => xs?.length ? `<div style="margin-top:4px"><span style="${muted}">${t} (${xs.length})</span> <span style="color:${c}">${xs.map(x => esc(strip(x))).join(', ')}</span></div>` : '';
        say(`✅ ${(d.selectors || []).length} program(s) to convert
          ${list('Programs', s.programs, '#e2e8f0')}${list('Also needs', s.needs, '#94a3b8')}${list('Missing', s.missing, '#f87171')}${list('Jobs that run end to end', s.jobs, '#a78bfa')}
          ${d.command ? `<div style="margin-top:6px;display:flex;gap:6px;align-items:center"><code style="flex:1;background:#0f172a;padding:4px 6px;border-radius:4px;word-break:break-all">${esc(d.command)}</code><button id="emc-copy" style="${btn}">Copy</button></div>` : ''}`);
        const cp = this.root.querySelector('#emc-copy');
        if (cp) cp.onclick = () => navigator.clipboard?.writeText(d.command);
        if (!run) return;
        if (!(d.selectors || []).length) { say('<span style="color:#f59e0b">The slice has no programs to convert.</span>'); return; }
        const lang = prompt('Target language for the AI loop (Java or CSharp):', document.getElementById('mc-language-select')?.value || 'Java');
        if (!lang) return;
        if (!confirm(`Convert ${d.selectors.length} program(s) from ${cid} to ${lang}? This starts a model run.`)) return;
        const body = {
          clusterId: cid, includeNeeds: needs, targetLanguage: lang,
          speedProfile: document.getElementById('mc-speed-select')?.value || 'balanced',
          provider: document.getElementById('mc-provider-select')?.value || undefined,
          modelId: document.getElementById('mc-model-select')?.value || null,
        };
        const sr = await fetch('/api/estate/slice/convert', { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body) });
        const p = await sr.json().catch(() => ({}));
        if (!sr.ok) throw new Error(p.error || p.title || `HTTP ${sr.status}`);
        say(`🔁 AI run <b>${esc(p.runId)}</b> (${esc(p.name)}) started over ${(p.programs || []).length} program(s). <a href="#" id="emc-goloop" style="color:#a78bfa">Open AI Loop →</a>`);
        this.root.querySelector('#emc-goloop').onclick = (ev) => { ev.preventDefault(); window.switchDashboard?.('ailoop'); };
      } catch (e) {
        say(`<span style="color:#ef4444">${esc(e.message)}</span>`);
      }
    }

    async rebuild() {
      const b = this.root.querySelector('#emc-rebuild, #emc-build');
      if (b) { b.disabled = true; b.textContent = '⟳ Rebuilding…'; }
      try {
        const r = await fetch('/api/estate/rebuild', { method: 'POST' });
        const d = await r.json().catch(() => ({}));
        if (!r.ok) throw new Error(d.error || d.title || `HTTP ${r.status}`);
        this.data = null;
        await this.loadAndRender(true);
      } catch (e) {
        alert(`Rebuild failed: ${e.message}`);
        if (b) { b.disabled = false; b.textContent = '⟳ Rebuild graph'; }
      }
    }
  }

  window.EstateMissionControlView = EstateMissionControlView;
})();
