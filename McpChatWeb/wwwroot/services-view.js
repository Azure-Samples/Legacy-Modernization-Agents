// Architecture & Services — projections of the REKT graph.
//
// The REKT graph is optional infrastructure. Every mode degrades to an explicit
// notice when it is unreachable, because an empty canvas is indistinguishable
// from an estate with no dependencies.

const SV_MODES = {
  layers: 'Layers',
  components: 'Components',
  techmap: 'Tech Map',
  dependencies: 'Dependencies',
  modules: 'Modules',
  deadcode: 'Reachability',
};

const SV_PENDING_MODES = ['deadcode'];

const SV_UNREACHABLE =
  'REKT graph is unavailable. Start it with ./doctor.sh rekt-full and confirm bolt://localhost:7688 is reachable.';

function svPlural(n, noun) {
  return `${n} ${noun}${n === 1 ? '' : 's'}`;
}

function svEscape(s) {
  return String(s ?? '').replace(/[&<>"']/g, c =>
    ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
}

class ServicesView {
  constructor(hostId) {
    this.host = document.getElementById(hostId);
    this.viewMode = 'layers';
    this.architecture = null;
    this.network = null;
    this._loadToken = 0;
  }

  setViewMode(mode) {
    this.viewMode = mode;
    document.querySelectorAll('.arch-mode-btn').forEach(b =>
      b.classList.toggle('active', b.dataset.mode === mode));
    this.loadAndRender();
  }

  refresh() {
    this.architecture = null;
    this.loadAndRender();
  }

  async loadAndRender() {
    if (!this.host) return;

    if (SV_PENDING_MODES.includes(this.viewMode)) {
      this._destroyNetwork();
      this.host.innerHTML = `<div class="mi-pending">
        <div class="mi-pending-icon">🚧</div>
        <div class="mi-pending-title">${svEscape(SV_MODES[this.viewMode])}</div>
        <div class="mi-pending-body">Not wired in this build — ships with a later feature.</div>
      </div>`;
      this._setStats('—');
      this._setLegend('<div class="sv-dim">Not available in this build.</div>');
      return;
    }

    this.host.innerHTML = '<div class="mi-loading">Loading…</div>';
    const token = ++this._loadToken;
    try {
      if (!this.architecture) {
        const runId = window.getSelectedScanRunId?.() ?? 'latest';
        const qs = /^\d+$/.test(String(runId)) ? `?scanRunId=${runId}` : '';
        const [services, architect] = await Promise.all([
          fetch(`/api/graph/rekt/services${qs}`).then(r => r.ok ? r.json() : { note: SV_UNREACHABLE }),
          fetch(`/api/graph/rekt/architect${qs}`).then(r => r.ok ? r.json() : { note: SV_UNREACHABLE }),
        ]);
        // A run switched while this was in flight; the newer load owns the cache and the DOM.
        if (token !== this._loadToken) return;
        this.architecture = { services, architect };
      }

      const { services, architect } = this.architecture;
      const note = services.note || architect.note;
      if (note) {
        this._destroyNetwork();
        this.host.innerHTML = `<div class="mi-notice mi-notice-warn">${svEscape(note)}</div>`;
        this._setStats('REKT graph unavailable');
        this._setLegend('<div class="sv-dim">No projection to describe.</div>');
        return;
      }

      this.host.innerHTML = '';
      this._render(services, architect);
    } catch (e) {
      console.error('Services view load error:', e);
      this.host.innerHTML =
        `<div class="mi-notice mi-notice-error">Could not load the projection: ${svEscape(e.message)}</div>`;
    }
  }

  _render(services, architect) {
    const nodes = services.nodes || [];
    const edges = services.edges || [];
    const programs = nodes.filter(n => n.type !== 'copybook');
    const copybooks = nodes.filter(n => n.type === 'copybook');

    this._setStats([
      svPlural(programs.length, 'program'),
      svPlural(copybooks.length, 'copybook'),
      svPlural(edges.length, 'edge')
    ].join(' · '));

    if (typeof vis === 'undefined') {
      this.host.innerHTML =
        '<div class="mi-notice mi-notice-warn">vis-network did not load, so the graph cannot be drawn.</div>';
      return;
    }

    const built = this._buildGraph(this.viewMode, nodes, edges, architect);
    this._destroyNetwork();
    this.network = new vis.Network(this.host, { nodes: built.nodes, edges: built.edges }, {
      physics: {
        solver: 'forceAtlas2Based',
        forceAtlas2Based: { gravitationalConstant: -200, centralGravity: 0.005, springLength: 250, avoidOverlap: 0.8 },
        stabilization: { iterations: 220 },
      },
      layout: built.hierarchical
        ? { hierarchical: { direction: 'UD', sortMethod: 'directed', levelSeparation: 140 } }
        : {},
      interaction: { hover: true, tooltipDelay: 120 },
    });

    this.network.on('click', params => {
      const id = params.nodes?.[0];
      if (id) this._showDetail(nodes.find(n => n.id === id));
    });

    this._setLegend(built.legend);
  }

  _buildGraph(mode, nodes, edges, architect) {
    const astByFile = new Map((architect.programs || []).map(p => [p.fileName, p]));
    const visEdges = edges.map(e => ({
      from: e.source,
      to: e.target,
      arrows: 'to',
      dashes: e.type === 'COPY',
      color: { color: e.type === 'COPY' ? '#10b98166' : '#60a5fa66' },
    }));

    const base = n => ({
      id: n.id,
      label: n.id,
      size: Math.min(34, 8 + Math.sqrt(n.lineCount || 0)),
      font: { color: '#e2e8f0', size: 11 },
      title: `${n.id}\n${n.lineCount || 0} LoC\nCALL ${n.callCount} · PERFORM ${n.performCount} · SQL ${n.sqlCount}`,
    });

    const paint = (n, color, shape) => ({
      ...base(n),
      shape: shape || (n.type === 'copybook' ? 'box' : 'dot'),
      color: { background: `${color}33`, border: color },
    });

    if (mode === 'layers') {
      // Presentation / logic / data inferred from what the AST actually
      // contains, so a program lands in a layer only on evidence.
      const layerOf = n => {
        if (n.type === 'copybook') return { name: 'Data structures', color: '#10b981', level: 2 };
        if (n.displayCount > 0 && n.sqlCount === 0) return { name: 'Presentation', color: '#f59e0b', level: 0 };
        if (n.sqlCount > 0) return { name: 'Data access', color: '#38bdf8', level: 2 };
        return { name: 'Business logic', color: '#60a5fa', level: 1 };
      };
      return {
        hierarchical: true,
        nodes: nodes.map(n => { const l = layerOf(n); return { ...paint(n, l.color), level: l.level, group: l.name }; }),
        edges: visEdges,
        legend: this._legend([
          ['#f59e0b', 'Presentation — DISPLAY, no SQL'],
          ['#60a5fa', 'Business logic'],
          ['#38bdf8', 'Data access — contains EXEC SQL'],
          ['#10b981', 'Data structures — copybooks'],
        ]),
      };
    }

    if (mode === 'techmap') {
      const color = n => n.sqlCount > 0 ? '#38bdf8'
        : n.displayCount > 0 ? '#f59e0b'
          : n.callCount > 0 ? '#a78bfa' : '#64748b';
      return {
        nodes: nodes.map(n => paint(n, color(n))),
        edges: visEdges,
        legend: this._legend([
          ['#38bdf8', 'Uses embedded SQL'],
          ['#f59e0b', 'Screen or report output'],
          ['#a78bfa', 'Calls other programs'],
          ['#64748b', 'Self-contained'],
        ]),
      };
    }

    if (mode === 'components') {
      // Fan-in is the practical measure of how much depends on a program.
      const fanIn = new Map();
      edges.forEach(e => fanIn.set(e.target, (fanIn.get(e.target) || 0) + 1));
      const color = n => {
        const c = fanIn.get(n.id) || 0;
        return c >= 5 ? '#ef4444' : c >= 2 ? '#f59e0b' : '#60a5fa';
      };
      return {
        nodes: nodes.map(n => ({
          ...paint(n, color(n)),
          size: Math.min(40, 10 + (fanIn.get(n.id) || 0) * 4),
        })),
        edges: visEdges,
        legend: this._legend([
          ['#ef4444', 'Depended on by 5+ — change is expensive'],
          ['#f59e0b', 'Depended on by 2–4'],
          ['#60a5fa', 'Depended on by 0–1'],
        ]),
      };
    }

    if (mode === 'modules') {
      // Group by leading name segment; mainframe estates are named by subsystem.
      const prefixOf = id => (id.match(/^[A-Za-z]{2,4}/) || ['MISC'])[0].toUpperCase();
      const palette = ['#60a5fa', '#10b981', '#f59e0b', '#a78bfa', '#ef4444', '#38bdf8', '#f472b6'];
      const prefixes = [...new Set(nodes.map(n => prefixOf(n.id)))].sort();
      const colorOf = p => palette[prefixes.indexOf(p) % palette.length];
      return {
        nodes: nodes.map(n => ({ ...paint(n, colorOf(prefixOf(n.id))), group: prefixOf(n.id) })),
        edges: visEdges,
        legend: this._legend(prefixes.slice(0, 12).map(p => [colorOf(p), `${p}*`])),
      };
    }

    // dependencies — the plain graph, sized by AST availability
    return {
      nodes: nodes.map(n => paint(n, astByFile.get(n.id)?.hasAst ? '#10b981' : '#64748b')),
      edges: visEdges,
      legend: this._legend([
        ['#10b981', 'Flow AST available'],
        ['#64748b', 'No AST — dependency edges only'],
        ['#60a5fa', 'Solid edge: CALL'],
        ['#10b981', 'Dashed edge: COPY'],
      ]),
    };
  }

  _legend(entries) {
    return entries.map(([color, label]) =>
      `<div class="sv-legend-row"><span class="sv-swatch" style="background:${color};"></span>${svEscape(label)}</div>`
    ).join('');
  }

  _showDetail(node) {
    const host = document.getElementById('services-detail');
    if (!host || !node) return;
    host.innerHTML = `
      <div class="sv-detail-name">${svEscape(node.id)}</div>
      <table class="mi-table mi-table-compact">
        <tbody>
          <tr><td>Kind</td><td>${svEscape(node.type)}</td></tr>
          <tr><td>Lines</td><td>${node.lineCount || 0}</td></tr>
          <tr><td>Flow AST</td><td>${node.hasAst ? 'present' : 'absent'}</td></tr>
          <tr><td>CALL</td><td>${node.callCount}</td></tr>
          <tr><td>PERFORM</td><td>${node.performCount}</td></tr>
          <tr><td>EXEC SQL</td><td>${node.sqlCount}</td></tr>
          <tr><td>DISPLAY</td><td>${node.displayCount}</td></tr>
        </tbody>
      </table>`;
  }

  searchNode(term) {
    if (!this.network || !term) return;
    const match = this.network.body.data.nodes.get()
      .find(n => String(n.id).toLowerCase().includes(term.toLowerCase()));
    if (match) this.network.focus(match.id, { scale: 1.3, animation: true });
  }

  zoomToFit() { this.network?.fit({ animation: true }); }

  _destroyNetwork() {
    this.network?.destroy();
    this.network = null;
  }

  _setStats(text) {
    const el = document.getElementById('services-stats');
    if (el) el.textContent = text;
  }

  _setLegend(html) {
    const el = document.getElementById('services-legend-content');
    if (el) el.innerHTML = `<h4 class="sv-legend-title">Legend</h4>${html}`;
  }
}

window.ServicesView = ServicesView;
