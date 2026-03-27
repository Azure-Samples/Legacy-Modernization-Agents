// Architecture View — Sigma.js clustered graph with ForceAtlas2
// Programs grouped by capability, toggleable clusters
// Safari/Edge compatible via ResizeObserver

class ArchitectView {
  constructor(containerId) {
    this.containerId = containerId;
    this.renderer = null;
    this.graph = null;
    this.labelsVisible = true;
    this.data = null;
    this.hiddenClusters = new Set();

    const searchInput = document.getElementById('architect-search');
    if (searchInput) {
      searchInput.addEventListener('input', (e) => this.searchNode(e.target.value));
    }
  }

  async loadAndRender() {
    const container = document.getElementById(this.containerId);
    if (!container) return;

    try {
      const resp = await fetch('/api/graph/rekt/architect');
      if (!resp.ok) return;
      this.data = await resp.json();
    } catch (e) {
      console.error('Architect: fetch error', e);
      return;
    }

    if (!this.data?.programs?.length) {
      container.innerHTML = '<div style="display:flex;align-items:center;justify-content:center;height:100%;color:#94a3b8;">No architecture data. Run: ./doctor.sh rekt-full</div>';
      return;
    }

    if (this.renderer) { try { this.renderer.kill(); } catch(_){} this.renderer = null; }

    this.graph = new graphology.Graph({ multi: true, type: 'directed' });

    const clusterColors = {
      sql: '#a855f7', logic: '#10b981', io: '#3b82f6', ui: '#f59e0b', copybook: '#ef4444',
    };

    const nodeSet = new Set();
    for (const prog of this.data.programs) {
      const id = prog.fileName;
      if (nodeSet.has(id)) continue;
      nodeSet.add(id);

      const cluster = this._classify(prog);
      const label = id.replace(/\.(cbl|cpy|CBL|CPY)$/, '');

      // Initial positions by cluster (radial)
      const clusterAngles = { sql: 0, logic: Math.PI*0.4, io: Math.PI*0.8, ui: Math.PI*1.2, copybook: Math.PI*1.6 };
      const angle = (clusterAngles[cluster] || 0) + (Math.random() - 0.5) * 0.8;
      const radius = 200 + Math.random() * 150;

      this.graph.addNode(id, {
        label,
        size: prog.isCopybook ? 4 : Math.min(20, 5 + (prog.lineCount || 0) * 0.003),
        color: clusterColors[cluster],
        x: Math.cos(angle) * radius,
        y: Math.sin(angle) * radius,
        _data: prog, _cluster: cluster, hidden: false,
      });
    }

    for (const dep of (this.data.dependencies || [])) {
      if (!this.graph.hasNode(dep.source) || !this.graph.hasNode(dep.target)) continue;
      this.graph.addEdge(dep.source, dep.target, {
        color: dep.type === 'CALL' ? '#10b981' : '#3b82f6',
        size: dep.type === 'CALL' ? 2 : 0.5,
        _type: dep.type, hidden: false,
      });
    }

    // ForceAtlas2 layout
    if (typeof ForceAtlas2Layout !== 'undefined') {
      ForceAtlas2Layout.assign(this.graph, { iterations: 150, settings: { gravity: 1.5, scalingRatio: 15, strongGravityMode: true, barnesHutOptimize: this.graph.order > 500 } });
    } else {
      this._clusterLayout();
    }

    container.innerHTML = '';
    container.style.minHeight = '400px';
    await this._waitForDimensions(container);

    try {
      this.renderer = new Sigma(this.graph, container, {
        renderEdgeLabels: false,
        labelDensity: 0.07,
        labelGridCellSize: 60,
        labelRenderedSizeThreshold: 4,
        labelFont: 'system-ui, -apple-system, sans-serif',
        defaultNodeColor: '#68bdf6',
        defaultEdgeColor: '#334155',
        allowInvalidContainer: true,
      });
    } catch (e) {
      console.error('Sigma init failed:', e);
      container.innerHTML = `<div style="padding:20px;color:#f87171;">WebGL failed: ${e.message}</div>`;
      return;
    }

    this.renderer.on('clickNode', ({ node }) => {
      const attrs = this.graph.getNodeAttributes(node);
      this._showDetail(attrs);
    });

    this.renderer.on('doubleClickNode', ({ node }) => {
      const attrs = this.graph.getNodeAttributes(node);
      if (typeof switchDashboard === 'function') {
        switchDashboard('controlflow');
        setTimeout(() => controlFlowView?.selectFile(attrs._data?.fileName || node), 100);
      }
    });

    const statsEl = document.getElementById('architect-stats');
    if (statsEl) {
      const progs = this.data.programs.filter(p => !p.isCopybook).length;
      const cpys = this.data.programs.filter(p => p.isCopybook).length;
      statsEl.textContent = `${progs} programs, ${cpys} copybooks, ${(this.data.dependencies || []).length} deps`;
    }
  }

  async _waitForDimensions(el) {
    if (typeof ResizeObserver !== 'undefined') {
      await new Promise(resolve => {
        if (el.offsetWidth > 0 && el.offsetHeight > 0) { resolve(); return; }
        const ro = new ResizeObserver(entries => {
          for (const entry of entries) {
            if (entry.contentRect.width > 0 && entry.contentRect.height > 0) {
              ro.disconnect(); resolve(); return;
            }
          }
        });
        ro.observe(el);
        setTimeout(() => { ro.disconnect(); resolve(); }, 1000);
      });
    } else {
      for (let i = 0; i < 20; i++) {
        await new Promise(r => setTimeout(r, 50));
        if (el.offsetWidth > 0 && el.offsetHeight > 0) break;
      }
    }
  }

  toggleCluster(cluster) {
    if (this.hiddenClusters.has(cluster)) this.hiddenClusters.delete(cluster);
    else this.hiddenClusters.add(cluster);
    this._applyFilters();
  }

  _applyFilters() {
    if (!this.graph || !this.renderer) return;
    this.graph.forEachNode((node, attrs) => {
      this.graph.setNodeAttribute(node, 'hidden', this.hiddenClusters.has(attrs._cluster));
    });
    this.graph.forEachEdge((edge, attrs, source, target) => {
      const srcH = this.graph.getNodeAttribute(source, 'hidden');
      const tgtH = this.graph.getNodeAttribute(target, 'hidden');
      this.graph.setEdgeAttribute(edge, 'hidden', srcH || tgtH);
    });
    this.renderer.refresh();
  }

  _classify(prog) {
    if (prog.isCopybook) return 'copybook';
    if (prog.sqlCount > 5) return 'sql';
    if (prog.displayCount > 3) return 'ui';
    if (prog.callCount > 0) return 'io';
    return 'logic';
  }

  _showDetail(attrs) {
    const detail = document.getElementById('architect-detail');
    if (!detail) return;
    const p = attrs._data; if (!p) return;
    detail.innerHTML = `
      <div style="border-top:1px solid #334155;padding-top:10px;">
        <strong style="color:${attrs.color}">${attrs.label}</strong>
        <div style="margin-top:8px;font-size:11px;">
          <div>📏 ${p.lineCount || '?'} lines</div>
          ${p.hasAst ? '<div style="color:#10b981;">✅ AST parsed</div>' : '<div style="color:#64748b;">— No AST data</div>'}
          ${p.sqlCount > 0 ? `<div style="color:#a855f7;">🗃️ ${p.sqlCount} SQL</div>` : ''}
          ${p.performCount > 0 ? `<div style="color:#06b6d4;">🔄 ${p.performCount} PERFORM</div>` : ''}
          ${p.callCount > 0 ? `<div style="color:#ef4444;">📞 ${p.callCount} CALL</div>` : ''}
          <div style="margin-top:8px;">
            <button class="btn-small drill-btn" onclick="switchDashboard('controlflow');setTimeout(()=>controlFlowView?.selectFile('${(p.fileName||'').replace(/'/g,"\\'")}'),100)">⚡ Control Flow</button>
            <button class="btn-small drill-btn" style="margin-top:4px;" onclick="astExplorer?.drillIntoProgram('${(p.fileName||'').replace(/'/g,"\\'")}')">🔬 AST</button>
          </div>
        </div>
      </div>`;
  }

  _clusterLayout() {
    const cp = { sql:{x:0,y:-300}, logic:{x:300,y:100}, io:{x:-300,y:100}, ui:{x:0,y:300}, copybook:{x:0,y:0} };
    this.graph.forEachNode((id, attrs) => {
      const c = cp[attrs._cluster] || {x:0,y:0};
      this.graph.setNodeAttribute(id, 'x', c.x + (Math.random()-0.5)*200);
      this.graph.setNodeAttribute(id, 'y', c.y + (Math.random()-0.5)*200);
    });
  }

  searchNode(query) {
    if (!this.graph || !this.renderer) return;
    if (!query) { this.graph.forEachNode(n => this.graph.setNodeAttribute(n, 'highlighted', false)); this.renderer.refresh(); return; }
    const q = query.toLowerCase(); let found = null;
    this.graph.forEachNode((node, attrs) => {
      const match = (attrs.label || '').toLowerCase().includes(q);
      this.graph.setNodeAttribute(node, 'highlighted', match);
      if (match && !found) found = node;
    });
    if (found) { const a = this.graph.getNodeAttributes(found); this.renderer.getCamera().animate({x:a.x,y:a.y,ratio:0.3},{duration:300}); }
    this.renderer.refresh();
  }

  zoomToFit() { if (this.renderer) this.renderer.getCamera().animate({x:0.5,y:0.5,ratio:1},{duration:300}); }
  toggleLabels() { this.labelsVisible = !this.labelsVisible; if (this.renderer) this.renderer.setSetting('labelRenderedSizeThreshold', this.labelsVisible ? 4 : 999); }
  refresh() { this.loadAndRender(); }
}
