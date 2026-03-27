// Architecture View — CAST Imaging-style layered discovery
// Three modes: Layers (swim lanes), Components (tech grouping), Tech Map (technology architecture)

class ServicesView {
  constructor(containerId) {
    this.containerId = containerId;
    this.network = null;
    this.data = null;
    this.nodesDS = null;
    this.edgesDS = null;
    this.viewMode = 'layers';
    this._classified = null; // cached classification
  }

  async loadAndRender() {
    const container = document.getElementById(this.containerId);
    if (!container) return;
    container.innerHTML = '<div style="display:flex;align-items:center;justify-content:center;height:100%;color:#94a3b8;">Loading architecture...</div>';

    try {
      const scanParam = typeof _currentScanRunId !== 'undefined' && _currentScanRunId && _currentScanRunId !== 'all' && _currentScanRunId !== 'latest'
        ? `?scanRunId=${_currentScanRunId}` : '';

      const [svcResp, archResp] = await Promise.all([
        fetch('/api/graph/rekt/services' + scanParam),
        fetch('/api/graph/rekt/architect' + scanParam)
      ]);
      const svcData = svcResp.ok ? await svcResp.json() : { nodes: [], edges: [] };
      const archData = archResp.ok ? await archResp.json() : { programs: [], dependencies: [] };

      const svcMap = new Map();
      for (const n of svcData.nodes) svcMap.set(n.id, n);

      this.data = {
        nodes: archData.programs.map(p => ({
          ...p, id: p.fileName,
          type: p.isCopybook ? 'copybook' : 'program',
          sqlCount: svcMap.get(p.fileName)?.sqlCount || 0,
          callCount: svcMap.get(p.fileName)?.callCount || 0,
          performCount: svcMap.get(p.fileName)?.performCount || 0,
          displayCount: svcMap.get(p.fileName)?.displayCount || 0,
        })),
        edges: archData.dependencies || svcData.edges || [],
      };
    } catch (e) {
      container.innerHTML = `<div style="padding:20px;color:#f87171;">Error: ${e.message}</div>`;
      return;
    }

    if (!this.data?.nodes?.length) {
      container.innerHTML = '<div style="padding:20px;color:#94a3b8;">No data. Run: ./doctor.sh rekt-full</div>';
      return;
    }

    this._classify();
    this._render();
  }

  // ── Classification engine — shared by all views ──
  _classify() {
    const programs = this.data.nodes.filter(n => n.type === 'program');
    const copybooks = this.data.nodes.filter(n => n.type === 'copybook');
    const callEdges = this.data.edges.filter(e => e.type === 'CALL');
    const copyEdges = this.data.edges.filter(e => e.type === 'COPY');

    const calledBy = {}, calls = {};
    for (const e of callEdges) {
      if (!calls[e.source]) calls[e.source] = [];
      calls[e.source].push(e.target);
      if (!calledBy[e.target]) calledBy[e.target] = [];
      calledBy[e.target].push(e.source);
    }

    const cpyUsage = {};
    for (const e of copyEdges) cpyUsage[e.target] = (cpyUsage[e.target] || 0) + 1;

    const layers = { presentation: [], coordination: [], business: [], batch: [], dataAccess: [] };
    for (const p of programs) {
      const name = (p.id || '').toUpperCase();
      const hasCalls = (calls[p.id] || []).length > 0;
      const isCalled = (calledBy[p.id] || []).length > 0;
      const hasSQL = (p.sqlCount || 0) > 0;

      if (name.match(/^(COSGN|COMEN|COADM|COUSR|COTRN|COCRD|COBIL|CORPT|COACTU|COACTV)/))
        layers.presentation.push(p);
      else if (hasCalls && !isCalled) layers.coordination.push(p);
      else if (hasSQL || name.match(/^(CSUTL|DBCR|XFRFUN|CREACC|DELACC|UPDACC|UPDCUST|DELCUS|INQACC|INQCUST)/))
        layers.dataAccess.push(p);
      else if (name.match(/^(CB|BATCH|CBACT|CBTRN|CBCUS|CBEX|CBIM|ABND)/))
        layers.batch.push(p);
      else layers.business.push(p);
    }

    this._classified = { programs, copybooks, callEdges, copyEdges, calledBy, calls, cpyUsage, layers };
  }

  _render() {
    const container = document.getElementById(this.containerId);
    if (this.network) { this.network.destroy(); this.network = null; }
    container.innerHTML = '';

    switch (this.viewMode) {
      case 'layers': this._renderLayers(container); break;
      case 'components': this._renderComponents(container); break;
      case 'techmap': this._renderTechMap(container); break;
      case 'dependencies': this._renderDependencies(container); break;
      case 'modules': this._renderModules(container); break;
      case 'deadcode': this._renderDeadCode(container); return; // async, handles its own legend
    }
    this._updateLegend();
  }

  setViewMode(mode) {
    this.viewMode = mode;
    document.querySelectorAll('.arch-mode-btn').forEach(b => b.classList.toggle('active', b.dataset.mode === mode));
    if (mode === 'deadcode') {
      const container = document.getElementById(this.containerId);
      if (this.network) { this.network.destroy(); this.network = null; }
      container.innerHTML = '';
      this._renderDeadCode(container);
      return;
    }
    if (this._classified) this._render();
  }

  // ═══════════════════════════════════════════════════════════════════
  //  VIEW 1: LAYERS — CAST Imaging horizontal swim-lane discovery
  //  Enhanced: layer metrics, coupling stats, complexity indicators
  // ═══════════════════════════════════════════════════════════════════
  _renderLayers(container) {
    const { programs, copybooks, callEdges, copyEdges, calledBy, calls, layers } = this._classified;

    const layerConfig = {
      presentation: { bg: '#1e3a5f', border: '#3b82f6', font: '#dbeafe', label: 'Presentation Layer', icon: '🖥️', y: 0, stripe: 'rgba(59,130,246,0.04)' },
      coordination: { bg: '#064e3b', border: '#10b981', font: '#d1fae5', label: 'Coordination Layer', icon: '🔗', y: 1, stripe: 'rgba(16,185,129,0.04)' },
      business:     { bg: '#78350f', border: '#f59e0b', font: '#fef3c7', label: 'Business Logic',     icon: '⚙️', y: 2, stripe: 'rgba(245,158,11,0.04)' },
      batch:        { bg: '#4c1d95', border: '#8b5cf6', font: '#e9d5ff', label: 'Batch Processing',   icon: '📦', y: 3, stripe: 'rgba(139,92,246,0.04)' },
      dataAccess:   { bg: '#7f1d1d', border: '#ef4444', font: '#fecaca', label: 'Data Access Layer',  icon: '🗃️', y: 4, stripe: 'rgba(239,68,68,0.04)' },
    };

    // Compute layer metrics
    const layerMetrics = {};
    const totalLOC = programs.reduce((s, p) => s + (p.lineCount || 0), 0);
    for (const [layerName, progs] of Object.entries(layers)) {
      const loc = progs.reduce((s, p) => s + (p.lineCount || 0), 0);
      const sql = progs.reduce((s, p) => s + (p.sqlCount || 0), 0);
      const callsOut = progs.reduce((s, p) => s + (calls[p.id]||[]).length, 0);
      const callsIn = progs.reduce((s, p) => s + (calledBy[p.id]||[]).length, 0);
      const copyCount = copyEdges.filter(e => progs.some(p => p.id === e.source)).length;
      // Fan-out: how many other layers does this layer call into
      const targetLayers = new Set();
      for (const p of progs) {
        for (const t of (calls[p.id] || [])) {
          for (const [ln, lp] of Object.entries(layers)) {
            if (ln !== layerName && lp.some(x => x.id === t)) targetLayers.add(ln);
          }
        }
      }
      layerMetrics[layerName] = { count: progs.length, loc, sql, callsOut, callsIn, copyCount, fanOut: targetLayers.size, locPct: totalLOC > 0 ? (loc / totalLOC * 100).toFixed(1) : 0 };
    }

    const nodeSet = new Set();
    const allNodes = [];
    const layerHeight = 170;
    const nodeSpacing = 180;
    // Track max X for swim lane width
    let maxX = 400;

    for (const [layerName, progs] of Object.entries(layers)) {
      if (progs.length === 0) continue;
      const cfg = layerConfig[layerName];
      const m = layerMetrics[layerName];
      const yBase = cfg.y * layerHeight;
      const rightEdge = (progs.length - 1) / 2 * nodeSpacing + 100;
      maxX = Math.max(maxX, rightEdge);

      // Layer label with metrics
      allNodes.push({
        id: `__layer_${layerName}`,
        label: `${cfg.icon} ${cfg.label} (${progs.length})\n${m.loc.toLocaleString()} LOC · ${m.locPct}%`,
        shape: 'text', font: { color: cfg.border, size: 13, face: 'system-ui', bold: true, multi: true },
        x: -350, y: yBase, fixed: true, physics: false,
      });

      // Program nodes
      progs.sort((a, b) => (b.lineCount || 0) - (a.lineCount || 0));
      progs.forEach((p, i) => {
        if (nodeSet.has(p.id)) return;
        nodeSet.add(p.id);
        const label = p.id.replace(/\.(cbl|cpy|CBL|CPY)$/i, '');
        const outCount = (calls[p.id]||[]).length;
        const inCount = (calledBy[p.id]||[]).length;
        const coupling = outCount + inCount;
        const loc = p.lineCount || 0;

        // Size node by LOC
        const sizeScale = Math.min(1.3, Math.max(0.8, loc / 400));

        // Border width by coupling
        const bw = coupling > 5 ? 3.5 : coupling > 2 ? 2.5 : 1.5;

        // Complexity indicator
        const complexityTag = loc > 1000 ? '🔴' : loc > 500 ? '🟡' : '🟢';

        allNodes.push({
          id: p.id,
          label: `${label}\n${loc.toLocaleString()} LOC`,
          title: `${label}\n━━━━━━━━━━━━━━━━\n📊 ${cfg.label}\n📏 ${loc.toLocaleString()} lines of code\n📞 ${outCount} outgoing CALL · ${inCount} incoming\n🔗 ${coupling} total coupling\n${p.sqlCount ? '🗃️ ' + p.sqlCount + ' SQL statements\n' : ''}${p.performCount ? '🔄 ' + p.performCount + ' PERFORM\n' : ''}${complexityTag} Complexity: ${loc > 1000 ? 'High' : loc > 500 ? 'Medium' : 'Low'}`,
          shape: 'box',
          x: (i - (progs.length - 1) / 2) * nodeSpacing,
          y: yBase,
          fixed: { y: true },
          widthConstraint: { minimum: 110 * sizeScale, maximum: 160 * sizeScale },
          heightConstraint: { minimum: 36 },
          margin: { top: 8, bottom: 8, left: 10, right: 10 },
          color: { background: cfg.bg, border: cfg.border, highlight: { background: '#fbbf24', border: '#f59e0b' }, hover: { background: cfg.border, border: '#e2e8f0' } },
          font: { color: cfg.font, size: 11, face: 'system-ui', align: 'center', multi: true },
          borderWidth: bw,
          shadow: { enabled: true, color: 'rgba(0,0,0,0.4)', size: 6, x: 2, y: 2 },
          _data: p, _layer: layerName,
        });
      });
    }

    // Persistence layer
    const persistenceY = 5 * layerHeight;
    allNodes.push({
      id: '__layer_persistence', label: '🗄️ Persistence Layer',
      shape: 'text', font: { color: '#a855f7', size: 13, face: 'system-ui', bold: true },
      x: -350, y: persistenceY, fixed: true, physics: false,
    });

    const sqlProgs = [...(layers.dataAccess||[]), ...(layers.business||[])].filter(p => (p.sqlCount||0) > 0);
    if (sqlProgs.length > 0) {
      allNodes.push({
        id: '__DB2', label: `🗄️ DB2\n${sqlProgs.reduce((s,p) => s + p.sqlCount, 0)} SQL`, shape: 'database', size: 30,
        color: { background: '#581c87', border: '#a855f7', highlight: { background: '#7c3aed' } },
        font: { color: '#e9d5ff', size: 12 }, x: -100, y: persistenceY, fixed: { y: true },
        shadow: { enabled: true, color: 'rgba(168,85,247,0.3)', size: 8 },
        _data: { id: 'DB2', type: 'database', lineCount: 0 }, _layer: 'persistence',
      });
    }

    allNodes.push({
      id: '__FILES', label: '📁 VSAM\nFiles', shape: 'database', size: 24,
      color: { background: '#1c1917', border: '#78350f', highlight: { background: '#92400e' } },
      font: { color: '#fde68a', size: 12 }, x: 100, y: persistenceY, fixed: { y: true },
      shadow: { enabled: true, color: 'rgba(120,53,15,0.3)', size: 8 },
      _data: { id: 'Files', type: 'storage', lineCount: 0 }, _layer: 'persistence',
    });

    if (copybooks.length > 0) {
      const cpyLoc = copybooks.reduce((s, c) => s + (c.lineCount || 0), 0);
      allNodes.push({
        id: '__COPYBOOKS', label: `📚 ${copybooks.length} Copybooks\n${cpyLoc.toLocaleString()} LOC`,
        shape: 'box', widthConstraint: { minimum: 160 },
        color: { background: '#292524', border: '#78350f', highlight: { background: '#44403c' } },
        font: { color: '#a8a29e', size: 11 }, x: 350, y: persistenceY, fixed: { y: true }, margin: 8,
        shadow: { enabled: true, color: 'rgba(0,0,0,0.3)', size: 6 },
        _data: { id: 'Copybooks', type: 'copybook', lineCount: cpyLoc }, _layer: 'persistence',
      });
    }

    this.nodesDS = new vis.DataSet(allNodes);

    // Edges
    const allEdges = [];
    let eid = 0;
    for (const e of callEdges) {
      if (!nodeSet.has(e.source) || !nodeSet.has(e.target)) continue;
      allEdges.push({
        id: eid++, from: e.source, to: e.target, label: 'CALL',
        arrows: { to: { enabled: true, scaleFactor: 0.6 } },
        color: { color: '#22c55e', highlight: '#4ade80' }, width: 2.5,
        font: { size: 9, color: '#22c55e', background: 'rgba(15,23,42,0.9)', strokeWidth: 0 },
        smooth: { type: 'cubicBezier', roundness: 0.2 }, _type: 'CALL',
      });
    }
    if (sqlProgs.length > 0) {
      for (const p of sqlProgs) {
        allEdges.push({
          id: eid++, from: p.id, to: '__DB2', label: `SQL×${p.sqlCount}`,
          arrows: { to: { enabled: true, scaleFactor: 0.5 } },
          color: { color: '#a855f7', highlight: '#c084fc' }, width: 1.5, dashes: [6, 3],
          font: { size: 9, color: '#a855f7', background: 'rgba(15,23,42,0.9)', strokeWidth: 0 },
          smooth: { type: 'cubicBezier', roundness: 0.3 }, _type: 'SQL',
        });
      }
    }
    this.edgesDS = new vis.DataSet(allEdges);

    this._createNetwork(container);

    // Draw swim-lane stripes behind nodes
    this.network.on('beforeDrawing', (ctx) => {
      for (const [layerName, progs] of Object.entries(layers)) {
        if (progs.length === 0) continue;
        const cfg = layerConfig[layerName];
        const yBase = cfg.y * layerHeight;
        ctx.save();
        ctx.fillStyle = cfg.stripe;
        ctx.fillRect(-maxX - 200, yBase - layerHeight / 2 + 10, maxX * 2 + 800, layerHeight - 5);
        // Thin divider line
        ctx.strokeStyle = cfg.border;
        ctx.globalAlpha = 0.15;
        ctx.lineWidth = 1;
        ctx.beginPath();
        ctx.moveTo(-maxX - 200, yBase + layerHeight / 2 + 5);
        ctx.lineTo(maxX + 600, yBase + layerHeight / 2 + 5);
        ctx.stroke();
        ctx.restore();
      }
    });

    this.network.redraw();
    this._updateStats();
  }

  // ═══════════════════════════════════════════════════════════════════
  //  VIEW 2: COMPONENTS — Technology component grouping (CAST-style)
  // ═══════════════════════════════════════════════════════════════════
  _renderComponents(container) {
    const { programs, copybooks, callEdges, copyEdges, calls, calledBy } = this._classified;

    // Canvas-based rendering with group boxes drawn as background
    // Categorize by technology component
    const components = {
      cics:    { label: 'CICS Online', icon: '🖥️', color: '#3b82f6', bg: 'rgba(59,130,246,0.08)', border: 'rgba(59,130,246,0.3)', items: [] },
      batch:   { label: 'Batch Jobs',  icon: '📦', color: '#8b5cf6', bg: 'rgba(139,92,246,0.08)', border: 'rgba(139,92,246,0.3)', items: [] },
      db2:     { label: 'DB2 Access',  icon: '🗄️', color: '#a855f7', bg: 'rgba(168,85,247,0.08)', border: 'rgba(168,85,247,0.3)', items: [] },
      logic:   { label: 'Business Rules', icon: '⚙️', color: '#f59e0b', bg: 'rgba(245,158,11,0.08)', border: 'rgba(245,158,11,0.3)', items: [] },
      utility: { label: 'Utilities',   icon: '🔧', color: '#06b6d4', bg: 'rgba(6,182,212,0.08)',  border: 'rgba(6,182,212,0.3)',  items: [] },
      data:    { label: 'Data Structures', icon: '📚', color: '#78716c', bg: 'rgba(120,113,108,0.08)', border: 'rgba(120,113,108,0.3)', items: [] },
    };

    // Classify programs into components
    for (const p of programs) {
      const name = (p.id || '').toUpperCase();
      const hasSQL = (p.sqlCount || 0) > 0;
      const hasDisplay = (p.displayCount || 0) > 0;
      const hasCalls = (calls[p.id] || []).length > 0;

      if (name.match(/^(COSGN|COMEN|COADM|COUSR|COTRN|COCRD|COBIL|CORPT|COACTU|COACTV)/))
        components.cics.items.push(p);
      else if (name.match(/^(CB|BATCH|CBACT|CBTRN|CBCUS|CBEX|CBIM|ABND)/))
        components.batch.items.push(p);
      else if (hasSQL || name.match(/^(CSUTL|DBCR|XFRFUN|CREACC|DELACC|UPDACC|UPDCUST|DELCUS|INQACC|INQCUST)/))
        components.db2.items.push(p);
      else if (name.match(/^(SSMAP|CSSET|CSDAT|LG)/))
        components.utility.items.push(p);
      else
        components.logic.items.push(p);
    }
    for (const c of copybooks) components.data.items.push(c);

    // Layout: arrange component groups in a 3×2 grid
    const allNodes = [];
    const nodeSet = new Set();
    const groupCols = 3, groupW = 550, groupH = 400, gapX = 80, gapY = 60;
    let gi = 0;

    for (const [compKey, comp] of Object.entries(components)) {
      if (comp.items.length === 0) continue;
      const col = gi % groupCols, row = Math.floor(gi / groupCols);
      const gx = col * (groupW + gapX), gy = row * (groupH + gapY);
      gi++;

      // Group header node
      allNodes.push({
        id: `__comp_${compKey}`,
        label: `${comp.icon} ${comp.label} (${comp.items.length})`,
        shape: 'text',
        font: { color: comp.color, size: 16, face: 'system-ui', bold: true },
        x: gx + groupW / 2, y: gy - 20,
        fixed: true, physics: false,
      });

      // Arrange items inside the group in rows
      const cols = Math.ceil(Math.sqrt(comp.items.length * 1.5));
      const nodeW = 140, nodeH = 50, padX = 30, padY = 40;

      comp.items.forEach((p, idx) => {
        if (nodeSet.has(p.id)) return;
        nodeSet.add(p.id);
        const c = idx % cols, r = Math.floor(idx / cols);
        const label = p.id.replace(/\.(cbl|cpy|CBL|CPY)$/i, '');
        const isCpy = p.type === 'copybook';
        const conns = (calls[p.id]||[]).length + (calledBy[p.id]||[]).length;
        const loc = p.lineCount || 0;

        // Size based on LOC
        const sizeScale = Math.min(1.4, Math.max(0.7, loc / 500));

        allNodes.push({
          id: p.id,
          label: isCpy ? label : `${label}\n${loc} LOC`,
          title: `${label}\n${comp.label}\n${loc} lines · ${conns} connections${p.sqlCount ? ' · ' + p.sqlCount + ' SQL' : ''}`,
          shape: isCpy ? 'dot' : 'box',
          size: isCpy ? 8 : undefined,
          x: gx + padX + c * (nodeW + 10),
          y: gy + padY + r * (nodeH + 10),
          fixed: true,
          widthConstraint: isCpy ? undefined : { minimum: 100 * sizeScale, maximum: 160 * sizeScale },
          margin: isCpy ? undefined : { top: 6, bottom: 6, left: 10, right: 10 },
          color: {
            background: isCpy ? comp.color : comp.bg.replace('0.08', '0.25'),
            border: comp.color,
            highlight: { background: '#fbbf24', border: '#f59e0b' },
            hover: { background: comp.color, border: '#e2e8f0' },
          },
          font: { color: isCpy ? '#94a3b8' : '#e2e8f0', size: isCpy ? 9 : 11, face: 'system-ui', align: 'center' },
          borderWidth: conns > 3 ? 2.5 : 1,
          shadow: { enabled: !isCpy, color: 'rgba(0,0,0,0.3)', size: 4 },
          _data: p, _layer: compKey,
        });
      });
    }

    this.nodesDS = new vis.DataSet(allNodes);

    // Edges — CALL between programs + COPY to copybooks
    const allEdges = [];
    let eid = 0;
    for (const e of callEdges) {
      if (!nodeSet.has(e.source) || !nodeSet.has(e.target)) continue;
      allEdges.push({
        id: eid++, from: e.source, to: e.target,
        arrows: { to: { enabled: true, scaleFactor: 0.5 } },
        color: { color: 'rgba(34,197,94,0.6)', highlight: '#4ade80' }, width: 2,
        smooth: { type: 'curvedCW', roundness: 0.15 }, _type: 'CALL',
      });
    }
    for (const e of copyEdges) {
      if (!nodeSet.has(e.source) || !nodeSet.has(e.target)) continue;
      allEdges.push({
        id: eid++, from: e.source, to: e.target,
        color: { color: 'rgba(100,116,139,0.25)', highlight: '#94a3b8' }, width: 0.8,
        dashes: [3, 3], smooth: { type: 'curvedCCW', roundness: 0.1 }, _type: 'COPY',
      });
    }
    this.edgesDS = new vis.DataSet(allEdges);

    // Custom beforeDrawing to paint group boxes
    this._createNetwork(container);

    // Draw group boxes behind nodes
    const compEntries = Object.entries(components).filter(([,c]) => c.items.length > 0);
    this.network.on('beforeDrawing', (ctx) => {
      let gi2 = 0;
      for (const [, comp] of compEntries) {
        const col = gi2 % groupCols, row = Math.floor(gi2 / groupCols);
        const gx = col * (groupW + gapX) - 20, gy = row * (groupH + gapY) - 50;
        gi2++;

        ctx.save();
        ctx.fillStyle = comp.bg;
        ctx.strokeStyle = comp.border;
        ctx.lineWidth = 1.5;
        ctx.setLineDash([6, 4]);
        const rx = 12;
        ctx.beginPath();
        ctx.moveTo(gx + rx, gy);
        ctx.lineTo(gx + groupW + 40 - rx, gy);
        ctx.quadraticCurveTo(gx + groupW + 40, gy, gx + groupW + 40, gy + rx);
        ctx.lineTo(gx + groupW + 40, gy + groupH - rx);
        ctx.quadraticCurveTo(gx + groupW + 40, gy + groupH, gx + groupW + 40 - rx, gy + groupH);
        ctx.lineTo(gx + rx, gy + groupH);
        ctx.quadraticCurveTo(gx, gy + groupH, gx, gy + groupH - rx);
        ctx.lineTo(gx, gy + rx);
        ctx.quadraticCurveTo(gx, gy, gx + rx, gy);
        ctx.closePath();
        ctx.fill();
        ctx.stroke();
        ctx.restore();
      }
    });

    this.network.redraw();
    this._updateStats();
  }

  // ═══════════════════════════════════════════════════════════════════
  //  VIEW 3: TECH MAP — Technology architecture overview
  // ═══════════════════════════════════════════════════════════════════
  _renderTechMap(container) {
    const { programs, copybooks, callEdges, copyEdges, layers } = this._classified;

    // High-level technology blocks with aggregate stats
    const techBlocks = [
      { id: 'T_CICS',     label: 'CICS\nOnline Transaction', x: 0,   y: 0,   w: 220, count: 0, loc: 0, color: '#3b82f6', icon: '🖥️', desc: 'Online screens & menus' },
      { id: 'T_BATCH',    label: 'Batch\nProcessing',        x: 280, y: 0,   w: 200, count: 0, loc: 0, color: '#8b5cf6', icon: '📦', desc: 'Scheduled batch jobs' },
      { id: 'T_COBOL',    label: 'COBOL\nBusiness Logic',    x: 140, y: 180, w: 240, count: 0, loc: 0, color: '#f59e0b', icon: '⚙️', desc: 'Core processing rules' },
      { id: 'T_DB2',      label: 'DB2\nRelational Database', x: 0,   y: 360, w: 200, count: 0, loc: 0, color: '#a855f7', icon: '🗄️', desc: 'SQL data persistence' },
      { id: 'T_VSAM',     label: 'VSAM\nFile Storage',       x: 260, y: 360, w: 200, count: 0, loc: 0, color: '#ef4444', icon: '📁', desc: 'Indexed file access' },
      { id: 'T_COPYBOOK', label: 'Copybooks\nData Structures', x: 520, y: 180, w: 200, count: 0, loc: 0, color: '#78716c', icon: '📚', desc: 'Shared data layouts' },
    ];

    // Aggregate stats
    const map = new Map(techBlocks.map(t => [t.id, t]));
    for (const p of layers.presentation) { map.get('T_CICS').count++; map.get('T_CICS').loc += p.lineCount || 0; }
    for (const p of layers.coordination) { map.get('T_COBOL').count++; map.get('T_COBOL').loc += p.lineCount || 0; }
    for (const p of layers.business)     { map.get('T_COBOL').count++; map.get('T_COBOL').loc += p.lineCount || 0; }
    for (const p of layers.batch)        { map.get('T_BATCH').count++; map.get('T_BATCH').loc += p.lineCount || 0; }
    for (const p of layers.dataAccess)   { map.get('T_DB2').count++;   map.get('T_DB2').loc += p.lineCount || 0; }
    map.get('T_COPYBOOK').count = copybooks.length;
    for (const c of copybooks) map.get('T_COPYBOOK').loc += c.lineCount || 0;
    // VSAM virtual
    map.get('T_VSAM').count = programs.filter(p => (p.id||'').toUpperCase().match(/FILE|VSAM|FD/)).length || 1;

    const allNodes = [];
    for (const t of techBlocks) {
      const kloc = (t.loc / 1000).toFixed(1);
      allNodes.push({
        id: t.id,
        label: `${t.icon} ${t.label}\n\n${t.count} components · ${kloc}K LOC`,
        title: `${t.desc}\n${t.count} components\n${t.loc.toLocaleString()} lines of code`,
        shape: 'box',
        x: t.x + t.w / 2, y: t.y + 40,
        fixed: true,
        widthConstraint: { minimum: t.w, maximum: t.w },
        heightConstraint: { minimum: 100 },
        margin: { top: 14, bottom: 14, left: 16, right: 16 },
        color: {
          background: `${t.color}18`, border: t.color,
          highlight: { background: `${t.color}40`, border: t.color },
          hover: { background: `${t.color}30`, border: '#e2e8f0' },
        },
        font: { color: '#e2e8f0', size: 13, face: 'system-ui', align: 'center', multi: true, bold: { color: t.color, size: 15 } },
        borderWidth: 2,
        borderWidthSelected: 3,
        shadow: { enabled: true, color: `${t.color}30`, size: 12, x: 0, y: 4 },
        shapeProperties: { borderRadius: 12 },
        _data: { id: t.id, type: 'technology', lineCount: t.loc, count: t.count, desc: t.desc },
        _layer: 'techmap',
      });
    }

    this.nodesDS = new vis.DataSet(allNodes);

    // Tech connections — flow arrows
    const flows = [
      { from: 'T_CICS',  to: 'T_COBOL',    label: 'CALL', count: callEdges.filter(e => layers.presentation.some(p => p.id === e.source)).length },
      { from: 'T_BATCH', to: 'T_COBOL',     label: 'CALL', count: callEdges.filter(e => layers.batch.some(p => p.id === e.source)).length },
      { from: 'T_COBOL', to: 'T_DB2',       label: 'SQL',  count: programs.reduce((s, p) => s + (p.sqlCount || 0), 0) },
      { from: 'T_COBOL', to: 'T_VSAM',      label: 'I/O',  count: 0 },
      { from: 'T_COBOL', to: 'T_COPYBOOK',  label: 'COPY', count: copyEdges.length },
      { from: 'T_CICS',  to: 'T_COPYBOOK',  label: 'COPY', count: copyEdges.filter(e => layers.presentation.some(p => p.id === e.source)).length },
    ];

    const allEdges = [];
    let eid = 0;
    for (const f of flows) {
      if (f.count === 0 && f.label !== 'I/O') continue;
      const edgeColor = f.label === 'CALL' ? '#22c55e' : f.label === 'SQL' ? '#a855f7' : f.label === 'I/O' ? '#ef4444' : '#64748b';
      allEdges.push({
        id: eid++, from: f.from, to: f.to,
        label: f.count > 0 ? `${f.label} ×${f.count}` : f.label,
        arrows: { to: { enabled: true, scaleFactor: 0.8, type: 'arrow' } },
        color: { color: edgeColor, highlight: '#fbbf24' },
        width: Math.min(6, Math.max(2, f.count / 10 + 2)),
        font: { size: 11, color: edgeColor, background: 'rgba(15,23,42,0.95)', strokeWidth: 0 },
        smooth: { type: 'cubicBezier', roundness: 0.25 },
        _type: f.label,
      });
    }
    this.edgesDS = new vis.DataSet(allEdges);

    this._createNetwork(container);
    this._updateStats();
  }

  // ═══════════════════════════════════════════════════════════════════
  //  VIEW 5: DEPENDENCIES — interactive dependency graph with insights
  //  Force-directed layout with hub detection, risk scoring,
  //  cluster visualization, and architectural violation analysis
  // ═══════════════════════════════════════════════════════════════════
  _renderDependencies(container) {
    if (!this._classified) return;
    const { programs, copybooks, callEdges, copyEdges, calledBy, calls, layers } = this._classified;

    const layerOf = {};
    const layerColor = {
      presentation: '#3b82f6', coordination: '#10b981', business: '#f59e0b',
      batch: '#8b5cf6', dataAccess: '#ef4444',
    };
    const layerLabels = {
      presentation: 'Presentation', coordination: 'Coordination',
      business: 'Business', batch: 'Batch', dataAccess: 'Data Access',
    };
    for (const [ln, progs] of Object.entries(layers)) {
      for (const p of progs) layerOf[p.id] = ln;
    }

    // ── Compute analytics ──
    const analytics = { violations: [], hubs: [], isolated: [], hotspots: [] };
    const nodeSet = new Set();
    const allNodes = [];

    for (const p of programs) {
      nodeSet.add(p.id);
      const label = p.id.replace(/\.(cbl|cpy|CBL|CPY)$/i, '');
      const outCount = (calls[p.id]||[]).length;
      const inCount = (calledBy[p.id]||[]).length;
      const coupling = outCount + inCount;
      const layer = layerOf[p.id] || 'business';
      const color = layerColor[layer] || '#f59e0b';
      const loc = p.lineCount || 0;

      // Hub detection: programs called by many others
      if (inCount >= 3) analytics.hubs.push({ id: p.id, label, inCount, outCount, coupling, loc, layer });
      // Isolated: no CALL connections at all
      if (coupling === 0) analytics.isolated.push({ id: p.id, label, loc, layer });
      // Hotspots: high LOC + high coupling = migration risk
      if (loc > 500 && coupling >= 2) analytics.hotspots.push({ id: p.id, label, loc, coupling, layer });

      // Node sizing: logarithmic scale for better distribution
      const sizeBase = Math.max(12, Math.min(50, 8 + Math.sqrt(coupling) * 10));
      const locScale = Math.max(0.8, Math.min(1.6, Math.sqrt(loc / 500)));
      const nodeSize = sizeBase * locScale;

      // Hub glow: >3 incoming calls = prominent ring
      const isHub = inCount >= 3;
      const borderW = isHub ? 4 : coupling > 2 ? 2.5 : 1.5;

      allNodes.push({
        id: p.id,
        label: label,
        title: `${label}\n━━━━━━━━━━━━━━━━\n📊 Layer: ${layerLabels[layer] || layer}\n📏 ${loc.toLocaleString()} LOC\n📞 ${outCount} outgoing CALL\n📥 ${inCount} incoming CALL\n🔗 Coupling score: ${coupling}\n${p.sqlCount ? '🗃️ ' + p.sqlCount + ' SQL statements\n' : ''}${isHub ? '⭐ HUB — called by 3+ programs\n' : ''}${coupling === 0 ? '🏝️ ISOLATED — no dependencies\n' : ''}`,
        shape: isHub ? 'dot' : 'dot',
        size: nodeSize,
        color: {
          background: coupling === 0 ? '#1e293b' : color,
          border: isHub ? '#fbbf24' : coupling === 0 ? '#475569' : color,
          highlight: { background: '#fbbf24', border: '#f59e0b' },
          hover: { background: color, border: '#e2e8f0' },
        },
        font: { color: '#e2e8f0', size: Math.max(9, Math.min(14, 8 + Math.sqrt(coupling) * 2)), face: 'system-ui', strokeWidth: 3, strokeColor: 'rgba(15,23,42,0.9)' },
        borderWidth: borderW,
        shadow: { enabled: true, color: isHub ? 'rgba(251,191,36,0.4)' : `${color}30`, size: isHub ? 18 : 8 },
        _data: p, _layer: layer,
      });
    }

    this.nodesDS = new vis.DataSet(allNodes);

    // ── Edges with violation detection ──
    const allEdges = [];
    let eid = 0;
    const layerOrder = ['presentation', 'coordination', 'business', 'batch', 'dataAccess'];
    let violationCount = 0, crossLayerCount = 0, sameLayerCount = 0;

    for (const e of callEdges) {
      if (!nodeSet.has(e.source) || !nodeSet.has(e.target)) continue;
      const srcLayer = layerOf[e.source] || '';
      const tgtLayer = layerOf[e.target] || '';
      const crossLayer = srcLayer !== tgtLayer;
      const srcIdx = layerOrder.indexOf(srcLayer);
      const tgtIdx = layerOrder.indexOf(tgtLayer);
      const isViolation = crossLayer && srcIdx >= 0 && tgtIdx >= 0 && Math.abs(tgtIdx - srcIdx) > 1;
      // Upward call = calling a higher-level layer (bad in clean architecture)
      const isUpward = crossLayer && srcIdx > tgtIdx && srcIdx >= 0 && tgtIdx >= 0;

      if (isViolation) {
        violationCount++;
        const srcLabel = e.source.replace(/\.(cbl|cpy)$/i, '');
        const tgtLabel = e.target.replace(/\.(cbl|cpy)$/i, '');
        analytics.violations.push({ from: srcLabel, to: tgtLabel, fromLayer: srcLayer, toLayer: tgtLayer });
      }
      if (crossLayer) crossLayerCount++; else sameLayerCount++;

      allEdges.push({
        id: eid++, from: e.source, to: e.target,
        arrows: { to: { enabled: true, scaleFactor: 0.5 } },
        color: { color: isViolation ? '#ef4444' : isUpward ? '#f59e0b' : crossLayer ? 'rgba(251,191,36,0.4)' : 'rgba(34,197,94,0.35)', highlight: '#fbbf24' },
        width: isViolation ? 3.5 : crossLayer ? 2 : 1.2,
        dashes: isViolation ? [8, 4] : false,
        smooth: { type: 'dynamic' },
        title: isViolation ? `⚠️ VIOLATION: ${srcLayer} → ${tgtLayer} (skips layer)` : isUpward ? `↑ Upward call: ${srcLayer} → ${tgtLayer}` : crossLayer ? `Cross-layer: ${srcLayer} → ${tgtLayer}` : 'Same-layer CALL',
        _type: 'CALL',
      });
    }

    // Store analytics for legend
    this._depAnalytics = { ...analytics, violationCount, crossLayerCount, sameLayerCount, totalEdges: allEdges.length };

    this.edgesDS = new vis.DataSet(allEdges);

    // ── Network with tuned physics ──
    if (this.network) { this.network.destroy(); this.network = null; }
    this.network = new vis.Network(container, { nodes: this.nodesDS, edges: this.edgesDS }, {
      physics: {
        enabled: true,
        solver: 'forceAtlas2Based',
        forceAtlas2Based: { gravitationalConstant: -120, centralGravity: 0.008, springLength: 180, springConstant: 0.04, damping: 0.6, avoidOverlap: 0.5 },
        stabilization: { iterations: 200, fit: true },
      },
      interaction: { hover: true, tooltipDelay: 80, navigationButtons: true, keyboard: true, multiselect: true },
      nodes: { borderWidth: 2 },
      edges: { smooth: { type: 'dynamic' } },
    });

    this.network.on('stabilizationIterationsDone', () => {
      this.network.setOptions({ physics: { enabled: false } });
    });

    this.network.on('click', (params) => {
      if (params.nodes.length > 0) {
        const nd = this.nodesDS.get(params.nodes[0]);
        if (nd?._data) this._showDepDetail(nd._data, params.nodes[0]);
      }
    });

    this.network.on('doubleClick', (params) => {
      if (params.nodes.length > 0 && typeof switchDashboard === 'function') {
        const nd = this.nodesDS.get(params.nodes[0]);
        if (nd?.id && !nd.id.startsWith('__')) {
          switchDashboard('controlflow');
          setTimeout(() => controlFlowView?.selectFile(nd.id), 100);
        }
      }
    });

    this._updateStats();
  }

  _showDepDetail(d, nodeId) {
    const panel = document.getElementById('services-detail');
    if (!panel) return;
    const label = (d.id || '').replace(/\.(cbl|cpy)$/i, '');
    const layer = this._classified ? (() => { for (const [ln, lp] of Object.entries(this._classified.layers)) { if (lp.some(x => x.id === d.id)) return ln; } return 'unknown'; })() : 'unknown';
    const layerLabels = { presentation: 'Presentation', coordination: 'Coordination', business: 'Business Logic', batch: 'Batch', dataAccess: 'Data Access' };
    const layerColor = { presentation: '#3b82f6', coordination: '#10b981', business: '#f59e0b', batch: '#8b5cf6', dataAccess: '#ef4444' };
    const color = layerColor[layer] || '#64748b';
    const { calls: callsMap = {}, calledBy: calledByMap = {} } = this._classified || {};
    const outgoing = (callsMap[d.id] || []).map(t => t.replace(/\.(cbl|cpy)$/i, ''));
    const incoming = (calledByMap[d.id] || []).map(t => t.replace(/\.(cbl|cpy)$/i, ''));
    const coupling = outgoing.length + incoming.length;
    const loc = d.lineCount || 0;
    const isHub = incoming.length >= 3;
    const risk = loc > 1000 ? 'High' : loc > 500 ? 'Medium' : 'Low';
    const riskColor = loc > 1000 ? '#ef4444' : loc > 500 ? '#f59e0b' : '#10b981';
    const riskIcon = loc > 1000 ? '🔴' : loc > 500 ? '🟡' : '🟢';

    panel.innerHTML = `
      <div style="border-left:3px solid ${color};padding-left:10px;margin-bottom:10px;">
        <div style="font-size:16px;font-weight:700;color:#e2e8f0;">${label}</div>
        <div style="color:${color};font-size:12px;font-weight:600;">${layerLabels[layer] || layer}</div>
        ${isHub ? '<div style="color:#fbbf24;font-size:11px;margin-top:2px;">⭐ Hub Program</div>' : ''}
      </div>
      <div style="display:grid;grid-template-columns:1fr 1fr 1fr;gap:4px;margin-bottom:10px;">
        <div style="background:#1e293b;border-radius:4px;padding:6px;text-align:center;">
          <div style="font-size:16px;font-weight:700;color:#60a5fa;">${loc.toLocaleString()}</div>
          <div style="font-size:8px;color:#64748b;">LOC</div>
        </div>
        <div style="background:#1e293b;border-radius:4px;padding:6px;text-align:center;">
          <div style="font-size:16px;font-weight:700;color:#fbbf24;">${coupling}</div>
          <div style="font-size:8px;color:#64748b;">COUPLING</div>
        </div>
        <div style="background:#1e293b;border-radius:4px;padding:6px;text-align:center;">
          <div style="font-size:16px;font-weight:700;color:${riskColor};">${riskIcon}</div>
          <div style="font-size:8px;color:#64748b;">${risk} RISK</div>
        </div>
      </div>
      ${d.sqlCount > 0 ? `<div style="color:#a855f7;font-size:11px;margin-bottom:6px;">🗃️ ${d.sqlCount} SQL statements</div>` : ''}
      ${d.performCount > 0 ? `<div style="color:#06b6d4;font-size:11px;margin-bottom:6px;">🔄 ${d.performCount} PERFORM calls</div>` : ''}
      ${incoming.length > 0 ? `<div style="border-top:1px solid #1e293b;padding-top:6px;margin-bottom:6px;">
        <div style="color:#22c55e;font-size:10px;text-transform:uppercase;margin-bottom:3px;">📥 Called by (${incoming.length})</div>
        ${incoming.slice(0, 8).map(c => `<div style="color:#cbd5e1;font-size:11px;cursor:pointer;" onclick="servicesView?.searchNode('${c}')">• ${c}</div>`).join('')}
        ${incoming.length > 8 ? `<div style="color:#475569;font-size:10px;">+ ${incoming.length - 8} more</div>` : ''}
      </div>` : ''}
      ${outgoing.length > 0 ? `<div style="border-top:1px solid #1e293b;padding-top:6px;margin-bottom:6px;">
        <div style="color:#f59e0b;font-size:10px;text-transform:uppercase;margin-bottom:3px;">📞 Calls (${outgoing.length})</div>
        ${outgoing.slice(0, 8).map(c => `<div style="color:#cbd5e1;font-size:11px;cursor:pointer;" onclick="servicesView?.searchNode('${c}')">• ${c}</div>`).join('')}
        ${outgoing.length > 8 ? `<div style="color:#475569;font-size:10px;">+ ${outgoing.length - 8} more</div>` : ''}
      </div>` : ''}
      <div style="display:flex;flex-direction:column;gap:4px;margin-top:6px;">
        <button class="btn-small drill-btn" onclick="switchDashboard('controlflow');setTimeout(()=>controlFlowView?.selectFile('${(d.id||'').replace(/'/g,"\\'")}'),100)">⚡ Control Flow</button>
        <button class="btn-small drill-btn" onclick="astExplorer?.drillIntoProgram('${(d.id||'').replace(/'/g,"\\'")}')">🔬 AST Explorer</button>
      </div>`;
  }

  // ═══════════════════════════════════════════════════════════════════
  //  VIEW 6: MODULES — TLA-grouped cross-module dependency map
  //  Auto-detects module prefixes, aggregates CALL flows between them
  // ═══════════════════════════════════════════════════════════════════
  _renderModules(container) {
    if (!this._classified) return;
    const { programs, callEdges, copyEdges, calledBy, calls, layers } = this._classified;

    // ── Detect TLA prefixes ──
    // Strategy: extract 2-char prefix, then merge groups < 2 programs into "OTHER"
    const prefixMap = {};
    for (const p of programs) {
      const name = p.id.replace(/\.(cbl|cpy|CBL|CPY)$/i, '').toUpperCase();
      // Try 4, 3, 2 char prefixes — pick the one that groups best
      let prefix = name.substring(0, 2);
      // Known COBOL module prefixes
      if (name.match(/^(COSGN|COMEN|COADM|COUSR|COTRN|COCRD|COBIL|CORPT)/)) prefix = 'CO';
      else if (name.match(/^COACTU|^COACTV/)) prefix = 'CO';
      else if (name.match(/^CBACT|^CBTRN|^CBCUS|^CBEX|^CBIM|^CBEXPORT/)) prefix = 'CB';
      else if (name.match(/^CSUTL|^CSSET|^CSDAT/)) prefix = 'CS';
      else if (name.match(/^(INQ)/)) prefix = 'INQ';
      else if (name.match(/^(CRE|DEL|UPD)/)) prefix = name.substring(0, 3);
      else if (name.match(/^(ABND|SSMAP)/)) prefix = name.substring(0, 4);
      else if (name.match(/^(XFRFUN|DBCR|STRESS)/)) prefix = name;
      else prefix = name.substring(0, 2);

      if (!prefixMap[prefix]) prefixMap[prefix] = [];
      prefixMap[prefix].push(p);
    }

    // Merge tiny groups into "OTHER"
    const modules = {};
    for (const [prefix, progs] of Object.entries(prefixMap)) {
      if (progs.length < 2 && prefix.length <= 2) {
        if (!modules['OTHER']) modules['OTHER'] = [];
        modules['OTHER'].push(...progs);
      } else {
        modules[prefix] = progs;
      }
    }

    // Module colors — cycle through a palette
    const palette = ['#3b82f6', '#10b981', '#f59e0b', '#8b5cf6', '#ef4444', '#06b6d4', '#ec4899', '#84cc16', '#f97316', '#14b8a6', '#a855f7', '#64748b'];
    const moduleKeys = Object.keys(modules).sort((a, b) => (modules[b].length) - (modules[a].length));
    const moduleColor = {};
    moduleKeys.forEach((k, i) => moduleColor[k] = palette[i % palette.length]);

    // Reverse lookup: program → module
    const progModule = {};
    for (const [mod, progs] of Object.entries(modules)) {
      for (const p of progs) progModule[p.id] = mod;
    }

    // ── Build module nodes ──
    const allNodes = [];
    const moduleStats = {};
    for (const mod of moduleKeys) {
      const progs = modules[mod];
      const loc = progs.reduce((s, p) => s + (p.lineCount || 0), 0);
      const sql = progs.reduce((s, p) => s + (p.sqlCount || 0), 0);
      const progNames = progs.map(p => p.id.replace(/\.(cbl|cpy)$/i, '')).sort();
      const color = moduleColor[mod];

      // Size by sqrt(LOC) for visual balance
      const nodeSize = Math.max(30, Math.min(80, 20 + Math.sqrt(loc / 50)));

      moduleStats[mod] = { count: progs.length, loc, sql, names: progNames };

      allNodes.push({
        id: `MOD_${mod}`,
        label: `${mod}\n${progs.length} pgm`,
        title: `Module: ${mod}\n━━━━━━━━━━━━━━━━\n📦 ${progs.length} programs\n📏 ${loc.toLocaleString()} LOC\n${sql > 0 ? '🗃️ ' + sql + ' SQL\n' : ''}\nPrograms:\n${progNames.map(n => '  • ' + n).join('\n')}`,
        shape: 'dot',
        size: nodeSize,
        color: { background: color, border: color, highlight: { background: '#fbbf24', border: '#f59e0b' }, hover: { background: color, border: '#e2e8f0' } },
        font: { color: '#e2e8f0', size: Math.max(12, Math.min(20, 10 + progs.length)), face: 'system-ui', bold: true, strokeWidth: 4, strokeColor: 'rgba(15,23,42,0.95)' },
        borderWidth: 3,
        shadow: { enabled: true, color: `${color}50`, size: 20, x: 0, y: 0 },
        _data: { id: mod, type: 'module', lineCount: loc, count: progs.length, sql, programs: progNames },
        _layer: 'module',
      });
    }

    this.nodesDS = new vis.DataSet(allNodes);

    // ── Build cross-module edges ──
    const crossFlows = {}; // "MOD_A→MOD_B" → count
    for (const e of callEdges) {
      const srcMod = progModule[e.source];
      const tgtMod = progModule[e.target];
      if (!srcMod || !tgtMod || srcMod === tgtMod) continue;
      const key = `MOD_${srcMod}→MOD_${tgtMod}`;
      crossFlows[key] = (crossFlows[key] || 0) + 1;
    }

    // Internal calls per module
    const internalCalls = {};
    for (const e of callEdges) {
      const srcMod = progModule[e.source];
      const tgtMod = progModule[e.target];
      if (srcMod && tgtMod && srcMod === tgtMod) {
        internalCalls[srcMod] = (internalCalls[srcMod] || 0) + 1;
      }
    }

    const allEdges = [];
    let eid = 0;
    for (const [key, count] of Object.entries(crossFlows)) {
      const [from, to] = key.split('→');
      const width = Math.max(1.5, Math.min(8, 1 + Math.sqrt(count) * 2));
      allEdges.push({
        id: eid++, from, to,
        label: count > 1 ? `${count}` : '',
        arrows: { to: { enabled: true, scaleFactor: 0.6 + count * 0.05, type: 'arrow' } },
        color: { color: 'rgba(251,191,36,0.6)', highlight: '#fbbf24' },
        width,
        font: { size: 12, color: '#fbbf24', background: 'rgba(15,23,42,0.95)', strokeWidth: 0, bold: true },
        smooth: { type: 'curvedCW', roundness: 0.15 },
        title: `${from.replace('MOD_','')} → ${to.replace('MOD_','')}: ${count} CALL${count > 1 ? 's' : ''}`,
        _type: 'CROSS_MODULE',
      });
    }

    this.edgesDS = new vis.DataSet(allEdges);

    // Store for legend
    this._moduleStats = { modules: moduleStats, moduleColor, crossFlows, internalCalls, totalCross: Object.values(crossFlows).reduce((s, v) => s + v, 0), totalInternal: Object.values(internalCalls).reduce((s, v) => s + v, 0) };

    // ── Network ──
    if (this.network) { this.network.destroy(); this.network = null; }
    this.network = new vis.Network(container, { nodes: this.nodesDS, edges: this.edgesDS }, {
      physics: {
        enabled: true,
        solver: 'forceAtlas2Based',
        forceAtlas2Based: { gravitationalConstant: -200, centralGravity: 0.005, springLength: 250, springConstant: 0.03, damping: 0.6, avoidOverlap: 0.8 },
        stabilization: { iterations: 200, fit: true },
      },
      interaction: { hover: true, tooltipDelay: 80, navigationButtons: true, keyboard: true },
      nodes: { borderWidth: 3 },
      edges: { smooth: { type: 'curvedCW', roundness: 0.15 } },
    });

    this.network.on('stabilizationIterationsDone', () => {
      this.network.setOptions({ physics: { enabled: false } });
    });

    // Draw module halos
    this.network.on('afterDrawing', (ctx) => {
      for (const mod of moduleKeys) {
        const nodeId = `MOD_${mod}`;
        const pos = this.network.getPosition(nodeId);
        if (!pos) continue;
        const color = moduleColor[mod];
        const progs = modules[mod];
        const nodeSize = Math.max(30, Math.min(80, 20 + Math.sqrt(progs.reduce((s, p) => s + (p.lineCount || 0), 0) / 50)));

        // Outer halo ring
        ctx.save();
        ctx.beginPath();
        ctx.arc(pos.x, pos.y, nodeSize + 8, 0, Math.PI * 2);
        ctx.strokeStyle = color;
        ctx.globalAlpha = 0.15;
        ctx.lineWidth = 3;
        ctx.setLineDash([4, 4]);
        ctx.stroke();
        ctx.restore();
      }
    });

    this.network.on('click', (params) => {
      if (params.nodes.length > 0) {
        const nd = this.nodesDS.get(params.nodes[0]);
        if (nd?._data) this._showModuleDetail(nd._data);
      }
    });

    this._updateStats();
  }

  _showModuleDetail(data) {
    const panel = document.getElementById('services-detail');
    if (!panel) return;
    const ms = this._moduleStats || {};
    const color = ms.moduleColor?.[data.id] || '#60a5fa';
    const internal = ms.internalCalls?.[data.id] || 0;

    // Find cross-module connections
    const outgoing = [], incoming = [];
    for (const [key, count] of Object.entries(ms.crossFlows || {})) {
      const [from, to] = key.split('→');
      if (from === `MOD_${data.id}`) outgoing.push({ mod: to.replace('MOD_',''), count });
      if (to === `MOD_${data.id}`) incoming.push({ mod: from.replace('MOD_',''), count });
    }

    panel.innerHTML = `
      <div style="border-left:3px solid ${color};padding-left:10px;margin-bottom:10px;">
        <div style="font-size:18px;font-weight:800;color:#e2e8f0;">${data.id}</div>
        <div style="color:${color};font-size:12px;font-weight:600;">Module · ${data.count} programs</div>
      </div>
      <div style="display:grid;grid-template-columns:1fr 1fr;gap:4px;margin-bottom:10px;">
        <div style="background:#1e293b;border-radius:4px;padding:6px;text-align:center;">
          <div style="font-size:16px;font-weight:700;color:#60a5fa;">${(data.lineCount || 0).toLocaleString()}</div>
          <div style="font-size:8px;color:#64748b;">LOC</div>
        </div>
        <div style="background:#1e293b;border-radius:4px;padding:6px;text-align:center;">
          <div style="font-size:16px;font-weight:700;color:#22c55e;">${internal}</div>
          <div style="font-size:8px;color:#64748b;">INTERNAL CALLS</div>
        </div>
      </div>
      ${data.sql > 0 ? `<div style="color:#a855f7;font-size:11px;margin-bottom:6px;">🗃️ ${data.sql} SQL statements across module</div>` : ''}
      ${outgoing.length > 0 ? `<div style="border-top:1px solid #1e293b;padding-top:6px;margin-bottom:6px;">
        <div style="color:#f59e0b;font-size:10px;text-transform:uppercase;margin-bottom:3px;">📤 Calls to (${outgoing.reduce((s,o) => s + o.count, 0)})</div>
        ${outgoing.sort((a,b) => b.count - a.count).map(o => `<div style="display:flex;justify-content:space-between;font-size:11px;color:#cbd5e1;cursor:pointer;" onclick="servicesView?.searchNode('${o.mod}')"><span>${o.mod}</span><span style="color:#fbbf24;font-weight:600;">${o.count}×</span></div>`).join('')}
      </div>` : ''}
      ${incoming.length > 0 ? `<div style="border-top:1px solid #1e293b;padding-top:6px;margin-bottom:6px;">
        <div style="color:#10b981;font-size:10px;text-transform:uppercase;margin-bottom:3px;">📥 Called by (${incoming.reduce((s,o) => s + o.count, 0)})</div>
        ${incoming.sort((a,b) => b.count - a.count).map(o => `<div style="display:flex;justify-content:space-between;font-size:11px;color:#cbd5e1;cursor:pointer;" onclick="servicesView?.searchNode('${o.mod}')"><span>${o.mod}</span><span style="color:#22c55e;font-weight:600;">${o.count}×</span></div>`).join('')}
      </div>` : ''}
      <div style="border-top:1px solid #1e293b;padding-top:6px;">
        <div style="color:#94a3b8;font-size:10px;text-transform:uppercase;margin-bottom:3px;">Programs in module</div>
        ${(data.programs || []).map(n => `<div style="font-size:10px;color:#cbd5e1;cursor:pointer;padding:1px 0;" onclick="servicesView?.setViewMode('dependencies');setTimeout(()=>servicesView?.searchNode('${n}'),200)">• ${n}</div>`).join('')}
      </div>`;
  }


  // ═══════════════════════════════════════════════════════════════════
  //  VIEW 7: REACHABILITY — scalable treemap + drill-down
  //  Treemap: every program as a rectangle, sized by function count,
  //  colored by reachability. Click to drill into paragraph detail.
  // ═══════════════════════════════════════════════════════════════════

  static CATEGORY = {
    entry:       { color: '#3b82f6', bg: 'rgba(59,130,246,0.15)',  border: '#3b82f6', icon: '🚀', label: 'Entry Point',        modernLabel: 'External API / main()', desc: 'Invoked externally — program start, CICS transaction, or batch JCL trigger' },
    called:      { color: '#10b981', bg: 'rgba(16,185,129,0.15)',  border: '#10b981', icon: '📞', label: 'Called (PERFORM)',    modernLabel: 'Function Call',          desc: 'Explicitly invoked via PERFORM — equivalent to a function call' },
    sequential:  { color: '#f59e0b', bg: 'rgba(245,158,11,0.15)', border: '#f59e0b', icon: '▶️', label: 'Sequential Flow',     modernLabel: 'Main Execution Flow',    desc: 'Reached via top-to-bottom execution — like the next line in main()' },
    unreachable: { color: '#ef4444', bg: 'rgba(239,68,68,0.15)',   border: '#ef4444', icon: '⚠️', label: 'Unreachable',         modernLabel: 'No Known Path',          desc: 'No PERFORM, no fall-through, not an entry point. May be invoked externally.' },
  };

  async _renderDeadCode(container) {
    container.innerHTML = '<div style="display:flex;align-items:center;justify-content:center;height:100%;color:#94a3b8;">Analyzing reachability across all programs...</div>';

    const scanParam = typeof _currentScanRunId !== 'undefined' && _currentScanRunId && _currentScanRunId !== 'all' && _currentScanRunId !== 'latest'
      ? `?scanRunId=${_currentScanRunId}` : '';

    let dcData;
    try {
      const resp = await fetch('/api/graph/rekt/deadcode' + scanParam);
      if (!resp.ok) throw new Error('Reachability API failed');
      dcData = await resp.json();
    } catch (e) {
      container.innerHTML = `<div style="padding:20px;color:#f87171;">Error: ${e.message}</div>`;
      return;
    }

    if (!dcData?.programs?.length) {
      container.innerHTML = '<div style="padding:20px;color:#94a3b8;">No AST data available. Run: ./doctor.sh rekt-full</div>';
      return;
    }

    this._dcData = dcData;
    container.innerHTML = '';

    const wrapper = document.createElement('div');
    wrapper.style.cssText = 'display:flex;height:100%;gap:0;';

    const graphDiv = document.createElement('div');
    graphDiv.style.cssText = 'flex:1;min-width:0;position:relative;height:100%;';
    graphDiv.id = 'deadcode-graph';

    const listDiv = document.createElement('div');
    listDiv.style.cssText = 'width:340px;background:#0f172a;border-left:1px solid #334155;overflow-y:auto;padding:0;';
    listDiv.id = 'reachability-panel';

    wrapper.appendChild(graphDiv);
    wrapper.appendChild(listDiv);
    container.appendChild(wrapper);

    this._renderTreemap(graphDiv, dcData);
    this._renderReachabilityPanel(listDiv, dcData);
    this._updateLegend();

    const hint = document.getElementById('services-stats');
    const s = dcData.summary || {};
    if (hint) hint.textContent = `${s.totalPrograms || 0} programs · ${s.totalParagraphs || 0} functions · ${s.called || 0} called · ${s.sequential || 0} sequential · ${s.unreachable || 0} unreachable`;
  }

  // ── Treemap: every program as a colored rectangle ──
  _renderTreemap(graphDiv, dcData) {
    const canvas = document.createElement('canvas');
    canvas.style.cssText = 'width:100%;height:100%;cursor:pointer;';
    graphDiv.appendChild(canvas);

    const resize = () => {
      canvas.width = graphDiv.clientWidth * (window.devicePixelRatio || 1);
      canvas.height = graphDiv.clientHeight * (window.devicePixelRatio || 1);
      canvas.style.width = graphDiv.clientWidth + 'px';
      canvas.style.height = graphDiv.clientHeight + 'px';
      this._drawTreemap(canvas, dcData);
    };

    const ro = new ResizeObserver(() => resize());
    ro.observe(graphDiv);
    setTimeout(resize, 50);

    canvas.addEventListener('click', (e) => {
      const rect = canvas.getBoundingClientRect();
      const dpr = window.devicePixelRatio || 1;
      const x = (e.clientX - rect.left) * dpr;
      const y = (e.clientY - rect.top) * dpr;
      if (this._treemapRects) {
        for (const r of this._treemapRects) {
          if (x >= r.x && x <= r.x + r.w && y >= r.y && y <= r.y + r.h) {
            this._drillIntoProgram(r.program);
            break;
          }
        }
      }
    });

    canvas.addEventListener('mousemove', (e) => {
      const rect = canvas.getBoundingClientRect();
      const dpr = window.devicePixelRatio || 1;
      const x = (e.clientX - rect.left) * dpr;
      const y = (e.clientY - rect.top) * dpr;
      let found = false;
      if (this._treemapRects) {
        for (const r of this._treemapRects) {
          if (x >= r.x && x <= r.x + r.w && y >= r.y && y <= r.y + r.h) {
            canvas.title = `${r.program}\n${r.total} functions: ${r.entry}E ${r.called}C ${r.seq}S ${r.unreach}U\nClick to drill down`;
            canvas.style.cursor = 'pointer';
            found = true;
            break;
          }
        }
      }
      if (!found) { canvas.title = ''; canvas.style.cursor = 'default'; }
    });
  }

  _drawTreemap(canvas, dcData) {
    const ctx = canvas.getContext('2d');
    const W = canvas.width, H = canvas.height;
    const dpr = window.devicePixelRatio || 1;
    ctx.clearRect(0, 0, W, H);

    const programs = dcData.programs.map(p => ({
      program: p.program,
      total: p.totalParagraphs || 1,
      entry: p.entryCount || 0,
      called: p.calledCount || 0,
      seq: p.sequentialCount || 0,
      unreach: p.unreachableCount || 0,
    })).sort((a, b) => b.total - a.total);

    if (programs.length === 0) return;

    const rects = this._squarify(programs.map(p => p.total), { x: 4 * dpr, y: 4 * dpr, w: W - 8 * dpr, h: H - 8 * dpr });
    this._treemapRects = rects.map((r, i) => ({ ...r, ...programs[i] }));

    const CAT = ServicesView.CATEGORY;
    for (const r of this._treemapRects) {
      const unreachPct = r.total > 0 ? r.unreach / r.total : 0;
      const calledPct = r.total > 0 ? r.called / r.total : 0;
      const seqPct = r.total > 0 ? r.seq / r.total : 0;

      let bgColor, borderColor;
      if (unreachPct > 0.3) {
        bgColor = `rgba(239,68,68,${0.15 + unreachPct * 0.4})`;
        borderColor = CAT.unreachable.color;
      } else if (calledPct > 0.6) {
        bgColor = `rgba(16,185,129,${0.1 + calledPct * 0.2})`;
        borderColor = CAT.called.color;
      } else if (seqPct > 0.5) {
        bgColor = `rgba(245,158,11,${0.1 + seqPct * 0.2})`;
        borderColor = CAT.sequential.color;
      } else {
        bgColor = 'rgba(59,130,246,0.12)';
        borderColor = '#334155';
      }

      ctx.fillStyle = bgColor;
      ctx.strokeStyle = borderColor;
      ctx.lineWidth = 1.5 * dpr;
      const rad = 4 * dpr;
      ctx.beginPath();
      ctx.moveTo(r.x + rad, r.y);
      ctx.lineTo(r.x + r.w - rad, r.y);
      ctx.quadraticCurveTo(r.x + r.w, r.y, r.x + r.w, r.y + rad);
      ctx.lineTo(r.x + r.w, r.y + r.h - rad);
      ctx.quadraticCurveTo(r.x + r.w, r.y + r.h, r.x + r.w - rad, r.y + r.h);
      ctx.lineTo(r.x + rad, r.y + r.h);
      ctx.quadraticCurveTo(r.x, r.y + r.h, r.x, r.y + r.h - rad);
      ctx.lineTo(r.x, r.y + rad);
      ctx.quadraticCurveTo(r.x, r.y, r.x + rad, r.y);
      ctx.closePath();
      ctx.fill();
      ctx.stroke();

      // Stacked bar (bottom)
      const barH = 4 * dpr, barY = r.y + r.h - barH - 2 * dpr;
      const barW = r.w - 4 * dpr, barX = r.x + 2 * dpr;
      let bx = barX;
      for (const [cnt, col] of [[r.entry, CAT.entry.color], [r.called, CAT.called.color], [r.seq, CAT.sequential.color], [r.unreach, CAT.unreachable.color]]) {
        if (cnt <= 0) continue;
        const segW = (cnt / r.total) * barW;
        ctx.fillStyle = col;
        ctx.fillRect(bx, barY, segW, barH);
        bx += segW;
      }

      // Labels
      const minW = 50 * dpr, minH = 28 * dpr;
      if (r.w > minW && r.h > minH) {
        const name = r.program.replace(/\.(cbl|cpy|CBL|CPY)$/i, '');
        const fontSize = Math.max(9, Math.min(13, r.w / (name.length * 0.8)));
        ctx.fillStyle = '#e2e8f0';
        ctx.font = `bold ${fontSize * dpr}px system-ui`;
        ctx.textBaseline = 'top';
        const tx = r.x + 4 * dpr, ty = r.y + 4 * dpr;
        ctx.fillText(name, tx, ty, r.w - 8 * dpr);

        if (r.h > 44 * dpr) {
          ctx.fillStyle = '#64748b';
          ctx.font = `${9 * dpr}px system-ui`;
          ctx.fillText(`${r.total} functions`, tx, ty + fontSize * dpr + 2 * dpr, r.w - 8 * dpr);
        }

        if (r.unreach > 0 && r.w > 70 * dpr && r.h > 50 * dpr) {
          const badge = `${r.unreach} unreachable`;
          ctx.fillStyle = 'rgba(239,68,68,0.3)';
          const bw = ctx.measureText(badge).width + 8 * dpr;
          ctx.fillRect(tx, ty + fontSize * dpr + 14 * dpr, bw, 14 * dpr);
          ctx.fillStyle = '#fca5a5';
          ctx.font = `${8 * dpr}px system-ui`;
          ctx.fillText(badge, tx + 4 * dpr, ty + fontSize * dpr + 17 * dpr);
        }
      }
    }
  }

  // Squarified treemap layout
  _squarify(values, rect) {
    const total = values.reduce((s, v) => s + v, 0);
    if (total === 0 || values.length === 0) return [];

    const rects = [];
    const items = values.map((v, i) => ({ value: v, index: i }));
    let remaining = { ...rect };

    const layoutRow = (row, rowTotal, container) => {
      const isHoriz = container.w >= container.h;
      const side = isHoriz ? container.h : container.w;
      const rowSize = side > 0 ? (rowTotal / total) * (container.w * container.h) / side : 0;
      let pos = isHoriz ? container.y : container.x;

      for (const item of row) {
        const itemSize = rowTotal > 0 ? (item.value / rowTotal) * side : 0;
        if (isHoriz) {
          rects[item.index] = { x: container.x, y: pos, w: rowSize, h: itemSize };
          pos += itemSize;
        } else {
          rects[item.index] = { x: pos, y: container.y, w: itemSize, h: rowSize };
          pos += itemSize;
        }
      }

      return isHoriz
        ? { x: container.x + rowSize, y: container.y, w: container.w - rowSize, h: container.h }
        : { x: container.x, y: container.y + rowSize, w: container.w, h: container.h - rowSize };
    };

    const worstRatio = (row, rowTotal, side) => {
      if (side <= 0 || total <= 0) return Infinity;
      const rowArea = (rowTotal / total) * (remaining.w * remaining.h);
      const rowSide = rowArea / side;
      let worst = 0;
      for (const item of row) {
        const itemSide = rowTotal > 0 ? (item.value / rowTotal) * side : 0;
        if (itemSide <= 0) continue;
        const ratio = Math.max(rowSide / itemSide, itemSide / rowSide);
        worst = Math.max(worst, ratio);
      }
      return worst;
    };

    let row = [], rowTotal = 0;
    for (const item of items) {
      const side = Math.min(remaining.w, remaining.h);
      if (side <= 0) { rects[item.index] = { x: 0, y: 0, w: 0, h: 0 }; continue; }

      const newRow = [...row, item];
      const newTotal = rowTotal + item.value;

      if (row.length === 0 || worstRatio(newRow, newTotal, side) <= worstRatio(row, rowTotal, side)) {
        row = newRow;
        rowTotal = newTotal;
      } else {
        remaining = layoutRow(row, rowTotal, remaining);
        row = [item];
        rowTotal = item.value;
      }
    }
    if (row.length > 0) layoutRow(row, rowTotal, remaining);

    const dpr = window.devicePixelRatio || 1;
    const pad = 2 * dpr;
    return rects.map(r => r ? { x: r.x + pad, y: r.y + pad, w: Math.max(0, r.w - pad * 2), h: Math.max(0, r.h - pad * 2) } : { x: 0, y: 0, w: 0, h: 0 });
  }

  // ── Drill into a single program ──
  _drillIntoProgram(programName) {
    if (!this._dcData) return;
    const prog = this._dcData.programs.find(p => p.program === programName);
    if (!prog) return;

    const panel = document.getElementById('reachability-panel');
    if (!panel) return;

    const CAT = ServicesView.CATEGORY;
    const paras = prog.paragraphs || [];
    const fileName = programName.replace(/\.(cbl|cpy)$/i, '');

    let html = `
      <div style="padding:12px;border-bottom:1px solid #334155;">
        <div style="display:flex;align-items:center;gap:8px;margin-bottom:8px;">
          <button class="btn-small" onclick="servicesView?._renderReachabilityPanel(document.getElementById('reachability-panel'), servicesView._dcData)" style="padding:2px 8px;">← Back</button>
          <div>
            <div style="font-size:14px;font-weight:700;color:#e2e8f0;">${fileName}</div>
            <div style="color:#64748b;font-size:10px;">${paras.length} functions</div>
          </div>
        </div>
        <div style="display:flex;gap:4px;margin-bottom:8px;">
          ${['entry','called','sequential','unreachable'].map(k => {
            const c = CAT[k];
            const cnt = paras.filter(p => p.category === k).length;
            return cnt > 0 ? `<div style="flex:1;background:#1e293b;border-radius:4px;padding:4px;text-align:center;border-bottom:2px solid ${c.color};">
              <div style="font-size:14px;font-weight:700;color:${c.color};">${cnt}</div>
              <div style="font-size:8px;color:#64748b;">${c.label.split(' ')[0].toUpperCase()}</div>
            </div>` : '';
          }).join('')}
        </div>
      </div>
      <div style="padding:8px;">
        ${['entry','called','sequential','unreachable'].map(catKey => {
          const catParas = paras.filter(p => p.category === catKey);
          if (catParas.length === 0) return '';
          const c = CAT[catKey];
          return `
            <div style="margin-bottom:8px;">
              <div style="color:${c.color};font-size:11px;font-weight:600;margin-bottom:4px;padding:2px 6px;background:${c.bg};border-radius:3px;display:inline-block;">
                ${c.icon} ${c.label} — ${c.modernLabel} (${catParas.length})
              </div>
              ${catParas.map(p => `
                <div style="padding:5px 8px;margin:2px 0;background:#1e293b;border-radius:3px;border-left:3px solid ${c.color};cursor:pointer;font-size:11px;"
                  onclick="servicesView?._showDeadCodeDetail(${JSON.stringify(p).replace(/"/g,'&quot;')})">
                  <div style="color:#e2e8f0;font-weight:500;">${p.name}</div>
                  <div style="color:#64748b;font-size:9px;margin-top:1px;">${(p.reason||'').substring(0, 60)}${(p.reason?.length||0) > 60 ? '...' : ''}</div>
                </div>
              `).join('')}
            </div>`;
        }).join('')}
      </div>`;

    panel.innerHTML = html;
  }

  // ── Right panel: summary view ──
  _renderReachabilityPanel(listDiv, dcData) {
    const CAT = ServicesView.CATEGORY;
    const summary = dcData.summary || {};

    let html = `
      <div style="padding:14px;border-bottom:1px solid #334155;">
        <h3 style="margin:0 0 6px;color:#e2e8f0;font-size:15px;">Reachability Analysis</h3>
        <div style="color:#94a3b8;font-size:11px;line-height:1.5;margin-bottom:10px;">
          Each rectangle = one <b style="color:#cbd5e1;">program</b>, sized by function count.
          Color shows reachability. <b style="color:#cbd5e1;">Click any program</b> to drill down.
        </div>

        <div style="display:grid;grid-template-columns:1fr 1fr;gap:6px;margin-bottom:10px;">
          ${[
            { val: summary.entry || 0, label: 'ENTRY', color: CAT.entry.color },
            { val: summary.called || 0, label: 'CALLED', color: CAT.called.color },
            { val: summary.sequential || 0, label: 'SEQUENTIAL', color: CAT.sequential.color },
            { val: summary.unreachable || 0, label: 'UNREACHABLE', color: CAT.unreachable.color },
          ].map(s => `<div style="background:#1e293b;border-radius:6px;padding:8px;text-align:center;border-bottom:2px solid ${s.color};">
            <div style="font-size:18px;font-weight:700;color:${s.color};">${s.val}</div>
            <div style="font-size:9px;color:#64748b;letter-spacing:0.5px;">${s.label}</div>
          </div>`).join('')}
        </div>

        <div style="background:#1e293b;border-radius:6px;padding:8px;">
          <div style="display:flex;justify-content:space-between;">
            <span style="color:#94a3b8;font-size:11px;">${summary.totalPrograms || 0} programs · ${summary.totalParagraphs || 0} functions</span>
            <span style="color:#ef4444;font-size:13px;font-weight:700;">${summary.unreachablePercentage || 0}%</span>
          </div>
          <div style="margin-top:4px;height:6px;background:#0f172a;border-radius:3px;overflow:hidden;display:flex;">
            <div style="height:100%;width:${((summary.entry||0)/(summary.totalParagraphs||1)*100).toFixed(1)}%;background:${CAT.entry.color};"></div>
            <div style="height:100%;width:${((summary.called||0)/(summary.totalParagraphs||1)*100).toFixed(1)}%;background:${CAT.called.color};"></div>
            <div style="height:100%;width:${((summary.sequential||0)/(summary.totalParagraphs||1)*100).toFixed(1)}%;background:${CAT.sequential.color};"></div>
            <div style="height:100%;width:${((summary.unreachable||0)/(summary.totalParagraphs||1)*100).toFixed(1)}%;background:${CAT.unreachable.color};"></div>
          </div>
        </div>
      </div>

      <div style="padding:10px 14px;border-bottom:1px solid #334155;">
        <h4 style="margin:0 0 6px;color:#60a5fa;font-size:12px;">COBOL → Modern Mapping</h4>
        <table style="width:100%;font-size:10px;color:#94a3b8;border-collapse:collapse;">
          <tr style="border-bottom:1px solid #1e293b;"><td style="padding:3px 0;color:#cbd5e1;">Paragraph</td><td>= function / method</td></tr>
          <tr style="border-bottom:1px solid #1e293b;"><td style="padding:3px 0;color:#cbd5e1;">PERFORM</td><td>= function call</td></tr>
          <tr style="border-bottom:1px solid #1e293b;"><td style="padding:3px 0;color:#cbd5e1;">Fall-through</td><td>= next line in main()</td></tr>
          <tr style="border-bottom:1px solid #1e293b;"><td style="padding:3px 0;color:#cbd5e1;">ENTRY / first para</td><td>= external API endpoint</td></tr>
          <tr><td style="padding:3px 0;color:#cbd5e1;">Section</td><td>= class / module</td></tr>
        </table>
      </div>

      <div style="padding:10px 14px;border-bottom:1px solid #334155;">
        <h4 style="margin:0 0 6px;color:#60a5fa;font-size:12px;">Object Summary</h4>
        <table style="width:100%;border-collapse:collapse;font-size:11px;">
          <thead><tr style="border-bottom:1px solid #334155;color:#64748b;">
            <th style="text-align:left;padding:3px 4px;font-weight:500;">Type</th>
            <th style="text-align:right;padding:3px 4px;font-weight:500;">Count</th>
            <th style="text-align:right;padding:3px 4px;font-weight:500;">LoC</th>
          </tr></thead>
          <tbody>${(dcData.objectList || []).map(o => {
            const isU = o.objectType?.includes('Unreachable');
            const isI = o.objectType?.startsWith('→');
            const col = isU ? '#f87171' : isI ? '#94a3b8' : '#cbd5e1';
            return `<tr style="border-bottom:1px solid #1e293b;${isU ? 'background:rgba(239,68,68,0.08);' : ''}">
              <td style="padding:3px 4px;color:${col};${isI ? 'padding-left:12px;' : ''}">${o.objectType}</td>
              <td style="text-align:right;padding:3px 4px;color:${col};font-weight:600;">${o.objectCount}</td>
              <td style="text-align:right;padding:3px 4px;color:${col};">${(o.loc || 0).toLocaleString()}</td>
            </tr>`;
          }).join('')}</tbody>
        </table>
      </div>

      <div style="padding:8px 14px;border-bottom:1px solid #334155;background:rgba(245,158,11,0.05);">
        <div style="color:#f59e0b;font-size:11px;font-weight:600;margin-bottom:3px;">⚠️ Important</div>
        <div style="color:#94a3b8;font-size:10px;line-height:1.4;">
          <b style="color:#cbd5e1;">"Unreachable" ≠ "safe to delete".</b>
          Static analysis cannot detect external invocations (JCL, CICS, dynamic CALL).
        </div>
      </div>

      <div style="padding:10px 14px;">
        <h4 style="margin:0 0 6px;color:#60a5fa;font-size:12px;">All Programs (${dcData.programs.length})</h4>
        <div style="max-height:300px;overflow-y:auto;">
          ${dcData.programs.map(p => {
            const uPct = p.totalParagraphs > 0 ? Math.round((p.unreachableCount || 0) / p.totalParagraphs * 100) : 0;
            const bc = uPct > 30 ? '#ef4444' : (p.unreachableCount || 0) > 0 ? '#f59e0b' : '#10b981';
            return `<div style="padding:4px 6px;margin-bottom:2px;background:#1e293b;border-radius:3px;cursor:pointer;border-left:2px solid ${bc};font-size:10px;"
              onclick="servicesView?._drillIntoProgram('${p.program.replace(/'/g,"\\'")}')">
              <div style="display:flex;justify-content:space-between;">
                <span style="color:#e2e8f0;font-weight:500;">${p.program.replace(/\.(cbl|cpy)$/i,'')}</span>
                <span style="color:#64748b;">${p.totalParagraphs}fn</span>
              </div>
              <div style="margin-top:2px;height:3px;background:#0f172a;border-radius:2px;overflow:hidden;display:flex;">
                <div style="height:100%;width:${(p.entryCount||0)/(p.totalParagraphs||1)*100}%;background:${CAT.entry.color};"></div>
                <div style="height:100%;width:${(p.calledCount||0)/(p.totalParagraphs||1)*100}%;background:${CAT.called.color};"></div>
                <div style="height:100%;width:${(p.sequentialCount||0)/(p.totalParagraphs||1)*100}%;background:${CAT.sequential.color};"></div>
                <div style="height:100%;width:${(p.unreachableCount||0)/(p.totalParagraphs||1)*100}%;background:${CAT.unreachable.color};"></div>
              </div>
            </div>`;
          }).join('')}
        </div>
      </div>`;

    listDiv.innerHTML = html;
  }

  _focusDeadProgram(program) { this._drillIntoProgram(program); }

  _showDeadCodeDetail(data) {
    const panel = document.getElementById('services-detail');
    if (!panel) return;
    const CAT = ServicesView.CATEGORY;
    const cat = CAT[data.category] || CAT.unreachable;
    const progName = (data.program || '').replace(/\.(cbl|cpy)$/i, '').replace(/^flow-ast-/, '');

    panel.innerHTML = `
      <div style="border-left:3px solid ${cat.color};padding-left:10px;margin-bottom:10px;">
        <div style="font-size:15px;font-weight:700;color:#e2e8f0;">${data.name}</div>
        <div style="color:${cat.color};font-size:12px;font-weight:600;">${cat.icon} ${cat.label}</div>
        <div style="color:#64748b;font-size:11px;margin-top:2px;">in ${progName}</div>
      </div>
      <div style="background:${cat.bg};border:1px solid ${cat.border}40;border-radius:6px;padding:10px;margin-bottom:10px;">
        <div style="color:#e2e8f0;font-size:11px;font-weight:600;margin-bottom:4px;">Why this classification?</div>
        <div style="color:#94a3b8;font-size:11px;line-height:1.5;">${data.reason || 'No reason available'}</div>
      </div>
      <div style="background:#1e293b;border-radius:6px;padding:8px;margin-bottom:10px;">
        <div style="color:#64748b;font-size:9px;text-transform:uppercase;margin-bottom:4px;">Modern Equivalent</div>
        <div style="color:#cbd5e1;font-size:12px;">${cat.modernLabel}</div>
        <div style="color:#64748b;font-size:10px;margin-top:2px;">${cat.desc}</div>
      </div>
      <div style="display:grid;grid-template-columns:1fr 1fr;gap:6px;margin-bottom:10px;">
        <div style="background:#1e293b;border-radius:4px;padding:6px;text-align:center;">
          <div style="font-size:16px;font-weight:700;color:#60a5fa;">${data.lineCount || '?'}</div>
          <div style="font-size:9px;color:#64748b;">LINES</div>
        </div>
        <div style="background:#1e293b;border-radius:4px;padding:6px;text-align:center;">
          <div style="font-size:14px;font-weight:700;color:#94a3b8;">${data.nodeType || ''}</div>
          <div style="font-size:9px;color:#64748b;">TYPE</div>
        </div>
      </div>
      ${data.category === 'unreachable' ? `<div style="background:rgba(245,158,11,0.1);border:1px solid rgba(245,158,11,0.3);border-radius:4px;padding:8px;margin-bottom:8px;">
        <div style="color:#fbbf24;font-size:11px;font-weight:600;">⚠️ Review Before Removing</div>
        <div style="color:#94a3b8;font-size:10px;margin-top:4px;line-height:1.4;">May be invoked by JCL, CICS LINK/XCTL, or dynamic CALL. Verify with runtime traces.</div>
      </div>` : ''}
      <div style="display:flex;flex-direction:column;gap:4px;">
        <button class="btn-small drill-btn" onclick="switchDashboard('controlflow');setTimeout(()=>controlFlowView?.selectFile('${(data.program||'').replace(/^flow-ast-/,'').replace(/'/g,"\\'")}'),100)">⚡ Control Flow</button>
        <button class="btn-small drill-btn" onclick="astExplorer?.drillIntoProgram('${(data.program||'').replace(/^flow-ast-/,'').replace(/'/g,"\\'")}')">🔬 AST Explorer</button>
      </div>`;
  }
  // ── Shared network creation ──
  _createNetwork(container) {
    this.network = new vis.Network(container, { nodes: this.nodesDS, edges: this.edgesDS }, {
      physics: { enabled: false },
      layout: { improvedLayout: false },
      interaction: { hover: true, tooltipDelay: 150, navigationButtons: true, keyboard: true, zoomView: true, dragView: true, dragNodes: true },
      nodes: { borderWidth: 1.5 },
      edges: { smooth: { type: 'cubicBezier', roundness: 0.2 } },
    });

    setTimeout(() => this.network?.fit({ animation: false }), 100);

    this.network.on('click', (params) => {
      if (params.nodes.length > 0) {
        const nd = this.nodesDS.get(params.nodes[0]);
        if (nd?._data) this._showDetail(nd._data, params.nodes[0]);
      }
    });

    this.network.on('doubleClick', (params) => {
      if (params.nodes.length > 0 && typeof switchDashboard === 'function') {
        const nd = this.nodesDS.get(params.nodes[0]);
        if (nd?.id && !nd.id.startsWith('__') && !nd.id.startsWith('T_')) {
          switchDashboard('controlflow');
          setTimeout(() => controlFlowView?.selectFile(nd.id), 100);
        }
      }
    });
  }

  // ── Legend panel — updates per view mode ──
  _updateLegend() {
    const panel = document.getElementById('services-legend-content');
    if (!panel) return;

    if (this.viewMode === 'layers') {
      const c = this._classified || {};
      const layers = c.layers || {};
      const programs = c.programs || [];
      const callEdges = c.callEdges || [];
      const copyEdges = c.copyEdges || [];
      const totalLOC = programs.reduce((s, p) => s + (p.lineCount || 0), 0);
      const layerDefs = [
        { key: 'presentation', color: '#3b82f6', label: 'Presentation' },
        { key: 'coordination', color: '#10b981', label: 'Coordination' },
        { key: 'business', color: '#f59e0b', label: 'Business Logic' },
        { key: 'batch', color: '#8b5cf6', label: 'Batch' },
        { key: 'dataAccess', color: '#ef4444', label: 'Data Access' },
        { key: null, color: '#a855f7', label: 'Persistence' },
      ];
      panel.innerHTML = `
        <h4 style="margin:0 0 8px;color:#60a5fa;">Architecture Layers</h4>
        ${layerDefs.map(l => {
          const lp = l.key ? (layers[l.key] || []) : [];
          const loc = lp.reduce((s, p) => s + (p.lineCount || 0), 0);
          const pct = totalLOC > 0 ? (loc / totalLOC * 100).toFixed(0) : 0;
          return `<div style="display:flex;align-items:center;gap:6px;margin-bottom:5px;">
            <div style="width:12px;height:12px;border-radius:3px;background:${l.color};flex-shrink:0;"></div>
            <div style="flex:1;">
              <div style="font-size:11px;color:#cbd5e1;">${l.label}</div>
              ${l.key ? `<div style="font-size:9px;color:#64748b;">${lp.length} pgm · ${loc.toLocaleString()} LOC · ${pct}%</div>` : ''}
            </div>
          </div>`;
        }).join('')}
        <div style="border-top:1px solid #334155;margin:10px 0 8px;padding-top:8px;">
          <h4 style="margin:0 0 6px;color:#60a5fa;">Portfolio Metrics</h4>
          <div style="display:grid;grid-template-columns:1fr 1fr;gap:4px;font-size:10px;">
            <div style="background:#1e293b;border-radius:3px;padding:4px;text-align:center;">
              <div style="font-size:14px;font-weight:700;color:#60a5fa;">${programs.length}</div>
              <div style="color:#64748b;">Programs</div>
            </div>
            <div style="background:#1e293b;border-radius:3px;padding:4px;text-align:center;">
              <div style="font-size:14px;font-weight:700;color:#fbbf24;">${(totalLOC/1000).toFixed(1)}K</div>
              <div style="color:#64748b;">Total LOC</div>
            </div>
            <div style="background:#1e293b;border-radius:3px;padding:4px;text-align:center;">
              <div style="font-size:14px;font-weight:700;color:#22c55e;">${callEdges.length}</div>
              <div style="color:#64748b;">CALL deps</div>
            </div>
            <div style="background:#1e293b;border-radius:3px;padding:4px;text-align:center;">
              <div style="font-size:14px;font-weight:700;color:#94a3b8;">${copyEdges.length}</div>
              <div style="color:#64748b;">COPY refs</div>
            </div>
          </div>
        </div>
        <div style="border-top:1px solid #334155;margin:8px 0;padding-top:8px;">
          <h4 style="margin:0 0 6px;color:#60a5fa;">Edges</h4>
          <div style="display:flex;align-items:center;gap:8px;margin-bottom:4px;">
            <div style="width:20px;height:2px;background:#22c55e;"></div>
            <span style="font-size:11px;color:#94a3b8;">CALL (invocation)</span>
          </div>
          <div style="display:flex;align-items:center;gap:8px;margin-bottom:4px;">
            <div style="width:20px;height:2px;background:#a855f7;border-top:1px dashed #a855f7;"></div>
            <span style="font-size:11px;color:#94a3b8;">SQL (data access)</span>
          </div>
        </div>
        <div style="border-top:1px solid #334155;margin:8px 0;padding-top:6px;">
          <div style="color:#64748b;font-size:9px;line-height:1.4;">
            🟢 &lt;500 LOC · 🟡 500–1K · 🔴 &gt;1K LOC<br>
            Thick border = high coupling (5+ connections)<br>
            Node size ∝ LOC · Hover for full metrics
          </div>
        </div>`;
    } else if (this.viewMode === 'components') {
      const compDefs = [
        { color: '#3b82f6', label: 'CICS Online' },
        { color: '#8b5cf6', label: 'Batch Jobs' },
        { color: '#a855f7', label: 'DB2 Access' },
        { color: '#f59e0b', label: 'Business Rules' },
        { color: '#06b6d4', label: 'Utilities' },
        { color: '#78716c', label: 'Data Structures' },
      ];
      panel.innerHTML = `
        <h4 style="margin:0 0 10px;color:#60a5fa;">Component Groups</h4>
        ${compDefs.map(c => `<div style="display:flex;align-items:center;gap:8px;margin-bottom:6px;">
          <div style="width:12px;height:12px;border-radius:3px;border:2px solid ${c.color};background:${c.color}20;flex-shrink:0;"></div>
          <span style="font-size:12px;color:#cbd5e1;">${c.label}</span>
        </div>`).join('')}
        <div style="border-top:1px solid #334155;margin:12px 0 8px;padding-top:8px;">
          <h4 style="margin:0 0 8px;color:#60a5fa;">Edges</h4>
          <div style="display:flex;align-items:center;gap:8px;margin-bottom:4px;">
            <div style="width:20px;height:2px;background:rgba(34,197,94,0.6);"></div>
            <span style="font-size:11px;color:#94a3b8;">CALL</span>
          </div>
          <div style="display:flex;align-items:center;gap:8px;margin-bottom:4px;">
            <div style="width:20px;height:1px;border-top:1px dashed #64748b;"></div>
            <span style="font-size:11px;color:#94a3b8;">COPY</span>
          </div>
        </div>
        <div style="color:#475569;font-size:10px;margin-top:8px;">Nodes sized by LOC · dashed boxes group by technology</div>`;
    } else if (this.viewMode === 'techmap') {
      panel.innerHTML = `
        <h4 style="margin:0 0 10px;color:#60a5fa;">Technology Map</h4>
        <div style="color:#94a3b8;font-size:12px;line-height:1.6;margin-bottom:12px;">
          High-level technology architecture showing major platform components and their relationships.
        </div>
        <div style="border-top:1px solid #334155;padding-top:8px;">
          <h4 style="margin:0 0 8px;color:#60a5fa;">Data Flows</h4>
          <div style="display:flex;align-items:center;gap:8px;margin-bottom:4px;">
            <div style="width:20px;height:3px;background:#22c55e;"></div>
            <span style="font-size:11px;color:#94a3b8;">CALL invocations</span>
          </div>
          <div style="display:flex;align-items:center;gap:8px;margin-bottom:4px;">
            <div style="width:20px;height:3px;background:#a855f7;"></div>
            <span style="font-size:11px;color:#94a3b8;">SQL queries</span>
          </div>
          <div style="display:flex;align-items:center;gap:8px;margin-bottom:4px;">
            <div style="width:20px;height:3px;background:#ef4444;"></div>
            <span style="font-size:11px;color:#94a3b8;">File I/O</span>
          </div>
          <div style="display:flex;align-items:center;gap:8px;margin-bottom:4px;">
            <div style="width:20px;height:2px;background:#64748b;"></div>
            <span style="font-size:11px;color:#94a3b8;">COPY references</span>
          </div>
        </div>
        <div style="color:#475569;font-size:10px;margin-top:12px;">Edge width = relative volume</div>`;
    } else if (this.viewMode === 'dependencies') {
      const c = this._classified || {};
      const a = this._depAnalytics || {};
      const callCount = (c.callEdges || []).length;
      const copyCount = (c.copyEdges || []).length;
      const programs = c.programs || [];
      const totalLOC = programs.reduce((s, p) => s + (p.lineCount || 0), 0);
      const avgCoupling = programs.length > 0 ? ((a.totalEdges || 0) * 2 / programs.length).toFixed(1) : 0;
      const hubs = (a.hubs || []).sort((x, y) => y.coupling - x.coupling).slice(0, 5);
      const violations = a.violations || [];
      const hotspots = (a.hotspots || []).sort((x, y) => (y.loc * y.coupling) - (x.loc * x.coupling)).slice(0, 5);
      const isolated = a.isolated || [];
      const healthScore = Math.max(0, 100 - (a.violationCount || 0) * 15 - isolated.length * 3 - (hubs.length > 3 ? 10 : 0));
      const healthColor = healthScore >= 70 ? '#10b981' : healthScore >= 40 ? '#f59e0b' : '#ef4444';

      panel.innerHTML = `
        <h4 style="margin:0 0 6px;color:#60a5fa;">Dependency Intelligence</h4>

        <div style="background:#1e293b;border-radius:6px;padding:10px;margin-bottom:10px;text-align:center;">
          <div style="font-size:28px;font-weight:800;color:${healthColor};">${healthScore}</div>
          <div style="font-size:10px;color:#64748b;text-transform:uppercase;letter-spacing:1px;">Architecture Health</div>
          <div style="margin-top:6px;height:4px;background:#0f172a;border-radius:2px;overflow:hidden;">
            <div style="height:100%;width:${healthScore}%;background:${healthColor};border-radius:2px;transition:width 0.5s;"></div>
          </div>
        </div>

        <div style="display:grid;grid-template-columns:1fr 1fr 1fr;gap:4px;margin-bottom:10px;">
          <div style="background:#1e293b;border-radius:4px;padding:5px;text-align:center;">
            <div style="font-size:15px;font-weight:700;color:#60a5fa;">${programs.length}</div>
            <div style="font-size:8px;color:#64748b;">PROGRAMS</div>
          </div>
          <div style="background:#1e293b;border-radius:4px;padding:5px;text-align:center;">
            <div style="font-size:15px;font-weight:700;color:#22c55e;">${callCount}</div>
            <div style="font-size:8px;color:#64748b;">CALL EDGES</div>
          </div>
          <div style="background:#1e293b;border-radius:4px;padding:5px;text-align:center;">
            <div style="font-size:15px;font-weight:700;color:#fbbf24;">${avgCoupling}</div>
            <div style="font-size:8px;color:#64748b;">AVG COUPLING</div>
          </div>
        </div>

        ${(a.violationCount || 0) > 0 ? `<div style="background:rgba(239,68,68,0.1);border:1px solid rgba(239,68,68,0.3);border-radius:6px;padding:8px;margin-bottom:10px;">
          <div style="display:flex;justify-content:space-between;align-items:center;margin-bottom:4px;">
            <span style="color:#fca5a5;font-size:11px;font-weight:700;">⚠️ ${a.violationCount} Layer Violations</span>
          </div>
          <div style="color:#94a3b8;font-size:9px;margin-bottom:6px;">Calls that skip architectural layers — breaks separation of concerns.</div>
          ${violations.slice(0, 3).map(v => `<div style="font-size:10px;color:#fca5a5;margin-bottom:2px;">
            <span style="color:#e2e8f0;">${v.from}</span> <span style="color:#ef4444;">→</span> <span style="color:#e2e8f0;">${v.to}</span>
            <span style="color:#64748b;">(${v.fromLayer} → ${v.toLayer})</span>
          </div>`).join('')}
          ${violations.length > 3 ? `<div style="color:#475569;font-size:9px;">+ ${violations.length - 3} more</div>` : ''}
        </div>` : `<div style="background:rgba(16,185,129,0.1);border:1px solid rgba(16,185,129,0.2);border-radius:6px;padding:8px;margin-bottom:10px;">
          <div style="color:#6ee7b7;font-size:11px;font-weight:600;">✅ No Layer Violations</div>
          <div style="color:#94a3b8;font-size:9px;">All CALL dependencies follow proper layer ordering.</div>
        </div>`}

        ${hubs.length > 0 ? `<div style="border-top:1px solid #334155;padding-top:8px;margin-bottom:8px;">
          <h4 style="margin:0 0 6px;color:#fbbf24;font-size:11px;">⭐ Hub Programs (high fan-in)</h4>
          <div style="color:#64748b;font-size:9px;margin-bottom:4px;">Called by 3+ programs — shared services, high migration impact.</div>
          ${hubs.map(h => `<div style="display:flex;justify-content:space-between;padding:3px 0;font-size:10px;cursor:pointer;" onclick="servicesView?.searchNode('${h.label}')">
            <span style="color:#e2e8f0;">${h.label}</span>
            <span style="color:#fbbf24;">${h.inCount} in · ${h.outCount} out</span>
          </div>`).join('')}
        </div>` : ''}

        ${hotspots.length > 0 ? `<div style="border-top:1px solid #334155;padding-top:8px;margin-bottom:8px;">
          <h4 style="margin:0 0 6px;color:#ef4444;font-size:11px;">🔥 Migration Hotspots</h4>
          <div style="color:#64748b;font-size:9px;margin-bottom:4px;">High LOC + high coupling = complex to migrate, ripple risk.</div>
          ${hotspots.map(h => `<div style="display:flex;justify-content:space-between;padding:3px 0;font-size:10px;cursor:pointer;" onclick="servicesView?.searchNode('${h.label}')">
            <span style="color:#e2e8f0;">${h.label}</span>
            <span style="color:#94a3b8;">${h.loc.toLocaleString()} LOC · ${h.coupling} deps</span>
          </div>`).join('')}
        </div>` : ''}

        ${isolated.length > 0 ? `<div style="border-top:1px solid #334155;padding-top:8px;margin-bottom:8px;">
          <h4 style="margin:0 0 6px;color:#64748b;font-size:11px;">🏝️ Isolated (${isolated.length})</h4>
          <div style="color:#64748b;font-size:9px;margin-bottom:4px;">No CALL dependencies — candidates for independent migration.</div>
          ${isolated.slice(0, 5).map(h => `<div style="font-size:10px;color:#94a3b8;padding:1px 0;cursor:pointer;" onclick="servicesView?.searchNode('${h.label}')">${h.label} <span style="color:#475569;">(${h.loc} LOC)</span></div>`).join('')}
          ${isolated.length > 5 ? `<div style="color:#475569;font-size:9px;">+ ${isolated.length - 5} more</div>` : ''}
        </div>` : ''}

        <div style="border-top:1px solid #334155;padding-top:8px;">
          <h4 style="margin:0 0 6px;color:#60a5fa;font-size:11px;">Edge Types</h4>
          <div style="display:flex;align-items:center;gap:6px;margin-bottom:3px;"><div style="width:14px;height:2px;background:rgba(34,197,94,0.5);"></div><span style="font-size:9px;color:#94a3b8;">Same-layer (${a.sameLayerCount || 0})</span></div>
          <div style="display:flex;align-items:center;gap:6px;margin-bottom:3px;"><div style="width:14px;height:2px;background:#f59e0b;"></div><span style="font-size:9px;color:#94a3b8;">Cross-layer (${(a.crossLayerCount || 0) - (a.violationCount || 0)})</span></div>
          <div style="display:flex;align-items:center;gap:6px;margin-bottom:3px;"><div style="width:14px;height:2px;background:#ef4444;border-top:2px dashed #ef4444;"></div><span style="font-size:9px;color:#ef4444;">Violation (${a.violationCount || 0})</span></div>
        </div>
        <div style="color:#475569;font-size:9px;margin-top:8px;">Node size = coupling × LOC · ⭐ border = hub · Click for details</div>`;
    } else if (this.viewMode === 'modules') {
      const ms = this._moduleStats || {};
      const mods = ms.modules || {};
      const modColor = ms.moduleColor || {};
      const modKeys = Object.keys(mods).sort((a, b) => (mods[b]?.count || 0) - (mods[a]?.count || 0));
      const totalCross = ms.totalCross || 0;
      const totalInternal = ms.totalInternal || 0;
      const couplingRatio = (totalCross + totalInternal) > 0 ? ((totalInternal / (totalCross + totalInternal)) * 100).toFixed(0) : 0;

      panel.innerHTML = `
        <h4 style="margin:0 0 6px;color:#60a5fa;">Module Map</h4>
        <div style="color:#94a3b8;font-size:10px;line-height:1.4;margin-bottom:10px;">
          Programs grouped by <b style="color:#cbd5e1;">naming prefix</b> (TLA).
          Each bubble = one module. Edges = cross-module CALL dependencies.
        </div>

        <div style="background:#1e293b;border-radius:6px;padding:10px;margin-bottom:10px;">
          <div style="display:flex;justify-content:space-between;margin-bottom:6px;">
            <span style="color:#94a3b8;font-size:11px;">Modularity Score</span>
            <span style="color:${parseInt(couplingRatio) > 60 ? '#10b981' : '#f59e0b'};font-size:14px;font-weight:700;">${couplingRatio}%</span>
          </div>
          <div style="height:6px;background:#0f172a;border-radius:3px;overflow:hidden;display:flex;">
            <div style="height:100%;width:${couplingRatio}%;background:#10b981;" title="Internal calls"></div>
            <div style="height:100%;width:${100 - parseInt(couplingRatio)}%;background:#f59e0b;" title="Cross-module calls"></div>
          </div>
          <div style="display:flex;justify-content:space-between;margin-top:4px;font-size:9px;color:#64748b;">
            <span>🟢 ${totalInternal} internal</span>
            <span>🟡 ${totalCross} cross-module</span>
          </div>
        </div>

        <div style="border-top:1px solid #334155;padding-top:8px;margin-bottom:8px;">
          <h4 style="margin:0 0 6px;color:#60a5fa;font-size:11px;">Detected Modules (${modKeys.length})</h4>
          ${modKeys.map(k => {
            const m = mods[k];
            const c = modColor[k] || '#64748b';
            return `<div style="display:flex;align-items:center;gap:6px;margin-bottom:4px;cursor:pointer;" onclick="servicesView?.searchNode('${k}')">
              <div style="width:10px;height:10px;border-radius:50%;background:${c};flex-shrink:0;"></div>
              <div style="flex:1;">
                <span style="font-size:11px;color:#e2e8f0;font-weight:600;">${k}</span>
                <span style="font-size:9px;color:#64748b;"> · ${m.count} pgm · ${m.loc.toLocaleString()} LOC</span>
              </div>
            </div>`;
          }).join('')}
        </div>

        <div style="border-top:1px solid #334155;padding-top:8px;">
          <h4 style="margin:0 0 4px;color:#60a5fa;font-size:11px;">Reading the Graph</h4>
          <div style="color:#94a3b8;font-size:9px;line-height:1.5;">
            <b style="color:#cbd5e1;">Bubble size</b> = lines of code in module<br>
            <b style="color:#fbbf24;">Gold arrows</b> = cross-module CALL dependencies<br>
            <b style="color:#cbd5e1;">Edge label</b> = number of CALL statements<br>
            <b style="color:#cbd5e1;">Thick edges</b> = tightly coupled modules<br><br>
            <b style="color:#10b981;">High modularity</b> = most calls stay internal<br>
            <b style="color:#f59e0b;">Low modularity</b> = many cross-module deps → harder to migrate independently
          </div>
        </div>
        <div style="color:#475569;font-size:9px;margin-top:8px;">Click module for details · Drag to rearrange</div>`;
    } else if (this.viewMode === 'deadcode') {
      const CAT = ServicesView.CATEGORY;
      panel.innerHTML = `
        <h4 style="margin:0 0 8px;color:#e2e8f0;font-size:13px;">Reachability Legend</h4>
        <div style="color:#94a3b8;font-size:10px;line-height:1.4;margin-bottom:10px;">
          How each <b style="color:#cbd5e1;">paragraph</b> (≈ function) can be reached during program execution.
        </div>
        ${Object.entries(CAT).map(([key, cat]) => `
          <div style="display:flex;align-items:flex-start;gap:8px;margin-bottom:8px;">
            <div style="width:14px;height:14px;border-radius:3px;background:${cat.bg};border:${key==='unreachable'?'2px dashed':'1px solid'} ${cat.border};flex-shrink:0;margin-top:1px;"></div>
            <div>
              <div style="font-size:11px;color:${cat.color};font-weight:600;">${cat.icon} ${cat.label}</div>
              <div style="font-size:10px;color:#64748b;">${cat.modernLabel}</div>
            </div>
          </div>
        `).join('')}
        <div style="border-top:1px solid #334155;margin:10px 0 8px;padding-top:8px;">
          <h4 style="margin:0 0 6px;color:#e2e8f0;font-size:12px;">How COBOL Executes</h4>
          <div style="color:#94a3b8;font-size:10px;line-height:1.5;">
            Unlike modern languages, COBOL runs paragraphs <b style="color:#cbd5e1;">top-to-bottom</b> by default.
            A paragraph doesn't need a PERFORM to run — it can execute simply by being next in order.
            <br><br>
            <b style="color:#f59e0b;">⚠️ "Unreachable" ≠ deletable.</b>
            External systems (JCL, CICS, CALL) may invoke code not visible to static analysis.
          </div>
        </div>
        <div style="color:#475569;font-size:10px;margin-top:8px;">Click any function for its analysis</div>`;
    }
  }

  // ── Stats bar ──
  _updateStats() {
    const hint = document.getElementById('services-stats');
    if (!hint || !this._classified) return;
    const { programs, copybooks, callEdges, copyEdges } = this._classified;
    const totalLOC = [...programs, ...copybooks].reduce((s, p) => s + (p.lineCount || 0), 0);
    hint.textContent = `${programs.length} programs · ${copybooks.length} copybooks · ${callEdges.length} CALL · ${copyEdges.length} COPY · ${(totalLOC/1000).toFixed(1)}K LOC`;
  }

  // ── Detail panel ──
  _showDetail(d, nodeId) {
    const panel = document.getElementById('services-detail');
    if (!panel) return;

    // Tech map nodes get a summary view
    if (d.type === 'technology') {
      panel.innerHTML = `
        <div style="border-left:3px solid #60a5fa;padding-left:10px;margin-bottom:10px;">
          <div style="font-size:16px;font-weight:700;color:#e2e8f0;">${d.id.replace('T_','')}</div>
          <div style="color:#60a5fa;font-size:12px;font-weight:600;">Technology Component</div>
        </div>
        <div style="color:#94a3b8;font-size:12px;margin-bottom:10px;">${d.desc || ''}</div>
        <div style="display:grid;grid-template-columns:1fr 1fr;gap:6px;">
          <div style="background:#1e293b;border-radius:4px;padding:6px;text-align:center;">
            <div style="font-size:18px;font-weight:700;color:#60a5fa;">${d.count || 0}</div>
            <div style="font-size:9px;color:#64748b;">COMPONENTS</div>
          </div>
          <div style="background:#1e293b;border-radius:4px;padding:6px;text-align:center;">
            <div style="font-size:18px;font-weight:700;color:#fbbf24;">${((d.lineCount||0)/1000).toFixed(1)}K</div>
            <div style="font-size:9px;color:#64748b;">LOC</div>
          </div>
        </div>`;
      return;
    }

    const label = (d.id || '').replace(/\.(cbl|cpy)$/i, '');
    const conns = this.network ? this.network.getConnectedNodes(nodeId).filter(c => !c.startsWith('__') && !c.startsWith('T_')) : [];
    const nd = this.nodesDS.get(nodeId);
    const layer = nd?._layer || 'unknown';
    const layerLabels = { presentation: 'Presentation', coordination: 'Coordination', business: 'Business Logic', batch: 'Batch', dataAccess: 'Data Access', persistence: 'Persistence', cics: 'CICS Online', db2: 'DB2 Access', logic: 'Business Rules', utility: 'Utilities', data: 'Data Structures' };
    const layerColors = { presentation: '#3b82f6', coordination: '#10b981', business: '#f59e0b', batch: '#8b5cf6', dataAccess: '#ef4444', persistence: '#78350f', cics: '#3b82f6', db2: '#a855f7', logic: '#f59e0b', utility: '#06b6d4', data: '#78716c' };
    const color = layerColors[layer] || '#64748b';

    let html = `
      <div style="border-left:3px solid ${color};padding-left:10px;margin-bottom:10px;">
        <div style="font-size:16px;font-weight:700;color:#e2e8f0;">${label}</div>
        <div style="color:${color};font-size:12px;font-weight:600;">${layerLabels[layer] || layer}</div>
      </div>
      <div style="display:grid;grid-template-columns:1fr 1fr;gap:6px;margin-bottom:10px;">
        <div style="background:#1e293b;border-radius:4px;padding:6px;text-align:center;">
          <div style="font-size:18px;font-weight:700;color:#60a5fa;">${d.lineCount || '?'}</div>
          <div style="font-size:9px;color:#64748b;">LINES</div>
        </div>
        <div style="background:#1e293b;border-radius:4px;padding:6px;text-align:center;">
          <div style="font-size:18px;font-weight:700;color:#fbbf24;">${conns.length}</div>
          <div style="font-size:9px;color:#64748b;">CONNECTIONS</div>
        </div>
      </div>`;

    if (d.hasAst) html += '<div style="color:#10b981;font-size:11px;margin-bottom:6px;">✅ AST parsed</div>';

    html += '<div style="border-top:1px solid #1e293b;padding-top:6px;margin-bottom:8px;">';
    if (d.sqlCount > 0) html += `<div style="color:#a855f7;font-size:11px;">🗃️ ${d.sqlCount} SQL</div>`;
    if (d.callCount > 0) html += `<div style="color:#10b981;font-size:11px;">📞 ${d.callCount} CALL</div>`;
    if (d.performCount > 0) html += `<div style="color:#06b6d4;font-size:11px;">🔄 ${d.performCount} PERFORM</div>`;
    if (d.displayCount > 0) html += `<div style="color:#f59e0b;font-size:11px;">🖥️ ${d.displayCount} DISPLAY</div>`;
    html += '</div>';

    if (conns.length > 0) {
      html += '<div style="border-top:1px solid #1e293b;padding-top:6px;margin-bottom:8px;">';
      html += '<div style="color:#94a3b8;font-size:10px;text-transform:uppercase;margin-bottom:4px;">Connected to</div>';
      for (const c of conns.slice(0, 10)) {
        const cLabel = c.replace(/\.(cbl|cpy)$/i, '');
        html += `<div style="color:#cbd5e1;font-size:11px;cursor:pointer;" onclick="servicesView?.searchNode('${cLabel}')">• ${cLabel}</div>`;
      }
      if (conns.length > 10) html += `<div style="color:#475569;font-size:10px;">+ ${conns.length - 10} more</div>`;
      html += '</div>';
    }

    html += `<div style="display:flex;flex-direction:column;gap:4px;">
      <button class="btn-small drill-btn" onclick="switchDashboard('controlflow');setTimeout(()=>controlFlowView?.selectFile('${(d.id||'').replace(/'/g,"\\'")}'),100)">⚡ Control Flow</button>
      <button class="btn-small drill-btn" onclick="astExplorer?.drillIntoProgram('${(d.id||'').replace(/'/g,"\\'")}')">🔬 AST Explorer</button>
    </div>`;
    panel.innerHTML = html;
  }

  searchNode(query) {
    if (!this.network || !this.nodesDS) return;
    if (!query) { this.network.fit(); return; }
    const q = query.toLowerCase();
    const match = this.nodesDS.get().find(n => (n.label || '').toLowerCase().includes(q));
    if (match) {
      this.network.focus(match.id, { scale: 1.5, animation: { duration: 500, easingFunction: 'easeInOutQuad' } });
      this.network.selectNodes([match.id]);
    }
  }

  refresh() { this.loadAndRender(); }
  zoomToFit() { if (this.network) this.network.fit({ animation: { duration: 500 } }); }
}
