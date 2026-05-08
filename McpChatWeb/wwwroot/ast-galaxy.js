// ═══════════════════════════════════════════════════════════════════════
// AST Galaxy View — Multi-file RAW AST graph (vis-network + 3D toggle)
// Shows ALL programs' AST nodes using the same visual style as the
// single-file AST Explorer RAW view: hierarchical layout, colored nodes,
// cubicBezier edges, navigation buttons. Programs shown as clusters.
// Toggle to 3D mode using 3d-force-graph for immersive exploration.
// ═══════════════════════════════════════════════════════════════════════

class ASTGalaxyView {
  constructor(containerId) {
    this.containerId = containerId;
    this.network = null;
    this.graph3d = null;
    this.is3D = false;
    this.isFullscreen = false;
    this.galaxyData = null;
    this.astData = null;
    this.viewMode = 'clustered';
    this.filter = 'all';
    this.showFilter = 'all';
    this.sortMode = 'name';
    this._searchTerm = '';
    this.nodes = null;
    this.edges = null;
    this._expandedClusters = new Set();
    this._3dSearchTerm = '';
    this._c4Level = 1;          // C4 model level: 1=Context, 2=Containers, 3=Components
    this._c4SelectedProg = null; // program selected for L3 drill
    this._bianShowCopybooks = false;
  }

  get _isBusinessMode() { return this.viewMode === 'business' || this.viewMode === 'business-expanded' || this.viewMode === 'service-catalog' || this.viewMode === 'service-catalog-expanded' || this.viewMode === 'service-catalog-expanded-3d' || this.viewMode === 'service-catalog-v2' || this.viewMode === 'service-catalog-v3'; }
  get _isServiceCatalogMode() { return this.viewMode === 'service-catalog' || this.viewMode === 'service-catalog-expanded' || this.viewMode === 'service-catalog-expanded-3d' || this.viewMode === 'service-catalog-v2' || this.viewMode === 'service-catalog-v3'; }
  get _isServiceCatalogExpanded() { return this.viewMode === 'service-catalog-expanded' || this.viewMode === 'service-catalog-expanded-3d'; }
  // Modes that render pure HTML — vis.js must be skipped for these
  get _isHtmlMode() { return this.viewMode === 'bian-matrix'; }

  // RAW AST view colors — exact match from ast-explorer.js
  static TYPE_COLORS = {
    SECTION: '#8b5cf6', PARAGRAPH: '#10b981', PARAGRAPH_NAME: '#10b981',
    SENTENCE: '#64748b', DIALECT: '#a855f7', DIALECT_CONTAINER: '#a855f7',
    MOVE: '#84cc16', PERFORM: '#06b6d4', IF_BRANCH: '#ec4899', EVALUATE: '#f59e0b',
    EXIT: '#475569', COMPUTE: '#f97316', DISPLAY: '#14b8a6', CALL: '#ef4444',
    PROCEDURE_DIVISION_BODY: '#3b82f6', PARAGRAPHS: '#334155',
    PROGRAM: '#3b82f6', COPYBOOK: '#f16667',
  };

  static HUMAN_TYPES = {
    DIALECT: 'SQL', DIALECT_CONTAINER: 'SQL', MOVE: 'MOVE', PERFORM: 'PERFORM',
    CALL: 'CALL', CallStatement: 'CALL', IF_BRANCH: 'IF', EVALUATE: 'EVALUATE',
    COMPUTE: 'COMPUTE', DISPLAY: 'DISPLAY', EXIT: 'EXIT', SECTION: 'SECTION',
    PARAGRAPH: 'PARA', PARAGRAPH_NAME: 'PARA', PARAGRAPHS: 'PARAGRAPHS',
    PROCEDURE_DIVISION_BODY: 'PROC DIV', PROGRAM: 'PROGRAM', COPYBOOK: 'COPYBOOK',
  };

  // ═══════════════════════════════════════════════════════════════════
  // 3d-force-graph compatibility shim
  // v1.73.x has no .onNodeDoubleClick chain method. We patch the factory
  // ONCE so existing chains (.onNodeClick(...).onNodeDoubleClick(...)) keep
  // working — the dbl-click is detected via click timing on top of onNodeClick.
  // ═══════════════════════════════════════════════════════════════════
  static _patchForceGraph3DDblClick() {
    if (typeof ForceGraph3D !== 'function') return;
    if (ForceGraph3D._dblClickPatched) return;
    const origFactory = ForceGraph3D;
    const wrappedFactory = function (...factoryArgs) {
      const instance = origFactory(...factoryArgs);
      const origMount = instance;
      // The factory returns a function-style instance; .onNodeDoubleClick
      // doesn't exist on the per-call instance until you call (container).
      // We lazily patch on first chain access by wrapping the resulting
      // graph-instance once mounted.
      const _origInvoke = function (container) {
        const graph = origMount(container);
        if (graph && !graph.onNodeDoubleClick) {
          let userClick = () => {};
          let dblHandler = null;
          let last = { id: null, t: 0 };
          const origOnNodeClick = graph.onNodeClick.bind(graph);
          graph.onNodeClick = (fn) => { userClick = (typeof fn === 'function') ? fn : (() => {}); return graph; };
          graph.onNodeDoubleClick = (fn) => { dblHandler = (typeof fn === 'function') ? fn : null; return graph; };
          origOnNodeClick(node => {
            const now = Date.now();
            const id = node && (node.id ?? node);
            if (dblHandler && last.id === id && now - last.t < 350) {
              last = { id: null, t: 0 };
              try { dblHandler(node); } catch (e) { console.warn('[dblclick handler]', e); }
              return;
            }
            last = { id, t: now };
            try { userClick(node); } catch (e) { console.warn('[click handler]', e); }
          });
        }
        return graph;
      };
      return _origInvoke;
    };
    wrappedFactory._dblClickPatched = true;
    // Preserve any other static properties from the original
    Object.assign(wrappedFactory, origFactory);
    wrappedFactory._dblClickPatched = true;
    window.ForceGraph3D = wrappedFactory;
  }

  // ═══════════════════════════════════════════════════════════════════
  // DATA LOADING
  // ═══════════════════════════════════════════════════════════════════

  async loadAndRender() {
    const container = document.getElementById(this.containerId);
    if (!container) return;
    container.innerHTML = '<div class="ast-loading">Loading AST Galaxy...</div>';

    try {
      // Build scan query parameter from global scan selector
      const scanId = typeof _currentScanRunId !== 'undefined' ? _currentScanRunId : 'latest';
      const scanParam = (scanId && scanId !== 'latest' && scanId !== 'all') ? `?scanRunId=${scanId}` : '';

      // Load both endpoints in parallel
      const [galaxyResp, astResp] = await Promise.all([
        fetch(`/api/graph/rekt/galaxy${scanParam}`),
        fetch(`/api/graph/rekt/galaxy-ast${scanParam}`),
      ]);

      if (galaxyResp.ok) this.galaxyData = await galaxyResp.json();
      if (astResp.ok) this.astData = await astResp.json();

      if (!this.astData?.nodes?.length && !this.galaxyData?.programs?.length) {
        container.innerHTML = '<div class="ast-empty">No AST data found.<code>./doctor.sh rekt-full</code></div>';
        return;
      }

      this._populateFileFilter();
      if (this._isHtmlMode) {
        this._renderHtmlMode(container);
      } else if (this.viewMode === 'c4-model') {
        this._buildC4VisData();
        this._renderVisNetworkInternal(container);
      } else if (this.viewMode === 'service-catalog-v3') {
        this._buildModernizationRadarVisData();
      } else if (this._isServiceCatalogMode) {
        this._buildServiceCatalogVisData();
      } else if (this._isBusinessMode) {
        this._buildBusinessDomainVisData();
      } else {
        this._buildVisData();
      }
      if (!this._isHtmlMode) {
        this._renderVisNetwork(container);
      }
      this._updateStatsBar();
      this._renderLegend();
    } catch (e) {
      console.error('Galaxy load error:', e);
      container.innerHTML = `<div class="ast-error">Error: ${e.message}</div>`;
    }
  }

  // ═══════════════════════════════════════════════════════════════════
  // BUILD VIS-NETWORK DATA — matching RAW AST view exactly
  // ═══════════════════════════════════════════════════════════════════

  // Layer assignments for CAST Imaging-style architecture view
  static LAYER = { PROGRAM: 0, COPYBOOK: 0, PROCEDURE_DIVISION_BODY: 1, SECTION: 2, PARAGRAPHS: 2, PARAGRAPH: 3, PARAGRAPH_NAME: 3, PERFORM: 4, CALL: 4, CallStatement: 4, DIALECT: 5, DIALECT_CONTAINER: 5, IF_BRANCH: 4, EVALUATE: 4, COMPUTE: 4, DISPLAY: 4, MOVE: 4, EXIT: 5 };
  static LAYER_LABEL = ['🖥️ Programs', '⚙️ Procedure Division', '📂 Sections', '📄 Paragraphs', '🔄 Statements', '💾 Data / SQL'];

  // Node visual config per type — CAST-style large circles with gradient fills
  static NODE_STYLE = {
    PROGRAM:     { shape: 'dot', size: 35, bg: '#3b82f6', border: '#60a5fa', icon: '🖥️' },
    COPYBOOK:    { shape: 'dot', size: 28, bg: '#f16667', border: '#fca5a5', icon: '📋' },
    PROCEDURE_DIVISION_BODY: { shape: 'dot', size: 22, bg: '#6366f1', border: '#a5b4fc', icon: '⚙️' },
    SECTION:     { shape: 'dot', size: 20, bg: '#8b5cf6', border: '#c4b5fd', icon: '📂' },
    PARAGRAPHS:  { shape: 'dot', size: 16, bg: '#334155', border: '#64748b', icon: '📑' },
    PARAGRAPH:   { shape: 'dot', size: 16, bg: '#10b981', border: '#6ee7b7', icon: '¶' },
    PARAGRAPH_NAME: { shape: 'dot', size: 14, bg: '#10b981', border: '#6ee7b7', icon: '¶' },
    PERFORM:     { shape: 'diamond', size: 14, bg: '#06b6d4', border: '#67e8f9', icon: '🔄' },
    CALL:        { shape: 'star', size: 16, bg: '#ef4444', border: '#fca5a5', icon: '📞' },
    CallStatement: { shape: 'star', size: 16, bg: '#ef4444', border: '#fca5a5', icon: '📞' },
    DIALECT:     { shape: 'triangle', size: 14, bg: '#a855f7', border: '#d8b4fe', icon: '🗄️' },
    DIALECT_CONTAINER: { shape: 'triangle', size: 14, bg: '#a855f7', border: '#d8b4fe', icon: '🗄️' },
    IF_BRANCH:   { shape: 'diamond', size: 12, bg: '#ec4899', border: '#f9a8d4', icon: '❓' },
    EVALUATE:    { shape: 'diamond', size: 12, bg: '#f59e0b', border: '#fcd34d', icon: '🔀' },
    COMPUTE:     { shape: 'dot', size: 10, bg: '#f97316', border: '#fdba74', icon: '🔢' },
    DISPLAY:     { shape: 'dot', size: 10, bg: '#14b8a6', border: '#5eead4', icon: '📺' },
    MOVE:        { shape: 'dot', size: 10, bg: '#84cc16', border: '#bef264', icon: '➡️' },
    EXIT:        { shape: 'dot', size: 8,  bg: '#475569', border: '#94a3b8', icon: '🚪' },
  };
  static DEFAULT_STYLE = { shape: 'dot', size: 10, bg: '#64748b', border: '#94a3b8', icon: '•' };

  // Edge visual config — CAST-style prominent colored lines
  static EDGE_STYLE = {
    CALL:        { color: '#ef4444', width: 3, label: 'CALL', dashes: false },
    COPY:        { color: '#3b82f6', width: 2, label: 'COPY', dashes: [10, 5] },
    CONTAINS:    { color: '#475569', width: 1, label: '', dashes: false },
    FOLLOWED_BY: { color: '#60a5fa', width: 2, label: '', dashes: false },
    JUMPS_TO:    { color: '#f59e0b', width: 2, label: 'GOTO', dashes: [6, 4] },
    DEPENDS_ON:  { color: '#10b981', width: 2, label: 'DEP', dashes: [8, 4] },
    PERFORM:     { color: '#06b6d4', width: 2, label: 'PERFORM', dashes: false },
  };
  static DEFAULT_EDGE = { color: '#475569', width: 1, label: '', dashes: false };

  _buildVisData() {
    const nodeList = [];
    const edgeList = [];
    const nodeIds = new Set();
    let edgeIdx = 0;

    // Group AST nodes by program + build O(1) lookup index
    const programNodes = new Map();
    const nodeIndex = new Map();
    if (this.astData?.nodes) {
      for (const n of this.astData.nodes) {
        if (!programNodes.has(n.program)) programNodes.set(n.program, []);
        programNodes.get(n.program).push(n);
        nodeIndex.set(n.id, n);
      }
    }

    const programs = this.galaxyData?.programs || [];
    const programMeta = new Map(programs.map(p => [p.program, p]));

    const filteredPrograms = this._applyShowFilter([...programs]);
    const filteredKeys = new Set(filteredPrograms.map(p => p.program));

    let programKeys = [...new Set([...programNodes.keys(), ...programMeta.keys()])];
    programKeys = programKeys.filter(k => filteredKeys.has(k));
    if (this.filter !== 'all') {
      programKeys = programKeys.filter(k => k === this.filter || k.includes(this.filter));
    }

    const STRUCTURAL_TYPES = new Set(['SECTION', 'PARAGRAPHS', 'PARAGRAPH', 'PARAGRAPH_NAME', 'PROCEDURE_DIVISION_BODY']);
    const KEY_STMT_TYPES = new Set(['PERFORM', 'CALL', 'CallStatement', 'IF_BRANCH', 'EVALUATE', 'DIALECT', 'DIALECT_CONTAINER']);
    // V2 trims further — only "communication" statements that create traceable
    // call/branch paths. Drops MOVE/COMPUTE/DISPLAY/EXIT noise that doesn't
    // contribute to a top-down communication trace.
    const V2_KEY_STMT_TYPES = new Set(['PERFORM', 'CALL', 'CallStatement', 'DIALECT', 'DIALECT_CONTAINER']);
    const isV2 = this.viewMode === 'expanded-v2';
    const KEY_TYPES_FOR_THIS_VIEW = isV2 ? V2_KEY_STMT_TYPES : KEY_STMT_TYPES;
    const MAX_STMTS_PER_PROG = isV2 ? 12 : 30;
    const isClustered = this.viewMode === 'clustered';
    const groupColors = {};

    const _style = (type) => ASTGalaxyView.NODE_STYLE[type] || ASTGalaxyView.DEFAULT_STYLE;
    const _eStyle = (type) => ASTGalaxyView.EDGE_STYLE[type] || ASTGalaxyView.DEFAULT_EDGE;

    for (const progKey of programKeys) {
      const meta = programMeta.get(progKey);
      const progNodes = programNodes.get(progKey) || [];
      const displayName = progKey.replace('flow-ast-', '').replace('.cbl', '');
      const isCopybook = meta?.isCopybook || false;
      const progNodeId = `prog__${progKey}`;
      const groupId = `grp_${displayName}`;
      const nodeType = isCopybook ? 'COPYBOOK' : 'PROGRAM';
      const style = _style(nodeType);

      groupColors[groupId] = {
        color: { background: 'rgba(30,41,59,0.15)', border: style.bg },
        font: { color: '#94a3b8' },
      };

      if (isClustered && !this._expandedClusters.has(progKey)) {
        const stmtCount = meta?.nodeCount || progNodes.length;
        const secCount = meta?.sectionCount || 0;
        const paraCount = meta?.paraCount || 0;
        const sqlCount = meta?.sqlCount || 0;
        const callCount = meta?.callCount || 0;

        // CAST-style: large circle with icon + metrics summary
        let label = `${style.icon} ${displayName}`;
        const metrics = [];
        if (secCount > 0) metrics.push(`${secCount} sec`);
        if (paraCount > 0) metrics.push(`${paraCount} para`);
        if (sqlCount > 0) metrics.push(`${sqlCount} SQL`);
        if (callCount > 0) metrics.push(`${callCount} CALL`);
        if (metrics.length > 0) label += `\n${metrics.join(' · ')}`;

        nodeList.push({
          id: progNodeId, label, group: groupId,
          level: ASTGalaxyView.LAYER[nodeType] || 0,
          title: `${nodeType}: ${displayName}\n${secCount} sections · ${paraCount} paragraphs · ${stmtCount} nodes\nSQL: ${sqlCount} · CALLs: ${callCount}\n\nDouble-click to expand`,
          color: { background: style.bg, border: style.border,
                   highlight: { background: '#fbbf24', border: '#f59e0b' },
                   hover: { background: style.border, border: '#e2e8f0' } },
          font: { color: '#e2e8f0', size: 13, multi: true, bold: { color: '#f8fafc' } },
          shape: style.shape, size: style.size + Math.min(15, Math.sqrt(stmtCount) * 0.6),
          borderWidth: 3, borderWidthSelected: 5, mass: 3,
          shadow: { enabled: true, color: style.bg, x: 0, y: 0, size: 12 },
          _data: { ...meta, program: progKey, nodeType, displayName },
        });
        nodeIds.add(progNodeId);
      } else {
        // Expanded program header — larger, glowing
        nodeList.push({
          id: progNodeId, group: groupId,
          label: `${style.icon} ${displayName}`,
          level: ASTGalaxyView.LAYER[nodeType] || 0,
          title: `${nodeType}: ${displayName}\nDouble-click to collapse`,
          color: { background: style.bg, border: style.border,
                   highlight: { background: '#fbbf24', border: '#f59e0b' },
                   hover: { background: style.border, border: '#e2e8f0' } },
          font: { color: '#f8fafc', size: 14, bold: true, multi: true },
          shape: style.shape, size: style.size + 5, borderWidth: 4, mass: 4,
          shadow: { enabled: true, color: style.bg, x: 0, y: 0, size: 18 },
          _data: { ...meta, program: progKey, nodeType, displayName },
        });
        nodeIds.add(progNodeId);

        let stmtAdded = 0;
        for (const n of progNodes) {
          if (nodeIds.has(n.id)) continue;
          const isStructural = STRUCTURAL_TYPES.has(n.nodeType);
          const isKeyStmt = KEY_TYPES_FOR_THIS_VIEW.has(n.nodeType);
          if (!isStructural && !isKeyStmt) continue;
          // V2: skip the redundant PARAGRAPH_NAME / PARAGRAPHS containers — the
          // PARAGRAPH itself already carries the name, so this halves the visual noise.
          if (isV2 && (n.nodeType === 'PARAGRAPH_NAME' || n.nodeType === 'PARAGRAPHS')) continue;
          if (isKeyStmt && stmtAdded >= MAX_STMTS_PER_PROG) continue;

          const cleanName = (n.name || '').replace(/.*\//, '').replace(/Context\/.*/, '');
          const typeLabel = ASTGalaxyView.HUMAN_TYPES[n.nodeType] || n.nodeType;
          const nStyle = _style(n.nodeType);
          const label = cleanName ? `${nStyle.icon} ${cleanName}` : `${nStyle.icon} ${typeLabel}`;

          nodeList.push({
            id: n.id, group: groupId,
            label,
            level: ASTGalaxyView.LAYER[n.nodeType] ?? 4,
            title: `${n.nodeType}: ${n.name || '—'}\nLines ${n.startLine}–${n.endLine}\nProgram: ${displayName}`,
            color: { background: nStyle.bg, border: nStyle.border,
                     highlight: { background: '#fbbf24', border: '#f59e0b' },
                     hover: { background: nStyle.border, border: '#e2e8f0' } },
            font: { color: '#e2e8f0', size: 11, multi: true },
            shape: nStyle.shape, size: nStyle.size,
            borderWidth: 2, mass: 1,
            shadow: { enabled: true, color: nStyle.bg, x: 0, y: 0, size: 6 },
            _data: { ...n, displayName: cleanName || typeLabel },
          });
          nodeIds.add(n.id);
          if (isKeyStmt) stmtAdded++;
        }

        // Intra-program edges with CAST-style labels
        if (this.astData?.edges) {
          for (const e of this.astData.edges) {
            if (e.source.startsWith('prog__') || e.target.startsWith('prog__')) continue;
            if (!nodeIds.has(e.source) || !nodeIds.has(e.target)) continue;
            const srcNode = nodeIndex.get(e.source);
            const tgtNode = nodeIndex.get(e.target);
            if (!srcNode || !tgtNode || srcNode.program !== progKey || tgtNode.program !== progKey) continue;

            const es = _eStyle(e.type);
            edgeList.push({
              id: `e_${edgeIdx++}`, from: e.source, to: e.target,
              arrows: { to: { enabled: true, scaleFactor: 0.6, type: 'arrow' } },
              label: es.label || undefined,
              font: es.label ? { color: es.color, size: 9, strokeWidth: 3, strokeColor: '#0f172a', align: 'top' } : undefined,
              color: { color: es.color, opacity: 0.7, highlight: '#fbbf24', hover: es.color },
              width: es.width, dashes: es.dashes || false,
              smooth: { type: 'curvedCW', roundness: 0.15 },
              _type: e.type,
            });
          }
        }

        // Connect program header → first structural child
        const firstChild = progNodes.find(n => STRUCTURAL_TYPES.has(n.nodeType) && nodeIds.has(n.id));
        if (firstChild) {
          edgeList.push({
            id: `e_${edgeIdx++}`, from: progNodeId, to: firstChild.id,
            arrows: { to: { enabled: true, scaleFactor: 0.5 } },
            color: { color: '#475569', opacity: 0.5 }, width: 1, _type: 'CONTAINS',
          });
        }
      }
    }

    // Inter-program edges (CALL/COPY) — CAST-style bright colored connections
    const addedEdges = new Set();
    const addInterEdge = (from, to, type) => {
      const key = `${from}→${to}`;
      if (addedEdges.has(key) || from === to || !nodeIds.has(from) || !nodeIds.has(to)) return;
      addedEdges.add(key);
      const es = _eStyle(type);
      edgeList.push({
        id: `e_${edgeIdx++}`, from, to,
        arrows: { to: { enabled: true, scaleFactor: 0.8, type: 'arrow' } },
        label: es.label || type,
        font: { color: es.color, size: 10, strokeWidth: 3, strokeColor: '#0f172a', align: 'horizontal' },
        color: { color: es.color, opacity: 0.9, highlight: '#fbbf24' },
        width: es.width + 1, dashes: es.dashes || false,
        smooth: { type: 'curvedCW', roundness: 0.2 },
        _type: type,
      });
    };

    if (this.astData?.edges) {
      for (const e of this.astData.edges) {
        if (e.source.startsWith('prog__') && e.target.startsWith('prog__')) {
          addInterEdge(e.source, e.target, e.type);
        }
      }
    }
    if (this.galaxyData?.edges) {
      for (const e of this.galaxyData.edges) {
        const srcId = `prog__${e.source}`;
        const tgtId = `prog__${e.target}`;
        const srcMatch = nodeList.find(n => n.id === srcId || n._data?.displayName === e.source?.replace('.cbl',''));
        const tgtMatch = nodeList.find(n => n.id === tgtId || n._data?.displayName === e.target?.replace('.cbl',''));
        if (srcMatch && tgtMatch) addInterEdge(srcMatch.id, tgtMatch.id, e.type);
      }
    }

    this.nodes = new vis.DataSet(nodeList);
    this.edges = new vis.DataSet(edgeList);
    this._groupColors = groupColors;
  }

  // ═══════════════════════════════════════════════════════════════════
  // BUSINESS DOMAIN 2D — vis-network grouped by business service
  // ═══════════════════════════════════════════════════════════════════

  _buildBusinessDomainVisData() {
    const nodeList = [];
    const edgeList = [];
    const nodeIds = new Set();
    let edgeIdx = 0;

    const programs = this.galaxyData?.programs || [];
    const seen = new Set();
    const uniqueProgs = [];
    for (const p of programs) {
      if (seen.has(p.program)) continue;
      seen.add(p.program);
      uniqueProgs.push(p);
    }

    // Apply filters
    let filtered = this._applyShowFilter([...uniqueProgs]);
    if (this.filter !== 'all') {
      filtered = filtered.filter(p => p.program === this.filter || p.program.includes(this.filter));
    }
    filtered = this._applySortMode(filtered);

    // Classify into domains
    const domainMap = new Map();
    for (const p of filtered) {
      const domain = this._classifyBusinessDomain(p.program, p);
      if (!domainMap.has(domain)) domainMap.set(domain, []);
      domainMap.get(domain).push(p);
    }

    const groupColors = {};
    const domainKeys = [...domainMap.keys()];

    // Create domain hub nodes + member programs
    for (let di = 0; di < domainKeys.length; di++) {
      const domain = domainKeys[di];
      const cfg = ASTGalaxyView.BUSINESS_DOMAINS[domain] || { color: '#64748b', icon: '•' };
      const members = domainMap.get(domain);
      const hubId = `domain__${domain.replace(/\s+/g, '_')}`;
      const groupId = `bgrp_${domain.replace(/\s+/g, '_')}`;

      groupColors[groupId] = {
        color: { background: 'rgba(30,41,59,0.1)', border: cfg.color },
        font: { color: '#94a3b8' },
      };

      // Domain hub — large circle
      const totalSQL = members.reduce((s, p) => s + (p.sqlCount || 0), 0);
      const totalCALL = members.reduce((s, p) => s + (p.callCount || 0), 0);
      const totalNodes = members.reduce((s, p) => s + (p.nodeCount || 0), 0);

      nodeList.push({
        id: hubId, group: groupId,
        label: `${cfg.icon} ${domain}\n${members.length} programs`,
        level: 0,
        title: `${cfg.icon} ${domain}\n${members.length} programs\n${totalSQL} SQL · ${totalCALL} CALLs · ${totalNodes} AST nodes\n\nDouble-click to expand/collapse`,
        color: { background: cfg.color, border: cfg.color,
                 highlight: { background: '#fbbf24', border: '#f59e0b' },
                 hover: { background: cfg.color, border: '#e2e8f0' } },
        font: { color: '#f8fafc', size: 14, multi: true, bold: { color: '#f8fafc' } },
        shape: 'dot', size: 35 + Math.sqrt(members.length) * 5,
        borderWidth: 4, borderWidthSelected: 6, mass: 5,
        shadow: { enabled: true, color: cfg.color, x: 0, y: 0, size: 20 },
        _data: { nodeType: 'DOMAIN', displayName: domain, domain, programCount: members.length },
      });
      nodeIds.add(hubId);

      // Member programs
      const isExpanded = this._expandedClusters.has(domain);
      if (isExpanded) {
        for (const p of members) {
          const progId = `prog__${p.program}`;
          if (nodeIds.has(progId)) continue;
          const displayName = p.program.replace('flow-ast-', '').replace('.cbl', '');
          const isCopybook = p.isCopybook || false;
          const style = ASTGalaxyView.NODE_STYLE[isCopybook ? 'COPYBOOK' : 'PROGRAM'] || ASTGalaxyView.DEFAULT_STYLE;
          const sqlCount = p.sqlCount || 0;
          const callCount = p.callCount || 0;
          const metrics = [];
          if (sqlCount > 0) metrics.push(`${sqlCount} SQL`);
          if (callCount > 0) metrics.push(`${callCount} CALL`);
          if (p.sectionCount > 0) metrics.push(`${p.sectionCount} sec`);

          let label = `${style.icon} ${displayName}`;
          if (metrics.length) label += `\n${metrics.join(' · ')}`;

          nodeList.push({
            id: progId, group: groupId,
            label,
            level: 1,
            title: `${displayName}\nDomain: ${domain}\nSQL: ${sqlCount} · CALLs: ${callCount}\nSections: ${p.sectionCount || 0}\n\nDouble-click → AST Explorer`,
            color: { background: style.bg, border: style.border,
                     highlight: { background: '#fbbf24', border: '#f59e0b' },
                     hover: { background: style.border, border: '#e2e8f0' } },
            font: { color: '#e2e8f0', size: 11, multi: true },
            shape: style.shape,
            size: style.size + Math.min(10, Math.sqrt(p.nodeCount || 0) * 0.4),
            borderWidth: 2, mass: 2,
            shadow: { enabled: true, color: style.bg, x: 0, y: 0, size: 8 },
            _data: { ...p, program: p.program, nodeType: isCopybook ? 'COPYBOOK' : 'PROGRAM', displayName, domain },
          });
          nodeIds.add(progId);

          // Hub → member edge
          edgeList.push({
            id: `e_${edgeIdx++}`, from: hubId, to: progId,
            color: { color: cfg.color, opacity: 0.2 },
            width: 1, arrows: '', dashes: false,
            smooth: { type: 'curvedCW', roundness: 0.1 },
            _type: 'CLUSTER',
          });
        }
      }
    }

    // Inter-domain edges (aggregate CALL/COPY between domains)
    const domainEdges = new Map(); // "domA→domB" → { call: N, copy: N }
    if (this.galaxyData?.edges) {
      for (const e of this.galaxyData.edges) {
        const srcProg = uniqueProgs.find(p =>
          p.program === e.source || p.program.replace('flow-ast-','').replace('.cbl','') === e.source?.replace('.cbl',''));
        const tgtProg = uniqueProgs.find(p =>
          p.program === e.target || p.program.replace('flow-ast-','').replace('.cbl','') === e.target?.replace('.cbl',''));
        if (!srcProg || !tgtProg) continue;
        const srcDomain = this._classifyBusinessDomain(srcProg.program, srcProg);
        const tgtDomain = this._classifyBusinessDomain(tgtProg.program, tgtProg);

        // Inter-program edges (within or across domains)
        const srcId = `prog__${srcProg.program}`;
        const tgtId = `prog__${tgtProg.program}`;
        if (nodeIds.has(srcId) && nodeIds.has(tgtId) && srcId !== tgtId) {
          const es = ASTGalaxyView.EDGE_STYLE[e.type] || ASTGalaxyView.DEFAULT_EDGE;
          const isCross = srcDomain !== tgtDomain;
          edgeList.push({
            id: `e_${edgeIdx++}`, from: srcId, to: tgtId,
            arrows: { to: { enabled: true, scaleFactor: 0.7 } },
            label: es.label || e.type,
            font: { color: isCross ? '#f59e0b' : es.color, size: 10, strokeWidth: 3, strokeColor: '#0f172a' },
            color: { color: isCross ? '#f59e0b' : es.color, opacity: isCross ? 0.9 : 0.7 },
            width: isCross ? 3 : es.width,
            dashes: es.dashes || false,
            smooth: { type: 'curvedCW', roundness: 0.2 },
            _type: e.type,
          });
        }

        // Aggregate cross-domain edges for hub-to-hub connections
        if (srcDomain !== tgtDomain) {
          const key = `${srcDomain}→${tgtDomain}`;
          if (!domainEdges.has(key)) domainEdges.set(key, { call: 0, copy: 0 });
          const counts = domainEdges.get(key);
          if (e.type === 'CALL') counts.call++;
          else if (e.type === 'COPY') counts.copy++;
        }
      }
    }

    // Hub-to-hub cross-domain edges
    for (const [key, counts] of domainEdges) {
      const [srcDomain, tgtDomain] = key.split('→');
      const srcHub = `domain__${srcDomain.replace(/\s+/g, '_')}`;
      const tgtHub = `domain__${tgtDomain.replace(/\s+/g, '_')}`;
      if (!nodeIds.has(srcHub) || !nodeIds.has(tgtHub)) continue;
      const total = counts.call + counts.copy;
      const label = [];
      if (counts.call > 0) label.push(`${counts.call} CALL`);
      if (counts.copy > 0) label.push(`${counts.copy} COPY`);

      edgeList.push({
        id: `e_${edgeIdx++}`, from: srcHub, to: tgtHub,
        arrows: { to: { enabled: true, scaleFactor: 1.0, type: 'arrow' } },
        label: label.join(' · '),
        font: { color: '#f59e0b', size: 11, strokeWidth: 3, strokeColor: '#0f172a', bold: true },
        color: { color: '#f59e0b', opacity: 0.85, highlight: '#fbbf24' },
        width: Math.min(6, 2 + total * 0.5),
        smooth: { type: 'curvedCW', roundness: 0.25 },
        _type: 'CROSS_DOMAIN',
      });
    }

    this.nodes = new vis.DataSet(nodeList);
    this.edges = new vis.DataSet(edgeList);
    this._groupColors = groupColors;
  }

  // ═══════════════════════════════════════════════════════════════════
  // VIS-NETWORK RENDERER — identical style to RAW AST view
  // ═══════════════════════════════════════════════════════════════════

  // ═══════════════════════════════════════════════════════════════════
  // EXPANDED V2 — manual swim-lane layout
  //
  // Each program gets its own NORTH-SOUTH column (swim lane). Within a
  // lane, nodes stack strictly by their AST layer (Program at the top,
  // SQL/CALL statements at the bottom). Inter-program CALL/COPY edges
  // are kept but they don't drive layout — they ride OVER the lanes as
  // discrete arched arrows so you can trace communication without the
  // diagram collapsing into spaghetti.
  // ═══════════════════════════════════════════════════════════════════

  _applyV2SwimLaneLayout() {
    if (!this.nodes || !this.edges) return;
    const all = this.nodes.get();

    // 1) Bucket nodes by program
    const lanes = new Map();
    for (const n of all) {
      const prog = n._data?.program || (n.id?.startsWith('prog__') ? n.id.replace('prog__', '') : 'unknown');
      if (!lanes.has(prog)) lanes.set(prog, []);
      lanes.get(prog).push(n);
    }

    // 2) Sort lanes by connection count (most-connected leftmost) so users
    //    see the hub programs first when reading left → right.
    const progMeta = new Map((this.galaxyData?.programs || []).map(p => [p.program, p]));
    const conn = (prog) => (progMeta.get(prog)?.callCount || 0) + (progMeta.get(prog)?.sqlCount || 0);
    const laneOrder = [...lanes.keys()].sort((a, b) => conn(b) - conn(a));

    // 3) Layout constants
    const LANE_W   = 320;   // horizontal gap between programs
    const LAYER_H  = 110;   // vertical gap between AST layers
    const NODE_GAP = 26;    // vertical spacing for siblings on the same layer
    const TOP_PAD  = 40;
    const SIDE_PAD = 40;

    // 4) Per-lane positioning
    const updates = [];
    let laneIdx = 0;
    for (const prog of laneOrder) {
      const items = lanes.get(prog);
      const cx = SIDE_PAD + laneIdx * LANE_W + LANE_W / 2;

      // Group nodes within this lane by their LAYER (Programs at top → SQL at bottom)
      const byLayer = new Map();
      for (const n of items) {
        const layer = ASTGalaxyView.LAYER[n._data?.nodeType] ?? 4;
        if (!byLayer.has(layer)) byLayer.set(layer, []);
        byLayer.get(layer).push(n);
      }

      // Sort layers ascending so 0 (Program) is at top, 5 (SQL) at bottom
      const sortedLayers = [...byLayer.keys()].sort((a, b) => a - b);
      let cursorY = TOP_PAD;
      for (const layer of sortedLayers) {
        const layerNodes = byLayer.get(layer);
        // Sort siblings by start line for predictable top-down reading
        layerNodes.sort((a, b) => (a._data?.startLine || 0) - (b._data?.startLine || 0));

        // Spread siblings horizontally within the lane
        const colCount = Math.min(layerNodes.length, 4);
        const subColW = LANE_W * 0.78 / colCount;
        for (let i = 0; i < layerNodes.length; i++) {
          const n = layerNodes[i];
          const col = i % colCount;
          const row = Math.floor(i / colCount);
          const xOff = (col - (colCount - 1) / 2) * subColW;
          const yOff = row * NODE_GAP;
          updates.push({
            id: n.id,
            x: cx + xOff,
            y: cursorY + yOff,
            fixed: { x: true, y: true },
            level: undefined, // remove any prior hierarchical level hint
          });
        }
        const rowsOnThisLayer = Math.ceil(layerNodes.length / colCount);
        cursorY += LAYER_H + (rowsOnThisLayer - 1) * NODE_GAP;
      }
      laneIdx++;
    }
    this.nodes.update(updates);

    // Stash layout extents for the post-render lane-header overlay
    this._v2Layout = {
      lanes: laneOrder.map((prog, i) => ({
        prog,
        cx: SIDE_PAD + i * LANE_W + LANE_W / 2,
        displayName: prog.replace(/^flow-ast-/, '').replace(/\.cbl$/i, ''),
      })),
      width: SIDE_PAD * 2 + laneOrder.length * LANE_W,
    };
  }

  _renderV2Network(container) {
    const totalNodes = this.nodes.length;

    this.network = new vis.Network(container, { nodes: this.nodes, edges: this.edges }, {
      nodes: {
        borderWidth: 2,
        shadow: { enabled: true, color: 'rgba(0,0,0,0.3)', size: 8, x: 0, y: 2 },
        font: { multi: true, color: '#e2e8f0' },
      },
      edges: {
        // Vertical north-south flow for intra-lane edges; arched for cross-lane
        smooth: { type: 'cubicBezier', forceDirection: 'vertical', roundness: 0.3 },
        font: { size: 9, strokeWidth: 3, strokeColor: '#0f172a' },
        chosen: { edge: (values) => { values.width = values.width * 1.5; values.opacity = 1; } },
      },
      // No physics, no auto-layout — we placed nodes manually
      layout: { hierarchical: { enabled: false }, improvedLayout: false, randomSeed: 1 },
      physics: { enabled: false },
      interaction: {
        hover: true, tooltipDelay: 150,
        navigationButtons: true, keyboard: true,
        zoomView: true, dragView: true,
        multiselect: true,
      },
      groups: this._groupColors || {},
    });

    // Lane-header overlay — labels each swim lane at the top of the canvas.
    // Drawn on a layer above the network, fades when zoomed far out.
    this._renderV2LaneHeaders(container);

    // Inspector wiring — shared with the standard 2D path
    this.network.on('click', (params) => {
      if (params.nodes.length > 0) {
        const nd = this.nodes.get(params.nodes[0]);
        if (nd?._data) {
          this._updateInspector(nd._data);
          this._updateSourcePanel(nd._data);
        }
      }
    });
    this.network.on('doubleClick', (params) => {
      if (params.nodes.length > 0) {
        const nodeId = params.nodes[0];
        const nd = this.nodes.get(nodeId);
        if (nd?._data?.program && typeof astExplorer !== 'undefined' && astExplorer) {
          astExplorer.drillIntoProgram(nd._data.program);
        }
      }
    });
    this.network.on('zoom', () => this._renderV2LaneHeaders(container));
    this.network.on('dragEnd', () => this._renderV2LaneHeaders(container));

    // Stats bar
    const statsEl = document.getElementById('galaxy-stats-bar');
    if (statsEl) statsEl.innerHTML = `<span class="ast-stat">🔽 Top-Down · ${this._v2Layout?.lanes?.length || 0} lanes · ${totalNodes} nodes</span>`;
  }

  // Floating lane-header banners that track each swim-lane's current
  // screen X position so they stay above their column when you pan/zoom.
  _renderV2LaneHeaders(container) {
    if (!this._v2Layout || !this.network) return;
    let bar = container.querySelector('.galaxy-v2-lanes');
    if (!bar) {
      bar = document.createElement('div');
      bar.className = 'galaxy-v2-lanes';
      bar.style.cssText = 'position:absolute;top:0;left:0;right:0;height:32px;z-index:10;pointer-events:none;overflow:hidden;background:linear-gradient(180deg,rgba(3,7,18,0.85),rgba(3,7,18,0));';
      container.appendChild(bar);
    }
    bar.innerHTML = '';
    for (const lane of this._v2Layout.lanes) {
      const screenPos = this.network.canvasToDOM({ x: lane.cx, y: 0 });
      // Skip lanes off-screen
      if (screenPos.x < -120 || screenPos.x > container.offsetWidth + 120) continue;
      const label = document.createElement('div');
      label.style.cssText = `position:absolute;left:${screenPos.x}px;top:6px;transform:translateX(-50%);
        background:rgba(15,23,42,0.92);color:#60a5fa;border:1px solid #334155;border-radius:4px;
        padding:3px 10px;font-size:11px;font-weight:700;white-space:nowrap;
        box-shadow:0 2px 8px rgba(0,0,0,0.4);max-width:180px;overflow:hidden;text-overflow:ellipsis;`;
      label.textContent = lane.displayName;
      label.title = lane.prog;
      bar.appendChild(label);
    }
  }

  // ═══════════════════════════════════════════════════════════════════
  // PROGRAM MAP — north-to-south 2D view
  // Each program is a column. Its direct dependencies (copybooks it
  // COPYs, programs it CALLs) are stacked vertically below it.
  // Shared dependencies shared by multiple programs are deduplicated
  // and placed in the column of their first (alphabetically) caller.
  // ═══════════════════════════════════════════════════════════════════

  _buildProgramMapVisData() {
    const allPrograms = this._getSortedPrograms();           // filtered + sorted
    const edges      = this.galaxyData?.edges || [];

    const COL_W   = 220;  // px between program columns
    const ROW_H   = 90;   // px between rows

    // Build adjacency: source → [targets] (using normalised names)
    const norm    = s => (s || '').replace(/\.cbl$/i, '').replace(/\.cpy$/i, '').replace(/^flow-ast-/, '').toUpperCase();

    // Deduplicate by normalised name — prefer entries with AST data (nodeCount > 0)
    const dedupMap = new Map();
    for (const p of allPrograms) {
      const key = norm(p.program);
      const cur = dedupMap.get(key);
      if (!cur || (p.nodeCount || 0) > (cur.nodeCount || 0)) dedupMap.set(key, p);
    }
    const deduped = [...dedupMap.values()];
    const progSet = new Set(deduped.map(p => norm(p.program)));

    // For each program, collect its direct service dependencies
    const depsOf  = new Map();  // normName → Set<normName>
    for (const e of edges) {
      const src = norm(e.source), tgt = norm(e.target);
      if (!depsOf.has(src)) depsOf.set(src, new Set());
      depsOf.get(src).add(tgt);
    }

    // Sort programs A-Z for left→right column order
    const sorted = [...deduped].sort((a, b) => norm(a.program).localeCompare(norm(b.program)));

    // Assign each dependency to the leftmost (first alphabetically) caller column
    // so shared copybooks appear only once in the visual
    const depAssigned = new Map();  // normDep → colIndex
    for (let col = 0; col < sorted.length; col++) {
      const pn = norm(sorted[col].program);
      for (const dep of (depsOf.get(pn) || [])) {
        if (!depAssigned.has(dep)) depAssigned.set(dep, col);
      }
    }

    const nodeList = [];
    const edgeList = [];
    const nodeIds  = new Set();

    const _progMeta = name => deduped.find(p => norm(p.program) === name) || null;

    // ── Program header nodes (row 0) ──
    for (let col = 0; col < sorted.length; col++) {
      const p       = sorted[col];
      const pn      = norm(p.program);
      const display = pn.replace(/^FLOW-AST-/, '');
      const isCopy  = p.isCopybook || false;
      const style   = ASTGalaxyView.NODE_STYLE[isCopy ? 'COPYBOOK' : 'PROGRAM'] || ASTGalaxyView.DEFAULT_STYLE;
      const depCount = (depsOf.get(pn) || new Set()).size;

      nodeList.push({
        id:    'pm_prog__' + pn,
        label: display,
        x:     col * COL_W,
        y:     0,
        fixed: { x: true, y: true },
        level: 0,
        shape: style.shape || 'box',
        color: { background: style.color, border: style.border || style.color, highlight: { background: style.color, border: '#fff' } },
        font:  { color: '#e2e8f0', size: 12, bold: true },
        size:  28,
        title: `${isCopy ? 'Copybook' : 'Program'}: ${display}\nLOC: ${p.lineCount || 0} · Sections: ${p.sectionCount || 0} · Paragraphs: ${p.paraCount || 0}\nSQL: ${p.sqlCount || 0} · CALLs: ${p.callCount || 0}\nDirect dependencies: ${depCount}`,
        _data: { ...p, program: p.program, nodeType: isCopy ? 'COPYBOOK' : 'PROGRAM', displayName: display },
      });
      nodeIds.add('pm_prog__' + pn);
    }

    // ── Dependency nodes (rows 1+) per assigned column ──
    // Group deps by assigned column, then sort within each column
    const colDeps = new Map();  // colIndex → [{normName, type}]
    for (const [dep, col] of depAssigned) {
      if (!colDeps.has(col)) colDeps.set(col, []);
      colDeps.get(col).push(dep);
    }

    for (const [col, deps] of colDeps) {
      const sortedDeps = [...deps].sort();
      sortedDeps.forEach((dep, rowIdx) => {
        const nodeId = 'pm_dep__' + dep;
        if (nodeIds.has(nodeId)) return;
        nodeIds.add(nodeId);

        const meta     = _progMeta(dep);
        const isCopy   = dep.endsWith('.CPY') || dep.endsWith('.cpy') || (meta?.isCopybook) || !progSet.has(dep);
        const display  = dep.replace(/^FLOW-AST-/, '');
        const style    = ASTGalaxyView.NODE_STYLE[isCopy ? 'COPYBOOK' : 'PROGRAM'] || ASTGalaxyView.DEFAULT_STYLE;

        nodeList.push({
          id:    nodeId,
          label: display,
          x:     col * COL_W,
          y:     (rowIdx + 1) * ROW_H,
          fixed: { x: true, y: true },
          level: 1,
          shape: style.shape || 'ellipse',
          color: { background: style.color + 'cc', border: style.border || style.color, highlight: { background: style.color, border: '#fff' } },
          font:  { color: '#cbd5e1', size: 11 },
          size:  20,
          title: `${isCopy ? 'Copybook' : 'Program'}: ${display}${meta ? `\nLOC: ${meta.lineCount || 0}` : '\n(not parsed — inferred from dependency edges)'}`,
          _data: { ...(meta || {}), program: dep, nodeType: isCopy ? 'COPYBOOK' : 'PROGRAM', displayName: display },
        });
      });
    }

    // ── Edges: program → dependency ──
    for (let col = 0; col < sorted.length; col++) {
      const pn = norm(sorted[col].program);
      for (const dep of (depsOf.get(pn) || [])) {
        const srcId = 'pm_prog__' + pn;
        const tgtId = 'pm_dep__' + dep;
        if (!nodeIds.has(tgtId)) continue;
        const isCross = depAssigned.get(dep) !== col;  // dep owned by another column
        edgeList.push({
          id:     `pm_e__${pn}__${dep}`,
          from:   srcId,
          to:     tgtId,
          dashes: isCross,
          color:  { color: isCross ? '#f59e0b' : '#475569', opacity: isCross ? 0.7 : 0.5 },
          width:  isCross ? 1 : 1.5,
          arrows: { to: { enabled: true, scaleFactor: 0.6 } },
          smooth: { type: 'curvedCW', roundness: isCross ? 0.3 : 0.1 },
          title:  `${pn} → ${dep}`,
        });
      }
    }

    this.nodes = new vis.DataSet(nodeList);
    this.edges = new vis.DataSet(edgeList);
    this._pmLayout = { sorted };  // stash for overlay headers
  }

  _renderVisNetwork(container) {
    if (this._isHtmlMode) return; // HTML modes render directly, not via vis.js
    if (this.viewMode === 'program-map') {
      // program-map uses the same swim-lane render as expanded-v2
      this.viewMode = 'expanded-v2';
      const result = this._renderVisNetworkInternal(container);
      this.viewMode = 'program-map';
      return result;
    }
    return this._renderVisNetworkInternal(container);
  }

  _renderProgramMapNetwork(container) {
    if (this.network) { this.network.destroy(); this.network = null; }
    container.innerHTML = '';

    const COL_W = 220;

    this.network = new vis.Network(container, { nodes: this.nodes, edges: this.edges }, {
      nodes: {
        borderWidth: 2,
        shadow: { enabled: true, color: 'rgba(0,0,0,0.4)', size: 8, x: 0, y: 2 },
        font: { multi: false },
      },
      edges: {
        smooth: { type: 'curvedCW', roundness: 0.15 },
        font: { size: 0 },
      },
      interaction: {
        hover: true,
        tooltipDelay: 150,
        navigationButtons: false,
        keyboard: { enabled: true, bindToWindow: false },
      },
      layout: { hierarchical: false },
      physics: { enabled: false },  // positions are pre-computed
    });

    // Column header overlay — one label per program at the top
    const sorted = this._pmLayout?.sorted || [];
    const overlay = document.createElement('div');
    overlay.style.cssText = 'position:absolute;top:0;left:0;pointer-events:none;z-index:10;width:100%;';

    const updateOverlay = () => {
      overlay.innerHTML = '';
      if (!this.network) return;
      for (let col = 0; col < sorted.length; col++) {
        const p       = sorted[col];
        const pn      = (p.program || '').replace(/\.cbl$/i,'').replace(/^flow-ast-/i,'').toUpperCase();
        const display = pn.replace(/^FLOW-AST-/, '');
        const pos     = this.network.canvasToDOM(this.network.getPosition('pm_prog__' + pn));
        if (!pos) continue;
        const lbl = document.createElement('div');
        lbl.style.cssText = `position:absolute;left:${pos.x - 50}px;top:4px;width:100px;text-align:center;font-size:10px;color:#64748b;pointer-events:none;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;`;
        lbl.title = display;
        lbl.textContent = display;
        overlay.appendChild(lbl);
      }
    };

    container.style.position = 'relative';
    container.appendChild(overlay);
    this.network.once('afterDrawing', () => {
      this.network.fit({ animation: { duration: 400 } });
      setTimeout(updateOverlay, 420);
    });
    this.network.on('zoom', updateOverlay);
    this.network.on('dragEnd', updateOverlay);

    // Click → inspector
    this.network.on('click', (params) => {
      if (params.nodes.length > 0) {
        const nd = this.nodes.get(params.nodes[0]);
        if (nd?._data) {
          this._updateInspector(nd._data);
          this._updateSourcePanel(nd._data);
        }
      }
    });

    // Double-click → open in AST Explorer
    this.network.on('doubleClick', (params) => {
      if (params.nodes.length > 0) {
        const nd = this.nodes.get(params.nodes[0]);
        if (nd?._data?.program && typeof astExplorer !== 'undefined' && astExplorer) {
          astExplorer.drillIntoProgram(nd._data.program);
        }
      }
    });

    this.network.on('hoverNode', () => { container.style.cursor = 'pointer'; });
    this.network.on('blurNode',  () => { container.style.cursor = 'default'; });
  }

  _renderVisNetworkInternal(container) {
    if (this.network) { this.network.destroy(); this.network = null; }
    container.innerHTML = '';

    const totalNodes = this.nodes.length;
    const isV2 = this.viewMode === 'expanded-v2';

    // ── EXPANDED V2 — manual swim-lane positioning ───────────────────
    // Each program is its own vertical north-south column. Nodes stack
    // strictly by AST layer. Inter-program edges are kept (they show
    // CALL/COPY across columns) but they don't drive layout, so the
    // lanes stay parallel and trivial to follow with the eye.
    if (isV2) {
      this._applyV2SwimLaneLayout();
      return this._renderV2Network(container);
    }

    const hasExpanded = this._expandedClusters.size > 0 || this.viewMode === 'expanded';

    // CAST-style: use hierarchical for expanded view (< 200 nodes), physics for larger.
    const useHierarchical = hasExpanded && totalNodes < 200;
    // Clustered programs-only view also uses hierarchical if small enough
    const useHierarchicalClustered = !hasExpanded && totalNodes < 80;

    const repulsion = totalNodes > 200 ? -12000 : totalNodes > 100 ? -8000 : -5000;
    const springLen = totalNodes > 200 ? 400 : totalNodes > 100 ? 320 : 250;

    this.network = new vis.Network(container, { nodes: this.nodes, edges: this.edges }, {
      nodes: {
        borderWidth: 2,
        shadow: { enabled: true, color: 'rgba(0,0,0,0.3)', size: 8, x: 0, y: 2 },
        font: { multi: true },
      },
      edges: {
        smooth: totalNodes > 400
          ? { type: 'continuous' }
          : { type: 'curvedCW', roundness: 0.15 },
        font: { size: 9, strokeWidth: 3, strokeColor: '#0f172a' },
        chosen: { edge: (values) => { values.width = values.width * 1.5; values.opacity = 1; } },
      },
      layout: (useHierarchical || useHierarchicalClustered)
        ? { hierarchical: { enabled: true, direction: 'UD', sortMethod: 'directed',
            nodeSpacing: 180, levelSeparation: 120, treeSpacing: 200,
            parentCentralization: true, blockShifting: true, edgeMinimization: true } }
        : { improvedLayout: totalNodes < 400 },
      physics: (useHierarchical || useHierarchicalClustered)
        ? { enabled: false }
        : {
            enabled: true,
            barnesHut: {
              gravitationalConstant: repulsion,
              centralGravity: 0.08,
              springLength: springLen,
              springConstant: 0.012,
              damping: 0.25,
              avoidOverlap: 0.7,
            },
            stabilization: {
              enabled: true,
              iterations: Math.min(500, Math.max(150, totalNodes * 2)),
              updateInterval: 25,
              fit: true,
            },
            maxVelocity: 20,
            minVelocity: 0.2,
          },
      interaction: {
        hover: true, tooltipDelay: 150,
        navigationButtons: true, keyboard: true,
        zoomView: true, dragView: true,
        multiselect: true,
        hideEdgesOnDrag: totalNodes > 300,
        hideEdgesOnZoom: totalNodes > 300,
      },
      groups: this._groupColors || {},
    });

    // Stabilization progress indicator with cancel button
    if (!(useHierarchical || useHierarchicalClustered) && totalNodes > 50) {
      const statsEl = document.getElementById('galaxy-stats-bar');
      if (statsEl) {
        this.network.on('stabilizationProgress', (params) => {
          const pct = Math.round((params.iterations / params.total) * 100);
          statsEl.innerHTML = `<span class="ast-stat perform" id="galaxy-progress-pct">⏳ Laying out ${totalNodes} nodes... ${pct}%</span>
            <button id="galaxy-cancel-layout" title="Stop layout simulation"
                    style="margin-left:8px;padding:2px 10px;background:#7f1d1d;color:#fecaca;border:1px solid #dc2626;border-radius:4px;font-size:11px;cursor:pointer;font-weight:600;">✕ Cancel</button>`;
          const btn = document.getElementById('galaxy-cancel-layout');
          if (btn && !btn._wired) {
            btn._wired = true;
            btn.addEventListener('click', () => this._cancelLayout());
          }
        });
        this.network.on('stabilizationIterationsDone', () => {
          this.network.setOptions({ physics: { enabled: false } });
          this._updateStatsBar();
          this.network.fit({ animation: { duration: 500, easingFunction: 'easeInOutQuad' } });
        });
      }
    }

    // Click → inspector
    this.network.on('click', (params) => {
      if (params.nodes.length > 0) {
        const nd = this.nodes.get(params.nodes[0]);
        if (nd?._data) {
          this._updateInspector(nd._data);
          this._updateSourcePanel(nd._data);
        }
      }
    });

    // Double-click → expand/collapse or drill
    this.network.on('doubleClick', (params) => {
      if (params.nodes.length > 0) {
        const nodeId = params.nodes[0];
        const nd = this.nodes.get(nodeId);
        if (nd?._data) {
          // Business domain hub → expand/collapse domain members
          if (nodeId.startsWith('domain__') && nd._data.nodeType === 'DOMAIN') {
            const domain = nd._data.domain || nd._data.displayName;
            if (this._expandedClusters.has(domain)) this._expandedClusters.delete(domain);
            else this._expandedClusters.add(domain);
            this._rebuildAndRender();
          } else if (nodeId.startsWith('prog__')) {
            if (this._isBusinessMode) {
              // In business mode, double-click program → drill into AST Explorer
              if (nd._data.program && typeof astExplorer !== 'undefined' && astExplorer) {
                astExplorer.drillIntoProgram(nd._data.program);
              }
            } else {
              // In technical mode, double-click program → expand/collapse cluster
              const progKey = nd._data.program;
              if (this._expandedClusters.has(progKey)) this._expandedClusters.delete(progKey);
              else this._expandedClusters.add(progKey);
              this._rebuildAndRender();
            }
          } else if (nd._data.program) {
            if (typeof astExplorer !== 'undefined' && astExplorer) astExplorer.drillIntoProgram(nd._data.program);
          }
        }
      }
    });

    // Hover glow effect
    this.network.on('hoverNode', (params) => {
      container.style.cursor = 'pointer';
    });
    this.network.on('blurNode', () => {
      container.style.cursor = 'grab';
    });

    // C4 specific: inject level switcher and wire L2→L3 drill
    if (this.viewMode === 'c4-model') {
      this._injectC4LevelUI(container);
      this.network.on('doubleClick', (params) => {
        if (params.nodes.length > 0) {
          const nd = this.nodes.get(params.nodes[0]);
          if (this._c4Level === 2 && nd?._data?.nodeType === 'C4_Container') {
            // Drill into container → show its programs as L3 components
            const progs = nd._data.programs || [];
            if (progs.length > 0) {
              this._c4Level = 3;
              this._c4SelectedProg = (progs[0].program||'').replace(/\.cbl$/i,'').replace(/^flow-ast-/i,'').toUpperCase();
              this._rebuildAndRender();
            }
          } else if ((this._c4Level === 2 || this._c4Level === 3) && nd?._data?.program) {
            if (typeof astExplorer !== 'undefined' && astExplorer) astExplorer.drillIntoProgram(nd._data.program);
          }
        }
      });
    }
  }

  // ═══════════════════════════════════════════════════════════════════
  // CANCEL — stop both 2D vis-network stabilization and 3D ForceGraph3D
  // simulation. Wired to the small "✕ Cancel" button shown next to the
  // layout-progress percentage during a long render.
  // ═══════════════════════════════════════════════════════════════════
  _cancelLayout() {
    let stopped = false;
    // 2D vis-network: disable physics so iteration stops immediately
    if (this.network) {
      try {
        this.network.stopSimulation?.();
        this.network.setOptions({ physics: { enabled: false } });
        stopped = true;
      } catch {}
    }
    // 3D ForceGraph3D: stop the d3 force engine + pause animation loop
    if (this.graph3d) {
      try {
        this.graph3d.pauseAnimation?.();
        this.graph3d.d3Force?.('charge', null);
        this.graph3d.cooldownTicks?.(0);
        stopped = true;
      } catch {}
    }
    // Replace the progress indicator with a clear "Stopped" badge
    const statsEl = document.getElementById('galaxy-stats-bar');
    if (statsEl) {
      statsEl.innerHTML = `<span class="ast-stat" style="color:#fbbf24;">⏸ Layout cancelled — graph is rendered but unsettled.</span>
        <button id="galaxy-resume-layout" title="Resume layout simulation"
                style="margin-left:8px;padding:2px 10px;background:#1e3a8a;color:#bfdbfe;border:1px solid #3b82f6;border-radius:4px;font-size:11px;cursor:pointer;font-weight:600;">▶ Resume</button>`;
      const r = document.getElementById('galaxy-resume-layout');
      r?.addEventListener('click', () => this._resumeLayout());
    }
    this._3dCancelOverlayHide();
    return stopped;
  }

  _resumeLayout() {
    if (this.network) {
      try { this.network.setOptions({ physics: { enabled: true } }); this.network.startSimulation(); } catch {}
    }
    if (this.graph3d) {
      try { this.graph3d.cooldownTicks?.(Infinity); this.graph3d.resumeAnimation?.(); } catch {}
    }
    this._updateStatsBar();
  }

  // 3D-only floating overlay: small "✕ Cancel" button anchored to the 3D
  // container while the simulation is hot.
  _3dCancelOverlayShow() {
    const c = document.getElementById('galaxy-3d-container');
    if (!c || document.getElementById('galaxy-3d-cancel-overlay')) return;
    const btn = document.createElement('button');
    btn.id = 'galaxy-3d-cancel-overlay';
    btn.textContent = '✕ Cancel layout';
    btn.title = 'Stop the 3D force-graph simulation';
    btn.style.cssText = 'position:absolute;top:10px;left:50%;transform:translateX(-50%);z-index:30;padding:4px 12px;background:#7f1d1d;color:#fecaca;border:1px solid #dc2626;border-radius:4px;font-size:11px;cursor:pointer;font-weight:600;backdrop-filter:blur(4px);';
    btn.addEventListener('click', () => this._cancelLayout());
    c.appendChild(btn);
    // Auto-hide after 8s if user didn't click — assume layout has cooled
    this._3dCancelTimer = setTimeout(() => this._3dCancelOverlayHide(), 8000);
  }

  _3dCancelOverlayHide() {
    if (this._3dCancelTimer) { clearTimeout(this._3dCancelTimer); this._3dCancelTimer = null; }
    document.getElementById('galaxy-3d-cancel-overlay')?.remove();
  }

  // ═══════════════════════════════════════════════════════════════════
  // 3D MODE — toggle between vis-network and 3d-force-graph
  // ═══════════════════════════════════════════════════════════════════

  toggle3D() {
    this.is3D = !this.is3D;
    const btn = document.getElementById('galaxy-3d-btn');
    if (btn) btn.classList.toggle('galaxy-3d-active', this.is3D);

    const graph2d = document.getElementById(this.containerId);
    const graph3d = document.getElementById('galaxy-3d-container');
    if (!graph2d || !graph3d) return;

    if (this.is3D) {
      graph2d.style.display = 'none';
      graph3d.style.display = '';
      graph3d.style.flex = '5';
      this._render3D(graph3d);
      this._3dCancelOverlayShow();
    } else {
      graph3d.style.display = 'none';
      graph2d.style.display = '';
      this._3dCancelOverlayHide();
      if (this.graph3d) { this.graph3d._destructor?.(); this.graph3d = null; }
      graph3d.innerHTML = '';
    }
    this._updateStatsBar();
    this._renderLegend();
  }

  // ═══════════════════════════════════════════════════════════════════
  // BUSINESS DOMAIN CLASSIFIER — groups programs by service/function
  // ═══════════════════════════════════════════════════════════════════

  static BUSINESS_DOMAINS = {
    'Customer Management':   { color: '#3b82f6', icon: '👤', keys: ['CUST','CUS','CLIENT','USR','USER','COUSR'] },
    'Account Operations':    { color: '#10b981', icon: '🏦', keys: ['ACC','ACCT','ACCOUNT','CBACT','COACT'] },
    'Transaction Processing':{ color: '#f59e0b', icon: '💳', keys: ['TRN','TRANS','COTRN','CBTRN','XFR','DBCR'] },
    'Credit Card Services':  { color: '#a855f7', icon: '💎', keys: ['CRD','CREDIT','CARD','COCRD','COBIL','BIL'] },
    'Administration & Auth': { color: '#64748b', icon: '🔐', keys: ['ADM','ADMIN','SGN','SIGN','LOGIN','AUTH','COADM','COSGN'] },
    'Reporting & Export':    { color: '#06b6d4', icon: '📊', keys: ['RPT','REPORT','EXPORT','IMPORT','CORPT','CBEXP','CBIMP'] },
    'Shared Data':           { color: '#ec4899', icon: '📋', keys: [] },
    'Infrastructure':        { color: '#475569', icon: '⚙️', keys: ['ABND','WAIT','UTIL','BDS','CSUT'] },
  };

  _classifyBusinessDomain(progName, meta) {
    const upper = (progName || '').toUpperCase().replace('FLOW-AST-','').replace('.CBL','').replace('.CPY','');
    if (meta?.isCopybook) return 'Shared Data';
    for (const [domain, cfg] of Object.entries(ASTGalaxyView.BUSINESS_DOMAINS)) {
      if (cfg.keys.some(k => upper.includes(k))) return domain;
    }
    // Heuristic fallback: heavy SQL → database ops, heavy CALL → orchestration
    if (meta?.sqlCount > 50) return 'Account Operations';
    if (meta?.callCount > 3) return 'Transaction Processing';
    return 'Infrastructure';
  }

  // ═══════════════════════════════════════════════════════════════════
  // SERVICE DESCRIPTIONS — what each program does in business terms
  // ═══════════════════════════════════════════════════════════════════

  static SERVICE_DESCRIPTIONS = {
    'CREACC': { desc: 'Create Account', detail: 'Creates new customer accounts with initial setup' },
    'DELACC': { desc: 'Delete Account', detail: 'Removes accounts and cleans up related data' },
    'UPDACC': { desc: 'Update Account', detail: 'Modifies account details and settings' },
    'INQACC': { desc: 'Account Inquiry', detail: 'Retrieves and displays account information' },
    'DELCUS': { desc: 'Delete Customer', detail: 'Removes customer records from the system' },
    'UPDCUST': { desc: 'Update Customer', detail: 'Modifies customer profile information' },
    'INQCUST': { desc: 'Customer Inquiry', detail: 'Looks up and displays customer details' },
    'XFRFUN': { desc: 'Fund Transfer', detail: 'Transfers funds between accounts' },
    'DBCRFUN': { desc: 'Debit/Credit', detail: 'Processes debit and credit transactions' },
    'COCRDSLC': { desc: 'Credit Card Select', detail: 'Lists and searches credit card records' },
    'COCRDUPC': { desc: 'Credit Card Update', detail: 'Updates credit card details and limits' },
    'COBIL00C': { desc: 'Billing Process', detail: 'Generates billing statements and charges' },
    'CRDTAGY1': { desc: 'Credit Agency 1', detail: 'Credit bureau reporting interface (agency 1)' },
    'CRDTAGY2': { desc: 'Credit Agency 2', detail: 'Credit bureau reporting interface (agency 2)' },
    'CRDTAGY3': { desc: 'Credit Agency 3', detail: 'Credit bureau reporting interface (agency 3)' },
    'CRDTAGY4': { desc: 'Credit Agency 4', detail: 'Credit bureau reporting interface (agency 4)' },
    'CRDTAGY5': { desc: 'Credit Agency 5', detail: 'Credit bureau reporting interface (agency 5)' },
    'COADM01C': { desc: 'Admin Console', detail: 'System administration and configuration' },
    'COSGN00C': { desc: 'Sign On / Auth', detail: 'User authentication and session management' },
    'CORPT00C': { desc: 'Report Generator', detail: 'Generates business reports and summaries' },
    'CBEXPORT': { desc: 'Data Export', detail: 'Batch export of data to external systems' },
    'CBIMPORT': { desc: 'Data Import', detail: 'Batch import of data from external sources' },
    'COUSR00C': { desc: 'User List', detail: 'Lists and searches system users' },
    'COUSR01C': { desc: 'Add User', detail: 'Creates new user accounts' },
    'COUSR02C': { desc: 'Update User', detail: 'Modifies user profile and permissions' },
    'COUSR03C': { desc: 'Delete User', detail: 'Removes user accounts from the system' },
    'COTRN00C': { desc: 'Transaction List', detail: 'Lists and filters transaction history' },
    'COTRN01C': { desc: 'Add Transaction', detail: 'Records new financial transactions' },
    'COTRN02C': { desc: 'Transaction Detail', detail: 'Displays full transaction details and audit' },
    'CBACT01C': { desc: 'Batch Account Ops', detail: 'Batch processing of account operations' },
    'CBACT02C': { desc: 'Batch Account View', detail: 'Batch account data retrieval' },
    'CBACT03C': { desc: 'Batch Account Sync', detail: 'Batch synchronization of account data' },
    'CBACT04C': { desc: 'Batch Account Report', detail: 'Batch account reporting and analytics' },
    'CBCUS01C': { desc: 'Batch Customer Ops', detail: 'Batch processing of customer records' },
    'CBTRN01C': { desc: 'Batch Trans Process', detail: 'Batch transaction processing pipeline' },
    'CBTRN02C': { desc: 'Batch Trans Validate', detail: 'Batch transaction validation and checks' },
    'CBTRN03C': { desc: 'Batch Trans Settle', detail: 'Batch transaction settlement' },
    'COACTUPC': { desc: 'Account Update UI', detail: 'CICS screen for account updates' },
    'COACTVWC': { desc: 'Account View UI', detail: 'CICS screen for viewing accounts' },
    'CUSTOMER-DISPLAY': { desc: 'Display Customer', detail: 'Renders customer information on screen' },
    'CUSTOMER-INQUIRY': { desc: 'Customer Lookup', detail: 'Interactive customer search and inquiry' },
    'ABNDPROC': { desc: 'Abend Handler', detail: 'Abnormal termination error handler' },
    'COBSWAIT': { desc: 'Wait Service', detail: 'Timer and synchronization service' },
    'CSUTLDTC': { desc: 'Date Utility', detail: 'Date conversion and validation utility' },
  };

  _describeProgram(name, meta) {
    const clean = (name || '').replace('flow-ast-', '').replace('.cbl', '').replace('.cpy', '');
    const entry = ASTGalaxyView.SERVICE_DESCRIPTIONS[clean];
    if (entry) return entry;
    // Heuristic: infer from name patterns
    const upper = clean.toUpperCase();
    if (meta?.isCopybook) return { desc: 'Data Structure', detail: 'Shared copybook data definitions' };
    if (upper.includes('INQ') || upper.includes('INQUIRY')) return { desc: 'Inquiry Service', detail: 'Reads and displays data' };
    if (upper.includes('UPD') || upper.includes('UPDATE')) return { desc: 'Update Service', detail: 'Modifies existing records' };
    if (upper.includes('DEL') || upper.includes('DELETE')) return { desc: 'Delete Service', detail: 'Removes records' };
    if (upper.includes('CRE') || upper.includes('ADD') || upper.includes('INSERT')) return { desc: 'Create Service', detail: 'Creates new records' };
    if (upper.includes('RPT') || upper.includes('REPORT')) return { desc: 'Reporting', detail: 'Generates reports' };
    if (upper.includes('XFR') || upper.includes('TRANSFER')) return { desc: 'Transfer Service', detail: 'Moves data between entities' };
    if ((meta?.sqlCount || 0) > 100) return { desc: 'Data Access', detail: 'Heavy database operations' };
    if ((meta?.callCount || 0) > 3) return { desc: 'Orchestrator', detail: 'Coordinates multiple sub-programs' };
    return { desc: clean, detail: 'COBOL program' };
  }

  // ═══════════════════════════════════════════════════════════════════
  // SERVICE CATALOG 2D — descriptive nodes showing what each program does
  // ═══════════════════════════════════════════════════════════════════

  _buildServiceCatalogVisData() {
    const nodeList = [];
    const edgeList = [];
    const nodeIds = new Set();
    let edgeIdx = 0;

    const programs = this.galaxyData?.programs || [];
    const seen = new Set();
    const uniqueProgs = [];
    for (const p of programs) {
      if (seen.has(p.program)) continue;
      seen.add(p.program);
      uniqueProgs.push(p);
    }

    let filtered = this._applyShowFilter([...uniqueProgs]);
    if (this.filter !== 'all') {
      filtered = filtered.filter(p => p.program === this.filter || p.program.includes(this.filter));
    }
    filtered = this._applySortMode(filtered);

    const domainMap = new Map();
    for (const p of filtered) {
      const domain = this._classifyBusinessDomain(p.program, p);
      if (!domainMap.has(domain)) domainMap.set(domain, []);
      domainMap.get(domain).push(p);
    }

    const groupColors = {};
    const domainKeys = [...domainMap.keys()];

    for (let di = 0; di < domainKeys.length; di++) {
      const domain = domainKeys[di];
      const cfg = ASTGalaxyView.BUSINESS_DOMAINS[domain] || { color: '#64748b', icon: '•' };
      const members = domainMap.get(domain);
      const hubId = `domain__${domain.replace(/\s+/g, '_')}`;
      const groupId = `sgrp_${domain.replace(/\s+/g, '_')}`;

      groupColors[groupId] = {
        color: { background: 'rgba(30,41,59,0.08)', border: cfg.color },
        font: { color: '#94a3b8' },
      };

      // Domain hub
      nodeList.push({
        id: hubId, group: groupId,
        label: `${cfg.icon} ${domain}\n${members.length} services`,
        level: 0,
        title: `${cfg.icon} ${domain}\n${members.length} programs providing services in this domain`,
        color: { background: cfg.color, border: cfg.color,
                 highlight: { background: '#fbbf24', border: '#f59e0b' },
                 hover: { background: cfg.color, border: '#e2e8f0' } },
        font: { color: '#f8fafc', size: 14, multi: true, bold: { color: '#f8fafc' } },
        shape: 'dot', size: 30 + Math.sqrt(members.length) * 5,
        borderWidth: 4, mass: 5,
        shadow: { enabled: true, color: cfg.color, x: 0, y: 0, size: 18 },
        _data: { nodeType: 'DOMAIN', displayName: domain, domain, programCount: members.length },
      });
      nodeIds.add(hubId);

      // Member programs with SERVICE DESCRIPTION labels
      for (const p of members) {
        const progId = `prog__${p.program}`;
        if (nodeIds.has(progId)) continue;
        const displayName = p.program.replace('flow-ast-', '').replace('.cbl', '');
        const isCopybook = p.isCopybook || false;
        const svc = this._describeProgram(p.program, p);
        const sqlCount = p.sqlCount || 0;
        const callCount = p.callCount || 0;

        // Multi-line label: service description + program name + key metric
        let label = `${svc.desc}`;
        label += `\n${displayName}`;
        const metricParts = [];
        if (sqlCount > 0) metricParts.push(`${sqlCount} SQL`);
        if (callCount > 0) metricParts.push(`${callCount} CALL`);
        if (p.sectionCount > 0) metricParts.push(`${p.sectionCount} sec`);
        if (metricParts.length) label += `\n${metricParts.join(' · ')}`;

        const style = ASTGalaxyView.NODE_STYLE[isCopybook ? 'COPYBOOK' : 'PROGRAM'] || ASTGalaxyView.DEFAULT_STYLE;

        nodeList.push({
          id: progId, group: groupId,
          label,
          level: 1,
          title: `🔧 ${svc.desc}\n${svc.detail}\n\nProgram: ${displayName}\nDomain: ${domain}\nSQL: ${sqlCount} · CALLs: ${callCount} · Sections: ${p.sectionCount || 0}\nLOC: ${p.lineCount || 0}`,
          color: { background: style.bg, border: style.border,
                   highlight: { background: '#fbbf24', border: '#f59e0b' },
                   hover: { background: style.border, border: '#e2e8f0' } },
          font: { color: '#e2e8f0', size: 11, multi: true, bold: { color: '#f8fafc', size: 13 } },
          shape: 'box',
          size: 18,
          borderWidth: 2, mass: 2,
          widthConstraint: { minimum: 120, maximum: 200 },
          shadow: { enabled: true, color: style.bg, x: 0, y: 0, size: 8 },
          _data: { ...p, program: p.program, nodeType: isCopybook ? 'COPYBOOK' : 'PROGRAM', displayName, domain, serviceDesc: svc.desc, serviceDetail: svc.detail },
        });
        nodeIds.add(progId);

        // SERVICE CATALOG EXPANDED: show structural AST nodes within this program
        if (this._isServiceCatalogExpanded && this.astData?.nodes) {
          const progAstNodes = this.astData.nodes.filter(n => n.program === p.program);
          const STRUCTURAL = new Set(['SECTION', 'PARAGRAPHS', 'PARAGRAPH', 'PARAGRAPH_NAME', 'PROCEDURE_DIVISION_BODY']);
          const KEY_STMTS = new Set(['PERFORM', 'CALL', 'CallStatement', 'IF_BRANCH', 'EVALUATE', 'DIALECT', 'DIALECT_CONTAINER']);
          let stmtAdded = 0;
          const nodeIndex = new Map();

          for (const an of progAstNodes) {
            if (nodeIds.has(an.id)) continue;
            const isSt = STRUCTURAL.has(an.nodeType);
            const isKey = KEY_STMTS.has(an.nodeType);
            if (!isSt && !isKey) continue;
            if (isKey && stmtAdded >= 20) continue;
            nodeIndex.set(an.id, an);

            const nStyle = ASTGalaxyView.NODE_STYLE[an.nodeType] || ASTGalaxyView.DEFAULT_STYLE;
            const cleanName = (an.name || '').replace(/.*\//, '').replace(/Context\/.*/, '');
            const typeLabel = ASTGalaxyView.HUMAN_TYPES[an.nodeType] || an.nodeType;
            const nodeLabel = cleanName ? `${nStyle.icon} ${cleanName}` : `${nStyle.icon} ${typeLabel}`;

            nodeList.push({
              id: an.id, group: groupId,
              label: nodeLabel,
              level: 2,
              title: `${an.nodeType}: ${an.name || '—'}\nLines ${an.startLine}–${an.endLine}\nService: ${svc.desc}\nProgram: ${displayName}`,
              color: { background: nStyle.bg, border: nStyle.border,
                       highlight: { background: '#fbbf24', border: '#f59e0b' } },
              font: { color: '#e2e8f0', size: 10, multi: true },
              shape: nStyle.shape, size: nStyle.size,
              borderWidth: 1, mass: 1,
              shadow: { enabled: true, color: nStyle.bg, x: 0, y: 0, size: 4 },
              _data: { ...an, displayName: cleanName || typeLabel, domain, serviceDesc: svc.desc },
            });
            nodeIds.add(an.id);
            if (isKey) stmtAdded++;
          }

          // Edges within expanded program
          if (this.astData?.edges) {
            for (const ae of this.astData.edges) {
              if (!nodeIds.has(ae.source) || !nodeIds.has(ae.target)) continue;
              const sn = nodeIndex.get(ae.source);
              const tn = nodeIndex.get(ae.target);
              if (!sn || !tn || sn.program !== p.program || tn.program !== p.program) continue;
              const aes = ASTGalaxyView.EDGE_STYLE[ae.type] || ASTGalaxyView.DEFAULT_EDGE;
              edgeList.push({
                id: `e_${edgeIdx++}`, from: ae.source, to: ae.target,
                arrows: { to: { enabled: true, scaleFactor: 0.4 } },
                color: { color: aes.color, opacity: 0.5 },
                width: aes.width * 0.7, dashes: aes.dashes || false,
                smooth: { type: 'curvedCW', roundness: 0.1 },
                _type: ae.type,
              });
            }
          }

          // Connect program → first structural child
          const firstChild = progAstNodes.find(n => STRUCTURAL.has(n.nodeType) && nodeIds.has(n.id));
          if (firstChild) {
            edgeList.push({
              id: `e_${edgeIdx++}`, from: progId, to: firstChild.id,
              color: { color: '#475569', opacity: 0.4 }, width: 1,
              arrows: { to: { enabled: true, scaleFactor: 0.3 } },
              _type: 'CONTAINS',
            });
          }
        }

        // Hub → member
        edgeList.push({
          id: `e_${edgeIdx++}`, from: hubId, to: progId,
          color: { color: cfg.color, opacity: 0.25 },
          width: 1, arrows: '', dashes: false,
          smooth: { type: 'curvedCW', roundness: 0.1 },
          _type: 'CLUSTER',
        });
      }
    }

    // Cross-domain edges
    if (this.galaxyData?.edges) {
      for (const e of this.galaxyData.edges) {
        const srcProg = uniqueProgs.find(p =>
          p.program === e.source || p.program.replace('flow-ast-','').replace('.cbl','') === e.source?.replace('.cbl',''));
        const tgtProg = uniqueProgs.find(p =>
          p.program === e.target || p.program.replace('flow-ast-','').replace('.cbl','') === e.target?.replace('.cbl',''));
        if (!srcProg || !tgtProg) continue;
        const srcId = `prog__${srcProg.program}`;
        const tgtId = `prog__${tgtProg.program}`;
        if (!nodeIds.has(srcId) || !nodeIds.has(tgtId) || srcId === tgtId) continue;
        const es = ASTGalaxyView.EDGE_STYLE[e.type] || ASTGalaxyView.DEFAULT_EDGE;
        const srcDom = this._classifyBusinessDomain(srcProg.program, srcProg);
        const tgtDom = this._classifyBusinessDomain(tgtProg.program, tgtProg);
        const isCross = srcDom !== tgtDom;
        edgeList.push({
          id: `e_${edgeIdx++}`, from: srcId, to: tgtId,
          arrows: { to: { enabled: true, scaleFactor: 0.7 } },
          label: es.label || e.type,
          font: { color: isCross ? '#f59e0b' : es.color, size: 10, strokeWidth: 3, strokeColor: '#0f172a' },
          color: { color: isCross ? '#f59e0b' : es.color, opacity: isCross ? 0.9 : 0.6 },
          width: isCross ? 3 : es.width,
          dashes: es.dashes || false,
          smooth: { type: 'curvedCW', roundness: 0.2 },
          _type: e.type,
        });
      }
    }

    this.nodes = new vis.DataSet(nodeList);
    this.edges = new vis.DataSet(edgeList);
    this._groupColors = groupColors;
  }

  _render3D(container) {
    if (!container || typeof ForceGraph3D === 'undefined') {
      container.innerHTML = '<div class="ast-error">3D library not loaded</div>';
      return;
    }
    container.innerHTML = '';
    console.log(`[_render3D] viewMode=${this.viewMode}, container=${container.id}, w=${container.offsetWidth}, h=${container.offsetHeight}`);

    // Install polyfill once: 3d-force-graph@1.73 has no .onNodeDoubleClick chain method.
    // We synthesise it on the factory so all subsequent .onNodeClick(...).onNodeDoubleClick(...)
    // chains keep working without per-call-site changes.
    ASTGalaxyView._patchForceGraph3DDblClick();

    if (this.viewMode === 'service-catalog-v2') {
      this._render3DServiceCatalogV2(container);
    } else if (this.viewMode === 'service-catalog-v3') {
      this._render3DModernizationRadar(container);
    } else if (this._isBusinessMode) {
      this._render3DBusiness(container);
    } else {
      this._render3DTechnical(container);
    }

    // Add mode label overlay so user can see which 3D view is active
    const modeLabels = {
      'clustered': '📦 Technical', 'expanded': '📦 Technical (Expanded)',
      'expanded-v2': '🔽 Technical (Expanded v2 — Top-Down)',
      'business': '🏢 Business Domains', 'business-expanded': '🏢 Business Domains (Expanded)',
      'service-catalog': '📋 Service Catalog', 'service-catalog-expanded': '📋 Service Catalog (Expanded)',
      'service-catalog-expanded-3d': '🧊 Service Catalog (Expanded 3D)',
      'service-catalog-v2': '🚀 Service Catalog v2',
      'service-catalog-v3': '🎯 Modernization Radar',
      'program-map': '📊 Programs',
      'bian-matrix': '🏦 BIAN Service Landscape',
      'c4-model': '🏗️ C4 Model',
    };
    const label = document.createElement('div');
    label.style.cssText = 'position:absolute;top:10px;left:10px;z-index:20;padding:6px 14px;background:rgba(3,7,18,0.85);color:#e2e8f0;border:1px solid #475569;border-radius:6px;font-size:13px;font-weight:600;pointer-events:none;backdrop-filter:blur(4px);';
    label.textContent = modeLabels[this.viewMode] || this.viewMode;
    container.appendChild(label);

    // Rich floating icon legend on every 3D view (content auto-tailors per mode).
    this._render3DLegendOverlay(container);
  }

  // ═══════════════════════════════════════════════════════════════════
  // 3D TECHNICAL VIEW — same nodes/edges as 2D vis-network, in 3D
  // ═══════════════════════════════════════════════════════════════════

  _render3DTechnical(container) {
    const nodes3d = [];
    const edges3d = [];
    const nodeIdSet = new Set();

    // Build data from current vis-network DataSets
    if (!this.nodes || !this.edges) return;

    // Group nodes by program for spatial clustering
    const programPositions = new Map();
    const programNodes = new Map();
    this.nodes.forEach(n => {
      const prog = n._data?.program || n._data?.displayName || 'unknown';
      if (!programNodes.has(prog)) programNodes.set(prog, []);
      programNodes.get(prog).push(n);
    });

    // Position programs in a ring, nodes within each program in a local cluster
    const progKeys = [...programNodes.keys()];
    const ringRadius = 80 + progKeys.length * 15;

    for (let pi = 0; pi < progKeys.length; pi++) {
      const prog = progKeys[pi];
      const members = programNodes.get(prog);
      const angle = (pi / progKeys.length) * Math.PI * 2;
      const cx = Math.cos(angle) * ringRadius;
      const cz = Math.sin(angle) * ringRadius;
      programPositions.set(prog, { x: cx, z: cz });

      for (let mi = 0; mi < members.length; mi++) {
        const n = members[mi];
        const nodeType = n._data?.nodeType || 'PROGRAM';
        const style = ASTGalaxyView.NODE_STYLE[nodeType] || ASTGalaxyView.DEFAULT_STYLE;
        const layer = ASTGalaxyView.LAYER[nodeType] ?? 0;

        // Spread within cluster
        const localAngle = (mi / members.length) * Math.PI * 2;
        const localR = members.length > 1 ? 8 + members.length * 2 : 0;
        const x = cx + Math.cos(localAngle) * localR;
        const z = cz + Math.sin(localAngle) * localR;
        const y = layer * -25; // vertical layering by type

        const domain = n._data?.domain || this._classifyBusinessDomain(n._data?.program, n._data);

        nodes3d.push({
          id: n.id,
          name: (n.label || '').replace(/\n/g, ' '),
          color: style.bg,
          borderColor: style.border,
          val: (style.size || 10) * 0.6,
          _data: n._data,
          _nodeType: nodeType,
          _domain: domain,
          _isHub: false,
          _isExpanded: false,
          _shape: style.shape,
          fx: x, fy: y, fz: z,
        });
        nodeIdSet.add(n.id);
      }
    }

    this.edges.forEach(e => {
      if (nodeIdSet.has(e.from) && nodeIdSet.has(e.to)) {
        const eStyle = ASTGalaxyView.EDGE_STYLE[e._type] || ASTGalaxyView.DEFAULT_EDGE;
        edges3d.push({
          source: e.from, target: e.to,
          color: eStyle.color,
          width: eStyle.width || 1,
          _type: e._type,
          _intraCluster: false,
        });
      }
    });

    this.graph3d = ForceGraph3D()(container)
      .graphData({ nodes: nodes3d, links: edges3d })
      .backgroundColor('#050810')
      .showNavInfo(false)
      .nodeColor(n => {
        if (this._3dSearchTerm && this._3dSearchTerm.length >= 2) {
          return (n.name || '').toLowerCase().includes(this._3dSearchTerm) ? '#fbbf24' : 'rgba(30,41,59,0.3)';
        }
        return n.color;
      })
      .nodeVal(n => {
        if (this._3dSearchTerm && this._3dSearchTerm.length >= 2) {
          return (n.name || '').toLowerCase().includes(this._3dSearchTerm) ? (n.val || 6) * 3 : (n.val || 6) * 0.2;
        }
        return n.val;
      })
      .nodeLabel(n => {
        const d = n._data || {};
        const type = ASTGalaxyView.HUMAN_TYPES[n._nodeType] || n._nodeType;
        const style = ASTGalaxyView.NODE_STYLE[n._nodeType] || ASTGalaxyView.DEFAULT_STYLE;
        let html = `<div style="background:rgba(5,8,16,0.95);padding:8px 12px;border-radius:6px;border:1px solid ${n.color};color:#e2e8f0;font-size:12px;min-width:160px;">`;
        html += `<div style="font-weight:700;color:${n.color};font-size:13px;">${style.icon} ${type}</div>`;
        html += `<div style="color:#e2e8f0;margin:2px 0;">${n.name}</div>`;
        if (d.program) html += `<div style="color:#94a3b8;font-size:11px;">Program: ${(d.program || '').replace('flow-ast-','').replace('.cbl','')}</div>`;
        if (n._domain) html += `<div style="color:#64748b;font-size:11px;">${n._domain}</div>`;
        if (d.sectionCount !== undefined) {
          html += `<div style="font-size:11px;margin-top:4px;color:#94a3b8;">${d.sectionCount} sec · ${d.paraCount} para · ${d.sqlCount || 0} SQL · ${d.callCount || 0} CALL</div>`;
        }
        if (d.startLine > 0) html += `<div style="font-size:10px;color:#64748b;">Lines ${d.startLine}–${d.endLine}</div>`;
        html += `</div>`;
        return html;
      })
      .nodeOpacity(0.95)
      .nodeThreeObject(n => {
        try {
          const THREE = window.THREE;
          if (!THREE) return undefined;
          const group = new THREE.Group();
          const r = Math.max(1, (n.val || 6) * 0.3);

          // Shape-based geometry matching 2D view
          let geometry;
          switch (n._shape) {
            case 'diamond': geometry = new THREE.OctahedronGeometry(r, 0); break;
            case 'star': geometry = new THREE.IcosahedronGeometry(r, 0); break;
            case 'triangle': geometry = new THREE.TetrahedronGeometry(r * 1.2, 0); break;
            default: geometry = new THREE.SphereGeometry(r, 12, 8); break;
          }
          const mat = new THREE.MeshPhongMaterial({
            color: new THREE.Color(n.color),
            emissive: new THREE.Color(n.color),
            emissiveIntensity: 0.5,
            transparent: true, opacity: 0.9,
          });
          group.add(new THREE.Mesh(geometry, mat));

          // Label
          const canvas = document.createElement('canvas');
          const label = n.name || '';
          const displayLabel = label.length > 28 ? label.slice(0, 25) + '...' : label;
          const fontSize = 22;
          canvas.width = 300; canvas.height = 32;
          const ctx = canvas.getContext('2d');
          ctx.font = `bold ${fontSize}px system-ui, sans-serif`;
          ctx.fillStyle = 'rgba(5,8,16,0.7)';
          ctx.fillRect(0, 0, 300, 32);
          ctx.fillStyle = '#cbd5e1';
          ctx.textAlign = 'center';
          ctx.textBaseline = 'middle';
          ctx.fillText(displayLabel, 150, 16);
          const texture = new THREE.CanvasTexture(canvas);
          texture.minFilter = THREE.LinearFilter;
          const spriteMat = new THREE.SpriteMaterial({ map: texture, transparent: true });
          const sprite = new THREE.Sprite(spriteMat);
          sprite.scale.set(14, 1.5, 1);
          sprite.position.y = r + 1.5;
          group.add(sprite);

          return group;
        } catch { return undefined; }
      })
      .nodeThreeObjectExtend(false)
      .linkColor(l => l.color || '#334155')
      .linkOpacity(0.4)
      .linkWidth(l => (l.width || 1) * 0.4)
      .linkDirectionalArrowLength(3)
      .linkDirectionalArrowRelPos(1)
      .linkDirectionalParticles(l => l._type === 'CALL' ? 2 : l._type === 'JUMPS_TO' ? 1 : 0)
      .linkDirectionalParticleWidth(1.5)
      .linkDirectionalParticleColor(l => l.color || '#60a5fa')
      .linkDirectionalParticleSpeed(0.004)
      .onNodeClick(node => {
        if (node._data) {
          this._updateInspector(node._data);
          this._updateSourcePanel(node._data);
        }
        const pos = { x: node.fx || node.x || 0, y: node.fy || node.y || 0, z: node.fz || node.z || 0 };
        const dist = 100;
        const hypot = Math.hypot(pos.x, pos.y, pos.z) || 1;
        const ratio = 1 + dist / hypot;
        this.graph3d.cameraPosition(
          { x: pos.x * ratio, y: pos.y * ratio + 20, z: pos.z * ratio },
          pos, 800
        );
      })
      .onNodeDoubleClick(node => {
        if (node._data?.program && typeof astExplorer !== 'undefined' && astExplorer) {
          astExplorer.drillIntoProgram(node._data.program);
        }
      })
      .width(container.offsetWidth)
      .height(container.offsetHeight);

    // Camera: elevated view
    setTimeout(() => {
      if (this.graph3d) {
        this.graph3d.cameraPosition({ x: 0, y: 250, z: 450 }, { x: 0, y: -30, z: 0 }, 1500);
      }
    }, 300);

    // Lights
    try {
      const THREE = window.THREE;
      if (THREE && this.graph3d.scene) {
        setTimeout(() => {
          this.graph3d.scene().add(new THREE.AmbientLight(0x404060, 1.5));
          const dl = new THREE.DirectionalLight(0xffffff, 0.8);
          dl.position.set(200, 400, 300);
          this.graph3d.scene().add(dl);
        }, 100);
      }
    } catch {}

    // Resize
    const ro = new ResizeObserver(() => {
      if (this.graph3d) this.graph3d.width(container.offsetWidth).height(container.offsetHeight);
    });
    ro.observe(container);
  }

  // ═══════════════════════════════════════════════════════════════════
  // MODERNIZATION RADAR (V3) — 2D concentric risk rings + 3D city skyline
  // ═══════════════════════════════════════════════════════════════════

  // ── Risk tier classification ──
  static RISK_TIERS = [
    { label: 'Critical', color: '#dc2626', maxCpx: 1.0, ring: 0 },
    { label: 'High',     color: '#f97316', maxCpx: 0.6, ring: 1 },
    { label: 'Medium',   color: '#eab308', maxCpx: 0.4, ring: 2 },
    { label: 'Low',      color: '#22c55e', maxCpx: 0.2, ring: 3 },
    { label: 'Minimal',  color: '#06b6d4', maxCpx: 0.0, ring: 4 },
  ];

  _getRiskTier(complexity) {
    if (complexity > 0.6)  return ASTGalaxyView.RISK_TIERS[0];
    if (complexity > 0.4)  return ASTGalaxyView.RISK_TIERS[1];
    if (complexity > 0.2)  return ASTGalaxyView.RISK_TIERS[2];
    if (complexity > 0.08) return ASTGalaxyView.RISK_TIERS[3];
    return ASTGalaxyView.RISK_TIERS[4];
  }

  // ── 2D: Concentric Risk Radar ──
  _buildModernizationRadarVisData() {
    const nodeList = [];
    const edgeList = [];
    const nodeIds = new Set();
    let edgeIdx = 0;

    const programs = this.galaxyData?.programs || [];
    const seen = new Set();
    const uniqueProgs = [];
    for (const p of programs) {
      if (seen.has(p.program)) continue;
      seen.add(p.program);
      uniqueProgs.push(p);
    }

    // Classify and sort by complexity (highest first)
    const enriched = uniqueProgs.map(p => ({
      ...p,
      complexity: this._computeComplexity(p),
      domain: this._classifyBusinessDomain(p.program, p),
      svc: this._describeProgram(p.program, p),
      displayName: p.program.replace('flow-ast-', '').replace('.cbl', ''),
    })).sort((a, b) => b.complexity - a.complexity);

    // Bucket into risk tiers
    const tiers = new Map(); // tier ring → programs[]
    for (const p of enriched) {
      const tier = this._getRiskTier(p.complexity);
      if (!tiers.has(tier.ring)) tiers.set(tier.ring, []);
      tiers.get(tier.ring).push(p);
    }

    // Ring layout parameters
    const centerX = 0;
    const centerY = 0;
    const ringGap = 220;
    const baseRadius = 120;

    // Center bullseye node
    nodeList.push({
      id: 'radar_center', label: '🎯\nModernization\nRadar',
      x: centerX, y: centerY, fixed: { x: true, y: true },
      shape: 'dot', size: 40,
      color: { background: '#0f172a', border: '#f59e0b',
               highlight: { background: '#1e293b', border: '#fbbf24' } },
      font: { color: '#fbbf24', size: 14, multi: true, bold: { color: '#fbbf24' } },
      borderWidth: 3, mass: 10,
      shadow: { enabled: true, color: '#f59e0b', x: 0, y: 0, size: 25 },
      _data: { nodeType: 'RADAR_CENTER', displayName: 'Radar Center' },
    });
    nodeIds.add('radar_center');

    // Ring label nodes (fixed, non-interactive)
    for (const tierCfg of ASTGalaxyView.RISK_TIERS) {
      const ringR = baseRadius + tierCfg.ring * ringGap;
      const ringId = `ring_label_${tierCfg.ring}`;
      nodeList.push({
        id: ringId,
        label: `── ${tierCfg.label} Risk ──`,
        x: centerX + ringR + 30, y: centerY - ringR - 10,
        fixed: { x: true, y: true },
        shape: 'text', size: 1,
        font: { color: tierCfg.color, size: 12, bold: true },
        physics: false, mass: 0,
        _data: { nodeType: 'RING_LABEL' },
      });
      nodeIds.add(ringId);
    }

    // Place programs on their risk ring
    for (const [ringIdx, members] of tiers.entries()) {
      const tierCfg = ASTGalaxyView.RISK_TIERS[ringIdx];
      const ringR = baseRadius + ringIdx * ringGap;

      for (let mi = 0; mi < members.length; mi++) {
        const p = members[mi];
        const progId = `prog__${p.program}`;
        if (nodeIds.has(progId)) continue;

        const angle = (mi / members.length) * Math.PI * 2 - Math.PI / 2;
        const jitter = (Math.random() - 0.5) * ringGap * 0.3;
        const px = centerX + (ringR + jitter) * Math.cos(angle);
        const py = centerY + (ringR + jitter) * Math.sin(angle);

        const domainCfg = ASTGalaxyView.BUSINESS_DOMAINS[p.domain] || { color: '#64748b', icon: '•' };
        const cpx = Math.round(p.complexity * 100);

        // Size = LOC (bigger program = bigger node)
        const size = Math.max(10, Math.min(45, 10 + Math.sqrt(p.lineCount || 0) * 0.3));

        // Multi-line label: service + name + metrics
        let label = `${p.svc.desc}`;
        label += `\n${p.displayName}`;
        label += `\n${cpx}% risk · ${(p.lineCount || 0).toLocaleString()} LOC`;

        nodeList.push({
          id: progId,
          label,
          x: px, y: py,
          shape: 'dot', size,
          color: {
            background: tierCfg.color,
            border: domainCfg.color,
            highlight: { background: '#fbbf24', border: '#f59e0b' },
            hover: { background: tierCfg.color, border: '#e2e8f0' },
          },
          font: { color: '#e2e8f0', size: 10, multi: true, bold: { color: '#f8fafc', size: 12 } },
          borderWidth: 3, mass: 2,
          title: `🔧 ${p.svc.desc}\n${p.svc.detail}\n\nProgram: ${p.displayName}\nDomain: ${domainCfg.icon} ${p.domain}\nRisk: ${cpx}% (${tierCfg.label})\nLOC: ${(p.lineCount || 0).toLocaleString()}\nSQL: ${p.sqlCount || 0} · CALLs: ${p.callCount || 0}\nSections: ${p.sectionCount || 0} · Paragraphs: ${p.paraCount || 0}`,
          shadow: { enabled: true, color: tierCfg.color, x: 0, y: 0, size: cpx > 50 ? 20 : 10 },
          _data: { ...p, program: p.program, nodeType: p.isCopybook ? 'COPYBOOK' : 'PROGRAM',
                   displayName: p.displayName, domain: p.domain, serviceDesc: p.svc.desc,
                   serviceDetail: p.svc.detail, complexity: p.complexity, riskTier: tierCfg.label },
        });
        nodeIds.add(progId);
      }
    }

    // Inter-program edges
    if (this.galaxyData?.edges) {
      for (const e of this.galaxyData.edges) {
        const srcProg = uniqueProgs.find(p =>
          p.program === e.source || p.program.replace('flow-ast-','').replace('.cbl','') === e.source?.replace('.cbl',''));
        const tgtProg = uniqueProgs.find(p =>
          p.program === e.target || p.program.replace('flow-ast-','').replace('.cbl','') === e.target?.replace('.cbl',''));
        if (!srcProg || !tgtProg) continue;
        const srcId = `prog__${srcProg.program}`;
        const tgtId = `prog__${tgtProg.program}`;
        if (!nodeIds.has(srcId) || !nodeIds.has(tgtId) || srcId === tgtId) continue;
        const es = ASTGalaxyView.EDGE_STYLE[e.type] || ASTGalaxyView.DEFAULT_EDGE;
        edgeList.push({
          id: `e_${edgeIdx++}`, from: srcId, to: tgtId,
          arrows: { to: { enabled: true, scaleFactor: 0.6 } },
          label: es.label || e.type,
          font: { color: es.color, size: 9, strokeWidth: 3, strokeColor: '#0f172a' },
          color: { color: es.color, opacity: 0.5 },
          width: es.width, dashes: es.dashes || false,
          smooth: { type: 'curvedCW', roundness: 0.15 },
          _type: e.type,
        });
      }
    }

    this.nodes = new vis.DataSet(nodeList);
    this.edges = new vis.DataSet(edgeList);
  }

  // ── 3D: Floating City Skyline ──
  _render3DModernizationRadar(container) {
    const programs = this.galaxyData?.programs || [];
    const seen = new Set();
    const uniqueProgs = [];
    for (const p of programs) {
      if (seen.has(p.program)) continue;
      seen.add(p.program);
      uniqueProgs.push(p);
    }

    // Enrich with domain, complexity, service desc
    const enriched = uniqueProgs.map(p => ({
      ...p,
      complexity: this._computeComplexity(p),
      domain: this._classifyBusinessDomain(p.program, p),
      svc: this._describeProgram(p.program, p),
      displayName: p.program.replace('flow-ast-', '').replace('.cbl', ''),
      tier: this._getRiskTier(this._computeComplexity(p)),
    }));

    // Group by domain for street grid layout
    const domainMap = new Map();
    for (const p of enriched) {
      if (!domainMap.has(p.domain)) domainMap.set(p.domain, []);
      domainMap.get(p.domain).push(p);
    }

    const nodes3d = [];
    const edges3d = [];
    const nodeIdSet = new Set();

    // City grid: each domain is a "block", programs are buildings
    const domainKeys = [...domainMap.keys()];
    const blockSize = 160;
    const streetWidth = 60;
    const cols = Math.ceil(Math.sqrt(domainKeys.length));

    for (let di = 0; di < domainKeys.length; di++) {
      const domain = domainKeys[di];
      const cfg = ASTGalaxyView.BUSINESS_DOMAINS[domain] || { color: '#64748b', icon: '•' };
      const members = domainMap.get(domain);
      const col = di % cols;
      const row = Math.floor(di / cols);
      const blockX = col * (blockSize + streetWidth) - (cols * (blockSize + streetWidth)) / 2;
      const blockZ = row * (blockSize + streetWidth) - (Math.ceil(domainKeys.length / cols) * (blockSize + streetWidth)) / 2;

      // Sort by complexity within domain (tallest in center)
      members.sort((a, b) => b.complexity - a.complexity);

      // Ground plate for the domain block
      const plateId = `plate__${domain.replace(/\s/g, '_')}`;
      nodes3d.push({
        id: plateId,
        name: `${cfg.icon} ${domain}`,
        color: cfg.color,
        val: 20,
        fx: blockX, fy: -2, fz: blockZ,
        _data: { nodeType: 'DOMAIN', displayName: domain, domain, programCount: members.length },
        _nodeType: 'DOMAIN', _domain: domain, _isPlate: true,
        _hubMetrics: {
          programs: members.length,
          loc: members.reduce((s, m) => s + (m.lineCount || 0), 0),
          sql: members.reduce((s, m) => s + (m.sqlCount || 0), 0),
          calls: members.reduce((s, m) => s + (m.callCount || 0), 0),
        },
      });
      nodeIdSet.add(plateId);

      // Place buildings within block
      const innerCols = Math.ceil(Math.sqrt(members.length));
      for (let mi = 0; mi < members.length; mi++) {
        const m = members[mi];
        const progId = `prog__${m.program}`;
        if (nodeIdSet.has(progId)) continue;

        const ic = mi % innerCols;
        const ir = Math.floor(mi / innerCols);
        const spacing = blockSize / (innerCols + 1);
        const bx = blockX - blockSize / 2 + spacing * (ic + 1);
        const bz = blockZ - blockSize / 2 + spacing * (ir + 1);

        // Building height = LOC (taller = more code)
        const maxLOC = Math.max(...enriched.map(e => e.lineCount || 1));
        const height = Math.max(3, ((m.lineCount || 1) / maxLOC) * 80);
        const by = height / 2; // center of building

        // Width = complexity (wider = more complex)
        const width = Math.max(2, 2 + m.complexity * 12);

        nodes3d.push({
          id: progId,
          name: `${m.svc.desc}\n${m.displayName}`,
          color: m.tier.color,
          borderColor: cfg.color,
          val: width,
          fx: bx, fy: by, fz: bz,
          _data: { ...m, program: m.program, nodeType: 'PROGRAM',
                   displayName: m.displayName, domain, serviceDesc: m.svc.desc,
                   serviceDetail: m.svc.detail, complexity: m.complexity,
                   riskTier: m.tier.label },
          _nodeType: 'PROGRAM', _domain: domain,
          _isPlate: false, _isBuilding: true,
          _buildingHeight: height, _buildingWidth: width,
          _tier: m.tier,
          _complexity: m.complexity,
          _metrics: {
            sqlCount: m.sqlCount || 0, callCount: m.callCount || 0,
            sectionCount: m.sectionCount || 0, paraCount: m.paraCount || 0,
            performCount: m.performCount || 0, branchCount: m.branchCount || 0,
            lineCount: m.lineCount || 0, nodeCount: m.nodeCount || 0,
            complexity: m.complexity,
          },
        });
        nodeIdSet.add(progId);
      }
    }

    // Edges: inter-program dependencies as data flow lines
    const edgeSet = new Set();
    if (this.galaxyData?.edges) {
      for (const e of this.galaxyData.edges) {
        const srcId = `prog__${e.source}`;
        const tgtId = `prog__${e.target}`;
        const srcOk = nodeIdSet.has(srcId) ? srcId : [...nodeIdSet].find(k => k.includes(e.source?.replace('.cbl','')));
        const tgtOk = nodeIdSet.has(tgtId) ? tgtId : [...nodeIdSet].find(k => k.includes(e.target?.replace('.cbl','')));
        if (!srcOk || !tgtOk || srcOk === tgtOk) continue;
        const key = `${srcOk}→${tgtOk}`;
        if (edgeSet.has(key)) continue;
        edgeSet.add(key);
        const srcNode = nodes3d.find(n => n.id === srcOk);
        const tgtNode = nodes3d.find(n => n.id === tgtOk);
        const isCross = srcNode?._domain !== tgtNode?._domain;
        const eStyle = ASTGalaxyView.EDGE_STYLE[e.type] || ASTGalaxyView.DEFAULT_EDGE;
        edges3d.push({
          source: srcOk, target: tgtOk,
          color: isCross ? '#f59e0b' : (eStyle.color || '#475569'),
          width: isCross ? 2 : 1,
          _type: e.type, _isCross: isCross,
        });
      }
    }

    // ── V3 state ──
    this._v3FocusNode = null;

    // ── Build ForceGraph3D (city) ──
    console.log(`[3D-V3] City: ${nodes3d.length} nodes, ${edges3d.length} edges, ${domainKeys.length} blocks`);
    this.graph3d = ForceGraph3D()(container)
      .graphData({ nodes: nodes3d, links: edges3d })
      .backgroundColor('#030712')
      .showNavInfo(false)
      .nodeColor(n => {
        if (this._v3FocusNode) {
          if (n.id === this._v3FocusNode) return n.color;
          const connected = edges3d.some(e =>
            (e.source?.id || e.source) === this._v3FocusNode && (e.target?.id || e.target) === n.id ||
            (e.target?.id || e.target) === this._v3FocusNode && (e.source?.id || e.source) === n.id
          );
          return connected ? n.color : 'rgba(30,41,59,0.12)';
        }
        if (this._3dSearchTerm?.length >= 2) {
          return (n.name || '').toLowerCase().includes(this._3dSearchTerm) ? '#fbbf24' : 'rgba(30,41,59,0.2)';
        }
        return n.color;
      })
      .nodeVal(n => n.val)
      .nodeLabel(n => {
        if (n._isPlate) {
          const hm = n._hubMetrics || {};
          return `<div style="background:rgba(3,7,18,0.95);padding:12px 16px;border-radius:10px;border:2px solid ${n.color};color:#e2e8f0;font-size:13px;min-width:220px;">
            <div style="font-size:16px;font-weight:700;color:${n.color};">${n.name}</div>
            <div style="display:grid;grid-template-columns:1fr 1fr;gap:4px 12px;margin-top:6px;font-size:12px;">
              <div>🖥️ <b>${hm.programs}</b> programs</div>
              <div>📝 ${(hm.loc || 0).toLocaleString()} LOC</div>
              <div>🗄️ ${hm.sql || 0} SQL</div>
              <div>📞 ${hm.calls || 0} CALLs</div>
            </div>
          </div>`;
        }
        const mt = n._metrics || {};
        const cpx = Math.round((mt.complexity || 0) * 100);
        const cpxColor = cpx > 60 ? '#dc2626' : cpx > 40 ? '#f97316' : cpx > 20 ? '#eab308' : '#22c55e';
        return `<div style="background:rgba(3,7,18,0.95);padding:12px 16px;border-radius:10px;border:1px solid ${n.color};color:#e2e8f0;font-size:12px;min-width:250px;">
          <div style="font-size:14px;font-weight:700;color:#fbbf24;">🔧 ${n._data?.serviceDesc || ''}</div>
          <div style="color:#94a3b8;font-size:11px;font-style:italic;margin-bottom:6px;">${n._data?.serviceDetail || ''}</div>
          <div style="font-weight:600;color:${n.borderColor || n.color};font-size:13px;">${n._data?.displayName || ''}</div>
          <div style="color:#64748b;font-size:11px;margin-bottom:6px;">${n._domain || ''} · ${n._data?.riskTier || ''} Risk</div>
          <div style="display:grid;grid-template-columns:1fr 1fr;gap:3px 12px;font-size:11px;">
            <div>📝 <b>${(mt.lineCount || 0).toLocaleString()}</b> LOC</div>
            <div>🗄️ ${mt.sqlCount || 0} SQL</div>
            <div>📞 ${mt.callCount || 0} CALLs</div>
            <div>📂 ${mt.sectionCount || 0} sections</div>
          </div>
          <div style="margin-top:6px;">
            <div style="font-size:10px;color:#64748b;">Risk: <span style="color:${cpxColor};font-weight:700;">${cpx}%</span></div>
            <div style="height:5px;background:#1e293b;border-radius:3px;overflow:hidden;margin-top:2px;">
              <div style="height:100%;width:${cpx}%;background:${cpxColor};border-radius:3px;"></div>
            </div>
          </div>
        </div>`;
      })
      .nodeOpacity(0.95)
      .nodeThreeObject(n => {
        try {
          const THREE = window.THREE;
          if (!THREE) return undefined;
          const group = new THREE.Group();

          if (n._isPlate) {
            // Ground plate: flat box with domain color
            const plateGeom = new THREE.BoxGeometry(blockSize * 0.9, 1.5, blockSize * 0.9);
            const plateMat = new THREE.MeshPhongMaterial({
              color: new THREE.Color(n.color), emissive: new THREE.Color(n.color),
              emissiveIntensity: 0.3, transparent: true, opacity: 0.35,
            });
            group.add(new THREE.Mesh(plateGeom, plateMat));

            // Grid lines on plate
            const edgeGeom = new THREE.EdgesGeometry(plateGeom);
            const edgeMat = new THREE.LineBasicMaterial({ color: new THREE.Color(n.color), transparent: true, opacity: 0.4 });
            group.add(new THREE.LineSegments(edgeGeom, edgeMat));

            // Domain label
            const canvas = document.createElement('canvas');
            canvas.width = 512; canvas.height = 64;
            const ctx = canvas.getContext('2d');
            ctx.font = 'bold 32px system-ui, sans-serif';
            ctx.fillStyle = n.color;
            ctx.textAlign = 'center';
            ctx.fillText(n.name, 256, 30);
            ctx.font = '20px system-ui, sans-serif';
            ctx.fillStyle = '#94a3b8';
            ctx.fillText(`${n._hubMetrics?.programs || 0} programs · ${(n._hubMetrics?.loc || 0).toLocaleString()} LOC`, 256, 56);
            const texture = new THREE.CanvasTexture(canvas);
            texture.minFilter = THREE.LinearFilter;
            const spriteMat = new THREE.SpriteMaterial({ map: texture, transparent: true });
            const sprite = new THREE.Sprite(spriteMat);
            sprite.scale.set(50, 6, 1);
            sprite.position.y = 5;
            group.add(sprite);

          } else if (n._isBuilding) {
            const h = n._buildingHeight || 10;
            const w = n._buildingWidth || 4;
            const tierColor = n._tier?.color || '#64748b';
            const domColor = n.borderColor || n.color;

            // Building body
            const bodyGeom = new THREE.BoxGeometry(w, h, w);
            const bodyMat = new THREE.MeshPhongMaterial({
              color: new THREE.Color(tierColor),
              emissive: new THREE.Color(tierColor),
              emissiveIntensity: 0.25,
              transparent: true, opacity: 0.88,
            });
            group.add(new THREE.Mesh(bodyGeom, bodyMat));

            // Wireframe overlay for building feel
            const wireGeom = new THREE.EdgesGeometry(bodyGeom);
            const wireMat = new THREE.LineBasicMaterial({
              color: new THREE.Color(domColor), transparent: true, opacity: 0.35,
            });
            group.add(new THREE.LineSegments(wireGeom, wireMat));

            // Window dots on facade (for visual flair)
            const floors = Math.min(Math.floor(h / 3), 12);
            for (let fi = 0; fi < floors; fi++) {
              const wy = -h / 2 + 2 + fi * (h - 2) / floors;
              for (let wi = 0; wi < 3; wi++) {
                const wx = -w * 0.35 + wi * w * 0.35;
                const winGeom = new THREE.PlaneGeometry(w * 0.12, 1.2);
                const winMat = new THREE.MeshBasicMaterial({
                  color: 0xfbbf24, transparent: true, opacity: 0.2 + Math.random() * 0.3,
                  side: THREE.DoubleSide,
                });
                const win = new THREE.Mesh(winGeom, winMat);
                win.position.set(wx, wy, w / 2 + 0.01);
                group.add(win);
              }
            }

            // Roof beacon for critical risk
            if (n._complexity > 0.5) {
              const beaconGeom = new THREE.SphereGeometry(1.2, 8, 6);
              const beaconMat = new THREE.MeshBasicMaterial({
                color: new THREE.Color(tierColor), transparent: true, opacity: 0.9,
              });
              const beacon = new THREE.Mesh(beaconGeom, beaconMat);
              beacon.position.y = h / 2 + 1.5;
              group.add(beacon);
              // Beacon glow
              const glowGeom = new THREE.SphereGeometry(3, 8, 6);
              const glowMat = new THREE.MeshBasicMaterial({
                color: new THREE.Color(tierColor), transparent: true, opacity: 0.12,
              });
              const glow = new THREE.Mesh(glowGeom, glowMat);
              glow.position.y = h / 2 + 1.5;
              group.add(glow);
            }

            // Label above building
            const canvas = document.createElement('canvas');
            canvas.width = 400; canvas.height = 52;
            const ctx = canvas.getContext('2d');
            ctx.fillStyle = 'rgba(3,7,18,0.8)';
            ctx.fillRect(0, 0, 400, 52);
            ctx.fillStyle = tierColor;
            ctx.fillRect(0, 0, 4, 52);
            ctx.font = 'bold 17px system-ui, sans-serif';
            ctx.fillStyle = '#fbbf24';
            ctx.textAlign = 'left';
            ctx.fillText(`🔧 ${(n._data?.serviceDesc || '').slice(0, 26)}`, 10, 18);
            ctx.font = '14px system-ui, sans-serif';
            ctx.fillStyle = '#94a3b8';
            const mt = n._metrics || {};
            ctx.fillText(`${n._data?.displayName || ''} · ${(mt.lineCount || 0).toLocaleString()} LOC · ${Math.round(n._complexity * 100)}%`, 10, 40);
            const texture = new THREE.CanvasTexture(canvas);
            texture.minFilter = THREE.LinearFilter;
            const spriteMat = new THREE.SpriteMaterial({ map: texture, transparent: true });
            const sprite = new THREE.Sprite(spriteMat);
            sprite.scale.set(22, 2.8, 1);
            sprite.position.y = h / 2 + 5;
            group.add(sprite);
          }
          return group;
        } catch { return undefined; }
      })
      .nodeThreeObjectExtend(false)
      // Edges: data flow lines between buildings
      .linkColor(l => {
        if (this._v3FocusNode) {
          const src = l.source?.id || l.source;
          const tgt = l.target?.id || l.target;
          if (src !== this._v3FocusNode && tgt !== this._v3FocusNode) return 'rgba(30,41,59,0.03)';
        }
        return l._isCross ? '#f59e0b' : (l.color || '#475569');
      })
      .linkOpacity(l => l._isCross ? 0.6 : 0.35)
      .linkWidth(l => l._isCross ? 2 : 0.8)
      .linkDirectionalArrowLength(4)
      .linkDirectionalArrowRelPos(1)
      .linkDirectionalParticles(l => l._isCross ? 3 : 1)
      .linkDirectionalParticleWidth(l => l._isCross ? 2.5 : 1.2)
      .linkDirectionalParticleColor(l => l._isCross ? '#fbbf24' : '#60a5fa')
      .linkDirectionalParticleSpeed(0.005)
      .linkCurvature(l => l._isCross ? 0.2 : 0)
      // Interaction
      .onNodeClick(node => {
        if (node._data) {
          this._updateInspector(node._data);
          this._updateSourcePanel(node._data);
        }
        this._v3FocusNode = this._v3FocusNode === node.id ? null : node.id;
        this.graph3d.nodeColor(this.graph3d.nodeColor());
        this.graph3d.linkColor(this.graph3d.linkColor());
        const pos = { x: node.fx || node.x || 0, y: node.fy || node.y || 0, z: node.fz || node.z || 0 };
        const dist = node._isPlate ? 200 : 80;
        this.graph3d.cameraPosition(
          { x: pos.x + dist * 0.5, y: pos.y + dist, z: pos.z + dist },
          pos, 1000
        );
      })
      .onNodeDoubleClick(node => {
        if (node._data?.program && typeof astExplorer !== 'undefined' && astExplorer) {
          astExplorer.drillIntoProgram(node._data.program);
        }
      })
      .width(container.offsetWidth)
      .height(container.offsetHeight);

    // ── Camera: bird's eye view of the city ──
    const totalWidth = cols * (blockSize + streetWidth);
    setTimeout(() => {
      if (this.graph3d) {
        this.graph3d.cameraPosition(
          { x: totalWidth * 0.3, y: 250, z: totalWidth * 0.8 },
          { x: 0, y: 20, z: 0 }, 2000
        );
      }
    }, 300);

    // ── Lights + ground ──
    try {
      const THREE = window.THREE;
      if (THREE && this.graph3d.scene) {
        setTimeout(() => {
          // Ground plane
          const groundGeom = new THREE.PlaneGeometry(totalWidth * 2, totalWidth * 2);
          const groundMat = new THREE.MeshBasicMaterial({
            color: 0x0a0e1a, transparent: true, opacity: 0.8, side: THREE.DoubleSide,
          });
          const ground = new THREE.Mesh(groundGeom, groundMat);
          ground.rotation.x = Math.PI / 2;
          ground.position.y = -2;
          this.graph3d.scene().add(ground);

          // Grid on ground
          const gridHelper = new THREE.GridHelper(totalWidth * 1.5, 30, 0x1e293b, 0x0f172a);
          gridHelper.position.y = -1.5;
          this.graph3d.scene().add(gridHelper);

          // Lights
          this.graph3d.scene().add(new THREE.AmbientLight(0x404060, 2.0));
          const dl = new THREE.DirectionalLight(0xffffff, 0.8);
          dl.position.set(200, 400, 200);
          this.graph3d.scene().add(dl);
          const dl2 = new THREE.PointLight(0xf59e0b, 0.4, 800);
          dl2.position.set(0, 150, 0);
          this.graph3d.scene().add(dl2);
        }, 150);
      }
    } catch {}

    // Focus reset button
    const focusBtn = document.createElement('button');
    focusBtn.textContent = '🔍 Reset View';
    focusBtn.style.cssText = 'position:absolute;bottom:12px;left:12px;z-index:20;padding:6px 12px;background:rgba(30,41,59,0.85);color:#e2e8f0;border:1px solid #475569;border-radius:6px;font-size:12px;cursor:pointer;';
    focusBtn.onclick = () => {
      this._v3FocusNode = null;
      this.graph3d.nodeColor(this.graph3d.nodeColor());
      this.graph3d.linkColor(this.graph3d.linkColor());
      this.graph3d.cameraPosition(
        { x: totalWidth * 0.3, y: 250, z: totalWidth * 0.8 },
        { x: 0, y: 20, z: 0 }, 1500
      );
    };
    container.appendChild(focusBtn);

    // Resize
    const ro = new ResizeObserver(() => {
      if (this.graph3d) this.graph3d.width(container.offsetWidth).height(container.offsetHeight);
    });
    ro.observe(container);
  }

  // ═══════════════════════════════════════════════════════════════════
  // 3D SERVICE CATALOG V2 — Z-layered, LOD zoom, focus mode, semantic overlays
  // ═══════════════════════════════════════════════════════════════════

  // ── Layer classification ──
  _classifyLayer(progName, meta) {
    const upper = (progName || '').toUpperCase().replace('FLOW-AST-','').replace('.CBL','').replace('.CPY','');
    if (meta?.isCopybook) return 3;
    // Data access: heavy SQL, batch jobs
    if ((meta?.sqlCount || 0) > 20) return 3;
    if (upper.match(/^CB/)) return 3; // Batch/copybook programs
    // Entry / UI: CICS screens, sign-on, menus
    if (upper.match(/^CO/) && upper.match(/00C$/)) return 0;
    if (upper.includes('SGN') || upper.includes('MENU') || upper.includes('SIGN')) return 0;
    // Utilities / shared
    if (upper.includes('UTIL') || upper.includes('WAIT') || upper.includes('ABND') || upper.includes('CSUT') || upper.includes('DATE')) return 2;
    // Everything else → business logic
    return 1;
  }

  static LAYER_CONFIG = [
    { z: 0, label: 'Entry / UI', color: '#3b82f6', icon: '🖥️' },
    { z: 1, label: 'Business Logic', color: '#10b981', icon: '⚙️' },
    { z: 2, label: 'Utilities', color: '#f59e0b', icon: '🔧' },
    { z: 3, label: 'Data Access', color: '#a855f7', icon: '🗄️' },
  ];

  _render3DServiceCatalogV2(container) {
    const programs = this.galaxyData?.programs || [];
    const seen = new Set();
    const uniqueProgs = [];
    for (const p of programs) {
      if (seen.has(p.program)) continue;
      seen.add(p.program);
      uniqueProgs.push(p);
    }

    // ── Classify each program into domain + layer ──
    const domainMap = new Map();
    const layerMap = new Map(); // layer → programs
    const progIndex = new Map(); // progId → enriched data

    for (const p of uniqueProgs) {
      const domain = this._classifyBusinessDomain(p.program, p);
      const layer = this._classifyLayer(p.program, p);
      const complexity = this._computeComplexity(p);
      const svc = this._describeProgram(p.program, p);
      const displayName = p.program.replace('flow-ast-', '').replace('.cbl', '');
      const progId = `prog__${p.program}`;

      const enriched = { ...p, domain, layer, complexity, svc, displayName, progId };
      progIndex.set(progId, enriched);

      if (!domainMap.has(domain)) domainMap.set(domain, []);
      domainMap.get(domain).push(enriched);
      if (!layerMap.has(layer)) layerMap.set(layer, []);
      layerMap.get(layer).push(enriched);
    }

    // ── Build dependency graph (for strength calc + edge data) ──
    const depCount = new Map(); // progId → number of inbound+outbound edges
    const edgeData = [];
    if (this.galaxyData?.edges) {
      for (const e of this.galaxyData.edges) {
        const srcId = `prog__${e.source}`;
        const tgtId = `prog__${e.target}`;
        const srcOk = progIndex.has(srcId) ? srcId : [...progIndex.keys()].find(k => k.includes(e.source?.replace('.cbl','')));
        const tgtOk = progIndex.has(tgtId) ? tgtId : [...progIndex.keys()].find(k => k.includes(e.target?.replace('.cbl','')));
        if (srcOk && tgtOk && srcOk !== tgtOk) {
          edgeData.push({ source: srcOk, target: tgtOk, type: e.type });
          depCount.set(srcOk, (depCount.get(srcOk) || 0) + 1);
          depCount.set(tgtOk, (depCount.get(tgtOk) || 0) + 1);
        }
      }
    }

    // ── Position: domains on X-ring, layers on Y-axis ──
    const nodes3d = [];
    const edges3d = [];
    const nodeIdSet = new Set();

    const domainKeys = [...domainMap.keys()];
    const domainAngles = new Map();
    for (let di = 0; di < domainKeys.length; di++) {
      domainAngles.set(domainKeys[di], (di / domainKeys.length) * Math.PI * 2 - Math.PI / 2);
    }

    const layerSpacing = 120;  // Y distance between layers
    const domainSpread = 200;  // X/Z spread per domain

    // ── Domain hub nodes (aggregate, visible when zoomed out) ──
    for (let di = 0; di < domainKeys.length; di++) {
      const domain = domainKeys[di];
      const cfg = ASTGalaxyView.BUSINESS_DOMAINS[domain] || { color: '#64748b', icon: '•' };
      const members = domainMap.get(domain);
      const angle = domainAngles.get(domain);
      const hubX = Math.cos(angle) * domainSpread * 0.6;
      const hubZ = Math.sin(angle) * domainSpread * 0.6;
      const avgLayer = members.reduce((s, m) => s + m.layer, 0) / members.length;
      const hubY = -avgLayer * layerSpacing;
      const hubId = `hub__${domain.replace(/\s/g,'_')}`;

      const totalSQL = members.reduce((s, m) => s + (m.sqlCount || 0), 0);
      const totalCALL = members.reduce((s, m) => s + (m.callCount || 0), 0);
      const totalLOC = members.reduce((s, m) => s + (m.lineCount || 0), 0);

      nodes3d.push({
        id: hubId,
        name: `${cfg.icon} ${domain}`,
        color: cfg.color,
        val: 30 + Math.sqrt(members.length) * 5,
        fx: hubX, fy: hubY, fz: hubZ,
        _data: { nodeType: 'DOMAIN', displayName: domain, domain, programCount: members.length,
                 sqlCount: totalSQL, callCount: totalCALL, lineCount: totalLOC },
        _nodeType: 'DOMAIN', _domain: domain, _isHub: true,
        _hubMetrics: { programs: members.length, sql: totalSQL, calls: totalCALL, loc: totalLOC },
        _v2: true, _v2LOD: 'far',
      });
      nodeIdSet.add(hubId);
    }

    // ── Program nodes — positioned by domain angle + layer Y ──
    for (const [domain, members] of domainMap.entries()) {
      const cfg = ASTGalaxyView.BUSINESS_DOMAINS[domain] || { color: '#64748b', icon: '•' };
      const angle = domainAngles.get(domain);
      const hubId = `hub__${domain.replace(/\s/g,'_')}`;

      // Sort members by layer for visual coherence
      members.sort((a, b) => a.layer - b.layer);

      for (let mi = 0; mi < members.length; mi++) {
        const m = members[mi];
        const progId = m.progId;
        if (nodeIdSet.has(progId)) continue;

        // Position: spread within domain sector at correct layer height
        const layerY = -m.layer * layerSpacing;
        const spreadAngle = angle + ((mi / members.length) - 0.5) * 0.8;
        const spreadR = domainSpread * (0.5 + Math.random() * 0.5);
        const px = Math.cos(spreadAngle) * spreadR;
        const pz = Math.sin(spreadAngle) * spreadR;

        // Size by complexity + LOC
        const size = Math.max(4, Math.min(20, 4 + Math.sqrt(m.nodeCount || 0) * 0.4 + m.complexity * 10));

        // Color by complexity (semantic overlay: red=high, yellow=mid, green=low, blue=data)
        let semanticColor;
        if (m.layer === 3) {
          semanticColor = '#818cf8'; // data layer → blue-indigo
        } else if (m.complexity > 0.6) {
          semanticColor = '#ef4444'; // high → red
        } else if (m.complexity > 0.3) {
          semanticColor = '#f59e0b'; // medium → amber
        } else {
          semanticColor = '#10b981'; // simple → green
        }

        nodes3d.push({
          id: progId,
          name: `${m.svc.desc}\n${m.displayName}`,
          color: semanticColor,
          borderColor: cfg.color,
          val: size,
          fx: px, fy: layerY, fz: pz,
          _data: { ...m, program: m.program, nodeType: m.isCopybook ? 'COPYBOOK' : 'PROGRAM',
                   displayName: m.displayName, domain, serviceDesc: m.svc.desc, serviceDetail: m.svc.detail,
                   layer: m.layer, complexity: m.complexity },
          _nodeType: m.isCopybook ? 'COPYBOOK' : 'PROGRAM',
          _domain: domain, _layer: m.layer,
          _isHub: false, _v2: true, _v2LOD: 'mid',
          _complexity: m.complexity,
          _semanticColor: semanticColor,
          _domainColor: cfg.color,
          _depStrength: depCount.get(progId) || 0,
          _metrics: {
            sqlCount: m.sqlCount || 0, callCount: m.callCount || 0,
            sectionCount: m.sectionCount || 0, paraCount: m.paraCount || 0,
            performCount: m.performCount || 0, branchCount: m.branchCount || 0,
            lineCount: m.lineCount || 0, nodeCount: m.nodeCount || 0,
            complexity: m.complexity,
          },
        });
        nodeIdSet.add(progId);

        // Hub → program edge (thin, for cluster grouping)
        edges3d.push({
          source: hubId, target: progId,
          color: cfg.color, width: 0.3, _type: 'CLUSTER', _intraCluster: true,
        });
      }
    }

    // ── Edges: inter-program dependencies ──
    // Rank by strength: edges with higher dep counts are "stronger"
    const edgeSet = new Set();
    const rankedEdges = edgeData.map(e => ({
      ...e,
      strength: (depCount.get(e.source) || 0) + (depCount.get(e.target) || 0),
    })).sort((a, b) => b.strength - a.strength);

    // Show top N strongest edges (scalable for large graphs)
    const maxEdges = Math.min(rankedEdges.length, Math.max(80, uniqueProgs.length * 2));
    for (let ei = 0; ei < maxEdges; ei++) {
      const e = rankedEdges[ei];
      const key = `${e.source}→${e.target}`;
      if (edgeSet.has(key) || !nodeIdSet.has(e.source) || !nodeIdSet.has(e.target)) continue;
      edgeSet.add(key);
      const srcNode = nodes3d.find(n => n.id === e.source);
      const tgtNode = nodes3d.find(n => n.id === e.target);
      const isCross = srcNode?._domain !== tgtNode?._domain;
      const isLayerCross = srcNode?._layer !== tgtNode?._layer;
      const eStyle = ASTGalaxyView.EDGE_STYLE[e.type] || ASTGalaxyView.DEFAULT_EDGE;

      edges3d.push({
        source: e.source, target: e.target,
        color: isCross ? '#f59e0b' : (eStyle.color || '#475569'),
        width: isCross ? 2 : isLayerCross ? 1.2 : 0.7,
        _type: e.type, _isCross: isCross, _isLayerCross: isLayerCross,
        _strength: e.strength,
      });
    }

    // ── V2 State: focus mode, overlay toggle ──
    this._v2FocusNode = null;
    this._v2OverlayEnabled = true;

    // ── Build ForceGraph3D (V2) ──
    console.log(`[3D-V2] nodes: ${nodes3d.length} (hubs: ${nodes3d.filter(n=>n._isHub).length}, progs: ${nodes3d.filter(n=>!n._isHub).length}) | edges: ${edges3d.length} | layers: ${[...layerMap.keys()].sort()}`);
    this.graph3d = ForceGraph3D()(container)
      .graphData({ nodes: nodes3d, links: edges3d })
      .backgroundColor('#030712')
      .showNavInfo(false)
      .nodeColor(n => {
        // Focus mode: fade unrelated nodes
        if (this._v2FocusNode) {
          if (n.id === this._v2FocusNode) return n._semanticColor || n.color;
          const isConnected = edges3d.some(e =>
            (e.source?.id || e.source) === this._v2FocusNode && (e.target?.id || e.target) === n.id ||
            (e.target?.id || e.target) === this._v2FocusNode && (e.source?.id || e.source) === n.id
          );
          return isConnected ? (n._semanticColor || n.color) : 'rgba(30,41,59,0.15)';
        }
        // Search
        if (this._3dSearchTerm?.length >= 2) {
          return (n.name || '').toLowerCase().includes(this._3dSearchTerm) ? '#fbbf24' : 'rgba(30,41,59,0.25)';
        }
        return this._v2OverlayEnabled ? (n._semanticColor || n.color) : (n._domainColor || n.color);
      })
      .nodeVal(n => {
        if (this._v2FocusNode) {
          if (n.id === this._v2FocusNode) return (n.val || 8) * 2;
          const isConnected = edges3d.some(e =>
            (e.source?.id || e.source) === this._v2FocusNode && (e.target?.id || e.target) === n.id ||
            (e.target?.id || e.target) === this._v2FocusNode && (e.source?.id || e.source) === n.id
          );
          return isConnected ? (n.val || 6) : (n.val || 6) * 0.2;
        }
        if (this._3dSearchTerm?.length >= 2) {
          return (n.name || '').toLowerCase().includes(this._3dSearchTerm) ? (n.val || 6) * 2.5 : (n.val || 6) * 0.3;
        }
        return n.val;
      })
      .nodeLabel(n => {
        const d = n._data || {};
        const mt = n._metrics || {};
        if (n._isHub) {
          const hm = n._hubMetrics || {};
          return `<div style="background:rgba(3,7,18,0.95);padding:12px 16px;border-radius:10px;border:2px solid ${n.color};color:#e2e8f0;font-size:13px;min-width:240px;backdrop-filter:blur(8px);">
            <div style="font-size:18px;font-weight:700;color:${n.color};margin-bottom:8px;">${n.name}</div>
            <div style="display:grid;grid-template-columns:1fr 1fr;gap:4px 16px;font-size:12px;">
              <div>🖥️ <b>${hm.programs}</b> programs</div>
              <div>📝 ${(hm.loc || 0).toLocaleString()} LOC</div>
              <div>🗄️ <span style="color:#a855f7;">${hm.sql || 0}</span> SQL</div>
              <div>📞 <span style="color:#ef4444;">${hm.calls || 0}</span> CALLs</div>
            </div>
            <div style="margin-top:8px;font-size:10px;color:#475569;">Click to focus · Double-click to drill</div>
          </div>`;
        }
        const cpx = Math.round((mt.complexity || d.complexity || 0) * 100);
        const cpxColor = cpx > 60 ? '#ef4444' : cpx > 30 ? '#f59e0b' : '#10b981';
        const layerCfg = ASTGalaxyView.LAYER_CONFIG[d.layer] || ASTGalaxyView.LAYER_CONFIG[1];
        return `<div style="background:rgba(3,7,18,0.95);padding:12px 16px;border-radius:10px;border:1px solid ${n.color};color:#e2e8f0;font-size:12px;min-width:260px;backdrop-filter:blur(8px);">
          <div style="font-size:15px;font-weight:700;color:#fbbf24;margin-bottom:2px;">🔧 ${d.serviceDesc || d.displayName}</div>
          <div style="color:#94a3b8;font-size:11px;margin-bottom:6px;font-style:italic;">${d.serviceDetail || ''}</div>
          <div style="font-weight:600;color:${n._domainColor || n.color};font-size:13px;">${d.displayName}</div>
          <div style="color:#64748b;font-size:11px;margin-bottom:8px;">${d.domain || ''} · ${layerCfg.icon} ${layerCfg.label}</div>
          <div style="display:grid;grid-template-columns:1fr 1fr;gap:3px 12px;font-size:11px;">
            <div>📂 <span style="color:#8b5cf6;">${mt.sectionCount || 0}</span> sections</div>
            <div>¶ <span style="color:#10b981;">${mt.paraCount || 0}</span> paragraphs</div>
            <div>🗄️ <span style="color:#a855f7;">${mt.sqlCount || 0}</span> SQL</div>
            <div>📞 <span style="color:#ef4444;">${mt.callCount || 0}</span> CALLs</div>
            <div>🔄 <span style="color:#06b6d4;">${mt.performCount || 0}</span> PERFORMs</div>
            <div>📝 <span style="color:#94a3b8;">${(mt.lineCount || 0).toLocaleString()}</span> LOC</div>
          </div>
          <div style="margin-top:6px;">
            <div style="font-size:10px;color:#64748b;margin-bottom:2px;">Complexity: <span style="color:${cpxColor};font-weight:600;">${cpx}%</span></div>
            <div style="height:4px;background:#1e293b;border-radius:2px;overflow:hidden;">
              <div style="height:100%;width:${cpx}%;background:${cpxColor};border-radius:2px;"></div>
            </div>
          </div>
          <div style="margin-top:6px;font-size:10px;color:#475569;">🔗 ${n._depStrength || 0} connections · Click to focus</div>
        </div>`;
      })
      .nodeOpacity(0.95)
      .nodeThreeObject(n => {
        try {
          const THREE = window.THREE;
          if (!THREE) return undefined;
          const group = new THREE.Group();

          if (n._isHub) {
            // ── Domain hub: translucent sphere + ring + label ──
            const hubR = 14 + Math.sqrt(n._hubMetrics?.programs || 1) * 2;
            const sphereGeom = new THREE.SphereGeometry(hubR, 32, 24);
            const sphereMat = new THREE.MeshPhongMaterial({
              color: new THREE.Color(n.color), emissive: new THREE.Color(n.color),
              emissiveIntensity: 0.6, transparent: true, opacity: 0.25,
            });
            group.add(new THREE.Mesh(sphereGeom, sphereMat));
            // Core
            const coreGeom = new THREE.SphereGeometry(hubR * 0.25, 16, 12);
            const coreMat = new THREE.MeshPhongMaterial({
              color: new THREE.Color(n.color), emissive: new THREE.Color(n.color),
              emissiveIntensity: 1.0, transparent: true, opacity: 0.9,
            });
            group.add(new THREE.Mesh(coreGeom, coreMat));
            // Ring
            const ringGeom = new THREE.TorusGeometry(hubR * 1.15, 0.25, 8, 48);
            const ringMat = new THREE.MeshBasicMaterial({ color: new THREE.Color(n.color), transparent: true, opacity: 0.2 });
            const ring = new THREE.Mesh(ringGeom, ringMat);
            ring.rotation.x = Math.PI / 2;
            group.add(ring);
            // Label
            const canvas = document.createElement('canvas');
            canvas.width = 512; canvas.height = 64;
            const ctx = canvas.getContext('2d');
            ctx.fillStyle = 'rgba(3,7,18,0.85)';
            ctx.fillRect(0, 0, 512, 64);
            ctx.font = 'bold 28px system-ui, sans-serif';
            ctx.fillStyle = n.color;
            ctx.textAlign = 'center';
            ctx.fillText(n.name, 256, 28);
            ctx.font = '18px system-ui, sans-serif';
            ctx.fillStyle = '#94a3b8';
            ctx.fillText(`${n._hubMetrics?.programs || 0} programs · ${(n._hubMetrics?.loc || 0).toLocaleString()} LOC`, 256, 52);
            const texture = new THREE.CanvasTexture(canvas);
            texture.minFilter = THREE.LinearFilter;
            const spriteMat = new THREE.SpriteMaterial({ map: texture, transparent: true });
            const sprite = new THREE.Sprite(spriteMat);
            sprite.scale.set(40, 5, 1);
            sprite.position.y = hubR + 6;
            group.add(sprite);
          } else {
            // ── Program node: shape + color by complexity ──
            const r = Math.max(1.5, (n.val || 6) * 0.25);
            const cpx = n._complexity || 0;
            const cpxColor = cpx > 0.6 ? '#ef4444' : cpx > 0.3 ? '#f59e0b' : '#10b981';

            // Geometry: vary by layer for visual distinction
            let geom;
            const layer = n._layer;
            if (layer === 0) {
              geom = new THREE.BoxGeometry(r * 2, r * 2, r * 0.5); // flat card → UI
            } else if (layer === 3) {
              geom = new THREE.CylinderGeometry(r * 0.8, r, r * 1.6, 8); // cylinder → data
            } else if (layer === 2) {
              geom = new THREE.OctahedronGeometry(r, 0); // octahedron → utility
            } else {
              geom = new THREE.SphereGeometry(r, 16, 12); // sphere → business logic
            }

            const color = this._v2OverlayEnabled ? (n._semanticColor || n.color) : (n._domainColor || n.color);
            const mat = new THREE.MeshPhongMaterial({
              color: new THREE.Color(color),
              emissive: new THREE.Color(color),
              emissiveIntensity: this._v2FocusNode === n.id ? 0.9 : 0.45,
              transparent: true,
              opacity: this._v2FocusNode && this._v2FocusNode !== n.id ? 0.15 : 0.9,
            });
            group.add(new THREE.Mesh(geom, mat));

            // Complexity ring
            if (cpx > 0.2) {
              const ringGeom = new THREE.TorusGeometry(r * 1.4, 0.15, 8, 32);
              const ringMat = new THREE.MeshBasicMaterial({
                color: new THREE.Color(cpxColor), transparent: true, opacity: 0.5,
              });
              const ring = new THREE.Mesh(ringGeom, ringMat);
              ring.rotation.x = Math.PI / 2;
              group.add(ring);
            }

            // Label: service desc + program name
            const canvas = document.createElement('canvas');
            canvas.width = 400; canvas.height = 56;
            const ctx = canvas.getContext('2d');
            ctx.fillStyle = 'rgba(3,7,18,0.8)';
            ctx.fillRect(0, 0, 400, 56);
            // Left accent
            ctx.fillStyle = n._domainColor || n.color;
            ctx.fillRect(0, 0, 4, 56);
            // Service description
            ctx.font = 'bold 18px system-ui, sans-serif';
            ctx.fillStyle = '#fbbf24';
            ctx.textAlign = 'left';
            const svcText = (n._data?.serviceDesc || n._data?.displayName || '').slice(0, 28);
            ctx.fillText(`🔧 ${svcText}`, 10, 20);
            // Program name + metrics
            ctx.font = '14px system-ui, sans-serif';
            ctx.fillStyle = '#94a3b8';
            const mt = n._metrics || {};
            const parts = [n._data?.displayName || ''];
            if (mt.sqlCount) parts.push(`${mt.sqlCount}SQL`);
            if (mt.callCount) parts.push(`${mt.callCount}CALL`);
            ctx.fillText(parts.join(' · '), 10, 42);

            const texture = new THREE.CanvasTexture(canvas);
            texture.minFilter = THREE.LinearFilter;
            const spriteMat = new THREE.SpriteMaterial({ map: texture, transparent: true });
            const sprite = new THREE.Sprite(spriteMat);
            sprite.scale.set(22, 3, 1);
            sprite.position.y = r + 2.5;
            group.add(sprite);
          }
          return group;
        } catch { return undefined; }
      })
      .nodeThreeObjectExtend(false)
      // Links
      .linkColor(l => {
        if (l._intraCluster) return l.color;
        if (this._v2FocusNode) {
          const src = l.source?.id || l.source;
          const tgt = l.target?.id || l.target;
          if (src !== this._v2FocusNode && tgt !== this._v2FocusNode) return 'rgba(30,41,59,0.05)';
        }
        return l._isCross ? '#f59e0b' : (l.color || '#475569');
      })
      .linkOpacity(l => {
        if (l._intraCluster) return 0.06;
        if (this._v2FocusNode) {
          const src = l.source?.id || l.source;
          const tgt = l.target?.id || l.target;
          if (src !== this._v2FocusNode && tgt !== this._v2FocusNode) return 0.02;
        }
        return l._isCross ? 0.7 : 0.4;
      })
      .linkWidth(l => {
        if (l._intraCluster) return 0.15;
        return l._isCross ? 2 : (l._isLayerCross ? 1.2 : 0.6);
      })
      .linkDirectionalArrowLength(l => l._intraCluster ? 0 : 4)
      .linkDirectionalArrowRelPos(1)
      .linkDirectionalParticles(l => {
        if (l._intraCluster) return 0;
        if (l._isCross) return 3;
        return l._type === 'CALL' ? 2 : 0;
      })
      .linkDirectionalParticleWidth(l => l._isCross ? 2.5 : 1.5)
      .linkDirectionalParticleColor(l => l._isCross ? '#fbbf24' : '#60a5fa')
      .linkDirectionalParticleSpeed(0.005)
      .linkCurvature(l => l._isCross ? 0.15 : 0) // curve cross-domain edges
      // Interaction
      .onNodeClick(node => {
        if (node._data) {
          this._updateInspector(node._data);
          this._updateSourcePanel(node._data);
        }
        // Toggle focus mode
        if (this._v2FocusNode === node.id) {
          this._v2FocusNode = null; // unfocus
        } else {
          this._v2FocusNode = node.id;
        }
        // Re-render visuals
        this.graph3d.nodeColor(this.graph3d.nodeColor());
        this.graph3d.nodeVal(this.graph3d.nodeVal());
        this.graph3d.linkColor(this.graph3d.linkColor());
        this.graph3d.linkOpacity(this.graph3d.linkOpacity());
        // Zoom to node
        const pos = { x: node.fx || node.x || 0, y: node.fy || node.y || 0, z: node.fz || node.z || 0 };
        const dist = node._isHub ? 200 : 100;
        const hypot = Math.hypot(pos.x, pos.y, pos.z) || 1;
        const ratio = 1 + dist / hypot;
        this.graph3d.cameraPosition(
          { x: pos.x * ratio, y: pos.y * ratio + 30, z: pos.z * ratio },
          pos, 1000
        );
      })
      .onNodeDoubleClick(node => {
        if (node._data?.program && typeof astExplorer !== 'undefined' && astExplorer) {
          astExplorer.drillIntoProgram(node._data.program);
        }
      })
      .width(container.offsetWidth)
      .height(container.offsetHeight);

    // ── Camera: elevated angle showing all layers ──
    setTimeout(() => {
      if (this.graph3d) {
        this.graph3d.cameraPosition({ x: 0, y: 200, z: 600 }, { x: 0, y: -layerSpacing, z: 0 }, 2000);
      }
    }, 300);

    // ── Scene additions: layer planes, cluster bubbles, lights ──
    try {
      const THREE = window.THREE;
      if (THREE && this.graph3d.scene) {
        setTimeout(() => {
          // Translucent layer planes
          for (const lc of ASTGalaxyView.LAYER_CONFIG) {
            const planeY = -lc.z * layerSpacing;
            const planeGeom = new THREE.PlaneGeometry(domainSpread * 3.5, domainSpread * 3.5);
            const planeMat = new THREE.MeshBasicMaterial({
              color: new THREE.Color(lc.color), transparent: true, opacity: 0.015, side: THREE.DoubleSide,
            });
            const plane = new THREE.Mesh(planeGeom, planeMat);
            plane.rotation.x = Math.PI / 2;
            plane.position.y = planeY;
            this.graph3d.scene().add(plane);

            // Layer label
            const canvas = document.createElement('canvas');
            canvas.width = 512; canvas.height = 48;
            const ctx = canvas.getContext('2d');
            ctx.font = 'bold 26px system-ui, sans-serif';
            ctx.fillStyle = lc.color;
            ctx.globalAlpha = 0.6;
            ctx.textAlign = 'center';
            ctx.fillText(`${lc.icon}  ${lc.label}  ─  Layer ${lc.z}`, 256, 30);
            const texture = new THREE.CanvasTexture(canvas);
            texture.minFilter = THREE.LinearFilter;
            const spriteMat = new THREE.SpriteMaterial({ map: texture, transparent: true });
            const sprite = new THREE.Sprite(spriteMat);
            sprite.scale.set(80, 8, 1);
            sprite.position.set(-domainSpread * 1.5, planeY + 2, -domainSpread * 1.5);
            this.graph3d.scene().add(sprite);
          }

          // Domain cluster wireframe bubbles
          for (const [domain, members] of domainMap.entries()) {
            const cfg = ASTGalaxyView.BUSINESS_DOMAINS[domain] || { color: '#64748b' };
            const angle = domainAngles.get(domain);
            const cx = Math.cos(angle) * domainSpread * 0.6;
            const cz = Math.sin(angle) * domainSpread * 0.6;
            const avgLayer = members.reduce((s, m) => s + m.layer, 0) / members.length;
            const bubbleR = 40 + members.length * 6;
            const bubbleGeom = new THREE.SphereGeometry(bubbleR, 20, 14);
            const bubbleMat = new THREE.MeshBasicMaterial({
              color: new THREE.Color(cfg.color), transparent: true, opacity: 0.03, wireframe: true,
            });
            const bubble = new THREE.Mesh(bubbleGeom, bubbleMat);
            bubble.position.set(cx, -avgLayer * layerSpacing, cz);
            this.graph3d.scene().add(bubble);
          }

          // Lights
          this.graph3d.scene().add(new THREE.AmbientLight(0x303050, 1.8));
          const dl = new THREE.DirectionalLight(0xffffff, 0.9);
          dl.position.set(200, 500, 300);
          this.graph3d.scene().add(dl);
          const dl2 = new THREE.DirectionalLight(0x6060ff, 0.3);
          dl2.position.set(-200, -200, -200);
          this.graph3d.scene().add(dl2);
        }, 150);
      }
    } catch { /* decorations are optional */ }

    // ── Overlay toggle button (injected into container) ──
    const overlayBtn = document.createElement('button');
    overlayBtn.textContent = '🎨 Overlay: Complexity';
    overlayBtn.style.cssText = 'position:absolute;bottom:12px;left:12px;z-index:20;padding:6px 12px;background:rgba(30,41,59,0.85);color:#e2e8f0;border:1px solid #475569;border-radius:6px;font-size:12px;cursor:pointer;backdrop-filter:blur(4px);';
    overlayBtn.onclick = () => {
      this._v2OverlayEnabled = !this._v2OverlayEnabled;
      overlayBtn.textContent = this._v2OverlayEnabled ? '🎨 Overlay: Complexity' : '🏢 Overlay: Domain';
      this.graph3d.nodeColor(this.graph3d.nodeColor());
      this.graph3d.nodeThreeObject(this.graph3d.nodeThreeObject()); // rebuild meshes
    };
    container.appendChild(overlayBtn);

    // Focus reset button
    const focusBtn = document.createElement('button');
    focusBtn.textContent = '🔍 Focus: Off';
    focusBtn.style.cssText = 'position:absolute;bottom:12px;left:200px;z-index:20;padding:6px 12px;background:rgba(30,41,59,0.85);color:#e2e8f0;border:1px solid #475569;border-radius:6px;font-size:12px;cursor:pointer;backdrop-filter:blur(4px);';
    focusBtn.onclick = () => {
      this._v2FocusNode = null;
      focusBtn.textContent = '🔍 Focus: Off';
      this.graph3d.nodeColor(this.graph3d.nodeColor());
      this.graph3d.nodeVal(this.graph3d.nodeVal());
      this.graph3d.linkColor(this.graph3d.linkColor());
      this.graph3d.linkOpacity(this.graph3d.linkOpacity());
      this.graph3d.cameraPosition({ x: 0, y: 200, z: 600 }, { x: 0, y: -layerSpacing, z: 0 }, 1500);
    };
    container.appendChild(focusBtn);

    // Resize
    const ro = new ResizeObserver(() => {
      if (this.graph3d) this.graph3d.width(container.offsetWidth).height(container.offsetHeight);
    });
    ro.observe(container);
  }

  // ═══════════════════════════════════════════════════════════════════
  // 3D BUSINESS DOMAIN VIEW — clustered by workload
  // ═══════════════════════════════════════════════════════════════════

  _render3DBusiness(container) {
    const domainMap = new Map(); // domain → [{ id, name, meta, nodeType }]
    const programs = this.galaxyData?.programs || [];
    const seen = new Set();

    // Deduplicate programs
    const uniqueProgs = [];
    for (const p of programs) {
      const key = p.program;
      if (seen.has(key)) continue;
      seen.add(key);
      uniqueProgs.push(p);
    }

    for (const p of uniqueProgs) {
      const domain = this._classifyBusinessDomain(p.program, p);
      if (!domainMap.has(domain)) domainMap.set(domain, []);
      const displayName = p.program.replace('flow-ast-', '').replace('.cbl', '');
      domainMap.get(domain).push({
        id: `prog__${p.program}`,
        name: displayName,
        meta: p,
        nodeType: p.isCopybook ? 'COPYBOOK' : 'PROGRAM',
      });
    }

    // ── Position clusters in a ring, programs orbit within each cluster ──
    const nodes3d = [];
    const edges3d = [];
    const nodeIdSet = new Set();
    const domainKeys = [...domainMap.keys()].filter(k => domainMap.get(k).length > 0);
    const clusterRadius = 300; // distance from center to each cluster center
    const domainPositions = new Map();

    for (let di = 0; di < domainKeys.length; di++) {
      const domain = domainKeys[di];
      const cfg = ASTGalaxyView.BUSINESS_DOMAINS[domain] || { color: '#64748b', icon: '•' };
      const members = domainMap.get(domain);
      const clusterAngle = (di / domainKeys.length) * Math.PI * 2 - Math.PI / 2;
      const isExpanded3D = this.viewMode === 'business-expanded' || this.viewMode === 'service-catalog' || this._isServiceCatalogExpanded;
      const isServiceCatalog = this.viewMode === 'service-catalog' || this._isServiceCatalogExpanded;
      const isServiceExpanded = this._isServiceCatalogExpanded;
      const clusterR = isServiceExpanded ? clusterRadius * 2.5 : (isExpanded3D ? clusterRadius * 1.3 : clusterRadius);
      const cx = Math.cos(clusterAngle) * clusterR;
      const cz = Math.sin(clusterAngle) * clusterR;
      const cy = 0;
      domainPositions.set(domain, { x: cx, y: cy, z: cz });

      // Aggregate domain metrics
      const totalSQL = members.reduce((s, p) => s + (p.meta?.sqlCount || 0), 0);
      const totalCALL = members.reduce((s, p) => s + (p.meta?.callCount || 0), 0);
      const totalLOC = members.reduce((s, p) => s + (p.meta?.lineCount || 0), 0);
      const totalNodes = members.reduce((s, p) => s + (p.meta?.nodeCount || 0), 0);

      // Cluster hub node — larger in collapsed mode (it's the only visual)
      const hubId = `hub__${domain}`;
      const hubSize = isExpanded3D ? 22 : 30 + Math.sqrt(members.length) * 4;
      nodes3d.push({
        id: hubId,
        name: `${cfg.icon} ${domain}`,
        color: cfg.color,
        borderColor: cfg.color,
        val: hubSize,
        _data: { nodeType: 'DOMAIN', displayName: domain, programCount: members.length, domain,
                 sqlCount: totalSQL, callCount: totalCALL, lineCount: totalLOC, nodeCount: totalNodes },
        _nodeType: 'DOMAIN',
        _domain: domain,
        _isHub: true,
        _isExpanded: isExpanded3D,
        _hubMetrics: { programs: members.length, sql: totalSQL, calls: totalCALL, loc: totalLOC, nodes: totalNodes },
        fx: cx, fy: cy + (isExpanded3D ? 50 : 0), fz: cz,
      });
      nodeIdSet.add(hubId);

      if (isExpanded3D) {
        // ── EXPANDED: show every member program with rich detail ──
        const memberRadius = isServiceExpanded ? 120 + members.length * 25 : 55 + members.length * 12;
        for (let mi = 0; mi < members.length; mi++) {
          const m = members[mi];
          const golden = (1 + Math.sqrt(5)) / 2;
          const theta = 2 * Math.PI * mi / golden;
          const phi = Math.acos(1 - 2 * (mi + 0.5) / members.length);
          const mx = cx + memberRadius * Math.sin(phi) * Math.cos(theta);
          const my = cy + memberRadius * Math.cos(phi);
          const mz = cz + memberRadius * Math.sin(phi) * Math.sin(theta);

          const sqlCount = m.meta?.sqlCount || 0;
          const callCount = m.meta?.callCount || 0;
          const nodeCount = m.meta?.nodeCount || 0;
          const sectionCount = m.meta?.sectionCount || 0;
          const paraCount = m.meta?.paraCount || 0;
          const performCount = m.meta?.performCount || 0;
          const branchCount = m.meta?.branchCount || 0;
          const lineCount = m.meta?.lineCount || 0;
          const complexity = this._computeComplexity(m.meta || {});
          const size = Math.max(8, Math.min(25, 8 + Math.sqrt(nodeCount) * 0.5));
          const svc = isServiceCatalog ? this._describeProgram(m.meta?.program, m.meta) : null;

          nodes3d.push({
            id: m.id,
            name: isServiceCatalog ? `${svc.desc}\n${m.name}` : m.name,
            color: cfg.color,
            borderColor: cfg.color,
            val: isServiceCatalog ? size * 1.2 : size,
            _data: { ...m.meta, program: m.meta?.program, nodeType: m.nodeType, displayName: m.name, domain,
                     ...(svc ? { serviceDesc: svc.desc, serviceDetail: svc.detail } : {}) },
            _nodeType: m.nodeType,
            _domain: domain,
            _isHub: false,
            _isExpanded: !isServiceCatalog,
            _isServiceCatalog: isServiceCatalog,
            _serviceDesc: svc?.desc,
            _serviceDetail: svc?.detail,
            _metrics: { sqlCount, callCount, nodeCount, sectionCount, paraCount, performCount, branchCount, lineCount, complexity },
            fx: mx, fy: my, fz: mz,
          });
          nodeIdSet.add(m.id);

          edges3d.push({
            source: hubId, target: m.id,
            color: cfg.color, width: 0.8,
            _type: 'CLUSTER', _domain: domain, _intraCluster: true,
          });

          // ── SERVICE CATALOG EXPANDED: layered orbital architecture ──
          // Layout: program at center → sections on top ring → paragraphs mid ring → actions bottom ring
          // Creates a readable "space station" blueprint per program
          if (this._isServiceCatalogExpanded && this.astData?.nodes) {
            const progAstNodes = this.astData.nodes.filter(n => n.program === m.meta?.program);
            console.log(`[SCE-3D] Program ${m.meta?.program}: ${progAstNodes.length} AST nodes found (total AST: ${this.astData.nodes.length})`);
            const astNodeMap = new Map();

            // ── Classify nodes into layers ──
            const layers = {
              sections:   [], // SECTION, PARAGRAPHS, PROCEDURE_DIVISION_BODY — top tier
              paragraphs: [], // PARAGRAPH, PARAGRAPH_NAME — middle tier
              actions:    [], // CALL, PERFORM, IF, EVALUATE, SQL — bottom tier
            };
            let actionCap = 0;
            for (const an of progAstNodes) {
              if (nodeIdSet.has(an.id)) continue;
              const t = an.nodeType;
              if (t === 'SECTION' || t === 'PARAGRAPHS' || t === 'PROCEDURE_DIVISION_BODY') {
                layers.sections.push(an);
              } else if (t === 'PARAGRAPH' || t === 'PARAGRAPH_NAME') {
                layers.paragraphs.push(an);
              } else if (t === 'PERFORM' || t === 'CALL' || t === 'CallStatement' ||
                         t === 'IF_BRANCH' || t === 'EVALUATE' ||
                         t === 'DIALECT' || t === 'DIALECT_CONTAINER') {
                if (actionCap < 18) { layers.actions.push(an); actionCap++; }
              }
            }

            // Ring radii and vertical offsets (relative to program position)
            const ringCfg = [
              { key: 'sections',   r: 22, yOff: 18,  label: 'Sections',   ringColor: '#8b5cf6', valS: 8 },
              { key: 'paragraphs', r: 38, yOff: 0,   label: 'Paragraphs', ringColor: '#10b981', valS: 5 },
              { key: 'actions',    r: 52, yOff: -18,  label: 'Actions',    ringColor: '#ef4444', valS: 4 },
            ];

            for (const ring of ringCfg) {
              const items = layers[ring.key];
              if (!items.length) continue;

              for (let ai = 0; ai < items.length; ai++) {
                const an = items[ai];
                astNodeMap.set(an.id, an);
                const nStyle = ASTGalaxyView.NODE_STYLE[an.nodeType] || ASTGalaxyView.DEFAULT_STYLE;
                const cleanName = (an.name || '').replace(/.*\//, '').replace(/Context\/.*/, '');
                const typeLabel = ASTGalaxyView.HUMAN_TYPES[an.nodeType] || an.nodeType;
                const nodeLabel = cleanName || typeLabel;
                const astColor = nStyle.border || nStyle.bg || ring.ringColor;

                // Even angular distribution on this ring
                const angle = (ai / items.length) * Math.PI * 2 - Math.PI / 2;
                const sx = mx + ring.r * Math.cos(angle);
                const sy = my + ring.yOff;
                const sz = mz + ring.r * Math.sin(angle);

                nodes3d.push({
                  id: an.id,
                  name: `${nStyle.icon} ${nodeLabel}`,
                  color: astColor,
                  borderColor: astColor,
                  val: ring.valS,
                  _data: { ...an, displayName: cleanName || typeLabel, domain, serviceDesc: svc?.desc },
                  _nodeType: an.nodeType,
                  _domain: domain,
                  _isHub: false,
                  _isExpanded: false,
                  _isAstSubNode: true,
                  _astLayer: ring.key,
                  _astLayerColor: ring.ringColor,
                  _astIcon: nStyle.icon,
                  _astShape: nStyle.shape,
                  fx: sx, fy: sy, fz: sz,
                });
                nodeIdSet.add(an.id);

                // Edge: program → section, section → paragraph (hierarchical),
                //        program → action (flat)
                const parentId = ring.key === 'sections' ? m.id
                  : ring.key === 'paragraphs' && layers.sections.length > 0
                    ? layers.sections[Math.min(ai % layers.sections.length, layers.sections.length - 1)].id
                    : m.id;
                if (nodeIdSet.has(parentId)) {
                  edges3d.push({
                    source: parentId, target: an.id,
                    color: astColor, width: ring.key === 'sections' ? 0.6 : 0.35,
                    _type: 'CONTAINS', _domain: domain, _intraCluster: true,
                  });
                }
              }
            }

            // Intra-program AST edges (FOLLOWED_BY, JUMPS_TO, etc.)
            if (this.astData?.edges) {
              for (const ae of this.astData.edges) {
                if (!astNodeMap.has(ae.source) || !astNodeMap.has(ae.target)) continue;
                const sn = astNodeMap.get(ae.source);
                const tn = astNodeMap.get(ae.target);
                if (sn.program !== m.meta?.program || tn.program !== m.meta?.program) continue;
                const aes = ASTGalaxyView.EDGE_STYLE[ae.type] || ASTGalaxyView.DEFAULT_EDGE;
                edges3d.push({
                  source: ae.source, target: ae.target,
                  color: aes.color || '#475569', width: 0.3,
                  _type: ae.type, _domain: domain, _intraCluster: true,
                });
              }
            }
          }
        }
      }
      // In collapsed mode: NO member nodes — only the hub
    }

    // ── Inter-program edges (CALL/COPY) ──
    const edgeSet = new Set();
    const addEdge = (src, tgt, type) => {
      const key = `${src}→${tgt}`;
      if (edgeSet.has(key) || src === tgt || !nodeIdSet.has(src) || !nodeIdSet.has(tgt)) return;
      edgeSet.add(key);
      const eStyle = ASTGalaxyView.EDGE_STYLE[type] || ASTGalaxyView.DEFAULT_EDGE;
      // Cross-cluster edges are brighter and thicker
      const srcNode = nodes3d.find(n => n.id === src);
      const tgtNode = nodes3d.find(n => n.id === tgt);
      const isCross = srcNode?._domain !== tgtNode?._domain;
      edges3d.push({
        source: src, target: tgt,
        color: isCross ? '#f59e0b' : eStyle.color,
        width: isCross ? 2 : (eStyle.width || 1),
        _type: type, _isCross: isCross,
      });
    };

    if (this.astData?.edges) {
      for (const e of this.astData.edges) {
        if (e.source.startsWith('prog__') && e.target.startsWith('prog__')) addEdge(e.source, e.target, e.type);
      }
    }
    if (this.galaxyData?.edges) {
      for (const e of this.galaxyData.edges) {
        const srcId = `prog__${e.source}`;
        const tgtId = `prog__${e.target}`;
        // Try direct match or fuzzy
        const srcOk = nodeIdSet.has(srcId) ? srcId : [...nodeIdSet].find(id => id.includes(e.source?.replace('.cbl','')));
        const tgtOk = nodeIdSet.has(tgtId) ? tgtId : [...nodeIdSet].find(id => id.includes(e.target?.replace('.cbl','')));
        if (srcOk && tgtOk) addEdge(srcOk, tgtOk, e.type);
      }
    }

    // ── Build ForceGraph3D (business) ──
    console.log(`[3DBusiness] viewMode=${this.viewMode} | nodes: ${nodes3d.length} (hubs: ${nodes3d.filter(n=>n._isHub).length}, progs: ${nodes3d.filter(n=>!n._isHub && !n._isAstSubNode).length}, ast: ${nodes3d.filter(n=>n._isAstSubNode).length}) | edges: ${edges3d.length} | astData: ${this.astData?.nodes?.length || 0} nodes`);
    this.graph3d = ForceGraph3D()(container)
      .graphData({ nodes: nodes3d, links: edges3d })
      .backgroundColor('#050810')
      .showNavInfo(false)
      .nodeColor(n => {
        if (this._3dSearchTerm && this._3dSearchTerm.length >= 2) {
          return (n.name || '').toLowerCase().includes(this._3dSearchTerm) ? '#fbbf24' : 'rgba(30,41,59,0.4)';
        }
        return n.color;
      })
      .nodeVal(n => {
        if (this._3dSearchTerm && this._3dSearchTerm.length >= 2) {
          const match = (n.name || '').toLowerCase().includes(this._3dSearchTerm);
          return match ? (n.val || 6) * 2.5 : (n.val || 6) * 0.3;
        }
        return n.val;
      })
      .nodeLabel(n => {
        const d = n._data || {};
        const m = n._metrics || {};
        if (n._isHub) {
          const members = domainMap.get(n._domain) || [];
          const hm = n._hubMetrics || {};
          if (n._isExpanded) {
            return `<div style="background:rgba(5,8,16,0.92);padding:10px 14px;border-radius:8px;border:2px solid ${n.color};color:#e2e8f0;font-size:13px;min-width:200px;">
              <div style="font-size:16px;font-weight:700;color:${n.color};margin-bottom:4px;">${n.name}</div>
              <div style="color:#94a3b8;">${members.length} programs expanded below</div>
              <div style="margin-top:4px;font-size:11px;color:#64748b;">Click programs for details</div>
            </div>`;
          }
          return `<div style="background:rgba(5,8,16,0.95);padding:12px 16px;border-radius:8px;border:2px solid ${n.color};color:#e2e8f0;font-size:13px;min-width:240px;">
            <div style="font-size:18px;font-weight:700;color:${n.color};margin-bottom:8px;">${n.name}</div>
            <div style="display:grid;grid-template-columns:1fr 1fr;gap:4px 16px;font-size:12px;">
              <div>🖥️ <span style="color:#e2e8f0;font-weight:600;">${hm.programs}</span> programs</div>
              <div>📝 <span style="color:#e2e8f0;">${(hm.loc || 0).toLocaleString()}</span> LOC</div>
              <div>🗄️ <span style="color:#a855f7;">${hm.sql || 0}</span> SQL</div>
              <div>📞 <span style="color:#ef4444;">${hm.calls || 0}</span> CALLs</div>
              <div>🧩 <span style="color:#94a3b8;">${(hm.nodes || 0).toLocaleString()}</span> AST nodes</div>
            </div>
            <div style="margin-top:8px;font-size:11px;color:#64748b;border-top:1px solid #1e293b;padding-top:6px;">Switch to <b>Expanded</b> to see individual programs</div>
          </div>`;
        }
        if (n._isExpanded && m.sqlCount !== undefined) {
          const cpx = Math.round((m.complexity || 0) * 100);
          const cpxColor = cpx > 60 ? '#ef4444' : cpx > 30 ? '#f59e0b' : '#10b981';
          return `<div style="background:rgba(5,8,16,0.95);padding:10px 14px;border-radius:8px;border:1px solid ${n.color};color:#e2e8f0;font-size:12px;min-width:220px;">
            <div style="font-weight:700;color:${n.color};font-size:14px;">${n.name}</div>
            <div style="color:#94a3b8;font-size:11px;margin-bottom:8px;">${n._domain}</div>
            <div style="display:grid;grid-template-columns:1fr 1fr;gap:4px 12px;font-size:11px;">
              <div>📂 <span style="color:#8b5cf6;">${m.sectionCount}</span> sections</div>
              <div>¶ <span style="color:#10b981;">${m.paraCount}</span> paragraphs</div>
              <div>🗄️ <span style="color:#a855f7;">${m.sqlCount}</span> SQL</div>
              <div>📞 <span style="color:#ef4444;">${m.callCount}</span> CALLs</div>
              <div>🔄 <span style="color:#06b6d4;">${m.performCount}</span> PERFORMs</div>
              <div>❓ <span style="color:#ec4899;">${m.branchCount}</span> branches</div>
              ${m.lineCount ? `<div>📝 <span style="color:#94a3b8;">${m.lineCount.toLocaleString()}</span> LOC</div>` : ''}
              <div>🧠 <span style="color:${cpxColor};">${cpx}%</span> complexity</div>
            </div>
            <div style="margin-top:6px;height:4px;background:#1e293b;border-radius:2px;overflow:hidden;">
              <div style="height:100%;width:${cpx}%;background:${cpxColor};border-radius:2px;"></div>
            </div>
          </div>`;
        }
        if (n._isServiceCatalog) {
          return `<div style="background:rgba(5,8,16,0.95);padding:12px 16px;border-radius:8px;border:2px solid ${n.color};color:#e2e8f0;font-size:13px;min-width:250px;">
            <div style="font-size:15px;font-weight:700;color:#fbbf24;margin-bottom:4px;">🔧 ${n._serviceDesc || n.name}</div>
            <div style="color:#94a3b8;font-size:12px;font-style:italic;margin-bottom:6px;">${n._serviceDetail || ''}</div>
            <div style="font-weight:600;color:${n.color};font-size:13px;">${n._data?.displayName || ''}</div>
            <div style="color:#64748b;font-size:11px;margin-bottom:6px;">${n._domain || ''}</div>
            <div style="display:grid;grid-template-columns:1fr 1fr;gap:3px 12px;font-size:11px;">
              <div>📂 <span style="color:#8b5cf6;">${m.sectionCount || 0}</span> sections</div>
              <div>🗄️ <span style="color:#a855f7;">${m.sqlCount || 0}</span> SQL</div>
              <div>📞 <span style="color:#ef4444;">${m.callCount || 0}</span> CALLs</div>
              <div>📝 <span style="color:#94a3b8;">${(m.lineCount || 0).toLocaleString()}</span> LOC</div>
            </div>
          </div>`;
        }
        if (n._isAstSubNode) {
          const typeLabel = ASTGalaxyView.HUMAN_TYPES[n._nodeType] || n._nodeType;
          return `<div style="background:rgba(5,8,16,0.92);padding:8px 12px;border-radius:6px;border:1px solid ${n.color};color:#e2e8f0;font-size:12px;">
            <div style="font-weight:600;color:${n.color};">${n._astIcon || ''} ${typeLabel}</div>
            <div style="color:#e2e8f0;font-size:11px;">${n._data?.displayName || n.name || ''}</div>
            <div style="color:#94a3b8;font-size:11px;">${n._domain || ''} · Lines ${n._data?.startLine || '?'}–${n._data?.endLine || '?'}</div>
            ${n._data?.serviceDesc ? `<div style="color:#fbbf24;font-size:11px;">🔧 ${n._data.serviceDesc}</div>` : ''}
          </div>`;
        }
        return `<div style="background:rgba(5,8,16,0.92);padding:8px 12px;border-radius:6px;border:1px solid ${n.color};color:#e2e8f0;font-size:12px;">
          <div style="font-weight:600;color:${n.color};">${n.name}</div>
          <div style="color:#94a3b8;font-size:11px;">Domain: ${n._domain || '—'}</div>
          ${d.sqlCount ? `<div style="color:#a855f7;font-size:11px;">SQL: ${d.sqlCount}</div>` : ''}
          ${d.callCount ? `<div style="color:#ef4444;font-size:11px;">CALLs: ${d.callCount}</div>` : ''}
        </div>`;
      })
      .nodeOpacity(0.95)
      .nodeThreeObject(n => {
        try {
          const THREE = window.THREE;
          if (!THREE) return undefined;
          const group = new THREE.Group();

          if (n._isHub) {
            const hm = n._hubMetrics || {};
            // Hub size scales with mode
            const sphereR = n._isExpanded ? 10 : 16 + Math.sqrt(hm.programs || 1) * 2;

            // ── Domain hub: glowing sphere ──
            const sphereGeom = new THREE.SphereGeometry(sphereR, 32, 24);
            const sphereMat = new THREE.MeshPhongMaterial({
              color: new THREE.Color(n.color),
              emissive: new THREE.Color(n.color),
              emissiveIntensity: n._isExpanded ? 0.5 : 0.7,
              transparent: true, opacity: n._isExpanded ? 0.3 : 0.45,
            });
            group.add(new THREE.Mesh(sphereGeom, sphereMat));

            // Inner glowing core
            const coreGeom = new THREE.SphereGeometry(sphereR * 0.3, 16, 12);
            const coreMat = new THREE.MeshPhongMaterial({
              color: new THREE.Color(n.color),
              emissive: new THREE.Color(n.color),
              emissiveIntensity: 1.0, transparent: true, opacity: 0.9,
            });
            group.add(new THREE.Mesh(coreGeom, coreMat));

            // Orbital ring
            const ringGeom = new THREE.TorusGeometry(sphereR * 1.2, 0.3, 8, 48);
            const ringMat = new THREE.MeshBasicMaterial({ color: new THREE.Color(n.color), transparent: true, opacity: 0.25 });
            const ring = new THREE.Mesh(ringGeom, ringMat);
            ring.rotation.x = Math.PI / 2;
            group.add(ring);

            if (!n._isExpanded) {
              // ── COLLAPSED: show program count dots orbiting & metric pillars ──
              const progCount = Math.min(hm.programs || 0, 20);
              for (let pi = 0; pi < progCount; pi++) {
                const pa = (pi / progCount) * Math.PI * 2;
                const pr = sphereR * 1.6;
                const dotGeom = new THREE.SphereGeometry(0.6, 8, 6);
                const dotMat = new THREE.MeshBasicMaterial({ color: new THREE.Color(n.color), transparent: true, opacity: 0.6 });
                const dot = new THREE.Mesh(dotGeom, dotMat);
                dot.position.set(Math.cos(pa) * pr, 0, Math.sin(pa) * pr);
                group.add(dot);
              }

              // Metric pillars on collapsed hub (SQL=purple, CALL=red)
              const hubMetrics = [
                { val: hm.sql || 0, max: 200, color: '#a855f7', angle: -Math.PI / 4 },
                { val: hm.calls || 0, max: 30, color: '#ef4444', angle: Math.PI / 4 },
              ];
              for (const mt of hubMetrics) {
                if (!mt.val) continue;
                const height = Math.max(1, Math.min(15, (mt.val / mt.max) * 15));
                const pillarGeom = new THREE.CylinderGeometry(0.5, 0.5, height, 6);
                const pillarMat = new THREE.MeshPhongMaterial({
                  color: new THREE.Color(mt.color),
                  emissive: new THREE.Color(mt.color),
                  emissiveIntensity: 0.7,
                  transparent: true, opacity: 0.7,
                });
                const pillar = new THREE.Mesh(pillarGeom, pillarMat);
                pillar.position.set(Math.cos(mt.angle) * (sphereR + 3), height / 2, Math.sin(mt.angle) * (sphereR + 3));
                group.add(pillar);
              }
            }

            // Domain label — richer in collapsed mode
            const canvas = document.createElement('canvas');
            canvas.width = 512; canvas.height = n._isExpanded ? 64 : 100;
            const ctx = canvas.getContext('2d');
            ctx.fillStyle = 'rgba(5,8,16,0.85)';
            ctx.fillRect(0, 0, canvas.width, canvas.height);
            ctx.font = 'bold 30px system-ui, -apple-system, sans-serif';
            ctx.fillStyle = n.color;
            ctx.textAlign = 'center';
            ctx.fillText(n.name, 256, n._isExpanded ? 32 : 28);
            if (!n._isExpanded) {
              ctx.font = '18px system-ui, sans-serif';
              ctx.fillStyle = '#94a3b8';
              ctx.fillText(`${hm.programs} programs · ${(hm.loc || 0).toLocaleString()} LOC`, 256, 56);
              ctx.font = '16px system-ui, sans-serif';
              ctx.fillStyle = '#64748b';
              const parts = [];
              if (hm.sql) parts.push(`${hm.sql} SQL`);
              if (hm.calls) parts.push(`${hm.calls} CALL`);
              parts.push(`${(hm.nodes || 0).toLocaleString()} nodes`);
              ctx.fillText(parts.join(' · '), 256, 80);
            }
            const texture = new THREE.CanvasTexture(canvas);
            texture.minFilter = THREE.LinearFilter;
            const spriteMat = new THREE.SpriteMaterial({ map: texture, transparent: true });
            const sprite = new THREE.Sprite(spriteMat);
            sprite.scale.set(n._isExpanded ? 36 : 50, n._isExpanded ? 4.5 : 10, 1);
            sprite.position.y = sphereR + (n._isExpanded ? 6 : 10);
            group.add(sprite);
          } else {
            // ── Program node ──
            const r = Math.max(1.5, (n.val || 6) * 0.25);
            const m = n._metrics || {};

            if (n._isServiceCatalog) {
              // ── SERVICE CATALOG: card-style node with description ──
              const cardR = r * 1.3;
              // Rounded box shape
              const boxGeom = new THREE.BoxGeometry(cardR * 3, cardR * 2, cardR * 0.4);
              const boxMat = new THREE.MeshPhongMaterial({
                color: new THREE.Color(n.color),
                emissive: new THREE.Color(n.color),
                emissiveIntensity: 0.35,
                transparent: true, opacity: 0.85,
              });
              group.add(new THREE.Mesh(boxGeom, boxMat));

              // Service card label: service description + program name + metrics
              const canvas = document.createElement('canvas');
              canvas.width = 400; canvas.height = 80;
              const ctx = canvas.getContext('2d');
              ctx.fillStyle = 'rgba(5,8,16,0.9)';
              ctx.fillRect(0, 0, 400, 80);
              // Border
              ctx.strokeStyle = n.color;
              ctx.lineWidth = 2;
              ctx.strokeRect(1, 1, 398, 78);
              // Service description (bold, gold)
              ctx.font = 'bold 20px system-ui, sans-serif';
              ctx.fillStyle = '#fbbf24';
              ctx.textAlign = 'center';
              ctx.fillText(`🔧 ${(n._serviceDesc || '').slice(0, 25)}`, 200, 22);
              // Program name
              ctx.font = '16px system-ui, sans-serif';
              ctx.fillStyle = n.color;
              const progName = (n._data?.displayName || '').slice(0, 25);
              ctx.fillText(progName, 200, 44);
              // Metrics
              ctx.font = '13px system-ui, sans-serif';
              ctx.fillStyle = '#94a3b8';
              const parts = [];
              if (m.sqlCount) parts.push(`${m.sqlCount} SQL`);
              if (m.callCount) parts.push(`${m.callCount} CALL`);
              if (m.sectionCount) parts.push(`${m.sectionCount} sec`);
              if (m.lineCount) parts.push(`${m.lineCount} LOC`);
              ctx.fillText(parts.join(' · '), 200, 66);

              const texture = new THREE.CanvasTexture(canvas);
              texture.minFilter = THREE.LinearFilter;
              const spriteMat = new THREE.SpriteMaterial({ map: texture, transparent: true });
              const sprite = new THREE.Sprite(spriteMat);
              sprite.scale.set(24, 4.8, 1);
              sprite.position.y = cardR * 1.5 + 2;
              group.add(sprite);

            } else if (n._isExpanded && m.sqlCount !== undefined) {
              // ── EXPANDED MODE: rich visualization with metric indicators ──
              const complexity = m.complexity || 0;
              const cpxColor = complexity > 0.6 ? '#ef4444' : complexity > 0.3 ? '#f59e0b' : '#10b981';

              // Main sphere — size reflects total AST nodes
              const sphereGeom = new THREE.SphereGeometry(r, 16, 12);
              const sphereMat = new THREE.MeshPhongMaterial({
                color: new THREE.Color(n.color),
                emissive: new THREE.Color(n.color),
                emissiveIntensity: 0.5,
                transparent: true, opacity: 0.9,
              });
              group.add(new THREE.Mesh(sphereGeom, sphereMat));

              // Complexity ring — red/amber/green based on score
              const ringR = r * 1.4;
              const ringGeom = new THREE.TorusGeometry(ringR, 0.25, 8, 32);
              const ringMat = new THREE.MeshBasicMaterial({
                color: new THREE.Color(cpxColor),
                transparent: true, opacity: 0.6,
              });
              const ring = new THREE.Mesh(ringGeom, ringMat);
              ring.rotation.x = Math.PI / 2;
              group.add(ring);

              // Metric pillars around the node (SQL=purple, CALL=red, PERFORM=cyan)
              const metrics = [
                { val: m.sqlCount, max: 100, color: '#a855f7', angle: 0 },
                { val: m.callCount, max: 10, color: '#ef4444', angle: Math.PI * 2 / 3 },
                { val: m.performCount, max: 20, color: '#06b6d4', angle: Math.PI * 4 / 3 },
              ];
              const pillarDist = r * 1.8;
              for (const mt of metrics) {
                if (!mt.val) continue;
                const height = Math.max(0.5, Math.min(8, (mt.val / mt.max) * 8));
                const pillarGeom = new THREE.CylinderGeometry(0.3, 0.3, height, 6);
                const pillarMat = new THREE.MeshPhongMaterial({
                  color: new THREE.Color(mt.color),
                  emissive: new THREE.Color(mt.color),
                  emissiveIntensity: 0.6,
                  transparent: true, opacity: 0.8,
                });
                const pillar = new THREE.Mesh(pillarGeom, pillarMat);
                pillar.position.set(
                  Math.cos(mt.angle) * pillarDist,
                  height / 2 - r * 0.5,
                  Math.sin(mt.angle) * pillarDist
                );
                group.add(pillar);
              }

              // Section count indicator — small dots orbiting
              const secCount = Math.min(m.sectionCount, 12);
              for (let si = 0; si < secCount; si++) {
                const sa = (si / secCount) * Math.PI * 2;
                const sd = r * 2.2;
                const dotGeom = new THREE.SphereGeometry(0.2, 6, 4);
                const dotMat = new THREE.MeshBasicMaterial({ color: 0x8b5cf6, transparent: true, opacity: 0.5 });
                const dot = new THREE.Mesh(dotGeom, dotMat);
                dot.position.set(Math.cos(sa) * sd, r * 0.8, Math.sin(sa) * sd);
                group.add(dot);
              }

              // Rich label with metrics summary
              const canvas = document.createElement('canvas');
              canvas.width = 320; canvas.height = 52;
              const ctx = canvas.getContext('2d');
              ctx.fillStyle = 'rgba(5,8,16,0.85)';
              ctx.fillRect(0, 0, 320, 52);
              // Name
              ctx.font = 'bold 18px system-ui, sans-serif';
              ctx.fillStyle = '#e2e8f0';
              ctx.textAlign = 'center';
              ctx.fillText(n.name.length > 22 ? n.name.slice(0, 19) + '...' : n.name, 160, 18);
              // Metrics bar
              ctx.font = '12px system-ui, sans-serif';
              const metricsStr = [];
              if (m.sqlCount) metricsStr.push(`SQL:${m.sqlCount}`);
              if (m.callCount) metricsStr.push(`CALL:${m.callCount}`);
              if (m.sectionCount) metricsStr.push(`SEC:${m.sectionCount}`);
              metricsStr.push(`${Math.round(complexity * 100)}%`);
              ctx.fillStyle = '#94a3b8';
              ctx.fillText(metricsStr.join(' · '), 160, 40);
              // Complexity bar at bottom
              ctx.fillStyle = '#1e293b';
              ctx.fillRect(10, 48, 300, 3);
              ctx.fillStyle = cpxColor;
              ctx.fillRect(10, 48, 300 * complexity, 3);

              const texture = new THREE.CanvasTexture(canvas);
              texture.minFilter = THREE.LinearFilter;
              const spriteMat = new THREE.SpriteMaterial({ map: texture, transparent: true });
              const sprite = new THREE.Sprite(spriteMat);
              sprite.scale.set(20, 3.2, 1);
              sprite.position.y = r + 3;
              group.add(sprite);
            } else if (n._isAstSubNode) {
              // ── AST SUB-NODE: layered orbital architecture ──
              const layer = n._astLayer || 'actions';
              const layerColor = n._astLayerColor || n.color;
              const nodeType = n._nodeType || '';

              // Size and geometry depend on layer (bigger = more important)
              const sizeScale = layer === 'sections' ? 2.2 : layer === 'paragraphs' ? 1.6 : 1.0;
              const astR = Math.max(0.6, sizeScale);
              let geom;
              if (nodeType === 'SECTION' || nodeType === 'PARAGRAPHS' || nodeType === 'PROCEDURE_DIVISION_BODY') {
                geom = new THREE.OctahedronGeometry(astR, 1); // smoother octahedron
              } else if (nodeType === 'PARAGRAPH' || nodeType === 'PARAGRAPH_NAME') {
                geom = new THREE.DodecahedronGeometry(astR, 0);
              } else if (nodeType === 'CALL' || nodeType === 'CallStatement') {
                geom = new THREE.TetrahedronGeometry(astR * 1.3, 0);
              } else if (nodeType === 'PERFORM') {
                geom = new THREE.ConeGeometry(astR * 0.7, astR * 1.8, 8);
              } else if (nodeType === 'DIALECT' || nodeType === 'DIALECT_CONTAINER') {
                geom = new THREE.BoxGeometry(astR * 1.2, astR * 1.2, astR * 1.2);
              } else if (nodeType === 'IF_BRANCH' || nodeType === 'EVALUATE') {
                geom = new THREE.TorusGeometry(astR * 0.7, astR * 0.25, 8, 12);
              } else {
                geom = new THREE.SphereGeometry(astR, 12, 8);
              }

              // Glowing material with emissive bloom
              const mat = new THREE.MeshPhongMaterial({
                color: new THREE.Color(n.color),
                emissive: new THREE.Color(n.color),
                emissiveIntensity: layer === 'sections' ? 0.8 : 0.5,
                transparent: true,
                opacity: layer === 'sections' ? 0.95 : 0.85,
              });
              group.add(new THREE.Mesh(geom, mat));

              // Glow halo for section nodes (top-tier emphasis)
              if (layer === 'sections') {
                const haloGeom = new THREE.SphereGeometry(astR * 1.8, 12, 8);
                const haloMat = new THREE.MeshBasicMaterial({
                  color: new THREE.Color(layerColor),
                  transparent: true, opacity: 0.08,
                });
                group.add(new THREE.Mesh(haloGeom, haloMat));
              }

              // Label: readable name + layer badge
              const canvas = document.createElement('canvas');
              const displayLabel = (n.name || '').length > 24 ? n.name.slice(0, 21) + '...' : (n.name || '');
              const layerBadge = layer === 'sections' ? '▸ SECTION' : layer === 'paragraphs' ? '▸ PARA' : '▸ ACTION';
              canvas.width = 320; canvas.height = 44;
              const ctx = canvas.getContext('2d');
              ctx.fillStyle = 'rgba(5,8,16,0.75)';
              ctx.fillRect(0, 0, 320, 44);
              // Left accent bar
              ctx.fillStyle = layerColor;
              ctx.fillRect(0, 0, 4, 44);
              // Name
              ctx.font = 'bold 18px system-ui, -apple-system, sans-serif';
              ctx.fillStyle = '#f1f5f9';
              ctx.textAlign = 'left';
              ctx.fillText(displayLabel, 12, 18);
              // Layer badge
              ctx.font = '12px system-ui, sans-serif';
              ctx.fillStyle = layerColor;
              ctx.fillText(layerBadge, 12, 36);
              const texture = new THREE.CanvasTexture(canvas);
              texture.minFilter = THREE.LinearFilter;
              const spriteMat = new THREE.SpriteMaterial({ map: texture, transparent: true });
              const sprite = new THREE.Sprite(spriteMat);
              sprite.scale.set(layer === 'sections' ? 18 : 14, layer === 'sections' ? 2.6 : 2.0, 1);
              sprite.position.y = astR + 1.5;
              group.add(sprite);
            } else {
              // ── COLLAPSED MODE: simple sphere + name ──
              const sphereGeom = new THREE.SphereGeometry(r, 12, 8);
              const sphereMat = new THREE.MeshPhongMaterial({
                color: new THREE.Color(n.color),
                emissive: new THREE.Color(n.color),
                emissiveIntensity: 0.5,
                transparent: true, opacity: 0.9,
              });
              group.add(new THREE.Mesh(sphereGeom, sphereMat));

              // Program label
              const canvas = document.createElement('canvas');
              const displayLabel = n.name.length > 20 ? n.name.slice(0, 17) + '...' : n.name;
              const fontSize = 24;
              canvas.width = 256; canvas.height = 36;
              const ctx = canvas.getContext('2d');
              ctx.font = `bold ${fontSize}px system-ui, sans-serif`;
              ctx.fillStyle = 'rgba(5,8,16,0.7)';
              ctx.fillRect(0, 0, 256, 36);
              ctx.fillStyle = '#cbd5e1';
              ctx.textAlign = 'center';
              ctx.textBaseline = 'middle';
              ctx.fillText(displayLabel, 128, 18);
              const texture = new THREE.CanvasTexture(canvas);
              texture.minFilter = THREE.LinearFilter;
              const spriteMat = new THREE.SpriteMaterial({ map: texture, transparent: true });
              const sprite = new THREE.Sprite(spriteMat);
              sprite.scale.set(16, 2.2, 1);
              sprite.position.y = r + 2;
              group.add(sprite);
            }
          }
          return group;
        } catch { return undefined; }
      })
      .nodeThreeObjectExtend(false)
      // Links
      .linkColor(l => l._intraCluster ? l.color : (l._isCross ? '#f59e0b' : (l.color || '#475569')))
      .linkOpacity(l => l._intraCluster ? (l._type === 'CONTAINS' ? 0.25 : 0.15) : 0.5)
      .linkWidth(l => l._intraCluster ? (l._type === 'CONTAINS' ? 0.5 : 0.2) : (l._isCross ? 1.5 : 0.8))
      .linkDirectionalArrowLength(l => l._intraCluster ? (l._type === 'CONTAINS' ? 2 : 0) : 4)
      .linkDirectionalArrowRelPos(1)
      .linkDirectionalParticles(l => {
        if (l._intraCluster) return 0;
        if (l._isCross) return 4;
        return l._type === 'CALL' ? 3 : l._type === 'COPY' ? 2 : 0;
      })
      .linkDirectionalParticleWidth(l => l._isCross ? 2.5 : 1.8)
      .linkDirectionalParticleColor(l => l._isCross ? '#fbbf24' : (l.color || '#60a5fa'))
      .linkDirectionalParticleSpeed(0.006)
      // Interaction
      .onNodeClick(node => {
        if (node._data) {
          this._updateInspector(node._data);
          this._updateSourcePanel(node._data);
        }
        // Zoom into cluster or node
        const dist = node._isHub ? 180 : 80;
        const pos = { x: node.fx || node.x || 0, y: node.fy || node.y || 0, z: node.fz || node.z || 0 };
        const hypot = Math.hypot(pos.x, pos.y, pos.z) || 1;
        const ratio = 1 + dist / hypot;
        this.graph3d.cameraPosition(
          { x: pos.x * ratio, y: pos.y * ratio + 30, z: pos.z * ratio },
          pos, 1000
        );
      })
      .onNodeDoubleClick(node => {
        if (node._data?.program && typeof astExplorer !== 'undefined' && astExplorer) {
          astExplorer.drillIntoProgram(node._data.program);
        }
      })
      .width(container.offsetWidth)
      .height(container.offsetHeight);

    // Initial camera: elevated angle to see all clusters
    const camDist = this._isServiceCatalogExpanded ? 1200 : 550;
    const camY = this._isServiceCatalogExpanded ? 600 : 350;
    setTimeout(() => {
      if (this.graph3d) {
        this.graph3d.cameraPosition({ x: 0, y: camY, z: camDist }, { x: 0, y: 0, z: 0 }, 2000);
      }
    }, 300);

    // ── Add cluster boundary spheres (transparent bubble per domain) ──
    try {
      const THREE = window.THREE;
      if (THREE && this.graph3d.scene) {
        setTimeout(() => {
          for (const [domain, pos] of domainPositions.entries()) {
            const cfg = ASTGalaxyView.BUSINESS_DOMAINS[domain] || { color: '#64748b' };
            const members = domainMap.get(domain) || [];
            const bubbleR = 35 + members.length * 8;
            const bubbleGeom = new THREE.SphereGeometry(bubbleR, 24, 16);
            const bubbleMat = new THREE.MeshBasicMaterial({
              color: new THREE.Color(cfg.color),
              transparent: true, opacity: 0.04,
              wireframe: true,
            });
            const bubble = new THREE.Mesh(bubbleGeom, bubbleMat);
            bubble.position.set(pos.x, pos.y, pos.z);
            this.graph3d.scene().add(bubble);
          }

          // ── Service Catalog Expanded: add orbital guide rings per program ──
          if (this._isServiceCatalogExpanded) {
            const orbitalLayers = [
              { r: 22, yOff: 18,  color: '#8b5cf6', label: 'Sections' },
              { r: 38, yOff: 0,   color: '#10b981', label: 'Paragraphs' },
              { r: 52, yOff: -18, color: '#ef4444', label: 'Actions' },
            ];
            for (const n of nodes3d) {
              if (!n._isServiceCatalog && !n._isExpanded) continue; // only program nodes
              if (n._isHub || n._isAstSubNode) continue;
              const px = n.fx || 0, py = n.fy || 0, pz = n.fz || 0;
              for (const ol of orbitalLayers) {
                const ringGeom = new THREE.TorusGeometry(ol.r, 0.12, 8, 64);
                const ringMat = new THREE.MeshBasicMaterial({
                  color: new THREE.Color(ol.color),
                  transparent: true, opacity: 0.18,
                });
                const ring = new THREE.Mesh(ringGeom, ringMat);
                ring.rotation.x = Math.PI / 2;
                ring.position.set(px, py + ol.yOff, pz);
                this.graph3d.scene().add(ring);
              }
            }
          }

          // Add ambient + directional lights for phong materials
          const ambient = new THREE.AmbientLight(0x404060, 1.5);
          this.graph3d.scene().add(ambient);
          const dirLight = new THREE.DirectionalLight(0xffffff, 0.8);
          dirLight.position.set(200, 400, 300);
          this.graph3d.scene().add(dirLight);
        }, 100);
      }
    } catch { /* cluster bubbles are optional */ }

    // Resize handler
    const ro = new ResizeObserver(() => {
      if (this.graph3d) this.graph3d.width(container.offsetWidth).height(container.offsetHeight);
    });
    ro.observe(container);

    // Node count badge (so user can verify expanded nodes are present)
    const astCount = nodes3d.filter(n => n._isAstSubNode).length;
    const progCount = nodes3d.filter(n => !n._isHub && !n._isAstSubNode).length;
    const hubCount = nodes3d.filter(n => n._isHub).length;
    const badge = document.createElement('div');
    badge.style.cssText = 'position:absolute;top:10px;right:10px;z-index:20;padding:6px 12px;background:rgba(3,7,18,0.85);color:#94a3b8;border:1px solid #334155;border-radius:6px;font-size:11px;pointer-events:none;backdrop-filter:blur(4px);';
    badge.innerHTML = `${hubCount} domains · ${progCount} programs` + (astCount > 0 ? ` · <span style="color:#fbbf24;">${astCount} AST nodes</span>` : '');
    container.appendChild(badge);
  }

  // ═══════════════════════════════════════════════════════════════════
  // LEGEND — icon, color, and shape descriptions
  // ═══════════════════════════════════════════════════════════════════

  // ═══════════════════════════════════════════════════════════════════
  // LEGEND — single rich content builder used in two surfaces:
  //   • Side panel (#galaxy-legend) for 2D views
  //   • Floating overlay on the 3D container for 3D views
  // Sections are gated by what the current view actually renders so
  // the legend always matches the icons on screen.
  // ═══════════════════════════════════════════════════════════════════
  _buildLegendHTML() {
    const mode = this.viewMode;
    const is3D = this.is3D;
    const isSCExpanded = this._isServiceCatalogExpanded;
    const isBusiness = this._isBusinessMode;
    const isService = this._isServiceCatalogMode;
    const isRadar = mode === 'service-catalog-v3';
    const isV2 = mode === 'service-catalog-v2';
    // Technical views show raw AST node types & shapes prominently
    const isTechnical = mode === 'clustered' || mode === 'expanded' || mode === 'expanded-v2';
    const isV2Technical = mode === 'expanded-v2';
    // Views that draw individual AST sub-nodes (sections/paragraphs/actions)
    const showsAstNodes = isTechnical || isSCExpanded || mode === 'business-expanded';

    const titleMap = {
      'clustered': '📦 Technical',
      'expanded': '📦 Technical (Expanded)',
      'expanded-v2': '🔽 Technical (Expanded v2 — Top-Down)',
      'business': '🏢 Business Domains',
      'business-expanded': '🏢 Business Domains (Expanded)',
      'service-catalog': '📋 Service Catalog',
      'service-catalog-expanded': '📋 Service Catalog (Expanded)',
      'service-catalog-expanded-3d': '🧊 Service Catalog (Expanded 3D)',
      'service-catalog-v2': '🚀 Service Catalog v2',
      'service-catalog-v3': '🎯 Modernization Radar',
    };
    const dim = is3D ? '🧊 3D' : '2D';

    // ── helpers ─────────────────────────────────────────────────────
    const sectionTitle = (label) =>
      `<div style="color:#94a3b8;font-size:10px;text-transform:uppercase;letter-spacing:0.05em;margin:8px 0 2px;">${label}</div>`;
    const dotRow = (color, text) =>
      `<div style="display:flex;align-items:center;gap:6px;line-height:1.5;">
        <span style="display:inline-block;width:10px;height:10px;border-radius:50%;background:${color};box-shadow:0 0 6px ${color};flex-shrink:0;"></span>
        <span style="color:#cbd5e1;font-size:11px;">${text}</span>
      </div>`;
    const swatchRow = (bg, border, icon, text) =>
      `<div style="display:flex;align-items:center;gap:6px;line-height:1.5;">
        <span style="display:inline-block;width:10px;height:10px;border-radius:2px;background:${bg};border:1px solid ${border};flex-shrink:0;"></span>
        <span style="font-size:11px;">${icon}</span>
        <span style="color:#cbd5e1;font-size:11px;">${text}</span>
      </div>`;
    const lineRow = (color, dashed, label) =>
      `<div style="display:flex;align-items:center;gap:6px;line-height:1.5;">
        <span style="display:inline-block;width:18px;height:0;border-top:2px ${dashed?'dashed':'solid'} ${color};flex-shrink:0;"></span>
        <span style="color:#cbd5e1;font-size:11px;">${label}</span>
      </div>`;

    let html = '';

    // ── Header strip ────────────────────────────────────────────────
    html += `<div style="display:flex;justify-content:space-between;align-items:center;margin-bottom:6px;">
      <strong style="color:#60a5fa;font-size:12px;">Legend</strong>
      <span style="color:#94a3b8;font-size:10px;">${titleMap[mode] || mode} · ${dim}</span>
    </div>`;

    // ── Modernization-radar specific (risk tiers + sizing) ──────────
    if (isRadar && Array.isArray(ASTGalaxyView.RISK_TIERS)) {
      html += sectionTitle('Risk Tiers');
      for (const t of ASTGalaxyView.RISK_TIERS) html += dotRow(t.color, `${t.label} risk`);
      html += sectionTitle('Sizing');
      html += `<div style="font-size:11px;color:#cbd5e1;line-height:1.6;">${
        is3D
          ? '↕️ Building height = LOC<br>↔️ Width = complexity<br>🔴 Roof beacon = high risk<br>🪟 Window density = code density'
          : '⬤ Node radius = LOC<br>🔆 Glow = complexity'
      }</div>`;
    }

    // ── v2 specific (layers + shapes) ───────────────────────────────
    if (isV2 && Array.isArray(ASTGalaxyView.LAYER_CONFIG)) {
      html += sectionTitle('Architectural Layers (Y-axis)');
      for (const lc of ASTGalaxyView.LAYER_CONFIG) html += dotRow(lc.color, `${lc.icon} ${lc.label}`);
      html += sectionTitle('Complexity Tier Colors');
      html += dotRow('#ef4444', '🔴 High risk (>60%)');
      html += dotRow('#f59e0b', '🟡 Medium (30–60%)');
      html += dotRow('#10b981', '🟢 Simple (<30%)');
      html += dotRow('#818cf8', '🔵 Data layer');
    }

    // ── Business Domains (any view that uses domain coloring) ───────
    if (isBusiness || isService) {
      html += sectionTitle('Business Domains');
      for (const [name, cfg] of Object.entries(ASTGalaxyView.BUSINESS_DOMAINS)) {
        html += `<div style="display:flex;align-items:center;gap:6px;line-height:1.5;">
          <span style="display:inline-block;width:10px;height:10px;border-radius:50%;background:${cfg.color};box-shadow:0 0 6px ${cfg.color};flex-shrink:0;"></span>
          <span style="font-size:11px;">${cfg.icon}</span>
          <span style="color:#cbd5e1;font-size:11px;">${name}</span>
        </div>`;
      }
    }

    // ── AST node icons (only when AST sub-nodes are actually drawn) ─
    if (showsAstNodes) {
      const astTypes = [
        ['PROGRAM', '🖥️', is3D ? 'Program (orbit center)' : 'Program'],
        ['COPYBOOK', '📋', 'Copybook (shared data)'],
        ['SECTION', '📂', isSCExpanded ? 'Section — top ring' : 'Section'],
        ['PARAGRAPH', '¶', isSCExpanded ? 'Paragraph — middle ring' : 'Paragraph'],
        ['CALL', '📞', 'CALL statement'],
        ['PERFORM', '🔄', 'PERFORM (loop / call)'],
        ['DIALECT', '🗄️', 'Embedded SQL'],
        ['IF_BRANCH', '❓', 'IF branch'],
        ['EVALUATE', '🔀', 'EVALUATE / case'],
      ];
      html += sectionTitle('AST Nodes');
      for (const [t, icon, desc] of astTypes) {
        const s = ASTGalaxyView.NODE_STYLE[t] || ASTGalaxyView.DEFAULT_STYLE;
        html += swatchRow(s.bg, s.border, icon, desc);
      }
    }

    // ── Service-Catalog Expanded orbital rings ──────────────────────
    if (isSCExpanded) {
      html += sectionTitle('Orbital Rings (per program)');
      html += `<div style="font-size:11px;color:#cbd5e1;line-height:1.7;">
        <div><span style="color:#8b5cf6;">●</span> Top — Sections (structure)</div>
        <div><span style="color:#10b981;">●</span> Middle — Paragraphs (logic)</div>
        <div><span style="color:#ef4444;">●</span> Bottom — Actions (CALL/PERFORM/SQL/IF)</div>
      </div>`;
    }

    // ── Service Catalog (collapsed) — node label colors ─────────────
    if (mode === 'service-catalog' && !is3D) {
      html += sectionTitle('Node Labels');
      html += `<div style="font-size:11px;line-height:1.6;">
        <div><b style="color:#fbbf24;">Gold text</b> <span style="color:#94a3b8;">= service description</span></div>
        <div><b style="color:#60a5fa;">Blue text</b> <span style="color:#94a3b8;">= program name</span></div>
        <div><span style="color:#94a3b8;">Gray text = SQL/CALL/LOC metrics</span></div>
      </div>`;
    }

    // ── Edges (every view has dependency edges) ─────────────────────
    html += sectionTitle('Edges');
    for (const [t, s] of Object.entries(ASTGalaxyView.EDGE_STYLE)) {
      html += lineRow(s.color, !!s.dashes, `${s.label || t} <span style="color:#64748b;">(${t})</span>`);
    }
    html += lineRow('#f59e0b', false, '⚡ Cross-domain (highlighted)');

    // ── Sizing/shape hints for non-domain views ─────────────────────
    if (isTechnical) {
      html += sectionTitle('Sizing & Shape');
      html += `<div style="font-size:11px;color:#cbd5e1;line-height:1.6;">
        ⬤ Larger node = more AST children<br>
        💠 Diamond = PERFORM / IF / EVAL<br>
        ⭐ Star = CALL<br>
        🔺 Triangle = SQL
      </div>`;
    }

    // ── V2-specific layout hint ─────────────────────────────────────
    if (isV2Technical) {
      html += sectionTitle('Swim-Lane Layout (v2)');
      html += `<div style="font-size:11px;color:#cbd5e1;line-height:1.6;">
        🔽 Each program is its own <b>vertical north-south column</b>. Within a lane, nodes
        stack strictly by AST layer:<br>
        <span style="color:#3b82f6;">━ Program</span> → <span style="color:#8b5cf6;">━ Section</span>
        → <span style="color:#10b981;">━ Paragraph</span> → <span style="color:#ef4444;">━ Action</span>
        (CALL / PERFORM / SQL).<br><br>
        <span style="color:#94a3b8;">Lanes are sorted left → right by hub-ness</span>
        (most-connected programs first), so the eye lands on the most-important code first.<br><br>
        <span style="color:#94a3b8;">Inter-program edges</span> (CALL / COPY / DEPENDS_ON) ride
        as discrete arched arrows over the lanes — they don't disturb the lane structure, so
        you can trace a communication path without the diagram collapsing into spaghetti.<br><br>
        <span style="color:#94a3b8;">Trimmed for clarity:</span> only PERFORM, CALL, embedded SQL.
        MOVE / COMPUTE / DISPLAY / EXIT / IF / EVALUATE are hidden.<br><br>
        <span style="color:#94a3b8;">Tip:</span> lane name banners pinned at the top track each
        column as you pan / zoom. Drag any node to override its position.
      </div>`;
    }

    // ── 3D-specific shape & motion cues ─────────────────────────────
    if (is3D) {
      html += sectionTitle('3D Cues');
      const cues = [];
      if (isBusiness) cues.push('🔮 Large sphere = domain hub');
      if (isSCExpanded) {
        cues.push('🔶 Octahedron = Section');
        cues.push('🔷 Dodecahedron = Paragraph');
        cues.push('🔺 Tetrahedron = CALL');
        cues.push('📐 Cone = PERFORM');
        cues.push('⬜ Cube = SQL/Dialect');
        cues.push('⭕ Torus = IF/EVALUATE');
      }
      if (mode === 'business-expanded') {
        cues.push('📊 Pillars = SQL/CALL/PERFORM counts');
      }
      if (isV2) {
        cues.push('⬜ Flat card = Entry / UI');
        cues.push('🛢️ Cylinder = Data access');
      }
      if (isRadar) cues.push('🏙️ City skyline — height encodes LOC');
      cues.push('⚡ Animated particles = data flow direction');
      cues.push('🌀 Translucent ring = orbital boundary');
      html += `<div style="font-size:11px;color:#cbd5e1;line-height:1.7;">${cues.map(c => `<div>${c}</div>`).join('')}</div>`;
    }

    // ── Hints (interactions) ────────────────────────────────────────
    html += sectionTitle('Hints');
    html += `<div style="font-size:11px;color:#cbd5e1;line-height:1.7;">
      <div>🟧 Orange edge = cross-domain dependency</div>
      <div><b>Click</b> → inspector · <b>Double-click</b> → AST Explorer</div>
      <div><b>Scroll</b> → zoom · <b>Drag</b> → pan${is3D ? ' / orbit' : ''}</div>
    </div>`;

    return html;
  }

  // ── 3D floating overlay: rich legend on top of the WebGL canvas ──
  _render3DLegendOverlay(container) {
    if (!container) return;
    document.getElementById('galaxy-3d-legend')?.remove();
    const legend = document.createElement('div');
    legend.id = 'galaxy-3d-legend';
    legend.style.cssText = [
      'position:absolute', 'top:50px', 'right:10px', 'z-index:25',
      'width:248px', 'max-height:calc(100% - 70px)', 'overflow-y:auto',
      'padding:10px 12px', 'background:rgba(3,7,18,0.92)', 'color:#e2e8f0',
      'border:1px solid #334155', 'border-radius:8px', 'font-size:11px',
      'line-height:1.5', 'backdrop-filter:blur(6px)', 'box-shadow:0 4px 20px rgba(0,0,0,0.5)'
    ].join(';');
    legend.innerHTML = `
      <div style="display:flex;justify-content:flex-end;margin-bottom:-22px;">
        <button id="legend-toggle" style="background:transparent;border:1px solid #334155;color:#94a3b8;border-radius:4px;padding:1px 6px;cursor:pointer;font-size:10px;position:relative;z-index:1;">hide</button>
      </div>
      <div id="legend-body">${this._buildLegendHTML()}</div>`;
    container.appendChild(legend);
    const btn = legend.querySelector('#legend-toggle');
    const body = legend.querySelector('#legend-body');
    btn?.addEventListener('click', () => {
      const hidden = body.style.display === 'none';
      body.style.display = hidden ? '' : 'none';
      btn.textContent = hidden ? 'hide' : 'show';
    });
  }

  // ── 2D legend: floating overlay on the canvas + sidebar fallback.
  // Lives directly on the graph container so it survives node clicks
  // (which overwrite #galaxy-inspector-content) and is always visible.
  _renderLegend() {
    // 1) keep the sidebar copy (some users dock it open) but only as a fallback
    const side = document.getElementById('galaxy-legend');
    if (side) side.innerHTML = `<div style="font-size:11px;line-height:1.5;">${this._buildLegendHTML()}</div>`;

    // 2) overlay on the 2D canvas — single source of truth for the legend
    if (this.is3D) {
      // 3D path renders its own overlay via _render3DLegendOverlay
      document.getElementById('galaxy-2d-legend')?.remove();
      return;
    }
    const container = document.getElementById(this.containerId);
    if (!container) return;
    container.style.position ||= 'relative';

    document.getElementById('galaxy-2d-legend')?.remove();
    const overlay = document.createElement('div');
    overlay.id = 'galaxy-2d-legend';
    overlay.style.cssText = [
      'position:absolute', 'top:10px', 'right:10px', 'z-index:25',
      'width:260px', 'max-height:calc(100% - 24px)', 'overflow-y:auto',
      'padding:10px 12px', 'background:rgba(3,7,18,0.92)', 'color:#e2e8f0',
      'border:1px solid #334155', 'border-radius:8px', 'font-size:11px',
      'line-height:1.5', 'backdrop-filter:blur(6px)',
      'box-shadow:0 4px 20px rgba(0,0,0,0.5)', 'pointer-events:auto'
    ].join(';');
    overlay.innerHTML = `
      <div style="display:flex;justify-content:flex-end;margin-bottom:-22px;">
        <button id="galaxy-2d-legend-toggle" title="Hide legend"
          style="background:transparent;border:1px solid #334155;color:#94a3b8;border-radius:4px;padding:1px 6px;cursor:pointer;font-size:10px;position:relative;z-index:1;">hide</button>
      </div>
      <div id="galaxy-2d-legend-body">${this._buildLegendHTML()}</div>`;
    container.appendChild(overlay);

    const btn = overlay.querySelector('#galaxy-2d-legend-toggle');
    const body = overlay.querySelector('#galaxy-2d-legend-body');
    btn?.addEventListener('click', () => {
      const hidden = body.style.display === 'none';
      body.style.display = hidden ? '' : 'none';
      overlay.style.width = hidden ? '260px' : 'auto';
      btn.textContent = hidden ? 'hide' : 'legend';
      btn.title = hidden ? 'Hide legend' : 'Show legend';
    });
  }

  // ═══════════════════════════════════════════════════════════════════
  // INSPECTOR PANEL — matching RAW AST view inspector
  // ═══════════════════════════════════════════════════════════════════

  _updateInspector(nodeData) {
    const content = document.getElementById('galaxy-inspector-content');
    if (!content || !nodeData) return;

    const nodeType = nodeData.nodeType || 'UNKNOWN';
    const color = ASTGalaxyView.TYPE_COLORS[nodeType] || '#64748b';

    // Business domain hub node
    if (nodeType === 'DOMAIN') {
      const domain = nodeData.domain || nodeData.displayName;
      const cfg = ASTGalaxyView.BUSINESS_DOMAINS[domain] || { color: '#64748b', icon: '•' };
      const count = nodeData.programCount || 0;
      let html = `<div class="inspector-header" style="color:${cfg.color}">${cfg.icon} ${domain}</div>`;
      html += `<div style="color:#94a3b8; margin-bottom:8px;">Business Domain Cluster</div>`;
      html += `<table class="inspector-table">`;
      html += `<tr><td class="inspector-key">Programs</td><td class="inspector-val">${count}</td></tr>`;
      html += `</table>`;
      html += `<div style="margin-top:12px;color:#64748b;font-size:11px;">Click programs in this cluster to inspect them. Double-click to open in AST Explorer.</div>`;
      content.innerHTML = html;
      return;
    }

    const humanType = ASTGalaxyView.HUMAN_TYPES[nodeType] || nodeType;
    const cleanName = nodeData.displayName || (nodeData.name || '').replace(/.*\//, '').replace(/Context\/.*/, '') || '—';

    let html = `<div class="inspector-header" style="color:${color}">${humanType}</div>`;
    html += `<div style="color:#e2e8f0; font-weight:600; margin-bottom:8px;">${this._esc(cleanName)}</div>`;

    html += '<table class="inspector-table">';
    html += `<tr><td class="inspector-key">Type</td><td class="inspector-val">${nodeType}</td></tr>`;

    if (nodeData.domain) {
      const domainCfg = ASTGalaxyView.BUSINESS_DOMAINS[nodeData.domain] || {};
      html += `<tr><td class="inspector-key">Domain</td><td class="inspector-val" style="color:${domainCfg.color || '#94a3b8'}">${domainCfg.icon || ''} ${nodeData.domain}</td></tr>`;
    }
    if (nodeData.serviceDesc) {
      html += `<tr><td class="inspector-key">Service</td><td class="inspector-val" style="color:#fbbf24;font-weight:600;">🔧 ${this._esc(nodeData.serviceDesc)}</td></tr>`;
    }
    if (nodeData.serviceDetail) {
      html += `<tr><td colspan="2" style="color:#94a3b8;font-size:11px;padding:4px 0 8px;font-style:italic;">${this._esc(nodeData.serviceDetail)}</td></tr>`;
    }
    if (nodeData.program) {
      const progName = nodeData.program.replace('flow-ast-', '').replace('.cbl', '');
      html += `<tr><td class="inspector-key">Program</td><td class="inspector-val">${this._esc(progName)}</td></tr>`;
    }
    if (nodeData.startLine > 0) html += `<tr><td class="inspector-key">Lines</td><td class="inspector-val">${nodeData.startLine}–${nodeData.endLine}</td></tr>`;
    if (nodeData.sectionCount !== undefined) {
      html += `<tr><td class="inspector-key">Sections</td><td class="inspector-val">${nodeData.sectionCount}</td></tr>`;
      html += `<tr><td class="inspector-key">Paragraphs</td><td class="inspector-val">${nodeData.paraCount}</td></tr>`;
      html += `<tr><td class="inspector-key">AST Nodes</td><td class="inspector-val">${nodeData.nodeCount}</td></tr>`;
      html += `<tr><td class="inspector-key">SQL</td><td class="inspector-val">${nodeData.sqlCount || 0}</td></tr>`;
      html += `<tr><td class="inspector-key">CALLs</td><td class="inspector-val">${nodeData.callCount || 0}</td></tr>`;
      html += `<tr><td class="inspector-key">PERFORMs</td><td class="inspector-val">${nodeData.performCount || 0}</td></tr>`;
      html += `<tr><td class="inspector-key">Branches</td><td class="inspector-val">${nodeData.branchCount || 0}</td></tr>`;
      if (nodeData.lineCount) html += `<tr><td class="inspector-key">LOC</td><td class="inspector-val">${nodeData.lineCount}</td></tr>`;
    }
    html += '</table>';

    if (nodeData.program) {
      html += `<div style="margin-top:12px;">
        <button class="btn-small" style="width:100%;" onclick="galaxyView?.drillInto('${this._escAttr(nodeData.program)}')">🔬 Open in AST Explorer</button>
      </div>`;
    }

    content.innerHTML = html;
  }

  // ═══════════════════════════════════════════════════════════════════
  // SOURCE/DETAIL PANEL — shows program overview on click
  // ═══════════════════════════════════════════════════════════════════

  _updateSourcePanel(nodeData) {
    const panel = document.getElementById('galaxy-source-panel');
    if (!panel || !nodeData) return;

    // Domain hub — show cluster overview with all member programs
    if (nodeData.nodeType === 'DOMAIN') {
      const domain = nodeData.domain || nodeData.displayName;
      const cfg = ASTGalaxyView.BUSINESS_DOMAINS[domain] || { color: '#64748b', icon: '•' };
      const programs = this.galaxyData?.programs || [];
      const members = programs.filter(p => {
        const d = this._classifyBusinessDomain(p.program, p);
        return d === domain;
      });
      // Deduplicate
      const seen = new Set();
      const unique = members.filter(p => { if (seen.has(p.program)) return false; seen.add(p.program); return true; });

      let html = `<div style="padding:16px;">
        <div style="font-size:18px;font-weight:700;color:${cfg.color};margin-bottom:4px;">${cfg.icon} ${domain}</div>
        <div style="color:#94a3b8;font-size:12px;margin-bottom:16px;">Business Domain · ${unique.length} programs</div>
        <div style="color:#64748b;font-size:11px;margin-bottom:12px;padding:8px;background:rgba(15,23,42,0.5);border-radius:6px;border-left:3px solid ${cfg.color};">
          This cluster groups programs that serve the <b style="color:#e2e8f0">${domain.toLowerCase()}</b> business function.
          Click individual programs to inspect them.
        </div>
        <div style="font-size:12px;font-weight:600;color:#e2e8f0;margin-bottom:8px;">Programs in this domain:</div>`;

      for (const p of unique.sort((a, b) => a.program.localeCompare(b.program))) {
        const name = p.program.replace('flow-ast-', '').replace('.cbl', '');
        const tag = p.isCopybook ? '<span style="color:#f16667;font-size:10px;"> CPY</span>' : '';
        const sql = p.sqlCount > 0 ? `<span style="color:#a855f7;font-size:10px;margin-left:4px;">${p.sqlCount} SQL</span>` : '';
        html += `<div style="padding:4px 8px;margin:2px 0;background:rgba(15,23,42,0.4);border-radius:4px;display:flex;justify-content:space-between;align-items:center;cursor:pointer;" onclick="galaxyView?.drillInto('${this._escAttr(p.program)}')">
          <span style="color:#e2e8f0;font-size:12px;">${this._esc(name)}${tag}</span>${sql}
        </div>`;
      }
      html += '</div>';
      panel.innerHTML = html;
      return;
    }

    const progKey = nodeData.program;
    const displayName = nodeData.displayName || progKey?.replace('flow-ast-', '').replace('.cbl', '') || '—';
    const meta = this.galaxyData?.programs?.find(p => p.program === progKey);

    if (!meta) {
      panel.innerHTML = `<div style="padding:16px;">
        <div style="color:#e2e8f0;font-size:14px;font-weight:600;margin-bottom:8px;">${this._esc(displayName)}</div>
        <div style="color:#64748b;font-size:12px;">Type: ${nodeData.nodeType || '—'}</div>
        ${nodeData.startLine > 0 ? `<div style="color:#64748b;font-size:12px;">Lines: ${nodeData.startLine}–${nodeData.endLine}</div>` : ''}
        ${progKey ? `<button class="btn-small" style="margin-top:12px;" onclick="galaxyView?.drillInto('${this._escAttr(progKey)}')">🔬 Open in AST Explorer</button>` : ''}
      </div>`;
      return;
    }

    // Full program detail card matching structure-overview style
    const sqlCount = meta.sqlCount || 0;
    const callCount = meta.callCount || 0;
    const performCount = meta.performCount || 0;
    const domain = nodeData.domain || this._classifyBusinessDomain(progKey, meta);
    const domainCfg = ASTGalaxyView.BUSINESS_DOMAINS[domain] || { color: '#64748b', icon: '•' };

    let html = `<div style="padding:12px;">
      <div class="structure-overview">
        <div class="overview-title">${this._esc(displayName)}</div>
        <div style="color:${domainCfg.color};font-size:11px;margin-bottom:8px;">${domainCfg.icon} ${domain}</div>
        <div class="overview-grid">
          <div class="overview-stat"><span class="ov-num">${meta.sectionCount}</span><span class="ov-label">Sections</span></div>
          <div class="overview-stat"><span class="ov-num">${meta.paraCount}</span><span class="ov-label">Paragraphs</span></div>
          <div class="overview-stat"><span class="ov-num">${meta.nodeCount}</span><span class="ov-label">AST Nodes</span></div>
          ${meta.lineCount > 0 ? `<div class="overview-stat"><span class="ov-num">${meta.lineCount}</span><span class="ov-label">LOC</span></div>` : ''}
          ${sqlCount > 0 ? `<div class="overview-stat sql"><span class="ov-num">${sqlCount}</span><span class="ov-label">SQL</span></div>` : ''}
          ${callCount > 0 ? `<div class="overview-stat call"><span class="ov-num">${callCount}</span><span class="ov-label">CALLs</span></div>` : ''}
        </div>
      </div>

      <div class="structure-hint">Double-click this program in the graph to expand its AST tree</div>

      <div style="margin:12px 0;display:flex;gap:8px;flex-wrap:wrap;">
        <button class="btn-small" onclick="galaxyView?.drillInto('${this._escAttr(progKey)}')">🔬 AST Explorer</button>
        <button class="btn-small" onclick="galaxyView?.expandCluster('${this._escAttr(progKey)}')">📂 Expand AST</button>
      </div>`;

    // Statement type breakdown
    const types = [
      { label: 'PERFORM', count: performCount, color: '#06b6d4' },
      { label: 'SQL', count: sqlCount, color: '#a855f7' },
      { label: 'CALL', count: callCount, color: '#ef4444' },
      { label: 'IF/EVAL', count: meta.branchCount || 0, color: '#ec4899' },
    ].filter(t => t.count > 0);

    if (types.length > 0) {
      const maxCount = Math.max(...types.map(t => t.count));
      html += '<div class="type-breakdown" style="margin-top:8px;">';
      for (const t of types) {
        const pct = Math.round((t.count / maxCount) * 100);
        html += `<div class="type-bar-row"><span class="type-label">${t.label}</span>
          <div class="type-bar"><div class="type-bar-fill" style="width:${pct}%;background:${t.color}"></div></div>
          <span class="type-count">${t.count}</span></div>`;
      }
      html += '</div>';
    }

    html += '</div>';
    panel.innerHTML = html;
  }

  // ═══════════════════════════════════════════════════════════════════
  // STATS BAR — matching AST Explorer
  // ═══════════════════════════════════════════════════════════════════

  _updateStatsBar() {
    const el = document.getElementById('galaxy-stats-bar');
    if (!el) return;

    const nodeCount = this.nodes?.length || 0;
    const edgeCount = this.edges?.length || 0;
    const programs = this.galaxyData?.programs || [];
    const progCount = programs.filter(p => !p.isCopybook).length;
    const copyCount = programs.filter(p => p.isCopybook).length;
    const deps = this.galaxyData?.edges?.length || 0;
    const expanded = this._expandedClusters.size;

    const modeLabel = this.viewMode === 'service-catalog-v3'
      ? '🎯 Modernization Radar'
      : this.viewMode === 'service-catalog-v2'
      ? '🚀 Service Catalog v2'
      : this.viewMode === 'program-map'
      ? '📊 Programs'
      : this.viewMode === 'bian-matrix'
      ? '🏦 BIAN Service Landscape'
      : this.viewMode === 'c4-model'
      ? `🏗️ C4 Model · L${this._c4Level} ${['System Context','Containers','Components'][this._c4Level-1]}`
      : this._isServiceCatalogMode
        ? (this.viewMode === 'service-catalog-expanded-3d'
            ? '🧊 Service Catalog (Expanded 3D)'
            : this.viewMode === 'service-catalog-expanded' ? '📋 Service Catalog (Expanded)' : '📋 Service Catalog')
        : this._isBusinessMode ? '🏢 Business Domains'
          : this.viewMode === 'expanded-v2' ? '🔽 Technical (Expanded v2)'
          : this.viewMode === 'expanded' ? '📦 Technical (Expanded)'
          : '📦 Technical';
    const dimLabel = this.is3D ? '🧊 3D' : '2D';

    if (this._isBusinessMode) {
      // Count unique domains
      const seen = new Set();
      const uniqueProgs = programs.filter(p => { if (seen.has(p.program)) return false; seen.add(p.program); return true; });
      const domains = new Set(uniqueProgs.map(p => this._classifyBusinessDomain(p.program, p)));
      el.innerHTML = `
        <span class="ast-stat">${modeLabel} · ${domains.size} domains · ${nodeCount} nodes</span>
        <span class="ast-stat">· ${edgeCount} edges</span>
        ${expanded > 0 ? `<span class="ast-stat perform">· ${expanded} expanded</span>` : ''}
        <span class="ast-stat" style="margin-left:auto;color:#64748b;">${dimLabel}</span>`;
    } else {
      el.innerHTML = `
        <span class="ast-stat">${modeLabel} · ${nodeCount} nodes (${progCount} prog, ${copyCount} cpy)</span>
        <span class="ast-stat">· ${edgeCount} edges · ${deps} deps</span>
        ${expanded > 0 ? `<span class="ast-stat perform">· ${expanded} expanded</span>` : ''}
        <span class="ast-stat" style="margin-left:auto;color:#64748b;">${dimLabel}</span>`;
    }
  }

  // ═══════════════════════════════════════════════════════════════════
  // FILE FILTER & SHOW/SORT
  // ═══════════════════════════════════════════════════════════════════

  _populateFileFilter() {
    const select = document.getElementById('galaxy-file-filter');
    if (!select) return;
    select.querySelectorAll('option:not(:first-child)').forEach(o => o.remove());

    const programs = this._getSortedPrograms();
    for (const p of programs) {
      const opt = document.createElement('option');
      opt.value = p.program;
      const name = p.program.replace('flow-ast-', '');
      const tag = p.isCopybook ? ' [CPY]' : '';
      opt.textContent = `${name}${tag}`;
      select.appendChild(opt);
    }
  }

  _getSortedPrograms() {
    let programs = [...(this.galaxyData?.programs || [])];
    // Apply show filter
    programs = this._applyShowFilter(programs);
    // Apply sort
    programs = this._applySortMode(programs);
    return programs;
  }

  _applyShowFilter(programs) {
    switch (this.showFilter) {
      case 'programs': return programs.filter(p => !p.isCopybook);
      case 'copybooks': return programs.filter(p => p.isCopybook);
      case 'sql-heavy': return programs.filter(p => p.sqlCount > 0);
      case 'call-heavy': return programs.filter(p => p.callCount > 0);
      case 'perform-heavy': return programs.filter(p => p.performCount > 3);
      case 'complex': return programs.filter(p => this._computeComplexity(p) > 0.4);
      case 'simple': return programs.filter(p => this._computeComplexity(p) <= 0.4);
      default: return programs;
    }
  }

  _applySortMode(programs) {
    const edgeCount = new Map();
    if (this.galaxyData?.edges) {
      for (const e of this.galaxyData.edges) {
        edgeCount.set(e.source, (edgeCount.get(e.source) || 0) + 1);
        edgeCount.set(e.target, (edgeCount.get(e.target) || 0) + 1);
      }
    }
    switch (this.sortMode) {
      case 'loc': return programs.sort((a, b) => (b.lineCount || 0) - (a.lineCount || 0));
      case 'complexity': return programs.sort((a, b) => this._computeComplexity(b) - this._computeComplexity(a));
      case 'sql': return programs.sort((a, b) => (b.sqlCount || 0) - (a.sqlCount || 0));
      case 'calls': return programs.sort((a, b) => (b.callCount || 0) - (a.callCount || 0));
      case 'sections': return programs.sort((a, b) => (b.sectionCount || 0) - (a.sectionCount || 0));
      case 'connections': return programs.sort((a, b) => {
        const aName = a.program.replace('flow-ast-','').replace('.cbl','');
        const bName = b.program.replace('flow-ast-','').replace('.cbl','');
        return (edgeCount.get(bName) || edgeCount.get(bName+'.cbl') || 0) - (edgeCount.get(aName) || edgeCount.get(aName+'.cbl') || 0);
      });
      default: return programs.sort((a, b) => a.program.localeCompare(b.program));
    }
  }

  _computeComplexity(prog) {
    const factors = [
      Math.min(1, (prog.branchCount || 0) / 30),
      Math.min(1, (prog.sqlCount || 0) / 20),
      Math.min(1, (prog.callCount || 0) / 10),
      Math.min(1, (prog.paraCount || 0) / 40),
      Math.min(1, (prog.nodeCount || 0) / 500),
    ];
    return factors.reduce((a, b) => a + b, 0) / factors.length;
  }

  // ═══════════════════════════════════════════════════════════════════
  // CONTROLS
  // ═══════════════════════════════════════════════════════════════════

  setFilter(value) {
    this.filter = value;
    this._expandedClusters.clear();
    this._rebuildAndRender();
  }

  setViewMode(value) {
    this.viewMode = value;
    if (value === 'expanded' || value === 'expanded-v2' || value === 'program-map') {
      const programs = this._getSortedPrograms();
      for (const p of programs) this._expandedClusters.add(p.program);
    } else if (value === 'business') {
      this._expandedClusters.clear();
    } else if (value === 'business-expanded') {
      // Expand all business domains so every member program is visible
      this._expandedClusters.clear();
      const programs = this.galaxyData?.programs || [];
      const seen = new Set();
      for (const p of programs) {
        if (seen.has(p.program)) continue;
        seen.add(p.program);
        const domain = this._classifyBusinessDomain(p.program, p);
        this._expandedClusters.add(domain);
      }
    } else if (value === 'service-catalog') {
      this._expandedClusters.clear();
      const programs2 = this.galaxyData?.programs || [];
      const seen2 = new Set();
      for (const p of programs2) {
        if (seen2.has(p.program)) continue;
        seen2.add(p.program);
        this._expandedClusters.add(this._classifyBusinessDomain(p.program, p));
      }
    } else if (value === 'service-catalog-expanded' || value === 'service-catalog-expanded-3d') {
      // Expand all domains AND all programs (show AST structure within each service)
      this._expandedClusters.clear();
      const programs3 = this.galaxyData?.programs || [];
      const seen3 = new Set();
      for (const p of programs3) {
        if (seen3.has(p.program)) continue;
        seen3.add(p.program);
        this._expandedClusters.add(this._classifyBusinessDomain(p.program, p));
        this._expandedClusters.add(p.program); // also expand individual programs
      }
      // Auto-enable 3D for the dedicated 3D variant
      if (value === 'service-catalog-expanded-3d' && !this.is3D) {
        this.is3D = true;
        const btn = document.getElementById('galaxy-3d-btn');
        if (btn) btn.classList.add('galaxy-3d-active');
      }
    } else if (value === 'service-catalog-v2') {
      // V2: force 3D mode, expand all domains
      this._expandedClusters.clear();
      const programs4 = this.galaxyData?.programs || [];
      const seen4 = new Set();
      for (const p of programs4) {
        if (seen4.has(p.program)) continue;
        seen4.add(p.program);
        this._expandedClusters.add(this._classifyBusinessDomain(p.program, p));
      }
      // Auto-enable 3D for v2
      if (!this.is3D) {
        this.is3D = true;
        const btn = document.getElementById('galaxy-3d-btn');
        if (btn) btn.classList.add('galaxy-3d-active');
      }
    } else if (value === 'service-catalog-v3') {
      this._expandedClusters.clear();
      const programs5 = this.galaxyData?.programs || [];
      const seen5 = new Set();
      for (const p of programs5) {
        if (seen5.has(p.program)) continue;
        seen5.add(p.program);
        this._expandedClusters.add(this._classifyBusinessDomain(p.program, p));
      }
    } else if (value === 'bian-matrix') {
      this._expandedClusters.clear();
    } else if (value === 'c4-model') {
      this._expandedClusters.clear();
      this._c4Level = 1;
      this._c4SelectedProg = null;
    } else if (value === 'program-map') {
      this._expandedClusters.clear();
    } else {
      this._expandedClusters.clear();
    }
    // If currently in 3D, re-render 3D with appropriate mode
    if (this.is3D) {
      const g3d = document.getElementById('galaxy-3d-container');
      const g2d = document.getElementById(this.containerId);
      if (g3d) {
        // Ensure 3D container is visible and 2D is hidden
        if (g2d) g2d.style.display = 'none';
        g3d.style.display = '';
        g3d.style.flex = '5';
        if (this.graph3d) { this.graph3d._destructor?.(); this.graph3d = null; }
        g3d.innerHTML = '';
        this._render3D(g3d);
        this._3dCancelOverlayShow();
      }
      this._updateStatsBar();
      this._renderLegend();
    } else {
      this._rebuildAndRender();
    }
  }

  setShowFilter(value) {
    this.showFilter = value;
    this._expandedClusters.clear();
    this._rebuildAndRender();
  }

  setSortMode(value) {
    this.sortMode = value;
    this._populateFileFilter();
  }

  search(query) {
    this._searchTerm = (query || '').toLowerCase();
    if (this.is3D) {
      this._3dSearchTerm = this._searchTerm;
      this._update3DHighlight();
      return;
    }
    if (!this.network || !this.nodes) return;
    if (!this._searchTerm || this._searchTerm.length < 2) {
      // Reset all highlights
      const updates = [];
      this.nodes.forEach(n => {
        if (n._origColor) updates.push({ id: n.id, color: n._origColor, font: { color: '#e2e8f0', size: n._origFontSize || 10, multi: true }, _origColor: undefined, _origFontSize: undefined });
      });
      if (updates.length) this.nodes.update(updates);
      return;
    }
    const updates = [];
    let firstMatch = null;
    this.nodes.forEach(n => {
      const label = (n.label || '').toLowerCase();
      const match = label.includes(this._searchTerm);
      if (match) {
        if (!firstMatch) firstMatch = n.id;
        if (!n._origColor) updates.push({ id: n.id, _origColor: n.color, _origFontSize: n.font?.size || 10, color: { background: '#fbbf24', border: '#f59e0b' }, font: { color: '#0f172a', size: 14, bold: true, multi: true } });
      } else {
        if (n._origColor) {
          updates.push({ id: n.id, color: n._origColor, font: { color: '#e2e8f0', size: n._origFontSize || 10, multi: true }, _origColor: undefined, _origFontSize: undefined });
        } else {
          updates.push({ id: n.id, color: { background: '#1e293b', border: '#0f172a' }, font: { color: '#334155', size: 10, multi: true } });
        }
      }
    });
    if (updates.length) this.nodes.update(updates);
    if (firstMatch) this.network.focus(firstMatch, { scale: 1.2, animation: { duration: 400, easingFunction: 'easeInOutQuad' } });
  }

  expandCluster(progKey) {
    this._expandedClusters.add(progKey);
    this._rebuildAndRender();
  }

  // ═══════════════════════════════════════════════════════════════════
  // BIAN-ALIGNED SERVICE LANDSCAPE (V14.0)
  // HTML matrix view — programs mapped to BIAN service domains
  // ═══════════════════════════════════════════════════════════════════

  static get BIAN_LANDSCAPE() {
    return [
      {
        area: 'Operations & Execution', icon: '⚙️',
        areaColor: '#1e3a8a', borderColor: '#3b82f6',
        domains: [
          { name: 'Current Account',            icon: '🏦', bianRef: 'BIAN::CurrentAccount',
            // BNK1* = MicroFocus Bank sample; CBACT*/COACT* = AWS CardDemo account management
            matches: ['CREACC','DELACC','INQACC','UPDACC','BNK1CAC','BNK1UAC',
                      'CBACT01C','CBACT02C','CBACT03C','CBACT04C',
                      'COACTUPC','COACTVNC','COACTVWC'],
            desc: 'Manage demand/current account lifecycle' },
          { name: 'Customer Agreement',          icon: '🤝', bianRef: 'BIAN::CustomerAgreement',
            // CBCUSR/COUSR = CardDemo customer management; CUSTOMER-* = screen programs
            matches: ['CRECUST','DELCUS','BNK1CRA',
                      'CBCUSR1C','CBCUS01C',
                      'COUSR00C','COUSR01C','COUSR02C','COUSR03C',
                      'CUSTOMER-DISPLAY','CUSTOMER-INQUIRY',
                      'INQCUST','INQUCUST','UPDCUST','UPQCUST','CHECUST_TEST','CRECUST_TEST'],
            desc: 'Establish and maintain customer contracts' },
          { name: 'Customer Profile',            icon: '👤', bianRef: 'BIAN::PartyDataManagement',
            matches: ['INQACCCU'],
            desc: 'Consolidated customer data lookup' },
          { name: 'Fund Transfer',               icon: '💸', bianRef: 'BIAN::FundsTransferPricing',
            matches: ['XFRFUN','BNK1TFN'],
            desc: 'Initiate and settle fund transfers' },
          { name: 'Financial Transaction',       icon: '💳', bianRef: 'BIAN::CardTransactionSwitch',
            // CBTRN*/COTRN* = CardDemo transaction batch/CICS; COBIL = billing
            matches: ['DBCRFUN',
                      'CBTRNR1C','CBTRNR2C','CBTRNR3C',
                      'CBTRN01C','CBTRN02C','CBTRN03C',
                      'COTRN00C','COTRN01C','COTRN02C','COTRN03C',
                      'COBIL00C'],
            desc: 'Debit/credit and card transaction processing' },
          { name: 'Card Management',             icon: '💎', bianRef: 'BIAN::CardCase',
            // COCRD* = card CICS screens; CRDTAGY* = credit agency batch
            matches: ['COCRDSLC','COCRDUPE','COCRDUPC',
                      'CRDTAGY1','CRDTAGY2','CRDTAGY3','CRDTAGY4','CRDTAGY5'],
            desc: 'Credit card issuance, disputes, and agency reporting' },
        ],
      },
      {
        area: 'Risk & Compliance', icon: '🛡️',
        areaColor: '#7f1d1d', borderColor: '#ef4444',
        domains: [
          { name: 'Regulatory Reporting',        icon: '📋', bianRef: 'BIAN::RegulatoryReporting',
            matches: ['RGNB649','CORPTG0C','CORPT00C'],
            desc: 'Generate mandated regulatory submissions' },
        ],
      },
      {
        area: 'Business Support', icon: '🔧',
        areaColor: '#1c1917', borderColor: '#78716c',
        domains: [
          { name: 'Data Management',             icon: '🗄️', bianRef: 'BIAN::DataManagement',
            matches: ['BANKDATA','BDSDA23','BDSDA2F','BDSM043','BDSMFJL',
                      'CBEXPORT','CBIMPORT'],
            desc: 'Shared data structures and batch utilities' },
          { name: 'System Administration',       icon: '🔐', bianRef: 'BIAN::ITSystemAdministration',
            // COADM = admin; COSGNN/COSGN = sign-on variants; ABNDPROC = abend handler; utilities
            matches: ['COADM01C',
                      'COSGNN0C','COSGNN00C','COSGN00C',
                      'ABNDPROC','COBSWAIT','CSUTLDTC'],
            desc: 'Sign-on, administration, error handling and utilities' },
        ],
      },
    ];
  }

  _bianMatchProgram(progName) {
    const key = (progName || '').replace(/\.cbl$/i,'').replace(/^flow-ast-/i,'').toUpperCase();
    for (const area of ASTGalaxyView.BIAN_LANDSCAPE) {
      for (const domain of area.domains) {
        if (domain.matches.includes(key)) return { area: area.area, domain: domain.name };
      }
    }
    return { area: null, domain: 'Unmapped' };
  }

  _renderHtmlMode(container) {
    if (this.viewMode === 'bian-matrix') return this._renderBianMatrix(container);
  }

  _renderBianMatrix(container) {
    if (this.network) { this.network.destroy(); this.network = null; }
    const programs = this.galaxyData?.programs || [];
    const norm = p => (p.program || '').replace(/\.cbl$/i,'').replace(/^flow-ast-/i,'').toUpperCase();

    // Map programs to BIAN domains
    const domainMap = new Map();  // "Area||Domain" → program[]
    const copybooks = [];
    const unmapped = [];
    for (const p of programs) {
      if (p.isCopybook) { copybooks.push(p); continue; }
      const key = norm(p);
      const { area, domain } = this._bianMatchProgram(p.program);
      if (!area) { unmapped.push(p); continue; }
      const mk = `${area}||${domain}`;
      if (!domainMap.has(mk)) domainMap.set(mk, []);
      domainMap.get(mk).push(p);
    }

    const chip = (p, color) => {
      const k = norm(p);
      const loc  = p.lineCount   || 0;
      const sql  = p.sqlCount    || 0;
      const call = p.callCount   || 0;
      const para = p.paraCount   || 0;
      const sqlTag = sql  > 0 ? `▪ ${sql} SQL`   : '';
      const callTag= call > 0 ? `▪ ${call} CALLs` : '';
      const badge = color || (sql > 20 ? '#7c3aed' : call > 5 ? '#b45309' : '#1e40af');
      return `<span style="display:inline-flex;align-items:center;gap:4px;margin:3px;padding:4px 8px;background:${badge};border-radius:12px;font-size:11px;color:#e2e8f0;cursor:pointer;white-space:nowrap;"
        title="${k}&#10;LOC: ${loc} · Sections: ${p.sectionCount||0} · Paragraphs: ${para}&#10;${sqlTag} ${callTag}"
        onclick="if(typeof astExplorer!=='undefined'&&astExplorer)astExplorer.drillIntoProgram('${k}.cbl')">
        ${k}${sql>0?'<span style="font-size:9px;opacity:.7"> ⚡</span>':''}
      </span>`;
    };

    const checked = this._bianShowCopybooks ? 'checked' : '';
    let html = `<div style="height:100%;overflow:auto;padding:16px;font-family:monospace;background:#0f172a;">
      <div style="margin-bottom:8px;">
        <span style="font-size:16px;font-weight:700;color:#e2e8f0;">🏦 BIAN-aligned Service Landscape</span>
        <span style="font-size:11px;color:#64748b;margin-left:12px;">V14.0 · heuristic mapping based on program naming conventions · click a chip to open in AST Explorer</span>
      </div>
      <div style="display:flex;align-items:center;gap:16px;margin-bottom:12px;flex-wrap:wrap;font-size:11px;color:#64748b;padding:8px 10px;background:#1e293b;border-radius:6px;">
        <span>⚡ = SQL-heavy (purple)</span>
        <span>▪ = CALL-heavy (amber)</span>
        <span>Click chip → AST Explorer</span>
        <span style="width:1px;height:16px;background:#334155;"></span>
        <label style="display:flex;align-items:center;gap:7px;cursor:pointer;font-size:12px;color:#e2e8f0;padding:4px 10px;background:#0f172a;border:1px solid ${this._bianShowCopybooks ? '#3b82f6' : '#334155'};border-radius:20px;transition:border-color .2s;"
          title="Show the copybooks (.cpy) included by the programs above">
          <input type="checkbox" ${checked} style="accent-color:#3b82f6;width:14px;height:14px;cursor:pointer;"
            onchange="galaxyView._bianShowCopybooks=this.checked;galaxyView._rebuildAndRender()">
          📂 Show Copybooks
        </label>
      </div>`;

    for (const area of ASTGalaxyView.BIAN_LANDSCAPE) {
      html += `<div style="margin-bottom:16px;border:1px solid ${area.borderColor};border-radius:8px;overflow:hidden;">
        <div style="background:${area.areaColor};padding:8px 14px;display:flex;align-items:center;gap:8px;">
          <span style="font-size:15px;">${area.icon}</span>
          <span style="font-weight:700;color:#e2e8f0;font-size:14px;">${area.area}</span>
          <span style="font-size:10px;color:#94a3b8;margin-left:auto;">BIAN Business Area</span>
        </div>
        <div style="display:flex;flex-wrap:wrap;gap:0;background:#0f172a;">`;

      for (const domain of area.domains) {
        const mk = `${area.area}||${domain.name}`;
        const progs = domainMap.get(mk) || [];
        html += `<div style="min-width:220px;flex:1;border-right:1px solid #1e293b;border-bottom:1px solid #1e293b;padding:10px;">
          <div style="font-size:12px;font-weight:600;color:#94a3b8;margin-bottom:4px;display:flex;align-items:center;gap:6px;">
            <span>${domain.icon}</span>
            <span>${domain.name}</span>
            <span style="font-size:9px;color:#475569;margin-left:auto;">${domain.bianRef}</span>
          </div>
          <div style="font-size:10px;color:#475569;margin-bottom:8px;">${domain.desc}</div>
          <div style="display:flex;flex-wrap:wrap;min-height:32px;">
            ${progs.length ? progs.map(chip).join('') : '<span style="font-size:10px;color:#334155;font-style:italic;">no programs mapped</span>'}
          </div>
        </div>`;
      }
      html += `</div></div>`;
    }

    if (unmapped.length) {
      html += `<div style="border:1px solid #334155;border-radius:8px;overflow:hidden;margin-bottom:16px;">
        <div style="background:#1e293b;padding:8px 14px;font-weight:600;color:#64748b;font-size:13px;">⚠️ Unmapped Programs</div>
        <div style="padding:10px;display:flex;flex-wrap:wrap;">${unmapped.map(p => chip(p)).join('')}</div>
      </div>`;
    }

    if (this._bianShowCopybooks && copybooks.length) {
      const sortedCpy = [...copybooks].sort((a,b)=>norm(a).localeCompare(norm(b)));
      html += `<div style="border:1px solid #1e3a5f;border-radius:8px;overflow:hidden;margin-bottom:16px;">
        <div style="background:#0c1f3a;padding:8px 14px;display:flex;align-items:center;gap:8px;">
          <span style="font-size:15px;">📂</span>
          <span style="font-weight:700;color:#93c5fd;font-size:14px;">Shared Copybooks</span>
          <span style="font-size:10px;color:#475569;margin-left:auto;">${sortedCpy.length} .cpy files · included by programs above</span>
        </div>
        <div style="padding:10px;display:flex;flex-wrap:wrap;">${sortedCpy.map(p => chip(p,'#1e3a5f')).join('')}</div>
      </div>`;
    }

    html += `<div style="font-size:10px;color:#334155;padding:8px 0;">
      BIAN Service Landscape V14.0 · Banking Industry Architecture Network ·
      <a href="https://bian.org" target="_blank" style="color:#475569;">bian.org</a>
    </div></div>`;

    container.innerHTML = html;
  }

  // ═══════════════════════════════════════════════════════════════════
  // C4 MODEL VIEW (Structurizr-style)
  // L1 System Context · L2 Containers · L3 Components
  // ═══════════════════════════════════════════════════════════════════

  static get C4_CONTAINER_MAP() {
    return {
      'Online (CICS)':      { color: '#1e40af', border: '#3b82f6', icon: '🖥️',  keys: ['BNK1CAC','BNK1CRA','BNK1TFN','BNK1UAC'] },
      'Business Logic':     { color: '#065f46', border: '#10b981', icon: '⚙️',  keys: ['CREACC','CRECUST','DBCRFUN','DELACC','DELCUS','INQACC','INQACCCU','UPDACC','XFRFUN'] },
      'Batch Processing':   { color: '#78350f', border: '#f59e0b', icon: '📦',  keys: ['BDSM043','BDSMFJL','RGNB649'] },
      'Shared Data':        { color: '#1c1917', border: '#78716c', icon: '🗄️',  keys: ['BANKDATA','BDSDA23','BDSDA2F'] },
    };
  }

  _c4ContainerOf(progName) {
    const key = (progName||'').replace(/\.cbl$/i,'').replace(/^flow-ast-/i,'').toUpperCase();
    for (const [name, cfg] of Object.entries(ASTGalaxyView.C4_CONTAINER_MAP)) {
      if (cfg.keys.includes(key)) return name;
    }
    return null;
  }

  _buildC4VisData() {
    const nodeList = [], edgeList = [];
    const programs = this.galaxyData?.programs || [];
    const norm = p => (p.program||'').replace(/\.cbl$/i,'').replace(/^flow-ast-/i,'').toUpperCase();
    const level = this._c4Level;

    if (level === 1) {
      // L1: System Context
      const box  = (id,lbl,sub,col,brd,shp,sz,x,y) => ({ id, label:`${lbl}\n[${sub}]`, x, y, fixed:{x:true,y:true},
        shape: shp, size: sz, color:{background:col,border:brd,highlight:{background:col,border:'#fff'}},
        font:{color:'#e2e8f0',size:12,multi:false}, borderWidth:2, _data:{displayName:lbl,nodeType:'C4_'+sub} });
      nodeList.push(
        { id:'actor_customer', label:'Bank Customer\n[Person]', shape:'ellipse', size:30, x:-300, y:0, fixed:{x:true,y:true},
          color:{background:'#1e3a5f',border:'#3b82f6'}, font:{color:'#e2e8f0',size:11}, borderWidth:2, _data:{displayName:'Bank Customer',nodeType:'C4_Person'} },
        { id:'actor_staff',    label:'Bank Staff\n[Person]',    shape:'ellipse', size:30, x:300, y:0, fixed:{x:true,y:true},
          color:{background:'#1e3a5f',border:'#3b82f6'}, font:{color:'#e2e8f0',size:11}, borderWidth:2, _data:{displayName:'Bank Staff',nodeType:'C4_Person'} },
        box('sys_core','Core Banking System','Software System','#1e3a8a','#60a5fa','box',60,0,0),
        box('ext_db',   'DB2 / VSAM',        'Database',        '#1c1917','#78716c','database',35,-200,200),
        box('ext_pay',  'Payment Network',   'External System', '#451a03','#f59e0b','box',35,200,200),
        box('ext_reg',  'Regulatory Auth.',  'External System', '#450a0a','#ef4444','box',35,0,280),
      );
      edgeList.push(
        {from:'actor_customer',to:'sys_core',label:'Uses',arrows:{to:{enabled:true,scaleFactor:.6}},dashes:false,color:{color:'#3b82f6'}},
        {from:'actor_staff',   to:'sys_core',label:'Uses',arrows:{to:{enabled:true,scaleFactor:.6}},dashes:false,color:{color:'#3b82f6'}},
        {from:'sys_core',to:'ext_db',  label:'Reads/Writes',arrows:{to:{enabled:true,scaleFactor:.6}},dashes:true,color:{color:'#78716c'}},
        {from:'sys_core',to:'ext_pay', label:'Initiates',   arrows:{to:{enabled:true,scaleFactor:.6}},dashes:true,color:{color:'#f59e0b'}},
        {from:'sys_core',to:'ext_reg', label:'Reports to',  arrows:{to:{enabled:true,scaleFactor:.6}},dashes:true,color:{color:'#ef4444'}},
      );
    } else if (level === 2) {
      // L2: Containers — one node per container group + DB2
      const containerMap = ASTGalaxyView.C4_CONTAINER_MAP;
      let col = 0;
      const contX = { 'Online (CICS)': -450, 'Business Logic': -150, 'Batch Processing': 150, 'Shared Data': 450 };
      for (const [name, cfg] of Object.entries(containerMap)) {
        const progs = programs.filter(p => cfg.keys.includes(norm(p)));
        const loc  = progs.reduce((s,p) => s+(p.lineCount||0), 0);
        const sql  = progs.reduce((s,p) => s+(p.sqlCount||0), 0);
        nodeList.push({
          id: 'cont__' + name,
          label: `${cfg.icon} ${name}\n[COBOL Container]\n${progs.length} programs · ${loc} LOC`,
          shape: 'box', size: 40,
          x: contX[name] || col * 320, y: 0, fixed:{x:true,y:true},
          color:{background:cfg.color,border:cfg.border,highlight:{background:cfg.color,border:'#fff'}},
          font:{color:'#e2e8f0',size:11,multi:false}, borderWidth:2,
          title: `${name}\n${progs.map(p=>norm(p)).join(', ')}\nTotal LOC: ${loc} · SQL stmts: ${sql}`,
          _data:{displayName:name,nodeType:'C4_Container',programs:progs},
        });
        // Program nodes below each container
        progs.forEach((p, i) => {
          const pn = norm(p);
          nodeList.push({
            id: 'prog_c4__' + pn,
            label: pn,
            shape: 'box', size:20,
            x: (contX[name]||0) + (i - (progs.length-1)/2) * 110,
            y: 160,
            fixed:{x:true,y:true},
            color:{background:cfg.color+'99',border:cfg.border,highlight:{background:cfg.color,border:'#fff'}},
            font:{color:'#cbd5e1',size:10}, borderWidth:1,
            title:`${pn}\nLOC: ${p.lineCount||0} · SQL: ${p.sqlCount||0} · CALLs: ${p.callCount||0}`,
            _data:{...p,displayName:pn,nodeType:'C4_Component',program:p.program},
          });
          edgeList.push({from:'cont__'+name,to:'prog_c4__'+pn,arrows:{to:{enabled:true,scaleFactor:.5}},color:{color:cfg.border,opacity:.4},width:.5,dashes:false});
        });
        col++;
      }
      nodeList.push({
        id:'ext_db_c', label:'DB2 / VSAM\n[Database]', shape:'database', size:30,
        x:0, y:-200, fixed:{x:true,y:true},
        color:{background:'#1c1917',border:'#78716c'}, font:{color:'#e2e8f0',size:11}, borderWidth:2,
        _data:{displayName:'DB2/VSAM',nodeType:'C4_Database'},
      });
      // Wire SQL-heavy containers to DB2
      for (const [name, cfg] of Object.entries(containerMap)) {
        const hasSql = programs.filter(p => cfg.keys.includes(norm(p))).some(p => (p.sqlCount||0)>0);
        if (hasSql) edgeList.push({from:'cont__'+name,to:'ext_db_c',label:'SQL',arrows:{to:{enabled:true,scaleFactor:.5}},dashes:true,color:{color:'#78716c',opacity:.5}});
      }
      // Inter-container dependency edges from galaxy edges
      const edgesData = this.galaxyData?.edges || [];
      const seen = new Set();
      for (const e of edgesData) {
        const sc = this._c4ContainerOf(e.source), tc = this._c4ContainerOf(e.target);
        if (!sc || !tc || sc === tc) continue;
        const key = `${sc}→${tc}`;
        if (seen.has(key)) continue; seen.add(key);
        edgeList.push({from:'cont__'+sc,to:'cont__'+tc,label:'calls',arrows:{to:{enabled:true,scaleFactor:.5}},dashes:false,color:{color:'#94a3b8',opacity:.6},width:1.5});
      }
    } else {
      // L3: Components — sections of selected program
      const selProg = this._c4SelectedProg;
      const astNodes = this.astData?.nodes || [];
      const astEdges = this.astData?.edges || [];
      const progNodes = astNodes.filter(n => {
        const pn = (n.program||'').replace(/\.cbl$/i,'').replace(/^flow-ast-/i,'').toUpperCase();
        return selProg ? pn === selProg : true;
      }).filter(n => ['SECTION','PARAGRAPH','PERFORM','CALL','CallStatement','IF_BRANCH','EVALUATE','DIALECT','DIALECT_CONTAINER'].includes(n.nodeType));

      const nodeIds = new Set(progNodes.map(n=>n.id));
      nodeList.push(...progNodes.map(n => ({
        id: n.id, label: (n.name||n.nodeType||'').substring(0,24),
        shape: ASTGalaxyView.NODE_STYLE[n.nodeType]?.shape || 'ellipse',
        size: 18,
        color: { background: ASTGalaxyView.TYPE_COLORS[n.nodeType] || '#475569', border: '#fff2', highlight:{border:'#fff'} },
        font: {color:'#e2e8f0',size:10}, borderWidth:1,
        title:`${n.nodeType}: ${n.name||''}\nLines ${n.startLine}–${n.endLine}`,
        _data:{...n,displayName:n.name||n.nodeType,program:n.program},
      })));
      edgeList.push(...astEdges
        .filter(e => nodeIds.has(e.source) && nodeIds.has(e.target))
        .map(e => ({from:e.source,to:e.target,arrows:{to:{enabled:true,scaleFactor:.5}},color:{color:'#475569',opacity:.5},width:.8})));
    }

    this.nodes = new vis.DataSet(nodeList);
    this.edges = new vis.DataSet(edgeList);
  }

  _injectC4LevelUI(container) {
    // Remove any prior C4 level bar
    container.querySelectorAll('.c4-level-bar').forEach(el => el.remove());
    const bar = document.createElement('div');
    bar.className = 'c4-level-bar';
    bar.style.cssText = 'position:absolute;top:8px;left:50%;transform:translateX(-50%);z-index:20;display:flex;gap:6px;background:rgba(3,7,18,0.88);border:1px solid #334155;border-radius:20px;padding:5px 10px;backdrop-filter:blur(6px);';
    const levels = [
      ['L1','System Context'],
      ['L2','Containers'],
      ['L3','Components'],
    ];
    levels.forEach(([code, label], idx) => {
      const btn = document.createElement('button');
      const active = (this._c4Level === idx+1);
      btn.textContent = `${code} · ${label}`;
      btn.style.cssText = `padding:4px 14px;border-radius:14px;border:1px solid ${active?'#3b82f6':'#334155'};background:${active?'#1e40af':'transparent'};color:${active?'#fff':'#94a3b8'};font-size:11px;font-weight:${active?700:400};cursor:pointer;transition:all .15s;`;
      btn.title = `Switch to C4 Level ${idx+1}: ${label}`;
      btn.addEventListener('click', () => {
        this._c4Level = idx+1;
        this._c4SelectedProg = null;
        this._rebuildAndRender();
      });
      bar.appendChild(btn);
    });
    if (this._c4Level === 3 && this._c4SelectedProg) {
      const sel = document.createElement('span');
      sel.style.cssText = 'padding:4px 10px;border-radius:14px;background:#065f46;color:#6ee7b7;font-size:11px;';
      sel.textContent = `▸ ${this._c4SelectedProg}`;
      bar.appendChild(sel);
    }
    container.style.position = 'relative';
    container.appendChild(bar);
  }

  _rebuildAndRender() {
    const container = document.getElementById(this.containerId);
    if (this._isHtmlMode) {
      if (this.network) { this.network.destroy(); this.network = null; }
      if (container) this._renderHtmlMode(container);
      this._updateStatsBar();
      this._renderLegend();
      return;
    }
    if (this.viewMode === 'c4-model') {
      this._buildC4VisData();
    } else if (this.viewMode === 'service-catalog-v3') {
      this._buildModernizationRadarVisData();
    } else if (this._isServiceCatalogMode) {
      this._buildServiceCatalogVisData();
    } else if (this._isBusinessMode) {
      this._buildBusinessDomainVisData();
    } else {
      this._buildVisData();
    }
    this._renderVisNetwork(document.getElementById(this.containerId));
    this._updateStatsBar();
    this._renderLegend();
  }

  toggleFullscreen() {
    const container = document.getElementById('ast-galaxy-container');
    if (!container) return;
    this.isFullscreen = !this.isFullscreen;
    container.classList.toggle('galaxy-fullscreen', this.isFullscreen);
    const btn = document.getElementById('galaxy-fullscreen-btn');
    if (btn) btn.textContent = this.isFullscreen ? '⛶ Exit' : '⛶ Full';
    // Resize vis-network / 3D after fullscreen toggle
    setTimeout(() => {
      if (this.network) this.network.fit({ animation: { duration: 300 } });
      if (this.graph3d) {
        const g3d = document.getElementById('galaxy-3d-container');
        if (g3d) this.graph3d.width(g3d.offsetWidth).height(g3d.offsetHeight);
      }
    }, 100);
  }

  drillInto(programName) {
    if (!programName) return;
    // Ensure AST Explorer tab is active and instantiated.
    if (typeof switchDashboard === 'function') switchDashboard('ast');
    // Lazy-init may have just created astExplorer inside switchDashboard.
    const explorer = (typeof astExplorer !== 'undefined' && astExplorer) ? astExplorer : window.astExplorer;
    if (explorer && typeof explorer.drillIntoProgram === 'function') {
      explorer.drillIntoProgram(programName);
    } else {
      console.warn('AST Explorer not ready for drill-through:', programName);
    }
  }

  _update3DHighlight() {
    if (!this.graph3d) return;
    // Force re-render with updated colors/sizes based on search
    this.graph3d.nodeColor(this.graph3d.nodeColor());
    this.graph3d.nodeVal(this.graph3d.nodeVal());
    // Focus on first match
    if (this._3dSearchTerm && this._3dSearchTerm.length >= 2) {
      const data = this.graph3d.graphData();
      const match = data.nodes.find(n => (n.name || '').toLowerCase().includes(this._3dSearchTerm));
      if (match && match.x !== undefined) {
        const dist = 150;
        const ratio = 1 + dist / Math.hypot(match.x, match.y, match.z);
        this.graph3d.cameraPosition({ x: match.x * ratio, y: match.y * ratio, z: match.z * ratio }, match, 800);
      }
    }
  }

  refresh() {
    this.galaxyData = null;
    this.astData = null;
    this._expandedClusters.clear();
    this.loadAndRender();
  }

  destroy() {
    if (this.network) { this.network.destroy(); this.network = null; }
    if (this.graph3d) { this.graph3d._destructor?.(); this.graph3d = null; }
    const g3d = document.getElementById('galaxy-3d-container');
    if (g3d) g3d.innerHTML = '';
    document.getElementById('galaxy-2d-legend')?.remove();
    document.getElementById('galaxy-3d-legend')?.remove();
    document.querySelector('.galaxy-v2-lanes')?.remove();
  }

  _esc(s) { return (s || '').replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;'); }
  _escAttr(s) { return (s || '').replace(/'/g, "\\'").replace(/"/g, '&quot;'); }
}
