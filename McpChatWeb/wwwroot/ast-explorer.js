// ═══════════════════════════════════════════════════════════════════════
// Program Structure Explorer — Developer-friendly COBOL program viewer
// Replaces raw AST dump with: collapsed sections → paragraphs → drill-down
// Split view: structure graph (left) + source code (right)
// ═══════════════════════════════════════════════════════════════════════

class ASTExplorer {
  constructor(containerId) {
    this.containerId = containerId;
    this.network = null;
    this.currentFile = null;
    this.viewMode = 'structure'; // 'structure' | 'ast' | 'cfg'
    this.structureData = null;
    this.expandedSections = new Set();
    this.selectedNodeId = null;

    const fileSelect = document.getElementById('ast-file-select');
    if (fileSelect) {
      fileSelect.addEventListener('change', (e) => {
        this.currentFile = e.target.value;
        if (this.currentFile) this.loadView(this.currentFile);
      });
    }

    const modeSelect = document.getElementById('ast-view-mode');
    if (modeSelect) {
      modeSelect.addEventListener('change', (e) => {
        this.viewMode = e.target.value;
        if (this.currentFile) this.loadView(this.currentFile);
      });
    }
  }

  async loadFileList() {
    const select = document.getElementById('ast-file-select');
    if (!select) return;
    try {
      let files = [];
      const scanParam = typeof _currentScanRunId !== 'undefined' && _currentScanRunId &&
        _currentScanRunId !== 'all' && _currentScanRunId !== 'latest'
        ? `?scanRunId=${_currentScanRunId}` : '';
      const rektResp = await fetch('/api/graph/rekt/files' + scanParam);
      if (rektResp.ok) files = await rektResp.json();

      const current = select.value;
      select.querySelectorAll('option:not(:first-child)').forEach(o => o.remove());
      for (const f of files) {
        const opt = document.createElement('option');
        opt.value = f.name;
        opt.textContent = f.name.replace('flow-ast-', '');
        select.appendChild(opt);
      }
      if (current) select.value = current;
    } catch (e) { console.error('Explorer: file list error', e); }
  }

  async loadView(fileName) {
    if (this.viewMode === 'structure') {
      await this.loadStructure(fileName);
    } else {
      await this.loadRawGraph(fileName);
    }
  }

  // ═══════════════════════════════════════════════════════════════════
  // STRUCTURE VIEW — Collapsed section/paragraph tree with stats
  // ═══════════════════════════════════════════════════════════════════

  async loadStructure(fileName) {
    const graphEl = document.getElementById('ast-graph');
    const sourceEl = document.getElementById('ast-source-panel');
    if (!graphEl) return;

    graphEl.innerHTML = '<div class="ast-loading">Loading program structure...</div>';
    if (sourceEl) sourceEl.innerHTML = '<div class="source-placeholder">Select a section or paragraph to view source</div>';

    try {
      const scanParam = typeof _currentScanRunId !== 'undefined' && _currentScanRunId &&
        _currentScanRunId !== 'all' && _currentScanRunId !== 'latest'
        ? `&scanRunId=${_currentScanRunId}` : '';
      const resp = await fetch(`/api/graph/rekt/structure?file=${encodeURIComponent(fileName)}${scanParam}`);
      if (!resp.ok) {
        graphEl.innerHTML = `<div class="ast-empty">No structure data for ${this._escHtml(fileName.replace('flow-ast-',''))}.<br><code>./doctor.sh rekt-full</code></div>`;
        return;
      }
      this.structureData = await resp.json();
      this.renderStructureTree(graphEl);
      this.updateStructureStats();
      // Auto-load full source
      this.loadFullSource(fileName.replace('flow-ast-', ''));
    } catch (e) {
      console.error('Structure load error:', e);
      graphEl.innerHTML = `<div class="ast-error">Error: ${e.message}</div>`;
    }
  }

  renderStructureTree(container) {
    const data = this.structureData;
    if (!data?.sections?.length) {
      container.innerHTML = '<div class="ast-empty">No sections found.</div>';
      return;
    }

    // Group paragraphs by section
    const sectionMap = new Map();
    for (const row of data.sections) {
      const key = row.sectionId || row.sectionName;
      if (!sectionMap.has(key)) {
        sectionMap.set(key, {
          id: row.sectionId, name: row.sectionName || 'UNNAMED',
          type: row.sectionType, startLine: row.secStart, endLine: row.secEnd,
          paragraphs: [], totalStmts: 0, totalSql: 0,
          totalPerforms: 0, totalMoves: 0, totalBranches: 0, totalCalls: 0,
        });
      }
      const sec = sectionMap.get(key);
      if (row.paraId) {
        sec.paragraphs.push({
          id: row.paraId, name: row.paraName || 'UNNAMED', type: row.paraType,
          startLine: row.paraStart, endLine: row.paraEnd,
          stmtCount: row.stmtCount || 0, sqlCount: row.sqlCount || 0,
          performCount: row.performCount || 0, moveCount: row.moveCount || 0,
          branchCount: row.branchCount || 0, callCount: row.callCount || 0,
        });
        sec.totalStmts += row.stmtCount || 0;
        sec.totalSql += row.sqlCount || 0;
        sec.totalPerforms += row.performCount || 0;
        sec.totalMoves += row.moveCount || 0;
        sec.totalBranches += row.branchCount || 0;
        sec.totalCalls += row.callCount || 0;
      }
    }

    let html = '<div class="structure-tree">';

    // Program overview card for architects
    const totalSections = sectionMap.size;
    const totalParas = [...sectionMap.values()].reduce((s, sec) => s + sec.paragraphs.length, 0);
    const totalStmts = [...sectionMap.values()].reduce((s, sec) => s + sec.totalStmts, 0);
    const totalSql = [...sectionMap.values()].reduce((s, sec) => s + sec.totalSql, 0);
    const totalCalls = [...sectionMap.values()].reduce((s, sec) => s + sec.totalCalls, 0);
    const progName = (this.currentFile || '').replace('flow-ast-', '').replace('.cbl', '');

    html += `<div class="structure-overview">
      <div class="overview-title">${this._escHtml(progName)}</div>
      <div class="overview-grid">
        <div class="overview-stat"><span class="ov-num">${totalSections}</span><span class="ov-label">Sections</span></div>
        <div class="overview-stat"><span class="ov-num">${totalParas}</span><span class="ov-label">Paragraphs</span></div>
        <div class="overview-stat"><span class="ov-num">${totalStmts}</span><span class="ov-label">Statements</span></div>
        ${totalSql > 0 ? `<div class="overview-stat sql"><span class="ov-num">${totalSql}</span><span class="ov-label">SQL Queries</span></div>` : ''}
        ${totalCalls > 0 ? `<div class="overview-stat call"><span class="ov-num">${totalCalls}</span><span class="ov-label">CALLs</span></div>` : ''}
      </div>
    </div>`;

    html += '<div class="structure-hint">Click a section to expand — click a paragraph to view source code</div>';

    for (const [key, sec] of sectionMap) {
      const isExpanded = this.expandedSections.has(key);
      const paraCount = sec.paragraphs.length;
      const dominant = this._dominantType(sec);
      // Describe what this section does
      const purpose = sec.totalSql > 3 ? 'Database operations' : sec.totalCalls > 0 ? 'External calls' : sec.totalPerforms > sec.totalMoves ? 'Orchestration' : 'Data processing';

      html += `
        <div class="structure-section ${isExpanded ? 'expanded' : ''}" data-section-id="${key}">
          <div class="section-header" onclick="astExplorer.toggleSection('${this._escAttr(key)}')">
            <span class="section-chevron">${isExpanded ? '▼' : '▶'}</span>
            <span class="section-icon ${dominant.cssClass}">§</span>
            <span class="section-name">${this._escHtml(sec.name)}</span>
            <span class="section-purpose">${purpose}</span>
            <span class="section-badge">${paraCount} para</span>
            <span class="section-stats">
              ${sec.totalStmts > 0 ? `<span class="stat-pill">${sec.totalStmts} stmts</span>` : ''}
              ${sec.totalSql > 0 ? `<span class="stat-pill sql">${sec.totalSql} SQL</span>` : ''}
              ${sec.totalPerforms > 0 ? `<span class="stat-pill perform">${sec.totalPerforms} PERFORM</span>` : ''}
              ${sec.totalCalls > 0 ? `<span class="stat-pill call">${sec.totalCalls} CALL</span>` : ''}
            </span>
            ${sec.startLine > 0 ? `<span class="section-line">L${sec.startLine}</span>` : ''}
          </div>
          ${isExpanded ? this._renderParagraphs(sec) : ''}
        </div>`;
    }
    html += '</div>';

    // Perform edges summary
    if (data.performEdges?.length) {
      html += `<div class="perform-edges-summary">
        <h4>PERFORM Call Graph (${data.performEdges.length} calls)</h4>
        <div class="perform-edges-list">`;
      for (const edge of data.performEdges) {
        html += `<div class="perform-edge">
          <span class="pe-from">${this._escHtml(edge.from)}</span>
          <span class="pe-arrow">→</span>
          <span class="pe-to">${this._escHtml(edge.to)}</span>
        </div>`;
      }
      html += '</div></div>';
    }

    container.innerHTML = html;
  }

  _renderParagraphs(section) {
    if (!section.paragraphs.length) return '<div class="no-paragraphs">No paragraphs</div>';
    let html = '<div class="paragraph-list">';
    for (const para of section.paragraphs) {
      const dominant = this._paraDominant(para);
      html += `
        <div class="paragraph-item ${this.selectedNodeId === para.id ? 'selected' : ''}"
             onclick="astExplorer.selectParagraph('${this._escAttr(para.id)}', '${this._escAttr(para.name)}', ${para.startLine}, ${para.endLine})"
             data-para-id="${para.id}">
          <div class="para-header">
            <span class="para-icon ${dominant.cssClass}">${dominant.icon}</span>
            <span class="para-name">${this._escHtml(para.name)}</span>
            ${para.startLine > 0 ? `<span class="para-line">L${para.startLine}</span>` : ''}
          </div>
          <div class="para-stats">
            ${para.stmtCount > 0 ? `<span class="stat-mini">${para.stmtCount} stmts</span>` : ''}
            ${para.sqlCount > 0 ? `<span class="stat-mini sql">${para.sqlCount} SQL</span>` : ''}
            ${para.performCount > 0 ? `<span class="stat-mini perform">${para.performCount} PERFORM</span>` : ''}
            ${para.moveCount > 0 ? `<span class="stat-mini move">${para.moveCount} MOVE</span>` : ''}
            ${para.branchCount > 0 ? `<span class="stat-mini branch">${para.branchCount} IF/EVAL</span>` : ''}
            ${para.callCount > 0 ? `<span class="stat-mini call">${para.callCount} CALL</span>` : ''}
          </div>
        </div>`;
    }
    html += '</div>';
    return html;
  }

  toggleSection(sectionId) {
    if (this.expandedSections.has(sectionId)) this.expandedSections.delete(sectionId);
    else this.expandedSections.add(sectionId);
    const graphEl = document.getElementById('ast-graph');
    if (graphEl) this.renderStructureTree(graphEl);
  }

  async selectParagraph(paraId, paraName, startLine, endLine) {
    this.selectedNodeId = paraId;
    document.querySelectorAll('.paragraph-item').forEach(el => el.classList.remove('selected'));
    const el = document.querySelector(`[data-para-id="${paraId}"]`);
    if (el) el.classList.add('selected');

    await this.loadSourceForRange(startLine, endLine, paraName);
    await this.loadParagraphDetail(paraId, paraName);
  }

  async loadSourceForRange(startLine, endLine, label) {
    const sourceEl = document.getElementById('ast-source-panel');
    if (!sourceEl) return;

    if (!startLine || startLine <= 0) {
      // No line info — load full file and try to find by name
      await this.loadFullSource(label, label);
      return;
    }

    try {
      const baseName = (this.currentFile || '').replace('flow-ast-', '');
      const padding = 10;
      const resp = await fetch(`/api/source/content?file=${encodeURIComponent(baseName)}&startLine=${Math.max(1, startLine - padding)}&endLine=${endLine + padding}`);
      if (!resp.ok) { await this.loadFullSource(label, label); return; }
      const data = await resp.json();
      this._renderSource(data, startLine, endLine, label);
    } catch (e) {
      sourceEl.innerHTML = `<div class="source-error">Error loading source: ${e.message}</div>`;
    }
  }

  async loadFullSource(label, searchTerm) {
    const sourceEl = document.getElementById('ast-source-panel');
    if (!sourceEl) return;
    try {
      const baseName = (this.currentFile || '').replace('flow-ast-', '');
      const resp = await fetch(`/api/source/content?file=${encodeURIComponent(baseName)}`);
      if (!resp.ok) {
        sourceEl.innerHTML = `<div class="source-placeholder"><span class="source-icon">📁</span><span>Source file not found for <strong>${this._escHtml(baseName)}</strong></span><span class="source-hint">Place file in source/ directory</span></div>`;
        return;
      }
      const data = await resp.json();

      // If a search term is provided and no line highlights, find the term in source
      let foundLine = -1;
      if (searchTerm && data.lines) {
        const term = searchTerm.toUpperCase().replace(/[^A-Z0-9-]/g, '');
        for (const l of data.lines) {
          if (l.text && l.text.toUpperCase().includes(term)) {
            foundLine = l.lineNumber;
            break;
          }
        }
      }

      this._renderSource(data, foundLine, foundLine, label || baseName);
    } catch (e) {
      sourceEl.innerHTML = `<div class="source-error">Error: ${e.message}</div>`;
    }
  }

  _renderSource(data, highlightStart, highlightEnd, label) {
    const sourceEl = document.getElementById('ast-source-panel');
    if (!sourceEl) return;

    const lines = data.lines || [];
    const linesHtml = lines.map(l => {
      const isHl = highlightStart > 0 && l.lineNumber >= highlightStart && l.lineNumber <= highlightEnd;
      return `<div class="source-line${isHl ? ' highlight' : ''}" data-line="${l.lineNumber}">` +
        `<span class="line-num">${l.lineNumber}</span>` +
        `<span class="line-text">${this._escHtml(l.text)}</span></div>`;
    }).join('');

    sourceEl.innerHTML = `
      <div class="source-header">
        <span class="source-file-name">📄 ${this._escHtml(data.file)} <span class="source-total">(${data.totalLines} lines)</span></span>
        ${label ? `<span class="source-label">${this._escHtml(label)}</span>` : ''}
        <button class="source-fullfile-btn" onclick="astExplorer.loadFullSource('${this._escAttr(data.file)}', null)">Full File</button>
      </div>
      <div class="source-code" id="source-code-scroll"><pre>${linesHtml}</pre></div>`;

    if (highlightStart > 0) {
      requestAnimationFrame(() => {
        const hl = sourceEl.querySelector('.source-line.highlight');
        if (hl) hl.scrollIntoView({ behavior: 'smooth', block: 'center' });
      });
    }
  }

  async loadParagraphDetail(paraId, paraName) {
    const inspectorEl = document.getElementById('ast-inspector-content');
    if (!inspectorEl) return;

    try {
      const scanParam = typeof _currentScanRunId !== 'undefined' && _currentScanRunId &&
        _currentScanRunId !== 'all' && _currentScanRunId !== 'latest'
        ? `&scanRunId=${_currentScanRunId}` : '';
      const resp = await fetch(`/api/graph/rekt/ast?file=${encodeURIComponent(this.currentFile)}${scanParam}`);
      if (!resp.ok) return;
      const astData = await resp.json();

      // Build parent→children map from edges
      const edgeMap = new Map();
      for (const e of astData.edges || []) {
        const from = e.from || e.source;
        const to = e.to || e.target;
        if (!edgeMap.has(from)) edgeMap.set(from, []);
        edgeMap.get(from).push(to);
      }

      // BFS from paraId
      const childIds = new Set();
      const queue = [paraId];
      while (queue.length > 0) {
        const id = queue.shift();
        if (childIds.has(id)) continue;
        childIds.add(id);
        for (const child of (edgeMap.get(id) || [])) queue.push(child);
      }

      const nodeMap = new Map(astData.nodes.map(n => [n.id, n]));
      const skipTypes = new Set(['SENTENCE', 'PARAGRAPH', 'PARAGRAPH_NAME', 'PARAGRAPHS', 'SECTION', 'SECTION_HEADER', 'PROCEDURE_DIVISION_BODY']);
      const statements = [];
      for (const id of childIds) {
        const n = nodeMap.get(id);
        if (n && !skipTypes.has(n.nodeType)) statements.push(n);
      }

      // Type breakdown
      const stmtTypes = {};
      for (const s of statements) {
        const t = this._humanType(s.nodeType);
        stmtTypes[t] = (stmtTypes[t] || 0) + 1;
      }

      let html = `<div class="inspector-header">${this._escHtml(paraName)}</div>`;
      html += `<div class="inspector-summary">${statements.length} statements</div>`;
      html += '<div class="type-breakdown">';
      const total = statements.length || 1;
      for (const [type, count] of Object.entries(stmtTypes).sort((a, b) => b[1] - a[1])) {
        const pct = Math.round((count / total) * 100);
        const color = this._typeColor(type);
        html += `<div class="type-bar-row"><span class="type-label">${type}</span><div class="type-bar"><div class="type-bar-fill" style="width:${pct}%;background:${color}"></div></div><span class="type-count">${count}</span></div>`;
      }
      html += '</div>';

      // Statement list
      html += '<div class="stmt-list">';
      for (const s of statements.slice(0, 40)) {
        const label = this._stmtLabel(s);
        const color = this._typeColor(this._humanType(s.nodeType));
        html += `<div class="stmt-item" onclick="astExplorer.loadSourceForRange(${s.startLine}, ${s.endLine}, '${this._escAttr(label)}')" style="border-left: 3px solid ${color}; cursor:pointer;">
          <span class="stmt-type">${this._humanType(s.nodeType)}</span>
          <span class="stmt-label">${this._escHtml(label)}</span></div>`;
      }
      if (statements.length > 40) html += `<div class="stmt-more">+ ${statements.length - 40} more</div>`;
      html += '</div>';

      inspectorEl.innerHTML = html;
    } catch (e) {
      inspectorEl.innerHTML = `<div class="inspector-error">Error: ${e.message}</div>`;
    }
  }

  updateStructureStats() {
    const statsEl = document.getElementById('ast-stats-bar');
    if (!statsEl || !this.structureData) return;
    const data = this.structureData;
    const sections = new Set(data.sections.map(r => r.sectionId)).size;
    const paragraphs = new Set(data.sections.filter(r => r.paraId).map(r => r.paraId)).size;
    const totalStmts = data.sections.reduce((sum, r) => sum + (r.stmtCount || 0), 0);
    const totalSql = data.sections.reduce((sum, r) => sum + (r.sqlCount || 0), 0);
    const performs = data.performEdges?.length || 0;

    statsEl.innerHTML = `
      <span class="ast-stat">${sections} sections</span>
      <span class="ast-stat">${paragraphs} paragraphs</span>
      <span class="ast-stat">${totalStmts} statements</span>
      ${totalSql > 0 ? `<span class="ast-stat sql">${totalSql} SQL</span>` : ''}
      ${performs > 0 ? `<span class="ast-stat perform">${performs} PERFORM calls</span>` : ''}`;
  }

  // ═══════════════════════════════════════════════════════════════════
  // RAW AST/CFG VIEW — vis-network graph (power users)
  // ═══════════════════════════════════════════════════════════════════

  async loadRawGraph(fileName) {
    const container = document.getElementById('ast-graph');
    if (!container) return;
    container.innerHTML = '<div class="ast-loading">Loading AST...</div>';
    const sourceEl = document.getElementById('ast-source-panel');
    if (sourceEl) sourceEl.innerHTML = '<div class="source-placeholder">Click a node to view its source code</div>';

    // Whitelist of valid endpoint names — guards against unknown viewModes that
    // would otherwise hit the SPA fallback (HTML 200) and break JSON.parse.
    const validModes = new Set(['ast', 'cfg', 'structure']);
    const mode = validModes.has(this.viewMode) ? this.viewMode : 'ast';

    try {
      const scanParam = typeof _currentScanRunId !== 'undefined' && _currentScanRunId &&
        _currentScanRunId !== 'all' && _currentScanRunId !== 'latest'
        ? `&scanRunId=${_currentScanRunId}` : '';
      const endpoint = `/api/graph/rekt/${mode}?file=${encodeURIComponent(fileName)}${scanParam}`;
      const resp = await fetch(endpoint);
      if (!resp.ok) { container.innerHTML = `<div class="ast-empty">No ${mode.toUpperCase()} data for ${this._escHtml(fileName)}</div>`; return; }
      const ct = resp.headers.get('content-type') || '';
      if (!ct.includes('application/json')) {
        container.innerHTML = `<div class="ast-empty">Endpoint ${endpoint} returned non-JSON (${ct || 'unknown'}).</div>`;
        return;
      }
      const graphData = await resp.json();
      this.renderVisNetwork(graphData, container);
    } catch (e) { container.innerHTML = `<div class="ast-error">Error: ${e.message}</div>`; }
  }

  renderVisNetwork(graphData, container) {
    if (this.network) { this.network.destroy(); this.network = null; }
    container.innerHTML = '';
    if (!graphData.nodes?.length) { container.innerHTML = '<div class="ast-empty">No nodes found.</div>'; return; }

    const typeColors = {
      SECTION: '#8b5cf6', PARAGRAPH: '#10b981', PARAGRAPH_NAME: '#10b981',
      SENTENCE: '#64748b', DIALECT: '#a855f7', DIALECT_CONTAINER: '#a855f7',
      MOVE: '#84cc16', PERFORM: '#06b6d4', IF_BRANCH: '#ec4899', EVALUATE: '#f59e0b',
      EXIT: '#475569', COMPUTE: '#f97316', DISPLAY: '#14b8a6', CALL: '#ef4444',
      PROCEDURE_DIVISION_BODY: '#3b82f6', PARAGRAPHS: '#334155',
    };

    const nodes = new vis.DataSet(graphData.nodes.map(n => {
      // Human-readable label: show name (cleaned) instead of UUID
      const cleanName = (n.name || '').replace(/.*\//, '').replace(/Context\/.*/, '');
      const typeLabel = this._humanType(n.nodeType);
      const displayLabel = cleanName ? `${typeLabel}\n${cleanName}` : typeLabel;

      return {
        id: n.id,
        label: displayLabel,
        title: `Type: ${n.nodeType}\nName: ${n.name || '—'}\nLines: ${n.startLine}–${n.endLine}\nID: ${n.id}`,
        color: { background: typeColors[n.nodeType] || '#64748b', border: '#1e293b', highlight: { background: '#fbbf24', border: '#f59e0b' } },
        font: { color: '#e2e8f0', size: 10, multi: true },
        shape: ['SECTION', 'PARAGRAPH'].includes(n.nodeType) ? 'box' : 'dot',
        size: ['SECTION', 'PARAGRAPH'].includes(n.nodeType) ? 14 : 6,
        _data: n,
      };
    }));

    const edges = new vis.DataSet((graphData.edges || []).map((e, i) => ({
      id: i, from: e.from || e.source, to: e.to || e.target, arrows: 'to',
      color: { color: e.type === 'CONTAINS' ? '#334155' : e.type === 'FOLLOWED_BY' ? '#3b82f6' : '#ef4444', opacity: 0.6 },
      width: e.type === 'CONTAINS' ? 1 : 2, dashes: e.type === 'JUMPS_TO' ? [5, 5] : false,
    })));

    this.network = new vis.Network(container, { nodes, edges }, {
      nodes: { borderWidth: 1, shadow: false },
      edges: { smooth: { type: 'cubicBezier', roundness: 0.4 } },
      layout: { hierarchical: { enabled: true, direction: 'UD', sortMethod: 'directed', nodeSpacing: 80, levelSeparation: 60 } },
      physics: { enabled: false },
      interaction: { hover: true, tooltipDelay: 100, navigationButtons: true, keyboard: true },
    });

    this.network.on('click', (params) => {
      if (params.nodes.length > 0) {
        const nd = nodes.get(params.nodes[0])?._data;
        if (nd) {
          this.loadSourceForRange(nd.startLine, nd.endLine, nd.name || nd.originalText || nd.label);
          this._updateRawInspector(nd);
        }
      }
    });
  }

  _updateRawInspector(nodeData) {
    const content = document.getElementById('ast-inspector-content');
    if (!content || !nodeData) return;

    const cleanName = (nodeData.name || '').replace(/.*\//, '').replace(/Context\/.*/, '') || '—';
    const humanType = this._humanType(nodeData.nodeType);

    let html = `<div class="inspector-header" style="color:${this._typeColor(humanType)}">${humanType}</div>`;
    html += `<div style="color:#e2e8f0; font-weight:600; margin-bottom:8px;">${this._escHtml(cleanName)}</div>`;

    html += '<table class="inspector-table">';
    html += `<tr><td class="inspector-key">Type</td><td class="inspector-val">${nodeData.nodeType}</td></tr>`;
    if (nodeData.startLine > 0) html += `<tr><td class="inspector-key">Lines</td><td class="inspector-val">${nodeData.startLine}–${nodeData.endLine}</td></tr>`;
    if (nodeData.section) html += `<tr><td class="inspector-key">Section</td><td class="inspector-val">${nodeData.section}</td></tr>`;
    if (nodeData.paragraph) html += `<tr><td class="inspector-key">Paragraph</td><td class="inspector-val">${nodeData.paragraph}</td></tr>`;
    html += `<tr><td class="inspector-key">ID</td><td class="inspector-val" style="font-size:10px;word-break:break-all;">${nodeData.id}</td></tr>`;

    if (nodeData.originalText) {
      const text = nodeData.originalText.length > 300 ? nodeData.originalText.substring(0, 300) + '...' : nodeData.originalText;
      html += `<tr><td colspan="2"><pre style="font-size:10px;color:#94a3b8;white-space:pre-wrap;margin-top:6px;max-height:150px;overflow-y:auto;">${this._escHtml(text)}</pre></td></tr>`;
    }
    html += '</table>';

    content.innerHTML = html;
  }

  // ═══════════════════════════════════════════════════════════════════
  // DRILL-THROUGH — Called from dependency graph (Sigma.js / vis-network)
  // ═══════════════════════════════════════════════════════════════════

  drillIntoProgram(fileName) {
    if (typeof switchDashboard === 'function') switchDashboard('ast');
    const select = document.getElementById('ast-file-select');
    if (select) {
      for (const opt of select.options) {
        if (opt.value === fileName || opt.value.includes(fileName.replace('.cbl', '').replace('.cpy', ''))) {
          select.value = opt.value; break;
        }
      }
    }
    this.currentFile = select?.value || fileName;
    // 'raw' is not a real endpoint — the dropdown only has 'structure' and 'ast'.
    // Using 'raw' caused /api/graph/rekt/raw to fall through to the SPA index.html
    // (HTTP 200, text/html), which then crashed JSON.parse with "Unexpected token '<'".
    this.viewMode = 'ast';
    const modeSelect = document.getElementById('ast-view-mode');
    if (modeSelect) modeSelect.value = 'ast';
    this.loadView(this.currentFile);
  }

  // ═══════════════════════════════════════════════════════════════════
  // HELPERS
  // ═══════════════════════════════════════════════════════════════════

  _humanType(nodeType) {
    const map = { DIALECT: 'SQL', DIALECT_CONTAINER: 'SQL', MOVE: 'MOVE', PERFORM: 'PERFORM', CALL: 'CALL',
      IF_BRANCH: 'IF', IF_YES: 'THEN', IF_NO: 'ELSE', EVALUATE: 'EVALUATE', COMPUTE: 'COMPUTE', ADD: 'ADD',
      DISPLAY: 'DISPLAY', EXIT: 'EXIT', GENERIC_STATEMENT: 'STMT', SECTION: 'SECTION', PARAGRAPH: 'PARA',
      SECTION_HEADER: 'SECTION', PARAGRAPH_NAME: 'PARA', PARAGRAPHS: 'PARAGRAPHS',
      PROCEDURE_DIVISION_BODY: 'PROCEDURE DIV', SENTENCE: 'SENTENCE', SYMBOL: 'SYMBOL' };
    return map[nodeType] || nodeType;
  }

  _typeColor(humanType) {
    const colors = { SQL: '#a855f7', MOVE: '#84cc16', PERFORM: '#06b6d4', CALL: '#ef4444',
      IF: '#ec4899', THEN: '#ec4899', ELSE: '#94a3b8', EVALUATE: '#f59e0b', COMPUTE: '#f97316',
      ADD: '#eab308', DISPLAY: '#14b8a6', EXIT: '#475569', STMT: '#64748b', SECTION: '#8b5cf6', PARA: '#10b981' };
    return colors[humanType] || '#64748b';
  }

  _dominantType(section) {
    if (section.totalSql > section.totalMoves) return { cssClass: 'dominant-sql', icon: '🗃️' };
    if (section.totalCalls > 0) return { cssClass: 'dominant-call', icon: '📞' };
    if (section.totalPerforms > section.totalMoves) return { cssClass: 'dominant-perform', icon: '🔄' };
    return { cssClass: 'dominant-move', icon: '📋' };
  }

  _paraDominant(para) {
    if (para.sqlCount > para.moveCount) return { cssClass: 'dominant-sql', icon: '🗃️' };
    if (para.callCount > 0) return { cssClass: 'dominant-call', icon: '📞' };
    if (para.branchCount > 0) return { cssClass: 'dominant-branch', icon: '🔀' };
    if (para.performCount > para.moveCount) return { cssClass: 'dominant-perform', icon: '🔄' };
    return { cssClass: 'dominant-move', icon: '📋' };
  }

  _stmtLabel(node) {
    const text = node.originalText || node.name || node.label || '';
    return text.length > 60 ? text.substring(0, 57) + '...' : text || node.nodeType;
  }

  _escHtml(s) { return (s || '').replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;'); }
  _escAttr(s) { return (s || '').replace(/'/g, "\\'").replace(/"/g, '&quot;'); }

  refresh() {
    if (this.currentFile) this.loadView(this.currentFile);
    else this.loadFileList();
  }
}
