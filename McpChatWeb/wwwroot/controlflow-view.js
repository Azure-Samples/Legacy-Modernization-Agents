// Control Flow View — Program-level expandable flow diagram
// Shows section→paragraph flow with FOLLOWED_BY/JUMPS_TO edges
// Expandable blocks reveal statement details
// Drill-down buttons to AST Explorer for deep inspection

class ControlFlowView {
  constructor(containerId) {
    this.containerId = containerId;
    this.network = null;
    this.currentFile = null;

    const fileSelect = document.getElementById('cf-file-select');
    if (fileSelect) {
      fileSelect.addEventListener('change', (e) => {
        this.currentFile = e.target.value;
        if (this.currentFile) this.loadCFG(this.currentFile);
      });
    }
  }

  async loadFileList() {
    const select = document.getElementById('cf-file-select');
    if (!select) return;
    try {
      const scanParam = typeof _currentScanRunId !== 'undefined' && _currentScanRunId &&
        _currentScanRunId !== 'all' && _currentScanRunId !== 'latest'
        ? `?scanRunId=${_currentScanRunId}` : '';
      const resp = await fetch('/api/graph/rekt/files' + scanParam);
      if (!resp.ok) return;
      const files = await resp.json();
      const current = select.value;
      select.querySelectorAll('option:not(:first-child)').forEach(o => o.remove());
      // Only show files that have CFG data
      for (const f of files) {
        if (!f.hasCfg) continue;
        const opt = document.createElement('option');
        opt.value = f.name;
        opt.textContent = f.name.replace('flow-ast-', '');
        select.appendChild(opt);
      }
      if (current) select.value = current;
    } catch (e) { console.error('CF: file list error', e); }
  }

  selectFile(fileName) {
    const select = document.getElementById('cf-file-select');
    if (select) {
      // Try to find matching option
      for (const opt of select.options) {
        if (opt.value === fileName || opt.value.includes(fileName.replace('.cbl','').replace('.cpy',''))) {
          select.value = opt.value;
          this.currentFile = opt.value;
          this.loadCFG(opt.value);
          return;
        }
      }
    }
    // Fallback: try with flow-ast- prefix
    this.currentFile = fileName;
    this.loadCFG(fileName);
  }

  async loadCFG(fileName) {
    const container = document.getElementById(this.containerId);
    if (!container) return;
    container.innerHTML = '<div style="display:flex;align-items:center;justify-content:center;height:100%;color:#94a3b8;">Loading control flow...</div>';

    const detailPanel = document.getElementById('cf-detail-content');
    if (detailPanel) detailPanel.innerHTML = 'Select a block to see details';

    try {
      const scanParam = typeof _currentScanRunId !== 'undefined' && _currentScanRunId &&
        _currentScanRunId !== 'all' && _currentScanRunId !== 'latest'
        ? `&scanRunId=${_currentScanRunId}` : '';
      const resp = await fetch(`/api/graph/rekt/cfg?file=${encodeURIComponent(fileName)}${scanParam}`);
      if (!resp.ok) {
        container.innerHTML = `<div style="padding:20px;color:#f87171;">No CFG data for ${fileName.replace('flow-ast-','')}. Run: ./doctor.sh rekt-full</div>`;
        return;
      }
      const data = await resp.json();
      this._renderGraph(data, container);

      // Update stats
      const statsEl = document.getElementById('cf-stats');
      if (statsEl && data.stats) {
        statsEl.textContent = `${data.stats.totalNodes} blocks, ${data.stats.followedBy} flow edges, ${data.stats.jumpsTo} jumps`;
      }
    } catch (e) {
      container.innerHTML = `<div style="padding:20px;color:#f87171;">Error: ${e.message}</div>`;
    }
  }

  _renderGraph(data, container) {
    if (this.network) { this.network.destroy(); this.network = null; }
    container.innerHTML = '';

    if (!data.nodes?.length) {
      container.innerHTML = '<div style="padding:20px;color:#94a3b8;">No flow blocks found.</div>';
      return;
    }

    const typeColors = {
      SECTION: '#8b5cf6', PARAGRAPH: '#10b981', PARAGRAPHS: '#334155',
      PROCEDURE_DIVISION_BODY: '#3b82f6', SENTENCE: '#475569',
      DIALECT: '#a855f7', DIALECT_CONTAINER: '#a855f7',
      MOVE: '#84cc16', PERFORM: '#06b6d4', IF_BRANCH: '#ec4899',
      EVALUATE: '#f59e0b', CALL: '#ef4444', EXIT: '#475569',
      DISPLAY: '#14b8a6', COMPUTE: '#f97316',
    };

    const humanType = (t) => {
      const map = { DIALECT:'SQL', DIALECT_CONTAINER:'SQL', PROCEDURE_DIVISION_BODY:'ENTRY',
        PARAGRAPHS:'GROUP', SECTION_HEADER:'SECTION', PARAGRAPH_NAME:'PARA',
        GENERIC_STATEMENT:'STMT', IF_BRANCH:'IF', IF_YES:'THEN', IF_NO:'ELSE' };
      return map[t] || t;
    };

    // Build vis-network with program-name labels instead of UUIDs
    const nodes = new vis.DataSet(data.nodes
      .filter(n => n.isFlowNode || ['SECTION','PARAGRAPH','PARAGRAPHS','PROCEDURE_DIVISION_BODY'].includes(n.nodeType))
      .map(n => {
        const label = n.name
          ? n.name.replace(/.*\//, '').replace(/ProcedureDivisionBodyContext\/.*/, 'ENTRY')
          : humanType(n.nodeType);
        return {
          id: n.id,
          label: label,
          title: `${humanType(n.nodeType)}\n${n.name || ''}\nL${n.startLine}-${n.endLine}`,
          color: {
            background: typeColors[n.nodeType] || '#64748b',
            border: '#1e293b',
            highlight: { background: '#fbbf24', border: '#f59e0b' }
          },
          font: { color: '#e2e8f0', size: ['SECTION','PARAGRAPH'].includes(n.nodeType) ? 12 : 9 },
          shape: ['SECTION','PARAGRAPH'].includes(n.nodeType) ? 'box' : 'dot',
          size: ['SECTION','PARAGRAPH'].includes(n.nodeType) ? 16 : 8,
          _data: n,
        };
      }));

    const validNodeIds = new Set(nodes.getIds());

    const edges = new vis.DataSet((data.edges || [])
      .filter(e => e.type !== 'CONTAINS' && validNodeIds.has(e.source) && validNodeIds.has(e.target))
      .map((e, i) => ({
        id: i,
        from: e.source,
        to: e.target,
        arrows: 'to',
        color: {
          color: e.type === 'FOLLOWED_BY' ? '#3b82f6' : e.type === 'JUMPS_TO' ? '#ef4444' : '#334155',
          opacity: 0.7,
        },
        width: e.type === 'JUMPS_TO' ? 2.5 : 1.5,
        dashes: e.type === 'JUMPS_TO' ? [5, 5] : false,
        _type: e.type,
      })));

    this.network = new vis.Network(container, { nodes, edges }, {
      nodes: { borderWidth: 1, shadow: false },
      edges: { smooth: { type: 'cubicBezier', roundness: 0.4 } },
      layout: { hierarchical: { enabled: true, direction: 'UD', sortMethod: 'directed', nodeSpacing: 100, levelSeparation: 80 } },
      physics: { enabled: false },
      interaction: { hover: true, tooltipDelay: 100, navigationButtons: true, keyboard: true },
    });

    this.network.on('click', (params) => {
      if (params.nodes.length > 0) {
        const nd = nodes.get(params.nodes[0])?._data;
        if (nd) this._showBlockDetail(nd);
      }
    });

    this.network.on('doubleClick', (params) => {
      if (params.nodes.length > 0) {
        const nd = nodes.get(params.nodes[0])?._data;
        if (nd && typeof astExplorer !== 'undefined' && astExplorer) {
          astExplorer.drillIntoProgram(this.currentFile || '');
        }
      }
    });
  }

  _showBlockDetail(blockData) {
    const panel = document.getElementById('cf-detail-content');
    if (!panel) return;

    const humanType = (t) => {
      const map = { DIALECT:'SQL (Embedded)', DIALECT_CONTAINER:'SQL Block', PROCEDURE_DIVISION_BODY:'Entry Point',
        PARAGRAPHS:'Paragraph Group', GENERIC_STATEMENT:'Statement', IF_BRANCH:'IF Branch',
        SECTION:'Section', PARAGRAPH:'Paragraph', PERFORM:'PERFORM Call',
        MOVE:'Data Move', DISPLAY:'Screen Output', CALL:'Program Call',
        EXIT:'Exit Point', EVALUATE:'EVALUATE (Switch)', COMPUTE:'Computation' };
      return map[t] || t;
    };

    const typeColors = {
      SECTION:'#8b5cf6', PARAGRAPH:'#10b981', PROCEDURE_DIVISION_BODY:'#3b82f6',
      DIALECT:'#a855f7', PERFORM:'#06b6d4', CALL:'#ef4444', IF_BRANCH:'#ec4899',
      MOVE:'#84cc16', DISPLAY:'#14b8a6', EXIT:'#475569', EVALUATE:'#f59e0b'
    };

    const name = blockData.name ? blockData.name.replace(/.*\//, '').replace(/Context\/.*/, '') : humanType(blockData.nodeType);
    const typeColor = typeColors[blockData.nodeType] || '#64748b';

    let html = '<div class="cf-block-detail">';

    // Header
    html += `<div style="border-left:3px solid ${typeColor};padding-left:10px;margin-bottom:10px;">`;
    html += `<div style="font-size:15px;font-weight:700;color:#e2e8f0;">${this._escHtml(name)}</div>`;
    html += `<div style="color:${typeColor};font-size:12px;font-weight:600;">${humanType(blockData.nodeType)}</div>`;
    html += '</div>';

    // Location
    if (blockData.startLine > 0) {
      html += `<div style="background:rgba(59,130,246,0.1);border:1px solid #1e3a5f;border-radius:4px;padding:6px 8px;margin-bottom:8px;font-size:11px;">`;
      html += `<span style="color:#60a5fa;">📍 Lines ${blockData.startLine}–${blockData.endLine}</span>`;
      html += `<span style="color:#475569;margin-left:8px;">(${blockData.endLine - blockData.startLine + 1} lines)</span>`;
      html += '</div>';
    }

    // What this block does (developer-friendly description)
    html += '<div style="margin-bottom:8px;font-size:11px;color:#94a3b8;">';
    const nt = blockData.nodeType;
    if (nt === 'SECTION') html += '📋 A SECTION groups related paragraphs. It runs sequentially unless a GO TO or PERFORM transfers control elsewhere.';
    else if (nt === 'PARAGRAPH') html += '📋 A PARAGRAPH is a named block of code (like a function). Other parts of the program can PERFORM it.';
    else if (nt === 'PROCEDURE_DIVISION_BODY') html += '🚀 The entry point — execution starts here and flows through sections/paragraphs in order.';
    else if (nt === 'PERFORM') html += '🔄 PERFORM calls another paragraph/section, runs it, then returns here (like a function call).';
    else if (nt === 'CALL') html += '📞 CALL invokes an external COBOL program with parameters via USING clause.';
    else if (nt === 'DIALECT' || nt === 'DIALECT_CONTAINER') html += '🗃️ Embedded SQL or EXEC block — database operation or system service call.';
    else if (nt === 'IF_BRANCH') html += '🔀 Conditional branch — the program takes different paths based on a condition.';
    else if (nt === 'EVALUATE') html += '🔀 EVALUATE is like a switch/case statement — matches against multiple conditions.';
    else if (nt === 'MOVE') html += '📝 MOVE copies data from one variable to another (assignment).';
    else if (nt === 'EXIT') html += '🚪 EXIT marks the end of a paragraph or program — returns control to the caller.';
    else if (nt === 'DISPLAY') html += '🖥️ DISPLAY outputs text to the console or screen.';
    html += '</div>';

    // Node ID (for developers)
    html += `<div style="font-size:10px;color:#475569;word-break:break-all;margin-bottom:8px;">ID: ${blockData.id}</div>`;

    // Source text preview
    if (blockData.originalText) {
      const text = blockData.originalText.length > 300 ? blockData.originalText.substring(0, 300) + '...' : blockData.originalText;
      html += `<div style="margin-top:6px;"><div style="color:#64748b;font-size:10px;text-transform:uppercase;margin-bottom:2px;">Source Code</div>`;
      html += `<pre class="cf-block-text">${this._escHtml(text)}</pre></div>`;
    }

    // Drill buttons
    html += `<div style="margin-top: 12px; display: flex; flex-direction: column; gap: 4px;">`;
    html += `<button class="btn-small drill-btn" onclick="astExplorer?.drillIntoProgram('${this._esc(this.currentFile || '')}')">🔬 Open in AST Explorer</button>`;
    html += `</div>`;
    html += `</div>`;

    panel.innerHTML = html;
  }

  refresh() {
    this.loadFileList();
    if (this.currentFile) this.loadCFG(this.currentFile);
  }

  _esc(s) { return (s || '').replace(/'/g, "\\'"); }
  _escHtml(s) { return (s || '').replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;'); }
}
