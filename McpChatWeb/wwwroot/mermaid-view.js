// Mermaid Diagrams View — system-level and per-file flowcharts
// Click nodes for details, toggle direction (LR/TD), copy source

class MermaidView {
  constructor(containerId) {
    this.containerId = containerId;
    this.mermaidSource = '';
    this.direction = 'LR'; // LR = left-right, TD = top-down
    this.currentFile = '';
    this.currentData = null;
    this.nodeData = new Map(); // id → metadata for click details

    const fileSelect = document.getElementById('mermaid-file-select');
    if (fileSelect) {
      fileSelect.addEventListener('change', (e) => {
        this.currentFile = e.target.value;
        this.loadDiagram(e.target.value);
      });
    }
  }

  async loadFileList() {
    const select = document.getElementById('mermaid-file-select');
    if (!select) return;
    try {
      const resp = await fetch('/api/graph/rekt/files');
      if (!resp.ok) return;
      const files = await resp.json();
      select.querySelectorAll('option:not(:first-child)').forEach(o => o.remove());
      for (const f of files) {
        const opt = document.createElement('option');
        opt.value = f.name;
        opt.textContent = f.name.replace('flow-ast-', '') + (f.hasCfg ? ' (has flow)' : '');
        select.appendChild(opt);
      }
    } catch (e) { console.error('Mermaid: file list error', e); }
  }

  toggleDirection() {
    this.direction = this.direction === 'LR' ? 'TD' : 'LR';
    // Update button text
    const btn = document.getElementById('mermaid-dir-btn');
    if (btn) btn.textContent = this.direction === 'LR' ? '↔ Horizontal' : '↕ Vertical';
    // Re-render with new direction
    if (this.currentData) this._renderDiagram(this.currentData);
  }

  async loadDiagram(file) {
    const container = document.getElementById(this.containerId);
    if (!container) return;
    container.innerHTML = '<div style="display:flex;align-items:center;justify-content:center;height:200px;color:#94a3b8;">Generating diagram...</div>';

    try {
      const url = file ? `/api/graph/rekt/mermaid?file=${encodeURIComponent(file)}` : '/api/graph/rekt/mermaid';
      const resp = await fetch(url);
      if (!resp.ok) { container.innerHTML = '<div style="padding:20px;color:#f87171;">Failed to generate diagram</div>'; return; }
      this.currentData = await resp.json();
      this._renderDiagram(this.currentData);
    } catch (e) {
      container.innerHTML = `<div style="padding:20px;color:#f87171;">Error: ${e.message}</div>`;
    }
  }

  _renderDiagram(data) {
    const container = document.getElementById(this.containerId);
    if (!container) return;

    // Swap direction in Mermaid source
    let source = data.mermaid || '';
    if (!source.trim()) {
      container.innerHTML = '<div style="padding:20px;color:#94a3b8;">No diagram data. Run: ./doctor.sh rekt-full</div>';
      return;
    }

    // Replace direction directive
    source = source.replace(/^(graph|flowchart)\s+(LR|TD|TB|RL)/m, `$1 ${this.direction}`);
    this.mermaidSource = source;

    // Build node metadata map for click details
    this.nodeData.clear();
    const nodeLines = source.split('\n').filter(l => l.match(/^\s+\w+[\[(]/));
    for (const line of nodeLines) {
      const m = line.match(/^\s+(\w+)\[([^\]]+)\]/);
      if (m) {
        const [, id, label] = m;
        const isCpy = line.includes(':::cpy');
        const isSec = line.includes(':::section');
        const isPara = line.includes(':::para');
        this.nodeData.set(id, {
          id, label,
          type: isCpy ? 'Copybook' : isSec ? 'Section' : isPara ? 'Paragraph' : 'Program',
          color: isCpy ? '#f97316' : isSec ? '#8b5cf6' : isPara ? '#10b981' : '#3b82f6'
        });
      }
    }

    // Count connections per node
    const connections = {};
    const edgeLines = source.split('\n').filter(l => l.includes('-->') || l.includes('-.->'));
    for (const line of edgeLines) {
      const m = line.match(/(\w+)\s+(?:-->|-.->)/);
      const m2 = line.match(/(?:-->|-.->)[|][^|]*[|]\s*(\w+)/);
      const m3 = line.match(/(?:-->|-.->)\s*(\w+)/);
      if (m) connections[m[1]] = (connections[m[1]] || 0) + 1;
      const target = m2 ? m2[1] : m3 ? m3[1] : null;
      if (target) connections[target] = (connections[target] || 0) + 1;
    }
    for (const [id, nd] of this.nodeData) nd.connections = connections[id] || 0;

    const diagramId = 'mermaid-diagram-' + Date.now();
    const isFile = data.type === 'file';
    const title = isFile ? `📐 ${data.program || this.currentFile} — Control Flow` : '📐 System Dependency Diagram';

    container.innerHTML = `
      <div style="display:flex;gap:16px;height:100%;">
        <div style="flex:1;overflow:auto;min-width:0;">
          <div style="margin-bottom:8px;display:flex;align-items:center;gap:8px;">
            <span style="color:#60a5fa;font-weight:600;font-size:14px;">${title}</span>
            <span style="color:#475569;font-size:11px;">${source.split('\n').length} lines · ${this.nodeData.size} nodes · ${edgeLines.length} edges</span>
          </div>
          <div class="mermaid" id="${diagramId}">${this._escHtml(source)}</div>
          <details style="margin-top:12px;">
            <summary style="color:#64748b;font-size:12px;cursor:pointer;">View Mermaid Source</summary>
            <pre style="background:#1e293b;border:1px solid #334155;border-radius:6px;padding:12px;font-size:11px;color:#94a3b8;white-space:pre-wrap;max-height:200px;overflow-y:auto;margin-top:8px;">${this._escHtml(source)}</pre>
          </details>
        </div>
        <div id="mermaid-detail" style="width:260px;min-width:200px;background:#0f172a;border-left:1px solid #334155;padding:12px;overflow-y:auto;font-size:12px;color:#cbd5e1;">
          <h4 style="margin:0 0 10px;color:#60a5fa;">Node Detail</h4>
          <div id="mermaid-detail-content" style="color:#64748b;font-size:12px;">Click any node in the diagram to see its details, type, connections, and drill-down options.</div>
        </div>
      </div>`;

    // Render Mermaid
    if (typeof mermaid !== 'undefined') {
      try {
        mermaid.run({ nodes: container.querySelectorAll('.mermaid') }).then(() => {
          this._attachClickHandlers(diagramId);
        });
      } catch (e) {
        console.error('Mermaid render error:', e);
        const el = document.getElementById(diagramId);
        if (el) {
          el.className = '';
          el.innerHTML = `<div style="color:#f87171;margin-bottom:8px;">Diagram too complex (${source.split('\n').length} lines)</div>
            <pre style="background:#1e293b;border:1px solid #334155;border-radius:6px;padding:12px;font-size:11px;color:#94a3b8;white-space:pre-wrap;">${this._escHtml(source)}</pre>`;
        }
      }
    }

    const statsEl = document.getElementById('mermaid-stats');
    if (statsEl) statsEl.textContent = isFile ? `${data.program} flow` : 'System overview';
  }

  _attachClickHandlers(diagramId) {
    const svg = document.querySelector(`#${diagramId} svg`);
    if (!svg) return;

    // Mermaid creates <g> elements with class "node" for each node
    const nodes = svg.querySelectorAll('g.node');
    nodes.forEach(node => {
      node.style.cursor = 'pointer';
      node.addEventListener('click', (e) => {
        e.stopPropagation();
        const nodeId = node.id?.replace(/^flowchart-/, '').replace(/-\d+$/, '') || '';
        const nd = this.nodeData.get(nodeId);
        if (nd) this._showNodeDetail(nd);
        // Highlight clicked node
        nodes.forEach(n => n.style.opacity = n === node ? '1' : '0.4');
        setTimeout(() => nodes.forEach(n => n.style.opacity = '1'), 3000);
      });
    });

    // Click background to deselect
    svg.addEventListener('click', () => {
      nodes.forEach(n => n.style.opacity = '1');
      const panel = document.getElementById('mermaid-detail-content');
      if (panel) panel.innerHTML = '<div style="color:#64748b;">Click any node for details</div>';
    });
  }

  _showNodeDetail(nd) {
    const panel = document.getElementById('mermaid-detail-content');
    if (!panel) return;

    const typeIcons = { Program: '⚙️', Copybook: '📚', Section: '§', Paragraph: '¶' };
    const typeDescs = {
      Program: 'An executable COBOL program (.cbl). Programs can CALL other programs and COPY shared data structures.',
      Copybook: 'A reusable data structure (.cpy) included via COPY statements. Shared across multiple programs — changes here affect ALL programs that COPY it.',
      Section: 'A named block within PROCEDURE DIVISION. Sections group related paragraphs and can be PERFORMed as a unit.',
      Paragraph: 'A named executable block (like a function/method). Can be PERFORMed from other paragraphs. Contains the actual business logic.'
    };

    const fileName = nd.label + (nd.type === 'Copybook' ? '.cpy' : nd.type === 'Program' ? '.cbl' : '');

    let html = `
      <div style="border-left:3px solid ${nd.color};padding-left:10px;margin-bottom:12px;">
        <div style="font-size:15px;font-weight:700;color:#e2e8f0;">${typeIcons[nd.type] || '📋'} ${nd.label}</div>
        <div style="color:${nd.color};font-size:11px;font-weight:600;">${nd.type}</div>
      </div>

      <div style="background:rgba(30,41,59,0.5);border:1px solid #1e293b;border-radius:6px;padding:8px;margin-bottom:10px;font-size:11px;color:#94a3b8;">
        ${typeDescs[nd.type] || 'COBOL program element'}
      </div>

      <div style="display:grid;grid-template-columns:1fr 1fr;gap:6px;margin-bottom:10px;">
        <div style="background:#1e293b;border-radius:4px;padding:6px;text-align:center;">
          <div style="font-size:18px;font-weight:700;color:#60a5fa;">${nd.connections}</div>
          <div style="font-size:9px;color:#64748b;text-transform:uppercase;">Connections</div>
        </div>
        <div style="background:#1e293b;border-radius:4px;padding:6px;text-align:center;">
          <div style="font-size:18px;font-weight:700;color:${nd.color};">${nd.type === 'Program' ? '📦' : nd.type === 'Copybook' ? '🔗' : '⚡'}</div>
          <div style="font-size:9px;color:#64748b;text-transform:uppercase;">${nd.type}</div>
        </div>
      </div>`;

    // Impact assessment
    const impact = nd.connections > 8 ? 'Critical' : nd.connections > 4 ? 'High' : nd.connections > 1 ? 'Medium' : 'Low';
    const impactColor = nd.connections > 8 ? '#ef4444' : nd.connections > 4 ? '#f59e0b' : nd.connections > 1 ? '#10b981' : '#64748b';
    html += `<div style="margin-bottom:10px;">
      <div style="color:#64748b;font-size:10px;text-transform:uppercase;margin-bottom:2px;">Migration Impact</div>
      <span style="background:${impactColor}22;color:${impactColor};border:1px solid ${impactColor}44;border-radius:4px;padding:2px 8px;font-size:11px;font-weight:600;">${impact}</span>
      <div style="color:#475569;font-size:10px;margin-top:4px;">${nd.connections > 8 ? 'Many programs depend on this — test thoroughly' : nd.connections > 4 ? 'Several connections — coordinate changes' : nd.connections > 1 ? 'Some dependencies — standard testing' : 'Minimal dependencies — safe to migrate first'}</div>
    </div>`;

    // Node ID
    html += `<div style="font-size:10px;color:#475569;word-break:break-all;margin-bottom:10px;">ID: ${nd.id}</div>`;

    // Drill-down buttons
    if (nd.type === 'Program' || nd.type === 'Section' || nd.type === 'Paragraph') {
      html += `<div style="display:flex;flex-direction:column;gap:4px;">`;
      if (nd.type === 'Program') {
        html += `<button class="btn-small drill-btn" onclick="switchDashboard('controlflow');setTimeout(()=>controlFlowView?.selectFile('${nd.label}.cbl'),100)">⚡ Control Flow</button>`;
        html += `<button class="btn-small drill-btn" onclick="astExplorer?.drillIntoProgram('${nd.label}.cbl')">🔬 AST Explorer</button>`;
        html += `<button class="btn-small drill-btn" onclick="mermaidView?.loadDiagram('flow-ast-${nd.label}.cbl')">📐 File Diagram</button>`;
      }
      if (nd.type === 'Section' || nd.type === 'Paragraph') {
        html += `<button class="btn-small drill-btn" onclick="astExplorer?.drillIntoProgram('${this.currentFile || ''}')">🔬 View in AST</button>`;
      }
      html += `</div>`;
    }

    panel.innerHTML = html;
  }

  copySource() {
    if (this.mermaidSource) {
      navigator.clipboard.writeText(this.mermaidSource).then(() => {
        const btn = document.getElementById('mermaid-copy-btn');
        if (btn) { const orig = btn.textContent; btn.textContent = 'Copied!'; setTimeout(() => btn.textContent = orig, 1500); }
      });
    }
  }

  refresh() {
    this.loadFileList();
    this.loadDiagram(this.currentFile || '');
  }

  _escHtml(s) { return (s || '').replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;'); }
}
