// Sigma.js WebGL Graph Renderer — handles 10K+ nodes at 60 FPS.
// Uses graphology for the data model and Sigma for GPU-accelerated rendering.
// ForceAtlas2 layout runs in the main thread (Web Worker version requires ESM build).

class SigmaGraphRenderer {
  constructor(containerId) {
    this.containerId = containerId;
    this.renderer = null;
    this.graph = null;
    this.labelsVisible = true;

    // Search handler
    const searchInput = document.getElementById('sigma-search');
    if (searchInput) {
      searchInput.addEventListener('input', (e) => this.searchNode(e.target.value));
    }
  }

  async loadAndRender(runId) {
    const container = document.getElementById(this.containerId);
    if (!container) return;

    // Fetch graph data from the rekt Neo4j endpoint first, fallback to standard
    let graphData;
    try {
      const rektResp = await fetch(`/api/graph/rekt${runId ? `?runId=${runId}` : ''}`);
      if (rektResp.ok) {
        graphData = await rektResp.json();
      }
    } catch (_) { /* fallback below */ }

    if (!graphData) {
      try {
        const params = runId ? `?runId=${runId}` : '';
        const resp = await fetch(`/api/graph${params}`);
        if (!resp.ok) return;
        graphData = await resp.json();
      } catch (e) {
        console.error('Sigma: fetch error', e);
        return;
      }
    }

    if (!graphData.nodes?.length) {
      container.innerHTML = '<div style="display:flex;align-items:center;justify-content:center;height:100%;color:#94a3b8;">No graph data. Run analysis first.</div>';
      return;
    }

    // Dispose previous renderer
    if (this.renderer) {
      this.renderer.kill();
      this.renderer = null;
    }

    // Build graphology graph (multi: allows duplicate edges between same nodes)
    this.graph = new graphology.Graph({ multi: true, type: 'directed' });

    const nodeColors = {
      program: '#68bdf6',
      copybook: '#f16667',
      called: '#10b981',
      inferred: '#f59e0b',
      default: '#68bdf6',
    };

    const edgeColors = {
      CALL: '#10b981',
      COPY: '#3b82f6',
      PERFORM: '#f59e0b',
      'EXEC SQL': '#a855f7',
      'EXEC CICS': '#ec4899',
      READ: '#06b6d4',
      WRITE: '#ec4899',
      OPEN: '#84cc16',
      CLOSE: '#ef4444',
    };

    // Add nodes
    const nodeSet = new Set();
    for (const node of graphData.nodes) {
      const id = node.id || node.fileName || node.name;
      if (nodeSet.has(id)) continue;
      nodeSet.add(id);

      let color = nodeColors.default;
      if (node.isInferred) color = nodeColors.inferred;
      else if (node.isCopybook) color = nodeColors.copybook;
      else if (node.isCalledProgram) color = nodeColors.called;
      else color = nodeColors.program;

      this.graph.addNode(id, {
        label: node.label || node.fileName || id,
        size: Math.min(15, 5 + (node.connections || 0) * 0.5),
        color: color,
        x: Math.random() * 1000,
        y: Math.random() * 1000,
        // Custom data for inspection
        _data: node,
      });
    }

    // Add edges
    let edgeIdx = 0;
    for (const edge of (graphData.edges || [])) {
      const source = edge.source || edge.from;
      const target = edge.target || edge.to;
      if (!this.graph.hasNode(source) || !this.graph.hasNode(target)) continue;

      this.graph.addEdge(source, target, {
        color: edgeColors[edge.type] || '#475569',
        size: 1,
        label: edge.type || '',
        _type: edge.type,
      });
      edgeIdx++;
    }

    // Run ForceAtlas2 layout if available, otherwise simple force layout
    if (typeof ForceAtlas2Layout !== 'undefined') {
      ForceAtlas2Layout.assign(this.graph, { iterations: 150, settings: { gravity: 1, scalingRatio: 15, strongGravityMode: true, barnesHutOptimize: this.graph.order > 500 } });
    } else if (typeof graphologyLayoutForceAtlas2 !== 'undefined') {
      graphologyLayoutForceAtlas2.assign(this.graph, { iterations: 150, settings: { gravity: 1, scalingRatio: 15 } });
    } else {
      this._simpleForceLayout(80);
    }

    // Ensure container is ready for WebGL (Safari/Edge compatibility)
    container.style.minHeight = '400px';
    if (typeof ResizeObserver !== 'undefined') {
      await new Promise(resolve => {
        if (container.offsetWidth > 0 && container.offsetHeight > 0) { resolve(); return; }
        const ro = new ResizeObserver(entries => {
          for (const entry of entries) {
            if (entry.contentRect.width > 0 && entry.contentRect.height > 0) { ro.disconnect(); resolve(); return; }
          }
        });
        ro.observe(container);
        setTimeout(() => { ro.disconnect(); resolve(); }, 1000);
      });
    } else {
      for (let i = 0; i < 20; i++) { await new Promise(r => setTimeout(r, 50)); if (container.offsetWidth > 0 && container.offsetHeight > 0) break; }
    }

    // Create Sigma renderer
    try {
      this.renderer = new Sigma(this.graph, container, {
        renderEdgeLabels: false,
        labelDensity: 0.07,
        labelGridCellSize: 60,
        labelRenderedSizeThreshold: 4,
        labelFont: 'system-ui, -apple-system, sans-serif',
        minCameraRatio: 0.01,
        maxCameraRatio: 100,
        defaultNodeColor: '#68bdf6',
        defaultEdgeColor: '#475569',
        allowInvalidContainer: true,
      });
    } catch (e) {
      console.error('Sigma init failed:', e);
      container.innerHTML = `<div style="padding:20px;color:#f87171;">WebGL init failed: ${e.message}</div>`;
      return;
    }

    // Click handler — show node details
    this.renderer.on('clickNode', ({ node }) => {
      const attrs = this.graph.getNodeAttributes(node);
      const detailsEl = document.getElementById('details-content');
      const detailsPanel = document.getElementById('node-details');
      if (detailsEl && detailsPanel) {
        detailsEl.innerHTML = `
          <p><strong>${attrs.label}</strong></p>
          <p>Type: ${attrs._data?.isCopybook ? 'Copybook' : 'Program'}</p>
          <p>Connections: ${this.graph.degree(node)}</p>
          ${attrs._data?.lineCount ? `<p>Lines: ${attrs._data.lineCount}</p>` : ''}
          <p><button class="btn-small drill-btn" onclick="astExplorer?.drillIntoProgram('${(attrs.label || node).replace(/'/g, "\\'")}')">🔍 Drill into Structure</button></p>
        `;
        detailsPanel.hidden = false;
      }
    });

    // Double-click handler — drill into program structure
    this.renderer.on('doubleClickNode', ({ node }) => {
      const attrs = this.graph.getNodeAttributes(node);
      if (astExplorer) {
        astExplorer.drillIntoProgram(attrs.label || node);
      }
    });

    // Update count
    const countEl = document.getElementById('sigma-node-count');
    if (countEl) {
      countEl.textContent = `${this.graph.order.toLocaleString()} nodes, ${this.graph.size.toLocaleString()} edges`;
    }
  }

  searchNode(query) {
    if (!this.graph || !this.renderer || !query) {
      // Reset all highlights
      if (this.graph) {
        this.graph.forEachNode((node, attrs) => {
          this.graph.setNodeAttribute(node, 'highlighted', false);
        });
      }
      if (this.renderer) this.renderer.refresh();
      return;
    }

    const lowerQuery = query.toLowerCase();
    let foundNode = null;

    this.graph.forEachNode((node, attrs) => {
      const match = (attrs.label || '').toLowerCase().includes(lowerQuery);
      this.graph.setNodeAttribute(node, 'highlighted', match);
      if (match && !foundNode) foundNode = node;
    });

    // Center camera on first match
    if (foundNode) {
      const attrs = this.graph.getNodeAttributes(foundNode);
      const camera = this.renderer.getCamera();
      camera.animate({ x: attrs.x, y: attrs.y, ratio: 0.3 }, { duration: 300 });
    }

    this.renderer.refresh();
  }

  zoomToFit() {
    if (this.renderer) {
      const camera = this.renderer.getCamera();
      camera.animate({ x: 0.5, y: 0.5, ratio: 1 }, { duration: 300 });
    }
  }

  toggleLabels() {
    this.labelsVisible = !this.labelsVisible;
    if (this.renderer) {
      this.renderer.setSetting('labelRenderedSizeThreshold', this.labelsVisible ? 6 : 999);
    }
  }

  // Simple force-directed layout (no external dependency)
  _simpleForceLayout(iterations) {
    const g = this.graph;
    const nodes = [];
    g.forEachNode((id, attrs) => {
      nodes.push({ id, x: attrs.x, y: attrs.y, vx: 0, vy: 0 });
    });
    const nodeIdx = new Map(nodes.map((n, i) => [n.id, i]));

    const edges = [];
    g.forEachEdge((edge, attrs, source, target) => {
      const si = nodeIdx.get(source);
      const ti = nodeIdx.get(target);
      if (si !== undefined && ti !== undefined) edges.push([si, ti]);
    });

    const repulsion = 5000;
    const attraction = 0.01;
    const gravity = 0.05;
    const damping = 0.9;

    for (let iter = 0; iter < iterations; iter++) {
      // Repulsion between all pairs (Barnes-Hut omitted for simplicity — works up to ~1K nodes)
      for (let i = 0; i < nodes.length; i++) {
        for (let j = i + 1; j < nodes.length; j++) {
          let dx = nodes[i].x - nodes[j].x;
          let dy = nodes[i].y - nodes[j].y;
          let dist = Math.sqrt(dx * dx + dy * dy) || 1;
          let force = repulsion / (dist * dist);
          let fx = (dx / dist) * force;
          let fy = (dy / dist) * force;
          nodes[i].vx += fx; nodes[i].vy += fy;
          nodes[j].vx -= fx; nodes[j].vy -= fy;
        }
      }

      // Attraction along edges
      for (const [si, ti] of edges) {
        let dx = nodes[ti].x - nodes[si].x;
        let dy = nodes[ti].y - nodes[si].y;
        let dist = Math.sqrt(dx * dx + dy * dy) || 1;
        let force = attraction * dist;
        let fx = (dx / dist) * force;
        let fy = (dy / dist) * force;
        nodes[si].vx += fx; nodes[si].vy += fy;
        nodes[ti].vx -= fx; nodes[ti].vy -= fy;
      }

      // Gravity toward center
      for (const n of nodes) {
        n.vx -= n.x * gravity;
        n.vy -= n.y * gravity;
      }

      // Apply velocities
      for (const n of nodes) {
        n.vx *= damping; n.vy *= damping;
        n.x += n.vx; n.y += n.vy;
      }
    }

    // Write back positions
    for (const n of nodes) {
      g.setNodeAttribute(n.id, 'x', n.x);
      g.setNodeAttribute(n.id, 'y', n.y);
    }
  }
}
