// Neo4j-style Dependency Graph Visualization using vis-network
class DependencyGraph {
  constructor(elementId) {
    this.elementId = elementId;
    this.network = null;
    this.currentQuery = 'full';
    this.currentLayout = 'force';
    this.runId = null; // Will be loaded dynamically
    this.isRendering = false;
    this.controlsSetup = false;
    
    this.setupControls();
    this.initializeAndLoad();
  }

  setupControls() {
    // Prevent duplicate event listeners
    if (this.controlsSetup) return;
    this.controlsSetup = true;

    const querySelect = document.getElementById('query-select');
    const layoutSelect = document.getElementById('layout-select');
    const refreshBtn = document.getElementById('refresh-graph');
    const fitBtn = document.getElementById('fit-graph');
    const stabilizeBtn = document.getElementById('stabilize-graph');
    const closeDetailsBtn = document.getElementById('close-details');

    if (querySelect) {
      querySelect.addEventListener('change', (e) => {
        this.currentQuery = e.target.value;
        this.loadAndRender();
      });
    }

    if (layoutSelect) {
      layoutSelect.addEventListener('change', (e) => {
        this.currentLayout = e.target.value;
        this.loadAndRender();
      });
    }

    if (refreshBtn) {
      refreshBtn.addEventListener('click', () => this.loadAndRender());
    }

    if (fitBtn) {
      fitBtn.addEventListener('click', () => {
        if (this.network) {
          this.network.fit();
        }
      });
    }

    if (stabilizeBtn) {
      stabilizeBtn.addEventListener('click', () => {
        if (this.network) {
          this.network.setOptions({ physics: true });
          this.network.stabilize();
          setTimeout(() => {
            this.network.setOptions({ physics: false });
          }, 2000);
        }
      });
    }

    if (closeDetailsBtn) {
      closeDetailsBtn.addEventListener('click', () => {
        document.getElementById('node-details').hidden = true;
      });
    }
  }

  async initializeAndLoad() {
    try {
      // Fetch current run ID from API
      const runInfoResponse = await fetch('/api/runinfo');
      if (runInfoResponse.ok) {
        const runInfo = await runInfoResponse.json();
        this.runId = runInfo.runId;
        document.getElementById('current-run-id').textContent = this.runId || 'Unknown';
      }
    } catch (error) {
      console.error('Error fetching run ID:', error);
    }
    
    await this.loadAndRender();
  }

  async fetchGraphData(runId = null) {
    try {
      // Fetch from the MCP-powered API endpoint with optional runId
      const url = runId ? `/api/graph?runId=${runId}` : '/api/graph';
      const response = await fetch(url);
      if (!response.ok) {
        throw new Error(`HTTP ${response.status}: ${response.statusText}`);
      }
      
      const contentType = response.headers.get('content-type');
      if (!contentType || !contentType.includes('application/json')) {
        const text = await response.text();
        console.error('Non-JSON response:', text.substring(0, 200));
        throw new Error('Server returned non-JSON response (likely an error page)');
      }
      
      const data = await response.json();
      
      // Update runId if returned in response
      if (data.runId) {
        this.runId = data.runId;
        console.log(`📊 Graph data received for Run ${this.runId}`);
        
        // Update the graph header title
        const graphHeader = document.querySelector('h2');
        if (graphHeader && graphHeader.textContent.includes('Dependency Graph')) {
          graphHeader.innerHTML = `Dependency Graph | <span style="color: #10b981;">Run ${this.runId}</span>`;
        }
        
        const runIdDisplay = document.getElementById('current-run-id');
        if (runIdDisplay) {
          runIdDisplay.textContent = this.runId;
        }
      }
      
      // Check if there's an error in the response
      if (data.error) {
        console.error('Graph data error:', data.error);
        this.updateInfo(data.error);
        return { nodes: [], edges: [] };
      }
      
      return data;
    } catch (error) {
      console.error('Error fetching graph data:', error);
      this.updateInfo(`Error: ${error.message}`);
      return { nodes: [], edges: [] };
    }
  }

  getCypherQuery() {
    const queries = {
      'full': `MATCH (f1:CobolFile)-[d:DEPENDS_ON]->(f2:CobolFile) 
               WHERE d.runId = ${this.runId} 
               RETURN f1, d, f2`,
      
      'circular': `MATCH path = (start:CobolFile)-[:DEPENDS_ON*2..10]->(start:CobolFile)
                   WHERE start.runId = ${this.runId}
                   RETURN path
                   LIMIT 50`,
      
      'critical': `MATCH (f:CobolFile)
                   WHERE f.runId = ${this.runId}
                   OPTIONAL MATCH (f)-[out:DEPENDS_ON]->()
                   WHERE out.runId = ${this.runId}
                   OPTIONAL MATCH ()-[in:DEPENDS_ON]->(f)
                   WHERE in.runId = ${this.runId}
                   WITH f, count(DISTINCT out) + count(DISTINCT in) as connections
                   WHERE connections > 0
                   MATCH (f)-[d:DEPENDS_ON]-(other)
                   WHERE d.runId = ${this.runId}
                   RETURN f, d, other`,
      
      'programs': `MATCH (f1:CobolFile)-[d:DEPENDS_ON]->(f2:CobolFile)
                   WHERE d.runId = ${this.runId} AND f1.isCopybook = false
                   RETURN f1, d, f2`,
      
      'copybooks': `MATCH (f1:CobolFile)-[d:DEPENDS_ON]->(f2:CobolFile)
                    WHERE d.runId = ${this.runId} AND f2.isCopybook = true
                    RETURN f1, d, f2`
    };

    return queries[this.currentQuery] || queries['full'];
  }

  getVisLayoutConfig() {
    const layouts = {
      'force': {
        randomSeed: 42
      },
      'hierarchical': {
        enabled: true,
        direction: 'UD',
        sortMethod: 'directed',
        nodeSpacing: 150,
        levelSeparation: 200,
        blockShifting: true,
        edgeMinimization: true,
        parentCentralization: true
      }
    };

    return layouts[this.currentLayout] || layouts['force'];
  }

  async loadAndRender(runId = null) {
    // Prevent multiple simultaneous renders
    if (this.isRendering) {
      console.log('Already rendering, skipping...');
      return;
    }
    
    this.isRendering = true;
    this.updateInfo(`Loading graph data${runId ? ` for Run ${runId}` : ''}...`);
    
    try {
      const graphData = await this.fetchGraphData(runId);
      this.render(graphData);
    } catch (error) {
      console.error('Error loading graph:', error);
      this.updateInfo(`Error: ${error.message}`);
    } finally {
      this.isRendering = false;
    }
  }
  
  // Public method to load graph for a specific run (called from chat handler)
  async loadGraphForRun(runId) {
    console.log(`🔄 loadGraphForRun called with runId: ${runId}`);
    this.runId = runId; // Store the runId immediately
    await this.loadAndRender(runId);
  }

  render(graphData) {
    const container = document.getElementById(this.elementId);
    if (!container) {
      console.error('Graph container not found');
      return;
    }
    
    // Check if we have valid data
    if (!graphData || !graphData.nodes || !graphData.edges) {
      console.error('Invalid graph data:', graphData);
      this.updateInfo('No graph data available');
      return;
    }

    // Deduplicate nodes on client side as safety net
    const uniqueNodesMap = new Map();
    graphData.nodes.forEach(node => {
      if (node && node.id && !uniqueNodesMap.has(node.id)) {
        uniqueNodesMap.set(node.id, node);
      }
    });
    const deduplicatedNodes = Array.from(uniqueNodesMap.values());
    
    console.log(`Graph data: ${graphData.nodes.length} raw nodes, ${deduplicatedNodes.length} unique nodes, ${graphData.edges.length} edges`);

    // Transform data for vis-network
    const nodes = new vis.DataSet(
      deduplicatedNodes.map(n => ({
        id: n.id,
        label: n.label,
        title: `<strong>${n.label}</strong><br/>Type: ${n.isCopybook ? 'Copybook' : 'Program'}`,
        color: {
          background: n.isCopybook ? '#f16667' : '#68bdf6',
          border: n.isCopybook ? '#dc2626' : '#1d4ed8',
          highlight: {
            background: n.isCopybook ? '#fca5a5' : '#93c5fd',
            border: n.isCopybook ? '#991b1b' : '#1e3a8a'
          }
        },
        font: { color: '#e2e8f0', size: 14 },
        shape: 'dot',
        size: 25,
        isCopybook: n.isCopybook
      }))
    );

    const edges = new vis.DataSet(
      graphData.edges.map((e, idx) => ({
        id: idx,
        from: e.source,
        to: e.target,
        label: e.type,
        title: `${e.type}`,
        arrows: 'to',
        color: { color: '#94d486', highlight: '#fbbf24' },
        width: 2,
        font: { size: 12, color: '#94a3b8', align: 'top' },
        smooth: { enabled: true, type: 'dynamic' }
      }))
    );

    const data = { nodes, edges };

    const options = {
      nodes: {
        borderWidth: 2,
        shadow: true
      },
      edges: {
        smooth: {
          enabled: true,
          type: 'dynamic',
          roundness: 0.5
        }
      },
      layout: this.getVisLayoutConfig(),
      physics: this.currentLayout === 'force' ? {
        enabled: true,
        solver: 'forceAtlas2Based',
        forceAtlas2Based: {
          gravitationalConstant: -50,
          centralGravity: 0.01,
          springLength: 150,
          springConstant: 0.08,
          damping: 0.4
        },
        stabilization: {
          enabled: true,
          iterations: 100,
          updateInterval: 25
        }
      } : {
        enabled: false
      },
      interaction: {
        hover: true,
        tooltipDelay: 200,
        navigationButtons: true,
        keyboard: true
      }
    };

    // Create network
    if (this.network) {
      this.network.destroy();
    }
    
    this.network = new vis.Network(container, data, options);

    // Event handlers
    this.network.on('stabilizationIterationsDone', () => {
      this.network.setOptions({ physics: false });
      this.updateInfo(`Graph loaded: ${graphData.nodes.length} nodes, ${graphData.edges.length} dependencies`);
    });

    this.network.on('click', (params) => {
      if (params.nodes.length > 0) {
        const nodeId = params.nodes[0];
        const node = nodes.get(nodeId);
        this.showNodeDetails(node);
      }
    });

    // Fit to view
    setTimeout(() => {
      this.network.fit();
    }, 500);
  }

  showNodeDetails(node) {
    if (!node) return;

    const detailsPanel = document.getElementById('node-details');
    const detailsContent = document.getElementById('details-content');
    
    if (!detailsPanel || !detailsContent) return;
    
    let html = '<table class="details-table">';
    html += `<tr><th colspan="2">${node.label || 'Unknown'}</th></tr>`;
    html += `<tr><td>Type</td><td>${node.isCopybook ? 'Copybook' : 'Program'}</td></tr>`;
    
    // Get connections
    if (this.network) {
      const connectedEdges = this.network.getConnectedEdges(node.id);
      html += `<tr><td>Connections</td><td>${connectedEdges.length}</td></tr>`;
      
      const connectedNodes = this.network.getConnectedNodes(node.id);
      html += `<tr><td>Connected Files</td><td>${connectedNodes.length}</td></tr>`;
    }
    
    html += '</table>';
    
    detailsContent.innerHTML = html;
    detailsPanel.hidden = false;
  }

  updateInfo(text) {
    const infoDiv = document.getElementById('graph-info');
    if (infoDiv) {
      infoDiv.textContent = text;
    }
  }
}

// Initialize graph when DOM is ready
let dependencyGraph;
document.addEventListener('DOMContentLoaded', () => {
  // Check if vis-network is loaded
  if (typeof vis === 'undefined') {
    console.error('vis-network library not loaded');
    const infoDiv = document.getElementById('graph-info');
    if (infoDiv) {
      infoDiv.textContent = 'Error: vis-network library failed to load. Check console for details.';
    }
    return;
  }
  
  dependencyGraph = new DependencyGraph('dependency-graph');
  // Make graph accessible globally for chat integration
  window.dependencyGraph = dependencyGraph;
});
