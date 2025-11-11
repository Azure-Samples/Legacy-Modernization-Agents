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
        this.updateGraphTitle(this.runId);
        console.log(`📊 Initial graph load for Run ${this.runId}`);
      }
    } catch (error) {
      console.error('Error fetching run ID:', error);
    }
    
    // Pass runId to loadAndRender to ensure correct data is fetched
    await this.loadAndRender(this.runId);
  }
  
  updateGraphTitle(runId) {
    const currentRunIdSpan = document.getElementById('current-run-id');
    const graphRunBadge = document.getElementById('graph-run-badge');
    
    if (currentRunIdSpan && runId) {
      currentRunIdSpan.textContent = runId;
    }
    
    if (graphRunBadge && runId) {
      graphRunBadge.style.display = 'inline';
    } else if (graphRunBadge) {
      graphRunBadge.style.display = 'none';
    }
  }

  async fetchGraphData(runId = null) {
    try {
      // Fetch from the MCP-powered API endpoint with optional runId
      const url = runId ? `/api/graph?runId=${runId}` : '/api/graph';
      console.log(`🔍 Fetching graph data from: ${url}${runId ? ` (Run ${runId})` : ' (current run)'}`);
      
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
      
      console.log(`📦 Received graph data:`, {
        requestedRunId: runId,
        returnedRunId: data.runId,
        nodeCount: data.nodes?.length || 0,
        edgeCount: data.edges?.length || 0
      });
      
      // Update runId if returned in response
      if (data.runId) {
        this.runId = data.runId;
        console.log(`📊 Graph data confirmed for Run ${this.runId}`);
        
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
        hierarchical: {
          enabled: true,
          direction: 'UD',
          sortMethod: 'directed',
          nodeSpacing: 150,
          levelSeparation: 200,
          blockShifting: true,
          edgeMinimization: true
        }
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

    // Calculate node importance (number of connections)
    const nodeConnections = new Map();
    graphData.edges.forEach(e => {
      nodeConnections.set(e.source, (nodeConnections.get(e.source) || 0) + 1);
      nodeConnections.set(e.target, (nodeConnections.get(e.target) || 0) + 1);
    });

    // Transform data for vis-network with enhanced details
    const nodes = new vis.DataSet(
      deduplicatedNodes.map(n => {
        const connections = nodeConnections.get(n.id) || 0;
        const nodeSize = 20 + (connections * 3); // Size based on connections
        const importance = connections > 5 ? 'Critical' : connections > 2 ? 'Important' : 'Standard';
        
        return {
          id: n.id,
          label: n.label,
          title: `${n.label}\nType: ${n.isCopybook ? 'Copybook (.cpy)' : 'Program (.cbl)'}\nDependencies: ${connections}\nPriority: ${importance}`,
          color: {
            background: n.isCopybook ? '#f16667' : '#68bdf6',
            border: n.isCopybook ? '#dc2626' : '#1d4ed8',
            highlight: {
              background: n.isCopybook ? '#fca5a5' : '#93c5fd',
              border: n.isCopybook ? '#991b1b' : '#1e3a8a'
            }
          },
          font: { 
            color: '#ffffff',
            size: 12 + Math.min(connections, 8),
            face: 'system-ui',
            strokeWidth: 3,
            strokeColor: '#0f172a'
          },
          shape: 'dot',
          size: nodeSize,
          borderWidth: 2 + Math.min(connections, 4),
          shadow: {
            enabled: true,
            color: n.isCopybook ? 'rgba(241, 102, 103, 0.5)' : 'rgba(104, 189, 246, 0.5)',
            size: 10,
            x: 2,
            y: 2
          },
          isCopybook: n.isCopybook,
          connections: connections
        };
      })
    );

    const edges = new vis.DataSet(
      graphData.edges.map((e, idx) => ({
        id: idx,
        from: e.source,
        to: e.target,
        label: e.type || 'DEPENDS_ON',
        title: `${e.type || 'DEPENDS_ON'}\nFrom: ${graphData.nodes.find(n => n.id === e.source)?.label || e.source}\nTo: ${graphData.nodes.find(n => n.id === e.target)?.label || e.target}`,
        arrows: {
          to: {
            enabled: true,
            scaleFactor: 0.8,
            type: 'arrow'
          }
        },
        color: { 
          color: '#94d486',
          highlight: '#fbbf24',
          hover: '#10b981',
          opacity: 0.8
        },
        width: 2.5,
        font: { 
          size: 11,
          color: '#a8dadc',
          align: 'horizontal',
          background: 'rgba(15, 23, 42, 0.8)',
          strokeWidth: 0
        },
        smooth: { 
          enabled: true,
          type: 'dynamic',
          roundness: 0.5
        },
        shadow: {
          enabled: true,
          color: 'rgba(148, 212, 134, 0.3)',
          size: 5,
          x: 1,
          y: 1
        }
      }))
    );

    const data = { nodes, edges };

    const options = {
      nodes: {
        borderWidth: 3,
        shadow: {
          enabled: true,
          color: 'rgba(0, 0, 0, 0.5)',
          size: 10,
          x: 3,
          y: 3
        },
        shapeProperties: {
          interpolation: true
        },
        scaling: {
          min: 20,
          max: 60,
          label: {
            enabled: true,
            min: 12,
            max: 20
          }
        }
      },
      edges: {
        smooth: {
          enabled: true,
          type: 'dynamic',
          roundness: 0.5
        },
        hoverWidth: 1.5,
        selectionWidth: 2
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
      
      // Calculate statistics
      const programs = deduplicatedNodes.filter(n => !n.isCopybook).length;
      const copybooks = deduplicatedNodes.filter(n => n.isCopybook).length;
      const avgConnections = Array.from(nodeConnections.values()).reduce((a, b) => a + b, 0) / deduplicatedNodes.length;
      
      this.updateInfo(`📊 Graph: ${deduplicatedNodes.length} nodes (${programs} programs, ${copybooks} copybooks) • ${graphData.edges.length} dependencies • Avg ${avgConnections.toFixed(1)} connections/node`);
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
    
    const typeIcon = node.isCopybook ? '📚' : '⚙️';
    const typeColor = node.isCopybook ? '#f16667' : '#68bdf6';
    const priorityText = node.connections > 5 ? 'Critical' : node.connections > 2 ? 'Important' : 'Standard';
    const priorityColor = node.connections > 5 ? '#f87171' : node.connections > 2 ? '#fbbf24' : '#10b981';
    
    let html = '<table class="details-table">';
    html += `<tr><th colspan="2" style="background: ${typeColor}; color: white;">${typeIcon} ${node.label || 'Unknown'}</th></tr>`;
    html += `<tr><td>File Type</td><td style="color: ${typeColor}; font-weight: 600;">${node.isCopybook ? 'COBOL Copybook (.cpy)' : 'COBOL Program (.cbl)'}</td></tr>`;
    
    // Get dependency information
    if (this.network) {
      const connectedEdges = this.network.getConnectedEdges(node.id);
      const connectedNodeIds = this.network.getConnectedNodes(node.id);
      
      // Separate incoming and outgoing dependencies
      const edges = this.network.body.data.edges;
      let dependsOn = 0;
      let usedBy = 0;
      
      connectedEdges.forEach(edgeId => {
        const edge = edges.get(edgeId);
        if (edge) {
          if (edge.from === node.id) dependsOn++;
          if (edge.to === node.id) usedBy++;
        }
      });
      
      html += `<tr><td>Total Dependencies</td><td style="color: #10b981; font-weight: bold;">${connectedEdges.length}</td></tr>`;
      html += `<tr><td>Uses (COPY/CALL)</td><td style="color: #38bdf8;">${dependsOn} files</td></tr>`;
      html += `<tr><td>Used By</td><td style="color: #fbbf24;">${usedBy} files</td></tr>`;
      html += `<tr><td>Impact Level</td><td style="color: ${priorityColor}; font-weight: 600;">${priorityText}</td></tr>`;
      
      // Show description based on type
      if (node.isCopybook) {
        html += `<tr><td colspan="2" style="padding-top: 8px; font-size: 11px; color: #94a3b8; border-top: 1px solid rgba(59, 130, 246, 0.2);">
          Copybooks are reusable code modules included via COPY statements. High usage indicates critical shared data structures or common routines.
        </td></tr>`;
      } else {
        html += `<tr><td colspan="2" style="padding-top: 8px; font-size: 11px; color: #94a3b8; border-top: 1px solid rgba(59, 130, 246, 0.2);">
          Programs are executable COBOL modules. Dependencies show which copybooks and subprograms are used.
        </td></tr>`;
      }
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
