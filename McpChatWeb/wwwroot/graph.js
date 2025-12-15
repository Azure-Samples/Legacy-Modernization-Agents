// Neo4j-style Dependency Graph Visualization using vis-network
class DependencyGraph {
  constructor(elementId) {
    console.log(`🏗️ DependencyGraph constructor called with elementId: ${elementId}`);
    this.elementId = elementId;
    this.network = null;
    this.currentQuery = 'full';
    this.currentLayout = 'force';
    this.runId = null; // Will be loaded dynamically
    this.isRendering = false;
    this.controlsSetup = false;
    
    console.log('📝 Setting up controls...');
    this.setupControls();
    console.log('📡 Calling initializeAndLoad...');
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

    // Add filter checkbox listeners
    const showPrograms = document.getElementById('show-programs');
    const showCopybooks = document.getElementById('show-copybooks');
    const showCalledPrograms = document.getElementById('show-called-programs');
    const showCallEdges = document.getElementById('show-call-edges');
    const showCopyEdges = document.getElementById('show-copy-edges');
    const showPerformEdges = document.getElementById('show-perform-edges');
    const showExecEdges = document.getElementById('show-exec-edges');
    const showReadEdges = document.getElementById('show-read-edges');
    const showWriteEdges = document.getElementById('show-write-edges');
    const showOpenEdges = document.getElementById('show-open-edges');
    const showCloseEdges = document.getElementById('show-close-edges');

    if (showPrograms) {
      showPrograms.addEventListener('change', () => this.applyFilters());
    }
    if (showCopybooks) {
      showCopybooks.addEventListener('change', () => this.applyFilters());
    }
    if (showCalledPrograms) {
      showCalledPrograms.addEventListener('change', () => this.applyFilters());
    }
    if (showCallEdges) {
      showCallEdges.addEventListener('change', () => this.applyFilters());
    }
    if (showCopyEdges) {
      showCopyEdges.addEventListener('change', () => this.applyFilters());
    }
    if (showPerformEdges) {
      showPerformEdges.addEventListener('change', () => this.applyFilters());
    }
    if (showExecEdges) {
      showExecEdges.addEventListener('change', () => this.applyFilters());
    }
    if (showReadEdges) {
      showReadEdges.addEventListener('change', () => this.applyFilters());
    }
    if (showWriteEdges) {
      showWriteEdges.addEventListener('change', () => this.applyFilters());
    }
    if (showOpenEdges) {
      showOpenEdges.addEventListener('change', () => this.applyFilters());
    }
    if (showCloseEdges) {
      showCloseEdges.addEventListener('change', () => this.applyFilters());
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
    console.log('🎨 render() method called');
    console.log('📦 graphData:', graphData);
    
    const container = document.getElementById(this.elementId);
    if (!container) {
      console.error(`❌ Graph container '${this.elementId}' not found`);
      return;
    }
    
    console.log(`✅ Container found:`, container);
    
    // Check if we have valid data
    if (!graphData || !graphData.nodes || !graphData.edges) {
      console.error('❌ Invalid graph data:', graphData);
      this.updateInfo('No graph data available');
      return;
    }
    
    console.log(`✅ Graph data valid: ${graphData.nodes?.length} nodes, ${graphData.edges?.length} edges`);

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

    // Identify called programs (targets of CALL edges)
    const calledProgramIds = new Set();
    graphData.edges.forEach(e => {
      if (e.type === 'CALL') {
        calledProgramIds.add(e.target);
      }
    });

    // Transform data for vis-network with enhanced details
    const nodes = new vis.DataSet(
      deduplicatedNodes.map(n => {
        const connections = nodeConnections.get(n.id) || 0;
        const nodeSize = 20 + (connections * 3); // Size based on connections
        const importance = connections > 5 ? 'Critical' : connections > 2 ? 'Important' : 'Standard';
        
        // Determine node type: Copybook, CalledProgram, or Program
        const isCalledProgram = !n.isCopybook && calledProgramIds.has(n.id);
        const nodeType = n.isCopybook ? 'Copybook (.cpy)' : (isCalledProgram ? 'Called Program (.cbl)' : 'Program (.cbl)');
        
        // Color scheme: Copybook=red, CalledProgram=green, Program=blue
        let backgroundColor, borderColor, highlightBg, highlightBorder, shadowColor;
        if (n.isCopybook) {
          backgroundColor = '#f16667';
          borderColor = '#dc2626';
          highlightBg = '#fca5a5';
          highlightBorder = '#991b1b';
          shadowColor = 'rgba(241, 102, 103, 0.5)';
        } else if (isCalledProgram) {
          backgroundColor = '#10b981';
          borderColor = '#059669';
          highlightBg = '#6ee7b7';
          highlightBorder = '#047857';
          shadowColor = 'rgba(16, 185, 129, 0.5)';
        } else {
          backgroundColor = '#68bdf6';
          borderColor = '#1d4ed8';
          highlightBg = '#93c5fd';
          highlightBorder = '#1e3a8a';
          shadowColor = 'rgba(104, 189, 246, 0.5)';
        }
        
        return {
          id: n.id,
          label: n.label,
          title: `${n.label}\nType: ${nodeType}\nDependencies: ${connections}\nPriority: ${importance}`,
          color: {
            background: backgroundColor,
            border: borderColor,
            highlight: {
              background: highlightBg,
              border: highlightBorder
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
            color: shadowColor,
            size: 10,
            x: 2,
            y: 2
          },
          isCopybook: n.isCopybook,
          isCalledProgram: isCalledProgram,
          connections: connections,
          lineCount: n.lineCount || 0
        };
      })
    );

    const edges = new vis.DataSet(
      graphData.edges.map((e, idx) => {
        const isCall = e.type === 'CALL';
        const isCopy = e.type === 'COPY';
        
        // Build edge label with line number if available
        let edgeLabel = e.type || 'DEPENDS_ON';
        if (e.lineNumber) {
          edgeLabel = `${e.type} (L${e.lineNumber})`;
        }
        
        // Build tooltip
        let tooltip = `${e.type || 'DEPENDS_ON'}\nFrom: ${graphData.nodes.find(n => n.id === e.source)?.label || e.source}\nTo: ${graphData.nodes.find(n => n.id === e.target)?.label || e.target}`;
        if (e.lineNumber) {
          tooltip += `\nLine: ${e.lineNumber}`;
        }
        if (e.context) {
          tooltip += `\n${e.context}`;
        }
        
        // Color scheme and styling for each dependency type
        const edgeType = (e.type || 'DEPENDS_ON').toUpperCase();
        let edgeColor, edgeWidth, fontSize;
        
        const edgeStyles = {
          'CALL': { color: '#10b981', width: 3, fontSize: 12 },      // Green
          'COPY': { color: '#3b82f6', width: 2.5, fontSize: 11 },    // Blue
          'PERFORM': { color: '#f59e0b', width: 2.5, fontSize: 11 }, // Amber
          'EXEC': { color: '#8b5cf6', width: 2.5, fontSize: 11 },    // Purple
          'READ': { color: '#06b6d4', width: 2, fontSize: 10 },      // Cyan
          'WRITE': { color: '#ec4899', width: 2, fontSize: 10 },     // Pink
          'OPEN': { color: '#84cc16', width: 2, fontSize: 10 },      // Lime
          'CLOSE': { color: '#ef4444', width: 2, fontSize: 10 }      // Red
        };
        
        const style = edgeStyles[edgeType] || { color: '#94d486', width: 2, fontSize: 10 };
        edgeColor = style.color;
        edgeWidth = style.width;
        fontSize = style.fontSize;
        
        return {
          id: idx,
          from: e.source,
          to: e.target,
          label: edgeLabel,
          title: tooltip,
          arrows: {
            to: {
              enabled: true,
              scaleFactor: 0.8,
              type: 'arrow'
            }
          },
          color: { 
            color: edgeColor,
            highlight: '#fbbf24',
            hover: '#fcd34d',
            opacity: 0.85
          },
          width: edgeWidth,
          font: { 
            size: fontSize,
            color: edgeColor,
            align: 'horizontal',
            background: 'rgba(15, 23, 42, 0.9)',
            strokeWidth: 0
          },
          edgeType: e.type,
          smooth: { 
            enabled: true,
            type: 'dynamic',
            roundness: 0.5
          },
          shadow: {
            enabled: true,
            color: `${edgeColor}40`,
            size: 5,
            x: 1,
            y: 1
          }
        };
      })
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
    console.log('🔨 Creating vis.Network...');
    if (this.network) {
      console.log('⚠️ Destroying existing network');
      this.network.destroy();
    }
    
    console.log('📊 Creating new vis.Network with:', { 
      containerExists: !!container, 
      nodesCount: nodes.length, 
      edgesCount: edges.length 
    });
    
    try {
      this.network = new vis.Network(container, data, options);
      console.log('✅ vis.Network created successfully');
    } catch (error) {
      console.error('❌ Error creating vis.Network:', error);
      this.updateInfo(`Error creating graph: ${error.message}`);
      return;
    }

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
    
    // Add Lines of Code (LoC)
    if (node.lineCount && node.lineCount > 0) {
      html += `<tr><td>Lines of Code</td><td style="color: #a78bfa; font-weight: 600;">${node.lineCount.toLocaleString()} LoC</td></tr>`;
    }
    
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

  applyFilters() {
    if (!this.network) {
      console.warn('Network not initialized, cannot apply filters');
      return;
    }

    // Get filter checkbox states
    const showPrograms = document.getElementById('show-programs')?.checked ?? true;
    const showCopybooks = document.getElementById('show-copybooks')?.checked ?? true;
    const showCalledPrograms = document.getElementById('show-called-programs')?.checked ?? true;
    const showCallEdges = document.getElementById('show-call-edges')?.checked ?? true;
    const showCopyEdges = document.getElementById('show-copy-edges')?.checked ?? true;
    const showPerformEdges = document.getElementById('show-perform-edges')?.checked ?? true;
    const showExecEdges = document.getElementById('show-exec-edges')?.checked ?? true;
    const showReadEdges = document.getElementById('show-read-edges')?.checked ?? true;
    const showWriteEdges = document.getElementById('show-write-edges')?.checked ?? true;
    const showOpenEdges = document.getElementById('show-open-edges')?.checked ?? true;
    const showCloseEdges = document.getElementById('show-close-edges')?.checked ?? true;

    // Get all nodes and edges
    const allNodes = this.network.body.data.nodes;
    const allEdges = this.network.body.data.edges;

    // Filter nodes based on type
    const visibleNodeIds = new Set();
    allNodes.forEach(node => {
      let isVisible = false;
      if (node.isCopybook && showCopybooks) {
        isVisible = true;
      } else if (node.isCalledProgram && showCalledPrograms) {
        isVisible = true;
      } else if (!node.isCopybook && !node.isCalledProgram && showPrograms) {
        isVisible = true;
      }

      if (isVisible) {
        visibleNodeIds.add(node.id);
      }
    });

    // Update node visibility
    const nodeUpdates = [];
    allNodes.forEach(node => {
      const shouldShow = visibleNodeIds.has(node.id);
      nodeUpdates.push({
        id: node.id,
        hidden: !shouldShow
      });
    });

    // Update edge visibility (hide edges connected to hidden nodes OR filtered edge types)
    const edgeUpdates = [];
    allEdges.forEach(edge => {
      const nodesVisible = visibleNodeIds.has(edge.from) && visibleNodeIds.has(edge.to);
      let edgeTypeVisible = true;
      
      const edgeType = (edge.edgeType || '').toUpperCase();
      if (edgeType === 'CALL' && !showCallEdges) {
        edgeTypeVisible = false;
      } else if (edgeType === 'COPY' && !showCopyEdges) {
        edgeTypeVisible = false;
      } else if (edgeType === 'PERFORM' && !showPerformEdges) {
        edgeTypeVisible = false;
      } else if (edgeType === 'EXEC' && !showExecEdges) {
        edgeTypeVisible = false;
      } else if (edgeType === 'READ' && !showReadEdges) {
        edgeTypeVisible = false;
      } else if (edgeType === 'WRITE' && !showWriteEdges) {
        edgeTypeVisible = false;
      } else if (edgeType === 'OPEN' && !showOpenEdges) {
        edgeTypeVisible = false;
      } else if (edgeType === 'CLOSE' && !showCloseEdges) {
        edgeTypeVisible = false;
      }
      
      const shouldShow = nodesVisible && edgeTypeVisible;
      edgeUpdates.push({
        id: edge.id,
        hidden: !shouldShow
      });
    });

    // Apply updates
    allNodes.update(nodeUpdates);
    allEdges.update(edgeUpdates);

    // Update info text
    const visibleCount = visibleNodeIds.size;
    const totalCount = allNodes.length;
    this.updateInfo(`Showing ${visibleCount} of ${totalCount} nodes`);
  }
}

// Initialize graph when DOM is ready
let dependencyGraph;
document.addEventListener('DOMContentLoaded', () => {
  console.log('🚀 DOMContentLoaded fired');
  
  // Check if vis-network is loaded
  if (typeof vis === 'undefined') {
    console.error('❌ vis-network library not loaded');
    const infoDiv = document.getElementById('graph-info');
    if (infoDiv) {
      infoDiv.textContent = 'Error: vis-network library failed to load. Check console for details.';
    }
    return;
  }
  
  console.log('✅ vis-network library loaded successfully');
  
  const graphContainer = document.getElementById('dependency-graph');
  if (!graphContainer) {
    console.error('❌ Graph container element not found');
    return;
  }
  console.log('✅ Graph container element found');
  
  console.log('🔨 Creating DependencyGraph instance...');
  dependencyGraph = new DependencyGraph('dependency-graph');
  console.log('✅ DependencyGraph instance created');
  
  // Make graph accessible globally for chat integration
  window.dependencyGraph = dependencyGraph;
});
