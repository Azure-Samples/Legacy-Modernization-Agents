// Dashboard Tab Switcher — controls which dashboard panel is visible
// Manages tab state, lazy-loading of dashboard data, and CSS transitions.

let sigmaGraph = null;
let astExplorer = null;
let galaxyView = null;
let servicesView = null;
let architectView = null;
let controlFlowView = null;
let mermaidView = null;
let migrationPlanner = null;
let targetArchView = null;

// Rekt scan run selector
let _currentScanRunId = 'latest';
window.getSelectedScanRunId = function() { return _currentScanRunId; };

// Populate scan run dropdown on load
async function loadScanRuns() {
  try {
    const resp = await fetch('/api/graph/rekt/runs');
    if (!resp.ok) return;
    const runs = await resp.json();
    const select = document.getElementById('rekt-scan-select');
    if (!select || runs.length === 0) return;

    // Keep the first two options (Latest, All)
    select.querySelectorAll('option').forEach((opt, i) => { if (i > 1) opt.remove(); });

    // Group runs by file count to find meaningful ones
    const meaningful = runs.filter(r => r.fileCount > 2).slice(0, 15);
    for (const run of meaningful) {
      const opt = document.createElement('option');
      opt.value = run.runId;
      opt.textContent = `Run ${run.runId} (${run.fileCount} files)`;
      select.appendChild(opt);
    }
  } catch (e) { console.error('Failed to load scan runs:', e); }
}

window._onScanRunChange = function(value) {
  const prev = _currentScanRunId;
  _currentScanRunId = value;
  console.log('Scan run changed to:', value);
  // Invalidate caches that pin to a specific scan run so they re-fetch.
  if (prev !== value) {
    if (migrationPlanner && typeof migrationPlanner.refresh === 'function') {
      migrationPlanner._scanRunIdAtFetch = value;
      migrationPlanner.refresh();
    }
    if (galaxyView) {
      galaxyView.galaxyData = null;
      galaxyView.astData = null;
    }
    // Reset file-level views so they reload file lists and clear stale content
    if (astExplorer) {
      astExplorer.structureData = null;
      astExplorer.currentFile = null;
      astExplorer.loadFileList();
    }
    if (controlFlowView) {
      controlFlowView.currentFile = null;
      controlFlowView.loadFileList();
    }
    if (mermaidView) {
      mermaidView.currentFile = '';
      mermaidView.loadFileList();
    }
    if (targetArchView) {
      // Invalidate cached recommendations / programs so the next loadAndRender
      // (triggered by switchDashboard below) refetches from the new run.
      // Filter + zoom state stay so the user doesn't lose UI context.
      targetArchView.programs = [];
      targetArchView.recommendations = null;
      targetArchView.selectedComponent = null;
      // Close any stale fullscreen overlay so we don't show diagram counts
      // from the previous scan run.
      if (typeof targetArchView._exitDiagramFullscreen === 'function') {
        targetArchView._exitDiagramFullscreen();
      }
    }
  }
  // Refresh the active view with the new scan context
  const activeTab = document.querySelector('.dashboard-tab.active');
  if (activeTab) switchDashboard(activeTab.dataset.tab);
};

// Load scan runs when page loads
document.addEventListener('DOMContentLoaded', loadScanRuns);

function switchDashboard(tabName) {
  // Update tab buttons
  document.querySelectorAll('.dashboard-tab').forEach(btn => {
    btn.classList.toggle('active', btn.dataset.tab === tabName);
  });

  // Hide all dashboard panels
  const panels = {
    services: ['services-container'],
    dependency: ['dependency-graph', 'graph-toolbar'],
    controlflow: ['controlflow-container'],
    mermaid: ['mermaid-container'],
    galaxy: ['ast-galaxy-container'],
    ast: ['ast-explorer-container'],
    migration: ['migration-planner-container'],
    'target-arch': ['target-arch-container'],
    portfolio: ['portfolio-container'],
    complexity: ['complexity-container'],
  };

  // Hide everything first
  Object.values(panels).flat().forEach(id => {
    const el = document.getElementById(id);
    if (el) el.style.display = 'none';
  });

  // Also hide graph toolbar for non-dependency views
  const toolbar = document.querySelector('.graph-toolbar');
  if (toolbar) toolbar.style.display = tabName === 'dependency' ? '' : 'none';

  // Show selected panel
  const toShow = panels[tabName] || [];
  toShow.forEach(id => {
    const el = document.getElementById(id);
    if (el) el.style.display = '';
  });

  // Lazy-init renderers
  if (tabName === 'services') {
    if (!servicesView) {
      servicesView = new ServicesView('services-content');
    }
    servicesView.loadAndRender();
  }

  if (tabName === 'controlflow') {
    if (!controlFlowView) {
      controlFlowView = new ControlFlowView('cf-graph');
    }
    controlFlowView.loadFileList();
  }

  if (tabName === 'mermaid') {
    if (!mermaidView) {
      mermaidView = new MermaidView('mermaid-content');
    }
    mermaidView.loadFileList();
    mermaidView.loadDiagram('');
  }

  if (tabName === 'ast') {
    if (!astExplorer) {
      astExplorer = new ASTExplorer('ast-graph');
      window.astExplorer = astExplorer;
    }
    astExplorer.loadFileList();
  }

  if (tabName === 'galaxy') {
    if (!galaxyView) {
      galaxyView = new ASTGalaxyView('galaxy-graph');
      window.galaxyView = galaxyView;
    }
    galaxyView.loadAndRender();
  }

  // Destroy galaxy when switching away to free resources
  if (tabName !== 'galaxy' && galaxyView) {
    galaxyView.destroy();
    galaxyView = null;
    window.galaxyView = null;
  }

  if (tabName === 'portfolio') {
    loadPortfolioData();
  }

  if (tabName === 'migration') {
    if (!migrationPlanner) {
      migrationPlanner = new MigrationPlanner('mp-root');
    }
    migrationPlanner.loadAndRender();
  }

  if (tabName === 'target-arch') {
    if (!targetArchView) {
      targetArchView = new TargetArchitectureView('tarch-root');
      window.targetArchView = targetArchView;
    }
    targetArchView.loadAndRender();
  }

  if (tabName === 'complexity') {
    loadComplexityData();
  }
}

// ── Portfolio aggregation ─────────────────────────────────────────────
async function loadPortfolioData() {
  try {
    // Use the rekt Neo4j endpoint for graph stats
    const resp = await fetch('/api/graph/stats');
    if (!resp.ok) {
      // Fallback: use existing dependency graph data
      const graphResp = await fetch('/api/graph');
      if (!graphResp.ok) return;
      const data = await graphResp.json();
      const programs = (data.nodes || []).filter(n => !n.isCopybook && !n.isInferred);
      const copybooks = (data.nodes || []).filter(n => n.isCopybook);
      setStatValue('portfolio-programs', programs.length);
      setStatValue('portfolio-copybooks', copybooks.length);
      setStatValue('portfolio-dependencies', (data.edges || []).length);
      return;
    }
    const stats = await resp.json();
    setStatValue('portfolio-programs', stats.programs ?? '-');
    setStatValue('portfolio-copybooks', stats.copybooks ?? '-');
    setStatValue('portfolio-loc', (stats.totalLoc ?? 0).toLocaleString());
    setStatValue('portfolio-dependencies', stats.dependencies ?? '-');
    setStatValue('portfolio-ast-nodes', (stats.astNodes ?? 0).toLocaleString());
    setStatValue('portfolio-circular', stats.circularDependencies ?? '-');

    // Critical files table
    if (stats.criticalFiles) {
      const container = document.getElementById('portfolio-critical-files');
      container.innerHTML = '<table style="width:100%;border-collapse:collapse;">' +
        '<tr style="color:#60a5fa;border-bottom:1px solid #334155;"><th style="text-align:left;padding:4px;">File</th><th style="text-align:right;padding:4px;">Connections</th><th style="text-align:right;padding:4px;">LOC</th></tr>' +
        stats.criticalFiles.map(f =>
          `<tr style="border-bottom:1px solid #1e293b;"><td style="padding:4px;">${f.name}</td><td style="text-align:right;padding:4px;">${f.connections}</td><td style="text-align:right;padding:4px;">${f.lineCount ?? '-'}</td></tr>`
        ).join('') + '</table>';
    }
  } catch (e) {
    console.error('Portfolio load error:', e);
  }
}

// ── Complexity / Readiness ────────────────────────────────────────────
async function loadComplexityData() {
  try {
    const resp = await fetch('/api/graph/complexity');
    if (!resp.ok) return;
    const data = await resp.json();

    setStatValue('readiness-reducible', data.reducibleFiles ?? '-');
    setStatValue('readiness-gotos', data.filesWithGoTo ?? '-');
    setStatValue('readiness-sql', data.filesWithSql ?? '-');
    setStatValue('readiness-cics', data.filesWithCics ?? '-');

    // Complexity distribution bar chart
    if (data.distribution) {
      const chart = document.getElementById('complexity-chart');
      const maxVal = Math.max(...Object.values(data.distribution), 1);
      chart.innerHTML = Object.entries(data.distribution).map(([tier, count]) => {
        const height = Math.max(8, (count / maxVal) * 180);
        const colors = { simple: '#10b981', medium: '#f59e0b', complex: '#ef4444' };
        return `<div style="flex:1;text-align:center;">
          <div style="height:${height}px;background:${colors[tier] || '#3b82f6'};border-radius:4px 4px 0 0;min-width:30px;"></div>
          <div style="font-size:11px;color:#94a3b8;margin-top:4px;">${tier}</div>
          <div style="font-size:13px;color:#e2e8f0;">${count}</div>
        </div>`;
      }).join('');
    }

    // Readiness table
    if (data.files) {
      const table = document.getElementById('readiness-table');
      table.innerHTML = '<table style="width:100%;border-collapse:collapse;">' +
        '<tr style="color:#60a5fa;border-bottom:1px solid #334155;"><th style="text-align:left;padding:4px;">File</th><th style="padding:4px;">Complexity</th><th style="padding:4px;">Reducible</th><th style="padding:4px;">GO TO</th><th style="padding:4px;">SQL</th></tr>' +
        data.files.slice(0, 50).map(f =>
          `<tr style="border-bottom:1px solid #1e293b;">
            <td style="padding:4px;">${f.name}</td>
            <td style="text-align:center;padding:4px;"><span style="color:${f.complexity === 'simple' ? '#10b981' : f.complexity === 'complex' ? '#ef4444' : '#f59e0b'}">${f.complexity}</span></td>
            <td style="text-align:center;padding:4px;">${f.isReducible ? '✅' : '❌'}</td>
            <td style="text-align:center;padding:4px;">${f.hasGoTo ? '⚠️' : '—'}</td>
            <td style="text-align:center;padding:4px;">${f.hasSql ? '🗄️' : '—'}</td>
          </tr>`
        ).join('') + '</table>';
    }
  } catch (e) {
    console.error('Complexity load error:', e);
  }
}

function setStatValue(cardId, value) {
  const card = document.getElementById(cardId);
  if (card) {
    const val = card.querySelector('.stat-value');
    if (val) val.textContent = value;
  }
}
