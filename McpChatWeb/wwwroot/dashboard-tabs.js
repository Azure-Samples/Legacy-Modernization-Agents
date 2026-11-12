// Dashboard Tab Switcher — controls which dashboard panel is visible
// Manages tab state, lazy-loading of dashboard data, and CSS transitions.

let servicesView = null;

// Rekt scan run selector
let _currentScanRunId = 'latest';
let _latestRunId = null;
let _scanRuns = [];

// 'latest' resolves to a concrete run id so it differs from 'all', which stays unfiltered
// and lets the backend deduplicate to the newest node per file across every run.
window.getSelectedScanRunId = function () {
  if (_currentScanRunId === 'all') return 'all';
  if (_currentScanRunId === 'latest') return _latestRunId ?? 'all';
  return _currentScanRunId;
};

window.getSelectedScanRun = function () {
  const id = window.getSelectedScanRunId();
  return _scanRuns.find(run => String(run.runId) === String(id)) || null;
};

// Populate scan run dropdown on load
async function loadScanRuns() {
  try {
    const resp = await fetch('/api/graph/rekt/runs');
    if (!resp.ok) return;
    const payload = await resp.json();
    const runs = payload.runs || [];
    _scanRuns = runs;
    const select = document.getElementById('rekt-scan-select');
    if (!select) return;

    // The REKT graph is optional. Say so rather than leaving an inert dropdown.
    if (payload.note) {
      select.title = payload.note;
      select.disabled = true;
      return;
    }
    select.disabled = false;
    select.title = '';
    if (runs.length === 0) return;

    // Keep the first two options (Latest, All)
    select.querySelectorAll('option').forEach((opt, i) => { if (i > 1) opt.remove(); });

    // A run covering one or two files is a re-scan of a single program, not a
    // scan of the estate, and picking one would silently empty every view.
    const meaningful = runs.filter(r => r.fileCount > 2 || r.name).slice(0, 15);
    // "Latest" follows the newest COBOL AST run, as the server does; a JCL-only
    // scan stays selectable explicitly without emptying every COBOL view.
    const withAst = meaningful.filter(r => r.fileCount > 0);
    _latestRunId = withAst.length > 0
      ? withAst.reduce((max, r) => (r.runId > max ? r.runId : max), withAst[0].runId)
      : null;
    for (const run of meaningful) {
      const opt = document.createElement('option');
      opt.value = run.runId;
      const jobs = run.jclJobCount > 0 ? `, ${run.jclJobCount} JCL job${run.jclJobCount === 1 ? '' : 's'}` : '';
      opt.textContent = `${run.name || `Run ${run.runId}`} (${run.fileCount} file${run.fileCount === 1 ? '' : 's'}${jobs})`;
      if (run.ingestionStatus && run.ingestionStatus !== 'complete')
        opt.textContent += ` [${run.ingestionStatus}]`;
      select.appendChild(opt);
    }
  } catch (e) { console.error('Failed to load scan runs:', e); }
}

window._onScanRunChange = function (value) {
  const prev = _currentScanRunId;
  _currentScanRunId = value;
  if (prev !== value && servicesView) {
    // Drop the cached projection so the next render re-reads the chosen run.
    servicesView.architecture = null;
  }
  const activeTab = document.querySelector('.dashboard-tab.active');
  if (activeTab) switchDashboard(activeTab.dataset.tab);
};

document.addEventListener('DOMContentLoaded', loadScanRuns);

function switchDashboard(tabName) {
  if (tabName !== 'ailoop') window.aiLoopView?.deactivate();
  document.querySelectorAll('.dashboard-tab').forEach(btn => {
    btn.classList.toggle('active', btn.dataset.tab === tabName);
  });

  const panels = {
    dependency: ['dependency-graph', 'graph-toolbar', 'graph-info', 'node-details'],
    services: ['services-container'],
    modernization: ['modernization-intelligence-container'],
    estate: ['estate-mission-control-container'],
    ailoop: ['ai-loop-container'],
    ast: ['ast-explorer-container'],
    programs: ['program-picker-container'],
    missing: ['missing-copybooks-container'],
    explorer: ['program-explorer-container'],
  };

  Object.values(panels).flat().forEach(id => {
    const el = document.getElementById(id);
    if (el) el.style.display = 'none';
  });

  const toolbar = document.querySelector('.graph-toolbar');
  if (toolbar) toolbar.style.display = tabName === 'dependency' ? '' : 'none';

  // AST exploration binds one snapshot, including its source and detail requests.
  const scanPicker = document.getElementById('rekt-scan-picker');
  if (scanPicker) {
    scanPicker.style.visibility = ['services', 'ast'].includes(tabName) ? '' : 'hidden';
    const allOption = scanPicker.querySelector('option[value="all"]');
    if (allOption) allOption.disabled = tabName === 'ast';
    if (tabName === 'ast' && _currentScanRunId === 'all') {
      _currentScanRunId = 'latest';
      document.getElementById('rekt-scan-select').value = 'latest';
      if (servicesView) servicesView.architecture = null;
    }
  }

  (panels[tabName] || []).forEach(id => {
    const el = document.getElementById(id);
    if (el) el.style.display = id === 'ast-explorer-container' ? 'flex' : '';
  });

  // Lazy-init renderers
  if (tabName === 'services') {
    if (!servicesView) {
      servicesView = new ServicesView('services-content');
      window.servicesView = servicesView;
    }
    servicesView.loadAndRender();
  }

  if (tabName === 'ast') {
    if (!window.astExplorer) {
      window.astExplorer = new ASTExplorer('ast-graph');
    }
    window.astExplorer.loadFileList();
  }

  if (tabName === 'programs') {
    if (!window.programPickerView) {
      window.programPickerView = new ProgramPickerView('program-picker-root');
    }
    window.programPickerView.loadAndRender();
  }

  if (tabName === 'missing') {
    if (!window.missingCopybooksView) {
      window.missingCopybooksView = new MissingCopybooksView('missing-copybooks-root');
    }
    window.missingCopybooksView.loadAndRender();
  }

  if (tabName === 'explorer') {
    if (!window.programExplorerView) {
      window.programExplorerView = new ProgramExplorerView('program-explorer-root');
    }
    window.programExplorerView.loadAndRender();
  }

  // The estate graph needs the room of the expanded panel; the tab expands it and gives it back on leave.
  const graphPanel = document.querySelector('.graph-panel');
  if (graphPanel && tabName !== 'estate' && graphPanel.dataset.estateExpanded) {
    graphPanel.classList.remove('expanded');
    delete graphPanel.dataset.estateExpanded;
  }

  if (tabName === 'estate') {
    if (graphPanel && !graphPanel.classList.contains('expanded')) {
      graphPanel.classList.add('expanded');
      graphPanel.dataset.estateExpanded = '1';
    }
    if (!window.estateMissionControlView) {
      window.estateMissionControlView = new EstateMissionControlView('estate-mission-control-root');
    }
    window.estateMissionControlView.loadAndRender();
  }

  if (tabName === 'ailoop') {
    if (!window.aiLoopView) window.aiLoopView = new AiLoopView('ai-loop-root');
    window.aiLoopView.loadAndRender();
  }

  if (tabName === 'modernization') {
    if (!window.modernizationIntelligenceView) {
      window.modernizationIntelligenceView =
        new ModernizationIntelligenceView('modernization-intelligence-root');
    }
    window.modernizationIntelligenceView.loadAndRender();
  }
}
