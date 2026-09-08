// Dashboard Tab Switcher — controls which dashboard panel is visible
// Manages tab state, lazy-loading of dashboard data, and CSS transitions.

let servicesView = null;

// Rekt scan run selector
let _currentScanRunId = 'latest';
window.getSelectedScanRunId = function () { return _currentScanRunId; };

// Populate scan run dropdown on load
async function loadScanRuns() {
  try {
    const resp = await fetch('/api/graph/rekt/runs');
    if (!resp.ok) return;
    const payload = await resp.json();
    const runs = payload.runs || [];
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
    const meaningful = runs.filter(r => r.fileCount > 2).slice(0, 15);
    for (const run of meaningful) {
      const opt = document.createElement('option');
      opt.value = run.runId;
      opt.textContent = `Run ${run.runId} (${run.fileCount} files)`;
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
  document.querySelectorAll('.dashboard-tab').forEach(btn => {
    btn.classList.toggle('active', btn.dataset.tab === tabName);
  });

  const panels = {
    dependency: ['dependency-graph', 'graph-toolbar'],
    services: ['services-container'],
    modernization: ['modernization-intelligence-container'],
  };

  Object.values(panels).flat().forEach(id => {
    const el = document.getElementById(id);
    if (el) el.style.display = 'none';
  });

  const toolbar = document.querySelector('.graph-toolbar');
  if (toolbar) toolbar.style.display = tabName === 'dependency' ? '' : 'none';

  (panels[tabName] || []).forEach(id => {
    const el = document.getElementById(id);
    if (el) el.style.display = '';
  });

  // Lazy-init renderers
  if (tabName === 'services') {
    if (!servicesView) {
      servicesView = new ServicesView('services-content');
      window.servicesView = servicesView;
    }
    servicesView.loadAndRender();
  }

  if (tabName === 'modernization') {
    if (!window.modernizationIntelligenceView) {
      window.modernizationIntelligenceView =
        new ModernizationIntelligenceView('modernization-intelligence-root');
    }
    window.modernizationIntelligenceView.loadAndRender();
  }
}
