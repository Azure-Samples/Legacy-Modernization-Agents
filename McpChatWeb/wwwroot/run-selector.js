// Run Selector - Allows users to switch between migration runs
let currentRunId = null;
let availableRuns = [];

// Initialize run selector
async function initRunSelector() {
  const runSelector = document.getElementById('run-selector');
  const refreshRunsBtn = document.getElementById('refresh-runs-btn');
  const currentRunIdSpan = document.getElementById('current-run-id');

  if (!runSelector) return;

  // Load available runs
  await loadAvailableRuns();

  // Set up event listeners
  runSelector.addEventListener('change', async (e) => {
    const selectedRunId = parseInt(e.target.value);
    if (selectedRunId && selectedRunId !== currentRunId) {
      await switchToRun(selectedRunId);
    }
  });

  refreshRunsBtn?.addEventListener('click', async () => {
    refreshRunsBtn.disabled = true;
    refreshRunsBtn.textContent = '⟳';
    await loadAvailableRuns();
    refreshRunsBtn.disabled = false;
  });
}

// Load all available migration runs from the API
async function loadAvailableRuns() {
  const runSelector = document.getElementById('run-selector');
  
  try {
    // Fetch all runs from the API
    const response = await fetch('/api/runs/all');
    if (!response.ok) {
      throw new Error(`HTTP ${response.status}`);
    }
    
    const data = await response.json();
    availableRuns = data.runs || [];
    
    // Get current run info
    const currentRunResponse = await fetch('/api/runinfo');
    if (currentRunResponse.ok) {
      const currentRunInfo = await currentRunResponse.json();
      currentRunId = currentRunInfo.runId;
    }
    
    // Populate the dropdown
    populateRunSelector(availableRuns, currentRunId);
    
  } catch (error) {
    console.error('Failed to load runs:', error);
    runSelector.innerHTML = '<option value="">Error loading runs</option>';
  }
}

// Populate the run selector dropdown
function populateRunSelector(runs, selectedRunId) {
  const runSelector = document.getElementById('run-selector');
  
  if (!runs || runs.length === 0) {
    runSelector.innerHTML = '<option value="">No runs available</option>';
    return;
  }
  
  // Sort runs by ID descending (most recent first)
  const sortedRuns = [...runs].sort((a, b) => b - a);
  
  runSelector.innerHTML = '';
  
  sortedRuns.forEach(runId => {
    const option = document.createElement('option');
    option.value = runId;
    option.textContent = `Run ${runId}`;
    
    if (runId === selectedRunId) {
      option.selected = true;
      option.textContent += ' (Current)';
    }
    
    runSelector.appendChild(option);
  });
  
  // Update graph title with current run
  if (selectedRunId) {
    updateGraphTitle(selectedRunId);
  }
}

// Update the graph title badge with current run
function updateGraphTitle(runId) {
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

// Switch to a different migration run
async function switchToRun(newRunId) {
  const currentRunIdSpan = document.getElementById('current-run-id');
  const responseCard = document.getElementById('response');
  const responseBody = document.getElementById('response-body');
  
  try {
    // Show loading indicator
    if (currentRunIdSpan) {
      currentRunIdSpan.textContent = `Switching to ${newRunId}...`;
    }
    
    // Call the API to switch runs (you'll need to implement this endpoint)
    const response = await fetch('/api/switch-run', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({ runId: newRunId })
    });
    
    if (!response.ok) {
      throw new Error(`Failed to switch run: HTTP ${response.status}`);
    }
    
    // Update current run
    currentRunId = newRunId;
    
    // Update graph title
    updateGraphTitle(newRunId);
    
    // Show success message
    if (responseCard && responseBody) {
      responseCard.hidden = false;
      responseBody.textContent = `✅ Switched to Run ${newRunId}\n\nYou can now query data from this migration run.\n\nNote: If runs analyzed the same COBOL files, the dependency graph will be identical.`;
    }
    
    // Reload resources and graph
    if (typeof fetchResources === 'function') {
      await fetchResources();
    }
    
    if (typeof loadDependencyGraph === 'function') {
      await loadDependencyGraph();
    }
    
    // Reload the dependency graph if the graph object exists
    if (window.dependencyGraph && typeof window.dependencyGraph.loadAndRender === 'function') {
      console.log(`🔄 Reloading graph for Run ${newRunId}...`);
      window.dependencyGraph.runId = newRunId;
      window.dependencyGraph.updateGraphTitle(newRunId);
      await window.dependencyGraph.loadAndRender(newRunId);
      console.log(`✅ Graph reloaded for Run ${newRunId}`);
    } else {
      console.warn('⚠️ Dependency graph not available yet');
    }
    
    console.log(`Switched to run ${newRunId}`);
    
  } catch (error) {
    console.error('Failed to switch run:', error);
    
    if (responseCard && responseBody) {
      responseCard.hidden = false;
      responseBody.textContent = `❌ Failed to switch to Run ${newRunId}\n\nError: ${error.message}`;
    }
    
    // Revert selector to current run
    const runSelector = document.getElementById('run-selector');
    if (runSelector && currentRunId) {
      runSelector.value = currentRunId;
    }
  }
}

// Initialize when DOM is ready
if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', initRunSelector);
} else {
  initRunSelector();
}
