// Run Selector - Allows users to switch between migration runs
let currentRunId = null;
let availableRuns = [];
let runSelectorRefreshInterval = null;
const RUN_SELECTOR_REFRESH_MS = 3000; // Refresh every 3 seconds for faster detection of new runs

// Initialize run selector
async function initRunSelector() {
  const runSelector = document.getElementById('run-selector');
  const refreshRunsBtn = document.getElementById('refresh-runs-btn');
  const currentRunIdSpan = document.getElementById('current-run-id');

  if (!runSelector) return;

  // Show loading state immediately
  runSelector.innerHTML = '<option value="">Loading runs...</option>';

  // Load available runs and AUTO-SELECT LATEST (non-blocking)
  await loadAvailableRuns(false, true); // forceSelectLatest = true on initial load

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
    await loadAvailableRuns(false, false); // DON'T force select - just refresh the list
    refreshRunsBtn.disabled = false;
  });

  // "Load Latest" button - jumps to most recent/running job
  const loadLatestBtn = document.getElementById('load-latest-btn');
  loadLatestBtn?.addEventListener('click', async () => {
    loadLatestBtn.disabled = true;
    loadLatestBtn.textContent = 'Loading...';
    await loadAvailableRuns(false, true); // forceSelectLatest = true (user explicitly wants latest)
    loadLatestBtn.textContent = '🔄 Latest';
    loadLatestBtn.disabled = false;
  });

  // Start periodic refresh
  startRunSelectorRefresh();
}

// Start periodic refresh of the run selector
function startRunSelectorRefresh() {
  if (runSelectorRefreshInterval) {
    clearInterval(runSelectorRefreshInterval);
  }
  runSelectorRefreshInterval = setInterval(async () => {
    if (window.pageIsVisible !== false) await loadAvailableRuns(true); // silent refresh
  }, RUN_SELECTOR_REFRESH_MS);
}

// Stop periodic refresh
function stopRunSelectorRefresh() {
  if (runSelectorRefreshInterval) {
    clearInterval(runSelectorRefreshInterval);
    runSelectorRefreshInterval = null;
  }
}

// Load all available migration runs from the API
async function loadAvailableRuns(silent = false, forceSelectLatest = false) {
  const runSelector = document.getElementById('run-selector');
  
  try {
    // Fetch all runs from the API
    const response = await fetch('/api/runs/all');
    if (!response.ok) {
      throw new Error(`HTTP ${response.status}`);
    }
    
    const data = await response.json();
    const newRuns = normalizeRuns(data.runsDetailed || data.runs);
    
    // Find the latest (highest ID) run
    const latestRunId = newRuns.length > 0 
      ? Math.max(...newRuns.map(r => r.id))
      : null;
    
    // Find any running job (for display purposes only)
    const runningJob = newRuns.find(r => 
      r.status && (r.status.toLowerCase() === 'running' || r.status.toLowerCase() === 'in progress')
    );
    
    // Determine which run to select:
    // ONLY auto-select on initial page load (forceSelectLatest=true) or when no current selection
    // Otherwise, ALWAYS preserve user's selection
    let targetRunId = currentRunId;
    
    if (forceSelectLatest) {
      // User explicitly wants latest (via "Load Latest" button or initial page load)
      // Prefer running job, fallback to latest
      if (runningJob) {
        targetRunId = runningJob.id;
        console.log(`🔄 Selecting running job: Run ${runningJob.id}`);
      } else {
        targetRunId = latestRunId;
        console.log(`📋 Selecting latest run: Run ${latestRunId}`);
      }
    } else if (!currentRunId && latestRunId) {
      // No current selection - select latest as fallback
      targetRunId = latestRunId;
      console.log(`📋 No selection, defaulting to latest: Run ${latestRunId}`);
    }
    // Otherwise keep currentRunId (user's selection)
    
    // Check if we have new runs (for notification purposes only, don't auto-switch)
    const newRunDetected = newRuns.some(r => !availableRuns.some(a => a.id === r.id));

    availableRuns = newRuns;
    
    // Populate the dropdown (preserves current selection)
    populateRunSelector(availableRuns, targetRunId || currentRunId);
    
    // Notify about new runs but DON'T auto-switch (user controls selection)
    if (silent && newRunDetected) {
      // Just dispatch event for notification purposes - don't change selection
      document.dispatchEvent(new CustomEvent('newRunDetected', { 
        detail: { runId: latestRunId, runs: newRuns, autoSwitch: false } 
      }));
      console.log(`📣 New run detected: Run ${latestRunId} (not auto-switching)`);
    }
    
    // Only update currentRunId if we explicitly want to switch (forceSelectLatest)
    if (targetRunId && targetRunId !== currentRunId) {
      currentRunId = targetRunId;
      if (runSelector) {
        runSelector.value = targetRunId;
      }
      // Update header badges
      await updateRunBadges(targetRunId);
    }
    
  } catch (error) {
    console.error('Failed to load runs:', error);
    if (!silent) {
      runSelector.innerHTML = '<option value="">Error loading runs</option>';
    }
  }
}

// Normalize run payloads from API (supports legacy numeric-only shape)
function normalizeRuns(rawRuns) {
  return (rawRuns || []).map(r => {
    if (typeof r === 'number') {
      return { id: r, status: 'Unknown' };
    }
    return { id: r.id, status: r.status || 'Unknown' };
  });
}

// Update the header run badges with current status
async function updateRunBadges(runId) {
  const runIdSpan = document.getElementById('current-run-id');
  const statusBadge = document.getElementById('run-status-badge');
  const targetBadge = document.getElementById('run-target-badge');
  
  if (runIdSpan) {
    runIdSpan.textContent = runId || '--';
  }
  
  if (!runId) {
    if (statusBadge) {
      statusBadge.textContent = '⏸️ No Run';
      statusBadge.className = 'run-status-badge status-idle';
    }
    if (targetBadge) {
      targetBadge.textContent = '🎯 --';
      targetBadge.className = 'run-target-badge';
    }
    return;
  }
  
  try {
    // Fetch migration log which has run status and target language
    const response = await fetch(`/api/runs/${runId}/migration-log?lines=1`);
    if (response.ok) {
      const data = await response.json();
      
      // Update status badge
      if (statusBadge) {
        const status = data.runStatus || 'Unknown';
        let statusIcon = '⏸️';
        let statusClass = 'status-idle';
        
        if (status === 'Running' || status === 'In Progress') {
          statusIcon = '🔄';
          statusClass = 'status-running';
        } else if (status === 'Completed') {
          statusIcon = '✅';
          statusClass = 'status-completed';
        } else if (status === 'Failed' || status === 'Cancelled') {
          statusIcon = '❌';
          statusClass = 'status-failed';
        }
        
        statusBadge.textContent = `${statusIcon} ${status}`;
        statusBadge.className = `run-status-badge ${statusClass}`;
      }
      
      // Update target language badge
      if (targetBadge) {
        const lang = (data.targetLanguage || 'unknown').toLowerCase();
        const langIcon = lang === 'java' ? '☕' : '⚙️';
        const langDisplay = lang === 'csharp' ? 'C#' : lang.charAt(0).toUpperCase() + lang.slice(1);
        targetBadge.textContent = `${langIcon} ${langDisplay}`;
        targetBadge.className = `run-target-badge ${lang}`;
      }
    }
  } catch (error) {
    console.error('Failed to update run badges:', error);
  }
}

// Make updateRunBadges globally accessible for migration-monitor.js
window.updateRunBadges = updateRunBadges;

// Populate the run selector dropdown
function populateRunSelector(runs, selectedRunId) {
  const runSelector = document.getElementById('run-selector');
  
  if (!runs || runs.length === 0) {
    runSelector.innerHTML = '<option value="">No runs available</option>';
    return;
  }
  
  // Sort runs by ID descending (most recent first)
  const sortedRuns = [...runs].sort((a, b) => b.id - a.id);
  
  runSelector.innerHTML = '';
  
  sortedRuns.forEach(run => {
    const option = document.createElement('option');
    option.value = run.id;

    const status = (run.status || '').toLowerCase();
    const isFailed = status.includes('fail') || status.includes('cancel');
    const statusLabel = status ? ` (${run.status})` : '';
    option.textContent = `${isFailed ? '❌ ' : ''}Run ${run.id}${statusLabel}`;
    
    if (run.id === selectedRunId) {
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
  const currentRunBadge = document.getElementById('current-run-badge');
  
  if (currentRunIdSpan && runId) {
    currentRunIdSpan.textContent = runId;
  }
  
  if (currentRunBadge) {
    currentRunBadge.style.display = 'inline-flex';
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
    
    // Update header badges with new run status
    await updateRunBadges(newRunId);
    
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

// Update run status badge - can be called from migration monitor
function updateRunStatusBadge(status) {
  const badge = document.getElementById('run-status-badge');
  if (!badge) return;
  
  // Remove all status classes
  badge.classList.remove('running', 'completed', 'failed');
  
  const statusLower = (status || '').toLowerCase();
  
  if (statusLower.includes('running') || statusLower.includes('progress') || statusLower.includes('analyzing')) {
    badge.textContent = '🔄 Running';
    badge.classList.add('running');
  } else if (statusLower.includes('complete') || statusLower.includes('success')) {
    badge.textContent = '✅ Completed';
    badge.classList.add('completed');
  } else if (statusLower.includes('fail') || statusLower.includes('error')) {
    badge.textContent = '❌ Failed';
    badge.classList.add('failed');
  } else if (statusLower.includes('cancel')) {
    badge.textContent = '⏹️ Cancelled';
    badge.classList.add('failed');
  } else {
    badge.textContent = '⏸️ Idle';
  }
}

// Export functions for global access
window.updateRunStatusBadge = updateRunStatusBadge;
window.loadAvailableRuns = loadAvailableRuns;
