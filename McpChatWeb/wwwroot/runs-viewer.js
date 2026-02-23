// All Runs & Data Guide Modal Handler

const modal = document.getElementById('allRunsModal');
const btn = document.getElementById('showAllRunsBtn');
const span = document.getElementsByClassName('close')[0];
let runsPollingInterval = null;

// Open modal
btn.onclick = function() {
  modal.style.display = 'block';
  loadAllRuns();
  startRunsPolling();
};

// Close modal
span.onclick = function() {
  modal.style.display = 'none';
  stopRunsPolling();
};

// Close on outside click
window.onclick = function(event) {
  if (event.target == modal) {
    modal.style.display = 'none';
    stopRunsPolling();
  }
};

function startRunsPolling() {
  if (runsPollingInterval) clearInterval(runsPollingInterval);
  runsPollingInterval = setInterval(() => {
    if (modal.style.display === 'block') {
      loadAllRuns(true); // silent refresh
    }
  }, 10000);
}

function stopRunsPolling() {
  if (runsPollingInterval) {
    clearInterval(runsPollingInterval);
    runsPollingInterval = null;
  }
}

// Tab switching
document.querySelectorAll('.tab-button').forEach(button => {
  button.addEventListener('click', () => {
    const tabName = button.getAttribute('data-tab');
    
    // Hide all tab contents
    document.querySelectorAll('.tab-content').forEach(content => {
      content.classList.remove('active');
    });
    
    // Remove active class from all buttons
    document.querySelectorAll('.tab-button').forEach(btn => {
      btn.classList.remove('active');
    });
    
    // Show selected tab
    document.getElementById(tabName + 'Tab').classList.add('active');
    button.classList.add('active');
  });
});

// Load all runs from API
async function loadAllRuns(silent = false) {
  try {
    const response = await fetch('/api/runs/all');
    const data = await response.json();
    
    const runsList = document.getElementById('runsList');
    if (!silent) runsList.innerHTML = '';
    
    const runsPayload = data.runsDetailed || data.runs || [];

    if (runsPayload.length > 0) {
      if (!silent) runsList.innerHTML = '<p>Click on any run to view its dependencies:</p>';
      
      // If silent, compare logic could be added here to avoid full redraw
      // For now, full redraw is simple and effective for small lists
      const tempContainer = document.createElement('div');
      
      runsPayload.forEach(run => {
        const runId = run.id ?? run; // support legacy numeric shape
        const status = run.status || 'Unknown';
        const runCard = document.createElement('div');
        runCard.className = 'run-card';
        runCard.innerHTML = `
          <div class="run-header">
            <h4>🔹 Run ${runId} (${status})</h4>
            <div style="display: flex; gap: 0.5rem;">
              <button onclick="loadRunDetails(${runId})" class="load-btn">View Dependencies</button>
              <button onclick="loadReDetails(${runId})" class="load-btn" style="background: rgba(99, 102, 241, 0.2); border-color: rgba(99, 102, 241, 0.4); color: #818cf8;">🔬 RE Results</button>
              <button onclick="generateRunReport(${runId})" class="load-btn" style="background: rgba(16, 185, 129, 0.2); border-color: rgba(16, 185, 129, 0.4); color: #10b981;">📄 Generate Report</button>
            </div>
          </div>
          <div id="run-${runId}-details" class="run-details"></div>
          <div id="run-${runId}-re" class="run-details"></div>
        `;
        tempContainer.appendChild(runCard);
      });
      
      runsList.innerHTML = tempContainer.innerHTML;
    } else {
      runsList.innerHTML = '<p>No migration runs found.</p>';
    }
  } catch (error) {
    console.error('Error loading runs:', error);
    if (!silent) document.getElementById('runsList').innerHTML = '<p class="error">Error loading runs.</p>';
  }
}

// Load details for specific run
async function loadRunDetails(runId) {
  const detailsDiv = document.getElementById(`run-${runId}-details`);
  detailsDiv.innerHTML = '<p class="loading">⏳ Loading dependencies...</p>';
  
  try {
    const response = await fetch(`/api/runs/${runId}/dependencies`);
    const data = await response.json();
    
    if (data.error) {
      detailsDiv.innerHTML = `<p class="error">❌ ${data.error}</p>`;
      return;
    }
    
    const stats = `
      <div class="run-stats">
        <div class="stat-item">
          <span class="stat-label">Total Nodes:</span>
          <span class="stat-value">${data.nodeCount}</span>
        </div>
        <div class="stat-item">
          <span class="stat-label">Dependencies:</span>
          <span class="stat-value">${data.edgeCount}</span>
        </div>
      </div>
    `;
    
    let filesBreakdown = '';
    if (data.graphData && data.graphData.nodes) {
      const programs = data.graphData.nodes.filter(n => !n.isCopybook).length;
      const copybooks = data.graphData.nodes.filter(n => n.isCopybook).length;
      
      filesBreakdown = `
        <div class="files-breakdown">
          <div class="file-type">
            <span class="file-icon" style="color: #68bdf6;">▪</span>
            <span>${programs} COBOL Programs</span>
          </div>
          <div class="file-type">
            <span class="file-icon" style="color: #f16667;">▪</span>
            <span>${copybooks} Copybooks</span>
          </div>
        </div>
      `;
    }
    
    const actions = `
      <div class="run-actions">
        <button onclick="viewRunInGraph(${runId})" class="action-btn">📊 View in Graph</button>
        <button onclick="downloadRunData(${runId})" class="action-btn">💾 Download JSON</button>
      </div>
    `;
    
    detailsDiv.innerHTML = stats + filesBreakdown + actions;
    detailsDiv.classList.add('loaded');
    
  } catch (error) {
    console.error(`Error loading run ${runId}:`, error);
    detailsDiv.innerHTML = '<p class="error">❌ Error loading dependencies</p>';
  }
}

// View run in main graph visualization
function viewRunInGraph(runId) {
  modal.style.display = 'none';
  // TODO: Update main graph to load this specific run
  alert(`Graph view for Run ${runId} - This would switch the main graph to display Run ${runId}'s dependencies.`);
}

// Download run data as JSON
async function downloadRunData(runId) {
  try {
    const response = await fetch(`/api/runs/${runId}/dependencies`);
    const data = await response.json();
    
    const blob = new Blob([JSON.stringify(data, null, 2)], { type: 'application/json' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = `run-${runId}-dependencies.json`;
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    URL.revokeObjectURL(url);
  } catch (error) {
    console.error(`Error downloading run ${runId} data:`, error);
    alert('Error downloading data');
  }
}

// Make functions global
window.loadRunDetails = loadRunDetails;
window.viewRunInGraph = viewRunInGraph;
window.downloadRunData = downloadRunData;
window.loadReDetails = loadReDetails;
window.deleteReResult = deleteReResult;

// Load reverse engineering business logic summary for a run
async function loadReDetails(runId) {
  const div = document.getElementById(`run-${runId}-re`);
  div.innerHTML = '<p class="loading">⏳ Loading RE results...</p>';

  try {
    const response = await fetch(`/api/runs/${runId}/business-logic`);
    const data = await response.json();

    if (data.error) {
      div.innerHTML = `<p class="error">❌ ${data.error}</p>`;
      return;
    }

    if (!data.files || data.files.length === 0) {
      div.innerHTML = `
        <div class="run-stats" style="border-top: 1px solid rgba(99,102,241,0.2); margin-top: 0.5rem; padding-top: 0.75rem;">
          <p style="color: #94a3b8; margin: 0;">⚠️ No persisted business logic found for Run ${runId}.</p>
          <p style="color: #64748b; font-size: 0.8rem; margin: 0.25rem 0 0;">Run reverse engineering first, or it may have been deleted.</p>
        </div>`;
      return;
    }

    const programs = data.files.filter(f => !f.isCopybook);
    const copybooks = data.files.filter(f => f.isCopybook);
    const totalStories  = data.files.reduce((s, f) => s + f.storyCount, 0);
    const totalFeatures = data.files.reduce((s, f) => s + f.featureCount, 0);
    const totalRules    = data.files.reduce((s, f) => s + f.ruleCount, 0);
    const rawCreated = data.files[0]?.createdAt;
    const firstCreated = rawCreated
      ? new Date(rawCreated.replace(' ', 'T')).toLocaleString()
      : 'unknown';

    const fileRows = data.files.map(f => `
      <tr>
        <td style="padding: 0.3rem 0.5rem;">
          <div style="color: ${f.isCopybook ? '#f16667' : '#68bdf6'}; font-size: 0.78rem;">${f.fileName}</div>
          ${f.businessPurpose ? `<div style="color: #64748b; font-size: 0.72rem; margin-top: 2px; line-height: 1.3;">${f.businessPurpose}</div>` : ''}
        </td>
        <td style="padding: 0.3rem 0.5rem; color: #94a3b8; text-align: center; vertical-align: top">${f.storyCount}</td>
        <td style="padding: 0.3rem 0.5rem; color: #94a3b8; text-align: center; vertical-align: top">${f.featureCount}</td>
        <td style="padding: 0.3rem 0.5rem; color: #94a3b8; text-align: center; vertical-align: top">${f.ruleCount}</td>
      </tr>`).join('');

    div.innerHTML = `
      <div style="border-top: 1px solid rgba(99,102,241,0.2); margin-top: 0.5rem; padding-top: 0.75rem;">
        <div style="display: flex; flex-wrap: wrap; gap: 6px; margin-bottom: 0.75rem;">
          ${[
            ['Programs',       programs.length],
            ['Copybooks',      copybooks.length],
            ['User Stories',   totalStories],
            ['Features',       totalFeatures],
            ['Business Rules', totalRules],
          ].map(([label, val]) => `
            <div style="background: rgba(99,102,241,0.08); border: 1px solid rgba(99,102,241,0.2); border-radius: 5px; padding: 5px 10px; display: flex; gap: 6px; align-items: center;">
              <span style="color: #94a3b8; font-size: 0.75rem;">${label}:</span>
              <span style="color: #818cf8; font-size: 0.9rem; font-weight: 600;">${val}</span>
            </div>`).join('')}
        </div>
        <p style="color: #64748b; font-size: 0.75rem; margin: 0 0 0.5rem;">Extracted: ${firstCreated}</p>
        <div style="max-height: 180px; overflow-y: auto; margin-bottom: 0.75rem;">
          <table style="width: 100%; font-size: 0.78rem; border-collapse: collapse;">
            <thead>
              <tr style="color: #64748b;">
                <th style="padding: 0.2rem 0.5rem; text-align: left">File</th>
                <th style="padding: 0.2rem 0.5rem; text-align: center">Stories</th>
                <th style="padding: 0.2rem 0.5rem; text-align: center">Features</th>
                <th style="padding: 0.2rem 0.5rem; text-align: center">Rules</th>
              </tr>
            </thead>
            <tbody>${fileRows}</tbody>
          </table>
        </div>
        <button onclick="deleteReResult(${runId})" style="background: rgba(239,68,68,0.15); border: 1px solid rgba(239,68,68,0.4); color: #f87171; padding: 0.35rem 0.75rem; border-radius: 4px; cursor: pointer; font-size: 0.8rem;">🗑️ Delete RE Result for Run ${runId}</button>
      </div>`;
  } catch (error) {
    console.error(`Error loading RE results for run ${runId}:`, error);
    div.innerHTML = '<p class="error">❌ Error loading RE results</p>';
  }
}

// Delete persisted business logic for a run
async function deleteReResult(runId) {
  if (!confirm(`Delete the reverse engineering result for Run ${runId}?\n\nThis allows you to re-run reverse engineering and replace it. The migration/conversion data for this run is not affected.`)) return;

  const div = document.getElementById(`run-${runId}-re`);
  div.innerHTML = '<p class="loading">⏳ Deleting...</p>';

  try {
    const response = await fetch(`/api/runs/${runId}/business-logic`, { method: 'DELETE' });
    const data = await response.json();
    if (data.success) {
      div.innerHTML = `<p style="color: #10b981; padding: 0.5rem 0;">✅ ${data.message}</p>`;
    } else {
      div.innerHTML = `<p class="error">❌ ${data.error || 'Unexpected error'}</p>`;
    }
  } catch (error) {
    console.error(`Error deleting RE result for run ${runId}:`, error);
    div.innerHTML = '<p class="error">❌ Error deleting RE result</p>';
  }
}

// Reverse Engineering Results Modal Handler
const archModal = document.getElementById('architectureModal');
const archBtn = document.getElementById('showArchitectureBtn');
const archClose = document.querySelector('.arch-close');

let reportMarkdown = '';

// Open modal and load report
if (archBtn) {
  archBtn.onclick = function() {
    archModal.style.display = 'block';
    if (!reportMarkdown) {
      loadReverseEngineeringReport();
    }
  };
}

// Close modal
if (archClose) {
  archClose.onclick = function() {
    archModal.style.display = 'none';
  };
}

// Close on outside click
window.onclick = function(event) {
  if (event.target == modal) {
    modal.style.display = 'none';
  }
  if (event.target == archModal) {
    archModal.style.display = 'none';
  }
};

// Generate migration report for a specific run
async function generateRunReport(runId) {
  const detailsDiv = document.getElementById(`run-${runId}-details`);
  detailsDiv.innerHTML = '<p class="loading">⏳ Generating migration report...</p>';
  
  try {
    const response = await fetch(`/api/runs/${runId}/report`);
    
    if (!response.ok) {
      throw new Error(`HTTP ${response.status}: ${response.statusText}`);
    }
    
    const contentType = response.headers.get('content-type');
    
    if (contentType && contentType.includes('application/json')) {
      // If JSON response, display the report content
      const data = await response.json();
      
      if (data.error) {
        detailsDiv.innerHTML = `<p class="error">❌ ${data.error}</p>`;
        return;
      }
      
      // Render the markdown report
      let reportHtml = '';
      if (typeof marked !== 'undefined' && data.content) {
        reportHtml = marked.parse(data.content);
      } else {
        reportHtml = `<pre>${escapeHtml(data.content || 'No report content available')}</pre>`;
      }
      
      detailsDiv.innerHTML = `
        <div class="report-container">
          <div class="report-header">
            <h4>📊 Migration Report - Run ${runId}</h4>
            <button onclick="downloadRunReport(${runId})" class="load-btn" style="font-size: 0.85rem; padding: 0.4rem 0.8rem;">📥 Download</button>
          </div>
          <div class="report-content" style="background: #0f172a; padding: 1.5rem; border-radius: 8px; max-height: 600px; overflow-y: auto;">
            ${reportHtml}
          </div>
        </div>
      `;
    } else {
      // If markdown file response, download it
      const blob = await response.blob();
      const url = URL.createObjectURL(blob);
      const a = document.createElement('a');
      a.href = url;
      a.download = `migration_report_run_${runId}.md`;
      document.body.appendChild(a);
      a.click();
      document.body.removeChild(a);
      URL.revokeObjectURL(url);
      
      detailsDiv.innerHTML = `<p style="color: #10b981;">✅ Report downloaded successfully!</p>`;
    }
  } catch (error) {
    console.error('Error generating report:', error);
    detailsDiv.innerHTML = `<p class="error">❌ Failed to generate report: ${error.message}</p>`;
  }
}

// Download run report as markdown file
async function downloadRunReport(runId) {
  try {
    const response = await fetch(`/api/runs/${runId}/report`);
    
    if (!response.ok) {
      throw new Error(`HTTP ${response.status}`);
    }
    
    const data = await response.json();
    
    if (data.content) {
      const blob = new Blob([data.content], { type: 'text/markdown' });
      const url = URL.createObjectURL(blob);
      const a = document.createElement('a');
      a.href = url;
      a.download = `migration_report_run_${runId}.md`;
      document.body.appendChild(a);
      a.click();
      document.body.removeChild(a);
      URL.revokeObjectURL(url);
    }
  } catch (error) {
    console.error('Error downloading report:', error);
    alert(`Failed to download report: ${error.message}`);
  }
}

// Load and render reverse engineering report
async function loadReverseEngineeringReport() {
  const contentDiv = document.getElementById('reportContent');
  const lastModifiedSpan = document.getElementById('reportLastModified');
  
  try {
    contentDiv.innerHTML = '<p style="text-align: center; color: #94a3b8;"><em>Loading reverse engineering report...</em></p>';
    
    const response = await fetch('/api/documentation/reverse-engineering-report');
    if (!response.ok) {
      if (response.status === 404) {
        const errorData = await response.json();
        contentDiv.innerHTML = `
          <div style="text-align: center; padding: 2rem; color: #94a3b8;">
            <p style="font-size: 3rem; margin-bottom: 1rem;">📋</p>
            <h3 style="color: #f1f5f9; margin-bottom: 0.5rem;">No Report Available</h3>
            <p>${errorData.hint || 'Run reverse engineering first to generate the report'}</p>
            <p style="font-size: 0.85rem; margin-top: 1rem; color: #64748b;">Expected location: output/reverse-engineering-details.md</p>
          </div>`;
        return;
      }
      throw new Error(`HTTP ${response.status}`);
    }
    
    const data = await response.json();
    reportMarkdown = data.content;
    
    // Render markdown using marked.js
    if (typeof marked !== 'undefined') {
      marked.setOptions({
        breaks: true,
        gfm: true,
        headerIds: true,
        mangle: false
      });
      
      contentDiv.innerHTML = marked.parse(reportMarkdown);
    } else {
      contentDiv.innerHTML = `<pre>${escapeHtml(reportMarkdown)}</pre>`;
    }
    
    // Update last modified and size
    if (lastModifiedSpan && data.lastModified) {
      const date = new Date(data.lastModified);
      const sizeKB = data.sizeBytes ? Math.round(data.sizeBytes / 1024) : 0;
      lastModifiedSpan.textContent = `Last updated: ${date.toLocaleDateString()} ${date.toLocaleTimeString()} (${sizeKB} KB)`;
    }
  } catch (error) {
    console.error('Error loading reverse engineering report:', error);
    contentDiv.innerHTML = `<p style="color: #ef4444;">Failed to load report: ${error.message}</p>`;
  }
}

// Download reverse engineering report
document.getElementById('downloadReportBtn')?.addEventListener('click', () => {
  if (!reportMarkdown) return;
  
  const blob = new Blob([reportMarkdown], { type: 'text/markdown' });
  const url = URL.createObjectURL(blob);
  const a = document.createElement('a');
  a.href = url;
  a.download = 'reverse-engineering-details.md';
  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);
  URL.revokeObjectURL(url);
});

// Copy report to clipboard
document.getElementById('copyReportBtn')?.addEventListener('click', async () => {
  if (!reportMarkdown) return;
  
  try {
    await navigator.clipboard.writeText(reportMarkdown);
    
    const btn = document.getElementById('copyReportBtn');
    const originalText = btn.textContent;
    btn.textContent = '✅ Copied!';
    btn.style.background = 'rgba(16, 185, 129, 0.2)';
    btn.style.borderColor = 'rgba(16, 185, 129, 0.5)';
    
    setTimeout(() => {
      btn.textContent = originalText;
      btn.style.background = '';
      btn.style.borderColor = '';
    }, 2000);
  } catch (error) {
    console.error('Failed to copy:', error);
    alert('Failed to copy to clipboard');
  }
});

// Refresh report button
document.getElementById('refreshReportBtn')?.addEventListener('click', () => {
  reportMarkdown = ''; // Clear cache
  loadReverseEngineeringReport();
});

// Helper function to escape HTML
function escapeHtml(text) {
  const div = document.createElement('div');
  div.textContent = text;
  return div.innerHTML;
}
