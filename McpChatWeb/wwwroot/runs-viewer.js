// All Runs & Data Guide Modal Handler

const modal = document.getElementById('allRunsModal');
const btn = document.getElementById('showAllRunsBtn');
const span = document.getElementsByClassName('close')[0];

// Open modal
btn.onclick = function() {
  modal.style.display = 'block';
  loadAllRuns();
};

// Close modal
span.onclick = function() {
  modal.style.display = 'none';
};

// Close on outside click
window.onclick = function(event) {
  if (event.target == modal) {
    modal.style.display = 'none';
  }
};

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
async function loadAllRuns() {
  try {
    const response = await fetch('/api/runs/all');
    const data = await response.json();
    
    const runsList = document.getElementById('runsList');
    runsList.innerHTML = '';
    
    if (data.runs && data.runs.length > 0) {
      runsList.innerHTML = '<p>Click on any run to view its dependencies:</p>';
      
      data.runs.forEach(runId => {
        const runCard = document.createElement('div');
        runCard.className = 'run-card';
        runCard.innerHTML = `
          <div class="run-header">
            <h4>🔹 Run ${runId}</h4>
            <div style="display: flex; gap: 0.5rem;">
              <button onclick="loadRunDetails(${runId})" class="load-btn">View Dependencies</button>
              <button onclick="generateRunReport(${runId})" class="load-btn" style="background: rgba(16, 185, 129, 0.2); border-color: rgba(16, 185, 129, 0.4); color: #10b981;">📄 Generate Report</button>
            </div>
          </div>
          <div id="run-${runId}-details" class="run-details"></div>
        `;
        runsList.appendChild(runCard);
      });
    } else {
      runsList.innerHTML = '<p>No migration runs found.</p>';
    }
  } catch (error) {
    console.error('Error loading runs:', error);
    document.getElementById('runsList').innerHTML = '<p class="error">Error loading runs.</p>';
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

// Architecture Documentation Modal Handler
const archModal = document.getElementById('architectureModal');
const archBtn = document.getElementById('showArchitectureBtn');
const archClose = document.querySelector('.arch-close');

let architectureMarkdown = '';

// Open architecture modal
if (archBtn) {
  archBtn.onclick = function() {
    archModal.style.display = 'block';
    loadArchitectureDoc();
  };
}

// Close architecture modal
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

// Load and render architecture documentation with Mermaid diagrams
async function loadArchitectureDoc() {
  const contentDiv = document.getElementById('architectureContent');
  const lastModifiedSpan = document.getElementById('docLastModified');
  
  try {
    contentDiv.innerHTML = '<p style="text-align: center; color: #94a3b8;"><em>Loading documentation...</em></p>';
    
    const response = await fetch('/api/documentation/architecture');
    if (!response.ok) {
      throw new Error(`HTTP ${response.status}`);
    }
    
    const data = await response.json();
    architectureMarkdown = data.content;
    
    // Render markdown using marked.js
    if (typeof marked !== 'undefined') {
      // Configure marked for GFM and code blocks
      marked.setOptions({
        breaks: true,
        gfm: true,
        headerIds: true,
        mangle: false
      });
      
      contentDiv.innerHTML = marked.parse(architectureMarkdown);
      
      // Render all Mermaid diagrams
      if (window.mermaid) {
        const mermaidBlocks = contentDiv.querySelectorAll('code.language-mermaid');
        
        for (let i = 0; i < mermaidBlocks.length; i++) {
          const codeBlock = mermaidBlocks[i];
          const mermaidCode = codeBlock.textContent;
          const container = document.createElement('div');
          container.className = 'mermaid-diagram';
          container.style.cssText = 'background: #0f172a; padding: 1.5rem; border-radius: 8px; margin: 1rem 0; overflow-x: auto;';
          
          try {
            const { svg } = await window.mermaid.render(`mermaid-diagram-${i}`, mermaidCode);
            container.innerHTML = svg;
            
            // Add zoom controls
            const svgElement = container.querySelector('svg');
            if (svgElement) {
              svgElement.style.maxWidth = '100%';
              svgElement.style.height = 'auto';
            }
            
            codeBlock.parentElement.replaceWith(container);
          } catch (err) {
            console.error('Mermaid render error:', err);
            container.innerHTML = `<p style="color: #f87171;">⚠️ Error rendering diagram: ${err.message}</p><pre style="background: #1e293b; padding: 1rem; border-radius: 4px; overflow-x: auto;">${escapeHtml(mermaidCode)}</pre>`;
            codeBlock.parentElement.replaceWith(container);
          }
        }
      }
    } else {
      // Fallback to plain text if marked.js not loaded
      contentDiv.innerHTML = `<pre>${escapeHtml(architectureMarkdown)}</pre>`;
    }
    
    // Update last modified
    if (lastModifiedSpan && data.lastModified) {
      const date = new Date(data.lastModified);
      lastModifiedSpan.textContent = `Last updated: ${date.toLocaleDateString()} ${date.toLocaleTimeString()}`;
    }
  } catch (error) {
    console.error('Error loading architecture documentation:', error);
    contentDiv.innerHTML = `<p style="color: #ef4444;">Failed to load documentation: ${error.message}</p>`;
  }
}

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

// Download markdown file
document.getElementById('downloadMarkdownBtn')?.addEventListener('click', () => {
  if (!architectureMarkdown) return;
  
  const blob = new Blob([architectureMarkdown], { type: 'text/markdown' });
  const url = URL.createObjectURL(blob);
  const a = document.createElement('a');
  a.href = url;
  a.download = 'REVERSE_ENGINEERING_ARCHITECTURE.md';
  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);
  URL.revokeObjectURL(url);
});

// Copy to clipboard
document.getElementById('copyToClipboardBtn')?.addEventListener('click', async () => {
  if (!architectureMarkdown) return;
  
  try {
    await navigator.clipboard.writeText(architectureMarkdown);
    
    // Visual feedback
    const btn = document.getElementById('copyToClipboardBtn');
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

// Helper function to escape HTML
function escapeHtml(text) {
  const div = document.createElement('div');
  div.textContent = text;
  return div.innerHTML;
}
