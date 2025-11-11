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
            <button onclick="loadRunDetails(${runId})" class="load-btn">View Dependencies</button>
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
