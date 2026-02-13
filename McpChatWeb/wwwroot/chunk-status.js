// chunk-status.js - Smart Chunking Progress Viewer with Real-Time Updates

(function() {
  'use strict';

  // DOM Elements
  const chunkStatusBtn = document.getElementById('showChunkStatusBtn');
  const chunkStatusModal = document.getElementById('chunkStatusModal');
  const chunkCloseBtn = document.querySelector('.chunk-close');
  const chunkSummary = document.getElementById('chunkSummary');
  const chunkFilesList = document.getElementById('chunkFilesList');
  const chunkDetails = document.getElementById('chunkDetails');
  const chunkDetailsFile = document.getElementById('chunkDetailsFile');
  const chunkDetailsContent = document.getElementById('chunkDetailsContent');

  // Summary elements - new simplified progress bar
  const overallProgressEl = document.getElementById('overallChunkProgress');
  const completedChunksEl = document.getElementById('completedChunks');
  const totalChunksCountEl = document.getElementById('totalChunksCount');
  const overallProgressBarEl = document.getElementById('overallChunkProgressBar');

  // Legacy elements (keep for compatibility, may be null)
  const totalFilesChunkedEl = document.getElementById('totalFilesChunked');
  const totalChunksEl = document.getElementById('totalChunks');
  const avgProgressEl = document.getElementById('avgProgress');
  const totalTokensEl = document.getElementById('totalTokens');

  // State
  let currentRunId = null;
  let autoRefreshInterval = null;
  let isModalOpen = false;
  let lastKnownState = null;

  // Auto-refresh settings
  const AUTO_REFRESH_INTERVAL = 15000; // 15 seconds (reduced from 3s to prevent CPU spikes)
  const HEADER_REFRESH_INTERVAL = 30000; // 30 seconds for mini dashboard (reduced from 5s)

  // Initialize
  function init() {
    if (!chunkStatusBtn || !chunkStatusModal) {
      console.warn('Chunk status elements not found');
      return;
    }

    chunkStatusBtn.addEventListener('click', openChunkStatusModal);
    chunkCloseBtn.addEventListener('click', closeChunkStatusModal);

    // Close on click outside
    window.addEventListener('click', (e) => {
      if (e.target === chunkStatusModal) {
        closeChunkStatusModal();
      }
    });

    // Listen for run selector changes
    const runSelector = document.getElementById('run-selector');
    if (runSelector) {
      runSelector.addEventListener('change', () => {
        currentRunId = runSelector.value;
        if (isModalOpen) {
          loadChunkStatus();
        }
        updateMiniDashboard();
      });
    }

    // Create mini dashboard in header
    createMiniDashboard();
    
    // Start header dashboard updates
    startHeaderDashboardUpdates();
  }

  // Mini Dashboard - shows progress even when modal is closed
  function createMiniDashboard() {
    const headerRight = document.querySelector('.header-right');
    if (!headerRight) return;

    const miniDashboard = document.createElement('div');
    miniDashboard.id = 'chunk-mini-dashboard';
    miniDashboard.className = 'chunk-mini-dashboard';
    miniDashboard.innerHTML = `
      <div class="mini-dash-header">
        <span class="mini-dash-icon">🧩</span>
        <span class="mini-dash-title">Chunking</span>
      </div>
      <div class="mini-dash-content">
        <div class="mini-dash-progress">
          <div class="mini-progress-bar">
            <div class="mini-progress-fill" id="miniProgressFill"></div>
          </div>
          <span class="mini-progress-text" id="miniProgressText">-</span>
        </div>
        <div class="mini-dash-stats">
          <span id="miniChunksComplete">0/0</span>
          <span class="mini-stat-label">chunks</span>
        </div>
      </div>
      <div class="mini-dash-status" id="miniDashStatus">
        <span class="status-dot"></span>
        <span class="status-text">Idle</span>
      </div>
    `;
    
    // Insert before db-status-container
    const dbStatus = headerRight.querySelector('.db-status-container');
    if (dbStatus) {
      headerRight.insertBefore(miniDashboard, dbStatus);
    } else {
      headerRight.appendChild(miniDashboard);
    }

    // Click to open modal
    miniDashboard.addEventListener('click', openChunkStatusModal);
  }

  function startHeaderDashboardUpdates() {
    updateMiniDashboard();
    setInterval(() => {
      if (window.pageIsVisible !== false) updateMiniDashboard();
    }, HEADER_REFRESH_INTERVAL);
  }

  async function updateMiniDashboard() {
    const miniDashboard = document.getElementById('chunk-mini-dashboard');
    if (!miniDashboard) return;

    const runSelector = document.getElementById('run-selector');
    const runId = runSelector ? runSelector.value : '';
    
    if (!runId) {
      updateMiniDashboardUI(null);
      return;
    }

    try {
      // Use direct SQL API instead of MCP
      const response = await fetch(`/api/runs/${runId}/chunks`);
      if (!response.ok) {
        updateMiniDashboardUI(null);
        return;
      }

      const data = await response.json();
      updateMiniDashboardUI(data);
    } catch (error) {
      console.debug('Mini dashboard update failed:', error);
      updateMiniDashboardUI(null);
    }
  }

  function updateMiniDashboardUI(data) {
    const progressFill = document.getElementById('miniProgressFill');
    const progressText = document.getElementById('miniProgressText');
    const chunksComplete = document.getElementById('miniChunksComplete');
    const statusEl = document.getElementById('miniDashStatus');

    if (!data || (!data.files || data.files.length === 0)) {
      // Check if using direct processing
      if (data && data.usingDirectProcessing) {
        if (progressFill) progressFill.style.width = '100%';
        if (progressText) progressText.textContent = '✓';
        if (chunksComplete) chunksComplete.textContent = 'Direct';
        if (statusEl) {
          statusEl.className = 'mini-dash-status complete';
          statusEl.querySelector('.status-text').textContent = 'Direct Mode';
        }
        return;
      }
      
      if (progressFill) progressFill.style.width = '0%';
      if (progressText) progressText.textContent = '-';
      if (chunksComplete) chunksComplete.textContent = '0/0';
      if (statusEl) {
        statusEl.className = 'mini-dash-status idle';
        statusEl.querySelector('.status-text').textContent = 'No data';
      }
      return;
    }

    const files = data.files;
    const totalChunks = files.reduce((sum, f) => sum + f.totalChunks, 0);
    const completedChunks = files.reduce((sum, f) => sum + f.completedChunks, 0);
    const failedChunks = files.reduce((sum, f) => sum + f.failedChunks, 0);
    const avgProgress = files.length > 0 
      ? files.reduce((sum, f) => sum + f.progressPercentage, 0) / files.length 
      : 0;

    if (progressFill) progressFill.style.width = `${avgProgress}%`;
    if (progressText) progressText.textContent = `${avgProgress.toFixed(0)}%`;
    if (chunksComplete) chunksComplete.textContent = `${completedChunks}/${totalChunks}`;

    // Determine status
    let status = 'idle';
    let statusText = 'Idle';
    
    if (avgProgress >= 100) {
      status = 'complete';
      statusText = 'Complete';
    } else if (failedChunks > 0) {
      status = 'error';
      statusText = `${failedChunks} failed`;
    } else if (completedChunks < totalChunks && completedChunks > 0) {
      status = 'processing';
      statusText = 'Processing...';
    } else if (totalChunks > 0 && completedChunks === 0) {
      status = 'pending';
      statusText = 'Pending';
    }

    if (statusEl) {
      statusEl.className = `mini-dash-status ${status}`;
      statusEl.querySelector('.status-text').textContent = statusText;
    }
  }

  function openChunkStatusModal() {
    chunkStatusModal.style.display = 'block';
    isModalOpen = true;
    loadChunkStatus();
    startAutoRefresh();
  }

  function closeChunkStatusModal() {
    chunkStatusModal.style.display = 'none';
    chunkDetails.style.display = 'none';
    isModalOpen = false;
    stopAutoRefresh();
  }

  function startAutoRefresh() {
    stopAutoRefresh(); // Clear any existing interval
    autoRefreshInterval = setInterval(() => {
      if (isModalOpen && window.pageIsVisible !== false) {
        loadChunkStatus(true); // true = silent refresh
      }
    }, AUTO_REFRESH_INTERVAL);
  }

  function stopAutoRefresh() {
    if (autoRefreshInterval) {
      clearInterval(autoRefreshInterval);
      autoRefreshInterval = null;
    }
  }

  async function loadChunkStatus(silent = false) {
    try {
      if (!silent) {
        chunkFilesList.innerHTML = '<p class="loading">Loading chunk status...</p>';
      }
      
      const runSelector = document.getElementById('run-selector');
      const runId = runSelector ? runSelector.value : '';
      
      if (!runId) {
        chunkFilesList.innerHTML = '<p class="error">Please select a migration run first</p>';
        return;
      }

      // Use direct SQL API instead of MCP
      const response = await fetch(`/api/runs/${runId}/chunks`);
      
      if (!response.ok) {
        throw new Error(`HTTP ${response.status}: ${response.statusText}`);
      }

      const data = await response.json();
      
      if (data.error) {
        if (!silent) {
          chunkFilesList.innerHTML = `<p class="error">${data.error}</p>`;
        }
        updateSummary(null);
        return;
      }

      // Check for changes before updating (reduces flicker)
      const newState = JSON.stringify(data);
      if (silent && newState === lastKnownState) {
        return; // No changes, skip update
      }
      lastKnownState = newState;

      displayChunkStatus(data, silent);
      
      // Also update mini dashboard
      updateMiniDashboardUI(data);
    } catch (error) {
      if (!silent) {
        console.error('Error loading chunk status:', error);
        chunkFilesList.innerHTML = `<p class="error">Failed to load chunk status: ${error.message}</p>`;
      }
      updateSummary(null);
    }
  }

  function displayChunkStatus(data, silent = false) {
    const files = data.files || [];
    
    // Update summary - pass full data for direct processing detection
    updateSummary(data);

    if (files.length === 0) {
      // Check if run used direct processing instead of chunking
      if (data.usingDirectProcessing) {
        chunkFilesList.innerHTML = `
          <div class="chunk-empty-state direct-processing">
            <div class="empty-icon">✅</div>
            <h3>Direct Processing Mode</h3>
            <p class="info">Files were processed directly without chunking.</p>
            <p class="hint">All files in this run were small enough to be converted in a single API call (under the chunk threshold of ~10,000 lines).</p>
            <div class="direct-processing-benefits">
              <h4>📊 Processing Benefits:</h4>
              <ul>
                <li>✓ Faster conversion (no chunk assembly required)</li>
                <li>✓ Simpler code flow (no inter-chunk dependencies)</li>
                <li>✓ Complete context in each conversion</li>
              </ul>
            </div>
            <p class="hint">Check the <strong>Migration Monitor</strong> or <strong>Process Status</strong> for detailed conversion progress.</p>
          </div>
        `;
      } else {
        chunkFilesList.innerHTML = `
          <div class="chunk-empty-state">
            <div class="empty-icon">🧩</div>
            <p class="info">No chunked files found for this run.</p>
            <p class="hint">Files are chunked when they exceed the configured line limit (typically 10,000 lines).</p>
            <div class="auto-refresh-indicator">
              <span class="refresh-dot"></span>
              Auto-refreshing every 3 seconds...
            </div>
          </div>
        `;
      }
      return;
    }

    // Calculate overall progress
    const totalChunks = files.reduce((sum, f) => sum + f.totalChunks, 0);
    const completedChunks = files.reduce((sum, f) => sum + f.completedChunks, 0);
    const failedChunks = files.reduce((sum, f) => sum + f.failedChunks, 0);
    const processingChunks = files.reduce((sum, f) => sum + (f.pendingChunks || 0), 0);
    const overallProgress = totalChunks > 0 ? (completedChunks / totalChunks * 100) : 0;

    // Build file list with enhanced visual indicators
    let html = `
      <div class="chunk-overall-progress">
        <div class="overall-header">
          <span class="overall-title">Overall Progress</span>
          <span class="overall-percentage">${overallProgress.toFixed(1)}%</span>
        </div>
        <div class="overall-progress-bar">
          <div class="overall-progress-fill ${getProgressClass(overallProgress)}" style="width: ${overallProgress}%"></div>
        </div>
        <div class="overall-stats">
          <span class="overall-stat">
            <span class="stat-icon">✅</span>
            <span class="stat-num">${completedChunks}</span> completed
          </span>
          <span class="overall-stat">
            <span class="stat-icon">⏳</span>
            <span class="stat-num">${processingChunks}</span> pending
          </span>
          ${failedChunks > 0 ? `
          <span class="overall-stat error">
            <span class="stat-icon">❌</span>
            <span class="stat-num">${failedChunks}</span> failed
          </span>
          ` : ''}
        </div>
        <div class="auto-refresh-indicator">
          <span class="refresh-dot"></span>
          Auto-refreshing every 3 seconds...
        </div>
      </div>
    `;
    
    html += '<div class="chunk-files-grid">';
    
    files.forEach(file => {
      const progressClass = getProgressClass(file.progressPercentage);
      const statusIcon = getStatusIcon(file);
      const isActive = file.progressPercentage > 0 && file.progressPercentage < 100;
      
      html += `
        <div class="chunk-file-card ${isActive ? 'active' : ''} ${file.failedChunks > 0 ? 'has-errors' : ''}" data-file="${escapeHtml(file.sourceFile)}">
          <div class="chunk-file-header">
            <span class="status-icon ${isActive ? 'pulse' : ''}">${statusIcon}</span>
            <span class="chunk-file-name" title="${escapeHtml(file.sourceFile)}">${escapeHtml(file.sourceFile)}</span>
          </div>
          <div class="chunk-progress-bar">
            <div class="chunk-progress-fill ${progressClass}" style="width: ${file.progressPercentage}%"></div>
          </div>
          <div class="chunk-progress-label">
            <span>${file.progressPercentage.toFixed(1)}%</span>
            <span class="chunk-fraction">${file.completedChunks}/${file.totalChunks} chunks</span>
          </div>
          <div class="chunk-file-stats">
            <span class="stat">
              <span class="stat-value">${formatNumber(file.totalTokensUsed)}</span>
              <span class="stat-label">tokens</span>
            </span>
            <span class="stat">
              <span class="stat-value">${formatDuration(file.totalProcessingTimeMs)}</span>
              <span class="stat-label">time</span>
            </span>
            <span class="stat">
              <span class="stat-value">${file.totalChunks}</span>
              <span class="stat-label">total</span>
            </span>
          </div>
          ${file.failedChunks > 0 ? `<div class="chunk-file-error">⚠️ ${file.failedChunks} failed chunks</div>` : ''}
          ${isActive ? '<div class="processing-indicator"><span class="processing-dot"></span>Processing...</div>' : ''}
        </div>
      `;
    });

    html += '</div>';
    
    // Add last updated timestamp
    html += `
      <div class="last-updated">
        Last updated: ${new Date().toLocaleTimeString()}
      </div>
    `;
    
    chunkFilesList.innerHTML = html;

    // Add click handlers for file cards
    document.querySelectorAll('.chunk-file-card').forEach(card => {
      card.addEventListener('click', () => {
        const fileName = card.dataset.file;
        loadChunkDetails(fileName);
      });
    });
  }

  function updateSummary(data) {
    // Update new simplified progress bar
    const updateProgressBar = (completed, total, percentage) => {
      if (overallProgressEl) overallProgressEl.textContent = `${percentage.toFixed(1)}%`;
      if (completedChunksEl) completedChunksEl.textContent = completed.toString();
      if (totalChunksCountEl) totalChunksCountEl.textContent = total.toString();
      if (overallProgressBarEl) overallProgressBarEl.style.width = `${percentage}%`;
    };

    // Handle null data or direct processing mode
    if (!data) {
      updateProgressBar(0, 0, 0);
      if (totalFilesChunkedEl) totalFilesChunkedEl.textContent = '-';
      if (totalChunksEl) totalChunksEl.textContent = '-';
      if (avgProgressEl) avgProgressEl.textContent = '-';
      if (totalTokensEl) totalTokensEl.textContent = '-';
      return;
    }

    const files = data.files || [];
    
    // Direct processing mode - show appropriate status
    if (data.usingDirectProcessing || files.length === 0) {
      if (data.usingDirectProcessing) {
        updateProgressBar(1, 1, 100);
        if (totalFilesChunkedEl) totalFilesChunkedEl.textContent = 'Direct';
        if (totalChunksEl) totalChunksEl.textContent = 'N/A';
        if (avgProgressEl) avgProgressEl.textContent = '✓';
        if (totalTokensEl) totalTokensEl.textContent = 'Direct';
      } else {
        updateProgressBar(0, 0, 0);
        if (totalFilesChunkedEl) totalFilesChunkedEl.textContent = '0';
        if (totalChunksEl) totalChunksEl.textContent = '0';
        if (avgProgressEl) avgProgressEl.textContent = '-';
        if (totalTokensEl) totalTokensEl.textContent = '0';
      }
      return;
    }

    const totalChunks = files.reduce((sum, f) => sum + f.totalChunks, 0);
    const completedChunks = files.reduce((sum, f) => sum + f.completedChunks, 0);
    const totalTokens = files.reduce((sum, f) => sum + f.totalTokensUsed, 0);
    const avgProgress = totalChunks > 0 
      ? (completedChunks / totalChunks) * 100
      : 0;

    // Update new progress bar
    updateProgressBar(completedChunks, totalChunks, avgProgress);

    // Update legacy elements if they exist
    if (totalFilesChunkedEl) totalFilesChunkedEl.textContent = files.length.toString();
    if (totalChunksEl) totalChunksEl.textContent = `${completedChunks}/${totalChunks}`;
    if (avgProgressEl) avgProgressEl.textContent = `${avgProgress.toFixed(1)}%`;
    if (totalTokensEl) totalTokensEl.textContent = formatNumber(totalTokens);
  }

  async function loadChunkDetails(fileName) {
    try {
      chunkDetails.style.display = 'block';
      chunkDetailsFile.textContent = fileName;
      chunkDetailsContent.innerHTML = '<p class="loading">Loading chunk details...</p>';

      const runSelector = document.getElementById('run-selector');
      const runId = runSelector ? runSelector.value : '';

      // Use direct SQL API instead of MCP
      const response = await fetch(`/api/runs/${runId}/chunks/${encodeURIComponent(fileName)}`);
      
      if (!response.ok) {
        throw new Error(`HTTP ${response.status}: ${response.statusText}`);
      }

      const data = await response.json();
      displayChunkDetails(data);
    } catch (error) {
      console.error('Error loading chunk details:', error);
      chunkDetailsContent.innerHTML = `<p class="error">Failed to load chunk details: ${error.message}</p>`;
    }
  }

  function displayChunkDetails(data) {
    const chunks = data.chunks || [];
    
    if (chunks.length === 0) {
      chunkDetailsContent.innerHTML = '<p class="info">No chunk details available.</p>';
      return;
    }

    let html = `
      <div class="chunk-details-summary">
        <span><strong>Total Lines:</strong> ${formatNumber(data.totalLines)}</span>
        <span><strong>Total Tokens:</strong> ${formatNumber(data.totalTokensUsed)}</span>
        <span><strong>Total Time:</strong> ${formatDuration(data.totalProcessingTimeMs)}</span>
      </div>
      <div class="chunk-timeline">
    `;

    chunks.forEach(chunk => {
      const statusClass = getChunkStatusClass(chunk.status);
      const statusIcon = getChunkStatusIcon(chunk.status);
      
      html += `
        <div class="chunk-item ${statusClass}">
          <div class="chunk-item-header">
            ${statusIcon}
            <span class="chunk-number">Chunk ${chunk.chunkIndex + 1}</span>
            <span class="chunk-status">${chunk.status}</span>
          </div>
          <div class="chunk-item-details">
            <span>Lines ${chunk.startLine}-${chunk.endLine}</span>
            ${chunk.tokensUsed ? `<span>${formatNumber(chunk.tokensUsed)} tokens</span>` : ''}
            ${chunk.processingTimeMs ? `<span>${formatDuration(chunk.processingTimeMs)}</span>` : ''}
          </div>
          ${chunk.semanticUnits && chunk.semanticUnits.length > 0 ? `
            <div class="chunk-semantic-units">
              <span class="units-label">Units:</span>
              ${chunk.semanticUnits.slice(0, 5).map(u => `<span class="unit-tag">${escapeHtml(u)}</span>`).join('')}
              ${chunk.semanticUnits.length > 5 ? `<span class="units-more">+${chunk.semanticUnits.length - 5} more</span>` : ''}
            </div>
          ` : ''}
        </div>
      `;
    });

    html += '</div>';
    chunkDetailsContent.innerHTML = html;
  }

  // Helper functions
  function getProgressClass(percentage) {
    if (percentage >= 100) return 'progress-complete';
    if (percentage >= 75) return 'progress-high';
    if (percentage >= 50) return 'progress-medium';
    if (percentage >= 25) return 'progress-low';
    return 'progress-start';
  }

  function getStatusIcon(file) {
    if (file.failedChunks > 0) return '❌';
    if (file.progressPercentage >= 100) return '✅';
    if (file.pendingChunks > 0) return '⏳';
    return '🔄';
  }

  function getChunkStatusClass(status) {
    switch (status.toLowerCase()) {
      case 'completed': return 'chunk-completed';
      case 'failed': return 'chunk-failed';
      case 'processing': return 'chunk-processing';
      default: return 'chunk-pending';
    }
  }

  function getChunkStatusIcon(status) {
    switch (status.toLowerCase()) {
      case 'completed': return '✅';
      case 'failed': return '❌';
      case 'processing': return '🔄';
      default: return '⏳';
    }
  }

  function formatNumber(num) {
    if (num >= 1000000) return (num / 1000000).toFixed(1) + 'M';
    if (num >= 1000) return (num / 1000).toFixed(1) + 'K';
    return num.toString();
  }

  function formatDuration(ms) {
    if (!ms || ms === 0) return '-';
    if (ms < 1000) return `${ms}ms`;
    if (ms < 60000) return `${(ms / 1000).toFixed(1)}s`;
    return `${(ms / 60000).toFixed(1)}m`;
  }

  function escapeHtml(str) {
    const div = document.createElement('div');
    div.textContent = str;
    return div.innerHTML;
  }

  // Initialize when DOM is ready
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }
})();
