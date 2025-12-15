// Activity Dashboard - Real-time monitoring of migration processes
(function() {
  'use strict';

  let activityInterval = null;
  let isExpanded = false;
  const REFRESH_INTERVAL = 10000; // 10 seconds (reduced from 2s to prevent CPU spikes)

  // Initialize the activity dashboard
  function init() {
    createDashboardHTML();
    setupEventListeners();
    startMonitoring();
  }

  // Create the dashboard HTML structure
  function createDashboardHTML() {
    const dashboard = document.createElement('div');
    dashboard.id = 'activity-dashboard';
    dashboard.className = 'activity-dashboard';
    dashboard.innerHTML = `
      <div class="activity-header" id="activity-header">
        <div class="activity-title">
          <span class="activity-icon">📊</span>
          <span class="activity-label">Live Activity</span>
          <span class="activity-status" id="activity-status-indicator">●</span>
        </div>
        <div class="activity-mini-stats" id="activity-mini-stats">
          <span class="mini-stat" id="mini-workers" title="Active Workers">👷 0</span>
          <span class="mini-stat" id="mini-chunks" title="Progress">📦 0/0</span>
          <span class="mini-stat" id="mini-phase" title="Current Phase">⏳ Idle</span>
        </div>
        <button class="activity-toggle" id="activity-toggle">▼</button>
      </div>
      
      <div class="activity-body" id="activity-body">
        <!-- Services Status -->
        <div class="activity-section">
          <h4>🔌 Services</h4>
          <div class="services-grid" id="services-grid">
            <div class="service-item" id="svc-portal">
              <span class="service-dot"></span>
              <span class="service-name">Portal</span>
            </div>
            <div class="service-item" id="svc-sqlite">
              <span class="service-dot"></span>
              <span class="service-name">SQLite</span>
            </div>
            <div class="service-item" id="svc-neo4j">
              <span class="service-dot"></span>
              <span class="service-name">Neo4j</span>
            </div>
            <div class="service-item" id="svc-mcp">
              <span class="service-dot"></span>
              <span class="service-name">MCP</span>
            </div>
          </div>
        </div>

        <!-- Migration Status -->
        <div class="activity-section">
          <h4>🚀 Migration Status</h4>
          <div class="migration-status" id="migration-status">
            <div class="status-row">
              <span class="status-label">Run ID:</span>
              <span class="status-value" id="run-id-value">-</span>
            </div>
            <div class="status-row">
              <span class="status-label">Status:</span>
              <span class="status-value" id="run-status-value">-</span>
            </div>
            <div class="status-row">
              <span class="status-label">Phase:</span>
              <span class="status-value" id="run-phase-value">-</span>
            </div>
            <div class="status-row" id="processing-mode-row">
              <span class="status-label">Mode:</span>
              <span class="status-value" id="processing-mode-value">-</span>
            </div>
            <div class="progress-bar-container">
              <div class="progress-bar" id="migration-progress-bar">
                <div class="progress-fill" id="migration-progress-fill"></div>
                <span class="progress-text" id="migration-progress-text">0%</span>
              </div>
            </div>
          </div>
        </div>

        <!-- Processing Stats (replaces Chunk Processing for direct mode) -->
        <div class="activity-section">
          <h4>📦 Processing Stats</h4>
          <div class="chunk-stats" id="chunk-stats">
            <div class="chunk-stat pending">
              <span class="chunk-count" id="pending-count">0</span>
              <span class="chunk-label">Pending</span>
            </div>
            <div class="chunk-stat processing">
              <span class="chunk-count" id="processing-count">0</span>
              <span class="chunk-label">Processing</span>
            </div>
            <div class="chunk-stat completed">
              <span class="chunk-count" id="completed-count">0</span>
              <span class="chunk-label">Completed</span>
            </div>
            <div class="chunk-stat failed">
              <span class="chunk-count" id="failed-count">0</span>
              <span class="chunk-label">Failed</span>
            </div>
          </div>
          <div class="active-chunks" id="active-chunks">
            <h5>Recent Activity:</h5>
            <div class="chunks-list" id="chunks-list">
              <div class="no-activity">No active processing</div>
            </div>
          </div>
        </div>

        <!-- Recent API Calls -->
        <div class="activity-section">
          <h4>🌐 Recent API Calls</h4>
          <div class="api-calls-list" id="api-calls-list">
            <div class="no-activity">No recent API calls</div>
          </div>
        </div>

        <!-- Activity Log -->
        <div class="activity-section">
          <h4>📜 Activity Log</h4>
          <div class="activity-log" id="activity-log">
            <div class="log-entry info">Dashboard initialized</div>
          </div>
        </div>
      </div>
    `;

    document.body.appendChild(dashboard);
  }

  // Setup event listeners
  function setupEventListeners() {
    const header = document.getElementById('activity-header');
    const toggle = document.getElementById('activity-toggle');
    
    header.addEventListener('click', toggleDashboard);
    toggle.addEventListener('click', (e) => {
      e.stopPropagation();
      toggleDashboard();
    });
  }

  // Toggle dashboard expanded/collapsed
  function toggleDashboard() {
    const dashboard = document.getElementById('activity-dashboard');
    const body = document.getElementById('activity-body');
    const toggle = document.getElementById('activity-toggle');
    
    isExpanded = !isExpanded;
    
    if (isExpanded) {
      dashboard.classList.add('expanded');
      body.style.display = 'block';
      toggle.textContent = '▲';
    } else {
      dashboard.classList.remove('expanded');
      body.style.display = 'none';
      toggle.textContent = '▼';
    }
  }

  // Start monitoring
  function startMonitoring() {
    fetchActivity();
    activityInterval = setInterval(() => {
      if (window.pageIsVisible !== false) fetchActivity();
    }, REFRESH_INTERVAL);
  }

  // Stop monitoring
  function stopMonitoring() {
    if (activityInterval) {
      clearInterval(activityInterval);
      activityInterval = null;
    }
  }

  // Fetch activity data from API
  async function fetchActivity() {
    try {
      const response = await fetch('/api/activity/live');
      if (!response.ok) {
        throw new Error(`HTTP ${response.status}`);
      }
      const data = await response.json();
      updateDashboard(data);
    } catch (error) {
      console.debug('Activity fetch error:', error);
      updateStatusIndicator('error');
    }
  }

  // Update the dashboard with new data
  function updateDashboard(data) {
    updateStatusIndicator('connected');
    updateServices(data.services);
    updateMigrationStatus(data.migration);
    updateChunkStats(data.chunks);
    updateApiCalls(data.apiCalls);
    updateMiniStats(data);
  }

  // Update status indicator
  function updateStatusIndicator(status) {
    const indicator = document.getElementById('activity-status-indicator');
    indicator.className = 'activity-status ' + status;
  }

  // Update services status
  function updateServices(services) {
    updateServiceStatus('svc-portal', services.portal);
    updateServiceStatus('svc-sqlite', services.sqlite);
    updateServiceStatus('svc-neo4j', services.neo4j);
    updateServiceStatus('svc-mcp', services.mcpServer);
  }

  function updateServiceStatus(id, connected) {
    const el = document.getElementById(id);
    if (el) {
      el.className = 'service-item ' + (connected ? 'connected' : 'disconnected');
    }
  }

  // Update migration status
  function updateMigrationStatus(migration) {
    document.getElementById('run-id-value').textContent = migration.runId || '-';
    document.getElementById('run-status-value').textContent = migration.status || '-';
    document.getElementById('run-status-value').className = 'status-value status-' + (migration.status || '').toLowerCase().replace(/\s+/g, '-');
    document.getElementById('run-phase-value').textContent = migration.phase || '-';
    
    // Show processing mode
    const processingMode = document.getElementById('processing-mode-value');
    if (processingMode) {
      const mode = migration.usingDirectProcessing ? '⚡ Direct' : '🧩 Chunked';
      processingMode.textContent = mode;
      processingMode.className = 'status-value ' + (migration.usingDirectProcessing ? 'mode-direct' : 'mode-chunked');
    }
    
    const progress = migration.progress;
    if (progress) {
      const percentage = progress.percentage || 0;
      document.getElementById('migration-progress-fill').style.width = percentage + '%';
      
      if (migration.usingDirectProcessing) {
        // For direct processing, show file counts instead of chunk counts
        const filesText = progress.completedFiles !== undefined 
          ? `${progress.completedFiles}/${progress.totalFiles} files (${percentage}%)`
          : `${percentage}%`;
        document.getElementById('migration-progress-text').textContent = filesText;
      } else {
        document.getElementById('migration-progress-text').textContent = 
          `${progress.completedChunks}/${progress.totalChunks} (${percentage}%)`;
      }
    }
  }

  // Update chunk statistics
  function updateChunkStats(chunks) {
    document.getElementById('pending-count').textContent = chunks.pendingChunks || chunks.pendingFiles || 0;
    document.getElementById('processing-count').textContent = chunks.processingChunks || chunks.processingFiles || 0;
    document.getElementById('completed-count').textContent = chunks.completedChunks || chunks.completedFiles || 0;
    document.getElementById('failed-count').textContent = chunks.failedChunks || chunks.failedFiles || 0;
    
    // Update active chunks/files list
    const chunksList = document.getElementById('chunks-list');
    const items = chunks.currentChunks || chunks.currentFiles || [];
    
    if (items.length > 0) {
      chunksList.innerHTML = items.map(item => {
        const statusClass = (item.status || '').toLowerCase();
        const fileName = (item.file || item.fileName || '').split('/').pop();
        const timeInfo = item.status === 'Completed' && item.processingTimeMs 
          ? `(${(item.processingTimeMs / 1000).toFixed(1)}s)` 
          : '';
        
        // Handle both chunk and file items
        const rangeInfo = item.chunkIndex !== undefined 
          ? `#${item.chunkIndex} [${item.startLine}-${item.endLine}]`
          : (item.lineCount ? `${item.lineCount} lines` : '');
          
        return `
          <div class="chunk-item ${statusClass}">
            <span class="chunk-status-icon">${getStatusIcon(item.status)}</span>
            <span class="chunk-file" title="${item.file || item.fileName || ''}">${fileName}</span>
            <span class="chunk-range">${rangeInfo}</span>
            <span class="chunk-time">${timeInfo}</span>
          </div>
        `;
      }).join('');
    } else if (chunks.usingDirectProcessing) {
      chunksList.innerHTML = `
        <div class="direct-processing-notice">
          <span class="notice-icon">⚡</span>
          <span class="notice-text">Direct processing mode - files converted without chunking</span>
        </div>
      `;
    } else {
      chunksList.innerHTML = '<div class="no-activity">No active processing</div>';
    }
  }

  // Get status icon
  function getStatusIcon(status) {
    switch (status.toLowerCase()) {
      case 'processing': return '⚙️';
      case 'completed': return '✅';
      case 'failed': return '❌';
      case 'pending': return '⏳';
      default: return '•';
    }
  }

  // Update API calls list
  function updateApiCalls(apiCalls) {
    const list = document.getElementById('api-calls-list');
    if (apiCalls.recentCalls && apiCalls.recentCalls.length > 0) {
      list.innerHTML = apiCalls.recentCalls.map(call => {
        const statusClass = call.status === 'success' ? 'success' : 'error';
        const time = call.timestamp ? new Date(call.timestamp).toLocaleTimeString() : '';
        return `
          <div class="api-call ${statusClass}">
            <span class="api-time">${time}</span>
            <span class="api-agent">${call.agent || 'Unknown'}</span>
            <span class="api-duration">${call.durationMs}ms</span>
          </div>
        `;
      }).join('');
    } else {
      list.innerHTML = '<div class="no-activity">No recent API calls</div>';
    }
  }

  // Update mini stats in header
  function updateMiniStats(data) {
    document.getElementById('mini-workers').textContent = `👷 ${data.chunks?.activeWorkers || 0}`;
    
    const total = data.migration?.progress?.totalChunks || 0;
    const completed = data.migration?.progress?.completedChunks || 0;
    document.getElementById('mini-chunks').textContent = `📦 ${completed}/${total}`;
    
    const phase = data.migration?.phase || 'Idle';
    document.getElementById('mini-phase').textContent = `⏳ ${phase}`;
  }

  // Add log entry
  function addLogEntry(message, level = 'info') {
    const log = document.getElementById('activity-log');
    const entry = document.createElement('div');
    entry.className = 'log-entry ' + level;
    entry.textContent = `[${new Date().toLocaleTimeString()}] ${message}`;
    log.insertBefore(entry, log.firstChild);
    
    // Keep only last 50 entries
    while (log.children.length > 50) {
      log.removeChild(log.lastChild);
    }
  }

  // Initialize when DOM is ready
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }

  // Export for external use
  window.activityDashboard = {
    refresh: fetchActivity,
    start: startMonitoring,
    stop: stopMonitoring,
    log: addLogEntry
  };
})();
