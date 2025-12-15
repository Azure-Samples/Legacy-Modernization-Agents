// Migration Monitor - Unified view combining Process Tracker, Chunks, and Agent Chat
(function() {
  'use strict';

  // Process stages configuration with enhanced status explanations
  const PROCESS_STAGES = [
    { id: 'file-discovery', name: 'File Discovery', icon: '📂', description: 'Scanning for COBOL files',
      statusHints: { pending: 'Waiting to start', running: 'Scanning source folder...', completed: 'Files discovered', failed: 'Scan failed' } },
    { id: 'technical-analysis', name: 'Technical Analysis', icon: '🔬', description: 'Analyzing COBOL structure',
      statusHints: { pending: 'Awaiting file discovery', running: 'Analyzing program structure...', completed: 'Analysis complete', failed: 'Analysis failed - large files are chunked automatically.' } },
    { id: 'business-logic', name: 'Business Logic', icon: '💼', description: 'Extracting business rules',
      statusHints: { pending: 'Waiting for analysis', running: 'Extracting business rules...', completed: 'Business logic extracted', failed: 'Extraction failed - large files are chunked automatically.' } },
    { id: 'dependency-mapping', name: 'Dependency Mapping', icon: '🔗', description: 'Building dependency graph',
      statusHints: { pending: 'Waiting', running: 'Mapping dependencies...', completed: 'Dependencies mapped', failed: 'Mapping failed' } },
    { id: 'chunking', name: 'Smart Chunking', icon: '🧩', description: 'Splitting large files',
      statusHints: { pending: 'Waiting', running: 'Creating semantic chunks...', completed: 'Files chunked', failed: 'Chunking failed' } },
    { id: 'conversion', name: 'Code Conversion', icon: '⚙️', description: 'Converting to target language',
      statusHints: { pending: 'Awaiting chunking', running: 'Converting COBOL to target language...', completed: 'Conversion complete', failed: 'Conversion failed' } },
    { id: 'assembly', name: 'Code Assembly', icon: '🔧', description: 'Assembling converted chunks',
      statusHints: { pending: 'Awaiting conversion', running: 'Assembling output files...', completed: 'Code assembled', failed: 'Assembly failed' } },
    { id: 'output', name: 'Output Generation', icon: '📤', description: 'Writing output files',
      statusHints: { pending: 'Waiting', running: 'Writing files...', completed: 'Files written', failed: 'Output failed' } }
  ];

  // Status indicator legend
  const STATUS_LEGEND = {
    pending: { icon: '⏸', label: 'Pending', description: 'Stage has not started yet' },
    running: { icon: '⏳', label: 'Running', description: 'Stage is currently processing' },
    completed: { icon: '✓', label: 'Completed', description: 'Stage finished successfully' },
    failed: { icon: '✗', label: 'Failed', description: 'Stage encountered an error' }
  };

  // Agent types and their colors
  const AGENTS = {
    'CobolAnalyzerAgent': { name: 'COBOL Analyzer', color: '#3b82f6', icon: '🔬' },
    'CobolAnalyzer': { name: 'COBOL Analyzer', color: '#3b82f6', icon: '🔬' },
    'BusinessLogicExtractorAgent': { name: 'Business Logic', color: '#10b981', icon: '💼' },
    'BusinessLogicExtractor': { name: 'Business Logic', color: '#10b981', icon: '💼' },
    'DependencyMapperAgent': { name: 'Dependency Mapper', color: '#f59e0b', icon: '🔗' },
    'DependencyMapper': { name: 'Dependency Mapper', color: '#f59e0b', icon: '🔗' },
    'ChunkAwareCSharpConverter': { name: 'C# Converter', color: '#8b5cf6', icon: '⚙️' },
    'CSharpConverter': { name: 'C# Converter', color: '#8b5cf6', icon: '⚙️' },
    'JavaConverterAgent': { name: 'Java Converter', color: '#ec4899', icon: '☕' },
    'JavaConverter': { name: 'Java Converter', color: '#ec4899', icon: '☕' },
    'ChunkingOrchestrator': { name: 'Chunking Orchestrator', color: '#06b6d4', icon: '🧩' },
    'RateLimiter': { name: 'Rate Limiter', color: '#ef4444', icon: '⏱️' }
  };

  // State
  let currentRunId = null;
  let conversationMessages = [];
  let processStatus = {};
  let pollInterval = null;
  let isMonitorVisible = false;
  let lastKnownChunkState = null;
  let logEntries = [];
  let currentLogFilter = 'all';
  let autoScrollLogs = true;
  let migrationLogEntries = [];
  let autoScrollMigrationLog = true;
  let autoScrollAgentChat = true;
  let agentChatExpanded = false;

  // Auto-refresh intervals
  const MODAL_REFRESH_INTERVAL = 10000; // 10 seconds when modal is open (reduced from 3s)
  const HEADER_REFRESH_INTERVAL = 30000; // 30 seconds for mini dashboard (reduced from 5s)

  // Initialize when DOM is ready
  document.addEventListener('DOMContentLoaded', initMigrationMonitor);

  function initMigrationMonitor() {
    setupEventListeners();
    createFlowchartStages();
    createMiniDashboard();
    startHeaderDashboardUpdates();
  }

  // ============================================================================
  // FLOWCHART STAGES (Overview Tab)
  // ============================================================================

  function createFlowchartStages() {
    const flowchart = document.getElementById('monitorFlowchart');
    if (!flowchart) return;
    
    // Add status legend first
    const legendHtml = `
      <div class="status-legend">
        <div class="legend-title">Status Legend:</div>
        <div class="legend-items">
          ${Object.entries(STATUS_LEGEND).map(([status, info]) => `
            <div class="legend-item status-${status}" title="${info.description}">
              <span class="legend-icon">${info.icon}</span>
              <span class="legend-label">${info.label}</span>
            </div>
          `).join('')}
        </div>
      </div>
    `;
    
    const stagesHtml = PROCESS_STAGES.map((stage, index) => `
      <div class="flowchart-stage status-pending" id="stage-${stage.id}" data-stage="${stage.id}" 
           title="${stage.statusHints?.pending || 'Pending'}">
        <div class="stage-icon">${stage.icon}</div>
        <div class="stage-content">
          <div class="stage-name">${stage.name}</div>
          <div class="stage-details">${stage.description}</div>
          <div class="stage-hint" id="hint-${stage.id}">${stage.statusHints?.pending || ''}</div>
        </div>
        <div class="stage-status-indicator" title="Pending">⏸</div>
      </div>
      ${index < PROCESS_STAGES.length - 1 ? '<div class="flowchart-connector">↓</div>' : ''}
    `).join('');
    
    flowchart.innerHTML = legendHtml + stagesHtml;
  }

  // ============================================================================
  // MINI DASHBOARD (Header)
  // ============================================================================

  function createMiniDashboard() {
    const headerRight = document.querySelector('.header-right');
    if (!headerRight) return;

    // Check if already exists
    if (document.getElementById('chunk-mini-dashboard')) return;

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
    miniDashboard.addEventListener('click', openMigrationMonitor);
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

    if (!data || !data.files || data.files.length === 0) {
      if (progressFill) progressFill.style.width = '0%';
      if (progressText) progressText.textContent = data?.usingDirectProcessing ? 'Skipped' : '-';
      if (chunksComplete) chunksComplete.textContent = '0/0';
      if (statusEl) {
        statusEl.className = data?.usingDirectProcessing ? 'mini-dash-status complete' : 'mini-dash-status idle';
        const statusTextEl = statusEl.querySelector('.status-text');
        if (statusTextEl) statusTextEl.textContent = data?.usingDirectProcessing ? 'Skipped (small files)' : 'No data';
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
      const statusTextEl = statusEl.querySelector('.status-text');
      if (statusTextEl) statusTextEl.textContent = statusText;
    }
  }

  // ============================================================================
  // EVENT LISTENERS
  // ============================================================================

  function setupEventListeners() {
    // Open modal button
    const openBtn = document.getElementById('showMigrationMonitorBtn');
    if (openBtn) {
      openBtn.addEventListener('click', openMigrationMonitor);
    }

    // Close modal button
    const closeBtn = document.querySelector('.migration-monitor-close');
    if (closeBtn) {
      closeBtn.addEventListener('click', closeMigrationMonitor);
    }

    // Click outside to close
    window.addEventListener('click', (e) => {
      const modal = document.getElementById('migrationMonitorModal');
      if (e.target === modal) {
        closeMigrationMonitor();
      }
    });

    // Tab switching
    document.querySelectorAll('.monitor-tab-btn').forEach(btn => {
      btn.addEventListener('click', (e) => {
        const tabName = e.target.dataset.tab;
        switchTab(tabName);
      });
    });

    // Agent filter
    document.getElementById('agentFilter')?.addEventListener('change', renderConversations);

    // Log type filter buttons
    document.querySelectorAll('.log-type-btn').forEach(btn => {
      btn.addEventListener('click', (e) => {
        document.querySelectorAll('.log-type-btn').forEach(b => b.classList.remove('active'));
        e.target.classList.add('active');
        currentLogFilter = e.target.dataset.log;
        renderLogs();
      });
    });

    // Auto-scroll toggle
    document.getElementById('logAutoScroll')?.addEventListener('change', (e) => {
      autoScrollLogs = e.target.checked;
    });

    // Clear logs button
    document.getElementById('clearLogsBtn')?.addEventListener('click', () => {
      logEntries = [];
      renderLogs();
    });

    // Refresh logs button
    document.getElementById('refreshLogsBtn')?.addEventListener('click', loadLogs);

    // Migration Log tab event listeners
    document.getElementById('migrationLogAutoScroll')?.addEventListener('change', (e) => {
      autoScrollMigrationLog = e.target.checked;
    });
    
    document.getElementById('refreshMigrationLogBtn')?.addEventListener('click', loadMigrationLog);

    // Agent Chat tab event listeners
    document.getElementById('agentChatAutoScroll')?.addEventListener('change', (e) => {
      autoScrollAgentChat = e.target.checked;
      const container = document.getElementById('conversationsList');
      if (container) {
        container.classList.toggle('no-auto-scroll', !autoScrollAgentChat);
      }
    });

    document.getElementById('expandAllAgentsBtn')?.addEventListener('click', () => {
      agentChatExpanded = true;
      const container = document.getElementById('conversationsList');
      if (container) {
        container.classList.add('expanded');
        container.querySelectorAll('.expand-toggle').forEach(btn => {
          const id = btn.dataset.id;
          const fullContent = document.getElementById(`full-${id}`);
          const truncatedContent = document.getElementById(`truncated-${id}`);
          if (fullContent && truncatedContent) {
            fullContent.style.display = 'block';
            truncatedContent.style.display = 'none';
            const expandIcon = btn.querySelector('.expand-icon');
            if (expandIcon) expandIcon.textContent = '▼';
          }
        });
      }
    });

    document.getElementById('collapseAllAgentsBtn')?.addEventListener('click', () => {
      agentChatExpanded = false;
      const container = document.getElementById('conversationsList');
      if (container) {
        container.classList.remove('expanded');
        container.querySelectorAll('.expand-toggle').forEach(btn => {
          const id = btn.dataset.id;
          const fullContent = document.getElementById(`full-${id}`);
          const truncatedContent = document.getElementById(`truncated-${id}`);
          if (fullContent && truncatedContent) {
            fullContent.style.display = 'none';
            truncatedContent.style.display = 'block';
            const expandIcon = btn.querySelector('.expand-icon');
            if (expandIcon) expandIcon.textContent = '▶';
          }
        });
      }
    });

    document.getElementById('refreshAgentsBtn')?.addEventListener('click', loadAgentConversations);

    // Listen for run selector changes
    document.getElementById('run-selector')?.addEventListener('change', (e) => {
      currentRunId = parseInt(e.target.value);
      if (isMonitorVisible) {
        loadAllData();
      }
      updateMiniDashboard();
    });

    // Listen for new run detection events from run-selector
    document.addEventListener('newRunDetected', (e) => {
      const { runId } = e.detail;
      if (runId && runId !== currentRunId) {
        currentRunId = runId;
        if (isMonitorVisible) {
          loadAllData();
        }
        updateMiniDashboard();
        console.log(`🆕 New run detected: ${runId}`);
      }
    });
  }

  function openMigrationMonitor() {
    const modal = document.getElementById('migrationMonitorModal');
    if (modal) {
      modal.style.display = 'block';
      isMonitorVisible = true;
      
      // Use the main run selector's value
      const mainSelector = document.getElementById('run-selector');
      if (mainSelector && mainSelector.value) {
        currentRunId = parseInt(mainSelector.value);
      }
      
      loadAllData();
      startPolling();
    }
  }

  function closeMigrationMonitor() {
    const modal = document.getElementById('migrationMonitorModal');
    if (modal) {
      modal.style.display = 'none';
    }
    isMonitorVisible = false;
    stopPolling();
  }

  function switchTab(tabName) {
    // Update tab buttons
    document.querySelectorAll('.monitor-tab-btn').forEach(btn => {
      btn.classList.toggle('active', btn.dataset.tab === tabName);
    });
    
    // Update tab content
    document.querySelectorAll('.monitor-tab-content').forEach(content => {
      content.classList.toggle('active', content.id === `${tabName}Tab`);
    });
  }

  // ============================================================================
  // DATA LOADING
  // ============================================================================

  function loadAllData() {
    loadProcessStatus();
    loadChunkStatus();
    loadAgentConversations();
    loadRecentActivity();
    loadLogs();
    loadMigrationLog();
  }

  async function loadProcessStatus() {
    if (!currentRunId) {
      updateUINoRun();
      return;
    }

    try {
      const response = await fetch(`/api/runs/${currentRunId}/process-status`);
      const data = await response.json();
      
      if (data.error) {
        console.error('Process status error:', data.error);
        return;
      }
      
      processStatus = data;
      updateRunInfo(data);
      updateFlowchart(data);
      updateQuickStats(data);
    } catch (error) {
      console.error('Failed to load process status:', error);
    }
  }

  function updateUINoRun() {
    const runIdEl = document.getElementById('monitorRunId');
    const statusEl = document.getElementById('monitorRunStatus');
    const progressEl = document.getElementById('monitorProgress');
    const chunksEl = document.getElementById('monitorChunks');
    const tokensEl = document.getElementById('monitorTokens');
    const timeEl = document.getElementById('monitorTime');

    if (runIdEl) runIdEl.textContent = 'No run selected';
    if (statusEl) statusEl.textContent = '--';
    if (progressEl) progressEl.textContent = '0%';
    if (chunksEl) chunksEl.textContent = '0/0';
    if (tokensEl) tokensEl.textContent = '0';
    if (timeEl) timeEl.textContent = '0s';
  }

  function updateRunInfo(data) {
    const runIdEl = document.getElementById('monitorRunId');
    const statusEl = document.getElementById('monitorRunStatus');
    const progressEl = document.getElementById('monitorProgress');
    const chunksEl = document.getElementById('monitorChunks');
    const tokensEl = document.getElementById('monitorTokens');
    const timeEl = document.getElementById('monitorTime');

    if (runIdEl) runIdEl.textContent = `#${data.runId}`;
    
    if (statusEl) {
      statusEl.textContent = data.runStatus || '--';
      statusEl.className = `run-status status-${(data.runStatus || '').toLowerCase()}`;
    }
    
    // Update header status badge
    if (typeof window.updateRunStatusBadge === 'function') {
      window.updateRunStatusBadge(data.runStatus);
    }
    
    const stats = data.stats || {};
    if (progressEl) progressEl.textContent = `${stats.progressPercentage || 0}%`;
    if (chunksEl) chunksEl.textContent = `${stats.completedChunks || 0}/${stats.totalChunks || 0}`;
    if (tokensEl) tokensEl.textContent = formatNumber(stats.totalTokens || 0);
    if (timeEl) timeEl.textContent = formatDuration(stats.totalTimeMs || 0);
  }

  function updateFlowchart(data) {
    const stages = data.stages || [];
    
    stages.forEach(stage => {
      const stageEl = document.getElementById(`stage-${stage.id}`);
      if (!stageEl) return;

      // Find stage config for hints
      const stageConfig = PROCESS_STAGES.find(s => s.id === stage.id);
      const statusHint = stageConfig?.statusHints?.[stage.status] || '';
      const statusInfo = STATUS_LEGEND[stage.status] || STATUS_LEGEND.pending;

      // Remove old status classes
      stageEl.classList.remove('status-pending', 'status-running', 'status-completed', 'status-failed');
      stageEl.classList.add(`status-${stage.status || 'pending'}`);
      
      // Update tooltip
      stageEl.title = statusHint || statusInfo.description;
      
      // Update status indicator
      const indicator = stageEl.querySelector('.stage-status-indicator');
      if (indicator) {
        indicator.textContent = statusInfo.icon;
        indicator.title = `${statusInfo.label}: ${statusHint}`;
      }
      
      // Update details
      const detailsEl = stageEl.querySelector('.stage-details');
      if (detailsEl && stage.details) {
        detailsEl.textContent = stage.details;
      }
      
      // Update hint
      const hintEl = document.getElementById(`hint-${stage.id}`);
      if (hintEl) {
        hintEl.textContent = statusHint;
        hintEl.className = `stage-hint hint-${stage.status || 'pending'}`;
      }
    });
  }

  function updateQuickStats(data) {
    const stats = data.stats || {};
    
    // Update sidebar quick stats
    const qsPending = document.getElementById('qsPending');
    const qsProcessing = document.getElementById('qsProcessing');
    const qsCompleted = document.getElementById('qsCompleted');
    const qsFailed = document.getElementById('qsFailed');
    
    if (qsPending) qsPending.textContent = stats.pendingChunks || 0;
    if (qsProcessing) qsProcessing.textContent = stats.processingChunks || 0;
    if (qsCompleted) qsCompleted.textContent = stats.completedChunks || 0;
    if (qsFailed) qsFailed.textContent = stats.failedChunks || 0;
  }

  async function loadRecentActivity() {
    const container = document.getElementById('recentActivityMini');
    if (!container) return;

    try {
      // Pass current runId to filter activity for this run only
      const url = currentRunId ? `/api/activity-feed?runId=${currentRunId}` : '/api/activity-feed';
      const response = await fetch(url);
      const data = await response.json();
      
      const activities = (data.activities || []).slice(0, 5);
      
      if (activities.length === 0) {
        container.innerHTML = '<p class="placeholder-text">No recent activity</p>';
        return;
      }

      container.innerHTML = activities.map(activity => `
        <div class="activity-mini-item activity-${activity.type || 'info'}">
          <span class="activity-icon">${getActivityIcon(activity.type)}</span>
          <span class="activity-message">${escapeHtml(activity.message).substring(0, 50)}${activity.message.length > 50 ? '...' : ''}</span>
        </div>
      `).join('');
    } catch (error) {
      console.debug('Failed to load recent activity:', error);
    }
  }

  // ============================================================================
  // CHUNKS TAB
  // ============================================================================

  async function loadChunkStatus(silent = false) {
    const container = document.getElementById('chunkFilesList');
    if (!container) return;

    if (!currentRunId) {
      container.innerHTML = '<p class="error">Please select a migration run first</p>';
      return;
    }

    if (!silent) {
      container.innerHTML = '<p class="loading">Loading chunk status...</p>';
    }

    try {
      const response = await fetch(`/api/runs/${currentRunId}/chunks`);
      
      if (!response.ok) {
        throw new Error(`HTTP ${response.status}: ${response.statusText}`);
      }

      const data = await response.json();
      
      if (data.error) {
        if (!silent) {
          container.innerHTML = `<p class="error">${data.error}</p>`;
        }
        return;
      }

      // Check for changes before updating (reduces flicker)
      const newState = JSON.stringify(data);
      if (silent && newState === lastKnownChunkState) {
        return; // No changes, skip update
      }
      lastKnownChunkState = newState;

      displayChunkStatus(data, container);
      updateMiniDashboardUI(data);
    } catch (error) {
      if (!silent) {
        console.error('Error loading chunk status:', error);
        container.innerHTML = `<p class="error">Failed to load chunk status: ${error.message}</p>`;
      }
    }
  }

  function displayChunkStatus(data, container) {
    const files = data.files || [];

    if (files.length === 0 && data.usingDirectProcessing) {
      container.innerHTML = `
        <div class="chunk-empty-state">
          <div class="empty-icon">🧩</div>
          <p class="info">Smart chunking skipped</p>
          <p class="hint">All files were under the chunk threshold and processed directly.</p>
          <div class="auto-refresh-indicator">
            <span class="refresh-dot"></span>
            Live monitoring continues for conversion and assembly.
          </div>
        </div>
      `;
      return;
    }

    if (files.length === 0) {
      container.innerHTML = `
        <div class="chunk-empty-state">
          <div class="empty-icon">🧩</div>
          <p class="info">No chunked files found for this run.</p>
          <p class="hint">Files are chunked when they exceed the configured line limit.</p>
          <div class="auto-refresh-indicator">
            <span class="refresh-dot"></span>
            Auto-refreshing...
          </div>
        </div>
      `;
      return;
    }

    // Calculate overall progress
    const totalChunks = files.reduce((sum, f) => sum + f.totalChunks, 0);
    const completedChunks = files.reduce((sum, f) => sum + f.completedChunks, 0);
    const failedChunks = files.reduce((sum, f) => sum + f.failedChunks, 0);
    const processingChunks = files.reduce((sum, f) => sum + (f.pendingChunks || 0), 0);
    const overallProgress = totalChunks > 0 ? (completedChunks / totalChunks * 100) : 0;

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
      </div>
    `;
    
    html += '<div class="chunk-files-grid">';
    
    files.forEach(file => {
      const progressClass = getProgressClass(file.progressPercentage);
      const statusIcon = getFileStatusIcon(file);
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
          </div>
          ${file.failedChunks > 0 ? `<div class="chunk-file-error">⚠️ ${file.failedChunks} failed chunks</div>` : ''}
          ${isActive ? '<div class="processing-indicator"><span class="processing-dot"></span>Processing...</div>' : ''}
        </div>
      `;
    });

    html += '</div>';
    html += `<div class="last-updated">Last updated: ${new Date().toLocaleTimeString()}</div>`;
    
    container.innerHTML = html;

    // Add click handlers for file cards
    document.querySelectorAll('.chunk-file-card').forEach(card => {
      card.addEventListener('click', () => {
        const fileName = card.dataset.file;
        loadChunkDetails(fileName);
      });
    });
  }

  async function loadChunkDetails(fileName) {
    const detailsSection = document.getElementById('chunkDetails');
    const fileNameEl = document.getElementById('chunkDetailsFile');
    const contentEl = document.getElementById('chunkDetailsContent');
    
    if (!detailsSection || !contentEl) return;

    detailsSection.style.display = 'block';
    if (fileNameEl) fileNameEl.textContent = fileName;
    contentEl.innerHTML = '<p class="loading">Loading chunk details...</p>';

    try {
      const response = await fetch(`/api/runs/${currentRunId}/chunks/${encodeURIComponent(fileName)}`);
      
      if (!response.ok) {
        throw new Error(`HTTP ${response.status}: ${response.statusText}`);
      }

      const data = await response.json();
      displayChunkDetails(data, contentEl);
    } catch (error) {
      console.error('Error loading chunk details:', error);
      contentEl.innerHTML = `<p class="error">Failed to load chunk details: ${error.message}</p>`;
    }
  }

  function displayChunkDetails(data, container) {
    const chunks = data.chunks || [];
    
    if (chunks.length === 0) {
      container.innerHTML = '<p class="info">No chunk details available.</p>';
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
    container.innerHTML = html;
  }

  // ============================================================================
  // AGENT CHAT TAB
  // ============================================================================

  async function loadAgentConversations() {
    if (!currentRunId) return;

    const container = document.getElementById('conversationsList');
    if (!container) return;

    container.innerHTML = '<p class="loading-text">Loading agent conversations...</p>';

    try {
      const response = await fetch(`/api/runs/${currentRunId}/agent-conversations`);
      const data = await response.json();
      
      conversationMessages = data.conversations || [];
      renderConversations();
    } catch (error) {
      console.error('Failed to load conversations:', error);
      container.innerHTML = '<p class="error-text">Failed to load conversations</p>';
    }
  }

  function renderConversations() {
    const container = document.getElementById('conversationsList');
    if (!container) return;

    const filterEl = document.getElementById('agentFilter');
    const filterValue = filterEl ? filterEl.value : '';
    
    if (!conversationMessages || conversationMessages.length === 0) {
      container.innerHTML = '<p class="placeholder-text">No agent conversations recorded</p>';
      return;
    }

    const filtered = filterValue 
      ? conversationMessages.filter(msg => (msg.agent || '').includes(filterValue))
      : conversationMessages;

    if (filtered.length === 0) {
      container.innerHTML = '<p class="placeholder-text">No conversations match the filter</p>';
      return;
    }

    container.innerHTML = filtered.map((msg, idx) => `
      <div class="conversation-item ${msg.isSuccess ? 'success' : 'error'}">
        <div class="conversation-header">
          ${getAgentBadge(msg.agent)}
          <span class="conversation-method">${msg.method || 'API Call'}</span>
          <span class="conversation-model">${msg.model || ''}</span>
          <span class="conversation-time">${formatTime(msg.timestamp)}</span>
        </div>
        <div class="conversation-details">
          <span class="detail-item">⏱️ ${(msg.durationMs || 0).toFixed(0)}ms</span>
          <span class="detail-item">🔢 ${msg.tokensUsed || 0} tokens</span>
          <span class="detail-item status-${msg.isSuccess ? 'success' : 'error'}">${msg.isSuccess ? '✓ Success' : '✗ Error'}</span>
        </div>
        ${msg.request ? renderExpandableContent('Request', msg.request, `req-${idx}`, 500) : ''}
        ${msg.response ? renderExpandableContent('Response', msg.response, `resp-${idx}`, 1000) : ''}
        ${msg.error ? renderExpandableContent('Error', msg.error, `err-${idx}`, 500, 'error') : ''}
      </div>
    `).join('');

    // Attach expand/collapse handlers
    container.querySelectorAll('.expand-toggle').forEach(btn => {
      btn.addEventListener('click', (e) => {
        e.stopPropagation();
        const id = e.currentTarget.dataset.id;
        const fullContent = document.getElementById(`full-${id}`);
        const truncatedContent = document.getElementById(`truncated-${id}`);
        const expandIcon = e.currentTarget.querySelector('.expand-icon');
        const expandText = e.currentTarget.querySelector('.expand-text');
        
        if (fullContent && truncatedContent) {
          const isExpanded = fullContent.style.display !== 'none';
          fullContent.style.display = isExpanded ? 'none' : 'block';
          truncatedContent.style.display = isExpanded ? 'block' : 'none';
          if (expandIcon) expandIcon.textContent = isExpanded ? '▶' : '▼';
          if (expandText) {
            const label = e.currentTarget.closest('.conversation-request') ? 'request' : 'response';
            expandText.textContent = isExpanded ? `Show full ${label}` : `Hide ${label}`;
          }
        }
      });
    });

    // Attach copy button handlers
    container.querySelectorAll('.copy-btn').forEach(btn => {
      btn.addEventListener('click', async (e) => {
        e.stopPropagation();
        const id = e.target.dataset.content;
        const fullContent = document.getElementById(`full-${id}`)?.querySelector('.full-content');
        if (fullContent) {
          try {
            await navigator.clipboard.writeText(fullContent.textContent);
            e.target.textContent = '✅ Copied!';
            setTimeout(() => { e.target.textContent = '📋 Copy'; }, 2000);
          } catch (err) {
            console.error('Copy failed:', err);
          }
        }
      });
    });

    // Apply expanded state and handle auto-scroll
    if (agentChatExpanded) {
      container.classList.add('expanded');
    }
    if (!autoScrollAgentChat) {
      container.classList.add('no-auto-scroll');
    } else {
      // Auto-scroll to bottom when new content is added
      container.scrollTop = container.scrollHeight;
    }
  }

  // ============================================================================
  // LOGS TAB
  // ============================================================================

  async function loadLogs() {
    if (!currentRunId) return;

    try {
      // Load from activity feed (filtered by runId) and agent conversations
      const [activityResponse, conversationsResponse] = await Promise.all([
        fetch(`/api/activity-feed?runId=${currentRunId}`),
        fetch(`/api/runs/${currentRunId}/agent-conversations`)
      ]);

      const activityData = await activityResponse.json();
      const conversationsData = await conversationsResponse.json();

      logEntries = [];

      // Add activity feed entries
      (activityData.activities || []).forEach(activity => {
        logEntries.push({
          type: activity.type === 'api_call' ? 'api' : (activity.type === 'error' ? 'console' : 'console'),
          timestamp: activity.timestamp,
          level: activity.type === 'error' ? 'error' : 'info',
          source: activity.agent || 'System',
          message: activity.message,
          details: activity.details
        });
      });

      // Add agent conversation entries
      (conversationsData.conversations || []).forEach(conv => {
        logEntries.push({
          type: 'agents',
          timestamp: conv.timestamp,
          level: conv.isSuccess ? 'info' : 'error',
          source: conv.agent || 'Agent',
          message: `${conv.method || 'API Call'} - ${conv.isSuccess ? 'Success' : 'Failed'}`,
          details: conv.response || conv.error,
          tokensUsed: conv.tokensUsed,
          durationMs: conv.durationMs
        });

        // Also add as API entry
        logEntries.push({
          type: 'api',
          timestamp: conv.timestamp,
          level: conv.isSuccess ? 'info' : 'error',
          source: conv.model || 'API',
          message: `${conv.agent}: ${conv.tokensUsed || 0} tokens, ${conv.durationMs || 0}ms`,
          details: conv.request?.substring(0, 500)
        });
      });

      // Sort by timestamp descending
      logEntries.sort((a, b) => new Date(b.timestamp) - new Date(a.timestamp));

      renderLogs();
    } catch (error) {
      console.error('Failed to load logs:', error);
      const container = document.getElementById('logEntries');
      if (container) {
        container.innerHTML = `<p class="error-text">Failed to load logs: ${error.message}</p>`;
      }
    }
  }

  function renderLogs() {
    const container = document.getElementById('logEntries');
    const countEl = document.getElementById('logCount');
    const lastUpdateEl = document.getElementById('logLastUpdate');
    
    if (!container) return;

    // Filter logs based on current filter
    const filtered = currentLogFilter === 'all' 
      ? logEntries 
      : logEntries.filter(log => log.type === currentLogFilter);

    if (countEl) countEl.textContent = `${filtered.length} entries`;
    if (lastUpdateEl) lastUpdateEl.textContent = `Last update: ${new Date().toLocaleTimeString()}`;

    if (filtered.length === 0) {
      container.innerHTML = `
        <div class="log-empty-state">
          <span class="empty-icon">📜</span>
          <p>No log entries found</p>
          <p class="hint">Logs will appear here during migration</p>
        </div>
      `;
      return;
    }

    container.innerHTML = filtered.map((log, idx) => `
      <div class="log-entry log-${log.level} log-type-${log.type}">
        <div class="log-entry-header">
          <span class="log-timestamp">${formatTime(log.timestamp)}</span>
          <span class="log-level ${log.level}">${getLogLevelIcon(log.level)}</span>
          <span class="log-source">${escapeHtml(log.source)}</span>
          ${log.tokensUsed ? `<span class="log-tokens">🔢 ${log.tokensUsed}</span>` : ''}
          ${log.durationMs ? `<span class="log-duration">⏱️ ${log.durationMs}ms</span>` : ''}
        </div>
        <div class="log-message">${escapeHtml(log.message)}</div>
        ${log.details ? `
          <div class="log-details-toggle" data-log-id="${idx}">
            <button class="log-expand-btn">▶ Details</button>
          </div>
          <div class="log-details" id="log-details-${idx}" style="display:none;">
            <pre>${escapeHtml(typeof log.details === 'string' ? log.details : JSON.stringify(log.details, null, 2))}</pre>
          </div>
        ` : ''}
      </div>
    `).join('');

    // Attach expand handlers for log details
    container.querySelectorAll('.log-expand-btn').forEach(btn => {
      btn.addEventListener('click', (e) => {
        const toggle = e.target.closest('.log-details-toggle');
        const logId = toggle.dataset.logId;
        const details = document.getElementById(`log-details-${logId}`);
        if (details) {
          const isExpanded = details.style.display !== 'none';
          details.style.display = isExpanded ? 'none' : 'block';
          e.target.textContent = isExpanded ? '▶ Details' : '▼ Details';
        }
      });
    });

    // Auto-scroll to bottom if enabled
    if (autoScrollLogs) {
      const logWindow = document.getElementById('logWindow');
      if (logWindow) {
        logWindow.scrollTop = logWindow.scrollHeight;
      }
    }
  }

  function getLogLevelIcon(level) {
    switch (level) {
      case 'error': return '❌';
      case 'warn': return '⚠️';
      case 'info': return 'ℹ️';
      case 'debug': return '🔍';
      default: return '📋';
    }
  }

  // ============================================================================
  // MIGRATION LOG TAB
  // ============================================================================

  async function loadMigrationLog() {
    if (!currentRunId) return;

    const container = document.getElementById('migrationLogEntries');
    const countEl = document.getElementById('migrationLogCount');
    const lastUpdateEl = document.getElementById('migrationLogLastUpdate');
    const targetLangBadge = document.getElementById('migrationTargetLanguage');
    const statusText = document.getElementById('migrationStatusText');

    try {
      const response = await fetch(`/api/runs/${currentRunId}/migration-log?lines=500`);
      const data = await response.json();

      if (data.error) {
        if (container) {
          container.innerHTML = `<p class="error-text">Error: ${data.error}</p>`;
        }
        return;
      }

      // Update target language badge
      if (targetLangBadge) {
        const lang = (data.targetLanguage || 'csharp').toLowerCase();
        targetLangBadge.textContent = lang === 'csharp' ? '⚙️ C#' : '☕ Java';
        targetLangBadge.className = `target-language-badge ${lang}`;
      }

      // Update status text
      if (statusText) {
        const status = data.runStatus || 'Unknown';
        statusText.textContent = status;
        statusText.className = `migration-status-text ${status.toLowerCase()}`;
      }

      // Update header badges with current run status
      if (window.updateRunBadges && currentRunId) {
        window.updateRunBadges(currentRunId);
      }

      // Update overall progress bar in Overview tab
      updateOverallProgress(data);

      // Store entries
      migrationLogEntries = data.entries || [];

      renderMigrationLog();

      renderLiveRunLog(data.liveLogTail || []);
      renderActiveFiles(data.activeFiles || []);

      if (countEl) countEl.textContent = `${migrationLogEntries.length} entries`;
      if (lastUpdateEl) lastUpdateEl.textContent = `Last update: ${new Date().toLocaleTimeString()}`;

    } catch (error) {
      console.error('Failed to load migration log:', error);
      if (container) {
        container.innerHTML = `<p class="error-text">Failed to load migration log: ${error.message}</p>`;
      }
    }
  }

  function renderMigrationLog() {
    const container = document.getElementById('migrationLogEntries');
    if (!container) return;

    if (migrationLogEntries.length === 0) {
      container.innerHTML = `
        <div class="migration-log-empty">
          <span class="empty-icon">📄</span>
          <p>No migration log entries found</p>
          <p class="hint">Start a migration to see the process log here</p>
        </div>
      `;
      return;
    }

    container.innerHTML = migrationLogEntries.map(entry => {
      const timestamp = entry.timestamp ? formatTimestamp(entry.timestamp) : '';
      const level = entry.level || 'info';
      const type = entry.type || 'step';
      const category = entry.category || 'LOG';
      
      // Parse message for step and status
      const message = entry.message || '';
      let formattedMessage = escapeHtml(message);
      
      // Highlight step names and statuses
      formattedMessage = formattedMessage
        .replace(/Step:\s*(\w+)/g, 'Step: <span class="step-name">$1</span>')
        .replace(/Status:\s*(IN_PROGRESS)/g, 'Status: <span class="status-badge in-progress">$1</span>')
        .replace(/Status:\s*(COMPLETED)/g, 'Status: <span class="status-badge completed">$1</span>')
        .replace(/Status:\s*(FAILED)/g, 'Status: <span class="status-badge failed">$1</span>');

      return `
        <div class="migration-log-entry level-${level} type-${type}">
          <span class="log-entry-timestamp">${timestamp}</span>
          <span class="log-entry-category">${escapeHtml(category)}</span>
          <span class="log-entry-message">${formattedMessage}</span>
        </div>
      `;
    }).join('');

    // Auto-scroll to bottom if enabled
    if (autoScrollMigrationLog) {
      const logWindow = document.getElementById('migrationLogWindow');
      if (logWindow) {
        logWindow.scrollTop = logWindow.scrollHeight;
      }
    }
  }

  function renderLiveRunLog(lines) {
    const container = document.getElementById('liveRunLogEntries');
    const windowEl = document.getElementById('liveRunLogWindow');
    if (!container) return;

    if (!lines || lines.length === 0) {
      container.innerHTML = '<p class="placeholder-text">No log lines yet</p>';
      return;
    }

    container.innerHTML = lines.map(line => {
      const text = typeof line === 'string' ? line : (line.text || '');
      return `<div class="live-log-line"><span class="live-log-text">${escapeHtml(text)}</span></div>`;
    }).join('');

    if (autoScrollMigrationLog && windowEl) {
      windowEl.scrollTop = windowEl.scrollHeight;
    }
  }

  function renderActiveFiles(activeFiles) {
    const container = document.getElementById('activeFilesList');
    if (!container) return;

    if (!activeFiles || activeFiles.length === 0) {
      container.innerHTML = '<p class="placeholder-text">Idle (no active files)</p>';
      return;
    }

    container.innerHTML = activeFiles.map(file => {
      const when = file.lastSeen ? formatTimestamp(file.lastSeen) : '';
      return `
        <div class="active-file-row">
          <div class="active-file-name">${escapeHtml(file.file || '')}</div>
          <div class="active-file-meta">
            <span class="active-file-stage">${escapeHtml(file.stage || '')}</span>
            ${when ? `<span class="active-file-time">${when}</span>` : ''}
          </div>
        </div>
      `;
    }).join('');
  }

  function updateOverallProgress(data) {
    const progressPercent = document.getElementById('overallProgressPercent');
    const progressFill = document.getElementById('overallProgressFill');
    const progressStatus = document.getElementById('overallProgressStatus');
    const progressTarget = document.getElementById('overallProgressTarget');
    const parallelWorkers = document.getElementById('parallelWorkers');

    if (!progressPercent || !progressFill) return;

    // Calculate progress based on run status and stages
    let progress = 0;
    let statusText = 'Waiting for migration...';

    const runStatus = data.runStatus || 'Unknown';
    
    if (runStatus === 'Completed') {
      progress = 100;
      statusText = '✅ Migration completed successfully';
    } else if (runStatus === 'Failed') {
      progress = processStatus?.stats?.progressPercentage || 0;
      statusText = '❌ Migration failed';
    } else if (runStatus === 'Running' || runStatus === 'In Progress') {
      progress = processStatus?.stats?.progressPercentage || 0;
      const liveProgress = data.liveProgress;
      if (liveProgress) {
        statusText = `🔄 ${liveProgress.currentStep || 'Processing'}...`;
      } else {
        statusText = '🔄 Migration in progress...';
      }
    } else if (data.entries && data.entries.length > 0) {
      // Estimate progress from log entries
      const lastEntry = data.entries[data.entries.length - 1];
      if (lastEntry.message?.includes('STEP_1')) progress = 10;
      else if (lastEntry.message?.includes('STEP_2')) progress = 25;
      else if (lastEntry.message?.includes('STEP_3')) progress = 40;
      else if (lastEntry.message?.includes('STEP_4')) progress = 55;
      else if (lastEntry.message?.includes('STEP_5')) progress = 70;
      else if (lastEntry.message?.includes('STEP_6')) progress = 85;
      else if (lastEntry.message?.includes('COMPLETE')) progress = 100;
      statusText = '🔄 Processing...';
    }

    progressPercent.textContent = `${progress.toFixed(0)}%`;
    progressFill.style.width = `${progress}%`;
    
    if (progressStatus) progressStatus.textContent = statusText;
    if (progressTarget) {
      const lang = (data.targetLanguage || 'unknown').toUpperCase();
      progressTarget.textContent = `Target: ${lang === 'CSHARP' ? 'C#' : lang}`;
    }
    
    // Update parallel workers indicator
    if (parallelWorkers) {
      const stats = processStatus?.stats || {};
      const activeWorkers = stats.activeWorkers || stats.processingChunks || 0;
      const maxWorkers = stats.maxParallelWorkers || 6;
      const isRunning = runStatus === 'Running' || runStatus === 'In Progress';
      
      if (activeWorkers > 0) {
        parallelWorkers.textContent = `Workers: 🚀 ${activeWorkers}/${maxWorkers}`;
        parallelWorkers.className = 'progress-stat workers-active';
      } else if (isRunning) {
        parallelWorkers.textContent = `Workers: 🚀 ${maxWorkers} parallel`;
        parallelWorkers.className = 'progress-stat workers-active';
      } else {
        parallelWorkers.textContent = `Workers: ⏸️ idle`;
        parallelWorkers.className = 'progress-stat workers-idle';
      }
    }
  }

  function formatTimestamp(timestamp) {
    if (!timestamp) return '';
    // Handle various timestamp formats:
    // - ISO: "2025-12-03T12:45:07.637Z"  
    // - Custom: "2025-12-03 12.45.07.637"
    // - Standard: "2025-12-03 12:45:07"
    try {
      // First try parsing as-is (handles ISO format)
      let date = new Date(timestamp);
      if (!isNaN(date.getTime())) {
        return date.toLocaleTimeString();
      }
      
      // Handle custom format with dots instead of colons in time: "2025-12-03 12.45.07.637"
      if (typeof timestamp === 'string' && timestamp.includes(' ')) {
        const parts = timestamp.split(' ');
        if (parts.length >= 2) {
          const datePart = parts[0];
          let timePart = parts[1];
          
          // Replace dots with colons in time part (12.45.07.637 -> 12:45:07.637)
          // Match HH.MM.SS pattern and replace first two dots with colons
          timePart = timePart.replace(/^(\d{2})\.(\d{2})\.(\d{2})/, '$1:$2:$3');
          
          // Try parsing the corrected timestamp
          const corrected = `${datePart}T${timePart}`;
          date = new Date(corrected);
          if (!isNaN(date.getTime())) {
            return date.toLocaleTimeString();
          }
          
          // Final fallback: just return the time portion formatted
          return timePart.substring(0, 8);
        }
      }
    } catch {
      // Silent catch
    }
    return timestamp;
  }

  // ============================================================================
  // POLLING
  // ============================================================================

  function startPolling() {
    if (pollInterval) return;
    
    pollInterval = setInterval(() => {
      if (currentRunId && isMonitorVisible && window.pageIsVisible !== false) {
        loadProcessStatus();
        loadChunkStatus(true); // silent refresh
        loadRecentActivity();
        loadMigrationLog(); // Always refresh migration log
        
        // Only refresh general logs if run is still running
        if (processStatus && processStatus.runStatus !== 'Completed' && processStatus.runStatus !== 'Failed') {
          loadLogs();
        }
      }
    }, MODAL_REFRESH_INTERVAL);
  }

  function stopPolling() {
    if (pollInterval) {
      clearInterval(pollInterval);
      pollInterval = null;
    }
  }

  // ============================================================================
  // UTILITY FUNCTIONS
  // ============================================================================

  function getActivityIcon(type) {
    switch (type) {
      case 'chunk': return '🧩';
      case 'run': return '🚀';
      case 'api_call': return '📡';
      case 'error': return '❌';
      default: return '📋';
    }
  }

  function getAgentBadge(agentName) {
    const agent = AGENTS[agentName] || { name: agentName || 'Unknown', color: '#64748b', icon: '🤖' };
    return `<span class="agent-badge" style="background: ${agent.color}">${agent.icon} ${agent.name}</span>`;
  }

  function getProgressClass(percentage) {
    if (percentage >= 100) return 'progress-complete';
    if (percentage >= 75) return 'progress-high';
    if (percentage >= 50) return 'progress-medium';
    if (percentage >= 25) return 'progress-low';
    return 'progress-start';
  }

  function getFileStatusIcon(file) {
    if (file.failedChunks > 0) return '❌';
    if (file.progressPercentage >= 100) return '✅';
    if (file.pendingChunks > 0) return '⏳';
    return '🔄';
  }

  function getChunkStatusClass(status) {
    switch ((status || '').toLowerCase()) {
      case 'completed': return 'chunk-completed';
      case 'failed': return 'chunk-failed';
      case 'processing': return 'chunk-processing';
      default: return 'chunk-pending';
    }
  }

  function getChunkStatusIcon(status) {
    switch ((status || '').toLowerCase()) {
      case 'completed': return '✅';
      case 'failed': return '❌';
      case 'processing': return '🔄';
      default: return '⏳';
    }
  }

  function formatDuration(ms) {
    if (!ms) return '0s';
    const seconds = Math.floor(ms / 1000);
    const minutes = Math.floor(seconds / 60);
    const hours = Math.floor(minutes / 60);
    
    if (hours > 0) {
      return `${hours}h ${minutes % 60}m`;
    } else if (minutes > 0) {
      return `${minutes}m ${seconds % 60}s`;
    } else if (ms < 1000) {
      return `${ms}ms`;
    } else {
      return `${seconds}s`;
    }
  }

  function formatNumber(num) {
    if (!num) return '0';
    if (num >= 1000000) return `${(num / 1000000).toFixed(1)}M`;
    if (num >= 1000) return `${(num / 1000).toFixed(1)}K`;
    return num.toString();
  }

  function formatTime(timestamp) {
    if (!timestamp) return '';
    try {
      const date = new Date(timestamp);
      return date.toLocaleTimeString();
    } catch {
      return timestamp;
    }
  }

  function truncateText(text, maxLength) {
    if (!text) return '';
    const escaped = escapeHtml(text);
    if (escaped.length <= maxLength) return escaped;
    return escaped.substring(0, maxLength) + '...';
  }

  function renderExpandableContent(label, text, id, truncateLength, extraClass = '') {
    if (!text) return '';
    const escaped = escapeHtml(text);
    const needsExpand = escaped.length > truncateLength;
    const cssClass = extraClass ? `conversation-${label.toLowerCase()} ${extraClass}` : `conversation-${label.toLowerCase()}`;
    
    if (!needsExpand) {
      return `<div class="${cssClass}"><strong>${label}:</strong> <span class="content-text">${escaped}</span></div>`;
    }
    
    const truncated = escaped.substring(0, truncateLength) + '...';
    return `
      <div class="${cssClass} expandable-content">
        <div class="expandable-header">
          <strong>${label}:</strong>
          <button class="expand-toggle" data-id="${id}" title="Click to expand/collapse">
            <span class="expand-icon">▶</span>
            <span class="expand-text">Show full ${label.toLowerCase()}</span>
          </button>
        </div>
        <div id="truncated-${id}" class="content-text truncated-text">${truncated}</div>
        <div id="full-${id}" class="full-content-wrapper" style="display:none;">
          <div class="full-content-controls">
            <button class="copy-btn" data-content="${id}" title="Copy to clipboard">📋 Copy</button>
            <span class="content-length">${escaped.length.toLocaleString()} chars</span>
          </div>
          <pre class="full-content">${escaped}</pre>
        </div>
      </div>
    `;
  }

  function escapeHtml(text) {
    if (!text) return '';
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
  }

})();
