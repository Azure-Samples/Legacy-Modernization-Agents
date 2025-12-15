// Process Tracker - Visual flowchart and agent conversation viewer
(function() {
  'use strict';

  // Process stages configuration
  const PROCESS_STAGES = [
    { id: 'file-discovery', name: 'File Discovery', icon: '📂', description: 'Scanning for COBOL files' },
    { id: 'technical-analysis', name: 'Technical Analysis', icon: '🔬', description: 'Analyzing COBOL structure' },
    { id: 'business-logic', name: 'Business Logic', icon: '💼', description: 'Extracting business rules' },
    { id: 'dependency-mapping', name: 'Dependency Mapping', icon: '🔗', description: 'Building dependency graph' },
    { id: 'chunking', name: 'Smart Chunking', icon: '🧩', description: 'Splitting large files' },
    { id: 'conversion', name: 'Code Conversion', icon: '⚙️', description: 'Converting to target language' },
    { id: 'assembly', name: 'Code Assembly', icon: '🔧', description: 'Assembling converted chunks' },
    { id: 'output', name: 'Output Generation', icon: '📤', description: 'Writing output files' }
  ];

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

  let currentRunId = null;
  let conversationMessages = [];
  let processStatus = {};
  let pollInterval = null;
  let isTrackerVisible = false;

  // Initialize when DOM is ready
  document.addEventListener('DOMContentLoaded', initProcessTracker);

  function initProcessTracker() {
    setupEventListeners();
    createFlowchartStages();
  }

  function createFlowchartStages() {
    const flowchart = document.getElementById('processFlowchart');
    if (!flowchart) return;
    
    flowchart.innerHTML = PROCESS_STAGES.map((stage, index) => `
      <div class="flowchart-stage status-pending" id="stage-${stage.id}" data-stage="${stage.id}">
        <div class="stage-icon">${stage.icon}</div>
        <div class="stage-content">
          <div class="stage-name">${stage.name}</div>
          <div class="stage-details">${stage.description}</div>
        </div>
        <div class="stage-status-indicator">⏸</div>
      </div>
      ${index < PROCESS_STAGES.length - 1 ? '<div class="flowchart-connector">↓</div>' : ''}
    `).join('');
  }

  function setupEventListeners() {
    // Open modal button (using HTML button)
    const openBtn = document.getElementById('showProcessTrackerBtn');
    if (openBtn) {
      openBtn.addEventListener('click', () => {
        const modal = document.getElementById('processTrackerModal');
        if (modal) {
          modal.style.display = 'block';
          isTrackerVisible = true;
          
          // Use the main run selector's value
          const mainSelector = document.getElementById('run-selector');
          if (mainSelector && mainSelector.value) {
            currentRunId = parseInt(mainSelector.value);
          }
          
          loadProcessStatus();
          loadAgentConversations();
          loadActivityFeed();
          startPolling();
        }
      });
    }

    // Close modal
    const closeBtn = document.querySelector('.process-tracker-close');
    if (closeBtn) {
      closeBtn.addEventListener('click', () => {
        document.getElementById('processTrackerModal').style.display = 'none';
        isTrackerVisible = false;
        stopPolling();
      });
    }

    // Click outside to close
    window.addEventListener('click', (e) => {
      const modal = document.getElementById('processTrackerModal');
      if (e.target === modal) {
        modal.style.display = 'none';
        isTrackerVisible = false;
        stopPolling();
      }
    });

    // Tab switching
    document.querySelectorAll('.process-tab-btn').forEach(btn => {
      btn.addEventListener('click', (e) => {
        const tabName = e.target.dataset.tab;
        switchTab(tabName);
      });
    });

    // Refresh buttons
    document.getElementById('refreshProcessBtn')?.addEventListener('click', loadProcessStatus);
    document.getElementById('refreshConversationsBtn')?.addEventListener('click', loadAgentConversations);
    document.getElementById('refreshActivityBtn')?.addEventListener('click', loadActivityFeed);

    // Agent filter
    document.getElementById('agentFilter')?.addEventListener('change', renderConversations);

    // Listen for run selector changes
    document.getElementById('run-selector')?.addEventListener('change', (e) => {
      if (isTrackerVisible) {
        currentRunId = parseInt(e.target.value);
        loadProcessStatus();
        loadAgentConversations();
        loadActivityFeed();
      }
    });
  }

  function switchTab(tabName) {
    // Update tab buttons
    document.querySelectorAll('.process-tab-btn').forEach(btn => {
      btn.classList.toggle('active', btn.dataset.tab === tabName);
    });
    
    // Update tab content
    document.querySelectorAll('.process-tab-content').forEach(content => {
      content.classList.toggle('active', content.id === `${tabName}Tab`);
    });
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
      updateStats(data);
    } catch (error) {
      console.error('Failed to load process status:', error);
    }
  }

  function updateUINoRun() {
    document.getElementById('processRunId').textContent = 'No run selected';
    document.getElementById('processRunStatus').textContent = '--';
    document.getElementById('processProgress').textContent = '0%';
    document.getElementById('processChunks').textContent = '0/0';
    document.getElementById('processTokens').textContent = '0';
    document.getElementById('processTime').textContent = '0s';
  }

  function updateRunInfo(data) {
    document.getElementById('processRunId').textContent = `#${data.runId}`;
    
    const statusEl = document.getElementById('processRunStatus');
    statusEl.textContent = data.runStatus || '--';
    statusEl.className = `run-status status-${(data.runStatus || '').toLowerCase()}`;
    
    const stats = data.stats || {};
    document.getElementById('processProgress').textContent = `${stats.progressPercentage || 0}%`;
    document.getElementById('processChunks').textContent = `${stats.completedChunks || 0}/${stats.totalChunks || 0}`;
    document.getElementById('processTokens').textContent = formatNumber(stats.totalTokens || 0);
    document.getElementById('processTime').textContent = formatDuration(stats.totalTimeMs || 0);
  }

  function updateFlowchart(data) {
    const stages = data.stages || [];
    
    stages.forEach(stage => {
      const stageEl = document.getElementById(`stage-${stage.id}`);
      if (!stageEl) return;

      // Remove old status classes
      stageEl.classList.remove('status-pending', 'status-running', 'status-completed', 'status-failed');
      stageEl.classList.add(`status-${stage.status || 'pending'}`);
      
      // Update status indicator
      const indicator = stageEl.querySelector('.stage-status-indicator');
      if (indicator) {
        switch (stage.status) {
          case 'completed': indicator.textContent = '✓'; break;
          case 'running': indicator.textContent = '⏳'; break;
          case 'failed': indicator.textContent = '✗'; break;
          default: indicator.textContent = '⏸';
        }
      }
      
      // Update details
      const detailsEl = stageEl.querySelector('.stage-details');
      if (detailsEl && stage.details) {
        detailsEl.textContent = stage.details;
      }
    });
  }

  function updateStats(data) {
    const stats = data.stats || {};
    // Stats already updated in updateRunInfo
  }

  async function loadAgentConversations() {
    if (!currentRunId) return;

    const container = document.getElementById('conversationsList');
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
    const filterValue = document.getElementById('agentFilter')?.value || '';
    
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

    container.innerHTML = filtered.map(msg => `
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
        ${msg.request ? `<div class="conversation-request"><strong>Request:</strong> ${truncateText(msg.request, 200)}</div>` : ''}
        ${msg.response ? `<div class="conversation-response"><strong>Response:</strong> ${truncateText(msg.response, 300)}</div>` : ''}
        ${msg.error ? `<div class="conversation-error"><strong>Error:</strong> ${escapeHtml(msg.error)}</div>` : ''}
      </div>
    `).join('');
  }

  async function loadActivityFeed() {
    const container = document.getElementById('activityFeed');
    const summary = document.getElementById('activitySummary');
    
    container.innerHTML = '<p class="loading-text">Loading activity feed...</p>';

    try {
      const response = await fetch('/api/activity-feed');
      const data = await response.json();
      
      const activities = data.activities || [];
      summary.innerHTML = `<span class="activity-count">${activities.length} activities</span> | Last updated: ${formatTime(data.lastUpdated)}`;
      
      if (activities.length === 0) {
        container.innerHTML = '<p class="placeholder-text">No recent activity</p>';
        return;
      }

      container.innerHTML = activities.map(activity => `
        <div class="activity-item activity-${activity.type || 'info'}">
          <span class="activity-icon">${getActivityIcon(activity.type)}</span>
          <span class="activity-message">${escapeHtml(activity.message)}</span>
          <span class="activity-time">${formatTime(activity.timestamp)}</span>
        </div>
      `).join('');
    } catch (error) {
      console.error('Failed to load activity feed:', error);
      container.innerHTML = '<p class="error-text">Failed to load activity feed</p>';
    }
  }

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

  function startPolling() {
    if (pollInterval) return;
    
    pollInterval = setInterval(() => {
      if (currentRunId && isTrackerVisible && window.pageIsVisible !== false) {
        loadProcessStatus();
        loadActivityFeed();
      }
    }, 5000); // Poll every 5 seconds
  }

  function stopPolling() {
    if (pollInterval) {
      clearInterval(pollInterval);
      pollInterval = null;
    }
  }

  // Utility functions
  function formatDuration(ms) {
    if (!ms) return '0s';
    const seconds = Math.floor(ms / 1000);
    const minutes = Math.floor(seconds / 60);
    const hours = Math.floor(minutes / 60);
    
    if (hours > 0) {
      return `${hours}h ${minutes % 60}m`;
    } else if (minutes > 0) {
      return `${minutes}m ${seconds % 60}s`;
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

  function escapeHtml(text) {
    if (!text) return '';
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
  }

})();
