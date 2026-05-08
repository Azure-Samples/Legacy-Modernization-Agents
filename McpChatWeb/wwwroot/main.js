let resourcesList, refreshButton, chatForm, promptInput, responseCard, responseBody, loadingIndicator, loadingStages;
let runSummaryContainer, runSummaryBody, runSummaryButton, mcpCallVisual, mcpCallAscii;

function initializeElements() {
  resourcesList = document.getElementById('resources-list');
  refreshButton = document.getElementById('refresh-graph');
  chatForm = document.getElementById('chat-form');
  promptInput = document.getElementById('prompt');
  responseCard = document.getElementById('response');
  responseBody = document.getElementById('response-body');
  loadingIndicator = document.getElementById('loading-indicator');
  loadingStages = {
    db: document.getElementById('stage-db'),
    ai: document.getElementById('stage-ai'),
    response: document.getElementById('stage-response')
  };

  runSummaryContainer = document.getElementById('run-summary-container');
  runSummaryBody = document.getElementById('run-summary-body');
  runSummaryButton = document.getElementById('toggle-run-summary');
  mcpCallVisual = document.getElementById('mcp-call-visual');
  mcpCallAscii = document.getElementById('mcp-call-ascii');

  if (runSummaryButton) {
    runSummaryButton.addEventListener('click', toggleRunSummary);
  }
}

async function fetchResources() {
  toggleLoading(refreshButton, true);
  try {
    const res = await fetch('/api/resources', { headers: { 'Accept': 'application/json' } });
    if (!res.ok) {
      throw new Error(`HTTP ${res.status}`);
    }
    const payload = await res.json();
    // Backend may return either an array of resources or an envelope:
    //   { resources: [...], error?: "..." } when the MCP subprocess fails.
    let resources = payload;
    let error = null;
    if (payload && !Array.isArray(payload) && typeof payload === 'object') {
      resources = Array.isArray(payload.resources) ? payload.resources : [];
      if (payload.error) error = payload.error;
    }
    renderResources(resources, error);
  } catch (err) {
    renderResources([], `Failed to load resources: ${err.message}`);
  } finally {
    toggleLoading(refreshButton, false);
  }
}

function renderResources(resources, errorMessage) {
  if (!resourcesList) {
    console.warn('Resources list element not found');
    return;
  }
  resourcesList.innerHTML = '';
  if (errorMessage) {
    const errorItem = document.createElement('li');
    errorItem.classList.add('error');
    // Friendlier copy when the error is the well-known MCP-subprocess failure
    if (/MCP process|MCP subprocess|specified command or file/i.test(errorMessage)) {
      errorItem.innerHTML = `MCP resource server unavailable.<br>
        <span style="font-size:11px;color:#94a3b8;">The MCP subprocess could not start. The dashboards (AST Explorer, AST Galaxy, Migration Planner) keep working — they read directly from Neo4j / SQLite and don't need MCP.</span><br>
        <span style="font-size:10px;color:#64748b;">Detail: ${errorMessage}</span>`;
    } else {
      errorItem.textContent = errorMessage;
    }
    resourcesList.appendChild(errorItem);
    return;
  }

  if (!Array.isArray(resources) || resources.length === 0) {
    const emptyItem = document.createElement('li');
    emptyItem.textContent = 'No resources available yet. Run a migration first.';
    resourcesList.appendChild(emptyItem);
    return;
  }

  // Add example queries section at the top
  const queriesSection = document.createElement('div');
  queriesSection.className = 'example-queries';
  queriesSection.innerHTML = `
    <h3>💡 Example Queries</h3>
    <div class="query-grid">
      <div class="query-card">
        <strong>COBOL Analysis</strong>
        <ul>
          <li class="clickable-query" data-query="Describe the top 3 critical copybooks and what they do">"Describe the top 3 critical copybooks and what they do"</li>
          <li class="clickable-query" data-query="Suggest a carveout strategy and where I should start">"Suggest a carveout strategy and where I should start"</li>
          <li class="clickable-query" data-query="Give me an overview of what the COBOL code is responsible for">"Give me an overview of what the COBOL code is responsible for"</li>
          <li class="clickable-query" data-query="Which COBOL files have the highest impact?">"Which COBOL files have the highest impact?"</li>
        </ul>
      </div>
      <div class="query-card">
        <strong>Migration Planning</strong>
        <ul>
          <li class="clickable-query" data-query="What's the recommended migration order for these programs?">"What's the recommended migration order for these programs?"</li>
          <li class="clickable-query" data-query="Identify the main entry point programs">"Identify the main entry point programs"</li>
          <li class="clickable-query" data-query="Which copybooks are shared across the most programs?">"Which copybooks are shared across the most programs?"</li>
          <li class="clickable-query" data-query="Show complexity metrics for BDSMFJL.cbl">"Show complexity metrics for BDSMFJL.cbl"</li>
        </ul>
      </div>
      <div class="query-card">
        <strong>Java Conversion</strong>
        <ul>
          <li class="clickable-query" data-query="How would BDSDA23.cbl be structured in Java?">"How would BDSDA23.cbl be structured in Java?"</li>
          <li class="clickable-query" data-query="What Java patterns should replace COBOL copybooks?">"What Java patterns should replace COBOL copybooks?"</li>
          <li class="clickable-query" data-query="Suggest a Quarkus architecture for this codebase">"Suggest a Quarkus architecture for this codebase"</li>
          <li class="clickable-query" data-query="Compare COBOL file I/O with modern Java alternatives">"Compare COBOL file I/O with modern Java alternatives"</li>
        </ul>
      </div>
    </div>
  `;
  resourcesList.appendChild(queriesSection);

  // Add click handlers for example queries
  queriesSection.querySelectorAll('.clickable-query').forEach(item => {
    item.addEventListener('click', () => {
      const query = item.getAttribute('data-query');
      if (promptInput) {
        promptInput.value = query;
        promptInput.focus();
        // Scroll to chat panel
        document.querySelector('.chat-panel').scrollIntoView({ behavior: 'smooth', block: 'start' });
      }
    });
  });
}

function setLoadingStage(stage) {
  if (!loadingIndicator || !loadingStages) return;
  
  const stages = ['db', 'ai', 'response'];
  const currentIndex = stages.indexOf(stage);
  
  if (stage === 'done') {
    // Hide all stages
    loadingIndicator.hidden = true;
    stages.forEach(s => {
      if (loadingStages[s]) {
        loadingStages[s].classList.remove('active', 'complete');
      }
    });
    return;
  }
  
  loadingIndicator.hidden = false;
  
  stages.forEach((s, index) => {
    const stageElement = loadingStages[s];
    if (!stageElement) return;
    
    if (index < currentIndex) {
      stageElement.classList.remove('active');
      stageElement.classList.add('complete');
    } else if (index === currentIndex) {
      stageElement.classList.add('active');
      stageElement.classList.remove('complete');
    } else {
      stageElement.classList.remove('active', 'complete');
    }
  });
}

function updateStageStatus(stage, status) {
  if (!loadingStages || !loadingStages[stage]) return;
  
  const statusElement = loadingStages[stage].querySelector('.stage-status');
  if (statusElement) {
    statusElement.textContent = status;
  }
}

function setRunSummary(summaryText) {
  if (!runSummaryContainer || !runSummaryBody || !runSummaryButton) return;

  const hasSummary = !!summaryText && summaryText.trim().length > 0;
  if (!hasSummary) {
    runSummaryContainer.hidden = true;
    runSummaryBody.hidden = true;
    return;
  }

  runSummaryBody.textContent = summaryText.trim();
  runSummaryBody.hidden = true;
  runSummaryContainer.hidden = false;
  runSummaryButton.textContent = '▶ Run summary (collapsed)';
}

function toggleRunSummary() {
  if (!runSummaryBody || !runSummaryButton) return;
  const willShow = runSummaryBody.hidden;
  runSummaryBody.hidden = !willShow;
  runSummaryButton.textContent = `${willShow ? '▼' : '▶'} Run summary (${willShow ? 'expanded' : 'collapsed'})`;
}

function renderMcpVisual(modelId, isMcpCall) {
  if (!mcpCallVisual || !mcpCallAscii) return;

  if (!isMcpCall) {
    mcpCallVisual.hidden = true;
    return;
  }

  const resolvedModel = modelId && modelId.length > 0 ? modelId : 'unknown-model';
  const asciiArt = [
    'User prompt',
    '    |',
    '    v',
    '+---------------+',
    '|    MCP call    |',
    '+---------------+',
    '    |',
    ` model: ${resolvedModel}`,
    '    |',
    'AI answer'
  ].join('\n');

  mcpCallAscii.textContent = asciiArt;
  mcpCallVisual.hidden = false;
}

async function handleChatSubmit(event) {
  event.preventDefault();
  const prompt = promptInput.value.trim();
  if (prompt.length === 0) {
    return;
  }

  // Append the user's prompt to the transcript immediately, then a pending
  // assistant placeholder. This is what makes it feel ChatGPT-fast.
  const reportContext = typeof getChatReportContext === 'function' ? getChatReportContext() : null;
  const rektContext = typeof getChatRektContextOptions === 'function'
    ? (getChatRektContextOptions() || { enabled: false, runScope: 'latest' })
    : { enabled: false, runScope: 'latest' };
  const selectedScanRaw = typeof getSelectedScanRunId === 'function' ? getSelectedScanRunId() : null;
  const selectedScanRunId = selectedScanRaw && selectedScanRaw !== 'latest' && selectedScanRaw !== 'all'
    ? Number(selectedScanRaw)
    : null;
  const scope = reportContext ? 'report' : (rektContext.enabled ? 'database+rekt' : 'database');
  const scopeLabel = reportContext ? (String(reportContext).split('/').pop() || 'Report') : null;
  if (window.ChatHistory) {
    window.ChatHistory.appendMessage('user', prompt, { scope, scopeLabel });
    window.ChatHistory.appendMessage('assistant', '', { pending: true, scope, scopeLabel });
  }
  promptInput.value = '';
  // Hide the legacy single-response card; the transcript replaces it.
  responseCard.hidden = true;
  // Keep the multi-stage strip but skip the artificial 300ms delays — show
  // each stage flip as soon as the work moves on.
  setLoadingStage('db');
  updateStageStatus('db', 'Querying migration data…');
  toggleLoading(chatForm.querySelector('button'), true);
  setRunSummary('');
  renderMcpVisual(null, false);

  // Move to the AI stage on the next tick instead of after a 300ms timeout.
  queueMicrotask(() => {
    setLoadingStage('ai');
    updateStageStatus('ai', 'Streaming AI response…');
  });

  try {
    const recent = window.ChatHistory ? window.ChatHistory.getRecentMessages(10) : [];
    // Drop the empty pending assistant from the context we send to the server.
    const history = recent.filter(m => !(m.role === 'assistant' && !m.content));
    const res = await fetch('/api/chat', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'Accept': 'application/json'
      },
      body: JSON.stringify({
        prompt,
        history,
        reportContext: reportContext || null,
        useRektContext: !!rektContext.enabled,
        rektRunScope: rektContext.runScope || 'latest',
        selectedScanRunId: Number.isFinite(selectedScanRunId) ? selectedScanRunId : null
      })
    });

    setLoadingStage('response');
    updateStageStatus('response', 'Formatting…');

    if (!res.ok) {
      let message;
      const contentType = res.headers.get('content-type') || '';
      if (contentType.includes('application/problem+json') || contentType.includes('application/json')) {
        try {
          const errBody = await res.json();
          message = errBody.detail || errBody.title || errBody.message || JSON.stringify(errBody);
        } catch { message = await res.text(); }
      } else {
        message = await res.text();
      }
      throw new Error(message || `HTTP ${res.status}`);
    }

    const payload = await res.json();
    setRunSummary(payload.runSummary);
    renderMcpVisual(payload.model, payload.isMcpCall);

    // Update the pending assistant bubble with the real response.
    if (window.ChatHistory) {
      window.ChatHistory.updateLastAssistantMessage(payload.response ?? 'No content returned.', {
        pending: false,
        model: payload.model,
        isMcpCall: payload.isMcpCall,
        runId: payload.runId,
        runSummary: payload.runSummary,
        scope, scopeLabel,
      });
    } else {
      responseBody.textContent = payload.response ?? 'No content returned.';
      responseCard.hidden = false;
    }

    if (payload.runId) {
      if (window.dependencyGraph) {
        window.dependencyGraph.loadGraphForRun(payload.runId);
      } else {
        setTimeout(() => {
          if (window.dependencyGraph) window.dependencyGraph.loadGraphForRun(payload.runId);
        }, 500);
      }
    }
  } catch (err) {
    if (window.ChatHistory) {
      window.ChatHistory.updateLastAssistantMessage(`❌ ${err.message}`, { pending: false, error: true });
    } else {
      responseBody.textContent = `❌ Chat Error\n\n${err.message}`;
      responseCard.hidden = false;
      responseCard.classList.add('error-response');
    }
  } finally {
    setLoadingStage('done');
    toggleLoading(chatForm.querySelector('button'), false);
    responseCard.classList.remove('error-response');
  }
}

function toggleLoading(element, isLoading) {
  if (!element) return;
  element.disabled = isLoading;
  element.dataset.loading = isLoading ? 'true' : 'false';
  if (isLoading) {
    element.classList.add('loading');
  } else {
    element.classList.remove('loading');
  }
}

// Handle suggestion chip clicks
function initializeSuggestionChips() {
  document.querySelectorAll('.suggestion-chip').forEach(chip => {
    chip.addEventListener('click', () => {
      const prompt = chip.getAttribute('data-prompt');
      if (promptInput) {
        promptInput.value = prompt;
        promptInput.focus();
      }
      // Optional: auto-submit
      // chatForm.dispatchEvent(new Event('submit'));
    });
  });
}

// Initialize everything when DOM is ready
// Database status monitoring
async function updateDatabaseStatus() {
  try {
    const response = await fetch('/api/health/databases');
    if (!response.ok) throw new Error(`HTTP ${response.status}`);
    
    const data = await response.json();
    
    // Update SQLite status
    const sqliteIndicator = document.getElementById('sqlite-status');
    if (sqliteIndicator) {
      sqliteIndicator.className = 'status-indicator ' + (data.sqlite.connected ? 'connected' : 'disconnected');
      sqliteIndicator.title = `SQLite: ${data.sqlite.status}`;
    }
    
    // Update Neo4j status
    const neo4jIndicator = document.getElementById('neo4j-status');
    if (neo4jIndicator) {
      neo4jIndicator.className = 'status-indicator ' + (data.neo4j.connected ? 'connected' : 'disconnected');
      neo4jIndicator.title = `Neo4j: ${data.neo4j.status}`;
    }

    // Update AI Model status (Chat)
    const aiModelIndicator = document.getElementById('ai-model-status');
    if (aiModelIndicator) {
      aiModelIndicator.className = 'status-indicator ' + (data.aiModel && data.aiModel.connected ? 'connected' : 'disconnected');
      const modelId = data.aiModel ? data.aiModel.modelId : 'Unknown';
      aiModelIndicator.title = `Chat Model: ${modelId}`;
      const label = aiModelIndicator.querySelector('.status-label');
      if (label) {
          label.textContent = data.aiModel && data.aiModel.connected ? modelId : 'Chat Model';
      }
    }

    // Update Codex Model status
    const codexModelIndicator = document.getElementById('codex-model-status');
    if (codexModelIndicator) {
      codexModelIndicator.className = 'status-indicator ' + (data.aiModel && data.aiModel.connected ? 'connected' : 'disconnected');
      const codexId = data.aiModel ? data.aiModel.codexModelId : 'Unknown';
      codexModelIndicator.title = `Code Model: ${codexId}`;
      const label = codexModelIndicator.querySelector('.status-label');
      if (label) {
          label.textContent = data.aiModel && data.aiModel.connected ? codexId : 'Code Model';
      }
    }
  } catch (err) {
    console.error('Failed to update database status:', err);
    // Set all to disconnected on error
    const sqliteIndicator = document.getElementById('sqlite-status');
    const neo4jIndicator = document.getElementById('neo4j-status');
    const aiModelIndicator = document.getElementById('ai-model-status');
    const codexModelIndicator = document.getElementById('codex-model-status');
    
    if (sqliteIndicator) sqliteIndicator.className = 'status-indicator disconnected';
    if (neo4jIndicator) neo4jIndicator.className = 'status-indicator disconnected';
    if (aiModelIndicator) aiModelIndicator.className = 'status-indicator disconnected';
    if (codexModelIndicator) codexModelIndicator.className = 'status-indicator disconnected';
  }
}

function initializeApp() {
  initializeElements();
  
  if (!chatForm) {
    console.error('Required elements not found in DOM');
    return;
  }
  
  // Set up event listeners
  chatForm.addEventListener('submit', handleChatSubmit);
  
  if (refreshButton) {
    refreshButton.addEventListener('click', () => {
      fetchResources();
    });
  }
  
  initializeSuggestionChips();
  fetchResources();
  
  // Start database status monitoring
  updateDatabaseStatus();
  setInterval(() => {
    if (window.pageIsVisible !== false) updateDatabaseStatus();
  }, 10000); // Update every 10 seconds
}

// Ensure DOM is ready before initializing
if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', initializeApp);
} else {
  initializeApp();
}
