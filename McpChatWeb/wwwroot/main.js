let resourcesList, refreshButton, chatForm, promptInput, responseCard, responseBody, loadingIndicator, loadingStages;

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
}

async function fetchResources() {
  toggleLoading(refreshButton, true);
  try {
    const res = await fetch('/api/resources', { headers: { 'Accept': 'application/json' } });
    if (!res.ok) {
      throw new Error(`HTTP ${res.status}`);
    }
    const resources = await res.json();
    renderResources(resources);
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
    errorItem.textContent = errorMessage;
    errorItem.classList.add('error');
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

async function handleChatSubmit(event) {
  event.preventDefault();
  const prompt = promptInput.value.trim();
  if (prompt.length === 0) {
    return;
  }

  // Hide previous response and show stage 1: Database Query
  responseCard.hidden = true;
  setLoadingStage('db');
  updateStageStatus('db', 'Fetching migration data...');
  toggleLoading(chatForm.querySelector('button'), true);
  
  try {
    // Move to stage 2: Azure OpenAI
    setTimeout(() => {
      setLoadingStage('ai');
      updateStageStatus('ai', 'Processing with AI...');
    }, 300);
    
    const res = await fetch('/api/chat', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'Accept': 'application/json'
      },
      body: JSON.stringify({ prompt })
    });

    // Move to stage 3: Building Response
    setLoadingStage('response');
    updateStageStatus('response', 'Formatting results...');

    if (!res.ok) {
      const message = await res.text();
      throw new Error(message || `HTTP ${res.status}`);
    }

    const payload = await res.json();
    
    // Small delay to show final stage
    await new Promise(resolve => setTimeout(resolve, 300));
    
    responseBody.textContent = payload.response ?? 'No content returned.';
    responseCard.hidden = false;
    
    // If the response includes a runId, update the graph to show that run
    console.log('Chat response received:', { hasRunId: !!payload.runId, runId: payload.runId, hasGraph: !!window.dependencyGraph });
    if (payload.runId) {
      if (window.dependencyGraph) {
        console.log(`✅ Updating graph to Run ${payload.runId}...`);
        window.dependencyGraph.loadGraphForRun(payload.runId);
      } else {
        console.warn('⚠️ Graph not ready yet, retrying in 500ms...');
        setTimeout(() => {
          if (window.dependencyGraph) {
            console.log(`✅ Delayed update: Loading graph for Run ${payload.runId}`);
            window.dependencyGraph.loadGraphForRun(payload.runId);
          } else {
            console.error('❌ Graph still not available after delay');
          }
        }, 500);
      }
    }
  } catch (err) {
    responseBody.textContent = `Error: ${err.message}`;
    responseCard.hidden = false;
  } finally {
    setLoadingStage('done');
    toggleLoading(chatForm.querySelector('button'), false);
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
  } catch (err) {
    console.error('Failed to update database status:', err);
    // Set both to disconnected on error
    const sqliteIndicator = document.getElementById('sqlite-status');
    const neo4jIndicator = document.getElementById('neo4j-status');
    if (sqliteIndicator) sqliteIndicator.className = 'status-indicator disconnected';
    if (neo4jIndicator) neo4jIndicator.className = 'status-indicator disconnected';
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
  setInterval(updateDatabaseStatus, 10000); // Update every 10 seconds
}

// Ensure DOM is ready before initializing
if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', initializeApp);
} else {
  initializeApp();
}