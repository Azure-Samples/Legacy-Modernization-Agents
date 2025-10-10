const resourcesList = document.getElementById('resources');
const refreshButton = document.getElementById('refresh');
const chatForm = document.getElementById('chat-form');
const promptInput = document.getElementById('prompt');
const responseCard = document.getElementById('response');
const responseBody = document.getElementById('response-body');

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

  for (const resource of resources) {
    const item = document.createElement('li');
    const title = document.createElement('strong');
    title.textContent = resource.name ?? resource.uri;

    const uri = document.createElement('code');
    uri.textContent = resource.uri;

    const description = document.createElement('p');
    description.textContent = resource.description ?? '';

    item.appendChild(title);
    item.appendChild(uri);
    if (description.textContent.trim().length > 0) {
      item.appendChild(description);
    }
    resourcesList.appendChild(item);
  }
}

const loadingIndicator = document.getElementById('loading-indicator');
const loadingStages = {
  db: document.getElementById('stage-db'),
  ai: document.getElementById('stage-ai'),
  response: document.getElementById('stage-response')
};

function setLoadingStage(stage) {
  const stages = ['db', 'ai', 'response'];
  const currentIndex = stages.indexOf(stage);
  
  if (stage === 'done') {
    // Hide all stages
    loadingIndicator.hidden = true;
    stages.forEach(s => {
      loadingStages[s].classList.remove('active', 'complete');
    });
    return;
  }
  
  loadingIndicator.hidden = false;
  
  stages.forEach((s, index) => {
    const stageElement = loadingStages[s];
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
  const statusElement = loadingStages[stage].querySelector('.stage-status');
  if (statusElement) {
    statusElement.textContent = status;
  }
}

chatForm.addEventListener('submit', async (event) => {
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
      updateStageStatus('ai', 'Processing with gpt-5-mini...');
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
});

refreshButton.addEventListener('click', () => {
  fetchResources();
});

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
document.querySelectorAll('.suggestion-chip').forEach(chip => {
  chip.addEventListener('click', () => {
    const prompt = chip.getAttribute('data-prompt');
    promptInput.value = prompt;
    promptInput.focus();
    // Optional: auto-submit
    // chatForm.dispatchEvent(new Event('submit'));
  });
});

fetchResources();