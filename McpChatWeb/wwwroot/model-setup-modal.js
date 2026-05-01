// ═══════════════════════════════════════════════════════════════════════════════
// Model Setup Modal — Authenticate & discover models from Azure OpenAI or
// GitHub Copilot SDK, then save the configuration from the portal.
// ═══════════════════════════════════════════════════════════════════════════════

let _setupModels = [];
let _setupServiceType = 'AzureOpenAI';

document.addEventListener('DOMContentLoaded', () => {
  initModelSetupModal();
});

function initModelSetupModal() {
  // Tab switching
  document.querySelectorAll('.setup-tab').forEach(tab => {
    tab.addEventListener('click', () => switchSetupTab(tab.dataset.provider));
  });

  // Connect buttons
  const azureConnectBtn = document.getElementById('azure-connect-btn');
  if (azureConnectBtn) azureConnectBtn.addEventListener('click', connectAzure);

  const copilotConnectBtn = document.getElementById('copilot-connect-btn');
  if (copilotConnectBtn) copilotConnectBtn.addEventListener('click', connectCopilot);

  // Save button
  const saveBtn = document.getElementById('setup-save-btn');
  if (saveBtn) saveBtn.addEventListener('click', saveSetupConfig);

  // Close / backdrop
  const closeBtn = document.getElementById('setup-modal-close');
  if (closeBtn) closeBtn.addEventListener('click', closeSetupModal);

  const modal = document.getElementById('modelSetupModal');
  if (modal) modal.addEventListener('click', e => { if (e.target === modal) closeSetupModal(); });

  // Auth method radio toggle
  document.querySelectorAll('input[name="azure-auth"]').forEach(radio => {
    radio.addEventListener('change', () => {
      const keyGroup = document.getElementById('azure-apikey-group');
      if (keyGroup) keyGroup.style.display = radio.value === 'apikey' ? 'block' : 'none';
    });
  });

  document.querySelectorAll('input[name="copilot-auth"]').forEach(radio => {
    radio.addEventListener('change', () => {
      const patGroup = document.getElementById('copilot-pat-group');
      if (patGroup) patGroup.style.display = radio.value === 'pat' ? 'block' : 'none';
    });
  });

  // Re-configure button in config panel
  const reconfigBtn = document.getElementById('open-setup-modal-btn');
  if (reconfigBtn) reconfigBtn.addEventListener('click', openSetupModal);
}

// ── Auto-check on page load ──────────────────────────────────────────────────

async function checkNeedsSetup() {
  try {
    const res = await fetch('/api/models/available');
    if (!res.ok) return;
    const data = await res.json();
    if (data.needsSetup) {
      openSetupModal();
    }
  } catch (err) {
    console.error('Setup check failed:', err);
  }
}

// Call after a small delay to let the page render first
setTimeout(checkNeedsSetup, 800);

// ── Modal open/close ─────────────────────────────────────────────────────────

function openSetupModal() {
  const modal = document.getElementById('modelSetupModal');
  if (modal) {
    modal.style.display = 'block';
    // Reset state
    setSetupStatus('');
    setSetupModelList([]);
    document.getElementById('setup-save-btn').disabled = true;
  }
}

function closeSetupModal() {
  const modal = document.getElementById('modelSetupModal');
  if (modal) modal.style.display = 'none';
}

// ── Tab switching ────────────────────────────────────────────────────────────

function switchSetupTab(provider) {
  document.querySelectorAll('.setup-tab').forEach(t => t.classList.remove('setup-tab-active'));
  document.querySelectorAll('.setup-tab-content').forEach(c => c.style.display = 'none');

  const tab = document.querySelector(`.setup-tab[data-provider="${provider}"]`);
  if (tab) tab.classList.add('setup-tab-active');

  const content = document.getElementById(`setup-${provider}`);
  if (content) content.style.display = 'block';

  _setupServiceType = provider;
}

// ── Connect: Azure OpenAI ────────────────────────────────────────────────────

async function connectAzure() {
  const btn = document.getElementById('azure-connect-btn');
  const endpoint = document.getElementById('azure-endpoint').value.trim();
  const authMethod = document.querySelector('input[name="azure-auth"]:checked')?.value || 'apikey';
  const apiKey = document.getElementById('azure-apikey').value.trim();

  if (!endpoint) {
    setSetupStatus('Please enter your Azure OpenAI endpoint URL.', true);
    return;
  }

  // Validate URL format
  try {
    const url = new URL(endpoint);
    if (url.protocol !== 'https:') {
      setSetupStatus('Endpoint must use HTTPS.', true);
      return;
    }
  } catch {
    setSetupStatus('Invalid URL format. Example: https://your-resource.openai.azure.com', true);
    return;
  }

  if (authMethod === 'apikey' && !apiKey) {
    setSetupStatus('Please enter your API key.', true);
    return;
  }

  btn.disabled = true;
  btn.textContent = '🔄 Connecting...';
  setSetupStatus('Connecting to Azure OpenAI...');

  try {
    const res = await fetch('/api/models/connect', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        serviceType: 'AzureOpenAI',
        endpoint,
        apiKey: authMethod === 'apikey' ? apiKey : null,
        useDefaultCredential: authMethod === 'azlogin'
      })
    });

    const data = await res.json();

    if (data.error) {
      setSetupStatus(data.error, true);
      return;
    }

    if (data.authenticated && data.models) {
      _setupModels = data.models;
      _setupServiceType = 'AzureOpenAI';
      setSetupStatus(`✅ Connected! Found ${data.modelCount} deployment(s).`);
      setSetupModelList(data.models);
      document.getElementById('setup-save-btn').disabled = false;
    }
  } catch (err) {
    setSetupStatus(`Connection failed: ${err.message}`, true);
  } finally {
    btn.disabled = false;
    btn.textContent = '🔌 Connect';
  }
}

// ── Connect: GitHub Copilot SDK ──────────────────────────────────────────────

async function connectCopilot() {
  const btn = document.getElementById('copilot-connect-btn');
  const authMethod = document.querySelector('input[name="copilot-auth"]:checked')?.value || 'cli';
  const pat = document.getElementById('copilot-pat').value.trim();

  if (authMethod === 'pat' && !pat) {
    setSetupStatus('Please enter your GitHub Personal Access Token.', true);
    return;
  }

  btn.disabled = true;
  btn.textContent = '🔄 Connecting...';
  setSetupStatus('Connecting to GitHub Copilot SDK... (this may take a few seconds)');

  try {
    const res = await fetch('/api/models/connect', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        serviceType: 'GitHubCopilotSDK',
        apiKey: authMethod === 'pat' ? pat : null,
        useDefaultCredential: authMethod === 'cli'
      })
    });

    const data = await res.json();

    if (data.error) {
      setSetupStatus(data.error, true);
      return;
    }

    if (data.authenticated && data.models) {
      _setupModels = data.models;
      _setupServiceType = 'GitHubCopilotSDK';
      setSetupStatus(`✅ Connected! Found ${data.modelCount} model(s).`);
      setSetupModelList(data.models);
      document.getElementById('setup-save-btn').disabled = false;
    }
  } catch (err) {
    setSetupStatus(`Connection failed: ${err.message}`, true);
  } finally {
    btn.disabled = false;
    btn.textContent = '🔌 Connect';
  }
}

// ── Render discovered models ─────────────────────────────────────────────────

function setSetupModelList(models) {
  const container = document.getElementById('setup-model-list');
  if (!container) return;

  if (!models || models.length === 0) {
    container.innerHTML = '<div class="setup-no-models">Connect to a provider to discover models.</div>';
    return;
  }

  // Group by publisher
  const groups = {};
  for (const m of models) {
    const pub = m.publisher || 'Other';
    if (!groups[pub]) groups[pub] = [];
    groups[pub].push(m);
  }

  // HTML-escape to prevent XSS from API-supplied model names
  const esc = s => String(s ?? '').replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;').replace(/"/g,'&quot;');

  let html = '<div class="setup-model-roles">';
  html += '<div class="setup-role-row">';
  html += '<label class="setup-role-label">💬 Chat Model (portal Q&A, reasoning):</label>';
  html += '<select id="setup-chat-model" class="setup-model-select">';
  for (const m of models) {
    html += `<option value="${esc(m.id)}">${esc(m.name)}</option>`;
  }
  html += '</select></div>';

  html += '<div class="setup-role-row">';
  html += '<label class="setup-role-label">⚙️ Code Model (migration agents):</label>';
  html += '<select id="setup-code-model" class="setup-model-select">';
  for (const m of models) {
    html += `<option value="${esc(m.id)}">${esc(m.name)}</option>`;
  }
  html += '</select></div>';
  html += '</div>';

  html += '<div class="setup-model-grid">';
  for (const [publisher, pubModels] of Object.entries(groups)) {
    html += `<div class="setup-publisher-group">`;
    html += `<div class="setup-publisher-name">${esc(publisher)} (${pubModels.length})</div>`;
    for (const m of pubModels) {
      html += `<div class="setup-model-item" data-model-id="${esc(m.id)}">`;
      html += `<span class="setup-model-name">${esc(m.name)}</span>`;
      html += `<span class="setup-model-family">${esc(m.family || '')}</span>`;
      if (m.description) html += `<span class="setup-model-desc">${esc(m.description)}</span>`;
      html += `</div>`;
    }
    html += `</div>`;
  }
  html += '</div>';

  container.innerHTML = html;

  // Auto-select best defaults
  autoSelectDefaults(models);
}

function autoSelectDefaults(models) {
  const chatSelect = document.getElementById('setup-chat-model');
  const codeSelect = document.getElementById('setup-code-model');
  if (!chatSelect || !codeSelect) return;

  // For chat: prefer reasoning/chat models
  const chatPreference = ['gpt-5.2-chat', 'gpt-5', 'gpt-4o', 'claude-sonnet-4', 'claude-opus-4'];
  for (const pref of chatPreference) {
    const match = models.find(m => m.id.toLowerCase().includes(pref.toLowerCase()));
    if (match) { chatSelect.value = match.id; break; }
  }

  // For code: prefer codex/code models
  const codePreference = ['codex-mini', 'codex', 'gpt-5.1', 'gpt-4o', 'claude-sonnet-4'];
  for (const pref of codePreference) {
    const match = models.find(m => m.id.toLowerCase().includes(pref.toLowerCase()));
    if (match) { codeSelect.value = match.id; break; }
  }
}

// ── Save configuration ──────────────────────────────────────────────────────

async function saveSetupConfig() {
  const btn = document.getElementById('setup-save-btn');
  const chatModel = document.getElementById('setup-chat-model')?.value;
  const codeModel = document.getElementById('setup-code-model')?.value;

  if (!chatModel && !codeModel) {
    setSetupStatus('Please select at least one model.', true);
    return;
  }

  btn.disabled = true;
  btn.textContent = '💾 Saving...';

  try {
    const endpoint = document.getElementById('azure-endpoint')?.value?.trim() || null;
    const authMethod = document.querySelector('input[name="azure-auth"]:checked')?.value;
    const apiKey = _setupServiceType === 'AzureOpenAI'
      ? (authMethod === 'apikey' ? document.getElementById('azure-apikey')?.value?.trim() : null)
      : (document.querySelector('input[name="copilot-auth"]:checked')?.value === 'pat'
          ? document.getElementById('copilot-pat')?.value?.trim() : null);

    const res = await fetch('/api/models/save-config', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        serviceType: _setupServiceType,
        endpoint: _setupServiceType === 'AzureOpenAI' ? endpoint : null,
        apiKey,
        useDefaultCredential: _setupServiceType === 'AzureOpenAI' && authMethod === 'azlogin',
        chatModelId: chatModel,
        codeModelId: codeModel
      })
    });

    const data = await res.json();

    if (data.success) {
      setSetupStatus('✅ Configuration saved! Reloading models...');
      btn.textContent = '✅ Saved';

      // Refresh the model dropdowns across the portal
      setTimeout(async () => {
        closeSetupModal();
        // Reload the model panel and mission control
        if (typeof loadModels === 'function') await loadModels();
        if (typeof fetchModelCatalog === 'function') await fetchModelCatalog();
        // Update the header badges
        if (typeof updateActiveModelBadge === 'function') updateActiveModelBadge(data.activeModelId);
      }, 1000);
    } else {
      setSetupStatus('Failed to save configuration.', true);
    }
  } catch (err) {
    setSetupStatus(`Save failed: ${err.message}`, true);
  } finally {
    btn.disabled = false;
    btn.textContent = '💾 Save & Apply';
  }
}

// ── Helpers ──────────────────────────────────────────────────────────────────

function setSetupStatus(message, isError = false) {
  const el = document.getElementById('setup-status');
  if (!el) return;
  el.textContent = message;
  el.className = 'setup-status' + (isError ? ' setup-status-error' : '');
  el.style.display = message ? 'block' : 'none';
}
