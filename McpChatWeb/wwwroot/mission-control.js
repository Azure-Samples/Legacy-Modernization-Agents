// ═══════════════════════════════════════════════════════════════════════════════
// Mission Control — Run management, file upload, folder browser, report viewer
// ═══════════════════════════════════════════════════════════════════════════════

let _managedRuns = [];
let _activeRunId = null;
let _runPollInterval = null;
let _reportsList = [];
let _chatWithReport = null; // path of report being used as chat context

// HTML-escape to prevent XSS from API-supplied strings
const esc = s => String(s ?? '').replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;').replace(/"/g,'&quot;').replace(/'/g,'&#39;');

document.addEventListener('DOMContentLoaded', () => {
  initMissionControl();
});

// ── Model catalog — loaded from configured provider (./doctor.sh setup) ───────
let _modelCatalog = [];

async function fetchModelCatalog() {
  try {
    const res = await fetch('/api/models/available');
    if (!res.ok) return;
    const data = await res.json();
    _modelCatalog = (data.models || []).map(m => ({
      id: m.id, label: `🧠 ${m.id}`
    }));

    // Set provider dropdown to match configured provider
    const providerSelect = document.getElementById('mc-provider-select');
    if (providerSelect && data.serviceType) {
      const val = (data.serviceType === 'GitHubCopilot' || data.serviceType === 'GitHubCopilotSDK')
        ? 'GitHubCopilot' : 'AzureOpenAI';
      providerSelect.value = val;
    }

    populateModelDropdown();
  } catch (err) {
    console.error('Failed to fetch model catalog:', err);
  }
}

function populateModelDropdown() {
  const modelSelect = document.getElementById('mc-model-select');
  if (!modelSelect) return;
  const models = _modelCatalog.length > 0 ? _modelCatalog : [{ id: '', label: '⚠️ Run ./doctor.sh setup first' }];
  modelSelect.innerHTML = models.map(m =>
    `<option value="${esc(m.id)}">${esc(m.label)}</option>`
  ).join('');
}

function initMissionControl() {
  // Fetch models from configured provider (set by ./doctor.sh setup)
  fetchModelCatalog();

  // Provider change triggers re-connect via the setup modal
  const providerSelect = document.getElementById('mc-provider-select');
  if (providerSelect) {
    providerSelect.addEventListener('change', () => {
      // Open the setup modal so user can connect to the new provider
      if (typeof openSetupModal === 'function') openSetupModal();
    });
  }

  // Provider info toggle
  const infoBtn = document.getElementById('mc-provider-info-btn');
  if (infoBtn) {
    infoBtn.addEventListener('click', () => {
      const panel = document.getElementById('mc-provider-info');
      if (panel) panel.style.display = panel.style.display === 'none' ? 'block' : 'none';
    });
  }

  // Start command buttons
  document.querySelectorAll('[data-run-command]').forEach(btn => {
    btn.addEventListener('click', () => startRunCommand(btn.dataset.runCommand));
  });

  // Stop/Pause buttons
  const stopBtn = document.getElementById('mc-stop-btn');
  if (stopBtn) stopBtn.addEventListener('click', stopActiveRun);

  const pauseBtn = document.getElementById('mc-pause-btn');
  if (pauseBtn) pauseBtn.addEventListener('click', pauseActiveRun);

  // File upload
  const uploadArea = document.getElementById('mc-upload-area');
  const fileInput = document.getElementById('mc-file-input');
  if (uploadArea && fileInput) {
    uploadArea.addEventListener('click', () => fileInput.click());
    uploadArea.addEventListener('dragover', e => { e.preventDefault(); uploadArea.classList.add('drag-over'); });
    uploadArea.addEventListener('dragleave', () => uploadArea.classList.remove('drag-over'));
    uploadArea.addEventListener('drop', e => { e.preventDefault(); uploadArea.classList.remove('drag-over'); handleFileUpload(e.dataTransfer.files); });
    fileInput.addEventListener('change', () => handleFileUpload(fileInput.files));
  }

  // Report toggle
  const reportToggle = document.getElementById('mc-report-toggle');
  if (reportToggle) reportToggle.addEventListener('change', toggleReportChat);

  const reportSelect = document.getElementById('mc-report-select');
  if (reportSelect) reportSelect.addEventListener('change', selectReport);

  // Run selector
  const runSelect = document.getElementById('mc-run-select');
  if (runSelect) runSelect.addEventListener('change', () => {
    _activeRunId = runSelect.value;
    refreshRunLog();
  });

  // Folder tabs
  document.querySelectorAll('[data-folder-tab]').forEach(btn => {
    btn.addEventListener('click', () => browseFolderTab(btn.dataset.folderTab));
  });

  // Expand log button
  const expandBtn = document.getElementById('mc-expand-log-btn');
  if (expandBtn) expandBtn.addEventListener('click', toggleLogExpand);

  // Initial loads
  loadManagedRuns();
  loadReports();
  browseFolderTab('source');

  // Poll for run updates every 3 seconds
  _runPollInterval = setInterval(pollRunUpdates, 3000);
}

// ── Run Commands ─────────────────────────────────────────────────────────────

async function startRunCommand(command) {
  const providerSelect = document.getElementById('mc-provider-select');
  const modelSelect = document.getElementById('mc-model-select');
  const langSelect = document.getElementById('mc-language-select');
  const speedSelect = document.getElementById('mc-speed-select');
  const nameInput = document.getElementById('mc-run-name');

  const provider = providerSelect?.value || 'AzureOpenAI';
  const modelId = modelSelect?.value || '';
  const targetLanguage = langSelect?.value || 'Java';
  const speedProfile = speedSelect?.value || 'balanced';
  const name = nameInput?.value || '';

  // Warn about Copilot SDK sequential processing
  if (provider === 'CopilotSDK') {
    const logEl = document.getElementById('mc-run-log');
    if (logEl) logEl.textContent = '⚠️ Copilot SDK: sequential processing (1 request at a time). This is slower but stable.';
  }

  // Disable all start buttons
  document.querySelectorAll('[data-run-command]').forEach(b => b.disabled = true);

  try {
    const res = await fetch('/api/runs/start', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ command, name, targetLanguage, speedProfile, provider, modelId })
    });
    if (!res.ok) throw new Error(`HTTP ${res.status}`);
    const run = await res.json();
    _activeRunId = run.runId;

    // Immediate feedback — clear the "Waiting" placeholder
    const logEl = document.getElementById('mc-run-log');
    if (logEl) logEl.textContent = `⏳ Starting ${command}...`;

    updateRunControls(run);
    await loadManagedRuns();
    refreshRunLog();          // fetch first log lines right away
    if (nameInput) nameInput.value = '';
  } catch (err) {
    console.error('Failed to start run:', err);
    alert(`Failed to start: ${err.message}`);
  } finally {
    document.querySelectorAll('[data-run-command]').forEach(b => b.disabled = false);
  }
}

async function stopActiveRun() {
  if (!_activeRunId) return;
  try {
    await fetch('/api/runs/stop', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ runId: _activeRunId })
    });
    await loadManagedRuns();
  } catch (err) { console.error('Stop failed:', err); }
}

async function pauseActiveRun() {
  if (!_activeRunId) return;
  try {
    await fetch(`/api/runs/pause/${_activeRunId}`, { method: 'POST' });
    await loadManagedRuns();
  } catch (err) { console.error('Pause failed:', err); }
}

// ── Run Polling ──────────────────────────────────────────────────────────────

async function pollRunUpdates() {
  if (!_activeRunId) return;
  try {
    const res = await fetch(`/api/runs/managed/${_activeRunId}`);
    if (!res.ok) return;
    const data = await res.json();
    updateRunControls(data.info);
    updateRunLog(data.log);
  } catch { /* silent */ }
}

async function loadManagedRuns() {
  try {
    const res = await fetch('/api/runs/managed');
    if (!res.ok) return;
    _managedRuns = await res.json();
    renderRunSelector();
    if (_managedRuns.length > 0 && !_activeRunId) {
      _activeRunId = _managedRuns[0].runId;
    }
    const active = _managedRuns.find(r => r.runId === _activeRunId);
    if (active) updateRunControls(active);
  } catch (err) { console.error('Failed to load runs:', err); }
}

function renderRunSelector() {
  const select = document.getElementById('mc-run-select');
  if (!select) return;
  select.innerHTML = '<option value="">No runs yet</option>';
  for (const run of _managedRuns) {
    const opt = document.createElement('option');
    opt.value = run.runId;
    const icon = statusIcon(run.status);
    opt.textContent = `${icon} ${run.name} (${run.command}) — ${run.status}`;
    if (run.runId === _activeRunId) opt.selected = true;
    select.appendChild(opt);
  }
}

function updateRunControls(run) {
  const statusEl = document.getElementById('mc-run-status');
  const stopBtn = document.getElementById('mc-stop-btn');
  const pauseBtn = document.getElementById('mc-pause-btn');

  if (statusEl) {
    statusEl.textContent = `${statusIcon(run.status)} ${run.status.toUpperCase()}`;
    statusEl.className = `mc-status mc-status-${run.status}`;
  }
  if (stopBtn) stopBtn.disabled = !['running', 'paused'].includes(run.status);
  if (pauseBtn) {
    pauseBtn.disabled = !['running', 'paused'].includes(run.status);
    pauseBtn.textContent = run.status === 'paused' ? '▶️ Resume' : '⏸️ Pause';
  }
}

function updateRunLog(lines) {
  const logEl = document.getElementById('mc-run-log');
  if (!logEl || !lines) return;
  const wasAtBottom = logEl.scrollHeight - logEl.scrollTop - logEl.clientHeight < 50;
  logEl.textContent = lines.join('\n');
  if (wasAtBottom) logEl.scrollTop = logEl.scrollHeight;
}

async function refreshRunLog() {
  if (!_activeRunId) return;
  try {
    const res = await fetch(`/api/runs/managed/${_activeRunId}/log?lines=200`);
    if (!res.ok) return;
    const data = await res.json();
    updateRunLog(data.lines);
  } catch { /* silent */ }
}

function statusIcon(status) {
  return { running: '🟢', paused: '🟡', completed: '✅', failed: '❌', stopped: '🛑', pending: '⏳' }[status] || '⚪';
}

function toggleLogExpand() {
  const logEl = document.getElementById('mc-run-log');
  if (!logEl) return;
  const overlay = document.getElementById('mc-log-overlay');

  if (logEl.classList.toggle('expanded')) {
    // Show overlay backdrop
    if (!overlay) {
      const o = document.createElement('div');
      o.id = 'mc-log-overlay';
      o.className = 'mc-log-overlay';
      o.addEventListener('click', toggleLogExpand);
      document.body.appendChild(o);
    } else {
      overlay.style.display = 'block';
    }
  } else {
    if (overlay) overlay.style.display = 'none';
  }
}

// ── File Upload ──────────────────────────────────────────────────────────────

async function handleFileUpload(fileList) {
  if (!fileList || fileList.length === 0) return;

  const statusEl = document.getElementById('mc-upload-status');
  if (statusEl) statusEl.textContent = `Uploading ${fileList.length} file(s)...`;

  const formData = new FormData();
  for (const f of fileList) formData.append('files', f);

  try {
    const res = await fetch('/api/source/upload', { method: 'POST', body: formData });
    if (!res.ok) throw new Error(`HTTP ${res.status}`);
    const data = await res.json();

    const ok = data.uploaded.filter(f => f.status === 'uploaded').length;
    const rejected = data.uploaded.filter(f => f.status === 'rejected').length;
    if (statusEl) statusEl.textContent = `✅ ${ok} uploaded${rejected ? `, ${rejected} rejected` : ''}`;

    // Refresh folder view and source summary
    browseFolderTab('source');
    if (typeof loadSourceFiles === 'function') loadSourceFiles();

    setTimeout(() => { if (statusEl) statusEl.textContent = ''; }, 4000);
  } catch (err) {
    if (statusEl) statusEl.textContent = `❌ Upload failed: ${err.message}`;
  }
}

// ── Folder Browser ───────────────────────────────────────────────────────────

async function browseFolderTab(folder) {
  // Update active tab
  document.querySelectorAll('[data-folder-tab]').forEach(b => b.classList.remove('mc-tab-active'));
  document.querySelector(`[data-folder-tab="${folder}"]`)?.classList.add('mc-tab-active');

  const listEl = document.getElementById('mc-folder-list');
  if (!listEl) return;
  listEl.innerHTML = '<div class="mc-loading">Loading...</div>';

  try {
    const res = await fetch(`/api/folders/browse?folder=${encodeURIComponent(folder)}`);
    if (!res.ok) throw new Error(`HTTP ${res.status}`);
    const data = await res.json();
    renderFolderContents(listEl, data, folder);
  } catch (err) {
    listEl.innerHTML = `<div class="mc-error">Failed: ${esc(err.message)}</div>`;
  }
}

function renderFolderContents(container, data, folder) {
  if (data.items.length === 0) {
    container.innerHTML = `<div class="mc-empty">📂 Empty — ${folder === 'source' ? 'Upload COBOL files above' : 'Run a migration to generate output'}</div>`;
    return;
  }

  const formatSize = (b) => b > 1048576 ? `${(b / 1048576).toFixed(1)}MB` : b > 1024 ? `${(b / 1024).toFixed(1)}KB` : `${b}B`;

  container.innerHTML = `
    <div class="mc-folder-header">${data.totalFiles} files · ${formatSize(data.totalSizeBytes)}</div>
    ${data.items.map(item => `
      <div class="mc-folder-item ${item.type}">
        <span class="mc-fi-icon">${item.type === 'directory' ? '📁' : fileIcon(item.name)}</span>
        <span class="mc-fi-name" ${item.type === 'file' ? `title="${esc(item.relativePath)}"` : ''}>${esc(item.name)}</span>
        <span class="mc-fi-meta">${item.lineCount ? `${item.lineCount} lines` : item.type === 'directory' ? '' : formatSize(item.sizeBytes)}</span>
        ${folder === 'source' && item.type === 'file' ? `<button class="mc-fi-delete" onclick="deleteSourceFile('${esc(item.name)}')" title="Remove">✕</button>` : ''}
      </div>
    `).join('')}
  `;
}

function fileIcon(name) {
  const ext = name.split('.').pop()?.toLowerCase();
  if (['cbl', 'cob'].includes(ext)) return '📄';
  if (['cpy', 'copy'].includes(ext)) return '📋';
  if (['java'].includes(ext)) return '☕';
  if (['cs'].includes(ext)) return '🟦';
  if (['md'].includes(ext)) return '📝';
  return '📎';
}

async function deleteSourceFile(fileName) {
  if (!confirm(`Delete ${fileName} from source folder?`)) return;
  try {
    await fetch(`/api/source/files/${encodeURIComponent(fileName)}`, { method: 'DELETE' });
    browseFolderTab('source');
    if (typeof loadSourceFiles === 'function') loadSourceFiles();
  } catch (err) { console.error('Delete failed:', err); }
}

// ── Reports & Chat Context ──────────────────────────────────────────────────

async function loadReports() {
  try {
    const res = await fetch('/api/reports/available');
    if (!res.ok) return;
    const data = await res.json();
    _reportsList = data.reports || [];
    renderReportDropdown();
  } catch { /* silent */ }
}

function renderReportDropdown() {
  const select = document.getElementById('mc-report-select');
  if (!select) return;
  select.innerHTML = '<option value="">No reports</option>';
  for (const r of _reportsList) {
    const opt = document.createElement('option');
    opt.value = r.path;
    opt.textContent = `${r.name} (${new Date(r.lastModified).toLocaleDateString()})`;
    select.appendChild(opt);
  }
  if (_reportsList.length > 0) select.value = _reportsList[0].path;
}

function toggleReportChat() {
  const toggle = document.getElementById('mc-report-toggle');
  const select = document.getElementById('mc-report-select');
  const indicator = document.getElementById('mc-report-indicator');

  if (toggle?.checked && select?.value) {
    _chatWithReport = select.value;
    if (indicator) { indicator.textContent = '📊 Chatting with report'; indicator.style.display = 'inline'; }
  } else {
    _chatWithReport = null;
    if (indicator) indicator.style.display = 'none';
  }
}

function selectReport() {
  const toggle = document.getElementById('mc-report-toggle');
  if (toggle?.checked) toggleReportChat();
}

// Export for chat integration
window.getChatReportContext = function() { return _chatWithReport; };
