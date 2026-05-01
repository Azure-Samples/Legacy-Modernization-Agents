// ═══════════════════════════════════════════════════════════════════════════════
// Model & Prompt Configuration Panel
// ═══════════════════════════════════════════════════════════════════════════════

let _modelsCache = null;
let _promptsCache = null;
let _sourceFilesCache = null;
let _editingPromptId = null;

// ── Lifecycle ────────────────────────────────────────────────────────────────

document.addEventListener('DOMContentLoaded', () => {
  initConfigPanel();
});

function initConfigPanel() {
  // Load data
  loadModels();
  loadSourceFiles();
  loadPrompts();

  // Wire events
  const applyBtn = document.getElementById('apply-model-btn');
  if (applyBtn) applyBtn.addEventListener('click', applyModelSelection);

  const modelSelect = document.getElementById('model-selector');
  if (modelSelect) modelSelect.addEventListener('change', onModelSelectChange);

  // Prompt editor modal
  const closeBtn = document.querySelector('.prompt-editor-close');
  if (closeBtn) closeBtn.addEventListener('click', closePromptEditor);

  const saveBtn = document.getElementById('prompt-editor-save');
  if (saveBtn) saveBtn.addEventListener('click', savePromptEdits);

  const cancelBtn = document.getElementById('prompt-editor-cancel');
  if (cancelBtn) cancelBtn.addEventListener('click', closePromptEditor);

  const autoGenBtn = document.getElementById('prompt-auto-generate-btn');
  if (autoGenBtn) autoGenBtn.addEventListener('click', autoGeneratePrompt);

  // Close prompt modal on backdrop click
  const modal = document.getElementById('promptEditorModal');
  if (modal) {
    modal.addEventListener('click', (e) => {
      if (e.target === modal) closePromptEditor();
    });
  }
}

// ── Toggle Panel ─────────────────────────────────────────────────────────────

function toggleConfigPanel() {
  const body = document.getElementById('config-body');
  const icon = document.getElementById('config-toggle-icon');
  if (!body || !icon) return;

  const isHidden = body.style.display === 'none';
  body.style.display = isHidden ? 'block' : 'none';
  icon.textContent = isHidden ? '▼' : '▶';
}

// ── Models ───────────────────────────────────────────────────────────────────

async function loadModels() {
  // Fetch configured models from /api/models/available (set by ./doctor.sh setup)
  try {
    const res = await fetch('/api/models/available');
    if (!res.ok) throw new Error(`HTTP ${res.status}`);
    const data = await res.json();

    const select = document.getElementById('model-selector');
    if (select) {
      select.innerHTML = '';

      if (data.models && data.models.length > 0) {
        const provider = data.serviceType === 'GitHubCopilot' ? '🤖 GitHub Copilot SDK' : '☁️ Azure OpenAI';
        const group = document.createElement('optgroup');
        group.label = provider;

        for (const m of data.models) {
          const opt = document.createElement('option');
          opt.value = m.id;
          const ctxLabel = m.contextWindow ? ` · ${formatContextWindow(m.contextWindow)} ctx` : '';
          opt.textContent = m.contextWindow ? `🧠 ${m.id}${ctxLabel}` : `🧠 ${m.id}`;
          if (m.id === data.activeModelId) opt.selected = true;
          group.appendChild(opt);
        }
        select.appendChild(group);
      } else {
        const opt = document.createElement('option');
        opt.value = '';
        opt.textContent = '⚠️ No models configured — click 🔧 Setup';
        select.appendChild(opt);

        // Auto-open the setup modal if no models are configured
        if (data.needsSetup && typeof openSetupModal === 'function') {
          setTimeout(() => openSetupModal(), 500);
        }
      }
    }

    // Also set the Mission Control provider dropdown to match
    const mcProvider = document.getElementById('mc-provider-select');
    if (mcProvider) {
      const providerValue = (data.serviceType === 'GitHubCopilot' || data.serviceType === 'GitHubCopilotSDK')
        ? 'GitHubCopilot' : 'AzureOpenAI';
      mcProvider.value = providerValue;
    }

    updateActiveModelBadge(data.activeModelId);
  } catch (err) {
    console.error('Failed to load models:', err);
  }
}

function renderModelDropdown(data) {
  const select = document.getElementById('model-selector');
  if (!select) return;

  select.innerHTML = '';

  if (!data.models || data.models.length === 0) {
    const opt = document.createElement('option');
    opt.value = '';
    opt.textContent = data.hasGitHubAuth
      ? 'No models returned from Copilot API'
      : '⚠️ No GitHub auth — run: gh auth login';
    select.appendChild(opt);
    return;
  }

  // Group models by publisher
  const groups = {};
  for (const m of data.models) {
    const publisher = m.publisher || 'Other';
    if (!groups[publisher]) groups[publisher] = [];
    groups[publisher].push(m);
  }

  for (const [publisher, models] of Object.entries(groups)) {
    const optgroup = document.createElement('optgroup');
    optgroup.label = `${publisher} (${models.length})`;
    for (const m of models) {
      const opt = document.createElement('option');
      opt.value = m.id;
      const ctxLabel = m.contextWindow ? ` · ${formatContextWindow(m.contextWindow)} ctx` : '';
      opt.textContent = `${m.name}${ctxLabel}`;
      opt.dataset.publisher = m.publisher;
      opt.dataset.family = m.family;
      opt.dataset.context = m.contextWindow || '';
      opt.dataset.description = m.description || '';
      if (m.id === data.activeModelId) opt.selected = true;
      optgroup.appendChild(opt);
    }
    select.appendChild(optgroup);
  }

  // Show connection status
  const infoRow = document.getElementById('model-info-row');
  if (infoRow) {
    infoRow.style.display = 'flex';
    const publisherEl = document.getElementById('model-publisher');
    if (publisherEl) {
      const status = data.copilotConnected
        ? `🟢 Copilot API · ${data.models.length} models`
        : data.hasGitHubAuth
          ? '🟡 GitHub auth OK · using fallback catalog'
          : '🔴 No GitHub auth · run: gh auth login';
      publisherEl.textContent = status;
    }
  }

  onModelSelectChange();
}

function formatContextWindow(tokens) {
  if (tokens >= 1_000_000) return `${(tokens / 1_000_000).toFixed(0)}M`;
  if (tokens >= 1_000) return `${(tokens / 1_000).toFixed(0)}K`;
  return `${tokens}`;
}

function onModelSelectChange() {
  const select = document.getElementById('model-selector');
  const infoRow = document.getElementById('model-info-row');
  const publisherEl = document.getElementById('model-publisher');
  const contextEl = document.getElementById('model-context');
  if (!select || !infoRow) return;

  const opt = select.selectedOptions[0];
  if (!opt || !opt.value) {
    infoRow.style.display = 'none';
    return;
  }

  infoRow.style.display = 'flex';
  if (publisherEl) publisherEl.textContent = `${opt.dataset.family || ''} · ${opt.dataset.publisher || ''}`;
  if (contextEl && opt.dataset.context) {
    contextEl.textContent = `Context: ${formatContextWindow(parseInt(opt.dataset.context))} tokens`;
  }
}

async function applyModelSelection() {
  const select = document.getElementById('model-selector');
  const btn = document.getElementById('apply-model-btn');
  if (!select || !select.value) return;

  btn.disabled = true;
  btn.textContent = '...';

  try {
    const res = await fetch('/api/models/active', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ modelId: select.value })
    });

    if (!res.ok) throw new Error(`HTTP ${res.status}`);

    const data = await res.json();
    updateActiveModelBadge(data.activeModelId);

    btn.textContent = '✓';
    btn.style.background = '#10b981';
    setTimeout(() => {
      btn.textContent = '✓';
      btn.style.background = '';
      btn.disabled = false;
    }, 1500);
  } catch (err) {
    console.error('Failed to set model:', err);
    btn.textContent = '✗';
    btn.style.background = '#ef4444';
    setTimeout(() => {
      btn.textContent = '✓';
      btn.style.background = '';
      btn.disabled = false;
    }, 2000);
  }
}

function updateActiveModelBadge(modelId) {
  const badge = document.getElementById('config-active-model-badge');
  if (badge) {
    badge.textContent = modelId || 'none';
    badge.title = `Active model: ${modelId}`;
  }
}

// ── Source Files ──────────────────────────────────────────────────────────────

async function loadSourceFiles() {
  try {
    const res = await fetch('/api/source/files');
    if (!res.ok) throw new Error(`HTTP ${res.status}`);
    const data = await res.json();
    _sourceFilesCache = data;
    renderSourceSummary(data);
  } catch (err) {
    console.error('Failed to scan source files:', err);
    const container = document.getElementById('source-files-summary');
    if (container) container.innerHTML = '<span class="source-error">Failed to scan</span>';
  }
}

function renderSourceSummary(data) {
  const container = document.getElementById('source-files-summary');
  const warningEl = document.getElementById('source-files-warning');
  if (!container) return;

  if (data.isEmpty || data.files.length === 0) {
    container.innerHTML = '<span class="source-empty">No files detected</span>';
    if (warningEl) {
      warningEl.style.display = 'block';
      warningEl.textContent = data.warning || 'No COBOL files found. Place .cbl/.cpy files in the source/ folder.';
    }
    return;
  }

  if (warningEl) warningEl.style.display = 'none';

  const s = data.summary;
  container.innerHTML = `
    <div class="source-stats">
      <span class="source-stat" title="${s.programs} COBOL programs">📄 ${s.programs} programs</span>
      <span class="source-stat" title="${s.copybooks} copybooks">📋 ${s.copybooks} copybooks</span>
      ${s.other > 0 ? `<span class="source-stat">📎 ${s.other} other</span>` : ''}
      <span class="source-stat source-total">Σ ${s.total} files</span>
    </div>
    <details class="source-details">
      <summary>View files & detected features</summary>
      <div class="source-file-list" id="source-file-list-inner">
        ${data.files.map(f => `
          <div class="source-file-item">
            <span class="source-file-icon">${f.fileType === 'Program' ? '📄' : f.fileType === 'Copybook' ? '📋' : '📎'}</span>
            <span class="source-file-name">${f.fileName}</span>
            <span class="source-file-meta">${f.lineCount} lines</span>
          </div>
        `).join('')}
      </div>
      <div id="source-analysis-loading" style="font-size:11px;color:#64748b;padding:4px 0;">Loading analysis...</div>
    </details>
  `;

  // Fetch deep analysis asynchronously
  loadSourceAnalysis();
}

async function loadSourceAnalysis() {
  try {
    const res = await fetch('/api/source/analyze');
    if (!res.ok) return;
    const data = await res.json();

    const loadingEl = document.getElementById('source-analysis-loading');
    if (loadingEl) loadingEl.remove();

    const listEl = document.getElementById('source-file-list-inner');
    if (!listEl || !data.files) return;

    // Re-render with analysis data
    listEl.innerHTML = data.files.map(f => `
      <div class="source-file-item">
        <span class="source-file-icon">${f.fileType === 'Program' ? '📄' : f.fileType === 'Copybook' ? '📋' : '📎'}</span>
        <span class="source-file-name">${escapeHtml(f.fileName)}</span>
        <span class="source-file-meta">${f.lineCount} lines · ${escapeHtml(f.complexity)}</span>
        ${f.features && f.features.length > 0
          ? `<div class="source-file-features">${f.features.map(ft => `<span class="analysis-feature-tag">${escapeHtml(ft.replace(/_/g, ' '))}</span>`).join('')}</div>`
          : ''}
      </div>
    `).join('');

    // Add architecture summary
    if (data.detectedFeatures && data.detectedFeatures.length > 0) {
      const summaryDiv = document.createElement('div');
      summaryDiv.className = 'source-analysis-summary';
      summaryDiv.innerHTML =
        `<span style="font-size:11px;color:#60a5fa;">Architecture: ${escapeHtml(data.architecturePattern)}</span> · ` +
        `<span style="font-size:11px;color:#64748b;">${data.totalLines?.toLocaleString()} total lines</span>`;
      listEl.parentElement.appendChild(summaryDiv);
    }
  } catch (err) {
    console.error('Source analysis failed:', err);
    const loadingEl = document.getElementById('source-analysis-loading');
    if (loadingEl) loadingEl.textContent = '';
  }
}

// ── Prompts ──────────────────────────────────────────────────────────────────

async function loadPrompts() {
  try {
    const res = await fetch('/api/prompts');
    if (!res.ok) throw new Error(`HTTP ${res.status}`);
    const data = await res.json();
    _promptsCache = data;
    renderPromptList(data);
  } catch (err) {
    console.error('Failed to load prompts:', err);
    const container = document.getElementById('prompt-list');
    if (container) container.innerHTML = '<span class="source-error">Failed to load prompts</span>';
  }
}

function renderPromptList(prompts) {
  const container = document.getElementById('prompt-list');
  if (!container) return;

  if (!prompts || prompts.length === 0) {
    container.innerHTML = '<span class="source-empty">No prompt templates found</span>';
    return;
  }

  container.innerHTML = prompts.map(p => {
    const score = p.qualityScore || 0;
    const scoreClass = score >= 8 ? 'high' : score >= 5 ? 'medium' : score > 0 ? 'low' : 'none';
    const scoreBadge = score > 0
      ? `<span class="quality-score ${scoreClass}" title="${escapeHtml(p.observations || '')}">${score}/10</span>`
      : '';
    return `
    <div class="prompt-item ${p.enabled ? '' : 'prompt-disabled'}" data-prompt-id="${escapeHtml(p.id)}">
      <div class="prompt-item-header">
        <label class="prompt-toggle" title="${p.enabled ? 'Enabled — click to disable' : 'Disabled — click to enable'}">
          <input type="checkbox" ${p.enabled ? 'checked' : ''} onchange="togglePrompt('${escapeHtml(p.id)}', this.checked)">
          <span class="prompt-toggle-slider"></span>
        </label>
        <span class="prompt-name">${escapeHtml(p.name)}</span>
        ${scoreBadge}
        <div class="prompt-actions">
          <button class="btn-prompt-action" onclick="openPromptEditor('${escapeHtml(p.id)}')" title="Edit current prompt">✏️</button>
          <button class="btn-prompt-action btn-prompt-rescore" onclick="rescorePrompt('${escapeHtml(p.id)}', this)" title="Re-evaluate quality score with AI">🔍 Score</button>
          <button class="btn-prompt-action btn-prompt-generate" onclick="autoGenerateForPrompt('${escapeHtml(p.id)}')" title="Analyze source files and generate optimized prompt">⚡ Generate</button>
        </div>
      </div>
      ${p.observations ? `<div class="prompt-obs">${escapeHtml(p.observations)}</div>` : ''}
      <div class="prompt-preview">${truncate(p.systemPrompt, 80)}</div>
    </div>`;
  }).join('');
}

function truncate(text, maxLen) {
  if (!text) return '<em>empty</em>';
  const clean = text.replace(/\n/g, ' ').trim();
  return clean.length > maxLen ? clean.substring(0, maxLen) + '...' : clean;
}

function escapeHtml(str) {
  const div = document.createElement('div');
  div.textContent = str;
  return div.innerHTML;
}

async function togglePrompt(promptId, enabled) {
  try {
    await fetch('/api/prompts/update', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ id: promptId, enabled })
    });

    // Update cache
    if (_promptsCache) {
      const p = _promptsCache.find(x => x.id === promptId);
      if (p) p.enabled = enabled;
    }

    // Update visual
    const item = document.querySelector(`.prompt-item[data-prompt-id="${promptId}"]`);
    if (item) {
      item.classList.toggle('prompt-disabled', !enabled);
    }
  } catch (err) {
    console.error('Failed to toggle prompt:', err);
  }
}

async function rescorePrompt(promptId, btn) {
  const origText = btn.textContent;
  btn.disabled = true;
  btn.textContent = '⏳ Scoring...';

  try {
    const res = await fetch(`/api/prompts/score/${encodeURIComponent(promptId)}`, { method: 'POST' });
    if (!res.ok) throw new Error(`HTTP ${res.status}`);
    const data = await res.json();

    // If AI model wasn't available, show warning without overwriting persisted scores
    if (!data.scored) {
      const item = document.querySelector(`.prompt-item[data-prompt-id="${promptId}"]`);
      if (item) {
        let warn = item.querySelector('.prompt-suggestions');
        if (!warn) { warn = document.createElement('div'); warn.className = 'prompt-suggestions'; item.appendChild(warn); }
        warn.textContent = '⚠️ ' + (data.observations || 'Scoring unavailable — select an AI model first');
      }
      btn.textContent = origText;
      btn.disabled = false;
      return;
    }

    // Update cache with real scores
    if (_promptsCache) {
      const p = _promptsCache.find(x => x.id === promptId);
      if (p) {
        p.qualityScore = data.qualityScore;
        p.observations = data.observations;
      }
    }

    // Re-render the list to show updated score (destroys current btn reference)
    renderPromptList(_promptsCache);

    // Show suggestions on the newly rendered item
    if (data.suggestions) {
      const item = document.querySelector(`.prompt-item[data-prompt-id="${promptId}"]`);
      if (item) {
        const existing = item.querySelector('.prompt-suggestions');
        if (existing) existing.remove();
        const sugEl = document.createElement('div');
        sugEl.className = 'prompt-suggestions';
        sugEl.textContent = '💡 Suggestions: ' + data.suggestions;
        item.appendChild(sugEl);
      }
    }

    // Re-acquire button from new DOM (old btn is detached after re-render)
    const newBtn = document.querySelector(`.prompt-item[data-prompt-id="${promptId}"] .btn-prompt-rescore`);
    if (newBtn) { newBtn.textContent = '✅ Scored'; setTimeout(() => { newBtn.textContent = origText; }, 2000); }
  } catch (err) {
    console.error('Failed to score prompt:', err);
    btn.textContent = '❌ Failed';
    setTimeout(() => { btn.textContent = origText; btn.disabled = false; }, 2000);
  }
}

function openPromptEditor(promptId) {
  const prompt = _promptsCache?.find(p => p.id === promptId);
  if (!prompt) return;

  _editingPromptId = promptId;

  document.getElementById('prompt-editor-title').textContent = prompt.name;
  document.getElementById('prompt-editor-system').value = prompt.systemPrompt || '';
  document.getElementById('prompt-editor-user').value = prompt.userPromptTemplate || '';

  // Hide analysis banner when opening fresh
  const banner = document.getElementById('prompt-analysis-banner');
  if (banner) banner.style.display = 'none';

  // Clear save status
  const status = document.getElementById('prompt-save-status');
  if (status) { status.textContent = ''; status.className = 'prompt-save-status'; }

  const modal = document.getElementById('promptEditorModal');
  if (modal) modal.style.display = 'block';
}

function closePromptEditor() {
  const modal = document.getElementById('promptEditorModal');
  if (modal) modal.style.display = 'none';
  _editingPromptId = null;
}

async function savePromptEdits() {
  if (!_editingPromptId) return;

  const systemPrompt = document.getElementById('prompt-editor-system').value;
  const userPrompt = document.getElementById('prompt-editor-user').value;
  const saveBtn = document.getElementById('prompt-editor-save');
  const statusEl = document.getElementById('prompt-save-status');

  if (saveBtn) { saveBtn.disabled = true; saveBtn.textContent = '💾 Saving...'; }
  if (statusEl) { statusEl.textContent = ''; statusEl.className = 'prompt-save-status'; }

  try {
    const res = await fetch('/api/prompts/update', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        id: _editingPromptId,
        systemPrompt,
        userPromptTemplate: userPrompt
      })
    });

    if (!res.ok) throw new Error(`HTTP ${res.status}`);

    // Update cache
    if (_promptsCache) {
      const p = _promptsCache.find(x => x.id === _editingPromptId);
      if (p) {
        p.systemPrompt = systemPrompt;
        p.userPromptTemplate = userPrompt;
      }
    }

    // Show success feedback
    if (statusEl) {
      statusEl.textContent = '✓ Saved to Agents/Prompts/' + _editingPromptId + '.md';
      statusEl.className = 'prompt-save-status success';
    }
    if (saveBtn) { saveBtn.textContent = '💾 Saved!'; saveBtn.style.background = '#10b981'; }
    setTimeout(() => {
      if (saveBtn) { saveBtn.textContent = '💾 Save to File'; saveBtn.style.background = ''; saveBtn.disabled = false; }
    }, 2000);

    // Refresh list
    renderPromptList(_promptsCache);
  } catch (err) {
    console.error('Failed to save prompt:', err);
    if (statusEl) {
      statusEl.textContent = '✗ Save failed';
      statusEl.className = 'prompt-save-status error';
    }
    if (saveBtn) { saveBtn.textContent = '💾 Save to File'; saveBtn.style.background = ''; saveBtn.disabled = false; }
  }
}

async function autoGeneratePrompt() {
  if (!_editingPromptId) return;
  await doAutoGenerate(_editingPromptId);
}

async function autoGeneratePrompt() {
  if (!_editingPromptId) return;
  await doAutoGenerate(_editingPromptId);
}

// ═══════════════════════════════════════════════════════════════════════════════
// Prompt Studio — Unified prompt generation dashboard
// ═══════════════════════════════════════════════════════════════════════════════

let _studioMode = null; // 'quick' or 'ai'

async function openPromptStudio() {
  const modal = document.getElementById('enhanceDashboardModal');
  if (modal) modal.style.display = 'block';

  // Show mode picker, hide progress
  document.getElementById('studio-mode-picker').style.display = 'block';
  document.getElementById('studio-progress').style.display = 'none';

  // Fetch active AI model to display in the AI card
  try {
    const res = await fetch('/api/models/active');
    if (res.ok) {
      const data = await res.json();
      const modelName = data.activeModelId || '';
      const modelEl = document.getElementById('studio-ai-model-name');
      const providerEl = document.getElementById('studio-ai-provider');
      const btnAi = document.getElementById('btn-studio-ai');
      const infoEl = document.getElementById('studio-ai-model-info');

      if (modelName && modelName !== 'unknown') {
        const serviceType = (await fetch('/api/models/active').then(r => r.json()).catch(() => ({}))).serviceType || '';
        const provider = serviceType.includes('github') ? 'GitHub Copilot Models' :
                         serviceType.includes('azure') ? 'Azure OpenAI' : serviceType || 'AI Provider';
        if (modelEl) modelEl.textContent = modelName;
        if (providerEl) providerEl.textContent = `via ${provider}`;
        if (infoEl) { infoEl.className = 'studio-ai-model-info connected'; }
        if (btnAi) { btnAi.disabled = false; btnAi.title = ''; }
      } else {
        if (modelEl) modelEl.textContent = 'No model selected';
        if (providerEl) providerEl.textContent = '— select a model above first';
        if (infoEl) { infoEl.className = 'studio-ai-model-info disconnected'; }
        if (btnAi) { btnAi.disabled = true; btnAi.title = 'Select an AI model in the dropdown above first'; }
      }
    }
  } catch { /* ignore */ }
}

function closeEnhanceDashboard() {
  const modal = document.getElementById('enhanceDashboardModal');
  if (modal) modal.style.display = 'none';
}

function setPhaseState(phaseNum, state) {
  const phase = document.getElementById(`enhance-phase-${phaseNum}`);
  if (!phase) return;
  const indicator = phase.querySelector('.phase-indicator');
  if (!indicator) return;
  indicator.className = `phase-indicator ${state}`;
  if (state === 'active') {
    indicator.innerHTML = '<span class="phase-spinner"></span>';
  } else if (state === 'done') {
    indicator.innerHTML = '<span class="phase-check">✓</span>';
  } else if (state === 'error') {
    indicator.innerHTML = '<span class="phase-error">✗</span>';
  } else {
    indicator.innerHTML = `<span class="phase-number">${phaseNum}</span>`;
  }
}

function resetProgress(mode) {
  _studioMode = mode;
  const isAi = mode === 'ai';

  // Show/hide phase 3 (AI Enhancement) based on mode
  const phase3 = document.getElementById('enhance-phase-3');
  if (phase3) phase3.style.display = isAi ? 'flex' : 'none';

  // Update phase 4 number label based on mode
  const phase4Num = document.querySelector('#enhance-phase-4 .phase-number');
  if (phase4Num) phase4Num.textContent = isAi ? '4' : '3';

  // Reset all phases
  for (let i = 1; i <= 4; i++) {
    setPhaseState(i, 'pending');
    const timing = document.getElementById(`phase-${i}-timing`);
    if (timing) timing.textContent = '';
  }

  document.getElementById('phase-1-detail').textContent = 'Scanning COBOL files with regex patterns...';
  document.getElementById('phase-2-detail').textContent = 'Building prompts from detected features...';
  document.getElementById('phase-3-detail').textContent = 'Sending code samples to AI for domain-specific insights...';
  document.getElementById('phase-4-detail').textContent = 'Writing prompts to Agents/Prompts/*.md files...';

  // Hide result sections
  ['enhance-analysis-summary', 'enhance-ai-badge', 'enhance-quality-info', 'enhance-agent-cards', 'enhance-sampled-files', 'enhance-final-summary'].forEach(id => {
    const el = document.getElementById(id);
    if (el) el.style.display = 'none';
  });

  // Show mode indicator
  const modeEl = document.getElementById('studio-progress-mode');
  if (modeEl) {
    modeEl.innerHTML = isAi
      ? '<span class="progress-mode-badge ai">🧠 AI-Enhanced Mode</span> Regex analysis + AI code review'
      : '<span class="progress-mode-badge quick">⚡ Quick Mode</span> Regex pattern analysis only';
  }
}

async function runPromptStudio(mode) {
  // Guard: verify model is set for AI mode before starting
  if (mode === 'ai') {
    try {
      const chk = await fetch('/api/models/active');
      if (chk.ok) {
        const d = await chk.json();
        if (!d.activeModelId || d.activeModelId === 'unknown') {
          alert('No AI model selected. Please select a model in the dropdown above before using AI-Enhanced mode.');
          return;
        }
      }
    } catch (_) { /* proceed anyway, server will handle it */ }
  }

  // Switch from picker to progress view
  document.getElementById('studio-mode-picker').style.display = 'none';
  document.getElementById('studio-progress').style.display = 'block';

  resetProgress(mode);
  setPhaseState(1, 'active');
  const startTime = Date.now();

  const endpoint = mode === 'ai' ? '/api/prompts/enhance-all' : '/api/prompts/generate-all';

  try {
    const res = await fetch(endpoint, { method: 'POST' });
    if (!res.ok) throw new Error(`HTTP ${res.status}`);
    const data = await res.json();

    if (!data.success) {
      setPhaseState(1, 'error');
      document.getElementById('phase-1-detail').textContent = data.warning || 'Failed';
      return;
    }

    const timings = data.phaseTimings || {};
    const totalTime = data.totalTimeMs || (Date.now() - startTime);

    // ── Phase 1: Source Analysis ──────────────────────────────────────────
    setPhaseState(1, 'done');
    document.getElementById('phase-1-detail').textContent =
      `Scanned ${data.analysis.totalFiles} files — ${data.analysis.detectedFeatures.length} features detected`;
    document.getElementById('phase-1-timing').textContent = `${timings.phase1_regex_ms || '—'}ms`;

    // Show analysis summary
    const summaryEl = document.getElementById('enhance-analysis-summary');
    if (summaryEl) {
      document.getElementById('enhance-stat-files').innerHTML = `<strong>${data.analysis.totalFiles}</strong> files`;
      document.getElementById('enhance-stat-programs').innerHTML = `<strong>${data.analysis.programs}</strong> programs`;
      document.getElementById('enhance-stat-copybooks').innerHTML = `<strong>${data.analysis.copybooks}</strong> copybooks`;
      document.getElementById('enhance-stat-lines').innerHTML = `<strong>${(data.analysis.totalLines || 0).toLocaleString()}</strong> lines`;
      document.getElementById('enhance-stat-arch').innerHTML = `<strong>${data.analysis.architecturePattern}</strong>`;

      const featureTags = document.getElementById('enhance-feature-tags');
      if (featureTags && data.analysis.detectedFeatures) {
        const important = ['EXEC_SQL', 'EXEC_CICS', 'CICS_SCREEN', 'FILE_IO', 'BATCH_LOOP', 'CALL_PROGRAM'];
        featureTags.innerHTML = data.analysis.detectedFeatures.map(f => {
          const isKey = important.includes(f);
          return `<span class="enhance-feature-tag ${isKey ? 'key' : ''}">${f.replace(/_/g, ' ')}</span>`;
        }).join('');
      }
      summaryEl.style.display = 'block';
    }

    // ── Phase 2: Base Prompt Generation ──────────────────────────────────
    setPhaseState(2, 'done');
    document.getElementById('phase-2-detail').textContent =
      `Generated base prompts for ${data.totalAgents} agents`;
    document.getElementById('phase-2-timing').textContent = `${timings.phase2_prompts_ms || '—'}ms`;

    // ── Phase 3: AI Enhancement (only in AI mode) ────────────────────────
    if (mode === 'ai') {
      if (data.aiEnhanced) {
        setPhaseState(3, 'done');
        document.getElementById('phase-3-detail').textContent =
          `AI enhanced prompts using ${data.aiModelUsed}`;
      } else {
        setPhaseState(3, data.aiModelUsed === '(none)' ? 'error' : 'done');
        document.getElementById('phase-3-detail').textContent =
          data.aiModelUsed === '(none)' ? 'No AI model configured' : 'AI returned no enhancements';
      }
      document.getElementById('phase-3-timing').textContent = `${timings.phase3_ai_ms || '—'}ms`;

      // Show AI badge
      const aiBadge = document.getElementById('enhance-ai-badge');
      if (aiBadge) {
        document.getElementById('enhance-ai-model-name').textContent = data.aiModelUsed || '(none)';
        const aiStatus = document.getElementById('enhance-ai-status');
        if (data.aiEnhanced) {
          aiStatus.textContent = 'enhanced';
          aiStatus.className = 'ai-badge-status success';
        } else if (data.aiModelUsed === '(none)') {
          aiStatus.textContent = 'skipped';
          aiStatus.className = 'ai-badge-status skipped';
        } else {
          aiStatus.textContent = 'no changes';
          aiStatus.className = 'ai-badge-status neutral';
        }
        aiBadge.style.display = 'flex';
      }
    }

    // ── Phase 4 (or 3 in quick mode): Save ───────────────────────────────
    setPhaseState(4, 'done');
    document.getElementById('phase-4-detail').textContent =
      `Saved ${data.savedCount}/${data.totalAgents} prompt files to disk`;
    document.getElementById('phase-4-timing').textContent = `${timings.phase4_save_ms || timings.phase3_save_ms || '—'}ms`;

    // Show agent quality cards
    const cardsEl = document.getElementById('enhance-agent-cards');
    if (cardsEl && data.results) {
      const isAi = mode === 'ai';
      // Show info box in AI mode
      const infoBox = document.getElementById('enhance-quality-info');
      if (infoBox && isAi) infoBox.style.display = 'block';

      // Compute average score
      const scores = data.results.filter(r => (r.qualityScore || 0) > 0).map(r => r.qualityScore);
      const avgScore = scores.length > 0 ? (scores.reduce((a, b) => a + b, 0) / scores.length).toFixed(1) : '—';
      const avgClass = avgScore >= 8 ? 'high' : avgScore >= 5 ? 'medium' : avgScore > 0 ? 'low' : 'none';

      cardsEl.innerHTML = `<div class="enhance-cards-title">${isAi ? `Agent Enhancement Quality <span class="quality-score avg ${avgClass}"  title="Average across all agents">avg ${avgScore}/10</span>` : 'Generated Prompts'}</div>` +
        data.results.map(r => {
          const score = r.qualityScore || 0;
          const scoreClass = score >= 8 ? 'high' : score >= 5 ? 'medium' : score > 0 ? 'low' : 'none';
          const badge = (isAi && r.enhanced)
            ? '<span class="enhanced-badge">🧠 AI-enhanced</span>'
            : '<span class="enhanced-badge regex-only">⚡ Regex-generated</span>';
          return `<div class="enhance-agent-card">
            <div class="agent-card-header">
              <span class="agent-card-name">${r.promptId}</span>
              ${badge}
              ${(isAi && score > 0) ? `<span class="quality-score ${scoreClass}">${score}/10</span>` : ''}
            </div>
            ${r.observations ? `<div class="agent-card-obs">${r.observations}</div>` : ''}
            <div class="agent-card-status">${r.savedToDisk ? '💾 Saved to disk' : '⚠️ Save failed'}</div>
          </div>`;
        }).join('');
      cardsEl.style.display = 'block';
    }

    // Show sampled files (AI mode only)
    if (mode === 'ai') {
      const sampledEl = document.getElementById('enhance-sampled-files');
      if (sampledEl && data.analysis.sampledFiles && data.analysis.sampledFiles.length > 0) {
        sampledEl.innerHTML = '<div class="sampled-title">📁 Sampled for AI Analysis</div>' +
          data.analysis.sampledFiles.map(f => `<span class="sampled-file">${f}</span>`).join('');
        sampledEl.style.display = 'flex';
      }
    }

    // Final summary
    const finalEl = document.getElementById('enhance-final-summary');
    if (finalEl) {
      const enhancedCount = (data.results || []).filter(r => r.enhanced).length;
      const modeLabel = mode === 'ai'
        ? (data.aiEnhanced ? `<strong>${enhancedCount}</strong> AI-enhanced` : 'regex-only (AI had no changes)')
        : 'regex-generated';
      finalEl.innerHTML =
        `<div class="final-summary-line">` +
        `✅ <strong>${data.savedCount}/${data.totalAgents}</strong> prompts saved — ${modeLabel}` +
        ` — total: <strong>${totalTime.toLocaleString()}ms</strong></div>`;
      finalEl.style.display = 'block';
    }

    // Reload prompts in background
    await loadPrompts();

  } catch (err) {
    console.error('Prompt Studio failed:', err);
    for (let i = 1; i <= 4; i++) {
      const phase = document.getElementById(`enhance-phase-${i}`);
      if (phase && phase.querySelector('.phase-indicator.active')) {
        setPhaseState(i, 'error');
        const detail = document.getElementById(`phase-${i}-detail`);
        if (detail) detail.textContent = `Error: ${err.message}`;
        break;
      }
    }
  }
}

async function autoGenerateForPrompt(promptId) {
  // Open editor first so the modal is visible, then generate into it
  openPromptEditor(promptId);
  await doAutoGenerate(promptId);
}

async function doAutoGenerate(promptId) {
  const autoBtn = document.getElementById('prompt-auto-generate-btn');
  const systemEl = document.getElementById('prompt-editor-system');
  const userEl = document.getElementById('prompt-editor-user');
  if (autoBtn) { autoBtn.disabled = true; autoBtn.textContent = '⚡ Analyzing source files...'; }

  try {
    const res = await fetch('/api/prompts/generate', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ promptId })
    });

    if (!res.ok) throw new Error(`HTTP ${res.status}`);
    const data = await res.json();

    if (data.warning) {
      alert(`⚠️ ${data.warning}`);
      return;
    }

    // Show analysis banner (modal is already open at this point)
    if (data.analysis) {
      showAnalysisBanner(data.analysis);
    }

    // Set both system and user prompt textareas
    if (data.generatedPrompt && systemEl) {
      systemEl.value = data.generatedPrompt;
    }
    if (data.generatedUserPrompt && userEl) {
      userEl.value = data.generatedUserPrompt;
    }
  } catch (err) {
    console.error('Failed to auto-generate prompt:', err);
    alert('Failed to generate prompt from source files.');
  } finally {
    if (autoBtn) { autoBtn.disabled = false; autoBtn.textContent = '⚡ Auto-Generate'; }
  }
}

function showAnalysisBanner(analysis) {
  const banner = document.getElementById('prompt-analysis-banner');
  if (!banner) return;

  // Architecture badge
  const archBadge = document.getElementById('analysis-arch-badge');
  if (archBadge) archBadge.textContent = analysis.architecturePattern || 'general';

  // Feature tags
  const featuresEl = document.getElementById('analysis-features');
  if (featuresEl && analysis.detectedFeatures) {
    const importantFeatures = ['EXEC_SQL', 'EXEC_CICS', 'CICS_SCREEN', 'FILE_IO', 'BATCH_LOOP', 'CALL_PROGRAM'];
    featuresEl.innerHTML = analysis.detectedFeatures.map(f => {
      const isImportant = importantFeatures.includes(f);
      return `<span class="analysis-feature-tag ${isImportant ? 'highlight' : ''}">${f.replace(/_/g, ' ')}</span>`;
    }).join('');
  }

  // Stats
  const statsEl = document.getElementById('analysis-stats');
  if (statsEl) {
    statsEl.innerHTML =
      `<span>📄 ${analysis.programs || 0} programs</span>` +
      `<span>📋 ${analysis.copybooks || 0} copybooks</span>` +
      `<span>Σ ${(analysis.totalLines || 0).toLocaleString()} lines</span>` +
      `<span>${analysis.totalFiles || 0} files total</span>`;
  }

  banner.style.display = 'block';
}
