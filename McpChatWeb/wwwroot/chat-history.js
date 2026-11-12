// Saved chats: every question and answer is kept in the browser (localStorage), listed in the
// sidebar and resumable. Ported from the preview branch; limits come from PortalChat in
// Config/appsettings.json via /api/chat/settings.

(function () {
  'use strict';

  const STORAGE_KEY = 'mcp.chatHistory.v1';
  const CURRENT_KEY = 'mcp.chatHistory.currentId';

  const settings = { maxHistoryMessages: 8, maxSavedConversations: 100 };

  const state = {
    conversations: [], // [{id, title, createdAt, updatedAt, messages: [{role, content, ts, scope, scopeLabel, model, runId, pending, error, kind}]}]
    currentId: null,
    searchQuery: '',
  };

  function load() {
    try {
      const raw = localStorage.getItem(STORAGE_KEY);
      const parsed = raw ? JSON.parse(raw) : [];
      state.conversations = Array.isArray(parsed) ? parsed : [];
    } catch { state.conversations = []; }
    // A reload mid-answer would otherwise leave a spinner in the saved chat forever.
    for (const c of state.conversations) {
      for (const m of c.messages || []) {
        if (m.pending) { m.pending = false; m.error = true; m.content = m.content || 'No answer was received (the page was reloaded or closed).'; }
      }
    }
  }

  function save() {
    try {
      if (state.conversations.length > settings.maxSavedConversations) {
        state.conversations = state.conversations
          .sort((a, b) => (b.updatedAt || 0) - (a.updatedAt || 0))
          .slice(0, settings.maxSavedConversations);
      }
      localStorage.setItem(STORAGE_KEY, JSON.stringify(state.conversations));
      if (state.currentId) localStorage.setItem(CURRENT_KEY, state.currentId);
      else localStorage.removeItem(CURRENT_KEY);
    } catch (e) { console.warn('Saving chat history failed', e); }
  }

  function getCurrent() {
    return state.conversations.find(c => c.id === state.currentId) || null;
  }

  function newConversation() {
    const c = {
      id: 'c-' + Date.now() + '-' + Math.random().toString(36).slice(2, 7),
      title: 'New chat', createdAt: Date.now(), updatedAt: Date.now(), messages: []
    };
    state.conversations.unshift(c);
    state.currentId = c.id;
    save();
    renderList(); renderTranscript();
    return c;
  }

  function selectConversation(id) {
    if (!state.conversations.find(c => c.id === id)) return;
    state.currentId = id;
    save();
    renderList(); renderTranscript();
  }

  function deleteConversation(id) {
    state.conversations = state.conversations.filter(c => c.id !== id);
    if (state.currentId === id) state.currentId = null;
    save();
    renderList(); renderTranscript();
  }

  function appendMessage(role, content, extra = {}) {
    let conv = getCurrent();
    if (!conv) conv = newConversation();
    const msg = { role, content: content || '', ts: Date.now(), ...extra };
    conv.messages.push(msg);
    conv.updatedAt = msg.ts;
    if (conv.title === 'New chat' && role === 'user' && content) {
      conv.title = content.slice(0, 60).replace(/\s+/g, ' ').trim() || 'New chat';
    }
    save();
    renderList(); renderTranscript();
    return msg;
  }

  function appendSystemNotice(text, kind) {
    const conv = getCurrent();
    // A notice alone does not start a saved chat; it is only useful between questions.
    if (!conv) return null;
    const last = conv.messages[conv.messages.length - 1];
    if (last && last.role === 'system' && last.kind === kind && last.content === text) return last;
    return appendMessage('system', text, { kind: kind || 'info' });
  }

  // Completes the exact pending message, wherever it lives: the user may switch or start chats
  // while the request is in flight. A deleted conversation simply drops the answer.
  function completeMessage(msg, content, extra = {}) {
    const conv = state.conversations.find(c => (c.messages || []).includes(msg));
    if (!conv) return;
    msg.content = content;
    Object.assign(msg, extra);
    conv.updatedAt = Date.now();
    save();
    renderList(); renderTranscript();
  }

  // Earlier answered turns, for follow-up questions. Notices, errors and the pending turn are
  // not conversation and are left out.
  function getRecentMessages(limit) {
    const conv = getCurrent();
    if (!conv) return [];
    const n = Number.isFinite(limit) ? limit : settings.maxHistoryMessages;
    if (!(n > 0)) return [];
    return conv.messages
      .filter(m => (m.role === 'user' || m.role === 'assistant') && !m.pending && !m.error && m.content)
      .slice(-n)
      .map(m => ({ role: m.role, content: m.content }));
  }

  function renderList() {
    const root = document.getElementById('ch-list');
    if (!root) return;
    const q = (state.searchQuery || '').toLowerCase();
    const items = state.conversations
      .filter(c => !q || (c.title || '').toLowerCase().includes(q) ||
                   (c.messages || []).some(m => (m.content || '').toLowerCase().includes(q)))
      .sort((a, b) => (b.updatedAt || 0) - (a.updatedAt || 0));
    if (items.length === 0) {
      root.innerHTML = '<div class="ch-empty">No saved chats yet.</div>';
      return;
    }
    const buckets = { Today: [], Yesterday: [], 'Previous 7 days': [], Older: [] };
    const now = new Date();
    const startOfToday = new Date(now.getFullYear(), now.getMonth(), now.getDate()).getTime();
    const startOfYesterday = startOfToday - 24 * 3600 * 1000;
    const weekAgo = startOfToday - 7 * 24 * 3600 * 1000;
    for (const c of items) {
      const t = c.updatedAt || c.createdAt || 0;
      if (t >= startOfToday) buckets.Today.push(c);
      else if (t >= startOfYesterday) buckets.Yesterday.push(c);
      else if (t >= weekAgo) buckets['Previous 7 days'].push(c);
      else buckets.Older.push(c);
    }
    let html = '';
    for (const [label, list] of Object.entries(buckets)) {
      if (list.length === 0) continue;
      html += `<div class="ch-bucket">${label}</div>`;
      for (const c of list) {
        const active = c.id === state.currentId ? ' ch-item-active' : '';
        const preview = ((c.messages || []).find(m => m.role === 'assistant' && !m.pending)?.content || '').slice(0, 80);
        html += `<div class="ch-item${active}" data-id="${esc(c.id)}" title="${esc(c.title)}">
          <div class="ch-item-row">
            <button type="button" class="ch-item-title">${esc(c.title)}</button>
            <button class="ch-item-del" data-del="${esc(c.id)}" title="Delete">✕</button>
          </div>
          ${preview ? `<div class="ch-item-preview">${esc(preview)}</div>` : ''}
        </div>`;
      }
    }
    root.innerHTML = html;
    root.querySelectorAll('.ch-item').forEach(el => {
      el.addEventListener('click', e => {
        if (e.target.dataset.del) return;
        selectConversation(el.dataset.id);
      });
    });
    root.querySelectorAll('.ch-item-del').forEach(el => {
      el.addEventListener('click', e => {
        e.stopPropagation();
        if (confirm('Delete this conversation?')) deleteConversation(el.dataset.del);
      });
    });
  }

  function renderTranscript() {
    const root = document.getElementById('chat-transcript');
    if (!root) return;
    const conv = getCurrent();
    if (!conv || conv.messages.length === 0) {
      root.hidden = true;
      root.innerHTML = '';
      return;
    }
    root.hidden = false;
    root.innerHTML = conv.messages.map((m, idx) => renderBubble(m, idx)).join('');
    root.querySelectorAll('.ct-copy').forEach(btn => {
      btn.addEventListener('click', () => {
        const msg = conv.messages[Number(btn.dataset.idx)];
        if (msg) navigator.clipboard?.writeText(msg.content || '');
        btn.textContent = '✓ Copied';
        setTimeout(() => { btn.textContent = '⧉ Copy'; }, 1200);
      });
    });
    root.scrollTop = root.scrollHeight;
  }

  function renderBubble(m, idx) {
    if (m.role === 'system') {
      const icon = m.kind === 'report' ? '📊' : m.kind === 'database' ? '🗄️' : 'ℹ️';
      const kind = ['report', 'database', 'info'].includes(m.kind) ? m.kind : 'info';
      return `<div class="ct-system ct-system-${kind}">
        <span class="ct-system-icon">${icon}</span>
        <span class="ct-system-text">${esc(m.content || '')}</span>
      </div>`;
    }
    const isUser = m.role === 'user';
    const isPending = m.pending === true;
    const avatar = isUser ? '<div class="ct-avatar ct-avatar-user">You</div>'
                          : '<div class="ct-avatar ct-avatar-assistant">AI</div>';
    const meta = [];
    if (m.scope === 'report' && m.scopeLabel) meta.push('📊 ' + esc(m.scopeLabel));
    else if (m.scope === 'database') meta.push('🗄️ Database');
    if (m.model) meta.push(esc(m.model));
    if (m.runId) meta.push('Run #' + esc(String(m.runId)));
    if (m.ts) meta.push(esc(new Date(m.ts).toLocaleString()));
    const metaHtml = meta.length ? `<span class="ct-meta">${meta.join(' · ')}</span>` : '';
    const body = isPending
      ? '<div class="ct-pending"><span class="ct-dot"></span><span class="ct-dot"></span><span class="ct-dot"></span></div>'
      : (isUser ? `<div class="ct-text">${esc(m.content || '')}</div>` : renderMarkdown(m.content || ''));
    const actions = !isUser && !isPending
      ? `<div class="ct-actions"><button class="ct-copy" data-idx="${idx}">⧉ Copy</button></div>`
      : '';
    return `<div class="ct-msg ct-msg-${isUser ? 'user' : 'assistant'}">
      ${avatar}
      <div class="ct-body">
        <div class="ct-header">${isUser ? 'You' : 'Assistant'}${metaHtml}</div>
        ${body}
        ${actions}
      </div>
    </div>`;
  }

  // Small markdown subset. Input is escaped first (quotes included), so model output cannot
  // break out of an attribute or inject markup.
  function renderMarkdown(s) {
    if (!s) return '<div class="ct-text"></div>';
    let html = esc(s);
    html = html.replace(/```([a-zA-Z0-9_+-]*)\n([\s\S]*?)```/g, (_, lang, code) =>
      `<pre class="ct-code"><code data-lang="${lang || ''}">${code}</code></pre>`);
    html = html.replace(/`([^`\n]+)`/g, '<code class="ct-icode">$1</code>');
    html = html.replace(/^###\s+(.+)$/gm, '<h4>$1</h4>')
               .replace(/^##\s+(.+)$/gm, '<h3>$1</h3>')
               .replace(/^#\s+(.+)$/gm, '<h2>$1</h2>');
    html = html.replace(/\*\*([^*]+)\*\*/g, '<strong>$1</strong>')
               .replace(/(^|[\s(])\*([^*\n]+)\*/g, '$1<em>$2</em>');
    html = html.replace(/(^|\n)([-*]\s.+(?:\n[-*]\s.+)*)/g, (_, lead, block) => {
      const items = block.split(/\n/).map(l => l.replace(/^[-*]\s+/, '').trim()).filter(Boolean);
      return `${lead}<ul>${items.map(i => `<li>${i}</li>`).join('')}</ul>`;
    });
    html = html.replace(/\[([^\]]+)\]\((https?:\/\/[^)\s]+)\)/g, '<a href="$2" target="_blank" rel="noopener noreferrer">$1</a>');
    html = html.split(/\n\n+/).map(p => {
      if (/^<(h\d|ul|pre|ol|blockquote)/.test(p.trim())) return p;
      return `<p>${p.replace(/\n/g, '<br>')}</p>`;
    }).join('');
    return `<div class="ct-text">${html}</div>`;
  }

  function esc(s) {
    return String(s ?? '')
      .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
  }

  async function loadSettings() {
    try {
      const res = await fetch('/api/chat/settings');
      if (!res.ok) return;
      const s = await res.json();
      if (Number.isFinite(s.maxHistoryMessages) && s.maxHistoryMessages >= 0) settings.maxHistoryMessages = s.maxHistoryMessages;
      if (Number.isFinite(s.maxSavedConversations) && s.maxSavedConversations > 0) settings.maxSavedConversations = s.maxSavedConversations;
    } catch { /* defaults stand */ }
  }

  function init() {
    load();
    // Start with an empty chat area; earlier chats stay in the sidebar to resume.
    state.currentId = null;
    try { localStorage.removeItem(CURRENT_KEY); } catch { /* storage unavailable */ }
    renderList();
    renderTranscript();
    loadSettings();
    document.getElementById('ch-new-btn')?.addEventListener('click', () => newConversation());
    document.getElementById('ch-search')?.addEventListener('input', e => {
      state.searchQuery = e.target.value || '';
      renderList();
    });
  }

  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', init);
  else init();

  window.ChatHistory = {
    getCurrent, newConversation, selectConversation, deleteConversation,
    appendMessage, appendSystemNotice, completeMessage, getRecentMessages,
    renderTranscript, renderList,
  };
})();
