// ═══════════════════════════════════════════════════════════════════════
// Chat history — localStorage-backed conversation store with a ChatGPT-style
// transcript and a sidebar list. Survives page reloads, supports continuing a
// previous chat, and surfaces a click-to-resume list in the MCP Resources panel.
// ═══════════════════════════════════════════════════════════════════════

(function () {
  'use strict';

  const STORAGE_KEY = 'mcp.chatHistory.v1';
  const CURRENT_KEY = 'mcp.chatHistory.currentId';
  const MAX_CONVERSATIONS = 100;

  const state = {
    conversations: [], // [{id, title, createdAt, updatedAt, messages: [{role, content, ts, model, runSummary, runId, isMcpCall}]}]
    currentId: null,
    searchQuery: '',
  };

  // ── Persistence ─────────────────────────────────────────────────────
  function load() {
    try {
      const raw = localStorage.getItem(STORAGE_KEY);
      state.conversations = raw ? JSON.parse(raw) : [];
    } catch { state.conversations = []; }
    state.currentId = localStorage.getItem(CURRENT_KEY) || null;
  }
  function save() {
    try {
      // Cap to keep storage bounded.
      if (state.conversations.length > MAX_CONVERSATIONS) {
        state.conversations = state.conversations
          .sort((a, b) => (b.updatedAt || 0) - (a.updatedAt || 0))
          .slice(0, MAX_CONVERSATIONS);
      }
      localStorage.setItem(STORAGE_KEY, JSON.stringify(state.conversations));
      if (state.currentId) localStorage.setItem(CURRENT_KEY, state.currentId);
      else localStorage.removeItem(CURRENT_KEY);
    } catch (e) { console.warn('Chat history save failed', e); }
  }

  // ── Public API ──────────────────────────────────────────────────────
  function getCurrent() {
    return state.conversations.find(c => c.id === state.currentId) || null;
  }
  function newConversation() {
    const c = { id: 'c-' + Date.now() + '-' + Math.random().toString(36).slice(2, 7),
                title: 'New chat', createdAt: Date.now(), updatedAt: Date.now(), messages: [] };
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
    renderList();
    renderTranscript();
    return msg;
  }

  // System notice — surfaced in the transcript as a small banner row.
  // Use this for "now chatting with report X" / "switched back to database" etc.
  function appendSystemNotice(text, kind) {
    let conv = getCurrent();
    if (!conv) conv = newConversation();
    // Suppress duplicate notices when a setting toggles rapidly (debounce by kind).
    const last = conv.messages[conv.messages.length - 1];
    if (last && last.role === 'system' && last.kind === kind && last.content === text) return last;
    return appendMessage('system', text, { kind: kind || 'info' });
  }
  function updateLastAssistantMessage(content, extra = {}) {
    const conv = getCurrent();
    if (!conv) return;
    for (let i = conv.messages.length - 1; i >= 0; i--) {
      if (conv.messages[i].role === 'assistant') {
        conv.messages[i].content = content;
        Object.assign(conv.messages[i], extra);
        conv.updatedAt = Date.now();
        save();
        renderTranscript();
        return;
      }
    }
  }
  function getRecentMessages(limit = 10) {
    const conv = getCurrent();
    if (!conv) return [];
    return conv.messages.slice(-limit).map(m => ({ role: m.role, content: m.content }));
  }

  // ── Sidebar list rendering ──────────────────────────────────────────
  function renderList() {
    const root = document.getElementById('ch-list');
    if (!root) return;
    const q = (state.searchQuery || '').toLowerCase();
    const items = state.conversations
      .filter(c => !q || c.title.toLowerCase().includes(q) ||
                   c.messages.some(m => (m.content || '').toLowerCase().includes(q)))
      .sort((a, b) => (b.updatedAt || 0) - (a.updatedAt || 0));
    if (items.length === 0) {
      root.innerHTML = '<div class="ch-empty">No saved chats yet.</div>';
      return;
    }
    const buckets = { Today: [], Yesterday: [], 'Previous 7 days': [], 'Older': [] };
    const now = new Date(); const startOfToday = new Date(now.getFullYear(), now.getMonth(), now.getDate()).getTime();
    const startOfYday = startOfToday - 24 * 3600 * 1000;
    const week = startOfToday - 7 * 24 * 3600 * 1000;
    for (const c of items) {
      const t = c.updatedAt || c.createdAt || 0;
      if (t >= startOfToday) buckets.Today.push(c);
      else if (t >= startOfYday) buckets.Yesterday.push(c);
      else if (t >= week) buckets['Previous 7 days'].push(c);
      else buckets.Older.push(c);
    }
    let html = '';
    for (const [label, list] of Object.entries(buckets)) {
      if (list.length === 0) continue;
      html += `<div class="ch-bucket">${label}</div>`;
      for (const c of list) {
        const active = c.id === state.currentId ? ' ch-item-active' : '';
        const preview = (c.messages.find(m => m.role === 'assistant')?.content || '').slice(0, 80);
        html += `<div class="ch-item${active}" data-id="${c.id}" title="${esc(c.title)}\n${new Date(c.updatedAt).toLocaleString()}">
          <div class="ch-item-row">
            <span class="ch-item-title">${esc(c.title)}</span>
            <button class="ch-item-del" data-del="${c.id}" title="Delete">✕</button>
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

  // ── Transcript rendering ────────────────────────────────────────────
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
    root.innerHTML = conv.messages.map(m => renderBubble(m)).join('');
    // Wire copy buttons
    root.querySelectorAll('.ct-copy').forEach(btn => {
      btn.addEventListener('click', () => {
        const idx = Number(btn.dataset.idx);
        const msg = conv.messages[idx];
        if (msg) navigator.clipboard?.writeText(msg.content || '');
        btn.textContent = '✓ Copied';
        setTimeout(() => { btn.textContent = '⧉ Copy'; }, 1200);
      });
    });
    // Auto-scroll to bottom
    root.scrollTop = root.scrollHeight;
  }

  function renderBubble(m) {
    const idx = (getCurrent()?.messages.indexOf(m)) ?? -1;
    if (m.role === 'system') {
      const icon = m.kind === 'report' ? '📊' : m.kind === 'database' ? '🗄️' : 'ℹ️';
      return `<div class="ct-system ct-system-${esc(m.kind || 'info')}">
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
    if (m.isMcpCall) meta.push('MCP');
    if (m.runId) meta.push('Run #' + esc(String(m.runId)));
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

  // Lightweight markdown: code fences, inline code, bold, italic, headers, lists, links.
  function renderMarkdown(s) {
    if (!s) return '<div class="ct-text"></div>';
    let html = esc(s);
    // Code fences ```lang\n...\n```
    html = html.replace(/```([a-zA-Z0-9_+-]*)\n([\s\S]*?)```/g, (_, lang, code) =>
      `<pre class="ct-code"><code data-lang="${esc(lang || '')}">${code}</code></pre>`);
    // Inline code `x`
    html = html.replace(/`([^`\n]+)`/g, '<code class="ct-icode">$1</code>');
    // Headers
    html = html.replace(/^###\s+(.+)$/gm, '<h4>$1</h4>')
               .replace(/^##\s+(.+)$/gm, '<h3>$1</h3>')
               .replace(/^#\s+(.+)$/gm, '<h2>$1</h2>');
    // Bold / italic
    html = html.replace(/\*\*([^*]+)\*\*/g, '<strong>$1</strong>')
               .replace(/(^|[\s(])\*([^*\n]+)\*/g, '$1<em>$2</em>');
    // Bullet lists
    html = html.replace(/(^|\n)([-*]\s.+(?:\n[-*]\s.+)*)/g, (m, lead, block) => {
      const items = block.split(/\n/).map(l => l.replace(/^[-*]\s+/, '').trim()).filter(Boolean);
      return `${lead}<ul>${items.map(i => `<li>${i}</li>`).join('')}</ul>`;
    });
    // Links [text](url)
    html = html.replace(/\[([^\]]+)\]\((https?:[^)]+)\)/g, '<a href="$2" target="_blank" rel="noopener">$1</a>');
    // Paragraph breaks
    html = html.split(/\n\n+/).map(p => {
      if (/^<(h\d|ul|pre|ol|blockquote)/.test(p.trim())) return p;
      return `<p>${p.replace(/\n/g, '<br>')}</p>`;
    }).join('');
    return `<div class="ct-text">${html}</div>`;
  }

  function esc(s) {
    return String(s ?? '').replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
  }

  // ── Wiring ──────────────────────────────────────────────────────────
  function init() {
    load();
    if (!getCurrent() && state.conversations.length === 0) {
      // Lazy: don't auto-create. Wait for the first user message.
    } else if (!getCurrent()) {
      // Pick the most recent if currentId is stale.
      const latest = state.conversations.sort((a, b) => (b.updatedAt || 0) - (a.updatedAt || 0))[0];
      if (latest) state.currentId = latest.id;
    }
    renderList();
    renderTranscript();
    document.getElementById('ch-new-btn')?.addEventListener('click', () => newConversation());
    const search = document.getElementById('ch-search');
    search?.addEventListener('input', e => { state.searchQuery = e.target.value || ''; renderList(); });
  }

  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', init);
  else init();

  // Expose to other modules
  window.ChatHistory = {
    getCurrent, newConversation, selectConversation, deleteConversation,
    appendMessage, appendSystemNotice, updateLastAssistantMessage, getRecentMessages,
    renderTranscript, renderList,
  };
})();
