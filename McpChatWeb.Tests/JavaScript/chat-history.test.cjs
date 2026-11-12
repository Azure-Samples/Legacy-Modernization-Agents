const { test } = require('node:test');
const assert = require('node:assert/strict');
const { readFileSync } = require('node:fs');
const { join } = require('node:path');
const vm = require('node:vm');

const plain = value => JSON.parse(JSON.stringify(value));

const source = readFileSync(join(__dirname, '../../McpChatWeb/wwwroot/chat-history.js'), 'utf8');

async function load(maxHistoryMessages = 8) {
  const store = new Map();
  const list = { innerHTML: '', querySelectorAll: () => [] };
  const window = {};
  vm.runInNewContext(source, {
    window, console, Date, Math, JSON, Number,
    localStorage: { getItem: k => store.get(k) ?? null, setItem: (k, v) => store.set(k, v), removeItem: k => store.delete(k) },
    document: { readyState: 'complete', getElementById: id => (id === 'ch-list' ? list : null) },
    fetch: async () => ({ ok: true, json: async () => ({ maxHistoryMessages, maxSavedConversations: 100 }) }),
  });
  await new Promise(resolve => setImmediate(resolve));
  return { history: window.ChatHistory, list };
}

test('an answer completes its own chat after the user switches to another', async () => {
  const { history } = await load();
  const first = history.newConversation();
  history.appendMessage('user', 'question A');
  const pending = history.appendMessage('assistant', '', { pending: true });
  const second = history.newConversation();
  history.appendMessage('user', 'question B');
  history.appendMessage('assistant', 'answer B');

  history.completeMessage(pending, 'answer A', { pending: false });

  assert.deepEqual(plain(first.messages.map(m => [m.content, !!m.pending])), [['question A', false], ['answer A', false]]);
  assert.equal(second.messages.at(-1).content, 'answer B');
  assert.equal(history.getCurrent().id, second.id);
});

test('a zero history limit sends no earlier messages', async () => {
  const { history } = await load(0);
  history.appendMessage('user', 'q');
  history.appendMessage('assistant', 'a');
  assert.deepEqual(plain(history.getRecentMessages()), []);
  assert.equal(history.getRecentMessages(1).length, 1);
});

test('saved chat titles are keyboard-reachable buttons', async () => {
  const { history, list } = await load();
  history.appendMessage('user', 'saved question');
  assert.match(list.innerHTML, /<button type="button" class="ch-item-title">saved question<\/button>/);
});

function loadWith({ setItem, transcript } = {}) {
  const store = new Map();
  const list = { innerHTML: '', querySelectorAll: () => [] };
  const window = {};
  let clock = Date.now();
  // A tick per call, so chats created in the same millisecond still have an order.
  const TickingDate = class extends Date { static now() { return ++clock; } };
  vm.runInNewContext(source, {
    window, console: { warn() {} }, Date: TickingDate, Math, JSON, Number, Error, Object,
    localStorage: {
      getItem: k => store.get(k) ?? null,
      setItem: (k, v) => (setItem ? setItem(k, v, store) : store.set(k, v)),
      removeItem: k => store.delete(k),
    },
    document: { readyState: 'complete', getElementById: id => (id === 'ch-list' ? list : id === 'chat-transcript' ? transcript ?? null : null) },
    fetch: async () => ({ ok: false }),
  });
  return { history: window.ChatHistory, list, store };
}

test('a full browser store drops the oldest chats, keeps the open one and says so', () => {
  const quota = Object.assign(new Error('full'), { name: 'QuotaExceededError' });
  const { history, list, store } = loadWith({
    setItem: (k, v, s) => { if (v.length > 2500) throw quota; s.set(k, v); },
  });
  for (const q of ['first', 'second', 'third']) {
    history.newConversation();
    history.appendMessage('user', q);
    history.appendMessage('assistant', q + ' ' + 'x'.repeat(1000));
  }
  const saved = JSON.parse(store.get('mcp.chatHistory.v1'));
  assert.deepEqual(saved.map(c => c.title), ['third', 'second']);
  assert.equal(history.getCurrent().title, 'third');
  assert.match(list.innerHTML, /role="status">1 older chat was removed to stay within browser storage/);
});

test('a failed save is shown to the user instead of only logged', () => {
  const { history, list } = loadWith({
    setItem: () => { throw Object.assign(new Error('denied'), { name: 'SecurityError' }); },
  });
  history.appendMessage('user', 'not persisted');
  assert.match(list.innerHTML, /role="alert">Chat history cannot be saved in this browser/);
});

test('code spans and fenced blocks are not formatted as markdown', () => {
  const transcript = { hidden: true, innerHTML: '', querySelectorAll: () => [] };
  const { history } = loadWith({ transcript });
  history.appendMessage('assistant',
    'Use `**not bold**` and `[x](https://e.example)`.\n\n```js\nconst a = "**b**";\n\nlink [y](https://e.example)\n```\nafter **bold**');
  const html = transcript.innerHTML;
  assert.match(html, /<code class="ct-icode">\*\*not bold\*\*<\/code>/);
  assert.match(html, /<code class="ct-icode">\[x\]\(https:\/\/e\.example\)<\/code>/);
  assert.match(html, /<pre class="ct-code"><code data-lang="js">const a = &quot;\*\*b\*\*&quot;;\n\nlink \[y\]\(https:\/\/e\.example\)\n<\/code><\/pre>/);
  assert.match(html, /<p>after <strong>bold<\/strong><\/p>/);
  assert.doesNotMatch(html, /<a href/);
  assert.doesNotMatch(html, /\u0000/);
});
