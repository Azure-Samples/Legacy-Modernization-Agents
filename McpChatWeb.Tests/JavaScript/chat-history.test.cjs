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
