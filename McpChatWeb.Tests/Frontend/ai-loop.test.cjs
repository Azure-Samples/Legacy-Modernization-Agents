const { test } = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');

function view(saved = null) {
  const storage = new Map(saved ? [['ai-loop-selection', JSON.stringify(saved)]] : []);
  const root = { innerHTML: '', querySelector: () => null, querySelectorAll: () => [] };
  const context = {
    window: {},
    document: { hidden: false, addEventListener() {}, getElementById: id => id === 'ai-loop-container' ? { style: {} } : root },
    sessionStorage: { getItem: key => storage.get(key), setItem: (key, value) => storage.set(key, value) },
    AbortController, setTimeout, clearTimeout, console,
  };
  vm.runInNewContext(fs.readFileSync(path.join(__dirname, '../../McpChatWeb/wwwroot/ai-loop.js'), 'utf8'), context);
  const instance = new context.window.AiLoopView('root');
  instance.schedule = () => {};
  return { instance, context, root, storage };
}

test('old sticky model selection does not override newest real native activity', async () => {
  const { instance, root } = view({ selected: '75' });
  instance.get = async url => {
    if (url.startsWith('/api/ai-loop/runs')) return {
      runs: [
        { runId: '75', startedAt: '2026-10-22T13:00:00Z', finishedAt: '2026-10-22T13:19:51Z' },
        { runId: 'native-abc', activityKind: 'native-rekt', lastEventAt: '2026-10-22T21:09:12Z' }
      ], managedRuns: [], hasMore: false
    };
    assert.equal(url, '/api/ai-loop/native-abc');
    return { summary: { runId: 'native-abc', activityId: 'native:all-source', activityKind: 'native-rekt',
      status: 'completed-with-gaps', lastEventAt: '2026-10-22T21:09:12Z' }, stages: [],
      native: { recordedName: 'all-source', programs: [{ relativePath: '<A>.cbl', status: 'failed' }] },
      logs: [{ path: 'output/rekt-scans/all-source/logs/A.log', kind: 'structure stderr', available: true }] };
  };
  await instance.loadAndRender();
  assert.equal(instance.followLatest, true);
  assert.equal(instance.selected, 'native-abc');
  assert.match(root.innerHTML, /Native REKT/);
  assert.match(root.innerHTML, /Model calls: not applicable/);
  assert.match(root.innerHTML, /&lt;A&gt;.cbl/);
  assert.match(root.innerHTML, /logs\/A.log/);
  assert.doesNotMatch(root.innerHTML, /Recorded conversion phases|Outstanding requests/);
});

test('explicit versioned native pin is explained and can follow new managed activity', async () => {
  const { instance, root } = view({ version: 2, followLatest: false, selected: 'native-abc' });
  instance.get = async url => {
    if (url.startsWith('/api/ai-loop/runs')) return { runs: [], managedRuns: [
      { runId: 'queued', status: 'pending', startedAt: '2026-10-23T00:00:00Z' }
    ] };
    if (url.includes('/managed/')) return null;
    return { summary: { runId: 'native-abc', activityKind: 'native-rekt', status: 'running' }, stages: [], native: { programs: [] } };
  };
  await instance.loadAndRender();
  assert.match(root.innerHTML, /Pinned selection/);
  assert.equal(instance.selected, 'native-abc');
  await instance.followNewest();
  assert.equal(instance.managedRunId, 'queued');
  assert.match(root.innerHTML, /No model activity or quality-gate result is established/);
});

test('native activity retains a loop using only recorded native stages and stops its halo at terminal status', () => {
  const { instance } = view();
  const detail = { summary: { activityKind: 'native-rekt', status: 'running', currentStage: 'JCL analysis' },
    stages: [{ name: 'Native parsing', durationMs: 1200 }, { name: 'JCL analysis', durationMs: null }],
    native: { programs: [] }, logs: [] };
  const running = instance.renderNative(detail);
  assert.match(running, /class="ail-loop"/);
  assert.match(running, /Recorded native stages/);
  assert.match(running, /JCL analysis: current recorded stage/);
  assert.match(running, /ail-phase-halo/);
  assert.doesNotMatch(running, /Recorded conversion phases|instrumented request|>Convert<|>Compile<|>Parity</);
  detail.summary.currentStage = 'JCL analysis: jobs/DAILY.jcl';
  assert.match(instance.renderNative(detail), /JCL analysis: current recorded stage/);
  detail.summary.status = 'failed';
  const terminal = instance.renderNative(detail);
  assert.doesNotMatch(terminal, /ail-phase-halo|ail-stage-current/);
  assert.match(terminal, /Run failed/);
});

test('native summary does not confuse unrecorded artifact flags with missing structural artifacts', () => {
  const { instance } = view();
  const html = instance.renderNative({ summary: { status: 'completed-with-gaps' }, stages: [],
    native: { structuralPrograms: 0, programs: [{ relativePath: 'A.cbl', status: 'Partial' }] }, logs: [] });
  assert.match(html, /0 with recorded AST \+ CFG \+ data flags/);
  assert.match(html, /1 program records have unrecorded artifact flags/);
  assert.match(html, /does not establish that their AST, CFG or data artifacts are absent/);
});

test('older legacy attempts use physical event position without inventing an attempt ID', async () => {
  const { instance } = view({ version: 2, followLatest: false, selected: '7~@0' });
  const requests = [];
  instance.get = async url => {
    requests.push(url);
    if (url.startsWith('/api/ai-loop/runs')) return { runs: [], managedRuns: [], hasMore: false, nextOffset: 50 };
    return { summary: { runId: '7', attemptIndex: 0, status: 'completed' }, gates: [], stages: [], agents: [], events: [], requests: [], contextHandoffs: [] };
  };
  await instance.loadAndRender();
  assert.ok(requests.includes('/api/ai-loop/7?attemptIndex=0'));
  assert.equal(instance.selected, '7~@0');
});

test('managed failures display recovered reasons and escaped recorded logs, not a waiting placeholder', async () => {
  const { instance, root } = view({ managedRunId: 'rejected' });
  instance.get = async url => {
    if (url.startsWith('/api/ai-loop/runs')) return {
      runs: [], managedRuns: [{ runId: 'rejected', status: 'failed' }], hasMore: false, nextOffset: 50,
    };
    if (url.startsWith('/api/ai-loop/managed/')) return null;
    return { info: { runId: 'rejected', status: 'failed', error: 'Unsupported speed profile', historyWarning: 'Missing historical data' }, log: ['<script>not markup</script>'] };
  };
  await instance.loadAndRender();
  assert.match(root.innerHTML, /Unsupported speed profile/);
  assert.match(root.innerHTML, /Missing historical data/);
  assert.match(root.innerHTML, /&lt;script&gt;not markup&lt;\/script&gt;/);
  assert.doesNotMatch(root.innerHTML, /Waiting for this launch/);
});

for (const selection of ['managed', 'attempt']) {
  test(`selecting another ${selection} clears the previous launch log before failed refreshes`, async () => {
    const { instance, root } = view();
    instance.get = async url => {
      if (url.startsWith('/api/ai-loop/runs')) return {
        runs: [], managedRuns: [{ runId: 'launch-A', status: 'failed' }, { runId: 'launch-B', status: 'failed' }],
        hasMore: false, nextOffset: 50,
      };
      if (url.startsWith('/api/ai-loop/managed/')) return null;
      return { info: { runId: 'launch-A', status: 'failed' }, log: ['launch-A-private-output'] };
    };
    await instance.selectManagedRun('launch-A');
    assert.match(root.innerHTML, /launch-A-private-output/);

    instance.get = async () => { throw new Error('history refresh failed'); };
    const pending = selection === 'managed'
      ? instance.selectManagedRun('launch-B') : instance.select('migration-B~attempt-B');
    assert.equal(instance.processLog, null);
    assert.doesNotMatch(root.innerHTML, /launch-A-private-output/);
    await pending;
    assert.match(root.innerHTML, /could not be refreshed/);
    assert.doesNotMatch(root.innerHTML, /launch-A-private-output/);
    await instance.loadAndRender();
    assert.equal(instance.processLog, null);
    assert.doesNotMatch(root.innerHTML, /launch-A-private-output/);
  });
}

test('database-only history is explicitly unmeasured, not zero model calls', () => {
  const { instance } = view();
  const html = instance.renderDetail({
    summary: { runId: '1', status: 'completed', telemetryAvailable: false, calls: 0, failedCalls: 0 },
    warning: 'Telemetry cannot be reconstructed', gates: [], stages: [], agents: [], events: [], requests: [], contextHandoffs: [],
  });
  assert.match(html, /Telemetry cannot be reconstructed/);
  assert.match(html, /not recorded<\/div><div class="emc-kpi-label">recorded outcomes/);
});

test('history pagination refreshes previously loaded pages rather than dropping old history', async () => {
  const { instance } = view();
  instance.historyOffsets = [0, 50];
  instance.get = async url => ({
    runs: [{ runId: url.endsWith('=50') ? '1' : '2', status: 'completed' }],
    managedRuns: [], hasMore: false, nextOffset: 100,
  });
  instance.render = () => {};
  await instance.loadAndRender();
  assert.equal(instance.runs.length, 2);
  assert.equal(instance.runs[1].runId, '1');
});

test('server-bounded event and context histories are not silently truncated again in the browser', () => {
  const { instance } = view();
  const events = Array.from({ length: 180 }, (_, i) => ({
    ts: '2026-10-22T12:45:26Z', kind: 'llm_call', requestId: `request-${i}`,
  }));
  const html = instance.renderEvents(events);
  assert.match(html, /request-0</);
  assert.match(html, /request-179</);
  assert.equal((html.match(/class="ail-event /g) || []).length, 180);
  assert.match(html, /title="2026-10-22T12:45:26Z"/);
  const contexts = events.slice(0, 48).map((e, i) => ({ ts: e.ts, file: `file-${i}`, state: 'prepared' }));
  const contextHtml = instance.renderContexts(contexts);
  assert.match(contextHtml, /file-0/);
  assert.equal((contextHtml.match(/<li>/g) || []).length, 48);
});

test('freshness distinguishes successful refresh time from historical activity and labels timezone', async () => {
  const { instance, root } = view({ selected: '7' });
  instance.get = async url => url.startsWith('/api/ai-loop/runs')
    ? { runs: [{ runId: '7' }], managedRuns: [], nextOffset: 50 }
    : { summary: { runId: '7', status: 'completed', startedAt: '2026-10-22T12:45:26Z',
      finishedAt: '2026-10-22T13:19:51Z', lastEventAt: '2026-10-22T13:19:51Z' },
      gates: [], stages: [], agents: [], events: [], requests: [], contextHandoffs: [] };
  await instance.loadAndRender();
  const refreshed = instance.lastRefreshedAt;
  assert.ok(refreshed);
  assert.match(root.innerHTML, /Last recorded activity:/);
  assert.match(root.innerHTML, /Times shown in/);
  assert.match(root.innerHTML, /refresh time is not work time/);
  instance.get = async () => { throw new Error('offline'); };
  await instance.loadAndRender();
  assert.equal(instance.lastRefreshedAt, refreshed);
  assert.match(root.innerHTML, /may be stale/);
  assert.equal(instance.when(null), 'not recorded');
});

test('a frozen older backend shows the newest supplied record without claiming complete freshness metadata', () => {
  const { instance } = view();
  const last = '2026-10-22T13:19:51Z';
  const html = instance.renderDetail({
    summary: { runId: '7', status: 'completed' }, gates: [], stages: [], agents: [], requests: [],
    events: [{ kind: 'run_started', ts: '2026-10-22T12:00:00Z' }],
    contextHandoffs: [{ state: 'prepared', ts: last }],
  });
  assert.ok(html.includes(instance.when(last)));
  assert.match(html, /latest supplied record; complete telemetry metadata requires the updated backend/);
});
