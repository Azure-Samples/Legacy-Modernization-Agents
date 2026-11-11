const { test } = require('node:test');
const assert = require('node:assert/strict');
const { readFileSync } = require('node:fs');
const { join } = require('node:path');
const vm = require('node:vm');

const source = readFileSync(join(__dirname, '../../McpChatWeb/wwwroot/ai-loop.js'), 'utf8');
const readinessSource = readFileSync(join(__dirname, '../../McpChatWeb/wwwroot/conversion-readiness.js'), 'utf8');
const response = data => ({ ok: true, json: async () => data });
const isRunList = url => new URL(url, 'http://portal').pathname === '/api/ai-loop/runs';
const list = runs => ({
  runs: runs.map(runId => ({ runId, status: 'completed' })), managedRuns: [],
  pollSeconds: 5, nextOffset: 50, hasMore: false
});
const detail = runId => ({
  summary: { runId, status: 'running', calls: 0, retries: 0, fallbacks: 0, failedCalls: 0 },
  stages: [], agents: [], gates: [], events: [], requests: [], contextHandoffs: []
});
const deferred = () => {
  let resolve;
  const promise = new Promise(r => { resolve = r; });
  return { promise, resolve };
};
const flush = () => new Promise(resolve => setImmediate(resolve));

function fixture(fetcher) {
  const listeners = new Map(), timers = new Map(), calls = [];
  let timerId = 0;
  const motion = {};
  const root = { innerHTML: '', querySelector: selector => selector === '.ail'
    ? { setAttribute: (name, value) => { motion[name] = value; } } : null,
    querySelectorAll: () => [] };
  const document = {
    hidden: false,
    addEventListener: (name, callback) => listeners.set(name, callback),
    getElementById: id => id === 'ai-loop-container' ? { style: { display: '' } } : root
  };
  const context = vm.createContext({
    document, window: {}, AbortController,
    setTimeout: (callback, delay) => { timers.set(++timerId, { callback, delay }); return timerId; },
    clearTimeout: id => timers.delete(id),
    fetch: (url, options) => {
      calls.push({ url, signal: options.signal });
      return fetcher(url, options, calls.length);
    }
  });
  vm.runInContext(readinessSource, context);
  vm.runInContext(source, context);
  const view = new context.window.AiLoopView('root');
  return { view, document, root, timers, calls, listeners, motion };
}

test('native results explain input and parser gaps without treating recorded bindings as current files', () => {
  const f = fixture(() => Promise.resolve(response({})));
  const d = { ...detail('native-inputs'), native: { programs: [
    { relativePath: 'bank/A.cbl', status: 'Partial', strictDependencyComplete: false,
      dependencyIssues: [
        { category: 'missing-copybook', item: 'HEADER', reason: 'COPY HEADER could not be resolved.', action: 'Supply the real header.' },
        { category: 'unsupported', item: '<context>', reason: 'Parser bindings are incomplete.', action: 'Extend the exporter.' }
      ],
      dependencyDiagnosticsTruncated: true,
      dependencyInputs: { items: [
        { kind: 'COPY', name: 'SHARED', status: 'recorded', scope: 'shared', path: '/old/source/SHARED.cpy' }
      ] },
      measurementReasons: { dependencyDetails: '<raw-condition>' } },
    { relativePath: 'legacy/B.cbl', status: 'Full' }
  ] } };
  const html = f.view.renderNative(d);
  assert.match(html, /Unresolved COPY input/);
  assert.match(html, /Parser\/exporter support gap/);
  assert.match(html, /&lt;context&gt;/);
  assert.match(html, /Present at scan time/);
  assert.match(html, /not current filesystem checks/);
  assert.match(html, /Input inventory not recorded/);
  assert.match(html, /Raw measurement notes/);
  assert.match(html, /&lt;raw-condition&gt;/);
  assert.match(html, /Diagnostic display is truncated/);
  assert.doesNotMatch(html, /<context>|<raw-condition>/);
});

test('native log fetch is expansion/visibility gated and late responses cannot cross selections', async () => {
      const pending = deferred();
      const f = fixture(() => pending.promise);
      const viewer = { open: false };
      f.root.querySelector = selector => selector === '#ail-native-log' ? viewer : null;
      f.view.active = true;
      f.view.detail = { ...detail('native-old'), summary: { runId: 'native-old', activityKind: 'native-rekt' } };
      await f.view.refreshNativeLog();
      assert.equal(f.calls.length, 0);
      viewer.open = true;
      const reading = f.view.refreshNativeLog();
      assert.equal(f.calls.length, 1);
      assert.equal(f.calls[0].url, '/api/ai-loop/native-old/native-log');
      f.view.cancel();
      f.view.detail = { ...detail('native-new'), summary: { runId: 'native-new', activityKind: 'native-rekt' } };
      pending.resolve(response({ runId: 'native-old', text: 'old private text' }));
      await reading;
      assert.equal(f.view.nativeLogs.size, 0);
      f.document.hidden = true;
      await f.view.refreshNativeLog();
      assert.equal(f.calls.length, 1);
    });

    test('native log escapes compiler HTML, strips ANSI and keeps original text visibly stale on failure', async () => {
      let failed = false;
      const f = fixture(() => failed
        ? Promise.resolve({ ok: false, json: async () => ({ error: 'Native log is missing. Check scan directory.' }) })
        : Promise.resolve(response({ runId: 'native-test', text: '\x1b[31m<script>compiler</script>\x1b[0m',
          readBytes: 40, totalBytes: 100000, truncated: true })));
      f.root.querySelector = selector => selector === '#ail-native-log' ? { open: true } : null;
      f.view.active = true;
      f.view.detail = { summary: { runId: 'native-test', activityKind: 'native-rekt' } };
      await f.view.refreshNativeLog();
      const initial = f.view.renderNativeLogBody('native-test');
      assert.match(initial, /&lt;script&gt;compiler&lt;\/script&gt;/);
      assert.doesNotMatch(initial, /\x1b|<script>/);
      assert.match(initial, /truncated tail/);
      failed = true;
      await f.view.refreshNativeLog();
      const stale = f.view.renderNativeLogBody('native-test');
      assert.match(stale, /may be stale/);
      assert.match(stale, /Check scan directory/);
      assert.match(stale, /compiler/);
    });

    test('native expansion and log scroll are captured per selection rather than leaking to another run', () => {
      const f = fixture(() => Promise.resolve(response({})));
      const details = [{ open: true }, { open: false }];
      const scroll = [{ dataset: { ailScroll: 'native-log' }, scrollTop: 120, scrollLeft: 4 }];
      f.root.querySelectorAll = selector => selector === 'details' ? details : selector === '[data-ail-scroll]' ? scroll : [];
      f.view.renderedSelection = 'native-one';
      f.view.renderedHasDetail = true;
      f.view.captureDisplayState();
      assert.equal(f.view.displayStates.get('native-one').details['detail-0'], true);
      assert.equal(f.view.displayStates.get('native-one').details['detail-1'], false);
      assert.equal(f.view.displayStates.get('native-one').scroll[0].top, 120);
      assert.equal(f.view.displayStates.has('native-two'), false);
      f.view.renderedHasDetail = false;
      details[0].open = false;
      f.view.captureDisplayState();
      assert.equal(f.view.displayStates.get('native-one').details['detail-0'], true);
    });

test('polling is serialized and discovers a run after an empty list', async () => {
  const pending = deferred();
  const f = fixture((url, options, n) => n === 1 ? pending.promise
    : Promise.resolve(response(isRunList(url) ? list(['new']) : detail('new'))));
  const first = f.view.loadAndRender();
  assert.equal(f.calls.length, 1);
  assert.equal(f.calls[0].url, '/api/ai-loop/runs?offset=0');
  assert.equal(f.timers.size, 0);
  pending.resolve(response(list([])));
  await first;
  const timer = [...f.timers.values()][0];
  assert.equal(timer.delay, 15000);
  await timer.callback();
  assert.equal(f.calls.length, 3);
  assert.equal(f.calls[1].url, '/api/ai-loop/runs?offset=0');
  assert.equal(f.view.selected, 'new');
  assert.equal(f.view.detail.summary.runId, 'new');
  assert.equal(f.timers.size, 1);
});

test('default live selection follows newly recorded attempts while explicit selection stays pinned', async () => {
  let poll = 0;
  const f = fixture(url => Promise.resolve(response(isRunList(url)
    ? list([++poll === 1 ? 'old' : 'new', 'older']) : detail(url.split('/').at(-1)))));
  await f.view.loadAndRender();
  await [...f.timers.values()][0].callback();
  assert.equal(f.view.runs[0].runId, 'new');
  assert.equal(f.view.selected, 'new');
  await f.view.select('old');
  await [...f.timers.values()][0].callback();
  assert.equal(f.view.selected, 'old');
  assert.match(f.root.innerHTML, /Pinned selection/);
  await f.view.followNewest();
  assert.equal(f.view.selected, 'new');
  assert.equal(f.timers.size, 1);
});

test('run selection aborts old detail and rejects its late ignored-abort response', async () => {
  const old = deferred();
  const f = fixture(url => isRunList(url) ? Promise.resolve(response(list(['1', '2'])))
    : url.endsWith('/1') ? old.promise : Promise.resolve(response(detail('2'))));
  const initial = f.view.loadAndRender();
  await flush();
  assert.equal(f.calls[1].url, '/api/ai-loop/1');
  await f.view.select('2');
  assert.equal(f.calls[1].signal.aborted, true);
  old.resolve(response(detail('1')));
  await initial;
  assert.equal(f.view.selected, '2');
  assert.equal(f.view.detail.summary.runId, '2');
  assert.equal(f.timers.size, 1);
});

test('a pinned running detail outside the loaded history page uses the live poll interval', async () => {
  const f = fixture(url => Promise.resolve(response(isRunList(url) ? list(['newer-completed']) : detail('outside-page'))));
  await f.view.select('outside-page');
  assert.equal(f.view.selected, 'outside-page');
  assert.equal([...f.timers.values()][0].delay, 5000);
});

test('deactivation aborts and cannot render a late result or leave a polling timer', async () => {
  const pending = deferred();
  const f = fixture(() => pending.promise);
  const loading = f.view.loadAndRender();
  f.view.deactivate();
  assert.equal(f.calls[0].signal.aborted, true);
  pending.resolve(response(list(['late'])));
  await loading;
  assert.equal(f.view.runs.length, 0);
  assert.equal(f.root.innerHTML, '');
  assert.equal(f.timers.size, 0);
});

test('document hiding cancels requests and visibility resumes discovery', async () => {
  const pending = deferred();
  const f = fixture((url, options, n) => n === 1 ? pending.promise : Promise.resolve(response(list([]))));
  const loading = f.view.loadAndRender();
  f.document.hidden = true;
  f.listeners.get('visibilitychange')();
  assert.equal(f.calls[0].signal.aborted, true);
  pending.resolve(response(list(['hidden'])));
  await loading;
  assert.equal(f.view.selected, null);
  assert.equal(f.timers.size, 0);
  f.document.hidden = false;
  f.listeners.get('visibilitychange')();
  await flush();
  assert.equal(f.calls.length, 2);
  assert.equal(f.timers.size, 1);
});

test('errors retain labelled stale data, back off, and never render server error content', async () => {
  let fail = false;
  const f = fixture(url => {
    if (fail) throw new Error('<script>secret provider payload</script>');
    return Promise.resolve(response(isRunList(url) ? list(['1']) : detail('1')));
  });
  await f.view.loadAndRender();
  fail = true;
  for (const delay of [10000, 20000, 40000, 60000]) {
    await f.view.loadAndRender();
    assert.equal(f.view.detail.summary.runId, '1');
    assert.match(f.root.innerHTML, /may be stale/);
    assert.doesNotMatch(f.root.innerHTML, /secret provider payload/);
    assert.equal([...f.timers.values()][0].delay, delay);
    assert.equal(f.timers.size, 1);
  }
  fail = false;
  await f.view.loadAndRender();
  assert.equal(f.view.error, null);
  assert.equal(f.view.failures, 0);
  assert.equal([...f.timers.values()][0].delay, 5000);
});

test('a response for another run cannot replace selected run data', async () => {
  const f = fixture(url => Promise.resolve(response(isRunList(url) ? list(['1']) : detail('2'))));
  await f.view.loadAndRender();
  assert.equal(f.view.selected, '1');
  assert.equal(f.view.detail, null);
  assert.match(f.view.error, /could not be refreshed/);
});

test('legacy missing data is not zero, successful execution, or a dispatched handoff', () => {
  const { view } = fixture(() => { throw new Error('unexpected fetch'); });
  const d = detail('legacy');
  d.contextHandoffs = [{ agent: 'Converter', file: 'A.cbl', state: 'prepared', execution: 'reused', mode: 'projection' }];
  const html = view.renderDetail(d);
  assert.match(html, /not recorded/);
  assert.match(html, /prepared/);
  assert.match(html, /reused/);
  assert.match(html, /Prepared does not mean dispatched/);
  assert.match(html, /No timeline events recorded/);
  assert.doesNotMatch(html, /Every call succeeded|0 requests started/);
});

test('ring uses recorded phases and escaped metadata without inventing completed gates', () => {
  const { view } = fixture(() => { throw new Error('unexpected fetch'); });
  const d = detail('1');
  d.summary.currentStage = 'Small File Conversion';
  d.stages = [{ name: 'Small File Conversion', number: 3, total: 6 }];
  d.events = [{ kind: 'llm_request_started', context: '<script>bad</script>', requestId: '"onclick', agent: 'A' }];
  const html = view.renderDetail(d);
  assert.match(html, /ail-phase-active/);
  assert.match(html, /current recorded stage/);
  assert.match(html, /not proof of success/);
  assert.match(html, /not parser provenance or authoritative discovery/);
  assert.match(html, /&lt;script&gt;bad&lt;\/script&gt;/);
  assert.match(html, /&quot;onclick/);
  assert.doesNotMatch(html, /<script>bad<\/script>/);
});

test('post-conversion validation highlights verification rather than conversion', () => {
  const { view } = fixture(() => { throw new Error('unexpected fetch'); });
  const html = view.renderLoop([{ name: 'Post-Conversion Validation' }],
    { status: 'running', currentStage: 'Post-Conversion Validation' });
  assert.match(html, /Parity: current recorded stage/);
  assert.match(html, /Convert: not recorded/);
});

test('observed transitions end only recorded intervals and never pass skipped compile or parity', () => {
  const { view } = fixture(() => { throw new Error('unexpected fetch'); });
  const stages = [
    { name: 'COBOL Analysis', durationMs: 200 },
    { name: 'Small File Conversion', durationMs: 0 },
    { name: 'Conversion Report', durationMs: null }
  ];
  const html = view.renderLoop(stages, { status: 'running', currentStage: 'Conversion Report' });
  assert.match(html, /Analyze: ended/);
  assert.match(html, /Convert: ended/);
  assert.match(html, /Report: current recorded stage/);
  assert.match(html, /Compile: not recorded/);
  assert.match(html, /Parity: not recorded/);
  assert.match(html, /aria-label="Recorded conversion phases\. Discover: not recorded\. Analyze: ended/);
  assert.match(html, /3 of 7 phases observed · 2 of 3 recorded stage intervals ended/);
  assert.doesNotMatch(html, /ail-phase-gate-pass/);
  const bars = view.renderStages(stages, { status: 'running' });
  assert.match(bars, /200 ms · ended/);
  assert.match(bars, /0 ms · ended/);
  assert.match(bars, /not completion/);
  assert.doesNotMatch(bars, /emc-good|width:4%/);
});

test('repeated phases remain current and gate outcomes are independent evidence', () => {
  const { view } = fixture(() => { throw new Error('unexpected fetch'); });
  const html = view.renderLoop([
    { name: 'Compile', durationMs: 100 }, { name: 'Compile Repair', durationMs: null }
  ], { status: 'running', currentStage: 'Compile Repair' }, [
    { id: 'compile', status: 'fail' }, { id: 'parity', status: 'pass' }
  ]);
  assert.match(html, /Compile: current recorded stage · ✕ Gate failed/);
  assert.match(html, /Parity: not recorded · ✓ Gate passed/);
  assert.match(html, /ail-phase-active ail-phase-gate-fail/);
  assert.match(html, /1 of 7 phases observed · 1 of 2 recorded stage intervals ended/);
  const warning = view.renderLoop([], { status: 'unknown' }, [{ id: 'compile', status: 'warn' }]);
  assert.match(warning, /Compile: not recorded · ! Gate warning/);
  assert.doesNotMatch(warning, /ail-phase-gate-pass|ail-phase-active/);
});

test('only recorded outstanding instrumented requests establish live request activity', () => {
  const { view } = fixture(() => { throw new Error('unexpected fetch'); });
  const summary = { status: 'running', currentStage: 'COBOL Analysis', startedRequests: 2 };
  const requests = [
    { status: 'in-flight', agent: '<Converter>' },
    { status: 'unknown', agent: 'Not live' }
  ];
  const html = view.renderLoop([], summary, [], requests);
  assert.match(html, /1 instrumented request in flight · &lt;Converter&gt;/);
  assert.doesNotMatch(html, /Not live|<Converter>/);
  assert.match(view.renderLoop([], summary), /may include uninstrumented work/);
  assert.match(view.renderLoop([], { ...summary, startedRequests: null }, [], requests),
    /Request activity not recorded/);
  for (const status of ['completed', 'failed', 'cancelled', 'interrupted', 'unknown', 'no_files']) {
    const ended = view.renderLoop([], { ...summary, status }, [], requests);
    assert.match(ended, new RegExp(`Run ${status}`));
    assert.doesNotMatch(ended, /ail-phase-active|instrumented request in flight/);
  }
});

test('long request histories remain available inside a named keyboard-scrollable region', () => {
  const { view } = fixture(() => { throw new Error('unexpected fetch'); });
  const d = detail('long-run');
  d.summary.startedRequests = 100;
  d.requests = Array.from({ length: 100 }, (_, i) => ({
    requestId: `request-${i}`, agent: 'Converter', context: `program-${i}`, status: 'in-flight'
  }));
  d.contextHandoffs = [{ agent: 'Converter', file: 'A.cbl', state: 'prepared', execution: 'reused' }];
  const html = view.renderDetail(d);
  const history = html.match(/<section class="ail-history"([^>]+)>([\s\S]*?)<\/section>/);
  assert.ok(history);
  assert.match(history[1], /tabindex="0"/);
  assert.match(history[1], /role="region" aria-label="Outstanding requests and context handoffs"/);
  for (const request of d.requests) assert.ok(history[2].includes(`<code>${request.requestId}</code>`));
  assert.match(history[2], /Context handoffs/);
  assert.match(history[2], /A\.cbl/);
  assert.match(history[2], /Prepared does not mean dispatched/);
});

test('motion stops on refresh failure, hidden document, deactivation and terminal attempts', async () => {
  let fail = false, status = 'running';
  const f = fixture(url => {
    if (fail) throw new Error('offline');
    const d = detail('1');
    d.summary.status = status;
    d.summary.currentStage = 'COBOL Analysis';
    d.stages = [{ name: 'COBOL Analysis', durationMs: null }];
    return Promise.resolve(response(isRunList(url) ? list(['1']) : d));
  });
  await f.view.loadAndRender();
  assert.equal(f.motion['data-motion'], 'true');
  fail = true;
  await f.view.loadAndRender();
  assert.equal(f.motion['data-motion'], 'false');
  assert.match(f.root.innerHTML, /displayed activity is stale/);
  assert.match(f.root.innerHTML, /ail-phase-stale/);
  fail = false;
  await f.view.loadAndRender();
  assert.equal(f.motion['data-motion'], 'true');
  f.document.hidden = true;
  f.listeners.get('visibilitychange')();
  assert.equal(f.motion['data-motion'], 'false');
  assert.equal(f.timers.size, 0);
  f.document.hidden = false;
  await f.view.loadAndRender();
  assert.equal(f.motion['data-motion'], 'true');
  f.view.deactivate();
  assert.equal(f.motion['data-motion'], 'false');
  assert.equal(f.timers.size, 0);
  for (status of ['completed', 'failed', 'cancelled', 'interrupted', 'unknown']) {
    await f.view.loadAndRender();
    assert.equal(f.motion['data-motion'], 'false');
  }
});

test('reduced-motion styling disables every live animation without hiding state labels', () => {
  const css = readFileSync(join(__dirname, '../../McpChatWeb/wwwroot/ai-loop.css'), 'utf8');
  const boundary = css.indexOf('@media (prefers-reduced-motion: reduce)');
  const reduced = css.slice(boundary);
  const animatedSelectors = [...css.slice(0, boundary).matchAll(/([^{}]+)\{\s*animation:\s*ail-[^}]+\}/g)]
    .flatMap(match => match[1].split(',').map(selector => selector.trim()));
  const disabledSelectors = [...reduced.matchAll(/([^{}]+)\{\s*animation:\s*none;\s*\}/g)]
    .flatMap(match => match[1].split(',').map(selector => selector.trim()));
  assert.equal(animatedSelectors.length, 3);
  for (const selector of animatedSelectors) {
    assert.ok(disabledSelectors.includes(selector),
      `Reduced motion must override the full selector at equal specificity: ${selector}`);
  }
  assert.doesNotMatch(reduced, /display: none|visibility: hidden/);
});

const missing = () => ({ ok: false, status: 404 });
const managedInfo = (runId, status = 'running', exitCode = null) => ({ info: { runId, status, exitCode } });

test('new launch pins managed identity and does not display a historical run while waiting', async () => {
  let ready = false;
  const f = fixture(url => Promise.resolve(
    isRunList(url) ? response(list(['74']))
      : url === '/api/ai-loop/managed/launch-new' ? ready
        ? response({ ...detail('75'), summary: { ...detail('75').summary, managedRunId: 'launch-new' } })
        : missing()
      : url === '/api/runs/managed/launch-new' ? response(managedInfo('launch-new'))
      : response(detail('74'))));
  await f.view.loadAndRender();
  assert.equal(f.view.detail.summary.runId, '74');

  await f.view.selectManagedRun('launch-new');
  assert.equal(f.view.selected, null);
  assert.equal(f.view.detail, null);
  assert.match(f.root.innerHTML, /Waiting for this launch/);
  assert.doesNotMatch(f.root.innerHTML, /Recorded conversion phases/);
  assert.equal([...f.timers.values()][0].delay, 5000);

  ready = true;
  await [...f.timers.values()][0].callback();
  assert.equal(f.view.selected, '75');
  assert.equal(f.view.detail.summary.managedRunId, 'launch-new');
  assert.equal(f.view.error, null);
});

test('failed-before-telemetry process appears on initial load instead of historical metrics', async () => {
  const f = fixture(url => Promise.resolve(
    isRunList(url) ? response({
      runs: [{ runId: '74', startedAt: '2026-10-06T10:00:00Z', status: 'completed' }],
      managedRuns: [{ runId: 'failed-launch', startedAt: '2026-10-22T10:00:00Z', status: 'failed' }]
    })
      : url === '/api/ai-loop/managed/failed-launch' ? missing()
        : response(managedInfo('failed-launch', 'failed', 1))));
  await f.view.loadAndRender();
  assert.equal(f.view.managedRunId, 'failed-launch');
  assert.equal(f.view.detail, null);
  assert.match(f.root.innerHTML, /process failed/);
  assert.match(f.root.innerHTML, /exit code 1/);
  assert.match(f.root.innerHTML, /ended without a recorded AI Loop attempt/);
  assert.match(f.root.innerHTML, /Historical runs are not evidence/);
  assert.equal(f.calls.some(c => c.url === '/api/ai-loop/74'), false);
});

test('selecting a launch cancels an old numeric response even when fetch ignores abort', async () => {
  const old = deferred();
  const f = fixture(url => isRunList(url) ? Promise.resolve(response(list(['74'])))
    : url === '/api/ai-loop/74' ? old.promise
      : Promise.resolve(url.startsWith('/api/ai-loop/managed/') ? missing() : response(managedInfo('new'))));
  const loading = f.view.loadAndRender();
  await flush();
  await f.view.selectManagedRun('new');
  old.resolve(response(detail('74')));
  await loading;
  assert.equal(f.calls[1].signal.aborted, true);
  assert.equal(f.view.managedRunId, 'new');
  assert.equal(f.view.detail, null);
  assert.match(f.root.innerHTML, /Waiting for this launch/);
  assert.equal(f.timers.size, 1);
});

test('managed resolver identity mismatch is rejected and historical selection releases launch pin', async () => {
  const f = fixture(url => Promise.resolve(response(
    isRunList(url) ? list(['74'])
      : url.startsWith('/api/ai-loop/managed/') ? {
        ...detail('75'), summary: { ...detail('75').summary, managedRunId: 'wrong-launch' }
      } : detail('74'))));
  await f.view.selectManagedRun('wanted');
  assert.equal(f.view.detail, null);
  assert.match(f.view.error, /could not be refreshed/);
  await f.view.select('74');
  assert.equal(f.view.managedRunId, null);
  assert.equal(f.view.detail.summary.runId, '74');
});

test('a process completed before telemetry is not reported as a successful conversion', async () => {
  const f = fixture(url => Promise.resolve(isRunList(url) ? response(list([]))
    : url.startsWith('/api/ai-loop/managed/') ? missing() : response(managedInfo('new', 'completed', 0))));
  await f.view.selectManagedRun('new');
  assert.match(f.root.innerHTML, /process completed/);
  assert.match(f.root.innerHTML, /No model activity or quality-gate result is established/);
  assert.equal(f.view.detail, null);
});

test('lifecycle and successful call event labels do not invent failure', () => {
  const { view } = fixture(() => { throw new Error('unexpected fetch'); });
  const html = view.renderEvents([
    { kind: 'run_started' }, { kind: 'stage', reason: 'Conversion' },
    { kind: 'llm_call', outcome: 'succeeded', reason: 'succeeded' }, { kind: 'run_finished', reason: 'completed' }
  ]);
  assert.match(html, /attempt started/);
  assert.match(html, /attempt finished/);
  assert.match(html, /outcome/);
  assert.match(html, /succeeded/);
  assert.doesNotMatch(html, /failed call/);
});
