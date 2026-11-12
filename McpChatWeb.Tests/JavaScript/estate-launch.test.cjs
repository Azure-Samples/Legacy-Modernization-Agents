const { test } = require('node:test');
const assert = require('node:assert/strict');
const { readFileSync } = require('node:fs');
const { join } = require('node:path');
const vm = require('node:vm');

const source = readFileSync(join(__dirname, '../../McpChatWeb/wwwroot/estate-mission-control.js'), 'utf8');
const readinessSource = readFileSync(join(__dirname, '../../McpChatWeb/wwwroot/conversion-readiness.js'), 'utf8');
const reply = (data, ok = true, status = ok ? 200 : 503) => ({ ok, status, json: async () => data });
const slice = { selectors: ['estate/a.cbl', 'estate/b.cbl'], slice: { programs: ['program:a', 'program:b'] } };
slice.readiness = {
  strictAllowed: true, draftAllowed: true,
  programs: slice.selectors.map(program => ({ program, status: 'eligible', blockers: [] }))
};
const run = { runId: 'managed-new', name: 'C02 conversion', status: 'running', programs: slice.selectors };

// Minimal DOM for the real side-panel renderer, event wiring and async status updates.
class Element {
  constructor(attributes = '') {
    this.attributes = attributes;
    this.dataset = {};
    for (const [, key, value] of attributes.matchAll(/data-([\w-]+)="([^"]*)"/g)) {
      this.dataset[key.replace(/-([a-z])/g, (_, c) => c.toUpperCase())] = value;
    }
    this.id = /id="([^"]+)"/.exec(attributes)?.[1];
    this.disabled = false;
    this.checked = /\bchecked\b/.test(attributes);
    this.value = /value="([^"]*)"/.exec(attributes)?.[1] || '';
    this.children = [];
  }
  set innerHTML(html) {
    this.html = html;
    this.children = [...html.matchAll(/<([a-z]+)\b([^>]*)>/g)]
      .map(([, tag, attributes]) => {
        const element = new Element(attributes);
        if (tag === 'select') element.value = /value="CSharp" selected/.test(html) ? 'CSharp' : 'Java';
        return element;
      });
  }
  get innerHTML() { return this.html || ''; }
  querySelectorAll(selector) {
    return this.children.flatMap(el => [el, ...el.querySelectorAll(selector)]).filter(el => selector.split(',').some(part => {
      const s = part.trim();
      return s.startsWith('#') ? el.id === s.slice(1) : el.attributes.includes(`${s.slice(1, -1)}=`);
    }));
  }
  querySelector(selector) { return this.querySelectorAll(selector)[0] || null; }
}

function fixture(fetcher, { confirm = true, trackingError = false, review = true, styleValues = {} } = {}) {
  const calls = [], selections = [], tabs = [], confirmations = [], reviews = [];
  const side = new Element(), legend = new Element(), graph = new Element(), count = new Element(), shell = new Element();
  const root = {
    querySelector: s => s === '#emc-side' ? side : s === '#emc-legend' ? legend
      : s === '#emc-graph' ? graph : s === '#emc-count' ? count : s === '.emc-shell' ? shell : side.querySelector(s),
    querySelectorAll: s => side.querySelectorAll(s)
  };
  const settings = {
    'mc-language-select': { value: 'CSharp' }, 'mc-speed-select': { value: 'thorough' },
    'mc-provider-select': { value: 'GitHubCopilot' }, 'mc-model-select': { value: 'chosen-model' },
    'mc-dependency-mode': { value: 'strict' }
  };
  const window = {
    switchDashboard: tab => tabs.push(tab),
    aiLoopView: {
      selectManagedRun: async id => {
        selections.push(id);
        if (trackingError) throw new Error('Tracking unavailable');
      }
    }
  };
  const context = vm.createContext({
    window, document: { getElementById: id => settings[id] || root },
    setTimeout, clearTimeout,
    getComputedStyle: () => ({ getPropertyValue: name => styleValues[name] || name }),
    confirm: message => { confirmations.push(message); return confirm; }, navigator: {},
    fetch: async (url, options) => {
      calls.push({ url, options });
      return fetcher(url, options, calls.length);
    }
  });
  vm.runInContext(readinessSource, context);
  window.ConversionReadiness.reviewPrompts = async request => { reviews.push(request); return review; };
  vm.runInContext(source, context);
  const view = new window.EstateMissionControlView('root');
  view.nodes = new Map();
  view.clusters = new Map();
  view.clusterColor = { C02: '#fff' };
  view.renderCluster({ id: 'C02', sliceId: 'C02', label: 'Selected cluster', members: [], programs: 2 });
  const click = selector => side.querySelector(selector).onclick({ preventDefault() {} });
  return { view, side, legend, calls, selections, tabs, click, settings, confirmations, reviews, window };
}

test('estate graph renders before readiness completes and applies rings without rebuilding the graph', async () => {
  let finish;
  const pending = new Promise(resolve => { finish = resolve; });
  const f = fixture(url => url === '/api/runs/readiness' ? pending : reply({ nodes: [], edges: [] }));
  f.window.cytoscape = () => {};
  let draws = 0, updates = 0;
  f.view.index = () => {};
  f.view.renderShell = () => {};
  f.view.draw = () => { draws++; };
  f.view.updateReadinessView = () => { updates++; };
  const loading = f.view.loadAndRender(true);
  await new Promise(resolve => setImmediate(resolve));
  assert.equal(draws, 1);
  assert.equal(updates, 0);
  assert.equal(f.view.readinessLoading, true);
  assert.equal(f.view.dependencyPlans.size, 0);
  finish(reply(slice.readiness));
  await loading;
  assert.equal(draws, 1);
  assert.equal(updates, 1);
  assert.equal(f.view.readinessLoading, false);
  assert.equal(f.view.dependencyPlans.size, 2);
});

test('refreshing readiness clears stale approval immediately and never refetches or relayouts the estate', async () => {
  let finish;
  const pending = new Promise(resolve => { finish = resolve; });
  const f = fixture(() => pending);
  f.view.dependencyPlans.set('A.cbl', { status: 'eligible' });
  let updates = 0;
  f.view.updateReadinessView = () => { updates++; };
  f.view.draw = () => { throw new Error('Unexpected relayout'); };
  const refresh = f.view.refreshReadiness();
  await f.view.refreshReadiness();
  assert.equal(f.view.dependencyPlans.size, 0);
  assert.equal(f.view.readinessLoading, true);
  assert.equal(f.calls.length, 1);
  assert.equal(f.calls[0].url, '/api/runs/readiness');
  finish(reply(slice.readiness));
  await refresh;
  assert.equal(updates, 2);
  assert.equal(f.view.readinessLoading, false);
});

test('returning to a cached graph rechecks evidence and replaces stale warning and approval rings without relayout', async () => {
  let finish;
  const pending = new Promise(resolve => { finish = resolve; });
  const f = fixture(() => pending);
  const programs = [
    { id: 'program:MENU', type: 'program', file: 'bank/MENU.cbl', status: { rekt: true } },
    { id: 'program:EDIT', type: 'program', file: 'bank/EDIT.cbl', status: { rekt: true } }
  ];
  f.view.data = { nodes: programs, edges: [] };
  f.view.index();
  f.view.dependencyPlans.set(programs[0].file, { status: 'blocked' });
  f.view.dependencyPlans.set(programs[1].file, { status: 'eligible' });
  const rings = new Map();
  f.view.cy = {
    resize() {},
    batch: fn => fn(),
    nodes: () => programs.map(program => ({
      id: () => program.id,
      data: (key, value) => { if (key === 'ring') rings.set(program.id, value); }
    }))
  };
  f.view.draw = () => { throw new Error('Unexpected relayout'); };
  const loading = f.view.loadAndRender();
  assert.equal(f.view.dependencyPlans.size, 0);
  assert.equal(rings.get('program:MENU'), '--cp-text-muted');
  assert.equal(rings.get('program:EDIT'), '--cp-text-muted');
  finish(reply({ programs: [
    { program: programs[0].file, status: 'eligible' },
    { program: programs[1].file, status: 'blocked' }
  ] }));
  await loading;
  assert.equal(f.calls.length, 1);
  assert.equal(f.calls[0].url, '/api/runs/readiness');
  assert.equal(rings.get('program:MENU'), '--cp-success');
  assert.equal(rings.get('program:EDIT'), '--cp-warning');
  assert.match(f.legend.innerHTML, /AST available: 2\/2/);
  assert.match(f.legend.innerHTML, /Check passed \(1\)/);
  assert.match(f.legend.innerHTML, /Dependency follow-up \(1\)/);
});

test('failed cached-view reassessment clears previous approval and displays the error instead of reusing it', async () => {
  const f = fixture(() => { throw new Error('Evidence service unavailable'); });
  f.view.data = { nodes: [
    { id: 'program:A', type: 'program', file: 'bank/A.cbl', status: { rekt: true } }
  ], edges: [] };
  f.view.index();
  f.view.dependencyPlans.set('bank/A.cbl', { status: 'eligible' });
  await f.view.loadAndRender();
  assert.equal(f.view.readiness, null);
  assert.equal(f.view.dependencyPlans.size, 0);
  assert.match(f.side.innerHTML, /Evidence service unavailable/);
  assert.match(f.legend.innerHTML, /Check passed \(0\)/);
  assert.match(f.legend.innerHTML, /Not checked \(1\)/);
});

test('automatic layout is bounded while explicit Layout retains the refinement pass', () => {
  const f = fixture(() => reply(slice));
  f.window.__emcFcose = true;
  const options = [];
  f.view.cy = { layout: value => { options.push(value); return { run() {} }; } };
  f.view.layout();
  f.view.layout(true);
  assert.equal(options[0].quality, 'default');
  assert.equal(options[0].numIter, 800);
  assert.equal(options[1].quality, 'proof');
  assert.equal(options[1].numIter, 2500);
});

test('real Cytoscape keeps readiness borders when search, selection or shared-data emphasis is applied', () => {
  const f = fixture(() => reply(slice), { styleValues: { '--cp-accent': 'pink', '--cp-danger': 'red' } });
  let styles;
  f.window.cytoscape = options => {
    styles = options.style;
    return { on() {} };
  };
  f.view.buildElements = () => [];
  f.view.layout = () => {};
  f.view.renderOverview = () => {};
  f.view.draw();
  const cytoscape = require('../../McpChatWeb/wwwroot/lib/cytoscape/cytoscape.min.js');
  const cy = cytoscape({
    headless: true, styleEnabled: true, layout: { name: 'preset' }, style: styles,
    elements: [{ data: {
      id: 'checked', type: 'program', color: 'blue', ring: 'green',
      ringW: 4, size: 30, shape: 'ellipse', label: 'Checked'
    } }]
  });
  try {
    const node = cy.getElementById('checked');
    const border = node.style('border-color'), width = node.style('border-width');
    for (const emphasis of ['hl', 'sel', 'shared', 'hl sel shared']) {
      node.classes(emphasis);
      assert.equal(node.style('border-color'), border, emphasis);
      assert.equal(node.style('border-width'), width, emphasis);
      assert.ok(parseFloat(node.style('overlay-opacity')) > 0, emphasis);
    }
  } finally {
    cy.destroy();
  }
});

test('rendered Send button stages selected scope and POSTs configured target then opens actual managed ID', async () => {
  const f = fixture((url, options) => reply(options ? run : slice));
  f.side.querySelector('#emc-needs').checked = false;
  await f.click('[data-stage-run]');
  assert.equal(f.calls[0].url, '/api/estate/cluster/C02/slice?includeNeeds=false');
  assert.equal(f.calls[1].url, '/api/estate/slice/convert');
  assert.equal(f.calls[1].options.method, 'POST');
  assert.equal(f.reviews.length, 1);
  assert.deepEqual(JSON.parse(f.calls[1].options.body), {
    clusterId: 'C02', includeNeeds: false, targetLanguage: 'CSharp',
    speedProfile: 'thorough', provider: 'GitHubCopilot', modelId: 'chosen-model', dependencyMode: 'strict'
  });
  assert.deepEqual(f.tabs, ['ailoop']);
  assert.deepEqual(f.selections, ['managed-new']);
  assert.match(f.side.querySelector('#emc-stage-out').innerHTML, /running/);
  assert.equal(f.side.querySelector('[data-stage-run]').disabled, false);
  assert.match(f.side.innerHTML, /Agents\/Prompts/);
  assert.match(f.side.innerHTML, /aria-live="polite"/);
});

test('program ring legend is compact, uses all four evidence colors and keeps the edge key collapsed', () => {
  const f = fixture(() => reply(slice));
  f.view.renderLegend();
  assert.match(f.legend.innerHTML, /emc-dependency-key/);
  for (const state of ['missing', 'blocked', 'eligible', 'unknown'])
    assert.match(f.legend.innerHTML, new RegExp(`dep-state-${state}`));
  assert.match(f.legend.innerHTML, /Passed does not mean converted/);
  assert.match(f.legend.innerHTML, /<details class="emc-edge-key">/);
  assert.doesNotMatch(f.legend.innerHTML, /class="dep-readiness"|<details[^>]*open/);
});

test('legend distinguishes AST availability from dependency checks and counts only visible source programs', () => {
  const f = fixture(() => reply(slice));
  f.view.f.estate = 'bank';
  f.view.data = { nodes: [
    { type: 'program', estate: 'bank', file: 'bank/A.cbl', status: { rekt: true, fidelity: 'partial' } },
    { type: 'program', estate: 'bank', file: 'bank/B.cbl', status: { rekt: true } },
    { type: 'program', estate: 'bank', file: 'bank/C.cbl' },
    { type: 'program', estate: 'bank', flags: ['not-in-source'] },
    { type: 'program', estate: 'other', file: 'other/A.cbl', status: { rekt: true } }
  ] };
  f.view.dependencyPlans = new Map([
    ['bank/A.cbl', { status: 'blocked' }],
    ['bank/B.cbl', { status: 'eligible' }],
    ['other/A.cbl', { status: 'eligible' }]
  ]);
  f.view.renderLegend();
  assert.match(f.legend.innerHTML, /AST available: 2\/3 source programs shown/);
  assert.match(f.legend.innerHTML, /Check passed \(1\)/);
  assert.match(f.legend.innerHTML, /Dependency follow-up \(1\)/);
  assert.match(f.legend.innerHTML, /Not checked \(1\)/);
  assert.match(f.legend.innerHTML, /Yellow blocks strict conversion, not necessarily parsing/);
  f.view.f.status = 'parsed';
  f.view.renderLegend();
  assert.match(f.legend.innerHTML, /AST available: 2\/2 source programs shown/);
  assert.match(f.legend.innerHTML, /Not checked \(0\)/);
});

test('estate overview counts input bindings only for the selected estate', () => {
  const f = fixture(() => reply(slice));
  f.view.f.estate = 'bank';
  f.view.data = { nodes: [
    { type: 'program', file: 'bank/A.cbl', estate: 'bank' },
    { type: 'program', file: 'other/B.cbl', estate: 'other' }
  ] };
  f.view.readiness = { programs: [
    { program: 'bank/A.cbl', status: 'eligible', dependencyInputs: { items: [
      { kind: 'COPY', name: 'SHARED', path: '/source/common/SHARED.cpy', status: 'resolved', scope: 'shared' }
    ] } },
    { program: 'other/B.cbl', status: 'blocked', dependencyInputs: { items: [
      { kind: 'COPY', name: 'ABSENT', status: 'missing' }
    ] } }
  ] };
  f.view.renderOverview();
  assert.match(f.side.innerHTML, /1\/1 programs passed individually/);
  assert.match(f.side.innerHTML, /dep-input-shared"><strong>1/);
  assert.match(f.side.innerHTML, /dep-input-missing"><strong>0/);
  f.view.f.estate = 'all';
  f.view.renderOverview();
  assert.match(f.side.innerHTML, /1\/2 programs passed individually/);
  assert.match(f.side.innerHTML, /dep-input-missing"><strong>1/);
});

test('missing, rejected or unavailable dependency evidence never launches or opens prompt review', async () => {
  for (const readiness of [undefined, {
    strictAllowed: false, draftAllowed: false,
    programs: [{ program: 'estate/a.cbl', status: 'blocked', blockers: [{
      raw: 'estate/a.cbl: missing dependency-evidence.json', reason: 'Missing <evidence>', action: 'Parse actual source'
    }] }]
  }]) {
    const f = fixture(() => reply({ ...slice, readiness }));
    await f.click('[data-stage-run]');
    assert.equal(f.calls.length, 1);
    assert.equal(f.reviews.length, 0);
    assert.equal(f.selections.length, 0);
    assert.match(f.side.querySelector('#emc-stage-out').innerHTML, /Conversion will not start/);
    assert.doesNotMatch(f.side.querySelector('#emc-stage-out').innerHTML, /<evidence>/);
  }
});

test('cancelling prompt review prevents launch and releases controls', async () => {
  const f = fixture(() => reply(slice), { review: false });
  await f.click('[data-stage-run]');
  assert.equal(f.calls.length, 1);
  assert.equal(f.reviews.length, 1);
  assert.equal(f.selections.length, 0);
  assert.equal(f.side.querySelector('[data-stage-run]').disabled, false);
});

test('draft slice launch forwards explicit policy and warns that blocked programs produce no code', async () => {
  const f = fixture((url, options) => reply(options ? run : slice));
  f.settings['mc-dependency-mode'].value = 'draft';
  await f.click('[data-stage-run]');
  assert.equal(JSON.parse(f.calls[1].options.body).dependencyMode, 'draft');
  assert.match(f.confirmations[0], /PARTIAL DRAFT — NOT VERIFIED; blocked programs produce no code/);
});

test('cluster rendering identifies missing references without promising generated code', () => {
  const f = fixture(() => reply(slice));
  f.view.renderCluster({
    id: 'C02', sliceId: 'C02', label: 'Selected cluster', members: [], programs: 2,
    missing: ['program:ABSENT', 'program:<UNRESOLVED>']
  });
  assert.match(f.side.innerHTML, /NOT IN SOURCE \(2\) · unresolved dependencies/);
  assert.match(f.side.innerHTML, /program:ABSENT, program:&lt;UNRESOLVED&gt;/);
  assert.doesNotMatch(f.side.innerHTML, /converts as stubs/);
  assert.equal(typeof f.side.querySelector('[data-stage-run]').onclick, 'function');
  assert.equal(f.calls.length, 0);
});

test('pending staging disables both actions and prevents duplicate launches; settings are captured at click', async () => {
  let resolve;
  const pending = new Promise(r => { resolve = r; });
  const f = fixture((url, options) => options ? reply(run) : pending);
  const first = f.click('[data-stage-run]');
  assert.equal(f.side.querySelector('[data-stage-run]').disabled, true);
  assert.equal(f.side.querySelector('[data-stage]').disabled, true);
  await f.click('[data-stage-run]');
  f.settings['mc-model-select'].value = 'changed-later';
  resolve(reply(slice));
  await first;
  assert.equal(f.calls.length, 2);
  assert.equal(JSON.parse(f.calls[1].options.body).modelId, 'chosen-model');
  assert.equal(f.side.querySelector('[data-stage]').disabled, false);
});

test('Stage only, empty slices and cancelled confirmation never POST', async () => {
  for (const mode of ['stage', 'empty', 'cancel']) {
    const f = fixture(() => reply(mode === 'empty' ? { selectors: [] } : slice), { confirm: mode !== 'cancel' });
    await f.click(mode === 'stage' ? '[data-stage]' : '[data-stage-run]');
    assert.equal(f.calls.length, 1, mode);
    assert.equal(f.selections.length, 0, mode);
    assert.equal(f.side.querySelector('[data-stage-run]').disabled, false, mode);
  }
});

test('staging, HTTP, malformed-success and network errors are visible and release controls without wrong-run navigation', async () => {
  const scenarios = [
    (url, options) => options ? reply({ error: '<img src=x onerror=bad>' }, false, 400) : reply(slice),
    () => reply({ error: 'Slice missing' }, false, 404),
    (url, options) => reply(options ? {} : slice),
    (url, options) => { if (options) throw new Error('Network unavailable'); return reply(slice); }
  ];
  for (const fetcher of scenarios) {
    const f = fixture(fetcher);
    await f.click('[data-stage-run]');
    const message = f.side.querySelector('#emc-stage-out').innerHTML;
    assert.match(message, /Could not launch\/stage slice/);
    assert.doesNotMatch(message, /<img/);
    assert.equal(f.side.querySelector('[data-stage-run]').disabled, false);
    assert.equal(f.tabs.length, 0);
    assert.equal(f.selections.length, 0);
  }
});

test('HTTP startup failure with managed ID opens that failed run and retains escaped diagnostic', async () => {
  const f = fixture((url, options) => options
    ? reply({ ...run, status: 'failed', error: 'Launch <failed>' }, false) : reply(slice));
  await f.click('[data-stage-run]');
  assert.deepEqual(f.selections, ['managed-new']);
  const message = f.side.querySelector('#emc-stage-out').innerHTML;
  assert.match(message, /failed/);
  assert.match(message, /Launch &lt;failed&gt;/);
  assert.doesNotMatch(message, /started over/);
});

test('tracking failure preserves created run and retry link selects same ID without launching twice', async () => {
  const f = fixture((url, options) => reply(options ? run : slice), { trackingError: true });
  await f.click('[data-stage-run]');
  assert.match(f.side.querySelector('#emc-stage-out').innerHTML, /Run created; tracking could not open/);
  await f.click('#emc-goloop');
  assert.equal(f.calls.length, 2);
  assert.deepEqual(f.selections, ['managed-new', 'managed-new']);
});
