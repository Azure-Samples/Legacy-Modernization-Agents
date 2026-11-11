const { test } = require('node:test');
const assert = require('node:assert/strict');
const { readFileSync, mkdtempSync, writeFileSync, rmSync } = require('node:fs');
const { join } = require('node:path');
const { tmpdir } = require('node:os');
const { execFileSync } = require('node:child_process');
const vm = require('node:vm');

const source = readFileSync(join(__dirname, '../../McpChatWeb/wwwroot/conversion-readiness.js'), 'utf8');
const prompts = [{ id: 'CobolAnalyzer', name: 'COBOL <Analyzer>' }, { id: 'JavaConverter', name: 'Java converter' }];
const reply = (data, ok = true) => ({ ok, status: ok ? 200 : 503, json: async () => data });
function fixture(response = reply(prompts)) {
  let dialog;
  const requests = [], editors = [];
  const window = { openPromptEditor: (...args) => editors.push(args) };
  const document = {
    documentElement: {},
    getElementById: () => null,
    body: { appendChild: element => { dialog = element; } },
    createElement: () => {
      const buttons = new Map(), events = new Map();
      return {
        setAttribute() {},
        set innerHTML(html) {
          this.html = html;
          for (const [, attribute, id] of html.matchAll(/<button[^>]* (data-[\w-]+)(?:="([^"]*)")?/g))
            buttons.set(id || attribute, { dataset: { reviewPrompt: id } });
        },
        get innerHTML() { return this.html; },
        querySelector: selector => buttons.get(selector.slice(1, -1)),
        querySelectorAll: () => prompts.map(p => buttons.get(p.id)).filter(Boolean),
        addEventListener: (name, callback) => events.set(name, callback),
        showModal() { this.open = true; },
        close(value) { this.returnValue = value; this.open = false; events.get('close')(); },
        remove() { this.removed = true; }
      };
    }
  };
  const context = vm.createContext({
    window, document,
    getComputedStyle: () => ({ getPropertyValue: variable => `theme:${variable}` }),
    fetch: async (...args) => { requests.push(args); return typeof response === 'function' ? response(...args) : response; }
  });
  vm.runInContext(source, context);
  return { api: window.ConversionReadiness, requests, editors, context, document, window, get dialog() { return dialog; } };
}
const settle = () => new Promise(resolve => setImmediate(resolve));

test('dependency states distinguish missing evidence, rejected evidence, verified zero dependencies and unknown', () => {
  const { api } = fixture();
  for (const [plan, expected, tone] of [
    [undefined, 'unknown', '--cp-text-muted'],
    [{ status: 'eligible', blockers: [] }, 'eligible', '--cp-success'],
    [{ status: 'blocked', blockers: [{ raw: 'MAIN.cbl: missing dependency-evidence.json' }] }, 'missing', '--cp-danger'],
    [{ status: 'blocked', blockers: [{ reason: 'Source hash changed' }] }, 'blocked', '--cp-warning']
  ]) {
    assert.equal(api.state(plan), expected);
    assert.match(api.badge(plan), new RegExp(`dep-state-${expected}`));
    assert.equal(api.graphColor(plan), `theme:${tone}`);
  }
  assert.equal(api.allows(undefined), false);
  assert.equal(api.allows({ strictAllowed: false, draftAllowed: true }), false);
  assert.equal(api.allows({ strictAllowed: false, draftAllowed: true }, 'draft'), true);
});

test('blocker rendering escapes diagnostics and does not equate parsing or evidence with successful conversion', () => {
  const { api } = fixture();
  const html = api.details({ programs: [{
    program: '<script>', status: 'blocked', blockers: [{ reason: '<img>', missingEvidence: 'dependency-evidence.json', action: '<parse>' }]
  }] });
  assert.match(html, /&lt;img&gt;/);
  assert.match(html, /Required:/);
  assert.match(html, /dep-panel-blocked/);
  assert.match(html, /dep-check-location/);
  assert.match(html, /Parsed structure is not dependency readiness/);
  assert.match(html, /not proof of equivalent/);
  assert.doesNotMatch(html, /<img>|<script>/);
});

test('selected assessment sends exact paths and artifact binding without caching; errors cannot authorise launch', async () => {
  const request = { programs: ['batch/PAY.cbl'], sourceFolder: 'source', rektFactsFolder: 'output/rekt-scans/run/artifacts' };
  const f = fixture(reply({ strictAllowed: true }));
  assert.equal((await f.api.read(request)).strictAllowed, true);
  assert.deepEqual(JSON.parse(f.requests[0][1].body), request);
  assert.equal(f.requests[0][1].cache, 'no-store');
  const failed = fixture(reply({ detail: 'Evidence unavailable' }, false));
  await assert.rejects(failed.api.read(request), /Evidence unavailable/);
});

test('input availability escapes paths and distinguishes shared, rejected, missing and unrecorded inputs', () => {
  const { api } = fixture();
  const html = api.inputs({ dependencyInputs: { items: [
    { kind: 'COPY', name: 'SHARED', status: 'resolved', scope: 'shared', path: '/source/common/<map>.cpy', reason: 'Hash matches' },
    { kind: 'COPY', name: 'HEADER', status: 'rejected', scope: 'local', path: '/source/app/HEADER.cpy', reason: 'Placeholder/stub' },
    { kind: 'COPY', name: 'ABSENT', status: 'missing', scope: 'unknown', reason: 'No matching filename' }
  ] } }, '/source');
  assert.match(html, /3 bindings · 1 shared · 2 need attention/);
  assert.match(html, /Shared source path/);
  assert.match(html, /common\/&lt;map&gt;.cpy/);
  assert.match(html, /Placeholder rejected/);
  assert.ok(html.indexOf('COPY HEADER') < html.indexOf('COPY SHARED'), 'Unusable inputs must appear before resolved ones');
  assert.match(html, /Not found in selected source/);
  assert.doesNotMatch(html, /<map>/);
  assert.match(api.inputs({}), /Input inventory not recorded.*zero dependencies/);
  const recorded = api.inputs({ dependencyInputs: { items: [
    { kind: 'COPY', name: 'SHARED', status: 'recorded', scope: 'shared', path: '/old/source/SHARED.cpy' }
  ] } }, '', true);
  assert.match(recorded, /Present at scan time/);
  assert.match(recorded, /not current filesystem checks/);
});

test('overview deduplicates shared bindings conservatively and separates parser, runtime and unknown inventory', () => {
  const { api } = fixture();
  const copy = { kind: 'COPY', name: 'SHARED', path: '/source/common/SHARED.cpy', status: 'resolved', scope: 'shared' };
  const programs = [
    { dependencyInputs: { items: [copy] }, blockers: [{ diagnosticCategory: 'unsupported' }] },
    { dependencyInputs: { items: [{ ...copy, scope: 'local' }] }, blockers: [{ diagnosticCategory: 'runtime-mapping' }] },
    { blockers: [] }
  ];
  assert.deepEqual(JSON.parse(JSON.stringify(api.inputTotals({ programs }))),
    { present: 1, shared: 1, configured: 0, missing: 0, attention: 0, parser: 1, runtime: 1, unknown: 1 });
  programs[1].dependencyInputs.items[0].status = 'stale';
  for (const order of [programs, [...programs].reverse()]) {
    const totals = api.inputTotals({ programs: order });
    assert.equal(totals.present, 0);
    assert.equal(totals.attention, 1);
  }
  assert.match(api.overview({ programs }), /Programs with parser\/exporter evidence gaps/);
  assert.match(api.overview({ programs }), /Programs needing runtime mapping/);
});

test('prompt reminder lists actual agents, escapes their names and only explicit continuation authorises launch', async () => {
  for (const action of ['cancel', 'continue', 'escape']) {
    const f = fixture();
    const review = f.api.reviewPrompts({ targetLanguage: 'Java' });
    await settle();
    assert.match(f.dialog.innerHTML, /COBOL &lt;Analyzer&gt;/);
    assert.match(f.dialog.innerHTML, /Java converter/);
    assert.match(f.dialog.innerHTML, /Existing templates can be kept/);
    if (action === 'escape') f.dialog.close('');
    else f.dialog.querySelector(`[data-prompt-${action}]`).onclick();
    assert.equal(await review, action === 'continue');
    assert.equal(f.dialog.removed, true);
    assert.deepEqual(f.requests.map(([url]) => url), ['/api/prompts']);
    assert.equal(f.editors.length, 0);
  }
});

test('reviewing an individual agent cancels launch and opens that exact current template without generation', async () => {
  const f = fixture();
  const review = f.api.reviewPrompts({ targetLanguage: 'Java' });
  await settle();
  f.dialog.querySelectorAll('[data-review-prompt]')[1].onclick();
  assert.equal(await review, false);
  assert.deepEqual(f.editors, [['JavaConverter', prompts]]);
  assert.equal(f.requests.length, 1);
});

test('unavailable or empty prompt inventory cannot silently bypass review', async () => {
  for (const response of [reply([], true), reply({}, false)]) {
    const f = fixture(response);
    await assert.rejects(f.api.reviewPrompts({}), /launch cancelled/);
    assert.equal(f.dialog, undefined);
  }
});

test('copied commands always use doctor convert-only with assessed source, evidence, policy and exact identities', () => {
  const { api } = fixture();
  const command = api.command({ sourceFolder: "/repo/it's source", factsFolder: '/repo/output/bound-facts' },
    ['batch/PAY$ONE.cbl', 'online/PAY.cbl'], 'Java');
  assert.match(command, /--source '\/repo\/it'\\''s source'/);
  assert.match(command, /\.\/doctor\.sh convert-only/);
  assert.match(command, /--language 'Java'/);
  assert.doesNotMatch(command, /dotnet|--skip-reverse-engineering/);
  assert.match(command, /REKT_FACTS_FOLDER='\/repo\/output\/bound-facts'/);
  assert.match(command, /CONVERSION_DEPENDENCY_MODE='strict'/);
  assert.match(command, /--programs 'batch\/PAY\$ONE.cbl,online\/PAY.cbl'/);
  assert.throws(() => api.command({}, [], 'Java'), /requires an assessed/);
  const root = mkdtempSync(join(tmpdir(), 'doctor-command-'));
  try {
    writeFileSync(join(root, 'doctor.sh'),
      '#!/bin/sh\nprintf "%s\\n" "$REKT_FACTS_FOLDER" "$CONVERSION_DEPENDENCY_MODE" "$@"\n', { mode: 0o755 });
    const args = execFileSync('bash', ['-c', command], { cwd: root, encoding: 'utf8' }).trim().split('\n');
    assert.deepEqual(args, ['/repo/output/bound-facts', 'strict', 'convert-only',
      '--source', "/repo/it's source", '--language', 'Java', '--programs', 'batch/PAY$ONE.cbl,online/PAY.cbl']);
  } finally {
    rmSync(root, { recursive: true, force: true });
  }
});

for (const [file, className, loadMethod, rowMethod] of [
  ['program-picker.js', 'ProgramPickerView', 'load', 'renderRow'],
  ['program-explorer.js', 'ProgramExplorerView', 'loadList', 'renderList']
]) {
  test(`${className} shows text and color on actual rows and cannot restore stale green after refresh`, async () => {
    let resolveOld, assessments = 0;
    const old = new Promise(resolve => { resolveOld = resolve; });
    const program = { basename: 'MAIN.cbl', relativePath: 'MAIN.cbl', parseFidelity: 'full', linesOfCode: 3 };
    const blocked = {
      programs: [{ program: 'MAIN.cbl', status: 'blocked', blockers: [{ raw: 'MAIN.cbl: missing dependency-evidence.json' }] }],
      strictAllowed: false, draftAllowed: false
    };
    const f = fixture(url => url === '/api/runs/readiness'
      ? (++assessments === 1 ? old : reply(blocked))
      : reply({ programs: [program] }));
    vm.runInContext(readFileSync(join(__dirname, '../../McpChatWeb/wwwroot', file), 'utf8'), f.context);
    const view = new f.window[className]('root');
    view.dependencyPlans.set('MAIN.cbl', { status: 'eligible' });
    const first = view[loadMethod]();
    assert.equal(view.dependencyPlans.size, 0, 'old green is cleared while checking');
    await view[loadMethod]();
    resolveOld(reply({ programs: [{ program: 'MAIN.cbl', status: 'eligible' }], strictAllowed: true }));
    await first;
    const html = rowMethod === 'renderRow' ? view.renderRow(program) : view.renderList();
    assert.match(html, /dep-row-missing/);
    assert.match(html, /Missing dependency evidence - blocked/);
    assert.match(html, /Parse: Full/);
    assert.match(html, /(?:px|pp)-fid" style="color:var\(--cp-text-muted\)"/);
    assert.doesNotMatch(html, /Dependency check passed/);
  });
}

test('Mission Control blocks missing evidence, honours prompt cancellation and prevents duplicate launch while reviewing', async () => {
  for (const mode of ['blocked', 'cancel', 'continue']) {
    const f = fixture(url => reply(url === '/api/runs/readiness'
      ? { programs: [], strictAllowed: mode !== 'blocked' } : { runId: 'managed-one' }));
    const buttons = [{ disabled: false }], alerts = [];
    f.document.addEventListener = () => {};
    f.document.querySelectorAll = () => buttons;
    const panel = {};
    f.document.getElementById = id => id === 'mc-dependency-readiness' ? panel : null;
    Object.assign(f.context, { alert: text => alerts.push(text), console: { error() {} } });
    vm.runInContext(readFileSync(join(__dirname, '../../McpChatWeb/wwwroot/mission-control.js'), 'utf8'), f.context);
    f.context.loadManagedRuns = async () => {};
    f.context.refreshRunLog = async () => {};
    f.context.updateRunControls = () => {};
    let reviews = 0, finishReview;
    f.api.reviewPrompts = async () => {
      reviews++;
      return mode === 'continue' ? new Promise(resolve => { finishReview = resolve; }) : false;
    };
    const launch = f.context.startRunCommand('convert-only');
    await settle();
    if (mode === 'continue') {
      assert.equal(buttons[0].disabled, true);
      await f.context.startRunCommand('convert-only');
      assert.equal(reviews, 1);
      finishReview(true);
    }
    await launch;
    assert.equal(f.requests.filter(([url]) => url === '/api/runs/start').length, mode === 'continue' ? 1 : 0);
    assert.equal(reviews, mode === 'blocked' ? 0 : 1);
    assert.equal(buttons[0].disabled, false);
    assert.equal(alerts.length, mode === 'blocked' ? 1 : 0);
  }
});
