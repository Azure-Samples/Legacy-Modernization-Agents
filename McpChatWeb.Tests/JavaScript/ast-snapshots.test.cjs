const { test } = require('node:test');
const assert = require('node:assert/strict');
const { readFileSync } = require('node:fs');
const { join } = require('node:path');
const vm = require('node:vm');

const source = readFileSync(join(__dirname, '../../McpChatWeb/wwwroot/ast-explorer.js'), 'utf8');
const reply = data => ({ ok: true, json: async () => data, headers: { get: () => 'application/json' } });

function fixture(fetcher, scanMetadata = {}, runtime = {}) {
  const elements = new Map();
  const element = () => ({
    value: '', children: [], textContent: '', innerHTML: '', hidden: false,
    addEventListener() {},
    appendChild(option) {
      this.children.push(option);
      option.remove = () => { this.children = this.children.filter(child => child !== option); };
    },
    querySelectorAll() { return this.children.slice(1); }
  });
  for (const id of ['ast-file-select', 'ast-view-mode', 'ast-graph', 'ast-source-panel',
    'ast-inspector-content', 'ast-stats-bar', 'ast-evidence-note']) elements.set(id, element());
  elements.get('ast-file-select').appendChild(element());
  let selectedRun = 11;
  const calls = [];
  const context = vm.createContext({
    window: {
      getSelectedScanRunId: () => selectedRun,
      getSelectedScanRun: () => ({
        runId: selectedRun, name: `snapshot-${selectedRun}`, evidenceNote: 'Structural evidence only.', ...scanMetadata
      })
    },
    document: { getElementById: id => elements.get(id), createElement: element },
    console: { error() {} }, AbortController, setTimeout, clearTimeout,
    fetch: async (url, options) => { calls.push(url); return fetcher(url, options); },
    ...runtime
  });
  vm.runInContext(source + '\nglobalThis.Explorer = ASTExplorer;', context);
  const view = new context.Explorer('ast-graph');
  return { view, elements, calls, selectRun: id => { selectedRun = id; } };
}

test('structure, raw AST, paragraph and source requests bind the file inventory snapshot', async () => {
  const f = fixture(url => reply(url.includes('/files')
    ? [{ name: 'sample.cbl', runId: 11 }]
    : url.includes('/structure') ? { sections: [], performEdges: [] }
    : url.includes('/source/content') ? { lines: [], file: 'sample.cbl' }
    : { nodes: [], edges: [] }));
  await f.view.loadFileList();
  f.view.currentFile = 'sample.cbl';
  f.view.renderStructureTree = () => {};
  f.view.updateStructureStats = () => {};
  f.view._renderSource = () => {};
  f.view.renderVisNetwork = () => {};
  await f.view.loadView('sample.cbl');
  await f.view.loadFullSource();
  await f.view.loadSourceForRange(1, 2, 'paragraph');
  await f.view.loadParagraphDetail('p', 'paragraph');
  await f.view.loadRawGraph('sample.cbl', 'ast');
  assert.ok(f.calls.some(url => url.includes('/structure')));
  assert.ok(f.calls.some(url => url.includes('/source/content')));
  assert.ok(f.calls.some(url => url.includes('/ast?')));
  assert.ok(f.calls.every(url => url.includes('scanRunId=11')), f.calls.join('\n'));
});

test('changing snapshots reloads the same file and clears previous graph and inspector state', async () => {
  let runId = 11;
  const f = fixture(() => reply([{ name: 'sample.cbl', runId }]));
  const reloads = [];
  f.view.loadView = async name => reloads.push([name, f.view.scanQuery()]);
  await f.view.loadFileList();
  f.elements.get('ast-file-select').value = 'sample.cbl';
  f.view.currentFile = 'sample.cbl';
  f.elements.get('ast-inspector-content').textContent = 'old detail';
  let destroyed = false;
  f.view.network = { destroy() { destroyed = true; } };
  runId = 22;
  f.selectRun(22);
  await f.view.loadFileList();
  assert.deepEqual(reloads, [['sample.cbl', '&scanRunId=22']]);
  assert.equal(destroyed, true);
  assert.notEqual(f.elements.get('ast-inspector-content').textContent, 'old detail');
  assert.match(f.elements.get('ast-evidence-note').innerHTML, /snapshot-22/);
});

test('a late file-list response cannot restore a previously selected snapshot', async () => {
  let finishOld;
  const old = new Promise(resolve => { finishOld = resolve; });
  const f = fixture(url => url.includes('scanRunId=11') ? old : reply([{ name: 'new.cbl', runId: 22 }]));
  const pending = f.view.loadFileList();
  f.selectRun(22);
  await f.view.loadFileList();
  finishOld(reply([{ name: 'old.cbl', runId: 11 }]));
  await pending;
  assert.equal(f.view.fileRuns.get('new.cbl'), 22);
  assert.equal(f.view.fileRuns.has('old.cbl'), false);
});

test('changing snapshots cancels the previous inventory request without clearing the new selection', async () => {
  let firstSignal;
  const f = fixture((url, options) => {
    if (url.includes('scanRunId=22')) return reply([{ name: 'new.cbl', runId: 22 }]);
    firstSignal = options.signal;
    return new Promise((resolve, reject) => firstSignal.addEventListener('abort', () => reject(new Error('aborted'))));
  });
  const previous = f.view.loadFileList();
  f.selectRun(22);
  await f.view.loadFileList();
  await previous;
  assert.equal(firstSignal.aborted, true);
  assert.equal(f.view.fileRuns.get('new.cbl'), 22);
  assert.equal(f.elements.get('ast-file-select').disabled, false);
});

test('a stalled inventory has a visible timeout instead of an indefinite loading message', async () => {
  let expire;
  const f = fixture((url, options) => new Promise((resolve, reject) =>
    options.signal.addEventListener('abort', () => reject(new Error('aborted')))), {},
    { setTimeout: callback => { expire = callback; return 1; }, clearTimeout() {} });
  const loading = f.view.loadFileList();
  expire();
  await loading;
  assert.match(f.elements.get('ast-graph').textContent, /timed out after 15 seconds/);
  assert.equal(f.view.fileRuns.size, 0);
});

test('late structure data is discarded after switching snapshots', async () => {
  let finishOld;
  const old = new Promise(resolve => { finishOld = resolve; });
  const f = fixture(url => url.includes('/structure') ? old : reply([{ name: 'new.cbl', runId: 22 }]));
  f.view.currentFile = 'old.cbl';
  f.view.fileRuns.set('old.cbl', 11);
  let rendered = false;
  f.view.renderStructureTree = () => { rendered = true; };
  const pending = f.view.loadStructure('old.cbl');
  f.selectRun(22);
  await f.view.loadFileList();
  finishOld(reply({ sections: [{ sectionName: 'old section' }] }));
  await pending;
  assert.equal(rendered, false);
  assert.equal(f.view.structureData, null);
});

test('failed inventory refresh removes stale selectable files and graph data', async () => {
  let fail = false;
  const f = fixture(() => fail
    ? { ok: false, status: 503 }
    : reply([{ name: 'old.cbl', runId: 11 }]));
  await f.view.loadFileList();
  fail = true;
  await f.view.loadFileList();
  assert.equal(f.elements.get('ast-file-select').children.length, 1);
  assert.equal(f.view.fileRuns.size, 0);
  assert.equal(f.elements.get('ast-file-select').disabled, true);
  assert.match(f.elements.get('ast-graph').textContent, /Unable to load scan/);
});

test('the shared scan picker presents named snapshots, including a one-program named scan', async () => {
  const dropdown = {
    children: [{}, {}],
    querySelectorAll() { return this.children; },
    appendChild(option) { this.children.push(option); }
  };
  const context = vm.createContext({
    window: {},
    document: {
      addEventListener() {},
      getElementById: () => dropdown,
      createElement: () => ({})
    },
    console,
    fetch: async () => reply({ runs: [
      { runId: 22, fileCount: 1, name: 'snapshot-v2',
        expectedProgramCount: 2, astProgramCount: 1, structuralGaps: ['missing.cbl'] },
      { runId: 11, fileCount: 8 }
    ] })
  });
  const dashboard = readFileSync(join(__dirname, '../../McpChatWeb/wwwroot/dashboard-tabs.js'), 'utf8');
  vm.runInContext(dashboard, context);
  await vm.runInContext('loadScanRuns()', context);
  assert.equal(dropdown.children[2].textContent, 'snapshot-v2 (1 file)');
  assert.equal(context.window.getSelectedScanRunId(), 22);
  assert.equal(context.window.getSelectedScanRun().name, 'snapshot-v2');
  assert.equal(context.window.getSelectedScanRun().expectedProgramCount, 2);
  assert.equal(context.window.getSelectedScanRun().astProgramCount, 1);
  assert.equal(context.window.getSelectedScanRun().structuralGaps[0], 'missing.cbl');
});

test('dependency graph counts and details are hidden on other dashboards', () => {
  const elements = new Map([
    ['graph-info', { style: {} }],
    ['node-details', { style: {} }]
  ]);
  const context = vm.createContext({
    window: { estateMissionControlView: { loadAndRender() {} } },
    document: {
      addEventListener() {},
      querySelectorAll: () => [],
      querySelector: () => null,
      getElementById: id => elements.get(id) || null
    }
  });
  vm.runInContext(readFileSync(
    join(__dirname, '../../McpChatWeb/wwwroot/dashboard-tabs.js'), 'utf8'), context);

  vm.runInContext("switchDashboard('estate')", context);
  assert.equal(elements.get('graph-info').style.display, 'none');
  assert.equal(elements.get('node-details').style.display, 'none');

  vm.runInContext("switchDashboard('dependency')", context);
  assert.equal(elements.get('graph-info').style.display, '');
  assert.equal(elements.get('node-details').style.display, '');
});

test('scan coverage includes programs without AST and lists their names collapsed, scoped to their snapshot', async () => {
  const metadata = {
    expectedProgramCount: 117, astProgramCount: 78,
    structuralGaps: Array.from({ length: 39 }, (_, index) => index === 0 ? '<missing>.cbl' : `missing-${index}.cbl`)
  };
  const f = fixture(() => reply(Array.from({ length: 78 }, (_, index) => ({
    name: `program-${index}.cbl`, runId: 11, diagnostics: []
  }))), metadata);
  await f.view.loadFileList();
  let html = f.elements.get('ast-evidence-note').innerHTML;
  assert.match(html, /78\/117 programs with AST; 39 programs without AST/);
  assert.match(html, /78 COBOL files with recorded AST structure/);
  assert.match(html, /<details><summary>Recorded programs without AST \(39 names\)/);
  assert.match(html, /&lt;missing&gt;\.cbl/);
  assert.doesNotMatch(html, /<missing>|<details open|66\.6|67%/);
  f.selectRun(22);
  f.view.updateEvidenceNote();
  html = f.elements.get('ast-evidence-note').innerHTML;
  assert.match(html, /AST total not recorded\/source program total not recorded programs with AST; missing-AST count not measured for this snapshot/);
  assert.doesNotMatch(html, /missing-1\.cbl|78\/117/);
});

test('legacy absent coverage names missing measurements while recorded zero AST and empty gap lists stay measured', async () => {
  const metadata = { expectedProgramCount: null, astProgramCount: null, structuralGaps: null };
  const f = fixture(() => reply([{ name: 'sample.cbl', runId: 11 }]), metadata);
  await f.view.loadFileList();
  let html = f.elements.get('ast-evidence-note').innerHTML;
  assert.match(html, /AST total not recorded\/source program total not recorded programs with AST; missing-AST count not measured for this snapshot/);
  assert.match(html, /Missing-AST program names are not recorded/);
  assert.doesNotMatch(html, /1\/1 programs/);
  metadata.expectedProgramCount = 1;
  metadata.astProgramCount = 0;
  metadata.structuralGaps = ['sample.cbl'];
  f.view.updateEvidenceNote();
  assert.match(f.elements.get('ast-evidence-note').innerHTML, /0\/1 programs with AST; 1 programs without AST/);
  metadata.astProgramCount = 1;
  metadata.structuralGaps = [];
  f.view.updateEvidenceNote();
  assert.match(f.elements.get('ast-evidence-note').innerHTML, /1\/1 programs with AST; 0 programs without AST/);
  assert.match(f.elements.get('ast-evidence-note').innerHTML, /Recorded programs without AST \(0 names\)/);
  metadata.expectedProgramCount = 0;
  f.view.updateEvidenceNote();
  assert.match(f.elements.get('ast-evidence-note').innerHTML, /1\/0 programs with AST; missing-AST count not measured for this snapshot/);
});

test('evidence starts collapsed with a compact coverage summary while retaining all details', () => {
  const f = fixture(() => reply([]), { expectedProgramCount: 117, astProgramCount: 78 });
  f.view.fileEvidence = new Map([['demo.cbl', { name: 'demo.cbl', runId: 11 }]]);
  f.view.updateEvidenceNote();
  const html = f.elements.get('ast-evidence-note').innerHTML;
  assert.match(html, /<details class="ast-evidence-disclosure"><summary>Evidence &amp; diagnostics/);
  assert.match(html, /78\/117 programs with AST/);
  assert.match(html, /class="ast-evidence-body"/);
  assert.match(html, /Structure and dependency evidence are different/);
  assert.doesNotMatch(html, /<details class="ast-evidence-disclosure" open/);
});

test('selected snapshot draft assessment requests exact binding and never claims routine safety', async () => {
  const f = fixture(url => reply(url.includes('/draft-plan?scanRunId=11') ? {
    isolation: 'Whole programs only; routine impact not measured.',
    programs: [{ program: 'claims/MAIN.cbl', status: 'blocked', blockers: [{
      category: 'coverage-gap', missingEvidence: 'COPY <PAYREC>', reason: 'Shared fields lack layout evidence.',
      action: 'Supply the real PAYREC and regenerate evidence.', routine: null, location: null
    }] }, { program: 'other/PROGRAM.cbl', status: 'blocked', blockers: [] }]
  } : []));
  const out = { textContent: '', innerHTML: '' };
  f.elements.set('ast-draft-assessment', out);
  f.view.currentFile = 'claims/MAIN.cbl';
  await f.view.assessLocalDraft();
  assert.deepEqual(f.calls, ['/api/estate/draft-plan?scanRunId=11']);
  assert.match(out.innerHTML, /Selected-snapshot source only — PARTIAL \/ NOT VERIFIED/);
  assert.match(out.innerHTML, /claims\/MAIN.cbl/);
  assert.match(out.innerHTML, /COPY &lt;PAYREC&gt;/);
  assert.match(out.innerHTML, /Supply the real PAYREC/);
  assert.match(out.innerHTML, /Routine impact cannot be attributed.*no validated usage\/control-flow linkage/);
  assert.match(out.innerHTML, /Location is not linked in this assessment/);
  assert.match(out.innerHTML, /from 2 assessed programs/);
  assert.doesNotMatch(out.innerHTML, /other\/PROGRAM/);
  assert.doesNotMatch(out.innerHTML, /<PAYREC>|safe routines|verified success/);
});

test('failed selected snapshot assessment preserves actionable refusal and never falls back to defaults', async () => {
  const f = fixture(() => ({ ok: false, status: 503, json: async () => { throw new Error('invalid response'); } }));
  const out = { textContent: '', innerHTML: '' };
  f.elements.set('ast-draft-assessment', out);
  await f.view.assessLocalDraft();
  assert.match(out.textContent, /supply readable local source\/dependency artifacts/);
  assert.match(out.textContent, /no eligibility established/);
});

test('snapshot binding refusal shows exact deficiency and corrective action without assessing configured defaults', async () => {
  const f = fixture(() => ({ ok: false, status: 409, json: async () => ({
    missingEvidence: 'Approved binding for selected scan 1792699201',
    reason: 'No default-path assessment was performed.',
    action: 'Use dependency-plan with the selected named scan manifest inputs/source and artifacts.'
  }) }));
  f.view.fileRuns.set('sql.cbl', 1792699201);
  f.view.currentFile = 'sql.cbl';
  const out = { textContent: '', innerHTML: '' };
  f.elements.set('ast-draft-assessment', out);
  await f.view.assessLocalDraft();
  assert.deepEqual(f.calls, ['/api/estate/draft-plan?scanRunId=1792699201']);
  assert.match(out.textContent, /Approved binding for selected scan 1792699201/);
  assert.match(out.textContent, /No default-path assessment was performed/);
  assert.match(out.textContent, /Use dependency-plan/);
  assert.equal(out.innerHTML, '');
});

test('unreadable COPY provider failures are separate from missing COPY files, with exact names and corrective actions', async () => {
  const raw = 'native:unreadable-copybook:SQLCA:Provider "implicit" not installed';
  const f = fixture(() => reply([{
    name: 'sql.cbl', runId: 11, dependencyEvidenceAvailable: true, diagnostics: [raw]
  }]));
  await f.view.loadFileList();
  f.view.currentFile = 'sql.cbl';
  f.view.updateEvidenceNote();
  const html = f.elements.get('ast-evidence-note').innerHTML;
  assert.match(f.elements.get('ast-file-select').children[1].textContent, /1 unreadable COPY names/);
  assert.doesNotMatch(f.elements.get('ast-file-select').children[1].textContent, /missing COPY/);
  assert.match(html, /Unreadable COPY inputs — not confirmed absent files/);
  assert.match(html, /<strong>COPY SQLCA<\/strong>/);
  assert.match(html, /not proof that the real file is absent/);
  assert.match(html, /Provider "implicit" not installed/);
  assert.match(html, /install or configure the required provider/);
  assert.doesNotMatch(html, /Exact recorded missing inputs/);
});

test('missing COPY names and corrective actions are visible without disguising AST availability', async () => {
  const diagnostic = 'native:unresolved-copybook:ABSENT';
  const f = fixture(() => reply([{
    name: 'sample.cbl', runId: 22, diagnostics: [
      diagnostic, 'native:parser-diagnostic:missing copybook:ABSENT: Copybook not found',
      'parser-diagnostic:None:Copybook declaration has more than 8 characters for: CUSTOMER-DATA'
    ], dependencyEvidenceAvailable: true
  }]));
  f.view.loadView = async () => {};
  f.elements.get('ast-file-select').value = 'sample.cbl';
  await f.view.loadFileList();
  assert.match(f.elements.get('ast-file-select').children[1].textContent, /1 missing COPY names/);
  const html = f.elements.get('ast-evidence-note').innerHTML;
  assert.match(html, /COPY ABSENT/);
  assert.match(html, /Location: not recorded/);
  assert.match(html, /Supply the real input, check include\/search paths/);
  assert.match(html, /AST recorded and available for inspection/);
  assert.match(html, /Dependency readiness:<\/strong> Not established/);
  assert.match(html, /Missing COPY inputs: 2; Parser diagnostics: 1/);
  assert.match(html, /AST node counts do not prove percentage completeness/);
  assert.equal(f.elements.get('ast-evidence-note').title, '');
});

test('overview distinguishes clean reports, absent reports, parser diagnostics and unsupported coverage', async () => {
  const f = fixture(() => reply([
    { name: 'clean.cbl', runId: 22, dependencyEvidenceAvailable: true, diagnostics: [] },
    { name: 'unknown.cbl', runId: 22, dependencyEvidenceAvailable: false, diagnostics: [] },
    { name: 'parser.cbl', runId: 22, dependencyEvidenceAvailable: true,
      diagnostics: ['parser-diagnostic:syntax:line 12 unexpected token', 'unsupported-file-io-dependency-contexts:OPEN'] },
    { name: 'job.jcl', kind: 'jcl', runId: 22 }
  ]));
  await f.view.loadFileList();
  let html = f.elements.get('ast-evidence-note').innerHTML;
  assert.match(html, /3 COBOL files with recorded AST structure; 1 with recorded diagnostics; 1 with a dependency report and no recorded diagnostics; 1 without/);
  assert.match(html, /not all missing dependencies or parser errors/);
  assert.doesNotMatch(f.elements.get('ast-file-select').children.map(child => child.textContent).join(), /dependency gaps|missing COPY/);
  f.view.currentFile = 'parser.cbl';
  f.view.updateEvidenceNote();
  html = f.elements.get('ast-evidence-note').innerHTML;
  assert.match(html, /Parser diagnostics: 1; Unsupported coverage: 1/);
  assert.match(html, /line 12 unexpected token/);
  assert.doesNotMatch(html, /Exact recorded missing inputs/);
  f.view.currentFile = 'clean.cbl';
  f.view.updateEvidenceNote();
  html = f.elements.get('ast-evidence-note').innerHTML;
  assert.match(html, /No diagnostics recorded in the dependency report; completeness not established/);
  assert.match(html, /No missing COPY names were recorded/);
  assert.doesNotMatch(html, /All \d+ recorded diagnostics|COPY ABSENT/);
  f.view.currentFile = 'unknown.cbl';
  f.view.updateEvidenceNote();
  assert.match(f.elements.get('ast-evidence-note').innerHTML, /Not recorded — dependency report unavailable/);
});

test('all-file diagnostic flags explain shared evidence failures rather than claiming every input is missing', async () => {
  const f = fixture(() => reply(['a.cbl', 'b.cbl'].map(name => ({
    name, runId: 22, dependencyEvidenceAvailable: true,
    diagnostics: ['dependency-export-failed:1', 'native-export-source-unverified:hash mismatch',
      'dependency-export-failed:1']
  }))));
  await f.view.loadFileList();
  const html = f.elements.get('ast-evidence-note').innerHTML;
  assert.match(html, /2 COBOL files with recorded AST structure; 2 with recorded diagnostics; 0 with a dependency report and no recorded diagnostics/);
  assert.match(html, /dependency-export-failed<\/code> \(2 files\)/);
  assert.match(html, /native-export-source-unverified<\/code> \(2 files\)/);
  assert.doesNotMatch(html, /missing COPY names/);
});

test('structured backend details preserve exact missing names and locations while escaping diagnostic content', async () => {
  const raw = 'native:unresolved-copybook:<img src=x>';
  const detail = {
    category: 'missing-copybook', code: 'unresolved-copybook', item: '<img src=x>',
    location: 'orders.cbl:42', reason: 'COPY input could not be resolved.',
    action: 'Supply the real copybook.', raw
  };
  const f = fixture(() => reply([{
    name: 'orders.cbl', runId: 22, dependencyEvidenceAvailable: true,
    diagnostics: [raw, raw], evidence: { diagnostics: [detail, detail] }
  }]));
  await f.view.loadFileList();
  f.view.currentFile = 'orders.cbl';
  f.view.updateEvidenceNote();
  const html = f.elements.get('ast-evidence-note').innerHTML;
  assert.match(f.elements.get('ast-file-select').children[1].textContent, /1 missing COPY names/);
  assert.match(html, /Location: orders.cbl:42/);
  assert.match(html, /&lt;img src=x&gt;/);
  assert.doesNotMatch(html, /<img/);
  assert.match(html, /All 2 recorded diagnostics/);
  assert.equal((html.match(/<strong>COPY &lt;img src=x&gt;<\/strong>/g) || []).length, 1);
});

test('structure overview and status share unique paragraph counts and distinguish containers from sections', () => {
  const f = fixture(() => reply({}));
  const paragraph = {
    sectionId: 'section', sectionName: 'MAIN', sectionType: 'SECTION',
    paraId: 'p', paraName: 'A010', paraType: 'PARAGRAPH', stmtCount: null
  };
  f.view.structureData = {
    statementCountsAvailable: false,
    sections: [paragraph, { ...paragraph }, {
      ...paragraph, paraId: 'name', paraType: 'PARAGRAPH_NAME'
    }, {
      ...paragraph, sectionId: 'unsectioned', sectionType: 'PARAGRAPHS',
      paraId: 'loose', paraName: 'LOOSE'
    }]
  };
  f.view.expandedSections.add('section');
  f.view.renderStructureTree(f.elements.get('ast-graph'));
  f.view.updateStructureStats();
  const tree = f.elements.get('ast-graph').innerHTML;
  const stats = f.elements.get('ast-stats-bar').innerHTML;
  assert.match(tree, /ov-num">1<\/span><span class="ov-label">Sections/);
  assert.match(tree, /ov-num">2<\/span><span class="ov-label">Paragraphs/);
  assert.match(tree, /ov-num">Unavailable<\/span><span class="ov-label">Statements/);
  assert.match(tree, /Unsectioned paragraphs/);
  assert.equal((tree.match(/data-para-id="p"/g) || []).length, 1);
  assert.doesNotMatch(tree, /data-para-id="name"|Data processing/);
  assert.match(stats, /1 sections/);
  assert.match(stats, /2 paragraphs/);
  assert.match(stats, /Statements unavailable/);
  assert.doesNotMatch(stats, /0 statements/);
});

test('a measured zero is distinct from unavailable statement counts', () => {
  const f = fixture(() => reply({}));
  f.view.structureData = { statementCountsAvailable: true, sections: [{
    sectionId: 's', sectionType: 'SECTION', paraId: 'p', paraType: 'PARAGRAPH',
    stmtCount: 0
  }] };
  f.view.updateStructureStats();
  assert.match(f.elements.get('ast-stats-bar').innerHTML, /0 statements/);
  f.view.structureData.sections[0].stmtCount = null;
  f.view.updateStructureStats();
  assert.match(f.elements.get('ast-stats-bar').innerHTML, /Statements unavailable/);
  f.view.structureData.sections[0].stmtCount = 0;
  delete f.view.structureData.statementCountsAvailable;
  f.view.updateStructureStats();
  assert.match(f.elements.get('ast-stats-bar').innerHTML, /Statements unavailable/);
});

test('paragraph inspector follows containment only and does not equate AST detail nodes with statements', async () => {
  const f = fixture(() => reply({
    nodes: [
      { id: 'p', nodeType: 'PARAGRAPH' },
      { id: 'sentence', nodeType: 'SENTENCE' },
      { id: 'dialect', nodeType: 'DIALECT', label: 'SQL' },
      { id: 'wrapper', nodeType: 'DIALECT_CONTAINER', label: 'Container' },
      { id: 'elsewhere', nodeType: 'MOVE', label: 'Other paragraph' }
    ],
    edges: [
      { source: 'p', target: 'sentence', type: 'CONTAINS' },
      { source: 'sentence', target: 'dialect', type: 'CONTAINS' },
      { source: 'dialect', target: 'wrapper', type: 'CONTAINS' },
      { source: 'dialect', target: 'elsewhere', type: 'FOLLOWED_BY' },
      { source: 'p', target: 'elsewhere', type: 'JUMPS_TO' }
    ]
  }));
  f.view.currentFile = 'sample.cbl';
  await f.view.loadParagraphDetail('p', 'A010');
  const html = f.elements.get('ast-inspector-content').innerHTML;
  assert.match(html, /2 AST detail nodes \(not a statement count\)/);
  assert.doesNotMatch(html, /Other paragraph|2 statements/);
});

test('raw inspector does not present generic embedded dialect nodes as SQL', () => {
  const f = fixture(() => reply({}));
  for (const [nodeType, originalText, heading] of [
    ['DIALECT', 'EXEC CICS ASSIGN APPLID(WS-APPLID) END-EXEC', 'EMBEDDED'],
    ['DIALECT', 'EXEC SQL SELECT CUSTOMER_ID FROM CUSTOMER END-EXEC', 'EMBEDDED'],
    ['DIALECT_CONTAINER', 'EXEC CICS RETURN END-EXEC', 'EMBEDDED CONTAINER'],
    ['SQL', 'SELECT CUSTOMER_ID FROM CUSTOMER', 'SQL']
  ]) {
    f.view._updateRawInspector({ id: 'embedded', nodeType, originalText });
    const html = f.elements.get('ast-inspector-content').innerHTML;
    assert.match(html, new RegExp(`class="inspector-header"[^>]*>${heading}</div>`));
    assert.ok(html.includes(originalText), 'The native text must remain visible without reinterpretation.');
    if (nodeType !== 'SQL') assert.doesNotMatch(html, /class="inspector-header"[^>]*>SQL<\/div>/);
  }
});
