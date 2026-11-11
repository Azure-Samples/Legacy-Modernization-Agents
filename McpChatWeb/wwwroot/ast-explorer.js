// ═══════════════════════════════════════════════════════════════════════
// Program Structure Explorer — Developer-friendly COBOL program viewer
// Replaces raw AST dump with: collapsed sections → paragraphs → drill-down
// Split view: structure graph (left) + source code (right)
// ═══════════════════════════════════════════════════════════════════════

class ASTExplorer {
  constructor(containerId) {
    this.containerId = containerId;
    this.network = null;
    this.currentFile = null;
    this.viewMode = 'structure'; // 'structure' | 'ast' | 'cfg'
    this.structureData = null;
    this.expandedSections = new Set();
    this.selectedNodeId = null;
    this.viewRevision = 0;
    this.fileRuns = new Map();
    this.fileListController = null;

    const fileSelect = document.getElementById('ast-file-select');
    if (fileSelect) {
      fileSelect.addEventListener('change', (e) => {
        this.currentFile = e.target.value;
        this.updateEvidenceNote();
        if (this.currentFile) this.loadView(this.currentFile);
        else { ++this.viewRevision; this.clearView('Select a file from this scan.'); }
      });
    }

    const modeSelect = document.getElementById('ast-view-mode');
    if (modeSelect) {
      modeSelect.addEventListener('change', (e) => {
        this.viewMode = e.target.value;
        if (this.currentFile) this.loadView(this.currentFile);
      });
    }
  }

  async loadFileList() {
    const select = document.getElementById('ast-file-select');
    if (!select) return;
    const revision = ++this.viewRevision;
    this.fileListController?.abort();
    const controller = new AbortController();
    this.fileListController = controller;
    let timedOut = false;
    const timeout = setTimeout(() => { timedOut = true; controller.abort(); }, 15000);
    select.disabled = true;
    this.clearView('Loading scan files...');
    const previousNote = document.getElementById('ast-evidence-note');
    if (previousNote) previousNote.hidden = true;
    try {
      let files = [];
      const selectedRun = window.getSelectedScanRunId?.();
      const scanParam = selectedRun && selectedRun !== 'all'
        ? `?scanRunId=${encodeURIComponent(selectedRun)}` : '';
      const rektResp = await fetch('/api/graph/rekt/files' + scanParam, { signal: controller.signal });
      if (!rektResp.ok) throw new Error(`Scan file list failed (${rektResp.status})`);
      files = await rektResp.json();
      if (revision !== this.viewRevision) return;

      const current = select.value;
      this.fileKinds = new Map(files.map(f => [f.name, f.kind || 'cobol']));
      this.fileRuns = new Map(files.map(f => [f.name, f.runId]));
      this.fileEvidence = new Map(files.map(f => [f.name, f]));
      select.querySelectorAll('option:not(:first-child)').forEach(o => o.remove());
      for (const f of files) {
        const opt = document.createElement('option');
        opt.value = f.name;
        opt.textContent = f.kind === 'jcl'
          ? `JCL · ${f.job || f.name.replace(/.*\//, '')}`
          : f.name.replace('flow-ast-', '') + this.evidenceLabel(f);
        select.appendChild(opt);
      }
      this.currentFile = files.some(file => file.name === current) ? current : null;
      select.value = this.currentFile || '';
      select.disabled = false;
      this.structureData = null;
      this.expandedSections.clear();
      this.selectedNodeId = null;
      this.updateEvidenceNote();
      this.clearView(files.length ? 'Select a file from this scan.' : 'No AST files in this scan.');
      if (this.currentFile) await this.loadView(this.currentFile);
    } catch (e) {
      if (revision !== this.viewRevision) return;
      console.error('Explorer: file list error', e);
      this.currentFile = null;
      this.fileRuns.clear();
      this.fileEvidence?.clear();
      this.structureData = null;
      select.querySelectorAll('option:not(:first-child)').forEach(option => option.remove());
      select.value = '';
      const note = document.getElementById('ast-evidence-note');
      if (note) note.hidden = true;
      this.clearView(timedOut ? 'Scan file listing timed out after 15 seconds. Check the graph connection and retry.'
        : `Unable to load scan: ${e.message}`);
    } finally {
      clearTimeout(timeout);
      if (this.fileListController === controller) this.fileListController = null;
    }
  }

  scanQuery() {
    const runId = this.fileRuns.get(this.currentFile);
    return runId == null ? '' : `&scanRunId=${encodeURIComponent(runId)}`;
  }

  updateEvidenceNote() {
    const note = document.getElementById('ast-evidence-note');
    if (!note) return;
    const scan = window.getSelectedScanRun?.();
    const evidence = this.fileEvidence?.get(this.currentFile);
    const files = [...(this.fileEvidence?.values() || [])].filter(file => file.kind !== 'jcl');
    const withDiagnostics = files.filter(file => file.diagnostics?.length).length;
    const cleanReports = files.filter(file => file.dependencyEvidenceAvailable === true && !file.diagnostics?.length).length;
    const missingReports = files.filter(file => file.dependencyEvidenceAvailable !== true).length;
    const esc = value => this._escHtml(String(value));
    let html = `<p><strong>${esc(scan?.name || 'Selected scan')} — Structure and dependency evidence are different.</strong></p>`;
    const sameSnapshot = scan?.runId != null
      && files.every(file => file.runId != null && String(file.runId) === String(scan.runId));
    const count = value => Number.isSafeInteger(value) && value >= 0 ? value : null;
    const expected = sameSnapshot ? count(scan.expectedProgramCount) : null;
    const astTotal = sameSnapshot ? count(scan.astProgramCount) : null;
    const missingAst = expected != null && astTotal != null && expected >= astTotal ? expected - astTotal : null;
    html += `<p><strong>Recorded scan coverage:</strong> ${astTotal ?? 'AST total not recorded'}/${expected ?? 'source program total not recorded'} programs with AST; `
      + `${missingAst ?? 'missing-AST count not measured for this snapshot'} programs without AST. `
      + 'These are program counts, not a percentage of AST completeness. The file picker lists AST-bearing files only.</p>';
    if (missingAst == null) {
      html += '<p class="evidence-limit">Missing evidence: consistent <code>expectedProgramCount</code>, '
        + '<code>astProgramCount</code> and source inventory for this exact snapshot. '
        + 'Regenerate the inventory/scan manifest and ingest a new named snapshot; do not infer affected-program counts from visible AST files.</p>';
    }
    if (sameSnapshot && Array.isArray(scan.structuralGaps)) {
      html += `<details><summary>Recorded programs without AST (${scan.structuralGaps.length} names)</summary>`
        + (scan.structuralGaps.length ? `<ul>${scan.structuralGaps.map(name => `<li><code>${esc(name)}</code></li>`).join('')}</ul>`
          : '<p>No structural gap names recorded in this snapshot.</p>') + '</details>';
    } else {
      html += '<p class="evidence-limit">Missing-AST program names are not recorded for this snapshot.</p>';
    }
    html += '<p><strong>Visible AST inventory:</strong> '
      + `${files.length} COBOL files with recorded AST structure; ${withDiagnostics} with recorded diagnostics; `
      + `${cleanReports} with a dependency report and no recorded diagnostics; ${missingReports} without a recorded dependency report. `
      + `JCL jobs are separate and not included in these counts.</p>`;

    const common = new Map();
    for (const file of files) {
      for (const code of new Set(this.diagnosticDetails(file).map(detail => detail.code))) {
        common.set(code, (common.get(code) || 0) + 1);
      }
    }
    if (common.size) {
      const conditions = [...common.entries()].sort((a, b) => b[1] - a[1] || a[0].localeCompare(b[0]));
      const formatConditions = values => values
        .map(([code, count]) => `<code>${esc(code)}</code> (${count} files)`).join('; ');
      html += `<p><strong>Why files are flagged:</strong> ${formatConditions(conditions.slice(0, 3))}. `
        + 'These are recorded conditions, not all missing dependencies or parser errors.</p>';
      if (conditions.length > 3) html += `<details><summary>Other shared diagnostic conditions</summary><p>${formatConditions(conditions.slice(3))}</p></details>`;
    }

    if (evidence?.kind === 'jcl') {
      html += '<p><strong>Selected job:</strong> JCL graph available; COBOL parser and COPY readiness do not apply to this view.</p>';
    } else if (evidence) {
      const details = this.diagnosticDetails(evidence);
      html += `<p><strong>${esc(evidence.name)}</strong> (snapshot ${esc(evidence.runId ?? 'not recorded')})<br>`
        + '<strong>Structural output:</strong> AST recorded and available for inspection, not a complete-parse guarantee.<br>'
        + `<strong>Dependency readiness:</strong> ${details.length ? 'Not established — recorded issues below.'
          : evidence.dependencyEvidenceAvailable === true ? 'No diagnostics recorded in the dependency report; completeness not established.'
          : 'Not recorded — dependency report unavailable.'}<br>`
        + '<strong>Parser/source health:</strong> No separate parser-success or original-source validation result is recorded in this inventory. '
        + 'Parser diagnostics, when present, are listed separately from missing inputs.</p>';
      html += '<p><strong>Draft conversion is opt-in, partial and not verified.</strong> '
        + `${esc(evidence.evidence?.missingDraftEvidence || 'Current source/export/definition hashes, provenance and dependency-domain coverage have not been validated by this graph projection.')} `
        + `${esc(evidence.evidence?.draftAction || 'Use the local draft plan to assess whole-program isolation. Missing COPY can affect shared fields and control flow; no routine is declared safe without deterministic usage evidence.')} `
        + 'The graph snapshot alone never authorizes conversion.</p>';
      html += '<button type="button" id="ast-assess-draft">Assess selected snapshot eligibility (no model calls)</button>'
        + '<div id="ast-draft-assessment" role="status"></div>';
      const uniqueInputs = categories => [...new Map(details
        .filter(detail => categories.includes(detail.category))
        .map(detail => [JSON.stringify([detail.category, detail.item, detail.location]), detail])).values()];
      const missing = uniqueInputs(['missing-copybook', 'missing-input']);
      if (missing.length) {
        html += '<p><strong>Exact recorded missing inputs</strong> (unique input/location entries, not an affected-code count; all diagnostic records retained below):</p><ul>'
          + missing.map(detail => `<li><strong>${detail.category === 'missing-copybook' ? 'COPY' : 'Source'} `
            + `${esc(detail.item || 'name not recorded')}</strong> — ${esc(detail.reason)} `
            + `Location: ${esc(detail.location || 'not recorded; consult the snapshot report/source')}. `
            + `${esc(detail.action)}</li>`).join('') + '</ul>';
      }
      const unreadable = uniqueInputs(['unreadable-copybook']);
      if (unreadable.length) {
        html += '<p><strong>Unreadable COPY inputs — not confirmed absent files:</strong></p><ul>'
          + unreadable.map(detail => `<li><strong>COPY ${esc(detail.item || 'name not recorded')}</strong> — `
            + `${esc(detail.reason)} ${esc(detail.action)}</li>`).join('') + '</ul>';
      }
      if (details.length) {
        const categories = new Map();
        for (const detail of details) categories.set(detail.category, (categories.get(detail.category) || 0) + 1);
        html += `<details><summary>All ${details.length} recorded diagnostics — ${[...categories.entries()]
          .map(([category, count]) => `${esc(this.diagnosticCategory(category))}: ${count}`).join('; ')}</summary><ul>`
          + details.map(detail => `<li><strong>${esc(this.diagnosticCategory(detail.category))}</strong>: `
            + `<code>${esc(detail.raw)}</code><br>${esc(detail.reason)}<br>`
            + `Location: ${esc(detail.location || 'not separately recorded; any location in the raw message above is preserved')}. `
            + `${esc(detail.action)}</li>`).join('') + '</ul></details>';
      } else if (evidence.dependencyEvidenceAvailable !== true) {
        html += '<p>Regenerate dependency-evidence.json for the exact source inputs and ingest a new named snapshot. '
          + 'Do not substitute another scan’s report or placeholder copybooks.</p>';
      } else {
        html += '<p>No missing COPY names were recorded. Verify coverage, input identities/hashes and absence proofs with the strict dependency gate before conversion.</p>';
      }
    } else {
      html += '<p>Select a file to see exact missing inputs, parser diagnostics, unsupported coverage and corrective actions.</p>';
    }
    html += '<p class="evidence-limit">An AST can be useful for navigation even with a small but blocking missing input. '
      + 'AST node counts do not prove percentage completeness, source health or behavioral correctness. '
      + 'Even a report with no diagnostics is not proof of complete, verified dependencies. Strict conversion evidence gates remain required.</p>';
    if (scan?.evidenceNote) html += `<details><summary>Snapshot evidence note</summary><p>${esc(scan.evidenceNote)}</p></details>`;
    const expanded = note.querySelector?.('.ast-evidence-disclosure')?.open === true;
    const coverage = astTotal != null && expected != null
      ? `${astTotal}/${expected} programs with AST` : `${files.length} AST files visible`;
    note.innerHTML = `<details class="ast-evidence-disclosure"${expanded ? ' open' : ''}>`
      + `<summary>Evidence &amp; diagnostics <span class="ast-evidence-coverage">${esc(coverage)}</span></summary>`
      + `<div class="ast-evidence-body">${html}</div></details>`;
    note.title = '';
    note.hidden = false;
    document.getElementById('ast-assess-draft')?.addEventListener('click', () => this.assessLocalDraft());
  }

  async assessLocalDraft() {
    const out = document.getElementById('ast-draft-assessment');
    if (!out) return;
    const scanRunId = this.fileRuns.get(this.currentFile) || window.getSelectedScanRunId?.() || 'unrecorded';
    out.textContent = 'Requesting exact selected-snapshot evidence binding. No default-path substitution or model calls.';
    const esc = value => this._escHtml(String(value));
    try {
      const response = await fetch(`/api/estate/draft-plan?scanRunId=${encodeURIComponent(scanRunId)}`);
      if (!response.ok) {
        const deficiency = await response.json().catch(() => ({}));
        throw new Error([deficiency.missingEvidence, deficiency.reason, deficiency.action]
          .filter(Boolean).join(' ') || `Selected-snapshot assessment failed (${response.status}); supply readable local source/dependency artifacts through an approved snapshot binding`);
      }
      const plan = await response.json();
      if (document.getElementById('ast-draft-assessment') !== out) return;
      const selectedPrograms = (plan.programs || []).filter(program => program.program === this.currentFile);
      if (!selectedPrograms.length) throw new Error(`The bound plan contains no assessment for ${this.currentFile}; no eligibility or routine impact is established for this file`);
      out.innerHTML = '<p><strong>Selected-snapshot source only — PARTIAL / NOT VERIFIED.</strong> '
        + 'This does not reclassify the historical snapshot or start conversion.</p>'
        + `<p>Showing ${esc(this.currentFile)} from ${esc(plan.programs?.length || 0)} assessed programs.</p>`
        + `<p>${esc(plan.isolation)}</p><ul>`
        + selectedPrograms.map(program => `<li><strong>${esc(program.program)}</strong>: ${esc(program.status)}`
          + (program.blockers?.length ? '<ul>' + program.blockers.map(blocker =>
            `<li>${esc(blocker.category)} — Missing input/evidence: ${esc(blocker.missingEvidence)}. `
            + `Reason: ${esc(blocker.reason)} Action: ${esc(blocker.action)} `
            + (blocker.routine ? `Routine: ${esc(blocker.routine)}. `
              : 'Routine impact cannot be attributed: this whole-program assessment has no validated usage/control-flow linkage. ')
            + (blocker.location ? `Location: ${esc(blocker.location)}. `
              : 'Location is not linked in this assessment; consult the scan’s parser/export diagnostics. ')
            + (blocker.raw ? `<details><summary>Exact recorded evidence</summary><code>${esc(blocker.raw)}</code></details>` : '')
            + '</li>').join('') + '</ul>'
            : ' — current whole-program dependency evidence passed; no behavioral verification asserted.')
          + '</li>').join('') + '</ul>';
    } catch (error) {
      out.textContent = `${error.message}; no eligibility established. No default-path assessment or model call was performed. Retry only with an approved exact snapshot source/artifact binding.`;
    }
  }

  evidenceLabel(file) {
    if (!file.diagnostics?.length) {
      return file.dependencyEvidenceAvailable === true ? ' [no recorded diagnostics]' : ' [evidence not recorded]';
    }
    const details = this.diagnosticDetails(file);
    const copies = new Set(details.filter(detail => detail.category === 'missing-copybook').map(detail => detail.item));
    const unreadable = new Set(details.filter(detail => detail.category === 'unreadable-copybook').map(detail => detail.item));
    const labels = [];
    if (copies.size) labels.push(`${copies.size} missing COPY names`);
    if (unreadable.size) labels.push(`${unreadable.size} unreadable COPY names`);
    return labels.length ? ` [${labels.join('; ')}; inspect evidence]` : ' [diagnostics; inspect evidence]';
  }

  diagnosticCategory(category) {
    return {
      'missing-copybook': 'Missing COPY inputs', 'missing-input': 'Missing source inputs',
      'unreadable-copybook': 'Unreadable COPY inputs / provider configuration',
      'parser': 'Parser diagnostics', 'unsupported': 'Unsupported coverage',
      'not-recorded': 'Incomplete / not recorded evidence', 'unverified-input': 'Unverified inputs / resolution',
      'analysis-failed': 'Analysis / export failures', 'export-failure': 'Analysis / export failures',
      'derived-layout': 'Incomplete derived layout', 'binding-gap': 'Unresolved exporter bindings',
      'missing-declaration': 'Required declaration unresolved', 'runtime-mapping': 'Target-runtime mapping unverified',
      'platform-replacement': 'Platform replacement contract', 'unresolved-semantics': 'Unresolved semantics',
      'other': 'Other diagnostics'
    }[category] || 'Other diagnostics';
  }

  diagnosticDetails(file) {
    if (file.evidence?.diagnostics) return file.evidence.diagnostics;
    // Older portal backends expose strings only. Never turn an unknown diagnostic into a missing input.
    return (file.diagnostics || []).map(raw => {
      const value = raw.replace(/^native:/, '');
      const separator = value.indexOf(':');
      const code = separator < 0 ? value : value.slice(0, separator);
      let item = separator < 0 ? null : value.slice(separator + 1);
      const parserMissingCopy = code === 'parser-diagnostic' && item?.startsWith('missing copybook:');
      if (parserMissingCopy) item = item.slice('missing copybook:'.length).split(':', 1)[0];
      if (code === 'unreadable-copybook') {
        const input = item?.split(':', 1)[0];
        return {
          raw, code, item: input, category: 'unreadable-copybook', location: null,
          reason: 'COPY input could not be read. A provider/configuration or input-access failure is not proof that the real file is absent. Recorded diagnostic: ' + raw,
          action: 'Check input access and install or configure the required provider; regenerate evidence into a new named snapshot.'
        };
      }
      const category = code === 'unresolved-copybook' || parserMissingCopy ? 'missing-copybook'
        : code === 'source-not-found' ? 'missing-input'
        : code === 'parser-diagnostic' ? 'parser'
        : code.startsWith('unsupported-') ? 'unsupported' : 'other';
      return {
        raw, code, item, category, location: null,
        reason: category === 'missing-copybook' ? 'COPY input could not be resolved.'
          : category === 'missing-input' ? 'Required source input was not found.'
          : 'Recorded diagnostic; inspect the exact message.',
        action: ['missing-copybook', 'missing-input'].includes(category)
          ? 'Supply the real input, check include/search paths and rescan into a new snapshot.'
          : 'Inspect dependency-evidence.json / parse-diagnostics.json for this snapshot; correct the recorded issue and regenerate evidence.'
      };
    });
  }

  clearView(message) {
    if (this.network) { this.network.destroy(); this.network = null; }
    const graph = document.getElementById('ast-graph');
    if (graph) graph.textContent = message;
    const source = document.getElementById('ast-source-panel');
    if (source) source.textContent = 'Select a section or paragraph to view source.';
    const inspector = document.getElementById('ast-inspector-content');
    if (inspector) inspector.textContent = 'Select a paragraph to inspect statements.';
    const stats = document.getElementById('ast-stats-bar');
    if (stats) stats.textContent = '';
  }

  async loadView(fileName) {
    ++this.viewRevision;
    // A JCL job has no COBOL structure or CFG; its only view is the job graph.
    if (this.fileKinds?.get(fileName) === 'jcl') {
      await this.loadRawGraph(fileName, 'ast');
    } else if (this.viewMode === 'structure') {
      await this.loadStructure(fileName);
    } else {
      await this.loadRawGraph(fileName);
    }
  }

  // ═══════════════════════════════════════════════════════════════════
  // STRUCTURE VIEW — Collapsed section/paragraph tree with stats
  // ═══════════════════════════════════════════════════════════════════

  async loadStructure(fileName) {
    const graphEl = document.getElementById('ast-graph');
    const sourceEl = document.getElementById('ast-source-panel');
    if (!graphEl) return;
    const revision = this.viewRevision;

    graphEl.innerHTML = '<div class="ast-loading">Loading program structure...</div>';
    if (sourceEl) sourceEl.innerHTML = '<div class="source-placeholder">Select a section or paragraph to view source</div>';

    try {
      const scanParam = this.scanQuery();
      const resp = await fetch(`/api/graph/rekt/structure?file=${encodeURIComponent(fileName)}${scanParam}`);
      if (revision !== this.viewRevision) return;
      if (!resp.ok) {
        graphEl.innerHTML = `<div class="ast-empty">No structure data for ${this._escHtml(fileName.replace('flow-ast-',''))}.<br><code>./doctor.sh rekt-full</code></div>`;
        return;
      }
      const data = await resp.json();
      if (revision !== this.viewRevision) return;
      this.structureData = data;
      this.renderStructureTree(graphEl);
      this.updateStructureStats();
      // Auto-load full source
      this.loadFullSource(fileName.replace('flow-ast-', ''));
    } catch (e) {
      if (revision !== this.viewRevision) return;
      console.error('Structure load error:', e);
      graphEl.innerHTML = `<div class="ast-error">Error: ${e.message}</div>`;
    }
  }

  _structureGroups() {
    const sectionMap = new Map();
    for (const row of this.structureData?.sections || []) {
      const key = row.sectionId || row.sectionName;
      if (!sectionMap.has(key)) {
        sectionMap.set(key, {
          id: row.sectionId, name: row.sectionName || 'UNNAMED',
          type: row.sectionType, startLine: row.secStart, endLine: row.secEnd,
          countsAvailable: this.structureData.statementCountsAvailable === true,
          paragraphs: [], totalStmts: 0, totalSql: 0,
          totalPerforms: 0, totalMoves: 0, totalBranches: 0, totalCalls: 0,
        });
      }
      const sec = sectionMap.get(key);
      if (row.paraId && row.paraType !== 'PARAGRAPH_NAME'
          && !sec.paragraphs.some(para => para.id === row.paraId)) {
        sec.countsAvailable &&= Number.isFinite(row.stmtCount);
        sec.paragraphs.push({
          id: row.paraId, name: row.paraName || 'UNNAMED', type: row.paraType,
          startLine: row.paraStart, endLine: row.paraEnd,
          stmtCount: row.stmtCount, sqlCount: row.sqlCount,
          performCount: row.performCount || 0, moveCount: row.moveCount || 0,
          branchCount: row.branchCount || 0, callCount: row.callCount || 0,
        });
        sec.totalStmts += row.stmtCount || 0;
        sec.totalSql += row.sqlCount || 0;
        sec.totalPerforms += row.performCount || 0;
        sec.totalMoves += row.moveCount || 0;
        sec.totalBranches += row.branchCount || 0;
        sec.totalCalls += row.callCount || 0;
      }
    }
    return sectionMap;
  }

  _structureTotals(sectionMap) {
    const groups = [...sectionMap.values()];
    return {
      sections: groups.filter(sec => sec.type === 'SECTION').length,
      paragraphs: new Set(groups.flatMap(sec => sec.paragraphs.map(para => para.id))).size,
      statements: this.structureData.statementCountsAvailable === true
        && groups.every(sec => sec.countsAvailable)
        ? groups.reduce((sum, sec) => sum + sec.totalStmts, 0) : null,
    };
  }

  renderStructureTree(container) {
    const data = this.structureData;
    if (!data?.sections?.length) {
      container.innerHTML = '<div class="ast-empty">No sections found.</div>';
      return;
    }
    const sectionMap = this._structureGroups();
    const totals = this._structureTotals(sectionMap);

    let html = '<div class="structure-tree">';

    // Program overview card for architects
    const totalSql = [...sectionMap.values()].reduce((s, sec) => s + sec.totalSql, 0);
    const totalCalls = [...sectionMap.values()].reduce((s, sec) => s + sec.totalCalls, 0);
    const progName = (this.currentFile || '').replace('flow-ast-', '').replace('.cbl', '');

    html += `<div class="structure-overview">
      <div class="overview-title">${this._escHtml(progName)}</div>
      <div class="overview-grid">
        <div class="overview-stat"><span class="ov-num">${totals.sections}</span><span class="ov-label">Sections</span></div>
        <div class="overview-stat"><span class="ov-num">${totals.paragraphs}</span><span class="ov-label">Paragraphs</span></div>
        <div class="overview-stat"><span class="ov-num">${totals.statements ?? 'Unavailable'}</span><span class="ov-label">Statements</span></div>
        ${totalSql > 0 ? `<div class="overview-stat sql"><span class="ov-num">${totalSql}</span><span class="ov-label">SQL Queries</span></div>` : ''}
        ${totalCalls > 0 ? `<div class="overview-stat call"><span class="ov-num">${totalCalls}</span><span class="ov-label">CALLs</span></div>` : ''}
      </div>
    </div>`;

    html += '<div class="structure-hint">Click a section to expand — click a paragraph to view source code</div>';

    for (const [key, sec] of sectionMap) {
      const isExpanded = this.expandedSections.has(key);
      const paraCount = sec.paragraphs.length;
      const dominant = this._dominantType(sec);
      // Describe what this section does
      const purpose = !sec.countsAvailable ? 'Statement counts unavailable' : sec.totalSql > 3 ? 'Database operations' : sec.totalCalls > 0 ? 'External calls' : sec.totalPerforms > sec.totalMoves ? 'Orchestration' : 'Data processing';

      html += `
        <div class="structure-section ${isExpanded ? 'expanded' : ''}" data-section-id="${key}">
          <div class="section-header" onclick="astExplorer.toggleSection('${this._escAttr(key)}')">
            <span class="section-chevron">${isExpanded ? '▼' : '▶'}</span>
            <span class="section-icon ${dominant.cssClass}">§</span>
            <span class="section-name">${sec.type === 'PARAGRAPHS' ? 'Unsectioned paragraphs' : this._escHtml(sec.name)}</span>
            <span class="section-purpose">${purpose}</span>
            <span class="section-badge">${paraCount} para</span>
            <span class="section-stats">
              ${sec.totalStmts > 0 ? `<span class="stat-pill">${sec.totalStmts} stmts</span>` : ''}
              ${sec.totalSql > 0 ? `<span class="stat-pill sql">${sec.totalSql} SQL</span>` : ''}
              ${sec.totalPerforms > 0 ? `<span class="stat-pill perform">${sec.totalPerforms} PERFORM</span>` : ''}
              ${sec.totalCalls > 0 ? `<span class="stat-pill call">${sec.totalCalls} CALL</span>` : ''}
            </span>
            ${sec.startLine > 0 ? `<span class="section-line">L${sec.startLine}</span>` : ''}
          </div>
          ${isExpanded ? this._renderParagraphs(sec) : ''}
        </div>`;
    }
    html += '</div>';

    // Perform edges summary
    if (data.performEdges?.length) {
      html += `<div class="perform-edges-summary">
        <h4>PERFORM Call Graph (${data.performEdges.length} calls)</h4>
        <div class="perform-edges-list">`;
      for (const edge of data.performEdges) {
        html += `<div class="perform-edge">
          <span class="pe-from">${this._escHtml(edge.from)}</span>
          <span class="pe-arrow">→</span>
          <span class="pe-to">${this._escHtml(edge.to)}</span>
        </div>`;
      }
      html += '</div></div>';
    }

    container.innerHTML = html;
  }

  _renderParagraphs(section) {
    if (!section.paragraphs.length) return '<div class="no-paragraphs">No paragraphs</div>';
    let html = '<div class="paragraph-list">';
    for (const para of section.paragraphs) {
      const dominant = this._paraDominant(para);
      html += `
        <div class="paragraph-item ${this.selectedNodeId === para.id ? 'selected' : ''}"
             onclick="astExplorer.selectParagraph('${this._escAttr(para.id)}', '${this._escAttr(para.name)}', ${para.startLine}, ${para.endLine})"
             data-para-id="${para.id}">
          <div class="para-header">
            <span class="para-icon ${dominant.cssClass}">${dominant.icon}</span>
            <span class="para-name">${this._escHtml(para.name)}</span>
            ${para.startLine > 0 ? `<span class="para-line">L${para.startLine}</span>` : ''}
          </div>
          <div class="para-stats">
            ${para.stmtCount > 0 ? `<span class="stat-mini">${para.stmtCount} stmts</span>` : ''}
            ${para.sqlCount > 0 ? `<span class="stat-mini sql">${para.sqlCount} SQL</span>` : ''}
            ${para.performCount > 0 ? `<span class="stat-mini perform">${para.performCount} PERFORM</span>` : ''}
            ${para.moveCount > 0 ? `<span class="stat-mini move">${para.moveCount} MOVE</span>` : ''}
            ${para.branchCount > 0 ? `<span class="stat-mini branch">${para.branchCount} IF/EVAL</span>` : ''}
            ${para.callCount > 0 ? `<span class="stat-mini call">${para.callCount} CALL</span>` : ''}
          </div>
        </div>`;
    }
    html += '</div>';
    return html;
  }

  toggleSection(sectionId) {
    if (this.expandedSections.has(sectionId)) this.expandedSections.delete(sectionId);
    else this.expandedSections.add(sectionId);
    const graphEl = document.getElementById('ast-graph');
    if (graphEl) this.renderStructureTree(graphEl);
  }

  async selectParagraph(paraId, paraName, startLine, endLine) {
    this.selectedNodeId = paraId;
    document.querySelectorAll('.paragraph-item').forEach(el => el.classList.remove('selected'));
    const el = document.querySelector(`[data-para-id="${paraId}"]`);
    if (el) el.classList.add('selected');

    await this.loadSourceForRange(startLine, endLine, paraName);
    await this.loadParagraphDetail(paraId, paraName);
  }

  async loadSourceForRange(startLine, endLine, label) {
    const sourceEl = document.getElementById('ast-source-panel');
    if (!sourceEl) return;
    const revision = this.viewRevision;

    if (!startLine || startLine <= 0) {
      // No line info — load full file and try to find by name
      await this.loadFullSource(label, label);
      return;
    }

    try {
      const baseName = (this.currentFile || '').replace('flow-ast-', '');
      const padding = 10;
      const resp = await fetch(`/api/source/content?file=${encodeURIComponent(baseName)}&startLine=${Math.max(1, startLine - padding)}&endLine=${endLine + padding}${this.scanQuery()}`);
      if (revision !== this.viewRevision) return;
      if (!resp.ok) { await this.loadFullSource(label, label); return; }
      const data = await resp.json();
      if (revision !== this.viewRevision) return;
      this._renderSource(data, startLine, endLine, label);
    } catch (e) {
      if (revision !== this.viewRevision) return;
      sourceEl.innerHTML = `<div class="source-error">Error loading source: ${e.message}</div>`;
    }
  }

  async loadFullSource(label, searchTerm) {
    const sourceEl = document.getElementById('ast-source-panel');
    if (!sourceEl) return;
    const revision = this.viewRevision;
    try {
      const baseName = (this.currentFile || '').replace('flow-ast-', '');
      const resp = await fetch(`/api/source/content?file=${encodeURIComponent(baseName)}${this.scanQuery()}`);
      if (revision !== this.viewRevision) return;
      if (!resp.ok) {
        const hint = this.scanQuery()
          ? 'Stored source is unavailable for this scan; re-ingest its snapshot.'
          : 'Place file in source/ directory';
        sourceEl.innerHTML = `<div class="source-placeholder"><span class="source-icon">📁</span><span>Source file not found for <strong>${this._escHtml(baseName)}</strong></span><span class="source-hint">${hint}</span></div>`;
        return;
      }
      const data = await resp.json();
      if (revision !== this.viewRevision) return;

      // If a search term is provided and no line highlights, find the term in source
      let foundLine = -1;
      if (searchTerm && data.lines) {
        const term = searchTerm.toUpperCase().replace(/[^A-Z0-9-]/g, '');
        for (const l of data.lines) {
          if (l.text && l.text.toUpperCase().includes(term)) {
            foundLine = l.lineNumber;
            break;
          }
        }
      }

      this._renderSource(data, foundLine, foundLine, label || baseName);
    } catch (e) {
      if (revision !== this.viewRevision) return;
      sourceEl.innerHTML = `<div class="source-error">Error: ${e.message}</div>`;
    }
  }

  _renderSource(data, highlightStart, highlightEnd, label) {
    const sourceEl = document.getElementById('ast-source-panel');
    if (!sourceEl) return;

    const lines = data.lines || [];
    const linesHtml = lines.map(l => {
      const isHl = highlightStart > 0 && l.lineNumber >= highlightStart && l.lineNumber <= highlightEnd;
      return `<div class="source-line${isHl ? ' highlight' : ''}" data-line="${l.lineNumber}">` +
        `<span class="line-num">${l.lineNumber}</span>` +
        `<span class="line-text">${this._escHtml(l.text)}</span></div>`;
    }).join('');

    sourceEl.innerHTML = `
      <div class="source-header">
        <span class="source-file-name">📄 ${this._escHtml(data.file)} <span class="source-total">(${data.totalLines} lines)</span></span>
        ${label ? `<span class="source-label">${this._escHtml(label)}</span>` : ''}
        <button class="source-fullfile-btn" onclick="astExplorer.loadFullSource('${this._escAttr(data.file)}', null)">Full File</button>
      </div>
      <div class="source-code" id="source-code-scroll"><pre>${linesHtml}</pre></div>`;

    if (highlightStart > 0) {
      requestAnimationFrame(() => {
        const hl = sourceEl.querySelector('.source-line.highlight');
        if (hl) hl.scrollIntoView({ behavior: 'smooth', block: 'center' });
      });
    }
  }

  async loadParagraphDetail(paraId, paraName) {
    const inspectorEl = document.getElementById('ast-inspector-content');
    if (!inspectorEl) return;
    const revision = this.viewRevision;

    try {
      const scanParam = this.scanQuery();
      const resp = await fetch(`/api/graph/rekt/ast?file=${encodeURIComponent(this.currentFile)}${scanParam}`);
      if (revision !== this.viewRevision) return;
      if (!resp.ok) return;
      const astData = await resp.json();
      if (revision !== this.viewRevision) return;

      // Build parent→children map from edges
      const edgeMap = new Map();
      for (const e of astData.edges || []) {
        if (e.type !== 'CONTAINS') continue;
        const from = e.from || e.source;
        const to = e.to || e.target;
        if (!edgeMap.has(from)) edgeMap.set(from, []);
        edgeMap.get(from).push(to);
      }

      // BFS from paraId
      const childIds = new Set();
      const queue = [paraId];
      while (queue.length > 0) {
        const id = queue.shift();
        if (childIds.has(id)) continue;
        childIds.add(id);
        for (const child of (edgeMap.get(id) || [])) queue.push(child);
      }

      const nodeMap = new Map(astData.nodes.map(n => [n.id, n]));
      const skipTypes = new Set(['SENTENCE', 'PARAGRAPH', 'PARAGRAPH_NAME', 'PARAGRAPHS', 'SECTION', 'SECTION_HEADER', 'PROCEDURE_DIVISION_BODY']);
      const statements = [];
      for (const id of childIds) {
        const n = nodeMap.get(id);
        if (n && !skipTypes.has(n.nodeType)) statements.push(n);
      }

      // Type breakdown
      const stmtTypes = {};
      for (const s of statements) {
        const t = this._humanType(s.nodeType);
        stmtTypes[t] = (stmtTypes[t] || 0) + 1;
      }

      let html = `<div class="inspector-header">${this._escHtml(paraName)}</div>`;
      html += `<div class="inspector-summary">${statements.length} AST detail nodes (not a statement count)</div>`;
      html += '<div class="type-breakdown">';
      const total = statements.length || 1;
      for (const [type, count] of Object.entries(stmtTypes).sort((a, b) => b[1] - a[1])) {
        const pct = Math.round((count / total) * 100);
        const color = this._typeColor(type);
        html += `<div class="type-bar-row"><span class="type-label">${type}</span><div class="type-bar"><div class="type-bar-fill" style="width:${pct}%;background:${color}"></div></div><span class="type-count">${count}</span></div>`;
      }
      html += '</div>';

      // Statement list
      html += '<div class="stmt-list">';
      for (const s of statements.slice(0, 40)) {
        const label = this._stmtLabel(s);
        const color = this._typeColor(this._humanType(s.nodeType));
        html += `<div class="stmt-item" onclick="astExplorer.loadSourceForRange(${s.startLine}, ${s.endLine}, '${this._escAttr(label)}')" style="border-left: 3px solid ${color}; cursor:pointer;">
          <span class="stmt-type">${this._humanType(s.nodeType)}</span>
          <span class="stmt-label">${this._escHtml(label)}</span></div>`;
      }
      if (statements.length > 40) html += `<div class="stmt-more">+ ${statements.length - 40} more</div>`;
      html += '</div>';

      inspectorEl.innerHTML = html;
    } catch (e) {
      if (revision !== this.viewRevision) return;
      inspectorEl.innerHTML = `<div class="inspector-error">Error: ${e.message}</div>`;
    }
  }

  updateStructureStats() {
    const statsEl = document.getElementById('ast-stats-bar');
    if (!statsEl || !this.structureData) return;
    const data = this.structureData;
    const sectionMap = this._structureGroups();
    const totals = this._structureTotals(sectionMap);
    const totalSql = [...sectionMap.values()].reduce((sum, sec) => sum + sec.totalSql, 0);
    const performs = data.performEdges?.length || 0;

    statsEl.innerHTML = `
      <span class="ast-stat">${totals.sections} sections</span>
      <span class="ast-stat">${totals.paragraphs} paragraphs</span>
      <span class="ast-stat">${totals.statements === null ? 'Statements unavailable' : `${totals.statements} statements`}</span>
      ${totalSql > 0 ? `<span class="ast-stat sql">${totalSql} SQL</span>` : ''}
      ${performs > 0 ? `<span class="ast-stat perform">${performs} PERFORM calls</span>` : ''}`;
  }

  // ═══════════════════════════════════════════════════════════════════
  // RAW AST/CFG VIEW — vis-network graph (power users)
  // ═══════════════════════════════════════════════════════════════════

  async loadRawGraph(fileName, forcedMode) {
    const container = document.getElementById('ast-graph');
    if (!container) return;
    const revision = this.viewRevision;
    container.innerHTML = '<div class="ast-loading">Loading AST...</div>';
    const sourceEl = document.getElementById('ast-source-panel');
    if (sourceEl) sourceEl.innerHTML = '<div class="source-placeholder">Click a node to view its source code</div>';

    // Whitelist of valid endpoint names — guards against unknown viewModes that
    // would otherwise hit the SPA fallback (HTML 200) and break JSON.parse.
    const validModes = new Set(['ast', 'cfg', 'structure']);
    const mode = forcedMode || (validModes.has(this.viewMode) ? this.viewMode : 'ast');

    try {
      const scanParam = this.scanQuery();
      const endpoint = `/api/graph/rekt/${mode}?file=${encodeURIComponent(fileName)}${scanParam}`;
      const resp = await fetch(endpoint);
      if (revision !== this.viewRevision) return;
      if (!resp.ok) { container.innerHTML = `<div class="ast-empty">No ${mode.toUpperCase()} data for ${this._escHtml(fileName)}</div>`; return; }
      const ct = resp.headers.get('content-type') || '';
      if (!ct.includes('application/json')) {
        container.innerHTML = `<div class="ast-empty">Endpoint ${endpoint} returned non-JSON (${ct || 'unknown'}).</div>`;
        return;
      }
      const graphData = await resp.json();
      if (revision !== this.viewRevision) return;
      this.renderVisNetwork(graphData, container);
    } catch (e) {
      if (revision === this.viewRevision)
        container.innerHTML = `<div class="ast-error">Error: ${e.message}</div>`;
    }
  }

  renderVisNetwork(graphData, container) {
    if (this.network) { this.network.destroy(); this.network = null; }
    container.innerHTML = '';
    if (!graphData.nodes?.length) { container.innerHTML = '<div class="ast-empty">No nodes found.</div>'; return; }

    const typeColors = {
      SECTION: '#8b5cf6', PARAGRAPH: '#10b981', PARAGRAPH_NAME: '#10b981',
      SENTENCE: '#64748b', DIALECT: '#a855f7', DIALECT_CONTAINER: '#a855f7',
      MOVE: '#84cc16', PERFORM: '#06b6d4', IF_BRANCH: '#ec4899', EVALUATE: '#f59e0b',
      EXIT: '#475569', COMPUTE: '#f97316', DISPLAY: '#14b8a6', CALL: '#ef4444',
      PROCEDURE_DIVISION_BODY: '#3b82f6', PARAGRAPHS: '#334155',
      JCL_JOB: '#3b82f6', JCL_STEP: '#10b981', JCL_UTILITY_STEP: '#f59e0b', JCL_TSO_STEP: '#06b6d4',
      JCL_UNRESOLVED_STEP: '#ef4444', JCL_PROGRAM: '#8b5cf6', JCL_UTILITY: '#f97316',
      JCL_DATASET: '#14b8a6', JCL_TEMP_DATASET: '#64748b', JCL_JOB_REF: '#ec4899',
    };
    const jclShapes = {
      JCL_JOB: 'box', JCL_STEP: 'box', JCL_UTILITY_STEP: 'box', JCL_TSO_STEP: 'box', JCL_UNRESOLVED_STEP: 'box',
      JCL_PROGRAM: 'ellipse', JCL_UTILITY: 'ellipse', JCL_DATASET: 'database', JCL_TEMP_DATASET: 'database',
      JCL_JOB_REF: 'diamond',
    };
    const edgeColors = {
      CONTAINS: '#334155', FOLLOWED_BY: '#3b82f6', RUNS: '#8b5cf6', READS: '#14b8a6', CREATES: '#10b981',
      APPENDS: '#84cc16', OPENS_EXCLUSIVE: '#f59e0b', DELETES: '#ef4444', FEEDS: '#ec4899',
    };
    const isJcl = graphData.kind === 'jcl';

    const nodes = new vis.DataSet(graphData.nodes.map(n => {
      // Human-readable label: show name (cleaned) instead of UUID
      const cleanName = isJcl ? (n.label || n.name || '') : (n.name || '').replace(/.*\//, '').replace(/Context\/.*/, '');
      const typeLabel = this._humanType(n.nodeType);
      const displayLabel = cleanName ? `${typeLabel}\n${cleanName}` : typeLabel;

      return {
        id: n.id,
        label: displayLabel,
        title: `Type: ${n.nodeType}\nName: ${n.name || '—'}\nLines: ${n.startLine}–${n.endLine}\nID: ${n.id}`,
        color: { background: typeColors[n.nodeType] || '#64748b', border: '#1e293b', highlight: { background: '#fbbf24', border: '#f59e0b' } },
        font: { color: '#e2e8f0', size: 10, multi: true },
        shape: jclShapes[n.nodeType] || (['SECTION', 'PARAGRAPH'].includes(n.nodeType) ? 'box' : 'dot'),
        size: (jclShapes[n.nodeType] || ['SECTION', 'PARAGRAPH'].includes(n.nodeType)) ? 14 : 6,
        _data: n,
      };
    }));

    const edges = new vis.DataSet((graphData.edges || []).map((e, i) => ({
      id: i, from: e.from || e.source, to: e.to || e.target, arrows: 'to',
      color: { color: edgeColors[e.type] || '#ef4444', opacity: 0.6 },
      width: e.type === 'CONTAINS' ? 1 : 2, dashes: e.type === 'JUMPS_TO' || e.type === 'FEEDS' ? [5, 5] : false,
      label: isJcl && e.type !== 'CONTAINS' ? (e.label ? `${e.type} ${e.label}` : e.type) : undefined,
      font: { color: '#94a3b8', size: 8, strokeWidth: 0 },
    })));

    this.network = new vis.Network(container, { nodes, edges }, {
      nodes: { borderWidth: 1, shadow: false },
      edges: { smooth: { type: 'cubicBezier', roundness: 0.4 } },
      layout: { hierarchical: { enabled: true, direction: isJcl ? 'LR' : 'UD', sortMethod: 'directed', nodeSpacing: isJcl ? 70 : 80, levelSeparation: isJcl ? 220 : 60 } },
      physics: { enabled: false },
      interaction: { hover: true, tooltipDelay: 100, navigationButtons: true, keyboard: true },
    });

    this.network.on('click', (params) => {
      if (params.nodes.length > 0) {
        const nd = nodes.get(params.nodes[0])?._data;
        if (nd) {
          this.loadSourceForRange(nd.startLine, nd.endLine, nd.name || nd.originalText || nd.label);
          this._updateRawInspector(nd);
        }
      }
    });
  }

  _updateRawInspector(nodeData) {
    const content = document.getElementById('ast-inspector-content');
    if (!content || !nodeData) return;

    const cleanName = (nodeData.name || '').replace(/.*\//, '').replace(/Context\/.*/, '') || '—';
    const humanType = this._humanType(nodeData.nodeType);

    let html = `<div class="inspector-header" style="color:${this._typeColor(humanType)}">${humanType}</div>`;
    html += `<div style="color:#e2e8f0; font-weight:600; margin-bottom:8px;">${this._escHtml(cleanName)}</div>`;

    html += '<table class="inspector-table">';
    html += `<tr><td class="inspector-key">Type</td><td class="inspector-val">${nodeData.nodeType}</td></tr>`;
    if (nodeData.startLine > 0) html += `<tr><td class="inspector-key">Lines</td><td class="inspector-val">${nodeData.startLine}–${nodeData.endLine}</td></tr>`;
    if (nodeData.section) html += `<tr><td class="inspector-key">Section</td><td class="inspector-val">${nodeData.section}</td></tr>`;
    if (nodeData.paragraph) html += `<tr><td class="inspector-key">Paragraph</td><td class="inspector-val">${nodeData.paragraph}</td></tr>`;
    html += `<tr><td class="inspector-key">ID</td><td class="inspector-val" style="font-size:10px;word-break:break-all;">${nodeData.id}</td></tr>`;

    if (nodeData.originalText) {
      const text = nodeData.originalText.length > 300 ? nodeData.originalText.substring(0, 300) + '...' : nodeData.originalText;
      html += `<tr><td colspan="2"><pre style="font-size:10px;color:#94a3b8;white-space:pre-wrap;margin-top:6px;max-height:150px;overflow-y:auto;">${this._escHtml(text)}</pre></td></tr>`;
    }
    html += '</table>';

    content.innerHTML = html;
  }

  // ═══════════════════════════════════════════════════════════════════
  // DRILL-THROUGH — Called from dependency graph (Sigma.js / vis-network)
  // ═══════════════════════════════════════════════════════════════════

  drillIntoProgram(fileName) {
    if (typeof switchDashboard === 'function') switchDashboard('ast');
    const select = document.getElementById('ast-file-select');
    if (select) {
      for (const opt of select.options) {
        if (opt.value === fileName || opt.value.includes(fileName.replace('.cbl', '').replace('.cpy', ''))) {
          select.value = opt.value; break;
        }
      }
    }
    this.currentFile = select?.value || fileName;
    // 'raw' is not a real endpoint — the dropdown only has 'structure' and 'ast'.
    // Using 'raw' caused /api/graph/rekt/raw to fall through to the SPA index.html
    // (HTTP 200, text/html), which then crashed JSON.parse with "Unexpected token '<'".
    this.viewMode = 'ast';
    const modeSelect = document.getElementById('ast-view-mode');
    if (modeSelect) modeSelect.value = 'ast';
    this.loadView(this.currentFile);
  }

  // ═══════════════════════════════════════════════════════════════════
  // HELPERS
  // ═══════════════════════════════════════════════════════════════════

  _humanType(nodeType) {
    const map = { DIALECT: 'EMBEDDED', DIALECT_CONTAINER: 'EMBEDDED CONTAINER', MOVE: 'MOVE', PERFORM: 'PERFORM', CALL: 'CALL',
      IF_BRANCH: 'IF', IF_YES: 'THEN', IF_NO: 'ELSE', EVALUATE: 'EVALUATE', COMPUTE: 'COMPUTE', ADD: 'ADD',
      DISPLAY: 'DISPLAY', EXIT: 'EXIT', GENERIC_STATEMENT: 'STMT', SECTION: 'SECTION', PARAGRAPH: 'PARA',
      SECTION_HEADER: 'SECTION', PARAGRAPH_NAME: 'PARA', PARAGRAPHS: 'PARAGRAPHS',
      PROCEDURE_DIVISION_BODY: 'PROCEDURE DIV', SENTENCE: 'SENTENCE', SYMBOL: 'SYMBOL',
      JCL_JOB: 'JOB', JCL_STEP: 'STEP', JCL_UTILITY_STEP: 'UTILITY STEP', JCL_TSO_STEP: 'TSO STEP',
      JCL_UNRESOLVED_STEP: 'PROC (unresolved)', JCL_PROGRAM: 'PROGRAM', JCL_UTILITY: 'UTILITY',
      JCL_DATASET: 'DATASET', JCL_TEMP_DATASET: 'TEMP DATASET', JCL_JOB_REF: 'JOB' };
    return map[nodeType] || nodeType;
  }

  _typeColor(humanType) {
    const colors = { SQL: '#a855f7', EMBEDDED: '#a855f7', 'EMBEDDED CONTAINER': '#a855f7', MOVE: '#84cc16', PERFORM: '#06b6d4', CALL: '#ef4444',
      IF: '#ec4899', THEN: '#ec4899', ELSE: '#94a3b8', EVALUATE: '#f59e0b', COMPUTE: '#f97316',
      ADD: '#eab308', DISPLAY: '#14b8a6', EXIT: '#475569', STMT: '#64748b', SECTION: '#8b5cf6', PARA: '#10b981' };
    return colors[humanType] || '#64748b';
  }

  _dominantType(section) {
    if (section.totalSql > section.totalMoves) return { cssClass: 'dominant-sql', icon: '🗃️' };
    if (section.totalCalls > 0) return { cssClass: 'dominant-call', icon: '📞' };
    if (section.totalPerforms > section.totalMoves) return { cssClass: 'dominant-perform', icon: '🔄' };
    return { cssClass: 'dominant-move', icon: '📋' };
  }

  _paraDominant(para) {
    if (para.sqlCount > para.moveCount) return { cssClass: 'dominant-sql', icon: '🗃️' };
    if (para.callCount > 0) return { cssClass: 'dominant-call', icon: '📞' };
    if (para.branchCount > 0) return { cssClass: 'dominant-branch', icon: '🔀' };
    if (para.performCount > para.moveCount) return { cssClass: 'dominant-perform', icon: '🔄' };
    return { cssClass: 'dominant-move', icon: '📋' };
  }

  _stmtLabel(node) {
    const text = node.originalText || node.name || node.label || '';
    return text.length > 60 ? text.substring(0, 57) + '...' : text || node.nodeType;
  }

  _escHtml(s) { return (s || '').replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;'); }
  // Output lands inside a single-quoted JS string inside an HTML attribute, so it needs both
  // escapings. Backslash must come first: doing it later would re-escape the escapes added
  // below, and leaving it out lets a name ending in a backslash close the string early.
  _escAttr(s) {
    return (s || '')
      .replace(/\\/g, '\\\\')
      .replace(/'/g, "\\'")
      .replace(/"/g, '&quot;')
      .replace(/</g, '&lt;');
  }

  refresh() {
    if (this.currentFile) this.loadView(this.currentFile);
    else this.loadFileList();
  }
}
