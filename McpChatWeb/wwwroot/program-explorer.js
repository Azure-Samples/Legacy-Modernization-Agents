// Program Explorer — the estate from one program's point of view.
//
// The other dashboards answer questions about the estate: how many programs are blocked, which
// copybooks are missing, what the architecture looks like. The question that actually precedes a
// conversion is narrower and is asked one program at a time: what is this program, what does it
// touch, what does the parser actually know about it, and is that enough to convert it.
//
// Answering that meant opening four views and correlating them by hand. Everything shown here is
// already served by the backend — this view does the correlating.

const PX_EMPTY = '—';

class ProgramExplorerView {
  constructor(rootId) {
    this.rootId = rootId;
    this.programs = [];
    this.filter = '';
    this.selected = null;
    this.detail = null;      // /api/modernization/program/{identity}
    this.flow = null;        // /api/modernization/flow/{identity}
    this.loading = false;
    this.error = null;
    this.loaded = false;
    // Open the sections a conversion decision actually rests on; flow and SQL detail are
    // reference material and stay collapsed until asked for.
    this.openSections = new Set(['summary', 'copybooks', 'dependencies', 'convert']);
  }

  get root() { return document.getElementById(this.rootId); }

  // Shared with the Convert Programs tab so a fidelity label means the same thing in both.
  get fidelityMap() {
    return (typeof window !== 'undefined' && window.PP_FIDELITY) || {
      full: { label: 'Full', color: '#10b981', why: '' },
      partial: { label: 'Partial', color: '#f59e0b', why: '' },
      'deps-only': { label: 'Deps only', color: '#38bdf8', why: '' },
      failed: { label: 'Failed', color: '#ef4444', why: '' },
      'not-parsed': { label: 'Not parsed', color: '#64748b', why: '' },
    };
  }

  async loadAndRender() {
    if (!this.loaded) { await this.loadList(); this.loaded = true; }
    this.render();
  }

  async loadList() {
    try {
      const resp = await fetch('/api/modernization/programs');
      if (!resp.ok) { this.error = `Program list unavailable (HTTP ${resp.status}).`; return; }
      const payload = await resp.json();
      this.programs = payload.programs || [];
      this.note = payload.note || null;
    } catch (e) {
      console.error('Program Explorer: failed to load programs', e);
      this.error = 'The program list could not be loaded.';
    }
  }

  async select(identity) {
    this.selected = identity;
    this.detail = null;
    this.flow = null;
    this.loading = true;
    this.render();

    // The flow report is optional: a program can be listed and scanned without one. A missing
    // flow must not blank the rest of the page, so the two are settled independently.
    const [detail, flow] = await Promise.allSettled([
      this.fetchJson(`/api/modernization/program/${encodeURI(identity)}`),
      this.fetchJson(`/api/modernization/flow/${encodeURI(identity)}`),
    ]);

    this.detail = detail.status === 'fulfilled' ? detail.value : null;
    this.flow = flow.status === 'fulfilled' ? flow.value : null;
    this.detailError = detail.status === 'rejected' ? String(detail.reason) : null;
    this.loading = false;
    this.render();
  }

  async fetchJson(url) {
    const resp = await fetch(url);
    if (!resp.ok) throw new Error(`HTTP ${resp.status}`);
    return resp.json();
  }

  toggleSection(name) {
    if (this.openSections.has(name)) this.openSections.delete(name);
    else this.openSections.add(name);
    this.render();
  }

  // ── Rendering ─────────────────────────────────────────────────────────

  render() {
    const root = this.root;
    if (!root) return;

    if (this.error) {
      root.innerHTML = `<div class="px-wrap"><div class="px-empty">${this.esc(this.error)}</div></div>`;
      return;
    }

    root.innerHTML = `
      <div class="px-wrap">
        <div class="px-header">
          <div>
            <h3 class="px-title">Program Explorer</h3>
            <div class="px-sub">
              One program at a time: what the scan found, what it depends on, and whether that is
              enough to convert it.
            </div>
          </div>
          <input class="px-filter" id="px-filter" placeholder="Filter by name or path…"
                 value="${this.esc(this.filter)}" />
        </div>
        ${this.note ? `<div class="px-note">${this.esc(this.note)}</div>` : ''}
        <div class="px-body">
          <div class="px-list-pane">
            <div class="px-list-head">${this.filtered().length} of ${this.programs.length} programs</div>
            <div class="px-list">${this.renderList()}</div>
          </div>
          <div class="px-detail-pane">${this.renderDetail()}</div>
        </div>
      </div>`;

    const input = document.getElementById('px-filter');
    if (input) {
      input.addEventListener('input', e => {
        this.filter = e.target.value;
        const list = root.querySelector('.px-list');
        const head = root.querySelector('.px-list-head');
        if (list) list.innerHTML = this.renderList();
        if (head) head.textContent = `${this.filtered().length} of ${this.programs.length} programs`;
        this.bindRows();
      });
      // Re-focus without losing the caret when the whole panel was rebuilt.
      if (this.filter) { input.focus(); input.setSelectionRange(input.value.length, input.value.length); }
    }

    this.bindRows();
    this.bindDetail();
  }

  bindRows() {
    this.root?.querySelectorAll('.px-row').forEach(row =>
      row.addEventListener('click', () => this.select(row.dataset.identity)));
  }

  bindDetail() {
    this.root?.querySelectorAll('.px-sec-head').forEach(head =>
      head.addEventListener('click', () => this.toggleSection(head.dataset.section)));

    const copy = this.root?.querySelector('#px-copy-cmd');
    if (copy) {
      copy.addEventListener('click', async () => {
        const cmd = copy.dataset.cmd || '';
        try {
          await navigator.clipboard.writeText(cmd);
          copy.textContent = 'Copied';
          setTimeout(() => { copy.textContent = 'Copy'; }, 1500);
        } catch {
          copy.textContent = 'Copy failed';
        }
      });
    }

    this.root?.querySelectorAll('.px-link').forEach(link =>
      link.addEventListener('click', () => {
        const target = this.resolveIdentity(link.dataset.name);
        if (target) this.select(target);
      }));
  }

  /// A dependency is recorded by name; the list holds paths. Only an unambiguous match navigates.
  resolveIdentity(name) {
    if (!name) return null;
    const stem = name.replace(/\.[^.]+$/, '').toLowerCase();
    const matches = this.programs.filter(p =>
      p.basename.replace(/\.[^.]+$/, '').toLowerCase() === stem);
    return matches.length === 1 ? matches[0].relativePath : null;
  }

  filtered() {
    const f = this.filter.trim().toLowerCase();
    if (!f) return this.programs;
    return this.programs.filter(p =>
      (p.basename || '').toLowerCase().includes(f) ||
      (p.relativePath || '').toLowerCase().includes(f));
  }

  renderList() {
    const rows = this.filtered();
    if (rows.length === 0) return `<div class="px-empty">No program matches “${this.esc(this.filter)}”.</div>`;

    return rows.map(p => {
      const fid = this.fidelityMap[p.parseFidelity] || this.fidelityMap['not-parsed'];
      const selected = p.relativePath === this.selected ? ' px-row-selected' : '';
      const missing = p.missingCopybookCount > 0
        ? `<span class="px-warn" title="${p.missingCopybookCount} copybook(s) absent">⚠ ${p.missingCopybookCount}</span>`
        : '';
      return `
        <div class="px-row${selected}" data-identity="${this.esc(p.relativePath)}">
          <div class="px-row-main">
            <span class="px-name">${this.esc(p.basename)}</span>
            <span class="px-path">${this.esc(p.relativePath)}</span>
          </div>
          <div class="px-row-meta">
            <span class="px-loc">${(p.linesOfCode || 0).toLocaleString()} LOC</span>
            ${missing}
            <span class="px-fid" style="color:${fid.color}">${this.esc(fid.label)}</span>
          </div>
        </div>`;
    }).join('');
  }

  renderDetail() {
    if (!this.selected) {
      return `<div class="px-detail-empty">
        Select a program to see what the scan found, what it depends on, and what still blocks it.
      </div>`;
    }
    if (this.loading) return `<div class="px-detail-empty">Loading ${this.esc(this.selected)}…</div>`;
    if (!this.detail) {
      return `<div class="px-detail-empty">
        ${this.esc(this.detailError || 'This program could not be loaded.')}
      </div>`;
    }

    const d = this.detail;
    if (d.ambiguousBasename && (d.candidates || []).length > 0) {
      return `<div class="px-detail-empty">
        <p>${this.esc(d.note || 'That name matches more than one source file.')}</p>
        <div class="px-chips">${d.candidates.map(c =>
          `<span class="px-chip px-link" data-name="${this.esc(c)}">${this.esc(c)}</span>`).join('')}</div>
      </div>`;
    }

    return [
      this.detailHeader(d),
      this.sectionSummary(d),
      this.sectionCopybooks(d),
      this.sectionDependencies(d),
      this.sectionData(d),
      this.sectionFlow(),
      this.sectionConvert(d),
    ].join('');
  }

  detailHeader(d) {
    const fid = this.fidelityMap[d.parseFidelity] || this.fidelityMap['not-parsed'];
    return `
      <div class="px-detail-title">${this.esc(d.basename || d.identity)}</div>
      <div class="px-detail-path">${this.esc(d.relativePath || d.identity)}</div>
      <div class="px-detail-stats">
        <span>${(d.linesOfCode || 0).toLocaleString()} lines</span>
        <span style="color:${fid.color}">${this.esc(fid.label)} parse</span>
        <span>${d.isCopybook ? 'Copybook' : 'Program'}</span>
      </div>`;
  }

  section(name, title, badge, body) {
    const open = this.openSections.has(name);
    return `
      <div class="px-sec">
        <div class="px-sec-head" data-section="${name}">
          <span class="px-caret">${open ? '▾' : '▸'}</span>
          <span class="px-sec-title">${this.esc(title)}</span>
          ${badge ? `<span class="px-sec-badge">${badge}</span>` : ''}
        </div>
        ${open ? `<div class="px-sec-body">${body}</div>` : ''}
      </div>`;
  }

  sectionSummary(d) {
    const fid = this.fidelityMap[d.parseFidelity] || this.fidelityMap['not-parsed'];
    const body = `
      ${fid.why ? `<div class="px-callout" style="border-left-color:${fid.color}">${this.esc(fid.why)}</div>` : ''}
      ${this.kv([
        ['Parse fidelity', `<span style="color:${fid.color}">${this.esc(fid.label)}</span>`],
        ['Evidence', this.esc(this.fidelitySourceLabel(d.fidelitySource))],
        ['Program facts', d.hasFacts
          ? `present · confidence ${this.esc(d.factsConfidence)} · ${this.esc(d.factsWarnings)} warning(s)`
          : 'not extracted for this program'],
        ['Lines of code', (d.linesOfCode || 0).toLocaleString()],
      ])}`;
    return this.section('summary', 'Scan result', '', body);
  }

  fidelitySourceLabel(source) {
    switch (source) {
      case 'scan-cache': return 'recorded by the parser during the scan';
      case 'facts': return 'inferred from the extracted program facts';
      case 'artifacts': return 'inferred from the report files on disk';
      default: return 'no measured outcome; the label is a default';
    }
  }

  sectionCopybooks(d) {
    const used = d.copybooks || [];
    const missing = d.missingCopybooks || [];

    // A copybook that is absent is still one this program COPYs. The parser cannot always list it
    // among the resolved ones — that list comes from a parse that the absence itself degraded — so
    // the two sources are unioned. Reading `copybooks` alone reports "this program COPYs nothing"
    // for a program whose copybooks are precisely the problem.
    const all = [...used];
    for (const m of missing) if (!all.some(u => this.sameName(u, m))) all.push(m);
    const present = all.filter(c => !missing.some(m => this.sameName(m, c)));

    const badge = missing.length > 0
      ? `<span class="px-badge-warn">${missing.length} missing</span>`
      : (all.length > 0 ? `<span class="px-badge-ok">complete</span>` : '');

    const body = `
      ${missing.length > 0 ? `
        <div class="px-callout px-callout-warn">
          ${missing.length} of ${all.length} copybooks are not in the source drop. The parser
          substituted a generated stub for each, so the fields behind them have no known layout
          and a conversion will infer them.
        </div>
        <div class="px-group-title">Missing</div>
        <div class="px-chips">${missing.map(c =>
          `<span class="px-chip px-chip-warn">${this.esc(c)}</span>`).join('')}</div>` : ''}
      <div class="px-group-title">Resolved (${present.length})</div>
      ${present.length
        ? `<div class="px-chips">${present.map(c => `<span class="px-chip">${this.esc(c)}</span>`).join('')}</div>`
        : `<div class="px-none">${all.length > 0
            ? 'None of this program&rsquo;s copybooks were resolved.'
            : 'This program COPYs nothing.'}</div>`}`;

    return this.section('copybooks', 'Copybooks', badge, body);
  }

  sameName(a, b) {
    const strip = s => String(s || '').replace(/\.[^.]+$/, '').toLowerCase();
    return strip(a) === strip(b);
  }

  sectionDependencies(d) {
    const calls = d.calls || [];
    const calledBy = d.calledBy || [];
    const closure = d.callClosure || [];
    const jobs = d.calledByJobs || [];

    const badge = `<span class="px-sec-count">${calls.length} out · ${calledBy.length} in</span>`;

    const body = `
      ${this.linkGroup('Calls', calls, 'This program calls nothing.')}
      ${this.linkGroup('Called by', calledBy, 'Nothing in the estate calls this program.')}
      ${closure.length > 0 ? `
        <div class="px-group-title">Call closure (${closure.length})</div>
        <div class="px-callout">
          Converting this program alone leaves these behind. Convert the closure to keep the
          service whole.
        </div>
        <div class="px-chips">${closure.map(c =>
          `<span class="px-chip px-link" data-name="${this.esc(c)}">${this.esc(c)}</span>`).join('')}</div>` : ''}
      ${jobs.length > 0 ? `
        <div class="px-group-title">Referenced by jobs (${jobs.length})</div>
        <div class="px-chips">${jobs.map(j => `<span class="px-chip">${this.esc(j)}</span>`).join('')}</div>` : ''}`;

    return this.section('dependencies', 'Dependencies', badge, body);
  }

  linkGroup(title, names, emptyText) {
    return `
      <div class="px-group-title">${this.esc(title)} (${names.length})</div>
      ${names.length
        ? `<div class="px-chips">${names.map(n =>
            `<span class="px-chip px-link" data-name="${this.esc(n)}">${this.esc(n)}</span>`).join('')}</div>`
        : `<div class="px-none">${this.esc(emptyText)}</div>`}`;
  }

  sectionData(d) {
    const tables = d.sqlTables || [];
    const statements = this.flow?.sqlStatements || [];

    const badge = tables.length || statements.length
      ? `<span class="px-sec-count">${tables.length} tables · ${statements.length} statements</span>`
      : '';

    const body = `
      <div class="px-group-title">Tables (${tables.length})</div>
      ${tables.length
        ? `<div class="px-chips">${tables.map(t => `<span class="px-chip">${this.esc(t)}</span>`).join('')}</div>`
        : `<div class="px-none">No SQL table access was recorded for this program.</div>`}
      ${statements.length ? `
        <div class="px-group-title">Statements (${statements.length})</div>
        <table class="px-table">
          <thead><tr><th>Operation</th><th>Tables</th></tr></thead>
          <tbody>${statements.slice(0, 40).map(s => `
            <tr><td>${this.esc(s.operation || PX_EMPTY)}</td>
                <td>${this.esc((s.tables || []).join(', ') || PX_EMPTY)}</td></tr>`).join('')}
          </tbody>
        </table>` : ''}`;

    return this.section('data', 'Data and SQL', badge, body);
  }

  sectionFlow() {
    const f = this.flow;
    if (!f) {
      return this.section('flow', 'Control flow', '',
        `<div class="px-none">No flow report was found for this program.</div>`);
    }

    const sections = f.sections || [];
    const edges = f.performEdges || [];
    const badge = `<span class="px-sec-count">${sections.length} sections · ${edges.length} edges</span>`;

    const body = `
      ${this.kv([
        ['Flow AST', f.hasFlowAst ? 'present' : 'absent'],
        ['Control-flow graph', f.hasCfg ? 'present' : 'absent'],
        ['Data structures', f.hasDataStructures ? 'present' : 'absent'],
        ['Report directory', `<code>${this.esc(f.reportDirectory || PX_EMPTY)}</code>`],
      ])}
      ${f.note ? `<div class="px-callout">${this.esc(f.note)}</div>` : ''}
      ${sections.length ? `
        <div class="px-group-title">Sections (${sections.length})</div>
        <div class="px-sections">${sections.slice(0, 60).map(s => `
          <div class="px-flow-sec">
            <span class="px-flow-name">${this.esc(s.name)}</span>
            <span class="px-flow-count">${(s.paragraphs || []).length} paragraphs</span>
          </div>`).join('')}</div>` : ''}
      ${edges.length ? `
        <div class="px-group-title">PERFORM edges (${edges.length})</div>
        <table class="px-table">
          <thead><tr><th>From</th><th>To</th></tr></thead>
          <tbody>${edges.slice(0, 60).map(e => `
            <tr><td>${this.esc(e.from)}</td><td>${this.esc(e.to)}</td></tr>`).join('')}
          </tbody>
        </table>` : ''}`;

    return this.section('flow', 'Control flow', badge, body);
  }

  sectionConvert(d) {
    const language = (window.PP_LANGUAGE || 'Java');
    const identity = d.relativePath || d.identity;
    const stem = (d.basename || '').replace(/\.[^.]+$/, '');
    const closure = (d.callClosure || []).length;

    const cmd = `./doctor.sh convert-only --language ${language} --program ${stem}`
      + (closure > 0 ? ' --include-callees' : '');

    const blocked = (d.missingCopybooks || []).length;

    const body = `
      ${blocked > 0 ? `
        <div class="px-callout px-callout-warn">
          ${blocked} copybook(s) are still absent. Converting now produces code whose record
          layouts are inferred rather than carried across. Supplying them first is the difference
          between a conversion and a guess.
        </div>` : `
        <div class="px-callout px-callout-ok">
          Every copybook this program uses was found. A conversion works from the real layouts.
        </div>`}
      <div class="px-group-title">Convert this program</div>
      <div class="px-cmd-row">
        <code class="px-cmd">${this.esc(cmd)}</code>
        <button class="px-copy" id="px-copy-cmd" data-cmd="${this.esc(cmd)}">Copy</button>
      </div>
      <div class="px-hint">
        ${closure > 0
          ? `<code>--include-callees</code> is added because this program reaches ${closure} other program(s).`
          : 'This program calls nothing, so it converts on its own.'}
        Run it from the repository root. Identity: <code>${this.esc(identity)}</code>.
      </div>`;

    return this.section('convert', 'Modernization', '', body);
  }

  kv(pairs) {
    return `<dl class="px-kv">${pairs.map(([k, v]) =>
      `<dt>${this.esc(k)}</dt><dd>${v}</dd>`).join('')}</dl>`;
  }

  esc(v) {
    return String(v ?? '').replace(/[&<>"']/g, c =>
      ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
  }
}

if (typeof window !== 'undefined') window.ProgramExplorerView = ProgramExplorerView;
