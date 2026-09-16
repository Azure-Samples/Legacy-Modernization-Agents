// Program Picker — choose what to convert, from a program's point of view.
//
// The rest of the dashboard answers "what does this estate look like". This view answers
// the question that follows it: "which of these do I convert next, and what comes with it".
//
// Two selection shapes are supported because both occur in practice. A single program is
// the common case. Several programs chosen together are the other: a service is rarely one
// program, and converting its members separately produces parts that were never compiled
// against each other.
//
// Every number shown comes from the REKT scan and facts.json via /api/modernization/programs.
// Where a program parsed only partially, that is shown rather than smoothed over: converting
// a program whose paragraphs were never recovered produces a plausible-looking result built
// from less than the source.

const PP_FIDELITY = {
  full: { label: 'Full', color: '#10b981' },
  partial: { label: 'Partial', color: '#f59e0b' },
  'deps-only': { label: 'Deps only', color: '#38bdf8' },
  failed: { label: 'Failed', color: '#ef4444' },
  'not-parsed': { label: 'Not parsed', color: '#64748b' },
};

class ProgramPickerView {
  constructor(rootId) {
    this.rootId = rootId;
    this.programs = [];
    this.totals = { programs: 0, copybooks: 0 };
    this.selected = new Set();
    this.details = new Map();
    this.filter = '';
    this.language = 'java';
    this.includeClosure = false;
    this.note = null;
    this.loaded = false;
  }

  get root() {
    return document.getElementById(this.rootId);
  }

  async loadAndRender() {
    if (!this.loaded) {
      await this.load();
      this.loaded = true;
    }
    this.render();
  }

  async load() {
    try {
      const resp = await fetch('/api/modernization/programs');
      if (!resp.ok) {
        this.note = `Program list unavailable (HTTP ${resp.status}).`;
        return;
      }
      const payload = await resp.json();
      this.programs = payload.programs || [];
      this.totals = {
        programs: payload.totalPrograms || 0,
        copybooks: payload.totalCopybooks || 0,
      };
      this.note = payload.note || null;
    } catch (e) {
      console.error('Failed to load program list:', e);
      this.note = 'Program list could not be loaded.';
    }
  }

  // The identity sent to the converter is the source-relative path, never the basename.
  // Two programs in an estate may share a basename, and the shorter form would convert
  // whichever the scanner happened to record last.
  identityOf(p) {
    return p.relativePath || p.basename;
  }

  visiblePrograms() {
    const q = this.filter.trim().toLowerCase();
    if (!q) return this.programs;
    return this.programs.filter(p =>
      (p.basename || '').toLowerCase().includes(q) ||
      (p.relativePath || '').toLowerCase().includes(q));
  }

  render() {
    const root = this.root;
    if (!root) return;

    const visible = this.visiblePrograms();
    const selectedCount = this.selected.size;

    root.innerHTML = `
      <div class="pp-wrap">
        <div class="pp-header">
          <div>
            <h2 class="pp-title">Choose programs to convert</h2>
            <div class="pp-sub">
              ${this.totals.programs} programs in this estate.
              ${this.totals.copybooks} copybooks travel with whatever you select.
            </div>
          </div>
          <input id="pp-filter" class="pp-filter" type="text" placeholder="Filter by name or path"
                 value="${this.escape(this.filter)}" />
        </div>

        ${this.note ? `<div class="pp-note">${this.escape(this.note)}</div>` : ''}

        <div class="pp-body">
          <div class="pp-list-pane">
            <div class="pp-list-head">
              <label class="pp-selectall">
                <input type="checkbox" id="pp-select-all"
                  ${visible.length > 0 && visible.every(p => this.selected.has(this.identityOf(p))) ? 'checked' : ''} />
                <span>Select all shown (${visible.length})</span>
              </label>
            </div>
            <div class="pp-list">
              ${visible.length === 0
                ? '<div class="pp-empty">No program matches that filter.</div>'
                : visible.map(p => this.renderRow(p)).join('')}
            </div>
          </div>

          <div class="pp-detail-pane" id="pp-detail">
            ${this.renderDetail()}
          </div>
        </div>

        <div class="pp-actionbar">
          <div class="pp-selection-summary">
            ${selectedCount === 0
              ? 'Nothing selected.'
              : `<strong>${selectedCount}</strong> program${selectedCount === 1 ? '' : 's'} selected`}
          </div>
          <label class="pp-closure">
            <input type="checkbox" id="pp-include-closure" ${this.includeClosure ? 'checked' : ''} />
            <span title="Adds every program reachable by CALL from your selection, so a converted program does not call something that was never converted. Same as --include-callees.">Add call closure</span>
          </label>
          <select id="pp-language" class="pp-language">
            <option value="java" ${this.language === 'java' ? 'selected' : ''}>Java</option>
            <option value="csharp" ${this.language === 'csharp' ? 'selected' : ''}>C#</option>
          </select>
          <button id="pp-convert" class="pp-convert" ${selectedCount === 0 ? 'disabled' : ''}>
            Convert selection
          </button>
        </div>

        <div id="pp-command" class="pp-command"></div>
      </div>
    `;

    this.bind();
  }

  renderRow(p) {
    const id = this.identityOf(p);
    const checked = this.selected.has(id) ? 'checked' : '';
    const fid = PP_FIDELITY[p.parseFidelity] || PP_FIDELITY['not-parsed'];

    // A partially parsed program is the single most useful warning on this screen, so it
    // sits on the row rather than behind a click.
    const warn = p.missingCopybookCount > 0
      ? `<span class="pp-warn" title="Copybooks this program COPYs were not found. Their record layouts will be inferred rather than carried.">${p.missingCopybookCount} missing copybook${p.missingCopybookCount === 1 ? '' : 's'}</span>`
      : '';

    return `
      <div class="pp-row ${checked ? 'pp-row-selected' : ''}" data-identity="${this.escape(id)}">
        <label class="pp-row-main">
          <input type="checkbox" class="pp-check" data-identity="${this.escape(id)}" ${checked} />
          <span class="pp-name">${this.escape(p.basename)}</span>
          ${p.ambiguousBasename ? `<span class="pp-path" title="This basename is not unique in the estate">${this.escape(p.relativePath)}</span>` : ''}
        </label>
        <div class="pp-row-meta">
          <span class="pp-loc">${p.linesOfCode.toLocaleString()} lines</span>
          <span class="pp-fid" style="color:${fid.color}" title="Parse fidelity: how much of this program the parser recovered">${fid.label}</span>
          ${p.callClosureCount > 0 ? `<span class="pp-closure-count" title="Programs in this estate reachable by CALL from here">+${p.callClosureCount} called</span>` : ''}
          ${warn}
          <button class="pp-inspect" data-identity="${this.escape(id)}">Details</button>
        </div>
      </div>
    `;
  }

  renderDetail() {
    if (!this.activeDetail) {
      return `<div class="pp-detail-empty">
        Select <em>Details</em> on a program to see what converting it would pull in.
      </div>`;
    }

    const d = this.activeDetail;
    if (d.note && !d.basename) {
      return `<div class="pp-detail-empty">${this.escape(d.note)}</div>`;
    }

    const list = (label, items, hint) => {
      const arr = items || [];
      return `
        <div class="pp-detail-block">
          <div class="pp-detail-label" ${hint ? `title="${this.escape(hint)}"` : ''}>${label} <span class="pp-count">${arr.length}</span></div>
          ${arr.length === 0
            ? '<div class="pp-detail-none">None recorded</div>'
            : `<div class="pp-chips">${arr.map(i => `<span class="pp-chip">${this.escape(i)}</span>`).join('')}</div>`}
        </div>`;
    };

    const fid = PP_FIDELITY[d.parseFidelity] || PP_FIDELITY['not-parsed'];

    return `
      <div class="pp-detail">
        <div class="pp-detail-title">${this.escape(d.basename || d.identity)}</div>
        <div class="pp-detail-path">${this.escape(d.relativePath || '')}</div>
        <div class="pp-detail-stats">
          <span>${(d.linesOfCode || 0).toLocaleString()} lines</span>
          <span style="color:${fid.color}">Parse: ${fid.label}</span>
          <span title="How confident the extractor was in the facts it recorded">Facts confidence: ${d.factsConfidence ?? 0}</span>
        </div>
        ${d.note ? `<div class="pp-detail-note">${this.escape(d.note)}</div>` : ''}
        ${list('Calls', d.calls, 'Names this program CALLs. Some may be external modules that are not part of this estate.')}
        ${list('Call closure in this estate', d.callClosure, 'Programs here that are reachable by CALL. These are the ones "Add call closure" would include.')}
        ${list('Called by', d.calledBy, 'Programs that CALL this one. Converting this without them leaves callers pointing at COBOL.')}
        ${list('Copybooks', d.copybooks)}
        ${list('Missing copybooks', d.missingCopybooks, 'Referenced but not found. Their layouts will be inferred rather than carried across.')}
      </div>
    `;
  }

  async showDetail(identity) {
    if (!this.details.has(identity)) {
      try {
        const resp = await fetch(`/api/modernization/program/${encodeURI(identity)}`);
        this.details.set(identity, resp.ok
          ? await resp.json()
          : { identity, note: `Details unavailable (HTTP ${resp.status}).` });
      } catch (e) {
        console.error('Failed to load program detail:', e);
        this.details.set(identity, { identity, note: 'Details could not be loaded.' });
      }
    }
    this.activeDetail = this.details.get(identity);
    const pane = document.getElementById('pp-detail');
    if (pane) pane.innerHTML = this.renderDetail();
  }

  // Expanding the closure needs each selected program's reachable set, which only the
  // detail endpoint carries. Fetched on demand so the list view stays one request.
  async resolveSelection() {
    const chosen = Array.from(this.selected);
    if (!this.includeClosure) return chosen;

    const expanded = new Set(chosen);
    for (const identity of chosen) {
      if (!this.details.has(identity)) {
        try {
          const resp = await fetch(`/api/modernization/program/${encodeURI(identity)}`);
          if (resp.ok) this.details.set(identity, await resp.json());
        } catch (e) {
          console.error('Failed to expand closure for', identity, e);
        }
      }
      const d = this.details.get(identity);
      (d?.callClosure || []).forEach(p => expanded.add(p));
    }
    return Array.from(expanded);
  }

  async renderCommand() {
    const target = document.getElementById('pp-command');
    if (!target) return;

    if (this.selected.size === 0) {
      target.innerHTML = '';
      return;
    }

    const identities = await this.resolveSelection();
    const added = identities.length - this.selected.size;

    target.innerHTML = `
      <div class="pp-command-box">
        <div class="pp-command-head">
          Converting <strong>${identities.length}</strong> program${identities.length === 1 ? '' : 's'} to
          <strong>${this.language === 'java' ? 'Java' : 'C#'}</strong>${added > 0 ? ` (${added} added by call closure)` : ''}
        </div>
        <pre class="pp-command-text">./doctor.sh convert-only \\
  --language ${this.language === 'java' ? 'Java' : 'CSharp'} \\
${identities.map(i => `  --program ${i}`).join(' \\\n')}</pre>
        <div class="pp-command-hint">
          Copybooks are always included, so they are not listed. Add
          <code>--dry-run</code> to see what this would convert without calling a model.
          Running through <code>doctor.sh</code> rather than the CLI directly picks up the
          concurrent-run guard and the existing failure and retry handling.
        </div>
      </div>
    `;
  }

  bind() {
    const root = this.root;
    if (!root) return;

    const filter = document.getElementById('pp-filter');
    if (filter) {
      filter.addEventListener('input', e => {
        this.filter = e.target.value;
        const pos = e.target.selectionStart;
        this.render();
        const again = document.getElementById('pp-filter');
        if (again) { again.focus(); again.setSelectionRange(pos, pos); }
      });
    }

    root.querySelectorAll('.pp-check').forEach(cb => {
      cb.addEventListener('change', e => {
        const id = e.target.dataset.identity;
        if (e.target.checked) this.selected.add(id); else this.selected.delete(id);
        this.render();
        this.renderCommand();
      });
    });

    const selectAll = document.getElementById('pp-select-all');
    if (selectAll) {
      selectAll.addEventListener('change', e => {
        const visible = this.visiblePrograms();
        visible.forEach(p => {
          const id = this.identityOf(p);
          if (e.target.checked) this.selected.add(id); else this.selected.delete(id);
        });
        this.render();
        this.renderCommand();
      });
    }

    root.querySelectorAll('.pp-inspect').forEach(btn => {
      btn.addEventListener('click', e => {
        e.preventDefault();
        this.showDetail(e.target.dataset.identity);
      });
    });

    const closure = document.getElementById('pp-include-closure');
    if (closure) {
      closure.addEventListener('change', e => {
        this.includeClosure = e.target.checked;
        this.renderCommand();
      });
    }

    const lang = document.getElementById('pp-language');
    if (lang) {
      lang.addEventListener('change', e => {
        this.language = e.target.value;
        this.renderCommand();
      });
    }

    const convert = document.getElementById('pp-convert');
    if (convert) convert.addEventListener('click', () => this.renderCommand());

    if (this.selected.size > 0) this.renderCommand();
  }

  escape(s) {
    return String(s ?? '').replace(/[&<>"']/g, c =>
      ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
  }
}

window.ProgramPickerView = ProgramPickerView;
