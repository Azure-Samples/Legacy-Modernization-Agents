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

// Parse fidelity is how much of a program the COBOL parser actually recovered. It is not a
// quality score for the program: it describes what the converter will have to work from.
//
// Structural fidelity is separate from the strict dependency gate. Missing inputs must never
// be described as permission to infer layouts or invent an implementation.
const PP_FIDELITY = {
  full: {
    label: 'Full', color: 'var(--cp-text-muted)',
    why: 'A full structural parse was recorded. This does not establish complete dependency evidence or conversion readiness.',
  },
  partial: {
    label: 'Partial', color: '#f59e0b',
    why: 'Partial structural evidence was recorded. Missing inputs and unresolved dependency evidence remain blockers; conversion must not invent their contents.',
  },
  'deps-only': {
    label: 'Deps only', color: '#38bdf8',
    why: 'Only the dependency list was recovered. Paragraphs, control flow and SQL were not, so there is far less for a conversion to work from.',
  },
  failed: {
    label: 'Failed', color: '#ef4444',
    why: 'The parser failed for this program. Strict conversion still requires valid dependency evidence.',
  },
  'not-parsed': {
    label: 'Not parsed', color: '#64748b',
    why: 'No parse output exists yet. Run ./doctor.sh rekt-full to produce it.',
  },
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
    this.readiness = null;
    this.dependencyPlans = new Map();
    this.commandRevision = 0;
    this.readinessRevision = 0;
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
    const revision = ++this.readinessRevision;
    this.readiness = null;
    this.dependencyPlans.clear();
    this.details.clear();
    this.activeDetail = null;
    this.readinessWarning = 'Checking dependency evidence. Conversion is blocked until checked.';
    this.render();
    try {
      const [resp, assessment] = await Promise.all([
        fetch('/api/modernization/programs'),
        window.ConversionReadiness.read().then(report => ({ report }), error => ({ error }))
      ]);
      if (revision !== this.readinessRevision) return;
      this.readiness = assessment.report || null;
      this.dependencyPlans = new Map((this.readiness?.programs || []).map(p => [p.program, p]));
      this.readinessWarning = assessment.error ? `Dependency readiness unavailable: ${assessment.error.message}. Conversion is blocked until checked.` : null;
      if (!resp.ok) {
        this.note = `Program list unavailable (HTTP ${resp.status}).`;
        return;
      }
      const payload = await resp.json();
      if (revision !== this.readinessRevision) return;
      this.programs = payload.programs || [];
      this.totals = {
        programs: payload.totalPrograms || 0,
        copybooks: payload.totalCopybooks || 0,
      };
      this.note = payload.note || null;
    } catch (e) {
      if (revision !== this.readinessRevision) return;
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
    const blockedCount = [...this.selected].filter(id => this.dependencyPlans.get(id)?.status !== 'eligible').length;

    root.innerHTML = `
      <div class="pp-wrap" data-portal-theme="dark">
        <div class="pp-header">
          <div>
            <h2 class="pp-title">Choose programs to convert</h2>
            <div class="pp-sub">
              Every row is a COBOL program — a compilable unit with a PROGRAM-ID and a
              PROCEDURE DIVISION. The ${this.totals.copybooks} copybooks in this estate are
              not listed: they are record layouts, not programs, and are always carried in by
              whichever programs COPY them.
            </div>
            <div class="pp-legend">
              ${Object.entries(PP_FIDELITY)
                .filter(([k]) => this.programs.some(p => p.parseFidelity === k))
                .map(([, f]) => `<span class="pp-legend-item" title="${this.escape(f.why)}">
                    <span style="color:${f.color}">●</span> ${f.label}</span>`).join('')}
              <span class="pp-legend-note">Parse fidelity — hover for what each means</span>
            </div>
          </div>
          <button id="pp-refresh-readiness" type="button">Refresh readiness</button>
          <input id="pp-filter" class="pp-filter" type="text" placeholder="Filter by name or path"
                 value="${this.escape(this.filter)}" />
        </div>

        ${this.note ? `<div class="pp-note">${this.escape(this.note)}</div>` : ''}
        ${this.readinessWarning ? `<div class="dep-readiness" role="alert">${this.escape(this.readinessWarning)}</div>` : ''}
        <div class="dep-readiness">Dependency evidence (separate from parse fidelity):
          ${Object.keys(window.ConversionReadiness.states).map(key =>
            `<span class="dep-state dep-state-${key}">${this.escape(window.ConversionReadiness.states[key].label)}</span>`).join('')}
        </div>

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
            ${blockedCount ? `<span class="dep-state dep-state-missing" role="status">${blockedCount} selected program(s) lack passing dependency evidence. Conversion will not start.</span>` : ''}
          </div>
          <label class="pp-closure">
            <input type="checkbox" id="pp-include-closure" ${this.includeClosure ? 'checked' : ''} />
            <span title="Adds every program reachable by CALL from your selection, so a converted program does not call something that was never converted. Same as --include-callees.">Add call closure</span>
          </label>
          <select id="pp-language" class="pp-language">
            <option value="java" ${this.language === 'java' ? 'selected' : ''}>Java</option>
            <option value="csharp" ${this.language === 'csharp' ? 'selected' : ''}>C#</option>
          </select>
          <button id="pp-convert" class="pp-convert" ${selectedCount === 0 || blockedCount ? 'disabled' : ''}>
            Review conversion command
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
      ? `<span class="pp-warn" title="Required copybooks were not found. Their record layouts must not be invented.">${p.missingCopybookCount} missing copybook${p.missingCopybookCount === 1 ? '' : 's'}</span>`
      : '';

    return `
      <div class="pp-row dep-row-${window.ConversionReadiness.state(this.dependencyPlans.get(id))} ${checked ? 'pp-row-selected' : ''}" data-identity="${this.escape(id)}">
        <label class="pp-row-main">
          <input type="checkbox" class="pp-check" data-identity="${this.escape(id)}" ${checked} />
          <span class="pp-name">${this.escape(p.basename)}</span>
          ${p.ambiguousBasename ? `<span class="pp-path" title="This basename is not unique in the estate">${this.escape(p.relativePath)}</span>` : ''}
        </label>
        <div class="pp-row-meta">
          <span class="pp-loc">${p.linesOfCode.toLocaleString()} lines</span>
          <span class="pp-fid" style="color:${fid.color}" title="${this.escape(fid.why)}">Parse: ${fid.label}</span>
          ${p.callClosureCount > 0 ? `<span class="pp-closure-count" title="Programs in this estate reachable by CALL from here">+${p.callClosureCount} called</span>` : ''}
          ${warn}
          ${window.ConversionReadiness.badge(this.dependencyPlans.get(id))}
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
          <span style="color:${fid.color}" title="${this.escape(fid.why)}">Parse: ${fid.label}</span>
          <span title="How confident the extractor was in the facts it recorded">Facts confidence: ${d.factsConfidence ?? 0}</span>
        </div>
        <div class="pp-detail-why">${this.escape(fid.why)}</div>
        ${window.ConversionReadiness.details(this.readiness
          && this.dependencyPlans.has(d.relativePath || d.identity)
          ? { ...this.readiness, programs: [this.dependencyPlans.get(d.relativePath || d.identity)] } : null)}
        ${d.note ? `<div class="pp-detail-note">${this.escape(d.note)}</div>` : ''}
        ${list('Calls', d.calls, 'Names this program CALLs. Some may be external modules that are not part of this estate.')}
        ${list('Call closure in this estate', d.callClosure, 'Programs here that are reachable by CALL. These are the ones "Add call closure" would include.')}
        ${list('Called by', d.calledBy, 'Programs that CALL this one. Converting this without them leaves callers pointing at COBOL.')}
        ${list('Copybooks', d.copybooks)}
        ${list('Missing copybooks', d.missingCopybooks, 'Referenced but not found. Supply the real definitions; their layouts must not be invented.')}
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
    const revision = this.readinessRevision;
    const chosen = Array.from(this.selected);
    if (!this.includeClosure) return chosen;

    const expanded = new Set(chosen);
    for (const identity of chosen) {
      if (!this.details.has(identity)) {
        try {
          const resp = await fetch(`/api/modernization/program/${encodeURI(identity)}`);
          if (!resp.ok) throw new Error(`Call closure unavailable for ${identity} (HTTP ${resp.status}).`);
          const detail = await resp.json();
          if (revision !== this.readinessRevision) throw new Error('The inventory changed while checking call closure. Review the current selection again.');
          this.details.set(identity, detail);
        } catch (e) {
          console.error('Failed to expand closure for', identity, e);
          throw e;
        }
      }
      const d = this.details.get(identity);
      if (!Array.isArray(d?.callClosure)) throw new Error(`Call closure is unavailable for ${identity}; conversion scope cannot be confirmed.`);
      d.callClosure.forEach(p => expanded.add(p));
    }
    return Array.from(expanded);
  }

  async renderCommand() {
    const revision = ++this.commandRevision;
    const target = document.getElementById('pp-command');
    if (!target) return;

    if (this.selected.size === 0) {
      target.innerHTML = '';
      return;
    }

    let identities;
    try { identities = await this.resolveSelection(); }
    catch (error) {
      if (revision === this.commandRevision) target.innerHTML = `<p role="alert">${this.escape(error.message)}</p>`;
      return;
    }
    if (revision !== this.commandRevision) return;
    const selectedPlans = identities.map(id => this.dependencyPlans.get(id));
    if (selectedPlans.some(p => p?.status !== 'eligible')) {
      target.innerHTML = `<p class="dep-state dep-state-missing" role="status">Conversion will not start: required program dependency evidence is missing, rejected or not checked.</p>`
        + window.ConversionReadiness.details(this.readiness
          ? { ...this.readiness, programs: selectedPlans.filter(Boolean) } : null);
      return;
    }
    const added = identities.length - this.selected.size;

    target.innerHTML = `
      <div class="pp-command-box">
        <div class="pp-command-head">
          Command preview only: <strong>${identities.length}</strong> program${identities.length === 1 ? '' : 's'} to
          <strong>${this.language === 'java' ? 'Java' : 'C#'}</strong>${added > 0 ? ` (${added} added by call closure)` : ''}
        </div>
        <pre class="pp-command-text">${this.escape(window.ConversionReadiness.command(
          this.readiness, identities, this.language === 'java' ? 'Java' : 'CSharp'))}</pre>
        <div class="pp-command-hint">
          Review each applicable agent's prompts in <code>Agents/Prompts</code> before running this command.
          This doctor.sh command checks setup and connectivity and binds the assessed source, evidence folder and exact program selection.
          Copybooks are read as context and are not listed as conversion targets.
          Run from the repository root; the CLI repeats its dependency checks.
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
    if (convert) convert.addEventListener('click', async () => {
      if (this.reviewing) return;
      this.reviewing = true;
      convert.disabled = true;
      try {
        const request = {
          command: 'convert-only', programs: await this.resolveSelection(),
          targetLanguage: this.language === 'java' ? 'Java' : 'CSharp', dependencyMode: 'strict',
          sourceFolder: this.readiness?.sourceFolder
        };
        this.readiness = await window.ConversionReadiness.read(request);
        this.dependencyPlans = new Map(this.readiness.programs.map(p => [p.program, p]));
        if (window.ConversionReadiness.allows(this.readiness)
            && !await window.ConversionReadiness.reviewPrompts(request)) return;
        await this.renderCommand();
      } catch (error) {
        document.getElementById('pp-command').innerHTML = `<p role="alert">${this.escape(error.message)}</p>`;
      } finally {
        this.reviewing = false;
        convert.disabled = this.selected.size === 0
          || [...this.selected].some(id => this.dependencyPlans.get(id)?.status !== 'eligible');
      }
    });
    document.getElementById('pp-refresh-readiness')?.addEventListener('click', async () => {
      this.loaded = false;
      await this.load();
      this.loaded = true;
      this.render();
    });

    if (this.selected.size > 0) this.renderCommand();
  }

  escape(s) {
    return String(s ?? '').replace(/[&<>"']/g, c =>
      ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
  }
}

window.ProgramPickerView = ProgramPickerView;

// Program Explorer shows the same labels for the same estate, so it reads this map rather than
// keeping a second copy that could describe "Partial" differently on the next tab along.
window.PP_FIDELITY = PP_FIDELITY;
