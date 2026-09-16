// Missing Copybooks — the estate's single largest constraint on conversion fidelity.
//
// A COPY target that is not in the source drop does not stop a conversion. The parser
// substitutes a generated stub so it can finish, the converter is handed that stub, and the
// result compiles and looks ordinary. What it does not have is the real record layout, so
// every field read through that copybook is inferred rather than carried across.
//
// This view exists because that failure is otherwise invisible. It is ordered by blast
// radius — how many programs each absent copybook affects — because that is the order in
// which getting them back pays off.

class MissingCopybooksView {
  constructor(rootId) {
    this.rootId = rootId;
    this.data = null;
    this.expanded = new Set();
    this.loaded = false;
  }

  get root() { return document.getElementById(this.rootId); }

  async loadAndRender() {
    if (!this.loaded) { await this.load(); this.loaded = true; }
    this.render();
  }

  async load() {
    try {
      const resp = await fetch('/api/modernization/dependency-health');
      this.data = resp.ok ? await resp.json() : null;
      if (!resp.ok) this.error = `Dependency health unavailable (HTTP ${resp.status}).`;
    } catch (e) {
      console.error('Failed to load dependency health:', e);
      this.error = 'Dependency health could not be loaded.';
    }
  }

  render() {
    const root = this.root;
    if (!root) return;

    if (!this.data) {
      root.innerHTML = `<div class="mc-empty">${this.escape(this.error || 'No data.')}</div>`;
      return;
    }

    const d = this.data;
    const rows = (d.missingCopybooks || [])
      .slice()
      .sort((a, b) => (b.referencedBy?.length || 0) - (a.referencedBy?.length || 0));

    const blocked = d.programsBlockedByMissing || 0;
    const total = d.totalPrograms || 0;
    const pct = total > 0 ? Math.round((blocked / total) * 100) : 0;

    root.innerHTML = `
      <div class="mc-wrap">
        <div class="mc-head">
          <h2 class="mc-title">Missing copybooks</h2>
          <div class="mc-sub">
            Copybooks referenced by a COPY statement that are not present in the source.
            The parser substitutes a generated stub, so the fields behind them have no known
            layout and a conversion infers their structure instead of carrying it across.
          </div>
        </div>

        ${rows.length === 0 ? `
          <div class="mc-ok">
            Every COPY target resolves. Conversions work from the real record layouts.
          </div>` : `
          <div class="mc-stats">
            <div class="mc-stat mc-stat-warn">
              <div class="mc-stat-num">${d.totalMissingCopybooks || 0}</div>
              <div class="mc-stat-label">copybooks absent</div>
            </div>
            <div class="mc-stat mc-stat-warn">
              <div class="mc-stat-num">${blocked}<span class="mc-of"> / ${total}</span></div>
              <div class="mc-stat-label">programs affected (${pct}%)</div>
            </div>
            <div class="mc-stat">
              <div class="mc-stat-num">${d.fullFidelityCount || 0}</div>
              <div class="mc-stat-label">parsed at full fidelity</div>
            </div>
            <div class="mc-stat">
              <div class="mc-stat-num">${d.partialFidelityCount || 0}</div>
              <div class="mc-stat-label">partial — mostly for this reason</div>
            </div>
          </div>

          <div class="mc-callout">
            This is a property of the source drop, not of the tooling. No change to the
            converter recovers a layout that was never delivered; obtaining these
            ${d.totalMissingCopybooks} files is what raises fidelity across the estate.
          </div>

          <div class="mc-table">
            <div class="mc-row mc-row-head">
              <div class="mc-c-name">COPYBOOK</div>
              <div class="mc-c-count">PROGRAMS AFFECTED</div>
              <div class="mc-c-bar">BLAST RADIUS</div>
            </div>
            ${rows.map(r => this.renderRow(r, rows[0].referencedBy?.length || 1)).join('')}
          </div>
        `}
      </div>
    `;

    root.querySelectorAll('.mc-row-click').forEach(el => {
      el.addEventListener('click', () => {
        const name = el.dataset.copybook;
        if (this.expanded.has(name)) this.expanded.delete(name); else this.expanded.add(name);
        this.render();
      });
    });
  }

  renderRow(row, max) {
    const refs = row.referencedBy || [];
    const open = this.expanded.has(row.copybook);
    const width = max > 0 ? Math.max(4, Math.round((refs.length / max) * 100)) : 0;

    return `
      <div class="mc-row mc-row-click" data-copybook="${this.escape(row.copybook)}">
        <div class="mc-c-name">
          <span class="mc-caret">${open ? '▾' : '▸'}</span>
          <span class="mc-name">${this.escape(row.copybook)}</span>
        </div>
        <div class="mc-c-count">${refs.length}</div>
        <div class="mc-c-bar"><div class="mc-bar" style="width:${width}%"></div></div>
      </div>
      ${open ? `
        <div class="mc-refs">
          ${refs.map(r => `<span class="mc-ref">${this.escape(r)}</span>`).join('')}
        </div>` : ''}
    `;
  }

  escape(s) {
    return String(s ?? '').replace(/[&<>"']/g, c =>
      ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
  }
}

window.MissingCopybooksView = MissingCopybooksView;
