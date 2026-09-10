// ═══════════════════════════════════════════════════════════════════════════════
// Focused Conversion Modal (preview) — convert a named slice of the estate
// instead of the whole thing. Backed by /api/modernization/program-catalog and
// POST /api/runs/convert.
// ═══════════════════════════════════════════════════════════════════════════════

(function () {
  const MODAL_ID = 'focusedConvertModal';
  const PROGRAM_LIST_CAP = 300;

  let catalog = null;
  let selected = new Set();

  function esc(value) {
    return String(value ?? '').replace(/[&<>"']/g, c => (
      { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]
    ));
  }

  // Counts are null when no call facts exist. A "0" would read as measured evidence
  // of a leaf program, which is exactly the confident-but-wrong claim to avoid.
  function count(value) {
    return value === null || value === undefined ? '—' : value;
  }

  // BadRequest bodies arrive as a JSON-encoded string; show the message, not its quotes.
  function unwrap(text) {
    try {
      const parsed = JSON.parse(text);
      return typeof parsed === 'string' ? parsed : text;
    } catch {
      return text;
    }
  }

  function ensureModal() {
    let modal = document.getElementById(MODAL_ID);
    if (modal) return modal;

    modal = document.createElement('div');
    modal.id = MODAL_ID;
    modal.className = 'fc-backdrop';
    modal.innerHTML = `
      <div class="fc-modal" role="dialog" aria-modal="true" aria-labelledby="fc-title">
        <div class="fc-head">
          <div>
            <span id="fc-title" class="fc-title">Focused conversion</span>
            <span class="fc-badge">preview</span>
          </div>
          <button type="button" id="fc-close" class="fc-x" aria-label="Close">✕</button>
        </div>

        <div class="fc-body">
          <div id="fc-status" class="fc-status">Loading catalog…</div>

          <label class="fc-label" for="fc-search">Programs</label>
          <input id="fc-search" class="fc-in" type="search" autocomplete="off"
                 placeholder="Filter by name or path…">
          <div id="fc-programs" class="fc-programs"></div>
          <div id="fc-selected" class="fc-hint"></div>

          <div id="fc-closure" class="fc-closure"></div>

          <div class="fc-row">
            <div>
              <label class="fc-label" for="fc-name">Run name</label>
              <input id="fc-name" class="fc-in" type="text" placeholder="optional">
            </div>
            <div>
              <label class="fc-label" for="fc-lang">Target</label>
              <select id="fc-lang" class="fc-in">
                <option>Java</option>
                <option>CSharp</option>
              </select>
            </div>
            <div>
              <label class="fc-label" for="fc-speed">Profile</label>
              <select id="fc-speed" class="fc-in">
                <option value="balanced">balanced</option>
                <option value="fast">fast</option>
                <option value="thorough">thorough</option>
              </select>
            </div>
          </div>

          <div id="fc-deferred" class="fc-deferred"></div>
          <div id="fc-result" class="fc-result"></div>
        </div>

        <div class="fc-foot">
          <button type="button" id="fc-cancel" class="fc-btn">Cancel</button>
          <button type="button" id="fc-start" class="fc-btn fc-btn-go" disabled>Start conversion</button>
        </div>
      </div>`;

    document.body.appendChild(modal);
    modal.addEventListener('click', e => { if (e.target === modal) close(); });
    modal.querySelector('#fc-close').addEventListener('click', close);
    modal.querySelector('#fc-cancel').addEventListener('click', close);
    modal.querySelector('#fc-start').addEventListener('click', start);
    modal.querySelector('#fc-search').addEventListener('input', renderPrograms);
    return modal;
  }

  function renderClosure() {
    const box = document.getElementById('fc-closure');
    if (!box) return;

    if (!catalog || !catalog.closureAvailable) {
      const reason = catalog ? catalog.closureUnavailableReason : 'Catalog unavailable.';
      box.innerHTML = `
        <label class="fc-chk fc-chk-off"><input type="checkbox" disabled> Include callees</label>
        <label class="fc-chk fc-chk-off"><input type="checkbox" disabled> Include callers</label>
        <div class="fc-hint fc-warn">${esc(reason)}</div>`;
      return;
    }

    box.innerHTML = `
      <label class="fc-chk"><input type="checkbox" id="fc-callees"> Include callees</label>
      <label class="fc-chk"><input type="checkbox" id="fc-callers"> Include callers</label>
      <div class="fc-hint">Pulls in the programs the selection calls, or that call into it.</div>`;
  }

  function renderDeferred() {
    const box = document.getElementById('fc-deferred');
    if (!box) return;

    const kinds = (catalog && catalog.deferredSelectors) || [];
    if (kinds.length === 0) { box.innerHTML = ''; return; }

    box.innerHTML = `
      <div class="fc-pending">
        <span class="fc-pending-icon">🚧</span>
        <div>
          <div class="fc-pending-title">Other selectors: ${kinds.map(esc).join(', ')}</div>
          <div class="fc-pending-body">Not wired in this build — ships with a later feature.</div>
        </div>
      </div>`;
  }

  function renderPrograms() {
    const box = document.getElementById('fc-programs');
    if (!box || !catalog) return;

    const needle = (document.getElementById('fc-search').value || '').trim().toLowerCase();
    const matches = catalog.programs.filter(p =>
      !needle
      || p.basename.toLowerCase().includes(needle)
      || p.relativePath.toLowerCase().includes(needle));

    if (matches.length === 0) {
      box.innerHTML = `<div class="fc-hint">No program matches “${esc(needle)}”. The estate holds ${catalog.totalPrograms}.</div>`;
      return;
    }

    const shown = matches.slice(0, PROGRAM_LIST_CAP);
    box.innerHTML = shown.map(p => `
      <label class="fc-prog">
        <input type="checkbox" data-path="${esc(p.relativePath)}"${selected.has(p.relativePath) ? ' checked' : ''}>
        <span class="fc-prog-name">${esc(p.basename)}</span>
        <span class="fc-prog-path">${esc(p.relativePath)}</span>
        <span class="fc-prog-meta">${p.linesOfCode} LOC · ${esc(p.parseFidelity)} · calls ${count(p.calleeCount)} · called by ${count(p.callerCount)}</span>
        ${p.ambiguousBasename ? '<span class="fc-prog-warn" title="Another program shares this basename; the full path is used.">ambiguous name</span>' : ''}
      </label>`).join('')
      + (matches.length > shown.length
        ? `<div class="fc-hint">Showing the first ${PROGRAM_LIST_CAP} of ${matches.length}.</div>`
        : '');

    box.querySelectorAll('input[type=checkbox]').forEach(cb => {
      cb.addEventListener('change', () => {
        if (cb.checked) selected.add(cb.dataset.path);
        else selected.delete(cb.dataset.path);
        renderSelected();
      });
    });
  }

  function renderSelected() {
    const box = document.getElementById('fc-selected');
    const go = document.getElementById('fc-start');
    if (box) {
      box.textContent = selected.size === 0
        ? 'Nothing selected — pick at least one program.'
        : `${selected.size} selected.`;
    }
    if (go) go.disabled = selected.size === 0;
  }

  async function load() {
    const status = document.getElementById('fc-status');
    try {
      const resp = await fetch('/api/modernization/program-catalog');
      if (!resp.ok) throw new Error(`catalog request failed (${resp.status})`);
      catalog = await resp.json();
      status.textContent = `${catalog.totalPrograms} program(s) under ${catalog.sourceRoot || 'source'}.`;
      status.className = 'fc-status';
    } catch (err) {
      catalog = null;
      status.textContent = `Could not load the program catalog: ${err.message}`;
      status.className = 'fc-status fc-warn';
    }
    renderPrograms();
    renderClosure();
    renderDeferred();
    renderSelected();
  }

  async function start() {
    const go = document.getElementById('fc-start');
    const result = document.getElementById('fc-result');
    const callees = document.getElementById('fc-callees');
    const callers = document.getElementById('fc-callers');

    go.disabled = true;
    result.className = 'fc-result';
    result.textContent = 'Staging the selection…';

    try {
      const resp = await fetch('/api/runs/convert', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          programs: [...selected],
          includeCallees: !!(callees && callees.checked),
          includeCallers: !!(callers && callers.checked),
          name: document.getElementById('fc-name').value || null,
          targetLanguage: document.getElementById('fc-lang').value,
          speedProfile: document.getElementById('fc-speed').value,
        }),
      });

      const text = await resp.text();
      if (!resp.ok) {
        // The server refuses ambiguous and unresolvable selectors; show its wording verbatim
        // rather than a generic failure, because the wording says how to fix the selector.
        result.className = 'fc-result fc-warn';
        result.textContent = unwrap(text) || `Conversion was refused (${resp.status}).`;
        return;
      }

      const data = JSON.parse(text);
      renderScope(result, data);
      if (typeof window.refreshRuns === 'function') window.refreshRuns();
    } catch (err) {
      result.className = 'fc-result fc-warn';
      result.textContent = `Could not start the conversion: ${err.message}`;
    } finally {
      go.disabled = selected.size === 0;
    }
  }

  function renderScope(box, data) {
    const scope = data.scope || {};
    const programs = scope.programs || [];
    const matches = scope.matches || [];
    const unresolved = scope.unresolvedCallTargets || [];

    box.className = 'fc-result fc-ok';
    box.innerHTML = `
      <div class="fc-ok-title">Started — converting ${programs.length} program(s), ${scope.copybooks ?? 0} copybook(s).</div>
      <ul class="fc-scope">${matches.map(m => `<li>${esc(m.program)} <span class="fc-dim">(${esc(m.reason)})</span></li>`).join('')}</ul>
      ${unresolved.length > 0 ? `<div class="fc-warn">Call targets with no matching source: ${unresolved.map(esc).join(', ')}. They are not in the run.</div>` : ''}`;
  }

  function open() {
    const modal = ensureModal();
    modal.classList.add('fc-open');
    selected = new Set();
    document.getElementById('fc-search').value = '';
    document.getElementById('fc-result').innerHTML = '';
    load();
  }

  function close() {
    const modal = document.getElementById(MODAL_ID);
    if (modal) modal.classList.remove('fc-open');
  }

  window.openFocusedConvertModal = open;

  document.addEventListener('DOMContentLoaded', () => {
    const trigger = document.getElementById('mc-focused-convert-btn');
    if (trigger) trigger.addEventListener('click', open);
  });
})();
