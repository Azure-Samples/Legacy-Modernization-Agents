// convert-modal.js — Shared "Convert" modal + inline button wiring.
//
// Exposes a single global function: openConvertModal({ programs, transactions,
// waves, targets, keywords, includeCallees, includeCallers, targetLanguage,
// speedProfile, provider, modelId, fallbackToAi, maxValidatorRetries,
// minProgramScore, onLowScore }) — every field is optional. Inline buttons in
// Target Architecture / Migration Planner / AST Galaxy call this with a
// pre-filled selector and the user can refine before hitting Start.
//
// Endpoints used:
//   POST /api/programs/search   — preview which files match (live as the user
//                                  edits the selectors)
//   POST /api/runs/convert      — start a focused conversion run

(function () {
  // ── Modal HTML (lazy-injected once) ───────────────────────────────────
  function ensureModal() {
    if (document.getElementById('convert-modal')) return;
    const wrap = document.createElement('div');
    wrap.id = 'convert-modal';
    wrap.style.cssText = `position:fixed;inset:0;background:rgba(3,7,18,0.85);z-index:9999;display:none;
      align-items:center;justify-content:center;font-family:'Inter',system-ui,sans-serif;color:#e2e8f0;`;
    wrap.innerHTML = `
      <div style="background:#0f172a;border:1px solid #334155;border-radius:10px;padding:0;
                  width:min(900px,92vw);max-height:90vh;display:flex;flex-direction:column;overflow:hidden;">
        <div style="padding:14px 18px;border-bottom:1px solid #1e293b;display:flex;align-items:center;gap:10px;">
          <span style="font-size:16px;font-weight:700;color:#93c5fd;">🛠️ Convert selected programs</span>
          <span style="font-size:11px;color:#64748b;">Same flag repeated = OR · different flags = AND</span>
          <button id="cm-close" style="margin-left:auto;background:none;border:1px solid #334155;color:#94a3b8;
                  border-radius:6px;padding:4px 10px;cursor:pointer;font-size:12px;">✕ Close (Esc)</button>
        </div>
        <div style="padding:14px 18px;overflow-y:auto;flex:1;display:grid;grid-template-columns:1fr 1fr;gap:14px;">

          <div><label class="cm-lab">Programs (comma-separated)</label>
            <input id="cm-programs" type="text" placeholder="e.g. CRECUST, COCRDSLC" class="cm-in"></div>

          <div><label class="cm-lab">Transactions (CICS, comma-separated)</label>
            <input id="cm-transactions" type="text" placeholder="e.g. CT01, BNK1" class="cm-in"></div>

          <div><label class="cm-lab">Waves</label>
            <div style="display:flex;gap:8px;">
              <label class="cm-chk"><input type="checkbox" value="1" class="cm-wave"> 1</label>
              <label class="cm-chk"><input type="checkbox" value="2" class="cm-wave"> 2</label>
              <label class="cm-chk"><input type="checkbox" value="3" class="cm-wave"> 3</label>
            </div></div>

          <div><label class="cm-lab">Target components (comma-separated)</label>
            <input id="cm-targets" type="text" placeholder="e.g. svc-data, batch-worker" class="cm-in"></div>

          <div style="grid-column:1/3;"><label class="cm-lab">Keywords in source (comma-separated)</label>
            <input id="cm-keywords" type="text" placeholder="e.g. CUSTOMER, EXEC SQL UPDATE" class="cm-in"></div>

          <div><label class="cm-chk"><input id="cm-callees" type="checkbox"> Include transitive callees</label></div>
          <div><label class="cm-chk"><input id="cm-callers" type="checkbox"> Include transitive callers</label></div>

          <hr style="grid-column:1/3;border:none;border-top:1px solid #1e293b;margin:4px 0;">

          <div><label class="cm-lab">Target language</label>
            <select id="cm-lang" class="cm-in"><option>Java</option><option>CSharp</option></select></div>

          <div><label class="cm-lab">Speed profile</label>
            <select id="cm-speed" class="cm-in">
              <option value="balanced" selected>Balanced</option>
              <option value="fast">Fast</option>
              <option value="thorough">Thorough</option>
              <option value="turbo">Turbo</option>
            </select></div>

          <div><label class="cm-chk"><input id="cm-fallback" type="checkbox"> Use AI fallback when REKT misses</label></div>

          <div><label class="cm-lab">Max validator retries</label>
            <input id="cm-max-retries" type="number" min="0" max="5" value="1" class="cm-in"></div>

          <div><label class="cm-lab">Minimum per-program score (0–1)</label>
            <input id="cm-min-score" type="number" min="0" max="1" step="0.05" value="0" class="cm-in"></div>

          <div><label class="cm-lab">On score below threshold</label>
            <select id="cm-on-low" class="cm-in">
              <option value="continue" selected>Continue (mark low-confidence)</option>
              <option value="stop">Stop the run</option>
            </select></div>

        </div>
        <div style="padding:12px 18px;border-top:1px solid #1e293b;display:flex;align-items:center;gap:10px;background:#1e293b22;">
          <button id="cm-preview" class="btn-small" style="background:#1e293b;border:1px solid #334155;">🔍 Preview matches</button>
          <span id="cm-preview-result" style="font-size:11px;color:#94a3b8;flex:1;"></span>
          <button id="cm-start" class="btn-small" style="background:#065f46;border:1px solid #10b981;color:#fff;font-weight:600;">🚀 Start conversion</button>
        </div>
      </div>
      <style>
        .cm-lab { display:block; font-size:11px; color:#94a3b8; margin-bottom:4px; }
        .cm-in  { width:100%; padding:6px 10px; background:#0a0e1a; border:1px solid #334155; color:#e2e8f0; border-radius:6px; font-size:12px; box-sizing:border-box; }
        .cm-chk { display:inline-flex; align-items:center; gap:6px; font-size:12px; color:#cbd5e1; cursor:pointer; }
        .cm-chk input { accent-color:#3b82f6; }
      </style>`;
    document.body.appendChild(wrap);

    // Wire close
    wrap.addEventListener('click', (e) => { if (e.target === wrap) wrap.style.display = 'none'; });
    document.getElementById('cm-close').addEventListener('click', () => wrap.style.display = 'none');
    document.addEventListener('keydown', (e) => {
      if (e.key === 'Escape' && wrap.style.display !== 'none') wrap.style.display = 'none';
    });
    document.getElementById('cm-preview').addEventListener('click', previewSelection);
    document.getElementById('cm-start').addEventListener('click', startConversion);
  }

  // ── Build a selector payload from the modal fields ────────────────────
  function readSelector() {
    const toList = id => (document.getElementById(id).value || '')
      .split(',').map(s => s.trim()).filter(Boolean);
    const waves = [...document.querySelectorAll('.cm-wave:checked')].map(c => parseInt(c.value, 10));
    return {
      programs: toList('cm-programs'),
      transactions: toList('cm-transactions'),
      waves,
      targets: toList('cm-targets'),
      keywords: toList('cm-keywords'),
      includeCallees: document.getElementById('cm-callees').checked,
      includeCallers: document.getElementById('cm-callers').checked,
      targetLanguage: document.getElementById('cm-lang').value,
      speedProfile:   document.getElementById('cm-speed').value,
      fallbackToAi:   document.getElementById('cm-fallback').checked,
      maxValidatorRetries: parseInt(document.getElementById('cm-max-retries').value, 10) || 0,
      minProgramScore:     parseFloat(document.getElementById('cm-min-score').value) || 0,
      onLowScore:          document.getElementById('cm-on-low').value,
    };
  }

  async function previewSelection() {
    const out = document.getElementById('cm-preview-result');
    out.textContent = 'Searching…';
    try {
      const resp = await fetch('/api/programs/search', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(readSelector()),
      });
      const data = await resp.json();
      if (data.count === 0) {
        out.textContent = data.summary || 'No matches.';
        out.style.color = '#f59e0b';
      } else {
        out.textContent = `${data.count} file(s) match: ${data.files.slice(0,4).join(', ')}${data.files.length > 4 ? '…' : ''}`;
        out.style.color = '#10b981';
      }
    } catch (e) {
      out.textContent = `Error: ${e.message}`;
      out.style.color = '#ef4444';
    }
  }

  async function startConversion() {
    const out = document.getElementById('cm-preview-result');
    out.textContent = 'Starting…';
    out.style.color = '#94a3b8';
    try {
      const resp = await fetch('/api/runs/convert', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(readSelector()),
      });
      if (!resp.ok) {
        const err = await resp.json().catch(() => ({ error: resp.statusText }));
        out.textContent = `❌ ${err.error || resp.statusText}`;
        out.style.color = '#ef4444';
        return;
      }
      const data = await resp.json();
      out.innerHTML = `✓ Run <code>${data.runId}</code> started — ${data.fileCount} file(s) staged. Watch the Mission Control panel for progress.`;
      out.style.color = '#10b981';
    } catch (e) {
      out.textContent = `❌ ${e.message}`;
      out.style.color = '#ef4444';
    }
  }

  // ── Public API ────────────────────────────────────────────────────────
  window.openConvertModal = function (prefill) {
    ensureModal();
    const setVal = (id, v) => { const el = document.getElementById(id); if (el) el.value = v ?? ''; };
    const setChk = (id, v) => { const el = document.getElementById(id); if (el) el.checked = !!v; };

    prefill = prefill || {};
    setVal('cm-programs',     (prefill.programs     || []).join(', '));
    setVal('cm-transactions', (prefill.transactions || []).join(', '));
    setVal('cm-targets',      (prefill.targets      || []).join(', '));
    setVal('cm-keywords',     (prefill.keywords     || []).join(', '));
    setChk('cm-callees',  prefill.includeCallees);
    setChk('cm-callers',  prefill.includeCallers);
    setChk('cm-fallback', prefill.fallbackToAi);

    document.querySelectorAll('.cm-wave').forEach(c => {
      c.checked = (prefill.waves || []).includes(parseInt(c.value, 10));
    });
    if (prefill.targetLanguage)     setVal('cm-lang',  prefill.targetLanguage);
    if (prefill.speedProfile)       setVal('cm-speed', prefill.speedProfile);
    if (prefill.maxValidatorRetries != null) setVal('cm-max-retries', prefill.maxValidatorRetries);
    if (prefill.minProgramScore     != null) setVal('cm-min-score',  prefill.minProgramScore);
    if (prefill.onLowScore)         setVal('cm-on-low', prefill.onLowScore);

    document.getElementById('cm-preview-result').textContent = '';
    document.getElementById('convert-modal').style.display = 'flex';
    // Auto-preview if any selector pre-filled
    if ((prefill.programs && prefill.programs.length) ||
        (prefill.transactions && prefill.transactions.length) ||
        (prefill.waves && prefill.waves.length) ||
        (prefill.targets && prefill.targets.length) ||
        (prefill.keywords && prefill.keywords.length)) {
      setTimeout(previewSelection, 100);
    }
  };
})();
