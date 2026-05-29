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
    // Help panel starts open the first time, then remembers user's preference.
    const helpOpen = localStorage.getItem('cm-help-open') !== 'false';
    wrap.innerHTML = `
      <div style="background:#0f172a;border:1px solid #334155;border-radius:10px;padding:0;
                  width:min(900px,92vw);max-height:90vh;display:flex;flex-direction:column;overflow:hidden;">
        <div style="padding:14px 18px;border-bottom:1px solid #1e293b;display:flex;align-items:center;gap:10px;">
          <span style="font-size:16px;font-weight:700;color:#93c5fd;">🛠️ Convert selected programs</span>
          <span style="font-size:11px;color:#64748b;flex:1;">Same flag repeated = OR · different flags = AND</span>
          <button id="cm-refresh" title="Re-fetch the catalog from the latest REKT scan" style="background:#1e293b;border:1px solid #334155;color:#94a3b8;
                  border-radius:6px;padding:4px 10px;cursor:pointer;font-size:12px;">🔄 Refresh catalog</button>
          <button id="cm-help-toggle" title="Toggle the help panel below" style="background:#1e1b4b;border:1px solid #6366f1;color:#c7d2fe;
                  border-radius:6px;padding:4px 10px;cursor:pointer;font-size:12px;">ℹ️ Help</button>
          <button id="cm-close" style="background:none;border:1px solid #334155;color:#94a3b8;
                  border-radius:6px;padding:4px 10px;cursor:pointer;font-size:12px;">✕ Close (Esc)</button>
        </div>

        <!-- Collapsible help panel — explains each field in plain English -->
        <div id="cm-help-panel" style="display:${helpOpen ? 'block' : 'none'};
             padding:14px 18px;background:#1e1b4b22;border-bottom:1px solid #1e293b;font-size:11px;color:#cbd5e1;line-height:1.6;">
          <div style="display:grid;grid-template-columns:1fr 1fr;gap:14px 22px;">

            <div><strong style="color:#93c5fd;">Programs</strong> — Exact program file names (with or without <code>.cbl</code>). Comma-separated. Each name OR-combines with the next.</div>
            <div><strong style="color:#93c5fd;">Transactions</strong> — CICS transaction codes (e.g. <code>CT01</code>). Resolved by scanning every program for <code>EXEC CICS RETURN TRANSID(X)</code> or <code>LINK PROGRAM(X)</code>.</div>

            <div><strong style="color:#93c5fd;">Waves</strong> — Migration wave from the Target Architecture plan. <strong>1</strong> = foundation / quick wins, <strong>2</strong> = core, <strong>3</strong> = complex rewrites. Requires <code>target-architecture.json</code> to be saved.</div>
            <div><strong style="color:#93c5fd;">Target components</strong> — Component IDs from the recommended architecture (e.g. <code>svc-data</code>, <code>batch-worker</code>, <code>web-ui</code>, <code>shared-lib</code>). Open the Target Architecture tab to see the full list.</div>

            <div><strong style="color:#93c5fd;">Keywords</strong> — Whole-word search across COBOL source. Useful when you want all programs that touch a specific table, copybook, or business term (e.g. <code>CUSTOMER</code>, <code>EXEC SQL UPDATE</code>).</div>
            <div><strong style="color:#93c5fd;">Include callees / callers</strong> — Expand the selection through the dependency graph: <em>callees</em> = also convert programs the selection calls, <em>callers</em> = also convert programs that call the selection. Transitive.</div>

            <hr style="grid-column:1/3;border:none;border-top:1px solid #334155;margin:6px 0;">

            <div><strong style="color:#93c5fd;">Target language</strong> — Java (Quarkus / Spring Boot) or C# (.NET). Picks which converter agent runs.</div>
            <div><strong style="color:#93c5fd;">Speed profile</strong> — Trade-off between speed and conversion quality:
              <br>• <strong>Turbo</strong>: lowest reasoning effort, 4 parallel workers — fastest, draftiest output
              <br>• <strong>Fast</strong>: low/medium reasoning, smaller token ceiling
              <br>• <strong>Balanced</strong> <em>(default)</em>: medium reasoning, 2 parallel workers
              <br>• <strong>Thorough</strong>: high reasoning, single worker — slowest but highest quality
            </div>

            <div><strong style="color:#93c5fd;">REKT structural context</strong> <em>(default ON)</em> — Injects the REKT AST/CFG/data-flow facts + FACT-LOCKING rules ("never invent fields/methods/CALLs") + shared-types registry (prevents CS0101 duplicate-type errors) into the converter prompt. Turn OFF only to A/B test prompts or if you haven't run <code>rekt-full</code> yet.</div>

            <div><strong style="color:#93c5fd;">Include reverse engineering</strong> <em>(default OFF)</em> — Run the full RE analysis (business logic extraction, use cases, glossary) <strong>before</strong> conversion. Adds 5–30 min per program. <strong>Most conversion runs don't need this</strong> — REKT structural context already gives the converter the facts it needs. Turn ON when you want a fresh RE report for documentation or when the conversion quality on a specific program is low.</div>

            <div><strong style="color:#93c5fd;">Use AI fallback when REKT misses</strong> — When the static analyser (smojol) can't parse a program, run <code>StructuralExtractorAgent</code> to synthesise the structure from raw source via the LLM. Adds tokens; output is cached.</div>
            <div><strong style="color:#93c5fd;">Max validator retries</strong> — How many times the parity validator may re-prompt the LLM to fix missing translations (missing methods, fields, CALLs). <code>0</code> = no repair pass. Default <code>1</code>.</div>

            <div><strong style="color:#93c5fd;">Minimum per-program score</strong> — Parity-validator gate (0–1). <code>0</code> = off (all results accepted). <code>0.75</code> means each converted file must hit ≥75% parity (sections→methods, fields, CALLs, SQL) or be flagged.</div>
            <div><strong style="color:#93c5fd;">On score below threshold</strong> — What happens when a file misses the minimum score:
              <br>• <strong>Continue</strong> <em>(default)</em>: mark the file as low-confidence and proceed
              <br>• <strong>Stop the run</strong>: halt the whole batch on the first failure
            </div>

          </div>
          <div style="margin-top:10px;font-size:10px;color:#64748b;font-style:italic;">
            Tip: leave selectors empty and pick just a wave or target component to convert a whole bucket at once. Combine <em>--target svc-data --wave 1 --include-callees</em> for "all wave-1 data services and the things they need".
            See <code>docs/rekt-grounded-conversion.md</code> for the full reference.
          </div>
        </div>

        <div style="padding:14px 18px;overflow-y:auto;flex:1;display:grid;grid-template-columns:1fr 1fr;gap:14px;">

          <!-- Scan-vs-source banner (populated by populateDropdowns) -->
          <div id="cm-scan-banner" style="grid-column:1/3;display:none;padding:10px 12px;border-radius:6px;font-size:12px;color:#fef3c7;"></div>

          <div><label class="cm-lab">Programs <span class="cm-q" title="Pick from the dropdown of programs found in the current scan. Comma-separated; each name OR-combines with the next.">?</span></label>
            <input id="cm-programs" type="text" placeholder="click to pick — e.g. CRECUST, COCRDSLC" class="cm-in" list="cm-programs-list" autocomplete="off">
            <datalist id="cm-programs-list"></datalist>
            <div id="cm-programs-count" class="cm-hint">Loading catalog…</div></div>

          <div><label class="cm-lab">Transactions (CICS) <span class="cm-q" title="Pick a CICS transaction code discovered in the scan. Shows tranid → program count.">?</span></label>
            <input id="cm-transactions" type="text" placeholder="click to pick — e.g. CT01, BNK1" class="cm-in" list="cm-transactions-list" autocomplete="off">
            <datalist id="cm-transactions-list"></datalist>
            <div id="cm-transactions-count" class="cm-hint"></div></div>

          <div><label class="cm-lab">Waves <span class="cm-q" title="Migration wave from target-architecture.json. Numbers in parens show how many programs fall into each wave.">?</span></label>
            <div id="cm-waves-box" style="display:flex;gap:8px;flex-wrap:wrap;">
              <span class="cm-hint">Loading…</span>
            </div></div>

          <div><label class="cm-lab">Target components <span class="cm-q" title="Pick a target component from the recommended architecture. Shows component → program count.">?</span></label>
            <input id="cm-targets" type="text" placeholder="click to pick — e.g. svc-data" class="cm-in" list="cm-targets-list" autocomplete="off">
            <datalist id="cm-targets-list"></datalist>
            <div id="cm-targets-count" class="cm-hint"></div></div>

          <div style="grid-column:1/3;"><label class="cm-lab">Keywords in source (comma-separated) <span class="cm-q" title="Whole-word, case-insensitive substring search across all .cbl files.">?</span></label>
            <input id="cm-keywords" type="text" placeholder="e.g. CUSTOMER, EXEC SQL UPDATE" class="cm-in"></div>

          <div><label class="cm-chk"><input id="cm-callees" type="checkbox"> Include transitive callees <span class="cm-q" title="Also include every program that the selection (directly or indirectly) calls.">?</span></label></div>
          <div><label class="cm-chk"><input id="cm-callers" type="checkbox"> Include transitive callers <span class="cm-q" title="Also include every program that (directly or indirectly) calls into the selection.">?</span></label></div>

          <hr style="grid-column:1/3;border:none;border-top:1px solid #1e293b;margin:4px 0;">

          <div><label class="cm-lab">Target language <span class="cm-q" title="Java = Quarkus / Spring Boot. CSharp = .NET 8.">?</span></label>
            <select id="cm-lang" class="cm-in"><option>Java</option><option>CSharp</option></select></div>

          <div><label class="cm-lab">Speed profile <span class="cm-q" title="Turbo (fastest) → Fast → Balanced (default) → Thorough (slowest, highest quality).">?</span></label>
            <select id="cm-speed" class="cm-in">
              <option value="balanced" selected>Balanced</option>
              <option value="fast">Fast</option>
              <option value="thorough">Thorough</option>
              <option value="turbo">Turbo</option>
            </select></div>

          <div><label class="cm-chk"><input id="cm-rekt-context" type="checkbox" checked> REKT structural context <span class="cm-q" title="Inject the REKT AST/CFG/data-flow facts + FACT-LOCKING rules + shared-types registry into the converter prompt. Default ON when you've run rekt-full. Turn OFF for pure-LLM (legacy) mode.">?</span></label></div>

          <div><label class="cm-chk"><input id="cm-include-re" type="checkbox"> Include reverse engineering <span class="cm-q" title="Run the full reverse-engineering analysis (business logic extraction) before conversion. Default OFF — skips RE and goes straight to conversion. Turn ON only if you need a fresh RE report (adds 5-30 min per program).">?</span></label></div>

          <div><label class="cm-chk"><input id="cm-fallback" type="checkbox"> Use AI fallback when REKT misses <span class="cm-q" title="Run StructuralExtractorAgent (LLM) on programs that smojol couldn't parse. Costs tokens; output cached.">?</span></label></div>

          <div><label class="cm-lab">Max validator retries <span class="cm-q" title="How many times the parity validator may re-prompt the LLM to fix missing translations. 0 = off.">?</span></label>
            <input id="cm-max-retries" type="number" min="0" max="5" value="1" class="cm-in"></div>

          <div><label class="cm-lab">Minimum per-program score (0–1) <span class="cm-q" title="Parity score gate. 0 = off. 0.75 means 75% of sections/fields/CALLs/SQL must have a matching translation.">?</span></label>
            <input id="cm-min-score" type="number" min="0" max="1" step="0.05" value="0" class="cm-in"></div>

          <div><label class="cm-lab">On score below threshold <span class="cm-q" title="What to do when a file misses the minimum score: continue with a low-confidence flag, or stop the run.">?</span></label>
            <select id="cm-on-low" class="cm-in">
              <option value="continue" selected>Continue (mark low-confidence)</option>
              <option value="stop">Stop the run</option>
            </select></div>

        </div>
        <!-- Output isolation banner — explains the per-run output folder contract -->
        <div style="padding:10px 18px;background:#0c4a3a;border-top:1px solid #047857;font-size:11px;color:#a7f3d0;">
          <strong style="color:#34d399;">📦 Per-run output isolation</strong>
          — Every conversion lands in its own immutable folder
          <code style="background:#022c22;padding:1px 5px;border-radius:3px;color:#86efac;">output/runs/&lt;runId&gt;-&lt;lang&gt;-&lt;slug&gt;-&lt;UTC&gt;/</code>
          so prior runs are never overwritten. The path is shown when the run starts and is queryable via <code>/api/runs/managed/&lt;runId&gt;</code>.
        </div>
        <div style="padding:12px 18px;border-top:1px solid #1e293b;display:flex;align-items:center;gap:10px;background:#1e293b22;">
          <button id="cm-preview" class="btn-small" style="background:#1e293b;border:1px solid #334155;">🔍 Preview matches</button>
          <span id="cm-preview-result" style="font-size:11px;color:#94a3b8;flex:1;"></span>
          <button id="cm-start" class="btn-small" style="background:#065f46;border:1px solid #10b981;color:#fff;font-weight:600;">🚀 Start conversion</button>
        </div>

        <!-- Running jobs panel -->
        <div id="cm-jobs-panel" style="display:none;padding:10px 18px;border-top:1px solid #1e293b;background:#0a0e1a;max-height:120px;overflow-y:auto;">
          <div style="display:flex;align-items:center;gap:8px;margin-bottom:6px;">
            <span style="font-size:11px;color:#94a3b8;font-weight:600;">Active runs:</span>
            <button id="cm-stop-all" class="btn-small" style="background:#7c2d12;border:1px solid #ea580c;color:#fef3c7;font-size:10px;padding:2px 8px;"
                    onclick="stopAllRuns()">⏹ Stop all</button>
            <button class="btn-small" style="background:#1e293b;border:1px solid #334155;color:#94a3b8;font-size:10px;padding:2px 8px;"
                    onclick="refreshJobsPanel()">🔄</button>
          </div>
          <div id="cm-jobs-list" style="font-size:10px;color:#cbd5e1;"></div>
        </div>
      </div>
      <style>
        .cm-lab { display:flex; align-items:center; gap:5px; font-size:11px; color:#94a3b8; margin-bottom:4px; }
        .cm-in  { width:100%; padding:6px 10px; background:#0a0e1a; border:1px solid #334155; color:#e2e8f0; border-radius:6px; font-size:12px; box-sizing:border-box; }
        .cm-chk { display:inline-flex; align-items:center; gap:6px; font-size:12px; color:#cbd5e1; cursor:pointer; }
        .cm-chk input { accent-color:#3b82f6; }
        .cm-q   {
          display:inline-flex; align-items:center; justify-content:center;
          width:14px; height:14px; border-radius:50%;
          background:#1e1b4b; color:#c7d2fe; border:1px solid #6366f1;
          font-size:9px; font-weight:700; cursor:help; user-select:none;
        }
        .cm-q:hover { background:#312e81; }
        .cm-hint { font-size:10px; color:#64748b; margin-top:3px; font-style:italic; }
        .cm-wave-pill {
          display:inline-flex; align-items:center; gap:5px; padding:3px 9px; border-radius:12px;
          background:#0a0e1a; border:1px solid #334155; cursor:pointer; font-size:12px; color:#cbd5e1;
        }
        .cm-wave-pill input { accent-color:#3b82f6; }
        .cm-wave-pill:hover { border-color:#3b82f6; }
      </style>`;
    document.body.appendChild(wrap);

    // Wire close + help toggle
    wrap.addEventListener('click', (e) => { if (e.target === wrap) wrap.style.display = 'none'; });
    document.getElementById('cm-close').addEventListener('click', () => wrap.style.display = 'none');
    document.getElementById('cm-help-toggle').addEventListener('click', () => {
      const panel = document.getElementById('cm-help-panel');
      const nowVisible = panel.style.display === 'none';
      panel.style.display = nowVisible ? 'block' : 'none';
      localStorage.setItem('cm-help-open', String(nowVisible));
    });
    document.getElementById('cm-refresh').addEventListener('click', async () => {
      const btn = document.getElementById('cm-refresh');
      const orig = btn.textContent;
      btn.textContent = '⏳ Refreshing…'; btn.disabled = true;
      try { populateDropdowns(await loadCatalog(true)); btn.textContent = '✓ Refreshed'; }
      catch (e) { btn.textContent = '✗ ' + e.message; }
      finally { setTimeout(() => { btn.textContent = orig; btn.disabled = false; }, 1500); }
    });
    document.addEventListener('keydown', (e) => {
      if (e.key === 'Escape' && wrap.style.display !== 'none') wrap.style.display = 'none';
    });
    document.getElementById('cm-preview').addEventListener('click', previewSelection);
    document.getElementById('cm-start').addEventListener('click', startConversion);

    // Datalist appends-on-pick: when the user selects from the dropdown, append
    // the value to the existing comma-separated list rather than replacing it.
    // Detection: 'input' event fires immediately on a datalist pick with a
    // value that exactly matches one of the options.
    ['cm-programs', 'cm-transactions', 'cm-targets'].forEach(id => {
      const el = document.getElementById(id);
      const listId = id + '-list';
      el.addEventListener('input', (e) => {
        const list = document.getElementById(listId);
        const opts = list ? [...list.options].map(o => o.value) : [];
        const val = el.value.trim();
        if (!val || !opts.includes(val)) return;
        const before = (el.dataset.lastValue || '').trim();
        // Only append if user picked from the dropdown (whole field == one option,
        // not a partial typed match that happens to be an exact prefix).
        if (before.includes(',') || (before && !opts.includes(before))) {
          // User was typing — leave the field as a partial entry.
          el.dataset.lastValue = el.value; return;
        }
        if (before === val) { el.dataset.lastValue = el.value; return; }
        // Append val to the current comma list and re-fire preview if applicable.
        const existing = (el.dataset.csv || '').split(',').map(s => s.trim()).filter(Boolean);
        if (!existing.some(v => v.toLowerCase() === val.toLowerCase())) existing.push(val);
        el.dataset.csv = existing.join(', ');
        el.value = el.dataset.csv;
        el.dataset.lastValue = el.value;
      });
      // On manual edit, sync the csv shadow.
      el.addEventListener('blur', () => { el.dataset.csv = el.value; });
    });
  }

  // ── Catalog cache + loader ────────────────────────────────────────────
  // Pre-populates the dropdowns with everything the current scan knows about:
  // programs, CICS transactions, waves, and target components. Cached for the
  // session so reopening the modal is instant; the user can force a refresh
  // with the 🔄 button next to the count.
  let _catalogCache = null;
  let _catalogLoading = null;

  async function loadCatalog(force) {
    const scanId = (typeof _currentScanRunId !== 'undefined' && _currentScanRunId &&
                    _currentScanRunId !== 'all' && _currentScanRunId !== 'latest')
                   ? _currentScanRunId : null;
    const cacheKey = scanId || '__live__';
    if (!force && _catalogCache && _catalogCache.__key === cacheKey) return _catalogCache;
    if (_catalogLoading) return _catalogLoading;
    _catalogLoading = (async () => {
      try {
        const url = scanId ? `/api/programs/catalog?scanRunId=${encodeURIComponent(scanId)}`
                           : '/api/programs/catalog';
        const resp = await fetch(url);
        const ctype = (resp.headers.get('content-type') || '').toLowerCase();
        if (!resp.ok) {
          throw new Error(`HTTP ${resp.status} ${resp.statusText}`);
        }
        if (!ctype.includes('application/json')) {
          throw new Error('Catalog endpoint missing — your portal is running an older build. Stop the portal process and re-run ./doctor.sh portal (or rebuild McpChatWeb).');
        }
        const data = await resp.json();
        data.__key = cacheKey;
        _catalogCache = data;
        return data;
      } finally { _catalogLoading = null; }
    })();
    return _catalogLoading;
  }

  function populateDropdowns(cat) {
    if (!cat) return;

    // Scan-vs-source banner: warn the user when the selected scan run contains
    // programs that are no longer on disk in source/. The convert endpoint will
    // refuse the run anyway, so flag it up front.
    const banner = document.getElementById('cm-scan-banner');
    if (banner) {
      const total = (cat.programs || []).length;
      const missing = cat.missingFromSource || 0;
      if (cat.scanRunId && missing > 0) {
        banner.style.display = 'block';
        banner.style.background = '#7c2d12';
        banner.style.border = '1px solid #ea580c';
        banner.innerHTML = `⚠️ This scan run (<code>${cat.scanRunId}</code>) references <strong>${missing} of ${total}</strong> programs that are <strong>not in <code>source/</code></strong>. They're shown greyed-out below and cannot be converted until you restore the COBOL files. The database has business-logic metadata only, not raw source.`;
      } else if (cat.scanRunId) {
        banner.style.display = 'block';
        banner.style.background = '#064e3b';
        banner.style.border = '1px solid #10b981';
        banner.innerHTML = `✓ Scan run <code>${cat.scanRunId}</code> — all ${total} programs are available in <code>source/</code>.`;
      } else {
        banner.style.display = 'none';
      }
    }

    // Programs — datalist option per program; show wave/target/availability hint.
    const progList = document.getElementById('cm-programs-list');
    progList.innerHTML = (cat.programs || []).map(p => {
      const parts = [];
      if (!p.availableInSource) parts.push('⚠ not in source/');
      if (p.wave > 0)        parts.push(`wave ${p.wave}`);
      if (p.targetComponent) parts.push(p.targetComponent);
      if (p.lineCount > 0)   parts.push(`${p.lineCount} LOC`);
      if (p.transactions && p.transactions.length)
        parts.push('txn: ' + p.transactions.slice(0, 3).join(','));
      const label = parts.length ? ` — ${parts.join(' · ')}` : '';
      // datalist options can't be disabled but the prefix makes the warning
      // visible in both the dropdown and the resulting comma list.
      const value = p.availableInSource ? p.name : `${p.name} ⚠`;
      return `<option value="${escapeAttr(value)}">${escapeAttr(p.name + label)}</option>`;
    }).join('');
    document.getElementById('cm-programs-count').textContent =
      cat.missingFromSource > 0
        ? `${(cat.programs || []).length} program(s) in scan · ${cat.missingFromSource} not on disk`
        : `${(cat.programs || []).length} program(s) in this scan`;

    // Transactions — show how many programs each transaction belongs to.
    const tranList = document.getElementById('cm-transactions-list');
    tranList.innerHTML = (cat.transactions || []).map(t =>
      `<option value="${escapeAttr(t.code)}">${escapeAttr(t.code)} — ${t.programs.length} program(s)</option>`).join('');
    document.getElementById('cm-transactions-count').textContent =
      (cat.transactions || []).length === 0
        ? 'No CICS transactions detected in this scan'
        : `${cat.transactions.length} transaction(s) discovered`;

    // Targets — show count.
    const tgtList = document.getElementById('cm-targets-list');
    tgtList.innerHTML = (cat.targets || []).map(t =>
      `<option value="${escapeAttr(t.component)}">${escapeAttr(t.component)} — ${t.count} program(s)</option>`).join('');
    document.getElementById('cm-targets-count').textContent =
      (cat.targets || []).length === 0
        ? 'No target architecture plan loaded (open Target Architecture tab and 💾 Save)'
        : `${cat.targets.length} component(s) in the plan`;

    // Waves — dynamic pills with counts. Preserve any existing checked state.
    const wavesBox = document.getElementById('cm-waves-box');
    const previouslyChecked = new Set(
      [...wavesBox.querySelectorAll('.cm-wave:checked')].map(c => c.value));
    const waves = (cat.waves || []);
    if (waves.length === 0) {
      wavesBox.innerHTML = '<span class="cm-hint">No waves in plan (open Target Architecture tab and 💾 Save)</span>';
    } else {
      wavesBox.innerHTML = waves.map(w =>
        `<label class="cm-wave-pill"><input type="checkbox" value="${w.wave}" class="cm-wave"${
          previouslyChecked.has(String(w.wave)) ? ' checked' : ''
        }> Wave ${w.wave} <span style="color:#64748b;">(${w.count})</span></label>`).join('');
    }
  }

  function escapeAttr(s) {
    return String(s == null ? '' : s)
      .replace(/&/g, '&amp;').replace(/"/g, '&quot;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
  }


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
      rektContext:    document.getElementById('cm-rekt-context').checked,
      includeReverseEngineering: document.getElementById('cm-include-re').checked,
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
    const startBtn = document.getElementById('cm-start');

    // Guard: prevent double-click spam that spawns 100+ processes
    if (startBtn.disabled) return;
    startBtn.disabled = true;
    startBtn.textContent = '⏳ Starting…';
    startBtn.style.opacity = '0.5';
    // Strip the "⚠" markers users may have picked from the dropdown — the
    // backend will refuse them anyway, but this gives a clearer "X not on disk"
    // error rather than "no files matched".
    const sel = readSelector();
    sel.programs = (sel.programs || []).map(p => p.replace(/\s*⚠\s*$/, '').trim()).filter(Boolean);
    out.textContent = 'Starting…';
    out.style.color = '#94a3b8';
    try {
      const resp = await fetch('/api/runs/convert', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(sel),
      });
      if (!resp.ok) {
        const err = await resp.json().catch(() => ({ error: resp.statusText }));
        if (err.missingFiles && err.missingFiles.length) {
          out.innerHTML = `❌ <strong>${escapeAttr(err.error)}</strong><br>
            <span style="font-size:11px;color:#fbbf24;">${escapeAttr(err.explanation || '')}</span><br>
            <span style="font-size:11px;color:#94a3b8;">Missing: ${err.missingFiles.slice(0,8).map(escapeAttr).join(', ')}${err.missingFiles.length>8?'…':''}</span>`;
        } else {
          out.textContent = `❌ ${err.error || resp.statusText}`;
        }
        out.style.color = '#ef4444';
        return;
      }
      const data = await resp.json();
      const skippedNote = data.missingFiles && data.missingFiles.length
        ? ` <span style="color:#fbbf24;">(skipped ${data.missingFiles.length} not on disk)</span>` : '';
      // Fetch the per-run output folder via the managed-run endpoint so we
      // can surface it immediately. Falls back gracefully if not available.
      let outFolderHtml = '';
      try {
        const runData = await fetch(`/api/runs/managed/${data.runId}`).then(r => r.json());
        const folder = runData?.info?.outputFolder;
        if (folder) {
          outFolderHtml = `<div style="margin-top:6px;font-size:11px;color:#86efac;">
            📦 Output folder (immutable): <code style="background:#022c22;padding:1px 5px;border-radius:3px;">${escapeAttr(folder)}</code>
          </div>`;
        }
      } catch { /* tolerated; banner just won't show the folder */ }

      out.innerHTML = `✓ Run <code>${data.runId}</code> started — ${data.fileCount} file(s) staged.${skippedNote}
        <button id="cm-stop-run" style="margin-left:10px;background:#7c2d12;border:1px solid #ea580c;color:#fef3c7;border-radius:6px;padding:4px 12px;cursor:pointer;font-size:11px;"
                onclick="stopConversionRun('${data.runId}')">⏹ Stop run</button>
        <span style="color:#94a3b8;font-size:11px;"> Watch the Mission Control panel for progress.</span>
        ${outFolderHtml}`;
      out.style.color = '#10b981';
      // Auto-switch Mission Control to this run so the log panel shows output.
      if (typeof _activeRunId !== 'undefined') window._activeRunId = data.runId;
      const mcSelect = document.getElementById('mc-run-select');
      if (mcSelect) {
        if (typeof loadManagedRuns === 'function') loadManagedRuns().then(() => {
          mcSelect.value = data.runId;
          if (typeof refreshRunLog === 'function') refreshRunLog();
        });
      }
      refreshJobsPanel();
      // Re-enable after 5s (enough to see the confirmation; prevents spam)
      setTimeout(() => { startBtn.disabled = false; startBtn.textContent = '🚀 Start conversion'; startBtn.style.opacity = '1'; }, 5000);
    } catch (e) {
      out.textContent = `❌ ${e.message}`;
      out.style.color = '#ef4444';
      startBtn.disabled = false; startBtn.textContent = '🚀 Start conversion'; startBtn.style.opacity = '1';
    }
  }

  // ── Public API ────────────────────────────────────────────────────────
  window.openConvertModal = function (prefill) {
    ensureModal();
    const setVal = (id, v) => { const el = document.getElementById(id); if (el) { el.value = v ?? ''; el.dataset.csv = el.value; el.dataset.lastValue = el.value; } };
    const setChk = (id, v) => { const el = document.getElementById(id); if (el) el.checked = !!v; };

    prefill = prefill || {};
    setVal('cm-programs',     (prefill.programs     || []).join(', '));
    setVal('cm-transactions', (prefill.transactions || []).join(', '));
    setVal('cm-targets',      (prefill.targets      || []).join(', '));
    setVal('cm-keywords',     (prefill.keywords     || []).join(', '));
    setChk('cm-callees',  prefill.includeCallees);
    setChk('cm-callers',  prefill.includeCallers);
    setChk('cm-fallback', prefill.fallbackToAi);
    // REKT context defaults ON unless the caller explicitly opts out.
    setChk('cm-rekt-context', prefill.rektContext !== false);
    // RE defaults OFF — most selector runs want convert-only.
    setChk('cm-include-re', !!prefill.includeReverseEngineering);

    if (prefill.targetLanguage)     setVal('cm-lang',  prefill.targetLanguage);
    if (prefill.speedProfile)       setVal('cm-speed', prefill.speedProfile);
    if (prefill.maxValidatorRetries != null) setVal('cm-max-retries', prefill.maxValidatorRetries);
    if (prefill.minProgramScore     != null) setVal('cm-min-score',  prefill.minProgramScore);
    if (prefill.onLowScore)         setVal('cm-on-low', prefill.onLowScore);

    document.getElementById('cm-preview-result').textContent = '';
    document.getElementById('convert-modal').style.display = 'flex';
    // Re-enable start button in case it was left disabled
    const startBtn = document.getElementById('cm-start');
    if (startBtn) { startBtn.disabled = false; startBtn.textContent = '🚀 Start conversion'; startBtn.style.opacity = '1'; }
    // Show any active runs
    if (typeof refreshJobsPanel === 'function') refreshJobsPanel();

    // Load catalog → populate dropdowns + waves. Re-apply any pre-selected
    // waves once the pills are rendered.
    loadCatalog().then(cat => {
      populateDropdowns(cat);
      document.querySelectorAll('.cm-wave').forEach(c => {
        c.checked = (prefill.waves || []).includes(parseInt(c.value, 10));
      });
      // Auto-preview if any selector pre-filled.
      if ((prefill.programs && prefill.programs.length) ||
          (prefill.transactions && prefill.transactions.length) ||
          (prefill.waves && prefill.waves.length) ||
          (prefill.targets && prefill.targets.length) ||
          (prefill.keywords && prefill.keywords.length)) {
        setTimeout(previewSelection, 100);
      }
    }).catch(err => {
      const hint = document.getElementById('cm-programs-count');
      if (hint) { hint.textContent = `Catalog load failed: ${err.message}`; hint.style.color = '#ef4444'; }
    });
  };

  // ── Running jobs panel ───────────────────────────────────────────────
  window.refreshJobsPanel = async function () {
    const panel = document.getElementById('cm-jobs-panel');
    const list = document.getElementById('cm-jobs-list');
    if (!panel || !list) return;
    try {
      const resp = await fetch('/api/runs/managed');
      if (!resp.ok) return;
      const runs = await resp.json();
      const active = runs.filter(r => r.status === 'running');
      if (active.length === 0) {
        panel.style.display = 'none';
        return;
      }
      panel.style.display = 'block';
      list.innerHTML = active.map(r =>
        `<div style="display:flex;align-items:center;gap:8px;padding:4px 0;border-bottom:1px solid #1e293b;flex-wrap:wrap;">
          <span style="color:#10b981;">🟢</span>
          <code style="color:#e2e8f0;">${escapeAttr(r.name)}</code>
          <span style="color:#64748b;">${r.command} · ${r.targetLanguage} · PID ${r.processId || '?'}</span>
          <button style="margin-left:auto;background:#7c2d12;border:1px solid #ea580c;color:#fef3c7;border-radius:4px;padding:1px 6px;cursor:pointer;font-size:9px;"
                  onclick="stopConversionRun('${r.runId}');setTimeout(refreshJobsPanel,1000)">⏹</button>
          ${r.outputFolder ? `<div style="flex-basis:100%;font-size:9px;color:#86efac;padding-left:20px;">
            📦 <code style="background:#022c22;padding:0 4px;border-radius:2px;">${escapeAttr(r.outputFolder)}</code>
          </div>` : ''}
        </div>`).join('');
    } catch { /* silent */ }
  };

  window.stopAllRuns = async function () {
    try {
      const resp = await fetch('/api/runs/managed');
      if (!resp.ok) return;
      const runs = await resp.json();
      const active = runs.filter(r => r.status === 'running');
      for (const r of active) {
        await fetch('/api/runs/stop', {
          method: 'POST', headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ runId: r.runId })
        });
      }
      const out = document.getElementById('cm-preview-result');
      if (out) { out.textContent = `⏹ Stopped ${active.length} run(s).`; out.style.color = '#fbbf24'; }
      setTimeout(refreshJobsPanel, 500);
    } catch (e) {
      console.error('stopAllRuns failed:', e);
    }
  };

  // ── Stop a running conversion ──────────────────────────────────────────
  window.stopConversionRun = async function (runId) {
    const btn = document.getElementById('cm-stop-run');
    if (btn) { btn.disabled = true; btn.textContent = '⏳ Stopping…'; }
    try {
      const resp = await fetch('/api/runs/stop', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ runId }),
      });
      if (resp.ok) {
        const out = document.getElementById('cm-preview-result');
        if (out) { out.innerHTML = `⏹ Run <code>${runId}</code> stopped.`; out.style.color = '#fbbf24'; }
      } else {
        if (btn) { btn.textContent = '⏹ Stop failed'; }
      }
    } catch (e) {
      if (btn) { btn.textContent = `⏹ ${e.message}`; }
    }
  };
})();
