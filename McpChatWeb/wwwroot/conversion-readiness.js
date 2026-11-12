// Shared dependency evidence labels and launch-time prompt review; neither calls a model.
(() => {
  const esc = value => String(value ?? '').replace(/[&<>"']/g,
    c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
  const states = {
    missing: { label: 'Missing dependency evidence - blocked', tone: 'danger' },
    blocked: { label: 'Dependency check rejected - blocked', tone: 'warning' },
    eligible: { label: 'Dependency check passed', tone: 'success' },
    unknown: { label: 'Dependency readiness unknown', tone: 'text-muted' },
  };
  const inputStates = {
    resolved: 'Present · hash matches',
    configured: 'Configured outside source',
    missing: 'Not found in selected source',
    rejected: 'Placeholder rejected',
    unverified: 'Present · not bound',
    stale: 'Present · evidence stale',
    unreadable: 'Cannot read input',
    recorded: 'Present at scan time',
  };
  function inputs(plan, sourceFolder = '', recorded = false) {
    const inventory = plan?.dependencyInputs;
    if (!inventory) return '<p class="dep-input-note">Input inventory not recorded. Missing evidence does not mean missing source or zero dependencies.</p>';
    const usable = item => ['resolved', 'configured', 'recorded'].includes(item.status);
    const items = [...(inventory.items || [])].sort((a, b) => Number(usable(a)) - Number(usable(b)));
    const attention = items.filter(item => !['resolved', 'configured', 'recorded'].includes(item.status)).length;
    const shared = items.filter(item => item.scope === 'shared').length;
    const root = String(sourceFolder).replace(/\\/g, '/').replace(/\/$/, '') + '/';
    const path = value => {
      const normalized = String(value).replace(/\\/g, '/');
      return sourceFolder && normalized.startsWith(root) ? normalized.slice(root.length) : value;
    };
    return `<details class="dep-input-inventory" ${attention || inventory.warning ? 'open' : ''}>
      <summary>${recorded ? 'Recorded inputs' : 'Input availability'}: ${items.length} bindings${shared ? ` · ${shared} shared` : ''}${attention ? ` · ${attention} need attention` : ''}</summary>
      ${inventory.warning ? `<p role="note">${esc(inventory.warning)}</p>` : ''}
      ${items.length ? `<ul>${items.map(item => `<li class="dep-input-item dep-input-${esc(item.status)}">
        <div><strong>${esc(item.kind)} ${esc(item.name)}</strong> <span>${esc(inputStates[item.status] || 'Availability unknown')}</span>
          ${item.scope === 'shared' ? '<span class="dep-input-shared">Shared source path</span>' : ''}
          ${item.scope === 'derived' ? '<span class="dep-input-note">Derived input</span>' : ''}</div>
        ${item.path ? `<code title="${esc(item.path)}">${esc(path(item.path))}</code>` : '<small>No matching local path recorded.</small>'}
        <small>${esc(item.reason)}</small></li>`).join('')}</ul>` : '<p>No file bindings recorded; this is not a claim of zero runtime dependencies.</p>'}
      <p class="dep-input-note">Shared means outside the program's top-level source folder, not missing or automatically rejected. Presence is separate from parser support and conversion readiness${recorded ? '; these are scan-time observations, not current filesystem checks' : ''}.</p>
    </details>`;
  }
  function inputTotals(report) {
    const programs = report?.programs || [];
    const unique = new Map();
    const priority = { resolved: 0, configured: 1, unverified: 2, stale: 3, rejected: 4, missing: 5, unreadable: 6 };
    for (const program of programs)
      for (const input of program.dependencyInputs?.items || []) {
        const key = `${input.kind}\0${input.path || String(input.name).toUpperCase()}`;
        const previous = unique.get(key);
        const selected = previous && (priority[previous.status] ?? 7) > (priority[input.status] ?? 7) ? previous : input;
        unique.set(key, { ...selected, shared: previous?.shared || input.scope === 'shared' });
      }
    const items = [...unique.values()];
    return {
      present: items.filter(item => item.status === 'resolved').length,
      shared: items.filter(item => item.status === 'resolved' && item.shared).length,
      configured: items.filter(item => item.status === 'configured').length,
      missing: items.filter(item => item.status === 'missing').length,
      attention: items.filter(item => ['rejected', 'unverified', 'stale', 'unreadable'].includes(item.status)).length,
      parser: programs.filter(program => (program.blockers || []).some(blocker => ['unsupported', 'binding-gap'].includes(blocker.diagnosticCategory))).length,
      runtime: programs.filter(program => (program.blockers || []).some(blocker => blocker.diagnosticCategory === 'runtime-mapping')).length,
      unknown: programs.filter(program => !program.dependencyInputs || program.dependencyInputs.warning).length,
    };
  }
  function overview(report) {
    if (!report) return '<p class="dep-input-note">Dependency input inventory unavailable. Refresh readiness; no conversion is authorised.</p>';
    const totals = inputTotals(report);
    return `<section class="dep-readiness dep-input-overview" aria-label="Dependency input overview">
      <strong>What is present, and what needs attention?</strong>
      <div class="dep-input-counts">${[
        ['Present', totals.present, 'resolved'], ['Shared · included in present', totals.shared, 'shared'],
        ['Configured outside source', totals.configured, 'configured'],
        ['Not found in source', totals.missing, 'missing'], ['Present but unusable / unbound', totals.attention, 'unverified'],
        ['Programs with parser/exporter evidence gaps', totals.parser, 'unverified'],
        ['Programs needing runtime mapping', totals.runtime, 'unverified'],
        ['Programs without a full input inventory', totals.unknown, 'unknown']
      ].map(([label, count, status]) => `<div class="dep-input-${status}"><strong>${count}</strong><span>${label}</span></div>`).join('')}</div>
      <p class="dep-input-note">Bindings are deduplicated across these programs; conflicting bindings use the most restrictive status. Missing inputs, parser support, runtime mapping and missing evidence are different problems. Click a program for exact paths and corrective actions.</p>
    </section>`;
  }
  const categories = {
    'missing-copybook': 'Unresolved COPY input', 'missing-input': 'Unresolved callee evidence',
    'unreadable-copybook': 'Unreadable COPY input', 'unverified-input': 'Unverified input',
    unsupported: 'Parser/exporter support gap', 'derived-layout': 'Incomplete derived layout',
    'binding-gap': 'Unresolved exporter bindings', 'missing-declaration': 'Required declaration unresolved',
    'runtime-mapping': 'Target-runtime mapping unverified', 'export-failure': 'Dependency export failed',
    unclassified: 'Unclassified recorded condition',
  };
  const diagnosticLabel = category => categories[category] || 'Other evidence condition';
  function blockers(plan) {
    const all = [...new Map((plan.blockers || []).map(blocker =>
      [`${blocker.reason}\0${blocker.missingEvidence}`, blocker])).values()];
    const classified = all.filter(blocker => blocker.diagnosticCategory)
      .sort((a, b) => Number(a.diagnosticCategory === 'export-failure') - Number(b.diagnosticCategory === 'export-failure'));
    const primary = (classified.length ? classified : all).slice(0, 3);
    const row = blocker => `<p>${blocker.diagnosticCategory ? `<strong>${esc(diagnosticLabel(blocker.diagnosticCategory))}:</strong> ` : ''}${esc(blocker.reason)}
      <br><strong>Required:</strong> ${esc(blocker.missingEvidence)}<br><strong>Action:</strong> ${esc(blocker.action)}</p>`;
    return primary.map(row).join('')
      + (all.length > primary.length ? `<details><summary>All ${all.length} evidence conditions · technical details</summary>${all.map(row).join('')}</details>` : '');
  }
  function state(plan) {
    if (plan?.status === 'eligible') return 'eligible';
    if (plan?.status !== 'blocked') return 'unknown';
    return (plan.blockers || []).some(b => String(b.raw || '').includes('missing dependency-evidence.json'))
      ? 'missing' : 'blocked';
  }
  function badge(plan) {
    const key = state(plan);
    return `<span class="dep-state dep-state-${key}">${esc(states[key].label)}</span>`;
  }
  function details(report) {
    if (!report) return '<p class="dep-state dep-state-unknown">Dependency readiness is unavailable. No launch is authorised by this view.</p>';
    const programs = report.programs || [];
    const passed = programs.filter(p => p.status === 'eligible').length;
    const overall = report.selectionError ? 'blocked'
      : programs.some(p => state(p) === 'missing') ? 'missing'
      : programs.some(p => state(p) === 'blocked') ? 'blocked'
      : programs.length && passed === programs.length ? 'eligible' : 'unknown';
    return `<section class="dep-readiness dep-panel-${overall}" aria-label="Program dependency readiness">
      <strong>Dependency checks: ${passed}/${programs.length} passed</strong>
      ${programs.length ? '' : '<p>No programs were assessed. This does not authorise conversion.</p>'}
      <p>Missing or rejected evidence blocks strict conversion. Parsed structure is not dependency readiness; a passed check is not proof of equivalent generated behaviour.</p>
      ${report.selectionError ? `<p class="dep-state-blocked">${esc(report.selectionError)}</p>` : ''}
      <details ${passed !== programs.length ? 'open' : ''}><summary>Program evidence and corrective actions</summary>
        <ul>${programs.map(p => `<li class="dep-evidence-item dep-row-${state(p)}"><strong>${esc(p.program)}</strong> ${badge(p)}
          ${inputs(p, report.sourceFolder)}${blockers(p)}</li>`).join('')}</ul>
      </details>
      <details class="dep-check-location"><summary>Check time and evidence location</summary>
        <small>Checked ${esc(report.checkedAt || 'time not recorded')}. Source: ${esc(report.sourceFolder)}. Evidence: ${esc(report.factsFolder)}.</small>
      </details>
    </section>`;
  }
  // refresh asks the server to discard its cached estate assessment and re-read every input.
  async function read(request, { refresh = false } = {}) {
    const response = await fetch(refresh && !request ? '/api/runs/readiness?refresh=true' : '/api/runs/readiness', request
      ? { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(request), cache: 'no-store' }
      : { cache: 'no-store' });
    const report = await response.json();
    if (!response.ok) throw new Error(report.error || report.detail || `Dependency assessment failed (HTTP ${response.status}).`);
    return report;
  }
  function allows(report, mode = 'strict') {
    return mode === 'partial' || (mode === 'draft' ? report?.draftAllowed === true : report?.strictAllowed === true);
  }
  async function reviewPrompts(request) {
    const response = await fetch('/api/prompts', { cache: 'no-store' });
    if (!response.ok) throw new Error(`Agent prompt inventory is unavailable (HTTP ${response.status}); launch cancelled.`);
    const prompts = await response.json();
    if (!Array.isArray(prompts) || !prompts.length) throw new Error('No agent prompt inventory was returned; launch cancelled.');
    const dialog = document.createElement('dialog');
    dialog.className = 'conversion-prompt-dialog';
    dialog.setAttribute('data-portal-theme', 'dark');
    dialog.setAttribute('aria-labelledby', 'conversion-prompt-title');
    dialog.innerHTML = `<h2 id="conversion-prompt-title">Review each agent's prompts</h2>
      <p>Before starting ${esc(request.targetLanguage || 'this run')}, review the applicable agent prompts in <code>Agents/Prompts</code>.
      Set the target architecture, framework, coding requirements, and error handling you want. Existing templates can be kept if they meet your needs.</p>
      <p>Templates listed below are not evidence that you have reviewed them. Editing a prompt cancels this launch; start again when ready.</p>
      <ul>${prompts.map(p => `<li><button type="button" data-review-prompt="${esc(p.id)}">${esc(p.name || p.id)}</button>
        <span>${p.enabled === false ? 'disabled' : 'current template'}</span></li>`).join('')}</ul>
      <p>Prompts cannot supply missing source or dependency evidence, or guarantee equivalent behaviour.</p>
      <div class="conversion-prompt-actions">
        <button type="button" data-prompt-cancel>Cancel</button>
        <button type="button" data-prompt-continue>Continue with current prompts</button>
      </div>`;
    document.body.appendChild(dialog);
    return new Promise(resolve => {
      dialog.addEventListener('close', () => {
        const proceed = dialog.returnValue === 'continue';
        dialog.remove();
        resolve(proceed);
      }, { once: true });
      dialog.querySelector('[data-prompt-cancel]').onclick = () => dialog.close('cancel');
      dialog.querySelector('[data-prompt-continue]').onclick = () => dialog.close('continue');
      dialog.querySelectorAll('[data-review-prompt]').forEach(button => {
        button.onclick = () => {
          dialog.close('cancel');
          window.openPromptEditor(button.dataset.reviewPrompt, prompts);
        };
      });
      dialog.showModal();
    });
  }
  function graphColor(plan, element = document.documentElement) {
    const variable = `--cp-${states[state(plan)].tone}`;
    return getComputedStyle(element).getPropertyValue(variable).trim();
  }
  function command(report, programs, language, mode = 'strict') {
    if (!report?.sourceFolder || !report?.factsFolder || !programs.length)
      throw new Error('A conversion command requires an assessed source, evidence root and nonempty program selection.');
    const quote = value => `'${String(value).replace(/'/g, "'\\''")}'`;
    return `REKT_FACTS_FOLDER=${quote(report.factsFolder)} CONVERSION_DEPENDENCY_MODE=${quote(mode)} `
      + `./doctor.sh convert-only --source ${quote(report.sourceFolder)} --language ${quote(language)} `
      + `--programs ${quote(programs.join(','))}`;
  }
  window.ConversionReadiness = { states, state, badge, details, inputs, inputTotals, overview, diagnosticLabel, read, allows, reviewPrompts, graphColor, command };
})();
