// Modernized Banking Services — browser client.
//
// Every panel calls the same service the tests call, so what is shown here is the service's real
// answer rather than a rendering of it. Failures are surfaced verbatim: a demo that hides a 500
// is worse than one that shows it.

const PANEL_FOR = {
  'Banking calendar': 'bankdate',
  'Date validation': 'bankdate',
  'Batch throughput': 'throughput',
  'Database diagnostics': 'diagnostics',
  'Rate reconciliation': 'rates',
};

async function api(path, options = {}) {
  const resp = await fetch(path, {
    headers: { 'Content-Type': 'application/json' },
    ...options,
  });
  const text = await resp.text();
  let body;
  try { body = text ? JSON.parse(text) : null; } catch { body = text; }
  if (!resp.ok) {
    const err = new Error(`HTTP ${resp.status}`);
    err.body = body;
    err.status = resp.status;
    throw err;
  }
  return body;
}

function el(html) {
  const t = document.createElement('template');
  t.innerHTML = html.trim();
  return t.content.firstElementChild;
}

function esc(v) {
  return String(v ?? '').replace(/[&<>"']/g, c =>
    ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
}

function showError(target, error) {
  const detail = error.body && typeof error.body === 'object'
    ? (error.body.message || JSON.stringify(error.body))
    : (error.body || error.message);
  target.innerHTML = '';
  target.appendChild(el(`<div class="error-box"><strong>${esc(error.status || 'Request failed')}</strong><br>${esc(detail)}</div>`));
}

function kv(pairs) {
  return `<dl class="kv">${pairs.map(([k, v]) => `<dt>${esc(k)}</dt><dd>${v}</dd>`).join('')}</dl>`;
}

// ── Navigation ────────────────────────────────────────────────────────

function activate(panelName) {
  document.querySelectorAll('.panel').forEach(p =>
    p.classList.toggle('hidden', p.dataset.panel !== panelName));
  document.querySelectorAll('.nav-item').forEach(b =>
    b.classList.toggle('active', b.dataset.target === panelName));
}

async function buildNav() {
  const nav = document.getElementById('sidenav');
  const cards = document.getElementById('capability-cards');

  nav.appendChild(el(`<button class="nav-item active" data-target="overview">Overview<small>all capabilities</small></button>`));

  let catalog = [];
  try {
    catalog = await api('/api/catalog');
  } catch (e) {
    cards.appendChild(el(`<div class="error-box">Could not load the capability catalogue: ${esc(e.message)}</div>`));
    return;
  }

  const seen = new Set();
  for (const c of catalog) {
    const panel = PANEL_FOR[c.capability] || 'overview';
    if (!seen.has(panel)) {
      seen.add(panel);
      nav.appendChild(el(
        `<button class="nav-item" data-target="${esc(panel)}">${esc(c.capability)}<small>${esc(c.program)}</small></button>`));
    }

    cards.appendChild(el(`
      <div class="card" data-target="${esc(panel)}">
        <h3>${esc(c.capability)}</h3>
        <p>${esc(c.summary)}</p>
        <div class="meta">
          <code>${esc(c.program)}</code>
          <span class="endpoint">${esc(c.endpoint)}</span>
        </div>
      </div>`));
  }

  document.querySelectorAll('.nav-item, .card').forEach(node =>
    node.addEventListener('click', () => activate(node.dataset.target)));
}

// ── Health ────────────────────────────────────────────────────────────

async function pollHealth() {
  const dot = document.querySelector('#health-status .dot');
  const text = document.getElementById('health-text');
  try {
    const h = await api('/api/health');
    dot.className = 'dot dot-ok';
    text.textContent = `${h.capabilities} capabilities · healthy`;
  } catch {
    dot.className = 'dot dot-err';
    text.textContent = 'service unreachable';
  }
}

// ── Banking calendar ──────────────────────────────────────────────────

function classificationBadge(c) {
  const cls = c === 'BankingDay' ? 'badge-ok' : c === 'Holiday' ? 'badge-warn' : 'badge-info';
  return `<span class="badge ${cls}">${esc(c)}</span>`;
}

function wireResolve() {
  const form = document.getElementById('form-resolve');
  const out = document.getElementById('result-resolve');
  form.addEventListener('submit', async e => {
    e.preventDefault();
    const data = new FormData(form);
    const asOf = data.get('asOf');
    try {
      const r = await api('/api/bankdate/resolve', {
        method: 'POST',
        body: JSON.stringify({
          asOf: asOf || null,
          offsetDays: Number(data.get('offsetDays')) || 0,
        }),
      });
      out.innerHTML = kv([
        ['Requested', esc(r.requested)],
        ['Banking date', `<strong>${esc(r.bankingDate)}</strong>`],
        ['Classification', classificationBadge(r.classification)],
        ['Days shifted', esc(r.daysShifted)],
      ]) + `<p class="explain">${esc(r.explanation)}</p>`;
    } catch (err) { showError(out, err); }
  });
}

function wireValidate() {
  const form = document.getElementById('form-validate');
  const out = document.getElementById('result-validate');
  form.addEventListener('submit', async e => {
    e.preventDefault();
    const value = new FormData(form).get('value');
    try {
      const r = await api('/api/bankdate/validate', {
        method: 'POST', body: JSON.stringify({ value }),
      });
      out.innerHTML = kv([
        ['Valid', `<span class="badge badge-ok">yes</span>`],
        ['Parsed', esc(r.parsed)],
        ['Code', esc(r.code)],
      ]) + `<p class="explain">${esc(r.message)}</p>`;
    } catch (err) {
      // A rejected date is an answer, not a failure: show the service's reason.
      if (err.status === 400 && err.body && err.body.code) {
        out.innerHTML = kv([
          ['Valid', `<span class="badge badge-err">no</span>`],
          ['Code', esc(err.body.code)],
        ]) + `<p class="explain">${esc(err.body.message)}</p>`;
      } else { showError(out, err); }
    }
  });
}

function wireHolidays() {
  const form = document.getElementById('form-holidays');
  const out = document.getElementById('result-holidays');
  form.addEventListener('submit', async e => {
    e.preventDefault();
    const year = new FormData(form).get('year');
    try {
      const days = await api(`/api/bankdate/holidays/${encodeURIComponent(year)}`);
      out.innerHTML = `<div class="group-title">${days.length} holidays in ${esc(year)}</div>
        <div class="chips">${days.map(d => `<span class="chip">${esc(d)}</span>`).join('')}</div>`;
    } catch (err) { showError(out, err); }
  });
}

// ── Throughput ────────────────────────────────────────────────────────

function wireThroughput() {
  const form = document.getElementById('form-throughput');
  const out = document.getElementById('result-throughput');
  form.addEventListener('submit', async e => {
    e.preventDefault();
    const d = new FormData(form);
    try {
      const r = await api('/api/batch/throughput', {
        method: 'POST',
        body: JSON.stringify({
          label: d.get('label'),
          unitsProcessed: Number(d.get('unitsProcessed')),
          elapsedSeconds: Number(d.get('elapsedSeconds')),
        }),
      });
      out.innerHTML = kv([
        ['Label', esc(r.label)],
        ['Units', Number(r.unitsProcessed).toLocaleString()],
        ['Elapsed', `${esc(r.elapsedSeconds)} s`],
        ['Throughput', `<strong>${Number(r.unitsPerMinute).toLocaleString()} units/min</strong>`],
      ]) + `<div class="group-title">Job log line</div><pre class="log">${esc(r.displayLine)}</pre>`;
    } catch (err) { showError(out, err); }
  });
}

// ── SQL diagnostics ───────────────────────────────────────────────────

function severityBadge(s) {
  const cls = s === 'Info' ? 'badge-ok' : s === 'Warning' ? 'badge-warn' : 'badge-err';
  return `<span class="badge ${cls}">${esc(s)}</span>`;
}

function wireSql() {
  const form = document.getElementById('form-sql');
  const out = document.getElementById('result-sql');
  form.addEventListener('submit', async e => {
    e.preventDefault();
    const d = new FormData(form);
    try {
      const r = await api('/api/diagnostics/sql', {
        method: 'POST',
        body: JSON.stringify({
          sqlCode: Number(d.get('sqlCode')),
          rowsAffected: Number(d.get('rowsAffected')) || 0,
          statement: d.get('statement') || null,
          sqlState: null,
        }),
      });
      out.innerHTML = kv([
        ['SQLCODE', esc(r.sqlCode)],
        ['SQLSTATE', esc(r.sqlState)],
        ['Severity', severityBadge(r.severity)],
        ['Condition', esc(r.condition)],
        ['Retryable', r.isRetryable
          ? '<span class="badge badge-warn">retry</span>'
          : '<span class="badge badge-info">no</span>'],
      ]) + `<p class="explain">${esc(r.explanation)}</p>
            <div class="group-title">Recommended action</div>
            <p class="explain">${esc(r.recommendedAction)}</p>`;
    } catch (err) { showError(out, err); }
  });
}

// ── Rate reconciliation ───────────────────────────────────────────────

const SAMPLE_PRIMARY = [
  { key: 'FI01-0001', rate: 2.25, category: 'KRD' },
  { key: 'FI01-0002', rate: 3.10, category: 'KRD' },
  { key: 'FI01-0003', rate: 1.75, category: 'KRD' },
  { key: 'FI01-0004', rate: 4.00, category: 'KRD' },
  { key: 'FI01-0005', rate: 9.99, category: 'KRD', fictitious: true },
  { key: 'FI01-0006', rate: 2.50, category: 'DEP' },
];

const SAMPLE_SECONDARY = [
  { key: 'FI01-0001', rate: 2.25, category: 'KRD' },
  { key: 'FI01-0002', rate: 3.15, category: 'KRD' },
  { key: 'FI01-0003', rate: 1.75, category: 'KRD' },
];

function rateTable(title, rows, cols) {
  if (!rows.length) return `<div class="group-title">${esc(title)} — none</div>`;
  return `<div class="group-title">${esc(title)} (${rows.length})</div>
    <table class="grid"><thead><tr>${cols.map(c => `<th>${esc(c[0])}</th>`).join('')}</tr></thead>
    <tbody>${rows.map(r => `<tr>${cols.map(c => `<td>${esc(c[1](r))}</td>`).join('')}</tr>`).join('')}</tbody></table>`;
}

function wireRates() {
  const form = document.getElementById('form-rates');
  const out = document.getElementById('result-rates');
  const primary = form.querySelector('[name=primary]');
  const secondary = form.querySelector('[name=secondary]');

  const loadSample = () => {
    primary.value = JSON.stringify(SAMPLE_PRIMARY, null, 2);
    secondary.value = JSON.stringify(SAMPLE_SECONDARY, null, 2);
  };
  document.getElementById('rates-sample').addEventListener('click', loadSample);
  loadSample();

  form.addEventListener('submit', async e => {
    e.preventDefault();
    let body;
    try {
      body = {
        primary: JSON.parse(primary.value),
        secondary: JSON.parse(secondary.value),
        requiredCategory: new FormData(form).get('requiredCategory'),
      };
    } catch (parseError) {
      out.innerHTML = '';
      out.appendChild(el(`<div class="error-box">The rate sets must be valid JSON arrays. ${esc(parseError.message)}</div>`));
      return;
    }

    try {
      const r = await api('/api/rates/reconcile', { method: 'POST', body: JSON.stringify(body) });
      out.innerHTML =
        kv([
          ['Primary entries', esc(r.primaryCount)],
          ['Secondary entries', esc(r.secondaryCount)],
          ['Matched', `<span class="badge badge-ok">${r.matched.length}</span>`],
          ['Differing', `<span class="badge badge-warn">${r.differing.length}</span>`],
          ['No counterpart', `<span class="badge badge-err">${r.missingCounterpart.length}</span>`],
        ]) +
        rateTable('Matched', r.matched, [['Key', x => x.key], ['Rate', x => x.rate]]) +
        rateTable('Differing', r.differing, [
          ['Key', x => x.key], ['Primary', x => x.primaryRate],
          ['Secondary', x => x.secondaryRate], ['Delta', x => x.delta]]) +
        rateTable('No counterpart', r.missingCounterpart, [['Key', x => x.key], ['Rate', x => x.rate]]) +
        (r.rejected.length
          ? `<div class="group-title">Rejected (${r.rejected.length})</div>
             <div class="chips">${r.rejected.map(x => `<span class="chip">${esc(x)}</span>`).join('')}</div>`
          : '');
    } catch (err) { showError(out, err); }
  });
}

// ── Boot ──────────────────────────────────────────────────────────────

(async function init() {
  document.getElementById('resolve-asof').value = new Date().toISOString().slice(0, 10);
  await buildNav();
  wireResolve(); wireValidate(); wireHolidays();
  wireThroughput(); wireSql(); wireRates();
  await pollHealth();
  setInterval(pollHealth, 20000);
})();
