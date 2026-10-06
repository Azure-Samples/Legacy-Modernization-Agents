// AI Loop — what the agents did in a run and whether the result passed its gates.
//
// Every number here comes from events the run wrote as it went (output/.metrics/<run>.jsonl) and the
// gate artifacts in its output folder: compile-status.json, conversion-parity.json and
// jobs-manifest.json. The view refreshes only while its tab is open and the run is still going.

class AiLoopView {
  constructor(rootId) {
    this.rootId = rootId;
    this.runs = [];
    this.selected = null;
    this.detail = null;
    this.error = null;
    this.pollSeconds = 5;
    this.timer = null;
  }

  get root() { return document.getElementById(this.rootId); }

  get visible() {
    const panel = document.getElementById('ai-loop-container');
    return !!panel && panel.style.display !== 'none';
  }

  async loadAndRender() {
    await this.loadRuns();
    if (this.selected) await this.loadDetail(this.selected);
    this.render();
    this.schedule();
  }

  async loadRuns() {
    this.error = null;
    try {
      const resp = await fetch('/api/ai-loop/runs');
      if (!resp.ok) throw new Error(`HTTP ${resp.status}`);
      const payload = await resp.json();
      this.runs = payload.runs || [];
      this.pollSeconds = payload.pollSeconds || this.pollSeconds;
      if (!this.runs.some(r => r.runId === this.selected)) this.selected = this.runs[0]?.runId ?? null;
    } catch (e) {
      console.error('AI loop runs failed:', e);
      this.error = `Runs could not be loaded (${e.message}).`;
    }
  }

  async loadDetail(runId) {
    try {
      const resp = await fetch(`/api/ai-loop/${encodeURIComponent(runId)}`);
      if (!resp.ok) throw new Error(`HTTP ${resp.status}`);
      this.detail = await resp.json();
    } catch (e) {
      this.detail = null;
      this.error = `Run ${runId} could not be loaded (${e.message}).`;
    }
  }

  // One timer, and only while someone is looking at a run that can still change.
  schedule() {
    clearTimeout(this.timer);
    const live = this.runs.some(r => r.status === 'running');
    if (!live) return;
    this.timer = setTimeout(async () => {
      if (!this.visible) return;
      await this.loadAndRender();
    }, this.pollSeconds * 1000);
  }

  async select(runId) {
    this.selected = runId;
    await this.loadDetail(runId);
    this.render();
  }

  render() {
    const root = this.root;
    if (!root) return;
    if (this.error && !this.runs.length) {
      root.innerHTML = `<div class="mc-empty">${this.esc(this.error)}</div>`;
      return;
    }
    if (!this.runs.length) {
      root.innerHTML = `
        <div class="ail"><h2 class="emc-title">AI Loop</h2>
          <div class="mc-empty">No runs have recorded AI loop events yet. Start a conversion from Mission Control
          or Estate Mission Control, or run <code>./doctor.sh convert-only</code>; it appears here as it runs.</div></div>`;
      return;
    }

    root.innerHTML = `
      <div class="ail">
        <div class="emc-head">
          <div>
            <h2 class="emc-title">AI Loop</h2>
            <div class="emc-sub">Model calls, retries and fallbacks per agent, and the gates the output had to pass</div>
          </div>
          <div style="display:flex;gap:8px;align-items:center">
            <select class="mc-select" id="ail-run">${this.runs.map(r => `
              <option value="${this.esc(r.runId)}" ${r.runId === this.selected ? 'selected' : ''}>
                Run ${this.esc(r.runId)} · ${this.esc(r.targetLanguage || '?')} · ${this.esc(r.status)}${r.startedAt ? ' · ' + this.esc(this.when(r.startedAt)) : ''}
              </option>`).join('')}
            </select>
            <button class="emc-btn" data-act="refresh">↻</button>
          </div>
        </div>
        ${this.detail ? this.renderDetail(this.detail) : `<div class="mc-empty">${this.esc(this.error || 'Loading…')}</div>`}
      </div>`;

    root.querySelector('#ail-run')?.addEventListener('change', ev => this.select(ev.target.value));
    root.querySelector('[data-act="refresh"]')?.addEventListener('click', () => this.loadAndRender());
  }

  renderDetail(d) {
    const s = d.summary;
    return `
      ${d.warning ? `<div class="emc-warn">${this.esc(d.warning)}</div>` : ''}
      <div class="emc-kpis">
        ${this.kpi(this.statusLabel(s.status), 'status', s.status === 'failed' || s.status === 'interrupted')}
        ${this.kpi(s.durationMs != null ? this.dur(s.durationMs) : (s.currentStage || '—'), s.durationMs != null ? 'duration' : 'current stage')}
        ${this.kpi(s.calls, 'model calls')}
        ${this.kpi(s.failedCalls, 'failed calls', s.failedCalls > 0)}
        ${this.kpi(s.retries, 'retries', s.retries > 0)}
        ${this.kpi(s.fallbacks, 'fallbacks to a stub', s.fallbacks > 0)}
      </div>
      <div class="emc-section-title">Quality gates</div>
      <div class="ail-gates">${d.gates.map(g => this.renderGate(g)).join('')}</div>
      <div class="emc-section-title">Stages</div>
      ${this.renderStages(d.stages, s)}
      <div class="emc-section-title">Agents</div>
      ${this.renderAgents(d.agents)}
      <div class="emc-section-title">Retries, fallbacks and failures <span class="emc-dim">· newest last</span></div>
      ${this.renderEvents(d.events)}
      ${d.outputFolder ? `<div class="emc-dim" style="margin-top:10px">Output: <code>${this.esc(d.outputFolder)}</code></div>` : ''}`;
  }

  renderGate(g) {
    const icon = { pass: '✓', warn: '!', fail: '✕', 'not-run': '–' }[g.status] || '?';
    return `
      <div class="ail-gate ail-${this.esc(g.status)}">
        <div class="ail-gate-head"><span class="ail-gate-icon">${icon}</span><b>${this.esc(g.title)}</b><span class="ail-gate-status">${this.esc(g.status)}</span></div>
        <div class="ail-gate-headline">${this.esc(g.headline)}</div>
        ${g.details?.length ? `<details><summary class="emc-dim">details</summary>${g.details.map(x => `<div class="emc-src"><code>${this.esc(x)}</code></div>`).join('')}</details>` : ''}
        ${g.source ? `<div class="emc-dim">${this.esc(g.source)}</div>` : ''}
      </div>`;
  }

  renderStages(stages, s) {
    if (!stages.length) return '<div class="emc-dim">No stages recorded.</div>';
    const known = stages.filter(x => x.durationMs != null).reduce((n, x) => n + x.durationMs, 0) || 1;
    return `<div class="ail-stages">${stages.map((x, i) => {
      const running = x.durationMs == null && s.status === 'running' && i === stages.length - 1;
      const pct = x.durationMs != null ? Math.max(2, (x.durationMs / known) * 100) : 4;
      return `
        <div class="ail-stage" title="${this.esc(x.name)}">
          <span class="ail-stage-name">${x.total > 0 && x.number < 99 ? `${x.number}/${x.total} ` : ''}${this.esc(x.name)}</span>
          <div class="emc-bar"><div class="emc-bar-fill ${running ? 'ail-running' : 'emc-good'}" style="width:${pct}%"></div></div>
          <span class="ail-stage-dur">${running ? 'running' : x.durationMs != null ? this.dur(x.durationMs) : '—'}</span>
        </div>`;
    }).join('')}</div>`;
  }

  renderAgents(agents) {
    if (!agents.length) return '<div class="emc-dim">No model calls recorded.</div>';
    const tok = a => a.inputTokens != null ? `${this.num(a.inputTokens)} / ${this.num(a.outputTokens)}` : `<span class="emc-dim">~${this.num(Math.round(a.promptChars / 4))} / ${this.num(Math.round(a.responseChars / 4))}</span>`;
    return `
      <div class="ail-table-wrap"><table class="ail-table">
        <thead><tr><th>Agent</th><th>Calls</th><th>Failed</th><th>Retries</th><th>Fallbacks</th><th>Avg</th><th>p95</th><th title="Input / output tokens; ~ means estimated from characters">Tokens in / out</th></tr></thead>
        <tbody>${agents.map(a => `
          <tr>
            <td><b>${this.esc(a.agent)}</b>${a.models.length ? `<div class="emc-dim">${a.models.map(m => this.esc(m)).join(', ')}</div>` : ''}</td>
            <td>${a.calls}</td>
            <td class="${a.failedCalls ? 'emc-bad' : ''}">${a.failedCalls}</td>
            <td class="${a.retries ? 'emc-mid' : ''}">${a.retries}</td>
            <td class="${a.fallbacks ? 'emc-bad' : ''}">${a.fallbacks}</td>
            <td>${this.dur(a.avgMs)}</td>
            <td>${this.dur(a.p95Ms)}</td>
            <td>${tok(a)}</td>
          </tr>`).join('')}
        </tbody></table></div>`;
  }

  renderEvents(events) {
    if (!events.length) return '<div class="emc-dim">None. Every call succeeded the first time.</div>';
    const label = { llm_retry: 'retry', llm_fallback: 'fallback', llm_call: 'failed call' };
    return `<div class="ail-events">${events.slice().reverse().slice(0, 100).map(e => `
      <div class="ail-event ail-ev-${this.esc(e.kind)}">
        <span class="emc-loc">${this.esc(this.time(e.ts))}</span>
        <span class="emc-kind">${this.esc(label[e.kind] || e.kind)}</span>
        <b>${this.esc(e.agent || '')}</b>
        <span class="emc-dim">${this.esc(e.context || '')}</span>
        <div class="emc-src"><code>${this.esc(e.reason || '')}</code></div>
      </div>`).join('')}</div>`;
  }

  kpi(value, label, warn = false) {
    return `<div class="emc-kpi${warn ? ' emc-kpi-warn' : ''}"><div class="emc-kpi-num">${this.esc(value)}</div><div class="emc-kpi-label">${this.esc(label)}</div></div>`;
  }

  statusLabel(s) { return { completed: 'done', running: 'running', failed: 'failed', interrupted: 'stopped', no_files: 'no files' }[s] || s; }
  dur(ms) {
    if (ms == null) return '—';
    if (ms < 1000) return `${ms} ms`;
    const s = ms / 1000;
    if (s < 60) return `${s.toFixed(1)} s`;
    const m = Math.floor(s / 60);
    return m < 60 ? `${m}m ${Math.round(s % 60)}s` : `${Math.floor(m / 60)}h ${m % 60}m`;
  }
  num(n) { return Number(n ?? 0).toLocaleString(); }
  when(iso) { const d = new Date(iso); return isNaN(d) ? '' : d.toLocaleString(); }
  time(iso) { const d = new Date(iso); return isNaN(d) ? '' : d.toLocaleTimeString(); }
  esc(s) {
    return String(s ?? '').replace(/[&<>"']/g, c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
  }
}

window.AiLoopView = AiLoopView;
