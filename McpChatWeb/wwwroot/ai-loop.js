// Only per-run structured events and gate artifacts establish activity; missing telemetry is not zero.

class AiLoopView {
  constructor(rootId) {
    this.rootId = rootId;
    this.runs = [];
    this.managedRuns = [];
    this.selected = null;
    this.managedRunId = null;
    this.managedStatus = null;
    this.detail = null;
    this.error = null;
    this.pollSeconds = 5;
    this.timer = null;
    this.controller = null;
    this.generation = 0;
    this.active = false;
    this.failures = 0;
    this.historyOffsets = [0];
    this.hasMore = false;
    this.nextOffset = 0;
    this.processLog = null;
    this.followLatest = true;
    this.nativeProgramFilter = 'all';
    this.lastRefreshedAt = null;
    this.nativeLogs = new Map();
    this.displayStates = new Map();
    this.renderedSelection = null;
    this.renderedHasDetail = false;
    this.logController = null;
    this.logEpoch = 0;
    try {
      const saved = JSON.parse(sessionStorage.getItem('ai-loop-selection') || 'null');
      if (saved?.version === 2 && saved.followLatest === false) {
        if (saved.managedRunId && /^[A-Za-z0-9_-]{1,64}$/.test(saved.managedRunId)) this.managedRunId = saved.managedRunId;
        else if (saved.selected && /^[A-Za-z0-9_-]{1,64}(~([A-Za-z0-9_-]{1,64}|@[0-9]+))?$/.test(saved.selected)) this.selected = saved.selected;
      }
      if (this.managedRunId || this.selected) this.followLatest = false;
    } catch { /* Storage is optional. */ }
    document.addEventListener('visibilitychange', () => {
      if (!this.active) return;
      if (document.hidden) {
        this.cancel();
        this.syncMotion();
      }
      else if (this.visible) this.loadAndRender();
    });
  }

  get root() { return document.getElementById(this.rootId); }

  get visible() {
    const panel = document.getElementById('ai-loop-container');
    return this.active && !document.hidden && !!panel && panel.style.display !== 'none';
  }

  async loadAndRender() {
    this.active = true;
    this.cancel();
    if (!this.visible) return;
    const generation = this.generation;
    const controller = this.controller = new AbortController();
    const current = () => generation === this.generation && this.visible && !controller.signal.aborted;
    try {
      const pages = await Promise.all(this.historyOffsets.map(offset =>
        this.get(`/api/ai-loop/runs?offset=${offset}`, controller.signal)));
      const payload = pages[0];
      if (!current()) return;
      this.runs = pages.flatMap(p => p.runs || []).sort((a, b) => this.activityTime(b) - this.activityTime(a));
      this.managedRuns = pages.flatMap(p => p.managedRuns || []).sort((a, b) => this.activityTime(b) - this.activityTime(a));
      this.hasMore = pages.at(-1).hasMore;
      this.nextOffset = pages.at(-1).nextOffset;
      this.pollSeconds = Math.max(1, Math.min(60, Number(payload.pollSeconds) || 5));
      if (this.followLatest) {
        const newest = this.managedRuns[0];
        const managedIsNewest = newest && (!this.runs.length || this.activityTime(newest) > this.activityTime(this.runs[0]));
        const managedId = managedIsNewest ? newest.runId : null;
        const selected = managedIsNewest ? null : this.runs[0] ? this.runKey(this.runs[0]) : null;
        if (this.managedRunId !== managedId || (!managedId && this.selected !== selected)) {
          this.detail = null;
          this.processLog = null;
          this.managedStatus = null;
        }
        this.managedRunId = managedId;
        this.selected = selected;
      }
      if (!this.selected && !this.managedRunId && this.managedRuns.length) {
        const newest = this.managedRuns[0];
        if (!this.runs.length || new Date(newest.startedAt) > new Date(this.runs[0].startedAt)) {
          this.managedRunId = newest.runId;
          this.managedStatus = newest;
        }
      }
      if (!this.managedRunId && !this.selected) {
        this.selected = this.runs[0] ? this.runKey(this.runs[0]) : null;
        this.detail = null;
      }
      const managedRunId = this.managedRunId;
      const selection = this.selected;
      const [runId, attemptId] = (selection || '').split('~');
      let detail;
      if (managedRunId) {
        detail = await this.get(`/api/ai-loop/managed/${encodeURIComponent(managedRunId)}`, controller.signal, true);
        if (!current() || this.managedRunId !== managedRunId) return;
        if (detail && detail.summary?.managedRunId !== managedRunId) throw new Error('Managed run identity mismatch');
        if (!detail) {
          const managed = await this.get(`/api/runs/managed/${encodeURIComponent(managedRunId)}`, controller.signal, true);
          if (!current()) return;
          if (managed && managed.info?.runId !== managedRunId) throw new Error('Managed process identity mismatch');
          this.managedStatus = managed?.info ?? null;
        }
        this.selected = detail ? this.runKey(detail.summary) : null;
      } else {
        const locator = attemptId?.startsWith('@') ? '?attemptIndex=' + encodeURIComponent(attemptId.slice(1))
          : attemptId ? '?attemptId=' + encodeURIComponent(attemptId) : '';
        detail = runId ? await this.get(`/api/ai-loop/${encodeURIComponent(runId)}${locator}`, controller.signal) : null;
        if (!current() || this.selected !== selection) return;
        if (detail && detail.summary?.runId !== runId) throw new Error('Run identity mismatch');
      }
      const processId = detail?.summary.managedRunId || managedRunId;
      this.processLog = null;
      if (processId) {
        const process = await this.get(`/api/runs/managed/${encodeURIComponent(processId)}`, controller.signal, true);
        if (!current()) return;
        this.managedStatus = process?.info ?? this.managedRuns.find(r => r.runId === processId) ?? null;
        this.processLog = process?.log ?? null;
      }
      this.detail = detail;
      this.lastRefreshedAt = new Date().toISOString();
      this.error = null;
      this.failures = 0;
    } catch (e) {
      if (!current() || e.name === 'AbortError') return;
      this.failures++;
      this.error = 'AI Loop could not be refreshed. Displayed data may be stale; retrying automatically.';
    } finally {
      if (current()) {
        this.controller = null;
        this.render();
        this.schedule();
      }
    }
  }

  async get(url, signal, allowMissing = false) {
    const resp = await fetch(url, { signal, cache: 'no-store' });
    if (allowMissing && resp.status === 404) return null;
    if (!resp.ok) throw new Error(`HTTP ${resp.status}`);
    return resp.json();
  }

  cancel() {
    clearTimeout(this.timer);
    this.timer = null;
    this.generation++;
    this.controller?.abort();
    this.controller = null;
    this.cancelNativeLog();
  }

  cancelNativeLog() {
    this.logEpoch++;
    this.logController?.abort();
    this.logController = null;
  }

  captureDisplayState() {
    if (!this.renderedSelection || !this.renderedHasDetail || !this.root) return;
    const details = Object.fromEntries([...this.root.querySelectorAll('details')]
      .map((el, index) => [el.id || el.dataset?.ailDetail || 'detail-' + index, el.open]));
    const scroll = [...this.root.querySelectorAll('[data-ail-scroll]')].map(el => ({
      key: el.dataset.ailScroll, top: el.scrollTop, left: el.scrollLeft
    }));
    this.displayStates.set(this.renderedSelection, { details, scroll });
    if (this.displayStates.size > 20) this.displayStates.delete(this.displayStates.keys().next().value);
  }

  async refreshNativeLog() {
    const viewer = this.root?.querySelector('#ail-native-log');
    const runId = this.detail?.summary.runId;
    if (!this.visible || !viewer?.open || this.detail?.summary.activityKind !== 'native-rekt' || !runId) return;
    if (this.logController) return;
    this.cancelNativeLog();
    const epoch = this.logEpoch;
    const controller = this.logController = new AbortController();
    const current = () => epoch === this.logEpoch && this.visible && !controller.signal.aborted
      && this.detail?.summary.runId === runId && this.root?.querySelector('#ail-native-log')?.open;
    try {
      const response = await fetch(`/api/ai-loop/${encodeURIComponent(runId)}/native-log`,
        { signal: controller.signal, cache: 'no-store' });
      const data = await response.json();
      if (!current()) return;
      if (!response.ok) throw new Error(data.error || `Native log request failed (HTTP ${response.status}).`);
      if (data.runId !== runId) throw new Error('Native log identity changed; refresh the selected run.');
      this.nativeLogs.set(runId, { data, error: null });
    } catch (error) {
      if (!current() || error.name === 'AbortError') return;
      this.nativeLogs.set(runId, { data: this.nativeLogs.get(runId)?.data,
        error: `Native log refresh failed; displayed text may be stale. ${error.message}` });
    } finally {
      if (current()) {
        this.logController = null;
        const body = this.root?.querySelector('#ail-native-log-body');
        const previous = body?.querySelector('pre');
        const top = previous?.scrollTop || 0, left = previous?.scrollLeft || 0;
        if (body) {
          body.innerHTML = this.renderNativeLogBody(runId);
          const pre = body.querySelector('pre');
          if (pre) { pre.scrollTop = top; pre.scrollLeft = left; }
        }
        if (this.nativeLogs.size > 20) this.nativeLogs.delete(this.nativeLogs.keys().next().value);
      }
    }
  }

  renderNativeLogBody(runId) {
    const state = this.nativeLogs.get(runId), data = state?.data;
    // Strip terminal control sequences; text still goes through HTML escaping.
    const plain = text => String(text || '').replace(/\x1b\][^\x07]*(?:\x07|\x1b\\)/g, '')
      .replace(/\x1b\[[0-?]*[ -/]*[@-~]/g, '').replace(/[\x00-\x08\x0b-\x1f\x7f]/g, '');
    return `${state?.error ? `<p class="emc-warn" role="alert">${this.esc(state.error)}</p>` : ''}
      ${data ? `<p class="emc-dim">Read ${this.esc(data.readBytes)} / ${this.esc(data.totalBytes)} bytes
        · ${data.truncated ? 'truncated tail (maximum 64 KiB; may start mid-line)' : 'complete at read time'}
        · log written ${this.esc(this.when(data.lastWriteAt))}
        · log refreshed ${this.esc(this.when(data.refreshedAt))}</p>
        <pre data-ail-scroll="native-log" class="ail-native-log-text">${this.esc(plain(data.text))}</pre>`
        : `<p role="status">${state?.error ? 'No readable native log text is available.' : 'Opening original native log…'}</p>`}`;
  }

  deactivate() {
    this.active = false;
    this.cancel();
    this.syncMotion();
  }

  get motionEnabled() {
    return this.visible && !this.error && this.detail?.summary.status === 'running';
  }

  syncMotion() {
    this.root?.querySelector('.ail')?.setAttribute('data-motion', String(this.motionEnabled));
  }

  schedule() {
    clearTimeout(this.timer);
    if (!this.visible) return;
    // Completed and empty lists still discover runs started in the configured controls.
    const seconds = this.failures ? Math.min(60, this.pollSeconds * 2 ** Math.min(this.failures, 4))
      : this.detail?.summary.status === 'running' || (this.managedRunId && !this.detail) || this.runs.some(r => r.status === 'running')
        ? this.pollSeconds : Math.max(15, this.pollSeconds);
    this.timer = setTimeout(() => this.loadAndRender(), seconds * 1000);
  }

  async select(runId) {
    this.followLatest = false;
    this.managedRunId = null;
    this.managedStatus = null;
    this.processLog = null;
    this.selected = runId;
    this.saveSelection();
    this.detail = null;
    this.render();
    await this.loadAndRender();
  }

  async selectManagedRun(managedRunId) {
    this.cancel();
    this.followLatest = false;
    this.managedRunId = managedRunId;
    this.managedStatus = null;
    this.processLog = null;
    this.selected = null;
    this.saveSelection();
    this.detail = null;
    this.error = null;
    this.render();
    await this.loadAndRender();
  }

  renderPending() {
    const status = this.managedStatus?.status;
    const ended = ['completed', 'partial', 'failed', 'stopped', 'cancelled', 'terminated', 'interrupted', 'unknown'].includes(status);
    return `<div class="mc-empty" role="status">Managed run <code>${this.esc(this.managedRunId)}</code>
      ${status ? ` · process ${this.esc(status)}` : ' · process status not available'}
      ${this.managedStatus?.exitCode != null ? ` · exit code ${this.esc(this.managedStatus.exitCode)}` : ''}.
      ${ended ? 'The process ended without a recorded AI Loop attempt.' : 'Waiting for this launch to record an AI Loop attempt.'}
      No model activity or quality-gate result is established. Historical runs are not evidence for this launch.
      ${this.managedStatus?.error ? `<p>${this.esc(this.managedStatus.error)}</p>` : ''}
      ${this.managedStatus?.historyWarning ? `<p>${this.esc(this.managedStatus.historyWarning)}</p>` : ''}
      ${this.managedStatus?.dependencyMode === 'draft' ? '<p>Draft dependency mode · partial/unverified, not a verified migration.</p>' : ''}
      ${this.renderProcessLog()}</div>`;
  }

  runKey(run) { return run.runId + (run.attemptId ? '~' + run.attemptId : run.attemptIndex != null ? '~@' + run.attemptIndex : ''); }
  activityTime(run) { return Date.parse(run.lastEventAt || run.finishedAt || run.completedAt || run.startedAt) || 0; }
  kindLabel(run) { return run.activityKind === 'native-rekt' ? 'Native REKT'
    : run.activityKind === 'migration-history' ? 'Migration history (no telemetry)' : 'Model attempt'; }

  saveSelection() {
    try { sessionStorage.setItem('ai-loop-selection', JSON.stringify({ version: 2, followLatest: this.followLatest, selected: this.selected, managedRunId: this.managedRunId })); } catch { }
  }

  async followNewest() {
    this.followLatest = true;
    this.managedRunId = null;
    this.selected = null;
    this.managedStatus = null;
    this.processLog = null;
    this.detail = null;
    this.error = null;
    this.saveSelection();
    this.render();
    await this.loadAndRender();
  }

  renderFreshness() {
    return `<div class="emc-dim" role="status">
      ${this.followLatest ? 'Following newest recorded activity (native, model or managed launch)' : 'Pinned selection · newer work does not replace this history; use Follow newest to resume'}
      · last successful refresh: ${this.esc(this.lastRefreshedAt ? this.when(this.lastRefreshedAt) : 'not yet recorded')}.
      Times shown in ${this.esc(Intl.DateTimeFormat().resolvedOptions().timeZone || 'browser local timezone')}; refresh time is not work time.
      Native REKT scans, graph reads and other work outside an instrumented migration attempt are not model calls.</div>`;
  }

  renderProcessLog() {
    const id = this.detail?.summary.managedRunId || this.managedRunId;
    return `<details class="ail-process-log"><summary>Managed process log${id ? ' · ' + this.esc(id) : ''}</summary>
      ${this.processLog?.length ? `<pre style="max-height:360px;overflow:auto;white-space:pre-wrap">${this.esc(this.processLog.join('\n'))}</pre>`
        : '<p>No managed process log is available for this attempt. Historical logs cannot be reconstructed.</p>'}
      ${id && this.managedStatus?.logUrl ? `<a href="${this.esc(this.managedStatus.logUrl)}?lines=2000" target="_blank" rel="noopener">Read recorded log tail (up to 2000 lines)</a>` : ''}
      <p class="emc-dim">Bounded tail only; full recorded logs remain on disk under Logs/portal. Output is shown only when explicitly associated with this launch.</p></details>`;
  }

  render() {
    const root = this.root;
    if (!root) return;
    this.captureDisplayState();
    const selection = this.managedRunId ? 'managed:' + this.managedRunId : this.selected;
    const state = this.displayStates.get(selection);
    const animationTimes = this.renderedSelection === selection
      ? (root.getAnimations?.({ subtree: true }) || []).map(animation => animation.currentTime) : [];
    if (!this.runs.length && !this.managedRunId && !this.managedRuns.length) {
      root.innerHTML = `
        <div class="ail"><h2 class="emc-title">AI Loop</h2>
          ${this.renderFreshness()}
          ${this.error ? `<div role="alert">${this.esc(this.error)}</div>` : ''}
          <div class="mc-empty">No runs have recorded AI loop events yet. Start a conversion from Mission Control
          or Estate Mission Control, or run <code>./doctor.sh convert-only</code>; it appears here as it runs.</div></div>`;
      return;
    }

    root.innerHTML = `
      <div class="ail">
        <div class="emc-head">
          <div>
            <h2 class="emc-title">AI Loop</h2>
            <div class="emc-sub">Native REKT activity, model attempts and managed launches · recorded evidence, not inferred success</div>
            <div class="emc-dim">Start conversions using the configured Mission Control or Estate Mission Control controls.</div>
          </div>
          <div style="display:flex;gap:8px;align-items:center">
            <select class="mc-select" id="ail-run">
              ${this.managedRunId && !this.detail
                && !this.managedRuns.some(r => r.runId === this.managedRunId)
                ? `<option value="" selected>Managed run ${this.esc(this.managedRunId)} · awaiting telemetry</option>` : ''}
              ${this.selected && !this.runs.some(r => this.runKey(r) === this.selected)
                ? `<option value="${this.esc(this.selected)}" selected>Run ${this.esc(this.selected)}${this.managedRunId ? ' · managed ' + this.esc(this.managedRunId) : ''}</option>` : ''}
              ${this.managedRuns.filter(r => !this.runs.some(m => m.managedRunId === r.runId)).map(r => `
                <option value="managed:${this.esc(r.runId)}" ${r.runId === this.managedRunId ? 'selected' : ''}>
                  Managed ${this.esc(r.runId)} · process ${this.esc(r.status)} · ${this.esc(this.when(r.startedAt))}
                </option>`).join('')}
              ${this.runs.map(r => `
              <option value="${this.esc(this.runKey(r))}" ${this.runKey(r) === this.selected ? 'selected' : ''}>
                [${this.kindLabel(r)}] ${this.esc(r.activityKind === 'native-rekt' ? r.activityId : 'Run ' + r.runId)}${r.attemptId ? ' · attempt ' + this.esc(r.attemptId.slice(0, 8)) : ''} · ${this.esc(r.status)}${r.telemetryAvailable === false ? ' · telemetry not recorded' : ''} · latest ${this.esc(this.when(r.lastEventAt || r.finishedAt || r.startedAt))}
              </option>`).join('')}
            </select>
            <button class="emc-btn" data-act="refresh">↻</button>
            <button class="emc-btn" data-act="latest" ${this.followLatest ? 'disabled' : ''}>Follow newest</button>
            ${this.hasMore ? '<button class="emc-btn" data-act="more">Load older runs</button>' : ''}
          </div>
          ${this.renderFreshness()}
        </div>
        ${this.error ? `<div role="alert" class="emc-warn">${this.esc(this.error)}</div>` : ''}
        ${this.detail ? this.renderDetail(this.detail) : this.managedRunId ? this.renderPending()
          : `<div class="mc-empty">${this.esc(this.error || 'Loading…')}</div>`}
      </div>`;

    root.querySelector('#ail-run')?.addEventListener('change', ev => {
      const value = ev.target.value;
      if (value.startsWith('managed:')) this.selectManagedRun(value.slice('managed:'.length));
      else if (value) this.select(value);
    });
    root.querySelector('[data-act="refresh"]')?.addEventListener('click', () => this.loadAndRender());
    root.querySelector('[data-act="latest"]')?.addEventListener('click', () => this.followNewest());
    root.querySelector('[data-act="more"]')?.addEventListener('click', () => {
      if (!this.historyOffsets.includes(this.nextOffset)) this.historyOffsets.push(this.nextOffset);
      this.loadAndRender();
    });
    root.querySelectorAll('[data-ail-attempt]').forEach(button => button.addEventListener('click', () => this.select(button.dataset.ailAttempt)));
    root.querySelector('#ail-native-filter')?.addEventListener('change', event => {
      this.nativeProgramFilter = event.target.value;
      this.render();
    });
    [...root.querySelectorAll('details')].forEach((el, index) => {
      el.open = state?.details[el.id || el.dataset?.ailDetail || 'detail-' + index] || false;
    });
    for (const scroll of state?.scroll || []) {
      const element = [...root.querySelectorAll('[data-ail-scroll]')].find(el => el.dataset.ailScroll === scroll.key);
      if (element) { element.scrollTop = scroll.top; element.scrollLeft = scroll.left; }
    }
    this.renderedSelection = selection;
    this.renderedHasDetail = !!this.detail;
    root.querySelector('#ail-native-log')?.addEventListener('toggle', () => {
      if (root.querySelector('#ail-native-log')?.open) this.refreshNativeLog();
      else this.cancelNativeLog();
    });
    this.syncMotion();
    (root.getAnimations?.({ subtree: true }) || []).forEach((animation, index) => {
      if (animationTimes[index] != null) animation.currentTime = animationTimes[index];
    });
    this.refreshNativeLog();
  }

  renderDetail(d) {
    if (d.summary.activityKind === 'native-rekt') return this.renderNative(d);
    const s = d.summary;
    const suppliedTimes = [...d.events, ...d.contextHandoffs].map(e => e.ts).filter(t => t && !isNaN(new Date(t)));
    const lastEventAt = s.lastEventAt === undefined
      ? suppliedTimes.sort((a, b) => new Date(b) - new Date(a))[0] : s.lastEventAt;
    return `
      ${d.warning ? `<div class="emc-warn">${this.esc(d.warning)}</div>` : ''}
      ${s.failureReason ? `<div role="alert" class="emc-warn">${this.esc(s.failureReason)}</div>` : ''}
      ${this.managedStatus?.error ? `<div class="emc-warn">Process: ${this.esc(this.managedStatus.error)}</div>` : ''}
      ${this.managedStatus?.historyWarning ? `<div class="emc-warn">${this.esc(this.managedStatus.historyWarning)}</div>` : ''}
      ${d.relatedAttempts?.length > 1 ? `<details><summary>Recorded attempt history (${d.relatedAttempts.length})</summary>
        ${d.relatedAttempts.map(a => `<button class="emc-btn" data-ail-attempt="${this.esc(this.runKey(a))}">Run ${this.esc(a.runId)} · ${this.esc(a.attemptId || 'ID not recorded')} · ${this.esc(a.status)}</button>`).join('')}</details>` : ''}
      <div class="ail-meta">Run ${this.esc(s.runId)} · attempt ${this.esc(s.attemptId || 'not recorded')}
        ${s.managedRunId ? ` · managed run ${this.esc(s.managedRunId)}` : ''}<br>
        Started: ${this.esc(this.when(s.startedAt))} · finished: ${this.esc(this.when(s.finishedAt))}<br>
        Last recorded activity: ${this.esc(this.when(lastEventAt))}
        (${s.lastEventAt === undefined ? 'latest supplied record; complete telemetry metadata requires the updated backend' : 'not the polling time'}).<br>
        Request coverage: ${this.esc(s.requestCoverage || 'legacy-outcomes-only')}.
        Counts cover instrumented agent invocations, not provider-internal HTTP retries or uninstrumented tools.
        ${s.startedRequests == null ? 'Request starts and historical in-flight state were not recorded.' : ''}</div>
      <div class="ail-loop-layout">
        ${this.renderLoop(d.stages, s, d.gates, d.requests)}
        <section class="ail-history" tabindex="0" role="region" aria-label="Outstanding requests and context handoffs">
          <h3>Outstanding requests</h3>${this.renderRequests(d.requests, s)}
          <h3>Context handoffs</h3>${this.renderContexts(d.contextHandoffs)}</section>
      </div>
      <div class="emc-kpis">
        ${this.kpi(this.statusLabel(s.status), 'status', ['failed', 'interrupted', 'terminated'].includes(s.status))}
        ${this.kpi(s.durationMs != null ? this.dur(s.durationMs) : (s.currentStage || '—'), s.durationMs != null ? 'duration' : 'current stage')}
        ${this.kpi(s.startedRequests ?? 'not recorded', 'requests started')}
        ${this.kpi(s.telemetryAvailable === false ? 'not recorded' : s.calls, 'recorded outcomes')}
        ${this.kpi(s.telemetryAvailable === false ? 'not recorded' : s.failedCalls, 'failed calls', s.failedCalls > 0)}
        ${this.kpi(s.telemetryAvailable === false ? 'not recorded' : s.cancelledCalls ?? 0, 'cancelled calls')}
        ${this.kpi(s.telemetryAvailable === false ? 'not recorded' : s.unknownCalls ?? 0, 'unknown outcomes')}
          ${this.kpi(s.telemetryAvailable === false ? 'not recorded' : s.retries, 'recorded retries', s.retries > 0)}
          ${this.kpi(s.telemetryAvailable === false ? 'not recorded' : s.fallbacks, 'recorded fallbacks', s.fallbacks > 0)}
      </div>
      <div class="emc-section-title">Quality gates</div>
      <p class="emc-dim">These are recorded artifact checks, not proof of behavioral equivalence.
        A completed process or recorded stage does not establish a passing gate.</p>
      <div class="ail-gates">${d.gates.map(g => this.renderGate(g)).join('')}</div>
      <div class="emc-section-title">Stages</div>
      ${this.renderStages(d.stages, s)}
      <div class="emc-section-title">Agents</div>
      ${this.renderAgents(d.agents)}
      <div class="emc-section-title">Live timeline <span class="emc-dim">· newest first · bounded recorded events</span></div>
      ${d.telemetry ? `<p class="emc-dim">${this.esc(d.telemetry.recordedEvents)} structured records in this attempt;
        showing ${d.events.length} of ${this.esc(d.telemetry.timelineEvents)} timeline events and
        ${d.contextHandoffs.length} of ${this.esc(d.telemetry.contextEvents)} context records.
        Display limits do not delete JSONL history. Unreadable lines in file: ${this.esc(d.telemetry.unreadableLines)};
        invalid/unrecorded timestamps: ${this.esc(d.telemetry.invalidTimestamps)}.</p>` : ''}
      ${this.renderEvents(d.events)}
      ${this.renderProcessLog()}
      <details><summary>Recorded logs and artifacts</summary>
        ${d.logs?.length ? d.logs.map(log => `<div><code>${this.esc(log.path)}</code> · ${this.esc(log.kind)}
          ${log.messages != null ? ' · ' + this.esc(log.messages) + ' matching messages' : ''}
          · ${log.available ? 'available on disk' : 'recorded path is no longer available'}</div>`).join('')
          : '<p>No correlated structured/model log export was recorded.</p>'}
        ${d.gates.filter(g => g.source).map(g => `<div>${this.esc(g.title)}: <code>${this.esc(g.source)}</code></div>`).join('')}
        <p class="emc-dim">Model exports may contain source code and prompts and remain on disk; their contents are not copied into telemetry.
        Older exports without recorded correlation cannot be assigned to this attempt. Abruptly stopped processes may not have exported conversation logs.</p>
      </details>
      ${d.outputFolder ? `<div class="emc-dim" style="margin-top:10px">Output: <code>${this.esc(d.outputFolder)}</code></div>` : ''}`;
  }

  renderNative(d) {
    const s = d.summary, n = d.native || {};
    const completed = (n.programs || []).filter(p => p.finishedAt).length;
    const inFlight = (n.programs || []).filter(p => p.startedAt && !p.finishedAt).length;
    const unmeasuredArtifacts = (n.programs || []).filter(p => [p.hasAst, p.hasCfg, p.hasData].some(v => v == null)).length;
    const rows = (n.programs || []).filter(p => this.nativeProgramFilter === 'failures'
      ? p.status.toLowerCase() === 'failed' || p.structureExitCode > 0 || [p.hasAst, p.hasCfg, p.hasData].includes(false)
      : this.nativeProgramFilter === 'dependencies' ? p.strictDependencyComplete === false : true);
    const explain = issue => `<p><strong>${this.esc(window.ConversionReadiness?.diagnosticLabel?.(issue.category) || 'Recorded dependency condition')}</strong>
      ${this.esc(issue.item || '')}<br>${this.esc(issue.reason)}<br><strong>Next:</strong> ${this.esc(issue.action)}</p>`;
    return `<section aria-label="Native REKT activity">
      <h3><span class="emc-badge">Native REKT</span> ${this.esc(n.recordedName || s.activityId)}</h3>
      <div class="ail-meta">Status: ${this.esc(s.status)} · started ${this.esc(this.when(s.startedAt))}
        · finished ${this.esc(this.when(s.finishedAt))}<br>
        Operation: ${this.esc(s.mode || 'native-rekt')}<br>
        Latest recorded activity: ${this.esc(this.when(s.lastEventAt))} · not polling time<br>
        Activity identity: <code>${this.esc(s.activityId)}</code> · catalogue locator: <code>${this.esc(s.runId)}</code><br>
        ${n.graphSnapshotId ? 'Recorded graph snapshot: ' + this.esc(n.graphSnapshotId) : 'No graph snapshot identity recorded in this scan manifest.'}
        Migration database IDs and graph snapshot IDs are separate namespaces.</div>
      ${d.warning ? `<p class="emc-warn">${this.esc(d.warning)}</p>` : ''}
      ${s.failureReason ? `<p role="alert">${this.esc(s.failureReason)}</p>` : ''}
      <p>Model calls: not applicable (native tools; no model attempt or migration is fabricated).</p>
      <div class="ail-loop-layout">
      ${this.renderLoop(d.stages, s, [], [], true)}
      <section><h3>Native stages and program progress</h3>${this.renderStages(d.stages, s)}
      <p>${this.esc(n.recordedPrograms ?? 0)} program records / ${this.esc(n.expectedPrograms ?? 'scope not recorded')}
        · ${completed} with recorded finish · ${inFlight} with recorded start and no finish
        · ${this.esc(n.structuralPrograms ?? 0)} with recorded AST + CFG + data flags
        · ${this.esc(n.failedPrograms ?? 0)} recorded structural failures.
        Native completion does not prove dependency completeness, migration acceptance or equivalence.</p></section></div>
      ${unmeasuredArtifacts ? `<p class="emc-dim">${this.esc(unmeasuredArtifacts)} program records have unrecorded artifact flags. This does not establish that their AST, CFG or data artifacts are absent.</p>` : ''}
      ${n.truncated ? '<p class="emc-warn">Program display is bounded; consult the original manifest for complete results.</p>' : ''}
      <label>Program results <select id="ail-native-filter">
        ${[['all', 'All recorded programs'], ['failures', 'Structural failures'], ['dependencies', 'Dependency gaps']].map(([value, label]) =>
          `<option value="${value}" ${this.nativeProgramFilter === value ? 'selected' : ''}>${label}</option>`).join('')}
      </select></label>
      <div data-ail-scroll="native-programs" style="max-height:480px;overflow:auto"><table><thead><tr><th>Program</th><th>Recorded status</th><th>AST / CFG / data</th><th>Strict dependencies</th><th>Structure / dependency exit</th><th>Started / finished / updated</th></tr></thead><tbody>
      ${rows.map(p => `<tr><td>${this.esc(p.relativePath)}
        ${(p.dependencyIssues || []).length ? `<details open data-ail-detail="dependency:${this.esc(p.relativePath)}"><summary>What needs attention</summary>
          ${(p.dependencyIssues || []).slice(0, 3).map(explain).join('')}
          ${p.dependencyIssues.length > 3 ? `<details><summary>All ${p.dependencyIssues.length} recorded conditions</summary>${p.dependencyIssues.map(explain).join('')}</details>` : ''}
          ${p.dependencyDiagnosticsTruncated ? '<p>Diagnostic display is truncated; inspect the original log for all conditions.</p>' : ''}
        </details>` : ''}
        ${window.ConversionReadiness?.inputs ? window.ConversionReadiness.inputs(p, '', true) : ''}
        ${p.measurementReasons && Object.keys(p.measurementReasons).length
        ? `<details data-ail-detail="measurement:${this.esc(p.relativePath)}"><summary>Raw measurement notes</summary>${Object.entries(p.measurementReasons).map(([key, value]) =>
          `<div>${this.esc(key)}: ${this.esc(value)}</div>`).join('')}</details>` : ''}</td><td>${this.esc(p.status)}</td>
        <td>${[p.hasAst, p.hasCfg, p.hasData].map(v => v == null ? 'not recorded' : v ? 'yes' : 'no').join(' / ')}</td>
        <td>${p.strictDependencyComplete == null ? 'not recorded' : p.strictDependencyComplete ? 'complete' : 'incomplete'}</td>
        <td>${this.esc(p.structureExitCode ?? 'not recorded')} / ${this.esc(p.dependencyExitCode ?? 'not recorded')}</td>
        <td>${this.esc(this.when(p.startedAt))} / ${this.esc(this.when(p.finishedAt))} / ${this.esc(this.when(p.updatedAt))}</td></tr>`).join('')}</tbody></table></div>
      <details id="ail-native-log"><summary>Original native operation log · bounded read-only tail</summary>
        <p class="emc-dim">Only this scan's original native.log. Read while expanded on normal activity refresh; full logs remain on disk. No model conversation or other source/configuration files are opened.</p>
        <div id="ail-native-log-body">${this.renderNativeLogBody(s.runId)}</div>
      </details>
      <details id="ail-native-evidence"><summary>Original evidence paths</summary>
      <p class="emc-dim">References only; evidence files are not opened by this viewer.</p>
      ${(d.logs || []).map(log => `<div><code>${this.esc(log.path)}</code> · ${this.esc(log.kind)} · ${log.available ? 'available' : 'not available'}</div>`).join('')}
      </details>
      </section>`;
  }

  renderLoop(stages, summary, gates = [], requests = [], native = false) {
    const phases = native ? [...new Set(stages.map(stage => stage.name))].map(name => [name, null]) : [
      ['Discover', /discover|initializ/i], ['Analyze', /analy|dependenc|reverse|context/i],
      ['Convert', /convert|conversion|chunk/i], ['Test', /test/i],
      ['Compile', /compil|build|repair/i], ['Parity', /parity|validat|reconcil/i],
      ['Report', /report|document|complet/i]
    ];
    const phaseIndex = name => phases.findLastIndex(([label, pattern]) => native
      ? label === name || (name || '').startsWith(label + ': ') : pattern.test(name || ''));
    const active = phaseIndex(summary.currentStage);
    const running = summary.status === 'running';
    const live = running && !this.error;
    const ended = stages.filter(s => s.durationMs != null).length;
    const observed = phases.filter((_, i) => stages.some(s => phaseIndex(s.name) === i)).length;
    const inFlight = running && summary.startedRequests != null
      ? requests.filter(r => r.status === 'in-flight') : [];
    const activity = this.error ? 'Refresh unavailable · displayed activity is stale'
      : running ? `Current recorded stage: ${summary.currentStage || 'not recorded'}`
        : `Run ${summary.status || 'unknown'} · ${summary.currentStage ? 'last recorded stage: ' + summary.currentStage : 'stage not recorded'}`;
    const descriptions = [];
    const nodes = phases.map(([label], i) => {
      const angle = (-90 + i * 360 / phases.length) * Math.PI / 180;
      const x = 170 + 115 * Math.cos(angle), y = 160 + 115 * Math.sin(angle);
      const records = stages.filter(s => phaseIndex(s.name) === i);
      const current = running && i === active;
      const gate = gates.find(g => g.id === ({ Compile: 'compile', Parity: 'parity' }[label]));
      const gateState = ['pass', 'fail', 'warn'].includes(gate?.status) ? gate.status : null;
      const state = current ? (live ? 'active' : 'stale') : records.length
        ? records.every(s => s.durationMs != null) ? 'ended' : 'recorded' : 'missing';
      const stateLabel = { active: 'Now', stale: 'Stale', ended: 'Ended', recorded: 'Recorded', missing: 'Not recorded' }[state];
      const gateLabel = { pass: '✓ Gate passed', fail: '✕ Gate failed', warn: '! Gate warning' }[gateState];
      const description = `${label}: ${current ? 'current recorded stage' : stateLabel.toLowerCase()}${gateLabel ? ' · ' + gateLabel : ''}`;
      descriptions.push(description);
      return `<g class="ail-phase ail-phase-${state}${gateState ? ' ail-phase-gate-' + gateState : ''}">
        <title>${this.esc(description)}</title>
        ${current ? `<circle class="ail-phase-halo" cx="${x}" cy="${y}" r="33"></circle>` : ''}
        <circle cx="${x}" cy="${y}" r="27"></circle>
        <text x="${x}" y="${y - 2}" text-anchor="middle">${this.esc(label)}</text>
        <text class="ail-phase-state" x="${x}" y="${y + 12}" text-anchor="middle">${this.esc(stateLabel)}</text>
        ${gateLabel ? `<text class="ail-phase-gate-label" x="${x}" y="${y + 43}" text-anchor="middle">${this.esc(gateLabel)}</text>` : ''}</g>`;
    }).join('');
    return `<section class="ail-loop"><svg viewBox="0 0 340 320" role="img" aria-label="${this.esc((native ? 'Recorded native stages. ' : 'Recorded conversion phases. ') + descriptions.join('. '))}">
      <circle class="ail-ring" cx="170" cy="160" r="115"></circle>${nodes}
      <text class="ail-loop-status ${['failed', 'terminated'].includes(summary.status) ? 'ail-status-failed' : ''}" x="170" y="155" text-anchor="middle">${this.esc(this.statusLabel(summary.status))}</text>
      <text x="170" y="177" text-anchor="middle">${native ? 'Native REKT' : 'AI Loop'}</text></svg>
      <div class="ail-activity" role="status" aria-live="polite"><span class="ail-activity-dot" aria-hidden="true"></span>
        ${this.esc(activity)}</div>
      ${live && !native ? `<p class="ail-request-activity">${inFlight.length
        ? `${inFlight.length} instrumented request${inFlight.length === 1 ? '' : 's'} in flight · ${inFlight.slice(0, 3).map(r => this.esc(r.agent || 'unknown agent')).join(', ')}${inFlight.length > 3 ? ' …' : ''}`
        : summary.startedRequests == null ? 'Request activity not recorded for this attempt.'
          : 'No instrumented request currently recorded; this stage may include uninstrumented work.'}</p>` : ''}
      <p class="ail-progress">${observed} of ${phases.length} phases observed · ${ended} of ${stages.length} recorded stage intervals ended</p>
      <div class="ail-legend"><span>◉ Now = current recorded phase</span><span>— Ended = observed interval ended, not proof of success</span>
        <span>○ Not recorded = no stage evidence, not queued or skipped</span>${native ? '' : '<span>✓ / ✕ Gate passed / failed = recorded artifact only; warning is not a pass</span>'}</div>
      <p class="emc-dim">${native ? 'Only stages recorded by this native operation are shown. No model requests, conversion phases or passing gates are inferred.'
        : 'Phase names group recorded stage labels, not parser provenance or authoritative discovery. Stages can repeat. Run completion is not a quality pass; green is reserved for a recorded passing gate.'}</p></section>`;
  }

  renderRequests(requests, summary) {
    if (summary.startedRequests == null) return '<p>Not recorded for this attempt.</p>';
    if (!requests?.length) return '<p>No outstanding recorded requests.</p>';
    return `<ul>${requests.map(r => `<li><b>${this.esc(r.agent || 'unknown')}</b> · ${this.esc(r.context)}
      · ${this.esc(r.status)} · ${this.esc(this.time(r.startedAt))}
      <code>${this.esc(r.requestId)}</code></li>`).join('')}</ul>
      <p class="emc-dim">An unmatched start after the attempt ends is unknown, not still running.</p>`;
  }

  renderContexts(contexts) {
    if (!contexts?.length) return '<p>Context preparation and dispatch not recorded.</p>';
    return `<ul>${contexts.slice().reverse().map(c => `<li>${this.esc(c.agent)} → ${this.esc(c.file)} ·
      ${this.esc(c.mode || 'unknown')} · <b>${this.esc(c.state)}</b> · ${this.esc(c.execution)} · ${this.esc(this.time(c.ts))}
      ${c.requestId ? `<code>${this.esc(c.requestId)}</code>` : ''}</li>`).join('')}</ul>
      <p class="emc-dim">Prepared does not mean dispatched. Reused structural artifacts do not imply a parser ran.
      Dispatch requires an exact prepared block and context identity in an instrumented agent request.</p>`;
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
    return `<p class="emc-dim">Bars compare recorded stage durations, not completion.</p><div class="ail-stages">${stages.map((x, i) => {
      const current = x.durationMs == null && s.status === 'running' && i === stages.length - 1;
      const running = current && !this.error;
      const pct = x.durationMs != null ? (x.durationMs / known) * 100 : 0;
      return `
        <div class="ail-stage" title="${this.esc(x.name)}">
          <span class="ail-stage-name">${x.total > 0 && x.number < 99 ? `${x.number}/${x.total} ` : ''}${this.esc(x.name)}</span>
          <div class="emc-bar"><div class="emc-bar-fill ail-duration" style="width:${pct}%"></div></div>
          <span class="ail-stage-dur ${running ? 'ail-stage-current' : ''}">${current ? this.error ? 'stale' : 'current' : x.durationMs != null ? `${this.dur(x.durationMs)} · ended` : 'end not recorded'}</span>
        </div>`;
    }).join('')}</div>`;
  }

  renderAgents(agents) {
    if (!agents.length) return '<div class="emc-dim">No model calls recorded.</div>';
    const tok = a => a.inputTokens != null
      ? `${this.num(a.inputTokens)} / ${this.num(a.outputTokens)} (${a.callsWithTokenUsage ?? '?'} calls reported)`
      : '<span class="emc-dim">not recorded</span>';
    return `
      <div class="ail-table-wrap"><table class="ail-table">
        <thead><tr><th>Agent</th><th>Starts / outcomes</th><th>Succeeded / cancelled / unknown</th><th>Failed</th><th>Retries</th><th>Fallbacks</th><th>Avg</th><th>p95</th><th>Reported tokens in / out</th></tr></thead>
        <tbody>${agents.map(a => `
          <tr>
            <td><b>${this.esc(a.agent)}</b>${a.models.length ? `<div class="emc-dim">${a.models.map(m => this.esc(m)).join(', ')}</div>` : ''}</td>
            <td>${a.startedRequests ?? 'not recorded'} / ${a.calls}</td>
            <td>${a.succeededCalls ?? 'not recorded'} / ${a.cancelledCalls ?? 'not recorded'} / ${a.unknownCalls ?? 'not recorded'}</td>
            <td class="${a.failedCalls ? 'emc-bad' : ''}">${a.failedCalls}</td>
            <td class="${a.retries ? 'emc-mid' : ''}">${a.retries}</td>
            <td class="${a.fallbacks ? 'emc-bad' : ''}">${a.fallbacks}</td>
            <td>${this.dur(a.avgMs)}<div class="emc-dim">${a.callsWithDuration ?? '?'} calls measured</div></td>
            <td>${this.dur(a.p95Ms)}</td>
            <td>${tok(a)}</td>
          </tr>`).join('')}
        </tbody></table></div>`;
  }

  renderEvents(events) {
    if (!events.length) return '<div class="emc-dim">No timeline events recorded.</div>';
    const label = { llm_retry: 'retry', llm_fallback: 'fallback', llm_call: 'outcome', llm_request_started: 'request start',
      run_started: 'attempt started', run_finished: 'attempt finished', stage: 'stage', validation_completed: 'output policy',
      chat_log_saved: 'conversation export saved' };
    return `<div class="ail-events">${events.slice().reverse().map(e => `
      <div class="ail-event ail-ev-${this.esc(e.kind)}">
        <span class="emc-loc" title="${this.esc(e.ts)}">${this.esc(this.time(e.ts))}</span>
        <span class="emc-kind">${this.esc(label[e.kind] || e.kind)}${e.iteration != null ? ' · attempt ' + this.esc(e.iteration) : ''}</span>
        <b>${this.esc(e.agent || '')}</b>
        <span class="emc-dim">${this.esc(e.context || '')}</span>
        ${e.requestId ? `<code>${this.esc(e.requestId)}</code>` : ''}
        <div class="emc-src"><code>${this.esc(e.reason || '')}</code></div>
      </div>`).join('')}</div>`;
  }

  kpi(value, label, warn = false) {
    return `<div class="emc-kpi${warn ? ' emc-kpi-warn' : ''}"><div class="emc-kpi-num">${this.esc(value)}</div><div class="emc-kpi-label">${this.esc(label)}</div></div>`;
  }

  statusLabel(s) { return { completed: 'done', running: 'running', failed: 'failed', interrupted: 'interrupted', terminated: 'terminated', cancelled: 'cancelled', unknown: 'unknown', no_files: 'no files' }[s] || s; }
  dur(ms) {
    if (ms == null) return 'not recorded';
    if (ms < 1000) return `${ms} ms`;
    const s = ms / 1000;
    if (s < 60) return `${s.toFixed(1)} s`;
    const m = Math.floor(s / 60);
    return m < 60 ? `${m}m ${Math.round(s % 60)}s` : `${Math.floor(m / 60)}h ${m % 60}m`;
  }
  num(n) { return Number(n ?? 0).toLocaleString(); }
  when(iso) { const d = new Date(iso); return !iso || isNaN(d) || d.getUTCFullYear() < 2 ? 'not recorded' : d.toLocaleString(undefined, { timeZoneName: 'short' }); }
  time(iso) { return this.when(iso); }
  esc(s) {
    return String(s ?? '').replace(/[&<>"']/g, c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
  }
}

window.AiLoopView = AiLoopView;
