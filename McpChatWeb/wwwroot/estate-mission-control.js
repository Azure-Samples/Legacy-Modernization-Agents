// Estate Mission Control — the estate as migration waves of convertible slices.
//
// The graph behind it is deterministic: every edge is a CALL, COPY, EXEC SQL, EXEC CICS, JCL step
// or DD statement read from the source, and the node panel shows the line it came from. Clusters
// are programs that move together; waves order them so callees convert before their callers. A
// slice is what converting one cluster takes, and Convert starts exactly that as a run.

class EstateMissionControlView {
  constructor(rootId) {
    this.rootId = rootId;
    this.summary = null;
    this.selected = null;
    this.slice = null;
    this.network = null;
    this.error = null;
    this.includeNeeds = true;
  }

  get root() { return document.getElementById(this.rootId); }

  async loadAndRender(force = false) {
    if (!this.summary || force) await this.load();
    this.render();
  }

  async load() {
    this.error = null;
    try {
      const resp = await fetch('/api/estate/summary');
      if (!resp.ok) throw new Error(`HTTP ${resp.status}`);
      this.summary = await resp.json();
      const clusters = this.summary.clusters || [];
      if (!clusters.some(c => c.id === this.selected)) this.selected = clusters[0]?.id ?? null;
    } catch (e) {
      console.error('Estate summary failed:', e);
      this.error = `The estate graph could not be loaded (${e.message}).`;
    }
  }

  cluster(id) { return (this.summary?.clusters || []).find(c => c.id === id); }

  render() {
    const root = this.root;
    if (!root) return;
    if (this.error || !this.summary) {
      root.innerHTML = `<div class="mc-empty">${this.esc(this.error || 'Loading estate…')}</div>`;
      return;
    }
    const s = this.summary;
    const counts = s.counts || {};
    const inSource = (s.clusters || []).reduce((n, c) => n + c.programs.length, 0);

    root.innerHTML = `
      <div class="emc">
        <div class="emc-head">
          <div>
            <h2 class="emc-title">Estate Mission Control</h2>
            <div class="emc-sub">Deterministic estate graph · clusters move together · waves convert callees first · no model involved</div>
          </div>
          <button class="emc-btn" data-act="refresh" title="Rebuild from the source folder">↻ Refresh</button>
        </div>
        ${s.warning ? `<div class="emc-warn">${this.esc(s.warning)}</div>` : ''}
        <div class="emc-kpis">
          ${this.kpi(inSource, 'programs in source')}
          ${this.kpi(counts.missing ?? 0, 'referenced, not in source', (counts.missing ?? 0) > 0)}
          ${this.kpi((s.clusters || []).filter(c => !c.standalone).length, 'clusters')}
          ${this.kpi((s.waves || []).length, 'waves')}
          ${this.kpi(counts.job ?? 0, 'JCL jobs')}
          ${this.kpi((s.hubs || []).length, 'hubs')}
        </div>
        <div class="emc-body">
          <div class="emc-left">
            ${(s.waves || []).map(w => this.renderWave(w)).join('') || '<div class="mc-empty">No programs found in the source folder.</div>'}
            ${this.renderHubs()}
            ${this.renderDiagnostics()}
          </div>
          <div class="emc-right" id="emc-detail">${this.renderDetail()}</div>
        </div>
      </div>`;

    root.querySelector('[data-act="refresh"]')?.addEventListener('click', () => this.loadAndRender(true));
    root.querySelectorAll('[data-cluster]').forEach(el =>
      el.addEventListener('click', () => this.select(el.dataset.cluster)));
    root.querySelectorAll('[data-node]').forEach(el =>
      el.addEventListener('click', () => this.showNode(el.dataset.node)));
    this.wireDetail();
  }

  kpi(value, label, warn = false) {
    return `<div class="emc-kpi${warn ? ' emc-kpi-warn' : ''}"><div class="emc-kpi-num">${this.esc(value)}</div><div class="emc-kpi-label">${this.esc(label)}</div></div>`;
  }

  renderWave(w) {
    const cycles = (w.cycles || []).map(c =>
      `<div class="emc-cycle" title="These clusters call each other, so they share a wave and must convert together">⟳ ${c.map(x => this.esc(x)).join(' ⇄ ')}</div>`).join('');
    return `
      <div class="emc-wave">
        <div class="emc-wave-head">Wave ${this.esc(w.number)} <span class="emc-dim">· ${w.clusters.length} cluster${w.clusters.length === 1 ? '' : 's'}</span></div>
        ${cycles}
        ${w.clusters.map(id => this.renderClusterCard(this.cluster(id))).join('')}
      </div>`;
  }

  renderClusterCard(c) {
    if (!c) return '';
    const sel = c.id === this.selected ? ' emc-card-sel' : '';
    return `
      <div class="emc-card${sel}" data-cluster="${this.esc(c.id)}">
        <div class="emc-card-top">
          <span class="emc-id">${this.esc(c.id)}</span>
          <span class="emc-label">${this.esc(c.label)}</span>
          <span class="emc-score ${this.scoreClass(c.carveScore)}" title="Carve score: how cleanly this cluster comes away">${this.esc(c.carveScore.toFixed(0))}</span>
        </div>
        <div class="emc-bar"><div class="emc-bar-fill ${this.scoreClass(c.carveScore)}" style="width:${Math.max(0, Math.min(100, c.carveScore))}%"></div></div>
        <div class="emc-card-meta">
          ${c.programs.length} program${c.programs.length === 1 ? '' : 's'}
          ${c.jobs.length ? ` · ${c.jobs.length} job${c.jobs.length === 1 ? '' : 's'}` : ''}
          ${c.missing.length ? ` · <span class="emc-miss">${c.missing.length} missing</span>` : ''}
          ${c.dependsOn.length ? ` · needs ${c.dependsOn.map(d => this.esc(d)).join(', ')}` : ''}
        </div>
      </div>`;
  }

  renderHubs() {
    const hubs = this.summary.hubs || [];
    if (!hubs.length) return '';
    return `
      <details class="emc-fold">
        <summary>Hubs <span class="emc-dim">· shared by many; convert once, early</span></summary>
        ${hubs.slice(0, 40).map(h => `
          <div class="emc-hub" data-node="${this.esc(h.nodeId)}">
            <span class="emc-kind emc-kind-${this.esc(h.kind)}">${this.esc(h.kind)}</span>
            <span class="emc-hub-name">${this.esc(h.name)}</span>
            <span class="emc-dim">in ${h.fanIn}${h.fanOut ? ` · out ${h.fanOut}` : ''}</span>
          </div>`).join('')}
      </details>`;
  }

  renderDiagnostics() {
    const d = this.summary.diagnostics || [];
    if (!d.length) return '';
    const more = this.summary.diagnosticCount - d.length;
    return `
      <details class="emc-fold">
        <summary>Notes <span class="emc-dim">· ${this.summary.diagnosticCount}</span></summary>
        ${d.map(x => `<div class="emc-note">${this.esc(x)}</div>`).join('')}
        ${more > 0 ? `<div class="emc-dim">…and ${more} more. <code>./doctor.sh estate</code> lists them all.</div>` : ''}
      </details>`;
  }

  // ── selected cluster ──────────────────────────────────────────────────────

  async select(id) {
    this.selected = id;
    this.slice = null;
    this.render();
    this.reveal('emc-detail');
  }

  // In a narrow pane the detail stacks under the list, so a click would otherwise change
  // something off-screen.
  reveal(elementId) {
    const el = document.getElementById(elementId);
    const left = this.root?.querySelector('.emc-left');
    if (el && left && el.getBoundingClientRect().top > left.getBoundingClientRect().top + 10) {
      el.scrollIntoView({ block: 'start', behavior: 'smooth' });
    }
  }

  renderDetail() {
    const c = this.cluster(this.selected);
    if (!c) return '<div class="mc-empty">Select a cluster.</div>';
    const b = c.scoreBreakdown || {};
    const parts = [
      ['cohesion', 'Coupling that stays inside'],
      ['completeness', 'References the source can satisfy'],
      ['independence', 'Calls that stay inside'],
      ['size', 'Fits one conversion slice'],
    ];
    return `
      <div class="emc-detail-head">
        <div>
          <div class="emc-detail-title">${this.esc(c.id)} · ${this.esc(c.label)}</div>
          <div class="emc-dim">Wave ${c.wave}${c.dependedOnBy.length ? ` · needed by ${c.dependedOnBy.map(x => this.esc(x)).join(', ')}` : ''}</div>
        </div>
        <div class="emc-score-big ${this.scoreClass(c.carveScore)}">${this.esc(c.carveScore.toFixed(1))}</div>
      </div>
      <div class="emc-breakdown">
        ${parts.map(([k, label]) => `
          <div class="emc-bd-row" title="${this.esc(label)}">
            <span class="emc-bd-name">${this.esc(k)}</span>
            <div class="emc-bar"><div class="emc-bar-fill ${this.scoreClass((b[k] ?? 0) * 100)}" style="width:${(b[k] ?? 0) * 100}%"></div></div>
            <span class="emc-bd-val">${this.esc(((b[k] ?? 0) * 100).toFixed(0))}%</span>
          </div>`).join('')}
      </div>
      <div class="emc-legend">
        <span><i class="emc-dot" style="background:#3b82f6"></i>program</span>
        <span><i class="emc-dot" style="background:#ef4444"></i>missing</span>
        <span><i class="emc-dot" style="background:#a855f7"></i>copybook</span>
        <span><i class="emc-dot" style="background:#f59e0b"></i>job</span>
        <span><i class="emc-dot" style="background:#10b981"></i>table / dataset</span>
        <span class="emc-dim">click a node for the source lines behind it</span>
      </div>
      <div id="emc-graph" class="emc-graph"></div>
      <div id="emc-node" class="emc-node"></div>
      <div id="emc-slice" class="emc-slice"><div class="emc-dim">Loading slice…</div></div>`;
  }

  async wireDetail() {
    const c = this.cluster(this.selected);
    if (!c) return;
    await Promise.all([this.drawGraph(c.id), this.loadSlice(c.id)]);
  }

  async drawGraph(clusterId) {
    const el = document.getElementById('emc-graph');
    if (!el) return;
    if (typeof vis === 'undefined') {
      el.innerHTML = '<div class="mc-empty">Graph library unavailable (offline?). The slice below still works.</div>';
      return;
    }
    let view;
    try {
      const resp = await fetch(`/api/estate/graph?cluster=${encodeURIComponent(clusterId)}`);
      if (!resp.ok) throw new Error(`HTTP ${resp.status}`);
      view = await resp.json();
    } catch (e) {
      el.innerHTML = `<div class="mc-empty">${this.esc(`Graph unavailable (${e.message}).`)}</div>`;
      return;
    }
    if (this.selected !== clusterId) return;

    const members = new Set(this.cluster(clusterId)?.programs || []);
    const status = this.summary.programs || {};
    const nodes = view.nodes.map(n => {
      const member = members.has(n.id);
      const st = status[n.id];
      const lines = [`${n.kind}: ${n.name}`];
      if (n.file) lines.push(n.file);
      if (!n.inSource) lines.push('Not in the source');
      if (st?.parseFidelity) lines.push(`Parse fidelity: ${st.parseFidelity}`);
      (st?.parity || []).forEach(p => lines.push(`Parity ${p.targetLanguage}: ${p.score != null ? p.score.toFixed(2) : p.outcome}`));
      return {
        id: n.id,
        label: n.name.length > 22 ? n.name.slice(0, 21) + '…' : n.name,
        title: lines.join('\n'),
        color: this.nodeColor(n, member),
        shape: n.kind === 'program' ? 'box' : n.kind === 'job' ? 'diamond' : n.kind === 'copybook' ? 'ellipse' : 'database',
        font: { color: '#e2e8f0', size: member ? 14 : 11 },
        borderWidth: member ? 2 : 1,
        size: 14,
      };
    });
    const edges = view.edges.map((e, i) => ({
      id: i, from: e.from, to: e.to, arrows: 'to',
      title: `${e.kind}${e.via ? ` (${e.via})` : ''} · ${e.evidenceCount} source line${e.evidenceCount === 1 ? '' : 's'}`,
      color: { color: this.edgeColor(e.kind), opacity: 0.8 },
      dashes: e.kind === 'copies',
      width: e.kind === 'calls' || e.kind === 'links' ? 2 : 1,
    }));

    this.network?.destroy();
    this.network = new vis.Network(el, { nodes: new vis.DataSet(nodes), edges: new vis.DataSet(edges) }, {
      physics: { solver: 'forceAtlas2Based', stabilization: { iterations: 200 } },
      interaction: { hover: true, tooltipDelay: 120 },
      layout: { randomSeed: 7 },
    });
    this.network.on('click', p => { if (p.nodes?.length) this.showNode(p.nodes[0]); });
  }

  nodeColor(n, member) {
    if (!n.inSource) return { background: '#7f1d1d', border: '#ef4444' };
    switch (n.kind) {
      case 'program': return member ? { background: '#1e3a8a', border: '#60a5fa' } : { background: '#1e293b', border: '#3b82f6' };
      case 'copybook': return { background: '#3b0764', border: '#a855f7' };
      case 'job': return { background: '#78350f', border: '#f59e0b' };
      case 'transaction': return { background: '#78350f', border: '#fbbf24' };
      default: return { background: '#064e3b', border: '#10b981' };
    }
  }

  edgeColor(kind) {
    return { calls: '#60a5fa', links: '#60a5fa', copies: '#a855f7', runs: '#f59e0b', feeds: '#f59e0b',
      writes: '#f87171', updates: '#f87171', deletes: '#f87171' }[kind] || '#34d399';
  }

  async showNode(id) {
    const el = document.getElementById('emc-node');
    if (!el) return;
    el.innerHTML = '<div class="emc-dim">Loading…</div>';
    this.reveal('emc-node');
    try {
      const resp = await fetch(`/api/estate/node/${encodeURIComponent(id)}`);
      if (!resp.ok) throw new Error(`HTTP ${resp.status}`);
      const d = await resp.json();
      const edgeRows = (list, dir) => list.map(e => `
        <div class="emc-ev">
          <div><span class="emc-kind">${this.esc(e.kind)}</span> ${dir} <b>${this.esc((dir === '→' ? e.to : e.from).replace(/^[a-z]+:/, ''))}</b>${e.via ? ` <span class="emc-dim">via ${this.esc(e.via)}</span>` : ''}</div>
          ${(e.evidence || []).slice(0, 5).map(v => `<div class="emc-src"><span class="emc-loc">${this.esc(v.file)}:${v.line}</span> <code>${this.esc(v.text)}</code></div>`).join('')}
          ${(e.evidence || []).length > 5 ? `<div class="emc-dim">…${e.evidence.length - 5} more</div>` : ''}
        </div>`).join('');
      const st = d.status;
      el.innerHTML = `
        <div class="emc-node-head">
          <span class="emc-kind emc-kind-${this.esc(d.node.kind)}">${this.esc(d.node.kind)}</span>
          <b>${this.esc(d.node.name)}</b>
          ${d.node.file ? `<span class="emc-dim">${this.esc(d.node.file)}</span>` : ''}
          ${!d.node.inSource ? '<span class="emc-miss">not in source</span>' : ''}
          ${d.cluster ? `<span class="emc-dim">· ${this.esc(d.cluster)}</span>` : ''}
          <button class="emc-x" title="Close">×</button>
        </div>
        ${st ? `<div class="emc-dim">Parse fidelity: ${this.esc(st.parseFidelity || 'not parsed')}${st.factsConfidence != null ? ` · facts confidence ${st.factsConfidence}` : ''}${(st.parity || []).map(p => ` · parity ${this.esc(p.targetLanguage)} ${p.score != null ? p.score.toFixed(2) : this.esc(p.outcome)}`).join('')}</div>` : ''}
        ${Object.keys(d.node.attributes || {}).length ? `<div class="emc-dim">${Object.entries(d.node.attributes).map(([k, v]) => `${this.esc(k)}: ${this.esc(v)}`).join(' · ')}</div>` : ''}
        ${edgeRows(d.outgoing, '→')}
        ${edgeRows(d.incoming, '←')}`;
      el.querySelector('.emc-x')?.addEventListener('click', () => { el.innerHTML = ''; });
    } catch (e) {
      el.innerHTML = `<div class="mc-error">${this.esc(`Node unavailable (${e.message}).`)}</div>`;
    }
  }

  // ── slice and conversion ──────────────────────────────────────────────────

  async loadSlice(clusterId) {
    const el = document.getElementById('emc-slice');
    if (!el) return;
    try {
      const resp = await fetch(`/api/estate/cluster/${encodeURIComponent(clusterId)}/slice?includeNeeds=${this.includeNeeds}`);
      if (!resp.ok) throw new Error(`HTTP ${resp.status}`);
      this.slice = await resp.json();
    } catch (e) {
      el.innerHTML = `<div class="mc-error">${this.esc(`Slice unavailable (${e.message}).`)}</div>`;
      return;
    }
    if (this.selected !== clusterId) return;
    this.renderSlice();
  }

  renderSlice() {
    const el = document.getElementById('emc-slice');
    if (!el || !this.slice) return;
    const s = this.slice.slice;
    const strip = id => id.replace(/^[a-z]+:/, '');
    const chips = (list, cls = '') => list.length
      ? list.map(x => `<span class="emc-chip ${cls}">${this.esc(strip(x))}</span>`).join('')
      : '<span class="emc-dim">none</span>';
    const lang = document.getElementById('mc-language-select')?.value || 'Java';
    el.innerHTML = `
      <div class="emc-section-title">Slice</div>
      <div class="emc-slice-grid">
        <div><div class="emc-dim">Programs (${s.programs.length})</div>${chips(s.programs)}</div>
        <div><div class="emc-dim">Also needs: what they call outside the cluster (${s.needs.length})</div>${chips(s.needs)}</div>
        <div><div class="emc-dim">Missing from the source (${s.missing.length})</div>${chips(s.missing, 'emc-chip-miss')}</div>
        <div><div class="emc-dim">JCL jobs that can run end to end (${s.jobs.length})</div>${chips(s.jobs, 'emc-chip-job')}</div>
      </div>
      ${s.missing.length ? '<div class="emc-warn">Missing programs and copybooks convert as stubs or inferred layouts. Add them to the source for a faithful slice.</div>' : ''}
      <div class="emc-cmd">
        <code id="emc-cmd-text">${this.esc(this.slice.command)}</code>
        <button class="emc-btn" data-act="copy" title="Copy to clipboard">Copy</button>
      </div>
      <div class="emc-convert">
        <label><input type="checkbox" id="emc-needs" ${this.includeNeeds ? 'checked' : ''}> include what it calls</label>
        <select id="emc-lang" class="mc-select">
          <option value="Java" ${lang === 'Java' ? 'selected' : ''}>Java</option>
          <option value="CSharp" ${lang === 'CSharp' ? 'selected' : ''}>C#</option>
        </select>
        <button class="emc-btn emc-btn-primary" data-act="convert">▶ Convert slice (${this.slice.selectors.length} program${this.slice.selectors.length === 1 ? '' : 's'})</button>
        <span class="emc-dim">uses the provider and model chosen in Mission Control</span>
      </div>
      <div id="emc-run" class="emc-run"></div>`;

    el.querySelector('[data-act="copy"]')?.addEventListener('click', () =>
      navigator.clipboard?.writeText(this.slice.command));
    el.querySelector('#emc-needs')?.addEventListener('change', ev => {
      this.includeNeeds = ev.target.checked;
      this.loadSlice(this.selected);
    });
    el.querySelector('[data-act="convert"]')?.addEventListener('click', () => this.convert());
  }

  async convert() {
    const out = document.getElementById('emc-run');
    const n = this.slice?.selectors?.length || 0;
    if (!n) return;
    if (!confirm(`Convert ${n} program${n === 1 ? '' : 's'} from ${this.selected}? This starts a model run.`)) return;

    const body = {
      clusterId: this.selected,
      includeNeeds: this.includeNeeds,
      targetLanguage: document.getElementById('emc-lang')?.value || 'Java',
      speedProfile: document.getElementById('mc-speed-select')?.value || 'balanced',
      provider: document.getElementById('mc-provider-select')?.value || 'AzureOpenAI',
      modelId: document.getElementById('mc-model-select')?.value || null,
    };
    const btn = this.root.querySelector('[data-act="convert"]');
    if (btn) btn.disabled = true;
    try {
      const resp = await fetch('/api/estate/slice/convert', {
        method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body),
      });
      const payload = await resp.json().catch(() => ({}));
      if (!resp.ok) throw new Error(payload.error || `HTTP ${resp.status}`);
      out.innerHTML = `<div class="mc-ok">Started run <b>${this.esc(payload.runId)}</b> (${this.esc(payload.name)}) over ${payload.programs.length} program(s). Follow it in Mission Control or the AI Loop tab.</div>`;
    } catch (e) {
      out.innerHTML = `<div class="mc-error">${this.esc(`Could not start the run: ${e.message}`)}</div>`;
    } finally {
      if (btn) btn.disabled = false;
    }
  }

  scoreClass(score) { return score >= 75 ? 'emc-good' : score >= 50 ? 'emc-mid' : 'emc-bad'; }

  esc(s) {
    return String(s ?? '').replace(/[&<>"']/g, c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
  }
}

window.EstateMissionControlView = EstateMissionControlView;
