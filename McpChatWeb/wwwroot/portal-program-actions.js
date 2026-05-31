// ─────────────────────────────────────────────────────────────────────────
// Universal program-action CTAs (#6)
//
// Wherever a COBOL program name appears in the portal (scorecards, service
// chain, locator results, capability hits, wave Kanban, etc.) the user
// should be able to:
//   • Convert that program (Java or C#)
//   • Open it in AST Galaxy filtered to that one program
//
// This helper exposes a single function that takes a program basename and
// renders a compact action menu, plus a launcher for the Convert modal.
// Pages call: <button onclick="PortalProgramActions.menu(this, 'BDSM043.cbl')">⋯ Actions</button>
// ─────────────────────────────────────────────────────────────────────────

window.PortalProgramActions = {
  /** Build inline action buttons (convert + AST) for a single program. */
  buttons(basename, opts = {}) {
    const compact = opts.compact !== false;
    const esc = (s) => String(s || '').replace(/[&<>"']/g, c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
    const b = esc(basename);
    if (compact) {
      return `<span class="ppa-actions" data-program="${b}">
        <button class="ppa-btn ppa-btn-convert" title="Convert ${b} to Java or C#"
                onclick="PortalProgramActions.convert('${b}');">🛠 Convert</button>
        <button class="ppa-btn ppa-btn-ast" title="Open ${b} in AST Galaxy"
                onclick="PortalProgramActions.openAst('${b}');">🌌 AST</button>
        <button class="ppa-btn ppa-btn-scorecard" title="Open ${b} scorecard"
                onclick="PortalProgramActions.openScorecard('${b}');">📦 Scorecard</button>
      </span>`;
    }
    return `<span class="ppa-actions" data-program="${b}">
      <button class="ppa-btn ppa-btn-convert" onclick="PortalProgramActions.convert('${b}');">🛠 Convert ${b}</button>
      <button class="ppa-btn ppa-btn-ast" onclick="PortalProgramActions.openAst('${b}');">🌌 Open in AST Galaxy</button>
      <button class="ppa-btn ppa-btn-scorecard" onclick="PortalProgramActions.openScorecard('${b}');">📦 Scorecard</button>
    </span>`;
  },

  /** Open the Convert modal pre-selected to this program. */
  convert(basename) {
    // The Convert modal is exposed globally as openConvertModal() by convert-modal.js
    if (typeof window.openConvertModal === 'function') {
      window.openConvertModal({ program: basename });
    } else {
      // Fallback: click the floating Convert button + remember the program
      window._ppaPendingProgram = basename;
      const btn = document.querySelector('button.btn-convert, #btn-convert, [data-action="open-convert"]');
      if (btn) btn.click();
      else alert('Convert modal not available on this page. Click the 🛠️ Convert button in the Mission Control header.');
    }
  },

  /** Switch to the AST Galaxy tab and filter to this program. */
  openAst(basename) {
    // 1. Activate the AST Galaxy tab (try multiple selectors for robustness)
    const tabBtn = document.querySelector('[data-tab="ast-galaxy"]')
                || document.querySelector('button[onclick*="ast-galaxy"]')
                || document.querySelector('.dashboard-tab-ast-galaxy');
    if (tabBtn) tabBtn.click();
    else if (typeof window.switchDashboardTab === 'function') window.switchDashboardTab('ast-galaxy');

    // 2. Apply the file filter after the tab has had a chance to render
    setTimeout(() => {
      const filter = document.getElementById('galaxy-file-filter');
      if (filter) {
        // Try exact match first, then case-insensitive
        let opt = Array.from(filter.options).find(o => o.value === basename);
        if (!opt) opt = Array.from(filter.options).find(o => o.value.toLowerCase() === basename.toLowerCase());
        if (opt) {
          filter.value = opt.value;
          if (window.galaxyView?.setFilter) window.galaxyView.setFilter(opt.value);
          PortalProgramActions._toast(`AST Galaxy filtered to ${basename}`);
        } else {
          PortalProgramActions._toast(`AST Galaxy opened — ${basename} not found in the dropdown (REKT may not have parsed it)`);
        }
      }
    }, 300);
  },

  /** Open the Developer scorecard drawer (Visual Cockpit) for this program. */
  openScorecard(basename) {
    if (window.visualCockpit?._openProgramDrawer) {
      // Switch to the Cockpit + Developer persona, then open the drawer
      const tabBtn = document.querySelector('[data-tab="cockpit"]')
                  || document.querySelector('button[onclick*="cockpit"]');
      if (tabBtn) tabBtn.click();
      else if (typeof window.switchDashboardTab === 'function') window.switchDashboardTab('cockpit');
      setTimeout(() => {
        const persona = document.querySelector('.vc-persona[data-persona="developer"]');
        if (persona) persona.click();
        setTimeout(() => window.visualCockpit._openProgramDrawer(basename), 200);
      }, 200);
    } else {
      this._toast('Scorecard drawer needs the Visual Cockpit tab to be loaded at least once first.');
    }
  },

  /** Lightweight bottom-center toast (no library). Auto-dismisses after 3s. */
  _toast(msg) {
    let t = document.getElementById('ppa-toast');
    if (!t) {
      t = document.createElement('div');
      t.id = 'ppa-toast';
      t.className = 'ppa-toast';
      document.body.appendChild(t);
    }
    t.textContent = msg;
    t.classList.add('ppa-toast-show');
    clearTimeout(t._hideTimer);
    t._hideTimer = setTimeout(() => t.classList.remove('ppa-toast-show'), 3000);
  },
};
