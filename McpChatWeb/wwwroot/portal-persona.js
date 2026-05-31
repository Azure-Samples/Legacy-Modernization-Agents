// ─────────────────────────────────────────────────────────────────────────
// Persona router (#7) — unified persona switcher
//
// The portal exposes overlapping personas inside three workspaces
// (Visual Cockpit, Insights Hub, Modernization Intelligence). This helper
// gives users ONE persona-first entry point that routes to the most-
// relevant surface + persona + subview for that role.
//
// Why not a full nav rewrite? Each workspace still has standalone value
// for power users. This is the friendly front door.
// ─────────────────────────────────────────────────────────────────────────

window.PortalPersona = {
  // Each persona maps to:
  //   tab     — top-level dashboard tab to activate
  //   persona — internal persona name to switch to inside that workspace
  //   highlight — pulse the persona pill to indicate it's active
  go(persona) {
    const map = {
      business:  { tab: 'cockpit', vcPersona: 'business',  toast: '💼 Business Owner — outcomes, blockers, ROI' },
      architect: { tab: 'cockpit', vcPersona: 'architect', toast: '🏗 Architect — coupling, domains, service hubs' },
      lead:      { tab: 'cockpit', vcPersona: 'lead',      toast: '🚀 Modernization Lead — wave Kanban, execution plan' },
      developer: { tab: 'cockpit', vcPersona: 'developer', toast: '👨‍💻 Developer — per-program scorecards, run history' },
      mission:   { tab: 'cockpit', vcPersona: 'mission',   toast: '🌐 Mission Control — consolidated overview' },
    };
    const m = map[persona];
    if (!m) return;

    // 1. Visually highlight the active persona pill
    document.querySelectorAll('.persona-pill').forEach(p =>
      p.classList.toggle('persona-pill-active', p.classList.contains(`persona-${persona}`)));

    // 2. Switch to the target dashboard tab
    if (typeof window.switchDashboard === 'function') {
      window.switchDashboard(m.tab);
    } else {
      const btn = document.querySelector(`[data-tab="${m.tab}"]`);
      if (btn) btn.click();
    }

    // 3. Once the cockpit instance exists, switch to the right persona
    setTimeout(() => {
      const personaBtn = document.querySelector(`.vc-persona[data-persona="${m.vcPersona}"]`);
      if (personaBtn) personaBtn.click();
    }, 250);

    // 4. Toast
    if (typeof window.PortalProgramActions?._toast === 'function') {
      PortalProgramActions._toast(m.toast);
    }
  },
};
