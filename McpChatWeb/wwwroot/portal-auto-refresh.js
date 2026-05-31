// ─────────────────────────────────────────────────────────────────────────
// Portal auto-refresh (#12) — shared helper used by:
//   • Visual Cockpit  (15s, baked-in earlier)
//   • Modernization Intelligence  (30s)
//   • Insights Hub               (30s)
//
// Why a separate helper? Each surface has different load mechanics, but
// they all share the same "poll only when visible, re-render only on
// change" pattern. Extracted so we don't reinvent it three times.
//
// Contract: the calling class must provide a `loadAndRender()` (or
// `_renderActive()`) method. We detect visibility via document.hidden
// AND the root element's offsetParent (covers dashboard-tabs router
// hiding panels via display:none).
// ─────────────────────────────────────────────────────────────────────────

window.PortalAutoRefresh = {
  attach(instance, intervalMs = 30000) {
    if (!instance || !instance.root) return;
    if (instance._autoRefreshAttached) return;
    instance._autoRefreshAttached = true;

    let timer = null;
    let lastFingerprint = '';
    let isVisible = false;

    const refresh = async () => {
      try {
        // Snapshot a 'fingerprint' of the current data so we can detect changes.
        const before = JSON.stringify(instance._data || instance._lastSnapshot || '');
        // The view exposes its own data-fetch — prefer loadAndRender (full
        // reload + render) since that's what every existing surface uses.
        if (typeof instance.loadAndRender === 'function') {
          // Clear stale cached data so we actually re-fetch from API
          if (instance._data !== undefined) instance._data = null;
          await instance.loadAndRender();
        }
        const after = JSON.stringify(instance._data || instance._lastSnapshot || '');
        if (before && after && before !== after) {
          PortalAutoRefresh._flashIndicator(instance.root);
        }
      } catch { /* fail-soft — next tick will retry */ }
    };

    const tick = () => {
      // Visibility = browser tab not hidden AND root has a non-null offsetParent
      // (offsetParent is null when display:none is applied anywhere up the tree).
      const visible = !document.hidden &&
        instance.root && instance.root.offsetParent !== null;
      if (visible !== isVisible) {
        isVisible = visible;
        if (visible) {
          if (timer) clearInterval(timer);
          timer = setInterval(refresh, intervalMs);
        } else if (timer) {
          clearInterval(timer); timer = null;
        }
      }
    };

    document.addEventListener('visibilitychange', tick);
    setInterval(tick, 2000);
    tick();
  },

  /** Briefly flash a 🟢 indicator at top-right of root to signal "data changed". */
  _flashIndicator(root) {
    let pip = root.querySelector('.portal-auto-pip');
    if (!pip) {
      pip = document.createElement('div');
      pip.className = 'portal-auto-pip';
      pip.title = 'Auto-refreshed (data changed)';
      pip.textContent = '🟢 live';
      root.appendChild(pip);
    }
    pip.classList.add('portal-auto-pip-flash');
    setTimeout(() => pip.classList.remove('portal-auto-pip-flash'), 1500);
  },
};
