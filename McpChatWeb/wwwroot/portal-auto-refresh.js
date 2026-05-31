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
    let isVisible = false;

    const refresh = async () => {
      try {
        const before = JSON.stringify(instance._data || instance._lastSnapshot || '');
        if (typeof instance.loadAndRender === 'function') {
          if (instance._data !== undefined) instance._data = null;
          await instance.loadAndRender();
        }
        const after = JSON.stringify(instance._data || instance._lastSnapshot || '');
        // Always update timestamp + tick; flash green only when data changed.
        PortalAutoRefresh._updateIndicator(instance.root, before && after && before !== after);
      } catch { /* fail-soft */ }
    };

    const tick = () => {
      const visible = !document.hidden &&
        instance.root && instance.root.offsetParent !== null;
      if (visible !== isVisible) {
        isVisible = visible;
        if (visible) {
          // Install/show the pip immediately so the user knows refresh is wired
          PortalAutoRefresh._updateIndicator(instance.root, false, intervalMs);
          if (timer) clearInterval(timer);
          timer = setInterval(refresh, intervalMs);
        } else if (timer) {
          clearInterval(timer); timer = null;
          PortalAutoRefresh._hideIndicator(instance.root);
        }
      }
    };

    document.addEventListener('visibilitychange', tick);
    setInterval(tick, 2000);
    tick();
  },

  /**
   * Render (or update) the LIVE indicator pip top-right of the surface.
   * Always visible while attached & focussed. Pulse-green-flash when data
   * actually changed; otherwise tick "Last X s ago" silently.
   */
  _updateIndicator(root, dataChanged, intervalMs) {
    let pip = root.querySelector('.portal-auto-pip');
    if (!pip) {
      pip = document.createElement('div');
      pip.className = 'portal-auto-pip';
      pip.innerHTML = '<span class="portal-pip-dot"></span><span class="portal-pip-label">LIVE</span>';
      root.appendChild(pip);
      // tick the "last refreshed" timestamp every second
      pip._lastTs = Date.now();
      pip._intervalLabel = setInterval(() => {
        const ago = Math.floor((Date.now() - pip._lastTs) / 1000);
        const lbl = pip.querySelector('.portal-pip-label');
        if (lbl) {
          if (ago < 2) lbl.textContent = 'LIVE · just now';
          else if (ago < 60) lbl.textContent = `LIVE · ${ago}s ago`;
          else lbl.textContent = `LIVE · ${Math.floor(ago / 60)}m ago`;
        }
      }, 1000);
    }
    pip.style.display = '';
    pip._lastTs = Date.now();
    if (dataChanged) {
      pip.classList.add('portal-auto-pip-flash');
      setTimeout(() => pip.classList.remove('portal-auto-pip-flash'), 1500);
    }
  },

  _hideIndicator(root) {
    const pip = root.querySelector('.portal-auto-pip');
    if (pip) pip.style.display = 'none';
  },
};
