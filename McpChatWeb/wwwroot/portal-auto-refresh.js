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

    /**
     * Check if it's safe to re-render. We must NOT re-render when:
     *   - any modal is open (would blow away the modal content)
     *   - any input/textarea/select has focus (user is typing)
     *   - the user is hovering over a tooltip or expandable detail
     *   - explicit opt-out: any element with [data-refresh="pause"] is visible
     * If unsafe, we still fetch fresh data silently into the cache so a future
     * tick can re-render without an extra round-trip, but we skip the visual
     * swap. The LIVE pip tick stays alive so the user sees the surface is
     * still being monitored.
     */
    const isSafeToRender = () => {
      // 1. Modal open anywhere on the page (matches .mi-modal, .ih-modal,
      //    .vc-drawer, .modal, [role=dialog] — common modal/drawer classes)
      const openModalSelectors = [
        '.mi-modal', '.ih-modal',
        '.vc-drawer', '#psr-modal',
        '[role="dialog"]', '.modal-open', '.modal[style*="display: flex"]',
        '#mi-compile-modal[style*="display: flex"]',
        '#mi-compare-modal[style*="display: flex"]',
        '#mi-cap-edit-modal[style*="display: flex"]',
      ];
      for (const sel of openModalSelectors) {
        const els = document.querySelectorAll(sel);
        for (const el of els) {
          // visible = either explicit display flex or in DOM with offsetParent
          if (el.style.display === 'flex' || el.style.display === 'block' ||
              (el.offsetParent !== null && el.style.display !== 'none')) return false;
        }
      }
      // 2. Active text editing
      const active = document.activeElement;
      if (active && (active.tagName === 'INPUT' || active.tagName === 'TEXTAREA' || active.tagName === 'SELECT')) {
        // Don't pause for read-only quick filters — only when actually typing
        if (active.type !== 'checkbox' && active.type !== 'radio' && !active.readOnly) return false;
      }
      // 3. Explicit pause flag on any element under instance.root (lets
      //    individual subviews opt out — e.g. expanded service-candidate row)
      if (instance.root.querySelector('[data-refresh="pause"]')) return false;
      return true;
    };

    const refresh = async () => {
      if (!isSafeToRender()) {
        // Silently bump the timestamp so the LIVE pip ticks correctly, but
        // don't fetch + render. Cheap.
        PortalAutoRefresh._touchIndicator(instance.root);
        return;
      }
      try {
        const before = JSON.stringify(instance._data || instance._lastSnapshot || '');
        if (typeof instance.loadAndRender === 'function') {
          if (instance._data !== undefined) instance._data = null;
          await instance.loadAndRender();
        }
        const after = JSON.stringify(instance._data || instance._lastSnapshot || '');
        PortalAutoRefresh._updateIndicator(instance.root, before && after && before !== after);
      } catch { /* fail-soft */ }
    };

    const tick = () => {
      const visible = !document.hidden &&
        instance.root && instance.root.offsetParent !== null;
      if (visible !== isVisible) {
        isVisible = visible;
        if (visible) {
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

  /** Bump just the timestamp (used when refresh is skipped due to user interaction). */
  _touchIndicator(root) {
    const pip = root.querySelector('.portal-auto-pip');
    if (pip) pip._lastTs = Date.now();
  },

  _hideIndicator(root) {
    const pip = root.querySelector('.portal-auto-pip');
    if (pip) pip.style.display = 'none';
  },
};
