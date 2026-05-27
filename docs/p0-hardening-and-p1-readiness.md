# P0 Hardening — Status and P1 Readiness Assessment

**Last updated**: 2026-05-27
**Scope**: Post-P0, pre-P1. Documents what is now safe to build on and what
still needs attention before the response cache / WorkspaceStore lands.

## Summary of hardening improvements

| Area | Before | After |
|---|---|---|
| Reservation lifecycle | AsyncLocal-stashed id; **silently leaked** because AsyncLocal does not flow back from `AcquireAsync` to the caller's continuation | `IRateLimiter.AcquireAsync` returns a typed `IRateLimitReservation` (`Commit` / `Cancel` / `Dispose`). No AsyncLocal. |
| Limiter observability | One log line per wait, no run context | Snapshot fields (`currentTpm`, `currentRpm`, `reservations`, `cooldownRemainingMs`) on every limiter log; `runId` and `correlationId` propagated via `LlmCorrelationContext` (AsyncLocal scoping is one-way — that direction is safe). |
| Provider capabilities | Implicit, scattered across clients | `ProviderCapabilities` static metadata for `azure-openai` and `github-copilot-sdk` (Retry-After support, streaming, token reporting, default caps). Read by tests; no routing logic. |
| Internal test access | None | `[InternalsVisibleTo("CobolToQuarkusMigration.Tests")]` so reservation/cooldown internals are testable without making them public. |
| Test coverage | 4 retry helper tests + 4 discovery tests | + 8 `RateLimitTrackerTests` (reservation lifecycle, cooldown, over-admission), + 11 `GitHubSdkRateLimitDetectionTests` (positive/negative regex coverage), + 3 `NestedDiscoveryTests`. **Total: 122 tests, all pass.** |
| Basename coupling | Implicit | Mapped in `docs/basename-coupling-map.md`; identifies every consumer and the three-wave migration plan. |

### Files changed in this pass

```
Agents/Infrastructure/IRateLimiter.cs            (handle-based API)
Agents/Infrastructure/LimiterReservation.cs      (new, concrete handle)
Agents/Infrastructure/LimiterSnapshot.cs         (new, ILimiterObservable)
Agents/Infrastructure/LlmCorrelationContext.cs   (new, AsyncLocal scope)
Agents/Infrastructure/ProviderCapabilities.cs    (new, static metadata)
Agents/Infrastructure/LlmRetryHelper.cs          (log enrichment)
Agents/Infrastructure/ResponsesApiClient.cs      (reservation id threading, log enrichment, dropped AsyncLocal)
Agents/Infrastructure/CopilotChatClient.cs       (handle-based limiter use)
Properties/InternalsVisibleTo.cs                 (new)
docs/basename-coupling-map.md                    (new)
docs/p0-hardening-and-p1-readiness.md            (this file)
CobolToQuarkusMigration.Tests/Infrastructure/RateLimitTrackerTests.cs        (new)
CobolToQuarkusMigration.Tests/Infrastructure/GitHubSdkRateLimitDetectionTests.cs (new)
CobolToQuarkusMigration.Tests/Helpers/NestedDiscoveryTests.cs                (new)
```

## Updated technical debt assessment

### Must fix before cache (P1) lands

| # | Item | Why it blocks P1 |
|---|---|---|
| 1 | **Cache key must include identity scheme version** | Once cache exists, switching basename → relative-path identity invalidates every entry silently. Adding `identitySchemeVersion = "v1-basename"` to the key now costs nothing and avoids a future migration headache. See `docs/basename-coupling-map.md` §Risk hotspots #5. |
| 2 | **Stable provider key string** for cache keys | Cache must namespace by provider. Codify `ProviderCapabilities.ProviderKey` (`"azure-openai"`, `"github-copilot-sdk"`) as the canonical string used in cache keys. |
| 3 | **`InternalsVisibleTo` for cache tests** already in place — verify with the same hardening pattern. | Cache code will live in the same project; internals access is needed for unit tests. |

### Should fix before cache, but not blocking

| # | Item | Notes |
|---|---|---|
| 4 | GitHub SDK 429 detection is message-pattern only | Will produce cache misses if the SDK changes wording. Acceptable for now; tracked in `GitHubSdkRateLimitDetectionTests` so a wording change fails a test rather than going silent. |
| 5 | Concurrent limiter test relies on cancellation timing | `ConcurrentAcquires_DoNotOverAdmitBeyondTpm` uses a 50ms cancel — adequate but flaky-prone on heavily-loaded CI. |
| 6 | `ReleaseReservation` is O(n) over the rolling 60s queue | Fine at current scale (≤ ~thousands of entries). Replace with a dict-indexed deque if a multi-thousand-request burst becomes typical. |

### Acceptable to defer past P1

| # | Item | Why acceptable |
|---|---|---|
| 7 | `preprocess-for-rekt.sh` interpolates paths into `python -c` | Works for COBOL paths in practice; refactor needs a heredoc rewrite (broader change). |
| 8 | Duplicate basename handling is warn-only | Captured in `docs/basename-coupling-map.md`; full fix is a multi-wave change. |
| 9 | GitHub TPM/RPM caps hardcoded in `ChatClientFactory` | Will move to config when `doctor.sh` UX is redesigned. |
| 10 | No streaming limiter integration | Streaming path isn't used by converters today. Will need attention if streaming becomes default. |

### Resolved (was on the debt list before this pass)

- ~~Reservation leaks via AsyncLocal asymmetry~~ — fixed by handle-based API.
- ~~No tests for reservation/cooldown semantics~~ — covered by `RateLimitTrackerTests`.
- ~~No nested-discovery regression coverage~~ — covered by `NestedDiscoveryTests`.

## What is now safe for P1

The following P1 work can begin without further hardening:

1. **Response cache (deterministic-only)** — interface boundary, reservation lifecycle, and provider-key naming are stable.
2. **Cache TTL + LRU eviction** — independent of identity scheme as long as key construction includes the scheme version (debt item #1).
3. **Per-call wait-ceiling override from config** — fields are already there; just needs wiring once config redesign is allowed.
4. **Provider capability assertions in tests** — `ProviderCapabilities` already exists; new tests can pin behaviour.
5. **Structured-log consumers** — every limiter / retry / 429 log now carries `runId`, `correlationId`, `provider`, `model`, and snapshot fields. Downstream parsers can rely on this shape.

## What still blocks P1 (if anything)

**Nothing hard-blocks P1 cache work**, provided:

- Cache key construction includes `identitySchemeVersion = "v1-basename"` from day one (additive — costs one extra hash input).
- Cache key uses `ProviderCapabilities.ProviderKey` as the provider namespace.
- Cache tests use the same `InternalsVisibleTo` pattern.

These three small constraints turn the open identity question into a future
migration that **can** happen instead of a present obstacle that **must**.

## Architectural risks discovered during hardening

1. **AsyncLocal asymmetry trap** — setting an AsyncLocal inside an async method does not propagate back to the caller's continuation. The original P0 reservation tracking relied on this and was silently incorrect. **Lesson:** any cross-call state must be passed explicitly as a value/handle. Future P1 designs (cache invalidation cues, run-id tagging) should not use AsyncLocal for state that needs to flow back — only forward.

2. **`LlmCorrelationContext` uses AsyncLocal correctly** — only for forward (parent→child) propagation of read-only context. Verified by inspection.

3. **Shared singleton GitHub limiter** (`ChatClientFactory.GitHubLimiter`) is correct for in-process sharing but invisible to anyone reading per-client constructors. If we ever support per-tenant or per-token GitHub configurations, this Lazy<> becomes a problem. Acceptable for now; surface in `ProviderCapabilities` if needed.

4. **`ProviderCapabilities` is read by tests but not yet consumed by production code.** Intentional — populating it now creates the contract, wiring it later avoids the routing-engine slippery slope the user explicitly forbade.

5. **Duplicate-basename warning in `resolve-programs.py` is fail-open**. If a duplicate exists, the warning fires once per process and the pipeline proceeds with last-write-wins. This is the same behaviour as before P0 but is now explicit. Documented in the coupling map; not safe to silently "fix" without the wave plan there.

6. **Test discovery uses `InternalsVisibleTo` to the test assembly**. New test projects (if added) need the assembly name added to `Properties/InternalsVisibleTo.cs`.

## Verification

- Build: clean (only the pre-existing `RegressionFixtureAgent.cs` nullability warning).
- Tests: 122 / 122 pass (`dotnet test` full suite).
- Smoke: `python3 tools/resolve-programs.py --program FLAT --repo-root /tmp/...` correctly finds nested `.cbl` and `.cob` files.
- Bash syntax: `bash -n doctor.sh tools/preprocess-for-rekt.sh` clean.
