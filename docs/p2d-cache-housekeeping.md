# PR2.d — Cache Housekeeping Across All Three Stores

**Last updated**: 2026-05-28
**Status**: Complete. 228 / 228 tests pass. End-to-end CLI smoke verified.

## Summary

Closes out the P1 cache thread with bounded-growth housekeeping for the
three caches that ship today:

| Cache | Where | Housekeeping ops added |
|---|---|---|
| Response cache (PR1) | `Data/llm-cache.db` | `llm-cache prune --ttl-days N [--max-bytes B]` |
| REKT scan cache (PR2) | `Data/rekt-scan.db` | `rekt-scan-cache prune --ttl-days N --max-entries N --drop-stale-semantic --drop-other-identity` |
| Program facts (PR3) | `output/rekt/*.facts.json` | `program-facts prune-orphans <facts-dir> --staging-dir <dir> [--dry-run]` |

All three are explicit verbs; nothing runs automatically. Same discipline
as the rest of P1: opt-in, observable, fail-open.

## What ships

### `IRektScanCache` extensions (`Agents/Infrastructure/RektCache/`)

Three new methods on the contract, implemented in `SqliteRektScanCache` with
the same fail-open pattern as the existing ops:

```csharp
Task<int> PruneByAgeAsync(TimeSpan maxAge, CancellationToken ct = default);
Task<int> PruneStaleSemanticVersionsAsync(CancellationToken ct = default);
Task<int> PruneToMaxEntriesAsync(int maxEntries, CancellationToken ct = default);
```

Every prune emits a structured `[RektScanCache]` log line:
`decision=prune-by-age | prune-stale-semantic | prune-to-max-entries`
with `deletedEntries=N` plus the relevant policy parameter.

### CLI verbs

**`rekt-scan-cache prune`** (extended; now multi-policy):
- `--ttl-days N` — delete entries whose `parsed_at_utc` is older than N days.
- `--max-entries N` — LRU-evict oldest-by-`parsed_at_utc` until under cap.
- `--drop-stale-semantic` — remove rows whose stored semantic-invalidation version isn't the current one (already cache misses; this just reclaims disk).
- `--drop-other-identity` — post-migration cleanup for the future ProgramKey scheme bump.
- Requires at least one policy flag; otherwise exits 2 with a helpful message.

**`llm-cache prune`** (new):
- `--ttl-days N` (default 7) — wraps `IResponseCache.PruneAsync(ttl)`.
- `--max-bytes B` — optional LRU-by-`last_hit_at_utc` size cap.

**`program-facts prune-orphans`** (new):
- Reads the staging dir's program basenames via `SourceTypeRegistry`.
- Deletes any `<stem>.facts.json` whose stem has no live source. `--dry-run` flag prints the plan without touching disk.
- Catches per-file IO errors and continues (one stuck file does not fail the whole prune).

All verbs print a one-line summary to stderr and structured `[…]` logs to the
configured logger.

## Files modified

```
Agents/Infrastructure/RektCache/IRektScanCache.cs              (+3 methods)
Agents/Infrastructure/RektCache/SqliteRektScanCache.cs         (+ ~80 lines impl)
Cli/RektScanCacheCommand.cs                                    (extended prune verb)
Cli/LlmCacheCommand.cs                                          (new — prune verb)
Cli/ProgramFactsCommand.cs                                     (+prune-orphans verb)
Program.cs                                                     (+1 line — register llm-cache)
CobolToQuarkusMigration.Tests/RektCache/SqliteRektScanCachePruneTests.cs  (7 tests)
docs/p2d-cache-housekeeping.md                                 (this file)
```

## End-to-end smoke results

```
$ dotnet ... rekt-scan-cache prune --db /tmp/rekt.db
rekt-scan-cache prune: no policy supplied. Pass at least one of
  --ttl-days, --max-entries, --drop-stale-semantic, --drop-other-identity.

$ dotnet ... rekt-scan-cache prune --db /tmp/rekt.db --ttl-days 1
[RektScanCache] decision=prune-by-age deletedEntries=0 maxAgeSeconds=86400
rekt-scan-cache prune: deleted 0 row(s) total.

$ dotnet ... llm-cache prune --db /tmp/llm.db --ttl-days 7
[LlmResponseCache] decision=prune deletedEntries=0 ttlSeconds=604800 maxBytes=-1
llm-cache prune: deleted 0 entries (ttlDays=7, maxBytes=-).

$ dotnet ... program-facts prune-orphans /tmp/facts --staging-dir /tmp/stage --dry-run
[ProgramFacts] decision=prune-orphan path=/tmp/facts/GONE.facts.json stem=GONE (dry-run)
program-facts prune-orphans: would delete 1 orphan(s).

$ dotnet ... program-facts prune-orphans /tmp/facts --staging-dir /tmp/stage
[ProgramFacts] decision=prune-orphan path=/tmp/facts/GONE.facts.json stem=GONE
program-facts prune-orphans: deleted 1 orphan(s).
$ ls /tmp/facts
A.facts.json
```

All four behaviours verified: no-policy guard, single-policy execution,
empty-cache no-op, dry-run separation, real deletion preserves live entries.

## Invalidation behaviour summary

| Trigger | Behaviour |
|---|---|
| `--ttl-days N` on a non-existent DB | Schema recreated, 0 deletes, no error. |
| `--ttl-days N` with no rows older than N | 0 deletes; log line emitted. |
| `--max-entries N` with row count ≤ N | 0 deletes. |
| `--max-entries 0` | Empties the cache; logged. |
| `--drop-stale-semantic` after a semantic version bump | Stale rows already invisible to `TryGetAsync`; this just reclaims disk. |
| Corrupt DB during prune | Fail open: logs `decision=*-failed reason=fail-open`, returns 0, never throws. |
| `program-facts prune-orphans` with missing staging dir | Exit 2 with explicit message; nothing deleted. |
| `program-facts prune-orphans --dry-run` | Logs `prune-orphan … (dry-run)`, count reported, no file touched. |
| `program-facts prune-orphans` real | Same logs without `(dry-run)`; per-file `File.Delete` errors caught and logged. |

## Recommended cadence (operational)

Until `doctor.sh` gains an explicit "housekeeping" command (out of scope per
P1 constraints), the suggested manual cadence is:

| Frequency | Command | Why |
|---|---|---|
| Daily / per-deploy | `rekt-scan-cache prune --ttl-days 30` | Drop rows for files that have been removed from the corpus. |
| Weekly | `llm-cache prune --ttl-days 7 --max-bytes 2147483648` | Keep response cache under ~2 GB; matches the 7-day code-iteration TTL in `docs/throttling-and-cache-design.md`. |
| After every `rekt-full` run | `program-facts prune-orphans output/rekt --staging-dir source/.rekt-staging` | Match facts files to current corpus. |
| Post-`ProgramKey` migration | `rekt-scan-cache prune --drop-other-identity` and `llm-cache prune --ttl-days 0` | One-time cleanup; safe to run any time, no-op when not needed. |

A small `doctor.sh cache-cleanup` wrapper can mechanise this once `doctor.sh`
redesign is allowed.

## Compatibility concerns

1. **`PruneOtherIdentitySchemesAsync` and `PruneStaleSemanticVersionsAsync` both delete by row.** They are not reversible. The semantic-version prune is safe by construction (those rows were already unreachable). The identity-scheme prune assumes the caller actually completed the migration.
2. **`--max-entries 0` empties the cache.** Intentional but easy to fire by accident; documented.
3. **`program-facts prune-orphans`** is keyed on the source's stem (case-insensitive). A program that exists in the corpus under a different extension (e.g. you renamed `.cob` → `.cbl`) will not be considered an orphan as long as the stem matches. Documented.
4. **Per-file delete errors** are caught and logged in `program-facts prune-orphans`. The exit code does not reflect partial failure — the summary count does. Acceptable for a manual housekeeping verb; surface to monitoring via the `[ProgramFacts]` log line if needed.

## Remaining technical debt (delta from PR3)

1. **No `doctor.sh` cache-cleanup wrapper.** All three verbs are manual today. Tracked for when `doctor.sh` redesign is allowed.
2. **`PruneByAgeAsync` and `PruneToMaxEntriesAsync` are O(N) per call.** Fine at a few thousand entries; if a 100k-entry corpus appears, add an index on `parsed_at_utc` (cheap, additive).
3. **No "vacuum" step** after large deletes. SQLite reclaims pages lazily; an explicit `VACUUM` after a big `--drop-stale-semantic` would shrink the file. Add a `--vacuum` flag later if file size matters.
4. **`rekt-scan-cache prune --max-entries`** uses `parsed_at_utc` as the eviction order (LRU by last parse), not by hit count or access time. Acceptable proxy; the scan cache doesn't track lookups separately.
5. **No automated CI test** that the CLI verbs are wired into `Program.cs`. Smoke is manual. The test surface covers the underlying methods; CLI registration is a one-line addition with low regression risk.

## Architectural risks discovered during PR2.d

1. **Three cache stores, three prune surfaces.** Each is well-scoped, but together they make four DB / file locations users must remember to clean. The operational table above documents the cadence; longer-term a single `doctor.sh housekeeping` umbrella verb would consolidate.
2. **`program-facts prune-orphans` is the only verb that touches files (not just DB rows).** Its `--dry-run` guard mirrors how `rm`-adjacent tools should behave. Worth pointing out in code review when more file-deleting verbs are added.
3. **Semantic-version prune is not symmetric.** `TryGetAsync` already treats mismatched versions as misses, so this verb is purely a disk-reclaim tool. Documented in the method doc comment so it isn't mistaken for an invalidation primitive.
4. **All prune ops fail open.** Matches the rest of the cache layer's discipline, but means a stuck-on-fail-open state silently wastes disk. Monitoring on `decision=*-failed` log lines is the recommended detection.
5. **No coordination between prune verbs and live readers.** SQLite WAL keeps writes safe; a long `PruneToMaxEntriesAsync` happening while a `doctor.sh rekt-full` is running will compete for the writer lock. Acceptable today (manual housekeeping vs. interactive run); revisit if both become hot.

## Next

P1 cache thread is now feature-complete:

- PR1 cache infrastructure ✅
- PR1.b JavaConverterAgent integration ✅
- PR2 REKT scan cache infrastructure ✅
- PR2.b doctor.sh wiring ✅
- PR2.c targeted REKT scan via `_REKT_PROGRAM_FILTER` ✅
- PR2.d housekeeping (this PR) ✅
- PR3 program-facts schema + extractor + CLI ✅

Remaining P1 work:
- **PR4** — prompt projection layer per agent; consume `*.facts.json`;
  extend `JavaConverterCacheKeys.RektFactsHash` so PR1 cache invalidates
  on facts changes. This is where the user-visible token-reduction and
  hallucination-drop value shows up.
- **PR5** — preprocessor hardening + `.preprocess.json` sidecars (the
  reader already ships in PR3; PR5 just starts populating).

Suggested order: **PR4 next** — biggest user-visible win, and the schema/extractor
groundwork is already in place. PR5 can land any time after; it's orthogonal.
