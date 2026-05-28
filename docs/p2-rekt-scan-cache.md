# PR2 — Incremental REKT Scan Cache

**Last updated**: 2026-05-28
**Status**: Infrastructure complete. 205 / 205 tests pass.

## Summary

Adds the C# infrastructure for content-hash-based incremental REKT scans with
dependency-aware invalidation. No `doctor.sh` / shell-pipeline wiring yet —
that's PR2.b, mirroring how PR1 (cache infra) was followed by PR1.b
(JavaConverterAgent wiring).

### What ships
- A typed scan cache (`SqliteRektScanCache`) with the same discipline as the response cache: WAL, per-operation connections, busy timeout, two independent schema versions (storage layout + semantic invalidation), fail-open on every storage error.
- A copybook dependency graph (`RektCopybookGraph`) that extracts direct `COPY` directives from preprocessed COBOL bytes and computes transitive closure.
- A pure decision engine (`IncrementalScanPlanner`) that consumes the graph + the cache and emits a `ScanPlan { ToParse, ToSkip }` with explicit per-program reasons.
- A `ComputeDependencyClosure(seeds)` helper that solves the "`--program X` without prior full-corpus scan" case at the infrastructure level. PR2.b wires it into the bash parse loop.

### What is intentionally out of scope
- No `doctor.sh` changes.
- No CLI subcommand (deferred to PR2.b).
- No prompt-injection or agent-behaviour changes.
- No REKT output-shape change.
- No `program-facts.json` (PR3).
- No transitive-CALL-graph awareness (smojol resolves CALLs per-file; PR2's scope is only the preprocess-bytes / copybook-dependency dimension).

## Files modified

```
Agents/Infrastructure/RektCache/RektScanEntry.cs          (new — record + enums)
Agents/Infrastructure/RektCache/IRektScanCache.cs         (new — contract)
Agents/Infrastructure/RektCache/SqliteRektScanCache.cs    (new — fail-open SQLite store)
Agents/Infrastructure/RektCache/RektCopybookGraph.cs      (new — COPY extraction + transitive closure)
Agents/Infrastructure/RektCache/IncrementalScanPlanner.cs (new — ScanPlan / ScanDecision / ScanReason)
CobolToQuarkusMigration.Tests/RektCache/RektCopybookGraphTests.cs       (5 tests)
CobolToQuarkusMigration.Tests/RektCache/SqliteRektScanCacheTests.cs     (7 tests)
CobolToQuarkusMigration.Tests/RektCache/IncrementalScanPlannerTests.cs (10 tests)
docs/p2-rekt-scan-cache.md                                 (this file)
```

## Invalidation behaviour summary

| Trigger | Planner reason | Logged decision |
|---|---|---|
| No previous entry | `NotCached` | `decision=parse reason=NotCached` |
| Preprocessed bytes changed | `SourceChanged` | `decision=parse reason=SourceChanged` |
| A copybook in the snapshot has a new hash | `DependencyChanged` | `decision=parse reason=DependencyChanged` |
| A copybook used today wasn't in the previous snapshot | `DependencyMissingFromCache` | `decision=parse reason=DependencyMissingFromCache` |
| A snapshotted copybook is no longer in the corpus | `DependencyMissingFromCorpus` | `decision=parse reason=DependencyMissingFromCorpus` |
| Previous parse was `DepsOnly` / `RawAst` (low confidence) | `PreviousParseLowConfidence` | `decision=parse reason=PreviousParseLowConfidence` |
| Identity-scheme mismatch (current vs stored) | (cache returns null) → `NotCached` | `decision=parse reason=NotCached` |
| Semantic invalidation version mismatch | (cache returns null with `decision=stale-entry`) → `NotCached` | first the stale-entry log, then `decision=parse reason=NotCached` |
| Storage schema mismatch | DROP + recreate | `decision=storage-schema-recreate` then everything misses |
| Corrupt DB / IO failure | Fail open | `decision=lookup-failed reason=fail-open` → planner sees `NotCached` |

Every decision (`parse`, `skip`, `stale-entry`, `record-parse`, `prune-other-identity-schemes`, `storage-schema-recreate`, all `*-failed`) emits a stable structured log event named `RektScanCache` carrying:

```
runId, correlationId, basename, decision, reason,
preprocessedHash (8-char short), dependencyCount,
missingCopybooks (count), identityScheme,
[outcome, confidence, relativePath on record-parse]
```

## Identity-migration compatibility

Every persisted row carries:
- `basename` (current identity)
- `identity_scheme` (`"v1-basename"` today; PRIMARY KEY component)
- `relative_path` (nullable column, forward-compat)
- `semantic_invalidation_ver` (per-row, lets us invalidate without dropping data)

`PruneOtherIdentitySchemesAsync(currentScheme)` exists for the eventual ProgramKey migration: when the scheme bumps to `"v2-relative-path"`, callers can either drop the old rows or let them age out naturally (mismatched scheme → cache miss → re-parse).

Test coverage:
- `DifferentIdentityScheme_IsolatesEntries`: two schemes coexist without collision.
- `PruneOtherIdentitySchemes_DropsLegacy`: explicit cleanup works.
- `IncrementalScanPlannerTests.DifferentIdentityScheme_TreatsCacheAsEmpty`: planner-level isolation.

## Scan-time reduction estimate

Today a full REKT scan on the 22-program test corpus takes ~3 minutes wall clock (≈5–8 seconds per program × 22 sequential `docker exec smojol` invocations). With PR2 wired by PR2.b:

| Scenario | New wall time | Saved |
|---|---|---|
| Cold cache (first run) | ~3 min (unchanged — populates cache) | 0% |
| Re-run with no source change | ~5–10 s (cache lookup + manifest verify) | ~95% |
| One program edited | ~5–8 s for the changed program + ~5 s cache check for the rest | ~90% |
| One leaf copybook edited (used by 3 programs) | ~15–24 s for the dependents + ~5 s cache check for the rest | ~85% |
| Targeted `--program X` from cold | Only X + its transitive copybooks | depends on closure size; typically 5–15 s vs. 3 min |

These are projections from infrastructure behaviour. Real wall-clock numbers require PR2.b wiring and a live `doctor.sh` run.

## Compatibility concerns

1. **Direct copybook dependency only by name, not by `COPY REPLACING` variant.** A program that does `COPY BOOK REPLACING ==X== BY ==Y==` is treated identically to one that does `COPY BOOK`. Correct over-approximation: cache invalidates when BOOK changes, regardless of REPLACING. Documented in `RektCopybookGraph` doc comments.
2. **Smojol CALL resolution is not in the dependency graph.** A program's parse outcome can depend on whether smojol can resolve CALL targets, but smojol does that per-file from the staging dir. Adding CALL-graph awareness would also need to track which CALL-target's parse the dependent depends on — out of P2 scope.
3. **Comment-line detection is fixed-format only** (column 7 = '*' or '/'). Free-format COBOL with leading `*>` is not respected; the regex would extract a `COPY` from inside a free-format comment. Acceptable for this codebase which is fixed-format; documented.
4. **`SourceTypeRegistry` and `RektCopybookGraph` agree on what's a copybook** but the user of `AddFile` decides via the `isCopybook` parameter. Callers should derive that from `SourceTypeRegistry.IsCopybook(path)` to stay consistent.
5. **The planner does not write to `output/rekt/`** — it only decides what to parse. After PR2.b wires the bash side, smojol's existing output paths are unchanged.

## Remaining technical debt (delta from PR1.b)

| # | Item | Severity |
|---|---|---|
| 1 | No `doctor.sh` integration — PR2.b mirror | Expected, scoped out. |
| 2 | No CLI subcommand for the planner | Will land with PR2.b. |
| 3 | No `output/rekt/<stem>.*` artifact existence check on skip | Today the planner trusts its own metadata. If the user deletes `output/rekt/` between runs, a "skip" decision will lead to a missing-artifact downstream. PR2.b should add an existence sanity check before honouring a skip. |
| 4 | `RektCopybookGraph` is built per-run in memory | Acceptable at corpus sizes ≤ a few thousand files. If we ever materialise it to disk, the storage schema should mirror the dependency snapshot format. |
| 5 | Direct vs transitive distinction is invisible to consumers | The planner only exposes the transitive snapshot. If a future analysis needs "what does this program directly reference?", expose a new method rather than re-deriving. |
| 6 | `GetManyAsync` is a loop of single-key queries | At our scale (≤1k programs), pooling makes this acceptable. Replace with a temp-table IN clause if a 10k-file corpus appears. |

## Architectural risks discovered during PR2

1. **The dependency snapshot is the contract.** Once a program is recorded with a snapshot of N copybooks, the cache's correctness depends on the planner re-hashing those exact N copybooks consistently on the next run. The risk is a copybook being moved (e.g. renamed) so the snapshot key no longer resolves. We treat that as `DependencyMissingFromCorpus` and force re-parse — correct, but worth highlighting.
2. **Fail-open on every storage error is mandatory but hides DB drift.** A corrupt `Data/rekt-scan.db` produces a warning log and silently degrades to full parse. Operationally this is right (the scan still works), but a stuck-on-fail-open state would silently lose the cache benefit. Recommend monitoring the `decision=*-failed` log lines.
3. **Semantic invalidation version is a per-row marker, not a schema PRAGMA.** This means bumping it doesn't recreate the table — it just causes mass cache misses. Cleaner than DROP for derived data, but a long-lived DB will accumulate stale rows. PR2.b should add a `prune-by-semantic-version` housekeeping op.
4. **No stampede protection.** Two concurrent `doctor.sh` runs against the same DB will both fail to find a cache entry, both run smojol, and both upsert. SQLite WAL keeps the writes safe but the work is duplicated. Real risk only if a user runs the pipeline in parallel against the same repo — not typical today.
5. **The planner is intentionally stateless and pure.** It does not perform IO beyond what the supplied cache does, and the graph is provided pre-built. This makes it trivially testable but pushes the orchestration burden onto the caller (PR2.b). The trade-off is correct for "infrastructure-only" scope.

## What PR2.b needs to do

1. Add a CLI subcommand `rekt-scan-cache plan <staging-dir>` that:
   - Walks the staging dir using `SourceTypeRegistry`.
   - Reads each file's preprocessed bytes (or the staged bytes if `.preprocessed/` is the source of truth).
   - Builds a `RektCopybookGraph`.
   - Calls `IncrementalScanPlanner.PlanAsync`.
   - Emits the plan to stdout as a JSON or newline-separated list with reason tags.
2. Add a CLI subcommand `rekt-scan-cache record <basename> --outcome ... --decision-json ...` that calls `RecordParseAsync`.
3. In `doctor.sh run_rekt_parse` (`scripts/...`), gate the new behaviour behind `_REKT_INCREMENTAL=true` (default off):
   - Before the `while` loop, invoke `plan` and split the staging files into `to_parse` / `to_skip`.
   - Iterate `to_parse` and call smojol as today.
   - After each successful parse, call `record`.
   - Log skipped files as `decision=skip` so the user sees the savings.
4. Add a `--force` flag to `rekt-full` that ignores the cache.

These are minimal, ~30-line additions per file. They explicitly mirror the PR1 → PR1.b pattern. PR2.b should NOT redesign the bash structure.
