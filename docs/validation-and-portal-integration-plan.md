**Last updated**: 2026-10-22

# Validation and portal integration plan

Status: proposed for review. The inventory and planning are complete; recovery,
integration, publication, and acceptance execution have not started.

## Scope and guardrails

- Recover unpublished framework work, then integrate Java/parity validation, the
  AI Loop visualization, chunk assembly, source/output handling, and JCL discovery.
- Use the existing local reference checkout for candidate improvements, not as a
  replacement baseline. Do not import its optional second-parser integration or
  dependencies. That integration remains excluded until explicitly requested.
- Base implementation on refreshed upstream main. Preserve shared provider routing,
  custom GitHub-host authentication, rate limiting, and existing C# behavior.
- Leave the current dirty checkout and unrelated pending work untouched.
- Keep implementation in permanent Git worktrees and push approved commits to
  remote branches. Keep generated code in `output/java/<run>/` or
  `output/csharp/<run>/`, with run-owned evidence. Disposable unit-test fixtures
  may still use isolated temporary directories; retained work and evidence may not.
- Do not invent missing copybooks, layouts, constants, or behavior.

## Verified starting point

| Item | Evidence |
|---|---|
| Recovery branch | `fix/source-parity-java-validation` at `b4580f262b47f462cf5df3f80cf584b616b04131` |
| Core Java/parity donor | `f57f7bc47bb82023f35666ca5104df8f5b7c95e3` |
| Inspected upstream baseline | Local `origin/main` at `33002ffb731db35eac132c0fa7074b27fad8d605`; refresh before implementation |
| Already integrated | PRs #181 through #187 are in the inspected upstream history |
| Recovery size | Endpoint comparison shows 62 changed files; this is not one ready-to-merge feature |
| Temporary checkout | Git retains the branch and objects, but its former working checkout is not intact |
| Reference checkout | Contains substantial modified, untracked, and ignored work; a Git bundle alone would not preserve it |

Compare endpoint trees and individual commits, not only a three-dot diff: the
recovery history has multiple merge bases. Do not replay already-merged fixes.

## 1. Preserve and inventory

1. Create a restricted, persistent recovery location outside the existing working
   trees. Preserve the relevant Git refs/objects with a verified bundle.
2. Preserve staged and unstaged changes separately, including binary patches,
   untracked-file manifests, and required ignored artifacts. Classify sensitive
   source, configuration, databases, and logs separately; do not publish them.
3. Verify backup integrity and recovery of representative files. Snapshot active
   databases consistently rather than copying a live database without its state.
4. Create a new permanent recovery worktree without pruning stale metadata first.
5. Refresh upstream and classify changes as already merged, independently reusable,
   tightly coupled, or deferred. Record exact PR ownership before editing.

Completion: committed history and required local changes are recoverable from
verified persistent backups; the current checkout remains unchanged.

## 2. Integrate source identity and Java/parity validation

Use the recovery branch as a donor rather than merging it wholesale.

### Focused PRs

| PR | Contents | Acceptance requirement |
|---|---|---|
| Source identity prerequisites | `OriginalSourcePath`, `SourcePathHelper`, removal of Java's shadowing `FilePath`, converter/process/context propagation | Directory and extension survive direct/chunked Java/C# output, attribution, and persistence |
| Honest REKT evidence | No fabricated copybooks; partial recovery, diagnostics, facts/cache/portal propagation; graph diagnostic exclusion | Missing definitions remain unresolved and other parser errors remain fatal |
| Java validation | Project scaffolding, real Maven compile, bounded repair, final parity remeasurement, configuration and documentation | Both conversion paths publish truthful build/parity outcomes |

Parser-related donor commits include `f2f6bd3`, `02c7721`, `211c89e`, `be82549`,
`22a687e`, and `1c4c9f3`, plus relevant parts of `f57f7bc`. Separate syntax support
from recovery/evidence changes where dependencies allow. Keep JCL commit `b4580f2`
for step 6.

The Java gate exists in `Helpers/JavaCompileGate.cs`; both migration processes
call it in the donor branch. Integration must additionally:

- Scaffold ordinary Java runs without depending on JCL generation. Preserve
  user-authored build files and retain the supported Java target unless a
  separate compatibility decision changes it.
- Confirm generated sources participate in the actual Maven compilation.
- Treat missing tooling/project files, dependency resolution errors, timeout,
  cancellation, and nonzero process exit as non-success.
- Bound repairs, reject forbidden identity/API changes, roll back worsening
  repairs, and remeasure parity against final output.
- Preserve source maps, build logs, compile status, parity evidence, and terminal
  status consistently. Avoid cross-run leakage through process-global exit state.

A compile pass cannot override incomplete conversion coverage. When coverage
cannot yet be established, report it as unknown rather than complete.

## 3. Integrate the AI Loop visualization

Reuse the circular workflow and card layout, not the reference implementation's
latest-text-log parsing or unrelated endpoints.

1. Extend the existing structured event contract in `Helpers/AiLoopEvents.cs`.
   Correlate run attempts, operations, requests, retries, and managed process IDs.
2. Instrument application-issued request starts and outcomes, context dispatch,
   and actual operation completion. Distinguish queue/backoff time from provider
   time, and cancellation from abandoned or unknown termination.
3. Extend `AiLoopReader` and `/api/ai-loop/*` with bounded live requests, phases,
   context handoffs, freshness, and telemetry coverage. Preserve historical
   compatibility: unavailable fields mean "not recorded", not zero.
4. Port the visualization into `wwwroot/ai-loop.js`, retaining run selection,
   compile/parity/JCL gates, and existing start-run controls.
5. Use explicit run correlation, serialized polling, response-generation guards,
   and cancellation of obsolete fetches. Preserve controls and scroll position.

Only animate stages supported by events. REKT context reused by a conversion is
not proof that parsing ran during that conversion. Do not store prompts, source
text, credentials, or response bodies in telemetry.

Completion: delayed responses for run A cannot replace selected run B; retry and
resume counts remain accurate; failed or killed work never appears completed.

## 4. Integrate chunked output assembly

Do not copy the candidate assembler unchanged. It expects complete compilation
units, while first/middle/last prompts request continuation fragments. It can also
discard conflicting methods and leave file-level success stale after rejection.

1. Decide the chunk-output contract before porting the assembler. The proposed
   direction is complete, mergeable units with a stable enclosing-type identity;
   update every affected prompt and consumer together. Retain an explicit legacy
   path if compatibility is required, not heuristic guessing.
2. Validate expected chunk coverage, ordering, source ownership, and usable output.
   Preserve nested and multiple top-level types.
3. Deduplicate identical members only. Reject incompatible headers, packages, or
   differing implementations with diagnostics naming their contributing chunks.
4. Return assembly diagnostics and provenance explicitly. Recalculate file/run
   acceptance after assembly, not before.
5. Retain rejected/raw output as diagnostics, separate from compilable source.
   Remaining files compiling successfully must not hide missing chunks.

Land source identity from step 2 first. Bring forward collision preflight from
step 5 before production multi-file writes. Keep broader service-layout changes
deferred.

## 5. Integrate remaining source/output handling

- Reuse the candidate source reader only with a documented encoding policy.
  UTF-8/Latin-1 handling is not general EBCDIC or UTF-16 support. Wire all relevant
  COBOL readers consistently and preserve meaningful source bytes/positions.
- Identify owners by source-relative directory, extension, and role. Preflight
  output collisions before writing, including case-insensitive filesystem risks.
- Prefer explicit collision failure initially over order-dependent suffixes.
  Any future automatic rename must update all references consistently and leave
  comments/literals unchanged.
- Harden generated-code extraction against comments, text blocks, truncated
  restarts, and multiple valid units. Do not silently keep one and discard others.
- Preserve upstream package/namespace mapping and actual writer-returned paths.
  Do not import service registries or force a new package architecture.
- Carry one persistent run output root through writing, repair, reporting, source
  maps, and reload. Different runs must not overwrite each other.

Source-reader work can proceed independently after preservation. Identity and
collision prerequisites must land before accepting new assembly output.

## 6. Finish JCL discovery and documentation

Recover the focused `b4580f2` change, resolve its changelog context against main,
and close the integration gaps rather than only replaying its tests.

- Discover COBOL, JCL, or both through `rekt-full`; keep extraction/ingestion
  separate from executable job scaffolding.
- Validate configured roots, Windows/Git Bash paths, spaces, and case-insensitive
  recursive `.jcl` discovery. Preserve the current COBOL-root restriction unless
  deliberately expanded.
- Carry the actual JCL source root into ingestion and source retrieval. External
  jobs must not obtain source text from an unrelated `source/` file.
- Prevent deleted or failed scans from exposing stale facts in JCL-only and mixed
  scans. Publish validated scan output without deleting retained run evidence.
- Replace misleading "utilities only" summaries for unresolved procedures.
  Separate discovered, generated, buildable, and executable states, including
  unsupported utilities.
- Recreate the README IMPORTANT source-folder box and explain the single scan
  command. Align JCL documentation and the changelog.

Exercise actual command dispatch and failure propagation with controlled
executables, plus real CLI extraction and graph-consumer cases. Function-extraction
tests alone do not establish the `rekt-full` contract. Include Windows CI coverage.

## 7. Run the acceptance pilot

First use controlled model responses through real orchestration, then run an
explicitly authorized live pilot.

| Target | Direct | Forced chunked |
|---|---|---|
| Java | Actual generated project build, source map, final parity and status | Same, plus complete assembly and chunk provenance |
| C# | Actual generated project build, source map, final parity and status | Same, plus complete assembly and chunk provenance |

Across the matrix, cover duplicate basenames and program/copybook stems; real
method bodies; absent output; missing dependencies; malformed/empty responses;
failed middle chunks; repair success and rollback; cancellation; and strict
parity failure. Verify CLI, persisted state, reports, portal, and post-restart
reload agree.

Use existing test projects and focused selectors. Configure isolated storage and
external boundaries before starting web hosts. Fake-compiler tests do not replace
actual generated-project builds.

The live pilot requires approved provider cost/time limits and isolated run-owned
database/container state. Preserve commit/input hashes, redacted configuration,
logs, actual build results, and gate evidence under persistent output.

Behavioral equivalence is a separate acceptance claim requiring trusted expected
results or an original runner. Compare outputs, data effects, return codes, and
relevant encoding/numeric behavior. Without that oracle, report "not established".

## Delivery order and estimate

The requested feature order remains Java/parity, AI Loop, assembly, remaining
source/output work, JCL, and acceptance. Identity/collision prerequisites must be
brought forward where required; independent preparation can run in parallel.
Overlapping process/writer files need coordinated ownership.

| Work package | Planning estimate |
|---|---|
| Preservation and inventory | 1-2 engineer-days |
| Identity, REKT evidence, Java/parity integration | 7-11 engineer-days |
| Reliable live telemetry, reader and AI Loop visualization | 8-11 engineer-days |
| Assembly and remaining source/output improvements | 8-14 engineer-days, excluding identity already counted |
| JCL completion and documentation | 2-4 engineer-days |
| Deterministic acceptance and evidence review | 2-3 engineer-days |

Allow roughly **30-45 engineer-days** for the full reliability scope, not a visual
prototype. These estimates are provisional, include focused regression work, and
exclude review delays, live-provider waiting, and estate-specific remediation.
Parallel execution reduces elapsed time, not total engineering effort.

## Decisions for review before implementation

1. Approve focused recovery/integration PRs rather than a wholesale branch merge.
2. Agree the chunk protocol and any compatibility path before prompt changes.
3. Prefer collision rejection initially; expand automatic renaming only if needed.
4. Retain the current Java target, encoding scope, and COBOL-root restriction
   unless representative requirements justify widening them.
5. Define JCL zero-job/unreadable-input behavior explicitly; the recommendation is
   visible non-success for an explicitly selected scan with no usable jobs.
6. Agree live-pilot scope, resource limits, and available behavioral oracle.
