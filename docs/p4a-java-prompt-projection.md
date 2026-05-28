# PR4.a — Java Converter Prompt Projection (program-facts.json)

**Last updated**: 2026-05-28
**Status**: Complete. 239 / 239 tests pass.

## Summary

Wires the PR3 `program-facts.json` schema into the **Java converter prompt** as
an opt-in replacement for the raw REKT context block. First production
consumer of the curated facts contract; pattern will mirror to the C#
converter (PR4.b) and other agents once validated.

## What ships

### `Helpers/PromptProjections/JavaConverterProjection.cs`

Pure projection from `ProgramFacts` → prompt block. Three public surfaces:

```csharp
public static bool IsEnabled();        // env: _USE_PROGRAM_FACTS=true
public static ProgramFacts? TryLoad(string factsDir, string programBasename);
public static string BuildPromptBlock(ProgramFacts facts);
```

- `IsEnabled()` is a pure function of `_USE_PROGRAM_FACTS`. No AsyncLocal, no cached state — matches the explicit-ownership rule from P0.
- `TryLoad` returns `null` on missing file, corrupt JSON, or any read error (fail-open).
- `BuildPromptBlock` emits a deterministic block starting with the exact marker the existing cache-key extractor pins (`"REKT STRUCTURAL CONTEXT (authoritative — use this as the conversion blueprint):"`). This means **no cache-key code change is needed** — the response cache automatically invalidates when facts change because the hashed substring is now the projection content.

### `Agents/JavaConverterAgent.cs`

One inline branch added inside the existing `ENABLE_REKT_CONTEXT` block:

1. If `_USE_PROGRAM_FACTS=true` **AND** `<stem>.facts.json` exists under `output/rekt/`, the projection is appended to the user prompt and the raw-AST path is skipped for this program.
2. Otherwise, the existing raw-AST path runs unchanged (the spec's "raw-AST fallback behind feature flag" requirement).

Per-program decision is logged:
- HIT: `Injected program-facts projection for {File} (schema=N, confidence=X, warnings=N)`
- MISS: `_USE_PROGRAM_FACTS=true but no facts.json for {File} — falling back to raw-AST path`

### Files modified

```
Helpers/PromptProjections/JavaConverterProjection.cs           (new)
Agents/JavaConverterAgent.cs                                   (one inline branch)
CobolToQuarkusMigration.Tests/PromptProjections/JavaConverterProjectionTests.cs  (11 tests)
docs/p4a-java-prompt-projection.md                              (this file)
```

## Cache-key behaviour (PR1 integration)

Confirmed by inspection and the existing test surface:

1. `JavaConverterAgent.ExtractRektContextBlock` (in PR1.b) scans the user prompt for `"REKT STRUCTURAL CONTEXT (authoritative"` and copies up to `"IMPORTANT REQUIREMENTS:"`.
2. The projection block starts with that exact marker and the agent appends `"IMPORTANT REQUIREMENTS:"` immediately after — so extraction captures the **projection content**.
3. `JavaConverterCacheKeys.ForConversion` already hashes that substring as `RektFactsHash`.
4. Consequence: **changing facts (PR3) → different projection block → different `RektFactsHash` → response cache (PR1) automatically invalidates.** Zero cache-key code change.

`BuildPromptBlock_DistinctOutputForDistinctFacts` and `BuildPromptBlock_StableAcrossRunsForSameInput` pin this.

## End-to-end behaviour

Without PR4.a (existing default):
```
ENABLE_REKT_CONTEXT=true  →  RektContextLoader reads flow-ast / flow-data / *-deps
                              → RektContextFormatter.ToPromptBlock(sc) appended
                              → ~12–18K tokens of raw AST + structural rules
```

With PR4.a opt-in:
```
ENABLE_REKT_CONTEXT=true _USE_PROGRAM_FACTS=true
  →  JavaConverterProjection.TryLoad(output/rekt/, PROG.cbl)
     ├─ found → BuildPromptBlock(facts) appended (~2–4K tokens of curated facts)
     └─ missing → raw-AST fallback (unchanged)
```

Token-reduction estimate vs. raw-AST path: **~60–80% smaller structural block**
for typical programs. The full prompt also contains the COBOL source and
business-logic context, which the projection does not change.

## Test coverage (11 new)

| Test | What it pins |
|---|---|
| `IsEnabled_DefaultsOff` | Behaviour-preserving default. |
| `IsEnabled_TrueOnlyForExactTrueString` | Env convention: `true`/`TRUE` enables; `1`/`false` does not. |
| `TryLoad_ReturnsNullWhenMissing` | Fail-open on missing facts file. |
| `TryLoad_ReturnsNullOnCorruptJson` | Fail-open on parse error. |
| `TryLoad_RoundTripsValidFile` | Schema serialisation works end-to-end. |
| `BuildPromptBlock_StartsWithCacheKeyMarker` | **Contract with PR1.b** — protects cache-key extraction. |
| `BuildPromptBlock_SurfacesAllStructuralFacts` | Every fact category renders (summary, data, copybooks, IO, callees, callers, control flow, effects, warnings, preprocess notes). |
| `BuildPromptBlock_PreservesFactLockingRules` | Fact-locking + DTO rules + "(none)" markers are present even when sections are empty. |
| `BuildPromptBlock_DistinctOutputForDistinctFacts` | **Contract with PR1.b** — different facts → different block → different cache key. |
| `BuildPromptBlock_StableAcrossRunsForSameInput` | Determinism for cache stability. |
| `BuildPromptBlock_SchemaVersionVisibleToHumanReaders` | Schema + identity version surfaced for debugging. |

## Compatibility concerns

1. **Opt-in only.** Default behaviour is unchanged: without `_USE_PROGRAM_FACTS=true`, the raw-AST path runs as before. Users who don't set the env var see zero behaviour change.
2. **Per-program fallback.** If the env var is on but `<stem>.facts.json` is missing for one program, that program silently falls back to raw-AST. The log line makes the fallback visible.
3. **Marker string is the contract.** Any future change to the projection's leading marker MUST update `JavaConverterAgent.ExtractRektContextBlock` in lockstep, or cache keys silently change. Pinned by `BuildPromptBlock_StartsWithCacheKeyMarker`.
4. **Operation ordering is now alphabetical** in the projection block (`CLOSE, OPEN, READ` not `OPEN, READ, CLOSE`). Acceptable — this is a NEW path; no prior agent behaviour depends on the order.
5. **No changes to the raw-AST path** — same code, same logs, same cache behaviour when the projection isn't used.

## Remaining technical debt (delta from PR3)

1. **Other agents not yet wired** — C# converter (PR4.b), reverse-engineering, architecture, BIAN tagger. Each is a small repeat of this PR with the same `IsEnabled` / `TryLoad` / `BuildPromptBlock` shape; PR4.b can copy 90% of the JavaConverterProjection code.
2. **No live-run validation in CI** — projection contents tested deterministically, but the actual LLM output quality vs. raw-AST path is a manual A/B comparison. Recommend one warm + one cold run of `BDSDA2F` with `_USE_PROGRAM_FACTS=true` before declaring victory.
3. **Projection does not yet differentiate by `Confidence`** — a `Low`-confidence facts file produces the same block as a `High`-confidence one (just with the confidence value displayed). Future improvement: degrade gracefully (e.g. add a "PROCEED WITH CAUTION" note) when confidence is `Low`.
4. **Doctor.sh does not auto-run `program-facts extract`** after `rekt-full` yet — the facts files only appear when the user runs the CLI explicitly. PR3.b can mechanise this (`_PROGRAM_FACTS=true` env var → after-parse hook).
5. **No metric on cache-hit rate by projection-vs-raw** — both paths share the same cache, so observed hit rates aren't broken out. Add a tag to the cache log if needed.

## Architectural risks discovered during PR4.a

1. **The marker string is now load-bearing.** Three places depend on it: the projection emits it, the cache-key extractor scans for it, the raw-AST path also emits it. Renaming the marker safely requires updating all three plus the test that pins it. Acceptable for now (one test fails loudly); flag in code review when prompts evolve.
2. **Projection and raw-AST emit slightly different content** — the raw-AST path includes `RektContextFormatter.ToPromptBlock` output (different formatting). Cache keys are NOT compatible across paths: switching `_USE_PROGRAM_FACTS` from off→on (or vice versa) invalidates all cached entries for affected programs. Correct behaviour (the input shape changed) but worth knowing.
3. **`TryLoad` swallows all exceptions.** Matches the cache layer's fail-open discipline. A corrupt facts file is silently treated as missing. Recommend a one-line `_logger?.LogDebug` in `TryLoad` if drift becomes a concern; today the agent log line distinguishes "no facts.json" from a successful load.
4. **`IsEnabled` is read once per call.** Toggling the env var mid-run causes some programs to use the projection and others not. Intentional for testing flexibility; documented.
5. **The projection does not yet include `relativePath`** in the cache-key extraction substring (it's mentioned in the block when present). When ProgramKey migration lands and the identity scheme bumps, both the projection content **and** `CacheKey.IdentitySchemeVersion` change — double invalidation is fine, but worth tracking that the cache key already has its own identity-scheme field independent of the projection content.

## Next steps

- **PR4.b (small)** — mirror to C# converter. ~40 lines: new `CSharpConverterProjection.cs` (copy + tweak DTO/CALL rules for .NET idioms), one-branch wire-in to `CSharpConverterAgent`, mirror test file. Estimated 30 min once the Java path is validated against a real run.
- **PR4.c (later)** — reverse-engineering / architecture / BIAN agents. Each is a smaller projection (only the fields they need); same pattern.
- **PR3.b** — `doctor.sh` hook to auto-extract facts after `run_rekt_parse` succeeds. Trivial.
- **PR5** — preprocessor hardening + `.preprocess.json` sidecars. Reader already in place (PR3); writer is PR5.

Validation recommendation before PR4.b: one warm A/B comparison run.

```sh
# A) baseline (no facts)
ENABLE_REKT_CONTEXT=true _LLM_CACHE_ENABLED=true \
  ./doctor.sh convert-only --program BDSDA2F --target java

# generate facts
dotnet ... program-facts extract source/.rekt-staging \
  --rekt-dir output/rekt --scan-cache-db Data/rekt-scan.db

# B) projection (with facts)
ENABLE_REKT_CONTEXT=true _USE_PROGRAM_FACTS=true _LLM_CACHE_ENABLED=true \
  ./doctor.sh convert-only --program BDSDA2F --target java
```

Compare: token counts in the API call log, generated Java structure, runtime
duration. If B is materially smaller without quality regression, PR4.b is safe
to copy the pattern to C#.
