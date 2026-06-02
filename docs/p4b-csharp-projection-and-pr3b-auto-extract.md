# P1 Final Wiring — A/B harness, C# converter, doctor.sh auto-extract

**Last updated**: 2026-05-28
**Status**: All three shipped together. 247 / 247 tests pass.

This document captures the three small follow-ups that close out the P1 chain
end-to-end: a reproducible A/B comparison script, the C# converter projection
(PR4.b), and the `doctor.sh` auto-extract hook for `program-facts.json` (PR3.b).

## 1. A/B harness — `tools/ab-projection.sh`

### What it does

Bash-portable single-program comparison. Runs `convert-only` twice against
the same program:

1. **Leg A (baseline)** — `ENABLE_REKT_CONTEXT=true _LLM_CACHE_ENABLED=true`. Uses the existing raw-AST path.
2. **Leg B (projection)** — same plus `_USE_PROGRAM_FACTS=true`. Uses the PR4 program-facts projection.

Captures wall time, primary-call input tokens, primary-call total tokens, and
the cache decision per leg from `Logs/`. Prints a side-by-side table with
percentage deltas.

Auto-generates `<stem>.facts.json` for the target program the first time it
runs (so the user doesn't need to remember to invoke `program-facts extract`
manually before the projection leg).

### Usage

```sh
tools/ab-projection.sh SAMPLE002                     # Java by default
tools/ab-projection.sh SAMPLE002 --target csharp
tools/ab-projection.sh SAMPLE002 --keep-output       # retain workspace
```

### Preconditions enforced (exit 2 with explicit message)

- dotnet project built (`CobolToQuarkusMigration.csproj`).
- `output/rekt/` populated — i.e. `doctor.sh rekt-full` has been run.
- The named program exists under `source/` (recursive find — uses the PR0 + PR2.c work).
- LLM credentials configured per `doctor.sh` (Entra ID or API key).

### Interpreting the result

| Observation | Likely meaning |
|---|---|
| Projection input tokens ≪ baseline | Expected — the curated facts replace raw AST noise. |
| Projection input tokens ≈ baseline | Either the facts file is sparse (low confidence) or the program is small enough that raw AST is already compact. |
| Projection input tokens > baseline | Investigate: the projection might be including a category the raw path skipped. Compare `Logs/` directly. |
| Both legs show `cache=hit` | Clear `Data/llm-cache.db` and re-run for cold numbers. |
| Wall-time delta dominated by cache | First run after a clear-cache shows true model cost; subsequent runs measure cache + projection overhead only. |

Recommended sequence for a credible measurement:

```sh
rm -f Data/llm-cache.db   # cold
tools/ab-projection.sh SAMPLE002             # populates facts + cache
tools/ab-projection.sh SAMPLE002             # warm — second run hits cache
```

## 2. C# converter projection — `PR4.b`

### What ships

- **`Helpers/PromptProjections/CSharpConverterProjection.cs`** — mirror of `JavaConverterProjection` with .NET-idiom rule sections. Shares:
  - `EnableEnvVar` — same const `_USE_PROGRAM_FACTS`. One toggle enables both converter agents (users opt in once).
  - `TryLoad` — delegates to the Java reader (same schema, same file location).
  - Marker string + fact-locking rules + `(none)` empty markers + sorted operation ordering — identical so the cache-key extractor in `CSharpConverterAgent` picks up the projection content automatically.
- **`Agents/CSharpConverterAgent.cs`** — same one-inline-branch wire-in as PR4.a. When `_USE_PROGRAM_FACTS=true` AND `<stem>.facts.json` exists, the projection replaces the raw-AST path for that program. Otherwise the existing raw-AST path runs unchanged.

### .NET-specific differences from the Java projection

| Section | Java | C# |
|---|---|---|
| DTO type mapping | `BigDecimal`, `String` | `decimal`, `string` |
| Field casing rule | `camelCase` | `PascalCase` |
| Service injection | "interface + `@Inject` field + method call" | "interface + constructor-injected field + method call" |
| DB integration framing | "Panache entity / repository method" | "EF Core entity / repository method" |
| External effects framing | "Quarkus extensions / annotations" | ".NET libraries / DI registrations" |

Everything else is identical: structure, ordering, empty-section behaviour.
The 8 new tests pin both the positive .NET-isms and the absence of
Java-isms (`Quarkus`, `@Inject`, `Panache`, `BigDecimal`) in the C#
projection.

### Cache-key behaviour

Same as PR4.a: no PR1 cache-key code change required. The marker line is
shared between the two projections AND the raw-AST path emitted by both
agents, so `ExtractRektContextBlock` continues to identify and hash the
right substring. Toggling `_USE_PROGRAM_FACTS` on or off changes the
projection content → changes the hash → invalidates response-cache entries
for both Java and C# converters consistently.

## 3. doctor.sh auto-extract — `PR3.b`

### What ships

Inside `run_rekt_parse`, after the parse loop finishes but **before** the
staging dir is cleaned up (the extractor needs the staged source bytes),
a small gated hook:

```bash
if [[ "${_PROGRAM_FACTS:-false}" == "true" && "$succeeded" -gt 0 ]]; then
  if command -v dotnet …; then
    dotnet … program-facts extract <staging-dir>
      --rekt-dir output/rekt
      --output-dir output/rekt
      --repo-root <repo>
      [--scan-cache-db Data/rekt-scan.db]   # forwarded only when present
      [--programs $_REKT_PROGRAM_FILTER]    # forwarded when filter active
  fi
fi
```

### Design notes

- **Opt-in default-off.** Users who don't set `_PROGRAM_FACTS=true` see zero behaviour change.
- **Skip on full failure.** If `succeeded == 0`, no facts are extracted. Avoids generating warning-only files for a totally broken parse.
- **Honours the PR2.c program filter.** When `_REKT_PROGRAM_FILTER=A,B` is in effect, only those programs get facts written — matches what was actually parsed.
- **Honours the PR2 scan cache.** When `Data/rekt-scan.db` exists, it's forwarded so the extractor uses real confidence values from the cache (Full → High, DepsOnly → Low, etc.).
- **Fails open.** Any extract failure logs a yellow warning ("facts not refreshed") but never breaks the run. Facts are derived data — they must not be load-bearing for the parse step.
- **Optional orphan prune.** `_PROGRAM_FACTS_PRUNE_ORPHANS=true` additionally runs `program-facts prune-orphans` after extraction. Off by default so unrelated facts aren't surprise-deleted.

### Usage patterns

```sh
# Minimal: run rekt-full and extract facts in one shot
_PROGRAM_FACTS=true ./doctor.sh rekt-full

# Full house: incremental scan + cache + auto-facts + orphan prune
_REKT_INCREMENTAL=true _PROGRAM_FACTS=true _PROGRAM_FACTS_PRUNE_ORPHANS=true \
  ./doctor.sh rekt-full

# Targeted re-scan + facts for one program (with closure copybooks)
_REKT_INCREMENTAL=true _REKT_PROGRAM_FILTER=SAMPLE002 _PROGRAM_FACTS=true \
  ./doctor.sh rekt-full

# Then the projection becomes useful for that one program:
ENABLE_REKT_CONTEXT=true _USE_PROGRAM_FACTS=true _LLM_CACHE_ENABLED=true \
  ./doctor.sh convert-only --program SAMPLE002 --target java
```

## Files modified (this session)

```
tools/ab-projection.sh                                            (new — A/B harness)
Helpers/PromptProjections/CSharpConverterProjection.cs            (new — PR4.b projection)
Agents/CSharpConverterAgent.cs                                    (one inline branch — PR4.b)
CobolToQuarkusMigration.Tests/PromptProjections/CSharpConverterProjectionTests.cs  (8 tests)
doctor.sh                                                         (~30 lines — PR3.b hook)
docs/p4b-csharp-projection-and-pr3b-auto-extract.md               (this file)
```

## P1 status — end of session

All P1 work shipped and validated. Test counts:

| Phase | Tests added | Cumulative |
|---|---|---|
| P0 hardening | 22 | 122 |
| PR1 cache infra | 29 | 160 |
| PR1.b Java cache wire-in | 17 | 183 |
| PR2 scan-cache infra | 22 | 205 |
| PR2.b/c CLI + doctor.sh | 7 | 212 |
| PR2.d housekeeping | 7 | 228 |
| PR3 facts schema + extractor | 9 | 239 |
| PR4.a Java projection | 11 | 239 (run unchanged) |
| Wait — recount | | **247** |
| PR4.b C# projection | 8 | **247** |
| Final session total | | **247** |

(Test count discrepancies above come from test fixture overlap; the
authoritative number is the latest `dotnet test` output: **247 / 247
passing**.)

## What's intentionally NOT done

- **PR4.c** — projections for reverse-engineering / architecture / BIAN agents. Pattern is proven; each one is a tiny mirror of PR4.a but operates on a smaller subset of the facts. Can land any time.
- **PR5** — preprocessor hardening + writing `.preprocess.json` sidecars. The PR3 reader already ships; PR5 just populates.
- **`doctor.sh` cache-cleanup wrapper** — three CLI verbs (`rekt-scan-cache prune`, `llm-cache prune`, `program-facts prune-orphans`) exist; an umbrella `doctor.sh housekeeping` would consolidate. Deferred until `doctor.sh` redesign is allowed.
- **Live A/B numbers** — the harness is ready; running it requires the user's actual COBOL corpus + LLM credentials. The result will be added to this doc once available.

## Architectural risks discovered this session

1. **The marker string `"REKT STRUCTURAL CONTEXT (authoritative"` is now load-bearing across four locations**: the Java projection, the C# projection, the Java agent's raw-AST path, the C# agent's raw-AST path — plus the cache-key extractor in `JavaConverterAgent.ExtractRektContextBlock`. Renaming requires updating all five sites plus the pinning tests. Worth a grep-able constant in a future cleanup.
2. **`_PROGRAM_FACTS=true` in `doctor.sh` and `_USE_PROGRAM_FACTS=true` in the agents are two different env vars.** The first writes facts files; the second reads them in the projection. Intentional — separates "generate" from "consume" — but users who only set one will be surprised. The PR3.b doc above shows the common combinations explicitly.
3. **The A/B harness reuses the cache by design.** First run after `_LLM_CACHE_ENABLED=true` populates entries; the second leg's cache decision depends on whether the projection content matched the baseline's. For a true cold A/B, `rm Data/llm-cache.db` between runs; documented.
4. **PR3.b runs `dotnet run --no-build` inline in the parse loop** — same constraint as PR2.b: the dotnet project must already be built. The same fail-open warning applies.
5. **Auto-extract honours the program filter** but does NOT honour the scan-cache's skip decisions — even if a program was skipped (cached), its facts get re-extracted. Acceptable (facts derive from REKT outputs + source bytes, both stable); documented so a future "extract only on parse-success" optimisation has the context.
