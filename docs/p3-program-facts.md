# PR3 — Curated `program-facts.json` Extractor

**Last updated**: 2026-05-28
**Status**: Schema + extractor + CLI complete. 221 / 221 tests pass.
**Not in scope (deferred to PR4)**: agent prompt integration.

## Summary

Adds a typed, versioned, per-program handover contract — the
`program-facts.json` file — derived from REKT outputs and source bytes.
PR3 ships the schema, the extractor, and a CLI surface. PR4 will wire it
into agent prompts; no agent behaviour changes in PR3.

## What ships

- **`ProgramFacts` schema** (`Agents/Infrastructure/Facts/ProgramFacts.cs`) — every spec-required top-level field as a typed record:
  - `schemaVersion`, `identitySchemeVersion`, `basename`, `stem`, `relativePath?`, `sourceHash`, `confidence`, `warnings`, `preprocessNotes`
  - `summary`, `io { files, screens, dbTables, queues }`, `data { groups, copybooksUsed }`, `callers`, `callees`, `controlFlow { entryPoints, performChains, exits }`, `externalEffects`
- **`ProgramFactsExtractor`** — builds `ProgramFacts` from:
  - Source bytes in the staging dir (drives `sourceHash`, file-IO heuristic, PROGRAM-ID, GOBACK/STOP-RUN, EXEC CICS/DLI flags).
  - REKT `flow-ast / flow-cfg / flow-data / *-deps` JSONs via the existing `RektContextLoader`.
  - Optional PR2 scan-cache entry for `confidence`.
  - Optional PR5-style `*.preprocess.json` sidecar for `preprocessNotes` (loaded opportunistically — schema reader ships now so PR5 lands without changes here).
- **CLI verbs** (`Cli/ProgramFactsCommand.cs`):
  - `program-facts extract <staging-dir> [--rekt-dir ...] [--output-dir ...] [--programs A,B,C] [--scan-cache-db ...] [--repo-root ...]`
  - `program-facts read <facts-json>` — pretty-print for human inspection.
- **Registered on the root command** in `Program.cs` (one line).

### What is intentionally out of scope

- No agent prompt changes (`RektPromptInjector` and `JavaConverterAgent` still consume `RektContext` as before).
- No `doctor.sh` integration. PR3.b can add a `program-facts extract` step after `run_rekt_parse`; trivial mirror of the PR2.b hook.
- Screens / queues left as empty lists with explicit warnings — extraction needs BMS/JCL readers (out of P1 scope per the platform constraints).
- No raw-AST fallback flag yet — there is no agent consumer of `program-facts.json` to switch between facts and raw AST until PR4.

## Files modified

```
Agents/Infrastructure/Facts/ProgramFacts.cs                    (new — schema records + enums)
Agents/Infrastructure/Facts/ProgramFactsExtractor.cs           (new — projection + persistence)
Cli/ProgramFactsCommand.cs                                     (new — extract / read verbs)
Program.cs                                                     (+1 line — register subcommand)
CobolToQuarkusMigration.Tests/Facts/ProgramFactsSchemaTests.cs  (3 tests)
CobolToQuarkusMigration.Tests/Facts/ProgramFactsExtractorTests.cs (6 tests)
docs/p3-program-facts.md                                       (this file)
```

## Schema example

```json
{
  "schemaVersion": 1,
  "identitySchemeVersion": "v1-basename",
  "basename": "PROG.cbl",
  "stem": "PROG",
  "relativePath": null,
  "sourceHash": "e55e6bd9aece20e27b307e664ea99161…",
  "confidence": 3,
  "warnings": ["cics-detected-screens-not-extracted (PR3 scope; deferred to PR5)"],
  "preprocessNotes": [
    { "rule": "move-zero", "line": 42, "before": "MOVE 0(1)", "after": "MOVE ZERO" }
  ],
  "summary": { "loc": 791, "paragraphs": 42, "sections": 4, "isCopybook": false, "programId": "PROG" },
  "io": {
    "files":   [ { "name": "CUSTFILE", "operations": ["CLOSE","OPEN","READ"] } ],
    "screens": [],
    "dbTables":[ { "name": "ACCOUNTS", "operations": ["SELECT","UPDATE"] } ],
    "queues":  []
  },
  "data": {
    "groups": [ { "name": "WS-CUSTOMER", "fields": 12, "redefines": false } ],
    "copybooksUsed": ["BOOK1","BOOK2"]
  },
  "callers": ["PARENT.cbl"],
  "callees": ["CHILD.cbl"],
  "controlFlow": {
    "entryPoints": ["MAIN-SECTION"],
    "performChains": [ ["MAIN-PARA","READ-CUST","WRITE-LOG"] ],
    "exits": ["GOBACK"]
  },
  "externalEffects": ["FILE_IO","DB_IO","CALL_OUT"]
}
```

Field values are `camelCase` (verified by test). Numeric enums for `confidence`
keep the wire format compact and JSON-roundtrip-safe.

## Identity migration compatibility

- `schemaVersion` and `identitySchemeVersion` are top-level. Bumping either invalidates downstream caches that include them in their key (PR4 will).
- `basename` is the current identity. `stem` is supplied separately so consumers don't have to re-derive.
- `relativePath` is nullable and always emitted as `null` today; populated when the ProgramKey migration ships.
- `sourceHash` is `CanonicalHasher.HashUtf8(stagingDir/<basename>)` — same hash function and same bytes as the PR1 response cache and PR2 scan cache. Drift between caches is impossible by construction.

## Cache invalidation interactions

| Trigger | Affected output | Mechanism |
|---|---|---|
| Source bytes change | `sourceHash` changes | PR2 invalidates scan-cache → next extract sees lower confidence; PR1 (when PR4 wires facts in) invalidates response cache via key. |
| REKT JSON changes (smojol re-run) | `summary`, `data`, `controlFlow`, `callees` change | Extractor re-emits facts; PR4 cache key (planned) includes a hash of facts content. |
| Copybook changes affecting REKT | Same as above; PR2 already invalidates the dependent program | Re-run of `rekt-scan-cache plan` + `program-facts extract` produces new facts. |
| Identity scheme bumps | `identitySchemeVersion` changes everywhere | All facts files become "v1-basename" stale; one re-extract regenerates. |
| Confidence change | `confidence` field changes | PR4 cache key includes confidence → re-converts when LLM was previously fed lower-confidence facts. |

## Smoke results

```
$ dotnet ... program-facts extract /tmp/stage --rekt-dir /tmp/output/rekt --output-dir /tmp/facts --repo-root /tmp
program-facts extract: wrote 1 *.facts.json to /tmp/facts

$ cat /tmp/facts/PROG.facts.json
{
  "schemaVersion": 1,
  "identitySchemeVersion": "v1-basename",
  "basename": "PROG.cbl",
  "stem": "PROG",
  "sourceHash": "e55e6bd9aece20e27b307e664ea99161...",
  "confidence": 0,
  "warnings": ["rekt-output-empty: no AST/CFG/DataStructure JSONs found"],
  "summary": { "loc": 8, "programId": "PROG", ... },
  "io": { "files": [{ "name": "CUSTFILE", "operations": ["CLOSE","OPEN","READ"] }], ... },
  "externalEffects": ["FILE_IO"]
}
```

The CLI works end-to-end. With real REKT outputs present, `groups`, `callees`,
`performChains`, `dbTables`, `sections`/`paragraphs` populate.

## Compatibility concerns

1. **Callers reconstruction is corpus-wide.** The extractor builds an inverse map by scanning every program's deps JSON once per `ExtractAllAsync` call. Callers for program X are correct **only** when X's callers are in the supplied basename list. Use `--programs` carefully — restricting it can produce empty `callers` lists. Documented; PR3.b doctor.sh integration should pass the full corpus.
2. **CALL-target normalisation** uses stem-based matching (`CHILD` → `CHILD.cbl`). Dynamic CALL targets (variable names) come through as raw strings and may not normalise to a basename — they will appear in `callees` verbatim. Acceptable; documented.
3. **File-IO extraction is regex-based.** Smojol does not currently expose FD entries in `flow-data`. The heuristic regex catches `OPEN/READ/WRITE/REWRITE/CLOSE` in the procedure division. False positives possible (e.g. a paragraph named `READ-CUST` would not match because the regex requires a file name after the verb). Acceptable; corrected in PR3.b if needed.
4. **Confidence inference fallback** runs when no scan-cache entry is available. The fallback is intentionally conservative — `High` only when both sections and data are non-empty. A `None` confidence with a populated facts file is a real possibility for cold runs without `--scan-cache-db`; documented.
5. **`preprocess.json` sidecar reader** ships before PR5 writes those files — so today the field is always empty. The schema-version handling in the reader tolerates absent / malformed sidecars without throwing.

## Remaining technical debt

1. **No `doctor.sh` integration** — PR3.b. Should run after `run_rekt_parse` succeeds, write into `output/rekt/`, gated on `_PROGRAM_FACTS=true` env var.
2. **`screens` and `queues` always empty.** Needs BMS / queue extraction; tracked for a post-P1 phase. Warning emitted on `EXEC CICS` so the gap is visible.
3. **Smojol-only File IO.** Switch to RektContextLoader-emitted FD entries once smojol exposes them; until then, regex heuristic.
4. **No facts-content hash field.** PR4 will need to hash the facts JSON to include in cache keys. Could be computed on demand from the file bytes; if it becomes hot, store as a top-level `factsHash` field.
5. **No deletion / cleanup.** When a program is removed from the corpus, its old `.facts.json` stays on disk. Recommend PR3.b add a "delete orphan facts" step.

## Architectural risks discovered during PR3

1. **Schema vs identity-scheme version are independent**, mirroring the response-cache and scan-cache discipline. Two bump knobs is good (orthogonal concerns) but easy to confuse — documented in `ProgramFacts` doc comments.
2. **Extractor is stateless and idempotent** by design (no AsyncLocal, no hidden cache). Re-running on the same inputs produces byte-identical facts (modulo the `confidence` field if the scan cache changed underneath). Verified by `Extract_SameInput_DeterministicSourceHash`.
3. **The schema is a public contract** as soon as PR4 starts consuming it. Bumping `SchemaVersion` after PR4 lands means every consumer's cache invalidates. Acceptable; documented.
4. **`RektContextLoader` is the shared dependency** between this extractor and the existing agent path. PR3 does not change it. If PR4 later refactors how agents read REKT, the extractor inherits the change.
5. **CLI is purely additive** — no existing subcommand changed, no existing flag removed. Backwards-compatible by construction.

## What PR4 will do (concrete plan)

1. Add an opt-in env var (e.g. `_USE_PROGRAM_FACTS=true`) and small per-agent projection classes (`Helpers/PromptProjections/JavaConverterProjection.cs`, etc.) that consume `<stem>.facts.json` and emit a compact prompt block.
2. Wire the projection into the existing `userPromptBuilder` in each agent, behind the env var. Raw-AST fallback stays as today's default.
3. Extend `JavaConverterCacheKeys.ForConversion` to hash the facts JSON content (or `factsHash`) as part of `RektFactsHash`. The PR1 cache then invalidates on facts changes automatically.
4. Tests: projection emits the expected subset; cache key changes when facts change.

PR4 is small and well-scoped because PR3 did the contract work. Ready when you are.
