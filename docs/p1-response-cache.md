# P1 — Response Cache (PR1) and Roadmap for PR2–5

**Last updated**: 2026-05-27
**Status**: PR1 (deterministic response cache infrastructure) — complete and tested.
PR2–5 — designed; not yet implemented. See "Roadmap" section.

## What shipped in PR1

A deterministic, opt-in, SQLite-backed response cache with full observability.
**No agent code was modified** — wiring into specific call sites is intentionally
the next, smaller PR per agent so behaviour change is reviewable.

### Files added

```
Helpers/CanonicalHasher.cs
Agents/Infrastructure/Caching/CacheKey.cs
Agents/Infrastructure/Caching/CacheEntry.cs            (also: CacheLookupResult, CacheMissReason)
Agents/Infrastructure/Caching/LlmInvocationResult.cs
Agents/Infrastructure/Caching/IResponseCache.cs
Agents/Infrastructure/Caching/SqliteResponseCache.cs
Agents/Infrastructure/Caching/CachedLlmInvoker.cs
CobolToQuarkusMigration.Tests/Caching/CacheKeyTests.cs               (15 tests)
CobolToQuarkusMigration.Tests/Caching/SqliteResponseCacheTests.cs    (8 tests)
CobolToQuarkusMigration.Tests/Caching/CachedLlmInvokerTests.cs       (6 tests)
```

160 / 160 tests pass.

### Cache key — required fields

Every field is mandatory at construction; `CacheKey.Build()` validates and throws on
missing values.

| Field | Source | Why |
|---|---|---|
| ProviderKey | `ProviderCapabilities.ProviderKey` | Namespace per provider |
| Model | Caller | Different models, different outputs |
| SystemPromptHash | `CanonicalHasher.HashUtf8(systemPrompt)` | Any system-prompt change invalidates |
| UserPromptHash | `CanonicalHasher.HashUtf8(userPrompt)` | Any user-prompt change invalidates |
| ReasoningEffort | Caller (`"high"`/`"medium"`/`"low"`) | Different effort, different output |
| ResponseFormat | Caller (`"text"`, `"json"`, ...) | Different shape, different output |
| PromptTemplateId | Caller (`"java-converter"`, `"reverse-eng"`, ...) | Different template, different output |
| PromptTemplateVersion | Caller (manual bump) | Lets us invalidate when prompts evolve |
| TargetLanguage | Caller (`"java"`/`"csharp"`) | Different target, different output |
| FrameworkSettings | Caller (`"quarkus"`, `"dotnet"`, ...) | Different framework, different output |
| SourceHash | Hash of preprocessed COBOL bytes | Source change → cache miss |
| RektFactsHash | Hash of REKT context fed to prompt | REKT change → cache miss |
| GenerationSettingsHash | Hash of max_output_tokens + stop + top_p + seed | Generation knobs affect output |
| IdentitySchemeVersion | `"v1-basename"` (constant) | Future identity migration ≠ silent cache loss |
| Basename | Optional, for diagnostics | Logged on every lookup |
| RelativePath | Optional, forward-compat | Stored alongside; not in key today |

### Cache miss reasons (logged)

- `Disabled` — cache is off (default).
- `NonDeterministic` — caller declared `isDeterministic: false`.
- `KeyNotFound` — no entry for this key.
- `Expired` — entry past TTL (currently surfaced only via PruneAsync).
- `UpstreamNotCacheable` — invoke returned `IsComplete=false` or `IsCacheable=false`.

### Storage schema

```sql
CREATE TABLE response_cache (
    key_hash               TEXT PRIMARY KEY,    -- SHA-256 of the canonical key
    provider_key           TEXT NOT NULL,       -- diagnostics
    model                  TEXT NOT NULL,       -- diagnostics
    identity_scheme        TEXT NOT NULL,       -- "v1-basename"
    key_schema_version     TEXT NOT NULL,       -- CacheKey.KeySchemaVersion at write time
    basename               TEXT,                -- forward-compat
    relative_path          TEXT,                -- forward-compat
    target_language        TEXT NOT NULL,
    prompt_template_id     TEXT NOT NULL,
    prompt_template_ver    TEXT NOT NULL,
    source_hash            TEXT NOT NULL,
    rekt_hash              TEXT NOT NULL,
    response_text          TEXT NOT NULL,
    created_at_utc         TEXT NOT NULL,       -- ISO 8601 UTC
    last_hit_at_utc        TEXT NOT NULL,       -- LRU
    hit_count              INTEGER NOT NULL DEFAULT 0,
    byte_size              INTEGER NOT NULL
);
CREATE INDEX idx_response_cache_lru          ON response_cache(last_hit_at_utc);
CREATE INDEX idx_response_cache_provider_model ON response_cache(provider_key, model);
PRAGMA user_version = 1;  -- storage schema version
```

**Two independent versions**:
- `PRAGMA user_version` (storage) — mismatch ⇒ DROP and recreate.
- `CacheKey.KeySchemaVersion` (key) — mismatch ⇒ old keys unreachable; no DB action.

### Cache invalidation behaviour summary

| Trigger | Mechanism | Logged as |
|---|---|---|
| Prompt template change | Bump `PromptTemplateVersion` in caller | New keys ⇒ KeyNotFound on old entries |
| System / user prompt text change | Hashes differ | KeyNotFound |
| COBOL source change | SourceHash differs (preprocessed bytes) | KeyNotFound |
| REKT facts change | RektFactsHash differs | KeyNotFound |
| Model / reasoning effort change | Direct key field | KeyNotFound |
| Generation settings change | GenerationSettingsHash | KeyNotFound |
| Identity scheme migration | IdentitySchemeVersion bump | KeyNotFound (cache survives, hits stop) |
| Key field set change | KeySchemaVersion bump | Old entries unreachable (DB unchanged) |
| Storage schema change | `user_version` bump | DROP and recreate; logged as `storage-schema-recreate` |
| Incomplete LLM response | `IsComplete=false` or `IsCacheable=false` | `decision=skip-store missReason=UpstreamNotCacheable` |
| TTL expiry | `PruneAsync(ttl)` | `decision=prune deletedEntries=N` |
| Size cap | `PruneAsync(_, maxBytes)` LRU | Same as TTL |

### Observability — structured log fields

Every cache event emits a single line with stable event name `LlmResponseCache`:

```
runId           — from LlmCorrelationContext
correlationId   — from LlmCorrelationContext
provider        — provider key
model           — model name
decision        — hit | miss | stored | bypass | skip-store | prune | storage-schema-recreate
missReason      — Disabled | NonDeterministic | KeyNotFound | Expired | UpstreamNotCacheable | -
keyHash         — first 12 chars (full hash queryable in DB)
ageSeconds      — for hits, computed from created_at
hitCount        — post-this-hit
byteSize        — bytes stored (on store events)
identityScheme  — v1-basename
basename        — diagnostic; the COBOL file the call relates to
template        — promptTemplateId/promptTemplateVersion
sourceHash      — first 8 chars
rektHash        — first 8 chars
```

Grep recipes:

```sh
# Why didn't this run hit the cache for PROG.cbl?
grep '"LlmResponseCache"' logs/ | grep 'PROG.cbl'

# All misses with explainable reason
grep 'decision=miss' logs/ | grep -o 'missReason=[A-Za-z]*' | sort | uniq -c

# Cache effectiveness per template
grep 'decision=hit' logs/ | grep -o 'template=[^ ]*' | sort | uniq -c
```

### Token reduction impact estimate

Cache infrastructure alone produces zero token reduction — nothing calls it yet.
Once wired into the converter agent (separate small PR), expected impact for a
re-run of the 22-program test corpus where no prompts changed:

- Cold cache: 0% reduction (first run pays full cost).
- Warm cache, no source change: ~100% reduction on cached calls (one DB read replaces an LLM call).
- Warm cache, single source file changed: ~95% reduction (only the changed file's calls invalidate).
- Warm cache, prompt template version bumped: 0% (intentional — prompt change = behaviour change).

At ~46K tokens per single-shot conversion of BDSDA2F, a single cache hit saves the
full call. At today's typical iteration loop (5–10 re-runs per converter prompt tuning
session), this is the largest single source of waste removed by P1.

## Compatibility concerns

1. **No agent wiring yet.** Calling `CachedLlmInvoker` is the next PR per agent.
   Adopting it requires the agent to assemble all 14 key fields — most are already
   available but `PromptTemplateId`/`PromptTemplateVersion`/`GenerationSettingsHash`
   need explicit construction. See "Roadmap → PR1.b" below.
2. **`Microsoft.Data.Sqlite` is already a package reference** — no new dependencies.
3. **`InternalsVisibleTo`** from P0 hardening was not needed here — cache types are all `public`.
4. **`identitySchemeVersion = "v1-basename"`** is hardcoded today. When the
   ProgramKey migration ships, callers will set it to `"v2-relative-path"` and the
   old cache becomes a one-time miss — exactly the planned behaviour.

## Remaining technical debt (delta from P0)

| # | Item | Severity |
|---|---|---|
| 1 | No per-agent integration yet; cache infrastructure is unused | Expected — PR1 scope. |
| 2 | TTL / size-cap defaults not configurable (callers must pass) | Low — `PruneAsync` is explicit. Wire to config after `doctor.sh` redesign. |
| 3 | Single-flight stampede protection not implemented | Low — only matters if many agents miss the same key concurrently. Add in a follow-up if observed. |
| 4 | LRU prune SQL uses a window-function CTE — fine on SQLite ≥ 3.25 (we ship modern); pin minimum version in docs | Low. |
| 5 | No cache warming command | Deferred. |
| 6 | Storage schema mismatch logs a warning but does not snapshot the existing DB | Acceptable — it's derived data. |
| 7 | Cache does not track *why* a particular call sequence missed (e.g. "source changed") — it only knows the key didn't match | Acceptable — the caller has the context to reason about it via logs. |

### Architectural risks discovered in PR1

1. **AsyncLocal trap (P0) confirmed avoided.** `CachedLlmInvoker` takes explicit `IResponseCache?`, `bool enabled`, `bool isDeterministic`. No hidden ambient state.
2. **Cache key completeness vs. ergonomics.** With 14 fields the call site is verbose. `CacheKey.Build()` enforces required fields; tests verify every field affects the hash. Discomfort is intentional — silent omission was the critique that drove the design.
3. **`IsComplete`/`IsCacheable` contract** is set by the caller's invoke wrapper. If the wrapper lies, the cache stores junk. Mitigation: provide a single helper to construct results from each provider client (next PR).
4. **Storage schema vs. key schema confusion.** Two independent versions; documented in the schema table above and in `SqliteResponseCache` doc comments. Worth pointing out in code review.
5. **TTL semantics.** Currently only enforced by `PruneAsync` calls; `TryGetAsync` does not check TTL. Implication: a hit can return an entry past its TTL until the next prune. Acceptable for derived data; would be wrong for security-sensitive caching. Documented.

---

## Roadmap — PR1.b through PR5

### PR1.b — wire cache into one agent (next small PR)
Pick the Java converter as the demonstrator (highest token cost, deterministic).
- Add `CacheKey` assembly in `JavaConverterAgent.RunAsync(...)`.
- Compute `SourceHash` from preprocessed COBOL bytes (the same bytes fed into the prompt).
- Compute `RektFactsHash` from the REKT context blob the agent already builds.
- `GenerationSettingsHash` = `CanonicalHasher.HashFields(maxOutputTokens.ToString(), stopSequences, ...)`.
- `PromptTemplateId = "java-converter"`, `PromptTemplateVersion = "1"`.
- Wrap the `ResponsesApiClient.GetResponseAsync` call with `CachedLlmInvoker.GetOrInvokeAsync`.
- Determine `IsComplete`/`IsCacheable` from the parsed Responses API status (already extracted at line ~590 in `ResponsesApiClient`).
- Env var `_LLM_CACHE_ENABLED=true` (default false) controls the `enabled` flag.

**Estimated diff:** ~120 lines, all in `JavaConverterAgent.cs`. No changes to client.

### PR2 — incremental REKT scan cache
Goals (from the spec):
- Hash preprocessed bytes per file.
- Skip parse if hash matches `Data/rekt-scan.db`.
- Re-scan dependents when copybooks change (use the REKT copybook graph the parser already emits).
- Preserve recursive discovery from P0.

Sketch:
- `SqliteRektScanCache` mirrors `SqliteResponseCache` patterns: own DB, two schema versions, structured logs.
- Schema:
  ```sql
  CREATE TABLE scan_entry (
      relative_path     TEXT PRIMARY KEY,    -- forward-compat
      basename          TEXT NOT NULL,        -- identity today
      identity_scheme   TEXT NOT NULL,
      preprocessed_hash TEXT NOT NULL,
      parsed_at_utc     TEXT NOT NULL,
      parse_outcome     TEXT NOT NULL,        -- "full" | "deps-only" | "failed"
      copybook_uses     TEXT                  -- JSON list of copybook basenames
  );
  ```
- `tools/preprocess-for-rekt.sh` writes a `<basename>.hash` file alongside the preprocessed file; doctor.sh reads it to decide whether to invoke smojol.
- Dependency-aware invalidation: when a copybook's hash changes, mark all programs whose `copybook_uses` contains it as stale.
- **Risk:** smojol output naming uses stems, not paths, so the cache key has the same basename-uniqueness assumption as P0. Document and warn on duplicate basenames (already done in `resolve-programs.py`).

### PR3 — curated `program-facts.json`
The biggest token-reduction lever.
- New typed extraction step that consumes raw REKT JSON and emits one `program-facts.json` per program.
- Schema (mandatory fields per the spec):
  ```json
  {
    "schemaVersion": 1,
    "identitySchemeVersion": "v1-basename",
    "basename": "PROG.cbl",
    "stem": "PROG",
    "relativePath": "FUENTES/src/PROG.cbl",
    "sourceHash": "sha256...",
    "summary": { ... },
    "io": { ... },
    "data": { "groups": [...], "copybooksUsed": [...] },
    "callers": [...],
    "callees": [...],
    "controlFlow": { ... },
    "externalEffects": ["FILE_IO", "DB_UPDATE"],
    "confidence": { "ast": "full", "cfg": "full", "data": "full" },
    "warnings": ["copybook BDCSEQOI not found on path"],
    "preprocessNotes": [{ "transform": "MOVE 0(1) → MOVE ZERO", "line": 482 }]
  }
  ```
- Implementation: a new C# extractor class (`Helpers/ProgramFactsExtractor.cs`) reading `output/rekt/*.json` and writing to `output/rekt/<stem>.facts.json`.
- Used to feed `RektFactsHash` for the response cache — wiring is automatic.

### PR4 — prompt projection layer
- New `Helpers/PromptProjections/` directory; one small class per agent:
  - `JavaConverterProjection`, `CSharpConverterProjection`, `ReverseEngineeringProjection`, `ArchitectureProjection`, `BianTaggerProjection`.
- Each picks the subset of `program-facts.json` it needs, formats into a stable JSON the prompt embeds.
- Existing `RektPromptInjector.cs` becomes a thin orchestrator that picks the projection by agent type.
- Raw-AST fallback behind a `_REKT_LEGACY_INJECTION=true` env var.

### PR5 — preprocessor hardening
- Catalog every "deps only" failure currently observed; add per-pattern transforms:
  - `Unsupported figurative constant: ALL '%'` → rewrite to `ALL "%"`.
  - `MOVE 0(1) TO …` → `MOVE ZERO TO …`.
  - EXEC SQL / EXEC CICS edge cases (parameterise).
- Every transform written to `<basename>.preprocess.json` alongside the preprocessed file:
  ```json
  {
    "schemaVersion": 1,
    "identitySchemeVersion": "v1-basename",
    "basename": "PROG.cbl",
    "transforms": [
      { "rule": "figurative-all-percent", "line": 1234, "before": "...", "after": "..." },
      { "rule": "move-0-1", "line": 482, "before": "...", "after": "..." }
    ]
  }
  ```
- Surfaced to PR3's `program-facts.json` under `preprocessNotes` so the LLM sees them as warnings.
