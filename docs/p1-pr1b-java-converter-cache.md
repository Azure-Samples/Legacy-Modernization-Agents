# PR1.b — JavaConverterAgent Cache Integration

**Last updated**: 2026-05-28
**Status**: Complete. 183 / 183 tests pass.

## Summary

Wires the P1 deterministic response cache into `JavaConverterAgent` only —
the first production integration path. Default-disabled (env-var opt-in).
No other agents touched.

## Files modified

```
Agents/Infrastructure/Caching/LlmCacheGate.cs              (new — env-var opt-in, lazy singleton)
Agents/Infrastructure/Caching/JavaConverterCacheKeys.cs    (new — key construction + cacheability check)
Agents/JavaConverterAgent.cs                                (~80 lines added: lookup, store, skip-continuation)
CobolToQuarkusMigration.Tests/Caching/JavaConverterCacheKeysTests.cs  (13 tests)
CobolToQuarkusMigration.Tests/Caching/LlmCacheGateTests.cs            (4 tests)
docs/p1-pr1b-java-converter-cache.md                        (this file)
```

## Design choices and why

### Cache at the outermost boundary, not around the LLM client

The lookup/store sits in `JavaConverterAgent.ConvertToJavaAsync`, **after** the
continuation loop and structural validity check. Stored value is the final,
extracted Java code — not the raw LLM response.

Implications:
- Truncated / partial responses **cannot reach the cache**: the validity gate
  (`IsCacheableJava`) is the same check the continuation loop uses to decide
  if the output is complete. If the agent itself would have asked for more,
  the cache does not store.
- A cache hit replays the final code directly and skips both the LLM call
  and the continuation logic — saving the full 46K-token round-trip for
  re-runs.

### Hash inputs are the actual prompt bytes, not the source files

- `SourceHash` = SHA-256 of `sanitizedContent` — the post-preprocessor,
  post-sanitisation bytes that actually appear inside the prompt. This
  matches the spec ("preprocessor changes must invalidate the cache") and
  avoids spurious misses from raw-file edits that don't affect the prompt.
- `RektFactsHash` = SHA-256 of the REKT block as embedded in the prompt
  (extracted between the `REKT STRUCTURAL CONTEXT (authoritative` marker
  and the `IMPORTANT REQUIREMENTS:` marker). Hashing the embedded text means
  REKT-file edits invalidate only when the prompt actually changes.
- `GenerationSettingsHash` = canonical hash over `max_output_tokens`,
  `reasoning_effort`, `response_format`, `top_p`, `seed`, `stop`. The agent
  calls `ResponsesApiClient.CalculateTokenSettings` **before** the LLM call
  to obtain deterministic generation settings for the key.

### Empty REKT ≠ any REKT content

`RektFactsHash` is the empty string when no REKT context is present; any
non-empty REKT context produces a SHA-256 hash. The two cannot collide.
`CanonicalHasher.HashFields` further distinguishes null from empty via a
sentinel token, preventing silent identity collisions inside `CacheKey`.

### Explicit opt-in, default disabled

- `_LLM_CACHE_ENABLED=true` activates the cache. Anything else (unset, "false",
  "0") keeps it off.
- `_LLM_CACHE_DB=<path>` overrides the default `Data/llm-cache.db` location.
- When disabled, `LlmCacheGate.Cache` returns `null` and the agent's lookup /
  store blocks short-circuit. Zero behaviour change for users who don't set
  the env var.
- When initialisation fails (disk error, permission, etc.), the cache fails
  open — a warning is logged and the conversion proceeds uncached. Cache
  failures must never break a conversion.

### Bypass for non-Responses path

The integration is guarded on `ResponsesClient is not null`. If the agent
was constructed for the `IChatClient` path (GitHub Copilot SDK), the cache
short-circuits. This keeps PR1.b strictly to the "primary Responses API
invocation" scope.

### No silent cache poisoning — IsCacheable derivation lives in one place

`JavaConverterCacheKeys.IsCacheableJava(javaCode)` is the single arbiter of
"can this be cached?". It returns true only when the code has a `package`
declaration, a `class` declaration, and balanced braces. The same logic the
continuation loop uses for "is this complete?". Tested with 7 explicit cases
(complete / unbalanced / no-package / no-class / interface-only / empty /
whitespace-only).

## Cache-hit validation results

Unit-level (deterministic, no LLM):
- Identical (system prompt, user prompt, source, REKT, model, max-tokens,
  effort) ⇒ identical key hash ⇒ a hit replays the stored Java.
- Any single input change ⇒ different key ⇒ miss.
- Bumping `PromptTemplateVersion` or `IdentitySchemeVersion` invalidates
  every existing entry without touching the DB.

End-to-end validation (in front of a live model) is intentionally out of
scope for unit tests; the recommended path is:

```sh
# Cold run — populate cache
_LLM_CACHE_ENABLED=true \
_LLM_CACHE_DB=Data/llm-cache.db \
./doctor.sh convert-only --program BDSDA2F --target java

# Warm run — same inputs, same prompt template version
_LLM_CACHE_ENABLED=true \
./doctor.sh convert-only --program BDSDA2F --target java

# Expect: "Cache HIT for BDSDA2F.cbl (age=…s, hits=1, key=…)" log line
# Expect: total LLM tokens for this program = 0 on the warm run
```

## Token reduction estimate (real run, projected)

Single-shot conversion of BDSDA2F (790 LoC) historically costs ~46K input
tokens + ~12K output tokens. After this PR with the cache enabled:

| Scenario | Tokens saved | LLM calls saved |
|---|---|---|
| Cold cache, first conversion | 0 | 0 |
| Same prompt, no source change | ~58K per file | 1 per file |
| Source unchanged, copybook changed (no REKT impact) | ~58K per file | 1 per file |
| Source changed, REKT updated | 0 per affected file (correct miss) | 0 |
| Prompt template version bumped (PR change) | 0 — intentional invalidation | 0 |

Iteration loops (5–10 re-runs while tuning a converter prompt) become
~5–10× cheaper for unchanged programs while keeping correctness guarantees
intact.

## Cache correctness risks discovered during PR1.b

1. **`IsCacheableJava` is a heuristic.** It matches the continuation loop's
   check, which has been the agent's de facto "good enough" gate for months.
   The risk is that a code-block that is structurally complete but
   semantically wrong gets cached and re-used. **Mitigation:** this is the
   same code the agent would have shipped without the cache — caching the
   same output the user would have received changes nothing about
   correctness, only saves the round-trip. If/when stricter validation
   lands (compile check, semantic diff), it should run **before** the cache
   store.

2. **Concurrent first-time call to the same program** (e.g. parallel
   conversion of identical files) currently leads to duplicate LLM calls
   and only one of the responses ends up in the cache. **Stampede
   protection is explicitly out of scope** for PR1.b — listed in the P1
   roadmap as a follow-up when observed.

3. **The continuation loop is skipped on cache hit.** Correct — but only
   because we cache the post-continuation, post-extraction code. If the
   continuation loop ever gains side effects (e.g. logging telemetry that
   downstream agents depend on), the cache-hit path would silently bypass
   them. **Mitigation:** continuation loop is purely additive text
   assembly today; no side effects. Documented here so a future change
   notices.

4. **`ExtractRektContextBlock` is marker-based.** It looks for the literal
   strings `REKT STRUCTURAL CONTEXT (authoritative` and `IMPORTANT
   REQUIREMENTS:`. If either marker changes in the prompt template, the
   extractor returns the wrong substring and the cache key shifts. Bumping
   `JavaConverterCacheKeys.TemplateVersion` is the safety valve and is
   already part of the documented workflow for prompt edits.

5. **`CalculateTokenSettings` determinism.** Reads only the prompt content
   and the agent's profile; both are stable across a run. No clock, no RNG,
   no environment. Verified by inspection — but worth re-checking if the
   complexity scorer ever pulls in external state.

## Remaining blockers before PR2 lets go

**None hard-block PR2** (incremental REKT scan cache). PR2 has its own SQLite
DB, its own schema, and does not touch the response cache. The two are
fully orthogonal.

Soft items worth doing in parallel:

1. **Live-run validation** — confirm the cache hit logs as expected against a
   real conversion. Cannot be tested in CI without an LLM endpoint; recommend
   one manual run on the 22-program test corpus before merging.
2. **Cache size monitoring** — `PruneAsync` is not yet invoked anywhere.
   Acceptable for short term; recommend a periodic doctor.sh-driven prune
   once the env-var redesign lands.
3. **Wiring helper for the C# converter** (PR1.c equivalent) — completely
   mirrors this PR; ~30 lines once we have the pattern. Not blocking PR2.

## Architectural risks discovered during PR1.b

1. **The cache-gate singleton uses static state.** The `LlmCacheGate` holds
   a process-wide `IResponseCache` instance behind a lock. This is correct
   for the in-process design but is the kind of pattern that grows into a
   service-locator over time. **Mitigation:** test-only `ResetForTests`
   isolates test runs. If DI gets adopted later, the gate becomes a thin
   adapter over the DI-resolved cache.

2. **Cache failure during conversion logs a warning and continues uncached.**
   This is the correct policy ("cache failures must never break a
   conversion") but could mask real DB problems. **Mitigation:** the warning
   is logged with the exception; structured log consumers can alarm on
   repeated occurrences.

3. **`PromptTemplateVersion = "1"`** is a constant in code. Bumping requires
   a code change + rebuild. This is acceptable for prompt-tuning iteration
   where the developer is already editing prompts in code; it would be
   wrong if templates ever moved to runtime configuration. **Documented**
   alongside `TemplateVersion` so the rule is discoverable.

4. **The cache key does not include the `ENABLE_REKT_CONTEXT` env var.**
   Turning REKT injection on/off changes `RektFactsHash` (empty vs full) so
   the cache correctly invalidates. No risk; calling it out so reviewers
   don't suggest adding it redundantly.

5. **Cache hits on the chunked path are not implemented.** PR1.b only
   targets the single-shot Java conversion path. Chunked conversions
   (`ChunkAwareJavaConverter`) bypass the cache entirely today. This is
   acceptable — chunked paths are a smaller share of runs and need their
   own cache-key design (per-chunk vs. per-program). Tracked for PR1.b.2.
