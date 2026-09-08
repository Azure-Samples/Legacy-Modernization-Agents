# Copilot instructions

## Testing objective

Apply this guidance when changing production code, tests, fixtures, or testing automation. Tests must protect observable behavior and data integrity, not merely increase coverage or confirm the current implementation.

- Establish expected behavior from requirements, supported contracts, and callers before choosing assertions. Current output alone is not an oracle.
- Prioritize reachable failures, output loss, false success, and broken integration boundaries over uncovered constructors, DTOs, or unused code.
- Prefer strengthening an existing meaningful test over adding overlapping cases. Keep useful negative tests, output-format tests, and lifecycle tests.
- Do not add tests that search source files for removed classes, obsolete strings, implementation snippets, or comments. Test the supported behavior instead. Assertions about generated output or intentionally absent results remain valid.
- Do not add tests for dead code to improve coverage. Confirm reachability and propose deletion separately.

## Regression workflow

1. Identify the behavior, entrypoint, dependencies, and concrete failure the test must detect.
2. Exercise the smallest real boundary that can expose the defect. A helper test cannot establish command exit status, persistence, orchestration, or UI state.
3. Choose inputs that actually execute the relevant branches. For cross-chunk context, create multiple chunks; for resume, include a failed middle chunk and earlier persisted output.
4. Assert exact outcomes and important side effects. Explain why the test would fail for the intended regression; demonstrate failure before the fix when practical.
5. If a failure exposes a production bug, fix the bug when in scope. For test-only work, report the blocker rather than weakening expectations, swallowing exceptions, skipping the test, or enshrining incorrect behavior.
6. Run the affected cases, inspect the results, and expand the scope when dependencies or results justify it. Coverage may locate gaps; it does not prove correctness.

Change an expected value only when the intended contract changed or the old expectation was demonstrably wrong. State the independent evidence; "this is what the implementation returns" is insufficient.

## Assertions that carry meaning

- Test names must describe what is actually exercised. A returned list is not proof of persistence; an enum mapping is not an incremental-planner test.
- Avoid tautologies such as `Count >= 0`, conditional assertions whose branch may never run, and success assertions derived from another unverified success flag.
- Assert content as well as count when content matters. With FluentAssertions, `ContainSingle("reason")` supplies an explanation, not an expected element. Use `ContainSingle().Which.Should().Be(expected)` for an exact single value.
- A successful HTTP status, non-null response, mock invocation, or nonempty output is not enough to prove feature success. Assert the payload, authoritative state, persisted result, or accepted output required by the contract.
- "Fixed" must mean the output changed and passes revalidation. "Resolved" must correspond to actual references. Do not preserve fabricated counters or placeholder repair success.
- Assert failure, partial completion, cancellation, and unknown state distinctly. Missing files, failed provider calls, and diagnostic fallback classes must not count as completed conversion.
- Use recording fakes for external boundaries; do not mock the method under test or compute expected results by calling the same implementation.

## Safe, deterministic fixtures

- Unit and ordinary integration tests must not use developer credentials, live AI providers, a shared Neo4j instance, or developer databases, prompts, logs, and output directories.
- Before starting a web test host, replace storage paths, AI/client creation, subprocess launching, and startup maintenance with isolated or controlled dependencies. Replacing only `IMcpClient` is insufficient. Do not run the existing `McpChatWeb.Tests` host against a configured checkout until these boundaries are isolated.
- Use unique temporary directories/databases owned by each test and clean up only those resources. Do not use fixed repository-relative fixture directories or broad process-name cleanup.
- Exercise persistence against temporary stores initialized with production schema code where possible. Do not maintain independent SQL copies that can drift or introduce obsolete tables.
- Restore environment variables, static state, and Python module substitutions after each case. Scope fakes to fixtures; do not install process-global fake modules at import time.
- Prefer injected clocks, explicit completion signals, and controlled response ordering over sleeps or real rate-limit waits. Bound asynchronous work and dispose processes, watchers, and clients.
- Keep tests parallel-safe. If unavoidable shared state requires serialization, scope it narrowly and explain why.
- Keep live-provider/database/container tests explicitly opt-in, separately configured, and excluded from normal isolated runs. Do not hide ordinary regressions behind opt-in markers.

## Repository-specific regression priorities

| Boundary | Valuable invariant |
|---|---|
| CLI and shell orchestration | Invoke the actual command; check exit code, stdout/stderr, explicit path precedence, and failure propagation. Offline commands must not require AI configuration. |
| Chunk planning and context | Run the real adapter-to-chunker path; primary source coverage is complete exactly once, intentional overlap is separate, limits are honored, and later chunks receive prior accepted context. |
| Conversion, assembly, and resume | Distinguish complete programs from fragments; preserve declarations and source coverage; retry holes and include persisted outputs in resumed assembly. Partial output is not full success. |
| Validation and reconciliation | Exercise real metadata producers and consumers, strict-mode acceptance, actual corrections, and revalidation rather than invented fix/reference counts. |
| Facts, caches, and persistence | Changed sources/dependencies/templates invalidate affected entries; missing artifacts do not become valid cache hits; partial refresh preserves global context; run and source identities remain distinct. |
| Provider adapters and prompts | Fake transports observe the selected model/options and full prompt; incomplete/refused output and cancellation stay visible; failed enhancement preserves existing prompt text. |
| Portal and asynchronous UI | Use delayed/out-of-order responses to prove the latest selected run, provider, and editor own the result; failed operations do not appear applied. |
| Parsers, reports, and logs | Use producer-to-consumer round trips and bounded sections; generated reports can be found/read, empty or malformed logs are not success, and data belongs to the requested run. |

## Tools and execution

- Reuse the existing toolchain: .NET 10; xUnit, FluentAssertions, and Moq in `CobolToQuarkusMigration.Tests`; xUnit and ASP.NET test hosting in `McpChatWeb.Tests`; existing Python tests under `tools/`.
- Match each project's installed assertion/mocking libraries. Do not add a library or a frontend test framework merely for coverage or stylistic consistency.
- Inspect the selected tests for side effects before running them. Start with the smallest relevant project/filter, combine related selectors, and confirm that the filter matched tests.
- With dependencies already restored, a local isolated example is:

  ```bash
  dotnet test CobolToQuarkusMigration.Tests/CobolToQuarkusMigration.Tests.csproj \
    --no-restore -p:CopilotSkipCliDownload=true \
    --filter 'FullyQualifiedName~ChunkingOrchestratorTests|FullyQualifiedName~SemanticUnitChunkerTests'
  ```

- Replace the example filter with the affected classes. Do not use `--no-build` after changing code. `CopilotSkipCliDownload` prevents the SDK's optional CLI download; it does not replace a missing test dependency or allow real SDK integration tests without their prerequisites.
- Restore/install only when dependencies changed or the selected command reports missing dependencies. Respect the environment's required proxy for every download; do not bypass proxy or TLS controls, guess credentials, or log secrets. If required access is unavailable, report the blocker.
- Use existing coverage collection only when useful to locate gaps or assess the exercised branches. Do not install mutation/coverage tooling just to satisfy this policy.
- In test-change handoffs, identify the protected contract, the commands actually run, and any unexecuted or blocked scope. Never claim broader coverage or execution than the evidence supports.
