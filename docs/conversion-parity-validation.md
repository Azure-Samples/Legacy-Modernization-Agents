**Last updated**: 2026-10-22

# Conversion Parity Validation

Code generation reports success whenever the model returns *something*. Nothing checked that the generated Java or C# actually represented the COBOL it came from, so a conversion could drop paragraphs, data fields, `CALL` targets or SQL access entirely and still be recorded as a win.

The scorer compares four axes of structural evidence against identifiers in generated files. Scoring is deterministic. The migration pipeline can additionally request bounded, source-backed model repairs, and always scores the resulting files again.

It is a **preview feature**, like the REKT scan it reads from.

---

## What it measures

| Axis | Weight | Expected symbols from |
|---|---|---|
| `procedures` | 0.40 | `StructuralContext` sections, paragraphs and PERFORM targets |
| `dataFields` | 0.25 | `StructuralContext` data structure, flattened through group items |
| `callTargets` | 0.20 | `ProgramFacts.Callees`, falling back to REKT `CallTargets` |
| `sqlTables` | 0.15 | `ProgramFacts.Io.DbTables`, falling back to REKT SQL statements |

Weights are renormalised over the axes that have at least one expected symbol. A program with no SQL is scored out of the remaining 0.85, not penalised for the absence.

`ProgramFacts` carries no field or section *names* — `Summary.Sections` is a count, and `ControlFlow.EntryPoints` holds only the first section — so those two axes must come from `StructuralContext`. Facts are preferred for callees and SQL tables, because those carry the operation-name fix from the dependency-health work.

Copybooks are measured against their directly declared members, not procedures, calls or SQL borrowed from a same-stem program. Their `SourceDeclared` provenance and evidence notes state that nested `COPY` expansion and storage-layout equivalence are not established.

## How a symbol is matched

Substring matching is not usable here: `WS-A` reduces to `wsa`, which appears inside unrelated identifiers, and a callee named `IO` matches almost anything. Matching is therefore token-based.

```mermaid
flowchart TD
    A[Generated file] --> B[Split into code text<br/>and comment/literal text]
    B --> C[Extract identifiers from each]
    C --> D[Split on case, digit<br/>and separator boundaries]
    E[Expected COBOL name] --> F[Split the same way,<br/>drop numeric-only tokens]
    D --> G{Token sequence<br/>contained?}
    F --> G
    G -->|in code| H[Covered]
    G -->|comments only| I[PossiblyRenamedOrMerged]
    G -->|neither| J[Missing]
```

`SEARCH-CUSTOMER` becomes `[search, customer]` and matches `searchCustomer`. `1000-INIT` drops its numeric segment and matches `init`. A leading scope prefix — `ws`, `ls`, `lk`, `fd`, `sd` — is stripped before length checks, so `WS-CUSTOMER-ID` is compared as `customerid`.

Ambiguity in the code/comment split always resolves toward the comment bucket. Under-counting code produces a visible gap; leaking comment text into the code bucket produces a silent false pass.

String literals are held in a third bucket rather than folded into either. For most axes they are weak evidence and count as comments, but on the `callTargets` and `sqlTables` axes the literal *is* the executable form — a table name reaches the database as a string inside a JDBC query or a `@Table` annotation, and a dynamically invoked program name reaches the dispatcher the same way. Treating those as comment text failed a correct JDBC conversion outright, which is the false positive most likely to get the check switched off.

Each expected symbol consumes at most one candidate in the generated file, matched exact-token-sequence first and compact-containment second. Without consumption a single surviving `wsLine01` satisfied all twelve of `WS-LINE-01`…`WS-LINE-12`, because purely numeric segments are dropped and the family collapses to one comparable form. The two passes stop a short name taking the candidate a longer one needs.

### Gap classification

| Symbol found in | Kind | Score credit |
|---|---|---|
| code | — not a gap | full |
| comments or string literals only | `PossiblyRenamedOrMerged` | **half** |
| neither | `Missing` | none |

A paragraph folded into another method, or renamed during conversion, characteristically survives as a comment naming the original. Scoring that as a total loss is a false positive, and false positives are what get a validator switched off. Scoring it as a full pass is the opposite error: a comment is evidence that the original name was *considered*, not that its behaviour was carried over. Half credit says exactly that, and the gap is still reported either way.

### Total axis loss

A score alone cannot express that an entire dimension was dropped. With the weights above, a conversion that loses every `CALL` target still scores 0.80 and passes a 0.75 threshold.

An axis with at least one expected symbol and **no code match at all** is therefore flagged as a total loss, and fails the threshold check regardless of the overall score. The axis names appear as `lostAxes` in the JSON and in the report table.

## What is deliberately not scored

- `FILLER`, level-88 condition names, level-66 renames and generated `TypedRecord*` types are excluded from `dataFields`. They are not storage a conversion is expected to reproduce.
- Data nodes attributed to the `PROCEDURE DIVISION` are excluded. The parser surfaces COBOL special registers such as `WHEN-COMPILED` as data items with `sourceSection: PROCEDURE_DIVISION`; they are injected by the compiler, not declared by the program, so no conversion can carry them. Real storage always carries `WORKING_STORAGE`, `LINKAGE` or `FILE`.
- Fields declared only by an auto-generated stub copybook are excluded — see *Evidence quality* below.
- A `PERFORM ... UNTIL` operand that is also a data field is dropped from `procedures`; it names a condition, not a paragraph.
- `CALL WS-PROGRAM-NAME` is not recorded as a callee. The variable holds a program name at runtime; treating the variable as a dependency would invent one.

## Evidence quality

### Required dependency evidence before conversion

Both direct and chunked migration now fail closed **before dependency commentary,
source analysis or conversion model calls** when required REKT dependency evidence
is missing, incomplete, stale or placeholder-backed. A reused reverse-engineering
map is not an exemption: current evidence is revalidated and its relationships must
match the current parser export. Rejection persists a failed run with explicit
diagnostics; no generated-source success fallback is written.

The source-relative sidecar is
`output/rekt/<relativePath>.report/dependency-evidence.json`. Its contract is:

| Field | Required evidence |
|---|---|
| `schemaVersion`, `producer` | Sidecar schema `1`, producer `cobol-rekt/dependency`; not an inferred parser version |
| `relativePath`, `originalSourcePath`, `originalSourceSha256` | Exact selected-source-relative identity, absolute original path, SHA256 of original file bytes |
| `exportPath`, `exportSha256`, `exportExitCode` | Absolute active REKT dependency export, its raw-byte SHA256 and successful export exit |
| `coverage` | Explicit parser-backed `complete` for each of `COPY`, `CALL`, `EXEC_SQL`, `EXEC_CICS`, `FILE_IO`; missing, `partial` and `unavailable` block |
| `definitions` | Actual resolved dependency names/kinds, absolute definition paths and raw-byte SHA256 values; grounded generated COPY inputs may additionally use `BMS` / `COPY_INPUT` |
| `unresolvedDiagnostics`, `placeholderContaminated` | No unresolved required inputs, and explicit absence of placeholder contamination |

`ProgramFacts.dependencyEvidence` retains this provenance, but the conversion gate
reads the current sidecar and rehashes the actual source, export and definitions.
If a facts projection exists, its evidence snapshot and dependency entries must
match that current export; stale legacy facts must be regenerated, not injected
into conversion prompts alongside a newly verified map.
`ProgramFacts.SourceHash` still covers staged/preprocessed bytes and is **not**
substituted for the original-source hash. Global `High` parse confidence,
nonempty AST, command exit zero, scanner absence of a COPY, and an empty
`copybooksUsed` array cannot establish complete zero-COPY coverage.

Relationships come from the hash-bound parser dependency export, not AI or a
local scanner. Source scanning can only reject contradictions; it never supplies
missing edges or makes coverage complete. Dynamic CALL operands and genuinely
missing external definitions remain unresolved. The contradiction guard ignores
dependency words inside literals and comments, while retaining actual quoted
CALL/COPY operands, including fixed-format literal continuations. The legacy dependency-tree format
cannot represent SQL/CICS/file-I/O relationships; positive source evidence for
these kinds is explicitly blocked rather than silently omitted.

Only the export root's direct dependencies become that source's relationships,
COPY usage and callees. A called program's subtree is supporting evidence, not
additional root edges: `A CALL B CALL C` does not imply `A CALL C`, and B's COPYs
are not assigned to A. Hashes, diagnostics and unresolved inputs throughout that
supporting subtree are still checked. The sidecar's `definitions` therefore
retains both direct and supporting inputs without changing their source origin.

Discovery also selects `.cpy` files, but the native producer parses programs,
not copybooks as standalone programs. A selected copybook therefore needs no
fabricated manifest: it is accepted only as an exact-path, current hash-bound COPY
definition in a fully validated selected program's direct or supporting export.
Unreferenced, stale, placeholder-backed or ambiguous definitions remain blocked.
This does not assert standalone per-kind coverage, add a COPY self-edge, or create
copybook program facts. Existing standalone copybook facts without a verified
definition-context projection contract are rejected rather than injected into
converter prompts.

**Legacy or unsupported producer evidence remains a blocker.** Legacy REKT exports
have no per-kind completeness declaration and cannot establish a verified empty
inventory. Rebuilding the first parser with patch `0012` enables typed, hash-bound
COPY/static CALL exports and supported-absence attestations from the actual
parser tree. Clean supported programs can then satisfy the strict gate after
their evidence is regenerated. Dynamic or ambiguous callees, missing definitions,
and present unsupported SQL/CICS/file-I/O constructs still block; adding a partial
sidecar diagnoses those gaps rather than completing their coverage. Do not
hand-edit coverage, accept generated vendor subsets as authentic libraries, or
invent constants/layouts to make this gate pass. A hash identifies bytes; it
cannot establish a vendor layout's authenticity without grounded input provenance.

These checks read **local REKT disk exports**. They do not query Neo4j, run a new
parser, switch graph endpoints, or establish behavioral equivalence. Archived
`.previous` and in-progress `.work` artifacts are not active facts/context.

Missing copybooks are never synthesized, including when the legacy
`REKT_NO_STUB_COPYBOOKS=false` setting is supplied. Supply genuine definitions;
the scan names unresolved dependencies in `output/rekt/missing-copybooks.txt`
and reports reduced coverage. The preprocessor removes only marked legacy
placeholders from its generated `.preprocessed/` directory. Staging rejects
marked placeholders in supplied source without modifying that source.
Deterministic symbolic maps derived from genuine BMS source remain supported.

The parser remains strict by default. `REKT_ALLOW_MISSING_COPYBOOKS=true` allows
recovery only when every ERROR diagnostic is a missing copybook and a parse tree
exists. `doctor.sh` opts in for analysis and requires nonempty AST and CFG JSON
before recording structural success. All other ERROR diagnostics remain fatal;
entries without a supported data layout fail explicitly rather than guessing
`PIC X(1)`. Recovery may still fail when analysis requires an unavailable definition.

Diagnostics are persisted as `parse-diagnostics.json` beside parser artifacts,
including failed attempts. Recovered structural results are recorded as `Partial`,
and missing-copybook warnings cap facts confidence at partial. Portal fidelity
also honors persisted diagnostics rather than trusting a stale full-confidence
cache entry. Diagnostics are not graph artifacts and are excluded from ingestion.

Older reports may still contain invented placeholder fields. Expecting them in
generated code produces gaps no conversion can close, so they are excluded.
Detection requires the marker string rather than guessing from the `-STUB` suffix,
which could drop real fields. Conversion and repair must preserve the unresolved
dependency, not invent members or suppress errors to make output compile.

Because their exclusion means field parity was measured against *incomplete* evidence, each affected program carries an `evidenceNotes` entry naming the stub copybooks. The score is not silently adjusted to compensate; the weakened evidence is stated instead.

## When parity is not measured

An inferred value is never presented as a measured one.

| Situation | Outcome |
|---|---|
| No structural context, or provenance `None` | `NotEvaluated`, no score, reason names `./doctor.sh rekt-full` |
| All four axes have no comparable symbols | `NotEvaluated`, axes still reported |
| Generated file cannot be mapped back to a source program | `NotEvaluated`, never guessed |
| Legacy outputs identify multiple sources only by a shared basename | `NotEvaluated`, reason names the count |
| Multiple source identities map to one overwritten output path | `NotEvaluated`; repair is refused |
| Source program produced **no** generated file | **Evaluated, score 0**, explicit `file` gap |
| File is a `ConversionOutputGuard` stub or a converter fallback | **Evaluated, score 0**, explicit `file` gap |

The two score-0 cases outrank the missing-context rule. A missing output and a stub marker are both direct evidence that conversion failed, so they are measured zeros even when nothing is known about the source. Parity therefore starts from the inventory of source programs, not from the list of files that happen to exist — a program that silently produced nothing is the most severe failure the check can find, and reading only the output directory would miss it entirely.

That case also drives the comment rule: the guard embeds the rejected model output inside a block comment. If comment matches earned full credit, a stub whose rejected output happened to be Java-shaped would score 1.0 — precisely the failure this feature exists to catch. Comment credit is therefore withheld entirely from any file containing no converted code.

The score is forced to zero rather than computed for these files. A converter fallback carries its own `run()` method, which otherwise scored full marks against a `RUN` paragraph; incidental token overlap with a file that converted nothing is not coverage.

## Where it runs

The post-pass runs after conversion has finished, in both `MigrationProcess` and `ChunkedMigrationProcess`. Both are needed: `SmartMigrationOrchestrator` routes the whole estate to the chunked path if any single file is large, so wiring one would leave parity dormant on any realistic estate.

Running post-write rather than per-file is what makes the guard stub visible at all — a pre-write validator cannot see output that does not exist yet.

Each run writes `conversion-parity.json` beside the generated code and appends a section to the migration report.

`CodeFile.OriginalSourcePath` is authoritative source identity; `CodeFile.FilePath` is the path returned by the writer. `generated-source-map.json` records source-relative identity, generated-relative path and source kind. A program and copybook sharing a stem, or programs sharing a basename in different directories, remain separate.

### Bounded repair and final measurement

For a failing source with available authoritative COBOL, the repair agent receives that source, its generated files and the measured gaps. It must return exactly those file keys, preserving identities and existing APIs. Related compiler context is read-only. Missing copybook definitions must not be invented to improve a score or satisfy compilation.

Candidates are accepted only after deterministic revalidation shows increased code coverage without losing coverage on another axis. Comment-only name additions do not establish improvement. Rejected candidates leave the original files intact; interrupted writes restore the originals. The report records attempts, accepted repairs and rejection notes.

After compiler repairs, parity is measured again from the files actually on disk. Repair history is retained, but the final score is never copied from the pre-build measurement. A successful compiler cannot turn a missing, diagnostic-only or conflicting conversion into a completed migration.

## Configuration

| Variable | Default | Effect |
|---|---|---|
| `MIN_PROGRAM_SCORE` | `0.75` | Score below which a program is counted as below threshold. Clamped to `[0,1]`; an unparseable value logs a warning and falls back to the default. |
| `ON_LOW_SCORE` | `warn` | `warn` reports only. `stop` additionally sets exit code `4`. |
| `PARITY_REPAIR_MAX_ROUNDS` | `2` | Maximum repair attempts per source; `0` measures only. Invalid or negative repair limits are rejected. |

Under `stop`, the migration pipeline also rejects sources that could not be evaluated. Missing evidence is not a pass. Missing or diagnostic-only conversions fail completion even under `warn`; the policy controls structural-score acceptance, not whether source coverage may be silently omitted.

These are read from the environment, which takes precedence over `Config/ai-config.local.env`.

```bash
MIN_PROGRAM_SCORE=0.8 ON_LOW_SCORE=stop ./doctor.sh run
```

`stop` cannot abort a migration, because parity runs after conversion has completed. It sets a non-zero exit code once both artefacts are written, which is the behaviour CI actually wants; a partially aborted migration is not.

Exit code `4` is distinct from the existing conventions (`2` usage or not-found, `3` parse failure). Enabled compile-gate failures use exit code `5`; final parity does not overwrite that build failure.

> Making that work required a fix in `Program.cs`. An `int`-returning `Main` overrides `Environment.ExitCode` entirely, so the gate — and every existing `Cli/` command that reports failure the same way — exited `0` regardless. `Main` now returns the environment code when its own is zero.

### Calibrating the threshold

`0.75` was calibrated against the sample estate, not guessed. On a real REKT scan, converted by hand from the COBOL alone rather than tuned to pass:

| Conversion | Score |
|---|---|
| Faithful, `CUSTOMER-INQUIRY.cbl` | 1.00 |
| Faithful, `CUSTOMER-DISPLAY.cbl` | 0.95 |
| One paragraph renamed, original named in a comment | 0.97 |
| One paragraph, five fields and the only `CALL` dropped | 0.27, `callTargets` lost |
| Source program that produced no output at all | 0.00 |
| `ConversionOutputGuard` stub | 0.00 |

The 0.95 is the copybook-scope limit described under *Known limits*, not a conversion defect: `CUST-STATUS` is read through a condition name and lives in a record class emitted alongside the other program.

The default sits between a conversion that is complete and one that has lost a fifth of its structure. Estates with many small paragraphs will want a different value, which is why it is configurable.

## Portal

`GET /api/modernization/conversion-parity` returns the persisted report for each target language, distinguishing targets that were never converted from reports that exist but could not be read. A corrupt report is never presented as an absent one. Reports are looked up under `JAVA_OUTPUT_FOLDER` and `CSHARP_OUTPUT_FOLDER`, falling back to `output/java` and `output/csharp`, so a converted estate written elsewhere is not reported as missing.

The Modernization Intelligence **Runtime** subview renders it. The runtime-telemetry half of that subview has no data source yet and remains a labelled placeholder.

## Relationship to the output guard

`ConversionOutputGuard` is a syntactic check at write time: empty files, stub markers, a missing `class` keyword. Parity is semantic and runs afterwards. They are complements — the guard catches output that is not code, parity catches code that is not the program.

## Known limits

Parity is an **identifier-retention check**. It answers "are the names the COBOL declared still visible in the generated code", which is a proxy for structural representation and nothing more. The limits below follow from that and are not defects.

- Matching is name-based. A conversion that faithfully reproduces behaviour under entirely unrelated names scores low; one that keeps the names while discarding the logic scores high. Parity is evidence of structural representation, not of correctness.
- Names that normalise identically are counted, not distinguished. `1000-INIT` and `2000-INIT` both reduce to `init`, so two generated `init` methods are needed to satisfy both; a single one leaves the second reported as a gap. Two paragraphs deliberately merged into one method therefore read as one loss. That direction is chosen on purpose: an under-count is a visible gap, whereas crediting one match to many names is a silent false pass.
- Scope is one source program against its own generated files. A field declared in a copybook and rendered into a shared record class belonging to a *different* program is reported missing, because the check never looks outside the program's own output. Where the section is known, the gap carries it — a `LINKAGE` field is annotated as supplied by the caller — so the reader can tell this case from a genuine drop.
- Recall depends on what the scan surfaced. A `CALL` nested inside a conditional is emitted by the parser as untyped statement text, and is recovered by scanning that text for quoted targets; a construct that survives in neither form cannot be expected, so its loss would go unreported. The axis reports `Expected = 0` rather than claiming coverage.
- Compact-form fallback matching can pair a short expected name with a longer identifier that contains it.
- Parity needs the repository layout to resolve source programs and copybooks. Running from a published image without it, the post-pass logs that it is skipping rather than reporting a false pass.
- Programs are identified by file name, not by path. Generated files record only the source basename, so `billing/CUSTOMER.cbl` and `claims/CUSTOMER.cbl` cannot be told apart from their output. Rather than merge them into one entry and drop the second from the report, the check reports the collision and measures neither. Renaming one of the two restores coverage.
- Standalone parity scoring does not independently verify every structural artifact's freshness. `ProgramFacts.SourceHash` covers staged/preprocessed bytes, not original bytes. Migration entrypoints separately require the source/export/definition-bound dependency evidence described above before invoking models; that gate is not proof of every AST fact or behavioral equivalence.

### Dimensions not validated

Presence of a name says nothing about these, and none of them is checked:

| Dimension | Not checked |
|---|---|
| SQL | which *operations* run against a table — only that the table is named |
| Data | `PIC` precision, `USAGE`, `OCCURS` bounds, `REDEFINES` overlays |
| Conditions | level-88 values, and whether the condition survives at all |
| Calls | parameter lists, `BY REFERENCE` / `BY CONTENT` semantics |
| Transactions | CICS and IMS verbs |
| Control flow | ordering, nesting, `GO TO` semantics |

A program can pass parity at 1.00 and still be a wrong conversion. The report says so.

## Not ported from the source branch

The upstream branch pairs the validator with an LLM repair pass, enabled by default, which rewrites the generated file to close gaps. It is not ported.

Its prompt template takes a `{{CobolContent}}` placeholder that the agent never supplies, and supplies `Gaps`, `StructuralContext`, `CobolSource`, `ConvertedCode` and `TargetLanguage`, none of which the template reads. Placeholders are not validated at load time, so the model would receive an analysis prompt containing a literal `{{CobolContent}}` and none of the code, and its reply would overwrite the conversion. Enabled by default, that corrupts conversions that were already correct.

The gap-classification ladder the upstream changelog describes — missing to added code, renamed and merged to comments, deferred to `TODO` — is a sound design that the shipped prompt never implemented. The deterministic half of it is implemented here as the `Missing` / `PossiblyRenamedOrMerged` distinction. A repair pass remains possible future work, but needs a prompt that matches its own inputs.
