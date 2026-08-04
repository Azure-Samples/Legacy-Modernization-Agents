# Local patches for cobol-rekt v0.1.0-RC6

These patches are applied during the Docker image build (see `../Dockerfile`)
to address gaps in the upstream smojol/Eclipse-LSP-COBOL pipeline that
would otherwise force large numbers of programs into "deps-only" mode.

Each patch is a unified diff, applied with `git apply` in lexical order.

## 0001-lenient-parse-pipeline.patch

**File touched:** `smojol-toolkit/src/main/java/org/smojol/toolkit/analysis/pipeline/ParsePipeline.java`

**Problem.** Upstream `ParsePipeline` throws `ParseDiagnosticRuntimeError`
the moment any diagnostic accumulates on the parse context, regardless of
severity or whether a parse tree was actually produced. Large COBOL
codebases routinely trip this for benign issues such as:

- "Fragment" copybooks that open at level `05` instead of `01` (meant to
  be COPY'd inside an existing structure) — emits hundreds of
  `A period was assumed before "10"` warnings that smojol promotes to
  fatal.
- Unresolved third-party COPY targets (`MISSING_COPYBOOK`) that our
  stub generator already satisfies downstream.

**Fix.** When accumulated diagnostics are present, log them (preserving
the upstream `LOGGER.info` behaviour) and proceed only when every
diagnostic is below `ERROR` severity and a parse tree was produced.
`ERROR` diagnostics and missing parse trees remain fatal so partial
structures are not published as authoritative.

The downstream pipeline already surfaces error counts via
`output/rekt/*.parse.log` and `output/rekt/missing-copybooks.txt`, and
the migration report flags affected programs as "reduced fidelity" so
the user is not misled about AST quality.

**Net effect.** Substantially fewer programs fall back to deps-only mode,
because a usable parse tree is no longer discarded over recoverable
diagnostics.

## 0002-null-safe-data-division.patch

**File touched:** `smojol-core/src/main/java/org/smojol/common/structure/CobolDataStructureBuilder.java`

**Problem.** `CobolDataStructureBuilder.build()` unconditionally calls
`extractFromWorkingStorage(dataDivisionBody)` even when `dataDivisionBody`
is `null`. Some legitimate COBOL programs (especially IBM mainframe
control-shells that delegate everything via `CALL`) have no DATA DIVISION
at all, and partial parses can also leave the body `null`. The result is
a `NullPointerException` deep inside extraction.

**Fix.** Guard the three `extractFrom…` calls with a null-check; log a
warning and continue with system globals only when no DATA DIVISION is
present.

**Applies to:** programs with no DATA DIVISION, and partial parses that
leave the division body null.

## 0003-null-safe-entry-name.patch

**File touched:** `smojol-core/src/main/java/org/smojol/common/vm/structure/NamingScheme.java`

**Problem.** `NamingScheme.IDENTITY` and `INDEXED` call
`d.entryName().getText()`. When a `DataDescriptionEntryFormat1Context`
has a `null` `entryName()` (typically when the entry is FILLER-only or
ANTLR couldn't bind a name to the token), the chain NPEs.

**Fix.** Return the sentinel string `[FILLER]` instead of NPEing.

**Applies to:** FILLER-only entries, and entries where ANTLR could not bind
a name to the token.

## 0004-tolerate-unknown-class-condition.patch

**Files touched:**

- `smojol-core/src/main/java/org/smojol/common/vm/expression/ClassConditionBuilder.java`
- `smojol-core/src/main/java/org/smojol/common/vm/expression/UnknownClassCondition.java`

**Problem.** COBOL allows user-defined class conditions via the `CLASS`
clause in `SPECIAL-NAMES` (e.g. `01 X IS AND15` where `AND15` was declared
elsewhere). Upstream `ClassConditionBuilder` only handles the built-in
classes (NUMERIC, ALPHABETIC, ALPHABETIC-LOWER, ALPHABETIC-UPPER, POSITIVE,
NEGATIVE, ZERO) and throws `UnsupportedClassConditionException` for any
other class name, killing the whole program.

**Fix.** Log a warning and preserve the source condition text in an explicit
`UNKNOWN_CLASS_CONDITION` node. The node remains opaque rather than being
substituted with a built-in predicate, so downstream consumers can re-derive
the real semantics from the program's `SPECIAL-NAMES CLASS …` declaration
without receiving incorrect numeric semantics.

**Applies to:** programs that declare custom class conditions via
`SPECIAL-NAMES`.

## 0005-skip-null-ast-children.patch

**File touched:** `smojol-core/src/main/java/org/smojol/common/ast/BuildSerialisableASTTask.java`

**Problem.** `buildContextGraph` blindly iterates `astParentNode.getChild(i)` and
hands the result to `new CobolContextAugmentedTreeNode(astChildNode, …)`, whose
constructor calls `astNode.getClass()`. ANTLR can legitimately return `null`
children for partially-recovered parse trees (typical for IMS/CICS dialect
statements that the parser inserted error tokens for). Result: NPE.

**Fix.** Skip null children with a `continue`; the rest of the AST is still
serialised.

**Applies to:** partially-recovered parse trees, typically IMS/CICS dialect
statements where the parser inserted error tokens.

## 0006-safe-data-spec.patch

**File touched:** `smojol-core/src/main/java/org/smojol/common/vm/structure/Format1DataStructure.java`

**Problem.** `spec()` calls `dataPictureClause().getFirst().pictureString().getFirst()`,
which throws `NoSuchElementException` on entries with no PIC clause (group-level
intermediates, USAGE-only entries, IMS/CICS dialect-rewritten entries).

**Fix.** Guard the `getFirst()` calls; fall back to `X(1)` when no picture string
is available, logging a warning. Memory layout still resolves and downstream
code continues.

**Applies to:** entries with no PIC clause — group-level intermediates,
USAGE-only entries, and dialect-rewritten entries.

## 0007-tolerate-null-procedure-division.patch

**File touched:** `smojol-toolkit/src/main/java/org/smojol/toolkit/analysis/task/analysis/BuildBaseModelTask.java`

**Problem.** `BuildBaseModelTask.run()` passes the result of
`navigator.procedureDivisionBody(...)` straight to `BuildSerialisableASTTask`.
Programs whose body lives entirely in a missing copybook (COPY-only stubs),
and partial parses, leave that context `null`, so the build NPEs.

**Fix.** When the procedure-division body is `null`, log a warning and return
a degenerate `BaseAnalysisModel` instead. The file still reaches Neo4j as a
`CobolFile` carrying its declared COPY dependencies, and downstream
`WRITE_FLOW_AST` / `WRITE_CFG` tasks can skip it cleanly.

**Applies to:** COPY-only stub programs and partial parses.

## 0008-skip-null-flow-writers.patch

**Files touched:** `WriteFlowASTTask.java`, `WriteControlFlowGraphTask.java`
(both under `smojol-toolkit/src/main/java/org/smojol/toolkit/analysis/task/analysis/`)

**Problem.** Both writers dereference `astRoot` unconditionally. Given the
degenerate model produced by patch `0007`, `astRoot` is `null` and the
visitor NPEs — which would turn a recoverable partial parse back into a
hard failure.

**Fix.** Return `AnalysisTaskResult.OK` early when `astRoot` is `null`, so
flow-AST and CFG emission are skipped while data-structure output still
lands on disk.

**Applies to:** any program that produced a degenerate model via `0007`.
Must stay in sync with `0007`.
