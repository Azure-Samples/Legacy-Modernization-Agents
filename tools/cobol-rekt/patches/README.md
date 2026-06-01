# Local patches for cobol-rekt v0.1.0-RC6

These patches are applied during the Docker image build (see `../Dockerfile`)
to address gaps in the upstream smojol/Eclipse-LSP-COBOL pipeline that
would otherwise force large numbers of programs into "deps-only" mode.

Each patch is a unified diff, applied with `git apply` in lexical order.

## 0001-lenient-parse-pipeline.patch

**File touched:** `smojol-toolkit/src/main/java/org/smojol/toolkit/analysis/pipeline/ParsePipeline.java`

**Problem.** Upstream `ParsePipeline` throws `ParseDiagnosticRuntimeError`
the moment any diagnostic accumulates on the parse context, regardless of
severity or whether a parse tree was actually produced. In enterprise
COBOL estates this causes 30+ programs to abort for benign issues such
as:

- "Fragment" copybooks that open at level `05` instead of `01` (meant to
  be COPY'd inside an existing structure) — emits hundreds of
  `A period was assumed before "10"` warnings that smojol promotes to
  fatal.
- Unresolved third-party COPY targets (`MISSING_COPYBOOK`) that our
  stub generator already satisfies downstream.

**Fix.** When accumulated errors are present, log them (preserving the
upstream `LOGGER.info` behaviour), then proceed with AST construction
as long as a parse tree was actually produced. Only re-throw if the
parse tree is `null`, which is the only genuinely unrecoverable case.

The downstream pipeline already surfaces error counts via
`output/rekt/*.parse.log` and `output/rekt/missing-copybooks.txt`, and
the migration report flags affected programs as "reduced fidelity" so
the user is not misled about AST quality.

**Net effect on FUENTES estate:** deps-only programs drop from 38/65
to an expected 5–8/65.

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

**Affected programs in FUENTES estate:** KYGHO003/008/010/011,
KYGHT003/008/010/011 (8 programs).

## 0003-null-safe-entry-name.patch

**File touched:** `smojol-core/src/main/java/org/smojol/common/vm/structure/NamingScheme.java`

**Problem.** `NamingScheme.IDENTITY` and `INDEXED` call
`d.entryName().getText()`. When a `DataDescriptionEntryFormat1Context`
has a `null` `entryName()` (typically when the entry is FILLER-only or
ANTLR couldn't bind a name to the token), the chain NPEs.

**Fix.** Return the sentinel string `[FILLER]` instead of NPEing.

**Affected programs in FUENTES estate:** T66017J1 (1 program, plus
others where the NPE occurred mid-build).

## 0004-tolerate-unknown-class-condition.patch

**File touched:** `smojol-core/src/main/java/org/smojol/common/vm/expression/ClassConditionBuilder.java`

**Problem.** COBOL allows user-defined class conditions via the `CLASS`
clause in `SPECIAL-NAMES` (e.g. `01 X IS AND15` where `AND15` was declared
elsewhere). Upstream `ClassConditionBuilder` only handles the built-in
classes (NUMERIC, ALPHABETIC, ALPHABETIC-LOWER, ALPHABETIC-UPPER, POSITIVE,
NEGATIVE, ZERO) and throws `UnsupportedClassConditionException` for any
other class name, killing the whole program.

**Fix.** Log a warning and return an opaque `IsNumericCondition` fallback
so the AST/CFG still ship. The class-condition expression appears in the
AST so the LLM converter can re-derive the real semantics from the
program's `SPECIAL-NAMES CLASS …` declaration.

**Affected programs in FUENTES estate:** KYGHB077 (1 program).

## 0005-skip-null-ast-children.patch

**File touched:** `smojol-core/src/main/java/org/smojol/common/ast/BuildSerialisableASTTask.java`

**Problem.** `buildContextGraph` blindly iterates `astParentNode.getChild(i)` and
hands the result to `new CobolContextAugmentedTreeNode(astChildNode, …)`, whose
constructor calls `astNode.getClass()`. ANTLR can legitimately return `null`
children for partially-recovered parse trees (typical for IMS/CICS dialect
statements that the parser inserted error tokens for). Result: NPE.

**Fix.** Skip null children with a `continue`; the rest of the AST is still
serialised.

**Affected programs in FUENTES estate:** KYGHO003/008/010/011,
KYGHT003/008/010/011 (8 programs).

## 0006-safe-data-spec.patch

**File touched:** `smojol-core/src/main/java/org/smojol/common/vm/structure/Format1DataStructure.java`

**Problem.** `spec()` calls `dataPictureClause().getFirst().pictureString().getFirst()`,
which throws `NoSuchElementException` on entries with no PIC clause (group-level
intermediates, USAGE-only entries, IMS/CICS dialect-rewritten entries).

**Fix.** Guard the `getFirst()` calls; fall back to `X(1)` when no picture string
is available, logging a warning. Memory layout still resolves and downstream
code continues.

**Affected programs in FUENTES estate:** T66017J1 (1 program).
