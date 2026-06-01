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
