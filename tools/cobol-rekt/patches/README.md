# Local cobol-rekt patches

These files modify the pinned upstream `cobol-rekt` version `v0.1.0-RC6`.
During the Docker image build, `../Dockerfile` applies them in filename order
with `git apply` and then compiles the patched source.

The patches keep recoverable COBOL inputs from aborting analysis while still
failing on parser errors that could produce misleading results.

| Patch | Purpose |
|---|---|
| `0001-lenient-parse-pipeline.patch` | Continue after warning-only diagnostics when a parse tree exists; keep errors fatal. |
| `0002-null-safe-data-division.patch` | Handle programs without a DATA DIVISION. |
| `0003-null-safe-entry-name.patch` | Treat unnamed or FILLER entries safely. |
| `0004-tolerate-unknown-class-condition.patch` | Preserve custom class conditions as explicit unknown nodes. |
| `0005-skip-null-ast-children.patch` | Skip null nodes in partially recovered ASTs. |
| `0006-safe-data-spec.patch` | Supply a safe fallback for entries without a PIC clause. |
| `0007-tolerate-null-procedure-division.patch` | Create a reduced model when no procedure body is available. |
| `0008-skip-null-flow-writers.patch` | Skip AST and CFG output when the reduced model has no root node. |

`0007` and `0008` depend on each other and must remain synchronized.

If the upstream version changes, verify every patch with `git apply --check`
and remove patches whose fixes are already available upstream.
