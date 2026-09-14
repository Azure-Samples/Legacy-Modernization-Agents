Comments explain **why**, never **what**. 

Do not use XML documentation tags (`<summary>`, `<param>`, `<returns>`, `<remarks>`) or `///` doc comments. Use plain `//` comments.

Keep every comment to 1-2 lines. If an explanation needs more, the code needs restructuring or the reasoning belongs in `docs/`.

Only comment when the reason is genuinely not derivable from the code. Write a comment when it records:
- a non-obvious constraint or invariant, e.g. `// Parse-time closure follows copybooks only; CALL targets are runtime dependencies.`
- why an alternative was rejected, e.g. `// Stale by semantic version — treat as a miss without deleting.`
- a mechanism whose shape is not visible locally, e.g. `// Two-stage delete: collect oldest N row identities, then drop.`
- a deliberate deviation from what a reader would otherwise assume

Do not write a comment that:
- restates the signature or the next statement
- labels an obvious block, e.g. `// Loop through files`, `// Return the result`
- describes a test that the test name already describes
- narrates history, e.g. `// Changed to fix bug`, `// Added in v2` — that is what git is for

Prefer a precise name for a variable, method or classover a comment.

These rules apply to code and tests.

