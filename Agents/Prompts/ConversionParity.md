## SECTION: System

Process the COBOL codebase: 32 programs, 187 copybooks, 43.273 lines.

Detected features: ARITHMETIC, CALL_PROGRAM, COPYBOOK_REF, EXEC_SQL, FILE_IO, SORT_MERGE, STRING_HANDLING, TABLE_HANDLING.

Provide a comprehensive analysis and conversion-ready assessment of this codebase.

## Domain-Specific Conversion Guidance
- Preserve the business-action protocol exactly: same commarea shape, same field names where externally observable, same status fields, same side effects on request/error/response areas, and the same multi-parameter CALL conventions.
- Do not normalize away status semantics. Some codes represent success or warning states, and conversion parity requires reproducing those exact combinations rather than converting everything into exceptions.
- Preserve field-level validation flags. Multi-state flags are part of the observable contract and must retain their original meaning.
- Preserve call ordering in orchestration services. Later sections may reuse identifiers and status produced by earlier sections.
- Preserve cross-category identifier propagation behavior exactly when a request can create or update multiple related entity types.
- Preserve external-system bridge rules and filename parsing rules when they are part of business logic rather than utility logic.
- Preserve lifecycle and version rules, coupled validation behavior, location-based authorization semantics, and migration/origin exceptions.
- Preserve SQL no-row handling and cursor logic when business outcomes depend on empty-result behavior.
- Treat commented `-COPY ... -PRE ...` lines as authoritative dependency hints when reconstructing logical copybook layouts.
- Ignore generic file-processing assumptions unless a specific sample actually performs meaningful file I/O.

## SECTION: User

Process the following COBOL source code.

```cobol
{{CobolContent}}
```

Provide comprehensive analysis and output.

