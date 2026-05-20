## SECTION: System

Process the COBOL codebase: 32 programs, 187 copybooks, 43.273 lines.

Detected features: ARITHMETIC, CALL_PROGRAM, COPYBOOK_REF, EXEC_SQL, FILE_IO, SORT_MERGE, STRING_HANDLING, TABLE_HANDLING.

Provide a comprehensive analysis and conversion-ready assessment of this codebase.


## Domain-Specific Conversion Guidance
- Generate tests around shared business-action contracts, not just paragraph coverage. Each synthesized test should assert status propagation, messages, diagnostics, and field-level validation flags where relevant.
- Include success-state tests where nonzero status values still represent successful completion, and include warning-path tests when warnings are intentionally non-fatal.
- High-value scenario sets should cover:
  - Single-category create/update flows.
  - Multi-category requests where downstream sections reuse identifiers from earlier sections.
  - Cross-category identifier propagation.
  - External bridge creation and filename parsing with both valid and invalid combinations.
  - Change-request, lifecycle, stage, and maturity prerequisites.
  - Mandatory attribute updates, fallback flows, and coupled validation flags.
  - Authorization decisions for limited-access users across view, update, create, and link scenarios.
  - Defaulting, numbering, migration-origin exceptions, derived flags, alias generation, and status-update logic.
- For SQL tests, preserve no-row and tolerated-null semantics when they represent business behavior instead of exceptional failure.
- Synthesized mocks and stubs should model service CALLs and repository behavior rather than assuming flat-file integrations.
- Include regression tests for shorthand COBOL OR conditions, OCCURS list ordering, and overlay semantics because these are easy to mistranslate in converted code.

## SECTION: User

Process the following COBOL source code.

```cobol
{{CobolContent}}
```

Provide comprehensive analysis and output.

