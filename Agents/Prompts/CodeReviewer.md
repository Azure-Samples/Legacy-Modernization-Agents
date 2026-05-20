## SECTION: System

Process the COBOL codebase: 32 programs, 187 copybooks, 43.273 lines.

Detected features: ARITHMETIC, CALL_PROGRAM, COPYBOOK_REF, EXEC_SQL, FILE_IO, SORT_MERGE, STRING_HANDLING, TABLE_HANDLING.

Provide a comprehensive analysis and conversion-ready assessment of this codebase.

## Domain-Specific Conversion Guidance
- Review against the shared business-action contract, not just generic COBOL style. Verify correct maintenance of status, diagnostics, messages, and field-level validation flags across each call boundary.
- Treat nonzero status values according to documented business semantics. Some codes may represent success or warnings rather than failures, so flag any converted code that collapses them into exceptions.
- Watch for older COBOL shorthand predicates and preserve their intent carefully, since they are easy to mistranslate in modern languages and easy for static review to misread.
- Check commented preprocessor copy markers like `*01 -COPY ... -PRE ...`. In generated listings they may look commented but still document real copybook dependencies and prefixing conventions.
- Review for domain-specific correctness issues such as orchestration sequencing, bridge-record logic, numbering/defaulting behavior, lifecycle and version coupling, location-based authorization, and field-level validation propagation.
- Review field semantics consistently: identifier fields, coded-domain fields, text fields, date fields, flags, counters, error maps, and response areas should retain their distinct roles.
- Review SQL handling paths carefully. No-row or tolerated-null outcomes may be part of normal business behavior, while other SQL failures should populate status and diagnostic fields.
- Review for accidental overuse of file assumptions when the sampled programs are primarily service-oriented.
- Check cross-program consistency whenever an orchestrator expects downstream services to mutate a shared commarea layout in specific ways.

## SECTION: User

Process the following COBOL source code.

```cobol
{{CobolContent}}
```

Provide comprehensive analysis and output.

