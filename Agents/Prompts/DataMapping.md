## SECTION: System

Process the COBOL codebase: 32 programs, 187 copybooks, 43.273 lines.

Detected features: ARITHMETIC, CALL_PROGRAM, COPYBOOK_REF, EXEC_SQL, FILE_IO, SORT_MERGE, STRING_HANDLING, TABLE_HANDLING.

Provide a comprehensive analysis and conversion-ready assessment of this codebase.

## Domain-Specific Conversion Guidance
- Model service-oriented codebases around shared commarea copybooks, not just tables. A single copybook may contain common request metadata, category-specific request sections, field-level error maps, response sections, bridge/reference payloads, and orchestration flags.
- Map naming conventions consistently into generic business language:
  - ID* = identifiers or keys
  - KD* = coded domain values
  - TX* / BE* = text or descriptions
  - TI* = dates or timestamps
  - FL* = flags
  - KV* = counters or quantities
  - DATA-* = request payload
  - FEL-* = field error flags
  - SVAR-* = response payload
  - repeating DATA groups = child collections
- Shared status fields across business actions should be modeled as reusable contract elements rather than scattered ad hoc properties.
- Preserve category-specific request, error, response, bridge, reference, and orchestration groups explicitly in the mapping model.
- Document representative domain values only in generic terms, such as category codes, user-type codes, action codes, access-control codes, lifecycle codes, and maturity codes.
- Map table layouts generically when needed, focusing on key fields, ownership attributes, lifecycle/version attributes, and authorization metadata instead of proprietary table names.
- Preserve list ordering for repeating groups because some business rules depend on first-row selection or loop order.
- Preserve REDEFINES overlay semantics instead of flattening them away without explanation.
- Do not infer screen-map structures unless the source explicitly contains them; many large copybooks are integration payloads and validation/result maps.

## SECTION: User

Process the following COBOL source code.

```cobol
{{CobolContent}}
```

Provide comprehensive analysis and output.

