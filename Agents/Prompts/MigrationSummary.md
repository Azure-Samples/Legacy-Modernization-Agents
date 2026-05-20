## SECTION: System

Process the COBOL codebase: 32 programs, 187 copybooks, 43.273 lines.

Detected features: ARITHMETIC, CALL_PROGRAM, COPYBOOK_REF, EXEC_SQL, FILE_IO, SORT_MERGE, STRING_HANDLING, TABLE_HANDLING.

Provide a comprehensive analysis and conversion-ready assessment of this codebase.

## Domain-Specific Conversion Guidance
- Reclassify service-oriented samples as online transactional orchestration built from many small business actions, not batch file-processing. The migration summary should emphasize commarea contracts, inter-service CALL chains, SQL access, and shared copybook schemas over files.
- Central shared schema pattern: large commarea copybooks often hold request data, field-level error flags, and response data in a single structure. These copybooks are the real integration surface and should be treated as first-class migration assets.
- Highest-risk modernization areas commonly include:
  1. Shared business-action protocol and status semantics.
  2. Large nested copybooks with REDEFINES, OCCURS, indexes, and multi-state flags.
  3. Security and authorization logic for limited-access users.
  4. Orchestrator programs that chain many create/update services and reuse returned identifiers.
  5. Domain rules tied to external-system integration, lifecycle/versioning, and location codes.
- Recommend migration sequencing generically:
  1. Shared commarea/copybook model layer and status/message conventions.
  2. Low-level authorization and lookup services.
  3. Core category-specific create/update services and bridge/version services.
  4. High-level orchestrators after lower-level services are behaviorally reproduced.
- Call out key cross-cutting services such as authorization, numbering, bridge integration, message handling, and lookup services without retaining proprietary program names.
- State clearly when screen maps are absent and FILE SECTION content is mostly template residue to prevent over-scoping UI or file migration work.

## SECTION: User

Process the following COBOL source code.

```cobol
{{CobolContent}}
```

Provide comprehensive analysis and output.

