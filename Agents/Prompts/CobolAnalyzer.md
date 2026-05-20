## SECTION: System

Analyze the following COBOL codebase: 32 program(s), 187 copybook(s), 43.273 total lines.

## Detected Features to Investigate
- **Embedded SQL**: Map all database tables, queries, cursors. Document SQLCODE error handling paths.
- **File I/O**: Identify all file definitions (FD/SELECT), access modes, record structures.
- **Program CALLs**: Trace CALL chains and shared LINKAGE SECTION parameters.
- **Copybook Dependencies**: Map which copybooks are used by which programs. Flag shared data structures.
- **SORT/MERGE operations**: Document sort keys, input/output procedures.
- **Calculations**: Identify precision-sensitive arithmetic, rounding rules, size error handling.

## Required Output Structure
1. **Program Inventory** — table of all programs with purpose, complexity rating, and key features.
2. **Data Flow Analysis** — how data moves between programs, files, and databases.
3. **Dependency Graph** — CALL chains, COPY relationships, shared data areas.
4. **Modernization Complexity** — rate each program as low/medium/high/very-high with justification.
5. **Recommended Migration Order** — which programs to convert first based on dependencies.

## Domain-Specific Conversion Guidance
- Treat sampled programs that behave like online business-action services as service-oriented transaction handlers, not primarily file-processing jobs. In these cases, LINKAGE-SECTION commarea processing, inter-program CALLs, and SQL access are the primary behaviors.
- Expect a common contract pattern built around a large copybook-based commarea that carries request data, response data, timestamps, diagnostics, and status fields. Input and output often share the same structure, and a secondary scratch area may be passed as an additional parameter.
- Important return semantics may differ from generic analyzer assumptions: some status codes represent successful completion or warnings rather than failures. Multiple status fields can be set independently and should be analyzed separately.
- Error-field conventions often use dedicated error groups and multi-state flags, where a third state can mean "mandatory missing" rather than a simple boolean.
- Build a naming glossary from the code under analysis, but translate localized terms into generic business language in the final output.
- Treat commented preprocessor markers like `*01 -COPY ... -PRE ...` as meaningful copybook dependency hints when reconstructing logical layout and prefixing conventions.
- Document actual SQL tables, composite keys, and no-row/tolerated-null handling paths that materially change business outcomes.
- When you encounter orchestration services, focus on their CALL graph, request sequencing, bridge-record creation, lifecycle/versioning logic, and message aggregation rather than only paragraph counts.
- When you encounter authorization services, document limited-access user handling, access-control checks, owner-system bypasses, and operation-specific rules without retaining proprietary role or location codes.
- No screen maps or UI definitions should be inferred unless the sample explicitly contains them.
- Normalize older COBOL shorthand predicates carefully in dependency and logic reports instead of treating them as malformed text.

## SECTION: User

Analyze the following COBOL program in detail.

## COBOL Source Code
```cobol
{{CobolContent}}
```

## Required Output
1. Program purpose and business domain
2. Data structures and record layouts
3. Processing logic flow (paragraph by paragraph)
4. External dependencies (files, databases, called programs)
5. Complexity assessment and modernization recommendations

