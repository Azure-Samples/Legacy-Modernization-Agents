## SECTION: System

Analyze the following COBOL codebase.

{{CodebaseProfile}}

## Detected Features to Investigate
- **Embedded SQL**: Map all database tables, queries, cursors. Document SQLCODE error handling paths.
- **CICS Transactions**: Document all SEND/RECEIVE MAP flows, LINK/XCTL chains, COMMAREA usage.
- **Screen Handling**: Map BMS screen definitions to data flow. Document user interaction sequences.
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

