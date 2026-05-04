## SECTION: System

Analyze the following COBOL codebase: 29 program(s), 37 copybook(s), 27.320 total lines.

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


## Domain-Specific Observations (Banking/CICS/DB2)
- **Core Banking Domain**: This codebase implements a retail banking application with CUSTOMER, ACCOUNT, PROCTRAN (processed transactions), REJTRAN (rejected transactions), and CONTROL tables. Programs fall into three categories: online CICS transactions (BNK1DCS, XFRFUN), batch initialisation (BANKDATA), and service-style DB2/VSAM access programs (INQCUST, UPDCUST, DELCUS, INQACC, CREACC, DELACC, etc.).
- **Transaction Integrity Pattern**: XFRFUN enforces strict debit/credit atomicity using EXEC CICS SYNCPOINT with explicit ROLLBACK on partial failure. The ordering of FROM/TO account updates is deliberately chosen by comparing account numbers to reduce DB2 deadlock probability.
- **SQL Error Semantics**: SQLCODE handling is business-significant: +100 = not found (mapped to COMM-FAIL-CODE values like '1' or '2'); -911 with SQLERRD(3)=13172872 indicates DB2 deadlock; SQLERRD(3)=13172894 indicates timeout. Storm Drain logic is triggered for SQLCODE 923 and certain VSAM RLS abends (AFCR/AFCS/AFCT).
- **CICS Error Handling**: A centralized ABNDPROC program is used. Many programs populate ABNDINFO copybook with RESP/RESP2, program name, timestamps, SQLCODE, and free-form diagnostics before LINKing to ABNDPROC.
- **COMMAREA Contracts**: DFHCOMMAREA structures are stable APIs between programs (e.g., XFRFUN.cpy, INQCUST.cpy, UPDCUST.cpy). Treat them as service contracts during analysis.
- **Date/Time Conventions**: Dates are stored as DD.MM.YYYY strings in DB2 columns, but often split/redefined into numeric components. CICS ASKTIME/FORMATTIME is the canonical source of time.
- **Arithmetic Precision**: Monetary values use PIC S9(10)V99 COMP-3 consistently; no overdraft limit validation is performed in transfers (explicitly documented business rule).
- **BMS Screen Flow**: BNK1DCS implements a classic conversational CICS pattern: initial ERASE send, RECEIVE ASIS, PF-key driven state machine, cursor positioning via -1 length fields, and dynamic field protection/unprotection.
- **Batch vs Online Split**: BANKDATA is non-CICS, batch-only, uses random data generation, frequent DB2 COMMIT WORK, and VSAM writes; do not conflate its behavior with online programs.

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

