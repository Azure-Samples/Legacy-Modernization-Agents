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


## Domain-Specific Insights from This Codebase
- **Banking Domain Model**: Core entities are CUSTOMER (VSAM), ACCOUNT (DB2), PROCTRAN (DB2), CONTROL (DB2). CUSTOMER–ACCOUNT is 1-to-many; PROCTRAN records financial movements such as transfers.
- **Monetary Semantics**: All monetary amounts use `PIC S9(10)V99 COMP-3` (packed decimal). No overdraft validation is performed during transfers; negative balances are explicitly allowed (business rule).
- **Transaction Ordering Rule**: In XFRFUN, FROM/TO account updates are ordered by account number comparison to reduce DB2 deadlock probability. This ordering is a critical concurrency-control rule.
- **Commit/Rollback Discipline**: Online programs rely on CICS SYNCPOINT with explicit ROLLBACK on partial failure. Batch (BANKDATA) commits every ~1000 records.
- **Error Taxonomy**: COMM-FAIL-CODE values are meaningful business states (e.g., '1'=FROM account not found, '2'=TO account not found, '3'=DB error). Do not treat them as generic flags.
- **Storm Drain / CPSM Awareness**: Programs explicitly detect DB2 SQLCODEs (e.g., 923, -911 with reason codes 13172872/13172894) and VSAM RLS abends (AFCR/AFCS/AFCT) to cooperate with WLM Storm Drain. This is an operational resilience pattern.
- **CICS UI Pattern**: BNK1DCS follows classic pseudo-conversational design: RECEIVE MAP → process → SEND MAP → RETURN TRANSID. Terminal UCTRAN settings are dynamically altered to preserve mixed-case input.
- **Date Handling**: Dates are stored as strings (DD.MM.YYYY or DDMMYYYY depending on context) and frequently redefined. Integer-of-date / date-of-integer functions are used in batch.
- **Copybook Roles**: COPY members like SORTCODE, ACCOUNT, PROCTRAN, ABNDINFO, CUSTCTRL, ACCTCTRL define shared canonical structures—treat them as system-wide contracts.

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

