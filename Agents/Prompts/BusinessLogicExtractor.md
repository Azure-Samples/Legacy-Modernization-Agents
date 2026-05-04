## SECTION: System

Extract business logic from the COBOL codebase (29 programs, 37 copybooks).

## Extraction Focus Areas
For each program, extract:
1. **Business Purpose** — what business function does this program serve?
2. **Business Rules** — all IF/EVALUATE conditions that encode business decisions (not just flow control).
3. **Validations** — input validation rules, range checks, cross-field validations.
4. **Calculations** — formulas, rates, accumulations with exact precision requirements.
5. **State Transitions** — how records/transactions change state through processing.

- **Data Rules**: Extract business meaning of each SQL query — not just the SQL, but what business operation it represents.
- **Transaction Rules**: Extract the business workflow encoded in CICS transaction flows.
- **Calculation Rules**: Document every COMPUTE/ADD/SUBTRACT/MULTIPLY/DIVIDE with its business meaning and precision.

## Output Format
Describe business logic in **domain language**, not COBOL syntax. A business analyst should understand the output without knowing COBOL.


## Banking Business Semantics to Extract
- **Funds Transfer Rules (XFRFUN)**:
  - Transfer amount must be > 0.
  - FROM and TO accounts must not be identical.
  - No overdraft limit checks are performed (explicit business decision).
  - Both available and actual balances are updated symmetrically.
  - Transaction is atomic: either both accounts and PROCTRAN are updated, or none.
- **Customer Maintenance (BNK1DCS)**:
  - PF5 deletes a customer and all associated accounts.
  - PF10 enables update mode; ENTER commits updates.
  - Customer names must start with an approved title list.
  - Address must not be entirely blank.
- **Batch Data Generation (BANKDATA)**:
  - Generates synthetic customers/accounts within a numeric range.
  - Each customer has 1–5 accounts of varying types.
  - Loan/Mortgage accounts always have negative balances.
- **Operational Resilience**:
  - Deadlocks and timeouts trigger retries or controlled abends.
  - Storm Drain conditions defer failure handling to workload management.

## SECTION: User

Extract the business logic from the following COBOL program.

## Glossary Context
{{GlossaryContext}}

## Source File: {{FileName}}
```cobol
{{CobolContent}}
```

## Extraction Requirements
1. Business rules in domain language (not COBOL syntax)
2. Validations and data transformations
3. Calculations with precision requirements
4. Decision trees and state transitions

