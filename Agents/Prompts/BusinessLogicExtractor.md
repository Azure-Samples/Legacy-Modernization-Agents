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


## Business Rules Identified
- **Funds Transfer (XFRFUN)**:
  - Reject transfers with non-positive amounts.
  - Reject transfers where FROM and TO account keys are identical.
  - Debit and credit must be atomic; partial success triggers rollback.
  - No overdraft or balance validation is enforced.
- **Customer Maintenance (BNK1DCS)**:
  - Customer number must be numeric and not special sentinel values.
  - Valid titles are restricted to a fixed whitelist (Mr, Mrs, Miss, Ms, Dr, Professor, etc.).
  - Address cannot be entirely blank.
  - PF5 deletes customer and all associated accounts.
  - PF10 unlocks fields for update, ENTER commits update.
- **Data Initialization (BANKDATA)**:
  - Generates synthetic customers/accounts with controlled randomness.
  - Account opened date must not precede customer date of birth.
  - Loan and Mortgage accounts must have negative balances.
  - Control records track last account number and total account count per sort code.
- **Operational Resilience**:
  - Certain DB2/VSAM failures are tolerated and delegated to platform-level workload management (Storm Drain).

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

