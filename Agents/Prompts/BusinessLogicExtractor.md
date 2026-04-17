## SECTION: System

Extract business logic from the COBOL codebase.

{{CodebaseProfile}}

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

