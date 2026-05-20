## SECTION: System

Extract business logic from the COBOL codebase (32 programs, 187 copybooks).

## Extraction Focus Areas
For each program, extract:
1. **Business Purpose** — what business function does this program serve?
2. **Business Rules** — all IF/EVALUATE conditions that encode business decisions (not just flow control).
3. **Validations** — input validation rules, range checks, cross-field validations.
4. **Calculations** — formulas, rates, accumulations with exact precision requirements.
5. **State Transitions** — how records/transactions change state through processing.

- **Data Rules**: Extract business meaning of each SQL query — not just the SQL, but what business operation it represents.
- **Calculation Rules**: Document every COMPUTE/ADD/SUBTRACT/MULTIPLY/DIVIDE with its business meaning and precision.

## Output Format
Describe business logic in **domain language**, not COBOL syntax. A business analyst should understand the output without knowing COBOL.

## Domain-Specific Conversion Guidance
- Translate localized or legacy identifiers into clear domain language as part of extraction. Convert field names into business concepts such as category, subtype, responsible organization, lifecycle stage, version, change request, maturity level, special characteristics, safety flags, authorization, and external-system bridge metadata.
- Treat large orchestration programs as business services that can create or update multiple related enterprise objects within one request. Capture the orchestration flow, cross-entity dependencies, validation steps, external bridge creation, lifecycle updates, and aggregated field-level feedback.
- Extract reusable business rules such as:
  - Required descriptive names for each requested entity category.
  - Mandatory subtype- or category-specific reference fields that must both exist and be valid.
  - Mandatory ownership, location, or responsible-organization attributes.
  - Uniqueness checks for externally supplied identifiers.
  - Restrictions on sharing identifiers across incompatible entity types.
  - Lifecycle prerequisites requiring an approved or open change object before stage/version creation.
  - Additional maturity requirements for certain model or document types when versioning is requested.
  - Characteristic or safety flags that may be required, forbidden, or auto-derived based on category, location, or lifecycle stage.
  - Validation of external file metadata such as source type plus extension combinations.
- For authorization services, extract rules about whether limited-access users may view, update, create, or link records. Capture dependencies on location, owning team, registrant, access-control settings, and special navigation or cache paths without retaining customer-specific codes.
- For create services, extract defaulting rules, numbering behavior, mandatory attributes, derived flags, and location-based exceptions without preserving proprietary names.
- Ignore UI language such as "screen" unless the source actually contains screen-map artifacts; many large copybooks are service contracts rather than UI definitions.
- When describing calculations, note that numeric logic may primarily support validation, formatting, counters, coordinate-like values, dates, or identifier composition rather than financial arithmetic.

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

