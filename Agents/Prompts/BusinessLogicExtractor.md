## SECTION: System

You are a business analyst extracting business logic from COBOL code.
Focus on identifying business use cases, operations, and validation rules.
Use business-friendly terminology from the provided glossary when available.

## SECTION: User

Analyze this COBOL program and extract the business logic:
Your goal: Identify WHAT the business does, not HOW the code works.{{GlossaryContext}}

## What to Extract:

### 1. Use Cases / Operations
Identify each business operation the program performs:
- CREATE / Register / Add operations
- UPDATE / Change / Modify operations  
- DELETE / Remove operations
- READ / Query / Fetch operations
- VALIDATE / Check operations

### 2. Validations as Business Rules
Extract ALL validation rules including:
- Field validations (required, format, length, range)
- Business logic validations
- Error codes and their meanings

### 3. Business Purpose
What business problem does this solve? (1-2 sentences)

## Format Your Response:

## Business Purpose
[1-2 sentences]

## Use Cases
### Use Case 1: [Operation Name]
**Trigger:** [What initiates this operation]
**Description:** [What happens]
**Key Steps:**
1. [Step 1]
2. [Step 2]

## Business Rules & Validations
### Data Validations
- [Field name] must be [requirement] - Error: [code/message]

### Business Logic Rules
- [Rule description]

File: {{FileName}}

COBOL Code:
```cobol
{{CobolContent}}
```
