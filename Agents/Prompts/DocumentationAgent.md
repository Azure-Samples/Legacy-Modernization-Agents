## SECTION: System

Process the COBOL codebase: 32 programs, 187 copybooks, 43.273 lines.

Detected features: ARITHMETIC, CALL_PROGRAM, COPYBOOK_REF, EXEC_SQL, FILE_IO, SORT_MERGE, STRING_HANDLING, TABLE_HANDLING.

Provide a comprehensive analysis and conversion-ready assessment of this codebase.

## Domain-Specific Conversion Guidance
- Document the system as a business-action service suite when the sampled code behaves that way, rather than defaulting to a generic file-processing COBOL estate. Emphasize online commarea contracts, inter-program services, SQL lookups, and external-system integration.
- Include a glossary for recurring legacy field prefixes and localized terms, but translate them into generic business language in public documentation.
- Explain the common business-action contract once and reuse it across docs: a shared contract typically carries user context, action name, usage code, result code, messages, diagnostics, timestamps, origin markers, and flags.
- Call out status semantics clearly because they may be non-intuitive: some numeric status codes represent successful completion or warnings, while rollback or business-error indicators may use a separate field.
- Describe orchestration services generically as multi-category create/update flows with bridge creation, mandatory-attribute updates, reference creation, lifecycle/version creation, and aggregated message handling.
- Describe authorization services generically as external-user access checks driven by location, organization, access-control settings, owner-system indicators, and operation type.
- Describe create services generically as handling numbering, required attributes, optional weight or characteristics, relations, aliases, event logging, and status updates.
- Document external dependencies generically: SQL tables, service CALLs, numbering services, access-control services, and bridge services.
- Document domain codes and rules only at the level needed to explain migration behavior; do not retain proprietary names or customer-specific codes.
- State explicitly when screen maps are absent and FILE SECTION content is mostly boilerplate so the architecture description stays accurate.

## SECTION: User

Process the following COBOL source code.

```cobol
{{CobolContent}}
```

Provide comprehensive analysis and output.

