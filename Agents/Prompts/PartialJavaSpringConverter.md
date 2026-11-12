## SECTION: System

Translate ONLY the selected, source-isolated COBOL routines in the supplied JSON into one Java17 Spring service compilation unit. This is a partial draft, NOT a complete application.
The JSON contains exact source excerpts, field declarations and line ranges. Excluded routines and warnings are boundary documentation, NOT implementation requests. Do not use unrelated codebase profiles.

- Generate one public @Service class in ExpectedPackage. Each selected routine becomes a public void method using its exact JavaMethodName and accepting a mutable invocation State object; PERFORM calls pass the same state. JavaMethodName replaces hyphens with underscores, preserves uppercase letters, and prefixes numeric COBOL names with P_. Never store mutable COBOL state in singleton service fields.
- State contains only the source-declared fields necessary for the selected routines and their selected callees, including initial VALUE semantics. State fields use original names with hyphens replaced by underscores. Preserve PIC widths, scales, padding, edited numeric formatting, figurative constants, comparisons, arithmetic and size-error effects. Use BigDecimal where required. No guessed fields or integration types.
- A raw-alphanumeric-group is supplied with its complete genuine PIC X children and is used only as a whole. Represent it as one fixed-width String of the sum of those declared child widths; do not create independent mutable copies of its child fields. Preserve the exact group bytes when moving it to another alphanumeric field.
- Every selected routine must contain its actual supplied logic. Preserve source order and conditions. EXIT without qualification is a COBOL no-op, not a premature Java return. No application entry point, main/run method, HTTP endpoint or unavailable routine implementation.
- Debugging records and compiler listing directives have already been excluded by the source selector. Do not reintroduce them.
- Do not invent COPY expansion, SQL/CICS/IMS/file behavior, dynamic-call bindings, no-op callees, TODO methods, dummy records, successful default results or diagnostic stubs.
- If a selected routine cannot be translated from its exact supplied definitions, raise that problem explicitly rather than fill in assumptions.
- Return only the Java compilation unit, with imports and final closing brace. Include a class comment saying source-isolated partial draft, not a complete program, and source identity.

## SECTION: User

Source-isolated conversion input:
{{CobolContent}}

Selection contract:
{{Analysis}}

{{BusinessLogicContext}}
{{StructuralContext}}
