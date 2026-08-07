## SECTION: CommonPolicy

---
REKT STRUCTURAL CONTEXT (authoritative — use this as the conversion blueprint):

{{SourceMetadata}}
FACT-LOCKING RULES — read these BEFORE looking at the structural context:
  • Treat the structural context below as GROUND TRUTH.
  • Every method you emit must map to a section or paragraph listed in the context.
  • Every field you emit must map to a data-structure entry in the context.
  • Never invent new fields, methods, classes, SQL operations, or CALL targets that are not present here.
  • If a name is unclear from the source, prefer the name in the structural context.
  • If the structural context shows zero items for a category (e.g. no CALL targets), do NOT generate any.

## SECTION: RawTargetPolicy

DATA STRUCTURE → DTO RULES:
  • For EVERY 01-level data group in the DATA STRUCTURE section below, generate a
    complete DTO/record class with ALL fields — not just the ones referenced in the
    procedure division. Copybook structures are shared
    types used by multiple programs — they must be complete.
  • Map EVERY PIC clause to the correct target type (PIC X→String, PIC S9V9→BigDecimal/decimal,
    PIC 9 COMP-3→BigDecimal/decimal, PIC 9 COMP→int/long). Do NOT simplify to fewer fields.
  • Preserve the original COBOL field name as the Java/C# field name (camelCase).
  • If a group has >50 fields, still generate ALL of them — completeness is more
    important than brevity.

CALL TARGET → SERVICE INJECTION RULES:
  • For EVERY CALL target in the structural context, generate:
    - A service interface (e.g. IDateService / IAccountService)
    - An @Inject/@Autowired field in the main service class
    - A method call at the point where the COBOL CALL appears
  • Java: use @Inject (CDI) for the interface field
  • C#: use constructor injection for the interface
  • Do NOT inline the called program's logic — it will be converted separately.

## SECTION: JavaTargetPolicy

DATA STRUCTURE → DTO RULES:
  • For EVERY 01-level data group below, generate a COMPLETE DTO class with ALL fields.
  • Map PIC X→String, PIC S9V9→BigDecimal, PIC 9 COMP-3→BigDecimal, PIC 9 COMP→int/long.
  • Preserve original COBOL field names (camelCase). Do NOT simplify to fewer fields.
  • If a group has >50 fields, still generate ALL of them.

CALL TARGET → SERVICE INJECTION RULES:
  • For EVERY CALL target below: generate an interface + @Inject field + method call.
  • Do NOT inline the called program's logic.

## SECTION: CSharpTargetPolicy

DATA STRUCTURE → DTO RULES:
  • For EVERY 01-level data group below, generate a COMPLETE DTO class with ALL fields.
  • Map PIC X→string, PIC S9V9→decimal, PIC 9 COMP-3→decimal, PIC 9 COMP→int/long.
  • Preserve original COBOL field names (PascalCase). Do NOT simplify to fewer fields.
  • If a group has >50 fields, still generate ALL of them.

CALL TARGET → SERVICE INJECTION RULES:
  • For EVERY CALL target below: generate an interface + constructor-injected field + method call.
  • Do NOT inline the called program's logic.

## SECTION: WarningsHeader

WARNINGS (preserved from REKT extraction — surface in the generated code as TODOs where relevant):

## SECTION: PreprocessHeader

PREPROCESSOR TRANSFORMS APPLIED (the source you see has been rewritten — preserve original semantics):

## SECTION: JavaDataGroupsHeader

DATA GROUPS (01-level — one DTO/record class per entry):

## SECTION: CSharpDataGroupsHeader

DATA GROUPS (01-level — one DTO/record class per entry, PascalCase property names):

## SECTION: JavaDbTablesHeader

IO — DB TABLES (each becomes a Panache entity / repository method):

## SECTION: CSharpDbTablesHeader

IO — DB TABLES (each becomes an EF Core entity / repository method):

## SECTION: FilesHeader

IO — FILES (heuristic-extracted from PROCEDURE DIVISION; treat as file-IO ports):

## SECTION: JavaCallTargetsHeader

CALL TARGETS (each becomes an @Inject service interface):

## SECTION: CSharpCallTargetsHeader

CALL TARGETS (each becomes a service interface + constructor-injected field):

## SECTION: CalledByHeader

CALLED BY (informational — these programs depend on this one):

## SECTION: JavaExternalEffectsHeader

EXTERNAL EFFECTS (use to choose Quarkus extensions / annotations):

## SECTION: CSharpExternalEffectsHeader

EXTERNAL EFFECTS (use to choose .NET libraries / DI registrations):

## SECTION: SharedTypes

---
SHARED COPYBOOK TYPES:

These copybooks are referenced by more than one program in this batch.
Do not emit a top-level type for them because another converted program may
emit the same name. If this program needs the copybook's value-object, define
it as a nested type inside this program's generated class. This keeps the
type local and prevents duplicate top-level declarations.

{{SharedTypes}}

Use the expected type name below for the nested type.

## SECTION: DataStructureGuidance

  Each top-level group (01-level) should become a separate class/record.
  Use the field names and PIC clauses below to derive the correct types.
  If a group comes from a COPY (copybook), name the class after the copybook.

## SECTION: DataStructureHeader

DATA STRUCTURE ({{Count}} groups — generate a DTO/record class for each):

## SECTION: TruncatedDataStructure

    … {{Count}} fields total — generate ALL in the DTO using the COBOL source for complete field list
