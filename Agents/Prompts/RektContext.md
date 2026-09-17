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

## SECTION: SyntheticLayoutPolicy

SYNTHETIC LAYOUTS — THESE COPYBOOKS WERE NOT FOUND IN THE SOURCE DROP:
{{StubCopybooks}}
  The parser needed a definition to resolve the COPY directive, so a placeholder holding a
  single filler field was generated. That placeholder is a tooling artefact. It is NOT the
  customer's layout, and the real field names, types, sizes and REDEFINES are unknown here.

  For the copybooks named above, and ONLY for those, the completeness rules above do not apply:
  • Do NOT invent fields, types or sizes to make the structure look plausible.
  • Do NOT carry the generated `-STUB` / `-VAL` filler into the output as if it were real.
  • Emit the type with no invented members, and mark it:
    `TODO: layout unknown — <COPYBOOK> was missing from the source drop; supply the copybook
     and regenerate before relying on this type.`
  • In comments and documentation, do not describe what these fields mean or what the
    structure is for. Nothing in the available evidence supports such a description.
  • Keep any reference to the structure compiling, but leave the shape unresolved.

  Every other data group in this context came from a real parse and must still be generated
  completely, exactly as the rules above require.


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

These copybooks are referenced by more than one program in this estate, so the types
built from them belong to no single program. They live in the shared namespace below
and are the same type for every program that uses them.

  shared namespace: {{SharedNamespace}}

{{SharedTypes}}

  • Do NOT define these types in this program's namespace, and do NOT nest them inside
    this program's class. Another converted program declares the same layout, and a
    record that exists once per caller is a record that can drift apart.
  • Reference them from the shared namespace instead (Java: import; C#: using).
  • Use exactly the expected type name given above so the reference resolves.

## SECTION: CallTargetContracts

---
CALL TARGET CONTRACTS (assigned — do not invent interface names or methods):

A called COBOL program has exactly one entry point, so its interface has exactly one method.
Where several programs call the same module, they must all use the same interface, declared
once. The assignment below is fixed; deviating from it produces a second declaration of the
same type and the service will not compile.

  shared namespace: {{SharedNamespace}}

DECLARE these interfaces in the shared namespace (you are responsible for them):
{{Declares}}

REFERENCE these and do NOT declare them (another program declares them):
{{References}}

  • Use exactly the interface and method names given. Do not rename them to suit this
    program's vocabulary — the other callers use the same names.
  • Inject the interface (Java: @Inject; C#: constructor injection) and call its method at
    the point where the COBOL CALL appears.
  • Do NOT inline the called program's logic.

## SECTION: NamespacePolicy

---
TARGET NAMESPACE (assigned — do not invent one):

  this program: {{ProgramNamespace}}
  shared types: {{SharedNamespace}}

  • Declare this program's types in the program namespace above, exactly as written.
  • Do NOT substitute a placeholder such as com.example, com.bank or CobolMigration.
  • Programs in the same service share a namespace deliberately: they are one deployable
    unit, and cross-service references go through the shared namespace only.


## SECTION: DataStructureGuidance

  Each top-level group (01-level) should become a separate class/record.
  Use the field names and PIC clauses below to derive the correct types.
  If a group comes from a COPY (copybook), name the class after the copybook.

## SECTION: DataStructureHeader

DATA STRUCTURE ({{Count}} groups — generate a DTO/record class for each):

## SECTION: TruncatedDataStructure

    … {{Count}} fields total — generate ALL in the DTO using the COBOL source for complete field list
