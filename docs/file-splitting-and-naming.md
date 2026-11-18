# File splitting and naming

**Last updated**: 2026-11-18

How generated code is split into files, how COBOL names are converted, and the Java framework and partial-conversion options that affect generated output.

## Configuration

File splitting is controlled in `Config/appsettings.json`:

```json
{
  "AssemblySettings": {
    "SplitStrategy": "ClassPerFile",
    "Java": {
      "PackagePrefix": "com.example.generated",
      "ServiceSuffix": "Service"
    },
    "CSharp": {
      "NamespacePrefix": "Generated",
      "ServiceSuffix": "Service"
    }
  }
}
```

## Split Strategies

| Strategy | Output |
|----------|--------|
| `SingleFile` | One large file with all classes |
| `ClassPerFile` | **Default** - One file per class (recommended) |
| `FilePerChunk` | One file per processing chunk |
| `LayeredArchitecture` | Organized into Services/, Repositories/, Models/ |

## Implementation Location

The split logic is in `Models/AssemblySettings.cs`:

```csharp
public enum FileSplitStrategy
{
    SingleFile,           // All code in one file
    ClassPerFile,         // One file per class (DEFAULT)
    FilePerChunk,         // Preserves chunk boundaries
    LayeredArchitecture   // Service/Repository/Model folders
}
```

## Naming Conversion

Java conversion defaults to Quarkus. Set `JAVA_FRAMEWORK=Spring` (or
`ConversionSettings.JavaFramework` to `Spring`) to use the existing whole-file and
chunked Java agents with Spring Boot instructions, constructor injection and
source-backed SQL mappings. Generated Spring projects receive a Boot entry point
and Maven dependencies matching their imports; selecting Spring does not imply
CICS/IMS runtime equivalence or resolve missing inputs.

Program-level translation units are not automatically microservices. The Spring
prompt follows a supplied domain map for higher-level capability classes,
distinguishes local injection from real cross-service clients, and does not
claim that transactions span HTTP. Domain aggregation, separate deployments and
browser/API assembly still require an explicit application-assembly step.

Explicit `--dependency-mode partial` (or `dependencyMode: "partial"` in the
managed launch API) selects a separate source-isolated Spring conversion path.
It translates restricted routines with locally established field declarations,
plus bounded straight-line prefixes before unsupported effects. Standard COPY
inputs are used only when their names resolve uniquely; the supported simple
pseudo-text replacement does not change quoted literals. Vendor prefix-copy
directives, missing fields, aliases/tables requiring unresolved layouts and
unsupported control/runtime effects remain excluded with specific reasons.
This mode does not relax strict whole-program dependency gates.
`partial-tree-plan.json` records original/COPY hashes, source ranges, selected
routines/fragments, omissions and actual generation outcomes. Maven compilation
is measured separately; no whole-program entry point, unavailable callee body
or behavioral-equivalence claim is generated. AI Loop records actual model
requests, per-program stages, process logs and the partial outcome.

For an isolated native scan, set `REKT_FACTS_FOLDER` to its exact artifact directory
and select the matching original source root. Dependency gates, discovery,
structural context and verified COPY context use that directory rather than stale
`output/rekt` artifacts; source and definition hash checks remain mandatory.
`POST /api/runs/start` accepts optional `javaFramework: "Spring"` and
`rektFactsFolder: "output/rekt-scans/<scan>/artifacts"` for a managed attempt.
Portal artifact paths must be existing, unlinked repository-relative directories.
Managed attempts retain their console logs and actual terminal status in AI Loop,
including attempts blocked before any model call.
An immutable portal deployment can set `MIGRATION_CLI_ASSEMBLY` to an existing
absolute CLI `.dll` path, so managed launches use that tested build without
rebuilding or overwriting binaries loaded by another process.

Naming strategies are configured in `ConversionSettings`:

```json
{
  "ConversionSettings": {
    "NamingStrategy": "Hybrid",
    "PreserveLegacyNamesAsComments": true
  }
}
```

| Strategy | Input | Output |
|----------|-------|--------|
| `Hybrid` | `CALCULATE-TOTAL` | Business-meaningful name |
| `PascalCase` | `CALCULATE-TOTAL` | `CalculateTotal` |
| `camelCase` | `CALCULATE-TOTAL` | `calculateTotal` |
| `Preserve` | `CALCULATE-TOTAL` | `CALCULATE_TOTAL` |
