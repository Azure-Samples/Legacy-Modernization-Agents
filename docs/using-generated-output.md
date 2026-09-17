# Working with generated output

The conversion agents write their output to a dated run folder under the target language:

```
output/csharp/20260917-102951/
output/java/20260917-140312/
```

Each run is kept rather than overwritten, so two runs can be compared. Readers treat
`output/<language>` as the stable root and resolve the newest run beneath it, so nothing needs to
be told the timestamp. Output produced before runs were dated sits flat in the language folder and
is still found.

`source/` and `output/` are gitignored. Nothing in this repository contains customer COBOL or the
code generated from it.

## Four different questions

These are routinely conflated, and only the first is currently answered by an automated suite.

| Question | How it is answered today |
|---|---|
| **1. Does the conversion tool work?** | `dotnet test` on the toolchain and portal suites |
| **2. Does the generated code compile?** | Not automated. Measured manually below — it does not |
| **3. Does the generated application run?** | Blocked by 2 |
| **4. Is it equivalent to the COBOL?** | **Unverified.** No compatible original runtime and no captured legacy inputs/outputs are available in this repository |

On the fourth: equivalence needs either a runnable original or trustworthy captured inputs, outputs
and starting state. Expectations inferred by a model from the same source the conversion read are
not independent evidence, and must not be presented as verification.

## Current state of generated C#

Measured on a real conversion of five COBOL programs and their 65 copybooks —
70 generated `.cs` files, run `20260917-102951`.

**It does not compile.** But the reason is not what the raw error count suggests, and the
distinction decides what is worth fixing.

Compiled with a hand-written project file the run produces **10 syntax errors**. Patching those two
files raises it to **1140**, because the compiler can then reach type resolution. Almost all of
that is one cause:

| Cause | Errors | Nature |
|---|---|---|
| Missing `using` directives | **1028** | Generator emits a fixed `using` block |
| Duplicate declarations | 88 | `CS0101` + `CS0111` |
| Two namespaces in one file | 16 | `CS8954` + `CS0234`, 4 files |
| Types referenced but never declared | 6 | `CS0246` |
| Interface not implemented | 2 | `CS0535` |

### Build scaffolding is now written with the output

Each C# run folder receives a `GlobalUsings.g.cs` and a `.csproj`, generated from the code itself:
the sources are scanned for the framework types they actually use, and only the matching namespaces
and package references are declared. Code that uses none of them gets no dependencies, because an
unused package reference is a false claim about what the converted code needs.

That takes the measured run from **1140 errors to 112** with no hand-written project file and no
change to converted logic. The remainder is listed below.

### The dominant cause was boilerplate, not business logic

Generated files open with a fixed block:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;
```

They then use `[Column]`, `[Key]`, `[Table]`, `[MaxLength]`, `DbSet<>`, `DbContext`,
`ILogger<>`, `IConfiguration` and `IServiceCollection` — none of which that block covers.

Adding six `global using` directives and the corresponding package references takes the run from
**1140 errors to 112**. Nothing about the converted logic changes.

```
System.ComponentModel.DataAnnotations          Microsoft.Extensions.Logging
System.ComponentModel.DataAnnotations.Schema   Microsoft.Extensions.Configuration
Microsoft.EntityFrameworkCore                  Microsoft.Extensions.DependencyInjection
```

### What remains after that

- **88 duplicate declarations** — the same problem `CopybookOwnershipRegistry` and
  `CallTargetRegistry` were written to remove, by naming a single declaring file for each copybook
  type and call-target interface. Those are in this branch but **have not been re-measured**
  against a fresh conversion.
- **16 from four files emitting a second file-scoped namespace** part-way through, producing
  `Modernized.Banking.Shared.Modernized.Banking`.
- **8** genuinely individual: two undeclared types and one `IComparer` implementation whose
  signature does not match the interface.

So of 1140 errors, roughly 1132 are systematic and mechanically addressable. Two look like real
defects in converted logic.

### Two generation defects worth naming

```csharp
// Db2diagi.cs:210 — HTML entities leaked into emitted source
set { if (value &amp;&amp; !DerVarNogetAndet) WsVarDerAndet = 1; }

// Reni303.cs:72 — comma where the accessor needs a semicolon
get => FromCode(_prisMinSatsKd),
```

## Diagnostics this repository does provide

Two checks run against generated output and report rather than rewrite:

- **`NamespaceCompliance`** — every generated file must declare a namespace under the configured
  root. Verified across 117 files of an earlier run: no deviations.
- **`GeneratedTypeCollisions`** — type names declared by more than one file in the same namespace,
  reported by namespace, type and file. These correspond to the `CS0101` errors above.

Neither rewrites generated code. Deciding which of several conflicting declarations is correct is
not a decision to take from a post-pass.

## What would make compilation possible

In descending order of measured effect:

1. ~~Emit the `using` directives the generated code actually uses~~ — **done**, worth 1028 of
   1140 errors.
2. ~~Emit a project file and dependency manifest with the run~~ — **done**, written from the
   code's own references.
3. **Re-measure duplicate declarations** after the copybook-ownership and call-target work in this
   branch. Expected to address the 88, unproven until a conversion is run.
4. **Stop emitting a second namespace declaration** part-way through a file.
5. **Fix the entity-escaping defect** in the generation path.

Only after 1 and 2 does "does the generated code compile" become a question CI can answer, and
only then does running it mean anything.
