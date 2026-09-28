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

At the time of the measurement above:

- **88 duplicate declarations**, the problem `CopybookOwnershipRegistry` and `CallTargetRegistry`
  address by naming a single declaring file for each copybook type and call-target interface.
- **16 from four files emitting a second file-scoped namespace** part-way through, producing
  `Modernized.Banking.Shared.Modernized.Banking`.
- **8** genuinely individual: two undeclared types and one `IComparer` implementation whose
  signature does not match the interface.

### Re-measured after copybook ownership

The same five programs were converted again with the ownership and call-target registries in
place (run `20260917-185339`). Both runs below are compiled with the same generated scaffolding,
with the `&amp;` defect patched so the compiler reaches type resolution, and errors are counted
once per location. These numbers are therefore not comparable with the 1140 / 112 above.

| | Run `102951` | Run `185339` | `185339` normalized |
|---|---|---|---|
| Unique errors | 56 | 107 | **71** |
| Duplicate declarations (`CS0101` + `CS0111`) | 44 | 26 | 26 |
| Type or namespace not found (`CS0246` + `CS0234`) | 7 | 50 | 26 |
| Files with more than one namespace | 4 | 14 | 0 |

Ownership worked where it was aimed: duplicate declarations fell from 44 to 26. It also made the
namespace defect much more common. A copybook that contains a `CALL` is treated as a caller, so
its file now declares its record type in `.Shared` and its service code in `.Bd`, and the model
writes that as two file-scoped namespaces (`CS8954`) or a file-scoped one followed by a block
(`CS8955`). The compiler nests the second inside the first, and the phantom
`Modernized.Banking.Bd.Modernized.Banking.Shared` it creates captures every
`using Modernized.Banking.Shared;` in `.Bd`. Types that do exist then stop resolving estate-wide.

`FileScopedNamespaceNormalizer` now runs as part of scaffolding and rewrites such files into block
namespaces. It changes syntax only: the same types land in the same namespaces, and files with a
single namespace are never touched. The chunked migration path previously wrote no scaffolding at
all; it now does. Every remaining `CS0246` in the normalized run names a type that is declared
nowhere in the output, such as `IBdsparmService`, `Bdsparmx` and `BdsDa01Entity`: missing
copybooks or callees that were not part of the conversion.

The 71 break down as 26 not found, 26 duplicate declarations, 8 unimplemented interface members
(`CS0535`), 6 invalid partial declarations (`CS8863`) and 5 duplicate attributes (`CS0579`).
One generated file also escapes the configured root:
`CobolMigration/Fallback/CeeigzctFallback.cs` declares `namespace CobolMigration.Fallback`.

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
3. ~~Re-measure duplicate declarations~~ after copybook ownership: **done**, 44 → 26 on a like-for-like
   count. The remaining 26 still need a fix at generation time.
4. ~~Stop a second namespace declaration from breaking the build~~: **done** by normalizing to block
   namespaces, 107 → 71.
5. **Fix the entity-escaping defect** in the generation path. It recurs across runs
   (`Db2diagi.cs`, `Rgni656.cs`) and hides every type-level error until patched.
6. **Supply or stub the missing callees and copybooks** behind the 26 undeclared types.

Only after 1 and 2 does "does the generated code compile" become a question CI can answer, and
only then does running it mean anything.
