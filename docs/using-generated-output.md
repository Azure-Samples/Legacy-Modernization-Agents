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

**It does not compile.** In order of what blocks first:

| Blocker | Evidence |
|---|---|
| No project file | 0 `.csproj` or `.sln` in the run folder |
| No entry point | 0 files declaring `Main` |
| No dependency declarations | nothing states the runtime packages the generated code refers to |
| Syntax errors in generated code | 2 files, below |
| Duplicate types and members | 62 `CS0101` + 26 `CS0111` |
| Unresolved types | 1022 `CS0246`, 18 `CS0234` |

Compiling the run with a hand-written project file — the only way to get a figure at all —
produced **10 syntax errors**. Patching just those two files raised **1140 errors**, because the
compiler could then reach type resolution.

The two syntax defects are worth naming because they are generation bugs rather than missing
scaffolding:

```csharp
// Db2diagi.cs:210 — HTML entities leaked into the emitted source
set { if (value &amp;&amp; !DerVarNogetAndet) WsVarDerAndet = 1; }

// Reni303.cs:72 — comma where the accessor needs a semicolon
get => FromCode(_prisMinSatsKd),
```

The bulk of `CS0246` is the generated code referring to framework and runtime types that nothing
declares a dependency on. That is scaffolding, not a conversion defect — but it does mean a run
folder is not a buildable project as it stands.

## Diagnostics this repository does provide

Two checks run against generated output and report rather than rewrite:

- **`NamespaceCompliance`** — every generated file must declare a namespace under the configured
  root. Verified across 117 files of an earlier run: no deviations.
- **`GeneratedTypeCollisions`** — type names declared by more than one file in the same namespace,
  reported by namespace, type and file. These correspond to the `CS0101` errors above.

Neither rewrites generated code. Deciding which of several conflicting declarations is correct is
not a decision to take from a post-pass.

## What would make compilation possible

Not attempted in this change, and listed so the gap is explicit rather than implied:

1. Emit a project file and dependency manifest alongside the generated sources.
2. Resolve duplicate declarations at their source — see
   `CopybookOwnershipRegistry` and `CallTargetRegistry`, which assign a single declaring file for
   copybook types and call-target interfaces.
3. Fix the entity-escaping defect in the generation path.
4. Provide the runtime shims the generated code assumes.

Only after 1–3 does question 2 become answerable, and only then does 3 become meaningful.
