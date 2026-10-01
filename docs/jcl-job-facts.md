**Last updated**: 2026-10-01

# JCL job facts

The JCL parser reads every job in `source/` and records what it runs, which datasets it reads and writes, and the conditions each step runs under. It is deterministic: no model is involved, and anything it cannot resolve from the source is reported rather than guessed.

The portal's Service Chain view reads jobs through it, and later conversion stages use the facts it writes.

## Running it

`./doctor.sh rekt-parse` runs it after the COBOL parse. It can also be run on its own:

```bash
dotnet run --project CobolToQuarkusMigration.csproj -- jcl-facts source --output-dir output/rekt
```

A failure is reported as a warning and never stops the parse.

## What it reads

| Files | Extension | Treated as |
|---|---|---|
| Jobs | `.jcl` | A job to parse |
| Procedures and INCLUDE members | `.jcl`, `.proc`, `.prc`, `.inc` | Members found by file name |

On a mainframe a catalogued procedure is found through the `JCLLIB` libraries. An exported estate only has file names, so a member is matched by its file stem. The `JCLLIB` library is used only to choose between two files with the same stem.

The parser handles:

- **Statements**
  - Continuations, including a quoted value continued in column 16.
  - Columns 73–80 (sequence numbers) are dropped.
  - Comments are skipped.
- **In-stream data**
  - `DD *` and `DD DATA`, including `DLM=`.
  - Data with no DD before it becomes an implicit `SYSIN`.
- **Symbols**
  - `SET` values, `PROC` defaults and `EXEC` overrides, in that order of increasing precedence.
  - `&&temp` names are left as temporary datasets.
- **Procedures**
  - In-stream (`PROC` … `PEND`) and catalogued procedures, nested.
  - DD overrides, qualified (`STEP.DD`) and unqualified.
  - `PARM` and `COND` overrides.
  - An unqualified `PARM` applies to the first step and nullifies it on the others.
- **Other statements**
  - `INCLUDE` members.
  - `IF`/`THEN`/`ELSE`/`ENDIF`: every step records the conditions it runs under.
  - Backward references: `DSN=*.STEP.DD`.
- **Control statements**
  - `RUN PROGRAM(...) PLAN(...)` in `SYSTSIN`, with the Db2 subsystem from `DSN SYSTEM(...)`.
  - IDCAMS `DELETE`, `REPRO` and `DEFINE`, recorded as dataset effects.
  - Sort control statements.

When a procedure is missing from the source, the step is kept as an unresolved procedure. Its in-stream `SYSTSIN` is still read, so the program the step runs under Db2 is still found.

## What it writes

Output goes under `output/rekt/`:

- **`<source-relative path>.job.json`**
  - One file per job, with its steps, DDs, conditions, Db2 runs, dataset effects, symbols and diagnostics.
  - `programs`: the estate programs the job runs.
  - `upstream` and `downstream`: the jobs it depends on through data.
- **`jcl-lineage.json`**
  - Every dataset with each job and step that creates, reads, appends to or deletes it.
  - The job dependencies inferred from those uses.
  - `externalInputs`: datasets that are read but never written by any job.

### Dataset lineage

| DISP status | Recorded as |
|---|---|
| `NEW`, or omitted | Create |
| `MOD` | Append |
| `SHR` | Read |
| `OLD` | Exclusive (the direction is unknown) |

A normal disposition of `DELETE` also records a Delete.

A job depends on another when it reads (Read or Exclusive) a dataset the other creates or appends to. Temporary datasets are local to their job. `STEPLIB`, `JOBLIB`, `STEPCAT` and `JOBCAT` are program and catalog libraries rather than data, so they are left out of lineage.

The inferred order comes from data only. A scheduler may enforce more, and it can contain cycles, because a generation data group is written by one run and read by the next.

## In the graph and the AST Explorer

`./doctor.sh rekt-ingest` loads each job into the REKT Neo4j as `JclNode` nodes. COBOL `ASTNode` data is untouched, so the COBOL views and counts are unaffected. The AST Explorer lists each job as `JCL · <job>` beside the programs and shows it as a graph. Clicking a node shows its JCL.

| Node type | What it is |
|---|---|
| `JCL_JOB` | The job |
| `JCL_STEP`, `JCL_UTILITY_STEP`, `JCL_TSO_STEP` | A step that runs a program, a utility, or programs under the TSO monitor |
| `JCL_UNRESOLVED_STEP` | A step calling a procedure that is not in the source |
| `JCL_PROGRAM`, `JCL_UTILITY` | What a step runs |
| `JCL_DATASET`, `JCL_TEMP_DATASET` | A dataset, or a temporary (`&&`) dataset local to the job |
| `JCL_JOB_REF` | Another job this one feeds or is fed by |

| Edge | From → to |
|---|---|
| `CONTAINS` | Job → step |
| `FOLLOWED_BY` | Step → next step |
| `RUNS` | Step → program or utility |
| `READS`, `CREATES`, `APPENDS`, `OPENS_EXCLUSIVE`, `DELETES` | Step → dataset, from the lineage above |
| `FEEDS` | Job → job, labelled with the datasets that connect them |

A fact file written by a newer schema than the populator knows is skipped with a warning rather than loaded wrongly.

## Diagnostics

| Code | Meaning |
|---|---|
| `NO_JOB_CARD` | No `JOB` statement; the file name is used as the job name |
| `INCLUDE_NOT_FOUND`, `PROC_NOT_FOUND` | The member is not in the source |
| `INCLUDE_RECURSION`, `PROC_RECURSION` | A member includes or calls itself |
| `MEMBER_AMBIGUOUS` | Several files have the member's name; the message names the one used |
| `UNBALANCED_IF` | An `IF` without `ENDIF`, or an `ELSE` or `ENDIF` without `IF` |
| `OVERRIDE_TARGET_MISSING` | An override names a step the procedure does not have |
| `BACKREF_UNRESOLVED` | `*.STEP.DD` names a DD that is not in the job |

## Limits

- **Scheduler variables** such as `&OCDATE` are recorded in `unresolvedSymbols` when used in JCL, and in `inStreamSymbols` when used in in-stream data. They are not substituted.
- **Enclosing apostrophes** are stripped from symbol values. This is a simplification.
- **Utilities** are recorded with their control statements. Only IDCAMS commands become dataset effects.
