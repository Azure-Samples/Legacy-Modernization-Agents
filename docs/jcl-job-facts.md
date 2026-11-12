**Last updated**: 2026-10-22

# JCL job facts

The JCL parser reads every job in `source/` and records what it runs, which datasets it reads and writes, and the conditions each step runs under. It is deterministic: no model is involved, and anything it cannot resolve from the source is reported rather than guessed.

The portal's Service Chain view reads jobs through it, and conversion turns each job into runnable code over the converted programs: see [Jobs generated from JCL](jcl-jobs.md).

## Running it

`./doctor.sh rekt-full` scans COBOL and JCL, then ingests their facts into Neo4j.
JCL-only folders are supported; no COBOL program is required. It does not generate
executable job scaffolding (`./doctor.sh jcl` remains the separate command for that).

```bash
bash ./doctor.sh rekt-full
```

JCL is discovered recursively with a case-insensitive `.jcl` extension under
`source/`, or under `JCL_SOURCE_FOLDER` when configured:

```bash
JCL_SOURCE_FOLDER="C:/path/to/jobs" bash ./doctor.sh rekt-full
```

Nonempty shell settings take precedence over `Config/ai-config.local.env`; with no
nonempty setting in either place the default is `source/`. Relative paths are relative to the checkout,
not the caller's current directory. Absolute POSIX paths, Windows drive paths
(`C:/jobs`, `C:\jobs`), UNC paths and Git Bash paths (`/c/jobs`) are supported;
quote paths containing spaces. Git Bash uses `cygpath` for drive and UNC paths.
An explicitly configured missing directory or empty JCL library fails before parsing.
These options do not change the REKT COBOL-root restriction:
`COBOL_SOURCE_FOLDER` must remain `source`, even for a JCL-only scan.

`./doctor.sh rekt-parse` performs the same fact extraction without ingestion.
JCL-only parsing needs the .NET CLI, but not REKT or Neo4j containers; ingestion
starts only Neo4j. Mixed/COBOL parsing still needs the REKT parser.
The extractor can also be run on its own:

```bash
dotnet run --project CobolToQuarkusMigration.csproj -- jcl-facts source --output-dir output/rekt
```

A failed JCL extraction stops the pipeline before ingestion; an explicitly configured
JCL folder with no jobs is reported as an error. Missing procedures or INCLUDE
definitions remain diagnostics rather than invented content.
An extension alone does not establish a job: a library containing only procedures,
INCLUDE members or unparseable text fails with “No JCL jobs were parsed”.
A COBOL program filter is refused for JCL-only input rather than silently ignored.

### Refresh and failure handling

A successful extraction replaces current JCL facts and removes only stale
`*.job.json` artifacts for jobs no longer present, including when the final job is
deleted from a mixed estate. A COBOL-only scan passes `--allow-empty` to refresh the
empty JCL subset. Direct `jcl-facts` calls reject zero jobs unless that option is
explicitly supplied; configured JCL libraries must contain a job.

JCL evidence is retained across failed extraction instead of broadly deleting
`output/rekt/`. `.jcl-scan-failed` marks incomplete refreshes; both `rekt-ingest` and
the Python ingestion consumer refuse them before writing graph data. Fix the input
and rerun `rekt-parse`/`rekt-full` (or `jcl-facts`) to refresh successfully. Do not
remove the marker to present old evidence as current. Other retained artifacts are
not removed by the JCL-only path; COBOL full scans retain their existing COBOL cleanup.

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
  - `sourceRoot`: the absolute JCL library root used for source retrieval during ingestion.
    Source-relative `job.file` identities are unchanged.
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
For external libraries the populator reads source blocks from the fact's `sourceRoot`,
not an unrelated same-relative-name file under `source/`. Older facts without this
field retain the `--source-dir` fallback; regenerate them when moving JCL externally.

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
