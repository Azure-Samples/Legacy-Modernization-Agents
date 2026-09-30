**Last updated**: 2026-09-30

# Jobs generated from JCL

After a conversion, every JCL job in the source becomes a job in the target language that runs the converted programs in the order and under the conditions the JCL gives. For C# it is a .NET job run by a small runner. For Java it is a Spring Batch job. The jobs are generated deterministically from the [JCL parser](jcl-job-facts.md)'s output; no model writes them.

## What is generated

| Target | Where | Files |
|---|---|---|
| C# | `output/csharp/Jobs/` | `JclJobRuntime.g.cs` (the runner and its contracts), `<Job>Job.g.cs` per job, `JclJobs.g.cs` (every job, by name) |
| Java | `output/java/<root>/jobs/` | the runtime classes, `<Job>Job.java` and `<Job>JobConfiguration.java` per job, `JclJobsConfiguration.java` |

The namespace is `{root}.Jobs` (C#) or `{root}.jobs` (Java), under the same root as the converted programs (`TARGET_ROOT_NAMESPACE`). The files are rewritten on every run. Files an earlier run generated for a job the source no longer has are removed; other files in the folder are left alone.

`jobs-manifest.json`, beside the output, lists for each job:

- the programs it runs;
- which of them the converted code already implements through the batch-program contract, and which it does not yet;
- the steps that cannot run (a procedure that is not in the source, or a condition that could not be read);
- the parser's diagnostics.

In C#, the jobs are part of the generated project and go through the compile gate with the programs. The compile gate does not repair generated `.g.cs` files, because they are rewritten on the next scaffold.

## The batch-program contract

A program that a JCL step runs, either by `EXEC PGM=` or by `RUN PROGRAM` under the TSO monitor, is converted with an extra instruction to implement this interface:

```csharp
public interface IBatchProgram
{
    string ProgramId { get; }
    Task<int> RunAsync(BatchStepContext context, CancellationToken cancellationToken = default);
}
```

```java
public interface BatchProgram {
    String programId();
    int run(BatchStepContext context) throws Exception;
}
```

The instruction includes the jobs and steps that run the program and the DDs each step gives it. The program:

- returns its `RETURN-CODE`;
- opens files by DD name (`OpenRead`/`OpenWrite`, `openRead`/`openWrite`), not by path;
- reads `PARM` from the context;
- throws `BatchAbendException("U0100", …)` to abend with a code.

The interface is generated with the jobs. The scaffold removes any copy a model declares, and adds the jobs namespace to the global usings.

## Running a job

C#:

```csharp
var runner = new JclJobRunner(programs, new DirectoryDatasetCatalog(datasetsRoot));
var result = await runner.RunAsync(JclJobs.Find("PAYJOB")!);
// result.Steps: per step, whether it ran, its return code, and its abend code.
```

`programs` is every `IBatchProgram` in the application, for example from the DI container.

Java: add the generated package to the Spring context. `JclJobsConfiguration` builds the runner, and each `<Job>JobConfiguration` contributes a `Job` bean for a `JobLauncher`. It needs the Spring Batch 5 infrastructure beans (`JobRepository`, `PlatformTransactionManager`) and these properties:

| Property | Meaning |
|---|---|
| `jcl.datasets.root` | Directory the datasets live under. Required unless a `DatasetCatalog` bean is supplied. |
| `jcl.programs.package` | Package scanned for the converted programs. Defaults to the conversion root. |
| `jcl.programs.scan` | `false` when the programs are already Spring beans. |

The converted Java programs are CDI beans (Quarkus). The generated configuration registers the CDI-scoped classes in that package with Spring as well, so the runner finds the batch programs and the beans they inject.

Each JCL step is one Spring Batch step. A bypassed step completes with exit status `BYPASSED`. An abended step completes with exit status `ABENDED`, and so does the job. Otherwise the job completes with `MAXCC=n`, the highest step return code, in its exit description.

## How a step is decided

The same rules apply in both languages. The runner applies them, not Spring Batch.

- **`IF`/`THEN`/`ELSE`**: a step runs only if the enclosing conditions hold. `ELSE` is the negated condition. `AND` and `OR` have equal precedence and are read left to right. `stepname.RC` for a step that calls a procedure is the highest return code among the procedure's steps.
- **Tests on a step that did not run**: an `RC` or `ABENDCC` test on a step that did not run, or that abended, is false.
- **After an abend**: steps are bypassed, except:
  - a step with `COND=EVEN` runs;
  - a step with `COND=ONLY` runs only after an abend;
  - a step under `IF` runs only if its condition tests `ABEND` or `ABENDCC`. This rule is our reading of the z/OS documentation, not a verified quote. Check it against your system if a job relies on it.
- **Step `COND`**: the step is bypassed if any test `(code,op[,step])` is true against a step that ran normally.
- **`JOB COND`**: after each step that completes normally, the job ends if any of its tests is true. The remaining steps are bypassed.
- **Program not found**: if no batch program has the id, the step abends with `S806`.

A step that cannot be run faithfully stops the job with `JclStepNotRunnableException` instead of running wrongly. The exception is not an abend and is not caught by `COND=EVEN`. This covers:

- a procedure missing from the source;
- a condition that could not be read;
- a utility with no equivalent.

## Datasets

`DirectoryDatasetCatalog` maps datasets to files under one root:

- `A.B.C` is `<root>/A.B.C`, and a member is a file inside it.
- A generation data group is a directory of `G0001V00`, `G0002V00` and so on. A relative generation such as `(+1)` or `(0)` is fixed for the whole run, as on the mainframe.
- Temporary datasets (`&&T`) live under `<root>/.temp/<run>` and are removed when the job ends.
- SYSOUT goes to `<root>/.sysout/<job>/<run>/`.
- A `DISP` of `DELETE` removes the dataset after the step.
- A new, kept dataset exists after the step even if the program never wrote it.

To map datasets somewhere else, such as blob storage or a database, implement `IDatasetCatalog` (C#) or `DatasetCatalog` (Java).

## Utilities

`DefaultJobUtilities` runs:

- `IEFBR14`;
- the `IDCAMS` commands `DELETE`, `REPRO`, `DEFINE` and `SET MAXCC/LASTCC`. `DELETE` of a missing dataset gives return code 8, as it does on z/OS.

Any other utility, such as a sort, a Db2 unload or a copy program, stops the job with a message naming it. To run one, subclass `DefaultJobUtilities` or implement `IJobUtilities`/`JobUtilities`. Its control statements are in `BatchStepContext.ControlStatements`.

## Configuration

| Setting | Environment | Default | Meaning |
|---|---|---|---|
| `JclJobs.Enabled` | `JCL_JOBS_ENABLED` | `true` | Generate jobs, and tell the programs they run about the contract |
| `JclJobs.SourceFolder` | `JCL_SOURCE_FOLDER` | the COBOL source folder | Where the JCL, procedures and `INCLUDE` members are |

To generate jobs without converting, for example over an existing output folder:

```bash
dotnet run -- jcl-jobs source --language CSharp --output-dir output/csharp
dotnet run -- jcl-jobs source --language Java --output-dir output/java
```
