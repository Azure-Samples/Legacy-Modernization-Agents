**Last updated**: 2026-10-06

# Estate Mission Control and the AI Loop

Two portal tabs help you decide what to convert and then check how a conversion went:

- **Estate Mission Control** splits the estate into slices that can be converted on their own and orders them into waves. You can convert a slice directly from the tab. No model is involved: the graph is built from the source alone.
- **AI Loop** shows what the agents did during a run: model calls, retries and fallbacks, broken down by stage and agent. It also shows the quality gates the run's output had to pass.

## Estate graph

The estate graph is built from the files in `source/`, plus the JCL folder when one is configured. Every node and edge records the file and line it came from, so you can check each relationship against the source.

| Node | Taken from |
|------|------------|
| Program | `.cbl` / `.cob` files, and `CALL` targets that are missing from the source |
| Copybook | `COPY` statements; `SQLCA` and `SQLDA` are skipped (`SystemCopybooks`) |
| Job | JCL `EXEC PGM=` steps, including those inside procedures |
| Dataset | DD statements, linked to the steps that write or read them |
| Table | Embedded SQL, split into reads and writes; the `SYSIBM.` catalogue is skipped |
| Transaction / map | CICS `LINK`, `XCTL`, `LOAD` and `START`/`RETURN TRANSID`, BMS maps (`MapExtensions`), CSD definitions (`CicsDefinitionExtensions`) and CICS resource definitions in YAML (`- transaction:` with `program:`, `CicsYamlExtensions`) |
| CICS file | `- file:` entries in the CICS YAML, linked to their dataset (`dsname`) by a `backed-by` edge |
| Table (DDL) | `CREATE TABLE` in `.ddl` files (`DdlExtensions`): the table is then in the source rather than only named by the code |
| API | z/OS Connect style operations: `operations/<path>/<method>/operation.yaml` names an asset (`zasset:`), and `zosAssets/<asset>/zosAsset.yaml` names the program it invokes (`ApiOperationFileNames`, `ApiAssetFileNames`) |

Each program also carries counts taken from its text (lines, paragraphs and sections, `IF`/`WHEN`/`UNTIL` complexity, `PERFORM`, `GO TO`, `EXEC CICS`/`SQL`/`DLI`) and a one-line description from its header comments: a `FUNCTION:` line when there is one, otherwise the first prose that is not a licence or banner.

### Hubs, clusters and waves

1. **Hubs.** A hub is a node with many connections, such as a shared copybook or a utility program. A node counts as a hub when its degree is at least `HubMinDegree` and at least the `HubPercentile` of all node degrees. Edges that pass through a hub are weighted down by `HubEdgeFactor`, so a common utility does not merge unrelated programs into one cluster.
2. **Clusters.** The coupling between two programs is the sum of their shared edges, each weighted by `CouplingWeights`:
   - calls: 3
   - same job: 2
   - dataset flow: 2
   - writes to a shared table: 2
   - reads of a shared table: 0.5
   - a shared copybook: 0.25

   The programs are then grouped with Louvain community detection, using `LouvainResolution`. Ties are broken by id, so the same source always produces the same clusters.
3. **Carve score.** Each cluster gets a score from 0 to 100, which is a weighted mean (`CarveWeights`) of four measures:
   - **cohesion**: the share of the cluster's coupling that stays inside the cluster
   - **completeness**: the share of called programs, copybooks and maps that are present in the source
   - **independence**: the share of call targets that are inside the cluster
   - **size**: 1 up to `TargetSliceSize` programs, then shrinking in proportion

   The tab shows this breakdown, together with the missing members and the outside calls.
4. **Waves.** A cluster comes one wave after the last cluster it calls into. Clusters that call each other in a cycle share a wave, and the tab reports the cycle. Within a wave, clusters are ordered by carve score.

### From the command line

```bash
./doctor.sh estate          # summary: waves, clusters, hubs
./doctor.sh estate C01      # the slice of one cluster, ready for convert-only
dotnet run -- estate-graph source --output output/estate/estate-graph.json [--jcl-source DIR] [--slice C01]
```

Both commands write `output/estate/estate-graph.json`. The file is git-ignored with the rest of `output/`.

### In the portal

Open the **Estate Mission Control** tab. The graph panel expands while the tab is open.

- **KPIs.** Programs and lines, entry points (transactions, APIs, jobs), online and batch programs, data stores, how much REKT parsed, how many programs have an evaluated conversion, clusters and hubs, and what needs attention.
- **Explore.** The whole estate as one graph (Cytoscape with the fCoSE layout), grouped by business function, carve-out cluster or estate. Filter by node type, program kind (online, API, batch, subroutine), technology (CICS, DB2, VSAM, IMS, MQ, files), status or unreferenced and unreachable programs, and search by name. A program's ring shows its status: converted with parity of 90% or more, converted, parsed, not parsed yet, or referenced but not in the source. Select a node to see its metrics, description, conversion parity and every relationship with the file and line behind it.
- **Carve-out plan.** The wave plan. Wave 0 holds the hub programs as shared services. The other waves follow call order, so a cluster comes after every cluster it calls into, and within a wave the highest carve score goes first. Each card shows a tier: **low-risk** at or above `LowRiskCarveScore`, **moderate** at or above `ModerateCarveScore`, otherwise **core**.
- **Cluster card.** Select a card or a cluster title in the graph. It shows the rationale and score breakdown, the members, the entry points (transactions, APIs, jobs) that become the cluster's front door, the **owned data** that moves with it, the **shared data** another cluster also reads or writes (it needs a data-access API or sync; red ring in the graph), the inbound calls that become service APIs, the outbound dependencies and the references missing from the source. System routines (`SystemProgramPrefixes`) are shown as utilities and not counted as missing.
- **Stage slice.** Lists the programs to convert, what they call outside the cluster, what is missing, the jobs that run end to end, and the equivalent `./doctor.sh convert-only --program …` command. **Include what it calls** adds the programs the slice calls but does not contain. The program name is used, or the source-relative path when two programs share a name.
- **Send to AI loop.** Starts a convert-only run of the slice in the language you choose, with the provider, model and speed selected in Mission Control.

The portal rebuilds the graph when the source tree changes, or when you select **Rebuild graph**.

Each program gets a business function from `DomainRules`: the first rule whose `Name` regex matches the program name, else the first whose `Text` regex matches its description and the resources it touches, else `DomainFallback`. Transactions and APIs take the function of the programs they start; screens take that of the programs that send them. The defaults are generic; tune them per estate.

| Endpoint | Returns |
|----------|---------|
| `GET /api/estate/summary` | KPIs, waves, clusters, hubs |
| `GET /api/estate/graph?cluster=C01` | Nodes and edges, for the whole estate or one cluster |
| `GET /api/estate/node/{id}` | One node with its edges and evidence |
| `GET /api/estate/cluster/{id}/slice?includeNeeds=true` | The program selectors and the command |
| `POST /api/estate/slice/convert` | Starts a convert-only run of a slice |
| `GET /api/estate/mission` | Everything the tab draws: nodes with kind, technology, business function, metrics and status; edges with evidence; clusters with owned and shared data; KPIs |
| `POST /api/estate/rebuild` | Rescans the source and rebuilds the graph now |

## AI Loop

Each run writes its events to `output/.metrics/<runId>.jsonl`, through the same sink as the projection metrics. Events are written only while a run is active, so tests and other tools leave no trace.

| Event | Written when |
|-------|--------------|
| `run_started` | A run begins. Records the mode, the target language and the output folder relative to the repository |
| `stage` | The run moves to its next step |
| `llm_call` | A model call returns or fails. Records the agent, model, file, duration, success, prompt and response size, and tokens when the provider reports them |
| `llm_retry` | A call is retried: `transient_error`, `rate_limit`, `reasoning_exhaustion` or `output_truncation` |
| `llm_fallback` | An agent gives up and writes a stub: `retries_exhausted`, `non_retryable_error`, `content_filter`, `reasoning_exhaustion` or `output_truncation` |
| `run_finished` | The run ends as `completed`, `failed` or `no_files` |

The events carry no source code, prompts or responses. Error messages are cut to 200 characters.

The **AI Loop** tab lists runs that recorded these events, newest first. For each run it shows:

- **KPIs**: the status, duration, calls, failed calls, retries and fallbacks
- **Quality gates**, one card each:
  - **Model loop**: warns when there are more than `MaxFallbacks` fallbacks, or when the failed-call rate is above `MaxFailedCallRate`
  - **Compile gate**: read from `compile-status.json` (C# only, when `CompileGate` is enabled)
  - **Conversion parity**: read from `conversion-parity.json`. A run below the threshold warns or fails, following `OnLowScore`
  - **JCL jobs**: read from `jobs-manifest.json`; shows the steps that cannot run yet
- **Stages**: each stage with its duration
- **Agents**: per agent, the calls, failures, retries, fallbacks, average and p95 latency, and tokens. Token counts are estimated (shown with `~`) when the provider does not report them
- **Events**: the latest retries, fallbacks and failed calls

A gate whose artifact is missing shows as *not run*. The reader follows a run's output folder only when that folder is inside `output/`. While a run is in progress, the tab polls every `PollSeconds`, and only while the tab is visible. A run that has not finished and has written no events for `StaleRunMinutes` is shown as *interrupted* rather than running.

| Endpoint | Returns |
|----------|---------|
| `GET /api/ai-loop/runs` | The latest `MaxRuns` runs and the poll interval |
| `GET /api/ai-loop/{runId}` | The summary, gates, stages, agents and events (up to `MaxTimelineEvents`) |

## Configuration

Both features read their settings from `Config/appsettings.json`.

| Section | Key | Default | Effect |
|---------|-----|---------|--------|
| `EstateGraph` | `CouplingWeights` | see above | How strongly each kind of shared edge ties two programs together |
| | `HubMinDegree`, `HubPercentile`, `HubEdgeFactor` | 4, 0.9, 0.25 | Which nodes are hubs, and how much their edges count |
| | `LouvainResolution` | 1.0 | Higher values give smaller clusters |
| | `TargetSliceSize` | 15 | The size above which the size measure drops |
| | `CarveWeights` | 0.4 / 0.3 / 0.2 / 0.1 | The weights for cohesion, completeness, independence and size |
| | `IgnoredTablePrefixes`, `SystemCopybooks` | `SYSIBM.`; `SQLCA`, `SQLDA` | Tables and copybooks that create no coupling |
| | `MapExtensions`, `CicsDefinitionExtensions` | `.bms`; `.csd` | Where maps and transactions are read from |
| | `GeneratedCopybookDirectories` | `copy-generated` | Folders of generated stand-in copybooks. When a name also exists elsewhere, the other copybook is used. The REKT parse reads the same list from `REKT_GENERATED_COPYBOOK_DIRS` |
| | `CicsYamlExtensions`, `DdlExtensions` | `.yaml`, `.yml`; `.ddl` | Where CICS YAML definitions and DDL are read from |
| | `ApiOperationFileNames`, `ApiAssetFileNames` | `operation.yaml`; `zosAsset.yaml` | The API operation and asset files |
| | `SystemProgramPrefixes` | `CEE`, `DSN`, `ILBO`, `IGZ`, `MQ`, `CBLTDLI`, `AIBTDLI`, `DFH` | Missing programs with these prefixes are system routines: shown as utilities, not as gaps |
| | `DomainRules`, `DomainFallback` | generic rules; `Other` | Business function per program (see above) |
| | `LowRiskCarveScore`, `ModerateCarveScore` | 75, 50 | The carve-score tiers |
| | `MaxEvidencePerEdge` | 20 | How many source lines are kept for each edge |
| `AiLoop` | `MaxRuns`, `MaxTimelineEvents` | 50, 200 | How much is listed |
| | `StaleRunMinutes` | 30 | When an unfinished run with no recent events is shown as interrupted |
| | `MaxFallbacks`, `MaxFailedCallRate` | 0, 0.1 | The thresholds at which the model-loop gate warns |
| | `PollSeconds` | 5 | The refresh interval while a run is in progress |
