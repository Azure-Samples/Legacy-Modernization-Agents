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
| Transaction / map | CICS `LINK`, `XCTL`, `LOAD` and `START`/`RETURN TRANSID`, BMS maps (`MapExtensions`) and CSD definitions (`CicsDefinitionExtensions`) |

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

Open the **Estate Mission Control** tab.

- **Waves and clusters.** The top of the tab shows KPIs and the waves, with cluster cards ordered by carve score.
- **Cluster detail.** Select a card to see its score breakdown and graph. Select a node to see its evidence: the file, line and source text behind each relationship.
- **Slice.** The slice lists the programs to convert. The program name is used, or the source-relative path when two programs share a name. **Include what it calls** adds the programs the slice calls but does not contain. The slice also shows the equivalent `./doctor.sh convert-only --program …` command.
- **Convert.** This starts a convert-only run of the slice. It uses the provider, model and speed currently selected in Mission Control.

The portal rebuilds the graph only when the source tree changes.

| Endpoint | Returns |
|----------|---------|
| `GET /api/estate/summary` | KPIs, waves, clusters, hubs |
| `GET /api/estate/graph?cluster=C01` | Nodes and edges, for the whole estate or one cluster |
| `GET /api/estate/node/{id}` | One node with its edges and evidence |
| `GET /api/estate/cluster/{id}/slice?includeNeeds=true` | The program selectors and the command |
| `POST /api/estate/slice/convert` | Starts a convert-only run of a slice |

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
| | `MaxEvidencePerEdge` | 20 | How many source lines are kept for each edge |
| `AiLoop` | `MaxRuns`, `MaxTimelineEvents` | 50, 200 | How much is listed |
| | `StaleRunMinutes` | 30 | When an unfinished run with no recent events is shown as interrupted |
| | `MaxFallbacks`, `MaxFailedCallRate` | 0, 0.1 | The thresholds at which the model-loop gate warns |
| | `PollSeconds` | 5 | The refresh interval while a run is in progress |
