**Last updated**: 2026-11-11

# Estate Mission Control and the AI Loop

Two portal tabs help you decide what to convert and then check how a conversion went:

- **Estate Mission Control** splits the estate into slices that can be converted on their own and orders them into waves. You can convert a slice directly from the tab. No model is involved: the graph is built from the source alone.
- **AI Loop** shows recorded native REKT scans, model attempts and managed launches. Model attempts include calls, retries, fallbacks, stages and quality gates; native scans have their own program results and original log references.

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
- **Explore.** The whole estate as one graph (Cytoscape with the fCoSE layout), grouped by business function, carve-out cluster or estate. Filter by node type, program kind (online, API, batch, subroutine), technology (CICS, DB2, VSAM, IMS, MQ, files), status or unreferenced and unreachable programs, and search by name. Program rings show **dependency readiness**: red for missing dependency evidence, amber for rejected evidence (including stale or incomplete evidence), green for a passed dependency check, and gray for unknown/unavailable assessment. Labels and the selected program's blocker details explain the colors. Parse status and conversion parity remain separate in the side panel; green does not mean converted or behaviourally equivalent. Cluster members, Convert Programs and Program Explorer show matching evidence labels and colored indicators.
- **Carve-out plan.** The wave plan. Wave 0 holds the hub programs as shared services. The other waves follow call order, so a cluster comes after every cluster it calls into, and within a wave the highest carve score goes first. Each card shows a tier: **low-risk** at or above `LowRiskCarveScore`, **moderate** at or above `ModerateCarveScore`, otherwise **core**.
- **Cluster card.** Select a card or a cluster title in the graph. It shows the rationale and score breakdown, the members, the entry points (transactions, APIs, jobs) that become the cluster's front door, the **owned data** that moves with it, the **shared data** another cluster also reads or writes (it needs a data-access API or sync; red ring in the graph), the inbound calls that become service APIs, the outbound dependencies and the references missing from the source. System routines (`SystemProgramPrefixes`) are shown as utilities and not counted as missing.
- **Stage slice.** Lists the selected source-relative identities, what they call outside the cluster, what is missing, the jobs that run end to end, and the dependency assessment for that exact selection. **Include what it calls** adds the programs the slice calls but does not contain. The displayed CLI command binds that source folder, evidence root, target and dependency policy explicitly, rather than silently using terminal defaults. The command is withheld when the selection does not meet the chosen dependency policy.
- **Send to AI loop.** Choose the target language beside the slice controls, then confirm to start a convert-only run with the provider, model and speed selected in Mission Control. The controls stay disabled while staging/launching to prevent duplicate submissions. On acceptance, the portal automatically opens AI Loop pinned to the returned **managed run ID**, not an unrelated previous run. Process acceptance is not conversion success: AI Loop tracks startup/exit status even before model telemetry exists. Staging/launch errors stay visible beside the controls; a tracking-navigation failure retains the created run ID and an **Open AI Loop** retry link.
- **Review your conversion prompts.** Before launching from Mission Control or an estate slice, a popup lists the actual templates returned by the prompt service. Select an agent to edit its current template, cancel, or explicitly continue with the current prompts. Editing cancels the pending launch; start again when ready. The Convert Programs command-review button uses the same reminder. Opening it does not generate prompts or call a model. Review the applicable prompts under `Agents/Prompts` for target architecture, frameworks, output behavior and coding requirements; existing templates may be kept. Prompts cannot supply missing code/evidence or guarantee behavioural equivalence.

Readiness is checked against the configured conversion evidence root (`REKT_FACTS_FOLDER`, otherwise `output/rekt`), not inferred from a parse-success label or a historical snapshot. A program with verified empty dependency evidence can pass; a missing `dependency-evidence.json` is **not** proof that a program has no dependencies. **Refresh readiness** rechecks current inputs. Inventory labels assess programs individually; launch checks assess the selected set together, including blocked selected callees. Strict launches with blockers are rejected before a child process starts. Draft launches require at least one eligible program and still omit blocked programs; explicit source-isolated partial mode retains its separate semantics. The CLI repeats its authoritative check at execution time.

The program-ring key is a compact two-column overlay; graph-edge explanations are
collapsed until expanded. The key, program evidence cards and launch dialogs use
the legacy portal's dark theme even when the operating system uses light mode.
Evidence panels have status-colored borders matching the program rings. Check
timestamps and source/evidence locations remain available in a collapsed disclosure.
Each readiness assessment builds its source catalog and facts-file listing once and
reuses them across program checks. The portal keeps the last whole-estate result
(warmed at startup) so program rings colour as soon as the graph draws. Any file
change under the source folder, the evidence root or `tools/` discards it, as does
a 15-minute age limit; the next request then reassesses. Launch checks and
selected-program assessments are never served from this cache.

The portal rebuilds the graph when the source tree changes, or when you select **Rebuild graph**.

Each program gets a business function from `DomainRules`: the first rule whose `Name` regex matches the program name, else the first whose `Text` regex matches its description and the resources it touches, else `DomainFallback`. Transactions and APIs take the function of the programs they start; screens take that of the programs that send them. The defaults are generic; tune them per estate.

| Endpoint | Returns |
|----------|---------|
| `GET /api/runs/readiness` | Current-source per-program dependency readiness, source/evidence roots, timestamp and actionable blockers; read-only. Served from the change-invalidated cache described above; `checkedAt` is the assessment time |
| `POST /api/runs/readiness` | Assessment for the requested source folder, optional evidence root and exact program selection; read-only, no subprocess/model calls |
| `GET /api/estate/summary` | KPIs, waves, clusters, hubs |
| `GET /api/estate/graph?cluster=C01` | Nodes and edges, for the whole estate or one cluster |
| `GET /api/estate/node/{id}` | One node with its edges and evidence |
| `GET /api/estate/cluster/{id}/slice?includeNeeds=true` | The program selectors and the command |
| `POST /api/estate/slice/convert` | Reassesses dependency readiness before starting a slice. Rejected dependency policy returns 409 with blockers and a managed rejection ID, without spawning a child. Accepted launches return managed `runId`, process `status`, selected `programs`, `error`, `logPath` and reserved relative `outputFolder`. Invalid input returns 400; process startup failure returns 503. Rejected attempts remain in managed history |
| `GET /api/estate/mission` | Everything the tab draws: nodes with kind, technology, business function, metrics and status; edges with evidence; clusters with owned and shared data; KPIs |
| `POST /api/estate/rebuild` | Rescans the source and rebuilds the graph now |

## AI Loop

### Unified activity catalogue

AI Loop follows **latest recorded activity** by default, across **Native REKT** scans,
**Model attempts**, persisted **Migration history (no telemetry)** and **Managed launches**.
Latest means a recorded event/finish/start, never the file modification time or polling time.
An explicit selection pins history, visibly labelled **Pinned selection**; **Follow newest**
resumes discovery. Old, unversioned browser selections no longer silently pin an old run.
Load older runs to inspect all retained history, including failed attempts and launches without
model telemetry. A managed exit code zero alone is not proof of migration acceptance.
Draft dependency-mode launches retain `dependencyMode: draft` in managed history and become
`partial`, not `completed`, on exit zero.

Native history is read directly from validated immediate `output/rekt-scans/*/scan.json`
manifests. This does not
insert migration database rows, rewrite JSONL, invent historical events, or identify scans
with Neo4j snapshot IDs. `activityId: native:<manifest-name>` is the manifest identity;
the opaque `native-…` `runId` is only an HTTP catalogue locator. Recorded graph identity is
shown only if a manifest explicitly supplies it. Model attempt identities retain migration
run ID plus recorded attempt ID (or physical attempt position for older telemetry).

For native activity, the tab shows native scan intervals, program status, structural output
presence, strict dependency completeness, tool exit codes and original log/evidence paths.
Filter program results to structural failures or dependency gaps. Model calls are **not
applicable**, not model success. Native scans retain a circular activity view using only
their recorded native stage labels, not the model conversion diagram. The current stage
pulses during a running operation; terminal status stops motion, and reduced-motion
preferences disable pulses without hiding stage labels. The collapsed **Original native
operation log** viewer reads only the selected validated manifest's `native.log`, using
`GET /api/ai-loop/{opaque-native-locator}/native-log`. It reads at most the last 64 KiB
without loading the whole file, labels truncation, log-write time and log-refresh time,
and escapes compiler text while stripping terminal control sequences. It refreshes only
while expanded and visible, at the existing activity refresh cadence. Expansion and scroll
are retained per selection; late cancelled responses cannot populate another run.
Missing, linked, inaccessible or changed files produce explicit errors, retaining any
previously read text with a stale warning. Arbitrary paths, model exports and evidence
file contents cannot be requested. Original evidence paths remain disk references.
Unrecorded per-program artifact flags remain unknown; a zero count of recorded
AST/CFG/data flags does not establish that native artifacts are absent.
Program times not recorded by the original scan remain unrecorded. Existing manifest times
are authoritative. The UI converts these to the browser timezone.

Future `./doctor.sh rekt-parse`, `rekt-full` and `rekt-ingest` operations create an
atomic metadata-only `doctor-<uuid>/scan.json` lifecycle journal and `native.log`, with the
actual operation recorded in `command`. Parse-only owns its parsing/JCL lifecycle.
**Full owns one operation identity across parsing, JCL and graph ingestion**, with no
duplicate child scan/ingest journal and no terminal finish until the entire command returns.
An ingestion failure leaves successfully parsed program results intact but the full operation
failed, with graph ingestion as its final stage. Ingest-only has its own operation identity
and does not claim to have freshly parsed existing artifacts.

For MCP-backed dashboard requests, cancelling a partial protocol read stops the portal's
owned MCP child before reconnecting on the next request. A partly consumed frame is never
reused, preventing an interrupted page load from leaving resource and graph requests stuck.
Filtered parsing records the resolved selected-program count, excluding programs retained
only as dependency search context. Older filtered journals without this count remain
explicitly unmeasured; their history is not rewritten.

Preparation failures, per-program outcomes/cache reuse, recorded stage intervals, command
exit and handled INT/TERM cancellation are retained. Stage intervals ending do not establish
stage success. A failed ingestion may have partially written to the graph; failure is not
proof that the graph was unchanged. No snapshot ID is inferred from the operation identity.
A killed shell may leave an unfinished program/journal; inactivity is labelled interrupted
with **no invented finish time**. External bespoke scan/ingest scripts must write the same
validated manifest contract to appear; arbitrary artifact directories are not runs.

Polling caches unchanged native manifests, rejects symlink scan roots/manifests, and reads
only immediate scan manifests (512 candidate directories, 8 MiB per manifest, 64 MiB newly
parsed per request; 5,000 displayed program rows and 2,000 log/evidence references).
It never recursively ingests artifacts or exports source/chat content. Malformed first
writes without a valid recorded start/schema are not invented as runs; an unreadable update
retains the last valid cached observation with a warning. Restart clears that observation.
Very large estates exceeding these safety limits require a catalogue/index extension.
Checksums are referenced, not rehashed by each poll. Existing evidence files are never
modified by the reader.

**Activation and read-only acceptance:** the running portal must use the newly built web
assembly and matching `wwwroot/ai-loop.js`. Building in an isolated artifacts directory does
not update an already-running process. Do not overwrite its runtime directory. At an approved
maintenance point, deploy the validated build/static assets together, preserve the repository
root/configuration, and start the replacement using the normal operator service procedure.
No scan, JVM, graph mutation or model call is needed to activate or inspect existing history.
The isolated validation build is under the session's
`files/unified-activity-validation/bin/McpChatWeb/debug/`; it is not the live
`ast-gap-live-runtime` directory. An operator-approved replacement can use that directory
without overwriting the old runtime. From the repository root, with the existing service's
environment preserved and **only after the operator releases the existing listener**:

```bash
# Set VALIDATED to the absolute isolated .../unified-activity-validation directory.
# Do not run against an occupied port or treat this as permission to stop the existing service.
dotnet "$VALIDATED/bin/McpChatWeb/debug/McpChatWeb.dll" \
  --contentRoot "$PWD/McpChatWeb" --urls http://localhost:5028
```

After activation:

```bash
# --scan names directories under output/rekt-scans; --model-run is optional.
python3 tools/check_ai_loop_activity.py --url http://localhost:5028 --scan <scan-name> --model-run <run-id>
curl --fail --silent 'http://localhost:5028/api/ai-loop/runs?offset=0'
# In AI Loop: Follow newest; select each Native REKT entry, inspect failure filters and paths;
# then explicitly pin the model run and verify its recorded model times/counts remain unchanged.
```

Each run writes its events to `output/.metrics/<runId>.jsonl`, through the same sink as the projection metrics. Requests and stages use the active run/attempt scope; tools outside that scope do not acquire a fabricated run. References to successfully saved chat exports may be appended afterwards using the run and attempt recorded on each message.

| Event | Written when |
|-------|--------------|
| `run_started` | An attempt begins. Records mode, language, relative output folder, a new `attemptId`, `telemetryVersion: 2`, and `requestCoverage: agent-boundary`. Portal subprocesses also carry their `managedRunId` through `AI_LOOP_MANAGED_RUN_ID` |
| `stage` | The run moves to its next step |
| `llm_request_started` | An instrumented agent dispatches a request after acquiring its limiter reservation. Records `requestId`, agent, provider, model and context identifier |
| `llm_call` | A request succeeds, fails or is cancelled. Retains the existing event name and fields, adding the matching `requestId` and explicit `outcome`. Sizes and reported token counts are metadata only |
| `llm_retry` | A call is retried: `transient_error`, `rate_limit`, `reasoning_exhaustion` or `output_truncation` |
| `llm_fallback` | An agent gives up and writes a stub: `retries_exhausted`, `non_retryable_error`, `content_filter`, `reasoning_exhaustion` or `output_truncation` |
| `projection_metrics` | A structural-context block was prepared, not necessarily dispatched. New records include `contextId` when an exact block can be correlated |
| `context_dispatched` | The same agent/context identifier dispatched a prompt containing the exact prepared block. Links `contextId` to `requestId`, without recording the block |
| `run_finished` | The attempt ends as `completed`, `failed`, `cancelled` or `no_files` |
| `validation_completed` | Output validation records acceptance separately from model-call success, including whether compilation and parity were measured |
| `chat_log_saved` | A correlated JSON chat export was saved successfully; records its repository-relative path, format and the message count for that exact run/attempt, not its contents |

Direct and chunked migration persist cancellation as `Cancelled` in the run database, matching the `cancelled` terminal event rather than reporting a conversion failure.
The compile gate reads the Java result for both the `Java` and `Java Quarkus` target labels emitted by migration runs.
Standard, smart, chunked and reverse-engineering paths establish tracking before
early validation/discovery failures. Routing into a selected process shares the same
attempt rather than creating duplicate starts or finishes. Resumes create distinct
attempts. Cancellation is passed into instrumented requests and retry delays.

New events carry no source code, prompts, responses, provider error bodies or secrets. Failures record exception types and stable reason codes, not truncated exception messages. Existing chat logging is separate and is not read by this view.

Coverage is explicitly **agent-boundary** in `AgentBase` and the analyzer's separate request path. It includes normal requests and their instrumented retries, reasoning/truncation escalation and adaptive re-chunk calls where supported. Analyzer requests, retry/fallback outcomes and cancellation are now recorded too; older uninstrumented analyzer calls cannot be recovered. Provider-internal HTTP/auth retries, tools outside these boundaries and time waiting for a limiter reservation are not request starts. Responses API token usage is unavailable at this boundary; it is not estimated or reported as zero. A context dispatch is recorded only for an exact prepared block and context identity; differently named chunks or transformed blocks remain unproven, not inferred from a filename.

Full chat exports remain separate from metadata telemetry. New JSON messages
record `runId`, `attemptId` and, for instrumented calls, `requestId`; export names
include a unique session identifier so concurrent sessions do not overwrite each
other. Only recorded run/attempt groups receive a `chat_log_saved` reference.
Outside-repository or uncorrelated exports are not assigned to a run by guessing.
Full exports can contain prompts, source and responses; metadata history does not
embed those payloads. Do not publish the exports as routine diagnostic metadata.

Structural projections loaded from existing REKT artifacts are **reused context**, not evidence of a parser execution. Projection-cache hits do not establish execution either. Context preparation is shown separately from dispatch; historical preparation records without correlation cannot prove a handoff.

### Time, freshness and coverage

Event timestamps are UTC (`Z`) at the metadata append boundary; they are not the
time a chat export was read or a dashboard polled. Explicit offsets are normalized
to UTC, and legacy timestamps without an offset are treated as UTC, matching the
metrics writer rather than the portal host's local timezone. The timeline sorts
by normalized event time (append order breaks ties); stage intervals and legacy
attempt boundaries still follow recorded lifecycle order.

The browser labels its local timezone and includes the date and timezone on
timeline/context timestamps. A timeline timestamp's tooltip retains the UTC
value. **Started**, **finished**, **last recorded activity**, and **last successful
refresh** are separate: a fresh refresh does not make old work current. Saved or
explicit selections are labelled **Pinned selection**. With no saved selection,
the tab follows the newest launch/attempt; **Follow newest** releases a pin.
Launch navigation still pins the exact managed ID until explicitly released.

The API rereads the complete JSONL file on every request with shared read/write
access; it has no file-tail cache or persisted read offset. An incomplete append
is retried on the next refresh. Unreadable lines and invalid/unrecorded timestamps
are reported, not silently treated as complete evidence. `telemetry` reports total
selected-attempt records, timeline/context totals and file-level read issues.
Both detail panels apply the server's `MaxTimelineEvents` bound without another
hidden browser limit. Full retained JSONL and process logs remain on disk; the
managed log endpoint is still a separately labelled bounded tail. AI Loop
responses and browser fetches use `no-store`.
If repository static assets are newer than a frozen running backend, the view
labels its fallback as the **latest supplied record**; complete `lastEventAt` and
coverage metadata require activating a build containing the updated reader.
Refreshing the browser alone does not replace a loaded server assembly.

A quiet unmatched request is not proof of termination. When an explicitly
associated managed process is still tracked running/paused, its process status
wins over the silence heuristic; recorded terminal events and superseded attempts
are never revived. Without that process evidence, stale interruption remains a
heuristic, not an observed exit. Process liveness does not prove model progress.

Native REKT operations with validated manifests appear as separate native activity,
including the existing scan evidence and future `doctor.sh rekt-parse`, `rekt-full`
and `rekt-ingest` journals.
They do **not** create model-run events or extend an unrelated migration's duration.
Their recorded scan start/finish/update times establish native activity; output file
modification times do not. Existing REKT projections establish reused context only.
AST/dependency queries, graph reads, graph rebuilds or ingestion outside the instrumented
doctor commands, and read-only acceptance checks are not automatically journalled.
Unrecorded historical operations
cannot be backfilled truthfully, and graph snapshot IDs are not substituted for native
activity identities. A later `chat_log_saved` record is export activity, not another
request or an extension of the conversion's duration.

The **AI Loop** tab combines validated native scan manifests, model telemetry,
migration database history and managed launches, newest recorded activity first.
**Load older runs** pages through the retained
catalogue; `MaxRuns` is a page size, not a retention limit. Database-only runs remain
discoverable without invented calls or stages. A launch pins its managed run ID
while waiting for its associated numeric attempt; startup failure is shown instead
of historical model calls. Missing telemetry, including for legacy launches,
remains **not recorded**, not inferred. Native discovery/display safety bounds are
documented above; paging does not bypass them. Native entries show their own scan/program
results and log references, with model calls **not applicable**. For model attempts it shows:

- **Live circular phases**: phase names are matched against recorded stages. The current recorded phase has a pulsing halo, an activity indicator and an explicit **Now** label. The activity line names the current recorded stage and instrumented requests still in flight, when available. A phase pulse alone does **not** prove that a model or parser is running; request activity requires recorded starts and outcomes, and older analyzer calls were uninstrumented.
  - **Ended** means a recorded stage interval ended (its duration is available), not that the phase passed. Stages may repeat, and a later stage does not establish that earlier, unrecorded phases ran or succeeded.
  - **Not recorded** means there is no stage evidence, not that the phase is queued, skipped or complete. Legacy unknown statuses remain unknown.
  - Phase names group recorded stage labels, not parser provenance or authoritative discovery. **Discover** does not establish that REKT executed; **Analyze** does not establish that dependency analysis was deterministic rather than AI-derived.
    Converter context and stored REKT artifacts do not prove live Neo4j dependency use or complete dependency extraction. Existing/ongoing runs may still show dependency-diagram model calls; those remain model activity, not deterministic dependency evidence.
  - Only the recorded compile/parity gate statuses add green **Gate passed**, red **Gate failed**, or **Gate warning** labels to their corresponding phases. Gate evidence is independent of stage evidence; an artifact can exist without a recorded stage. Warnings are not passes, and run completion is not a quality pass.
  - Progress is a count of observed phases and ended stage intervals, **not a percentage**. Stage bars compare recorded durations only; neither elapsed time nor stage order creates success.
  - Motion uses CSS only and the existing serialized polling. It stops on terminal/interrupted/unknown status, refresh error (stale activity is labelled), tab deactivation or a hidden document. Reduced-motion preferences disable all pulses while preserving the same visible text and SVG descriptions. No new polling, parser or model calls are introduced.
- **KPIs**: status, duration, recorded outcomes, request starts when available, failures, cancellations, unknown outcomes, retries and fallbacks
- **Quality gates**, one card each:
  - **Model loop**: warns when there are more than `MaxFallbacks` fallbacks, or when the failed-call rate is above `MaxFailedCallRate`
  - **Compile gate**: read from `compile-status.json` for C#, or the Java gate's structured `java-compile-status.json`. An unmeasured build warns; measured compilation failure fails. Raw build logs are not parsed
  - **Conversion parity**: read from `conversion-parity.json`. A run below the threshold warns or fails, following `OnLowScore`
  - **JCL jobs**: read from `jobs-manifest.json`; shows the steps that cannot run yet
- **Stages**: each stage with its duration
- **Agents**: starts/outcomes, success/cancellation/unknown counts, failures, retries, fallbacks, measured latency and reported tokens. Partial token sums identify how many calls reported usage
- **Outstanding requests**: unmatched starts are in flight only while the attempt is running. After it ends or is interrupted, unmatched requests are unknown, not successes or still running
- **Bounded history panel**: outstanding requests and context handoffs share a keyboard-focusable scroll region, so long histories do not stretch the phase panel or push quality gates far below it. All rendered details remain available by scrolling on desktop and narrow screens.
- **Events**: bounded starts, outcomes, lifecycle, stages, retries and fallbacks; each correlated request retains its identifier
- **Attempts**: the latest attempt is selected by default, but retained earlier attempts are selectable. Recorded IDs remain unchanged; an explicit event-block index selects legacy attempts without manufacturing an ID.
- **Logs and evidence**: the selected managed launch's process log tail, its migration/attempt identifiers, output location and available correlated chat-export references. Changing selection clears the previous launch's log, including when the next refresh fails. Conversation references expose metadata only, not prompt/response contents.

A gate whose artifact is missing or predates the current attempt shows as *not run*. The reader follows a run's output folder only when it is inside `output/`; legacy gate files have no attempt identifier, so freshness uses filesystem modification time. Model-loop success is not inferred from unmatched requests or an incomplete run.

An unqualified detail request covers the **latest attempt** in a run's append-only
JSONL file. `relatedAttempts` lists retained alternatives; `attemptId` or
`attemptIndex` selects one explicitly. A resume does not inherit an earlier finish,
calls or stages. Explicit attempt identifiers also exclude late writes from
previous attempts, including chat exports saved after a resume. Database
termination is used only when its completion timestamp is at least the current
attempt's start. Older JSONL remains readable: request starts are
`null`/not recorded, rather than zero, and outcomes without a success flag remain
unknown.

Managed launch metadata is persisted atomically under
`Logs/portal-history/<managedRunId>.json`; captured stdout/stderr and portal
lifecycle messages are stored under `Logs/portal/<managedRunId>.log`. These paths
are relative to the stable repository working directory, not the deployed binary
directory. Startup reloads retained launches and marks unresolved nonterminal
tracking as **interrupted**, not successful. Existing process logs without a
journal can be discovered as legacy evidence, but do not establish a conversion
outcome. In-memory display limits do not delete persistent history. Missing or
unreadable log evidence remains explicit.

Polling is serialized, with abort and generation/run guards. Leaving the tab or hiding the document cancels pending work; reactivation refreshes. Running lists or a running selected detail poll every `PollSeconds`, including a selected attempt outside the loaded page; empty/completed lists continue discovering new runs at a slower interval (at least 15 seconds). Failures retain clearly labelled stale data, display a safe error and back off to at most 60 seconds. Start controls remain in Mission Control and Estate Mission Control, preserving their configured provider, model, speed, source and program-array contracts.

No log scraping, latest-log fallback, second parser, private engine source/dependency or donor backend is used.

| Endpoint | Returns |
|----------|---------|
| `GET /api/ai-loop/runs?offset=N` | A page of retained migration/telemetry runs and managed launches, with `hasMore`, `nextOffset` and the poll interval |
| `GET /api/ai-loop/{runId}?attemptId=ID` | The selected attempt's summary, gates, stages, agents, bounded events/context handoffs, outstanding requests, `relatedAttempts` and log references; omit the selector for latest |
| `GET /api/ai-loop/{runId}?attemptIndex=N` | Selects a legacy attempt by its recorded event-block position when no explicit attempt ID exists |
| `GET /api/runs/managed/{managedRunId}/log?lines=2000` | The selected launch's persisted process-log tail as `lines`, `available`, `warning`; defaults to 100 lines and clamps to 1–2000, without accepting a filesystem path |

Focused offline validation:

```bash
dotnet test CobolToQuarkusMigration.Tests/CobolToQuarkusMigration.Tests.csproj --artifacts-path output/validation/ai-loop-check -p:CopilotSkipCliDownload=true --filter 'FullyQualifiedName~AiLoopEventsTests|FullyQualifiedName~RunTrackingTests'
dotnet test McpChatWeb.Tests/McpChatWeb.Tests.csproj --artifacts-path output/validation/ai-loop-check -p:CopilotSkipCliDownload=true --filter 'FullyQualifiedName~AiLoopReaderTests|FullyQualifiedName~AiLoopEndpointsTests'
node --test McpChatWeb.Tests/JavaScript/ai-loop.test.cjs
node --test McpChatWeb.Tests/JavaScript/estate-launch.test.cjs
node --test McpChatWeb.Tests/Frontend/*.test.cjs
```

Endpoint tests start only the AI Loop routes with isolated storage and no production startup, configured database, Neo4j, AI clients or real migration subprocesses. Process-exit cases use a local shell fixture.
The Estate JavaScript tests exercise rendered staging/launch clicks with an isolated DOM and mocked HTTP: they do not start conversions, call providers or touch configured storage.
Use an artifact directory that is not mapped by a running portal; the commands
above keep build outputs away from its deployed DLLs. Core lifecycle tests use
scripted clients, not paid providers.

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
| `AiLoop` | `MaxRuns`, `MaxTimelineEvents` | 50, 200 | History page size and bounded event-detail size; neither deletes retained records |
| | `StaleRunMinutes` | 30 | When an unfinished run with no recent events is shown as interrupted |
| | `MaxFallbacks`, `MaxFailedCallRate` | 0, 0.1 | The thresholds at which the model-loop gate warns |
| | `PollSeconds` | 5 | The refresh interval while a run is in progress |
