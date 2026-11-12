# rekt-oss-mma

`rekt-oss-mma` is the COBOL parser image used by the `cobol-rekt` service in
`docker-compose.yml`.

The image build:

1. Clones [cobol-rekt](https://github.com/avishek-sen-gupta/cobol-rekt) at
   `v0.1.0-RC6`.
2. Applies the local fixes under [`patches/`](./patches/).
3. Compiles `smojol-cli.jar` and packages it with the IDMS dialect and Graphviz.

## Build

```bash
docker compose build cobol-rekt
```

The builder caps Maven's heap at 512 MiB and metaspace at 192 MiB so it can
coexist with local graph services. Override `MAVEN_OPTS` with a build argument
when a larger build budget is available.

The container stays running so the pipeline can invoke the parser with
`docker exec`. Source files are mounted at `/source` and parser output is
written to `/output`.

```bash
docker exec cobol-rekt java -jar /app/smojol-cli.jar --version
```

## Dependency evidence and cache safety

Patch `0021` also supports initialized LOCAL-STORAGE program-name literals under
the same complete no-write proof as WORKING-STORAGE literals. This covers
DBCRFUN's local `ABNDPROC` operand without guessing a dynamic target or changing
the source. The export identifies the actual storage class; mutations, address
escapes, missing catalogs and incomplete callees still block conversion.

`doctor.sh` requests the existing `dependency --export` command after **every**
structural outcome, not only after failures. Parser exit 0 or `Full` structural
confidence is not a dependency-completeness claim. Failed exports retain their
errors, source-relative sidecar, and any partial export.

Filtered `program-facts extract --programs ...` runs resolve call targets against
the whole staged program inventory; the filter limits which facts files are
written, not the source identities available for dependency resolution. Duplicate
callee names remain ambiguous rather than selecting an arbitrary source path.
Older filtered facts may retain a bare callee name where strict conversion expects
its source-relative path. Regenerate those projections from the matching inputs;
do not hand-edit them or disable the evidence gate.

For a source at `batch/PROGRAM.cbl`, the active output contract is:

* `batch/PROGRAM.cbl-deps.json`: the parser's actual recursive
  `callTarget`/`dependencies` tree, never completed by a regex or model.
* `batch/PROGRAM.cbl.report/dependency-evidence.json`:
  `schemaVersion: 1`, `producer: "cobol-rekt/dependency"`, `relativePath`,
  absolute `originalSourcePath`, raw `originalSourceSha256`, absolute
  `exportPath`, raw `exportSha256`, integer `exportExitCode`,
  `coverage` (`COPY`, `CALL`, `EXEC_CICS`, `EXEC_SQL`, `FILE_IO` mapped to
  `complete`, `partial`, or `unavailable`), `definitions`
  (`kind`, `name`, absolute `path`, raw `sha256`), `unresolvedDiagnostics`,
  and explicit `placeholderContaminated`.
* `batch/PROGRAM.cbl.parse.log`: retained parser/export diagnostics.

The native lifecycle journal records bounded dependency diagnostic strings and
hash-checked input metadata (`kind`, `name`, `path`) at measurement time. The
portal explains those conditions as unresolved inputs, parser/exporter gaps,
derived-layout gaps or runtime-mapping requirements, retaining raw measurement
notes separately. Recorded input presence is historical: polling never opens
current source files to rewrite a scan's result. Older journals explicitly show
that input inventory was not recorded. New running updates clear previous
measurements, and truncated diagnostic/input lists are labelled as truncated.
These display fields do not change the strict dependency gate.

Legacy exports have no native per-kind coverage declaration or resolved-definition
hashes. They therefore remain partial, even when structurally successful. Patch
`0012` adds `dependencyExportSchemaVersion`, parsed `sourcePath`/`sourceSha256`,
`dependencyCoverage`, `dependencyDiagnostics`, and typed, hash-bound COPY/CALL
nodes to the existing export. The helper only promotes supported coverage after
checking that native metadata against the actual original/staged input chain.
Clean native trees can additionally attest supported absence:
`dependencyAbsenceEvidence.EXEC_CICS` and `.EXEC_SQL` must equal
`antlr-tree:no-dialect-contexts`, and `.FILE_IO` must equal
`antlr-tree:no-file-io-contexts`, with `dependencyAnalysisComplete: true`.
The producer examines the actual ANTLR tree after dialect restoration.
Unknown dialect/foreign/error contexts prevent these absence claims, and actual
I/O or file-declaration contexts prevent file-I/O absence. No regex establishes absence.
Native `dependencyAnalysisComplete` and a successful dependency-command exit
require all five kinds complete, no issues, and complete analyses for every
non-COPY callee. Incomplete callee domains and diagnostics propagate to callers.
Root dependency edges remain direct; recursive supporting inputs retain their
own hashes without becoming invented caller-to-grandchild edges.
Source scans are contradiction guards only: original COPY statements and an
empty exported COPY set are incomplete, not evidence of zero dependencies.
Unresolved SQL/CICS/file-I/O bindings still have partial coverage and block
strict conversion.

The conversion gate resolves callee identities against the original source
catalog, excluding preprocessing/staging copies, even when assessing one selected
caller. This keeps its comparison with ingested source-relative facts stable;
it does not waive callee hashes, completeness checks or incorrect facts paths.

Patch `0013` distinguishes an empty native `FileSectionContext` from actual file
declarations. The visitor still traverses its children and the rest of the
program: SELECT/FD declarations, I/O statements, errors and opaque dialect
contexts continue to prevent file-I/O absence claims.

Native `Cics_returnContext` nodes also produce `dependencyControlTransfers` in
the hash-bound export, recording the RETURN text, parser line/column, bare versus
optioned form, `return-to-cics` control effect and `runtimeMapping: "unverified"`.
Coordinates explicitly use `coordinateSpace: "expanded-parser-input"`: COPY
expansion can shift them, so they are not presented as original-file locations.
These records are not CALL edges, absence proofs or generated Java behavior.
This patch does not remove RETURN or treat it as a Java `return`, and neither dependency
recognition nor compilation establishes Spring/CICS runtime equivalence.

Patch `0014` exports `dependencyDialectStatements` directly from restored CICS
and DB2 SQL grammar contexts. Each statement includes its operation, native rule,
text, parser coordinates, full nested rule/token syntax, binding status and
`runtimeMapping: "unverified"`. Shared restored SQL contexts are deduplicated by
object identity, not source text; repeated statements at different locations survive.
Unknown/unrestored dialects remain blockers.

Known CICS does not invalidate parser-attested SQL absence, and known SQL does
not invalidate CICS absence. These use `antlr-tree:no-sql-contexts` and
`antlr-tree:no-cics-contexts` respectively. Bare RETURN has no resource operands:
its complete dependency coverage is positive evidence recorded as
`dependencyPresenceEvidence.EXEC_CICS: "antlr-tree:resource-free-cics-return"`,
not a claim that CICS is absent. Its retained grammar must contain only RETURN.
This permits file-I/O absence alongside bare RETURN when no real file contexts
exist. Optioned RETURN, LINK and SQL statements retain their unresolved resource
bindings and nonzero dependency exit. Missing copybooks, derived-map gaps and
incomplete callees remain failures. Once source dependencies and bindings are
verified, conversion receives the native CICS/SQL records as implementation
requirements, including records in supporting callees. Target adapters need not
already exist: the converter must implement them. A claimed
`runtimeMapping: "verified"` string is not an implemented or validated target
contract. Java output goes through the bounded Maven build/repair loop; neither
compilation nor dependency completeness proves runtime behavior.

Patch `0015` can resolve literal local CICS WRITE FILE/DATASET dependencies from
genuine YAML resource definitions. Set `REKT_CICS_RESOURCE_DEFINITIONS` to their
container-visible paths, separated by `:` in the Linux parser; `doctor.sh`
forwards an explicitly set host value. For example:

```bash
_REKT_PROGRAM_FILTER=bank-of-z/cobol/ABNDPROC.cbl \
REKT_CICS_RESOURCE_DEFINITIONS=/source/bank-of-z/cics/bank-of-z-definitions.yaml \
  ./doctor.sh rekt-parse
```

This example selects only ABNDPROC. The native export retains
`CICS_FILE` names, definition paths/raw SHA256 and dataset/layout attributes.
Missing, duplicate, invalid, dynamic and remote SYSID operands remain unresolved.
The consumer rechecks the actual definition file and invalidates stale evidence.
File-I/O coverage uses positive bindings rather than a false absence claim.
`DFHRESP(NORMAL)` is resource-free, not a fabricated numeric runtime constant.

Patch `0036` also reads IBM DFHCSDUP definition files (any path not ending in
`.yaml`/`.yml`). `doctor.sh` selects CICS resources per estate: a configured
value is used only when every path lies inside `/source/<estate>/`; otherwise
the estate's own `*.csd` files are used, so one estate never binds against
another's definitions. Constant `RETURN TRANSID`/`MAP`/`MAPSET` operands bind
only when the named item is a provably read-only `VALUE` literal.

Patch `0037` binds keyed CICS file commands and constant `XCTL PROGRAM`
targets. A CSD FILE without lengths takes them only from the estate's own
IDCAMS `DEFINE` jobs (`doctor.sh` passes `/source/<estate>/**/*.jcl|*.prc`
containing `PGM=IDCAMS` as `REKT_VSAM_DEFINITIONS`); alternate-index paths
use the AIX key and the base record size. Every IDCAMS source is hash-checked
as a `CICS_DATASET` definition. Non-KSDS, conflicting or missing definitions,
forbidden CSD services and runtime-assigned names stay unresolved.

Patch `0038` adds the CICS SPI `INQUIRE PROGRAM` grammar form and resolves
`XCTL`/`INQUIRE PROGRAM` operands written as `NAME(SUB)` when `NAME` lives in
an `OCCURS` group that `REDEFINES` a sibling group of literal `VALUE` items
and every reference is read-only. Each defined occurrence becomes a CSD-backed
`CICS_PROGRAM` binding; occurrences past the `VALUE` area are recorded as
`null` (undefined) rather than invented.

Patch `0039` resolves `XCTL PROGRAM(field)` through a structured forward
reaching-definition walk of one program. Every literal, constant or blanking
`MOVE`/`INITIALIZE` that can reach the XCTL becomes a CSD-backed binding. Any
other write, `GO TO`, `ALTER`, `HANDLE`, `THRU`, `DECLARATIVES` or pointer use
leaves the target unresolved.

Patch `0040` lets `LINK PROGRAM(field)` use the same read-only constant and
`OCCURS` table proofs as `XCTL` (bindings carry `controlEffect: call`). It also
treats the unqualified Db2 13 for z/OS built-in `TIMESTAMP_FORMAT`, which the
grammar's built-in rule omits, as a built-in rather than an external function.

Patch `0016` retains the existing DB2 semantic grammar parse and binds SQL
table/cursor references to exact declarations and host references to unique
native COBOL declarations. `semanticSyntax`, `resourceReferences` and
`resourceBindings` preserve the actual grammar and expanded-input declarations.
SQL cursor declarations and their queries are no longer removed by preprocessing.
Supported dependency operations and conservative limits are detailed in
[`patches/README.md`](./patches/README.md): no guessed default schema, missing
library routine, ambiguous declaration or unresolved external SQL function is
silently accepted. SQL-only file I/O uses positive
`antlr-tree:bound-sql-resources` evidence. These are dependency checks, not live
database verification, Java SQL adapters or transaction-equivalence claims.
Generation receives these records as mandatory implementation requirements;
target runtime behavior still needs separate verification.

Set `REKT_DEPENDENCY_PROFILE` explicitly to use a target-analysis profile.
Patch `0017` can qualify an otherwise-unbound SQL relation against that profile's
chosen schema, but only when an actual matching parsed declaration exists.
`contracts/bank-of-z-spring-h2-v2.json` selects STTESTER for the new target and supplies
documented CEEGMT/CEEDATM/CEEDAYS/CEELOCT contracts. The original profile remains
unchanged so historical hash-bound evidence is not invalidated. Their external-runtime leaves do not claim
parsed COBOL implementations. The profile's hash and exact contract contents
are checked by evidence generation, cache validation and the conversion gate.
This is not evidence of the original DB2 configuration or implemented Java behavior.

An estate can carry its own profile at `contracts/estates/<estate>.json`. Doctor uses it
instead of `REKT_DEPENDENCY_PROFILE` for that estate only, so contracts and the SQL
default schema are never borrowed across estates. `contracts/estates/carddemo.json`
declares only the documented CEE3ABD contract and no default schema, because
CardDemo qualifies its tables explicitly.

To run the full parse-and-ingest pipeline for Bank-of-Z only, keep the full source
tree available for dependency resolution and pass exact source-relative selectors:

```bash
bank_programs=$(find source/bank-of-z/cobol -maxdepth 1 -type f -iname '*.cbl' \
  | sed 's#^source/##' | sort | paste -sd, -)
_REKT_PROGRAM_FILTER="$bank_programs" \
REKT_CICS_RESOURCE_DEFINITIONS=/source/bank-of-z/cics/bank-of-z-definitions.yaml \
REKT_SQL_DECLARATIONS=/source/bank-of-z/copy/CONTDB2.cpy \
REKT_DEPENDENCY_PROFILE="$PWD/tools/cobol-rekt/contracts/bank-of-z-spring-h2-v2.json" \
  ./doctor.sh rekt-full
```

Use a container built with the current patch chain; building an image does not
replace a running parser container. `REKT_CONTAINER` can select an existing
patched container without restarting another checkout's services. The profile
must be readable inside that container at the supplied path and by the host
evidence checks. A same-path read-only mount of the contracts directory satisfies
this requirement. The evidence producer accepts an outside-source profile only
when it matches the explicitly configured `REKT_DEPENDENCY_PROFILE` file
(`write --dependency-profile` can supply the same binding directly). The native
profile hash and contract contents must still match. This does not allow COPY,
CALL or CICS source definitions outside the declared source root.
For a manually created parser, preserve its mounts and restart policy. After an
intentional stop, start that named container before running doctor; the compose
fallback does not recreate a custom image and its additional profile mount.
Do not run two scans concurrently against one checkout's
shared `.rekt-staging`. The filter limits program outputs, not dependency search
context; missing vendor copybooks and incomplete derived BMS layouts still block
dependency readiness.

The optional CICS definition, SQL declaration catalog and profile settings can also be saved in
`Config/ai-config.local.env`; explicitly set shell values, including an empty
value, take precedence.
`MIGRATION_CLI_ASSEMBLY` can select an isolated, prebuilt CLI for program/native-JCL fact extraction
and optional orphan pruning, as it does for portal process launches. This permits
testing updated evidence consumers without overwriting assemblies loaded by a
running portal or MCP process. Otherwise program-facts extraction uses the checkout's
existing `--no-build` CLI, while native-JCL scanning keeps its normal `dotnet run`
build. Build the CLI before expecting updated program-facts consumers.

Patch `0018` retains COMP-1/COMP-2 and their COMPUTATIONAL aliases as numeric,
four/eight-byte native floating-point fields. Preprocessing no longer changes them
to alphanumeric PIC declarations. Native memory execution remains explicitly
unsupported without the original encoding/compiler settings; the parser does
not guess IEEE or IBM hexadecimal representation.

Patch `0019` preserves `FOR FETCH ONLY` in DB2 cursor declarations and retains
colon-prefixed null indicators as actual host references. Patch `0020` binds
current-task CICS data operands and local abend labels to native declarations.
Local LINK requires a supplied PROGRAM catalog entry and a genuine recursively
parsed callee, with both input hashes retained. A variable LINK target requires a
conservative whole-program immutability proof, not just its initial VALUE.
Unresolved targets, remote/channel variants and missing vendor definitions remain
blocked. See [`patches/README.md`](./patches/README.md) for the supported subset;
dependency bindings do not implement the commands or establish runtime equivalence.

Patch `0022` binds qualified `OF`/`IN` data operands using their native declaration
ancestry, accepts trailing space padding within eight-byte program names, and
retains named ENQ/DEQ operations with their actual operands. Bare duplicate fields,
missing qualifiers, mutable targets and invalid padding remain unresolved.
Supported response symbols are NORMAL, INVREQ, MAPFAIL, NOTFINISHED and NOTFND;
no numeric response codes are invented. Patch `0023` adds the documented
CEEDAYS/CEELOCT profile contracts. Lock adapters, original time-zone settings and
date/century-window behavior still require separate target implementation and
verification.

Patch `0024` adds explicitly configured shared SQL declaration catalogs through
`REKT_SQL_DECLARATIONS` (container-visible paths, separated by `:` on Linux).
For Bank-of-Z, `/source/bank-of-z/copy/CONTDB2.cpy` supplies the genuine
`STTESTER.CONTROL` declaration without injecting COPY statements into CREACC.
Repository presence alone is not enough: catalogs must be explicitly supplied,
within the source root, parsed by the existing DB2 grammar, and hash-bound in the
export and evidence. Local declarations take precedence. Shared catalogs supply
table declarations only, never host variables or cursors; ambiguity, invalid SQL
and changed input hashes remain blockers. Unqualified schema selection still
requires the explicit dependency profile.

Structural parsing retains its one-program `.parser-input/<relativePath>` scope.
Dependency exports use a separate `.dependency-input/<relativePath>` search view:
the exact selected entry plus regular-file snapshots of uniquely named staged
programs, including genuine sibling callees. Program stems are compared
case-insensitively across `.cbl`/`.cob` extensions; ambiguous callees are withheld
rather than letting native recursive find-first lookup select another estate.
The selected entry remains explicit even when its basename occurs elsewhere.
These views create no edges: the native parser still determines actual CALLs.
The helper verifies native full paths and hashes against each view snapshot,
canonical staging identity/provenance, current original, and any current
transformation. CALL definitions bind the original and, when transformed, the
persistent actual transformation with separate hashes before staging is removed.
An ambiguous actual CALL, changed snapshot, or stale provenance remains partial.

Before reparsing a source, its previous artifacts are moved beneath
`output/rekt/.previous/<unique-run>/<source-relative-path>...`. Unrelated sources
are not deleted. Temporary parser reports live beneath `.work` and are relocated
only to their exact source-relative identity. Active artifact and graph discovery
ignore `.previous` and `.work`.

Preprocessing similarly preserves old shadows under `.preprocessed/.previous`
and regenerates current transformations. Staging accepts a transformation only
when its original path, original raw hash, and output raw hash still match.
Program transformations preserve their source-relative directories beneath
`.preprocessed`; two same-named programs cannot overwrite one flat rewrite or
borrow each other's provenance. Ambiguous legacy flat rewrites remain blocked.
Known placeholders and unproven configured generated-copybook libraries are not
genuine definitions merely because files exist or lack a stub marker. Grounded
BMS transformations retain both BMS input and generated-output hashes; they are
not interchangeable with authentic vendor layouts. Patch `0025` permits complete
COPY coverage only for the supported `bms-symbolic-map/layout-v1` storage recipe,
with exact original BMS, generated COPY and layout-manifest hashes linked in
native `dependencyDerivedInputs`. The producer regenerates that recipe and the
conversion gate checks the original/COPY/manifest/native linkage. Unsupported
macros, operands, pictures, storage modes or inconsistent lengths retain explicit
partial-layout diagnostics. This is symbolic storage evidence, not terminal
rendering or target-runtime equivalence. Missing vendor copybooks, including
DFHAID and DFHBMSCA when required, still need genuine supplied definitions.

Patch `0026` retains PUT/GET CONTAINER with channel and FLENGTH operands,
DELAY FOR SECONDS, FETCH ANY/CHILD and RUN TRANSID instead of removing them in
preprocessing. Native declarations bind task-local data operands. RUN additionally
requires enabled, local, non-dynamic TRANSACTION and COBOL PROGRAM catalog entries,
the actual program-source hashes and complete recursive `CICS_RUN` analyses.
Targets may be literal transaction IDs or a conservative native proof: one
test-before inline PERFORM, a local single-digit counter with literal bounds
between 1 and 8 and step 1, and an unconditional first STRING of three literal
characters plus that counter into a local PIC X(4) target. Mutation, address/SQL
escapes, ambiguous declarations, remote routes and missing programs remain blocked.
This derives Bank-of-Z's OCR1–OCR5 routes from its supplied source and catalogs;
it does not hardcode a Bank routing table or accept arbitrary dynamic targets.
The parser restores DFHVALUE(NORMAL/ABEND/SECERROR) as symbolic expressions and
preserves FORMATTIME DATESEP arguments. No completion codes, channel adapter or
asynchronous target-runtime behavior is invented; runtime mappings remain unverified.

The producer, cache and conversion gate also reject original EXEC CICS/SQL operations
and DFHRESP/DFHVALUE expressions omitted
from native statement evidence. Comments, literals, inactive fixed-format debug
lines and documentary identification paragraphs do not establish operations.
This is a contradiction guard, never a source-scanning substitute for native
dependency analysis. Unsupported operations remain blocked rather than becoming
dependency-ready because a command disappeared.

`doctor.sh rekt-full` streams native parser and dependency-export stdout/stderr to
the real operation's `native.log` as well as each program's parse log. AI Loop
shows a bounded, refreshable tail; full logs remain on disk. A zero structural
parser exit or complete AST is not a dependency-completeness or conversion claim.

### Isolated native dependency integration checks

After building an isolated image tag, run the native opt-in checks from the
repository root, setting `REKT_TEST_IMAGE` to that tag:

```bash
REKT_TEST_IMAGE=rekt-oss-mma:latest python3 -m unittest \
  tools.tests.test_rekt_dependency_evidence.NativeDependencyExportTests
```

The checks create declared synthetic inputs, mount them read-only in disposable
containers, and bound each parser invocation. They do not read business sources,
invoke a model provider, or connect to Neo4j. Run them serially alongside graph
services; each container is limited to 1 GiB with a 512 MiB Java heap.

The controls cover hash-bound COPY/static CALL, parser-attested zero references,
dynamic CALL, missing definitions, typed CICS/SQL statements and operands, mixed
dialects, repeated statements, file-I/O contexts, and incomplete callees.
Empty file-section controls must succeed without I/O, while real file declarations
or operations remain blocked. Bare RETURN can complete dependency analysis;
optioned RETURN remains partial. Both retain unverified runtime mapping.
Additional controls cover supplied CICS files and their missing, duplicate,
dynamic and remote variants; declared SQL table/cursor/host bindings; absent
schemas/hosts; ambiguous table declarations; external SQL functions; and evidence
tampering. Built-in types/functions stay distinct from custom definitions, and
invalid SQL retains its original parser failure. SQL binding controls must pass
without authorizing target execution.
Success for a supported control does not establish coverage
for unsupported constructs or the authenticity of vendor copybooks. Legacy empty
exports remain partial after a rebuild until current evidence is regenerated.

## Separate named scan views

Keep a fresh scan's artifacts and source snapshot in a separate directory, such
as `output/rekt-scans/estate-v2/`, rather than replacing earlier output. After
generating the structural exports with the first parser, ingest just that
snapshot using the usual Neo4j environment settings:

```bash
python3 tools/graph-populator/populator.py ingest \
  --source-dir output/rekt-scans/estate-v2/inputs/parsed-source \
  --rekt-output output/rekt-scans/estate-v2/artifacts \
  --run-id "$(date +%s)" --scan-name estate-v2
```

A named scan requires an unused numeric run ID and existing input directories.
Ingestion records the scan's `scan.json` manifest path (by default the one beside
the artifacts directory, or `--scan-manifest`) so the portal can bind selected-snapshot
draft assessment to that exact evidence.
Its source snapshot must contain the exact prepared bytes the parser used, so
line ranges agree with the displayed source. Retain original bytes and
transformation provenance separately rather than relabeling normalized inputs as
unaltered source.
It refuses to overwrite a migration run or earlier scan, keeps its own
`RektScan` metadata, and leaves historical graph instances and volumes untouched.
If ingestion fails, partial data is retained with a failed status, not relabeled
as complete. A completed ingestion means graph loading finished, not that every
program parsed completely or that dependency evidence is verified.

The Architecture and AST Explorer scan picker displays the name. AST structure,
paragraph details and source content bind to the selected snapshot's run ID;
source comes from its stored blocks rather than a potentially newer disk file.
The Explorer uses one snapshot at a time and marks its evidence as structural
only. Missing definitions and unsupported dependency domains still block verified
conversion independently of whether an AST can be displayed. Persisted unresolved
dependency diagnostics remain visible beside the selected file, not hidden by
the presence of a structural graph.

## Files

- [`patches/README.md`](./patches/README.md): purpose of each local patch.
- [`LICENSE`](./LICENSE): local patches and image packaging.
- [`LICENSE.cobol-rekt`](./LICENSE.cobol-rekt): upstream cobol-rekt license.
- [`NOTICE`](./NOTICE): third-party components and licenses.
