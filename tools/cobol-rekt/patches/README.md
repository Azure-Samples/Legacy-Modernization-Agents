# Local cobol-rekt patches

These files modify the pinned upstream `cobol-rekt` version `v0.1.0-RC6`.
During the Docker image build, `../Dockerfile` applies them in filename order
with `git apply` and then compiles the patched source.

The patches keep recoverable COBOL inputs from aborting analysis while still
failing on parser errors that could produce misleading results.

| Patch | Purpose |
|---|---|
| `0001-lenient-parse-pipeline.patch` | Continue after warning-only diagnostics when a parse tree exists; keep errors fatal. |
| `0002-null-safe-data-division.patch` | Handle programs without a DATA DIVISION. |
| `0003-null-safe-entry-name.patch` | Treat unnamed or FILLER entries safely. |
| `0004-tolerate-unknown-class-condition.patch` | Preserve custom class conditions as explicit unknown nodes. |
| `0005-skip-null-ast-children.patch` | Skip null nodes in partially recovered ASTs. |
| `0006-safe-data-spec.patch` | Report an unknown layout explicitly instead of assigning invented one-byte storage to entries without a supported PIC clause. |
| `0007-tolerate-null-procedure-division.patch` | Create a reduced model when no procedure body is available. |
| `0008-skip-null-flow-writers.patch` | Skip AST and CFG output when the reduced model has no root node. |
| `0009-null-safe-data-matching.patch` | An unnamed data entry cannot match a record name; do not dereference its absent name. |
| `0010-cics-terminal-translation.patch` | Parse CICS `INQUIRE TERMINAL` and `SET TERMINAL` with `UCTRANST`, retaining dialect nodes and surrounding control flow. |
| `0011-missing-copybook-recovery.patch` | Opt-in recovery after missing-copybook-only errors; retain diagnostics beside artifacts and keep all other ERROR diagnostics fatal. |
| `0012-truthful-dependency-export.patch` | Export actual resolved COPY definitions and hashes from the existing parser repository, bind parsed source bytes, qualify COPY/CALL coverage, retain dependency failures, and propagate failed analysis/export status. |
| `0013-empty-file-section-and-cics-return-evidence.patch` | Distinguish empty native FILE SECTION headers from file declarations; record typed CICS RETURN control transfers and unresolved runtime mapping without claiming CICS absence or conversion equivalence. |
| `0014-native-dialect-dependency-contexts.patch` | Export restored CICS/SQL grammar trees, operands and coordinates; distinguish known dialects from opaque contexts and resource-free bare RETURN dependencies from unimplemented runtime behavior. |
| `0015-cics-file-resource-bindings.patch` | Bind literal local CICS WRITE FILE/DATASET operands to explicitly supplied YAML file definitions and hashes; retain real layout attributes and reject missing, duplicate, dynamic or remote definitions. |
| `0016-native-sql-declaration-bindings.patch` | Retain the existing DB2 semantic grammar tree and bind exact declared SQL tables/cursors and unique COBOL host declarations without guessing schemas or claiming runtime support. |
| `0017-explicit-dependency-profiles.patch` | Bind an explicitly configured, hash-bound target schema and documented CEEGMT/CEEDATM service contracts without inventing original deployment settings or COBOL service bodies. |
| `0018-native-floating-point-layouts.patch` | Recognize COMP-1/COMP-2 and COMPUTATIONAL-1/2 as numeric floating-point fields with actual four/eight-byte storage; retain unsupported native memory interpretation as an explicit error. |
| `0019-sql-host-indicator-boundaries.patch` | Require a colon on an SQL null-indicator reference so `FOR FETCH ONLY` is not consumed as a host variable and a second statement. |
| `0020-native-cics-local-control-bindings.patch` | Bind current-task CICS operands and local abend labels to native declarations; bind local LINK targets to supplied PROGRAM definitions and genuine recursively parsed source. |
| `0021-native-local-storage-cics-targets.patch` | Extend the same whole-program immutability proof to initialized LOCAL-STORAGE program operands. |
| `0022-bank-cics-qualified-bindings.patch` | Bind OF/IN-qualified data using native declaration ancestry, retain named ENQ/DEQ operations, resolve space-padded program literals within eight bytes, and recognize the Bank response symbols without inventing numeric values. |
| `0023-language-environment-date-contracts.patch` | Accept explicit, documented CEEDAYS and CEELOCT contracts alongside CEEGMT/CEEDATM; retain hash-bound contracts rather than invented COBOL sources or verified target implementations. |
| `0024-explicit-shared-sql-declarations.patch` | Parse explicitly supplied shared SQL table declarations with the existing DB2 grammar, retaining their paths, raw hashes and declaration syntax without importing host variables or cursors. |
| `0025-grounded-symbolic-map-layouts.patch` | Bind generated symbolic COPY layouts to an explicit supported BMS recipe, original BMS snapshot and hash-bound layout manifest; incomplete or changed derivations remain blockers. |
| `0026-native-async-cics-transactions.patch` | Retain channel containers, DELAY, FETCH and asynchronous RUN; bind finite local transaction routes to real catalogs and recursively parsed programs, and restore symbolic completion expressions without inventing their numeric values. |
| `0027-symbolic-platform-copy-contracts.patch` | Bind explicitly selected analysis-only DFHAID event projections to named-event contracts and native predicates, retaining target implementation requirements rather than vendor bytes or ordinary storage. Aggregate use, writes and unsupported predicates remain blockers. |
| `0028-cics-terminal-and-continuation-bindings.patch` | Bind supported SEND CONTROL/TEXT/MAP, RECEIVE MAP, terminal translation, HANDLE ABEND CANCEL, BIF DEEDIT and RETURN transaction continuations; retain grounded BMS layouts, declaration evidence and symbolic terminal settings without claiming target-runtime equivalence. |
| `0029-bms-attributes-and-association-bindings.patch` | Parse and bind bounded INQUIRE ASSOCIATION origin-data operands and EIBTASKN context; bind explicitly selected DFHBMSCA attribute projections to real BMS output fields. Preserve raw character/figurative assignments, and classify ambiguous standalone DFHAID comparisons as UNRESOLVED_CICS_AID_SEMANTIC without changing their behavior. |
| `0030-aid-conjuncts-with-application-relations.patch` | Accept `EIBAID = <named event>` ANDed only with complete application relations (for example `EIBAID = DFHENTER AND COMM-UPD = 'Y'`) and record each event equality as an `application-and-conjunct` usage. OR mixing, abbreviated operands, negated event operands and standalone DFHAID remain blocked. |
| `0031-cics-program-dynamic-default.patch` | Treat an omitted `dynamic` on an existing CICS `program` resource as IBM's documented default `DYNAMIC(NO)` and record `dynamicProvenance: cics-default` on the binding. Explicit `NO` stays as-is; explicit `YES` and any other value are rejected. A missing PROGRAM definition still reports `cics-program-definition-not-found`. |
| `0032-condition-name-in-combined-conditions.patch` | Stop the flow builder from throwing a NullPointerException when a level-88 condition-name appears in an AND/OR chain (for example `WHEN FLG-A AND FLG-B` in `EVALUATE TRUE`). Per COBOL abbreviated-condition rules, only a complete relation supplies the subject and operator for later abbreviated relations, so condition-names leave them unchanged. Conditions that already built are unaffected. |
| `0033-evaluate-eibaid-and-supplied-bms-maps.patch` | Bind `EVALUATE EIBAID` with `WHEN <named AID event>` branches as DFHAID usages: one subject usage followed by one usage per WHEN. ALSO, multi-subject and multi-event WHENs stay blocked. Bind qualified (`X OF MAPO/MAPI`) and multi-target DFHBMSCA MOVEs to every target field, using the full DFHBMSCA attribute, colour and highlight symbol set. Bind BMS layouts for application-supplied symbolic-map copybooks (for example CardDemo `cpy-bms`) when the preprocessor has proven them token-equivalent to their `.bms` source. |
| `0034-native-file-control-bindings.patch` | Bind native COBOL file statements (`OPEN`, `CLOSE`, `READ`, `WRITE`, `REWRITE`, `DELETE`, `START`) to the `SELECT … ASSIGN` entry and `FD` that declare them, and export each as a `FILE_IO` statement with a `COBOL_FILE` resource binding (assignment, organization, access mode, record names and the parsed SELECT/FD syntax). FILE_IO coverage becomes complete with presence evidence `antlr-tree:bound-file-control-resources` only when every statement binds. Undeclared files or records, a missing FD, dynamic `ASSIGN`, SD/SORT/MERGE and I-O-CONTROL stay blocked. An empty `FILE-CONTROL` paragraph is not file I/O. The ASSIGN target's runtime dataset stays `unverified`. |
| `0035-cee3abd-runtime-contract.patch` | Accept a documented Language Environment `CEE3ABD` (terminate enclave with abend) contract in a dependency profile, with exactly its two documented input parameters (abend code, clean-up). As with CEEGMT/CEEDATM/CEEDAYS/CEELOCT, the call becomes a hash-bound `EXTERNAL_RUNTIME` leaf with `runtimeMapping: unverified`, not a parsed COBOL implementation. |
| `0036-cics-constant-operands-and-csd-resources.patch` | Resolve `RETURN TRANSID` and `SEND`/`RECEIVE MAP`/`MAPSET` operands that name an immutable WORKING-STORAGE/LOCAL-STORAGE `PIC X(n) VALUE 'literal'` item (plain named groups only; no OCCURS, REDEFINES, level 88, SQL host use, `ADDRESS` or any write/pass reference) and export the definition/ancestor syntax as `targetResolution` proof. Read IBM DFHCSDUP definition files (any non-YAML path) strictly; a CSD PROGRAM with blank LANGUAGE and `DYNAMIC(NO)` is recorded as Language Environment COBOL with `languageProvenance: cics-blank-language-environment`. A declared `SELECT`/`FD` with no bound file statement now stays unresolved instead of claiming FILE_IO presence. Runtime-assigned names still require dataflow and stay unresolved. |
| `0037-cics-file-idcams-and-xctl-bindings.patch` | Bind keyed CICS file commands (`READ`, `READNEXT`/`READPREV`, `STARTBR`, `ENDBR`, `WRITE`, `REWRITE`, `DELETE`, `UNLOCK`) whose `FILE`/`DATASET` is a literal or provably read-only constant. A CSD FILE without record/key lengths is completed only from a supplied IDCAMS `DEFINE CLUSTER`/`ALTERNATEINDEX`/`PATH` (JCL `PGM=IDCAMS` instream SYSIN, listed in `REKT_VSAM_DEFINITIONS`); only KSDS bases are accepted, conflicting duplicates are rejected and every definition's path/SHA256 is exported as `datasetDefinitions`. The CSD ADD/BROWSE/DELETE/READ/UPDATE services (IBM defaults when omitted) must permit the operation. `SYSID`, RBA/RRN, `GENERIC`, `SET`, `TOKEN`, `MASSINSERT` and `REQID` stay unresolved. `XCTL PROGRAM` binds a literal/constant CSD program as `transfer-control` without adding a call edge; `CHANNEL`/`INPUTMSG` and runtime-assigned names stay unresolved. `DFHRESP(ENDFILE/DUPREC/DUPKEY)` are resource-free conditions. |
| `0038-cics-inquire-program-and-menu-table-targets.patch` | Add the SPI `INQUIRE PROGRAM(...)` form to the CICS grammar and bind it as `program-definition-inquiry` against the CSD. Resolve `XCTL`/`INQUIRE PROGRAM` operands of the form `NAME(SUB)` where `NAME` is a `PIC X` member of the sole `OCCURS` child of a `REDEFINES` over a same-level group of `PIC X(n)`/`9(n)` `VALUE` items, and every reference to the table, redefined area and value items is read-only. Each defined occurrence yields one `CICS_PROGRAM` binding sharing an `immutable-redefined-table-values` proof; occurrences beyond the `VALUE` area are recorded as `null`, partial overlaps and non-name values reject. Subscripted CICS data references bind with `qualificationPath` and `subscriptSyntax`. |
| `0039-cics-xctl-structured-reaching-values.patch` | Resolve `XCTL PROGRAM(field)` with an intra-program structured reaching-definition analysis (sentences, `IF`, `EVALUATE`, `PERFORM` paragraph/section/inline). Bind every literal/constant `MOVE` that reaches the XCTL; blanking `MOVE SPACES`/`LOW-VALUES`/`INITIALIZE` contribute `null`. Unresolved when any other write, `GO TO`, `ALTER`, `HANDLE`, `THRU`, `DECLARATIVES`, `REDEFINES`/88-level aliasing, SQL host use or `ADDRESS` touches the analysis. |
| `0040-cics-link-constants-and-db2-zos-builtins.patch` | `LINK PROGRAM(field)` falls back to the shared read-only constant/`OCCURS`-table proofs (group members, not only 01-level items) and records `controlEffect: call` plus static call targets. Structured reaching values stay `XCTL`-only. Unqualified `TIMESTAMP_FORMAT`, a documented Db2 13 for z/OS built-in missing from `dbs_inbuild_functions`, is no longer reported as an external SQL function. |
| `0041-cics-commarea-slots-bms-overlay-and-documented-runtime-calls.patch` | COMMAREA slot inventory and closure across `XCTL`/`LINK`; symbolic map overlays (preprocessor `FIL-R` renames of `FILLER REDEFINES`) with length/attribute/extended-attribute offsets; single-map reaching proofs for `SEND`/`RECEIVE MAP`; plain `SEND FROM`, `HANDLE CONDITION` labels and `WRITEQ TD` to CSD-defined extrapartition queues; `LENGTH OF` as a special register. Dynamic `CALL identifier` is replaced by a static edge only when the identifier is a proven read-only constant naming a `documented-platform-service` runtime contract in the estate profile (for example `DSNTIAC`); other constants stay dynamic. CALLs nested in `EVALUATE` are read from the parse tree. `SYSIBM.SYSDUMMY1` is a documented Db2 catalog table. |

Patch 0028 completes dependency bindings for the listed grammar variants, not
every CICS command. MAP/MAPSET names must resolve uniquely to a supported,
hash-bound BMS layout, including its original mapset label and direction.
Omitting MAPSET means the mapset name defaults to the MAP name; the exporter
does not guess a different map or mapset. Dynamic names, missing/ambiguous maps,
unsupported variants and missing data declarations remain blockers.

RETURN TRANSID binds an enabled, explicitly local transaction and its supplied
program source. It is exported as a transaction continuation, not a CALL,
LINK or RUN dependency: returning to a transaction does not synchronously call
its program. `CICS_CONTINUATION` and `CICS_CONTINUATION_SOURCE` track the selected
input and original destination source without claiming a recursively parsed
callee body or an implemented target route. Actual CALL/LINK/RUN closure is
unchanged. COMMAREA, LENGTH, IMMEDIATE, terminal options and response operands
remain in the typed implementation requirement.

EIBTRMID is bound only as terminal-context input in the supported INQUIRE/SET
forms. UCTRAN, TRANIDONLY and NOUCTRAN remain named DFHVALUE symbols; no numeric
encodings, vendor copybook bytes or successful terminal no-ops are supplied.

The first eight patches originated in `origin/preview`. The warning-only
pipeline, unknown-class handling and null-flow writers have stricter guards than
preview; do not overwrite them with the older variants.

`REKT_ALLOW_MISSING_COPYBOOKS=true` enables partial parser recovery, not missing
definition synthesis. `doctor.sh` enables it for analysis commands and records
`Partial` outcomes with missing-copybook warnings. Standalone parser runs remain
strict by default. `parse-diagnostics.json` preserves diagnostic codes, messages,
severity and source locations, including failed attempts. Recovery still requires
a parse tree and may fail if downstream analysis needs an unavailable definition.

The upstream `dependency` command only walks external control-flow nodes; it does
**not** export COPY usage. It also catches analysis failures and can return an empty
tree with exit 0. Patch `0012` fixes the producer rather than filling that tree from
a source scanner: it snapshots `AnalysisContext.getCopybooksRepository()` and
walks CALL nodes in the same parser tree. Resolved nodes include actual input
paths and raw SHA256 hashes. Dynamic CALL targets remain unresolved; other
external-control transfers are explicitly distinct from standard CALL.
The same full ANTLR tree is examined before and after dialect restoration.
A clean tree without dialect, foreign, or error contexts can attest **zero**
`EXEC_CICS` and `EXEC_SQL` dependencies. File-I/O zero coverage additionally
requires no native I/O statements or file-declaration contexts. These absence
attestations are exported in `dependencyAbsenceEvidence`, not inferred from
source text or exit status. Present/unknown dialect contexts and present file
I/O remain explicitly `partial` with unsupported-context diagnostics; this
patch does not invent their edges or pretend to export their semantics.
The command only returns success after all required root coverage and every
non-COPY callee analysis are complete, with no unresolved issues. An incomplete
callee propagates its unsupported domains and diagnostics to the caller;
dynamic/missing callees cannot hide behind a clean root parse.

Patch `0013` by itself does not waive unsupported CICS evidence. It records the actual
`Cics_returnContext`, including options and explicitly expanded-parser coordinates, rather than
inventing a target or translating it into an ordinary language return. The
dependency command still fails for these unsupported dialect contexts. Only a
typed `FileSectionContext` with no file-description entries stops counting as a
file declaration; its descendants and all other statement contexts are still
visited. Real SELECT/FD declarations, file operations, parser errors and opaque
dialects remain blockers. The strict conversion gate is unchanged.

Patch `0014` examines the restored native grammar, not the temporary dialect
placeholders. `dependencyDialectStatements` retains command/SQL operation,
grammar rule, source text, parser coordinates, nested grammar/token syntax,
binding status and unverified runtime mapping. SQL restoration can attach the
same parse context more than once; identity-based deduplication retains one
statement without collapsing identical statements at different source locations.
Unrestored placeholders, unknown dialects and parser errors still block coverage.

Known CICS is not evidence of SQL, and known SQL is not evidence of CICS. Their
separate absence attestations are `antlr-tree:no-sql-contexts` and
`antlr-tree:no-cics-contexts`; completely dialect-free trees retain the old proof.
Bare `Cics_returnContext` has no resource operands, so it can establish positive
dependency coverage through
`dependencyPresenceEvidence.EXEC_CICS: "antlr-tree:resource-free-cics-return"`.
The consumer checks the retained bare-RETURN syntax rather than mistaking this
for absence. Optioned RETURN, LINK, SQL statements and other unresolved bindings
remain partial. Real file declarations and I/O remain blockers.

Dependency completeness is not runtime support: RETURN still records
`runtimeMapping: "unverified"`. The conversion gate validates source dependencies
and passes complete native CICS/SQL records to generation as implementation
requirements; it does not demand pre-existing target adapters. This patch itself
neither converts commands to Java nor invents SDK definitions, callees, map layouts
or SQL schema. Runtime behavior still requires separate verification.

Patch `0015` reads CICS YAML resource catalogs from the explicitly configured
`REKT_CICS_RESOURCE_DEFINITIONS` environment variable. Paths are container-visible,
separated by the platform path separator (`:` in the Linux parser). `doctor.sh`
forwards a host value when explicitly set; otherwise the container configuration
is retained. Safe YAML loading rejects duplicate keys and collection aliases.
Only literal, local WRITE FILE/DATASET commands can bind a `CICS_FILE` resource:
the native export records the supplied definition path, raw SHA256, dataset name,
record length and key length. Missing, ambiguous, invalid, dynamic and SYSID
bindings remain failures. `DFHRESP(NORMAL)` requires no external resource; the
patch does not assign it an invented runtime value. File I/O uses positive
`antlr-tree:bound-cics-file-resources` evidence, never an absence claim. The
consumer validates grammar, names, attributes and the unchanged source definition.

Patch `0016` retains the `StartSqlRuleContext` already parsed by the DB2 dialect
visitor on its original SQL context. It does not run another SQL parser or
extract dependencies from raw SQL tokens. `semanticSyntax` preserves that nested
grammar; `resourceReferences` and `resourceBindings` record actual reference kinds,
names and declaration syntax. Bindings use `definitionScope: "expanded-parser-input"`;
they are declarations in the parsed program, not an
invented database catalog or original-file source locations.

The DB2 grammar prefers its actual built-in column-type and function rules over
the broad identifier alternatives. Standard types such as INTEGER, SMALLINT and
DATE therefore do not become invented custom-type dependencies; genuinely named
custom types/functions remain unresolved. ABS is recognized by its native
function rule while remaining usable as an identifier where the grammar allows.

Supported dependency operations are DECLARE TABLE/CURSOR, SELECT, INSERT, UPDATE,
DELETE, OPEN, FETCH, CLOSE, COMMIT and ROLLBACK. Table/view and cursor names require
one exact declaration; COBOL host variables require one native data declaration.
Unknown names, duplicate declarations, unverified nested-program scopes, sequence
references, user functions, user-defined types and prepared statements remain
unresolved. No implicit schema or vendor routine is invented. Preprocessing
retains DECLARE CURSOR and its query rather than commenting it out. SQL uses
positive `antlr-tree:bound-sql-resources` evidence for SQL and SQL-only file I/O;
mixed CICS file I/O also requires valid SQL bindings. The consumer independently
checks reference syntax and declaration membership before accepting these proofs.
Parsing/binding does not validate a live database schema, SQL execution or target
transaction behavior: every runtime mapping remains unverified.

Patch `0017` loads only the explicitly configured `REKT_DEPENDENCY_PROFILE` file.
For an otherwise-unbound unqualified SQL relation, its `sqlDefaultSchema` can
select an actual qualified parsed declaration. Exact declarations take precedence;
wrong schemas, duplicate matches and missing declarations remain unresolved.
The checked-in Bank-of-Z profile selects STTESTER for the new Spring/H2 target,
not as a claim about the original DB2 runtime.

Static CEEGMT/CEEDATM calls can bind supplied IBM Language Environment contracts
after an actual COBOL source search finds no definition. These `EXTERNAL_RUNTIME`
leaves record `dependencyAnalysisComplete: false`, `runtimeContractBindingComplete: true`
and `runtimeMapping: "unverified"`. They do not claim a parsed body, source hash
for an invented program, or dialect-absence proofs. Consumers verify the actual
profile file hash and matching contract payload, retain CALL edges, and pass the
contract to generation as an implementation requirement. Unknown/dynamic calls
remain unresolved; changing the profile invalidates cached evidence.

Patch `0018` preserves native floating-point declarations instead of preprocessing
them into alphanumeric PIC fields. Native usage, numeric classification and
four/eight-byte layout are retained, including the COMPUTATIONAL aliases.
This is parser/layout support, not a floating-point emulator: reading or executing
the native memory representation still fails explicitly because its encoding
requires compiler/runtime settings. No IEEE or IBM hexadecimal encoding is guessed.

Patch `0019` gives SQL null indicators their own grammar rule and requires their
colon, including after `INDICATOR`. Real indicators remain host-variable
references; the read-only cursor suffix `FOR FETCH ONLY` stays in its cursor
declaration instead of becoming an invented variable FOR and cursor ONLY.

Patch `0020` resolves data operands for ASSIGN, ASKTIME, FORMATTIME, ABEND and
SYNCPOINT against unique native COBOL declarations. HANDLE ABEND LABEL must
name an actual local paragraph or procedure section. Nested-program scopes,
missing/ambiguous declarations and unsupported HANDLE variants remain blockers.
These task operations do not name an external resource; their runtime effects,
including transaction rollback, are still unverified implementation requirements.

Local CICS LINK additionally requires an explicitly supplied PROGRAM definition
with `language: COBOL`, `dynamic: "NO"`, no remote-routing attributes, and exactly
one genuine callee source. Its catalog and source hashes are retained, and its
`CICS_LINK` callee is recursively analyzed; an incomplete callee blocks its caller.
A variable program operand is accepted only for an exact working-storage
`01 ... PIC X(8) VALUE '...'` declaration with a whole-program native usage proof:
no use outside its declaration and CICS PROGRAM operands, no SQL host reference,
and no ADDRESS tokens. An unrelated pointer declaration does not expose the
target's address. Initial VALUE alone is never sufficient. The native LINK
grammar also permits a PROGRAM operand without optional parameters.
Other dynamic targets, SYSID, CHANNEL, INPUTEVENT and ACTIVITY variants remain
unresolved. This patch supplies neither missing vendor copybooks nor a Java CICS
runtime and does not prove legacy behavior equivalence.

### 0021 - Initialized LOCAL-STORAGE CICS program operands

DBCRFUN keeps its `ABNDPROC` program name in LOCAL-STORAGE, not WORKING-STORAGE.
Accept that initialized storage class under the same whole-program no-write proof
as `0020`, and export `immutable-local-storage-literal` rather than mislabelling it
as WORKING-STORAGE. LOCAL-STORAGE's VALUE is initialized for each invocation.
LINKAGE, missing/ambiguous declarations, mutation, SQL host references and ADDRESS
escapes remain unresolved. The evidence consumer recognizes the new native proof
kind without adding a regex-based resolution fallback. No business source or
dependency-completeness requirement is changed.

### 0022 - Qualified data, padded program names and named locks

Qualified data operands resolve against native data levels and named ancestors,
including skipped intermediate ancestors. Duplicate bare names, missing qualifiers,
nested programs, and unsupported subscript/reference-modifier expressions remain
unresolved. Bindings retain the declaration path; the evidence producer verifies
the field, ancestor names, levels and syntax rather than accepting a concatenated
identifier as proof.

ENQ/DEQ require a RESOURCE operand and retain the complete command, lifetime,
length and response syntax with `parsed-cics-lock-operation` bindings. Dependency
binding does not implement region/global lock behavior or transaction lifetime.
Program literals may have trailing space padding only within eight bytes;
internal spaces, oversized literals, mutable operands and remote variants still
block. DFHRESP supports NORMAL, INVREQ, MAPFAIL, NOTFINISHED and NOTFND as symbolic
requirements; no numeric response codes are manufactured.

### 0023 - Documented date services

The versioned `contracts/bank-of-z-spring-h2-v2.json` profile adds CEEDAYS and
CEELOCT using IBM's documented parameter and feedback contracts. It preserves
the original profile file so existing hash-bound evidence remains verifiable.
The native parser, Python evidence producer and .NET gate recognize these service
contracts together. The original runtime time zone, century window and compiler
date options are not guessed. Target adapters still need implementation and
behavioral verification.

### 0024 - Explicit shared SQL declaration catalogs

`REKT_SQL_DECLARATIONS` lists explicitly supplied, container-visible declaration
files using the platform path separator. The existing DB2 grammar parses DCLGEN
`EXEC SQL DECLARE ... TABLE ... END-EXEC` envelopes or standalone `.sql` table
declarations. Bindings retain the exact declaration syntax, absolute source path
and raw SHA256. Local declarations take precedence; shared duplicates remain
ambiguous, non-declaration SQL is rejected, and catalogs cannot supply host
variables or cursors. Profile-based default schemas still require a matching
real declaration. The evidence producer and conversion gate require within-source,
hash-bound `SQL_DECLARATION` inputs; merely finding a file is not a binding.

### 0025 - Grounded symbolic BMS storage

Generated symbolic COPY definitions can bind a supported
`bms-symbolic-map/layout-v1` manifest and original BMS snapshot. The native
export retains their hashes alongside the actual parsed COPY hash in
`dependencyDerivedInputs`. The Python producer independently regenerates the
supported recipe from the original BMS; cache checks use persistent inputs,
not deleted staging snapshots. The conversion gate requires matching
`BMS`, `BMS_LAYOUT`, COPY and native derivation bindings.

The supported subset includes COBOL/COBOL2 symbolic maps, STORAGE=AUTO with
its default TIOA prefix, non-AUTO map overlays, supported extended attributes
and checked PIC/field lengths. Unsupported layout-affecting constructs remain
explicitly partial. This does not supply missing vendor SDK copybooks, emulate
terminal rendering, or establish equivalent generated behavior.

`doctor.sh` tags the image `rekt-oss-mma:<digest>`, where the digest covers the
Dockerfile, licence files and every patch. When the running `cobol-rekt` container
uses a different image, `doctor.sh` rebuilds and recreates only that container, so a
new or changed patch can never be skipped by reusing a stale parser.

An existing running image does not acquire these patches automatically. Rebuild and
run the opt-in dependency integration tests against an isolated image before
trusting native coverage metadata:

```bash
REKT_TEST_IMAGE=<isolated-image-tag> python3 -m unittest tools.tests.test_rekt_dependency_evidence.NativeDependencyExportTests
```

These tests use declared synthetic COPY/static CALL, dynamic CALL, missing COPY,
and zero-reference controls, with source mounted read-only. They do not use
business-source fixtures, a model provider, or Neo4j.

`0007` and `0008` depend on each other and must remain synchronized.

If the upstream version changes, verify every patch with `git apply --check`
and remove patches whose fixes are already available upstream.

After rebuilding the parser image, run the opt-in integration regression:

```bash
REKT_TEST_IMAGE=rekt-oss-mma:latest python3 -m unittest tools.tests.test_rekt_terminal_parser tools.tests.test_rekt_missing_copybook_parser
```

It starts disposable containers over synthetic fixtures only; it does not use a
developer's source folder, model provider or Neo4j instance.
