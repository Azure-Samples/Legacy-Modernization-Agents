# P1 A/B Validation Protocol

**Last updated**: 2026-05-28
**Status**: **WAITING FOR LIVE RESULTS.** Feature work (PR4.c, PR5) is paused
until the table below is filled in and the decision recorded.

This is the validation gate for the program-facts projection (PR4.a + PR4.b).
The goal is to confirm that swapping the raw-AST REKT context for the curated
`program-facts.json` projection **preserves conversion quality** while
**materially reducing prompt size**.

## Why this gate exists

P0–PR3 are infrastructure (caches, schemas, extractor). PR4.a + PR4.b started
using `program-facts.json` instead of raw AST in the Java and C# converter
prompts. The projection is ~60–80% smaller in tokens — but smaller is only
useful if generated code is **at least as correct** as the raw-AST baseline.

If projections regress quality:
- PR4.c (other agents) waits until the projection content is refined.
- PR5 (preprocessor `.preprocess.json` writer) waits because PR5 surfaces
  preprocess notes via the projection — if the projection is broken, PR5's
  additions can't be properly evaluated either.

If projections preserve quality:
- PR4 architecture is validated.
- PR4.c can mirror the pattern to the remaining agents confidently.

## Program selection

Per the gate spec: **3–5 small + 1 medium + 1 high-complexity / ugly**.
Pick from your corpus following these criteria:

| Bucket | Count | Selection guidance |
|---|---|---|
| Small | 3–5 | < 200 LOC, ≤ 5 paragraphs, ≤ 2 copybooks, no SQL or CICS |
| Medium | 1 | 200–800 LOC, multiple sections, ≥ 1 SQL or file IO |
| High-complexity / ugly | 1 | ≥ 800 LOC OR known to fall to "deps only" REKT outcome OR has IDMS/IMS/CICS or non-standard column formats |

Record the chosen set:

```
small  : ____________, ____________, ____________
medium : ____________
ugly   : ____________
```

## Preconditions

Before starting:

1. **doctor.sh rekt-full has run successfully** (`output/rekt/*.json` populated). If not, run:
   ```sh
   _REKT_INCREMENTAL=true _PROGRAM_FACTS=true ./doctor.sh rekt-full
   ```
   This also generates `*.facts.json` for every program in one shot.

2. **Project is built**:
   ```sh
   dotnet build CobolToQuarkusMigration.csproj
   ```

3. **LLM credentials configured** (Azure OpenAI key/Entra ID or GitHub Models token via `doctor.sh setup`).

4. **For cold-cost A/B, response cache is empty**:
   ```sh
   rm -f Data/llm-cache.db Data/llm-cache.db-wal Data/llm-cache.db-shm
   ```
   The suite runner clears it between programs automatically.

## How to run the suite

```sh
# Single-shot, all programs, cold cache between programs, target=java
tools/ab-projection-suite.sh \
    --programs "PROG1,PROG2,PROG3,PROG4,PROG5,PROG_MED,PROG_UGLY" \
    --target java

# Or from a list file
cat > tools/ab-list.txt <<EOF
# 3-5 small
PROG1
PROG2
PROG3
# 1 medium
PROG_MED
# 1 ugly
PROG_UGLY
EOF
tools/ab-projection-suite.sh --program-file tools/ab-list.txt --target java
```

Repeat with `--target csharp` after the Java run to validate PR4.b too.

Output lands in `tools/ab-results-YYYYMMDD-HHMMSS/` (or `--output-dir` of your
choice). Each program gets its own subfolder with `baseline.log` and
`projection.log`.

Estimated wall time per program: cold baseline + cold projection ≈ 8–12 min
each for gpt-5.3-codex on medium programs. Plan ~2 hours for the full suite.

## What the suite captures automatically

The suite runner emits a CSV and a markdown table covering:

- wall clock time per leg
- primary-call input tokens
- primary-call total tokens
- input / total token savings %
- cache decision per leg (should be `miss` then `stored` for cold)
- per-program leg status

Paste the `suite-summary.md` table into the §Automated metrics section below
when complete.

## What requires manual inspection

The automated table does **not** cover code-quality dimensions. For each
program, score the projection against the baseline using this rubric, then
fill in the §Per-program manual review section below.

### Manual review checklist per program

| Dimension | Method | Pass criterion |
|---|---|---|
| **Compile success** | `cd output/<run>/java && javac -d /tmp/out *.java` (or `dotnet build` for C#) | Both legs compile, or neither does (regression = projection fails where baseline passed) |
| **DTO / entity correctness** | Diff DTO class fields between legs; cross-check against `program-facts.json → data.groups` | Projection ≥ baseline coverage of 01-level groups; field types match COBOL PIC clauses |
| **CALL / service correctness** | Diff service interface / @Inject lists; cross-check against `program-facts.json → callees` | Projection emits an interface for every callee in facts; no extra invented services |
| **Imports / packages** | `head -20 output/<run>/java/*.java` | Sensible imports; no missing imports for types actually used; reasonable package name |
| **Hallucinations / drift** | Diff full output between legs | Projection does NOT invent fields, methods, classes, SQL ops, or CALL targets absent from facts |
| **Overall structure** | Visual scan + line-count comparison | Structure is recognisably the same program; method count comparable |

Useful diff commands:

```sh
# Same program, both legs (after copying outputs aside):
diff -u baseline/<Class>.java projection/<Class>.java

# Field-level inspection
grep -E '^\s*private|^\s*public.*get|^\s*public.*set' baseline/<Class>.java | sort
grep -E '^\s*private|^\s*public.*get|^\s*public.*set' projection/<Class>.java | sort

# CALL/service inspection
grep -E '@Inject|implements .*Service' baseline/*.java
grep -E '@Inject|implements .*Service' projection/*.java

# Cross-check against facts
cat output/rekt/<STEM>.facts.json | jq '.data.groups, .callees, .io'
```

## Decision rule

After all programs are scored:

| Result pattern | Decision |
|---|---|
| **All programs**: compile parity AND DTO/CALL parity AND hallucination ≤ baseline AND structure parity, with input-token savings ≥ 40% | **Proceed to PR4.c.** Architecture validated. |
| **≥ 1 program**: projection compiles where baseline failed, or projection produces materially worse output | **Pause + refine projection.** Identify the failure category (data section, IO section, control flow, etc.), fix in `JavaConverterProjection` / `CSharpConverterProjection`, rerun the affected program. |
| **Tokens roughly equal** (< 20% savings) | Investigate facts file: usually means low-confidence parse → sparse facts. Consider improving REKT parse coverage (PR5) before declaring PR4 done. |
| **Latency materially worse** (despite fewer tokens) | Check the reasoning effort the model chose — fewer tokens can occasionally trigger higher reasoning effort. Acceptable if quality holds. |

---

## Recording the results

Fill in the sections below as you go. Treat this file as the single source of
truth for the decision.

### Suite metadata

- **Date run**: `YYYY-MM-DD`
- **Operator**: `name`
- **Model (code)**: `gpt-5.3-codex` / `claude-opus-4.7` / other
- **Provider**: `azure-openai` / `github-copilot-sdk`
- **Cache state**: cold (Data/llm-cache.db cleared between programs)
- **Output dir**: `tools/ab-results-YYYYMMDD-HHMMSS/`

### Automated metrics

**Paste the contents of `suite-summary.md` here:**

```
(awaiting suite run)
```

### Per-program manual review

For each program: copy the template, fill in the cells, replace `?` with
`✅` / `❌` / `≈` as appropriate.

#### Program: `<NAME>` (bucket: small / medium / ugly)

| Dimension | Baseline | Projection | Notes |
|---|:-:|:-:|---|
| Compile success | ? | ? | |
| DTO / entity correctness | ? | ? | |
| CALL / service correctness | ? | ? | |
| Imports / packages | ? | ? | |
| Hallucinations / drift | ? | ? | |
| Overall structure | ? | ? | |
| Verdict | — | parity / regression / improvement | |

(repeat for each program)

### Decision

- [ ] **Proceed to PR4.c** — projection preserves quality with material token savings.
- [ ] **Refine projection** — record the failure mode below and fix before expanding.
- [ ] **Refine REKT first** — facts are too sparse to validate; PR5 first.

#### If refinement needed, what to change

```
(record the projection / facts schema delta needed)
```

#### Materialised token savings

```
average input-token savings   : __%
average total-token savings   : __%
average wall-time delta       : __%
```

---

## What happens next

- If **proceed** is selected: PR4.c (reverse-engineering / architecture / BIAN agent projections) and PR5 (preprocessor `.preprocess.json` writer) unblock. Each is small and orthogonal.
- If **refine** is selected: open a focused PR against `JavaConverterProjection` / `CSharpConverterProjection` per the recorded failure mode. Rerun the affected programs only with the suite. Update the decision section. Repeat until proceed.
- Either way, the live numbers in §Automated metrics + §Per-program manual review become the canonical evidence the projection architecture works.

## Risks during validation

1. **gpt-5.3-codex reasoning time variance.** Same prompt twice can produce different wall times — the 480 s LLM-call timeout (`LlmCallTimeout`) is the upper bound. If you see one leg time out, rerun that program alone.
2. **Response cache mid-suite.** If you forget `--keep-cache` was off, the second leg may hit the first leg's cached output and report `cache=hit`. Always check the `cache decision` column.
3. **Toggling `_USE_PROGRAM_FACTS` mid-suite** changes the cache key. The suite's cold-cache mode handles this; manual single-program A/B without clearing the cache will mis-attribute hits.
4. **Manual review subjectivity.** Two reviewers may disagree on "structure parity" for borderline programs. The diff-based rubric above is the deciding evidence — if the diff is clean, that's parity.
5. **Sample size of 5–7 programs is small.** A regression that appears only on rare COBOL constructs may not surface. Acceptable for the gate; widen the suite if PR4.c proves the easy cases and ambiguity remains.
