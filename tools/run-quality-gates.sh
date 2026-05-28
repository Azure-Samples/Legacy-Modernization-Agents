#!/usr/bin/env bash
# ─────────────────────────────────────────────────────────────────────────────
# run-quality-gates.sh — post-suite quality + analytics report
#
# After a completed conversion (single or A/B suite), this script:
#   1. Runs tools/check-compile.sh against output/java for the latest N runs
#   2. Ingests all metrics into Data/benchmark.db
#   3. Prints a consolidated report
#
# Usage:
#   tools/run-quality-gates.sh                  # latest 1 run
#   tools/run-quality-gates.sh --runs 10        # latest 10 runs
#   tools/run-quality-gates.sh --from 29 --to 38  # specific run range
# ─────────────────────────────────────────────────────────────────────────────
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
N=1
FROM=""
TO=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        --runs)  N="$2"; shift 2 ;;
        --from)  FROM="$2"; shift 2 ;;
        --to)    TO="$2"; shift 2 ;;
        --help|-h)
            grep -E "^#" "$0" | sed 's/^# \?//'
            exit 0
            ;;
        *) shift ;;
    esac
done

echo "═══════════════════════════════════════════════════════════════════════════"
echo "Post-suite quality gates"
echo "═══════════════════════════════════════════════════════════════════════════"

# Resolve target run range
if [[ -n "$FROM" && -n "$TO" ]]; then
    RUNS=$(seq "$FROM" "$TO")
elif [[ -n "$FROM" ]]; then
    RUNS=$(sqlite3 "$REPO_ROOT/Data/migration.db" \
        "SELECT id FROM runs WHERE id >= $FROM ORDER BY id")
else
    RUNS=$(sqlite3 "$REPO_ROOT/Data/migration.db" \
        "SELECT id FROM runs WHERE status='Completed' ORDER BY id DESC LIMIT $N" | tac)
fi

if [[ -z "$RUNS" ]]; then
    echo "(no runs matched — nothing to gate)"
    exit 0
fi

echo "Runs to gate: $(echo $RUNS | tr '\n' ' ')"
echo ""

# Quality gate per run — points at the same output/java dir but tags each event
# with the appropriate run id. Subsequent ingestion deduplicates by (file, line).
for run in $RUNS; do
    echo "─── Run $run ───"
    # check-compile.sh exits 1 on compile failure; that's an expected outcome
    # for a quality gate, not a script error. Don't let it abort the loop.
    "$REPO_ROOT/tools/check-compile.sh" --run "$run" 2>&1 \
        | grep -E "compile|generated|fallback|@Inject|✅|❌|FAILED" \
        | head -10 || true
    echo ""
done

# Ingest + report
echo ""
echo "═══════════════════════════════════════════════════════════════════════════"
echo "Aggregated report"
echo "═══════════════════════════════════════════════════════════════════════════"
python3 "$REPO_ROOT/tools/ingest-metrics.py" --rebuild --report
