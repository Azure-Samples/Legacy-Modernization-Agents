#!/usr/bin/env bash
# ─────────────────────────────────────────────────────────────────────────────
# verify-env-propagation.sh — single-program smoke test for PR4 env vars
#
# Purpose:
#   Confirms that critical env vars (_USE_PROGRAM_FACTS,
#   COPILOT_SDK_REQUEST_TIMEOUT_SECONDS) actually reach the dotnet conversion
#   process and that MetricsSink writes a JSONL artifact we can read.
#
# What it asserts after a single projection-leg run:
#   1. output/.metrics/{runId}.jsonl exists
#   2. The file contains projectionMode=projection (proves _USE_PROGRAM_FACTS reached the agent)
#   3. The file contains projectionTokens > 0
#
# Run BEFORE the full A/B suite to catch env-propagation regressions cheaply.
#
# Usage:
#   tools/verify-env-propagation.sh SAMPLE001
#   tools/verify-env-propagation.sh SAMPLE001 --timeout 900
# ─────────────────────────────────────────────────────────────────────────────
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
PROGRAM="${1:-}"
TIMEOUT_SECONDS=900

# Parse optional flags
shift 2>/dev/null || true
while [[ $# -gt 0 ]]; do
    case "$1" in
        --timeout) TIMEOUT_SECONDS="$2"; shift 2 ;;
        *) shift ;;
    esac
done

if [[ -z "$PROGRAM" ]]; then
    echo "Usage: tools/verify-env-propagation.sh <program-basename> [--timeout SECONDS]" >&2
    exit 2
fi

echo "═══════════════════════════════════════════════════════════════════════════"
echo "Env propagation smoke test"
echo "═══════════════════════════════════════════════════════════════════════════"
echo "  program:      $PROGRAM"
echo "  sdk timeout:  ${TIMEOUT_SECONDS}s"
echo "  metrics dir:  $REPO_ROOT/output/.metrics/"
echo ""

# Snapshot pre-existing metrics files so we know which runId is new.
PRE_RUNS=$(ls -1 "$REPO_ROOT/output/.metrics/" 2>/dev/null | sort || true)

LOG="/tmp/verify-env-${PROGRAM}-$$.log"
echo "→ Running single projection leg (log: $LOG)"

set +e
(
    cd "$REPO_ROOT"
    export ENABLE_REKT_CONTEXT=true
    export _LLM_CACHE_ENABLED=false
    export MCP_AUTO_LAUNCH=0
    export PORTAL_LAUNCHED=true       # force direct dotnet, bypass portal
    export _USE_PROGRAM_FACTS=true
    export COPILOT_SDK_REQUEST_TIMEOUT_SECONDS="$TIMEOUT_SECONDS"
    export LLM_CALL_TIMEOUT_SECONDS="$TIMEOUT_SECONDS"
    ./doctor.sh convert-only --program "$PROGRAM" --target java --no-portal \
        > "$LOG" 2>&1
)
EXIT_CODE=$?
set -e

if [[ $EXIT_CODE -ne 0 ]]; then
    echo "  ✗ Conversion command exited with code $EXIT_CODE"
    echo "  Last 30 lines of log:"
    tail -30 "$LOG" | sed 's/^/    /'
    exit 3
fi

echo "  ✓ Conversion completed"
echo ""

# Find any NEW metrics files (didn't exist before our run).
POST_RUNS=$(ls -1 "$REPO_ROOT/output/.metrics/" 2>/dev/null | sort || true)
NEW_FILES=$(comm -13 <(echo "$PRE_RUNS") <(echo "$POST_RUNS") 2>/dev/null | grep -v "^$" || true)

echo "═══════════════════════════════════════════════════════════════════════════"
echo "Assertions"
echo "═══════════════════════════════════════════════════════════════════════════"

PASS=0
FAIL=0

# Assertion 1: a new metrics file exists
if [[ -z "$NEW_FILES" ]]; then
    echo "  ✗ FAIL: no new file appeared in output/.metrics/"
    echo "        MetricsSink not wired up, or runId resolution failed."
    FAIL=$((FAIL+1))
else
    echo "  ✓ PASS: new metrics file(s) created:"
    echo "$NEW_FILES" | sed 's/^/        /'
    PASS=$((PASS+1))

    # Pick the first new file for content assertions
    METRIC_FILE="$REPO_ROOT/output/.metrics/$(echo "$NEW_FILES" | head -1)"

    # Assertion 2: file contains projectionMode=projection
    if grep -q '"projectionMode":"projection"' "$METRIC_FILE"; then
        echo "  ✓ PASS: projectionMode=projection found in $(basename "$METRIC_FILE")"
        echo "        → _USE_PROGRAM_FACTS reached the agent"
        PASS=$((PASS+1))
    else
        MODE=$(grep -oE '"projectionMode":"[^"]+"' "$METRIC_FILE" | head -1 || echo "(none)")
        echo "  ✗ FAIL: projectionMode is not 'projection' (got: $MODE)"
        echo "        → _USE_PROGRAM_FACTS did NOT reach the agent"
        FAIL=$((FAIL+1))
    fi

    # Assertion 3: projectionTokens > 0
    PROJ_TOK=$(grep -oE '"projectionTokens":[0-9]+' "$METRIC_FILE" | head -1 | grep -oE '[0-9]+' || echo "0")
    if [[ "$PROJ_TOK" -gt 0 ]]; then
        echo "  ✓ PASS: projectionTokens=$PROJ_TOK (>0)"
        PASS=$((PASS+1))
    else
        echo "  ✗ FAIL: projectionTokens=0 — projection block was empty"
        FAIL=$((FAIL+1))
    fi
fi

echo ""
echo "═══════════════════════════════════════════════════════════════════════════"
echo "Summary: $PASS passed, $FAIL failed"
echo "═══════════════════════════════════════════════════════════════════════════"

[[ $FAIL -eq 0 ]] || exit 1
echo "✅ Env propagation verified — safe to run full A/B suite"
