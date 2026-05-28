#!/usr/bin/env bash
# ─────────────────────────────────────────────────────────────────────────────
# ab-projection-suite.sh — multi-program A/B comparison runner
#
# Loops over a program list, runs each program through ab-projection.sh in
# cold-cache mode (clears Data/llm-cache.db before each leg pair), and emits
# both a per-program CSV and a markdown summary suitable for pasting into
# docs/p1-ab-validation-protocol.md §Automated metrics.
#
# Usage:
#   tools/ab-projection-suite.sh --programs PROG1,PROG2,PROG3 [--target java|csharp]
#   tools/ab-projection-suite.sh --program-file path/to/list.txt
#
# Program list format: one program basename (or stem) per line; blank lines and
# lines starting with '#' ignored.
#
# Outputs (in --output-dir, default tools/ab-results-YYYYMMDD-HHMMSS):
#   suite.csv                        — one row per program with token / latency deltas
#   suite-summary.md                 — markdown table for the protocol doc
#   <program>/baseline.log           — raw baseline log
#   <program>/projection.log         — raw projection log
#
# Flags:
#   --programs A,B,C       comma-separated list
#   --program-file FILE    path to a file with one program per line
#   --target java|csharp   target language (default java)
#   --projection-only      run only the projection leg (useful for isolating runtime issues)
#   --output-dir DIR       results directory (default auto-named)
#   --keep-cache           do NOT clear Data/llm-cache.db between programs
#                          (use only when measuring cache hit-rate, not cold cost)
#   --skip-existing        if a program already has logs in output dir, skip it
#                          (useful for resuming a partial run)
#
# Exit codes:
#   0  all programs ran (some may have failed legs — see summary)
#   2  precondition not met (missing file, no programs supplied)
# ─────────────────────────────────────────────────────────────────────────────
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
PROGRAMS=""
PROGRAM_FILE=""
TARGET="java"
PROJECTION_ONLY=false
OUTPUT_DIR=""
KEEP_CACHE=false
SKIP_EXISTING=false

while [[ $# -gt 0 ]]; do
    case "$1" in
        --programs)       PROGRAMS="$2"; shift 2 ;;
        --program-file)   PROGRAM_FILE="$2"; shift 2 ;;
        --target)         TARGET="$2"; shift 2 ;;
        --projection-only) PROJECTION_ONLY=true; shift ;;
        --output-dir)     OUTPUT_DIR="$2"; shift 2 ;;
        --keep-cache)     KEEP_CACHE=true; shift ;;
        --skip-existing)  SKIP_EXISTING=true; shift ;;
        --help|-h)
            sed -n '2,40p' "$0"; exit 0 ;;
        *)
            echo "Unknown option: $1" >&2; exit 2 ;;
    esac
done

# Resolve program list.
PROGRAM_LIST=()
if [[ -n "$PROGRAMS" ]]; then
    IFS=',' read -ra PROGRAM_LIST <<< "$PROGRAMS"
fi
if [[ -n "$PROGRAM_FILE" ]]; then
    if [[ ! -f "$PROGRAM_FILE" ]]; then
        echo "Program file not found: $PROGRAM_FILE" >&2; exit 2
    fi
    while IFS= read -r line; do
        line=$(echo "$line" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')
        [[ -z "$line" || "$line" =~ ^# ]] && continue
        PROGRAM_LIST+=("$line")
    done < "$PROGRAM_FILE"
fi
if [[ ${#PROGRAM_LIST[@]} -eq 0 ]]; then
    echo "No programs supplied. Use --programs A,B,C or --program-file LIST" >&2; exit 2
fi
if [[ "$PROJECTION_ONLY" == "true" && "$KEEP_CACHE" == "true" ]]; then
    echo "Cannot combine --projection-only with --keep-cache; projection-only mode is already isolated." >&2
    exit 2
fi

# Resolve output dir.
if [[ -z "$OUTPUT_DIR" ]]; then
    OUTPUT_DIR="$REPO_ROOT/tools/ab-results-$(date +%Y%m%d-%H%M%S)"
fi
mkdir -p "$OUTPUT_DIR"
CSV="$OUTPUT_DIR/suite.csv"
SUMMARY="$OUTPUT_DIR/suite-summary.md"

echo "program,baseline_ms,projection_ms,baseline_input_tokens,projection_input_tokens,baseline_total_tokens,projection_total_tokens,input_savings_pct,total_savings_pct,baseline_cache,projection_cache,mode,raw_rekt_context_tokens,projection_context_tokens,status" > "$CSV"

echo "═══════════════════════════════════════════════════════════════════════════"
echo "Suite: ${#PROGRAM_LIST[@]} program(s) → $TARGET"
echo "Output: $OUTPUT_DIR"
if [[ "$PROJECTION_ONLY" == "true" ]]; then
    echo "Mode: projection-only"
else
    echo "Cache: $([[ "$KEEP_CACHE" == "true" ]] && echo "preserved between programs (warm)" || echo "cleared between programs (cold)")"
fi
echo "═══════════════════════════════════════════════════════════════════════════"
echo ""

extract_field() {
    grep -E "^\s*$1\s*\|" 2>/dev/null | head -1 | awk -F'|' '{print $2}' | tr -d ' '
}

extract_int_pair() {
    # Pulls the baseline / projection int columns from a row in ab-projection's
    # final results table, given the metric name.
    local metric="$1" suite_log="$2"
    grep -E "^\s*$metric\b" "$suite_log" 2>/dev/null | head -1 \
        | awk '{print $(NF-2), $(NF-1)}' || true
}

extract_pct() {
    local metric="$1" suite_log="$2"
    grep -E "^\s*$metric\b" "$suite_log" 2>/dev/null | head -1 \
        | awk '{print $NF}' | sed 's/%//' || true
}

extract_cache_pair() {
    local suite_log="$1"
    grep -E '^\s*cache decision' "$suite_log" 2>/dev/null | head -1 \
        | awk '{print $(NF-2)"|"$(NF-1)}' || true
}

extract_input_tokens_from_log() {
    local leg_log="$1"
    
    # Try Copilot SDK format first: "CopilotChatClient metrics: ... totalCompletionTokens=123"
    local sdk_tokens=$(grep -Eo 'CopilotChatClient metrics:.*totalCompletionTokens=[0-9]+' "$leg_log" 2>/dev/null | head -1 \
        | grep -Eo 'totalCompletionTokens=[0-9]+' | grep -Eo '[0-9]+' || true)
    if [[ -n "$sdk_tokens" ]]; then echo "$sdk_tokens"; return; fi
    
    # Fall back to Azure Responses API format: "Responses API: ~N input ..."
    grep -E '^\s*Responses API:|^\s*Responses API completed' "$leg_log" 2>/dev/null || true \
        | grep -Eo '~[0-9]+ input|[0-9]+ input tokens' | head -1 \
        | grep -Eo '[0-9]+' | head -1 || true
}

extract_total_tokens_from_log() {
    local leg_log="$1"
    
    # Try Copilot SDK format first: "CopilotChatClient metrics: ... totalCompletionTokens=123"
    local sdk_tokens=$(grep -Eo 'CopilotChatClient metrics:.*totalCompletionTokens=[0-9]+' "$leg_log" 2>/dev/null | head -1 \
        | grep -Eo 'totalCompletionTokens=[0-9]+' | grep -Eo '[0-9]+' || true)
    if [[ -n "$sdk_tokens" ]]; then echo "$sdk_tokens"; return; fi
    
    # Fall back to Azure Responses API format: "Responses API completed ... = N tokens"
    grep -E 'Responses API completed' "$leg_log" 2>/dev/null || true \
        | grep -Eo '= [0-9]+ tokens' | head -1 \
        | grep -Eo '[0-9]+' || true
}

# Extract PROJECTION_METRICS structured log:
# "[JavaConverterAgent] PROJECTION_METRICS projectionMode=X file=Y projectionTokens=N rawRektTokens=M reductionPercent=P"
extract_projection_mode() {
    grep -Eo 'PROJECTION_METRICS projectionMode=[a-z-]+' "$1" 2>/dev/null | head -1 \
        | grep -Eo '[a-z-]+$' || echo "unknown"
}
extract_proj_tokens_from_metrics() {
    grep -Eo 'PROJECTION_METRICS.*projectionTokens=[0-9]+' "$1" 2>/dev/null | head -1 \
        | grep -Eo 'projectionTokens=[0-9]+' | grep -Eo '[0-9]+' || echo "0"
}
extract_rekt_tokens_from_metrics() {
    grep -Eo 'PROJECTION_METRICS.*rawRektTokens=[0-9]+' "$1" 2>/dev/null | head -1 \
        | grep -Eo 'rawRektTokens=[0-9]+' | grep -Eo '[0-9]+' || echo "0"
}

time_ms() {
    if command -v gdate >/dev/null 2>&1; then gdate +%s%3N
    else date +%s000; fi
}

success_count=0
fail_count=0

run_projection_only() {
    local program="$1" log="$2"
    local start=$(time_ms)
    (
        cd "$REPO_ROOT"
        export ENABLE_REKT_CONTEXT=true
        export _LLM_CACHE_ENABLED=false
        export _USE_PROGRAM_FACTS=true
        export MCP_AUTO_LAUNCH=0
        export LLM_CALL_TIMEOUT_SECONDS="${LLM_CALL_TIMEOUT_SECONDS:-900}"
        export COPILOT_SDK_REQUEST_TIMEOUT_SECONDS="${COPILOT_SDK_REQUEST_TIMEOUT_SECONDS:-900}"
        ./doctor.sh convert-only --program "$program" --target "$TARGET" --no-portal \
            > "$log" 2>&1
    ) || {
        echo "FAIL: projection-only leg returned non-zero. See $log" >&2
        return 1
    }
    local end=$(time_ms)
    echo $(( end - start ))
}

for raw in "${PROGRAM_LIST[@]}"; do
    PROG="${raw%.cbl}"; PROG="${PROG%.cob}"; PROG="${PROG%.CBL}"; PROG="${PROG%.COB}"
    LEG_DIR="$OUTPUT_DIR/$PROG"

    if [[ "$SKIP_EXISTING" == "true" && -f "$LEG_DIR/suite-log.txt" ]]; then
        echo "→ $PROG : already present, skipping (--skip-existing)"
        continue
    fi

    mkdir -p "$LEG_DIR"
    SUITE_LOG="$LEG_DIR/suite-log.txt"

    if [[ "$KEEP_CACHE" != "true" && "$PROJECTION_ONLY" != "true" ]]; then
        rm -f "$REPO_ROOT/Data/llm-cache.db" \
              "$REPO_ROOT/Data/llm-cache.db-wal" \
              "$REPO_ROOT/Data/llm-cache.db-shm"
    fi

    if [[ "$PROJECTION_ONLY" == "true" ]]; then
        echo "→ $PROG : running projection-only (target=$TARGET) ..."
        if PROJ_MS=$(run_projection_only "$PROG" "$LEG_DIR/projection.log"); then
            PROJ_IN=$(extract_input_tokens_from_log "$LEG_DIR/projection.log"); PROJ_IN=${PROJ_IN:-0}
            PROJ_TOTAL=$(extract_total_tokens_from_log "$LEG_DIR/projection.log"); PROJ_TOTAL=${PROJ_TOTAL:-0}
            PROJ_MODE=$(extract_projection_mode "$LEG_DIR/projection.log")
            PROJ_CONTEXT_TOK=$(extract_proj_tokens_from_metrics "$LEG_DIR/projection.log")
            REKT_CONTEXT_TOK=$(extract_rekt_tokens_from_metrics "$LEG_DIR/projection.log")
            echo "$PROG,0,${PROJ_MS:-0},0,${PROJ_IN:-0},0,${PROJ_TOTAL:-0},n/a,n/a,-,-,$PROJ_MODE,$REKT_CONTEXT_TOK,$PROJ_CONTEXT_TOK,ok" >> "$CSV"
            success_count=$((success_count + 1))
            echo "  ✓ $PROG : mode=$PROJ_MODE proj_context=${PROJ_CONTEXT_TOK}tok rekt_context=${REKT_CONTEXT_TOK}tok total=${PROJ_TOTAL:-?}tok ${PROJ_MS:-?}ms"
        else
            echo "$PROG,0,0,0,0,0,0,n/a,n/a,-,-,unknown,0,0,fail" >> "$CSV"
            fail_count=$((fail_count + 1))
            echo "  ✗ $PROG : projection-only leg failed, see $LEG_DIR/projection.log"
        fi
    else
        echo "→ $PROG : running A/B (target=$TARGET) ..."
        if "$REPO_ROOT/tools/ab-projection.sh" "$PROG" --target "$TARGET" --keep-output \
                > "$SUITE_LOG" 2>&1; then
            WORKSPACE=$(grep -Eo 'Leaving workspace at /[^ ]+' "$SUITE_LOG" | awk '{print $NF}')
            if [[ -n "$WORKSPACE" && -d "$WORKSPACE" ]]; then
                cp -f "$WORKSPACE/baseline.log" "$LEG_DIR/baseline.log" 2>/dev/null || true
                cp -f "$WORKSPACE/projection.log" "$LEG_DIR/projection.log" 2>/dev/null || true
                rm -rf "$WORKSPACE"
            fi

            # Parse the final results table from suite log.
            read -r BASE_MS PROJ_MS < <(extract_int_pair "wall clock \(ms\)" "$SUITE_LOG") || true
            read -r BASE_IN PROJ_IN < <(extract_int_pair "input tokens \(primary\)" "$SUITE_LOG") || true
            read -r BASE_TOTAL PROJ_TOTAL < <(extract_int_pair "total tokens \(primary\)" "$SUITE_LOG") || true
            IN_PCT=$(extract_pct "input tokens \(primary\)" "$SUITE_LOG") || true
            TOTAL_PCT=$(extract_pct "total tokens \(primary\)" "$SUITE_LOG") || true
            CACHES=$(extract_cache_pair "$SUITE_LOG") || true
            BASE_CACHE="${CACHES%|*}"
            PROJ_CACHE="${CACHES#*|}"

            # Extract PROJECTION_METRICS from both legs
            BASE_REKT_TOK=$(extract_rekt_tokens_from_metrics "$LEG_DIR/baseline.log"); BASE_REKT_TOK=${BASE_REKT_TOK:-0}
            PROJ_CONTEXT_TOK=$(extract_proj_tokens_from_metrics "$LEG_DIR/projection.log"); PROJ_CONTEXT_TOK=${PROJ_CONTEXT_TOK:-0}
            # Compute context reduction: how much smaller was projection vs raw REKT?
            if [[ "$BASE_REKT_TOK" -gt 0 && "$PROJ_CONTEXT_TOK" -gt 0 ]]; then
                CTX_REDUCTION=$(awk -v b="$BASE_REKT_TOK" -v a="$PROJ_CONTEXT_TOK" \
                    'BEGIN { printf "%.1f%%", (b-a)*100/b }')
            else
                CTX_REDUCTION="n/a"
            fi

            echo "$PROG,${BASE_MS:-0},${PROJ_MS:-0},${BASE_IN:-0},${PROJ_IN:-0},${BASE_TOTAL:-0},${PROJ_TOTAL:-0},${IN_PCT:-n/a},${TOTAL_PCT:-n/a},${BASE_CACHE:--},${PROJ_CACHE:--},ab,$BASE_REKT_TOK,$PROJ_CONTEXT_TOK,ok" >> "$CSV"
            success_count=$((success_count + 1))
            echo "  ✓ $PROG : baseline=${BASE_IN:-?}in/${BASE_TOTAL:-?}tot/${BASE_MS:-?}ms → projection=${PROJ_IN:-?}in/${PROJ_TOTAL:-?}tot/${PROJ_MS:-?}ms (input ${IN_PCT:-n/a}, total ${TOTAL_PCT:-n/a})"
            echo "       context: raw-rekt=${BASE_REKT_TOK}tok → projection=${PROJ_CONTEXT_TOK}tok (reduction=${CTX_REDUCTION})"
        else
            echo "$PROG,0,0,0,0,0,0,n/a,n/a,-,-,ab,0,0,fail" >> "$CSV"
            fail_count=$((fail_count + 1))
            echo "  ✗ $PROG : leg failed, see $SUITE_LOG"
        fi
    fi
done

# ── Markdown summary ────────────────────────────────────────────────────────
{
    echo "# Suite results"
    echo ""
    echo "- target: \`$TARGET\`"
    echo "- programs run: ${#PROGRAM_LIST[@]} (success: $success_count, fail: $fail_count)"
    if [[ "$PROJECTION_ONLY" == "true" ]]; then
        echo "- mode: projection-only"
    else
        echo "- cache mode: $([[ "$KEEP_CACHE" == "true" ]] && echo "warm" || echo "cold")"
    fi
    echo "- output dir: \`$OUTPUT_DIR\`"
    echo ""
    echo "## Automated metrics"
    echo ""
    echo "| program | baseline ms | projection ms | baseline in | projection in | baseline total | projection total | input Δ | total Δ | rekt-ctx tok | proj-ctx tok | ctx Δ | status |"
    echo "|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---|"
    tail -n +2 "$CSV" | while IFS=',' read -r p bm pm bi pi bt pt ip tp bc pc mode rektok prjtok st; do
        # Compute context reduction inline
        ctx_delta="n/a"
        if [[ "$rektok" =~ ^[0-9]+$ && "$prjtok" =~ ^[0-9]+$ && "$rektok" -gt 0 && "$prjtok" -gt 0 ]]; then
            ctx_delta=$(awk -v b="$rektok" -v a="$prjtok" 'BEGIN { printf "%.1f%%", (b-a)*100/b }')
        fi
        echo "| $p | $bm | $pm | $bi | $pi | $bt | $pt | $ip | $tp | $rektok | $prjtok | $ctx_delta | $st |"
    done
    echo ""
    echo "## Per-program raw logs"
    echo ""
    for raw in "${PROGRAM_LIST[@]}"; do
        local_prog="${raw%.cbl}"; local_prog="${local_prog%.cob}"
        echo "- \`$local_prog\` : [\`baseline.log\`](./${local_prog}/baseline.log) · [\`projection.log\`](./${local_prog}/projection.log)"
    done
    echo ""
    echo "## Next steps"
    echo ""
    echo "1. Inspect each pair of logs for compile diagnostics (\`grep -E 'error|warning' projection.log\`)."
    echo "2. Diff the generated source files between the two legs."
    echo "3. Score each program against the rubric in \`docs/p1-ab-validation-protocol.md\`."
    echo "4. Paste the table above into the protocol doc under §Automated metrics."
} > "$SUMMARY"

echo ""
echo "═══════════════════════════════════════════════════════════════════════════"
echo "Suite complete: $success_count ok, $fail_count failed."
echo "  CSV     : $CSV"
echo "  Summary : $SUMMARY"
echo "═══════════════════════════════════════════════════════════════════════════"
