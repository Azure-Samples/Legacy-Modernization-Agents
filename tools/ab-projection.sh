#!/usr/bin/env bash
# ─────────────────────────────────────────────────────────────────────────────
# ab-projection.sh — A/B comparison: raw-AST path vs. program-facts projection
#
# Runs two single-program convert-only passes against the same input and
# compares prompt size / token usage / wall time. Both legs use the response
# cache so the second run on the same inputs is near-instant — clear the
# cache between runs if you want cold numbers.
#
# Usage:
#   tools/ab-projection.sh <program-basename> [--target java|csharp] [--keep-output]
#
# Examples:
#   tools/ab-projection.sh BDSDA2F
#   tools/ab-projection.sh BDSDA2F --target csharp --keep-output
#
# Prerequisites (any one of these missing → script exits 2 with the reason):
#   • dotnet project built (CobolToQuarkusMigration.csproj)
#   • rekt-full has run at least once (output/rekt/ populated)
#   • LLM credentials configured per doctor.sh (Entra ID or API key)
#
# The script does NOT:
#   • run rekt-full for you (intentional — keep it cheap)
#   • alter your default config (env vars are scoped to each leg)
#
# Exit codes:
#   0  comparison succeeded; summary printed.
#   2  precondition not met (missing program, no facts, etc.).
#   3  one of the conversion legs failed (look at the leg log paths).
# ─────────────────────────────────────────────────────────────────────────────
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"

# Help flag short-circuit (must run before positional parsing).
case "${1:-}" in
    --help|-h|"")
        if [[ "${1:-}" == "" ]]; then
            echo "Usage: tools/ab-projection.sh <program-basename> [--target java|csharp] [--keep-output]" >&2
            exit 2
        fi
        sed -n '2,30p' "$0"
        exit 0
        ;;
esac

PROGRAM="${1:-}"
TARGET="java"
KEEP_OUTPUT=false
shift || true
while [[ $# -gt 0 ]]; do
    case "$1" in
        --target)       TARGET="$2"; shift 2 ;;
        --keep-output)  KEEP_OUTPUT=true; shift ;;
        *)
            echo "Unknown option: $1" >&2
            exit 2
            ;;
    esac
done

if [[ -z "$PROGRAM" ]]; then
    echo "Usage: tools/ab-projection.sh <program-basename> [--target java|csharp] [--keep-output]" >&2
    exit 2
fi

PROGRAM_BASE="${PROGRAM%.cbl}"
PROGRAM_BASE="${PROGRAM_BASE%.cob}"

# ── Preconditions ────────────────────────────────────────────────────────────
if [[ ! -f "$REPO_ROOT/CobolToQuarkusMigration.csproj" ]]; then
    echo "ERROR: project not found at $REPO_ROOT" >&2; exit 2
fi
if ! command -v dotnet >/dev/null 2>&1; then
    echo "ERROR: dotnet not on PATH" >&2; exit 2
fi
if [[ ! -d "$REPO_ROOT/output/rekt" ]] || \
   ! find "$REPO_ROOT/output/rekt" -maxdepth 1 -name '*.json' -print -quit | grep -q .; then
    echo "ERROR: no REKT output found in $REPO_ROOT/output/rekt — run 'doctor.sh rekt-full' first" >&2
    exit 2
fi

# Verify the program exists in the source tree (recursive — PR0 / PR2.c work).
PROG_PATH=$(find "$REPO_ROOT/source" -type f \
    \( -name "${PROGRAM_BASE}.cbl" -o -name "${PROGRAM_BASE}.CBL" \
       -o -name "${PROGRAM_BASE}.cob" -o -name "${PROGRAM_BASE}.COB" \) \
    ! -path "*/.rekt-staging/*" ! -path "*/.preprocessed/*" \
    -print -quit 2>/dev/null || true)
if [[ -z "$PROG_PATH" ]]; then
    echo "ERROR: program '$PROGRAM_BASE' not found under $REPO_ROOT/source/" >&2
    exit 2
fi
PROG_BASENAME=$(basename "$PROG_PATH")

# ── Workspace ────────────────────────────────────────────────────────────────
WORKDIR=$(mktemp -d -t ab-projection.XXXXXX)
BASELINE_LOG="$WORKDIR/baseline.log"
PROJECTION_LOG="$WORKDIR/projection.log"
BASELINE_OUT="$WORKDIR/baseline-output"
PROJECTION_OUT="$WORKDIR/projection-output"
mkdir -p "$BASELINE_OUT" "$PROJECTION_OUT"

cleanup() {
    if [[ "$KEEP_OUTPUT" == "true" ]]; then
        echo "Leaving workspace at $WORKDIR" >&2
    else
        rm -rf "$WORKDIR"
    fi
}
trap cleanup EXIT

echo ""
echo "═══════════════════════════════════════════════════════════════════════════"
echo "A/B comparison for $PROG_BASENAME → $TARGET"
echo "═══════════════════════════════════════════════════════════════════════════"
echo "  workspace : $WORKDIR"
echo "  source    : $PROG_PATH"
echo ""

# ── Ensure facts file exists for the projection leg ─────────────────────────
FACTS_PATH="$REPO_ROOT/output/rekt/${PROGRAM_BASE}.facts.json"
if [[ ! -f "$FACTS_PATH" ]]; then
    echo "→ generating program-facts.json (one-time) ..."
    STAGING_DIR="$REPO_ROOT/source/.rekt-staging"
    if [[ ! -d "$STAGING_DIR" ]]; then
        STAGING_DIR="$REPO_ROOT/source"
    fi
    (cd "$REPO_ROOT" && dotnet run --project CobolToQuarkusMigration.csproj --no-build -- \
        program-facts extract "$STAGING_DIR" \
        --rekt-dir "$REPO_ROOT/output/rekt" \
        --output-dir "$REPO_ROOT/output/rekt" \
        --programs "$PROG_BASENAME" \
        --repo-root "$REPO_ROOT" >/dev/null 2>&1) || {
            echo "ERROR: program-facts extract failed for $PROG_BASENAME" >&2; exit 2; }
    if [[ ! -f "$FACTS_PATH" ]]; then
        echo "ERROR: program-facts extract did not produce $FACTS_PATH" >&2; exit 2
    fi
fi

# ── Time helper (Bash-portable across mac/linux) ────────────────────────────
time_ms() {
    if command -v gdate >/dev/null 2>&1; then gdate +%s%3N
    else date +%s000; fi   # macOS bash 3 fallback: seconds × 1000
}

run_leg() {
    local name="$1" log="$2" outdir="$3" use_facts="$4"
    local start=$(time_ms)
    (
        cd "$REPO_ROOT"
        export ENABLE_REKT_CONTEXT=true
        export _LLM_CACHE_ENABLED=true
        # doctor.sh convert-only auto-launches McpChatWeb at the end (line ~2516)
        # which blocks indefinitely. MCP_AUTO_LAUNCH=0 disables that.
        export MCP_AUTO_LAUNCH=0
        # Force direct dotnet invocation, bypassing run_via_portal.
        # The portal API only forwards a fixed allowlist of env vars; routing
        # through it would silently drop _USE_PROGRAM_FACTS,
        # COPILOT_SDK_REQUEST_TIMEOUT_SECONDS, and others — invalidating the
        # A/B test. Setting PORTAL_LAUNCHED=true short-circuits run_via_portal
        # and falls back to a direct child dotnet process that inherits our
        # full environment.
        export PORTAL_LAUNCHED=true
        if [[ "$use_facts" == "true" ]]; then
            export _USE_PROGRAM_FACTS=true
        else
            unset _USE_PROGRAM_FACTS
        fi
        # convert-only routes through JavaConverterAgent or CSharpConverterAgent.
        ./doctor.sh convert-only --program "$PROGRAM_BASE" --target "$TARGET" --no-portal \
            > "$log" 2>&1
    ) || {
        echo "FAIL: $name leg returned non-zero. See $log" >&2
        return 1
    }
    local end=$(time_ms)
    echo $(( end - start ))
}

# ── Run both legs ────────────────────────────────────────────────────────────
echo "→ leg A (baseline, raw-AST) ..."
BASELINE_MS=$(run_leg baseline "$BASELINE_LOG" "$BASELINE_OUT" false) || exit 3
echo "  done in ${BASELINE_MS}ms (log: $BASELINE_LOG)"

echo "→ leg B (projection, program-facts) ..."
PROJECTION_MS=$(run_leg projection "$PROJECTION_LOG" "$PROJECTION_OUT" true) || exit 3
echo "  done in ${PROJECTION_MS}ms (log: $PROJECTION_LOG)"

# ── Extract metrics ──────────────────────────────────────────────────────────
# Support both Responses API (Azure OpenAI) and Copilot SDK (GitHub Copilot)
# Responses API logs: "Responses API: ~N input + M max output = ~T total tokens"
# Copilot SDK logs: "CopilotChatClient metrics: ... totalCompletionTokens=N"
# Pick the first line per leg (the primary call); continuations would inflate.
extract_input_tokens() {
    local log_file="$1"
    
    # Try Copilot SDK format first
    local sdk_tokens=$(grep -Eo 'CopilotChatClient metrics:.*totalCompletionTokens=[0-9]+' "$log_file" 2>/dev/null | head -1 \
        | grep -Eo 'totalCompletionTokens=[0-9]+' | grep -Eo '[0-9]+' || true)
    if [[ -n "$sdk_tokens" ]]; then
        echo "$sdk_tokens"
        return
    fi
    
    # Fall back to Responses API format
    grep -E '^\s*Responses API:|^\s*Responses API completed' "$log_file" 2>/dev/null \
        | grep -Eo '~[0-9]+ input|[0-9]+ input tokens' | head -1 \
        | grep -Eo '[0-9]+' | head -1 || true
}

extract_total_tokens() {
    local log_file="$1"
    
    # Try Copilot SDK format first
    local sdk_tokens=$(grep -Eo 'CopilotChatClient metrics:.*totalCompletionTokens=[0-9]+' "$log_file" 2>/dev/null | head -1 \
        | grep -Eo 'totalCompletionTokens=[0-9]+' | grep -Eo '[0-9]+' || true)
    if [[ -n "$sdk_tokens" ]]; then
        echo "$sdk_tokens"
        return
    fi
    
    # Fall back to Responses API format
    grep -E 'Responses API completed' "$log_file" 2>/dev/null \
        | grep -Eo '= [0-9]+ tokens' | head -1 \
        | grep -Eo '[0-9]+' || true
}

extract_cache_decision() {
    grep -Eo 'LlmResponseCache.*decision=[a-z-]+' "$1" 2>/dev/null | head -1 \
        | grep -Eo 'decision=[a-z-]+' | head -1 || true
}

BASELINE_IN=$(extract_input_tokens "$BASELINE_LOG"); BASELINE_IN=${BASELINE_IN:-0}
BASELINE_TOTAL=$(extract_total_tokens "$BASELINE_LOG"); BASELINE_TOTAL=${BASELINE_TOTAL:-0}
PROJECTION_IN=$(extract_input_tokens "$PROJECTION_LOG"); PROJECTION_IN=${PROJECTION_IN:-0}
PROJECTION_TOTAL=$(extract_total_tokens "$PROJECTION_LOG"); PROJECTION_TOTAL=${PROJECTION_TOTAL:-0}
BASELINE_CACHE=$(extract_cache_decision "$BASELINE_LOG" || echo "(none)")
PROJECTION_CACHE=$(extract_cache_decision "$PROJECTION_LOG" || echo "(none)")

savings_pct() {
    local before="$1" after="$2"
    if [[ "$before" -eq 0 ]]; then echo "n/a"; return; fi
    awk -v b="$before" -v a="$after" 'BEGIN { printf "%.1f%%", (b-a)*100/b }'
}

echo ""
echo "═══════════════════════════════════════════════════════════════════════════"
echo "RESULTS"
echo "═══════════════════════════════════════════════════════════════════════════"
printf "  %-25s %-15s %-15s %-10s\n" "metric" "baseline" "projection" "delta"
printf "  %-25s %-15s %-15s %-10s\n" "─────────────────────────" "───────────────" "───────────────" "──────────"
printf "  %-25s %-15s %-15s %-10s\n" "wall clock (ms)" "$BASELINE_MS" "$PROJECTION_MS" "$(savings_pct "$BASELINE_MS" "$PROJECTION_MS")"
printf "  %-25s %-15s %-15s %-10s\n" "input tokens (primary)" "$BASELINE_IN" "$PROJECTION_IN" "$(savings_pct "$BASELINE_IN" "$PROJECTION_IN")"
printf "  %-25s %-15s %-15s %-10s\n" "total tokens (primary)" "$BASELINE_TOTAL" "$PROJECTION_TOTAL" "$(savings_pct "$BASELINE_TOTAL" "$PROJECTION_TOTAL")"
printf "  %-25s %-15s %-15s %-10s\n" "cache decision" "$BASELINE_CACHE" "$PROJECTION_CACHE" "-"
echo ""
echo "Interpretation:"
echo "  • Input-token delta < 0% means the projection block is LARGER than the raw"
echo "    structural context for this program — usually means the facts file is sparse"
echo "    (low confidence) or the program is small enough that raw-AST is already compact."
echo "  • If both cache decisions are 'hit', clear Data/llm-cache.db and re-run for"
echo "    a true cold comparison."
echo "  • The 'projection' leg invalidates any cached entry from the baseline leg"
echo "    because the prompt content (and thus the cache key) differs — expected."
echo ""
echo "Logs preserved at:"
echo "  baseline   : $BASELINE_LOG"
echo "  projection : $PROJECTION_LOG"
if [[ "$KEEP_OUTPUT" != "true" ]]; then
    echo "Pass --keep-output to retain $WORKDIR after the run."
fi
