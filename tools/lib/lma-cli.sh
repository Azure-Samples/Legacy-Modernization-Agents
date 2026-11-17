#!/bin/bash
# Migration CLI helper runner shared by doctor.sh and tools/preprocess-for-rekt.sh. Source this
# file; it defines functions only. Works with bash 3.2 (macOS) and Git Bash on Windows.

# Runs one offline helper: lma_cli_run <assembly> <command> [args...]. Callers may treat a
# non-zero exit as an answer, so only a crash is reported here - but always, in logs that no
# caller redirect can hide. A swallowed crash once let a scan finish on half its evidence.
lma_cli_run() {
    local assembly="$1"
    shift
    if [[ -z "$assembly" ]]; then
        lma_cli_report_crash 127 /dev/null "$@"
        return 127
    fi
    local err_file rc=0
    err_file="$(mktemp "${TMPDIR:-/tmp}/lma-cli.XXXXXX" 2>/dev/null)" || err_file=/dev/null
    # stdout passes through untouched; stderr stays live and is also kept for the report.
    # The && / || pair reads the helper's own status without tripping set -e or pipefail.
    { "${DOTNET_CMD:-dotnet}" "$assembly" "$@" 2>&1 1>&3 3>&- | tee "$err_file" >&2 \
        && rc=${PIPESTATUS[0]} || rc=${PIPESTATUS[0]}; } 3>&1
    if (( rc != 0 )) && lma_cli_is_crash "$rc" "$err_file"; then
        lma_cli_report_crash "$rc" "$err_file" "$@"
    fi
    [[ "$err_file" == /dev/null ]] || rm -f "$err_file"
    return "$rc"
}

# A crash is an unhandled .NET exception (System.CommandLine and the runtime both print
# "Unhandled exception") or a signal/runtime abort (exit >= 128). Handled errors exit 1-3.
lma_cli_is_crash() {
    (( $1 >= 128 )) || grep -q 'Unhandled exception' "$2" 2>/dev/null
}

# Appends the crash to LMA_HELPER_CRASH_LOG (the current scan, which then fails) and
# LMA_HELPER_LOG (persistent), then prints a banner on stderr.
lma_cli_report_crash() {
    local rc="$1" err_file="$2"
    shift 2
    local entry log logged=""
    entry="$(printf '=== %s lma_cli %s (exit %s)\n' "$(date '+%Y-%m-%dT%H:%M:%S')" "$*" "$rc"; cat "$err_file" 2>/dev/null)"
    for log in "${LMA_HELPER_CRASH_LOG:-}" "${LMA_HELPER_LOG:-}"; do
        [[ -n "$log" ]] || continue
        if mkdir -p "$(dirname "$log")" 2>/dev/null && printf '%s\n\n' "$entry" >> "$log" 2>/dev/null; then
            logged="${logged:+$logged, }$log"
        fi
    done
    printf '\033[0;31m❌ Internal helper crashed: %s %s (exit %s). This is a tool bug, not a source problem.\033[0m\n' \
        "${1:-<none>}" "${2:-}" "$rc" >&2
    [[ -z "$logged" ]] || printf '   Details: %s\n' "$logged" >&2
}
