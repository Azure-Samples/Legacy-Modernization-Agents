#!/bin/bash
# Port helpers shared by doctor.sh and helper-scripts/. Source this file; it defines
# functions only. Works with lsof (Linux, macOS) and netstat/taskkill (Git Bash on Windows).

# PIDs listening on a TCP port. Git Bash has no lsof, so fall back to Windows netstat
# ("TCP  0.0.0.0:5028  0.0.0.0:0  LISTENING  1234", PID in the last field).
port_listen_pids() {
    local port="$1"
    if command -v lsof >/dev/null 2>&1; then
        lsof -Pi ":$port" -sTCP:LISTEN -t 2>/dev/null
    elif command -v netstat >/dev/null 2>&1; then
        netstat -ano 2>/dev/null | grep -E "[:.]${port}[[:space:]]+.*LISTENING" \
            | awk '{print $NF}' | sort -u
    fi
}

# Kill the processes listening on a TCP port; returns 1 when nothing was listening.
kill_port_listeners() {
    local port="$1" pids pid
    pids="$(port_listen_pids "$port")"
    [[ -z "$pids" ]] && return 1
    if ! command -v lsof >/dev/null 2>&1 && command -v taskkill >/dev/null 2>&1; then
        # Double slashes stop Git Bash rewriting /F and /PID into file paths.
        while IFS= read -r pid; do
            [[ -n "$pid" ]] && taskkill //F //PID "$pid" >/dev/null 2>&1
        done <<<"$pids"
    else
        echo "$pids" | xargs kill -9 2>/dev/null
    fi
}

# True when something listens on the port.
port_in_use() {
    [[ -n "$(port_listen_pids "$1")" ]]
}
