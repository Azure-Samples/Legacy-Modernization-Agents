#!/bin/bash
# Smoke-tests the platform helpers in doctor.sh (port lookup, Python detection, venv paths).
# Runs on Linux, macOS and Git Bash on Windows; CI runs it on each.

set -uo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
failures=0

pass() { echo "ok   - $1"; }
fail() { echo "FAIL - $1"; failures=$((failures + 1)); }

# Load only the helper functions; running doctor.sh itself would start its menu.
eval "$(sed -n \
    -e '/^verify_python_candidate()/,/^}/p' \
    -e '/^detect_python()/,/^}/p' \
    -e '/^port_listen_pids()/,/^}/p' \
    -e '/^kill_port_listeners()/,/^}/p' \
    -e '/^graph_populator_python()/,/^}/p' \
    -e '/^read_local_config_value()/,/^}/p' \
    -e '/^SETUP_OWNED_KEYS=/p' \
    -e '/^restore_unowned_config_keys()/,/^}/p' \
    "$REPO_ROOT/doctor.sh")"

for fn in verify_python_candidate detect_python port_listen_pids kill_port_listeners graph_populator_python \
    read_local_config_value restore_unowned_config_keys; do
    declare -F "$fn" >/dev/null || fail "doctor.sh defines $fn"
done

# Line endings: a CRLF checkout breaks bash before doctor.sh reaches its first command.
if grep -q $'\r' "$REPO_ROOT/doctor.sh"; then
    fail "doctor.sh is checked out with LF line endings"
else
    pass "doctor.sh is checked out with LF line endings"
fi

# Python detection rejects a command that is on PATH but does not run Python.
stub_dir="$(mktemp -d)"
printf '#!/bin/sh\necho "Python was not found; run without arguments to install from the Microsoft Store"\n' \
    >"$stub_dir/python-stub"
chmod +x "$stub_dir/python-stub"
if PATH="$stub_dir:$PATH" verify_python_candidate python-stub; then
    fail "verify_python_candidate rejects a Store-style stub"
else
    pass "verify_python_candidate rejects a Store-style stub"
fi
rm -rf "$stub_dir"

PYTHON_CMD="$(detect_python)"
if [[ -n "$PYTHON_CMD" ]] && verify_python_candidate "$PYTHON_CMD"; then
    pass "detect_python found a working interpreter ($PYTHON_CMD)"
else
    fail "detect_python found a working interpreter"
fi

# Venv interpreter path follows the platform layout.
venv_python="$(graph_populator_python)"
case "$(uname -s)" in
    MINGW*|MSYS*|CYGWIN*) expected="Scripts/python.exe" ;;
    *) expected="bin/python" ;;
esac
if [[ "$venv_python" == *"/tools/graph-populator/.venv/$expected" ]]; then
    pass "graph_populator_python uses .venv/$expected"
else
    fail "graph_populator_python uses .venv/$expected (got $venv_python)"
fi

# Port lookup and cleanup work with lsof or, on Git Bash, netstat/taskkill.
port="${DOCTOR_SMOKE_PORT:-58731}"
if [[ -n "$PYTHON_CMD" ]]; then
    "$PYTHON_CMD" -m http.server "$port" --bind 127.0.0.1 >/dev/null 2>&1 &
    server_pid=$!
    for _ in $(seq 1 50); do
        [[ -n "$(port_listen_pids "$port")" ]] && break
        sleep 0.2
    done
    if [[ -n "$(port_listen_pids "$port")" ]]; then
        pass "port_listen_pids sees a listener on $port"
    else
        fail "port_listen_pids sees a listener on $port"
    fi
    kill_port_listeners "$port"
    for _ in $(seq 1 25); do
        [[ -z "$(port_listen_pids "$port")" ]] && break
        sleep 0.2
    done
    if [[ -z "$(port_listen_pids "$port")" ]]; then
        pass "kill_port_listeners frees port $port"
    else
        fail "kill_port_listeners frees port $port"
    fi
    kill "$server_pid" 2>/dev/null || true
    wait "$server_pid" 2>/dev/null || true
fi

if kill_port_listeners "$port"; then
    fail "kill_port_listeners reports nothing to kill on a free port"
else
    pass "kill_port_listeners reports nothing to kill on a free port"
fi

# Re-running setup replaces the AI provider keys but keeps everything else: the Neo4j
# passwords are the only values that open existing graph volumes.
config_dir="$(mktemp -d)"
cat >"$config_dir/previous" <<'EOF'
_CHAT_MODEL="old-model"
COPILOT_GITHUB_TOKEN="old-token"
NEO4J_PASSWORD="volume-password"
NEO4J_CONTAINER="migration-neo4j-second"
COBOL_SOURCE_FOLDER="estates/one"
EOF
cat >"$config_dir/current" <<'EOF'
_CHAT_MODEL="new-model"
NEO4J_PASSWORD="template-default"
COBOL_SOURCE_FOLDER="source"
EOF
restore_unowned_config_keys "$config_dir/previous" "$config_dir/current"
check_value() {
    local key="$1" expected="$2" actual
    actual="$(read_local_config_value "$key" "$config_dir/current")"
    if [[ "$actual" == "$expected" ]]; then
        pass "setup merge: $key is '${expected:-<unset>}'"
    else
        fail "setup merge: $key is '${expected:-<unset>}' (got '$actual')"
    fi
}
check_value _CHAT_MODEL new-model
check_value COPILOT_GITHUB_TOKEN ""
check_value NEO4J_PASSWORD volume-password
check_value NEO4J_CONTAINER migration-neo4j-second
check_value COBOL_SOURCE_FOLDER estates/one
if [[ "$(grep -c '^NEO4J_PASSWORD=' "$config_dir/current")" == 1 ]]; then
    pass "setup merge writes each key once"
else
    fail "setup merge writes each key once"
fi
rm -rf "$config_dir"

echo ""
if [[ $failures -gt 0 ]]; then
    echo "$failures check(s) failed"
    exit 1
fi
echo "All checks passed"
