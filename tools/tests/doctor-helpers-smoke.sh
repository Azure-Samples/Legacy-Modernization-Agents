#!/bin/bash
# Smoke-tests the platform helpers in doctor.sh (port lookup, setup config merge).
# Runs on Linux, macOS and Git Bash on Windows; CI runs it on each.

set -uo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
failures=0

pass() { echo "ok   - $1"; }
fail() { echo "FAIL - $1"; failures=$((failures + 1)); }

# Load only the helper functions; running doctor.sh itself would start its menu.
eval "$(sed -n \
    -e '/^read_local_config_value()/,/^}/p' \
    -e '/^SETUP_OWNED_KEYS=/p' \
    -e '/^restore_unowned_config_keys()/,/^}/p' \
    -e '/^ensure_rekt_containers()/,/^}/p' \
    -e '/^prepare_bind_directories()/,/^}/p' \
    "$REPO_ROOT/doctor.sh")"
# shellcheck source=../lib/ports.sh
source "$REPO_ROOT/tools/lib/ports.sh"

for fn in port_listen_pids kill_port_listeners port_in_use \
    read_local_config_value restore_unowned_config_keys; do
    declare -F "$fn" >/dev/null || fail "doctor.sh or tools/lib/ports.sh defines $fn"
done

# Line endings: a CRLF checkout breaks bash before doctor.sh reaches its first command.
if grep -q $'\r' "$REPO_ROOT/doctor.sh"; then
    fail "doctor.sh is checked out with LF line endings"
else
    pass "doctor.sh is checked out with LF line endings"
fi

# Port lookup and cleanup work with lsof or, on Git Bash, netstat/taskkill.
port="${DOCTOR_SMOKE_PORT:-58731}"
# Perl ships with Linux, macOS and Git for Windows, so it gives a listener everywhere.
if command -v perl >/dev/null 2>&1; then
    perl -MIO::Socket::INET -e '$s = IO::Socket::INET->new(LocalAddr => "127.0.0.1", LocalPort => $ARGV[0], Listen => 1, ReuseAddr => 1) or die; sleep 60' \
        "$port" >/dev/null 2>&1 &
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

mount_dir="$(mktemp -d)"
if (
    REPO_ROOT="$mount_dir"
    NEO4J_PASSWORD="test-only"
    NEO4J_CONTAINER="test-neo4j"
    REKT_CONTAINER="test-rekt"
    NEO4J_SERVICE="neo4j"
    REKT_SERVICE="cobol-rekt"
    RED="" GREEN="" YELLOW="" BLUE="" NC=""
    detect_docker_api_version() { :; }
    ensure_neo4j_image() { :; }
    container_exec() { return 0; }
    run_compose() {
        [[ "$*" == "up -d neo4j cobol-rekt" ]] || return 1
        [[ -d "$REPO_ROOT/source" ]] || return 1
        printf 'host-owned output\n' >"$REPO_ROOT/output/rekt/compose-write"
    }
    ensure_rekt_containers >"$mount_dir/startup.log" 2>&1 &&
        [[ "$(cat "$mount_dir/output/rekt/compose-write")" == "host-owned output" ]]
); then
    pass "REKT startup prepares writable bind sources before Compose"
else
    fail "REKT startup prepares writable bind sources before Compose"
fi

if (
    REPO_ROOT="$mount_dir/blocked"
    mkdir -p "$REPO_ROOT"
    printf 'not a directory\n' >"$REPO_ROOT/output"
    NEO4J_PASSWORD="test-only"
    RED="" BLUE="" NC=""
    detect_docker_api_version() { :; }
    ensure_neo4j_image() { :; }
    run_compose() { touch "$mount_dir/unexpected-compose"; }
    if ensure_rekt_containers >"$mount_dir/blocked.log" 2>&1; then
        exit 1
    fi
    [[ ! -e "$mount_dir/unexpected-compose" ]] &&
        [[ "$(cat "$REPO_ROOT/output")" == "not a directory" ]]
); then
    pass "REKT startup rejects invalid bind sources before starting containers"
else
    fail "REKT startup rejects invalid bind sources before starting containers"
fi

if (
    REPO_ROOT="$mount_dir/root-container"
    mkdir -p "$REPO_ROOT/source"
    NEO4J_PASSWORD="test-only"
    NEO4J_CONTAINER="test-neo4j"
    REKT_CONTAINER="test-rekt"
    NEO4J_SERVICE="neo4j"
    REKT_SERVICE="cobol-rekt"
    REKT_IMAGE="rekt-test:1"
    REKT_UID=4242 REKT_GID=4343
    RED="" GREEN="" YELLOW="" BLUE="" NC=""
    detect_docker_api_version() { :; }
    ensure_neo4j_image() { :; }
    container_exec() { return 0; }
    docker() {
        case "$*" in
            *State.Running*) echo true ;;
            *Config.Image*) echo "$REKT_IMAGE" ;;
            *Config.User*) echo "" ;;
        esac
    }
    run_compose() { printf '%s\n' "$*" >>"$mount_dir/compose-calls"; }
    ensure_rekt_containers >"$mount_dir/root-container.log" 2>&1
    [[ "$(head -1 "$mount_dir/compose-calls")" == "up -d cobol-rekt" ]] &&
        grep -q "uses 'root'; switching to host user 4242:4343" "$mount_dir/root-container.log"
); then
    pass "REKT startup recreates a running parser container whose user is not the host user"
else
    fail "REKT startup recreates a running parser container whose user is not the host user"
fi

if (
    REPO_ROOT="$mount_dir/setup"
    mkdir -p "$REPO_ROOT"/{Data,Config,tools/cobol-rekt/contracts}
    prepare_bind_directories >"$mount_dir/setup-missing.log" 2>&1 && exit 1
    grep -q "Repository-provided bind directory is missing: $REPO_ROOT/source" "$mount_dir/setup-missing.log" &&
        [[ ! -e "$REPO_ROOT/source" ]] || exit 1
    mkdir "$REPO_ROOT/source"
    prepare_bind_directories >"$mount_dir/setup.log" 2>&1 &&
        [[ -d "$REPO_ROOT/Logs" && -d "$REPO_ROOT/output/rekt" ]]
); then
    pass "setup creates runtime bind directories but never replaces repository-provided ones"
else
    fail "setup creates runtime bind directories but never replaces repository-provided ones"
fi
rm -rf "$mount_dir"

echo ""
if [[ $failures -gt 0 ]]; then
    echo "$failures check(s) failed"
    exit 1
fi
echo "All checks passed"
