#!/bin/bash

# COBOL Migration Tool - All-in-One Management Script
# ===================================================
# This script consolidates all functionality for setup, testing, running, and diagnostics

# Colors for output
RED=$'\033[0;31m'
GREEN=$'\033[0;32m'
YELLOW=$'\033[1;33m'
BLUE=$'\033[0;34m'
CYAN=$'\033[0;36m'
MAGENTA=$'\033[0;35m'
BOLD=$'\033[1m'
NC=$'\033[0m' # No Color

# Resolve the sqlite3 command, handling Windows (Git Bash / MSYS2) paths
SQLITE3_CMD=""
resolve_sqlite3() {
    if [ -n "$SQLITE3_CMD" ]; then return 0; fi
    if command -v sqlite3 >/dev/null 2>&1; then
        SQLITE3_CMD="sqlite3"
    elif command -v sqlite3.exe >/dev/null 2>&1; then
        SQLITE3_CMD="sqlite3.exe"
    else
        local winget_match
        winget_match=$(find "${LOCALAPPDATA:-/dev/null}/Microsoft/WinGet/Packages" -name "sqlite3.exe" 2>/dev/null | head -1)
        if [ -n "$winget_match" ]; then
            SQLITE3_CMD="$winget_match"
        fi
    fi
    [ -n "$SQLITE3_CMD" ]
}

# Get repository root (directory containing this script)
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Determine the preferred dotnet CLI (favor .NET 10 installations when available)
detect_dotnet_cli() {
    local default_cli="dotnet"
    local cli_candidate="$default_cli"

    # Check if default dotnet has .NET 10 runtime
    if command -v "$default_cli" >/dev/null 2>&1; then
        if "$default_cli" --list-runtimes 2>/dev/null | grep -q "Microsoft.NETCore.App 10."; then
            echo "$default_cli"
            return
        fi
    fi

    # Use whatever dotnet is available
    echo "$cli_candidate"
}

DOTNET_CMD="$(detect_dotnet_cli)"
detect_python() {
    if command -v python3 >/dev/null 2>&1; then
        echo python3
        return
    fi

    if command -v python >/dev/null 2>&1; then
        echo python
        return
    fi

    echo ""
}

PYTHON_CMD="$(detect_python)"
DEFAULT_MCP_HOST="localhost"
DEFAULT_MCP_PORT=5028

# Function to show usage
show_usage() {
    echo -e "${BOLD}${BLUE}🧠 COBOL to Java/C# Migration Tool${NC}"
    echo -e "${BLUE}==========================================${NC}"
    echo
    echo -e "${BOLD}Usage:${NC} $0 [command]"
    echo
    echo -e "${BOLD}Available Commands:${NC}"
    echo -e "  ${GREEN}setup${NC}           Interactive configuration setup"
    echo -e "  ${GREEN}test${NC}            Full system validation and testing"
    echo -e "  ${GREEN}run${NC}             Start full migration (auto-detects chunking needs)"
    echo -e "  ${GREEN}convert-only${NC}    Convert COBOL only (skips RE; prompts to reuse persisted RE context)"
    echo -e "  ${GREEN}portal${NC}          Start the web portal (documentation & monitoring)"
    echo -e "  ${GREEN}doctor${NC}          Diagnose configuration issues (default)"
    echo -e "  ${GREEN}reverse-eng${NC}     Run reverse engineering analysis only (no conversion) + Portal"
    echo -e "  ${GREEN}resume${NC}          Resume interrupted migration"
    echo -e "  ${GREEN}monitor${NC}         Monitor migration progress"
    echo -e "  ${GREEN}chunking-health${NC} Check smart chunking infrastructure"
    echo -e "  ${GREEN}validate${NC}        Validate system requirements"
    echo -e "  ${GREEN}conversation${NC}    Generate conversation log from migration data"
    echo
    echo -e "${BOLD}Examples:${NC}"
    echo -e "  $0                   ${CYAN}# Run configuration doctor${NC}"
    echo -e "  $0 setup             ${CYAN}# Interactive setup${NC}"
    echo -e "  $0 test              ${CYAN}# Test configuration and dependencies${NC}"
    echo -e "  $0 reverse-eng       ${CYAN}# Extract business logic only (no conversion) + Portal${NC}"
    echo -e "  $0 run               ${CYAN}# Full migration (auto-chunks large files)${NC}"
    echo -e "  $0 portal            ${CYAN}# Start portal to view docs & reports${NC}"
    echo -e "  $0 convert-only      ${CYAN}# Conversion only (prompts to reuse cached RE results) + UI${NC}"
    echo
    echo -e "${BOLD}Business Logic Persistence (--reuse-re):${NC}"
  echo -e "  RE results are automatically persisted to the database after each run."
  echo -e "  ${GREEN}Mode 1${NC} — Full migration (default): RE runs and context is injected into prompts."
  echo -e "  ${GREEN}Mode 2${NC} — ${GREEN}--skip-reverse-engineering${NC}: Pure conversion, no RE context."
  echo -e "  ${GREEN}Mode 3${NC} — ${GREEN}--skip-reverse-engineering --reuse-re${NC}: Loads cached RE results from"
  echo -e "          the database and injects them into conversion prompts."
  echo -e "  The ${GREEN}convert-only${NC} command prompts interactively for the --reuse-re choice."
  echo -e "  To view or delete persisted RE results, open the Portal → 🔬 RE Results button."
  echo
  echo -e "${BOLD}Smart Chunking (v0.2):${NC}"
    echo -e "  Large files (>150K chars or >3000 lines) are automatically"
    echo -e "  routed through SmartMigrationOrchestrator for optimal processing."
    echo -e "  - Full Migration: Uses ChunkedMigrationProcess for conversion"
    echo -e "  - RE-Only Mode: Uses ChunkedReverseEngineeringProcess for analysis"
    echo -e "  No manual chunking flags required - detection is automatic."
    echo
}

# Resolve the migration database path (absolute) from config or environment
get_migration_db_path() {
    local base_dir="$REPO_ROOT"

    if [[ -n "$MIGRATION_DB_PATH" ]]; then
        if [[ -z "$PYTHON_CMD" ]]; then
            echo "$MIGRATION_DB_PATH"
            return
        fi

        PY_BASE="$base_dir" PY_DB_PATH="$MIGRATION_DB_PATH" "$PYTHON_CMD" - <<'PY'
import os
base = os.environ["PY_BASE"]
path = os.environ["PY_DB_PATH"]
if not os.path.isabs(path):
    path = os.path.abspath(os.path.join(base, path))
else:
    path = os.path.abspath(path)
print(path)
PY
        return
    fi

    if [[ -z "$PYTHON_CMD" ]]; then
        if [[ -f "$base_dir/Data/migration.db" ]]; then
            echo "$base_dir/Data/migration.db"
        else
            echo ""
        fi
        return
    fi

    PY_BASE="$base_dir" "$PYTHON_CMD" - <<'PY'
import json
import os

base = os.environ["PY_BASE"]
config_path = os.path.join(base, "Config", "appsettings.json")
fallback = "Data/migration.db"
try:
    with open(config_path, "r", encoding="utf-8") as f:
        data = json.load(f)
        path = data.get("ApplicationSettings", {}).get("MigrationDatabasePath") or fallback
except FileNotFoundError:
    path = fallback

if not os.path.isabs(path):
    path = os.path.abspath(os.path.join(base, path))
else:
    path = os.path.abspath(path)

print(path)
PY
}

# Fetch the latest migration run summary from SQLite (if available)
get_latest_run_summary() {
    local db_path="$1"
    if [[ -z "$db_path" || ! -f "$db_path" ]]; then
        return 1
    fi

    if [[ -z "$PYTHON_CMD" ]]; then
        return 1
    fi

    PY_DB_PATH="$db_path" "$PYTHON_CMD" - <<'PY'
import os
import sqlite3

db_path = os.environ["PY_DB_PATH"]
if not os.path.exists(db_path):
    raise SystemExit

query = """
SELECT id, status, coalesce(completed_at, started_at)
FROM runs
ORDER BY started_at DESC
LIMIT 1
"""

with sqlite3.connect(db_path) as conn:
    row = conn.execute(query).fetchone()

if row:
    run_id, status, completed_at = row
    completed_at = completed_at or ""
    print(f"{run_id}|{status}|{completed_at}")
PY
}

open_url_in_browser() {
    local url="$1"
    local auto_open="${MCP_AUTO_OPEN:-1}"
    if [[ "$auto_open" != "1" ]]; then
        return
    fi

    case "$(uname -s)" in
        Darwin)
            if command -v open >/dev/null 2>&1; then
                open "$url" >/dev/null 2>&1 &
            fi
            ;;
        Linux)
            if command -v xdg-open >/dev/null 2>&1; then
                xdg-open "$url" >/dev/null 2>&1 &
            fi
            ;;
        CYGWIN*|MINGW*|MSYS*|Windows_NT)
            if command -v powershell.exe >/dev/null 2>&1; then
                powershell.exe -NoProfile -Command "Start-Process '$url'" >/dev/null 2>&1 &
            elif command -v cmd.exe >/dev/null 2>&1; then
                cmd.exe /c start "" "$url"
            fi
            ;;
    esac
}

launch_mcp_web_ui() {
    local db_path="$1"
    local host="${MCP_WEB_HOST:-$DEFAULT_MCP_HOST}"
    local port="${MCP_WEB_PORT:-$DEFAULT_MCP_PORT}"
    local url="http://$host:$port"

    # Ensure AI env is loaded (chat vs responses) before launching portal/MCP
    if ! load_configuration || ! load_ai_config; then
        echo -e "${RED}❌ Failed to load AI configuration. Portal launch aborted.${NC}"
        return 1
    fi

    echo ""
    echo -e "${BLUE}🌐 Launching MCP Web UI...${NC}"
    echo "================================"
    echo -e "Using database: ${BOLD}$db_path${NC}"

    if summary=$(get_latest_run_summary "$db_path" 2>/dev/null); then
        IFS='|' read -r run_id status completed_at <<<"$summary"
        echo -e "Latest migration run: ${GREEN}#${run_id}${NC} (${status})"
        if [[ -n "$completed_at" ]]; then
            echo -e "Completed at: $completed_at"
        fi
        echo ""
    fi

    echo -e "${BLUE}➡️  Starting web server at${NC} ${BOLD}$url${NC}"
    
    # Check if port is already in use and clean up (only kill the LISTEN socket owner)
    if lsof -Pi :$port -sTCP:LISTEN -t >/dev/null 2>&1; then
        echo -e "${YELLOW}⚠️  Port $port is already in use. Cleaning up...${NC}"
        local listen_pids
        listen_pids=$(lsof -Pi :$port -sTCP:LISTEN -t 2>/dev/null)
        if [[ -n "$listen_pids" ]]; then
            echo "$listen_pids" | xargs kill -9 2>/dev/null && echo -e "${GREEN}✅ Killed existing process on port $port${NC}" || true
            sleep 1
        fi
    fi
    
    echo -e "${BLUE}➡️  Press Ctrl+C to stop the UI and exit.${NC}"

    open_url_in_browser "$url"

    export MIGRATION_DB_PATH="$db_path"
    ASPNETCORE_URLS="$url" ASPNETCORE_HTTP_PORTS="$port" "$DOTNET_CMD" run --project "$REPO_ROOT/McpChatWeb"
}

# Function to launch portal in background for monitoring during migration
launch_portal_background() {
    local db_path="${1:-$REPO_ROOT/Data/migration.db}"
    local host="${MCP_WEB_HOST:-$DEFAULT_MCP_HOST}"
    local port="${MCP_WEB_PORT:-$DEFAULT_MCP_PORT}"
    local url="http://$host:$port"

    echo ""
    echo -e "${BLUE}🌐 Launching Portal in Background for Monitoring...${NC}"
    echo "===================================================="
    
    # Check if port is already in use and clean up (only kill the LISTEN socket owner)
    if lsof -Pi :$port -sTCP:LISTEN -t >/dev/null 2>&1; then
        echo -e "${YELLOW}⚠️  Port $port is already in use. Cleaning up...${NC}"
        local listen_pids
        listen_pids=$(lsof -Pi :$port -sTCP:LISTEN -t 2>/dev/null)
        if [[ -n "$listen_pids" ]]; then
            echo "$listen_pids" | xargs kill -9 2>/dev/null && echo -e "${GREEN}✅ Killed existing process on port $port${NC}" || true
            sleep 1
        fi
    fi

    # Launch portal in background
    export MIGRATION_DB_PATH="$db_path"
    ASPNETCORE_URLS="$url" ASPNETCORE_HTTP_PORTS="$port" "$DOTNET_CMD" run --project "$REPO_ROOT/McpChatWeb" > "$REPO_ROOT/Logs/portal.log" 2>&1 &
    PORTAL_PID=$!
    
    # Wait for portal to start
    echo -e "${BLUE}⏳ Waiting for portal to start...${NC}"
    local max_wait=15
    local waited=0
    while ! lsof -Pi :$port -sTCP:LISTEN -t >/dev/null 2>&1; do
        sleep 1
        waited=$((waited + 1))
        if [[ $waited -ge $max_wait ]]; then
            echo -e "${YELLOW}⚠️  Portal may not have started yet, continuing...${NC}"
            break
        fi
    done
    
    if lsof -Pi :$port -sTCP:LISTEN -t >/dev/null 2>&1; then
        echo -e "${GREEN}✅ Portal running at ${BOLD}$url${NC} (PID: $PORTAL_PID)"
        open_url_in_browser "$url"
    fi
    
    echo -e "${CYAN}📊 Monitor migration progress in portal: $url${NC}"
    echo -e "${CYAN}📄 Click 'Migration Monitor' button to see real-time progress${NC}"
    echo ""
}

# Function to stop background portal
stop_portal_background() {
    if [[ -n "$PORTAL_PID" ]] && kill -0 "$PORTAL_PID" 2>/dev/null; then
        echo -e "${BLUE}🛑 Stopping background portal (PID: $PORTAL_PID)...${NC}"
        kill "$PORTAL_PID" 2>/dev/null
        wait "$PORTAL_PID" 2>/dev/null
        echo -e "${GREEN}✅ Portal stopped${NC}"
    fi
}

# Function to run portal standalone
run_portal() {
    echo -e "${BLUE}🌐 Starting Migration Portal${NC}"
    echo "============================="
    echo ""
    echo "The portal provides:"
    echo "  • 📊 Migration monitoring and progress"
    echo "  • 📄 Architecture documentation with Mermaid diagrams"
    echo "  • 📋 Reverse engineering reports with business logic"
    echo "  • 🔄 Real-time agent chat and chunk status"
    echo ""
    
    local db_path
    if ! db_path="$(get_migration_db_path)" || [[ -z "$db_path" ]]; then
        db_path="$REPO_ROOT/Data/migration.db"
        echo -e "${YELLOW}ℹ️  Using default database path: $db_path${NC}"
    fi
    
    # Check for generated RE report
    if [[ -f "$REPO_ROOT/output/reverse-engineering-details.md" ]]; then
        echo -e "${GREEN}✅ Reverse engineering report available in portal${NC}"
    else
        echo -e "${YELLOW}ℹ️  No RE report yet - run './doctor.sh reverse-eng' first${NC}"
    fi
    echo ""
    
    launch_mcp_web_ui "$db_path"
}

# Function to load configuration
load_configuration() {
    if [[ -f "$REPO_ROOT/Config/load-config.sh" ]]; then
        source "$REPO_ROOT/Config/load-config.sh"
        return $?
    else
        echo -e "${RED}❌ Configuration loader not found: Config/load-config.sh${NC}"
        return 1
    fi
}

# Verify that configured model deployments exist on the Azure OpenAI resource
check_model_deployments() {
    echo ""
    echo -e "${BLUE}🤖 Verifying Model Deployments${NC}"
    echo "================================="

    # GitHub Models: model was already verified in check_ai_connectivity
    local service_type="${AZURE_OPENAI_SERVICE_TYPE:-AzureOpenAI}"
    local service_type_lower
    service_type_lower=$(echo "$service_type" | tr '[:upper:]' '[:lower:]')
    if [[ "$service_type_lower" == "githubcopilot" ]] || [[ "$service_type_lower" == "github" ]] || [[ "$service_type_lower" == "githubmodels" ]] || [[ "$service_type_lower" == "githubcopilotsdk" ]]; then
        local model_id="${AZURE_OPENAI_MODEL_ID}"
        echo -e "  ${GREEN}✅ Model '${model_id}' verified via GitHub Models${NC}"
        echo ""
        return 0
    fi

    local endpoint="${AZURE_OPENAI_ENDPOINT}"
    local api_key="${AZURE_OPENAI_API_KEY}"
    local has_api_key=false
    local token=""

    # Determine auth method
    if [[ -n "$api_key" ]] && [[ "$api_key" != *"your-"* ]] && [[ "$api_key" != *"placeholder"* ]] && [[ "$api_key" != *"key-placeholder"* ]]; then
        has_api_key=true
    fi

    if [[ "$has_api_key" == false ]]; then
        if command -v az >/dev/null 2>&1 && az account show >/dev/null 2>&1; then
            token=$(az account get-access-token --resource "https://cognitiveservices.azure.com" --query "accessToken" -o tsv 2>/dev/null)
            if [[ -z "$token" ]]; then
                echo -e "  ${YELLOW}⚠️  Could not obtain Azure AD token, skipping deployment check${NC}"
                return 0
            fi
        else
            echo -e "  ${YELLOW}⚠️  No auth available, skipping deployment check${NC}"
            return 0
        fi
    fi

    # Collect unique deployment names and their roles
    local code_deploy="${AISETTINGS__DEPLOYMENTNAME}"
    local chat_deploy="${AISETTINGS__CHATDEPLOYMENTNAME}"

    # Build parallel arrays (bash 3 compatible, no associative arrays)
    local deploy_names=""
    local deploy_roles=""

    if [[ -n "$code_deploy" ]]; then
        deploy_names="$code_deploy"
        deploy_roles="code model (migration agents)"
    fi
    if [[ -n "$chat_deploy" ]] && [[ "$chat_deploy" != "$code_deploy" ]]; then
        deploy_names="${deploy_names}|${chat_deploy}"
        deploy_roles="${deploy_roles}|chat model (portal & reports)"
    elif [[ -n "$chat_deploy" ]] && [[ -n "$deploy_roles" ]]; then
        deploy_roles="${deploy_roles} + chat model"
    fi

    if [[ -z "$deploy_names" ]]; then
        echo -e "  ${YELLOW}⚠️  No deployment names configured, skipping check${NC}"
        return 0
    fi

    local all_ok=true
    local api_version="2024-06-01"
    local idx=0

    IFS='|' read -ra _deploy_arr <<< "$deploy_names"
    IFS='|' read -ra _role_arr <<< "$deploy_roles"

    for deploy_name in "${_deploy_arr[@]}"; do
        local role="${_role_arr[$idx]}"
        idx=$((idx + 1))

        # Use a lightweight inference probe: POST a minimal (invalid) chat completion.
        # This only needs inference-level RBAC permissions.
        # Expected: 400 = deployment exists, 404 = not found, 200 = also exists.
        local test_url="${endpoint%/}/openai/deployments/${deploy_name}/chat/completions?api-version=${api_version}"
        local tmp_resp
        tmp_resp=$(mktemp)
        local curl_args=("-s" "-o" "$tmp_resp" "-w" "%{http_code}" "--connect-timeout" "10" "--max-time" "15")
        local http_status=""
        local post_body='{"messages":[],"max_tokens":1}'

        if [[ "$has_api_key" == true ]]; then
            http_status=$(curl "${curl_args[@]}" -X POST -H "api-key: $api_key" -H "Content-Type: application/json" -d "$post_body" "$test_url" 2>/dev/null)
        else
            http_status=$(curl "${curl_args[@]}" -X POST -H "Authorization: Bearer $token" -H "Content-Type: application/json" -d "$post_body" "$test_url" 2>/dev/null)
        fi

        local body
        body=$(cat "$tmp_resp" 2>/dev/null)
        rm -f "$tmp_resp"

        # 400 = deployment exists but our dummy request is invalid (expected)
        # 200 = deployment exists and responded
        # 404 = deployment truly not found
        if [[ "$http_status" == "200" ]] || [[ "$http_status" == "400" ]]; then
            echo -e "  ${GREEN}✅ Deployment '${deploy_name}' exists${NC} (${role})"
        elif [[ "$http_status" == "404" ]]; then
            # Check error code to distinguish "deployment not found" from "endpoint not found"
            local error_code=""
            if command -v jq >/dev/null 2>&1; then
                error_code=$(echo "$body" | jq -r '(.error.code // empty)' 2>/dev/null)
            fi
            if [[ "$error_code" == "DeploymentNotFound" ]]; then
                echo -e "  ${RED}❌ Deployment '${deploy_name}' NOT FOUND${NC} (${role})"
                echo -e "     ${YELLOW}Create this deployment in the Azure portal or update the name in Config/ai-config.local.env${NC}"
                all_ok=false
            else
                echo -e "  ${RED}❌ Deployment '${deploy_name}' NOT FOUND (HTTP 404)${NC} (${role})"
                echo -e "     ${YELLOW}Create this deployment in the Azure portal or update the name in Config/ai-config.local.env${NC}"
                all_ok=false
            fi
        elif [[ "$http_status" == "429" ]]; then
            echo -e "  ${GREEN}✅ Deployment '${deploy_name}' exists${NC} (${role}) ${YELLOW}(rate limited)${NC}"
        else
            local error_msg=""
            if command -v jq >/dev/null 2>&1; then
                error_msg=$(echo "$body" | jq -r '(.error.message // .error.code // empty)' 2>/dev/null)
            fi
            echo -e "  ${YELLOW}⚠️  Deployment '${deploy_name}' check returned HTTP ${http_status}${NC} (${role})"
            if [[ -n "$error_msg" ]]; then
                echo -e "     ${YELLOW}Error: $error_msg${NC}"
            fi
        fi
    done

    echo ""
    if [[ "$all_ok" == false ]]; then
        echo -e "  ${RED}❌ One or more model deployments are missing.${NC}"
        echo -e "  ${YELLOW}Fix: Verify deployment names in Config/ai-config.local.env match your Azure OpenAI resource.${NC}"
        return 1
    fi

    return 0
}

# ═══════════════════════════════════════════════════════════════════════
# AI PROVIDER & MODEL SELECTION
# ═══════════════════════════════════════════════════════════════════════

# Resolve a GitHub token from environment or gh CLI
resolve_github_token() {
    # 1. Explicit GITHUB_TOKEN env var
    if [[ -n "$GITHUB_TOKEN" ]]; then
        echo "$GITHUB_TOKEN"
        return 0
    fi

    # 2. gh auth token CLI
    if command -v gh >/dev/null 2>&1; then
        local token
        token=$(gh auth token 2>/dev/null)
        if [[ -n "$token" ]]; then
            echo "$token"
            return 0
        fi
    fi

    return 1
}

# ── Heartbeat monitor: shows progress while migration runs ──
# Polls the DB every 30s and prints status so the user knows it's alive.
# Usage: start_heartbeat <db_path> <dotnet_pid>
#        stop_heartbeat
HEARTBEAT_PID=""
start_heartbeat() {
    local db_path="$1"
    local target_pid="$2"
    local interval=30

    (
        local elapsed=0
        local last_analyses=0
        local last_bl=0
        local last_deps=0
        local spinner_chars='⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏'
        local spin_idx=0

        while kill -0 "$target_pid" 2>/dev/null; do
            sleep "$interval"
            elapsed=$((elapsed + interval))

            # Check if process is still alive
            if ! kill -0 "$target_pid" 2>/dev/null; then
                break
            fi

            # Query progress from DB
            local run_id
            run_id=$(sqlite3 "$db_path" "SELECT id FROM runs WHERE status='Running' ORDER BY id DESC LIMIT 1;" 2>/dev/null)

            if [[ -z "$run_id" ]]; then
                continue
            fi

            local files
            files=$(sqlite3 "$db_path" "SELECT COUNT(*) FROM cobol_files WHERE run_id=$run_id;" 2>/dev/null)
            local analyses
            analyses=$(sqlite3 "$db_path" "SELECT COUNT(*) FROM analyses a JOIN cobol_files cf ON a.cobol_file_id=cf.id WHERE cf.run_id=$run_id;" 2>/dev/null)
            local bl
            bl=$(sqlite3 "$db_path" "SELECT COUNT(*) FROM business_logic WHERE run_id=$run_id;" 2>/dev/null)
            local deps
            deps=$(sqlite3 "$db_path" "SELECT COUNT(*) FROM dependencies WHERE run_id=$run_id;" 2>/dev/null)

            local mins=$((elapsed / 60))
            local secs=$((elapsed % 60))

            # Detect phase
            local phase="Initializing"
            if [[ "$deps" -gt 0 ]]; then
                phase="Code Conversion"
            elif [[ "$bl" -gt 0 ]] && [[ "$bl" -ge "$files" ]]; then
                phase="Dependency Mapping"
            elif [[ "$bl" -gt 0 ]]; then
                phase="Business Logic ($bl/$files)"
            elif [[ "$analyses" -gt 0 ]] && [[ "$analyses" -ge "$files" ]]; then
                phase="Business Logic Extraction"
            elif [[ "$analyses" -gt 0 ]]; then
                phase="COBOL Analysis ($analyses/$files)"
            elif [[ "$files" -gt 0 ]]; then
                phase="COBOL Analysis (waiting for AI...)"
            fi

            # Spinner
            local s="${spinner_chars:$spin_idx:1}"
            spin_idx=$(( (spin_idx + 1) % ${#spinner_chars} ))

            # Print status line (overwrite previous)
            printf "\r\033[K  %s \033[0;36m[%dm%02ds]\033[0m Phase: \033[1m%s\033[0m | Files: %s | Analyses: %s | BL: %s | Deps: %s" \
                "$s" "$mins" "$secs" "$phase" "$files" "$analyses" "$bl" "$deps"

        done
        echo "" # newline after heartbeat ends
    ) &
    HEARTBEAT_PID=$!
}

stop_heartbeat() {
    if [[ -n "$HEARTBEAT_PID" ]] && kill -0 "$HEARTBEAT_PID" 2>/dev/null; then
        kill "$HEARTBEAT_PID" 2>/dev/null
        wait "$HEARTBEAT_PID" 2>/dev/null
        HEARTBEAT_PID=""
        echo "" # clean newline
    fi
}

# Interactive provider + model selection menu.
# Sets env vars so that Program.cs OverrideSettingsFromEnvironment() picks them up.
# Returns 0 on success, 1 on failure.
select_ai_provider() {
    while true; do
    echo ""
    echo -e "${BOLD}🔌 Select AI Provider${NC}"
    echo "========================================"
    echo -e "  1) Azure OpenAI    (uses your Azure deployment: ${GREEN}${AZURE_OPENAI_DEPLOYMENT_NAME:-gpt-5.1-codex-mini}${NC})"
    echo -e "  2) GitHub Models   (OpenAI, Grok via models.github.ai REST API)"
    echo -e "  3) Copilot SDK     (Claude, GPT, Grok — ${YELLOW}⚠️  slow for batch, best for single files${NC})"
    echo "  0) Back / Cancel"
    echo ""
    read -p "Enter choice (0-3) [default: 1]: " provider_choice
    provider_choice=$(echo "$provider_choice" | tr -d '[:space:]')

    # Cancel
    if [[ "$provider_choice" == "0" ]]; then
        echo -e "${YELLOW}↩ Cancelled. Returning to menu.${NC}"
        return 1
    fi

    case "$provider_choice" in
        3)
            # ── GitHub Copilot SDK path ──
            # Uses CopilotChatClient (stdio) — same auth as VS Code Copilot.
            # Gives access to ALL models including Claude.
            # Model names are simple (no vendor prefix).
            echo ""
            echo -e "${BOLD}🤖 Select Model (Copilot SDK)${NC}"
            echo "========================================"
            echo ""
            echo -e "  ${MAGENTA}── Anthropic Claude ──${NC}"
            echo "  1) claude-opus-4          Best quality, deepest reasoning"
            echo "  2) claude-sonnet-4        Balanced quality and speed"
            echo ""
            echo -e "  ${MAGENTA}── OpenAI Codex ──${NC}"
            echo "  3) gpt-5.3-codex          Code-optimized, fast reasoning"
            echo ""
            echo -e "  ${MAGENTA}── OpenAI GPT ──${NC}"
            echo "  4) gpt-5.4                Latest GPT, highest capability"
            echo "  5) gpt-5                  GPT-5"
            echo "  6) gpt-4.1                General purpose"
            echo "  7) gpt-4o                 Multimodal, fast"
            echo ""
            echo -e "  ${MAGENTA}── xAI Grok ──${NC}"
            echo "  8) grok-3                 Fast reasoning"
            echo "  9) grok-3-mini            Fastest, lightweight"
            echo ""
            echo " 10) Custom (enter model name)"
            echo "  0) Back"
            echo ""
            read -p "Enter choice (0-10) [default: 3]: " model_choice
            model_choice=$(echo "$model_choice" | tr -d '[:space:]')

            if [[ "$model_choice" == "0" ]]; then continue; fi

            local selected_model=""
            case "$model_choice" in
                1)    selected_model="claude-opus-4" ;;
                2)    selected_model="claude-sonnet-4" ;;
                3|"") selected_model="gpt-5.3-codex" ;;
                4)    selected_model="gpt-5.4" ;;
                5)    selected_model="gpt-5" ;;
                6)    selected_model="gpt-4.1" ;;
                7)    selected_model="gpt-4o" ;;
                8)    selected_model="grok-3" ;;
                9)    selected_model="grok-3-mini" ;;
                10)
                    read -p "Enter model name (e.g. claude-opus-4, gpt-4o): " custom_model
                    custom_model=$(echo "$custom_model" | tr -d '[:space:]')
                    if [[ -z "$custom_model" ]]; then
                        echo -e "${YELLOW}No model entered, going back...${NC}"
                        continue
                    fi
                    selected_model="$custom_model"
                    ;;
                *)
                    echo -e "${YELLOW}Invalid choice, try again${NC}"
                    continue
                    ;;
            esac

            # Set env vars for Program.cs — CopilotSDK needs ServiceType + ModelId
            export AZURE_OPENAI_SERVICE_TYPE="GitHubCopilotSDK"
            export AZURE_OPENAI_MODEL_ID="$selected_model"
            export AZURE_OPENAI_DEPLOYMENT_NAME="$selected_model"
            export AISETTINGS__MODELID="$selected_model"
            export AISETTINGS__DEPLOYMENTNAME="$selected_model"

            # Per-agent model overrides — must set BOTH naming conventions
            # (AZURE_OPENAI_* read by OverrideSettingsFromEnvironment, AISETTINGS__* by .NET config binding)
            export AZURE_OPENAI_COBOL_ANALYZER_MODEL="$selected_model"
            export AZURE_OPENAI_JAVA_CONVERTER_MODEL="$selected_model"
            export AZURE_OPENAI_UNIT_TEST_MODEL="$selected_model"
            export AZURE_OPENAI_DEPENDENCY_MAPPER_MODEL="$selected_model"
            export AISETTINGS__COBOLANALYZERMODELID="$selected_model"
            export AISETTINGS__JAVACONVERTERMODELID="$selected_model"
            export AISETTINGS__UNITTESTMODELID="$selected_model"
            export AISETTINGS__DEPENDENCYMAPPERMODELID="$selected_model"

            # Chat model also needs to use Copilot SDK
            export AZURE_OPENAI_CHAT_MODEL_ID="$selected_model"
            export AZURE_OPENAI_CHAT_DEPLOYMENT_NAME="$selected_model"
            export AISETTINGS__CHATMODELID="$selected_model"
            export AISETTINGS__CHATDEPLOYMENTNAME="$selected_model"

            # ── Force sequential processing for Copilot SDK ──
            # The stdio pipe deadlocks with concurrent sessions.
            # Sequential is slower but actually completes.
            export AI_MAX_PARALLEL_CONVERSION="1"
            export AI_MAX_PARALLEL_ANALYSIS="1"
            export AI_MAX_PARALLEL_CHUNKS="1"

            echo ""
            echo -e "${GREEN}✅ Provider: GitHub Copilot SDK (CLI)${NC}"
            echo -e "${GREEN}✅ Model: ${BOLD}$selected_model${NC}"
            echo -e "${GREEN}✅ Auth: Copilot CLI (same as VS Code)${NC}"
            echo -e "${YELLOW}ℹ️  Sequential processing enabled (Copilot SDK requires 1 request at a time)${NC}"
            ;;

        2)
            # ── GitHub Models path ──
            # IMPORTANT: GitHub Models requires vendor-prefixed model names (e.g. openai/gpt-4o)
            echo ""
            echo -e "${BOLD}🤖 Select GitHub Model${NC}"
            echo "========================================"
            echo ""
            echo -e "  ${MAGENTA}── OpenAI GPT ──${NC}"
            echo "  1) openai/gpt-5           Latest GPT-5, highest capability"
            echo "  2) openai/gpt-4.1         GPT 4.1, general purpose"
            echo "  3) openai/gpt-4o          Multimodal, fast"
            echo "  4) openai/gpt-4o-mini     Lightweight, fastest GPT"
            echo ""
            echo -e "  ${MAGENTA}── xAI Grok ──${NC}"
            echo "  5) xai/grok-3             Fast reasoning"
            echo "  6) xai/grok-3-mini        Fastest, lightweight"
            echo ""
            echo "  7) Custom (enter exact vendor/model name)"
            echo "  0) Back"
            echo ""
            read -p "Enter choice (0-7) [default: 1]: " model_choice
            model_choice=$(echo "$model_choice" | tr -d '[:space:]')

            if [[ "$model_choice" == "0" ]]; then continue; fi

            local selected_model=""
            case "$model_choice" in
                1|"") selected_model="openai/gpt-5" ;;
                2)    selected_model="openai/gpt-4.1" ;;
                3)    selected_model="openai/gpt-4o" ;;
                4)    selected_model="openai/gpt-4o-mini" ;;
                5)    selected_model="xai/grok-3" ;;
                6)    selected_model="xai/grok-3-mini" ;;
                7)
                    echo -e "  ${CYAN}Format: vendor/model (e.g. openai/gpt-4o, xai/grok-3)${NC}"
                    read -p "Enter exact model name from GitHub Models catalog: " custom_model
                    custom_model=$(echo "$custom_model" | tr -d '[:space:]')
                    if [[ -z "$custom_model" ]]; then
                        echo -e "${YELLOW}No model entered, going back...${NC}"
                        continue
                    fi
                    selected_model="$custom_model"
                    ;;
                *)
                    echo -e "${YELLOW}Invalid choice, try again${NC}"
                    continue
                    ;;
            esac

            # Resolve GitHub token
            local gh_token
            gh_token=$(resolve_github_token)
            if [[ -z "$gh_token" ]]; then
                echo ""
                echo -e "${RED}❌ No GitHub token found!${NC}"
                echo "  Set GITHUB_TOKEN env var or run: gh auth login"
                return 1
            fi

            # Set env vars for Program.cs
            export AZURE_OPENAI_SERVICE_TYPE="GitHubCopilot"
            export AZURE_OPENAI_ENDPOINT="https://models.github.ai/inference"
            export AZURE_OPENAI_API_KEY="$gh_token"
            export AZURE_OPENAI_DEPLOYMENT_NAME="$selected_model"
            export AZURE_OPENAI_MODEL_ID="$selected_model"
            export AISETTINGS__ENDPOINT="https://models.github.ai/inference"
            export AISETTINGS__APIKEY="$gh_token"
            export AISETTINGS__DEPLOYMENTNAME="$selected_model"
            export AISETTINGS__MODELID="$selected_model"
            export GITHUB_TOKEN="$gh_token"

            # Per-agent model overrides — must set BOTH naming conventions
            export AZURE_OPENAI_COBOL_ANALYZER_MODEL="$selected_model"
            export AZURE_OPENAI_JAVA_CONVERTER_MODEL="$selected_model"
            export AZURE_OPENAI_UNIT_TEST_MODEL="$selected_model"
            export AZURE_OPENAI_DEPENDENCY_MAPPER_MODEL="$selected_model"
            export AISETTINGS__COBOLANALYZERMODELID="$selected_model"
            export AISETTINGS__JAVACONVERTERMODELID="$selected_model"
            export AISETTINGS__UNITTESTMODELID="$selected_model"
            export AISETTINGS__DEPENDENCYMAPPERMODELID="$selected_model"

            # Chat model also uses same provider
            export AZURE_OPENAI_CHAT_MODEL_ID="$selected_model"
            export AZURE_OPENAI_CHAT_DEPLOYMENT_NAME="$selected_model"
            export AZURE_OPENAI_CHAT_API_KEY="$gh_token"
            export AISETTINGS__CHATMODELID="$selected_model"
            export AISETTINGS__CHATDEPLOYMENTNAME="$selected_model"
            export AISETTINGS__CHATAPIKEY="$gh_token"

            local masked_token="${gh_token:0:4}...${gh_token: -4}"
            echo ""
            echo -e "${GREEN}✅ Provider: GitHub Models${NC}"
            echo -e "${GREEN}✅ Model: ${BOLD}$selected_model${NC}"
            echo -e "${GREEN}✅ Token: $masked_token${NC}"
            echo -e "${GREEN}✅ Endpoint: https://models.github.ai/inference${NC}"
            ;;

        1|"")
            # ── Azure OpenAI path — keep existing configuration ──
            echo -e "${GREEN}✅ Provider: Azure OpenAI${NC}"
            echo -e "${GREEN}✅ Deployment: ${AZURE_OPENAI_DEPLOYMENT_NAME:-gpt-5.1-codex-mini}${NC}"
            echo -e "${GREEN}✅ Endpoint: ${AZURE_OPENAI_ENDPOINT}${NC}"
            ;;

        *)
            echo -e "${YELLOW}Invalid choice, try again${NC}"
            continue
            ;;
    esac

    echo ""
    return 0
    done
}

# Pre-check: verify AI connectivity via API key or Azure AD (Entra ID) auth
check_ai_connectivity() {
    echo ""
    echo -e "${BLUE}🔌 Pre-Check: AI Service Connectivity${NC}"
    echo "======================================="

    local service_type="${AZURE_OPENAI_SERVICE_TYPE:-AzureOpenAI}"
    local service_type_lower
    service_type_lower=$(echo "$service_type" | tr '[:upper:]' '[:lower:]')

    # GitHub Copilot SDK: authentication handled by CLI, no REST endpoint needed
    if [[ "$service_type_lower" == "githubcopilotsdk" ]]; then
        echo -e "  Provider: ${GREEN}GitHub Copilot SDK (CLI)${NC}"

        if command -v github-copilot-cli >/dev/null 2>&1; then
            echo -e "  github-copilot-cli: ${GREEN}✅ found$(NC)"
        elif command -v gh >/dev/null 2>&1 && gh copilot --help >/dev/null 2>&1; then
            echo -e "  gh copilot extension: ${GREEN}✅ found${NC}"
        else
            echo -e "  ${YELLOW}⚠️  github-copilot-cli not found in PATH. The SDK will try to locate it automatically.${NC}"
        fi

        local model_id="${AZURE_OPENAI_MODEL_ID}"
        if [[ -n "$model_id" ]]; then
            echo -e "  Model ID: ${GREEN}$model_id${NC}"
        else
            echo -e "  ${RED}❌ AZURE_OPENAI_MODEL_ID is not set. Required for Copilot SDK.${NC}"
            return 1
        fi

        echo -e "  ${GREEN}✅ GitHub Copilot SDK configuration OK${NC}"
        echo ""
        return 0
    fi

    # GitHub Models (GitHubCopilot / GitHub / GitHubModels): test models.github.ai
    if [[ "$service_type_lower" == "githubcopilot" ]] || [[ "$service_type_lower" == "github" ]] || [[ "$service_type_lower" == "githubmodels" ]]; then
        echo -e "  Provider: ${GREEN}GitHub Models (models.github.ai)${NC}"

        local gh_token="${AZURE_OPENAI_API_KEY}"
        local model_id="${AZURE_OPENAI_MODEL_ID}"

        if [[ -z "$gh_token" ]]; then
            echo -e "  ${RED}❌ No GitHub token set. Run 'gh auth login' or set GITHUB_TOKEN.${NC}"
            return 1
        fi

        local masked="${gh_token:0:4}...${gh_token: -4}"
        echo -e "  Token: ${GREEN}$masked${NC}"
        echo -e "  Model: ${GREEN}$model_id${NC}"

        # Test connectivity with a minimal chat completion request
        local test_url="https://models.github.ai/inference/chat/completions"
        local post_body="{\"model\":\"$model_id\",\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}],\"max_tokens\":1}"
        local tmp_resp
        tmp_resp=$(mktemp)
        local http_status
        http_status=$(curl -s -o "$tmp_resp" -w "%{http_code}" --connect-timeout 10 --max-time 15 \
            -X POST \
            -H "Authorization: Bearer $gh_token" \
            -H "Content-Type: application/json" \
            -d "$post_body" \
            "$test_url" 2>/dev/null)

        local body
        body=$(cat "$tmp_resp" 2>/dev/null)
        rm -f "$tmp_resp"

        echo -ne "  Connection: "
        if [[ "$http_status" == "200" ]]; then
            echo -e "${GREEN}✅ OK (HTTP 200)${NC}"
        elif [[ "$http_status" == "400" ]]; then
            # 400 = model exists but test request params are invalid (expected for reasoning models)
            echo -e "${GREEN}✅ OK (model exists, HTTP 400 on test request)${NC}"
        elif [[ "$http_status" == "429" ]]; then
            echo -e "${GREEN}✅ OK (rate limited, model exists)${NC}"
        elif [[ "$http_status" == "401" ]] || [[ "$http_status" == "403" ]]; then
            echo -e "${RED}❌ Authentication failed (HTTP $http_status)${NC}"
            echo -e "  ${YELLOW}Your GitHub token may be invalid. Run: gh auth login${NC}"
            return 1
        elif [[ "$http_status" == "404" ]]; then
            local error_msg=""
            if command -v jq >/dev/null 2>&1; then
                error_msg=$(echo "$body" | jq -r '(.error.message // .error // empty)' 2>/dev/null)
            fi
            echo -e "${RED}❌ Model not found (HTTP 404)${NC}"
            if [[ -n "$error_msg" ]]; then
                echo -e "  ${YELLOW}Error: $error_msg${NC}"
            fi
            echo -e "  ${YELLOW}Model '$model_id' is not available with your token.${NC}"
            echo -e "  ${YELLOW}GitHub Models requires vendor-prefixed names (e.g. openai/gpt-4o, xai/grok-3).${NC}"
            echo ""

            # Probe a few known models to show what's available
            echo -e "  ${BLUE}Probing known models to check availability...${NC}"
            local probe_models=("openai/gpt-5" "openai/gpt-4.1" "openai/gpt-4o" "openai/gpt-4o-mini" "xai/grok-3" "xai/grok-3-mini")
            local available_models=""
            for probe_model in "${probe_models[@]}"; do
                local probe_body="{\"model\":\"$probe_model\",\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}],\"max_tokens\":1}"
                local probe_status
                probe_status=$(curl -s -o /dev/null -w "%{http_code}" --connect-timeout 5 --max-time 10 \
                    -X POST \
                    -H "Authorization: Bearer $gh_token" \
                    -H "Content-Type: application/json" \
                    -d "$probe_body" \
                    "$test_url" 2>/dev/null)
                if [[ "$probe_status" == "200" ]] || [[ "$probe_status" == "400" ]] || [[ "$probe_status" == "429" ]]; then
                    echo -e "    ${GREEN}✅${NC} $probe_model"
                    available_models="$available_models $probe_model"
                fi
            done

            if [[ -n "$available_models" ]]; then
                echo ""
                echo -e "  ${YELLOW}Re-run and pick one of the above models, or use 'Custom' with the correct vendor/model name.${NC}"
            else
                echo -e "  ${YELLOW}No models responded. Your token may lack GitHub Models access.${NC}"
                echo -e "  ${YELLOW}Check: https://github.com/marketplace/models${NC}"
            fi
            return 1
        else
            echo -e "${RED}❌ FAILED (HTTP $http_status)${NC}"
            if [[ -n "$body" ]]; then
                echo -e "  ${YELLOW}Response: $(echo "$body" | head -c 200)${NC}"
            fi
            return 1
        fi

        echo ""
        return 0
    fi

    local endpoint="${AZURE_OPENAI_ENDPOINT}"
    local api_key="${AZURE_OPENAI_API_KEY}"
    local deployment="${AZURE_OPENAI_DEPLOYMENT_NAME}"

    # Determine authentication method
    local auth_method=""
    local has_api_key=false
    local has_azure_ad=false

    # Check for valid API key
    if [[ -n "$api_key" ]] && [[ "$api_key" != *"your-"* ]] && [[ "$api_key" != *"placeholder"* ]] && [[ "$api_key" != *"key-placeholder"* ]]; then
        has_api_key=true
        auth_method="API Key"
    fi

    # Check for Azure AD / Entra ID login
    if command -v az >/dev/null 2>&1; then
        if az account show >/dev/null 2>&1; then
            has_azure_ad=true
            local az_account
            az_account=$(az account show --query "{name:name, user:user.name}" -o tsv 2>/dev/null)
            if [[ "$has_api_key" == true ]]; then
                auth_method="API Key (Azure AD also available)"
            else
                auth_method="Azure AD (Entra ID)"
            fi
        fi
    fi

    # Fail if neither auth method is available
    if [[ "$has_api_key" == false ]] && [[ "$has_azure_ad" == false ]]; then
        echo -e "${RED}❌ No valid authentication found!${NC}"
        echo ""
        echo "  You must configure one of the following:"
        echo "    1) Set a valid API key in Config/ai-config.local.env"
        echo "    2) Log in via Azure CLI: az login"
        echo ""
        echo "  For API key setup:  ./doctor.sh setup"
        echo "  For Azure AD setup: az login && ./doctor.sh run"
        return 1
    fi

    echo -e "  Auth method: ${GREEN}$auth_method${NC}"
    if [[ "$has_azure_ad" == true ]]; then
        local az_user
        az_user=$(az account show --query "user.name" -o tsv 2>/dev/null)
        local az_sub
        az_sub=$(az account show --query "name" -o tsv 2>/dev/null)
        echo -e "  Azure account: ${GREEN}$az_user${NC} (${az_sub})"
    fi

    # Connection check: attempt a lightweight request to the endpoint
    if [[ -z "$endpoint" ]] || [[ "$endpoint" == *"your-"* ]]; then
        echo -e "${RED}❌ Endpoint not configured. Update AZURE_OPENAI_ENDPOINT in Config/ai-config.local.env${NC}"
        return 1
    fi

    echo -e "  Endpoint: ${GREEN}$endpoint${NC}"
    echo -ne "  Connection: "

    # Build the test URL — try to reach the endpoint root or models list
    local test_url="${endpoint%/}/openai/models?api-version=2024-06-01"
    local http_status=""
    local response_body=""
    local tmp_response
    tmp_response=$(mktemp)
    local curl_args=("-s" "-o" "$tmp_response" "-w" "%{http_code}" "--connect-timeout" "10" "--max-time" "15")

    if [[ "$has_api_key" == true ]]; then
        http_status=$(curl "${curl_args[@]}" -H "api-key: $api_key" "$test_url" 2>/dev/null)
    elif [[ "$has_azure_ad" == true ]]; then
        local token
        token=$(az account get-access-token --resource "https://cognitiveservices.azure.com" --query "accessToken" -o tsv 2>/dev/null)
        if [[ -n "$token" ]]; then
            http_status=$(curl "${curl_args[@]}" -H "Authorization: Bearer $token" "$test_url" 2>/dev/null)
        else
            rm -f "$tmp_response"
            echo -e "${YELLOW}⚠️  Could not obtain Azure AD token for Cognitive Services${NC}"
            echo -e "  ${YELLOW}Try: az login --scope https://cognitiveservices.azure.com/.default${NC}"
            return 1
        fi
    fi

    response_body=$(cat "$tmp_response" 2>/dev/null)
    rm -f "$tmp_response"

    # Extract a human-readable error message from the JSON response (if any)
    local error_msg=""
    if [[ -n "$response_body" ]]; then
        # Try jq first, fall back to grep/sed
        if command -v jq >/dev/null 2>&1; then
            error_msg=$(echo "$response_body" | jq -r '(.error.message // .error.code // .message // empty)' 2>/dev/null)
        fi
        if [[ -z "$error_msg" ]]; then
            error_msg=$(echo "$response_body" | grep -o '"message":"[^"]*"' | head -1 | sed 's/"message":"//;s/"$//')
        fi
    fi

    if [[ -z "$http_status" ]] || [[ "$http_status" == "000" ]]; then
        echo -e "${RED}❌ FAILED (could not reach endpoint)${NC}"
        echo -e "  ${YELLOW}Check that the endpoint URL is correct and accessible from this network.${NC}"
        return 1
    elif [[ "$http_status" == "200" ]]; then
        echo -e "${GREEN}✅ OK (HTTP $http_status)${NC}"
    elif [[ "$http_status" == "401" ]] || [[ "$http_status" == "403" ]]; then
        echo -e "${RED}❌ FAILED (HTTP $http_status - authentication rejected)${NC}"
        if [[ -n "$error_msg" ]]; then
            echo -e "  ${YELLOW}Error: $error_msg${NC}"
        elif [[ -n "$response_body" ]]; then
            echo -e "  ${YELLOW}Response: $response_body${NC}"
        fi
        if [[ "$has_api_key" == true ]]; then
            echo -e "  ${YELLOW}Your API key may be invalid or expired. Update Config/ai-config.local.env.${NC}"
        else
            echo -e "  ${YELLOW}Your Azure AD token lacks permissions. Check RBAC role assignments.${NC}"
        fi
        return 1
    elif [[ "$http_status" == "404" ]]; then
        # 404 on models endpoint is acceptable — the endpoint is reachable
        echo -e "${GREEN}✅ OK (endpoint reachable, HTTP $http_status on models list)${NC}"
    else
        # Other status codes (e.g. 400, 429, 500) — endpoint is reachable but request failed
        echo -e "${RED}❌ FAILED (HTTP $http_status)${NC}"
        if [[ -n "$error_msg" ]]; then
            echo -e "  ${YELLOW}Error: $error_msg${NC}"
        elif [[ -n "$response_body" ]]; then
            echo -e "  ${YELLOW}Response: $response_body${NC}"
        fi
        return 1
    fi

    echo ""

    # Verify model deployments exist
    if ! check_model_deployments; then
        return 1
    fi

    return 0
}

# Function for configuration doctor (original functionality)
run_doctor() {
    echo -e "${BLUE}🏥 Configuration Doctor - COBOL Migration Tool${NC}"
    echo "=============================================="
    echo

    # Check if configuration files exist
    echo -e "${BLUE}📋 Checking Configuration Files...${NC}"
    echo

    config_files_ok=true

    # Check template configuration
    if [[ -f "$REPO_ROOT/Config/ai-config.env" ]]; then
        echo -e "${GREEN}✅ Template configuration found: Config/ai-config.env${NC}"
    else
        echo -e "${RED}❌ Missing template configuration: Config/ai-config.env${NC}"
        config_files_ok=false
    fi

    # Check local configuration
    if [[ -f "$REPO_ROOT/Config/ai-config.local.env" ]]; then
        echo -e "${GREEN}✅ Local configuration found: Config/ai-config.local.env${NC}"
        local_config_exists=true
    else
        echo -e "${YELLOW}⚠️  Missing local configuration: Config/ai-config.local.env${NC}"
        local_config_exists=false
    fi

    # Check configuration loader
    if [[ -f "$REPO_ROOT/Config/load-config.sh" ]]; then
        echo -e "${GREEN}✅ Configuration loader found: Config/load-config.sh${NC}"
    else
        echo -e "${RED}❌ Missing configuration loader: Config/load-config.sh${NC}"
        config_files_ok=false
    fi

    # Check appsettings.json
    if [[ -f "$REPO_ROOT/Config/appsettings.json" ]]; then
        echo -e "${GREEN}✅ Application settings found: Config/appsettings.json${NC}"
    else
        echo -e "${RED}❌ Missing application settings: Config/appsettings.json${NC}"
        config_files_ok=false
    fi

    echo

    # Check reverse engineering components
    echo -e "${BLUE}🔍 Checking Reverse Engineering Components...${NC}"
    echo

    # Check models
    if [[ -f "$REPO_ROOT/Models/BusinessLogic.cs" ]]; then
        echo -e "${GREEN}✅ BusinessLogic model found${NC}"
    else
        echo -e "${YELLOW}⚠️  Missing BusinessLogic model (optional feature)${NC}"
    fi

    # Check agents
    if [[ -f "$REPO_ROOT/Agents/BusinessLogicExtractorAgent.cs" ]]; then
        echo -e "${GREEN}✅ BusinessLogicExtractorAgent found${NC}"
    else
        echo -e "${YELLOW}⚠️  Missing BusinessLogicExtractorAgent (optional feature)${NC}"
    fi

    # Check process
    if [[ -f "$REPO_ROOT/Processes/ReverseEngineeringProcess.cs" ]]; then
        echo -e "${GREEN}✅ ReverseEngineeringProcess found${NC}"
    else
        echo -e "${YELLOW}⚠️  Missing ReverseEngineeringProcess (optional feature)${NC}"
    fi

    # Check documentation
    if [[ -f "$REPO_ROOT/docs/REVERSE_ENGINEERING_ARCHITECTURE.md" ]]; then
        echo -e "${GREEN}✅ Reverse engineering architecture documentation found${NC}"
    else
        echo -e "${YELLOW}⚠️  Missing reverse engineering architecture documentation${NC}"
    fi

    # Check for generated RE report
    if [[ -f "$REPO_ROOT/output/reverse-engineering-details.md" ]]; then
        echo -e "${GREEN}✅ Generated reverse engineering report found${NC}"
    else
        echo -e "${YELLOW}ℹ️  No generated RE report yet (run reverse engineering first)${NC}"
    fi

    echo

    # If local config doesn't exist, offer to create it
    if [[ "$local_config_exists" == false ]]; then
        echo -e "${YELLOW}🔧 Local Configuration Setup${NC}"
        echo "----------------------------"
        echo "You need a local configuration file with your AI service credentials."
        echo
        read -p "Would you like me to create Config/ai-config.local.env from the template? (y/n): " create_local
        
        if [[ "$create_local" =~ ^[Yy]$ ]]; then
            if [[ -f "$REPO_ROOT/Config/ai-config.local.env.example" ]]; then
                cp "$REPO_ROOT/Config/ai-config.local.env.example" "$REPO_ROOT/Config/ai-config.local.env"
                echo -e "${GREEN}✅ Created Config/ai-config.local.env from example${NC}"
                echo -e "${YELLOW}⚠️  You must edit this file with your actual AI service credentials before running the migration tool.${NC}"
                local_config_exists=true
            else
                echo -e "${RED}❌ Example file not found: Config/ai-config.local.env.example${NC}"
            fi
        fi
        echo
    fi

    # Load and validate configuration if local config exists
    if [[ "$local_config_exists" == true ]]; then
        echo -e "${BLUE}🔍 Validating Configuration Content...${NC}"
        echo
        
        # Source the configuration loader
        if load_configuration && load_ai_config 2>/dev/null; then
            
            local service_type="${AZURE_OPENAI_SERVICE_TYPE:-AzureOpenAI}"
            local service_type_lower
            service_type_lower=$(echo "$service_type" | tr '[:upper:]' '[:lower:]')

            # ── GitHub Copilot SDK path: only need MODEL_ID ──
            if [[ "$service_type_lower" == "githubcopilotsdk" ]]; then
                echo -e "${CYAN}Provider: GitHub Copilot SDK (CLI)${NC}"
                echo
                config_valid=true

                model_id_val="${AZURE_OPENAI_MODEL_ID}"
                if [[ -z "$model_id_val" ]]; then
                    echo -e "  ${RED}❌ AZURE_OPENAI_MODEL_ID is not set${NC}"
                    config_valid=false
                else
                    echo -e "  ${GREEN}✅ AZURE_OPENAI_MODEL_ID: $model_id_val${NC}"
                fi

                if command -v github-copilot-cli >/dev/null 2>&1; then
                    echo -e "  ${GREEN}✅ github-copilot-cli found${NC}"
                else
                    echo -e "  ${YELLOW}⚠️  github-copilot-cli not in PATH (SDK will try to locate it)${NC}"
                fi

                echo
                if [[ "$config_valid" == true ]]; then
                    echo -e "${GREEN}🎉 GitHub Copilot SDK configuration valid!${NC}"
                else
                    echo -e "${YELLOW}⚠️  Configuration needs attention${NC}"
                fi
            else
            # ── Standard provider path (AzureOpenAI / GitHubCopilot / OpenAI) ──
            
            # Check required variables
            config_valid=true

            # --- Core: Endpoint (required) ---
            echo -e "${CYAN}Endpoint:${NC}"
            endpoint_val="${AZURE_OPENAI_ENDPOINT}"
            if [[ -z "$endpoint_val" ]]; then
                echo -e "  ${RED}❌ AZURE_OPENAI_ENDPOINT is not set${NC}"
                config_valid=false
            elif [[ "$endpoint_val" == *"your-"* ]] || [[ "$endpoint_val" == *"placeholder"* ]]; then
                echo -e "  ${YELLOW}⚠️  AZURE_OPENAI_ENDPOINT contains placeholder: $endpoint_val${NC}"
                config_valid=false
            else
                echo -e "  ${GREEN}✅ AZURE_OPENAI_ENDPOINT: $endpoint_val${NC}"
            fi
            echo

            # --- Authentication ---
            echo -e "${CYAN}Authentication:${NC}"
            api_key_val="${AZURE_OPENAI_API_KEY}"
            if [[ -n "$api_key_val" ]] && [[ "$api_key_val" != *"your-"* ]] && [[ "$api_key_val" != *"placeholder"* ]] && [[ "$api_key_val" != *"key-placeholder"* ]]; then
                masked_key="${api_key_val:0:4}...${api_key_val: -4}"
                echo -e "  ${GREEN}✅ API Key: $masked_key${NC}"
            elif command -v az >/dev/null 2>&1 && az account show >/dev/null 2>&1; then
                local az_user
                az_user=$(az account show --query "user.name" -o tsv 2>/dev/null)
                echo -e "  ${GREEN}✅ Azure AD (Entra ID): $az_user${NC}"
            else
                echo -e "  ${RED}❌ No valid auth: set API key in ai-config.local.env or run 'az login'${NC}"
                config_valid=false
            fi
            echo

            # --- Code Model (Responses API - used by migration agents) ---
            echo -e "${CYAN}Code Model (migration agents):${NC}"
            code_vars=("AISETTINGS__DEPLOYMENTNAME" "AISETTINGS__MODELID")
            for var in "${code_vars[@]}"; do
                value="${!var}"
                if [[ -z "$value" ]]; then
                    echo -e "  ${RED}❌ $var is not set${NC}"
                    config_valid=false
                elif [[ "$value" == *"your-"* ]] || [[ "$value" == *"placeholder"* ]]; then
                    echo -e "  ${YELLOW}⚠️  $var contains placeholder: $value${NC}"
                    config_valid=false
                else
                    echo -e "  ${GREEN}✅ $var: $value${NC}"
                fi
            done
            echo

            # --- Chat Model (Chat Completions API - used by portal & reports) ---
            echo -e "${CYAN}Chat Model (portal & reports):${NC}"
            chat_vars=("AISETTINGS__CHATDEPLOYMENTNAME" "AISETTINGS__CHATMODELID")
            for var in "${chat_vars[@]}"; do
                value="${!var}"
                if [[ -z "$value" ]]; then
                    echo -e "  ${YELLOW}⚠️  $var is not set (will fall back to code model)${NC}"
                else
                    echo -e "  ${GREEN}✅ $var: $value${NC}"
                fi
            done

            # --- Agent-specific model overrides (optional) ---
            echo
            echo -e "${CYAN}Agent model overrides (optional):${NC}"
            agent_vars=("AZURE_OPENAI_COBOL_ANALYZER_MODEL" "AZURE_OPENAI_JAVA_CONVERTER_MODEL" "AZURE_OPENAI_DEPENDENCY_MAPPER_MODEL" "AZURE_OPENAI_UNIT_TEST_MODEL")
            for var in "${agent_vars[@]}"; do
                value="${!var}"
                if [[ -n "$value" ]]; then
                    echo -e "  ${GREEN}✅ $var: $value${NC}"
                else
                    echo -e "  ${BLUE}ℹ️  $var: (defaults to code model)${NC}"
                fi
            done
            
            echo
            
            if [[ "$config_valid" == true ]]; then
                echo -e "${GREEN}🎉 Configuration validation successful!${NC}"
                echo
                echo "Your configuration is ready to use. You can now run:"
                echo "  ./doctor.sh run"
                echo "  ./doctor.sh test"
                echo "  dotnet run"
            else
                echo -e "${YELLOW}⚠️  Configuration needs attention${NC}"
                echo
                echo "Next steps:"
                echo "1. Edit Config/ai-config.local.env"
                echo "2. Replace template placeholders with your actual AI service credentials"
                echo "3. Run this doctor script again to validate"
                echo
                echo "Need help? Run: ./doctor.sh setup"
            fi
            fi  # end standard provider path
        else
            echo -e "${RED}❌ Failed to load configuration${NC}"
        fi
    fi

    echo
    echo -e "${BLUE}🔧 Available Commands${NC}"
    echo "===================="
    echo "• ./doctor.sh setup         - Interactive configuration setup"
    echo "• ./doctor.sh test          - Full system validation"
    echo "• ./doctor.sh run           - Start migration"
    echo "• ./doctor.sh reverse-eng   - Run reverse engineering only"
    echo "• ./doctor.sh portal        - Start the web portal"
    echo ""
    echo -e "${BLUE}📄 Documentation${NC}"
    echo "=================="
    echo "• output/reverse-engineering-details.md - Generated business logic report"
    echo ""
    echo -e "${BLUE}🌐 Portal Documentation${NC}"
    echo "========================"
    echo "• Start portal: ./doctor.sh portal (or cd McpChatWeb && dotnet run)"

    echo
    echo -e "${BLUE}💡 Troubleshooting Tips${NC}"
    echo "======================"
    echo "• Make sure your AI service endpoint is deployed and accessible"
    echo "• Verify your model deployment names match your provider setup"
    echo "• Check that your API key has proper permissions (or Azure AD login is active)"
    echo "• Ensure your endpoint URL is correct (should end with /)"

    echo
    echo "Configuration doctor completed!"
}

# Function to generate migration report
generate_migration_report() {
    echo -e "${BLUE}📝 Generating Migration Report...${NC}"
    
    if ! resolve_sqlite3; then
        echo -e "${RED}❌ sqlite3 is not installed. Install it to generate reports.${NC}"
        echo -e "${YELLOW}   macOS:   brew install sqlite3${NC}"
        echo -e "${YELLOW}   Linux:   sudo apt install sqlite3${NC}"
        echo -e "${YELLOW}   Windows: winget install SQLite.SQLite  (then restart your terminal)${NC}"
        return 1
    fi

    local db_path="$REPO_ROOT/Data/migration.db"
    
    if [ ! -f "$db_path" ]; then
        echo -e "${RED}❌ Migration database not found at: $db_path${NC}"
        return 1
    fi
    
    # Get the latest run ID
    local run_id=$($SQLITE3_CMD "$db_path" "SELECT MAX(run_id) FROM cobol_files;")
    
    if [ -z "$run_id" ]; then
        echo -e "${RED}❌ No migration runs found in database${NC}"
        return 1
    fi
    
    echo -e "${GREEN}✅ Found run ID: $run_id${NC}"
    echo "Generating comprehensive report..."
    
    local output_dir="$REPO_ROOT/output"
    local report_file="$output_dir/migration_report_run_${run_id}.md"
    
    # Generate the report using SQLite queries
    {
        echo "# COBOL Migration Report - Run $run_id"
        echo ""
        echo "**Generated:** $(date '+%Y-%m-%d %H:%M:%S')"
        echo ""
        echo "---"
        echo ""
        
        echo "## 📊 Migration Summary"
        echo ""
        
        $SQLITE3_CMD "$db_path" <<SQL
.mode markdown
.headers off
SELECT '- **Total COBOL Files:** ' || COUNT(DISTINCT file_name) FROM cobol_files WHERE run_id = $run_id;
SELECT '- **Programs (.cbl):** ' || COUNT(DISTINCT file_name) FROM cobol_files WHERE run_id = $run_id AND file_name LIKE '%.cbl';
SELECT '- **Copybooks (.cpy):** ' || COUNT(DISTINCT file_name) FROM cobol_files WHERE run_id = $run_id AND file_name LIKE '%.cpy';
SQL
        
        echo ""
        
        $SQLITE3_CMD "$db_path" <<SQL
.mode markdown
.headers off
SELECT '- **Total Dependencies:** ' || COUNT(*) FROM dependencies WHERE run_id = $run_id;
SELECT '  - CALL: ' || COUNT(*) FROM dependencies WHERE run_id = $run_id AND dependency_type = 'CALL';
SELECT '  - COPY: ' || COUNT(*) FROM dependencies WHERE run_id = $run_id AND dependency_type = 'COPY';
SELECT '  - PERFORM: ' || COUNT(*) FROM dependencies WHERE run_id = $run_id AND dependency_type = 'PERFORM';
SELECT '  - EXEC: ' || COUNT(*) FROM dependencies WHERE run_id = $run_id AND dependency_type = 'EXEC';
SELECT '  - READ: ' || COUNT(*) FROM dependencies WHERE run_id = $run_id AND dependency_type = 'READ';
SELECT '  - WRITE: ' || COUNT(*) FROM dependencies WHERE run_id = $run_id AND dependency_type = 'WRITE';
SELECT '  - OPEN: ' || COUNT(*) FROM dependencies WHERE run_id = $run_id AND dependency_type = 'OPEN';
SELECT '  - CLOSE: ' || COUNT(*) FROM dependencies WHERE run_id = $run_id AND dependency_type = 'CLOSE';
SQL
        
        echo ""
        echo "---"
        echo ""
        
        echo "## 📁 File Inventory"
        echo ""
        
        $SQLITE3_CMD "$db_path" <<SQL
.mode markdown
.headers on
SELECT file_name AS 'File Name', file_path AS 'Path', is_copybook AS 'Is Copybook'
FROM cobol_files 
WHERE run_id = $run_id
ORDER BY file_name;
SQL
        
        echo ""
        echo "---"
        echo ""
        
        echo "## 🔗 Dependency Relationships"
        echo ""
        
        $SQLITE3_CMD "$db_path" <<SQL
.mode markdown
.headers on
SELECT source_file AS 'Source', target_file AS 'Target', dependency_type AS 'Type', 
       COALESCE(line_number, '') AS 'Line', COALESCE(context, '') AS 'Context'
FROM dependencies 
WHERE run_id = $run_id
ORDER BY source_file, dependency_type, target_file;
SQL
        
        echo ""
        echo "---"
        echo ""
        echo "*Report generated by COBOL Migration Tool*"
        
    } > "$report_file"
    
    echo -e "${GREEN}✅ Report generated successfully!${NC}"
    echo -e "${CYAN}📄 Location: $report_file${NC}"
    
    # Ask if user wants to view the report
    echo ""
    read -p "View the report now? (Y/n): " -n 1 -r
    echo ""
    if [[ $REPLY =~ ^[Yy]$ ]] || [[ -z $REPLY ]]; then
        if command -v less >/dev/null 2>&1; then
            less "$report_file"
        else
            cat "$report_file"
        fi
    fi
}

# Function for interactive setup
run_setup() {
    echo -e "${CYAN}🚀 COBOL to Java Migration Tool - Setup${NC}"
    echo "========================================"
    echo ""

    # Check if local config already exists
    LOCAL_CONFIG="$REPO_ROOT/Config/ai-config.local.env"
    if [ -f "$LOCAL_CONFIG" ]; then
        echo -e "${YELLOW}⚠️  Local configuration already exists:${NC} $LOCAL_CONFIG"
        echo ""
        read -p "Do you want to overwrite it? (y/N): " -n 1 -r
        echo ""
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            echo -e "${BLUE}ℹ️  Setup cancelled. Your existing configuration is preserved.${NC}"
            return 0
        fi
    fi

    # Create local config from template
    echo -e "${BLUE}📁 Creating local configuration file...${NC}"
    TEMPLATE_CONFIG="$REPO_ROOT/Config/ai-config.local.env.example"

    if [ ! -f "$TEMPLATE_CONFIG" ]; then
        echo -e "${RED}❌ Example configuration file not found: $TEMPLATE_CONFIG${NC}"
        return 1
    fi

    cp "$TEMPLATE_CONFIG" "$LOCAL_CONFIG"
    echo -e "${GREEN}✅ Created: $LOCAL_CONFIG${NC}"
    echo ""

    # Interactive configuration
    echo -e "${BLUE}🔧 Interactive Configuration Setup${NC}"
    echo "=================================="
    echo ""
    echo "Please provide your AI service configuration details:"
    echo ""

    # Get AI Endpoint
    read -p "AI Endpoint (e.g., https://your-resource.openai.azure.com/): " endpoint
    if [[ -n "$endpoint" ]]; then
        # Ensure endpoint ends with /
        [[ "${endpoint}" != */ ]] && endpoint="${endpoint}/"
        sed -i.bak "s|_MAIN_ENDPOINT=\".*\"|_MAIN_ENDPOINT=\"$endpoint\"|" "$LOCAL_CONFIG"
    fi

    # Get API Key
    read -s -p "API Key (leave empty for Azure AD/Entra ID auth): " api_key
    echo ""
    if [[ -n "$api_key" ]]; then
        sed -i.bak "s|_MAIN_API_KEY=\".*\"|_MAIN_API_KEY=\"$api_key\"|" "$LOCAL_CONFIG"
    else
        echo -e "${BLUE}ℹ️  No API key set — will use Azure AD (Entra ID) via 'az login'.${NC}"
        echo -e "${BLUE}   Make sure you have the 'Cognitive Services OpenAI User' role.${NC}"
        echo -e "${BLUE}   See: azlogin-auth-guide.md for details.${NC}"
    fi

    # Get Code Model Deployment Name
    read -p "Code Model Deployment Name (default: gpt-5.1-codex-mini): " code_model
    code_model=${code_model:-gpt-5.1-codex-mini}
    sed -i.bak "s|_CODE_MODEL=\".*\"|_CODE_MODEL=\"$code_model\"|" "$LOCAL_CONFIG"

    # Clean up backup file
    rm -f "$LOCAL_CONFIG.bak"

    echo ""
    echo -e "${GREEN}✅ Configuration completed!${NC}"
    echo ""
    echo -e "${BLUE}🔍 Testing configuration...${NC}"
    
    # Test the configuration
    if load_configuration && load_ai_config 2>/dev/null; then
        echo -e "${GREEN}✅ Configuration loaded successfully!${NC}"
        echo ""
        echo -e "${BLUE}Next steps:${NC}"
        echo "1. Run: ./doctor.sh test    # Test system dependencies"
        echo "2. Run: ./doctor.sh run     # Start migration"
        echo ""
        echo "Your configuration is ready to use!"
    else
        echo -e "${RED}❌ Configuration test failed${NC}"
        echo "Please check your settings and try again."
    fi
}

# Function for comprehensive testing
run_test() {
    echo -e "${BOLD}${BLUE}COBOL to Java Quarkus Migration Tool - Test Suite${NC}"
    echo "=================================================="

    echo -e "${BLUE}Using dotnet CLI:${NC} $DOTNET_CMD"

    # Load configuration
    echo "🔧 Loading AI configuration..."
    if ! load_configuration; then
        echo -e "${RED}❌ Failed to load configuration system${NC}"
        return 1
    fi

    echo ""
    echo "Testing Configuration:"
    echo "====================="

    if load_ai_config; then
        echo ""
        echo -e "${GREEN}✅ Configuration loaded successfully!${NC}"
        echo ""
        echo "Configuration Summary:"
        show_config_summary 2>/dev/null || echo "Configuration details loaded"
    else
        echo ""
        echo -e "${RED}❌ Configuration loading failed!${NC}"
        echo ""
        echo "To fix this:"
        echo "1. Run: ./doctor.sh setup"
        echo "2. Edit Config/ai-config.local.env with your AI service credentials"
        echo "3. Run this test again"
        return 1
    fi

    # Check .NET version
    echo ""
    echo "Checking .NET version..."
    dotnet_version=$("$DOTNET_CMD" --version 2>/dev/null)
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}✅ .NET version: $dotnet_version${NC}"
        
        # Check if it's .NET 10.0 or higher
        major_version=$(echo $dotnet_version | cut -d. -f1)
        if [ "$major_version" -ge 10 ]; then
            echo -e "${GREEN}✅ .NET 10.0+ requirement satisfied${NC}"
        else
            echo -e "${YELLOW}⚠️  Warning: .NET 10.0+ recommended (current: $dotnet_version)${NC}"
        fi
    else
        echo -e "${RED}❌ .NET is not installed or not in PATH${NC}"
        return 1
    fi

    # Check Microsoft Agent Framework dependencies
    echo ""
    echo "Checking Microsoft Agent Framework dependencies..."
    if "$DOTNET_CMD" list package | grep -q "Microsoft.Agents.AI"; then
        af_version=$("$DOTNET_CMD" list package | grep "Microsoft.Agents.AI" | awk '{print $3}' | head -1)
        echo -e "${GREEN}✅ Microsoft Agent Framework dependencies resolved (version: $af_version)${NC}"
    else
        echo -e "${YELLOW}⚠️  Microsoft Agent Framework packages not found, checking project file...${NC}"
    fi

    # Build project
    echo ""
    echo "Building project and restoring packages..."
    echo "="
    if timeout 30s "$DOTNET_CMD" build "$REPO_ROOT/CobolToQuarkusMigration.csproj" --no-restore --verbosity quiet 2>/dev/null || "$DOTNET_CMD" build "$REPO_ROOT/CobolToQuarkusMigration.csproj" --verbosity minimal; then
        echo -e "${GREEN}✅ Project builds successfully${NC}"
    else
        echo -e "${RED}❌ Project build failed${NC}"
        echo "Try running: dotnet restore CobolToQuarkusMigration.csproj"
        return 1
    fi

    # Check source folders
    echo ""
    echo "Checking source folders..."
    cobol_files=$(find "$REPO_ROOT/source" -name "*.cbl" 2>/dev/null | wc -l)
    copybook_files=$(find "$REPO_ROOT/source" -name "*.cpy" 2>/dev/null | wc -l)
    total_files=$((cobol_files + copybook_files))
    
    if [ "$total_files" -gt 0 ]; then
        if [ "$cobol_files" -gt 0 ]; then
            echo -e "${GREEN}✅ Found $(printf "%8d" $cobol_files) COBOL files in source directory${NC}"
        fi
        if [ "$copybook_files" -gt 0 ]; then
            echo -e "${GREEN}✅ Found $(printf "%8d" $copybook_files) copybooks in source directory${NC}"
        fi
    else
        echo -e "${YELLOW}⚠️  No COBOL files or copybooks found in source directory${NC}"
        echo "   Add your COBOL files to ./source/ to test migration"
    fi

    # Check output directories
    echo ""
    echo "Checking output directories..."
    
    # Check Java output folder
    if [ -d "$REPO_ROOT/output/java" ]; then
        java_files=$(find "$REPO_ROOT/output/java" -name "*.java" 2>/dev/null | wc -l)
        if [ "$java_files" -gt 0 ]; then
            echo -e "${GREEN}✅ Found previous Java output ($java_files files) in output/java/${NC}"
        else
            echo -e "${BLUE}ℹ️  No previous Java output found in output/java/${NC}"
        fi
    else
        echo -e "${BLUE}ℹ️  Java output directory (output/java/) will be created during migration${NC}"
    fi
    
    # Check C# output folder
    if [ -d "$REPO_ROOT/output/csharp" ]; then
        csharp_files=$(find "$REPO_ROOT/output/csharp" -name "*.cs" 2>/dev/null | wc -l)
        if [ "$csharp_files" -gt 0 ]; then
            echo -e "${GREEN}✅ Found previous C# output ($csharp_files files) in output/csharp/${NC}"
        else
            echo -e "${BLUE}ℹ️  No previous C# output found in output/csharp/${NC}"
        fi
    else
        echo -e "${BLUE}ℹ️  C# output directory (output/csharp/) will be created during migration${NC}"
    fi
    
    # Check for reverse engineering output
    if [ -d "$REPO_ROOT/output" ]; then
        md_files=$(find "$REPO_ROOT/output" -name "*.md" 2>/dev/null | wc -l)
        if [ "$md_files" -gt 0 ]; then
            echo -e "${GREEN}✅ Found previous reverse engineering output ($md_files markdown files)${NC}"
        else
            echo -e "${BLUE}ℹ️  No previous reverse engineering output found${NC}"
        fi
    fi

    # Check logging infrastructure
    echo ""
    echo "Checking logging infrastructure..."
    if [ -d "$REPO_ROOT/Logs" ]; then
        log_files=$(find "$REPO_ROOT/Logs" -name "*.log" 2>/dev/null | wc -l)
        echo -e "${GREEN}✅ Log directory exists with $(printf "%8d" $log_files) log files${NC}"
    else
    mkdir -p "$REPO_ROOT/Logs"
        echo -e "${GREEN}✅ Created Logs directory${NC}"
    fi

    # Check for reverse engineering agents and models
    echo ""
    echo "Checking reverse engineering components..."
    re_components=0
    re_components_total=3
    
    [ -f "$REPO_ROOT/Models/BusinessLogic.cs" ] && ((re_components++))
    [ -f "$REPO_ROOT/Agents/BusinessLogicExtractorAgent.cs" ] && ((re_components++))
    [ -f "$REPO_ROOT/Processes/ReverseEngineeringProcess.cs" ] && ((re_components++))
    
    if [ $re_components -eq $re_components_total ]; then
        echo -e "${GREEN}✅ All reverse engineering components present ($re_components/$re_components_total)${NC}"
    elif [ $re_components -gt 0 ]; then
        echo -e "${YELLOW}⚠️  Partial reverse engineering support ($re_components/$re_components_total components)${NC}"
    else
        echo -e "${BLUE}ℹ️  Reverse engineering feature not installed${NC}"
    fi
    
    # Check for smart chunking infrastructure (v0.2)
    echo ""
    echo "Checking smart chunking infrastructure (v0.2)..."
    chunking_components=0
    chunking_total=3
    
    [ -f "$REPO_ROOT/Processes/SmartMigrationOrchestrator.cs" ] && ((chunking_components++))
    [ -f "$REPO_ROOT/Processes/ChunkedMigrationProcess.cs" ] && ((chunking_components++))
    [ -f "$REPO_ROOT/Processes/ChunkedReverseEngineeringProcess.cs" ] && ((chunking_components++))
    
    if [ $chunking_components -eq $chunking_total ]; then
        echo -e "${GREEN}✅ Smart chunking infrastructure complete ($chunking_components/$chunking_total)${NC}"
        echo -e "   ${CYAN}SmartMigrationOrchestrator${NC} - Routes files by size/complexity"
        echo -e "   ${CYAN}ChunkedMigrationProcess${NC} - Handles large file conversion"
        echo -e "   ${CYAN}ChunkedReverseEngineeringProcess${NC} - Handles large file RE analysis"
    elif [ $chunking_components -gt 0 ]; then
        echo -e "${YELLOW}⚠️  Partial smart chunking support ($chunking_components/$chunking_total components)${NC}"
    else
        echo -e "${YELLOW}⚠️  Smart chunking infrastructure not found${NC}"
    fi

    # Verify model deployments
    echo ""
    echo "Checking model deployments..."
    if load_configuration >/dev/null 2>&1 && load_ai_config >/dev/null 2>&1; then
        check_model_deployments
    else
        echo -e "${YELLOW}⚠️  Could not load config to verify deployments${NC}"
    fi

    echo ""
    echo -e "${GREEN}🚀 Ready to run migration!${NC}"
    echo ""
    echo "Migration Options:"
    echo "  Standard:         ./doctor.sh run"
    echo "  Reverse Engineer: dotnet run reverse-engineer --source ./source"
    echo "  Full Migration:   dotnet run -- --source ./source"
    echo ""
    if [ $re_components -eq $re_components_total ]; then
        echo "Reverse Engineering Available:"
        echo "  Extract business logic from COBOL before migration"
        echo "  Generate documentation in markdown format"
        echo "  Run: dotnet run reverse-engineer --source ./source"
        echo ""
    fi
    if [ "$total_files" -gt 0 ]; then
        echo "Expected Results:"
        echo "  - Process $cobol_files COBOL files and $copybook_files copybooks"
        echo "  - Generate Java files to output/java/ OR C# files to output/csharp/"
        echo "  - Create dependency maps"
        echo "  - Generate migration reports"
        echo ""
        echo "Target Language:"
        echo "  - Select during migration (Java or C#)"
        echo "  - Large files auto-split into multiple output files"
    fi
}

# Function to select migration speed profile
# Sets environment variables that override the three-tier reasoning system
# in appsettings.json via Program.cs OverrideSettingsFromEnvironment().
select_speed_profile() {
    echo ""
    echo "Speed Profile"
    echo "======================================"
    echo "  Controls how much reasoning effort the AI model spends per file."
    echo "  Higher effort means better output quality but slower processing."
    echo ""
    echo "  1) TURBO"
    echo "     Lowest reasoning on ALL files, no exceptions. Speed comes from low"
    echo "     reasoning effort + parallel file conversion (4 workers). 65K token"
    echo "     ceiling. Designed for testing and smoke runs."
    echo ""
    echo "  2) FAST"
    echo "     Low reasoning on most files, medium only on the most complex ones."
    echo "     32K token ceiling, parallel conversion (3 workers). Good for quick"
    echo "     iterations and proof-of-concept runs."
    echo ""
    echo "  3) BALANCED (default)"
    echo "     Uses the three-tier content-aware reasoning system. Simple files get"
    echo "     low effort, complex files get high effort. 100K token ceiling,"
    echo "     parallel conversion (2 workers). Recommended for production."
    echo ""
    echo "  4) THOROUGH"
    echo "     Maximum reasoning on all files regardless of complexity. 100K token"
    echo "     ceiling, parallel conversion (2 workers). Best for critical codebases"
    echo "     where accuracy matters more than speed."
    echo ""
    read -p "Enter choice (1-4) [default: 3]: " speed_choice
    speed_choice=$(echo "$speed_choice" | tr -d '[:space:]')

    case "$speed_choice" in
        1)
            echo -e "${GREEN}Selected: TURBO${NC}"
            export AI_LOW_REASONING_EFFORT="low"
            export AI_MEDIUM_REASONING_EFFORT="low"
            export AI_HIGH_REASONING_EFFORT="low"
            export AI_MAX_OUTPUT_TOKENS="65536"
            export AI_MIN_OUTPUT_TOKENS="8192"
            export AI_LOW_MULTIPLIER="1.0"
            export AI_MEDIUM_MULTIPLIER="1.0"
            export AI_HIGH_MULTIPLIER="1.5"
            export AI_STAGGER_DELAY_MS="200"
            export AI_MAX_PARALLEL_CONVERSION="4"
            export AI_RATE_LIMIT_SAFETY_FACTOR="0.85"
            ;;
        2)
            echo -e "${GREEN}Selected: FAST${NC}"
            export AI_LOW_REASONING_EFFORT="low"
            export AI_MEDIUM_REASONING_EFFORT="low"
            export AI_HIGH_REASONING_EFFORT="medium"
            export AI_MAX_OUTPUT_TOKENS="32768"
            export AI_MIN_OUTPUT_TOKENS="16384"
            export AI_LOW_MULTIPLIER="1.0"
            export AI_MEDIUM_MULTIPLIER="1.5"
            export AI_HIGH_MULTIPLIER="2.0"
            export AI_STAGGER_DELAY_MS="500"
            export AI_MAX_PARALLEL_CONVERSION="3"
            ;;
        4)
            echo -e "${GREEN}Selected: THOROUGH${NC}"
            export AI_LOW_REASONING_EFFORT="medium"
            export AI_MEDIUM_REASONING_EFFORT="high"
            export AI_HIGH_REASONING_EFFORT="high"
            export AI_MAX_OUTPUT_TOKENS="100000"
            export AI_MIN_OUTPUT_TOKENS="32768"
            export AI_LOW_MULTIPLIER="2.0"
            export AI_MEDIUM_MULTIPLIER="3.0"
            export AI_HIGH_MULTIPLIER="3.5"
            export AI_STAGGER_DELAY_MS="1500"
            export AI_MAX_PARALLEL_CONVERSION="2"
            ;;
        3|"")
            echo -e "${GREEN}Selected: BALANCED (default)${NC}"
            # Multipliers intentionally not overridden — uses appsettings.json defaults
            # (1.5/2.5/3.5 with 100K max) for the full three-tier content-aware system.
            export AI_STAGGER_DELAY_MS="1000"
            export AI_MAX_PARALLEL_CONVERSION="2"
            ;;
        *)
            echo -e "${YELLOW}Invalid choice, using BALANCED${NC}"
            ;;
    esac
    echo ""
}

# Function to run migration
run_migration() {
    echo -e "${BLUE}🚀 COBOL Migration Tool${NC}"
    echo "=============================================="

    echo -e "${BLUE}Using dotnet CLI:${NC} $DOTNET_CMD"

    # Load configuration
    echo "🔧 Loading AI configuration..."
    if ! load_configuration; then
        echo -e "${RED}❌ Configuration loading failed. Please run: ./doctor.sh setup${NC}"
        return 1
    fi

    # Load and validate configuration
    if ! load_ai_config; then
        echo -e "${RED}❌ Configuration loading failed. Please check your ai-config.local.env file.${NC}"
        return 1
    fi

    # Select AI provider and model
    if ! select_ai_provider; then
        return 1
    fi

    # Pre-check: verify AI connectivity
    if ! check_ai_connectivity; then
        echo -e "${RED}❌ Please fix connection issues first.${NC}"
        return 1
    fi

    # Check for existing reverse engineering results
    local re_output_file="$REPO_ROOT/output/reverse-engineering-details.md"
    local has_re_report="no"
    if [ -f "$re_output_file" ]; then
        has_re_report="yes"
    fi

    echo ""
    echo "📋 What would you like to do?"
    echo "========================================"
    echo "  1) Full Migration (Analysis + Code Conversion)"
    echo "  2) Reverse Engineering Report Only (no code conversion)"
    if [[ "$has_re_report" == "yes" ]]; then
        echo "  3) Code Conversion Only (use existing RE report)"
    fi
    echo ""
    
    local max_choice=3
    # [[ "$has_re_report" == "yes" ]] && max_choice=3
    
    read -p "Enter choice (1-$max_choice) [default: 1]: " action_choice
    
    # Default to full migration
    if [[ -z "$action_choice" ]]; then
        action_choice="1"
    fi

    # Handle Reverse Engineering Only
    if [[ "$action_choice" == "2" ]]; then
        echo ""
        echo -e "${GREEN}✅ Selected: Reverse Engineering Report Only${NC}"
        echo ""
        run_reverse_engineering --skip-setup
        return $?
    fi

    # Handle Conversion Only (if RE report exists)
    if [[ "$action_choice" == "3" ]] && [[ "$has_re_report" == "yes" ]]; then
        echo ""
        echo -e "${GREEN}✅ Selected: Code Conversion Only (using existing RE report)${NC}"
        # Set flag to skip RE and continue to language selection
        local skip_reverse_eng="--skip-reverse-engineering"
    else
        local skip_reverse_eng=""
    fi

    # For options 1 and 3, continue with language selection and migration
    echo ""
    echo "🎯 Select Target Language for Migration"
    echo "========================================"
    echo "  1) Java (Quarkus)"
    echo "  2) C# (.NET)"
    echo ""
    read -p "Enter choice (1 or 2) [default: 1]: " lang_choice
    
    # Trim whitespace from input
    lang_choice=$(echo "$lang_choice" | tr -d '[:space:]')
    
    # Validate the choice explicitly
    if [[ "$lang_choice" == "2" ]]; then
        export TARGET_LANGUAGE="CSharp"
        echo -e "${GREEN}✅ Selected: C# (.NET)${NC}"
    elif [[ "$lang_choice" == "1" ]] || [[ -z "$lang_choice" ]]; then
        export TARGET_LANGUAGE="Java"
        echo -e "${GREEN}✅ Selected: Java (Quarkus)${NC}"
    else
        echo -e "${YELLOW}⚠️  Invalid choice '$lang_choice', defaulting to Java${NC}"
        export TARGET_LANGUAGE="Java"
    fi

    # ========================
    # QUALITY GATE: Verify TARGET_LANGUAGE is correctly set before proceeding
    # ========================
    echo ""
    echo -e "${CYAN}🔒 Quality Gate: Verifying language selection...${NC}"
    if [[ "$TARGET_LANGUAGE" != "Java" ]] && [[ "$TARGET_LANGUAGE" != "CSharp" ]]; then
        echo -e "${RED}❌ QUALITY GATE FAILED: TARGET_LANGUAGE='$TARGET_LANGUAGE' is invalid${NC}"
        echo -e "${RED}   Must be 'Java' or 'CSharp'. Aborting migration.${NC}"
        return 1
    fi
    
    # Double-check: Ask for confirmation if C# was selected (to prevent accidental Java)
    if [[ "$TARGET_LANGUAGE" == "CSharp" ]]; then
        echo -e "${BOLD}${GREEN}▶▶▶ CONFIRMED: Target Language = C# (.NET) ◀◀◀${NC}"
    else
        echo -e "${BOLD}${GREEN}▶▶▶ CONFIRMED: Target Language = Java (Quarkus) ◀◀◀${NC}"
    fi
    echo -e "${GREEN}✅ Quality Gate PASSED: TARGET_LANGUAGE='$TARGET_LANGUAGE'${NC}"
    echo ""

    # Select speed profile
    select_speed_profile

    echo -e "${CYAN}🧩 Smart Chunking: AUTO-ENABLED${NC}"
    echo "================================"
    echo "Large files (>150K chars or >3000 lines) will automatically"
    echo "be split into semantic chunks for optimal processing."
    echo ""
    
    # Launch portal in background for monitoring
    local db_path="$REPO_ROOT/Data/migration.db"
    launch_portal_background "$db_path"
    
    echo "🚀 Starting COBOL to ${TARGET_LANGUAGE} Migration..."
    echo "=============================================="

    if [[ -z "$skip_reverse_eng" ]]; then
        echo ""
        echo -e "${BLUE}ℹ️  Full migration will include reverse engineering + ${TARGET_LANGUAGE} conversion${NC}"
        echo ""
    fi

    # Run the application - smart chunking is auto-detected
    # Export TARGET_LANGUAGE and output folder so it's available to the dotnet process
    export TARGET_LANGUAGE
    export MIGRATION_DB_PATH="$REPO_ROOT/Data/migration.db"
    if [[ "$TARGET_LANGUAGE" == "Java" ]]; then
        export JAVA_OUTPUT_FOLDER="output/java"
    else
        export CSHARP_OUTPUT_FOLDER="output/csharp"
    fi
    
    echo -e "${CYAN}🎯 Target: ${TARGET_LANGUAGE}${NC}"
    echo -e "${CYAN}💾 Database: $MIGRATION_DB_PATH${NC}"
    echo ""
    echo -e "${CYAN}💓 Progress heartbeat active (updates every 30s)${NC}"
    
    "$DOTNET_CMD" run -- --source ./source $skip_reverse_eng &
    local dotnet_pid=$!
    
    # Start heartbeat monitor
    start_heartbeat "$MIGRATION_DB_PATH" "$dotnet_pid"
    
    # Wait for dotnet to finish
    wait "$dotnet_pid"
    local migration_exit=$?
    
    # Stop heartbeat
    stop_heartbeat

    if [[ $migration_exit -ne 0 ]]; then
        echo ""
        echo -e "${RED}❌ Migration process failed (exit code $migration_exit).${NC}"
        echo -e "${BLUE}ℹ️  Portal is still running at http://localhost:$DEFAULT_MCP_PORT for debugging${NC}"
        return $migration_exit
    fi
    
    # Ask if user wants to generate a migration report
    echo ""
    echo -e "${BLUE}📄 Generate Migration Report?${NC}"
    echo "========================================"
    read -p "Generate a detailed migration report for this run? (Y/n): " -n 1 -r
    echo ""
    if [[ $REPLY =~ ^[Yy]$ ]] || [[ -z $REPLY ]]; then
        generate_migration_report
    fi

    echo ""
    echo -e "${GREEN}✅ Migration completed successfully!${NC}"
    echo -e "${BLUE}🌐 Portal is running at http://localhost:$DEFAULT_MCP_PORT${NC}"
    echo -e "${CYAN}📊 View results in 'Migration Monitor' or '📄 Reverse Engineering Results'${NC}"
    echo ""
    echo -e "${YELLOW}Press Ctrl+C to stop the portal when done viewing.${NC}"
    
    # Keep portal running in foreground now
    if [[ -n "$PORTAL_PID" ]] && kill -0 "$PORTAL_PID" 2>/dev/null; then
        # Bring portal to foreground by waiting for it
        wait "$PORTAL_PID"
    fi
}

# Function to resume migration
run_resume() {
    echo -e "${BLUE}🔄 Resuming COBOL Migration...${NC}"
    echo "======================================"

    echo -e "${BLUE}Using dotnet CLI:${NC} $DOTNET_CMD"

    # Load configuration
    if ! load_configuration || ! load_ai_config; then
        echo -e "${RED}❌ Configuration loading failed. Please check your setup.${NC}"
        return 1
    fi

    # Select AI provider and model
    if ! select_ai_provider; then
        return 1
    fi

    # Pre-check: verify AI connectivity
    if ! check_ai_connectivity; then
        echo -e "${RED}❌ Please fix connection issues first.${NC}"
        return 1
    fi

    echo ""
    echo "🎯 Select Target Language for Resume"
    echo "========================================"
    echo "  1) Java (Quarkus)"
    echo "  2) C# (.NET)"
    echo ""
    read -p "Enter choice (1 or 2) [default: 1]: " lang_choice
    lang_choice=$(echo "$lang_choice" | tr -d '[:space:]')

    if [[ "$lang_choice" == "2" ]]; then
        export TARGET_LANGUAGE="CSharp"
        echo -e "${GREEN}✅ Selected: C# (.NET)${NC}"
    else
        export TARGET_LANGUAGE="Java"
        echo -e "${GREEN}✅ Selected: Java (Quarkus)${NC}"
    fi

    # Select speed profile
    select_speed_profile

    echo ""
    echo "Checking for resumable migration state..."
    
    # Check for existing partial results
    if [ -d "$REPO_ROOT/output" ] && [ "$(ls -A $REPO_ROOT/output 2>/dev/null)" ]; then
        echo -e "${GREEN}✅ Found existing migration output${NC}"
        echo "Resuming from last position..."
    else
        echo -e "${YELLOW}⚠️  No previous migration state found${NC}"
        echo "Starting fresh migration..."
    fi

    # Run with resume logic
    export TARGET_LANGUAGE
    export MIGRATION_DB_PATH="$REPO_ROOT/Data/migration.db"
    if [[ "$TARGET_LANGUAGE" == "Java" ]]; then
        export JAVA_OUTPUT_FOLDER="output/java"
    else
        export CSHARP_OUTPUT_FOLDER="output/csharp"
    fi

    "$DOTNET_CMD" run -- --source ./source --resume
}

# Function to monitor migration
run_monitor() {
    echo -e "${BLUE}📊 Migration Progress Monitor${NC}"
    echo "============================"

    if [ ! -d "$REPO_ROOT/Logs" ]; then
        echo -e "${YELLOW}⚠️  No logs directory found${NC}"
        return 1
    fi

    echo "Monitoring migration logs..."
    echo "Press Ctrl+C to exit monitoring"
    echo ""

    # Monitor log files for progress
    tail -f "$REPO_ROOT/Logs"/*.log 2>/dev/null || echo "No active log files found"
}

# Function to validate system
run_validate() {
    echo -e "${BLUE}✅ System Validation${NC}"
    echo "==================="

    errors=0

    # Check .NET
    if command -v dotnet >/dev/null 2>&1; then
        echo -e "${GREEN}✅ .NET CLI available${NC}"
    else
        echo -e "${RED}❌ .NET CLI not found${NC}"
        ((errors++))
    fi

    # Check configuration files
    required_files=(
        "Config/ai-config.env"
        "Config/load-config.sh"
        "Config/appsettings.json"
        "CobolToQuarkusMigration.csproj"
        "Program.cs"
    )

    for file in "${required_files[@]}"; do
    if [ -f "$REPO_ROOT/$file" ]; then
            echo -e "${GREEN}✅ $file${NC}"
        else
            echo -e "${RED}❌ Missing: $file${NC}"
            ((errors++))
        fi
    done

    # Check directories
    for dir in "source" "output"; do
    if [ -d "$REPO_ROOT/$dir" ]; then
            echo -e "${GREEN}✅ Directory: $dir${NC}"
        else
            echo -e "${YELLOW}⚠️  Creating directory: $dir${NC}"
            mkdir -p "$REPO_ROOT/$dir"
        fi
    done

    # Validate reverse engineering components
    echo ""
    echo "Checking reverse engineering feature..."
    re_valid=0
    [ -f "$REPO_ROOT/Models/BusinessLogic.cs" ] && ((re_valid++))
    [ -f "$REPO_ROOT/Agents/BusinessLogicExtractorAgent.cs" ] && ((re_valid++))
    [ -f "$REPO_ROOT/Processes/ReverseEngineeringProcess.cs" ] && ((re_valid++))
    
    if [ $re_valid -eq 3 ]; then
        echo -e "${GREEN}✅ Reverse engineering feature: Complete (3/3 components)${NC}"
    elif [ $re_valid -gt 0 ]; then
        echo -e "${YELLOW}⚠️  Reverse engineering feature: Incomplete ($re_valid/3 components)${NC}"
        ((errors++))
    else
        echo -e "${BLUE}ℹ️  Reverse engineering feature: Not installed (optional)${NC}"
    fi
    
    # Validate smart chunking infrastructure (v0.2)
    echo ""
    echo "Checking smart chunking infrastructure (v0.2)..."
    chunk_valid=0
    [ -f "$REPO_ROOT/Processes/SmartMigrationOrchestrator.cs" ] && ((chunk_valid++))
    [ -f "$REPO_ROOT/Processes/ChunkedMigrationProcess.cs" ] && ((chunk_valid++))
    [ -f "$REPO_ROOT/Processes/ChunkedReverseEngineeringProcess.cs" ] && ((chunk_valid++))
    [ -f "$REPO_ROOT/Chunking/ChunkingOrchestrator.cs" ] && ((chunk_valid++))
    
    if [ $chunk_valid -eq 4 ]; then
        echo -e "${GREEN}✅ Smart chunking infrastructure: Complete (4/4 components)${NC}"
    elif [ $chunk_valid -gt 0 ]; then
        echo -e "${YELLOW}⚠️  Smart chunking infrastructure: Incomplete ($chunk_valid/4 components)${NC}"
    else
        echo -e "${YELLOW}⚠️  Smart chunking infrastructure: Not found${NC}"
    fi

    if [ $errors -eq 0 ]; then
        echo -e "${GREEN}🎉 System validation passed!${NC}"
        return 0
    else
        echo -e "${RED}❌ System validation failed with $errors errors${NC}"
        return 1
    fi
}

# Function for conversation log generation
run_conversation() {
    echo -e "${BLUE}💭 Conversation Log Generator${NC}"
    echo "=============================="

    echo -e "${BLUE}Using dotnet CLI:${NC} $DOTNET_CMD"
    
    # Load configuration
    if ! load_configuration || ! load_ai_config; then
        echo -e "${RED}❌ Configuration loading failed.${NC}"
        return 1
    fi

    echo "Generating readable conversation log from migration data..."
    echo ""

    export MIGRATION_DB_PATH="$REPO_ROOT/Data/migration.db"
    "$DOTNET_CMD" run -- conversation
}

# Function for reverse engineering
run_reverse_engineering() {
    echo -e "${BLUE}🔍 Running Reverse Engineering Analysis${NC}"
    echo "========================================"

    echo -e "${BLUE}Using dotnet CLI:${NC} $DOTNET_CMD"

    # Skip setup if already done (e.g. called from run_migration)
    if [[ "$1" != "--skip-setup" ]]; then
        # Load configuration
        echo "🔧 Loading AI configuration..."
        if ! load_configuration; then
            echo -e "${RED}❌ Configuration loading failed. Please run: ./doctor.sh setup${NC}"
            return 1
        fi

        # Load and validate configuration
        if ! load_ai_config; then
            echo -e "${RED}❌ Configuration loading failed. Please check your ai-config.local.env file.${NC}"
            return 1
        fi

        # Select AI provider and model
        if ! select_ai_provider; then
            return 1
        fi

        # Pre-check: verify AI connectivity
        if ! check_ai_connectivity; then
            echo -e "${RED}❌ Please fix connection issues first.${NC}"
            return 1
        fi
    fi

    # Check if reverse engineering components are present
    if [ ! -f "$REPO_ROOT/Processes/ReverseEngineeringProcess.cs" ]; then
        echo -e "${RED}❌ Reverse engineering feature not found.${NC}"
        echo "This feature may not be available in your version."
        return 1
    fi

    # Select speed profile
    select_speed_profile

    echo ""
    echo "🔍 Starting Reverse Engineering Analysis..."
    echo "=========================================="
    echo ""
    echo "This will:"
    echo "  • Extract business logic as feature descriptions and use cases"
    echo "  • Analyze modernization opportunities"
    echo "  • Generate markdown documentation"
    echo ""

    # Check for COBOL files
    cobol_count=$(find "$REPO_ROOT/source" -name "*.cbl" 2>/dev/null | wc -l)
    copybook_count=$(find "$REPO_ROOT/source" -name "*.cpy" 2>/dev/null | wc -l)
    total_count=$((cobol_count + copybook_count))
    
    if [ "$total_count" -eq 0 ]; then
        echo -e "${YELLOW}⚠️  No COBOL files or copybooks found in ./source/${NC}"
        echo "Add COBOL files to analyze and try again."
        return 1
    fi

    if [ "$cobol_count" -gt 0 ]; then
        echo -e "Found ${GREEN}$cobol_count${NC} COBOL file(s) to analyze"
    fi
    if [ "$copybook_count" -gt 0 ]; then
        echo -e "Found ${GREEN}$copybook_count${NC} copybook(s) to analyze"
    fi
    echo ""

    # Show file size info
    local total_lines=0
    local large_file_count=0
    if command -v wc >/dev/null 2>&1; then
        total_lines=$(find "$REPO_ROOT/source" -name "*.cbl" -exec wc -l {} + 2>/dev/null | tail -1 | awk '{print $1}')
        # Count files over threshold
        while IFS= read -r file; do
            local lines=$(wc -l < "$file" 2>/dev/null | tr -d ' ')
            if [[ "$lines" -gt 3000 ]]; then
                large_file_count=$((large_file_count + 1))
            fi
        done < <(find "$REPO_ROOT/source" -name "*.cbl" 2>/dev/null)
        
        if [[ "$large_file_count" -gt 0 ]]; then
            echo -e "${CYAN}🧩 Smart Chunking: AUTO-ENABLED${NC}"
            echo "   Large files detected: $large_file_count file(s) over threshold"
            echo "   Files >150K chars or >3000 lines will use ChunkedReverseEngineeringProcess"
            echo "   Semantic boundary detection preserves paragraph/section context"
            echo ""
        fi
    fi

    # Launch portal in background for monitoring
    launch_portal_background

    # Run the reverse engineering command - chunking is auto-detected
    export MIGRATION_DB_PATH="$REPO_ROOT/Data/migration.db"
    "$DOTNET_CMD" run reverse-engineer --source ./source

    local exit_code=$?

    if [ $exit_code -eq 0 ]; then
        echo ""
        echo -e "${GREEN}✅ Reverse engineering completed successfully!${NC}"
        echo ""
        echo "Output files created in: ./output/"
        echo "  • reverse-engineering-details.md - Complete analysis with business logic and technical details"
        echo "  • Results persisted to database — use './doctor.sh convert-only' and answer 'y' to"
        echo "    inject them into conversion prompts (or pass --skip-reverse-engineering --reuse-re)"
        echo ""
        echo -e "${CYAN}📄 View in Portal:${NC}"
        echo "  • Portal running at: http://localhost:5028"
        echo "  • Click '📄 Reverse Engineering Results' button to view the full RE report"
        echo "  • Each run card now has a '🔬 RE Results' button to view or delete persisted results"
        echo ""
        echo "Next steps:"
        echo "  • Review the generated documentation in portal or output folder"
        echo "  • Run full migration: ./doctor.sh run"
        echo "  • Or run conversion only with RE context: ./doctor.sh convert-only  (answer 'y' to reuse)"
        echo ""
        echo -e "${CYAN}Portal is running. Press Ctrl+C to stop.${NC}"
        
        # Keep portal running
        if [ -n "$PORTAL_PID" ]; then
            wait $PORTAL_PID 2>/dev/null
        fi
    else
        echo ""
        echo -e "${RED}❌ Reverse engineering failed (exit code $exit_code)${NC}"
        if [ -n "$PORTAL_PID" ]; then
            echo -e "${YELLOW}Portal is still running at http://localhost:5028 for debugging${NC}"
        fi
    fi

    return $exit_code
}

# Function to run conversion-only (skip reverse engineering)
run_conversion_only() {
    echo -e "${BLUE}🔄 Starting COBOL to Java Conversion (Skip Reverse Engineering)${NC}"
    echo "================================================================"

    echo -e "${BLUE}Using dotnet CLI:${NC} $DOTNET_CMD"

    # Load configuration
    echo "🔧 Loading AI configuration..."
    if ! load_configuration; then
        echo -e "${RED}❌ Configuration loading failed. Please run: ./doctor.sh setup${NC}"
        return 1
    fi

    # Load and validate configuration
    if ! load_ai_config; then
        echo -e "${RED}❌ Configuration loading failed. Please check your ai-config.local.env file.${NC}"
        return 1
    fi

    # Select AI provider and model
    if ! select_ai_provider; then
        return 1
    fi

    # Pre-check: verify AI connectivity
    if ! check_ai_connectivity; then
        echo -e "${RED}❌ Please fix connection issues first.${NC}"
        return 1
    fi

    # Select speed profile
    select_speed_profile

    echo ""
    echo "🔄 Starting Conversion Only..."
    echo "=============================="
    echo ""
    echo -e "${BLUE}ℹ️  Reverse engineering will be skipped${NC}"
    echo ""

    # ------------------------------------------------------------------
    # BUSINESS LOGIC REUSE PROMPT
    # ------------------------------------------------------------------
    echo -e "${BOLD}♻️  Reuse Business Logic from a Previous RE Run?${NC}"
    echo "============================================================"
    echo "  If you ran reverse engineering before, results are persisted in"
    echo "  the database and can be injected into conversion prompts for"
    echo "  higher-quality output."
    echo ""
    read -p "Reuse business logic from last RE run? (y/N): " -n 1 -r
    echo ""
    local reuse_re_flag=""
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        reuse_re_flag="--reuse-re"
        echo -e "${GREEN}✅ Will load business logic from previous RE run and inject into prompts${NC}"
    else
        echo -e "${BLUE}ℹ️  Pure conversion mode — no business logic context will be used${NC}"
    fi
    echo ""

    # ------------------------------------------------------------------
    # TARGET LANGUAGE SELECTION (if not already set)
    # ------------------------------------------------------------------
    if [[ -z "$TARGET_LANGUAGE" ]]; then
        echo "🎯 Select Target Language"
        echo "========================"
        echo "  1) Java (Quarkus)"
        echo "  2) C# (.NET)"
        echo ""
        read -p "Enter choice (1 or 2) [default: 1]: " lang_choice
        lang_choice=$(echo "$lang_choice" | tr -d '[:space:]')
        
        if [[ "$lang_choice" == "2" ]]; then
            export TARGET_LANGUAGE="CSharp"
            echo -e "${GREEN}✅ Selected: C# (.NET)${NC}"
        elif [[ "$lang_choice" == "1" ]] || [[ -z "$lang_choice" ]]; then
            export TARGET_LANGUAGE="Java"
            echo -e "${GREEN}✅ Selected: Java (Quarkus)${NC}"
        else
            echo -e "${YELLOW}⚠️  Invalid choice, defaulting to Java${NC}"
            export TARGET_LANGUAGE="Java"
        fi
        echo ""
    fi
     # Export output folder variables based on language selection
    if [[ "$TARGET_LANGUAGE" == "Java" ]]; then
        export JAVA_OUTPUT_FOLDER="output/java"
    else
        export CSHARP_OUTPUT_FOLDER="output/csharp"
    fi

    # Run the application with skip-reverse-engineering flag
    export MIGRATION_DB_PATH="$REPO_ROOT/Data/migration.db"
    
    # Check for resume flag
    resume_flag=""
    if [[ "$1" == "--resume" ]]; then
        resume_flag="--resume"
    fi
    
    "$DOTNET_CMD" run -- --source ./source --skip-reverse-engineering $reuse_re_flag $resume_flag
    local migration_exit=$?

    if [[ $migration_exit -ne 0 ]]; then
        echo ""
        echo -e "${RED}❌ Conversion process failed (exit code $migration_exit). Skipping MCP web UI launch.${NC}"
        return $migration_exit
    fi

    local db_path
    if ! db_path="$(get_migration_db_path)" || [[ -z "$db_path" ]]; then
        echo ""
        echo -e "${YELLOW}⚠️  Could not resolve migration database path. MCP web UI will not be started automatically.${NC}"
        return 0
    fi

    if [[ "${MCP_AUTO_LAUNCH:-1}" != "1" ]]; then
        echo ""
        echo -e "${BLUE}ℹ️  MCP web UI launch skipped (MCP_AUTO_LAUNCH set to ${MCP_AUTO_LAUNCH}).${NC}"
        echo -e "Use ${BOLD}MIGRATION_DB_PATH=$db_path ASPNETCORE_URLS=http://$DEFAULT_MCP_HOST:$DEFAULT_MCP_PORT $DOTNET_CMD run --project \"$REPO_ROOT/McpChatWeb\"${NC} to start manually."
        return 0
    fi

    launch_mcp_web_ui "$db_path"
}

# Main command routing
main() {
    # Create required directories if they don't exist
    mkdir -p "$REPO_ROOT/source" "$REPO_ROOT/output" "$REPO_ROOT/Logs"

    case "${1:-doctor}" in
        "setup")
            run_setup
            ;;
        "test")
            run_test
            ;;
        "run"|"run-chunked"|"chunked")
            # All run commands use the same function - chunking is auto-detected
            run_migration
            ;;
        "convert-only"|"conversion-only"|"convert")
            run_conversion_only
            ;;
        "portal"|"web"|"ui")
            run_portal
            ;;
        "doctor"|"")
            run_doctor
            ;;
        "reverse-eng"|"reverse-engineer"|"reverse")
            run_reverse_engineering
            ;;
        "resume")
            run_resume
            ;;
        "monitor")
            run_monitor
            ;;
        "conversation")
            run_conversation
            ;;
        "chunking-health"|"chunk-health"|"chunks")
            check_chunking_health
            ;;
        "validate")
            run_validate
            ;;
        "help"|"-h"|"--help")
            show_usage
            ;;
        *)
            echo -e "${RED}❌ Unknown command: $1${NC}"
            echo ""
            show_usage
            exit 1
            ;;
    esac
}

# Function to check chunking infrastructure health
check_chunking_health() {
    echo -e "${BLUE}🧩 Smart Chunking Health Check${NC}"
    echo "================================"
    echo ""
    
    local db_path
    db_path="$(get_migration_db_path)"
    
    # Check 1: Database existence
    echo -e "${CYAN}1. Database Status${NC}"
    if [[ -f "$db_path" ]]; then
        echo -e "   ${GREEN}✅ Database found:${NC} $db_path"
        local db_size=$(du -h "$db_path" 2>/dev/null | cut -f1)
        echo -e "   ${GREEN}✅ Size:${NC} $db_size"
    else
        echo -e "   ${YELLOW}⚠️  Database not found (will be created on first run)${NC}"
    fi
    echo ""
    
    # Check 2: Required process files
    echo -e "${CYAN}2. Smart Chunking Components${NC}"
    local components=(
        "Processes/SmartMigrationOrchestrator.cs:SmartMigrationOrchestrator (routes files by size)"
        "Processes/ChunkedMigrationProcess.cs:ChunkedMigrationProcess (conversion)"
        "Processes/ChunkedReverseEngineeringProcess.cs:ChunkedReverseEngineeringProcess (RE analysis)"
        "Chunking/ChunkingOrchestrator.cs:ChunkingOrchestrator (chunk coordination)"
    )
    for component in "${components[@]}"; do
        local file=$(echo "$component" | cut -d':' -f1)
        local desc=$(echo "$component" | cut -d':' -f2)
        if [[ -f "$REPO_ROOT/$file" ]]; then
            echo -e "   ${GREEN}✅ $desc${NC}"
        else
            echo -e "   ${RED}❌ $desc - MISSING${NC}"
        fi
    done
    echo ""
    
    # Check 2b: Required tables
    echo -e "${CYAN}3. Chunking Tables${NC}"
    if [[ -f "$db_path" ]]; then
        local tables=("chunk_metadata" "forward_references" "signatures" "type_mappings")
        for table in "${tables[@]}"; do
            resolve_sqlite3 2>/dev/null
            local exists=$($SQLITE3_CMD "$db_path" "SELECT name FROM sqlite_master WHERE type='table' AND name='$table';" 2>/dev/null)
            if [[ -n "$exists" ]]; then
                local count=$($SQLITE3_CMD "$db_path" "SELECT COUNT(*) FROM $table;" 2>/dev/null)
                echo -e "   ${GREEN}✅ $table${NC} ($count rows)"
            else
                echo -e "   ${YELLOW}⚠️  $table not found (created on first chunked run)${NC}"
            fi
        done
    else
        echo -e "   ${YELLOW}ℹ️  Tables will be created when database is initialized${NC}"
    fi
    echo ""
    
    # Check 4: Chunking configuration
    echo -e "${CYAN}4. Configuration (appsettings.json)${NC}"
    local config_file="$REPO_ROOT/Config/appsettings.json"
    if [[ -f "$config_file" ]] && command -v jq >/dev/null 2>&1; then
        local enabled=$(jq -r '.ChunkingSettings.EnableChunking // "auto"' "$config_file" 2>/dev/null)
        local max_lines=$(jq -r '.ChunkingSettings.MaxLinesPerChunk // 10000' "$config_file" 2>/dev/null)
        local max_tokens=$(jq -r '.ChunkingSettings.MaxTokensPerChunk // 28000' "$config_file" 2>/dev/null)
        local overlap=$(jq -r '.ChunkingSettings.OverlapLines // 500' "$config_file" 2>/dev/null)
        local parallel=$(jq -r '.ChunkingSettings.MaxParallelChunks // 3' "$config_file" 2>/dev/null)
        local resumable=$(jq -r '.ChunkingSettings.EnableResumability // true' "$config_file" 2>/dev/null)
        
        echo -e "   ${GREEN}✅ EnableChunking:${NC} $enabled (auto-detects large files)"
        echo -e "   ${GREEN}✅ MaxLinesPerChunk:${NC} $max_lines"
        echo -e "   ${GREEN}✅ MaxTokensPerChunk:${NC} $max_tokens"
        echo -e "   ${GREEN}✅ OverlapLines:${NC} $overlap"
        echo -e "   ${GREEN}✅ MaxParallelChunks:${NC} $parallel"
        echo -e "   ${GREEN}✅ EnableResumability:${NC} $resumable"
    else
        echo -e "   ${YELLOW}⚠️  Cannot parse config (jq not installed or config missing)${NC}"
        echo -e "   ${BLUE}ℹ️  Install jq: brew install jq${NC}"
    fi
    echo ""
    
    # Check 5: Recent chunk activity
    echo -e "${CYAN}5. Recent Chunk Activity${NC}"
    if [[ -f "$db_path" ]]; then
        resolve_sqlite3 2>/dev/null
        local recent=$($SQLITE3_CMD "$db_path" "
            SELECT run_id, source_file, 
                   COUNT(*) as chunks,
                   SUM(CASE WHEN status='Completed' THEN 1 ELSE 0 END) as completed,
                   SUM(CASE WHEN status='Failed' THEN 1 ELSE 0 END) as failed
            FROM chunk_metadata
            GROUP BY run_id, source_file
            ORDER BY run_id DESC
            LIMIT 5;
        " 2>/dev/null)
        
        if [[ -n "$recent" ]]; then
            echo -e "   Recent chunked files:"
            echo "$recent" | while IFS='|' read -r run_id file chunks completed failed; do
                local status_icon="✅"
                [[ "$failed" -gt 0 ]] && status_icon="⚠️"
                echo -e "   $status_icon Run $run_id: $file - $completed/$chunks chunks complete"
            done
        else
            echo -e "   ${BLUE}ℹ️  No chunked migrations yet${NC}"
        fi
    else
        echo -e "   ${BLUE}ℹ️  No migration history yet${NC}"
    fi
    echo ""
    
    # Check 6: Container Health
    echo -e "${CYAN}6. Container Health${NC}"
    
    # Check if container is running
    if command -v docker >/dev/null 2>&1; then
        if docker ps --format '{{.Names}}' | grep -q "cobol-migration-portal"; then
            echo -e "   ${GREEN}✅ Container 'cobol-migration-portal' is running${NC}"
        else
            echo -e "   ${YELLOW}⚠️  Container 'cobol-migration-portal' is NOT running${NC}"
            echo -e "      (Run 'docker-compose up -d' to start the containerized portal)"
        fi
    else
        echo -e "   ${YELLOW}⚠️  Docker not available - skipping container checks${NC}"
    fi
    echo ""
    
    # Check 7: Source file analysis
    echo -e "${CYAN}7. Source File Analysis${NC}"
    local cobol_files=$(find "$REPO_ROOT/source" -name "*.cbl" 2>/dev/null)
    if [[ -n "$cobol_files" ]]; then
        local large_file_count=0
        local total_lines=0
        
        while IFS= read -r file; do
            local lines=$(wc -l < "$file" 2>/dev/null | tr -d ' ')
            total_lines=$((total_lines + lines))
            if [[ "$lines" -gt 3000 ]]; then
                large_file_count=$((large_file_count + 1))
                echo -e "   ${YELLOW}📦 $(basename "$file")${NC} - $lines lines (will be chunked)"
            fi
        done <<< "$cobol_files"
        
        if [[ "$large_file_count" -eq 0 ]]; then
            echo -e "   ${GREEN}✅ No large files detected${NC} (all files < 3000 lines)"
        else
            echo -e "   ${YELLOW}📊 $large_file_count file(s) will use smart chunking${NC}"
        fi
        echo -e "   ${BLUE}ℹ️  Total lines across all files:${NC} $total_lines"
    else
        echo -e "   ${YELLOW}⚠️  No COBOL files found in source/${NC}"
    fi
    echo ""
    
    # Summary
    echo -e "${BLUE}═══════════════════════════════════════════════════════════════════════════${NC}"
    echo -e "${GREEN}💡 Smart Chunking Tips:${NC}"
    echo "   • Files >150K chars or >3000 lines auto-trigger chunking"
    echo "   • SmartMigrationOrchestrator routes files to appropriate process"
    echo "   • Full migration uses ChunkedMigrationProcess for conversion"
    echo "   • RE-only mode uses ChunkedReverseEngineeringProcess for analysis"
    echo "   • Monitor progress in portal: http://localhost:5028"
    echo "   • Adjust MaxLinesPerChunk in appsettings.json for tuning"
    echo ""
    echo -e "${GREEN}📊 Output Validation:${NC}"
    echo "   • Generated code is validated for completeness"
    echo "   • Large files are reassembled with proper ordering"
    echo "   • Cross-chunk references are resolved via SignatureRegistry"
    echo "   • Check output/<lang>/ folder for generated files"
    echo ""
}

# Run main function with all arguments
main "$@"
