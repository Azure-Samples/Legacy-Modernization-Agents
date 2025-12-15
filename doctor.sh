#!/bin/bash

# COBOL Migration Tool - All-in-One Management Script
# ===================================================
# This script consolidates all functionality for setup, testing, running, and diagnostics

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m' # No Color

# Get repository root (directory containing this script)
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Determine the preferred dotnet CLI (favor .NET 9 installations when available)
detect_dotnet_cli() {
    local default_cli="dotnet"
    local cli_candidate="$default_cli"

    # Check if default dotnet has .NET 9 runtime
    if command -v "$default_cli" >/dev/null 2>&1; then
        if "$default_cli" --list-runtimes 2>/dev/null | grep -q "Microsoft.NETCore.App 9."; then
            echo "$default_cli"
            return
        fi
    fi

    # Fallback: Check Homebrew .NET 9 location
    local homebrew_dotnet9="/opt/homebrew/opt/dotnet/libexec/dotnet"
    if [ -x "$homebrew_dotnet9" ]; then
        export DOTNET_ROOT="/opt/homebrew/opt/dotnet/libexec"
        export PATH="$DOTNET_ROOT:$PATH"
        echo "$homebrew_dotnet9"
        return
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
    echo -e "  ${GREEN}convert-only${NC}    Convert COBOL only (skip reverse eng + UI)"
    echo -e "  ${GREEN}portal${NC}          Start the web portal (documentation & monitoring)"
    echo -e "  ${GREEN}doctor${NC}          Diagnose configuration issues (default)"
    echo -e "  ${GREEN}reverse-eng${NC}     Run reverse engineering analysis only (no UI)"
    echo -e "  ${GREEN}resume${NC}          Resume interrupted migration"
    echo -e "  ${GREEN}monitor${NC}         Monitor migration progress"
    echo -e "  ${GREEN}chunking-health${NC} Check smart chunking infrastructure"
    echo -e "  ${GREEN}chat-test${NC}       Test chat logging functionality"
    echo -e "  ${GREEN}validate${NC}        Validate system requirements"
    echo -e "  ${GREEN}conversation${NC}    Start interactive conversation mode"
    echo
    echo -e "${BOLD}Examples:${NC}"
    echo -e "  $0                   ${CYAN}# Run configuration doctor${NC}"
    echo -e "  $0 setup             ${CYAN}# Interactive setup${NC}"
    echo -e "  $0 test              ${CYAN}# Test configuration and dependencies${NC}"
    echo -e "  $0 reverse-eng       ${CYAN}# Extract business logic only (no conversion, no UI)${NC}"
    echo -e "  $0 run               ${CYAN}# Full migration (auto-chunks large files)${NC}"
    echo -e "  $0 portal            ${CYAN}# Start portal to view docs & reports${NC}"
    echo -e "  $0 convert-only      ${CYAN}# Conversion only (skip reverse eng) + UI${NC}"
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
    
    # Check if port is already in use and clean up
    if lsof -Pi :$port -sTCP:LISTEN -t >/dev/null 2>&1; then
        echo -e "${YELLOW}⚠️  Port $port is already in use. Cleaning up...${NC}"
        local pid=$(lsof -ti:$port)
        if [[ -n "$pid" ]]; then
            kill -9 $pid 2>/dev/null && echo -e "${GREEN}✅ Killed existing process on port $port${NC}" || true
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
    
    # Check if port is already in use and clean up
    if lsof -Pi :$port -sTCP:LISTEN -t >/dev/null 2>&1; then
        echo -e "${YELLOW}⚠️  Port $port is already in use. Cleaning up...${NC}"
        local pid=$(lsof -ti:$port)
        if [[ -n "$pid" ]]; then
            kill -9 $pid 2>/dev/null && echo -e "${GREEN}✅ Killed existing process on port $port${NC}" || true
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
    if [[ -f "$REPO_ROOT/REVERSE_ENGINEERING_ARCHITECTURE.md" ]]; then
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
        echo "You need a local configuration file with your Azure OpenAI credentials."
        echo
        read -p "Would you like me to create Config/ai-config.local.env from the template? (y/n): " create_local
        
        if [[ "$create_local" =~ ^[Yy]$ ]]; then
            if [[ -f "$REPO_ROOT/Config/ai-config.local.env.template" ]]; then
                cp "$REPO_ROOT/Config/ai-config.local.env.template" "$REPO_ROOT/Config/ai-config.local.env"
                echo -e "${GREEN}✅ Created Config/ai-config.local.env from template${NC}"
                echo -e "${YELLOW}⚠️  You must edit this file with your actual Azure OpenAI credentials before running the migration tool.${NC}"
                local_config_exists=true
            else
                echo -e "${RED}❌ Template file not found: Config/ai-config.local.env.template${NC}"
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
            
            # Check required variables
            required_vars=(
                "AZURE_OPENAI_ENDPOINT"
                "AZURE_OPENAI_API_KEY"
                "AZURE_OPENAI_DEPLOYMENT_NAME"
                "AZURE_OPENAI_MODEL_ID"
            )
            
            config_valid=true
            
            for var in "${required_vars[@]}"; do
                value="${!var}"
                if [[ -z "$value" ]]; then
                    echo -e "${RED}❌ Missing: $var${NC}"
                    config_valid=false
                elif [[ "$value" == *"your-"* ]]; then
                    echo -e "${YELLOW}⚠️  Template placeholder detected in $var: $value${NC}"
                    config_valid=false
                else
                    # Mask API key for display
                    if [[ "$var" == "AZURE_OPENAI_API_KEY" ]]; then
                        masked_value="${value:0:8}...${value: -4}"
                        echo -e "${GREEN}✅ $var: $masked_value${NC}"
                    else
                        echo -e "${GREEN}✅ $var: $value${NC}"
                    fi
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
                echo "2. Replace template placeholders with your actual Azure OpenAI credentials"
                echo "3. Run this doctor script again to validate"
                echo
                echo "Need help? Run: ./doctor.sh setup"
            fi
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
    echo "• CONFIGURATION_GUIDE.md                  - Detailed setup instructions"
    echo "• REVERSE_ENGINEERING_ARCHITECTURE.md    - RE architecture & diagrams"
    echo "• output/reverse-engineering-details.md  - Generated business logic report"
    echo ""
    echo -e "${BLUE}🌐 Portal Documentation${NC}"
    echo "========================"
    echo "• Start portal: ./doctor.sh portal (or cd McpChatWeb && dotnet run)"
    echo "• Click '📄 Architecture Documentation' button in portal"
    echo "• View tabs: 🏗️ Architecture (diagrams) | 📊 RE Report (business logic)"

    echo
    echo -e "${BLUE}💡 Troubleshooting Tips${NC}"
    echo "======================"
    echo "• Make sure your Azure OpenAI resource is deployed and accessible"
    echo "• Verify your model deployment names match your Azure setup"
    echo "• Check that your API key has proper permissions"
    echo "• Ensure your endpoint URL is correct (should end with /)"

    echo
    echo "Configuration doctor completed!"
}

# Function to generate migration report
generate_migration_report() {
    echo -e "${BLUE}📝 Generating Migration Report...${NC}"
    
    local db_path="$REPO_ROOT/Data/migration.db"
    
    if [ ! -f "$db_path" ]; then
        echo -e "${RED}❌ Migration database not found at: $db_path${NC}"
        return 1
    fi
    
    # Get the latest run ID
    local run_id=$(sqlite3 "$db_path" "SELECT MAX(run_id) FROM cobol_files;")
    
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
        
        sqlite3 "$db_path" <<SQL
.mode markdown
.headers off
SELECT '- **Total COBOL Files:** ' || COUNT(DISTINCT file_name) FROM cobol_files WHERE run_id = $run_id;
SELECT '- **Programs (.cbl):** ' || COUNT(DISTINCT file_name) FROM cobol_files WHERE run_id = $run_id AND file_name LIKE '%.cbl';
SELECT '- **Copybooks (.cpy):** ' || COUNT(DISTINCT file_name) FROM cobol_files WHERE run_id = $run_id AND file_name LIKE '%.cpy';
SQL
        
        echo ""
        
        sqlite3 "$db_path" <<SQL
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
        
        sqlite3 "$db_path" <<SQL
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
        
        sqlite3 "$db_path" <<SQL
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
    TEMPLATE_CONFIG="$REPO_ROOT/Config/ai-config.local.env.template"

    if [ ! -f "$TEMPLATE_CONFIG" ]; then
        echo -e "${RED}❌ Template configuration file not found: $TEMPLATE_CONFIG${NC}"
        return 1
    fi

    cp "$TEMPLATE_CONFIG" "$LOCAL_CONFIG"
    echo -e "${GREEN}✅ Created: $LOCAL_CONFIG${NC}"
    echo ""

    # Interactive configuration
    echo -e "${BLUE}🔧 Interactive Configuration Setup${NC}"
    echo "=================================="
    echo ""
    echo "Please provide your Azure OpenAI configuration details:"
    echo ""

    # Get Azure OpenAI Endpoint
    read -p "Azure OpenAI Endpoint (e.g., https://your-resource.openai.azure.com/): " endpoint
    if [[ -n "$endpoint" ]]; then
        # Ensure endpoint ends with /
        [[ "${endpoint}" != */ ]] && endpoint="${endpoint}/"
        sed -i.bak "s|AZURE_OPENAI_ENDPOINT=\".*\"|AZURE_OPENAI_ENDPOINT=\"$endpoint\"|" "$LOCAL_CONFIG"
    fi

    # Get API Key
    read -s -p "Azure OpenAI API Key: " api_key
    echo ""
    if [[ -n "$api_key" ]]; then
        sed -i.bak "s|AZURE_OPENAI_API_KEY=\".*\"|AZURE_OPENAI_API_KEY=\"$api_key\"|" "$LOCAL_CONFIG"
    fi

    # Get Model Deployment Name
    read -p "Model Deployment Name (default: gpt-4.1): " deployment_name
    deployment_name=${deployment_name:-gpt-4.1}
    sed -i.bak "s|AZURE_OPENAI_DEPLOYMENT_NAME=\".*\"|AZURE_OPENAI_DEPLOYMENT_NAME=\"$deployment_name\"|" "$LOCAL_CONFIG"

    # Update model ID to match deployment name
    sed -i.bak "s|AZURE_OPENAI_MODEL_ID=\".*\"|AZURE_OPENAI_MODEL_ID=\"$deployment_name\"|" "$LOCAL_CONFIG"

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
        echo "2. Edit Config/ai-config.local.env with your Azure OpenAI credentials"
        echo "3. Run this test again"
        return 1
    fi

    # Check .NET version
    echo ""
    echo "Checking .NET version..."
    dotnet_version=$("$DOTNET_CMD" --version 2>/dev/null)
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}✅ .NET version: $dotnet_version${NC}"
        
        # Check if it's .NET 9.0 or higher
        major_version=$(echo $dotnet_version | cut -d. -f1)
        if [ "$major_version" -ge 9 ]; then
            echo -e "${GREEN}✅ .NET 9.0+ requirement satisfied${NC}"
        else
            echo -e "${YELLOW}⚠️  Warning: .NET 9.0+ recommended (current: $dotnet_version)${NC}"
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
    
    local max_choice=2
    [[ "$has_re_report" == "yes" ]] && max_choice=3
    
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
        run_reverse_engineering
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

    # For full migration (option 1), check if user wants to skip existing RE
    if [[ -z "$skip_reverse_eng" ]] && [[ "$has_re_report" == "yes" ]]; then
        echo ""
        echo -e "${GREEN}✅ Found existing reverse engineering results:${NC} $(basename "$re_output_file")"
        echo -e "${BLUE}ℹ️  You can skip reverse engineering to save time and API costs${NC}"
        echo ""
        read -p "Do you want to skip reverse engineering? (Y/n): " -n 1 -r
        echo ""
        if [[ $REPLY =~ ^[Yy]$ ]] || [[ -z $REPLY ]]; then
            skip_reverse_eng="--skip-reverse-engineering"
            echo -e "${BLUE}ℹ️  Skipping reverse engineering, using existing results${NC}"
        else
            echo -e "${BLUE}ℹ️  Will re-run reverse engineering as requested${NC}"
        fi
        echo ""
    elif [[ -z "$skip_reverse_eng" ]]; then
        echo ""
        echo -e "${BLUE}ℹ️  No previous reverse engineering results found${NC}"
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
    
    "$DOTNET_CMD" run -- --source ./source $skip_reverse_eng
    local migration_exit=$?

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
    echo -e "${CYAN}📊 View results in 'Migration Monitor' or '📄 Architecture Documentation' → '📊 RE Report'${NC}"
    echo ""
    echo -e "${YELLOW}Press Ctrl+C to stop the portal when done viewing.${NC}"
    
    # Keep portal running in foreground now
    if [[ -n "$PORTAL_PID" ]] && kill -0 "$PORTAL_PID" 2>/dev/null; then
        # Bring portal to foreground by waiting for it
        wait "$PORTAL_PID"
    fi
}

# Function to run migration with chunking auto-enabled (no prompts for chunking)
run_migration_chunked() {
    echo -e "${BLUE}🧩 Starting COBOL Migration with Smart Chunking...${NC}"
    echo "===================================================="

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

    echo ""
    echo "🎯 Select Target Language for Migration"
    echo "========================================"
    echo "  1) Java (Quarkus)"
    echo "  2) C# (.NET)"
    echo ""
    read -p "Enter choice (1 or 2) [default: 2 - C#]: " lang_choice
    
    # Trim whitespace from input
    lang_choice=$(echo "$lang_choice" | tr -d '[:space:]')
    
    # Validate the choice explicitly - default to C# for chunked migrations
    if [[ "$lang_choice" == "1" ]]; then
        export TARGET_LANGUAGE="Java"
        echo -e "${GREEN}✅ Selected: Java (Quarkus)${NC}"
    elif [[ "$lang_choice" == "2" ]] || [[ -z "$lang_choice" ]]; then
        export TARGET_LANGUAGE="CSharp"
        echo -e "${GREEN}✅ Selected: C# (.NET)${NC}"
    else
        echo -e "${YELLOW}⚠️  Invalid choice '$lang_choice', defaulting to C# (.NET)${NC}"
        export TARGET_LANGUAGE="CSharp"
    fi

    # ========================
    # QUALITY GATE: Verify TARGET_LANGUAGE is correctly set
    # ========================
    echo ""
    echo -e "${CYAN}🔒 Quality Gate: Verifying language selection...${NC}"
    if [[ "$TARGET_LANGUAGE" != "Java" ]] && [[ "$TARGET_LANGUAGE" != "CSharp" ]]; then
        echo -e "${RED}❌ QUALITY GATE FAILED: TARGET_LANGUAGE='$TARGET_LANGUAGE' is invalid${NC}"
        echo -e "${RED}   Must be 'Java' or 'CSharp'. Aborting migration.${NC}"
        return 1
    fi
    
    if [[ "$TARGET_LANGUAGE" == "CSharp" ]]; then
        echo -e "${BOLD}${GREEN}▶▶▶ CONFIRMED: Target Language = C# (.NET) ◀◀◀${NC}"
    else
        echo -e "${BOLD}${GREEN}▶▶▶ CONFIRMED: Target Language = Java (Quarkus) ◀◀◀${NC}"
    fi
    echo -e "${GREEN}✅ Quality Gate PASSED: TARGET_LANGUAGE='$TARGET_LANGUAGE'${NC}"

    # Auto-enable chunking
    chunking_flag="--chunked"
    echo ""
    echo -e "${GREEN}✅ Smart chunking auto-enabled for large file processing${NC}"
    echo -e "${BLUE}ℹ️  Files > 10,000 lines will be split into semantic chunks${NC}"
    echo -e "${BLUE}ℹ️  Parallel processing enabled with rate limiting${NC}"
    
    echo ""
    echo "🚀 Starting COBOL to ${TARGET_LANGUAGE} Migration with Smart Chunking..."
    echo "========================================================================"

    # Check if reverse engineering results already exist
    local re_output_file="$REPO_ROOT/output/reverse-engineering-details.md"
    local skip_reverse_eng=""
    
    if [ -f "$re_output_file" ]; then
        echo ""
        echo -e "${GREEN}✅ Found existing reverse engineering results${NC}"
        read -p "Skip reverse engineering? (Y/n): " -n 1 -r
        echo ""
        if [[ $REPLY =~ ^[Yy]$ ]] || [[ -z $REPLY ]]; then
            skip_reverse_eng="--skip-reverse-engineering"
            echo -e "${BLUE}ℹ️  Skipping reverse engineering${NC}"
        fi
    fi

    # Show file statistics
    echo ""
    cobol_count=$(find "$REPO_ROOT/source" -name "*.cbl" 2>/dev/null | wc -l | tr -d ' ')
    total_lines=$(find "$REPO_ROOT/source" -name "*.cbl" -exec wc -l {} + 2>/dev/null | tail -1 | awk '{print $1}')
    echo -e "📁 Found ${GREEN}$cobol_count${NC} COBOL files with ${GREEN}${total_lines:-0}${NC} total lines"
    
    if [[ -n "$total_lines" ]] && [[ "$total_lines" -gt 10000 ]]; then
        estimated_chunks=$((total_lines / 3000 + 1))
        echo -e "🧩 Estimated ${GREEN}$estimated_chunks${NC} chunks to process (3000 lines each)"
        
        # Read parallel settings from appsettings.json
        local parallel_workers=3
        if command -v jq >/dev/null 2>&1 && [[ -f "$REPO_ROOT/Config/appsettings.json" ]]; then
            parallel_workers=$(jq -r '.ChunkingSettings.MaxParallelChunks // 3' "$REPO_ROOT/Config/appsettings.json" 2>/dev/null)
        fi
        echo -e "⚡ Parallel workers: ${GREEN}$parallel_workers${NC}"
        
        # Estimate time (2 min per chunk with parallel processing)
        local estimated_time_min=$((estimated_chunks * 2 / parallel_workers))
        echo -e "⏱️  Estimated time: ${GREEN}~$estimated_time_min minutes${NC}"
    fi
    echo ""
    
    # Start background progress monitor
    echo -e "${CYAN}📊 Starting progress monitor...${NC}"
    start_progress_monitor &
    PROGRESS_PID=$!
    
    # Set trap to clean up progress monitor
    trap "kill $PROGRESS_PID 2>/dev/null; wait $PROGRESS_PID 2>/dev/null" EXIT

    # Run the application with chunking enabled
    export TARGET_LANGUAGE
    export MIGRATION_DB_PATH="$REPO_ROOT/Data/migration.db"
    "$DOTNET_CMD" run -- --source ./source $skip_reverse_eng $chunking_flag
    local migration_exit=$?

    # Stop progress monitor
    kill $PROGRESS_PID 2>/dev/null
    wait $PROGRESS_PID 2>/dev/null
    trap - EXIT

    if [[ $migration_exit -ne 0 ]]; then
        echo ""
        echo -e "${RED}❌ Migration process failed (exit code $migration_exit).${NC}"
        show_final_progress
        return $migration_exit
    fi
    
    # Show final progress
    show_final_progress
    
    # Generate migration report
    echo ""
    read -p "Generate a detailed migration report? (Y/n): " -n 1 -r
    echo ""
    if [[ $REPLY =~ ^[Yy]$ ]] || [[ -z $REPLY ]]; then
        generate_migration_report
    fi

    local db_path
    if ! db_path="$(get_migration_db_path)" || [[ -z "$db_path" ]]; then
        echo ""
        echo -e "${YELLOW}⚠️  Could not resolve migration database path.${NC}"
        return 0
    fi

    if [[ "${MCP_AUTO_LAUNCH:-1}" != "1" ]]; then
        echo ""
        echo -e "${BLUE}ℹ️  MCP web UI launch skipped.${NC}"
        return 0
    fi

    launch_mcp_web_ui "$db_path"
}

# Background progress monitor for chunked migration
start_progress_monitor() {
    local db_path="$REPO_ROOT/Data/migration.db"
    local last_completed=0
    local spinner_chars="⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"
    local spinner_idx=0
    
    sleep 5  # Wait for migration to start and DB to be populated
    
    while true; do
        if [[ ! -f "$db_path" ]]; then
            sleep 2
            continue
        fi
        
        # Get latest run info
        local run_info=$(sqlite3 "$db_path" "
            SELECT id, status FROM runs ORDER BY started_at DESC LIMIT 1;
        " 2>/dev/null)
        
        if [[ -z "$run_info" ]]; then
            sleep 2
            continue
        fi
        
        local run_id=$(echo "$run_info" | cut -d'|' -f1)
        local status=$(echo "$run_info" | cut -d'|' -f2)
        
        # Get chunk statistics
        local chunk_stats=$(sqlite3 "$db_path" "
            SELECT 
                COUNT(*) as total,
                SUM(CASE WHEN status='Completed' THEN 1 ELSE 0 END) as completed,
                SUM(CASE WHEN status='Failed' THEN 1 ELSE 0 END) as failed,
                SUM(CASE WHEN status='Processing' THEN 1 ELSE 0 END) as processing
            FROM chunk_metadata WHERE run_id=$run_id;
        " 2>/dev/null)
        
        if [[ -n "$chunk_stats" ]]; then
            local total=$(echo "$chunk_stats" | cut -d'|' -f1)
            local completed=$(echo "$chunk_stats" | cut -d'|' -f2)
            local failed=$(echo "$chunk_stats" | cut -d'|' -f3)
            local processing=$(echo "$chunk_stats" | cut -d'|' -f4)
            
            if [[ "$total" -gt 0 ]]; then
                local percent=$((completed * 100 / total))
                local spinner=${spinner_chars:spinner_idx:1}
                spinner_idx=$(( (spinner_idx + 1) % ${#spinner_chars} ))
                
                # Build progress bar
                local bar_width=30
                local filled=$((completed * bar_width / total))
                local empty=$((bar_width - filled))
                local bar=$(printf "%${filled}s" | tr ' ' '█')$(printf "%${empty}s" | tr ' ' '░')
                
                # Only show update if something changed
                if [[ "$completed" -ne "$last_completed" ]]; then
                    echo -ne "\r\033[K"  # Clear line
                    echo -e "${CYAN}$spinner${NC} [$bar] ${GREEN}$completed${NC}/${total} chunks (${percent}%) | ✓${completed} ⏳${processing} ✗${failed}"
                    last_completed=$completed
                fi
            fi
        fi
        
        # Check if migration is still running
        if [[ "$status" == "Completed" ]] || [[ "$status" == "Failed" ]]; then
            break
        fi
        
        sleep 3
    done
}

# Show final progress summary
show_final_progress() {
    local db_path="$REPO_ROOT/Data/migration.db"
    
    if [[ ! -f "$db_path" ]]; then
        return
    fi
    
    echo ""
    echo -e "${BLUE}╔══════════════════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${BLUE}║                        📊 Migration Summary                              ║${NC}"
    echo -e "${BLUE}╚══════════════════════════════════════════════════════════════════════════╝${NC}"
    echo ""
    
    # Get latest run info
    local run_info=$(sqlite3 "$db_path" "
        SELECT id, status, started_at, completed_at,
               julianday(COALESCE(completed_at, datetime('now'))) - julianday(started_at)
        FROM runs ORDER BY started_at DESC LIMIT 1;
    " 2>/dev/null)
    
    if [[ -n "$run_info" ]]; then
        local run_id=$(echo "$run_info" | cut -d'|' -f1)
        local status=$(echo "$run_info" | cut -d'|' -f2)
        local created=$(echo "$run_info" | cut -d'|' -f3)
        local completed=$(echo "$run_info" | cut -d'|' -f4)
        local duration_days=$(echo "$run_info" | cut -d'|' -f5)
        
        # Calculate duration in human-readable format
        local duration_seconds=$(echo "$duration_days * 86400" | bc 2>/dev/null || echo "0")
        local duration_minutes=$(printf "%.0f" $(echo "$duration_seconds / 60" | bc -l 2>/dev/null || echo "0"))
        local remaining_seconds=$(printf "%.0f" $(echo "$duration_seconds - ($duration_minutes * 60)" | bc -l 2>/dev/null || echo "0"))
        
        echo -e "   Run ID: ${BOLD}#$run_id${NC}"
        
        if [[ "$status" == "Completed" ]]; then
            echo -e "   Status: ${GREEN}✅ Completed${NC}"
        elif [[ "$status" == "Failed" ]]; then
            echo -e "   Status: ${RED}❌ Failed${NC}"
        else
            echo -e "   Status: ${YELLOW}⏳ $status${NC}"
        fi
        
        echo -e "   Duration: ${duration_minutes}m ${remaining_seconds}s"
        echo ""
        
        # Get chunk statistics
        local chunk_stats=$(sqlite3 "$db_path" "
            SELECT 
                COUNT(*) as total,
                SUM(CASE WHEN status='Completed' THEN 1 ELSE 0 END) as completed,
                SUM(CASE WHEN status='Failed' THEN 1 ELSE 0 END) as failed,
                SUM(COALESCE(tokens_used, 0)) as tokens,
                SUM(COALESCE(processing_time_ms, 0)) / 1000.0 as time_sec
            FROM chunk_metadata WHERE run_id=$run_id;
        " 2>/dev/null)
        
        if [[ -n "$chunk_stats" ]]; then
            local total=$(echo "$chunk_stats" | cut -d'|' -f1)
            local completed=$(echo "$chunk_stats" | cut -d'|' -f2)
            local failed=$(echo "$chunk_stats" | cut -d'|' -f3)
            local tokens=$(echo "$chunk_stats" | cut -d'|' -f4)
            local time_sec=$(echo "$chunk_stats" | cut -d'|' -f5)
            
            echo -e "   ${CYAN}🧩 Chunks${NC}"
            echo -e "      Total: $total"
            echo -e "      Completed: ${GREEN}$completed${NC}"
            if [[ "$failed" -gt 0 ]]; then
                echo -e "      Failed: ${RED}$failed${NC}"
            fi
            
            if [[ "$tokens" -gt 0 ]]; then
                local formatted_tokens=$(printf "%'d" $tokens 2>/dev/null || echo "$tokens")
                echo ""
                echo -e "   ${MAGENTA}⚡ Tokens Used: $formatted_tokens${NC}"
            fi
        fi
    fi
    
    echo ""
    echo -e "${BLUE}═══════════════════════════════════════════════════════════════════════════${NC}"
    echo -e "${DIM}For detailed progress tracking, run: ./helper-scripts/track-progress.sh${NC}"
    echo -e "${DIM}Or open the web portal: http://localhost:5028${NC}"
}

# Function to resume migration
run_resume() {
    echo -e "${BLUE}🔄 Resuming COBOL to Java Migration...${NC}"
    echo "======================================"

    echo -e "${BLUE}Using dotnet CLI:${NC} $DOTNET_CMD"

    # Load configuration
    if ! load_configuration || ! load_ai_config; then
        echo -e "${RED}❌ Configuration loading failed. Please check your setup.${NC}"
        return 1
    fi

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
    export MIGRATION_DB_PATH="$REPO_ROOT/Data/migration.db"
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

# Function to test chat logging
run_chat_test() {
    echo -e "${BLUE}💬 Testing Chat Logging Functionality${NC}"
    echo "====================================="

    echo -e "${BLUE}Using dotnet CLI:${NC} $DOTNET_CMD"

    # Load configuration
    if ! load_configuration || ! load_ai_config; then
        echo -e "${RED}❌ Configuration loading failed.${NC}"
        return 1
    fi

    echo "Testing chat logging system..."
    
    # Run a simple test
    export MIGRATION_DB_PATH="$REPO_ROOT/Data/migration.db"
    "$DOTNET_CMD" run -- --test-chat-logging
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

# Function for conversation mode
run_conversation() {
    echo -e "${BLUE}💭 Interactive Conversation Mode${NC}"
    echo "================================"

    echo -e "${BLUE}Using dotnet CLI:${NC} $DOTNET_CMD"
    
    # Load configuration
    if ! load_configuration || ! load_ai_config; then
        echo -e "${RED}❌ Configuration loading failed.${NC}"
        return 1
    fi

    echo "Starting interactive conversation with the migration system..."
    echo "Type 'exit' to quit"
    echo ""

    export MIGRATION_DB_PATH="$REPO_ROOT/Data/migration.db"
    "$DOTNET_CMD" run -- --interactive
}

# Function for reverse engineering
run_reverse_engineering() {
    echo -e "${BLUE}🔍 Running Reverse Engineering Analysis${NC}"
    echo "========================================"

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

    # Check if reverse engineering components are present
    if [ ! -f "$REPO_ROOT/Processes/ReverseEngineeringProcess.cs" ]; then
        echo -e "${RED}❌ Reverse engineering feature not found.${NC}"
        echo "This feature may not be available in your version."
        return 1
    fi

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
        echo ""
        echo -e "${CYAN}📄 View in Portal:${NC}"
        echo "  • Portal running at: http://localhost:5028"
        echo "  • Click '📄 Architecture Documentation' → '📊 RE Report' tab"
        echo ""
        echo "Next steps:"
        echo "  • Review the generated documentation in portal or output folder"
        echo "  • Run full migration: ./doctor.sh run"
        echo "  • Or run conversion only: ./doctor.sh convert-only"
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

    echo ""
    echo "🔄 Starting Java Conversion Only..."
    echo "==================================="
    echo ""
    echo -e "${BLUE}ℹ️  Reverse engineering will be skipped${NC}"
    echo -e "${BLUE}ℹ️  Only COBOL to Java Quarkus conversion will be performed${NC}"
    echo ""

    # Run the application with skip-reverse-engineering flag
    export MIGRATION_DB_PATH="$REPO_ROOT/Data/migration.db"
    "$DOTNET_CMD" run -- --source ./source --skip-reverse-engineering
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
        "chat-test")
            run_chat_test
            ;;
        "validate")
            run_validate
            ;;
        "conversation")
            run_conversation
            ;;
        "chunking-health"|"chunk-health"|"chunks")
            check_chunking_health
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
            local exists=$(sqlite3 "$db_path" "SELECT name FROM sqlite_master WHERE type='table' AND name='$table';" 2>/dev/null)
            if [[ -n "$exists" ]]; then
                local count=$(sqlite3 "$db_path" "SELECT COUNT(*) FROM $table;" 2>/dev/null)
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
        local recent=$(sqlite3 "$db_path" "
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
    
    # Check 6: Source file analysis
    echo -e "${CYAN}6. Source File Analysis${NC}"
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
