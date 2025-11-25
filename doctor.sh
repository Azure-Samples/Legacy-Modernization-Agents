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
    echo -e "${BOLD}${BLUE}🧠 COBOL to Java Quarkus Migration Tool${NC}"
    echo -e "${BLUE}==========================================${NC}"
    echo
    echo -e "${BOLD}Usage:${NC} $0 [command]"
    echo
    echo -e "${BOLD}Available Commands:${NC}"
    echo -e "  ${GREEN}setup${NC}           Interactive configuration setup"
    echo -e "  ${GREEN}test${NC}            Full system validation and testing"
    echo -e "  ${GREEN}run${NC}             Start full migration (reverse eng + Java conversion + UI)"
    echo -e "  ${GREEN}convert-only${NC}    Convert COBOL to Java only (skip reverse eng + UI)"
    echo -e "  ${GREEN}doctor${NC}          Diagnose configuration issues (default)"
    echo -e "  ${GREEN}reverse-eng${NC}     Run reverse engineering analysis only (no UI)"
    echo -e "  ${GREEN}resume${NC}          Resume interrupted migration"
    echo -e "  ${GREEN}monitor${NC}         Monitor migration progress"
    echo -e "  ${GREEN}chat-test${NC}       Test chat logging functionality"
    echo -e "  ${GREEN}validate${NC}        Validate system requirements"
    echo -e "  ${GREEN}conversation${NC}    Start interactive conversation mode"
    echo
    echo -e "${BOLD}Examples:${NC}"
    echo -e "  $0                   ${CYAN}# Run configuration doctor${NC}"
    echo -e "  $0 setup             ${CYAN}# Interactive setup${NC}"
    echo -e "  $0 test              ${CYAN}# Test configuration and dependencies${NC}"
    echo -e "  $0 reverse-eng       ${CYAN}# Extract business logic only (no conversion, no UI)${NC}"
    echo -e "  $0 run               ${CYAN}# Full migration: reverse eng + Java conversion + UI${NC}"
    echo -e "  $0 convert-only      ${CYAN}# Java conversion only (skip reverse eng) + UI${NC}"
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
SELECT id, status, coalesce(completed_at, updated_at, created_at)
FROM migration_runs
ORDER BY created_at DESC
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
    if [[ -f "$REPO_ROOT/REVERSE_ENGINEERING.md" ]]; then
        echo -e "${GREEN}✅ Reverse engineering documentation found${NC}"
    else
        echo -e "${YELLOW}⚠️  Missing reverse engineering documentation${NC}"
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
    echo "• CONFIGURATION_GUIDE.md    - Detailed setup instructions"
    echo "• REVERSE_ENGINEERING.md    - Reverse engineering guide"

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

    # Check Semantic Kernel dependencies
    echo ""
    echo "Checking Semantic Kernel dependencies..."
    if "$DOTNET_CMD" list package | grep -q "Microsoft.SemanticKernel"; then
        sk_version=$("$DOTNET_CMD" list package | grep "Microsoft.SemanticKernel" | awk '{print $3}' | head -1)
        echo -e "${GREEN}✅ Semantic Kernel dependencies resolved (version: $sk_version)${NC}"
    else
        echo -e "${YELLOW}⚠️  Semantic Kernel packages not found, checking project file...${NC}"
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
    if [ -d "$REPO_ROOT/output" ]; then
        java_files=$(find "$REPO_ROOT/output" -name "*.java" 2>/dev/null | wc -l)
        if [ "$java_files" -gt 0 ]; then
            echo -e "${GREEN}✅ Found previous Java output ($java_files files)${NC}"
        else
            echo -e "${BLUE}ℹ️  No previous Java output found (will be created during migration)${NC}"
        fi

        md_files=$(find "$REPO_ROOT/output" -name "*.md" 2>/dev/null | wc -l)
        if [ "$md_files" -gt 0 ]; then
            echo -e "${GREEN}✅ Found previous reverse engineering output ($md_files markdown files)${NC}"
        else
            echo -e "${BLUE}ℹ️  No previous reverse engineering output found${NC}"
        fi
    else
        echo -e "${BLUE}ℹ️  Output directory will be created during migration${NC}"
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
        echo "  - Generate $cobol_files+ Java files"
        echo "  - Create dependency maps"
        echo "  - Generate migration reports"
    fi
}

# Function to run migration
run_migration() {
    echo -e "${BLUE}🚀 Starting COBOL Migration...${NC}"
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

    echo ""
    echo "🎯 Select Target Language for Migration"
    echo "========================================"
    echo "  1) Java (Quarkus)"
    echo "  2) C# (.NET)"
    echo ""
    read -p "Enter choice (1 or 2) [default: 1]: " lang_choice
    
    # Default to Java if empty or invalid
    if [[ -z "$lang_choice" ]] || [[ "$lang_choice" != "2" ]]; then
        lang_choice="1"
    fi
    
    case $lang_choice in
        1)
            export TARGET_LANGUAGE="Java"
            echo -e "${GREEN}✅ Selected: Java (Quarkus)${NC}"
            ;;
        2)
            export TARGET_LANGUAGE="CSharp"
            echo -e "${GREEN}✅ Selected: C# (.NET)${NC}"
            ;;
        *)
            export TARGET_LANGUAGE="Java"
            echo -e "${YELLOW}⚠️  Invalid choice, defaulting to Java${NC}"
            ;;
    esac
    
    echo ""
    echo "🚀 Starting COBOL to ${TARGET_LANGUAGE} Migration..."
    echo "=============================================="

    # Check if reverse engineering results already exist
    local re_output_file="$REPO_ROOT/output/reverse-engineering-details.md"
    local skip_reverse_eng=""
    
    if [ -f "$re_output_file" ]; then
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
    else
        echo ""
        echo -e "${BLUE}ℹ️  No previous reverse engineering results found${NC}"
        echo -e "${BLUE}ℹ️  Full migration will include reverse engineering + ${TARGET_LANGUAGE} conversion${NC}"
        echo ""
    fi

    # Run the application with updated folder structure
    # Export TARGET_LANGUAGE so it's available to the dotnet process
    export TARGET_LANGUAGE
    "$DOTNET_CMD" run -- --source ./source $skip_reverse_eng
    local migration_exit=$?

    if [[ $migration_exit -ne 0 ]]; then
        echo ""
        echo -e "${RED}❌ Migration process failed (exit code $migration_exit). Skipping MCP web UI launch.${NC}"
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

    # Run the reverse engineering command
    "$DOTNET_CMD" run reverse-engineer --source ./source

    local exit_code=$?

    if [ $exit_code -eq 0 ]; then
        echo ""
        echo -e "${GREEN}✅ Reverse engineering completed successfully!${NC}"
        echo ""
        echo "Output files created in: ./output/"
        echo "  • reverse-engineering-details.md - Complete analysis with business logic and technical details"
        echo ""
        echo "Next steps:"
        echo "  • Review the generated documentation"
        echo "  • Run full migration: ./doctor.sh run"
        echo "  • Or run conversion only: ./doctor.sh convert-only"
    else
        echo ""
        echo -e "${RED}❌ Reverse engineering failed (exit code $exit_code)${NC}"
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
        "run")
            run_migration
            ;;
        "convert-only"|"conversion-only"|"convert")
            run_conversion_only
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

# Run main function with all arguments
main "$@"
