#!/bin/bash

# ═══════════════════════════════════════════════════════════════════════════════
# COBOL Migration Progress Tracker
# ═══════════════════════════════════════════════════════════════════════════════
# A standalone CLI tool to track migration progress from SQLite database.
# Works even if VS Code restarts - just run this script in any terminal.
#
# Usage: ./helper-scripts/track-progress.sh [--watch] [--run-id N]
#
# ═══════════════════════════════════════════════════════════════════════════════

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
BOLD='\033[1m'
DIM='\033[2m'
NC='\033[0m'

# Get repository root
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DB_PATH="$REPO_ROOT/Data/migration.db"
LOG_DIR="$REPO_ROOT/Logs"

# Parse arguments
WATCH_MODE=false
SPECIFIC_RUN_ID=""
REFRESH_INTERVAL=3

while [[ $# -gt 0 ]]; do
    case $1 in
        --watch|-w)
            WATCH_MODE=true
            shift
            ;;
        --run-id|-r)
            SPECIFIC_RUN_ID="$2"
            shift 2
            ;;
        --interval|-i)
            REFRESH_INTERVAL="$2"
            shift 2
            ;;
        --help|-h)
            echo "Usage: $0 [options]"
            echo ""
            echo "Options:"
            echo "  --watch, -w          Continuously monitor progress (refresh every 3s)"
            echo "  --run-id N, -r N     Show progress for specific run ID"
            echo "  --interval N, -i N   Set refresh interval in seconds (default: 3)"
            echo "  --help, -h           Show this help message"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Check if sqlite3 is available
if ! command -v sqlite3 &> /dev/null; then
    echo -e "${RED}Error: sqlite3 is required but not installed.${NC}"
    exit 1
fi

# Check if database exists
check_db() {
    if [[ ! -f "$DB_PATH" ]]; then
        echo -e "${YELLOW}⚠️  Database not found: $DB_PATH${NC}"
        echo -e "${DIM}Run a migration first to create the database.${NC}"
        return 1
    fi
    return 0
}

# Draw a progress bar
draw_bar() {
    local current=$1
    local total=$2
    local width=${3:-30}
    local label=${4:-""}
    
    if [[ $total -eq 0 ]]; then
        printf "${DIM}[%-${width}s]${NC} 0%%" ""
        return
    fi
    
    local percent=$((current * 100 / total))
    local filled=$((current * width / total))
    local empty=$((width - filled))
    
    # Color based on progress
    local color=$YELLOW
    if [[ $percent -ge 100 ]]; then
        color=$GREEN
    elif [[ $percent -ge 50 ]]; then
        color=$CYAN
    fi
    
    printf "${color}["
    printf "%${filled}s" | tr ' ' '█'
    printf "${DIM}%${empty}s${NC}" | tr ' ' '░'
    printf "${color}]${NC} %3d%% " "$percent"
    
    if [[ -n "$label" ]]; then
        printf "(%s)" "$label"
    fi
}

# Get the latest or specific run ID
get_run_id() {
    if [[ -n "$SPECIFIC_RUN_ID" ]]; then
        echo "$SPECIFIC_RUN_ID"
    else
        sqlite3 "$DB_PATH" "SELECT MAX(id) FROM migration_runs;" 2>/dev/null || echo "0"
    fi
}

# Get run status
get_run_status() {
    local run_id=$1
    sqlite3 "$DB_PATH" "SELECT status, created_at, completed_at FROM migration_runs WHERE id=$run_id;" 2>/dev/null
}

# Get file statistics
get_file_stats() {
    local run_id=$1
    sqlite3 "$DB_PATH" "
        SELECT 
            COUNT(DISTINCT file_name) as total_files,
            SUM(CASE WHEN is_copybook = 0 THEN 1 ELSE 0 END) as programs,
            SUM(CASE WHEN is_copybook = 1 THEN 1 ELSE 0 END) as copybooks
        FROM cobol_files WHERE run_id=$run_id;
    " 2>/dev/null
}

# Get chunk statistics
get_chunk_stats() {
    local run_id=$1
    sqlite3 "$DB_PATH" "
        SELECT 
            COUNT(*) as total,
            SUM(CASE WHEN status='Completed' THEN 1 ELSE 0 END) as completed,
            SUM(CASE WHEN status='Failed' THEN 1 ELSE 0 END) as failed,
            SUM(CASE WHEN status='Processing' THEN 1 ELSE 0 END) as processing,
            SUM(CASE WHEN status='Pending' THEN 1 ELSE 0 END) as pending,
            SUM(COALESCE(tokens_used, 0)) as total_tokens,
            SUM(COALESCE(processing_time_ms, 0)) as total_time_ms
        FROM chunk_metadata WHERE run_id=$run_id;
    " 2>/dev/null
}

# Get per-file chunk details
get_file_chunk_details() {
    local run_id=$1
    sqlite3 -separator '|' "$DB_PATH" "
        SELECT 
            source_file,
            COUNT(*) as total_chunks,
            SUM(CASE WHEN status='Completed' THEN 1 ELSE 0 END) as completed,
            SUM(CASE WHEN status='Failed' THEN 1 ELSE 0 END) as failed,
            MIN(start_line) as start_line,
            MAX(end_line) as end_line
        FROM chunk_metadata 
        WHERE run_id=$run_id
        GROUP BY source_file
        ORDER BY source_file;
    " 2>/dev/null
}

# Get individual chunk status for a file
get_chunk_list() {
    local run_id=$1
    local file_name=$2
    sqlite3 -separator '|' "$DB_PATH" "
        SELECT 
            chunk_index,
            status,
            start_line,
            end_line,
            COALESCE(tokens_used, 0),
            COALESCE(processing_time_ms, 0) / 1000.0,
            COALESCE(error_message, '')
        FROM chunk_metadata 
        WHERE run_id=$run_id AND source_file='$file_name'
        ORDER BY chunk_index;
    " 2>/dev/null
}

# Get API call statistics
get_api_stats() {
    local today=$(date +%Y-%m-%d)
    local api_log="$LOG_DIR/ApiCalls/api_calls_$today.log"
    
    if [[ -f "$api_log" ]]; then
        local total=$(wc -l < "$api_log" | tr -d ' ')
        local errors=$(grep -c "ERROR\|FAILED\|429\|400" "$api_log" 2>/dev/null || echo "0")
        echo "$total|$errors"
    else
        echo "0|0"
    fi
}

# Get latest activity
get_latest_activity() {
    local today=$(date +%Y-%m-%d)
    local log_file=$(ls -t "$LOG_DIR"/*.log 2>/dev/null | head -1)
    
    if [[ -f "$log_file" ]]; then
        tail -1 "$log_file" 2>/dev/null | cut -c1-80
    else
        echo "No recent activity"
    fi
}

# Check if migration process is running
is_migration_running() {
    pgrep -f "dotnet.*run.*--source\|dotnet.*CobolToQuarkusMigration" > /dev/null 2>&1
}

# Display the progress dashboard
display_dashboard() {
    local run_id=$(get_run_id)
    
    if [[ "$run_id" == "0" || -z "$run_id" ]]; then
        echo -e "${YELLOW}No migration runs found.${NC}"
        return
    fi
    
    # Clear screen for watch mode
    if [[ "$WATCH_MODE" == true ]]; then
        clear
    fi
    
    # Header
    echo ""
    echo -e "${BOLD}${BLUE}╔══════════════════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${BOLD}${BLUE}║              🚀 COBOL Migration Progress Tracker                        ║${NC}"
    echo -e "${BOLD}${BLUE}╚══════════════════════════════════════════════════════════════════════════╝${NC}"
    echo ""
    
    # Run info
    IFS='|' read -r status created_at completed_at <<< "$(get_run_status $run_id)"
    
    echo -e "${CYAN}📋 Run #$run_id${NC}"
    
    # Status with icon
    case "$status" in
        "Running"|"InProgress")
            if is_migration_running; then
                echo -e "   Status: ${YELLOW}⏳ Running${NC} (process active)"
            else
                echo -e "   Status: ${RED}⚠️  Interrupted${NC} (process not found)"
            fi
            ;;
        "Completed")
            echo -e "   Status: ${GREEN}✅ Completed${NC}"
            ;;
        "Failed")
            echo -e "   Status: ${RED}❌ Failed${NC}"
            ;;
        *)
            echo -e "   Status: $status"
            ;;
    esac
    
    echo -e "   Started: ${DIM}$created_at${NC}"
    if [[ -n "$completed_at" && "$completed_at" != "null" ]]; then
        echo -e "   Finished: ${DIM}$completed_at${NC}"
    fi
    echo ""
    
    # File statistics
    IFS='|' read -r total_files programs copybooks <<< "$(get_file_stats $run_id)"
    echo -e "${CYAN}📁 Files${NC}"
    echo -e "   Total: ${BOLD}$total_files${NC} (${programs} programs, ${copybooks} copybooks)"
    echo ""
    
    # Chunk statistics
    IFS='|' read -r chunk_total chunk_completed chunk_failed chunk_processing chunk_pending total_tokens total_time_ms <<< "$(get_chunk_stats $run_id)"
    
    if [[ "$chunk_total" -gt 0 ]]; then
        echo -e "${CYAN}🧩 Chunk Progress${NC}"
        echo -n "   "
        draw_bar "$chunk_completed" "$chunk_total" 40 "$chunk_completed/$chunk_total chunks"
        echo ""
        
        # Detailed chunk status
        echo ""
        echo -e "   ${GREEN}✓ Completed:${NC} $chunk_completed"
        if [[ "$chunk_processing" -gt 0 ]]; then
            echo -e "   ${YELLOW}⏳ Processing:${NC} $chunk_processing"
        fi
        if [[ "$chunk_pending" -gt 0 ]]; then
            echo -e "   ${DIM}○ Pending:${NC} $chunk_pending"
        fi
        if [[ "$chunk_failed" -gt 0 ]]; then
            echo -e "   ${RED}✗ Failed:${NC} $chunk_failed"
        fi
        echo ""
        
        # Token usage
        if [[ "$total_tokens" -gt 0 ]]; then
            local formatted_tokens=$(printf "%'d" $total_tokens)
            local time_sec=$((total_time_ms / 1000))
            echo -e "   ${MAGENTA}⚡ Tokens used:${NC} $formatted_tokens"
            echo -e "   ${MAGENTA}⏱️  Processing time:${NC} ${time_sec}s"
            echo ""
        fi
        
        # Per-file breakdown
        echo -e "${CYAN}📊 Per-File Breakdown${NC}"
        echo -e "   ${DIM}┌────────────────────────────────────┬────────┬──────────┬────────┐${NC}"
        echo -e "   ${DIM}│ File                               │ Chunks │ Progress │ Status │${NC}"
        echo -e "   ${DIM}├────────────────────────────────────┼────────┼──────────┼────────┤${NC}"
        
        get_file_chunk_details $run_id | while IFS='|' read -r file_name total_chunks completed failed start_line end_line; do
            local short_name="${file_name:0:34}"
            local pct=0
            if [[ $total_chunks -gt 0 ]]; then
                pct=$((completed * 100 / total_chunks))
            fi
            
            local status_icon="⏳"
            local status_color=$YELLOW
            if [[ $completed -eq $total_chunks ]]; then
                status_icon="✅"
                status_color=$GREEN
            elif [[ $failed -gt 0 ]]; then
                status_icon="⚠️"
                status_color=$RED
            fi
            
            printf "   ${DIM}│${NC} %-34s ${DIM}│${NC} %6s ${DIM}│${NC} %6d%% ${DIM}│${NC} ${status_color}%s${NC}    ${DIM}│${NC}\n" \
                "$short_name" "$completed/$total_chunks" "$pct" "$status_icon"
        done
        
        echo -e "   ${DIM}└────────────────────────────────────┴────────┴──────────┴────────┘${NC}"
        echo ""
        
        # Show chunk details for large files
        local large_file=$(sqlite3 "$DB_PATH" "SELECT source_file FROM chunk_metadata WHERE run_id=$run_id GROUP BY source_file HAVING COUNT(*) > 5 LIMIT 1;" 2>/dev/null)
        
        if [[ -n "$large_file" ]]; then
            echo -e "${CYAN}🔍 Chunk Details: ${BOLD}$large_file${NC}"
            echo -e "   ${DIM}┌───────┬────────────┬─────────────────┬────────┬─────────┐${NC}"
            echo -e "   ${DIM}│ Chunk │   Status   │     Lines       │ Tokens │ Time(s) │${NC}"
            echo -e "   ${DIM}├───────┼────────────┼─────────────────┼────────┼─────────┤${NC}"
            
            get_chunk_list $run_id "$large_file" | head -20 | while IFS='|' read -r idx status start end tokens time_s error; do
                local status_display="$status"
                local status_color=$NC
                
                case "$status" in
                    "Completed") status_color=$GREEN; status_display="✓ Done" ;;
                    "Failed") status_color=$RED; status_display="✗ Fail" ;;
                    "Processing") status_color=$YELLOW; status_display="⏳ Work" ;;
                    "Pending") status_color=$DIM; status_display="○ Wait" ;;
                esac
                
                printf "   ${DIM}│${NC} %5d ${DIM}│${NC} ${status_color}%10s${NC} ${DIM}│${NC} %7d-%-7d ${DIM}│${NC} %6d ${DIM}│${NC} %7.1f ${DIM}│${NC}\n" \
                    "$idx" "$status_display" "$start" "$end" "$tokens" "$time_s"
            done
            
            echo -e "   ${DIM}└───────┴────────────┴─────────────────┴────────┴─────────┘${NC}"
            
            local chunk_count=$(sqlite3 "$DB_PATH" "SELECT COUNT(*) FROM chunk_metadata WHERE run_id=$run_id AND source_file='$large_file';" 2>/dev/null)
            if [[ $chunk_count -gt 20 ]]; then
                echo -e "   ${DIM}... and $((chunk_count - 20)) more chunks${NC}"
            fi
        fi
    else
        echo -e "${DIM}No chunks created yet. Chunking may not be enabled or files are small.${NC}"
    fi
    
    echo ""
    
    # API Statistics
    IFS='|' read -r api_total api_errors <<< "$(get_api_stats)"
    if [[ "$api_total" -gt 0 ]]; then
        echo -e "${CYAN}📡 API Calls Today${NC}"
        echo -e "   Total: $api_total | Errors: ${RED}$api_errors${NC}"
        echo ""
    fi
    
    # Latest activity
    echo -e "${CYAN}📝 Latest Activity${NC}"
    echo -e "   ${DIM}$(get_latest_activity)${NC}"
    echo ""
    
    # Footer
    if [[ "$WATCH_MODE" == true ]]; then
        echo -e "${DIM}Refreshing every ${REFRESH_INTERVAL}s... Press Ctrl+C to exit${NC}"
    else
        echo -e "${DIM}Run with --watch for continuous monitoring${NC}"
    fi
}

# Main
main() {
    if ! check_db; then
        exit 1
    fi
    
    if [[ "$WATCH_MODE" == true ]]; then
        while true; do
            display_dashboard
            sleep "$REFRESH_INTERVAL"
        done
    else
        display_dashboard
    fi
}

main
