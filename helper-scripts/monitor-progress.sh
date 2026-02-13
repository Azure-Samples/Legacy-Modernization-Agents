#!/bin/bash

# Real-time Migration Progress Monitor
# ====================================
# Shows live progress for large file processing including chunking status

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DB_PATH="$REPO_ROOT/Data/migration.db"
LOG_DIR="$REPO_ROOT/Logs"

clear
echo -e "${BOLD}${BLUE}╔══════════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BOLD}${BLUE}║           COBOL Migration Real-Time Progress Monitor             ║${NC}"
echo -e "${BOLD}${BLUE}╚══════════════════════════════════════════════════════════════════╝${NC}"
echo ""

# Function to get file counts from source
get_source_stats() {
    local source_dir="$1"
    if [[ -d "$source_dir" ]]; then
        local cbl_count=$(find "$source_dir" -name "*.cbl" 2>/dev/null | wc -l | tr -d ' ')
        local cpy_count=$(find "$source_dir" -name "*.cpy" 2>/dev/null | wc -l | tr -d ' ')
        local total_lines=$(find "$source_dir" -name "*.cbl" -exec wc -l {} + 2>/dev/null | tail -1 | awk '{print $1}')
        echo "$cbl_count|$cpy_count|${total_lines:-0}"
    else
        echo "0|0|0"
    fi
}

# Function to get database stats
get_db_stats() {
    if [[ -f "$DB_PATH" ]]; then
        local run_id=$(sqlite3 "$DB_PATH" "SELECT MAX(id) FROM migration_runs;" 2>/dev/null || echo "0")
        local status=$(sqlite3 "$DB_PATH" "SELECT status FROM migration_runs WHERE id=$run_id;" 2>/dev/null || echo "Unknown")
        local files_processed=$(sqlite3 "$DB_PATH" "SELECT COUNT(DISTINCT file_name) FROM cobol_files WHERE run_id=$run_id;" 2>/dev/null || echo "0")
        local chunks_total=$(sqlite3 "$DB_PATH" "SELECT COUNT(*) FROM chunk_metadata WHERE run_id=$run_id;" 2>/dev/null || echo "0")
        local chunks_completed=$(sqlite3 "$DB_PATH" "SELECT COUNT(*) FROM chunk_metadata WHERE run_id=$run_id AND status='Completed';" 2>/dev/null || echo "0")
        local chunks_failed=$(sqlite3 "$DB_PATH" "SELECT COUNT(*) FROM chunk_metadata WHERE run_id=$run_id AND status='Failed';" 2>/dev/null || echo "0")
        echo "$run_id|$status|$files_processed|$chunks_total|$chunks_completed|$chunks_failed"
    else
        echo "0|No DB|0|0|0|0"
    fi
}

# Function to get latest API call info
get_latest_api_call() {
    local today=$(date +%Y-%m-%d)
    local api_log="$LOG_DIR/BEHIND_SCENES_API_CALL_$today.log"
    if [[ -f "$api_log" ]]; then
        tail -1 "$api_log" 2>/dev/null | grep -o '"message":"[^"]*"' | head -1 | sed 's/"message":"//;s/"//'
    else
        echo "No API calls logged today"
    fi
}

# Function to get latest processing info
get_latest_processing() {
    local today=$(date +%Y-%m-%d)
    local proc_log="$LOG_DIR/BEHIND_SCENES_PROCESSING_$today.log"
    if [[ -f "$proc_log" ]]; then
        tail -1 "$proc_log" 2>/dev/null | grep -o '"message":"[^"]*"' | head -1 | sed 's/"message":"//;s/"//'
    else
        echo "No processing logged today"
    fi
}

# Function to show chunk progress for a file
show_chunk_progress() {
    local file_name="$1"
    if [[ -f "$DB_PATH" ]]; then
        local run_id=$(sqlite3 "$DB_PATH" "SELECT MAX(id) FROM migration_runs;" 2>/dev/null || echo "0")
        sqlite3 "$DB_PATH" "SELECT chunk_index, status, tokens_used, processing_time_ms FROM chunk_metadata WHERE run_id=$run_id AND source_file='$file_name' ORDER BY chunk_index;" 2>/dev/null
    fi
}

# Function to draw progress bar
draw_progress_bar() {
    local current=$1
    local total=$2
    local width=40
    
    if [[ $total -eq 0 ]]; then
        printf "[%-${width}s] 0%%" ""
        return
    fi
    
    local percent=$((current * 100 / total))
    local filled=$((current * width / total))
    local empty=$((width - filled))
    
    printf "["
    printf "%${filled}s" | tr ' ' '█'
    printf "%${empty}s" | tr ' ' '░'
    printf "] %d%% (%d/%d)" "$percent" "$current" "$total"
}

# Main monitoring loop
monitor() {
    local source_dir="${1:-source}"
    
    while true; do
        # Move cursor to top
        tput cup 4 0
        
        # Get stats
        IFS='|' read -r cbl_count cpy_count total_lines <<< "$(get_source_stats "$source_dir")"
        IFS='|' read -r run_id status files_processed chunks_total chunks_completed chunks_failed <<< "$(get_db_stats)"
        
        # Display source info
        echo -e "${CYAN}📁 Source Directory:${NC} $source_dir"
        echo -e "   COBOL Files: ${GREEN}$cbl_count${NC} | Copybooks: ${GREEN}$cpy_count${NC} | Total Lines: ${GREEN}$total_lines${NC}"
        echo ""
        
        # Display run info
        echo -e "${CYAN}🔄 Current Run:${NC} #$run_id"
        if [[ "$status" == "InProgress" ]]; then
            echo -e "   Status: ${YELLOW}⏳ $status${NC}"
        elif [[ "$status" == "Completed" ]]; then
            echo -e "   Status: ${GREEN}✅ $status${NC}"
        elif [[ "$status" == "Failed" ]]; then
            echo -e "   Status: ${RED}❌ $status${NC}"
        else
            echo -e "   Status: $status"
        fi
        echo ""
        
        # Display file progress
        local total_files=$((cbl_count + cpy_count))
        echo -e "${CYAN}📊 File Progress:${NC}"
        echo -n "   "
        draw_progress_bar "$files_processed" "$total_files"
        echo ""
        echo ""
        
        # Display chunk progress
        if [[ $chunks_total -gt 0 ]]; then
            echo -e "${CYAN}🧩 Chunk Progress:${NC}"
            echo -n "   "
            draw_progress_bar "$chunks_completed" "$chunks_total"
            echo ""
            if [[ $chunks_failed -gt 0 ]]; then
                echo -e "   ${RED}Failed Chunks: $chunks_failed${NC}"
            fi
            echo ""
        fi
        
        # Display latest activity
        echo -e "${CYAN}📡 Latest API Call:${NC}"
        echo "   $(get_latest_api_call)" | head -c 70
        echo ""
        echo ""
        
        echo -e "${CYAN}⚙️  Latest Processing:${NC}"
        echo "   $(get_latest_processing)" | head -c 70
        echo ""
        echo ""
        
        # Display large file chunk details
        if [[ -f "$DB_PATH" ]]; then
            local large_file=$(sqlite3 "$DB_PATH" "SELECT DISTINCT source_file FROM chunk_metadata WHERE run_id=$run_id LIMIT 1;" 2>/dev/null)
            if [[ -n "$large_file" ]]; then
                echo -e "${CYAN}🔍 Chunk Details for:${NC} $large_file"
                echo "   ┌─────────┬────────────┬────────────┬────────────┐"
                echo "   │ Chunk # │   Status   │   Tokens   │   Time(s)  │"
                echo "   ├─────────┼────────────┼────────────┼────────────┤"
                sqlite3 "$DB_PATH" "SELECT chunk_index, status, COALESCE(tokens_used, 0), COALESCE(processing_time_ms/1000.0, 0) FROM chunk_metadata WHERE run_id=$run_id AND source_file='$large_file' ORDER BY chunk_index LIMIT 10;" 2>/dev/null | while IFS='|' read -r idx st tok tm; do
                    printf "   │ %7s │ %10s │ %10s │ %10.1f │\n" "$idx" "$st" "$tok" "$tm"
                done
                echo "   └─────────┴────────────┴────────────┴────────────┘"
            fi
        fi
        
        echo ""
        echo -e "${BOLD}Press Ctrl+C to exit${NC} | Refreshing every 5 seconds..."
        
        sleep 5
    done
}

# Check for arguments
if [[ "$1" == "--help" || "$1" == "-h" ]]; then
    echo "Usage: $0 [source-directory]"
    echo ""
    echo "Monitor migration progress in real-time."
    echo "Default source directory: source"
    exit 0
fi

# Run monitor
monitor "${1:-source}"
