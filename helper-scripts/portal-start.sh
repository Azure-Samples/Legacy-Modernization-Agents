#!/bin/bash

# Script to start the MCP Chat Web portal
# Automatically handles port conflicts and provides clear user feedback

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PORT="${MCP_WEB_PORT:-5028}"
HOST="${MCP_WEB_HOST:-localhost}"
URL="http://$HOST:$PORT"

echo -e "${BLUE}🌐 Starting MCP Chat Web Portal...${NC}"
echo "=================================="
echo ""

# Check if McpChatWeb project exists
if [[ ! -f "$REPO_ROOT/McpChatWeb/McpChatWeb.csproj" ]]; then
    echo -e "${RED}❌ McpChatWeb project not found${NC}"
    echo -e "${YELLOW}Expected location: $REPO_ROOT/McpChatWeb/${NC}"
    exit 1
fi

# Check if migration database exists
DB_PATH="${MIGRATION_DB_PATH:-$REPO_ROOT/Data/migration.db}"
if [[ ! -f "$DB_PATH" ]]; then
    echo -e "${YELLOW}⚠️  Warning: Migration database not found at: $DB_PATH${NC}"
    echo -e "${CYAN}💡 Run a migration first with:${NC} ./doctor.sh run"
    echo ""
    read -p "Continue anyway? (y/N): " -n 1 -r
    echo ""
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo -e "${BLUE}Cancelled.${NC}"
        exit 0
    fi
fi

# Check if port is already in use
if lsof -Pi :$PORT -sTCP:LISTEN -t >/dev/null 2>&1; then
    echo -e "${YELLOW}⚠️  Port $PORT is already in use${NC}"
    
    PID=$(lsof -ti:$PORT)
    PROCESS=$(ps -p $PID -o comm= 2>/dev/null || echo "unknown")
    
    echo -e "Process using port: ${BOLD}$PROCESS${NC} (PID: $PID)"
    echo ""
    
    read -p "Kill the existing process and continue? (Y/n): " -n 1 -r
    echo ""
    
    if [[ $REPLY =~ ^[Nn]$ ]]; then
        echo -e "${BLUE}Cancelled. Try a different port with:${NC}"
        echo -e "   ${BOLD}MCP_WEB_PORT=5029 $0${NC}"
        exit 0
    fi
    
    echo -e "${BLUE}Stopping existing process...${NC}"
    kill -9 $PID 2>/dev/null && echo -e "${GREEN}✅ Process stopped${NC}" || echo -e "${RED}❌ Failed to stop process${NC}"
    sleep 1
    
    # Verify port is free
    if lsof -Pi :$PORT -sTCP:LISTEN -t >/dev/null 2>&1; then
        echo -e "${RED}❌ Port $PORT is still in use${NC}"
        exit 1
    fi
fi

# Export database path
export MIGRATION_DB_PATH="$DB_PATH"

echo -e "${GREEN}✅ Starting web server at:${NC} ${BOLD}$URL${NC}"
echo -e "${CYAN}💡 The portal will open automatically in your browser${NC}"
echo -e "${BLUE}➡️  Press Ctrl+C to stop the server${NC}"
echo ""

# Try to open browser
sleep 2 &
SLEEP_PID=$!

# Wait a bit then try to open browser
(
    sleep 3
    if command -v code >/dev/null 2>&1 && [ -n "$VSCODE_GIT_IPC_HANDLE" ]; then
        code --open-url "$URL" 2>/dev/null || true
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        open "$URL" 2>/dev/null || true
    elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
        if command -v xdg-open >/dev/null 2>&1; then
            xdg-open "$URL" 2>/dev/null || true
        fi
    fi
) &

# Start the web server
cd "$REPO_ROOT/McpChatWeb" && dotnet run --urls "$URL"

EXIT_CODE=$?

echo ""
if [[ $EXIT_CODE -eq 0 ]]; then
    echo -e "${GREEN}✅ Portal stopped gracefully${NC}"
else
    echo -e "${YELLOW}⚠️  Portal exited with code: $EXIT_CODE${NC}"
fi

exit $EXIT_CODE
