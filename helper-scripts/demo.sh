#!/bin/bash

# COBOL Migration Portal Demo Script
# This script starts Neo4j and the web portal without running a new migration
# Perfect for demonstrating existing data

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
# shellcheck source=../tools/lib/ports.sh
source "$REPO_ROOT/tools/lib/ports.sh"
PORTAL_PORT="${MCP_WEB_PORT:-5028}"
NEO4J_HTTP_PORT="${NEO4J_HTTP_PORT:-7474}"
NEO4J_BOLT_PORT="${NEO4J_BOLT_PORT:-7687}"
cd "$REPO_ROOT"

LOCAL_CONFIG="$REPO_ROOT/Config/ai-config.local.env"
if [[ ! -f "$LOCAL_CONFIG" ]]; then
    echo "Run ./doctor.sh setup before running the demo."
    exit 1
fi

neo4j_line=$(grep -E '^NEO4J_PASSWORD=' "$LOCAL_CONFIG" | tail -1)
export NEO4J_PASSWORD="${neo4j_line#NEO4J_PASSWORD=}"
NEO4J_PASSWORD="${NEO4J_PASSWORD%\"}"
NEO4J_PASSWORD="${NEO4J_PASSWORD#\"}"
NEO4J_PASSWORD="${NEO4J_PASSWORD%\'}"
NEO4J_PASSWORD="${NEO4J_PASSWORD#\'}"
export NEO4J_PASSWORD
export ApplicationSettings__Neo4j__Password="$NEO4J_PASSWORD"

if [[ -z "$NEO4J_PASSWORD" ]]; then
    echo "NEO4J_PASSWORD is required in Config/ai-config.local.env."
    exit 1
fi

echo "╔══════════════════════════════════════════════════════════════╗"
echo "║   COBOL Migration Portal - Demo Mode                        ║"
echo "║   (View existing data - No new analysis)                    ║"
echo "╚══════════════════════════════════════════════════════════════╝"
echo ""

# Function to check if a command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Function to check if Neo4j is running
neo4j_running() {
    docker ps | grep -q neo4j
}

# Function to check if Neo4j ports are in use
neo4j_port_conflict() {
    port_in_use "$NEO4J_HTTP_PORT" || port_in_use "$NEO4J_BOLT_PORT"
}

# Function to check if portal is running
portal_running() {
    port_in_use "$PORTAL_PORT"
}

# Check for required tools
echo "🔍 Checking prerequisites..."

if ! command_exists docker; then
    echo "❌ Docker is not installed. Please install Docker Desktop."
    exit 1
fi

if ! command_exists dotnet; then
    echo "❌ .NET SDK is not installed. Please install .NET 9 SDK."
    exit 1
fi

echo "✅ All prerequisites met"
echo ""

# Step 1: Start Neo4j if not running
echo "📊 Step 1: Starting Neo4j graph database..."
if neo4j_running; then
    echo "✅ Neo4j is already running"
elif neo4j_port_conflict; then
    echo "⚠️  Warning: Neo4j ports (${NEO4J_HTTP_PORT}/${NEO4J_BOLT_PORT}) are in use by another process"
    echo "   Checking if it's accessible..."
    if curl -s http://localhost:${NEO4J_HTTP_PORT} > /dev/null 2>&1; then
        echo "✅ Neo4j is accessible and ready to use"
    else
        echo "❌ Ports are blocked but Neo4j is not accessible"
        echo "   Please stop the conflicting process or container"
        exit 1
    fi
else
    echo "   Starting Neo4j container..."
    docker-compose up -d neo4j
    echo "   Waiting for Neo4j to be ready..."
    sleep 5
    
    # Wait for Neo4j to be ready
    max_attempts=30
    attempt=0
    while [ $attempt -lt $max_attempts ]; do
        if curl -s http://localhost:${NEO4J_HTTP_PORT} > /dev/null 2>&1; then
            echo "✅ Neo4j is ready"
            break
        fi
        attempt=$((attempt + 1))
        echo "   Waiting... ($attempt/$max_attempts)"
        sleep 2
    done
    
    if [ $attempt -eq $max_attempts ]; then
        echo "⚠️  Neo4j may not be fully ready, but continuing..."
    fi
fi
echo ""

# Step 2: Check database
echo "💾 Step 2: Checking database..."
DB_PATH="Data/migration.db"
if [ -f "$DB_PATH" ]; then
    # Get the latest run ID
    LATEST_RUN=$(sqlite3 "$DB_PATH" "SELECT MAX(Id) FROM MigrationRuns WHERE Status != 'Failed';" 2>/dev/null || echo "39")
    echo "✅ Database found with Run $LATEST_RUN"
    export MCP_RUN_ID=$LATEST_RUN
else
    echo "⚠️  No database found. Portal will show empty data."
    export MCP_RUN_ID=39
fi
echo ""

# Step 3: Stop any existing portal
echo "🧹 Step 3: Cleaning up old portal instances..."
if portal_running; then
    echo "   Stopping existing portal..."
    kill_port_listeners "$PORTAL_PORT" || true
    sleep 2
    
    # Final check
    if portal_running; then
        echo "❌ Failed to stop existing portal on port ${PORTAL_PORT}"
        echo "   Stop the process listening on port $PORTAL_PORT and retry"
        exit 1
    fi
fi
echo "✅ Ready to start fresh"
echo ""

# Step 4: Start the portal
echo "🚀 Step 4: Starting web portal..."
echo "   Portal will be available at: http://localhost:${PORTAL_PORT}"
echo "   Neo4j Browser available at: http://localhost:${NEO4J_HTTP_PORT}"
echo ""
echo "📝 Quick Demo Guide:"
echo "   1. Open http://localhost:${PORTAL_PORT} in your browser"
echo "   2. Try the suggestion chips for quick queries"
echo "   3. View the dependency graph on the right panel"
echo "   4. Ask questions about COBOL files and dependencies"
echo ""
echo "🔑 Neo4j Credentials (if needed):"
echo "   Username: neo4j"
echo "   Password: the NEO4J_PASSWORD value from Config/ai-config.local.env"
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Starting portal in background..."
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

# Use relative path from the script location or current directory
# If running from root: ./McpChatWeb
# If script is in helper-scripts/: ../McpChatWeb
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
cd "$PROJECT_ROOT/McpChatWeb"

# Start portal in background
nohup dotnet run --urls "http://localhost:${PORTAL_PORT}" > /tmp/cobol-portal.log 2>&1 &
PORTAL_PID=$!

# Wait for portal to be ready (max 30 seconds)
echo -n "⏳ Waiting for portal to start"
for i in {1..30}; do
  HTTP_CODE=$(curl -s -o /dev/null -w "%{http_code}" http://localhost:${PORTAL_PORT}/ 2>/dev/null || echo "000")
  if [ "$HTTP_CODE" = "200" ]; then
    echo ""
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "🎉 Portal is ready!"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo ""
    echo "🌐 Access your demo:"
    echo "   Portal:        http://localhost:${PORTAL_PORT}"
    echo "   Neo4j Browser: http://localhost:${NEO4J_HTTP_PORT}"
    echo ""
    echo "📊 Viewing Migration Run: $MCP_RUN_ID"
    echo ""
    echo "💡 In VS Code Dev Container:"
    echo "   1. Check the 'PORTS' tab (next to Terminal)"
    echo "   2. Click the globe icon next to port ${PORTAL_PORT} to open in browser"
    echo "   3. Or Ctrl+Click the URL above"
    echo ""
    echo "🛑 To stop the demo:"
    echo "   Portal: kill $PORTAL_PID"
    echo "   Neo4j:  docker-compose down"
    echo ""
    echo "📝 View portal logs: tail -f /tmp/cobol-portal.log"
    echo ""
    
    # Try to open in VS Code Simple Browser if available
    if [ -n "$VSCODE_GIT_IPC_HANDLE" ] || [ -n "$VSCODE_IPC_HOOK" ]; then
        echo "🚀 Attempting to open portal in VS Code..."
        echo "   If it doesn't auto-open, check the PORTS tab and click the globe icon next to port ${PORTAL_PORT}"
        code --open-url http://localhost:${PORTAL_PORT} 2>/dev/null || true
    fi
    
    echo ""
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "✨ Quick Commands:"
    echo "   Status:      ./helper-scripts/status.sh"
    echo "   Open Portal: ./helper-scripts/open-portal.sh"
    echo "   Stop All:    docker-compose down && kill $PORTAL_PID"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo ""
    
    exit 0
  fi
  echo -n "."
  sleep 1
done

# If we get here, portal failed to start
echo " ❌"
echo ""
echo "⚠️  Portal failed to start within 30 seconds"
echo ""
echo "📝 Check logs:"
echo "   tail -50 /tmp/cobol-portal.log"
echo ""
echo "🔧 Troubleshooting:"
echo "   1. Check if port $PORTAL_PORT is in use: helper-scripts/status.sh"
echo "   2. Try manually: cd McpChatWeb && MCP_RUN_ID=$MCP_RUN_ID dotnet run --urls \"http://localhost:${PORTAL_PORT}\""
echo "   3. Check .NET version: dotnet --version (should be 9.x)"
echo ""
echo "🧹 Cleaning up failed process..."
kill $PORTAL_PID 2>/dev/null || true
echo ""
echo "To stop Neo4j:"
echo "   docker-compose down"
