#!/bin/bash

# COBOL Migration Portal Demo Script
# This script starts Neo4j and the web portal without running a new migration
# Perfect for demonstrating existing data

set -e

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
    lsof -ti:7474 >/dev/null 2>&1 || lsof -ti:7687 >/dev/null 2>&1
}

# Function to check if portal is running
portal_running() {
    lsof -ti:5028 >/dev/null 2>&1
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
    echo "⚠️  Warning: Neo4j ports (7474/7687) are in use by another process"
    echo "   Checking if it's accessible..."
    if curl -s http://localhost:7474 > /dev/null 2>&1; then
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
        if curl -s http://localhost:7474 > /dev/null 2>&1; then
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
    pkill -f "dotnet.*McpChatWeb" 2>/dev/null || true
    sleep 2
    
    # Force kill if still running
    if portal_running; then
        echo "   Force stopping portal..."
        pkill -9 -f "dotnet.*McpChatWeb" 2>/dev/null || true
        sleep 1
    fi
    
    # Final check
    if portal_running; then
        echo "❌ Failed to stop existing portal on port 5028"
        echo "   Run: lsof -ti:5028 | xargs kill -9"
        exit 1
    fi
fi
echo "✅ Ready to start fresh"
echo ""

# Step 4: Start the portal
echo "🚀 Step 4: Starting web portal..."
echo "   Portal will be available at: http://localhost:5028"
echo "   Neo4j Browser available at: http://localhost:7474"
echo ""
echo "📝 Quick Demo Guide:"
echo "   1. Open http://localhost:5028 in your browser"
echo "   2. Try the suggestion chips for quick queries"
echo "   3. View the dependency graph on the right panel"
echo "   4. Ask questions about COBOL files and dependencies"
echo ""
echo "🔑 Neo4j Credentials (if needed):"
echo "   Username: neo4j"
echo "   Password: cobol-migration-2025"
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Starting portal in background..."
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

cd /workspaces/Legacy-Modernization-Agents/McpChatWeb

# Start portal in background
nohup dotnet run --urls "http://localhost:5028" > /tmp/cobol-portal.log 2>&1 &
PORTAL_PID=$!

# Wait for portal to be ready (max 30 seconds)
echo -n "⏳ Waiting for portal to start"
for i in {1..30}; do
  HTTP_CODE=$(curl -s -o /dev/null -w "%{http_code}" http://localhost:5028/ 2>/dev/null || echo "000")
  if [ "$HTTP_CODE" = "200" ]; then
    echo ""
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "🎉 Portal is ready!"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo ""
    echo "🌐 Access your demo:"
    echo "   Portal:        http://localhost:5028"
    echo "   Neo4j Browser: http://localhost:7474"
    echo ""
    echo "📊 Viewing Migration Run: $MCP_RUN_ID"
    echo ""
    echo "💡 In VS Code Dev Container:"
    echo "   1. Check the 'PORTS' tab (next to Terminal)"
    echo "   2. Click the globe icon next to port 5028 to open in browser"
    echo "   3. Or Ctrl+Click the URL above"
    echo ""
    echo "🛑 To stop the demo:"
    echo "   Portal: kill $PORTAL_PID  (or: pkill -f 'dotnet.*McpChatWeb')"
    echo "   Neo4j:  docker-compose down"
    echo ""
    echo "📝 View portal logs: tail -f /tmp/cobol-portal.log"
    echo ""
    
    # Try to open in VS Code Simple Browser if available
    if [ -n "$VSCODE_GIT_IPC_HANDLE" ] || [ -n "$VSCODE_IPC_HOOK" ]; then
        echo "🚀 Attempting to open portal in VS Code..."
        echo "   If it doesn't auto-open, check the PORTS tab and click the globe icon next to port 5028"
        code --open-url http://localhost:5028 2>/dev/null || true
    fi
    
    echo ""
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "✨ Quick Commands:"
    echo "   Status:      ./helper-scripts/status.sh"
    echo "   Open Portal: ./helper-scripts/open-portal.sh"
    echo "   Stop All:    docker-compose down && pkill -f McpChatWeb"
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
echo "   1. Check if port 5028 is in use: lsof -i :5028"
echo "   2. Try manually: cd McpChatWeb && MCP_RUN_ID=$MCP_RUN_ID dotnet run --urls \"http://localhost:5028\""
echo "   3. Check .NET version: dotnet --version (should be 9.x)"
echo ""
echo "🧹 Cleaning up failed process..."
kill $PORTAL_PID 2>/dev/null || true
echo ""
echo "To stop Neo4j:"
echo "   docker-compose down"
