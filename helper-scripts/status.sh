#!/bin/bash

# Status check script for COBOL Migration Portal
# Shows the current state of all services

echo "╔══════════════════════════════════════════════════════════════╗"
echo "║   COBOL Migration Portal - Status Check                     ║"
echo "╚══════════════════════════════════════════════════════════════╝"
echo ""

# Function to check service status with colored output
check_service() {
    local name=$1
    local check_command=$2
    local url=$3
    
    echo -n "📊 $name: "
    
    if eval "$check_command" >/dev/null 2>&1; then
        if [ -n "$url" ]; then
            HTTP_CODE=$(curl -s -o /dev/null -w "%{http_code}" "$url" 2>/dev/null || echo "000")
            if [ "$HTTP_CODE" = "200" ]; then
                echo "✅ Running & Accessible (HTTP 200)"
            elif [ "$HTTP_CODE" = "000" ]; then
                echo "⚠️  Running but not responding"
            else
                echo "⚠️  Running (HTTP $HTTP_CODE)"
            fi
        else
            echo "✅ Running"
        fi
        return 0
    else
        echo "❌ Not running"
        return 1
    fi
}

# Check Portal
PORTAL_RUNNING=false
if check_service "Portal (Port 5028)" "lsof -ti:5028" "http://localhost:5028/"; then
    PORTAL_RUNNING=true
    PORTAL_PID=$(lsof -ti:5028)
    echo "   Process ID: $PORTAL_PID"
    echo "   Logs: tail -f /tmp/cobol-portal.log"
fi

echo ""

# Check Neo4j
NEO4J_RUNNING=false
if check_service "Neo4j Container" "docker ps | grep -q neo4j"; then
    NEO4J_RUNNING=true
    NEO4J_STATUS=$(docker ps --filter "name=neo4j" --format "{{.Status}}")
    echo "   Status: $NEO4J_STATUS"
fi

echo ""

# Check Database
DB_PATH="/workspaces/Legacy-Modernization-Agents/Data/migration.db"
if [ -f "$DB_PATH" ]; then
    LATEST_RUN=$(sqlite3 "$DB_PATH" "SELECT MAX(Id) FROM MigrationRuns WHERE Status != 'Failed';" 2>/dev/null || echo "0")
    RUN_COUNT=$(sqlite3 "$DB_PATH" "SELECT COUNT(*) FROM MigrationRuns;" 2>/dev/null || echo "0")
    FILE_COUNT=$(sqlite3 "$DB_PATH" "SELECT COUNT(*) FROM cobol_files WHERE run_id = $LATEST_RUN;" 2>/dev/null || echo "0")
    
    echo "📊 Database: ✅ Found"
    echo "   Location: $DB_PATH"
    echo "   Total Runs: $RUN_COUNT"
    echo "   Latest Run: #$LATEST_RUN"
    echo "   Files in Run: $FILE_COUNT"
else
    echo "📊 Database: ❌ Not found"
    echo "   Expected: $DB_PATH"
fi

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# Summary and actions
if [ "$PORTAL_RUNNING" = true ] && [ "$NEO4J_RUNNING" = true ]; then
    echo "✅ All services running!"
    echo ""
    echo "🌐 Access Points:"
    echo "   Portal:  http://localhost:5028"
    echo "   Neo4j:   http://localhost:7474"
    echo ""
    echo "💡 To open portal: ./helper-scripts/open-portal.sh"
elif [ "$PORTAL_RUNNING" = false ] && [ "$NEO4J_RUNNING" = false ]; then
    echo "❌ Services not running"
    echo ""
    echo "🚀 To start everything:"
    echo "   ./helper-scripts/demo.sh"
elif [ "$PORTAL_RUNNING" = false ]; then
    echo "⚠️  Portal not running (but Neo4j is)"
    echo ""
    echo "🚀 To start portal:"
    echo "   cd McpChatWeb && dotnet run --urls http://localhost:5028"
else
    echo "⚠️  Neo4j not running (but portal is)"
    echo ""
    echo "🚀 To start Neo4j:"
    echo "   docker-compose up -d neo4j"
fi

echo ""
