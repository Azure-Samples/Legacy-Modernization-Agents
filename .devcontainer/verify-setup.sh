#!/bin/bash

# Dev Container Setup Verification Script
# Checks if all components are properly configured and running

set -e

echo "╔══════════════════════════════════════════════════════════════╗"
echo "║   Dev Container Setup Verification                          ║"
echo "╚══════════════════════════════════════════════════════════════╝"
echo ""

ERRORS=0
WARNINGS=0

# Function to check status
check_status() {
    if [ $? -eq 0 ]; then
        echo "✅ $1"
    else
        echo "❌ $1"
        ERRORS=$((ERRORS + 1))
    fi
}

check_warning() {
    if [ $? -eq 0 ]; then
        echo "✅ $1"
    else
        echo "⚠️  $1"
        WARNINGS=$((WARNINGS + 1))
    fi
}

echo "🔍 Checking system dependencies..."
echo ""

# Check .NET SDK
dotnet --version > /dev/null 2>&1
check_status ".NET SDK installed ($(dotnet --version 2>/dev/null || echo 'not found'))"

# Check Java
java -version > /dev/null 2>&1
check_status "Java JDK installed ($(java -version 2>&1 | head -n 1 | awk -F '"' '{print $2}' || echo 'not found'))"

# Check Docker
docker --version > /dev/null 2>&1
check_status "Docker CLI installed"

# Check Docker Compose
docker-compose --version > /dev/null 2>&1
check_status "Docker Compose installed"

# Check SQLite
sqlite3 --version > /dev/null 2>&1
check_status "SQLite3 installed"

# Check cypher-shell
cypher-shell --version > /dev/null 2>&1
check_status "Neo4j cypher-shell installed"

echo ""
echo "🐳 Checking Docker containers..."
echo ""

# Check if Docker daemon is accessible
docker ps > /dev/null 2>&1
check_status "Docker daemon accessible"

# Check Neo4j container
if docker ps | grep -q "cobol-migration-neo4j"; then
    NEO4J_STATUS=$(docker inspect --format='{{.State.Status}}' cobol-migration-neo4j)
    check_status "Neo4j container running (status: $NEO4J_STATUS)"
    
    # Check Neo4j health
    if docker inspect --format='{{.State.Health.Status}}' cobol-migration-neo4j 2>/dev/null | grep -q "healthy"; then
        echo "✅ Neo4j health check passed"
    else
        echo "⚠️  Neo4j health check pending (container may still be starting)"
        WARNINGS=$((WARNINGS + 1))
    fi
    
    # Test Neo4j HTTP endpoint
    if curl -s http://localhost:7474 > /dev/null 2>&1; then
        echo "✅ Neo4j HTTP endpoint accessible (http://localhost:7474)"
    else
        echo "⚠️  Neo4j HTTP endpoint not yet accessible (may still be starting)"
        WARNINGS=$((WARNINGS + 1))
    fi
else
    echo "❌ Neo4j container not running"
    ERRORS=$((ERRORS + 1))
    echo "   💡 Run: docker-compose up -d neo4j"
fi

echo ""
echo "📁 Checking workspace structure..."
echo ""

# Check directories
[ -d "/workspaces/Legacy-Modernization-Agents/Data" ]
check_status "Data directory exists"

[ -d "/workspaces/Legacy-Modernization-Agents/Logs" ]
check_status "Logs directory exists"

[ -d "/workspaces/Legacy-Modernization-Agents/source" ]
check_status "source directory exists"

[ -d "/workspaces/Legacy-Modernization-Agents/output" ]
check_status "output directory exists"

[ -d "/workspaces/Legacy-Modernization-Agents/McpChatWeb" ]
check_status "McpChatWeb directory exists"

echo ""
echo "💾 Checking project files..."
echo ""

# Check project files
[ -f "/workspaces/Legacy-Modernization-Agents/CobolToQuarkusMigration.csproj" ]
check_status "Main project file exists"

[ -f "/workspaces/Legacy-Modernization-Agents/McpChatWeb/McpChatWeb.csproj" ]
check_status "McpChatWeb project file exists"

[ -f "/workspaces/Legacy-Modernization-Agents/docker-compose.yml" ]
check_status "docker-compose.yml exists"

[ -f "/workspaces/Legacy-Modernization-Agents/helper-scripts/demo.sh" ] && [ -x "/workspaces/Legacy-Modernization-Agents/helper-scripts/demo.sh" ]
check_status "demo.sh script exists and is executable"

[ -f "/workspaces/Legacy-Modernization-Agents/doctor.sh" ] && [ -x "/workspaces/Legacy-Modernization-Agents/doctor.sh" ]
check_status "doctor.sh script exists and is executable"

echo ""
echo "📊 Checking database..."
echo ""

# Check if database exists
if [ -f "/workspaces/Legacy-Modernization-Agents/Data/migration.db" ]; then
    DB_SIZE=$(du -h /workspaces/Legacy-Modernization-Agents/Data/migration.db | awk '{print $1}')
    echo "✅ SQLite database exists (size: $DB_SIZE)"
    
    # Count runs in database
    if command -v sqlite3 > /dev/null 2>&1; then
        RUN_COUNT=$(sqlite3 /workspaces/Legacy-Modernization-Agents/Data/migration.db "SELECT COUNT(*) FROM runs;" 2>/dev/null || echo "0")
        if [ "$RUN_COUNT" -gt 0 ]; then
            echo "✅ Database contains $RUN_COUNT migration run(s)"
            LATEST_RUN=$(sqlite3 /workspaces/Legacy-Modernization-Agents/Data/migration.db "SELECT id FROM runs ORDER BY id DESC LIMIT 1;" 2>/dev/null)
            echo "   Latest run ID: $LATEST_RUN"
        else
            echo "⚠️  Database exists but has no migration runs yet"
            WARNINGS=$((WARNINGS + 1))
        fi
    fi
else
    echo "⚠️  SQLite database not found (no migrations run yet)"
    echo "   💡 Run: ./helper-scripts/demo.sh or ./doctor.sh run"
    WARNINGS=$((WARNINGS + 1))
fi

echo ""
echo "⚙️  Checking configuration..."
echo ""

# Check AI config
if [ -f "/workspaces/Legacy-Modernization-Agents/Config/ai-config.local.env" ]; then
    echo "✅ AI configuration file exists"
    
    # Check if it contains real values (not template)
    if grep -q "test-api-key" /workspaces/Legacy-Modernization-Agents/Config/ai-config.local.env; then
        echo "⚠️  AI configuration contains template values"
        echo "   💡 Update Config/ai-config.local.env with your Azure OpenAI credentials"
        WARNINGS=$((WARNINGS + 1))
    else
        echo "✅ AI configuration appears to be customized"
    fi
else
    echo "⚠️  AI configuration file not found"
    echo "   💡 Copy Config/ai-config.env.example to Config/ai-config.local.env"
    WARNINGS=$((WARNINGS + 1))
fi

echo ""
echo "╔══════════════════════════════════════════════════════════════╗"
echo "║   Verification Summary                                       ║"
echo "╚══════════════════════════════════════════════════════════════╝"
echo ""

if [ $ERRORS -eq 0 ] && [ $WARNINGS -eq 0 ]; then
    echo "✅ All checks passed! Your dev container is fully configured."
    echo ""
    echo "🚀 Next steps:"
    echo "   1. Configure Azure OpenAI credentials (if not done)"
    echo "   2. Run: ./helper-scripts/demo.sh"
    echo "   3. Open: http://localhost:5028"
    echo ""
    exit 0
elif [ $ERRORS -eq 0 ]; then
    echo "⚠️  Setup complete with $WARNINGS warning(s)"
    echo "   Your dev container is functional but some optional features may not be available."
    echo ""
    exit 0
else
    echo "❌ Setup incomplete: $ERRORS error(s), $WARNINGS warning(s)"
    echo "   Please fix the errors above before proceeding."
    echo ""
    exit 1
fi
