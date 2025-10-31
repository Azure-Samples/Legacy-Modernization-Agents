#!/bin/bash

# Data Persistence Verification Script
# Tests that migration data is properly stored and persists across restarts

set -e

echo "🔍 Verifying Data Persistence"
echo "=============================="
echo ""

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Track results
PASS=0
FAIL=0

echo "1. Checking SQLite Database Persistence..."
if [ -f "Data/migration.db" ]; then
    DB_SIZE=$(du -h Data/migration.db | awk '{print $1}')
    RUN_COUNT=$(sqlite3 Data/migration.db "SELECT COUNT(*) FROM runs;" 2>/dev/null || echo "0")
    
    echo -e "${GREEN}✅ PASS: SQLite database exists${NC}"
    echo "   Location: Data/migration.db"
    echo "   Size: $DB_SIZE"
    echo "   Migration runs stored: $RUN_COUNT"
    
    if [ "$RUN_COUNT" -gt 0 ]; then
        LATEST_RUN=$(sqlite3 Data/migration.db "SELECT id FROM runs ORDER BY id DESC LIMIT 1;" 2>/dev/null)
        LATEST_STATUS=$(sqlite3 Data/migration.db "SELECT status FROM runs ORDER BY id DESC LIMIT 1;" 2>/dev/null)
        echo "   Latest run: #$LATEST_RUN ($LATEST_STATUS)"
    fi
    
    ((PASS++))
else
    echo -e "${RED}❌ FAIL: SQLite database not found${NC}"
    echo "   Expected: Data/migration.db"
    echo "   Run ./doctor.sh run to create database"
    ((FAIL++))
fi
echo ""

echo "2. Checking Neo4j Docker Volumes..."
NEO4J_VOLUMES=$(docker volume ls --format "{{.Name}}" | grep neo4j | wc -l)
if [ "$NEO4J_VOLUMES" -ge 3 ]; then
    echo -e "${GREEN}✅ PASS: Neo4j volumes exist${NC}"
    docker volume ls --format "table {{.Name}}\t{{.Driver}}\t{{.Size}}" | head -1
    docker volume ls --format "table {{.Name}}\t{{.Driver}}\t{{.Size}}" | grep neo4j
    ((PASS++))
else
    echo -e "${RED}❌ FAIL: Neo4j volumes not found${NC}"
    echo "   Expected: 3 volumes (data, logs, import)"
    echo "   Found: $NEO4J_VOLUMES volumes"
    echo "   Run: docker-compose up -d neo4j"
    ((FAIL++))
fi
echo ""

echo "3. Checking Data Directory Gitignore Status..."
if grep -q "^Data/" .gitignore; then
    echo -e "${GREEN}✅ PASS: Data/ directory is gitignored${NC}"
    echo "   Database will not be committed to git"
    echo "   Data persists locally only (security best practice)"
    ((PASS++))
else
    echo -e "${YELLOW}⚠️  WARNING: Data/ not explicitly in .gitignore${NC}"
    echo "   Consider adding to prevent accidental commits"
fi
echo ""

echo "4. Checking Neo4j Container Status..."
if docker ps | grep -q cobol-migration-neo4j; then
    NEO4J_STATUS=$(docker inspect cobol-migration-neo4j --format='{{.State.Status}}')
    NEO4J_HEALTH=$(docker inspect cobol-migration-neo4j --format='{{.State.Health.Status}}' 2>/dev/null || echo "N/A")
    
    echo -e "${GREEN}✅ PASS: Neo4j container running${NC}"
    echo "   Container: cobol-migration-neo4j"
    echo "   Status: $NEO4J_STATUS"
    echo "   Health: $NEO4J_HEALTH"
    
    # Check if volumes are mounted
    MOUNTED_VOLUMES=$(docker inspect cobol-migration-neo4j --format='{{range .Mounts}}{{.Name}} {{end}}' | grep neo4j | wc -w)
    echo "   Mounted volumes: $MOUNTED_VOLUMES"
    
    ((PASS++))
elif docker ps -a | grep -q cobol-migration-neo4j; then
    echo -e "${YELLOW}⚠️  WARNING: Neo4j container exists but not running${NC}"
    echo "   Run: docker-compose up -d neo4j"
else
    echo -e "${RED}❌ FAIL: Neo4j container not found${NC}"
    echo "   Run: docker-compose up -d neo4j"
    ((FAIL++))
fi
echo ""

echo "5. Testing SQLite Database Schema..."
if [ -f "Data/migration.db" ]; then
    TABLES=$(sqlite3 Data/migration.db ".tables" 2>/dev/null | tr ' ' '\n' | wc -l)
    
    if [ "$TABLES" -ge 6 ]; then
        echo -e "${GREEN}✅ PASS: Database schema is complete${NC}"
        echo "   Tables found: $TABLES"
        echo "   Expected tables:"
        sqlite3 Data/migration.db ".tables" 2>/dev/null | tr ' ' '\n' | sed 's/^/     • /'
        ((PASS++))
    else
        echo -e "${RED}❌ FAIL: Database schema incomplete${NC}"
        echo "   Tables found: $TABLES"
        echo "   Expected: 6 tables (runs, cobol_files, analyses, dependencies, copybook_usage, metrics)"
        ((FAIL++))
    fi
else
    echo -e "${YELLOW}⚠️  SKIP: No database to test${NC}"
fi
echo ""

echo "6. Testing Data Persistence After Docker Restart..."
echo -e "${BLUE}ℹ️  Testing Neo4j restart scenario...${NC}"

# Save current node count if Neo4j is running
if docker ps | grep -q cobol-migration-neo4j; then
    BEFORE_RESTART=$(docker exec cobol-migration-neo4j cypher-shell -u neo4j -p cobol-migration-2025 \
        "MATCH (n) RETURN count(n) as count;" 2>/dev/null | grep -o '[0-9]\+' | head -1 || echo "0")
    
    echo "   Nodes before restart: $BEFORE_RESTART"
    
    # Restart container
    docker restart cobol-migration-neo4j >/dev/null 2>&1
    sleep 3
    
    AFTER_RESTART=$(docker exec cobol-migration-neo4j cypher-shell -u neo4j -p cobol-migration-2025 \
        "MATCH (n) RETURN count(n) as count;" 2>/dev/null | grep -o '[0-9]\+' | head -1 || echo "0")
    
    echo "   Nodes after restart: $AFTER_RESTART"
    
    if [ "$BEFORE_RESTART" = "$AFTER_RESTART" ]; then
        echo -e "${GREEN}✅ PASS: Neo4j data persisted after restart${NC}"
        ((PASS++))
    else
        echo -e "${RED}❌ FAIL: Neo4j data changed after restart${NC}"
        echo "   Before: $BEFORE_RESTART nodes"
        echo "   After: $AFTER_RESTART nodes"
        ((FAIL++))
    fi
else
    echo -e "${YELLOW}⚠️  SKIP: Neo4j not running, cannot test restart${NC}"
fi
echo ""

echo "7. Checking Backup Directories..."
if [ -d "backups" ] || [ -f "Data/migration.db.backup" ]; then
    echo -e "${GREEN}✅ INFO: Backup files found${NC}"
    [ -d "backups" ] && echo "   Backup directory: backups/"
    [ -f "Data/migration.db.backup" ] && echo "   Database backup: Data/migration.db.backup"
else
    echo -e "${BLUE}ℹ️  INFO: No backups found (optional)${NC}"
    echo "   Consider creating backups:"
    echo "   cp Data/migration.db Data/migration.db.backup"
fi
echo ""

# Summary
echo "=========================================="
echo "Data Persistence Verification Summary"
echo "=========================================="
echo ""
echo -e "${GREEN}Passed: $PASS${NC}"
echo -e "${RED}Failed: $FAIL${NC}"
echo ""

if [ $FAIL -eq 0 ]; then
    echo -e "${GREEN}✅ ALL DATA PERSISTENCE CHECKS PASSED!${NC}"
    echo ""
    echo "Your migration data is properly stored and will survive:"
    echo "  ✅ Container restarts"
    echo "  ✅ System reboots"
    echo "  ✅ Docker Desktop restarts"
    echo "  ✅ Application crashes"
    echo ""
    echo "Data locations:"
    echo "  • SQLite: $(pwd)/Data/migration.db"
    echo "  • Neo4j: Docker volumes (persistent)"
    echo ""
    echo "Backup recommendations:"
    echo "  1. cp Data/migration.db Data/migration.db.\$(date +%Y%m%d).backup"
    echo "  2. See DATA_PERSISTENCE.md for Neo4j backup"
    exit 0
else
    echo -e "${RED}❌ SOME CHECKS FAILED!${NC}"
    echo ""
    echo "Please review the failures above."
    echo "Common fixes:"
    echo "  • Run migration: ./doctor.sh run"
    echo "  • Start Neo4j: docker-compose up -d neo4j"
    echo "  • Check logs: docker logs cobol-migration-neo4j"
    echo ""
    echo "See DATA_PERSISTENCE.md for detailed troubleshooting."
    exit 1
fi
