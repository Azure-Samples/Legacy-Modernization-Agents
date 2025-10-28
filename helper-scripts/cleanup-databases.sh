#!/bin/bash

# cleanup-databases.sh
# Removes all database data to prepare repository for clean distribution
# Schema will be automatically recreated on first run

set -e

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "🗑️  Database Cleanup Script"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""
echo "This script will:"
echo "  1. Remove SQLite database file (Data/migration.db)"
echo "  2. Stop and remove Neo4j Docker container and volumes"
echo "  3. Clean all migration data"
echo ""
echo "⚠️  This action cannot be undone!"
echo ""
read -p "Continue? (y/N): " -n 1 -r
echo ""

if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "❌ Cleanup cancelled"
    exit 0
fi

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "📦 Step 1: Removing SQLite Database"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

if [ -f "Data/migration.db" ]; then
    rm -f "Data/migration.db"
    echo "✅ Removed Data/migration.db"
else
    echo "ℹ️  Data/migration.db not found (already clean)"
fi

# Also remove any SQLite journal/wal files
if [ -f "Data/migration.db-journal" ]; then
    rm -f "Data/migration.db-journal"
    echo "✅ Removed Data/migration.db-journal"
fi

if [ -f "Data/migration.db-wal" ]; then
    rm -f "Data/migration.db-wal"
    echo "✅ Removed Data/migration.db-wal"
fi

if [ -f "Data/migration.db-shm" ]; then
    rm -f "Data/migration.db-shm"
    echo "✅ Removed Data/migration.db-shm"
fi

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "🗄️  Step 2: Removing Neo4j Database"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# Check if docker-compose is available
if ! command -v docker-compose &> /dev/null && ! docker compose version &> /dev/null; then
    echo "⚠️  Docker Compose not found - skipping Neo4j cleanup"
    echo "ℹ️  If you have Neo4j running, manually stop it with:"
    echo "    docker-compose down -v"
else
    # Try docker-compose (older) or docker compose (newer)
    if command -v docker-compose &> /dev/null; then
        COMPOSE_CMD="docker-compose"
    else
        COMPOSE_CMD="docker compose"
    fi
    
    echo "Stopping Neo4j container and removing volumes..."
    $COMPOSE_CMD down -v 2>&1 | grep -v "Warning: No resource found" || true
    echo "✅ Neo4j container and volumes removed"
fi

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "✨ Cleanup Complete!"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""
echo "📋 Summary:"
echo "  • SQLite database deleted"
echo "  • Neo4j container and volumes removed"
echo "  • All migration data cleaned"
echo ""
echo "ℹ️  Note: Database schemas will be automatically recreated on next run"
echo "         - SQLite: Uses 'CREATE TABLE IF NOT EXISTS'"
echo "         - Neo4j: Fresh container initialization"
echo ""
echo "🚀 Your repository is now ready for distribution!"
echo ""
