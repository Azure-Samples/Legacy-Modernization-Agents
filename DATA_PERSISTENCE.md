# Data Persistence and Storage

## Overview

All migration data is **automatically persisted** to disk and survives system restarts, container restarts, and shutdowns. The system uses a **hybrid database architecture** with both SQLite and Neo4j for reliable data storage.

## Storage Architecture

### 1. SQLite Database (Metadata)

**Location:** `Data/migration.db`

**Purpose:** Stores all migration run metadata, file analyses, and structured data

**Persistence:** 
- ✅ Stored on local filesystem in `Data/` directory
- ✅ Survives container restarts
- ✅ Survives system shutdowns
- ✅ Single 8-10MB file (easy to backup)
- ✅ Automatically created on first migration

**What's Stored:**
- Migration runs (44 runs in current database)
- Run metadata (status, timestamps, paths)
- COBOL file information
- Code analysis results
- Dependencies mapping
- Copybook usage
- Metrics and insights

**Schema:**
```sql
-- Migration runs
CREATE TABLE runs (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    started_at TEXT NOT NULL,
    completed_at TEXT,
    status TEXT NOT NULL,
    cobol_source TEXT,
    java_output TEXT,
    notes TEXT
);

-- COBOL files
CREATE TABLE cobol_files (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    run_id INTEGER NOT NULL,
    file_name TEXT NOT NULL,
    file_path TEXT NOT NULL,
    is_copybook INTEGER NOT NULL,
    content TEXT,
    FOREIGN KEY(run_id) REFERENCES runs(id)
);

-- Analysis results
CREATE TABLE analyses (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    cobol_file_id INTEGER NOT NULL,
    program_description TEXT,
    raw_analysis TEXT,
    data_divisions_json TEXT,
    procedure_divisions_json TEXT,
    variables_json TEXT,
    paragraphs_json TEXT,
    copybooks_json TEXT,
    FOREIGN KEY(cobol_file_id) REFERENCES cobol_files(id)
);

-- Dependencies
CREATE TABLE dependencies (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    run_id INTEGER NOT NULL,
    source_file TEXT NOT NULL,
    target_file TEXT NOT NULL,
    dependency_type TEXT,
    line_number INTEGER,
    context TEXT,
    FOREIGN KEY(run_id) REFERENCES runs(id)
);

-- Copybook usage
CREATE TABLE copybook_usage (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    run_id INTEGER NOT NULL,
    program TEXT NOT NULL,
    copybook TEXT NOT NULL,
    FOREIGN KEY(run_id) REFERENCES runs(id)
);

-- Metrics
CREATE TABLE metrics (
    run_id INTEGER PRIMARY KEY,
    total_programs INTEGER,
    total_copybooks INTEGER,
    total_dependencies INTEGER,
    avg_dependencies_per_program REAL,
    most_used_copybook TEXT,
    most_used_copybook_count INTEGER,
    circular_dependencies_json TEXT,
    analysis_insights TEXT,
    mermaid_diagram TEXT,
    FOREIGN KEY(run_id) REFERENCES runs(id)
);
```

### 2. Neo4j Graph Database (Relationships)

**Location:** Docker volume `legacy-modernization-agents_neo4j_data`

**Purpose:** Stores dependency graph for visualization and graph queries

**Persistence:**
- ✅ Stored in Docker named volume
- ✅ Survives container restarts
- ✅ Survives container deletions (volume persists)
- ✅ Survives system shutdowns
- ✅ Automatically mounted on container start

**Docker Volumes:**
```bash
# Three persistent volumes for Neo4j
legacy-modernization-agents_neo4j_data    # Graph database
legacy-modernization-agents_neo4j_logs    # Neo4j logs
legacy-modernization-agents_neo4j_import  # Import directory
```

**What's Stored:**
- File nodes (COBOL programs and copybooks)
- Dependency edges (COPY, CALL, INCLUDE relationships)
- Graph metadata
- Query indices

**Configuration (docker-compose.yml):**
```yaml
volumes:
  - neo4j_data:/data         # Persistent data
  - neo4j_logs:/logs         # Persistent logs
  - neo4j_import:/var/lib/neo4j/import
```

## Data Persistence Guarantees

### ✅ What Survives

| Event | SQLite | Neo4j | Notes |
|-------|--------|-------|-------|
| Container Restart | ✅ Yes | ✅ Yes | Data fully preserved |
| System Reboot | ✅ Yes | ✅ Yes | All data intact |
| Container Deletion | ✅ Yes | ✅ Yes | Volumes persist |
| Docker Desktop Restart | ✅ Yes | ✅ Yes | Local files and volumes safe |
| Application Crash | ✅ Yes | ✅ Yes | Database transactions ACID-compliant |
| Power Failure | ⚠️ Partial | ⚠️ Partial | Uncommitted transactions lost |

### ❌ What Doesn't Persist (By Design)

| Data | Reason | Alternative |
|------|--------|-------------|
| Azure OpenAI API Keys | Security | Store in `Config/ai-config.env` (gitignored) |
| COBOL Source Files | Proprietary | Store in `cobol-source/` (gitignored) |
| Generated Java Output | Regenerable | Store in `java-output/` (gitignored) |
| Logs | Temporary | Store in `Logs/` (gitignored) |
| Temporary Files | Ephemeral | Cleared on restart |

## Verifying Data Persistence

### Check SQLite Database

```bash
# Check if database exists
ls -lh Data/migration.db

# Check database size
du -h Data/migration.db

# Count migration runs
sqlite3 Data/migration.db "SELECT COUNT(*) FROM runs;"

# View latest runs
sqlite3 Data/migration.db "SELECT id, status, started_at FROM runs ORDER BY id DESC LIMIT 5;"

# Check all tables
sqlite3 Data/migration.db ".tables"

# Show database schema
sqlite3 Data/migration.db ".schema"
```

### Check Neo4j Volumes

```bash
# List Neo4j volumes
docker volume ls | grep neo4j

# Inspect volume details
docker volume inspect legacy-modernization-agents_neo4j_data

# Check volume size
docker system df -v | grep neo4j_data

# Verify Neo4j is using persistent storage
docker inspect cobol-migration-neo4j | grep -A 10 Mounts
```

### Check Graph Data

```bash
# Access Neo4j Browser
open http://localhost:7474

# Run Cypher query to count nodes
MATCH (n) RETURN count(n) as total_nodes;

# Count relationships
MATCH ()-[r]->() RETURN count(r) as total_relationships;

# View file nodes
MATCH (f:File) RETURN f.name, f.type LIMIT 10;
```

## Backup and Restore

### SQLite Backup

```bash
# Simple copy backup
cp Data/migration.db Data/migration.db.backup

# Timestamped backup
cp Data/migration.db "Data/migration.db.$(date +%Y%m%d_%H%M%S).backup"

# SQLite dump (SQL format)
sqlite3 Data/migration.db .dump > migration_backup.sql

# Restore from SQL dump
sqlite3 Data/migration.db < migration_backup.sql
```

### Neo4j Backup

```bash
# Export all data as Cypher statements
docker exec cobol-migration-neo4j cypher-shell -u neo4j -p cobol-migration-2025 \
  "CALL apoc.export.cypher.all('backup.cypher', {format: 'cypher-shell'});"

# Stop Neo4j
docker-compose down neo4j

# Backup volume data
docker run --rm -v legacy-modernization-agents_neo4j_data:/data \
  -v $(pwd):/backup alpine tar czf /backup/neo4j_backup.tar.gz /data

# Restore volume data
docker run --rm -v legacy-modernization-agents_neo4j_data:/data \
  -v $(pwd):/backup alpine tar xzf /backup/neo4j_backup.tar.gz -C /
```

### Full System Backup

```bash
# Backup script
./backup-all.sh  # (create this)

# Manual backup
tar czf migration_backup_$(date +%Y%m%d).tar.gz \
  Data/migration.db \
  Config/ai-config.env \
  docker-compose.yml

# Backup Neo4j volumes
docker-compose down neo4j
docker run --rm -v legacy-modernization-agents_neo4j_data:/data \
  -v $(pwd)/backups:/backup alpine tar czf /backup/neo4j_data.tar.gz /data
```

## Data Location Summary

### Local Filesystem

```
Legacy-Modernization-Agents/
├── Data/
│   └── migration.db              # ✅ PERSISTED - All migration metadata
├── Config/
│   └── ai-config.env             # ⚠️  LOCAL ONLY - Azure credentials (gitignored)
├── cobol-source/                 # ⚠️  LOCAL ONLY - Proprietary COBOL (gitignored)
├── java-output/                  # ⚠️  LOCAL ONLY - Generated Java (gitignored)
└── Logs/                         # ⚠️  LOCAL ONLY - Runtime logs (gitignored)
```

### Docker Volumes

```
Docker Volumes:
├── legacy-modernization-agents_neo4j_data     # ✅ PERSISTED - Graph database
├── legacy-modernization-agents_neo4j_logs     # ✅ PERSISTED - Neo4j logs
└── legacy-modernization-agents_neo4j_import   # ✅ PERSISTED - Import staging
```

### DevContainer Volume Mounts

```json
{
  "mounts": [
    "source=/var/run/docker.sock,target=/var/run/docker.sock,type=bind"
  ],
  "workspaceMount": "source=${localWorkspaceFolder},target=/workspace,type=bind,consistency=cached",
  "workspaceFolder": "/workspace"
}
```

**Key Points:**
- `${localWorkspaceFolder}` mounted to `/workspace` → Local files persist
- Docker socket bind-mounted → Container can manage Neo4j volumes
- Neo4j volumes managed separately → Survive container deletion

## Data Security and Privacy

### What's NOT Stored in Git

The `.gitignore` file explicitly excludes:

```gitignore
# Azure OpenAI Credentials
Config/ai-config.local.env
*.key
*.secret

# COBOL Source Files (Proprietary)
cobol-source/
*.cbl
*.cpy

# Generated Java Output (Business Logic)
java-output/

# Logs (May contain sensitive data)
Logs/
*.log

# Migration Database (Business Data)
Data/
migration.db
```

### Sensitive Data Handling

| Data Type | Storage | Persisted | Git | Backup Strategy |
|-----------|---------|-----------|-----|-----------------|
| Azure API Keys | `Config/ai-config.env` | ✅ Local | ❌ No | Manual backup (encrypted) |
| COBOL Source | `cobol-source/` | ✅ Local | ❌ No | Version control separately |
| Java Output | `java-output/` | ✅ Local | ❌ No | Regenerable from migration |
| Migration DB | `Data/migration.db` | ✅ Local | ❌ No | SQLite backup scripts |
| Neo4j Data | Docker volume | ✅ Docker | ❌ No | Volume backup scripts |
| Logs | `Logs/` | ✅ Local | ❌ No | Rotate/archive as needed |

## Troubleshooting Data Persistence

### SQLite Database Not Found

```bash
# Check if Data directory exists
ls -la Data/

# Create Data directory if missing
mkdir -p Data

# Run migration to create database
./doctor.sh run
```

### Neo4j Volume Not Persisting

```bash
# Check if volumes exist
docker volume ls | grep neo4j

# Recreate volumes if missing
docker-compose down
docker volume create legacy-modernization-agents_neo4j_data
docker-compose up -d neo4j
```

### Data Lost After Container Deletion

```bash
# Volumes should persist even after container deletion
docker rm -f cobol-migration-neo4j  # Delete container
docker volume ls | grep neo4j        # Volumes still exist
docker-compose up -d neo4j           # Container recreated with same volumes
```

### Database Locked Error

```bash
# Check for open connections
lsof Data/migration.db

# Kill processes holding the database
pkill -f "dotnet.*McpChatWeb"

# Restart portal
./demo.sh
```

## Migration Run Counter

Current system state (as of check):

```bash
sqlite3 Data/migration.db "SELECT COUNT(*) FROM runs;"
# Result: 44 migration runs stored
```

**This proves:**
- ✅ Multiple runs stored successfully
- ✅ Data accumulated over time
- ✅ Database persists between sessions

## Summary

### ✅ Data Persistence Confirmed

1. **SQLite Database** (`Data/migration.db`)
   - 44 migration runs stored
   - 8.8MB file size
   - Fully persisted on local filesystem
   - Survives all restarts

2. **Neo4j Graph Database** (Docker volumes)
   - 3 persistent volumes created
   - Graph data survives container restarts
   - Volumes persist even after container deletion

3. **Configuration Files** (`Config/ai-config.env`)
   - Local storage only (security)
   - Not in git (sensitive credentials)
   - Manually backed up by user

### 🔒 Security Guarantees

- No sensitive data in git repository
- API keys stored locally only
- Proprietary COBOL source excluded
- Database files gitignored

### 📦 Backup Recommendations

1. **Daily:** Copy `Data/migration.db` to backup location
2. **Weekly:** Export Neo4j volume with tar backup script
3. **Before major changes:** Full backup of Data/ and volumes
4. **Secure:** Encrypt backups containing business logic

The system is designed for **reliable persistence** with **zero data loss** during normal operations. All migration work is safely stored and survives shutdowns, restarts, and container recreation.
