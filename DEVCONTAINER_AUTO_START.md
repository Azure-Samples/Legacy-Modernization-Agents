# DevContainer Auto-Start Configuration

## Overview

The devcontainer is configured to automatically start all required services and the portal after migration runs. This document explains how the auto-start mechanism works and how ports are locked to prevent changes.

## 📌 Latest Updates (Oct 29, 2025)

The portal now includes:
- ✅ **Enhanced UI**: Dark theme consistency, rich graph colors, dynamic node sizing
- ✅ **Smart Run Selection**: Only shows runs with actual graph data (e.g., runs 44, 49)
- ✅ **Intelligent Chat**: Combines SQLite metadata with Neo4j graph insights automatically
- ✅ **Example Queries**: Interactive suggestions for COBOL analysis and migration planning
- ✅ **Schema-Accurate**: Fixed SQLite queries to match actual database structure

## Auto-Start Behavior

### On Container Creation (`postCreateCommand`)

When you first create the devcontainer:

1. ✅ Restores .NET dependencies
2. ✅ Builds the project
3. ✅ Starts Neo4j container
4. ✅ Displays service URLs with **locked ports**

```
✅ Dev container ready!

🚀 Quick Start:
  ./doctor.sh run  - Run migration and auto-start portal
  ./demo.sh        - View existing migration data

📍 Fixed Ports (cannot change):
  Portal:  http://localhost:5028
  Neo4j:   http://localhost:7474
  Bolt:    bolt://localhost:7687
```

### On Container Restart (`postStartCommand`)

When you reopen or restart the container:

1. ✅ **Restarts Neo4j container** (preserves all graph data)
2. ✅ **Detects previous migration** (checks `./Data/migration.db`)
3. ✅ **Auto-starts portal** if migration exists
4. ✅ **Connects to both databases automatically**:
   - SQLite at `Data/migration.db`
   - Neo4j at `bolt://localhost:7687`
5. ✅ **Starts MCP server** (stdio child process)
6. ✅ **Portal ready** at http://localhost:5028

**Console Output:**
```
♻️  Restarting Neo4j...

📊 Previous migration detected!

🚀 Auto-starting portal...
✅ Portal running at http://localhost:5028
📊 Neo4j Browser at http://localhost:7474

💡 Use ./doctor.sh to manage services

🔗 Connected to:
  • SQLite: Data/migration.db (metadata, content, analyses)
  • Neo4j: bolt://localhost:7687 (dependency graph)
  • MCP Server: Running (stdio communication)
```

**What you can do immediately:**
- ✅ Open http://localhost:5028 - Portal with full data
- ✅ Chat with AI about any previous migration run
- ✅ View dependency graphs for all runs
- ✅ Query SQLite and Neo4j through portal
- ✅ Switch between different migration runs
- ✅ No manual configuration needed

## Locked Ports Configuration

All ports are **locked and cannot be changed** to ensure consistent access:

| Service | Port | Protocol | Purpose |
|---------|------|----------|---------|
| **Portal** | `5028` | HTTP | Migration Portal (McpChatWeb) |
| **Neo4j Browser** | `7474` | HTTP | Graph Database UI |
| **Neo4j Bolt** | `7687` | Bolt | Database Connection |

### Port Attributes (devcontainer.json)

```json
"portsAttributes": {
  "5028": {
    "label": "Migration Portal (McpChatWeb)",
    "onAutoForward": "openBrowser",
    "protocol": "http"
  },
  "7474": {
    "label": "Neo4j Browser",
    "onAutoForward": "notify",
    "protocol": "http"
  },
  "7687": {
    "label": "Neo4j Bolt Protocol",
    "onAutoForward": "silent"
  }
}
```

## How Auto-Launch Works

### 1. Initial Migration (`./doctor.sh run`)

When you run a migration for the first time:

```bash
./doctor.sh run
```

**What Happens:**
1. Validates environment (Azure OpenAI, Neo4j)
2. Runs migration process
3. **Auto-fixes common issues** (port conflicts, Neo4j status)
4. **Auto-starts portal** at port 5028
5. Opens browser automatically

**Enhanced Error Handling:**
- ❌ **Migration fails** → Doctor.sh diagnoses issues
  - Checks Neo4j container status
  - Validates Azure OpenAI configuration
  - Clears port 5028 conflicts
  - Provides fix commands
- ✅ **Migration succeeds** → Portal launches immediately

### 2. Viewing Existing Data (`./demo.sh`)

To view previous migration data:

```bash
./demo.sh
```

**What Happens:**
1. Checks if migration database exists
2. Verifies Neo4j is running
3. Starts portal on port 5028
4. Opens browser to view data

### 3. Running Portal Without COBOL Scan

You can start the portal to explore the UI **without running any COBOL migration**:

```bash
cd McpChatWeb
dotnet run --urls http://localhost:5028
```

**Use Cases:**
- Preview the portal interface
- Test the web UI
- Explore features before migration
- Development and debugging

**Note:** The chat functionality requires migration data to work, but you can see the UI layout, graph visualization (empty), and API documentation.

### 3. Container Restart (Automatic)

When you reopen VS Code:

1. DevContainer detects `./Data/migration.db`
2. Automatically starts portal
3. Portal available immediately at http://localhost:5028

## Port Locking Mechanism

### Environment Variables

All scripts enforce port 5028:

```bash
# doctor.sh
export MCP_WEB_PORT=5028
export ASPNETCORE_URLS="http://localhost:5028"
export ASPNETCORE_HTTP_PORTS="5028"

# demo.sh
portal_running() {
    lsof -ti:5028 >/dev/null 2>&1
}
```

### Port Conflict Resolution

Doctor.sh **automatically fixes** port conflicts:

```bash
# Check if port 5028 is in use
if lsof -ti:5028 >/dev/null 2>&1; then
    echo "⚠️  Port 5028 is in use, cleaning up..."
    pkill -f "dotnet.*McpChatWeb" 2>/dev/null || true
    sleep 2
fi
```

### Validation

Both scripts validate port 5028 before starting:

1. **Check port availability**
2. **Kill conflicting processes**
3. **Verify Neo4j accessibility**
4. **Start portal on exact port 5028**

## Troubleshooting

### Portal Doesn't Auto-Start

**Check if migration database exists:**
```bash
ls -lh ./Data/migration.db
```

**Manually start portal:**
```bash
./demo.sh
```

### Port 5028 Already in Use

**Doctor.sh automatically fixes this:**
```bash
./doctor.sh run  # Auto-clears port conflicts
```

**Manual fix:**
```bash
# Kill any process on 5028
pkill -f "dotnet.*McpChatWeb"

# Or find and kill specific PID
lsof -ti:5028 | xargs kill -9
```

### Neo4j Not Running

**Doctor.sh checks and starts Neo4j:**
```bash
./doctor.sh diagnose  # Checks Neo4j status
./doctor.sh run       # Auto-starts if needed
```

**Manual fix:**
```bash
docker-compose up -d neo4j
docker logs cobol-migration-neo4j
```

### Portal Logs

View portal output:
```bash
cat /tmp/portal.log  # When started by postStartCommand
```

Or run in foreground:
```bash
cd McpChatWeb
dotnet run --urls http://localhost:5028
```

## Service Management Commands

### Check Service Status

```bash
# Check portal
lsof -ti:5028 && echo "✅ Portal running" || echo "❌ Portal not running"

# Check Neo4j
docker ps | grep cobol-migration-neo4j
```

### Stop All Services

```bash
# Stop portal
pkill -f "dotnet.*McpChatWeb"

# Stop Neo4j
docker-compose down neo4j
```

### Restart Everything

```bash
./doctor.sh run  # Full restart with auto-diagnostics
```

## Configuration Files

### Key Files for Auto-Start

1. **`.devcontainer/devcontainer.json`**
   - Defines ports and auto-start behavior
   - `postCreateCommand` for initial setup
   - `postStartCommand` for portal auto-launch

2. **`doctor.sh`**
   - Main management script
   - Auto-fixes issues
   - Enforces port 5028
   - `MCP_AUTO_LAUNCH=1` by default

3. **`demo.sh`**
   - Quick view mode
   - Port 5028 validation
   - Portal startup

4. **`docker-compose.yml`**
   - Neo4j service definition
   - Port mappings (7474, 7687)

## 🌐 View Portal Without Running Migration

You can access the portal with existing data without running a new migration.

### Option 1: Use demo.sh (Recommended)

```bash
./demo.sh
```

**What it does:**
1. Checks if Neo4j is running, starts if needed
2. Checks if Data/migration.db exists
3. Starts portal at http://localhost:5028
4. Opens browser automatically

**Features Available:**
- ✅ Full chat interface with AI
- ✅ View all previous migration runs
- ✅ Interactive dependency graphs
- ✅ Query both SQLite and Neo4j
- ✅ Switch between different runs
- ✅ Example query suggestions

---

### Option 2: Manual Start (For Development)

```bash
# 1. Ensure Neo4j is running
docker-compose up -d neo4j

# 2. Verify Neo4j is accessible
curl http://localhost:7474
# Should return HTML

# 3. Start portal
cd McpChatWeb
dotnet run --urls http://localhost:5028

# 4. Open browser
open http://localhost:5028  # macOS
# xdg-open http://localhost:5028  # Linux
# start http://localhost:5028  # Windows
```

**Console Output:**
```
info: Microsoft.Hosting.Lifetime[14]
      Now listening on: http://localhost:5028
info: Microsoft.Hosting.Lifetime[0]
      Application started. Press Ctrl+C to shut down.

✅ Connected to SQLite: Data/migration.db
✅ Connected to Neo4j: bolt://localhost:7687
✅ MCP Server: Started (child process)
```

---

### Option 3: DevContainer Auto-Start (After Restart)

Simply reopen the devcontainer:

1. **Close VS Code** (or reload window)
2. **Reopen folder in container**
3. **Wait 5-10 seconds** for `postStartCommand` to complete
4. **Portal auto-starts** if `Data/migration.db` exists

**Auto-start conditions:**
- ✅ `Data/migration.db` file exists
- ✅ Neo4j container can start
- ✅ Port 5028 is available

---

### Troubleshooting Portal-Only Mode

**Problem: "No data available"**
```bash
# Check if database exists
ls -lh Data/migration.db
# Should show file size > 0 bytes

# Check if database has runs
sqlite3 Data/migration.db "SELECT COUNT(*) FROM migration_runs;"
# Should return > 0
```

**Solution:** Run a migration first:
```bash
./doctor.sh run
```

---

**Problem: "Cannot connect to Neo4j"**
```bash
# Check Neo4j status
docker ps | grep neo4j
# Should show container running

# Check Neo4j health
docker exec cobol-migration-neo4j cypher-shell -u neo4j -p cobol-migration-2025 "RETURN 1"
# Should return 1
```

**Solution:** Restart Neo4j:
```bash
docker-compose restart neo4j
sleep 5  # Wait for startup
```

---

**Problem: "Port 5028 already in use"**
```bash
# Find process using port
lsof -i:5028

# Kill old portal process
ps aux | grep "dotnet.*5028" | grep -v grep | awk '{print $2}' | xargs kill
```

**Solution:** Use demo.sh which handles this automatically:
```bash
./demo.sh
```

---

### Database Connection Details

When portal starts, it connects to:

| Database | Location | Purpose | Auto-Connect |
|----------|----------|---------|--------------|
| **SQLite** | `Data/migration.db` | Metadata, content, analyses | ✅ Yes |
| **Neo4j** | `bolt://localhost:7687` | Dependency graph | ✅ Yes |

**Credentials (Neo4j):**
- Username: `neo4j`
- Password: `cobol-migration-2025`
- Configured in: `docker-compose.yml` and `appsettings.json`

---

## Best Practices

### ✅ Do

- Use `./demo.sh` to view existing data (simplest)
- Use `./doctor.sh run` for new migrations (auto-fixes issues)
- Let devcontainer auto-start portal on restart
- Use locked ports: 5028, 7474, 7687
- Run `./doctor.sh diagnose` if issues occur
- Access Neo4j Browser at http://localhost:7474 for direct queries

### ❌ Don't

- Don't manually change ports in scripts
- Don't bypass auto-start (it ensures consistency)
- Don't run multiple portal instances simultaneously
- Don't modify `postStartCommand` without testing
- Don't delete `Data/migration.db` if you want to keep migration history

## Advanced Configuration

### Disable Auto-Launch

To prevent portal from auto-starting in devcontainer:

**Edit `.devcontainer/devcontainer.json`:**
```json
"postStartCommand": "docker compose restart neo4j && sleep 5"
```

**Or set environment variable:**
```bash
export MCP_AUTO_LAUNCH=0
./doctor.sh run  # Won't auto-start portal
```

### Custom Port (Not Recommended)

If you must use a different port:

1. Update `.devcontainer/devcontainer.json` ports
2. Update `doctor.sh` DEFAULT_MCP_PORT
3. Update `demo.sh` portal_running function
4. Update all documentation references
5. Test thoroughly

**Note:** This breaks auto-start assumptions and is not supported.

## Summary

- ✅ **Auto-start enabled** by default
- ✅ **Ports locked** to 5028, 7474, 7687
- ✅ **Doctor.sh fixes issues** automatically
- ✅ **DevContainer restarts** portal on reopen
- ✅ **No manual intervention** needed

The system is designed to "just work" - run `./doctor.sh run` once, and everything starts automatically afterward.
