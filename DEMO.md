# Demo Script for COBOL Migration Portal

This script launches the portal and Neo4j for demonstration purposes **without** running a new migration.

## How to Use

### Option 1: Automatic Launch (Recommended)

```bash
./demo.sh
```

**What it does:**
1. Checks if `Data/migration.db` exists
2. Verifies Neo4j is running
3. Starts portal on port 5028
4. Opens browser to http://localhost:5028

### Option 2: Portal Preview (No Migration Data)

View the portal interface **without running any COBOL scan**:

```bash
cd McpChatWeb
dotnet run --urls http://localhost:5028
```

**When to use:**
- First-time exploration of the UI
- Testing portal features
- Previewing before migration
- Development work

**Limitations:** Graph and chat features require migration data.

### Option 3: Manual Launch

## Features Available in Demo Mode

### Portal (http://localhost:5028)
- 💬 **Chat Interface** - Ask questions about COBOL code
- 🎯 **Suggestion Chips** - 6 pre-configured queries:
  - Circular dependencies
  - Critical files
  - Impact analysis
  - Copybook usage
  - Dependency summary
  - Main programs
- 📊 **Dependency Graph** - Interactive vis-network visualization
- 📋 **Resource Browser** - View all available MCP resources

### Neo4j Browser (http://localhost:7474)
- 🔍 **Graph Visualization** - Full Neo4j Browser experience
- 📈 **Query Interface** - Run custom Cypher queries
- 🔗 **Relationship Explorer** - Trace dependencies visually

**Credentials:**
- Username: `neo4j`
- Password: `cobol-migration-2025`

## Stopping the Demo

Press `Ctrl+C` to stop the portal.

To stop Neo4j:
```bash
docker-compose down
```

## Troubleshooting

### Port Already in Use
If you see "Address already in use" error for the portal:
```bash
# Kill existing process
pkill -f "dotnet.*McpChatWeb"
# Then run demo.sh again
./demo.sh
```

### Neo4j Port Conflict
**Automatic Handling:** The script detects ANY Neo4j container on ports 7474/7687 and uses it automatically.

If you see Neo4j port conflicts:
```bash
# Check which containers are using Neo4j ports
docker ps -a | grep neo4j

# Option 1: Use the existing Neo4j (demo.sh does this automatically)
./demo.sh

# Option 2: Stop all Neo4j containers and start fresh
docker stop $(docker ps -q --filter "ancestor=neo4j")
docker-compose up -d neo4j
```

**Note:** `demo.sh` works with any Neo4j container on the correct ports - it doesn't require a specific container name.

### Neo4j Not Starting
```bash
# Check Docker is running
docker ps

# Restart Neo4j
docker-compose restart neo4j
```

### No Data Showing
The portal displays data from the latest successful migration run. If no data appears:
- Check `Data/migration.db` exists
- Verify Neo4j contains data: http://localhost:7474
- Run a full migration with `./doctor.sh run` to populate data

## Example Demo Flow

1. **Start the demo:**
   ```bash
   ./demo.sh
   ```

2. **Open portal:** http://localhost:5028

3. **Try a suggestion chip:**
   - Click "🔄 Circular Dependencies"
   - See the AI analyze the codebase

4. **View the graph:**
   - Look at the right panel
   - See nodes and edges representing COBOL files
   - Click nodes for details

5. **Ask custom questions:**
   - "Which files depend on BDSMFJLI.cpy?"
   - "Show me all copybooks used by BDSMFJL.cbl"
   - "What are the most complex programs?"

6. **Explore Neo4j:**
   - Open http://localhost:7474
   - Login with credentials above
   - Run: `MATCH (f:CobolFile) RETURN f LIMIT 25`

## Data Sources

The demo uses existing data from:
- **SQLite:** `Data/migration.db` (Run metadata, analyses)
- **Neo4j:** Graph database (Dependencies, relationships)

**No new analysis is performed** - perfect for demos and presentations!

## Quick Reference

| Service | URL | Purpose |
|---------|-----|---------|
| Portal | http://localhost:5028 | Main UI with chat and graph |
| Neo4j Browser | http://localhost:7474 | Raw graph database access |
| Neo4j Bolt | bolt://localhost:7687 | Graph query endpoint |

## See Also

- `doctor.sh run` - Run full migration (analysis + conversion)
- `QUERY_GUIDE.md` - 50+ example queries
- `MIGRATION_FIXES.md` - Recent improvements
