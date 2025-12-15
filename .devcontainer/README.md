# Dev Container for COBOL Migration Framework

This dev container provides a fully automated development environment with all dependencies pre-configured.

## 🚀 What's Included

### Development Tools
- ✅ **.NET 9.0 SDK** - Latest .NET for running the migration framework
- ✅ **Java 17 JDK + Maven** - For Quarkus development
- ✅ **Docker-in-Docker** - Run Docker containers inside the dev container
- ✅ **Azure CLI** - Manage Azure resources
- ✅ **Node.js LTS** - For frontend development

### Databases
- ✅ **Neo4j 5.15.0** - Graph database for dependency visualization
  - Auto-starts on container creation
  - Accessible at http://localhost:7474 (browser)
  - Bolt protocol at bolt://localhost:7687
  - Credentials: `neo4j` / `cobol-migration-2025`

- ✅ **SQLite3** - For migration metadata and content storage
  - Database location: `/workspace/Data/migration.db`
  - CLI tool included for manual queries

### VS Code Extensions
- ✅ **C# Dev Kit** - .NET development support
- ✅ **Java Extension Pack** - Java and Quarkus support
- ✅ **Semantic Kernel** - AI orchestration support
- ✅ **GitHub Copilot** - AI pair programming
- ✅ **Neo4j Extension** - Query graph database from VS Code
- ✅ **SQLite Extension** - Browse SQLite databases in VS Code

### Utilities
- ✅ **cypher-shell** - Neo4j CLI for Cypher queries
- ✅ **jq** - JSON parsing in terminal
- ✅ **Helpful bash aliases** - Quick commands for common tasks

## 📋 Quick Start

### 1. Open in Dev Container

**Prerequisites:**
- Docker Desktop installed and running
- VS Code with "Dev Containers" extension

**Steps:**
```bash
# Clone the repository
git clone <repo-url>
cd Legacy-Modernization-Agents

# Open in VS Code
code .

# When prompted, click "Reopen in Container"
# Or: Command Palette → "Dev Containers: Reopen in Container"
```

### 2. Wait for Initialization (First Time Only)

The dev container will automatically:
1. Build the Docker image (~3-5 minutes first time)
2. Install .NET dependencies (`dotnet restore`)
3. Build the project (`dotnet build`)
4. Start Neo4j container
5. Show welcome message with quick commands

### 3. Verify Setup

Run the verification script:
```bash
./.devcontainer/verify-setup.sh
```

This checks:
- ✅ All system dependencies installed
- ✅ Docker containers running
- ✅ Workspace directories created
- ✅ Project files exist
- ✅ Database status
- ✅ Configuration files

### 4. Configure Azure OpenAI (Required)

```bash
# Copy the template
cp Config/ai-config.local.env.example Config/ai-config.local.env

# Edit with your credentials
nano Config/ai-config.local.env
```

Required values:
- `AZURE_OPENAI_ENDPOINT` - Your Azure OpenAI endpoint URL
- `AZURE_OPENAI_API_KEY` - Your API key
- `AZURE_OPENAI_DEPLOYMENT_NAME` - Your deployment name (e.g., "gpt-5-mini-2" or "gpt-4o")

### 5. Run Demo

```bash
./demo.sh
```

This will:
- Check prerequisites
- Ensure Neo4j is running
- Find latest migration run
- Start web portal at http://localhost:5028

## 🎯 Quick Commands

The dev container includes helpful aliases:

```bash
# Run demo with existing data
demo

# Run full COBOL migration
migration-run

# Start web portal manually
portal-start

# Check Neo4j status
neo4j-status

# Start/stop Neo4j
neo4j-start
neo4j-stop

# View Neo4j logs
neo4j-logs

# Verify setup
./.devcontainer/verify-setup.sh
```

## 🌐 Endpoints

Once running, access these URLs:

| Service | URL | Description |
|---------|-----|-------------|
| **Migration Portal** | http://localhost:5028 | Web UI with chat and graph |
| **Neo4j Browser** | http://localhost:7474 | Graph database browser |
| **Neo4j Bolt** | bolt://localhost:7687 | Direct database connection |

## 📁 Workspace Structure

```
/workspace/
├── Data/                    # SQLite database
│   └── migration.db         # Migration metadata
├── Logs/                    # Migration logs
├── source/                  # Input COBOL files (YOUR COBOL FILES GO HERE)
├── output/                  # Generated Java or C# code (unified output folder)
├── McpChatWeb/              # Web portal project
│   └── wwwroot/             # Frontend files
├── Config/                  # Configuration files
│   └── ai-config.local.env  # Azure OpenAI credentials
├── helper-scripts/
│   ├── demo.sh              # Quick start demo script
└── doctor.sh                # Full migration script
```

## 🔧 Troubleshooting

### Neo4j Not Running

```bash
# Check status
neo4j-status

# Start manually
docker-compose up -d neo4j

# Check logs for errors
neo4j-logs

# Restart if needed
docker restart cobol-migration-neo4j
```

### Portal Won't Start

```bash
# Check if port 5028 is in use
lsof -i :5028

# Kill existing process
pkill -f "dotnet.*McpChatWeb"

# Start fresh
cd McpChatWeb
dotnet run --urls "http://localhost:5028"
```

### Build Errors

```bash
# Clean and rebuild
dotnet clean
dotnet restore
dotnet build

# Check .NET version
dotnet --version
# Should be 9.0.x
```

### Database Issues

```bash
# Check database exists
ls -lh Data/migration.db

# Query database directly
sqlite3 Data/migration.db "SELECT * FROM runs;"

# If corrupted, delete and re-run migration
rm Data/migration.db
./doctor.sh run
```

## 🐳 Docker Compose Services

The dev container uses docker-compose for Neo4j:

```bash
# View all services
docker-compose ps

# View logs
docker-compose logs neo4j

# Restart service
docker-compose restart neo4j

# Stop all services
docker-compose down

# Start all services
docker-compose up -d
```

## 🔍 Verification Checklist

Run `./.devcontainer/verify-setup.sh` or manually verify:

- [ ] .NET 9.0 SDK installed: `dotnet --version`
- [ ] Java 17 installed: `java -version`
- [ ] Docker accessible: `docker ps`
- [ ] Neo4j running: `docker ps | grep neo4j`
- [ ] Neo4j healthy: `curl http://localhost:7474`
- [ ] Database exists: `ls Data/migration.db`
- [ ] AI config exists: `ls Config/ai-config.local.env`
- [ ] Scripts executable: `ls -l *.sh`

## 🪟 Windows Compatibility

The migration framework includes comprehensive Windows compatibility features:

### Cross-Platform File Writing
- ✅ **Windows MAX_PATH handling** - Automatic path shortening for 260-char limit
- ✅ **Reserved filename detection** - Handles CON, PRN, AUX, NUL, COM1-9, LPT1-9
- ✅ **File locking retry logic** - 3 retries for antivirus/indexing locks
- ✅ **UTF-8 without BOM** - Better Java compiler compatibility
- ✅ **Platform-specific line endings** - CRLF on Windows, LF on Unix/Mac
- ✅ **Invalid character sanitization** - Removes OS-specific invalid chars

### Java Output Files
When generating Java files on Windows:
```bash
# The system automatically:
# 1. Detects Windows platform
# 2. Validates path lengths (< 240 chars)
# 3. Shortens package names if needed
# 4. Falls back to flat structure if still too long
# 5. Retries file writes if locked by antivirus
# 6. Uses CRLF line endings
```

### Best Practices for Windows
1. Use short output paths: `--output "C:\migration\out"`
2. Avoid deep package hierarchies
3. Add `output` folder to antivirus exclusions
4. Monitor logs for path length warnings

**See:** `/workspace/WINDOWS_COMPATIBILITY.md` for complete documentation

## 📚 Additional Resources

- **Main README**: `/workspace/README.md`
- **Architecture Docs**: See README for system diagrams
- **Windows Compatibility**: `/workspace/WINDOWS_COMPATIBILITY.md`
- **API Documentation**: `/workspace/McpChatWeb/README.md` (if exists)
- **Change Log**: `/workspace/CHANGELOG.md`

## 🆘 Getting Help

If you encounter issues:

1. Run verification script: `./.devcontainer/verify-setup.sh`
2. Check container logs: `docker logs cobol-migration-devcontainer`
3. Rebuild container: Command Palette → "Dev Containers: Rebuild Container"
4. Check main README troubleshooting section

## 🎉 Success Criteria

Your dev container is ready when:

✅ Verification script passes all checks  
✅ Neo4j browser loads at http://localhost:7474  
✅ `./helper-scripts/demo.sh` completes without errors  
✅ Portal opens at http://localhost:5028  
✅ Graph visualization displays nodes and edges  

Happy coding! 🚀
