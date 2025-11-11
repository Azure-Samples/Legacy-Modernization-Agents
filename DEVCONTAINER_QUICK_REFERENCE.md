# Dev Container Setup - Quick Reference

**Last Updated:** October 23, 2025  
**Status:** ✅ Fully Automated

## 🚀 30-Second Quick Start

```bash
# 1. Open in VS Code
code .

# 2. Click "Reopen in Container" (or F1 → Dev Containers: Reopen in Container)

# 3. Wait 3-5 minutes for first-time setup

# 4. Configure Azure OpenAI credentials
cp Config/ai-config.local.env.example Config/ai-config.local.env
nano Config/ai-config.local.env  # Add your credentials

# 5. Run demo
./demo.sh

# 6. Open portal
open http://localhost:5028
```

## ✅ What's Automated

| Component | Status | Details |
|-----------|--------|---------|
| **.NET 9.0 SDK** | ✅ Auto | Installed and configured |
| **Java 17 + Maven** | ✅ Auto | For Quarkus development |
| **Neo4j 5.15.0** | ✅ Auto | Starts on container creation |
| **Neo4j Persistence** | ✅ Auto | Restarts when container reopens |
| **Docker Compose** | ✅ Auto | For Neo4j container management |
| **Project Build** | ✅ Auto | `dotnet restore && build` runs automatically |
| **VS Code Extensions** | ✅ Auto | C#, Java, Neo4j, SQLite, Copilot installed |
| **Port Forwarding** | ✅ Auto | 5028 (portal), 7474 (Neo4j), 7687 (Bolt) |
| **Bash Aliases** | ✅ Auto | demo, migration-run, neo4j-status, etc. |
| **Workspace Setup** | ✅ Auto | All directories created automatically |

## ⚠️ Manual Steps

Only **ONE** manual step required:

### Azure OpenAI Configuration (2 minutes)
```bash
cp Config/ai-config.local.env.example Config/ai-config.local.env
nano Config/ai-config.local.env
```

Add your credentials:
- `AZURE_OPENAI_ENDPOINT` - Your endpoint URL
- `AZURE_OPENAI_API_KEY` - Your API key
- `AZURE_OPENAI_DEPLOYMENT_NAME` - Must be "gpt-4.1"

## 🎯 Verification

```bash
# Quick check
verify-setup

# Expected output:
# ✅ .NET SDK installed (9.0.x)
# ✅ Java installed (17.x)
# ✅ Docker accessible
# ✅ Neo4j container running
# ✅ Neo4j healthy
# ✅ Workspace structure verified
# ⚠️  Configure AI credentials to run migrations
```

## 🌐 Endpoints

| Service | URL | Credentials |
|---------|-----|-------------|
| **Migration Portal** | http://localhost:5028 | None |
| **Neo4j Browser** | http://localhost:7474 | neo4j / cobol-migration-2025 |
| **Neo4j Bolt** | bolt://localhost:7687 | neo4j / cobol-migration-2025 |

## 🛠️ Helpful Commands

| Command | What It Does |
|---------|--------------|
| `demo` | Start demo with existing data |
| `migration-run` | Run full COBOL migration |
| `portal-start` | Start web portal manually |
| `neo4j-status` | Check Neo4j container status |
| `neo4j-start` | Start Neo4j container |
| `neo4j-stop` | Stop Neo4j container |
| `neo4j-logs` | View Neo4j logs |
| `verify-setup` | Check all components |

## 🔧 Troubleshooting

### Neo4j Not Running
```bash
neo4j-start
# or
docker-compose up -d neo4j
```

### Portal Won't Start
```bash
pkill -f "dotnet.*McpChatWeb"
demo
```

### Rebuild Container
```
F1 → "Dev Containers: Rebuild Container"
```

## 🪟 Windows Compatibility

The framework includes comprehensive Windows support:

- ✅ **MAX_PATH handling** - Automatic path shortening for 260-char limit
- ✅ **Reserved names** - Detects CON, PRN, AUX, NUL, COM1-9, LPT1-9
- ✅ **File locking retry** - Handles antivirus/Windows Defender locks
- ✅ **Line endings** - CRLF on Windows, LF on Unix/Mac
- ✅ **UTF-8 without BOM** - Better Java compatibility

**Best Practices:**
- Use short paths: `--java-output "C:\out"`
- Add `java-output` to antivirus exclusions
- Monitor logs for path warnings

**See:** WINDOWS_COMPATIBILITY.md for details

## 📚 Documentation

- **Complete Setup Guide:** README.md (Dev Container Setup section)
- **Dev Container Details:** .devcontainer/README.md
- **Automation Checklist:** DEVCONTAINER_AUTOMATION_CHECKLIST.md
- **Windows Compatibility:** WINDOWS_COMPATIBILITY.md
- **Change Log:** CHANGELOG.md

## 🎉 Success Criteria

Your dev container is ready when:

✅ `verify-setup` passes all checks  
✅ Neo4j browser loads at http://localhost:7474  
✅ `./demo.sh` runs without errors  
✅ Portal opens at http://localhost:5028  
✅ Graph visualization shows nodes and edges  

---

**Total Setup Time:** 5 minutes (3 min automated + 2 min credentials)  
**Automation Level:** 95% (only Azure credentials manual)
