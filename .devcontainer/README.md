# Dev Container for COBOL Migration Framework

This dev container provides a fully automated development environment for the **COBOL to Java/C# Migration project** using **Microsoft Agent Framework**.

## 🚀 What's Included

### Development Tools
- ✅ **.NET 9.0 SDK** - Core runtime for migration framework
- ✅ **Java 17 JDK + Maven** - For Quarkus/Java targets
- ✅ **Docker-in-Docker** - Run Neo4j container inside dev container
- ✅ **Git** - Version control

### Databases
- ✅ **Neo4j 5.15.0** - Graph database for dependency visualization
  - Auto-starts on container creation
  - Browser: http://localhost:7474
  - Bolt: bolt://localhost:7687
  - Credentials: `neo4j` / `cobol-migration-2025`

- ✅ **SQLite3** - Migration metadata and content storage
  - Location: `/workspace/Data/migration.db`

### VS Code Extensions
- ✅ **C# Dev Kit** - .NET development
- ✅ **Java Extension Pack** - Java and Quarkus support
- ✅ **GitHub Copilot** - AI pair programming
- ✅ **Neo4j Extension** - Query graph database from VS Code
- ✅ **SQLite Extension** - Browse databases in VS Code
- ✅ **Mermaid Preview** - View architecture diagrams

## ⚠️ Prerequisites

### Azure OpenAI Setup

> **💡 IMPORTANT:** We recommend **1M+ TPM (tokens per minute)** quota on your Codex model deployment for smooth parallel processing with Smart Chunking.

Required model deployments:

| Model | Purpose | API Type |
|-------|---------|----------|
| `gpt-5.1-codex-mini` | Code generation (agents) | **Responses API** |
| `gpt-5.1-chat` (or `gpt-4o`) | Chat (portal) | Chat Completions API |

> **Note:** The Codex model MUST support the Responses API for code generation agents. The chat model uses standard Chat Completions API.

## 📋 Quick Start

### 1. Open in Dev Container

**Prerequisites:**
- Docker Desktop installed and running
- VS Code with "Dev Containers" extension

\`\`\`bash
# Clone and open
git clone <repo-url>
cd Legacy-Modernization-Agents
code .

# When prompted: "Reopen in Container"
# Or: Command Palette → "Dev Containers: Reopen in Container"
\`\`\`

### 2. Wait for Initialization (First Time ~3-5 min)

The dev container automatically:
1. Builds the Docker image
2. Installs dependencies (\`dotnet restore\`)
3. Builds the project
4. Starts Neo4j container

### 3. Configure Azure OpenAI (Required)

**Option A: Edit appsettings.json directly (Recommended)**

Edit \`Config/appsettings.json\` and replace:
- \`YOUR-RESOURCE\` → Your Azure OpenAI resource name
- \`YOUR-API-KEY-HERE\` → Your actual API key

**Option B: Use local override file (for secrets)**

\`\`\`bash
# Create local config (gitignored)
cat > Config/ai-config.local.env << 'EOF'
# Azure OpenAI Configuration
AZURE_OPENAI_CODEX_ENDPOINT=https://YOUR-RESOURCE.openai.azure.com
AZURE_OPENAI_CODEX_API_KEY=your-actual-key
AZURE_OPENAI_CODEX_DEPLOYMENT=gpt-5-mini-2

AZURE_OPENAI_CHAT_ENDPOINT=https://YOUR-RESOURCE.openai.azure.com
AZURE_OPENAI_CHAT_API_KEY=your-actual-key
AZURE_OPENAI_CHAT_DEPLOYMENT=gpt-4o
EOF
\`\`\`

> \`ai-config.local.env\` takes precedence over \`appsettings.json\` when loaded by \`doctor.sh\`

### 4. Verify Setup

\`\`\`bash
./.devcontainer/verify-setup.sh
\`\`\`

### 5. Run!

\`\`\`bash
./doctor.sh run
\`\`\`

## 🎯 Quick Commands

\`\`\`bash
# Full migration (interactive menu)
./doctor.sh run

# Launch portal only
./doctor.sh portal

# Business logic extraction only
./doctor.sh reverse-eng

# Demo with sample COBOL
demo

# Check setup
verify-setup
\`\`\`

## 🌐 Endpoints

| Service | URL | Description |
|---------|-----|-------------|
| **Migration Portal** | http://localhost:5028 | Web UI with chat and graph |
| **Neo4j Browser** | http://localhost:7474 | Graph database browser |
| **Neo4j Bolt** | bolt://localhost:7687 | Direct database connection |

## 📁 Workspace Structure

\`\`\`
/workspace/
├── Config/
│   ├── appsettings.json      # Main config (edit API keys here)
│   └── ai-config.local.env   # Optional: override with real keys (gitignored)
├── Data/
│   └── migration.db          # SQLite database
├── Logs/                     # Migration logs
├── source/                   # YOUR COBOL FILES GO HERE
├── output/                   # Generated Java or C# code
├── McpChatWeb/               # Web portal project
└── doctor.sh                 # Main entry point
\`\`\`

## 🔧 Troubleshooting

### Neo4j Not Running

\`\`\`bash
# Check status
docker ps | grep neo4j

# Start manually
docker-compose up -d neo4j

# Check logs
docker logs cobol-migration-neo4j
\`\`\`

### Portal Won't Start

\`\`\`bash
# Check if port 5028 is in use
lsof -i :5028

# Kill existing process
pkill -f "dotnet.*McpChatWeb"

# Start fresh
./doctor.sh portal
\`\`\`

### Build Errors

\`\`\`bash
# Clean and rebuild
dotnet clean
dotnet restore
dotnet build
\`\`\`

### Configuration Issues

\`\`\`bash
# Verify API key is set
grep -i "api-key\|endpoint" Config/appsettings.json

# Check if local override exists
ls -la Config/ai-config.local.env

# Test connection (will fail if keys wrong)
./doctor.sh portal
\`\`\`

## 🎉 Success Criteria

Your dev container is ready when:

✅ Verification script passes all checks  
✅ Neo4j browser loads at http://localhost:7474  
✅ \`demo\` command completes without errors  
✅ Portal opens at http://localhost:5028  
✅ Graph visualization displays nodes and edges  

## 📚 Additional Resources

- **Main README**: \`/workspace/README.md\` - Complete documentation
- **Architecture**: See main README for system diagrams
- **Change Log**: \`/workspace/CHANGELOG.md\`

Happy migrating! 🚀
