# 🚀 Quick Start Guide

Get up and running with the COBOL Migration Portal in 5 minutes!

## 🎯 Prerequisites Checklist

- [ ] Docker Desktop installed and running
- [ ] .NET 9.0 SDK installed (`dotnet --version` shows 9.0.x)
- [ ] Azure OpenAI credentials ready (endpoint + API key)
- [ ] 8GB RAM minimum, 16GB recommended
- [ ] Modern browser (Chrome, Edge, Firefox, Safari)

## ⚡ Fast Track Setup (Dev Container)

**Best for: Team collaboration, consistent environment**

```bash
# 1. Clone repository
git clone https://github.com/microsoft/Legacy-Modernization-Agents.git
cd Legacy-Modernization-Agents

# 2. Open in VS Code
code .

# 3. When prompted, click "Reopen in Container"
# Wait 3-5 minutes for first-time setup

# 4. Configure Azure OpenAI (in container terminal)
cp Config/ai-config.local.env.example Config/ai-config.local.env
nano Config/ai-config.local.env
# Edit: AZURE_OPENAI_ENDPOINT, AZURE_OPENAI_API_KEY

# 5. Run demo
./demo.sh
```

✅ **Done!** Portal opens at http://localhost:5250

---

## 🔧 Manual Setup (Local Development)

**Best for: Experienced developers, custom configurations**

### Step 1: Install Prerequisites

```bash
# Check .NET version
dotnet --version
# Should show: 9.0.x

# If not, install from: https://dotnet.microsoft.com/download/dotnet/9.0

# Check Docker
docker --version
docker ps
```

### Step 2: Start Neo4j

```bash
# Option A: Docker Compose (recommended)
docker-compose up -d neo4j

# Option B: Manual Docker
docker run -d \
  --name cobol-migration-neo4j \
  -p 7474:7474 -p 7687:7687 \
  -e NEO4J_AUTH=neo4j/cobol-migration-2025 \
  neo4j:5.15.0

# Verify
docker ps | grep neo4j
# Wait 30 seconds for Neo4j to be ready
```

### Step 3: Configure Azure OpenAI

```bash
# Copy template
cp Config/ai-config.local.env.example Config/ai-config.local.env

# Edit with your credentials
nano Config/ai-config.local.env
```

**Required values:**
```bash
AZURE_OPENAI_ENDPOINT="https://YOUR-RESOURCE.openai.azure.com/"
AZURE_OPENAI_API_KEY="your-32-character-key-here"
AZURE_OPENAI_DEPLOYMENT_NAME="gpt-4.1"
```

**Find your values:**
- Endpoint: Azure Portal → OpenAI Resource → Keys and Endpoint → Endpoint
- API Key: Azure Portal → OpenAI Resource → Keys and Endpoint → Key 1
- Deployment: Must be named "gpt-4.1" in your Azure OpenAI instance

### Step 4: Build Project

```bash
# Restore dependencies
dotnet restore

# Build solution
dotnet build

# Verify zero warnings
# Output should show: "0 Warning(s), 0 Error(s)"
```

### Step 5: Run Demo

```bash
# One command to start everything
./demo.sh

# Or manual steps:
# 1. Ensure Neo4j is running
# 2. cd McpChatWeb
# 3. dotnet run
# 4. Open http://localhost:5250
```

---

## 🎬 First Run Experience

After running `./demo.sh`, you'll see:

```
╔══════════════════════════════════════════════════════════════╗
║   COBOL Migration Portal - Demo Mode                        ║
╚══════════════════════════════════════════════════════════════╝

🔍 Checking prerequisites...
✅ Docker installed
✅ .NET 9.0 SDK found
✅ Neo4j accessible

📊 Starting Neo4j...
✅ Neo4j is running

💾 Checking database...
✅ Database found with Run 43

🚀 Starting web portal...
⏳ Waiting for portal to start ✅

🌐 Access your demo:
   Portal:        http://localhost:5250
   Neo4j Browser: http://localhost:7474

📊 Portal Features:
   • Three-panel dashboard
   • AI-powered chat
   • Interactive graph
   • Multi-run support
   • File analysis
```

**Browser automatically opens** to http://localhost:5250

---

## 🧪 Test Your Setup

### 1. Verify Portal Loads

**Expected:**
- ✅ Three panels visible (resources | chat | graph)
- ✅ Graph shows "Dependency Graph | Run 43"
- ✅ Resources list shows 8 MCP resources
- ✅ Suggestion chips visible in chat

**If not working:**
```bash
# Check portal logs
# Look for "Now listening on: http://localhost:5250"
```

### 2. Test Chat

**Try this query:**
```
"Hello, what can you help me with?"
```

**Expected response:**
- ✅ AI responds with migration capabilities
- ✅ Response appears within 3-5 seconds
- ✅ No error messages

**If error:**
```bash
# Check Azure OpenAI config
./doctor.sh doctor
```

### 3. Test File Analysis

**Try this query:**
```
"What functions are in BDSDA23.cbl?"
```

**Expected response:**
- ✅ Program purpose description
- ✅ List of functions/paragraphs
- ✅ Variables with PIC clauses
- ✅ Copybooks referenced
- ✅ Data source URI shown

### 4. Test Multi-Run

**Try this query:**
```
"Show me run 42"
```

**Expected:**
- ✅ Response shows Run 42 data
- ✅ Graph updates automatically
- ✅ Graph header shows "Run 42"
- ✅ Response includes SQLite and Neo4j data

### 5. Test Graph

**Actions:**
- Click **"Full Graph"** filter
- Zoom in with mouse wheel
- Click on a blue node (program)
- Click on a red node (copybook)
- Hover over edges (relationships)

**Expected:**
- ✅ Graph displays 49 nodes + 64 edges
- ✅ Smooth zooming and panning
- ✅ Tooltips appear on hover
- ✅ Node selection highlights

---

## 🎓 First Tasks to Try

### Task 1: Explore Dependencies (2 minutes)

```
1. Click "🔄 Circular Dependencies" chip
2. Read AI response about cycles
3. Click "Programs Only" filter
4. Observe only blue nodes shown
5. Click "Full Graph" to restore
```

### Task 2: Analyze a File (3 minutes)

```
1. Type: "What does RENI033.cpy contain?"
2. Read the analysis response
3. Note the variables and their PIC clauses
4. Click on the file node in graph
5. Explore relationships visually
```

### Task 3: Compare Runs (3 minutes)

```
1. Type: "Show me run 43"
2. Note the statistics (102 files, etc.)
3. Type: "Now show me run 42"
4. Watch graph update automatically
5. Compare the two runs
```

### Task 4: Access Raw Data (5 minutes)

```
1. Click "📖 Data Retrieval Guide"
2. Copy SQLite query example
3. Open terminal
4. Run: sqlite3 Data/migration.db
5. Paste query and explore
```

---

## 📚 Next Steps

Now that you're set up, explore these areas:

1. **Run a Full Migration:**
   ```bash
   # Place COBOL files in source/
   ./doctor.sh run
   ```

2. **Read the Architecture:**
   - See [Complete Architecture](README.md#-complete-architecture) section
   - Understand SQLite + Neo4j hybrid approach
   - Learn about Semantic Kernel orchestration

3. **Customize Agents:**
   - Edit agent prompts in `Agents/` folder
   - Adjust for your specific COBOL dialect
   - Change output format (Java, .NET, documentation)

4. **Integrate with Tools:**
   - Claude Desktop for MCP access
   - Cursor IDE for code editing
   - Neo4j Browser for graph queries

5. **Explore Advanced Features:**
   - Multi-run comparisons
   - Impact analysis workflows
   - Custom Cypher queries
   - API integration

---

## ❓ Common Issues

### Issue: "NETSDK1045: The current .NET SDK does not support targeting .NET 9.0"

**Solution:**
```bash
# Install .NET 9.0 SDK
https://dotnet.microsoft.com/download/dotnet/9.0

# Verify
dotnet --version
# Should show: 9.0.x
```

### Issue: "Neo4j connection refused"

**Solution:**
```bash
# Check if running
docker ps | grep neo4j

# If not running, start it
docker-compose up -d neo4j

# Wait 30 seconds, then verify
curl http://localhost:7474
```

### Issue: "Azure OpenAI API error"

**Solution:**
```bash
# Check configuration
./doctor.sh doctor

# Verify endpoint ends with /
# Verify API key is 32 characters
# Verify deployment name is "gpt-4.1"

# Test connection
./doctor.sh test
```

### Issue: "Portal shows no data"

**Solution:**
```bash
# Check database exists
ls -lh Data/migration.db

# If missing, run a migration first
./doctor.sh run

# Or use demo mode (uses existing data)
./demo.sh
```

### Issue: "Graph not displaying"

**Solution:**
1. Open browser DevTools (F12)
2. Check Console tab for JavaScript errors
3. Verify `/api/graph` endpoint returns data:
   ```bash
   curl http://localhost:5250/api/graph | jq '.nodes | length'
   # Should show: 49
   ```
4. Hard refresh browser (Ctrl+Shift+R or Cmd+Shift+R)

---

## 🆘 Get Help

- **Documentation**: Read full [README.md](README.md)
- **Architecture**: See [SEMANTIC_KERNEL_ARCHITECTURE.md](SEMANTIC_KERNEL_ARCHITECTURE.md)
- **Changes**: Check [CHANGELOG.md](CHANGELOG.md)
- **Issues**: Open GitHub issue with logs
- **Diagnostics**: Run `./doctor.sh doctor` and share output

---

## 🎉 Success Criteria

You're all set when:

- ✅ Portal loads at http://localhost:5250
- ✅ Chat responds to queries
- ✅ Graph displays nodes and edges
- ✅ File analysis returns detailed data
- ✅ Multi-run queries work
- ✅ Zero build warnings
- ✅ Neo4j accessible at http://localhost:7474

**Time to start migrating COBOL! 🚀**
