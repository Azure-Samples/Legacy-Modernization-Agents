# Running Portal Without COBOL Migration

## Overview

You can start the portal to explore the user interface **without running any COBOL migration**. This is useful for:

- 👀 **First-time preview** - See what the portal looks like before migration
- 🧪 **Testing** - Test portal functionality and features
- 🛠️ **Development** - Work on UI/UX improvements
- 📚 **Demo** - Show the interface to stakeholders
- 📖 **Documentation** - Explore API documentation

## Quick Start

### 1. Start Portal Only

```bash
cd McpChatWeb
dotnet run --urls http://localhost:5028
```

**Expected output:**
```
Building...
info: Microsoft.Hosting.Lifetime[14]
      Now listening on: http://localhost:5028
info: Microsoft.Hosting.Lifetime[0]
      Application started. Press Ctrl+C to shut down.
```

### 2. Open Browser

Navigate to: **http://localhost:5028**

## What You'll See

### Portal Interface (Empty State)

- ✅ **Three-panel layout** - Chat, Resources, Graph
- ✅ **API Documentation** - Complete MCP API reference
- ✅ **Empty graph visualization** - Ready for data
- ✅ **Chat interface** - UI visible but no data to query
- ✅ **Resources list** - Shows "No migration data yet"

### Features Available Without Data

| Feature | Available | Notes |
|---------|-----------|-------|
| UI Layout | ✅ Yes | Full three-panel design |
| API Docs | ✅ Yes | Complete documentation modal |
| Graph Canvas | ✅ Yes | Empty visualization ready |
| Chat Input | ✅ Yes | UI functional, no data to query |
| Run Selector | ⚠️ Limited | Dropdown shows but no runs available |
| Resource List | ⚠️ Limited | Shows empty state |

### Features Requiring Migration Data

| Feature | Requires Data | Alternative |
|---------|---------------|-------------|
| Dependency Graph | ❌ Yes | Run `./doctor.sh run` first |
| Chat Responses | ❌ Yes | Run `./demo.sh` to see with data |
| File Analysis | ❌ Yes | Need COBOL files analyzed |
| Run Switching | ❌ Yes | Need multiple migration runs |

## When to Use Each Command

### Preview Only (No Data)
```bash
cd McpChatWeb
dotnet run --urls http://localhost:5028
```
**Use when:** You want to see the UI without any migration

### View Existing Data
```bash
./demo.sh
```
**Use when:** You want to see portal with previous migration results

### Run New Migration
```bash
./doctor.sh run
```
**Use when:** You want to analyze COBOL files and populate the portal

## Stopping the Portal

Press `Ctrl+C` in the terminal where you ran `dotnet run`, or:

```bash
# Find and kill portal process
pkill -f "dotnet.*McpChatWeb"

# Or find PID and kill
ps aux | grep "dotnet.*McpChatWeb" | grep -v grep
kill <PID>
```

## Next Steps

After previewing the portal, you have two options:

### Option 1: View Sample Data (Quick)
```bash
./demo.sh
```
- Uses existing migration database (`Data/migration.db`)
- Shows pre-analyzed COBOL files
- Instant visualization
- Perfect for demos

### Option 2: Run Your Own Migration (Full)
```bash
./doctor.sh run
```
- Analyzes your COBOL files
- Creates new migration run
- Populates Neo4j graph database
- Auto-starts portal when complete

## Troubleshooting

### Port 5028 Already in Use

```bash
# Check what's using the port
lsof -ti:5028

# Kill the process
kill $(lsof -ti:5028)

# Or use doctor.sh which auto-fixes this
./doctor.sh run
```

### .NET Build Errors

```bash
# Restore dependencies
dotnet restore

# Clean and rebuild
dotnet clean
dotnet build

# Then try again
cd McpChatWeb
dotnet run --urls http://localhost:5028
```

### Portal Won't Start

```bash
# Check .NET version (needs 9.0)
dotnet --version

# Should show: 9.0.x
# If not, install from: https://dotnet.microsoft.com/download/dotnet/9.0
```

## Portal Features Overview

Even without data, you can explore:

### 1. UI Layout
- **Left Panel:** Chat interface with input
- **Middle Panel:** Resources list and info
- **Right Panel:** Graph visualization canvas

### 2. API Documentation
Click the **"Data Retrieval Guide"** button to see:
- All available MCP API endpoints
- Request/response examples
- curl command samples
- Query patterns

### 3. Graph Controls
- Layout toggle (Force Directed ↔ Hierarchical)
- Zoom controls
- Full-screen mode
- Physics simulation toggle

### 4. Responsive Design
- Resize panels by dragging dividers
- Mobile-friendly interface
- Dark theme with COBOL green accents

## Comparison with Other Commands

| Command | Data Needed | Neo4j Required | Use Case |
|---------|-------------|----------------|----------|
| `cd McpChatWeb && dotnet run` | ❌ No | ❌ No | Preview UI only |
| `./demo.sh` | ✅ Yes (existing DB) | ✅ Yes | View previous results |
| `./doctor.sh run` | ❌ No (creates new) | ✅ Yes | Full migration |

## Summary

**To preview the portal interface without any migration:**

```bash
cd McpChatWeb
dotnet run --urls http://localhost:5028
```

Then open **http://localhost:5028** in your browser.

**To see the portal with actual data:**

```bash
./demo.sh  # View existing data
# or
./doctor.sh run  # Run new migration
```

The portal is designed to be explored even without data, so you can understand the interface before committing to a full COBOL migration run.
