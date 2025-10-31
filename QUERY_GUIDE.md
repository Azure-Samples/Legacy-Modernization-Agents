# 🎯 COBOL Migration Portal - Query Guide

## 🚀 Getting Started

Your COBOL migration portal is now running with **full graph database integration**! Here's everything you can explore.

### ✅ What's Working

- **Three-Panel UI**: Resources (left), Chat (center), Graph Visualization (right)
- **Multi-Stage Loading**: See exactly what's happening (Database → Azure OpenAI → Response)
- **Graph Visualization**: Interactive D3.js force-directed graph with zoom/pan
- **Neo4j Integration**: Advanced dependency analysis powered by graph queries
- **8 MCP Resources**: Including 4 new graph-powered insights

---

## 💡 Suggested Prompts to Try

### 🔄 **Circular Dependencies**
Circular dependencies can cause compilation issues and make code harder to maintain.

**Try asking:**
- "What are the circular dependencies in this codebase?"
- "Show me any dependency cycles"
- "Are there any files that reference each other circularly?"
- "Which copybooks create circular dependencies?"

**What you'll learn:**
- Files involved in circular reference chains
- Length of dependency cycles (2-10 levels deep)
- Specific paths through the circular references

---

### ⭐ **Critical Files Analysis**
Some files are more connected than others - changing them affects many files.

**Try asking:**
- "Which files are most critical based on their connections?"
- "Show me the top 5 most connected files"
- "What files have the most dependencies?"
- "Which copybooks are used by the most programs?"

**What you'll learn:**
- Files with highest incoming dependencies (most depended upon)
- Files with highest outgoing dependencies (depend on most others)
- Total connection counts for risk assessment
- Files that should be tested most thoroughly when changed

---

### 📊 **Impact Analysis**
Before modifying a file, understand what else might break.

**Try asking:**
- "Show me the impact analysis for BDSMFJL.cbl"
- "What files would be affected if I change BDSM043.cbl?"
- "If I modify CPYBOOK1.cpy, what breaks?"
- "Show me the downstream impact of BDSMFJL.cbl"

**What you'll learn:**
- **Downstream files**: What depends on this file (what will break)
- **Upstream dependencies**: What this file depends on (what it needs)
- **Distance levels**: How many hops away are affected files
- **Total impact**: Complete ripple effect count

---

### 📚 **Copybook Usage Analysis**
Understanding copybook patterns helps with refactoring and migration planning.

**Try asking:**
- "What copybooks are used most frequently?"
- "Which programs use the most copybooks?"
- "Show me the copybook dependency structure"
- "What programs depend on CPYBOOK1.cpy?"

**What you'll learn:**
- Copybook usage frequency
- Programs using each copybook
- Reverse dependencies (what uses what)
- Reusability patterns

---

### 🗂️ **Dependency Structure Overview**
Get a high-level understanding of your codebase architecture.

**Try asking:**
- "Summarize the dependency structure"
- "Give me an overview of the migration"
- "What's the overall structure of dependencies?"
- "How complex is this codebase?"

**What you'll learn:**
- Total programs and copybooks
- Total number of dependencies
- Average dependencies per program
- Most used copybooks
- Complexity metrics

---

### 🎯 **Main Programs & File Discovery**
Identify entry points and understand what you're working with.

**Try asking:**
- "What are the main programs in this migration?"
- "List all COBOL programs"
- "What files were discovered?"
- "Show me the analyzed programs"

**What you'll learn:**
- All discovered COBOL files
- Program vs copybook classification
- File descriptions and purposes
- Program types and characteristics

---

### 📈 **Mermaid Diagram Requests**
Visual representations help communicate architecture.

**Try asking:**
- "Generate a dependency diagram"
- "Show me the Mermaid diagram"
- "Create a visual representation of dependencies"
- "Draw the dependency graph"

**What you'll learn:**
- Visual flowchart of dependencies
- Mermaid diagram code (can be rendered elsewhere)
- Clear visualization of relationships

---

### 🔍 **Specific File Analysis**
Deep dive into individual files.

**Try asking:**
- "Tell me about BDSMFJL.cbl"
- "Analyze BDSM043.cbl"
- "What does CPYBOOK1.cpy contain?"
- "Show me the analysis for [filename]"

**What you'll learn:**
- Program description and purpose
- Paragraphs and procedures
- Data structures
- Key functionality
- Complexity assessment

---

## 🎨 **Graph Visualization Features**

The right panel shows an **interactive dependency graph**:

### Controls
- **🔍+** Zoom in
- **🔍−** Zoom out
- **↺** Reset view
- **🔄** Refresh graph

### Visual Legend
- **Blue nodes (●)**: COBOL programs
- **Red nodes (●)**: Copybooks
- **Arrows (→)**: Dependencies (direction shows "depends on")

### Interactions
- **Hover**: Highlight connected files
- **Drag**: Move nodes around
- **Scroll**: Zoom in/out
- **Click & drag background**: Pan the view

---

## 🛠️ **Advanced Queries**

### Combining Concepts
- "What are the critical files AND do any have circular dependencies?"
- "Show me impact analysis for the most connected file"
- "List circular dependencies and their impact analysis"

### Migration Planning
- "What order should I migrate these files?"
- "Which files should I test first?"
- "What's the risk of changing BDSMFJL.cbl?"

### Code Quality
- "Are there any code smells in the dependency structure?"
- "What files have the highest complexity?"
- "Which files would benefit from refactoring?"

---

## 📋 **Available MCP Resources**

The left panel shows 8 available resources:

1. **Summary** - High-level metrics for the migration run
2. **Dependencies** - Dependency map and Mermaid diagram
3. **Analyses** - Index of analyzed COBOL programs
4. **Analysis [file]** - Detailed analysis for specific file
5. **Graph** 🆕 - Dependency graph visualization data (Neo4j)
6. **Circular Dependencies** 🆕 - Detected cycles in codebase
7. **Critical Files** 🆕 - Files with highest connections
8. **Impact [file]** 🆕 - Impact analysis for specific file

---

## 🌟 **Pro Tips**

### For Best Results
1. **Be specific**: "Show me impact for BDSMFJL.cbl" works better than "tell me about impacts"
2. **Ask follow-ups**: After getting circular dependencies, ask "What's the impact of breaking these cycles?"
3. **Use file names**: The AI knows all your COBOL files by name
4. **Explore connections**: Ask about files mentioned in previous responses

### Understanding the Data
- **Run 34**: Currently loaded migration run
- **5 COBOL files**: 2 programs + 3 copybooks
- **12 dependencies**: Tracked relationships
- **Neo4j powered**: Graph queries run in milliseconds

### Troubleshooting
- If graph doesn't load: Click the **🔄 Refresh** button
- If query takes long: Check the loading stages for progress
- If response is generic: Be more specific with file names

---

## 🎓 **Learning Exercises**

Try these sequences to get comfortable:

### Exercise 1: Full Impact Assessment
1. "What are the critical files?"
2. "Show me impact analysis for [top file from step 1]"
3. "Are there circular dependencies involving [that file]?"

### Exercise 2: Migration Planning
1. "Summarize the dependency structure"
2. "Which copybooks are used most frequently?"
3. "Show me impact analysis for [most-used copybook]"
4. "What programs depend on that copybook?"

### Exercise 3: Risk Analysis
1. "What are the circular dependencies?"
2. "For each file in that cycle, show me the critical files"
3. "What's the total impact if we break this cycle?"

---

## 📞 **Need Help?**

- **Can't find a file?** Try: "List all programs"
- **Graph not showing?** Refresh the page and check Neo4j is running
- **Query too slow?** Start with simpler questions and build up
- **Unclear results?** Ask for clarification or more details

---

## 🔗 **External Resources**

- **Neo4j Browser**: http://localhost:7474 (neo4j / cobol-migration-2025)
- **Portal**: http://localhost:5028
- **GitHub Copilot**: Ask me anything about this data!

---

## 🎉 **Enjoy Exploring!**

Your COBOL migration data is now fully queryable with AI-powered insights and graph database capabilities. The combination of Azure OpenAI intelligence and Neo4j's graph queries gives you unprecedented visibility into your legacy codebase.

**Start with the suggestion chips** in the portal, or ask your own questions. The AI is ready to help you understand your migration! 🚀
