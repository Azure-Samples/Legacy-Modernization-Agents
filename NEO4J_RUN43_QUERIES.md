# Neo4j Queries for Run 43

Run 43 is successfully loaded in Neo4j with:
- **49 files** 
- **64 dependencies**

## Access Neo4j Browser

Open your browser and go to: **http://localhost:7474**

- Username: `neo4j`
- Password: `cobol-migration-2025`

## Useful Queries for Run 43

### 1. View Run 43 Summary
```cypher
MATCH (r:Run {id: 43})
RETURN r.id as RunID, r.timestamp as Timestamp
```

### 2. Count Files and Dependencies
```cypher
MATCH (r:Run {id: 43})-[:ANALYZED]->(f:CobolFile)
WITH r, count(f) as fileCount
MATCH (f1:CobolFile)-[d:DEPENDS_ON]->(f2:CobolFile)
WHERE f1.runId = 43
RETURN r.id as RunID, fileCount as Files, count(d) as Dependencies
```

### 3. View All Files in Run 43
```cypher
MATCH (r:Run {id: 43})-[:ANALYZED]->(f:CobolFile)
RETURN f.fileName as FileName, f.isCopybook as IsCopybook
ORDER BY f.isCopybook DESC, f.fileName
```

### 4. Visualize Full Dependency Graph (Run 43)
```cypher
MATCH (source:CobolFile)-[d:DEPENDS_ON]->(target:CobolFile)
WHERE source.runId = 43 AND target.runId = 43
RETURN source, d, target
LIMIT 100
```

### 5. Find Hub Files (Most Connected)
```cypher
MATCH (f:CobolFile)
WHERE f.runId = 43
OPTIONAL MATCH (f)<-[incoming:DEPENDS_ON]-()
OPTIONAL MATCH (f)-[outgoing:DEPENDS_ON]->()
WITH f, count(DISTINCT incoming) as inCount, count(DISTINCT outgoing) as outCount
RETURN f.fileName as FileName, 
       f.isCopybook as IsCopybook,
       inCount as IncomingDeps, 
       outCount as OutgoingDeps,
       inCount + outCount as TotalConnections
ORDER BY TotalConnections DESC
LIMIT 10
```

### 6. Find Circular Dependencies
```cypher
MATCH path = (start:CobolFile)-[:DEPENDS_ON*2..10]->(start)
WHERE start.runId = 43
WITH path, length(path) as pathLength
ORDER BY pathLength
RETURN [node in nodes(path) | node.fileName] as Cycle, pathLength as Length
LIMIT 20
```

### 7. Impact Analysis for a Specific File
Replace `'BDSMFJL.cbl'` with your file name:
```cypher
MATCH (source:CobolFile {fileName: 'BDSMFJL.cbl', runId: 43})
OPTIONAL MATCH path1 = (source)<-[:DEPENDS_ON*1..5]-(affected)
OPTIONAL MATCH path2 = (source)-[:DEPENDS_ON*1..5]->(dependency)
RETURN source.fileName as TargetFile,
       collect(DISTINCT affected.fileName) as AffectedFiles,
       collect(DISTINCT dependency.fileName) as Dependencies
```

### 8. View Copybooks Only
```cypher
MATCH (f:CobolFile {runId: 43, isCopybook: true})
RETURN f.fileName as Copybook
ORDER BY f.fileName
```

### 9. View Programs Only (Non-Copybooks)
```cypher
MATCH (f:CobolFile {runId: 43, isCopybook: false})
RETURN f.fileName as Program
ORDER BY f.fileName
```

### 10. Dependency Path Between Two Files
Replace file names with your specific files:
```cypher
MATCH path = shortestPath(
  (source:CobolFile {fileName: 'BDSMFJL.cbl', runId: 43})-[:DEPENDS_ON*]->(target:CobolFile {fileName: 'BDSMFJLI.cpy', runId: 43})
)
RETURN [node in nodes(path) | node.fileName] as Path, length(path) as Distance
```

## Verify Run 43 is Loaded

Run this command in your terminal to confirm:
```bash
docker exec cobol-migration-neo4j cypher-shell -u neo4j -p "cobol-migration-2025" \
  "MATCH (r:Run {id: 43})-[:ANALYZED]->(f:CobolFile) 
   WITH r, count(f) as fileCount 
   MATCH (f1:CobolFile)-[d:DEPENDS_ON]->(f2:CobolFile) 
   WHERE f1.runId = 43 
   RETURN r.id as runId, fileCount, count(d) as depCount;"
```

Expected output:
```
runId, fileCount, depCount
43, 49, 64
```

## Troubleshooting

If you don't see Run 43:
1. Check Neo4j is running: `docker ps | grep neo4j`
2. Restart Neo4j: `docker restart cobol-migration-neo4j`
3. Check connection: Open http://localhost:7474 in browser

## Compare Runs

To see differences between runs:
```cypher
MATCH (r:Run)
WHERE r.id IN [40, 41, 42, 43]
OPTIONAL MATCH (r)-[:ANALYZED]->(f:CobolFile)
RETURN r.id as RunID, 
       r.timestamp as Timestamp,
       count(f) as FileCount
ORDER BY r.id DESC
```
