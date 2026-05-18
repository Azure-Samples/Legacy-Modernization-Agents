#!/bin/bash
# Query REKT Neo4j database for actionable insights
# Usage: ./tools/query-rekt.sh complexity
#        ./tools/query-rekt.sh structures  
#        ./tools/query-rekt.sh business-logic

set -e

NEO4J_URI="bolt://localhost:7688"
NEO4J_USER="neo4j"
NEO4J_PASS="cobol-rekt-2026"

run_query() {
    local query="$1"
    curl -s -X POST "http://localhost:7475/db/neo4j/tx/commit" \
        -H "Content-Type: application/json" \
        -u "$NEO4J_USER:$NEO4J_PASS" \
        -d "{\"statements\": [{\"statement\": \"$query\"}]}" 2>/dev/null
}

case "${1:-help}" in
    complexity)
        echo "🔥 Top 20 Most Complex Programs (by AST statement count)"
        echo "================================================================"
        run_query "MATCH (f:CobolFile) OPTIONAL MATCH (f)-[:CONTAINS]->(ast:ASTNode) WITH f, COUNT(ast) as complexity ORDER BY complexity DESC LIMIT 20 RETURN f.name as program, complexity as statements" \
            | python3 -c "
import sys, json
try:
    r = json.load(sys.stdin)
    rows = r['results'][0]['data']
    if not rows:
        print('No data found')
    else:
        for idx, row in enumerate(rows, 1):
            prog, stmts = row['row']
            risk = '🔴 CRITICAL' if stmts > 5000 else '🟡 HIGH' if stmts > 2000 else '🟢 MEDIUM'
            print(f'{idx:2}. {prog or \"UNKNOWN\":30} {stmts:6,} statements  {risk}')
except Exception as e:
    print(f'Error: {e}', file=sys.stderr)
"
        ;;
    structures)
        echo "📦 All Data Structures (Records, Fields, Copybook Items)"
        echo "================================================================"
        run_query "MATCH (ds:DataStructure) RETURN ds.name as name, ds.type as type ORDER BY ds.name LIMIT 50" \
            | python3 -c "
import sys, json
try:
    r = json.load(sys.stdin)
    rows = r['results'][0]['data']
    if not rows:
        print('No structures found')
    else:
        for row in rows:
            name, dtype = row['row']
            print(f'  {name:40} ({dtype or \"item\"})')
except Exception as e:
    print(f'Error: {e}', file=sys.stderr)
"
        ;;
    business-logic)
        echo "💡 Extracted Business Logic Patterns"
        echo "================================================================"
        run_query "MATCH (bl:BusinessLogic) RETURN bl.name, COUNT(*) as frequency ORDER BY frequency DESC LIMIT 30" \
            | python3 -c "
import sys, json
try:
    r = json.load(sys.stdin)
    rows = r['results'][0]['data']
    if not rows:
        print('No business logic extracted')
    else:
        for row in rows:
            name, freq = row['row']
            print(f'  {name or \"?\":50} (found {freq} times)')
except Exception as e:
    print(f'Error: {e}', file=sys.stderr)
"
        ;;
    stats)
        echo "📊 Graph Statistics"
        echo "================================================================"
        run_query "CALL { MATCH (f:CobolFile) RETURN count(f) as programs } CALL { MATCH (n:ASTNode) RETURN count(n) as ast_nodes } CALL { MATCH (d:DataStructure) RETURN count(d) as data_structures } CALL { MATCH (b:BusinessLogic) RETURN count(b) as business_logic } RETURN programs, ast_nodes, data_structures, business_logic" \
            | python3 -c "
import sys, json
try:
    r = json.load(sys.stdin)
    rows = r['results'][0]['data']
    if rows:
        progs, ast, structs, logic = rows[0]['row']
        print(f'COBOL Programs:          {progs:,}')
        print(f'AST Statement Nodes:     {ast:,}')
        print(f'Data Structures:         {structs:,}')
        print(f'Business Logic Units:    {logic:,}')
except Exception as e:
    print(f'Error: {e}', file=sys.stderr)
"
        ;;
    help|*)
        echo "REKT Neo4j Query Tool — Extract insights from your COBOL scan"
        echo ""
        echo "Usage: ./tools/query-rekt.sh <command>"
        echo ""
        echo "Commands:"
        echo "  complexity      — Programs ranked by complexity (statement count)"
        echo "  structures      — All identified data structures (records, fields)"
        echo "  business-logic  — Extracted business patterns"
        echo "  stats           — Overall graph statistics"
        echo ""
        echo "Examples:"
        echo "  ./tools/query-rekt.sh complexity"
        echo "  ./tools/query-rekt.sh structures | grep ACCOUNT"
        echo "  ./tools/query-rekt.sh stats"
        ;;
esac
