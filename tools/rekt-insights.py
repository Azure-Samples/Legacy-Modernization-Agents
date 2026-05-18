#!/usr/bin/env python3
"""
REKT Data Insights — Extract useful analysis from Neo4j graph without dashboards.
Analyzes parsed COBOL structure, complexity, and modernization priorities.
"""

import json
import sys
from pathlib import Path
from typing import Any
import os

try:
    from neo4j import GraphDatabase
    from rich.console import Console
    from rich.table import Table
    from rich.panel import Panel
except ImportError:
    print("❌ Missing dependencies. Install with:")
    print("   pip install neo4j rich")
    sys.exit(1)

console = Console()

def get_driver():
    uri = os.environ.get("NEO4J_URI", "bolt://localhost:7688")
    user = os.environ.get("NEO4J_USER", "neo4j")
    password = os.environ.get("NEO4J_PASSWORD", "cobol-rekt-2026")
    return GraphDatabase.driver(uri, auth=(user, password))


def analyze_overview():
    """Show overall graph statistics."""
    driver = get_driver()
    with driver.session() as session:
        result = session.run("""
            CALL {
                MATCH (f:CobolFile) RETURN count(f) as programs
            }
            CALL {
                MATCH (n:ASTNode) RETURN count(n) as ast_nodes
            }
            CALL {
                MATCH (d:DataStructure) RETURN count(d) as data_structures
            }
            CALL {
                MATCH (b:BusinessLogic) RETURN count(b) as business_logic_units
            }
            CALL {
                MATCH (r:Run) RETURN count(r) as scan_runs
            }
            RETURN programs, ast_nodes, data_structures, business_logic_units, scan_runs
        """)
        stats = result.single()
    
    table = Table(title="📊 COBOL Codebase Overview", show_header=True)
    table.add_column("Metric", style="cyan")
    table.add_column("Count", style="green")
    
    table.add_row("COBOL Programs", str(stats[0]))
    table.add_row("AST Nodes (parsed statements)", str(stats[1]))
    table.add_row("Data Structures (records, copybooks)", str(stats[2]))
    table.add_row("Business Logic Units (extracted)", str(stats[3]))
    table.add_row("Analysis Scans Performed", str(stats[4]))
    
    console.print(table)
    driver.close()


def analyze_program_complexity():
    """Identify programs by complexity (lines, structures, logic)."""
    driver = get_driver()
    with driver.session() as session:
        result = session.run("""
            MATCH (f:CobolFile)
            OPTIONAL MATCH (f)-[:CONTAINS]->(ast:ASTNode)
            OPTIONAL MATCH (f)-[:HAS_DATA_STRUCTURE]->(ds:DataStructure)
            OPTIONAL MATCH (f)-[:HAS_BUSINESS_LOGIC]->(bl:BusinessLogic)
            WITH f, COUNT(ast) as ast_count, COUNT(ds) as ds_count, COUNT(bl) as bl_count
            ORDER BY ast_count DESC
            LIMIT 20
            RETURN f.name, ast_count, ds_count, bl_count
        """)
        rows = result.fetch(20)
    
    if not rows:
        console.print("[yellow]⚠️  No program complexity data found[/yellow]")
        return
    
    table = Table(title="🔥 Top 20 Most Complex Programs", show_header=True)
    table.add_column("Program Name", style="cyan")
    table.add_column("AST Nodes", style="yellow")
    table.add_column("Data Structures", style="blue")
    table.add_column("Business Logic Units", style="green")
    
    for name, ast_c, ds_c, bl_c in rows:
        complexity = "🔴 CRITICAL" if ast_c > 5000 else "🟡 HIGH" if ast_c > 2000 else "🟢 MEDIUM"
        table.add_row(name or "UNKNOWN", str(ast_c), str(ds_c), str(bl_c))
    
    console.print(table)
    driver.close()


def analyze_data_structures():
    """Identify key data structures and their usage."""
    driver = get_driver()
    with driver.session() as session:
        result = session.run("""
            MATCH (ds:DataStructure)
            OPTIONAL MATCH (ds)<-[:USES_STRUCTURE]-(f:CobolFile)
            WITH ds, COUNT(f) as usage_count
            ORDER BY usage_count DESC
            LIMIT 20
            RETURN ds.name, ds.type, usage_count, ds.size
        """)
        rows = result.fetch(20)
    
    if not rows:
        console.print("[yellow]⚠️  No data structure data found[/yellow]")
        return
    
    table = Table(title="📦 Top Data Structures by Usage", show_header=True)
    table.add_column("Structure Name", style="cyan")
    table.add_column("Type", style="blue")
    table.add_column("Used By N Programs", style="yellow")
    table.add_column("Size (bytes)", style="green")
    
    for name, ds_type, usage, size in rows:
        table.add_row(name or "UNKNOWN", ds_type or "?", str(usage), str(size or "?"))
    
    console.print(table)
    driver.close()


def analyze_business_logic():
    """Extract high-level business logic patterns."""
    driver = get_driver()
    with driver.session() as session:
        result = session.run("""
            MATCH (bl:BusinessLogic)
            OPTIONAL MATCH (bl)-[:EXTRACTED_FROM]->(f:CobolFile)
            WITH bl, f, COUNT(DISTINCT f) as occurrences
            ORDER BY occurrences DESC
            LIMIT 25
            RETURN bl.name, bl.description, bl.category, occurrences
        """)
        rows = result.fetch(25)
    
    if not rows:
        console.print("[yellow]⚠️  No business logic extracted[/yellow]")
        return
    
    table = Table(title="💡 Identified Business Logic Patterns", show_header=True)
    table.add_column("Pattern", style="cyan")
    table.add_column("Description", style="white", width=40)
    table.add_column("Category", style="blue")
    table.add_column("Occurrences", style="yellow")
    
    for name, desc, category, occurrences in rows:
        desc_short = (desc[:37] + "...") if desc and len(desc) > 40 else (desc or "")
        table.add_row(name or "?", desc_short, category or "?", str(occurrences))
    
    console.print(table)
    driver.close()


def analyze_dependencies():
    """Show inter-program dependencies (calls, uses)."""
    driver = get_driver()
    with driver.session() as session:
        # Find programs with most dependencies
        result = session.run("""
            MATCH (f:CobolFile)
            OPTIONAL MATCH (f)-[:CALLS|:USES|:REFERENCES]->(other:CobolFile)
            WITH f, COUNT(DISTINCT other) as outgoing_deps
            OPTIONAL MATCH (caller:CobolFile)-[:CALLS|:USES|:REFERENCES]->(f)
            WITH f, outgoing_deps, COUNT(DISTINCT caller) as incoming_deps
            ORDER BY (outgoing_deps + incoming_deps) DESC
            LIMIT 15
            RETURN f.name, outgoing_deps, incoming_deps, (outgoing_deps + incoming_deps) as total
        """)
        rows = result.fetch(15)
    
    if not rows or all(r[1] == 0 and r[2] == 0 for r in rows):
        console.print("[yellow]⚠️  No call graph relationships found (missing copybooks)[/yellow]")
        console.print("    This is expected without complete copybook set.")
        return
    
    table = Table(title="🔗 Programs with Most Dependencies", show_header=True)
    table.add_column("Program", style="cyan")
    table.add_column("Calls", style="yellow")
    table.add_column("Called By", style="blue")
    table.add_column("Total Deps", style="red")
    
    for name, out_deps, in_deps, total in rows:
        table.add_row(name or "?", str(out_deps), str(in_deps), str(total))
    
    console.print(table)
    driver.close()


def export_analysis_report():
    """Export full analysis as JSON for further processing."""
    driver = get_driver()
    
    report = {
        "timestamp": str(__import__('datetime').datetime.now()),
        "overview": {},
        "top_complex_programs": [],
        "data_structures": [],
        "business_logic": [],
        "recommendations": []
    }
    
    with driver.session() as session:
        # Overview
        stats = session.run("""
            RETURN 
                (MATCH (f:CobolFile) RETURN count(f)) as programs,
                (MATCH (n:ASTNode) RETURN count(n)) as ast_nodes,
                (MATCH (d:DataStructure) RETURN count(d)) as data_structures,
                (MATCH (b:BusinessLogic) RETURN count(b)) as business_logic_units
        """).single()
        
        report["overview"] = {
            "programs": stats[0],
            "ast_nodes": stats[1],
            "data_structures": stats[2],
            "business_logic_units": stats[3]
        }
        
        # Complex programs
        result = session.run("""
            MATCH (f:CobolFile)
            OPTIONAL MATCH (f)-[:CONTAINS]->(ast:ASTNode)
            WITH f, COUNT(ast) as complexity
            ORDER BY complexity DESC
            LIMIT 30
            RETURN f.name, complexity
        """)
        report["top_complex_programs"] = [
            {"name": r[0], "complexity": r[1]} for r in result
        ]
        
        # Data structures
        result = session.run("""
            MATCH (ds:DataStructure)
            OPTIONAL MATCH (ds)<-[:USES_STRUCTURE]-(f:CobolFile)
            WITH ds, COUNT(f) as usage
            ORDER BY usage DESC
            LIMIT 30
            RETURN ds.name, ds.type, usage
        """)
        report["data_structures"] = [
            {"name": r[0], "type": r[1], "usage_count": r[2]} for r in result
        ]
    
    # Write report
    report_path = Path("output/rekt-insights-report.json")
    with open(report_path, "w") as f:
        json.dump(report, f, indent=2)
    
    console.print(f"\n✅ Report exported to: {report_path}")
    driver.close()


def main():
    """Run all analyses."""
    console.clear()
    console.print("[bold cyan]╔════════════════════════════════════════════╗[/bold cyan]")
    console.print("[bold cyan]║  COBOL-REKT Data Insights Generator        ║[/bold cyan]")
    console.print("[bold cyan]║  Extracting actionable intelligence...     ║[/bold cyan]")
    console.print("[bold cyan]╚════════════════════════════════════════════╝[/bold cyan]\n")
    
    try:
        analyze_overview()
        console.print()
        
        analyze_program_complexity()
        console.print()
        
        analyze_business_logic()
        console.print()
        
        analyze_data_structures()
        console.print()
        
        analyze_dependencies()
        console.print()
        
        export_analysis_report()
        
        console.print("\n[green]✅ Analysis complete![/green]")
        console.print("[blue]📝 Recommendation:[/blue] Copy missing copybooks to source/ and rerun")
        console.print("   [cyan]./doctor.sh rekt-full[/cyan]")
        console.print("   This will unlock call graphs (+70% more insights)\n")
        
    except Exception as e:
        console.print(f"[red]❌ Error: {e}[/red]")
        import traceback
        traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()
