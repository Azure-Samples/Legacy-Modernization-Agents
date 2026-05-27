"""
Graph Populator — Ingests Cobol-REKT JSON exports + MMA metadata into Neo4j.

Usage:
    python -m populator ingest   --source-dir /source --rekt-output /output
    python -m populator schema   (apply schema only)
    python -m populator migrate  --sqlite-db /data/migration.db

Environment:
    NEO4J_URI       bolt://localhost:7688
    NEO4J_USER      neo4j
    NEO4J_PASSWORD  cobol-rekt-2026
"""

from __future__ import annotations

import json
import os
import sqlite3
import uuid
from concurrent.futures import ProcessPoolExecutor, as_completed
from pathlib import Path
from typing import Any

import click
from neo4j import GraphDatabase, ManagedTransaction
from rich.console import Console
from rich.progress import Progress, SpinnerColumn, BarColumn, TextColumn

console = Console()

BATCH_SIZE = 10_000  # nodes per transaction — tuned for million-node ingestion
SOURCE_BLOCK_LINES = 500  # lines per SourceBlock node


# ═══════════════════════════════════════════════════════════════════════
# Neo4j Connection
# ═══════════════════════════════════════════════════════════════════════

def get_driver():
    uri = os.environ.get("NEO4J_URI", "bolt://localhost:7688")
    user = os.environ.get("NEO4J_USER", "neo4j")
    password = os.environ.get("NEO4J_PASSWORD", "cobol-rekt-2026")
    return GraphDatabase.driver(uri, auth=(user, password))


# ═══════════════════════════════════════════════════════════════════════
# Schema Application
# ═══════════════════════════════════════════════════════════════════════

def apply_schema(driver) -> None:
    """Apply constraints and indexes from schema.cypher."""
    schema_path = Path(__file__).parent / "schema.cypher"
    statements = [
        s.strip()
        for s in schema_path.read_text().split(";")
        if s.strip() and not s.strip().startswith("//")
    ]
    with driver.session() as session:
        for stmt in statements:
            # Skip pure comment lines
            lines = [l for l in stmt.split("\n") if not l.strip().startswith("//")]
            clean = "\n".join(lines).strip()
            if clean:
                session.run(clean)
    console.print("[green]Schema applied successfully[/green]")


# ═══════════════════════════════════════════════════════════════════════
# Batch Helpers
# ═══════════════════════════════════════════════════════════════════════

def _batch_list(items: list, size: int = BATCH_SIZE):
    """Yield successive chunks from a list."""
    for i in range(0, len(items), size):
        yield items[i : i + size]


def _run_batch_create(tx: ManagedTransaction, cypher: str, batch: list[dict]) -> None:
    tx.run(cypher, batch=batch)


def batch_merge_nodes(
    driver, label: str, props_list: list[dict], merge_key: str = "id"
) -> int:
    """Bulk-MERGE nodes using UNWIND batching. Returns count created."""
    if not props_list:
        return 0
    cypher = (
        f"UNWIND $batch AS row "
        f"MERGE (n:{label} {{{merge_key}: row.{merge_key}}}) "
        f"SET n += row"
    )
    total = 0
    with driver.session() as session:
        for batch in _batch_list(props_list):
            session.execute_write(_run_batch_create, cypher, batch)
            total += len(batch)
    return total


def batch_merge_relationships(
    driver,
    from_label: str,
    from_key: str,
    rel_type: str,
    to_label: str,
    to_key: str,
    rels: list[dict],
    rel_props: list[str] | None = None,
) -> int:
    """Bulk-MERGE relationships using UNWIND batching."""
    if not rels:
        return 0
    prop_set = ""
    if rel_props:
        assignments = ", ".join(f"r.{p} = row.{p}" for p in rel_props)
        prop_set = f"SET {assignments}"

    cypher = (
        f"UNWIND $batch AS row "
        f"MATCH (a:{from_label} {{{from_key}: row.from_id}}) "
        f"MATCH (b:{to_label} {{{to_key}: row.to_id}}) "
        f"MERGE (a)-[r:{rel_type}]->(b) "
        f"{prop_set}"
    )
    total = 0
    with driver.session() as session:
        for batch in _batch_list(rels):
            session.execute_write(_run_batch_create, cypher, batch)
            total += len(batch)
    return total


# ═══════════════════════════════════════════════════════════════════════
# Rekt JSON Ingestion
# ═══════════════════════════════════════════════════════════════════════

def _make_uid(*parts) -> str:
    """Deterministic UID from parts."""
    return str(uuid.uuid5(uuid.NAMESPACE_URL, "/".join(str(p) for p in parts)))


def ingest_rekt_ast(driver, program: str, ast_json: dict, run_id: int) -> int:
    """Ingest a rekt FLOW_AST JSON export into Neo4j as ASTNode tree."""
    nodes: list[dict] = []
    edges: list[dict] = []

    def _walk(node: dict, parent_id: str | None = None):
        node_id = node.get("id", _make_uid(program, node.get("type", ""), node.get("text", ""), len(nodes)))
        props = {
            "id": str(node_id),
            "program": program,
            "runId": run_id,
            "nodeType": node.get("type", "UNKNOWN"),
            "label": node.get("label", node.get("type", "")),
            "originalText": node.get("text", ""),
            "startLine": node.get("span", {}).get("start", -1),
            "endLine": node.get("span", {}).get("end", -1),
        }
        # Copy extra properties from rekt (scope, level, etc.)
        for key in ("scope", "level", "name", "section", "paragraph"):
            if key in node:
                props[key] = node[key]

        nodes.append(props)

        if parent_id is not None:
            edges.append({"from_id": parent_id, "to_id": str(node_id)})

        for child in node.get("children", []):
            _walk(child, str(node_id))

    _walk(ast_json)

    count = batch_merge_nodes(driver, "ASTNode", nodes)
    batch_merge_relationships(driver, "ASTNode", "id", "CONTAINS", "ASTNode", "id", edges)

    # Link root to CobolFile
    root_id = nodes[0]["id"] if nodes else None
    if root_id:
        with driver.session() as session:
            session.run(
                "MATCH (f:CobolFile {fileName: $program, runId: $runId}) "
                "MATCH (a:ASTNode {id: $rootId}) "
                "MERGE (f)-[:HAS_AST]->(a)",
                program=program, runId=run_id, rootId=root_id,
            )
    return count


def ingest_rekt_cfg(driver, program: str, cfg_json: dict, run_id: int) -> int:
    """Ingest a rekt CFG JSON export — FOLLOWED_BY and JUMPS_TO edges on ASTNodes."""
    followed_by: list[dict] = []
    jumps_to: list[dict] = []

    for edge in cfg_json.get("edges", []):
        from_id = str(edge.get("from") or edge.get("fromNodeID", ""))
        to_id = str(edge.get("to") or edge.get("toNodeID", ""))
        if not from_id or not to_id:
            continue
        rec = {"from_id": from_id, "to_id": to_id}
        edge_type = edge.get("type") or edge.get("edgeType", "FOLLOWED_BY")
        if edge_type == "JUMPS_TO":
            jumps_to.append(rec)
        else:
            followed_by.append(rec)

    count = 0
    count += batch_merge_relationships(
        driver, "ASTNode", "id", "FOLLOWED_BY", "ASTNode", "id", followed_by
    )
    count += batch_merge_relationships(
        driver, "ASTNode", "id", "JUMPS_TO", "ASTNode", "id", jumps_to
    )
    return count


def ingest_rekt_data_structures(
    driver, program: str, ds_json: dict, run_id: int
) -> int:
    """Ingest rekt data structure JSON into Neo4j."""
    nodes: list[dict] = []
    contains_edges: list[dict] = []
    flows_into_edges: list[dict] = []
    redefines_edges: list[dict] = []

    def _walk_ds(node: dict, parent_id: str | None = None):
        node_id = str(node.get("id", _make_uid(program, "ds", node.get("name", ""), len(nodes))))
        props = {
            "id": node_id,
            "program": program,
            "runId": run_id,
            "name": node.get("name", ""),
            "level": node.get("level", 0),
            "dataType": node.get("dataType", ""),
            "picture": node.get("picture", ""),
            "originalText": node.get("text", ""),
        }
        nodes.append(props)

        if parent_id:
            contains_edges.append({"from_id": parent_id, "to_id": node_id})

        for child in node.get("children", []):
            _walk_ds(child, node_id)

    # Top-level might be a list or a single root
    records = ds_json if isinstance(ds_json, list) else ds_json.get("records", [ds_json])
    for rec in records:
        _walk_ds(rec)

    count = batch_merge_nodes(driver, "DataStructure", nodes)
    batch_merge_relationships(
        driver, "DataStructure", "id", "CONTAINS", "DataStructure", "id", contains_edges
    )

    # FLOWS_INTO / REDEFINES from explicit edges in the JSON
    for edge in ds_json.get("edges", []) if isinstance(ds_json, dict) else []:
        rec = {"from_id": str(edge["from"]), "to_id": str(edge["to"])}
        if edge.get("type") == "REDEFINES":
            redefines_edges.append(rec)
        else:
            flows_into_edges.append(rec)

    batch_merge_relationships(
        driver, "DataStructure", "id", "FLOWS_INTO", "DataStructure", "id", flows_into_edges
    )
    batch_merge_relationships(
        driver, "DataStructure", "id", "REDEFINES", "DataStructure", "id", redefines_edges
    )
    return count


# ═══════════════════════════════════════════════════════════════════════
# Source Block Splitting
# ═══════════════════════════════════════════════════════════════════════

def create_source_blocks(
    driver, program: str, content: str, run_id: int
) -> int:
    """Split source content into SourceBlock nodes (500 lines each)."""
    lines = content.split("\n")
    blocks: list[dict] = []
    for i in range(0, len(lines), SOURCE_BLOCK_LINES):
        block_lines = lines[i : i + SOURCE_BLOCK_LINES]
        blocks.append(
            {
                "uid": _make_uid(run_id, program, "sb", i),
                "program": program,
                "runId": run_id,
                "blockIndex": i // SOURCE_BLOCK_LINES,
                "startLine": i + 1,
                "endLine": min(i + SOURCE_BLOCK_LINES, len(lines)),
                "content": "\n".join(block_lines),
            }
        )

    count = batch_merge_nodes(driver, "SourceBlock", blocks, merge_key="uid")

    # Link to CobolFile
    rels = [{"from_id": _make_uid(run_id, program), "to_id": b["uid"]} for b in blocks]
    batch_merge_relationships(
        driver, "CobolFile", "uid", "HAS_SOURCE_BLOCK", "SourceBlock", "uid", rels
    )
    return count


# ═══════════════════════════════════════════════════════════════════════
# SQLite Migration (Hybrid Phase — read from SQLite, write to Neo4j)
# ═══════════════════════════════════════════════════════════════════════

def migrate_sqlite(driver, db_path: str) -> dict[str, int]:
    """Read existing SQLite migration.db and replicate into Neo4j."""
    if not Path(db_path).exists():
        console.print(f"[yellow]SQLite DB not found: {db_path}[/yellow]")
        return {}

    conn = sqlite3.connect(db_path)
    conn.row_factory = sqlite3.Row
    counts: dict[str, int] = {}

    # ── Runs ──
    runs = [dict(r) for r in conn.execute("SELECT * FROM runs").fetchall()]
    run_nodes = [
        {
            "id": r["id"],
            "startedAt": r["started_at"],
            "completedAt": r.get("completed_at", ""),
            "status": r["status"],
            "cobolSource": r.get("cobol_source", ""),
            "javaOutput": r.get("java_output", ""),
            "notes": r.get("notes", ""),
        }
        for r in runs
    ]
    counts["Run"] = batch_merge_nodes(driver, "Run", run_nodes)

    # ── CobolFiles ──
    files = [dict(r) for r in conn.execute("SELECT * FROM cobol_files").fetchall()]
    file_nodes = [
        {
            "uid": _make_uid(f["run_id"], f["file_name"]),
            "runId": f["run_id"],
            "fileName": f["file_name"],
            "filePath": f["file_path"],
            "isCopybook": bool(f["is_copybook"]),
            "content": f.get("content", ""),
            "lineCount": len((f.get("content") or "").split("\n")),
        }
        for f in files
    ]
    counts["CobolFile"] = batch_merge_nodes(driver, "CobolFile", file_nodes, merge_key="uid")

    # Run→CobolFile relationships
    run_file_rels = [
        {"from_id": f["run_id"], "to_id": _make_uid(f["run_id"], f["file_name"])}
        for f in files
    ]
    batch_merge_relationships(
        driver, "Run", "id", "ANALYZED", "CobolFile", "uid", run_file_rels
    )

    # Create SourceBlocks for large files
    for f in files:
        content = f.get("content") or ""
        if len(content) > 50_000:  # >50KB → split into blocks
            create_source_blocks(driver, f["file_name"], content, f["run_id"])

    # ── Dependencies ──
    deps = [dict(r) for r in conn.execute("SELECT * FROM dependencies").fetchall()]
    dep_rels = [
        {
            "from_id": _make_uid(d["run_id"], d["source_file"]),
            "to_id": _make_uid(d["run_id"], d["target_file"]),
            "type": d.get("dependency_type", "UNKNOWN"),
            "lineNumber": d.get("line_number", -1),
            "context": d.get("context", ""),
        }
        for d in deps
    ]
    counts["dependencies"] = batch_merge_relationships(
        driver, "CobolFile", "uid", "DEPENDS_ON", "CobolFile", "uid",
        dep_rels, rel_props=["type", "lineNumber", "context"],
    )

    # ── Signatures ──
    try:
        sigs = [dict(r) for r in conn.execute("SELECT * FROM signatures").fetchall()]
        sig_nodes = [
            {
                "uid": _make_uid(s["run_id"], s["source_file"], s["legacy_name"]),
                "runId": s["run_id"],
                "sourceFile": s["source_file"],
                "chunkIndex": s["chunk_index"],
                "legacyName": s["legacy_name"],
                "targetMethodName": s["target_method_name"],
                "targetSignature": s["target_signature"],
                "returnType": s["return_type"],
                "parameters": s.get("parameters", ""),
            }
            for s in sigs
        ]
        counts["Signature"] = batch_merge_nodes(driver, "Signature", sig_nodes, merge_key="uid")
        sig_rels = [
            {"from_id": _make_uid(s["run_id"], s["source_file"]), "to_id": _make_uid(s["run_id"], s["source_file"], s["legacy_name"])}
            for s in sigs
        ]
        batch_merge_relationships(
            driver, "CobolFile", "uid", "DEFINES", "Signature", "uid", sig_rels
        )
    except sqlite3.OperationalError:
        pass  # table may not exist yet

    # ── Business Logic ──
    try:
        bls = [dict(r) for r in conn.execute("SELECT * FROM business_logic").fetchall()]
        bl_nodes = [
            {
                "uid": _make_uid(b["run_id"], b["file_name"], "bl"),
                "runId": b["run_id"],
                "fileName": b["file_name"],
                "isCopybook": bool(b.get("is_copybook", 0)),
                "businessPurpose": b.get("business_purpose", ""),
                "userStoriesJson": b.get("user_stories_json", ""),
                "featuresJson": b.get("features_json", ""),
                "businessRulesJson": b.get("business_rules_json", ""),
            }
            for b in bls
        ]
        counts["BusinessLogic"] = batch_merge_nodes(driver, "BusinessLogic", bl_nodes, merge_key="uid")
        bl_rels = [
            {"from_id": _make_uid(b["run_id"], b["file_name"]), "to_id": _make_uid(b["run_id"], b["file_name"], "bl")}
            for b in bls
        ]
        batch_merge_relationships(
            driver, "CobolFile", "uid", "HAS_BUSINESS_LOGIC", "BusinessLogic", "uid", bl_rels
        )
    except sqlite3.OperationalError:
        pass

    # ── Chunk Metadata ──
    try:
        chunks = [dict(r) for r in conn.execute("SELECT * FROM chunk_metadata").fetchall()]
        chunk_nodes = [
            {
                "uid": _make_uid(c["run_id"], c["source_file"], "chunk", c["chunk_index"]),
                "runId": c["run_id"],
                "sourceFile": c["source_file"],
                "chunkIndex": c["chunk_index"],
                "startLine": c["start_line"],
                "endLine": c["end_line"],
                "status": c["status"],
                "tokensUsed": c.get("tokens_used", 0),
                "processingTimeMs": c.get("processing_time_ms", 0),
                "convertedCode": c.get("converted_code", ""),
            }
            for c in chunks
        ]
        counts["Chunk"] = batch_merge_nodes(driver, "Chunk", chunk_nodes, merge_key="uid")
        chunk_rels = [
            {"from_id": _make_uid(c["run_id"], c["source_file"]), "to_id": _make_uid(c["run_id"], c["source_file"], "chunk", c["chunk_index"])}
            for c in chunks
        ]
        batch_merge_relationships(
            driver, "CobolFile", "uid", "HAS_CHUNK", "Chunk", "uid", chunk_rels
        )
    except sqlite3.OperationalError:
        pass

    # ── Type Mappings ──
    try:
        tms = [dict(r) for r in conn.execute("SELECT * FROM type_mappings").fetchall()]
        tm_nodes = [
            {
                "uid": _make_uid(t["run_id"], t["source_file"], t["legacy_variable"]),
                "runId": t["run_id"],
                "sourceFile": t["source_file"],
                "legacyVariable": t["legacy_variable"],
                "legacyType": t["legacy_type"],
                "targetType": t["target_type"],
                "targetFieldName": t["target_field_name"],
                "isNullable": bool(t.get("is_nullable", 0)),
                "defaultValue": t.get("default_value", ""),
            }
            for t in tms
        ]
        counts["TypeMapping"] = batch_merge_nodes(driver, "TypeMapping", tm_nodes, merge_key="uid")
        tm_rels = [
            {"from_id": _make_uid(t["run_id"], t["source_file"]), "to_id": _make_uid(t["run_id"], t["source_file"], t["legacy_variable"])}
            for t in tms
        ]
        batch_merge_relationships(
            driver, "CobolFile", "uid", "HAS_TYPE_MAP", "TypeMapping", "uid", tm_rels
        )
    except sqlite3.OperationalError:
        pass

    # ── Metrics ──
    try:
        metrics = [dict(r) for r in conn.execute("SELECT * FROM metrics").fetchall()]
        m_nodes = [
            {
                "uid": _make_uid(m["run_id"], "metrics"),
                "runId": m["run_id"],
                "totalPrograms": m.get("total_programs", 0),
                "totalCopybooks": m.get("total_copybooks", 0),
                "totalDependencies": m.get("total_dependencies", 0),
                "avgDependenciesPerProgram": m.get("avg_dependencies_per_program", 0.0),
                "mostUsedCopybook": m.get("most_used_copybook", ""),
                "circularDependenciesJson": m.get("circular_dependencies_json", ""),
                "mermaidDiagram": m.get("mermaid_diagram", ""),
            }
            for m in metrics
        ]
        counts["Metrics"] = batch_merge_nodes(driver, "Metrics", m_nodes, merge_key="uid")
        m_rels = [
            {"from_id": m["run_id"], "to_id": _make_uid(m["run_id"], "metrics")}
            for m in metrics
        ]
        batch_merge_relationships(
            driver, "Run", "id", "HAS_METRICS", "Metrics", "uid", m_rels
        )
    except sqlite3.OperationalError:
        pass

    conn.close()
    return counts


# ═══════════════════════════════════════════════════════════════════════
# Full Ingestion Pipeline
# ═══════════════════════════════════════════════════════════════════════

def ingest_rekt_outputs(driver, rekt_output_dir: str, source_dir: str, run_id: int) -> dict[str, int]:
    """Scan rekt JSON output directory and ingest all exports into Neo4j."""
    output_path = Path(rekt_output_dir)
    source_path = Path(source_dir)
    counts: dict[str, int] = {}

    if not output_path.exists():
        console.print(f"[yellow]Rekt output dir not found: {rekt_output_dir}[/yellow]")
        return counts

    # Find all COBOL source files recursively and create CobolFile nodes.
    # Ignore tool artifacts so they don't pollute run-scoped file sets.
    def _is_ignored_source_file(p: Path) -> bool:
        ignored_markers = {".rekt-staging", ".preprocessed"}
        return any(part in ignored_markers for part in p.parts)

    cobol_files = [
        p for p in source_path.rglob("*")
        if p.is_file()
        and p.suffix.lower() in (".cbl", ".cob")
        and not _is_ignored_source_file(p)
    ]
    copybooks = [
        p for p in source_path.rglob("*")
        if p.is_file()
        and p.suffix.lower() == ".cpy"
        and not _is_ignored_source_file(p)
    ]

    file_nodes = []
    for f in cobol_files + copybooks:
        content = f.read_text(errors="replace")
        file_nodes.append(
            {
                "uid": _make_uid(run_id, f.name),
                "runId": run_id,
                "fileName": f.name,
                "filePath": str(f),
                "isCopybook": f.suffix.lower() in (".cpy",),
                "content": content,
                "lineCount": content.count("\n") + 1,
            }
        )
    counts["CobolFile"] = batch_merge_nodes(driver, "CobolFile", file_nodes, merge_key="uid")

    # Create SourceBlocks for all files
    for fn in file_nodes:
        create_source_blocks(driver, fn["fileName"], fn["content"], run_id)

    # Process rekt JSON exports per program
    ast_total = 0
    cfg_total = 0
    ds_total = 0

    for json_file in sorted(output_path.rglob("*.json")):
        program_name = json_file.stem
        rel_path = json_file.relative_to(output_path)

        try:
            data = json.loads(json_file.read_text())
        except json.JSONDecodeError:
            console.print(f"[yellow]Skipping invalid JSON: {rel_path}[/yellow]")
            continue

        # Detect export type from filename conventions
        name_lower = json_file.name.lower()
        if "flow-ast" in name_lower or "ast" in name_lower:
            ast_total += ingest_rekt_ast(driver, program_name, data, run_id)
        elif "cfg" in name_lower:
            cfg_total += ingest_rekt_cfg(driver, program_name, data, run_id)
        elif "data-structure" in name_lower or "data_structure" in name_lower or name_lower.endswith("-data.json"):
            ds_total += ingest_rekt_data_structures(driver, program_name, data, run_id)

    counts["ASTNode"] = ast_total
    counts["CFGEdge"] = cfg_total
    counts["DataStructure"] = ds_total
    return counts


# ═══════════════════════════════════════════════════════════════════════
# CLI Entry Point
# ═══════════════════════════════════════════════════════════════════════

@click.group()
def cli():
    """Cobol-REKT Graph Populator — ingest COBOL analysis into Neo4j."""
    pass


@cli.command()
def schema():
    """Apply Neo4j schema (constraints + indexes)."""
    driver = get_driver()
    try:
        apply_schema(driver)
    finally:
        driver.close()


@cli.command()
@click.option("--source-dir", default="/source", help="COBOL source directory")
@click.option("--rekt-output", default="/output", help="Rekt JSON output directory")
@click.option("--run-id", default=1, type=int, help="Migration run ID")
@click.option("--sqlite-db", default=None, help="Optional SQLite DB to migrate from")
def ingest(source_dir: str, rekt_output: str, run_id: int, sqlite_db: str | None):
    """Full ingestion: rekt JSON exports + optional SQLite migration."""
    driver = get_driver()
    try:
        console.print("[bold blue]Applying schema...[/bold blue]")
        apply_schema(driver)

        if sqlite_db:
            console.print(f"[bold blue]Migrating SQLite: {sqlite_db}[/bold blue]")
            sqlite_counts = migrate_sqlite(driver, sqlite_db)
            for label, count in sqlite_counts.items():
                console.print(f"  {label}: {count} nodes")

        console.print("[bold blue]Ingesting rekt outputs...[/bold blue]")
        rekt_counts = ingest_rekt_outputs(driver, rekt_output, source_dir, run_id)
        for label, count in rekt_counts.items():
            console.print(f"  {label}: {count}")

        # Print totals
        with driver.session() as session:
            result = session.run(
                "MATCH (n) RETURN count(n) AS nodes, "
                "size([(a)-[r]->(b) | r]) AS rels"
            )
            rec = result.single()
            console.print(
                f"\n[bold green]Done. Total: {rec['nodes']} nodes, {rec['rels']} relationships[/bold green]"
            )
    finally:
        driver.close()


@cli.command()
@click.option("--sqlite-db", required=True, help="Path to migration.db")
def migrate(sqlite_db: str):
    """Migrate SQLite data into Neo4j (hybrid phase)."""
    driver = get_driver()
    try:
        apply_schema(driver)
        counts = migrate_sqlite(driver, sqlite_db)
        for label, count in counts.items():
            console.print(f"  {label}: {count} nodes")
    finally:
        driver.close()


if __name__ == "__main__":
    cli()
