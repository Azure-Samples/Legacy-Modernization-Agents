"""
Orchestrator — Coordinates the rekt container and graph populator.

Calls the cobol-rekt container to parse each COBOL file, then feeds
the JSON output into the graph populator for Neo4j ingestion.

Usage:
    python -m orchestrate --source-dir /source --run-id 1
    python -m orchestrate --source-dir /source --run-id 1 --workers 4

Environment:
    NEO4J_URI, NEO4J_USER, NEO4J_PASSWORD — Neo4j connection
    REKT_OUTPUT_DIR — where rekt writes JSON (default: /output)
    REKT_PARALLEL_WORKERS — parallel file processing (default: CPU/2)
"""

from __future__ import annotations

import os
import subprocess
import time
from concurrent.futures import ProcessPoolExecutor, as_completed
from pathlib import Path

import click
from rich.console import Console
from rich.progress import Progress, SpinnerColumn, BarColumn, TextColumn, TimeElapsedColumn

from populator import get_driver, apply_schema, ingest_rekt_outputs, migrate_sqlite
from source_paths import source_relative_path

console = Console()


def get_worker_count() -> int:
    env_val = os.environ.get("REKT_PARALLEL_WORKERS")
    if env_val:
        return int(env_val)
    return max(1, (os.cpu_count() or 2) // 2)


REKT_COMMANDS = "BUILD_BASE_ANALYSIS WRITE_FLOW_AST WRITE_CFG WRITE_DATA_STRUCTURES EXPORT_UNIFIED_TO_JSON"


def parse_single_file(
    file_path: str,
    source_dir: str,
    output_dir: str,
    dialect: str = "COBOL",
) -> dict:
    """
    Call the cobol-rekt container to parse a single COBOL file.
    Returns {file, success, duration_s, error?}.
    """
    file_name = source_relative_path(file_path, source_dir)
    start = time.time()

    cmd = [
        "docker", "exec", "cobol-rekt",
        "java", "-jar", "/app/smojol-cli.jar",
        "run", file_name,
        f"--commands={REKT_COMMANDS}",
        f"--srcDir={source_dir}",
        f"--copyBooksDir={source_dir}",
        "--dialectJarPath=/app/dialect-idms.jar",
        f"--dialect={dialect}",
        f"--reportDir={output_dir}",
        "--generation=PROGRAM",
    ]

    try:
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            timeout=300,  # 5 min max per file
        )
        duration = time.time() - start

        if result.returncode != 0:
            return {
                "file": file_name,
                "success": False,
                "duration_s": duration,
                "error": result.stderr[:500] if result.stderr else f"Exit code {result.returncode}",
            }
        return {"file": file_name, "success": True, "duration_s": duration}

    except subprocess.TimeoutExpired:
        return {
            "file": file_name,
            "success": False,
            "duration_s": 300,
            "error": "Timeout (5 min)",
        }
    except Exception as e:
        return {
            "file": file_name,
            "success": False,
            "duration_s": time.time() - start,
            "error": str(e),
        }


@click.command()
@click.option("--source-dir", default="/source", help="COBOL source directory")
@click.option("--output-dir", default="/output", help="Rekt JSON output directory")
@click.option("--run-id", default=1, type=int, help="Migration run ID")
@click.option("--workers", default=None, type=int, help="Parallel workers (default: CPU/2)")
@click.option("--sqlite-db", default=None, help="SQLite DB path for hybrid migration")
@click.option("--dialect", default="COBOL", help="COBOL dialect (COBOL, IDMS)")
@click.option("--skip-parse", is_flag=True, help="Skip rekt parsing, ingest existing JSON only")
def orchestrate(
    source_dir: str,
    output_dir: str,
    run_id: int,
    workers: int | None,
    sqlite_db: str | None,
    dialect: str,
    skip_parse: bool,
):
    """Full pipeline: parse COBOL files → ingest into Neo4j."""
    source_path = Path(source_dir)
    worker_count = workers or get_worker_count()

    # Find COBOL files (recursive — supports nested source layouts).
    # Keep in sync with SourceTypeRegistry: .cbl, .cob → programs; .cpy → copybook.
    skip_dirs = {".rekt-staging", ".preprocessed"}
    exts = {".cbl", ".cob", ".cpy"}
    cobol_files = sorted(
        p for p in source_path.rglob("*")
        if p.is_file()
        and p.suffix.lower() in exts
        and not any(part in skip_dirs for part in p.parts)
    )

    if not cobol_files:
        console.print(f"[red]No COBOL files found in {source_dir}[/red]")
        return

    console.print(f"[bold blue]Found {len(cobol_files)} COBOL files[/bold blue]")
    console.print(f"[bold blue]Workers: {worker_count}[/bold blue]")

    # ── Step 1: Parse with rekt container ────────────────────────────
    if not skip_parse:
        console.print("\n[bold cyan]Step 1/3: Parsing with Cobol-REKT...[/bold cyan]")

        # Verify rekt container is running
        check = subprocess.run(
            ["docker", "inspect", "--format={{.State.Running}}", "cobol-rekt"],
            capture_output=True, text=True,
        )
        if "true" not in (check.stdout or ""):
            console.print("[red]cobol-rekt container is not running. Start it with:[/red]")
            console.print("[yellow]  docker-compose up -d cobol-rekt cobol-rekt-neo4j[/yellow]")
            return

        results = []
        with Progress(
            SpinnerColumn(),
            TextColumn("[progress.description]{task.description}"),
            BarColumn(),
            TextColumn("[progress.percentage]{task.percentage:>3.0f}%"),
            TimeElapsedColumn(),
        ) as progress:
            task = progress.add_task("Parsing files...", total=len(cobol_files))

            # Process files in parallel
            with ProcessPoolExecutor(max_workers=worker_count) as executor:
                futures = {
                    executor.submit(
                        parse_single_file, str(f), source_dir, output_dir, dialect
                    ): f
                    for f in cobol_files
                }
                for future in as_completed(futures):
                    result = future.result()
                    results.append(result)
                    status = "[green]OK" if result["success"] else f"[red]FAIL: {result.get('error', '')[:60]}"
                    progress.console.print(
                        f"  {result['file']}: {status} ({result['duration_s']:.1f}s)"
                    )
                    progress.advance(task)

        succeeded = sum(1 for r in results if r["success"])
        console.print(f"\n  Parsed: {succeeded}/{len(results)} files")
    else:
        console.print("\n[bold cyan]Step 1/3: Skipping parse (--skip-parse)[/bold cyan]")

    # ── Step 2: Apply schema + migrate SQLite ────────────────────────
    console.print("\n[bold cyan]Step 2/3: Applying schema & migrating data...[/bold cyan]")

    driver = get_driver()
    try:
        apply_schema(driver)

        if sqlite_db:
            console.print(f"  Migrating SQLite: {sqlite_db}")
            sqlite_counts = migrate_sqlite(driver, sqlite_db)
            for label, count in sqlite_counts.items():
                console.print(f"    {label}: {count}")

        # ── Step 3: Ingest rekt JSON into Neo4j ───────────────────────
        console.print("\n[bold cyan]Step 3/3: Ingesting into Neo4j...[/bold cyan]")

        rekt_counts = ingest_rekt_outputs(driver, output_dir, source_dir, run_id)
        for label, count in rekt_counts.items():
            console.print(f"    {label}: {count}")

        # Final stats
        with driver.session() as session:
            result = session.run(
                "MATCH (n) RETURN count(n) AS nodes"
            )
            node_count = result.single()["nodes"]
            result2 = session.run(
                "MATCH ()-[r]->() RETURN count(r) AS rels"
            )
            rel_count = result2.single()["rels"]

        console.print(
            f"\n[bold green]Complete. Graph: {node_count:,} nodes, {rel_count:,} relationships[/bold green]"
        )
    finally:
        driver.close()


if __name__ == "__main__":
    orchestrate()
