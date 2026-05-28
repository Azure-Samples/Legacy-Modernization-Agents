#!/usr/bin/env python3
# ─────────────────────────────────────────────────────────────────────────────
# ingest-metrics.py — JSONL → SQLite ingester for MetricsSink events
#
# Reads output/.metrics/*.jsonl and populates Data/benchmark.db with a flat
# events table that supports regression tracking and ad-hoc analysis.
#
# Usage:
#   python3 tools/ingest-metrics.py                  # ingest all new events
#   python3 tools/ingest-metrics.py --rebuild        # drop and rebuild from scratch
#   python3 tools/ingest-metrics.py --report         # print a summary
# ─────────────────────────────────────────────────────────────────────────────
import argparse
import json
import os
import sqlite3
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
METRICS_DIR = REPO_ROOT / "output" / ".metrics"
DB_PATH = REPO_ROOT / "Data" / "benchmark.db"

SCHEMA = """
CREATE TABLE IF NOT EXISTS metric_events (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    run_id TEXT NOT NULL,
    ts TEXT NOT NULL,
    event TEXT,
    agent TEXT,
    file TEXT,
    target_language TEXT,
    projection_mode TEXT,
    projection_tokens INTEGER,
    raw_rekt_tokens INTEGER,
    projection_hash TEXT,
    facts_schema INTEGER,
    facts_confidence INTEGER,
    facts_warnings INTEGER,
    provider TEXT,
    model TEXT,
    outcome TEXT,
    first_token_latency_ms INTEGER,
    stream_duration_ms INTEGER,
    sdk_timeout_ms INTEGER,
    completion_tokens INTEGER,
    fallback_reason TEXT,
    rekt_provenance TEXT,
    rekt_confidence REAL,
    payload_json TEXT NOT NULL,
    source_file TEXT NOT NULL,
    source_line INTEGER NOT NULL,
    UNIQUE(source_file, source_line)
);

CREATE INDEX IF NOT EXISTS idx_event ON metric_events(event);
CREATE INDEX IF NOT EXISTS idx_run ON metric_events(run_id);
CREATE INDEX IF NOT EXISTS idx_agent ON metric_events(agent);
CREATE INDEX IF NOT EXISTS idx_projection_mode ON metric_events(projection_mode);
"""


def open_db(rebuild: bool) -> sqlite3.Connection:
    DB_PATH.parent.mkdir(parents=True, exist_ok=True)
    if rebuild and DB_PATH.exists():
        DB_PATH.unlink()
    conn = sqlite3.connect(DB_PATH)
    conn.executescript(SCHEMA)
    return conn


def ingest(conn: sqlite3.Connection) -> int:
    inserted = 0
    skipped = 0
    if not METRICS_DIR.exists():
        print(f"(no metrics dir at {METRICS_DIR} — nothing to ingest)", file=sys.stderr)
        return 0

    files = sorted(METRICS_DIR.glob("*.jsonl"))
    for f in files:
        with f.open("r", encoding="utf-8") as fh:
            for line_no, raw in enumerate(fh, start=1):
                raw = raw.strip()
                if not raw:
                    continue
                try:
                    obj = json.loads(raw)
                except json.JSONDecodeError:
                    skipped += 1
                    continue
                try:
                    conn.execute(
                        """
                        INSERT OR IGNORE INTO metric_events (
                            run_id, ts, event, agent, file, target_language,
                            projection_mode, projection_tokens, raw_rekt_tokens,
                            projection_hash, facts_schema, facts_confidence,
                            facts_warnings, provider, model, outcome,
                            first_token_latency_ms, stream_duration_ms,
                            sdk_timeout_ms, completion_tokens, fallback_reason,
                            rekt_provenance, rekt_confidence,
                            payload_json, source_file, source_line
                        ) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
                        """,
                        (
                            obj.get("runId"),
                            obj.get("ts"),
                            obj.get("event"),
                            obj.get("agent"),
                            obj.get("file"),
                            obj.get("targetLanguage"),
                            obj.get("projectionMode"),
                            obj.get("projectionTokens"),
                            obj.get("rawRektTokens"),
                            obj.get("projectionHash"),
                            obj.get("factsSchema"),
                            obj.get("factsConfidence"),
                            obj.get("factsWarnings"),
                            obj.get("provider"),
                            obj.get("model"),
                            obj.get("outcome"),
                            obj.get("firstTokenLatencyMs"),
                            obj.get("streamDurationMs"),
                            obj.get("sdkTimeoutMs"),
                            obj.get("completionTokens"),
                            obj.get("fallbackReason"),
                            obj.get("rektProvenance"),
                            obj.get("rektConfidence"),
                            raw,
                            str(f.relative_to(REPO_ROOT)),
                            line_no,
                        ),
                    )
                    if conn.total_changes > inserted:
                        inserted = conn.total_changes
                except sqlite3.Error as e:
                    print(f"[ingest] DB error on {f.name}:{line_no}: {e}", file=sys.stderr)
                    skipped += 1

    conn.commit()
    print(f"Ingested {inserted} new events from {len(files)} file(s). Skipped {skipped}.")
    return inserted


def report(conn: sqlite3.Connection) -> None:
    cur = conn.cursor()
    print("\n=== Event counts by type ===")
    for row in cur.execute("SELECT event, COUNT(*) FROM metric_events GROUP BY event ORDER BY 2 DESC"):
        print(f"  {row[0] or '(null)':<24} {row[1]:>6}")

    print("\n=== Projection mode breakdown ===")
    for row in cur.execute(
        "SELECT projection_mode, COUNT(*) FROM metric_events "
        "WHERE event='projection_metrics' GROUP BY projection_mode ORDER BY 2 DESC"
    ):
        print(f"  {row[0] or '(none)':<16} {row[1]:>6}")

    print("\n=== Context-token reduction per program (latest run only) ===")
    rows = list(cur.execute("""
        WITH latest AS (
            SELECT MAX(run_id) AS max_run FROM metric_events WHERE event='projection_metrics'
        ),
        agg AS (
            SELECT file, projection_mode,
                   AVG(projection_tokens) AS proj_tok,
                   AVG(raw_rekt_tokens) AS raw_tok,
                   COUNT(*) AS n
              FROM metric_events
             WHERE event='projection_metrics'
             GROUP BY file, projection_mode
        )
        SELECT a.file,
               MAX(CASE WHEN projection_mode='raw-rekt' THEN raw_tok END) AS raw,
               MAX(CASE WHEN projection_mode='projection' THEN proj_tok END) AS proj
          FROM agg a
         GROUP BY a.file
         HAVING raw IS NOT NULL AND proj IS NOT NULL
         ORDER BY raw DESC
    """))
    if rows:
        print(f"  {'file':<50} {'raw':>6} {'proj':>6} {'Δ':>7}")
        for row in rows:
            f, raw, proj = row
            delta = "n/a" if not raw or raw == 0 else f"{(raw - proj) * 100 / raw:.0f}%"
            print(f"  {f or '?':<50} {raw or 0:>6.0f} {proj or 0:>6.0f} {delta:>7}")
    else:
        print("  (no programs with both raw + projection events yet)")

    print("\n=== LLM call outcomes ===")
    for row in cur.execute(
        "SELECT outcome, COUNT(*), AVG(stream_duration_ms), AVG(completion_tokens) "
        "FROM metric_events WHERE event='llm_call' GROUP BY outcome ORDER BY 2 DESC"
    ):
        outcome, n, avg_dur, avg_tok = row
        print(f"  {outcome or '?':<10} count={n:>4}  avgDurationMs={(avg_dur or 0):>6.0f}  avgCompletionTok={(avg_tok or 0):>5.0f}")

    print("\n=== Projection hash reuse (cache groundwork) ===")
    rows = list(cur.execute(
        "SELECT projection_hash, COUNT(DISTINCT file) AS files, COUNT(*) AS uses "
        "FROM metric_events WHERE event='projection_metrics' AND projection_hash IS NOT NULL "
        "GROUP BY projection_hash HAVING uses > 1 ORDER BY uses DESC LIMIT 10"
    ))
    if rows:
        print(f"  {'hash (prefix)':<16} {'distinct_files':>14} {'total_uses':>11}")
        for row in rows:
            print(f"  {(row[0] or '')[:12] + '...':<16} {row[1]:>14} {row[2]:>11}")
    else:
        print("  (no hash reuse observed yet — each program has unique facts)")

    print("\n=== Quality metrics (latest 5 runs) ===")
    rows = list(cur.execute("""
        SELECT run_id,
               json_extract(payload_json,'$.compileSuccess') AS ok,
               json_extract(payload_json,'$.compileErrors') AS errs,
               json_extract(payload_json,'$.generatedClassCount') AS classes,
               json_extract(payload_json,'$.generatedJavaLines') AS lines,
               json_extract(payload_json,'$.fallbackClassCount') AS fb,
               json_extract(payload_json,'$.injectAnnotationCount') AS inject
          FROM metric_events
         WHERE event='quality_metrics'
         ORDER BY ts DESC LIMIT 5
    """))
    if rows:
        print(f"  {'run':<8} {'compile':<8} {'errors':>7} {'files':>6} {'lines':>6} {'fallback':>9} {'@Inject':>8}")
        for r in rows:
            ok = "✅" if r[1] in (1, "true", True) else "❌"
            print(f"  {r[0] or '?':<8} {ok:<8} {r[2] or 0:>7} {r[3] or 0:>6} {r[4] or 0:>6} {r[5] or 0:>9} {r[6] or 0:>8}")
    else:
        print("  (no quality_metrics events yet — run tools/check-compile.sh after a conversion)")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--rebuild", action="store_true", help="drop and rebuild DB")
    ap.add_argument("--report", action="store_true", help="print summary after ingest")
    args = ap.parse_args()

    conn = open_db(rebuild=args.rebuild)
    ingest(conn)
    if args.report:
        report(conn)
    conn.close()


if __name__ == "__main__":
    main()
