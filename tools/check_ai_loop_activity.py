#!/usr/bin/env python3
"""Read-only post-activation acceptance for the unified AI Loop catalogue."""
import argparse
from datetime import datetime
import json
from pathlib import Path
import sqlite3
from urllib.request import urlopen


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--url", default="http://localhost:5028")
    parser.add_argument("--repo-root", type=Path, default=Path(__file__).resolve().parents[1])
    parser.add_argument("--scan", action="append", required=True,
                        help="output/rekt-scans/<name> manifest expected in the catalogue (repeatable)")
    parser.add_argument("--model-run", help="migration run ID whose model-attempt summary must be catalogued")
    args = parser.parse_args()
    args.repo_root = args.repo_root.resolve()

    def get(path):
        with urlopen(args.url.rstrip("/") + path, timeout=30) as response:
            return json.load(response)

    payload = get("/api/ai-loop/runs")
    runs = payload["runs"]
    for _ in range(20):
        if not payload.get("hasMore"):
            break
        payload = get("/api/ai-loop/runs?offset=" + str(payload["nextOffset"]))
        runs.extend(payload["runs"])
    for name in args.scan:
        manifest = json.loads((args.repo_root / "output/rekt-scans" / name / "scan.json").read_text())
        matches = [r for r in runs if r.get("activityId") == "native:" + name]
        assert len(matches) == 1, f"{name}: native activity missing; activate the new portal assembly first"
        summary = matches[0]
        detail = get("/api/ai-loop/" + summary["runId"])
        assert summary["activityKind"] == "native-rekt"
        assert summary["requestCoverage"] == "not-applicable" and summary["calls"] == 0
        assert summary["attemptId"] is None and summary["managedRunId"] is None
        assert detail["native"]["recordedPrograms"] == len(manifest["programs"])
        assert detail["native"]["expectedPrograms"] == manifest["expectedPrograms"]
        assert detail["summary"]["status"] == manifest["status"]
        assert datetime.fromisoformat(summary["finishedAt"].replace("Z", "+00:00")) == datetime.fromisoformat(manifest["finishedAt"])
        assert not detail["agents"] and not detail["requests"]
        assert any(log["kind"] == "structure stderr" for log in detail["logs"])
        print(json.dumps({k: summary.get(k) for k in
                          ["activityId", "runId", "activityKind", "status", "startedAt", "finishedAt", "lastEventAt"]}))
    model = get("/api/ai-loop/" + args.model_run)["summary"] if args.model_run else None
    if model is not None:
        assert model["activityKind"] == "model-attempt"
        assert model["startedRequests"] >= model["calls"]
    metrics = list((args.repo_root / "output/.metrics").glob("*.jsonl"))
    with sqlite3.connect((args.repo_root / "Data/migration.db").as_uri() + "?mode=ro", uri=True) as database:
        count = database.execute("SELECT COUNT(*) FROM runs").fetchone()[0]
    print(json.dumps(dict(modelFinishedAt=model and model["finishedAt"], modelStarts=model and model["startedRequests"],
                          modelOutcomes=model and model["calls"], migrationRows=count, metricsFiles=len(metrics),
                          metricsRecords=sum(sum(1 for _ in p.open(encoding="utf-8")) for p in metrics))))
    print("PASS: recorded native manifests are catalogued without fabricated model activity.")


if __name__ == "__main__":
    main()
