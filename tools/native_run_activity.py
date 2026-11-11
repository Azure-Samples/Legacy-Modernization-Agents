#!/usr/bin/env python3
"""Atomic, metadata-only native REKT lifecycle journal used by doctor.sh.

Completion measurements reuse native artifact/dependency validators; null means
unrecorded or unverified, with bounded reasons and original artifact/log references.
Parser process exit codes are not reconstructed from artifact availability.
"""
import argparse
import contextlib
import datetime
import fcntl
import json
import os
from pathlib import Path
import uuid


def now():
    return datetime.datetime.now(datetime.timezone.utc).isoformat()


def write(path, data):
    pending = path.with_name(f"{path.stem}.{os.getpid()}.{uuid.uuid4().hex}.pending")
    pending.write_text(json.dumps(data, indent=2) + "\n", encoding="utf-8")
    os.replace(pending, path)


@contextlib.contextmanager
def journal_lock(path):
    # Parallel parse jobs update the same journal; serialise each read-modify-write.
    with open(path.with_suffix(".lock"), "a", encoding="utf-8") as handle:
        fcntl.flock(handle, fcntl.LOCK_EX)
        try:
            yield
        finally:
            fcntl.flock(handle, fcntl.LOCK_UN)


def measurements(source_root, output_root, relative_path):
    from rekt_dependency_evidence import KINDS, PRODUCER, digest, evidence_current
    from rekt_parse_diagnostics import has_structure

    report = output_root / (relative_path + ".report")
    evidence_path = report / "dependency-evidence.json"
    result = dict(reportPath=str(report), dependencyEvidencePath=str(evidence_path),
                  parseLogPath=str(output_root / (relative_path + ".parse.log")),
                  hasAst=None, hasCfg=None, hasData=None, strictDependencyComplete=None,
                  dependencyExportExitCode=None)
    reasons = {}
    for field, directory in (("hasAst", "flow_ast"), ("hasCfg", "cfg"), ("hasData", "data_structures")):
        if not any((report / directory).glob("*.json")):
            reasons[field] = "artifact-not-recorded"
            continue
        try:
            result[field] = has_structure(report, (directory,))
            if not result[field]:
                reasons[field] = "artifact-structure-empty-or-invalid"
        except (OSError, ValueError, TypeError):
            reasons[field] = "artifact-unreadable-or-invalid-json"
    try:
        evidence = json.loads(evidence_path.read_text(encoding="utf-8"))
        source = (source_root / relative_path).resolve()
        if (evidence["schemaVersion"] != 1 or evidence["producer"] != PRODUCER
                or evidence["relativePath"] != relative_path
                or Path(evidence["originalSourcePath"]).resolve() != source
                or evidence["originalSourceSha256"] != digest(source)):
            raise ValueError("source binding")
        if (not isinstance(evidence["coverage"], dict)
                or any(evidence["coverage"].get(kind) not in ("complete", "partial", "unavailable")
                       for kind in KINDS)
                or not isinstance(evidence["definitions"], list)
                or not isinstance(evidence["unresolvedDiagnostics"], list)
                or any(not isinstance(issue, str) for issue in evidence["unresolvedDiagnostics"])
                or type(evidence["placeholderContaminated"]) is not bool):
            raise ValueError("incomplete evidence record")
        export = output_root / (relative_path + "-deps.json")
        if (Path(evidence["exportPath"]).resolve() != export.resolve()
                or evidence["exportSha256"] != (digest(export) if export.is_file() else None)):
            raise ValueError("export integrity")
        for definition in evidence["definitions"]:
            if definition["sha256"] != digest(definition["path"]):
                raise ValueError("definition integrity")
        code = evidence["exportExitCode"]
        if type(code) is not int:
            raise ValueError("unrecorded exporter exit")
        issues = list(dict.fromkeys(evidence["unresolvedDiagnostics"]))
        definitions = list({(item["kind"], item["name"], item["path"]):
                            dict(kind=item["kind"], name=item["name"], path=item["path"])
                            for item in evidence["definitions"]}.values())
        if any(not isinstance(value, str) for item in definitions for value in item.values()):
            raise ValueError("invalid input metadata")
        result["strictDependencyComplete"] = evidence_current(source_root, relative_path, output_root)
        result["dependencyExportExitCode"] = code
        result["dependencyDiagnostics"] = [issue[:512] for issue in issues[:32]]
        result["dependencyDiagnosticsTruncated"] = len(issues) > 32 or any(len(issue) > 512 for issue in issues[:32])
        result["inputSourceRoot"] = str(source_root.resolve())
        result["resolvedDefinitions"] = definitions[:128]
        result["resolvedDefinitionsTruncated"] = len(definitions) > 128
        if not result["strictDependencyComplete"]:
            reasons["strictDependencyComplete"] = "dependency-evidence-not-strictly-complete"
            if issues:
                reasons["dependencyDetails"] = " | ".join(issues[:8])[:1000]
    except FileNotFoundError:
        reasons["strictDependencyComplete"] = "dependency-evidence-or-bound-input-not-recorded"
    except (OSError, ValueError, KeyError, TypeError, AttributeError):
        reasons["strictDependencyComplete"] = "dependency-evidence-invalid-or-integrity-mismatch"
    result["measurementReasons"] = reasons
    result["measuredAt"] = now()
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("operation", choices=["start", "program", "finish", "stage"])
    parser.add_argument("path", type=Path)
    parser.add_argument("--program")
    parser.add_argument("--status")
    parser.add_argument("--stage")
    parser.add_argument("--exit-code", type=int)
    parser.add_argument("--expected-programs", type=int)
    parser.add_argument("--source-root", type=Path)
    parser.add_argument("--output-root", type=Path)
    parser.add_argument("--command", choices=["rekt-parse", "rekt-full", "rekt-ingest"], default="rekt-parse")
    args = parser.parse_args()
    if args.operation == "start":
        folder = args.path / ("doctor-" + uuid.uuid4().hex)
        folder.mkdir(parents=True)
        path = folder / "scan.json"
        stamp = now()
        stage = args.stage or "Native parsing"
        write(path, dict(schemaVersion=1, activityKind="native-rekt", name=folder.name, command=args.command,
                         startedAt=stamp, updatedAt=stamp, status="running",
                         currentStage=stage, stages=[dict(name=stage, startedAt=stamp)], programs=[],
                         logPath="native.log"))
        print(path)
        return
    path = args.path
    measured = None
    if (args.operation == "program" and args.status != "running"
            and args.source_root is not None and args.output_root is not None):
        measured = measurements(args.source_root, args.output_root, args.program)
    with journal_lock(path):
        update(path, args, measured)


def update(path, args, measured):
    data = json.loads(path.read_text(encoding="utf-8"))
    if data.get("schemaVersion") != 1 or data.get("activityKind") != "native-rekt":
        raise ValueError("Not a native lifecycle journal")
    data["updatedAt"] = now()
    if args.operation == "program":
        row = next((p for p in data["programs"] if p["relativePath"] == args.program), {})
        row.update(relativePath=args.program, status=args.status, updatedAt=data["updatedAt"])
        if args.status == "running":
            row["startedAt"] = data["updatedAt"]
            row.pop("finishedAt", None)
            for key in ("hasAst", "hasCfg", "hasData", "strictDependencyComplete",
                        "dependencyExportExitCode", "measurementReasons", "measuredAt",
                        "dependencyDiagnostics", "dependencyDiagnosticsTruncated", "inputSourceRoot",
                        "resolvedDefinitions", "resolvedDefinitionsTruncated",
                        "reportPath", "dependencyEvidencePath", "parseLogPath"):
                row.pop(key, None)
        else:
            row["finishedAt"] = data["updatedAt"]
            if measured is not None:
                row.update(measured)
        data["programs"] = [p for p in data["programs"] if p["relativePath"] != args.program] + [row]
        data["currentStage"] = "Native parsing: " + args.program
    elif args.operation == "stage":
        data["currentStage"] = args.stage
        stages = data.setdefault("stages", [])
        if not stages or stages[-1]["name"] != args.stage:
            if stages:
                stages[-1]["finishedAt"] = data["updatedAt"]
            stages.append(dict(name=args.stage, startedAt=data["updatedAt"]))
        if args.expected_programs is not None:
            data["expectedPrograms"] = args.expected_programs
    else:
        data["finishedAt"] = data["updatedAt"]
        data["exitCode"] = args.exit_code
        statuses = {p["status"] for p in data["programs"]}
        data["status"] = ("cancelled" if args.exit_code in (130, 143) else
                          "failed" if args.exit_code else
                          "completed-with-gaps" if statuses - {"Full", "NoDialect", "cached"} else
                          "completed")
        if args.exit_code:
            data["error"] = "Native command exited with code " + str(args.exit_code)
        if data.get("stages"):
            data["stages"][-1]["finishedAt"] = data["updatedAt"]
            data["stages"][-1]["status"] = data["status"]
    write(path, data)


if __name__ == "__main__":
    main()
