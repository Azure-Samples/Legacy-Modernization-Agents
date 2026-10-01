"""
JCL job facts as a graph: the job, its steps in order, the programs they run, the datasets
they touch and the jobs that feed it or that it feeds.

The input is what `jcl-facts` writes (Jcl/JclEstate.cs): one <source-relative>.job.json per
job and jcl-lineage.json for the estate. Nothing is re-derived here; dataset access and the
exclusion of program libraries come from the lineage, so the rules stay in one place.
"""

from __future__ import annotations

from source_paths import scoped_graph_id

# File names and the schema version written by Jcl/JclEstate.cs.
FACTS_SUFFIX = ".job.json"
LINEAGE_FILE = "jcl-lineage.json"
SUPPORTED_SCHEMA = 1

STEP_TYPES = {
    "Program": "JCL_STEP",
    "Utility": "JCL_UTILITY_STEP",
    "TsoBatch": "JCL_TSO_STEP",
    "UnresolvedProcedure": "JCL_UNRESOLVED_STEP",
}

ACCESS_EDGES = {
    "Read": "READS",
    "Create": "CREATES",
    "Append": "APPENDS",
    "Exclusive": "OPENS_EXCLUSIVE",
    "Delete": "DELETES",
}

MAX_LISTED = 3
MAX_CONTROL_LINES = 6


def is_jcl_artifact(file_name: str) -> bool:
    lower = file_name.lower()
    return lower.endswith(FACTS_SUFFIX) or lower == LINEAGE_FILE


def build_jcl_graph(facts: dict, lineage: dict | None, run_id: int) -> tuple[list[dict], list[dict]]:
    """Nodes and edges for one job. Edges carry from_id/to_id as node uids, a type and a label."""
    job = facts.get("job") or {}
    program = job.get("file") or job.get("name") or ""
    job_name = job.get("name") or program
    nodes: dict[str, dict] = {}
    edges: list[dict] = []

    def node(local_id: str, node_type: str, name: str, text: str = "", line: int = -1) -> str:
        uid = scoped_graph_id(run_id, program, local_id)
        if uid not in nodes:
            nodes[uid] = {
                "uid": uid,
                "id": local_id,
                "program": program,
                "job": job_name,
                "runId": run_id,
                "nodeType": node_type,
                "label": name,
                "name": name,
                "originalText": text,
                "startLine": line,
                "endLine": line,
            }
        return uid

    def edge(source: str, target: str, kind: str, label: str = "") -> None:
        edges.append({"from_id": source, "to_id": target, "type": kind, "label": label})

    job_uid = node("JOB", "JCL_JOB", job_name, _job_text(job), 1)

    steps = job.get("steps") or []
    step_uids: dict[str, str] = {}
    previous = None
    for step in steps:
        name = step.get("name") or ""
        # A qualified name (RUN1.STEPA) is a step of an expanded procedure, whose line is in
        # the procedure's member rather than in this file.
        line = step.get("line", -1) if "." not in name else -1
        uid = node(f"STEP:{name}", STEP_TYPES.get(step.get("kind"), "JCL_STEP"), name, _step_text(step), line)
        step_uids[name.upper()] = uid
        edge(job_uid, uid, "CONTAINS")
        if previous is not None:
            edge(previous, uid, "FOLLOWED_BY")
        previous = uid

        if step.get("program"):
            utility = step.get("kind") in ("Utility", "TsoBatch")
            target = node(f"PGM:{step['program']}", "JCL_UTILITY" if utility else "JCL_PROGRAM", step["program"])
            edge(uid, target, "RUNS", "EXEC PGM")
        for run in step.get("runs") or []:
            if run.get("program"):
                target = node(f"PGM:{run['program']}", "JCL_PROGRAM", run["program"])
                edge(uid, target, "RUNS", _join("RUN PROGRAM", run.get("plan") and f"PLAN({run['plan']})"))

    for dataset in (lineage or {}).get("datasets") or []:
        key = dataset.get("dataset") or ""
        for use in dataset.get("uses") or []:
            if use.get("job") != job_name:
                continue
            step_uid = step_uids.get((use.get("step") or "").upper())
            if step_uid is None:
                continue
            temporary = bool(dataset.get("temporary"))
            # Temporary datasets are keyed JOB:&&NAME in the lineage; the name alone is what the JCL says.
            display = key.split(":", 1)[1] if temporary and ":" in key else key
            target = node(f"DS:{key}", "JCL_TEMP_DATASET" if temporary else "JCL_DATASET", display)
            generation = use.get("generation")
            edge(step_uid, target, ACCESS_EDGES.get(use.get("access"), "USES"),
                 _join(use.get("via") or "", generation and f"({generation})"))

    datasets_between = {
        (d.get("upstream"), d.get("downstream")): d.get("datasets") or []
        for d in (lineage or {}).get("dependencies") or []
    }
    for upstream in facts.get("upstream") or []:
        ref = node(f"JOBREF:{upstream}", "JCL_JOB_REF", upstream)
        edge(ref, job_uid, "FEEDS", _listed(datasets_between.get((upstream, job_name), [])))
    for downstream in facts.get("downstream") or []:
        ref = node(f"JOBREF:{downstream}", "JCL_JOB_REF", downstream)
        edge(job_uid, ref, "FEEDS", _listed(datasets_between.get((job_name, downstream), [])))

    return list(nodes.values()), edges


def _job_text(job: dict) -> str:
    lines = [_join(f"//{job.get('name', '')} JOB",
                   job.get("jobClass") and f"CLASS={job['jobClass']}",
                   job.get("cond") and f"COND={job['cond']}")]
    diagnostics = job.get("diagnostics") or []
    if diagnostics:
        counts: dict[str, int] = {}
        for d in diagnostics:
            counts[d.get("code", "")] = counts.get(d.get("code", ""), 0) + 1
        lines.append(f"{len(diagnostics)} diagnostic(s): " + ", ".join(f"{c} ×{n}" for c, n in sorted(counts.items())))
    unresolved = job.get("unresolvedSymbols") or []
    if unresolved:
        lines.append("Unresolved symbols: " + ", ".join(unresolved))
    return "\n".join(lines)


def _step_text(step: dict) -> str:
    lines = []
    if step.get("program"):
        lines.append(_join(f"EXEC PGM={step['program']}", step.get("parm") is not None and f"PARM='{step['parm']}'"))
    if step.get("procedure"):
        inner = step.get("procedureStep")
        lines.append(f"Procedure {step['procedure']}" + (f", step {inner}" if inner else "")
                     + (" (not in the source)" if step.get("kind") == "UnresolvedProcedure" else ""))
    if step.get("cond"):
        lines.append(f"COND={step['cond']}")
    for condition in step.get("conditions") or []:
        expression = condition.get("expression", "")
        lines.append(f"ELSE of IF {expression}" if condition.get("negated") else f"IF {expression}")
    for run in step.get("runs") or []:
        lines.append(_join(f"RUN PROGRAM({run.get('program', '')})", run.get("plan") and f"PLAN({run['plan']})"))
    control = step.get("controlStatements") or []
    lines.extend(control[:MAX_CONTROL_LINES])
    if len(control) > MAX_CONTROL_LINES:
        lines.append(f"... {len(control) - MAX_CONTROL_LINES} more control statement(s)")
    return "\n".join(lines)


def _join(*parts) -> str:
    return " ".join(p for p in parts if p)


def _listed(items: list[str]) -> str:
    shown = ", ".join(items[:MAX_LISTED])
    return shown + (f" +{len(items) - MAX_LISTED}" if len(items) > MAX_LISTED else "")
