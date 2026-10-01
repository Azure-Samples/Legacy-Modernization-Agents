import json
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from jcl_graph import build_jcl_graph, is_jcl_artifact  # noqa: E402
from source_paths import scoped_graph_id  # noqa: E402

FACTS = {
    "schemaVersion": 1,
    "job": {
        "name": "PAYJOB",
        "file": "jcl/PAYJOB.jcl",
        "jobClass": "A",
        "steps": [
            {"name": "SORT1", "line": 2, "kind": "Utility", "program": "SORT",
             "controlStatements": ["SORT FIELDS=(1,8,CH,A)"]},
            {"name": "RUN1.CALC", "line": 3, "kind": "Program", "program": "PAYCALC",
             "procedure": "PAYPROC", "procedureStep": "CALC", "parm": "X",
             "conditions": [{"expression": "SORT1.RC = 0", "negated": False}]},
            {"name": "DB2", "line": 9, "kind": "TsoBatch", "program": "IKJEFT01",
             "runs": [{"program": "PAYDB2", "plan": "PAYPLAN"}]},
            {"name": "MISSING", "line": 12, "kind": "UnresolvedProcedure", "procedure": "NOPROC"},
        ],
        "diagnostics": [{"line": 12, "code": "PROC_NOT_FOUND", "message": "x"}],
    },
    "programs": ["PAYCALC", "PAYDB2"],
    "upstream": ["FEEDJOB"],
    "downstream": [],
}

LINEAGE = {
    "datasets": [
        {"dataset": "PAY.IN", "temporary": False, "uses": [
            {"job": "PAYJOB", "step": "SORT1", "access": "Read", "via": "SORTIN"},
            {"job": "FEEDJOB", "step": "S1", "access": "Create", "via": "OUT"},
        ]},
        {"dataset": "PAYJOB:&&WORK", "temporary": True, "uses": [
            {"job": "PAYJOB", "step": "SORT1", "access": "Create", "via": "SORTOUT"},
            {"job": "PAYJOB", "step": "RUN1.CALC", "access": "Read", "via": "IN"},
        ]},
        {"dataset": "PAY.HIST", "temporary": False, "uses": [
            {"job": "PAYJOB", "step": "RUN1.CALC", "access": "Append", "generation": "+1", "via": "HIST"},
        ]},
    ],
    "dependencies": [{"upstream": "FEEDJOB", "downstream": "PAYJOB", "datasets": ["PAY.IN"]}],
    "externalInputs": [],
}


class BuildJclGraphTests(unittest.TestCase):
    def setUp(self):
        nodes, edges = build_jcl_graph(FACTS, LINEAGE, 7)
        self.nodes = {n["id"]: n for n in nodes}
        by_uid = {n["uid"]: n["id"] for n in nodes}
        self.edges = {(by_uid[e["from_id"]], by_uid[e["to_id"]], e["type"]): e["label"] for e in edges}

    def test_nodes_are_scoped_to_the_run_and_the_job_file(self):
        job = self.nodes["JOB"]
        self.assertEqual(scoped_graph_id(7, "jcl/PAYJOB.jcl", "JOB"), job["uid"])
        self.assertEqual(("JCL_JOB", "PAYJOB", "jcl/PAYJOB.jcl"), (job["nodeType"], job["name"], job["program"]))
        self.assertIn("CLASS=A", job["originalText"])
        self.assertIn("PROC_NOT_FOUND ×1", job["originalText"])

    def test_steps_follow_each_other_and_say_what_they_run(self):
        self.assertEqual(
            ["JCL_UTILITY_STEP", "JCL_STEP", "JCL_TSO_STEP", "JCL_UNRESOLVED_STEP"],
            [self.nodes[f"STEP:{s}"]["nodeType"] for s in ("SORT1", "RUN1.CALC", "DB2", "MISSING")])
        self.assertIn(("STEP:SORT1", "STEP:RUN1.CALC", "FOLLOWED_BY"), self.edges)
        self.assertIn(("JOB", "STEP:DB2", "CONTAINS"), self.edges)
        self.assertEqual("JCL_UTILITY", self.nodes["PGM:SORT"]["nodeType"])
        self.assertEqual("JCL_UTILITY", self.nodes["PGM:IKJEFT01"]["nodeType"])
        self.assertEqual("JCL_PROGRAM", self.nodes["PGM:PAYDB2"]["nodeType"])
        self.assertEqual("RUN PROGRAM PLAN(PAYPLAN)", self.edges[("STEP:DB2", "PGM:PAYDB2", "RUNS")])
        self.assertIn("IF SORT1.RC = 0", self.nodes["STEP:RUN1.CALC"]["originalText"])
        self.assertIn("(not in the source)", self.nodes["STEP:MISSING"]["originalText"])

    def test_only_steps_written_in_the_file_carry_its_lines(self):
        self.assertEqual(2, self.nodes["STEP:SORT1"]["startLine"])
        self.assertEqual(-1, self.nodes["STEP:RUN1.CALC"]["startLine"])

    def test_datasets_come_from_the_lineage_for_this_job_only(self):
        self.assertEqual("SORTIN", self.edges[("STEP:SORT1", "DS:PAY.IN", "READS")])
        self.assertEqual("HIST (+1)", self.edges[("STEP:RUN1.CALC", "DS:PAY.HIST", "APPENDS")])
        work = self.nodes["DS:PAYJOB:&&WORK"]
        self.assertEqual(("JCL_TEMP_DATASET", "&&WORK"), (work["nodeType"], work["name"]))
        self.assertNotIn("STEP:S1", self.nodes)

    def test_jobs_that_feed_it_are_linked_by_their_datasets(self):
        self.assertEqual("JCL_JOB_REF", self.nodes["JOBREF:FEEDJOB"]["nodeType"])
        self.assertEqual("PAY.IN", self.edges[("JOBREF:FEEDJOB", "JOB", "FEEDS")])

    def test_without_lineage_the_job_still_has_its_steps(self):
        nodes, _ = build_jcl_graph(FACTS, None, 1)
        self.assertFalse(any(n["nodeType"].endswith("DATASET") for n in nodes))
        self.assertEqual(4, sum(n["id"].startswith("STEP:") for n in nodes))

    def test_jcl_artifacts_are_recognised_by_name(self):
        self.assertTrue(is_jcl_artifact("PAYJOB.jcl.job.json"))
        self.assertTrue(is_jcl_artifact("jcl-lineage.json"))
        self.assertFalse(is_jcl_artifact("flow-ast-PAYCALC.cbl.json"))


class IngestJclOutputsTests(unittest.TestCase):
    def test_ingests_each_job_with_its_source_and_skips_newer_schemas(self):
        import test_populator  # noqa: F401  installs the neo4j/click/rich stubs
        from populator import ingest_jcl_outputs

        with tempfile.TemporaryDirectory() as tmp:
            out, src = Path(tmp, "out"), Path(tmp, "src")
            (out / "jcl").mkdir(parents=True)
            (src / "jcl").mkdir(parents=True)
            (src / "jcl/PAYJOB.jcl").write_text("//PAYJOB JOB\n")
            (out / "jcl/PAYJOB.jcl.job.json").write_text(json.dumps(FACTS))
            (out / "jcl/NEWER.jcl.job.json").write_text(json.dumps({**FACTS, "schemaVersion": 99}))
            (out / "jcl-lineage.json").write_text(json.dumps(LINEAGE))

            with patch("populator.batch_merge_nodes", return_value=5) as merge_nodes, \
                    patch("populator.batch_merge_relationships") as merge_rels, \
                    patch("populator.create_source_blocks") as blocks:
                total = ingest_jcl_outputs(object(), out, src, 3)

        self.assertEqual(5, total)
        self.assertEqual("JclNode", merge_nodes.call_args.args[1])
        self.assertIn("FEEDS", {c.args[3] for c in merge_rels.call_args_list})
        self.assertEqual("jcl/PAYJOB.jcl", blocks.call_args.args[1])


if __name__ == "__main__":
    unittest.main()
