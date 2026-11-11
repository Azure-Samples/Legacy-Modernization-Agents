import importlib.util
import json
import os
import shutil
import sys
import types
from pathlib import Path
from unittest.mock import MagicMock, Mock, patch

from test_doctor_jcl_scan import JclCommandFixture, REPO


def load_isolated_populator():
    def decorator(*args, **kwargs):
        return lambda function: function

    def group(*args, **kwargs):
        def decorate(function):
            function.command = decorator
            return function
        return decorate

    click = types.ModuleType("click")
    click.group, click.command, click.option = group, decorator, decorator
    neo4j = types.ModuleType("neo4j")
    neo4j.GraphDatabase = Mock()
    neo4j.GraphDatabase.driver.side_effect = AssertionError("No database is allowed in this test")
    neo4j.ManagedTransaction = object
    console = types.ModuleType("rich.console")
    console.Console = Mock
    progress = types.ModuleType("rich.progress")
    for name in ("Progress", "SpinnerColumn", "BarColumn", "TextColumn"):
        setattr(progress, name, Mock)
    dependencies = {"click": click, "neo4j": neo4j, "rich": types.ModuleType("rich"),
                    "rich.console": console, "rich.progress": progress}
    spec = importlib.util.spec_from_file_location("isolated_jcl_populator", REPO / "tools/graph-populator/populator.py")
    module = importlib.util.module_from_spec(spec)
    with patch.dict(sys.modules, dependencies), patch.object(sys, "path", [str(REPO / "tools/graph-populator"), *sys.path]):
        spec.loader.exec_module(module)
    return module


class JclCliIngestTests(JclCommandFixture):
    def setUp(self):
        super().setUp()
        dll = Path(os.environ.get("JCL_TEST_DLL", REPO / "bin/Debug/net10.0/CobolToQuarkusMigration.dll"))
        self.assertTrue(dll.is_file(), "Build CobolToQuarkusMigration.csproj before running the genuine CLI tests")
        self.env.update(JCL_REAL_DLL=str(dll), JCL_REAL_DOTNET=shutil.which("dotnet"))
        self.populator = load_isolated_populator()

    def consume(self):
        self.nodes, self.blocks = [], []

        def nodes(driver, label, rows, **kwargs):
            self.nodes.extend((label, row) for row in rows)
            return len(rows)

        def blocks(driver, program, text, run_id):
            self.blocks.append((program, text, run_id))

        with patch.object(self.populator, "batch_merge_nodes", side_effect=nodes), \
                patch.object(self.populator, "batch_merge_relationships"), \
                patch.object(self.populator, "create_source_blocks", side_effect=blocks):
            return self.populator.ingest_rekt_outputs(
                MagicMock(), str(self.root / "output/rekt"), str(self.root / "source"), 42)

    def test_external_nested_uppercase_job_roundtrips_exact_source_not_local_same_name(self):
        self.job(text="//WRONG JOB\n//S1 EXEC PGM=IEFBR14\n")
        external = self.job("external jobs/nested jobs/SAMPLE.JCL",
                            "//RIGHT JOB\n//S1 EXEC PGM=IEFBR14\n")
        self.env["JCL_SOURCE_FOLDER"] = str(external.parents[1])
        self.assert_success(self.run_command("rekt-full"))
        counts = self.consume()
        jobs = [row for label, row in self.nodes if label == "JclNode" and row["nodeType"] == "JCL_JOB"]
        self.assertEqual(["RIGHT"], [row["name"] for row in jobs])
        self.assertEqual("nested jobs/SAMPLE.JCL", jobs[0]["program"])
        self.assertGreater(counts["JclNode"], 0)
        self.assertEqual([("nested jobs/SAMPLE.JCL", external.read_text(), 42)], self.blocks)
        self.assertFalse(list((self.root / "output").rglob("*.cs")))
        self.assertFalse(list((self.root / "output").rglob("*.java")))
        self.assertFalse(list((self.root / "output").rglob("jobs-manifest.json")))

    def test_mixed_scan_retires_deleted_job_facts_including_last_job(self):
        self.job("source/PROGRAM.cbl", "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. PROGRAM.\n")
        first = self.job()
        second = self.job("source/second.jcl", "//SECOND JOB\n//S1 EXEC PGM=IEFBR14\n")
        self.assert_success(self.run_command("rekt-full"))
        first.unlink()
        self.assert_success(self.run_command("rekt-full"))
        self.consume()
        jobs = [row["name"] for label, row in self.nodes if label == "JclNode" and row["nodeType"] == "JCL_JOB"]
        self.assertEqual(["SECOND"], jobs)
        second.unlink()
        self.assert_success(self.run_command("rekt-full"))
        self.assertEqual(0, self.consume()["JclNode"])
        self.assertEqual([], list((self.root / "output/rekt").rglob("*.job.json")))
        self.assertTrue(any(label == "CobolFile" for label, _ in self.nodes))
        self.assertTrue(any("run" in args and "java" in args for args in self.calls("docker")))

    def test_failed_mixed_refresh_retains_job_evidence_but_consumer_refuses_it(self):
        self.job("source/PROGRAM.cbl", "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. PROGRAM.\n")
        self.job()
        self.assert_success(self.run_command("rekt-full"))
        facts_file = self.root / "output/rekt/nested jobs/SAMPLE.JCL.job.json"
        retained = facts_file.read_bytes()
        (self.root / "ingested.json").unlink()
        self.env["JCL_TEST_FAIL"] = "1"
        result = self.run_command("rekt-full")
        self.assertNotEqual(0, result.returncode)
        self.assertEqual(retained, facts_file.read_bytes())
        self.assertFalse((self.root / "ingested.json").exists())
        with self.assertRaisesRegex(ValueError, "JCL refresh did not complete"):
            self.consume()
        self.assertEqual([], self.nodes)
        self.assertEqual([], self.blocks)
        self.assertNotEqual(0, self.run_command("rekt-ingest").returncode)

    def test_procedure_or_garbage_extension_is_not_a_successful_job_scan(self):
        for text in ("//P PROC\n//S1 EXEC PGM=IEFBR14\n// PEND\n", "not JCL at all\n"):
            with self.subTest(text=text):
                self.job(text=text)
                result = self.run_command("rekt-full")
                self.assertNotEqual(0, result.returncode, result.stdout + result.stderr)
                self.assertIn("No JCL jobs were parsed", result.stderr)
                self.assertFalse((self.root / "ingested.json").exists())

    def test_unresolved_procedure_and_unsupported_utility_are_not_labelled_ready(self):
        self.job(text="//MISSING JOB\n//S1 EXEC ABSENT\n")
        self.job("source/sort.jcl", "//SORTJOB JOB\n//S1 EXEC PGM=SORT\n")
        self.env["TARGET_LANGUAGE"] = "CSharp"
        result = self.run_command("jcl")
        self.assert_success(result)
        manifests = list((self.root / "output").rglob("jobs-manifest.json"))
        self.assertTrue(manifests)
        entries = {entry["job"]: entry for entry in json.loads(manifests[-1].read_text())}
        self.assertEqual(["S1"], entries["MISSING"]["stepsNotRunnable"])
        self.assertEqual(["S1"], entries["SORTJOB"]["stepsNotRunnable"])
        self.assertIn("0 of 2 job(s) have every program and step they need", result.stdout)
        self.assertNotIn("runs utilities only", result.stdout)
        self.assertIn("unsupported utility", result.stdout)
