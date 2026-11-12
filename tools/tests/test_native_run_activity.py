import json
from pathlib import Path
import shutil
import subprocess
import sys
import unittest
import uuid


ROOT = Path(__file__).resolve().parents[2]
TOOL = ROOT / "tools/native_run_activity.py"
sys.path.insert(0, str(ROOT / "tools"))
from native_run_activity import measurements
from rekt_dependency_evidence import ABSENCE_PROOFS, KINDS, build_evidence, digest


class NativeRunActivityTests(unittest.TestCase):
    def setUp(self):
        self.folder = Path.cwd() / (".native-activity-test-" + uuid.uuid4().hex)
        self.folder.mkdir()

    def tearDown(self):
        shutil.rmtree(self.folder)

    def invoke(self, operation, path, *options):
        return subprocess.check_output([sys.executable, str(TOOL), operation, str(path), *options], text=True).strip()

    def copy_helpers(self, root):
        (root / "tools").mkdir(parents=True)
        for name in ("native_run_activity.py", "rekt_dependency_evidence.py",
                     "rekt_parse_diagnostics.py", "rekt_source_identity.py"):
            shutil.copyfile(ROOT / "tools" / name, root / "tools" / name)

    def test_completion_measures_checked_artifacts_and_actual_failed_export(self):
        source, output = self.folder / "source", self.folder / "output/rekt"
        source.mkdir()
        (source / "A.cbl").write_text("           GOBACK.\n")
        report = output / "A.cbl.report"
        for directory in ("flow_ast", "cfg", "data_structures"):
            artifact = report / directory / "A.json"
            artifact.parent.mkdir(parents=True)
            artifact.write_text('{"nodes":[1]}')
        (report / "dependency-evidence.json").write_text(json.dumps(
            build_evidence(source, "A.cbl", output, 19)))
        path = Path(self.invoke("start", self.folder / "catalogue"))
        self.invoke("program", path, "--program", "A.cbl", "--status", "Partial",
                    "--source-root", str(source), "--output-root", str(output))
        row = json.loads(path.read_text())["programs"][0]
        self.assertTrue(all(row[key] is True for key in ("hasAst", "hasCfg", "hasData")))
        self.assertFalse(row["strictDependencyComplete"])
        self.assertEqual(19, row["dependencyExportExitCode"])
        self.assertNotIn("structureExitCode", row, "A parser process exit was not captured")
        self.assertEqual(str(report), row["reportPath"])
        self.assertIn("dependency-export-failed:19", row["measurementReasons"]["dependencyDetails"])
        self.assertIn("dependency-export-failed:19", row["dependencyDiagnostics"])
        self.assertEqual([], row["resolvedDefinitions"])
        self.assertLessEqual(len(row["measurementReasons"]["dependencyDetails"]), 1000)
        self.invoke("program", path, "--program", "A.cbl", "--status", "running")
        running = json.loads(path.read_text())["programs"][0]
        for key in ("dependencyDiagnostics", "resolvedDefinitions", "inputSourceRoot", "strictDependencyComplete"):
            self.assertNotIn(key, running)

    def test_recorded_bindings_are_bounded_metadata_with_explicit_diagnostic_truncation(self):
        source, output = self.folder / "source", self.folder / "output"
        source.mkdir()
        (source / "A.cbl").write_text("           GOBACK.\n")
        copy = source / "SHARED.cpy"
        copy.write_text("       01 SHARED PIC X.\n")
        evidence = build_evidence(source, "A.cbl", output, 1)
        evidence["definitions"] = [dict(kind="COPY", name="SHARED", path=str(copy), sha256=digest(copy))]
        evidence["unresolvedDiagnostics"] = ["native:unresolved-copybook:HEADER"] + [
            "unknown-condition:" + str(index) for index in range(40)]
        report = output / "A.cbl.report"
        report.mkdir(parents=True)
        (report / "dependency-evidence.json").write_text(json.dumps(evidence))
        row = measurements(source, output, "A.cbl")
        self.assertEqual([dict(kind="COPY", name="SHARED", path=str(copy))], row["resolvedDefinitions"])
        self.assertEqual(32, len(row["dependencyDiagnostics"]))
        self.assertTrue(row["dependencyDiagnosticsTruncated"])
        self.assertEqual(str(source.resolve()), row["inputSourceRoot"])
        self.assertFalse(row["strictDependencyComplete"])

    def test_missing_invalid_and_integrity_failures_remain_unknown(self):
        source, output = self.folder / "source", self.folder / "output"
        source.mkdir()
        program = source / "A.cbl"
        program.write_text("           GOBACK.\n")
        missing = measurements(source, output, "A.cbl")
        self.assertTrue(all(missing[key] is None for key in
                            ("hasAst", "hasCfg", "hasData", "strictDependencyComplete")))
        self.assertEqual("artifact-not-recorded", missing["measurementReasons"]["hasAst"])
        report = output / "A.cbl.report"
        (report / "flow_ast").mkdir(parents=True)
        (report / "flow_ast/A.json").write_text("{invalid")
        (report / "dependency-evidence.json").write_text(json.dumps(
            build_evidence(source, "A.cbl", output, 1)))
        program.write_text("           STOP RUN.\n")
        invalid = measurements(source, output, "A.cbl")
        self.assertIsNone(invalid["hasAst"])
        self.assertIsNone(invalid["strictDependencyComplete"])
        self.assertIsNone(invalid["dependencyExportExitCode"])
        self.assertEqual("dependency-evidence-invalid-or-integrity-mismatch",
                         invalid["measurementReasons"]["strictDependencyComplete"])

    def test_cached_completion_checks_artifacts_instead_of_assuming_flags(self):
        self.copy_helpers(self.folder)
        path = Path(self.invoke("start", self.folder / "catalogue"))
        self.invoke("program", path, "--program", "A.cbl", "--status", "cached",
                    "--source-root", str(self.folder / "source"), "--output-root", str(self.folder / "output"))
        row = json.loads(path.read_text())["programs"][0]
        self.assertIsNone(row["hasAst"])
        self.assertIsNone(row["strictDependencyComplete"])

    def test_strict_complete_requires_current_native_proofs_and_export_integrity(self):
        source, output = self.folder / "source", self.folder / "output"
        source.mkdir()
        output.mkdir()
        program = source / "A.cbl"
        program.write_text("           GOBACK.\n")
        export = output / "A.cbl-deps.json"
        export.write_text(json.dumps(dict(
            dependencyExportSchemaVersion=1, sourcePath=str(program), sourceSha256=digest(program),
            callTarget=dict(name="A.cbl", programReferenceType="STATIC"), dependencies=[],
            dependencyAnalysisComplete=True, dependencyCoverage=dict.fromkeys(KINDS, "complete"),
            dependencyAbsenceEvidence=dict(ABSENCE_PROOFS), dependencyDiagnostics=[])))
        report = output / "A.cbl.report"
        report.mkdir()
        (report / "dependency-evidence.json").write_text(json.dumps(build_evidence(source, "A.cbl", output, 0)))
        self.assertTrue(measurements(source, output, "A.cbl")["strictDependencyComplete"])
        evidence_path = report / "dependency-evidence.json"
        evidence = json.loads(evidence_path.read_text())
        del evidence["coverage"]
        evidence_path.write_text(json.dumps(evidence))
        self.assertIsNone(measurements(source, output, "A.cbl")["strictDependencyComplete"])
        evidence_path.write_text(json.dumps(build_evidence(source, "A.cbl", output, 0)))
        export.write_text("{}")
        self.assertIsNone(measurements(source, output, "A.cbl")["strictDependencyComplete"])
        export.write_text("[]")
        evidence["coverage"] = dict.fromkeys(KINDS, "complete")
        evidence["exportSha256"] = digest(export)
        evidence_path.write_text(json.dumps(evidence))
        self.assertIsNone(measurements(source, output, "A.cbl")["strictDependencyComplete"])

    def test_concurrent_program_updates_keep_every_program(self):
        path = Path(self.invoke("start", self.folder / "catalogue"))
        programs = [f"P{index:02}.cbl" for index in range(12)]
        workers = [subprocess.Popen([sys.executable, str(TOOL), "program", str(path),
                                     "--program", name, "--status", "Full"]) for name in programs]
        self.assertEqual([0] * len(workers), [worker.wait() for worker in workers])
        recorded = json.loads(path.read_text())["programs"]
        self.assertEqual(sorted(programs), sorted(row["relativePath"] for row in recorded))
        self.assertEqual([], list(path.parent.glob("*.pending")))

    def test_running_partial_and_cancelled_are_recorded_without_model_events(self):
        path = Path(self.invoke("start", self.folder))
        initial = json.loads(path.read_text())
        self.assertEqual("running", initial["status"])
        self.assertNotIn("finishedAt", initial)
        self.invoke("program", path, "--program", "estate/A.cbl", "--status", "running")
        self.invoke("finish", path, "--exit-code", "143")
        final = json.loads(path.read_text())
        self.assertEqual("cancelled", final["status"])
        self.assertEqual("running", final["programs"][0]["status"])
        self.assertEqual(initial["startedAt"], final["startedAt"])
        self.assertGreaterEqual(final["finishedAt"], initial["startedAt"])
        self.assertNotIn("runId", final)
        self.assertNotIn("model", final)

    def test_program_updates_preserve_exact_paths_and_failed_outcomes(self):
        path = Path(self.invoke("start", self.folder))
        for program, status in [("one/A.cbl", "running"), ("two/A.cbl", "Failed"), ("one/A.cbl", "Partial")]:
            self.invoke("program", path, "--program", program, "--status", status)
        self.invoke("finish", path, "--exit-code", "0")
        final = json.loads(path.read_text())
        self.assertEqual("completed-with-gaps", final["status"])
        self.assertEqual(2, len(final["programs"]))
        self.assertEqual({"two/A.cbl", "one/A.cbl"}, {p["relativePath"] for p in final["programs"]})
        self.assertFalse(path.with_suffix(".pending").exists())
        finished = next(p for p in final["programs"] if p["relativePath"] == "one/A.cbl")
        self.assertLessEqual(finished["startedAt"], finished["finishedAt"])

    def test_doctor_wrapper_records_terminal_results_without_running_a_scan(self):
        doctor = (ROOT / "doctor.sh").read_text()
        start = doctor.index("native_activity_program() {")
        end = doctor.index("\nrun_rekt_parse_impl()", start)
        wrapper = doctor[start:end]
        # Copy only the helper; this fixture has no source, graph, containers or native CLI.
        self.copy_helpers(self.folder)
        scenarios = [
            ("early-failure", "", 7, "failed"),
            ("success", "Full", 0, "completed"),
            ("degraded", "Partial", 0, "completed-with-gaps"),
            ("program-failure", "Failed", 0, "completed-with-gaps"),
            ("interrupt", "running", 130, "cancelled"),
            ("terminate", "running", 143, "cancelled"),
        ]
        for label, program_status, exit_code, expected_status in scenarios:
            with self.subTest(label=label):
                before = set((self.folder / "output/rekt-scans").glob("*/scan.json"))
                command = (
                    'REPO_ROOT="$1"; PYTHON_CMD="$2"; PROGRAM_STATUS="$3"; EXIT_CODE="$4"\n'
                    + wrapper
                    + '\nrun_rekt_parse_impl() {\n'
                    + 'echo "synthetic lifecycle test"\n'
                    + 'if [[ -n "$PROGRAM_STATUS" ]]; then\n'
                    + 'native_activity_program estate/A.cbl running\n'
                    + 'native_activity_program estate/A.cbl "$PROGRAM_STATUS"\n'
                    + 'fi\nreturn "$EXIT_CODE"\n}\nrun_rekt_parse'
                )
                result = subprocess.run(
                    ["bash", "-c", command, "fixture", str(self.folder), sys.executable,
                     program_status, str(exit_code)], capture_output=True, text=True)
                self.assertEqual(exit_code, result.returncode, result.stderr)
                created = set((self.folder / "output/rekt-scans").glob("*/scan.json")) - before
                self.assertEqual(1, len(created))
                path = created.pop()
                final = json.loads(path.read_text())
                self.assertEqual(expected_status, final["status"])
                self.assertEqual(exit_code, final["exitCode"])
                self.assertTrue(final["finishedAt"])
                self.assertIn("synthetic lifecycle test", (path.parent / final["logPath"]).read_text())
                if program_status:
                    program = final["programs"][0]
                    self.assertEqual("estate/A.cbl", program["relativePath"])
                    self.assertEqual(program_status, program["status"])
                    self.assertTrue(program["startedAt"])
                    self.assertEqual(program_status != "running", "finishedAt" in program)
                else:
                    self.assertEqual([], final["programs"])

    def test_full_and_ingest_operations_own_one_journal_until_the_entire_operation_returns(self):
        doctor = (ROOT / "doctor.sh").read_text()
        common = doctor[doctor.index("native_activity_program() {"):doctor.index("\nrun_rekt_parse_impl()")]
        ingest = doctor[doctor.index("run_rekt_ingest() {"):doctor.index("\nrun_rekt_ingest_impl()")]
        full = doctor[doctor.index("run_rekt_full() {"):doctor.index("\nrun_rekt_status()")]
        scenarios = [
            ("full-success", "rekt-full", 0, 0, "completed"),
            ("full-ingest-failure", "rekt-full", 0, 7, "failed"),
            ("full-parse-failure", "rekt-full", 8, 0, "failed"),
            ("ingest-success", "rekt-ingest", 0, 0, "completed"),
            ("ingest-failure", "rekt-ingest", 0, 7, "failed"),
        ]
        for label, operation, parse_exit, ingest_exit, expected in scenarios:
            with self.subTest(label=label):
                root = self.folder / label
                self.copy_helpers(root)
                command = (
                    'REPO_ROOT="$1"; PYTHON_CMD="$2"; OPERATION="$3"; PARSE_EXIT="$4"; INGEST_EXIT="$5"\n'
                    + common + "\n" + ingest + "\n" + full + "\n"
                    + r'''
run_rekt_parse_impl() {
    native_activity_program estate/A.cbl running
    native_activity_program estate/A.cbl Full
    [[ "$PARSE_EXIT" == 0 ]] || return "$PARSE_EXIT"
    native_activity_stage "Native JCL scan"
}
run_rekt_ingest_impl() {
    "$PYTHON_CMD" - "$_REKT_ACTIVITY_PATH" "$REPO_ROOT/ingest-observation.json" <<'PY'
import json, sys
from pathlib import Path
data = json.loads(Path(sys.argv[1]).read_text())
Path(sys.argv[2]).write_text(json.dumps(data))
PY
    return "$INGEST_EXIT"
}
case "$OPERATION" in
    rekt-full) run_rekt_full ;;
    rekt-ingest) run_rekt_ingest ;;
esac
''')
                result = subprocess.run(
                    ["bash", "-c", command, "fixture", str(root), sys.executable,
                     operation, str(parse_exit), str(ingest_exit)], capture_output=True, text=True)
                self.assertEqual(0 if expected == "completed" else 1 if operation == "rekt-full" else ingest_exit,
                                 result.returncode, result.stderr)
                manifests = list((root / "output/rekt-scans").glob("*/scan.json"))
                self.assertEqual(1, len(manifests), "A full operation must not create a second parse/ingest journal")
                final = json.loads(manifests[0].read_text())
                self.assertEqual(operation, final["command"])
                self.assertEqual(expected, final["status"])
                self.assertTrue(final["finishedAt"])
                self.assertNotIn("graphSnapshotId", final)
                observation_path = root / "ingest-observation.json"
                if parse_exit:
                    self.assertFalse(observation_path.exists(), "Ingestion must not run after parse failure")
                    self.assertEqual("Native parsing", final["stages"][-1]["name"])
                else:
                    observation = json.loads(observation_path.read_text())
                    self.assertEqual(final["name"], observation["name"])
                    self.assertEqual("running", observation["status"])
                    self.assertNotIn("finishedAt", observation)
                    self.assertEqual("Native graph ingestion", observation["currentStage"])
                    self.assertNotIn("finishedAt", observation["stages"][-1])
                    self.assertEqual(expected, final["stages"][-1]["status"])
                    if operation == "rekt-full":
                        self.assertEqual(["Native parsing", "Native JCL scan", "Native graph ingestion"],
                                         [stage["name"] for stage in final["stages"]])
                        self.assertEqual("Full", final["programs"][0]["status"])
                self.assertTrue(all(stage.get("finishedAt") for stage in final["stages"]))


if __name__ == "__main__":
    unittest.main()
